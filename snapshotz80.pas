unit SnapshotZ80;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitFileSna, UnitSpectrum;

type
  // https://sinclair.wiki.zxnet.co.uk/wiki/Z80_format
  // https://worldofspectrum.org/faq/reference/z80format.htm

  TSnapshotZ80 = class(TSnapshotFile)
  private

    type

      THeader1 = packed record
        AF: Word;
        BC, HL: Word;
        PC: Word;
        SP: Word;
        IR: Word;
        ViB: Byte; // various info
        DE: Word;
        BC1, DE1, HL1: Word;
        AF1: Word;
        Iy, Ix: Word;
        IFF1, IFF2: Byte; // 0 - disabled, otherwise enabled
        ViB2: Byte; // various info
      end;

      THeader2 = packed record
        LeH3: Word;
        PC: Word;
        HardwareMode: Byte;
        B35, B36, B37: Byte;
        AyActiveRegister: Byte;
        SoundRegisters: packed array [0..15] of Byte;
      end;

      THeader3 = packed record
        LoTStateCounter: Word;
        HiTStateCounter: Byte;
        B58, B59, B60, B61, B62: Byte;

        // 63 10 5 x keyboard mappings for user defined joystick
        // 73 10 5 x ASCII word: keys corresponding to mappings above
        // (..? format? what)
        KeyMappingsForJoystick, KeysForMappings: packed array [0..9] of Byte;

        B83, B84, B85: Byte;
        // ** one more byte should be read if word in position 30 is 55
      end;

      TFourBytes = packed array [0..3] of Byte;

  private
    const
      EndOfMemStream: TFourBytes = (0, $ED, $ED, 0);

  public
    class function GetDefaultExtension: String; override;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;
  end;

implementation

{ TSnapshotZ80 }

class function TSnapshotZ80.GetDefaultExtension: String;
begin
  Result := 'z80';
end;

function TSnapshotZ80.LoadFromStream(const Stream: TStream): Boolean;

  function ReadFromCompressed(const Str, AMemory: TMemoryStream; const AMemPosition, AMemSize: Integer): Boolean;
  var
    P, P1, PE, PE1: PByte;
    N: Integer;

  begin
    Result := False;
    //
    if AMemory.Size - AMemPosition >= AMemSize then begin
      P := PByte(Str.Memory);
      P1 := PByte(AMemory.Memory) + AMemPosition;
      PE := P + Str.Size;
      PE1 := P1 + AMemSize;
      while (P < PE) and (P1 < PE1) do begin
        if (P^ = 0) and (P + 3 < PE)
          and (CompareByte(P^, EndOfMemStream[0], Length(EndOfMemStream)) = 0)
        then begin
          Inc(P1, 4);
          //Inc(L, 4);
          //Break;
        end else if (P^ = $ED) and (P + 3 < PE) and ((P + 1)^ = $ED) then begin
          Inc(P, 2);
          N := P^;
          if P1 + N > PE1 then
            Exit;
          Inc(P);
          FillChar(P1^, N, P^);
          Inc(P1, N);
        end else begin
          P1^ := P^;
          Inc(P1);
        end;
        Inc(P);
      end;

      if P1 = PE1 then begin
        Result := True;
      end;
    end;
  end;

var
  MemoryPagesToRead: Byte;
  Is128K: Boolean;

  function LoadAPage(AMemory: TMemoryStream): Boolean;
  var
    W: Word;
    Pg: Integer;
    B: Byte;
    Le: Int64;
    N: Integer;
    Str: TMemoryStream;
  begin
    Result := False;
    if Stream.Position + 3 < Stream.Size then begin
      if (Stream.Read(W{%H-}, 2) = 2) and (Stream.Read(B{%H-}, 1) = 1) then begin
        W := LEtoN(W);
        Le := W;
        //
        if Stream.Position + Le > Stream.Size then
          Exit;

        if Is128K then begin
          if (B >= 3) and (B <= 10) then
            Pg := B - 3
          else
            Result := True; // ignore these

        end else
          case B of
            4:
              Pg := 2;
            5:
              Pg := 0;
            8:
              Pg := 5;
          otherwise
            Result := True; // ignore these
          end;

        if Result then begin
          Stream.Seek(Le, TSeekOrigin.soCurrent);
          Exit;
        end;

        case Pg of
          0, 2, 5:
            N := 2 - Pg div 2;
        otherwise
          N := (7 + 2 * Pg) div 3;
        end;

        if (MemoryPagesToRead shr Pg) and 1 = 0 then
          Exit;
        MemoryPagesToRead := MemoryPagesToRead and (not (1 shl Pg));

        N := N * KB16;

        if W = $FFFF then begin
          Result := (AMemory.Size >= N + Le) and (Stream.Read((PByte(AMemory.Memory) + N)^, Le) = Le);
          Exit;
        end;

        Str := TMemoryStream.Create;
        try
          Str.Size := Le;
          if Stream.Read(Str.Memory^, Le) = Le then
            Result := ReadFromCompressed(Str, AMemory, N, KB16);
        finally
          Str.Free;
        end;

      end;
    end;
  end;

var
  HeadersRead: Integer;
  //Proc: TProcessor;
  Header1: THeader1;
  Header2: THeader2;
  Header3: THeader3;
  WasPaused: Boolean;
  MemRead: Boolean;
  Len1: Integer;
  MemStream: TMemoryStream;
  BanksCount: Integer;
  I: Integer;
  State: TSpectrumInternalState;
  CheckEndOfStream: TFourBytes;
  HasV3: Boolean;
  B: Byte;
  N: Int32;
  Str: TMemoryStream;

begin
  Result := False;

  if Stream = nil then
    Exit;
  if FSpectrum = nil then
    Exit;
  if not FSpectrum.IsRunning then
    Exit;

  HeadersRead := 0;

  WasPaused := FSpectrum.Paused;
  try
    FSpectrum.Paused := True;
    try
      if Stream.Read(Header1{%H-}, SizeOf(Header1)) = SizeOf(Header1) then begin
        HeadersRead := 0;
        State := Default(TSpectrumInternalState);

        // Registers A and F (as well as A` and F` and I, R) are stored in order A, F (so we can treat AF stored as big endian)
        State.AF := BEtoN(Header1.AF);
        State.AF1 := BEtoN(Header1.AF1);
        State.IR := BEtoN(Header1.IR);

        // others stored little endian
        State.BC := LEtoN(Header1.BC);
        State.HL := LEtoN(Header1.HL);

        State.SP := LEtoN(Header1.SP);
        State.DE := LEtoN(Header1.DE);
        State.BC1 := LEtoN(Header1.BC1);
        State.DE1 := LEtoN(Header1.DE1);
        State.HL1 := LEtoN(Header1.HL1);
        State.Iy := LEtoN(Header1.Iy);
        State.Ix := LEtoN(Header1.Ix);
        State.IFF1 := Header1.IFF1 <> 0;
        State.IFF2 := Header1.IFF2 <> 0;

        if Header1.ViB = 255 then
          Header1.ViB := 1;
        WordRec(State.IR).Lo := (WordRec(State.IR).Lo and $7F) or Byte(Header1.ViB shl 7);
        State.BorderColour := (Header1.ViB shr 1) and %111;
        State.InterruptMode := Header1.ViB2 and %11;

        State.Reset7ffd();
        Is128K := False;

        if Header1.PC = 0 then begin // means we have header 2
          if Stream.Read(Header2{%H-}, SizeOf(Header2)) = SizeOf(Header2) then begin
            State.PC := LEtoN(Header2.PC);
            Header2.LeH3 := LeToN(Header2.LeH3);
            HasV3 := Header2.LeH3 in [54, 55];

            case Header2.HardwareMode of
              0, 1:
                State.SpectrumModel := TSpectrumModel.sm48K_issue_3;
              //2: SamRam
              3:
                if HasV3 then
                  State.SpectrumModel := TSpectrumModel.sm48K_issue_3
                else
                  State.SpectrumModel := TSpectrumModel.sm128K                                                 ;
              4, 5, 6:
                State.SpectrumModel := TSpectrumModel.sm128K;

              //7, 8:  Plus3

              12:
                State.SpectrumModel := TSpectrumModel.smPlus2;
              //13: Plus 2A
            otherwise
              State.SpectrumModel := TSpectrumModel.smNone;
            end;

            N := 17472;
            if State.SpectrumModel <> smNone then begin
              case State.SpectrumModel of
                TSpectrumModel.sm128K, TSpectrumModel.smPlus2:
                  begin
                    Is128K := True;
                    N := 17727;
                    State.Set7ffd(Header2.B35);
                    if Header2.B37 and $80 <> 0 then
                      State.SpectrumModel := TSpectrumModel.smPlus2;
                  end;
                TSpectrumModel.sm48K_issue_3:
                  if Header2.B37 and $80 <> 0 then begin
                    State.SpectrumModel := TSpectrumModel.sm16K_issue_3;
                  end;
              otherwise
              end;

              // AY...
              State.HasAy := Header2.B37 and %100 <> 0;
              State.AyState.ActiveRegisterNumber := Header2.AyActiveRegister;
              for I := 0 to 15 do
                State.AyState.Registers[I] := Header2.SoundRegisters[I];

              if HasV3 then begin
                if Stream.Read(Header3{%H-}, SizeOf(Header3)) = SizeOf(Header3) then begin
                  Header3.LoTStateCounter := Leton(Header3.LoTStateCounter);
                  State.T_States := Header3.HiTStateCounter;
                  State.T_States :=
                    ((State.T_States + 1) mod 4 + 1) * N - 1 - Int32(Header3.LoTStateCounter);

                  // read one more byte if LeH3 = 55
                  if Header2.LeH3 <> 55 then
                    HeadersRead := 3
                  else if Stream.Read(B{%H-}, 1) = 1 then
                    HeadersRead := 4;
                end;
              end else
                HeadersRead := 2;
            end;
          end;
        end else begin
          State.PC := LEtoN(Header1.PC);
          HeadersRead := 1;
          State.SpectrumModel := TSpectrumModel.sm48K_issue_3;
        end;

        if HeadersRead <> 0 then begin
          case State.SpectrumModel of
            TSpectrumModel.sm16K_issue_3:
              begin
                MemoryPagesToRead := %00100000; // 16K, we need to read only memory page 5
                if Header1.ViB2 and %100 <> 0 then // issue2 emulation
                  State.SpectrumModel := sm16K_issue_2;
              end;
            TSpectrumModel.sm48K_issue_3:
              begin
                MemoryPagesToRead := %00100101; // 48K, memory pages 0, 2, 5 needs to be read
                if Header1.ViB2 and %100 <> 0 then // issue2 emulation
                  State.SpectrumModel := sm48K_issue_2;
              end;
            otherwise
              MemoryPagesToRead := $FF; // 128K, all memory pages are there, we need to read all of them
          end;

          Len1 := Stream.Size - Stream.Position;
          if Len1 > 0 then begin
            MemRead := False;
            BanksCount := 0;
            for I := 0 to 7 do begin
              if (MemoryPagesToRead shr I) and 1 = 1 then
                Inc(BanksCount);
            end;

            MemStream := TMemoryStream.Create;
            try

              MemStream.Size := Int64(BanksCount) * KB16;
              if HeadersRead = 1 then begin
                if (Header1.ViB and %00100000) = 0 then begin // not compressed
                  //
                  Stream.Seek(Int64(KB48), TSeekOrigin.soCurrent);
                  if (Stream.Read({%H-}CheckEndOfStream[0], Length(CheckEndOfStream)) = Length(CheckEndOfStream))
                    and (CompareByte(CheckEndOfStream[0], EndOfMemStream[0], Length(CheckEndOfStream)) = 0)
                  then begin
                    Stream.Seek(-Int64(KB48) - Length(CheckEndOfStream), TSeekOrigin.soCurrent);
                    MemRead := Stream.Read(MemStream.Memory^, MemStream.Size) = MemStream.Size;
                  end;
                end else begin
                  Str := TMemoryStream.Create;
                  try
                    Str.Size := Len1;
                    if Stream.Read(Str.Memory^, Len1) = Len1 then begin
                      Str.Position := 0;
                      MemRead := ReadFromCompressed(Str, MemStream, 0, MemStream.Size);
                    end;

                  finally
                    Str.Free;
                  end;
                end;
              end else begin
                while Stream.Position < Stream.Size do begin
                  if not LoadAPage(MemStream) then
                    Break;
                  if Stream.Position = Stream.Size then begin
                    MemRead := MemoryPagesToRead = 0;
                  end;
                end;
              end;

              if MemRead then begin
                Result := State.SaveToSpectrum(FSpectrum)
                  and FSpectrum.Memory.LoadRamFromStream(MemStream);
              end;

            finally
              MemStream.Free;
            end;

          end;

        end;
      end;
    except
    end;
  finally
    FSpectrum.Paused := WasPaused;
  end;
end;

function TSnapshotZ80.SaveToStream(const Stream: TStream): Boolean;

  function PackRamPage(const StrPage, Mem: TMemoryStream): Int32;
  var
    P, PE: PByte;
    B: Byte;
    WasSingleED: Boolean;
    B4: TFourBytes;

  begin
    Result := 0;

    B4[0] := $ED;
    B4[1] := $ED;

    Mem.Position := 0;
    StrPage.Position := 0;
    P := PByte(StrPage.Memory);
    PE := P + (StrPage.Size - 1);
    WasSingleED := False;
    repeat
      B := 1;
      if not WasSingleED then begin
        while (P <> PE) and ((P + 1)^ = P^) and (B <> 255) do begin
          Inc(B);
          Inc(P);
        end;

        if B >= 2 then begin
          if (B >= 5) or (P^ = $ED) then begin
            B4[2] := B;
            B4[3] := P^;
            if Mem.Write(B4[0], 4) <> 4 then
              Exit(-1);
            B := 0;
            Inc(Result, 4);
          end;
        end;
      end;
      if B > 0 then begin
        Inc(Result, B);
        while B > 0 do begin
          if Mem.Write(P^, 1) <> 1 then
            Exit(-1);
          Dec(B);
        end;
        WasSingleED := P^ = $ED;
      end;
      Inc(P);
    until P > PE;

  end;

var
  Header1: THeader1;
  Header2: THeader2;
  Header3: THeader3;

  MemStream: TMemoryStream;
  BanksCount: Integer;
  I: Integer;
  State: TSpectrumInternalState;
  B: Byte;
  N: Int32;
  Str: TMemoryStream;
  Str2: TMemoryStream;
  Is128K: Boolean;
  W: Word;
  L: Int32;

begin
  Result := False;
  
  if Stream = nil then
    Exit;
  if FSpectrum = nil then
    Exit;
  if not FSpectrum.IsRunning then
    Exit;

  if State.LoadFromSpectrum(FSpectrum) then begin
    Header1 := Default(THeader1);

    // Registers A and F (as well as A` and F` and I, R) are stored in order A, F (so we can treat AF stored as big endian)
    Header1.AF := NtoBE(State.AF);
    Header1.AF1 := NtoBE(State.AF1);
    Header1.IR := NtoBE(State.IR);

    // others stored little endian
    Header1.BC := NtoLE(State.BC);
    Header1.HL := NtoLE(State.HL);

    Header1.SP := NtoLE(State.SP);
    Header1.DE := NtoLE(State.DE);
    Header1.BC1 := NtoLE(State.BC1);
    Header1.DE1 := NtoLE(State.DE1);
    Header1.HL1 := NtoLE(State.HL1);
    Header1.Iy := NtoLE(State.Iy);
    Header1.Ix := NtoLE(State.Ix);
    if State.IFF1 then
      Header1.IFF1 := 1;
    if State.IFF2 then
      Header1.IFF2 := 1;

    Header1.ViB := (WordRec(State.IR).Lo shr 7)
      or ((State.BorderColour and Byte(%111)) shl 1)
      or %100000
      ;

    Header1.ViB2 := State.InterruptMode;
    case State.SpectrumModel of
      TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm48K_issue_2:
        Header1.ViB2 := Header1.ViB2 or %100;
    end;

    Header2 := Default(THeader2);

    Header2.LeH3 := 54;
    Header2.PC := NtoLE(State.PC);

    Is128K := False;

    BanksCount := 8; // this is for 128K - all banks will be saved
    case State.SpectrumModel of
      TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm16K_issue_3:
        begin
          Header2.HardwareMode := 0;
          Header2.B37 := $80;
          BanksCount := 1; // for 16K, only bank 5 is going to be saved
        end;
      TSpectrumModel.sm48K_issue_2, TSpectrumModel.sm48K_issue_3:
        begin
          Header2.HardwareMode := 0;
          BanksCount := 3; // for 48K, banks 5, 2, 0 are going to be saved
        end;
      TSpectrumModel.sm128K:
        begin
          Header2.HardwareMode := 4;
          Is128K := True;
        end;
      TSpectrumModel.smPlus2:
        begin
          Header2.HardwareMode := 12;
          //Header2.B37 := $80;
          Is128K := True;
        end;
    end;

    if Is128K then
      Header2.B36 := State.Get7ffd();

    Header2.B37 := Header2.B37 or %11; // ?

    if State.HasAy then begin
      Header2.B37 := Header2.B37 or %100;

      Header2.AyActiveRegister := State.AyState.ActiveRegisterNumber;
      for I := 0 to 15 do
        Header2.SoundRegisters[I] := State.AyState.Registers[I];
    end;

    Header3 := Default(THeader3);

    if Is128K then
      N := 17727
    else
      N := 17472;
    Header3.HiTStateCounter := (State.T_States div N + 3) mod 4;
    Header3.LoTStateCounter := N - 1 - State.T_States mod N;
    Header3.LoTStateCounter := NtoLE(Header3.LoTStateCounter);

    if (Stream.Write(Header1, SizeOf(Header1)) = SizeOf(Header1))
      and (Stream.Write(Header2, SizeOf(Header2)) = SizeOf(Header2))
      and (Stream.Write(Header3, SizeOf(Header3)) = SizeOf(Header3))
    then begin
      Str := TMemoryStream.Create;
      try
        MemStream := TMemoryStream.Create;
        try
          I := 0;
          repeat
            case I of
              0..2:
                N := (10 - 5 * I) div 2;
            otherwise
              N := (3 * I) div 2 - 3;
            end;

            Str.Position := 0;
            if not FSpectrum.Memory.SaveToStream(N, False, Str) then
              Break;

            if Str.Size <> KB16 then
              Break;

            L := PackRamPage(Str, MemStream);
            if L <= 0 then
              Break;

            if L >= KB16 then begin
              L := KB16;
              Str2 := Str;
              W := $FFFF;
            end else begin
              W := L;
              Str2 := MemStream;
            end;

            W := NtoLE(W);
            if Is128K then
              B := N + 3
            else
              B := 8 - (5 - I) mod 5;

            Str2.Position := 0;
            if not (
              (Stream.Write(W, 2) = 2)
                and (Stream.Write(B, 1) = 1)
                and (Stream.Write(Str2.Memory^, L) = L)
              )
            then
              Break;

            if I = BanksCount then begin
              Result := True;
              Break;
            end;

            Inc(I);
          until False;

        finally
          MemStream.Free;
        end;
      finally
        Str.Free;
      end;
    end;

  end;
end;

end.

