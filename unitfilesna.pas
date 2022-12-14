unit UnitFileSna;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitMemory, Z80Processor, FastIntegers;

const
  KB48 = 48 * 1024;
  KB16 = 16 * 1024;

type

  TSpectrumInternalState = record
  public
    AF, BC, DE, HL: Word;
    AF1, BC1, DE1, HL1: Word;
    Ix, Iy: Word;
    PC, SP: Word;
    IR: Word;
    WZ: Word;
    IFF1, IFF2: Boolean;
    T_States: Integer;
    InterruptMode: Byte; // 0, 1, or 2
    Halt: Boolean;
    FlagsModified: Boolean;
    SkipInterruptCheck: Boolean;
    RemainingIntPinUp: UInt16;
    BorderColour: Byte;
    FlashState: UInt16;
    Ear: Byte;

    function LoadFromSpectrum(const ASpectrum: TSpectrum): Boolean;
    function SaveToSpectrum(const ASpectrum: TSpectrum): Boolean;
  end;

  TSpectrumFile = class(TObject)
  strict protected
    FSpectrum: TSpectrum;
  public
    procedure SetSpectrum(Value: TSpectrum);
  end;

  TSnapshot = class abstract (TSpectrumFile)
  public
    function LoadFromStream(const Stream: TStream): Boolean; virtual; abstract;
    function SaveToStream(const Stream: TStream): Boolean; virtual; abstract;
  end;

  TSnapshotFile = class abstract (TSnapshot)
  public
    class function GetDefaultExtension: String; virtual; abstract;
    function LoadFromFile(const FileName: String): Boolean;
    function SaveToFile(const FileName: String): Boolean;
  end;

  TSnapshotFileClass = class of TSnapshotFile;

  // https://sinclair.wiki.zxnet.co.uk/wiki/SNA_format
  // https://worldofspectrum.org/faq/reference/formats.htm#File
  TSnapshotSNA = class(TSnapshotFile)
  strict private
    type
      THeader = packed record
        I: Byte;
        L1, H1, E1, D1, C1, B1, F1, A1: Byte;
        L, H, E, D, C, B, IyL, IyH, IxL, IxH: Byte;
        Interrupt: Byte; // bit 2 contains IFF2, other bits unused
        R: Byte;
        F, A, P, S: Byte;
        IntMode: Byte; // 0, 1, or 2
        Border: Byte;
      end;

  strict private
    const
      SnaSize = SizeOf(THeader) + KB48;

  public
    class function GetDefaultExtension: String; override;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;
  end;

  TSnapshotSNAClass = class of TSnapshotSNA;

  // https://sinclair.wiki.zxnet.co.uk/wiki/Z80_format
  // https://worldofspectrum.org/faq/reference/z80format.htm
  TSnapshotZ80 = class(TSnapshotFile)
  strict private
    type
      THeader1 = packed record
        A, F: Byte;
        C, B, L, H: Byte;
        PC: Word;
        SpL, SpH: Byte;
        I, R: Byte;
        ViB: Byte; // various info
        E, D: Byte;
        C1, B1, E1, D1, L1, H1: Byte;
        A1, F1: Byte;
        IyL, IyH, IxL, IxH: Byte;
        IFF1, IFF2: Byte; // 0 - disabled, otherwise enabled
        ViB2: Byte; // various info
      end;

      THeader2 = packed record
        LeH3Lo, LeH3Hi: Byte;
        PC: Word;
        HardwareMode: Byte;
        B35, B36, B37, B38: Byte;
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

      TBlockHeader = packed record
        BlockLength: Word;
        Page: Byte;
      end;

  public
    class function GetDefaultExtension: String; override;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;
  end;

  TSnapshotZ80Class = class of TSnapshotZ80;

  TSnapshotInternal48 = class(TSnapshot)
  public
    class function GetScreenMem(const Stream: TStream; var ScreenMemArr: Array of Byte): Boolean; static;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;
  end;

implementation

{ TSpectrumInternalState }

function TSpectrumInternalState.LoadFromSpectrum(const ASpectrum: TSpectrum): Boolean;
var
  Proc: TProcessor;
begin
  if Assigned(ASpectrum) then begin
    Proc := ASpectrum.GetProcessor;
    if Assigned(Proc) then begin
      AF := Proc.RegAF;
      BC := Proc.RegBC;
      DE := Proc.RegDE;
      HL := Proc.RegHL;
      AF1 := Proc.RegAF1;
      BC1 := Proc.RegBC1;
      DE1 := Proc.RegDE1;
      HL1 := Proc.RegHL1;
      Ix := Proc.Ix;
      Iy := Proc.Iy;
      PC := Proc.RegPC;
      SP := Proc.RegSP;
      IR := Proc.RegIR;
      WZ := Proc.RegWZ;
      IFF1 := Proc.Iff1;
      IFF2 := Proc.Iff2;
      T_States := Proc.TStatesInCurrentFrame;
      InterruptMode := Proc.InterruptMode;
      Halt := Proc.Halt;
      FlagsModified := Proc.FlagsModified;
      SkipInterruptCheck := Proc.SkipInterruptCheck;

      RemainingIntPinUp := ASpectrum.RemainingIntPinUp;
      BorderColour := ASpectrum.CodedBorderColour;
      FlashState := ASpectrum.FlashState;
      Ear := ASpectrum.Ear;
      Exit(True);
    end;
  end;

  Result := False;
end;

function TSpectrumInternalState.SaveToSpectrum(const ASpectrum: TSpectrum): Boolean;
var
  Proc: TProcessor;
begin
  if Assigned(ASpectrum) then begin
    Proc := ASpectrum.GetProcessor;
    if Assigned(Proc) then begin
      Proc.RegAF := AF;
      Proc.RegBC := BC;
      Proc.RegDE := DE;
      Proc.RegHL := HL;
      Proc.RegAF1 := AF1;
      Proc.RegBC1 := BC1;
      Proc.RegDE1 := DE1;
      Proc.RegHL1 := HL1;
      Proc.Ix := Ix;
      Proc.Iy := Iy;
      Proc.RegPC := PC;
      Proc.RegSP := SP;
      Proc.RegIR := IR;
      Proc.RegWZ := WZ;
      Proc.Iff1 := IFF1;
      Proc.Iff2 := IFF2;
      Proc.TStatesInCurrentFrame := T_States;
      Proc.InterruptMode := InterruptMode;
      Proc.Halt := Halt;
      Proc.FlagsModified := FlagsModified;
      Proc.SkipInterruptCheck := SkipInterruptCheck;

      ASpectrum.RemainingIntPinUp := RemainingIntPinUp;
      ASpectrum.CodedBorderColour := BorderColour;
      ASpectrum.FlashState := FlashState;
      ASpectrum.Ear := Ear;

      Exit(True);
    end;
  end;
  Result := False;
end;

{ TSnapshotInternal48 }

class function TSnapshotInternal48.GetScreenMem(const Stream: TStream;
  var ScreenMemArr: array of Byte): Boolean;
begin
  if Assigned(Stream) and (Length(ScreenMemArr) = 6912) and (Stream.Size >= SizeOf(TSpectrumInternalState) + Length(ScreenMemArr)) then begin
    Stream.Seek(SizeOf(TSpectrumInternalState), TSeekOrigin.soBeginning);
    if Stream.Read(ScreenMemArr[0], Length(ScreenMemArr)) = Length(ScreenMemArr) then
      Exit(True);
  end;
  Result := False;
end;

function TSnapshotInternal48.LoadFromStream(const Stream: TStream): Boolean;
var
  Proc: TProcessor;
  State: TSpectrumInternalState;
  WasPaused: Boolean;
begin
  Result := False;

  if Stream = nil then
    Exit;

  if Stream.Size <> SizeOf(TSpectrumInternalState) + KB48 then
    Exit;

  if FSpectrum = nil then
    Exit;

  if not FSpectrum.IsRunning then
    Exit;

  Proc := FSpectrum.GetProcessor;
  if Proc = nil then
    Exit;

  WasPaused := FSpectrum.Paused;
  try
    FSpectrum.Paused := True;
    FSpectrum.ResetSpectrum;

    Stream.Position := 0;
    if Stream.Read(State{%H-}, SizeOf(State)) = SizeOf(State) then begin
      if Proc.GetMemory^.LoadRamFromStream(Stream) then begin
        Result := State.SaveToSpectrum(FSpectrum);
      end;
    end;
  finally
    FSpectrum.Paused := WasPaused;
  end;

end;

function TSnapshotInternal48.SaveToStream(const Stream: TStream): Boolean;
var
  Proc: TProcessor;
  State: TSpectrumInternalState;
begin
  Result := False;

  if Stream = nil then
    Exit;

  if FSpectrum = nil then
    Exit;

  if not FSpectrum.IsRunning then
    Exit;

  Proc := FSpectrum.GetProcessor;
  if Proc = nil then
    Exit;

  if State.LoadFromSpectrum(FSpectrum) then begin
    Stream.Position := 0;
    if Stream.Write(State, SizeOf(State)) = SizeOf(State) then
      if Proc.GetMemory^.SaveRamToStream(Stream) then
        Result := True;
  end;

end;

{ TSnapshotZ80 }

class function TSnapshotZ80.GetDefaultExtension: String;
begin
  Result := 'z80';
end;

function TSnapshotZ80.LoadFromStream(const Stream: TStream): Boolean;
var
  HeadersRead: Integer;
  Mem: PMemory;

  BlockHeader: TBlockHeader;
  MSBlock: TMemoryStream;

  function UnpackBlock(Position, Len: Integer): Boolean;
  var
    L, I, J: Integer;
    B: PByte;
    K: Byte;
    A: packed array of Byte;

  begin
    Result := False;
    if Len <= 0 then
      Exit;
    if Position < Mem^.RomSize then
      Exit;
    Position := Position - Mem^.RomSize;
    SetLength(A, Len);
    if Stream.Read(A[0], Len) = Len then begin
      if MSBlock = nil then begin
        MSBlock := TMemoryStream.Create;
        MSBlock.Size := KB16;
      end;
      B := PByte(MSBlock.Memory);
      I := 0;
      J := 0;
      while I < Len do begin
        begin
          if (A[I] = $ED) and (I <= Len - 4) and (A[I + 1] = $ED) then begin
            L := A[I + 2];
            K := A[I + 3];
            Inc(I, 4);
            if J + L > MSBlock.Size then begin
              L := MSBlock.Size - J;
              if L = 0 then
                Break;
            end;
            FillChar((B + J)^, L, K);
            Inc(J, L);
          end else begin
            K := A[I];
            Inc(I);
            if J >= MSBlock.Size then begin
              Break;
            end;
            (B + J)^ := K;
            Inc(J);
          end;
        end;
      end;

      if (I = Len) and (J = MSBlock.Size) then begin
        MSBlock.Position := 0;
        Result := Mem^.LoadRamBlockFromStream(MSBlock, Position, MSBlock.Size);
      end;

    end;
  end;
  
  function ReadMemFromCompressed3: Boolean;
  var
    MemAdr: Integer;
    BlLen: Integer;

  begin
    Result := False;

    MSBlock := nil;
    try
      while Stream.Position < Stream.Size - 3 do begin

        if Stream.Read(BlockHeader, 3) <> 3 then begin
          Result := False;
          Break;
        end;

        if BlockHeader.BlockLength = $FFFF then
          BlLen := KB16
        else
          BlLen := BlockHeader.BlockLength;

        if Stream.Position + BlLen > Stream.Size then begin
          Result := False;
          Break;
        end;

        case BlockHeader.Page of
          4:
            MemAdr := $8000;
          5:
            MemAdr := $C000;
          8:
            MemAdr := $4000;
        else
          MemAdr := -1;
          Stream.Seek(Int64(BlLen), TSeekOrigin.soCurrent);
        end;

        if MemAdr >= 0 then begin
          if BlockHeader.BlockLength = $FFFF then begin
            Result := Mem^.LoadRamBlockFromStream(Stream, MemAdr, BlLen);
          end else
            Result := UnpackBlock(MemAdr, BlLen);

          if not Result then
            Break;
        end;

      end;

    finally
      MSBlock.Free;
    end;

  end;

var
  Len1: PtrInt;

  function ReadMemFromCompressed2: Boolean;
  var
    P: PByte;
    A: packed array of Byte;
    I, J, K: Integer;
    MS: TMemoryStream;
  begin
    Result := False;

    SetLength(A, Len1);
                                                     
    if Stream.Read(A[0], Len1) = Len1 then begin
      MS := TMemoryStream.Create;
      try
        MS.Size := KB48;
        P := PByte(MS.Memory);

        J := 0;
        I := 0;
        while I < Len1 do begin
          if (A[I] = 0) and (I <= Len1 - 4) and
               (A[I + 1] = $ED) and (A[I + 2] = $ED) and (A[I + 3] = 0)
          then
            Inc(I, 4)
          else begin
            if (A[I] = $ED) and (I <= Len1 - 4) and (A[I + 1] = $ED) then begin
              K := A[I + 2];
              if J + K > KB48 then begin
                Break;
              end;
              FillChar(P^, K, A[I + 3]);
              Inc(J, K);
              Inc(P, K);
              Inc(I, 4);

            end else begin 
              if J >= KB48 then begin
                Break;
              end;
              P^ := A[I];
              Inc(J);
              Inc(P);
              Inc(I);
            end;
          end;

        end;

        if I = Len1 then begin
          MS.Position := 0;
          Result := Mem^.LoadRamFromStream(MS);
        end;

      finally
        MS.Free;;
      end;
    end;
  end;

var
  Header1: THeader1;
  Header2: THeader2;
  Header3: THeader3;
  B86: Byte;
  MemRead: Boolean;
  Proc: TProcessor;
  WasPaused: Boolean;
  WR: WordRec;
  W: Word;
  B: Byte;

begin
  Result := False;
  if FSpectrum = nil then
    Exit;
  if not FSpectrum.IsRunning then
    Exit;

  WasPaused := FSpectrum.Paused;
  FSpectrum.Paused := True;
  try
    Proc := FSpectrum.GetProcessor;
    if Assigned(Proc) then begin
      Mem := Proc.GetMemory;
      if Assigned(Stream) then begin
        try
          HeadersRead := 0;
          Stream.Position := 0;
          if Stream.Read(Header1{%H-}, SizeOf(Header1)) = SizeOf(Header1) then begin
            if Header1.ViB = 255 then
              Header1.ViB := 1;

            if Header1.PC = 0 then begin // means we have Header2
              if (Stream.Read(Header2{%H-}, SizeOf(Header2)) = SizeOf(Header2)) then begin
                //
                WR.Lo := Header2.LeH3Lo;
                WR.Hi := Header2.LeH3Hi;
                case Word(WR) of
                  23:
                    HeadersRead := 2;
                  54, 55:
                    begin
                      if Stream.Read(Header3{%H-}, SizeOf(Header3)) = SizeOf(Header3) then begin
                        if Word(WR) = 55 then begin
                          if Stream.Read(B86{%H-}, 1) = 1 then
                            HeadersRead := 4;
                        end else
                          HeadersRead := 3;
                      end;
                    end;
                otherwise
                end;

              end;
            end else
              HeadersRead := 1;

            if HeadersRead <> 0 then begin
              Len1 := Stream.Size - Stream.Position;
              if Len1 > 0 then begin
                MemRead := False;

                if HeadersRead = 1 then begin

                  if (Header1.ViB and %00100000) = 0 then begin // not compressed
                    if Len1 = KB48 + 4 then begin
                      // check if last four bytes are 00EDED00

                      Stream.Seek(Int64(KB48), TSeekOrigin.soCurrent);
                      if (Stream.Read(B, 1) = 1) and (B = 0) then begin
                        if (Stream.Read(B, 1) = 1) and (B = $ED) then begin
                          if (Stream.Read(B, 1) = 1) and (B = $ED) then begin
                            if (Stream.Read(B, 1) = 1) and (B = 0) then begin
                              Stream.Seek(-Int64(KB48) - 4, TSeekOrigin.soCurrent);
                              MemRead := Mem^.LoadRamBlockFromStream(Stream, 0, KB48);
                            end;
                          end;
                        end;
                      end;

                    end else if Len1 <= KB48 then begin
                      MemRead := Mem^.LoadRamFromStream(Stream);
                    end;
                  end else begin
                    // compressed
                    MemRead := ReadMemFromCompressed2;
                  end;

                end else
                  MemRead := ReadMemFromCompressed3;

                if MemRead then begin
                  Proc.RegA := Header1.A;
                  Proc.RegF := Header1.F;
                  Proc.RegC := Header1.C;
                  Proc.RegB := Header1.B;
                  Proc.RegL := Header1.L;
                  Proc.RegH := Header1.H;
                  if HeadersRead >= 2 then begin
                    Proc.RegPC := LEtoN(Header2.PC)
                  end else begin
                    Proc.RegPC := LEtoN(Header1.PC);
                  end;

                  WR.Lo := Header1.SpL;
                  WR.Hi := Header1.SpH;
                  Proc.RegSP := Word(WR);

                  Proc.RegI := Header1.I;
                  Proc.RegR := Header1.R and %01111111;

                  if Header1.ViB = $FF then
                    Header1.ViB := 1;
                  Proc.RegR := Proc.RegR or Byte(Header1.ViB shl 7);
                  FSpectrum.CodedBorderColour := (Header1.ViB shr 1) and %111;

                  Proc.RegE := Header1.E;
                  Proc.RegD := Header1.D;

                  Proc.RegC1 := Header1.C1;
                  Proc.RegB1 := Header1.B1;
                  Proc.RegE1 := Header1.E1;
                  Proc.RegD1 := Header1.D1;
                  Proc.RegL1 := Header1.L1;
                  Proc.RegH1 := Header1.H1;

                  Proc.RegA1 := Header1.A1;
                  Proc.RegF1 := Header1.F1;

                  WR.Lo := Header1.IyL;
                  WR.Hi := Header1.IyH;
                  Proc.Iy := Word(WR);
                  WR.Lo := Header1.IxL;
                  WR.Hi := Header1.IxH;
                  Proc.Ix := Word(WR);

                  Proc.Iff1 := Header1.IFF1 <> 0;
                  Proc.Iff2 := Header1.IFF2 <> 0;

                  Proc.InterruptMode := Header1.ViB2 and %11;
                  //
                  if HeadersRead = 1 then
                    Result := True
                  else begin
                    //
                    // ...
                    if HeadersRead >= 3 then begin
                      Proc.TStatesInCurrentFrame := Header3.HiTStateCounter;
                      Proc.TStatesInCurrentFrame := Proc.TStatesInCurrentFrame * 17472 + 17471;

                      W := LEtoN(Header3.LoTStateCounter);
                      Proc.TStatesInCurrentFrame := Proc.TStatesInCurrentFrame - Int32Fast(W);
                      //
                      // ...
                      Result := True;
                    end else
                      Result := True;
                  end;

                end;
              end;
            end;

          end;

        except
        end;

      end;
    end;
  finally
    FSpectrum.Paused := WasPaused;
  end;
end;

function TSnapshotZ80.SaveToStream(const Stream: TStream): Boolean;
var
  MS, MS2: TMemoryStream;
  BlockHeader: TBlockHeader;
  Mem: PMemory;

  function PackBlock: Boolean;
  var
    A: PByte;
    J, L, LL: Integer;
    BlockPos: Integer;
    Lon, K, KK: Byte;
    ED1, PrevED: Boolean;
    ArrEDED: packed array [0..3] of Byte;

  begin
    Result := False;
    MS.Position := 0;

    case BlockHeader.Page of
      4:
        BlockPos := $8000;
      5:
        BlockPos := $C000;
      8:
        BlockPos := $4000;
    otherwise
      BlockPos := -1;
    end;

    if BlockPos > 0 then begin

      if BlockPos < Mem^.RomSize then
        Exit;
      BlockPos := BlockPos - Mem^.RomSize;

      ArrEDED[0] := $ED;
      ArrEDED[1] := $ED;

      if MS2 = nil then begin
        MS2 := TMemoryStream.Create;
        MS2.Size := $2000;
      end;
      MS2.Position := 0;

      PrevED := False;
      A := PByte(MS.Memory);
      A := A + BlockPos;

      LL := 0;
      J := 0;
      repeat
        L := 1;
        Inc(LL);
        while (LL < KB16) {and (L < 255)} and ((A + L)^ = A^)  do begin
          Inc(L);
          Inc(LL);
        end;
        A := A + (L - 1);

        Lon := L mod 255;
        L := L div 255;
        while L >= 0 do begin
          if L > 0 then begin
            K := 255;
          end else
            K := Lon;

          ED1 := False;

          while K > 0 do begin
            if PrevED and (K > 1) then begin
              KK := K - 1;
              K := 1;
            end else
              KK := 0;
                              
            PrevED := False;

            if (K > 1) and ((A^ = $ED) or (K >= 5)) then begin
              ArrEDED[2] := K;
              ArrEDED[3] := A^;
              MS2.Write(ArrEDED[0], 4);
              Inc(J, 4);
            end else begin
              Inc(J, K);

              if (K = 1) and (A^ = $ED) then
                ED1 := True;

              while K > 0 do begin
                MS2.Write(A^, 1);
                Dec(K);
              end;
            end;
            K := KK;
          end;

          PrevED := ED1;

          Dec(L);
        end;

        if LL >= KB16 then
          Break;
        Inc(A);
      until False;

      BlockHeader.BlockLength := J;
      Stream.Write(BlockHeader, SizeOf(TBlockHeader));
      MS2.Position := 0;
      Stream.Write(MS2.Memory^, J);
      Result := True;
    end;
  end;

  function PackRam: Boolean;
  begin
    Result := False;

    MS2 := nil;
    try
      BlockHeader.Page := 4;
      if PackBlock then begin
        BlockHeader.Page := 5;
        if PackBlock then begin
          BlockHeader.Page := 8;
          Result := PackBlock;
        end;
      end;
    finally
      MS2.Free;
    end;
  end;

var
  Header1: THeader1;
  Header2: THeader2;
  Header3: THeader3;
  Proc: TProcessor;
  W: Word;

begin
  Result := False;

  if Stream = nil then
    Exit;
  if (FSpectrum = nil) or (not FSpectrum.IsRunning) then
    Exit;
  Proc := FSpectrum.GetProcessor;
  if Proc = nil then
    Exit;

  Mem := Proc.GetMemory;

  MS := TMemoryStream.Create;
  try
    if Mem^.SaveRamToStream(MS) then begin
      MS.Position := 0;
      Header1 := Default(THeader1);

      Header1.A := Proc.RegA;
      Header1.F := Proc.RegF;
      Header1.C := Proc.RegC;
      Header1.B := Proc.RegB;
      Header1.L := Proc.RegL;
      Header1.H := Proc.RegH;
      Header1.PC := 0; // !!!
      Header1.SpL := WordRec(Proc.RegSP).Lo;
      Header1.SpH := WordRec(Proc.RegSP).Hi;
                             
      Header1.H := Proc.RegH;
      Header1.I := Proc.RegI;
      Header1.R := Proc.RegR and %01111111;
      Header1.ViB := Proc.RegR shr 7;
      Header1.ViB := Header1.ViB or (Byte(FSpectrum.CodedBorderColour shl 1) and %1110);
      Header1.ViB := Header1.ViB or %00100000; // compressed

      Header1.E := Proc.RegE;
      Header1.D := Proc.RegD;
      Header1.C1 := Proc.RegC1;
      Header1.B1 := Proc.RegB1;
      Header1.E1 := Proc.RegE1;
      Header1.D1 := Proc.RegD1;
      Header1.L1 := Proc.RegL1;
      Header1.H1 := Proc.RegH1;
      Header1.A1 := Proc.RegA1;
      Header1.F1 := Proc.RegF1;
      Header1.IyL := WordRec(Proc.Iy).Lo;
      Header1.IyH := WordRec(Proc.Iy).Hi;
      Header1.IxL := WordRec(Proc.Ix).Lo;
      Header1.IxH := WordRec(Proc.Ix).Hi;

      if Proc.Iff1 then
        Header1.IFF1 := 1
      else
        Header1.IFF1 := 0;

      if Proc.Iff2 then
        Header1.IFF2 := 1
      else
        Header1.IFF2 := 0;

      Header1.ViB2 := Proc.InterruptMode;
      //
      //
      Header2 := Default(THeader2);
      W := 54;

      Header2.LeH3Lo := WordRec(W).Lo;
      Header2.LeH3Hi := WordRec(W).Hi;

      Header2.PC := NtoLE(Proc.RegPC);
      Header2.HardwareMode := 0; // 48 k spectrum

      Header2.B37 := %11;

      Header3 := Default(THeader3);
      W := Proc.TStatesInCurrentFrame mod 17472;
      Header3.HiTStateCounter := Proc.TStatesInCurrentFrame div 17472;
      W := 17471 - W;
      Header3.LoTStateCounter := NtoLE(W);

      if (Stream.Write(Header1, SizeOf(THeader1)) = SizeOf(THeader1))
        and (Stream.Write(Header2, SizeOf(THeader2)) = SizeOf(THeader2))
        and (Stream.Write(Header3, SizeOf(THeader3)) = SizeOf(THeader3))
      then
        Result := PackRam;
    end;

  finally
    MS.Free;
  end;
end;

{ TSpectrumFile }

procedure TSpectrumFile.SetSpectrum(Value: TSpectrum);
begin
  FSpectrum := Value;
end;

{ TSnapshotSNA }

class function TSnapshotSNA.GetDefaultExtension: String;
begin
  Result := 'sna';
end;

function TSnapshotSNA.LoadFromStream(const Stream: TStream): Boolean;
var
  Header: THeader;
  Mem: PMemory;
  Proc: TProcessor;
  WasPaused, WasRomWritteable: Boolean;
  WR: WordRec;
begin
  Result := False;
  if FSpectrum = nil then
    Exit;
  if not FSpectrum.IsRunning then
    Exit;                         
  Mem := nil;
  Proc := FSpectrum.GetProcessor;
  if Assigned(Proc) then
    Mem := Proc.GetMemory;

  if Assigned(Stream) and (Stream.Size = SnaSize) and (Mem^.RamSize = KB48) then begin
    try
      Stream.Position := 0;
      if (Stream.Read(Header{%H-}, SizeOf(Header)) = SizeOf(Header))
      then begin
        WasPaused := FSpectrum.Paused;
        FSpectrum.Paused := True;
        WasRomWritteable := Mem^.AllowWriteToRom;
        Mem^.AllowWriteToRom := False;
        try
          Proc.RegI := Header.I;
          Proc.RegL1 := Header.L1;
          Proc.RegH1 := Header.H1;
          Proc.RegE1 := Header.E1;
          Proc.RegD1 := Header.D1;
          Proc.RegC1 := Header.C1;
          Proc.RegB1 := Header.B1;
          Proc.RegF1 := Header.F1;
          Proc.RegA1 := Header.A1;

          Proc.RegL := Header.L;
          Proc.RegH := Header.H;
          Proc.RegE := Header.E;
          Proc.RegD := Header.D;
          Proc.RegC := Header.C;
          Proc.RegB := Header.B;

          WR.Lo := Header.IyL;
          WR.Hi := Header.IyH;
          Proc.Iy := Word(WR);
          WR.Lo := Header.IxL;
          WR.Hi := Header.IxH;
          Proc.Ix := Word(WR);

          Proc.Iff2 := (Header.Interrupt and %100) <> 0;

          Proc.RegR := Header.R;

          Proc.RegF := Header.F;
          Proc.RegA := Header.A;

          WR.Lo := Header.P;
          WR.Hi := Header.S;
          Proc.RegSP := Word(WR);
          Proc.InterruptMode := Header.IntMode;

          FSpectrum.CodedBorderColour := Header.Border;

          if Mem^.LoadRamFromStream(Stream) then begin
            {$push}{$R-}{$Q-}
            WR.Lo := Mem^.ReadByte(Proc.RegSP);
            Proc.RegSP := Proc.RegSP + 1;
            WR.Hi := Mem^.ReadByte(Proc.RegSP);
            Proc.RegSP := Proc.RegSP + 1;
            Proc.RegPC := Word(WR);
            // should we write two zero bytes below the stack (as advised - https://worldofspectrum.org/faq/reference/formats.htm)?
            Mem^.WriteByte(Proc.RegSP - 1, 0);
            Mem^.WriteByte(Proc.RegSP - 2, 0);
            {$pop}
            Proc.Iff1 := Proc.Iff2;

            Result := True;
          end;

        finally
          Mem^.AllowWriteToRom := WasRomWritteable;
          FSpectrum.Paused := WasPaused;
        end;
      end;
    except
    end;
  end;
end;

function TSnapshotSNA.SaveToStream(const Stream: TStream): Boolean;
var
  Header: THeader;
  Mem: PMemory;
  Proc: TProcessor;
  WR, W2: WordRec;
  WasPaused: Boolean;
begin
  Result := False;
  if Assigned(FSpectrum) and FSpectrum.IsRunning and Assigned(Stream) then begin
    Mem := nil;
    Proc := FSpectrum.GetProcessor;
    if Assigned(Proc) then
      Mem := Proc.GetMemory;
    if Mem^.RamSize = KB48 then begin
      WasPaused := FSpectrum.Paused;
      FSpectrum.Paused := True;
      try
        Header.I  := Proc.RegI;
        Header.L1 := Proc.RegL1;
        Header.H1 := Proc.RegH1;
        Header.E1 := Proc.RegE1;
        Header.D1 := Proc.RegD1;
        Header.C1 := Proc.RegC1;
        Header.B1 := Proc.RegB1;
        Header.F1 := Proc.RegF1;
        Header.A1 := Proc.RegA1;

        Header.L := Proc.RegL;
        Header.H := Proc.RegH;
        Header.E := Proc.RegE;
        Header.D := Proc.RegD;
        Header.C := Proc.RegC;
        Header.B := Proc.RegB;

        Word(WR) := Proc.Iy;
        Header.IyL := WR.Lo;
        Header.IyH := WR.Hi;
        Word(WR) := Proc.Ix;
        Header.IxL := WR.Lo;
        Header.IxH := WR.Hi;

        if Proc.Iff2 then
          Header.Interrupt := %100
        else
          Header.Interrupt := 0;

        Header.R := Proc.RegR;
        Header.F := Proc.RegF;
        Header.A := Proc.RegA;

        {$push}{$R-}{$Q-}
        Word(WR) := Proc.RegPC;
        Proc.RegSP := Proc.RegSP - 1;
        W2.Hi := Mem^.ReadByte(Proc.RegSP);
        Mem^.WriteByte(Proc.RegSP, WR.Hi);
        Proc.RegSP := Proc.RegSP - 1;
        W2.Lo := Mem^.ReadByte(Proc.RegSP);
        Mem^.WriteByte(Proc.RegSP, WR.Lo);
        {$pop}

        Header.P := WR.Lo;
        Header.S := WR.Hi;

        Header.IntMode := Proc.InterruptMode;
        Header.Border := FSpectrum.CodedBorderColour;

        Stream.Size := SnaSize;
        Stream.Position := 0;

        Result := (Stream.Write(Header, SizeOf(Header)) = SizeOf(Header))
                    and Mem^.SaveRamToStream(Stream);

        {$push}{$R-}{$Q-}
        Mem^.WriteByte(Proc.RegSP, W2.Lo);
        Proc.RegSP := Proc.RegSP + 1;
        Mem^.WriteByte(Proc.RegSP, W2.Hi);
        Proc.RegSP := Proc.RegSP + 1;
        {$pop}

      finally
        FSpectrum.Paused := WasPaused;
      end;
    end;
  end;
end;

{ TSnapshotFile }

function TSnapshotFile.LoadFromFile(const FileName: String): Boolean;
var
  Stream: TStream;
begin
  Result := False;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      Result := LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
  end;
end;

function TSnapshotFile.SaveToFile(const FileName: String): Boolean;
var
  Stream: TStream;
begin
  Result := False;
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Result := SaveToStream(Stream);
    finally
      Stream.Free;
    end;

  except
  end;
end;

end.

