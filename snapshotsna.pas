unit SnapshotSNA;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitFileSna, UnitSpectrum;

type

  // https://sinclair.wiki.zxnet.co.uk/wiki/SNA_format
  // https://worldofspectrum.org/faq/reference/formats.htm#File
  TSnapshotSNA = class(TSnapshotFile)
  strict private
    type

      THeader = packed record
        I: Byte;
        HL1: Word;
        DE1: Word;
        BC1: Word;
        AF1: Word;
        HL: Word;
        DE: Word;
        BC: Word;
        Iy: Word;
        Ix: Word;
        Interrupt: Byte; // bit 2 contains IFF2, other bits unused
        R: Byte;
        AF: Word;
        SP: Word;
        IntMode: Byte; // 0, 1, or 2
        Border: Byte;
      end;

  public
    class function GetDefaultExtension: String; override;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;
  end;


implementation

{ TSnapshotSNA }

class function TSnapshotSNA.GetDefaultExtension: String;
begin
  Result := 'sna';
end;

function TSnapshotSNA.LoadFromStream(const Stream: TStream): Boolean;
var
  Header: THeader;
  State: TSpectrumInternalState;
  RamPagesCount: Integer;
  P: PByte;
  HasDoubleSavedPage: Boolean;
  B: Byte;
  N: Int64;
  I: Integer;
  MS: TMemoryStream;
  WasPaused: Boolean;
  WR: WordRec;
  L: Int64;

begin
  Result := False;

  if Stream = nil then
    Exit;
  if FSpectrum = nil then
    Exit;
  if not FSpectrum.IsRunning then
    Exit;

  WasPaused := FSpectrum.Paused;
  try
    try
      FSpectrum.Paused := True;

      if Stream.Read(Header{%H-}, SizeOf(Header)) = SizeOf(Header) then begin
        State := Default(TSpectrumInternalState);

        State.AF := LEtoN(Header.AF);
        State.AF1 := LEtoN(Header.AF1);
        State.BC := LEtoN(Header.BC);
        State.DE := LEtoN(Header.DE);
        State.HL := LEtoN(Header.HL);
        State.BC1 := LEtoN(Header.BC1);
        State.DE1 := LEtoN(Header.DE1);
        State.HL1 := LEtoN(Header.HL1);
        State.SP := LEtoN(Header.SP);
        State.Iy := LEtoN(Header.Iy);
        State.Ix := LEtoN(Header.Ix);
        WordRec(State.IR).Hi := Header.I;
        WordRec(State.IR).Lo := Header.R;
        State.IFF2 := Header.Interrupt and %100 <> 0;
        State.InterruptMode := Header.IntMode and %11;
        State.BorderColour := Header.Border;

        State.IFF1 := State.IFF2;

        State.Reset7ffd();
        State.PC := FSpectrum.GetProcessor.RegPC;
        State.SpectrumModel := TSpectrumModel.smNone;
        L := Stream.Size - Stream.Position;
        HasDoubleSavedPage := False;
        if L = KB48 then begin
          if FSpectrum.IsIssue2 then
            State.SpectrumModel := TSpectrumModel.sm48K_issue_2
          else
            State.SpectrumModel := TSpectrumModel.sm48K_issue_3;
          RamPagesCount := 3;
        end else if L = KB16 then begin
          if FSpectrum.IsIssue2 then
            State.SpectrumModel := TSpectrumModel.sm16K_issue_2
          else
            State.SpectrumModel := TSpectrumModel.sm16K_issue_3;
          RamPagesCount := 1;
        end else begin
          if L = KB128 + 4 then begin
            RamPagesCount := 8;
          end else if L = KB128 + KB16 + 4 then begin
            RamPagesCount := 8;
            HasDoubleSavedPage := True;
          end else
            RamPagesCount := 0;

          if RamPagesCount = 8 then begin
            N := Stream.Position;
            if (Stream.Seek(Int64(KB48), TSeekOrigin.soCurrent) = KB48 + N)
              and (Stream.Read(State.PC, 2) = 2)
              and (Stream.Read(B{%H-}, 1) = 1)
              and (Stream.Seek(-(Int64(KB48) + 3), TSeekOrigin.soCurrent) = N)
            then begin
              State.Set7ffd(B);
              if (State.MountedRamPage in [2, 5]) = HasDoubleSavedPage then begin
                State.PC := LEtoN(State.PC);

                State.SpectrumModel := TSpectrumModel.sm128K;
              end;
            end;

          end;
        end;

        if State.SpectrumModel <> TSpectrumModel.smNone then begin
          MS := TMemoryStream.Create;
          try
            MS.Size := RamPagesCount * KB16;
            P := PByte(MS.Memory);
            if Stream.Read(P^, KB16) = KB16 then begin
              if RamPagesCount > 1 then begin
                Inc(P, KB16);
                if Stream.Read(P^, KB16) <> KB16 then
                  Abort;
                Inc(P, KB16);
                if State.SpectrumModel <> TSpectrumModel.sm128K then begin
                  if Stream.Read(P^, KB16) <> KB16 then
                    Abort;
                end else begin
                  case State.MountedRamPage of
                    2, 5:
                      Stream.Seek(Int64(KB16), TSeekOrigin.soCurrent);
                  otherwise
                    N := State.MountedRamPage;
                    N := ((5 * N + 2) div 7);
                    N := N * KB16;
                    if Stream.Read((P + N)^, KB16) <> KB16 then
                      Abort;
                  end;
                  Stream.Seek(Int64(4), TSeekOrigin.soCurrent);

                  for I := 0 to RamPagesCount - 1 do begin
                    case I of
                      2, 5:
                        ;
                    otherwise
                      if I <> State.MountedRamPage then begin
                        N := ((5 * I + 2) div 7);
                        N := N * KB16;
                        if Stream.Read((P + N)^, KB16) <> KB16 then
                          Abort;
                      end;
                    end;
                  end;
                end;

                if State.SaveToSpectrum(FSpectrum) then begin
                  MS.Position := 0;
                  if FSpectrum.Memory.LoadRamFromStream(MS) then begin
                    if State.SpectrumModel <> sm128K then begin
                      {$push}
                      {$R-}{$Q-}
                      WR.Lo := FSpectrum.Memory.ReadByte(FSpectrum.GetProcessor.RegSP);
                      FSpectrum.GetProcessor.RegSP := FSpectrum.GetProcessor.RegSP + 1;
                      WR.Hi := FSpectrum.Memory.ReadByte(FSpectrum.GetProcessor.RegSP);
                      FSpectrum.GetProcessor.RegSP := FSpectrum.GetProcessor.RegSP + 1;
                      FSpectrum.GetProcessor.RegPC := Word(WR);
                      // should we write two zero bytes below the stack (as advised - https://worldofspectrum.org/faq/reference/formats.htm)?
                      FSpectrum.Memory.WriteByte(FSpectrum.GetProcessor.RegSP - 1, 0);
                      FSpectrum.Memory.WriteByte(FSpectrum.GetProcessor.RegSP - 2, 0);
                      {$pop}
                    end;
                    Result := True;
                  end;
                end;
              end;
            end;

          finally
            MS.Free;
          end;
        end;
      end;
    except
      Result := False;
    end;
  finally
    FSpectrum.Paused := WasPaused;
  end;
end;

function TSnapshotSNA.SaveToStream(const Stream: TStream): Boolean;
var
  Header: THeader;
  State: TSpectrumInternalState;
  MemStr: TMemoryStream;
  P: PByte;
  I: Integer;
  RamBanksCount: Integer;
  L: Integer;
  MS16: TMemoryStream;
              
  W: Word;

begin
  Result := False;

  if Stream = nil then
    Exit;
  if FSpectrum = nil then
    Exit;
  if not FSpectrum.IsRunning then
    Exit;

  try
    if State.LoadFromSpectrum(FSpectrum) then begin

      MemStr := TMemoryStream.Create;
      try
        case State.SpectrumModel of
          TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm16K_issue_3:
            RamBanksCount := 1;

          TSpectrumModel.sm48K_issue_2, TSpectrumModel.sm48K_issue_3:
            RamBanksCount := 3;

        otherwise
          RamBanksCount := 8;
        end;

        if RamBanksCount < 8 then begin

          L := KB48;
          MemStr.Size := L;
          MemStr.Position := 0;
          if not FSpectrum.Memory.SaveRamToStream(MemStr) then
            Abort;

          if RamBanksCount = 1 then begin
            P := PByte(MemStr.Memory) + KB16;
            //
            FillChar(P^, KB16 * 2, $FF);
          end;

          {$push}{$R-}{$Q-}
          W := State.SP - 1;
          W := W - KB16;
          P := PByte(MemStr.Memory);
          (P + W)^ := WordRec(State.PC).Hi;

          W := W - 1;
          (P + W)^ := WordRec(State.PC).Lo;

          State.SP := State.SP - 2;
          {$pop}

        end else begin
          L := 8 * KB16 + 4;

          case State.MountedRamPage of
            2, 5:
              L := L + KB16;
          otherwise
          end;
          MemStr.Size := L;
          MemStr.Position := 0;

          MS16 := TMemoryStream.Create;
          try
            MS16.Size := KB16;

            MS16.Position := 0;
            if not FSpectrum.Memory.SaveToStream(5, False, MS16) then
              Abort;
            P := PByte(MemStr.Memory);
            Move(MS16.Memory^, P^, KB16);
            Inc(P, KB16);

            MS16.Position := 0;
            if not FSpectrum.Memory.SaveToStream(2, False, MS16) then
              Abort;
            Move(MS16.Memory^, P^, KB16);
            Inc(P, KB16);

            MS16.Position := 0;
            if not FSpectrum.Memory.SaveToStream(State.MountedRamPage, False, MS16) then
              Abort;
            Move(MS16.Memory^, P^, KB16);
            Inc(P, KB16);

            P^ := WordRec(State.PC).Lo;
            Inc(P);
            P^ := WordRec(State.PC).Hi;
            Inc(P);
            P^ := State.Get7ffd();
            Inc(P);
            P^ := 0;
            Inc(P);

            for I := 0 to 7 do begin
              case I of
                2, 5:
                  ;
              otherwise
                if I <> State.MountedRamPage then begin
                  MS16.Position := 0;
                  if not FSpectrum.Memory.SaveToStream(I, False, MS16) then
                    Abort;                   
                  Move(MS16.Memory^, P^, KB16);
                  Inc(P, KB16);
                end;
              end;
            end;

          finally
            MS16.Free;
          end;
        end;

        if MemStr.Size = L then begin
          Header := Default(THeader);

          Header.AF := NtoLE(State.AF);
          Header.AF1 := NtoLE(State.AF1);
          Header.BC := NtoLE(State.BC);
          Header.DE := NtoLE(State.DE);
          Header.HL := NtoLE(State.HL);
          Header.BC1 := NtoLE(State.BC1);
          Header.DE1 := NtoLE(State.DE1);
          Header.HL1 := NtoLE(State.HL1);

          Header.Iy := NtoLE(State.Iy);
          Header.Ix := NtoLE(State.Ix);

          Header.I := WordRec(State.IR).Hi;
          Header.R := WordRec(State.IR).Lo;

          if State.IFF2 then
            Header.Interrupt := %100;
          Header.IntMode := State.InterruptMode;
          Header.Border := State.BorderColour;

          Header.SP := NtoLE(State.SP);

          MemStr.Position := 0;
          P := PByte(MemStr.Memory);
          Result := (Stream.Write(Header, SizeOf(Header)) = SizeOf(Header))
            and (Stream.Write(P^, L) = L);
        end;
      finally
        MemStr.Free;
      end;
    end;
  except
    Result := False;
  end;

end;

end.

