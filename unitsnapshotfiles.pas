unit UnitSnapshotFiles;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitMemory, Z80Processor, SoundChipAY_3_8912;

const
  KB48 = 48 * 1024;
  KB16 = 16 * 1024;
  KB128 = 128 * 1024;

type

  { TSpectrumInternalState }

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
    PrefixByte: Byte;
    RemainingIntPinUp: UInt16;
    BorderColour: Byte;
    FlashState: UInt16;
    Ear: Byte;
    SpectrumModel: TSpectrumModel;
    LateTimings: Boolean;

    PagingEnabled: Boolean;
    MountedRamPage: Byte;
    MountedRomPage: Byte;
    ShadowScreenDisplay: Boolean;
    HasAy: Boolean;
    AyState: TAyState;

    procedure Set7ffd(const AValue: Byte);
    procedure Reset7ffd();
    function Get7ffd(): Byte;
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

  TSnapshotInternalSwan = class(TSnapshot)
  public
    class function GetScreenMem(const Stream: TStream; var ScreenMemArr: Array of Byte): Boolean; static;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;
  end;

implementation

{ TSpectrumInternalState }

procedure TSpectrumInternalState.Set7ffd(const AValue: Byte);
begin
  MountedRomPage := (AValue shr 4) and 1;
  MountedRamPage := AValue and 7;
  ShadowScreenDisplay := AValue and 8 <> 0;
  PagingEnabled := AValue and 32 = 0;
end;

procedure TSpectrumInternalState.Reset7ffd;
begin
  MountedRomPage := 0;
  MountedRamPage := 0;
  ShadowScreenDisplay := False;
  PagingEnabled := False;
end;

function TSpectrumInternalState.Get7ffd(): Byte;
begin
  Result := ((MountedRomPage and 1) shl 4) or (MountedRamPage and 7);
  if ShadowScreenDisplay then
    Result := Result or 8;
  if not PagingEnabled then
    Result := Result or 32;
end;

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
      PrefixByte := Proc.PrefixByte;

      RemainingIntPinUp := ASpectrum.RemainingIntPinUp;
      BorderColour := ASpectrum.CodedBorderColour;
      FlashState := ASpectrum.FlashState;
      Ear := ASpectrum.InternalEar;
      SpectrumModel := ASpectrum.SpectrumModel;
      LateTimings := ASpectrum.LateTimings;

      if ASpectrum.Is128KModel then begin
        MountedRamPage := ASpectrum.Memory.ActiveRamPageNo;
        MountedRomPage := ASpectrum.Memory.ActiveRomPageNo;
        ShadowScreenDisplay := ASpectrum.Memory.ShadowScreenDisplay;
        PagingEnabled := ASpectrum.IsPagingEnabled;
      end else
        Reset7ffd();

      HasAy := Assigned(ASpectrum.AYSoundChip);
      AyState.LoadFromAyChip(ASpectrum.AYSoundChip);

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
      ASpectrum.SetSpectrumModel(SpectrumModel, nil); // must be above all other, might trigger reset spectrum

      ASpectrum.LateTimings := LateTimings;

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
      Proc.PrefixByte := PrefixByte;

      ASpectrum.RemainingIntPinUp := RemainingIntPinUp;
      ASpectrum.CodedBorderColour := BorderColour and %111;
      ASpectrum.FlashState := FlashState;
      ASpectrum.InternalEar := Ear;

      if not ASpectrum.Is128KModel then
        Reset7ffd();

      ASpectrum.Memory.ActiveRamPageNo := MountedRamPage;
      Proc.ContendedHighBank := MountedRamPage and 1 <> 0; { #todo : different on +3! }
      ASpectrum.Memory.ActiveRomPageNo := MountedRomPage;
      ASpectrum.Memory.ShadowScreenDisplay := ShadowScreenDisplay;
      ASpectrum.IsPagingEnabled := PagingEnabled;
      if not HasAy then
        AyState.Clear;
      AyState.SaveToAYChip(ASpectrum.AYSoundChip);

      Exit(True);
    end;
  end;
  Result := False;
end;

{ TSnapshotInternalSwan }

class function TSnapshotInternalSwan.GetScreenMem(const Stream: TStream;
  var ScreenMemArr: array of Byte): Boolean;
var
  State: TSpectrumInternalState;
begin
  if Assigned(Stream) and (Length(ScreenMemArr) = 6912)
    and (Stream.Size >= SizeOf(TSpectrumInternalState) + Length(ScreenMemArr))
  then begin
    Stream.Position := 0;
    if Stream.Read(State{%H-}, SizeOf(State)) = SizeOf(State) then begin
      if State.ShadowScreenDisplay then begin
        if Stream.Size < SizeOf(State) + Length(ScreenMemArr) + 7 * KB16 then
          Exit(False);
        Stream.Seek(7 * Int64(KB16), TSeekOrigin.soCurrent);
      end;

      if Stream.Read(ScreenMemArr[0], Length(ScreenMemArr)) = Length(ScreenMemArr) then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TSnapshotInternalSwan.LoadFromStream(const Stream: TStream): Boolean;
var
  Proc: TProcessor;
  State: TSpectrumInternalState;
  WasPaused: Boolean;
  RamBanksCount: Integer;
  Mem: TMemory;

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

  Mem := FSpectrum.Memory;
  if Mem = nil then
    Exit;

  WasPaused := FSpectrum.Paused;
  try
    FSpectrum.Paused := True;

    Stream.Position := 0;
    if Stream.Read(State{%H-}, SizeOf(State)) = SizeOf(State) then begin
      case State.SpectrumModel of
        TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm16K_issue_3:
          RamBanksCount := 1;
        TSpectrumModel.sm48K_issue_2, TSpectrumModel.sm48K_issue_3:
          RamBanksCount := 3;
        TSpectrumModel.sm128K, TSpectrumModel.smPlus2, TSpectrumModel.smPlus2a, TSpectrumModel.smPlus3:
          RamBanksCount := 8;
      otherwise
        RamBanksCount := 0;
      end;

      Result := (RamBanksCount > 0)
        and (Stream.Size = SizeOf(State) + RamBanksCount * KB16)
        and State.SaveToSpectrum(FSpectrum)
        and Mem.LoadRamFromStream(Stream);
    end;
  finally
    FSpectrum.Paused := WasPaused;
  end;

end;

function TSnapshotInternalSwan.SaveToStream(const Stream: TStream): Boolean;
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
      if FSpectrum.Memory.SaveRamToStream(Stream) then
        Result := True;
  end;

end;

{ TSpectrumFile }

procedure TSpectrumFile.SetSpectrum(Value: TSpectrum);
begin
  FSpectrum := Value;
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

