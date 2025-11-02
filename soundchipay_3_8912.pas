unit SoundChipAY_3_8912;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils, UnitSoundPlayer;

type

  TFuncTicks = function(): Int64 of object;

  TSoundAY_3_8912 = class(TObject)
  public
    type
      TOutputMode = (omMono, omStereoABC, omStereoACB);

  strict private
    class var
      Vols: array [0..15] of Integer;
      HalfVols: array [0..15] of Integer;

  strict private
    FRegToneFrequency: array [0..2] of Word; // reg A, B, C

    FNoiseWidth: Byte;
    FMixer: Byte;
    FVolumeA: Byte;
    FVolumeB: Byte;
    FVolumeC: Byte;

    FEnvelopeDuration: Word;
    FEnvelopeShape: Byte;
    FIOPortA: Byte;

    FReg15: Byte;
    FFadingValue: Byte;

    FActiveRegisterPointer: PByte;
    FActiveRegisterMask: Byte;

    OutputChannelsLengths: array [0..2] of Integer;
    OutputChCurrentPositions: array [0..2] of Integer;

    OutputChannelsVolumes: array [0..2] of Integer;

    FEnvelopePosition: Integer;
    FEnvelopeValue: Byte;
    FEnvelopePeriod: Integer;
    FEnvelopeDirection: -1..1; // falling, hold, rising
    FEnvelopeHold: Boolean;
    FEnvelopeHoldUp: Boolean;
    FEnvelopeAlter: Boolean;

    NoisePosition: Integer;
    NoiseHalfPeriod: Integer;
    NoiseLevel: Integer;
    NoiseGenerator: UInt32;
    NoiseOnCh: array [0..2] of Integer;

    StartFadingTicks: Int64;

    FOutputMode: TOutputMode;
    FOnCheckTicks: TFuncTicks;
    FIsStereo: Boolean;

    Vols1: array [0..2, 0..15] of Integer;
    Vols2: array [0..2, 0..15] of Integer;

    procedure InitRegPointers();

    procedure ResetEnvelope;
    procedure SetOnCheckTicks(const AValue: TFuncTicks);
    function EmptyCheckTicks(): Int64;
    procedure SetOutputMode(AValue: TOutputMode);

  private
    FActiveRegisterNum: Byte;
    FRegPointers: array [0..15] of PByte;

    procedure RecalcOutputChannels;
    procedure StopFading; inline;

  private
    const
      RegMasks: array [0..15] of Byte = (
        $FF, $0F, $FF, $0F, $FF, $0F, $1F, $FF,
        $1F, $1F, $1F, $FF, $FF, $0F, $FF, $FF
      );

  private
    class procedure Init; static;

  public
    procedure Fill(const ABeeper: Int32; P: PInt16; const Len: Integer);

    constructor Create;
    function GetRegValue(): Byte;
    procedure SetRegValue(AValue: Byte);
    procedure Reset();
    procedure SetActiveRegNum(const ARegNumber: Byte);

    property OutputMode: TOutputMode read FOutputMode write SetOutputMode;
    property OnCheckTicks: TFuncTicks read FOnCheckTicks write SetOnCheckTicks;
  end;

  TAyState = record
  private
    FActiveRegisterNumber: Byte;
    FRegisters: array[0..15] of Byte;

    function GetRegisters(I: Integer): Byte;
    procedure SetRegisters(I: Integer; AValue: Byte);

    class operator Initialize(var X: TAyState);
  public
    procedure Clear;
    procedure LoadFromAyChip(Ay: TSoundAY_3_8912);
    procedure SaveToAYChip(Ay: TSoundAY_3_8912);

    property Registers[I: Integer]: Byte read GetRegisters write SetRegisters;
    property ActiveRegisterNumber: Byte read FActiveRegisterNumber write FActiveRegisterNumber;
  end;

implementation

{ TSoundAY_3_8912 }

procedure TSoundAY_3_8912.InitRegPointers();
begin
  FRegPointers[0] := @WordRec(FRegToneFrequency[0]).Lo;
  FRegPointers[1] := @WordRec(FRegToneFrequency[0]).Hi;
  FRegPointers[2] := @WordRec(FRegToneFrequency[1]).Lo;
  FRegPointers[3] := @WordRec(FRegToneFrequency[1]).Hi;
  FRegPointers[4] := @WordRec(FRegToneFrequency[2]).Lo;
  FRegPointers[5] := @WordRec(FRegToneFrequency[2]).Hi;
  FRegPointers[6] := @FNoiseWidth;
  FRegPointers[7] := @FMixer;
  FRegPointers[8] := @FVolumeA;
  FRegPointers[9] := @FVolumeB;
  FRegPointers[10] := @FVolumeC;
  FRegPointers[11] := @WordRec(FEnvelopeDuration).Lo;
  FRegPointers[12] := @WordRec(FEnvelopeDuration).Hi;
  FRegPointers[13] := @FEnvelopeShape;
  FRegPointers[14] := @FIOPortA;
  FRegPointers[15] := @FReg15;
end;

procedure TSoundAY_3_8912.RecalcOutputChannels;
var
  L: Integer;
  Ch: Integer;

begin
  if FEnvelopeDuration = 0 then
    FEnvelopePeriod := 1
  else begin
    FEnvelopePeriod := FEnvelopeDuration;
    FEnvelopePeriod := (FEnvelopePeriod * 3584 + 281) div 563;
  end;

  NoiseHalfPeriod := FNoiseWidth;
  NoiseHalfPeriod := (NoiseHalfPeriod * 112 + 281) div 563;

  for Ch := 0 to 2 do begin
    NoiseOnCh[Ch] := ((FMixer shr (Ch + 3)) and 1) * $0F;
    if FMixer and (1 shl Ch) = 0 then begin
      L := (Integer(FRegToneFrequency[Ch]) * 224 + 281) div 563;
      if L >= 1 then
        OutputChannelsLengths[Ch] := L
      else
        OutputChannelsLengths[Ch] := 1;
    end else
      OutputChannelsLengths[Ch] := 1;

    OutputChannelsVolumes[Ch] := FRegPointers[Ch or 8]^;
  end;
end;

procedure TSoundAY_3_8912.StopFading;
begin
  FFadingValue := 0;
  StartFadingTicks := 0;
end;

class procedure TSoundAY_3_8912.Init;
// The actual values measured, listed on this site:
//   [archived on 2021-01-18, as the original link is not available now (2023-11-04)]:
// https://web.archive.org/web/20210118234335/https://forum.tslabs.info/viewtopic.php?f=6&t=539
//const
//  CVols: array [0..15] of UInt16 = (
//    $0000, $028F, $03B3, $0564, $07DC, $0BA9, $1083, $1B7C, $2068, $347A, $4ACE, $5F72, $7E16, $A2A4, $CE3A, $FFFF
//  );
//
// Another site (https://www.cpcwiki.eu/index.php/PSG#D.2FA_converter_table) lists these:
//const
//  CVols: array [0..15] of UInt16 = (
//    0, 231, 695, 1158, 2084, 2779, 4168, 6716, 8105, 13200, 18294, 24315, 32189, 40757, 52799, 65535
//  );
var
  I: Integer;
  N: Integer;
  D: Double;
  Sqrt2: Double;

begin
  Sqrt2 := System.Sqrt(2.0);
  N := Int16.MaxValue div 31;
  D := N;
  for I := 15 downto 0 do begin
    // we could use the predefined values from one of the arrays above
    //D := CVols[I] / 62.0;

    // or the formula (described here: https://www.cpcwiki.eu/index.php/PSG#0Ah_-_Channel_C_Volume_.280-0Fh.3Dvolume.2C_10h.3Duse_envelope_instead.29)
    // amplitude = maxVolume / sqrt(2)^(15-i)
    // where maxVolume is the highest volume level.
    // This formula matches the diagram shown in AY datasheet
    if I = 0 then
      D := 0.0; // set the lowest volume to zero

    N := Trunc(D + 0.5);
    Vols[I] := N; // used in mono output by all channels

    N := Trunc(D / 2.0 + 0.5);
    HalfVols[I] := N; // used in stereo "middle channel"

    D := D / Sqrt2;
  end;
end;

procedure TSoundAY_3_8912.SetOutputMode(AValue: TOutputMode);
var
  I: Integer;
  N: Integer;
  M: Integer;
  R: Integer;
begin
  if FOutputMode <> AValue then begin
    FOutputMode := AValue;
    FIsStereo := AValue <> TOutputMode.omMono;

    if FIsStereo then begin
      case AValue of
        TOutputMode.omStereoABC:
          R := 2; // right chanel - register C
      otherwise // TOutputMode.omStereoACB:
        R := 1; // right chanel - register B
      end;

      M := 3 - R; // middle channel

      for I := 0 to 15 do begin
        N := Vols[I];
        // stereo output (A-B-C or A-C-B)
        Vols1[0, I] := N; // left stereo channel - register A - full output
        Vols2[R, I] := N; // right stereo channel - register C - full output

        N := HalfVols[I];
        Vols1[M, I] := N; // "middle" channel register
        Vols2[M, I] := N; // - half of volume to each stereo output channel

        Vols1[R, I] := 0; // left stereo channel - register C (no output)
        Vols2[0, I] := 0; // right stereo channel - register A (no output)
      end;
    end;
  end;
end;

procedure TSoundAY_3_8912.SetOnCheckTicks(const AValue: TFuncTicks);
begin
  if not Assigned(AValue) then
    FOnCheckTicks := @EmptyCheckTicks
  else
    FOnCheckTicks := AValue;
end;

function TSoundAY_3_8912.EmptyCheckTicks: Int64;
begin
  Result := 0;
end;

procedure TSoundAY_3_8912.ResetEnvelope;
begin
  { envelopes
          cont attack alter hold
   ----------------------------------------------------------------------
    0:      0     0     0    0    \__________    single decay then off
    1:      0     0     0    1    \__________    single decay then off
    2:      0     0     1    0    \__________    single decay then off
    3:      0     0     1    1    \__________    single decay then off
    4:      0     1     0    0    /__________    single attack then off
    5:      0     1     0    1    /__________    single attack then off
    6:      0     1     1    0    /__________    single attack then off
    7:      0     1     1    1    /__________    single attack then off
    8:      1     0     0    0    \\\\\\\\\\\    repeated decay
    9:      1     0     0    1    \__________    single decay then off
   10:      1     0     1    0    \/\/\/\/\/\    repeated decay-attack
   11:      1     0     1    1    \``````````    single decay then hold
   12:      1     1     0    0    ///////////    repeated attack
   13:      1     1     0    1    /``````````    single attack then hold
   14:      1     1     1    0    /\/\/\/\/\/    repeated attack-decay
   15:      1     1     1    1    /__________    single attack then off
  }

  FEnvelopePosition := 0;

  if FEnvelopeShape and %0100 = 0 then begin
    FEnvelopeValue := $0F;
    FEnvelopeDirection := -1;
  end else begin
    FEnvelopeValue := 0;
    FEnvelopeDirection := 1;
  end;

  FEnvelopeHold := (FEnvelopeShape and %1001) <> %1000;
  FEnvelopeHoldUp := FEnvelopeShape in [11, 13];
  FEnvelopeAlter := (FEnvelopeShape and %0010) <> 0; // it only matters when hold is false, so this is good
end;

procedure TSoundAY_3_8912.Fill(const ABeeper: Int32; P: PInt16; const Len: Integer);
var
  J, K, L: Integer;
  N: Integer;
  PEnd: PInt16;
  SoundPlayerCh1: Integer;
  SoundPlayerCh2: Integer;

begin
  if FIsStereo then
    PEnd := P + 2 * Len
  else
    PEnd := P + Len;

  while P < PEnd do begin

    SoundPlayerCh1 := 0;
    SoundPlayerCh2 := 0;

    Inc(NoisePosition);
    if NoisePosition >= NoiseHalfPeriod then begin
      NoisePosition := 0;
      // recalculate current noise level - low or high
      // https://www.cpcwiki.eu/index.php/PSG#06h_-_Noise_Frequency_.285bit.29
      NoiseLevel := ((NoiseGenerator xor UInt32(NoiseLevel)) and 1) * $0F;
      NoiseGenerator :=
        ((((NoiseGenerator shr 3) xor NoiseGenerator) shl 16) or (NoiseGenerator shr 1)) and $1FFFF;
    end;

    for J := 0 to 2 do begin
      L := OutputChannelsLengths[J];
      K := OutputChCurrentPositions[J] mod L;

      if K shl 1 < L then begin

        N := OutputChannelsVolumes[J];
        if N and $10 <> 0 then
          N := FEnvelopeValue;

        N := N and (NoiseLevel or NoiseOnCh[J]);

        if FIsStereo then begin
          SoundPlayerCh1 := SoundPlayerCh1 + Vols1[J, N];
          SoundPlayerCh2 := SoundPlayerCh2 + Vols2[J, N];
        end else
          SoundPlayerCh1 := SoundPlayerCh1 + Vols[N];

      end;

      OutputChCurrentPositions[J] := K + 1;
    end;

    P^ := (SoundPlayerCh1 * TSoundPlayer.Volume + ABeeper) shr 2;
    if FIsStereo then begin
      Inc(P);
      P^ := (SoundPlayerCh2 * TSoundPlayer.Volume + ABeeper) shr 2;
    end;

    if FEnvelopeDirection <> 0 then begin
      Inc(FEnvelopePosition);
      K := (FEnvelopePosition * 16) div FEnvelopePeriod;

      if K >= 16 then begin
        if FEnvelopeHold then begin
          FEnvelopeDirection := 0;
          if FEnvelopeHoldUp then
            FEnvelopeValue := 15
          else
            FEnvelopeValue := 0;
        end else begin
          if FEnvelopeAlter then
            FEnvelopeDirection := -FEnvelopeDirection;

          if FEnvelopeDirection = 1 then begin
            FEnvelopeValue := 0;
          end else
            FEnvelopeValue := 15;
        end;
        FEnvelopePosition := 0;
      end else begin
        if FEnvelopeDirection = 1 then begin
          FEnvelopeValue := K;
        end else
          FEnvelopeValue := 15 - K;
      end;
    end;

    Inc(P);
  end;
end;

constructor TSoundAY_3_8912.Create;
begin
  inherited Create;

  FOutputMode := TOutputMode.omStereoABC;
  SetOutputMode(TOutputMode.omMono);
  SetOnCheckTicks(nil);
  InitRegPointers();
  Reset();
end;

function TSoundAY_3_8912.GetRegValue: Byte;
var
  T: Int64;
begin
  Result := FActiveRegisterPointer^;
  if (StartFadingTicks <> 0) and (FActiveRegisterNum >= 16) then begin
    T := (FOnCheckTicks() - StartFadingTicks) div 62324;

    if T <= 7 then begin
      Result := Result and Byte(255 shr T);
    end else begin
      Result := 0;
    end;

    if Result = 0 then
      StopFading;

  end;

end;

procedure TSoundAY_3_8912.SetRegValue(AValue: Byte);
begin
  if FActiveRegisterNum <= 15 then begin
    AValue := AValue and FActiveRegisterMask;

    if AValue <> FActiveRegisterPointer^ then begin
      FActiveRegisterPointer^ := AValue;
      RecalcOutputChannels;
    end;

    // Setting envelope shape is the only event that resets envelope.
    // Resets envelope even when setting to the same value.
    if FActiveRegisterNum = 13 then
      ResetEnvelope;

  end else begin
    FFadingValue := AValue;
    StartFadingTicks := FOnCheckTicks();
  end;
end;

procedure TSoundAY_3_8912.Reset();
var
  I: Integer;
begin
  for I := 0 to 2 do begin
    FRegToneFrequency[I] := 0;
    OutputChannelsLengths[I] := 1;
    OutputChCurrentPositions[I] := 0;
    OutputChannelsVolumes[I] := $0F;
    NoiseOnCh[I] := $0F;
  end;

  NoisePosition := 0;
  NoiseHalfPeriod := 1;
  NoiseGenerator := $1FFFF;
  NoiseLevel := $0F;

  FNoiseWidth := 0;
  FMixer := $FF;
  FVolumeA := 0;
  FVolumeB := 0;
  FVolumeC := 0;

  FEnvelopeDuration := 0;
  FEnvelopeShape := 0;
  FIOPortA := 0;

  FReg15 := 0;

  StopFading;

  FActiveRegisterNum := 1;

  ResetEnvelope;

  SetActiveRegNum(0);
  FActiveRegisterPointer^ := 1;
  SetRegValue(0);
end;

procedure TSoundAY_3_8912.SetActiveRegNum(const ARegNumber: Byte);
begin
  if ARegNumber <> FActiveRegisterNum then begin
    FActiveRegisterNum := ARegNumber;
    if ARegNumber <= 15 then begin
      FActiveRegisterPointer := FRegPointers[ARegNumber];

      FActiveRegisterMask := RegMasks[ARegNumber];
    end else begin
      // If value greater than 15 is set, we have fading "register"
      //  (https://worldofspectrum.org/forums/discussion/23327/)
      FActiveRegisterPointer := @FFadingValue;
      FActiveRegisterMask := $FF;
    end;
  end;
end;

{ TAyState }

class operator TAyState.Initialize(var X: TAyState);
begin
  X.Clear;
end;

function TAyState.GetRegisters(I: Integer): Byte;
begin
  case I of
    Low(FRegisters)..High(FRegisters):
      Result := FRegisters[I];
  otherwise
    Result := 0;
  end;
end;

procedure TAyState.SetRegisters(I: Integer; AValue: Byte);
begin
  case I of
    Low(FRegisters)..High(FRegisters):
      FRegisters[I] := AValue and TSoundAY_3_8912.RegMasks[I];
  otherwise
  end;
end;

procedure TAyState.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);

end;

procedure TAyState.LoadFromAyChip(Ay: TSoundAY_3_8912);
var
  I: Integer;
begin
  if Assigned(Ay) then begin
    for I := 0 to 15 do begin
      Registers[I] := Ay.FRegPointers[I]^;
    end;
    FActiveRegisterNumber := Ay.FActiveRegisterNum;
  end else
    Clear;
end;

procedure TAyState.SaveToAYChip(Ay: TSoundAY_3_8912);
var
  I: Integer;
begin
  if Assigned(Ay) then begin
    Ay.Reset();
    for I := 0 to 15 do begin
      Ay.FRegPointers[I]^ := FRegisters[I] and TSoundAY_3_8912.RegMasks[I];
    end;

    Ay.FActiveRegisterNum := not FActiveRegisterNumber;
    Ay.SetActiveRegNum(FActiveRegisterNumber);

    Ay.StopFading; // When loading from a snapshot we can't tell if the "fading register
                   // had faded already, so let's assume it had, it's the best we can.

    Ay.RecalcOutputChannels;
  end;
end;

initialization
  TSoundAY_3_8912.Init;

end.

