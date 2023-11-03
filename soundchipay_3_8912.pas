unit SoundChipAY_3_8912;
// Copyright 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, LCLType, UnitSoundPlayer;

type

  TFuncTicks = function(): Int64 of object;

  { TSoundAY_3_8912 }

  TSoundAY_3_8912 = class(TObject)
  strict private
    FRegA: Word;
    FRegB: Word;
    FRegC: Word;

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

    OutputChanelsLengths: array [0..2] of Integer;
    OutputChCurrentPositions: array [0..2] of Integer;

    OutputChanelsVolumes: array [0..2] of Byte;

    FEnvelopePosition: Integer;
    FEnvelopeValue: Byte;
    FEnvelopePeriod: Integer;
    FEnvelopeDirection: -1..1; // falling, hold, rising
    FEnvelopeHold: Boolean;
    FEnvelopeHoldUp: Boolean;
    FEnvelopeAlter: Boolean;
    FEnvelopeOnCh: array [0..2] of Boolean;

    NoisePosition: Integer;
    NoiseHalfPeriod: Integer;
    NoiseLevel: Integer;
    NoiseGenerator: UInt32;
    NoiseOnCh: array [0..2] of Byte;

    FOnCheckTicks: TFuncTicks;
    StartFadingTicks: Int64;

    procedure InitRegPointers();
    procedure RecalcOutputChanels;

    procedure ResetEnvelope;
    procedure SetOnCheckTicks(AValue: TFuncTicks);
    function EmptyCheckTicks(): Int64;

  private
    FActiveRegisterNum: Byte;
    FRegPointers: array [0..15] of PByte;

  private
    const
      RegMasks: array [0..15] of Byte = (
        $FF, $0F, $FF, $0F, $FF, $0F, $1F, $FF,
        $1F, $1F, $1F, $FF, $FF, $0F, $FF, $FF
      );

  strict private
    class var
      Vols: array [0..15] of Single;

  private
    class procedure Init;

  public
    procedure Fill(F: Single; P: PSingle; Len: Integer);

    constructor Create;
    function GetRegValue(): Byte;
    procedure SetRegValue(AValue: Byte);
    procedure Reset();
    procedure SetActiveRegNum(const ARegNumber: Byte);

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
  FRegPointers[0] := @WordRec(FRegA).Lo;
  FRegPointers[1] := @WordRec(FRegA).Hi;
  FRegPointers[2] := @WordRec(FRegB).Lo;
  FRegPointers[3] := @WordRec(FRegB).Hi;
  FRegPointers[4] := @WordRec(FRegC).Lo;
  FRegPointers[5] := @WordRec(FRegC).Hi;
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

procedure TSoundAY_3_8912.RecalcOutputChanels;

  procedure RecalcOutChanel(Ch: Integer; Reg: Word; Vol: Byte);
  var
    Le2: Integer;
  begin
    NoiseOnCh[Ch] := ((FMixer shr (Ch + 3)) and 1) * $0F;
    FEnvelopeOnCh[Ch] := FRegPointers[Ch or 8]^ and $10 <> 0;

    if FMixer and (1 shl Ch) = 0 then begin

      Le2 := (Integer(Reg) * 224 + 281) div 563;
      if Le2 < 1 then begin
        OutputChanelsLengths[Ch] := 1;
      end else begin
        OutputChanelsLengths[Ch] := Le2;
      end;

    end else begin
      OutputChanelsLengths[Ch] := 1;
      Vol := 15;
    end;

    OutputChanelsVolumes[Ch] := Vol;

  end;

begin
  if FEnvelopeDuration = 0 then
    FEnvelopePeriod := 1
  else begin
    FEnvelopePeriod := FEnvelopeDuration;
    FEnvelopePeriod := (FEnvelopePeriod * 3584 + 281) div 563;
  end;

  NoiseHalfPeriod := FNoiseWidth;
  NoiseHalfPeriod := (NoiseHalfPeriod * 112 + 281) div 563;

  RecalcOutChanel(0, FRegA, FVolumeA);
  RecalcOutChanel(1, FRegB, FVolumeB);
  RecalcOutChanel(2, FRegC, FVolumeC);
end;

class procedure TSoundAY_3_8912.Init;
const
  CDiv: Double = 65535.0 * 127;
  CVols: array [0..15] of UInt16 = (
    $0000, $028F, $03B3, $0564, $07DC, $0BA9, $1083, $1B7C, $2068, $347A, $4ACE, $5F72, $7E16, $A2A4, $CE3A, $FFFF
  );

var
  I: Integer;
  N: Integer;
  X: Single;

begin
  for I := 0 to 15 do begin
    //Vols[I] := CVols[I] / CDiv;
    N := Int32(127) shl ((15 - I) shr 1);
    X := 1.0 / N;
    if I and 1 = 0 then
      X := X / Sqrt(2.0);
    Vols[I] := X;
  end;
end;

procedure TSoundAY_3_8912.SetOnCheckTicks(AValue: TFuncTicks);
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
  FEnvelopeAlter := (FEnvelopeShape and %0010) <> 0;
end;

procedure TSoundAY_3_8912.Fill(F: Single; P: PSingle; Len: Integer);
var
  J, K, Q, L: Integer;
  N: Integer;
  PE: PSingle;
  NW: Single;

begin
  PE := P + Len;
  while P < PE do begin

    NW := 0.0;

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
      L := OutputChanelsLengths[J];
      K := OutputChCurrentPositions[J] mod L;

      if K shl 1 < L then begin

        if FEnvelopeOnCh[J] then begin
          // envelope...
          N := FEnvelopeValue;
        end else
          N := OutputChanelsVolumes[J];

        N := N and (NoiseLevel or NoiseOnCh[J]);

        NW := NW + Vols[N];

      end;

      OutputChCurrentPositions[J] := K + 1;
    end;

    NW := (NW * TSoundPlayer.Volume + F) / 2.0625 - 1.0;

    P^ := NW;

    Inc(FEnvelopePosition);
    if FEnvelopeDirection <> 0 then begin
      Q := (FEnvelopePosition * 16) div FEnvelopePeriod;

      if Q >= 16 then begin
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
          FEnvelopeValue := Q;
        end else
          FEnvelopeValue := 15 - Q;
      end;
    end;

    Inc(P);
  end;
end;

constructor TSoundAY_3_8912.Create;
begin
  inherited Create;

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

    if Result = 0 then begin
      StartFadingTicks := 0;
      FFadingValue := 0;
    end;

  end;

end;

procedure TSoundAY_3_8912.SetRegValue(AValue: Byte);
begin
  if FActiveRegisterNum <= 15 then begin
    AValue := AValue and FActiveRegisterMask;

    if AValue <> FActiveRegisterPointer^ then begin
      FActiveRegisterPointer^ := AValue;
      RecalcOutputChanels;
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
    OutputChanelsLengths[I] := 1;
    OutputChCurrentPositions[I] := 0;
    OutputChanelsVolumes[I] := $0F;
    NoiseOnCh[I] := $0F;
    FEnvelopeOnCh[I] := False;
  end;

  NoisePosition := 0;
  NoiseHalfPeriod := 1;
  NoiseGenerator := $1FFFF;
  NoiseLevel := $0F;

  FRegA := 0;
  FRegB := 0;
  FRegC := 0;

  FNoiseWidth := 0;
  FMixer := $FF;
  FVolumeA := 0;
  FVolumeB := 0;
  FVolumeC := 0;

  FEnvelopeDuration := 0;
  FEnvelopeShape := 0;
  FIOPortA := 0;

  FReg15 := 0;

  FFadingValue := 0;
  StartFadingTicks := 0;

  FActiveRegisterNum := 1;

  ResetEnvelope;

  SetActiveRegNum(0);
  FActiveRegisterPointer^ := 1;
  SetRegValue(0);
end;

procedure TSoundAY_3_8912.SetActiveRegNum(const ARegNumber: Byte);
begin
  //if ARegNumber <> FActiveRegisterNum then begin
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
  //end;
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
    for I := 0 to 15 do begin
      Ay.FRegPointers[I]^ := FRegisters[I] and TSoundAY_3_8912.RegMasks[I];
    end;

    Ay.FActiveRegisterNum := not FActiveRegisterNumber;
    Ay.SetActiveRegNum(FActiveRegisterNumber);
  end;
end;

initialization
  TSoundAY_3_8912.Init;

end.

