unit SoundChipAY_3_8912;
// Copyright 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, PortAudioHeader, UnitSoundPlayer;

type

  { TSoundAY_3_8912 }

  TSoundAY_3_8912 = class(TObject)
  strict private
    FRegA: Word;
    FRegB: Word;
    FRegC: Word;

    FNoisePitch: Byte;
    FMixer: Byte;
    FVolumeA: Byte;
    FVolumeB: Byte;
    FVolumeC: Byte;

    FEnvelopeDuration: Word;
    FEnvelopeShape: Byte;
    FIOPortA: Byte;

    FReg15: Byte;
    FDefValue: Byte;

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

    procedure InitRegPointers();
    procedure RecalcOutputChanels;

    procedure ResetEnvelope;

  private
    FActiveRegisterNum: Byte;
    FRegPointers: array [0..15] of PByte;

  private
    class var
      RegMasks: array [0..15] of Byte;

    class procedure Init;

  public
    procedure Fill(B: Byte; P: PByte; Len: Integer);

    constructor Create;
    function GetRegValue(): Byte;
    procedure SetRegValue(AValue: Byte);
    procedure Reset();
    procedure SetActiveRegNum(const ARegNumber: Byte);
  end;

  TAyState = record
  private
    FActiveRegisterNumber: Byte;
    FRegisters: array[0..15] of Byte;

    function GetRegisters(I: Integer): Byte;
    class operator Initialize(var X: TAyState);
    procedure SetRegisters(I: Integer; AValue: Byte);
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
  FRegPointers[6] := @FNoisePitch;
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
    B: Byte;
  begin

    B := FMixer shr Ch;
    if B and 1 = 0 then begin
      if B and 8 = 0 then begin
        Reg := Reg or FNoisePitch;
      end;

      Le2 := (Integer(Reg) * 224 + 281) div 563;
      if Le2 <= 1 then begin
        OutputChanelsLengths[Ch] := 1;
        OutputChanelsVolumes[Ch] := 0;
      end else begin
        OutputChanelsLengths[Ch] := Le2;

        if Vol and $10 <> 0 then begin
        // envelopes...
          Vol := $0F
        end;

        OutputChanelsVolumes[Ch] := Vol;
      end;
    end else begin
      OutputChanelsLengths[Ch] := 1;
      OutputChanelsVolumes[Ch] := 0;
    end;

    OutputChCurrentPositions[Ch] := 0;
  end;

begin                                          
  FEnvelopePeriod := FEnvelopeDuration;
  FEnvelopePeriod := (FEnvelopePeriod * 3584 + 281) div 563;

  RecalcOutChanel(0, FRegA, FVolumeA);
  RecalcOutChanel(1, FRegB, FVolumeB);
  RecalcOutChanel(2, FRegC, FVolumeC);
end;

class procedure TSoundAY_3_8912.Init;
begin
  RegMasks[0] := $FF;
  RegMasks[1] := $0F;
  RegMasks[2] := $FF;
  RegMasks[3] := $0F;
  RegMasks[4] := $FF;
  RegMasks[5] := $0F;
  RegMasks[6] := $1F;
  RegMasks[7] := $FF;
  RegMasks[8] := $1F;
  RegMasks[9] := $1F;
  RegMasks[10] := $1F;
  RegMasks[11] := $FF;
  RegMasks[12] := $FF;
  RegMasks[13] := $0F;
  RegMasks[14] := $FF;
  RegMasks[15] := $FF;
end;

procedure TSoundAY_3_8912.ResetEnvelope;
begin
  // envelopes...
  {
          cont attack alter hold
   ----------------------------------------------------------------------
    0:      0     0     0    0    \__________    single decay then off
    1:      0     0     0    1    \__________    single decay then off
    2:      0     0     1    0    \__________    single decay then off
    3:      0     0     1    1    \__________    single decay then off
    4:      0     1     0    0    /|_________    single attack then off
    5:      0     1     0    1    /|_________    single attack then off
    6:      0     1     1    0    /|_________    single attack then off
    7:      0     1     1    1    /|_________    single attack then off
    8:      1     0     0    0    \|\|\|\|\|\    repeated decay
    9:      1     0     0    1    \__________    single decay then off
   10:      1     0     1    0    \/\/\/\/\/\    repeated decay-attack
   11:      1     0     1    1    \|`````````    single decay then hold
   12:      1     1     0    0    /|/|/|/|/|/    repeated attack
   13:      1     1     0    1    /``````````    single attack then hold
   14:      1     1     1    0    /\/\/\/\/\/    repeated attack-decay
   15:      1     1     1    1    /|_________    single attack then off
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

procedure TSoundAY_3_8912.Fill(B: Byte; P: PByte; Len: Integer);
var
  J, K, Q: Integer;
  N, NW: Integer;
  PE: PByte;
  Ndiv: Integer;

begin
  PE := P + Len;
  //BI := Integer(B) * TSoundPlayer.Volume shr 6;

  while P < PE do begin
    //
    NW := 0;

    for J := 0 to 2 do begin
      K := OutputChCurrentPositions[J];
      if K < OutputChanelsLengths[J] div 2 then begin
        N := OutputChanelsVolumes[J];
        if N <> 0 then begin
          if FRegPointers[8 or J]^ and $10 <> 0 then begin
            // envelope...
            N := FEnvelopeValue;
          end;
          NW := NW + N;
        end;

      end;
      OutputChCurrentPositions[J] := (K + 1) mod OutputChanelsLengths[J];
    end;

    NW := (NW * TSoundPlayer.Volume) shr 6 + B;
    P^ := NW;

    if FEnvelopePeriod > 0 then begin
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
    end;

    Inc(P);
  end;
end;

constructor TSoundAY_3_8912.Create;
begin
  inherited Create;

  InitRegPointers();
  Reset();
end;

function TSoundAY_3_8912.GetRegValue: Byte;
begin
  Result := FActiveRegisterPointer^;
end;

procedure TSoundAY_3_8912.SetRegValue(AValue: Byte);
begin
  AValue := AValue and FActiveRegisterMask;

  if AValue <> FActiveRegisterPointer^ then begin
    FActiveRegisterPointer^ := AValue;
    RecalcOutputChanels;
  end;

  // Setting envelope shape is the only event that resets envelope.
  // Resets envelope even when setting to the same value.
  if FActiveRegisterNum = 13 then
    ResetEnvelope;
end;

procedure TSoundAY_3_8912.Reset();
var
  I: Integer;
begin
  for I := 0 to 2 do begin
    OutputChanelsLengths[I] := 1;
    OutputChCurrentPositions[I] := 0;
  end;

  FRegA := 0;
  FRegB := 0;
  FRegC := 0;

  FNoisePitch := 0;
  FMixer := 0;
  FVolumeA := 0;
  FVolumeB := 0;
  FVolumeC := 0;

  FEnvelopeDuration := 0;
  FEnvelopeShape := 0;
  FIOPortA := 0;

  FReg15 := 0;

  FDefValue := 0;

  FActiveRegisterNum := 1;

  ResetEnvelope;
  //FEnvelopeState := 0;

  SetActiveRegNum(0);
end;

procedure TSoundAY_3_8912.SetActiveRegNum(const ARegNumber: Byte);
begin
  if ARegNumber <> FActiveRegisterNum then begin
    FActiveRegisterNum := ARegNumber;
    if ARegNumber <= 15 then begin
      FActiveRegisterPointer := FRegPointers[ARegNumber];

      FActiveRegisterMask := RegMasks[ARegNumber];
    end else begin
      // what if value greater than 15 is set?
      { #todo : implement decay of this "register" (https://worldofspectrum.org/forums/discussion/23327/) }
      FActiveRegisterPointer := @FDefValue;
      FActiveRegisterMask := 0;
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

