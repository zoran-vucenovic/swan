
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock12 }

TTzxBlock12 = class (TTzxBlock)
private
  FLen: Integer;
  PulseLen: Integer; // ticks needed
  NumOfPulses: Integer; // pulses needed

  TicksNeeded: Int64;
  PulsesNeeded: Integer;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
  procedure Start; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor TTzxBlock12.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FLen := 0;
end;

class function {TTzxPlayer.}TTzxBlock12.GetBlockId: Integer;
begin
  Result := $12;
end;

function {TTzxPlayer.}TTzxBlock12.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock12.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;

begin
  Result := False;
  if Stream.Size >= Stream.Position + 4 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      PulseLen := W;
      if Stream.Read(W, 2) = 2 then begin
        Result := True;
        FLen := 4;
        W := LEtoN(W);
        NumOfPulses := W;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock12.GetBlockDescription: String;
begin
  Result := 'Pure Tone';
end;

function {TTzxPlayer.}TTzxBlock12.GetNextPulse: Boolean;
var
  ProcTicks: Int64;

begin
  if State = TPlayState.psFinished then
    Exit(False);
  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    if State <> TPlayState.psStart then
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;

    Dec(PulsesNeeded);
    if PulsesNeeded = 0 then begin
      case State of
        TPlayState.psStart:
          begin
            State := TPlayState.psData;
            PulsesNeeded := NumOfPulses;
          end;
      otherwise
        State := TPlayState.psFinished;
      end;
    end;
    TicksNeeded := ProcTicks + PulseLen;
  end;
  Result := True;
end;

procedure {TTzxPlayer.}TTzxBlock12.Start;
begin
  inherited Start;
  State := TPlayState.psStart;

  TicksNeeded := TicksNeeded.MinValue;
  PulsesNeeded := 1;
end;

procedure {TTzxPlayer.}TTzxBlock12.Details(out S: String);
begin
  S := 'pulse length: ' + PulseLen.ToString + #13
    + 'number of pulses: ' + NumOfPulses.ToString;
end;

{$endif}

