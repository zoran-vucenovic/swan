
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock13 }

TTzxBlock13 = class (TTzxBlock)
strict private
  FLen: Integer;
  NumOfPulses: Integer; // pulses needed
  PulsesLengths: array of Integer;

  TicksNeeded: Int64;
  CurrentPulseNumber: Integer;

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

constructor TTzxBlock13.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FLen := 0;
  NumOfPulses := 0;
  SetLength(PulsesLengths, 0);
end;

class function {TTzxPlayer.}TTzxBlock13.GetBlockId: Integer;
begin
  Result := $13;
end;

function {TTzxPlayer.}TTzxBlock13.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock13.LoadBlock(const Stream: TStream): Boolean;
var
  I: Integer;
  B: Byte;
  W: Word;
begin
  Result := False;

  if Stream.Size > Stream.Position then begin
    if Stream.Read(B{%H-}, 1) = 1 then begin
      NumOfPulses := B;
      FLen := NumOfPulses * 2 + 1;
      Result := NumOfPulses = 0;
      if not Result then begin
        if Stream.Size - FLen >= Stream.Position - 1 then begin
          SetLength(PulsesLengths, NumOfPulses);
          I := 0;
          while (I < NumOfPulses) and (Stream.Read(W{%H-}, 2) = 2) do begin
            W := LEtoN(W);
            PulsesLengths[I] := W;
            Inc(I);
          end;

          Result := I = NumOfPulses;
        end;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock13.GetBlockDescription: String;
begin
  Result := 'Pulse sequence';
end;

function {TTzxPlayer.}TTzxBlock13.GetNextPulse: Boolean;
var
  ProcTicks: Int64;
begin
  if State = TPlayState.psFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    if State <> TPlayState.psStart then
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;

    if CurrentPulseNumber < NumOfPulses then begin
      TicksNeeded := ProcTicks + PulsesLengths[CurrentPulseNumber];
      State := TPlayState.psPilot;
    end else begin
      State := TPlayState.psFinished;
    end;

    Inc(CurrentPulseNumber);
  end;
  Result := True;
end;

procedure {TTzxPlayer.}TTzxBlock13.Start;
begin
  inherited Start;
  State := TPlayState.psStart;

  TicksNeeded := TicksNeeded.MinValue;

  CurrentPulseNumber := 0;
  if NumOfPulses = 0 then
    State := TPlayState.psFinished;
end;

procedure {TTzxPlayer.}TTzxBlock13.Details(out S: String);
var
  I: Integer;
  MaybeComma: String;
begin
  S := 'number of pulses: ' + NumOfPulses.ToString
    ;
  MaybeComma := #13 + 'pulse lengths: ';
  for I := 0 to NumOfPulses - 1 do begin
    S := S + MaybeComma + PulsesLengths[I].ToString;
    MaybeComma := ', ';
  end;
end;

{$endif}

