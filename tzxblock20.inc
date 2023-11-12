
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlockPause = class (TTzxBlock)
private
  FPauseLength: Integer;
  TicksNeeded: Int64;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
  procedure Start; override;
  procedure SetPauseLen(const APauseLength: Integer);
end;

TTzxBlock20 = class (TTzxBlockPause)
public
  procedure Details(out S: String); override;
  function GetStopPlaying: Boolean; override;
end;

{$else}

constructor TTzxBlockPause.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FPauseLength := 0;
end;

class function {TTzxPlayer.}TTzxBlockPause.GetBlockId: Integer;
begin
  Result := $20;
end;

function {TTzxPlayer.}TTzxBlockPause.GetBlockLength: Integer;
begin
  Result := 2;
end;

function {TTzxPlayer.}TTzxBlockPause.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
begin
  Result := False;
  if Stream.Size >= Stream.Position + 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      FPauseLength := W;

      Result := True;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlockPause.GetBlockDescription: String;
begin
  Result := 'Pause (silence) or ''Stop the Tape'' command';
end;

function {TTzxPlayer.}TTzxBlockPause.GetNextPulse: Boolean;
var
  ProcTicks: Int64;
  TicksToWait: Int64;

begin
  if State = TPlayState.psFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin

    case State of
      TPlayState.psStart:
        begin
          TicksToWait := TicksBeforePause;
          State := TPlayState.psData;
        end;
      TPlayState.psData:
        begin
          TicksToWait := FPauseLength * TicksPerMilliSecond;
          AdjustTicksIfNeeded(TicksToWait);
          FTapePlayer.ActiveBit := 0;
          FTapePlayer.InPause := True;
          State := TPlayState.psPause;
        end;

    otherwise
      TicksToWait := 0;
      State := TPlayState.psFinished;
    end;

    TicksNeeded := ProcTicks + TicksToWait;
  end;

  Result := True;
end;

procedure {TTzxPlayer.}TTzxBlockPause.Start;
begin                
  inherited Start;
  State := TPlayState.psStart;

  TicksNeeded := TicksNeeded.MinValue;
end;

procedure {TTzxPlayer.}TTzxBlockPause.SetPauseLen(const APauseLength: Integer);
begin
  FPauseLength := APauseLength;
end;

{ TTzxBlock20 }

procedure TTzxBlock20.Details(out S: String);
begin
  if GetStopPlaying then
    S := 'stop playing'
  else
    S := 'Pause ' + FPauseLength.ToString + ' ms';
end;

function TTzxBlock20.GetStopPlaying: Boolean;
begin
  Result := FPauseLength = 0;
end;


{$endif}

