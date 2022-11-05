
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock20 = class (TTzxBlock)
strict private
  FPauseLength: Integer;
  TicksNeeded: Int64;

public
  constructor Create(ATzxPlayer: TTzxPlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
  procedure Start; override;
  procedure Details(out S: String); override;
  procedure SetPauseLen(const APauseLength: Integer);
  function GetStopPlaying: Boolean; override;
end;

{$else}

constructor {TTzxPlayer.}TTzxBlock20.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create(ATzxPlayer);

  FPauseLength := 0;
end;

class function {TTzxPlayer.}TTzxBlock20.GetBlockId: Integer;
begin
  Result := $20;
end;

function {TTzxPlayer.}TTzxBlock20.GetBlockLength: Integer;
begin
  Result := 2;
end;

function {TTzxPlayer.}TTzxBlock20.LoadBlock(const Stream: TStream): Boolean;
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

class function {TTzxPlayer.}TTzxBlock20.GetBlockDescription: String;
begin
  Result := 'Pause (silence) or ''Stop the Tape'' command';
end;

function {TTzxPlayer.}TTzxBlock20.GetNextPulse: Boolean;
var
  ProcTicks: Int64;

begin
  if State = TPlayState.psFinished then
    Exit(False);

  ProcTicks := FTzxPlayer.FSpectrum.SumTicks + FTzxPlayer.FSpectrum.GetProcessor.TStatesInCurrentFrame;
  if ProcTicks >= TicksNeeded then begin

    case State of
      TPlayState.psStart:
        begin
          TicksNeeded := ProcTicks + TicksBeforePause;
          State := TPlayState.psData;
        end;
      TPlayState.psData:
        begin
          TicksNeeded := ProcTicks + FPauseLength * TicksPerMilliSecond;
          FTzxPlayer.ActiveBit := 0;
          State := TPlayState.psPause;
        end;

    otherwise
      State := TPlayState.psFinished;
    end;
  end;

  Result := True;
end;

procedure {TTzxPlayer.}TTzxBlock20.Start;
begin                
  inherited Start;
  TicksNeeded := TicksNeeded.MinValue;
end;

procedure {TTzxPlayer.}TTzxBlock20.Details(out S: String);
begin
  if GetStopPlaying then
    S := 'stop playing'
  else
    S := 'Pause ' + FPauseLength.ToString + ' ms';
end;

procedure {TTzxPlayer.}TTzxBlock20.SetPauseLen(const APauseLength: Integer);
begin
  FPauseLength := APauseLength;
end;

function TTzxBlock20.GetStopPlaying: Boolean;
begin
  Result := FPauseLength = 0;
end;

{$endif}

