
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlockPause = class (TTzxBlock)
private
  FPauseLength: Integer;
  TicksNeeded: Int64;

protected
  function GetTicksNextEdge: Int64; override;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: DWord; override;
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

function TTzxBlockPause.GetTicksNextEdge: Int64;
var
  N: Int64;
  B: Byte;
begin
  case State of
    TPlayState.psData:
      Result := TicksNeeded;
    TPlayState.psPilot:
      begin
        if FTapePlayer.ActiveBit <> 0 then
          Exit(TicksNeeded);
        Result := FPauseLength * TTapePlayer.TicksPerMilliSecond;
        AdjustTicksIfNeeded(Result);
        Result := Result + TicksNeeded;
      end;
  otherwise
    Result := inherited GetTicksNextEdge;
    Exit;
  end;

  B := not FTapePlayer.GetSpectrum.GetProcessor.RegB;
  N := Result - GetCurrentTotalSpectrumTicks;
  N := (N - 385) div 59;
  if N < B then
    Result := inherited GetTicksNextEdge;
end;

constructor TTzxBlockPause.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FPauseLength := 0;
end;

class function TTzxBlockPause.GetBlockId: DWord;
begin
  Result := $20;
end;

function TTzxBlockPause.GetBlockLength: Integer;
begin
  Result := 2;
end;

function TTzxBlockPause.LoadBlock(const Stream: TStream): Boolean;
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

class function TTzxBlockPause.GetBlockDescription: String;
begin
  Result := 'Pause (silence) or ''Stop the Tape'' command';
end;

function TTzxBlockPause.GetNextPulse: Boolean;
var
  TicksToWait: Int64;

begin
  if State = TPlayState.psFinished then
    Exit(False);

  Result := True;

  if GetCurrentTotalSpectrumTicks >= TicksNeeded then begin

    case State of
      TPlayState.psStart:
        begin
          TicksToWait := TTapePlayer.TicksBeforePause;
          State := TPlayState.psPilot;
        end;
      TPlayState.psPilot:
        begin
          TicksToWait := FPauseLength * TTapePlayer.TicksPerMilliSecond;
          FTapePlayer.ActiveBit := 0;
          FTapePlayer.InPause := True;
          State := TPlayState.psData;
        end;

    otherwise
      State := TPlayState.psFinished;
      Exit;
    end;

    AdjustTicksIfNeeded(TicksToWait);
    TicksNeeded := TicksNeeded + TicksToWait;
  end;
end;

procedure TTzxBlockPause.Start;
begin
  inherited Start;
  State := TPlayState.psStart;

  TicksNeeded := GetCurrentTotalSpectrumTicks;
end;

procedure TTzxBlockPause.SetPauseLen(const APauseLength: Integer);
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

