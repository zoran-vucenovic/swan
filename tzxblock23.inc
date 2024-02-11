
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock23 }

TTzxBlock23 = class (TTzxBlock)
strict private
  RelativeJumpValue: Integer;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetRelativeJumpValue: Integer; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor TTzxBlock23.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  RelativeJumpValue := 0;
end;

class function {TTzxPlayer.}TTzxBlock23.GetBlockId: DWord;
begin
  Result := $23;
end;

function {TTzxPlayer.}TTzxBlock23.GetBlockLength: Integer;
begin
  Result := 2;
end;

function {TTzxPlayer.}TTzxBlock23.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
  N: Int16 absolute W;
begin
  Result := False;
  if Stream.Size >= Stream.Position + 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      RelativeJumpValue := N;
      Result := True;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock23.GetBlockDescription: String;
begin
  Result := 'Jump to block';
end;

function {TTzxPlayer.}TTzxBlock23.GetRelativeJumpValue: Integer;
begin
  Result := RelativeJumpValue;
end;

procedure {TTzxPlayer.}TTzxBlock23.Details(out S: String);
var
  N: Integer;
begin
  if RelativeJumpValue < 0 then begin
    S := 'back';
    N := -RelativeJumpValue;
  end else begin
    S := 'forward';
    N := RelativeJumpValue;
  end;

  S := Format('jump %s %d blocks', [S, N]);
end;

{$endif}

