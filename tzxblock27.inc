
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock27 = class (TTzxBlock)
protected
  function CheckReturnFromCallSequence: Boolean; override;
public
  constructor Create(ATzxPlayer: TTzxPlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const {%H-}Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
end;

{$else}

function TTzxBlock27.CheckReturnFromCallSequence: Boolean;
begin
  Result := True;
end;

constructor {TTzxPlayer.}TTzxBlock27.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create(ATzxPlayer);
end;

class function {TTzxPlayer.}TTzxBlock27.GetBlockId: Integer;
begin
  Result := $27;
end;

function {TTzxPlayer.}TTzxBlock27.GetBlockLength: Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxBlock27.LoadBlock(const Stream: TStream): Boolean;
begin
  Result := True;
end;

class function {TTzxPlayer.}TTzxBlock27.GetBlockDescription: String;
begin
  Result := 'Return from sequence';
end;

function {TTzxPlayer.}TTzxBlock27.GetNextPulse: Boolean;
begin
  Result := False;
  State := TPlayState.psFinished;
end;

{$endif}

