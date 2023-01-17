
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock25 = class (TTzxBlock)
protected
  function CheckLoopEnd: Boolean; override;
public
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const {%H-}Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
end;

{$else}

function TTzxBlock25.CheckLoopEnd: Boolean;
begin
  Result := True;
end;

class function {TTzxPlayer.}TTzxBlock25.GetBlockId: Integer;
begin
  Result := $25;
end;

function {TTzxPlayer.}TTzxBlock25.GetBlockLength: Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxBlock25.LoadBlock(const Stream: TStream): Boolean;
begin
  Result := True;
end;

class function {TTzxPlayer.}TTzxBlock25.GetBlockDescription: String;
begin
  Result := 'Loop end';
end;

{$endif}

