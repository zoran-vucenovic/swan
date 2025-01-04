
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock22 = class (TTzxBlock)
public
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const {%H-}Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock22.GetBlockId: DWord;
begin
  Result := $22;
end;

function {TTzxPlayer.}TTzxBlock22.GetBlockLength: Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxBlock22.LoadBlock(const Stream: TStream): Boolean;
begin
  Result := True;
end;

class function {TTzxPlayer.}TTzxBlock22.GetBlockDescription: String;
begin
  Result := 'Group end';
end;

{$endif}

