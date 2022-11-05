
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock16 = class (TTzxBlockUnsupported)
public
  class function GetBlockId: Integer; override;
  class function GetBlockDescription: String; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock16.GetBlockId: Integer;
begin
  Result := $16;
end;

class function {TTzxPlayer.}TTzxBlock16.GetBlockDescription: String;
begin
  Result := 'C64 ROM Type Data Block';
end;

{$endif}

