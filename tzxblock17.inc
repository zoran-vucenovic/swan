
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock17 = class (TTzxBlockUnsupported)
public
  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock17.GetBlockId: DWord;
begin
  Result := $17;
end;

class function {TTzxPlayer.}TTzxBlock17.GetBlockDescription: String;
begin
  Result := 'C64 Turbo Tape Data Block';
end;

{$endif}


