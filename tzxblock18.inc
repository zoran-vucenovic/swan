
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock18 = class (TTzxBlockUnsupported)
public
  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock18.GetBlockId: DWord;
begin
  Result := $18;
end;

class function {TTzxPlayer.}TTzxBlock18.GetBlockDescription: String;
begin
  Result := 'CSW Recording';
end;

{$endif}


