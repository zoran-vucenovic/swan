
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock18 = class (TTzxBlockUnsupported)
public
  class function GetBlockId: Integer; override;
  class function GetBlockDescription: String; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock18.GetBlockId: Integer;
begin
  Result := $18;
end;

class function {TTzxPlayer.}TTzxBlock18.GetBlockDescription: String;
begin
  Result := 'CSW Recording';
end;

{$endif}


