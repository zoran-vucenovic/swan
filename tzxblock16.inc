
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock16 = class (TTzxBlockUnsupported)
public
  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
end;

{$else}

class function TTzxBlock16.GetBlockId: DWord;
begin
  Result := $16;
end;

class function TTzxBlock16.GetBlockDescription: String;
begin
  Result := 'C64 ROM Type Data Block';
end;

{$endif}

