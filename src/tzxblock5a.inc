
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock5A = class (TTzxBlock)
public
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock5A.GetBlockId: DWord;
begin
  Result := $5A;
end;

function {TTzxPlayer.}TTzxBlock5A.GetBlockLength: Integer;
begin
  Result := 9;
end;

function {TTzxPlayer.}TTzxBlock5A.LoadBlock(const Stream: TStream): Boolean;
var
  S: AnsiString;
begin
  Result := False;
  if Stream.Size - Stream.Position >= 9 then begin
    SetLength(S{%H-}, 9);
    if Stream.Read(S[1], 9) = 9 then begin
      Result := Copy(S, 1, 7) = 'XTape!' + #$1A;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock5A.GetBlockDescription: String;
begin
  Result := 'Glue block';
end;

{$endif}

