
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlockUnsupported = class (TTzxBlockStandard)
private
  FDetails: String;
strict protected
  function Load2(const Stream: TStream; L: Integer): Boolean; override;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  class function GetBlockDescription: String; override;
  procedure Details(out S: String); override;
end;

{$else}

class function TTzxBlockUnsupported.GetBlockId: Integer;
begin
  Result := 0;
end;

class function TTzxBlockUnsupported.GetBlockDescription: String;
begin
  Result := 'Block not supported';
end;

procedure TTzxBlockUnsupported.Details(out S: String);
begin
  S := FDetails;
end;

function TTzxBlockUnsupported.Load2(const Stream: TStream; L: Integer
  ): Boolean;
var
  B: Byte;
  N: Integer;
begin
  if GetBlockId = 0 then begin
    Stream.Seek(-5, TSeekOrigin.soCurrent);
    if Stream.Read(B{%H-}, 1) <> 1 then
      Exit(False);

    N := B;
    FDetails := 'Unknown block — 0x' + IntToHex(N, 2);
    L := L + 4;
  end else
    FDetails := 'Block not supported';

  Stream.Seek(L, TSeekOrigin.soCurrent);
  Result := True;
end;

constructor TTzxBlockUnsupported.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FDetails := '';
end;

{$endif}
