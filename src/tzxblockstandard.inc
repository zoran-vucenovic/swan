
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlockStandard = class abstract (TTzxBlock)
strict protected
  FLen: Integer;
  function Load2(const Stream: TStream; L: Integer): Boolean; virtual; abstract;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
end;

{$else}

constructor TTzxBlockStandard.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FLen := 0;
end;

function {TTzxPlayer.}TTzxBlockStandard.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlockStandard.LoadBlock(const Stream: TStream
  ): Boolean;
var
  LL: UInt32;
  L: Int32 absolute LL;
begin
  Result := False;
  if Stream.Size >= Stream.Position + 4 then begin
    if Stream.Read(LL{%H-}, 4) = 4 then begin
      LL := LEtoN(LL);
      FLen := L + 4;
      Result := (L >= 0) and ((L = 0) or ((Stream.Size - Stream.Position >= L) and Load2(Stream, L)));
    end;
  end;
end;



{$endif}
