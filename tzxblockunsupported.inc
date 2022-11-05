
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlockUnsupported = class (TTzxBlockStandard)
strict protected
  function Load2(const Stream: TStream; L: Integer): Boolean; override;
public
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlockUnsupported.GetBlockId: Integer;
begin
  Result := 0;
end;

class function {TTzxPlayer.}TTzxBlockUnsupported.GetBlockDescription: String;
begin
  Result := 'Unsupported block';
end;

function {TTzxPlayer.}TTzxBlockUnsupported.GetNextPulse: Boolean;
begin
  Result := False;
end;

function {TTzxPlayer.}TTzxBlockUnsupported.Load2(const Stream: TStream; L: Integer
  ): Boolean;
begin
  Stream.Seek(L, TSeekOrigin.soCurrent);
  Result := True;
end;

{$endif}
