
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock27 = class (TTzxBlock)
protected
  function CheckReturnFromCallSequence: Boolean; override;
  function IsReallyPlayableBlock: Boolean; override;
public
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const {%H-}Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
end;

{$else}

function TTzxBlock27.CheckReturnFromCallSequence: Boolean;
begin
  Result := True;
end;

function TTzxBlock27.IsReallyPlayableBlock: Boolean;
begin
  Result := True;
end;

class function TTzxBlock27.GetBlockId: DWord;
begin
  Result := $27;
end;

function TTzxBlock27.GetBlockLength: Integer;
begin
  Result := 0;
end;

function TTzxBlock27.LoadBlock(const Stream: TStream): Boolean;
begin
  Result := True;
end;

class function TTzxBlock27.GetBlockDescription: String;
begin
  Result := 'Return from sequence';
end;

{$endif}

