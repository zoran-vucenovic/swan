
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock28 = class (TTzxBlock)
strict private
  FLen: Integer;
public
  constructor Create(ATzxPlayer: TTzxPlayer); override;

  class function GetBlockId: Integer; override;
  class function GetBlockDescription: String; override;
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  function GetNextPulse: Boolean; override;
end;

{$else}

constructor {TTzxPlayer.}TTzxBlock28.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create(ATzxPlayer);
  FLen := 0;
end;

class function {TTzxPlayer.}TTzxBlock28.GetBlockId: Integer;
begin
  Result := $28;
end;

class function {TTzxPlayer.}TTzxBlock28.GetBlockDescription: String;
begin
  Result := 'Select block';
end;

function {TTzxPlayer.}TTzxBlock28.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock28.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
  L: Integer;
begin
  Result := False;
  if Stream.Size >= Stream.Position + 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      L := W;
      if Stream.Size - Stream.Position >= L then begin
        Stream.Seek(L, TSeekOrigin.soCurrent);
        FLen := L + 2;
        Result := True;
      end;
    end;
  end;
end;

function {TTzxPlayer.}TTzxBlock28.GetNextPulse: Boolean;
begin
  Result := False;
end;

{$endif}


