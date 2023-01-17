
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock24 }

TTzxBlock24 = class (TTzxBlock)
strict private
  NumberOfRepetitions: Integer;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNumberOfRepetitions: Integer; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor TTzxBlock24.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  NumberOfRepetitions := 0;
end;

class function {TTzxPlayer.}TTzxBlock24.GetBlockId: Integer;
begin
  Result := $24;
end;

function {TTzxPlayer.}TTzxBlock24.GetBlockLength: Integer;
begin
  Result := 2;
end;

function {TTzxPlayer.}TTzxBlock24.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
begin
  Result := False;
  if Stream.Size >= Stream.Position + 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      NumberOfRepetitions := W;
      Result := True;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock24.GetBlockDescription: String;
begin
  Result := 'Loop start';
end;

function {TTzxPlayer.}TTzxBlock24.GetNumberOfRepetitions: Integer;
begin
  Result := NumberOfRepetitions;
end;

procedure {TTzxPlayer.}TTzxBlock24.Details(out S: String);
begin
  S := NumberOfRepetitions.ToString + ' repetitions';
end;

{$endif}

