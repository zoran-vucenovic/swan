
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock24 }

TTzxBlock24 = class (TTzxBlock)
strict private
  NumberOfRepetitions: Integer;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: DWord; override;
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

class function TTzxBlock24.GetBlockId: DWord;
begin
  Result := $24;
end;

function TTzxBlock24.GetBlockLength: Integer;
begin
  Result := 2;
end;

function TTzxBlock24.LoadBlock(const Stream: TStream): Boolean;
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

class function TTzxBlock24.GetBlockDescription: String;
begin
  Result := 'Loop start';
end;

function TTzxBlock24.GetNumberOfRepetitions: Integer;
begin
  Result := NumberOfRepetitions;
  if Result = 0 then // if loop count is zero, run through code once, as if it were 1
    Result := 1;
end;

procedure TTzxBlock24.Details(out S: String);
begin
  S := NumberOfRepetitions.ToString + ' repetitions';
  if NumberOfRepetitions = 0 then begin
    S := S + ' (passes once, as if it were 1)';
  end;
end;

{$endif}

