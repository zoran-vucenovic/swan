
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock21 }

TTzxBlock21 = class (TTzxBlock)
strict private
  FLen: Integer;
  GroupName: AnsiString;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor TTzxBlock21.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FLen := 0;
  GroupName := '';
end;

class function {TTzxPlayer.}TTzxBlock21.GetBlockId: DWord;
begin
  Result := $21;
end;

function {TTzxPlayer.}TTzxBlock21.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock21.LoadBlock(const Stream: TStream): Boolean;
var
  B: Byte;
begin
  Result := False;
  if Stream.Size > Stream.Position then begin
    if Stream.Read(B{%H-}, 1) = 1 then begin
      if Stream.Size - Stream.Position >= B then begin
        Result := B = 0;
        FLen := 1 + B;            
        SetLength(GroupName, B);
        if not Result then begin
          if Stream.Read(GroupName[1], B) = B then begin
            TCommonFunctions.ConvertCodePageFromCp1252ToUtf8(GroupName);
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock21.GetBlockDescription: String;
begin
  Result := 'Group start';
end;

procedure {TTzxPlayer.}TTzxBlock21.Details(out S: String);
begin
  S := GroupName;
end;

{$endif}

