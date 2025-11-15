
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock30 = class (TTzxBlock)
strict private
  FLen: Integer;
  FTextDescription: AnsiString;
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

constructor TTzxBlock30.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FLen := 0;
  FTextDescription := '';
end;

class function TTzxBlock30.GetBlockId: DWord;
begin
  Result := $30;
end;

function TTzxBlock30.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function TTzxBlock30.LoadBlock(const Stream: TStream): Boolean;
var
  B: Byte;
begin
  Result := False;
  if Stream.Size > Stream.Position then begin
    if Stream.Read(B{%H-}, 1) = 1 then begin
      if Stream.Size - Stream.Position >= B then begin
        Result := B = 0;
        FLen := 1 + B;
        SetLength(FTextDescription, B);
        if not Result then begin
          if Stream.Read(FTextDescription[1], B) = B then begin
            TCommonFunctions.ConvertCodePageFromCp1252ToUtf8(FTextDescription);
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

class function TTzxBlock30.GetBlockDescription: String;
begin
  Result := 'Text description';
end;

procedure TTzxBlock30.Details(out S: String);
begin
  S := FTextDescription;
end;

{$endif}

