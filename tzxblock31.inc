
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock31 }

TTzxBlock31 = class (TTzxBlock)
strict private
  FLen: Integer;
  FTextDescription: AnsiString;
  FTime: Integer;
public
  constructor Create(ATzxPlayer: TTzxPlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor {TTzxPlayer.}TTzxBlock31.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create(ATzxPlayer);

  FTime := 0;
  FLen := 0;
  FTextDescription := '';
end;

class function {TTzxPlayer.}TTzxBlock31.GetBlockId: Integer;
begin
  Result := $31;
end;

function {TTzxPlayer.}TTzxBlock31.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock31.LoadBlock(const Stream: TStream): Boolean;
var
  B: Byte;
begin
  Result := False;
  if Stream.Size > Stream.Position then begin
    if Stream.Read(B{%H-}, 1) = 1 then begin
      FTime := B;
      if Stream.Size > Stream.Position then begin
        if Stream.Read(B, 1) = 1 then begin
          if Stream.Size - Stream.Position >= B then begin
            Result := B = 0;
            FLen := 2 + B;
            SetLength(FTextDescription, B);
            if not Result then begin
              if Stream.Read(FTextDescription[1], B) = B then begin
                ConvertCodePageFromISO8859_1_to_Utf8(FTextDescription);
                Result := True;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock31.GetBlockDescription: String;
begin
  Result := 'Message block';
end;

procedure {TTzxPlayer.}TTzxBlock31.Details(out S: String);
begin
  S := FTextDescription;
end;

{$endif}

