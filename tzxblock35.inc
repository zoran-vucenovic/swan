
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock35 }

TTzxBlock35 = class (TTzxBlock)
private
  FLen: Integer;
  FIdentificationString: AnsiString;
  FCustomInfo: PByte;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  destructor Destroy; override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor TTzxBlock35.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FCustomInfo := nil;
  FIdentificationString := '';
  FLen := 0;
end;

destructor {TTzxPlayer.}TTzxBlock35.Destroy;
begin
  if FCustomInfo <> nil then
    Freemem(FCustomInfo);

  inherited Destroy;
end;

class function {TTzxPlayer.}TTzxBlock35.GetBlockId: Integer;
begin
  Result := $35;
end;

function {TTzxPlayer.}TTzxBlock35.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock35.LoadBlock(const Stream: TStream): Boolean;
var
  LL: UInt32;
  L: Int32 absolute LL;
begin
  Result := False;
  if Stream.Size - Stream.Position >= $14 then begin
    SetLength(FIdentificationString, $10);
    if Stream.Read(FIdentificationString[1], $10) = $10 then begin
        // identification string must be in ascii (so no chars above 127)?
        // Just in case:
        TCommonFunctions.ConvertCodePageFromISO8859_1_to_Utf8(FIdentificationString);
        if Stream.Read(LL{%H-}, 4) = 4 then begin
          LL := LEtoN(LL);
          if Stream.Size - Stream.Position >= L then begin
            Result := L = 0;
            FLen := $14 + L;
            if not Result then begin
              GetMem(FCustomInfo, L);
              if Stream.Read(FCustomInfo^, L) = L then begin
                Result := True;
              end;
            end;
          end;
        end;

    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock35.GetBlockDescription: String;
begin
  Result := 'Custom info block';
end;

procedure {TTzxPlayer.}TTzxBlock35.Details(out S: String);
begin
  S := FIdentificationString;
end;

{$endif}

