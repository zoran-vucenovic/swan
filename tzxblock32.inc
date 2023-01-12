
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock32 = class (TTzxBlock)
strict private
  type
    TTextStructure = record
      TextIdByte: Byte;
      Text: AnsiString;
    end;

    TTextStructures = Array of TTextStructure;

strict private
  FLen: Integer;
  TextStructures: TTextStructures;

  class function DecodeTextIdByte(TextIdByte: Byte): String;
public
  constructor Create(ATzxPlayer: TTzxPlayer); override;
  class function GetBlockId: Integer; override;
  class function GetBlockDescription: String; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  function GetBlockLength: Integer; override;
  procedure Details(out S: String); override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock32.DecodeTextIdByte(TextIdByte: Byte): String;
begin
  case TextIdByte of
    $00: Result := 'Full title';
    $01: Result := 'Software house/publisher';
    $02: Result := 'Author(s)';
    $03: Result := 'Year of publication';
    $04: Result := 'Language';
    $05: Result := 'Game/utility type';
    $06: Result := 'Price';
    $07: Result := 'Protection scheme/loader';
    $08: Result := 'Origin';
    $FF: Result := 'Comment(s)';
  otherwise
    Result := '';
  end;
  if Result <> '' then
    Result := ' - ' + Result;
  Result := '0x' + IntToHex(TextIdByte) + Result;
end;

constructor {TTzxPlayer.}TTzxBlock32.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create(ATzxPlayer);
  FLen := 0;
  SetLength(TextStructures, 0);
end;

class function {TTzxPlayer.}TTzxBlock32.GetBlockId: Integer;
begin
  Result := $32;
end;

class function {TTzxPlayer.}TTzxBlock32.GetBlockDescription: String;
begin
  Result := 'Archive info';
end;

function {TTzxPlayer.}TTzxBlock32.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
  I, L: Integer;
  B: Byte;
  N: Integer;
begin
  if Stream.Size >= Stream.Position + 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      L := W;
      if Stream.Size - Stream.Position >= L then begin
        FLen := L + 2;
        if L >= 1 then begin
          if Stream.Read(B{%H-}, 1) = 1 then begin
            Dec(L);
            N := B;
            if N > 0 then begin
              SetLength(TextStructures, N);
              for I := 0 to N - 1 do begin
                if L < 2 then
                  Exit(False);

                if Stream.Read(B, 1) <> 1 then
                  Exit(False);

                TextStructures[I].TextIdByte := B;
                if Stream.Read(B, 1) <> 1 then
                  Exit(False);

                L := L - 2;
                if L < B then
                  Exit(False);

                SetLength(TextStructures[I].Text, B);
                if B > 0 then begin
                  if Stream.Read(TextStructures[I].Text[1], B) <> B then
                    Exit(False);
                  ConvertCodePageFromISO8859_1_to_Utf8(TextStructures[I].Text);
                  L := L - B;
                end;
              end;
            end;    
            Exit(True);
          end;
        end;
      end;
    end;
  end;
  Result := False;
end;

function {TTzxPlayer.}TTzxBlock32.GetBlockLength: Integer;
begin
  Result := FLen;
end;

procedure {TTzxPlayer.}TTzxBlock32.Details(out S: String);
var
  I: Integer;
  S1: String;
begin
  inherited Details(S);

  for I := 0 to High(TextStructures) do begin
    if I > 0 then
      S := S + #13;
    S := S + DecodeTextIdByte(TextStructures[I].TextIdByte);
    S1 := Trim(TextStructures[I].Text);
    if S1 <> '' then
      S := S + ': ' + S1;
  end;
end;

{$endif}


