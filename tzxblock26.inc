
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock26 }

TTzxBlock26 = class (TTzxBlock)
strict private
  CallBlockNumbers: array of Int16;
  NumOfCalls: Integer;
public
  constructor Create(ATzxPlayer: TTzxPlayer); override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNumOfCalls: Integer; override;
  function GetCallBlockNumber(const I: Integer): Integer; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor {TTzxPlayer.}TTzxBlock26.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create(ATzxPlayer);

  NumOfCalls := 0;
  SetLength(CallBlockNumbers, 0);
end;

class function {TTzxPlayer.}TTzxBlock26.GetBlockId: Integer;
begin
  Result := $26;
end;

function {TTzxPlayer.}TTzxBlock26.GetBlockLength: Integer;
begin
  Result := 2 + 2 * Length(CallBlockNumbers);
end;

function {TTzxPlayer.}TTzxBlock26.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
  I: Integer;
begin
  Result := False;
  if Stream.Size >= Stream.Position + 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      NumOfCalls := W;
      if Stream.Size >= Stream.Position + 2 * Int64(NumOfCalls) then begin
        SetLength(CallBlockNumbers, NumOfCalls);
        for I := 0 to NumOfCalls - 1 do begin
          if Stream.Read(W, 2) <> 2 then
            Exit;
          Word(CallBlockNumbers[I]) := LEtoN(W);
        end;
        Result := True;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock26.GetBlockDescription: String;
begin
  Result := 'Call sequence';
end;

function {TTzxPlayer.}TTzxBlock26.GetNumOfCalls: Integer;
begin
  Result := NumOfCalls;
end;

function {TTzxPlayer.}TTzxBlock26.GetCallBlockNumber(const I: Integer): Integer;
begin
  if (I >= 0) and (I < NumOfCalls) then
    Result := CallBlockNumbers[I]
  else
    Result := inherited GetCallBlockNumber(I);
end;

procedure {TTzxPlayer.}TTzxBlock26.Details(out S: String);
var
  I: Integer;
  MaybeComma: String;
begin
  S := Format('Number of calls: %d%sblocks (relative): ', [NumOfCalls, #13]);

  MaybeComma := '';
  for I := 0 to NumOfCalls - 1 do begin
    S := S + MaybeComma + IntToStr(CallBlockNumbers[I]);
    MaybeComma := ', ';
  end;
end;

{$endif}

