
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock2B }

TTzxBlock2B = class (TTzxBlockStandard)
strict private
  SignalLevel: Byte;
strict protected
  function Load2(const Stream: TStream; L: Integer): Boolean; override;
public
  class function GetBlockId: Integer; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse: Boolean; override;
  procedure Details(out S: String); override;
end;

{$else}

function {TTzxPlayer.}TTzxBlock2B.Load2(const Stream: TStream; L: Integer
  ): Boolean;
begin
  Result := (L = 1) and (Stream.Read(SignalLevel, 1) = 1);
  if SignalLevel <> 0 then
    SignalLevel := 64;
end;

class function {TTzxPlayer.}TTzxBlock2B.GetBlockId: Integer;
begin
  Result := $2B;
end;

class function {TTzxPlayer.}TTzxBlock2B.GetBlockDescription: String;
begin
  Result := 'Set signal level';
end;

function {TTzxPlayer.}TTzxBlock2B.GetNextPulse: Boolean;
begin
  Result := False;
  FTzxPlayer.ActiveBit := SignalLevel;
end;

procedure {TTzxPlayer.}TTzxBlock2B.Details(out S: String);
begin
  if SignalLevel = 0 then
    S := 'low'
  else
    S := 'high';
end;

{$endif}


