
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock2B = class (TTzxBlockStandard)
strict private
  SignalLevel: Byte;
strict protected
  function Load2(const Stream: TStream; L: Integer): Boolean; override;
public
  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse: Boolean; override;
  procedure Details(out S: String); override;
end;

{$else}

function TTzxBlock2B.Load2(const Stream: TStream; L: Integer): Boolean;
begin
  Result := (L = 1) and (Stream.Read(SignalLevel, 1) = 1);
  SignalLevel := SignalLevel and 1;
end;

class function TTzxBlock2B.GetBlockId: DWord;
begin
  Result := $2B;
end;

class function TTzxBlock2B.GetBlockDescription: String;
begin
  Result := 'Set signal level';
end;

function TTzxBlock2B.GetNextPulse: Boolean;
begin
  Result := False;

  if SignalLevel = 0 then
    FTapePlayer.ResetPulseLevel
  else
    FTapePlayer.SetPulseLevel;
end;

procedure TTzxBlock2B.Details(out S: String);
begin
  if SignalLevel = 0 then
    S := 'low'
  else
    S := 'high';
end;

{$endif}


