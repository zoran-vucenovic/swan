
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock2A = class (TTzxBlockStandard)
strict protected
  function Load2(const {%H-}Stream: TStream; {%H-}L: Integer): Boolean; override;
public
  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
  function GetStopPlaying: Boolean; override;
end;

{$else}

class function {TTzxPlayer.}TTzxBlock2A.GetBlockId: DWord;
begin
  Result := $2A;
end;

class function {TTzxPlayer.}TTzxBlock2A.GetBlockDescription: String;
begin
  Result := 'Stop the tape if in 48K mode';
end;

function TTzxBlock2A.GetStopPlaying: Boolean;
begin
  Result := not FTapePlayer.GetSpectrum.Is128KModel;
end;

function {TTzxPlayer.}TTzxBlock2A.Load2(const Stream: TStream; L: Integer
  ): Boolean;
begin
  Result := True;
end;

{$endif}


