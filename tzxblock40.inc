
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock40 }

TTzxBlock40 = class (TTzxBlock)
strict private
  FLen: Integer;
  FSnapshotType: Byte;
  FSnapStream: TMemoryStream;
  FSnapLen: Integer;
public
  constructor Create(ATzxPlayer: TTzxPlayer); override;
  destructor Destroy; override;
  class function GetBlockId: Integer; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
  procedure Start; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor {TTzxPlayer.}TTzxBlock40.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create(ATzxPlayer);

  FSnapStream := nil;
  FLen := 0;
  FSnapshotType := 0;
end;

destructor {TTzxPlayer.}TTzxBlock40.Destroy;
begin
  FSnapStream.Free;

  inherited Destroy;
end;

class function {TTzxPlayer.}TTzxBlock40.GetBlockId: Integer;
begin
  Result := $40;
end;

function {TTzxPlayer.}TTzxBlock40.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock40.LoadBlock(const Stream: TStream): Boolean;
var
  B: Byte;
  L: Int32;
begin
  Result := False;
  if Stream.Size - Stream.Position >= 4 then begin
    if Stream.Read(B{%H-}, 1) = 1 then begin
      if B <> 0 then
        FSnapshotType := 1
      else
        FSnapshotType := 0;

      if ReadThreeBytes(Stream, L) then begin
        Result := L = 0;
        FLen := L + 4;
        FSnapLen := L;
        if not Result then begin
          if Stream.Size - Stream.Position >= L then begin
            FSnapStream := TMemoryStream.Create;
            FSnapStream.Size := L;
            if Stream.Read(FSnapStream.Memory^, L) = L then begin
              FSnapStream.Position := 0;
              Result := True;
            end;
          end;
        end;
      end;

    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock40.GetBlockDescription: String;
begin
  Result := 'Snapshot block';
end;

function {TTzxPlayer.}TTzxBlock40.GetNextPulse: Boolean;
var
  S: TSnapshotFile;
begin
  Result := False;
  if State <> TPlayState.psFinished then begin
    if Assigned(FTzxPlayer.FSpectrum) then begin
      if FSnapshotType = 0 then
        S := TSnapshotZ80.Create
      else
        S := TSnapshotSNA.Create;

      try
        S.SetSpectrum(FTzxPlayer.FSpectrum);
        FTzxPlayer.FSpectrum.ResetSpectrum;
        Result := S.LoadFromStream(FSnapStream);
      finally
        S.Free;
      end;
    end;
    State := TPlayState.psFinished;
  end;
end;

procedure {TTzxPlayer.}TTzxBlock40.Start;
begin
  inherited Start;
  if (FSnapStream = nil) or (FSnapLen = 0) then
    State := TPlayState.psFinished;
end;

procedure {TTzxPlayer.}TTzxBlock40.Details(out S: String);
begin
  if FSnapshotType = 0 then
    S := 'z80'
  else
    S := 'sna';
  S := '"' + S + '" snapshot' + #13 + 'snapshot length: ' + FSnapLen.ToString;
end;

{$endif}

