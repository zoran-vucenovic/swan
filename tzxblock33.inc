
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock33 = class (TTzxBlock)
strict private
  type
    THWInfo =
      packed record
        HWType, HWId, HWInformation: Byte;
      end;

    PHWInfo = ^THWInfo;

strict private
  FLen: Integer;
  FHWInfo: PHWInfo;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  destructor Destroy; override;
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
end;

{$else}

constructor TTzxBlock33.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FHWInfo := nil;
  FLen := 0;
end;

destructor {TTzxPlayer.}TTzxBlock33.Destroy;
begin
  if Assigned(FHWInfo) then
    FreeMem(FHWInfo);

  inherited Destroy;
end;

class function {TTzxPlayer.}TTzxBlock33.GetBlockId: DWord;
begin
  Result := $33;
end;

function {TTzxPlayer.}TTzxBlock33.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function {TTzxPlayer.}TTzxBlock33.LoadBlock(const Stream: TStream): Boolean;
var
  B: Byte;
  N: Integer;
begin
  Result := False;
  if Stream.Size > Stream.Position then begin
    if Stream.Read(B{%H-}, 1) = 1 then begin
      N := B;

      if Stream.Size - Stream.Position >= N then begin
        Result := N = 0;
        N := N * SizeOf(THWInfo);
        FLen := 1 + N;

        if not Result then begin
          if Stream.Size - Stream.Position >= N then begin
            Getmem(FHWInfo, N);
            if Stream.Read(FHWInfo^, N) = N then
              Result := True;
          end;
        end;
      end;

    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock33.GetBlockDescription: String;
begin
  Result := 'Hardware type';
end;

{$endif}

