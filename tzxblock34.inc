
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock34 = class (TTzxBlock)
strict private
  FGeneralEmulationFlags: Word;
  ScreenRefreshDelay: Byte;
  InteruptFrequency: Word;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: DWord; override;
  {returns whole block length, but without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
end;

{$else}

constructor TTzxBlock34.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  ScreenRefreshDelay := 1;
  FGeneralEmulationFlags := Word(%0000000011100111);
end;

class function {TTzxPlayer.}TTzxBlock34.GetBlockId: DWord;
begin
  Result := $34;
end;

function {TTzxPlayer.}TTzxBlock34.GetBlockLength: Integer;
begin
  Result := 8;
end;

function {TTzxPlayer.}TTzxBlock34.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
begin
  Result := False;
  if Stream.Size - Stream.Position >= 8 then begin

    if Stream.Read(W{%H-}, 2) = 2 then begin
      FGeneralEmulationFlags := LEtoN(W);

      if Stream.Read(ScreenRefreshDelay, 1) = 1 then begin

        if Stream.Read(W, 2) = 2 then begin
          W := LEtoN(W);
          InteruptFrequency := W mod 999;
          Stream.Seek(3, TSeekOrigin.soCurrent);
        end;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock34.GetBlockDescription: String;
begin
  Result := 'Emulation info';
end;

{$endif}

