
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock18 = class (TTzxBlockStandard)
strict private
  type
    TCswBlock18 = class(TCswBlock)
    end;

strict private
  Csw: TCswBlock18;

  FPauseAfterBlock: Integer;

strict protected
  function Load2(const Stream: TStream; L: Integer): Boolean; override;

protected
  function GetTicksNextEdge: Int64; override;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  destructor Destroy; override;

  procedure Start; override;
  function GetNextPulse: Boolean; override;

  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
  function GetBlockLength: Integer; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor TTzxBlock18.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  Csw := TCswBlock18.Create(ATapePlayer);
end;

destructor TTzxBlock18.Destroy;
begin
  Csw.Free;

  inherited Destroy;
end;

procedure TTzxBlock18.Start;
begin
  inherited Start;

  Csw.Start;
  if Csw.State = TCswBlock.TCswPlayState.cpsStart then begin
    State := TPlayState.psStart;
  end;
end;

function TTzxBlock18.GetNextPulse: Boolean;
begin
  if State = psFinished then
    Exit(False);

  if Csw.State <> TCswBlock.TCswPlayState.cpsFinished then
    Result := Csw.GetNextPulse
  else begin
    if FPauseAfterBlock > 0 then begin
      FTapePlayer.StartPauseBlock(FPauseAfterBlock);
    end;
    State := psFinished;
    Result := True;
  end;
end;

class function TTzxBlock18.GetBlockId: DWord;
begin
  Result := $18;
end;

class function TTzxBlock18.GetBlockDescription: String;
begin
  Result := 'CSW Recording';
end;

function TTzxBlock18.GetBlockLength: Integer;
begin
  Result := FLen;
end;

procedure TTzxBlock18.Details(out S: String);
begin
  Csw.Details(S);
  S := S + #13 + 'Pause after block: ' + FPauseAfterBlock.ToString + ' ms'
end;

function TTzxBlock18.Load2(const Stream: TStream; L: Integer): Boolean;
var
  W: Word;
  N: Int32;
  B: Byte;
  Dw: DWord;
begin
  Result := False;
  if L >= 10 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      FPauseAfterBlock := W;
      if ReadThreeBytes(Stream, N) and (N > 0) and (Stream.Read(B{%H-}, 1) = 1) then begin
        Csw.SampleRate := N;
        if B = 2 then
          Csw.CompressionType := TCswBlock.TCswCompressionType.cctZRle
        else
          Csw.CompressionType := TCswBlock.TCswCompressionType.cctRle;

        if Stream.Read(Dw{%H-}, 4) = 4 then begin
          Dw := LEtoN(Dw);
          Csw.TotalNumOfPulses := Dw;
          Csw.CswDataLength := L - 10;
          Result := Csw.LoadBlock(Stream);
        end;
      end;
    end;
  end;
end;

function TTzxBlock18.GetTicksNextEdge: Int64;
begin
  Result := Csw.GetTicksNextEdge;
end;

{$endif}

