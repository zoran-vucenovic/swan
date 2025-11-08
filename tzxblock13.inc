
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock13 }

TTzxBlock13 = class (TTzxBlock)
strict private
  FLen: Integer;
  NumOfPulses: Integer; // pulses needed
  PulsesLengths: array of Integer;

  TicksNeeded: Int64;
  CurrentPulseNumber: Integer;
  FDetails: String;
  FDetailsFilled: Boolean;

protected
  function GetTicksNextEdge: Int64; override;
  function IsReallyPlayableBlock: Boolean; override;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
  procedure Start; override;
  procedure Details(out S: String); override;
end;

{$else}

function TTzxBlock13.GetTicksNextEdge: Int64;
begin
  Result := TicksNeeded;
end;

function TTzxBlock13.IsReallyPlayableBlock: Boolean;
begin
  Result := True;
end;

constructor TTzxBlock13.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FDetailsFilled := False;
  FLen := 0;
  NumOfPulses := 0;
  SetLength(PulsesLengths, 0);
end;

class function TTzxBlock13.GetBlockId: DWord;
begin
  Result := $13;
end;

function TTzxBlock13.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function TTzxBlock13.LoadBlock(const Stream: TStream): Boolean;
var
  I: Integer;
  B: Byte;
  W: Word;
begin
  Result := False;

  FDetailsFilled := False;
  if Stream.Size > Stream.Position then begin
    if Stream.Read(B{%H-}, 1) = 1 then begin
      NumOfPulses := B;
      FLen := NumOfPulses * 2 + 1;
      Result := NumOfPulses = 0;
      if not Result then begin
        if Stream.Size - FLen >= Stream.Position - 1 then begin
          SetLength(PulsesLengths, NumOfPulses);
          I := 0;
          while (I < NumOfPulses) and (Stream.Read(W{%H-}, 2) = 2) do begin
            W := LEtoN(W);
            PulsesLengths[I] := W;
            Inc(I);
          end;

          Result := I = NumOfPulses;
        end;
      end;
    end;
  end;
end;

class function TTzxBlock13.GetBlockDescription: String;
begin
  Result := 'Pulse sequence';
end;

function TTzxBlock13.GetNextPulse: Boolean;
var
  TicksNeeded0: Int64;

begin
  if State = TPlayState.psFinished then
    Exit(False);

  if GetCurrentTotalSpectrumTicks >= TicksNeeded then begin
    if State <> TPlayState.psStart then
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;

    if CurrentPulseNumber < NumOfPulses then begin
      TicksNeeded0 := PulsesLengths[CurrentPulseNumber];
      AdjustTicksIfNeeded(TicksNeeded0);
      TicksNeeded := TicksNeeded + TicksNeeded0;
      State := TPlayState.psPilot;
    end else begin
      State := TPlayState.psFinished;
    end;

    Inc(CurrentPulseNumber);
  end;
  Result := True;
end;

procedure TTzxBlock13.Start;
begin
  inherited Start;

  if NumOfPulses > 0 then begin
    State := TPlayState.psStart;

    TicksNeeded := GetCurrentTotalSpectrumTicks;
    CurrentPulseNumber := 0;
  end;
end;

procedure TTzxBlock13.Details(out S: String);

  procedure FillDetails;
  const
    MaxLines = 5;
    MaxPulsesInFirstLine = 8;
    MaxPulsesInLine = 10;

  var
    I: Integer;
    N: Integer;
    K: Integer;
    M: Integer;
    S2: String;

  begin
    FDetails := 'Number of pulses: ' + NumOfPulses.ToString;

    if NumOfPulses > 0 then begin
      K := MaxPulsesInFirstLine;
      M := MaxPulsesInLine;

      if NumOfPulses > K + (MaxLines - 1) * M then begin
        N := K + (MaxLines - 2) * M;
        S2 := #13 + Format('  ... (and %d more)', [NumOfPulses - N]);
      end else begin
        S2 := '';

        repeat
          I := NumOfPulses + M - K;
          N := I mod M;
          if (N = 0) or (I div M >= M - N) then
            Break;
          Dec(K);
          Dec(M);
        until False;

        N := NumOfPulses;
      end;

      FDetails := FDetails + #13 + 'Pulse lengths: ' + PulsesLengths[0].ToString;
      for I := 1 to N - 1 do begin
        Dec(K);
        if K = 0 then begin
          FDetails := FDetails + ',' + #13 + '   ';
          K := M;
        end else
          FDetails := FDetails + ', ';

        FDetails := FDetails + PulsesLengths[I].ToString;
      end;

      FDetails := FDetails + S2;
    end;
    FDetailsFilled := True;
  end;

begin
  if not FDetailsFilled then
    FillDetails;
  S := FDetails;
end;

{$endif}

