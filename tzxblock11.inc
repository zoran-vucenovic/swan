
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock11 = class (TTzxBlock11abstract)
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  procedure Details(out S: String); override;
end;

{$else}

constructor TTzxBlock11.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  PilotTicksNeeded := 0;
end;

class function {TTzxPlayer.}TTzxBlock11.GetBlockId: Integer;
begin
  Result := $11;
end;

function {TTzxPlayer.}TTzxBlock11.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
  B: Byte;
  N: Int32;
begin
  Result := False;

  if Stream.Size - Stream.Position >= $12 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      PilotTicksNeeded := W;
      if Stream.Read(W, 2) = 2 then begin
        W := LEtoN(W);
        Sync1TicksNeeded := W;
        if Stream.Read(W, 2) = 2 then begin
          W := LEtoN(W);
          Sync2TicksNeeded := W;
          if Stream.Read(W, 2) = 2 then begin
            W := LEtoN(W);
            Bit0TicksNeeded := W;
            if Stream.Read(W, 2) = 2 then begin
              W := LEtoN(W);
              Bit1TicksNeeded := W;
              if Stream.Read(W, 2) = 2 then begin
                W := LEtoN(W);
                PilotPulsesNeeded := W;
                if Stream.Read(B{%H-}, 1) = 1 then begin

                  if Stream.Read(W, 2) = 2 then begin
                    W := LEtoN(W);
                    PauseAfterBlock := W;
                    if ReadThreeBytes(Stream, N) then begin
                      if Stream.Size - Stream.Position >= N then begin
                        DataLen := N;
                        Result := N = 0;
                        FLen := N + $12;
                        if not Result then begin

                          GetMem(Data, N);
                          try
                            if Stream.Read(Data^, N) = N then begin
                              if B >= 8 then
                                UsedBitsInLastByte := 8
                              else if B = 0 then begin
                                Dec(N);
                                UsedBitsInLastByte := 8;
                              end else
                                UsedBitsInLastByte := B;

                              if N > 0 then begin
                                PEnd := Data + (N - 1);
                                Result := True;
                              end;
                            end;

                          finally
                            if not Result then
                              FreeMemAndNil(Data);
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxBlock11.GetBlockDescription: String;
begin
  Result := 'Turbo Speed Data Block';
end;

procedure {TTzxPlayer.}TTzxBlock11.Details(out S: String);
begin
  inherited Details(S);
  S := S
    + #13 + 'Pilot pulse t-states: ' + IntToStr(PilotTicksNeeded)
    + #13 + Format('Sync pulses t-states: (%d, %d)', [Sync1TicksNeeded, Sync2TicksNeeded])
    + #13 + Format('T-states in zero/one bit pulses: (%d, %d)', [Bit0TicksNeeded, Bit1TicksNeeded])
    + #13 + 'Pilot tone pulses: ' + IntToStr(PilotPulsesNeeded)
    ;
end;

{$endif}

