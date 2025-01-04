
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock14 = class (TTzxBlock11abstract)
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: DWord; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
end;

{$else}

constructor TTzxBlock14.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  PilotPulsesNeeded := 0;
  PilotTicksNeeded := 0;
  Sync1TicksNeeded := 0;
  Sync2TicksNeeded := 0;
end;

class function TTzxBlock14.GetBlockId: DWord;
begin
  Result := $14;
end;

function TTzxBlock14.LoadBlock(const Stream: TStream): Boolean;
var
  L: Int32;
  W: Word;
  B: Byte;
begin
  Result := False;
  if Stream.Size - Stream.Position >= 10 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      Bit0TicksNeeded := W;
      if Stream.Read(W, 2) = 2 then begin
        W := LEtoN(W);
        Bit1TicksNeeded := W;
        if (Stream.Read(B{%H-}, 1) = 1) then begin

          if Stream.Read(W, 2) = 2 then begin
            W := LEtoN(W);
            PauseAfterBlock := W;
            if ReadThreeBytes(Stream, L) then begin
              if Stream.Size - Stream.Position >= L then begin
                DataLen := L;
                FLen := 10 + L;
                if L = 0 then
                  Result := PauseAfterBlock > 0
                else begin
                  Getmem(Data, L);
                  try
                    if Stream.Read(Data^, L) = L then begin
                      if B > 8 then
                        UsedBitsInLastByte := 8
                      else if B = 0 then begin
                        Dec(L);
                        UsedBitsInLastByte := 8;
                      end else
                        UsedBitsInLastByte := B;

                      if L > 0 then begin
                        PEnd := Data + (L - 1);
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

class function TTzxBlock14.GetBlockDescription: String;
begin
  Result := 'Pure Data Block';
end;

{$endif}

