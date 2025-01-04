
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock11abstract = class abstract (TTzxBlock)
strict protected
  const
    DefaultPilotHeaderPulsesNeeded = 8063;
    DefaultPilotDataPulsesNeeded = 3223;

    DefaultPilotTicksNeeded = 2168;
    DefaultSync1TicksNeeded = 667;
    DefaultSync2TicksNeeded = 735;
    DefaultBitPulsesNeeded = 2;
    DefaultBit0TicksNeeded = 855;
    DefaultBit1TicksNeeded = 1710;

strict protected
  PilotTicksNeeded: Integer;
  Sync1TicksNeeded: Integer;
  Sync2TicksNeeded: Integer;
  BitPulsesNeeded: Integer;
  Bit0TicksNeeded: Integer;
  Bit1TicksNeeded: Integer;
  UsedBitsInLastByte: Integer;
  PauseAfterBlock: Integer;
  FLen: Integer;
  DataLen: Integer;
  Data: PByte;
  PilotPulsesNeeded: Integer;
  PEnd: PByte;

strict private
  UnusedBitsInLastByte: Integer;
  P: PByte;
  BitPosition: Integer;
  TicksNeeded0: Int64;
  PulsesNeeded: Integer;
  TicksNeeded: Int64;

strict protected
  function Details2: String; virtual;

protected
  function GetTicksNextEdge: Int64; override;
  function IsReallyPlayableBlock: Boolean; override;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  destructor Destroy; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function GetNextPulse(): Boolean; override;
  procedure Start; override;
  procedure Details(out S: String); override;
end;

{$else}

function TTzxBlock11abstract.Details2: String;
begin
  Result := #13 + 'Used bits in last byte: ' + IntToStr(UsedBitsInLastByte);
end;

function TTzxBlock11abstract.GetTicksNextEdge: Int64;
begin
  Result := TicksNeeded;
end;

function TTzxBlock11abstract.IsReallyPlayableBlock: Boolean;
begin
  Result := True;
end;

constructor TTzxBlock11abstract.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  PilotTicksNeeded := DefaultPilotTicksNeeded;
  Sync1TicksNeeded := DefaultSync1TicksNeeded;
  Sync2TicksNeeded := DefaultSync2TicksNeeded;
  BitPulsesNeeded := DefaultBitPulsesNeeded;
  Bit0TicksNeeded := DefaultBit0TicksNeeded;
  Bit1TicksNeeded := DefaultBit1TicksNeeded;
  UsedBitsInLastByte := 8;
  Data := nil;
  PauseAfterBlock := 1000;
  FLen := 0;

  PilotPulsesNeeded := 0;
  PulsesNeeded := 0;
  TicksNeeded := 0;

  P := nil;
  PEnd := nil;
  BitPosition := 0;
end;

destructor TTzxBlock11abstract.Destroy;
begin
  if Data <> nil then
    Freemem(Data);
  inherited Destroy;
end;

function TTzxBlock11abstract.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function TTzxBlock11abstract.GetNextPulse: Boolean;

  procedure ToPause;
  begin
    if PauseAfterBlock > 0 then
      FTapePlayer.StartPauseBlock(PauseAfterBlock);

    State := TPlayState.psFinished;
    TicksNeeded0 := 0;
  end;

  procedure GetNextDataBit;
  begin
    if (P < PEnd) or ((P = PEnd) and (BitPosition >= UnusedBitsInLastByte)) then begin
      if ((P^ shr BitPosition) and 1) = 0 then
        TicksNeeded0 := Bit0TicksNeeded
      else
        TicksNeeded0 := Bit1TicksNeeded;

      PulsesNeeded := 2;

      if BitPosition > 0 then
        Dec(BitPosition)
      else begin
        BitPosition := 7;
        Inc(P);
      end;
    end else
      ToPause;

  end;

var
  ProcTicks: Int64;

begin
  if State = TPlayState.psFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    if State <> TPlayState.psStart then
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;
    Dec(PulsesNeeded);
    if PulsesNeeded = 0 then begin
      case State of
        TPlayState.psData:
          GetNextDataBit;
      otherwise
        repeat
          Inc(State);

          case State of
            TPlayState.psPilot:
              begin
                TicksNeeded0 := PilotTicksNeeded;
                if TicksNeeded0 = 0 then begin
                  if (P = nil) and (PauseAfterBlock = 0) then begin
                    State := TPlayState.psFinished;
                    Exit(False);
                  end;
                  State := TPlayState.psSync2;
                end;
                PulsesNeeded := PilotPulsesNeeded;
              end;
            TPlayState.psSync1:
              begin
                TicksNeeded0 := Sync1TicksNeeded;
                PulsesNeeded := 1;
              end;
            TPlayState.psSync2:
              begin
                TicksNeeded0 := Sync2TicksNeeded;
                PulsesNeeded := 1;
              end;
            TPlayState.psData:
              if P <> nil then
                GetNextDataBit
              else
                ToPause;
          otherwise
          end;

        until (TicksNeeded0 <> 0) or (State > TPlayState.psData);
      end;

      AdjustTicksIfNeeded(TicksNeeded0);
    end;
    TicksNeeded := ProcTicks + TicksNeeded0;
  end;

  Result := True;
end;

procedure TTzxBlock11abstract.Start;
begin
  inherited Start;
  State := TPlayState.psStart;

  TicksNeeded0 := 0;
  TicksNeeded := TicksNeeded.MinValue;
  PulsesNeeded := 1;

  UnusedBitsInLastByte := 8 - UsedBitsInLastByte;

  P := Data;
  BitPosition := 7;
end;

procedure TTzxBlock11abstract.Details(out S: String);
begin
  S := 'Data length: ' + IntToStr(DataLen)
    + Details2
    + #13 + 'Pause after block: ' + IntToStr(PauseAfterBlock) + ' ms'
    ;
end;

{$endif}

