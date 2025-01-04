
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

{ TTzxBlock15 }

TTzxBlock15 = class (TTzxBlock)
strict private
  FLen: Integer;
  PulseLen: Integer; // (pulse length - ticks needed for one pulse)
  UsedBitsInLastByte: Integer;

  UnusedBitsInLastByte: Integer;
  PauseAfterBlock: Integer;

  TicksNeeded: Int64;
  TicksNeeded0: Int64;
  Data: PByte;
  P: PByte;
  PEnd: PByte;
  BitPosition: Integer;
protected
  function IsReallyPlayableBlock: Boolean; override;
public
  constructor Create(ATapePlayer: TTapePlayer); override;
  destructor Destroy; override;
  class function GetBlockId: DWord; override;
  {returns whole block length, without id byte}
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse(): Boolean; override;
  procedure Start; override;
end;

{$else}

function TTzxBlock15.IsReallyPlayableBlock: Boolean;
begin
  Result := True;
end;

constructor TTzxBlock15.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  P := nil;
  FLen := 0;
  PulseLen := 0;
  UsedBitsInLastByte := 8;
  PauseAfterBlock := 1000;
end;

destructor TTzxBlock15.Destroy;
begin
  if Data <> nil then
    Freemem(Data);
  inherited Destroy;
end;

class function TTzxBlock15.GetBlockId: DWord;
begin
  Result := $15;
end;

function TTzxBlock15.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function TTzxBlock15.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
  B: Byte;
  L: Int32;
begin
  Result := False;
  if Stream.Size - Stream.Position >= 8 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      PulseLen := W;

      if Stream.Read(W, 2) = 2 then begin
        W := LEtoN(W);
        PauseAfterBlock := W;

        if Stream.Read(B{%H-}, 1) = 1 then begin
          UsedBitsInLastByte := B;
          UnusedBitsInLastByte := 8 - UsedBitsInLastByte mod 8;

          if ReadThreeBytes(Stream, L) then begin
            Result := L = 0;
            if not Result then begin
              if Stream.Size - Stream.Position >= L then begin
                //Stream.Seek(Int64(L), TSeekOrigin.soCurrent);
                Getmem(Data, L);
                try
                  if Stream.Read(Data^, L) = L then begin
                    PEnd := Data + (L - 1);
                    Result := True;
                  end;
                finally
                  if not Result then
                    FreeMemAndNil(Data);
                end;
              end;
            end;
            FLen := 8 + L;
          end;

        end;

      end;
    end;
  end;
end;

class function TTzxBlock15.GetBlockDescription: String;
begin
  Result := 'Direct Recording';
end;

function TTzxBlock15.GetNextPulse: Boolean;
var
  ProcTicks: Int64;

  procedure ToPause;
  begin
    if PauseAfterBlock > 0 then
      FTapePlayer.StartPauseBlock(PauseAfterBlock);
    State := TPlayState.psFinished;
  end;

  procedure GetNextDataBit;
  begin
    if (P < PEnd) or ((P = PEnd) and (BitPosition >= UnusedBitsInLastByte)) then begin
      if ((P^ shr BitPosition) and 1) = 0 then
        FTapePlayer.ActiveBit := 0
      else
        FTapePlayer.ActiveBit := %01000000;

      TicksNeeded := ProcTicks + TicksNeeded0;

      if BitPosition > 0 then
        Dec(BitPosition)
      else begin
        BitPosition := 7;
        Inc(P);
      end;
    end else
      ToPause;
  end;

begin
  { Here each bit represents a pulse! }

  if State = TPlayState.psFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;

  if ProcTicks >= TicksNeeded then begin
    // set active bit here
    case State of
      TPlayState.psData:
        GetNextDataBit;
      TPlayState.psStart:
        if P = nil then
          ToPause
        else
          State := TPlayState.psData;
    otherwise
    end;
  end;

  Result := True;
end;

procedure TTzxBlock15.Start;
begin
  inherited Start;

  State := TPlayState.psStart;
  P := Data;
  TicksNeeded := TicksNeeded.MinValue;
  BitPosition := 7;
  TicksNeeded0 := PulseLen;
  AdjustTicksIfNeeded(TicksNeeded0);
end;

{$endif}

