
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock19 = class (TTzxBlockStandard)
strict private
  type
    TSymdef = record
      SymbolFlag: Byte;
      PulseLengths: array of Integer;
      Len: Integer;
    end;

    TSymdefs = array of TSymdef;

    TPrle = packed record
      SymbolToBeRepresented: Byte;
      NumberOfRepetitions: Word;
    end;

    TPrles = array of TPrle;

strict private
  PauseAfterBlock: Integer;
  Totp: Integer;
  Npp: Integer;
  Asp: Integer;
  Totd: Integer;
  Npd: Integer;
  Asd: Integer;
  SymDefsPilot: TSymdefs;
  SymDefsData: TSymdefs;
  Prles: TPrles;
  Prle: TPrle;
  Data: PByte;
  P, PEnd: PByte;

  TicksNeeded: Int64;

  PulsesNeeded: Integer;
  BitPosition: Integer;

  SymDef: TSymdef;
  SymPos: Integer;
  SymLen: Integer;
  //CurrentSym: Integer;
  NB: Integer;
  CurrentPrle: Integer;
  Mask: Integer;
  UnusedBitsInLastByte: Integer;
  //PausePass: Integer;
  TicksNeeded0: Int64;
  Start1: Boolean;
  FDetails: RawByteString;

  procedure FillDetails;

strict protected
  function Load2(const Stream: TStream; L: Integer): Boolean; override;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  destructor Destroy; override;

  class function GetBlockId: Integer; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse: Boolean; override;
  procedure Start; override;
  procedure Details(out S: String); override;
end;

{$else}

procedure {TTzxPlayer.}TTzxBlock19.FillDetails;
begin
  FDetails :=
    'TOTP: ' + IntToStr(Totp) + #13
    + 'NPP: ' + IntToStr(Npp) + #13
    + 'ASP: ' + IntToStr(Asp) + #13
    + 'TOTD: ' + IntToStr(Totd) + #13
    + 'NPD: ' + IntToStr(Npd) + #13
    + 'ASD: ' + IntToStr(Asd) + #13
    + 'Pause after block: ' + IntToStr(PauseAfterBlock) + ' ms'
    ;

end;

function {TTzxPlayer.}TTzxBlock19.Load2(const Stream: TStream; L: Integer
  ): Boolean;

  function LoadSymDefs(var SymDefs: TSymDefs; Asx, Npx: Integer): Boolean;
  var
    I, J: Integer;
    WA: Array[0..255] of Word;
    W: Word;
    B: Byte;
  begin
    SetLength(SymDefs, Asx);
    if Asx > 0 then begin
      Result := False;
      if Npx = 0 then
        Exit;
      L := L - Asx * (2 * Npx + 1);
      if L >= 0 then begin
        for I := 0 to Asx - 1 do begin
          if Stream.Read(B{%H-}, 1) <> 1 then
            Exit;
          SymDefs[I].SymbolFlag := B and %11;
          if Stream.Read({%H-}WA[0], 2 * Npx) <> 2 * Npx then
            Exit;
          SetLength(SymDefs[I].PulseLengths, Npx);
          SymDefs[I].Len := Npx;
          J := 0;
          while J < Npx do begin
            W := LEtoN(WA[J]);
            if W = 0 then begin
              if J = 0 then
                Exit;
              SymDefs[I].Len := J;
              SetLength(SymDefs[I].PulseLengths, J);
              Break;
            end;

            SymDefs[I].PulseLengths[J] := W;
            Inc(J);
          end;
        end;
      end;
    end;

    Result := True;
  end;

type
   TBlock19Header = packed record
     PauseAfterBlock: Word;
     Totp: UInt32;
     Npp: Byte;
     Asp: Byte;
     Totd: UInt32;
     Npd: Byte;
     Asd: Byte;
   end;

const
  SizeOfBlockHeader = SizeOf(TBlock19Header);

var
  W: Word;
  UL: UInt32;
  Block19Header: TBlock19Header;
  I, L2: Integer;

begin
  Result := False;

  if L >= SizeOfBlockHeader then begin
    L := L - SizeOfBlockHeader;
    if Stream.Read(Block19Header{%H-}, SizeOfBlockHeader) = SizeOfBlockHeader then begin
      W := LEtoN(Block19Header.PauseAfterBlock);
      PauseAfterBlock := W;
      UL := LEtoN(Block19Header.Totp);
      Totp := UL;
      if Totp > 0 then begin
        Npp := Block19Header.Npp;
        Asp := Block19Header.Asp;
        if Asp = 0 then
          Asp := 256;
      end else begin
        Totp := 0;
        Npp := 0;
        Asp := 0;
      end;

      UL := LEtoN(Block19Header.Totd);
      Totd := UL;
      if Totd > 0 then begin
        Npd := Block19Header.Npd;
        if Npd = 0 then
          Exit;
        Asd := Block19Header.Asd;
        if Asd = 0 then
          Asd := 256;
      end else begin
        Totd := 0;
        Npd := 0;
        Asd := 0;
      end;

      if LoadSymDefs(SymDefsPilot, Asp, Npp) then begin
        SetLength(Prles, Totp);
        if Totp > 0 then begin
          L := L - Totp * 3;
          if L < 0 then
            Exit;

          for I := 0 to Totp - 1 do begin
            if Stream.Read(Prles[I], 3) <> 3 then
              Exit;
            Prles[I].NumberOfRepetitions := LEtoN(Prles[I].NumberOfRepetitions);
          end;
        end;

        UnusedBitsInLastByte := 0;
        NB := 0;
        if Totd > 0 then begin
          if LoadSymDefs(SymDefsData, Asd, Npd) and (L > 0) then begin
            L2 := Asd;
            while L2 <> 0 do begin
              Inc(NB);
              L2 := L2 shr 1;
            end;
            if (1 shl (NB - 1)) = Asd then
              Dec(NB);

            if (NB * Totd + 7) div 8 = L then begin
              Getmem(Data, L);
              PEnd := Data + (L - 1);
              Mask := (1 shl NB) - 1;
              UnusedBitsInLastByte := L * 8 - NB * Totd;
              Result := Stream.Read(Data^, L) = L;
            end;

          end;
        end else
          Result := L = 0;
      end;

      FillDetails;
    end
  end;
end;

constructor TTzxBlock19.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FDetails := '';
  PauseAfterBlock := 0;
  Totp := 0;
  Npp := 0;
  Asp := 0;
  Totd := 0;
  Npd := 0;
  Asd := 0;

  SetLength(SymDefsPilot, 0);
  SetLength(Prles, 0);
  SetLength(SymDefsData, 0);

  Data := nil;
  P := nil;
end;

destructor {TTzxPlayer.}TTzxBlock19.Destroy;
begin
  if Data <> nil then
    Freemem(Data);

  inherited Destroy;
end;

class function {TTzxPlayer.}TTzxBlock19.GetBlockId: Integer;
begin
  Result := $19;
end;

class function {TTzxPlayer.}TTzxBlock19.GetBlockDescription: String;
begin
  Result := 'Generalized Data Block';
end;

function {TTzxPlayer.}TTzxBlock19.GetNextPulse: Boolean;

  procedure ToPause;
  begin
    FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;
    if PauseAfterBlock > 0 then
      FTapePlayer.StartPauseBlock(PauseAfterBlock);

    State := TPlayState.psFinished;
    TicksNeeded0 := 0;
  end;

  procedure NewSymDef; inline;
  begin
    case SymDef.SymbolFlag of
      0, 1:
        if (SymDef.SymbolFlag = 0) xor Start1 then
          FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;
      2:
        FTapePlayer.ActiveBit := 0;
      3:
        FTapePlayer.ActiveBit := %01000000;
    end;
    Start1 := False;
    SymLen := SymDef.Len;
  end;

  procedure GetNextDataPulse;
  var
    NN, MM, K: Integer;
  begin
    Inc(SymPos);
    if SymPos >= SymLen then begin
      SymPos := 0;
      NN := BitPosition + 1 - NB;
      if (P < PEnd) or ((P = PEnd) and (NN >= UnusedBitsInLastByte)) then begin
        K := P^;
        if NN = 0 then begin
          MM := K and Mask;
          Inc(P);
          BitPosition := 7;
        end else if NN > 0 then begin
          MM := (K shr NN) and Mask;
          BitPosition := BitPosition - NB;
        end else begin
          MM := K shl (-NN);
          Inc(P);
          K := P^;
          MM := (MM or (K shr (NN + 8))) and Mask;
          BitPosition := 7 + NN;
        end;

        SymDef := SymDefsData[MM];
        NewSymDef;
      end else begin
        ToPause;
        Exit;
      end;

    end else
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;

    TicksNeeded0 := SymDef.PulseLengths[SymPos];
    PulsesNeeded := 1;
  end;

var
  ProcTicks: Int64;

begin
  if State = TPlayState.psFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    Dec(PulsesNeeded);
    if PulsesNeeded = 0 then begin
      repeat // never loops, but allows break
        case State of
          TPlayState.psData:
            begin
              GetNextDataPulse;
            end;

          TPlayState.psPilot:
            begin
              Inc(SymPos);
              if SymPos >= SymLen then begin
                SymPos := 0;
                Inc(CurrentPrle);
                if CurrentPrle >= Totp then begin
                  SymLen := -1;
                  State := TPlayState.psData;
                  GetNextDataPulse;
                  Break;
                end;

                Prle := Prles[CurrentPrle];

                SymDef := SymDefsPilot[Prle.SymbolToBeRepresented];
                NewSymDef;
              end else
                FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;

              PulsesNeeded := Prle.NumberOfRepetitions;
              TicksNeeded0 := SymDef.PulseLengths[SymPos];
            end;

        otherwise
          ToPause;
        end;

      until True;

      AdjustTicksIfNeeded(TicksNeeded0);
    end else
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;

    TicksNeeded := ProcTicks + TicksNeeded0;
  end;

  Result := True; // state <> psFinished;
end;

procedure {TTzxPlayer.}TTzxBlock19.Start;
begin
  inherited Start;
  State := TPlayState.psStart;

  Start1 := True;
  TicksNeeded0 := 0;
  TicksNeeded := TicksNeeded.MinValue;
  PulsesNeeded := 1;

  P := Data;
  BitPosition := 7;

  CurrentPrle := -1;

  //PausePass := 0;
  //CurrentSym := -1;
  SymPos := 0;
  SymLen := -1;

  if Totp > 0 then
    State := TPlayState.psPilot
  else if Totd > 0 then
    State := TPlayState.psData
  else
    State := psFinished;
end;

procedure {TTzxPlayer.}TTzxBlock19.Details(out S: String);
begin
  S := FDetails;
end;


{$endif}

