
// Copyright 2022-2025 Zoran Vučenović
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
  NB: Integer;
  CurrentPrle: Integer;
  Mask: Integer;
  UnusedBitsInLastByte: Integer;
  TicksNeeded0: Int64;
  Start1: Boolean;
  FDetails: RawByteString;

  procedure FillDetails;

strict protected
  function Load2(const Stream: TStream; L: Integer): Boolean; override;

protected
  function IsReallyPlayableBlock: Boolean; override;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  destructor Destroy; override;

  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
  function GetNextPulse: Boolean; override;
  procedure Start; override;
  procedure Details(out S: String); override;
end;

{$else}

procedure TTzxBlock19.FillDetails;
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

function TTzxBlock19.Load2(const Stream: TStream; L: Integer
  ): Boolean;

  // The length of SymDefsData is Asd. All values in data stream must point to
  // a valid symdef in SymDefsData array (all values must be smaller than Asd).
  // CheckData function checks that.
  function CheckData(): Boolean;
  var
    N, M, K: Integer;
    T: Integer;
  begin
    Result := False;

    P := Data;
    BitPosition := 7;
    T := 0;

    repeat
      N := BitPosition + 1 - NB;
      if (P >= PEnd) and ((P > PEnd) or (N < UnusedBitsInLastByte)) then
        Break;

      Inc(T);
      K := P^;
      if N = 0 then begin
        M := K and Mask;
        Inc(P);
        BitPosition := 7;
      end else if N > 0 then begin
        M := (K shr N) and Mask;
        BitPosition := BitPosition - NB;
      end else begin
        M := K shl (-N);
        Inc(P);
        K := P^;
        M := (M or (K shr (N + 8))) and Mask;
        BitPosition := 7 + N;
      end;

      if M >= Asd then
        Exit; // the value points to outside of SymDefs array!

    until False;

    Result := ((N + NB) mod 8 = UnusedBitsInLastByte) and (T = Totd);
  end;

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
            // The length of SymDefsPilot is Asp. Each prle in Prles array must
            // point to allocated symbol (SymbolToBeRepresented must be less than Asp).
            if Prles[I].SymbolToBeRepresented >= Asp then
              Exit;
            Prles[I].NumberOfRepetitions := LEtoN(Prles[I].NumberOfRepetitions);
          end;
        end;

        UnusedBitsInLastByte := 0;
        NB := 0;
        if Totd > 0 then begin
          if LoadSymDefs(SymDefsData, Asd, Npd) and (L > 0) then begin
            if Asd = 1 then
              NB := 1
            else begin
              L2 := Asd - 1;
              while L2 > 0 do begin
                Inc(NB);
                L2 := L2 shr 1;
              end;
            end;

            if (NB * Totd + 7) div 8 = L then begin
              Getmem(Data, L);
              PEnd := Data + (L - 1);
              Mask := (1 shl NB) - 1;
              UnusedBitsInLastByte := L * 8 - NB * Totd;
              Result := (Stream.Read(Data^, L) = L)
                and CheckData(); // check that every value in data stream points to an allocated symbol!
            end;

          end;
        end else
          Result := L = 0;
      end;

      FillDetails;
    end
  end;
end;

function TTzxBlock19.IsReallyPlayableBlock: Boolean;
begin
  Result := True;
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

destructor TTzxBlock19.Destroy;
begin
  if Data <> nil then
    Freemem(Data);

  inherited Destroy;
end;

class function TTzxBlock19.GetBlockId: DWord;
begin
  Result := $19;
end;

class function TTzxBlock19.GetBlockDescription: String;
begin
  Result := 'Generalized Data Block';
end;

function TTzxBlock19.GetNextPulse: Boolean;

  procedure ToPause;
  begin
    FTapePlayer.InvertPulseLevel;
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
          FTapePlayer.InvertPulseLevel;
      2:
        FTapePlayer.ResetPulseLevel;
      3:
        FTapePlayer.SetPulseLevel;
    otherwise
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
      FTapePlayer.InvertPulseLevel;

    TicksNeeded0 := SymDef.PulseLengths[SymPos];
    PulsesNeeded := 1;
  end;

begin
  if State = TPlayState.psFinished then
    Exit(False);

  if GetCurrentTotalSpectrumTicks >= TicksNeeded then begin
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
                  if Totd > 0 then begin
                    SymLen := -1;
                    State := TPlayState.psData;
                    GetNextDataPulse;
                  end else
                    ToPause;
                  Break;
                end;

                Prle := Prles[CurrentPrle];

                SymDef := SymDefsPilot[Prle.SymbolToBeRepresented];
                NewSymDef;
              end else
                FTapePlayer.InvertPulseLevel;

              PulsesNeeded := Prle.NumberOfRepetitions;
              TicksNeeded0 := SymDef.PulseLengths[SymPos];
            end;

        otherwise
          ToPause;
        end;

      until True;

      AdjustTicksIfNeeded(TicksNeeded0);
    end else
      FTapePlayer.InvertPulseLevel;

    TicksNeeded := TicksNeeded + TicksNeeded0;
  end;

  Result := True;
end;

procedure TTzxBlock19.Start;
begin
  inherited Start;

  Start1 := True;
  TicksNeeded0 := 0;
  TicksNeeded := GetCurrentTotalSpectrumTicks;
  PulsesNeeded := 1;

  P := Data;
  BitPosition := 7;

  CurrentPrle := -1;

  SymPos := 0;
  SymLen := -1;

  if Totp > 0 then
    State := TPlayState.psPilot
  else if Totd > 0 then
    State := TPlayState.psData
  else
    State := TPlayState.psFinished;

end;

procedure TTzxBlock19.Details(out S: String);
begin
  S := FDetails;
end;

{$endif}

