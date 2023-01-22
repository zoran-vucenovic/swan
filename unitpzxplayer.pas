unit UnitPzxPlayer;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, UnitTapePlayer, UnitSpectrum, UnitCommon;

type
  TPzxBlock = class abstract (TTapeBlock)
  strict private
    FLen: Integer;
  strict protected
    type
      TPzxPlayState = (ppsStart, ppsPlaying, ppsFinished);
  strict protected
    State: TPzxPlayState;

  protected
    function LoadBlock2(const Stream: TStream): Boolean; virtual; abstract;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    function GetBlockLength: Integer; override;
    function LoadBlock(const Stream: TStream): Boolean; override;
    class function GetBlockId: Integer; override;
    procedure Start; override;
  end;

  TPzxPlayer = class(TTapePlayer)
  public
  protected
    procedure CheckNextBlock(); override;
    class function CheckHeader(const Stream: TStream): Boolean; override;
    class function GetNextBlockClass(const Stream: TStream): TTapeBlockClass;
      override;
    class function GetTapeType: TTapeType; override;
    class function CheckIsMyClass(const Stream: TStream): Boolean; override;

  strict private
    class var
      PzxBlocksMap: TTapeBlocksMap;

  private
    class procedure Init;
    class procedure Final;
  public
    procedure Rewind; override;
  end;

implementation

type

  TPzxBlockClass = class of TPzxBlock;

  TPzxBlockUnsuported = class(TPzxBlock)
  private
    BlockIdentifier: String;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    class function GetBlockDescription: String; override;
    procedure Start; override;
    class function GetBlockIdAsString: String; override;
    function GetNextPulse: Boolean; override;
    procedure Details(out S: String); override;
  end;

  TPzxBlockPZXT = class(TPzxBlock)
  private
    const
      SupportedPZXVersionMajor = 1;
      SupportedPZXVersionMinor = 0;

  private
    VerMajor: Byte;
    VerMinor: Byte;
    FDetails: String;

    function FillDetails(N: Integer; const Stream: TStream): Boolean;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    destructor Destroy; override;
    class function GetBlockDescription: String; override;
    procedure Start; override;
    procedure Details(out S: String); override;
    class function GetBlockIdAsString: String; override;
  end;

  TPzxBlockPULS = class(TPzxBlock)
  private
    type
      TPulse = record
        RepeatCount: Int16;
        Duration: Int32;
      end;
  private
    Pulses: Array of TPulse;
    
    TicksNeeded: Int64;
    CurrentPulseNumber: Integer;
    RepeatCount: Integer;
    Duration: Integer;

    FDetails: String;

    procedure FillDetails;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;

    class function GetBlockDescription: String; override;
    procedure Details(out S: String); override;
    class function GetBlockIdAsString: String; override;
    function GetNextPulse: Boolean; override;
    procedure Start; override;
  end;

  TPzxBlockDATA = class(TPzxBlock)
  private
    type
      TArrUInt16 = Array of UInt16;
  private
    TotalBits: Integer;
    InitialPulseLevel: Byte;
    Tail: UInt16;
    P0: Integer; // number of pulses encoding bit 0
    P1: Integer; // number of pulses encoding bit 1
    S0: TArrUInt16; // sequence of pulse durations encoding bit 0
    S1: TArrUInt16; // sequence of pulse durations encoding bit 1
    Data: PByte;

    P, PEnd: PByte;
    UnusedBitsInLastByte: Integer;
    BitPosition: Integer;
    PulsesNeeded: Integer;
    TicksNeeded: Int64;
    //TailStarted: Boolean;
    STail: TArrUInt16;
    SPulses: TArrUInt16;
    SPulsesPos: Integer;

    FDetails: String;

    procedure FillDetails;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    destructor Destroy; override;
    class function GetBlockDescription: String; override;
    class function GetBlockIdAsString: String; override;
    function GetNextPulse: Boolean; override;
    procedure Start; override;
    procedure Details(out S: String); override;
  end;

  TPzxBlockPAUS = class(TPzxBlock)
  private
    PauseLen: Integer;
    InitialPulseLevel: Byte;
    TicksNeeded: Int64;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    class function GetBlockDescription: String; override;
    class function GetBlockIdAsString: String; override;
    procedure Start; override;
    function GetNextPulse: Boolean; override;
    procedure Details(out S: String); override;
  end;

  TPzxBlockBRWS = class(TPzxBlock)
  private
    BrwsText: String;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    class function GetBlockDescription: String; override;
    class function GetBlockIdAsString: String; override;
    procedure Details(out S: String); override;
  end;

  TPzxBlockSTOP = class(TPzxBlock)
  private
    FlagStopOnlyIf48K: Boolean;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    class function GetBlockDescription: String; override;
    class function GetBlockIdAsString: String; override;
    procedure Details(out S: String); override;
    function GetStopPlaying: Boolean; override;
  end;

{ TPzxBlockSTOP }

function TPzxBlockSTOP.LoadBlock2(const Stream: TStream): Boolean;
var
  Flags: UInt16;
begin
  if GetBlockLength = 2 then begin
    if Stream.Read(Flags, 2) = 2 then begin
      FlagStopOnlyIf48K := (Flags and 1) = 1;
      Exit(True);
    end;
  end;
  Result := False;
end;

class function TPzxBlockSTOP.GetBlockDescription: String;
begin
  Result := 'Stop tape command';
end;

class function TPzxBlockSTOP.GetBlockIdAsString: String;
begin
  Result := 'STOP';
end;

procedure TPzxBlockSTOP.Details(out S: String);
begin
  S := 'Stop playing';
  if FlagStopOnlyIf48K then
    S := S + ' if in 48K mode';
end;

function TPzxBlockSTOP.GetStopPlaying: Boolean;
begin
  Result := True; // when non 48 models are supported, add check
end;

{ TPzxBlockBRWS }

function TPzxBlockBRWS.LoadBlock2(const Stream: TStream): Boolean;
begin
  SetLength(BrwsText, GetBlockLength);
  if GetBlockLength = 0 then
    Exit(True);
  Result := Stream.Read(BrwsText[1], GetBlockLength) = GetBlockLength;
  if Result then
    BrwsText :=
       StringReplace(
         StringReplace(
            StringReplace(BrwsText, #13#10, #13, [rfReplaceAll])
            , #10, #13, [rfReplaceAll])
         , #0, #13, [rfReplaceAll]);
end;

class function TPzxBlockBRWS.GetBlockDescription: String;
begin
  Result := 'Browse point';
end;

class function TPzxBlockBRWS.GetBlockIdAsString: String;
begin
  Result := 'BRWS';
end;

procedure TPzxBlockBRWS.Details(out S: String);
begin
  S := BrwsText;
end;

{ TPzxBlockUnsuported }

function TPzxBlockUnsuported.LoadBlock2(const Stream: TStream): Boolean;
var
  N: Int64;
  P: Int64;
begin
  Stream.Seek(Int64(-8), TSeekOrigin.soCurrent);
  SetLength(BlockIdentifier, 4);
  Stream.Read(BlockIdentifier[1], 4);
  Stream.Seek(Int64(4), TSeekOrigin.soCurrent);
  if GetBlockLength > 0 then begin
    P := Stream.Position;
    Result := Stream.Seek(Int64(GetBlockLength), TSeekOrigin.soCurrent) = P + GetBlockLength;
  end else
    Result := True;
end;

class function TPzxBlockUnsuported.GetBlockDescription: String;
begin
  Result := 'Block not supported';
end;

procedure TPzxBlockUnsuported.Start;
begin
  inherited Start;
  State := ppsFinished;
end;

class function TPzxBlockUnsuported.GetBlockIdAsString: String;
begin
  Result := '';
end;

function TPzxBlockUnsuported.GetNextPulse: Boolean;
begin
  Result := inherited GetNextPulse;
end;

procedure TPzxBlockUnsuported.Details(out S: String);
begin
  //inherited Details(S);
  S := 'Unknown block "' + BlockIdentifier + '"';
end;

{ TPzxBlockPAUS }

function TPzxBlockPAUS.LoadBlock2(const Stream: TStream): Boolean;
var
  N: UInt32;
begin
  Result := False;

  if GetBlockLength >= 4 then begin
    if Stream.Read(N, 4) = 4 then begin
      N := LEtoN(N);
      InitialPulseLevel := 0;
      if N shr 31 <> 0 then begin
        N := N and $7FFFFFFF;
        InitialPulseLevel := %01000000;
      end;
      PauseLen := N;
      Result := True;
    end;
  end;
end;

class function TPzxBlockPAUS.GetBlockDescription: String;
begin
  Result := 'Pause';
end;

class function TPzxBlockPAUS.GetBlockIdAsString: String;
begin
  Result := 'PAUS';
end;

procedure TPzxBlockPAUS.Start;
begin
  inherited Start;
  FTapePlayer.InPause := True;
  TicksNeeded := TicksNeeded.MinValue;
end;

function TPzxBlockPAUS.GetNextPulse: Boolean;
var
  ProcTicks: Int64;
begin
  if State = ppsFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    Inc(State);
    TicksNeeded := ProcTicks + PauseLen;
  end;
  Result := True;
end;

procedure TPzxBlockPAUS.Details(out S: String);
var
  M: Int64;
begin
  //inherited Details(S);
  M := PauseLen;
  M := (M + 1750) div 3500;

  S := Format('Initial pulse level: %d%sPause length: %d ticks (%d ms)',
    [InitialPulseLevel, #13, PauseLen, M]);
end;

{ TPzxBlockDATA }

procedure TPzxBlockDATA.FillDetails;
begin
  FDetails :=
    Format('Count (bits): %d%sInitial pulse level: %d%sTail: %d%sP0: %d%sP1: %d',
      [TotalBits, #13, InitialPulseLevel, #13, Tail, #13, P0, #13, P1]);
end;

function TPzxBlockDATA.LoadBlock2(const Stream: TStream): Boolean;

  function LoadS(const P: Integer; var Arr: TArrUInt16): Boolean;
  var
    I: Integer;
    W: Word;
  begin
    SetLength(Arr, P);
    for I := 0 to P - 1 do begin
      if Stream.Read(W{%H-}, 2) <> 2 then
        Exit(False);
      Arr[I] := LEtoN(W);
    end;
    Result := True;
  end;

var
  B: Byte;
  N: UInt32;
  TotalBytes: Integer;
begin
  Result := False;

  if GetBlockLength > 8 then begin
    if Stream.Read(N, 4) = 4 then begin
      N := LEtoN(N);
      InitialPulseLevel := 0;
      if N shr 31 <> 0 then begin
        N := N and $7FFFFFFF;
        InitialPulseLevel := %01000000;
      end;
      TotalBits := N;
      if Stream.Read(Tail, 2) = 2 then begin
        Tail := LEtoN(Tail);
        if Stream.Read(B, 1) = 1 then begin
          P0 := B;
          if Stream.Read(B, 1) = 1 then begin
            P1 := B;
            TotalBytes := (TotalBits + 7) div 8;
            if (GetBlockLength = TotalBytes + (P0 + P1) * 2 + 8)
              and LoadS(P0, S0) and LoadS(P1, S1)
            then begin
              try
                Data := GetMem(TotalBytes);
                if Stream.Read(Data^, TotalBytes) = TotalBytes then begin
                  PEnd := Data + (TotalBytes - 1);
                  UnusedBitsInLastByte := TotalBytes * 8 - TotalBits;
                  if Tail > 0 then begin
                    SetLength(STail, 1);
                    STail[0] := Tail;
                  end else
                    SetLength(STail, 0);
                  Result := True;
                  FillDetails;
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

constructor TPzxBlockDATA.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  Data := nil;
  FDetails := '';
end;

destructor TPzxBlockDATA.Destroy;
begin
  if Data <> nil then
    Freemem(Data);
  inherited Destroy;
end;

class function TPzxBlockDATA.GetBlockDescription: String;
begin
  Result := 'Data block';
end;

class function TPzxBlockDATA.GetBlockIdAsString: String;
begin
  Result := 'DATA';
end;

function TPzxBlockDATA.GetNextPulse: Boolean;

  procedure CheckTail;
  begin
    if (Length(STail) > 0) and (SPulses <> STail) then begin
      SPulses := STail;
      PulsesNeeded := 1;
    end else
      State := ppsFinished;
  end;

  procedure GetNextDataBit;
  begin
    SPulsesPos := 0;

    if (P < PEnd) or ((P = PEnd) and (BitPosition >= UnusedBitsInLastByte)) then begin
      if ((P^ shr BitPosition) and 1) = 0 then begin
        SPulses := S0;
        PulsesNeeded := P0;
      end else begin
        SPulses := S1;
        PulsesNeeded := P1;
      end;

      if BitPosition > 0 then
        Dec(BitPosition)
      else begin
        BitPosition := 7;
        Inc(P);
      end;
    end else
      CheckTail;

  end;

var
  ProcTicks: Int64;
begin
  if State = ppsFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    if State <> ppsStart then
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000
    else begin
      FTapePlayer.ActiveBit := Self.InitialPulseLevel;
      State := ppsPlaying;
    end;
    Inc(SPulsesPos);
    if SPulsesPos >= PulsesNeeded then begin
      GetNextDataBit;
    end;
    TicksNeeded := ProcTicks + SPulses[SPulsesPos];
  end;

  Result := True;
end;

procedure TPzxBlockDATA.Start;
begin
  inherited Start;

  P := Data;
  TicksNeeded := TicksNeeded.MinValue;
  PulsesNeeded := 1;
  SPulsesPos := 0;
  BitPosition := 7;
  SPulses := S0;
end;

procedure TPzxBlockDATA.Details(out S: String);
begin
  S := FDetails;
end;

{ TPzxBlockPULS }

procedure TPzxBlockPULS.FillDetails;
var
  I, N: Integer;
  S: String;
begin
  N := Length(Pulses);
  FDetails := 'Pulses count: ' + N.ToString;
  if N > 12 then begin
    S := #13 + Format(' ... (%d pulses more)', [N - 10]);
    N := 10;
  end else
    S := '';
  for I := 0 to N - 1 do begin
    FDetails := FDetails + #13 + Format('%3d. %d ticks', [I + 1, Pulses[I].Duration]);
    if Pulses[I].RepeatCount > 1 then
      FDetails := FDetails + ', repeted ' + Pulses[I].RepeatCount.ToString + ' times';
  end;
  FDetails := FDetails + S;
end;

function TPzxBlockPULS.LoadBlock2(const Stream: TStream): Boolean;
var
  N, Count: Integer;
  D: UInt16;

  function ReadNextWord(): Boolean;
  begin
    Inc(N, 2);
    if (N <= GetBlockLength) and (Stream.Read(D, 2) = 2) then begin
      D := LEtoN(D);
      Exit(True);
    end;
    Count := 0;
    Result := False;
  end;                              

var
  Pulse: TPulse;
begin
  Result := False;

  Count := 0;
  N := 0;
  SetLength(Pulses, GetBlockLength div 6 + 2);

  while ReadNextWord do begin
    Pulse.RepeatCount := 1;
    if D > $8000 then begin
      Pulse.RepeatCount := D and $7FFF;
      if not ReadNextWord() then
        Break;
    end;
    Pulse.Duration := D and $7FFF;
    if D >= $8000 then begin
      if not ReadNextWord() then
        Break;
      Pulse.Duration := (Pulse.Duration shl 16) or D;
    end;

    if Pulse.Duration = 0 then
      Pulse.RepeatCount := Pulse.RepeatCount and 1;

    if Pulse.RepeatCount > 0 then begin
      if Length(Pulses) <= Count then
        SetLength(Pulses, (Length(Pulses) * 7) div 5 + 2);
      Pulses[Count] := Pulse;
      Inc(Count);
    end;

    if N = GetBlockLength then begin
      Result := True;
      FillDetails;
      Break;
    end;
  end;

  SetLength(Pulses, Count);
end;

constructor TPzxBlockPULS.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FDetails := '';
  SetLength(Pulses, 0);
end;

class function TPzxBlockPULS.GetBlockDescription: String;
begin
  Result := 'Pulse sequence';
end;

procedure TPzxBlockPULS.Details(out S: String);
begin
  inherited Details(S);
  S := FDetails;
end;

class function TPzxBlockPULS.GetBlockIdAsString: String;
begin
  Result := 'PULS';
end;

function TPzxBlockPULS.GetNextPulse: Boolean;
var
  ProcTicks: Int64;
begin
  if State = ppsFinished then
    Exit(False);
  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    if State = ppsPlaying then
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000
    else begin
      FTapePlayer.ActiveBit := 0;
      State := ppsPlaying;
    end;
    
    Dec(RepeatCount);
    if RepeatCount <= 0 then begin
      repeat
        Inc(CurrentPulseNumber);
        if CurrentPulseNumber >= Length(Pulses) then begin
          State := ppsFinished;
          Break;
        end;
        Duration := Pulses[CurrentPulseNumber].Duration; 
        RepeatCount := Pulses[CurrentPulseNumber].RepeatCount;
        if Duration > 0 then
          Break;
        FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;
      until False;
    end;

    TicksNeeded := ProcTicks + Duration;
  end;

  Result := True;
end;

procedure TPzxBlockPULS.Start;
begin
  inherited Start;

  RepeatCount := 0;
  CurrentPulseNumber := -1;
  TicksNeeded := TicksNeeded.MinValue;
end;

{ TPzxBlockPZXT }

function TPzxBlockPZXT.FillDetails(N: Integer; const Stream: TStream): Boolean;
var
  S, S0: String;
  I, J, P: Integer;

begin
  Result := N <= 0;
  FDetails := '';
  if not Result then begin
    SetLength(S, N);
    if Stream.Read(S[1], N) = N then begin
      J := 1;
      I := 1;
      while I < N do begin
        P := Pos(#0, S, I);
        if P = 0 then
          P := N + 1;
        S0 := Copy(S, I, P - I);
        UnitCommon.ConvertCodePageFromISO8859_1_to_Utf8(S0);
        if J and 1 = 0 then begin
          S0 := #13 + UTF8Trim(S0);
          if S0[Length(S0)] <> ':' then
            S0 := S0 + ':';
          S0 := S0 + ' ';
        end;
        FDetails := FDetails + S0;
        I := P + 1;
        if I >= N then
          Break;
        Inc(J);          
      end;

      Result := True;
    end;
  end;

  if Result then begin
    S0 := '';
    if VerMajor > SupportedPZXVersionMajor then begin
      S0 := 'WARNING: %sTape loading might not%s!';
    end else if VerMajor = SupportedPZXVersionMajor then begin
      if VerMinor > SupportedPZXVersionMinor then
        S0 := 'Note: %sTape loading should still%s.';
    end;
    if S0 <> '' then begin
      S0 := #13 + Format(S0, [Format('Swan supports PZX ver %d.%d%s', [SupportedPZXVersionMajor, SupportedPZXVersionMinor, #13]), ' work correctly']);
      if FDetails <> '' then
        S0 := S0 + #13 + ' ';
    end;
    if FDetails <> '' then
      S0 := S0 + #13;

    FDetails := Format('PZX ver. %d.%d', [VerMajor, VerMinor]) + S0 + FDetails;
  end;
end;

function TPzxBlockPZXT.LoadBlock2(const Stream: TStream): Boolean;
var
  N: Integer;
begin
  if GetBlockLength >= 2 then begin
    if (Stream.Read(VerMajor, 1) = 1) and (Stream.Read(VerMinor, 1) = 1) then begin
      N := GetBlockLength - 2;
      if FillDetails(N, Stream) then
        Exit(True);
    end;
  end;
  Result := False;
end;

constructor TPzxBlockPZXT.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FDetails := '';
end;

destructor TPzxBlockPZXT.Destroy;
begin

  inherited Destroy;
end;

class function TPzxBlockPZXT.GetBlockDescription: String;
begin
  Result := 'PZX header block';
end;

procedure TPzxBlockPZXT.Start;
begin
  inherited Start;
  State := ppsFinished;
end;

procedure TPzxBlockPZXT.Details(out S: String);
begin
  inherited Details(S);
  S := S + FDetails;
end;

class function TPzxBlockPZXT.GetBlockIdAsString: String;
begin
  Result := 'PZXT';
end;

{ TPzxBlock }

function TPzxBlock.GetBlockLength: Integer;
begin
  Result := FLen;
end;

constructor TPzxBlock.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FLen := 0;
  State := ppsFinished;
end;

function TPzxBlock.LoadBlock(const Stream: TStream): Boolean;
var
  M: UInt32;
  N: Int32 absolute M;
begin
  Result := False;
  if Stream.Read(M{%H-}, 4) = 4 then begin
    M := LEtoN(M);
    if N >= 0 then begin
      FLen := N;
      if Stream.Size - Stream.Position >= N then
        Result := LoadBlock2(Stream);
    end;
  end;
end;

class function TPzxBlock.GetBlockId: Integer;
var
  S: String;
begin
  Result := 0;
  S := GetBlockIdAsString;
  if Length(S) = 4 then begin
    Move(S[1], Result, 4);
    //Result := BEtoN(Result);
  end;
end;

procedure TPzxBlock.Start;
begin
  inherited Start;
  State := ppsStart;
end;

{ TPzxPlayer }

procedure TPzxPlayer.CheckNextBlock;
begin
  if FCurrentBlock.GetStopPlaying then
    StopPlaying
  else begin
    StartBlock(FCurrentBlockNumber + 1);
  end;
end;

class function TPzxPlayer.CheckHeader(const Stream: TStream): Boolean;
var
  P: Int64;
begin
  Result := False;
  P := Stream.Position;
  Result := GetNextBlockClass(Stream).InheritsFrom(TPzxBlockPZXT.ClassType);
  Result := (Stream.Seek(P - Stream.Position, TSeekOrigin.soCurrent) = P) and Result;
end;

class function TPzxPlayer.GetNextBlockClass(const Stream: TStream
  ): TTapeBlockClass;
var
  D: DWord;
  C: TTapeBlockClass;
begin
  Result := nil;
  if Stream.Size >= Stream.Position + 8 then begin
    if Stream.Read(D{%H-}, 4) = 4 then begin
      if PzxBlocksMap.TryGetData(D, C) then begin
        // the pzx specification says that the first block must be PZXT:
        //if (GetBlockCount > 0) or C.InheritsFrom(TPzxBlockPZXT.ClassType) then
          Result := C;
      end else
        Result := TPzxBlockUnsuported;
    end;
  end;
end;

class function TPzxPlayer.GetTapeType: TTapeType;
begin
  Result := ttPzx;
end;

class procedure TPzxPlayer.Init;

  procedure AddBlockClass(BlockClass: TPzxBlockClass);
  begin
    PzxBlocksMap.Add(BlockClass.GetBlockId, BlockClass);
  end;

begin
  RegisterTapePlayerClass(Self);

  PzxBlocksMap := TTapeBlocksMap.Create;

  AddBlockClass(TPzxBlockPZXT);
  AddBlockClass(TPzxBlockPULS);
  AddBlockClass(TPzxBlockDATA);
  AddBlockClass(TPzxBlockPAUS);
  AddBlockClass(TPzxBlockBRWS);
  AddBlockClass(TPzxBlockSTOP);
end;

class procedure TPzxPlayer.Final;
begin
  PzxBlocksMap.Free;
end;

procedure TPzxPlayer.Rewind;
begin
  inherited Rewind;
end;

class function TPzxPlayer.CheckIsMyClass(const Stream: TStream): Boolean;
begin
  Result := CheckHeader(Stream);
end;

initialization
  TPzxPlayer.Init;

finalization
  TPzxPlayer.Final;

end.

