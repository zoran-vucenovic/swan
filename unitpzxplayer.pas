unit UnitPzxPlayer;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

// pzx tape format
// homepage: http://zxds.raxoft.cz/pzx.html
// pzx format specification: http://zxds.raxoft.cz/docs/pzx.txt

interface

uses
  Classes, SysUtils, LazUTF8, UnitTapePlayer, UnitSpectrum, UnitCommon;

implementation

type

  TPzxPlayer = class(TTapePlayer)
  protected
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
  end;

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
    class function GetBlockId: DWord; override;
    procedure Start; override;
  end;

  TPzxBlockUnsupported = class(TPzxBlock)
  private
    BlockIdentifier: String;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;

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
    FDetails: AnsiString;

    function FillDetails(N: Integer; const Stream: TStream): Boolean;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    class function GetBlockDescription: String; override;
    procedure Start; override;
    procedure Details(out S: String); override;
    class function GetBlockIdAsString: String; override;
  end;

  TPzxBlockPULS = class(TPzxBlock)
  strict private
    type
      TPulse = record
        RepeatCount: Int16;
        Duration: Int32;
      end;
  strict private
    Pulses: Array of TPulse;

    TicksNeeded: Int64;
    CurrentPulseNumber: Integer;
    RepeatCount: Integer;
    Duration: Int64;

    FDetails: String;

    procedure FillDetails;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
    function IsReallyPlayableBlock: Boolean; override;
    function GetTicksNextEdge: Int64; override;
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
    STail: TArrUInt16;
    SPulses: TArrUInt16;
    SPulsesPos: Integer;

    FDetails: String;

    procedure FillDetails;
  protected
    function LoadBlock2(const Stream: TStream): Boolean; override;
    function IsReallyPlayableBlock: Boolean; override;
    function GetTicksNextEdge: Int64; override;
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
    function GetTicksNextEdge: Int64; override;
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
    if Stream.Read(Flags{%H-}, 2) = 2 then begin
      Flags := LEtoN(Flags);
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
  Result := not (FlagStopOnlyIf48K and FTapePlayer.GetSpectrum.Is128KModel);
end;

{ TPzxBlockBRWS }

function TPzxBlockBRWS.LoadBlock2(const Stream: TStream): Boolean;
begin
  SetLength(BrwsText, GetBlockLength);
  if GetBlockLength = 0 then
    Exit(True);
  Result := Stream.Read(BrwsText[1], GetBlockLength) = GetBlockLength;
  if Result then begin
    BrwsText :=
       StringReplace(
         StringReplace(
            StringReplace(BrwsText, #13#10, #13, [rfReplaceAll])
            , #10, #13, [rfReplaceAll])
         , #0, #13, [rfReplaceAll]);

    // See the comment inside TPzxBlockPZXT.FillDetails method
    TCommonFunctions.ConvertCodePageFromCp1252ToUtf8(BrwsText);
  end;
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

function TPzxBlockUnsupported.LoadBlock2(const Stream: TStream): Boolean;
var
  P: Int64;
begin
  P := Stream.Position + GetBlockLength;

  Stream.Seek(Int64(-8), TSeekOrigin.soCurrent);
  SetLength(BlockIdentifier, 4);
  Stream.Read(BlockIdentifier[1], 4);

  Result := Stream.Seek(Int64(4) + GetBlockLength, TSeekOrigin.soCurrent) = P;
end;

constructor TPzxBlockUnsupported.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  BlockIdentifier := '';
end;

class function TPzxBlockUnsupported.GetBlockDescription: String;
begin
  Result := 'Block not supported';
end;

procedure TPzxBlockUnsupported.Start;
begin
  inherited Start;
  State := ppsFinished;
end;

class function TPzxBlockUnsupported.GetBlockIdAsString: String;
begin
  Result := '';
end;

function TPzxBlockUnsupported.GetNextPulse: Boolean;
begin
  Result := inherited GetNextPulse;
end;

procedure TPzxBlockUnsupported.Details(out S: String);
begin
  S := 'Unknown block "' + BlockIdentifier + '"';
end;

{ TPzxBlockPAUS }

function TPzxBlockPAUS.LoadBlock2(const Stream: TStream): Boolean;
var
  N: UInt32;
begin
  Result := False;

  if GetBlockLength >= 4 then begin
    if Stream.Read(N{%H-}, 4) = 4 then begin
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

function TPzxBlockPAUS.GetTicksNextEdge: Int64;
var
  N: Int64;
  B: Byte;
begin
  Result := TicksNeeded;
  B := not FTapePlayer.GetSpectrum.GetProcessor.RegB;
  N := Result - GetCurrentTotalSpectrumTicks;
  N := (N - 385) div 59;
  if N < B then
    Result := inherited GetTicksNextEdge;
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
  TicksNeeded0: Int64;

begin
  if State = ppsFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    Inc(State);

    FTapePlayer.ActiveBit := InitialPulseLevel;
    TicksNeeded0 := PauseLen;
    AdjustTicksIfNeeded(TicksNeeded0);
    TicksNeeded := ProcTicks + TicksNeeded0;
  end;
  Result := True;
end;

procedure TPzxBlockPAUS.Details(out S: String);
var
  M: Int64;
begin
  M := PauseLen;
  M := (M + TTapePlayer.TicksPerMilliSecond div 2) div TTapePlayer.TicksPerMilliSecond;

  if InitialPulseLevel <> 0 then
    S := 'high'
  else
    S := 'low';

  S := Format('Initial pulse level: %s%sPause length: %d ticks (',
    [S, #13, PauseLen]);
  if M * TTapePlayer.TicksPerMilliSecond <> PauseLen then
    S := S + '~';
  S := S + M.ToString + ' ms)';
end;

{ TPzxBlockDATA }

procedure TPzxBlockDATA.FillDetails;

  function GetSArr(SArr: TArrUInt16): String;
  var
    I, N: Integer;
    MaybeComma, S: String;
  begin
    N := Length(SArr);
    if N = 0 then
      Result := ' <empty>'
    else begin
      if N > 26 then begin
        N := 22;
        S := '... (' + IntToStr(Length(SArr) - N) + ' more)';
      end else
        S := '';

      MaybeComma := ' ';
      Result := '';
      for I := 0 to N - 1 do begin
        Result := Result + MaybeComma + IntToStr(SArr[I]);
        MaybeComma := ', ';
      end;
      Result := Result + S;
    end;
  end;

var
  S: String;
begin
  if InitialPulseLevel <> 0 then
    S := 'high'
  else
    S := 'low';

  FDetails :=
    Format('Count (bits): %d%sInitial pulse level: %s%sTail: %d%sP0: %d%sP1: %d%s%s%s%s%s%s',
      [TotalBits, #13, S, #13, Tail, #13, P0, #13, P1, #13, 'S0:', GetSArr(S0), #13, 'S1:', GetSArr(S1)]);
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
    if Stream.Read(N{%H-}, 4) = 4 then begin
      N := LEtoN(N);
      InitialPulseLevel := 0;
      if N shr 31 <> 0 then begin
        N := N and $7FFFFFFF;
        InitialPulseLevel := %01000000;
      end;
      TotalBits := N;
      if Stream.Read(Tail, 2) = 2 then begin
        Tail := LEtoN(Tail);
        if Stream.Read(B{%H-}, 1) = 1 then begin
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

function TPzxBlockDATA.IsReallyPlayableBlock: Boolean;
begin
  Result := True;
end;

function TPzxBlockDATA.GetTicksNextEdge: Int64;
var
  N: Int64;
  B: Byte;

begin
  Result := TicksNeeded;
  if SPulsesPos < PulsesNeeded then
    Exit;

  if SPulses <> STail then begin
    if Length(STail) > 0 then
      Exit;
    if (P < PEnd) or ((P = PEnd) and (BitPosition >= UnusedBitsInLastByte)) then
      Exit;
  end;

  B := not FTapePlayer.GetSpectrum.GetProcessor.RegB;
  N := Result - GetCurrentTotalSpectrumTicks;
  N := (N - 385) div 59;
  if N < B then
    Result := inherited GetTicksNextEdge;
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
  TicksNeeded0: Int64;

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
    TicksNeeded0 := SPulses[SPulsesPos];
    AdjustTicksIfNeeded(TicksNeeded0);
    TicksNeeded := ProcTicks + TicksNeeded0;
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
    S := #13 + Format('  ... (and %d more)', [N - 10]);
    N := 10;
  end else
    S := '';
  for I := 0 to N - 1 do begin
    FDetails := FDetails + #13 + Format('%3d. %d ticks', [I + 1, Pulses[I].Duration]);
    if Pulses[I].RepeatCount > 1 then
      FDetails := FDetails + ', repeated ' + Pulses[I].RepeatCount.ToString + ' times';
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

    Result := False;
  end;

var
  Pulse: TPulse;
begin
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

    if Pulse.Duration = 0 then begin
      //treat odd repeat count as 1, skip even
      Pulse.RepeatCount := Pulse.RepeatCount and 1;

      // if two consecutive duration zero pulses, skip both:
      if (Pulse.RepeatCount = 1) and (Count > 0) and (Pulses[Count - 1].Duration = 0) then begin
        Pulse.RepeatCount := 0; // skip this pulse
        Dec(Count); // and remove the previous one
      end;
    end;

    if Pulse.RepeatCount > 0 then begin
      if Length(Pulses) <= Count then
        SetLength(Pulses, (Count * 7) div 5 + 2);
      Pulses[Count] := Pulse;
      Inc(Count);
    end;

    if N = GetBlockLength then begin
      SetLength(Pulses, Count);
      FillDetails;
      Exit(True);
    end;
  end;

  SetLength(Pulses, 0);
  Result := False;
end;

function TPzxBlockPULS.IsReallyPlayableBlock: Boolean;
begin
  Result := True;
end;

function TPzxBlockPULS.GetTicksNextEdge: Int64;
var
  I: Integer;
  L: Integer;
  Prev: Integer;
  N, R: Int64;
  B: Byte;

begin
  Result := TicksNeeded;
  if RepeatCount > 0 then
    Exit;

  R := 0;
  Prev := 0;
  I := CurrentPulseNumber + 1;
  L := Length(Pulses);
  while I < L do begin
    if Pulses[I].Duration <> 0 then begin
      if Prev <> 0 then
        Break;
      Prev := Pulses[I].Duration;
      R := R + Prev;
      if Pulses[I].RepeatCount > 1 then
        Break;
    end else
      Prev := 0;

    Inc(I);
  end;
  AdjustTicksIfNeeded(R);
  Result := Result + R;

  if I = L then begin
    B := not FTapePlayer.GetSpectrum.GetProcessor.RegB;
    N := Result - GetCurrentTotalSpectrumTicks;
    N := (N - 385) div 59;
    if N < B then
      Result := inherited GetTicksNextEdge;
  end;

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
        if Duration > 0 then begin
          AdjustTicksIfNeeded(Duration);
          Break;
        end;
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
  S, S0: RawByteString;
  I, P: Integer;
  IsKey: Boolean;

begin
  Result := N <= 0;
  FDetails := '';
  if not Result then begin
    SetLength(S{%H-}, N);
    if Stream.Read(S[1], N) = N then begin
      IsKey := False; // begin with the title, then alternate key - value
      I := 1;
      while I < N do begin
        P := Pos(#0, S, I);
        if P = 0 then
          P := N + 1;
        S0 := UTF8Trim(Copy(S, I, P - I));

        if IsKey then begin
          S0 := #13 + S0;
        end else begin
          if FDetails <> '' then
            case FDetails[Length(FDetails)] of
              #13, ':':
                ;
            otherwise
              S0 := ': ' + S0;
            end;
        end;

        FDetails := FDetails + S0;
        I := P + 1;
        IsKey := not IsKey;
      end;

      // Unilike tzx, pzx specification says that texts should be utf8 encoded.
      // However, this rule is immediately broken there by the files provided
      // for testing, which have text encoded in cp1252 (iso-8859-1).
      // See the pound character in header block of these games:
      //   - World Cup Carnival
      //   - Super Scramble
      //   - Atlantis
      //   - Myth
      // So, let's treat text as cp1252, as we do with tzx files:
      TCommonFunctions.ConvertCodePageFromCp1252ToUtf8(FDetails);

      Result := True;
    end;
  end;

  if Result then begin
    // Unilike tzx specification, the pzx specification says that any
    // implementation should accept only files whose major version it implements
    // and reject anything else. An implementation may report a warning in case
    // it encounters minor greater than it implements for given major, if the
    // implementor finds it convenient to do so.
    // We will show WARNING when major version is higher than what we implement,
    // and NOTE when only minor version is higher.
    // We will try to load the file anyway.
    S0 := '';
    if VerMajor > SupportedPZXVersionMajor then begin
      S0 := 'WARNING: %smight not%s!';
    end else if VerMajor = SupportedPZXVersionMajor then begin
      if VerMinor > SupportedPZXVersionMinor then
        S0 := 'NOTE: %sshould still%s.';
    end;
    if S0 <> '' then begin
      S0 := #13
        + Format(S0, [
            Format('Swan supports PZX ver %d.%d%sTape loading ', [
              SupportedPZXVersionMajor, SupportedPZXVersionMinor, #13
            ])
            , ' work correctly'
          ]);

      if FDetails <> '' then
        S0 := S0 + #13;
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
  VerMajor := 0;
  VerMinor := 0;
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

class function TPzxBlock.GetBlockId: DWord;
var
  S: AnsiString;
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

class function TPzxPlayer.CheckHeader(const Stream: TStream): Boolean;
var
  P: Int64;
  C: TTapeBlockClass;
begin
  Result := False;
  P := Stream.Position;
  // the pzx specification says that the first block must be PZXT:
  C := GetNextBlockClass(Stream);
  Result := Assigned(C) and C.InheritsFrom(TPzxBlockPZXT.ClassType);
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
      if PzxBlocksMap.TryGetData(D, C) then
        Result := C
      else
        Result := TPzxBlockUnsupported;
    end;
  end;
end;

class function TPzxPlayer.GetTapeType: TTapeType;
begin
  Result := ttPzx;
end;

class procedure TPzxPlayer.Init;

  procedure AddBlockClass(BlockClass: TTapeBlockClass);
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

class function TPzxPlayer.CheckIsMyClass(const Stream: TStream): Boolean;
begin
  Result := CheckHeader(Stream);
end;

initialization
  TPzxPlayer.Init;

finalization
  TPzxPlayer.Final;

end.

