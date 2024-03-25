unit UnitCSW;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

// CSW (compressed square wave) format
// The CSW specification:
//   [archived on 2022-02-20, as the original link is not available now (2024-03-24)]
// http://web.archive.org/web/20220220082010/http://ramsoft.bbk.org.omegahg.com/csw.html

interface

uses
  Classes, SysUtils, UnitTapePlayer, UnitSpectrum, UnitCommon,
  UnitStreamCompression;

type

  TCswBlock = class (TTapeBlock)
  public
    type
      TCswCompressionType = (cctRle, cctZRle);
      TCswPlayState = (cpsStart, cpsPlaying, cpsFinished);
  strict private
    FState: TCswPlayState;
    MS: TMemoryStream;
    P: PByte;
    PEnd: PByte;

    FInitialPolarity: Byte; // 0 or 64

    FSampleRate: Integer;
    FHalfSampleRate: Integer;
    FTotalNumOfPulses: Integer;
    FCountedNumOfPulses: Integer;
    FCompressionType: TCswCompressionType;
    FCswDataLength: Integer;
    FCswMajorRevisionNumber: Byte;
    FCswMinorRevisionNumber: Byte;

    TicksNeeded: Int64;
    FProcTicksPerSec: Int64;

    procedure SetInitialPolarity(AValue: Byte);
    procedure SetSampleRate(AValue: Integer);

    function LoadCswData(const Stream: TStream; const L: Integer): Boolean;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    destructor Destroy; override;

    function CountNumberOfPulses(): Integer;
    function GetBlockLength: Integer; override;
    class function GetBlockDescription: String; override;
    function LoadBlock(const Stream: TStream): Boolean; override;
    class function GetBlockId: DWord; override;
    class function GetBlockIdAsString: String; override;
    procedure Start; override;
    function GetNextPulse: Boolean; override;
    procedure Details(out S: String); override;

    property State: TCswPlayState read FState;
    property SampleRate: Integer read FSampleRate write SetSampleRate;
    property TotalNumOfPulses: Integer read FTotalNumOfPulses write FTotalNumOfPulses;
    property InitialPolarity: Byte read FInitialPolarity write SetInitialPolarity;
    property CompressionType: TCswCompressionType read FCompressionType write FCompressionType;
    property CswDataLength: Integer read FCswDataLength write FCswDataLength;
    property CswMajorRevisionNumber: Byte read FCswMajorRevisionNumber write FCswMajorRevisionNumber;
    property CswMinorRevisionNumber: Byte read FCswMinorRevisionNumber write FCswMinorRevisionNumber;

  end;

  TCswPlayer = class(TTapePlayer)
  protected
    procedure CheckNextBlock(); override;
    class function CheckHeader(const Stream: TStream): Boolean; override;
    class function GetNextBlockClass(const {%H-}Stream: TStream): TTapeBlockClass;
      override;
    class function GetTapeType: TTapeType; override;
    class function CheckIsMyClass(const Stream: TStream): Boolean; override;
  private
    class procedure Init;
  end;

implementation

type
  TCswBlockHeader1 = packed record
    SampleRate: Word;
    CompressionType: Byte; // 1: RLE
    Flags: Byte; // bit 0 -- initial polarity, 1 - high, 0 - low
    Reserved: packed array[0..2] of Byte;
  end;

  TCswBlockHeader2 = packed record
    SampleRate: DWord;
    TotalNumOfPulses: DWord; // total number of pulses (after decompression)
    CompressionType: Byte; // 1: RLE, 2: Z-RLE
    Flags: Byte; // bit 0 -- initial polarity, 1 - high, 0 - low
    HDR: Byte; // Header extension length in bytes - for future expansions only, must be zero in version 2
    EncodingApplicationDescription: array [0..15] of AnsiChar;
  end;

  TCswBlock1 = class(TCswBlock)
  strict private
    FLen: Integer;
  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    function LoadBlock(const Stream: TStream): Boolean; override;
    function GetBlockLength: Integer; override;
    procedure Details(out S: String); override;
  end;

{ TCswBlock1 }

constructor TCswBlock1.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FLen := 0;
end;

function TCswBlock1.LoadBlock(const Stream: TStream): Boolean;
var
  Header1: TCswBlockHeader1;
  Header2: TCswBlockHeader2;
  VMajor, VMinor: Byte;
  L: Integer;

  Okay: Boolean;
begin
  Result := False;
  L := Stream.Size - Stream.Position;
  if L >= 2 then begin
    if (Stream.Read(VMajor{%H-}, 1) = 1) and (Stream.Read(VMinor{%H-}, 1) = 1) then begin
      case VMajor of
        1:
          begin
            Okay := (Stream.Size - Stream.Position >= SizeOf(Header1))
              and (Stream.Read(Header1{%H-}, SizeOf(Header1)) = SizeOf(Header1));
            if Okay then begin
              Header1.SampleRate := LEtoN(Header1.SampleRate);
              InitialPolarity := Header1.Flags;
              SampleRate := Header1.SampleRate;
              TotalNumOfPulses := 0;
              CompressionType := cctRle;
            end;
          end;
      otherwise
        Okay := (Stream.Size - Stream.Position >= SizeOf(Header2))
              and (Stream.Read(Header2{%H-}, SizeOf(Header2)) = SizeOf(Header2));
        if Okay then begin
          Header2.SampleRate := LEtoN(Header2.SampleRate);
          Header2.TotalNumOfPulses := LEtoN(Header2.TotalNumOfPulses);

          InitialPolarity := Header2.Flags;
          SampleRate := Header2.SampleRate;
          TotalNumOfPulses := Header2.TotalNumOfPulses;
          if Header2.CompressionType = 2 then
            CompressionType := cctZRle
          else
            CompressionType := cctRle;

          if Header2.HDR > 0 then begin
            Stream.Seek(Int64(Header2.HDR), TSeekOrigin.soCurrent);
          end;

        end;
      end;


      if Okay then begin
        CswMajorRevisionNumber := VMajor;
        CswMinorRevisionNumber := VMinor;
        CswDataLength := Stream.Size - Stream.Position;

        if inherited LoadBlock(Stream) then begin
          FLen := L;
          Result := True;
        end;
      end;

    end;
  end;
end;

function TCswBlock1.GetBlockLength: Integer;
begin
  Result := FLen;
end;

procedure TCswBlock1.Details(out S: String);
begin
  inherited Details(S);
  S := S + #13 + 'initial polarity: ' + InitialPolarity.ToString;
end;

{ TCswBlock }

procedure TCswBlock.SetInitialPolarity(AValue: Byte);
begin
  FInitialPolarity := AValue and 1;
end;

constructor TCswBlock.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FCountedNumOfPulses := -2;
  MS := nil;
  P := nil;
  PEnd := nil;
  FState := cpsFinished;
  FInitialPolarity := 0;
  SampleRate := 0;
  FTotalNumOfPulses := 0;
  FCompressionType := cctRle;
end;

destructor TCswBlock.Destroy;
begin
  MS.Free;

  inherited Destroy;
end;

function TCswBlock.CountNumberOfPulses(): Integer;
var
  N: Integer;
  P0: PByte;
  Pdw0: PDWord absolute P0;
begin
  if FCountedNumOfPulses = -2 then begin
    Result := -1;
    if Assigned(MS) and (MS.Size > 0) then begin
      N := 0;
      P0 := PByte(MS.Memory);
      repeat
        if P0 > PEnd then begin
          if PEnd + 1 = P0 then begin
            Result := N;
          end;
          Break;
        end;

        Inc(N);
        if P0^ > 0 then begin
          Inc(P0);
        end else begin
          Inc(P0);
          Pdw0^ := LEtoN(Pdw0^);
          Inc(P0, 4);
        end;
      until False;
    end;

    FCountedNumOfPulses := Result;
  end else
    Result := FCountedNumOfPulses;
end;

function TCswBlock.LoadCswData(const Stream: TStream; const L: Integer
  ): Boolean;
var
  S1, S2: TMemoryStream;
begin
  FreeAndNil(MS);

  if (L > 0) and (Stream.Size - Stream.Position >= L) then begin
    S2 := TMemoryStream.Create;
    try
      S2.Size := L;
      if Stream.Read(S2.Memory^, L) = L then begin
        if FCompressionType = TCswCompressionType.cctZRle then begin
          S1 := TMemoryStream.Create;
          try
            if DecompressStream(S2, S1) then begin
              MS := S1;
              S1 := nil;
              Result := True;
            end;
          finally
            S1.Free;
          end;
        end else begin
          MS := S2;
          S2 := nil;
          Result := True;
        end;

        if Result then begin
          if MS.Size > 0 then
            MS.Position := 0
          else begin
            Result := False;
            FreeAndNil(MS);
          end;
        end;
      end;

    finally
      S2.Free;
    end;

  end;
end;

procedure TCswBlock.SetSampleRate(AValue: Integer);
begin
  FSampleRate := AValue;
  FHalfSampleRate := AValue div 2;
end;

function TCswBlock.GetBlockLength: Integer;
begin
  Result := 0;
end;

class function TCswBlock.GetBlockDescription: String;
begin
  Result := 'csw';
end;

function TCswBlock.LoadBlock(const Stream: TStream): Boolean;
var
  N: Integer;
begin
  FCountedNumOfPulses := -2;
  Result := LoadCswData(Stream, FCswDataLength);

  if Result then begin
    PEnd := PByte(MS.Memory) + (MS.Size - 1);
    N := CountNumberOfPulses;
    case FCswMajorRevisionNumber of
      1:
        TotalNumOfPulses := N;

    otherwise
      if TotalNumOfPulses <> N then
        Result := False;
    end;
  end;
end;

class function TCswBlock.GetBlockId: DWord;
begin
  Result := 0;
end;

class function TCswBlock.GetBlockIdAsString: String;
begin
  Result := '';
end;

procedure TCswBlock.Start;
begin
  inherited Start;

  if Assigned(MS) then begin
    if FTapePlayer.GetSpectrum.Is128KModel then
      FProcTicksPerSec := 3546900
    else
      FProcTicksPerSec := 3500000;
    FState := TCswPlayState.cpsStart;
    TicksNeeded := TicksNeeded.MinValue;

    P := PByte(MS.Memory);
  end else begin
    FState := TCswPlayState.cpsFinished;
    P := nil;
  end;
end;

function TCswBlock.GetNextPulse: Boolean;
var
  ProcTicks: Int64;
  TicksNeeded0: Int64;

begin
  if State = TCswPlayState.cpsFinished then
    Exit(False);

  ProcTicks := GetCurrentTotalSpectrumTicks;
  if ProcTicks >= TicksNeeded then begin
    if FState <> TCswPlayState.cpsStart then
      FTapePlayer.ActiveBit := FTapePlayer.ActiveBit xor %01000000;

    if P < PEnd then begin
      TicksNeeded0 := P^;
      Inc(P);
      if TicksNeeded0 = 0 then begin
        TicksNeeded0 := PDWord(P)^;
        Inc(P, 4);
      end;
      TicksNeeded0 := (TicksNeeded0 * FProcTicksPerSec + FHalfSampleRate) div FSampleRate;
      TicksNeeded := ProcTicks + TicksNeeded0;
      FState := TCswPlayState.cpsPlaying;
    end else begin
      FState := TCswPlayState.cpsFinished;
    end;
  end;
  Result := True;
end;

procedure TCswBlock.Details(out S: String);
begin
  inherited Details(S);

  WriteStr(S, CompressionType);
  Delete(S, 1, 3);
  if UpCase(S[1]) = 'Z' then
    Insert('-', S, 2);

  S := Format('csw ver. %d.%d', [FCswMajorRevisionNumber, FCswMinorRevisionNumber])
    + #13 + 'sample rate: ' + SampleRate.ToString
    + #13 + 'total number of pulses: ' + TotalNumOfPulses.ToString
    + #13 + 'compression type: ' + S
  ;
end;

{ TCswPlayer }

procedure TCswPlayer.CheckNextBlock();
begin
  StopPlaying;
end;

class function TCswPlayer.CheckHeader(const Stream: TStream): Boolean;
type
  TCswSignature = packed array[0..22] of AnsiChar;
const
  CCswSignature: RawByteString = 'Compressed Square Wave';
var
  S: RawByteString;
  CswSignature: TCswSignature; // "Compressed Square Wave" signature plus terminator code 0x1A

begin
  Result := False;

  if Stream.Size - Stream.Position >= SizeOf(CswSignature) then begin
    if Stream.Read(CswSignature{%H-}, SizeOf(CswSignature)) = SizeOf(CswSignature) then begin
      if CswSignature[22] = #$1A then begin
        SetLength(S{%H-}, 22);
        Move(CswSignature[0], S[1], 22);
        Result := CompareText(S, CCswSignature) = 0;
      end;
    end;
  end;
end;

class function TCswPlayer.GetNextBlockClass(const Stream: TStream
  ): TTapeBlockClass;
begin
  Result := TCswBlock1;
end;

class function TCswPlayer.GetTapeType: TTapeType;
begin
  Result := ttCsw;
end;

class function TCswPlayer.CheckIsMyClass(const Stream: TStream): Boolean;
var
  P: Int64;
begin
  Result := False;
  P := Stream.Position;
  Result := CheckHeader(Stream);
  Result := (Stream.Seek(P - Stream.Position, TSeekOrigin.soCurrent) = P) and Result;
end;

class procedure TCswPlayer.Init;
begin
  RegisterTapePlayerClass(Self);
end;

initialization
  TCswPlayer.Init;

end.

