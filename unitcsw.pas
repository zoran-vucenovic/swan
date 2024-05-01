unit UnitCSW;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

// CSW (compressed square wave) format
// The CSW specification:
//   [archived on 2022-02-20, as the original link is not available now (2024-03-24)]
// https://web.archive.org/web/20220220082010/http://ramsoft.bbk.org.omegahg.com/csw.html

interface

uses
  Classes, SysUtils, UnitTapePlayer, UnitSpectrum, UnitStreamCompression;

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

    FSampleRate: Int64;
    FHalfSampleRate: Int64;
    FTotalNumOfPulses: Integer;
    FCountedNumOfPulses: Integer;
    FCompressionType: TCswCompressionType;
    FCswDataLength: Integer;

    TicksNeeded: Int64;
    FProcTicksPerSec: Int64;

    procedure SetSampleRate(AValue: Int64);

    function LoadCswData(const Stream: TStream; const L: Integer): Boolean;

  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    destructor Destroy; override;

    function GetBlockLength: Integer; override;
    class function GetBlockDescription: String; override;
    function LoadBlock(const Stream: TStream): Boolean; override;
    class function GetBlockId: DWord; override;
    class function GetBlockIdAsString: String; override;
    procedure Start; override;
    function GetNextPulse: Boolean; override;
    procedure Details(out S: String); override;

    property State: TCswPlayState read FState;
    property SampleRate: Int64 read FSampleRate write SetSampleRate;
    property TotalNumOfPulses: Integer read FTotalNumOfPulses write FTotalNumOfPulses;
    property CompressionType: TCswCompressionType read FCompressionType write FCompressionType;
    property CswDataLength: Integer read FCswDataLength write FCswDataLength;
    property CountedNumOfPulses: Integer read FCountedNumOfPulses;
  end;

implementation

type

  TCswBlock1 = class(TCswBlock)
  strict private
    FLen: Integer;
    FEncodingApplicationDescription: AnsiString;
    FCswMajorRevisionNumber: Byte;
    FCswMinorRevisionNumber: Byte;

  public
    constructor Create(ATapePlayer: TTapePlayer); override;
    function LoadBlock(const Stream: TStream): Boolean; override;
    function GetBlockLength: Integer; override;
    procedure Details(out S: String); override;
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

{ TCswBlock1 }

constructor TCswBlock1.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);
  FLen := 0;
end;

function TCswBlock1.LoadBlock(const Stream: TStream): Boolean;
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

var
  Header1: TCswBlockHeader1;
  Header2: TCswBlockHeader2;
  VMajor, VMinor: Byte;
  L: Integer;
  N: Integer;

  Okay: Boolean;
begin
  Result := False;
  L := Stream.Size - Stream.Position;
  if L >= 2 then begin
    if (Stream.Read(VMajor{%H-}, 1) = 1) and (Stream.Read(VMinor{%H-}, 1) = 1) then begin
      FEncodingApplicationDescription := '';
      case VMajor of
        1:
          begin
            Okay := (Stream.Size - Stream.Position >= SizeOf(Header1))
              and (Stream.Read(Header1{%H-}, SizeOf(Header1)) = SizeOf(Header1));
            if Okay then begin
              Header1.SampleRate := LEtoN(Header1.SampleRate);

              SampleRate := Header1.SampleRate;
              TotalNumOfPulses := 0;
              CompressionType := cctRle;
            end;
          end;
      otherwise
        Okay := (Stream.Size - Stream.Position >= SizeOf(Header2))
              and (Stream.Read(Header2{%H-}, SizeOf(Header2)) = SizeOf(Header2))
              and ((Header2.HDR = 0) or (Stream.Size - Stream.Position >= Header2.HDR));
        if Okay then begin
          if Header2.HDR > 0 then begin
            Stream.Seek(Int64(Header2.HDR), TSeekOrigin.soCurrent);
          end;

          Header2.SampleRate := LEtoN(Header2.SampleRate);
          Header2.TotalNumOfPulses := LEtoN(Header2.TotalNumOfPulses);

          SampleRate := Header2.SampleRate;
          TotalNumOfPulses := Header2.TotalNumOfPulses;
          if Header2.CompressionType = 2 then
            CompressionType := cctZRle
          else
            CompressionType := cctRle;

          N := 0;
          while (N <= 15) and (Header2.EncodingApplicationDescription[N] <> #0) do begin
            Inc(N);
          end;

          if N > 0 then begin
            SetLength(FEncodingApplicationDescription, N);
            Move(Header2.EncodingApplicationDescription[0], FEncodingApplicationDescription[1], N);
            FEncodingApplicationDescription := Trim(FEncodingApplicationDescription);
          end;

        end;
      end;

      if Okay then begin
        FCswMajorRevisionNumber := VMajor;
        FCswMinorRevisionNumber := VMinor;
        CswDataLength := Stream.Size - Stream.Position;

        if inherited LoadBlock(Stream) then begin
          if FCswMajorRevisionNumber = 1 then
            TotalNumOfPulses := CountedNumOfPulses;
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

  S := Format('CSW ver. %d.%d', [FCswMajorRevisionNumber, FCswMinorRevisionNumber])
    + #13 + S;
  if FEncodingApplicationDescription <> '' then
    S := S + #13 + 'Encoding application description: ' + FEncodingApplicationDescription;
end;

{ TCswBlock }

constructor TCswBlock.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FCountedNumOfPulses := 0;
  MS := nil;
  P := nil;
  PEnd := nil;
  FState := cpsFinished;
  SampleRate := 0;
  FTotalNumOfPulses := 0;
  FCompressionType := cctRle;
end;

destructor TCswBlock.Destroy;
begin
  MS.Free;

  inherited Destroy;
end;

function TCswBlock.LoadCswData(const Stream: TStream; const L: Integer
  ): Boolean;
var
  S1, S2: TMemoryStream;
begin
  FreeAndNil(MS);

  if (FSampleRate > 0) and (L > 0) and (Stream.Size - Stream.Position >= L) then begin
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

procedure TCswBlock.SetSampleRate(AValue: Int64);
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
  Result := 'CSW';
end;

function TCswBlock.LoadBlock(const Stream: TStream): Boolean;

  function CountPulses(): Boolean;
  var
    N: Integer;
    P0: PByte;
    P1: PByte;
    Pdw0: PDWord absolute P0;
  begin
    Result := False;
    FCountedNumOfPulses := 0;
    if Assigned(MS) and (MS.Size > 0) then begin
      P0 := PByte(MS.Memory);
      PEnd := P0 + MS.Size;

      N := 0;
      repeat
        if P0 >= PEnd then begin
          if P0 = PEnd then begin
            FCountedNumOfPulses := N;
            Result := N > 0;
          end;
          Break;
        end;

        if P0^ > 0 then begin
          Inc(N);
          Inc(P0);
        end else begin
          Inc(P0);
          P1 := P0 + 4;
          if P1 <= PEnd then begin
            Inc(N);
            Pdw0^ := LEtoN(Pdw0^);
            P0 := P1;
          end else
            PEnd := P0;
        end;
      until False;
    end;
  end;

begin
  Result := LoadCswData(Stream, FCswDataLength)
    and CountPulses();
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

  if Assigned(MS) and (FSampleRate > 0) then begin
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
  //inherited Details(S);

  case CompressionType of
    TCswCompressionType.cctRle:
      S := 'RLE';
  otherwise
    S := 'Z-RLE';
  end;

  S := 'Sample rate: ' + SampleRate.ToString
    + #13 + 'Total number of pulses: ' + TotalNumOfPulses.ToString
    + #13 + 'Compression type: ' + S
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

