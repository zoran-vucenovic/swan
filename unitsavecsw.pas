unit UnitSaveCSW;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitVer, UnitStreamCompression;

type
  TSaveCsw = class(TSpectrum.TAbstractTapeRecorder)
  strict private
    FStream: TStream;
    FActiveBit: Byte;
    FSampleRate: Int32;
    FPrevTicks: Int64;
    FSpectrum: TSpectrum;
    FProcTicksPerSec: Int64;
    FHalfProcTicksPerSec: Int64;
    FTotalPulses: Integer;

    procedure DoWrite();
    procedure SetSampleRate(AValue: Int32);

  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure WritePulses(); override;
    procedure SetSpectrum(ASpectrum: TSpectrum); override;
    procedure StartRecording(); override;
    function StopRecording(AStream: TStream): Boolean; override;

    class function GetDefExtension: RawByteString; override;

    property SampleRate: Int32 read FSampleRate write SetSampleRate;
  end;

implementation

{ TSaveCsw }

constructor TSaveCsw.Create;
begin
  inherited Create;

  FStream := nil;
  FSampleRate := 44100;
end;

destructor TSaveCsw.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TSaveCsw.WritePulses();
begin
  if FSpectrum.Mic <> FActiveBit then begin
    DoWrite;
    FActiveBit := FSpectrum.Mic;
  end;
end;

procedure TSaveCsw.DoWrite();
var
  ProcTicks: Int64;
  Ticks0: Int64;
  B: Byte;
  Dw: DWord;

begin
  ProcTicks := FSpectrum.GetTotalTicks();

  Ticks0 := ProcTicks - FPrevTicks;
  Ticks0 := (Ticks0 * FSampleRate + FHalfProcTicksPerSec) div FProcTicksPerSec;
  if Ticks0 > 0 then begin
    FPrevTicks := ProcTicks;
    Inc(FTotalPulses);
    if Ticks0 > $FF then begin
      B := 0;
      FStream.Write(B, 1);
      Dw := Ticks0;
      Dw := NtoLE(Dw);
      FStream.Write(Dw, 4);
    end else begin
      B := Ticks0;
      FStream.Write(B, 1);
    end;
  end;
end;

procedure TSaveCsw.SetSampleRate(AValue: Int32);
begin
  if FSampleRate <> AValue then
    FSampleRate := AValue;
end;

procedure TSaveCsw.SetSpectrum(ASpectrum: TSpectrum);
begin
  FSpectrum := ASpectrum;
  if ASpectrum = nil then begin
    FreeAndNil(FStream);
  end;
end;

procedure TSaveCsw.StartRecording;
begin
  FPrevTicks := FSpectrum.GetTotalTicks();
  FActiveBit := FSpectrum.Mic;
  FTotalPulses := 0;

  if FStream = nil then
    FStream := TMemoryStream.Create;

  if FSpectrum.Is128KModel then
    FProcTicksPerSec := 3546900
  else
    FProcTicksPerSec := 3500000;

  FHalfProcTicksPerSec := FProcTicksPerSec div 2;
end;

function TSaveCsw.StopRecording(AStream: TStream): Boolean;

type
  TCswBlockHeader2 = packed record
    CSWSignature: packed array [0..21] of AnsiChar;
    TerminatorCode: Byte;
    VerMajor: Byte;
    VerMinor: Byte;
    SampleRate: DWord;
    TotalNumOfPulses: DWord; // total number of pulses (after decompression)
    CompressionType: Byte; // 1: RLE, 2: Z-RLE (We'll save as Z-RLE)
    Flags: Byte; // bit 0 -- initial polarity, 1 - high, 0 - low (The csw specification says ignore it!)
    HDR: Byte; // Header extension length in bytes - for future expansions only, must be zero in version 2
    EncodingApplicationDescription: packed array [0..15] of AnsiChar;
  end;

const
  CCswSignature: RawByteString = 'Compressed Square Wave';

var
  ApplicationDesc: RawByteString;
  H: TCswBlockHeader2;
  Ms: TMemoryStream;

begin
  Result := False;

  DoWrite();

  Ms := TMemoryStream.Create;
  try
    if UnitStreamCompression.CompressStream(FStream, Ms) then begin
      FreeAndNil(FStream);
      ApplicationDesc := ApplicationName + ' ' + UnitVer.TVersion.VersionString;

      FillChar(H{%H-}, SizeOf(H), 0);
      Move(CCswSignature[1], H.CSWSignature[0], Length(H.CSWSignature));
      H.TerminatorCode := $1A;

      H.VerMajor := 2;
      H.VerMinor := 0;

      H.SampleRate := FSampleRate;
      H.TotalNumOfPulses := FTotalPulses;
      H.CompressionType := 2;
      //H.Flags := 0;
      //H.HDR := 0; // Reserved for future use. In csw ver. 2.0 should be set to zero.

      // Just to make sure it's not longer than 15 characters (the last byte must be 0!):
      ApplicationDesc := Copy(ApplicationDesc, 1, Length(H.EncodingApplicationDescription) - 1);
      Move(ApplicationDesc[1], H.EncodingApplicationDescription[0], Length(ApplicationDesc));

      H.SampleRate := NtoLE(H.SampleRate);
      H.TotalNumOfPulses := NtoLE(H.TotalNumOfPulses);
      //
      Result := (AStream.Write(H, SizeOf(H)) = SizeOf(H))
          and (AStream.Write(Ms.Memory^, Ms.Size) = Ms.Size);
    end;
  finally
    Ms.Free;
  end;

  FreeAndNil(FStream);
end;

class function TSaveCsw.GetDefExtension: RawByteString;
begin
  Result := 'csw';
end;

end.

