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
    FCompressionMethodZRle: Boolean;
    FStream: TMemoryStream;
    FPulseLevelBit: Byte;
    FSampleRate: Int32;
    FPrevTicks: Int64;
    FPrePreTicks: Int64;
    FSpectrum: TSpectrum;
    FProcTicksPerSec: Int64;
    FHalfProcTicksPerSec: Int64;
    FTotalPulses: Integer;
    FPrevPulseLen: Int64;
    FNeedGoBack: Boolean;

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

    // false - RLE, true - Z-RLE
    property CompressionMethodZRle: Boolean read FCompressionMethodZRle write FCompressionMethodZRle;
  end;

implementation

{ TSaveCsw }

constructor TSaveCsw.Create;
begin
  inherited Create;

  FCompressionMethodZRle := True;
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
  if FSpectrum.Mic <> FPulseLevelBit then begin
    DoWrite;
    FPulseLevelBit := FSpectrum.Mic;
  end;
end;

procedure TSaveCsw.DoWrite();
var
  ProcTicks: Int64;
  SamplesPassed: Int64;
  B: Byte;
  Dw: DWord;

  procedure CalculateSamples(); inline;
  begin
    SamplesPassed := ((ProcTicks - FPrevTicks) * FSampleRate + FHalfProcTicksPerSec) div FProcTicksPerSec;
  end;

begin
  ProcTicks := FSpectrum.GetTotalTicks();

  CalculateSamples();
  if SamplesPassed > 0 then begin
    if FNeedGoBack then begin
      // With a lower sample rate, we could get (in theory) two (or more) pulse
      // changes within the same output sample. So, let's cope with this as good
      // as we can, and that is by rewriting previous step.
      FNeedGoBack := False; // -- reset the flag that it had happened
      if FTotalPulses > 0 then begin // skip if we had not written anything already
        Dec(FTotalPulses);
        FStream.Seek(-FPrevPulseLen, TSeekOrigin.soCurrent);
        FPrevTicks := FPrePreTicks;
        CalculateSamples();
      end;
    end;

    FPrePreTicks := FPrevTicks;
    FPrevTicks := ProcTicks;
    Inc(FTotalPulses);
    if SamplesPassed > $FF then begin
      B := 0;
      FPrevPulseLen := 5;
      FStream.Write(B, 1);
      Dw := SamplesPassed;
      Dw := NtoLE(Dw);
      FStream.Write(Dw, 4);
    end else begin
      B := SamplesPassed;
      FPrevPulseLen := 1;
      FStream.Write(B, 1);
    end;

  end else begin
    FNeedGoBack := not FNeedGoBack;
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
  FPrePreTicks := FPrevTicks;
  FPulseLevelBit := FSpectrum.Mic;
  FTotalPulses := 0;
  FPrevPulseLen := 0;
  FNeedGoBack := False;

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
    CompressionType: Byte; // 1: RLE, 2: Z-RLE
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
  Okay: Boolean;

begin
  Result := False;

  DoWrite();

  if FStream.Size > 0 then begin
    Ms := nil;
    try
      FStream.Size := FStream.Position;

      FillChar(H{%H-}, SizeOf(H), 0);

      if FCompressionMethodZRle then begin
        H.CompressionType := 2;
        Ms := TMemoryStream.Create;
        Okay := UnitStreamCompression.CompressStream(FStream, Ms);
        FreeAndNil(FStream);
      end else begin
        H.CompressionType := 1;
        Ms := FStream;
        FStream := nil;
        Okay := True;
      end;

      if Okay then begin
        Move(CCswSignature[1], H.CSWSignature[0], Length(H.CSWSignature));
        H.TerminatorCode := $1A;

        H.VerMajor := 2;
        H.VerMinor := 0;

        H.SampleRate := FSampleRate;
        H.SampleRate := NtoLE(H.SampleRate);

        H.TotalNumOfPulses := FTotalPulses;
        H.TotalNumOfPulses := NtoLE(H.TotalNumOfPulses);

        ApplicationDesc := ApplicationName + ' ' + UnitVer.TVersion.VersionString;
        // Just to make sure it's not longer than 15 characters (the last byte must be 0!):
        ApplicationDesc := Copy(ApplicationDesc, 1, Length(H.EncodingApplicationDescription) - 1);
        Move(PAnsiChar(ApplicationDesc)^, H.EncodingApplicationDescription[0], Length(ApplicationDesc));
        //
        Result := (AStream.Write(H, SizeOf(H)) = SizeOf(H))
            and (AStream.Write(Ms.Memory^, Ms.Size) = Ms.Size);
      end;

    finally
      Ms.Free;
    end;
  end;

  FreeAndNil(FStream);
end;

class function TSaveCsw.GetDefExtension: RawByteString;
begin
  Result := 'csw';
end;

end.

