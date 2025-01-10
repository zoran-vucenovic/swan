unit UnitSoundPlayer;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, ctypes, PortAudioHeader;

type

  TSoundPlayer = class sealed (TObject)
  strict private
    const
      SampleRate = 44100;

  strict private
    class var
      FLibPath: String;
      Initialized: Boolean;
      FPlaying: Boolean;
      FVolume: Integer;
      Stream: TPaStream;

    class procedure SetLibPath(const AValue: String); static;
    class function InitPortAudio(out Err: TPaError): Boolean; static;
    class function TerminatePortAudio(out Err: TPaError): Boolean; static;
    class procedure SetVolume(AValue: Integer); static;
    class procedure SetStereo(AValue: Boolean); static;
    class procedure SetBufferLen(const AValue: Integer); static;
    class function GetStereo: Boolean; static;

  private
    class var
      FBufferLen: Integer;
      PlayPosition: Integer;
      FChNum: Integer;

    class procedure Init; static;
    class procedure Final; static;

  public
    class var
      SoundBuffer: PByte;
      CurrentPosition: Integer;

  public
    class function IsLibLoaded: Boolean; static;
    class function TryLoadLib(APath: String): Boolean; static;
    class function TryLoadLib: Boolean; static;
    class function TryUnloadLib: Boolean; static;
    class function Start: String; static;
    class function Stop: String; static;
    class function StopAndTerminate: Boolean; static;

    class property LibPath: String read FLibPath write SetLibPath;
    class property BufferLen: Integer read FBufferLen write SetBufferLen;
    class property Volume: Integer read FVolume write SetVolume;
    class property Playing: Boolean read FPlaying;
    class property Stereo: Boolean read GetStereo write SetStereo;
    { ChNum is number of channels - with mono output ChNum is 1, with stereo it is 2 }
    class property ChNum: Integer read FChNum;
  end;

implementation

function PortAudioCallbackFun({%H-}Input: Pointer; Output: Pointer;
      FrameCount: culong; {%H-}TimeInfo: PPaStreamCallbackTimeInfo;
      {%H-}StatusFlags: TPaStreamCallbackFlags; {%H-}UserData: Pointer): cint; cdecl;
var
  Data: PByte;
  N, M: Integer;
  Out0: PByte absolute Output;

begin
  Data := TSoundPlayer.SoundBuffer + TSoundPlayer.PlayPosition;

  // multiply by 2 or by 4 (number of bytes used in buffer for each audio sample)
  N := FrameCount shl TSoundPlayer.FChNum;

  M := TSoundPlayer.FBufferLen - TSoundPlayer.PlayPosition;
  if N >= M then begin
    Move(Data^, Out0^, M);
    Inc(Out0, M);

    N := N - M;
    Data := TSoundPlayer.SoundBuffer;
    TSoundPlayer.PlayPosition := 0;
  end;
  Move(Data^, Out0^, N);
  Inc(TSoundPlayer.PlayPosition, N);

  Result := paContinue;
end;

{ TSoundPlayer }

class procedure TSoundPlayer.SetBufferLen(const AValue: Integer);
begin
  if AValue <> FBufferLen then begin
    Stop;

    if AValue <= 0 then begin
      FBufferLen := 0;
      if SoundBuffer <> nil then
        FreeMemAndNil(SoundBuffer);
    end else begin
      FBufferLen := AValue;
      ReAllocMem(SoundBuffer, FBufferLen);
    end;
  end;
end;

class procedure TSoundPlayer.Init;
begin
  FPlaying := False;
  Initialized := False;
  FLibPath := '';
  SoundBuffer := nil;
  FBufferLen := 0;
  FVolume := 26;
  FChNum := 1;
end;

class function TSoundPlayer.GetStereo: Boolean;
begin
  Result := FChNum = 2;
end;

class procedure TSoundPlayer.Final;
var
  Err: TPaError;
begin
  SetBufferLen(0);
  TerminatePortAudio(Err);
end;

class procedure TSoundPlayer.SetStereo(AValue: Boolean);
var
  N: Integer;
begin
  if AValue then
    N := 2
  else
    N := 1;
  if FChNum <> N then begin
    if StopAndTerminate then
      FChNum := N;
  end;
end;

class procedure TSoundPlayer.SetVolume(AValue: Integer);
begin
  if AValue > 0 then
    FVolume := AValue and 31
  else
    FVolume := 0;
end;

class procedure TSoundPlayer.SetLibPath(const AValue: String);
begin
  if AValue <> FLibPath then begin
    if TryUnloadLib() then
      FLibPath := AValue;
  end;
end;

class function TSoundPlayer.InitPortAudio(out Err: TPaError): Boolean;
begin
  Err := 0;

  if Initialized then
    Exit(True);

  Err := PortAudioHeader.Pa_Initialize();
  if Err < 0 then
    Exit(False);

  Initialized := True;
  Result := True;
end;

class function TSoundPlayer.TerminatePortAudio(out Err: TPaError): Boolean;
begin
  Err := 0;

  if not Initialized then
    Exit(True);

  if Stop <> '' then
    Exit(False);

  Err := PortAudioHeader.Pa_Terminate();
  if Err < 0 then
    Exit(False);

  Initialized := False;
  Result := True;
end;

class function TSoundPlayer.IsLibLoaded: Boolean;
begin
  Result := PortAudioHeader.IsLoaded;
end;

class function TSoundPlayer.TryLoadLib(APath: String): Boolean;
begin
  Result := PortAudioHeader.IsLoaded;
  if not Result then begin
    if PortAudioHeader.TryLoadPortAudioLib(APath) then begin
      FLibPath := APath;
      Result := True;
    end;
  end;
end;

class function TSoundPlayer.TryLoadLib: Boolean;
begin
  Result := TryLoadLib(FLibPath);
end;

class function TSoundPlayer.TryUnloadLib: Boolean;
var
  Err: TPaError;
begin
  if not TerminatePortAudio(Err) then
    Exit(False);

  PortAudioHeader.TryUnloadPortAudioLib;

  Result := not PortAudioHeader.IsLoaded;
end;

class function TSoundPlayer.Start: String;
var
  Err: TPaError;
  ErrStr: String;

  function InError(const AErr: PortAudioHeader.TPaError): Boolean;
  begin
    Err := AErr;
    if AErr >= 0 then
      Exit(False);

    Result := True;
    ErrStr := Pa_GetErrorText(AErr);
  end;

begin
  Result := '';

  if FPlaying then
    Exit;

  if not IsLibLoaded then
    Exit('NL');

  if not InitPortAudio(Err) then begin
    Result := Pa_GetErrorText(Err);
    Exit;
  end;

  CurrentPosition := 0;
  PlayPosition := 0;

  if FBufferLen <= 0 then
    Exit('Buffer size zero!');

  FillChar(SoundBuffer^, FBufferLen, #0);

  if InError(
    PortAudioHeader.Pa_OpenDefaultStream(@Stream, 0, FChNum, paInt16,
    SampleRate, {256} paFramesPerBufferUnspecified,
    @PortAudioCallbackFun, SoundBuffer)
  )
  then
    Exit(ErrStr);

  if InError(Pa_IsStreamStopped(Stream)) then
    Exit(ErrStr);

  if Err <> 1 then
    if InError(Pa_AbortStream(Stream)) then
      Exit(ErrStr);

  if InError(Pa_StartStream(Stream)) then
    Exit(ErrStr);

  FPlaying := True;
end;

class function TSoundPlayer.Stop: String;
var
  Err: TPaError;
begin
  if FPlaying then begin
    Err := Pa_AbortStream(Stream);
    if Err < 0 then begin
      Result := Pa_GetErrorText(Err);
      Exit;
    end;

    Err := Pa_CloseStream(Stream);
    if Err < 0 then begin
      Result := Pa_GetErrorText(Err);
      Exit;
    end;

    FPlaying := False;
  end;
  Result := '';
end;

class function TSoundPlayer.StopAndTerminate: Boolean;
var
  Err: TPaError;
begin
  Result := TerminatePortAudio(Err);
end;

initialization
  TSoundPlayer.Init;

finalization
  TSoundPlayer.Final;

end.

