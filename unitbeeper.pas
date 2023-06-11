unit UnitBeeper;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, ctypes, PortAudioHeader;

type
  TBeeperBuffer = PByte;

  TBeeper = class sealed (TObject)
  strict private
    class var
      Stream: TPaStream;

    class procedure SetBufferLen(const AValue: Integer); static;
  strict private
    const
      SampleRate = 44100;
  strict private
    class var
      FLibPath: String;
      Initialized: Boolean;
      FPlaying: Boolean;
      FBeeperVolume: Integer;

    class procedure SetLibPath(const AValue: String); static;
    class function InitPortAudio(out Err: TPaError): Boolean; static;
    class function TerminatePortAudio(out Err: TPaError): Boolean; static;
    class procedure SetBeeperVolume(AValue: Integer); static;
  private
    class var
      FBufferLen: Integer;
      PlayPosition: Integer;

    class procedure Init; static;
    class procedure Final; static;
  public
    class var
      BeeperBuffer: TBeeperBuffer;
      CurrentPosition: Integer;
  public
    class function IsLibLoaded: Boolean; static;
    class function TryLoadLib(APath: String): Boolean; static;
    class function TryLoadLib: Boolean; static;
    class function TryUnloadLib: Boolean; static;
    class function StartBeeper: String; static;
    class function StopBeeper: String; static;
    class function StopAndTerminate: Boolean; static;

    class property LibPath: String read FLibPath write SetLibPath;
    class property BufferLen: Integer read FBufferLen write SetBufferLen;
    class property BeeperVolume: Integer read FBeeperVolume write SetBeeperVolume;
    class property Playing: Boolean read FPlaying;
  end;

implementation

function PortAudioCallbackFun(constref {%H-}Input: Pointer; Output: Pointer;
      FrameCount: culong; {%H-}TimeInfo: PPaStreamCallbackTimeInfo;
      {%H-}StatusFlags: TPaStreamCallbackFlags; UserData: Pointer): cint; cdecl;
var
  Data: PByte;
  N: Integer;
  Out0: PByte absolute Output;

begin
  Data := PByte(UserData) + TBeeper.PlayPosition;
  //
  N := FrameCount;
  if TBeeper.PlayPosition + N >= TBeeper.FBufferLen then begin
    N := TBeeper.BufferLen - TBeeper.PlayPosition;
    Move(Data^, Out0^, N);
    Inc(Out0, N);
    Data := PByte(UserData);
    N := FrameCount - N;
    TBeeper.PlayPosition := 0;
  end;
  Move(Data^, Out0^, N);
  Inc(TBeeper.PlayPosition, N);

  Result := paContinue;
end;

{ TBeeper }

class procedure TBeeper.SetBufferLen(const AValue: Integer);
begin
  if AValue <> FBufferLen then begin
    StopBeeper;

    if AValue <= 0 then begin
      FBufferLen := 0;
      if BeeperBuffer <> nil then
        FreeMemAndNil(BeeperBuffer);
    end else begin
      FBufferLen := AValue;
      ReAllocMem(BeeperBuffer, FBufferLen);
    end;
  end;
end;

class procedure TBeeper.Init;
begin
  FPlaying := False;
  Initialized := False;
  FLibPath := '';
  BeeperBuffer := nil;
  FBufferLen := 0;
  FBeeperVolume := 127 div 2;
end;

class procedure TBeeper.Final;
var
  Err: TPaError;
begin
  SetBufferLen(0);
  TerminatePortAudio(Err);
end;

class procedure TBeeper.SetBeeperVolume(AValue: Integer);
begin
  if AValue > 0 then
    FBeeperVolume := AValue and 127
  else
    FBeeperVolume := 0;
end;

class procedure TBeeper.SetLibPath(const AValue: String);
begin
  if AValue <> FLibPath then begin
    if TryUnloadLib() then
      FLibPath := AValue;
  end;
end;

class function TBeeper.InitPortAudio(out Err: TPaError): Boolean;
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

class function TBeeper.TerminatePortAudio(out Err: TPaError): Boolean;
begin
  Err := 0;

  if not Initialized then
    Exit(True);

  if StopBeeper <> '' then
    Exit(False);

  Err := PortAudioHeader.Pa_Terminate();
  if Err < 0 then
    Exit(False);

  Initialized := False;
  Result := True;
end;

class function TBeeper.IsLibLoaded: Boolean;
begin
  Result := PortAudioHeader.IsLoaded;
end;

class function TBeeper.TryLoadLib(APath: String): Boolean;
begin
  Result := PortAudioHeader.IsLoaded;
  if not Result then begin
    if PortAudioHeader.TryLoadPortAudioLib(APath) then begin
      FLibPath := APath;
      Result := True;
    end;
  end;
end;

class function TBeeper.TryLoadLib: Boolean;
begin
  Result := TryLoadLib(FLibPath);
end;

class function TBeeper.TryUnloadLib: Boolean;
var
  Err: TPaError;
begin
  if not TerminatePortAudio(Err) then
    Exit(False);

  PortAudioHeader.TryUnloadPortAudioLib;

  Result := not PortAudioHeader.IsLoaded;
end;

class function TBeeper.StartBeeper: String;
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

  FillChar(BeeperBuffer^, FBufferLen, #0);

  if InError(
    PortAudioHeader.Pa_OpenDefaultStream(@Stream, 0, 1, paInt8,
    SampleRate, {256} paFramesPerBufferUnspecified,
    @PortAudioCallbackFun, BeeperBuffer)
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

class function TBeeper.StopBeeper: String;
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

class function TBeeper.StopAndTerminate: Boolean;
var
  Err: TPaError;
begin
  Result := TerminatePortAudio(Err);
end;

initialization
  TBeeper.Init;

finalization
  TBeeper.Final;

end.

