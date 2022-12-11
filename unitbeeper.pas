unit UnitBeeper;
// Copyright 2022 Zoran Vučenović
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
      InitCnt: Integer;
      FPlaying: Boolean;

    class procedure SetLibPath(const AValue: String); static;
    class function InitPortAudio(out Err: TPaError): Boolean; static;
    class function TerminatePortAudio(out Err: TPaError): Boolean; static;
    class procedure SetBeeperVolume(AValue: Int8); static;
  private
    class var
      FBufferLen: Integer;
      PlayPosition: Integer;
      FBeeperVolume: cint8;

    class procedure Init; static;
    class procedure Final; static;
  public
    class var
      BeeperBuffer: TBeeperBuffer;
      CurrentPosition: Integer;
  public
    class function IsLibLoaded: Boolean; static;
    class function StartBeeper: String; static;
    class function StopBeeper: String; static;
    class function IsPlaying: Boolean; static;

    class property LibPath: String read FLibPath write SetLibPath;
    class property BufferLen: Integer read FBufferLen write SetBufferLen;
    class property BeeperVolume: Int8 read FBeeperVolume write SetBeeperVolume;
  end;

implementation

function PortAudioCallbackFun(constref {%H-}Input: Pointer; Output: Pointer;
      FrameCount: culong; {%H-}TimeInfo: PPaStreamCallbackTimeInfo;
      {%H-}StatusFlags: TPaStreamCallbackFlags; UserData: Pointer): cint; cdecl;
var
  Data: PByte;
  Out0: pcint8;
  N: Integer;

  procedure ProcessRange; inline;
  var
    I: Integer;
  begin
    for I := 1 to N do begin
      if Data^ = 0 then
        Out0^ := 0
      else
        Out0^ := TBeeper.FBeeperVolume;
      Inc(Out0);
      Inc(Data);
    end;
  end;

begin
  Data := PByte(UserData) + TBeeper.PlayPosition;
  Out0 := pcint8(Output);
  //
  N := FrameCount;
  if TBeeper.PlayPosition + N >= TBeeper.FBufferLen then begin
    N := TBeeper.BufferLen - TBeeper.PlayPosition;
    ProcessRange;
    Data := PByte(UserData);
    N := FrameCount - N;
    TBeeper.PlayPosition := 0;
  end;
  ProcessRange;
  Inc(TBeeper.PlayPosition, N);

  Result := paContinue;
end;

{ TBeeper }

class procedure TBeeper.SetBufferLen(const AValue: Integer);
begin
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

class procedure TBeeper.Init;
begin
  FPlaying := False;
  InitCnt := 0;
  FLibPath := '';
  BeeperBuffer := nil;
  FBeeperVolume := 127 div 2;
  SetBufferLen(0);
end;

class procedure TBeeper.Final;
var
  Err: TPaError;
begin
  SetBufferLen(0);
  TerminatePortAudio(Err);
end;

class procedure TBeeper.SetBeeperVolume(AValue: Int8);
begin
  if AValue >= 0 then
    FBeeperVolume := AValue
  else
    FBeeperVolume := Integer(AValue) + 128;
end;

class procedure TBeeper.SetLibPath(const AValue: String);
var
  Err: TPaError;
begin
  if AValue <> FLibPath then begin
    if not TerminatePortAudio(Err) then
      Exit;

    PortAudioHeader.TryUnloadPortAudioLib;
    if PortAudioHeader.IsLoaded then
      Exit;

    FLibPath := AValue;
  end;
end;

class function TBeeper.InitPortAudio(out Err: TPaError): Boolean;
begin
  Err := 0;

  if InitCnt > 0 then
    Exit(True);

  Err := PortAudioHeader.Pa_Initialize();
  if Err < 0 then
    Exit(False);

  InitCnt := 1;
  Result := True;
end;

class function TBeeper.TerminatePortAudio(out Err: TPaError): Boolean;
begin
  Err := 0;

  if InitCnt <= 0 then
    Exit(True);

  StopBeeper;

  Err := PortAudioHeader.Pa_Terminate();
  if Err < 0 then
    Exit(False);

  InitCnt := 0;
  Result := True;
end;

class function TBeeper.IsLibLoaded: Boolean;
begin
  Result := PortAudioHeader.IsLoaded or TryLoadPortAudioLib(FLibPath);
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

  if InError(Pa_StartStream(Stream)) then begin
    FPlaying := False;
    Exit(ErrStr);
  end;       

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

class function TBeeper.IsPlaying: Boolean;
begin
  Result := FPlaying;
end;

initialization
  TBeeper.Init;

finalization
  TBeeper.Final;

end.

