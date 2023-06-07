unit PortAudioHeader;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0
{
  Some declarations from file portaudio.h, translated from C to Free Pascal.
  http://www.portaudio.com/
  http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h_source.html

  Not all declarations from the C file are included; the purpose of this
  translation is to be used by Swan ZX Spectrum emulator for producing sound
  and declarations were added only when needed there.
}

{$mode ObjFPC}{$H+}
{$J-}
{$PackRecords C}

interface

uses
  SysUtils, dynlibs, ctypes;

const
  paContinue = 0;
  paComplete = 1;
  paAbort = 2;

  paFramesPerBufferUnspecified = 0;

  paNoError = 0;
  paMinimalErrorCode = -10000;
  paNotInitialized = paMinimalErrorCode;
  paUnanticipatedHostError = paMinimalErrorCode + 1;
  paInvalidChannelCount = paMinimalErrorCode + 2;
  paInvalidSampleRate = paMinimalErrorCode + 3;
  paInvalidDevice = paMinimalErrorCode + 4;
  paInvalidFlag = paMinimalErrorCode + 5;
  paSampleFormatNotSupported = paMinimalErrorCode + 6;
  paBadIODeviceCombination = paMinimalErrorCode + 7;
  paInsufficientMemory = paMinimalErrorCode + 8;
  paBufferTooBig = paMinimalErrorCode + 9;
  paBufferTooSmall = paMinimalErrorCode + 10;
  paNullCallback = paMinimalErrorCode + 11;
  paBadStreamPtr = paMinimalErrorCode + 12;
  paTimedOut = paMinimalErrorCode + 13;
  paInternalError = paMinimalErrorCode + 14;
  paDeviceUnavailable = paMinimalErrorCode + 15;
  paIncompatibleHostApiSpecificStreamInfo = paMinimalErrorCode + 16;
  paStreamIsStopped = paMinimalErrorCode + 17;
  paStreamIsNotStopped = paMinimalErrorCode + 18;
  paInputOverflowed = paMinimalErrorCode + 19;
  paOutputUnderflowed = paMinimalErrorCode + 20;
  paHostApiNotFound = paMinimalErrorCode + 21;
  paInvalidHostApi = paMinimalErrorCode + 22;
  paCanNotReadFromACallbackStream = paMinimalErrorCode + 23;
  paCanNotWriteToACallbackStream = paMinimalErrorCode + 24;
  paCanNotReadFromAnOutputOnlyStream = paMinimalErrorCode + 25;
  paCanNotWriteToAnInputOnlyStream = paMinimalErrorCode + 26;
  paIncompatibleStreamHostApi = paMinimalErrorCode + 27;
  paBadBufferPtr = paMinimalErrorCode + 28;

type
  TPaTime = cdouble;
  TPaStreamCallbackTimeInfo = record
    InputBufferAdcTime: TPaTime;
    CurrentTime: TPaTime;
    OutputBufferDacTime: TPaTime;
  end;
  PPaStreamCallbackTimeInfo = ^TPaStreamCallbackTimeInfo;
  TPaStreamCallbackFlags = culong;

  TPaStreamCallback =
    function(constref Input: Pointer; Output: Pointer;
      FrameCount: culong; TimeInfo: PPaStreamCallbackTimeInfo;
      StatusFlags: TPaStreamCallbackFlags; UserData: Pointer): cint; cdecl;

  TPaError = cint;

  TPaStream = Pointer;
  PPaStream = ^TPaStream;
//  PPPaStream = ^PPaStream;
  TPaSampleFormat = culong;

const
  paFloat32 = TPaSampleFormat($00000001);
  paInt32 = TPaSampleFormat($00000002);
  paInt24 = TPaSampleFormat($00000004);
  paInt16 = TPaSampleFormat($00000008);
  paInt8 = TPaSampleFormat($00000010);
  paUInt8 = TPaSampleFormat($00000020);
  paCustomFormat = TPaSampleFormat($00010000);
  paNonInterleaved = TPaSampleFormat($80000000);

var
  Pa_Initialize: function(): TPaError; cdecl;
  Pa_Terminate: function(): TPaError; cdecl;
  Pa_OpenDefaultStream: function(Stream: PPaStream; NumInputChanels: cint;
    NumOutputChanels: cint; SampleFormat: TPaSampleFormat; SampleRate: cdouble;
    FramesPerBuffer: culong; StreamCallback: TPaStreamCallback; UserData: Pointer): TPaError; cdecl;
  Pa_StartStream: function(Stream: TPaStream): TPaError; cdecl;
  Pa_StopStream: function(Stream: TPaStream): TPaError; cdecl;
  Pa_AbortStream: function(Stream: TPaStream): TPaError; cdecl;
  Pa_CloseStream: function(Stream: TPaStream): TPaError; cdecl;
  Pa_IsStreamStopped: function(Stream: TPaStream): TPaError; cdecl;
  Pa_IsStreamActive: function(Stream: TPaStream): TPaError; cdecl;
  Pa_GetErrorText: function(ErrorCode: TPaError): PAnsiChar; cdecl;
  Pa_Sleep: procedure(MSec: clong); cdecl;

function TryLoadPortAudioLib(LibraryPath: String): Boolean;
procedure TryUnloadPortAudioLib;
function IsLoaded: Boolean;

implementation

var
  LoadCount: Int64;
  LibHandle: TLibHandle;

function EmptyInitializeTerminateFun(): TPaError; cdecl;
begin
  Result := 0;
end;

{$push}
{$warn 5024 off : Parameter "$1" not used}
function EmptyOpenDefaultStreamFun(
  Stream: PPaStream; NumInputChanels: cint;
  NumOutputChanels: cint; SampleFormat: TPaSampleFormat; SampleRate: cdouble;
  FramesPerBuffer: culong; StreamCallback: TPaStreamCallback; UserData: Pointer
  ): TPaError; cdecl;
begin
  Result := 0;
end;

function EmptyStartStopAbortClose(Stream: TPaStream): TPaError; cdecl;
begin
  Result := 0;
end;

var
  EmptyStr: AnsiString;

function EmptyGetErrorText(ErrorCode: TPaError): PAnsiChar; cdecl;
begin
  Result := PAnsiChar(EmptyStr);
end;

procedure EmptySleepProc(MSec: clong); cdecl;
begin
  //
end;
{$pop}

procedure SetToEmptyProcedures;
begin
  Pa_Initialize := @EmptyInitializeTerminateFun;
  Pa_Terminate := @EmptyInitializeTerminateFun;
  Pa_OpenDefaultStream := @EmptyOpenDefaultStreamFun;
  Pa_StartStream := @EmptyStartStopAbortClose;
  Pa_StopStream := @EmptyStartStopAbortClose;
  Pa_AbortStream := @EmptyStartStopAbortClose;
  Pa_CloseStream := @EmptyStartStopAbortClose;
  Pa_IsStreamStopped := @EmptyStartStopAbortClose;
  Pa_IsStreamActive := @EmptyStartStopAbortClose;
  Pa_GetErrorText := @EmptyGetErrorText;
  Pa_Sleep := @EmptySleepProc;
end;

function TryLoadPortAudioLib(LibraryPath: String): Boolean;
const
  DefaultLibraryName =
  {$if defined(mswindows)}
    'portaudio.dll';
  {$elseif defined(darwin)}
    'libportaudio.dylib';
  {$elseif defined(unix)}
    'libportaudio.so.2';
  {$else}
    {$fatal OS not supported!}
  {$endif}
                    
{$ifdef mswindows}
var
  Ext: String;
  LibAux: String;

  procedure TryWithBitness(S: String); //inline;
  begin
    S := S + IntToStr(SizeOf(Pointer) * 8);
    LibHandle := SafeLoadLibrary(S + Ext);
    if LibHandle = NilHandle then begin
      S := 'lib' + S;
      LibHandle := SafeLoadLibrary(S + Ext);
      if LibHandle = NilHandle then
        LibHandle := SafeLoadLibrary(S + 'bit' + Ext);
    end;
  end;
{$endif}

begin
  if LoadCount = 0 then begin   
    Result := False;
    if Trim(LibraryPath) = '' then begin
      LibHandle := SafeLoadLibrary(DefaultLibraryName);
      {$ifdef mswindows}
      if LibHandle = NilHandle then begin
        LibAux := ExtractFileName(DefaultLibraryName);
        Ext := ExtractFileExt(LibAux);
        LibAux := Copy(LibAux, 1, Length(LibAux) - Length(Ext));
        TryWithBitness(LibAux);
        if LibHandle = NilHandle then begin
          TryWithBitness(LibAux + '-');
          if LibHandle = NilHandle then
            TryWithBitness(LibAux + '_');
        end;
      end;
      {$endif}
    end else
      LibHandle := SafeLoadLibrary(LibraryPath);

    if LibHandle = NilHandle then begin
      SetToEmptyProcedures;
      Exit;
    end;

    Pointer(Pa_Initialize) := GetProcAddress(LibHandle, 'Pa_Initialize');
    Pointer(Pa_Terminate) := GetProcAddress(LibHandle, 'Pa_Terminate');
    Pointer(Pa_OpenDefaultStream) := GetProcAddress(LibHandle, 'Pa_OpenDefaultStream');
    Pointer(Pa_StartStream) := GetProcAddress(LibHandle, 'Pa_StartStream');
    Pointer(Pa_StopStream) := GetProcAddress(LibHandle, 'Pa_StopStream');
    Pointer(Pa_AbortStream) := GetProcAddress(LibHandle, 'Pa_AbortStream');
    Pointer(Pa_CloseStream) := GetProcAddress(LibHandle, 'Pa_CloseStream');
    Pointer(Pa_IsStreamStopped) := GetProcAddress(LibHandle, 'Pa_IsStreamStopped');
    Pointer(Pa_IsStreamActive) := GetProcAddress(LibHandle, 'Pa_IsStreamActive');
    Pointer(Pa_GetErrorText) := GetProcAddress(LibHandle, 'Pa_GetErrorText');
    Pointer(Pa_Sleep) := GetProcAddress(LibHandle, 'Pa_Sleep');

    if not (Assigned(Pa_Initialize) and Assigned(Pa_Terminate) and Assigned(Pa_OpenDefaultStream)
      and Assigned(Pa_StartStream) and Assigned(Pa_StopStream) and Assigned(Pa_AbortStream)
      and Assigned(Pa_CloseStream) and Assigned(Pa_IsStreamStopped) and Assigned(Pa_IsStreamActive)
      and Assigned(Pa_GetErrorText) and Assigned(Pa_Sleep)
      )
    then begin
      LoadCount := 1;
      TryUnloadPortAudioLib;
      Exit;
    end;
  end;
  LoadCount := LoadCount + 1;
  Result := True;
end;

procedure TryUnloadPortAudioLib;
begin
  if LoadCount > 0 then begin
    if LoadCount = 1 then begin
      SetToEmptyProcedures;
      if not UnloadLibrary(LibHandle) then
        Exit;
      LibHandle := NilHandle;
    end;
    LoadCount := LoadCount - 1;
  end;
end;

function IsLoaded: Boolean;
begin
  Result := LibHandle <> NilHandle;
end;

procedure Init;
begin
  EmptyStr := '';
  LibHandle := NilHandle;
  LoadCount := 0;
  SetToEmptyProcedures;
end;

procedure Final;
begin
  if LoadCount > 0 then begin
    LoadCount := 1;
    TryUnloadPortAudioLib;
  end;
end;

initialization
  Init;

finalization
  Final;

end.

