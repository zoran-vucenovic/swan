unit UnitSpectrum;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, BGRABitmapTypes, UnitMemory, Z80Processor,
  UnitSpectrumKeyboard, FastIntegers, UnitSpectrumColourMap,
  UnitSpectrumColoursBGRA, UnitJoystick, UnitSoundPlayer, SoundChipAY_3_8912,
  Graphics, LCLType, Forms;

const
  TopBorder = 36;
  BottomBorder = TopBorder;
  LeftBorder = 48;
  RightBorder = LeftBorder;

  CentralScreenHeight = 192;
  CentralScreenWidth = 256;

  WholeScreenHeight = TopBorder + CentralScreenHeight + BottomBorder; // 264
  WholeScreenWidth = LeftBorder + CentralScreenWidth + RightBorder; // 352

type
  TSpectrumModel = (
    smNone, sm16K_issue_2, sm16K_issue_3, sm48K_issue_2, sm48K_issue_3
      , sm128K, smPlus2, smPlus2a, smPlus3 // one day... maybe.
    );

  TSpectrumColour = Integer;
  TSpectrumColours = array [False..True, 0..7] of TSpectrumColour;

  { TSpectrum }

  TSpectrum = class(TThread)
  public type

    IDebugger = interface ['{5B8746EF-84D0-4549-98FE-AE2AE4687912}']
      procedure SetSpectrum(Spectrum: TSpectrum);
      procedure OnStep(out DoContinue: Boolean);
      procedure AfterStep;
      function CheckBreakPoints: Boolean; // should return true when breakpoint is reached.
    end;

    TAbstractTapePlayer = class abstract (TObject)
    public
      InPause: Boolean;
      procedure GetNextPulse(); virtual; abstract;
    end;

  public const
    NormalSpeed = 512;
    MaxSpeed = NormalSpeed div 16;
    MinSpeed = NormalSpeed * 8;

  strict private
    procedure SetPaging; inline;
    // processor events
    procedure ProcessorInput;
    procedure ProcessorOutput;

  strict private
    CentralScreenStart: Int32Fast;
    TicksPerScanLine: Int32Fast;
    ScreenStart: Int32Fast;
    ScreenEnd: Int32Fast;
    HoldInterruptPinTicks: Int16Fast;

    FloatBusFirstInterestingTick: Int32Fast;
    FloatBusLastInterestingTick: Int32Fast;

    FIs128KModel: Boolean;
    FModelWithHALbug: Boolean;
    FOnSync: TThreadMethod;
    FPaused: Boolean;
    FSpectrumModel: TSpectrumModel;
    FBkpSpectrumModel: TSpectrumModel;
    FIssue2Keyboard: Boolean;
    FProcessor: TProcessor;
    FMemory: TMemory;
    FDebugger: IDebugger;
    FTapePlayer: TAbstractTapePlayer;
    FAYSoundChip: TSoundAY_3_8912;

    // screen processing
    FCodedBorderColour: Byte;
    FLCLColours: TLCLColourMap;
    FFlashState: Int16Fast;
    TicksFrom: Int32Fast;
    FSumTicks: Int64;
    MicrosecondsNeeded: Int64;

    PCTicksStart: Int64;

    FSpeed: Integer;
    SpeedCorrection: Int16Fast;
    AskForSpeedCorrection: Boolean;
    FRunning: Boolean;
    FKeepRunning: Boolean;
    FEar: Byte;
    FInternalEar: Byte;
    FEarFromTape: Byte;
    FMic: Byte;
    FOnEndRun: TThreadMethod;
    StepInDebugger: Boolean;
    FFrameCount: Int64;
    FIntPinUpCount: Int16Fast;
    FLatestTickUpdatedSoundBuffer: Int64;
    FRestoringSpectrumModel: Boolean;

    FSoundPlayerRatePortAudio: Int64;
    FSoundPlayerRateProcessor: Int64;
    FDivVol: Single;

    procedure StopSoundPlayer; inline;
    procedure UpdateSoundBuffer; inline;
    procedure UpdateAskForSpeedCorrection;
    procedure SetPaused(AValue: Boolean);
    procedure WriteToScreen(TicksTo: Int32Fast);
    procedure InitTimes; inline;
    procedure AdjustSpectrumColours;
    procedure SetCodedBorderColour(AValue: Byte); inline;
    procedure SetOnSync(AValue: TThreadMethod);
    procedure SetOnEndRun(AValue: TThreadMethod);
    procedure SetSpeed(AValue: Integer);
    procedure DoSync; inline;
    procedure CheckDebuggerStep;
    procedure AfterDebuggerStep;
    procedure DoStep; inline;
    procedure RunSpectrum;

  strict private
    FInLoadingSnapshot: Boolean;
    FOnChangeModel: TThreadMethod;
    FOnResetSpectrum: TThreadMethod;
    FOnStartRun: TThreadMethod;
    FSoundMuted: Boolean;
    SpectrumColoursBGRA: TSpectrumColoursBGRA;
    CustomRomsMounted: Boolean;
    FCustomRomsFileNames: TStringDynArray;
    FLateTimings: Boolean;

    function GetFlashState: UInt16;
    function GetRemainingIntPinUp: Integer;
    procedure SetFlashState(AValue: UInt16);
    procedure SetRemainingIntPinUp(AValue: Integer);
    procedure SetSoundMuted(AValue: Boolean);
    procedure UpdateDebuggedOrPaused;
    procedure SetInternalEar(AValue: Byte);
    procedure SetLateTimings(AValue: Boolean);

  strict private
    // 128 K
    FPagingEnabled: Boolean;

  protected
    procedure Execute; override;

  public
    KeyBoard: TSpectrumKeyBoard;

    constructor Create;
    destructor Destroy; override;

    class function DefaultSpectrumModel: TSpectrumModel; static;

    procedure SetSpectrumModel(ASpectrumModel: TSpectrumModel; ACustomRoms: TStream);
    procedure AttachDebugger(ADebugger: IDebugger);
    procedure DettachDebugger;
    procedure SetTapePlayer(ATapePlayer: TAbstractTapePlayer);
    procedure SetSpectrumColours(const Colours: TLCLColourMap);
    procedure GetSpectrumColours(out Colours: TLCLColourMap);
    procedure SetWriteScreen(const AValue: Boolean);
    procedure SetEarFromTape(AValue: Byte);
    procedure CheckStartSoundPlayer;
    procedure StepToInstructionEndIfNeeded;

    procedure StopRunning;
    procedure ResetSpectrum;
    function IsRunning: Boolean;
    function GetProcessor: TProcessor;
    function GetFrameCount: Int64;
    procedure DrawToCanvas(const ACanvas: TCanvas; const R: TRect); inline;
    function GetBgraColours: TBGRAColours;
    function IsIssue2: Boolean;
    function GetTotalTicks(): Int64;
    function GetCustomRomFiles(out ARomFiles: TStringDynArray): Integer;
    procedure SetCustomRomFiles(const ARomFiles: TStringDynArray);

    property RemainingIntPinUp: Integer // for szx file
      read GetRemainingIntPinUp write SetRemainingIntPinUp;
    property LateTimings: Boolean read FLateTimings write SetLateTimings;
    property SumTicks: Int64 read FSumTicks;
    property CodedBorderColour: Byte read FCodedBorderColour write SetCodedBorderColour;
    property Speed: Integer read FSpeed write SetSpeed;
    property OnSync: TThreadMethod read FOnSync write SetOnSync;
    property OnStartRun: TThreadMethod read FOnStartRun write FOnStartRun;
    property OnEndRun: TThreadMethod read FOnEndRun write SetOnEndRun;
    property OnResetSpectrum: TThreadMethod read FOnResetSpectrum write FOnResetSpectrum;
    property OnChangeModel: TThreadMethod read FOnChangeModel write FOnChangeModel;
    property Paused: Boolean read FPaused write SetPaused;
    property SoundMuted: Boolean read FSoundMuted write SetSoundMuted;
    property InternalEar: Byte read FInternalEar write SetInternalEar;
    property FlashState: UInt16 read GetFlashState write SetFlashState;
    property SpectrumModel: TSpectrumModel read FSpectrumModel;
    property BkpSpectrumModel: TSpectrumModel read FBkpSpectrumModel write FBkpSpectrumModel;
    property InLoadingSnapshot: Boolean read FInLoadingSnapshot write FInLoadingSnapshot;
    property Is128KModel: Boolean read FIs128KModel;
    property Memory: TMemory read FMemory;
    property AYSoundChip: TSoundAY_3_8912 read FAYSoundChip;
    property IsPagingEnabled: Boolean read FPagingEnabled write FPagingEnabled;
  end;

implementation

{ TSpectrum }

procedure TSpectrum.UpdateSoundBuffer;
var
  N, M: Integer;
  P: PByte;
  F: Single;
  BB: DWord absolute F;

begin
  if TSoundPlayer.Playing then begin
    N := ((FSumTicks + FProcessor.TStatesInCurrentFrame) * FSoundPlayerRatePortAudio + FSoundPlayerRateProcessor shr 1 - FLatestTickUpdatedSoundBuffer) div FSoundPlayerRateProcessor;
    if N > 0 then begin
      FLatestTickUpdatedSoundBuffer := FLatestTickUpdatedSoundBuffer + N * FSoundPlayerRateProcessor;

      F := FEar + FMic;
      F := F * TSoundPlayer.Volume / FDivVol;

      P := TSoundPlayer.SoundBuffer + TSoundPlayer.CurrentPosition;
      M := TSoundPlayer.BufferLen - TSoundPlayer.CurrentPosition;

      if N * 4 >= M then begin
        M := M div 4;
        if Assigned(FAYSoundChip) then
          FAYSoundChip.Fill(F, PSingle(P), M)
        else
          FillDWord(P^, M, BB);

        N := N - M;
        TSoundPlayer.CurrentPosition := 0;
        P := TSoundPlayer.SoundBuffer;
      end;

      if Assigned(FAYSoundChip) then
        FAYSoundChip.Fill(F, PSingle(P), N)
      else
        FillDWord(P^, N, BB);

      TSoundPlayer.CurrentPosition := TSoundPlayer.CurrentPosition + N * 4;
    end;
  end;
end;

procedure TSpectrum.StopSoundPlayer;
begin
  //TSoundPlayer.Stop;
  // Terminate portaudio! After stopping without terminating portaudio, and then
  // restarting, the sound seems to make bigger and bigger delay.
  TSoundPlayer.StopAndTerminate;
end;

procedure TSpectrum.InitTimes;
begin
  MicrosecondsNeeded := 500; // start with half of millisecond
  PCTicksStart := SysUtils.GetTickCount64;
end;

procedure TSpectrum.SetCodedBorderColour(AValue: Byte);
begin
  if AValue <> FCodedBorderColour then begin
    FCodedBorderColour := AValue;
    SpectrumColoursBGRA.BorderColour2 := SpectrumColoursBGRA.BGRAColours[False, AValue];
  end;
end;

procedure TSpectrum.AdjustSpectrumColours;
var
  Bright: Boolean;
  C: 0..7;
begin
  for Bright := False to True do
    for C := 0 to 7 do begin
      SpectrumColoursBGRA.BGRAColours[Bright, C].FromColor(FLCLColours.Colours[Bright, C]);
    end;

  FCodedBorderColour := FCodedBorderColour or $F0;
  SetCodedBorderColour(FCodedBorderColour and %111);
end;

procedure TSpectrum.SetOnSync(AValue: TThreadMethod);
begin
  if FOnSync <> AValue then
    FOnSync := AValue;
end;

// Note that 512 means 100% speed.
procedure TSpectrum.SetSpeed(AValue: Integer);
begin
  if FSpeed <> AValue then begin
    FSpeed := AValue;
    StopSoundPlayer;

    if FIs128KModel then begin
      // For 1.000.000 microseconds, passes 3.546.900 ticks
      // How many microseconds it actually takes for 70908 ticks?
      // 1000000 / 3546900 = x / 70908
      // x = 70908 * 10000 / 35469
      //
      //SpeedCorrection is (70908 * 10000) / (35469 * 512);
      // that is:
      SpeedCorrection := MulDiv(3693125, FSpeed, 94584);
    end else begin
      // For 20 milliseconds (20.000 microseconds) passes 70.000 ticks.
      // How many microseconds it actually takes for 69888 ticks?
      // 20000 / 70000 = x / 69888
      // x = 69888 * 2 / 7
      // x = 19968
      // So:
      //SpeedCorrection := MulDiv(19968, FSpeed, 512);
      // that is:
      SpeedCorrection := 39 * FSpeed;
    end;

    UpdateAskForSpeedCorrection;
    CheckStartSoundPlayer;
  end;
end;

procedure TSpectrum.DoSync;
begin
  if Assigned(FOnSync) then
    Synchronize(FOnSync);
end;

procedure TSpectrum.CheckDebuggerStep;
begin
  if Assigned(FDebugger) then
    FDebugger.OnStep(StepInDebugger)
  else
    StepInDebugger := False;
end;

procedure TSpectrum.AfterDebuggerStep;
begin
  if Assigned(FDebugger) then
    FDebugger.AfterStep;
end;

procedure TSpectrum.SetPaging;
begin
  if FPagingEnabled then begin
    FMemory.ActiveRamPageNo := FProcessor.DataBus and %111;
    FProcessor.ContendedHighBank := FProcessor.DataBus and 1 <> 0; { #todo : different on +3! }
    FMemory.ShadowScreenDisplay := FProcessor.DataBus and %1000 <> 0;
    FMemory.ActiveRomPageNo := (FProcessor.DataBus shr 4) and 1;
    if FProcessor.DataBus and %100000 <> 0 then
      FPagingEnabled := False;
  end;
end;

procedure TSpectrum.ProcessorInput;

  procedure CheckFloatingBus;
  var
    N, X, Y: Integer;
    W: Word;
    WR: WordRec absolute W;
  begin
    if (FProcessor.TStatesInCurrentFrame <= FloatBusLastInterestingTick)
        and (FProcessor.TStatesInCurrentFrame >= FloatBusFirstInterestingTick)
    then begin
      //
      N := FProcessor.TStatesInCurrentFrame - FloatBusFirstInterestingTick;
      X := N mod TicksPerScanLine;
      if X and $84 = 0 then begin //if (X <= 127) and (X mod 8 <= 3) then begin
        Y := N div TicksPerScanLine;
        if X and 1 = 0 then
          WR.Hi := ((Y shr 3) and %00011000) or (Y and %111)
        else
          WR.Hi := %00011000 or ((Y shr 6) and %11);
        WR.Lo := (((Y shl 2) and %11100000) or (X shr 2)) + ((X shr 1) and 1);
        FProcessor.DataBus := FMemory.ReadScreenByte(W);
      end;
    end;
  end;

var
  Ba, B: Byte;
  I: Integer;
begin
  // read port from address bus,
  // get byte from device connected to that port
  // put that byte on data bus

//  if FProcessor.AddressBus and $FF = $FE then begin
  if FProcessor.AddressBus and 1 = 0 then begin
    // read keyboard...
    B := %00011111;

    Ba := WordRec(FProcessor.AddressBus).Hi;
    for I := Low(KeyBoard.HalfRows) to High(KeyBoard.HalfRows) do
      if (Ba shr I) and 1 = 0 then
        B := B and KeyBoard.HalfRows[I];

    if Assigned(FTapePlayer) then
      FTapePlayer.GetNextPulse();
    if FIssue2Keyboard and ((FTapePlayer = nil) or FTapePlayer.InPause) then
      B := B or ((not FMic shl 3) and %01000000);

    FProcessor.DataBus :=
      B // keyboard can set LOW bits 0-4
      or FEar // and bit 6 is read from ear socket
      or %10100000
      ;
    //
  end else begin
    if TJoystick.Joystick.Enabled and
      (
        ((FProcessor.AddressBus and $FF = $1F) and (TJoystick.Joystick.JoystickType = TJoystick.TJoystickType.jtKempston))
          or ((FProcessor.AddressBus and $FF = $7F) and (TJoystick.Joystick.JoystickType = TJoystick.TJoystickType.jtFuller))
      )
    then begin
      FProcessor.DataBus := TJoystick.Joystick.State;
    end else if Assigned(FAYSoundChip)
        and (FProcessor.AddressBus and $C002 = $C000)
    then begin
      FProcessor.DataBus := FAYSoundChip.GetRegValue();
    end else begin
      FProcessor.DataBus := $FF;
      CheckFloatingBus;

      // HAL bug (https://sinclair.wiki.zxnet.co.uk/wiki/ZX_Spectrum_128#HAL_bugs):
      // "Reads from port 0x7ffd cause a crash, as the 128's HAL10H8 chip
      // does not distinguish between reads and writes to this port, resulting
      // in a floating data bus being used to set the paging registers."
      // Test: "print in 32765" from 128K basic should crash Spectrum
      //   (see https://foro.speccy.org/viewtopic.php?t=2374)
      // and FloatFFD test https://github.com/redcode/ZXSpectrum/wiki/FloatFFD
      if FModelWithHALbug then
        if FProcessor.AddressBus and $8002 = 0 then
          SetPaging;
    end;
  end;
end;

procedure TSpectrum.ProcessorOutput;
var
  Aux: Byte;
begin
  // read port from address bus
  // read byte from data bus
  // write that byte to device connected to the port
  //if FProcessor.AddressBus and $FF = $FE then begin
  if FProcessor.AddressBus and 1 = 0 then begin
    Aux := FProcessor.DataBus;
    SetCodedBorderColour(Aux and %111);

    // sound... byte
    // 1 in bit 4 activates EAR, whilst 0 in bit 3 activates MIC
    // Implemented so that EAR state can be read by IN, whereas MIC not, but
    // MIC is (to lesser extent) taken into account when playing sound.
    UpdateSoundBuffer;
    FInternalEar := (Aux and %10000) shl 2;
    FMic := (not Aux) and %1000;
    FEar := FInternalEar or FEarFromTape;
  end;

  if FProcessor.AddressBus and $8002 = 0 then begin
    SetPaging;
  end else begin
    if Assigned(FAYSoundChip) then begin
      case FProcessor.AddressBus and $C002 of
        $C000:
          FAYSoundChip.SetActiveRegNum(FProcessor.DataBus);
        $8000:
          begin
            UpdateSoundBuffer;
            FAYSoundChip.SetRegValue(FProcessor.DataBus);
            // calculate new values for output
          end;
      otherwise
      end;
    end;
  end;

end;

function TSpectrum.GetTotalTicks(): Int64;
begin
  Result := FSumTicks + FProcessor.TStatesInCurrentFrame;
end;

function TSpectrum.GetCustomRomFiles(out ARomFiles: TStringDynArray): Integer;
var
  I: Integer;
begin
  Result := Length(FCustomRomsFileNames);
  SetLength(ARomFiles{%H-}, Result);
  for I := Low(FCustomRomsFileNames) to High(FCustomRomsFileNames) do
    ARomFiles[I] := FCustomRomsFileNames[I];
end;

procedure TSpectrum.SetCustomRomFiles(const ARomFiles: TStringDynArray);
var
  I: Integer;
begin
  if Length(ARomFiles) <= 4 then begin
    SetLength(FCustomRomsFileNames, Length(ARomFiles));
    for I := Low(ARomFiles) to High(ARomFiles) do
      FCustomRomsFileNames[I] := ARomFiles[I];
  end;
end;

procedure TSpectrum.CheckStartSoundPlayer;
begin
  if (not FSoundMuted) and AskForSpeedCorrection and FRunning and (FSpeed = NormalSpeed)
      and TSoundPlayer.IsLibLoaded and (not TSoundPlayer.Playing)
  then begin
    FLatestTickUpdatedSoundBuffer := (FSumTicks + FProcessor.TStatesInCurrentFrame) * FSoundPlayerRatePortAudio;
    TSoundPlayer.Start;
  end;
end;

{ This is used when saving snapshots to file - if cpu is in the middle of
  prefixed instruction, finish it before saving the snapshot }
procedure TSpectrum.StepToInstructionEndIfNeeded;
begin
  case FProcessor.PrefixByte of
    $CB, $ED:
      DoStep;
    $DD, $FD:
      begin
        case FMemory.ReadByte(FProcessor.RegPC) of
          $DD, $ED, $FD:
            // Followed by another prefix, this is NONI, so don't step further.
            // Szx format (but not z80 or sna) will save that the interrupts
            // are disabled in the next step.
            ;
        otherwise
          DoStep;
        end;
      end;
  otherwise
  end;
end;

procedure TSpectrum.UpdateAskForSpeedCorrection;
begin
  if AskForSpeedCorrection xor
      (FKeepRunning and (SpeedCorrection <> 0)
      and (FProcessor.OnNeedWriteScreen = @WriteToScreen))
  then begin
    KeyBoard.ClearKeyboard;
    TJoystick.Joystick.ResetState;

    AskForSpeedCorrection := not AskForSpeedCorrection;
    if AskForSpeedCorrection then begin
      InitTimes;
    end;
  end;
end;

procedure TSpectrum.SetOnEndRun(AValue: TThreadMethod);
begin
  if FOnEndRun <> AValue then
    FOnEndRun := AValue;
end;

procedure TSpectrum.Execute;
begin
  RunSpectrum;
end;

constructor TSpectrum.Create;
var
  Colours: TLCLColourMap;
begin
  inherited Create(True);

  FreeOnTerminate := False;

  SetLength(FCustomRomsFileNames, 0);
  FPagingEnabled := False;
  FSoundMuted := False;
  FDivVol := 1.0;

  FDebugger := nil;
  FTapePlayer := nil;

  CustomRomsMounted := False;
  FIs128KModel := False;

  FMemory := TMemory.Create;
  FProcessor := TProcessor.Create;
  FProcessor.SetMemory(FMemory);

  FLateTimings := False;
  FSpectrumModel := smNone;
  FBkpSpectrumModel := smNone;
  FIssue2Keyboard := False;
  FOnChangeModel := nil;
  FRestoringSpectrumModel := False;

  FRunning := False;

  SpectrumColoursBGRA.Bmp.SetSize(WholeScreenWidth, WholeScreenHeight);

  GetActiveColours(Colours);
  FCodedBorderColour := 7;
  SetSpectrumColours(Colours);
  FSpeed := 0;
  SetSpeed(NormalSpeed);

  FProcessor.OnInputRequest := @ProcessorInput;
  FProcessor.OnOutputRequest := @ProcessorOutput;
  SetWriteScreen(True);

  FOnSync := nil;
  FOnStartRun := nil;
  FOnEndRun := nil;
  FOnResetSpectrum := nil;

  FSumTicks := 0;
  ResetSpectrum;

  FKeepRunning := True;
  FPaused := True;
  SetPaused(False);
end;

destructor TSpectrum.Destroy;
begin
  FreeAndNil(FAYSoundChip);
  FProcessor.Free;
  FMemory.Free;

  inherited Destroy;
end;

class function TSpectrum.DefaultSpectrumModel: TSpectrumModel;
begin
  Result := sm48K_issue_3;
end;

procedure TSpectrum.SetSpectrumModel(ASpectrumModel: TSpectrumModel;
  ACustomRoms: TStream);

  function GetRomResNames(AModel: TSpectrumModel; out ARoms: TStringDynArray): Boolean;
  begin
    case AModel of
      sm16K_issue_2, sm16K_issue_3, sm48K_issue_2, sm48K_issue_3:
        begin
          SetLength(ARoms{%H-}, 1);
          ARoms[0] := 'SPECTRUM48_ROM';
        end;
      sm128K:
        begin
          SetLength(ARoms, 2);
          ARoms[0] := 'SPECTRUM128_ENGLISH_0_ROM';
          ARoms[1] := 'SPECTRUM128_ENGLISH_1_ROM';
        end;
      smPlus2:
        begin
          SetLength(ARoms, 2);
          ARoms[0] := 'PLUS2_ENGLISH_0_ROM';
          ARoms[1] := 'PLUS2_ENGLISH_1_ROM';
        end;
      // perhaps more models one day...
    otherwise
      SetLength(ARoms, 0);
    end;

    Result := Length(ARoms) >= 1;
  end;

const
  FrameTicks48 = 69888;
  FrameTicks128 = 70908;

  TicksPerScanLine48 = 224;
  TicksPerScanLine128 = 228;

  // constants used in conversion between portaudio frequency and cpu frequency
  SoundPlayerRatePortAudio48 = 63; // in 48K spectrum,
  SoundPlayerRateProcessor48 = 5000; //  it is 44100 / 3500000 = 63 / 5000

  SoundPlayerRatePortAudio128 = 7; // in 128K spectrum,
  SoundPlayerRateProcessor128 = 563; //  it is 44100 / 3546900 = 7 / 563

  HoldInterruptPinTicks48 = 32; // 48K and +3 models hold interrupt for 32 t-states,
  HoldInterruptPinTicks128 = 36; // whereas 128K hods it for 36 t-states

var
  RomStream: TStream;
  NewRamSize: Word;

  SkipReset: Boolean;
  Roms: TStringDynArray;
  I: Integer;
  PrevSpeed: Integer;
  RomsCount: Integer;

begin
  if (ACustomRoms = nil) and (not CustomRomsMounted) then
    if ASpectrumModel = FSpectrumModel then
      Exit;

  SkipReset := False;

  NewRamSize := 128;
  FIs128KModel := True;
  FDivVol := 127 * 64;
  FModelWithHALbug := False;
  TicksPerScanLine := TicksPerScanLine128;
  RomsCount := 1;
  FProcessor.FrameTicks := FrameTicks128;
  HoldInterruptPinTicks := HoldInterruptPinTicks48; // 48K and +3

  try
    case ASpectrumModel of
      sm16K_issue_2, sm16K_issue_3, sm48K_issue_2, sm48K_issue_3:
        begin
          TicksPerScanLine := TicksPerScanLine48;
          FIs128KModel := False;
          FDivVol := FDivVol * 4.1;

          FProcessor.FrameTicks := FrameTicks48;
          case ASpectrumModel of
            sm16K_issue_2, sm16K_issue_3:
              NewRamSize := 16;
          otherwise
            NewRamSize := 48;
          end;

          SkipReset := (ACustomRoms = nil) and (not CustomRomsMounted)
            and (NewRamSize = FMemory.RamSizeKB);
        end;

      sm128K, smPlus2:
        begin
          HoldInterruptPinTicks := HoldInterruptPinTicks128;
          FModelWithHALbug := True;
          RomsCount := 2;
        end;

    otherwise
      Abort;
    end;

    FProcessor.TicksPerLine := TicksPerScanLine;

    FLateTimings := not FLateTimings; // force recalculations made
    SetLateTimings(not FLateTimings); // when setting early/late ula timings

    FMemory.InitBanks(NewRamSize, RomsCount);
    SetLength(Roms{%H-}, 0);
    if Assigned(ACustomRoms) then begin
      if ACustomRoms.Size <> (RomsCount shl 14) then
        Abort;

      for I := 0 to RomsCount - 1 do begin
        ACustomRoms.Position := I shl 14;

        if not FMemory.LoadFromStream(I, True, ACustomRoms) then
          Abort;
      end;
      CustomRomsMounted := True;

    end else begin
      if not (GetRomResNames(ASpectrumModel, Roms) and (Length(Roms) = RomsCount)) then
        Abort;

      for I := 0 to RomsCount - 1 do begin
        RomStream := nil;
        try
          RomStream := TResourceStream.Create(HINSTANCE, Roms[I], RT_RCDATA);
          RomStream.Position := 0;

          if not FMemory.LoadFromStream(I, True, RomStream) then
            Abort;
        finally
          RomStream.Free;
        end;
      end;
      CustomRomsMounted := False;
    end;

  except
    FIs128KModel := False;
    ASpectrumModel := smNone;
  end;

  if FIs128KModel then begin
    FSoundPlayerRatePortAudio := SoundPlayerRatePortAudio128;
    FSoundPlayerRateProcessor := SoundPlayerRateProcessor128;

    if FAYSoundChip = nil then begin
      FAYSoundChip := TSoundAY_3_8912.Create;
      FAYSoundChip.OnCheckTicks := @GetTotalTicks;
    end else
      FAYSoundChip.Reset();
  end else begin                            
    FreeAndNil(FAYSoundChip);
    FSoundPlayerRatePortAudio := SoundPlayerRatePortAudio48;
    FSoundPlayerRateProcessor := SoundPlayerRateProcessor48;
  end;

  if FInLoadingSnapshot then begin
    if FBkpSpectrumModel = smNone then
      FBkpSpectrumModel := FSpectrumModel;
  end else
    FBkpSpectrumModel := smNone;

  SkipReset :=
    SkipReset or FRestoringSpectrumModel;

  FSpectrumModel := ASpectrumModel;
  FIssue2Keyboard := FSpectrumModel in [sm16K_issue_2, sm48K_issue_2];

  if Assigned(FOnChangeModel) then
    Synchronize(FOnChangeModel);

  PrevSpeed := FSpeed;
  FSpeed := 0;
  if not SkipReset then
    ResetSpectrum;
  SetSpeed(PrevSpeed);
end;

procedure TSpectrum.UpdateDebuggedOrPaused;
begin
  if (FPaused or Assigned(FDebugger)) = FKeepRunning then begin
    StopSoundPlayer;
    FKeepRunning := FRunning and not FKeepRunning;

    UpdateAskForSpeedCorrection;
    CheckStartSoundPlayer;
  end;
end;

procedure TSpectrum.AttachDebugger(ADebugger: IDebugger);
begin
  if (FDebugger = nil) and (ADebugger <> nil) then begin
    ADebugger.SetSpectrum(Self);
    FDebugger := ADebugger;
  end;

  UpdateDebuggedOrPaused();
end;

procedure TSpectrum.DettachDebugger;
begin
  FDebugger := nil;
  UpdateDebuggedOrPaused;
end;

procedure TSpectrum.SetTapePlayer(ATapePlayer: TAbstractTapePlayer);
begin
  FTapePlayer := ATapePlayer;
end;

procedure TSpectrum.SetSpectrumColours(const Colours: TLCLColourMap);
begin
  FLCLColours := Colours;
  AdjustSpectrumColours();
end;

procedure TSpectrum.GetSpectrumColours(out Colours: TLCLColourMap);
begin
  Colours := FLCLColours;
end;

procedure TSpectrum.SetWriteScreen(const AValue: Boolean);
begin
  if AValue xor (FProcessor.OnNeedWriteScreen = @WriteToScreen) then begin
    StopSoundPlayer;
    if AValue then
      FProcessor.OnNeedWriteScreen := @WriteToScreen
    else
      FProcessor.OnNeedWriteScreen := nil;

    UpdateAskForSpeedCorrection;
    CheckStartSoundPlayer;
  end;
end;

procedure TSpectrum.StopRunning;
begin
  FRunning := False;
  FKeepRunning := False;
end;

procedure TSpectrum.ResetSpectrum;
begin
  StopSoundPlayer;
  if not FInLoadingSnapshot then
    if FBkpSpectrumModel <> smNone then begin
      if FSpectrumModel <> FBkpSpectrumModel then begin
        FRestoringSpectrumModel := True;
        try
          SetSpectrumModel(FBkpSpectrumModel, nil);
        finally
          FRestoringSpectrumModel := False;
        end;
      end;
      FBkpSpectrumModel := smNone;
    end;

  if Assigned(FOnResetSpectrum) then
    Synchronize(FOnResetSpectrum);

  FIntPinUpCount := HoldInterruptPinTicks;

  FInternalEar := 0;
  FEarFromTape := 0;
  FEar := 0;
  FMic := 0;
  //FCodedBorderColour := 0;
  //SetCodedBorderColour(7);
  TicksFrom := ScreenStart;
  FSumTicks := FSumTicks + FProcessor.TStatesInCurrentFrame;
  FProcessor.ResetCPU;
  FProcessor.IntPin := True;
  FFlashState := 0;

  FMemory.InitBanks();
  FMemory.ClearRam;
  FPagingEnabled := FIs128KModel;
  FProcessor.ContendedHighBank := False;

  FFrameCount := 0;
  InitTimes;
  KeyBoard.ClearKeyboard;
  TJoystick.Joystick.ResetState;

  if Assigned(FAYSoundChip) then begin
    FAYSoundChip.Reset();
  end;

  TSoundPlayer.BufferLen := 512 * 64 * 4 * 8;
  CheckStartSoundPlayer;
end;

function TSpectrum.IsRunning: Boolean;
begin
  Result := FRunning;
end;

function TSpectrum.GetProcessor: TProcessor;
begin
  Result := FProcessor;
end;

function TSpectrum.GetFrameCount: Int64;
begin
  Result := FFrameCount;
end;

procedure TSpectrum.DrawToCanvas(const ACanvas: TCanvas; const R: TRect);
begin
  SpectrumColoursBGRA.Bmp.InvalidateBitmap;
  SpectrumColoursBGRA.Bmp.Draw(ACanvas, R, True);
end;

function TSpectrum.GetBgraColours: TBGRAColours;
begin
  Result := SpectrumColoursBGRA.BGRAColours;
end;

function TSpectrum.IsIssue2: Boolean;
begin
  Result := FIssue2Keyboard;
end;

procedure TSpectrum.DoStep;
var
  MilliSecondsPassed: Int64;
  MilliSecondsToWait: Int64;
begin
  if Assigned(FTapePlayer) then
    FTapePlayer.GetNextPulse();
  FProcessor.DoProcess;

  if FIntPinUpCount <> 0 then begin
    if FProcessor.TStatesInCurrentFrame >= FIntPinUpCount then begin
      FProcessor.IntPin := False;
      FIntPinUpCount := 0;
    end;
  end;

  if FProcessor.TStatesInCurrentFrame >= FProcessor.FrameTicks then begin
    FProcessor.IntPin := True;

    Inc(FFrameCount);
    //WriteToScreen(ScreenEnd);
    FProcessor.OnNeedWriteScreen(ScreenEnd);

    FSumTicks := FSumTicks + FProcessor.FrameTicks;
    FProcessor.TStatesInCurrentFrame := FProcessor.TStatesInCurrentFrame - FProcessor.FrameTicks;
    FIntPinUpCount := HoldInterruptPinTicks;

    TicksFrom := ScreenStart;

    FFlashState := (FFlashState + 1) and 31;

    if AskForSpeedCorrection then begin
      UpdateSoundBuffer;
      //
      MicrosecondsNeeded := MicrosecondsNeeded + SpeedCorrection;

      // speed 100%:
      MilliSecondsPassed := GetTickCount64 - PCTicksStart;
      MilliSecondsToWait := MicrosecondsNeeded div 1000 - MilliSecondsPassed;

      if MilliSecondsToWait > 0 then
        Sleep(MilliSecondsToWait);
    end;

    DoSync;
  end;
end;

procedure TSpectrum.RunSpectrum;
begin
  if FRunning then
    Exit;
  if FProcessor = nil then
    Exit;
  if FSpectrumModel = smNone then
    Exit;

  ResetSpectrum;
  FRunning := True;
  FKeepRunning := True;

  if Assigned(FOnStartRun) then
    Synchronize(FOnStartRun);

  CheckStartSoundPlayer;

  while FRunning do begin
    while FKeepRunning do
      DoStep;

    while FRunning and not FKeepRunning do begin
      if Assigned(FDebugger) then begin
        Synchronize(@CheckDebuggerStep);
        if StepInDebugger then begin
          DoStep;
          Synchronize(@AfterDebuggerStep);
        end;
      end;
      Sleep(20);
      DoSync;
    end;
  end;
  StopSoundPlayer;

  FSumTicks := FSumTicks + FProcessor.TStatesInCurrentFrame;

  if Assigned(FOnEndRun) then
    Synchronize(FOnEndRun);

  FOnChangeModel := nil;
  Terminate;
end;

procedure TSpectrum.SetSoundMuted(AValue: Boolean);
begin
  if FSoundMuted = AValue then
    Exit;

  StopSoundPlayer;
  FSoundMuted := AValue;
  CheckStartSoundPlayer;
end;

procedure TSpectrum.SetFlashState(AValue: UInt16);
begin
  FFlashState := AValue and 31;
end;

function TSpectrum.GetRemainingIntPinUp: Integer;
begin
  if FProcessor.IntPin and (FIntPinUpCount > FProcessor.TStatesInCurrentFrame) then begin
    Result := FIntPinUpCount - FProcessor.TStatesInCurrentFrame;
  end else
    Result := 0;
end;

function TSpectrum.GetFlashState: UInt16;
begin
  Result := FFlashState and 31;
end;

procedure TSpectrum.SetRemainingIntPinUp(AValue: Integer);
begin
  if AValue > 0 then begin
    FIntPinUpCount := FProcessor.TStatesInCurrentFrame + AValue
  end else
    FIntPinUpCount := 0;
  FProcessor.IntPin := FIntPinUpCount > 0;
end;

procedure TSpectrum.WriteToScreen(TicksTo: Int32Fast);
var
  X, Y: Int32Fast;
  ScreenPixel: PBGRAPixel;

  procedure WriteToCentralScreenArea; inline;
  var
    AdrX, AdrY: Int32Fast;
    Adr: Word;
    I: Integer;
    By, BAttr: Byte;
    Bright: Boolean;
    CInk, CPaper: TBGRAPixel;
    WRAdr: WordRec absolute Adr;
  begin
    AdrX := (X - LeftBorder) shr 3;
    AdrY := Y - TopBorder;
    WRAdr.Hi := {%01000000 or} ((AdrY shr 3) and %00011000) or (AdrY and %111);
    WRAdr.Lo := ((AdrY shl 2) and %11100000) or AdrX;

    By := FMemory.ReadScreenByte(Adr);

    WRAdr.Hi := (WRAdr.Hi shr 3) {or $50} or $18;
    Battr := FMemory.ReadScreenByte(Adr);

    Bright := BAttr and %01000000 <> 0;

    CPaper := SpectrumColoursBGRA.BGRAColours[Bright, (BAttr shr 3) and %111];
    CInk := SpectrumColoursBGRA.BGRAColours[Bright, BAttr and %111];

    if (FFlashState and %10000 <> 0) and (BAttr and %10000000 <> 0) then
      By := not By;

    for I := 7 downto 0 do begin
      if By and (1 shl I) <> 0 then
        ScreenPixel^ := CInk
      else
        ScreenPixel^ := CPaper;
      Inc(ScreenPixel);
    end;
  end;

const
  RightBorderStart = LeftBorder + CentralScreenWidth;
  BottomBorderStart = TopBorder + CentralScreenHeight;

var
  CCTicks, CCTicksTo: Int32Fast;
begin
  if TicksTo > ScreenEnd then
    TicksTo := ScreenEnd;

  if TicksFrom <= TicksTo then begin

    CCTicks := TicksFrom - ScreenStart;
    CCTicksTo := TicksTo - ScreenStart;

    X := (CCTicks shl 1) mod (TicksPerScanLine shl 1);
    Y := CCTicks div TicksPerScanLine;

    if X < WholeScreenWidth then
      ScreenPixel := SpectrumColoursBGRA.Bmp.ScanLine[Y] + X;

    repeat
      if X < WholeScreenWidth then begin
        if (X < LeftBorder) or (X >= RightBorderStart)
            or (Y < TopBorder) or (Y >= BottomBorderStart)
        then begin
          FillDWord({%H-}ScreenPixel^, 8, DWord(SpectrumColoursBGRA.BorderColour2));
          Inc(ScreenPixel, 8);
        end else
          WriteToCentralScreenArea;

        Inc(CCTicks, 4);
        if CCTicks > CCTicksTo then
          Break;  
        Inc(X, 8);
      end else begin
        // horizontal retrace
        Inc(Y);
        CCTicks := Y * TicksPerScanLine;
        if CCTicks > CCTicksTo then
          Break;
        X := 0;
        ScreenPixel := SpectrumColoursBGRA.Bmp.ScanLine[Y];
      end;
    until False;

    TicksFrom := CCTicks + ScreenStart;
  end;
end;

procedure TSpectrum.SetPaused(AValue: Boolean);
begin
  if FPaused xor AValue then begin
    FPaused := AValue;
    UpdateDebuggedOrPaused;
  end;
end;

procedure TSpectrum.SetInternalEar(AValue: Byte);
begin
  UpdateSoundBuffer;
  FInternalEar := AValue;
  FEarFromTape := 0;
  FEar := AValue;
end;

procedure TSpectrum.SetLateTimings(AValue: Boolean);
const
  CentralScreenStart48 = 14335;
  CentralScreenStart128 = 14361;

begin
  if FLateTimings xor AValue then begin
    FLateTimings := AValue;

    if FIs128KModel then
      CentralScreenStart := CentralScreenStart128
    else
      CentralScreenStart := CentralScreenStart48;

    if AValue then
      CentralScreenStart := CentralScreenStart + 1;

    ScreenStart := CentralScreenStart - TopBorder * TicksPerScanLine - 24; //6247
    ScreenEnd := ScreenStart + (WholeScreenHeight - 1) * TicksPerScanLine + WholeScreenWidth div 2 - 1; // 65334

    FProcessor.ContentionFrom := CentralScreenStart;
    FProcessor.ContentionTo := FProcessor.ContentionFrom + (CentralScreenHeight - 1) * TicksPerScanLine + 128;
    FloatBusFirstInterestingTick := FProcessor.ContentionFrom + 4;
    FloatBusLastInterestingTick := FProcessor.ContentionTo + 3;
  end;
end;

procedure TSpectrum.SetEarFromTape(AValue: Byte);
begin
  UpdateSoundBuffer;
  if AValue <> FEarFromTape then begin
    FEarFromTape := AValue;
    FEar := FInternalEar or AValue;
  end;
end;

end.

