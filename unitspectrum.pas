unit UnitSpectrum;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, UnitMemory, Z80Processor,
  UnitSpectrumKeyboard, FastIntegers, UnitSpectrumColourMap,
  UnitSpectrumColoursBGRA, UnitJoystick, UnitBeeper, Graphics, LCLType, Forms;

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
  TSpectrumModel = (smNone, sm48K); // one day, more models... maybe.

  TSpectrumColour = Integer;
  TSpectrumColours = array [False..True, 0..7] of TSpectrumColour;

  { TSpectrum }

  TSpectrum = class(TThread)
  public type
    ISpectrumDevice = interface ['{9B450A9B-DA25-4144-9E8C-085001BE9CAC}']
      procedure SetSpectrum(Spectrum: TSpectrum);
    end;

    IDebugger = interface(ISpectrumDevice) ['{5B8746EF-84D0-4549-98FE-AE2AE4687912}']
      procedure OnStep(out DoContinue: Boolean);
      procedure AfterStep;
      function CheckBreakPoints: Boolean; // should return true when breakpoint is reached.
    end;

    ITapePlayer = interface(ISpectrumDevice) ['{B7E9C019-71C1-43A9-88D7-F794B1A1984B}']
      procedure Continue;
      procedure Rewind;
      procedure StopPlaying;
      procedure GetNextPulse();
    end;

  public const
    NormalSpeed = 512;
    MaxSpeed = NormalSpeed div 16;
    MinSpeed = NormalSpeed * 8;

  private
    // processor events
    procedure ProcessorInput;
    procedure ProcessorOutput;

  strict private
    FOnSync: TThreadMethod;
    FPaused: Boolean;
    FSpectrumModel: TSpectrumModel;
    FProcessor: TProcessor;
    FMemory: PMemory;
    FDebugger: IDebugger;
    FTapePlayer: ITapePlayer;

    // screen processing
    FCodedBorderColour: Byte;
    FLCLColours: TLCLColourMap;
    FlashState: Int16Fast;
    TicksFrom: Int32Fast;
    FSumTicks: Int64;
    MicrosecondsNeeded: Int64;

    PCTicksStart, PCTicksEnd: Int64;

    FSpeed: Integer;
    SpeedCorrection: Int16Fast;
    AskForSpeedCorrection: Boolean;
    FRunning: Boolean;
    FDebuggedOrPaused: Boolean;
    FEar: Byte;
    FOnEndRun: TThreadMethod;
    StepInDebugger: Boolean;
    FFrameCount: Int64;
    FIntPinUpCount: Int16Fast;
    FLatestTickUpdatedBeeper: Int64;

    procedure CheckStartBeeper;
    procedure UpdateBeeperBuffer; inline;
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
    procedure RunSpectrum;

  strict private
    FOnResetSpectrum: TThreadMethod;
    FOnStartRun: TThreadMethod;
    FSoundMuted: Boolean;
    SpectrumColoursBGRA: TSpectrumColoursBGRA;
    function GetPortAudioLibPath: String;
    function GetSoundVolume: Int8;
    procedure SetPortAudioLibPath(const AValue: String);
    procedure SetSoundMuted(AValue: Boolean);
    procedure SetSoundVolume(AValue: Int8);
    procedure UpdateDebuggedOrPaused;
    procedure SetEar(AValue: Byte);

  protected
    procedure Execute; override;

  public
    KeyBoard: TSpectrumKeyBoard;

    constructor Create;
    destructor Destroy; override;

    class function DefaultSpectrumModel: TSpectrumModel; static;
    procedure SetSpectrumModel(ASpectrumModel: TSpectrumModel);

    procedure AttachDebugger(ADebugger: IDebugger);
    procedure DettachDebugger;
    procedure AttachTapePlayer(ATapePlayer: ITapePlayer);
    procedure DettachTapePlayer;
    procedure SetSpectrumColours(const Colours: TLCLColourMap);
    procedure GetSpectrumColours(out Colours: TLCLColourMap);
    procedure SetWriteScreen(const AValue: Boolean);

    procedure StopRunning;
    procedure ResetSpectrum;
    function GetTicksRun(): Int64;
    function IsRunning: Boolean;
    function GetProcessor: TProcessor;
    function GetFrameCount: Int64;
    procedure DrawToCanvas(ACanvas: TCanvas); inline;

    property SumTicks: Int64 read FSumTicks;
    property CodedBorderColour: Byte read FCodedBorderColour write SetCodedBorderColour;
    property Speed: Integer read FSpeed write SetSpeed;
    property OnSync: TThreadMethod read FOnSync write SetOnSync;
    property OnStartRun: TThreadMethod read FOnStartRun write FOnStartRun;
    property OnEndRun: TThreadMethod read FOnEndRun write SetOnEndRun;
    property OnResetSpectrum: TThreadMethod read FOnResetSpectrum write FOnResetSpectrum;
    property Paused: Boolean read FPaused write SetPaused;
    property PortAudioLibPath: String read GetPortAudioLibPath write SetPortAudioLibPath;
    property SoundMuted: Boolean read FSoundMuted write SetSoundMuted;
    property SoundVolume: Int8 read GetSoundVolume write SetSoundVolume;
    property Ear: Byte read FEar write SetEar;
  end;

implementation

const
  CentralScreenStart = 14335;
  ScreenStart = CentralScreenStart - TopBorder * 224 - 24; //6247
  ScreenEnd = ScreenStart + (WholeScreenHeight - 1) * 224 + WholeScreenWidth div 2 - 1; // 65334

{ TSpectrum }

procedure TSpectrum.InitTimes;
begin
  MicrosecondsNeeded := 500; // start with half of millisecond
  PCTicksStart := SysUtils.GetTickCount64;
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

  SetCodedBorderColour(FCodedBorderColour or $F0);
end;

procedure TSpectrum.SetCodedBorderColour(AValue: Byte);
begin
  if AValue <> FCodedBorderColour then begin
    FCodedBorderColour := AValue and %111;
    SpectrumColoursBGRA.BorderColour2 := SpectrumColoursBGRA.BGRAColours[False, FCodedBorderColour];
  end;
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
    TBeeper.StopBeeper;

    // For 20 milliseconds (20.000 microseconds) passes 70.000 ticks.
    // How many microseconds it actually takes for 69888 ticks?
    // 20000 / 70000 = x / 69888
    // x = 69888 * 2 / 7
    // x = 19968
    // So:
    //SpeedCorrection := MulDiv(19968, FSpeed, 512);
    // that is:
    SpeedCorrection := 39 * FSpeed;

    UpdateAskForSpeedCorrection;
    CheckStartBeeper;
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

procedure TSpectrum.ProcessorInput;

  procedure CheckFloatingBus;
  const
    FirstInterestingTState = 14339;
    LastInterestingTState = FirstInterestingTState + 224 * (CentralScreenHeight - 1) + 127;
  var
    N, X, Y: Integer;
    W: Word;
    WR: WordRec absolute W;
  begin
    if (FProcessor.TStatesInCurrentFrame <= LastInterestingTState)
        and (FProcessor.TStatesInCurrentFrame >= FirstInterestingTState)
    then begin
      //
      N := FProcessor.TStatesInCurrentFrame - FirstInterestingTState;
      X := N mod 224;
      if X and $84 = 0 then begin //if (X <= 127) and (X mod 8 <= 3) then begin
        Y := N div 224;
        if X and 1 = 0 then
          WR.Hi := %01000000 or ((Y shr 3) and %00011000) or (Y and %111)
        else
          WR.Hi := %01011000 or ((Y shr 6) and %11);
        WR.Lo := (((Y shl 2) and %11100000) or (X shr 2)) + ((X shr 1) and 1);
        FProcessor.DataBus := FMemory^.ReadByte(W);
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
    // read KeyBoard...
    B := %00011111;

    if Application.Active then begin
      Ba := WordRec(FProcessor.AddressBus).Hi;
      for I := Low(KeyBoard.HalfRows) to High(KeyBoard.HalfRows) do
        if (Ba shr I) and 1 = 0 then
          B := B and KeyBoard.HalfRows[I];
    end;

    if Assigned(FTapePlayer) then
      FTapePlayer.GetNextPulse();

    FProcessor.DataBus :=
      B // KeyBoard can set LOW bits 0-4
      or FEar // and bit 6 is read from ear socket
      or %10100000
      ;
    //
  end else begin
    if ((FProcessor.AddressBus and $FF = $1F) and (TJoystick.Joystick.JoystickType = TJoystick.TJoystickType.jtKempston))
      or ((FProcessor.AddressBus and $FF = $7F) and (TJoystick.Joystick.JoystickType = TJoystick.TJoystickType.jtFuller))
    then begin
      FProcessor.DataBus := TJoystick.Joystick.State;
    end else begin
      FProcessor.DataBus := $FF;
      CheckFloatingBus;
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
    // As currently emulated, Ear represents both EAR and MIC
    {!!! NOTE: I would excpect the following line to work, but Patrik Rak's tests do not pass! }
    //Aux := ((Aux or (not Aux shl 1)) and %10000) shl 2;
    // This passes, though (only 1 in bit 4):
    Aux := (Aux and %10000) shl 2;

    SetEar(Aux);
  end;
end;

procedure TSpectrum.CheckStartBeeper;
begin
  if (not FSoundMuted) and AskForSpeedCorrection and FRunning and (FSpeed = NormalSpeed)
      and TBeeper.IsLibLoaded and (not TBeeper.IsPlaying)
  then begin
    FLatestTickUpdatedBeeper := (FSumTicks + FProcessor.TStatesInCurrentFrame) * 63;
    TBeeper.StartBeeper;
  end;
end;

procedure TSpectrum.UpdateBeeperBuffer;
var
  NewLatestFrame: Int64;
  FramesPassed: Int64;
begin
  if (FSpeed = NormalSpeed) and TBeeper.IsPlaying then begin
    NewLatestFrame := (FSumTicks + FProcessor.TStatesInCurrentFrame) * 63;

    FramesPassed := NewLatestFrame - FLatestTickUpdatedBeeper;
    while FramesPassed >= 2500 do begin
      (TBeeper.BeeperBuffer + TBeeper.CurrentPosition)^ := FEar;
      Inc(TBeeper.CurrentPosition);
      if TBeeper.CurrentPosition >= TBeeper.BufferLen then
        TBeeper.CurrentPosition := 0;
      FramesPassed := FramesPassed - 5000;
    end;
    FLatestTickUpdatedBeeper := NewLatestFrame - FramesPassed;
  end;
end;

procedure TSpectrum.UpdateAskForSpeedCorrection;
begin
  if AskForSpeedCorrection =
      (FDebuggedOrPaused or (SpeedCorrection = 0)
                                or (FProcessor.OnNeedWriteScreen <> @WriteToScreen))
  then begin
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

  FSoundMuted := False;

  FreeOnTerminate := False;

  FDebugger := nil;
  FTapePlayer := nil;

  FProcessor := TProcessor.Create;
  FMemory := FProcessor.GetMemory();
  FSpectrumModel := smNone;

  FRunning := False;

  SpectrumColoursBGRA.Bmp.SetSize(WholeScreenWidth, WholeScreenHeight);

  GetActiveColours(Colours);
  SetSpectrumColours(Colours);
  FSpeed := 0;
  SetSpeed(NormalSpeed);

  FProcessor.OnInputRequest := @ProcessorInput;
  FProcessor.OnOutputRequest := @ProcessorOutput;
  SetWriteScreen(True);

  FOnSync := nil;
  FOnStartRun := nil;
  FOnEndRun := nil;

  ResetSpectrum;

  FDebuggedOrPaused := False;
  FPaused := True;
  SetPaused(False);
end;

destructor TSpectrum.Destroy;
begin
  FMemory := nil;
  FProcessor.Free;

  inherited Destroy;
end;

class function TSpectrum.DefaultSpectrumModel: TSpectrumModel;
begin
  Result := sm48K;
end;

procedure TSpectrum.SetSpectrumModel(ASpectrumModel: TSpectrumModel);
var
  RomStream: TStream;
begin
  if ASpectrumModel = FSpectrumModel then
    Exit;

  FSpectrumModel := ASpectrumModel;
  try
    case FSpectrumModel of
      sm48K:
        begin
          RomStream := nil;
          try
            RomStream := TResourceStream.Create(HINSTANCE, 'SPECTRUM48_ROM', RT_RCDATA);

            FMemory^.SetSizesInKB(16, 48);
            RomStream.Position := 0;
            if not FMemory^.LoadRomFromStream(RomStream) then
              Abort;

          finally
            RomStream.Free;
          end;
        end;
    otherwise
    end;

  except
    FSpectrumModel := smNone;
  end;

  ResetSpectrum;
end;

procedure TSpectrum.UpdateDebuggedOrPaused;
begin
  if (FPaused or Assigned(FDebugger)) xor FDebuggedOrPaused then begin
    TBeeper.StopBeeper;
    FDebuggedOrPaused := not FDebuggedOrPaused;

    UpdateAskForSpeedCorrection;
    CheckStartBeeper;
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

procedure TSpectrum.AttachTapePlayer(ATapePlayer: ITapePlayer);
begin
  if ATapePlayer <> FTapePlayer then begin
    DettachTapePlayer;

    if ATapePlayer <> nil then begin
      ATapePlayer.SetSpectrum(Self);
      FTapePlayer := ATapePlayer;
      FTapePlayer.Rewind;
    end;
  end;

end;

procedure TSpectrum.DettachTapePlayer;
begin
  if FTapePlayer <> nil then begin
    FTapePlayer.SetSpectrum(nil);
    FTapePlayer := nil;
  end;
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
    TBeeper.StopBeeper;
    if AValue then
      FProcessor.OnNeedWriteScreen := @WriteToScreen
    else
      FProcessor.OnNeedWriteScreen := nil;

    UpdateAskForSpeedCorrection;
    CheckStartBeeper;
  end;
end;

procedure TSpectrum.StopRunning;
begin
  FRunning := False;
end;

procedure TSpectrum.ResetSpectrum;
begin
  TBeeper.StopBeeper;

  { #todo : why stop playing?
when spectrum is reset the tape seems stopped for some time...
some bug... This is a workaround, so investigate, see to remove this "stop playing"... }
  if Assigned(FTapePlayer) then
    FTapePlayer.StopPlaying;

  if Assigned(FOnResetSpectrum) then
    Synchronize(FOnResetSpectrum);

  FIntPinUpCount := 0;
  FSumTicks := 0;
  FEar := 0;
  FCodedBorderColour := 0;
  SetCodedBorderColour(7);
  TicksFrom := ScreenStart;
  FProcessor.ResetCPU;
  FlashState := 0;
  FProcessor.GetMemory()^.ClearRam;

  FFrameCount := 0;
  InitTimes;
  KeyBoard.ClearKeyboard;
  TJoystick.Joystick.ResetState;

  CheckStartBeeper;
end;

function TSpectrum.GetTicksRun(): Int64;
begin
  Result := PCTicksEnd - PCTicksStart;
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

procedure TSpectrum.DrawToCanvas(ACanvas: TCanvas);
begin
  SpectrumColoursBGRA.Bmp.InvalidateBitmap;
  SpectrumColoursBGRA.Bmp.Draw(ACanvas, 0, 0, True);
end;

procedure TSpectrum.RunSpectrum;

  procedure DoStep; inline;    
  const
    FrameTicks = 69888;
  var                        
    MilliSecondsPassed: Int64;
    MilliSecondsToWait: Int64;
  begin
    if Assigned(FTapePlayer) then
      FTapePlayer.GetNextPulse();
    FProcessor.DoProcess;

    if FIntPinUpCount > 0 then begin
      if FProcessor.IntPin then begin
        if FProcessor.TStatesInCurrentFrame >= FIntPinUpCount then begin
          FProcessor.IntPin := False;
          FIntPinUpCount := 0;
        end;

      end;
    end;

    if FProcessor.TStatesInCurrentFrame >= FrameTicks then begin
      FProcessor.IntPin := True;

      Inc(FFrameCount);
      //WriteToScreen(ScreenEnd);
      FProcessor.OnNeedWriteScreen(ScreenEnd);

      FSumTicks := FSumTicks + FrameTicks;
      FProcessor.TStatesInCurrentFrame := FProcessor.TStatesInCurrentFrame - FrameTicks;
      FIntPinUpCount := FProcessor.TStatesInCurrentFrame + 32;

      TicksFrom := ScreenStart;

      FlashState := (FlashState + 1) and 31;

      if AskForSpeedCorrection then begin
        UpdateBeeperBuffer;
        //
        MicrosecondsNeeded := MicrosecondsNeeded + SpeedCorrection;

        // speed 100%:
        MilliSecondsPassed := GetTickCount64 - PCTicksStart;
        MilliSecondsToWait := (MicrosecondsNeeded div 1000) - MilliSecondsPassed;

        if MilliSecondsToWait > 0 then
          Sleep(MilliSecondsToWait);
      end;

      DoSync;
    end;
  end;

begin
  if FRunning then
    Exit;
  if FProcessor = nil then
    Exit;
  if FSpectrumModel = smNone then
    Exit;

  ResetSpectrum;
  FRunning := True;

  if Assigned(FOnStartRun) then
    Synchronize(FOnStartRun);

  TBeeper.BufferLen := 512 * 64; //43;
  CheckStartBeeper;

  while FRunning do begin
    if not FDebuggedOrPaused then begin
      DoStep;
    end else begin
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
  TBeeper.StopBeeper;

  PCTicksEnd := GetTickCount64;
  FSumTicks := FSumTicks + FProcessor.TStatesInCurrentFrame;

  if Assigned(FOnEndRun) then
    Synchronize(FOnEndRun);

  Terminate;
end;

procedure TSpectrum.SetSoundMuted(AValue: Boolean);
begin
  if FSoundMuted = AValue then
    Exit;
  TBeeper.StopBeeper;
  FSoundMuted := AValue;
  CheckStartBeeper;
end;

function TSpectrum.GetSoundVolume: Int8;
begin
  Result := TBeeper.BeeperVolume;
end;

function TSpectrum.GetPortAudioLibPath: String;
begin
  Result := TBeeper.LibPath;
end;

procedure TSpectrum.SetPortAudioLibPath(const AValue: String);
begin
  TBeeper.LibPath := AValue;
  CheckStartBeeper;
end;

procedure TSpectrum.SetSoundVolume(AValue: Int8);
begin
  TBeeper.BeeperVolume := AValue;
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
    WRAdr.Hi := %01000000 or ((AdrY shr 3) and %00011000) or (AdrY and %111);
    WRAdr.Lo := ((AdrY shl 2) and %11100000) or AdrX;

    By := FMemory^.ReadByte(Adr);

    WRAdr.Hi := (WRAdr.Hi shr 3) or $50;
    Battr := FMemory^.ReadByte(Adr);

    Bright := BAttr and %01000000 <> 0;

    CPaper := SpectrumColoursBGRA.BGRAColours[Bright, (BAttr shr 3) and %111];
    CInk := SpectrumColoursBGRA.BGRAColours[Bright, BAttr and %111];

    if (FlashState and %10000 <> 0) and (BAttr and %10000000 <> 0) then
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

    CCTicks := (TicksFrom - ScreenStart);
    CCTicksTo := (TicksTo - ScreenStart);

    X := (CCTicks shl 1) mod 448;
    Y := CCTicks div 224;

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
        CCTicks := Y * 224;
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

procedure TSpectrum.SetEar(AValue: Byte);
begin
  //if FEar <> AValue then begin
    UpdateBeeperBuffer;
    FEar := AValue;
  //end;
end;

end.

