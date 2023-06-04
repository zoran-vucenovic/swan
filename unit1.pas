unit unit1;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ComCtrls, LCLType, Buttons, StdCtrls, ExtCtrls, zipper, fpjson, UnitSpectrum,
  UnitFileSna, AboutBox, DebugForm, UnitFormBrowser,
  UnitColourPalette, UnitSpectrumColourMap, CommonFunctionsLCL,
  UnitFormKeyMappings, UnitJoystick, UnitFormJoystickSetup,
  UnitDataModuleImages, unitSoundVolume, UnitConfigs,
  UnitInputLibraryPathDialog, UnitFormInputPokes, UnitHistorySnapshots, UnitSZX,
  UnitFormHistorySnapshots, UnitTapePlayer, UnitVer, UnitKeyboardOnScreen;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionShowKeyboardOnScreen: TAction;
    ActionModel48KIssue3: TAction;
    ActionModel48KIssue2: TAction;
    ActionResetCPU: TAction;
    ActionNMI: TAction;
    ActionHistorySnapshots: TAction;
    ActionSaveSzx: TAction;
    ActionEnableHistory: TAction;
    ActionMoveBack: TAction;
    ActionInputPokes: TAction;
    ActionPortAudioLibPath: TAction;
    ActionMuteSound: TAction;
    ActionDontDrawScreenWhenLoading: TAction;
    ActionEjectTape: TAction;
    ActionEnableJoystick: TAction;
    ActionDecTapeBlock: TAction;
    ActionJoystick: TAction;
    ActionKeyMappings: TAction;
    ActionIncTapeBlock: TAction;
    ActionStop: TAction;
    ActionRewind: TAction;
    ActionShowTapePlayer: TAction;
    ActionPlay: TAction;
    ActionAttachTap: TAction;
    ActionShowDebugger: TAction;
    ActionAbout: TAction;
    ActionSaveZ80: TAction;
    ActionSaveSna: TAction;
    ActionColours: TAction;
    ActionOpen: TAction;
    ActionReset: TAction;
    ActionFullSpeed: TAction;
    ActionNormalSpeed: TAction;
    ActionSizeDecrease: TAction;
    ActionSizeIncrease: TAction;
    ActionSpeedIncrease: TAction;
    ActionSpeedDecrease: TAction;
    ActionPause: TAction;
    ActionExit: TAction;
    ActionList1: TActionList;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelStatus: TPanel;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    SpeedButton1: TSpeedButton;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionAttachTapExecute(Sender: TObject);
    procedure ActionColoursExecute(Sender: TObject);
    procedure ActionDecTapeBlockExecute(Sender: TObject);
    procedure ActionDontDrawScreenWhenLoadingExecute(Sender: TObject);
    procedure ActionEjectTapeExecute(Sender: TObject);
    procedure ActionEnableHistoryExecute(Sender: TObject);
    procedure ActionEnableJoystickExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionFullSpeedExecute(Sender: TObject);
    procedure ActionHistorySnapshotsExecute(Sender: TObject);
    procedure ActionIncTapeBlockExecute(Sender: TObject);
    procedure ActionInputPokesExecute(Sender: TObject);
    procedure ActionJoystickExecute(Sender: TObject);
    procedure ActionKeyMappingsExecute(Sender: TObject);
    procedure ActionModel48KIssue2Execute(Sender: TObject);
    procedure ActionModel48KIssue3Execute(Sender: TObject);
    procedure ActionMoveBackExecute(Sender: TObject);
    procedure ActionMuteSoundExecute(Sender: TObject);
    procedure ActionNMIExecute(Sender: TObject);
    procedure ActionNormalSpeedExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionPlayExecute(Sender: TObject);
    procedure ActionPortAudioLibPathExecute(Sender: TObject);
    procedure ActionResetCPUExecute(Sender: TObject);
    procedure ActionResetExecute(Sender: TObject);
    procedure ActionRewindExecute(Sender: TObject);
    procedure ActionSaveSnaExecute(Sender: TObject);
    procedure ActionSaveSzxExecute(Sender: TObject);
    procedure ActionSaveZ80Execute(Sender: TObject);
    procedure ActionShowDebuggerExecute(Sender: TObject);
    procedure ActionShowKeyboardOnScreenExecute(Sender: TObject);
    procedure ActionShowTapePlayerExecute(Sender: TObject);
    procedure ActionSizeDecreaseExecute(Sender: TObject);
    procedure ActionSizeIncreaseExecute(Sender: TObject);
    procedure ActionSpeedDecreaseExecute(Sender: TObject);
    procedure ActionSpeedIncreaseExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  strict private
    const
      cSection0 = 'general';
      cSectionSkipWriteScr = 'skip_scr_upd_when_tape_playing';
      cSectionScreenSizeFactor = 'screen_size_factor';
      cSectionSoundVolume = 'sound_volume';
      cSectionSoundMuted = 'sound_muted';
      cSectionPortAudioLibPath32 = 'portaudio_lib_path32';
      cSectionPortAudioLibPath64 = 'portaudio_lib_path64';
      cSectionLastFilePath = 'last_file_path';
      cSectionSpectrumModel = 'spectrum_model';
      cSectionSwanVersion = 'swan_version';

  strict private
    FNewModel: TSpectrumModel;

    procedure DoChangeModel(Sender: TObject);
    function MakeExtensionsFilter(ArSt: Array of String): String;
    procedure AfterShow(Data: PtrInt);
    procedure UpdateActionsModel();
    procedure UpdateShowCurrentlyActiveJoystick;
    procedure UpdateShowSound;
    procedure LoadFromConf;
    procedure SaveToConf;
    function GetSoundVolume: Integer;
    procedure SetSoundVolume(N: Integer);
    procedure SetSnapshotHistoryEnabled(const B: Boolean);

    procedure KeyFromFormKeyboardOnScreen(AKeyValue: Word; Flags: Integer);
    procedure ReleaseShifts(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure DoOnKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  strict private
    type
      TScreenSizeFactor = 1..4;

      TFileUnzipper = class(TObject)
      strict private
        Stream: TStream;

        procedure DoCreateOutZipStream(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);
        procedure DoDoneOutZipStream(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);
      public
        class function GetFileFromZipFile(const ZipFileName: String; const Extensions: array of String;
          out AStream: TStream; out FileNameInZip: String): Boolean; static;
      end;

      TSnapshotOrTape = (stSnapshot, stTape, stBoth);

      TKeyEventRec = record
        KeyIndex: Integer;
        BDown: Integer;
      end;

  strict private
    FTapeBrowser: TFormBrowseTape;
    FKeyboardOnScreen: UnitKeyboardOnScreen.TFormKeyboardOnScreen;
    PrevTicks: Int64;
    PrevPCTicks: Int64;
    PrevTimeStop: Integer;

    EventsQueueCount: Integer;
    EventsQueue: Array of TNotifyEvent;

    KeyEventCount: Integer;
    KeyEventQueue: Array of TKeyEventRec;

    Bmp: TBitmap;
    Spectrum: TSpectrum;
    ScreenSizeFactor: TScreenSizeFactor;
    DrawingRect: TRect;

    FormDebug: TFormDebug;
    TapePlayer: TTapePlayer;
    FWriteScreen: Boolean;
    FSkipWriteScreen: Boolean;
    FSoundVolumeForm: TFormSoundVolume;
    FLastFilePath: RawByteString;
    FPortaudioLibPathOtherBitness: RawByteString;
    HistoryQueue: TSnapshotHistoryQueue;
    SnapshotHistoryOptions: TSnapshotHistoryOptions;

    procedure TryLoadFromFiles(const SnapshotOrTape: TSnapshotOrTape; const AFileNames: Array of String);
    procedure UpdateActiveSnapshotHistory;
    procedure UpdateTextTapeRunning;
    procedure UpdateWriteScreen;
    procedure UpdateCheckWriteScreen;
    procedure AddKeyEventToQueue(KeyIndex: Integer; BDown: Integer);
    procedure AddEventToQueue(Event: TNotifyEvent);
    procedure PaintScreen(Sender: TObject);
    procedure DoDetachDebugger(Sender: TObject);
    procedure SaveSnapshot(SnapshotClass: TSnapshotFileClass);
    procedure SoundVolumeOnChg(Sender: TObject);
    procedure SoundVolumeAfterShow(Data: PtrInt);
    procedure SoundVolumeOnShow(Sender: TObject);
    procedure EventPlayerOnChangeBlock(Sender: TObject);
    procedure PlayerOnChangeBlock;
    procedure EventTapeBrowserGoToBlock(Sender: TObject);
    procedure TapeBrowserGoToBlock();
    procedure TapeBrowserAttachTape;
    procedure GetAcceptableExtensions(const SnapshotOrTape: TSnapshotOrTape; const IncludeZip: Boolean; out Extensions: TStringDynArray);
    procedure LoadAsk(const SnapshotOrTape: TSnapshotOrTape);
    procedure DoLoad(const SnapshotOrTape: TSnapshotOrTape; ASourceFile: String);
    procedure RunSpectrum;
    procedure DoOnResetSpectrum;
    procedure DestroySpectrum;
    procedure SyncSpectrum;
    procedure SpectrumEndRun;
    procedure SpectrumStartRun;
    procedure UpdateScreenSizeFactor;
    procedure SetScreenSizeFactor(NewFactor: Integer);
    procedure SetSpectrumSpeed(NewSpeed: Integer);
    procedure ShowTapeBrowser();
    procedure ShowKeyboardOnScreen();
    procedure ShowSoundVolumeForm();
    procedure DestroySoundVolumeForm();
    procedure FreeTapePlayer;
    procedure SetNewAutoSize(Sender: TObject);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1.TFileUnzipper }

procedure TForm1.TFileUnzipper.DoCreateOutZipStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  Stream := TMemoryStream.Create;
  Stream.Position := 0;
  AStream := Stream;
end;

procedure TForm1.TFileUnzipper.DoDoneOutZipStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  if Assigned(AStream) then
    AStream.Position := 0;
end;

class function TForm1.TFileUnzipper.GetFileFromZipFile(
  const ZipFileName: String; const Extensions: array of String; out
  AStream: TStream; out FileNameInZip: String): Boolean;
var
  UnZ: TUnZipper;
  I, II, LE: Integer;
  Entry: TFullZipFileEntry;
  Extension, S: AnsiString;
  SL: TStringList;
  FiUnz: TFileUnzipper;
  ExtFound: Boolean;

begin
  Result := False;
  AStream := nil;
  FileNameInZip := '';

  LE := Length(Extensions);
  if LE > 0 then begin
    UnZ := TUnZipper.Create;
    try
      UnZ.FileName := ZipFileName;
      UnZ.Examine;

      I := 0;
      while (I < UnZ.Entries.Count) and (not Result) do begin
        Entry := UnZ.Entries.FullEntries[I];
        if not (Entry.IsDirectory or Entry.IsLink) then begin
          FileNameInZip := Entry.ArchiveFileName;
          Extension := ExtractFileExt(FileNameInZip);

          ExtFound := False;
          II := 0;
          while (not ExtFound) and (II < LE) do begin
            S := Trim(Extensions[II]);
            if Length(S) > 0 then begin
              if not S.StartsWith(ExtensionSeparator) then
                S := ExtensionSeparator + S;
              ExtFound := AnsiCompareText(Extension, S) = 0;
            end;
            Inc(II);
          end;

          if ExtFound then begin
            SL := TStringList.Create;
            try
              SL.Append(FileNameInZip);
              FiUnz := TFileUnzipper.Create;
              try
                FiUnz.Stream := nil;
                try
                  UnZ.OnCreateStream := @(FiUnz.DoCreateOutZipStream);
                  UnZ.OnDoneStream := @(FiUnz.DoDoneOutZipStream);

                  UnZ.UnZipFiles(SL);
                  if Assigned(FiUnz.Stream) then begin
                    if FiUnz.Stream.Size = 0 then
                      FreeAndNil(FiUnz.Stream)
                    else begin
                      AStream := FiUnz.Stream;
                      Result := True;
                    end;
                  end;
                except
                  AStream := nil;
                  FiUnz.Stream.Free;
                end;
              finally
                FiUnz.Free;
              end;

            finally
              SL.Free;
            end;

          end;

        end;
        Inc(I);
      end;

    finally
      UnZ.Free;
    end;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FNewModel := TSpectrumModel.smNone;
  HistoryQueue := nil;

  ActionAbout.Caption := 'About ' + ApplicationName + '...';
  ActionEnableHistory.Hint :=
    'Enable going through emulation back in time step by step';

  ActionMoveBack.ShortCut := SnapshotHistoryOptions.KeyGoBack;

  ActionModel48KIssue2.Tag := Ord(TSpectrumModel.sm48K_issue_2);
  ActionModel48KIssue3.Tag := Ord(TSpectrumModel.sm48K_issue_3);

  UpdateActiveSnapshotHistory;

  FLastFilePath := '';
  FPortaudioLibPathOtherBitness := '';
  Label2.Caption := ' ';
  Label1.Caption := ' ';

  FSoundVolumeForm := nil;
  FTapeBrowser := nil;
  FKeyboardOnScreen := nil;
  FormDebug := nil;
  TapePlayer := nil;

  EventsQueueCount := 0;
  SetLength(EventsQueue, 0);
  KeyEventCount := 0;
  SetLength(KeyEventQueue, 0);

  Caption := ApplicationName;
  Spectrum := nil;

  FSkipWriteScreen := True;
  FWriteScreen := True;

  Label1.Constraints.MinWidth := PanelStatus.Canvas.GetTextWidth('Wspeed 100.99%W');
  Label2.Constraints.MinWidth := PanelStatus.Canvas.GetTextWidth('WWWJoystick: Interface II right');

  AutoSize := False;
  Bmp := TBitmap.Create;

  DestroySpectrum;
  Spectrum := TSpectrum.Create;
  ScreenSizeFactor := 1;

  LoadFromConf;
  if FNewModel = TSpectrumModel.smNone then
    FNewModel := TSpectrum.DefaultSpectrumModel;
  DoChangeModel(nil);
  if ScreenSizeFactor = 1 then begin
    ScreenSizeFactor := 2;
    SetScreenSizeFactor(1);
  end;
  UpdateCheckWriteScreen;
  //
  TCommonFunctionsLCL.FormToScreenCentre(Self);
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  KeyEventCount := 0;
  Spectrum.KeyBoard.ClearKeyboard;
  TJoystick.Joystick.ResetState;
end;

procedure TForm1.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ActionColoursExecute(Sender: TObject);
var
  WasPaused: Boolean;
  Colours: TLCLColourMap;
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionColoursExecute);
  end else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;
      Spectrum.GetSpectrumColours(Colours);

      if UnitColourPalette.TFrameColourPalette3.PickColours(Colours) then begin
        Spectrum.SetSpectrumColours(Colours);
      end;

    finally
      Spectrum.Paused := WasPaused;
    end;

  end;
end;

procedure TForm1.ActionDecTapeBlockExecute(Sender: TObject);
begin
  if Assigned(TapePlayer) then begin
    if Sender <> Spectrum then
      AddEventToQueue(@ActionDecTapeBlockExecute)
    else begin
      TapePlayer.IncBlock(-1);
    end;
  end;
end;

procedure TForm1.ActionDontDrawScreenWhenLoadingExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionDontDrawScreenWhenLoadingExecute);
  end else begin
    FSkipWriteScreen := not FSkipWriteScreen;
    UpdateCheckWriteScreen;
  end;
end;

procedure TForm1.ActionEjectTapeExecute(Sender: TObject);
begin
  if Assigned(TapePlayer) then begin
    if Sender <> Spectrum then begin
      AddEventToQueue(@ActionEjectTapeExecute)
    end else begin
      FreeTapePlayer;
    end;
  end;
end;

procedure TForm1.ActionEnableHistoryExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionEnableHistoryExecute);
  end else begin
    SetSnapshotHistoryEnabled(not Assigned(HistoryQueue));
  end;
end;

procedure TForm1.ActionEnableJoystickExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionEnableJoystickExecute);
  end else begin
    KeyEventCount := 0;
    Spectrum.KeyBoard.ClearKeyboard;
    TJoystick.Joystick.ResetState;
    TJoystick.Joystick.Enabled := not TJoystick.Joystick.Enabled;
    UpdateShowCurrentlyActiveJoystick;
  end;
end;

procedure TForm1.ActionAboutExecute(Sender: TObject);
var
  WasPaused: Boolean;
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionAboutExecute);
  end else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;
      TFormAbout.ShowAbout();
    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

procedure TForm1.ActionAttachTapExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionAttachTapExecute);
  end else begin

    LoadAsk(TSnapshotOrTape.stTape);
  end;

end;

procedure TForm1.ActionFullSpeedExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionFullSpeedExecute);
  end else begin
    SetSpectrumSpeed(0);
  end;
end;

procedure TForm1.ActionHistorySnapshotsExecute(Sender: TObject);
var
  ActivateHistory: Boolean;
  WasPaused: Boolean;
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionHistorySnapshotsExecute)
  else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;
      if TFormHistorySnapshots.ShowFormHistorySnapshots(
        HistoryQueue, SnapshotHistoryOptions, Spectrum.GetBgraColours(), ActivateHistory)
      then begin
        ActionMoveBack.ShortCut := SnapshotHistoryOptions.KeyGoBack;
        if Assigned(HistoryQueue) xor ActivateHistory then
          SetSnapshotHistoryEnabled(ActivateHistory)
        else
          if Assigned(HistoryQueue) then
            HistoryQueue.UpdateOptions(SnapshotHistoryOptions);

      end;
    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

procedure TForm1.ActionIncTapeBlockExecute(Sender: TObject);
begin
  if Assigned(TapePlayer) then begin
    if Sender <> Spectrum then
      AddEventToQueue(@ActionIncTapeBlockExecute)
    else begin
      TapePlayer.IncBlock(1);
    end;
  end;
end;

procedure TForm1.ActionInputPokesExecute(Sender: TObject);
var
  Pokes: TPokesArray;
  PE: TPokeEntry;
  I: Integer;
  WasPaused: Boolean;
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionInputPokesExecute)
  else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;
      if TFormInputPokes.ShowInputPokesDialog(
        Spectrum.GetProcessor.GetMemory^.RomSize,
        Spectrum.GetProcessor.GetMemory^.MemSize - 1, Pokes)
      then begin
        for I := Low(Pokes) to High(Pokes) do begin
          PE := Pokes[I];
          Spectrum.GetProcessor.GetMemory^.WriteByte(PE.Adr, PE.Value);
        end;
      end;

    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

procedure TForm1.ActionJoystickExecute(Sender: TObject);
var
  WasPaused: Boolean;
  JoystickEnabled: Boolean;
  JoystickType: TJoystick.TJoystickType;
  AKeys: TJoystick.TJoystickDirectionsKeys;
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionJoystickExecute);
  end else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;

      KeyEventCount := 0;
      Spectrum.KeyBoard.ClearKeyboard;
      TJoystick.Joystick.ResetState;

      AKeys[TJoystick.TJoystickDirection.diUp] := TJoystick.Joystick.KeyUp;
      AKeys[TJoystick.TJoystickDirection.diDown] := TJoystick.Joystick.KeyDown;
      AKeys[TJoystick.TJoystickDirection.diLeft] := TJoystick.Joystick.KeyLeft;
      AKeys[TJoystick.TJoystickDirection.diRight] := TJoystick.Joystick.KeyRight;
      AKeys[TJoystick.TJoystickDirection.diFire] := TJoystick.Joystick.KeyFire;
      JoystickType := TJoystick.Joystick.JoystickType;
      JoystickEnabled := TJoystick.Joystick.Enabled;
      if TFormJoystickSetup.ShowJoystickOptionsDialog(JoystickType, AKeys, JoystickEnabled) then begin
        TJoystick.Joystick.SetKeys(AKeys);
        TJoystick.Joystick.Enabled := JoystickEnabled;
        TJoystick.Joystick.JoystickType := JoystickType;
        UpdateShowCurrentlyActiveJoystick;
      end;
    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

procedure TForm1.ActionKeyMappingsExecute(Sender: TObject);
var
  WasPaused: Boolean;
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionKeyMappingsExecute);
  end else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;

      if TFormKeyMappings.ShowFormKeyMappings() then begin
        Spectrum.KeyBoard.LoadFromKeyMappings;
      end;
      KeyEventCount := 0;
      Spectrum.KeyBoard.ClearKeyboard;
      TJoystick.Joystick.ResetState;
    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

procedure TForm1.ActionModel48KIssue2Execute(Sender: TObject);
begin
  FNewModel := TSpectrumModel.sm48K_issue_2;
  AddEventToQueue(@DoChangeModel);
end;

procedure TForm1.ActionModel48KIssue3Execute(Sender: TObject);
begin
  FNewModel := TSpectrumModel.sm48K_issue_3;
  AddEventToQueue(@DoChangeModel);
end;

procedure TForm1.ActionMoveBackExecute(Sender: TObject);
begin
  if Assigned(HistoryQueue) then begin
    if Sender <> Spectrum then begin
      AddEventToQueue(@ActionMoveBackExecute);
    end else begin
      HistoryQueue.LoadSnapshot(0, True);
    end;
  end;
end;

procedure TForm1.ActionMuteSoundExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionMuteSoundExecute);
  end else begin
    Spectrum.SoundMuted := not Spectrum.SoundMuted;
    UpdateShowSound;
  end;
end;

procedure TForm1.ActionNMIExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionNMIExecute);
  end else begin
    Spectrum.GetProcessor.NMI();
  end;
end;

procedure TForm1.ActionNormalSpeedExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionNormalSpeedExecute);
  end else begin
    SetSpectrumSpeed(TSpectrum.NormalSpeed);
  end;
end;

procedure TForm1.ActionOpenExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionOpenExecute);
  end else begin
    LoadAsk(TSnapshotOrTape.stBoth);
  end;
end;

procedure TForm1.ActionPauseExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionPauseExecute);
  end else begin
    Spectrum.Paused := not Spectrum.Paused;
  end;
end;

procedure TForm1.ActionPlayExecute(Sender: TObject);
begin
  if Assigned(TapePlayer) then begin
    if Sender <> Spectrum then
      AddEventToQueue(@ActionPlayExecute)
    else begin
      TapePlayer.Continue();
    end;
  end;
end;

procedure TForm1.ActionPortAudioLibPathExecute(Sender: TObject);
var
  S: String;
  WasPaused: Boolean;
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionPortAudioLibPathExecute);
  end else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;
      S := Spectrum.PortAudioLibPath;
      if TFormInputLibraryPath.ShowLibraryPathDialog(S) then begin
        Spectrum.PortAudioLibPath := S;
      end;
    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

procedure TForm1.ActionResetCPUExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionResetCPUExecute)
  end else begin
    Spectrum.GetProcessor.ResetPin();
  end;
end;

procedure TForm1.ActionResetExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionResetExecute);
  end else begin
    Spectrum.ResetSpectrum;
  end;
end;

procedure TForm1.ActionRewindExecute(Sender: TObject);
begin
  if Assigned(TapePlayer) then begin
    if Sender <> Spectrum then
      AddEventToQueue(@ActionRewindExecute)
    else begin
      TapePlayer.Rewind;
    end;
  end;
end;

procedure TForm1.ActionSaveSnaExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionSaveSnaExecute);
  end else
    SaveSnapshot(TSnapshotSNA);
end;

procedure TForm1.ActionSaveSzxExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionSaveSzxExecute);
  end else
    SaveSnapshot(TSnapshotSZX);
end;

procedure TForm1.ActionSaveZ80Execute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionSaveZ80Execute);
  end else
    SaveSnapshot(TSnapshotZ80);
end;

procedure TForm1.ActionShowDebuggerExecute(Sender: TObject);
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionShowDebuggerExecute)
  else begin
    if FormDebug = nil then begin
      FormDebug := TFormDebug.Create(nil);
      FormDebug.AddHandlerOnBeforeDestruction(@DoDetachDebugger);
    end;

    Spectrum.AttachDebugger(FormDebug);
    FormDebug.Show;
  end;
end;

procedure TForm1.ActionShowKeyboardOnScreenExecute(Sender: TObject);
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionShowKeyboardOnScreenExecute)
  else
    ShowKeyboardOnScreen();
end;

procedure TForm1.ActionShowTapePlayerExecute(Sender: TObject);
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionShowTapePlayerExecute)
  else
    ShowTapeBrowser();
end;

procedure TForm1.ActionSizeDecreaseExecute(Sender: TObject);
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionSizeDecreaseExecute)
  else
    SetScreenSizeFactor(ScreenSizeFactor - 1);
end;

procedure TForm1.ActionSizeIncreaseExecute(Sender: TObject);
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionSizeIncreaseExecute)
  else
    SetScreenSizeFactor(ScreenSizeFactor + 1);
end;

procedure TForm1.ActionSpeedDecreaseExecute(Sender: TObject);
begin                        
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionSpeedDecreaseExecute);
  end else begin
    if Spectrum.Speed = 0 then
      SetSpectrumSpeed(TSpectrum.NormalSpeed)
    else
      SetSpectrumSpeed(Spectrum.Speed * 2);
  end;

end;

procedure TForm1.ActionSpeedIncreaseExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionSpeedIncreaseExecute);
  end else begin

    if Spectrum.Speed = 0 then
      SetSpectrumSpeed(TSpectrum.NormalSpeed)
    else
      SetSpectrumSpeed(Spectrum.Speed div 2);

  end;
end;

procedure TForm1.ActionStopExecute(Sender: TObject);
begin
  if Assigned(TapePlayer) then begin
    if Sender <> Spectrum then
      AddEventToQueue(@ActionStopExecute)
    else begin
      TapePlayer.StopPlaying();
    end;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(Spectrum) then begin
    CanClose := False;
    Spectrum.OnSync := nil;
    //Application.ProcessMessages;
    DestroySpectrum;
    CanClose := True;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(Self);
  Application.RemoveAllHandlersOfObject(Self);
  if Assigned(FKeyboardOnScreen) then begin
    FKeyboardOnScreen.RemoveFreeNotification(Self);
    FreeAndNil(FKeyboardOnScreen);
  end;
  if Assigned(FTapeBrowser) then begin
    FTapeBrowser.RemoveFreeNotification(Self);
    FreeAndNil(FTapeBrowser);
  end;
  DestroySoundVolumeForm();
  FreeAndNil(HistoryQueue);

  DestroySpectrum;

  FreeAndNil(FormDebug);
  FreeTapePlayer;
  Bmp.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
var
  Sot: TSnapshotOrTape;

begin
  if Length(FileNames) > 0 then begin
    if Sender = FTapeBrowser then
      Sot := TSnapshotOrTape.stTape
    else
      Sot := TSnapshotOrTape.stBoth;
    TryLoadFromFiles(Sot, FileNames);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  AfterShow(2);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if FSoundVolumeForm = nil then begin
    if Sender <> Spectrum then
      AddEventToQueue(@SpeedButton1Click)
    else
      ShowSoundVolumeForm();
  end;
end;

procedure TForm1.DoChangeModel(Sender: TObject);
begin
  if FNewModel <> TSpectrumModel.smNone then begin
    if FNewModel <> Spectrum.SpectrumModel then begin
      Spectrum.SpectrumModel := FNewModel;
      UpdateActionsModel();
    end;
    FNewModel := TSpectrumModel.smNone;
  end;
end;

procedure TForm1.TryLoadFromFiles(const SnapshotOrTape: TSnapshotOrTape;
  const AFileNames: array of String);
var
  I, J: Integer;
  Extensions: TStringDynArray;
  S, FN, EFN: AnsiString;
begin
  if Length(AFileNames) > 0 then begin
    GetAcceptableExtensions(SnapshotOrTape, True, Extensions);
    for I := Low(AFileNames) to High(AFileNames) do begin
      FN := AFileNames[I];
      EFN := ExtractFileExt(FN);
      if EFN <> '' then begin
        for J := Low(Extensions) to High(Extensions) do begin
          S := Extensions[J];
          if S <> '' then begin
            if not S.StartsWith(ExtensionSeparator) then
              S := ExtensionSeparator + S;
            if AnsiCompareText(S, EFN) = 0 then begin
              DoLoad(SnapshotOrTape, FN);
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TForm1.MakeExtensionsFilter(ArSt: array of String): String;
var
  I, L: Integer;
  S1, Sa, Sb: String;
  AI: String;
  {$ifdef LCLGtk2}
  J: Integer;
  AIU: String;
  {$endif}
begin
  Result := '';

  S1 := '';
  Sa := '';
  Sb := '';

  L := 0;
  for I := Low(ArSt) to High(ArSt) do begin
    AI := LowerCase(Trim(ArSt[I]));
    if AI <> '' then begin

      {$ifdef LCLGtk2}
      AIU := UpperCase(AI);
      S1 := '';
      for J := 1 to Length(AI) do
        if AIU[J] <> AI[J] then
          S1 := S1 + '[' + AIU[J] + AI[J] + ']'
        else
          S1 := S1 + AI[J];
      {$else}
      S1 := AI;
      {$endif}

      S1 := '*' + ExtensionSeparator + S1;
      Result := Result + AI + '|' + S1 + '|';

      Inc(L);
      if L > 1 then begin
        Sa := Sa + ', ';
        Sb := Sb + ';';
      end;

      Sa := Sa + AI;
      Sb := Sb + S1;

    end;
           
  end;

  Result := Result + 'all files|*';

  if L > 1 then
    Result := Sa + '|' + Sb + '|' + Result;

end;

procedure TForm1.AfterShow(Data: PtrInt);
begin
  Self.AutoSize := False;
  TCommonFunctionsLCL.FormToScreenCentre(Self);
  if Data > 0 then begin
    UpdateScreenSizeFactor;
    Self.AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end else
    RunSpectrum;

end;

procedure TForm1.UpdateActionsModel;
var
  A: TContainedAction;
  I: Integer;
begin
  for I := 0 to ActionList1.ActionCount - 1 do begin
    A := ActionList1.Actions[I];
    if AnsiCompareText(A.Category, 'SpectrumModel') = 0 then begin
      if A is TCustomAction then begin
        TCustomAction(A).Checked := Ord(Spectrum.SpectrumModel) = A.Tag;
      end;
    end;
  end;
end;

procedure TForm1.UpdateShowCurrentlyActiveJoystick;
begin
  if TJoystick.Joystick.Enabled then
    Label2.Caption := 'Joystick: ' + TJoystick.Joystick.CurrentJoystickTypeAsString
  else
    Label2.Caption := 'Joystick disabled';
  ActionEnableJoystick.Checked := TJoystick.Joystick.Enabled;
end;

procedure TForm1.UpdateShowSound;
begin
  ActionMuteSound.Checked := Spectrum.SoundMuted;
  if Spectrum.SoundMuted then
    SpeedButton1.ImageIndex := 17
  else
    SpeedButton1.ImageIndex := 18;
  if Assigned(FSoundVolumeForm) then begin
    FSoundVolumeForm.SpeedButton1.ImageIndex := SpeedButton1.ImageIndex;
    FSoundVolumeForm.Muted := Spectrum.SoundMuted;
    FSoundVolumeForm.SpeedButton1.Update;
  end;
  SpeedButton1.Update;
end;

procedure TForm1.LoadFromConf;
var
  JObj: TJSONObject;
  M, N: Integer;
  SModel: TSpectrumModel;
  S, S1: String;
  SPortaudioLib32, SPortaudioLib64: String;
begin
  JObj := TConfJSON.GetJSONObject(cSection0);
  N := GetSoundVolume;
  if Assigned(JObj) then begin
    SetScreenSizeFactor(JObj.Get(cSectionScreenSizeFactor, Integer(1)));

    S := '';
    S := Trim(JObj.Get(cSectionSpectrumModel, S));
    if S <> '' then begin
      S := 'sm' + StringReplace(S, ' ', '_', [rfReplaceAll]);
      for SModel := Low(TSpectrumModel) to High(TSpectrumModel) do begin
        WriteStr(S1, SModel);
        if AnsiCompareText(S1, S) = 0 then begin
          FNewModel := SModel;
          Break;
        end;
      end;
    end;

    FLastFilePath := '';
    FLastFilePath := JObj.Get(cSectionLastFilePath, FLastFilePath);
    FSkipWriteScreen := JObj.Get(cSectionSkipWriteScr, Integer(0)) <> 0;
    UpdateCheckWriteScreen;

    Spectrum.SoundMuted := JObj.Get(cSectionSoundMuted, Integer(0)) <> 0;
    UpdateShowSound;
    SPortaudioLib64 := '';
    SPortaudioLib32 := Trim(JObj.Get(cSectionPortAudioLibPath32, SPortaudioLib64));
    SPortaudioLib64 := Trim(JObj.Get(cSectionPortAudioLibPath64, SPortaudioLib64));

    {$if SizeOf(SizeInt) = 4}
      FPortaudioLibPathOtherBitness := SPortaudioLib64;
      S := SPortaudioLib32;
    {$elseif SizeOf(SizeInt) = 8}
      FPortaudioLibPathOtherBitness := SPortaudioLib32;
      S := SPortaudioLib64;
    {$else}
      {$fatal platform not supported!}
    {$endif}

    if (S <> '') and (S <> Spectrum.PortAudioLibPath) then begin
      Spectrum.PortAudioLibPath := S;
    end;

    M := JObj.Get(cSectionSoundVolume, N);
    if (M >= 0) and (M <= 31) then
      N := M;
  end;
  SetSoundVolume(N);
end;

procedure TForm1.SaveToConf;
var
  JObj: TJSONObject;
  N: Integer;
  SPortaudioLib32, SPortaudioLib64: String;
  S: String;
begin
  if FSkipWriteScreen then
    N := 1
  else
    N := 0;
  JObj := TJSONObject.Create;
  try
    JObj.Add(cSectionSwanVersion, UnitVer.TVersion.FullVersionString);
    JObj.Add(cSectionScreenSizeFactor, ScreenSizeFactor);
    JObj.Add(cSectionSkipWriteScr, N);

    N := GetSoundVolume;
    JObj.Add(cSectionSoundVolume, N);

    if Spectrum.SoundMuted then
      N := 1
    else
      N := 0;
    JObj.Add(cSectionSoundMuted, N);

    WriteStr(S, Spectrum.SpectrumModel);
    if (Length(S) > 2) and (AnsiCompareText('sm', Copy(S, 1, 2)) = 0) then begin
      S := StringReplace(Copy(S, 3), '_', ' ', [rfReplaceAll]);
      JObj.Add(cSectionSpectrumModel, S);
    end;

    {$if SizeOf(SizeInt) = 4}
      SPortaudioLib64 := FPortaudioLibPathOtherBitness;
      SPortaudioLib32 := Spectrum.PortAudioLibPath;
    {$elseif SizeOf(SizeInt) = 8}
      SPortaudioLib32 := FPortaudioLibPathOtherBitness;
      SPortaudioLib64 := Spectrum.PortAudioLibPath;
    {$else}
      {$fatal platform not supported!}
    {$endif}

    if SPortaudioLib32 <> '' then begin
      JObj.Add(cSectionPortAudioLibPath32, SPortaudioLib32);
    end;
    if SPortaudioLib64 <> '' then begin
      JObj.Add(cSectionPortAudioLibPath64, SPortaudioLib64);
    end;

    if FLastFilePath <> '' then
      JObj.Add(cSectionLastFilePath, FLastFilePath);

    TConfJSON.RemoveSection(cSection0);
    if TConfJSON.AddToConf(cSection0, JObj) then
      JObj := nil;
  finally
    JObj.Free;
  end;
end;

function TForm1.GetSoundVolume: Integer;
begin
  Result := Spectrum.SoundVolume div 4;
end;

procedure TForm1.SetSoundVolume(N: Integer);
begin
  Spectrum.SoundVolume := N * 4;
end;

procedure TForm1.SetSnapshotHistoryEnabled(const B: Boolean);
begin
  if Assigned(HistoryQueue) xor B then begin
    if B then begin
      HistoryQueue := TSnapshotHistoryQueue.Create;
      HistoryQueue.Spectrum := Spectrum;
      HistoryQueue.UpdateOptions(SnapshotHistoryOptions);
    end else begin
      FreeAndNil(HistoryQueue);
    end;
    UpdateActiveSnapshotHistory();
  end;
end;

procedure TForm1.KeyFromFormKeyboardOnScreen(AKeyValue: Word; Flags: Integer);
begin
  AddKeyEventToQueue(AKeyValue, Flags);
end;

procedure TForm1.ReleaseShifts(Sender: TObject);
begin
  if Assigned(FKeyboardOnScreen) then
    FKeyboardOnScreen.ReleaseShifts;
end;

procedure TForm1.DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  I: Integer;
begin
  I := TJoystick.Joystick.CheckKey(Key);
  if I >= 0 then begin
    AddKeyEventToQueue(I, 3);
    Key := 0;
  end else begin
    I := Spectrum.KeyBoard.CheckKeyMap(Key);
    if I >= 0 then begin
      AddKeyEventToQueue(I, 1);
      Key := 0;
    end;
  end;
end;

procedure TForm1.DoOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  I: Integer;
begin
  I := TJoystick.Joystick.CheckKey(Key);
  if I >= 0 then begin
    AddKeyEventToQueue(I, 2);
    Key := 0;
  end else begin
    I := Spectrum.KeyBoard.CheckKeyMap(Key);
    if I >= 0 then begin
      AddKeyEventToQueue(I, 0);
      Key := 0;
    end;
  end;
end;

procedure TForm1.UpdateActiveSnapshotHistory;
begin
  ActionEnableHistory.Checked := Assigned(HistoryQueue);
  ActionMoveBack.Enabled := Assigned(HistoryQueue);
end;

procedure TForm1.UpdateTextTapeRunning;
const
  cTapeRunningMsg = 'Tape running...' + LineEnding + LineEnding
    + 'You chose the option which turns off updating the screen'
    + ' while tape is running. You can turn it off in'
    + ' main menu:' + LineEnding + 'Options -> '
    ;

var
  R: TRect;
  TS: TTextStyle;
  STapeRunningMsg: String;
begin
  if not FWriteScreen then begin
    Bmp.SetSize(
      PaintBox1.ClientWidth,
      PaintBox1.ClientHeight
      );
    Bmp.Canvas.GetUpdatedHandle([
      TCanvasStates.csHandleValid,
      TCanvasStates.csPenvalid,
      TCanvasStates.csBrushValid
      ]);

    Bmp.Canvas.Brush.Color := TColor($522200);
    Bmp.Canvas.Brush.Style := bsSolid;
    R := Rect(0, 0, Bmp.Canvas.Width, Bmp.Canvas.Height);
    R.Inflate(-(R.Width div 11), -(R.Height div 11));
    Bmp.Canvas.Clear;

    TS := Bmp.Canvas.TextStyle;
    TS.Layout := TTextLayout.tlCenter;
    TS.Alignment := TAlignment.taCenter;
    TS.Opaque := False;
    TS.SingleLine := False;
    TS.Wordbreak := True;

    Bmp.Canvas.Font.Height := MulDiv(GetFontData(Self.Font.Handle).Height, ScreenSizeFactor * 5 , 4);
    Bmp.Canvas.Font.Color := TColor($d17700);

    STapeRunningMsg := cTapeRunningMsg + ActionDontDrawScreenWhenLoading.Caption;

    Bmp.Canvas.Pen.Color := Bmp.Canvas.Font.Color;
    Bmp.Canvas.Pen.Width := 2;

    Bmp.Canvas.TextRect(R, R.Left, R.Top, STapeRunningMsg, TS);
    PaintBox1.Invalidate;
  end else
    if (Bmp.Width <> WholeScreenWidth) or (Bmp.Height <> WholeScreenHeight) then
      Bmp.SetSize(WholeScreenWidth, WholeScreenHeight);
end;

procedure TForm1.UpdateWriteScreen;
begin
  if FWriteScreen = (FSkipWriteScreen and Assigned(TapePlayer) and TapePlayer.IsPlaying) then begin
    FWriteScreen := not FWriteScreen;

    Spectrum.SetWriteScreen(FWriteScreen);
    UpdateTextTapeRunning;
  end;
end;

procedure TForm1.UpdateCheckWriteScreen;
begin
  ActionDontDrawScreenWhenLoading.Checked := FSkipWriteScreen;
  UpdateWriteScreen;
end;

procedure TForm1.AddKeyEventToQueue(KeyIndex: Integer; BDown: Integer);
var
  KeyEventRec: TKeyEventRec;
begin
  if Length(KeyEventQueue) <= KeyEventCount then
    SetLength(KeyEventQueue, (KeyEventCount * 7) div 5 + 2);
  KeyEventRec.KeyIndex := KeyIndex;
  KeyEventRec.BDown := BDown;
  KeyEventQueue[KeyEventCount] := KeyEventRec;
  Inc(KeyEventCount);
end;

procedure TForm1.AddEventToQueue(Event: TNotifyEvent);
begin
  if Length(EventsQueue) <= EventsQueueCount then
    SetLength(EventsQueue, (EventsQueueCount * 7) div 5 + 2);
  EventsQueue[EventsQueueCount] := Event;
  Inc(EventsQueueCount);
end;

procedure TForm1.PaintScreen(Sender: TObject);
begin
  PaintBox1.Canvas.StretchDraw(DrawingRect, Bmp);
end;

procedure TForm1.DoDetachDebugger(Sender: TObject);
begin
  FormDebug := nil;
end;

procedure TForm1.SaveSnapshot(SnapshotClass: TSnapshotFileClass);
var
  FileSnapshot: TSnapshotFile;
  WasPaused: Boolean;
  S: RawByteString;
begin
  if Spectrum.IsRunning then begin
    WasPaused := Spectrum.Paused;
    try                          
      Spectrum.Paused := True;
      SaveDialog1.DefaultExt := ExtensionSeparator + SnapshotClass.GetDefaultExtension;
      SaveDialog1.Filter :=
        SnapshotClass.GetDefaultExtension
        + '|*' + ExtensionSeparator + SnapshotClass.GetDefaultExtension
        + '|all files|' + '*';

      if FLastFilePath <> '' then begin
        S := ExtractFilePath(FLastFilePath);
        if DirectoryExists(S) then
          SaveDialog1.InitialDir := S;
      end;
      SaveDialog1.FileName := '';

      if SaveDialog1.Execute then begin
        FileSnapshot := SnapshotClass.Create;
        try
          FileSnapshot.SetSpectrum(Spectrum);
          if FileSnapshot.SaveToFile(SaveDialog1.FileName) then
            FLastFilePath := SaveDialog1.FileName;
        finally
          FileSnapshot.Free;
        end;
      end;

    finally
      Spectrum.Paused := WasPaused;
    end;

  end;
end;

procedure TForm1.SoundVolumeOnChg(Sender: TObject);
begin
  if Assigned(FSoundVolumeForm) then begin
    if Sender <> Spectrum then begin
      AddEventToQueue(@SoundVolumeOnChg);
    end else begin
      SetSoundVolume(FSoundVolumeForm.Level);
    end;
  end;
end;

procedure TForm1.SoundVolumeAfterShow(Data: PtrInt);
var
  P: TPoint;
begin
  if Assigned(FSoundVolumeForm) then begin
    P := SpeedButton1.ControlToScreen(
      Point(SpeedButton1.Width - FSoundVolumeForm.Width, SpeedButton1.Height)
      );
    FSoundVolumeForm.Left := P.X;

    P := SpeedButton1.ControlToScreen(
      Point(0, PanelStatus.Height)
      );
    FSoundVolumeForm.Top := P.Y - FSoundVolumeForm.Height;

    if Data > 0 then
      Application.QueueAsyncCall(@SoundVolumeAfterShow, Data - 1);
  end;
end;

procedure TForm1.SoundVolumeOnShow(Sender: TObject);
begin
  SoundVolumeAfterShow(1);
end;

procedure TForm1.EventPlayerOnChangeBlock(Sender: TObject);
begin
  if Assigned(TapePlayer) and TapePlayer.IsPlaying then
    Spectrum.SetTapePlayer(TapePlayer)
  else
    Spectrum.SetTapePlayer(nil);

  if Assigned(FTapeBrowser) then begin
    FTapeBrowser.UpdateCurrentBlockNumber(False);
  end;
  UpdateWriteScreen;
end;

procedure TForm1.PlayerOnChangeBlock;
begin
  AddEventToQueue(@EventPlayerOnChangeBlock);
end;

procedure TForm1.EventTapeBrowserGoToBlock(Sender: TObject);
begin
  if Assigned(TapePlayer) and Assigned(FTapeBrowser) then
    TapePlayer.GoToBlock(FTapeBrowser.GetBlockToGoTo());
end;

procedure TForm1.TapeBrowserGoToBlock;
begin
  AddEventToQueue(@EventTapeBrowserGoToBlock);
end;

procedure TForm1.TapeBrowserAttachTape;
begin
  if Assigned(FTapeBrowser) then
    FTapeBrowser.SetTapePlayer(TapePlayer);
  if Assigned(TapePlayer) then
    TapePlayer.OnChangeBlock := @PlayerOnChangeBlock;
end;

procedure TForm1.GetAcceptableExtensions(const SnapshotOrTape: TSnapshotOrTape;
  const IncludeZip: Boolean; out Extensions: TStringDynArray);
const
  SnapshotExtensions: array[0..2] of String = ('szx', 'z80', 'sna');
  TapeExtensions: array[0..2] of String = ('tap', 'tzx', 'pzx');
var
  I, L: Integer;
begin
  case SnapshotOrTape of
    stSnapshot:
      L := Length(SnapshotExtensions);
    stTape:
      L := Length(TapeExtensions);
  otherwise
    L := Length(SnapshotExtensions) + Length(TapeExtensions);
  end;
  if IncludeZip then
    Inc(L);
  SetLength(Extensions{%H-}, L);

  L := 0;
  if SnapshotOrTape in [stSnapshot, stBoth] then
    for I := Low(SnapshotExtensions) to High(SnapshotExtensions) do begin
      Extensions[L] := SnapshotExtensions[I];
      Inc(L);
    end;
  if SnapshotOrTape in [stTape, stBoth] then
    for I := Low(TapeExtensions) to High(TapeExtensions) do begin
      Extensions[L] := TapeExtensions[I];
      Inc(L);
    end;
  if IncludeZip then
    Extensions[L] := 'zip';
end;

procedure TForm1.LoadAsk(const SnapshotOrTape: TSnapshotOrTape);
var
  WasPaused: Boolean;
  L: Boolean;
  Extensions: TStringDynArray;
  S: String;
  I: Integer;

begin
  if Spectrum.IsRunning then begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;

      GetAcceptableExtensions(SnapshotOrTape, True, Extensions);
      OpenDialog1.FilterIndex := 1;
      OpenDialog1.Filter := MakeExtensionsFilter(Extensions);

      L := False;
      if FLastFilePath <> '' then begin
        S := ExtractFilePath(FLastFilePath);
        if DirectoryExists(S) then begin
          OpenDialog1.InitialDir := S;
          if FileExists(FLastFilePath) then begin
            S := ExtractFileExt(FLastFilePath);
            if S <> '' then begin
              if not S.StartsWith(ExtensionSeparator) then
                S := ExtensionSeparator + S;
              for I := Low(Extensions) to High(Extensions) do begin
                if AnsiCompareText(S, ExtensionSeparator + Extensions[I]) = 0 then begin
                  OpenDialog1.FileName := ExtractFileName(FLastFilePath);
                  L := True;
                  Break;
                end;
              end;
            end;
          end;
        end;
      end;
      if not L then
        OpenDialog1.FileName := '';

      if OpenDialog1.Execute then
        DoLoad(SnapshotOrTape, OpenDialog1.FileName);

    finally
      Spectrum.Paused := WasPaused;
    end;

  end;
end;

procedure TForm1.DoLoad(const SnapshotOrTape: TSnapshotOrTape;
  ASourceFile: String);

  procedure LoadingFailed;
  begin
    MessageDlg('Loading failed.' + LineEnding + 'Bad file?', mtError, [mbClose], 0);
  end;

var
  WasPaused: Boolean;
  Extension: String;
  FileName: String;
  Stream: TStream;
  SnapshotFile: TSnapshotFile;
  L: Boolean;
  TapeType: TTapeType;
  TapePlayerClass: TTapePlayerClass;
  AcceptedExtensions: TStringDynArray;

begin
  Stream := nil;
  if Spectrum.IsRunning then begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;

      Extension := ExtractFileExt(ASourceFile);
      
      if AnsiCompareText(Extension, ExtensionSeparator + 'zip') = 0 then begin
        GetAcceptableExtensions(SnapshotOrTape, False, AcceptedExtensions);
        if not TFileUnzipper.GetFileFromZipFile(ASourceFile, AcceptedExtensions, Stream, FileName) then begin
          Stream := nil;
        end else begin
          Extension := ExtractFileExt(FileName);
          FileName := IncludeTrailingPathDelimiter(ASourceFile) + FileName;
        end;
      end else
        try
          Stream := TFileStream.Create(ASourceFile, fmOpenRead or fmShareDenyWrite);
          FileName := ASourceFile;
        except
          Stream := nil;
        end;

      if Assigned(Stream) then begin
        try
          SnapshotFile := nil;
          if SnapshotOrTape in [stSnapshot, stBoth] then begin
            if (AnsiCompareText(Extension, ExtensionSeparator + 'szx') = 0)
              //or (AnsiCompareText(Extension, ExtensionSeparator + 'zx-state') = 0)
            then
              SnapshotFile := TSnapshotSZX.Create
            else if AnsiCompareText(Extension, ExtensionSeparator + 'z80') = 0 then
              SnapshotFile := TSnapshotZ80.Create
            else if AnsiCompareText(Extension, ExtensionSeparator + 'sna') = 0 then
              SnapshotFile := TSnapshotSNA.Create;
          end;

          L := False;
          if Assigned(SnapshotFile) then begin
            try
              SnapshotFile.SetSpectrum(Spectrum);

              Spectrum.ResetSpectrum;
              L := SnapshotFile.LoadFromStream(Stream);
              if not L then
                LoadingFailed;

            finally
              SnapshotFile.Free;
            end;
          end else if SnapshotOrTape in [stTape, stBoth] then begin
            FreeTapePlayer;

            TapePlayerClass := TTapePlayer.CheckRealTapePlayerClass(Stream);
            if TapePlayerClass = nil then begin
              TapeType := UnitTapePlayer.TTapeType.ttTap;
              if AnsiCompareText(Extension, ExtensionSeparator + 'tzx') = 0 then
                TapeType := UnitTapePlayer.TTapeType.ttTzx
              else if AnsiCompareText(Extension, ExtensionSeparator + 'pzx') = 0 then
                TapeType := UnitTapePlayer.TTapeType.ttPzx;

              TapePlayerClass := TTapePlayer.GetTapePlayerClassFromType(TapeType);
            end;
            if Assigned(TapePlayerClass) then begin
              TapePlayer := TapePlayerClass.Create;
              TapePlayer.FileName := FileName;

              try
                if TapePlayer.LoadFromStream(Stream) then begin
                  TapePlayer.SetSpectrum(Spectrum);
                  TapePlayer.Rewind;

                  TapeBrowserAttachTape;
                  L := True;
                end;

              finally
                if not L then begin
                  FreeTapePlayer;
                end;
              end;
            end;
            if not L then
              LoadingFailed;
          end;

          if L then
            FLastFilePath := ASourceFile;
        finally
          Stream.Free;
        end;
      end;

    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

procedure TForm1.RunSpectrum;
begin
  Spectrum.OnResetSpectrum := @DoOnResetSpectrum;
  Spectrum.OnEndRun := @SpectrumEndRun;
  Spectrum.OnStartRun := @SpectrumStartRun;

  Spectrum.OnSync := @SyncSpectrum;
  SetSpectrumSpeed(512);

  PrevTimeStop := 0;
  PrevTicks := 0;
  PrevPCTicks := GetTickCount64;

  KeyEventCount := 0;
  Self.OnKeyUp := @DoOnKeyUp;
  Self.OnKeyDown := @DoOnKeyDown;
  UpdateShowCurrentlyActiveJoystick;
  Spectrum.Start;

  UpdateShowSound;
  PaintBox1.OnPaint := @PaintScreen;
  Application.AddOnDeactivateHandler(@FormDeactivate);
end;

procedure TForm1.DoOnResetSpectrum;
begin
  PrevTimeStop := 0;
  PrevTicks := Spectrum.SumTicks;
  PrevPCTicks := GetTickCount64;
  KeyEventCount := 0;
end;

procedure TForm1.DestroySpectrum;
var
  Colours: TLCLColourMap;

begin
  PaintBox1.OnPaint := nil;
  Application.RemoveAllHandlersOfObject(Self);
  Self.OnKeyDown := nil;
  Self.OnKeyUp := nil;
  EventsQueueCount := 0;
  KeyEventCount := 0;
  SetLength(EventsQueue, 0);
  SetLength(KeyEventQueue, 0);
  FreeAndNil(HistoryQueue);
  if Spectrum <> nil then begin
    Spectrum.OnSync := nil;
    FreeTapePlayer;
    Spectrum.DettachDebugger;
    FreeAndNil(FormDebug);

    Spectrum.GetSpectrumColours(Colours);

    Spectrum.StopRunning;
    Application.ProcessMessages;
    Spectrum.WaitFor;
    FreeAndNil(Spectrum);

    UnitSpectrumColourMap.SetActiveColours(Colours);
  end
end;

procedure TForm1.SyncSpectrum;
var
  I: Integer;
  KRec: TKeyEventRec;
  K, Ticks, CurrentPCTicks: Int64;

begin
  if (EventsQueueCount > 0) or (KeyEventCount > 0) then begin
    // EventsQueueCount can change (to zero?) inside some action
    // So, do not use for loop, use while.
    I := 0;
    while I < EventsQueueCount do begin
      EventsQueue[I](Spectrum);
      Inc(I);
    end;
    EventsQueueCount := 0;

    I := 0;
    while I < KeyEventCount do begin
      KRec := KeyEventQueue[I];
      if KRec.BDown and 2 = 0 then
        Spectrum.KeyBoard.SetKeyState(KRec.KeyIndex, KRec.BDown and 1 <> 0)
      else if KRec.BDown and 4 = 0 then
        TJoystick.Joystick.SetState(KRec.KeyIndex, KRec.BDown and 1 <> 0, Spectrum.KeyBoard)
      else begin
        Spectrum.KeyBoard.SetKeyState(WordRec(LongRec(KRec.KeyIndex).Lo).Hi, WordRec(LongRec(KRec.KeyIndex).Lo).Lo, KRec.BDown and 1 <> 0);
        if KRec.BDown and 8 <> 0 then
          AddEventToQueue(@ReleaseShifts);
      end;

      Inc(I);
    end;
    KeyEventCount := 0;
  end;

  if FWriteScreen then begin
    Spectrum.DrawToCanvas(Bmp.Canvas);
    PaintBox1.Invalidate;
  end;

  Inc(PrevTimeStop);
  if PrevTimeStop > 10 then begin
    CurrentPCTicks := GetTickCount64;
    K := CurrentPCTicks - PrevPCTicks;
    if K > 1000 then begin
      Ticks := Spectrum.SumTicks;

      PrevPCTicks := CurrentPCTicks;
      PrevTimeStop := 0;
      if not Spectrum.Paused then begin
        K := K * 35;
        K := (Ticks - PrevTicks + (K shr 1)) div K;
        Label1.Caption := 'speed ' + IntToStr(K) + '%';
      end else
        Label1.Caption := 'paused';

      PrevTicks := Ticks;
      Label1.Update;
    end;
  end;

  if Assigned(HistoryQueue) then
    if not Spectrum.Paused then
      HistoryQueue.CheckSaveHistorySnapshot;
end;

procedure TForm1.SpectrumEndRun;
begin
  SaveToConf;
end;

procedure TForm1.SpectrumStartRun;
var
  I: Integer;
  Arr: TStringDynArray;
begin
  SetLength(Arr{%H-}, ParamCount);
  for I := 1 to ParamCount do
    Arr[I - 1] := ParamStr(I);
  TryLoadFromFiles(TSnapshotOrTape.stBoth, Arr);
end;

procedure TForm1.UpdateScreenSizeFactor;
begin
  PaintBox1.ClientWidth := WholeScreenWidth * ScreenSizeFactor;
  PaintBox1.ClientHeight := WholeScreenHeight * ScreenSizeFactor;
  DrawingRect := PaintBox1.ClientRect;

  UpdateTextTapeRunning;
end;

procedure TForm1.SetScreenSizeFactor(NewFactor: Integer);
begin
  if NewFactor < Low(TScreenSizeFactor) then
    NewFactor := Low(TScreenSizeFactor);
  if NewFactor > High(TScreenSizeFactor) then
    NewFactor := High(TScreenSizeFactor);
  if NewFactor <> ScreenSizeFactor then begin
    ScreenSizeFactor := NewFactor;
    ActionSizeDecrease.Enabled := ScreenSizeFactor > Low(TScreenSizeFactor);
    ActionSizeIncrease.Enabled := ScreenSizeFactor < High(TScreenSizeFactor);

    UpdateScreenSizeFactor;

    SetNewAutoSize(nil);
  end;
end;

procedure TForm1.SetSpectrumSpeed(NewSpeed: Integer);
begin
  Spectrum.Speed := NewSpeed;
  ActionSpeedDecrease.Enabled := {(Spectrum.Speed = 0) or} (Spectrum.Speed < TSpectrum.MinSpeed);
  ActionSpeedIncrease.Enabled := (Spectrum.Speed = 0) or (Spectrum.Speed > TSpectrum.MaxSpeed);

end;

procedure TForm1.ShowTapeBrowser();
var
  I, W: Integer;
begin
  if FTapeBrowser = nil then begin
    FTapeBrowser := TFormBrowseTape.Create(nil);
    FTapeBrowser.FreeNotification(Self);
    FTapeBrowser.AllowDropFiles := True;
    FTapeBrowser.OnDropFiles := Self.OnDropFiles;

    FTapeBrowser.OnGoToBlock := @TapeBrowserGoToBlock;

    W := 24;
    I := 0;
    while I < DataModuleImages.ImageList1.ResolutionCount do begin
      if DataModuleImages.ImageList1.ResolutionByIndex[I].Width + 3 >= W then begin
        W := DataModuleImages.ImageList1.ResolutionByIndex[I].Width;
        Break;
      end;

      Inc(I);
    end;

    FTapeBrowser.AddButtonActions(
      W,
      [
        ActionAttachTap,
        ActionEjectTape,
        nil, // divider
        ActionPlay,
        ActionStop,
        ActionRewind,
        ActionDecTapeBlock,
        ActionIncTapeBlock
      ]);
  end;

  TapeBrowserAttachTape;
  FTapeBrowser.Show;
end;

procedure TForm1.ShowKeyboardOnScreen;
begin
  if FKeyboardOnScreen = nil then begin
    FKeyboardOnScreen := TFormKeyboardOnScreen.ShowSpectrumKeyboardOnScreen();
    FKeyboardOnScreen.FreeNotification(Self);
    FKeyboardOnScreen.OnChgSpectrumKeyEx := @KeyFromFormKeyboardOnScreen;
    FKeyboardOnScreen.Show;
    FKeyboardOnScreen.OnKeyDown := @DoOnKeyDown;
    FKeyboardOnScreen.OnKeyUp := @DoOnKeyUp;
    FKeyboardOnScreen.OnDeactivate := @FormDeactivate;
    FKeyboardOnScreen.KeyPreview := True;
  end;
  FKeyboardOnScreen.BringToFront;
end;

procedure TForm1.ShowSoundVolumeForm;
begin
  if FSoundVolumeForm = nil then begin
    FSoundVolumeForm := TFormSoundVolume.ShowSoundVolumeTracker(
      Spectrum.SoundMuted, GetSoundVolume);
    FSoundVolumeForm.FreeNotification(Self);
  end;

  SpeedButton1.Enabled := False;
  FSoundVolumeForm.SpeedButton1.Images := SpeedButton1.Images;
  FSoundVolumeForm.SpeedButton1.ImageIndex := SpeedButton1.ImageIndex;

  FSoundVolumeForm.OnTrackBarPositionChg := @SoundVolumeOnChg;
  FSoundVolumeForm.OnMuteClick := @ActionMuteSoundExecute;
  SoundVolumeAfterShow(0);
  FSoundVolumeForm.OnShow := (@SoundVolumeOnShow);
  FSoundVolumeForm.Show;
end;

procedure TForm1.DestroySoundVolumeForm;
begin
  if Assigned(FSoundVolumeForm) then begin
    FSoundVolumeForm.RemoveAllHandlersOfObject(Self);
    FSoundVolumeForm.RemoveFreeNotification(Self);
    FSoundVolumeForm.OnShow := nil;
    FSoundVolumeForm.OnMuteClick := nil;
    FSoundVolumeForm.OnTrackBarPositionChg := nil;
    FreeAndNil(FSoundVolumeForm);
  end;
end;

procedure TForm1.FreeTapePlayer;
begin
  if Assigned(FTapeBrowser) then
    FTapeBrowser.SetTapePlayer(nil);

  if Assigned(Spectrum) then
    Spectrum.SetTapePlayer(nil);

  if Assigned(TapePlayer) then begin
    TapePlayer.OnChangeBlock := nil;
    TapePlayer.StopPlaying();
  end;

  FreeAndNil(TapePlayer);
end;

procedure TForm1.SetNewAutoSize(Sender: TObject);
begin
  AutoSize := False;
  if Sender <> Spectrum then begin
    AutoSize := True;
    AddEventToQueue(@SetNewAutoSize);
  end else
    PanelStatus.Update;
end;

procedure TForm1.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = TOperation.opRemove then begin
    if AComponent = FTapeBrowser then begin
      FTapeBrowser := nil;
    end else if AComponent = FKeyboardOnScreen then begin
      FKeyboardOnScreen.OnKeyUp := nil;
      FKeyboardOnScreen.OnKeyDown := nil;
      FKeyboardOnScreen.OnDeactivate := nil;
      FKeyboardOnScreen := nil;
      Spectrum.KeyBoard.ClearKeyboard;
      TJoystick.Joystick.ResetState;
    end else if AComponent = FSoundVolumeForm then begin
      FSoundVolumeForm := nil;
      SpeedButton1.Enabled := True;
    end;

  end;
end;

end.

