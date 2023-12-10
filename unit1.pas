unit unit1;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  ComCtrls, LCLType, Buttons, StdCtrls, ExtCtrls, fpjson, UnitSpectrum,
  UnitSnapshotFiles, AboutBox, DebugForm, UnitFormBrowser, UnitColourPalette,
  UnitSpectrumColourMap, CommonFunctionsLCL, UnitFrameKeyMappings, UnitJoystick,
  UnitFrameJoystickSetup, UnitDataModuleImages, unitSoundVolume, UnitConfigs,
  UnitInputLibraryPathDialog, UnitFormInputPokes, UnitHistorySnapshots, UnitSZX,
  UnitFormHistorySnapshots, UnitTapePlayer, UnitVer, UnitKeyboardOnScreen,
  UnitSoundPlayer, UnitOptions, UnitFrameSpectrumModel,
  UnitFrameSound, UnitFrameOtherOptions, UnitFrameHistorySnapshotOptions,
  UnitRecentFiles, UnitCommon, UnitCommonSpectrum, SnapshotZ80, SnapshotSNA,
  UnitFileZip, UnitRomPaths, UnitSwanToolbar;

// On Linux, bgra drawing directly to PaintBox in its OnPaint event seems to be
// extremly slow. However, we get better time when we have an auxiliary bitmap
// and let bgra draw to this bitmap instead, and then copy this bitmap to
// PaintBox in OnPaint.
// On Windows it is quicker to let bgra draw directly.
// Still, drawing screen remains much slower in Linux than in Windows, although
// this workaround with auxiliary bitmap makes real improvment.
{$ifdef unix}
  {$define UseАuxiliaryBmp}
{$endif}

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionHideToolbar: TAction;
    ActionLateTimings: TAction;
    ActionModelMoreOptions: TAction;
    ActionModelPlus2: TAction;
    ActionModel128K: TAction;
    ActionAllOptions: TAction;
    ActionModel16KIssue2: TAction;
    ActionModel16KIssue3: TAction;
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
    LabelSpeed: TLabel;
    LabelJoystick: TLabel;
    LabelModel: TLabel;
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
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItemRecentFiles: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelStatus: TPanel;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionAllOptionsExecute(Sender: TObject);
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
    procedure ActionLateTimingsExecute(Sender: TObject);
    procedure ActionModel128KExecute(Sender: TObject);
    procedure ActionModel16KIssue2Execute(Sender: TObject);
    procedure ActionModel16KIssue3Execute(Sender: TObject);
    procedure ActionModel48KIssue2Execute(Sender: TObject);
    procedure ActionModel48KIssue3Execute(Sender: TObject);
    procedure ActionModelMoreOptionsExecute(Sender: TObject);
    procedure ActionModelPlus2Execute(Sender: TObject);
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
    procedure ActionHideToolbarExecute(Sender: TObject);
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
    procedure ToolButton1ArrowClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  strict private
    const
      cSection0 = 'general';
      cSectionSkipWriteScr = 'skip_scr_upd_when_tape_playing';
      cSectionScreenSizeFactor = 'screen_size_factor';
      cSectionSoundVolume = 'sound_volume';
      cSectionSoundMuted = 'sound_muted';
      cSectionPortAudioLibPath32 = 'portaudio_lib_path32';
      cSectionPortAudioLibPath64 = 'portaudio_lib_path64';
      cSectionSpectrumModel = 'spectrum_model';
      cSectionVersion = 'version';
      cSectionOtherOptions = 'other_options';
      cSectionSkipJoystickInfoSzxLoad = 'skip_load_joystick_info_from_szx';
      cSectionAutoShowTapePlayer = 'auto_show_tape_player';
      cSectionSkipTapeInfoSzxLoad = 'skip_load_tape_info_from_szx';
      cSectionSzxSaveOptions = 'szx_save_options';
      cSectionRecentFiles = 'recent_files';
      cSectionBuildDate = 'build_date';
      cSectionCustomRomPaths = 'custom_rom_paths';
      cSectionLateTimings = 'late_timings';
      cSectionHideToolBar = 'hide_toolbar';
      cSectionDontAskForPortAudioPath = 'do_not_ask_for_portaudio';

  strict private
    FNewModel: TSpectrumModel;
    FFileToOpen: String;
    FAutoShowTapePlayerWhenTapeLoaded: Boolean;
    FInitiallyHideToolBar: Boolean;
    FDontAskPortAudioPath: Boolean;

    procedure SpectrumOnChangeModel();
    procedure DoChangeModel(Sender: TObject);
    procedure AfterShow(Data: PtrInt);
    procedure UpdateActionsModel();
    procedure UpdateShowCurrentlyActiveJoystick;
    procedure UpdateCheckPaused;
    procedure UpdateCheckLateTimings;
    procedure UpdateSoundControls;
    procedure LoadFromConf;
    procedure SaveToConf;
    procedure SetSnapshotHistoryEnabled(const B: Boolean);
    procedure ShowAllOptionsDialog(ControlClass: TControlClass);
    procedure UpdateRecentFiles;
    procedure RecentFilesOnClick(Sender: TObject);

    procedure KeyFromFormKeyboardOnScreen(AKeyValue: Word; Flags: Integer);
    procedure ReleaseShifts(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure DoOnKeyUp(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  strict private
    type
      TScreenSizeFactor = 1..4;

      TSnapshotOrTape = (stSnapshot, stTape, stBoth);

      TKeyEventRec = record
        KeyIndex: Integer;
        BDown: Integer;
      end;

      TDropFiles = class(TObject)
        SnapshotOrTape: TSnapshotOrTape;
        Filename: String;
      end;

      TSzxSaveTapeOptions = (sstoSkip, sstoEmbeddedCompressed,
        sstoEmbeddedUncompressed, sstoFilePathOnly);

  strict private
    FTapeBrowser: TFormBrowseTape;
    FKeyboardOnScreen: UnitKeyboardOnScreen.TFormKeyboardOnScreen;
    PrevTicks: Int64;
    PrevPCTicks: Int64;
    PrevTimeStop: Integer;
    DropFiles: TDropFiles;

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
    FRecentFiles: TRecentFiles;
    FPortaudioLibPathOtherBitness: RawByteString;
    HistoryQueue: TSnapshotHistoryQueue;
    SnapshotHistoryOptions: TSnapshotHistoryOptions;
    FToolBar: TSwanToolBar;

    procedure SetSzxSaveOptionsToSzx(const SzxSaveTapeOptions: TSzxSaveTapeOptions);
    procedure GetSzxSaveOptionsFromSzx(out SzxSaveTapeOptions: TSzxSaveTapeOptions);
    procedure SoundLibraryOnSave(var LibPath: String);
    function SoundLibraryDialogCheckLoad(const APath: String): Boolean;
    procedure TryLoadFromFiles(const SnapshotOrTape: TSnapshotOrTape; const AFileNames: Array of String);
    procedure DropFilesLoad(Sender: TObject);
    procedure UpdateActiveSnapshotHistory;
    procedure UpdateTextTapeRunning;
    procedure UpdateWriteScreen;
    procedure UpdateCheckWriteScreen;
    procedure UpdateCheckHideToolbar;
    procedure AddKeyEventToQueue(KeyIndex: Integer; BDown: Integer);
    procedure AddEventToQueue(Event: TNotifyEvent);
    {$ifNdef UseАuxiliaryBmp}
    procedure PaintScreen(Sender: TObject);
    {$endif}
    procedure PaintBmp(Sender: TObject);
    procedure DoDetachDebugger(Sender: TObject);
    procedure SaveSnapshot(SnapshotClass: TSnapshotFileClass);
    procedure SoundVolumeOnChg(Sender: TObject);
    procedure SoundVolumeAfterShow(Data: PtrInt);
    procedure SoundVolumeOnShow(Sender: TObject);
    procedure ToolButtonOnShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure EventPlayerOnChangeBlock(Sender: TObject);
    procedure PlayerOnChangeBlock;
    procedure EventTapeBrowserGoToBlock(Sender: TObject);
    procedure TapeBrowserGoToBlock();
    procedure TapeBrowserAttachTape;
    procedure GetAcceptableExtensions(const SnapshotOrTape: TSnapshotOrTape; const IncludeZip: Boolean; out Extensions: TStringDynArray);
    procedure LoadAsk(const SnapshotOrTape: TSnapshotOrTape);
    procedure DoLoad(const SnapshotOrTape: TSnapshotOrTape; ASourceFile: String);
    function LoadTape(const Stream: TStream; const FileName: String;
      Extension: String): Boolean;
    procedure SzxOnLoadTape(AStream: TStream; const AFileName: String;
        const AExtension: String; ACurrentBlock: Integer);
    procedure SzxOnSaveTape(out ATapePlayer: TTapePlayer);
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
    procedure ShowSoundVolumeForm(const AToggleMute: Boolean);
    procedure ShowOrHideToolbar(AShow: Boolean);
    procedure DestroySoundVolumeForm();
    procedure ShowMessageSoundLibNotLoaded(Sender: TObject);
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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAutoShowTapePlayerWhenTapeLoaded := True;
  FNewModel := TSpectrumModel.smNone;
  HistoryQueue := nil;
  FFileToOpen := '';
  FDontAskPortAudioPath := False;

  ActionAbout.Caption := 'About ' + ApplicationName + '...';
  ActionEnableHistory.Hint :=
    'Enable going through emulation back in time step by step';

  ActionHistorySnapshots.Hint := ActionHistorySnapshots.Caption + LineEnding
    + '(' + ActionEnableHistory.Hint + ')';

  ActionMoveBack.ShortCut := SnapshotHistoryOptions.KeyGoBack;

  ActionModel16KIssue2.Tag := Ord(TSpectrumModel.sm16K_issue_2);
  ActionModel16KIssue3.Tag := Ord(TSpectrumModel.sm16K_issue_3);
  ActionModel48KIssue2.Tag := Ord(TSpectrumModel.sm48K_issue_2);
  ActionModel48KIssue3.Tag := Ord(TSpectrumModel.sm48K_issue_3);
  ActionModel128K.Tag := Ord(TSpectrumModel.sm128K);
  ActionModelPlus2.Tag := Ord(TSpectrumModel.smPlus2);

  UpdateActiveSnapshotHistory;
  FToolBar := nil;
  FInitiallyHideToolBar := False;

  FRecentFiles := TRecentFiles.Create;
  UpdateRecentFiles;

  FPortaudioLibPathOtherBitness := '';
  LabelJoystick.Caption := ' ';
  LabelSpeed.Caption := ' ';
  LabelModel.Caption := ' ';

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

  AutoSize := False;

  {$ifdef UseАuxiliaryBmp}
  Bmp := TBitmap.Create;
  {$else}
  Bmp := nil;
  {$endif}

  DropFiles := nil;
  DestroySpectrum;
  Spectrum := TSpectrum.Create;
  ScreenSizeFactor := 1;
  Spectrum.OnChangeModel := @SpectrumOnChangeModel;
  TSnapshotSZX.OnSzxLoadTape := @SzxOnLoadTape;
  TSnapshotSZX.OnSzxSaveTape := @SzxOnSaveTape;

  LoadFromConf;
  TSoundPlayer.TryLoadLib;
  if FNewModel = TSpectrumModel.smNone then
    FNewModel := TSpectrum.DefaultSpectrumModel;
  DoChangeModel(nil);
  if ScreenSizeFactor = 1 then begin
    ScreenSizeFactor := 2;
    SetScreenSizeFactor(1);
  end;
  UpdateCheckWriteScreen;
  //
  ShowOrHideToolbar(not FInitiallyHideToolBar);
  UpdateCheckHideToolbar;

  TCommonFunctionsLCL.FormToScreenCentre(Self);
  ToolButton1.OnShowHint := @ToolButtonOnShowHint;
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

      if UnitColourPalette.TFrameColourPalette.PickColours(Colours) then begin
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
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionAboutExecute);
  end else begin
    TFormAbout.ShowAbout();
  end;
end;

procedure TForm1.ActionAllOptionsExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionAllOptionsExecute);
  end else begin
    ShowAllOptionsDialog(TFrameOtherOptions);
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
        TCommonSpectrum.KB16,
        TCommonSpectrum.KB64 - 1,
        Pokes
        //Spectrum.GetProcessor.GetMemory^.RomSize,
        //Spectrum.GetProcessor.GetMemory^.MemSize - 1, Pokes
        )
      then begin
        for I := Low(Pokes) to High(Pokes) do begin
          PE := Pokes[I];
          Spectrum.Memory.WriteByte(PE.Adr, PE.Value);
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

      TJoystick.Joystick.GetKeys(AKeys);
      JoystickType := TJoystick.Joystick.JoystickType;
      JoystickEnabled := TJoystick.Joystick.Enabled;
      if TFrameJoystickSetup.ShowJoystickOptionsDialog(JoystickType, AKeys, JoystickEnabled) then begin
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

      if TFrameKeyMappings.ShowFormKeyMappings() then begin
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

procedure TForm1.ActionLateTimingsExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionLateTimingsExecute);
  end else begin
    Spectrum.LateTimings := not Spectrum.LateTimings;
    UpdateCheckLateTimings;
  end;
end;

procedure TForm1.ActionModel128KExecute(Sender: TObject);
begin
  FNewModel := TSpectrumModel.sm128K;
  AddEventToQueue(@DoChangeModel);
end;

procedure TForm1.ActionModel16KIssue2Execute(Sender: TObject);
begin
  FNewModel := TSpectrumModel.sm16K_issue_2;
  AddEventToQueue(@DoChangeModel);
end;

procedure TForm1.ActionModel16KIssue3Execute(Sender: TObject);
begin
  FNewModel := TSpectrumModel.sm16K_issue_3;
  AddEventToQueue(@DoChangeModel);
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

procedure TForm1.ActionModelMoreOptionsExecute(Sender: TObject);
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionModelMoreOptionsExecute);
  end else begin
    ShowAllOptionsDialog(TFrameSpectrumModel);
  end;
end;

procedure TForm1.ActionModelPlus2Execute(Sender: TObject);
begin
  FNewModel := TSpectrumModel.smPlus2;
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
    UpdateSoundControls;
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
    UpdateCheckPaused;
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
  WasPaused: Boolean;
begin
  if Sender <> Spectrum then begin
    AddEventToQueue(@ActionPortAudioLibPathExecute);
  end else begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;
      TFrameInputLibraryPath.ShowLibraryPathDialog(
        TSoundPlayer.LibPath, @SoundLibraryDialogCheckLoad, @SoundLibraryOnSave);
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

procedure TForm1.ActionHideToolbarExecute(Sender: TObject);
begin
  if Sender <> Spectrum then
    AddEventToQueue(@ActionHideToolbarExecute)
  else
    ShowOrHideToolbar(FToolBar = nil);
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
  FreeAndNil(DropFiles);
  if Assigned(FTapeBrowser) then begin
    FTapeBrowser.RemoveFreeNotification(Self);
    FreeAndNil(FTapeBrowser);
  end;
  FreeAndNil(FToolBar);
  DestroySoundVolumeForm();
  FreeAndNil(HistoryQueue);

  DestroySpectrum;

  MenuItemRecentFiles.Clear;
  TSnapshotSZX.OnSzxLoadTape := nil;
  FreeAndNil(FormDebug);
  FreeTapePlayer;
  Bmp.Free;
  FRecentFiles.Free;
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

    if DropFiles = nil then begin
      DropFiles := TDropFiles.Create;
      DropFiles.Filename := '';
    end;
    TryLoadFromFiles(Sot, FileNames);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  AfterShow(2);
end;

procedure TForm1.ToolButton1ArrowClick(Sender: TObject);
begin
  if FSoundVolumeForm = nil then begin
    if Sender <> Spectrum then
      AddEventToQueue(@ToolButton1ArrowClick)
    else begin
      ShowSoundVolumeForm(False);
    end;
  end;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  if FSoundVolumeForm = nil then begin
    if Sender <> Spectrum then
      AddEventToQueue(@ToolButton1Click)
    else begin
      ShowSoundVolumeForm(True);
    end;
  end;
end;

procedure TForm1.SpectrumOnChangeModel;
var
  S: String;
begin
  UpdateActionsModel();
  WriteStr(S, Spectrum.SpectrumModel);
  S := StringReplace(Copy(S, 3), '_', ' ', [rfReplaceAll]);
  LabelModel.Caption := 'Spectrum ' + StringReplace(S, 'plus', '+', [rfIgnoreCase]) + ' ';
end;

procedure TForm1.DoChangeModel(Sender: TObject);
begin
  if FNewModel <> TSpectrumModel.smNone then begin
    if FNewModel <> Spectrum.SpectrumModel then begin
      Spectrum.SetSpectrumModel(FNewModel, nil);
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
            if not S.StartsWith(ExtensionSeparator, True) then
              S := ExtensionSeparator + S;
            if AnsiCompareText(S, EFN) = 0 then begin
              if Assigned(DropFiles) then begin
                DropFiles.SnapshotOrTape := SnapshotOrTape;
                if DropFiles.Filename = '' then begin
                  AddEventToQueue(@DropFilesLoad);
                end;
                DropFiles.Filename := FN;
              end else
                DoLoad(SnapshotOrTape, FN);

              Exit;
            end;
          end;
        end;
      end;
    end;
  end;

  if Assigned(DropFiles) and (DropFiles.Filename = '') then
    FreeAndNil(DropFiles);
end;

procedure TForm1.DropFilesLoad(Sender: TObject);
begin
  if Assigned(DropFiles) then begin
    if (DropFiles.Filename <> '') then
      DoLoad(DropFiles.SnapshotOrTape, DropFiles.Filename);

    FreeAndNil(DropFiles);
  end;
end;

procedure TForm1.AfterShow(Data: PtrInt);
begin
  Self.AutoSize := False;
  TCommonFunctionsLCL.FormToScreenCentre(Self);
  if Data > 0 then begin
    UpdateScreenSizeFactor;
    Self.AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end else begin
    Panel4.BringToFront;
    LabelSpeed.Constraints.MinWidth := PanelStatus.Canvas.GetTextWidth('Wspeed 100%');
    LabelJoystick.Constraints.MinWidth := PanelStatus.Canvas.GetTextWidth('WJoystick: Interface II right');
    LabelModel.Constraints.MinWidth := PanelStatus.Canvas.GetTextWidth('W48K issue 2');
    RunSpectrum;
  end;

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
    LabelJoystick.Caption := 'Joystick: ' + TJoystick.Joystick.CurrentJoystickTypeAsString
  else
    LabelJoystick.Caption := 'Joystick disabled';
  ActionEnableJoystick.Checked := TJoystick.Joystick.Enabled;
end;

procedure TForm1.UpdateCheckPaused;
begin
  ActionPause.Checked := Spectrum.Paused;
end;

procedure TForm1.UpdateCheckLateTimings;
begin
  ActionLateTimings.Checked := Spectrum.LateTimings;
end;

procedure TForm1.UpdateSoundControls;
var
  SoundAllowed: Boolean;
  SoundIsMuted: Boolean;
begin
  SoundAllowed := TSoundPlayer.IsLibLoaded and Assigned(Spectrum);
  SoundIsMuted := (not SoundAllowed) or Spectrum.SoundMuted;
  ActionMuteSound.Checked := SoundIsMuted;

  if SoundIsMuted then
    ToolButton1.ImageIndex := 32
  else
    ToolButton1.ImageIndex := 33;

  if not SoundAllowed then
    DestroySoundVolumeForm()
  else if Assigned(FSoundVolumeForm) then begin
    FSoundVolumeForm.ToolButton1.ImageIndex := ToolButton1.ImageIndex;
    FSoundVolumeForm.ToolButton1.Update;
  end;

  ActionMuteSound.Enabled := SoundAllowed;
  ToolButton1.Enabled := SoundAllowed;
  ToolBar1.Enabled := SoundAllowed and (FSoundVolumeForm = nil);
  ToolButton1.Update;
end;

procedure TForm1.LoadFromConf;

  function UnpackVersionString(S: String): DWord;
  var
    P: Integer;
    N: Integer;
    S1: String;
    I, K: Integer;
    D: DWord;
  begin
    D := 0;
    Result := 0;

    N := 1;
    I := 0;
    repeat
      P := Pos('.', S, N);
      if P > 0 then begin
        if I >= 2 then
          Exit;

        S1 := Copy(S, N, P - N);
        N := P + 1;

      end else
        S1 := Copy(S, N);

      if (not TCommonFunctions.TryStrToIntDecimal(S1, K))
         or (K < 0) or ((I > 0) and (K > 99))
      then
        Exit;

      Inc(I);

      D := D * 100 + DWord(K);

    until P <= 0;

    case I of
      2:
        Result := D * 100;
      3:
        Result := D;
    otherwise
    end;

  end;

const
  cOldSectionSwanVersion = 'swan_version';

var
  JObj: TJSONObject;
  JObj2: TJSONObject;
  JD, JVer: TJSONData;

  M: Integer;
  SoundVol: Integer;
  SModel: TSpectrumModel;
  S, S1: String;
  SPortaudioLib32, SPortaudioLib64: String;
  K: Integer;
  SzxSaveTapeOptions: TSzxSaveTapeOptions;

  FullVersionFromConf: DWord;

begin
  JObj := TConfJSON.GetJSONObject(cSection0);
  SoundVol := TSoundPlayer.Volume;
  if Assigned(JObj) then begin
    SetScreenSizeFactor(JObj.Get(cSectionScreenSizeFactor, Integer(1)));

    FullVersionFromConf := 0;

    JD := JObj.Extract(cOldSectionSwanVersion);
    JVer := JObj.Find(cSectionVersion);
    if not Assigned(JVer) then begin
      if Assigned(JD) then begin
        JObj.Add(cSectionVersion, JD);
        JVer := JD;
        JD := nil;
      end;
    end;
    JD.Free;

    // Full version which saved the conf. We can use it in this procedure when needed.
    // if needed, we can compare it with current TVersion.FullVersion
    if not Assigned(JVer) then begin
      // before 0.9.4, Swan didn't save version in conf.
      if TConfJSON.Possible092Conf then begin
        FullVersionFromConf := 902; // might as well be 0.9.0, never mind.
      end;

    end else
      if JVer is TJSONString then
        FullVersionFromConf := UnpackVersionString(JVer.AsString);

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

    JD := JObj.Find(cSectionOtherOptions);
    if JD is TJSONObject then begin
      JObj2 := TJSONObject(JD);
      if TSnapshotSZX.SkipJoystickInfoLoad then
        K := 1
      else
        K := 0;
      TSnapshotSZX.SkipJoystickInfoLoad := JObj2.Get(cSectionSkipJoystickInfoSzxLoad, K) <> 0;

      if FAutoShowTapePlayerWhenTapeLoaded then
        K := 1
      else
        K := 0;
      FAutoShowTapePlayerWhenTapeLoaded := JObj2.Get(cSectionAutoShowTapePlayer, K) <> 0;

      if Assigned(TSnapshotSZX.OnSzxLoadTape) then
        K := 0
      else
        K := 1;
      if JObj2.Get(cSectionSkipTapeInfoSzxLoad, K) = 0 then
        TSnapshotSZX.OnSzxLoadTape := @SzxOnLoadTape
      else
        TSnapshotSZX.OnSzxLoadTape := nil;

      GetSzxSaveOptionsFromSzx(SzxSaveTapeOptions);
      K := Integer(SzxSaveTapeOptions);
      K := JObj2.Get(cSectionSzxSaveOptions, K);
      if (K >= Integer(Low(TSzxSaveTapeOptions)))
         and (K <= Integer(High(TSzxSaveTapeOptions)))
      then begin
        SzxSaveTapeOptions := TSzxSaveTapeOptions(K);
        SetSzxSaveOptionsToSzx(SzxSaveTapeOptions);
      end;
    end;

    JD := JObj.Find(cSectionRecentFiles);
    if JD is TJSONArray then begin
      FRecentFiles.LoadFromJSONArray(TJSONArray(JD));
      UpdateRecentFiles;
    end;

    JD := JObj.Find(cSectionCustomRomPaths);
    if JD is TJSONObject then
      TRomPaths.GetRomPaths.LoadFromJSON(TJSONObject(JD));

    FSkipWriteScreen := JObj.Get(cSectionSkipWriteScr, Integer(0)) <> 0;
    UpdateCheckWriteScreen;

    Spectrum.SoundMuted := JObj.Get(cSectionSoundMuted, Integer(0)) <> 0;
    Spectrum.LateTimings := JObj.Get(cSectionLateTimings, Integer(0)) <> 0;
    UpdateCheckLateTimings;

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

    TSoundPlayer.LibPath := S;

    M := JObj.Get(cSectionSoundVolume, SoundVol);

    if (M >= 0) and (M <= 127) then begin
      SoundVol := M;

      // Since 0.9.7, volume is lower... let's raise what was saved when loading from older conf.
      if (FullVersionFromConf > 0) and (FullVersionFromConf <= 906) and (M <= 31) then begin
        SoundVol := 1024;

        while M < 31 do begin
          Inc(M);
          SoundVol := (SoundVol * 800) div 729;
        end;

        SoundVol := 127 - (((SoundVol shr 10) - 1) * 127) div 16;
      end;
    end;

    FInitiallyHideToolBar := JObj.Get(cSectionHideToolBar, Integer(0)) <> 0;
    FDontAskPortAudioPath := JObj.Get(cSectionDontAskForPortAudioPath, Integer(0)) <> 0;
  end;
  TSoundPlayer.Volume := SoundVol;
end;

procedure TForm1.SaveToConf;
var
  JObj: TJSONObject;
  N: Integer;
  SPortaudioLib32, SPortaudioLib64: String;
  S: String;
  K: Integer;
  JObj2: TJSONObject;
  SzxSaveTapeOptions: TSzxSaveTapeOptions;
  JArr: TJSONArray;

begin
  JObj := TJSONObject.Create;
  try
    JObj.Add(cSectionVersion, UnitVer.TVersion.FullVersionString);
    JObj.Add(cSectionBuildDate, TCommonSpectrum.BuildDateString);
    JObj.Add(cSectionScreenSizeFactor, Integer(ScreenSizeFactor));
    if FSkipWriteScreen then
      N := 1
    else
      N := 0;
    JObj.Add(cSectionSkipWriteScr, N);

    N := TSoundPlayer.Volume;
    JObj.Add(cSectionSoundVolume, N);

    if Spectrum.SoundMuted then
      N := 1
    else
      N := 0;
    JObj.Add(cSectionSoundMuted, N);

    if Spectrum.BkpSpectrumModel = smNone then
      Spectrum.BkpSpectrumModel := Spectrum.SpectrumModel;
    WriteStr(S, Spectrum.BkpSpectrumModel);
    if (Length(S) > 2) and (AnsiCompareText('sm', Copy(S, 1, 2)) = 0) then begin
      S := StringReplace(Copy(S, 3), '_', ' ', [rfReplaceAll]);
      JObj.Add(cSectionSpectrumModel, S);
    end;

    if Spectrum.LateTimings then
      N := 1
    else
      N := 0;
    JObj.Add(cSectionLateTimings, N);

    if FToolBar = nil then
      N := 1
    else
      N := 0;
    JObj.Add(cSectionHideToolBar, N);

    if FDontAskPortAudioPath then
      N := 1
    else
      N := 0;
    JObj.Add(cSectionDontAskForPortAudioPath, N);

    {$if SizeOf(SizeInt) = 4}
      SPortaudioLib64 := FPortaudioLibPathOtherBitness;
      SPortaudioLib32 := TSoundPlayer.LibPath;
    {$elseif SizeOf(SizeInt) = 8}
      SPortaudioLib32 := FPortaudioLibPathOtherBitness;
      SPortaudioLib64 := TSoundPlayer.LibPath;
    {$else}
      {$fatal platform not supported!}
    {$endif}

    if SPortaudioLib32 <> '' then begin
      JObj.Add(cSectionPortAudioLibPath32, SPortaudioLib32);
    end;
    if SPortaudioLib64 <> '' then begin
      JObj.Add(cSectionPortAudioLibPath64, SPortaudioLib64);
    end;

    FRecentFiles.SaveToJSONArray(JArr);
    if Assigned(JArr) then
      JObj.Add(cSectionRecentFiles, JArr);

    JObj2 := TJSONObject.Create;
    try
      if TSnapshotSZX.SkipJoystickInfoLoad then
        K := 1
      else
        K := 0;
      JObj2.Add(cSectionSkipJoystickInfoSzxLoad, K);

      if FAutoShowTapePlayerWhenTapeLoaded then
        K := 1
      else
        K := 0;
      JObj2.Add(cSectionAutoShowTapePlayer, K);

      if Assigned(TSnapshotSZX.OnSzxLoadTape) then
        K := 0
      else
        K := 1;
      JObj2.Add(cSectionSkipTapeInfoSzxLoad, K);

      GetSzxSaveOptionsFromSzx(SzxSaveTapeOptions);
      K := Integer(SzxSaveTapeOptions);
      JObj2.Add(cSectionSzxSaveOptions, K);

      if JObj.Add(cSectionOtherOptions, JObj2) >= 0 then
        JObj2 := nil;
    finally
      JObj2.Free;
    end;

    if TRomPaths.RomPathsAssigned then begin
      JObj2 := TRomPaths.GetRomPaths.SaveToJSon;
      if Assigned(JObj2) then begin
        try
          if JObj.Add(cSectionCustomRomPaths, JObj2) >= 0 then
            JObj2 := nil;
        finally
          JObj2.Free;
        end;
      end;
    end;

    TConfJSON.RemoveSection(cSection0);
    if TConfJSON.AddToConf(cSection0, JObj) then
      JObj := nil;
  finally
    JObj.Free;
  end;
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

procedure TForm1.ShowAllOptionsDialog(ControlClass: TControlClass);
var
  WasPaused: Boolean;
  OptionsDialog: TFormOptions;
  FrameKeyMappings: TFrameKeyMappings;
  FrameColourPalette: TFrameColourPalette;
  Colours: TLCLColourMap;
  FrameJoystickSetup: TFrameJoystickSetup;
  JoystickEnabled: Boolean;
  JoystickType: TJoystick.TJoystickType;
  AKeys: TJoystick.TJoystickDirectionsKeys;
  FrameSpectrumModel: TFrameSpectrumModel;
  FrameSound: TFrameSound;
  FrameSoundLib: TFrameInputLibraryPath;
  FrameOtherOptions: TFrameOtherOptions;
  FrameHistorySnapshotOptions: TFrameHistorySnapshotOptions;
  SzxSaveTapeOptions: TSzxSaveTapeOptions;

begin
  WasPaused := Spectrum.Paused;
  try
    Spectrum.Paused := True;

    KeyEventCount := 0;
    Spectrum.KeyBoard.ClearKeyboard;
    TJoystick.Joystick.ResetState;

    OptionsDialog := TFormOptions.CreateOptionsDialog([]);
    if Assigned(OptionsDialog) then
      try
        repeat // this never loops, but allows break
          FrameHistorySnapshotOptions :=
            TFrameHistorySnapshotOptions.CreateForAllOptions(OptionsDialog);
          if not Assigned(FrameHistorySnapshotOptions) then
            Break;
          FrameHistorySnapshotOptions.HistoryEnabled := Assigned(HistoryQueue);
          FrameHistorySnapshotOptions.UpdateValuesFromHistoryOptions(
            SnapshotHistoryOptions);

          FrameColourPalette := TFrameColourPalette.CreateForOptionsDialog(OptionsDialog);
          if not Assigned(FrameColourPalette) then
            Break;
          Spectrum.GetSpectrumColours(Colours);
          FrameColourPalette.LCLColours := Colours;

          TJoystick.Joystick.GetKeys(AKeys);
          FrameJoystickSetup := TFrameJoystickSetup.CreateForAllOptions(
            OptionsDialog, TJoystick.Joystick.JoystickType, AKeys, TJoystick.Joystick.Enabled);
          if not Assigned(FrameJoystickSetup) then
            Break;

          FrameKeyMappings := TFrameKeyMappings.CreateFrameKeyMappingsForAllOptions(OptionsDialog);
          if not Assigned(FrameKeyMappings) then
            Break;
          FrameKeyMappings.ParentColor := False;

          FrameSpectrumModel := TFrameSpectrumModel.CreateForAllOptions(
            OptionsDialog, Spectrum, TRomPaths.GetRomPaths);
          if not Assigned(FrameSpectrumModel) then
            Break;

          FrameSoundLib := TFrameInputLibraryPath.CreateLibraryPathDialog(
            OptionsDialog, TSoundPlayer.LibPath, @SoundLibraryDialogCheckLoad,
            @SoundLibraryOnSave);
          FrameSoundLib.AddFormEvents(OptionsDialog);
          FrameSound := TFrameSound.CreateForAllOptions(OptionsDialog, Spectrum,
            FrameSoundLib);
          if not Assigned(FrameSound) then
            Break;
          FrameSound.VolLevel := TSoundPlayer.Volume;

          FrameOtherOptions := TFrameOtherOptions.CreateForAllOptions(OptionsDialog);
          if not Assigned(FrameOtherOptions) then
            Break;
          FrameOtherOptions.AutoShowTapePlayerOnLoadTape := FAutoShowTapePlayerWhenTapeLoaded;
          FrameOtherOptions.SkipJoystickInfoSzxLoad := TSnapshotSZX.SkipJoystickInfoLoad;
          FrameOtherOptions.SkipTapeInfoSzxLoad := not Assigned(TSnapshotSZX.OnSzxLoadTape);
          GetSzxSaveOptionsFromSzx(SzxSaveTapeOptions);
          FrameOtherOptions.SaveTapeInfoSzxSave := Integer(SzxSaveTapeOptions);

          if ControlClass = nil then
            ControlClass := TFrameOtherOptions;
          OptionsDialog.SetCurrentControlByClass(ControlClass);

          if OptionsDialog.ShowModal = mrOK then begin
            Spectrum.SetSpectrumColours(FrameColourPalette.LCLColours);
            Spectrum.KeyBoard.LoadFromKeyMappings;
            TSoundPlayer.Volume := FrameSound.VolLevel;
            FrameJoystickSetup.GetJoystickSetup(JoystickType, AKeys, JoystickEnabled);
            TJoystick.Joystick.SetKeys(AKeys);
            TJoystick.Joystick.Enabled := JoystickEnabled;
            TJoystick.Joystick.JoystickType := JoystickType;
            UpdateShowCurrentlyActiveJoystick;
            UpdateCheckLateTimings;
            FAutoShowTapePlayerWhenTapeLoaded := FrameOtherOptions.AutoShowTapePlayerOnLoadTape;
            TSnapshotSZX.SkipJoystickInfoLoad := FrameOtherOptions.SkipJoystickInfoSzxLoad;
            if FrameOtherOptions.SkipTapeInfoSzxLoad then
              TSnapshotSZX.OnSzxLoadTape := nil
            else
              TSnapshotSZX.OnSzxLoadTape := @SzxOnLoadTape;
            SzxSaveTapeOptions := TSzxSaveTapeOptions(FrameOtherOptions.SaveTapeInfoSzxSave);
            SetSzxSaveOptionsToSzx(SzxSaveTapeOptions);

            FrameHistorySnapshotOptions.UpdateSnapshotHistoryOptionsFromValues(SnapshotHistoryOptions);
            ActionMoveBack.ShortCut := SnapshotHistoryOptions.KeyGoBack;
            if Assigned(HistoryQueue) xor FrameHistorySnapshotOptions.HistoryEnabled then
              SetSnapshotHistoryEnabled(FrameHistorySnapshotOptions.HistoryEnabled)
            else
              if Assigned(HistoryQueue) then
                HistoryQueue.UpdateOptions(SnapshotHistoryOptions);
          end;

        until True;
      finally
        OptionsDialog.Free;
      end;
  finally
    Spectrum.Paused := WasPaused;
  end;
end;

procedure TForm1.UpdateRecentFiles;
var
  M: TMenuItem;
  I: Integer;
  SA: TStringDynArray;

begin
  MenuItemRecentFiles.Clear;

  if FRecentFiles.Count > 0 then begin
    FRecentFiles.GetAll(SA);
    for I := Low(SA) to High(SA) do begin
      M := TMenuItem.Create(nil);
      M.Name := TCommonFunctions.GlobalObjectNameGenerator(M);
      M.Caption := SA[I];
      M.OnClick := @RecentFilesOnClick;
      MenuItemRecentFiles.Add(M);
    end;
    MenuItemRecentFiles.Enabled := True;
  end else begin
    MenuItemRecentFiles.AddSeparator;
    MenuItemRecentFiles.Enabled := False;
  end;
end;

procedure TForm1.RecentFilesOnClick(Sender: TObject);
begin
  if Sender = Spectrum then begin
    DoLoad(TSnapshotOrTape.stBoth, FFileToOpen);
  end else if Sender is TMenuItem then begin
    FFileToOpen := TMenuItem(Sender).Caption;
    AddEventToQueue(@RecentFilesOnClick);
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

procedure TForm1.SetSzxSaveOptionsToSzx(
  const SzxSaveTapeOptions: TSzxSaveTapeOptions);
begin
  if SzxSaveTapeOptions = TSzxSaveTapeOptions.sstoSkip then
    TSnapshotSZX.OnSzxSaveTape := nil
  else begin
    TSnapshotSZX.OnSzxSaveTape := @SzxOnSaveTape;

    TSnapshotSZX.SaveTapeEmbedded := SzxSaveTapeOptions <> TSzxSaveTapeOptions.sstoFilePathOnly;
    TSnapshotSZX.SaveTapeCompressed := SzxSaveTapeOptions = TSzxSaveTapeOptions.sstoEmbeddedCompressed;
  end;
end;

procedure TForm1.GetSzxSaveOptionsFromSzx(out
  SzxSaveTapeOptions: TSzxSaveTapeOptions);
begin
  if not Assigned(TSnapshotSZX.OnSzxSaveTape) then
    SzxSaveTapeOptions := TSzxSaveTapeOptions.sstoSkip
  else if not TSnapshotSZX.SaveTapeEmbedded then
    SzxSaveTapeOptions := TSzxSaveTapeOptions.sstoFilePathOnly
  else if TSnapshotSZX.SaveTapeCompressed then
    SzxSaveTapeOptions := TSzxSaveTapeOptions.sstoEmbeddedCompressed
  else
    SzxSaveTapeOptions := TSzxSaveTapeOptions.sstoEmbeddedUncompressed;
end;

procedure TForm1.SoundLibraryOnSave(var LibPath: String);
begin
  TSoundPlayer.LibPath := LibPath;
  Spectrum.CheckStartSoundPlayer;
  UpdateSoundControls;
end;

function TForm1.SoundLibraryDialogCheckLoad(const APath: String): Boolean;
begin
  if TSoundPlayer.IsLibLoaded and (APath <> TSoundPlayer.LibPath) then
    TSoundPlayer.TryUnloadLib;
  Result := TSoundPlayer.TryLoadLib(APath);
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
    if Bmp = nil then
      Bmp := TBitmap.Create;

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

    PaintBox1.OnPaint := @PaintBmp;
  end else begin
    {$ifdef UseАuxiliaryBmp}
    if (Bmp.Width <> WholeScreenWidth) or (Bmp.Height <> WholeScreenHeight) then
      Bmp.SetSize(WholeScreenWidth, WholeScreenHeight);
    {$else}
    PaintBox1.OnPaint := @PaintScreen;
    {$endif}
  end;

  PaintBox1.Invalidate;
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

procedure TForm1.UpdateCheckHideToolbar;
begin
  ActionHideToolbar.Checked := FToolBar = nil;
end;

procedure TForm1.ShowOrHideToolbar(AShow: Boolean);
begin
  if AShow xor Assigned(FToolBar) then begin
    DisableAlign;
    try
      if AShow then begin
        FToolBar := TSwanToolBar.Create(nil);
        FToolBar.Align := alNone;
        FToolBar.Anchors := [];
        FToolBar.AnchorParallel(akTop, 0, Self);
        FToolBar.AnchorParallel(akLeft, 0, Self);
        FToolBar.AnchorParallel(akRight, 0, PaintBox1);
        PaintBox1.AnchorToNeighbour(akTop, 0, FToolBar);
        FToolBar.Images := ActionList1.Images;
        FToolBar.Wrapable := True;
        FToolBar.AutoSize := True;

        FToolBar.Parent := Self;

        FToolBar.AddButtonActions([
          ActionOpen,
          ActionPause,
          ActionReset,
          ActionLateTimings,
          ActionShowKeyboardOnScreen,
          ActionDontDrawScreenWhenLoading,
          ActionHistorySnapshots,
          ActionAllOptions,
          ActionAbout
        ]);
      end else begin
        PaintBox1.AnchorParallel(akTop, 0, Self);
        FreeAndNil(FToolBar);
      end;

      UpdateCheckHideToolbar;

    finally
      EnableAlign;
    end;

    SetNewAutoSize(nil);
  end;
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

{$ifNdef UseАuxiliaryBmp}
procedure TForm1.PaintScreen(Sender: TObject);
begin
  Spectrum.DrawToCanvas(PaintBox1.Canvas, DrawingRect);
end;
{$endif}

procedure TForm1.PaintBmp(Sender: TObject);
begin
  PaintBox1.Canvas.StretchDraw(DrawingRect, Bmp);
end;

procedure TForm1.DoDetachDebugger(Sender: TObject);
begin
  FormDebug := nil;
end;

procedure TForm1.SaveSnapshot(SnapshotClass: TSnapshotFileClass);

  function ChooseSaveFilePath(): Boolean;
  const
    SConfirmOverwrite: String =
      'The file %s' + LineEnding
      + 'already exists. Are you sure you want to overwrite this file?';

  var
    MR: TModalResult;
  begin
    repeat
      SaveDialog1.FileName := '';

      if not SaveDialog1.Execute then
        Break;

      if not FileExists(SaveDialog1.FileName) then
        Exit(True);

      MR := QuestionDlg('',
        Format(SConfirmOverwrite, [AnsiQuotedStr(SaveDialog1.FileName, '"')]),
        mtConfirmation, [
          mrNo, '&No, choose another', 'IsDefault',
          mrYes, '&Yes, overwrite',
          mrCancel, '&Cancel', 'IsCancel'
          ], 0
        );

      if MR = mrYes then
        Exit(True);

      if MR <> mrNo then
        Break;

    until False;

    Result := False;
  end;

var
  FileSnapshot: TSnapshotFile;
  WasPaused: Boolean;
  S: RawByteString;
  Extension: String;
  LastFilePath: RawByteString;

begin
  if Spectrum.IsRunning then begin
    WasPaused := Spectrum.Paused;
    try                          
      Spectrum.Paused := True;
      Extension := SnapshotClass.GetDefaultExtension;
      if not Extension.StartsWith(ExtensionSeparator, True) then
        Extension := ExtensionSeparator + Extension;

      SaveDialog1.DefaultExt := Extension;
      SaveDialog1.Filter :=
        SnapshotClass.GetDefaultExtension
        + '|*' + Extension
        + '|all files|' + '*';

      LastFilePath := FRecentFiles.GetLastFilePath;
      if LastFilePath <> '' then begin
        S := ExtractFilePath(LastFilePath);
        if DirectoryExists(S) then
          SaveDialog1.InitialDir := S;
      end;


      if ChooseSaveFilePath() then begin
        FileSnapshot := SnapshotClass.Create;
        try
          FileSnapshot.SetSpectrum(Spectrum);
          if FileSnapshot.SaveToFile(SaveDialog1.FileName) then begin
            FRecentFiles.Add(SaveDialog1.FileName);
            UpdateRecentFiles;
          end;
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
      TSoundPlayer.Volume := FSoundVolumeForm.Level;
    end;
  end;
end;

procedure TForm1.SoundVolumeAfterShow(Data: PtrInt);
var
  P: TPoint;
begin
  if Assigned(FSoundVolumeForm) then begin
    P := ToolButton1.ControlToScreen(
      Point(ToolButton1.Width - FSoundVolumeForm.Width, ToolButton1.Height)
      );
    FSoundVolumeForm.Left := P.X;
    FSoundVolumeForm.Top := P.Y - FSoundVolumeForm.Height;

    if Data > 0 then
      Application.QueueAsyncCall(@SoundVolumeAfterShow, Data - 1);
  end;
end;

procedure TForm1.SoundVolumeOnShow(Sender: TObject);
begin
  SoundVolumeAfterShow(1);
end;

procedure TForm1.ToolButtonOnShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  Tb: TToolButton;
begin
  if Sender is TToolButton then begin
    Tb := TToolButton(Sender);
    if Tb.PointInArrow(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y) then begin
      if Assigned(FSoundVolumeForm) and FSoundVolumeForm.IsParentOf(Tb) then
        HintInfo^.HintStr := 'Hide sound volume'
      else
        HintInfo^.HintStr := 'Adjust sound volume';
    end else
      HintInfo^.HintStr := 'Mute sound';
  end;
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
  S, LastFilePath: String;
  I: Integer;

begin
  if Spectrum.IsRunning then begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;

      GetAcceptableExtensions(SnapshotOrTape, True, Extensions);
      OpenDialog1.FilterIndex := 1;
      OpenDialog1.Filter := TCommonFunctionsLCL.MakeExtensionsFilter(Extensions);

      LastFilePath := FRecentFiles.GetLastFilePath;
      L := False;
      if LastFilePath <> '' then begin
        S := ExtractFilePath(LastFilePath);
        if DirectoryExists(S) then begin
          OpenDialog1.InitialDir := S;
          if FileExists(LastFilePath) then begin
            S := ExtractFileExt(LastFilePath);
            if S <> '' then begin
              if not S.StartsWith(ExtensionSeparator, True) then
                S := ExtensionSeparator + S;
              for I := Low(Extensions) to High(Extensions) do begin
                if AnsiCompareText(S, ExtensionSeparator + Extensions[I]) = 0 then begin
                  OpenDialog1.FileName := ExtractFileName(LastFilePath);
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
  AcceptedExtensions: TStringDynArray;

begin
  FFileToOpen := '';

  if (ASourceFile <> '') and Spectrum.IsRunning then begin
    WasPaused := Spectrum.Paused;
    try
      Spectrum.Paused := True;

      Stream := nil;
      Extension := ExtractFileExt(ASourceFile);
      
      if AnsiCompareText(Extension, ExtensionSeparator + 'zip') = 0 then begin
        GetAcceptableExtensions(SnapshotOrTape, True, AcceptedExtensions);
        if not TFileUnzipper.GetFileFromZipFile(ASourceFile, AcceptedExtensions, Stream, FileName) then begin
          Stream := nil;
        end else begin
          DoDirSeparators(FileName);
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

      if not Assigned(Stream) then begin
        if FRecentFiles.Remove(ASourceFile) then
          UpdateRecentFiles;
      end else begin
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

              //Spectrum.ResetSpectrum;
              Spectrum.InLoadingSnapshot := True;
              try
                L := SnapshotFile.LoadFromStream(Stream);
                UpdateShowCurrentlyActiveJoystick;
                UpdateCheckLateTimings;
              finally
                Spectrum.InLoadingSnapshot := False;
              end;
              if not L then
                LoadingFailed;

            finally
              SnapshotFile.Free;
            end;
          end else if SnapshotOrTape in [stTape, stBoth] then begin
            L := LoadTape(Stream, FileName, Extension);
            if not L then
              LoadingFailed;
          end;

          if L then begin
            FRecentFiles.Add(ASourceFile);
            UpdateRecentFiles;
          end;
        finally
          Stream.Free;
        end;
      end;

    finally
      Spectrum.Paused := WasPaused;
    end;
  end;
end;

function TForm1.LoadTape(const Stream: TStream; const FileName: String; Extension: String): Boolean;
var
  TapePlayerClass: TTapePlayerClass;

begin
  Result := False;

  if Assigned(Stream) then begin
    FreeTapePlayer;

    TapePlayerClass := TTapePlayer.CheckRealTapePlayerClass(Stream);
    if TapePlayerClass = nil then begin
      TapePlayerClass := TapePlayerClass.GetTapePlayerClassFromExtension(Extension);
      if TapePlayerClass = nil then
        TapePlayerClass := TTapePlayer.GetTapePlayerClassFromType(TTapeType.ttTap);
    end;

    if Assigned(TapePlayerClass) then begin
      TapePlayer := TapePlayerClass.Create;
      TapePlayer.FileName := FileName;

      try
        if TapePlayer.LoadFromStream(Stream) then begin
          TapePlayer.SetSpectrum(Spectrum);
          TapePlayer.Rewind;
          if FAutoShowTapePlayerWhenTapeLoaded then
            ShowTapeBrowser()
          else
            TapeBrowserAttachTape;
          Result := True;
        end;

      finally
        if not Result then begin
          FreeTapePlayer;
        end;
      end;
    end;
  end;
end;

procedure TForm1.SzxOnLoadTape(AStream: TStream; const AFileName: String;
  const AExtension: String; ACurrentBlock: Integer);
const
  ErrorMessageBadTapeFile: String = 'The szx snapshot contains path to %s'
    + LineEnding + 'This tape file cannot be opened.'
    + LineEnding + LineEnding + 'Check if the file exists and is a valid Spectrum tape file.';

  ErrorMessageBadTapeEmbedded: String = 'The szx snapshot contains an embedded tape file,'
    + LineEnding + 'but it does not seem to be valid.';

  ErrorMessageContinueLoading: String =
    LineEnding + '%s will try to go on loading of the snapshot without'
    + ' inserting this tape in cassete player.'
    ;
var
  FS: TStream;
  BadFilePath: Boolean;
  S: String;

begin
  BadFilePath := False;
  FS := nil;
  try
    if AStream = nil then begin
      BadFilePath := AFileName = '';
      if not BadFilePath then begin
        try
          FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
          AStream := FS;
        except
          on EFOpenError do begin
            BadFilePath := True;
          end;
        end;
      end;
    end;

    S := '';
    if not BadFilePath then begin
      if LoadTape(AStream, AFileName, AExtension) then begin
        if ACurrentBlock > 0 then
          TapePlayer.GoToBlock(ACurrentBlock);
      end else begin
        BadFilePath := Assigned(FS);
        if not BadFilePath then begin
          S := ErrorMessageBadTapeEmbedded;
        end;
      end;
    end;

    if BadFilePath then begin
      FreeTapePlayer;
      if AFileName <> '' then
        S := LineEnding + 'the tape file ""' + AFileName + '""'
      else
        S := 'a tape file.';
      S := Format(ErrorMessageBadTapeFile, [S]);
    end;

    if S <> '' then begin
      MessageDlg(ApplicationName, S + Format(ErrorMessageContinueLoading, [ApplicationName]),
        mtWarning, [mbOK], 0);
    end;

  finally
    FS.Free;
  end;

end;

procedure TForm1.SzxOnSaveTape(out ATapePlayer: TTapePlayer);
begin
  ATapePlayer := TapePlayer;
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
    {$ifdef UseАuxiliaryBmp}
    Spectrum.DrawToCanvas(Bmp.Canvas, Rect(0, 0, WholeScreenWidth, WholeScreenHeight));
    {$endif}
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
        if Spectrum.Is128KModel then begin
          K := K * 35469;
          K := ((Ticks - PrevTicks) * 1000 + (K shr 1)) div K;
        end else begin
          K := K * 35;
          K := (Ticks - PrevTicks + (K shr 1)) div K;
        end;

        LabelSpeed.Caption := 'speed ' + IntToStr(K) + '%';
      end else
        LabelSpeed.Caption := 'paused';

      PrevTicks := Ticks;
      LabelSpeed.Update;
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
  UpdateSoundControls;
  {$ifdef UseАuxiliaryBmp}
  PaintBox1.OnPaint := @PaintBmp;
  {$else}
  PaintBox1.OnPaint := @PaintScreen;
  {$endif}
  Application.AddOnDeactivateHandler(@FormDeactivate);

  SetLength(Arr{%H-}, ParamCount);
  for I := 1 to ParamCount do
    Arr[I - 1] := ParamStr(I);
  FreeAndNil(DropFiles);
  TryLoadFromFiles(TSnapshotOrTape.stBoth, Arr);

  if not (FDontAskPortAudioPath or TSoundPlayer.IsLibLoaded) then
    AddEventToQueue(@ShowMessageSoundLibNotLoaded);
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
  TB: TSwanSpeedButtonsBar;
begin
  if FTapeBrowser = nil then begin
    FTapeBrowser := TFormBrowseTape.Create(nil);
    FTapeBrowser.FreeNotification(Self);
    FTapeBrowser.AllowDropFiles := True;
    FTapeBrowser.OnDropFiles := Self.OnDropFiles;

    FTapeBrowser.OnGoToBlock := @TapeBrowserGoToBlock;

    TB := TSwanSpeedButtonsBar.Create(nil);
    try
      if FTapeBrowser.AddActionsToolBar(TB) then begin
        TB.Images := UnitDataModuleImages.DataModuleImages.ImageList1;
        TB.ImageWidth := 24;
        TB.AddButtonActions(
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
        TB := nil;
      end;
    finally
      TB.Free;
    end;

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

procedure TForm1.ShowSoundVolumeForm(const AToggleMute: Boolean);
begin
  if FSoundVolumeForm = nil then begin
    if AToggleMute then begin
      Spectrum.SoundMuted := not Spectrum.SoundMuted;
      UpdateSoundControls;
    end;
    FSoundVolumeForm := TFormSoundVolume.ShowSoundVolumeTracker(
      Spectrum.SoundMuted,
      TSoundPlayer.Volume);

    FSoundVolumeForm.Color := ToolBar1.GetRGBColorResolvingParent;
    FSoundVolumeForm.Panel2.Constraints.MinWidth := ToolBar1.Width;
    FSoundVolumeForm.FreeNotification(Self);

    ToolBar1.Enabled := False;
    FSoundVolumeForm.ToolBar1.Images := ToolBar1.Images;
    FSoundVolumeForm.ToolButton1.Style := ToolButton1.Style;
    FSoundVolumeForm.ToolButton1.ImageIndex := ToolButton1.ImageIndex;
    FSoundVolumeForm.ToolButton1.ShowHint := ToolButton1.ShowHint;
    FSoundVolumeForm.ToolButton1.OnShowHint := ToolButton1.OnShowHint;

    FSoundVolumeForm.OnTrackBarPositionChg := @SoundVolumeOnChg;
    FSoundVolumeForm.OnMuteClick := @ActionMuteSoundExecute;

    SoundVolumeAfterShow(0);
    FSoundVolumeForm.OnShow := @SoundVolumeOnShow;
    FSoundVolumeForm.Show;
  end;
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

procedure TForm1.ShowMessageSoundLibNotLoaded(Sender: TObject);
var
  MR: TModalResult;
  WasPaused: Boolean;
begin
  WasPaused := Spectrum.Paused;
  try
    Spectrum.Paused := True;
    MR := QuestionDlg('Loading portaudio failed',
      'Failed to load portaudio library.' + LineEnding
        + 'Without portaudio you are not going to hear any sound.' + LineEnding
        + 'Do you want to locate the library now?',
      mtConfirmation, [
        mrYes, '&Yes, locate the library yourself', 'IsDefault',
        mrNo, '&Not now, start Spectrum without sound', 'IsCancel',
        mrNoToAll, 'No, and don''t bother me with this again.'
        ], 0
      );

    case MR of
      mrYes:
        ActionPortAudioLibPathExecute(Spectrum);
      mrNoToAll:
        FDontAskPortAudioPath := True;
    otherwise
    end;

  finally
    Spectrum.Paused := WasPaused;
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
      UpdateSoundControls;
    end;

  end;
end;

end.

