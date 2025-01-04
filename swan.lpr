program swan;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads, clocale,
  {$endif}
  SysUtils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  UnitInit, // keep this at the top (above other our units; rtl, fcl and lcl units can be above, of course)
  UnitFrameWordDisplay, UnitSnapshotFiles, UnitSpectrumKeyboard, unit1,
  UnitSpectrum, UnitVer, UnitConfigs, UnitTzxPlayer, UnitFormBrowser,
  UnitColourPalette, UnitSpectrumColourMap, UnitKeyMaps,
  UnitSpectrumColoursBGRA, UnitKeyMapRecords,
  UnitSpectrumKeysDialog, UnitFormPressAKey, UnitCommon, UnitCommonSpectrum,
  UnitJoystick, UnitFrameJoystickSetup, AboutBox, UnitColourFunctions,
  UnitDataModuleImages, PortAudioHeader, UnitSoundPlayer, unitSoundVolume,
  FastIntegers, UnitInputLibraryPathDialog, UnitFormInputPokes, UnitSZX,
  UnitStreamCompression, UnitFormHistorySnapshots, UnitControlScreenBitmap,
  UnitTapePlayer, UnitPzxPlayer, UnitSpectrumKeysControl, UnitKeyboardOnScreen,
  UnitFormForOptionsBasic, UnitFrameKeyMappings, UnitOptions,
  UnitFrameSnapshotOptions, UnitFrameSpectrumModel, UnitFrameSound,
  UnitFrameHistorySnapshotOptions, UnitRecentFiles, UnitMemory, SnapshotZ80,
  SnapshotSNA, SoundChipAY_3_8912, UnitFrameChooseFile, UnitFileZip,
  UnitRomPaths, UnitSwanToolbar, UnitFrameSoundVolume, UnitFormChooseString,
  UnitCSW, UnitDisassembler, UnitDlgStartAdress, UnitGridSpectrumMemory,
  UnitFormDebug, UnitFrameGridMemory, UnitDebugCpuState, UnitDebugger,
  UnitDebugBreakpointsGrid, UnitFrameBreakpoints, UnitFrameToolbarOptions,
  UnitSwanTreeView, UnitFrameTapeOptions, UnitControlKeyMappings;

{$R *.res}

procedure InitBuildDateString;
begin
  // Keep this here in main project file, so that included values are updated.
  TCommonSpectrum.SetBuildDate({$i %dateyear%}, {$i %datemonth%}, {$i %dateday%});
end;

procedure SetApplicationTitle;
begin
  Application.Title := OnGetApplicationName();
end;

begin
  RequireDerivedFormResource := True;
  InitBuildDateString;
  SetApplicationTitle;

  Application.Scaled := True;
  Application.TaskBarBehavior := TTaskBarBehavior.tbSingleButton;
  Application.Initialize;
  Application.ShowButtonGlyphs := TApplicationShowGlyphs.sbgAlways;
  Application.ShowMenuGlyphs := TApplicationShowGlyphs.sbgAlways;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.

