program swan;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  cthreads, clocale,
  {$endif}
  SysUtils,
  Interfaces, // this includes the LCL widgetset
  Forms,
  UnitInit, // keep this at the top (abore other our units; rtl, fcl and lcl units can be above, of course)
  DebugForm, UnitFrameWordDisplay, UnitFileSna, UnitSpectrumKeyboard, unit1,
  UnitSpectrum, UnitVer, UnitConfigs, UnitTzxPlayer, UnitFormBrowser,
  UnitColourPalette, UnitSpectrumColourMap, UnitKeyMaps,
  UnitSpectrumColoursBGRA, UnitKeyMapRecords, UnitFrameOneKeyMap,
  UnitFormKeyMappings, UnitSpectrumKeysDialog, UnitFormPressAKey, UnitCommon,
  UnitCommonSpectrum, UnitJoystick, UnitFormJoystickSetup, AboutBox,
  UnitColourFunctions, UnitDataModuleImages, PortAudioHeader, UnitBeeper,
  unitSoundVolume, FastIntegers, UnitInputLibraryPathDialog,
  UnitFormInputPokes, UnitSZX, UnitStreamCompression;

{$R *.res}

procedure InitBuildDateString;
begin
  // Keep this here in main project file, so that included values are updated.
  TFormAbout.BuildDate :=
    // We might display build date in localized form:
    //DateToStr(EncodeDate({$i %dateyear%}, {$i %datemonth%}, {$i %dateday%}))
    // or rather always use standard YYYY-MM-DD form:
    Format('%.4d-%.2d-%.2d', [{$i %dateyear%}, {$i %datemonth%}, {$i %dateday%}])
    //+ ' ' + {$i %Time%}
    ;
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
  Application.Initialize;
  Application.ShowButtonGlyphs := TApplicationShowGlyphs.sbgAlways;
  Application.ShowMenuGlyphs := TApplicationShowGlyphs.sbgAlways;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.

