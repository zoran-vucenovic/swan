unit UnitFrameSpectrumModel;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitOptions, UnitFrameChooseFile, UnitCommon,
  CommonFunctionsLCL, UnitRomPaths, Forms, Controls, ExtCtrls, Graphics,
  StdCtrls, Dialogs;

type

  { TFrameSpectrumModel }

  TFrameSpectrumModel = class(TFrame)
    CheckBoxCustomRoms: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    RadioGroupUlaTimings: TRadioGroup;
    RadioGroupSpectrumModel: TRadioGroup;
    RadioGroupKeyboardModel: TRadioGroup;
    procedure CheckBoxCustomRomsChange(Sender: TObject);
    procedure RadioGroupSpectrumModelSelectionChanged(Sender: TObject);
    procedure RadioGroupUlaTimingsSelectionChanged(Sender: TObject);
  private
    FSpectrum: TSpectrum;
    FramesChooseFile: array [0..3] of TFrameChooseFile;
    ModelToSet: TSpectrumModel;
    Roms: TMemoryStream;
    FRomPaths: TRomPaths;

    SkipUpdateControls: Boolean;

    procedure UpdateControls();
    procedure FormOnCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCloseCallback(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateFrameSpectrumModel(AOwner: TComponent;
      ASpectrum: TSpectrum; ARomPaths: TRomPaths);
    destructor Destroy; override;

    class function CreateForAllOptions(AOptionsDialog: TFormOptions;
      ASpectrum: TSpectrum; ARomPaths: TRomPaths): TFrameSpectrumModel;
  end;

implementation

{$R *.lfm}

{ TFrameSpectrumModel }

procedure TFrameSpectrumModel.RadioGroupSpectrumModelSelectionChanged(
  Sender: TObject);
begin
  UpdateControls();
end;

procedure TFrameSpectrumModel.RadioGroupUlaTimingsSelectionChanged(
  Sender: TObject);
begin
  UpdateControls();
end;

procedure TFrameSpectrumModel.CheckBoxCustomRomsChange(Sender: TObject);
begin
  UpdateControls();
end;

procedure TFrameSpectrumModel.UpdateControls();
var
  I: Integer;
  R: Integer;
  B: TFrameChooseFile;

begin
  if SkipUpdateControls then
    Exit;

  RadioGroupKeyboardModel.Enabled := RadioGroupSpectrumModel.ItemIndex <= 1;

  R := ((RadioGroupSpectrumModel.ItemIndex div 2) * 3) div 2;
  for I := 0 to 3 do begin
    B := FramesChooseFile[I];
    B.Enabled := B.IsVisible and CheckBoxCustomRoms.Checked and (I <= R);
    if B.IsVisible then begin
      case R of
        0:
          B.Path := FRomPaths.RomPaths1[I];
      otherwise
        B.Path := FRomPaths.RomPaths2[I];
      end;
    end;
  end;
end;

procedure TFrameSpectrumModel.FormOnCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  F: TCustomForm;
  I, J: Integer;
  Stream: TStream;
  RomsCount: Integer;
  RomFile: String;
  ErrMsg: String;
  BadFile: Boolean;
  L: Integer;
  K: Integer;
  NeededSize, NeededSizeKB: Integer;

begin
  FreeAndNil(Roms);

  ErrMsg := '';
  if Sender is TCustomForm then begin
    F := TCustomForm(Sender);

    if F.ModalResult = mrOK then begin
      CanClose := False;
      case RadioGroupSpectrumModel.ItemIndex of
        0:
          ModelToSet := TSpectrumModel.sm16K_issue_3;
        1:
          ModelToSet := TSpectrumModel.sm48K_issue_3;
        2:
          ModelToSet := TSpectrumModel.sm128K;
        3:
          ModelToSet := TSpectrumModel.smPlus2;
        // ...
      otherwise
        ModelToSet := TSpectrumModel.smNone;
      end;

      if ModelToSet <> TSpectrumModel.smNone then begin

        if RadioGroupKeyboardModel.Enabled and (RadioGroupKeyboardModel.ItemIndex = 0) then
          Dec(ModelToSet);

        CanClose := not CheckBoxCustomRoms.Checked;
        if not CanClose then begin
          BadFile := True;
          ErrMsg := '';
          RomFile := '';
          Roms := TMemoryStream.Create;
          try
            RomsCount := ((RadioGroupSpectrumModel.ItemIndex div 2) * 3) div 2 + 1;
            NeededSizeKB := RomsCount shl 4;
            NeededSize := NeededSizeKB shl 10;
            Roms.Size := NeededSize;
            L := 0;
            J := 0;
            for I := 0 to RomsCount - 1 do begin
              RomFile := FramesChooseFile[I].Path;
              if Trim(RomFile) <> '' then begin
                Inc(J);

                Stream := nil;
                try
                  Stream := TFileStream.Create(RomFile, fmOpenRead or fmShareDenyWrite);
                  BadFile := False;
                except
                  BadFile := True;
                end;
                if BadFile then begin
                  FreeAndNil(Roms);
                  Stream.Free;
                  ErrMsg := 'Cannot load custom rom file'
                    + LineEnding + RomFile + LineEnding + LineEnding
                    + 'Check if the file exists.';
                  Break;
                end;

                try
                  K := L;
                  L := L + Stream.Size;
                  BadFile := L > NeededSize;
                  if not BadFile then begin
                    Stream.Position := 0;
                    if Stream.Read((Roms.Memory + K)^, Stream.Size) <> Stream.Size then begin
                      FreeAndNil(Roms);
                      BadFile := True;
                      ErrMsg := 'Cannot load custom rom file' + LineEnding + RomFile;
                    end;
                  end;
                finally
                  Stream.Free;
                end;
                if BadFile then
                  Break;
              end;
            end;

            if ErrMsg = '' then begin
              if J = 0 then begin
                ErrMsg := 'Custom rom file paths must not be empty.';
              end else if L <> NeededSize then begin
                ErrMsg := Format('Total size of provided files must be exactly %d KB.', [NeededSizeKB]);
              end;
            end;

            CanClose := ErrMsg = '';
          finally
            if not CanClose then begin
              FreeAndNil(Roms);
              MessageDlg(ErrMsg, mtError, [mbClose], 0);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFrameSpectrumModel.FormCloseCallback(Sender: TObject;
  var CloseAction: TCloseAction);

var
  R: Integer;

begin
  if (Sender is TCustomForm) and (TCustomForm(Sender).ModalResult = mrOK) then begin
    FSpectrum.SetSpectrumModel(ModelToSet, Roms);
    FSpectrum.LateTimings := RadioGroupUlaTimings.ItemIndex = 1;
    if Assigned(Roms) and (FSpectrum.SpectrumModel = ModelToSet)
      and Assigned(FRomPaths) and CheckBoxCustomRoms.Checked
    then begin
      R := ((RadioGroupSpectrumModel.ItemIndex div 2) * 3) div 2;
      case R of
        0:
          FRomPaths.RomPaths1[0] := FramesChooseFile[0].Path;
      otherwise
        FRomPaths.RomPaths2[0] := FramesChooseFile[0].Path;
        FRomPaths.RomPaths2[1] := FramesChooseFile[1].Path;
      end;
    end;

  end;
end;

constructor TFrameSpectrumModel.Create(TheOwner: TComponent);
var
  I: Integer;
  Fm: TFrameChooseFile;
  S: String;
  Lab: TCustomLabel;

begin
  inherited Create(TheOwner);

  Caption := 'Spectrum model';
  SkipUpdateControls := True;

  FRomPaths := nil;
  Roms := nil;

  Label2.Caption := 'Choose non-standard roms.';
  Label1.Caption := ' 16K and 48K models need one 16K rom file.'
    + LineEnding + ' 128K and +2 need one 32K or two 16K rom files'
    { #todo : enable this line when +3 is implemented }
    //+ LineEnding + ' +3 and +2a need up to four rom files (64K total size).'
    ;
  S := '';
  I := 0;

  OpenDialog1.Filter := TCommonFunctionsLCL.MakeExtensionsFilter(['rom']);

  while I <= 3 do begin
    Fm := TFrameChooseFile.Create(Panel4);
    Fm.Name := UnitCommon.TCommonFunctions.GlobalObjectNameGenerator(Fm);
    FramesChooseFile[I] := Fm;
    Fm.Anchors := [];
    if I = 0 then
      Fm.AnchorToNeighbour(akTop, 4, Label1)
    else
      Fm.AnchorToNeighbour(akTop, 4, FramesChooseFile[I - 1]);

    Lab := TCustomLabel.Create(Panel4);
    Lab.Name := UnitCommon.TCommonFunctions.GlobalObjectNameGenerator(Lab);
    Lab.Anchors := [];
    Lab.AnchorVerticalCenterTo(Fm);
    Lab.AnchorParallel(akLeft, 0, Panel4);
    Fm.AnchorToNeighbour(akLeft, 2, Lab);
    Fm.AnchorParallel(akRight, 0, Panel4);

    Fm.InitFrameChooseFile(S, OpenDialog1);

    Lab.Caption := IntToStr(I) + '.';
    Lab.Parent := Panel4;

    { #todo : remove this line when +3 is implemented! }
    Fm.Visible := I <= 1;

    Fm.Parent := Panel4;

    Inc(I);
  end;

  Panel4.BevelOuter := bvNone;
  Panel4.AutoSize := True;
end;

constructor TFrameSpectrumModel.CreateFrameSpectrumModel(AOwner: TComponent;
  ASpectrum: TSpectrum; ARomPaths: TRomPaths);
var
  N: Integer;
begin
  if ASpectrum = nil then
    Abort;

  Create(AOwner);

  FRomPaths := ARomPaths;

  ModelToSet := ASpectrum.SpectrumModel;
  Roms := nil;
  case ModelToSet of
    TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm16K_issue_3:
      RadioGroupSpectrumModel.ItemIndex := 0;
    TSpectrumModel.sm48K_issue_2, TSpectrumModel.sm48K_issue_3:
      RadioGroupSpectrumModel.ItemIndex := 1;
    TSpectrumModel.sm128K:
      RadioGroupSpectrumModel.ItemIndex := 2;
    TSpectrumModel.smPlus2:
      RadioGroupSpectrumModel.ItemIndex := 3;
  otherwise
    ModelToSet := TSpectrumModel.sm48K_issue_3;
    RadioGroupSpectrumModel.ItemIndex := 1;
  end;

  N := 1;
  case ModelToSet of
    TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm48K_issue_2:
      N := 0;
    TSpectrumModel.sm16K_issue_3, TSpectrumModel.sm48K_issue_3:
      ;
  otherwise
    RadioGroupKeyboardModel.Enabled := False;
  end;

  RadioGroupKeyboardModel.ItemIndex := N;

  if ASpectrum.LateTimings then
    N := 1
  else
    N := 0;

  RadioGroupUlaTimings.ItemIndex := N;

  SkipUpdateControls := False;
  UpdateControls();
end;

destructor TFrameSpectrumModel.Destroy;
begin
  Roms.Free;
  inherited Destroy;
end;

class function TFrameSpectrumModel.CreateForAllOptions(
  AOptionsDialog: TFormOptions; ASpectrum: TSpectrum; ARomPaths: TRomPaths
  ): TFrameSpectrumModel;
var
  Fm: TFrameSpectrumModel;
begin
  Result := nil;
  if Assigned(AOptionsDialog) and Assigned(ASpectrum) then begin
    Fm := TFrameSpectrumModel.CreateFrameSpectrumModel(AOptionsDialog, ASpectrum, ARomPaths);
    try
      Fm.FSpectrum := ASpectrum;

      AOptionsDialog.AddCloseQuery(@Fm.FormOnCloseQuery);
      AOptionsDialog.AddHandlerClose(@Fm.FormCloseCallback);
      AOptionsDialog.AddAnOptionControl(Fm, 'Model');
      Result := Fm;
    except
      FreeAndNil(Fm);
    end;
  end;
end;

end.

