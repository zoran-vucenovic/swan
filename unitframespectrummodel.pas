unit UnitFrameSpectrumModel;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitOptions, UnitFrameChooseFile, UnitCommon,
  CommonFunctionsLCL, Forms, Controls, ExtCtrls, Graphics, StdCtrls, Dialogs;

type

  { TFrameSpectrumModel }

  TFrameSpectrumModel = class(TFrame)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    RadioGroupSpectrumModel: TRadioGroup;
    RadioGroupKeyboardModel: TRadioGroup;
    procedure CheckBox1Change(Sender: TObject);
    procedure RadioGroupSpectrumModelSelectionChanged(Sender: TObject);
  private
    FSpectrum: TSpectrum;
    FramesChooseFile: array [0..3] of TFrameChooseFile;
    ModelToSet: TSpectrumModel;
    Roms: TMemoryStream;

    procedure UpdateControls();
    procedure FormOnCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCloseCallback(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateFrameSpectrumModel(AOwner: TComponent;
      ASpectrum: TSpectrum);
    destructor Destroy; override;

    class function CreateForAllOptions(AOptionsDialog: TFormOptions;
      ASpectrum: TSpectrum): TFrameSpectrumModel;
  end;

implementation

{$R *.lfm}

{ TFrameSpectrumModel }

procedure TFrameSpectrumModel.RadioGroupSpectrumModelSelectionChanged(
  Sender: TObject);
begin
  UpdateControls();
end;

procedure TFrameSpectrumModel.CheckBox1Change(Sender: TObject);
begin
  UpdateControls();
end;

procedure TFrameSpectrumModel.UpdateControls();
var
  I: Integer;
  R: Integer;

begin
  RadioGroupKeyboardModel.Enabled := RadioGroupSpectrumModel.ItemIndex <= 1;

  R := ((RadioGroupSpectrumModel.ItemIndex div 2) * 3) div 2;
  for I := 0 to 3 do begin
    FramesChooseFile[I].Enabled := FramesChooseFile[I].Visible and CheckBox1.Checked and (I <= R);
  end;
end;

procedure TFrameSpectrumModel.FormOnCloseQuery(Sender: TObject;
  var CanClose: Boolean);

const
  RomBankSize = 1024 * 16;

var
  F: TCustomForm;
  I: Integer;
  Stream: TStream;
  RomsCount: Integer;
  RomFile: String;
  ErrMsg: String;
  BadFile: Boolean;

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

        CanClose := not CheckBox1.Checked;
        if not CanClose then begin
          BadFile := True;

          RomFile := '';
          Roms := TMemoryStream.Create;
          try
            RomsCount := ((RadioGroupSpectrumModel.ItemIndex div 2) * 3) div 2 + 1;
            Roms.Size := RomsCount shl 14;
            for I := 0 to RomsCount - 1 do begin
              BadFile := True;
              RomFile := FramesChooseFile[I].Path;

              if Trim(RomFile) = '' then begin
                ErrMsg := 'Custom rom file path must not be empty.';
              end else begin
                try
                  Stream := TFileStream.Create(RomFile, fmOpenRead or fmShareDenyWrite);
                  try
                    if Stream.Size <> RomBankSize then begin
                      ErrMsg := 'Bad custom rom file'
                        + LineEnding + RomFile + LineEnding + LineEnding
                        + 'The size of rom file must be exactly 16K';

                    end else begin
                      Stream.Position := 0;
                      if Stream.Read((Roms.Memory + (I shl 14))^, RomBankSize) = RomBankSize then
                        BadFile := False;
                    end;
                  finally
                    Stream.Free;
                  end;
                except
                  BadFile := True;
                  ErrMsg := 'Cannot load custom rom file'
                    + LineEnding + RomFile + LineEnding + LineEnding
                    + 'Check if the file exists.';
                end;
              end;

              if BadFile then
                Break;
            end;

            CanClose := not BadFile;
          finally
            if BadFile then begin
              FreeAndNil(Roms);
              if ErrMsg = '' then
                ErrMsg := 'Cannot load custom rom file'
                  + LineEnding + RomFile;

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
begin
  if (Sender is TCustomForm) and (TCustomForm(Sender).ModalResult = mrOK) then begin
    FSpectrum.SetSpectrumModel(ModelToSet, Roms);
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

  Label2.Caption := 'Choose non-standard roms.'
    + LineEnding + 'Each rom file must have exactly 16K';
  Label1.Caption := ' 16K and 48 K models need one rom file.'
    + LineEnding + ' 128K and +2 need two rom files'
    //+ LineEnding + ' +3 and +2a need four rom files.' { #todo : enable this line when +3 is implemented }
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
  ASpectrum: TSpectrum);
var
  N: Integer;
begin
  if ASpectrum = nil then
    Abort;

  Create(AOwner);

  ModelToSet := ASpectrum.SpectrumModel;
  Roms := nil;
  case ASpectrum.SpectrumModel of
    TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm16K_issue_3:
      RadioGroupSpectrumModel.ItemIndex := 0;
    TSpectrumModel.sm48K_issue_2, TSpectrumModel.sm48K_issue_3:
      RadioGroupSpectrumModel.ItemIndex := 1;
    TSpectrumModel.sm128K:
      RadioGroupSpectrumModel.ItemIndex := 2;
    TSpectrumModel.smPlus2:
      RadioGroupSpectrumModel.ItemIndex := 3;
  end;

  N := 1;
  case ASpectrum.SpectrumModel of
    TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm48K_issue_2:
      N := 0;
    TSpectrumModel.sm16K_issue_3, TSpectrumModel.sm48K_issue_3:
      ;
  otherwise
    RadioGroupKeyboardModel.Enabled := False;
  end;

  RadioGroupKeyboardModel.ItemIndex := N;

  UpdateControls();
end;

destructor TFrameSpectrumModel.Destroy;
begin
  Roms.Free;
  inherited Destroy;
end;

class function TFrameSpectrumModel.CreateForAllOptions(
  AOptionsDialog: TFormOptions; ASpectrum: TSpectrum): TFrameSpectrumModel;
var
  Fm: TFrameSpectrumModel;
begin
  Result := nil;
  if Assigned(AOptionsDialog) and Assigned(ASpectrum) then begin
    Fm := TFrameSpectrumModel.CreateFrameSpectrumModel(AOptionsDialog, ASpectrum);
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

