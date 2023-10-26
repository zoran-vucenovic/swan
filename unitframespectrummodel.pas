unit UnitFrameSpectrumModel;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitOptions, Forms, Controls, ExtCtrls,
  Graphics;

type

  { TFrameSpectrumModel }

  TFrameSpectrumModel = class(TFrame)
    Panel1: TPanel;
    RadioGroupSpectrumModel: TRadioGroup;
    RadioGroupKeyboardModel: TRadioGroup;
    procedure RadioGroupSpectrumModelSelectionChanged(Sender: TObject);
  private
    FSpectrum: TSpectrum;

    procedure FormCloseCallback(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateFrameSpectrumModel(AOwner: TComponent;
      ASpectrum: TSpectrum);

    class function CreateForAllOptions(AOptionsDialog: TFormOptions;
      ASpectrum: TSpectrum): TFrameSpectrumModel;
  end;

implementation

{$R *.lfm}

{ TFrameSpectrumModel }

procedure TFrameSpectrumModel.RadioGroupSpectrumModelSelectionChanged(
  Sender: TObject);
begin
  RadioGroupKeyboardModel.Enabled := RadioGroupSpectrumModel.ItemIndex <= 1;
end;

procedure TFrameSpectrumModel.FormCloseCallback(Sender: TObject;
  var CloseAction: TCloseAction);
var
  ModelToSet: TSpectrumModel;
begin
  if (Sender is TCustomForm) and (TCustomForm(Sender).ModalResult = mrOK) then begin
    case RadioGroupSpectrumModel.ItemIndex of
      0:
        ModelToSet := TSpectrumModel.sm16K_issue_3;
      1:
        ModelToSet := TSpectrumModel.sm48K_issue_3;
      2:
        ModelToSet := TSpectrumModel.sm128K;
      3:
        ModelToSet := TSpectrumModel.smPlus2;
    end;

    if RadioGroupKeyboardModel.Enabled and (RadioGroupKeyboardModel.ItemIndex = 0) then
      Dec(ModelToSet);

    FSpectrum.SpectrumModel := ModelToSet;
  end;
end;

constructor TFrameSpectrumModel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Spectrum model';
end;

constructor TFrameSpectrumModel.CreateFrameSpectrumModel(AOwner: TComponent;
  ASpectrum: TSpectrum);
var
  N: Integer;
begin
  if ASpectrum = nil then
    Abort;

  Create(AOwner);

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

      AOptionsDialog.AddHandlerClose(@Fm.FormCloseCallback);
      AOptionsDialog.AddAnOptionControl(Fm, 'Model');
      Result := Fm;
    except
      FreeAndNil(Fm);
    end;
  end;
end;

end.

