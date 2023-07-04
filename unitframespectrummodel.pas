unit UnitFrameSpectrumModel;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitOptions, Forms, Controls, ExtCtrls,
  StdCtrls, Graphics;

type
  TFrameSpectrumModel = class(TFrame)
    Panel1: TPanel;
    RadioGroupSpectrumModel: TRadioGroup;
    RadioGroupKeyboardModel: TRadioGroup;
  private
    InitialIndexSpectrumModel: Integer;
    InitialIndexKeyboardModel: Integer;
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
    end;

    if RadioGroupKeyboardModel.ItemIndex = 0 then
      Dec(ModelToSet);

    FSpectrum.SpectrumModel := ModelToSet;
  end;
end;

constructor TFrameSpectrumModel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Spectrum model';
  InitialIndexSpectrumModel := -1;
  InitialIndexKeyboardModel := -1;
end;

constructor TFrameSpectrumModel.CreateFrameSpectrumModel(AOwner: TComponent;
  ASpectrum: TSpectrum);
begin
  if ASpectrum = nil then
    Abort;

  Create(AOwner);

  case ASpectrum.SpectrumModel of
    TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm16K_issue_3:
      RadioGroupSpectrumModel.ItemIndex := 0;
    TSpectrumModel.sm48K_issue_2, TSpectrumModel.sm48K_issue_3:
      RadioGroupSpectrumModel.ItemIndex := 1;
  end;

  case ASpectrum.SpectrumModel of
    TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm48K_issue_2:
      RadioGroupKeyboardModel.ItemIndex := 0;
    TSpectrumModel.sm16K_issue_3, TSpectrumModel.sm48K_issue_3:
      RadioGroupKeyboardModel.ItemIndex := 1;
  end;

  InitialIndexSpectrumModel := RadioGroupSpectrumModel.ItemIndex;
  InitialIndexKeyboardModel := RadioGroupKeyboardModel.ItemIndex;
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

