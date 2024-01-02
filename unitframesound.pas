unit UnitFrameSound;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitInputLibraryPathDialog, UnitSpectrum, UnitOptions,
  UnitFrameSoundVolume, Forms, Controls, ExtCtrls, StdCtrls;

type
  TFrameSound = class(TFrame)
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    LabelVolume: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelVolume: TPanel;
    PanelSoundLibrary: TPanel;
  private
    FrameSoundLib: TFrameInputLibraryPath;
    FSpectrum: TSpectrum;
    FrameSoundVolume: TFrameSoundVolume;

    procedure FormOnShow(Sender: TObject);
    procedure AfterShow(Data: PtrInt);
    function GetSoundMuted: Boolean;
    function GetVolLevel: Integer;
    procedure SetFrameSoundLib(AFrameSoundLib: TFrameInputLibraryPath);
    procedure SetSoundMuted(const AValue: Boolean);
    procedure FormCloseCallback(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure SetVolLevel(AValue: Integer);
  public
    constructor Create(TheOwner: TComponent); override;

    class function CreateForAllOptions(AOptionsDialog: TFormOptions;
      ASpectrum: TSpectrum; AFrameSoundLib: TFrameInputLibraryPath): TFrameSound;

    property VolLevel: Integer read GetVolLevel write SetVolLevel;
  end;

implementation

{$R *.lfm}

{ TFrameSound }

procedure TFrameSound.FormOnShow(Sender: TObject);
begin
  AfterShow(2);
end;

procedure TFrameSound.AfterShow(Data: PtrInt);
begin
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShow, Data - 1)
  else begin
    DisableAlign;
    try
      FrameSoundLib.AnchorParallel(akRight, 0, PanelSoundLibrary);
      FrameSoundLib.AutoSize := True;
      PanelSoundLibrary.AnchorParallel(akRight, 0, Panel1);
      Panel1.AnchorParallel(akRight, 0, Self);
    finally
      EnableAlign;
    end;
  end;
end;

function TFrameSound.GetSoundMuted: Boolean;
begin
  Result := CheckBox1.Checked;
end;

function TFrameSound.GetVolLevel: Integer;
begin
  Result := FrameSoundVolume.Level;
end;

procedure TFrameSound.SetSoundMuted(const AValue: Boolean);
begin
  CheckBox1.Checked := AValue;
end;

procedure TFrameSound.FormCloseCallback(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if (Sender is TCustomForm) and (TCustomForm(Sender).ModalResult = mrOK) then
    FSpectrum.SoundMuted := GetSoundMuted;
end;

procedure TFrameSound.SetVolLevel(AValue: Integer);
begin
  FrameSoundVolume.Level := AValue;
end;

constructor TFrameSound.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Sound options';

  PanelSoundLibrary.BevelOuter := bvNone;
  PanelSoundLibrary.Caption := '';
  Panel1.BevelOuter := bvNone;
  Panel1.Caption := '';
  Panel2.Caption := '';
  PanelVolume.BevelOuter := bvNone;
  PanelVolume.Caption := '';

  PanelSoundLibrary.AutoSize := True;

  FrameSoundVolume := TFrameSoundVolume.Create(PanelVolume);
  FrameSoundVolume.VerticalOrientation := False;
  FrameSoundVolume.Anchors := [];
  FrameSoundVolume.AnchorParallel(akTop, 0, PanelVolume);
  FrameSoundVolume.AnchorToNeighbour(akLeft, 5, LabelVolume);
  FrameSoundVolume.Parent := PanelVolume;

  PanelVolume.OnMouseWheel := FrameSoundVolume.TrackBar1.OnMouseWheel;
  Panel2.OnMouseWheel := FrameSoundVolume.TrackBar1.OnMouseWheel;

  PanelVolume.AutoSize := True;
  Panel1.AutoSize := True;
  Self.AutoSize := True;
end;

procedure TFrameSound.SetFrameSoundLib(AFrameSoundLib: TFrameInputLibraryPath);
begin
  FrameSoundLib := AFrameSoundLib;
  FrameSoundLib.Anchors := [];
  FrameSoundLib.AnchorParallel(akLeft, 0, PanelSoundLibrary);
  FrameSoundLib.AnchorParallel(akTop, 0, PanelSoundLibrary);
  FrameSoundLib.Parent := PanelSoundLibrary;
end;

class function TFrameSound.CreateForAllOptions(AOptionsDialog: TFormOptions;
  ASpectrum: TSpectrum; AFrameSoundLib: TFrameInputLibraryPath): TFrameSound;
var
  Fm: TFrameSound;
begin
  Result := nil;
  if Assigned(AOptionsDialog)
    and Assigned(ASpectrum)
    and Assigned(AFrameSoundLib)
  then begin
    Fm := TFrameSound.Create(AOptionsDialog);
    try
      Fm.SetFrameSoundLib(AFrameSoundLib);
      Fm.FSpectrum := ASpectrum;
      Fm.SetSoundMuted(ASpectrum.SoundMuted);

      AOptionsDialog.AddHandlerFirstShow(@Fm.FormOnShow);
      AOptionsDialog.AddHandlerClose(@Fm.FormCloseCallback);
      AOptionsDialog.AddAnOptionControl(Fm, 'Sound');
      Result := Fm;
    except
      FreeAndNil(Fm);
    end;
  end;
end;

end.

