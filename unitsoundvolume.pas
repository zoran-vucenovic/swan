unit unitSoundVolume;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UnitFrameSoundVolume, Forms,
  Controls, ComCtrls, ExtCtrls;

type
  TFormSoundVolume = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ToolButton1ArrowClick(Sender: TObject);
  private
    FrameSoundVolume: UnitFrameSoundVolume.TFrameSoundVolume;

    function GetLevel: Integer;
    procedure SetOnMuteClick(AValue: TNotifyEvent);
    procedure SetOnTrackBarPositionChg(AValue: TNotifyEvent);

  public
    class function ShowSoundVolumeTracker(const AMuted: Boolean; const ALevel: Integer): TFormSoundVolume;
    property OnTrackBarPositionChg: TNotifyEvent write SetOnTrackBarPositionChg;
    property OnMuteClick: TNotifyEvent write SetOnMuteClick;
    property Level: Integer read GetLevel;
  end;

implementation

{$R *.lfm}

{ TFormSoundVolume }

procedure TFormSoundVolume.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormSoundVolume.ToolButton1ArrowClick(Sender: TObject);
begin
  Close;
end;

function TFormSoundVolume.GetLevel: Integer;
begin
  Result := FrameSoundVolume.Level;
end;

procedure TFormSoundVolume.SetOnMuteClick(AValue: TNotifyEvent);
begin
  ToolButton1.OnClick := AValue;
end;

procedure TFormSoundVolume.SetOnTrackBarPositionChg(AValue: TNotifyEvent);
begin
  FrameSoundVolume.OnTrackBarPositionChg := AValue;
end;

procedure TFormSoundVolume.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormSoundVolume.FormCreate(Sender: TObject);
begin
  Panel1.BorderStyle := bsNone;
  Panel2.BorderStyle := bsNone;
  FrameSoundVolume := TFrameSoundVolume.Create(Panel1);
  FrameSoundVolume.VerticalOrientation := True;
  FrameSoundVolume.Anchors := [];
  FrameSoundVolume.AnchorParallel(akTop, 0, Panel1);
  FrameSoundVolume.AnchorParallel(akLeft, 0, Panel1);
  FrameSoundVolume.Parent := Panel1;
  Panel1.OnMouseWheel := FrameSoundVolume.TrackBar1.OnMouseWheel;
  Panel2.OnMouseWheel := FrameSoundVolume.TrackBar1.OnMouseWheel;
  Panel1.AutoSize := True;
end;

class function TFormSoundVolume.ShowSoundVolumeTracker(const AMuted: Boolean;
  const ALevel: Integer): TFormSoundVolume;
begin
  Result := TFormSoundVolume.Create(nil);
  Result.FrameSoundVolume.Level := ALevel;
  Result.AutoSize := True;
end;

end.

