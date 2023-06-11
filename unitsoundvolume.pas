unit unitSoundVolume;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UnitDataModuleImages, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Buttons, ExtCtrls, Types, Math;

type
  TFormSoundVolume = class(TForm)
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    TrackBar1: TTrackBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDeactivate(Sender: TObject);
    procedure TrackBar1MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
  private
    FMuted: Boolean;

    function GetLevel: Integer;
    procedure SetMuted(const AValue: Boolean);
    procedure SetOnMuteClick(AValue: TNotifyEvent);
    procedure SetOnTrackBarPositionChg(AValue: TNotifyEvent);

  public
    class function ShowSoundVolumeTracker(const AMuted: Boolean; const ALevel: Integer): TFormSoundVolume;
    property OnTrackBarPositionChg: TNotifyEvent write SetOnTrackBarPositionChg;
    property OnMuteClick: TNotifyEvent write SetOnMuteClick;
    property Level: Integer read GetLevel;
    property Muted: Boolean write SetMuted;
  end;

implementation

{$R *.lfm}

{ TFormSoundVolume }

procedure TFormSoundVolume.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormSoundVolume.TrackBar1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  TrackBar1.Position := TrackBar1.Position + 2 * Sign(WheelDelta);
  Handled := True;
end;

function TFormSoundVolume.GetLevel: Integer;
begin
  Result := TrackBar1.Position;
end;

procedure TFormSoundVolume.SetMuted(const AValue: Boolean);
begin
  FMuted := AValue;
end;

procedure TFormSoundVolume.SetOnMuteClick(AValue: TNotifyEvent);
begin
  SpeedButton1.OnClick := AValue;
end;

procedure TFormSoundVolume.SetOnTrackBarPositionChg(AValue: TNotifyEvent);
begin
  TrackBar1.OnChange := AValue;
end;

procedure TFormSoundVolume.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

class function TFormSoundVolume.ShowSoundVolumeTracker(const AMuted: Boolean;
  const ALevel: Integer): TFormSoundVolume;
begin
  Result := TFormSoundVolume.Create(nil);
  Result.TrackBar1.Position := ALevel;
  //Result.FormStyle := fsStayOnTop;
  Result.AutoSize := True;
  Result.SetMuted(AMuted);
end;

end.

