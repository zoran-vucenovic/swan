unit UnitFrameSoundVolume;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Types, Math;

type
  TFrameSoundVolume = class(TFrame)
    TrackBar1: TTrackBar;
    procedure TrackBar1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    function GetLevel: Integer;
    function GetVerticalOrientation: Boolean;
    procedure SetLevel(AValue: Integer);
    procedure SetOnTrackBarPositionChg(AValue: TNotifyEvent);
    procedure SetVerticalOrientation(AValue: Boolean);

  public
    property OnTrackBarPositionChg: TNotifyEvent write SetOnTrackBarPositionChg;
    property Level: Integer read GetLevel write SetLevel;
    property VerticalOrientation: Boolean read GetVerticalOrientation write SetVerticalOrientation;
  end;

implementation

{$R *.lfm}

{ TFrameSoundVolume }

procedure TFrameSoundVolume.TrackBar1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  TrackBar1.Position := TrackBar1.Position + 2 * Sign(WheelDelta);
  Handled := True;
end;

function TFrameSoundVolume.GetLevel: Integer;
begin
  Result := TrackBar1.Position and 31;
end;

function TFrameSoundVolume.GetVerticalOrientation: Boolean;
begin
  Result := TrackBar1.Orientation = trVertical;
end;

procedure TFrameSoundVolume.SetLevel(AValue: Integer);
begin
  TrackBar1.Position := AValue;
end;

procedure TFrameSoundVolume.SetOnTrackBarPositionChg(AValue: TNotifyEvent);
begin
  TrackBar1.OnChange := AValue;
end;

procedure TFrameSoundVolume.SetVerticalOrientation(AValue: Boolean);
begin
  if AValue then
    TrackBar1.Orientation := trVertical
  else
    TrackBar1.Orientation := trHorizontal;
  TrackBar1.Reversed := AValue;
end;

end.

