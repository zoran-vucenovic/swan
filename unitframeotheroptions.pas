unit UnitFrameOtherOptions;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitOptions, Forms, Controls, ExtCtrls, StdCtrls;

type
  TFrameOtherOptions = class(TFrame)
    CheckBoxSkipTapeInfoSzxLoad: TCheckBox;
    CheckBoxSkipJoystickInfoSzxLoad: TCheckBox;
    CheckBoxAutoShowTapePlayerOnLoadTape: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    function GetAutoShowTapePlayerOnLoadTape: Boolean;
    function GetSkipJoystickInfoSzxLoad: Boolean;
    function GetSkipTapeInfoSzxLoad: Boolean;
    procedure SetAutoShowTapePlayerOnLoadTape(AValue: Boolean);
    procedure SetSkipJoystickInfoSzxLoad(AValue: Boolean);
    procedure SetSkipTapeInfoSzxLoad(AValue: Boolean);

  public
    constructor Create(TheOwner: TComponent); override;

    class function CreateForAllOptions(AOptionsDialog: TFormOptions): TFrameOtherOptions;

    property AutoShowTapePlayerOnLoadTape: Boolean read GetAutoShowTapePlayerOnLoadTape write SetAutoShowTapePlayerOnLoadTape;
    property SkipJoystickInfoSzxLoad: Boolean read GetSkipJoystickInfoSzxLoad write SetSkipJoystickInfoSzxLoad;
    property SkipTapeInfoSzxLoad: Boolean read GetSkipTapeInfoSzxLoad write SetSkipTapeInfoSzxLoad;
  end;

implementation

{$R *.lfm}

{ TFrameOtherOptions }

function TFrameOtherOptions.GetAutoShowTapePlayerOnLoadTape: Boolean;
begin
  Result := CheckBoxAutoShowTapePlayerOnLoadTape.Checked;
end;

function TFrameOtherOptions.GetSkipJoystickInfoSzxLoad: Boolean;
begin
  Result := CheckBoxSkipJoystickInfoSzxLoad.Checked;
end;

function TFrameOtherOptions.GetSkipTapeInfoSzxLoad: Boolean;
begin
  Result := CheckBoxSkipTapeInfoSzxLoad.Checked;
end;

procedure TFrameOtherOptions.SetAutoShowTapePlayerOnLoadTape(AValue: Boolean);
begin
  CheckBoxAutoShowTapePlayerOnLoadTape.Checked := AValue;
end;

procedure TFrameOtherOptions.SetSkipJoystickInfoSzxLoad(AValue: Boolean);
begin
  CheckBoxSkipJoystickInfoSzxLoad.Checked := AValue;
end;

procedure TFrameOtherOptions.SetSkipTapeInfoSzxLoad(AValue: Boolean);
begin
  CheckBoxSkipTapeInfoSzxLoad.Checked := AValue;
end;

constructor TFrameOtherOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Other options';
end;

class function TFrameOtherOptions.CreateForAllOptions(AOptionsDialog: TFormOptions
  ): TFrameOtherOptions;
var
  Fm: TFrameOtherOptions;
begin
  Result := nil;
  Fm := TFrameOtherOptions.Create(AOptionsDialog);
  try
    AOptionsDialog.AddAnOptionControl(Fm, 'Other');

    Result := Fm;
  except
    Fm.Free;
  end;
end;

end.

