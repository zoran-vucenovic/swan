unit UnitFrameOtherOptions;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitOptions, UnitCommon, Forms, Controls, ExtCtrls,
  StdCtrls;

type

  { TFrameOtherOptions }

  TFrameOtherOptions = class(TFrame)
    CheckBoxSkipTapeInfoSzxLoad: TCheckBox;
    CheckBoxSkipJoystickInfoSzxLoad: TCheckBox;
    CheckBoxAutoShowTapePlayerOnLoadTape: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
  private
    function GetAutoShowTapePlayerOnLoadTape: Boolean;
    function GetSaveTapeInfoSzxSave: Integer;
    function GetSkipJoystickInfoSzxLoad: Boolean;
    function GetSkipTapeInfoSzxLoad: Boolean;
    procedure SetAutoShowTapePlayerOnLoadTape(AValue: Boolean);
    procedure SetSaveTapeInfoSzxSave(const AValue: Integer);
    procedure SetSkipJoystickInfoSzxLoad(AValue: Boolean);
    procedure SetSkipTapeInfoSzxLoad(AValue: Boolean);

  public
    constructor Create(TheOwner: TComponent); override;

    class function CreateForAllOptions(AOptionsDialog: TFormOptions): TFrameOtherOptions;

    property AutoShowTapePlayerOnLoadTape: Boolean read GetAutoShowTapePlayerOnLoadTape write SetAutoShowTapePlayerOnLoadTape;
    property SkipJoystickInfoSzxLoad: Boolean read GetSkipJoystickInfoSzxLoad write SetSkipJoystickInfoSzxLoad;
    property SkipTapeInfoSzxLoad: Boolean read GetSkipTapeInfoSzxLoad write SetSkipTapeInfoSzxLoad;
    property SaveTapeInfoSzxSave: Integer read GetSaveTapeInfoSzxSave write SetSaveTapeInfoSzxSave;
  end;

implementation

{$R *.lfm}

{ TFrameOtherOptions }

function TFrameOtherOptions.GetAutoShowTapePlayerOnLoadTape: Boolean;
begin
  Result := CheckBoxAutoShowTapePlayerOnLoadTape.Checked;
end;

function TFrameOtherOptions.GetSaveTapeInfoSzxSave: Integer;
begin
  Result := ComboBox1.ItemIndex;
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

procedure TFrameOtherOptions.SetSaveTapeInfoSzxSave(const AValue: Integer);
begin
  if (AValue >= 0) and (AValue < ComboBox1.Items.Count) then
    ComboBox1.ItemIndex := AValue;
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

  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);
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

