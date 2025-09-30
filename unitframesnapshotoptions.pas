unit UnitFrameSnapshotOptions;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitOptions, UnitCommon, Forms, Controls, ExtCtrls,
  StdCtrls, Graphics;

type

  TFrameSnapshotOptions = class(TFrame, IHasDividerColour)
    CheckBoxCompressRamRom: TCheckBox;
    CheckBoxSkipTapeInfoSzxLoad: TCheckBox;
    CheckBoxSkipJoystickInfoSzxLoad: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Shape1: TShape;
  private
    function GetCompressRamAndRomBlocks: Boolean;
    function GetSaveTapeInfoSzxSave: Integer;
    function GetSkipJoystickInfoSzxLoad: Boolean;
    function GetSkipTapeInfoSzxLoad: Boolean;
    procedure SetCompressRamAndRomBlocks(AValue: Boolean);
    procedure SetSaveTapeInfoSzxSave(const AValue: Integer);
    procedure SetSkipJoystickInfoSzxLoad(AValue: Boolean);
    procedure SetSkipTapeInfoSzxLoad(AValue: Boolean);

  public
    constructor Create(TheOwner: TComponent); override;

    class function CreateForAllOptions(AOptionsDialog: TFormOptions): TFrameSnapshotOptions;

    function GetDividerColour: TColor;
    procedure SetDividerColour(AValue: TColor);

    property SkipJoystickInfoSzxLoad: Boolean read GetSkipJoystickInfoSzxLoad write SetSkipJoystickInfoSzxLoad;
    property SkipTapeInfoSzxLoad: Boolean read GetSkipTapeInfoSzxLoad write SetSkipTapeInfoSzxLoad;
    property SaveTapeInfoSzxSave: Integer read GetSaveTapeInfoSzxSave write SetSaveTapeInfoSzxSave;
    property CompressRamAndRomBlocks: Boolean read GetCompressRamAndRomBlocks write SetCompressRamAndRomBlocks;
  end;

implementation

{$R *.lfm}

{ TFrameSnapshotOptions }

function TFrameSnapshotOptions.GetDividerColour: TColor;
begin
  Result := Shape1.Brush.Color;
end;

function TFrameSnapshotOptions.GetCompressRamAndRomBlocks: Boolean;
begin
  Result := not CheckBoxCompressRamRom.Checked;
end;

function TFrameSnapshotOptions.GetSaveTapeInfoSzxSave: Integer;
begin
  Result := ComboBox1.ItemIndex;
end;

function TFrameSnapshotOptions.GetSkipJoystickInfoSzxLoad: Boolean;
begin
  Result := CheckBoxSkipJoystickInfoSzxLoad.Checked;
end;

function TFrameSnapshotOptions.GetSkipTapeInfoSzxLoad: Boolean;
begin
  Result := CheckBoxSkipTapeInfoSzxLoad.Checked;
end;

procedure TFrameSnapshotOptions.SetCompressRamAndRomBlocks(AValue: Boolean);
begin
  CheckBoxCompressRamRom.Checked := not AValue;
end;

procedure TFrameSnapshotOptions.SetDividerColour(AValue: TColor);
begin
  Shape1.Brush.Color := AValue;
end;

procedure TFrameSnapshotOptions.SetSaveTapeInfoSzxSave(const AValue: Integer);
begin
  if (AValue >= 0) and (AValue < ComboBox1.Items.Count) then
    ComboBox1.ItemIndex := AValue;
end;

procedure TFrameSnapshotOptions.SetSkipJoystickInfoSzxLoad(AValue: Boolean);
begin
  CheckBoxSkipJoystickInfoSzxLoad.Checked := AValue;
end;

procedure TFrameSnapshotOptions.SetSkipTapeInfoSzxLoad(AValue: Boolean);
begin
  CheckBoxSkipTapeInfoSzxLoad.Checked := AValue;
end;

constructor TFrameSnapshotOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);
  Caption := 'Snapshot options';
end;

class function TFrameSnapshotOptions.CreateForAllOptions(AOptionsDialog: TFormOptions
  ): TFrameSnapshotOptions;
var
  Fm: TFrameSnapshotOptions;
begin
  Result := nil;
  Fm := TFrameSnapshotOptions.Create(AOptionsDialog);
  try
    AOptionsDialog.AddAnOptionControl(Fm, 'Snapshots');

    Result := Fm;
  except
    Fm.Free;
  end;
end;

end.

