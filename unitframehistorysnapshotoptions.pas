unit UnitFrameHistorySnapshotOptions;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitKeyMaps, CommonFunctionsLCL, UnitFormPressAKey,
  UnitHistorySnapshots, UnitOptions, UnitCommon, Forms, Controls, ExtCtrls,
  LCLType, StdCtrls, Spin;

type
  TFrameHistorySnapshotOptions = class(TFrame)
    CheckBoxAutoCreateSnapshots: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel14: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure Panel10Paint(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit2EditingDone(Sender: TObject);
  private
    FKeyBack: Word;
    LabelEdit: TCustomLabel;
    SpinEdit2Changing: Boolean;

    function GetHistoryEnabled: Boolean;
    function GetMaxNumberOfSnapshotsInMemory: Integer;
    function GetSavePeriodInFrames: Integer;
    procedure SetHistoryEnabled(AValue: Boolean);
    procedure SetMaxNumberOfSnapshotsInMemory(AValue: Integer);
    procedure SetSavePeriodInFrames(AValue: Integer);
    procedure UpdateDisplayKeyBack;
    procedure OnEditClick(Sender: TObject);
    procedure SetKeyBack(AValue: Word);

  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; Vertical: Boolean);

    procedure UpdateValuesFromHistoryOptions(
      constref ASnapshotHistoryOptions: TSnapshotHistoryOptions);
    procedure UpdateSnapshotHistoryOptionsFromValues(
      var ASnapshotHistoryOptions: TSnapshotHistoryOptions);

    class function CreateForAllOptions(AOptionsDialog: TFormOptions): TFrameHistorySnapshotOptions;

    property HistoryEnabled: Boolean read GetHistoryEnabled write SetHistoryEnabled;
  end;

implementation

{$R *.lfm}

{ TFrameHistorySnapshotOptions }

procedure TFrameHistorySnapshotOptions.SpinEdit2Change(Sender: TObject);
var
  N: Integer;
  S: String;
begin
  if not SpinEdit2Changing then begin
    SpinEdit2Changing := True;
    try
      N := SpinEdit2.Value div SpinEdit2.Increment;

      S := IntToStr(N) + ' second';
      if N <> 1 then
        S := S + 's';
      Label8.Caption := S + ')';
    finally
      SpinEdit2Changing := False;
    end;
  end;
end;

procedure TFrameHistorySnapshotOptions.Panel10Paint(Sender: TObject);
var
  R: TRect;
begin
  R := Panel10.ClientRect;
  R.Inflate(0, 0, -1, -1);
  Panel10.Canvas.Pen.Color := $00ABCDDE;
  Panel10.Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top),
     R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);
end;

procedure TFrameHistorySnapshotOptions.SpinEdit2EditingDone(Sender: TObject);
begin
  SetSavePeriodInFrames(SpinEdit2.Value);
end;

procedure TFrameHistorySnapshotOptions.UpdateDisplayKeyBack;
var
  S: String;
begin
  if FKeyBack > 0 then begin
    S := UnitKeyMaps.DecodePCKey(FKeyBack);
    if S = '' then begin
      S := Trim(FKeyBack.ToString);
    end;
    Label7.Caption := S;
  end else
    Label7.Caption := '  ';
end;

function TFrameHistorySnapshotOptions.GetHistoryEnabled: Boolean;
begin
  Result := CheckBoxAutoCreateSnapshots.Checked;
end;

function TFrameHistorySnapshotOptions.GetMaxNumberOfSnapshotsInMemory: Integer;
begin
  Result := SpinEdit1.Value;
end;

function TFrameHistorySnapshotOptions.GetSavePeriodInFrames: Integer;
begin
  Result := SpinEdit2.Value;
end;

procedure TFrameHistorySnapshotOptions.SetHistoryEnabled(AValue: Boolean);
begin
  CheckBoxAutoCreateSnapshots.Checked := AValue;
end;

procedure TFrameHistorySnapshotOptions.SetMaxNumberOfSnapshotsInMemory(
  AValue: Integer);
begin
  SpinEdit1.Value := AValue;
end;

procedure TFrameHistorySnapshotOptions.SetSavePeriodInFrames(AValue: Integer);
begin
  AValue := AValue div SpinEdit2.Increment;
  SpinEdit2.Value := AValue * SpinEdit2.Increment;
end;

procedure TFrameHistorySnapshotOptions.OnEditClick(Sender: TObject);
var
  W: Word;
begin
  W := FKeyBack;
  if UnitFormPressAKey.TFormPressAKey.ShowFormPressAKey(W) then begin
    SetKeyBack(W);
  end;
end;

procedure TFrameHistorySnapshotOptions.SetKeyBack(AValue: Word);
begin
  if FKeyBack <> AValue then begin
    FKeyBack := AValue;
    UpdateDisplayKeyBack;
  end;
end;

constructor TFrameHistorySnapshotOptions.Create(AOwner: TComponent);
begin
  Create(AOwner, False);
end;

constructor TFrameHistorySnapshotOptions.Create(AOwner: TComponent;
  Vertical: Boolean);
begin
  inherited Create(AOwner);

  Name := UnitCommon.TCommonFunctions.GlobalObjectNameGenerator(Self);
  Caption := 'Auto saving in-memory snapshots';

  Width := 1;
  if Vertical then begin
    Panel2.BorderSpacing.Left := 8;

    CheckBoxAutoCreateSnapshots.AnchorParallel(akTop, 4, Panel8);
    Panel9.AnchorToNeighbour(akTop, 12, CheckBoxAutoCreateSnapshots);
    Panel9.AnchorParallel(akLeft, 21, Panel8);

    Panel14.BorderSpacing.Left := Panel9.BorderSpacing.Left;
    Panel4.AnchorParallel(akTop, 9, Panel14);
    Panel5.AnchorToNeighbour(akTop, 15, Panel4);
    Panel5.AnchorParallel(akLeft, 0, Panel14);
  end;

  Panel10.Color := $00DAE8EF;
  Panel9.BorderStyle := bsNone;
  Panel10.BorderStyle := bsNone;
  Panel11.BorderStyle := bsNone;
  Panel14.BorderStyle := bsNone;

  SpinEdit2Changing := False;
  SpinEdit2.MaxValue := UnitHistorySnapshots.TSnapshotHistoryOptions.MaxSavePeriodInFrames;
  SpinEdit1.MaxValue := UnitHistorySnapshots.TSnapshotHistoryOptions.MaxMaxNumberOfSnapshotsInMemory;
  SpinEdit2.Increment := UnitHistorySnapshots.TSnapshotHistoryOptions.IncrementStep;
  SpinEdit2.MinValue := SpinEdit2.Increment;
  SpinEdit1.MinValue := 1;

  LabelEdit := TCommonFunctionsLCL.CreateLinkLabel(Panel11, 'Edit');
  LabelEdit.Anchors := [];
  LabelEdit.AnchorParallel(akTop, 0, Panel11);
  LabelEdit.AnchorParallel(akLeft, 0, Panel11);

  LabelEdit.Parent := Panel11;
  LabelEdit.OnClick := @OnEditClick;
  Panel11.AutoSize := True;

  FKeyBack := 1;
  SetKeyBack(0);
end;

procedure TFrameHistorySnapshotOptions.UpdateValuesFromHistoryOptions(constref
  ASnapshotHistoryOptions: TSnapshotHistoryOptions);
begin
  SetKeyBack(ASnapshotHistoryOptions.KeyGoBack);
  SetMaxNumberOfSnapshotsInMemory(
    ASnapshotHistoryOptions.MaxNumberOfSnapshotsInMemory);
  SetSavePeriodInFrames(
    ASnapshotHistoryOptions.SavePeriodInFrames);
end;

procedure TFrameHistorySnapshotOptions.UpdateSnapshotHistoryOptionsFromValues(
  var ASnapshotHistoryOptions: TSnapshotHistoryOptions);
begin
  ASnapshotHistoryOptions.KeyGoBack := FKeyBack;
  ASnapshotHistoryOptions.MaxNumberOfSnapshotsInMemory := GetMaxNumberOfSnapshotsInMemory;
  ASnapshotHistoryOptions.SavePeriodInFrames := GetSavePeriodInFrames;
end;

class function TFrameHistorySnapshotOptions.CreateForAllOptions(
  AOptionsDialog: TFormOptions): TFrameHistorySnapshotOptions;
var
  Fm: TFrameHistorySnapshotOptions;
begin
  Result := nil;
  Fm := TFrameHistorySnapshotOptions.Create(AOptionsDialog, True);
  try
    AOptionsDialog.AddAnOptionControl(Fm, 'Auto saving');

    Result := Fm;
  except
    FreeAndNil(Fm);
  end;
end;

end.

