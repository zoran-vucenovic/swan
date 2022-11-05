unit UnitFormJoystickSetup;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils, UnitKeyMaps, CommonFunctionsLCL, UnitFormPressAKey,
  UnitJoystick, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, StdCtrls;

type
  TFormJoystickSetup = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  strict private
    type

      TJoystickKey = class(TCustomControl)
      public
        type
          TCheckSameKeysEvent = function(var ADirection: TJoystick.TJoystickDirection; AKey: Word): Boolean of object;
      strict private
        type
          TLabelKeyControl = class(TCustomControl)
          private
            FHasFrame: Boolean;
            procedure SetHasFrame(AValue: Boolean);
          public
            constructor Create(AOwner: TComponent); override;
            procedure Paint; override;
            property HasFrame: Boolean read FHasFrame write SetHasFrame;
          end;
      strict private
        FDirection: TJoystick.TJoystickDirection;
        FOnCheckSameKeys: TCheckSameKeysEvent;
        LabelDirection: TLabel;
        LabelKey: TLabel;
        LabelKeyControl: TLabelKeyControl;
        LabelEdit: TCustomLabel;
        FKey: Word;

        procedure SetDirection(AValue: TJoystick.TJoystickDirection);
        procedure SetKey(AValue: Word);
        procedure UpdateLabelKey;
        procedure UpdateLabelDirection;
        procedure OnEditClick(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); override;
        constructor Create(AOwner: TComponent; ADirection: TJoystick.TJoystickDirection);
        property Key: Word read FKey write SetKey;
        property Direction: TJoystick.TJoystickDirection read FDirection write SetDirection;
        property OnCheckSameKeys: TCheckSameKeysEvent read FOnCheckSameKeys write FOnCheckSameKeys;
      end;

      TJoystickTypeObj = class(TObject)
      public
        JT: TJoystick.TJoystickType;
      end;

  strict private
    JoystickKeys: Array[TJoystick.TJoystickDirection] of TJoystickKey;

    function GetJoystickType: TJoystick.TJoystickType;
    procedure SetJoystickType(const AValue: TJoystick.TJoystickType);
    procedure GetDirectionsKeys(out AValue: TJoystick.TJoystickDirectionsKeys);
    procedure SetDirectionsKeys(const AValue: TJoystick.TJoystickDirectionsKeys);
    function CheckSameKeys(var ADirection: TJoystick.TJoystickDirection; AKey: Word): Boolean;
    procedure AfterShow(Data: PtrInt);
    procedure FillCombo;
    procedure ClearCombo;

    property JoystickType: TJoystick.TJoystickType read GetJoystickType write SetJoystickType;
  public
    class function ShowJoystickOptionsDialog(var AJoystickType: TJoystick.TJoystickType;
      var AKeys: TJoystick.TJoystickDirectionsKeys; var AEnabled: Boolean): Boolean;
  end;

implementation

{$R *.lfm}

{ TFormJoystickSetup.TJoystickKey.TLabelKeyControl }

procedure TFormJoystickSetup.TJoystickKey.TLabelKeyControl.SetHasFrame(
  AValue: Boolean);
begin
  if FHasFrame = AValue then Exit;
  FHasFrame := AValue;
  Invalidate;
end;

constructor TFormJoystickSetup.TJoystickKey.TLabelKeyControl.Create(
  AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHasFrame := False;
end;

procedure TFormJoystickSetup.TJoystickKey.TLabelKeyControl.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if FHasFrame then begin
    R := Rect(0, 0, ClientWidth - 1, ClientHeight - 1);
    Canvas.Pen.Color := $00ABCDDE;
    Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top), R.BottomRight,
      Point(R.Left, R.Bottom), R.TopLeft]);
  end;
end;

{ TFormJoystickSetup.TJoystickKey }

procedure TFormJoystickSetup.TJoystickKey.UpdateLabelKey;
var
  S: String;
begin
  if FKey <> 0 then begin
    S := DecodePCKey(FKey);
    if S = '' then
      S := FKey.ToString;
    LabelKey.Font.Style := LabelKey.Font.Style + [fsBold] - [fsItalic];
    LabelKey.Caption := S;
    LabelKeyControl.Color := $00DAE8EF;
    LabelKeyControl.HasFrame := True;
  end else begin
    LabelKey.Font.Style := LabelKey.Font.Style - [fsBold] + [fsItalic];
    LabelKey.Caption := '<none>';
    LabelKeyControl.ParentColor := True;
    LabelKeyControl.HasFrame := False;
  end;

end;

procedure TFormJoystickSetup.TJoystickKey.UpdateLabelDirection;
begin
  LabelDirection.Caption := TJoystick.DirectionToString(FDirection);
end;

procedure TFormJoystickSetup.TJoystickKey.OnEditClick(Sender: TObject);
var
  W: Word;
  D: TJoystick.TJoystickDirection;
  S, S1: String;
begin
  W := Key;
  if TFormPressAKey.ShowFormPressAKey(W) then begin
    D := Direction;
    if Assigned(FOnCheckSameKeys) and FOnCheckSameKeys(D, W) then begin
      S := DecodePCKey(W);
      if S = '' then
        S := W.ToString;
      S1 := TJoystick.DirectionToString(D);
      if D = TJoystick.TJoystickDirection.diFire then
        S1 := S1 + ' button'
      else
        S1 := S1 + ' direction';
      MessageDlg('Key ' + S + ' is already assigned to ' + S1 + '.',
        mtError, [mbClose], 0);
    end else
      SetKey(W);
  end;
end;

procedure TFormJoystickSetup.TJoystickKey.SetKey(AValue: Word);
begin
  FKey := AValue;
  UpdateLabelKey;
end;

procedure TFormJoystickSetup.TJoystickKey.SetDirection(
  AValue: TJoystick.TJoystickDirection);
begin
  FDirection := AValue;
  UpdateLabelDirection;
end;

constructor TFormJoystickSetup.TJoystickKey.Create(AOwner: TComponent;
  ADirection: TJoystick.TJoystickDirection);
var
  C: TCustomControl;
begin
  inherited Create(AOwner);

  LabelDirection := TLabel.Create(Self);
  LabelDirection.Anchors := [];
  LabelDirection.AnchorParallel(akTop, 0, Self);
  LabelDirection.AnchorParallel(akLeft, 0, Self);

  C := TCustomControl.Create(Self);
  C.Anchors := [];
  C.AnchorToNeighbour(akTop, 1, LabelDirection);
  C.AnchorParallel(akLeft, 1, Self);

  LabelKeyControl := TLabelKeyControl.Create(C);
  LabelKeyControl.Anchors := [];
  LabelKeyControl.AnchorVerticalCenterTo(C);
  LabelKeyControl.AnchorParallel(akLeft, 0, C);

  LabelKey := TLabel.Create(LabelKeyControl);
  LabelKey.Anchors := [];
  LabelKey.AnchorVerticalCenterTo(LabelKeyControl);
  LabelKey.AnchorParallel(akLeft, 0, LabelKeyControl);
  LabelKey.BorderSpacing.Around := 2;
  LabelKey.Font.Size := 10;

  LabelEdit := TCommonFunctionsLCL.CreateLinkLabel(C, 'Edit');
  LabelEdit.Anchors := [];
  LabelEdit.AnchorVerticalCenterTo(C);
  LabelEdit.AnchorToNeighbour(akLeft, 3, LabelKeyControl);

  SetKey(0);
  SetDirection(ADirection);

  LabelKeyControl.AutoSize := True;
  LabelKey.Parent := LabelKeyControl;
  C.AutoSize := True;
  LabelKeyControl.Parent := C;
  LabelEdit.Parent := C;
  LabelDirection.Parent := Self;
  C.Parent := Self;

  AutoSize := True;
  LabelEdit.OnClick := @OnEditClick;
end;

constructor TFormJoystickSetup.TJoystickKey.Create(AOwner: TComponent);
begin
  Create(AOwner, TJoystick.TJoystickDirection.diFire);
end;

{ TFormJoystickSetup }

procedure TFormJoystickSetup.FormCreate(Sender: TObject);
var
  JD: TJoystick.TJoystickDirection;
  JK: TJoystickKey;
begin
  Label2.Font.Color := clMaroon;
  Label3.Font.Color := clMaroon;
  Label3.Font.Style := Label3.Font.Style + [fsBold];
  Label3.Caption := 'Note: ';
  Label2.Caption := 'When Joystick is enabled, then key mappings to joystick, defined here, take precedence over key mappings to spectrum keys.';
  Panel1.BevelOuter := bvNone;
  Panel2.BevelOuter := bvNone;

  FillCombo;

  for JD := Low(TJoystick.TJoystickDirection) to High(TJoystick.TJoystickDirection) do begin
    JK := TJoystickKey.Create(Panel2, JD);
    JoystickKeys[JD] := JK;
    JK.Anchors := [];
    case JD of
      TJoystick.TJoystickDirection.diUp:
        begin
          JK.AnchorParallel(akTop, 0, Panel2);
          JK.AnchorHorizontalCenterTo(Panel2);
        end;
      TJoystick.TJoystickDirection.diDown:
        begin
          JK.AnchorParallel(akBottom, 0, Panel2);
          JK.AnchorHorizontalCenterTo(Panel2);
        end;
      TJoystick.TJoystickDirection.diLeft:
        begin
          JK.AnchorVerticalCenterTo(Panel2);
          JK.AnchorParallel(akLeft, 0, Panel2);
        end;
      TJoystick.TJoystickDirection.diRight:
        begin
          JK.AnchorVerticalCenterTo(Panel2);
          JK.AnchorParallel(akRight, 0, Panel2);
        end;
    otherwise
      JK.AnchorVerticalCenterTo(Panel2);
      JK.AnchorHorizontalCenterTo(Panel2);
    end;

    JK.Parent := Panel2;
    JK.OnCheckSameKeys := @CheckSameKeys;
  end;

  Panel6.AutoSize := True;
  Self.AutoSize := True;

  AfterShow(-1);
end;

procedure TFormJoystickSetup.FormDestroy(Sender: TObject);
begin
  ClearCombo;
end;

procedure TFormJoystickSetup.FormShow(Sender: TObject);
begin
  AfterShow(2);
end;

function TFormJoystickSetup.GetJoystickType: TJoystick.TJoystickType;
var
  Obj: TObject;
begin
  Result := Low(TJoystick.TJoystickType);
  if ComboBox1.ItemIndex >= 0 then begin
    Obj := ComboBox1.Items.Objects[ComboBox1.ItemIndex];
    if Obj is TJoystickTypeObj then begin
      Result := TJoystickTypeObj(Obj).JT;
    end;
  end;
end;

procedure TFormJoystickSetup.SetJoystickType(
  const AValue: TJoystick.TJoystickType);
var
  Obj: TObject;
  I: Integer;
begin
  for I := 0 to ComboBox1.Items.Count - 1 do begin
    Obj := ComboBox1.Items.Objects[I];
    if Obj is TJoystickTypeObj then
      if TJoystickTypeObj(Obj).JT = AValue then begin
        ComboBox1.ItemIndex := I;
        Break;
      end;
  end;
end;

procedure TFormJoystickSetup.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  JD: TJoystick.TJoystickDirection;
begin
  if (ModalResult = mrOK) and CheckBox1.Checked then begin
    for JD := Low(TJoystick.TJoystickDirection) to High(TJoystick.TJoystickDirection) do begin
      if JoystickKeys[JD].Key = 0 then begin
        CanClose := False;
        MessageDlg('Joystick cannot be enabled, because not all keys are assigned.'
          + LineEnding + LineEnding
          + 'To be able to enable joystick, you have to assign a key to each joystick direction/button.',
          mtError, [mbClose], 0);
        Break;
      end;
    end;
  end;
end;

procedure TFormJoystickSetup.GetDirectionsKeys(out
  AValue: TJoystick.TJoystickDirectionsKeys);
var
  JD: TJoystick.TJoystickDirection;
begin
  for JD := Low(TJoystick.TJoystickDirection) to High(TJoystick.TJoystickDirection) do begin
    AValue[JD] := JoystickKeys[JD].Key;
  end;
end;

procedure TFormJoystickSetup.SetDirectionsKeys(
  const AValue: TJoystick.TJoystickDirectionsKeys);
var
  JD: TJoystick.TJoystickDirection;
begin
  for JD := Low(TJoystick.TJoystickDirection) to High(TJoystick.TJoystickDirection) do begin
    JoystickKeys[JD].Key := AValue[JD];
  end;
end;

function TFormJoystickSetup.CheckSameKeys(var ADirection: TJoystick.TJoystickDirection;
  AKey: Word): Boolean;
var
  JD: TJoystick.TJoystickDirection;
begin
  for JD := Low(TJoystick.TJoystickDirection) to High(TJoystick.TJoystickDirection) do begin
    if JD <> ADirection then begin
      if JoystickKeys[JD].Key = AKey then begin
        ADirection := JD;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

procedure TFormJoystickSetup.AfterShow(Data: PtrInt);
begin                                          
  AutoSize := False;
  TCommonFunctionsLCL.AdjustFormPos(Self);
  if Data = 0 then begin
    Panel2.AnchorParallel(akRight, 0, Panel3);
    Panel2.AnchorParallel(akBottom, 0, Panel3);
    Exit;
  end;

  AutoSize := True;
  if Data > 0 then begin
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end;

  if Constraints.MinHeight < Height then
    Constraints.MinHeight := Height;
  if Constraints.MinWidth < Width then
    Constraints.MinWidth := Width;

end;

procedure TFormJoystickSetup.FillCombo;
var
  JT: TJoystick.TJoystickType;
  S: String;
  JTO: TJoystickTypeObj;
begin
  ClearCombo;
  for JT := Low(TJoystick.TJoystickType) to High(TJoystick.TJoystickType) do begin
    S := TJoystick.JoystickTypeToString(JT);
    JTO := TJoystickTypeObj.Create;
    JTO.JT := JT;
    ComboBox1.Items.AddObject(S, JTO);
  end;
end;

procedure TFormJoystickSetup.ClearCombo;
var
  I: Integer;
begin
  for I := 0 to ComboBox1.Items.Count - 1 do begin
    ComboBox1.Items.Objects[I].Free;
    ComboBox1.Items.Objects[I] := nil;
  end;
  ComboBox1.Clear;
end;

class function TFormJoystickSetup.ShowJoystickOptionsDialog(
  var AJoystickType: TJoystick.TJoystickType;
  var AKeys: TJoystick.TJoystickDirectionsKeys; var AEnabled: Boolean): Boolean;
var
  F: TFormJoystickSetup;

begin
  Result := False;
  F := TFormJoystickSetup.Create(nil);
  try
    F.SetDirectionsKeys(AKeys);
    F.JoystickType := AJoystickType;
    F.CheckBox1.Checked := AEnabled;
    //
    if F.ShowModal = mrOK then begin
      F.GetDirectionsKeys(AKeys);
      AJoystickType := F.JoystickType;
      AEnabled := F.CheckBox1.Checked;
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

end.

