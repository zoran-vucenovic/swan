unit UnitFormHistorySnapshots;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, UnitFileSna, UnitHistorySnapshots,
  CommonFunctionsLCL, UnitKeyMaps, UnitFormPressAKey, UnitControlScreenBitmap,
  UnitSpectrumColoursBGRA, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Buttons, LMessages;

type
  TFormHistorySnapshots = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel10Paint(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit2EditingDone(Sender: TObject);
  strict private
    HistorySnapshots: TSnapshotHistoryQueue;
    FKeyBack: Word;
    LabelEdit: TCustomLabel;
    ScrollBox: TScrollBox;
    BgraColours: TBGRAColours;
    PrevCtrl: TControl;
    SpinEdit2Changing: Boolean;
    FTimer: TFlashTimer;

    function GetSelectedSnapshotNegOffs: Integer;
    procedure AfterShow(Data: PtrInt);
    procedure OnEditClick(Sender: TObject);
    procedure UpdateDisplayKeyBack;
    procedure SetKeyBack(AValue: Word);
    procedure UpdateCheckEnabled;
    procedure FillSnapshots;
    procedure TimerOnTimer(Sender: TObject);
    procedure ActiveCtrlChg(Sender: TObject; LastControl: TControl);
  public
    class function ShowFormHistorySnapshots(
      AHistorySnapshots: TSnapshotHistoryQueue;
      var ASnapshotHistoryOptions: TSnapshotHistoryOptions;
      const ABGRAColours: TBGRAColours;
      out AHistoryEnabled: Boolean): Boolean;
  end;

implementation

{$R *.lfm}

type
  TSnapshotControl = class(TCustomControl)
  private
    Lab: TLabel;
    CtrlScrBmp: TControlScreenBitmap;
    InternalNegativeSnapNo: Integer;
    AB: Array of Byte;
    LastSelected: Boolean;

    procedure UserInputEvent(Sender: TObject; Msg: Cardinal);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
  end;

{ TSnapshotControl }

procedure TSnapshotControl.UserInputEvent(Sender: TObject; Msg: Cardinal);
begin
  case Msg of
    LM_LBUTTONDOWN, LM_RBUTTONDOWN:
      if MouseInClient and CanSetFocus then
        SetFocus;
  end;
end;

constructor TSnapshotControl.Create(AOwner: TComponent);
var
  F: TFont;
  FD: TFontData;
begin
  inherited Create(AOwner);

  LastSelected := False;
  AutoSize := False;
  Color := clWhite;
  SetLength(AB, 0);
  CtrlScrBmp := nil;

  Lab := TLabel.Create(nil);
  Lab.Alignment := TAlignment.taRightJustify;

  F := Lab.Font;
  FD := GetFontData(F.Handle);
  F.BeginUpdate;
  try
    F.Name := FD.Name;
    F.Style := F.Style + [fsBold];
    F.Height := (FD.Height * 4) div 3;
    F.Color := clNavy;
  finally
    F.EndUpdate;
  end;

  Lab.Font := F;
end;

destructor TSnapshotControl.Destroy;
begin
  Application.RemoveAllHandlersOfObject(Self);
  Lab.Free;
  CtrlScrBmp.Free;
  SetLength(AB, 0);

  inherited Destroy;
end;

procedure TSnapshotControl.Paint;
var
  R: TRect;

  procedure DrawRect(); inline;
  begin
    Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top), R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);
  end;

begin
  inherited Paint;

  if LastSelected then begin
    Canvas.Pen.Color := clNavy;
    R := ClientRect;
    R.Inflate(-2, -2);
    DrawRect();
    R.Inflate(-1, -1);
    DrawRect();
  end;

end;

{ TFormHistorySnapshots }

procedure TFormHistorySnapshots.FormCreate(Sender: TObject);
begin
  PrevCtrl := nil;
  ScrollBox := nil;
  HistorySnapshots := nil;
  Panel1.Enabled := False;
  FTimer := nil;
  FKeyBack := 0;

  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;

  SpinEdit2Changing := False;
  SpinEdit2.Enabled := False;
  SpinEdit2.MaxValue := UnitHistorySnapshots.TSnapshotHistoryOptions.MaxSavePeriodInFrames;
  SpinEdit1.MaxValue := UnitHistorySnapshots.TSnapshotHistoryOptions.MaxMaxNumberOfSnapshotsInMemory;
  SpinEdit2.Increment := UnitHistorySnapshots.TSnapshotHistoryOptions.IncrementStep;
  SpinEdit2.MinValue := SpinEdit2.Increment;
  SpinEdit1.MinValue := 1;

  BitBtn1.AutoSize := True;
  BitBtn2.AutoSize := True;
  Panel9.BorderStyle := bsNone;
  Panel10.BorderStyle := bsNone;
  Panel11.BorderStyle := bsNone;
  Panel14.BorderStyle := bsNone;

  UpdateDisplayKeyBack;
  LabelEdit := TCommonFunctionsLCL.CreateLinkLabel(Panel11, 'Edit');
  LabelEdit.Anchors := [];
  LabelEdit.AnchorParallel(akTop, 0, Panel11);
  LabelEdit.AnchorParallel(akLeft, 0, Panel11);

  LabelEdit.Parent := Panel11;
  LabelEdit.OnClick := @OnEditClick;
  Panel11.AutoSize := True;
end;

procedure TFormHistorySnapshots.CheckBox1Change(Sender: TObject);
begin
  UpdateCheckEnabled;
end;

procedure TFormHistorySnapshots.FormDestroy(Sender: TObject);
begin
  if Assigned(FTimer) then
    FTimer.Enabled := False;
  Screen.RemoveAllHandlersOfObject(Self);
  ScrollBox.Free;
  FTimer.Free;
end;

procedure TFormHistorySnapshots.FormShow(Sender: TObject);
begin
  Panel10.Color := $00DAE8EF;
  //FillSnapshots;
  AfterShow(2);
end;

procedure TFormHistorySnapshots.Panel10Paint(Sender: TObject);
var
  R: TRect;
begin
  inherited Paint;

  R := Panel10.ClientRect;
  R.Inflate(0, 0, -1, -1);
  Panel10.Canvas.Pen.Color := $00ABCDDE;
  Panel10.Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top),
     R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);
end;

procedure TFormHistorySnapshots.SpinEdit2Change(Sender: TObject);
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

procedure TFormHistorySnapshots.SpinEdit2EditingDone(Sender: TObject);
var
  N: Integer;
begin
  N := SpinEdit2.Value div SpinEdit2.Increment;
  SpinEdit2.Value := N * SpinEdit2.Increment;
end;

function TFormHistorySnapshots.GetSelectedSnapshotNegOffs: Integer;
begin
  Result := 1;
  if PrevCtrl is TSnapshotControl then
    Result := TSnapshotControl(PrevCtrl).InternalNegativeSnapNo;
end;

procedure TFormHistorySnapshots.SetKeyBack(AValue: Word);
begin
  if FKeyBack <> AValue then begin
    FKeyBack := AValue;
    UpdateDisplayKeyBack;
  end;
end;

procedure TFormHistorySnapshots.UpdateCheckEnabled;
begin
  CheckBox2.Enabled := Assigned(HistorySnapshots)
    and (HistorySnapshots.Count > 0)
    and (PrevCtrl is TSnapshotControl)
    ;

  if CheckBox2.State <> TCheckBoxState.cbUnchecked then begin
    if not CheckBox2.Enabled then begin
      CheckBox2.State := TCheckBoxState.cbGrayed;
    end else begin
      CheckBox2.State := TCheckBoxState.cbChecked;
    end;
  end;
end;

procedure TFormHistorySnapshots.FillSnapshots;
var
  N, J: Integer;
  SnapshotControl: TSnapshotControl;
  C, CC, CC2: TCustomControl;
  St: TStream;
  Sz: TSize;
  S: String;

begin
  if Assigned(HistorySnapshots) xor Assigned(ScrollBox) then begin
    FreeAndNil(FTimer);
    PrevCtrl := nil;
    Screen.RemoveHandlerActiveControlChanged(@ActiveCtrlChg);
    J := 0;

    if Assigned(HistorySnapshots) then begin
      ScrollBox := TScrollBox.Create(nil);
      ScrollBox.BorderStyle := bsNone;

      C := nil;
      N := 0;
      while N < HistorySnapshots.Count do begin
        if HistorySnapshots.GetStream(-N, St) then begin
          SnapshotControl := TSnapshotControl.Create(ScrollBox);
          SnapshotControl.Anchors := [];
          SnapshotControl.AnchorParallel(akLeft, 2, ScrollBox);
          if C = nil then
            SnapshotControl.AnchorParallel(akTop, 2, ScrollBox)
          else
            SnapshotControl.AnchorToNeighbour(akTop, 4, C);

          C := SnapshotControl;

          SetLength(SnapshotControl.AB, 6912);
          if TSnapshotInternal48.GetScreenMem(St, SnapshotControl.AB) then begin

            CC2 := TCustomControl.Create(SnapshotControl);
            CC2.Caption := '';
            CC2.BorderStyle := bsSingle;

            SnapshotControl.CtrlScrBmp := TControlScreenBitmap.CreateNewControl(
              CC2, BGRAColours, @SnapshotControl.AB[0]
            );
            SnapshotControl.InternalNegativeSnapNo := -N;

            if J = 0 then begin
              TCommonFunctionsLCL.CalculateTextSize(SnapshotControl.Lab.Font, 'w.' + IntToStr(HistorySnapshots.Count), Sz);
              FTimer := TFlashTimer.Create(nil);
              FTimer.OnTimer := @TimerOnTimer;
            end;

            Inc(J);
            SnapshotControl.CtrlScrBmp.Timer := FTimer;
            SnapshotControl.Lab.Caption := J.ToString + '.';
            SnapshotControl.Lab.Constraints.MinWidth := Sz.cx;

            CC := TCustomControl.Create(SnapshotControl);
            CC.Anchors := [];
            CC.AnchorParallel(akLeft, 0, SnapshotControl);
            CC.AnchorParallel(akTop, 0, SnapshotControl);
            CC.BorderSpacing.Around := 5;

            SnapshotControl.Lab.Anchors := [];
            SnapshotControl.Lab.AnchorVerticalCenterTo(CC);
            SnapshotControl.Lab.AnchorParallel(akLeft, 2, CC);

            CC2.Anchors := [];
            CC2.AnchorVerticalCenterTo(CC);
            CC2.AnchorToNeighbour(akLeft, 0, SnapshotControl.Lab);
            CC2.BorderSpacing.Around := 3;

            SnapshotControl.CtrlScrBmp.Anchors := [];
            SnapshotControl.CtrlScrBmp.AnchorParallel(akTop, 0, CC2);
            SnapshotControl.CtrlScrBmp.AnchorParallel(akLeft, 0, CC2);

            SnapshotControl.CtrlScrBmp.Parent := CC2;
            SnapshotControl.Lab.Parent := CC;
            CC2.AutoSize := True;
            CC2.Parent := CC;
            CC.AutoSize := True;
            CC.Parent := SnapshotControl;
            SnapshotControl.AutoSize := True;
            SnapshotControl.TabStop := True;

            SnapshotControl.Parent := ScrollBox;
            Application.AddOnUserInputHandler(@(SnapshotControl.UserInputEvent));
          end;
        end;     
        Inc(N);
      end;
    end;

    if J <= 0 then
      FreeAndNil(ScrollBox);

    if Assigned(ScrollBox) then begin
      if J = 1 then
        S := ''
      else
        S := 's, most recent on top';
      Label5.Caption := Format(
        'Currently saved in memory (%d snapshot%s):', [J, S]);
      ScrollBox.Anchors := [];
      ScrollBox.AnchorParallel(akTop, 0, Panel13);
      ScrollBox.AnchorParallel(akLeft, 0, Panel13);
      ScrollBox.AnchorParallel(akRight, 0, Panel13);
      ScrollBox.AnchorParallel(akBottom, 0, Panel13);

      ScrollBox.Parent := Panel13;
      Screen.AddHandlerActiveControlChanged(@ActiveCtrlChg);

      FTimer.Enabled := True;
    end;
  end;
  if ScrollBox = nil then
    Label5.Caption := 'No snapshots saved.';
end;

procedure TFormHistorySnapshots.TimerOnTimer(Sender: TObject);
begin
  FTimer.FlashState := not FTimer.FlashState;
  ScrollBox.Invalidate;
end;

procedure TFormHistorySnapshots.ActiveCtrlChg(Sender: TObject;
  LastControl: TControl);
var
  C: TControl;
  SC: TSnapshotControl;
begin
  if ScrollBox = nil then
    Exit;

  SC := nil;
  C := LastControl;
  while Assigned(C) do begin
    if C is TSnapshotControl then begin
      SC := TSnapshotControl(C);
      Break;
    end;

    C := C.Parent;
  end;

  if Assigned(SC) then begin
    if PrevCtrl is TSnapshotControl then
      TSnapshotControl(PrevCtrl).LastSelected := False;
    SC.LastSelected := True;
    PrevCtrl := SC;            
    ScrollBox.ScrollInView(SC);
    ScrollBox.Invalidate;
  end;
  UpdateCheckEnabled;
end;

procedure TFormHistorySnapshots.AfterShow(Data: PtrInt);
var
  N: Integer;
begin
  AutoSize := False;
  if BitBtn1.Width > BitBtn2.Width then
    N := BitBtn1.Width
  else
    N := BitBtn2.Width;
  BitBtn1.Constraints.MinWidth := N;
  BitBtn2.Constraints.MinWidth := N;

  CommonFunctionsLCL.TCommonFunctionsLCL.AdjustFormPos(Self);
  if Data > 0 then begin
    AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1)
  end else begin
    Constraints.MinHeight := 0;
    Constraints.MaxHeight := 0;
    BitBtn1.AutoSize := False;
    BitBtn2.AutoSize := False;
    FillSnapshots;
    SpinEdit2.Enabled := True;
    Panel1.Enabled := True;
  end;

end;

procedure TFormHistorySnapshots.OnEditClick(Sender: TObject);
var
  W: Word;
begin
  W := FKeyBack;
  if UnitFormPressAKey.TFormPressAKey.ShowFormPressAKey(W) then begin
    SetKeyBack(W);
  end;
end;

procedure TFormHistorySnapshots.UpdateDisplayKeyBack;
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

class function TFormHistorySnapshots.ShowFormHistorySnapshots(
  AHistorySnapshots: TSnapshotHistoryQueue;
  var ASnapshotHistoryOptions: TSnapshotHistoryOptions;
  const ABGRAColours: TBGRAColours; out AHistoryEnabled: Boolean): Boolean;
var
  F: TFormHistorySnapshots;
  N: Integer;
begin
  Result := False;
  AHistoryEnabled := Assigned(AHistorySnapshots);
  F := TFormHistorySnapshots.Create(nil);
  try
    F.HistorySnapshots := AHistorySnapshots;
    F.CheckBox1.Checked := AHistoryEnabled;
    F.SetKeyBack(ASnapshotHistoryOptions.KeyGoBack);
    F.SpinEdit1.Value := ASnapshotHistoryOptions.MaxNumberOfSnapshotsInMemory;
    F.SpinEdit2.Value := ASnapshotHistoryOptions.SavePeriodInFrames;
    F.BgraColours := ABGRAColours;

    F.UpdateCheckEnabled;

    if F.ShowModal = mrOK then begin
      ASnapshotHistoryOptions.KeyGoBack := F.FKeyBack;
      ASnapshotHistoryOptions.MaxNumberOfSnapshotsInMemory := F.SpinEdit1.Value;
      ASnapshotHistoryOptions.SavePeriodInFrames := F.SpinEdit2.Value;
      AHistoryEnabled := F.CheckBox1.Checked;

      if F.CheckBox2.Enabled and F.CheckBox2.Checked then begin
        N := F.GetSelectedSnapshotNegOffs;
        if N <= 0 then
          F.HistorySnapshots.LoadSnapshot(N, False);
      end;
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

end.

