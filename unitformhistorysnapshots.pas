unit UnitFormHistorySnapshots;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, UnitSnapshotFiles, UnitHistorySnapshots,
  CommonFunctionsLCL, UnitControlScreenBitmap, UnitSpectrumColoursBGRA,
  UnitFrameHistorySnapshotOptions, UnitSwanButtonPanel, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, LMessages, ButtonPanel;

type
  TFormHistorySnapshots = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    CheckBox2: TCheckBox;
    Label5: TLabel;
    Panel1: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel15: TPanel;
    Panel2: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  strict private
    HistorySnapshots: TSnapshotHistoryQueue;
    ScrollBox: TScrollBox;
    BgraColours: TBGRAColours;
    PrevCtrl: TControl;
    FTimer: TFlashTimer;
    FrameHistorySnapshotOptions: TFrameHistorySnapshotOptions;

    procedure CheckBoxHistoryEnabledOnChange(Sender: TObject);
    function GetSelectedSnapshotNegOffs: Integer;
    procedure AfterShow(Data: PtrInt);
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
  otherwise
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
var
  ButtonPanel: TControl;
begin
  BorderIcons := BorderIcons - [TBorderIcon.biMaximize, TBorderIcon.biMinimize];
  PrevCtrl := nil;
  ScrollBox := nil;
  HistorySnapshots := nil;
  Panel1.Enabled := False;
  FTimer := nil;

  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;

  FrameHistorySnapshotOptions := TFrameHistorySnapshotOptions.Create(Panel2);
  FrameHistorySnapshotOptions.Parent := Panel2;
  Panel2.AutoSize := True;

  FrameHistorySnapshotOptions.CheckBoxAutoCreateSnapshots.OnChange := @CheckBoxHistoryEnabledOnChange;

  ButtonPanel := TSwanButtonPanel.Create(Self);
  ButtonPanel.AnchorParallel(akLeft, 6, Panel15);
  ButtonPanel.AnchorParallel(akRight, 6, Panel15);
  ButtonPanel.AnchorParallel(akBottom, 6, Panel15);
  ButtonPanel.Parent := Panel15;
  Panel1.AnchorToNeighbour(akBottom, 6, ButtonPanel);
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
  //FillSnapshots;
  AfterShow(2);
end;

procedure TFormHistorySnapshots.CheckBoxHistoryEnabledOnChange(Sender: TObject);
begin
  UpdateCheckEnabled;
end;

function TFormHistorySnapshots.GetSelectedSnapshotNegOffs: Integer;
begin
  Result := 1;
  if PrevCtrl is TSnapshotControl then
    Result := TSnapshotControl(PrevCtrl).InternalNegativeSnapNo;
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
          if TSnapshotInternalSwan.GetScreenMem(St, SnapshotControl.AB) then begin

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
begin
  AutoSize := False;

  CommonFunctionsLCL.TCommonFunctionsLCL.AdjustFormPos(Self);
  if Data > 0 then begin
    AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1)
  end else begin
    Screen.BeginWaitCursor;
    try
      Constraints.MinHeight := 0;
      Constraints.MaxHeight := 0;
      FillSnapshots;
      Panel1.Enabled := True;
    finally
      Screen.EndWaitCursor;
    end;
    Self.SelectNext(Panel1, True, True);
  end;

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
    F.FrameHistorySnapshotOptions.HistoryEnabled := AHistoryEnabled;
    F.FrameHistorySnapshotOptions.UpdateValuesFromHistoryOptions(
      ASnapshotHistoryOptions);
    F.BgraColours := ABGRAColours;

    F.UpdateCheckEnabled;
    F.HelpKeyword := 'help:Autosaving-snapshots';
    if F.ShowModal = mrOK then begin
      F.FrameHistorySnapshotOptions.UpdateSnapshotHistoryOptionsFromValues(
        ASnapshotHistoryOptions);
      AHistoryEnabled := F.FrameHistorySnapshotOptions.HistoryEnabled;

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

