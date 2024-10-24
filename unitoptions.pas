unit UnitOptions;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, CommonFunctionsLCL, UnitCommon, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, ButtonPanel, Grids, LCLType, LCLIntf, StdCtrls,
  LazMethodList;

type

  TFormOptions = class(TForm, IFormAddCloseQuery)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    type
      TCtrlRec = record
        Control: TControl;
        LabSelect: String;
        LabTitle: String;
      end;

  { #todo : Instead of using grid, think of designing a custom control }
      TGridSelector = class(TCustomDrawGrid)
      protected
        procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      public
        property GridState: TGridState read FGridState write FGridState;
      end;

  private
    CloseQueryList: TMethodList;

    FOptionControlsCount: Integer;
    FOptionControls: Array of TCtrlRec;
    FInternalSettingRow: Boolean;
    Grid: TGridSelector;
    CellInnerBorder: Integer;
    FCurrentControl: TControl;

    procedure AfterShow(Data: PtrInt);
    procedure SetCurrentControlNumber(ANumber: Integer);
    procedure GridOnSelectCell(Sender: TObject; {%H-}aCol, aRow: Integer;
          var {%H-}CanSelect: Boolean);
    procedure GridOnSelection(Sender: TObject; aCol, aRow: Integer);
    procedure GridOnDrawCell(Sender: TObject; {%H-}aCol, aRow: Integer;
          aRect: TRect; {%H-}aState: TGridDrawState);
    procedure GridOnPrepareCanvas(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer;
          aState: TGridDrawState);
  public
    procedure AddAnOptionControl(const C: TControl;
          const ALabelSelect: String = ''; const ALabelTitle: String = '');
    class function CreateOptionsDialog(const AControls: Array of TControl): TFormOptions;
    function CloseQuery: boolean; override;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddCloseQuery(C: TCloseQueryEvent);
    procedure SetCurrentControlByClass(AControlClass: TControlClass);

    property CurrentControl: TControl read FCurrentControl;
    property OptionControlsCount: Integer read FOptionControlsCount;
  end;

implementation

{$R *.lfm}

{ TFormOptions }

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);

  CloseQueryList := nil;
  Label1.Caption := ' ';

  FCurrentControl := nil;
  CellInnerBorder := 0;
  FInternalSettingRow := False;
  FOptionControlsCount := 0;
  SetLength(FOptionControls, 0);
  Panel1.Caption := '';
  Panel2.Caption := '';
  Panel3.Caption := '';
  Panel4.Caption := '';
  Panel5.Caption := '';
  Panel6.Caption := '';
  Panel7.Caption := '';
  Panel1.BevelOuter := bvNone;
  Panel2.BevelOuter := bvNone;
  Panel3.BevelOuter := bvNone;
  Panel4.BevelOuter := bvNone;
  Panel5.BevelOuter := bvNone;
  Panel6.BevelOuter := bvNone;
  Panel7.BevelOuter := bvNone;
  Panel7.Height := 2;

  Grid := TGridSelector.Create(Self);

  Grid.AnchorParallel(akLeft, 0, Panel2);
  Grid.AnchorParallel(akTop, 0, Panel2);
  //Grid.AnchorParallel(akRight, 0, Panel2);
  Grid.AnchorParallel(akBottom, 0, Panel2);

  Grid.BorderStyle := bsNone;
  Grid.TitleStyle := TTitleStyle.tsNative;
  Grid.FixedCols := 0;
  Grid.FixedRows := 0;
  Grid.ColCount := 1;
  Grid.AutoFillColumns := True;
  Grid.Flat := True;
  Grid.Options := Grid.Options
    + [goRowSelect]
    - [goDrawFocusSelected, goFixedHorzLine, goFixedVertLine, goHorzLine,
       goVertLine, goRangeSelect]
    ;
  Grid.ExtendedSelect := False;
  Grid.AllowOutboundEvents := False;
  Grid.DefaultDrawing := False;
  Grid.OnDrawCell := @GridOnDrawCell;
  Grid.OnPrepareCanvas := @GridOnPrepareCanvas;
  Grid.Color := clWhite;

  Panel2.AutoSize := True;
  Grid.Parent := Panel2;
end;

procedure TFormOptions.GridOnPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  R, G, B: Integer;
  C: TColor;
begin
  if aState * [gdSelected, gdFocused] = [] then begin
    C := Grid.Color;
    Grid.Canvas.Font.Style := Grid.Canvas.Font.Style - [fsBold];
  end else begin
    C := Panel5.Color;
    Grid.Canvas.Font.Style := Grid.Canvas.Font.Style + [fsBold];
  end;
  Grid.Canvas.Brush.Color := C;
  R := Red(C);
  G := Green(C);
  B := Blue(C);
  Grid.Canvas.Font.Color := TCommonFunctionsLCL.GetBestContrastColorForFont(R, G, B);

  Grid.Canvas.Pen.Color := clBlue;
  Grid.Canvas.Pen.Width := 3;
end;

procedure TFormOptions.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for I := Low(FOptionControls) to High(FOptionControls) do
    Application.RemoveAsyncCalls(FOptionControls[I].Control);
  Application.RemoveAsyncCalls(Self);
  FreeAndNil(CloseQueryList);
end;

procedure TFormOptions.FormShow(Sender: TObject);
var
  Sz, Sz2: TSize;
  I: Integer;
  FD: TFontData;
  FontHeight: Integer;

begin
  Grid.ParentFont := False;

  Grid.Font.Style := Grid.Font.Style + [fsBold];

  FD := GetFontData(Self.Font.Reference.Handle);
  FontHeight := MulDiv(FD.Height, 6, 5);
  if Abs(FontHeight) < 10 then
    FontHeight := (FontHeight * 10) div Abs(FontHeight);

  Grid.Font.Height := FontHeight;
  Label1.Font := Grid.Font;

  Sz2 := Default(TSize);
  for I := 0 to FOptionControlsCount - 1 do begin
    TCommonFunctionsLCL.CalculateTextSize(Grid.Font, FOptionControls[I].LabSelect, Sz);
    if Sz.cx > Sz2.cx then
      Sz2.cx := Sz.cx;
    if Sz.cy > Sz2.cy then
      Sz2.cy := Sz.cy;
  end;
  Grid.Font.Style := Grid.Font.Style - [fsBold];

  Sz.cx := Sz2.cx div 10 + 5;
  Sz2.cy := Sz2.cy * 7 div 5 + 7;

  Sz2.cx := Sz2.cx + Sz.cx + GetSystemMetrics(SM_CXVSCROLL);
  CellInnerBorder := Sz.cx div 2;

  Grid.DefaultRowHeight := Sz2.cy;
  Grid.ClientWidth := Sz2.cx;

  AfterShow(2);
end;

procedure TFormOptions.GridOnSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  C: TControl;
  N: Integer;
begin
  if CanSelect and (aRow <> Grid.Row) then begin
    if FOptionControlsCount <= 0 then begin
      CanSelect := False;
    end else begin
      N := aRow - Grid.FixedRows;
      if (N < 0) or (N >= FOptionControlsCount) then begin
        N := 0;
      end;

      C := FOptionControls[N].Control;

      CanSelect := Assigned(C) and (C <> FCurrentControl);
      if CanSelect then begin
        if FCurrentControl is ICheckStateValid then begin
          CanSelect := (FCurrentControl as ICheckStateValid).IsStateValid;
          if not CanSelect then begin
            if Grid.GridState = TGridState.gsSelecting then
              Grid.GridState:= TGridState.gsNormal;
          end;
        end;
      end;
    end;

  end;
end;

procedure TFormOptions.GridOnSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if not FInternalSettingRow then
    SetCurrentControlNumber(aRow - Grid.FixedRows);

end;

procedure TFormOptions.GridOnDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  TS: TTextStyle;
begin
  Grid.Canvas.FillRect(aRect);

  TS := Canvas.TextStyle;
  TS.Opaque := False;
  TS.Alignment := TAlignment.taLeftJustify;
  TS.Layout := TTextLayout.tlCenter;
  TS.EndEllipsis := False;
  TS.Wordbreak := False;
  TS.SingleLine := True;
  TS.ShowPrefix := False;

  Grid.Canvas.TextRect(aRect, aRect.Left + CellInnerBorder, aRect.Right, FOptionControls[aRow - Grid.FixedRows].LabSelect, TS);
end;

procedure TFormOptions.AfterShow(Data: PtrInt);
var
  I: Integer;
  C: TControl;
begin
  AutoSize := False;

  for I := 0 to FOptionControlsCount - 1 do begin
    C := FOptionControls[I].Control;
    if C.Width >= Panel3.Width then
      Panel3.Constraints.MinWidth := C.Width;
    if C.Height >= Panel3.Height then
      Panel3.Constraints.MinHeight := C.Height;
  end;

  if Data > 0 then begin
    AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end else begin
    Panel3.ParentColor := False;
    Grid.ParentColor := False;
    Grid.Color := Panel4.GetRGBColorResolvingParent;
    Panel3.Color := Grid.Color;
    Panel5.ParentColor := False;
    Panel5.Color := $7a7a7a;
    Panel6.ParentColor := False;
    Panel6.Color := Grid.Color;
    Panel7.Color := Panel5.Color;

    for I := 0 to FOptionControlsCount - 1 do begin
      FOptionControls[I].Control.AnchorParallel(akRight, 0, Panel3);
    end;

    Constraints.MinWidth := Panel2.Width + Panel3.Constraints.MinWidth * 3 div 5;
    Constraints.MinHeight := Height div 2;

    Panel3.Constraints.MinWidth := 0;
    Panel3.Constraints.MinHeight := 0;

    Grid.OnSelectCell := @GridOnSelectCell;
    Grid.OnSelection := @GridOnSelection;
    if FCurrentControl = nil then
      SetCurrentControlNumber(0);
  end;
  TCommonFunctionsLCL.AdjustFormPos(Self);
end;

procedure TFormOptions.SetCurrentControlNumber(ANumber: Integer);
var
  C: TControl;

begin
  if FOptionControlsCount <= 0 then begin
    C := nil;
  end else begin
    if (ANumber < 0) or (ANumber >= FOptionControlsCount) then begin
      ANumber := 0;
    end;

    C := FOptionControls[ANumber].Control;
  end;

  if Assigned(FCurrentControl) then
    FCurrentControl.Visible := C = FCurrentControl;

  if Assigned(C) then begin
    if C <> FCurrentControl then begin
      Panel3.Color := Grid.Color;

      FInternalSettingRow := True;
      try
        Grid.Row := Grid.FixedRows + ANumber;
        C.Visible := True;
        Label1.Caption := FOptionControls[ANumber].LabTitle;
        Panel3.Color := C.GetRGBColorResolvingParent;
      finally
        FInternalSettingRow := False;
      end;
    end;
  end else
    Label1.Caption := ' ';

  FCurrentControl := C;
end;

class function TFormOptions.CreateOptionsDialog(
  const AControls: array of TControl): TFormOptions;
var
  I: Integer;
begin
  Result := TFormOptions.Create(nil);
  if Assigned(Result) then begin
    Result.Grid.RowCount := Result.Grid.FixedRows + Length(AControls);

    for I := Low(AControls) to High(AControls) do begin
      Result.AddAnOptionControl(AControls[I]);
    end;
    Result.Grid.RowCount := Result.Grid.FixedRows + Result.FOptionControlsCount;
  end;
end;

function TFormOptions.CloseQuery: boolean;
var
  I: Integer;
begin
  Result := inherited CloseQuery;

  if Assigned(CloseQueryList) then begin
    I := 0;
    while Result and (I < CloseQueryList.Count) do begin
      TCloseQueryEvent(CloseQueryList[I])(Self, Result);
      Inc(I);
    end;
  end;
end;

procedure TFormOptions.RemoveAllHandlersOfObject(AnObject: TObject);
begin
  inherited RemoveAllHandlersOfObject(AnObject);

  if Assigned(CloseQueryList) then begin
    CloseQueryList.RemoveAllMethodsOfObject(AnObject);
    if CloseQueryList.Count = 0 then
      FreeAndNil(CloseQueryList);
  end;
end;

procedure TFormOptions.AddCloseQuery(C: TCloseQueryEvent);
begin
  if Assigned(C) then begin
    if CloseQueryList = nil then begin
      CloseQueryList := TMethodList.Create;
    end;

    CloseQueryList.Add(TMethod(C));
  end;
end;

procedure TFormOptions.SetCurrentControlByClass(AControlClass: TControlClass);
var
  I: Integer;
begin
  if Assigned(AControlClass) then begin
    for I := 0 to FOptionControlsCount - 1 do begin
      if FOptionControls[I].Control.ClassType = AControlClass then begin
        SetCurrentControlNumber(I);
        Break;
      end;
    end;
  end;
end;

procedure TFormOptions.AddAnOptionControl(const C: TControl;
  const ALabelSelect: String; const ALabelTitle: String);
var
  R: TCtrlRec;
begin
  if Assigned(C) then begin
    R.LabSelect := Trim(ALabelSelect);
    R.LabTitle := Trim(ALabelTitle);

    if R.LabSelect = '' then
      R.LabSelect := C.Caption;
    if R.LabTitle = '' then
      R.LabTitle := C.Caption;

    R.Control := C;
    if FOptionControlsCount + Grid.FixedRows >= Grid.RowCount then begin
      Grid.RowCount := Grid.FixedRows + FOptionControlsCount + 1;
    end;

    if Length(FOptionControls) <= FOptionControlsCount then
      SetLength(FOptionControls, FOptionControlsCount + 1);
    FOptionControls[FOptionControlsCount] := R;

    C.Visible := False;
    Inc(FOptionControlsCount);

    C.Anchors := [];
    C.AnchorParallel(akLeft, 0, Panel3);
    C.AnchorToNeighbour(akTop, 0, Panel6);

    C.Parent := Panel3;
  end;
end;

{ TFormOptions.TGridSelector }

procedure TFormOptions.TGridSelector.MouseMove(Shift: TShiftState; X, Y: Integer
  );
begin
  // prevent moving selection with the mouse
  if GridState = TGridState.gsSelecting then begin
    GridState := TGridState.gsNormal;
    inherited MouseMove(Shift, X, Y);
    GridState := TGridState.gsSelecting;
  end else
    inherited MouseMove(Shift, X, Y);
end;

end.

