unit UnitOptions;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, CommonFunctionsLCL, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ButtonPanel, Grids, LCLType, LCLIntf, StdCtrls,
  LazMethodList;

type

  TFormOptions = class(TForm)
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

      TGridSelector = class(TCustomDrawGrid)
      end;
  private
    CloseQueryList: TMethodList;

    FOptionControlsCount: Integer;
    FOptionControls: Array of TCtrlRec;
    FInternalSettingRow: Boolean;
    Grid: TGridSelector;
    CellInnerBorder: Integer;

    procedure AfterShow(Data: PtrInt);
    procedure SetCurrentControlNumber(ANumber: Integer);
    //function GetCurrentControlNumber(): Integer;
    procedure GridOnSelectCell(Sender: TObject; {%H-}aCol, aRow: Integer;
          var {%H-}CanSelect: Boolean);
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

    property OptionControlsCount: Integer read FOptionControlsCount;
  end;

implementation

{$R *.lfm}

{ TFormOptions }

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  Label1.Caption := ' ';

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
    - [goDrawFocusSelected, goFixedHorzLine, goFixedVertLine, goHorzLine, goVertLine, goRangeSelect]
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
  //  Selected := False;
    Grid.Canvas.Font.Style := Grid.Canvas.Font.Style - [fsBold];
  end else begin
    C := Panel5.Color;
    //Selected := True;
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
begin
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
  //Panel6.Color := clWhite;
  Grid.Constraints.MinWidth := Panel2.ClientWidth;

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
  //Grid.ParentFont := True;
  Grid.Font.Style := Grid.Font.Style - [fsBold];

  Sz.cx := Sz2.cx * 3 div 10 + 5;
  Sz2.cy := Sz2.cy * 17 div 10 + 12;
  if Sz.cx < 5 then
    Sz.cx := 5;

  //ShowMessage(IntToStr(I));
  Sz2.cx := Sz2.cx + Sz.cx + GetSystemMetrics(SM_CXVSCROLL);
  CellInnerBorder := Sz.cx div 2;

  if Grid.DefaultRowHeight < Sz2.cy then
    Grid.DefaultRowHeight := Sz2.cy;
  if Grid.ClientWidth < Sz2.cx then
    Grid.ClientWidth := Sz2.cx;

  AfterShow(2);
end;

procedure TFormOptions.GridOnSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
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

    //Grid.BorderSpacing.Top := Panel2.ScreenToClient(Panel3.ClientToScreen(Point(0, 0))).Y;
    Panel3.Constraints.MinWidth := 0;
    Panel3.Constraints.MinHeight := 0;

    Grid.OnSelectCell := @GridOnSelectCell;
    SetCurrentControlNumber(0);
  end;
  TCommonFunctionsLCL.AdjustFormPos(Self);
end;

procedure TFormOptions.SetCurrentControlNumber(ANumber: Integer);
var
  C: TControl;
  I: Integer;
begin
  if FOptionControlsCount <= 0 then
    ANumber := -1
  else if (ANumber < 0) or (ANumber >= FOptionControlsCount) then
    ANumber := 0;

  Panel3.Color := Grid.Color;

  for I := 0 to FOptionControlsCount - 1 do begin
    if I <> ANumber then
      FOptionControls[I].Control.Hide
    else begin
      FInternalSettingRow := True;
      try
        Grid.Row := Grid.FixedRows + I;
        C := FOptionControls[I].Control;
        Label1.Caption := FOptionControls[I].LabTitle;
        C.Show;
        Panel3.Color := C.GetRGBColorResolvingParent;
      finally
        FInternalSettingRow := False;
      end;
    end;
  end;
end;

//function TFormOptions.GetCurrentControlNumber: Integer;
//begin
//  Result := Grid.Row - Grid.FixedRows;
//end;

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
    Result.SetCurrentControlNumber(0);
  end;
end;

function TFormOptions.CloseQuery: boolean;
var
  I: Integer;
begin
  Result := inherited CloseQuery;

  I := 0;
  while Result and (I < CloseQueryList.Count) do begin
    TCloseQueryEvent(CloseQueryList[I])(Self, Result);
    Inc(I);
  end;
end;

procedure TFormOptions.RemoveAllHandlersOfObject(AnObject: TObject);
var
  I: Integer;
begin
  inherited RemoveAllHandlersOfObject(AnObject);

  for I := 0 to CloseQueryList.Count - 1 do
    CloseQueryList.RemoveAllMethodsOfObject(AnObject);
  if CloseQueryList.Count = 0 then
    FreeAndNil(CloseQueryList);
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
    //TCustomStringGrid(Grid).Cells[Grid.FixedCols, Grid.FixedRows + FOptionControlsCount] := R.LabSelect;

    if Length(FOptionControls) <= FOptionControlsCount then
      SetLength(FOptionControls, FOptionControlsCount + 1);
    FOptionControls[FOptionControlsCount] := R;
    C.Visible := FOptionControlsCount <= 0;
    Inc(FOptionControlsCount);

    C.Anchors := [];
    C.AnchorParallel(akLeft, 0, Panel3);
    //C.AnchorParallel(akTop, 0, Panel3);
    C.AnchorToNeighbour(akTop, 0, Panel6);

    C.Parent := Panel3;
  end;
end;

end.

