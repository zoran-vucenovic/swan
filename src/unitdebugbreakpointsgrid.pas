unit UnitDebugBreakpointsGrid;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, Grids, Controls, Graphics, UnitDebugger, UnitCommon,
  CommonFunctionsLCL;


type
  TGridBreakpoints = class(TCustomDrawGrid)
  private
    FDebugger: TDebugger;

    procedure SetDebugger(AValue: TDebugger);
    procedure FBreakpointsOnChange(Sender: TObject);
  protected
    procedure DrawColumnText(aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState); override;
    function MouseButtonAllowed(Button: TMouseButton): boolean; override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState
      ); override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FillGrid;
    property Debugger: TDebugger read FDebugger write SetDebugger;
  end;


implementation

{ TGridBreakpoints }

procedure TGridBreakpoints.SetDebugger(AValue: TDebugger);
begin
  if FDebugger <> AValue then begin
    if Assigned(FDebugger) then
      FDebugger.RemoveOnBreakPointChangeHandlerOfObject(Self);
    FDebugger := AValue;
    if Assigned(FDebugger) then
      FDebugger.AddOnBreakpointChange(@FBreakpointsOnChange);

    FillGrid;
  end;
end;

procedure TGridBreakpoints.FBreakpointsOnChange(Sender: TObject);
begin
  FillGrid;
end;

procedure TGridBreakpoints.DrawColumnText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  aRect.Left := aRect.Left + 4 * (aCol + 1 - FixedCols);
  inherited DrawColumnText(aCol, aRow, aRect, aState);
end;

function TGridBreakpoints.MouseButtonAllowed(Button: TMouseButton): boolean;
begin
  Result := TCommonFunctionsLCL.GridMouseButtonAllowed(Self, Button);
end;

procedure TGridBreakpoints.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  I: Integer;
  TS: TTextStyle;
  S: RawByteString;
  W: Word;
  Re: TRect;

begin
  inherited DrawCell(aCol, aRow, aRect, aState);

  if (aRow >= FixedRows) and (aCol >= FixedCols) then begin

    if aState * [gdSelected, gdFocused] <> [] then begin
      Re := Rect(aRect.Left + 1, aRect.Top + 1, aRect.Right - 2, aRect.Bottom - 2);
      Canvas.Pen.Color := GridLineColor;
      Canvas.Pen.Width := 2;
      Canvas.Polyline([Re.TopLeft, Point(Re.Right, Re.Top), Re.BottomRight, Point(Re.Left, Re.Bottom), Re.TopLeft]);
    end;

    aRect.Left := aRect.Left + 4;

    TS := Canvas.TextStyle;
    TS.Layout := TTextLayout.tlCenter;

    I := aRow - FixedRows;

    case aCol - FixedCols of
      0:
        begin
          W := FDebugger.Breakpoints.Keys[I];
          S :=  W.ToHexString(4) + ' (' + W.ToString + ')';
        end;
      1:
        begin
          S := FDebugger.Breakpoints.Data[I].ConditionExpr;
          if S = '' then begin
            S := ' <none>';
            Canvas.Font.Color := clMaroon;
            Canvas.Font.Style := Canvas.Font.Style + [fsItalic];
          end;
        end;
    otherwise
      S := '';
    end;

    Canvas.TextRect(aRect, aRect.Left, aRect.Top, S, TS);
  end;
end;

procedure TGridBreakpoints.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  inherited PrepareCanvas(aCol, aRow, aState);
  //if aRow < FixedRows then
  //  Canvas.Brush.Color := Color;
end;

constructor TGridBreakpoints.Create(AOwner: TComponent);
var
  C: TGridColumn;
  I: Integer;
  Sz: TSize;
begin
  inherited Create(AOwner);

  FDebugger := nil;
  BorderStyle := bsNone;

  FocusRectVisible := False;
  FixedCols := 0;

  TCommonFunctionsLCL.CalculateTextSize(TitleFont, 'Ag(', Sz);
  RowHeights[0] := RowHeights[0] + Sz.cy;

  Options := Options
    - [goColSizing, goEditing, goHorzLine, goRangeSelect, goVertLine,
       goDrawFocusSelected, goColMoving, goFixedVertLine, goFixedHorzLine];
  Flat := True;

  for I := 0 to 1 do begin
    {$ifdef ConditionalBreakpointsNotYetImplemented}
    if I = 1 then
      Break;
    {$endif}
    C := Columns.Add;
    C.ReadOnly := True;
    C.SizePriority := 0;

    C.Title.MultiLine := True;
    C.Title.Layout := TTextLayout.tlCenter;
    case I of
      0:
        begin
          C.Font := TCommonFunctionsLCL.GetMonoFont();
          C.Font.Color := Self.Font.Color;
          TCommonFunctionsLCL.CalculateTextSize(C.Font, '0000 (00000) ', Sz);
          C.Width := Sz.cx;
          C.Title.Caption := 'address' + LineEnding + 'hex (dec)';
        end;
      1:
        begin
          C.Width := 173;
          C.Title.Caption := 'condition';
        end;
    otherwise
    end;
  end;

  Self.AutoFillColumns := True;
  RowCount := FixedRows;
end;

destructor TGridBreakpoints.Destroy;
begin
  SetDebugger(nil);

  inherited Destroy;
end;

procedure TGridBreakpoints.FillGrid;
begin
  BeginUpdate;
  try
    if FDebugger = nil then begin
      RowCount := FixedRows
    end else begin
      RowCount := FixedRows + FDebugger.Breakpoints.Count;
    end;

  finally
    EndUpdate();
  end;
  AfterMoveSelection(-2, -2);
end;

end.

