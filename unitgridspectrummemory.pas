unit UnitGridSpectrumMemory;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, UnitMemory, UnitDisassembler, UnitCommonSpectrum,
  CommonFunctionsLCL, UnitCommon, UnitDebugger, Grids, Graphics, Forms,
  Controls, Clipbrd;

type
  TSpectrumMemoryGrid = class(TCustomDrawGrid)
  strict private
    FDebugger: TDebugger;
    FDisassembler: TDisassembler;
    FFollowPc: Boolean;
    FPc: Word;
    FSp: Word;

    procedure SetDisassembler(ADisasembler: TDisassembler);
    procedure SetFollowPc(const AValue: Boolean);
    procedure SetPc(const AValue: Word);
    procedure SetSp(const AValue: Word);
    procedure FocusAddressInGrid(Addr: Word);
    procedure FillGrid();
    function TextForCell(aCol, aRow: Integer): RawByteString;
    procedure SetDebugger(AValue: TDebugger);
    procedure FBreakpointsOnChange(Sender: TObject);
    class procedure CreateBmps();

  strict private
    class var
      BmpPC, BmpSP, BmpBk: TBitmap;

  private
    class procedure Init; static;
    class procedure Final; static;
  protected
    procedure DoCopyToClipboard; override;
    procedure DrawColumnText(aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState); override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState
      ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    function MouseButtonAllowed(Button: TMouseButton): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure JumpTo(Addr: Word);

    property Pc: Word read FPc write SetPc;
    property Sp: Word read FSp write SetSp;
    property FollowPc: Boolean read FFollowPc write SetFollowPc;
    property Disassembler: TDisassembler read FDisassembler write SetDisassembler;
    property Debugger: TDebugger read FDebugger write SetDebugger;
  end;

implementation

{ TSpectrumMemoryGrid }

procedure TSpectrumMemoryGrid.SetPc(const AValue: Word);
begin
  if FPc <> AValue then begin
    FPc := AValue;

    if FFollowPc then
      FocusAddressInGrid(AValue);
  end;
end;

procedure TSpectrumMemoryGrid.SetFollowPc(const AValue: Boolean);
begin
  if FFollowPc <> AValue then begin
    FFollowPc := AValue;
    if AValue then
      FocusAddressInGrid(FPc);
  end;
end;

procedure TSpectrumMemoryGrid.SetSp(const AValue: Word);
begin
  if FSp <> AValue then begin
    FSp := AValue;
  end;
end;

procedure TSpectrumMemoryGrid.FocusAddressInGrid(Addr: Word);
begin
  TCommonFunctionsLCL.RowInView(Self, FixedRows + Addr);
end;

procedure TSpectrumMemoryGrid.DrawColumnText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  aRect.Left := aRect.Left + 4;
  inherited DrawColumnText(aCol, aRow, aRect, aState);
end;

procedure TSpectrumMemoryGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

  procedure DrawBmp(ABmp: TBitmap; R: TRect);
  begin
    Canvas.Draw(R.Right - ABmp.Width - 2, R.Top + (R.Height - ABmp.Height) div 2, ABmp);
  end;

var
  S: RawByteString;
  TS: TTextStyle;
  Re: TRect;
  Addr: Word;

begin
  inherited DrawCell(aCol, aRow, aRect, aState);

  if aRow < FixedRows then
    Exit;

  if aCol < FixedCols then begin
    aRow := aRow - FixedRows;

    if aRow = FPc then begin
      if aRow <> FSp then begin
        DrawBmp(BmpPC, aRect);
      end else begin 
        Re := aRect;
        DrawBmp(BmpSP, Re);
        Re.Offset(-BmpSP.Width, 0);
        DrawBmp(BmpPc, Re);
      end;
    end else if aRow = FSp then begin
      DrawBmp(BmpSP, aRect);
    end;

    Addr := aRow;

    if FDebugger.IsBreakpoint(Addr) then begin
      Canvas.Draw(aRect.Left + 3, aRect.Top + (aRect.Height - BmpBk.Height) div 2, BmpBk);
    end;
  end else begin
    if aState * [gdSelected, gdFocused] <> [] then begin
      Re := Rect(aRect.Left + 1, aRect.Top + 1, aRect.Right - 2, aRect.Bottom - 2);
      Canvas.Pen.Color := GridLineColor;
      Canvas.Pen.Width := 2;
      Canvas.Polyline([Re.TopLeft, Point(Re.Right, Re.Top), Re.BottomRight, Point(Re.Left, Re.Bottom), Re.TopLeft]);
    end;

    S := TextForCell(aCol, aRow);
    if S = '' then
      Exit;

    TS := Canvas.TextStyle;
    TS.Alignment := TAlignment.taLeftJustify;

    aRect.Left := aRect.Left + 6;
    Canvas.TextRect(ARect, aRect.Left, aRect.Top, S, TS);
  end;

end;

procedure TSpectrumMemoryGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  W: Word;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = TMouseButton.mbLeft then begin
    if TCommonFunctionsLCL.GridMouseToCellRegular(Self, Point(X, Y), P) then begin
      if (P.X >= 0) and (P.Y >= 0) then begin
        if P.X < FixedCols then begin
          if P.Y >= FixedRows then begin
            W := P.Y - FixedRows;
            if FDebugger.IsBreakPoint(W) then
              FDebugger.RemoveBreakpoint(W)
            else
              FDebugger.AddBreakpoint(W);

            //Invalidate;
          end;
        end;
      end;
    end;
  end;
end;

function TSpectrumMemoryGrid.MouseButtonAllowed(Button: TMouseButton): boolean;
begin
  Result := Button in [mbLeft, mbRight, mbMiddle];
end;

constructor TSpectrumMemoryGrid.Create(AOwner: TComponent);
var
  C: TGridColumn;
  I: Integer;
  Sz: TSize;
begin
  inherited Create(AOwner);

  Color := TColor($f9f9f9);

  FocusRectVisible := False;
  Font := TCommonFunctionsLCL.GetMonoFont();

  BorderStyle := bsNone;
  TitleFont.Name := 'default';
  FFollowPc := True;
  FDisassembler := nil;
  RowCount := FixedRows;
  TCommonFunctionsLCL.CalculateTextSize(TitleFont, 'Ag(', Sz);
  RowHeights[0] := RowHeights[0] + Sz.cy;
  ExtendedSelect := False;

  Options := Options
    + [goColSizing, goColMoving]
    - [goEditing, goHorzLine, goRangeSelect, goVertLine, goDrawFocusSelected,
       goFixedVertLine, goFixedHorzLine];

  for I := 0 to 3 do begin
    C := Columns.Add;
    C.ReadOnly := True;
    C.Title.MultiLine := True;
    C.Title.Layout := tlCenter;
    C.Tag := I;
    case I of
      0:
        begin
          C.Width := 93;
          C.Title.Caption := 'address' + LineEnding + 'hex (dec)';
        end;
      1:
        begin
          C.Width := 80;
          C.Title.Caption := 'value' + LineEnding + 'hex (dec)';
        end;
      2:
        begin
          C.Width := 140;
          C.Title.Caption := 'opcode' + LineEnding + 'in hex';
        end;
      3:
        begin
          C.Width := 140;
          C.Title.Caption := 'instruction';
        end;
    end;
  end;

  CreateBmps();
  ColWidths[0] := BmpSP.Width + BmpPC.Width + BmpBk.Width + 6;
end;

procedure TSpectrumMemoryGrid.JumpTo(Addr: Word);
begin
  if Addr < FixedRows + RowCount then begin
    Row := FixedRows + Addr;
    CommonFunctionsLCL.TCommonFunctionsLCL.RowInView(Self, Row);
    Invalidate;
  end;
end;

procedure TSpectrumMemoryGrid.DoCopyToClipboard;
var
  S: RawByteString;
begin
  //inherited DoCopyToClipboard;
  S := TextForCell(Col, Row);
  if S <> '' then
    Clipboard.AsText := S;
end;

function TSpectrumMemoryGrid.TextForCell(aCol, aRow: Integer): RawByteString;
var
  R: Integer;
  C: Integer;
  W: Word;
  B: Byte;
  I: Integer;

begin
  Result := '';

  if aCol >= FixedCols then begin
    R := aRow - FixedRows;
    if R >= 0 then begin
      C := ColumnFromGridColumn(aCol).Tag;
      case C of
        0:
          Result := R.ToHexString(4) + ' (' + R.ToString + ')';
        1:
          begin
            W := R;
            B := FDisassembler.Memory.ReadByte(W);
            Result := B.ToHexString(2) + ' (' + B.ToString + ')';
          end;
        2:
          begin
            W := R;
            FDisassembler.Dissasemble(W, C);
            for I := 1 to C do begin
              if I > 1 then
                Result := Result + TCommonFunctions.NonBreakSpace;
              Result := Result + FDisassembler.Memory.ReadByte(W).ToHexString(2);
              {$push}{$Q-}{$R-}
              Inc(W);
              {$pop}
            end;
          end;
        3:
          begin
            W := R;
            Result := FDisassembler.Dissasemble(W, C);
          end;

      otherwise
      end;
    end;
  end;
end;

class procedure TSpectrumMemoryGrid.Init;
begin
  BmpPC := nil;
  BmpBk := nil;
  BmpSP := nil;
end;

class procedure TSpectrumMemoryGrid.Final;
begin
  BmpBk.Free;
  BmpSP.Free;
  BmpPC.Free;
end;

class procedure TSpectrumMemoryGrid.CreateBmps;
var
  F: TFont;
  BackgroundColour: TColor;
  TS: TTextStyle;

  function CreateBmp(const S: RawByteString): TBitmap;
  var
    Sz: TSize;
    R: Integer;
  begin
    Result := TBitmap.Create;
    Result.Canvas.Font := F;

    TCommonFunctionsLCL.CalculateTextSize(Result.Canvas.Font, S, Sz);
    Result.SetSize(Sz.cx + 2, Sz.cy + 1);

    Result.Canvas.Brush.Style := bsSolid;
    Result.Canvas.Brush.Color := clFuchsia;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));

    Result.Canvas.Brush.Color := BackgroundColour;
    Result.TransparentColor := clFuchsia;
    Result.TransparentMode := tmFixed;
    Result.Masked := True;

    Result.Canvas.Pen.Color := Result.Canvas.Brush.Color;

    R := Result.Height;
    if Result.Width < R then
      R := Result.Width;

    R := R * 2 div 5;
    Result.Canvas.RoundRect(0, 0, Result.Width, Result.Height, R, R);

    TS := Result.Canvas.TextStyle;
    TS.Alignment := TAlignment.taCenter;
    TS.Layout := TTextLayout.tlCenter;
    TS.Opaque := False;

    Result.Canvas.TextRect(Rect(0, 0, Result.Width, Result.Height), 0, 0, S, TS);
  end;

begin
  if BmpPC = nil then begin
    F := TFont.Create;
    try
      F.Style := F.Style + [fsBold];

      F.Size := 8;
      F.Color := TColor($e2ffff);
      BackgroundColour := TColor($2534da);
      BmpBk := CreateBmp('b');

      F.Color := clNavy;
      BackgroundColour := clYellow;
      BmpPC := CreateBmp('PC');
      BmpSP := CreateBmp('SP');

    finally
      F.Free;
    end;
  end;
end;

procedure TSpectrumMemoryGrid.SetDebugger(AValue: TDebugger);
begin
  if FDebugger = AValue then Exit;
  if Assigned(FDebugger) then
    FDebugger.RemoveOnBreakPointChangeHandlerOfObject(Self);
  FDebugger := AValue;

  if Assigned(FDebugger) then
    FDebugger.AddOnBreakpointChange(@FBreakpointsOnChange);
end;

procedure TSpectrumMemoryGrid.FBreakpointsOnChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TSpectrumMemoryGrid.SetDisassembler(ADisasembler: TDisassembler);
var
  Mem: TMemory;
begin
  Mem := nil;
  if Assigned(FDisassembler) then
    Mem := FDisassembler.Memory;
  FDisassembler := ADisasembler;
  if (FDisassembler = nil) or (Mem <> FDisassembler.Memory) then
    FillGrid;
end;

procedure TSpectrumMemoryGrid.FillGrid();
var
  Mem: TMemory;
  MemSize: Integer;

begin
  if Assigned(FDisassembler) then
    Mem := FDisassembler.Memory
  else
    Mem := nil;

  if Mem <> nil then begin
    MemSize := Mem.CurrentlyMappedMemSizeKB * TCommonSpectrum.KiloByte;
  end else
    MemSize := 0;

  RowCount := FixedRows + MemSize;
end;

initialization
  TSpectrumMemoryGrid.Init;

finalization
  TSpectrumMemoryGrid.Final;

end.
