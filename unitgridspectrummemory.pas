unit UnitGridSpectrumMemory;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitMemory, UnitDisassembler, UnitCommonSpectrum,
  CommonFunctionsLCL, UnitCommon, Grids, Graphics, Forms, Controls, Clipbrd;

type
  TSpectrumMemoryGrid = class(TDrawGrid)
  strict private
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

  protected
    procedure DrawColumnText(aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState); override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState
      ); override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
      override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure JumpTo(Addr: Word);
    procedure DoCopyToClipboard; override;

    property Pc: Word read FPc write SetPc;
    property Sp: Word read FSp write SetSp;
    property FollowPc: Boolean read FFollowPc write SetFollowPc;
    property Disassembler: TDisassembler read FDisassembler write SetDisassembler;
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
var
  C: Integer;
  S: RawByteString;
  TS: TTextStyle;
  Re: TRect;

begin
  inherited DrawCell(aCol, aRow, aRect, aState);

  if aRow < FixedRows then
    Exit;

  if aState * [gdSelected, gdFocused] <> [] then begin
    Re := Rect(aRect.Left + 1, aRect.Top + 1, aRect.Right - 2, aRect.Bottom - 2);
    Canvas.Pen.Color := GridLineColor;
    Canvas.Pen.Width := 2;
    Canvas.Polyline([Re.TopLeft, Point(Re.Right, Re.Top), Re.BottomRight, Point(Re.Left, Re.Bottom), Re.TopLeft]);
  end;

  TS := Canvas.TextStyle;
  S := TextForCell(aCol, aRow);
  if S = '' then
    Exit;

  C := aCol - FixedCols;
  if C < 0 then begin
    TS.Alignment := TAlignment.taRightJustify;
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];

    Canvas.Brush.Color := clYellow;
    TS.Layout := tlCenter;
    TS.Opaque := True;

    aRect.Right := aRect.Right - 6;
  end else begin
    if C <> 4 then
      TS.Alignment := TAlignment.taLeftJustify
    else begin
      TS.Alignment := TAlignment.taRightJustify;
      aRect.Right := aRect.Right - ColWidths[aCol] div 3;
    end;

  end;

  aRect.Left := aRect.Left + 6;
  Canvas.TextRect(ARect, aRect.Left, aRect.Top, S, TS);
end;

procedure TSpectrumMemoryGrid.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
const
  RomColour = TColor($f9f9f9);
  ScreenColour = TColor($cdeac0);
  ScreenAttrColour = TColor($d7ead7);

begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if aCol >= FixedCols then begin
    aRow := aRow - FixedRows;
    if aRow >= 0 then begin
      if aRow < TCommonSpectrum.KB16 then begin
        Canvas.Brush.Color := RomColour;
      end else if aRow < TCommonSpectrum.KB16 + 32 * 192 then begin
        Canvas.Brush.Color := ScreenColour;
      end else if aRow < TCommonSpectrum.KB16 + 32 * (192 + 24) then begin
        Canvas.Brush.Color := ScreenAttrColour;
      end;
    end;
  end;
end;

constructor TSpectrumMemoryGrid.Create(AOwner: TComponent);
var
  C: TGridColumn;
  I: Integer;
begin
  inherited Create(AOwner);

  FocusRectVisible := False;
  Font := TCommonFunctionsLCL.GetMonoFont();

  BorderStyle := bsNone;
  TitleFont.Name := 'default';
  Color := clWhite;
  AlternateColor := Color;
  FFollowPc := True;
  FDisassembler := nil;
  RowCount := FixedRows;
  RowHeights[0] := RowHeights[0] * 2;
  ExtendedSelect := False;

  ColWidths[0] := 46;

  Options := Options
    + [goColSizing, goColMoving]
    - [goEditing, goHorzLine, goRangeSelect, goVertLine, goDrawFocusSelected,
       goFixedVertLine, goFixedHorzLine];

  for I := 0 to 4 do begin
    C := Columns.Add;
    C.ReadOnly := True;
    C.Title.MultiLine := I <> 3;
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
      4:
        begin
          C.Width := 78;
          C.Title.Caption := 'opcode len.' + LineEnding + 'in bytes';
        end;
    end;
  end;
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
begin
  //inherited DoCopyToClipboard;
  if (Row >= FixedRows) and (Col >= FixedCols) then
    Clipboard.AsText := TextForCell(Col, Row);
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
  R := aRow - FixedRows;
  C := aCol - FixedCols;
  if C < 0 then begin
    if R = FPc then begin
      if R <> FSp then
        Result := 'PC'
      else
        Result := 'PC,SP';
    end else if R = FSp then begin
      Result := 'SP';
    end;

  end else begin
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
      4:
        begin
          W := R;
          FDisassembler.Dissasemble(W, C);
          if C > 0 then begin
            Result := C.ToString;
          end;
        end;
    otherwise
    end;
  end;
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
  BeginUpdate;
  try
    if Assigned(FDisassembler) then
      Mem := FDisassembler.Memory
    else
      Mem := nil;

    if Mem <> nil then begin
      MemSize := Mem.RamSizeKB * TCommonSpectrum.KiloByte;
      if MemSize >= TCommonSpectrum.KB48 then
        MemSize := TCommonSpectrum.KB48;
      MemSize := TCommonSpectrum.KB16 + MemSize;
    end else
      MemSize := 0;

    RowCount := FixedRows + MemSize;

  finally
    EndUpdate();
  end;
end;

end.
