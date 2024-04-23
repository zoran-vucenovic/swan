unit UnitFormBrowser;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, ActnList, Menus, Buttons, Types, UnitTapePlayer, CommonFunctionsLCL,
  UnitConfigs, fpjson;

type
  TFormBrowseTape = class(TForm)
    ActionGoToBlock: TAction;
    ActionList1: TActionList;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PopupMenu1: TPopupMenu;
    procedure ActionGoToBlockExecute(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    const
      cSection0 = 'tape_browser';
      cFollow = 'follow_block';
  private
    type
      TTapeGrid = class(TDrawGrid)
      protected
        function MouseButtonAllowed({%H-}Button: TMouseButton): Boolean; override;
      public
        constructor Create(AOwner: TComponent); override;
      end;

      TCellContent = class(TObject)
      public
        Details: String;
        TextHeightDetails: Integer;
      end;

      TCellContents = array of TCellContent;

  private
    FCurrentBlockNumber: Integer;
    FOnGoToBlock: TProcedureOfObject;
    FTapePlayer: TTapePlayer;
    CellContents: TCellContents;
    TextRowHeight: Integer;
    TextSpcHeight: Integer;
    Grid: TTapeGrid;
    FClosing: Boolean;
    FBlockToGoTo: Integer;
    FToolBar: TControl;

    procedure LoadFromConf;
    procedure SaveToConf;
    procedure ClearGrid;
    procedure FillGrid();
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
          aRect: TRect; aState: TGridDrawState);
    procedure GridOnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure AfterShow(Data: PtrInt);
  public
    function GetBlockToGoTo(): Integer;
    procedure SetTapePlayer(const ATapePlayer: TTapePlayer);
    procedure UpdateCurrentBlockNumber(const UncoditionallyPositionGrid: Boolean); inline;
    function AddActionsToolBar(AToolBar: TControl): Boolean;
    property OnGoToBlock: TProcedureOfObject read FOnGoToBlock write FOnGoToBlock;
  end;

implementation

{$R *.lfm}

{ TFormBrowseTape.TTapeGrid }

constructor TFormBrowseTape.TTapeGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FixedCols := 0;
  FixedRows := 0;
  ColCount := 3;

  ScrollBars := ssAutoBoth;
  DefaultDrawing := False;
  ExtendedSelect := False;
  Flat := True;
  TitleStyle := tsNative;
  Options := Options
    + [goColSizing]
    - [goRangeSelect]
    ;
  AllowOutboundEvents := False;
end;

function TFormBrowseTape.TTapeGrid.MouseButtonAllowed(Button: TMouseButton
  ): Boolean;
begin
  Result := TCommonFunctionsLCL.GridMouseButtonAllowed(Self, Button);
end;

{ TFormBrowseTape }
  
procedure TFormBrowseTape.UpdateCurrentBlockNumber(
  const UncoditionallyPositionGrid: Boolean);
begin
  if Assigned(FTapePlayer) and (not FClosing) then begin
    FCurrentBlockNumber := FTapePlayer.GetCurrentBlockNumber;
    if UncoditionallyPositionGrid or CheckBox1.Checked then
      TCommonFunctionsLCL.RowInView(Grid, Grid.FixedRows + FCurrentBlockNumber);
  end else
    FCurrentBlockNumber := -1;
  Grid.Invalidate;
  Grid.Update;
end;

function TFormBrowseTape.AddActionsToolBar(AToolBar: TControl): Boolean;
begin
  if Assigned(FToolBar) then
    FToolBar.Free;
  FToolBar := AToolBar;
  Result := True;

  if Assigned(AToolBar) then begin
    AToolBar.Anchors := [];
    AToolBar.AnchorParallel(akLeft, 2, Panel4);
    AToolBar.AnchorParallel(akTop, 0, Panel4);
    AToolBar.AutoSize := True;
    AToolBar.Parent := Panel4;
  end;
end;

procedure TFormBrowseTape.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  CC: TCellContent;
  TS: TTextStyle;

  procedure WriteLines;
  var
    P, P0, L: Integer;
    YY: Integer;
  begin
    Grid.Canvas.TextStyle := TS;
    L := Length(CC.Details);
    YY := aRect.Top + (aRect.Height - CC.TextHeightDetails) div 2;
    P0 := 1;
    repeat
      P := Pos(#13, CC.Details, P0);
      if P = 0 then begin
        Grid.Canvas.TextRect(aRect, aRect.Left, YY, Copy(CC.Details, P0));
        Break;
      end;

      Grid.Canvas.TextRect(aRect, aRect.Left, YY, Copy(CC.Details, P0, P - P0));
      if P >= L then
        Break;
      P0 := P + 1;
      YY := YY + TextRowHeight;
    until False;
  end;

var
  R: TRect;
  P1, P2, P3: TPoint;
  IsCurrentBlock: Boolean;
begin
  if (aCol >= Grid.FixedCols) and (aRow >= Grid.FixedRows) then begin
    IsCurrentBlock := aRow = Grid.FixedRows + FCurrentBlockNumber;
    if IsCurrentBlock then begin
      if FTapePlayer.IsPlaying then
        Grid.Canvas.Brush.Color := $d9FFd9
      else
        Grid.Canvas.Brush.Color :=  $d5f5FF;
    end else begin
      Grid.Canvas.Brush.Color := Grid.Color;
    end;
    
    Grid.Canvas.FillRect(aRect);

    if aState * [gdSelected, gdFocused] <> [] then begin
      R := Rect(aRect.Left + 1, aRect.Top + 1, aRect.Right - 2, aRect.Bottom - 2);
      Grid.Canvas.Pen.Color := Grid.GridLineColor;
      Grid.Canvas.Pen.Width := 2;
      Grid.Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top), R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);
    end;

    CC := CellContents[(aRow - Grid.FixedRows) * (Grid.ColCount - Grid.FixedCols) + aCol - Grid.FixedCols];
    if CC.Details <> '' then begin
      TS := Grid.Canvas.TextStyle;
      TS.Layout := TTextLayout.tlTop;
      TS.Opaque := False;
      if aCol = Grid.FixedCols then begin
        TS.Alignment := TAlignment.taRightJustify;

        Grid.Canvas.Font.Style := Grid.Font.Style + [TFontStyle.fsBold];
        Grid.Canvas.Font.Color := clBlue;
        WriteLines;
        Grid.Canvas.Font := Grid.Font;

        if IsCurrentBlock then begin
          if FTapePlayer.IsPlaying then begin
            P1 := Point(aRect.Left + 5, (aRect.Bottom + aRect.Top - 1) div 2 - 9);
            P2 := Point(P1.X + 9, P1.Y + 9);
            P3 := Point(P1.X, P1.Y + 18);

            Grid.Canvas.Brush.Color := $39ed39;
            Grid.Canvas.Pen.Color := clGreen;
            Grid.Canvas.Pen.Width := 2;

            Grid.Canvas.Polygon([P1, P2, P3]);
          end else begin
            R.TopLeft := Point(aRect.Left + 5, (aRect.Bottom + aRect.Top - 1) div 2 - 6);
            R.BottomRight := Point(R.Left + 12, R.Top + 12);

            Grid.Canvas.Pen.Color := clSilver;
            Grid.Canvas.Brush.Color := clGray;
            Grid.Canvas.Pen.Width := 2;
            Grid.Canvas.Rectangle(R);
          end;
        end;
      end else
        WriteLines;
    end;

  end;
end;

procedure TFormBrowseTape.GridOnContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPoint;
begin
  Handled := not (TCommonFunctionsLCL.GridMouseToCellRegular(Grid, MousePos, P) and (P.Y >= Grid.FixedRows));
  if not Handled then begin
    P.Y := P.Y - Grid.FixedRows;
    FBlockToGoTo := P.Y;
    ActionGoToBlock.Enabled := P.Y <> FCurrentBlockNumber;
  end;
end;

procedure TFormBrowseTape.AfterShow(Data: PtrInt);
begin
  TCommonFunctionsLCL.AdjustFormPos(Self);
  if Data > 0 then begin
    Panel4.AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end;
end;

function TFormBrowseTape.GetBlockToGoTo: Integer;
begin
  Result := FBlockToGoTo;
end;

procedure TFormBrowseTape.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FClosing := True;
  Application.RemoveAsyncCalls(Self);
  CloseAction := caFree;
end;

procedure TFormBrowseTape.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then begin
    FCurrentBlockNumber := -1;
    UpdateCurrentBlockNumber(True);
  end;
end;

procedure TFormBrowseTape.ActionGoToBlockExecute(Sender: TObject);
begin
  if Assigned(FOnGoToBlock) and (Grid.RowCount > 1) then
    if FBlockToGoTo <> FCurrentBlockNumber then
      FOnGoToBlock();
end;

procedure TFormBrowseTape.FormCreate(Sender: TObject);
var
  F: TFont;
  Sz: TSize;
begin
  FClosing := False;
  OnGoToBlock := nil;
  FBlockToGoTo := -1;
  FToolBar := nil;
  Grid := TTapeGrid.Create(Panel2);
  Grid.AnchorParallel(akTop, 0, Panel2);
  Grid.AnchorParallel(akLeft, 0, Panel2);
  Grid.AnchorParallel(akBottom, 0, Panel2);
  Grid.AnchorParallel(akRight, 0, Panel2);

  Grid.OnContextPopup := @GridOnContextPopup;
  Grid.PopupMenu := PopupMenu1;
  Grid.Parent := Panel2;

  F := TFont.Create;
  try
    F.Assign(Grid.Font);
    F.Style := F.Style + [fsBold];
    TCommonFunctionsLCL.CalculateTextSize(F, 'Fg', Sz);
  finally
    F.Free;
  end;
  TextSpcHeight := Sz.cy;
  TextRowHeight := (TextSpcHeight * 7) div 6 + 1;
  TextSpcHeight := ((TextRowHeight - TextSpcHeight) * 3) div 2;

  SetLength(CellContents, 0);
  SetTapePlayer(nil);
  Grid.OnDrawCell := @GridDrawCell;
  LoadFromConf;

  CommonFunctionsLCL.TCommonFunctionsLCL.GrowFormHeight(Self);
  AfterShow(-1);
end;

procedure TFormBrowseTape.FormDestroy(Sender: TObject);
begin
  FClosing := True;
  SaveToConf;
  Application.RemoveAsyncCalls(Self);
  ClearGrid;
  FToolBar.Free;
end;

procedure TFormBrowseTape.FormShow(Sender: TObject);
begin
  AfterShow(2);
end;

procedure TFormBrowseTape.LoadFromConf;
var
  JObj: TJSONObject;
begin
  JObj := TConfJSON.GetJSONObject(cSection0);

  CheckBox1.Checked := (JObj = nil) or (JObj.Get(cFollow, Integer(0)) <> 0);
end;

procedure TFormBrowseTape.SaveToConf;
var
  JObj: TJSONObject;
  N: Integer;
begin
  if CheckBox1.Checked then
    N := 1
  else
    N := 0;
  JObj := TJSONObject.Create;
  try
    JObj.Add(cFollow, N);
    TConfJSON.RemoveSection(cSection0);
    if TConfJSON.AddToConf(cSection0, JObj) then
      JObj := nil;
  finally
    JObj.Free;
  end;
end;

procedure TFormBrowseTape.ClearGrid;
var
  I: Integer;
begin
  for I := Low(CellContents) to High(CellContents) do begin
    CellContents[I].Free;
    CellContents[I] := nil;
  end;
  SetLength(CellContents, 0);
  Grid.RowCount := Grid.FixedRows;
  FCurrentBlockNumber := -1;
  FBlockToGoTo := -1;
end;

procedure TFormBrowseTape.FillGrid;

  function AdjustText(CC: TCellContent): Integer;
  var
    S1, S2: String;
    P, K, P0: Integer;
    Sz: TSize;
  begin
    Result := 0;
    K := 1;
    S2 := AdjustLineBreaks(CC.Details, TTextLineBreakStyle.tlbsCR);
    CC.Details := '';
    P0 := 1;
    repeat
      P := Pos(#13, S2, P0);

      if P = 0 then
        S1 := Copy(S2, P0)
      else begin
        S1 := Copy(S2, P0, P - P0);
        P0 := P + 1;
      end;
      if S1 <> '' then begin
        S1 := ' ' + S1 + ' ';
        TCommonFunctionsLCL.CalculateTextSize(Grid.Font, S1, Sz);
        if CC.Details <> '' then begin
          S1 := #13 + S1;
          Inc(K);
        end;
        CC.Details := CC.Details + S1;
        if Sz.cx > Result then
          Result := Sz.cx;
      end;
    until P = 0;
    CC.TextHeightDetails := K * TextRowHeight;
  end;

var
  K, N, I, J, TxtW, NN, M: Integer;
  CC: TCellContent;
  BL: TTapeBlock;
  S1: String;

begin
  Grid.BeginUpdate;
  try
    ClearGrid;

    if Assigned(FTapePlayer) then begin
      S1 := ExtractFileName(FTapePlayer.FileName);
      if Trim(S1) = '' then begin
        Label1.Caption := ' ';
        S1 := '';
      end else begin
        Label1.Caption := S1;
        S1 := ' (' + S1 + ')';
      end;
      Label2.Caption := '  ' + FTapePlayer.FileName;
      Panel1.Hint := FTapePlayer.FileName;
      Panel1.ShowHint := True;

      M := Grid.ColCount - Grid.FixedCols;
      Grid.RowCount := Grid.FixedRows + FTapePlayer.GetBlockCount;
      N := FTapePlayer.GetBlockCount * M;

      if N > 0 then begin
        Grid.Col := Grid.FixedCols;
        for J := Grid.FixedCols + 1 to Grid.ColCount - 1 do begin
          Grid.ColWidths[J] := 6;
        end;
        SetLength(CellContents, N);
        NN := 0;

        for I := 0 to FTapePlayer.GetBlockCount - 1 do begin
          BL := FTapePlayer.GetBlock(I);

          for J := 0 to M - 1 do begin
            TxtW := 0;

            CC := TCellContent.Create;
            CellContents[NN] := CC;
            Inc(NN);
            case J of
              0:
                CC.Details := ' ' + IntToStr(I + 1) + '. ';
              1:
                begin
                  CC.Details := BL.GetBlockIdAsString;
                  if CC.Details <> '' then
                    CC.Details := CC.Details + ' — ';

                  CC.Details :=
                    Format(' %s%s (%d) ', [
                      CC.Details,
                      BL.GetBlockDescription,
                      BL.GetBlockLength
                    ]);
                end;
              2:
                BL.Details(CC.Details);
            otherwise
              CC.Details := '';
            end;

            TxtW := AdjustText(CC);

            K := CC.TextHeightDetails + TextSpcHeight;

            if K > Grid.RowHeights[Grid.FixedRows + I] then
              Grid.RowHeights[Grid.FixedRows + I] := K;

            if (J > 0) and (TxtW > 0) then begin
              TxtW := TxtW + 6;
              if TxtW > Grid.ColWidths[Grid.FixedCols + J] then
                Grid.ColWidths[Grid.FixedCols + J] := TxtW;
            end;
          end;
        end;
      end;
    end else begin
      S1 := ' (empty)';
      Label1.Caption := ' ';
      Label2.Caption := ' ';
      Panel1.ShowHint := False;
    end;
    Caption := 'Tape player' + S1;

  finally
    Grid.EndUpdate();
  end;

  UpdateCurrentBlockNumber(True);
end;

procedure TFormBrowseTape.SetTapePlayer(const ATapePlayer: TTapePlayer);
begin
  if Assigned(FTapePlayer) then
    FTapePlayer.OnChangeBlock := nil;

  if Assigned(ATapePlayer) and (ATapePlayer.GetBlockCount = 0) then
    FTapePlayer := nil
  else
    FTapePlayer := ATapePlayer;

  FillGrid;
end;

end.

