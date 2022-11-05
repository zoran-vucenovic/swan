unit UnitFormBrowser;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, ActnList, Menus, Buttons, Types, UnitTzxPlayer, CommonFunctionsLCL,
  UnitConfigs, StrUtils, fpjson;

type
  TFormBrowseTape = class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
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
        procedure RowInView(ARow: Integer);
      end;

      TCellContent = class(TObject)
      public
        Details: String;
        TextHeightDetails: Integer;
      end;

      TCellContents = array of TCellContent;

  private
    FCurrentBlockNumber: Integer;
    FTzxPlayer: TTzxPlayer;
    CellContents: TCellContents;
    TextRowHeight: Integer;
    TextSpcHeight: Integer;
    Grid: TTapeGrid;
    FClosing: Boolean;

    procedure LoadFromConf;
    procedure SaveToConf;
    procedure ClearGrid;
    procedure FillGrid();
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer;
          aRect: TRect; aState: TGridDrawState);
    procedure AfterShow(Data: PtrInt);
  public
    procedure SetTzxPlayer(const ATzxPlayer: TTzxPlayer);
    procedure UpdateCurrentBlockNumber; inline;
    procedure AddButtonActions(AImageWidth: Integer;
          AActions: Array of TCustomAction);
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
    //- []
    ;
end;

procedure TFormBrowseTape.TTapeGrid.RowInView(ARow: Integer);
var
  N: Integer;
begin
  BeginUpdate;
  try
    N := ARow - 2;
    while ((N < ARow) and (not MoveExtend(False, Col, N, False))) do begin
      Inc(N);
    end;
    N := ARow + 2;
    while ((N > ARow) and (not MoveExtend(False, Col, N, False))) do begin
      Dec(N);
    end;
    MoveExtend(False, Col, ARow, True);
  finally
    EndUpdate;
  end;
end;

function TFormBrowseTape.TTapeGrid.MouseButtonAllowed(Button: TMouseButton
  ): Boolean;
begin
  Result := Button in [TMouseButton.mbLeft, TMouseButton.mbRight, TMouseButton.mbMiddle];
end;

{ TFormBrowseTape }
  
procedure TFormBrowseTape.UpdateCurrentBlockNumber;
begin
  if Assigned(FTzxPlayer) and (not FClosing) then begin
    FCurrentBlockNumber := FTzxPlayer.GetCurrentBlockNumber;
    if CheckBox1.Checked then begin
      Grid.RowInView(Grid.FixedRows + FCurrentBlockNumber);
    end;
  end else
    FCurrentBlockNumber := -1;
  Grid.Invalidate;
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
      P := PosEx(#13, CC.Details, P0);
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
      if FTzxPlayer.IsPlaying then
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
          if FTzxPlayer.IsPlaying then begin
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

procedure TFormBrowseTape.AfterShow(Data: PtrInt);
begin
  TCommonFunctionsLCL.AdjustFormPos(Self);
  if Data > 0 then begin
    Panel4.AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end;
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
    UpdateCurrentBlockNumber;
  end;
end;

procedure TFormBrowseTape.FormCreate(Sender: TObject);
var
  TempCanvas: TCanvas;
begin
  FClosing := False;
  Grid := TTapeGrid.Create(Panel2);
  Grid.AnchorParallel(akTop, 0, Panel2);
  Grid.AnchorParallel(akLeft, 0, Panel2);
  Grid.AnchorParallel(akBottom, 0, Panel2);
  Grid.AnchorParallel(akRight, 0, Panel2);

  Grid.Parent := Panel2;
  TempCanvas := GetWorkingCanvas(Grid.Canvas);
  try
    TempCanvas.Font := Grid.Font;
    TempCanvas.Font.Style := TempCanvas.Font.Style + [fsBold];
    TextSpcHeight := TempCanvas.TextHeight('Fg');
    TextRowHeight := (TextSpcHeight * 7) div 6 + 1;
    TextSpcHeight := ((TextRowHeight - TextSpcHeight) * 3) div 2;
  finally
    if TempCanvas <> Grid.Canvas then
      FreeWorkingCanvas(TempCanvas);
  end;
  SetLength(CellContents, 0);
  SetTzxPlayer(nil);
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
end;

procedure TFormBrowseTape.FillGrid;
var                              
  TempCanvas: TCanvas;

  function AdjustText(CC: TCellContent): Integer;
  var
    S1, S2: String;
    P, L, K: Integer;
  begin
    Result := 0;
    K := 1;
    S2 := AdjustLineBreaks(CC.Details, TTextLineBreakStyle.tlbsCR);
    CC.Details := '';
    repeat
      P := Pos(#13, S2);

      if P = 0 then
        S1 := S2
      else begin
        S1 := Copy(S2, 1, P - 1);
        Delete(S2, 1, P);
      end;
      if S1 <> '' then begin
        S1 := ' ' + S1 + ' ';
        L := TempCanvas.TextWidth(S1);
        if CC.Details <> '' then begin
          S1 := #13 + S1;
          Inc(K);
        end;
        CC.Details := CC.Details + S1;
        if L > Result then
          Result := L;
      end;
    until P = 0;
    CC.TextHeightDetails := K * TextRowHeight;
  end;

var
  K, N, I, J, TxtW, NN, M: Integer;
  CC: TCellContent;
  BL: TTzxBlock;
  S1: String;
begin
  Grid.BeginUpdate;
  try
    ClearGrid;

    if Assigned(FTzxPlayer) then begin
      S1 := ExtractFileName(FTzxPlayer.FileName);
      Label1.Caption := S1;
      Label2.Caption := '  ' + FTzxPlayer.FileName;
      Panel1.Hint := FTzxPlayer.FileName;
      Panel1.ShowHint := True;

      M := Grid.ColCount - Grid.FixedCols;
      Grid.RowCount := Grid.FixedRows + FTzxPlayer.GetBlockCount;
      N := FTzxPlayer.GetBlockCount * M;

      if N > 0 then begin
        for J := Grid.FixedCols + 1 to Grid.ColCount - 1 do begin
          Grid.ColWidths[J] := 6;
        end;
        SetLength(CellContents, N);
        NN := 0;
        TempCanvas := GetWorkingCanvas(Grid.Canvas);
        try
          TempCanvas.Font := Grid.Font;
          for I := 0 to FTzxPlayer.GetBlockCount - 1 do begin
            BL := FTzxPlayer.GetBlock(I);

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
                    CC.Details := Format(' 0x%s — %s (%d) ', [
                      IntToHex(BL.GetBlockId, 2),
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

        finally
          if TempCanvas <> Grid.Canvas then
            FreeWorkingCanvas(TempCanvas);
        end;
      end;
    end else begin
      S1 := 'empty';
      Label1.Caption := ' ';
      Label2.Caption := ' ';
      Panel1.ShowHint := False;
    end;
    Caption := Format('Tape player (%s)', [S1]);

    UpdateCurrentBlockNumber;
  finally
    Grid.EndUpdate();
  end;
end;

procedure TFormBrowseTape.SetTzxPlayer(const ATzxPlayer: TTzxPlayer);
begin
  if Assigned(FTzxPlayer) then
    FTzxPlayer.OnChangeBlock := nil;

  if Assigned(ATzxPlayer) and (ATzxPlayer.GetBlockCount = 0) then
    FTzxPlayer := nil
  else
    FTzxPlayer := ATzxPlayer;
  FillGrid;
end;

procedure TFormBrowseTape.AddButtonActions(AImageWidth: Integer;
  AActions: array of TCustomAction);
var
  I: Integer;
  SB, PrevSB: TSpeedButton;
  W, L: Integer;
  A: TCustomAction;
begin
  PrevSB := nil;
  L := 0;
  for I := Low(AActions) to High(AActions) do begin
    A := AActions[I];

    if Assigned(A) then begin
      SB := TSpeedButton.Create(Panel4);
      SB.ShowCaption := False;
      SB.ShowHint := True;
      SB.Flat := True;

      SB.Anchors := [];
      SB.AnchorParallel(akTop, 0, Panel4);

      if PrevSB = nil then begin
        AImageWidth := Scale96ToFont(AImageWidth);
        W := (AImageWidth * 32 + 12) div 24;
      end;

      if L > 0 then
        L := L * ((W + 3) div 4);
      if PrevSB = nil then
        SB.AnchorParallel(akLeft, L, Panel4)
      else
        SB.AnchorToNeighbour(akLeft, L, PrevSB);

      SB.ClientWidth := W + 1;
      SB.ClientHeight := W;
      SB.ImageWidth := AImageWidth;

      SB.Action := A;

      SB.Parent := Panel4;
      PrevSB := SB;
      L := 0;
    end else begin
      // add divider
      Inc(L);
    end;
  end;
end;

end.

