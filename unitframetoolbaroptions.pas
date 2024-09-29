unit UnitFrameToolbarOptions;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, CommonFunctionsLCL, UnitSwanTreeView,
  UnitFormForOptionsBasic, UnitColourFunctions, Forms, Controls, ExtCtrls,
  Buttons, ComCtrls, Grids, ActnList, Menus, Dialogs, Graphics, StdCtrls;

type

  TFrameToobarOptions = class(TFrame)
    ActionMoveDown: TAction;
    ActionMoveUp: TAction;
    ActionAdd: TAction;
    ActionRemove: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelAll: TPanel;
    PanelSelected: TPanel;
    Splitter1: TSplitter;
    procedure ActionAddExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);

  strict private
    type
      TGridOptionsList = class(TCustomDrawGrid)
      protected
        function MouseButtonAllowed(Button: TMouseButton): boolean; override;
      public
        NodeArr: array of TSwanTreeNode;
        NodeCount: Integer;

        constructor Create(AOwner: TComponent); override;
        procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
          override;
        procedure DrawFocusRect(aCol, aRow: Integer; ARect: TRect); override;
        procedure DrawCellGrid(aCol, aRow: Integer; aRect: TRect;
          aState: TGridDrawState); override;
        function AddNode(Nd: TSwanTreeNode): Integer;
        function RemoveNode(N: Integer): TSwanTreeNode;
      end;

  strict private
    Grid: TGridOptionsList;
    FTreeViewAll: TSwanTreeView;
    FSelectedItems: TComponentArray;

    procedure ClearGrid;
    procedure ShowToolbarCannotBeEmptyMsg;
    procedure FillGrid(const SelectedItems: TComponentArray);
    procedure GridOnDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
              aState:TGridDrawState);
    procedure FormCloseCallback(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure SetTreeAndSelectedItems(ATree: TSwanTreeView;
              const SelectedItems: TComponentArray);
  public
    constructor Create(TheOwner: TComponent); override;

    class function ShowOnForm(ATree: TSwanTreeView;
      var ASelectedItems: TComponentArray; var AShowToolbar: Boolean): Boolean;
  end;

implementation

{$R *.lfm}

{ TFrameToobarOptions }

procedure TFrameToobarOptions.ActionAddExecute(Sender: TObject);

  procedure SelectNextNode(Nd: TTreeNode);
  var
    Nd0: TTreeNode;
    B: Boolean;
  begin
    Nd0 := Nd;
    B := False;

    repeat
      if not B then
        Nd0 := Nd0.GetNext
      else
        Nd0 := Nd0.GetPrev;

      if Assigned(Nd0) then begin
        if (Nd0.Visible) and Assigned(Nd0.Data) then begin
          Nd0.Selected := True;
          Break;
        end;
      end else begin
        if B then
          Break;
        B := True;
        Nd0 := Nd;
      end;
    until False;
  end;

var
  Nd: TSwanTreeNode;
  Ndd: TTreeNode;
  N: Integer;

begin
  Nd := nil;
  Ndd := FTreeViewAll.Selected;
  if Ndd is TSwanTreeNode then
    Nd := TSwanTreeNode(Ndd);

  if Assigned(Nd) and (Nd.Owner = FTreeViewAll.Items) and Assigned(Nd.Data) then begin
    N := Grid.AddNode(Nd);
    if N >= 0 then begin
      N := N + Grid.FixedRows;
      Nd.Visible := False;
      SelectNextNode(Nd);
      if N < Grid.RowCount then
        Grid.Row := N;

      Grid.Invalidate;
    end;
  end;
end;

procedure TFrameToobarOptions.ActionMoveDownExecute(Sender: TObject);
var
  N: Integer;
  Nd: TSwanTreeNode;
begin
  N := Grid.Row - Grid.FixedRows;
  if (N >= 0) and (N < Grid.NodeCount - 1) then begin
    Nd := Grid.NodeArr[N];
    Grid.NodeArr[N] := Grid.NodeArr[N + 1];
    Grid.NodeArr[N + 1] := Nd;
    Grid.Row := Grid.Row + 1;
    Grid.Invalidate;
  end;
end;

procedure TFrameToobarOptions.ActionMoveUpExecute(Sender: TObject);
var
  N: Integer;
  Nd: TSwanTreeNode;
begin
  N := Grid.Row - Grid.FixedRows;
  if (N > 0) and (N < Grid.NodeCount) then begin
    Nd := Grid.NodeArr[N];
    Grid.NodeArr[N] := Grid.NodeArr[N - 1];
    Grid.NodeArr[N - 1] := Nd;
    Grid.Row := Grid.Row - 1;
    Grid.Invalidate;
  end;
end;

procedure TFrameToobarOptions.ActionRemoveExecute(Sender: TObject);
begin
  Grid.RemoveNode(-1);
end;

procedure TFrameToobarOptions.ClearGrid;
begin
  Grid.RowCount := Grid.FixedRows;
end;

procedure TFrameToobarOptions.ShowToolbarCannotBeEmptyMsg;
const
  Msg: AnsiString =
    'The toolbar cannot be left empty.' + LineEnding
    + 'If you don''t want the toolbar to be shown, just uncheck "Show toolbar".';
begin
  MessageDlg(Msg, TMsgDlgType.mtError, [mbClose], 0);
end;

procedure TFrameToobarOptions.FillGrid(const SelectedItems: TComponentArray);
var
  I, N: Integer;
  Nd: TTreeNode;
begin
  if Assigned(FTreeViewAll) then begin
    Grid.BeginUpdate;
    try
      SetLength(Grid.NodeArr, Length(SelectedItems));
      N := 0;
      for I := Low(SelectedItems) to High(SelectedItems) do begin
        Nd := FTreeViewAll.Items.FindNodeWithData(SelectedItems[I]);

        if Nd is TSwanTreeNode then begin
          Grid.NodeArr[N] := TSwanTreeNode(Nd);
          Nd.Visible := False;
          Inc(N);
        end;
      end;

      SetLength(Grid.NodeArr, N);
      Grid.NodeCount := N;
      Grid.RowCount := Grid.FixedRows + N;
    finally
      Grid.EndUpdate();
    end;
  end;
end;

procedure TFrameToobarOptions.GridOnDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Nd: TSwanTreeNode;
  I: Integer;
  S: AnsiString;
  R: Integer;
  TS: TTextStyle;
  Re: TRect;
  N: Integer;
  Cl: TColor;

begin
  if (aRow >= Grid.FixedRows) then begin
    I := aRow - Grid.FixedRows;
    if I < Grid.NodeCount then begin
      Nd := Grid.NodeArr[I];
      S := Nd.Text;

      if S <> '' then begin
        R := (aRect.Height - FTreeViewAll.Images.Width) div 2;
        TS := Grid.Canvas.TextStyle;

        if gdSelected in aState then begin
          Re := Rect(aRect.Left + 1, aRect.Top + 1, aRect.Right - 2, aRect.Bottom - 2);

          Cl := Grid.SelectedColor;
          Grid.Canvas.Pen.Width := 1;
          N := 0;
          repeat
            Grid.Canvas.Pen.Color := Cl;
            Grid.Canvas.Polyline([
              Re.TopLeft,
              Point(Re.Right, Re.Top),
              Re.BottomRight,
              Point(Re.Left, Re.Bottom),
              Re.TopLeft
            ]);

            if N >= 5 then
              Break;

            Re.Inflate(-1, -1);
            Cl := TColourFunctions.MakeLighterColour(Cl, 2);

            Inc(N);
          until False;

        end;

        TS.Layout := TTextLayout.tlCenter;

        Grid.Canvas.TextRect(aRect, aRect.Left + R + aRect.Height, aRect.Top, S, TS);

        if Nd.ImageIndex >= 0 then begin
          FTreeViewAll.Images.Draw(
            Grid.Canvas, aRect.Left + R, aRect.Top + R, Nd.ImageIndex);
        end;
      end;
    end;
  end;
end;

procedure TFrameToobarOptions.FormCloseCallback(Sender: TObject;
  var CloseAction: TCloseAction);
var
  F: TCustomForm;
  I: Integer;
  J: Integer;
  Obj: TObject;
begin
  CloseAction := TCloseAction.caFree;
  if Sender is TCustomForm then begin
    F := TCustomForm(Sender);
    if F.ModalResult = mrOK then begin
      if Grid.NodeCount > 0 then begin
        SetLength(FSelectedItems, Grid.NodeCount);
        J := 0;
        for I := 0 to Grid.NodeCount - 1 do begin
          Obj := TObject(Grid.NodeArr[I].Data);
          if Obj is TComponent then begin
            FSelectedItems[J] := TComponent(Obj);
            Inc(J);
          end;
        end;
        SetLength(FSelectedItems, J);
      end else begin
        ShowToolbarCannotBeEmptyMsg;
        CloseAction := TCloseAction.caNone;
      end;
    end;
  end;

  if CloseAction = caFree then
    FTreeViewAll.Parent := nil;
end;

procedure TFrameToobarOptions.SetTreeAndSelectedItems(ATree: TSwanTreeView;
  const SelectedItems: TComponentArray);
var
  H: Integer;
  Sz: TSize;
begin
  ATree.Anchors := [];
  ATree.AnchorParallel(akLeft, 0, PanelAll);
  ATree.AnchorParallel(akTop, 0, PanelAll);
  ATree.AnchorParallel(akBottom, 0, PanelAll);
  ATree.AnchorParallel(akRight, 0, PanelAll);

  ATree.Parent := PanelAll;

  FTreeViewAll := ATree;

  H := 0;
  if Assigned(ATree.Images) then begin
    H := ATree.Images.Width;
  end;

  TCommonFunctionsLCL.CalculateTextSize(Grid.Font, 'Fg', Sz);
  if Sz.cy > H then
    H := Sz.cy;

  H := H * 11 div 8 + 4;
  if H > Grid.DefaultRowHeight then
    Grid.DefaultRowHeight := H;

  ATree.MakeAllNodesVisible;
  FillGrid(SelectedItems);
  Grid.OnDrawCell := @GridOnDrawCell;
end;

constructor TFrameToobarOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Self.Caption := 'Toolbar options';

  BitBtn1.Caption := '';
  BitBtn2.Caption := '';
  BitBtn3.Caption := '';
  BitBtn4.Caption := '';

  FTreeViewAll := nil;

  Grid := TGridOptionsList.Create(PanelSelected);

  Panel2.AutoSize := True;
  Panel2.Left := (Self.ClientWidth + Splitter1.Width - Panel2.Width) div 2;

  Grid.Anchors := [];
  Grid.AnchorParallel(akLeft, 0, PanelSelected);
  Grid.AnchorParallel(akTop, 0, PanelSelected);
  Grid.AnchorParallel(akRight, 0, PanelSelected);
  Grid.AnchorParallel(akBottom, 0, PanelSelected);

  Grid.Color := clWhite;
  Grid.Font.Color := clBlack;
  Grid.SelectedColor := $d77800;

  Grid.Parent := PanelSelected;
end;

class function TFrameToobarOptions.ShowOnForm(ATree: TSwanTreeView;
  var ASelectedItems: TComponentArray; var AShowToolbar: Boolean): Boolean;

var
  Fm: TFrameToobarOptions;
  F: TCustomForm;

begin
  Result := False;
  if Assigned(ATree) then begin
    Fm := TFrameToobarOptions.Create(nil);
    try
      F := TFormForOptionsBasic.CreateForControl(nil, Fm, False);
      try
        F.AddHandlerClose(@Fm.FormCloseCallback);
        Fm.SetTreeAndSelectedItems(ATree, ASelectedItems);
        Fm.CheckBox1.Checked := AShowToolbar;

        if F.ShowModal = mrOK then begin
          ASelectedItems := Fm.FSelectedItems;
          AShowToolbar := Fm.CheckBox1.Checked;
          Result := True;
        end;
      finally
        F.Free;
      end;
    finally
      Fm.Free;
    end;
  end;
end;

{ TFrameToobarOptions.TGridOptionsList }

function TFrameToobarOptions.TGridOptionsList.MouseButtonAllowed(
  Button: TMouseButton): boolean;
begin
  Result := TCommonFunctionsLCL.GridMouseButtonAllowed(Self, Button);
end;

constructor TFrameToobarOptions.TGridOptionsList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Flat := True;
  FixedCols := 0;
  FixedRows := 0;

  ColCount := 1;

  Options := Options
    + [goSelectionActive, goDrawFocusSelected]
    - [goEditing, goRangeSelect, goFixedHorzLine, goFixedVertLine, goHorzLine, goVertLine];

  AutoFillColumns := True;
end;

procedure TFrameToobarOptions.TGridOptionsList.PrepareCanvas(aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  inherited PrepareCanvas(aCol, aRow, aState - [gdSelected]);
end;

procedure TFrameToobarOptions.TGridOptionsList.DrawFocusRect(aCol,
  aRow: Integer; ARect: TRect);
begin
  //inherited DrawFocusRect(aCol, aRow, ARect);
end;

procedure TFrameToobarOptions.TGridOptionsList.DrawCellGrid(aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  //inherited DrawCellGrid(aCol, aRow, aRect, aState);
end;

function TFrameToobarOptions.TGridOptionsList.AddNode(Nd: TSwanTreeNode
  ): Integer;
var
  N: Integer;
  Found: Boolean;
  I: Integer;
begin
  Result := -1;
  if Assigned(Nd) then begin
    Found := False;
    N := 0;
    while (not Found) and (N < NodeCount) do begin
      Found := NodeArr[N] = Nd;
      Inc(N);
    end;

    if not Found then begin
      BeginUpdate;
      try
        if Length(NodeArr) <= NodeCount then begin
          SetLength(NodeArr, Length(NodeArr) * 10 div 7 + 2);
        end;

        I := NodeCount;
        Inc(NodeCount);
        RowCount := FixedRows + NodeCount;

        N := Row + 1;
        if N > FixedRows then
          N := N - FixedRows
        else
          N := 0;

        while I > N do begin
          NodeArr[I] := NodeArr[I - 1];
          Dec(I);
        end;
        NodeArr[I] := Nd;
        Result := I;

      finally
        EndUpdate;
      end;
    end;
  end;
end;

function TFrameToobarOptions.TGridOptionsList.RemoveNode(N: Integer
  ): TSwanTreeNode;
var
  I: Integer;
begin
  Result := nil;
  if N < 0 then
    N := Row - FixedRows;

  if (N >= 0) and (N < NodeCount) then begin
    BeginUpdate;
    try
      Result := NodeArr[N];
      if Assigned(Result) then begin
        for I := N to NodeCount - 2 do
          NodeArr[I] := NodeArr[I + 1];

        Dec(NodeCount);
        RowCount := FixedRows + NodeCount;

        Result.Visible := True;
        Result.ExpandParents;
        Result.Selected := True;
      end;

    finally
      EndUpdate;
    end;
  end;
end;

end.

