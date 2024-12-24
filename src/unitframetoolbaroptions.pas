unit UnitFrameToolbarOptions;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, CommonFunctionsLCL, UnitSwanTreeView,
  UnitFormForOptionsBasic, UnitColourFunctions, UnitOptions, UnitCommon,
  UnitCommonSpectrum, Forms, Controls, ExtCtrls, Buttons, ComCtrls, Grids,
  ActnList, Menus, Dialogs, Graphics, StdCtrls, ImgList;

type

  TFrameToobarOptions = class(TFrame, ICheckStateValid)
    ActionAddDivider: TAction;
    ActionAddSpacer: TAction;
    ActionReset: TAction;
    ActionMoveDown: TAction;
    ActionMoveUp: TAction;
    ActionAdd: TAction;
    ActionRemove: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    PanelAll: TPanel;
    PanelSelected: TPanel;
    Splitter1: TSplitter;
    procedure ActionAddDividerExecute(Sender: TObject);
    procedure ActionAddExecute(Sender: TObject);
    procedure ActionAddSpacerExecute(Sender: TObject);
    procedure ActionMoveDownExecute(Sender: TObject);
    procedure ActionMoveUpExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionResetExecute(Sender: TObject);

  public
    type
      TProcGetDefToolbarActions = procedure (out AToolbarActions: TObjectArray) of object;

  strict private
    type
      TGridOptionsList = class(TCustomDrawGrid)
      protected
        function MouseButtonAllowed(Button: TMouseButton): boolean; override;
      public
        ItemsArr: array of TObject;
        ItemsCount: Integer;

        constructor Create(AOwner: TComponent); override;
        procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
          override;
        procedure DrawFocusRect({%H-}aCol, {%H-}aRow: Integer; {%H-}ARect: TRect); override;
        procedure DrawCellGrid({%H-}aCol, {%H-}aRow: Integer; {%H-}aRect: TRect;
          {%H-}aState: TGridDrawState); override;
        function AddItem(AObj: TObject): Integer;
        procedure RemoveItem(N: Integer);
      end;

  strict private
    class var
      FOnGetDefToolbarActions: TProcGetDefToolbarActions;
      FDefToolbarActions: TObjectArray;

  private
    class procedure Init;

  strict private
    Grid: TGridOptionsList;
    FTreeViewAll: TSwanTreeView;
    FSelectedItems: TObjectArray;

    procedure ShowToolbarCannotBeEmptyMsg;
    procedure FillGrid(const ASelectedItems: TObjectArray);
    procedure GridOnDrawCell(Sender: TObject; {%H-}aCol, aRow: Integer; aRect: TRect;
              aState:TGridDrawState);
    procedure FormCloseQuery(Sender : TObject; var CanClose: Boolean);
    procedure SetTreeAndSelectedItems(ATree: TSwanTreeView;
              const ASelectedItems: TObjectArray);
    procedure FormOnFirstShow(Sender: TObject);
    procedure AfterShowForm(Data: PtrInt);

  public
    constructor Create(TheOwner: TComponent); override;

    class function ShowOnForm(ATree: TSwanTreeView;
      var ASelectedItems: TObjectArray; var AShowToolbar: Boolean): Boolean;
    class function CreateForAllOptions(AOptionsDialog: TFormOptions;
      ATree: TSwanTreeView; const ASelectedItems: TObjectArray;
      const AShowToolbar: Boolean): TFrameToobarOptions;

    function IsStateValid: Boolean;
    function GetSelectedItems(): TObjectArray;

    class property OnGetDefToolbarActions: TProcGetDefToolbarActions write FOnGetDefToolbarActions;
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
          Nd0.ExpandParents;
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

begin
  Nd := nil;
  Ndd := FTreeViewAll.Selected;
  if Ndd is TSwanTreeNode then
    Nd := TSwanTreeNode(Ndd);

  if Assigned(Nd) and (Nd.Owner = FTreeViewAll.Items) and Assigned(Nd.Data) then begin
    if Grid.AddItem(Nd) >= 0 then begin
      Nd.Visible := False;
      SelectNextNode(Nd);
    end;
  end;
end;

procedure TFrameToobarOptions.ActionAddSpacerExecute(Sender: TObject);
begin
  Grid.AddItem(nil);
end;

procedure TFrameToobarOptions.ActionAddDividerExecute(Sender: TObject);
begin
  Grid.AddItem(TCommonSpectrum.DummyObj);
end;

procedure TFrameToobarOptions.ActionMoveDownExecute(Sender: TObject);
var
  N: Integer;
  Obj: TObject;
begin
  N := Grid.Row - Grid.FixedRows;
  if (N >= 0) and (N < Grid.ItemsCount - 1) then begin
    Obj := Grid.ItemsArr[N];
    Grid.ItemsArr[N] := Grid.ItemsArr[N + 1];
    Grid.ItemsArr[N + 1] := Obj;
    Grid.Row := Grid.Row + 1;
    Grid.Invalidate;
  end;
end;

procedure TFrameToobarOptions.ActionMoveUpExecute(Sender: TObject);
var
  N: Integer;
  Obj: TObject;
begin
  N := Grid.Row - Grid.FixedRows;
  if (N > 0) and (N < Grid.ItemsCount) then begin
    Obj := Grid.ItemsArr[N];
    Grid.ItemsArr[N] := Grid.ItemsArr[N - 1];
    Grid.ItemsArr[N - 1] := Obj;
    Grid.Row := Grid.Row - 1;
    Grid.Invalidate;
  end;
end;

procedure TFrameToobarOptions.ActionRemoveExecute(Sender: TObject);
begin
  Grid.RemoveItem(-1);
end;

procedure TFrameToobarOptions.ActionResetExecute(Sender: TObject);
begin
  if Length(FDefToolbarActions) = 0 then begin
    if Assigned(FOnGetDefToolbarActions) then
      FOnGetDefToolbarActions(FDefToolbarActions);
  end;

  if Length(FDefToolbarActions) > 0 then
    FillGrid(FDefToolbarActions);
end;

class procedure TFrameToobarOptions.Init;
begin
  SetLength(FDefToolbarActions, 0);
  FOnGetDefToolbarActions := nil;
end;

procedure TFrameToobarOptions.ShowToolbarCannotBeEmptyMsg;
const
  Msg: AnsiString =
    'The toolbar cannot be left empty.' + LineEnding
    + 'If you don''t want the toolbar to be shown, just uncheck "Show toolbar".';
begin
  MessageDlg(Msg, TMsgDlgType.mtError, [mbClose], 0);
end;

procedure TFrameToobarOptions.FillGrid(const ASelectedItems: TObjectArray);
var
  I, N, L: Integer;
  Nd: TTreeNode;
  Obj: TObject;
begin
  if Assigned(FTreeViewAll) then begin
    FTreeViewAll.BeginUpdate;
    try
      Grid.BeginUpdate;
      try
        FTreeViewAll.MakeAllNodesVisible;
        SetLength(Grid.ItemsArr, Length(ASelectedItems));
        L := 0;
        N := 0;
        for I := Low(ASelectedItems) to High(ASelectedItems) do begin
          Obj := ASelectedItems[I];
          if (Obj = nil) or (Obj = TCommonSpectrum.DummyObj) then begin
            if L > 0 then begin
              Grid.ItemsArr[N] := Obj;
              Inc(N);
            end;
          end else if Obj is TComponent then begin
            Nd := FTreeViewAll.Items.FindNodeWithData(ASelectedItems[I]);

            if Nd is TSwanTreeNode then begin
              Grid.ItemsArr[N] := TSwanTreeNode(Nd);
              Nd.Visible := False;
              Inc(N);
              L := N;
            end;
          end;
        end;

        SetLength(Grid.ItemsArr, L);
        Grid.ItemsCount := L;
        Grid.RowCount := Grid.FixedRows + L;
      finally
        Grid.EndUpdate();
      end;

    finally
      FTreeViewAll.EndUpdate;
    end;
  end;
end;

procedure TFrameToobarOptions.GridOnDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Obj: TObject;
  Nd: TSwanTreeNode;
  I: Integer;
  S: AnsiString;
  R: Integer;
  TS: TTextStyle;
  Re: TRect;
  N: Integer;
  Cl: TColor;
  ImgList: TCustomImageList;
  ImgIndex: Integer;
  Spec: Boolean;

begin
  if (aRow >= Grid.FixedRows) then begin
    I := aRow - Grid.FixedRows;
    if I < Grid.ItemsCount then begin
      Obj := Grid.ItemsArr[I];
      S := '';
      ImgIndex := -1;
      ImgList := nil;
      Nd := nil;

      Spec := True;
      if Obj = nil then begin
        ImgIndex := ActionAddSpacer.ImageIndex;
        S := '<space>';
      end else if Obj = UnitCommonSpectrum.TCommonSpectrum.DummyObj then begin
        ImgIndex := ActionAddDivider.ImageIndex;
        S := '<divider>';
      end else if Obj is TSwanTreeNode then begin
        Nd := TSwanTreeNode(Obj);
        if Assigned(Nd.TreeView) then begin
          Spec := False;
          S := Nd.Text;
          ImgList := Nd.TreeView.Images;
          ImgIndex := Nd.ImageIndex;
        end;
      end;

      if S <> '' then begin
        if Spec then begin
          ImgList := ActionList1.Images;
          Grid.Canvas.Font.Color := clMaroon;
          Grid.Canvas.Font.Style := Grid.Canvas.Font.Style + [fsItalic];
        end;

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
        TS.Alignment := TAlignment.taLeftJustify;
        TS.Wordbreak := False;
        TS.SingleLine := True;

        Grid.Canvas.TextRect(aRect, aRect.Left + R + aRect.Height, aRect.Top, S, TS);

        if Assigned(ImgList) and (ImgIndex >= 0) then begin
          ImgList.Draw(
            Grid.Canvas, aRect.Left + R, aRect.Top + R, ImgIndex);
        end;
      end;
    end;
  end;
end;

procedure TFrameToobarOptions.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Sender is TCustomForm then begin
    if TCustomForm(Sender).ModalResult = mrOK then begin
      CanClose := IsStateValid;
    end;
  end;
end;

procedure TFrameToobarOptions.SetTreeAndSelectedItems(ATree: TSwanTreeView;
  const ASelectedItems: TObjectArray);
var
  H: Integer;
  Sz: TSize;
  Nd: TTreeNode;
  Ndd: TTreeNode;

begin
  ATree.FullCollapse;

  ATree.Anchors := [];
  ATree.AnchorParallel(akLeft, 0, PanelAll);
  ATree.AnchorParallel(akBottom, 0, PanelAll);
  ATree.AnchorParallel(akRight, 0, PanelAll);
  ATree.AnchorToNeighbour(akTop, 0, Panel5);

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

  FillGrid(ASelectedItems);

  Ndd := FTreeViewAll.Items.GetFirstVisibleNode;
  while Assigned(Ndd) do begin
    if Ndd.Visible then begin
      Nd := Ndd.GetFirstVisibleChild();
      if Assigned(Nd) then begin
        Nd.ExpandParents;
        FTreeViewAll.Selected := Nd;
        Break;
      end;
    end;

    Ndd := Ndd.GetNextSibling;
  end;

  Grid.OnDrawCell := @GridOnDrawCell;
end;

procedure TFrameToobarOptions.FormOnFirstShow(Sender: TObject);
begin
  AfterShowForm(1);
end;

procedure TFrameToobarOptions.AfterShowForm(Data: PtrInt);
begin
  Splitter1.Left := (Self.ClientWidth - Splitter1.Width - Panel2.Width) div 2;
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShowForm, Data - 1);
end;

constructor TFrameToobarOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);
  Self.Caption := 'Toolbar';

  BitBtn1.Caption := '';
  BitBtn2.Caption := '';
  BitBtn3.Caption := '';
  BitBtn4.Caption := '';
  BitBtn5.Caption := '';

  FTreeViewAll := nil;

  Grid := TGridOptionsList.Create(PanelSelected);

  Panel2.AutoSize := True;

  Grid.Anchors := [];
  Grid.AnchorParallel(akLeft, 0, PanelSelected);
  Grid.AnchorParallel(akRight, 0, PanelSelected);
  Grid.AnchorParallel(akBottom, 0, PanelSelected);
  Grid.AnchorToNeighbour(akTop, 0, Panel6);

  Grid.Color := clWhite;
  Grid.Font.Color := clBlack;
  Grid.SelectedColor := $d77800;

  Grid.Parent := PanelSelected;

  AfterShowForm(-1);
end;

class function TFrameToobarOptions.ShowOnForm(ATree: TSwanTreeView;
  var ASelectedItems: TObjectArray; var AShowToolbar: Boolean): Boolean;

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
        (F as IFormAddCloseQuery).AddCloseQuery(@Fm.FormCloseQuery);
        Fm.SetTreeAndSelectedItems(ATree, ASelectedItems);
        Fm.CheckBox1.Checked := AShowToolbar;
        F.AddHandlerFirstShow(@Fm.FormOnFirstShow);

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

class function TFrameToobarOptions.CreateForAllOptions(
  AOptionsDialog: TFormOptions; ATree: TSwanTreeView;
  const ASelectedItems: TObjectArray; const AShowToolbar: Boolean
  ): TFrameToobarOptions;

var
  Fm: TFrameToobarOptions;

begin
  Result := nil;
  if Assigned(AOptionsDialog) and Assigned(ATree) then begin
    Fm := TFrameToobarOptions.Create(AOptionsDialog);
    try
      AOptionsDialog.AddAnOptionControl(Fm);

      Fm.SetTreeAndSelectedItems(ATree, ASelectedItems);
      Fm.CheckBox1.Checked := AShowToolbar;
      AOptionsDialog.AddCloseQuery(@Fm.FormCloseQuery);
      AOptionsDialog.AddHandlerFirstShow(@Fm.FormOnFirstShow);

      Fm.AnchorParallel(akBottom, 0, Fm.Parent);

      Result := Fm;
    except
      FreeAndNil(Fm);
    end;
  end;
end;

function TFrameToobarOptions.IsStateValid: Boolean;
var
  I: Integer;
  J: Integer;
  Obj: TObject;
  L: Integer;

begin
  Result := True;
  L := 0;

  if Grid.ItemsCount > 0 then begin
    SetLength(FSelectedItems, Grid.ItemsCount);
    J := 0;
    for I := 0 to Grid.ItemsCount - 1 do begin
      Obj := Grid.ItemsArr[I];
      if Obj is TSwanTreeNode then begin
        Obj := TObject(TSwanTreeNode(Obj).Data);
        if Obj is TComponent then begin
          FSelectedItems[J] := Obj;
          Inc(J);
          L := J;
        end;
      end else if L > 0 then begin
        if (Obj = nil) or (Obj = TCommonSpectrum.DummyObj) then begin
          FSelectedItems[J] := Obj;
          Inc(J);
        end;
      end;
    end;
    SetLength(FSelectedItems, L);
  end;

  if L <= 0 then begin
    ShowToolbarCannotBeEmptyMsg;
    Result := False;
  end;
end;

function TFrameToobarOptions.GetSelectedItems: TObjectArray;
begin
  Result := FSelectedItems;
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

  BorderStyle := bsNone;
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
  inherited PrepareCanvas(aCol, aRow, aState - [gdSelected, gdFocused]);
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

function TFrameToobarOptions.TGridOptionsList.AddItem(AObj: TObject): Integer;
var
  N: Integer;
  Found: Boolean;
  I: Integer;
  Nd: TSwanTreeNode;

begin
  Result := -1;

  Found := False;
  if AObj is TSwanTreeNode then begin
    Nd := TSwanTreeNode(AObj);
    N := 0;
    while (not Found) and (N < ItemsCount) do begin
      Found := ItemsArr[N] = Nd;
      Inc(N);
    end;
  end;

  if not Found then begin
    BeginUpdate;
    try
      if Length(ItemsArr) <= ItemsCount then begin
        SetLength(ItemsArr, Length(ItemsArr) * 10 div 7 + 2);
      end;

      I := ItemsCount;
      Inc(ItemsCount);
      RowCount := FixedRows + ItemsCount;

      N := Row + 1;
      if N > FixedRows then
        N := N - FixedRows
      else
        N := 0;

      while I > N do begin
        ItemsArr[I] := ItemsArr[I - 1];
        Dec(I);
      end;
      ItemsArr[I] := AObj;

      Row := FixedRows + I;

      Result := I;

    finally
      EndUpdate;
    end;
  end;

end;

procedure TFrameToobarOptions.TGridOptionsList.RemoveItem(N: Integer);
var
  I: Integer;
  Nd: TSwanTreeNode;
  Obj: TObject;
begin
  if N < 0 then
    N := Row - FixedRows;

  if (N >= 0) and (N < ItemsCount) then begin
    BeginUpdate;
    try
      Obj := ItemsArr[N];

      for I := N to ItemsCount - 2 do
        ItemsArr[I] := ItemsArr[I + 1];

      Dec(ItemsCount);
      RowCount := FixedRows + ItemsCount;

      if Obj is TSwanTreeNode then begin
        Nd := TSwanTreeNode(Obj);

        Nd.Visible := True;
        Nd.ExpandParents;
        Nd.Selected := True;
      end;

    finally
      EndUpdate;
    end;
  end;
end;

initialization
  TFrameToobarOptions.Init;

end.

