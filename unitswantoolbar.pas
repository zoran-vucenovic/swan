unit UnitSwanToolbar;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitCommon, UnitCommonSpectrum, Controls, ActnList,
  Buttons, ComCtrls, Graphics, Menus;

type

  TSwanToolButton = class(TToolButton)
  end;

  TSwanToolBar = class(TToolBar)
  private
    FRecentFilesMenuItem: TMenuItem;
    FRecentFilesToolButton: TSwanToolButton;
    FSpacers: array of TControl;

    procedure RemoveAllButtons;
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddButtons(AItems: array of TObject);

    procedure UpdateRecentFiles();
    property RecentFilesMenuItem: TMenuItem write FRecentFilesMenuItem;
  end;

  TSwanSpeedButtonsBar = class(TCustomControl)
  strict private
    type
      TSwanSpeedButton = class(TSpeedButton)
      private
      public
        constructor Create(AOwner: TComponent); override;
      end;

  strict private
    FImages: TImageList;
    LastSB: TSwanSpeedButton;
    FImageWidth: Integer;

    procedure SetImageWidth(AValue: Integer);

  public
    constructor Create(AOwner: TComponent); override;

    procedure AddButtonActions(AActionsLeft: Array of TCustomAction; AActionsRight: Array of TCustomAction);
    procedure RemoveAllButtons;

    property ImageWidth: Integer read FImageWidth write SetImageWidth;
    property Images: TImageList read FImages write FImages;
  end;

implementation

{ TSwanToolBar }

constructor TSwanToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SetLength(FSpacers, 0);
  FRecentFilesMenuItem := nil;
  FRecentFilesToolButton := nil;
  Align := alNone;
  Flat := True;
  ShowHint := True;
  EdgeBorders := [];
  ShowCaptions := False;
end;

procedure TSwanToolBar.AddButtons(AItems: array of TObject);
var
  I: Integer;
  TB: TSwanToolButton;
  C: TControl;
  Obj: TObject;
  Ac: TCustomAction;
  Mi: TMenuItem;
  J: Integer;
  M: TMenuItem;
  II: Integer;

begin
  DisableAlign;
  try
    RemoveAllButtons;
    J := 0;
    for I := Low(AItems) to High(AItems) do begin
      Obj := AItems[I];
      if Obj = nil then begin
        C := TGraphicControl.Create(Self);
        C.Name := TCommonFunctions.GlobalObjectNameGenerator(C);
        C.AutoSize := False;
        C.Width := Self.ButtonWidth div 4;
        C.Height := 1;
        C.Constraints.MinWidth := C.Width;
        C.Parent := Self;

        if Length(FSpacers) <= J then
          SetLength(FSpacers, J * 5 div 3 + 2);
        FSpacers[J] := C;
        Inc(J);

      end else begin
        Ac := nil;
        Mi := nil;
        if Obj = TCommonSpectrum.DummyObj then begin
          TB := TSwanToolButton.Create(Self);
          TB.Name := TCommonFunctions.GlobalObjectNameGenerator(TB);
          TB.Style := tbsDivider;
          TB.Caption := '';
          TB.ShowHint := False;
        end else if Obj is TCustomAction then begin
          TB := TSwanToolButton.Create(Self);
          TB.Name := TCommonFunctions.GlobalObjectNameGenerator(TB);
          Ac := TCustomAction(Obj);
        end else if Obj is TMenuItem then begin
          TB := TSwanToolButton.Create(Self);
          TB.Name := TCommonFunctions.GlobalObjectNameGenerator(TB);
          Mi := TMenuItem(Obj);
          if Mi.Action is TCustomAction then begin
            Ac := TCustomAction(Mi.Action);

          end else begin
            TB.Style := tbsButtonDrop;
            TB.ImageIndex := Mi.ImageIndex;
            TB.Caption := Mi.Caption;
            TB.Hint := Mi.Hint;

            if Mi = FRecentFilesMenuItem then begin
              FRecentFilesToolButton := TB;
            end else begin

              if Mi.Count > 0 then begin
                TB.DropdownMenu := TPopupMenu.Create(TB);
                TB.DropdownMenu.Name := TCommonFunctions.GlobalObjectNameGenerator(TB.DropdownMenu);
                TB.DropdownMenu.Images := Mi.GetImageList;
                for II := 0 to Mi.Count - 1 do begin
                  M := TMenuItem.Create(TB.DropdownMenu);
                  M.Name := TCommonFunctions.GlobalObjectNameGenerator(M);
                  M.Action := Mi.Items[II].Action;
                  if M.Action = nil then begin
                    M.Caption := Mi.Items[II].Caption;
                    M.ImageIndex := Mi.Items[II].ImageIndex;
                    M.Hint := Mi.Items[II].Hint;
                  end;
                  TB.DropdownMenu.Items.Add(M);
                end;
              end;

              TB.Enabled := Mi.Enabled;
            end;

          end;
        end;
        if Assigned(TB) then begin

          if Assigned(Ac) then begin
            TB.Style := tbsButton;
            TB.Action := Ac;
          end;

          if TB.Hint = '' then
            TB.Hint := TB.Caption;

          TB.AutoSize := True;
          TB.Parent := Self;
        end;
      end;
    end;
    SetLength(FSpacers, J);
    UpdateRecentFiles;

  finally
    EnableAlign;
  end;
end;

procedure TSwanToolBar.RemoveAllButtons;
var
  I: Integer;
begin
  DisableAlign;
  try
    FRecentFilesToolButton := nil;

    for I := Low(FSpacers) to High(FSpacers) do
      FSpacers[I].Free;
    SetLength(FSpacers, 0);
    for I := Self.ButtonCount - 1 downto 0 do begin
      Self.Buttons[I].Free;
    end;

  finally
    EnableAlign;
  end;
end;

procedure TSwanToolBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  WrapButtons(Width, PreferredWidth, PreferredHeight, True);
end;

procedure TSwanToolBar.UpdateRecentFiles;
var
  I: Integer;
  M: TMenuItem;
begin
  if Assigned(FRecentFilesToolButton) and Assigned(FRecentFilesMenuItem) then begin
    if FRecentFilesMenuItem.Count > 0 then begin
      if FRecentFilesToolButton.DropdownMenu = nil then begin
        FRecentFilesToolButton.DropdownMenu := TPopupMenu.Create(FRecentFilesToolButton);
        FRecentFilesToolButton.DropdownMenu.Name := TCommonFunctions.GlobalObjectNameGenerator(FRecentFilesToolButton.DropdownMenu);
        FRecentFilesToolButton.DropdownMenu.Images := FRecentFilesMenuItem.GetImageList;
      end;

      FRecentFilesToolButton.DropdownMenu.Items.Clear;
      for I := 0 to FRecentFilesMenuItem.Count - 1 do begin
        M := TMenuItem.Create(FRecentFilesToolButton.DropdownMenu);
        M.Name := TCommonFunctions.GlobalObjectNameGenerator(M);
        M.Caption := FRecentFilesMenuItem.Items[I].Caption;
        M.ImageIndex := FRecentFilesMenuItem[I].ImageIndex;
        M.OnClick := FRecentFilesMenuItem.Items[I].OnClick;
        FRecentFilesToolButton.DropdownMenu.Items.Add(M);
      end;
    end else begin
      FRecentFilesToolButton.DropdownMenu.Free;
      FRecentFilesToolButton.DropdownMenu := nil;
    end;

    FRecentFilesToolButton.Enabled := FRecentFilesMenuItem.Enabled;
  end;
end;

{ TSwanSpeedButtonsBar }

procedure TSwanSpeedButtonsBar.SetImageWidth(AValue: Integer);
begin
  if AValue > 0 then
    FImageWidth := AValue;
end;

constructor TSwanSpeedButtonsBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FImages := nil;
  FImageWidth := 0;
  LastSB := nil;
  AutoSize := True;
end;

procedure TSwanSpeedButtonsBar.AddButtonActions(
  AActionsLeft: array of TCustomAction; AActionsRight: array of TCustomAction);

var
  I, J: Integer;
  SB, PrevSB: TSwanSpeedButton;
  W, L: Integer;
  A: TCustomAction;
  ImgW: Integer;
  LeArrLeft: Integer;
  AnchKind: TAnchorKind;

begin
  ImgW := FImageWidth;
  if ImgW = 0 then
    ImgW := 16;

  PrevSB := nil;

  LeArrLeft := Length(AActionsLeft);
  DisableAutoSizing;
  try
    L := 0;
    AnchKind := TAnchorKind.akLeft;
    for I := 0 to LeArrLeft + Length(AActionsRight) - 1 do begin
      if I = LeArrLeft then begin
        L := 0;
        PrevSB := nil;
        AnchKind := akRight;
      end;
      if I < LeArrLeft then begin
        A := AActionsLeft[I];
      end else begin
        A := AActionsRight[I - LeArrLeft];
      end;

      if Assigned(A) then begin

        SB := TSwanSpeedButton.Create(Self);

        SB.Images := FImages;
        SB.Anchors := [];
        SB.AnchorParallel(akTop, 0, Self);

        if PrevSB = nil then begin
          if Assigned(FImages) then begin
            J := 0;
            while J < FImages.ResolutionCount do begin
              if FImages.ResolutionByIndex[J].Width + 3 >= ImgW then begin
                ImgW := FImages.ResolutionByIndex[J].Width;
                Break;
              end;

              Inc(J);
            end;
          end;

          ImgW := Scale96ToFont(ImgW);

          W := (ImgW * 32 + 12) div 24;

          PrevSb := LastSB;
        end;

        if L > 0 then
          L := L * ((W + 3) div 4);

        if PrevSB = nil then
          SB.AnchorParallel(AnchKind, L, Self)
        else
          SB.AnchorToNeighbour(AnchKind, L, PrevSB);

        SB.ClientWidth := W + 1;
        SB.ClientHeight := W;
        SB.ImageWidth := ImgW;

        SB.Action := A;

        SB.Parent := Self;
        SB.SendToBack;

        PrevSB := SB;
        L := 0;
      end else begin
        // add divider
        Inc(L);
      end;
    end;


  finally
    EnableAutoSizing;
  end;

  if Assigned(PrevSB) then
    LastSB := PrevSB;

end;

procedure TSwanSpeedButtonsBar.RemoveAllButtons;
var
  SB: TSwanSpeedButton;
  ASide: TAnchorSide;
begin
  while Assigned(LastSB) do begin
    SB := LastSB;

    ASide := LastSB.AnchorSide[akLeft];
    if Assigned(ASide) and (ASide.Control is TSwanSpeedButton) then
      LastSB := TSwanSpeedButton(ASide.Control)
    else
      LastSB := nil;

    SB.Free;
  end;
end;

{ TSwanSpeedButtonsBar.TSwanSpeedButton }

constructor TSwanSpeedButtonsBar.TSwanSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.Flat := True;
  Self.ShowCaption := False;
  Self.ShowHint := True;
end;

end.

