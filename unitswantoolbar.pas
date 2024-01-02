unit UnitSwanToolbar;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Controls, ActnList, Buttons, ComCtrls, Graphics;

type

  TSwanToolButton = class(TToolButton)
  end;

  TSwanToolBar = class(TToolBar)
  private
  protected
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddButtonActions(AActions: Array of TCustomAction);
    procedure Paint; override;
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

    procedure AddButtonActions(AActions: Array of TCustomAction);
    procedure RemoveAllButtons;

    property ImageWidth: Integer read FImageWidth write SetImageWidth;
    property Images: TImageList read FImages write FImages;
  end;

implementation

{ TSwanToolBar }

constructor TSwanToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Align := alNone;
  Flat := True;
  ShowHint := True;
  EdgeBorders := [];
  ShowCaptions := False;
end;

procedure TSwanToolBar.AddButtonActions(AActions: array of TCustomAction);
var
  C: TControl;
  TB: TSwanToolButton;
  I: Integer;

begin
  DisableAlign;
  try
    for I := Low(AActions) to High(AActions) do begin
      if AActions[I] = nil then begin
        C := TCustomControl.Create(Self);
        C.Width := Self.ButtonWidth div 4;
        C.Constraints.MinWidth := C.Width;
      end else begin

        TB := TSwanToolButton.Create(Self);
        TB.Style := tbsButton;
        TB.Action := AActions[I];    
        if AActions[I].Hint = '' then
          TB.Hint := AActions[I].Caption;

        C := TB;
      end;
      C.Parent := Self;

      C.AutoSize := True;
    end;

  finally
    EnableAlign;
  end;

end;

procedure TSwanToolBar.Paint;
begin

  inherited Paint;
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
end;

procedure TSwanSpeedButtonsBar.AddButtonActions(AActions: array of TCustomAction);

var
  I, J: Integer;
  SB, PrevSB: TSwanSpeedButton;
  W, L: Integer;
  A: TCustomAction;
  ImgW: Integer;

begin
  ImgW := FImageWidth;
  if ImgW = 0 then
    ImgW := 16;

  PrevSB := nil;

  DisableAutoSizing;
  try
    L := 0;
    for I := Low(AActions) to High(AActions) do begin
      A := AActions[I];

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
          SB.AnchorParallel(akLeft, L, Self)
        else
          SB.AnchorToNeighbour(akLeft, L, PrevSB);

        SB.ClientWidth := W + 1;
        SB.ClientHeight := W;
        SB.ImageWidth := ImgW;

        SB.Action := A;

        SB.Parent := Self;

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

