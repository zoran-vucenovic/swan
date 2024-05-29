unit UnitFrameBreakpoints;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, UnitDebugBreakpointsGrid, UnitDebugger,
  UnitCommon, Forms, Controls, ExtCtrls, StdCtrls, ActnList, Menus;

type
  TFrameBreakpoints = class(TFrame)
    ActionRemoveAll: TAction;
    ActionRemove: TAction;
    ActionEdit: TAction;
    ActionAdd: TAction;
    ActionListEditBreakpoints: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    procedure ActionAddExecute(Sender: TObject);
    procedure ActionEditExecute(Sender: TObject);
    procedure ActionRemoveAllExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
  private
    FPanelWithActionButtons: TCustomControl;
    FGridBreakpoints: TGridBreakpoints;
    FPopupMenu: TPopupMenu;

    function GetHasPanelWithActionButtons: Boolean;
    procedure SetHasPanelWithActionButtons(AValue: Boolean);
    procedure GridOnContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property HasPanelWithActionButtons: Boolean
       read GetHasPanelWithActionButtons write SetHasPanelWithActionButtons;
    property GridBreakpoints: TGridBreakpoints read FGridBreakpoints;

    class function ShowFormBreakpoints(AOwner: TComponent; ADebugger: TDebugger;
      AHasPanelWithActionButtons: Boolean): TCustomForm; static;
  end;

implementation

{$R *.lfm}

{ TFrameBreakpoints }

procedure TFrameBreakpoints.ActionAddExecute(Sender: TObject);
begin
  //
end;

procedure TFrameBreakpoints.ActionEditExecute(Sender: TObject);
begin
  //
end;

procedure TFrameBreakpoints.ActionRemoveAllExecute(Sender: TObject);
begin
  FGridBreakpoints.Debugger.RemoveAllBreakpoints();
end;

procedure TFrameBreakpoints.ActionRemoveExecute(Sender: TObject);
var
  I: Integer;
  W: Word;
begin
  I := FGridBreakpoints.Row - FGridBreakpoints.FixedRows;
  if (I >= 0) and (I < FGridBreakpoints.Debugger.Breakpoints.Count) then begin
    W := FGridBreakpoints.Debugger.Breakpoints.Keys[I];
    FGridBreakpoints.Debugger.RemoveBreakpoint(W);
  end;
end;

function TFrameBreakpoints.GetHasPanelWithActionButtons: Boolean;
begin
  Result := Assigned(FPanelWithActionButtons);
end;

procedure TFrameBreakpoints.SetHasPanelWithActionButtons(AValue: Boolean);
var
  Lab: TCustomLabel;
  CPrev: TControl;
  I: Integer;
  Act: TBasicAction;

begin
  if GetHasPanelWithActionButtons xor AValue then begin
    DisableAlign;
    try
      if not AValue then begin
        Panel1.AnchorParallel(akTop, 0, Self);
        FreeAndNil(FPanelWithActionButtons);
      end else begin
        FPanelWithActionButtons := TCustomControl.Create(nil);
        FPanelWithActionButtons.Anchors := [];
        FPanelWithActionButtons.AnchorParallel(akLeft, 0, Self);
        FPanelWithActionButtons.AnchorParallel(akTop, 0, Self);
        FPanelWithActionButtons.AnchorParallel(akRight, 0, Self);
        FPanelWithActionButtons.BorderSpacing.Around := 3;

        CPrev := nil;
        for I := 0 to ActionListEditBreakpoints.ActionCount - 1 do begin
          Act := ActionListEditBreakpoints.Actions[I];
          if (Act is TCustomAction) and TCustomAction(Act).Visible then begin
            Lab := TCommonFunctionsLCL.CreateLinkLabel(FPanelWithActionButtons);
            Lab.Action := Act;
            Lab.ShowHint := True;
            if CPrev = nil then
              Lab.AnchorParallel(akLeft, 0, FPanelWithActionButtons)
            else
              Lab.AnchorToNeighbour(akLeft, 4, CPrev);
            Lab.AnchorParallel(akTop, 0, FPanelWithActionButtons);
            Lab.Parent := FPanelWithActionButtons;
            CPrev := Lab;
          end;
        end;

        FPanelWithActionButtons.AutoSize := True;
        FPanelWithActionButtons.Parent := Self;

        Panel1.AnchorToNeighbour(akTop, 0, FPanelWithActionButtons);
      end;

    finally
      EnableAlign;
    end;
  end;
end;

procedure TFrameBreakpoints.GridOnContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  P: TPoint;

begin
  ActionRemove.Enabled := TCommonFunctionsLCL.GridMouseToCellRegular(FGridBreakpoints, MousePos, P);
  Handled := P.Y < FGridBreakpoints.FixedRows;
  if not Handled then begin
    ActionRemoveAll.Enabled := FGridBreakpoints.RowCount > FGridBreakpoints.FixedRows;
    ActionEdit.Enabled := ActionRemove.Enabled;
  end;
end;

constructor TFrameBreakpoints.Create(TheOwner: TComponent);
var
  Mi: TMenuItem;
  I: Integer;

begin
  inherited Create(TheOwner);

  {$ifdef ConditionalBreakpointsNotYetImplemented}
  ActionEdit.Enabled := False;
  ActionEdit.Visible := False;
  ActionAdd.Enabled := False;
  ActionAdd.Visible := False;
  {$endif}

  FPanelWithActionButtons := nil;

  Panel1.Caption := '';

  FGridBreakpoints := TGridBreakpoints.Create(nil);
  FGridBreakpoints.AnchorParallel(akLeft, 0, Panel1);
  FGridBreakpoints.AnchorParallel(akTop, 0, Panel1);
  FGridBreakpoints.AnchorParallel(akRight, 0, Panel1);
  FGridBreakpoints.AnchorParallel(akBottom, 0, Panel1);

  FGridBreakpoints.Parent := Panel1;

  FPopupMenu := TPopupMenu.Create(nil);
  for I := 0 to ActionListEditBreakpoints.ActionCount - 1 do begin
    Mi := TMenuItem.Create(FPopupMenu);
    Mi.Action := ActionListEditBreakpoints.Actions[I];
    FPopupMenu.Items.Add(Mi);
  end;

  FGridBreakpoints.PopupMenu := FPopupMenu;
  FGridBreakpoints.OnContextPopup := @GridOnContextPopup;
end;

destructor TFrameBreakpoints.Destroy;
begin
  FPopupMenu.Free;
  FPanelWithActionButtons.Free;
  FGridBreakpoints.Free;

  inherited Destroy;
end;

type
  TBreakpointsForm = class(TCustomForm)
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;
  end;

{ TBreakpointsForm }

procedure TBreakpointsForm.DoClose(var CloseAction: TCloseAction);
begin
  //inherited DoClose(CloseAction);
  CloseAction := TCloseAction.caFree;
end;

class function TFrameBreakpoints.ShowFormBreakpoints(AOwner: TComponent;
  ADebugger: TDebugger; AHasPanelWithActionButtons: Boolean): TCustomForm;
var
  F: TBreakpointsForm;
  Fm: TFrameBreakpoints;
begin
  F := TBreakpointsForm.CreateNew(AOwner, 0);
  F.Name := TCommonFunctions.GlobalObjectNameGenerator(F);
  F.Caption := 'Breakpoints';

  F.BorderIcons := F.BorderIcons - [TBorderIcon.biMaximize, TBorderIcon.biMinimize];
  F.Position := TPosition.poDesigned;
  Fm := TFrameBreakpoints.Create(F);
  Fm.HasPanelWithActionButtons := AHasPanelWithActionButtons;
  Fm.GridBreakpoints.Debugger := ADebugger;
  F.SetBounds((Screen.Width - Fm.Width) div 2, (Screen.Height - Fm.Height) div 2, Fm.Width, Fm.Height);
  F.Constraints.MinHeight := (F.Height * 3) div 5;
  F.Constraints.MinWidth := (F.Width * 5) div 9;
  Fm.Anchors := [];
  Fm.AnchorParallel(akLeft, 0, F);
  Fm.AnchorParallel(akTop, 0, F);
  Fm.AnchorParallel(akRight, 0, F);
  Fm.AnchorParallel(akBottom, 0, F);
  //
  Fm.Parent := F;

  F.PopupMode := TPopupMode.pmAuto;
  F.Show;
  Result := F;
end;

end.

