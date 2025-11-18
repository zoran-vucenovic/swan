unit UnitSwanButtonPanel; 
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, UnitCommon, ButtonPanel, Controls;

type

  TSwanButtonPanel = class(TCustomControl)
  strict private
    FButtons: TButtonPanel;

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TSwanButtonPanel }

constructor TSwanButtonPanel.Create(AOwner: TComponent);
var
  HelpCtrl: TControl;

begin
  inherited Create(AOwner);
  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);

  FButtons := TButtonPanel.Create(Self);
  FButtons.Name := TCommonFunctions.GlobalObjectNameGenerator(FButtons);
  FButtons.Align := alNone;
  FButtons.ShowBevel := False;
  FButtons.ShowButtons := [pbOK, pbCancel];
  FButtons.Anchors := [];
  FButtons.AnchorParallel(akRight, 0, Self);
  FButtons.AnchorVerticalCenterTo(Self);
  FButtons.BorderSpacing.Around := 0;
  FButtons.Parent := Self;
  FButtons.CancelButton.BorderSpacing.Around := 0;
  FButtons.OKButton.BorderSpacing.Around := 0;

  if Assigned(TCommonFunctionsLCL.OnGetHelpControl) then
    HelpCtrl := TCommonFunctionsLCL.OnGetHelpControl(Self)
  else
    HelpCtrl := nil;

  if Assigned(HelpCtrl) then begin
    HelpCtrl.AnchorParallel(akLeft, 0, Self);
    HelpCtrl.AnchorVerticalCenterTo(Self);
    HelpCtrl.Parent := Self;
    HelpCtrl.SendToBack;
  end;

  Anchors := [];
  Self.AutoSize := True;
end;

end.

