unit UnitFormForOptionsBasic;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, UnitCommon, UnitSwanButtonPanel, Forms,
  ButtonPanel, LazMethodList, Controls;

type

  TFormForOptionsBasic = class(TCustomForm, IFormAddCloseQuery)
  private
    CloseQueryList: TMethodList;
    MainControl: TControl;
    ButtonPanel: TSwanButtonPanel;
    Panel1: TCustomControl;
    FFixConstraints: Boolean;

    procedure AfterShow(Data: PtrInt);

  protected
    procedure DoShow; override;
  private
    const
      DefaultPanelButtons = [TPanelButton.pbOK, TPanelButton.pbCancel];
      DefaultPanelButtonsWithHelp: TPanelButtons = DefaultPanelButtons + [TPanelButton.pbHelp];

  public
    constructor CreateForControl(AOwner: TComponent; C: TControl;
      AFixConstraints: Boolean; const AHelpKeyword: AnsiString);
    destructor Destroy; override;

    function CloseQuery: Boolean; override;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;

    procedure AddCloseQuery(C: TCloseQueryEvent);
  end;

implementation

{ TFormForOptionsBasic }

function TFormForOptionsBasic.CloseQuery: Boolean;
var
  I: Integer;
begin
  Result := inherited CloseQuery;

  if Assigned(CloseQueryList) then begin
    I := 0;
    while Result and (I < CloseQueryList.Count) do begin
      TCloseQueryEvent(CloseQueryList[I])(Self, Result);
      Inc(I);
    end;
  end;
end;

procedure TFormForOptionsBasic.RemoveAllHandlersOfObject(AnObject: TObject);
begin
  inherited RemoveAllHandlersOfObject(AnObject);

  if Assigned(CloseQueryList) then begin
    CloseQueryList.RemoveAllMethodsOfObject(AnObject);
    if CloseQueryList.Count = 0 then
      FreeAndNil(CloseQueryList);
  end;
end;

procedure TFormForOptionsBasic.AfterShow(Data: PtrInt);
begin
  Self.AutoSize := False;
  if Data > 0 then begin
    Self.AutoSize := True;
    TCommonFunctionsLCL.AdjustFormPos(Self);
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end else begin
    DisableAlign;
    try
      Panel1.AutoSize := False;

      MainControl.AnchorParallel(akLeft, 0, Panel1);
      if Assigned(ButtonPanel) then begin
        ButtonPanel.Anchors := ButtonPanel.Anchors - [akTop];
        ButtonPanel.AnchorParallel(akBottom, 0, Panel1);
        MainControl.AnchorToNeighbour(akBottom, 0, ButtonPanel);
      end else begin
        MainControl.AnchorParallel(akBottom, 0, Panel1);
      end;

      Panel1.AnchorParallel(akRight, 0, Self);
      Panel1.AnchorParallel(akBottom, 0, Self);

      if FFixConstraints then begin
        Constraints.MinHeight := Height;
        Constraints.MinWidth := Width;
      end;
    finally
      EnableAlign;
    end;
  end;
end;

procedure TFormForOptionsBasic.DoShow;
begin
  inherited DoShow;
  AfterShow(2);
end;

constructor TFormForOptionsBasic.CreateForControl(AOwner: TComponent;
  C: TControl; AFixConstraints: Boolean; const AHelpKeyword: AnsiString);

begin
  if C = nil then
    Abort;
  inherited CreateNew(AOwner, 0);

  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);
  FFixConstraints := AFixConstraints;

  //BorderStyle := bsSingle;
  BorderIcons := BorderIcons - [TBorderIcon.biMaximize, TBorderIcon.biMinimize];

  MainControl := C;
  Caption := C.Caption;

  Panel1 := TCustomControl.Create(Self);
  Panel1.Name := TCommonFunctions.GlobalObjectNameGenerator(Panel1);
  Panel1.Caption := '';
  Panel1.Anchors := [];
  Panel1.AnchorParallel(akLeft, 0, Self);
  Panel1.AnchorParallel(akTop, 0, Self);

  C.Anchors := [];
  C.AnchorParallel(akRight, 0, Panel1);
  C.AnchorParallel(akTop, 0, Panel1);

  ButtonPanel := TSwanButtonPanel.Create(Self);

  Self.HelpKeyword := AHelpKeyword;

  ButtonPanel.AnchorParallel(akRight, 0, Panel1);
  ButtonPanel.AnchorToNeighbour(akTop, 0, C);
  ButtonPanel.AnchorParallel(akLeft, 0, Panel1);
  ButtonPanel.BorderSpacing.Around := 6;

  ButtonPanel.Parent := Panel1;

  C.Parent := Panel1;
  CloseQueryList := nil;

  Panel1.AutoSize := True;

  if C is TWinControl then
    TWinControl(C).TabOrder := 0;

  Panel1.Parent := Self;
end;

destructor TFormForOptionsBasic.Destroy;
begin
  FreeAndNil(CloseQueryList);
  inherited Destroy;
end;

procedure TFormForOptionsBasic.AddCloseQuery(C: TCloseQueryEvent);
begin
  if Assigned(C) then begin
    if CloseQueryList = nil then begin
      CloseQueryList := TMethodList.Create;
    end;

    CloseQueryList.Add(TMethod(C));
  end;
end;

end.
