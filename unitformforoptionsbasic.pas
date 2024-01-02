unit UnitFormForOptionsBasic;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, UnitCommon, Forms, ButtonPanel,
  LazMethodList, Controls;

type

  TFormForOptionsBasic = class(TCustomForm, IFormAddCloseQuery)
  private
    CloseQueryList: TMethodList;
    MainControl: TControl;
    BP: TButtonPanel;
    Panel1: TCustomControl;
    FFixConstraints: Boolean;

    function GetFramesArrCount: Integer;
    procedure AfterShow(Data: PtrInt);

  protected
    procedure DoShow; override;
  public
    const
      DefaultPanelButtons = [TPanelButton.pbOK, TPanelButton.pbCancel];
      //DefaultPanelButtonsWithHelp: TPanelButtons = [TPanelButton.pbOK, TPanelButton.pbCancel, TPanelButton.pbHelp];
  public
    constructor CreateForControl(AOwner: TComponent; C: TControl;
      AFixConstraints: Boolean; Buttons: TPanelButtons = DefaultPanelButtons);
    destructor Destroy; override;

    function CloseQuery: Boolean; override;
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;

    procedure AddCloseQuery(C: TCloseQueryEvent);

    property FramesArrCount: Integer read GetFramesArrCount;
  end;

implementation

{ TFormForOptionsBasic }

function TFormForOptionsBasic.CloseQuery: Boolean;
var
  I: Integer;
begin
  Result := inherited CloseQuery;

  I := 0;
  while Result and (I < CloseQueryList.Count) do begin
    TCloseQueryEvent(CloseQueryList[I])(Self, Result);
    Inc(I);
  end;
end;

procedure TFormForOptionsBasic.RemoveAllHandlersOfObject(AnObject: TObject);
var
  I: Integer;
begin
  inherited RemoveAllHandlersOfObject(AnObject);

  for I := 0 to CloseQueryList.Count - 1 do
    CloseQueryList.RemoveAllMethodsOfObject(AnObject);
  if CloseQueryList.Count = 0 then
    FreeAndNil(CloseQueryList);
end;

function TFormForOptionsBasic.GetFramesArrCount: Integer;
begin
  if Assigned(CloseQueryList) then
    Exit(CloseQueryList.Count);
  Result := 0;
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
      if Assigned(BP) then begin
        BP.Anchors := BP.Anchors - [akTop];
        BP.AnchorParallel(akBottom, 0, Panel1);
        MainControl.AnchorToNeighbour(akBottom, 0, BP);
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
  C: TControl; AFixConstraints: Boolean; Buttons: TPanelButtons);

begin
  if C = nil then
    Abort;
  inherited CreateNew(AOwner, 0);

  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);
  FFixConstraints := AFixConstraints;

  //BorderStyle := bsSingle;
  BorderIcons := BorderIcons - [TBorderIcon.biMaximize, TBorderIcon.biMinimize];

  BP := nil;
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

  if Buttons <> [] then begin
    BP := TButtonPanel.Create(Panel1);
    BP.Name := TCommonFunctions.GlobalObjectNameGenerator(BP);
    BP.Align := alNone;
    BP.Anchors := [];
    BP.ShowBevel := False;
    BP.ShowButtons := Buttons;

    BP.AnchorParallel(akRight, 0, Panel1);
    BP.AnchorToNeighbour(akTop, 0, C);
    BP.BorderSpacing.Around := 6;

    BP.Parent := Panel1;
  end;
                      
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

