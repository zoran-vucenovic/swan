unit UnitKeyboardOnScreen;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  SysUtils, CommonFunctionsLCL, UnitSpectrumKeysControl,
  Forms, Controls, Dialogs, ExtCtrls;

type
  TFormKeyboardOnScreen = class(TForm)
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSpectrumKeysControl: TSpectrumKeysControl;
    function GetOnChgSpectrumKeyEx: TChgSpectrumKeyEx;
    procedure SetOnChgSpectrumKeyEx(AValue: TChgSpectrumKeyEx);
    procedure AfterShow(Data: PtrInt);
  public
    class function ShowSpectrumKeyboardOnScreen(): TFormKeyboardOnScreen;
    procedure ReleaseShifts;

    property OnChgSpectrumKeyEx: TChgSpectrumKeyEx read GetOnChgSpectrumKeyEx write SetOnChgSpectrumKeyEx;
  end;

implementation

{$R *.lfm}

{ TFormKeyboardOnScreen }

procedure TFormKeyboardOnScreen.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := TCloseAction.caFree;
end;

procedure TFormKeyboardOnScreen.FormCreate(Sender: TObject);
begin
  Caption := 'Spectrum keyboard - click on the keys';
  FSpectrumKeysControl := TSpectrumKeysControl.Create(Self, False);
  FSpectrumKeysControl.Anchors := [];
  FSpectrumKeysControl.AnchorParallel(akLeft, 0, Panel1);
  FSpectrumKeysControl.AnchorParallel(akTop, 6, Panel1);
  FSpectrumKeysControl.BorderSpacing.Right := 6;
  FSpectrumKeysControl.BorderSpacing.Bottom := 6;
  FSpectrumKeysControl.Parent := Panel1;
  AutoSize := True;
end;

procedure TFormKeyboardOnScreen.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(Self);
  FSpectrumKeysControl.Free;
end;

procedure TFormKeyboardOnScreen.FormShow(Sender: TObject);
begin
  AfterShow(1);
end;

procedure TFormKeyboardOnScreen.ReleaseShifts;
begin
  FSpectrumKeysControl.ReleaseShifts;
end;

function TFormKeyboardOnScreen.GetOnChgSpectrumKeyEx: TChgSpectrumKeyEx;
begin
  Result := FSpectrumKeysControl.OnChgSpectrumKeyEx;
end;

procedure TFormKeyboardOnScreen.SetOnChgSpectrumKeyEx(AValue: TChgSpectrumKeyEx
  );
begin
  FSpectrumKeysControl.OnChgSpectrumKeyEx := AValue;
end;

procedure TFormKeyboardOnScreen.AfterShow(Data: PtrInt);
begin
  TCommonFunctionsLCL.AdjustFormPos(Self, True);
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShow, Data - 1);
end;

class function TFormKeyboardOnScreen.ShowSpectrumKeyboardOnScreen: TFormKeyboardOnScreen;
begin
  Result := TFormKeyboardOnScreen.Create(nil);
  TCommonFunctionsLCL.AdjustFormPos(Result, True);
end;

end.

