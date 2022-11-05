unit UnitFormPressAKey;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  TFormPressAKey = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FKey: Word;
    procedure AfterShow(Data: PtrInt);
  public
    class function ShowFormPressAKey(out AKey: Word): Boolean;
  end;

implementation

{$R *.lfm}

{ TFormPressAKey }

procedure TFormPressAKey.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  FKey := Key;
  ModalResult := mrOK;
end;

procedure TFormPressAKey.FormShow(Sender: TObject);
begin
  AfterShow(2);
end;

procedure TFormPressAKey.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(Self);
end;

procedure TFormPressAKey.FormCreate(Sender: TObject);
begin
  TCommonFunctionsLCL.FormToScreenCentre(Self);
end;

procedure TFormPressAKey.AfterShow(Data: PtrInt);
begin
  TCommonFunctionsLCL.FormToScreenCentre(Self);
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShow, Data - 1);
end;

class function TFormPressAKey.ShowFormPressAKey(out AKey: Word): Boolean;
var
  F: TFormPressAKey;
begin
  Result := False;
  F := TFormPressAKey.Create(nil);
  try
    F.Caption := 'Choose PC key';
    if F.ShowModal = mrOK then begin
      AKey := F.FKey;
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

end.

