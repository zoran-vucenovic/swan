unit UnitSpectrumKeysDialog;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitKeyMaps, CommonFunctionsLCL,
  UnitSpectrumKeysControl, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel;

type

  { TFormSpectrumKeysDialog }

  TFormSpectrumKeysDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelViewKeys: TPanel;
    PanelSpectrumKeys: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SpectrumKeysControl: TSpectrumKeysControl;
    PCKeyText: String;
    ControlSpectrumKeysText: TCustomControl;
    procedure UpdateKeysText;
    procedure OnChangeSpectrumKey(Sender: TObject);
    procedure AfterShow(Data: PtrInt);
  public
    class function EditKeyMappings(const PCKey: Word; var SpectrumKeys: TArrayOfWord): Boolean;
  end;

implementation

{$R *.lfm}

{ TFormSpectrumKeysDialog }

procedure TFormSpectrumKeysDialog.FormCreate(Sender: TObject);
begin
  PCKeyText := ' ';
  ControlSpectrumKeysText := nil;
  Label1.ShowAccelChar := False;
  Label2.ShowAccelChar := False;
  Label3.ShowAccelChar := False;

  SpectrumKeysControl := TSpectrumKeysControl.Create(PanelSpectrumKeys);
  SpectrumKeysControl.Anchors := [];
  SpectrumKeysControl.AnchorParallel(akTop, 0, PanelSpectrumKeys);
  SpectrumKeysControl.AnchorParallel(akLeft, 0, PanelSpectrumKeys);
  SpectrumKeysControl.Parent := PanelSpectrumKeys;
  PanelSpectrumKeys.BorderStyle := bsNone;
  PanelSpectrumKeys.AutoSize := True;
  SpectrumKeysControl.FOnChgSpectrumKey := @OnChangeSpectrumKey;
  CommonFunctionsLCL.TCommonFunctionsLCL.FormToScreenCentre(Self);
  Label1.Caption := 'PC key';

  Label2.Font.Style := Label2.Font.Style + [fsBold];
  Label2.Font.Color := clNavy;
  Label2.Font.Size := 10;
end;

procedure TFormSpectrumKeysDialog.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(Self);
  Application.RemoveAllHandlersOfObject(Self);
end;

procedure TFormSpectrumKeysDialog.FormShow(Sender: TObject);
begin

  AfterShow(2);
end;

procedure TFormSpectrumKeysDialog.UpdateKeysText;

  function NewLabel: TLabel;
  begin
    Result := TLabel.Create(ControlSpectrumKeysText);
    Result.ShowAccelChar := False;
    Result.Anchors := [];
    Result.AnchorVerticalCenterTo(ControlSpectrumKeysText);
    Result.Parent := ControlSpectrumKeysText;
  end;

var
  S1: String;
  I, N: Integer;
  L: TLabel;
  C: TControl;
  AW: TArrayOfWord;

begin
  Label2.Caption := PCKeyText;

  SpectrumKeysControl.GetKeyStates(AW);
  N := Length(AW);
  ControlSpectrumKeysText.Free;
  if N = 0 then begin
    Label3.Font.Style := Label3.Font.Style + [fsItalic];
    ControlSpectrumKeysText := nil;
    Label3.Caption := ' (click on Spectrum keys to map)';
  end else begin
    Label3.Font.Style := Label3.Font.Style - [fsItalic];
    ControlSpectrumKeysText := TCustomControl.Create(Panel2);
    ControlSpectrumKeysText.AutoSize := True;
    ControlSpectrumKeysText.Anchors := [];
    S1 := ' maps to Spectrum key';
    if N > 1 then
      S1 := S1 + 's';
    Label3.Caption := S1 + ' ';
    C := nil;
    for I := 0 to N - 1 do begin
      L := NewLabel;
      if C = nil then begin
        L.AnchorParallel(akLeft, 0, ControlSpectrumKeysText);
      end else begin
        L.AnchorToNeighbour(akLeft, 0, C);
        L.Caption := ' + ';
        C := L;
        L := NewLabel;
        L.AnchorToNeighbour(akLeft, 0, C);
      end;
      L.Caption := DecodeSpectrumKey(AW[I]);
      L.Font := Label2.Font;

      C := L;
    end;

    ControlSpectrumKeysText.AnchorVerticalCenterTo(Panel2);
    ControlSpectrumKeysText.Parent := Panel2;
    ControlSpectrumKeysText.AnchorToNeighbour(akLeft, 0, Label3);
  end;

end;

procedure TFormSpectrumKeysDialog.OnChangeSpectrumKey(Sender: TObject);
begin
  UpdateKeysText;
end;

procedure TFormSpectrumKeysDialog.AfterShow(Data: PtrInt);
begin
  TCommonFunctionsLCL.FormToScreenCentre(Self);
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShow, Data - 1);
end;

class function TFormSpectrumKeysDialog.EditKeyMappings(const PCKey: Word;
  var SpectrumKeys: TArrayOfWord): Boolean;
var
  F: TFormSpectrumKeysDialog;
  S: String;
begin
  Result := False;
  F := TFormSpectrumKeysDialog.Create(nil);
  try
    F.ButtonPanel1.ShowButtons := [pbOK, pbCancel];
    F.Caption := 'ZX Spectrum Keyboard';
    S := DecodePCKey(PCKey);
    if S = '' then
      S := PCKey.ToString;
    F.PCKeyText := ' ' + S;

    F.SpectrumKeysControl.SetKeyStates(SpectrumKeys);
    if F.ShowModal = mrOK then begin
      F.SpectrumKeysControl.GetKeyStates(SpectrumKeys);
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

end.

