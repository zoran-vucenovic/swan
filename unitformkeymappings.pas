unit UnitFormKeyMappings;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitFrameOneKeyMap, UnitKeyMapRecords, UnitFormPressAKey,
  CommonFunctionsLCL, Forms, Controls, Graphics, Dialogs, ExtCtrls, ButtonPanel,
  StdCtrls, LazUTF8;

type

  { TFormKeyMappings }

  TFormKeyMappings = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ButtonPanel1: TButtonPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    PanelAddRemoveScheme: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    type
      TKeyMapsObj = class(TObject)
      public
        KM: TKeyMapRecs;
      end;

  private
    KeyMappingsCtrl: TControlKeyMappings;
    LastComboItemIndex: Integer;
    procedure FillKeyMappingsNames;
    procedure FillCurrentMap;
    procedure SetMappingsToDefault;
    procedure ComboMappingNamesChg(Sender: TObject);
    procedure SaveToKeyMappings;
    procedure UpdateCurrentComboItem();
    procedure AfterShow(Data: PtrInt);
    function AddOneMappingToCombo(AMappingName: String; KMRecs: TKeyMapRecs): Integer;
    procedure ClearCombo;

    procedure AddPCKeyOnClick(Sender: TObject);
    procedure SaveSchemeOnClick(Sender: TObject);
    procedure RemoveSchemeOnClick(Sender: TObject);
    procedure ResetToDefaultOnClick(Sender: TObject);
  public
    class function ShowFormKeyMappings(): Boolean;
  end;

implementation

{$R *.lfm}

{ TFormKeyMappings }

procedure TFormKeyMappings.FormCreate(Sender: TObject);
var
  LabelAddPCKey: TCustomLabel;
  LabelAddScheme: TCustomLabel;
  LabelRemoveScheme: TCustomLabel;
  LabelResetToDefault: TCustomLabel;
begin
  LastComboItemIndex := -1;
  Label5.Caption := 'Note: ';
  Label5.Font.Style := Label5.Font.Style + [fsBold];
  Label5.Font.Color := clMaroon;
  Label4.Caption :=
    'When Joystick is enabled, then key mappings to joystick take precedence over mappings to spectrum keys defined here.';
  Label4.Font.Color := clMaroon;

  PanelAddRemoveScheme.BevelOuter := bvNone;
  Panel1.BevelOuter := bvNone;
  Panel1.AutoSize := True;

  KeyMappingsCtrl := TControlKeyMappings.Create(Panel7);
  KeyMappingsCtrl.Anchors := [];
  KeyMappingsCtrl.AnchorParallel(akTop, 0, Panel7);
  KeyMappingsCtrl.AnchorParallel(akLeft, 0, Panel7);
  KeyMappingsCtrl.AnchorParallel(akBottom, 0, Panel7);
  KeyMappingsCtrl.AnchorParallel(akRight, 0, Panel7);
  KeyMappingsCtrl.Parent := Panel7;

  LabelAddPCKey := TCommonFunctionsLCL.CreateLinkLabel(Panel6, 'Add key mapping...');
  LabelAddPCKey.Anchors := [];
  LabelAddPCKey.AnchorParallel(akLeft, 0, Panel6);
  LabelAddPCKey.AnchorParallel(akTop, 0, Panel6);
  LabelAddPCKey.Hint := 'Add (or find existing) key mapping';

  LabelAddScheme := TCommonFunctionsLCL.CreateLinkLabel(PanelAddRemoveScheme, 'Save as...');
  LabelAddScheme.Anchors := [];
  LabelAddScheme.AnchorParallel(akLeft, 0, PanelAddRemoveScheme);
  LabelAddScheme.AnchorParallel(akTop, 0, PanelAddRemoveScheme);

  LabelRemoveScheme := TCommonFunctionsLCL.CreateLinkLabel(PanelAddRemoveScheme, 'Remove');
  LabelRemoveScheme.Anchors := [];
  LabelRemoveScheme.AnchorToNeighbour(akLeft, 6, LabelAddScheme);
  LabelRemoveScheme.AnchorParallel(akTop, 0, PanelAddRemoveScheme);

  LabelResetToDefault := TCommonFunctionsLCL.CreateLinkLabel(PanelAddRemoveScheme, 'Reset to default');
  LabelResetToDefault.Anchors := [];
  LabelResetToDefault.AnchorToNeighbour(akLeft, 6, LabelRemoveScheme);
  LabelResetToDefault.AnchorParallel(akTop, 0, PanelAddRemoveScheme);

  LabelAddPCKey.Parent := Panel6;
  LabelAddScheme.Parent := PanelAddRemoveScheme;
  LabelRemoveScheme.Parent := PanelAddRemoveScheme;
  LabelResetToDefault.Parent := PanelAddRemoveScheme;

  PanelAddRemoveScheme.AutoSize := True;
  Panel6.AutoSize := True;
  LabelAddPCKey.OnClick := @AddPCKeyOnClick;
  LabelAddScheme.OnClick := @SaveSchemeOnClick;
  LabelRemoveScheme.OnClick := @RemoveSchemeOnClick;
  LabelResetToDefault.OnClick := @ResetToDefaultOnClick;

  HandleNeeded;
end;

procedure TFormKeyMappings.FormDestroy(Sender: TObject);
begin
  Screen.RemoveAllHandlersOfObject(Self);
  ClearCombo;
end;

procedure TFormKeyMappings.FormShow(Sender: TObject);
begin
  //
  AfterShow(1);
end;

procedure TFormKeyMappings.FillKeyMappingsNames;
var
  I, N: Integer;
  S: RawByteString;
  KM: TKeyMapRecs;
begin
  ClearCombo;
  N := -1;
  if TKeyMappings.KMappings.Count > 0 then begin
    for I := 0 to TKeyMappings.KMappings.Count - 1 do begin
      S := TKeyMappings.KMappings.Keys[I];
      if S <> '' then begin
        KM := TKeyMappings.KMappings.Data[I];

        AddOneMappingToCombo(S, KM);

        if N < 0 then
          if TKeyMappings.KMappings.ActiveKeyMapName = S then
            N := ComboBox1.Items.Count - 1;
      end;
    end;
  end;
  ComboBox1.ItemIndex := N;
end;

procedure TFormKeyMappings.FillCurrentMap;
begin
  if ComboBox1.ItemIndex >= 0 then begin
    LastComboItemIndex := ComboBox1.ItemIndex;
    KeyMappingsCtrl.LoadFromKeyMapRecs(TKeyMapsObj(ComboBox1.Items.Objects[LastComboItemIndex]).KM);
  end else
    SetMappingsToDefault;

end;

procedure TFormKeyMappings.SetMappingsToDefault;
begin
  LastComboItemIndex := -1;
  KeyMappingsCtrl.LoadFromKeyMapRecs(TKeyMappings.KMappings.DefaultMapRecs);
end;

procedure TFormKeyMappings.ComboMappingNamesChg(Sender: TObject);
begin
  UpdateCurrentComboItem();
  FillCurrentMap;
end;

procedure TFormKeyMappings.SaveToKeyMappings;
var
  I: Integer;
  KMObj: TKeyMapsObj;
  S: RawByteString;
  Obj: TObject;

begin
  TKeyMappings.KMappings.ActiveKeyMapName := '';
  UpdateCurrentComboItem();

  TKeyMappings.KMappings.Clear;
  for I := 0 to ComboBox1.Items.Count - 1 do begin
    //
    Obj := ComboBox1.Items.Objects[I];
    if Obj is TKeyMapsObj then begin
      KMObj := TKeyMapsObj(Obj);
      S := ComboBox1.Items.Strings[I];

      TKeyMappings.KMappings.Add(S, KMObj.KM);
      if I = ComboBox1.ItemIndex then
        TKeyMappings.KMappings.ActiveKeyMapName := S;
    end;
  end;

end;

procedure TFormKeyMappings.UpdateCurrentComboItem;
var
  KMRecs: TKeyMapRecs;
  KMObj: TKeyMapsObj;
  Obj: TObject;
begin
  if (LastComboItemIndex >= 0) and (LastComboItemIndex < ComboBox1.Items.Count) then begin
    KeyMappingsCtrl.SaveToKeyMapRecs(KMRecs);
    Obj := ComboBox1.Items.Objects[LastComboItemIndex];
    if Obj is TKeyMapsObj then begin
      KMObj := TKeyMapsObj(Obj);
      SetLength(KMObj.KM, 0);
      KMObj.KM := KMRecs;
    end;
  end;
end;

procedure TFormKeyMappings.AfterShow(Data: PtrInt);
begin
  PanelAddRemoveScheme.BorderSpacing.Left :=
    Panel2.ScreenToClient(ComboBox1.ClientOrigin).X;

  TCommonFunctionsLCL.AdjustFormPos(Self);
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShow, Data - 1);
end;

function TFormKeyMappings.AddOneMappingToCombo(AMappingName: String;
  KMRecs: TKeyMapRecs): Integer;
var
  J: Integer;
  KMObj: TKeyMapsObj;
begin
  Result := -1;
  AMappingName := UTF8Trim(AMappingName);
  if AMappingName <> '' then begin
    KMObj := TKeyMapsObj.Create;
    SetLength(KMObj.KM, Length(KMRecs));
    for J := 0 to High(KMRecs) do
      KMObj.KM[J] := KMRecs[J];

    Result := ComboBox1.Items.AddObject(AMappingName, KMObj);
  end;
end;

procedure TFormKeyMappings.ClearCombo;
var
  I: Integer;
begin
  for I := 0 to ComboBox1.Items.Count - 1 do begin
    ComboBox1.Items.Objects[I].Free;
    ComboBox1.Items.Objects[I] := nil;
  end;
  ComboBox1.Clear;
end;

procedure TFormKeyMappings.AddPCKeyOnClick(Sender: TObject);
var
  W: Word;
begin
  if TFormPressAKey.ShowFormPressAKey(W) then begin
    KeyMappingsCtrl.AddPCKey(W);
  end;
end;

procedure TFormKeyMappings.SaveSchemeOnClick(Sender: TObject);
var
  S: String;
  N: Integer;
  KMObj: TKeyMapsObj;
  KMRecs: TKeyMapRecs;
begin
  if ComboBox1.ItemIndex >= 0 then
    S := ComboBox1.Items.Strings[ComboBox1.ItemIndex]
  else
    S := '';
  if InputQuery('Save current scheme', 'Scheme name:', S) then begin
    S := UTF8Trim(S);
    if S <> '' then begin
      N := ComboBox1.Items.IndexOf(S);
      if (N < 0) or
        (MessageDlg(
          Format('Scheme named "%s" already exists.%sDo you want to overwrite this scheme?'
            , [S, LineEnding])
          , mtConfirmation, [mbYes, mbNo], 0) = mrYes)
      then begin
        KeyMappingsCtrl.SaveToKeyMapRecs(KMRecs);
        if N < 0 then
          N := AddOneMappingToCombo(S, KMRecs)
        else begin
          KMObj := TKeyMapsObj.Create;
          KMObj.KM := KMRecs;
          ComboBox1.Items.Objects[N].Free;
          ComboBox1.Items.Objects[N] := KMObj;
        end;
        LastComboItemIndex := N;
        ComboBox1.ItemIndex := N;
      end;

    end;
  end;
end;

procedure TFormKeyMappings.RemoveSchemeOnClick(Sender: TObject);
var
  N: Integer;
begin
  N := ComboBox1.ItemIndex;
  if N >= 0 then begin
    ComboBox1.ItemIndex := -1;
    ComboBox1.Items.Objects[N].Free;
    ComboBox1.Items.Objects[N] := nil;
    ComboBox1.Items.Delete(N);
    SetMappingsToDefault;
  end;
end;

procedure TFormKeyMappings.ResetToDefaultOnClick(Sender: TObject);
begin
  ComboBox1.ItemIndex := -1;
  SetMappingsToDefault;
end;

class function TFormKeyMappings.ShowFormKeyMappings: Boolean;
var
  F: TFormKeyMappings;
begin
  Result := False;
  F := TFormKeyMappings.Create(nil);
  try
    F.FillKeyMappingsNames;
    F.FillCurrentMap;
    F.ComboBox1.OnChange := @(F.ComboMappingNamesChg);
    if F.ShowModal = mrOK then begin
      F.SaveToKeyMappings;
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

end.

