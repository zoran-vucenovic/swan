unit UnitInputLibraryPathDialog;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitFormForOptionsBasic, UnitCommon, CommonFunctionsLCL,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  TFrameInputLibraryPath = class(TFrame)
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure BitBtn1Click(Sender: TObject);
  public
    type
      TOnCheckLoadEvent = function(const APath: String): Boolean of object;
  private
    FOnCheckLoad: TOnCheckLoadEvent;
    FDoOnCloseOK: TProcedureOfObjectString;

    procedure InitProc(var S: String);
    procedure AfterShow(Data: PtrInt);
    procedure OnFormFirstShow(Sender: TObject);
    procedure MakeExtensionsFilter;
    function DoCheckLoad: Boolean;
    procedure FormOnCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormOnClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateLibraryPathDialog(AOwner: TComponent; S: String;
      const AOnCheckLoad: TOnCheckLoadEvent; ADoOnCloseOK: TProcedureOfObjectString);

    procedure AddFormEvents(F: TCustomForm);
    class function ShowLibraryPathDialog(const S: String;
      const AOnCheckLoad: TOnCheckLoadEvent; ADoOnCloseOK: TProcedureOfObjectString): Boolean;
  end;

implementation

{$R *.lfm}

{ TFrameInputLibraryPath }

procedure TFrameInputLibraryPath.InitProc(var S: String);
var
  S1: String;
  L: Integer;
  Separators: set of AnsiChar;
begin
  S := Trim(S);
  Edit1.Text := S;

  Separators := AllowDirectorySeparators + AllowDriveSeparators;
  S1 := S;
  repeat
    L := Length(S1);
    if (L > 0) and (S1[L] in Separators) then
      Delete(S1, L, 1);
    S1 := Trim(ExtractFilePath(S1));
    if Length(S1) = L then begin
      S1 := '';
    end;
  until (S1 = '') or (DirectoryExists(S1));

  if S1 = '' then
    OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName)
  else
    OpenDialog1.InitialDir := S1;

  OpenDialog1.FileName := ExtractFileName(S);
  MakeExtensionsFilter;
end;

procedure TFrameInputLibraryPath.BitBtn1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    Edit1.Text := OpenDialog1.FileName;
  end;    
  OpenDialog1.FileName := ExtractFileName(OpenDialog1.FileName);
end;

procedure TFrameInputLibraryPath.FormOnCloseQuery(Sender: TObject;
  var CanClose: Boolean);
const
  ContinueClicked = mrLast + 1;
  CancelClicked = ContinueClicked + 1;
  Msg: String =
    '%sPortAudio library cannot be %s' + LineEnding + LineEnding
      + 'Do you want to continue without sound, or keep trying to locate the library%s?'
    ;
var
  S, S1, S2, S3: String;
  F: TCustomForm;
begin
  if Sender is TCustomForm then begin
    F := TCustomForm(Sender);
    CanClose := F.ModalResult <> mrOK;
    if not CanClose then begin
      S := Trim(Edit1.Text);
      Edit1.Text := S;
      S1 := '';
      S2 := '';
      S3 := '';
      if S <> '' then begin
        if not FileExists(S) then begin
          S1 := 'File "' + S + '"' + LineEnding
            + 'does not exist.' + LineEnding + LineEnding;
          S2 := 'loaded.';
        end else if not DoCheckLoad then begin
          S2 := 'loaded from file "' + S + '".';
        end else
          CanClose := True;
      end else begin
        CanClose := DoCheckLoad;
        S2 := 'found in default locations.';
        S3 := ' yourself';
      end;

      CanClose := CanClose or
        (QuestionDlg('PortAudio library loading failed', Format(Msg, [S1, S2, S3]), TMsgDlgType.mtConfirmation,
           [ContinueClicked, 'Continue without sound', 'IsDefault',
           CancelClicked, 'Keep trying', 'IsCancel'], 0)
         = ContinueClicked);
    end;
  end;
end;

procedure TFrameInputLibraryPath.FormOnClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  S: String;
begin
  if Sender is TCustomForm then
    if TCustomForm(Sender).ModalResult = mrOK then
      if Assigned(FDoOnCloseOK) then begin
        S := Trim(Edit1.Text);
        FDoOnCloseOK(S);
      end;

end;

constructor TFrameInputLibraryPath.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FOnCheckLoad := nil;
  FDoOnCloseOK := nil;
  Edit1.Clear;
  Caption := 'Portaudio library path';
end;

constructor TFrameInputLibraryPath.CreateLibraryPathDialog(AOwner: TComponent;
  S: String; const AOnCheckLoad: TOnCheckLoadEvent;
  ADoOnCloseOK: TProcedureOfObjectString);
begin
  Create(AOwner);
  InitProc(S);
  FOnCheckLoad := AOnCheckLoad;
  FDoOnCloseOK := ADoOnCloseOK;
end;

procedure TFrameInputLibraryPath.AddFormEvents(F: TCustomForm);
begin
  F.AddHandlerFirstShow(@OnFormFirstShow);
  if F is IFormAddCloseQuery then
    (F as IFormAddCloseQuery).AddCloseQuery(@FormOnCloseQuery);
  F.AddHandlerClose(@FormOnClose);
end;

procedure TFrameInputLibraryPath.AfterShow(Data: PtrInt);
begin
  if Data > 0 then begin
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end else begin
    DisableAlign;
    try
      Panel2.AutoSize := False;
      Panel1.AutoSize := False;
      BitBtn1.Anchors := BitBtn1.Anchors - [akLeft];
      BitBtn1.AnchorParallel(akRight, 0, Panel1);
      Edit1.AnchorToNeighbour(akRight, 0, BitBtn1);
      Panel1.AnchorParallel(akRight, 0, Panel2);
      Panel2.AnchorParallel(akRight, 0, Self);
    finally
      EnableAlign;
    end;
  end;
end;

procedure TFrameInputLibraryPath.OnFormFirstShow(Sender: TObject);
begin
  AfterShow(1);
end;

procedure TFrameInputLibraryPath.MakeExtensionsFilter;
begin
  OpenDialog1.Filter := TCommonFunctionsLCL.MakeExtensionsFilter([
    {$if defined(mswindows)}
      'dll'
    {$elseif defined(darwin)}
      'dylib'
    {$elseif defined(unix)}
      'so'
    {$else}
      {$fatal OS not supported!}
    {$endif}
  ]);
end;

function TFrameInputLibraryPath.DoCheckLoad: Boolean;
begin
  if Assigned(FOnCheckLoad) then
    Result := FOnCheckLoad(Trim(Edit1.Text));
end;

class function TFrameInputLibraryPath.ShowLibraryPathDialog(const S: String;
  const AOnCheckLoad: TOnCheckLoadEvent; ADoOnCloseOK: TProcedureOfObjectString
  ): Boolean;
var
  Fm: TFrameInputLibraryPath;
  F: TFormForOptionsBasic;
begin
  Result := False;
  Fm := TFrameInputLibraryPath.CreateLibraryPathDialog(
    nil, S, AOnCheckLoad, ADoOnCloseOK);
  try
    F := TFormForOptionsBasic.CreateForControl(nil, Fm, True);
    try
      Fm.AddFormEvents(F);
      if F.ShowModal = mrOK then begin
        Result := True;
      end;

    finally
      F.Free;
    end;
  finally
    Fm.Free;
  end;
end;

end.

