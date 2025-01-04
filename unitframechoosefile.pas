unit UnitFrameChooseFile;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Dialogs, StdCtrls, Buttons;

type

  { TFrameChooseFile }

  TFrameChooseFile = class(TFrame)
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    Panel4: TPanel;
    procedure BitBtn1Click(Sender: TObject);
  private
    OpenDialog: TOpenDialog;

    function GetPath: String;
    procedure SetPath(const AValue: String);

    procedure AdjustPath(var S: String);
  public
    constructor Create(TheOwner: TComponent); override;

    procedure InitFrameChooseFile(var S: String; AOpenDialog: TOpenDialog);

    property Path: String read GetPath write SetPath;
  end;

implementation

{$R *.lfm}

{ TFrameChooseFile }

procedure TFrameChooseFile.BitBtn1Click(Sender: TObject);
var
  S: String;
begin
  S := Edit1.Text;
  AdjustPath(S);

  if OpenDialog.Execute then begin
    Edit1.Text := OpenDialog.FileName;
  end;
  OpenDialog.FileName := ExtractFileName(OpenDialog.FileName);
end;

function TFrameChooseFile.GetPath: String;
begin
  Result := Edit1.Text;
end;

procedure TFrameChooseFile.SetPath(const AValue: String);
begin
  Edit1.Text := AValue;
end;

procedure TFrameChooseFile.AdjustPath(var S: String);
var
  S1: String;
  L: Integer;
  Separators: set of AnsiChar;
begin
  Separators := AllowDirectorySeparators + AllowDriveSeparators;
  S1 := S;
  repeat
    L := Length(S);
    if (L > 0) and (S[L] in Separators) then
      Delete(S, L, 1);
    S := Trim(ExtractFilePath(S1));
    if Length(S) = L then begin
      S := '';
    end;
  until (S = '') or (DirectoryExists(S));

  if S <> '' then
    OpenDialog.InitialDir := S;

  OpenDialog.FileName := ExtractFileName(S1);
end;

constructor TFrameChooseFile.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OpenDialog := nil;
end;

procedure TFrameChooseFile.InitFrameChooseFile(var S: String;
  AOpenDialog: TOpenDialog);
begin
  OpenDialog := AOpenDialog;
  if OpenDialog = nil then begin
    OpenDialog := TOpenDialog.Create(Self);
  end;

  S := Trim(S);
  Edit1.Text := S;
end;

end.

