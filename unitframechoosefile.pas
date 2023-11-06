unit UnitFrameChooseFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Dialogs, StdCtrls, Buttons;

type
  TFrameChooseFile = class(TFrame)
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    Panel4: TPanel;
    procedure BitBtn1Click(Sender: TObject);
  private
    function GetPath: String;
    procedure SetPath(const AValue: String);

  public
    procedure InitialPath(var S: String);

    property Path: String read GetPath write SetPath;
  end;

implementation

{$R *.lfm}

{ TFrameChooseFile }

procedure TFrameChooseFile.BitBtn1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    Edit1.Text := OpenDialog1.FileName;
  end;
  OpenDialog1.FileName := ExtractFileName(OpenDialog1.FileName);
end;

function TFrameChooseFile.GetPath: String;
begin
  Result := Edit1.Text;
end;

procedure TFrameChooseFile.SetPath(const AValue: String);
begin
  Edit1.Text
end;

procedure TFrameChooseFile.InitialPath(var S: String);
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
end;

end.

