unit UnitInputLibraryPathDialog;

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ButtonPanel;

type
  TFormInputLibraryPath = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    Edit1: TEdit;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AfterShow(Data: PtrInt);
    procedure OnFormFirstShow(Sender: TObject);
  public
    class function ShowLibraryPathDialog(var S: String): Boolean;
  end;

implementation

{$R *.lfm}

{ TFormInputLibraryPath }

procedure TFormInputLibraryPath.FormCreate(Sender: TObject);
begin
  Edit1.Clear;
  AfterShow(-1);
  Self.AddHandlerFirstShow(@OnFormFirstShow);
end;

procedure TFormInputLibraryPath.BitBtn1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    Edit1.Text := OpenDialog1.FileName;
  end;    
  OpenDialog1.FileName := ExtractFileName(OpenDialog1.FileName);
end;

procedure TFormInputLibraryPath.AfterShow(Data: PtrInt);
begin
  CommonFunctionsLCL.TCommonFunctionsLCL.FormToScreenCentre(Self);
  AutoSize := False;
  if Data > 0 then begin
    AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end;
end;

procedure TFormInputLibraryPath.OnFormFirstShow(Sender: TObject);
begin
  AfterShow(2);
end;

class function TFormInputLibraryPath.ShowLibraryPathDialog(var S: String): Boolean;
var
  F: TFormInputLibraryPath;
  S1: String;
begin
  Result := False;
  F := TFormInputLibraryPath.Create(nil);
  try
    F.Caption := 'Portaudio library path';
    S := Trim(S);
    F.Edit1.Text := S;
    if S = '' then
      S1 := Application.ExeName
    else
      S1 := S;

    F.OpenDialog1.InitialDir := ExtractFilePath(S1);
    F.OpenDialog1.FileName := ExtractFileName(S);

    if F.ShowModal = mrOK then begin
      S := Trim(F.Edit1.Text);
      Result := True;
    end;
  finally
    F.Free;
  end;
end;

end.

