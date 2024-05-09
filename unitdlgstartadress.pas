unit UnitDlgStartAdress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, StdCtrls,
  ButtonPanel, ExtCtrls;

type
  TFormDlgStartAddress = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpinEdit1: TSpinEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FilePath: AnsiString;
    procedure FormFirstShow(Sender: TObject);
  public
    class function ShowDlgStartAddress(var AAddress: Word; const LowAdr,
      HighAdr: Word; const AFilePath: AnsiString): TModalResult;
  end;

var
  FormDlgStartAddress: TFormDlgStartAddress;

implementation

{$R *.lfm}

{ TFormDlgStartAddress }

procedure TFormDlgStartAddress.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then begin
    CanClose := (SpinEdit1.Value <= SpinEdit1.MaxValue) and (SpinEdit1.Value >= SpinEdit1.MinValue);
  end;
end;

procedure TFormDlgStartAddress.FormCreate(Sender: TObject);
begin
  Panel1.Constraints.MinHeight := Panel1.Height;
  Panel1.AutoSize := True;
  FilePath := '';
  AddHandlerFirstShow(@FormFirstShow);
end;

procedure TFormDlgStartAddress.FormFirstShow(Sender: TObject);
var
  S: RawByteString;
begin
  S := Trim(ExtractFileName(FilePath));
  if S = '' then begin
    S := ' ';
    Panel3.ShowHint := False;
  end else begin
    Panel3.Hint := FilePath;
    Panel3.ShowHint := True;
  end;
  Label3.Caption := S;
end;

class function TFormDlgStartAddress.ShowDlgStartAddress(var AAddress: Word;
  const LowAdr, HighAdr: Word; const AFilePath: AnsiString): TModalResult;
var
  F: TFormDlgStartAddress;
  M: TModalResult;
begin
  if LowAdr = HighAdr then begin
    AAddress := LowAdr;
    Result := mrOK;
  end else if LowAdr < HighAdr then begin
    Result := mrNoToAll;
    F := TFormDlgStartAddress.Create(nil);
    try
      F.SpinEdit1.MinValue := LowAdr;
      F.SpinEdit1.MaxValue := HighAdr;
      F.SpinEdit1.Value := AAddress;
      F.FilePath := AFilePath;
      F.Label1.Caption := Format('Start address (%d — %d):', [LowAdr, HighAdr]);
      M := F.ShowModal;
      if M = mrOK then begin
        AAddress := F.SpinEdit1.Value;
      end;
      Result := M;
    finally
      F.Free;
    end;
  end;
end;

end.

