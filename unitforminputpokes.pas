unit UnitFormInputPokes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  ButtonPanel;

type
  TPokeEntry = record
    Adr: Word;
    Value: Byte;
  end;

  TPokesArray = Array of TPokeEntry;

  TFormInputPokes = class(TForm)
    ButtonPanel1: TButtonPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    StringGrid1: TStringGrid;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1ValidateEntry(Sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    LowBoundary: Word;
    HighBoundary: Word;
    function ShowPokesDialog(out APokes: TPokesArray): Boolean;
  public
    class function ShowInputPokesDialog(const ALowBoundary, AHighBoundary: Word; out APokes: TPokesArray): Boolean;
  end;

implementation

function TryStrToIntDecimal(const S: String; out N: Int32): Boolean;
var
  P: PAnsiChar;
begin
  P := PAnsiChar(TrimLeft(S));
  if P^ = '-' then
    Inc(P);
  case P^ of
    '0':
      if not ((P + 1)^ in [#0, '0'..'9']) then
        Exit(False);
    '1'..'9':
      ;
  otherwise
    Exit(False);
  end;

  Result := TryStrToInt(S, N);
end;

{$R *.lfm}

{ TFormInputPokes }

procedure TFormInputPokes.StringGrid1ValidateEntry(Sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);

  procedure ShowErrorMessage(const S: String);
  begin
    MessageDlg(S, TMsgDlgType.mtError, [mbClose], 0);
    //NewValue := '';
    NewValue := OldValue;
  end;

var
  N: Integer;
  Value: Byte;
  ValAsInt8: Int8 absolute Value;
begin
  if Trim(NewValue) = '' then begin
    NewValue := OldValue;
    Exit;
  end;

  if not TryStrToIntDecimal(NewValue, N) then begin
    ShowErrorMessage('Not a number!');
    Exit;
  end;

  if aCol = StringGrid1.FixedCols then begin
    if N < 0 then begin
      ShowErrorMessage('Negative number not allowed!');
      Exit;
    end;

    if N < LowBoundary then begin
      ShowErrorMessage('Writing to ROM not allowed!');
      Exit;
    end;
      
    if N > HighBoundary then begin
      ShowErrorMessage('Address must not be greater than ' + HighBoundary.ToString);
      //Exit;
    end;

  end else begin
    if N > Byte.MaxValue then begin
      ShowErrorMessage('Value must not be greater than ' + Byte.MaxValue.ToString);
      Exit;
    end;

    if N < 0 then begin
      if N < Int8.MinValue then begin
        ShowErrorMessage('Value must be greater than ' + Int8.MinValue.ToString);
        Exit;
      end;

      ValAsInt8 := N;
      NewValue := Value.ToString;
    end;

  end;

end;

procedure TFormInputPokes.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  //
end;

procedure TFormInputPokes.FormCreate(Sender: TObject);
begin
  StringGrid1.RowCount := StringGrid1.FixedRows + 1;
end;

function TFormInputPokes.ShowPokesDialog(out APokes: TPokesArray): Boolean;
var
  I: Integer;
  N: Integer;
  Poke: TPokeEntry;
  Value: Byte;
  AuxInt8: Int8 absolute Value;
begin
  Result := False;
  if ShowModal = mrOK then begin
    SetLength(APokes, StringGrid1.RowCount - StringGrid1.FixedRows);
    for I := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do begin
      if not TryStrToIntDecimal(StringGrid1.Cells[StringGrid1.FixedCols, I], N) then begin
        Exit;
      end;
      if (N < LowBoundary) or (N > HighBoundary) then
        Exit;
      Poke.Adr := N;
      if not TryStrToIntDecimal(StringGrid1.Cells[StringGrid1.FixedCols + 1, I], N) then begin
        Exit;
      end;
      if N > Byte.MaxValue then
        Exit;
      if N < 0 then begin
        if N < Int8.MinValue then
          Exit;
        AuxInt8 := N;
        N := Value;
      end;

    end;

    Result := True;
  end;
end;

class function TFormInputPokes.ShowInputPokesDialog(const ALowBoundary,
  AHighBoundary: Word; out APokes: TPokesArray): Boolean;
var
  F: TFormInputPokes;
begin
  Result := False;
  F := TFormInputPokes.Create(nil);
  try
    F.LowBoundary := ALowBoundary;
    F.HighBoundary := AHighBoundary;
    //
    Result := F.ShowPokesDialog(APokes);
  finally
    F.Free;
  end;
end;

end.

