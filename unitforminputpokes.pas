unit UnitFormInputPokes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, Forms, Controls, Graphics, Dialogs,
  Grids, LCLType, ExtCtrls, Buttons;

type
  TPokeEntry = record
    Adr: Word;
    Value: Byte;
  end;

  TPokesArray = Array of TPokeEntry;

  TFormInputPokes = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    type
      TGridNums = class(TStringGrid)
      protected
        function EditorCanAcceptKey(const Ch: TUTF8Char): Boolean; override;
        function ValidateEntry(const ACol, ARow: Integer;
          const OldValue: string; var NewValue: string): Boolean; override;
      public
        MinAddress: Word;
        MaxAddress: Word;

        constructor Create(AOwner: TComponent); override;
        function CheckValue(const ACol: Integer; const AValue: String; out N: Int32
          ): Boolean;
        procedure DefaultDrawCell(aCol, aRow: Integer; var aRect: TRect;
          aState: TGridDrawState); override;
        procedure EditingDone; override;

      end;
  strict private
    FPokes: TPokesArray;
    GridNums: TGridNums;
    procedure GridOnButtonClick(Sender: TObject; aCol, aRow: Integer);
    procedure AfterShow(Data: PtrInt);
  public
    class function ShowInputPokesDialog(const AMinAddress, AMaxAddress: Word; out APokes: TPokesArray): Boolean;
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

{ TFormInputPokes.TGridNums }

function TFormInputPokes.TGridNums.EditorCanAcceptKey(const Ch: TUTF8Char
  ): Boolean;
begin
  if Length(Ch) = 1 then
    case Ch[1] of
      '0'..'9', AnsiChar(VK_BACK):
        Exit(inherited EditorCanAcceptKey(Ch));
    otherwise
    end;

  Result := False;
end;

function TFormInputPokes.TGridNums.ValidateEntry(const ACol, ARow: Integer;
  const OldValue: string; var NewValue: string): Boolean;
var
  N: Int32;
begin
  Result := inherited ValidateEntry(ACol, ARow, OldValue, NewValue)
    and CheckValue(ACol, Trim(NewValue), N);

  if not Result then
    EditordoResetValue;
end;

function TFormInputPokes.TGridNums.CheckValue(const ACol: Integer;
  const AValue: String; out N: Int32): Boolean;
var
  MinVal, MaxVal: Integer;
begin
  if AValue = '' then
    Exit(True);

  if ACol = FixedCols then begin
    MinVal := MinAddress;
    MaxVal := MaxAddress;
  end else begin
    MinVal := Byte.MinValue;
    MaxVal := Byte.MaxValue;
  end;

  Result := TryStrToIntDecimal(AValue, N)
    and (N >= MinVal) and (N <= MaxVal);

end;

procedure TFormInputPokes.TGridNums.DefaultDrawCell(aCol, aRow: Integer;
  var aRect: TRect; aState: TGridDrawState);
var
  TS: TTextStyle;
begin
  if (ACol = FixedCols + 2) and (aRow >= FixedRows) then begin
    inherited DefaultDrawCell(aCol, aRow, aRect, aState);

    TS := Canvas.TextStyle;
    TS.Layout := TTextLayout.tlCenter;
    TS.Alignment := TAlignment.taCenter;
    TS.Opaque := False;
    TS.Wordbreak := False;
    Canvas.TextRect(ARect, ARect.Left, ARect.Top, 'delete', TS);
  end else begin
    if aCol < FixedCols then begin
      TS := Canvas.TextStyle;
      TS.Alignment := TAlignment.taRightJustify;
      Canvas.TextStyle := TS;
    end;

    inherited DefaultDrawCell(aCol, aRow, aRect, aState);
  end;
end;

procedure TFormInputPokes.TGridNums.EditingDone;
begin
  inherited EditingDone;

  if Row = RowCount - 1 then
    if not IsEmptyRow(Row) then
      RowCount := RowCount + 1;
end;

constructor TFormInputPokes.TGridNums.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ValidateOnSetSelection := True;
  Options := Options + [goAlwaysShowEditor, goAutoAddRows, goColSizing, goEditing, goTabs, goFixedRowNumbering];
  FixedCols := 1;
  FixedRows := 1;
  Flat := True;

  TitleStyle := tsNative;

  RowCount := FixedRows + 1;
end;

{ TFormInputPokes }

procedure TFormInputPokes.FormCreate(Sender: TObject);
begin
  GridNums := TGridNums.Create(Panel2);
  GridNums.AnchorParallel(akTop, 0, Panel2);
  GridNums.AnchorParallel(akLeft, 0, Panel2);
  GridNums.AnchorParallel(akRight, 0, Panel2);
  GridNums.AnchorParallel(akBottom, 0, Panel2);
  GridNums.Parent := Panel2;
  GridNums.OnButtonClick := @GridOnButtonClick;

  AfterShow(-1);
end;

procedure TFormInputPokes.FormShow(Sender: TObject);
begin
  AfterShow(1);
end;

procedure TFormInputPokes.GridOnButtonClick(Sender: TObject; aCol,
  aRow: Integer);
begin
  if (aCol = GridNums.FixedCols + 2) and (aRow >= GridNums.FixedRows) then begin
    if aRow < GridNums.RowCount - 1 then
      GridNums.DeleteRow(aRow)
    else begin
      GridNums.Cells[GridNums.FixedCols, aRow] := '';
      GridNums.Cells[GridNums.FixedCols + 1, aRow] := '';
    end;
  end;
end;

procedure TFormInputPokes.AfterShow(Data: PtrInt);
var
  I, N: Integer;
begin
  if BitBtn2.Width > BitBtn1.Width then
    N := BitBtn2.Width
  else
    N := BitBtn1.Width;
  BitBtn1.Constraints.MinWidth := N;
  BitBtn2.Constraints.MinWidth := N;
  CommonFunctionsLCL.TCommonFunctionsLCL.AdjustFormPos(Self);
  if Data >= 0 then begin
    for I := GridNums.FixedCols to GridNums.FixedCols + 1 do
      GridNums.AutoSizeColumn(I);
    if Data > 0 then
      Application.QueueAsyncCall(@AfterShow, Data - 1)
    else
      if GridNums.CanSetFocus then
        GridNums.SetFocus;
  end;
end;

procedure TFormInputPokes.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );

  procedure SelectCellWithError(const ACol, ARow: Integer; const Msg: String);
  begin
    CanClose := False;
    GridNums.Col := ACol;
    GridNums.Row := ARow;
    MessageDlg(Msg, mtError, [mbClose], 0);
    if GridNums.CanSetFocus then
      GridNums.SetFocus;
  end;

var
  I, J, JJ: Integer;
  N: Int32;
  Skip: Boolean;
  PokeEntry: TPokeEntry;
  S1, S2: String;
  ColAdr, ColVal: Integer;
  UN: UInt32 absolute N;

begin
  if ModalResult = mrOK then begin
    SetLength(FPokes, GridNums.RowCount - GridNums.FixedRows);
  //
    ColAdr := GridNums.FixedCols;
    ColVal := ColAdr + 1;
    J := 0;
    for I := GridNums.FixedRows to GridNums.RowCount - 1 do begin
      Skip := False;
      S1 := Trim(GridNums.Cells[ColAdr, I]);
      S2 := Trim(GridNums.Cells[ColVal, I]);
      if S1 = '' then begin
        if S2 = '' then
          Skip := True
        else begin
          SelectCellWithError(ColAdr, I, 'You have to enter address for the poke.');
          Break;
        end;

      end else
        if S2 = '' then begin
          SelectCellWithError(ColVal, I, 'You have to enter value.');
          Break;
        end;

      if not Skip then begin
        if not GridNums.CheckValue(ColAdr, S1, N) then begin
          SelectCellWithError(ColAdr, I, 'Invalid address.');
          Break;
        end;
        PokeEntry.Adr := N;
        if not GridNums.CheckValue(ColVal, S2, N) then begin
          SelectCellWithError(ColVal, I, 'Invalid Value');
          Break;
        end;
        PokeEntry.Value := UN;

        for JJ := 0 to J - 1 do begin
          if FPokes[JJ].Adr = PokeEntry.Adr then begin
            SelectCellWithError(ColAdr, I, Format(
              'Multiple pokes to address %d entered.%sYou cannot enter multiple pokes to same memory address.',
              [PokeEntry.Adr, LineEnding]));
            Break;
          end;
        end;
        if not CanClose then
          Break;

        FPokes[J] := PokeEntry;
        Inc(J);
      end;

    end;
    SetLength(FPokes, J);

  end else
    SetLength(FPokes, 0);
end;

class function TFormInputPokes.ShowInputPokesDialog(const AMinAddress,
  AMaxAddress: Word; out APokes: TPokesArray): Boolean;
var
  F: TFormInputPokes;
  C: TGridColumn;

begin
  Result := False;
  F := TFormInputPokes.Create(nil);
  try
    F.GridNums.MinAddress := AMinAddress;
    F.GridNums.MaxAddress := AMaxAddress;

    C := F.GridNums.Columns.Add();
    C.Title.Caption := Format('address (%d-%d)', [AMinAddress, AMaxAddress]);

    C := F.GridNums.Columns.Add();
    C.Title.Caption := Format('value (%d-%d)', [Byte.MinValue, Byte.MaxValue]);

    C := F.GridNums.Columns.Add();
    C.ButtonStyle := cbsButtonColumn;
    C.Title.Caption := ' ';

    if (F.ShowModal = mrOK) and (Length(F.FPokes) > 0) then begin
      APokes := F.FPokes;
      Result := True;
    end else
      APokes := nil;

  finally
    F.Free;
  end;
end;

end.

