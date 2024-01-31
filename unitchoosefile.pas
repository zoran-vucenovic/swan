unit UnitChooseFile;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ButtonPanel, StdCtrls, Grids;

type
  TFormChooseFile = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure FormShow(Sender: TObject);
  private
    Grid: TCustomStringGrid;
    procedure GridOnDblClick(Sender: TObject);
    procedure AfterShow(Data: PtrInt);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    class function ShowFormChooseFile(const SL: TStrings; out N: Integer): Boolean; static;
  end;

implementation

{$R *.lfm}

type

  TFileListGrid = class(TCustomStringGrid)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TFormChooseFile }

procedure TFormChooseFile.FormShow(Sender: TObject);
begin
  AfterShow(1);
end;

procedure TFormChooseFile.GridOnDblClick(Sender: TObject);
var
  P, PMouse: TPoint;
  G: TFileListGrid;
  Rest: Integer;
begin
  PMouse := Grid.ScreenToClient(Mouse.CursorPos);
  G := Grid as TFileListGrid;
  if G.OffsetToColRow(
      False, // IsCol
      True,  // Physical, means take scroll offset into consideration.
      PMouse.Y,
      P.Y,
      Rest
    )
    and G.OffsetToColRow(
      True, // IsCol
      True, // Physical
      PMouse.X,
      P.X,
      Rest
    )
  then begin
    if P.Y >= Grid.FixedRows then begin
      Grid.Row := P.Y;
      ModalResult := mrOK;
    end;
  end;

end;

procedure TFormChooseFile.AfterShow(Data: PtrInt);
begin
  CommonFunctionsLCL.TCommonFunctionsLCL.FormToScreenCentre(Self);
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShow, Data - 1);
end;

constructor TFormChooseFile.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Label1.Caption := ' ';
  Grid := TFileListGrid.Create(nil);
  Grid.Anchors := [];
  Grid.AnchorParallel(akLeft, 0, Panel2);
  Grid.AnchorParallel(akTop, 0, Panel2);
  Grid.AnchorParallel(akRight, 0, Panel2);
  Grid.AnchorParallel(akBottom, 0, Panel2);
  Grid.Parent := Panel2;
  Grid.OnDblClick := @GridOnDblClick;

  AfterShow(-1);
end;

destructor TFormChooseFile.Destroy;
begin
  Grid.Free;
  inherited Destroy;
end;

class function TFormChooseFile.ShowFormChooseFile(const SL: TStrings; out
  N: Integer): Boolean;
const
  MessageText: AnsiString =
    'The chosen zip file contains %d file entries which might be Spectrum files'
      + '. Please choose one:'
    ;

var
  I: Integer;
  F: TFormChooseFile;

begin
  Result := False;
  N := -1;
  if (SL = nil) or (SL.Count <= 0) then
    Exit;

  if (SL.Count = 1) then begin
    N := 0;
    Exit(True);
  end;

  F := TFormChooseFile.Create(nil);
  try
    F.Label1.Caption := Format(MessageText, [SL.Count]);
    F.Grid.RowCount := F.Grid.FixedRows + SL.Count;
    for I := 0 to SL.Count - 1 do begin
      F.Grid.Cells[F.Grid.FixedCols, F.Grid.FixedRows + I] := SL.Strings[I];
    end;
    F.Grid.Row := F.Grid.FixedRows;

    if F.ShowModal <> mrOK then
      Result := True // User canceled
    else begin
      N := F.Grid.Row - F.Grid.FixedRows;
      Result := N >= 0;
    end;
  finally
    F.Free;
  end;
end;

{ TFileListGrid }

constructor TFileListGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Options := Options
    + [goSelectionActive, goRowSelect, goDrawFocusSelected]
    - [goFixedHorzLine, goFixedVertLine,
         goHorzLine, goVertLine, goRangeSelect];
  FixedCols := 0;
  FixedRows := 0;
  ColCount := 1;
  AllowOutboundEvents := False;
  AutoFillColumns := True;
  Flat := True;
end;

end.

