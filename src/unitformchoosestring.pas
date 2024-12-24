unit UnitFormChooseString;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, CommonFunctionsLCL, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ButtonPanel, StdCtrls, Grids;

type
  TFormChooseString = class(TForm)
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
    class function ShowFormChooseString(const SA: array of String; const ACaption,
      ATitle: AnsiString; out N: Integer): Boolean; static;
  end;

implementation

{$R *.lfm}

type

  TGridChooseString = class(TCustomStringGrid)
  protected
    function MouseButtonAllowed(Button: TMouseButton): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TFormChooseString }

procedure TFormChooseString.FormShow(Sender: TObject);
begin
  AfterShow(1);
end;

procedure TFormChooseString.GridOnDblClick(Sender: TObject);
var
  P: TPoint;
begin
  if TCommonFunctionsLCL.GridMouseToCellRegular(Grid, Grid.ScreenToClient(Mouse.CursorPos), P) then begin
    if P.Y >= Grid.FixedRows then begin
      Grid.Row := P.Y;
      ModalResult := mrOK;
    end;
  end;

end;

procedure TFormChooseString.AfterShow(Data: PtrInt);
begin
  CommonFunctionsLCL.TCommonFunctionsLCL.FormToScreenCentre(Self);
  if Data > 0 then
    Application.QueueAsyncCall(@AfterShow, Data - 1);
end;

constructor TFormChooseString.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Label1.Caption := ' ';
  Grid := TGridChooseString.Create(nil);
  Grid.Anchors := [];
  Grid.AnchorParallel(akLeft, 0, Panel2);
  Grid.AnchorParallel(akTop, 0, Panel2);
  Grid.AnchorParallel(akRight, 0, Panel2);
  Grid.AnchorParallel(akBottom, 0, Panel2);
  Grid.Parent := Panel2;
  Grid.OnDblClick := @GridOnDblClick;

  AfterShow(-1);
end;

destructor TFormChooseString.Destroy;
begin
  Grid.Free;
  inherited Destroy;
end;

class function TFormChooseString.ShowFormChooseString(
  const SA: array of String; const ACaption, ATitle: AnsiString; out N: Integer
  ): Boolean;
var
  I: Integer;
  F: TFormChooseString;

begin
  Result := False;
  N := -1;

  if Length(SA) > 0 then begin
    if Length(SA) = 1 then begin
      N := 0;
      Exit(True);
    end;

    F := TFormChooseString.Create(nil);
    try
      F.Caption := ACaption;

      F.Label1.Caption :=  Trim(ATitle);
      if F.Label1.Caption = '' then begin
        F.Panel3.Hide;
        F.Panel2.AnchorParallel(akTop, 0, F.Panel1);
      end else begin
        F.Panel3.Show;
      end;
      F.Grid.RowCount := F.Grid.FixedRows + Length(SA);
      for I := Low(SA) to High(SA) do begin
        F.Grid.Cells[F.Grid.FixedCols, F.Grid.FixedRows + I] := SA[I];
      end;
      F.Grid.Row := F.Grid.FixedRows;

      if F.ShowModal <> mrOK then
        Result := True // user cancelled
      else begin
        N := F.Grid.Row - F.Grid.FixedRows;
        Result := N >= 0;
      end;
    finally
      F.Free;
    end;

  end;
end;

{ TFileListGrid }

function TGridChooseString.MouseButtonAllowed(Button: TMouseButton): boolean;
begin
  Result := TCommonFunctionsLCL.GridMouseButtonAllowed(Self, Button);
end;

constructor TGridChooseString.Create(AOwner: TComponent);
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

