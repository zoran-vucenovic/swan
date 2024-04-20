unit UnitFrameWordDisplay;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls;

type
  TFrameWordDisplay = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelBin: TPanel;
    PanelHex: TPanel;
  private
    FByteMode: Boolean;
    FValue: Word;

    procedure SetByteMode(AValue: Boolean);
    procedure SetValue(AValue: Word);
  public                                                  
    constructor Create(TheOwner: TComponent); override;
    class function MakeNew(const Title: String): TFrameWordDisplay;

    procedure SetTitle(const S: String);

    property Value: Word read FValue write SetValue;
    property ByteMode: Boolean read FByteMode write SetByteMode;
  end;

implementation

{$R *.lfm}

{ TFrameWordDisplay }

procedure TFrameWordDisplay.SetValue(AValue: Word);
begin
  if not FByteMode then begin
    FValue := AValue;

    Label3.Caption := HexStr(WordRec(FValue).Hi, 2);
    Label1.Caption := binStr(WordRec(FValue).Hi, 8);
  end else begin
    FValue := AValue and $FF;

    Label3.Caption := '';
    Label1.Caption := '';
  end;

  Label4.Caption := HexStr(WordRec(FValue).Lo, 2);
  Label2.Caption := binStr(WordRec(FValue).Lo, 8);

  Label5.Caption := IntToStr(FValue);
end;

procedure TFrameWordDisplay.SetByteMode(AValue: Boolean);
begin
  if FByteMode <> AValue then begin
    FByteMode := AValue;

    SetValue(FValue);
  end;
end;

constructor TFrameWordDisplay.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FByteMode := False;
  Label6.Constraints.MinWidth := Label6.Width;
  Label6.AutoSize := True;
end;

class function TFrameWordDisplay.MakeNew(const Title: String
  ): TFrameWordDisplay;
begin
  Result := TFrameWordDisplay.Create(nil);
  Result.SetTitle(Title);
end;

procedure TFrameWordDisplay.SetTitle(const S: String);
begin
  Label6.Caption := S;
end;

end.

