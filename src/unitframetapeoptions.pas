unit UnitFrameTapeOptions;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitOptions, UnitCommon, Forms, Controls, StdCtrls,
  ExtCtrls;

type

  TFrameTapeOptions = class(TFrame)
    CheckBoxFastLoad: TCheckBox;
    CheckBoxAutoShowTapePlayerOnLoadTape: TCheckBox;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    function GetAutoShowTapePlayerOnLoadTape: Boolean;
    function GetFastLoad: Boolean;
    procedure SetAutoShowTapePlayerOnLoadTape(const AValue: Boolean);
    procedure SetFastLoad(const AValue: Boolean);

  public
    constructor Create(TheOwner: TComponent); override;

    class function CreateForAllOptions(AOptionsDialog: TFormOptions): TFrameTapeOptions;

    property AutoShowTapePlayerOnLoadTape: Boolean
      read GetAutoShowTapePlayerOnLoadTape write SetAutoShowTapePlayerOnLoadTape;
    property FastLoad: Boolean read GetFastLoad write SetFastLoad;
  end;

implementation

{$R *.lfm}

{ TFrameTapeOptions }

function TFrameTapeOptions.GetAutoShowTapePlayerOnLoadTape: Boolean;
begin
  Result := CheckBoxAutoShowTapePlayerOnLoadTape.Checked;
end;

function TFrameTapeOptions.GetFastLoad: Boolean;
begin
  Result := CheckBoxFastLoad.Checked;
end;

procedure TFrameTapeOptions.SetAutoShowTapePlayerOnLoadTape(const AValue: Boolean);
begin
  CheckBoxAutoShowTapePlayerOnLoadTape.Checked := AValue;
end;

procedure TFrameTapeOptions.SetFastLoad(const AValue: Boolean);
begin
  CheckBoxFastLoad.Checked := AValue;
end;

constructor TFrameTapeOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Name := TCommonFunctions.GlobalObjectNameGenerator(Self);

  Caption := 'Tape options';
  Panel1.BevelOuter := bvNone;
  Panel2.BevelOuter := bvNone;
end;

class function TFrameTapeOptions.CreateForAllOptions(AOptionsDialog: TFormOptions
  ): TFrameTapeOptions;
var
  Fm: TFrameTapeOptions;
begin
  Result := nil;
  Fm := TFrameTapeOptions.Create(AOptionsDialog);
  try
    AOptionsDialog.AddAnOptionControl(Fm, 'Tape');

    Result := Fm;
  except
    Fm.Free;
  end;
end;

end.

