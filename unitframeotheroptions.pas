unit UnitFrameOtherOptions;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls;

type
  TFrameOtherOptions = class(TFrame)
    Panel1: TPanel;
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TFrameOtherOptions }

constructor TFrameOtherOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Other options';
end;

end.

