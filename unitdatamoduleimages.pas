unit UnitDataModuleImages;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Controls, ImgList;

type
  TDataModuleImages = class(TDataModule)
    ImageList1: TImageList;
    ImageList2: TImageList;
  private
  public
  end;

var
  DataModuleImages: TDataModuleImages;

implementation

{$R *.lfm}

initialization
  DataModuleImages := TDataModuleImages.Create(nil);

finalization
  DataModuleImages.Free;

end.

