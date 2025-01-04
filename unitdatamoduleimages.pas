unit UnitDataModuleImages;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Controls, ImgList, Graphics;

type
  TDataModuleImages = class(TDataModule)
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList3: TImageList;
  private
  public                                                                               
    function DrawCheckForImage(AIndex: Integer): Integer;
  end;

var
  DataModuleImages: TDataModuleImages;

implementation

{$R *.lfm}

{ TDataModuleImages }

function TDataModuleImages.DrawCheckForImage(AIndex: Integer): Integer;
var
  I: Integer;
  L: Integer;
  N: Integer;
  Ba: array of TCustomBitmap;
  B: TBitmap;
  R: TRect;

begin
  Result := -1;

  L := ImageList1.Count;
  if (AIndex < L) then begin
    N := ImageList1.ResolutionCount;
    if N > 0 then begin
      SetLength(Ba, N);
      try
        for I := 0 to N - 1 do begin
          B := TBitmap.Create;
          ImageList1.ResolutionByIndex[I].GetBitmap(AIndex, B);
          R := Rect(B.Width div 2 - 2, B.Height div 2 - 2, B.Width, B.Height);

          ImageList3.Resolution[R.Width].StretchDraw(B.Canvas, 0, R);
          Ba[I] := B;
        end;
        Result := ImageList1.AddMultipleResolutions(Ba);
      finally
        for I := Low(Ba) to High(Ba) do
          Ba[I].Free;
      end;
    end;
  end;
end;

initialization
  DataModuleImages := TDataModuleImages.Create(nil);

finalization
  DataModuleImages.Free;

end.

