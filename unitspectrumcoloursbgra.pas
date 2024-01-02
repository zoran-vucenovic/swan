unit UnitSpectrumColoursBGRA;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes;

type
  TBGRAColours = array [False..True, 0..7] of TBGRAPixel;

  TSpectrumColoursBGRA = record
  private
    class operator Initialize(var X: TSpectrumColoursBGRA);
    class operator Finalize(var X: TSpectrumColoursBGRA);
  public
    Bmp: TBGRABitmap;
    BorderColour2: TBGRAPixel;
    BGRAColours: TBGRAColours;
  end;

implementation

{ TSpectrumColoursBGRA }

class operator TSpectrumColoursBGRA.Initialize(var X: TSpectrumColoursBGRA);
begin
  X.Bmp := TBGRABitmap.Create(0, 0);
end;

class operator TSpectrumColoursBGRA.Finalize(var X: TSpectrumColoursBGRA);
begin
  X.Bmp.Free;
end;

end.

