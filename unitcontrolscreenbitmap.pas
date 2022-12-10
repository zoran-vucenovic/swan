unit UnitControlScreenBitmap;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, BGRABitmap, Controls, Graphics, ExtCtrls,
  UnitSpectrumColoursBGRA;

type
  TFlashTimer = class(TTimer)
  public
    FlashState: Boolean;
    constructor Create(AOwner: TComponent); override;
  end;

  TControlScreenBitmap = class(TCustomControl)
  private
    SpectrumColoursBGRA: TSpectrumColoursBGRA;
    PScreenStart: PByte;
    BmpFlash: TBGRABitmap;
    FTimer: TFlashTimer;

    procedure DrawToBgraBitmap;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNewControl(AOwner: TComponent;
      const ABGRAColours: TBGRAColours; const APScreenStart: PByte);
    destructor Destroy; override;
    procedure Paint; override;
    property Timer: TFlashTimer read FTimer write FTimer;
  end;

implementation

{ TFlashTimer }

constructor TFlashTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Enabled := False;
  Interval := 320;
  FlashState := False;
end;

{ TControlScreenBitmap }

procedure TControlScreenBitmap.DrawToBgraBitmap;
var
  M, N, I, J, X, Y: Integer;
  ScreenPixel, ScreenPixelF: PBGRAPixel;
  CInk, CPaper: TBGRAPixel;
  By, BAttr: Byte;
  HasFlashAttr, Bright: Boolean;
  W: Word;
  WRAdr: WordRec absolute W;
begin
  for Y := 0 to 23 do begin
    for X := 0 to 31 do begin
      WRAdr.Lo := ((Y shl 5) or X) and $FF;
      WRAdr.Hi := (Y shr 3) or 24;
      BAttr := (PScreenStart + W)^;
      WRAdr.Hi := Y and 24;
      Bright := BAttr and %01000000 <> 0;
      HasFlashAttr := BAttr and %10000000 <> 0;

      CPaper := SpectrumColoursBGRA.BGRAColours[Bright, (BAttr shr 3) and %111];
      CInk := SpectrumColoursBGRA.BGRAColours[Bright, BAttr and %111];

      N := X shl 3;
      for J := 0 to 7 do begin
        M := (Y shl 3) or J;
        ScreenPixel := SpectrumColoursBGRA.Bmp.ScanLine[M] + N;
        ScreenPixelF := BmpFlash.ScanLine[M] + N;
        By := (PScreenStart + W)^;

        if HasFlashAttr then begin
          for I := 7 downto 0 do begin
            if By and (1 shl I) <> 0 then begin
              ScreenPixel^ := CInk;
              ScreenPixelF^ := CPaper;
            end else begin
              ScreenPixel^ := CPaper;
              ScreenPixelF^ := CInk;
            end;
            Inc(ScreenPixel);
            Inc(ScreenPixelF);
          end;
        end else begin
          for I := 7 downto 0 do begin
            if By and (1 shl I) <> 0 then
              ScreenPixel^ := CInk
            else
              ScreenPixel^ := CPaper;
            ScreenPixelF^ := ScreenPixel^;
            Inc(ScreenPixel);
            Inc(ScreenPixelF);
          end;
        end;
        Inc(WRAdr.Hi);
      end;
    end;
  end;

  SpectrumColoursBGRA.Bmp.InvalidateBitmap;
end;

constructor TControlScreenBitmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTimer := nil;
  PScreenStart := nil;
  Self.ClientWidth := 32 * 8; // UnitSpectrum.CentralScreenWidth
  Self.ClientHeight := 24 * 8; // UnitSpectrum.CentralScreenHeight
end;

constructor TControlScreenBitmap.CreateNewControl(AOwner: TComponent;
  const ABGRAColours: TBGRAColours; const APScreenStart: PByte);
begin
  Create(AOwner);
  SpectrumColoursBGRA.BGRAColours := ABGRAColours;
  BmpFlash := TBGRABitmap.Create(0, 0);
  PScreenStart := APScreenStart;
end;

destructor TControlScreenBitmap.Destroy;
begin
  BmpFlash.Free;
  inherited Destroy;
end;

procedure TControlScreenBitmap.Paint;
var
  Bmp: TBGRABitmap;
begin
  inherited Paint;

  if SpectrumColoursBGRA.Bmp.Width = 0 then begin
    if Assigned(PScreenStart) then begin
      SpectrumColoursBGRA.Bmp.SetSize(ClientWidth, ClientHeight);
      BmpFlash.SetSize(ClientWidth, ClientHeight);
      DrawToBgraBitmap();
    end;
  end;

  if Assigned(FTimer) and FTimer.FlashState then
    Bmp := BmpFlash
  else
    Bmp := SpectrumColoursBGRA.Bmp;

  Bmp.Draw(Canvas, 0, 0, True);
end;

end.

