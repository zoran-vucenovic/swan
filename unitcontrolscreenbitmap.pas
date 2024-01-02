unit UnitControlScreenBitmap;
// Copyright 2022-2024 Zoran Vučenović
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

  TControlScreenBitmap = class(TGraphicControl)
  strict private
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
    property Timer: TFlashTimer write FTimer;
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
  ScreenPixel, Spx: PBGRAPixel;
  CInk, CPaper: TBGRAPixel;
  By, BAttr: Byte;
  HasFlashAttr, Bright, LastPass: Boolean;
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

      if HasFlashAttr and (BmpFlash = nil) then begin
        BmpFlash := TBGRABitmap.Create(ClientWidth, ClientHeight);
        for J := 0 to Y * 8 - 1 do
          Move(SpectrumColoursBGRA.Bmp.ScanLine[J]^, BmpFlash.ScanLine[J]^, 8 * 32 * SizeOf(TBGRAPixel));
        if X > 0 then
          for J := Y * 8 to Y * 8 + 7 do
            Move(SpectrumColoursBGRA.Bmp.ScanLine[J]^, BmpFlash.ScanLine[J]^, 8 * SizeOf(TBGRAPixel) * X);
      end;

      CPaper := SpectrumColoursBGRA.BGRAColours[Bright, (BAttr shr 3) and %111];
      CInk := SpectrumColoursBGRA.BGRAColours[Bright, BAttr and %111];

      N := X shl 3;
      for J := 0 to 7 do begin
        M := (Y shl 3) or J;
        ScreenPixel := SpectrumColoursBGRA.Bmp.ScanLine[M] + N;
        By := (PScreenStart + W)^;
                              
        Spx := ScreenPixel;
        LastPass := BmpFlash = nil;
        repeat
          I := 7;
          repeat
            if By and (1 shl I) <> 0 then begin
              Spx^ := CInk;
            end else begin
              Spx^ := CPaper;
            end;
            if I = 0 then
              Break;
            Inc(Spx);
            Dec(I);
          until False;

          if not HasFlashAttr then begin
            if Assigned(BmpFlash) then
              Move(ScreenPixel^, (BmpFlash.ScanLine[M] + N)^, 8 * SizeOf(TBGRAPixel));
            Break;
          end;

          if LastPass then
            Break;

          LastPass := True;
          Spx := BmpFlash.ScanLine[M] + N;
          By := not By;
        until False;
        Inc(WRAdr.Hi);
      end;
    end;
  end;

  SpectrumColoursBGRA.Bmp.InvalidateBitmap;
  if Assigned(BmpFlash) then
    BmpFlash.InvalidateBitmap;

end;

constructor TControlScreenBitmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTimer := nil;
  BmpFlash := nil;
  PScreenStart := nil;
  Self.ClientWidth := 32 * 8;
  Self.ClientHeight := 24 * 8;
end;

constructor TControlScreenBitmap.CreateNewControl(AOwner: TComponent;
  const ABGRAColours: TBGRAColours; const APScreenStart: PByte);
begin
  Create(AOwner);
  SpectrumColoursBGRA.BGRAColours := ABGRAColours;
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
      DrawToBgraBitmap();
    end;
  end;

  if Assigned(BmpFlash) and Assigned(FTimer) and FTimer.FlashState then
    Bmp := BmpFlash
  else
    Bmp := SpectrumColoursBGRA.Bmp;

  Bmp.Draw(Canvas, 0, 0, True);
end;

end.

