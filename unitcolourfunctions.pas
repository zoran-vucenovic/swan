unit UnitColourFunctions;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Math, Graphics;

type
  TColourFunctions = class sealed (TObject)
  strict private
    class procedure NormalizeHue(var Hue: Double); static;
    class procedure RaiseBadColourException(const Message: String); static; inline;
    class procedure RGBToHSL(RGB: TColor; out H, S, L: Double); static;
    class function HSLToRGB(H, S, L: Double): TColor; static;
  public
    type
      EBadColourException = class(Exception);                
  public
    class function MakeLighterColour(C: TColor; Coef: Double): TColor; static;
  end;

implementation

class procedure TColourFunctions.NormalizeHue(var Hue: Double);
begin
  // "normalize" hue into [0, 360) range:
  if Hue < 0.0 then
    Hue := 360.0 - Trunc(-Hue) mod 360 + Frac(Hue);
  //else -- no else, the above can return 360.0, when Hue was -360.0 in first place
  if Hue >= 360.0 then
    Hue := Trunc(Hue) mod 360 + Frac(Hue);
end;

class procedure TColourFunctions.RaiseBadColourException(const Message: String
  );
begin
  raise EBadColourException.Create(Message);
end;

class procedure TColourFunctions.RGBToHSL(RGB: TColor; out H, S, L: Double);
var
  R, G, B, IntH: Integer;
  R1, G1, B1: Double;
  MaxRGB, MinRGB: Double;

begin
  // No system colours, no alpha...
  if (RGB and $ff000000) <> 0 then
    RaiseBadColourException('Only RGB, please!');

  R := RGB and $ff;
  G := (RGB shr 8) and $ff;
  B := (RGB shr 16) and $ff;

  R1 := R / 255.0;
  G1 := G / 255.0;
  B1 := B / 255.0;

  if R1 > G1 then begin
    MaxRGB := R1;
    MinRGB := G1;
    IntH := 0;
  end else begin
    MaxRGB := G1;
    MinRGB := R1;
    IntH := 2;
  end;

  if B1 > MaxRGB then begin
    MaxRGB := B1;
    IntH := 4;
  end else if B1 < MinRGB then
    MinRGB := B1;

  L := (MinRGB + MaxRGB) / 2.0;

  if Math.SameValue(MinRGB, MaxRGB) then begin
    S := 0.0;
    H := NaN; // Hue has no meaning when saturation is zero.
  end else begin
    if L > 0.5 then begin
      S := (MaxRGB - MinRGB) / (2.0 - (MaxRGB + MinRGB));
    end else begin
      S := (MaxRGB - MinRGB) / (MaxRGB + MinRGB);
    end;

    case IntH of
      0:
        H := (G1 - B1) / (MaxRGB - MinRGB);
      2:
        H := 2.0 + (B1 - R1) / (MaxRGB - MinRGB);
    else
      //4:
        H := 4.0 + (R1 - G1) / (MaxRGB - MinRGB);
    end;

    H := 60.0 * H;

    // "normalize" hue into [0, 360) range:
    NormalizeHue(H);
  end;
end;

{ Hue in degrees, saturation and luminacy in [0, 1] }
class function TColourFunctions.HSLToRGB(H, S, L: Double): TColor;
var
  Tmp1, Tmp2: Double;

  procedure TestClr(var C: Double);
  begin
    if C < 60.0 then
      C := Tmp2 + (Tmp1 - Tmp2) * C / 60.0
    else if C < 180.0 then
      C := Tmp1
    else if C < 240.0 then
      C := Tmp2 + (Tmp1 - Tmp2) * (4.0 - C / 60.0)
    else
      C := Tmp2;
  end;

  procedure RaiseBadHSL; inline;
  begin
    RaiseBadColourException('Bad HSL');
  end;

  procedure CheckSL(var X: Double);
  const
    L0 = -0.5 / 255.0;
    L1 = 1.0 - L0;
  begin
    if IsNan(X) or IsInfinite(X) then
      RaiseBadHSL;

    if X < 0.0 then begin
      if X > L0 then
        X := 0.0
      else
        RaiseBadHSL;
    end else if X > 1.0 then begin
      if X < L1 then
        X := 1.0
      else
        RaiseBadHSL;
    end;

  end;

var
  R, G, B: Integer;
  R1, G1, B1: Double;
begin
  // L and S must be in [0, 1] range
  CheckSL(L);
  CheckSL(S);

  if Math.IsZero(S) then begin
    R := Round(L * 255.0);
    G := R;
    B := R;
  end else begin
    // when S <> 0, hue must be regular number
    if IsNan(H) or IsInfinite(H) then
      RaiseBadHSL;

   // normalize hue
    NormalizeHue(H);

    if L < 0.5 then
      Tmp1 := L * (1.0 + S)
    else
      Tmp1 := L + S - L * S;

    Tmp2 := 2.0 * L - Tmp1;

    R1 := H + 120.0;
    if R1 > 360.0 then
      R1 := R1 - 360.0;

    G1 := H;

    B1 := H - 120.0;
    if B1 < 0.0 then
      B1 := B1 + 360.0;

    TestClr(R1);
    TestClr(G1);
    TestClr(B1);

    R := Round(R1 * 255.0);
    G := Round(G1 * 255.0);
    B := Round(B1 * 255.0);
  end;

  Result := (B shl 16) or (G shl 8) or R;
end;

class function TColourFunctions.MakeLighterColour(C: TColor; Coef: Double
  ): TColor;
var
  H, S, L: Double;
begin
  if Coef > 0.0 then begin
    Result := Graphics.ColorToRGB(C);
    if not (Math.IsZero(Coef) or Math.IsZero(Coef - 1.0)) then begin
      RGBToHSL(Result, H, S, L);
      if Coef + L < 1.0 then
        Result := clBlack
      else begin
        L := (Coef + L - 1.0) / Coef; // Lighter colour
        Result := HSLToRGB(H, S, L);
      end;

    end;
  end else
    Result := clBlack;
end;

end.

