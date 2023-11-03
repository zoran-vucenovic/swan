unit UnitSpectrumColourMap;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitConfigs, Graphics, fpjson;

type
  TLCLColours = array [False..True, 0..7] of TColor;
  TLCLColourMap = record
    Colours: TLCLColours;
    Name: String;
  end;
  TLCLColourMapArray = Array of TLCLColourMap;

procedure GetDefaultSpectrumColoursC0(out Colours: TLCLColourMap);
procedure GetDefaultSpectrumColoursD7(out Colours: TLCLColourMap);
procedure GetBlackWhiteSpectrumColours(out Colours: TLCLColourMap);
procedure GetActiveColours(out Colours: TLCLColourMap);
procedure SetActiveColours(const Colours: TLCLColourMap);

procedure GetColourMaps(out LCLColourMapArray: TLCLColourMapArray);
procedure SetColourMaps(const LCLColourMapArray: TLCLColourMapArray);
function ConvertToBW(const AColour: TColor): TColor;
procedure ConvertToBW(const AColours: TLCLColours; out BWColours: TLCLColours);
function IsBW(const AColours: TLCLColourMap): Boolean;

implementation

const
  cColourMaps: RawByteString = 'colour_maps';
  cPalettes: RawByteString = 'palettes';
  cName: RawByteString = 'palette_name';
  cColours: RawByteString = 'palette_colours';
  cActiveColours: RawByteString = 'active_palette';

var
  FDefaultLCLColoursC0: TLCLColourMap;
  FDefaultLCLColoursD7: TLCLColourMap;
  FBlackWhiteSpectrumColours: TLCLColourMap;
  FActiveColours: TLCLColourMap;
  FLCLColourMapArray: TLCLColourMapArray;

procedure GetDefaultSpectrumColoursC0(out Colours: TLCLColourMap);
begin
  Colours := FDefaultLCLColoursC0;
end;

procedure GetDefaultSpectrumColoursD7(out Colours: TLCLColourMap);
begin
  Colours := FDefaultLCLColoursD7;
end;

procedure GetBlackWhiteSpectrumColours(out Colours: TLCLColourMap);
begin
  Colours := FBlackWhiteSpectrumColours;
end;

var
  Loaded: Boolean = False;

procedure SaveToConf;

  procedure SaveLCLColourMap(JS: TJSONObject; const CM: TLCLColourMap);
  var
    JAA: TJSONArray;
    B: Boolean;
    J: Integer;
  begin
    JS.Add(cName, CM.Name);
    JAA := TJSONArray.Create;
    try
      for B := False to True do
        for J := 0 to 7 do begin
          JAA.Add(Integer(CM.Colours[B, J]));
        end;

      if JS.Add(cColours, JAA) >= 0 then
        JAA := nil;
    finally
      JAA.Free;
    end;
  end;

var
  JS, JO: TJSONObject;
  JA: TJSONArray;
  I: Int32;
  B: Boolean;
begin
  if Loaded then begin
    TConfJSON.RemoveSection(cColourMaps);

    JO := TJSONObject.Create;
    try
      B := False;

      JS := TJSONObject.Create;
      try
        SaveLCLColourMap(JS, FActiveColours);
        if JO.Add(cActiveColours, JS) >= 0 then begin
          JS := nil;
          B := True;
        end;
      finally
        JS.Free;
      end;

      if Length(FLCLColourMapArray) > 0 then begin
        JA := TJSONArray.Create;
        try
          for I := 0 to High(FLCLColourMapArray) do begin
            JS := TJSONObject.Create;
            try
              SaveLCLColourMap(JS, FLCLColourMapArray[I]);

              if JA.Add(JS) >= 0 then
                JS := nil;
            finally
              JS.Free;
            end;
          end;

          if JO.Add(cPalettes, JA) >= 0 then begin
            JA := nil;
            B := True;
          end;
        finally
          JA.Free;
        end;
      end;

      if B then
        if TConfJSON.AddToConf(cColourMaps, JO) then
          JO := nil;
    finally
      JO.Free;
    end;
  end;
end;

procedure LoadFromConf;

  function LoadLCLColourMap(JS: TJSONObject; var CM: TLCLColourMap): Boolean;
  var
    K: Integer;
    JAA: TJSONArray;
    S: RawByteString;
    D: TJSONData;
  begin
    Result := False;

    if Assigned(JS) then begin
      JAA := JS.Arrays[cColours];
      if Assigned(JAA) and (JAA.Count = 16) then begin
        D := JS.Find(cName);
        if Assigned(D) and (D is TJSONString) then begin
          S := TJSONString(D).AsString;
          CM.Name := S;

          for K := 0 to JAA.Count - 1 do
            CM.Colours[K > 7, K mod 8] := TColor(JAA.Integers[K]);

          Result := True;
        end;
      end;
    end;
  end;

var
  JS, JO: TJSONObject;
  JA: TJSONArray;
  I, J: Int32;

begin
  if Loaded then
    Exit;

  Loaded := True;
  JS := nil;
  SetLength(FLCLColourMapArray, 0);

  JO := TConfJSON.GetJSONObject(cColourMaps);
  if Assigned(JO) then begin
    JA := nil;
    JA := JO.Get(cPalettes, JA);

    if Assigned(JA) then begin
      SetLength(FLCLColourMapArray, JA.Count);
      J := 0;
      for I := 0 to JA.Count - 1 do begin
        JS := JA.Objects[I];
        if LoadLCLColourMap(JS, FLCLColourMapArray[J]) then
          Inc(J);
      end;
      SetLength(FLCLColourMapArray, J);
    end;

    JS := JO.Get(cActiveColours, JS);
  end;

  if not (Assigned(JS) and LoadLCLColourMap(JS, FActiveColours)) then
    FActiveColours := FDefaultLCLColoursD7;
end;

procedure GetActiveColours(out Colours: TLCLColourMap);
begin
  LoadFromConf;

  Colours := FActiveColours;
end;

procedure SetActiveColours(const Colours: TLCLColourMap);
begin
  FActiveColours := Colours;
end;

procedure GetColourMaps(out LCLColourMapArray: TLCLColourMapArray);
var
  I: Integer;
begin
  LoadFromConf;

  SetLength(LCLColourMapArray{%H-}, Length(FLCLColourMapArray));

  for I := 0 to High(LCLColourMapArray) do
    LCLColourMapArray[I] := FLCLColourMapArray[I];
end;

procedure SetColourMaps(const LCLColourMapArray: TLCLColourMapArray);
var
  I: Integer;
begin
  SetLength(FLCLColourMapArray, Length(LCLColourMapArray));

  for I := 0 to High(LCLColourMapArray) do
    FLCLColourMapArray[I] := LCLColourMapArray[I];
end;

function ConvertToBW(const AColour: TColor): TColor;
var
  X: Integer;
  R, G, B: Integer;
begin
  X := ColorToRGB(AColour);

  R := Red(X);
  G := Green(X);
  B := Blue(X);
  //
  X := (R * 299 + G * 587 + B * 114 + 500) div 1000;
  Result := (X shl 16) or (X shl 8) or X;
end;

procedure ConvertToBW(const AColours: TLCLColours; out BWColours: TLCLColours);
var
  B: Boolean;
  I: Integer;
begin
  for B := False to True do
    for I := 0 to 7 do
      BWColours[B, I] := ConvertToBW(AColours[B, I]);
end;

function IsBW(AColours: TLCLColours): Boolean;
var
  B: Boolean;
  I: Integer;
  R: Byte;
  C: TColor;
begin
  for B := False to True do
    for I := 0 to 7 do begin
      C := AColours[B, I];
      R := Red(C);
      if (R <> Green(C)) or (R <> Blue(C)) then
        Exit(False);
    end;

  Result := True;
end;

function IsBW(const AColours: TLCLColourMap): Boolean;
begin
  Result := IsBW(AColours.Colours);
end;

procedure InitColoursFromByte(var Cl: TLCLColourMap; const ClName: String; ClrChannel: Byte);
var
  ClrChannels: LongInt;
  I: Integer;
begin
  ClrChannels := ClrChannel;
  ClrChannels := ClrChannels or (ClrChannels shl 8) or (ClrChannels shl 16);
  Cl.Name := ClName;

  Cl.Colours[True, 0] := $000000; // black
  Cl.Colours[True, 1] := $FF0000; // blue
  Cl.Colours[True, 2] := $0000FF; // red
  Cl.Colours[True, 3] := $FF00FF; // magenta
  Cl.Colours[True, 4] := $00FF00; // green
  Cl.Colours[True, 5] := $FFFF00; // cyan
  Cl.Colours[True, 6] := $00FFFF; // yellow
  Cl.Colours[True, 7] := $FFFFFF; // white

  for I := 0 to 7 do
    Cl.Colours[False, I] := Cl.Colours[True, I] and ClrChannels;
end;

procedure InitDefaultColours;
begin
  InitColoursFromByte(FDefaultLCLColoursD7, 'Default scheme (0xD7)', $D7);
  InitColoursFromByte(FDefaultLCLColoursC0, 'Darker scheme (0xC0)', $C0);
end;

procedure InitBlackWhiteColours;
var
  Cl: TLCLColourMap absolute FBlackWhiteSpectrumColours;
begin                
  Cl.Name := 'Black and white';
  ConvertToBW(FDefaultLCLColoursD7.Colours, Cl.Colours);
end;

procedure InitPredefinedColours;
begin
  InitDefaultColours;
  InitBlackWhiteColours;
end;

initialization
  InitPredefinedColours;

finalization
  SaveToConf;

end.

