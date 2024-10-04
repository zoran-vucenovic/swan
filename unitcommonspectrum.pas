unit UnitCommonSpectrum;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  SysUtils, UnitCommon;

type
  TCommonSpectrum = class sealed (TObject)
  public
    const
      AuthorName: String = 'Zoran Vučenović';
  strict private
    class var
      FBuildYear: Word;
      FBuildMonth: Word;
      FBuildDay: Word;
      FDummyObj: TObject;
  private
    class function GetBuildDateString: String; static;
    class function GetDummyObj: TObject; static;
    class procedure Init;
    class procedure Final;
  public
    const
      KiloByte = 1024;
      KB16 = KiloByte * 16;
      KB48 = KiloByte * 48;
      KB64 = KiloByte * 64;
      SpectrumColourNames: Array of String = ('black', 'blue', 'red', 'magenta', 'green', 'cyan', 'yellow', 'white');
  public
    class procedure SortSpectrumKeys(var AKeys: Array of Word); static;
    class function CompareSpectrumKeyValues(X, Y: Word): Integer; static;

    class procedure SetBuildDate(const AYear, AMonth, ADay: Word); static;
    class property BuildDateString: String read GetBuildDateString;
    class property DummyObj: TObject read GetDummyObj;
  end;

implementation

type
  TSpectrumKeysSorter = class(specialize TNumArraySorter<Word>)
  end;

var
  SpectrumKeysSorter: TSpectrumKeysSorter;

{ TCommonSpectrum }

class procedure TCommonSpectrum.SortSpectrumKeys(var AKeys: array of Word);
begin
  if SpectrumKeysSorter = nil then
    SpectrumKeysSorter := TSpectrumKeysSorter.Create(@CompareSpectrumKeyValues);

  SpectrumKeysSorter.SortArray(AKeys);
end;

class function TCommonSpectrum.CompareSpectrumKeyValues(X, Y: Word): Integer;

  function GetPositionRowNum(B: Byte): Byte; inline;
  begin
    if B > 3 then
      Result := (B - 4) * 2 + 1
    else
      Result := (3 - B) * 2;
  end;

var
  WRx: WordRec absolute X;
  WRy: WordRec absolute Y;
begin
  X := X and $07FF;
  Y := Y and $07FF;
  if X = Y then
    Exit(0);

  // Caps shift before all others
  if X = 0 then
    Exit(-1);
  if Y = 0 then
    Exit(1);

  // Then symbol shift
  if X = $0701 then
    Exit(-1);
  if Y = $0701 then
    Exit(1);

  // Then top to bottom, left to right
  if WRx.Hi = WRy.Hi then begin
    if WRx.Lo < WRy.Lo then
      Result := -1
    else
      Result := 1;
    if WRx.Hi > 3 then
      Result := -Result;
  end else begin
    if GetPositionRowNum(WRx.Hi) < GetPositionRowNum(WRy.Hi) then
      Result := -1
    else
      Result := 1;
  end;
end;

class procedure TCommonSpectrum.SetBuildDate(const AYear, AMonth, ADay: Word);
begin
  FBuildYear := AYear;
  FBuildMonth := AMonth;
  FBuildDay := ADay;
end;

class function TCommonSpectrum.GetBuildDateString: String;
begin
  Result :=
  // We might display build date in localized form:
  //DateToStr(EncodeDate(FBuildYear, FBuildMonth, FBuildDay))
  // or rather always use standard YYYY-MM-DD form:
    Format('%.4d-%.2d-%.2d', [FBuildYear, FBuildMonth, FBuildDay]);
end;

class function TCommonSpectrum.GetDummyObj: TObject;
begin
  if FDummyObj = nil then
    FDummyObj := TObject.Create;
  Result := FDummyObj;
end;

class procedure TCommonSpectrum.Init;
begin
  FDummyObj := nil;
  SpectrumKeysSorter := nil;
end;

class procedure TCommonSpectrum.Final;
begin
  SpectrumKeysSorter.Free;
  FDummyObj.Free;
end;


initialization
  TCommonSpectrum.Init;

finalization
  TCommonSpectrum.Final;

end.

