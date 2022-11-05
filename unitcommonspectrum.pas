unit UnitCommonSpectrum;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  SysUtils, UnitCommon;

type
  TCommonSpectrum = class sealed (TObject)
  strict private
    const
      FAuthorName: String = 'Zoran Vučenović';
  private
    class procedure Init;
    class procedure Final;
  public
    class procedure SortSpectrumKeys(var AKeys: Array of Word); static;

    class property AuthorName: String read FAuthorName;
  end;

implementation

type
  TSpectrumKeysSorter = class(specialize TNumArraySorter<Word>)
  public
    constructor Create; override;
    class function CompareKeys(X, Y: Word): Integer; static;
  end;

var
  SpectrumKeysSorter: TSpectrumKeysSorter;

{ TCommonSpectrum }

class procedure TCommonSpectrum.SortSpectrumKeys(var AKeys: array of Word);
begin
  if SpectrumKeysSorter = nil then
    SpectrumKeysSorter := TSpectrumKeysSorter.Create();

  SpectrumKeysSorter.SortArray(AKeys);
end;

{ TSpectrumKeysSorter }

constructor TSpectrumKeysSorter.Create;
begin
  inherited Create(@CompareKeys);
end;

class function TSpectrumKeysSorter.CompareKeys(X, Y: Word): Integer;

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


class procedure TCommonSpectrum.Init;
begin
  SpectrumKeysSorter := nil;
end;

class procedure TCommonSpectrum.Final;
begin
  SpectrumKeysSorter.Free;
end;


initialization
  TCommonSpectrum.Init;

finalization
  TCommonSpectrum.Final;

end.

