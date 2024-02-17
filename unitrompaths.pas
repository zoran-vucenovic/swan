unit UnitRomPaths;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, fpjson;

type
  TRomPaths = class(TObject)
  strict private
    class var
      FRomPaths: TRomPaths;

  strict private
    const
      c_rom_path_1 = 'rom_path_48_1';
      c_rom_path_2_1 = 'rom_path_128_1';
      c_rom_path_2_2 = 'rom_path_128_2';

  strict private
    FRomPaths1: TStringDynArray;
    FRomPaths2: TStringDynArray;
    function GetRomPaths1(const I: Integer): AnsiString;
    function GetRomPaths2(const I: Integer): AnsiString;
    procedure SetRomPaths1(const I: Integer; const AValue: AnsiString);
    procedure SetRomPaths2(const I: Integer; const AValue: AnsiString);

  private
    class procedure Init(); static;
    class procedure Final(); static;

  public
    constructor Create;
    constructor Create(ARomPaths: TRomPaths);

    class function GetRomPaths: TRomPaths;
    class function RomPathsAssigned: Boolean;

    procedure ClearPaths();
    procedure Assign(ARomPaths: TRomPaths);
    procedure LoadFromJSON(const JObj: TJSONObject);
    function SaveToJSon: TJSONObject;

    property RomPaths1[I: Integer]: AnsiString read GetRomPaths1 write SetRomPaths1;
    property RomPaths2[I: Integer]: AnsiString read GetRomPaths2 write SetRomPaths2;
  end;

implementation

{ TRomPaths }

function TRomPaths.GetRomPaths1(const I: Integer): AnsiString;
begin
  if I <> 0 then
    Result := ''
  else
    Result := FRomPaths1[I];
end;

function TRomPaths.GetRomPaths2(const I: Integer): AnsiString;
begin
  if (I < 0) or (I > 1) then
    Result := ''
  else
    Result := FRomPaths2[I];
end;

procedure TRomPaths.SetRomPaths1(const I: Integer; const AValue: AnsiString);
begin
  if I = 0 then
    FRomPaths1[0] := AValue;
end;

procedure TRomPaths.SetRomPaths2(const I: Integer; const AValue: AnsiString);
begin
  if (I >= 0) and (I <= 1) then
    FRomPaths2[I] := AValue;
end;

class procedure TRomPaths.Init;
begin
  FRomPaths := nil;
end;

class procedure TRomPaths.Final;
begin
  FRomPaths.Free;
end;

constructor TRomPaths.Create;
begin
  inherited Create;

  SetLength(FRomPaths1, 1);
  SetLength(FRomPaths2, 2);

  ClearPaths();
end;

constructor TRomPaths.Create(ARomPaths: TRomPaths);
begin
  Create;
  if Assigned(ARomPaths) then
    Self.Assign(ARomPaths);
end;

class function TRomPaths.GetRomPaths: TRomPaths;
begin
  if FRomPaths = nil then
    FRomPaths := TRomPaths.Create;
  Result := FRomPaths;
end;

class function TRomPaths.RomPathsAssigned: Boolean;
begin
  Result := Assigned(FRomPaths);
end;

procedure TRomPaths.ClearPaths();
begin
  FRomPaths1[0] := '';
  FRomPaths2[0] := '';
  FRomPaths2[1] := '';
end;

procedure TRomPaths.Assign(ARomPaths: TRomPaths);
begin
  FRomPaths1[0] := ARomPaths.GetRomPaths1(0);
  FRomPaths2[0] := ARomPaths.GetRomPaths2(0);
  FRomPaths2[1] := ARomPaths.GetRomPaths2(1);
end;

procedure TRomPaths.LoadFromJSON(const JObj: TJSONObject);
var
  Df: TJSONStringType;
begin
  ClearPaths();
  if Assigned(JObj) then begin
    Df := '';
    FRomPaths1[0] := JObj.Get(c_rom_path_1, Df);
    FRomPaths2[0] := JObj.Get(c_rom_path_2_1, Df);
    FRomPaths2[1] := JObj.Get(c_rom_path_2_2, Df);
  end;
end;

function TRomPaths.SaveToJSon: TJSONObject;
begin
  Result := nil;

  try
    if FRomPaths1[0] <> '' then begin
      Result := TJSONObject.Create();
      Result.Add(c_rom_path_1, FRomPaths1[0]);
    end;

    if FRomPaths2[0] <> '' then begin
      if Result = nil then
        Result := TJSONObject.Create();
      Result.Add(c_rom_path_2_1, FRomPaths2[0]);
    end;

    if FRomPaths2[1] <> '' then begin
      if Result = nil then
        Result := TJSONObject.Create();
      Result.Add(c_rom_path_2_2, FRomPaths2[1]);
    end;
  except
    FreeAndNil(Result);
  end;
end;

initialization
  TRomPaths.Init();

finalization
  TRomPaths.Final();

end.

