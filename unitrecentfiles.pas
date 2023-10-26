unit UnitRecentFiles;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, fpjson;

type

  { TRecentFiles }

  TRecentFiles = class(TObject)
  private
    FMaxCount: Integer;
    FRecentFiles: TStringDynArray;
    function GetCount(): Integer;

    procedure SetMaxCount(AValue: Integer);
  public
    constructor Create;

    procedure LoadFromJSONArray(JArr: TJSONArray);
    procedure SaveToJSONArray(out JArr: TJSONArray);

    function GetLastFilePath: AnsiString;
    function Remove(const S: AnsiString): Boolean;
    procedure Add(const S: AnsiString);
    procedure GetAll(out Arr: TStringDynArray);

    property Count: Integer read GetCount;
    property MaxCount: Integer read FMaxCount write SetMaxCount;
  end;

implementation

{ TRecentFiles }

procedure TRecentFiles.SetMaxCount(AValue: Integer);
begin
  if AValue > 0 then begin
    FMaxCount := AValue;
    if AValue < GetCount() then
      SetLength(FRecentFiles, AValue);
  end;
end;

constructor TRecentFiles.Create;
begin
  inherited Create;

  FMaxCount := 10;
  SetLength(FRecentFiles, 0);
end;

procedure TRecentFiles.LoadFromJSONArray(JArr: TJSONArray);
var
  I, J, M, II: Integer;
  S: RawByteString;
  JD: TJSONData;
  Found: Boolean;
begin
  M := JArr.Count;
  if FMaxCount < M then
    M := FMaxCount;
  SetLength(FRecentFiles, M);
  J := 0;
  for I := 0 to M - 1 do begin
    S := '';
    JD := JArr.Items[I];
    if JD is TJSONString then begin
      S := JD.AsString;
      if S <> '' then begin
        Found := False;
        for II := 0 to J - 1 do begin
          if SameFileName(FRecentFiles[II], S) then begin
            Found := True;
            Break;
          end;
        end;
        if not Found then begin
          FRecentFiles[J] := S;
          Inc(J);
        end;
      end;
    end;
  end;
  SetLength(FRecentFiles, J);
end;

procedure TRecentFiles.SaveToJSONArray(out JArr: TJSONArray);
var
  I, M: Integer;
begin
  JArr := nil;
  M := GetCount();
  if M > 0 then begin
    JArr := TJSONArray.Create;
    for I := 0 to M - 1 do begin
      JArr.Add(FRecentFiles[I]);
    end;
  end;
end;

function TRecentFiles.GetCount: Integer;
begin
  Result := Length(FRecentFiles);
end;

function TRecentFiles.GetLastFilePath: AnsiString;
begin
  if Length(FRecentFiles) > 0 then
    Result := FRecentFiles[0]
  else
    Result := '';
end;

function TRecentFiles.Remove(const S: AnsiString): Boolean;
var
  K, I, J: Integer;
  Arr: TStringDynArray;
begin
  Result := False;
  K := Length(FRecentFiles);
  SetLength(Arr{%H-}, K);
  J := 0;
  for I := Low(FRecentFiles) to High(FRecentFiles) do begin
    if not SameFileName(FRecentFiles[I], S) then begin
      Arr[J] := FRecentFiles[I];
      Inc(J);
    end;
  end;
  if J < K then begin
    SetLength(Arr, J);
    FRecentFiles := Arr;
    Result := True;
  end;
end;

procedure TRecentFiles.Add(const S: AnsiString);
var
  I, J, M, K: Integer;
  Arr: TStringDynArray;
begin
  if S <> '' then begin
    K := Length(FRecentFiles);
    if FMaxCount <= K then
      M := FMaxCount
    else
      M := K + 1;
    SetLength(Arr{%H-}, M);
    J := 1;
    for I := 0 to K - 1 do begin
      if not SameFileName(FRecentFiles[I], S) then begin
        Arr[J] := FRecentFiles[I];
        Inc(J);
        if J >= M then
          Break;
      end;
    end;
    SetLength(Arr, J);
    Arr[0] := S;
    FRecentFiles := Arr;
  end;
end;

procedure TRecentFiles.GetAll(out Arr: TStringDynArray);
var
  I: Integer;
begin
  SetLength(Arr{%H-}, GetCount());
  for I := Low(Arr) to High(Arr) do
    Arr[I] := FRecentFiles[I];
end;

end.

