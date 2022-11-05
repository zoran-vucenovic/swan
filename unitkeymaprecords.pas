unit UnitKeyMapRecords;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, UnitConfigs, fpjson;

type
  TKeyMapRec = record
    Key: Word;
    SpectrumKey: Word;
  end;

  TKeyMapRecs = array of TKeyMapRec;

  { TKeyMappings }

  TKeyMappings = class(specialize TFPGMap<RawByteString, TKeyMapRecs>)
  strict private
    const
      cKeyMap: RawByteString = 'key_map';
      cKeyMapName: RawByteString = 'key_map_name';
      cActiveKeyMapName: RawByteString = 'active_key_map_name';
      cKeyMapping: RawByteString = 'key_mapping';
      cKeyMaps: RawByteString = 'key_maps';
      cPCKey: RawByteString = 'pc_key';
      cSpectrumKey: RawByteString = 'spectrum_key';

  strict private
    FActiveKeyMapName: RawByteString;
    //FActiveKeyMapRecs: TKeyMapRecs;

    function SaveToJSON(out JSObj: TJSONObject): Boolean;
    procedure LoadFromJSON(const JSObj: TJSONObject);
    procedure LoadFromConf;
    procedure SaveToConf;
    procedure SetActiveKeyMapName(const AName: RawByteString);
  strict private
    class var
      FKMappings: TKeyMappings;
      FDefaultMapRecs: TKeyMapRecs;
  private
    class procedure Init;
    class procedure Final;
    class procedure SetDefaultMapRecs(AValue: TKeyMapRecs); static;

  public
    constructor Create;
    destructor Destroy; override;

    function GetKeyMapRecs(const AName: RawByteString; out AKeyMapRecs: TKeyMapRecs): Boolean;
    //procedure GetActiveKeyMapRecs(out AName: RawByteString; out AKeyMapRecs: TKeyMapRecs);

    property ActiveKeyMapName: RawByteString read FActiveKeyMapName write SetActiveKeyMapName;
    class property KMappings: TKeyMappings read FKMappings;
    class property DefaultMapRecs: TKeyMapRecs read FDefaultMapRecs write SetDefaultMapRecs;
  end;

implementation

{ TKeyMappings }

procedure TKeyMappings.LoadFromConf;
var
  JObj: TJSONObject;
begin
  JObj := TConfJSON.GetJSONObject(cKeyMapping);
  if Assigned(JObj) then
    LoadFromJSON(JObj);
end;

procedure TKeyMappings.SaveToConf;
var
  JObj: TJSONObject;
begin
  if SaveToJSON(JObj) and Assigned(JObj) then
    try
      TConfJSON.RemoveSection(cKeyMapping);
      if TConfJSON.AddToConf(cKeyMapping, JObj) then
        JObj := nil;
    finally
      JObj.Free;
    end;
end;

class procedure TKeyMappings.Init;
begin
  SetLength(FDefaultMapRecs, 0);
  FKMappings := TKeyMappings.Create;
end;

class procedure TKeyMappings.Final;
begin
  FKMappings.Free;
end;

class procedure TKeyMappings.SetDefaultMapRecs(AValue: TKeyMapRecs);
begin
  if Length(FDefaultMapRecs) = 0 then
    FDefaultMapRecs := AValue;
end;

destructor TKeyMappings.Destroy;
begin
  SaveToConf;

  inherited Destroy;
end;

function TKeyMappings.GetKeyMapRecs(const AName: RawByteString; out
  AKeyMapRecs: TKeyMapRecs): Boolean;
begin
  if (AName = '') then begin
    AKeyMapRecs := FDefaultMapRecs;
    Result := True;
  end else
    Result := TryGetData(AName, AKeyMapRecs);

  Result := Result and (Length(AKeyMapRecs) > 0);
  if not Result then
    SetLength(AKeyMapRecs, 0);
end;

function KeyMappingsStringsCompareFun(const S1, S2: RawByteString): Integer;
begin
  Result := AnsiCompareText(UTF8Trim(S1), UTF8Trim(S2));
end;

constructor TKeyMappings.Create;
begin
  inherited Create;

  FActiveKeyMapName := '';
  OnKeyCompare := @KeyMappingsStringsCompareFun;
  Sorted := True;
  Duplicates := TDuplicates.dupIgnore;

  LoadFromConf;
end;

function TKeyMappings.SaveToJSON(out JSObj: TJSONObject): Boolean;
var
  JArr, JArr2: TJSONArray;
  JO, JO2: TJSONObject;
  I, J, N: Integer;
  W: Word;
  KMaps: TKeyMapRecs;
  KMapRec: TKeyMapRec;
  S: RawByteString;

begin
  Result := False;
  JSObj := nil;

  if Self.Count > 0 then begin
    JArr := TJSONArray.Create;
    try
      for I := 0 to Self.Count - 1 do begin
        JO := TJSONObject.Create;
        try
          S := UTF8Trim(Self.Keys[I]);
          if S <> '' then begin
            KMaps := Self.Data[I];
            if Length(KMaps) > 0 then begin

              JO.Add(cKeyMapName, S);

              JArr2 := TJSONArray.Create;
              try
                for J := Low(KMaps) to High(KMaps) do begin
                  KMapRec := KMaps[J];
                  JO2 := TJSONObject.Create;
                  try
                    W := KMapRec.Key;
                    N := W;
                    JO2.Add(cPCKey, N);
                    W := KMapRec.SpectrumKey;
                    N := W;
                    JO2.Add(cSpectrumKey, N);

                    if JArr2.Add(JO2) >= 0 then
                      JO2 := nil;
                  finally
                    JO2.Free;
                  end;
                end;

                if JO.Add(cKeyMap, JArr2) >= 0 then
                  JArr2 := nil;
              finally
                JArr2.Free;
              end;
              if JArr.Add(JO) >= 0 then
                JO := nil;
            end;
          end;
        finally
          JO.Free;
        end;
      end;

      JSObj := TJSONObject.Create;
      try
        if (JSObj.Add(cActiveKeyMapName, FActiveKeyMapName) >= 0)
          and (JSObj.Add(cKeyMaps, JArr) >= 0)
        then begin
          JArr := nil;
          Result := True;
        end;

      finally
        if not Result then
          FreeAndNil(JSObj);
      end;
    finally
      JArr.Free;
    end;
  end;
end;

procedure TKeyMappings.LoadFromJSON(const JSObj: TJSONObject);
var
  JD: TJSONData;
  JArr, JArr2: TJSONArray;
  JO, JO2: TJSONObject;
  I, J, N, K: Integer;
  KMaps: TKeyMapRecs;
  KMapRec: TKeyMapRec;
  S: RawByteString;

  W: Word;
  W2: Word;
  //WR: WordRec absolute W2;

begin
  //Result := False;
  Self.Clear;

  if Assigned(JSObj) then begin
    if JSObj.Find(cKeyMaps, JArr) and Assigned(JArr) then begin
      for I := 0 to JArr.Count - 1 do begin
        JD := JArr.Items[I];
        if JD is TJSONObject then begin
          JO := TJSONObject(JD);
          //
          S := '';
          JD := JO.Find(cKeyMapName);
          if JD is TJSONString then begin
            S := UTF8Trim(JD.AsString);
            if S <> '' then begin
              if JO.Find(cKeyMap, JArr2) then begin
                SetLength(KMaps{%H-}, JArr2.Count);
                K := 0;
                for J := 0 to JArr2.Count - 1 do begin
                  JD := JArr2.Items[J];
                  if JD is TJSONObject then begin
                    JO2 := TJSONObject(JD);
                    N := -1;
                    N := JO2.Get(cPCKey, N);
                    if (N > 0) and (N <= Word.MaxValue) then begin
                      W := N;
                      N := -1;
                      N := JO2.Get(cSpectrumKey, N);
                      if (N >= 0) and (N <= Word.MaxValue) then begin
                        W2 := N;
                        KMapRec.Key := W;
                        KMapRec.SpectrumKey := W2;
                        KMaps[K] := KMapRec;
                        Inc(K);
                      end;
                    end;
                  end;
                end;
                SetLength(KMaps, K);
                if K > 0 then
                  Self.Add(S, KMaps);
              end;
            end;

          end;

        end;
      end;
    end;
    S := '';
    JD := JSObj.Find(cActiveKeyMapName);
    if JD is TJSONString then begin
      S := UTF8Trim(JD.AsString);
      //Result := SetActiveKeyMapName(S);
    end;
    SetActiveKeyMapName(S);
  end;
end;

//procedure TKeyMappings.GetActiveKeyMapRecs(out AName: RawByteString; out
//  AKeyMapRecs: TKeyMapRecs);
//begin
//  if GetKeyMapRecs(FActiveKeyMapName, AKeyMapRecs) then
//    AName := FActiveKeyMapName
//  else
//    AName := '';
//end;

procedure TKeyMappings.SetActiveKeyMapName(const AName: RawByteString);
begin
  FActiveKeyMapName := UTF8Trim(AName);
end;

initialization
  TKeyMappings.Init;

finalization
  TKeyMappings.Final;

end.

