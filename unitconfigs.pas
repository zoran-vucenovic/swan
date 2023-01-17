unit UnitConfigs;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner
  , UnitInit // initialization in this unit affects GetConfDirectory, so keep it here in uses.
  ;

type

  TConfJSON = class(TObject)
  strict private
    type
      TRemapping = record
        RemapFrom: String;
        RemapTo: String;
      end;
      TRemappings = Array of TRemapping;
  strict private
    class var
      FConf: TJSONObject;
      FFileName: String;
      FConfDirectory: String;
      FRemappings: TRemappings;
  private
    class procedure Init;
    class procedure Final;
  strict private
    class function GetFileName: String;
    class function GetConf: TJSONObject; static;
    class function GetConfDirectory: String; static;
    class function LoadFromStream(Stream: TStream): Boolean;
    class function SaveToStream(Stream: TStream): Boolean;
    class function LoadFromFile(): Boolean;
    class function SaveToFile(): Boolean;
    class procedure Remap(R: TRemapping);
    class procedure AddRemapping(RemapFrom, RemapTo: String);
  public
    class function AddToConf(const Section: String; Data: TJSONData): Boolean;
    class function GetJSONObject(const Section: String): TJSONObject;
    class function GetJSONArray(const Section: String): TJSONArray;
    class procedure RemoveSection(const Section: String);
  end;

implementation

{ TConfJSON }

class procedure TConfJSON.Init;
begin
  FConf := nil;
  FConfDirectory := '';
  FFileName := '';

  SetLength(FRemappings, 0);
  AddRemapping('f1', 'general');
end;

class procedure TConfJSON.Final;
begin
  SaveToFile();
  FConf.Free;
end;

class function TConfJSON.GetFileName: String;
begin
  if FFileName = '' then
    FFileName := GetConfDirectory + ChangeFileExt(ExtractFileName(ApplicationName), ConfigExtension);
  Result := FFileName;
end;

class function TConfJSON.GetConf: TJSONObject;
{$push}
{$J+}
const
  FirstPass: Boolean = True;
{$pop}
begin
  if FirstPass then begin
    FirstPass := False;
    if FConf = nil then begin
      LoadFromFile();
    end;
  end;
  Result := FConf;
end;

class function TConfJSON.GetConfDirectory: String;
//{$push}
//{$J+}
//const
//  FConfDirectory: String = ''; // moved to outer scope -- reason: unfreed memory reported (a bug in compiler)
//                                 // bug report: https://gitlab.com/freepascal.org/fpc/source/-/issues/33804
//{$pop}
begin
  if FConfDirectory = '' then
    FConfDirectory := IncludeTrailingPathDelimiter(GetAppConfigDir(False));

  Result := FConfDirectory;
end;

class function TConfJSON.LoadFromStream(Stream: TStream): Boolean;
var
  Parser: TJSONParser;
  JSData: TJSONData;

  I: Integer;
begin
  Result := False;
  FreeAndNil(FConf);

  if Assigned(Stream) then begin
    Parser := TJSONParser.Create(Stream, [joUTF8]);
    try
      JSData := Parser.Parse;

      if JSData is TJSONObject then begin
        FConf := TJSONObject(JSData);

        for I := Low(FRemappings) to High(FRemappings) do begin
          Remap(FRemappings[I]);
        end;
        Result := True;
      end else
        JSData.Free;

    except
    end;

    Parser.Free;
  end;
end;

class function TConfJSON.SaveToStream(Stream: TStream): Boolean;
var
  S: RawByteString;
  L: Integer;
begin
  Result := False;
  if Assigned(Stream) and Assigned(FConf) then begin
    S := FConf.FormatJSON();
    L := Length(S);
    Stream.Size := L;
    Stream.Position := 0;
    Result := Stream.Write(PAnsiChar(S)^, L) = L;
  end;
end;

class function TConfJSON.LoadFromFile: Boolean;
var
  Stream: TStream;
begin
  Result := False;
  FreeAndNil(FConf);
  try
    Stream := TFileStream.Create(GetFileName, fmOpenRead or fmShareDenyNone);
    try
      Result := LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
  end;
end;

class function TConfJSON.SaveToFile(): Boolean;   
var
  Stream: TStream;
  Mode: Word;
begin
  Result := False;
  try
    if Assigned(FConf) and ForceDirectories(GetConfDirectory) then begin
      Mode := fmOpenReadWrite or fmCreate;
      Stream := TFileStream.Create(GetFileName, Mode);
      try
        Result := SaveToStream(Stream);
      finally
        Stream.Free;
      end;
    end;
  except
  end;
end;

class procedure TConfJSON.Remap(R: TRemapping);
var
  JD: TJSONData;
begin
  if Assigned(GetConf) then begin
    JD := GetConf.Extract(R.RemapFrom);
    if Assigned(JD) then begin
      if GetConf.IndexOfName(R.RemapTo) <> -1 then
        JD.Free
      else
        GetConf.Add(R.RemapTo, JD);
    end;
  end;
end;

class procedure TConfJSON.AddRemapping(RemapFrom, RemapTo: String);
var
  L: Integer;
begin
  L := Length(FRemappings);
  SetLength(FRemappings, L + 1);
  FRemappings[L].RemapFrom := RemapFrom;
  FRemappings[L].RemapTo := RemapTo;
end;

class function TConfJSON.AddToConf(const Section: String; Data: TJSONData
  ): Boolean;
begin
  if GetConf = nil then begin
    FConf := TJSONObject.Create;
  end;

  Result := FConf.Add(Section, Data) >= 0;
end;

class function TConfJSON.GetJSONObject(const Section: String): TJSONObject;
var
  D: TJSONData;
begin
  Result := nil;
  if Assigned(GetConf) then begin
    D := FConf.Find(Section);
    if D is TJSONObject then
      Result := TJSONObject(D);
  end;
end;

class function TConfJSON.GetJSONArray(const Section: String): TJSONArray;
var
  D: TJSONData;
begin
  Result := nil;
  if Assigned(GetConf) then begin
    D := FConf.Find(Section);
    if D is TJSONArray then
      Result := TJSONArray(D);
  end;
end;

class procedure TConfJSON.RemoveSection(const Section: String);
var
  D: TJSONData;
begin
  if Assigned(GetConf) then begin
    D := FConf.Extract(Section);
    D.Free;
  end;

end;

initialization
  TConfJSON.Init;

finalization
  TConfJSON.Final;

end.

