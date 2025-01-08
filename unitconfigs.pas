unit UnitConfigs;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner
  , UnitInit // initialization in this unit affects GetConfDirectory, so keep it here in uses.
  , UnitVer, UnitCommon
  ;

type

  TConfJSON = class(TObject)
  strict private
    const
      cSectionVersion = 'version';

  strict private
    class var
      FConf: TJSONObject;
      FFileName: String;
      FConfDirectory: String;

      FFullVersionLoadedFromConf: DWord;
  private
    class procedure Init;
    class procedure Final;
  strict private
    class function GetFileName: String;
    class function GetConf: TJSONObject; static;
    class function GetConfDirectory: String; static;
    class function GetFullVersionLoadedFromConf: DWord; static;
    class procedure LoadFullVersionFromConf; static;
    class function LoadFromStream(Stream: TStream): Boolean;
    class function SaveToStream(Stream: TStream): Boolean;
    class function LoadFromFile(): Boolean;
    class function SaveToFile(): Boolean;
    class procedure ChangeOldName(AFrom, ATo: String);
  public
    class function AddToConf(const Section: String; Data: TJSONData): Boolean;
    class function GetJSONObject(const Section: String): TJSONObject;
    class function GetJSONArray(const Section: String): TJSONArray;
    class procedure RemoveSection(const Section: String);

    class property FullVersionLoadedFromConf: DWord read GetFullVersionLoadedFromConf;
  end;

implementation

{ TConfJSON }

class procedure TConfJSON.Init;
begin
  FConf := nil;
  FConfDirectory := '';
  FFileName := '';
  FFullVersionLoadedFromConf := 0;
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
      if FConf = nil then
        FConf := TJSONObject.Create;
      ChangeOldName('f1', 'general');
      LoadFullVersionFromConf;
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

class function TConfJSON.GetFullVersionLoadedFromConf: DWord; static;
begin
  GetConf;
  Result := FFullVersionLoadedFromConf;
end;

class procedure TConfJSON.LoadFullVersionFromConf;

  function UnpackVersionString(S: String): DWord;
  var
    P: Integer;
    N: Integer;
    S1: String;
    I, K: Integer;
    D: DWord;
  begin
    D := 0;
    Result := 0;

    N := 1;
    I := 0;
    repeat
      P := Pos('.', S, N);
      if P > 0 then begin
        if I >= 2 then
          Exit;

        S1 := Copy(S, N, P - N);
        N := P + 1;

      end else
        S1 := Copy(S, N);

      if (not TCommonFunctions.TryStrToIntDecimal(S1, K))
         or (K < 0) or ((I > 0) and (K > 99))
      then
        Exit;

      Inc(I);

      D := D * 100 + DWord(K);

    until P <= 0;

    case I of
      2:
        Result := D * 100;
      3:
        Result := D;
    otherwise
    end;

  end;

{$push}
{$J+}
const
  FirstPass: Boolean = True;
{$pop}

const
  cGeneral = 'general';
  cOldSectionSwanVersion = 'swan_version';
var
  Jd, JVer: TJSONData;
  JObj: TJSONObject;

begin
  if FirstPass then begin
    FirstPass := False;
    FFullVersionLoadedFromConf := 0;

    if Assigned(GetConf) then begin
      JVer := FConf.Find(cSectionVersion);
      if JVer is TJSONString then begin
        FFullVersionLoadedFromConf := UnpackVersionString(JVer.AsString);
        JVer.AsString := TVersion.FullVersionString;

        Exit;
      end;

      JVer := FConf.Extract(cSectionVersion);
      FreeAndNil(JVer);

      FConf.Add(cSectionVersion, TVersion.FullVersionString);

      JObj := GetJSONObject(cGeneral);
      if Assigned(JObj) then begin
        Jd := JObj.Extract(cOldSectionSwanVersion);
        JVer := JObj.Extract(cSectionVersion);
        if not Assigned(JVer) then begin
          JVer := Jd;
          Jd := nil;
        end;
        Jd.Free;

        if JVer is TJSONString then
          FFullVersionLoadedFromConf := UnpackVersionString(JVer.AsString);

        JVer.Free;
      end;

    end;
  end;
end;

class function TConfJSON.LoadFromStream(Stream: TStream): Boolean;
var
  Parser: TJSONParser;
  JSData: TJSONData;

begin
  Result := False;
  FreeAndNil(FConf);

  if Assigned(Stream) then begin
    Parser := TJSONParser.Create(Stream, [joUTF8]);
    try
      JSData := Parser.Parse;

      if JSData is TJSONObject then begin
        FConf := TJSONObject(JSData);

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

class procedure TConfJSON.ChangeOldName(AFrom, ATo: String);
var
  JD: TJSONData;
begin
  if Assigned(GetConf) then begin
    JD := GetConf.Extract(AFrom);
    if Assigned(JD) then begin
      if GetConf.IndexOfName(ATo) <> -1 then
        JD.Free
      else
        GetConf.Add(ATo, JD);
    end;
  end;
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

