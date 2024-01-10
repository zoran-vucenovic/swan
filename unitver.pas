unit UnitVer;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$J-}

interface

uses
  Classes, SysUtils, fileinfo, versiontypes;

type
  TVersion = class sealed (TObject)
  strict private
    class var
      FVersionString: String;
      FFullVersionString: String;
      FVersionBuildNumber: Word;
      FVersionMajor: Word;
      FVersionMinor: Word;
      FVersionRevision: Word;
      FFullVersion: DWord;

    class function GetFullVersion: DWord; static;
    class function GetFullVersionString: String; static;
    class function GetVersionBuildNumber: Word; static;
    class function GetVersionMajor: Word; static;
    class function GetVersionMinor: Word; static;
    class function GetVersionRevision: Word; static;
    class function GetVersionString: String; static;
    class function Load: Boolean; static;
  private
    class procedure Init; static;
  public
    class property VersionMajor: Word read GetVersionMajor;
    class property VersionMinor: Word read GetVersionMinor;
    class property VersionRevision: Word read GetVersionRevision;
    class property VersionBuildNumber: Word read GetVersionBuildNumber;
    class property FullVersion: DWord read GetFullVersion;     
    class property FullVersionString: String read GetFullVersionString;

    class property VersionString: String read GetVersionString;
  end;

implementation

{ TVersion }

class function TVersion.GetVersionString: String;
begin
  if Load then
    Exit(FVersionString);
  Result := '';
end;

class function TVersion.GetVersionBuildNumber: Word; static;
begin
  if not Load then
    Exit(0);
  Result := FVersionBuildNumber;
end;

class function TVersion.GetFullVersion: DWord; static;
begin
  if not Load then
    Exit(0);
  Result := FFullVersion;
end;

class function TVersion.GetFullVersionString: String; static;
begin
  if not Load then
    Exit('');
  Result := FFullVersionString;
end;

class function TVersion.GetVersionMajor: Word; static;
begin
  if not Load then
    Exit(0);
  Result := FVersionMajor;
end;

class function TVersion.GetVersionMinor: Word; static;
begin
  if not Load then
    Exit(0);
  Result := FVersionMinor;
end;

class function TVersion.GetVersionRevision: Word; static;
begin
  if not Load then
    Exit(0);
  Result := FVersionRevision;
end;

class function TVersion.Load: Boolean;

  function BuildVersionString(): String;
  begin
    // When we reach 1.0, let's just use Major and Minor version number (not revision):
    WriteStr(Result, FVersionMajor, '.', FVersionMinor);

    // Add revision only if it is not zero.
    // Before 1.0, always use revision, (and perhaps later, when needed).
    if FVersionRevision <> 0 then
      Result := Result + '.' + FVersionRevision.ToString;

    // Plan:
    // - in alpha stage - keep versions 0.1.x - 0.5.x (probably just stick to 0.1.x)
    // - in beta stage - versions 0.6.x - 0.9.x (probably just 0.9.x)
    // If we change this plan, change this code.
    if FVersionMajor < 1 then begin
      if FVersionMinor <= 5 then
        Result := Result + ' alpha'
      else if FVersionMinor <= 9 then
        Result := Result + ' beta';
    end;
    // and never display build number (FPV[3]).
  end;

{$push}
{$J+}
const
  Loaded: Boolean = False;
{$pop}

var
  VerInfo: TVersionInfo;
  FPV: TFileProductVersion;

begin
  if not Loaded then
    try
      VerInfo := TVersionInfo.Create;
      try
        VerInfo.Load(HINSTANCE);
        FPV := VerInfo.FixedInfo.FileVersion;

        FVersionMajor := FPV[0];
        FVersionMinor := FPV[1];
        FVersionRevision := FPV[2];
        FVersionBuildNumber := FPV[3];
        FFullVersion := (FVersionMajor * 100 + FVersionMinor) * 100 + FVersionRevision;
        FFullVersionString := Format('%d.%d.%d', [FVersionMajor, FVersionMinor, FVersionRevision]);

        FVersionString := BuildVersionString();

        Loaded := True;

      finally
        VerInfo.Free;
      end;

    except
    end;

  Result := Loaded;
end;

class procedure TVersion.Init;
begin
  FVersionString := '';
  FFullVersionString := '';
  FVersionMajor := 0;
  FVersionMinor := 0;
  FVersionRevision := 0;
  FVersionBuildNumber := 0;
  FFullVersion := 0;
end;

initialization
  TVersion.Init;

end.

