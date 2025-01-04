unit UnitInit;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0
{
  Put here anything that might be needed to execute first. Even before some
  initialization section in other units.
  So, keep this unit at the top of main unit uses list (above other our units).
}

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  SysUtils;

implementation

function GetAppName: String;
begin
  Result := 'Swan';
end;

{ This is for config path }
function GetVendorName: String;
begin
  Result := GetAppName + ' ZX Spectrum Emulator';
end;

procedure Init;
begin
  OnGetApplicationName := @GetAppName;
  OnGetVendorName := @GetVendorName;
end;

initialization
  Init;

end.

