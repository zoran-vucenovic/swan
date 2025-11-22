unit UnitOnlineHelp;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitCommon, LazHelpHTML;

implementation

type
  TOnlineHelp = class(TObject)
  strict private
    HelpDatabase: THTMLHelpDatabase;
    HelpViewer: THTMLBrowserHelpViewer;

  private
    class var
      OnlineHelp: TOnlineHelp;

  private
    class procedure Init; static;
    class procedure Final; static;

  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TOnlineHelp }

class procedure TOnlineHelp.Init;
begin
  OnlineHelp := TOnlineHelp.Create;
end;

class procedure TOnlineHelp.Final;
begin
  FreeAndNil(OnlineHelp);
end;

constructor TOnlineHelp.Create;
begin
  inherited Create;

  HelpDatabase := THTMLHelpDatabase.Create(nil);
  HelpViewer := THTMLBrowserHelpViewer.Create(nil);

  HelpDatabase.BaseURL := TCommonFunctions.ApplicationHelp;
  HelpDatabase.KeywordPrefix := 'help:';

  HelpDatabase.AutoRegister := True;
  HelpViewer.AutoRegister := True;
end;

destructor TOnlineHelp.Destroy;
begin
  HelpViewer.Free;
  HelpDatabase.Free;

  inherited Destroy;
end;

initialization
  TOnlineHelp.Init;

finalization
  TOnlineHelp.Final;

end.
