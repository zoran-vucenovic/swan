unit UnitDebugger;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, LazMethodList, UnitSpectrum, UnitDisassembler;

type
  TBreakpointCondition = class(TObject)
  private
    FCondExprRpn: RawByteString;
    FConditionExpr: RawByteString;
    procedure SetConditionExpr(AValue: RawByteString);
  public
    constructor Create;

    procedure Clear;
    property ConditionExpr: RawByteString read FConditionExpr write SetConditionExpr;
    property CondExprRpn: RawByteString read FCondExprRpn write FCondExprRpn;
  end;

  TBreakpoints = class(specialize TFPGMapObject<Word, TBreakpointCondition>)
  public
    constructor Create;
  end;

  TDebugger = class(TSpectrum.TAbstractDebugger)
  strict private
    FBreakpoints: TBreakpoints;
    FDisassembler: TDisassembler;
    FOnBreakpointChangeList: TMethodList;
    FSpectrum: TSpectrum;

    procedure DoOnBreakpointChange;
  public
    constructor CreateDebugger; override;
    destructor Destroy; override;

    procedure SetSpectrum(ASpectrum: TSpectrum); override;
    function IsBreakpoint(Addr: Word): Boolean; inline;
    function IsOnBreakpoint: Boolean; override;
    function BreakpointsEmpty: Boolean; override;
    procedure AddBreakpoint(Addr: Word; ABkpointCond: TBreakpointCondition);
    procedure RemoveBreakpoint(Addr: Word);
    procedure RemoveAllBreakpoints();
    procedure AddOnBreakpointChange(ANotifyEvent: TNotifyEvent);
    procedure RemoveOnBreakPointChangeHandlerOfObject(AObject: TObject);

    property Breakpoints: TBreakpoints read FBreakpoints;
    property Disassembler: TDisassembler read FDisassembler;
  end;

implementation

{ TBreakpointCondition }

procedure TBreakpointCondition.SetConditionExpr(AValue: RawByteString);
begin
  if FConditionExpr <> AValue then begin
    FCondExprRpn := '';
    FConditionExpr := AValue;
  end;
end;

constructor TBreakpointCondition.Create;
begin
  Clear;
end;

procedure TBreakpointCondition.Clear;
begin
  FConditionExpr := '';
  FCondExprRpn := '';
end;

{ TBreakpoints }

constructor TBreakpoints.Create;
begin
  inherited Create(True);

  Duplicates := TDuplicates.dupIgnore;
  Sorted := True;
end;

{ TDebugger }

procedure TDebugger.DoOnBreakpointChange;
var
  I: Integer;
begin
  if Assigned(FOnBreakpointChangeList) then begin
    for I := 0 to FOnBreakpointChangeList.Count - 1 do begin
      TNotifyEvent(FOnBreakpointChangeList[I])(Self);
    end;
  end;
end;

constructor TDebugger.CreateDebugger;
begin
  inherited Create;

  FOnBreakpointChangeList := nil;
  FDisassembler := nil;
  FSpectrum := nil;
  FBreakpoints := TBreakpoints.Create;
end;

destructor TDebugger.Destroy;
begin
  FOnBreakpointChangeList.Free;
  FBreakpoints.Free;
  FDisassembler.Free;

  inherited Destroy;
end;

function TDebugger.BreakpointsEmpty: Boolean;
begin
  Result := FBreakpoints.Count = 0;
end;

procedure TDebugger.SetSpectrum(ASpectrum: TSpectrum);
begin
  FSpectrum := ASpectrum;
  if FDisassembler = nil then begin
    FDisassembler := TDisassembler.Create(FSpectrum.Memory);
  end else
    FDisassembler.Memory := FSpectrum.Memory;
end;

function TDebugger.IsBreakpoint(Addr: Word): Boolean;
var
  N: Integer;
begin
  Result := FBreakpoints.Find(Addr, N);
end;

function TDebugger.IsOnBreakpoint: Boolean;
var
  N: Integer;
begin
  Result := FBreakpoints.Find(FSpectrum.GetProcessor.RegPC, N);
end;

procedure TDebugger.AddBreakpoint(Addr: Word; ABkpointCond: TBreakpointCondition
  );
begin
  if ABkpointCond = nil then
    ABkpointCond := TBreakpointCondition.Create;

  FBreakpoints.AddOrSetData(Addr, ABkpointCond);
  DoOnBreakpointChange;
end;

procedure TDebugger.RemoveBreakpoint(Addr: Word);
begin
  FBreakpoints.Remove(Addr);
  DoOnBreakpointChange;
end;

procedure TDebugger.RemoveAllBreakpoints();
begin
  FBreakpoints.Clear;
  DoOnBreakpointChange;
end;

procedure TDebugger.AddOnBreakpointChange(ANotifyEvent: TNotifyEvent);
begin
  if Assigned(ANotifyEvent) then begin
    if FOnBreakpointChangeList = nil then begin
      FOnBreakpointChangeList := TMethodList.Create;
    end;

    FOnBreakpointChangeList.Add(TMethod(ANotifyEvent));
  end;
end;

procedure TDebugger.RemoveOnBreakPointChangeHandlerOfObject(AObject: TObject);
begin
  if Assigned(FOnBreakpointChangeList) then begin
    FOnBreakpointChangeList.RemoveAllMethodsOfObject(AObject);
    if FOnBreakpointChangeList.Count = 0 then
      FreeAndNil(FOnBreakpointChangeList);
  end;

end;

end.

