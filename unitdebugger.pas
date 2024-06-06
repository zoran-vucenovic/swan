unit UnitDebugger;
// Copyright 2022-2024 Zoran Vučenović
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
    type
      TBreakpointsW = class(specialize TFPGMap<Word, Word>)
      public
        constructor Create;
      end;

  strict private
    FBreakpointsWide: TBreakpointsW;
    FBreakpoints: TBreakpoints;
    FDisassembler: TDisassembler;
    FOnBreakpointChangeList: TMethodList;

    procedure DoOnBreakpointChange;
  public
    constructor CreateDebugger; override;
    destructor Destroy; override;

    procedure SetSpectrum(ASpectrum: TSpectrum); override;
    function IsBreakpoint(Addr: Word): Boolean; inline;
    function IsOnBreakpoint: Boolean; override;
    function BreakpointsEmpty: Boolean; override;
    procedure AddBreakpoint(Addr: Word);
    procedure RemoveBreakpoint(Addr: Word);
    procedure RemoveAllBreakpoints();
    procedure AddOnBreakpointChange(ANotifyEvent: TNotifyEvent);
    procedure RemoveOnBreakPointChangeHandlerOfObject(AObject: TObject);

    property Breakpoints: TBreakpoints read FBreakpoints write FBreakpoints;
    property Disassembler: TDisassembler read FDisassembler;
  end;

implementation

{ TDebugger.TBreakpointsW }

constructor TDebugger.TBreakpointsW.Create;
begin
  inherited Create;

  Duplicates := TDuplicates.dupIgnore;
  Sorted := True;
end;

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
  FBreakpointsWide := TBreakpointsW.Create;
  FBreakpoints := TBreakpoints.Create;
end;

destructor TDebugger.Destroy;
begin
  FOnBreakpointChangeList.Free;
  FBreakpointsWide.Free;
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

{$push}
{$Q-}{$R-}
function TDebugger.IsOnBreakpoint: Boolean;
var
  N: Integer;
  W: Word;
  I: Word;
begin
  W := FSpectrum.GetProcessor.RegPC;
  Result := FBreakpointsWide.Find(W, N);

  if Result then begin
    I := FBreakpointsWide.Data[N];
    if I > 0 then begin
      FDisassembler.Dissasemble(W, N);
      if N <= I then
        Result := False;
    end;
  end;
end;

procedure TDebugger.AddBreakpoint(Addr: Word);
var
  I: Word;
  W: Word;
  B: TBreakpointCondition;
begin
  B := TBreakpointCondition.Create;
  FBreakpoints.AddOrSetData(Addr, B);

  W := Addr;

  I := 0;
  while True do begin
    FBreakpointsWide.AddOrSetData(W, I);
    if I = 3 then
      Break;
    Dec(W);
    if IsBreakpoint(W) then
      Break;
    Inc(I);
  end;

  DoOnBreakpointChange;
end;

procedure TDebugger.RemoveBreakpoint(Addr: Word);
var
  J, I: Word;
  N, L: Integer;
  W: Word;
begin
  FBreakpoints.Remove(Addr);

  W := Addr;
  Inc(W);
  I := I.MaxValue;
  if FBreakpointsWide.Find(W, L) then begin
    J := FBreakpointsWide.Data[L];
    if (J < 3) then begin
      I := J;
    end;
  end;

  N := 0;
  W := Addr;
  while N <= 3 do begin
    if (N > 0) then begin
      if FBreakpointsWide.Find(W, L) then begin
        J := FBreakpointsWide.Data[L];
        if (J = 0) then
          Break;
      end;
    end;
    if I < 3 then begin
      Inc(I);
      FBreakpointsWide.AddOrSetData(W, I);
    end else
      FBreakpointsWide.Remove(W);

    Inc(N);
    Dec(W);
  end;

  DoOnBreakpointChange;
end;

{$pop}

procedure TDebugger.RemoveAllBreakpoints();
begin
  FBreakpoints.Clear;
  FBreakpointsWide.Clear;
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

