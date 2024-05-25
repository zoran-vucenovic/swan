unit UnitDebugger;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, UnitSpectrum, UnitDisassembler;

type

  TDebugger = class(TSpectrum.TAbstractDebugger)
  strict private
    type
      TBreakpoints = class(specialize TFPGMap<Word, Word>)
      public
        constructor Create;
      end;

  strict private
    FBreakpoints: TBreakpoints;
    FDisassembler: TDisassembler;
  public
    constructor CreateDebugger; override;
    destructor Destroy; override;

    procedure SetSpectrum(ASpectrum: TSpectrum); override;
    function IsOnBreakpoint: Boolean; override;
    function IsBreakpoint(Addr: Word): Boolean;
    procedure AddBreakpoint(Addr: Word);
    procedure RemoveBreakpoint(Addr: Word);
    property Breakpoints: TBreakpoints read FBreakpoints write FBreakpoints;
    property Disassembler: TDisassembler read FDisassembler;
  end;


implementation

{ TBreakpoints }

constructor TDebugger.TBreakpoints.Create;
begin
  inherited Create;

  Duplicates := TDuplicates.dupIgnore;
  Sorted := True;
end;

{ TDebugger }

constructor TDebugger.CreateDebugger;
begin
  inherited Create;

  FDisassembler := nil;
  FSpectrum := nil;
  FBreakpoints := TBreakpoints.Create;
end;

destructor TDebugger.Destroy;
begin
  FBreakpoints.Free;
  FDisassembler.Free;

  inherited Destroy;
end;

procedure TDebugger.SetSpectrum(ASpectrum: TSpectrum);
begin
  FSpectrum := ASpectrum;
  if FDisassembler = nil then begin
    FDisassembler := TDisassembler.Create(FSpectrum.Memory);
  end else
    FDisassembler.Memory := FSpectrum.Memory;
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
  Result := FBreakpoints.Find(W, N);

  if Result then begin
    I := FBreakpoints.Data[N];
    if I > 0 then begin
      FDisassembler.Dissasemble(W, N);
      if N <= I then
        Result := False;
    end;
  end;
end;

function TDebugger.IsBreakpoint(Addr: Word): Boolean;
var
  N: Integer;
  J: Word;
begin
  if FBreakpoints.Find(Addr, N) then begin
    J := FBreakpoints.Data[N];
    if J = 0 then
      Exit(True);
  end;
  Result := False;
end;

procedure TDebugger.AddBreakpoint(Addr: Word);
var
  N: Integer;
  I, J: Word;
  W: Word;
begin
  W := Addr;

  I := 0;
  while True do begin
    FBreakpoints.AddOrSetData(W, I);
    if I = 3 then
      Break;
    Dec(W);
    if FBreakpoints.Find(W, N) then begin
      J := FBreakpoints.Data[N];
      if J = 0 then
        Break;

    end;
    Inc(I);
  end;
end;

procedure TDebugger.RemoveBreakpoint(Addr: Word);
var
  J, I: Word;
  N, L: Integer;
  W: Word;
begin
  W := Addr;
  Inc(W);
  I := I.MaxValue;
  if FBreakpoints.Find(W, L) then begin
    J := FBreakpoints.Data[L];
    if (J < 3) then begin
      I := J;
    end;
  end;

  N := 0;
  W := Addr;
  while N <= 3 do begin
    if (N > 0) then begin
      if FBreakpoints.Find(W, L) then begin
        J := FBreakpoints.Data[L];
        if (J = 0) then
          Break;
      end;
    end;
    if I < 3 then begin
      Inc(I);
      FBreakpoints.AddOrSetData(W, I);
    end else
      FBreakpoints.Remove(W);

    Inc(N);
    Dec(W);
  end;
end;
{$pop}

end.

