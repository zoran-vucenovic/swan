unit UnitDebugCpuState;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRAPolygon,
  Z80Processor, UnitFrameWordDisplay, CommonFunctionsLCL, Controls, Graphics,
  StdCtrls;

type

  TFlagsCtrl = class(TCustomControl)
  public
    const
      DefColour = $fefdf8;
  strict private
    FFlagCtrls: array of TCustomControl;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; FlagCtrls: array of AnsiString; const LabelsLeft: Boolean; AVertical: Boolean);
    procedure SetValues(AValue: Byte);
  end;

  TDebugCpuState = class(TCustomControl)
  private
    FrAF: TFrameWordDisplay;
    FrBC: TFrameWordDisplay;
    FrDE: TFrameWordDisplay;
    FrHL: TFrameWordDisplay;
    FrAF1: TFrameWordDisplay;
    FrBC1: TFrameWordDisplay;
    FrDE1: TFrameWordDisplay;
    FrHL1: TFrameWordDisplay;
    FrIx: TFrameWordDisplay;
    FrIy: TFrameWordDisplay;
    FrI: TFrameWordDisplay;
    FrR: TFrameWordDisplay;
    FrPC: TFrameWordDisplay;
    FrSP: TFrameWordDisplay;
    FrWZ: TFrameWordDisplay;

    FlCtrlsFlags: TFlagsCtrl;
    FlCtrlsIff: TFlagsCtrl;
    FlCtrlsInterruptPin: TFlagsCtrl;
    FlCtrlsHalt: TFlagsCtrl;

    InterruptModeCtrl: TCustomControl;
    LabInterruptMode: TLabel;
    LabInterruptModeValue: TLabel;
    RegistersContainer: TCustomControl;
    FlagsContainer: TCustomControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetAll(const AProc: TProcessor);
  end;

implementation

type
  TFlagCtrl = class(TCustomControl)
  strict private
    type
      TPaintCtrl = class(TGraphicControl)
      private
        class var
          BmpFalse: TBGRABitmap;
          BmpTrue: TBGRABitmap;
      private
        class procedure Init;
        class procedure Final;
      public
        FValue: Boolean;
        procedure Paint; override;
      end;

  private
    class procedure Init;
    class procedure Final;
  strict private
    FLabelLeft: Boolean;

    FPaintCtrl: TPaintCtrl;
    Lab: TLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetValue(const AValue: Boolean);
    procedure SetLabelLeft(const AValue: Boolean);
    procedure SetLabel(const ACaption: AnsiString);
  end;

{ TFlagCtrl.TPaintCtrl }

class procedure TFlagCtrl.TPaintCtrl.Init;
var
  BPix, BPix1, BPix2: TBGRAPixel;
begin
  BmpTrue := TBGRABitmap.Create(12, 12, BGRAPixelTransparent);
  BmpFalse := TBGRABitmap.Create(12, 12, BGRAPixelTransparent);

  BPix.FromColor($a0c0b0);
  BPix1.FromColor(clNavy);
  BPix2.FromColor($f2edf0);

  BmpFalse.FillEllipseLinearColorAntialias(5, 5, 5, 5, BPix2, BGRAWhite);
  BmpFalse.Ellipse(5, 5, 4.3, 4.3, BPix, 1.0, TDrawMode.dmSet);
  BmpTrue.FillEllipseLinearColorAntialias(5, 5, 5, 5, BGRAWhite, BPix1);
  BmpTrue.Ellipse(5, 5, 4.3, 4.3, BPix, 1.0, TDrawMode.dmSet);
end;

class procedure TFlagCtrl.TPaintCtrl.Final;
begin
  BmpFalse.Free;
  BmpTrue.Free;
end;

procedure TFlagCtrl.TPaintCtrl.Paint;
var
  B: TBGRABitmap;
begin
  inherited Paint;

  if FValue then
    B := BmpTrue
  else
    B := BmpFalse;

  B.Draw(Canvas, 0, 0, False);
end;

{ TFlagsCtrl }

constructor TFlagsCtrl.Create(AOwner: TComponent);
begin
  Create(AOwner, [''], True, False);
end;

constructor TFlagsCtrl.Create(AOwner: TComponent;
  FlagCtrls: array of AnsiString; const LabelsLeft: Boolean; AVertical: Boolean
  );
var
  Fl, FlPrev: TFlagCtrl;
  N, FlagsCount: Integer;
begin
  inherited Create(AOwner);

  Color := DefColour;

  BorderStyle := bsSingle;

  FlagsCount := Length(FlagCtrls);
  if FlagsCount > 8 then
    FlagsCount := 8;
  SetLength(FFlagCtrls, FlagsCount);

  if LabelsLeft then
    N := 3
  else
    N := 1;

  FlPrev := nil;
  while FlagsCount > 0 do begin
    Dec(FlagsCount);
    Fl := TFlagCtrl.Create(Self);
    FFlagCtrls[FlagsCount] := Fl;
    Fl.SetValue(Odd(FlagsCount));
    Fl.SetLabel(FlagCtrls[FlagsCount]);
    Fl.SetLabelLeft(LabelsLeft);

    Fl.Anchors := [];
    if FlPrev = nil then begin
      Fl.AnchorParallel(akTop, 1, Self);
      Fl.AnchorParallel(akRight, 1, Self);
    end else if not AVertical then begin
      Fl.AnchorParallel(akTop, 1, Self);
      Fl.AnchorToNeighbour(akRight, N, FlPrev);
    end else begin
      Fl.AnchorToNeighbour(akTop, 1, FlPrev);
      Fl.AnchorParallel(akRight, 1, Self);
    end;
    FlPrev := Fl;
    Fl.Parent := Self;
  end;

  AutoSize := True;
end;

procedure TFlagsCtrl.SetValues(AValue: Byte);
var
  I: Integer;
begin
  for I := Low(FFlagCtrls) to High(FFlagCtrls) do begin
    TFlagCtrl(FFlagCtrls[High(FFlagCtrls) - I]).SetValue((AValue shr I) and 1 <> 0);
  end;
end;

{ TFlagCtrl }

class procedure TFlagCtrl.Init;
begin
  TPaintCtrl.Init;
end;

class procedure TFlagCtrl.Final;
begin
  TPaintCtrl.Final;
end;

constructor TFlagCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPaintCtrl := TPaintCtrl.Create(Self);
  FPaintCtrl.FValue := False;
  FPaintCtrl.ParentColor := True;
  //FPaintCtrl.Color := clWhite;

  Lab := TLabel.Create(Self);
  Lab.ShowAccelChar := False;
  Lab.Caption := '';

  FPaintCtrl.AutoSize := False;
  FPaintCtrl.Width := 12;
  FPaintCtrl.Height := FPaintCtrl.Width;

  FPaintCtrl.Parent := Self;
  Lab.Parent := Self;

  FLabelLeft := True;
  SetLabelLeft(False);

  Constraints.MinWidth := 26;
  AutoSize := True;

  TabStop := False;
end;

procedure TFlagCtrl.SetValue(const AValue: Boolean);
begin
  if AValue xor FPaintCtrl.FValue then begin
    FPaintCtrl.FValue := AValue;
    FPaintCtrl.Invalidate;
  end;
end;

procedure TFlagCtrl.SetLabelLeft(const AValue: Boolean);
begin
  if FLabelLeft xor AValue then begin
    FLabelLeft := AValue;
    Lab.Anchors := [];
    FPaintCtrl.Anchors := [];
    if AValue then begin
      FPaintCtrl.AnchorVerticalCenterTo(Self);
      FPaintCtrl.AnchorParallel(akRight, 0, Self);
      Lab.AnchorVerticalCenterTo(Self);
      Lab.AnchorToNeighbour(akRight, 2, FPaintCtrl);
    end else begin
      FPaintCtrl.AnchorHorizontalCenterTo(Self);
      FPaintCtrl.AnchorParallel(akTop, 0, Self);
      Lab.AnchorHorizontalCenterTo(Self);
      Lab.AnchorToNeighbour(akTop, 0, FPaintCtrl);
    end;
    FPaintCtrl.BorderSpacing.Around := 1;
    Lab.BorderSpacing.Around := 1;
  end;
end;

procedure TFlagCtrl.SetLabel(const ACaption: AnsiString);
begin
  Lab.Caption := ACaption;
  Lab.Visible := ACaption <> '';
end;

{ TDebugCpuState }

constructor TDebugCpuState.Create(AOwner: TComponent);
const
  SpcRegs = 3;

begin
  inherited Create(AOwner);

  FrAF := TFrameWordDisplay.MakeNew('AF:');
  FrBC := TFrameWordDisplay.MakeNew('BC:');
  FrDE := TFrameWordDisplay.MakeNew('DE:');
  FrHL := TFrameWordDisplay.MakeNew('HL:');
  FrAF1 := TFrameWordDisplay.MakeNew('AF'':');
  FrBC1 := TFrameWordDisplay.MakeNew('BC'':');
  FrDE1 := TFrameWordDisplay.MakeNew('DE'':');
  FrHL1 := TFrameWordDisplay.MakeNew('HL'':');
  FrIx := TFrameWordDisplay.MakeNew('IX:');
  FrIy := TFrameWordDisplay.MakeNew('IY:');
  FrWZ := TFrameWordDisplay.MakeNew('WZ:');

  FrI := TFrameWordDisplay.MakeNew('I:');
  FrI.ByteMode := True;
  FrR := TFrameWordDisplay.MakeNew('R:');
  FrR.ByteMode := True;

  FrSP := TFrameWordDisplay.MakeNew('SP:');
  FrPC := TFrameWordDisplay.MakeNew('PC:');

  RegistersContainer := TCustomControl.Create(nil);

  FrAF.AnchorParallel(akLeft, 0, RegistersContainer);
  FrAF.AnchorParallel(akTop, 0, RegistersContainer);

  FrBC.AnchorParallel(akLeft, 0, FrAF);
  FrDE.AnchorParallel(akLeft, 0, FrAF);
  FrHL.AnchorParallel(akLeft, 0, FrAF);
  FrIx.AnchorParallel(akLeft, 0, FrAF);
  FrPC.AnchorParallel(akLeft, 0, FrAF);

  FrBC.AnchorToNeighbour(akTop, SpcRegs, FrAF);
  FrDE.AnchorToNeighbour(akTop, SpcRegs, FrBC);
  FrHL.AnchorToNeighbour(akTop, SpcRegs, FrDE);
  FrIx.AnchorToNeighbour(akTop, SpcRegs, FrHL);
  FrPC.AnchorToNeighbour(akTop, SpcRegs, FrIx);

  FrAF1.AnchorToNeighbour(akLeft, SpcRegs, FrAF);
  FrBC1.AnchorParallel(akLeft, 0, FrAF1);
  FrDE1.AnchorParallel(akLeft, 0, FrAF1);
  FrHL1.AnchorParallel(akLeft, 0, FrAF1);
  FrIy.AnchorParallel(akLeft, 0, FrAF1);
  FrSP.AnchorParallel(akLeft, 0, FrAF1);

  FrAF1.AnchorParallel(akTop, 0, FrAF);
  FrBC1.AnchorToNeighbour(akTop, SpcRegs, FrAF1);
  FrDE1.AnchorToNeighbour(akTop, SpcRegs, FrBC1);
  FrHL1.AnchorToNeighbour(akTop, SpcRegs, FrDE1);
  FrIy.AnchorToNeighbour(akTop, SpcRegs, FrHL1);
  FrSP.AnchorToNeighbour(akTop, SpcRegs, FrIy);

  FrI.AnchorParallel(akLeft, 0, FrAF);
  FrI.AnchorToNeighbour(akTop, SpcRegs, FrPC);

  FrR.AnchorParallel(akLeft, 0, FrI);
  FrR.AnchorToNeighbour(akTop, SpcRegs, FrI);

  FrWZ.AnchorParallel(akLeft, 0, FrAF);
  FrWZ.AnchorToNeighbour(akTop, SpcRegs, FrR);

  FrAF.Parent := RegistersContainer;
  FrBC.Parent := RegistersContainer;
  FrDE.Parent := RegistersContainer;
  FrHL.Parent := RegistersContainer;
  FrAF1.Parent := RegistersContainer;
  FrBC1.Parent := RegistersContainer;
  FrDE1.Parent := RegistersContainer;
  FrHL1.Parent := RegistersContainer;
  FrIx.Parent := RegistersContainer;
  FrIy.Parent := RegistersContainer;
  FrI.Parent := RegistersContainer;
  FrR.Parent := RegistersContainer;
  FrPC.Parent := RegistersContainer;
  FrSP.Parent := RegistersContainer;
  FrWZ.Parent := RegistersContainer;

  FlagsContainer := TCustomControl.Create(nil);
  FlCtrlsFlags := TFlagsCtrl.Create(nil, ['S', 'Z', '5', 'H', '3', 'PV', 'N', 'C'], False, False);

  FlCtrlsIff := TFlagsCtrl.Create(nil, ['Iff1', 'Iff2'], True, False);

  FlCtrlsInterruptPin := TFlagsCtrl.Create(nil, ['interrupt pin'], True, False);
  FlCtrlsHalt := TFlagsCtrl.Create(nil, ['halt'], True, False);

  FlCtrlsInterruptPin.Anchors := [];
  FlCtrlsHalt.Anchors := [];
  FlCtrlsIff.Anchors := [];
  FlCtrlsFlags.Anchors := [];

  FlCtrlsFlags.AnchorParallel(akLeft, 0, FlagsContainer);
  FlCtrlsFlags.AnchorParallel(akTop, 0, FlagsContainer);
  FlCtrlsIff.AnchorParallel(akLeft, 0, FlagsContainer);
  FlCtrlsIff.AnchorToNeighbour(akTop, 3, FlCtrlsFlags);

  FlCtrlsHalt.AnchorToNeighbour(akLeft, SpcRegs, FlCtrlsIff);
  FlCtrlsHalt.AnchorParallel(akTop, 0, FlCtrlsIff);

  FlCtrlsInterruptPin.AnchorToNeighbour(akLeft, SpcRegs, FlCtrlsHalt);
  FlCtrlsInterruptPin.AnchorParallel(akTop, 0, FlCtrlsIff);

  FlCtrlsFlags.Parent := FlagsContainer;
  FlCtrlsIff.Parent := FlagsContainer;

  FlCtrlsInterruptPin.Parent := FlagsContainer;
  FlCtrlsHalt.Parent := FlagsContainer;

  InterruptModeCtrl := TCustomControl.Create(nil);
  LabInterruptMode := TLabel.Create(nil);
  LabInterruptModeValue := TLabel.Create(nil);

  InterruptModeCtrl.BorderStyle := bsSingle;
  InterruptModeCtrl.AnchorParallel(akLeft, 0, FrAF1);
  InterruptModeCtrl.AnchorParallel(akTop, 0, FrI);

  InterruptModeCtrl.Hint := 'Interrupt mode (0, 1 or 2)';
  InterruptModeCtrl.ShowHint := True;
  LabInterruptMode.Caption := 'IM:';
  LabInterruptModeValue.Font := TCommonFunctionsLCL.GetMonoFont();
  LabInterruptModeValue.Font.Style := LabInterruptModeValue.Font.Style + [fsBold];

  LabInterruptModeValue.Caption := ' ';
  LabInterruptMode.AnchorParallel(akLeft, 0, InterruptModeCtrl);
  LabInterruptMode.AnchorParallel(akBottom, 0, InterruptModeCtrl);
  LabInterruptMode.Layout := TTextLayout.tlBottom;
  LabInterruptModeValue.Layout := TTextLayout.tlBottom;
  LabInterruptModeValue.AnchorToNeighbour(akLeft, 2, LabInterruptMode);
  LabInterruptModeValue.AnchorParallel(akBottom, 0, InterruptModeCtrl);

  LabInterruptMode.BorderSpacing.Around := 1;
  LabInterruptModeValue.BorderSpacing.Around := 1;

  LabInterruptMode.Parent := InterruptModeCtrl;
  LabInterruptModeValue.Parent := InterruptModeCtrl;
  InterruptModeCtrl.Parent := RegistersContainer;
  InterruptModeCtrl.AutoSize := True;
  InterruptModeCtrl.Color := FlCtrlsHalt.Color;

  FlagsContainer.AnchorToNeighbour(akLeft, SpcRegs, InterruptModeCtrl);
  FlagsContainer.AnchorParallel(akTop, 0, InterruptModeCtrl);

  FlagsContainer.AutoSize := True;

  RegistersContainer.AnchorParallel(akTop, SpcRegs, Self);
  RegistersContainer.AnchorParallel(akLeft, SpcRegs, Self);
  RegistersContainer.AutoSize := True;

  FlagsContainer.Parent := RegistersContainer;
  RegistersContainer.Parent := Self;

  AutoSize := True;
end;

destructor TDebugCpuState.Destroy;
begin
  FrAF.Free;
  FrBC.Free;
  FrDE.Free;
  FrHL.Free;
  FrAF1.Free;
  FrBC1.Free;
  FrDE1.Free;
  FrHL1.Free;
  FrIx.Free;
  FrIy.Free;
  FrI.Free;
  FrR.Free;
  FrPC.Free;
  FrSP.Free;
  FrWZ.Free;

  RegistersContainer.Free;

  InterruptModeCtrl.Free;
  LabInterruptMode.Free;
  LabInterruptModeValue.Free;

  FlCtrlsHalt.Free;
  FlCtrlsInterruptPin.Free;
  FlCtrlsFlags.Free;
  FlCtrlsIff.Free;

  FlagsContainer.Free;

  inherited Destroy;
end;

procedure TDebugCpuState.SetAll(const AProc: TProcessor);
var
  B: Byte;
begin
  if Assigned(AProc) then begin
    FrSP.Value := AProc.RegSP;
    FrPC.Value := AProc.RegPC;
    FrI.Value := AProc.RegI;
    FrR.Value := AProc.RegR;
    FrWZ.Value := AProc.RegWZ;
    FrIx.Value := AProc.Ix;
    FrIy.Value := AProc.Iy;
    FrAF.Value := AProc.RegAF;
    FrBC.Value := AProc.RegBC;
    FrDE.Value := AProc.RegDE;
    FrHL.Value := AProc.RegHL;
    FrAF1.Value := AProc.RegAF1;
    FrBC1.Value := AProc.RegBC1;
    FrDE1.Value := AProc.RegDE1;
    FrHL1.Value := AProc.RegHL1;

    FlCtrlsFlags.SetValues(AProc.RegF);
    if AProc.Iff1 then
      B := 2
    else
      B := 0;
    if AProc.Iff2 then
      B := B + 1;
    FlCtrlsIff.SetValues(B);

    if AProc.Halt then
      B := 1
    else
      B := 0;
    FlCtrlsHalt.SetValues(B);

    if AProc.IntPin then
      B := 1
    else
      B := 0;
    FlCtrlsInterruptPin.SetValues(B);

    LabInterruptModeValue.Caption := AProc.InterruptMode.ToString;
  end;
end;

initialization
  TFlagCtrl.Init;

finalization
  TFlagCtrl.Final;

end.
