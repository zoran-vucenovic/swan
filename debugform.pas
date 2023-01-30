unit DebugForm;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Grids, ActnList, UnitFrameWordDisplay, UnitSpectrum, Z80Processor,
  UnitMemory, FastIntegers, CommonFunctionsLCL, Types, fgl;

type

  TFormDebug = class(TForm, TSpectrum.IDebugger)
    ActionUpdate: TAction;
    ActionAddAddressBreakPoint: TAction;
    ActionStep: TAction;
    ActionGotoPC: TAction;
    ActionGotoSP: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape0: TShape;
    Shape8: TShape;
    Shape9: TShape;
    StringGrid1: TStringGrid;

    procedure ActionAddAddressBreakPointExecute(Sender: TObject);
    procedure ActionGotoPCExecute(Sender: TObject);
    procedure ActionGotoSPExecute(Sender: TObject);
    procedure ActionStepExecute(Sender: TObject);
    procedure ActionUpdateExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; {%H-}aState: TGridDrawState);
    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      {%H-}aState: TGridDrawState);

  private
    type
      TBreakPoint = class abstract (TObject)
      private
        Enabled: Boolean;
      protected
        //function CheckIfReached: Boolean; virtual; abstract;
      public
        constructor Create; //virtual;
      end;

      TBreakPointAdress = class(TBreakPoint)
      private
        Address: Word;
      protected
        //function CheckIfReached: Boolean; override;
      end;

      TBreakPointTicksInFrame = class(TBreakPoint)
      private
        TicksToStop: Int32Fast;
      end;

      TBreakPointList = class(specialize TFPGList<TBreakPoint>)
      public
      //
      end;

      TAddressBreakPoints = class(specialize TFPGMapObject<Word, TBreakPoint>)
      public
        constructor Create;
      end;

      TAccessStringGrid = class(TStringGrid);

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

    //RegAF: Word;
    //RegBC: Word;
    //RegDE: Word;
    //RegHL: Word;
    //RegAF1: Word;
    //RegBC1: Word;
    //RegDE1: Word;
    //RegHL1: Word;
    //Ix: Word;
    //Iy: Word;
    ProgrCounter: Word;
    StackPointer: Word;
    //SP: Word;
    //RegIR: Word;
    //
    FSpectrum: TSpectrum;
    FOneStep: Boolean;

    //function GetRegA: Byte;
    //function GetRegA1: Byte;
    //function GetRegB: Byte;
    //function GetRegB1: Byte;
    //function GetRegC: Byte;
    //function GetRegC1: Byte;
    //function GetRegD: Byte;
    //function GetRegD1: Byte;
    //function GetRegE: Byte;
    //function GetRegE1: Byte;
    //function GetRegF: Byte;
    //function GetRegF1: Byte;
    //function GetRegH: Byte;
    //function GetRegH1: Byte;
    //function GetRegI: Byte;
    //function GetRegL: Byte;
    //function GetRegL1: Byte;
    //function GetRegR: Byte;
    //procedure SetRegA(AValue: Byte);
    //procedure SetRegA1(AValue: Byte);
    //procedure SetRegB(AValue: Byte);
    //procedure SetRegB1(AValue: Byte);
    //procedure SetRegC(AValue: Byte);
    //procedure SetRegC1(AValue: Byte);
    //procedure SetRegD(AValue: Byte);
    //procedure SetRegD1(AValue: Byte);
    //procedure SetRegE(AValue: Byte);
    //procedure SetRegE1(AValue: Byte);
    //procedure SetRegF(AValue: Byte);
    //procedure SetRegF1(AValue: Byte);
    //procedure SetRegH(AValue: Byte);
    //procedure SetRegH1(AValue: Byte);
    //procedure SetRegI(AValue: Byte);
    //procedure SetRegL(AValue: Byte);
    //procedure SetRegL1(AValue: Byte);
    //procedure SetRegR(AValue: Byte);

  private
    //property RegA: Byte read GetRegA write SetRegA;
    //property RegF: Byte read GetRegF write SetRegF;
    //property RegB: Byte read GetRegB write SetRegB;
    //property RegC: Byte read GetRegC write SetRegC;
    //property RegD: Byte read GetRegD write SetRegD;
    //property RegE: Byte read GetRegE write SetRegE;
    //property RegH: Byte read GetRegH write SetRegH;
    //property RegL: Byte read GetRegL write SetRegL;
    //
    //property RegA1: Byte read GetRegA1 write SetRegA1;
    //property RegF1: Byte read GetRegF1 write SetRegF1;
    //property RegB1: Byte read GetRegB1 write SetRegB1;
    //property RegC1: Byte read GetRegC1 write SetRegC1;
    //property RegD1: Byte read GetRegD1 write SetRegD1;
    //property RegE1: Byte read GetRegE1 write SetRegE1;
    //property RegH1: Byte read GetRegH1 write SetRegH1;
    //property RegL1: Byte read GetRegL1 write SetRegL1;
    //
    //property RegR: Byte read GetRegR write SetRegR;
    //property RegI: Byte read GetRegI write SetRegI;
    //

    FAddressBreakPoints: TAddressBreakPoints;

    ShapeArray: array [0..7] of TShape;

    function FillRegisters: Boolean;
    procedure AddAdressBreakPoint(const Address: Word);
    procedure UpdateValues;
    procedure FocusAddressInGrid(const Address: Word);
    procedure GotoPC;

  public
    procedure SetSpectrum(Spectrum: TSpectrum);
    procedure OnStep(out DoContinue: Boolean);
    procedure AfterStep;
    function CheckBreakPoints: Boolean;
  end;

implementation

{$R *.lfm}

{ TFormDebug.TBreakPoint }

constructor TFormDebug.TBreakPoint.Create;
begin
  inherited Create;
  Enabled := True;
end;

{ TFormDebug.TAddressBreakPoints }

constructor TFormDebug.TAddressBreakPoints.Create;
begin
  inherited Create(True);

  Duplicates := dupIgnore;
  Sorted := True;
end;

{ TFormDebug }

procedure TFormDebug.FormCreate(Sender: TObject);
const
  SpcRegs = 3;

begin
  FSpectrum := nil;

  CheckBox1.Checked := True;

  ShapeArray[0] := Shape0;
  ShapeArray[1] := Shape1;
  ShapeArray[2] := Shape2;
  ShapeArray[3] := Shape3;
  ShapeArray[4] := Shape4;
  ShapeArray[5] := Shape5;
  ShapeArray[6] := Shape6;
  ShapeArray[7] := Shape7;

  Panel2.BevelInner := bvNone;
  Panel2.BevelOuter := bvNone;
  Panel2.Caption := '';
  Panel3.BevelInner := bvNone;
  Panel3.BevelOuter := bvNone;
  Panel3.Caption := '';
  Panel4.BevelInner := bvNone;
  Panel4.BevelOuter := bvNone;
  Panel4.Caption := '';
  Panel5.BevelInner := bvNone;
  Panel5.BevelOuter := bvNone;
  Panel5.Caption := '';
  Panel6.BevelInner := bvNone;
  Panel6.BevelOuter := bvNone;
  Panel6.Caption := '';

  FrAF := TFrameWordDisplay.MakeNew('AF:');
  FrBC := TFrameWordDisplay.MakeNew('BC:');
  FrDE := TFrameWordDisplay.MakeNew('DE:');
  FrHL := TFrameWordDisplay.MakeNew('HL:');
  FrAF1 := TFrameWordDisplay.MakeNew('AF'':');
  FrBC1 := TFrameWordDisplay.MakeNew('BC'':');
  FrDE1 := TFrameWordDisplay.MakeNew('DE'':');
  FrHL1 := TFrameWordDisplay.MakeNew('HL'':');
  FrIx := TFrameWordDisplay.MakeNew('Ix:');
  FrIy := TFrameWordDisplay.MakeNew('Iy:');
  FrWZ := TFrameWordDisplay.MakeNew('WZ:');

  FrI := TFrameWordDisplay.MakeNew('I:');
  FrI.ByteMode := True;
  FrR := TFrameWordDisplay.MakeNew('R:');
  FrR.ByteMode := True;

  FrSP := TFrameWordDisplay.MakeNew('SP:');
  FrPC := TFrameWordDisplay.MakeNew('PC:');

  FrPC.AnchorParallel(akLeft, 0, Panel2);
  FrPC.AnchorParallel(akTop, 0, Panel2);
  FrSP.AnchorToNeighbour(akLeft, SpcRegs, FrPC);
  FrSP.AnchorParallel(akTop, 0, Panel2);

  FrAF.AnchorParallel(akLeft, 0, Panel2);
  FrBC.AnchorParallel(akLeft, 0, FrAF);
  FrDE.AnchorParallel(akLeft, 0, FrAF);
  FrHL.AnchorParallel(akLeft, 0, FrAF);
  FrIx.AnchorParallel(akLeft, 0, FrAF);

  //FrAF.AnchorParallel(akTop, 0, Panel2);
  FrAF.AnchorToNeighbour(akTop, SpcRegs + 2, FrPC);
  FrBC.AnchorToNeighbour(akTop, SpcRegs, FrAF);
  FrDE.AnchorToNeighbour(akTop, SpcRegs, FrBC);
  FrHL.AnchorToNeighbour(akTop, SpcRegs, FrDE);
  FrIx.AnchorToNeighbour(akTop, SpcRegs + 2, FrHL);

  FrAF1.AnchorToNeighbour(akLeft, SpcRegs, FrAF);
  FrBC1.AnchorParallel(akLeft, 0, FrAF1);
  FrDE1.AnchorParallel(akLeft, 0, FrAF1);
  FrHL1.AnchorParallel(akLeft, 0, FrAF1);
  FrIy.AnchorParallel(akLeft, 0, FrAF1);
  FrWZ.AnchorParallel(akLeft, 0, FrAF);

  //FrAF1.AnchorParallel(akTop, 0, Panel2);
  FrAF1.AnchorParallel(akTop, 0, FrAF);
  FrBC1.AnchorToNeighbour(akTop, SpcRegs, FrAF1);
  FrDE1.AnchorToNeighbour(akTop, SpcRegs, FrBC1);
  FrHL1.AnchorToNeighbour(akTop, SpcRegs, FrDE1);
  FrIy.AnchorToNeighbour(akTop, SpcRegs + 2, FrHL1);

  
  FrI.AnchorHorizontalCenterTo(FrIx);
  FrI.AnchorToNeighbour(akTop, SpcRegs + 2, FrIx);

  FrR.AnchorHorizontalCenterTo(FrI);
  FrR.AnchorToNeighbour(akTop, SpcRegs, FrI);
  FrWZ.AnchorToNeighbour(akTop, SpcRegs + 2, FrR);
//
//  FrPC.AnchorParallel(akLeft, 0, FrIy);
//  FrPC.AnchorToNeighbour(akTop, SpcRegs + 2, FrIy);
//  FrSP.AnchorParallel(akLeft, 0, FrPC);
//  FrSP.AnchorToNeighbour(akTop, SpcRegs, FrPC);
//
  FrAF.Parent := Panel2;
  FrBC.Parent := Panel2;
  FrDE.Parent := Panel2;
  FrHL.Parent := Panel2;
  FrAF1.Parent := Panel2;
  FrBC1.Parent := Panel2;
  FrDE1.Parent := Panel2;
  FrHL1.Parent := Panel2;
  FrIx.Parent := Panel2;
  FrIy.Parent := Panel2;
  FrI.Parent := Panel2;
  FrR.Parent := Panel2;
  FrPC.Parent := Panel2;
  FrSP.Parent := Panel2;
  FrWZ.Parent := Panel2;

  //Panel8.AnchorParallel(akLeft, 0, FrIy);
  Panel8.AnchorHorizontalCenterTo(FrIy);
  //Panel8.AnchorToNeighbour(akTop, SpcRegs + 2, FrSP);
  Panel8.AnchorToNeighbour(akTop, SpcRegs + 2, FrIy);

  Panel10.AnchorHorizontalCenterTo(Panel8);
  Panel10.AnchorToNeighbour(akTop, SpcRegs + 2, Panel8);

  Panel2.AutoSize := True;
  Panel3.AutoSize := True;

  FAddressBreakPoints := TAddressBreakPoints.Create;

end;

procedure TFormDebug.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  //CloseAction := TCloseAction.caHide;
  FSpectrum.DettachDebugger;
end;

procedure TFormDebug.ActionStepExecute(Sender: TObject);
begin
  FOneStep := True;
end;

procedure TFormDebug.ActionUpdateExecute(Sender: TObject);
begin
  UpdateValues;
end;

procedure TFormDebug.ActionGotoPCExecute(Sender: TObject);
begin
  GotoPC;
  if StringGrid1.CanSetFocus then
    StringGrid1.SetFocus;
end;

procedure TFormDebug.ActionAddAddressBreakPointExecute(Sender: TObject);
begin
  //
end;

procedure TFormDebug.ActionGotoSPExecute(Sender: TObject);
begin                                                        
  FocusAddressInGrid(StackPointer);
end;

procedure TFormDebug.FormDestroy(Sender: TObject);
//var
//  I: Integer;
begin
  //FAddressBreakPoints.Clear;

  FAddressBreakPoints.Free;

  FrWZ.Free;

  FrSP.Free;
  FrPC.Free;
  FrR.Free;
  FrI.Free;

  FrIy.Free;
  FrIx.Free;
  FrHL1.Free;
  FrDE1.Free;
  FrBC1.Free;
  FrAF1.Free;
  FrHL.Free;
  FrDE.Free;
  FrBC.Free;
  FrAF.Free;
end;

procedure TFormDebug.FormShow(Sender: TObject);
begin
  TCommonFunctionsLCL.AdjustFormPos(Self);
end;

procedure TFormDebug.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);            
var
  Gr: TCustomStringGrid;
  W: Word;
  S: String;
  R: TRect;
  TS: TTextStyle;

begin
  if Sender is TCustomStringGrid then begin
    Gr := TCustomStringGrid(Sender);

    if aCol < Gr.FixedCols then begin
      if aRow > StringGrid1.FixedRows then begin
        W := aRow - StringGrid1.FixedRows;

        if (W = ProgrCounter) or (W = StackPointer) then begin
          if W = ProgrCounter then begin
            if W = StackPointer then
              S := 'PS'
            else
              S := 'PC';
          end else
            S := 'SP';
          Gr.Canvas.Brush.Color := clYellow;
          //R := Gr.CellRect(aCol, aRow);
          TS := Gr.Canvas.TextStyle;
          TS.Alignment := taCenter;
          TS.Layout := tlCenter;
          TS.Opaque := True;

          R := Rect(aRect.Right - 2 - Canvas.TextWidth('WIW'), aRect.Top, aRect.Right - 2, aRect.Bottom);

          Gr.Canvas.Font.Style := Gr.Canvas.Font.Style + [fsBold];
          Gr.Canvas.Font.Color := clBlack;
          Gr.Canvas.TextRect(R, 0, 0, S, TS);
        end;

        if FAddressBreakPoints.IndexOf(W) >= 0 then begin
          Gr.Canvas.Brush.Color := clMaroon;
          Gr.Canvas.Font.Color := clYellow;
          R := Rect(aRect.Right - 5 - 2 * Canvas.TextWidth('WIW'), aRect.Top, aRect.Right, aRect.Bottom);
          R.Right := R.Left + Canvas.TextWidth('WIW');

          TS := Gr.Canvas.TextStyle;
          //TS.Alignment := taRightJustify;
          TS.Layout := tlCenter;
          TS.Opaque := True;

          Gr.Canvas.Font.Style := Gr.Canvas.Font.Style + [fsBold];
          Gr.Canvas.TextRect(R, 0, 0, 'B', TS);
        end;
      end;
    end;
  end;
end;

procedure TFormDebug.StringGrid1PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  Gr: TCustomStringGrid;
  N: Integer;
  Mem: PMemory;
  Proc: TProcessor;

begin
  if Sender is TCustomStringGrid then begin
    Proc := FSpectrum.GetProcessor;
    if Assigned(Proc) then begin
      Mem := Proc.GetMemory;
      Gr := TCustomStringGrid(Sender);

      if (aCol = Gr.FixedCols) and (aRow >= Gr.FixedRows) then begin
        N := aRow - Gr.FixedRows;

        if N < Mem^.RomSize then
          Gr.Canvas.Brush.Color := clSilver
        else if N < Mem^.RomSize + 32 * 192 then
          Gr.Canvas.Brush.Color := clLime
        else if N < Mem^.RomSize + 32 * 192 + 32 * 24 then
          Gr.Canvas.Brush.Color := clMoneyGreen;

      end;
    end;
  end;
end;

function TFormDebug.FillRegisters: Boolean;
var
  Proc: TProcessor;
  I: Integer;
begin
  Result := False;
  if Assigned(FSpectrum) then begin
    Proc := FSpectrum.GetProcessor;
    if Assigned(Proc) then begin
      FrAF.Value := Proc.RegAF;
      FrBC.Value := Proc.RegBC;
      FrDE.Value := Proc.RegDE;
      FrHL.Value := Proc.RegHL;
      FrAF1.Value := Proc.RegAF1;
      FrBC1.Value := Proc.RegBC1;
      FrDE1.Value := Proc.RegDE1;
      FrHL1.Value := Proc.RegHL1;

      FrIx.Value := Proc.Ix;
      FrIy.Value := Proc.Iy;
      FrI.Value := Proc.RegI;
      FrR.Value := Proc.RegR;

      FrSP.Value := Proc.RegSP;
      StackPointer := Proc.RegSP;
      FrPC.Value := Proc.RegPC;
      ProgrCounter := Proc.RegPC;

      FrWZ.Value := Proc.RegWZ;

      for I := 0 to 7 do begin
        if Proc.RegAF and (1 shl I) <> 0 then
          ShapeArray[I].Brush.Style := bsSolid
        else
          ShapeArray[I].Brush.Style := bsClear;
      end;

      if Proc.Iff1 then
        Shape8.Brush.Style := bsSolid
      else
        Shape8.Brush.Style := bsClear;

      if Proc.Iff2 then
        Shape9.Brush.Style := bsSolid
      else
        Shape9.Brush.Style := bsClear;

      Label12.Caption := 'IM: ' + IntToStr(Proc.InterruptMode);
      Label13.Caption := 'ticks: ' + IntToStr(Proc.TStatesInCurrentFrame);
      Label14.Caption := 'frames: ' + IntToStr(FSpectrum.GetFrameCount);

      Result := True;
    end;
  end;
end;

procedure TFormDebug.AddAdressBreakPoint(const Address: Word);
var
  B: TBreakPointAdress;
begin
  if FAddressBreakPoints.IndexOf(Address) < 0 then begin
    B := TBreakPointAdress.Create;
    B.Address := Address;
    FAddressBreakPoints.Add(Address, B);
  end;
end;

procedure TFormDebug.UpdateValues;
var
  I, N: Integer;
  Mem: PMemory;
  Proc: TProcessor;
  B: Byte;
begin
  if Assigned(FSpectrum) and FSpectrum.IsRunning then begin
    Proc := FSpectrum.GetProcessor;
    if Assigned(Proc) then begin
      Mem := Proc.GetMemory;

      if FillRegisters then begin
        StringGrid1.BeginUpdate;
        try
          StringGrid1.RowCount := StringGrid1.FixedRows + Mem^.MemSize;

          N := 0;
          for I := StringGrid1.FixedRows to StringGrid1.RowCount - 1 do begin
            StringGrid1.Cells[1, I] := IntToHex(N, 4) + ' (' + N.ToString.PadLeft(5, '0') + ')';
            B := Mem^.ReadByte(N);
            StringGrid1.Cells[2, I] := IntToHex(B, 2) + ' (' + B.ToString.PadLeft(3, '0') + ')';

            Inc(N);
          end;
        finally
          StringGrid1.EndUpdate;
        end;
      end;

    end;
  end;
end;

procedure TFormDebug.FocusAddressInGrid(const Address: Word);
begin
  TCommonFunctionsLCL.RowInView(StringGrid1, StringGrid1.FixedRows + Address);
end;

procedure TFormDebug.GotoPC;
begin
  FocusAddressInGrid(ProgrCounter);
end;

procedure TFormDebug.SetSpectrum(Spectrum: TSpectrum);
begin
  FSpectrum := Spectrum;

  StringGrid1.RowCount := StringGrid1.FixedRows;
  //FOneStep := True;
  StringGrid1.AutoAdjustColumns;
  AfterStep;
end;

procedure TFormDebug.OnStep(out DoContinue: Boolean);
var
  Proc: TProcessor;

begin
  DoContinue := False;

  if FOneStep then begin
    FOneStep := False;
    if Assigned(FSpectrum) then begin
      Proc := FSpectrum.GetProcessor;
      if Assigned(Proc) then begin
        DoContinue := True;
      end;
    end;
  end;
end;

procedure TFormDebug.AfterStep;
begin
  UpdateValues;
  
  if CheckBox1.Checked then
    GotoPC;
end;

function TFormDebug.CheckBreakPoints: Boolean;
begin
  Result := False;

  if FSpectrum.GetFrameCount >= 84 then
  begin
    if ((FSpectrum.GetProcessor.TStatesInCurrentFrame >= 59090 //59144 //990 //65858 // 65382 //47994 //23994 //11994 //5994 //3218 //3242
    )
       and (FSpectrum.GetProcessor.TStatesInCurrentFrame <= 59120))
    then
      Result := True;
  end;
end;

end.

