unit UnitFormDebug;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitDisassembler, Z80Processor,
  UnitFrameGridMemory, CommonFunctionsLCL, UnitDebugCpuState, UnitConfigs,
  UnitDebugger, UnitFrameBreakpoints, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, fpjson;

type
  TFormDebug = class (TForm, TSpectrum.IFormDebug)
    Label1: TLabel;
    Label2: TLabel;
    LabelTicksInCurrentFrame: TLabel;
    LabelFramesPassed: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  strict private
    const
      cSectionDebugger: RawByteString = 'debugger';

  strict private
    FSpectrum: TSpectrum;
    FrameGridMemory: TFrameGridMemory;
    FDebugCPUState: TDebugCpuState;
    FormBreakpoints: TCustomForm;

    procedure FormFirstShow(Sender: TObject);
    function GetDebugger: TDebugger;
    procedure AfterShow(Data: PtrInt);
    procedure LabelBreakpointsOnClick(Sender: TObject);
    procedure FormBreakpointsBeforeDestruction(Sender: TObject);
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;

  public
    procedure SetSpectrum(ASpectrum: TSpectrum);
    procedure OnStep(out DoContinue: Boolean);
    procedure AfterStep;
    procedure LoadFromJson();
    procedure SaveToJson();
  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.lfm}

{ TFormDebug }

procedure TFormDebug.FormCreate(Sender: TObject);
var
  Lab: TCustomLabel;
begin
  FSpectrum := nil;
  FormBreakpoints := nil;

  Panel1.Caption := '';
  Panel2.Caption := '';
  Panel3.Caption := '';
  Panel3.BevelOuter := bvNone;
  Panel5.Caption := '';
  Panel5.BevelOuter := bvNone;
  Panel4.Caption := '';
  Panel4.BevelOuter := bvNone;
  Panel4.BorderStyle := bsSingle;
  Panel4.AutoSize := True;

  Panel6.Caption := '';
  Panel6.BevelOuter := bvNone;
  Panel7.Caption := '';
  Panel7.BevelOuter := bvNone;
  Panel7.BorderStyle := bsSingle;
  Panel7.AutoSize := True;

  Panel2.Anchors := [];
  Panel3.Anchors := [];

  Panel3.AnchorParallel(akLeft, 0, Panel1);
  Panel3.AnchorParallel(akTop, 0, Panel1);

  Panel2.AnchorParallel(akLeft, 0, Panel1);
  Panel2.AnchorToNeighbour(akTop, 0, Panel3);
  Panel2.AnchorParallel(akBottom, 0, Panel1);
  Panel2.AnchorParallel(akRight, 0, Panel1);

  FrameGridMemory := TFrameGridMemory.Create(Self);

  FrameGridMemory.AnchorParallel(akLeft, 0, Panel2);
  FrameGridMemory.AnchorParallel(akTop, 0, Panel2);
  FrameGridMemory.AnchorParallel(akRight, 0, Panel2);
  FrameGridMemory.AnchorParallel(akBottom, 0, Panel2);

  FDebugCPUState := TDebugCpuState.Create(Self);
  FDebugCPUState.Anchors := [];
  FDebugCPUState.AnchorParallel(akTop, 0, Panel3);
  FDebugCPUState.AnchorParallel(akLeft, 0, Panel3);

  Panel4.Anchors := [];
  Panel4.AnchorParallel(akLeft, 3, Panel3);
  Panel4.AnchorToNeighbour(akTop, 3, FDebugCPUState);
  Panel7.Anchors := [];
  Panel7.AnchorToNeighbour(akLeft, 3, Panel4);
  Panel7.AnchorToNeighbour(akTop, 3, FDebugCPUState);

  FDebugCPUState.Parent := Panel3;
  FrameGridMemory.Parent := Panel2;

  Panel3.AutoSize := True;

  Lab := TCommonFunctionsLCL.CreateLinkLabel(Panel3, 'Edit breakpoints');
  Lab.OnClick := @LabelBreakpointsOnClick;
  Lab.Anchors := [];

  Lab.AnchorParallel(akRight, 9, Panel3);
  Lab.AnchorParallel(akTop, 0, Panel7);
  Lab.Parent := Panel3;

  TCommonFunctionsLCL.GrowFormHeight(Self);
  Self.Constraints.MinHeight := Self.Height;

  LabelFramesPassed.Caption := ' ';
  LabelTicksInCurrentFrame.Caption := ' ';

  Panel4.Hint := 'Frames passed since Spectrum was switched on'
    + LineEnding + '(application start or last hard reset)';
  Panel4.ShowHint := True;

  Panel7.Hint := 'Clock ticks passed since last interrupt';
  Panel7.ShowHint := True;

  AfterShow(-1);
  AddHandlerFirstShow(@FormFirstShow);
end;

procedure TFormDebug.FormDestroy(Sender: TObject);
begin
  RemoveAllHandlersOfObject(Self);
  Application.RemoveAsyncCalls(Self);

  SetSpectrum(nil);
  if Assigned(FrameGridMemory) then begin
    FreeAndNil(FrameGridMemory);
  end;

end;

procedure TFormDebug.LoadFromJson;
var
  JObj: TJSONObject;
begin
  JObj := TConfJSON.GetJSONObject(cSectionDebugger);
  if Assigned(JObj) then begin
    FrameGridMemory.LoadFromJSON(JObj);
  end;
end;

procedure TFormDebug.SaveToJson;
var
  JObj: TJSONObject;
begin
  if Assigned(FrameGridMemory) then
    if FrameGridMemory.SaveToJSON(JObj) then
      if Assigned(JObj) then
        try
          TConfJSON.RemoveSection(cSectionDebugger);
          if TConfJSON.AddToConf(cSectionDebugger, JObj) then
            JObj := nil;

        finally
          JObj.Free;
        end;

end;

procedure TFormDebug.FormFirstShow(Sender: TObject);
begin
  RemoveHandlerFirstShow(@FormFirstShow);
  AfterShow(2);
end;

function TFormDebug.GetDebugger: TDebugger;
begin
  Result := FrameGridMemory.Debugger;
end;

procedure TFormDebug.AfterShow(Data: PtrInt);
begin
  TCommonFunctionsLCL.AdjustFormPos(Self);

  AutoSize := False;
  if Data > 0 then begin
    AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end else if Data = 0 then begin
    Constraints.MinHeight := 0;
  end;
end;

procedure TFormDebug.LabelBreakpointsOnClick(Sender: TObject);
begin
  if FormBreakpoints = nil then begin
    FormBreakpoints := TFrameBreakpoints.ShowFormBreakpoints(Self, GetDebugger, True);
    FormBreakpoints.AddHandlerOnBeforeDestruction(@FormBreakpointsBeforeDestruction);
  end else
    FormBreakpoints.BringToFront;
end;

procedure TFormDebug.FormBreakpointsBeforeDestruction(Sender: TObject);
begin
  if Assigned(FormBreakpoints) then begin
    FormBreakpoints.RemoveAllHandlersOfObject(Self);
    FormBreakpoints := nil;
  end;
end;

procedure TFormDebug.DoClose(var CloseAction: TCloseAction);
var
  D: TDebugger;
begin
  Application.RemoveAllHandlersOfObject(Self);
  Application.RemoveAsyncCalls(Self);
  D := GetDebugger;
  if Assigned(D) then
    D.RemoveOnBreakPointChangeHandlerOfObject(Self);
  FSpectrum.DettachFormDebug;
  CloseAction := caFree;

  inherited DoClose(CloseAction);
end;

procedure TFormDebug.SetSpectrum(ASpectrum: TSpectrum);
var
  D: TDebugger;
begin
  if ASpectrum <> FSpectrum then begin
    D := nil;
    if Assigned(FSpectrum) then
      FSpectrum.DettachFormDebug;

    if ASpectrum = nil then begin
      SaveToJson();
    end else if Assigned(ASpectrum.Memory) then begin
      if ASpectrum.GetDebugger is TDebugger then begin
        D := TDebugger(ASpectrum.GetDebugger);
      end;
    end;

    FSpectrum := ASpectrum;

    FrameGridMemory.Debugger := D;

    if Assigned(D) then
      LoadFromJson();

    AfterStep;
  end;
end;

procedure TFormDebug.OnStep(out DoContinue: Boolean);
begin
  DoContinue := False;
  if Assigned(FrameGridMemory) then begin
    FrameGridMemory.OnStep(DoContinue);
  end;
end;

procedure TFormDebug.AfterStep;
begin
  if Assigned(FrameGridMemory) then
    if Assigned(FSpectrum) then begin
      FrameGridMemory.Sp := FSpectrum.GetProcessor.RegSP;
      FrameGridMemory.Pc := FSpectrum.GetProcessor.RegPC;
      FDebugCPUState.SetAll(FSpectrum.GetProcessor);
      LabelFramesPassed.Caption := FSpectrum.GetFrameCount.ToString;
      LabelTicksInCurrentFrame.Caption := FSpectrum.GetProcessor.TStatesInCurrentFrame.ToString;
      FrameGridMemory.AfterStep;
    end;
end;

end.

