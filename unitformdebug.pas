unit UnitFormDebug;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitDisassembler, Z80Processor,
  UnitFrameGridMemory, CommonFunctionsLCL, UnitDebugCpuState, UnitConfigs,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, fpjson;

type
  TFormDebug = class (TForm, TSpectrum.IDebugger)
    Label1: TLabel;
    LabelFramesPassed: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  strict private
    const
      cSectionDebugger: RawByteString = 'debugger';

  private
    FSpectrum: TSpectrum;
    FrameGridMemory: TFrameGridMemory;
    FDisassembler: TDisassembler;
    FDebugCPUState: TDebugCpuState;

    procedure FormFirstShow(Sender: TObject);

    procedure AfterShow(Data: PtrInt);
  protected
    procedure DoClose(var CloseAction: TCloseAction); override;

  public
    procedure SetSpectrum(ASpectrum: TSpectrum);
    procedure OnStep(out DoContinue: Boolean);
    procedure AfterStep;
    procedure LoadFromJson();
    procedure SaveToJson();
    function CheckBreakPoints: Boolean;

  end;

var
  FormDebug: TFormDebug;

implementation

{$R *.lfm}

{ TFormDebug }

procedure TFormDebug.FormCreate(Sender: TObject);
begin
  FSpectrum := nil;
  FDisassembler := nil;

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

  Panel2.Anchors := [];
  Panel3.Anchors := [];

  Panel3.AnchorParallel(akLeft, 0, Panel1);
  Panel3.AnchorParallel(akTop, 0, Panel1);

  Panel2.AnchorParallel(akRight, 0, Panel1);
  Panel2.AnchorParallel(akTop, 0, Panel1);
  Panel2.AnchorParallel(akBottom, 0, Panel1);
  Panel2.AnchorToNeighbour(akLeft, 0, Panel3);

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

  FDebugCPUState.Parent := Panel3;
  FrameGridMemory.Parent := Panel2;

  Panel3.AutoSize := True;

  TCommonFunctionsLCL.GrowFormHeight(Self);
  Self.Constraints.MinHeight := Self.Height;

  LabelFramesPassed.Caption := ' ';

  Panel4.Hint := 'Frames passed since Spectrum was switched on'
    + LineEnding + '(application start or last hard reset)';
  Panel4.ShowHint := True;

  AfterShow(-1);
  AddHandlerFirstShow(@FormFirstShow);
end;

procedure TFormDebug.FormDestroy(Sender: TObject);
begin
  RemoveAllHandlersOfObject(Self);
  Application.RemoveAsyncCalls(Self);

  SaveToJson();

  if Assigned(FSpectrum) then
    FSpectrum.DettachDebugger;
  SetSpectrum(nil);
  if Assigned(FrameGridMemory) then begin
    FrameGridMemory.SetDisasembler(nil);
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

procedure TFormDebug.DoClose(var CloseAction: TCloseAction);
begin
  Application.RemoveAllHandlersOfObject(Self);
  Application.RemoveAsyncCalls(Self);
  FSpectrum.DettachDebugger;
  CloseAction := caFree;
  inherited DoClose(CloseAction);
end;

procedure TFormDebug.SetSpectrum(ASpectrum: TSpectrum);
begin
  if ASpectrum <> FSpectrum then begin
    if Assigned(ASpectrum) and Assigned(ASpectrum.Memory) then begin
      FDisassembler := TDisassembler.Create(ASpectrum.Memory);
    end else
      FreeAndNil(FDisassembler);

    FSpectrum := ASpectrum;

    FrameGridMemory.SetDisasembler(FDisassembler);

    if Assigned(FDisassembler) then
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
      FrameGridMemory.AfterStep;
    end;
end;

function TFormDebug.CheckBreakPoints: Boolean;
begin
  Result := False;
end;

end.

