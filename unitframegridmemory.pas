unit UnitFrameGridMemory;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitGridSpectrumMemory, UnitDisassembler, UnitDebugger,
  Forms, Controls, ExtCtrls, StdCtrls, Buttons, fpjson;

type
  TFrameGridMemory = class(TFrame)
    BitBtn1: TBitBtn;
    ButtonRunStop: TButton;
    ButtonJumpToPC: TButton;
    ButtonJumpToSP: TButton;
    ButtonJumpGo: TButton;
    ButtonStep: TButton;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute: TCheckBox;
    CheckBoxFollowPC: TCheckBox;
    ComboBoxNumBase: TComboBox;
    ComboBoxHexFormat: TComboBox;
    EditAddr: TEdit;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    PanelOptions: TPanel;
    PanelNumBase: TPanel;
    PanelHexFormat: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure ButtonJumpGoClick(Sender: TObject);
    procedure ButtonJumpToPCClick(Sender: TObject);
    procedure ButtonJumpToSPClick(Sender: TObject);
    procedure ButtonStepClick(Sender: TObject);
  private
    const
      cSectionHexFormat: RawByteString = 'hex_format';
      cSectionNumBase: RawByteString = 'num_base';
      cSectionDisplayRelativeJumpOffsetAsAbsolute: RawByteString =
        'display_relative_jump_offset_as_absolute';

      cCheckBoxFollowPCHint: RawByteString =
        'When stepping through debugger, the focus follows the programme counter'
        ;
      cPanelNumBaseHint: RawByteString =
        'Numeric base in assembly instructions — hex, dec or bin.'
        + LineEnding + 'Note:'
        + LineEnding + '  This setting does not affect relative offset (signed 8-bit) numbers, such as d in ...(IX+d),'
        + LineEnding + '  as well as in JR d (the later unless "show relative jumps as absolute" option is chosen).'
        + LineEnding + '  These are always displayed as signed decimal.'
        ;
      cPanelHexFormatHint: RawByteString =
        'Hex number format in assembly instructions:'
        + LineEnding + 'You can choose among prefix%s or suffix%s.'
        + LineEnding + 'This makes difference only when hex num. format is chosen.'
        ;
      cCheckBoxDisplayRelativeJumpOffsetAsAbsoluteHint: RawByteString =
        'Relative offsets in JR and DJNZ instructions are displayed'
        + LineEnding + 'as absolute addresses of the destination.'
        ;
      cJumpToAddrHint: RawByteString = 'Jump to address'
        + LineEnding + 'To input address in hexadecimal, use $ as prefix.'
        ;
      cJumpToPC: RawByteString = 'Jump to programme counter';
      cJumpToSP: RawByteString = 'Jump to stack pointer';
      cStartHint: RawByteString = 'Continue running';
      cStopHint: RawByteString = 'Stop running';
      cStepHint: RawByteString = 'Step to next instruction';

  private
    FGrid: TSpectrumMemoryGrid;
    FOneStep: Boolean;
    EditAddrPrevValue: String;

    function GetActive: Boolean;
    function GetDisassembler: TDisassembler;
    function GetDebugger: TDebugger;
    function GetPc: Word;
    function GetSp: Word;
    procedure SetActive(AValue: Boolean);
    procedure SetDebugger(const AValue: TDebugger);
    procedure SetOnRunStop(AValue: TNotifyEvent);
    procedure SetPc(const AValue: Word);
    procedure SetSp(const AValue: Word);
    procedure CheckBoxFollowPcOnChange(Sender: TObject);
    procedure DisassemblerDisplayChange(Sender: TObject);
    procedure UpdateValuesFromDisassembler;
    function ValidateEditAddr: Integer;
    procedure ValidateEditAddrAndJumpTo();
    procedure EditAddrEditingDone(Sender: TObject);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure OnStep(out DoContinue: Boolean);
    procedure AfterStep;
    function SaveToJSON(out JSONObj: TJSONObject): Boolean;
    procedure LoadFromJSON(const JSONObj: TJSONObject);

    property Pc: Word read GetPc write SetPc;
    property Sp: Word read GetSp write SetSp;
    property Debugger: TDebugger read GetDebugger write SetDebugger;
    property Grid: TSpectrumMemoryGrid read FGrid;
    property IsActive: Boolean read GetActive write SetActive;
    property OnRunStop: TNotifyEvent write SetOnRunStop;
  end;

implementation

{$R *.lfm}

{ TFrameGridMemory }

procedure TFrameGridMemory.ButtonJumpToPCClick(Sender: TObject);
begin
  FGrid.JumpTo(FGrid.Pc);
end;

procedure TFrameGridMemory.ButtonJumpGoClick(Sender: TObject);
begin
  ValidateEditAddrAndJumpTo();
end;

procedure TFrameGridMemory.BitBtn1Click(Sender: TObject);
begin
  PanelOptions.Visible := not PanelOptions.Visible;
  if PanelOptions.Visible then
    BitBtn1.ImageIndex := 1
  else
    BitBtn1.ImageIndex := 0;
end;

procedure TFrameGridMemory.ButtonJumpToSPClick(Sender: TObject);
begin
  FGrid.JumpTo(FGrid.Sp);
end;

procedure TFrameGridMemory.ButtonStepClick(Sender: TObject);
begin
  FOneStep := True;
end;

function TFrameGridMemory.GetDisassembler: TDisassembler;
var
  D: TDebugger;
begin
  D := GetDebugger;
  if Assigned(D) then
    Result := D.Disassembler
  else
    Result := nil;
end;

function TFrameGridMemory.GetActive: Boolean;
begin
  Result := FGrid.Enabled;
end;

function TFrameGridMemory.GetDebugger: TDebugger;
begin
  if Assigned(FGrid) then
    Result := FGrid.Debugger
  else
    Result := nil;
end;

function TFrameGridMemory.GetPc: Word;
begin
  Result := FGrid.Pc;
end;

function TFrameGridMemory.GetSp: Word;
begin
  Result := FGrid.Sp;
end;

procedure TFrameGridMemory.SetActive(AValue: Boolean);
begin
  if AValue xor GetActive then begin
    PanelOptions.Enabled := AValue;
    Panel1.Enabled := AValue;
    if AValue then begin
      ButtonRunStop.Caption := 'Run';
      ButtonRunStop.Hint := cStartHint;
    end else begin
      ButtonRunStop.Caption := 'Stop';
      ButtonRunStop.Hint := cStopHint;
    end;
    FGrid.Enabled := AValue;
  end;
end;

procedure TFrameGridMemory.SetDebugger(const AValue: TDebugger);
begin
  if Assigned(FGrid) then begin
    FGrid.Debugger := AValue;
    UpdateValuesFromDisassembler;
  end;
end;

procedure TFrameGridMemory.SetOnRunStop(AValue: TNotifyEvent);
begin
  ButtonRunStop.OnClick := AValue;
end;

procedure TFrameGridMemory.SetPc(const AValue: Word);
begin
  FGrid.Pc := AValue;
end;

procedure TFrameGridMemory.SetSp(const AValue: Word);
begin
  FGrid.Sp := AValue;
end;

procedure TFrameGridMemory.CheckBoxFollowPcOnChange(Sender: TObject);
begin
  FGrid.FollowPc := CheckBoxFollowPC.Checked;
end;

procedure TFrameGridMemory.DisassemblerDisplayChange(Sender: TObject);
var
  D: TDisassembler;
begin
  D := GetDisassembler;
  if Assigned(D) then begin
    D.NumDisplay := TDisassembler.TNumDisplay(ComboBoxNumBase.ItemIndex);
    D.HexFormatIndex := ComboBoxHexFormat.ItemIndex;
    D.DisplayRelativeJumpOffsetAsAbsolute :=
      CheckBoxDisplayRelativeJumpOffsetAsAbsolute.Checked;
    FGrid.Invalidate;
  end;

end;

procedure TFrameGridMemory.UpdateValuesFromDisassembler;
var
  D: TDisassembler;
begin
  D := GetDisassembler;
  if Assigned(D) then begin
    ValidateEditAddr;
    ComboBoxNumBase.ItemIndex := Ord(D.NumDisplay);
    ComboBoxHexFormat.ItemIndex := D.HexFormatIndex;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute.OnChange := nil;
    CheckBoxFollowPC.OnChange := nil;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute.Checked := D.DisplayRelativeJumpOffsetAsAbsolute;
    CheckBoxFollowPC.Checked := FGrid.FollowPc;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute.OnChange := @DisassemblerDisplayChange;
    CheckBoxFollowPC.OnChange := @CheckBoxFollowPcOnChange;
  end;
end;

function TFrameGridMemory.ValidateEditAddr: Integer;

  function ValidateAddr(const S: AnsiString): Integer;
  var
    C: AnsiChar;
  begin
    C := PAnsiChar(S)^;
    if (C in ['%', '&']) // don't allow binary or octal
       or (not TryStrToInt(S, Result)) or (Result < 0) // allow decimal or hex
       or (Result >= FGrid.RowCount - FGrid.FixedRows)
    then
      Result := -1;
  end;

var
  S: AnsiString;
begin
  S := Trim(EditAddr.Text);
  Result := ValidateAddr(S);
  if Result < 0 then begin
    if ValidateAddr(EditAddrPrevValue) < 0 then
      S := '0'
    else
      S := EditAddrPrevValue;
  end;
  EditAddr.Text := S;
end;

procedure TFrameGridMemory.ValidateEditAddrAndJumpTo();
var
  N: Integer;
begin
  N := ValidateEditAddr;
  if N >= 0 then begin
    FGrid.JumpTo(N);
    EditAddrPrevValue := EditAddr.Text;
  end;
end;

procedure TFrameGridMemory.EditAddrEditingDone(Sender: TObject);
begin
  if EditAddr.Focused then
    ValidateEditAddrAndJumpTo();
end;

constructor TFrameGridMemory.Create(TheOwner: TComponent);
var
  Nd: TDisassembler.TNumDisplay;
  I: Integer;
  S, S1, S2: RawByteString;
begin
  inherited Create(TheOwner);

  SetOnRunStop(nil);
  FOneStep := False;
  EditAddrPrevValue := #0;
  EditAddr.Text := EditAddrPrevValue;

  Panel7.BevelOuter := bvNone;
  Panel6.BevelOuter := bvNone;

  Panel1.Caption := '';
  Panel1.BevelOuter := bvNone;

  Panel2.Caption := '';
  Panel2.BevelOuter := bvNone;

  PanelOptions.Caption := '';
  PanelOptions.BevelOuter := bvNone;

  FGrid := TSpectrumMemoryGrid.Create(nil);
  FGrid.AnchorParallel(akTop, 0, Panel2);
  FGrid.AnchorParallel(akLeft, 0, Panel2);
  FGrid.AnchorParallel(akRight, 0, Panel2);
  FGrid.AnchorParallel(akBottom, 0, Panel2);

  Panel1.AutoSize := True;

  CheckBoxFollowPC.Checked := FGrid.FollowPc;
  CheckBoxFollowPC.OnChange := @CheckBoxFollowPcOnChange;

  ComboBoxNumBase.Items.Clear;
  for Nd := Low(TDisassembler.TNumDisplay) to High(TDisassembler.TNumDisplay) do begin
    WriteStr(S, Nd);
    S := LowerCase(Copy(S, 3));
    ComboBoxNumBase.Items.Add(S);
  end;

  ComboBoxHexFormat.Items.Clear;
  S1 := '';
  for I := Low(TDisassembler.AllowedHexPrefixes) to High(TDisassembler.AllowedHexPrefixes) do begin
    S := Lowercase(Trim(TDisassembler.AllowedHexPrefixes[I]));
    ComboBoxHexFormat.Items.Add(S + '...');
    if I > 0 then begin
      if I = 1 then
        S1 := 'es' + S1;
      S1 := S1 + ',';
    end;
    S1 := S1 + ' "' + S + '"';
  end;

  S2 := '';
  for I := Low(TDisassembler.AllowedHexSuffixes) to High(TDisassembler.AllowedHexSuffixes) do begin
    S := Lowercase(Trim(TDisassembler.AllowedHexSuffixes[I]));
    ComboBoxHexFormat.Items.Add('...' + S);
    if I > 0 then begin
      if I = 1 then
        S2 := 'es' + S2;
      S2 := S2 + ',';
    end;
    S2 := S2 + ' "' + S + '"';
  end;

  PanelNumBase.Hint := cPanelNumBaseHint;
  PanelHexFormat.Hint := Format(cPanelHexFormatHint, [S1, S2]);
  CheckBoxDisplayRelativeJumpOffsetAsAbsolute.Hint :=
    cCheckBoxDisplayRelativeJumpOffsetAsAbsoluteHint;
  CheckBoxFollowPC.Hint := cCheckBoxFollowPCHint;
  ButtonStep.Hint := cStepHint;
  ButtonJumpGo.Hint := cJumpToAddrHint;
  EditAddr.Hint := cJumpToAddrHint;
  ButtonJumpToPC.Hint := cJumpToPC;
  ButtonJumpToSP.Hint := cJumpToSP;

  Panel5.ShowHint := True;

  ComboBoxNumBase.OnChange := @DisassemblerDisplayChange;
  ComboBoxHexFormat.OnChange := @DisassemblerDisplayChange;

  BorderStyle := bsNone;
  FGrid.Enabled := False;
  SetActive(True);
  FGrid.Parent := Panel2;
  ButtonRunStop.Constraints.MinWidth := ButtonRunStop.Width;
  EditAddr.OnEditingDone := @EditAddrEditingDone;
end;

destructor TFrameGridMemory.Destroy;
begin
  FGrid.Free;

  inherited Destroy;
end;

procedure TFrameGridMemory.OnStep(out DoContinue: Boolean);
begin
  DoContinue := FOneStep;
  FOneStep := False;
end;

procedure TFrameGridMemory.AfterStep;
begin
  if Assigned(FGrid) then begin
    FGrid.Invalidate;
  end;
end;

function TFrameGridMemory.SaveToJSON(out JSONObj: TJSONObject): Boolean;
var
  N: Integer;
  S: RawByteString;
  D: TDisassembler;
  JObj: TJSONObject;

begin
  Result := False;
  JSONObj := nil;

  JObj := TJSONObject.Create;
  try
    D := GetDisassembler;
    if Assigned(D) then begin
      WriteStr(S, D.NumDisplay);
      S := LowerCase(Copy(S, 3));
      JObj.Add(cSectionNumBase, S);
      JObj.Add(cSectionHexFormat, D.Hexfix);
      if D.DisplayRelativeJumpOffsetAsAbsolute then
        N := 1
      else
        N := 0;
      JObj.Add(cSectionDisplayRelativeJumpOffsetAsAbsolute, N);
    end;

    if JObj.Count > 0 then begin
      JSONObj := JObj;
      JObj := nil;
      Result := True;
    end;

  finally
    JObj.Free;
  end;
end;

procedure TFrameGridMemory.LoadFromJSON(const JSONObj: TJSONObject);
var
  S, S1: RawByteString;
  Nd: TDisassembler.TNumDisplay;
  D: TDisassembler;

begin
  if Assigned(JSONObj) then begin
    D := GetDisassembler;
    if Assigned(D) then begin
      S := '';
      S := JSONObj.Get(cSectionNumBase, S);
      for Nd := Low(TDisassembler.TNumDisplay) to High(TDisassembler.TNumDisplay) do begin
        WriteStr(S1, Nd);
        if CompareText(Copy(S1, 3), S) = 0 then begin
          D.NumDisplay := Nd;
          Break;
        end;
      end;

      S := '';
      S := JSONObj.Get(cSectionHexFormat, S);
      D.HexFormatIndex := TDisassembler.DecodeHexFormatIndex(S);
      D.DisplayRelativeJumpOffsetAsAbsolute := JSONObj.Get(cSectionDisplayRelativeJumpOffsetAsAbsolute, Integer(0)) <> 0;
      UpdateValuesFromDisassembler;
    end;
  end;
end;

end.
