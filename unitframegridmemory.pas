unit UnitFrameGridMemory;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitGridSpectrumMemory, UnitDisassembler, Forms, Controls,
  ExtCtrls, StdCtrls, Spin, Buttons, fpjson;

type
  TFrameGridMemory = class(TFrame)
    BitBtn1: TBitBtn;
    ButtonJumpToPC: TButton;
    ButtonJumpToSP: TButton;
    ButtonJumpGo: TButton;
    ButtonStep: TButton;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute: TCheckBox;
    CheckBoxFollowPC: TCheckBox;
    ComboBoxNumBase: TComboBox;
    ComboBoxHexFormat: TComboBox;
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
    PanelOptions: TPanel;
    PanelNumBase: TPanel;
    PanelHexFormat: TPanel;
    SpinEdit1: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure ButtonJumpGoClick(Sender: TObject);
    procedure ButtonJumpToPCClick(Sender: TObject);
    procedure ButtonJumpToSPClick(Sender: TObject);
    procedure ButtonStepClick(Sender: TObject);
  private
    const
      cSectionFollowPC: RawByteString = 'follow_pc';
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
        'Relative offsets in JR in DJNZ instructions are displayed'
        + LineEnding + 'as absolute addresses of the destination.'
        ;
  private
    FGrid: TSpectrumMemoryGrid;
    FOneStep: Boolean;

    function GetPc: Word;
    function GetSp: Word;
    procedure SetPc(const AValue: Word);
    procedure SetSp(const AValue: Word);
    procedure CheckBoxFollowPcOnChange(Sender: TObject);
    procedure DisassemblerDisplayChange(Sender: TObject);
    procedure UpdateValuesFromDisassembler;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetDisasembler(ADisasembler: TDisassembler);
    procedure OnStep(out DoContinue: Boolean);
    procedure AfterStep;
    function SaveToJSON(out JSONObj: TJSONObject): Boolean;
    function LoadFromJSON(const JSONObj: TJSONObject): Boolean;

    property Pc: Word read GetPc write SetPc;
    property Sp: Word read GetSp write SetSp;
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
  FGrid.JumpTo(SpinEdit1.Value);
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

function TFrameGridMemory.GetPc: Word;
begin
  Result := FGrid.Pc;
end;

function TFrameGridMemory.GetSp: Word;
begin
  Result := FGrid.Sp;
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
begin
  if Assigned(FGrid) and Assigned(FGrid.Disassembler) then begin
    FGrid.Disassembler.NumDisplay := TDisassembler.TNumDisplay(ComboBoxNumBase.ItemIndex);
    FGrid.Disassembler.HexFormatIndex := ComboBoxHexFormat.ItemIndex;
    FGrid.Disassembler.DisplayRelativeJumpOffsetAsAbsolute :=
      CheckBoxDisplayRelativeJumpOffsetAsAbsolute.Checked;
    FGrid.Invalidate;
  end;
end;

procedure TFrameGridMemory.UpdateValuesFromDisassembler;
begin
  if Assigned(FGrid.Disassembler) then begin
    SpinEdit1.MaxValue := FGrid.RowCount - FGrid.FixedRows - 1;
    ComboBoxNumBase.ItemIndex := Ord(FGrid.Disassembler.NumDisplay);
    ComboBoxHexFormat.ItemIndex := FGrid.Disassembler.HexFormatIndex;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute.OnChange := nil;
    CheckBoxFollowPC.OnChange := nil;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute.Checked := FGrid.Disassembler.DisplayRelativeJumpOffsetAsAbsolute;
    CheckBoxFollowPC.Checked := FGrid.FollowPc;
    CheckBoxDisplayRelativeJumpOffsetAsAbsolute.OnChange := @DisassemblerDisplayChange;
    CheckBoxFollowPC.OnChange := @CheckBoxFollowPcOnChange;
  end;
end;

constructor TFrameGridMemory.Create(TheOwner: TComponent);
var
  Nd: TDisassembler.TNumDisplay;
  I, J: Integer;
  S, S1, S2: RawByteString;
begin
  inherited Create(TheOwner);

  FOneStep := False;
  SpinEdit1.Value := 0;
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

  PanelNumBase.ShowHint := True;
  PanelHexFormat.ShowHint := True;
  CheckBoxDisplayRelativeJumpOffsetAsAbsolute.ShowHint := True;
  CheckBoxFollowPC.ShowHint := True;

  PanelNumBase.Hint := cPanelNumBaseHint;
  PanelHexFormat.Hint := Format(cPanelHexFormatHint, [S1, S2]);
  CheckBoxDisplayRelativeJumpOffsetAsAbsolute.Hint :=
    cCheckBoxDisplayRelativeJumpOffsetAsAbsoluteHint;
  CheckBoxFollowPC.Hint := cCheckBoxFollowPCHint;

  ComboBoxNumBase.OnChange := @DisassemblerDisplayChange;
  ComboBoxHexFormat.OnChange := @DisassemblerDisplayChange;

  BorderStyle := bsNone;
  FGrid.Parent := Panel2;
end;

destructor TFrameGridMemory.Destroy;
begin
  FGrid.Free;

  inherited Destroy;
end;

procedure TFrameGridMemory.SetDisasembler(ADisasembler: TDisassembler);
begin
  if Assigned(FGrid) then begin
    FGrid.Disassembler := ADisasembler;
    UpdateValuesFromDisassembler;
  end;
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
begin
  Result := False;

  JSONObj := TJSONObject.Create;
  try
    if FGrid.FollowPc then
      N := 1
    else
      N := 0;

    JSONObj.Add(cSectionFollowPC, N);
    if Assigned(FGrid.Disassembler) then begin
      WriteStr(S, FGrid.Disassembler.NumDisplay);
      S := LowerCase(Copy(S, 3));
      JSONObj.Add(cSectionNumBase, S);
      JSONObj.Add(cSectionHexFormat, FGrid.Disassembler.Hexfix);
      if FGrid.Disassembler.DisplayRelativeJumpOffsetAsAbsolute then
        N := 1
      else
        N := 0;
      JSONObj.Add(cSectionDisplayRelativeJumpOffsetAsAbsolute, N);
    end;

    Result := JSONObj.Count > 0;

  finally
    if not Result then
      FreeAndNil(JSONObj);
  end;
end;

function TFrameGridMemory.LoadFromJSON(const JSONObj: TJSONObject): Boolean;
var
  N: Integer;
  S, S1: RawByteString;
  Nd: TDisassembler.TNumDisplay;
begin
  Result := False;
  if Assigned(JSONObj) then begin
    FGrid.FollowPc := JSONObj.Get(cSectionFollowPC, Integer(1)) <> 0;
    if Assigned(FGrid.Disassembler) then begin
      S := '';
      S := JSONObj.Get(cSectionNumBase, S);
      for Nd := Low(TDisassembler.TNumDisplay) to High(TDisassembler.TNumDisplay) do begin
        WriteStr(S1, Nd);
        if CompareText(Copy(S1, 3), S) = 0 then begin
          FGrid.Disassembler.NumDisplay := Nd;
          Break;
        end;
      end;

      S := '';
      S := JSONObj.Get(cSectionHexFormat, S);
      FGrid.Disassembler.HexFormatIndex := TDisassembler.DecodeHexFormatIndex(S);
      FGrid.Disassembler.DisplayRelativeJumpOffsetAsAbsolute := JSONObj.Get(cSectionDisplayRelativeJumpOffsetAsAbsolute, Integer(0)) <> 0;
      UpdateValuesFromDisassembler;
    end;
  end;
end;

end.
