unit UnitFrameOneKeyMap;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, fgl, UnitKeyMaps, CommonFunctionsLCL,
  UnitKeyMapRecords, UnitSpectrumKeysDialog, UnitCommonSpectrum, Forms,
  Controls, ExtCtrls, StdCtrls, LCLType, Graphics, LMessages;

type

  { TFrameOnePCKeyMapping }

  TFrameOnePCKeyMapping = class(TFrame)
    Bevel1: TBevel;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelEditSpectrumKeys: TPanel;
    PanelRemovePCKey: TPanel;
    PanelPCKey: TPanel;
    PanelSpectrumKeys: TPanel;
    Shape1: TShape;
    procedure FrameResize(Sender: TObject);
    procedure PanelPCKeyResize(Sender: TObject);
  private
    type
      TOneSpectrumKeyControl = class(TCustomControl)
      strict private
        const
          cHintRemove = 'Remove Spectrum key%s from mapping';
      strict private
        FSpectrumKey: Word;
        FKeyName: String;
        Lab: TLabel;
        LabPlus: TLabel;

        procedure SetSpectrumKey(const AValue: Word);
        procedure ControlsMouseDown(Sender: TObject);
      public
        constructor Create(AOwner: TComponent); override;

        procedure SetHasPlus(const AValue: Boolean);
        function GetHasPlus: Boolean;
        property KeyName: String read FKeyName;
        property SpectrumKey: Word read FSpectrumKey write SetSpectrumKey;
      end;

      TSpectrumKeyControls = Array of TOneSpectrumKeyControl;

  strict private
    const
      cHintRemovePCKey = 'Remove all mappings for PC key%s';
  private
    SpectrumKeys: TSpectrumKeyControls;

    procedure UpdateSpectrumKeysLayout;
    procedure SortSpectrumKeys;
    procedure AddSpectrumKey(const AValue: Word);
  strict private
    FPCKeyName: String;
    FPCKey: Word;
    LabRemovePCKey: TCustomLabel;
    LabEditSpectrumKeys: TCustomLabel;
    UpdateSpectrumKeysLayoutCnt: Integer;

    function GetOnRemoveClick: TNotifyEvent;
    procedure SetOnRemoveClick(AValue: TNotifyEvent);
    procedure SetPCKey(AValue: Word);

    function FindSpectrumKeyControl(const AValue: Word): TOneSpectrumKeyControl;
    procedure OnClickLabEditSpectrumKeys(Sender: TObject);
    procedure UserInputEvent(Sender: TObject; Msg: Cardinal);
  public
    constructor Create(TheOwner: TComponent; Sz: TSize);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearSpectrumKeys;
    property PCKey: Word read FPCKey write SetPCKey;
    property OnRemoveClick: TNotifyEvent read GetOnRemoveClick write SetOnRemoveClick;

    procedure Paint; override;
  end;

  { TControlKeyMappings }

  TControlKeyMappings = class(TCustomControl)
  strict private
    type
      TFrameMappingsMap = class(specialize TFPGMapObject<Word, TFrameOnePCKeyMapping>)
      private
      public
        constructor Create(AFreeObjects: Boolean);
      end;

  strict private
    FrameMappingsMap: TFrameMappingsMap;
    Sz: TSize;
    ScrollBox: TScrollBox;
    PrevCtrl: TControl;

    function InternalAddPCKey(AValue: Word; WithFocus: Boolean): Integer;
    procedure RemovePCKey(AValue: Word);
    procedure OnClickRemovePCKey(Sender: TObject);
    procedure ActiveCtrlChg(Sender: TObject; LastControl: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure AddPCKey(AValue: Word);

    procedure LoadFromKeyMapRecs(const KMRecs: TKeyMapRecs);
    procedure SaveToKeyMapRecs(out KMRecs: TKeyMapRecs);
  end;

implementation

{$R *.lfm}

{ TControlKeyMappings.TFrameMappingsMap }

constructor TControlKeyMappings.TFrameMappingsMap.Create(AFreeObjects: Boolean);
begin
  inherited Create(AFreeObjects);

  Duplicates := TDuplicates.dupIgnore;
  Sorted := True;
end;

{ TControlKeyMappings }

procedure TControlKeyMappings.OnClickRemovePCKey(Sender: TObject);
var
  C: TControl;
begin
  if Sender is TControl then begin
    C := TControl(Sender);
    repeat
      if C is TFrameOnePCKeyMapping then begin
        RemovePCKey(TFrameOnePCKeyMapping(C).PCKey);
        Break;
      end;

      C := C.Parent;
    until (C = nil) or (C = Self);
  end;
end;

procedure TControlKeyMappings.ActiveCtrlChg(Sender: TObject;
  LastControl: TControl);
var
  B: Boolean;
begin
  B := IsParentOf(LastControl);
  if Assigned(PrevCtrl) or B then begin
    Invalidate;
    if not B then
      PrevCtrl := nil
    else begin
      PrevCtrl := LastControl;
      if LastControl.Parent = ScrollBox then
        ScrollBox.ScrollInView(LastControl);
    end;
  end;
end;

constructor TControlKeyMappings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PrevCtrl := nil;
  TCommonFunctionsLCL.CalculateTextSize(Self.Font, DecodePCKey(VK_CLEAR), Sz);
  ScrollBox := TScrollBox.Create(Self);
  ScrollBox.Anchors := [];
  ScrollBox.AnchorParallel(akTop, 0, Self);
  ScrollBox.AnchorParallel(akLeft, 0, Self);
  ScrollBox.AnchorParallel(akBottom, 0, Self);
  ScrollBox.AnchorParallel(akRight, 0, Self);
  ScrollBox.AutoSize := True;
  ScrollBox.AutoScroll := True;
  ScrollBox.BorderStyle := bsNone;

  ScrollBox.Parent := Self;
  FrameMappingsMap := TFrameMappingsMap.Create(False);

  AutoSize := True;
  Screen.AddHandlerActiveControlChanged(@ActiveCtrlChg);
end;

destructor TControlKeyMappings.Destroy;
begin
  Screen.RemoveAllHandlersOfObject(Self);
  FrameMappingsMap.Free;

  inherited Destroy;
end;

procedure TControlKeyMappings.Clear;
var
  I: Integer;
begin
  for I := 0 to FrameMappingsMap.Count - 1 do begin
    FrameMappingsMap.Data[I].Free;
    FrameMappingsMap.Data[I] := nil;
  end;
  FrameMappingsMap.Clear;
end;

function TControlKeyMappings.InternalAddPCKey(AValue: Word; WithFocus: Boolean
  ): Integer;
var
  F, F1: TFrameOnePCKeyMapping;
begin
  Result := FrameMappingsMap.IndexOf(AValue);

  if Result >= 0 then begin
    F := FrameMappingsMap.Data[Result];
  end else begin
    F := TFrameOnePCKeyMapping.Create(ScrollBox, Sz);

    F.PCKey := AValue;
    Result := FrameMappingsMap.Add(AValue, F);
    F.Anchors := [];
    F.AnchorParallel(akLeft, 0, ScrollBox);
    F.Color := clWindow;
    F.BorderSpacing.Around := 2;

    F.Parent := ScrollBox;
    F.TabStop := True;
    if Result > 0 then begin
      F1 := FrameMappingsMap.Data[Result - 1];
      F.AnchorToNeighbour(akTop, 0, F1);
    end else begin
      F.AnchorParallel(akTop, 0, ScrollBox);
    end;

    if FrameMappingsMap.Count - 1 > Result then begin
      F1 := FrameMappingsMap.Data[Result + 1];
      F1.AnchorToNeighbour(akTop, 0, F);
    end;

    F.TabOrder := Result;
    F.OnRemoveClick := @OnClickRemovePCKey;
  end;

  if WithFocus and Assigned(F) and F.CanSetFocus then begin
    F.SetFocus;
    ScrollBox.ScrollInView(F);
  end;

end;

procedure TControlKeyMappings.AddPCKey(AValue: Word);
begin
  InternalAddPCKey(AValue, True);
end;

procedure TControlKeyMappings.RemovePCKey(AValue: Word);
var
  N: Integer;
  F: TFrameOnePCKeyMapping;
  I: Integer;
begin
  N := FrameMappingsMap.IndexOf(AValue);
  if N >= 0 then begin
    F := FrameMappingsMap.Data[N];
    if N < FrameMappingsMap.Count - 1 then
      FrameMappingsMap.Data[N + 1].AnchorSame(akTop, F);
    FrameMappingsMap.Data[N] := nil;
    Application.ReleaseComponent(F);
    FrameMappingsMap.Delete(N);
  end;
end;

procedure TControlKeyMappings.LoadFromKeyMapRecs(const KMRecs: TKeyMapRecs);
var
  N, I: Integer;
begin
  DisableAlign;
  try
    Clear;
    for I := 0 to High(KMRecs) do begin
      N := Self.InternalAddPCKey(KMRecs[I].Key, False);

      FrameMappingsMap.Data[N].AddSpectrumKey(KMRecs[I].SpectrumKey);
    end;
    for I := 0 to FrameMappingsMap.Count - 1 do begin
      FrameMappingsMap.Data[I].SortSpectrumKeys;
      FrameMappingsMap.Data[I].UpdateSpectrumKeysLayout;
    end;
  finally
    EnableAlign;
  end;
end;

procedure TControlKeyMappings.SaveToKeyMapRecs(out KMRecs: TKeyMapRecs);
var
  I, J, K, N: Integer;
  W: Word;
  SKL: TFrameOnePCKeyMapping.TSpectrumKeyControls;
begin
  N := (FrameMappingsMap.Count * 7) div 5 + 2;
  SetLength(KMRecs, N);
  K := 0;
  for I := 0 to FrameMappingsMap.Count - 1 do begin
    SKL := FrameMappingsMap.Data[I].SpectrumKeys;
    if Length(SKL) > 0 then begin
      W := FrameMappingsMap.Keys[I];
      N := K + Length(SKL);
      if N >= Length(KMRecs) then begin
        N := ((N + FrameMappingsMap.Count - I) * 6) div 5;
        SetLength(KMRecs, N);
      end;
      for J := 0 to High(SKL) do begin
        KMRecs[K].Key := W;
        KMRecs[K].SpectrumKey := SKL[J].SpectrumKey;
        Inc(K);
      end;
    end;
  end;
  SetLength(KMRecs, K);
end;

{ TFrameOnePCKeyMapping.TOneSpectrumKeyControl }

procedure TFrameOnePCKeyMapping.TOneSpectrumKeyControl.SetSpectrumKey(
  const AValue: Word);
var
  S: String;
begin
  if FSpectrumKey <> AValue then begin
    FSpectrumKey := AValue;
    FKeyName := UnitKeyMaps.DecodeSpectrumKey(AValue);
    if FKeyName = '' then begin
      FKeyName := AValue.ToString;
      S := '';
    end else begin
      S := ' ' + FKeyName;
    end;

    Lab.Hint := Format(cHintRemove, [S]);
    Lab.Caption := FKeyName;
  end;
end;

procedure TFrameOnePCKeyMapping.TOneSpectrumKeyControl.ControlsMouseDown(
  Sender: TObject);
begin
  if CanSetFocus then
    SetFocus;
end;

constructor TFrameOnePCKeyMapping.TOneSpectrumKeyControl.Create(
  AOwner: TComponent);
var
  Shape: TShape;
  C: TCustomControl;
  C2: TGraphicControl;
begin
  inherited Create(AOwner);

  LabPlus := TLabel.Create(Self);
  LabPlus.ShowAccelChar := False;
  LabPlus.Anchors := [];
  LabPlus.AnchorVerticalCenterTo(Self);
  LabPlus.AnchorParallel(akLeft, 0, Self);

  C := TCustomControl.Create(Self);
  C.Anchors := [];
  C.AnchorToNeighbour(akLeft, 0, LabPlus);
  C.AnchorVerticalCenterTo(Self);
  C.Color := $00FAEBDE;

  C.AutoSize := True;

  Shape := TShape.Create(C);
  Shape.Shape := TShapeType.stRectangle;
  Shape.AutoSize := False;
  Shape.Width := 1;
  Shape.Height := 1;
  Shape.Pen.Color := clSkyBlue;
                                                     
  Lab := TLabel.Create(C);
  Lab.ShowAccelChar := False;

  C2 := TGraphicControl.Create(C);
  C2.AutoSize := False;
  C2.Width := 2;
  C2.Height := 2;
  C2.Anchors := [];
  C2.AnchorToNeighbour(akLeft, 0, Lab);
  C2.AnchorToNeighbour(akTop, 0, Lab);

  Shape.AnchorParallel(akLeft, 0, C);
  Shape.AnchorParallel(akTop, 0, C);
  Shape.AnchorParallel(akRight, 0, C2);
  Shape.AnchorParallel(akBottom, 0, C2);

  LabPlus.Parent := Self;

  FKeyName := '';
  FSpectrumKey := $FFFF;
  Lab.Anchors := [];

  Lab.AnchorParallel(akTop, 2, C);
  Lab.AnchorParallel(akLeft, 2, C);

  Lab.Parent := C;
  C2.Parent := C;

  Lab.Font.Color := clNavy;
  Shape.Brush.Style := bsClear;
  Shape.Parent := C;
  Shape.SendToBack;
  C2.SendToBack;

  C.Parent := Self;

  TabStop := False;
  Self.AutoSize := True;
end;

procedure TFrameOnePCKeyMapping.TOneSpectrumKeyControl.SetHasPlus(
  const AValue: Boolean);
begin
  if AValue then begin
    LabPlus.Caption := ' + '
  end else
    LabPlus.Caption := '';
end;

function TFrameOnePCKeyMapping.TOneSpectrumKeyControl.GetHasPlus: Boolean;
begin
  Result := Trim(LabPlus.Caption) <> '';
end;

{ TFrameOnePCKeyMapping }

procedure TFrameOnePCKeyMapping.SetPCKey(AValue: Word);
var
  S: String;
begin
  FPCKey := AValue;
  FPCKeyName := DecodePCKey(AValue);
  if FPCKeyName = '' then begin
    S := '';
    FPCKeyName := AValue.ToString;
  end else begin
    S := ' ' + FPCKeyName;
  end;

  LabRemovePCKey.Hint := Format(cHintRemovePCKey, [S]);
  Label1.Caption := FPCKeyName;
end;

procedure TFrameOnePCKeyMapping.FrameResize(Sender: TObject);
begin
  UpdateSpectrumKeysLayout;
end;

procedure TFrameOnePCKeyMapping.PanelPCKeyResize(Sender: TObject);
begin
  PanelSpectrumKeys.Constraints.MinHeight := PanelPCKey.Height;
end;

function TFrameOnePCKeyMapping.GetOnRemoveClick: TNotifyEvent;
begin
  Result := LabRemovePCKey.OnClick;
end;

procedure TFrameOnePCKeyMapping.SetOnRemoveClick(AValue: TNotifyEvent);
begin
  LabRemovePCKey.OnClick := AValue;
end;

function TFrameOnePCKeyMapping.FindSpectrumKeyControl(const AValue: Word
  ): TOneSpectrumKeyControl;
var
  I: Integer;
begin
  for I := Low(SpectrumKeys) to High(SpectrumKeys) do
    if SpectrumKeys[I].SpectrumKey = AValue then
      Exit(SpectrumKeys[I]);

  Result := nil;
end;

procedure TFrameOnePCKeyMapping.OnClickLabEditSpectrumKeys(Sender: TObject);
var
  AW: UnitSpectrumKeysDialog.TArrayOfWord;
  I: Integer;
begin
  SetLength(AW, Length(SpectrumKeys));
  for I := 0 to High(SpectrumKeys) do begin
    AW[I] := SpectrumKeys[I].SpectrumKey;
  end;

  if UnitSpectrumKeysDialog.TFormSpectrumKeysDialog.EditKeyMappings(PCKey, AW) then begin
    ClearSpectrumKeys;
    for I := 0 to High(AW) do
      AddSpectrumKey(AW[I]);
    UpdateSpectrumKeysLayout;
  end;
end;

procedure TFrameOnePCKeyMapping.UserInputEvent(Sender: TObject; Msg: Cardinal);
begin
  case Msg of
    LM_LBUTTONDOWN, LM_RBUTTONDOWN:
      if MouseInClient and CanSetFocus then
        SetFocus;
  end;
end;

procedure TFrameOnePCKeyMapping.UpdateSpectrumKeysLayout;
const
  SpcW: Integer = 2;
  SpcH: Integer = 1;

var
  I: Integer;
  WE, W, H: Integer;
  OSC, OSC1: TOneSpectrumKeyControl;
begin
  if UpdateSpectrumKeysLayoutCnt = 0 then begin // skip recursive calls
    Inc(UpdateSpectrumKeysLayoutCnt);
    try
      OSC1 := nil;
      WE := 0;
      for I := 0 to High(SpectrumKeys) do begin
        OSC := SpectrumKeys[I];
        OSC.GetPreferredSize(W, H);
        WE := WE + W;
        if Assigned(OSC1) then begin
          WE := WE + SpcW;
          if WE >= PanelSpectrumKeys.ClientWidth then begin
            WE := W;
            OSC.AnchorToNeighbour(akTop, SpcH, OSC1);
            OSC.AnchorParallel(akLeft, 0, PanelSpectrumKeys);
          end else begin
            OSC.AnchorParallel(akTop, 0, OSC1);
            OSC.AnchorToNeighbour(akLeft, SpcW, OSC1);
          end;
        end else begin
          OSC.AnchorParallel(akTop, 0, PanelSpectrumKeys);
          OSC.AnchorParallel(akLeft, 0, PanelSpectrumKeys);
        end;

        OSC1 := OSC;
      end;
    finally
      Dec(UpdateSpectrumKeysLayoutCnt);
    end;
  end;
end;

procedure TFrameOnePCKeyMapping.SortSpectrumKeys;
var
  AW: UnitSpectrumKeysDialog.TArrayOfWord;
  I: Integer;
begin
  SetLength(AW, Length(SpectrumKeys));
  for I := 0 to High(SpectrumKeys) do begin
    AW[I] := SpectrumKeys[I].SpectrumKey;
  end;

  TCommonSpectrum.SortSpectrumKeys(AW);

  PanelSpectrumKeys.DisableAutoSizing;
  try
    ClearSpectrumKeys;
    for I := 0 to High(AW) do
      AddSpectrumKey(AW[I]);
  finally
    PanelSpectrumKeys.EnableAutoSizing;
  end;
end;

constructor TFrameOnePCKeyMapping.Create(TheOwner: TComponent; Sz: TSize);
var
  C, CP: TControl;
begin
  inherited Create(TheOwner);

  C := Label1;
  while Assigned(C) do begin
    CP := C.Parent;
    if CP = nil then
      Break;
    Sz.cx := Sz.cx + C.ClientOrigin.X - CP.ClientOrigin.X;

    C := CP;
    if C = Self then
      Break;
  end;
  UpdateSpectrumKeysLayoutCnt := 1;

  Name := 'fpm' + PtrInt(Self).ToHexString;
  SetLength(SpectrumKeys, 0);
  Panel2.BevelOuter := bvNone;
  Panel1.BevelOuter := bvNone;

  PanelRemovePCKey.BevelOuter := bvNone;
  PanelPCKey.BevelOuter := bvNone;
  PanelSpectrumKeys.BevelOuter := bvNone;
  PanelEditSpectrumKeys.BevelOuter := bvNone;

  PanelPCKey.Constraints.MinWidth := Sz.cx;
  PanelPCKey.Constraints.MaxWidth := Sz.cx;

  PanelSpectrumKeys.Constraints.MinWidth := Sz.cx;
  PanelSpectrumKeys.Constraints.MaxWidth := Sz.cx;
  Label1.ShowAccelChar := False;
  Label1.Caption := ' ';

  Panel2.Color := $00DAE8EF;
  Shape1.Pen.Color := $00ABCDDE;

  LabRemovePCKey := TCommonFunctionsLCL.CreateLinkLabel(Self.PanelRemovePCKey, 'Remove');

  LabRemovePCKey.Anchors := [];
  LabRemovePCKey.AnchorParallel(akTop, 0, PanelRemovePCKey);
  LabRemovePCKey.AnchorParallel(akLeft, 0, PanelRemovePCKey);

  LabEditSpectrumKeys := TCommonFunctionsLCL.CreateLinkLabel(Self.PanelEditSpectrumKeys, 'Edit...');
  LabEditSpectrumKeys.Hint := 'Edit spectrum keys';
  LabEditSpectrumKeys.Anchors := [];
  LabEditSpectrumKeys.AnchorParallel(akTop, 0, PanelEditSpectrumKeys);
  LabEditSpectrumKeys.AnchorParallel(akLeft, 0, PanelEditSpectrumKeys);
  LabEditSpectrumKeys.OnClick := @OnClickLabEditSpectrumKeys;

  LabEditSpectrumKeys.Parent := PanelEditSpectrumKeys;
  LabRemovePCKey.Parent := PanelRemovePCKey;
  PanelRemovePCKey.AutoSize := True;
  PanelPCKey.AutoSize := True;
  PanelSpectrumKeys.Constraints.MinHeight := PanelPCKey.Height;
  PanelSpectrumKeys.AutoSize := True;
  PanelEditSpectrumKeys.AutoSize := True;
  Panel2.AutoSize := True;

  Application.AddOnUserInputHandler(@UserInputEvent);
  Self.AutoSize := True;

  UpdateSpectrumKeysLayoutCnt := 0;
end;

constructor TFrameOnePCKeyMapping.Create(TheOwner: TComponent);
var
  Sz: TSize;
begin
  TCommonFunctionsLCL.CalculateTextSize(Self.Font, DecodePCKey(VK_CLEAR), Sz);
  Create(TheOwner, Sz);
end;

destructor TFrameOnePCKeyMapping.Destroy;
begin
  Application.RemoveAllHandlersOfObject(Self);
  DisableAlign;
  SetLength(SpectrumKeys, 0);

  inherited Destroy;
end;

procedure TFrameOnePCKeyMapping.AddSpectrumKey(const AValue: Word);
var
  OSC: TOneSpectrumKeyControl;
begin
  OSC := FindSpectrumKeyControl(AValue);
  if OSC = nil then begin
    OSC := TOneSpectrumKeyControl.Create(PanelSpectrumKeys);
    OSC.SpectrumKey := AValue;
    OSC.SetHasPlus(Length(SpectrumKeys) > 0);

    SetLength(SpectrumKeys, Length(SpectrumKeys) + 1);
    SpectrumKeys[High(SpectrumKeys)] := OSC;

    OSC.Parent := PanelSpectrumKeys;
  end;
end;

procedure TFrameOnePCKeyMapping.ClearSpectrumKeys;
var
  I: Integer;
begin
  PanelSpectrumKeys.DisableAutoSizing;
  try
    for I := High(SpectrumKeys) downto Low(SpectrumKeys) do
      SpectrumKeys[I].Free;

    SetLength(SpectrumKeys, 0);
  finally
    PanelSpectrumKeys.EnableAutoSizing;
  end;
end;

procedure TFrameOnePCKeyMapping.Paint;
var
  C: TWinControl;
  F: TCustomForm;
  R: TRect;
begin
  F := GetParentForm(Self, False);
  if Assigned(F) then begin
    C := F.ActiveControl;

    while Assigned(C) do begin
      if C = Self then begin
        Canvas.Pen.Style := psSolid;
        R := ClientRect;
        R := Rect(R.Left + 1, R.Top + 1, R.Right - 2, R.Bottom - 2);

        Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top),
          R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);

        InflateRect(R, -1, -1);
        Canvas.Pen.Style := psSolid;
        Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top),
          R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft]);
        Break;
      end;
      C := C.Parent;
    end;

  end;
  inherited Paint;
end;

end.

