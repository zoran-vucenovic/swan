unit UnitColourPalette;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, UnitSpectrumColourMap, CommonFunctionsLCL,
  UnitCommonSpectrum, UnitFormForOptionsBasic, UnitOptions, UnitCommon, Forms,
  Controls, ExtCtrls, Graphics, StdCtrls, Dialogs, Buttons, LCLType, LCLIntf,
  LazUTF8;

type
  TFrameColourPalette = class(TFrame)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ColorDialog1: TColorDialog;
    ComboBoxCustom: TComboBox;
    ComboBoxPredefined: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure ComboBoxCustomChange(Sender: TObject);
    procedure ComboBoxPredefinedChange(Sender: TObject);
  strict private
    type
      TPan = class(TCustomControl)
      public
        property ParentFont;
      end;

      TColourArea = class(TGraphicControl)
      private
        DisplayText: String;
        TextStyle: TTextStyle;
      protected
        procedure SetColor(Value: TColor); override;
        procedure MouseEnter; override;
        procedure MouseLeave; override;
      public
        constructor Create(AOwner: TComponent); override;
        procedure Paint; override;            
        property ParentColor default False;
        property Color nodefault;
      end;

      TObjColoursMap = class(TObject)
      public
        LCLColourMap: TLCLColourMap;
      end;
  strict private
    ColourPanels: Array[0..15] of TColourArea;
    LabColour: Array [0..7] of TLabel;

    function GetLCLColours: TLCLColourMap;
    procedure SetLCLColours(AValue: TLCLColourMap);
    procedure SetLCLColour(AColour: TColor; ABright: Boolean; ASpectrumColourIndex: Integer);
    procedure CAClick(Sender: TObject);
    procedure FillPredefinedPalettes;
    procedure FillCustomPalettes;
    procedure SaveCustomPalettes;
    procedure ClearCombo(CB: TCustomComboBox);

    procedure SwitchToCB(CBFrom, CBTo: TCustomComboBox);
    procedure FillColourMapFromColourPanels(var CM: TLCLColourMap);
    procedure InternalAdd(N: Integer; const S: String);
    procedure NewPalette;
    procedure FormCloseCallback(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    class function PickColours(var AColours: TLCLColourMap): Boolean;
    class function CreateForOptionsDialog(AOptionsDialog: TFormOptions): TFrameColourPalette;

    property LCLColours: TLCLColourMap read GetLCLColours write SetLCLColours;
  end;

implementation

{$R *.lfm}

{ TFrameColourPalette.TColourArea }

procedure TFrameColourPalette.TColourArea.SetColor(Value: TColor);
var
  R, G, B: Integer;
begin
  if Value = Color then
    Exit;
  // We don't allow system colours here.
  if Value = clDefault then
    Value := GetDefaultColor(TDefaultColorType.dctBrush);

  Value := ColorToRGB(Value);

  R := Red(Value);
  G := Green(Value);
  B := Blue(Value);
  DisplayText := Format('RGB:%3:s%0:s%4:s%1:s%4:s%2:s',
    [IntToHex(R, 2), IntToHex(G, 2), IntToHex(B, 2), TCommonFunctions.NonBreakSpace,
        TCommonFunctions.NarrowNonBreakSpace]);

  Font.Color := TCommonFunctionsLCL.GetBestContrastColorForFont(R, G, B);

  inherited SetColor(Value);
end;

procedure TFrameColourPalette.TColourArea.MouseEnter;
begin
  inherited MouseEnter;
  Invalidate;
end;

procedure TFrameColourPalette.TColourArea.MouseLeave;
begin
  inherited MouseLeave;
  Invalidate;
end;

constructor TFrameColourPalette.TColourArea.Create(AOwner: TComponent);
begin
  DisplayText := '';
  inherited Create(AOwner);
  ParentColor := False;
  Color := GetRGBColorResolvingParent;

  TextStyle := Canvas.TextStyle;
  TextStyle.Alignment := taCenter;
  TextStyle.Layout := tlCenter;
  TextStyle.Opaque := False;
  TextStyle.Wordbreak := False;
  TextStyle.SystemFont := False;
  TextStyle.RightToLeft := False;
  Cursor := crHandPoint;
end;

procedure TFrameColourPalette.TColourArea.Paint;

  procedure DrawRect(const R: TRect; AndFill: Boolean);
  var
    Points: Array of TPoint;
  begin
    Points := [
      R.TopLeft, Point(R.Right, R.Top), R.BottomRight,
      Point(R.Left, R.Bottom)
      , R.TopLeft
    ];
    if AndFill then
      Canvas.Polygon(Points, True, 1)
    else
      Canvas.Polyline(Points);
  end;

var
  R: TRect;
begin
  inherited Paint;

  R := Rect(0, 0, ClientWidth - 1, ClientHeight - 1);
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := clBlack;
  DrawRect(R, True);

  if MouseInClient then begin
    R.Inflate(-2, -2);
    Canvas.Pen.Color := Font.Color;
    DrawRect(R, False);
  end;

  Canvas.TextRect(Self.ClientRect, 0, 0, DisplayText, TextStyle);
end;

{ TFrameColourPalette }

procedure TFrameColourPalette.SetLCLColours(AValue: TLCLColourMap);
var
  I: Integer;
begin
  AValue.Name := UTF8Trim(AValue.Name);
  I := ComboBoxPredefined.Items.IndexOf(AValue.Name);
  if I >= 0 then begin
    ComboBoxPredefined.ItemIndex := I;
    SwitchToCB(ComboBoxCustom, ComboBoxPredefined);
  end else begin
    I := ComboBoxCustom.Items.IndexOf(AValue.Name);
    if I >= 0 then begin
      ComboBoxCustom.ItemIndex := I;
      SwitchToCB(ComboBoxPredefined, ComboBoxCustom);
    end else begin
      ComboBoxPredefined.ItemIndex := -1;
      ComboBoxCustom.ItemIndex := -1;
      for I := Low(ColourPanels) to High(ColourPanels) do
        ColourPanels[I].Color := AValue.Colours[I > 7, I mod 8];
    end;
  end;

end;

procedure TFrameColourPalette.NewPalette;
var
  S: String;
  N: Integer;
begin
  S := '';

  if InputQuery('New colour palette', 'Name:', S) then begin
    S := UTF8Trim(S);
    if S <> '' then begin
      N := ComboBoxPredefined.Items.IndexOf(S);
      if N >= 0 then begin
        MessageDlg(
          Format('"%s" is a predefined palette name.%sIt cannot be used as a custom palette name.',
              [ComboBoxPredefined.Items.Strings[N], LineEnding])
          , mtError, [mbClose], 0)
      end else begin
        N := ComboBoxCustom.Items.IndexOf(S);
        if (N < 0) or
          (MessageDlg(
            Format('"%s" is an already used palette name.%sDo you want to overwrite this palette?',
                [ComboBoxCustom.Items.Strings[N], LineEnding])
            , mtConfirmation, [mbYes, mbNo], 0) = mrYes)
        then begin
          InternalAdd(N, S);
        end;
      end;
    end;

  end;

end;

procedure TFrameColourPalette.FormCloseCallback(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Application.RemoveAsyncCalls(Self);
  if Sender is TCustomForm then begin
    if TCustomForm(Sender).ModalResult = mrOK then begin
      SaveCustomPalettes;
    end;
  end;
end;

procedure TFrameColourPalette.BitBtn1Click(Sender: TObject);
begin
  NewPalette;
end;

procedure TFrameColourPalette.BitBtn2Click(Sender: TObject);
var
  N: Integer;
  S: String;
begin
  N := ComboBoxCustom.ItemIndex;
  if N < 0 then begin
    N := ComboBoxPredefined.ItemIndex;
    if N >= 0 then begin
      S := 'Currently chosen palette "' + ComboBoxPredefined.Items.Strings[N] + '" is a predefined palette.'
        + LineEnding + 'It cannot be removed, only custom palettes can be removed.';
    end else begin
      S := 'Current palette is not saved. There is nothing to delete.';
    end;
    MessageDlg(S, mtInformation, [mbClose], 0);
  end else begin
    if MessageDlg(Format('Delete palette "%s"?', [ComboBoxCustom.Items.Strings[N]]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      ComboBoxCustom.Items.Objects[N].Free;
      ComboBoxCustom.Items.Objects[N] := nil;
      ComboBoxCustom.Items.Delete(N);
      ComboBoxCustom.ItemIndex := -1;
    end;
  end;
end;

procedure TFrameColourPalette.BitBtn3Click(Sender: TObject);
var
  ColourMap: TLCLColourMap;
  ColoursArr: TLCLColours;
  S: String;
begin
  ColourMap := GetLCLColours;
  if IsBW(ColourMap) then begin
    S := UTF8Trim(ColourMap.Name);
    if S <> '' then
      S := ' "' + S + '"';
    MessageDlg('Current colour palette' + S + ' is already a black and white pallete.',
      mtInformation, [mbClose], 0);
  end else begin
    ColourMap.Name := '';
    ColoursArr := ColourMap.Colours;
    ConvertToBW(ColoursArr, ColourMap.Colours);
    SetLCLColours(ColourMap);
  end;
end;

procedure TFrameColourPalette.ComboBoxCustomChange(Sender: TObject);
begin
  SwitchToCB(ComboBoxPredefined, ComboBoxCustom);
end;

procedure TFrameColourPalette.ComboBoxPredefinedChange(Sender: TObject);
begin
  SwitchToCB(ComboBoxCustom, ComboBoxPredefined);
end;

function TFrameColourPalette.GetLCLColours: TLCLColourMap;
begin
  if ComboBoxCustom.ItemIndex >= 0 then
    Result := (ComboBoxCustom.Items.Objects[ComboBoxCustom.ItemIndex] as TObjColoursMap).LCLColourMap
  else if ComboBoxPredefined.ItemIndex >= 0 then
    Result := (ComboBoxPredefined.Items.Objects[ComboBoxPredefined.ItemIndex] as TObjColoursMap).LCLColourMap
  else begin
    FillColourMapFromColourPanels(Result);
    Result.Name := '';
  end;
end;

procedure TFrameColourPalette.SetLCLColour(AColour: TColor; ABright: Boolean;
  ASpectrumColourIndex: Integer);
var
  J: Integer;
begin
  J := ASpectrumColourIndex mod 8;
  if ABright then
    J := J + 8;
  ColourPanels[J].Color := AColour;
end;

procedure TFrameColourPalette.CAClick(Sender: TObject);
var
  CA: TColourArea;
  S: String;
begin
  if Sender is TColourArea then begin
    CA := TColourArea(Sender);
    if CA.Tag > 7 then
      S := ' bright'
    else
      S := '';

    S := Format('Spectrum colour%s %s', [S, TCommonSpectrum.SpectrumColourNames[CA.Tag mod 8]]);
    ColorDialog1.Title := S;
    ColorDialog1.Color := CA.Color;
    if ColorDialog1.Execute then begin
      if CA.Color <> ColorDialog1.Color then begin
        CA.Color := ColorDialog1.Color;
        ComboBoxCustom.ItemIndex := -1;
        ComboBoxPredefined.ItemIndex := -1;
        Invalidate;
      end;
    end;
  end;
end;

procedure TFrameColourPalette.FillPredefinedPalettes;
var
  C: TObjColoursMap;
begin
  ClearCombo(ComboBoxPredefined);
  C := TObjColoursMap.Create;
  UnitSpectrumColourMap.GetDefaultSpectrumColoursD7(C.LCLColourMap);
  ComboBoxPredefined.Items.AddObject(C.LCLColourMap.Name, C);
  C := TObjColoursMap.Create;
  UnitSpectrumColourMap.GetDefaultSpectrumColoursC0(C.LCLColourMap);
  ComboBoxPredefined.Items.AddObject(C.LCLColourMap.Name, C);
  C := TObjColoursMap.Create;
  UnitSpectrumColourMap.GetBlackWhiteSpectrumColours(C.LCLColourMap);
  ComboBoxPredefined.Items.AddObject(C.LCLColourMap.Name, C);
end;

procedure TFrameColourPalette.FillCustomPalettes;
var
  C: TObjColoursMap;
  CC: TLCLColourMapArray;
  I: Integer;
begin
  ClearCombo(ComboBoxCustom);
  GetColourMaps(CC);
  for I := 0 to High(CC) do begin
    C := TObjColoursMap.Create;
    C.LCLColourMap := CC[I];
    ComboBoxCustom.Items.AddObject(C.LCLColourMap.Name, C);
  end;
end;

procedure TFrameColourPalette.SaveCustomPalettes;
var
  Obj: TObject;
  CC: TLCLColourMapArray;
  J, I: Integer;
begin
  SetLength(CC{%H-}, ComboBoxCustom.Items.Count);
  J := 0;
  for I := 0 to ComboBoxCustom.Items.Count - 1 do begin
    Obj := ComboBoxCustom.Items.Objects[I];
    if Obj is TObjColoursMap then begin
      CC[J] := TObjColoursMap(Obj).LCLColourMap;
      Inc(J);
    end;
  end;
  SetLength(CC, J);
  SetColourMaps(CC);
end;

procedure TFrameColourPalette.ClearCombo(CB: TCustomComboBox);
var
  I: Integer;
begin
  for I := 0 to CB.Items.Count - 1 do begin
    CB.Items.Objects[I].Free;
    CB.Items.Objects[I] := nil;
  end;
  CB.Clear;
end;

procedure TFrameColourPalette.SwitchToCB(CBFrom, CBTo: TCustomComboBox);
var
  O: TObject;
  CM: TLCLColourMap;
  I: Integer;
begin
  if CBTo.ItemIndex >= 0 then begin
    CBFrom.ItemIndex := -1;
    O := CBTo.Items.Objects[CBTo.ItemIndex];

    if O is TObjColoursMap then begin
      CM := TObjColoursMap(O).LCLColourMap;
      for I := Low(ColourPanels) to High(ColourPanels) do
        ColourPanels[I].Color := CM.Colours[I > 7, I mod 8];
    end;

  end;
end;

procedure TFrameColourPalette.FillColourMapFromColourPanels(
  var CM: TLCLColourMap);

var
  B: Boolean;
  I, J: Integer;
begin
  for B := False to True do begin
    for I := 0 to 7 do begin
      if B then
        J := I + 8
      else
        J := I;
      CM.Colours[B, I] := ColourPanels[J].Color;
    end;
  end;
end;

procedure TFrameColourPalette.InternalAdd(N: Integer; const S: String);
var
  C: TObjColoursMap;
begin
  C := TObjColoursMap.Create;
  FillColourMapFromColourPanels(C.LCLColourMap);
  C.LCLColourMap.Name := S;
  if N >= 0 then begin
    ComboBoxCustom.Items.Objects[N].Free;
    ComboBoxCustom.Items.Objects[N] := nil;
    ComboBoxCustom.Items.Delete(N);
  end else
    N := ComboBoxCustom.Items.Count;
  ComboBoxPredefined.ItemIndex := -1;
  ComboBoxCustom.Items.InsertObject(N, S, C);
  ComboBoxCustom.ItemIndex := N;
end;

constructor TFrameColourPalette.Create(TheOwner: TComponent);

var
  Pan: TPan;
  F: TFont;
  CA: TColourArea;

  procedure CalculateBoxSizes(var W, H: Integer);
  var
    Sz: TSize;
  begin
    try
      CommonFunctionsLCL.TCommonFunctionsLCL.CalculateTextSize(F, ' RGB: WW WW WW ', Sz);

      if W < Sz.cx then
        W := Sz.cx;
      if H < Sz.cy then
        H := Sz.cy;
    except
    end;
  end;

const
  W0: Integer = 50;
  H0: Integer = 31;

var
  B: Boolean;

  J, I: Integer;
  W, H, SpcW, SpcH: Integer;
  C0: TControl;
  Lab, Lab2: TLabel;
  FD: TFontData;

begin
  inherited Create(TheOwner);

  ComboBoxCustom.Items.UseLocale := True;
  ComboBoxPredefined.Items.UseLocale := ComboBoxCustom.Items.UseLocale;
  if ComboBoxCustom.Items is TStringList then
    TStringList(ComboBoxCustom.Items).CaseSensitive := False;
  if ComboBoxPredefined.Items is TStringList then
    TStringList(ComboBoxPredefined.Items).CaseSensitive := False;

  Label3.Font.Style := Label3.Font.Style + [fsUnderline];

  Caption := 'Spectrum colour palette';
  Panel1.BevelOuter := bvNone;
  Pan := TPan.Create(Panel1);
  Pan.Anchors := [];

  F := nil;
  try
    C0 := nil;
    for B := True downto False do begin
      for I := 0 to 7 do begin
        if B then
          J := I + 8
        else
          J := I;
        Lab2 := nil;
        CA := TColourArea.Create(Pan);
        ColourPanels[J] := CA;
        CA.AutoSize := False;
        CA.Tag := J;
        CA.OnClick := @CAClick;
        CA.ParentFont := False;

        if F = nil then begin
          F := TFont.Create;
          F.Assign(CA.Font);

          FD := GetFontData(F.Handle);
          F.BeginUpdate;
          try
            F.Name := FD.Name;
            F.Style := F.Style + [fsBold];
          finally
            F.EndUpdate;
          end;
          W := W0;
          H := H0;
          CalculateBoxSizes(W, H);

          SpcW := W div 10;
          SpcH := H div 10;
        end;

        CA.Font.Assign(F);

        CA.Width := W;
        CA.Anchors := [];

        if not B then begin
          C0 := ColourPanels[I + 8];
          CA.AnchorToNeighbour(akRight, SpcW, C0);
          CA.AnchorParallel(akTop, 0, C0);
          CA.AnchorParallel(akBottom, 0, C0);

          Lab := TLabel.Create(Pan);
          LabColour[I] := Lab;
          Lab.Anchors := [];

          Lab.AnchorVerticalCenterTo(CA);
          Lab.AnchorToNeighbour(akRight, 4, CA);
          Lab.Caption := TCommonSpectrum.SpectrumColourNames[I] + ':';
          Lab.AutoSize := True;
          Lab.Parent := Pan;

          if I = 0 then begin
            Lab2 := TLabel.Create(Pan);
            Lab2.Caption := 'normal';
            Lab2.Anchors := [];
            Lab2.AnchorToNeighbour(akRight, (SpcW + 1) div 2, C0);
          end;
        end else begin
          CA.Height := H;
          CA.AnchorParallel(akRight, 0, Pan);
          if C0 = nil then begin
            Lab2 := TLabel.Create(Pan);
            Lab2.Caption := 'bright';
            Lab2.Anchors := [];
            C0 := Lab2;
          end;
          CA.AnchorToNeighbour(akTop, SpcH, C0);
          C0 := CA;
        end;

        if Assigned(Lab2) then begin
          Lab2.AnchorParallel(akTop, 0, Pan);
          Lab2.AnchorParallel(akLeft, 0, CA);
          Lab2.Parent := Pan;
        end;

        CA.Parent := Pan;
      end;
    end;

  finally
    F.Free;
  end;
  Pan.AnchorParallel(akTop, 0, Panel1);
  Pan.AnchorParallel(akRight, 0, Panel1);
  Pan.BorderSpacing.Around := SpcW;
  Pan.AutoSize := True;

  Panel6.Anchors := [];
  Panel6.AnchorToNeighbour(akTop, 0, Pan);
  Panel6.AnchorParallel(akRight, 0, Pan);
  Pan.Parent := Panel1;
  Pan.ParentFont := True;

  Panel1.AutoSize := True;

  FillPredefinedPalettes;
  FillCustomPalettes;

  Panel4.Constraints.MinWidth := Panel4.Width;
  ShowHint := True;
  Self.AutoSize := True;
end;

destructor TFrameColourPalette.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  Screen.RemoveAllHandlersOfObject(Self);

  ClearCombo(ComboBoxCustom);
  ClearCombo(ComboBoxPredefined);

  inherited Destroy;
end;

class function TFrameColourPalette.PickColours(var AColours: TLCLColourMap
  ): Boolean;
var
  F: TCustomForm;
  Fm: TFrameColourPalette;

begin
  Result := False;
  Fm := TFrameColourPalette.Create(nil);
  try
    F := UnitFormForOptionsBasic.TFormForOptionsBasic.CreateForControl(nil, Fm, False);
    try
      F.AddHandlerClose(@Fm.FormCloseCallback);
      F.BorderStyle := bsSingle;
      Fm.SetLCLColours(AColours);
      if F.ShowModal = mrOK then begin
        AColours := Fm.GetLCLColours;
        Result := True;
      end;
    finally
      F.Free;
    end;
  finally
    Fm.Free;
  end;
end;

class function TFrameColourPalette.CreateForOptionsDialog(
  AOptionsDialog: TFormOptions): TFrameColourPalette;
var
  Fm: TFrameColourPalette;
begin
  Result := nil;
  if Assigned(AOptionsDialog) then begin
    Fm := TFrameColourPalette.Create(AOptionsDialog);
    try
      AOptionsDialog.AddHandlerClose(@Fm.FormCloseCallback);
      AOptionsDialog.AddAnOptionControl(Fm, 'Spectrum colours');
      Result := Fm;
    except
      FreeAndNil(Fm);
    end;
  end;
end;

end.

