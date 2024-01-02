unit UnitSpectrumKeysControl;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, UnitKeyMaps, CommonFunctionsLCL, UnitCommonSpectrum,
  UnitSpectrumColourMap, Controls, StdCtrls, Graphics, LMessages, Forms,
  LCLType;

type

  TChgSpectrumKeyEx = procedure(AKeyValue: Word; Flags: Integer) of object;

  TSpectrumKeysControl = class(TCustomControl)
  strict private
    const
      BackgroundColour = TColor($001E1E1E);
  strict private
    type
      TSpectrumKeyButtons = array [1..40] of TCustomControl;
  strict private
    class var
      ColoursArr: array of TColor;
  strict private
    FOnChgSpectrumKey: TNotifyEvent;
    FOnChgSpectrumKeyEx: TChgSpectrumKeyEx;
    FButtonCapsShift: TCustomControl;
    FButtonSymbolShift: TCustomControl;
    Keys: TSpectrumKeyButtons;

    procedure DoOnChgSpectrumKey(Sender: TObject);
    procedure CreateKeyButtons(ButtonsToggle: Boolean);
  private
    class procedure Init(); static;
    class procedure InitColoursArr(); static;
  protected
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; ButtonsToggle: Boolean);
    procedure ReleaseShifts;
    procedure SetKeyStates(const SpectrumKeys: TWordDynArray);
    procedure GetKeyStates(out SpectrumKeys: TWordDynArray);
    function GetKeysString(out NumberOfPressedKeys: Integer): String;
    property OnChgSpectrumKey: TNotifyEvent read FOnChgSpectrumKey write FOnChgSpectrumKey;
    property OnChgSpectrumKeyEx: TChgSpectrumKeyEx read FOnChgSpectrumKeyEx write FOnChgSpectrumKeyEx;
  end;

implementation

type

  TSpectrumKeyButtonControl = class(TCustomControl)
  strict private
    FDisplayText: String;
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    LastMouseInClient: Boolean;
    function GetDisplayText: String;
    procedure SetDisplayText(AValue: String);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure MouseEventToggle(Sender: TObject; {%H-}Button: TMouseButton;
                      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure MouseEventPush(Sender: TObject; {%H-}Button: TMouseButton;
                      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure MouseEventRelease(Sender: TObject; {%H-}Button: TMouseButton;
                      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure UserInputEvent(Sender: TObject; Msg: Cardinal);
  private
    KeyValue: Word;
    Lab: TLabel;
    LabCommand: TLabel;
    LabSymbol: TLabel;
    Control0: TCustomControl;
    Toggle: Boolean;
  protected
    procedure SetChecked(AValue: Boolean);
    function GetChecked: Boolean;
    property DisplayText: String read GetDisplayText write SetDisplayText;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    procedure SetMouseEvents(AToggle: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

  TKeyGraphicShape = class(TGraphicControl)
  protected
    procedure DoOnResize; override;
  public
    Flags: Byte;
    procedure Paint; override;
  end;

{ TKeyGraphicShape }

procedure TKeyGraphicShape.DoOnResize;
begin
  inherited DoOnResize;
  Width := Height;
end;

procedure TKeyGraphicShape.Paint;
var
  B: TSpectrumKeyButtonControl;
  C: TWinControl;
  R: TRect;
  W: Integer;
  I: Integer;
begin
  inherited Paint;

  B := nil;
  C := Self.Parent;
  while (C <> nil) and (B = nil) do begin
    if C is TSpectrumKeyButtonControl then begin
      B := TSpectrumKeyButtonControl(C);
      Canvas.Pen.Color := B.Lab.Font.Color;
      Canvas.Brush.Color := B.Lab.Font.Color;

      W := Self.ClientWidth div 2 - 1;
      R := Bounds(1, 1, 2 * W - 1, 2 * W - 1);

      for I := 0 to 3 do begin
        if (1 shl I) and Flags <> 0 then begin
          Canvas.Rectangle(
            Bounds(
              R.Left + (1 - ((I mod 3) + 1) div 2) * W,
              R.Top + (I div 2) * W,
              W,
              W
            )
          );
        end;
      end;
      Canvas.Polyline([R.TopLeft, Point(R.Right, R.Top), R.BottomRight,
        Point(R.Left, R.Bottom), R.TopLeft]);
    end;
    C := C.Parent;
  end;
end;


{ TSpectrumKeysControl }

procedure TSpectrumKeysControl.DoOnChgSpectrumKey(Sender: TObject);
var
  B: TSpectrumKeyButtonControl;
  //NeedUncheck: Boolean;
  Flags: Integer;
begin
  if Assigned(FOnChgSpectrumKey) then
    FOnChgSpectrumKey(Sender);

  if Assigned(FOnChgSpectrumKeyEx) then begin
    if Sender is TSpectrumKeyButtonControl then begin
      B := TSpectrumKeyButtonControl(Sender);
      if B.GetChecked then begin
        Flags := 7;
        if (B <> FButtonCapsShift)
          and TSpectrumKeyButtonControl(FButtonCapsShift).GetChecked
        then begin
          Flags := 15;
        end else
          if B <> FButtonSymbolShift then begin
            if TSpectrumKeyButtonControl(FButtonSymbolShift).GetChecked then
              Flags := 15;
          end;
      end else
        Flags := 6;

      FOnChgSpectrumKeyEx(B.KeyValue, Flags);
    end;
  end;
end;

procedure TSpectrumKeysControl.CreateKeyButtons(ButtonsToggle: Boolean);
var
  H0, W0, Hb, Wb: Integer;
  K, L, I, J, M, N, FS, Q: Integer;
  B, B1: TSpectrumKeyButtonControl;
  CC, CC1, CC2: TCustomControl;
  KT: TKeyTexts;
  Sz: TSize;
  La, LabAbove, LabBelow, LabColour: TLabel;
  S: String;
  W: Word;
  G: TKeyGraphicShape;
  WR: WordRec absolute W;
begin
  InitColoursArr();
  DisableAutoSizing;
  try
    FS := MulDiv(GetFontData(Self.Font.Reference.Handle).Height, 10, 7);

    W0 := 0;
    Hb := 0;
    K := 0;
    for I := 0 to 3 do begin
      for L := 0 to 1 do begin
        B := nil;
        LabAbove := nil;
        CC := TCustomControl.Create(Self);
        if L = 0 then begin
          M := 3 - I;
        end else begin
          M := I + 4;
        end;
        for J := 0 to 4 do begin
          if L = 0 then
            N := J
          else
            N := 4 - J;

          WR.Hi := M;
          WR.Lo := N;

          S := UnitKeyMaps.DecodeSpectrumKeyTexts(W, KT);
          LabAbove := TLabel.Create(CC);
          LabAbove.Caption := KT[2];
          LabBelow := TLabel.Create(CC);
          LabBelow.Caption := KT[3];
          LabAbove.Anchors := [];
          LabBelow.Anchors := [];

          B1 := B;
          B := TSpectrumKeyButtonControl.Create(CC);

          B.Anchors := [];
          if Assigned(B1) then
            B.AnchorToNeighbour(akLeft, 7, B1)
          else
            B.AnchorParallel(akLeft, 7, CC);

          B.AnchorToNeighbour(akTop, 1, LabAbove);
          LabAbove.AnchorParallel(akLeft, 6, B);
          LabBelow.AnchorParallel(akLeft, 6, B);
          LabBelow.AnchorToNeighbour(akTop, 1, B);
          B.KeyValue := W;
          case W of
            0:
              FButtonCapsShift := B;
            $0701:
              FButtonSymbolShift := B;
          end;

          if I = 0 then begin
            La := B.LabSymbol;
            B.LabSymbol := B.LabCommand;
            B.LabCommand := La;

            Q := L * 5 + J;
            if Q <= 7 then begin
              KT[0] := ' ';
              G := TKeyGraphicShape.Create(B);
              G.Flags := ($F - ((Q + 1) and 7));
              if G.Flags and 4 = 0 then
                G.Flags := G.Flags xor $C;
              G.Anchors := [];
              G.AnchorParallel(akTop, 0, La);
              G.AnchorParallel(akRight, 0, La);
              G.AnchorParallel(akBottom, 0, La);
              G.Parent := La.Parent;
            end;

            LabColour := TLabel.Create(CC);
            LabColour.Font := Self.Font;

            Q := (Q + 1) mod 10;
            if Q <= 7 then begin
              LabColour.Caption := AnsiUpperCase(TCommonSpectrum.SpectrumColourNames[Q]);
              LabColour.Font.Color := ColoursArr[Q];
              if Q = 0 then begin
                LabColour.Transparent := False;
                LabColour.Color := ColoursArr[7];
                LabColour.Caption := ' ' + LabColour.Caption + ' ';
              end;
            end else
              LabColour.Caption := ' ';

            LabColour.AnchorParallel(akLeft, 0, LabAbove);
            LabAbove.AnchorToNeighbour(akTop, 1, LabColour);
            LabColour.AnchorParallel(akTop, 2, CC);
            LabColour.Parent := CC;
          end else begin
            LabAbove.AnchorParallel(akTop, 2, CC);

            if Trim(KT[0]) = '' then begin
              B.Lab.Alignment := TAlignment.taCenter;
              B.Lab.Layout := TTextLayout.tlCenter;

              B.Lab.AnchorParallel(akLeft, 0, B.Control0);
              B.Lab.AnchorParallel(akRight, 0, B.Control0);
              B.Lab.AnchorParallel(akTop, 0, B.Control0);
              B.Lab.AnchorParallel(akBottom, 0, B.Control0);

              if I = 3 then
                if N = 0 then
                  B.Tag := 2 * L + 13;
            end;
          end;

          B.Lab.Font := Self.Font;
          B.LabCommand.Font := B.Lab.Font;

          B.LabSymbol.Font := B.LabCommand.Font;
          La := B.LabSymbol;

          if B = FButtonSymbolShift then // symbol shift
            La := B.Lab;

          La.Font.Color := TColor($0b0ba9);

          if B.Lab.Alignment <> TAlignment.taCenter then
            B.Lab.Font.Height := FS;

          TCommonFunctionsLCL.CalculateTextSize(B.LabCommand.Font, KT[1], Sz);
          B.LabSymbol.Caption := KT[1]; // symbol text on button
          Wb := Sz.cx;
          H0 := Sz.cy;

          TCommonFunctionsLCL.CalculateTextSize(B.LabCommand.Font, KT[0], Sz);
          B.LabCommand.Caption := KT[0]; // command text on button
          if Sz.cx > Wb then
            Wb := Sz.cx;
          H0 := H0 + Sz.cy;

          B.DisplayText := S;
          TCommonFunctionsLCL.CalculateTextSize(B.Lab.Font, B.DisplayText, Sz);
          Wb := Wb + Sz.cx;
          if H0 < Sz.cy then
            H0 := Sz.cy;

          if W0 < Wb then
            W0 := Wb;
          if Hb < H0 then
            Hb := H0;

          LabBelow.Font := B.LabSymbol.Font;
          LabAbove.Font := B.LabSymbol.Font;
          if I = 0 then begin
            LabAbove.Font.Color := clWhite;
          end else begin
            LabAbove.Font.Color := TColor($34b934); //clGreen;
          end;
          LabBelow.Font.Color := TColor($4949FF);

          LabAbove.Parent := CC;
          B.Parent := CC;
          LabBelow.Parent := CC;

          Inc(K);
          Keys[K] := B;
          B.OnChange := @DoOnChgSpectrumKey;
          B.SetMouseEvents(ButtonsToggle);
        end;
        CC.AutoSize := True;
        CC.Anchors := [];
        if I = 0 then
          CC.AnchorParallel(akTop, 0, Self)
        else begin
          {$push} // turn off the warning, as when I <> 0, CC2 was initialized below.
          {$warn 5036 off : Local variable "$1" does not seem to be initialized}
          CC.AnchorToNeighbour(akTop, 0, CC2);
          {$pop}
        end;
        if L = 0 then begin
          CC.AnchorParallel(akLeft, (I mod 3) * B.Width div 3, Self);
          CC1 := CC;
        end else begin
          CC.AnchorToNeighbour(akLeft, 0, CC1);
          CC2 := CC;
        end;
        CC.Parent := Self;
      end;
    end;

    Wb := W0 * 5 div 6;
    Hb := Hb + 1;

    for I := Low(Keys) to High(Keys) do begin
      B := TSpectrumKeyButtonControl(Keys[I]);
      if B.Tag = 0 then
        B.Control0.Width := Wb
      else
        B.Control0.Width := MulDiv(W0, B.Tag, 12);

      B.Control0.Height := Hb;
      B.AutoSize := True;
    end;
  finally
    EnableAutoSizing;
  end;
end;

class procedure TSpectrumKeysControl.Init;
begin
  SetLength(ColoursArr, 0);
end;

class procedure TSpectrumKeysControl.InitColoursArr;
var
  C: UnitSpectrumColourMap.TLCLColourMap;
  I: Integer;
begin
  if Length(ColoursArr) = 0 then begin
    UnitSpectrumColourMap.GetDefaultSpectrumColoursD7(C);
    SetLength(ColoursArr, 8);
    for I := Low(ColoursArr) to High(ColoursArr) do begin
      ColoursArr[I] := C.Colours[False, I];
    end;
  end;
end;

procedure TSpectrumKeysControl.SetParent(NewParent: TWinControl);
var
  F: TCustomForm;
begin
  inherited SetParent(NewParent);
  F := GetParentForm(Self);
  if Assigned(F) then
    F.Color := BackgroundColour;
end;

constructor TSpectrumKeysControl.Create(AOwner: TComponent);
begin
  Create(AOwner, True);
end;

procedure TSpectrumKeysControl.SetKeyStates(const SpectrumKeys: TWordDynArray);
var
  I, J: Integer;
  B: TSpectrumKeyButtonControl;
  L: Boolean;
begin
  for J := 1 to 40 do begin
    L := False;
    B := TSpectrumKeyButtonControl(Keys[J]);
    for I := Low(SpectrumKeys) to High(SpectrumKeys) do begin
      if B.KeyValue = SpectrumKeys[I] then begin
        L := True;
        Break;
      end;
    end;
    B.SetChecked(L);
  end;

  DoOnChgSpectrumKey(Self);
end;

procedure TSpectrumKeysControl.GetKeyStates(out SpectrumKeys: TWordDynArray);
var
  I, K: Integer;
  B: TSpectrumKeyButtonControl;
begin
  K := 0;
  SetLength(SpectrumKeys{%H-}, 3);
  for I := 1 to 40 do begin
    B := TSpectrumKeyButtonControl(Keys[I]);
    if B.GetChecked then begin
      if K >= Length(SpectrumKeys) then
        SetLength(SpectrumKeys, (K * 7) div 5 + 2);
      SpectrumKeys[K] := B.KeyValue;
      Inc(K);
    end;
  end;
  SetLength(SpectrumKeys, K);
  TCommonSpectrum.SortSpectrumKeys(SpectrumKeys);
end;

function TSpectrumKeysControl.GetKeysString(out NumberOfPressedKeys: Integer
  ): String;
var
  I: Integer;
  MaybePlus: String;
  AW: TWordDynArray;
begin
  Result := '';
  MaybePlus := '';
  GetKeyStates(AW);
  NumberOfPressedKeys := Length(AW);
  for I := Low(AW) to High(AW) do begin
    Result := Result + MaybePlus + DecodeSpectrumKey(AW[I]);
    MaybePlus := ' + ';
  end;
end;

constructor TSpectrumKeysControl.Create(AOwner: TComponent;
  ButtonsToggle: Boolean);
begin
  inherited Create(AOwner);

  FButtonCapsShift := nil;
  FButtonSymbolShift := nil;
  FOnChgSpectrumKey := nil;
  FOnChgSpectrumKeyEx := nil;
  CreateKeyButtons(ButtonsToggle);
  SetKeyStates([]);
  AutoSize := True;
end;

procedure TSpectrumKeysControl.ReleaseShifts;

  procedure ReleaseButton(C: TCustomControl);
  var
    B: TSpectrumKeyButtonControl;
  begin
    B := TSpectrumKeyButtonControl(C);
    if B.GetChecked then
      B.SetChecked(False);
  end;

begin
  ReleaseButton(FButtonCapsShift);
  ReleaseButton(FButtonSymbolShift);
end;

{ TSpectrumKeyToggleButton }

function TSpectrumKeyButtonControl.GetDisplayText: String;
begin
  Result := FDisplayText;
end;

procedure TSpectrumKeyButtonControl.SetChecked(AValue: Boolean);
begin
  if AValue then begin
    Self.Color := clWhite;
    LabCommand.Font.Color := clNavy;
  end else begin
    Self.Color := TColor($9a8b80);
    LabCommand.Font.Color := clWhite;
  end;
  if KeyValue <> $0701 then // symbol shift
    Lab.Font.Color := LabCommand.Font.Color;

  if FChecked xor AValue then begin
    FChecked := AValue;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TSpectrumKeyButtonControl.GetChecked: Boolean;
begin
  Result := FChecked;
end;

type
  TAuxControl = class(TControl);

procedure TSpectrumKeyButtonControl.SetMouseEvents(AToggle: Boolean);
type
  TMouseEventType = (metDown, metUp);

  procedure SetMouseEvent(const C: TControl; AEvent: TMouseEvent; AType: TMouseEventType);
  var
    I: Integer;
    WC: TWinControl;
  begin
    if AType = metUp then
      TAuxControl(C).OnMouseUp := AEvent
    else
      TAuxControl(C).OnMouseDown := AEvent;

    if C is TWinControl then begin
      WC := TWinControl(C);
      for I := 0 to WC.ControlCount - 1 do begin
        SetMouseEvent(WC.Controls[I], AEvent, AType);
      end;
    end;
  end;

begin
  Toggle := AToggle;
  if AToggle then
    SetMouseEvent(Self, @MouseEventToggle, metDown)
  else
    case KeyValue of
      0, $0701: // caps shift or symbol shift
        SetMouseEvent(Self, @MouseEventToggle, metDown);
    otherwise
      SetMouseEvent(Self, @MouseEventPush, metDown);
      SetMouseEvent(Self, @MouseEventRelease, metUp);
    end;

  Application.AddOnUserInputHandler(@UserInputEvent);
end;

procedure TSpectrumKeyButtonControl.SetDisplayText(AValue: String);
begin
  FDisplayText := Trim(AValue);
  AValue := StringReplace(AValue, ' ', LineEnding, [rfReplaceAll]);
  Lab.Caption := UpperCase(StringReplace(AValue, '/', LineEnding, [rfReplaceAll]));
end;

procedure TSpectrumKeyButtonControl.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange <> AValue then
    FOnChange := AValue;
end;

procedure TSpectrumKeyButtonControl.MouseEventToggle(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetChecked(not GetChecked);
end;

procedure TSpectrumKeyButtonControl.MouseEventPush(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetChecked(True);
end;

procedure TSpectrumKeyButtonControl.MouseEventRelease(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetChecked(False);
end;

procedure TSpectrumKeyButtonControl.UserInputEvent(Sender: TObject; Msg: Cardinal
  );
begin
  case Msg of
    LM_MOUSEMOVE, LM_MOUSEENTER, LM_MOUSELEAVE:
      begin
        if (CanSetFocus and MouseInClient) xor LastMouseInClient then begin
          LastMouseInClient := not LastMouseInClient;

          if not (Toggle or LastMouseInClient) then begin
            case KeyValue of
              0, $0701: // caps shift or symbol shift
                ;
            otherwise
              SetChecked(False);
            end;
          end;

          Invalidate;
        end;
      end;
  otherwise
  end;
end;

constructor TSpectrumKeyButtonControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Toggle := False;
  FDisplayText := '';
  AutoSize := False;
  LastMouseInClient := False;
  FOnChange := nil;

  Control0 := TCustomControl.Create(Self);
  Control0.Anchors := [];
  Control0.AnchorParallel(akLeft, 6, Self);
  Control0.AnchorParallel(akTop, 5, Self);
  Control0.BorderSpacing.Right := 6;
  Control0.BorderSpacing.Bottom := 5;
  Control0.AutoSize := False;

  Lab := TLabel.Create(Control0);
  Lab.ShowAccelChar := False;
  Lab.Anchors := [];
  Lab.AnchorParallel(akTop, 0, Control0);
  Lab.AnchorParallel(akLeft, 0, Control0);

  LabSymbol := TLabel.Create(Control0);
  LabSymbol.ShowAccelChar := False;
  LabSymbol.Anchors := [];

  LabSymbol.AnchorParallel(akRight, 0, Control0);
  LabSymbol.AnchorParallel(akTop, 0, Control0);
  LabSymbol.Alignment := TAlignment.taCenter;
  LabSymbol.Layout := TTextLayout.tlCenter;

  LabCommand := TLabel.Create(Control0);
  LabCommand.ShowAccelChar := False;
  LabCommand.Anchors := [];
  LabCommand.AnchorParallel(akRight, 0, Control0);
  LabCommand.AnchorParallel(akBottom, 0, Control0);

  Lab.Parent := Control0;
  LabSymbol.Parent := Control0;
  LabCommand.Parent := Control0;
  Control0.Parent := Self;

  Width := 80;
  Height := 40;

  BorderWidth := 1;
  BorderStyle := bsSingle;
end;

destructor TSpectrumKeyButtonControl.Destroy;
begin
  Application.RemoveAllHandlersOfObject(Self);
  inherited Destroy;
end;

procedure TSpectrumKeyButtonControl.Paint;
var
  R: TRect;
begin
  inherited Paint;

  if LastMouseInClient then begin
    R := Self.ClientRect;
    R := Rect(R.Left + 2, R.Top + 2, R.Right - 3, R.Bottom - 3);

    Canvas.Pen.Color := Self.LabCommand.Font.Color;

    Canvas.Polyline([
      R.TopLeft, Point(R.Right, R.Top), R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft
    ]);
    InflateRect(R, -1, -1);
    Canvas.Polyline([
      R.TopLeft, Point(R.Right, R.Top), R.BottomRight, Point(R.Left, R.Bottom), R.TopLeft
    ]);

  end;
end;

initialization
  TSpectrumKeysControl.Init();

end.

