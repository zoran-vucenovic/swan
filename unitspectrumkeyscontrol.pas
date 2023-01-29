unit UnitSpectrumKeysControl;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, UnitKeyMaps, CommonFunctionsLCL, UnitCommonSpectrum,
  Controls, StdCtrls, Graphics, LMessages, Forms;

type

  TArrayOfWord = array of Word;

  TSpectrumKeysControl = class(TCustomControl)
  strict private
    type
      TToggleButtons = array [1..40] of TCustomControl;

  strict private
    Keys: TToggleButtons;
    procedure DoOnChgSpectrumKey(Sender: TObject);
    procedure CreateKeyButtons;
  public
    FOnChgSpectrumKey: TNotifyEvent;
    procedure SetKeyStates(const SpectrumKeys: TArrayOfWord);
    procedure GetKeyStates(out SpectrumKeys: TArrayOfWord);
    function GetKeysString(out NumberOfPressedKeys: Integer): String;
    constructor Create(AOwner: TComponent); override;
  end;


implementation

type

  TSpectrumKeyToggleButton = class(TCustomControl)
  strict private
    FDisplayText: String;
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    LastMouseInClient: Boolean;
    function GetDisplayText: String;
    procedure SetDisplayText(AValue: String);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure MouseUpEvent(Sender: TObject; {%H-}Button: TMouseButton;
                      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure UserInputEvent(Sender: TObject; Msg: Cardinal);
  private
    KeyValue: Word;
    Lab: TLabel;
    LabCommand: TLabel;
    LabSymbol: TLabel;
    Control0: TCustomControl;
  protected
    procedure SetChecked(AValue: Boolean);
    function GetChecked: Boolean;
    property DisplayText: String read GetDisplayText write SetDisplayText;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;


{ TSpectrumKeysControl }

procedure TSpectrumKeysControl.DoOnChgSpectrumKey(Sender: TObject);
begin
  if Assigned(FOnChgSpectrumKey) then
    FOnChgSpectrumKey(Sender);
end;

procedure TSpectrumKeysControl.CreateKeyButtons;
var
  HTs0, HTs, WTs, WLg: Integer;
  K, L, I, J, M, N, FS: Integer;
  B, B1: TSpectrumKeyToggleButton;
  CC, CC1, CC2, CCA: TCustomControl;
  KT: TKeyTexts;
  Sz: TSize;
  La, LabAbove, LabBelow: TLabel;
  S: String;
  W: Word;
  WR: WordRec absolute W;
begin
  DisableAutoSizing;
  try
    FS := GetFontData(Self.Font.Reference.Handle).Height;

    WLg := 0;
    HTs := 0;
    WTs := 0;
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

          LabAbove.AnchorParallel(akTop, 2, CC);

          B1 := B;
          B := TSpectrumKeyToggleButton.Create(CC);

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

          if I = 0 then begin
            La := B.LabSymbol;
            B.LabSymbol := B.LabCommand;
            B.LabCommand := La;
          end else begin
            if Trim(KT[0]) = '' then begin
              B.Lab.Alignment := TAlignment.taCenter;
              B.Lab.Layout := TTextLayout.tlCenter;

              CCA := TCustomControl.Create(B);

              CCA.Parent := B;

              CCA.AnchorParallel(akLeft, 0, B.Control0);
              CCA.AnchorParallel(akRight, 0, B.Control0);
              CCA.AnchorParallel(akTop, 0, B.Control0);
              CCA.AnchorParallel(akBottom, 0, B.LabCommand);

              B.Lab.Parent := CCA;
              B.Lab.AnchorParallel(akLeft, 0, CCA);
              B.Lab.AnchorParallel(akRight, 0, CCA);
              B.Lab.AnchorParallel(akTop, 0, CCA);
              B.Lab.AnchorParallel(akBottom, 0, CCA);

              CCA.BringToFront;
            end;
          end;

          B.Lab.Font := Self.Font;
          B.LabCommand.Font := B.Lab.Font;

          B.LabSymbol.Font := B.LabCommand.Font;
          La := B.LabSymbol;
          if (I = 3) and (L = 1) and (J = 3) then // symbol shift
            La := B.Lab;

          La.Font.Color := TColor($000099);

          if B.Lab.Alignment <> TAlignment.taCenter then
            B.Lab.Font.Size := FS * 4 div 3;

          TCommonFunctionsLCL.CalculateTextSize(B.Lab.Font, S, Sz);
          B.DisplayText := S;

          WTs := Sz.cx;
          HTs0 := Sz.cy;

          TCommonFunctionsLCL.CalculateTextSize(B.LabSymbol.Font, KT[1], Sz);
          B.LabSymbol.Caption := KT[1]; // symbol text on button

          WTs := WTs + 2 + Sz.cx;
          if WLg < WTs then
            WLg := WTs;
          if HTs0 < Sz.cy then
            HTs0 := Sz.cy;
          if HTs < HTs0 then
            HTs := HTs0;

          TCommonFunctionsLCL.CalculateTextSize(B.LabSymbol.Font, KT[0], Sz);
          B.LabCommand.Caption := KT[0]; // command text on button

          if WLg < Sz.cx then
            WLg := Sz.cx;

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

    for I := Low(Keys) to High(Keys) do begin
      B := TSpectrumKeyToggleButton(Keys[I]);
      B.Control0.Width := WLg + 3;
      B.Control0.Height := HTs + 2;
      B.AutoSize := True;
    end;
  finally
    EnableAutoSizing;
  end;
end;

procedure TSpectrumKeysControl.SetKeyStates(const SpectrumKeys: TArrayOfWord);
var
  I, J: Integer;
  B: TSpectrumKeyToggleButton;
  L: Boolean;
begin
  for J := 1 to 40 do begin
    L := False;
    B := TSpectrumKeyToggleButton(Keys[J]);
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

procedure TSpectrumKeysControl.GetKeyStates(out SpectrumKeys: TArrayOfWord);
var
  I, K: Integer;
  B: TSpectrumKeyToggleButton;
begin
  K := 0;
  SetLength(SpectrumKeys{%H-}, 3);
  for I := 1 to 40 do begin
    B := TSpectrumKeyToggleButton(Keys[I]);
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
  AW: TArrayOfWord;
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

constructor TSpectrumKeysControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnChgSpectrumKey := nil;
  CreateKeyButtons;
  SetKeyStates([]);
  AutoSize := True;
end;

{ TSpectrumKeyToggleButton }

function TSpectrumKeyToggleButton.GetDisplayText: String;
begin
  Result := FDisplayText;
end;

procedure TSpectrumKeyToggleButton.SetChecked(AValue: Boolean);
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

function TSpectrumKeyToggleButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TSpectrumKeyToggleButton.SetDisplayText(AValue: String);
begin
  FDisplayText := Trim(AValue);
  Lab.Caption := UpperCase(StringReplace(StringReplace(AValue, ' ', '/', [rfReplaceAll]), '/', LineEnding, [rfReplaceAll]));
end;

procedure TSpectrumKeyToggleButton.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange <> AValue then
    FOnChange := AValue;
end;

procedure TSpectrumKeyToggleButton.MouseUpEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetChecked(not GetChecked);
end;

procedure TSpectrumKeyToggleButton.UserInputEvent(Sender: TObject; Msg: Cardinal
  );
var
  B: boolean;
begin
  case Msg of
    LM_MOUSEMOVE:
      begin
        B := CanSetFocus and MouseInClient;
        if B xor LastMouseInClient then begin
          LastMouseInClient := B;
          Invalidate;
        end;

      end;
  otherwise
  end;
end;

type
  TAuxControl = class(TControl);

constructor TSpectrumKeyToggleButton.Create(AOwner: TComponent);

  procedure SetOnMouseUp(const C: TControl);
  var
    I: Integer;
    WC: TWinControl;
  begin
    TAuxControl(C).OnMouseUp := @MouseUpEvent;
    if C is TWinControl then begin
      WC := TWinControl(C);
      for I := 0 to WC.ControlCount - 1 do begin
        SetOnMouseUp(WC.Controls[I]);
      end;
    end;
  end;

begin
  inherited Create(AOwner);

  FDisplayText := '';
  AutoSize := False;
  LastMouseInClient := False;
  FOnChange := nil;

  Control0 := TCustomControl.Create(Self);
  Control0.Anchors := [];
  Control0.AnchorParallel(akLeft, 6, Self);
  Control0.AnchorParallel(akTop, 5, Self);
  Control0.BorderSpacing.Right := 6;
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
  LabSymbol.AnchorVerticalCenterTo(Lab);
  LabSymbol.Alignment := TAlignment.taCenter;
  LabSymbol.Layout := TTextLayout.tlCenter;

  LabCommand := TLabel.Create(Self);
  LabCommand.ShowAccelChar := False;
  LabCommand.Anchors := [];
  LabCommand.AnchorParallel(akRight, 0, Control0);
  LabCommand.AnchorToNeighbour(akTop, 1, Control0);

  LabCommand.BorderSpacing.Bottom := 5;
  LabCommand.BorderSpacing.Left := 6;

  Lab.Parent := Control0;
  LabSymbol.Parent := Control0;
  Control0.Parent := Self;
  LabCommand.Parent := Self;

  Width := 80;
  Height := 40;

  BorderWidth := 1;
  BorderStyle := bsSingle;

  SetOnMouseUp(Self);

  Application.AddOnUserInputHandler(@UserInputEvent);
end;

destructor TSpectrumKeyToggleButton.Destroy;
begin
  Application.RemoveAllHandlersOfObject(Self);
  inherited Destroy;
end;

procedure TSpectrumKeyToggleButton.Paint;
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

end.

