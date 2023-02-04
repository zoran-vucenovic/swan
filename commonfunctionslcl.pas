unit CommonFunctionsLCL;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, LCLIntf, Graphics, Controls,
  StdCtrls, Grids;

type

  TCommonFunctionsLCL = class sealed (TObject)
  strict private
    class var
      Canv: TCanvas;
  private
    class procedure Init; static;
    class procedure Final; static;
  strict private
    type
      TLinkLabel = class(TCustomLabel)
      strict private
        class var
          GlobalCounter: Int64;
      private
        class procedure Init; static;
      protected
        procedure MouseEnter; override;
        procedure MouseLeave; override;
      public
        constructor Create(AOwner: TComponent; const ACaption: String); virtual;
        constructor Create(AOwner: TComponent); override;
        property ShowAccelChar default False;
      end;

  public
    class function CreateLinkLabel(AOwner: TComponent; const ACaption: String = ''): TCustomLabel;
    class procedure FormToScreenCentre(Form: TCustomForm); static;
    class procedure GrowFormHeight(F: TCustomForm); static;
    class procedure AdjustFormPos(Form: TCustomForm; Horizontal: Boolean = False); static;
    class procedure CalculateTextSize(const F: TFont;
        const S: String; out ATextSize: TSize); static;
    class procedure RowInView(AGrid: TCustomGrid; ARow: Integer); static;
  end;

implementation

{ TCommonFunctionsLCL.TLinkLabel }

class procedure TCommonFunctionsLCL.TLinkLabel.Init;
begin
  GlobalCounter := 0;
end;

procedure TCommonFunctionsLCL.TLinkLabel.MouseEnter;
begin
  inherited MouseEnter;

  Font.Style := Font.Style + [fsBold];
end;

procedure TCommonFunctionsLCL.TLinkLabel.MouseLeave;
begin
  inherited MouseLeave;

  Font.Style := Font.Style - [fsBold];
end;

constructor TCommonFunctionsLCL.TLinkLabel.Create(AOwner: TComponent;
  const ACaption: String);
var
  S: String;
begin
  inherited Create(AOwner);

  ShowAccelChar := False;
  S := StringReplace(ClassName, '.', '_', [rfReplaceAll]);
  Name := S + GlobalCounter.ToHexString(16);
  Inc(GlobalCounter);
  if ACaption = '' then
    Caption := ' '
  else
    Caption := ACaption;
  AutoSize := True;
  ShowHint := True;
  Font.Color := clBlue;
  Font.Style := Font.Style + [fsUnderline];
  Cursor := crHandPoint;
end;

constructor TCommonFunctionsLCL.TLinkLabel.Create(AOwner: TComponent);
begin
  Create(AOwner, ' ');
end;

class procedure TCommonFunctionsLCL.CalculateTextSize(const F: TFont;
  const S: String; out ATextSize: TSize);

begin
  if Canv = nil then begin
    Canv := TCanvas.Create;
    Canv.Handle := GetDC(0);
  end;
  Canv.Font := F;
  ATextSize := Canv.TextExtent(S);
end;

type
  TGridAccessProtected = class(TCustomGrid);

class procedure TCommonFunctionsLCL.RowInView(AGrid: TCustomGrid; ARow: Integer
  );
var
  N: Integer;
  G: TGridAccessProtected;
begin
  G := TGridAccessProtected(AGrid);
  G.BeginUpdate;
  try
    N := ARow - 2;
    while ((N < ARow) and (not G.MoveExtend(False, G.Col, N, False))) do begin
      Inc(N);
    end;
    N := ARow + 2;
    while ((N > ARow) and (not G.MoveExtend(False, G.Col, N, False))) do begin
      Dec(N);
    end;
    G.MoveExtend(False, G.Col, ARow, True);
  finally
    G.EndUpdate;
  end;
end;

class procedure TCommonFunctionsLCL.Init;
begin
  Canv := nil;
  TLinkLabel.Init;
end;

class procedure TCommonFunctionsLCL.Final;
begin
  if Assigned(Canv) then begin
    if Canv.HandleAllocated then
      ReleaseDC(0, Canv.Handle);

    Canv.Free;
  end;
end;

class function TCommonFunctionsLCL.CreateLinkLabel(AOwner: TComponent;
  const ACaption: String): TCustomLabel;
begin
  Result := TLinkLabel.Create(AOwner, ACaption);
end;

class procedure TCommonFunctionsLCL.FormToScreenCentre(Form: TCustomForm);
var
  M: TMonitor;
  R, FormRect: TRect;
  L, T: Integer;
begin
  Form.HandleNeeded;
  if Form.HandleAllocated then begin
    M := Screen.MonitorFromWindow(Form.Handle);
    R := M.WorkareaRect;
    if (R.Right <= R.Left) or (R.Bottom <= R.Top) then
      R := M.BoundsRect;

    T := R.Bottom - R.Top;
    L := R.Right - R.Left;

    if GetWindowRect(Form.Handle, FormRect{%H-}) <> 0 then begin
      L := L - FormRect.Width;
      T := T - FormRect.Height;
    end else begin
      T := T - Form.Height;
      L := L - Form.Width;
    end;

    T := T div 2;
    if T < R.Top then
      T := R.Top;

    L := L div 2;
    if L < R.Left then
      L := R.Left;

    Form.SetBounds(L, T, Form.Width, Form.Height);
  end;

end;

class procedure TCommonFunctionsLCL.GrowFormHeight(F: TCustomForm);
var
  M: TMonitor;
  H: Integer;
  R: TRect;
begin
  F.HandleNeeded;
  M := Screen.MonitorFromWindow(F.Handle);
  R := M.WorkareaRect;
  if R.Height <= 0 then
    R := M.BoundsRect;

  H := (R.Height * 21) div 23;
  if F.BorderStyle <> TFormBorderStyle.bsNone then
    H := H - 26;
  if F.Height < H then
    F.Height := H;
end;

class procedure TCommonFunctionsLCL.AdjustFormPos(Form: TCustomForm;
  Horizontal: Boolean);
var
  M: TMonitor;
  R: TRect;
  P: TPoint;
  N, K, Nn: Integer;
  W, H: Integer;
  SpectrumForm: TCustomForm;
begin
  SpectrumForm := Application.MainForm;
  if Assigned(SpectrumForm) and SpectrumForm.HandleAllocated and SpectrumForm.IsVisible then begin
    M := Screen.MonitorFromWindow(SpectrumForm.Handle);

    if Assigned(M) then begin
      R := M.WorkareaRect;
      if (R.Left >= R.Right) or (R.Top >= R.Bottom) then
        R := M.BoundsRect;

      W := Form.Width;
      H := Form.Height;
      if W > R.Width then
        W := R.Width;
      if H > R.Height then
        H := R.Height;

      N := 6;
      Nn := N;
      if not Horizontal then begin
        P.X := (R.Width - SpectrumForm.Width) div 2;
        if SpectrumForm.Left < P.X then begin
          P.X := SpectrumForm.BoundsRect.Right + N;
        end else begin
          P.X := SpectrumForm.Left - W - N;
          if Form.BorderStyle <> bsNone then
            P.X := P.X - N;
        end;

        K := R.Width - W;

        if Form.BorderStyle <> bsNone then begin
          R.Bottom := R.Bottom - 27 - N;
          K := K - N - 3;
        end;
        if P.X > K then
          P.X := K;

        P.Y := SpectrumForm.BoundsRect.Top;
        if P.Y + H > R.Bottom then begin
          P.Y := R.Bottom - H; // - N;
        end;

      end else begin
        K := R.Height - H;

        if Form.BorderStyle <> bsNone then begin
          Nn := N + 27;
          R.Right := R.Right - N;
          K := K - 27;
        end;

        P.Y := (R.Height - SpectrumForm.Height) div 2;
        if SpectrumForm.Top < P.Y then begin
          P.Y := SpectrumForm.BoundsRect.Bottom + Nn + N;
        end else begin
          P.Y := SpectrumForm.Top - H - Nn;
        end;

        if P.Y > K then
          P.Y := K;

        P.X := SpectrumForm.BoundsRect.Left;
        if P.X + W > R.Right then begin
          P.X := R.Right - W; // - N;
        end;

      end;

      if P.X < 0 then
        P.X := 0;
      if P.Y < 0 then
        P.Y := 0;

      Form.SetBounds(P.X, P.Y, W, H);
    end;
  end;
end;

initialization
  TCommonFunctionsLCL.Init;

finalization
  TCommonFunctionsLCL.Final;

end.

