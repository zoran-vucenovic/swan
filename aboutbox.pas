unit AboutBox;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc}
{$macro on}

{$define step_by_step_disappear}

interface

uses
  Classes, SysUtils, Math, CommonFunctionsLCL, UnitVer, UnitColourFunctions,
  UnitCommonSpectrum, Forms, Controls, Graphics,
  {$ifdef mswindows}
  LCLPlatformDef,
  {$endif}
  Dialogs, ExtCtrls, StdCtrls, LCLType, InterfaceBase, Buttons, LCLIntf,
  LazVersion;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Shape1: TShape;

    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  strict private
    const
      BorderLineWidth = 2;
    class var
      FBuildDate: String;
      FormAbout: TFormAbout;

  strict private
    WasShown: Boolean;
    CloseButton: TCustomControl;
    Image1: TImage;
    WC: TWinControl;

    {$ifdef step_by_step_disappear}
    Tim: TTimer;

    procedure StartDisappearing;
    procedure DisappearStep;
    procedure DoOnTimer(Sender: TObject);
    procedure CreateTimer;
    {$endif}

    procedure StopDisappearing;
    procedure AfterShow(Data: PtrInt);
    procedure OnCloseButtonClick(Sender: TObject);
    procedure DoAdjustPos;
    procedure OpenHintUrl(Sender: TObject);

  private
    class procedure Init; static;
    class procedure Final; static;
  public
    class procedure ShowAbout(); static;
    class property BuildDate: String write FBuildDate;
  end;

implementation

uses Types;

{$R *.lfm}

type
  TFormAboutCloseButton = class(TCustomControl)
  private
    IconClose: TGraphic;
    IconPosition: TPoint;
    IsDown: Boolean;

    procedure CalculateIconPosition;
    procedure GoDown;
    procedure GoUp;
  protected
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;

{ TFormAboutCloseButton }

procedure TFormAboutCloseButton.CalculateIconPosition;
const
  BorderSpc = 7;
begin
  ClientHeight := 2 * BorderSpc + IconClose.Height;
  IconPosition.y := (ClientHeight - IconClose.Height) div 2;
  IconPosition.x := (ClientWidth - IconClose.Width) div 2;
end;

procedure TFormAboutCloseButton.GoDown;
begin
  IsDown := True;
  Invalidate;
end;

procedure TFormAboutCloseButton.GoUp;
begin
  IsDown := False;
  Invalidate;
end;

procedure TFormAboutCloseButton.MouseEnter;
begin
  inherited MouseEnter;
  Invalidate;
end;

procedure TFormAboutCloseButton.MouseLeave;
begin
  inherited MouseLeave;
  Invalidate;
end;

procedure TFormAboutCloseButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  GoDown;
end;

procedure TFormAboutCloseButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  GoUp;
end;

constructor TFormAboutCloseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IconClose := GetButtonIcon(LCLType.idButtonClose);
  IsDown := False;
end;

destructor TFormAboutCloseButton.Destroy;
begin
  IconClose.Free;
  IconClose := nil;
  inherited Destroy;
end;

procedure TFormAboutCloseButton.Paint;
const
  FrameColour = $81A8D1;
var
  R: TRect;
  N: Integer;

begin
  inherited Paint;

  Canvas.Pen.Width := 1;
  Canvas.Brush.Style := Graphics.bsSolid;
  R := ClientRect;

  if IsDown then
    N := 5
  else if MouseInClient and R.Contains(ScreenToClient(Mouse.CursorPos)) then
    N := 4
  else
    N := 0;

  if N > 1 then begin
    Canvas.Pen.Color := TColourFunctions.MakeLighterColour(FrameColour, N / 2);
    Canvas.Brush.Color := TColourFunctions.MakeLighterColour(FrameColour, N);
  end else begin
    Canvas.Pen.Color := TColourFunctions.MakeLighterColour(FrameColour, 3);
    Canvas.Brush.Color := clWhite;
  end;

  R.Right := R.Right - 1;
  R.Bottom := R.Bottom - 1;

  Canvas.RoundRect(R, 5, 5);

  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := Graphics.bsClear;

  Canvas.Draw(IconPosition.x, IconPosition.y, IconClose);
end;

{ TFormAbout }

procedure TFormAbout.FormShow(Sender: TObject);

var
  FontHeight: Integer;
  FD: TFontData;
  B: TFormAboutCloseButton;
begin
  if not WasShown then begin
    WasShown := True;
    Label1.Font.SetDefault;
    Label1.Font.Color := clNavy;
    Label5.Font := Label1.Font;
    Label5.Font.Style := Label5.Font.Style + [fsBold];

    FD := GetFontData(Label1.Font.Handle);
    FontHeight := MulDiv(FD.Height, 14, 5);
    if Abs(FontHeight) < 10 then
      FontHeight := Sign(FontHeight) * 10;

    Label1.Font.Height := FontHeight;

    Label2.Font.SetDefault;
    Label3.Font := Label2.Font;      
    Label7.Font := Label2.Font;
    FontHeight := MulDiv(FD.Height, 6, 5);
    if Abs(FontHeight) < 10 then
      FontHeight := Sign(FontHeight) * 10;

    Label2.Font.Height := FontHeight;

    //FontHeight := MulDiv(FD.Height, 6, 5);
    //if Abs(FontHeight) < 10 then
    //  FontHeight := Sign(FontHeight) * 10;

    Label7.Font.Height := FontHeight;

    Label3.Font.Style := Label2.Font.Style + [fsBold];

    if CloseButton is TFormAboutCloseButton then begin
      B := TFormAboutCloseButton(CloseButton);
      B.CalculateIconPosition;
      B.OnClick := @OnCloseButtonClick;
    end;
    AfterShow(2);
  end else
    DoAdjustPos;
end;

{$ifdef step_by_step_disappear}
procedure TFormAbout.StartDisappearing;
begin
  if Tim.Enabled then
    Exit;
  AlphaBlend := True;

  if Self.Active and Application.MainForm.Visible then begin
    Application.MainForm.SetFocus;
  end;
  DisappearStep;
end;

procedure TFormAbout.DisappearStep;
begin
  Tim.Enabled := False;
  if AlphaBlendValue < 10 then
    Close
  else begin
    if AlphaBlendValue > 49 then
      AlphaBlendValue := AlphaBlendValue - 27
    else
      AlphaBlendValue := AlphaBlendValue div 2;

    Tim.Enabled := True;
  end;
end;

procedure TFormAbout.DoOnTimer(Sender: TObject);
begin
  DisappearStep;
end;

procedure TFormAbout.CreateTimer;
begin
  Tim := TTimer.Create(Self);
  Tim.Enabled := False;
  Tim.Interval := 70;
  Tim.OnTimer := @DoOnTimer;
end;

{$endif}

procedure TFormAbout.StopDisappearing;
begin
  {$ifdef step_by_step_disappear}
  Tim.Enabled := False;
  {$endif}
  AlphaBlend := False;
  AlphaBlendValue := $FF;
end;

procedure TFormAbout.FormCreate(Sender: TObject);
var
  C: TControl;

  procedure AddIcon;
  begin
    Image1 := TImage.Create(WC.Owner);
    Image1.Anchors := [];
    Image1.Center := True;
    Image1.AutoSize := True;
    Image1.AnchorToNeighbour(akRight, 10, WC);
    Image1.AnchorVerticalCenterTo(WC.Parent);

    Image1.Picture.Icon := Application.Icon;
    Image1.Parent := WC.Parent;
  end;

  procedure AddALabel(const S: String; S2: String = '');
  var
    L1, L2: TLabel;
    CC: TCustomControl;
  begin
    CC := TCustomControl.Create(Panel8);
    L2 := TLabel.Create(CC);
    L2.ShowAccelChar := False;
    L2.Anchors := [];

    if WC = nil then begin
      WC := TCustomControl.Create(Panel8);
      WC.Anchors := [];
      WC.AnchorParallel(akTop, 0, Panel8);
      WC.AnchorParallel(akRight, 0, Panel8);
      WC.AutoSize := True;
      WC.Parent := Panel8;
      CC.AnchorParallel(akTop, 0, WC);
      AddIcon;
    end else begin
      CC.AnchorToNeighbour(akTop, 5, C);
    end;
    C := CC;

    L2.AnchorParallel(akRight, 0, CC);

    if S2 = '' then begin
      L1 := L2;
    end else begin
      L2.Caption := ' ' + S2;
      L2.AnchorParallel(akTop, 0, CC);

      L1 := TLabel.Create(CC);
      L1.ShowAccelChar := False;
      L1.Anchors := [];
      L1.AnchorToNeighbour(akRight, 0, L2);
      L1.Font.Style := L1.Font.Style + [fsBold];
      L2.Parent := CC;
    end;

    L1.AnchorParallel(akTop, 0, CC);
    L1.Caption := S;
    L1.Parent := CC;

    CC.AnchorParallel(akRight, 0, WC);
    CC.AutoSize := True;
    CC.Parent := WC;
  end;

  function CreateLinkLabel(AOwner: TComponent; const AUrl: String; ACaption: String; const LeftLabelText: String = ''; const RightLabelText: String = ''): TCustomControl;
  var
    L, LL: TCustomLabel;
  begin
    Result := TCustomControl.Create(AOwner);
    Result.Anchors := [];

    if ACaption = '' then
      ACaption := AUrl;
    LL := TCommonFunctionsLCL.CreateLinkLabel(Result, ACaption);
    LL.ShowHint := True;
    LL.Hint := AUrl;
    LL.OnClick := @OpenHintUrl;
    LL.Anchors := [];
    LL.AnchorVerticalCenterTo(Result);

    if LeftLabelText <> '' then begin
      L := TCustomLabel.Create(Result);
      L.Caption := LeftLabelText;
      L.Anchors := [];
      L.AnchorVerticalCenterTo(Result);
      L.AnchorParallel(akLeft, 0, Result);
      LL.AnchorToNeighbour(akLeft, 0, L);
      L.Parent := Result;
    end else
      LL.AnchorParallel(akLeft, 0, Result);

    LL.Parent := Result;

    if RightLabelText <> '' then begin
      L := TCustomLabel.Create(Result);
      L.Caption := RightLabelText;
      L.Anchors := [];
      L.AnchorVerticalCenterTo(Result);
      L.AnchorToNeighbour(akLeft, 0, LL);
      L.Parent := Result;
    end;

    Result.AutoSize := True;
  end;

  procedure AddLinks;
  var
    S: String;
    CC1, CC2, CC3: TCustomControl;
  begin
    S := Format('Lazarus IDE %d.%d', [laz_major, laz_minor])
    {$if (laz_release > 0) or (laz_patch > 0)}
      + '.' + IntToStr(laz_release)
      {$if laz_patch > 0}
         + '.' + IntToStr(laz_patch)
      {$endif}
    {$endif}
    ;
    CC1 := CreateLinkLabel(Panel5, 'https://www.lazarus-ide.org/', S, 'Created with: ');
    CC1.AnchorParallel(akTop, 0, Panel5);
    CC1.AnchorHorizontalCenterTo(Panel5);
    CC1.Parent := Panel5;

    S := Format('Free Pascal compiler %d.%d', [fpc_version, fpc_release])
    {$if fpc_patch > 0}
      + '.' + IntToStr(fpc_patch)
    {$endif}
    ;
    CC2 := CreateLinkLabel(Panel5, 'https://www.freepascal.org/', S, 'Compiled with: ');
    CC2.AnchorToNeighbour(akTop, 5, CC1);
    CC2.AnchorHorizontalCenterTo(Panel5);
    CC2.Parent := Panel5;

    Label6.Caption := ApplicationName + ' uses:';
    Label6.Anchors := [];
    Label6.AnchorParallel(akTop, 0, Panel6);

    CC3 := TCustomControl.Create(Panel6);
    CC3.Anchors := [];
    CC3.AnchorHorizontalCenterTo(Panel6);
    //
    CC1 := CreateLinkLabel(CC3, 'http://www.portaudio.com/', 'PortAudio', '', ' sound library');
    CC1.AnchorParallel(akTop, 0, CC3);
    CC1.AnchorParallel(akLeft, 0, CC3);
    CC1.Parent := CC3;
    //
    CC2 := CreateLinkLabel(CC3, 'https://github.com/bgrabitmap/bgrabitmap/', 'BGRABitmap', '', ' graphic library');
    CC2.AnchorToNeighbour(akTop, 5, CC1);
    CC2.AnchorParallel(akLeft, 0, CC3);
    CC2.Parent := CC3;

    CC1 := CreateLinkLabel(Panel6, 'https://gitlab.com/freepascal.org/lazarus/lazarus/-/tree/main/images/general_purpose', 'General purpose icons', '', ' by Roland Hahn');
    CC1.AnchorToNeighbour(akTop, 5, CC2);
    CC1.AnchorParallel(akLeft, 0, CC3);
    CC1.Parent := CC3;

    CC1 := TCustomControl.Create(Panel6);
    CC1.Anchors := [];
    CC1.AnchorToNeighbour(akRight, 0, CC3);
    CC1.AnchorParallel(akTop, 0, CC3);
    CC1.Width := 11;
    CC3.AnchorToNeighbour(akTop, 2, Label6);
    CC3.AutoSize := True;
    CC3.Parent := Panel6;

    CC1.Parent := Panel6;

    Label6.AnchorParallel(akLeft, 0, CC1);
  end;

var
  C1: TCustomControl;
  S, S1: String;
  B: TFormAboutCloseButton;

begin
  {$ifdef step_by_step_disappear}
  CreateTimer;
  {$endif}

  Label5.Caption := 'ZX Spectrum 48K emulator';

  Color := clWhite;

  C1 := CreateLinkLabel(Panel9,
    'https://github.com/zoran-vucenovic/swan', ''
    //, 'GitHub page '
    );
  C1.AnchorParallel(akTop, 0, Panel9);
  C1.AnchorHorizontalCenterTo(Panel9);
  C1.Parent := Panel9;

  Panel9.BevelOuter := bvNone;
  Panel9.Caption := '';
  Panel9.AutoSize := True;

  Label7.Caption := 'The Swan icon created by Handoko';

  Label4.Caption :=
    'Spectrum 48k rom image, included in the executable, is copyright';

  C1 := CreateLinkLabel(Panel7,
    'https://groups.google.com/g/comp.sys.amstrad.8bit/c/HtpBU2Bzv_U/m/HhNDSU3MksAJ',
    'gave permission', 'of Amstrad. Amstrad kindly ', ' to distribute it');
  C1.AnchorToNeighbour(akTop, 2, Label4);
  C1.AnchorHorizontalCenterTo(Panel7);
  C1.Parent := Panel7;

  Panel7.BevelOuter := bvNone;
  Panel7.Caption := '';
  Panel7.AutoSize := True;

  WasShown := False;

  WC := nil;
  Caption := 'About ' + ApplicationName;
  Label1.Caption := ApplicationName;

  S := UnitVer.TVersion.VersionString;
  if S <> '' then begin
    AddALabel('Version ' + S);
  end;

  S :=
  {$ifdef mswindows}
    'Windows'
  {$else}
    {$i %FPCTargetOS%}
  {$endif}
    + ' ' + {$i %FPCTargetCPU%}
    + Format(' (%d-bit)', [SizeOf(Pointer) * 8])
  ;

  if Assigned(WidgetSet)
  {$ifdef mswindows}
    and (WidgetSet.LCLPlatform <> lpWin32) // let's not display (Win32) on Windows
  {$endif}
  then begin
    WriteStr(S1, WidgetSet.LCLPlatform);

    if Length(S1) > 2 then begin
      if LowerCase(Copy(S1, 1, 2)) = 'lp' then begin
        Delete(S1, 1, 2);
        if LowerCase(S1) = 'qt' then
          S1 := S1 + '4';
      end;

      S := S + ' (' + S1 + ')';
    end;
  end;
  AddALabel(S);

  AddALabel(
    'Build date:', FBuildDate
    );

  //Image1.Picture.Icon := Application.Icon;
  //Image1.AutoSize := True;
  //Image1.AnchorToNeighbour(akRight, 5, WC);
  //Image1.AnchorParallel(akTop, 0, WC);
  Panel8.BevelOuter := bvNone;
  Panel8.Caption := '';
  Panel8.AutoSize := True;
  //Image1.BringToFront;

  Label3.Caption := 'Author: ';
  Label2.Caption := TCommonSpectrum.AuthorName;

  Panel4.BorderStyle := bsNone;
  Panel1.BorderStyle := bsNone;
  //Panel2.BorderStyle := bsNone;
  Panel2.BevelOuter := bvNone;
  Panel2.AutoSize := True;
  Panel3.BorderStyle := bsNone;
  Panel3.Caption := '';
  Panel3.AutoSize := True;
  Panel5.BorderStyle := bsNone;
  Panel5.Caption := '';
  Panel5.AutoSize := True;
  Panel6.BorderStyle := bsNone;
  Panel6.Caption := '';
  Panel6.AutoSize := True;

  Panel1.Constraints.MinWidth := Panel1.Width;
  Panel1.AutoSize := True;

  AddLinks;

  Panel1.BorderSpacing.Around := BorderLineWidth + 1;
  Shape1.Pen.Width := BorderLineWidth;
  Shape1.Pen.Color := $FF3535;
  Panel1.BringToFront;

  B := TFormAboutCloseButton.Create(Panel4);
  B.Anchors := [];
  B.AnchorParallel(akLeft, 0, Panel4);
  B.AnchorParallel(akTop, 0, Panel4);
  B.AnchorParallel(akRight, 0, Panel4);

  Panel4.AutoSize := True;
  B.Parent := Panel4;
  //B.CalculateIconPosition;

  CloseButton := B;
  //B.OnClick := @OnCloseButtonClick;
end;

procedure TFormAbout.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormAbout := nil;
  CloseAction := caFree;
end;

procedure TFormAbout.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  {$ifdef step_by_step_disappear}
  if AlphaBlendValue >= 10 then begin
    CanClose := False;
    StartDisappearing;
  end else
  {$endif}
    CanClose := True;
end;

procedure TFormAbout.FormActivate(Sender: TObject);
begin
  StopDisappearing;
end;

procedure TFormAbout.FormDestroy(Sender: TObject);
begin
  Application.RemoveAsyncCalls(Self);
end;

procedure TFormAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN, VK_ESCAPE, VK_SPACE:
      if CloseButton is TFormAboutCloseButton then
        TFormAboutCloseButton(CloseButton).GoDown;
  end;
end;

procedure TFormAbout.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  B: TFormAboutCloseButton;
begin
  case Key of
    VK_RETURN, VK_ESCAPE, VK_SPACE:
      if CloseButton is TFormAboutCloseButton then begin
        B := TFormAboutCloseButton(CloseButton);
        if B.IsDown then begin
          B.GoUp;
          Close;
        end else begin
          B.Invalidate;
        end;

      end;
  end;
end;

procedure TFormAbout.AfterShow(Data: PtrInt);
var
  H: Integer;
begin
  //TCommonFunctionsLCL.FormToScreenCentre(Self);
  H := WC.Height * 13 div 10;
  Image1.Picture.Icon.Current := Image1.Picture.Icon.GetBestIndexForSize(Size(H, H));
  Image1.BorderSpacing.Right := WC.Height div 5;
  DoAdjustPos;
  AutoSize := False;
  if Data > 0 then begin
    AutoSize := True;
    Application.QueueAsyncCall(@AfterShow, Data - 1);
  end;
end;

procedure TFormAbout.OnCloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAbout.DoAdjustPos;
begin
  CommonFunctionsLCL.TCommonFunctionsLCL.AdjustFormPos(Self);
end;

procedure TFormAbout.OpenHintUrl(Sender: TObject);
var
  Url: String;
begin
  if Sender is TControl then begin
    Url := Trim(TControl(Sender).Hint);
    if Url <> '' then
      OpenURL(Url);
  end;
end;

class procedure TFormAbout.Init;
begin
  FormAbout := nil;
end;

class procedure TFormAbout.Final;
begin
  FormAbout.Free;
end;

class procedure TFormAbout.ShowAbout;
begin
  if FormAbout = nil then begin
    FormAbout := TFormAbout.Create(nil);
    FormAbout.PopupMode := TPopupMode.pmExplicit;
    FormAbout.PopupParent := Application.MainForm;
  end;

  FormAbout.StopDisappearing;
  FormAbout.Show;
end;

initialization
  TFormAbout.Init;

finalization
  TFormAbout.Final;

end.

