unit UnitJoystick;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils, UnitConfigs, UnitSpectrumKeyboard, LCLType, LazUTF8,
  fpjson;

type
  TJoystick = class(TObject)
  public
    type
      TJoystickType = (jtKempston, jtFuller, jtInterface_II_left,
        jtInterface_II_right, jtCursor);
      TJoystickDirection = (diUp, diDown, diLeft, diRight, diFire);
      TJoystickDirectionsKeys = Array [TJoystickDirection] of Word;

  strict private
    type
      TSpectrumKeyMap = array [TJoystickDirection] of Word;
      TSpectrumKeyMaps = array [TJoystickType.jtInterface_II_left..TJoystickType.jtCursor] of TSpectrumKeyMap;
      TJoystickMap = array [TJoystickDirection] of Byte;
      TJoystickMaps = array [TJoystickType.jtKempston..TJoystickType.jtFuller] of TJoystickMap;
      TJoystickDirectionPressed = array [TJoystickDirection] of Boolean;

    const
      cSection0 = 'joysticks';
      cJoystickType = 'joystick_type';
      cEnabled = 'enabled';

      // Kempston: port 0x1F, 000FUDLR, active bits high
      // Fuller: port 0x7F, F---RLDU, active bits low
      JoystickDirectionMaps: TJoystickMaps = (
        //    up       down       left      right      fire
        (%00001000, %00000100, %00000010, %00000001, %00010000), // kempston
        (%00000001, %00000010, %00000100, %00001000, %10000000)  // fuller
      );

      // For joysticks that map to keyboard:
      // high byte maps to row index, low byte maps to key index in row.
      SpectrumKeyMaps: TSpectrumKeyMaps = (
        // up    down   left  right   fire
        ($0303, $0302, $0300, $0301, $0304), //4,3,1,2,5 interface II, left port
        ($0401, $0402, $0404, $0403, $0400), //9,8,6,7,0 interface II, right port
        ($0403, $0404, $0304, $0402, $0400)  //7,6,5,8,0 cursor joystick
      );

  strict private
    FSpectrumKeyMap: TSpectrumKeyMap;
    FJoystickDirectionMap: TJoystickMap;
    FState: Byte;
    FJoystickType: TJoystickType;
    FKeys: TJoystickDirectionsKeys;
    FEnabled: Boolean;
    FJoystickDirectionPressed: TJoystickDirectionPressed;

    procedure SetEnabled(AValue: Boolean);
    function GetCurrentJoystickTypeAsString: String;
    function GetKeyDown: Word;
    function GetKeyFire: Word;
    function GetKeyLeft: Word;
    function GetKeyRight: Word;
    function GetKeyUp: Word;
    procedure LoadFormConf;
    procedure SaveToConf;
    procedure SetJoystickType(AValue: TJoystickType);
    procedure InitDefaultKeys;
  private
    class procedure Init;
    class procedure Final;
  strict private
    class var
      FJoystick: TJoystick;
      DirectionSections: Array [TJoystickDirection] of String;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetState(const I: Integer; const IsDown: Boolean; const AKeyBoard: TSpectrumKeyBoard);
    function CheckKey(const Key: Word): Integer;
    procedure ResetState;
    class function JoystickTypeToString(AJoystickType: TJoystickType): String;
    class function DirectionToString(ADirection: TJoystickDirection): String;
    function SetKeys(const AKeys: TJoystickDirectionsKeys): Boolean;
    procedure GetKeys(out AKeys: TJoystickDirectionsKeys);

    property State: Byte read FState;
    property KeyUp: Word read GetKeyUp;
    property KeyDown: Word read GetKeyDown;
    property KeyLeft: Word read GetKeyLeft;
    property KeyRight: Word read GetKeyRight;
    property KeyFire: Word read GetKeyFire;

    property CurrentJoystickTypeAsString: String read GetCurrentJoystickTypeAsString;
    property JoystickType: TJoystickType read FJoystickType write SetJoystickType;
    property Enabled: Boolean read FEnabled write SetEnabled;

    class property Joystick: TJoystick read FJoystick;
  end;

implementation

{ TJoystick }

class procedure TJoystick.Init;
var
  JD: TJoystickDirection;
begin
  for JD := Low(TJoystickDirection) to High(TJoystickDirection) do
    DirectionSections[JD] := 'key_' + LowerCase(DirectionToString(JD));

  FJoystick := TJoystick.Create;
end;

class procedure TJoystick.Final;
begin
  FJoystick.Free;
end;

procedure TJoystick.SetEnabled(AValue: Boolean);
var
  I: TJoystickDirection;
begin
  ResetState;

  if AValue then
    for I := Low(TJoystickDirection) to High(TJoystickDirection) do
      if FKeys[I] = 0 then begin
        AValue := False;
        Break;
      end;

  FEnabled := AValue;
end;

function TJoystick.GetKeyDown: Word;
begin
  Result := FKeys[TJoystickDirection.diDown];
end;

function TJoystick.GetKeyFire: Word;
begin
  Result := FKeys[TJoystickDirection.diFire];
end;

function TJoystick.GetKeyLeft: Word;
begin
  Result := FKeys[TJoystickDirection.diLeft];
end;

function TJoystick.GetKeyRight: Word;
begin
  Result := FKeys[TJoystickDirection.diRight];
end;

function TJoystick.GetKeyUp: Word;
begin
  Result := FKeys[TJoystickDirection.diUp];
end;

function TJoystick.GetCurrentJoystickTypeAsString: String;
begin
  Result := JoystickTypeToString(Self.JoystickType);
end;

procedure TJoystick.LoadFormConf;

var
  JObj: TJSONObject;

  function GetWordAsPCKey(const ASection: String): Word;
  var
    N: Integer;
  begin
    Result := 0;
    N := -1;
    N := JObj.Get(ASection, N);
    if (N > 0) and (N <= Word.MaxValue) then begin
      Result := N;
    end;
  end;

var
  S: RawByteString;
  JT: TJoystickType;
  I: TJoystickType;
  JD: TJoystickDirection;
  N: Integer;
  AuxKeys: TJoystickDirectionsKeys;
  W: Word;

begin
  JObj := TConfJSON.GetJSONObject(cSection0);

  if Assigned(JObj) then begin
    for JD := Low(DirectionSections) to High(DirectionSections) do begin
      W := GetWordAsPCKey(DirectionSections[JD]);
      if W = 0 then begin
        Break;
      end;
      AuxKeys[JD] := W;
    end;
    if W > 0 then
      SetKeys(AuxKeys);

    N := 0;

    S := '';
    S := JObj.Get(cJoystickType, S);
    S := UTF8Trim(S);

    JT := Low(TJoystickType);
    if S <> '' then
      for I := Low(TJoystickType) to High(TJoystickType) do begin
        if AnsiSameText(S, JoystickTypeToString(I)) then begin
          JT := I;
          N := JObj.Get(cEnabled, N);
          Break;
        end;
      end;

    SetJoystickType(JT);
    SetEnabled(N > 0);
  end;
end;

procedure TJoystick.SaveToConf;
var
  JObj: TJSONObject;
  N: Integer;
  JD: TJoystickDirection;

begin
  JObj := TJSONObject.Create;
  try
    for JD := Low(DirectionSections) to High(DirectionSections) do begin
      N := FKeys[JD];
      JObj.Add(DirectionSections[JD], N);
    end;

    JObj.Add(cJoystickType, JoystickTypeToString(JoystickType));
    if Enabled then
      N := 1
    else
      N := 0;
    JObj.Add(cEnabled, N);

    TConfJSON.RemoveSection(cSection0);
    if TConfJSON.AddToConf(cSection0, JObj) then
      JObj := nil;

  finally
    JObj.Free;
  end;
end;

procedure TJoystick.SetJoystickType(AValue: TJoystickType);
begin
  FJoystickType := AValue;
  ResetState;

  case FJoystickType of
    Low(TSpectrumKeyMaps)..High(TSpectrumKeyMaps):
      begin
        FSpectrumKeyMap := SpectrumKeyMaps[AValue];
      end;
    Low(TJoystickMaps)..High(TJoystickMaps):
      begin
        FJoystickDirectionMap := JoystickDirectionMaps[AValue];
      end;
  otherwise
    FSpectrumKeyMap := Default(TSpectrumKeyMap);
  end;
end;

procedure TJoystick.InitDefaultKeys;
const
  CDefKeys: TJoystickDirectionsKeys = (VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_TAB);
begin
  FKeys := CDefKeys;
end;

function TJoystick.SetKeys(const AKeys: TJoystickDirectionsKeys): Boolean;
var
  I, J: TJoystickDirection;
begin
  for I := Succ(Low(TJoystickDirection)) to High(TJoystickDirection) do
    if AKeys[I] <> 0 then
      for J := Low(TJoystickDirection) to Pred(I) do
        if AKeys[J] = AKeys[I] then
          Exit(False);

  FKeys := AKeys;

  Result := True;
end;

procedure TJoystick.GetKeys(out AKeys: TJoystickDirectionsKeys);
begin
  AKeys := FKeys;
end;

constructor TJoystick.Create;
begin
  inherited Create;

  FEnabled := False;
  SetJoystickType(Low(TJoystickType));

  InitDefaultKeys;

  LoadFormConf;
end;

destructor TJoystick.Destroy;
begin
  SaveToConf;
  inherited Destroy;
end;

procedure TJoystick.SetState(const I: Integer; const IsDown: Boolean;
  const AKeyBoard: TSpectrumKeyBoard);
var
  B: Byte;
  W: Word;
  JD: TJoystickDirection;
  WR: WordRec absolute W;
begin
  JD := TJoystickDirection(I);
  if IsDown xor FJoystickDirectionPressed[JD] then begin
    case FJoystickType of
      TJoystickType.jtKempston, TJoystickType.jtFuller:
        begin
          B := FJoystickDirectionMap[JD];

          if IsDown xor (FJoystickType = TJoystickType.jtFuller) then
            FState := FState or B
          else
            FState := FState and (not B);

        end;

    otherwise
      // other joystick types map to keyboard.
      W := FSpectrumKeyMap[JD];
      AKeyBoard.SetKeyState(TSpectrumKeyBoard.THalfRowIndex(WR.Hi), TSpectrumKeyBoard.TKeyIndex(WR.Lo), IsDown);
    end;

    FJoystickDirectionPressed[JD] := IsDown;
  end;

end;

function TJoystick.CheckKey(const Key: Word): Integer;
var
  JD: TJoystickDirection;
begin
  if FEnabled then begin
    for JD := Low(TJoystickDirection) to High(TJoystickDirection) do begin
      if Key = FKeys[JD] then begin
        Exit(Ord(JD));
      end;

    end;
  end;

  Result := -1;
end;

procedure TJoystick.ResetState;
begin
  if FJoystickType <> TJoystickType.jtFuller then
    FState := 0
  else
    FState := $FF;
  FJoystickDirectionPressed := Default(TJoystickDirectionPressed);
end;

class function TJoystick.JoystickTypeToString(AJoystickType: TJoystickType
  ): String;
var
  I: Integer;
begin
  WriteStr(Result, AJoystickType);

  for I := 2 to Length(Result) do begin
    if LowerCase(Result[I]) <> Result[I] then begin
      Delete(Result, 1, I - 1);
      Result := StringReplace(Result, '_', ' ', [rfReplaceAll]);
      Break;
    end;
  end;
end;

class function TJoystick.DirectionToString(ADirection: TJoystickDirection
  ): String;
var
  I: Integer;
begin
  WriteStr(Result, ADirection);

  for I := 2 to Length(Result) do begin
    if LowerCase(Result[I]) <> Result[I] then begin
      Delete(Result, 1, I - 1);
      Break;
    end;
  end;
end;

initialization
  TJoystick.Init;

finalization
  TJoystick.Final;

end.

