unit UnitSpectrumKeyboard;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, UnitKeyMapRecords, LCLType;

type

  TSpectrumKeyBoard = record
  public
    type
      THalfRowIndex = 0..7;
      TKeyIndex = 0..4;
      THalfRows = array [THalfRowIndex] of Byte;

  strict private
    type
      TAKeyMaps = specialize TFPGList<Word>;

      TKeyMapper = class(specialize TFPGMapObject<Word, TAKeyMaps>)
      public
        constructor Create;
        procedure MapKey(const AKey: Word; const HalfRowIndex: THalfRowIndex; const KeyIndex: TKeyIndex);
        procedure LoadFromKeyMapRecs(const AKeyMapRecs: TKeyMapRecs);
        procedure SaveToKeyMapRecs(out AKeyMapRecs: TKeyMapRecs);
      end;

  strict private
    FExtendedKeySupport: Boolean;
    FHalfRows: THalfRows;
    FKeyMapper: TKeyMapper;

    procedure MapDefaultKeys;

    class operator Initialize(var X: TSpectrumKeyBoard);
    class operator Finalize(var X: TSpectrumKeyBoard);
  public
    procedure ClearKeyboard;
    procedure SetKeyState(const HalfRowIndex: THalfRowIndex; const KeyIndex: TKeyIndex;
      const IsDown: Boolean); inline;
    function CheckKeyMap(const AKey: Word): Integer;
    procedure SetKeyState(I: Integer; const IsDown: Boolean);
    function LoadFromKeyMappings: Boolean;

    property HalfRows: THalfRows read FHalfRows;
    property ExtendedKeySupport: Boolean read FExtendedKeySupport; // write FExtendedKeySupport;
  end;

implementation

{ TSpectrumKeyBoard.TKeyMapper }

constructor TSpectrumKeyBoard.TKeyMapper.Create;
begin
  inherited Create(True);

  Sorted := True;
  Duplicates := TDuplicates.dupIgnore;
end;

procedure TSpectrumKeyBoard.TKeyMapper.MapKey(const AKey: Word;
  const HalfRowIndex: THalfRowIndex; const KeyIndex: TKeyIndex);
var
  I: Integer;
  AKM: TAKeyMaps;
  Found: Boolean;
  W: Word;
  WR: WordRec absolute W;
begin
  WR.Hi := HalfRowIndex;
  WR.Lo := KeyIndex;
  Found := False;
  if Find(AKey, I) then begin
    AKM := Data[I];
    Found := AKM.IndexOf(W) >= 0;
  end else begin
    AKM := TAKeyMaps.Create;
    Add(AKey, AKM);
  end;
  if not Found then begin
    AKM.Add(W);
  end;
end;

procedure TSpectrumKeyBoard.TKeyMapper.LoadFromKeyMapRecs(
  const AKeyMapRecs: TKeyMapRecs);
var
  KeyMapRec: TKeyMapRec;
  I: Integer;
  WR: WordRec absolute KeyMapRec.SpectrumKey;

begin
  Clear;
  for I := Low(AKeyMapRecs) to High(AKeyMapRecs) do begin
    KeyMapRec := AKeyMapRecs[I];
    if (WR.Hi <= High(THalfRowIndex)) and (WR.Lo <= High(TKeyIndex)) then begin
      MapKey(KeyMapRec.Key, WR.Hi, WR.Lo);
    end;
  end;
end;

procedure TSpectrumKeyBoard.TKeyMapper.SaveToKeyMapRecs(out
  AKeyMapRecs: TKeyMapRecs);
var
  KeyMapRec: TKeyMapRec;
  I, J, L: Integer;
  AKM: TAKeyMaps;

begin
  L := 0;

  SetLength(AKeyMapRecs{%H-}, 50);
  for I := 0 to Count - 1 do begin
    KeyMapRec.Key := Keys[I];
    AKM := Data[I];
    if Assigned(AKM) and (AKM.Count > 0) then begin
      if Length(AKeyMapRecs) < L + AKM.Count then
        SetLength(AKeyMapRecs, ((L + AKM.Count) * 3) div 2 + 3);

      for J := 0 to AKM.Count - 1 do begin
        KeyMapRec.SpectrumKey := AKM.Items[J];
        AKeyMapRecs[L] := KeyMapRec;
        Inc(L);
      end;
    end;

  end;
  SetLength(AKeyMapRecs, L);
end;

{ TSpectrumKeyBoard }

function TSpectrumKeyBoard.LoadFromKeyMappings: Boolean;
var
  KMRecs: TKeyMapRecs;
begin
  Result := False;

  ClearKeyboard;
  TKeyMappings.KMappings.GetKeyMapRecs(TKeyMappings.KMappings.ActiveKeyMapName, KMRecs);

  if Length(KMRecs) > 0 then begin
    FKeyMapper.LoadFromKeyMapRecs(KMRecs);
    Result := True;
  end else begin
    MapDefaultKeys;
  end;
end;

procedure TSpectrumKeyBoard.MapDefaultKeys;

  procedure SetOneKeyPerSpectrumKey(
    const HalfRowIndex: THalfRowIndex; const WA: array of Word);
  var
    I, J: Integer;
  begin
    if Length(WA) = 5 then begin
      for I := 0 to High(WA) do begin
        if FKeyMapper.Find(WA[I], J) then
          FKeyMapper.Data[J].Clear;
        FKeyMapper.MapKey(WA[I], HalfRowIndex, I);
      end;
    end;
  end;

  procedure AddPCKeysToSpectrumKey(
    const HalfRowIndex: THalfRowIndex; const Index: TKeyIndex;
    PCKeys: array of Word);
  var
    I: Integer;
  begin
    for I := Low(PCKeys) to High(PCKeys) do begin
      FKeyMapper.MapKey(PCKeys[I], HalfRowIndex, Index);
    end;
  end;

const
  KeysCsZXCV = THalfRowIndex(0);
  KeysASDFG = THalfRowIndex(1);
  KeysQWERT = THalfRowIndex(2);
  Keys12345 = THalfRowIndex(3);
  Keys67890 = THalfRowIndex(4);
  KeysYUIOP = THalfRowIndex(5);
  KeysHJKLEn = THalfRowIndex(6);
  KeysBNMSySp = THalfRowIndex(7);

var
  KMRecs: TKeyMapRecs;

begin
  if Length(TKeyMappings.DefaultMapRecs) > 0 then begin
    FKeyMapper.LoadFromKeyMapRecs(TKeyMappings.DefaultMapRecs);
  end else begin
  // this needs to be done only once...
    FKeyMapper.Clear;

    if FExtendedKeySupport then begin
      SetOneKeyPerSpectrumKey(KeysCsZXCV, [VK_LSHIFT, VK_Z, VK_X, VK_C, VK_V]);
      AddPCKeysToSpectrumKey(KeysCsZXCV, 0, [VK_LCONTROL]);
    end else begin
      SetOneKeyPerSpectrumKey(KeysCsZXCV, [VK_SHIFT, VK_Z, VK_X, VK_C, VK_V]);
    end;
    SetOneKeyPerSpectrumKey(KeysASDFG, [VK_A, VK_S, VK_D, VK_F, VK_G]);
    SetOneKeyPerSpectrumKey(KeysQWERT, [VK_Q, VK_W, VK_E, VK_R, VK_T]);
    SetOneKeyPerSpectrumKey(Keys12345, [VK_1, VK_2, VK_3, VK_4, VK_5]);
    //
    SetOneKeyPerSpectrumKey(Keys67890, [VK_0, VK_9, VK_8, VK_7, VK_6]);
    SetOneKeyPerSpectrumKey(KeysYUIOP, [VK_P, VK_O, VK_I, VK_U, VK_Y]);
    SetOneKeyPerSpectrumKey(KeysHJKLEn, [VK_RETURN, VK_L, VK_K, VK_J, VK_H]);
    //
    if FExtendedKeySupport then begin
      SetOneKeyPerSpectrumKey(KeysBNMSySp, [VK_SPACE, VK_RCONTROL, VK_M, VK_N, VK_B]);
      AddPCKeysToSpectrumKey(KeysBNMSySp, 1, [VK_RSHIFT]);
    end else begin
      SetOneKeyPerSpectrumKey(KeysBNMSySp, [VK_SPACE, VK_CONTROL, VK_M, VK_N, VK_B]);
    end;
    //
    AddPCKeysToSpectrumKey(Keys12345, 0, [VK_NUMPAD1]);
    AddPCKeysToSpectrumKey(Keys12345, 1, [VK_NUMPAD2]);
    AddPCKeysToSpectrumKey(Keys12345, 2, [VK_NUMPAD3]);
    AddPCKeysToSpectrumKey(Keys12345, 3, [VK_NUMPAD4]);
    AddPCKeysToSpectrumKey(Keys12345, 4, [VK_NUMPAD5]);
    //
    AddPCKeysToSpectrumKey(Keys67890, 0, [VK_NUMPAD0]);
    AddPCKeysToSpectrumKey(Keys67890, 1, [VK_NUMPAD9]);
    AddPCKeysToSpectrumKey(Keys67890, 2, [VK_NUMPAD8]);
    AddPCKeysToSpectrumKey(Keys67890, 3, [VK_NUMPAD7]);
    AddPCKeysToSpectrumKey(Keys67890, 4, [VK_NUMPAD6]);
    //
    //{ Key combinations -- +-*/,.backspace, cursors }
    AddPCKeysToSpectrumKey(KeysHJKLEn, 2, [VK_ADD, VK_OEM_PLUS]);
    AddPCKeysToSpectrumKey(KeysBNMSySp, 1, [VK_ADD, VK_OEM_PLUS]);
    //
    AddPCKeysToSpectrumKey(KeysHJKLEn, 3, [VK_SUBTRACT, VK_OEM_MINUS]);
    AddPCKeysToSpectrumKey(KeysBNMSySp, 1, [VK_SUBTRACT, VK_OEM_MINUS]);
    //
    AddPCKeysToSpectrumKey(KeysBNMSySp, 4, [VK_MULTIPLY]);
    AddPCKeysToSpectrumKey(KeysBNMSySp, 1, [VK_MULTIPLY]);
    //
    AddPCKeysToSpectrumKey(KeysCsZXCV, 4, [VK_DIVIDE]);
    AddPCKeysToSpectrumKey(KeysBNMSySp, 1, [VK_DIVIDE]);
    //
    AddPCKeysToSpectrumKey(Keys12345, 1, [VK_CAPITAL]);
    AddPCKeysToSpectrumKey(KeysCsZXCV, 0, [VK_CAPITAL]);
    //
    AddPCKeysToSpectrumKey(Keys12345, 4, [VK_LEFT]);
    AddPCKeysToSpectrumKey(KeysCsZXCV, 0, [VK_LEFT]);
    //
    AddPCKeysToSpectrumKey(Keys67890, 3, [VK_UP]);
    AddPCKeysToSpectrumKey(KeysCsZXCV, 0, [VK_UP]);
    //
    AddPCKeysToSpectrumKey(Keys67890, 4, [VK_DOWN]);
    AddPCKeysToSpectrumKey(KeysCsZXCV, 0, [VK_DOWN]);
    //
    AddPCKeysToSpectrumKey(Keys67890, 2, [VK_RIGHT]);
    AddPCKeysToSpectrumKey(KeysCsZXCV, 0, [VK_RIGHT]);
    //
    AddPCKeysToSpectrumKey(Keys67890, 0, [VK_BACK]);
    AddPCKeysToSpectrumKey(KeysCsZXCV, 0, [VK_BACK]);
    //
    AddPCKeysToSpectrumKey(KeysBNMSySp, 2, [VK_OEM_PERIOD, VK_DECIMAL]);
    AddPCKeysToSpectrumKey(KeysBNMSySp, 1, [VK_OEM_PERIOD, VK_DECIMAL]);

    AddPCKeysToSpectrumKey(KeysBNMSySp, 3, [VK_OEM_COMMA]);
    AddPCKeysToSpectrumKey(KeysBNMSySp, 1, [VK_OEM_COMMA]);

    FKeyMapper.SaveToKeyMapRecs(KMRecs);
    TKeyMappings.DefaultMapRecs := KMRecs;
  end;
end;

class operator TSpectrumKeyBoard.Initialize(var X: TSpectrumKeyBoard);
begin
  X.ClearKeyboard;
  X.FKeyMapper := TKeyMapper.Create;
  X.FExtendedKeySupport := False;

  X.MapDefaultKeys;
  X.LoadFromKeyMappings;
end;

class operator TSpectrumKeyBoard.Finalize(var X: TSpectrumKeyBoard);
begin
  X.FKeyMapper.Free;
end;

procedure TSpectrumKeyBoard.ClearKeyboard;
var
  I: Integer;
begin
  for I := Low(FHalfRows) to High(FHalfRows) do
    FHalfRows[I] := %00011111;
end;

procedure TSpectrumKeyBoard.SetKeyState(const HalfRowIndex: THalfRowIndex;
  const KeyIndex: TKeyIndex; const IsDown: Boolean);
var
  B: Byte;
begin
  B := 1 shl KeyIndex;
  if IsDown then
    FHalfRows[HalfRowIndex] := FHalfRows[HalfRowIndex] and (not B)
  else
    FHalfRows[HalfRowIndex] := FHalfRows[HalfRowIndex] or B;
end;

function TSpectrumKeyBoard.CheckKeyMap(const AKey: Word): Integer;
begin
  if not FKeyMapper.Find(AKey, Result) then
    Result := -1;
end;

procedure TSpectrumKeyBoard.SetKeyState(I: Integer; const IsDown: Boolean);
var
  AKM: TAKeyMaps;
  W: Word;
  WR: WordRec absolute W;
begin
  AKM := FKeyMapper.Data[I];
  for I := 0 to AKM.Count - 1 do begin
    W := AKM.Items[I];
    SetKeyState(WR.Hi, WR.Lo, IsDown);
  end;
end;

end.

