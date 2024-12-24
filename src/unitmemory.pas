unit UnitMemory;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitCommon;

type

  { TMemory }

  TMemory = class(TObject)
  strict private
    const
      KiloByte = 1024;
      BankSize = 16 * KiloByte;
  strict private
    FRamSize: Int32;
    MemStart: PByte;
    FRamBanks: Array[0..7] of PByte;
    FRomBanks: Array[0..3] of PByte;
    FActiveRamPageNo: Byte;
    FActiveRomPageNo: Byte;
    FActiveScreenPageNo: Byte;
    ActiveRamBank: PByte;
    ActiveRomBank: PByte;
    ActiveScreenBank: PByte;
    Bank5: PByte;
    Bank2: PByte;

    function GetCurrentlyMappedMemSizeKB: Word;
    procedure SetActiveRamPageNo(B: Byte);
    procedure SetActiveRomPageNo(B: Byte);
    procedure SetShadowScreenDisplay(B: Boolean);
    function GetShadowScreenDisplay: Boolean;
    function GetRamSizeKB: Word;
    procedure FreeBanks;

  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromStream(const BankNum: Byte; IsRom: Boolean; const AStream: TStream): Boolean;
    function SaveToStream(const BankNum: Byte; IsRom: Boolean; const AStream: TStream): Boolean;

    function LoadFromStream(const AStream: TStream; const AddrFrom: Word): Boolean;

    // Ram to one stream - pages 5, 2, 0, 1, 3, 4, 6, 7 respectively:
    function LoadRamFromStream(const AStream: TStream): Boolean;
    function SaveRamToStream(const AStream: TStream): Boolean;

    // Rom to one stream:
    function SaveRomToStream(const AStream: TStream): Boolean;

    function ReadByte(const Address: Word): Byte; inline;
    function ReadScreenByte(const Address: Word): Byte; inline;
    procedure WriteByte(const Address: Word; const B: Byte); inline;

    procedure ClearRam();
    procedure RandomizeRam();

    procedure InitBanks(const ARamSizeKB: Word; ARomBanksCount: Integer = 0);
    procedure InitBanks();

    property ActiveRamPageNo: Byte read FActiveRamPageNo write SetActiveRamPageNo;
    property ActiveRomPageNo: Byte read FActiveRomPageNo write SetActiveRomPageNo;
    property RamSizeKB: Word read GetRamSizeKB;
    property CurrentlyMappedMemSizeKB: Word read GetCurrentlyMappedMemSizeKB;
    property ShadowScreenDisplay: Boolean read GetShadowScreenDisplay write SetShadowScreenDisplay;
  end;

implementation

{ TMemory }

function TMemory.GetRamSizeKB: Word;
begin
  Result := FRamSize shr 10;
end;

procedure TMemory.SetActiveRomPageNo(B: Byte);
begin
  FActiveRomPageNo := B;
  ActiveRomBank := FRomBanks[B];
end;

procedure TMemory.SetShadowScreenDisplay(B: Boolean);
begin
  if B then
    FActiveScreenPageNo := 7
  else
    FActiveScreenPageNo := 5;
  ActiveScreenBank := FRamBanks[FActiveScreenPageNo];
end;

function TMemory.GetShadowScreenDisplay: Boolean;
begin
  Result := FActiveScreenPageNo = 7;
end;

constructor TMemory.Create;
begin
  inherited Create;

  MemStart := nil;
  FreeBanks;
end;

destructor TMemory.Destroy;
begin
  FreeBanks;

  inherited Destroy;
end;

function TMemory.LoadFromStream(const BankNum: Byte; IsRom: Boolean;
  const AStream: TStream): Boolean;
var
  P: PByte;
begin
  if Assigned(AStream) and (AStream.Size - AStream.Position >= BankSize) then begin
    P := nil;
    if IsRom then begin
      if BankNum < Length(FRomBanks) then
        P := FRomBanks[BankNum];
    end else begin
      if BankNum < Length(FRamBanks) then
        P := FRamBanks[BankNum];
    end;

    Result := Assigned(P) and (AStream.Read(P^, BankSize) = BankSize);

  end else
    Result := False;
end;

function TMemory.SaveToStream(const BankNum: Byte; IsRom: Boolean; const AStream: TStream
  ): Boolean;
var
  P: PByte;
begin
  Result := False;

  if not Assigned(AStream) then
    Exit;

  P := nil;
  if IsRom then begin
    if (BankNum < Length(FRomBanks)) then
      P := FRomBanks[BankNum];
  end else begin
    if BankNum < Length(FRamBanks) then
      P := FRamBanks[BankNum];
  end;

  if Assigned(P) then
    try
      Result := AStream.Write(P^, BankSize) = BankSize;
    except
      Result := False;
    end;
end;

function TMemory.LoadFromStream(const AStream: TStream; const AddrFrom: Word
  ): Boolean;
var
  L, N: Integer;
  P: PByte;
  Address: Word;
begin
  Result := False;

  L := AStream.Size - AStream.Position;

  if L + AddrFrom > Integer(GetCurrentlyMappedMemSizeKB) * KiloByte then
    Exit;

  P := nil;
  case AddrFrom shr 14 of
    0:
      begin
        P := ActiveRomBank;
        N := ActiveRomPageNo;
      end;
    1:
      N := 5;
    2:
      N := 2;
  otherwise
    N := ActiveRamPageNo;
  end;

  if P = nil then
    P := FRamBanks[N];

  if Assigned(P) then begin
    Address := AddrFrom and $3FFF;
    N := BankSize - Integer(Address);
    if N > L then
      N := L;
    if AStream.Read((P + Address)^, N) = N then begin
      if L = N then
        Result := True
      else if L > N then begin
        Address := AddrFrom + N;
        Result := LoadFromStream(AStream, Address);
      end;
    end;
  end;
end;

function TMemory.LoadRamFromStream(const AStream: TStream): Boolean;
begin
  Result := LoadFromStream(5, False, AStream);
  if Result and (GetRamSizeKB >= 48) then begin
    Result := LoadFromStream(2, False, AStream)
      and LoadFromStream(0, False, AStream);
    if Result and (GetRamSizeKB >= 128) then begin
      Result := LoadFromStream(1, False, AStream)
        and LoadFromStream(3, False, AStream)
        and LoadFromStream(4, False, AStream)
        and LoadFromStream(6, False, AStream)
        and LoadFromStream(7, False, AStream);
    end;
  end;
end;

function TMemory.SaveRamToStream(const AStream: TStream): Boolean;
begin
  Result := SaveToStream(5, False, AStream);
  if Result and (GetRamSizeKB >= 48) then begin
    Result := SaveToStream(2, False, AStream)
      and SaveToStream(0, False, AStream);
    if Result and (GetRamSizeKB >= 128) then begin
      Result := SaveToStream(1, False, AStream)
        and SaveToStream(3, False, AStream)
        and SaveToStream(4, False, AStream)
        and SaveToStream(6, False, AStream)
        and SaveToStream(7, False, AStream);
    end;
  end;
end;

function TMemory.SaveRomToStream(const AStream: TStream): Boolean;
var
  I: Integer;
begin
  Result := False;
  I := 0;
  while (I < Length(FRomBanks)) and Assigned(FRomBanks[I]) do begin
    Result := SaveToStream(I, True, AStream);
    if not Result then
      Break;

    Inc(I);
  end;
end;

function TMemory.ReadByte(const Address: Word): Byte;
var
  P: PByte;
begin
  case Address shr 14 of
    0:
      P := ActiveRomBank;
    1:
      P := Bank5;
    2:
      P := Bank2;
  otherwise
    P := ActiveRamBank;
  end;

  if Assigned(P) then
    Result := (P + (Address and $3FFF))^
  else
    Result := $FF;
end;

function TMemory.ReadScreenByte(const Address: Word): Byte;
begin
  Result := (ActiveScreenBank + Address)^;
end;

procedure TMemory.WriteByte(const Address: Word; const B: Byte);
var
  P: PByte;
begin
  case Address shr 14 of
    0:
      // rom bank, do nothing.
      Exit;
    1:
      P := Bank5;
    2:
      P := Bank2;
  otherwise // 3
    P := ActiveRamBank;
  end;

  if Assigned(P) then
    (P + (Address and $3FFF))^ := B;
end;

procedure TMemory.ClearRam();

  procedure ClearRamBank(const P: PByte);
  begin
    if Assigned(P) then
      FillChar(P^, BankSize, 0);
  end;

var
  I: Integer;
begin
  for I := Low(FRamBanks) to High(FRamBanks) do
    ClearRamBank(FRamBanks[I]);
end;

procedure TMemory.RandomizeRam();

  procedure RandomizeRamBank(P: PByte);
  const
    TopByte = Int32(Byte.MaxValue) + 1;
  var
    P1: PByte;
  begin
    if Assigned(P) then begin
      P1 := P + BankSize;
      while P < P1 do begin
        P^ := Byte(Random(TopByte));
        Inc(P);
      end;
    end;
  end;

var
  I: Integer;
begin
  UnitCommon.TCommonFunctions.CallRandomizeIfNotCalledAlready();

  for I := Low(FRamBanks) to High(FRamBanks) do
    RandomizeRamBank(FRamBanks[I]);
end;

procedure TMemory.SetActiveRamPageNo(B: Byte);
begin
  FActiveRamPageNo := B;
  ActiveRamBank := FRamBanks[B];
end;

function TMemory.GetCurrentlyMappedMemSizeKB: Word;
begin
  Result := GetRamSizeKB;
  if Result > 48 then
    Result := 48;
  Result := Result + 16;
end;

procedure TMemory.InitBanks(const ARamSizeKB: Word; ARomBanksCount: Integer);

var
  I: Integer;
  RamBanksCount: Integer;
  P: PByte;

begin
  case ARamSizeKB of
    16, 48:
      ARomBanksCount := 1;

    128:
      if ARomBanksCount <> 4 then
        ARomBanksCount := 2;

  otherwise
    FreeBanks;
    Exit;
  end;

  RamBanksCount := ARamSizeKB div 16;
  FRamSize := Int32(ARamSizeKB) * KiloByte;
  ReAllocMem(MemStart, FRamSize + ARomBanksCount * BankSize);

  P := MemStart;
  for I := Low(FRomBanks) to High(FRomBanks) do begin
    if I >= ARomBanksCount then
      FRomBanks[I] := nil
    else begin
      FRomBanks[I] := P;
      P := P + BankSize;
    end;
  end;

  Bank5 := P;
  P := P + BankSize;

  if RamBanksCount >= 3 then begin
    Bank2 := P;
    FRamBanks[0] := Bank2 + BankSize;
    P := P + 2 * BankSize;
  end else begin
    Bank2 := nil;
    FRamBanks[0] := nil;
  end;

  FRamBanks[2] := Bank2;
  FRamBanks[5] := Bank5;

  for I := Low(FRamBanks) to High(FRamBanks) do begin
    if I in [1, 3, 4, 6, 7] then begin
      if ARomBanksCount > 1 then begin // 128K
        FRamBanks[I] := P;
        P := P + BankSize;
      end else
        FRamBanks[I] := nil;
    end;
  end;

  SetActiveRomPageNo(0);
  SetActiveRamPageNo(0);
  SetShadowScreenDisplay(False);
end;

procedure TMemory.InitBanks();
var
  N: Integer;
begin
  if FRomBanks[3] <> nil then
    N := 4
  else
    N := 0;
  InitBanks(GetRamSizeKB(), N);
end;

procedure TMemory.FreeBanks;
var
  I: Integer;
begin
  FRamSize := 0;
  FActiveRamPageNo := 0;
  FActiveRomPageNo := 0;
  FActiveScreenPageNo := 0;
  Bank2 := nil;
  Bank5 := nil;
  ActiveRomBank := nil;
  ActiveRamBank := nil;
  ActiveScreenBank := nil;
  for I := Low(FRomBanks) to High(FRomBanks) do
    FRomBanks[I] := nil;
  for I := Low(FRamBanks) to High(FRamBanks) do
    FRamBanks[I] := nil;

  if MemStart <> nil then
    FreeMemAndNil(MemStart);
end;

end.

