unit UnitMemory;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}

{-$define StaticMem}

interface

uses
  Classes, SysUtils;

type

  { TMemory }

  TMemory = record
  strict private
    const
      KiloByte = 1024;
      MinimalRomSize = KiloByte;
  strict private
    FAllowWriteToRom: Boolean;
    FMemSize: Int32;
    FRomSize: Int32;
    FRamSize: Int32;
    P: PByte;
    {$ifdef StaticMem}
    MemArr: array [0..(64 * KiloByte - 1)] of Byte;
    {$endif}

    procedure Empty;
    procedure SetSizes(const ARomSize, ARamSize: Int32);
    function LoadBlockFromStream(const Stream: TStream; const PositionInMemory, Len: Integer): Boolean;

    class operator Initialize(var X: TMemory);
    class operator Finalize(var X: TMemory);
  public
    procedure SetSizesInKB(const ARomSizeKB, ARamSizeKB: Word);
    function ReadByte(const Address: Word): Byte; inline;
    procedure WriteByte(const Address: Word; const B: Byte); inline;

    function LoadRomFromStream(const Stream: TStream): Boolean;
    function LoadRamFromStream(const Stream: TStream): Boolean;
    function LoadRamBlockFromStream(const Stream: TStream; const Position, Len: Integer): Boolean;
    function SaveRamToStream(const Stream: TStream): Boolean;
    procedure ClearRam;
    procedure RandomizeRam;

    property MemSize: Int32 read FMemSize;
    property RomSize: Int32 read FRomSize;
    property RamSize: Int32 read FRamSize;
    property AllowWriteToRom: Boolean read FAllowWriteToRom write FAllowWriteToRom;
  end;

  PMemory = ^TMemory;

implementation

{ TMemory }

procedure TMemory.Empty;
begin
  {$ifNdef StaticMem}
  if P <> nil then
    FreeMemAndNil(P);
  {$endif}

  FMemSize := 0;
  FRamSize := 0;
  FRomSize := 0;
end;

procedure TMemory.SetSizes(const ARomSize, ARamSize: Int32);
var
  NewMemSize: Int32;
begin
  if (ARomSize < MinimalRomSize) or (ARamSize = 0) then
    Empty
  else
    if (ARomSize <> FRomSize) or (ARamSize <> FRamSize) then begin
      NewMemSize := ARomSize + ARamSize;
      {$ifdef StaticMem}
      if NewMemSize > SizeOf(MemArr) then begin
        Empty;
        Exit;
      end;
      {$endif}
      FRomSize := ARomSize;
      FRamSize := ARamSize;

      if NewMemSize <> FMemSize then begin
        FMemSize := NewMemSize;
        {$ifNdef StaticMem}
        ReAllocMem(P, NewMemSize);
        {$endif}
      end;
    end;

end;

function TMemory.LoadBlockFromStream(const Stream: TStream; const PositionInMemory,
  Len: Integer): Boolean;
begin
  Result := Assigned(Stream)
    and (PositionInMemory + Len <= FMemSize)
    and (Stream.Position + Len <= Stream.Size)
    and (Stream.Read((P + PositionInMemory)^, Len) = Len);
end;

class operator TMemory.Initialize(var X: TMemory);
begin
  X.FAllowWriteToRom := False;
  {$ifdef StaticMem}
  X.P := @(X.MemArr[0]);
  {$else}
  X.P := nil;
  {$endif}

  X.Empty;
end;

class operator TMemory.Finalize(var X: TMemory);
begin
  {$ifNdef StaticMem}
  if X.P <> nil then
    FreeMem(X.P);
  {$endif}
end;

procedure TMemory.SetSizesInKB(const ARomSizeKB, ARamSizeKB: Word);
begin
  SetSizes(ARomSizeKB * KiloByte, ARamSizeKB * KiloByte);
end;

function TMemory.ReadByte(const Address: Word): Byte;
begin
  if Address < FMemSize then begin
    {$ifdef StaticMem}
    Result := MemArr[Address];
    {$else}
    Result := (P + Address)^;
    {$endif}
  end else
    //Result := 0;
    Result := $FF
    ; // ?
end;

procedure TMemory.WriteByte(const Address: Word; const B: Byte);
begin
  if (Address < FMemSize) and ((Address >= FRomSize) or FAllowWriteToRom) then begin
    {$ifdef StaticMem}
    MemArr[Address] := B;
    {$else}
    (P + Address)^ := B;
    {$endif}
  end;
end;

function TMemory.LoadRomFromStream(const Stream: TStream): Boolean;
begin
  Result := LoadBlockFromStream(Stream, 0, FRomSize);
end;

function TMemory.LoadRamFromStream(const Stream: TStream): Boolean;
begin
  Result := LoadBlockFromStream(Stream, FRomSize, Stream.Size - Stream.Position);
end;

function TMemory.LoadRamBlockFromStream(const Stream: TStream; const Position,
  Len: Integer): Boolean;
begin
  Result := LoadBlockFromStream(Stream, FRomSize + Position, Len);
end;

function TMemory.SaveRamToStream(const Stream: TStream): Boolean;
begin
  Result := Assigned(Stream) and (Stream.Write((P + FRomSize)^, FRamSize) = FRamSize);
end;

procedure TMemory.ClearRam;
begin
  FillChar((P + RomSize)^, FRamSize, 0);
end;

procedure TMemory.RandomizeRam;
const
  //TopByte = Int32($100);
  TopByte = Int32(Byte.MaxValue) + 1;
{$push}{$J+}
const
  RandomizeAlreadyCalled: Boolean = False;
{$pop}
var
  P1, P2: PByte;
begin
  if not RandomizeAlreadyCalled then begin
    RandomizeAlreadyCalled := True;
    Randomize;
  end;
  P1 := P + FRomSize;
  P2 := P1 + FRamSize;
  while P1 < P2 do begin
    P1^ := Byte(Random(TopByte));
    Inc(P1);
  end;
end;

end.

