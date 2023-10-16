unit UnitCommon;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  SysUtils;

type
  TProcedureOfObject = procedure of object;
  TProcedureOfObjectString = procedure(var S: String) of object;

  TGlobalCounter = class sealed (TObject)
  public
    class function NextValue(): Int64; static;
  end;

  generic TNumArraySorter<T> = class(TObject)
  public
    type
      TSortCompareProc = function(X, Y: T): Integer;
  strict private
    FSortCompareProc: TSortCompareProc;
  public
    constructor Create(ASortCompareProc: TSortCompareProc); virtual;

    procedure SortArray(var A: Array of T);
  end;

  { TCommonFunctions }

  TCommonFunctions = class sealed (TObject)
  public
    class function GlobalClassNameGenerator(C: TClass): String; static;
    class function GlobalObjectNameGenerator(Obj: TObject): String; static;
    class function SpectrumCharToUtf8(S: RawByteString): RawByteString; static;
    class procedure ConvertCodePageFromISO8859_1_to_Utf8(var S: AnsiString); static;
    class procedure CallRandomizeIfNotCalledAlready();
  end;

implementation

class function TCommonFunctions.GlobalClassNameGenerator(C: TClass): String;
begin
  if C = nil then
    Result := 'z'
  else begin
    Result := C.ClassName;
    Result := StringReplace(Trim(Result), '.', '_', [rfReplaceAll]);
  end;
  Result := Result + TGlobalCounter.NextValue().ToHexString(3);
end;

class function TCommonFunctions.GlobalObjectNameGenerator(Obj: TObject): String;
begin
  if Obj = nil then
    Result := GlobalClassNameGenerator(TClass(nil))
  else begin
    Result := Obj.ClassName;
    Result := StringReplace(Trim(Result), '.', '_', [rfReplaceAll]);
    Result := Result + PtrUInt(Obj).ToHexString(3);
  end;
end;

class function TCommonFunctions.SpectrumCharToUtf8(S: RawByteString): RawByteString;
const
  UdgDelta = $90 - Ord('A');
  BlockCodes: array [0..15] of RawByteString = (
    ' ',
    #$e2#$96#$9d, // quadrant upper right
    #$e2#$96#$98, // quadrant upper left
    #$e2#$96#$80, // upper half block
    #$e2#$96#$97, // quadrant lower right
    #$e2#$96#$90, // right half block
    #$e2#$96#$9a, // quadrant upper left and lower right
    #$e2#$96#$9c, // quadrant upper left and upper right and lower right
    #$e2#$96#$96, // quadrant lower left
    #$e2#$96#$9e, // quadrant upper right and lower left
    #$e2#$96#$8c, // left half block
    #$e2#$96#$9b, // quadrant upper left and upper right and lower left
    #$e2#$96#$84, // lower half block
    #$e2#$96#$9f, // quadrant upper right and lower left and lower right
    #$e2#$96#$99, // quadrant upper left and lower left and lower right
    #$e2#$96#$88  // full block
  );
  BasicConstants: array [0..90] of RawByteString = (
    'RND', 'INKEY$', 'PI', 'FN', 'POINT', 'SCREEN$', 'ATTR', 'AT', 'TAB', 'VAL$',
    'CODE', 'VAL', 'LEN', 'SIN', 'COS', 'TAN', 'ASN', 'ACS', 'ATN', 'LN', 'EXP',
    'INT', 'SQR', 'SGN', 'ABS', 'PEEK', 'IN', 'USR', 'STR$', 'CHR$', 'NOT', 'BIN',
    'OR', 'AND', '<=', '>=', '<>', 'LINE', 'THEN', 'TO', 'STEP', 'DEF FN', 'CAT',
    'FORMAT', 'MOVE', 'ERASE', 'OPEN #', 'CLOSE #', 'MERGE', 'VERIFY', 'BEEP',
    'CIRCLE', 'INK', 'PAPER', 'FLASH', 'BRIGHT', 'INVERSE', 'OVER', 'OUT', 'LPRINT',
    'LLIST', 'STOP', 'READ', 'DATA', 'RESTORE', 'NEW', 'BORDER', 'CONTINUE', 'DIM',
    'REM', 'FOR', 'GO TO', 'GO SUB', 'INPUT', 'LOAD', 'LIST', 'LET', 'PAUSE',
    'NEXT', 'POKE', 'PRINT', 'PLOT', 'RUN', 'SAVE', 'RANDOMIZE', 'IF', 'CLS',
    'DRAW', 'CLEAR', 'RETURN', 'COPY'
  );
var
  I, L, N, LastLen: Integer;
  S1: RawByteString;
begin
  Result := '';
  LastLen := 0;
  L := Length(S);
  I := 0;
  while I < L do begin
    Inc(I);

    case S[I] of
      #$0..#$07, #$09..#$0c, #$0e..#$1f:
        S1 := '';
      #$08:
        begin
          if LastLen > 0 then
            SetLength(Result, Length(Result) - LastLen);
          S1 := '';
        end;
      #$60:
        S1 := #$c2 + #$a3; // pound sign
      #$7f:
        S1 := #$c2 + #$a9; // copyright sign
      #$80..#$8F: // block characters
        S1 := BlockCodes[Ord(S[I]) - $80];
      #$90..#$a4:
        S1 := AnsiChar(Ord(S[I]) - UdgDelta);
      #$a5..#$ff:
        { #todo : some of these strings conditionally put spaces around, some not }
        begin
          N := Ord(S[I]) - $a5;
          S1 := BasicConstants[N];
          if (Length(Result) > 0) and (not (Result[Length(Result)] in [' ', #13])) then
            S1 := ' ' + S1;
          if (I < Length(S)) and (not (S[I + 1] in [' ', #13])) then
            S1 := S1 + ' ';
        end;
    otherwise
      S1 := S[I];
    end;
    Result := Result + S1;
    LastLen := Length(S1);
  end;
end;

class procedure TCommonFunctions.ConvertCodePageFromISO8859_1_to_Utf8(var S: AnsiString);
begin                      
//  https://docs.microsoft.com/en-us/windows/win32/intl/code-page-identifiers
  SetCodePage(RawByteString(S), 28591, False); // set to iso-8859-1
  SetCodePage(RawByteString(S), CP_UTF8, True); // convert
end;

class procedure TCommonFunctions.CallRandomizeIfNotCalledAlready();
{$push}
{$J+}
const
  RandomizeAlreadyCalled: Boolean = False;
{$pop}
begin
  if not RandomizeAlreadyCalled then begin
    RandomizeAlreadyCalled := True;
    Randomize;
  end;
end;

{ TGlobalCounter }

class function TGlobalCounter.NextValue: Int64;
{$push}
{$J+}
const
  GlobalCounter: Int64 = 0;
{$pop}
begin
  Result := GlobalCounter;
  Inc(GlobalCounter);
end;


{ TNumArraySorter }

constructor TNumArraySorter.Create(ASortCompareProc: TSortCompareProc);
begin
  inherited Create;

  FSortCompareProc := ASortCompareProc;
end;

procedure TNumArraySorter.SortArray(var A: array of T);

type
  P_T = ^T;

var
  Pivot, N: T;

  procedure InternalQSort(PFirst, PLast: P_T);
  var
    PLeft, PRight: P_T;
  begin
    PLeft := PFirst;
    PRight := PLast;

    Pivot := (PLeft + (PRight - PLeft) div 2)^;
    repeat
      while FSortCompareProc(PLeft^, Pivot) < 0 do
        Inc(PLeft);
      while FSortCompareProc(Pivot, PRight^) < 0 do
        Dec(PRight);
      if PLeft <= PRight then begin
        N := PLeft^;
        PLeft^ := PRight^;
        PRight^ := N;

        Inc(PLeft);
        Dec(PRight);
      end;
    until PLeft > PRight;

    if PRight > PFirst then
      InternalQSort(PFirst, PRight);
    if PLeft < PLast then
      InternalQSort(PLeft, PLast);
  end;

var
  L: SizeInt;
begin
  L := Length(A) - 1;
  if L > 0 then
    InternalQSort(@A[0], @A[L]);
end;

end.

