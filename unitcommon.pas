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

  generic TNumArraySorter<T> = class(TObject)
  public
    type
      TSortCompareProc = function(X, Y: T): Integer;
  strict private
    FSortCompareProc: TSortCompareProc;
    class function DefaultSortCompareProc(X, Y: T): Integer; static;
  public
    constructor Create; virtual;
    constructor Create(ASortCompareProc: TSortCompareProc); virtual;

    procedure SortArray(var A: Array of T);
  end;

  function SpectrumCharToUtf8(S: RawByteString): RawByteString;
  procedure ConvertCodePageFromISO8859_1_to_Utf8(var S: AnsiString);

implementation

function SpectrumCharToUtf8(S: RawByteString): RawByteString;
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

procedure ConvertCodePageFromISO8859_1_to_Utf8(var S: AnsiString);
begin                      
//  https://docs.microsoft.com/en-us/windows/win32/intl/code-page-identifiers
  SetCodePage(RawByteString(S), 28591, False); // set to iso-8859-1
  SetCodePage(RawByteString(S), CP_UTF8, True); // convert
end;

{ TNumArraySorter }

class function TNumArraySorter.DefaultSortCompareProc(X, Y: T): Integer;
begin
  if X < Y then
    Result := -1
  else if Y < X then
    Result := 1
  else
    Result := 0;
end;

constructor TNumArraySorter.Create;
begin
  Create(nil);
end;

constructor TNumArraySorter.Create(ASortCompareProc: TSortCompareProc);
begin
  inherited Create;

  if Assigned(ASortCompareProc) then
    FSortCompareProc := ASortCompareProc
  else
    FSortCompareProc := @DefaultSortCompareProc;
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

