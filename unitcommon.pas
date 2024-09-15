unit UnitCommon;
// Copyright 2022-2024 Zoran Vučenović
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
    const
      NonBreakSpace: RawByteString = #$c2 + #$a0;
      NarrowNonBreakSpace: RawByteString = #$e2 + #$80 + #$af;
  public
    class function GlobalClassNameGenerator(C: TClass): String; static;
    class function GlobalObjectNameGenerator(Obj: TObject): String; static;
    class function SpectrumCharToUtf8(const Is128K: Boolean; S: RawByteString): RawByteString; static;
    class procedure ConvertCodePageFromCp1252ToUtf8(var S: AnsiString); static;
    class procedure CallRandomizeIfNotCalledAlready(); static;
  strict private
    class function CheckStartForDecimal(const S: String): Boolean; static;
  public
    class function TryStrToIntDecimal(const S: String; out N: Int32): Boolean; static;
    class function TryStrToInt64Decimal(const S: String; out N: Int64): Boolean; static;
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
  if Assigned(Obj) then
    Result := GlobalClassNameGenerator(Obj.ClassType)
  else
    Result := GlobalClassNameGenerator(TClass(nil));
end;

class function TCommonFunctions.SpectrumCharToUtf8(const Is128K: Boolean;
  S: RawByteString): RawByteString;
const
  UdgDelta = $90 - Ord('A'); // used for UDG area ($90-$A4)
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
  BasicConstants: array [0..92] of RawByteString = (
    'SPECTRUM', 'PLAY', // these two are additional keywords on 128k models
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
  I: Integer;
  L: Integer;
  J: Integer;
  K: Integer;
  KJ: Integer;
  CharLengths: array of Integer;
  S1: RawByteString;

begin
  Result := '';
  L := Length(S);
  SetLength(CharLengths{%H-}, L);
  I := 0;
  J := -1;
  while I < L do begin
    Inc(I);

    if S[I] = #08 then begin // backspace, delete the previous character
      if J >= 0 then begin
        SetLength(Result, Length(Result) - CharLengths[J]);
        Dec(J);
      end;
    end else begin
      KJ := 0;
      case S[I] of
        #$0..#$05, #$07, #$0a..#$0c, #$0e, #$0f, #$18..#$1f:
          S1 := '?';
        #$06:
          S1 := ' ';
        #$09:
          S1 := '';
        #$10..#$15:
          begin
            S1 := '';
            KJ := 1;
            Inc(I);
          end;
        #$16, #$17:
          begin
            S1 := ' ';
            KJ := 2;
            I := I + KJ;
          end;
        #$5e:
          S1 := #$e2 + #$86 + #$91; // upwards arrow symbol
        #$60:
          S1 := #$c2 + #$a3; // pound sign
        #$7f:
          S1 := #$c2 + #$a9; // copyright sign
        #$80..#$8F: // block characters
          S1 := BlockCodes[Ord(S[I]) - $80];
        #$90..#$ff:
          if (S[I] <= #$a2) or ((not Is128K) and (S[I] <= #$a4)) then begin
            // UDG - user defined graphics - 21 characters, by default same as letters A-U
            // On 128K models the last two bytes in UDG area are mapped to
            // additional two keywords, leaving 19 characters for UDG.
            // The best we can do is use these keywords if model is 128K when inserting tape
            S1 := AnsiChar(Ord(S[I]) - UdgDelta);
          end else begin
            S1 := BasicConstants[Ord(S[I]) - $a3];
            if (Length(Result) > 0) and (not (Result[Length(Result)] in [' ', #13])) then begin
              K := J;
              while K >= 0 do begin
                if CharLengths[K] > 0 then begin
                  S1 := ' ' + S1;
                  Break;
                end;
                Dec(K);
              end;
            end;

            if (I < Length(S)) and (not (S[I + 1] in [' ', #13])) then
              S1 := S1 + ' ';
          end;
      otherwise
        S1 := S[I];
      end;
      Result := Result + S1;

      if I < L then
        repeat
          Inc(J);
          if J >= Length(CharLengths) then
            SetLength(CharLengths, J * 7 div 5 + 2);
          CharLengths[J] := Length(S1);
          S1 := '';
          Dec(KJ);
        until KJ < 0;
    end;
  end;
  SetLength(CharLengths, 0);
end;

class procedure TCommonFunctions.ConvertCodePageFromCp1252ToUtf8(var S: AnsiString);
begin
// https://docs.microsoft.com/en-us/windows/win32/intl/code-page-identifiers
  SetCodePage(RawByteString(S), 1252, False); // set to cp1252
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

class function TCommonFunctions.CheckStartForDecimal(const S: String): Boolean;
var
  P, Pe: PAnsiChar;
begin
  P := PAnsiChar(S);
  Pe := P + Length(S);

  while (P < Pe) and (P^ <= AnsiChar($20)) do
    Inc(P);

  if P^ = '-' then
    Inc(P);

  case P^ of
    '0':
      if not ((P + 1)^ in [#0, '0'..'9']) then
        Exit(False);
    '1'..'9':
      ;
  otherwise
    Exit(False);
  end;

  Result := True;
end;

class function TCommonFunctions.TryStrToIntDecimal(const S: String; out N: Int32
  ): Boolean;
begin
  Result := CheckStartForDecimal(S) and TryStrToInt(S, N);
end;

class function TCommonFunctions.TryStrToInt64Decimal(const S: String; out
  N: Int64): Boolean;
begin
  Result := CheckStartForDecimal(S) and TryStrToInt64(S, N);
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

