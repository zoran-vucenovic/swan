unit UnitDisassembler;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitMemory;

type
  TDisassembler = class(TObject)
  public
    type
      TNumDisplay = (ndHex, ndDec, ndBin);
  public
    const
      AllowedHexPrefixes: array of RawByteString = ('$', '0x', '#', '&');
      AllowedHexSuffixes: array of RawByteString = ('h');
  strict private
    const
      Nop: RawByteString = 'NOP';
      Alu: array of RawByteString = ('ADD A,', 'ADC A,', 'SUB', 'SBC A,', 'AND', 'XOR', 'OR', 'CP');

  strict private
    FDisplayRelativeJumpOffsetAsAbsolute: Boolean;
    FHexfix: RawByteString;
    FHexFormatIndex: Integer;
    FMemory: TMemory;
    FNumDisplay: TNumDisplay;
    FHexPrefix: Boolean;

    function ApplyHexfix(const S: RawByteString): RawByteString; inline;
    function ConvByteToString(B: Byte): RawByteString;
    function ConvWordToString(W: Word): RawByteString;
    procedure SetHexFormatIndex(AValue: Integer);
  public
    constructor Create();
    constructor Create(AMemory: TMemory);
    function Dissasemble(aPc: Word; out aOpcodeLen: Integer): RawByteString;
    class function DecodeHexFormatIndex(AValue: RawByteString): Integer;

    property NumDisplay: TNumDisplay read FNumDisplay write FNumDisplay;
    property Hexfix: RawByteString read FHexfix;
    property HexFormatIndex: Integer read FHexFormatIndex write SetHexFormatIndex;
    property HexIsPrefix: Boolean read FHexPrefix;
    property Memory: TMemory read FMemory write FMemory;
    property DisplayRelativeJumpOffsetAsAbsolute: Boolean read FDisplayRelativeJumpOffsetAsAbsolute write FDisplayRelativeJumpOffsetAsAbsolute;
  end;

implementation

{ TDisassembler }

function TDisassembler.ApplyHexfix(const S: RawByteString): RawByteString;
begin
  if FHexPrefix then
    Result := FHexfix + S
  else
    Result := S + FHexfix;
end;

function TDisassembler.ConvByteToString(B: Byte): RawByteString;
begin
  case FNumDisplay of
    ndHex:
      Result := ApplyHexfix(IntToHex(B));
    ndDec:
      Result := IntToStr(B);
  otherwise
    Result := '%' + B.ToBinString;
  end;
end;

function TDisassembler.ConvWordToString(W: Word): RawByteString;
begin
  case FNumDisplay of
    ndHex:
      Result := ApplyHexfix(IntToHex(W));
    ndDec:
      Result := IntToStr(W);
  otherwise
    Result := '%' + W.ToBinString;
  end;
end;

procedure TDisassembler.SetHexFormatIndex(AValue: Integer);
begin
  if (AValue >= 0) and (FHexFormatIndex <> AValue) then begin
    if AValue < Length(AllowedHexSuffixes) + Length(AllowedHexPrefixes) then begin
      FHexFormatIndex := AValue;
      FHexPrefix := AValue < Length(AllowedHexPrefixes);
      if FHexPrefix then begin
        FHexfix := AllowedHexPrefixes[AValue];
      end else begin
        FHexfix := AllowedHexSuffixes[AValue - Length(AllowedHexPrefixes)];
      end;
    end;
  end;
end;

constructor TDisassembler.Create();
begin
  Create(nil);
end;

constructor TDisassembler.Create(AMemory: TMemory);
begin
  inherited Create;
  FMemory := AMemory;
  FNumDisplay := ndHex;
  FHexFormatIndex := -1;
  SetHexFormatIndex(0);
end;

function TDisassembler.Dissasemble(aPc: Word; out aOpcodeLen: Integer
  ): RawByteString;
const
  Cond: array [0..7] of RawByteString = ('NZ', 'Z', 'NC', 'C', 'PO', 'PE', 'P', 'M');

var
  Opcode: Byte;
  x, y, z: Byte;
  Pref: Byte;

  {$push}
  {$Q-}{$R-}
  function ReadNextByte(): Byte;
  begin
    Inc(aPc);
    Result := FMemory.ReadByte(aPc);
    Inc(aOpcodeLen);
  end;

  function ResolveMemHL(): RawByteString;
  var
    D: Int8;
    Bd: Byte absolute D;
  begin
    case Pref of
      $DD:
        Result := 'IX';
      $FD:
        Result := 'IY';
    otherwise
      Exit('(HL)');
    end;

    Bd := ReadNextByte();
    if D < 0 then begin
      Result := Result + ' - ';
      D := -D;
    end else
      Result := Result + ' + ';

    Result := '(' + Result + Bd.ToString + ')';
  end;
  {$pop}

  function ResolveHL(): RawByteString; inline;
  begin
    case Pref of
      $DD:
        Result := 'IX';
      $FD:
        Result := 'IY';
    otherwise
      Result := 'HL';
    end;
  end;

  function ResolveH(): RawByteString; inline;
  begin
    case Pref of
      $DD:
        Result := 'IXH';
      $FD:
        Result := 'IYH';
    otherwise
      Result := 'H';
    end;
  end;

  function ResolveL(): RawByteString; inline;
  begin
    case Pref of
      $DD:
        Result := 'IXL';
      $FD:
        Result := 'IYL';
    otherwise
      Result := 'L';
    end;
  end;

  function ResolveTableRP(): RawByteString; inline;
  begin
    case y shr 1 of
      0:
        Result := 'BC';
      1:
        Result := 'DE';
      2:
        Result := ResolveHL();
    otherwise //  3:
      if x <> 3 then
        Result := 'SP'
      else
        Result := 'AF';
    end;
  end;

  function ResolveTableR(const B: Byte): RawByteString;
  begin
    case B of
      0:
        Result := 'B';
      1:
        Result := 'C';
      2:
        Result := 'D';
      3:
        Result := 'E';
      7:
        Result := 'A';
      4:
        Result := ResolveH;
      5:
        Result := ResolveL;
    otherwise // 6:
      Result := ResolveMemHL();
    end;
  end;

  function ResolveRegSimple(const B: Byte): RawByteString;
  begin
    case B of
      0:
        Result := 'B';
      1:
        Result := 'C';
      2:
        Result := 'D';
      3:
        Result := 'E';
      7:
        Result := 'A';
      4:
        Result := 'H';
      5:
        Result := 'L';
    otherwise // 6:
      Result := '';
    end;
  end;

  function ReadNextWord(): Word;
  begin
    WordRec(Result).Lo := ReadNextByte();
    WordRec(Result).Hi := ReadNextByte();
  end;

  function GetRelativeJumpOffset(): RawByteString;
  var
    N: Int32;
    D: Int8;
    B: Byte absolute D;
  begin
    B := ReadNextByte();
    N := D;
    if not FDisplayRelativeJumpOffsetAsAbsolute then begin
      Result := (N + 2).ToString;
      if Result[1] <> '-' then
        Result := '+' + Result;
    end else begin
      N := N + aPc + 1;
      Result := ConvWordToString(LongRec(N).Lo);
    end;
  end;

  function Bli(const A, B: Byte): RawByteString;
  begin
    case B of
      0: // LDI, LDD, LDIR, LDDR
        Result := 'LD';
      1: // CPI, CPD, CPIR, CPDR
        Result := 'CP';
      2: // INI, IND, INIR, INDR
        Result := 'IN';
    otherwise // 3
      // OUTI, OUTD, OTIR, OTDR
      if A >= 6 then
        Result := 'OT'
      else
        Result := 'OUT';
    end;

    case A of
      4:
        Result := Result + 'I';
      5:
        Result := Result + 'D';
      6:
        Result := Result + 'IR';
    otherwise // 7
      Result := Result + 'DR';
    end;
  end;

  function ProcessCB(): RawByteString;
  const
    Rot: array [0..7] of RawByteString = ('RLC', 'RRC', 'RL', 'RR', 'SLA', 'SRA', 'SLL', 'SRL');
  begin
    if Pref <> $CB then begin
      Result := ResolveMemHL();
      Opcode := ReadNextByte();
      x := Opcode shr 6;
      y := (Opcode shr 3) and %111;
      z := Opcode and %111;
    end else
      Result := ResolveTableR(z);

    case x of
      0:
        Result := Rot[y] + ' ' + Result;
      1:
        begin
          Result := 'BIT ' + y.ToString + ', ' + Result;
          Exit;
        end;
      2:
        Result := 'RES ' + y.ToString + ', ' + Result;
    otherwise // 3
      Result := 'SET ' + y.ToString + ', ' + Result;
    end;

    if Pref <> $CB then
      if z <> 6 then begin
        // (Alternative syntaxes)
        // either:
        Result := Result + ', ' + ResolveRegSimple(z);
        // or:
        //Result := 'LD ' + ResolveRegSimple(z) + ', ' + Result;
      end;
  end;

  function ProcessED(): RawByteString;
  begin
    Result := '';
    case x of
      1:
        begin
          case z of
            0: // IN (C); IN r[y], (C)
              begin
                Result := ResolveRegSimple(y);
                if Result <> '' then
                  Result := Result + ', ';
                Result := 'IN ' + Result + '(C)';
              end;
            1:
              begin
                Result := ResolveRegSimple(y);
                if Result = '' then
                  Result := '0';
                Result := 'OUT (C), ' + Result;
              end;
            2:
              begin
                if y and 1 = 0 then
                  Result := 'SBC'
                else
                  Result := 'ADC';
                Result := Result + ' HL, ' + ResolveTableRP();
              end;
            3:
              begin
                Result := '(' + ConvWordToString(ReadNextWord()) + ')';
                if y and 1 = 0 then
                  Result := Result + ', ' + ResolveTableRP()
                else
                  Result := ResolveTableRP() + ', ' + Result;
                Result := 'LD ' + Result;
              end;
            4:
              Result := 'NEG';
            5:
              begin
                if y <> 1 then
                  Result := 'RETN'
                else
                  Result := 'RETI';
              end;
            6:
              Result := 'IM ' + (((y and 3) shl 1) div 3).ToString;

          otherwise // 7
            case y of
              0:
                Result := 'LD I, A';
              1:
                Result := 'LD R, A';
              2:
                Result := 'LD A, I';
              3:
                Result := 'LD A, R';
              4:
                Result := 'RRD';
              5:
                Result := 'RLD';
            otherwise
              Result := Nop;
            end;
          end;
        end;
      2:
        if (z and %100 = 0) and (y and %100 <> 0) then begin
          Result := Bli(y, z);
        end else
          Result := Nop;
    otherwise // 0, 3
      Result := Nop;
    end;
  end;

  function ProcessX0(): RawByteString;
  begin
    case z of
      0:
        begin
          case y of
            0:
              Result := Nop;
            1:
              Result := 'EX AF, AF''';
          otherwise
            if y = 2 then
              Result := 'DJNZ'
            else begin
              Result := 'JR';

              if y >= 4 then begin
                Result := Result + ' ' + Cond[y and 3] + ',';
              end;
            end;

            Result := Result + ' ' + GetRelativeJumpOffset();
          end;
        end;
      1:
        begin
          if y and 1 = 0 then begin
            Result := 'LD ' + ResolveTableRP() + ', ' + ConvWordToString(ReadNextWord());
          end else begin
            Result := 'ADD ' + ResolveHL() + ', ' + ResolveTableRP();
          end;
        end;
      2:
        begin
          case y of
            0:
              Result := '(BC), A';
            1:
              Result := 'A, (BC)';
            2:
              Result := '(DE), A';
            3:
              Result := 'A, (DE)';
          otherwise
            Result := '(' + ConvWordToString(ReadNextWord()) + ')';
            case y of
              4: // LD (nn), HL
                Result := Result + ', ' + ResolveHL();
              5: // LD HL, (nn)
                Result := ResolveHL() + ', ' + Result;
              6: // LD (nn), A
                Result := Result + ', A';
            otherwise // 7
              // LD A, (nn)
              Result := 'A, ' + Result;
            end;
          end;

          Result := 'LD ' + Result;
        end;
      3:
        begin
          if y and 1 = 0 then
            Result := 'INC '
          else
            Result := 'DEC ';

          Result := Result + ResolveTableRP();
        end;
      4:
        Result := 'INC ' + ResolveTableR(y);
      5:
        Result := 'DEC ' + ResolveTableR(y);
      6:
        begin
          Result := ResolveTableR(y);
          Result := 'LD ' + Result + ', ' + ConvByteToString(ReadNextByte());
        end;
    otherwise // 7
      case y of
        0: // RLCA
          Result := 'RLCA';
        1: // RRCA
          Result := 'RRCA';
        2: // RLA
          Result := 'RLA';
        3: // RRA
          Result := 'RRA';
        4: // DAA
          Result := 'DAA';
        5: // CPL
          Result := 'CPL';
        6: // SCF
          Result := 'SCF';
      otherwise // 7:
        // CCF
        Result := 'CCF';
      end;
    end;
  end;

  function ProcessX1(): RawByteString;
  begin
    if z <> 6 then begin
      if y <> 6 then
        Result := ResolveTableR(z)
      else
        Result := ResolveRegSimple(z);
      Result := ResolveTableR(y) + ', ' + Result;
    end else begin
      if y <> 6 then begin
        Result := ResolveRegSimple(y) + ', ' + ResolveMemHL();
      end else begin
        Result := 'HALT';
        Exit;
      end;
    end;
    Result := 'LD ' + Result;
  end;

  function ProcessX2(): RawByteString;
  begin
    Result := Alu[y] + ' ' + ResolveTableR(z);
  end;

  function ProcessX3(): RawByteString;
  var
    N: Integer;
  begin
    Result := '';
    case z of
      0:
        Result := 'RET ' + Cond[y];
      1:
        if y and 1 = 0 then begin
          Result := 'POP ' + ResolveTableRP();
        end else begin
          case y shr 1 of
            0:
              Result := 'RET';
            1:
              Result := 'EXX';
            2:
              Result := 'JP ' + ResolveHL();
          otherwise // 3:
            Result := 'LD SP, ' + ResolveHL();
          end;
        end;
      2:
        begin
          Result := 'JP ' + Cond[y] + ', ' + ConvWordToString(ReadNextWord());
        end;
      3:
        case y of
          0:
            begin
              Result := 'JP ' + ConvWordToString(ReadNextWord());
            end;
          1:
            Result := ''; // CB prefix
          2:
            begin
              Result := 'OUT (' + ConvByteToString(ReadNextByte()) + '), A';
            end;
          3:
            begin
              Result := 'IN A, (' + ConvByteToString(ReadNextByte()) + ')';
            end;
          4:
            Result := 'EX (SP), ' + ResolveHL();
          5:
            Result := 'EX DE, HL';
          6:
            Result := 'DI';

        otherwise // 7:
          Result := 'EI';
        end;

      4:
        begin
          Result := 'CALL ' + Cond[y] + ', ' + ConvWordToString(ReadNextWord());
        end;

      5:
        if y and 1 = 0 then begin
          Result := 'PUSH ' + ResolveTableRP();
        end else if y shr 1 = 0 then begin
          Result := 'CALL ' + ConvWordToString(ReadNextWord())
        end else
          Result := Nop; // prefix EE, EE or FD

      6:
        begin
          Result := Alu[y] + ' ' + ConvByteToString(ReadNextByte());
        end;

    otherwise // 7
      Result := 'RST ' + ConvByteToString(y);
    end;
  end;

begin
  Opcode := FMemory.ReadByte(aPc);

  aOpcodeLen := 1;
  case Opcode of
    $CB, $ED, $DD, $FD:
      begin
        Pref := Opcode;
        Opcode := ReadNextByte();
      end;
  otherwise
    Pref := 0;
  end;

  x := Opcode shr 6;
  y := (Opcode shr 3) and %111;
  z := Opcode and %111;

  case Pref of
    $CB:
      begin
        Result := ProcessCB();
        Exit;
      end;
    $ED:
      begin
        Result := ProcessED();
        Exit;
      end;
    $DD, $FD:
      case OpCode of
        $DD, $ED, $FD:
          begin
            // NONI
            Result := Nop;
            Exit;
          end;
        $CB:
          begin
            Result := ProcessCB;
            Exit;
          end;
      otherwise
        // Keep Pref, as it will change the instructions which affect HL!
      end;
  otherwise
  end;

  case x of
    0:
      Result := ProcessX0;
    1:
      Result := ProcessX1;
    2:
      Result := ProcessX2;
  otherwise // 3:
    Result := ProcessX3;
  end;
end;

class function TDisassembler.DecodeHexFormatIndex(AValue: RawByteString
  ): Integer;
var
  I: Integer;
begin
  for I := Low(AllowedHexPrefixes) to High(AllowedHexPrefixes) do begin
    if CompareText(AllowedHexPrefixes[I], AValue) = 0 then begin
      Exit(I);
    end;
  end;

  for I := Low(AllowedHexSuffixes) to High(AllowedHexSuffixes) do begin
    if CompareText(AllowedHexSuffixes[I], AValue) = 0 then begin
      Exit(Length(AllowedHexPrefixes) + I);
    end;
  end;

  Result := -1;
end;

end.
