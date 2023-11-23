unit Z80Processor;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}
{$i zxinc.inc}
{$R-}{$Q-}

interface

uses
  Classes, SysUtils, UnitMemory, FastIntegers;

type

  TProcessorEvent = procedure of object;
  TWriteScrEvent = procedure(TicksTo: Int32Fast) of object;

  { TProcessor }

  TProcessor = class(TObject)
  public
    FrameTicks: Int32Fast;
    ContendedHighBank: Boolean;
    ContentionFrom: Int32Fast;
    ContentionTo: Int32Fast;
    TicksPerLine: Int32Fast;

  strict private

    type

      TRec16 = packed record
        case Integer of
          0: // unsigned word (16-bit unsigned int)
            (U16bit: Word);
          1: // pair of unsigned bytes
          {$ifdef endian_little}
            (UByteLo, UByteHi: Byte);
          {$else}
            (UByteHi, UByteLo: Byte);
          {$endif}
      end;

      // general purpose registers
      TGPRegs = record
        BC: TRec16;
        DE: TRec16;
        HL: TRec16;
      end;

  strict private
    FIff1: Boolean;
    FIff2: Boolean;
    FRegPC: Word;
    FRegAF: TRec16;
    FRegIx: TRec16;
    FRegIy: TRec16;
    FRegIR: TRec16;
    FRegSP: Word;
    GPRegs: TGPRegs;  // general purpose registers
    GPRegs1: TGPRegs; // alternative set of general purpose registers
    FRegAF1: TRec16;
    FInterruptMode: Byte;

    // internal register WZ, also called memptr, is implemented
    // because its state affects flags in some special cases
    // (see https://faqwiki.zxnet.co.uk/wiki/Z80#Bits_3_and_5_of_the_F_register)
    FRegWZ: Word;
    procedure SetInterruptMode(const AValue: Byte);

  strict private
    PRegA: PByte;
    PRegB: PByte;
    PRegC: PByte;
    PRegD: PByte;
    PRegE: PByte;
    PRegH: PByte;
    PRegL: PByte;

    PRegAF: PWord;
    PRegBC: PWord;
    PRegDE: PWord;
    PRegHL: PWord;
    PRegSP: PWord;

    PRegIx: PWord;
    PRegIy: PWord;
    PRegIxH: PByte;
    PRegIxL: PByte;
    PRegIyH: PByte;
    PRegIyL: PByte;

    procedure InitRegPointers;

  public // we need to make these public, so that snapshots can be read or written :(
    property RegAF: Word read FRegAF.U16bit write FRegAF.U16bit;
    property RegBC: Word read GPRegs.BC.U16bit write GPRegs.BC.U16bit;
    property RegDE: Word read GPRegs.DE.U16bit write GPRegs.DE.U16bit;
    property RegHL: Word read GPRegs.HL.U16bit write GPRegs.HL.U16bit;
    property RegWZ: Word read FRegWZ write FRegWZ;

    property RegA: Byte read FRegAF.UByteHi write FRegAF.UByteHi;
    property RegF: Byte read FRegAF.UByteLo write FRegAF.UByteLo;
    property RegB: Byte read GPRegs.BC.UByteHi write GPRegs.BC.UByteHi;
    property RegC: Byte read GPRegs.BC.UByteLo write GPRegs.BC.UByteLo;
    property RegD: Byte read GPRegs.DE.UByteHi write GPRegs.DE.UByteHi;
    property RegE: Byte read GPRegs.DE.UByteLo write GPRegs.DE.UByteLo;
    property RegH: Byte read GPRegs.HL.UByteHi write GPRegs.HL.UByteHi;
    property RegL: Byte read GPRegs.HL.UByteLo write GPRegs.HL.UByteLo;

    property Ix: Word read FRegIx.U16bit write FRegIx.U16bit;
    property Iy: Word read FRegIy.U16bit write FRegIy.U16bit;

    property RegIR: Word read FRegIR.U16bit write FRegIR.U16bit;
    property RegI: Byte read FRegIR.UByteHi write FRegIR.UByteHi; // interrupt register;
    property RegR: Byte read FRegIR.UByteLo write FRegIR.UByteLo; // refresh memory register;

    property RegSP: Word read FRegSP write FRegSP; // stack pointer
    property RegPC: Word read FRegPC write FRegPC; // program counter

    property RegAF1: Word read FRegAF1.U16bit write FRegAF1.U16bit;
    property RegBC1: Word read GPRegs1.BC.U16bit write GPRegs1.BC.U16bit;
    property RegDE1: Word read GPRegs1.DE.U16bit write GPRegs1.DE.U16bit;
    property RegHL1: Word read GPRegs1.HL.U16bit write GPRegs1.HL.U16bit;

    property RegA1: Byte read FRegAF1.UByteHi write FRegAF1.UByteHi;
    property RegF1: Byte read FRegAF1.UByteLo write FRegAF1.UByteLo;
    property RegH1: Byte read GPRegs1.HL.UByteHi write GPRegs1.HL.UByteHi;
    property RegL1: Byte read GPRegs1.HL.UByteLo write GPRegs1.HL.UByteLo;
    property RegB1: Byte read GPRegs1.BC.UByteHi write GPRegs1.BC.UByteHi;
    property RegC1: Byte read GPRegs1.BC.UByteLo write GPRegs1.BC.UByteLo;
    property RegD1: Byte read GPRegs1.DE.UByteHi write GPRegs1.DE.UByteHi;
    property RegE1: Byte read GPRegs1.DE.UByteLo write GPRegs1.DE.UByteLo;

    property Iff1: Boolean read FIff1 write FIff1;
    property Iff2: Boolean read FIff2 write FIff2;
    property InterruptMode: Byte read FInterruptMode write SetInterruptMode;

  strict private
    const
      NMIStartAdr = Word($0066);
      IntMode1StartAdr = Word($0038);

      Bit12 = 1 shl 12;
      Mask12 = Bit12 - 1;

    type
      TIncDecProc = procedure(const PB: PByte) of object;

  strict private
    FFlagsModified: Boolean; // register "Q"
    FTStatesInCurrentFrame: Int32Fast;
    FPrefixByte: Byte;
    FMemory: UnitMemory.TMemory;
    FOnInputRequest: TProcessorEvent;
    FOnOutputRequest: TProcessorEvent;
    FOnNeedWriteScreen: TWriteScrEvent;

    procedure EmptyProcessorEvent;
    procedure EmptyWriteScrEvent({%H-}TicksTo: Int32Fast);

    class function IsByteParityEven(B: Byte): Boolean; static; inline;

    procedure SetOnInputRequest(AValue: TProcessorEvent);
    procedure SetOnNeedWriteScreen(AValue: TWriteScrEvent);
    procedure SetOnOutputRequest(AValue: TProcessorEvent);

    procedure RefreshMem; inline;

    procedure CheckContention; inline;
    procedure Contention; inline;
    procedure ContentionAndIncTStates; inline;
    procedure Contention2Times; inline;
    procedure Contention5Times; inline;
    procedure IOTimings; inline;
    procedure RequestInput; inline;
    procedure RequestOutput; inline;
    function ReadMem(const Adr: Word): Byte; inline;
    procedure WriteMem(const Adr: Word; const B: Byte); inline;
    function ReadNextByte: Byte; inline;
    function ReadNextWord: Word; inline;
    function PopFromStack: Word; inline;
    procedure PushToStack(const W: Word); inline;

    function SubOrSubc(const B: Int16Fast; var Flags: Int16Fast): Int16Fast; inline;
    procedure Alu(const I: Integer; const B: Byte); inline;
    procedure Rot(const I: Integer; var B: Byte); inline;
    procedure Bli(const A, B: Byte);

    // EX AF, AF`
    procedure ExAF; inline;
    // SUB A,
    procedure SubA(const B: Byte); inline;
    // ADC HL, ss
    procedure AdcHLss(const W: Word);
    // SBC HL, ss
    procedure SbcHLss(const W: Word);
    // INC r
    procedure IncR(const PB: PByte);
    // DEC r
    procedure DecR(const PB: PByte);
    // RLCA
    procedure RLCA;
    // RRCA
    procedure RRCA;
    // RLA
    procedure RLA; inline;
    // RRA
    procedure RRA; inline;
    // CPL
    procedure NotAccumulator; inline;
    // SCF
    procedure SCF; inline;
    // CCF
    procedure CCF; inline;
    // DAA
    procedure DAA;
    // EXX
    procedure Exx; inline;
    // CALL nn, CALL cc, nn
    procedure CALL(const DoCall: Boolean);
    // BIT
    procedure TestBit(const BitNo, B: Byte); inline;
    // SET
    procedure SetBit(const BitNo: Byte; var B: Byte); inline;
    // RES
    procedure ResetBit(const BitNo: Byte; var B: Byte); inline;
    // RLD, RRD
    procedure RldOrRrd(const IsRRD: Boolean);
    // IN A, (n)
    procedure InAn;
    // IN (C); IN r[y], (C)
    procedure InC0;
    // OUT (n), A
    procedure OUTnA;

  strict private
    FAddressBus: Word;
    FDataBus: Byte;
    FHalt: Boolean;
    FIntPin: Boolean;
    FNMI: Boolean;
    //
  public
    // Pins
    // There are 40 pins.
    // Three pins which are not emulated here:
    // - Clk -- the clock is simulated differently.
    // - power -- pointless in emulation.
    // - ground -- --//--

    // adress bus (output) - 16 pins, emulated as Word (16 bit unsigned int)
    property AddressBus: Word read FAddressBus;
    // data bus (i/o) - 8 pins, emulated as Byte (8 bit unsigned int)
    property DataBus: Byte read FDataBus write FDataBus;

    // System control pins (6 pins, all output)
    // not emulated directly
    //property M1: Boolean read FM1;
    //property MREQ: Boolean read FMREQ;
    //property IORQ: Boolean read FIORQ;
    //property RD: Boolean read FRD;
    //property WR: Boolean read FWR;
    //property RFSH: Boolean read FRFSH;

    // CPU control (5 pins, one output, other four input)
    property Halt: Boolean read FHalt write FHalt; {write, because of szx}
    //property Wait: Boolean write FWait;
    property IntPin: Boolean read FIntPin write FIntPin;
    // Pins NMI and RESET implemented as procedures
    procedure NMI();
    procedure ResetPin(); // "soft" reset

    // CPU Bus control (two pins, one input, one output)
    // not emulated directly
    //property BusRQ: Boolean write FBusRQ;
    //property BusAck: Boolean read FBusAck;

  public
    constructor Create;

    procedure ResetCPU; // "hard" reset, full initialization

    procedure DoProcess;

    procedure SetMemory(Mem: TMemory);

    property FlagsModified: Boolean read FFlagsModified write FFlagsModified;

    // PrefixByte might be $CB, $DD, $ED, $FD and $FB
    // ($FB is not a real opcode prefix, but it is opcode for EI instruction — used just to skip interrupt check)
    property PrefixByte: Byte read FPrefixByte write FPrefixByte;

    property TStatesInCurrentFrame: Int32Fast read FTStatesInCurrentFrame write FTStatesInCurrentFrame;

    property OnInputRequest: TProcessorEvent read FOnInputRequest write SetOnInputRequest;
    property OnOutputRequest: TProcessorEvent read FOnOutputRequest write SetOnOutputRequest;
    property OnNeedWriteScreen: TWriteScrEvent read FOnNeedWriteScreen write SetOnNeedWriteScreen;
  end;

implementation

{ TProcessor }

procedure TProcessor.InitRegPointers;
begin
  PRegA := @(FRegAF.UByteHi);
  PRegB := @(GPRegs.BC.UByteHi);
  PRegC := @(GPRegs.BC.UByteLo);
  PRegD := @(GPRegs.DE.UByteHi);
  PRegE := @(GPRegs.DE.UByteLo);
  PRegH := @(GPRegs.HL.UByteHi);
  PRegL := @(GPRegs.HL.UByteLo);

  PRegIxH := @(FRegIx.UByteHi);
  PRegIxL := @(FRegIx.UByteLo);
  PRegIyH := @(FRegIy.UByteHi);
  PRegIyL := @(FRegIy.UByteLo);

  PRegAF := @(FRegAF.U16bit);
  PRegBC := @(GPRegs.BC.U16bit);
  PRegDE := @(GPRegs.DE.U16bit);
  PRegHL := @(GPRegs.HL.U16bit);
  PRegSP := @(FRegSP);

  PRegIx := @(FRegIx.U16bit);
  PRegIy := @(FRegIy.U16bit);
end;

procedure TProcessor.EmptyProcessorEvent;
begin
  //
end;

procedure TProcessor.EmptyWriteScrEvent(TicksTo: Int32Fast);
begin
  //
end;

procedure TProcessor.SetInterruptMode(const AValue: Byte);
begin
  case AValue of
    0, {1,} 2:
      FInterruptMode := AValue;
  otherwise
    FInterruptMode := 1;
  end;
end;

{ True when number of set bits is even, False when odd. }
class function TProcessor.IsByteParityEven(B: Byte): Boolean;
{$if SizeOf(SizeInt) >= 8}
begin
  { http://www.graphics.stanford.edu/~seander/bithacks.html#ParityWith64Bits }
  // note: the original function on that link returns true when number of set bits is odd, but we return the opposite.
  Result :=
    (((QWord(B) * QWord($0101010101010101)) and QWord($8040201008040201)) mod $01FF) and 1 = 0;
end;
{$else}
// On 32-bit system the "naive" implementation seems to be faster, actually:
var
  Aux: Byte;
begin
  Aux := 0;
  while B <> 0 do begin
    Aux := Aux xor B;
    B := B shr 1;
  end;
  Result := Aux and 1 = 0;
end;
{$endif}

procedure TProcessor.CheckContention;
var
  N: Int32Fast;
begin
  // ULA contention (conditionally add t-states)
  //                             58037                                 14361
  //if (FTStatesInCurrentFrame < 57247) and (FTStatesInCurrentFrame >= 14335) then begin
  if (FTStatesInCurrentFrame < ContentionTo) and (FTStatesInCurrentFrame >= ContentionFrom)
  then begin
  //  N := ((FTStatesInCurrentFrame - 14335) mod 224) and $87;
    N := ((FTStatesInCurrentFrame - ContentionFrom) mod TicksPerLine) and $87;
    //N := (FProcessor.TStatesInCurrentFrame - 14335) mod 224;
    //if N <= 127 then begin
    //  N := N and 7;
      if N <= 5 then
        FTStatesInCurrentFrame := FTStatesInCurrentFrame + 6 - N;
    //end;
  end;
end;

procedure TProcessor.Contention;
begin
  if (FAddressBus and $C000 = $4000)
     or (ContendedHighBank and (FAddressBus and $C000 = $C000))
  then
    CheckContention;
end;

procedure TProcessor.ContentionAndIncTStates;
begin
  Contention;
  Inc(FTStatesInCurrentFrame);
end;

procedure TProcessor.Contention2Times;
begin
  if (FAddressBus and $C000 = $4000)
     or (ContendedHighBank and (FAddressBus and $C000 = $C000))
  then begin
    CheckContention;
    Inc(FTStatesInCurrentFrame);
    CheckContention;
    Inc(FTStatesInCurrentFrame);
  end else
    Inc(FTStatesInCurrentFrame, 2);
end;

procedure TProcessor.Contention5Times;
begin
  if (FAddressBus and $C000 = $4000)
     or (ContendedHighBank and (FAddressBus and $C000 = $C000))
  then begin
    CheckContention;
    Inc(FTStatesInCurrentFrame);
    CheckContention;
    Inc(FTStatesInCurrentFrame);
    CheckContention;
    Inc(FTStatesInCurrentFrame);
    CheckContention;
    Inc(FTStatesInCurrentFrame);
    CheckContention;
    Inc(FTStatesInCurrentFrame);
  end else
    Inc(FTStatesInCurrentFrame, 5);
end;

procedure TProcessor.IOTimings;
begin
  if (FAddressBus and $C000 = $4000)
     or (ContendedHighBank and (FAddressBus and $C000 = $C000))
  then begin
    //if FAddressBus and $FF = $FE then begin
    if FAddressBus and 1 = 0 then begin
      // C:1, C:3
      CheckContention;
      Inc(FTStatesInCurrentFrame);
      CheckContention;
      Inc(FTStatesInCurrentFrame, 3);
    end else begin
      // C:1, C:1, C:1, C:1
      CheckContention;
      Inc(FTStatesInCurrentFrame);
      CheckContention;
      Inc(FTStatesInCurrentFrame);
      CheckContention;
      Inc(FTStatesInCurrentFrame);
      CheckContention;
      Inc(FTStatesInCurrentFrame);
    end;
  end else begin
    //if FAddressBus and $FF = $FE then begin
    if FAddressBus and 1 = 0 then begin
      // N:1, C:3
      Inc(FTStatesInCurrentFrame);
      CheckContention;
      Inc(FTStatesInCurrentFrame, 3);
    end else begin
      // N:4
      Inc(FTStatesInCurrentFrame, 4);
    end;
  end;
end;

procedure TProcessor.RequestInput;
begin
  IOTimings;
  FOnInputRequest();
end;

procedure TProcessor.RequestOutput;
begin
  FOnNeedWriteScreen(FTStatesInCurrentFrame - 4);
  IOTimings;
  FOnOutputRequest();
end;

function TProcessor.ReadMem(const Adr: Word): Byte;
begin
  FAddressBus := Adr;
  Contention;
  Result := FMemory.ReadByte(Adr);
  Inc(FTStatesInCurrentFrame, 3);
end;

procedure TProcessor.WriteMem(const Adr: Word; const B: Byte);
begin
  FAddressBus := Adr;
  Contention;
  FOnNeedWriteScreen(FTStatesInCurrentFrame);

  FMemory.WriteByte(Adr, B);
  Inc(FTStatesInCurrentFrame, 3);
end;

function TProcessor.ReadNextByte: Byte;
begin
  FAddressBus := FRegPC;
  Contention;

  Result := FMemory.ReadByte(FAddressBus);

  Inc(FTStatesInCurrentFrame, 3);
  Inc(FRegPC);
end;

function TProcessor.ReadNextWord: Word;
var
  ResultAsRec16: TRec16 absolute Result;
begin
  ResultAsRec16.UByteLo := ReadNextByte;
  ResultAsRec16.UByteHi := ReadNextByte;
end;

function TProcessor.PopFromStack: Word;
begin
  TRec16(Result).UByteLo := ReadMem(FRegSP);
  Inc(FRegSP);
  TRec16(Result).UByteHi := ReadMem(FRegSP);
  Inc(FRegSP);
end;

procedure TProcessor.PushToStack(const W: Word);
begin
  Dec(FRegSP);
  WriteMem(FRegSP, TRec16(W).UByteHi);
  Dec(FRegSP);
  WriteMem(FRegSP, TRec16(W).UByteLo);
end;

function TProcessor.SubOrSubc(const B: Int16Fast; var Flags: Int16Fast
  ): Int16Fast;
var
  A1: Int16Fast;
begin
  A1 := RegA;

  Result := A1 - B - Flags;

  A1 := ((A1 xor B) xor Result) and $1FF;

  Flags := (A1 shr 8) // C
    or ((((A1 shl 1) xor A1) shr 6) and %100) // PV
    or (A1 and %10000) // H
    or %10 // N
    ;

  Result := Result and $FF;

  if Result = 0 then
    Flags := Flags or %01000000 // Z
  else begin
    // do not set 5, 3 here, leave it to the calling function
    // (this is because CP instruction will set these flags according to the
    //  operand, whereas SUB and SBC will set them according to result)
    Flags := Flags or (Result and %10000000); // S
  end;

end;

procedure TProcessor.ExAF;
var
  Aux: TRec16;
begin
  Aux := FRegAF;
  FRegAF := FRegAF1;
  FRegAF1 := Aux;
end;

procedure TProcessor.SubA(const B: Byte);
var
  Flags: Int16Fast;
begin
  Flags := 0;
  RegA := SubOrSubc(B, Flags);
  RegF := Flags or (RegA and %00101000); // 5, 3
end;

procedure TProcessor.AdcHLss(const W: Word);
var
  Flags: Int16Fast;
  Aux, IWReg, IW: Int32Fast;
begin
  Aux := RegF and 1;

  IWReg := RegHL;

  IW := W;
  // H flag:
  if (((IWReg and Mask12) + (IW and Mask12) + Aux) and Bit12) <> 0 then
    Flags := %00010000 // H
  else
    Flags := 0;

  Aux := Aux + IWReg + W;

  // carry flag:
  if Aux and $10000 <> 0 then
    Flags := Flags or 1; // C

  // PV (overflow in signed addition):
  if ((IWReg xor IW) and $8000 = 0)  // both operands have same sign
     and ((IWReg xor Aux) and $8000 <> 0) // but result has the opposite sign
  then
    Flags := Flags or %100; // PV

  RegHL := Aux and $FFFF;
  if RegHL = 0 then
    Flags := Flags or %01000000; // Z

  // flags S, 5, 3 from high byte of result:
  Flags := Flags or (RegH and %10101000);

  // N is reset (N is 0)
  RegF := Flags;
end;

procedure TProcessor.SbcHLss(const W: Word);
var
  Flags: Int16Fast;
  Aux, IWReg, IW: Int32Fast;
begin
  Aux := RegF and 1;

  IWReg := RegHL;
  IW := W;

  if (IWReg and Mask12) - (IW and Mask12) - Aux < 0 then begin
    Flags := %00010010; // H, N
  end else begin
    Flags := %10; // N
  end;

  Aux := IWReg - IW - Aux;

  // carry flag:
  if Aux < 0 then
    Flags := Flags or 1; // C

  // PV -- overflow -- means overflow of signed operation.
  // it's set iff:
  if ((IWReg xor IW) and $8000 <> 0) // operands have different signs
     and ((IWReg xor Aux) and $8000 <> 0) // result has the opposite sign from what was in Reg in the first place
  then
    Flags := Flags or %100; // PV

  RegHL := Aux and $FFFF;
  if RegHL = 0 then
    RegF := Flags or %01000000 // Z
  else
    // flags S, 5, 3 from hi byte of result:
    RegF := Flags or (RegH and %10101000);

end;

procedure TProcessor.IncR(const PB: PByte);
var
  Flags, B: Int16Fast;
begin
  // NOTE: the difference from ADD r, 1 -- carry is not affected!
  Flags := RegF and 1; // preserve carry

  Inc(PB^);
  B := PB^;

  if B <> 0 then begin
    if B and $0F = 0 then begin
      if B = $80 then
        Flags := Flags or %100; // PV

      Flags := Flags or %00010000; // H
    end;

    Flags := Flags or (B and %10101000); // S, 5, 3
  end else
    Flags := Flags or %01010000; // Z, H

  RegF := Flags;

  FFlagsModified := True;
end;

procedure TProcessor.DecR(const PB: PByte);
var
  Flags, B: Int16Fast;
begin
  // NOTE: the difference from SUB r, 1 -- carry is not affected!
  Flags := RegF and 1; // preserve carry

  Dec(PB^);
  B := PB^;

  if B = 0 then
    Flags := Flags or %01000010 // Z, N
  else begin
    if B and $0F = $0F then begin
      if B = $7F then
        Flags := Flags or %100; // PV

      Flags:= Flags or %00010000; // H
    end;

    Flags := Flags or (B and %10101000) or %10; // S, 5, 3, N
  end;

  RegF := Flags;

  FFlagsModified := True;
end;

procedure TProcessor.RLCA;
begin
  RegA := Byte((RegA shl 1) or (RegA shr 7));

  RegF :=
    (RegF and %11000100) // S, Z, PV not affected
    or (RegA and %00101001); // 5, 3, C
  // H, N reset
end;

procedure TProcessor.RRCA;
begin
  RegF := (RegA and 1) // C
    or (RegF and %11000100) // S, Z, PV not affected
    ;
  RegA := Byte((RegA shr 1) or (RegA shl 7));
  RegF := RegF or (RegA and %00101000);  // 5, 3
  // H, N reset
end;

procedure TProcessor.RLA;
var
  FlagC: Byte;
begin
  FlagC := RegA shr 7;
  RegA := Byte(RegA shl 1) or (RegF and 1);
  
  RegF := FlagC // C
    or (RegF and %11000100) // S, Z, PV not affected
    or (RegA and %00101000); // 5, 3
  // H, N reset
end;

procedure TProcessor.RRA;
var
  FlagC: Byte;
begin
  FlagC := RegA and 1;
  RegA := Byte(RegA shr 1) or Byte(RegF shl 7);

  RegF := FlagC // C
    or (RegF and %11000100) // S, Z, PV not affected
    or (RegA and %00101000); // 5, 3
  // H, N reset
end;

procedure TProcessor.NotAccumulator;
begin
  RegA := not RegA;

  // H, N are set, 5, 3 copied from accumulator, others unaffected.
  RegF := %00010010 // H, N
    or (RegA and %00101000) // 5, 3
    or (RegF and %11000101) // S, Z, PV, C not affected
    ;
end;

{
Flags 5, 3 in SCF and CCF instructions:
- if the previous instruction modified flags, then flags 5 and 3 are copied from accumulator.
- if the previous instruction did not modify flags, flags 5 and 3 are OR-ed with accumulator.
Discovered by Patrik Rak - described here: https://worldofspectrum.org/forums/discussion/comment/669314#Comment_669314
See also: https://sinclair.wiki.zxnet.co.uk/wiki/Z80#SCF_and_CCF
}
procedure TProcessor.SCF;
var
  B: Byte;
begin
  if FFlagsModified then
    B := RegA
  else
    B := RegF or RegA;

  // C set, H, N reset, 5, 3 as noted above, others unaffected.
  RegF :=
    1 // C
    or (B and %00101000) // 5, 3
    or (RegF and %11000100) // S, Z, PV unaffected
     // H, N reset
    ;
end;

{ CCF -- see the comment above SCF }
procedure TProcessor.CCF;
var
  B: Byte;
begin
  if FFlagsModified then
    B := RegA
  else
    B := RegF or RegA;

  // Flags 5, 3 - see note above SCF
  // Flag C inverted, H is what was C; 5, 3 note above SCF, N is reset:
  RegF :=
      ((not RegF) and 1) // C
      or Byte((RegF and 1) shl 4) // H
      or (B and %00101000) // 5, 3
      or (RegF and %11000100) // S, Z, PV unaffected
     // N reset
    ;
end;

procedure TProcessor.DAA;
var
  Aux, IRegA, Flags: Int16Fast;
begin
// https://www.worldofspectrum.org/faq/reference/z80reference.htm#DAA

  IRegA := RegA;
  Flags := RegF and %10; // N flag will be preserved.

  if (RegA > $99) or (RegF and 1 <> 0) then begin
    Aux := $60;
    Flags := Flags or 1; // C
  end else begin
    Aux := 0;
  end;

  if (IRegA and $0F > 9) or (RegF and %00010000 <> 0) then
    Aux := Aux or $06;

  if Flags and %10 = 0 then // N not set
    Aux := IRegA + Aux
  else
    Aux := IRegA - Aux;

  Flags := Flags or ((IRegA xor Aux) and %00010000); // H

  RegA := Byte(Aux);

  if RegA = 0 then
    RegF := Flags or %01000100 // Z, PV
  else begin
    if IsByteParityEven(RegA) then
      Flags := Flags or %100; // PV

    // flags S, 5, 3 copied from A
    RegF := Flags or (Aux and %10101000); // S, 5, 3
  end;

end;

procedure TProcessor.Exx;
var
  Aux: TGPRegs;
begin
  Aux := GPRegs;
  GPRegs := GPRegs1;
  GPRegs1 := Aux;
end;

procedure TProcessor.CALL(const DoCall: Boolean);
begin
  FRegWZ := ReadNextWord;
  if DoCall then begin
    ContentionAndIncTStates;

    PushToStack(FRegPC);
    FRegPC := FRegWZ;
  end;
end;

{ BIT instruction -- only affects flags }
procedure TProcessor.TestBit(const BitNo, B: Byte);
var
  FlagsToSet: Byte;
begin
  //  S   Z   F5   H   F3   PV   N   C
  // *Z513*0-  PV as Z, S set only if n=7 and b7 of r set, C not affected
  if B and (1 shl BitNo) = 0 then
    FlagsToSet := %01010100 // Z, H, PV
  else if BitNo = 7 then
    FlagsToSet := %10010000 // S, H
  else
    FlagsToSet := %00010000; // H

  RegF := FlagsToSet
    or (RegF and 1);

  // Leave flags 5 and 3 to be set by caller, here, memptr (WZ register) might affect these flags!

  FFlagsModified := True;
end;

procedure TProcessor.SetBit(const BitNo: Byte; var B: Byte);
begin
  B := B or (1 shl BitNo);
  FFlagsModified := False;
end;

procedure TProcessor.ResetBit(const BitNo: Byte; var B: Byte);
begin
  B := B and (not (1 shl BitNo));
  FFlagsModified := False;
end;

procedure TProcessor.RldOrRrd(const IsRRD: Boolean);
var
  Aux: Byte;
begin
  Aux := ReadMem(RegHL);
  FRegWZ := RegHL + 1;

  Contention2Times;
  Contention2Times;

  if IsRRD then begin
    WriteMem(RegHL, Byte(Aux shr 4) or Byte(RegA shl 4));
    RegA := (RegA and $F0) or (Aux and $0F);
  end else begin
    WriteMem(RegHL, Byte(Aux shl 4) or (RegA and Byte($0F)));
    RegA := (RegA and $F0) or Byte(Aux shr 4);
  end;

  RegF := RegF and 1; // preserve carry

  if RegA = 0 then
    RegF := RegF or %01000100 // Z, PV
  else begin
    // flags S, 5, 3 copied from A
    RegF := RegF or (RegA and %10101000); // S, 5, 3
    if IsByteParityEven(RegA) then
      RegF := RegF or %100; // PV
  end;

  FFlagsModified := True;
end;

procedure TProcessor.InAn;
var
  B: Byte;
begin
  B := ReadNextByte;
  TRec16(FAddressBus).UByteLo := B;
  TRec16(FAddressBus).UByteHi := RegA;
  FRegWZ := FAddressBus + 1;
  // request io read...
  RequestInput;
  // now one byte from the selected port is placed on data bus...
  //
  RegA := FDataBus;
end;

procedure TProcessor.InC0;
var
  FlagsToSet: Int16Fast;
begin
  FAddressBus := RegBC;
  FRegWZ := FAddressBus + 1;

  // request io read...
  RequestInput;

  FlagsToSet := RegF and 1; // preserve carry

  if FDataBus = 0 then
    RegF := FlagsToSet or %01000100 // Z, P
  else begin
    if IsByteParityEven(FDataBus) then
      FlagsToSet := FlagsToSet or %100; // P
    // flags S, 5, 3 copied from DataBus
    RegF := FlagsToSet or (FDataBus and %10101000); // S, 5, 3
  end;
end;

procedure TProcessor.OUTnA;

begin                                               
  TRec16(FAddressBus).UByteLo := ReadNextByte;
  TRec16(FAddressBus).UByteHi := RegA;
  FDataBus := RegA;
  FRegWZ := FAddressBus + 1;
  TRec16(FRegWZ).UByteHi := FDataBus;

  RequestOutput;
end;

procedure TProcessor.NMI;
begin
  FNMI := True;
end;

procedure TProcessor.SetOnInputRequest(AValue: TProcessorEvent);
begin
  if AValue = nil then
    FOnInputRequest := @EmptyProcessorEvent
  else
    FOnInputRequest := AValue;
end;

procedure TProcessor.SetOnNeedWriteScreen(AValue: TWriteScrEvent);
begin
  if AValue = nil then
    FOnNeedWriteScreen := @EmptyWriteScrEvent
  else
    FOnNeedWriteScreen := AValue;
end;

procedure TProcessor.SetOnOutputRequest(AValue: TProcessorEvent);
begin
  if AValue = nil then
    FOnOutputRequest := @EmptyProcessorEvent
  else
    FOnOutputRequest := AValue;
end;

procedure TProcessor.Alu(const I: Integer; const B: Byte);

  // ADD A, s
  // ADC A, s
  procedure AddOrAdcAs(); inline;
  var
    A1, Aux: Int16Fast;
    B1: Int16Fast;
  begin
    B1 := B;
    A1 := RegA;
    Aux := A1 + B1 + RegF and 1;
    RegA := Aux and $FF;

    A1 := (A1 xor B1) xor Aux;

    A1 := (A1 shr 8) // C
      or (((A1 shr 6) xor (A1 shr 5)) and %100) // PV
      or (A1 and %10000) // H
      ;

    if RegA = 0 then
      A1 := A1 or %01000000 // Z
    else
      // flags S, 5, 3 copied from A
      A1 := A1 or (Aux and %10101000); // S, 5, 3

    RegF := A1 and $FF
    ;
  end;

  // SBC A, s
  procedure SbcA(); inline;
  var
    Flags: Int16Fast;
  begin
    Flags := RegF and 1;
    RegA := SubOrSubc(B, Flags);
    RegF := Flags or (RegA and %00101000); // 5, 3
  end;

  // CP s
  procedure CP(); inline;
  var
    Flags: Int16Fast;
  begin
    Flags := 0;
    SubOrSubc(B, Flags);
    // according to http://www.z80.info/z80sflag.htm F3, F5 are copied from operand, not result!
    RegF := Flags or (B and %00101000); // 5, 3
  end;

  procedure SetAndOrXorFlags(); inline;
  begin
    if RegA = 0 then
      RegF := %01000100 // Z, PV
    else begin
    // flags S, 5, 3 copied from RegA:
      RegF := RegA and %10101000;

      if IsByteParityEven(RegA) then
        RegF := RegF or %100; // PV
    end;
  end;

begin
  case I of
    0: // ADD A, s
      begin
        RegF := 0;
        AddOrAdcAs();
      end;
    1: // ADC A, s
      AddOrAdcAs();
    2: // SUB A, s
      SubA(B);
    3: // SBC A, s
      SbcA();
    4: // AND s
      begin
        RegA := RegA and B;
        SetAndOrXorFlags();
        RegF := RegF or %10000; // H
      end;
    5: // XOR s
      begin
        RegA := RegA xor B;
        SetAndOrXorFlags();
      end;
    6: // OR s
      begin
        RegA := RegA or B;
        SetAndOrXorFlags();
      end;
    7: // CP s
      CP();
  otherwise
  end;

  FFlagsModified := True;
end;

procedure TProcessor.Rot(const I: Integer; var B: Byte);
var
  FlagsToSet: UInt16Fast;
begin
  //RLC RRC RL RR SLA SRA SLL SRL
  case I of
    0:
      begin //RLC(B);
        FlagsToSet := B shr 7;
        B := Byte((B shl 1) or FlagsToSet);
      end;
    1: //RRC(B);
      begin
        FlagsToSet := B and 1;
        B := Byte((B shr 1) or (FlagsToSet shl 7));
      end;
    2: //RL(B);
      begin
        FlagsToSet := B shr 7;
        B := Byte(Byte(B shl 1) or (RegF and 1));
      end;
    3: //RR(B);
      begin
        FlagsToSet := B and 1;
        B := Byte((B shr 1) or (RegF shl 7));
      end;
    4: //SLA(B);
      begin
        FlagsToSet := B shr 7;
        B := Byte(B shl 1);
      end;
    5: //SRA(B);
      begin
        FlagsToSet := B and 1;
        B := Byte(B shr 1) or (B and %10000000);
      end;
    6: //SLL(B);
      begin
        FlagsToSet := Byte(B shr 7);
        B := Byte(B shl 1) or 1;
      end;
    7: //SRL(B);
      begin
        FlagsToSet := B and 1;
        B := B shr 1;
      end;
  otherwise
    Assert(False);
    FlagsToSet := 0;
  end;

  if B = 0 then
    FlagsToSet := FlagsToSet or %01000100 // Z, P
  else begin
    if IsByteParityEven(B) then
      FlagsToSet := FlagsToSet or %100;
    // flags S, 5, 3 copied from A
    FlagsToSet := FlagsToSet or (B and %10101000); // S, 5, 3
  end;

  RegF := FlagsToSet and $FF;

  FFlagsModified := True;
end;

procedure TProcessor.Bli(const A, B: Byte);
 
  { decrease PC if (already decreased) BC is not zero -- the same instruction
    will be executed again, until BC becomes zero. }
  procedure DecreasePC();
  begin
    Dec(FRegPC, 2);
    Contention5Times;
  end;

var
  FlagsToSet: Int16Fast;
  K: Int16Fast;
  By, B2: Byte;

begin
  // block instructions
  case B of
    0: // LDI, LDD, LDIR, LDDR
      begin
        // Transfer (HL) to (DE):
        By := ReadMem(RegHL);
        WriteMem(RegDE, By);
        Contention2Times;

        FlagsToSet := (RegF and %11000001); // S, Z, C not affected

        Dec(GPRegs.BC.U16bit);

        if A and 1 = 0 then begin
          // LDI, LDIR
          Inc(GPRegs.DE.U16bit);
          Inc(GPRegs.HL.U16bit);
        end else begin
          // LDD, LDDR
          Dec(GPRegs.DE.U16bit);
          Dec(GPRegs.HL.U16bit);
        end;

        if (A and 2 <> 0) and (RegBC <> 0) then begin
          FRegWZ := FRegPC - 1;
          DecreasePC();
          FlagsToSet := FlagsToSet or (TRec16(FRegPC).UByteHi and %00101000) or %100;
        end else begin
          By := By + RegA; // (transferred byte + A) -- affects F3 and F5
          FlagsToSet := FlagsToSet                                    
            or Byte((By and %10) shl 4) //F5 is bit 1 of (transferred byte + A)
            or (By and %1000) //F3 is bit 3 of (transferred byte + A)
            ;

          if RegBC <> 0 then
            FlagsToSet := FlagsToSet or %100; // PV
        end;
      end;

    1: // CPI, CPD, CPIR, CPDR
      begin
        FlagsToSet := RegF and 1; // C is not affected

        By := ReadMem(RegHL);
        Contention5Times;
        Dec(GPRegs.BC.U16bit);

        if By <> RegA then begin
          if (By and $0F) > (RegA and $0F) then
            FlagsToSet := FlagsToSet or %10000; // H

          By := RegA - By;
          if (A and 2 <> 0) and (RegBC <> 0) then begin
            if A and 1 = 0 then begin
              DecreasePC();
              FRegWZ := FRegPC;
            end else begin
              FRegWZ := FRegPC;
              DecreasePC();
            end;
            FlagsToSet := FlagsToSet or (TRec16(FRegPC).UByteHi and %00101000);
          end else begin
            { B2 = A - (HL) - H, H as in F after instruction,
              then F3 is bit 3 of B2 and F5 is bit 1 of B2 }
            B2 := By - (FlagsToSet shr 4);
            FlagsToSet := FlagsToSet
                 or Byte((B2 and %10) shl 4) or (B2 and %1000); // 5, 3
          end;
          FlagsToSet := FlagsToSet or (By and %10000000) or %10; // S, N;
        end else
          FlagsToSet := FlagsToSet or %01000010; // Z, N

        if RegBC <> 0 then
          FlagsToSet := FlagsToSet or %100; // PV

        if A and 1 = 0 then begin
          Inc(GPRegs.HL.U16bit);
          Inc(FRegWZ);
        end else begin
          Dec(GPRegs.HL.U16bit);
          Dec(FRegWZ);
        end;

      end;

  otherwise
    ContentionAndIncTStates;
    if B = 2 then begin
     // INI, IND, INIR, INDR
      
      FAddressBus := RegBC;
      RequestInput;

      WriteMem(RegHL, FDataBus);

      if A and 1 = 0 then begin
        FRegWZ := RegBC + 1;
        Inc(GPRegs.HL.U16bit);
      end else begin
        FRegWZ := RegBC - 1;
        Dec(GPRegs.HL.U16bit);
      end;
      Dec(GPRegs.BC.UByteHi);

      K := TRec16(FRegWZ).UByteLo;
    end else begin
      // B = 3
      // OUTI, OUTD, OTIR, OTDR
      FDataBus := ReadMem(RegHL);
      Dec(GPRegs.BC.UByteHi);
      FAddressBus := RegBC;
      RequestOutput;

      if A and 1 = 0 then begin
        FRegWZ := RegBC + 1;
        Inc(GPRegs.HL.U16bit);
      end else begin
        FRegWZ := RegBC - 1;
        Dec(GPRegs.HL.U16bit);
      end;
      //
      K := RegL;
    end;
    //
    FlagsToSet := (FDataBus shr 6) and %10; // N

    if RegB = 0 then begin
      FlagsToSet := FlagsToSet or %01000000; // Z
    end else
      FlagsToSet := FlagsToSet or (RegB and %10101000); // S, 5, 3

    K := FDataBus + K;
    if K and $FF00 <> 0 then
      FlagsToSet := FlagsToSet or %00010001; // H, C

    if IsByteParityEven(Byte(K and 7) xor RegB) then
      FlagsToSet := FlagsToSet or %100; // P

    if (A and 2 <> 0) and (RegB <> 0) then begin
      DecreasePC();
      FlagsToSet := (FlagsToSet and %11010111) or (TRec16(FRegPC).UByteHi and %00101000);
      if FlagsToSet and 1 = 0 then
        By := RegB
      else if FlagsToSet and %10 <> 0 then begin
        if RegB and $F <> 0 then
          FlagsToSet := FlagsToSet and %11101111;
        By := RegB - 1;
      end else begin
        if RegB and $F <> $F then
          FlagsToSet := FlagsToSet and %11101111;
        By := RegB + 1;
      end;

      if not IsByteParityEven(By and 7) then
        FlagsToSet := FlagsToSet xor %100;
    end;

  end;                    

  RegF := FlagsToSet;

  FFlagsModified := True;
end;

procedure TProcessor.RefreshMem;
begin
  RegR := ((RegR + 1) and %01111111) or (RegR and %10000000);
end;

procedure TProcessor.ResetPin();
begin
  FInterruptMode := 0;
  FPrefixByte := 0;
  FIff1 := False;
  FIff2 := False;
  FRegPC := 0;
  FRegIR.U16bit := 0;

  FHalt := False;
  FIntPin:= False;
  FNMI := False;

  // All pins are set to inactive state,
  // but other pins are not emulated
end;

constructor TProcessor.Create;
begin
  inherited Create;

  FOnInputRequest := @EmptyProcessorEvent;
  FOnOutputRequest := @EmptyProcessorEvent;
  FOnNeedWriteScreen := @EmptyWriteScrEvent;
  InitRegPointers;
  ContendedHighBank := False;
  ResetCPU;
end;

procedure TProcessor.ResetCPU;
begin
  ResetPin();

  FFlagsModified := False;

  FTStatesInCurrentFrame := 0;

  //FillChar(GPRegs, SizeOf(GPRegs), 0); //??
  //FillChar(GPRegs1, SizeOf(GPRegs1), 0); //??
  FillChar(GPRegs, SizeOf(GPRegs), $FF);
  FillChar(GPRegs1, SizeOf(GPRegs1), $FF);

  FRegWZ := $FFFF;

  FRegSP := $FFFF;
  //FRegSP := 0;
  FRegAF.U16bit := $FFFF;
  FRegAF1.U16bit := $FFFF;
  //FRegAF.U16bit := 0;
  //FRegAF1.U16bit := 0;

  FRegIx.U16bit := $FFFF;
  FRegIy.U16bit := $FFFF;
  //FRegIx.U16bit := 0;
  //FRegIy.U16bit := 0;

  { TODO : zero or ff?: }
  FDataBus := 0; // ff?
  FAddressBus := 0; // ffff?
end;

procedure TProcessor.DoProcess;

var
  x, y, z: Byte;
  Pref: Byte;
  OpCode: Byte;

  function IfCc(const CC: Byte): Boolean;
  begin
    case CC of
      0:
        Result := RegF and %01000000 = 0;  // NZ
      1:
        Result := RegF and %01000000 <> 0; // Z
      2:
        Result := RegF and 1 = 0;          // NC
      3:
        Result := RegF and 1 <> 0;         // C
      4:
        Result := RegF and %100 = 0;       // PO
      5:
        Result := RegF and %100 <> 0;      // PE
      6:
        Result := RegF and %10000000 = 0;  // P
    otherwise // 7:
      Result := RegF and %10000000 <> 0; // M
    end;
  end;

  function ResolveHL: PWord; inline;
  begin
    case Pref of
      $DD:
        Result := PRegIx;
      $FD:
        Result := PRegIy;
    otherwise
      Result := PRegHL;
    end;
  end;

  function ResolveH: PByte; inline;
  begin
    case Pref of
      $DD:
        Result := PRegIxH;
      $FD:
        Result := PRegIyH;
    otherwise
      Result := PRegH;
    end;
  end;

  function ResolveL: PByte; inline;
  begin
    case Pref of
      $DD:
        Result := PRegIxL;
      $FD:
        Result := PRegIyL;
    otherwise
      Result := PRegL;
    end;
  end;

  function ResolveRegSimple(const B: Byte): PByte;
  begin
    case B of
      0:
        Result := PRegB;
      1:
        Result := PRegC;
      2:
        Result := PRegD;
      3:
        Result := PRegE;
      7:
        Result := PRegA;
      4:
        Result := PRegH;
      5:
        Result := PRegL;
    otherwise // 6:
      Result := nil; // Must never happen!
    end;
  end;

  function ResolveTableR(const B: Byte): PByte;
  begin
    case B of
      0:
        Result := PRegB;
      1:
        Result := PRegC;
      2:
        Result := PRegD;
      3:
        Result := PRegE;
      7:
        Result := PRegA;
      4:
        Result := ResolveH;
      5:
        Result := ResolveL;
    otherwise // 6:
      Result := nil; // Must never happen!
    end;
  end;

  function ResolveTableRP(): PWord; inline;
  begin
    case y shr 1 of
      0:
        Result := PRegBC;
      1:
        Result := PRegDE;
      2:
        Result := ResolveHL;
    otherwise
    //  3:
      if x <> 3 then
        Result := PRegSP
      else
        Result := PRegAF;
    end;
  end;

  function ReadMem16(const Adr: Word): Word;
  var
    ResultAsRec16: TRec16 absolute Result;
  begin
    ResultAsRec16.UByteLo := ReadMem(Adr);
    ResultAsRec16.UByteHi := ReadMem(Adr + 1);
  end;

  procedure WriteMem16(const Adr: Word; const W: Word);
  var
    WAsRec16: TRec16 absolute W;
  begin
    WriteMem(Adr, WAsRec16.UByteLo);
    WriteMem(Adr + 1, WAsRec16.UByteHi);;
  end;

  function GetMemAddressWithDisplacement: Word;
  var
    B: Byte;
    D: Int8 absolute B;
    ResultAsInt16: Int16 absolute Result;
  begin
    B := ReadNextByte;

    if Pref = $DD then
      Result := Ix
    else
      Result := Iy;

    ResultAsInt16 := ResultAsInt16 + D;
    FRegWZ := Result;
  end;

  procedure Add16(const W: Word);
  var
    Flags: Int16Fast;
    Aux: Int32Fast;
    PWReg: PWord;
  begin
    PWReg := ResolveHL;
    FRegWZ := PWReg^ + 1;

    Aux := PWReg^;
    Aux := Aux + W;

    Flags := (RegF and %11000100) // S, Z, PV not affected
      or ((Aux shr 16) and 1) // C
      or ((((PWReg^ and Mask12) + (W and Mask12)) shr 8) and $10) // H
    ;

    PWReg^ := Aux and $FFFF;

    // flags 5, 3 from hi byte of result:
    Flags := Flags or (TRec16(PWReg^).UByteHi and %00101000);

    // N is reset
    RegF := Flags;

    FFlagsModified := True;
  end;

  procedure IncDecHL(const Fun: TIncDecProc);
  var
    Adr: Word;
    B: Byte;
  begin
    case Pref of
      $DD, $FD:
        begin
          Adr := GetMemAddressWithDisplacement;

          Contention5Times;
        end;
    otherwise
      Adr := RegHL;
    end;

    B := ReadMem(Adr);

    ContentionAndIncTStates;

    Fun(@B);
    WriteMem(Adr, B);
  end;

  procedure LDmHLn;
  var
    Adr: Word;
    B: Byte;
  begin
    case Pref of
      $DD, $FD:
        begin
          Adr := GetMemAddressWithDisplacement;
          B := ReadNextByte;
          Contention2Times;
          WriteMem(Adr, B);
        end;
    otherwise
      WriteMem(RegHL, ReadNextByte);
    end;
    //
  end;

  procedure RelativeJump(const Condition: Boolean);
  var
    D: Int8;
    B: Byte absolute D;
  begin
    B := ReadNextByte;
    if Condition then begin
      Contention5Times;

      FRegPC := FRegPC + D;
      FRegWZ := FRegPC;
    end;
  end;

  procedure ProcessX0; inline;
  var
    Adr: Word;
  begin
    case z of
      0:
        begin
          case y of
            0: // NOP
              ;
            1: // EX AF, AF'
              ExAF;
            2: // DJNZ d
              begin
                ContentionAndIncTStates;
                Dec(GPRegs.BC.UByteHi);
                RelativeJump(RegB <> 0);
              end;
            3: // JR d
              RelativeJump(True);
          otherwise // 4..7 // JR cc[y-4], d
            RelativeJump(IfCc(y and %11));
          end;
          FFlagsModified := False;
        end;
      1:
        case y and 1 of
          0: // LD rp[p], nn
            begin
              ResolveTableRP()^ := ReadNextWord;
              FFlagsModified := False;
            end;
        otherwise //1:
          // ADD HL, rp[p]
          Contention2Times;
          Contention5Times;
          Add16(ResolveTableRP()^);
        end;
      2:
        begin
          case y of
            0: // LD (BC), A
              begin
                WriteMem(RegBC, RegA);
                RegWZ := RegBC + 1;
                TRec16(FRegWZ).UByteHi := RegA;
              end;
            1: // LD A, (BC)
              begin
                RegA := ReadMem(RegBC);
                RegWZ := RegBC + 1;
              end;
            2: // LD (DE), A
              begin                 
                WriteMem(RegDE, RegA);
                RegWZ := RegDE + 1;
                TRec16(FRegWZ).UByteHi := RegA;
              end;
            3: // LD A, (DE)
              begin
                RegA := ReadMem(RegDE);
                RegWZ := RegDE + 1;
              end;
            4: // LD (nn), HL
              begin
                FRegWZ := ReadNextWord;
                WriteMem16(FRegWZ, ResolveHL^);
                Inc(FRegWZ);
              end;
            5: // LD HL, (nn)
              begin
                Adr := ReadNextWord;
                FRegWZ := Adr + 1;
                ResolveHL^ := ReadMem16(Adr);
              end;
            6: // LD (nn), A
              begin
                Adr := ReadNextWord;
                WriteMem(Adr, RegA);
                FRegWZ := Adr + 1;
                TRec16(FRegWZ).UByteHi := RegA;
              end;
          otherwise // 7:
            // LD A, (nn)
            Adr := ReadNextWord;
            RegA := ReadMem(Adr);
            FRegWZ := Adr + 1;
          end;
          FFlagsModified := False;
        end;
      3:
        begin
          Contention2Times;
          case y and 1 of
            0: // INC rp[p]
              Inc(ResolveTableRP()^);
            1: // DEC rp[p]
              Dec(ResolveTableRP()^);
          end;
          FFlagsModified := False;
        end;
      4: // INC r[y]
        if y <> 6 then
          IncR(ResolveTableR(y))
        else
          IncDecHL(@IncR);
      5: // DEC r[y]
        if y <> 6 then
          DecR(ResolveTableR(y))
        else
          IncDecHL(@DecR);
      6: // LD r[y], n
        begin
          if y <> 6 then
            ResolveTableR(y)^ := ReadNextByte
          else
            LDmHLn;

          FFlagsModified := False;
        end;
    otherwise // 7:
      case y of
        0: // RLCA
          RLCA;
        1: // RRCA
          RRCA;
        2: // RLA
          RLA;
        3: // RRA
          RRA;
        4: // DAA
          DAA;
        5: // CPL
          NotAccumulator;
        6: // SCF
          SCF;
      otherwise // 7:
        // CCF
        CCF;
      end;

      FFlagsModified := True;
    end;
  end;

  procedure ProcessX1; inline;
  var
    Adr: Word;
  begin
    if z <> 6 then begin
      // LD r[y], r[z]
      if y <> 6 then
        ResolveTableR(y)^ := ResolveTableR(z)^
      else begin
        case Pref of
          $DD, $FD:
            begin
              Adr := GetMemAddressWithDisplacement;

              Contention5Times;
              WriteMem(Adr, ResolveRegSimple(z)^);
            end;
        otherwise
          WriteMem(RegHL, ResolveRegSimple(z)^);
        end;
      end;

    end else begin
      if y <> 6 then begin
        case Pref of
          $DD, $FD:
            begin
              Adr := GetMemAddressWithDisplacement;

              Contention5Times;
              ResolveRegSimple(y)^ := ReadMem(Adr);
            end;
        otherwise
          ResolveRegSimple(y)^ := ReadMem(RegHL);
        end;

      end else begin
        FHalt := True;
        Dec(FRegPC);
      end;
    end;
    FFlagsModified := False;
  end;

  procedure ProcessX2; inline;
  var
    Adr: Word;
  begin
    if z <> 6 then
      Alu(y, ResolveTableR(z)^) // alu[y] r[z]
    else begin
      case Pref of
        $DD, $FD:
          begin
            Adr := GetMemAddressWithDisplacement;

            Contention5Times;
          end
      otherwise
        Adr := RegHL;
      end;
      Alu(y, ReadMem(Adr));
    end;
  end;

  procedure ProcessX3; inline;
  var
    W: Word;
    PW: PWord;
  begin
    FFlagsModified := False;
    case z of
      0: // RET cc[y]
        begin
          ContentionAndIncTStates;
          if IfCc(y) then begin
            FRegPC := PopFromStack;
            FRegWZ := FRegPC;
          end;
        end;
      1:
        if y and 1 = 0 then begin
          // POP rp2[p]
          ResolveTableRP()^ := PopFromStack;
        end else
          case y shr 1 of
            0: // RET
              begin
                FRegPC := PopFromStack;
                FRegWZ := FRegPC;
              end;
            1: // EXX
              Exx;
            2: // JP HL
              FRegPC := ResolveHL^;
          otherwise // 3: // LD SP, HL
            Contention2Times;
            FRegSP := ResolveHL^;
          end;

      2: // JP cc[y], nn
        begin
          FRegWZ := ReadNextWord;
          if IfCc(y) then
            FRegPC := FRegWZ;
        end;
      3:
        case y of
          0: // JP nn
            begin
              FRegPC := ReadNextWord;
              FRegWZ := FRegPC;
            end;
          1: // (CB prefix)
            FPrefixByte := $CB;
          2: // OUT (n), A
            OUTnA;
          3: // IN A, (n)
            InAn;
          4: // EX (SP), HL
            begin                                 
              PW := ResolveHL;
              TRec16(W).UByteLo := ReadMem(FRegSP);
              Inc(FRegSP);
              TRec16(W).UByteHi := ReadMem(FRegSP);
              ContentionAndIncTStates;
              WriteMem(FRegSP, TRec16(PW^).UByteHi);
              Dec(FRegSP);
              WriteMem(FRegSP, TRec16(PW^).UByteLo);

              Contention2Times;
              PW^ := W;
              FRegWZ := W;
            end;
          5: // EX DE, HL
            begin
              W := RegDE;
              // THIS ONE IS UNAFFECTED BY PREFIX (HL is always HL)
              RegDE := RegHL;
              RegHL := W;
            end;
          6: // DI
            begin
              FIff1 := False;
              FIff2 := False;
            end;

        otherwise // 7:
          // EI
          FPrefixByte := $FB; // opcode of EI
                 // one more instruction will pass before the interrupt check
          FIff1 := True;
          FIff2 := True;
        end;
      4: // CALL cc[y], nn
        CALL(IfCc(y));
      5:
        case y and 1 of
          0:
            begin
              // PUSH rp2[p]
              ContentionAndIncTStates;
              PushToStack(ResolveTableRP()^);
            end;
        otherwise
          case y shr 1 of
            0:
              // CALL nn
              CALL(True);
          otherwise
            // for p in [1, 2, 3], we have DD, ED or FD prefixes:
            FPrefixByte := OpCode;
          end;
        end;
      6: // alu[y] n
        Alu(y, ReadNextByte);

    otherwise // 7:
      // RST y*8
      ContentionAndIncTStates;
      PushToStack(FRegPC);
      FRegPC := y shl 3;
      FRegWZ := FRegPC;
    end;
  end;

  procedure ProcessCB;
  var
    Adr: Word;
    B: Byte;
  begin
    //
//BIT b,(ii+n)   pc:4,pc+1:4,pc+2:3,pc+3:3,pc+3:1 ×2,ii+n:3,ii+n:1
//SET b,(ii+n)   pc:4,pc+1:4,pc+2:3,pc+3:3,pc+3:1 x2,ii+n:3,ii+n:1,ii+n(write):3
//BIT b,(HL)     pc:4,pc+1:4,hl:3,hl:1

    if Pref = $CB then begin
      Adr := RegHL;
    end else begin
      // $DD, $FD
      Adr := GetMemAddressWithDisplacement;

      B := ReadNextByte;
      Contention2Times;
      x := B shr 6;
      y := (B shr 3) and %111;
      z := B and %111;
    end;

    B := ReadMem(Adr);

    ContentionAndIncTStates;

    case x of
      0: // rot[y] r[z]
        Rot(y, B);
      1: // BIT y, r[z]
        begin
          TestBit(y, B);
          // Here, memptr (WZ register) affects flags!
          RegF := RegF or (TRec16(FRegWZ).UByteHi and %00101000);
          Exit;
        end;
      2: // RES y, r[z]
        ResetBit(y, B);
    otherwise // 3:
      // SET y, r[z]
      SetBit(y, B);
    end;

    WriteMem(Adr, B);
    if z <> 6 then
      ResolveRegSimple(z)^ := B;
  end;

  // LD A, R or LD A, I:
  procedure LDAri(const B: Byte);
  var
    FlagsToSet: Int16Fast;
  begin
    ContentionAndIncTStates;
    RegA := B;

    FlagsToSet := RegF and 1; // carry flag not affected

    // PV set according to value of Iff2
    if FIff2 then begin
      // BUT, Zilog 80 has a bug when interrupt is accepted at this point
      // https://worldofspectrum.org/faq/reference/z80reference.htm#Interrupts
      // https://worldofspectrum.org/forums/discussion/4971/
      // hence this following condition (are we not about to enter interrupt?)
      if not (FIff1 and (FIntPin or (FTStatesInCurrentFrame >= FrameTicks))) then
        FlagsToSet := FlagsToSet or %100;
    end;

    if B = 0 then
      RegF := FlagsToSet or %01000000 // Z
    else
      // flags S, 5, 3 copied from A
      RegF := FlagsToSet or (B and %10101000); // S, 5, 3

    FFlagsModified := True;
  end;

  procedure ProcessED;
  var
    B: Byte;
  begin
    case x of
      1:
        case z of
          0: // IN (C); IN r[y], (C)
            begin
              InC0;
              if y <> 6 then
                ResolveTableR(y)^ := FDataBus;
              FFlagsModified := True;
            end;
          1:
            begin
              if y = 6 then // OUT (C), 0
                FDataBus := 0
              else // OUT (C), r[y]
                FDataBus := ResolveTableR(y)^;

              FAddressBus := RegBC;
              FRegWZ := FAddressBus + 1;
              RequestOutput;
              FFlagsModified := False;
            end;
          2:
            begin
              Contention5Times;
              Contention2Times;
              FRegWZ := RegHL + 1;
              case y and 1 of
                0: // SBC HL, rp[p]
                  SbcHLss(ResolveTableRP()^);
              otherwise // 1:
                // ADC HL, rp[p]
                AdcHLss(ResolveTableRP()^);
              end;
              FFlagsModified := True;
            end;
          3:
            begin
              FRegWZ := ReadNextWord;
              case y and 1 of
                0: // LD (nn), rp[p]
                  WriteMem16(FRegWZ, ResolveTableRP()^);
              otherwise // 1:
                // LD rp[p], (nn)
                ResolveTableRP()^ := ReadMem16(FRegWZ);
              end;
              Inc(FRegWZ);
              FFlagsModified := False;
            end;
          4: // NEG
            begin
              B := RegA;
              RegA := 0;
              SubA(B);
              FFlagsModified := True;
            end;
          5:
            begin
              // y = 1 -- RETI
              // otherwise -- RETN
              // but functionally identical, same code for both:
              FRegPC := PopFromStack;
              FRegWZ := FRegPC;
              FIff1 := FIff2;
              FFlagsModified := False;
            end;
          6: // IM im[y]
            begin
              case y of
                2, 6:
                  FInterruptMode := 1;
                3, 7:
                  FInterruptMode := 2;
              otherwise
                FInterruptMode := 0;
              end;
              FFlagsModified := False;
            end;
        otherwise // 7:
          case y of
            0: // LD I, A
              begin
                ContentionAndIncTStates;
                RegI := RegA;
                FFlagsModified := False;
              end;
            1: // LD R, A
              begin
                ContentionAndIncTStates;
                RegR := RegA;
                FFlagsModified := False;
              end;
            2: // LD A, I
              LDAri(RegI);
            3: // LD A, R
              LDAri(RegR);
            4: // RRD
              RldOrRrd(True);
            5: // RLD
              RldOrRrd(False);
          otherwise // 6, 7: // NOP
            FFlagsModified := False;
          end;
        end;
      2:
        if (z and %100 = 0) and (y and %100 <> 0) then
          // bli[y, z]
          Bli(y, z)
        else begin
          // NONI + NOP
          FFlagsModified := False;
        end;
    otherwise
      //0, 3: // NONI + NOP
      FFlagsModified := False;
    end;
  end;

begin
  if FPrefixByte = 0 then begin
    // check interrupts
    if FNMI then begin
      FNMI := False;
      //FIff2 := FIff1; // NO! (page 20-21 - http://www.myquest.nl/z80undocumented/z80-documented-v0.91.pdf)

      FIff1 := False;
      if FHalt then begin
        FHalt := False;
        Inc(FRegPC);
      end;
      FAddressBus := FRegPC;
      Contention;
      Inc(FTStatesInCurrentFrame, 4);

      RefreshMem;
      FAddressBus := FRegIR.U16bit;
      ContentionAndIncTStates;
      PushToStack(FRegPC);

      FRegPC := NMIStartAdr;
      FFlagsModified := False;

      Exit;
    end;

    if FIntPin then begin
      if FIff1 then begin
        FFlagsModified := False;

        FIff2 := False;
        FIff1 := False;

        RefreshMem;
        if FHalt then begin
          FHalt := False;
          Inc(FRegPC);
        end;

        Inc(FTStatesInCurrentFrame, 7);
        PushToStack(FRegPC);

        case FInterruptMode of
          //0:  NOT IMPLEMENTED (for now...?)
          //  begin
          //   Spectrum never uses this mode (it cannot behave differently from im1, as it jumps to rom address which is, hm... fixed).
          //   Nevertheless, see if it could be implemented... one day }
          //    // ?
          //  end;
          2:
            FRegPC := ReadMem16(FRegIR.U16bit or $00FF);
        otherwise

          FRegPC := IntMode1StartAdr;
        end;

        Exit;
      end;
    end;
  end;
                           
  if FHalt then begin
    FAddressBus := FRegPC + 1;
    Contention;
    Inc(FTStatesInCurrentFrame, 4);
    RefreshMem;

    Exit;
  end;

  Pref := FPrefixByte;
  FPrefixByte := 0;

  FAddressBus := FRegPC;
  Contention;
  Inc(FTStatesInCurrentFrame, 4);

  OpCode := FMemory.ReadByte(FAddressBus);
  RefreshMem;
  FAddressBus := FRegIR.U16bit;
  Inc(FRegPC);

  x := OpCode shr 6;
  y := (OpCode shr 3) and %111;
  z := OpCode and %111;
  //p := y shr 1;
  //q := y and 1;

  case Pref of
    $CB:
      begin
        if z = 6 then // memory read, write:
          ProcessCB
        else // simple case, no memory access, directly manipulating registers:
          case x of
            0: // rot[y] r[z]
              Rot(y, ResolveTableR(z)^);
            1: // BIT y, r[z]
              begin
                x := ResolveTableR(z)^;
                TestBit(y, x);
                RegF := RegF or (x and %00101000);
              end;
            2: // RES y, r[z]
              ResetBit(y, ResolveTableR(z)^);
            3: // SET y, r[z]
              SetBit(y, ResolveTableR(z)^);
          otherwise
          end;

        Exit;
      end;
    $ED:
      begin
        ProcessED;
        Exit;
      end;
    $DD, $FD:
      case OpCode of
        $DD, $ED, $FD:
          begin
            // NONI
            FPrefixByte := OpCode;
            FFlagsModified := False;
            Exit;
          end;
        $CB:
          begin
            ProcessCB;
            Exit;
          end;
      otherwise
        // Keep Pref, as it will change the instructions which affect HL!
      end;
  otherwise
  end;

  case x of
    0:
      ProcessX0;
    1:
      ProcessX1;
    2:
      ProcessX2;
    3:
      ProcessX3;
  end;

end;

procedure TProcessor.SetMemory(Mem: TMemory);
begin
  FMemory := Mem;
end;

end.

