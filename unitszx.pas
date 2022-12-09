unit UnitSZX;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{ This is implementation of zx-state file format.
  http://www.spectaculator.com/redirect.php?id=szx
  the zx-state file format is maintained by Jonathan Needle,
  the author of Spectaculator. }

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, UnitFileSna, UnitStreamCompression, UnitSpectrum, UnitVer;

type

  TSnapshotSZX = class(TSnapshotFile)
  strict private
    type
      TZxstHeadr = packed record
      public
        const
          // machine id:
          ZXSTMID_16K = 0;
          ZXSTMID_48K = 1;
          // ....

          ZXSTMF_ALTERNATETIMINGS = 1; // late timings, for ChFlags
      public
        DwMagic: UInt32; // ZXST
        MajorVer: Byte;
        MinorVer: Byte;
        MachineId: Byte;
        ChFlags: Byte;

        class function GetUMagic: UInt32; static;
      end;

      TSzxBlock = class abstract (TObject)
      public
        type
          TChr4 = packed array [0..3] of AnsiChar;
      protected
        function WriteBlockSize(const Stream: TStream): Boolean;
        function LoadFromStream(const Stream: TStream): Boolean; virtual; abstract;
        function SaveToStream(const Stream: TStream): Boolean; virtual; abstract;
        class function GetBlockId: UInt32;

        class function GetBlockIdAsStr: RawByteString; virtual; abstract;
      public
        Szx: TSnapshotSZX;
        BlockSize: SizeUInt;

        constructor Create; virtual;
      end;

      TSzxBlockClass = class of TSzxBlock;
      
      TSzxBlocksMap = class(specialize TFPGMap<UInt32, TSzxBlockClass>)
      public
        constructor Create;
      end;

      TZxstZ80Regs = class(TSzxBlock)
      public
        const
          ZXSTZF_EILAST = 1; // the last instruction was EI, or $DD or $FD prefix
          ZXSTZF_HALTED = 2; // halted, nops are executed
          ZXSTZF_FSET = 4; // last instruction sets the flags
      public
        type
          TRecZ80Regs = packed record
            AF, BC, DE, HL: UInt16;
            AF1, BC1, DE1, HL1: UInt16;
            Ix, Iy, SP, PC: UInt16;
            I: Byte;
            R: Byte;
            IFF1, IFF2: Byte;
            IM: Byte; // interrupt mode (0, 1 or 2)
            DwCyclesStart: UInt32; // t-states in current frame
            ChHoldIntReqCycles: Byte; // when int pin is up, for how many t-states it should remain up.
            ChFlags: Byte; // combination of ZXSTZF_EILAST, ZXSTZF_HALTED and ZXSTZF_FSET
            MemPtr: UInt16;
          end;

      protected
        function LoadFromStream(const Stream: TStream): Boolean; override;
        function SaveToStream(const Stream: TStream): Boolean; override;

        class function GetBlockIdAsStr: RawByteString; override;
      end;

      TZxstSpecRegs = class(TSzxBlock)
      public
        type
          TRecSpec = packed record
            Border: Byte;
            Ch7ffd: Byte; // memory paging, not used on 48k
            Ch1ffd_or_Cheff7: Byte; // additional memory paging, not used on 48k
            Chfe: Byte; // last byte written to port $fe
            ChReserved: packed Array [0..3] of Byte;
          end;
      protected
        function LoadFromStream(const Stream: TStream): Boolean; override;
        function SaveToStream(const Stream: TStream): Boolean; override;

        class function GetBlockIdAsStr: RawByteString; override;
      end;

      TZxstRamPage = class(TSzxBlock)
      public
        const
          ZXSTRF_COMPRESSED = 1;
      public
        type
          TRecRamPage = packed record
            Flags: UInt16;
            PageNo: Byte;
          end;
      protected
        function LoadFromStream(const Stream: TStream): Boolean; override;
        function SaveToStream(const Stream: TStream): Boolean; override;

        class function GetBlockIdAsStr: RawByteString; override;
      public
        Rec: TRecRamPage;
      end;

      TZxstCreator = class(TSzxBlock)
      public
        type
          TRecCreator = packed record
            ZxCreator: Array [0..31] of Byte;
            MajorVer: UInt16;
            MinorVer: UInt16;
          //  Data: Byte;
          end;
      protected
        function LoadFromStream(const Stream: TStream): Boolean; override;
        function SaveToStream(const Stream: TStream): Boolean; override;

        class function GetBlockIdAsStr: RawByteString; override;
      end;

  strict private
    class var
      SzxBlocksMap: TSzxBlocksMap;

  strict private                  
    Mem: TMemoryStream;

  private
    State: TSpectrumInternalState;
    function GetMemStr(): TMemoryStream;
    class procedure Init; static;
    class procedure Final; static;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetDefaultExtension: String; override;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;
  end;

implementation

{ TSnapshotSZX.TZxstCreator }

function TSnapshotSZX.TZxstCreator.LoadFromStream(const Stream: TStream): Boolean;
begin
  Stream.Seek(BlockSize, TSeekOrigin.soCurrent);
  Result := True;
end;

function TSnapshotSZX.TZxstCreator.SaveToStream(const Stream: TStream): Boolean;
const
  Creator32: RawByteString = 'Swan';
var
  Rec: TRecCreator;
  S: RawByteString;
begin
  Rec := Default(TRecCreator);

  Move(Creator32[1], Rec.ZxCreator[0], Length(Creator32));
  Rec.MajorVer := UnitVer.TVersion.VersionMajor;
  Rec.MajorVer := NtoLE(Rec.MajorVer);
  Rec.MinorVer := UnitVer.TVersion.VersionMinor;
  Rec.MinorVer := NtoLE(Rec.MinorVer);

  S := 'Swan ZX Spectrum emulator ver. ' + TVersion.VersionString;

  BlockSize := SizeOf(Rec) + Length(S) + 1;
  Result := WriteBlockSize(Stream)
    and (Stream.Write(Rec, SizeOf(Rec)) = SizeOf(Rec))
    and (Stream.Write(PChar(S)^, Length(S) + 1) = Length(S) + 1);
end;

class function TSnapshotSZX.TZxstCreator.GetBlockIdAsStr: RawByteString;
begin
  Result := 'CRTR';
end;

{ TSnapshotSZX.TZxstHeadr }

class function TSnapshotSZX.TZxstHeadr.GetUMagic: UInt32;
const
  StrMagic: RawByteString = 'ZXST';
  {$push}
  {$J+}
  const
    UMagic: UInt32 = 0;
  {$pop}
begin
  if UMagic = 0 then begin
    Move(StrMagic[1], UMagic, 4);
  end;
  Result := UMagic;
end;

{ TSnapshotSZX.TZxstRamPage }

function TSnapshotSZX.TZxstRamPage.LoadFromStream(const Stream: TStream): Boolean;
var
  MemReadLen: Integer;
  Str1: TMemoryStream;
  Okay: Boolean;
  StreamMem: TMemoryStream;

begin
  Result := False;

  Rec := Default(TRecRamPage);
  if Stream.Read(Rec, SizeOf(Rec)) = SizeOf(Rec) then begin
    case Rec.PageNo of
      0, 2, 5:
        begin
          Okay := False;
          StreamMem := nil;
          try
            Rec.Flags := LEtoN(Rec.Flags);
            if Rec.Flags and ZXSTRF_COMPRESSED <> 0 then begin
              MemReadLen := BlockSize - SizeOf(Rec);
              if Stream.Size - Stream.Position >= MemReadLen then begin
                Str1 := TMemoryStream.Create;
                try
                  Str1.Size := MemReadLen;
                  Str1.Position := 0;
                  if Stream.Read(Str1.Memory^, MemReadLen) = MemReadLen then begin
                    StreamMem := TMemoryStream.Create;
                    if UnitStreamCompression.DecompressStream(Str1, StreamMem) and (StreamMem.Size = KB16) then begin
                      Okay := True;
                    end;
                  end;
                finally
                  Str1.Free;
                end;
              end;
            end else begin
              if Stream.Size - Stream.Position >= KB16 then begin
                StreamMem := TMemoryStream.Create;

                StreamMem.Size := KB16;
                StreamMem.Position := 0;
                if Stream.Read(StreamMem.Memory^, KB16) = KB16 then begin
                  Okay := True;
                end;
              end;
            end;

            if Okay then begin
              Szx.GetMemStr().Position := (2 - Rec.PageNo div 2) * KB16;
              if Szx.GetMemStr().Write(StreamMem.Memory^, KB16) = KB16 then
                Result := True;
            end;

          finally
            StreamMem.Free;
          end;

        end;
    otherwise
    end;
  end;
end;

function TSnapshotSZX.TZxstRamPage.SaveToStream(const Stream: TStream): Boolean;
var
  Str1, Str2: TStream;
  PB: PByte;
  IsCompressed: Boolean;
begin
  Result := False;
  case Rec.PageNo of
    0, 2, 5:
      begin
        IsCompressed := Rec.Flags and ZXSTRF_COMPRESSED <> 0;
        Rec.Flags := NtoLE(Rec.Flags);
        PB := PByte(Szx.GetMemStr.Memory) + (2 - Rec.PageNo div 2) * KB16;
        if IsCompressed then begin
          Str1 := TMemoryStream.Create();
          try
            Str1.Position := 0;
            if Str1.Write(PB^, KB16) = KB16 then begin
              Str2 := TMemoryStream.Create;
              try
                if CompressStream(Str1, Str2) then begin
                  BlockSize := Str2.Size + SizeOf(Rec);
                  Result := WriteBlockSize(Stream)
                    and (Stream.Write(Rec, SizeOf(Rec)) = SizeOf(Rec))
                    and (Stream.Write(TMemoryStream(Str2).Memory^, Str2.Size) = Str2.Size);
                end;

              finally
                Str2.Free;
              end;
            end;
          finally
            Str1.Free;
          end;
        end else begin
          BlockSize := KB16 + SizeOf(Rec);
          Result := WriteBlockSize(Stream)
              and (Stream.Write(Rec, SizeOf(Rec)) = SizeOf(Rec))
              and (Stream.Write(PB^, KB16) = KB16);
        end;

      end;
  otherwise
  end;
end;

class function TSnapshotSZX.TZxstRamPage.GetBlockIdAsStr: RawByteString;
begin
  Result := 'RAMP';
end;

{ TSnapshotSZX.TZxstSpecRegs }

function TSnapshotSZX.TZxstSpecRegs.LoadFromStream(const Stream: TStream): Boolean;
var
  Rec: TRecSpec;
  BlSz, N: SizeUInt;
begin
  Result := False;

  if BlockSize <= SizeOf(Rec) then begin
    BlSz := BlockSize;
    N := 0;
  end else begin
    BlSz := SizeOf(Rec);
    N := BlockSize - BlSz;
  end;

  Rec := Default(TRecSpec);
  if Stream.Read(Rec, BlSz) = BlSz then begin
    if N > 0 then
      Stream.Seek(N, TSeekOrigin.soCurrent);
    Szx.State.BorderColour := Rec.Border;
    Szx.State.Ear := (Rec.Chfe shl 2) and 64;
    // everything else ignored...
    Result := True;
  end;
end;

function TSnapshotSZX.TZxstSpecRegs.SaveToStream(const Stream: TStream): Boolean;
var
  Rec: TRecSpec;

begin
  Rec := Default(TRecSpec);
  Rec.Border := Szx.State.BorderColour;
  Rec.Chfe := (Szx.State.Ear and 64) shr 2;

  BlockSize := SizeOf(Rec);
  Result := WriteBlockSize(Stream)
    and (Stream.Write(Rec, BlockSize) = BlockSize);
end;

class function TSnapshotSZX.TZxstSpecRegs.GetBlockIdAsStr: RawByteString;
begin
  Result := 'SPCR';
end;

{ TSnapshotSZX.TSzxBlocksMap }

constructor TSnapshotSZX.TSzxBlocksMap.Create;
begin
  inherited Create;

  Sorted := True;
  Duplicates := TDuplicates.dupIgnore;
end;

{ TSnapshotSZX.TZxstZ80Regs }

function TSnapshotSZX.TZxstZ80Regs.LoadFromStream(const Stream: TStream): Boolean;
var
  Rec: TRecZ80Regs;
  BlSz, N: SizeUInt;
begin
  Result := False;

  if BlockSize <= SizeOf(Rec) then begin
    BlSz := BlockSize;
    N := 0;
  end else begin
    BlSz := SizeOf(Rec);
    N := BlockSize - BlSz;
  end;

  Rec := Default(TRecZ80Regs);
  if Stream.Read(Rec, BlSz) = BlSz then begin
    if N > 0 then
      Stream.Seek(N, TSeekOrigin.soCurrent);
    Szx.State.AF := LEtoN(Rec.AF);
    Szx.State.BC := LEtoN(Rec.BC);
    Szx.State.DE := LEtoN(Rec.DE);
    Szx.State.HL := LEtoN(Rec.HL);
    Szx.State.AF1 := LEtoN(Rec.AF1);
    Szx.State.BC1 := LEtoN(Rec.BC1);
    Szx.State.DE1 := LEtoN(Rec.DE1);
    Szx.State.HL1 := LEtoN(Rec.HL1);
    Szx.State.Ix := LEtoN(Rec.Ix);
    Szx.State.Iy := LEtoN(Rec.Iy);
    Szx.State.SP := LEtoN(Rec.SP);
    Szx.State.PC := LEtoN(Rec.PC);
    WordRec(Szx.State.IR).Hi := Rec.I;
    WordRec(Szx.State.IR).Lo := Rec.R;
    Szx.State.IFF1 := Rec.IFF1 <> 0;
    Szx.State.IFF2 := Rec.IFF2 <> 0;

    Szx.State.InterruptMode := Rec.IM;
    Szx.State.T_States := LEtoN(Rec.DwCyclesStart);
    Szx.State.RemainingIntPinUp := Rec.ChHoldIntReqCycles;
    Szx.State.SkipInterruptCheck := (Rec.ChFlags and ZXSTZF_EILAST) <> 0;
    Szx.State.Halt := (Rec.ChFlags and ZXSTZF_HALTED) <> 0;
    Szx.State.FlagsModified := (Rec.ChFlags and ZXSTZF_FSET) <> 0;
    Szx.State.WZ := LEtoN(Rec.MemPtr);

    Result := True;
  end;

end;

function TSnapshotSZX.TZxstZ80Regs.SaveToStream(const Stream: TStream): Boolean;
var
  Rec: TRecZ80Regs;

begin
  Rec.AF := NtoLE(Szx.State.AF);
  Rec.BC := NtoLE(Szx.State.BC);
  Rec.DE := NtoLE(Szx.State.DE);
  Rec.HL := NtoLE(Szx.State.HL);
  Rec.AF1 := NtoLE(Szx.State.AF1);
  Rec.BC1 := NtoLE(Szx.State.BC1);
  Rec.DE1 := NtoLE(Szx.State.DE1);
  Rec.HL1 := NtoLE(Szx.State.HL1);
  Rec.Ix := NtoLE(Szx.State.Ix);
  Rec.Iy := NtoLE(Szx.State.Iy);
  Rec.SP := NtoLE(Szx.State.SP);
  Rec.PC := NtoLE(Szx.State.PC);
  Rec.I := WordRec(Szx.State.IR).Hi;
  Rec.R := WordRec(Szx.State.IR).Lo;
  if Szx.State.IFF1 then
    Rec.IFF1 := 1
  else
    Rec.IFF1 := 0;
  if Szx.State.IFF2 then
    Rec.IFF2 := 1
  else
    Rec.IFF2 := 0;
  Rec.IM := Szx.State.InterruptMode;
  Rec.DwCyclesStart := NtoLE(Szx.State.T_States);

  Rec.ChHoldIntReqCycles := Byte(Szx.State.RemainingIntPinUp and $FF);
  Rec.ChFlags := 0;
  if Szx.State.SkipInterruptCheck then
    Rec.ChFlags := Rec.ChFlags or ZXSTZF_EILAST;
  if Szx.State.Halt then
    Rec.ChFlags := Rec.ChFlags or ZXSTZF_HALTED;
  if Szx.State.FlagsModified then
    Rec.ChFlags := Rec.ChFlags or ZXSTZF_FSET;

  Rec.MemPtr := NtoLE(Szx.State.WZ);

  BlockSize := SizeOf(Rec);

  Result := WriteBlockSize(Stream)
    and (Stream.Write(Rec, BlockSize) = BlockSize);
end;

class function TSnapshotSZX.TZxstZ80Regs.GetBlockIdAsStr: RawByteString;
begin
  Result := 'Z80R';
end;

{ TSnapshotSZX }

function TSnapshotSZX.GetMemStr: TMemoryStream;
begin
  if Mem = nil then begin
    Mem := TMemoryStream.Create;
    Mem.Size := KB48;
  end;
  Result := Mem;
end;

class procedure TSnapshotSZX.Init;

  procedure AddBlockClass(BlockClass: TSzxBlockClass);
  begin
    SzxBlocksMap.Add(BlockClass.GetBlockId, BlockClass);
  end;

begin
  SzxBlocksMap := TSzxBlocksMap.Create;

  AddBlockClass(TZxstZ80Regs);
  AddBlockClass(TZxstSpecRegs);
  AddBlockClass(TZxstRamPage);
end;

class procedure TSnapshotSZX.Final;
begin
  SzxBlocksMap.Free;
end;

constructor TSnapshotSZX.Create;
begin
  State := Default(TSpectrumInternalState);
  Mem := nil;
end;

destructor TSnapshotSZX.Destroy;
begin
  Mem.Free;

  inherited Destroy;
end;

class function TSnapshotSZX.GetDefaultExtension: String;
begin
  Result := 'szx';
end;

function TSnapshotSZX.LoadFromStream(const Stream: TStream): Boolean;

  function LoadHeader(): Boolean;
  var
    SzxHeader: TZxstHeadr;
  begin
    Result := False;

    if Stream.Size >= SizeOf(SzxHeader) then begin
      if Stream.Read(SzxHeader{%H-}, SizeOf(SzxHeader)) = SizeOf(SzxHeader) then begin
        if SzxHeader.DwMagic = TZxstHeadr.GetUMagic() then
          case SzxHeader.MachineId of
            TZxstHeadr.ZXSTMID_16K, TZxstHeadr.ZXSTMID_48K:
              Exit(True);
          end;
      end;
    end;
  end;

  function LoadBlock(): Boolean;
  var
    BlSize: UInt32;
    BlId: UInt32;
    Block: TSzxBlock;
    BlClass: TSzxBlockClass;

  begin
    Result := False;
    if Stream.Size - Stream.Position >= 8 then begin
      if (Stream.Read(BlId{%H-}, 4) = 4) and (Stream.Read(BlSize{%H-}, 4) = 4) then begin
        BlSize := LEtoN(BlSize);
        if Stream.Size - Stream.Position >= BlSize then begin
          if SzxBlocksMap.TryGetData(BlId, BlClass) then begin
            Block := BlClass.Create;
            try
              Block.Szx := Self;
              Block.BlockSize := BlSize;

              if Block.LoadFromStream(Stream) then begin
                Result := True;
              end;
            except
            end;
            Block.Free;

          end else begin
            Stream.Seek(BlSize, TSeekOrigin.soCurrent);
            Result := True; // block not supported, just skip it.
          end;
        end;
      end;
    end;
  end;

begin
  Stream.Position := 0;

  if LoadHeader() then begin
    Self.State := Default(TSpectrumInternalState);
    FillChar(GetMemStr().Memory^, GetMemStr().Size, 0);

    while LoadBlock() do
      if Stream.Position = Stream.Size then begin
        if State.SaveToSpectrum(FSpectrum) then begin
          GetMemStr().Position := 0;
          if FSpectrum.GetProcessor.GetMemory^.LoadRamFromStream(GetMemStr()) then
            Exit(True);
        end;
      end;
  end;

  Result := False;
end;

function TSnapshotSZX.SaveToStream(const Stream: TStream): Boolean;

  function SaveHeader: Boolean;
  var
    SzxHeader: TZxstHeadr;
  begin
    //
    SzxHeader.DwMagic := TZxstHeadr.GetUMagic;
    SzxHeader.MajorVer := 1;
    SzxHeader.MinorVer := 5;
    SzxHeader.MachineId := TZxstHeadr.ZXSTMID_48K;
    SzxHeader.ChFlags := 0;

    Result := Stream.Write(SzxHeader, SizeOf(SzxHeader)) = SizeOf(SzxHeader);
  end;

  function SaveBlock(Block: TSzxBlock): Boolean;
  begin
    try
      Block.Szx := Self;
      Result := (Stream.Write(Block.GetBlockId, 4) = 4)
        and (Block.SaveToStream(Stream));
    finally
      Block.Free;
    end;
  end;

var
  BlockRP: TZxstRamPage;
  I: Integer;
begin
  Result := False;

  if State.LoadFromSpectrum(FSpectrum) then begin
    GetMemStr.Position := 0;
    if FSpectrum.GetProcessor.GetMemory^.SaveRamToStream(GetMemStr()) then begin
      if SaveHeader then begin
        if SaveBlock(TZxstCreator.Create)
          and SaveBlock(TZxstZ80Regs.Create)
          and SaveBlock(TZxstSpecRegs.Create)
        then begin
          for I := 2 downto 0 do begin
            BlockRP := TZxstRamPage.Create;
            BlockRP.Rec.PageNo := 2 * I + I div 2;
            BlockRP.Rec.Flags := TZxstRamPage.ZXSTRF_COMPRESSED;
            if not SaveBlock(BlockRP) then
              Exit(False);
            Result := True;
          end;
        end;
      end;
    end;
  end;
end;

{ TSnapshotSZX.TSzxBlock }

function TSnapshotSZX.TSzxBlock.WriteBlockSize(const Stream: TStream): Boolean;
var
  BlSize: UInt32;
begin
  BlSize := BlockSize;
  BlSize := NtoLE(BlSize);

  Result := (Stream.Write(BlSize, 4) = 4);
end;

class function TSnapshotSZX.TSzxBlock.GetBlockId: UInt32;
var
  S: RawByteString;
begin
  Result := 0;
  S := GetBlockIdAsStr;
  if Length(S) = 4 then
    Move(S[1], Result, 4);
end;

constructor TSnapshotSZX.TSzxBlock.Create;
begin
  Szx := nil;
  BlockSize := 0;
end;

initialization
  TSnapshotSZX.Init;

finalization
  TSnapshotSZX.Final;

end.

