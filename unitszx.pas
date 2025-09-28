unit UnitSZX;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{ This is implementation of zx-state (szx) file format.
  Specification: https://www.spectaculator.com/docs/zx-state/intro.shtml
     ver. 1.5 currently: https://www.spectaculator.com/docs/svn/zx-state/intro.shtml
  The zx-state file format is maintained by Jonathan Needle,
  the author of Spectaculator. }

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, UnitSnapshotFiles, UnitStreamCompression, UnitSpectrum,
  UnitVer, UnitJoystick, UnitTapePlayer, UnitCommonSpectrum;

type

  TSnapshotSZX = class(TSnapshotFile)
  public
    type
      TSzxSaveTapeOptions = (sstoSkip, sstoEmbeddedCompressed,
        sstoEmbeddedUncompressed, sstoFilePathOnly);

      TOnSzxLoadTape = procedure(AStream: TStream; const AFileName: String;
        const AExtension: String; ACurrentBlock: Integer) of object;
      TOnSzxSaveTape = procedure(out ATapePlayer: TTapePlayer) of object;

  private

    type
      // zx-state header
      TZxstHeadr = packed record
      public
        const
          // machine id:
          ZXSTMID_16K = 0;
          ZXSTMID_48K = 1;
          ZXSTMID_128K = 2;
          ZXSTMID_PLUS2 = 3;
          // models currently not supported in Swan:
          ZXSTMID_PLUS2A = 4;
          ZXSTMID_PLUS3 = 5;
          ZXSTMID_PLUS3E = 6;
          ZXSTMID_PENTAGON128 = 7;
          ZXSTMID_TC2048 = 8;
          ZXSTMID_TC2068 = 9;
          ZXSTMID_SCORPION = 10;
          ZXSTMID_SE = 11;
          ZXSTMID_TS2068 = 12;
          ZXSTMID_PENTAGON512 = 13;
          ZXSTMID_PENTAGON1024 = 14;
          ZXSTMID_NTSC48K = 15;
          ZXSTMID_128KE = 16;

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
      protected
        function WriteBlockSize(const Stream: TStream): Boolean;
        function LoadFromStream(const Stream: TStream): Boolean; virtual; abstract;
        function SaveToStream(const Stream: TStream): Boolean; virtual; abstract;

        class function GetBlockId: UInt32;
        class function GetBlockIdAsStr: RawByteString; virtual; abstract;
      public
        Szx: TSnapshotSZX;
        BlockSize: Int64;

        constructor Create; virtual;
      end;

      TSzxBlockClass = class of TSzxBlock;

      TSzxBlocksMap = class(specialize TFPGMap<UInt32, TSzxBlockClass>)
      public
        constructor Create;
      end;

  strict private
    class var
      SzxBlocksMap: TSzxBlocksMap;
      FSkipJoystickInfoLoad: Boolean;
      FOnSzxLoadTape: TOnSzxLoadTape;
      FOnSzxSaveTape: TOnSzxSaveTape;
      FSaveTapeOptions: TSzxSaveTapeOptions;

  private
    Mem: TMemoryStream;
    RomStream: TMemoryStream;

    IsIssue2: Boolean;
    JoystickType: TJoystick.TJoystickType;
    JoystickAttached: Boolean;

    State: TSpectrumInternalState;

    function GetMemStr(): TMemoryStream;
    procedure SetMemSize(ASize: Integer);

    procedure RaiseSnapshotLoadErrorSZX(const S: AnsiString);

  private
    class procedure Init; static;
    class procedure Final; static;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetDefaultExtension: String; override;
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;

    class property SkipJoystickInfoLoad: Boolean read FSkipJoystickInfoLoad write FSkipJoystickInfoLoad;
    class property OnSzxLoadTape: TOnSzxLoadTape read FOnSzxLoadTape write FOnSzxLoadTape;
    class property OnSzxSaveTape: TOnSzxSaveTape read FOnSzxSaveTape write FOnSzxSaveTape;
    class property SaveTapeOptions: TSzxSaveTapeOptions read FSaveTapeOptions write FSaveTapeOptions;
  end;

implementation

type

  // ZXSTZ80REGS
  TZxstZ80Regs = class(TSnapshotSZX.TSzxBlock)
  private
    const
      ZXSTZF_EILAST = 1; // the last instruction was EI
      ZXSTZF_HALTED = 2; // halted, nops are executed
      ZXSTZF_FSET = 4; // last instruction sets the flags

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

  // ZXSTSPECREGS
  TZxstSpecRegs = class(TSnapshotSZX.TSzxBlock)
  private
    type
      TRecSpec = packed record
        Border: Byte;
        Ch7ffd: Byte; // memory paging, not used on 48k
        Ch1ffd_or_Cheff7: Byte; // additional memory paging, used on +2A, +3
        Chfe: Byte; // last byte written to port $fe
        ChReserved: packed Array [0..3] of Byte;
      end;

  protected
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;

    class function GetBlockIdAsStr: RawByteString; override;
  end;

  // ZXSTRAMPAGE
  TZxstRamPage = class(TSnapshotSZX.TSzxBlock)
  public
    const
      ZXSTRF_COMPRESSED = 1;
  private
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

  // ZXSTCREATOR
  TZxstCreator = class(TSnapshotSZX.TSzxBlock)
  private
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

  // ZXSTKEYBOARD
  TZxstKeyboard = class(TSnapshotSZX.TSzxBlock)
  private
    const
      ZXSTKF_ISSUE2 = 1;

      ZXSTKJT_KEMPSTON = 0;
      ZXSTKJT_FULLER = 1;
      ZXSTKJT_CURSOR = 2;
      ZXSTKJT_SINCLAIR1 = 3;
      ZXSTKJT_SINCLAIR2 = 4;
      //ZXSTKJT_SPECTRUMPLUS = 5;
      //ZXSTKJT_TIMEX1 = 6;
      //ZXSTKJT_TIMEX2 = 7;
      ZXSTKJT_NONE = 8;

    type
      TRecKeyBoard = packed record
        DwFlags: DWord;
        ChKeyboardJoystick: Byte;
      end;

  protected
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;

    class function GetBlockIdAsStr: RawByteString; override;
  end;

  // ZXSTTAPE
  TZxstTape = class(TSnapshotSZX.TSzxBlock)
  private
    const
      ZXSTTP_EMBEDDED = 1;
      ZXSTTP_COMPRESSED = 2;

    type
      TRecTape = packed record
        CurrentBlockNo: UInt16;
        Flags: UInt16;
        UncompressedSize: UInt32;
        CompressedSize: UInt32;
        FileExtension: Array [0..15] of AnsiChar;
      end;

  protected
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;

    class function GetBlockIdAsStr: RawByteString; override;

  public
    FTapePlayer: TTapePlayer;

    constructor Create; override;
    constructor Create(ATapePlayer: TTapePlayer);
  end;

  // ZXSTAYBLOCK
  TZxstAYBlock = class(TSnapshotSZX.TSzxBlock)
  private
    type
      TAyBlockRec = packed record
        Flags: Byte;
        CurrentRegister: Byte;
        AyRegisters: array[0..15] of Byte;
      end;

  protected
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;

    class function GetBlockIdAsStr: RawByteString; override;
  end;

  // ZXSTROM
  TZxstROM = class(TSnapshotSZX.TSzxBlock)
  private
    const
      ZXSTRF_COMPRESSED = 1;

    type
      TRecRom = packed record
        Flags: Word;
        UncompressedSize: DWord;
      end;

  protected
    function LoadFromStream(const Stream: TStream): Boolean; override;
    function SaveToStream(const Stream: TStream): Boolean; override;

    class function GetBlockIdAsStr: RawByteString; override;
  end;

{ TZxstROM }

function TZxstROM.LoadFromStream(const Stream: TStream): Boolean;
var
  Rec: TRecRom;
  DataSize: Int32;
  Str1, Str2: TMemoryStream;
  UncompressedSize: Int32 absolute Rec.UncompressedSize;

begin
  Result := False;

  if BlockSize > SizeOf(TRecRom) then begin
    if Stream.Read(Rec{%H-}, SizeOf(Rec)) = SizeOf(Rec) then begin
      Rec.Flags := LEtoN(Rec.Flags);
      Rec.UncompressedSize := LEtoN(Rec.UncompressedSize);

      if (UncompressedSize > 0) and (UncompressedSize mod KB16 = 0) then begin
        DataSize := BlockSize - SizeOf(TRecRom);
        if Stream.Size >= Stream.Position + DataSize then begin
          Str1 := nil;
          try
            // Free RomStream -- only to prevent memory leak if the szx contains
            // more than one ZXSTROM block. This should never happen, but it is
            // out of our control, so keep it here:
            FreeAndNil(Szx.RomStream);

            if Rec.Flags and ZXSTRF_COMPRESSED <> 0 then begin
              Str2 := TMemoryStream.Create;
              try
                Str2.Size := DataSize;
                if Stream.Read(Str2.Memory^, DataSize) = DataSize then begin
                  Str1 := TMemoryStream.Create;
                  Result := DecompressStream(Str2, Str1) and (Str1.Size = UncompressedSize);
                end;
              finally
                Str2.Free;
              end;
            end else begin
              if DataSize = UncompressedSize then begin
                Str1 := TMemoryStream.Create;
                Str1.Size := DataSize;
                Result := Stream.Read(Str1.Memory^, DataSize) = DataSize;
              end;
            end;

            if Result then begin
              Szx.RomStream := Str1;
              Str1 := nil;
            end;
          finally
            Str1.Free;
          end;

        end;
      end;
    end;

  end;
end;

function TZxstROM.SaveToStream(const Stream: TStream): Boolean;
var
  Str1: TMemoryStream;
  Rec: TRecRom;

begin
  Result := False;

  if Assigned(Szx.RomStream) then begin
    Str1 := TMemoryStream.Create;
    try
      Szx.RomStream.Position := 0;
      if CompressStream(Szx.RomStream, Str1) then begin
        Rec.Flags := ZXSTRF_COMPRESSED;
        Rec.UncompressedSize := Szx.RomStream.Size;

        Rec.Flags := NtoLE(Rec.Flags);
        Rec.UncompressedSize := NtoLE(Rec.UncompressedSize);

        BlockSize := Str1.Size + SizeOf(Rec);
        Result := WriteBlockSize(Stream)
          and (Stream.Write(Rec, SizeOf(Rec)) = SizeOf(Rec))
          and (Stream.Write(Str1.Memory^, Str1.Size) = Str1.Size);
      end;

    finally
      Str1.Free;
    end;
  end;
end;

class function TZxstROM.GetBlockIdAsStr: RawByteString;
begin
  Result := 'ROM' + #0;
end;

{ TZxstAYBlock }

function TZxstAYBlock.LoadFromStream(const Stream: TStream): Boolean;
var
  AyRec: TAyBlockRec;
  I: Integer;

begin
  Result := False;
  if (BlockSize >= SizeOf(AyRec))
     and (Stream.Read(AyRec{%H-}, SizeOf(AyRec)) = SizeOf(AyRec))
  then begin
    if BlockSize > SizeOf(AyRec) then
      Stream.Seek(BlockSize - SizeOf(AyRec), TSeekOrigin.soCurrent);

    for I := 0 to 15 do begin
      Szx.State.AyState.Registers[I] := AyRec.AyRegisters[I];
    end;
    Szx.State.AyState.ActiveRegisterNumber := AyRec.CurrentRegister;
    Szx.State.HasAy := True;

    Result := True;
  end;
end;

function TZxstAYBlock.SaveToStream(const Stream: TStream): Boolean;
var
  AyRec: TAyBlockRec;
  I: Integer;

begin
  AyRec.CurrentRegister := Szx.State.AyState.ActiveRegisterNumber;
  for I := 0 to 15 do
    AyRec.AyRegisters[I] := Szx.State.AyState.Registers[I];

  BlockSize := SizeOf(AyRec);
  Result := WriteBlockSize(Stream)
    and (Stream.Write(AyRec, BlockSize) = BlockSize);
end;

class function TZxstAYBlock.GetBlockIdAsStr: RawByteString;
begin
  Result := 'AY' + #0 + #0;
end;

{ TZxstTape }

function TZxstTape.LoadFromStream(const Stream: TStream): Boolean;
var
  RecTape: TRecTape;
  FileExtension: RawByteString;
  DataSize: Int32;
  IsEmbedded: Boolean;
  Str1: TMemoryStream;
  Str2: TMemoryStream;
  S: RawByteString;
  I: Integer;

begin
  if not Assigned(Szx.OnSzxLoadTape) then begin
    Stream.Seek(BlockSize, TSeekOrigin.soCurrent);
    Result := True;
  end else begin
    Result := False;
    if (BlockSize > SizeOf(RecTape))
      and (Stream.Read(RecTape{%H-}, SizeOf(RecTape)) = SizeOf(RecTape))
    then begin
      RecTape.Flags := LEtoN(RecTape.Flags);
      RecTape.UncompressedSize := LEtoN(RecTape.UncompressedSize);
      RecTape.CompressedSize := LEtoN(RecTape.CompressedSize);
      RecTape.CurrentBlockNo := LEtoN(RecTape.CurrentBlockNo);

      DataSize := BlockSize - SizeOf(TRecTape);
      if (DataSize > 0) and (DataSize = Int32(RecTape.CompressedSize))
        and (Stream.Size >= Stream.Position + DataSize)
      then begin
        Str1 := nil;
        try
          IsEmbedded := (RecTape.Flags and ZXSTTP_EMBEDDED) <> 0;
          if IsEmbedded then begin
            S := '';
            if (RecTape.Flags and ZXSTTP_COMPRESSED) <> 0 then begin
              Str2 := TMemoryStream.Create;
              try
                Str2.Size := DataSize;
                if Stream.Read(Str2.Memory^, DataSize) = DataSize then begin
                  Str1 := TMemoryStream.Create;
                  Result := DecompressStream(Str2, Str1) and (Str1.Size = RecTape.UncompressedSize);
                end;
              finally
                Str2.Free;
              end;
            end else begin
              Str1 := TMemoryStream.Create;
              Str1.Size := DataSize;
              Result := Stream.Read(Str1.Memory^, DataSize) = DataSize;
            end;

            if Assigned(Str1) then
              Str1.Position := 0;

            SetLength(FileExtension{%H-}, Length(RecTape.FileExtension));
            Move(RecTape.FileExtension[0], FileExtension[1], Length(RecTape.FileExtension));
            FileExtension := Trim(FileExtension);

          end else begin
            // linked file by name
            SetLength(S, DataSize);
            if Stream.Read(S[1], DataSize) = DataSize then begin
              // When testing szx tape blocks with tape paths made by other
              // emulators (SpecEmu), there were found null characters added at
              // the end of the file name. So:
              for I := 1 to DataSize do
                if S[I] = #0 then begin
                  SetLength(S, I - 1);
                  Break;
                end;

              if Length(S) > 0 then begin
                FileExtension := ExtractFileExt(S);
                Result := True;
              end;
            end;
          end;
          //
          if Result then
            Szx.OnSzxLoadTape(Str1, S, FileExtension, RecTape.CurrentBlockNo);
        finally
          Str1.Free;
        end;
      end;
    end;
  end;
end;

function TZxstTape.SaveToStream(const Stream: TStream): Boolean;
var
  Rec: TRecTape;

  function WriteRec: Boolean;
  begin
    if Rec.CompressedSize = 0 then
      Exit(False);

    BlockSize := SizeOf(Rec);
    BlockSize := BlockSize + Rec.CompressedSize;

    Rec.CurrentBlockNo := NtoLE(Rec.CurrentBlockNo);
    Rec.Flags := NtoLE(Rec.Flags);
    Rec.CompressedSize := NtoLE(Rec.CompressedSize);
    Rec.UncompressedSize := NtoLE(Rec.UncompressedSize);

    Result := WriteBlockSize(Stream)
      and (Stream.Write(Rec, SizeOf(Rec)) = SizeOf(Rec));
  end;

var
  Str0, Str1, Str2: TMemoryStream;
  FileName, FileExt: RawByteString;
  I: Integer;

begin
  Result := False;
  if Assigned(FTapePlayer) then begin
    Rec := Default(TRecTape);

    FileName := FTapePlayer.FileName;

    FileExt := FTapePlayer.GetDefaultExtension();
    if FileExt = '' then
      FileExt := Trim(ExtractFileExt(FileName));

    I := Length(ExtensionSeparator);
    if AnsiCompareText(Copy(FileExt, 1, I), ExtensionSeparator) = 0 then
      FileExt := Trim(Copy(FileExt, I + 1));

    I := Length(FileExt);
    if I > Length(Rec.FileExtension) then
      I := Length(Rec.FileExtension);
    if I > 0 then begin
      Move(FileExt[1], Rec.FileExtension[0], I);

      if FTapePlayer.GetCurrentBlockNumber >= 0 then begin
        Rec.CurrentBlockNo := FTapePlayer.GetCurrentBlockNumber;
        if Szx.SaveTapeOptions = TSnapshotSZX.TSzxSaveTapeOptions.sstoFilePathOnly then begin
          Rec.CompressedSize := Length(FileName);
          Rec.UncompressedSize := Rec.CompressedSize;
          Result := WriteRec and (Stream.Write(FileName[1], Length(FileName)) = Length(FileName));
        end else begin
          Rec.Flags := ZXSTTP_EMBEDDED;
          Str0 := TMemoryStream.Create;
          try
            if FTapePlayer.SaveToStream(Str0) then begin
              Rec.UncompressedSize := Str0.Size;
              if Rec.UncompressedSize > 0 then begin
                try
                  if Szx.SaveTapeOptions <> TSnapshotSZX.TSzxSaveTapeOptions.sstoEmbeddedCompressed then begin
                    Rec.CompressedSize := Rec.UncompressedSize;
                    Result := True;
                  end else begin
                    Rec.Flags := Rec.Flags or ZXSTTP_COMPRESSED;
                    Str1 := TMemoryStream.Create;
                    try
                      if CompressStream(Str0, Str1) then begin
                        Rec.CompressedSize := Str1.Size;
                        if Rec.CompressedSize > 0 then begin
                          Str2 := Str1;
                          Str1 := Str0;
                          Str0 := Str2;
                          Result := True;
                        end;
                      end;
                    finally
                      Str1.Free;
                    end;
                  end;
                  Result := Result and WriteRec
                    and (Stream.Write(Str0.Memory^, Str0.Size) = Str0.Size);
                except
                  Result := False;
                end;
              end;
            end;
          finally
            Str0.Free;
          end;
        end;
      end;
    end;
  end;
end;

class function TZxstTape.GetBlockIdAsStr: RawByteString;
begin
  Result := 'TAPE';
end;

constructor TZxstTape.Create;
begin
  Create(nil);
end;

constructor TZxstTape.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create;

  FTapePlayer := ATapePlayer;
end;

{ TZxstKeyboard }

function TZxstKeyboard.LoadFromStream(const Stream: TStream
  ): Boolean;

var
  Rec: TRecKeyBoard;
  N: Integer;
  BlSz: Integer;

begin
  Result := False;

  if BlockSize >= SizeOf(DWord) then begin // at least the first field must be there (it's defined since ver. 1.0)

    if BlockSize <= SizeOf(Rec) then begin
      // possible if file ver. is 1.0 -- the block size was one byte smaller.
      BlSz := BlockSize;
      N := 0;
    end else begin
      BlSz := SizeOf(Rec);
      N := BlockSize - BlSz;
    end;

    Rec := Default(TRecKeyBoard);
    Rec.ChKeyboardJoystick := ZXSTKJT_NONE;
    if Stream.Read(Rec, BlSz) = BlSz then begin
      if N > 0 then
        Stream.Seek(N, TSeekOrigin.soCurrent);
      Rec.DwFlags := LEtoN(Rec.DwFlags);
      Szx.IsIssue2 := Rec.DwFlags and 1 <> 0;

      Szx.JoystickAttached := True;
      case Rec.ChKeyboardJoystick of
        ZXSTKJT_KEMPSTON:
          Szx.JoystickType := TJoystick.TJoystickType.jtKempston;
        ZXSTKJT_FULLER:
          Szx.JoystickType := TJoystick.TJoystickType.jtFuller;
        ZXSTKJT_CURSOR:
          Szx.JoystickType := TJoystick.TJoystickType.jtCursor;
        ZXSTKJT_SINCLAIR1:
          Szx.JoystickType := TJoystick.TJoystickType.jtInterface_II_left;
        ZXSTKJT_SINCLAIR2:
          Szx.JoystickType := TJoystick.TJoystickType.jtInterface_II_right;
      otherwise
        Szx.JoystickAttached := False;
      end;

      Result := True;
    end;

  end;
end;

function TZxstKeyboard.SaveToStream(const Stream: TStream
  ): Boolean;
var
  Rec: TRecKeyBoard;
begin
  Rec := Default(TRecKeyBoard);
  if Szx.IsIssue2 then
    Rec.DwFlags := ZXSTKF_ISSUE2;
  Rec.DwFlags := NtoLE(Rec.DwFlags);
  Rec.ChKeyboardJoystick := ZXSTKJT_NONE;
  if Szx.JoystickAttached then begin
    case Szx.JoystickType of
      TJoystick.TJoystickType.jtKempston:
        Rec.ChKeyboardJoystick := ZXSTKJT_KEMPSTON;
      TJoystick.TJoystickType.jtFuller:
        Rec.ChKeyboardJoystick := ZXSTKJT_FULLER;
      TJoystick.TJoystickType.jtInterface_II_left:
        Rec.ChKeyboardJoystick := ZXSTKJT_SINCLAIR1;
      TJoystick.TJoystickType.jtInterface_II_right:
        Rec.ChKeyboardJoystick := ZXSTKJT_SINCLAIR2;
      TJoystick.TJoystickType.jtCursor:
        Rec.ChKeyboardJoystick := ZXSTKJT_CURSOR;
    otherwise
    end;
  end;

  BlockSize := SizeOf(Rec);
  Result := WriteBlockSize(Stream)
    and (Stream.Write(Rec, BlockSize) = BlockSize);
end;

class function TZxstKeyboard.GetBlockIdAsStr: RawByteString;
begin
  Result := 'KEYB';
end;

{ TZxstCreator }

function TZxstCreator.LoadFromStream(const Stream: TStream): Boolean;
begin
  Stream.Seek(BlockSize, TSeekOrigin.soCurrent);
  Result := True;
end;

function TZxstCreator.SaveToStream(const Stream: TStream): Boolean;
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
    and (Stream.Write(PAnsiChar(S)^, Length(S) + 1) = Length(S) + 1);
end;

class function TZxstCreator.GetBlockIdAsStr: RawByteString;
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

{ TZxstRamPage }

function TZxstRamPage.LoadFromStream(const Stream: TStream): Boolean;
var
  MemReadLen: Integer;
  Str1: TMemoryStream;
  Okay: Boolean;
  StreamMem: TMemoryStream;
  P: Integer;

begin
  Result := False;

  if BlockSize > SizeOf(Rec) then begin
    Rec := Default(TRecRamPage);
    if Stream.Read(Rec, SizeOf(Rec)) = SizeOf(Rec) then begin
      case Rec.PageNo of
        5:
          Okay := True;
        0, 2:
          Okay := Szx.GetMemStr().Size >= KB48;
        1, 3, 4, 6, 7:
          Okay := Szx.GetMemStr().Size >= KB128;
      otherwise
        Okay := False;
      end;

      if Okay then begin
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
            case Rec.PageNo of
              0, 2, 5:
                P := 2 - Rec.PageNo div 2;
            otherwise
              P := (7 + 2 * Rec.PageNo) div 3;
            end;

            P := P * KB16;
            if Szx.GetMemStr().Size - P >= KB16 then begin
              StreamMem.Position := 0;
              if StreamMem.Read((PByte(Szx.GetMemStr().Memory) + P)^, KB16) = KB16 then
                Result := True;
            end;
          end;

        finally
          StreamMem.Free;
        end;
      end;
    end;
  end;
end;

function TZxstRamPage.SaveToStream(const Stream: TStream): Boolean;
var
  Str1, Str2: TStream;
  PB: PByte;
  IsCompressed: Boolean;
  P: Integer;

begin
  Result := False;
  case Rec.PageNo of
    0..7:
      begin
        IsCompressed := Rec.Flags and ZXSTRF_COMPRESSED <> 0;
        Rec.Flags := NtoLE(Rec.Flags);

        case Rec.PageNo of
          0, 2, 5:
            P := 2 - Rec.PageNo div 2;
        otherwise
          P := (7 + 2 * Rec.PageNo) div 3;
        end;

        P := P * KB16;

        if Szx.GetMemStr().Size - P >= KB16 then begin
          PB := PByte(Szx.GetMemStr.Memory) + P;
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
      end;
  otherwise
  end;
end;

class function TZxstRamPage.GetBlockIdAsStr: RawByteString;
begin
  Result := 'RAMP';
end;

{ TZxstSpecRegs }

function TZxstSpecRegs.LoadFromStream(const Stream: TStream): Boolean;

const
  BlSz = SizeOf(TRecSpec);

var
  Rec: TRecSpec;
  N: SizeUInt;

begin
  Result := False;

  if BlockSize >= BlSz then begin
    N := BlockSize - BlSz;

    Rec := Default(TRecSpec);
    if Stream.Read(Rec, BlSz) = BlSz then begin
      if N > 0 then
        Stream.Seek(N, TSeekOrigin.soCurrent);
      Szx.State.BorderColour := Rec.Border;
      Szx.State.Ear := (Rec.Chfe shl 2) and 64;
      case Szx.State.SpectrumModel of
        TSpectrumModel.sm128K, TSpectrumModel.smPlus2:
          begin
            Szx.State.Set7ffd(Rec.Ch7ffd);
          end;
      otherwise
        Szx.State.Reset7ffd();
      end;
      // everything else ignored...
      Result := True;
    end;
  end;
end;

function TZxstSpecRegs.SaveToStream(const Stream: TStream): Boolean;
var
  Rec: TRecSpec;

begin
  Rec := Default(TRecSpec);
  Rec.Border := Szx.State.BorderColour;
  Rec.Chfe := (Szx.State.Ear and 64) shr 2;

  case Szx.State.SpectrumModel of
    TSpectrumModel.sm128K, TSpectrumModel.smPlus2:
      ;
  otherwise
    Szx.State.Reset7ffd();
  end;

  Rec.Ch7ffd := Szx.State.MountedRamPage or (Szx.State.MountedRomPage shl 4);
  if Szx.State.ShadowScreenDisplay then
    Rec.Ch7ffd := Rec.Ch7ffd or 8;
  if not Szx.State.PagingEnabled then
    Rec.Ch7ffd := Rec.Ch7ffd or 32;

  BlockSize := SizeOf(Rec);
  Result := WriteBlockSize(Stream)
    and (Stream.Write(Rec, BlockSize) = BlockSize);
end;

class function TZxstSpecRegs.GetBlockIdAsStr: RawByteString;
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

{ TZxstZ80Regs }

function TZxstZ80Regs.LoadFromStream(const Stream: TStream): Boolean;

var
  Rec: TRecZ80Regs;

begin
  Result := False;

  if BlockSize >= SizeOf(Rec) then begin
    Rec := Default(TRecZ80Regs);

    if Stream.Read(Rec, SizeOf(Rec)) = SizeOf(Rec) then begin

      if BlockSize > SizeOf(Rec) then
        Stream.Seek(BlockSize - SizeOf(Rec), TSeekOrigin.soCurrent);

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

      if (Rec.ChFlags and ZXSTZF_EILAST) <> 0 then
        Szx.State.PrefixByte := $FB // EI opcode
      else
        Szx.State.PrefixByte := 0;

      Szx.State.Halt := (Rec.ChFlags and ZXSTZF_HALTED) <> 0;
      Szx.State.FlagsModified := (Rec.ChFlags and ZXSTZF_FSET) <> 0;
      Szx.State.WZ := LEtoN(Rec.MemPtr);

      Result := True;
    end;
  end;

end;

function TZxstZ80Regs.SaveToStream(const Stream: TStream): Boolean;
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
  if Szx.State.PrefixByte <> 0 then
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

class function TZxstZ80Regs.GetBlockIdAsStr: RawByteString;
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

procedure TSnapshotSZX.SetMemSize(ASize: Integer);
begin
  if Mem = nil then
    Mem := TMemoryStream.Create;

  if Mem.Size <> ASize then
    Mem.Size := ASize;
end;

procedure TSnapshotSZX.RaiseSnapshotLoadErrorSZX(const S: AnsiString);
var
  Msg: AnsiString;
begin
  Msg := 'SZX snapshot load error';
  if S <> '' then
    Msg := Msg + LineEnding + LineEnding + S;
  RaiseSnapshotLoadError(Msg);
end;

class procedure TSnapshotSZX.Init;

  procedure AddBlockClass(BlockClass: TSzxBlockClass);
  begin
    SzxBlocksMap.Add(BlockClass.GetBlockId, BlockClass);
  end;

begin
  FSkipJoystickInfoLoad := True;
  FOnSzxLoadTape := nil;
  FSaveTapeOptions := TSzxSaveTapeOptions.sstoSkip;

  SzxBlocksMap := TSzxBlocksMap.Create;

  AddBlockClass(TZxstCreator);
  AddBlockClass(TZxstZ80Regs);
  AddBlockClass(TZxstSpecRegs);
  AddBlockClass(TZxstRamPage);
  AddBlockClass(TZxstKeyboard);
  AddBlockClass(TZxstTape);
  AddBlockClass(TZxstAYBlock);
  AddBlockClass(TZxstROM);
end;

class procedure TSnapshotSZX.Final;
begin
  SzxBlocksMap.Free;
end;

constructor TSnapshotSZX.Create;
begin
  inherited Create;

  IsIssue2 := False;
  JoystickAttached := False;
  State := Default(TSpectrumInternalState);
  Mem := nil;
  RomStream := nil;
end;

destructor TSnapshotSZX.Destroy;
begin
  Mem.Free;
  RomStream.Free;

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
        if SzxHeader.DwMagic = TZxstHeadr.GetUMagic() then begin

          State.SpectrumModel := TSpectrumModel.smNone;
          State.LateTimings := (SzxHeader.ChFlags and TZxstHeadr.ZXSTMF_ALTERNATETIMINGS) <> 0;

          case SzxHeader.MachineId of
            TZxstHeadr.ZXSTMID_16K:
              begin
                SetMemSize(KB16);
                State.SpectrumModel := TSpectrumModel.sm16K_issue_3;
              end;

            TZxstHeadr.ZXSTMID_48K:
              begin
                SetMemSize(KB48);
                State.SpectrumModel := TSpectrumModel.sm48K_issue_3;
              end;

            TZxstHeadr.ZXSTMID_128K:
              begin
                SetMemSize(KB128);
                State.SpectrumModel := TSpectrumModel.sm128K;
              end;

            TZxstHeadr.ZXSTMID_PLUS2:
              begin
                SetMemSize(KB128);
                State.SpectrumModel := TSpectrumModel.smPlus2;
              end;

            // Models not yet supported, but most likely will be:
            TZxstHeadr.ZXSTMID_PLUS2A, TZxstHeadr.ZXSTMID_PLUS3,

            // Models which are not likely to ever be supported in Swan...
            TZxstHeadr.ZXSTMID_PLUS3E, TZxstHeadr.ZXSTMID_PENTAGON128,
            TZxstHeadr.ZXSTMID_TC2048, TZxstHeadr.ZXSTMID_TC2068, TZxstHeadr.ZXSTMID_SCORPION,
            TZxstHeadr.ZXSTMID_SE, TZxstHeadr.ZXSTMID_TS2068, TZxstHeadr.ZXSTMID_PENTAGON512,
            TZxstHeadr.ZXSTMID_PENTAGON1024, TZxstHeadr.ZXSTMID_NTSC48K,
            TZxstHeadr.ZXSTMID_128KE:
              RaiseSnapshotLoadErrorSZX(Format('Model not supported (id %d).', [SzxHeader.MachineId]));
          otherwise
            RaiseSnapshotLoadErrorSZX(Format('Model unknown (id %d).', [SzxHeader.MachineId]));
          end;

          Result := State.SpectrumModel <> TSpectrumModel.smNone;
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
  Result := False;
  Stream.Position := 0;

  FreeAndNil(RomStream);
  Self.State := Default(TSpectrumInternalState); // must be above loadheader, because of model setting, as well as late timings
  if LoadHeader() then begin
    FillChar(GetMemStr().Memory^, GetMemStr().Size, 0);
    IsIssue2 := FSpectrum.IsIssue2;
    JoystickAttached := TJoystick.Joystick.Enabled;
    JoystickType := TJoystick.Joystick.JoystickType;

    try
      while LoadBlock() do
        if Stream.Position = Stream.Size then begin

          if IsIssue2 then begin
            case State.SpectrumModel of
              TSpectrumModel.sm16K_issue_3:
                State.SpectrumModel := TSpectrumModel.sm16K_issue_2;
              TSpectrumModel.sm48K_issue_3:
                State.SpectrumModel := TSpectrumModel.sm48K_issue_2;
            otherwise
            end;
          end;

          if State.SaveToSpectrum(FSpectrum, RomStream) then begin
            if not FSkipJoystickInfoLoad then begin // otherwise ignore joystick info from szx
              TJoystick.Joystick.Enabled := JoystickAttached;
              if JoystickAttached then
                TJoystick.Joystick.JoystickType := JoystickType;
            end;

            GetMemStr().Position := 0;
            Result := FSpectrum.Memory.LoadRamFromStream(GetMemStr());
          end;

          Break;
        end;

    finally
      FreeAndNil(RomStream);
    end;
  end;

end;

function TSnapshotSZX.SaveToStream(const Stream: TStream): Boolean;

var
  SzxHeader: TZxstHeadr;

  function SaveHeader: Boolean;
  begin
    //
    SzxHeader.DwMagic := TZxstHeadr.GetUMagic;
    SzxHeader.MajorVer := 1;
    SzxHeader.MinorVer := 5;
    case State.SpectrumModel of
      TSpectrumModel.sm16K_issue_2, TSpectrumModel.sm16K_issue_3:
        SzxHeader.MachineId := TZxstHeadr.ZXSTMID_16K;
      TSpectrumModel.sm48K_issue_2, TSpectrumModel.sm48K_issue_3:
        SzxHeader.MachineId := TZxstHeadr.ZXSTMID_48K;
      TSpectrumModel.sm128K:
        SzxHeader.MachineId := TZxstHeadr.ZXSTMID_128K;
      TSpectrumModel.smPlus2:
        SzxHeader.MachineId := TZxstHeadr.ZXSTMID_PLUS2;
    otherwise
      Exit(False);
    end;
    SzxHeader.ChFlags := 0;
    if State.LateTimings then
      SzxHeader.ChFlags := SzxHeader.ChFlags or TZxstHeadr.ZXSTMF_ALTERNATETIMINGS;

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

  function SaveRom(): Boolean;
  begin
    Result := False;

    FreeAndNil(RomStream);
    RomStream := TMemoryStream.Create;
    try
      RomStream.Position := 0;
      Result := FSpectrum.Memory.SaveRomToStream(RomStream)
        and SaveBlock(TZxstROM.Create);
    finally
      FreeAndNil(RomStream);
    end;
  end;

var
  BlockRP: TZxstRamPage;
  I, J: Integer;
  TapePlayer: TTapePlayer;

begin
  Result := False;

  if State.LoadFromSpectrum(FSpectrum, True) then begin
    SetMemSize(FSpectrum.Memory.RamSizeKB * TCommonSpectrum.KiloByte);
    GetMemStr.Position := 0;
    if FSpectrum.Memory.SaveRamToStream(GetMemStr()) then begin
      if SaveHeader then begin
        IsIssue2 := FSpectrum.IsIssue2;
        JoystickAttached := TJoystick.Joystick.Enabled;
        JoystickType := TJoystick.Joystick.JoystickType;

        if SaveBlock(TZxstCreator.Create)
          and SaveBlock(TZxstZ80Regs.Create)
          and SaveBlock(TZxstSpecRegs.Create)
          and SaveBlock(TZxstKeyboard.Create)
        then begin
          case FSpectrum.Memory.RamSizeKB of
            16:
              J := 0;
            48:
              J := 2;
          otherwise
            J := 7;
          end;

          for I := 0 to J do begin
            BlockRP := TZxstRamPage.Create;
            case I of
              0..2:
                BlockRP.Rec.PageNo := (10 - 5 * I) div 2;
            otherwise
              BlockRP.Rec.PageNo := (3 * I) div 2 - 3;
            end;
            BlockRP.Rec.Flags := TZxstRamPage.ZXSTRF_COMPRESSED;
            if not SaveBlock(BlockRP) then
              Exit;
          end;

          if FSpectrum.CustomRomsMounted and (not SaveRom()) then
            Exit;

          if State.HasAy and (not SaveBlock(TZxstAYBlock.Create)) then
            Exit;

          if (FSaveTapeOptions <> TSzxSaveTapeOptions.sstoSkip) and Assigned(FOnSzxSaveTape) then begin
            FOnSzxSaveTape(TapePlayer);
            if Assigned(TapePlayer) then begin
              if (FSaveTapeOptions <> TSzxSaveTapeOptions.sstoFilePathOnly)
                or (TapePlayer.IsRealPath) // file wasn't loaded from zip or from another szx where it had been embedded
              then
                if not SaveBlock(TZxstTape.Create(TapePlayer)) then
                  Exit;
            end;
          end;

          Result := True;
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

  Result := Stream.Write(BlSize, 4) = 4;
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

