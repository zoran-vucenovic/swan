unit UnitTzxPlayer;
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

// tzx tape format
// tzx format specification: https://worldofspectrum.net/TZXformat.html

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitSnapshotFiles, UnitCommon,
  UnitTapePlayer, UnitCSW;

implementation

uses
  SnapshotZ80, SnapshotSNA; // these are included for tzx block 40

type

  TTzxPlayer = class(TTapePlayer)
  strict private
    type

      TLoopRec = record
        StartBlockNumber: Integer;
        LoopCount: Integer;
      end;
      TLoopArray = array of TLoopRec;

      TCallSeq = array of Integer;

  strict private
    class var
      TzxBlocksMap: TTapeBlocksMap;

  strict private
    FLoopArray: TLoopArray;
    FCallSeq: TCallSeq;
    FCallSeqCount: Integer;
    FLoopArrayCount: Integer;
    FPauseBlock: TTapeBlock;

  protected
    procedure CheckNextBlock(); override;
    class function CheckHeader(const Stream: TStream): Boolean; override;
    class function GetNextBlockClass(const Stream: TStream): TTapeBlockClass;
      override;
    class function GetTapeType: TTapeType; override;
    class function CheckIsMyClass(const Stream: TStream): Boolean; override;

  private
    class procedure Init;
    class procedure Final;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Rewind; override;

    procedure StartPauseBlock(const APauseLength: Integer); override;
  end;

  TTapPlayer = class(TTzxPlayer)
  private
    class procedure Init;
  protected
    class function CheckHeader(const {%H-}Stream: TStream): Boolean; override;
    class function GetNextBlockClass(const {%H-}Stream: TStream): TTapeBlockClass;
      override;
    class function GetTapeType: TTapeType; override;
    class function CheckIsMyClass(const {%H-}Stream: TStream): Boolean; override;
  end;

  TTzxBlock = class abstract (TTapeBlock)
  strict protected
    type
      TPlayState = (psStart, psPilot, psSync1, psSync2, psData, psFinished);

  strict protected
    State: TPlayState;

    class function ReadThreeBytes(const Stream: TStream; out N: Int32): Boolean; static;

  private
    function GetRelativeJumpValue: Integer; virtual;
    function GetNumberOfRepetitions: Integer; virtual;
    // the following two are for call sequences (block $26):
    function GetNumOfCalls: Integer; virtual;
    function GetCallBlockNumber(const {%H-}I: Integer): Integer; virtual;
    function CheckReturnFromCallSequence: Boolean; virtual;
    function CheckLoopEnd: Boolean; virtual;
    function CheckUserInteraction: Boolean; virtual;

  public
    constructor Create(ATapPlayer: TTapePlayer); override;
    procedure Start; override;
    class function GetBlockIdAsString: String; override;
  end;

{$define tzx_header_section}
{$i tzxblocks.inc}
{$undef tzx_header_section}

{ TTzxPlayer.TTzxBlock }

class function TTzxBlock.ReadThreeBytes(const Stream: TStream; out
  N: Int32): Boolean;
var
  B: packed array [0..2] of Byte;
  I: Integer;
begin
  Result := False;

  N := 0;
  if Stream.Size >= Stream.Position + 3 then begin
    if Stream.Read({%H-}B[0], 3) = 3 then begin
      for I := 2 downto 0 do // little endian
        N := (N shl 8) or B[I];

      Result := True;
    end;
  end;
end;

constructor TTzxBlock.Create(ATapPlayer: TTapePlayer);
begin
  inherited Create(ATapPlayer);

  State := TPlayState.psFinished;
end;

procedure TTzxBlock.Start;
begin
  inherited Start;
  State := TPlayState.psFinished;
end;

class function TTzxBlock.GetBlockIdAsString: String;
begin
  if GetBlockId = 0 then
    Result := ''
  else
    Result := '0x' + IntToHex(GetBlockId, 2);
end;

function TTzxBlock.GetRelativeJumpValue: Integer;
begin
  Result := 0;
end;

function TTzxBlock.GetNumberOfRepetitions: Integer;
begin
  Result := 0;
end;

function TTzxBlock.GetNumOfCalls: Integer;
begin
  Result := 0;
end;

function TTzxBlock.GetCallBlockNumber(const I: Integer): Integer;
begin
  Result := 0;
end;

function TTzxBlock.CheckReturnFromCallSequence: Boolean;
begin
  Result := False;
end;

function TTzxBlock.CheckLoopEnd: Boolean;
begin
  Result := False;
end;

function TTzxBlock.CheckUserInteraction: Boolean;
begin
  Result := False;
end;

{$i tzxblocks.inc}

{ TTzxPlayer }

class procedure TTzxPlayer.Init;

  procedure AddBlockClass(BlockClass: TTapeBlockClass);
  begin
    TzxBlocksMap.Add(BlockClass.GetBlockId, BlockClass);
  end;

begin
  RegisterTapePlayerClass(Self);

  TzxBlocksMap := TTapeBlocksMap.Create;

  AddBlockClass(TTzxBlock10);
  AddBlockClass(TTzxBlock11);
  AddBlockClass(TTzxBlock12);
  AddBlockClass(TTzxBlock13);
  AddBlockClass(TTzxBlock14);
  AddBlockClass(TTzxBlock15);
  AddBlockClass(TTzxBlock16);
  AddBlockClass(TTzxBlock17);
  AddBlockClass(TTzxBlock18);
  AddBlockClass(TTzxBlock19);
  AddBlockClass(TTzxBlock20);
  AddBlockClass(TTzxBlock21);
  AddBlockClass(TTzxBlock22);
  AddBlockClass(TTzxBlock23);
  AddBlockClass(TTzxBlock24);
  AddBlockClass(TTzxBlock25);
  AddBlockClass(TTzxBlock26);
  AddBlockClass(TTzxBlock27);
  AddBlockClass(TTzxBlock28);
  AddBlockClass(TTzxBlock2A);
  AddBlockClass(TTzxBlock2B);
  AddBlockClass(TTzxBlock30);
  AddBlockClass(TTzxBlock31);
  AddBlockClass(TTzxBlock32);
  AddBlockClass(TTzxBlock33);
  AddBlockClass(TTzxBlock34);
  AddBlockClass(TTzxBlock35);
  AddBlockClass(TTzxBlock40);
  AddBlockClass(TTzxBlock5A);
end;

class procedure TTzxPlayer.Final;
begin
  TzxBlocksMap.Free;
end;

procedure TTzxPlayer.CheckNextBlock;

var
  BL: TTzxBlock;

  function JumpToBlock: Boolean;
  var
    N: Integer;
  begin
    N := BL.GetRelativeJumpValue;
    if N <> 0 then begin
      N := FCurrentBlockNumber + N;
      if (N >= 0) and (N  < GetBlockCount) then begin
        StartBlock(N);
        Exit(True);
      end;
    end;
    Result := False;
  end;

  function AddCallSeq: Boolean;
  var
    J, I: Integer;
  begin
    Result := False;

    if FCallSeqCount = 0 then begin
      FCallSeqCount := BL.GetNumOfCalls;
      if FCallSeqCount > 0 then begin
        if Length(FCallSeq) <= FCallSeqCount then
          SetLength(FCallSeq, (FCallSeqCount * 7) div 5 + 3);
        J := 0;
        FCallSeq[0] := FCurrentBlockNumber + 1;
        for I := FCallSeqCount downto 1 do begin
          FCallSeq[I] := FCurrentBlockNumber + BL.GetCallBlockNumber(J);
          Inc(J);
        end;
        StartBlock(FCallSeq[FCallSeqCount]);
        Result := True;
      end;
    end;
  end;

  function ReturnFromSequence: Boolean;
  begin
    Result := False;
    if BL.CheckReturnFromCallSequence and (FCallSeqCount > 0) then begin
      Dec(FCallSeqCount);
      StartBlock(FCallSeq[FCallSeqCount]);
      Result := True;
    end;
  end;

  procedure LoopCheck;

    function CheckEmptyLoop(APosition: Integer): Integer;
    var
      B: TTzxBlock;
      N: Integer;
    begin
      N := FCurrentBlockNumber + APosition + 1;
      if N >= GetBlockCount then
        Exit(N);

      B := TTzxBlock(Blocks[N]);
      if B.GetNumberOfRepetitions > 0 then begin
        N := CheckEmptyLoop(APosition + 1);
        if N = 0 then
          Exit(0);

        Inc(N);
        if N >= GetBlockCount then
          Exit(N);
        B := TTzxBlock(Blocks[N]);
      end;
      if B.CheckLoopEnd then
        Exit(N);

      Result := 0;
    end;

  var
    LoopRec: ^TLoopRec;
    N: Integer;
    EL: Integer;
  begin
    if BL.CheckLoopEnd then begin
      // Loop end
      if FLoopArrayCount > 0 then begin
        LoopRec := @(FLoopArray[FLoopArrayCount - 1]);
        if LoopRec^.LoopCount > 1 then begin
          LoopRec^.LoopCount := LoopRec^.LoopCount - 1;
          FCurrentBlockNumber := LoopRec^.StartBlockNumber;
        end else
          Dec(FLoopArrayCount);
      end
    end else begin
      // Loop start
      N := BL.GetNumberOfRepetitions;
      if N > 0 then begin
        EL := CheckEmptyLoop(0);
        if EL > 0 then begin
          if EL > GetBlockCount then
            EL := GetBlockCount;
          FCurrentBlockNumber := EL;
        end else begin
          if Length(FLoopArray) <= FLoopArrayCount then
            SetLength(FLoopArray, (FLoopArrayCount * 7) div 5 + 2);

          LoopRec := @(FLoopArray[FLoopArrayCount]);
          LoopRec^.StartBlockNumber := FCurrentBlockNumber;
          LoopRec^.LoopCount := N;
          Inc(FLoopArrayCount);
        end;
      end;
    end;
  end;

begin
  if FCurrentBlock is TTzxBlock then begin
    BL := TTzxBlock(FCurrentBlock);
    if JumpToBlock
       or AddCallSeq
       or ReturnFromSequence
       or BL.CheckUserInteraction
    then
      Exit;

    LoopCheck;
  end;

  inherited CheckNextBlock();
end;

class function TTzxPlayer.CheckHeader(const Stream: TStream): Boolean;
var
  S: RawByteString;
begin
  Result := False;
  if Stream.Size - Stream.Position > 10 then begin // tzx header size
    SetLength(S{%H-}, 10);
    if (Stream.Read(S[1], 10) = 10)
       and (Copy(S, 1, 8) = 'ZXTape!' + #$1A)
       // This is followed by tzx version (two bytes), but we will just skip it.
       // That is because tzx specification says that all blocks possibly added
       // in future must have the length of the block in first 4 bytes after the
       // block id (so we can "implement" these blocks by just skipping them).
       // So tzx specification can (and does) say that the reader must handle
       // the file, even if it cannot handle all the data in the file.
       // Therefore, we have nothing meaningful to do with the version here.
    then
      Result := True;
  end;
end;

class function TTzxPlayer.GetNextBlockClass(const Stream: TStream
  ): TTapeBlockClass;
var
  B: Byte;
  D: DWord;
begin
  Result := nil;
  if Stream.Read(B{%H-}, 1) = 1 then begin
    D := B;
    if not TzxBlocksMap.TryGetData(D, Result) then
      Result := TTzxBlockUnsupported;
  end;
end;

class function TTzxPlayer.GetTapeType: TTapeType;
begin
  Result := ttTzx;
end;

procedure TTzxPlayer.StartPauseBlock(const APauseLength: Integer);
begin
  inherited StartPauseBlock(APauseLength);

  if FPauseBlock = nil then
    FPauseBlock := TTzxBlockPause.Create(Self);
  TTzxBlockPause(FPauseBlock).SetPauseLen(APauseLength);
  FCurrentBlock := FPauseBlock;
  FPauseBlock.Start;
  DoOnChangeBlock;
end;

class function TTzxPlayer.CheckIsMyClass(const Stream: TStream): Boolean;
var
  P: Int64;
begin
  Result := False;
  P := Stream.Position;
  Result := CheckHeader(Stream);
  Result := (Stream.Seek(P - Stream.Position, TSeekOrigin.soCurrent) = P) and Result;
end;

constructor TTzxPlayer.Create;
begin
  inherited Create;

  SetLength(FCallSeq, 5);
  SetLength(FLoopArray, 5);

  FPauseBlock := nil;

  Rewind;
end;

destructor TTzxPlayer.Destroy;
begin
  FreeAndNil(FPauseBlock);

  inherited Destroy;
end;

procedure TTzxPlayer.Rewind;
begin
  FCallSeqCount := 0;
  FLoopArrayCount := 0;

  inherited Rewind;
end;

{ TTapPlayer }

class procedure TTapPlayer.Init;
begin
  RegisterTapePlayerClass(Self);
end;

class function TTapPlayer.CheckHeader(const Stream: TStream): Boolean;
begin
  Result := True;
end;

class function TTapPlayer.GetNextBlockClass(const Stream: TStream
  ): TTapeBlockClass;
begin
  Result := TTzxTapBlock;
end;

class function TTapPlayer.GetTapeType: TTapeType;
begin
  Result := ttTap;
end;

class function TTapPlayer.CheckIsMyClass(const Stream: TStream): Boolean;
begin
  Result := False;
end;

initialization
  TTzxPlayer.Init;
  TTapPlayer.Init;

finalization
  TTzxPlayer.Final;

end.

