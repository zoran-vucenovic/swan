unit UnitTzxPlayer;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, UnitFileSna, UnitCommon, UnitTapePlayer;

implementation

const
  TicksPerMilliSecond = Int64(3500);
  //
  // What is the correct duration of oposite level before going to low when entering pause?
  // According to tzx specification, when pause is non-zero, going to pause should be
  // emulated by making an edge, wait AT LEAST one millisecond with the oposite
  // level and then go to low for the rest of the pause period.
  // Quote from https://worldofspectrum.net/TZXformat.html:
  //   To ensure that the last edge produced is properly finished there should be
  //   at least 1 ms. pause of the opposite level and only after that the pulse should go to 'low'
  // For the majority of tzx files I tried (acutally, all I tested, except one),
  // one millisecond (3500 ticks) of oposite level is enough, but not in one case I found:
  //   Pheenix.tzx by Megadodo, original release (https://spectrumcomputing.co.uk/entry/3690/ZX-Spectrum/Pheenix)
  // This tzx file works when inverted level is kept for at least 4214 (does not work with 4213) ticks.
  //
  // Let's put it on 4250 ticks, don't go much higher though, as it might make
  // other tzx files not play well.
  TicksBeforePause = Int64(4250);

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
      TPlayState = (psStart, psPilot, psSync1, psSync2, psData, psPause, psFinished);
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

  public
    constructor Create(ATapPlayer: TTapePlayer); override;
    procedure Start; override;
    class function GetBlockIdAsString: String; override;
  end;

{$define tzx_header_section}
{$i tzxblocks.inc}
{$undef tzx_header_section}

{ TTzxPlayer.TTzxBlock }

class function {TTzxPlayer.}TTzxBlock.ReadThreeBytes(const Stream: TStream; out
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

constructor {TTzxPlayer.}TTzxBlock.Create(ATapPlayer: TTapePlayer);
begin
  inherited Create(ATapPlayer);

  State := psFinished;
end;

procedure {TTzxPlayer.}TTzxBlock.Start;
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

function {TTzxPlayer.}TTzxBlock.GetRelativeJumpValue: Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxBlock.GetNumberOfRepetitions: Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxBlock.GetNumOfCalls: Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxBlock.GetCallBlockNumber(const I: Integer): Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxBlock.CheckReturnFromCallSequence: Boolean;
begin
  Result := False;
end;

function {TTzxPlayer.}TTzxBlock.CheckLoopEnd: Boolean;
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
      StartBlock(FCurrentBlockNumber + N);
      Exit(True);
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
  var
    LoopRec: ^TLoopRec;
    N: Integer;
  begin 
    if BL.CheckLoopEnd then begin
      // Loop end
      if FLoopArrayCount > 0 then begin
        LoopRec := @(FLoopArray[FLoopArrayCount - 1]);
        if LoopRec^.LoopCount > 0 then begin
          LoopRec^.LoopCount := LoopRec^.LoopCount - 1;
          FCurrentBlockNumber := LoopRec^.StartBlockNumber;
        end else
          Dec(FLoopArrayCount);
      end
    end else begin
      // Loop start
      N := BL.GetNumberOfRepetitions;
      if N > 0 then begin
        if Length(FLoopArray) <= FLoopArrayCount then
          SetLength(FLoopArray, (FLoopArrayCount * 7) div 5 + 2);

        LoopRec := @(FLoopArray[FLoopArrayCount]);
        LoopRec^.StartBlockNumber := FCurrentBlockNumber;
        LoopRec^.LoopCount := N;
        Inc(FLoopArrayCount);
      end;
    end;
  end;

begin
  if FCurrentBlock.GetStopPlaying then begin
    StopPlaying();
  end else begin
    BL := TTzxBlock(FCurrentBlock);
    if not (
       JumptoBlock
       or AddCallSeq
       or ReturnFromSequence
       )
    then begin
      LoopCheck;
      StartBlock(FCurrentBlockNumber + 1);
    end;
  end;

  if (FCurrentBlock = nil) and (ActiveBit <> 0) then
    StartPauseBlock(0);
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

