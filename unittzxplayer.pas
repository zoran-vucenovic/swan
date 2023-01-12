unit UnitTzxPlayer;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitSpectrum, fgl, UnitFileSna, UnitCommon;

type                          

  TTapeType = (ttTap, ttTzx);

  TTzxPlayer = class;

  TTzxBlock = class abstract (TObject)
  strict protected
    type
      TPlayState = (psStart, psPilot, psSync1, psSync2, psData, psPause, psFinished);
  strict protected
    State: TPlayState;
    FTzxPlayer: TTzxPlayer;

    class function ReadThreeBytes(const Stream: TStream; out N: Int32): Boolean; static;

  private
    function GetStopPlaying: Boolean; virtual;
    function GetRelativeJumpValue: Integer; virtual;
    function GetNumberOfRepetitions: Integer; virtual;
    // the following two are for call sequences (block $26):
    function GetNumOfCalls: Integer; virtual;
    function GetCallBlockNumber(const {%H-}I: Integer): Integer; virtual;
    function CheckReturnFromCallSequence: Boolean; virtual;
    function CheckLoopEnd: Boolean; virtual;

    function LoadBlock(const Stream: TStream): Boolean; virtual; abstract;
    function GetNextPulse(): Boolean; virtual;
    procedure Start; virtual;
  public
    constructor Create(ATzxPlayer: TTzxPlayer); virtual;
    class function GetBlockId: Integer; virtual; abstract;
    {returns whole block length, without id byte}
    function GetBlockLength: Integer; virtual; abstract;

    class function GetBlockDescription: String; virtual; abstract;

    procedure Details(out S: String); virtual;
  end;

  TTzxPlayer = class(TSpectrum.TAbstractTapePlayer)
  strict private
    type
      TTzxBlockClass = class of TTzxBlock;

      TTzxBlocksMap = class(specialize TFPGMap<Byte, TTzxBlockClass>)
      public
        constructor Create;
      end;

      TLoopRec = record
        StartBlockNumber: Integer;
        LoopCount: Integer;
      end;
      TLoopArray = array of TLoopRec;

      TCallSeq = array of Integer;

  strict private
    class var
      TzxBlocksMap: TTzxBlocksMap;

  strict private
    Blocks: array of TTzxBlock;
    FBlockCount: Integer;
    FCurrentBlockNumber: Integer;
    FCurrentBlock: TTzxBlock;
    FFileName: String;
    FLoopArray: TLoopArray;
    FCallSeq: TCallSeq;
    FCallSeqCount: Integer;
    FLoopArrayCount: Integer;
    FOnChangeBlock: TProcedureOfObject;
    FTapeType: TTapeType;
    FPauseBlock: TTzxBlock;

    procedure ClearBlocks;
    procedure CheckNextBlock();

    procedure DoOnChangeBlock inline;
    procedure SetOnChangeBlock(AValue: TProcedureOfObject);
    procedure StartBlock(BlockNumber: Integer);
    procedure EmptyProcedure;
    
  private
    ActiveBit: Byte;
    FSpectrum: TSpectrum;

    procedure StartPauseBlock(const APauseLength: Integer);

    class procedure Init;
    class procedure Final;

  public
    constructor Create;
    destructor Destroy; override;
    procedure SetSpectrum(Spectrum: TSpectrum);
    function LoadFromStream(const Stream: TStream): Boolean;

    procedure Continue;
    procedure Rewind;
    procedure StopPlaying();

    procedure GetNextPulse(); override;
    function GetBlockCount: Integer;
    function GetCurrentBlockNumber: Integer;
    function IsPlaying: Boolean;
    function GetBlock(const I: Integer): TTzxBlock;
    procedure IncBlock(const IncBy: Integer);

    property TapeType: TTapeType {read FTapeType} write FTapeType;
    property FileName: String read FFileName write FFileName;
    property OnChangeBlock: TProcedureOfObject read FOnChangeBlock write SetOnChangeBlock;
  end;

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

constructor {TTzxPlayer.}TTzxBlock.Create(ATzxPlayer: TTzxPlayer);
begin
  inherited Create;

  FTzxPlayer := ATzxPlayer;
  State := psFinished;
end;

procedure {TTzxPlayer.}TTzxBlock.Details(out S: String);
begin
  S := '';
end;

procedure {TTzxPlayer.}TTzxBlock.Start;
begin
  State := TPlayState.psFinished;
  FTzxPlayer.InPause := False;
end;

function {TTzxPlayer.}TTzxBlock.GetStopPlaying: Boolean;
begin
  Result := False;
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

function TTzxBlock.GetNextPulse: Boolean;
begin
  Result := False;
end;

constructor TTzxPlayer.TTzxBlocksMap.Create;
begin
  inherited Create;

  Sorted := True;
  Duplicates := TDuplicates.dupIgnore;
end;
                
{$i tzxblocks.inc}

{ TTzxPlayer }

class procedure TTzxPlayer.Init;

  procedure AddBlockClass(BlockClass: TTzxBlockClass);
  begin
    TzxBlocksMap.Add(BlockClass.GetBlockId, BlockClass);
  end;

begin
  TzxBlocksMap := TTzxBlocksMap.Create;

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

procedure TTzxPlayer.ClearBlocks;
begin                      
  Rewind;
  while FBlockCount > 0 do begin
    Dec(FBlockCount);
    Blocks[FBlockCount].Free;
  end;
end;

procedure TTzxPlayer.CheckNextBlock;

  function JumpToBlock: Boolean;
  var
    N: Integer;
  begin
    N := FCurrentBlock.GetRelativeJumpValue;
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
      FCallSeqCount := FCurrentBlock.GetNumOfCalls;
      if FCallSeqCount > 0 then begin
        if Length(FCallSeq) <= FCallSeqCount then
          SetLength(FCallSeq, (FCallSeqCount * 7) div 5 + 3);
        J := 0;
        FCallSeq[0] := FCurrentBlockNumber + 1;
        for I := FCallSeqCount downto 1 do begin
          FCallSeq[I] := FCurrentBlockNumber + FCurrentBlock.GetCallBlockNumber(J);
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
    if FCurrentBlock.CheckReturnFromCallSequence and (FCallSeqCount > 0) then begin
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
    if FCurrentBlock.CheckLoopEnd then begin
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
      N := FCurrentBlock.GetNumberOfRepetitions;
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
  end else if not (
     JumptoBlock
     or AddCallSeq
     or ReturnFromSequence
     )
  then begin
    LoopCheck;
    StartBlock(FCurrentBlockNumber + 1);
  end;

  if (FCurrentBlock = nil) and (ActiveBit <> 0) then
    StartPauseBlock(0);
end;

procedure TTzxPlayer.DoOnChangeBlock;
begin
  FOnChangeBlock();
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

procedure TTzxPlayer.StopPlaying();
begin
  FCurrentBlock := nil;
  ActiveBit := 0;
  if Assigned(FSpectrum) then
    FSpectrum.SetEarFromTape(0);
  DoOnChangeBlock;
end;

procedure TTzxPlayer.SetOnChangeBlock(AValue: TProcedureOfObject);
begin
  if AValue = nil then
    FOnChangeBlock := @EmptyProcedure
  else
    FOnChangeBlock := AValue;
end;

constructor TTzxPlayer.Create;
begin                    
  inherited Create;

  FTapeType := ttTzx;
  FSpectrum := nil;
  SetOnChangeBlock(nil);

  SetLength(FCallSeq, 5);
  SetLength(FLoopArray, 5);
  SetLength(Blocks, 2);

  FPauseBlock := nil;
  FBlockCount := 0;

  Rewind;
end;

destructor TTzxPlayer.Destroy;
begin
  SetOnChangeBlock(nil);
  ClearBlocks;
  FreeAndNil(FPauseBlock);

  inherited Destroy;
end;

procedure TTzxPlayer.SetSpectrum(Spectrum: TSpectrum);
begin
  StopPlaying();
  FSpectrum := Spectrum;
end;

function TTzxPlayer.LoadFromStream(const Stream: TStream): Boolean;

  function AddBlock(): Boolean;
  var
    BL: TTzxBlock;
    C: TTzxBlockClass;
    B: Byte;
  begin
    Result := False;
    //
    if Stream.Size > Stream.Position then begin
      C := nil;
      if FTapeType = ttTap then
        C := TTzxTapBlock
      else if (Stream.Read(B{%H-}, 1) = 1) then begin
        if not TzxBlocksMap.TryGetData(B, C) then
          C := TTzxBlockUnsupported;
      end;

      if Assigned(C) then begin
        BL := C.Create(Self);
        try
          if BL.LoadBlock(Stream) then begin
            if Length(Blocks) <= FBlockCount then
              SetLength(Blocks, FBlockCount * 7 div 5 + 2);
            Blocks[FBlockCount] := BL;
            Inc(FBlockCount);
            Result := True;
          end;
        finally
          if not Result then
            BL.Free;
        end;
      end;
    end;
  end;

  function CheckTzxHeader: Boolean;
  var
    S: RawByteString;
  begin
    if FTapeType = ttTzx then begin
      Result := False;
      if Stream.Size - Stream.Position > 10 then begin // tzx header size
        SetLength(S{%H-}, 10);
        if (Stream.Read(S[1], 10) = 10)
           and (Copy(S, 1, 8) = 'ZXTape!' + #$1A)
        then
          Result := True;
      end;
    end else
      Result := True;
  end;

begin
  Stream.Position := 0;

  if CheckTzxHeader then
    while AddBlock do
      if Stream.Position = Stream.Size then
        Exit(True);

  Result := False;
end;

procedure TTzxPlayer.StartBlock(BlockNumber: Integer);
begin
  if (BlockNumber < 0) then
    BlockNumber := 0;

  if BlockNumber < FBlockCount then begin
    FCurrentBlockNumber := BlockNumber;
    FCurrentBlock := Blocks[FCurrentBlockNumber];

    FCurrentBlock.Start;
  end else begin
    FCurrentBlock := nil;
  end;
  DoOnChangeBlock;
end;

procedure TTzxPlayer.EmptyProcedure;
begin
  //
end;

procedure TTzxPlayer.Continue;
begin
  if FSpectrum = nil then begin
    StopPlaying();
    Exit;
  end;
  if IsPlaying then
    Exit;
  if FCurrentBlockNumber >= FBlockCount then
    Rewind;
  ActiveBit := 0;

  StartBlock(FCurrentBlockNumber);
  if Assigned(FCurrentBlock) and FCurrentBlock.GetStopPlaying then
    StartBlock(FCurrentBlockNumber + 1);
end;

procedure TTzxPlayer.Rewind;
begin
  FCallSeqCount := 0;
  FLoopArrayCount := 0;
  FCurrentBlockNumber := -1;
  StopPlaying();
end;

procedure TTzxPlayer.GetNextPulse();
begin
  while Assigned(FCurrentBlock) do begin
    if FCurrentBlock.GetNextPulse() then begin
      FSpectrum.SetEarFromTape(ActiveBit);
      Exit;
    end;
    CheckNextBlock();
  end;
end;

function TTzxPlayer.GetBlockCount: Integer;
begin
  Result := FBlockCount;
end;

function TTzxPlayer.GetCurrentBlockNumber: Integer;
begin
  if (FCurrentBlockNumber < 0) or (FCurrentBlockNumber >= FBlockCount) then begin
    FCurrentBlockNumber := -1;
    Exit(0);
  end;
  Result := FCurrentBlockNumber;
end;

function TTzxPlayer.IsPlaying: Boolean;
begin
  Result := FCurrentBlock <> nil;
end;

function TTzxPlayer.GetBlock(const I: Integer): TTzxBlock;
begin
  if (I >= 0) and (I < FBlockCount) then
    Result := Blocks[I]
  else
    Result := nil;
end;

procedure TTzxPlayer.IncBlock(const IncBy: Integer);
begin
  case IncBy of
    -1, 1:
      begin
        StopPlaying();
        FCurrentBlockNumber := GetCurrentBlockNumber + IncBy;
        if IncBy < 0 then
          if FCurrentBlockNumber < 0 then
            FCurrentBlockNumber := FBlockCount - 1;
        DoOnChangeBlock;
      end;
  otherwise
  end;
end;

initialization
  TTzxPlayer.Init;

finalization
  TTzxPlayer.Final;

end.

