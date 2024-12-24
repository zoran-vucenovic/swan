unit UnitTapePlayer;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fgl, UnitSpectrum, UnitCommon;

type
  TTapeType = (ttTap, ttTzx, ttPzx, ttCsw);

  TTapePlayer = class;
  TTapePlayerClass = class of TTapePlayer;

  { TTapeBlock }

  TTapeBlock = class abstract (TObject)
  strict protected
    FTapePlayer: TTapePlayer;

    procedure AdjustTicksIfNeeded(var NeededTicks: Int64);
    function GetCurrentTotalSpectrumTicks: Int64; inline;

  protected
    function GetTicksNextEdge: Int64; virtual;
    function IsReallyPlayableBlock: Boolean; virtual;

  public
    constructor Create(ATapePlayer: TTapePlayer); virtual;
    {returns whole block length, without id byte}
    function GetBlockLength: Integer; virtual; abstract;
    class function GetBlockDescription: String; virtual; abstract;

    function LoadBlock(const Stream: TStream): Boolean; virtual; abstract;
    function GetNextPulse(): Boolean; virtual;
    function GetStopPlaying: Boolean; virtual;

    procedure Start; virtual;
    procedure Details(out S: String); virtual;
    class function GetBlockId: DWord; virtual; abstract;
    class function GetBlockIdAsString: String; virtual; abstract;
  end;

  { TTapePlayer }

  TTapePlayer = class abstract (TSpectrum.TAbstractTapePlayer)
  public
  // used only in tzx block 28
    type
      TSelection = record
        RelativeOffset: Int16;
        DescriptionText: AnsiString;
      end;

      TSelections = array of TSelection;

      TOnSelectBlock = function(const ASelections: TSelections): Boolean of object;

  strict private
    FOnChangeBlock: TProcedureOfObject;
    FOnSelectBlock: TOnSelectBlock;
    FBlockCount: Integer;
    FFileName: String;

    procedure EmptyProcedure;
    procedure SetOnChangeBlock(AValue: TProcedureOfObject);
    procedure ClearBlocks;
  protected
    type
      TTapeBlockClass = class of TTapeBlock;

      TTapeBlocksMap = class(specialize TFPGMap<DWord, TTapeBlockClass>)
      public
        constructor Create;
      end;

      TTapePlayerMap = class(specialize TFPGMap<TTapeType, TTapePlayerClass>)
      public
        constructor Create;
      end;

  protected
    Blocks: array of TTapeBlock;
    FCurrentBlockNumber: Integer;
    FCurrentBlock: TTapeBlock;
    FStream: TMemoryStream;

    procedure DoOnChangeBlock; inline;
    procedure StartBlock(BlockNumber: Integer);
    class function CheckHeader(const {%H-}Stream: TStream): Boolean; virtual;
    procedure CheckNextBlock(); virtual; abstract;
    function AddBlock(const Stream: TStream): Boolean;

    class function GetTapeType: TTapeType; virtual; abstract;

    class procedure RegisterTapePlayerClass(TapePlayerClass: TTapePlayerClass); static;
    class function GetNextBlockClass(const {%H-}Stream: TStream): TTapeBlockClass; virtual;
    class function CheckIsMyClass(const Stream: TStream): Boolean; virtual; abstract;

  private
    class var
      TapePlayerMap: TTapePlayerMap;

  private
    FIsRealPath: Boolean;
    FSpectrum: TSpectrum;
    FLastReallyPlayableBlock: Integer;
    FNoMoreReallyPlayableBlocks: Boolean;

    class procedure Init;
    class procedure Final;
  public
    ActiveBit: Byte;

    constructor Create; virtual;
    destructor Destroy; override;

    function GetLastReallyPlayableBlock: Integer;
    function NoMoreReallyPlayableBlocks: Boolean;
    function LoadFromStream(const AStream: TStream): Boolean; //virtual;
    function SaveToStream(const AStream: TStream): Boolean;
    procedure SetSpectrum(Spectrum: TSpectrum);
    function GetSpectrum: TSpectrum;
    procedure GetNextPulse; override;
    function GetTicksNextEdge: Int64; override;
    procedure Continue;
    procedure Rewind; virtual;
    procedure StopPlaying();
    procedure StartPauseBlock(const {%H-}APauseLength: Integer); virtual;
    function IsPlaying: Boolean;
    function GetCurrentBlockNumber: Integer;
    function GetBlock(const I: Integer): TTapeBlock;
    procedure IncBlock(const IncBy: Integer);
    procedure GoToBlock(const FBlockToGoTo: Integer);
    class function GetTapePlayerClassFromType(const TapeType: TTapeType): TTapePlayerClass; static;
    class function GetTapePlayerClassFromExtension(Extension: String): TTapePlayerClass;
    class function GetDefaultExtension: String;
    class function CheckRealTapePlayerClass(const Stream: TStream): TTapePlayerClass;

    function GetBlockCount: Integer;
    property FileName: String read FFileName write FFileName;
    property IsRealPath: Boolean read FIsRealPath write FIsRealPath;
    property OnChangeBlock: TProcedureOfObject read FOnChangeBlock write SetOnChangeBlock;
    property OnSelectBlock: TOnSelectBlock read FOnSelectBlock write FOnSelectBlock;
  end;

implementation

{ TTapePlayer.TTapePlayerMap }

constructor TTapePlayer.TTapePlayerMap.Create;
begin
  inherited Create;

  Sorted := True;
  Duplicates := TDuplicates.dupIgnore;
end;

{ TTapePlayer.TTapeBlocksMap }

constructor TTapePlayer.TTapeBlocksMap.Create;
begin
  inherited Create;

  Sorted := True;
  Duplicates := TDuplicates.dupIgnore;
end;

{ TTapeBlock }

function TTapeBlock.GetStopPlaying: Boolean;
begin
  Result := False;
end;

function TTapeBlock.GetNextPulse: Boolean;
begin
  Result := False;
end;

constructor TTapeBlock.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create;
  FTapePlayer := ATapePlayer;
end;

procedure TTapeBlock.AdjustTicksIfNeeded(var NeededTicks: Int64);
begin
  // When playing tape on 128K model, adjust pulse duration (just always assume that 48K wrote the file)
  if FTapePlayer.GetSpectrum.Is128KModel then
    NeededTicks := (NeededTicks * 5067 + 2500) div 5000;
end;

function TTapeBlock.GetCurrentTotalSpectrumTicks: Int64;
begin
  Result := FTapePlayer.FSpectrum.GetTotalTicks();
end;

function TTapeBlock.GetTicksNextEdge: Int64;
begin
  Result := Int64.MinValue;
end;

function TTapeBlock.IsReallyPlayableBlock: Boolean;
begin
  Result := False;
end;

procedure TTapeBlock.Start;
begin
  FTapePlayer.InPause := False;
end;

procedure TTapeBlock.Details(out S: String);
begin
  S := '';
end;

{ TTapePlayer }

function TTapePlayer.GetBlockCount: Integer;
begin
  Result := FBlockCount;
end;

procedure TTapePlayer.EmptyProcedure;
begin
  //
end;

procedure TTapePlayer.SetOnChangeBlock(AValue: TProcedureOfObject);
begin
  if AValue = nil then
    FOnChangeBlock := @EmptyProcedure
  else
    FOnChangeBlock := AValue;
end;

procedure TTapePlayer.ClearBlocks;
begin
  Rewind;
  while FBlockCount > 0 do begin
    Dec(FBlockCount);
    Blocks[FBlockCount].Free;
  end;
end;

function TTapePlayer.NoMoreReallyPlayableBlocks: Boolean;
begin
  Result := FNoMoreReallyPlayableBlocks;
end;

procedure TTapePlayer.DoOnChangeBlock;
begin
  FOnChangeBlock();
end;

constructor TTapePlayer.Create;
begin
  inherited Create;

  FStream := nil;
  FIsRealPath := False;
  SetLength(Blocks, 2);
  FBlockCount := 0;
  FOnSelectBlock := nil;
  SetOnChangeBlock(nil);
  FSpectrum := nil;
end;

procedure TTapePlayer.StartBlock(BlockNumber: Integer);
begin
  FNoMoreReallyPlayableBlocks := False;
  if (BlockNumber < 0) then
    BlockNumber := 0;

  if BlockNumber < FBlockCount then begin
    FCurrentBlockNumber := BlockNumber;
    FCurrentBlock := Blocks[FCurrentBlockNumber];

    FCurrentBlock.Start;
    if FCurrentBlockNumber > FLastReallyPlayableBlock then begin
      FNoMoreReallyPlayableBlocks := True;
    end;
  end else begin
    FCurrentBlock := nil;
    FNoMoreReallyPlayableBlocks := True;
  end;
  DoOnChangeBlock;
end;

class function TTapePlayer.CheckHeader(const Stream: TStream): Boolean;
begin
  Result := True;
end;

function TTapePlayer.AddBlock(const Stream: TStream): Boolean;
var
  BL: TTapeBlock;
  C: TTapeBlockClass;
begin
  Result := False;
  //
  if Stream.Size > Stream.Position then begin
    C := GetNextBlockClass(Stream);

    if Assigned(C) then begin
      BL := C.Create(Self);
      try
        if BL.LoadBlock(Stream) then begin
          if Length(Blocks) <= FBlockCount then
            SetLength(Blocks, FBlockCount * 7 div 5 + 2);
          Blocks[FBlockCount] := BL;
          if BL.IsReallyPlayableBlock then
            FLastReallyPlayableBlock := FBlockCount;
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

class procedure TTapePlayer.RegisterTapePlayerClass(
  TapePlayerClass: TTapePlayerClass);
begin
  if TapePlayerMap = nil then
    TapePlayerMap := TTapePlayerMap.Create;
  TapePlayerMap.Add(TapePlayerClass.GetTapeType, TapePlayerClass);
end;

class function TTapePlayer.GetNextBlockClass(const Stream: TStream
  ): TTapeBlockClass;
begin
  Result := nil;
end;

class procedure TTapePlayer.Init;
begin
  TapePlayerMap := nil;
end;

class procedure TTapePlayer.Final;
begin
  TapePlayerMap.Free;
end;

destructor TTapePlayer.Destroy;
begin
  SetOnChangeBlock(nil);
  ClearBlocks;
  FStream.Free;

  inherited Destroy;
end;

function TTapePlayer.GetLastReallyPlayableBlock: Integer;
begin
  Result := FLastReallyPlayableBlock;
end;

function TTapePlayer.LoadFromStream(const AStream: TStream): Boolean;
begin
  Result := False;
  AStream.Position := 0;
  FLastReallyPlayableBlock := -1;

  if CheckHeader(AStream) then
    while AddBlock(AStream) do
      if AStream.Position = AStream.Size then begin
        if FStream = nil then
          FStream := TMemoryStream.Create;
        FStream.Size := AStream.Size;
        AStream.Position := 0;
        Result := AStream.Read(FStream.Memory^, FStream.Size) = AStream.Size;
        Break;
      end;
end;

function TTapePlayer.SaveToStream(const AStream: TStream): Boolean;
begin
  Result := Assigned(AStream)
    and ((FStream = nil) or (FStream.Size = 0)
        or (AStream.Write(FStream.Memory^, FStream.Size) = FStream.Size)
        );
end;

procedure TTapePlayer.SetSpectrum(Spectrum: TSpectrum);
begin
  StopPlaying();
  FSpectrum := Spectrum;
end;

function TTapePlayer.GetSpectrum: TSpectrum;
begin
  Result := FSpectrum;
end;

procedure TTapePlayer.GetNextPulse;
begin
  while Assigned(FCurrentBlock) do begin
    if FCurrentBlock.GetNextPulse() then begin
      FSpectrum.SetEarFromTape(ActiveBit);
      Exit;
    end;
    CheckNextBlock();
  end;
end;

function TTapePlayer.GetTicksNextEdge: Int64;
begin
  if Assigned(FCurrentBlock) then
    Result := FCurrentBlock.GetTicksNextEdge
  else
    Result := Int64.MinValue;
end;

procedure TTapePlayer.Continue;
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

procedure TTapePlayer.Rewind;
begin
  FCurrentBlockNumber := -1;
  StopPlaying();
end;

procedure TTapePlayer.StopPlaying;
begin
  FCurrentBlock := nil;
  ActiveBit := 0;
  if Assigned(FSpectrum) then
    FSpectrum.SetEarFromTape(0);
  DoOnChangeBlock;
end;

procedure TTapePlayer.StartPauseBlock(const APauseLength: Integer);
begin
  if FCurrentBlockNumber >= GetLastReallyPlayableBlock then
    FNoMoreReallyPlayableBlocks := True;
end;

function TTapePlayer.IsPlaying: Boolean;
begin
  Result := FCurrentBlock <> nil;
end;

function TTapePlayer.GetCurrentBlockNumber: Integer;
begin
  if (FCurrentBlockNumber < 0) or (FCurrentBlockNumber >= FBlockCount) then begin
    FCurrentBlockNumber := -1;
    Exit(0);
  end;
  Result := FCurrentBlockNumber;
end;

function TTapePlayer.GetBlock(const I: Integer): TTapeBlock;
begin
  if (I >= 0) and (I < FBlockCount) then
    Result := Blocks[I]
  else
    Result := nil;
end;

procedure TTapePlayer.IncBlock(const IncBy: Integer);
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

procedure TTapePlayer.GoToBlock(const FBlockToGoTo: Integer);
begin
  if (FBlockToGoTo >= 0)
    and (FBlockToGoTo < FBlockCount)
    and (FBlockToGoTo <> FCurrentBlockNumber)
  then begin
    StopPlaying();
    FCurrentBlockNumber := FBlockToGoTo;
    DoOnChangeBlock;
  end;
end;

class function TTapePlayer.GetTapePlayerClassFromType(const TapeType: TTapeType
  ): TTapePlayerClass;
begin
  if (TapePlayerMap = nil) or (not TapePlayerMap.TryGetData(TapeType, Result)) then
    Result := nil;
end;

class function TTapePlayer.GetTapePlayerClassFromExtension(Extension: String
  ): TTapePlayerClass;
var
  I: Integer;
begin
  if Assigned(TapePlayerMap) then begin
    if Extension.StartsWith(ExtensionSeparator, True) then
      Extension := Trim(Copy(Extension, Length(ExtensionSeparator) + 1));
    if Extension <> '' then
      for I := 0 to TapePlayerMap.Count - 1 do begin
        Result := TapePlayerMap.Data[I];
        if AnsiCompareText(Result.GetDefaultExtension, Extension) = 0 then
          Exit;
      end;
  end;

  Result := nil;
end;

class function TTapePlayer.GetDefaultExtension: String;
begin
  WriteStr(Result, GetTapeType);
  Delete(Result, 1, 2);
  Result := LowerCase(Result);
end;

class function TTapePlayer.CheckRealTapePlayerClass(const Stream: TStream
  ): TTapePlayerClass;
var
  I: Integer;
begin
  if Assigned(TapePlayerMap) then
    for I := 0 to TapePlayerMap.Count - 1 do begin
      Result := TapePlayerMap.Data[I];
      if Result.CheckIsMyClass(Stream) then
        Exit;
    end;

  Result := nil;
end;

initialization
  TTapePlayer.Init;

finalization
  TTapePlayer.Final;

end.

