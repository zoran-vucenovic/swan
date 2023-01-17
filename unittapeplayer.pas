unit UnitTapePlayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl, UnitSpectrum, UnitCommon;

type
  TTapeType = (ttTap, ttTzx, ttPzx);

  TTapePlayer = class;
  TTapePlayerClass = class of TTapePlayer;

  TTapeBlock = class abstract (TObject)
  strict protected
    FTapePlayer: TTapePlayer;

  public
    function GetStopPlaying: Boolean; virtual;

    function LoadBlock(const Stream: TStream): Boolean; virtual; abstract;
    function GetNextPulse(): Boolean; virtual;

  public
    constructor Create(ATapePlayer: TTapePlayer); virtual;
    {returns whole block length, without id byte}
    function GetBlockLength: Integer; virtual; abstract;

    class function GetBlockDescription: String; virtual; abstract;
    function GetCurrentTotalSpectrumTicks: Int64; inline;
    procedure Start; virtual;
    procedure Details(out S: String); virtual;
    class function GetBlockId: Integer; virtual; abstract;
    class function GetBlockIdAsString: String; virtual; abstract;
  end;

  TTapePlayer = class(TSpectrum.TAbstractTapePlayer)
  strict private
    procedure EmptyProcedure;
    procedure SetOnChangeBlock(AValue: TProcedureOfObject);
    procedure ClearBlocks;
  strict private
    FOnChangeBlock: TProcedureOfObject;
    FBlockCount: Integer;
    FFileName: String;
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

    procedure DoOnChangeBlock; inline;
    procedure StartBlock(BlockNumber: Integer);
    class function CheckHeader(const {%H-}Stream: TStream): Boolean; virtual;
    procedure CheckNextBlock(); virtual; abstract;
    function AddBlock(const Stream: TStream): Boolean;

    class function GetTapeType: TTapeType; virtual; abstract;

    class procedure RegisterTapePlayerClass(TapePlayerClass: TTapePlayerClass);
    class function GetNextBlockClass(const Stream: TStream): TTapeBlockClass; virtual;
    class function CheckIsMyClass(const Stream: TStream): Boolean; virtual; abstract;

  private
    class var
      TapePlayerMap: TTapePlayerMap;

  private
    FSpectrum: TSpectrum;

    class procedure Init;
    class procedure Final;
  public
    ActiveBit: Byte;

    constructor Create; virtual;
    destructor Destroy; override;

    function LoadFromStream(const Stream: TStream): Boolean; virtual;
    procedure SetSpectrum(Spectrum: TSpectrum);
    function GetSpectrum: TSpectrum;
    procedure GetNextPulse; override;
    procedure Continue;
    procedure Rewind; virtual;
    procedure StopPlaying();
    procedure StartPauseBlock(const {%H-}APauseLength: Integer); virtual;
    function IsPlaying: Boolean;
    function GetCurrentBlockNumber: Integer;
    function GetBlock(const I: Integer): TTapeBlock;
    procedure IncBlock(const IncBy: Integer);
    class function GetTapePlayerClassFromType(const TapeType: TTapeType): TTapePlayerClass; static;
    class function CheckRealTapePlayerClass(const Stream: TStream): TTapePlayerClass;

    function GetBlockCount: Integer;
    property FileName: String read FFileName write FFileName;
    property OnChangeBlock: TProcedureOfObject read FOnChangeBlock write SetOnChangeBlock;
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

{ TTapePlayer.TTapeBlocksMap }

//constructor TTapePlayer.TTapeBlocksMap.Create;
//begin
//  inherited Create;
//
//  Sorted := True;
//  Duplicates := TDuplicates.dupIgnore;
//end;

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

function TTapeBlock.GetCurrentTotalSpectrumTicks: Int64;
begin
  Result := FTapePlayer.FSpectrum.SumTicks + FTapePlayer.FSpectrum.GetProcessor.TStatesInCurrentFrame;
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

procedure TTapePlayer.DoOnChangeBlock;
begin
  FOnChangeBlock();
end;

constructor TTapePlayer.Create;
begin
  inherited Create;

  SetLength(Blocks, 2);
  FBlockCount := 0;
  SetOnChangeBlock(nil);
  FSpectrum := nil;
end;
  
procedure TTapePlayer.StartBlock(BlockNumber: Integer);
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

  inherited Destroy;
end;

function TTapePlayer.LoadFromStream(const Stream: TStream): Boolean;
begin
  Stream.Position := 0;

  if CheckHeader(Stream) then
    while AddBlock(Stream) do
      if Stream.Position = Stream.Size then
        Exit(True);

  Result := False;
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
  //
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

class function TTapePlayer.GetTapePlayerClassFromType(const TapeType: TTapeType
  ): TTapePlayerClass;
begin
  if not TapePlayerMap.TryGetData(TapeType, Result) then
    Result := nil;
end;

class function TTapePlayer.CheckRealTapePlayerClass(const Stream: TStream
  ): TTapePlayerClass;
var
  I: Integer;
  C: TTapePlayerClass;
begin
  for I := 0 to TapePlayerMap.Count - 1 do begin
    C := TapePlayerMap.Data[I];
    if C.CheckIsMyClass(Stream) then
      Exit(C);
  end;
  Result := nil;
end;

initialization
  TTapePlayer.Init;

finalization
  TTapePlayer.Final;

end.
