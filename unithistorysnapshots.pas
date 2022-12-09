unit UnitHistorySnapshots;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, fpjson, UnitFileSna, UnitSpectrum, UnitConfigs;

type

  TSnapshotHistoryOptions = record
  public
    const
      MaxSavePeriodInFrames = 3000;
      MaxMaxNumberOfSnapshotsInMemory = 120;
      DefSavePeriodInFrames = 150; // about three seconds
      DefMaxNumberOfSnapshotsInMemory = 40;
  strict private
    const
      cSnapshotHistoryOptions = 'autosaving_snapshots';
      cSavePeriodInFrames = 'save_period_in_frames';
      cMaxNumberOfSnapshotsInMemory = 'max_number_of_snapshots_in_memory';
      cGoBackKey = 'go_back_key';
  private
    procedure LoadFromJSonObject(const JSObj: TJSONObject);
    procedure SaveToJSonObject(const JSObj: TJSONObject);
  public
    SavePeriodInFrames: Integer;
    MaxNumberOfSnapshotsInMemory: Integer;
    KeyGoBack: Word;

    procedure LoadFromConf;
    procedure SaveToConf;
  end;

  TSnapshotHistoryQueue = class
  strict private
    type
      TSnapshotsHistoryElement = class
      private
      public
        Stream: TStream;
        Next: TSnapshotsHistoryElement;
        Prev: TSnapshotsHistoryElement;

        constructor Create;
        destructor Destroy; override;
      end;

  strict private             
    First: TSnapshotsHistoryElement;
    Last: TSnapshotsHistoryElement;
    FSavePeriod: Integer;
    FSpectrum: TSpectrum;
    FramesPassed: Integer; // frames since last save
    FramesDelay: Integer; // when going back, skip the last, go to previous snapshot
    FMaxNumberOfSnapshotsInMemory: Integer;
    FSnap: TSnapshotInternal48;
    FCount: Integer;

    function GetSnap(): TSnapshotInternal48;
    procedure SetSavePeriod(AValue: Integer);
    procedure SetMaxNumberOfSnapshotsInMemory(const AValue: Integer);
    procedure SaveSnapshot();
    procedure SetSpectrum(const ASpectrum: TSpectrum);
    function GetElement(NegativeOffset: Integer): TSnapshotsHistoryElement;

  public

    constructor Create;
    destructor Destroy; override;

    procedure UpdateOptions(const ASnapshotHistoryOptions: TSnapshotHistoryOptions);
    function LoadSnapshot(NegativeOffset: Integer): Boolean;
    function GetStream(NegativeOffset: Integer; out Stream: TStream): Boolean;
    procedure CheckSaveHistorySnapshot;

    property Count: Integer read FCount;
    property Spectrum: TSpectrum read FSpectrum write SetSpectrum;
  end;

implementation

{ TSnapshotHistoryQueue.TSnapshotsHistoryElement }

constructor TSnapshotHistoryQueue.TSnapshotsHistoryElement.Create;
begin
  Stream := TMemoryStream.Create;
  Next := nil;
  Prev := nil;
end;

destructor TSnapshotHistoryQueue.TSnapshotsHistoryElement.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;

{ TSnapshotHistoryOptions }

procedure TSnapshotHistoryOptions.LoadFromJSonObject(const JSObj: TJSONObject);
var
  N: Integer;
begin
  if Assigned(JSObj) then begin
    N := JSObj.Get(cSavePeriodInFrames, SavePeriodInFrames);
    if (N >= 1) and (N <= MaxSavePeriodInFrames) then
      SavePeriodInFrames := N;
    N := JSObj.Get(cMaxNumberOfSnapshotsInMemory, MaxNumberOfSnapshotsInMemory);
    if (N >= 1) and (N <= MaxMaxNumberOfSnapshotsInMemory) then
      MaxNumberOfSnapshotsInMemory := N;
    N := KeyGoBack;
    N := JSObj.Get(cGoBackKey, N);
    if (N > 0) and (N <= KeyGoBack.MaxValue) then
      KeyGoBack := N;
  end;
end;

procedure TSnapshotHistoryOptions.SaveToJSonObject(const JSObj: TJSONObject);
var
  N: Integer;
begin
  if Assigned(JSObj) then begin
    JSObj.Add(cSavePeriodInFrames, SavePeriodInFrames);
    JSObj.Add(cMaxNumberOfSnapshotsInMemory, MaxNumberOfSnapshotsInMemory);
    N := KeyGoBack;
    JSObj.Add(cGoBackKey, N);
  end;
end;

procedure TSnapshotHistoryOptions.LoadFromConf;
begin
  LoadFromJSonObject(TConfJSON.GetJSONObject(cSnapshotHistoryOptions));
end;

procedure TSnapshotHistoryOptions.SaveToConf;
var
  JObj: TJSONObject;
begin
  JObj := TJSONObject.Create;
  try
    SaveToJSonObject(JObj);
    TConfJSON.RemoveSection(cSnapshotHistoryOptions);
    if TConfJSON.AddToConf(cSnapshotHistoryOptions, JObj) then
      JObj := nil;
  finally
    JObj.Free;
  end;
end;

{ TSnapshotHistoryQueue }

function TSnapshotHistoryQueue.GetSnap: TSnapshotInternal48;
begin
  if FSnap = nil then begin
    FSnap := TSnapshotInternal48.Create;
    FSnap.SetSpectrum(FSpectrum);
  end;

  Result := FSnap;
end;

procedure TSnapshotHistoryQueue.SetSavePeriod(AValue: Integer);
begin
  if (AValue >= 1) and (AValue <= TSnapshotHistoryOptions.MaxSavePeriodInFrames) then begin
    FSavePeriod := AValue;
    //FramesDelay := AValue div 3;
  end;
end;

procedure TSnapshotHistoryQueue.SetMaxNumberOfSnapshotsInMemory(
  const AValue: Integer);
var
  Element: TSnapshotsHistoryElement;
begin
  if FMaxNumberOfSnapshotsInMemory <> AValue then begin
    if AValue < 1 then
      FMaxNumberOfSnapshotsInMemory := 1
    else if AValue > TSnapshotHistoryOptions.MaxMaxNumberOfSnapshotsInMemory then
      FMaxNumberOfSnapshotsInMemory := TSnapshotHistoryOptions.MaxMaxNumberOfSnapshotsInMemory
    else
      FMaxNumberOfSnapshotsInMemory := AValue;

    while FCount > FMaxNumberOfSnapshotsInMemory do begin
      Element := First;
      First := First.Next;
      Element.Free;
      Dec(FCount);
    end;

    if First = nil then
      Last := nil
    else
      First.Prev := nil;
  end;
end;

function TSnapshotHistoryQueue.LoadSnapshot(NegativeOffset: Integer): Boolean;
var
  Element: TSnapshotsHistoryElement;

begin
  if (NegativeOffset = 0) and (FramesPassed < FramesDelay) then begin
    if LoadSnapshot(-1) then
      Exit(True);
  end;

  Element := GetElement(NegativeOffset);
  if Assigned(Element) and Assigned(Element.Stream) then begin
    if GetSnap.LoadFromStream(Element.Stream) then begin
      FramesPassed := 0;
      Last := Element;
      FCount := FCount + NegativeOffset;
      Exit(True);
    end;
  end;

  Result := False;
end;

function TSnapshotHistoryQueue.GetStream(NegativeOffset: Integer; out
  Stream: TStream): Boolean;
var
  Element: TSnapshotsHistoryElement;
begin
  Result := False;
  Element := GetElement(NegativeOffset);

  if Assigned(Element) then begin
    Stream := Element.Stream;
    Result := Assigned(Stream);
  end;
end;

procedure TSnapshotHistoryQueue.SaveSnapshot;
var
  Element: TSnapshotsHistoryElement;
begin
  if FCount < FMaxNumberOfSnapshotsInMemory then begin
    if (Last = nil) or (Last.Next = nil) then begin
      Element := TSnapshotsHistoryElement.Create;
      if First = nil then
        First := Element;
    end else
      Element := Last.Next;

    Inc(FCount);
  end else begin
    Element := First;
    First := First.Next;
    Element.Next := nil;
    First.Prev := nil;
  end;

  Element.Prev := Last;
  if Assigned(Last) then
    Last.Next := Element;

  Last := Element;
  GetSnap.SaveToStream(Last.Stream);
end;

procedure TSnapshotHistoryQueue.CheckSaveHistorySnapshot;
begin
  Inc(FramesPassed);
  if (FramesPassed >= FSavePeriod) and (not FSpectrum.GetProcessor.SkipInterruptCheck) then begin
    FramesPassed := 0;
    SaveSnapshot;
  end;
end;

constructor TSnapshotHistoryQueue.Create;
begin
  FSnap := nil;
  First := nil;
  Last := nil;
  FSpectrum := nil;
  FramesPassed := 0;
  FCount := 0;
  FMaxNumberOfSnapshotsInMemory := TSnapshotHistoryOptions.DefMaxNumberOfSnapshotsInMemory;

  FramesDelay := 50; // when less than 50 frames (~1 second) passed since
                        // the latest snapshot, skip that one and go to previous
  SetSavePeriod(TSnapshotHistoryOptions.DefSavePeriodInFrames);
end;

destructor TSnapshotHistoryQueue.Destroy;
begin
  FCount := 0;
  while First <> nil do begin
    Last := First;
    First := First.Next;
    Last.Free;
  end;

  FSnap.Free;
  inherited Destroy;
end;

procedure TSnapshotHistoryQueue.UpdateOptions(
  const ASnapshotHistoryOptions: TSnapshotHistoryOptions);
begin
  SetMaxNumberOfSnapshotsInMemory(ASnapshotHistoryOptions.MaxNumberOfSnapshotsInMemory);
  SetSavePeriod(ASnapshotHistoryOptions.SavePeriodInFrames);
end;

procedure TSnapshotHistoryQueue.SetSpectrum(const ASpectrum: TSpectrum);
begin
  if FSpectrum <> ASpectrum then begin
    FSpectrum := ASpectrum;
        
    FramesPassed := 0;
    if Assigned(ASpectrum) then begin
      FramesPassed := FSavePeriod;
      CheckSaveHistorySnapshot;
    end;
  end;
end;

function TSnapshotHistoryQueue.GetElement(NegativeOffset: Integer
  ): TSnapshotsHistoryElement;
begin
  Result := nil;
  if NegativeOffset > 0 then
    Exit;

  if NegativeOffset <= -FCount then
    Exit;

  Result := Last;

  while NegativeOffset < 0 do begin
    Result := Result.Prev;
    Inc(NegativeOffset);
  end;
end;

end.

