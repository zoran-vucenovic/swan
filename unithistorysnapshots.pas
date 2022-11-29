unit UnitHistorySnapshots;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitFileSna, UnitSpectrum;

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

  TSnapshotHistoryQueue = class
  private
    FSavePeriod: Integer;
    FSpectrum: TSpectrum;
    FramesPassed: Integer; // frames since last save
    FramesDelay: Integer; // when going back, skip the last, go to previous snapshot
    FTopLimit: Integer;
    FSnap: TSnapshot;

    function GetSnap(): TSnapshot;
    procedure SetSavePeriod(AValue: Integer);
    procedure SaveSnapshot();

    procedure SpectrumOnCheckSaveHistorySnapshot;
  public
    First: TSnapshotsHistoryElement;
    Last: TSnapshotsHistoryElement;
    Count: Integer;

    constructor Create;
    destructor Destroy; override;

    function LoadSnapshot(NegativeOffset: Integer = 0): Boolean;
    procedure SetSpectrum(const ASpectrum: TSpectrum);

    property SavePeriod: Integer read FSavePeriod write SetSavePeriod; // in frames (20 is about 1 second)
  end;

implementation

{ TSnapshotHistoryQueue }

function TSnapshotHistoryQueue.GetSnap: TSnapshot;
begin
  if FSnap = nil then begin
    FSnap := TSnapshotInternal48.Create;
    FSnap.SetSpectrum(FSpectrum);
  end;

  Result := FSnap;
end;

procedure TSnapshotHistoryQueue.SetSavePeriod(AValue: Integer);
begin
  if FSavePeriod = AValue then Exit;
  FSavePeriod := AValue;
  FramesDelay := AValue div 3;
end;

function TSnapshotHistoryQueue.LoadSnapshot(NegativeOffset: Integer): Boolean;
var
  N: Integer;
  Element: TSnapshotsHistoryElement;
begin
  Result := False;
  if NegativeOffset > 0 then
    Exit;

  N := NegativeOffset + Count;
  if N < 1 then
    Exit;

  if (NegativeOffset = 0) and (FramesPassed < FramesDelay) then begin
    Result := LoadSnapshot(NegativeOffset - 1);
    if Result then
      Exit;
  end;

  Element := Last;

  N := Count;
  while NegativeOffset < 0 do begin
    Element := Element.Prev;
    Inc(NegativeOffset);
    Dec(N);
  end;

  Result := GetSnap.LoadFromStream(Element.Stream);
  if Result then begin
    FramesPassed := 0;
    Last := Element;
    Count := N;
  end;
end;

procedure TSnapshotHistoryQueue.SaveSnapshot;
var
  Element: TSnapshotsHistoryElement;
begin
  if Count < FTopLimit then begin
    if (Last = nil) or (Last.Next = nil) then begin
      Element := TSnapshotsHistoryElement.Create;
      if First = nil then
        First := Element;
    end else
      Element := Last.Next;

    Inc(Count);
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

procedure TSnapshotHistoryQueue.SpectrumOnCheckSaveHistorySnapshot;
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
  Count := 0;
  FTopLimit := 11;

  FSavePeriod := 0;
  SetSavePeriod(200); // about four seconds
end;

destructor TSnapshotHistoryQueue.Destroy;
begin
  if Assigned(FSpectrum) then
    if FSpectrum.OnCheckSaveHistorySnapshot = @SpectrumOnCheckSaveHistorySnapshot then
      FSpectrum.OnCheckSaveHistorySnapshot := nil;

  Count := 0;
  while First <> nil do begin
    Last := First;
    First := First.Next;
    Last.Free;
  end;

  FSnap.Free;
  inherited Destroy;
end;

procedure TSnapshotHistoryQueue.SetSpectrum(const ASpectrum: TSpectrum);
begin
  if FSpectrum <> ASpectrum then begin
    if Assigned(FSpectrum) then begin
      if FSpectrum.OnCheckSaveHistorySnapshot = @SpectrumOnCheckSaveHistorySnapshot then
        FSpectrum.OnCheckSaveHistorySnapshot := nil;
    end;
    FSpectrum := ASpectrum;

    if Assigned(ASpectrum) then begin
      ASpectrum.OnCheckSaveHistorySnapshot := @SpectrumOnCheckSaveHistorySnapshot;
      FramesPassed := FSavePeriod;
      SpectrumOnCheckSaveHistorySnapshot;
    end;

    FramesPassed := 0;
  end;
end;

{ TSnapshotsHistoryElement }

constructor TSnapshotsHistoryElement.Create;
begin
  Stream := TMemoryStream.Create;
  Next := nil;
  Prev := nil;
end;

destructor TSnapshotsHistoryElement.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;

end.

