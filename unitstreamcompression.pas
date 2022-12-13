unit UnitStreamCompression;
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, ZStream;

function DecompressStream(const InStream: TStream; const OutStream: TStream): Boolean;
function CompressStream(const InStream: TStream; const OutStream: TStream): Boolean;

implementation

function DecompressStream(const InStream: TStream; const OutStream: TStream
  ): Boolean;
var
  DS: Tdecompressionstream;
  Buff: Array [0..1023] of Byte;
  N: Integer;
begin
  Result := False;
  if Assigned(InStream) and Assigned(OutStream) then begin
    try
      InStream.Position := 0;
      DS := Tdecompressionstream.create(InStream, False);
      try
        DS.SourceOwner := False;
        DS.Position := 0;

        OutStream.Position := 0;
        repeat
          N := DS.read(Buff[0], SizeOf(Buff));
          if N > 0 then
            OutStream.Write(Buff[0], N);
        until N < SizeOf(Buff);
        if OutStream.Size > OutStream.Position then
          OutStream.Size := OutStream.Position;

        Result := True;
      finally
        DS.Free;
      end;
    except
    end;
  end;
end;

function CompressStream(const InStream: TStream; const OutStream: TStream
  ): Boolean;
var
  CS: Tcompressionstream;
  Sz: SizeInt;
  Buff: PByte;
begin
  Result := False;
  if Assigned(InStream) and Assigned(OutStream) then begin
    try
      OutStream.Size := 0;
      OutStream.Position := 0;
      CS := Tcompressionstream.create(Tcompressionlevel.cldefault, OutStream, False);
      try
        CS.SourceOwner := False;

        Sz := InStream.Size;
        GetMem(Buff, Sz);
        try
          InStream.Position := 0;
          if InStream.Read(Buff^, Sz) = Sz then begin
            if CS.write(Buff^, Sz) = Sz then begin
              Result := True;
            end;
          end;
        finally
          FreeMem(Buff);
        end;
      finally
        CS.Free;
      end;
    except
    end;
  end;
end;

end.

