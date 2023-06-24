
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxTapBlock = class (TTzxBlock11abstract)
strict private
  FHeaderDetails: String;

strict protected
  function AddUsedBitsDetails: String; override;

public
  constructor Create(ATapePlayer: TTapePlayer); override;
  class function GetBlockId: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  class function GetBlockDescription: String; override;
  procedure Details(out S: String); override;
end;

TTzxBlock10 = class (TTzxTapBlock)
public
  class function GetBlockId: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
end;

{$else}

class function TTzxBlock10.GetBlockId: Integer;
begin
  Result := $10;
end;

function {TTzxPlayer.}TTzxBlock10.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
begin
  Result := False;
  if Stream.Size - Stream.Position >= 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      PauseAfterBlock := W;
      Result := inherited LoadBlock(Stream);
      FLen := FLen + 2;
    end;
  end;

end;

function {TTzxPlayer.}TTzxTapBlock.AddUsedBitsDetails: String;
begin
  Result := '';
end;

constructor TTzxTapBlock.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FHeaderDetails := '';
end;

class function TTzxTapBlock.GetBlockId: Integer;
begin
  Result := 0;
end;

function {TTzxPlayer.}TTzxTapBlock.LoadBlock(const Stream: TStream): Boolean;

type
  THeader = packed record
    DataType: Byte;
    FileName: packed array [1..10] of Byte;
    DataLenght: Word;
    Param1: Word;
    Param2: Word;
  end;

var
  Header: THeader;

  function DecodeVariableName(): RawByteString;
  var
    C: AnsiChar;
  begin
    if (Header.DataType in [1, 2]) then begin
      C := AnsiChar((WordRec(Header.Param1).Hi and $1F) + $60);
      if C in ['a'..'z'] then begin
        Result := #13 + 'variable name: ' + C;
        if Header.DataType = 2 then
          Result := Result + '$';
        Exit;
      end;
    end;
    Result := '';
  end;

  procedure DecodeHeader();
  var
    FileName: RawByteString;
  begin
    Header.Param1 := LEtoN(Header.Param1);
    Header.Param2 := LEtoN(Header.Param2);
    Header.DataLenght := LEtoN(Header.DataLenght);
    case Header.DataType of
      0:
        FHeaderDetails := 'Program';
      1:
        FHeaderDetails := 'Number array';
      2:
        FHeaderDetails := 'Character array';
      3:
        FHeaderDetails := 'Code file';
    otherwise
      Exit;
    end;

    SetLength(FileName{%H-}, 10);
    Move(Header.FileName[1], FileName[1], 10);
    FileName := TCommonFunctions.SpectrumCharToUtf8(FileName);

    FHeaderDetails :=
      FHeaderDetails + ': ' + FileName + DecodeVariableName() + #13
      + Format('Length of data block: %d%sParam1: %d%sParam2: %d%s',
                [Header.DataLenght, #13, Header.Param1, #13, Header.Param2, #13])
      ;
  end;

var
  L: Integer;
  W: Word;

begin
  Result := False;
  FHeaderDetails := '';
  L := Stream.Size - Stream.Position - 2;
  if L >= 0 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      DataLen := W;
      if DataLen <= L then begin
        Result := DataLen = 0;
        FLen := DataLen + 2;
        if not Result then begin
          Getmem(Data, DataLen);
          if Stream.Read(Data^, DataLen) = DataLen then begin
            PEnd := Data + (DataLen - 1);
            Result := True;
            if (Data^ and $80) = 0 then begin
              PilotPulsesNeeded := DefaultPilotHeaderPulsesNeeded;
              if DataLen > SizeOf(THeader) then begin
                Move((Data + 1)^, Header, SizeOf(THeader));
                DecodeHeader();
              end;
            end else
              PilotPulsesNeeded := DefaultPilotDataPulsesNeeded;

          end else
            FreeMemAndNil(Data);
        end;
      end;
    end;
  end;
end;

class function {TTzxPlayer.}TTzxTapBlock.GetBlockDescription: String;
begin
  Result := 'Standard Speed Data Block';
end;

procedure {TTzxPlayer.}TTzxTapBlock.Details(out S: String);
begin
  inherited Details(S);
  S := FHeaderDetails + S;
end;

{$endif}

