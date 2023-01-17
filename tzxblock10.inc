
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxTapBlock = class (TTzxBlock11abstract)
strict private
  type
    THeader = packed record
      DataType: Byte;
      FileName: packed array [1..10] of Byte;
      DataLenght: Word;
      Param1: Word;
      Param2: Word;
    end;

    THeaderWithDescription = record
      Header: THeader;
      IsValid: Boolean;
      Description: String;
    end;

strict private
  HeaderWD: THeaderWithDescription;

  procedure DecodeHeader();

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
  function LoadBlock(const Stream: TStream): Boolean; override;
end;

{$else}

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

procedure {TTzxPlayer.}TTzxTapBlock.DecodeHeader;
var
  FileName: RawByteString;
begin
  if HeaderWD.IsValid then begin
    HeaderWD.Header.Param1 := LEtoN(HeaderWD.Header.Param1);
    HeaderWD.Header.Param2 := LEtoN(HeaderWD.Header.Param2);
    case HeaderWD.Header.DataType of
      0:
        HeaderWD.Description := 'Program%s';
      1:
        HeaderWD.Description := 'Number array%s';
      2:
        HeaderWD.Description := 'Character array%svariable name: ' + AnsiChar(WordRec(HeaderWD.Header.Param1).Hi) + #13;
      3:
        HeaderWD.Description := 'Code file%s';
    otherwise
      HeaderWD.IsValid := False;
      Exit;
    end;

    SetLength(FileName{%H-}, 10);
    Move(HeaderWD.Header.FileName[1], FileName[1], 10);
    FileName := SpectrumCharToUtf8(FileName);

    HeaderWD.Description := Format(HeaderWD.Description, [': ' + FileName + #13]);

    HeaderWD.Header.DataLenght := LEtoN(HeaderWD.Header.DataLenght);
    HeaderWD.Description := HeaderWD.Description + 'Length of data block: ' + IntToStr(HeaderWD.Header.DataLenght)
      + #13 + Format('Param1: %d%sParam2: %d', [HeaderWD.Header.Param1, #13, HeaderWD.Header.Param2])
      ;
  end;
end;

function {TTzxPlayer.}TTzxTapBlock.AddUsedBitsDetails: String;
begin
  Result := '';
end;

constructor TTzxTapBlock.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  HeaderWD.IsValid := False;
end;

class function {TTzxPlayer.}TTzxTapBlock.GetBlockId: Integer;
begin
  Result := $10;
end;

function {TTzxPlayer.}TTzxTapBlock.LoadBlock(const Stream: TStream): Boolean;
var
  L: Integer;
  W: Word;
begin
  Result := False;
  HeaderWD.IsValid := False;
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
            Result := True;
            PEnd := Data + (DataLen - 1);
            if (Data^ and $80) = 0 then begin
              PilotPulsesNeeded := DefaultPilotHeaderPulsesNeeded;
              if DataLen > SizeOf(THeader) then begin
                Move((Data + 1)^, HeaderWD.Header, SizeOf(THeader));
                HeaderWD.IsValid := True;
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
  if HeaderWD.IsValid then
    S := HeaderWD.Description + #13 + S;
end;

{$endif}

