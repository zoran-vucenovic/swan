
// Copyright 2022-2025 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$ifdef tzx_header_section}

TTzxBlock28 = class (TTzxBlock)
strict private
  FLen: Integer;
  FSelections: TTapePlayer.TSelections;
  FDetails: String;

  procedure FillDetails;
public
  constructor Create(ATapePlayer: TTapePlayer); override;

  class function GetBlockId: DWord; override;
  class function GetBlockDescription: String; override;
  function GetBlockLength: Integer; override;
  function LoadBlock(const Stream: TStream): Boolean; override;
  function CheckUserInteraction: Boolean; override;
  procedure Details(out S: String); override;
end;

{$else}

procedure TTzxBlock28.FillDetails;
var
  I, N: Integer;
  S1, S2: String;

begin
  N := Length(FSelections);
  if N > 0 then begin
    if N > 15 then begin
      S1 := #13 + ' ... and ' + IntToStr(N - 12) + ' more.';
      N := 12;
    end else begin
      S1 := '';
    end;

    if N >= 10 then
      S2 := ' '
    else
      S2 := '';

    for I := 0 to N - 1 do begin
      if I = 9 then
        S2 := '';
      FDetails := FDetails + #13 + S2 + IntToStr(I + 1) + '. offset ';
      if FSelections[I].RelativeOffset > 0 then
        FDetails := FDetails + '+';
      FDetails := FDetails + FSelections[I].RelativeOffset.ToString;
      if Length(FSelections[I].DescriptionText) > 0 then
        FDetails := FDetails + ': ' + Trim(FSelections[I].DescriptionText);
    end;

    FDetails := FDetails + S1;
  end else
    FDetails := ' — empty —';

end;

constructor TTzxBlock28.Create(ATapePlayer: TTapePlayer);
begin
  inherited Create(ATapePlayer);

  FLen := 0;
  SetLength(FSelections, 0);
  FDetails := '';
end;

class function TTzxBlock28.GetBlockId: DWord;
begin
  Result := $28;
end;

class function TTzxBlock28.GetBlockDescription: String;
begin
  Result := 'Select block';
end;

function TTzxBlock28.GetBlockLength: Integer;
begin
  Result := FLen;
end;

function TTzxBlock28.LoadBlock(const Stream: TStream): Boolean;
var
  W: Word;
  B: Byte;
  N: Integer;
  I: Integer;
  J: Integer;
  K: Integer;
  L: Integer;
  M: Integer;
  Okay: Boolean;
  S, S1: AnsiString;

begin
  Result := False;
  if Stream.Size >= Stream.Position + 2 then begin
    if Stream.Read(W{%H-}, 2) = 2 then begin
      W := LEtoN(W);
      L := W;
      FLen := L + 2;
      if (L >= 1) and (Stream.Size - Stream.Position >= L) then begin
        if Stream.Read(B{%H-}, 1) = 1 then begin
          Dec(L);
          N := B;
          SetLength(FSelections, N);

          I := 0;
          repeat
            if I = N then begin
              if L = 0 then begin
                FillDetails;
                Result := True;
              end;
              Break;
            end;

            Okay := False;
            if L >= 3 then begin
              if (Stream.Read(W, 2) = 2) and (Stream.Read(B, 1) = 1) then begin
                L := L - 3;
                M := B;
                if L >= M then begin
                  W := LEtoN(W);
                  FSelections[I].RelativeOffset := Int16(W);
                  SetLength(S{%H-}, M);
                  Okay := M = 0;
                  if not Okay then begin
                    SetLength(S1{%H-}, M);
                    if Stream.Read(S1[1], M) = M then begin
                      L := L - M;
                      J := 0;
                      K := 0;
                      while J < M do begin
                        Inc(J);
                        Inc(K);
                        if S1[J] <= #32 then begin
                          S[K] := #32;
                          if (S1[J] = #13) and (J < M) and (S1[J + 1] = #10) then
                            Inc(J);
                        end else begin
                          S[K] := S1[J];
                        end;
                      end;
                      SetLength(S, K);
                      TCommonFunctions.ConvertCodePageFromCp1252ToUtf8(S);
                      Okay := True;
                    end;
                  end;
                  FSelections[I].DescriptionText := Trim(S);
                end;
              end;
            end;

            if not Okay then
              Break;

            Inc(I);
          until False;
        end;

      end;
    end;
  end;

  if not Result then
    SetLength(FSelections, 0);
end;

function TTzxBlock28.CheckUserInteraction: Boolean;
begin
  Result := inherited CheckUserInteraction;

  if Assigned(FTapePlayer.OnSelectBlock) then begin
    if Length(FSelections) > 0 then begin
      Result := FTapePlayer.OnSelectBlock(FSelections);
    end;
  end;
end;

procedure TTzxBlock28.Details(out S: String);
begin
  S := FDetails;
end;

{$endif}


