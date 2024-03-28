unit UnitFileZip;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, Types, UnitFormChooseString, Zipper;

type

  { TFileUnzipper }

  TFileUnzipper = class(TObject)
    strict private
      InStream: TStream;
      OutStream: TStream;

      procedure DoOpenInputStream(Sender: TObject; var AStream: TStream);
      procedure DoCloseInputStream(Sender: TObject; var AStream: TStream);
      procedure DoCreateOutZipStream(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);
      procedure DoDoneOutZipStream(Sender : TObject; var AStream : TStream; {%H-}AItem : TFullZipFileEntry);

    public
      class function GetFileFromZipStream(const AZipStream: TStream; const Extensions: array of String;
        out AStream: TStream; out FileNameInZip: String): Boolean; static;
      class function GetFileFromZipFile(const ZipFileName: String; const Extensions: array of String;
        out AStream: TStream; out FileNameInZip: String): Boolean; static;
    end;


implementation

{ TFileUnzipper }

procedure TFileUnzipper.DoOpenInputStream(Sender: TObject; var AStream: TStream
  );
begin
  AStream := InStream;
  if Assigned(InStream) then
    InStream.Position := 0;
end;

procedure TFileUnzipper.DoCloseInputStream(Sender: TObject; var AStream: TStream
  );
begin
  AStream := nil;
end;

procedure TFileUnzipper.DoCreateOutZipStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  OutStream := TMemoryStream.Create;
  OutStream.Position := 0;
  AStream := OutStream;
end;

procedure TFileUnzipper.DoDoneOutZipStream(Sender: TObject;
  var AStream: TStream; AItem: TFullZipFileEntry);
begin
  if Assigned(AStream) then
    AStream.Position := 0;
end;

class function TFileUnzipper.GetFileFromZipStream(const AZipStream: TStream;
  const Extensions: array of String; out AStream: TStream; out
  FileNameInZip: String): Boolean;

const
  CaptionText: AnsiString = 'Choose a file';
  MessageText: AnsiString =
    'The chosen zip file contains %d file entries which might be Spectrum files'
      + '. Please choose one:'
    ;

var
  UnZ: TUnZipper;
  I, II, LE, N, L, LL, LLL: Integer;
  Entry: TFullZipFileEntry;
  Extension: AnsiString;
  SL: TStringList;
  FiUnz: TFileUnzipper;
  ExtFound: Boolean;
  S, S2: AnsiString;
  SA: TStringDynArray;

begin
  Result := False;
  AStream := nil;
  FileNameInZip := '';

  if Assigned(AZipStream) then begin
    LE := Length(Extensions);
    if LE > 0 then begin
      FiUnz := TFileUnzipper.Create;
      try
        UnZ := TUnZipper.Create;
        try
          UnZ.FileName := '';
          FiUnz.InStream := AZipStream;
          UnZ.OnOpenInputStream := @(FiUnz.DoOpenInputStream);
          UnZ.OnCloseInputStream := @(FiUnz.DoCloseInputStream);
          UnZ.Examine;

          LL := UnZ.Entries.Count;
          if LL <= 144 then
            L := LL
          else
            L := 144;
          SetLength(SA{%H-}, L);
          L := 0;
          I := 0;
          while I < LL do begin
            Entry := UnZ.Entries.FullEntries[I];
            if not (Entry.IsDirectory or Entry.IsLink) then begin
              S2 := Entry.ArchiveFileName;
              Extension := ExtractFileExt(S2);

              ExtFound := False;
              II := 0;
              while (not ExtFound) and (II < LE) do begin
                S := Trim(Extensions[II]);
                if Length(S) > 0 then begin
                  if not S.StartsWith(ExtensionSeparator, True) then
                    S := ExtensionSeparator + S;
                  ExtFound := AnsiCompareText(Extension, S) = 0;
                end;
                Inc(II);
              end;

              if ExtFound then begin
                if L >= Length(SA) then begin
                  LLL := (L * 7) div 5;
                  if LLL > LL then
                    LLL := LL;
                  SetLength(SA, LLL);
                end;
                SA[L] := S2;
                Inc(L);
              end;

            end;
            Inc(I);
          end;
          SetLength(SA, L);

          if UnitFormChooseString.TFormChooseString.ShowFormChooseString(
            SA, CaptionText, Format(MessageText, [L]), N)
          then begin
            if N < 0 then
              Result := True // user cancelled
            else begin
              FileNameInZip := SA[N];
              SetLength(SA, 0);

              SL := TStringList.Create;
              try
                SL.Append(FileNameInZip);

                FiUnz.OutStream := nil;
                try
                  try
                    UnZ.OnCreateStream := @(FiUnz.DoCreateOutZipStream);
                    UnZ.OnDoneStream := @(FiUnz.DoDoneOutZipStream);
                    UnZ.UnZipFiles(SL);
                    if Assigned(FiUnz.OutStream) then begin
                      if FiUnz.OutStream.Size > 0 then begin
                        Extension := ExtractFileExt(FileNameInZip);
                        if AnsiCompareText(Extension, ExtensionSeparator + 'zip') = 0 then begin
                          if GetFileFromZipStream(FiUnz.OutStream, Extensions, AStream, S) then begin
                            FileNameInZip := IncludeTrailingPathDelimiter(FileNameInZip) + S;

                            Result := True;
                          end else
                            AStream := nil;
                        end else begin
                          AStream := FiUnz.OutStream;
                          FiUnz.OutStream := nil;
                          Result := True;
                        end;
                      end;
                    end;
                  except
                    AStream := nil;
                  end;
                finally
                  FiUnz.OutStream.Free;
                end;
              finally
                SL.Free;
              end;
            end;
          end;
        finally
          UnZ.Free;
        end;
      finally
        FiUnz.Free;
      end;
    end;
  end;
end;

class function TFileUnzipper.GetFileFromZipFile(const ZipFileName: String;
  const Extensions: array of String; out AStream: TStream; out
  FileNameInZip: String): Boolean;
var
  FileStream: TStream;
  ZipStream: TMemoryStream;
begin
  Result := False;
  AStream := nil;
  try
    FileStream := TFileStream.Create(ZipFileName, fmOpenRead or fmShareDenyWrite);
    try
      // Create auxiliary in-memory stream, instead of passing the file stream
      // directly, so that we can release file locks immediately.
      ZipStream := TMemoryStream.Create;
      try
        ZipStream.Size := FileStream.Size;
        FileStream.Position := 0;
        ZipStream.Position := 0;
        if FileStream.Read(ZipStream.Memory^, ZipStream.Size) = ZipStream.Size then begin
          FreeAndNil(FileStream); // release file locks immediately
          Result := GetFileFromZipStream(ZipStream, Extensions, AStream, FileNameInZip);
        end;
      finally
        ZipStream.Free;
      end;

    finally
      FileStream.Free;
    end;
  except
  end;
end;

end.

