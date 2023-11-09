unit UnitFileZip;
// Copyright 2022, 2023 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode ObjFPC}{$H+}
{$i zxinc.inc}

interface

uses
  Classes, SysUtils, UnitChooseFile, Zipper;

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
var
  UnZ: TUnZipper;
  I, II, LE, N: Integer;
  Entry: TFullZipFileEntry;
  Extension: AnsiString;
  SL: TStringList;
  FiUnz: TFileUnzipper;
  ExtFound: Boolean;
  S, S2: AnsiString;
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

          SL := TStringList.Create;
          try
            I := 0;
            while I < UnZ.Entries.Count do begin
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

                if ExtFound then
                  SL.Append(S2);

              end;
              Inc(I);
            end;

            if UnitChooseFile.TFormChooseFile.ShowFormChooseFile(SL, N) then begin
              FileNameInZip := SL.Strings[N];
              SL.Clear;
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
            end;
          finally
            SL.Free;
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

