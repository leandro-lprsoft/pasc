unit Utils.IO;

{$MODE DELPHI}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils;

  /// get the contents of AFileName in string
  function GetFileContent(const AFileName: string): string;

  /// save AContent string in a file
  procedure SaveFileContent(const AFileName, AContent: string);

  /// returns first ocurrence of text in format line:column
  function FindInCodeFile(ACodeFile: TStringList; const AText: string): string;

  /// Recursively search for the file from the specified path. 
  /// Returns the filename with the full path if it was found.
  function FindFile(const ACurrentDir, AFileName: string): string;
  
implementation

function GetFileContent(const AFileName: string): string;
var
  LFile: TStringList = nil;
begin
  Result := '';
  LFile := TStringList.Create;
  try
    LFile.LoadFromFile(AFileName);
    Result := LFile.Text;
    LFile.Free;
  except
    on E: Exception do
    begin
      LFile.Free;
      raise;
    end;
  end;
end;

procedure SaveFileContent(const AFileName, AContent: string);
var
  LFile: TStringList = nil;
begin
  LFile := TStringList.Create;
  try
    LFile.Text := AContent;
    LFile.SaveToFile(AFileName);
    LFile.Free;
  except
    on E: Exception do
    begin
      LFile.Free;
      raise;
    end;
  end;
end;

function FindInCodeFile(ACodeFile: TStringList; const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ACodeFile.Count - 1 do
    if ContainsText(ACodeFile.Strings[I], AText) then
      Exit(IntToStr(I + 1) + ':1');
end;

function FindFile(const ACurrentDir, AFileName: string): string;
var
  LSearch: TSearchRec;
  LFileFound: string;
begin
  LFileFound := ConcatPaths([ACurrentDir, AFileName]);
  if FileExists(LFileFound) then
    exit(LFileFound);
  LFileFound := '';

  if FindFirst(ConcatPaths([ACurrentDir, AllFilesMask]), faDirectory or faAnyFile, LSearch) = 0 then
    try
      repeat
        if ((LSearch.Attr and faDirectory) <> 0) and 
            (not AnsiMatchText(LSearch.Name, ['.', '..', '.git'])) then
        begin
          LFileFound := FindFile(ConcatPaths([ACurrentDir, LSearch.Name]), AFileName);
          if LFileFound <> '' then
            Exit(LFileFound);
        end;
      until FindNext(LSearch) <> 0;
    finally
      FindClose(LSearch);
    end;
  Result := '';
end;

end.