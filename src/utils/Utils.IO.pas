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

end.