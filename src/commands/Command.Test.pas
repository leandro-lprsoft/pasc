unit Command.Test;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure TestCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  Utils.Tests,
  Utils.Leak,
  Utils.Shell;

function FindInCodeFile(ACodeFile: TStringList; const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ACodeFile.Count - 1 do
    if ContainsText(ACodeFile.Strings[I], AText) then
      Exit(IntToStr(I + 1) + ':1');
end;

function FindTestProject(const AProjectDir: string): string;
var
  LResult: TStringList;
  LSearch: TSearchRec;
  LCodeFile: string;
begin
  Result := '';
  if not DirectoryExists(AProjectDir) then
    Exit;

  LResult := TStringList.Create;
  if FindFirst(ConcatPaths([AProjectDir, '*.lpr']), faAnyFile, LSearch) = 0 then
    try
      repeat
        if ((LSearch.Attr and faAnyFile) <> 0) and (ExtractFileExt(LSearch.Name) = '.lpr') then
        begin
          LCodeFile := ConcatPaths([AProjectDir, LSearch.Name]);
          LResult.LoadFromFile(LCodeFile);
          if FindInCodeFile(LResult, 'TestRunner') <> '' then
          begin
            FindClose(LSearch);
            Exit(LCodeFile);
          end;
          LResult.Clear;
        end;
      until FindNext(LSearch) <> 0;
    finally
      FindClose(LSearch);
      LResult.Free;
    end;  
end;

function GetTestExecutable: string;
var
  LProjectFile, LExeFile: string;
begin
  Result := '';

  LProjectFile := FindTestProject(GetCurrentDir);
  if LProjectFile = '' then
    LProjectFile := FindTestProject(ConcatPaths([GetCurrentDir, 'tests']));

  if LProjectFile = '' then
  begin
    WriteLn(
      'Test project not found on current dir or tests sub folder. ' +
      'Projet type should use fpcunit');
    exit;
  end;
    
  LExeFile := ChangeFileExt(LProjectFile, {$IF DEFINED(WINDOWS)}'.exe'{$ELSE}''{$ENDIF});
  if not FileExists(LExeFile) then
  begin
    WriteLn('Executable test not found. Build it before running pasc test');
    exit;
  end;

  Result := LExeFile;
end;

procedure TestCommand(ABuilder: ICommandBuilder);
var
  LExeFile, LExeOnly, LXmlFile: string;
  LReport: TTestReport;
  LLeak: TLeakReport;
  LOutput: string;
begin
  LExeFile := GetTestExecutable;
  LExeOnly := ExtractFileName(LExeFile);
  LXmlFile := ChangeFileExt(LExeFile, '.xml');
  LOutput := ShellCommand(LExeFile, ['-a', '--file=' + LXmlFile]);

  LReport := TTestReport.New(ABuilder, ExtractFilePath(LExeFile));
  try
    LReport.Executable := LExeOnly;
    LReport.ParseXmlTestsFile(LExeOnly, LXmlFile);
    LReport.Output;
  finally
    LReport.Free;
  end;

  LLeak := TLeakReport.New(ABuilder, ExtractFilePath(LExeFile));
  try
    LLeak.Executable := LExeOnly;
    LLeak.ParseHeapTrace('');
    LLeak.Output;
  except
    LLeak.Free;
  end;

end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'test',
      'run the tests and display a nice report, with links to testing methods,'#13#10 +
      'with posible leak info too.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' test',
      @TestCommand,
      [ccNoParameters]);
end;

end.