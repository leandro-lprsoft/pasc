/// <summary> This unit contains procedures to configure and execute a command to create 
/// dipslay unit tests colored output. Also should display source code location for each
/// failed test. 
/// 
/// If a heap trace file is found on test project directory an output with possible leaks
/// is generated.
/// </summary>
unit Command.Test;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  /// <summary> This command recursively searches for a previously compiled test project 
  /// from the current folder and runs it. At the end of the execution, an xml file with 
  /// the same name as the test project is expected to be read. From this reading, a result
  /// is generated with the objective of highlighting the tests that failed, formatting the 
  /// name of the source code file with a line and column in a format that vscode is able to 
  /// understand and providing the developer with a link for quick navigation through the code.
  ///
  /// If a memory leak trace file named heap.trc is found, the command also outputs 
  /// information with details about the possible memory leaks in a format that vscode can 
  /// provide easy navigation through the code.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output user instructions about the execution state of this command. </param>
  procedure TestCommand(ABuilder: ICommandBuilder);

  /// <summary> Registry a test command using the command builder from pascli. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// registry the command. </param>
  procedure Registry(ABuilder: ICommandBuilder);

  {$IF DEFINED(TESTAPP)}

  /// <summary> From the current directory it tries to locate an fpcunit compatible test 
  /// project that has been previously compiled. Returns complete test file executable name
  /// </summary>  
  function GetTestExecutable: string;

  /// <summary> From the current directory it tries to locate an fpcunit compatible test 
  /// project. Returns the test project file name. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output user instructions or to iteract with the user. </param>
  function FindTestProject(const AProjectDir: string): string;
  
  {$ENDIF}

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  Utils.Tests,
  Utils.Leak,
  Utils.Shell,
  Utils.IO;

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
  LOutput := ShellExecute(LExeFile, ['-a', '--file=' + LXmlFile]);

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