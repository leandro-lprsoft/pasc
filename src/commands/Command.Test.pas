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

  /// <summary> From the current directory it tries to locate an fpcunit compatible test 
  /// project that has been previously compiled. Returns complete test file executable name.
  /// If the function returns empty, the project does not exist or is not of the expected type.
  /// </summary>  
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output info about the existence of the test project and whether it is valid. </param>
  function GetTestExecutable(ABuilder: ICommandBuilder): string;

implementation

uses
  Classes,
  SysUtils,
  Utils.Interfaces,
  Utils.Tests,
  Utils.Leak,
  Utils.Shell,
  Utils.IO;

function GetTestExecutable(ABuilder: ICommandBuilder): string;
var
  LProjectFile, LExeFile: string;
begin
  Result := '';

  LProjectFile := FindProjectFile(GetCurrentDir, 'TestRunner');
  if LProjectFile = '' then
    LProjectFile := FindProjectFile(ConcatPaths([GetCurrentDir, 'tests']), 'TestRunner');

  if LProjectFile = '' then
  begin
    ABuilder.Output(
      'A test project was not found in the current path or in a tests subfolder. '#13#10 +
      'The test project must be based on the fpcunit test framework.');
    exit;
  end;
    
  LExeFile := ChangeFileExt(LProjectFile, {$IF DEFINED(WINDOWS)}'.exe'{$ELSE}''{$ENDIF});
  if not FileExists(LExeFile) then
  begin
    ABuilder.Output('Executable test not found. Build it before running pasc test');
    exit;
  end;

  Result := LExeFile;
end;

procedure TestCommand(ABuilder: ICommandBuilder);
var
  LExeFile, LExeOnly, LXmlFile, LTestCase: string;
  LReport: TTestReport;
  LLeak: ILeakReport;
begin
  LExeFile := GetTestExecutable(ABuilder);
  if LExeFile.IsEmpty then
    Exit;
  LExeOnly := ExtractFileName(LExeFile);
  LXmlFile := ChangeFileExt(LExeFile, '.xml');
  LTestCase := '';
  if ABuilder.CheckOption('test-case', LTestCase) then
    ShellExecute(LExeFile, ['--suite=' + LTestCase, '--file=' + LXmlFile])
  else
    ShellExecute(LExeFile, ['-a', '--file=' + LXmlFile]);

  LReport := TTestReport.New(ABuilder, ExtractFilePath(LExeFile));
  try
    LReport.Executable := LExeOnly;
    LReport.ParseXmlTestsFile(LExeOnly, LXmlFile);
    LReport.Output;
  finally
    LReport.Free;
  end;

  LLeak := TLeakReport.New(ABuilder, ExtractFilePath(LExeFile));
  LLeak.Executable := LExeOnly;
  LLeak.ParseHeapTrace('');
  LLeak.Output;
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'test',
      'run the tests and display a nice report, with links to testing methods.'#13#10 +
      ''#13#10 +
      'This command recursively searches for a previously compiled test project '#13#10 +
      'from the current folder and runs it. At the end of the execution, an xml '#13#10 +
      'file with the same name as the test project is expected to be read. From '#13#10 + 
      'this reading, a result is generated with the objective of highlighting '#13#10 +
      'the tests that failed, formatting the name of the source code file with '#13#10 + 
      'a line and column in a format that vscode is able to understand and '#13#10 +
      'providing the developer with a link for quick navigation through the code.'#13#10 +
      ''#13#10 +
      'If a memory leak trace file named heap.trc is found, the command also outputs '#13#10 + 
      'information with details about the possible memory leaks in a format that vscode can '#13#10 + 
      'provide easy navigation through the code. '#13#10 +
      ''#13#10 +
      'Ex: ' + ABuilder.ExeName + ' test'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' test=TTestCaseSuite',
      @TestCommand,
      [ccNoParameters])
      .AddOption(
        't',
        'test-case',
        'run only the specified test case or test suite.',
        [],
        ocOptionalValue);
end;

end.