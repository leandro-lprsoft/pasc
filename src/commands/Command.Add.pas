/// <summary> This unit contains procedures to configure and execute a command to add 
/// a new free pascal/lazarus unit tests project based on fpcunit test framework.
/// Also allows to add code documentation capabilities to the project base on pasdoc
/// </summary>
unit Command.Add;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  /// <summary> This command adds features to the project like a unit tests project creating a
  /// tests subfolder, or adds code documentation capabilities to the project base on pasdoc.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output user instructions about the execution state of this command. </param>
  procedure AddCommand(ABuilder: ICommandBuilder);

  /// <summary> Registry a add command using the command builder from pascli. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// registry the command. </param>
  procedure Registry(ABuilder: ICommandBuilder);

  {$IF DEFINED(TESTAPP)}

  /// <summary> Create the task and launch files for vscode. Within the tasks created 
  /// we have build, test build, test task using pasc to output the results. </summary>
  /// <param name="AProjectName">The project name that will be used as the file name
  /// for tasks and launch file. </param>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output user instructions about the execution state of this command. </param>
  /// <param name="AProjectDir">Project source path. </param>
  procedure CreateTestProjectFiles(ABuilder: ICommandBuilder; const AProjectName, AProjectDir: string);

  /// <summary> Create docs folder, generate sub folder for documetation build using pasdoc. Add 
  /// minimal template files for documentation. Add an item to tasks.json to run docs/generate/build.ps1 
  /// script. This script will call pasdoc to generate code documentation. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output user instructions about the execution state of this command. </param>  
  /// <param name="AProjectName">The project name that will be used to create the documentation
  /// </param>
  /// <param name="AProjectDir">Project source path. </param>
  procedure CreateDocsFiles(ABuilder: ICommandBuilder; const AProjectName, AProjectDir: string);

  {$ENDIF}

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  Command.Colors,
  Utils.IO,
  Utils.Output,
  Utils.Resources;

procedure CreateTestProjectFiles(ABuilder: ICommandBuilder; const AProjectName, AProjectDir: string);
var
  LFile: TStringList = nil;
  LContent, LTestsDir, LTestProject: string;
begin
  try
    OutputInfo(ABuilder, 'Starting', 'to add a test project to the current folder'); 

    LTestsDir := ConcatPaths([AProjectDir, 'tests']);
    LTestProject := 'Test' + AProjectName;
    SetCurrentDir(AProjectDir);

    OutputInfo(ABuilder, 'Creating', 'tests sub folder'); 

    if not DirectoryExists(LTestsDir) then
      CreateDir('tests');

    OutputInfo(ABuilder, 'Creating', 'test project files'); 
   
    LFile := TStringList.Create;

    LContent := GetResource('fpcunitprojectlpr');
    LContent := StringReplace(LContent, '{TESTPROJECTNAME}', LTestProject, [rfReplaceAll]);
    LContent := StringReplace(LContent, '{TestCase1}', 'TestCase' + AProjectName, [rfReplaceAll]);
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([LTestsDir, LTestProject + '.lpr']));

    LContent := GetResource('fpcunitprojectlpi');
    LContent := StringReplace(LContent, '{TESTPROJECTNAME}', LTestProject, [rfReplaceAll]);
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([LTestsDir, LTestProject + '.lpi']));

    LContent := GetResource('testcase1pas');
    LContent := StringReplace(LContent, '{TestCase1}', 'TestCase' + AProjectName, [rfReplaceAll]);
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([LTestsDir, 'TestCase' + AProjectName + '.pas']));

    OutputInfo(ABuilder, 'Done', 'test project added with success'); 

    LFile.Free;
  except
    on E: Exception do
    begin
      FreeAndNil(LFile);
      raise;
    end;
  end;  
end;

procedure CreateDocsFiles(ABuilder: ICommandBuilder; const AProjectName, AProjectDir: string);
var
  LFile: TStringList = nil;
  LContent, LDocsDir: string;
begin
  try
    OutputInfo(ABuilder, 'Docs', 'adding documentation resources to the project'); 

    LDocsDir := ConcatPaths([AProjectDir, 'docs']);
    SetCurrentDir(AProjectDir);

    OutputInfo(ABuilder, 'Creating', 'docs sub folder'); 

    if not DirectoryExists(LDocsDir) then
      CreateDir('docs');

    if not DirectoryExists(ConcatPaths([LDocsDir, 'generate'])) then
    begin
      SetCurrentDir(LDocsDir);
      CreateDir('generate');
      SetCurrentDir(AProjectDir);
    end;

    LDocsDir := ConcatPaths([LDocsDir, 'generate']);

    OutputInfo(ABuilder, 'Creating', 'documentation resource files'); 
   
    LFile := TStringList.Create;

    LContent := GetResource('buildps1');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([LDocsDir, 'build.ps1']));

    LContent := GetResource('customcss');
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([LDocsDir, 'custom.css']));

    LContent := GetResource('introduction');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([LDocsDir, 'introduction.txt']));

    LContent := GetResource('quickstart');
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([LDocsDir, 'quickstart.txt']));

    OutputInfo(ABuilder, 'Done', 'documentation resources added with success'); 

    LFile.Free;
  except
    on E: Exception do
    begin
      FreeAndNil(LFile);
      raise;
    end;
  end;  
end;

procedure AddCommand(ABuilder: ICommandBuilder);
var
  LProjectDir: string = '';
  LAddTests, LAddDocs: Boolean;
  LProjectName: string;
  LSourceFound: Boolean = false;
begin
  LAddTests := ABuilder.CheckOption('tests');
  LAddDocs := ABuilder.CheckOption('docs');
  try
    OutputInfo(ABuilder, ABuilder.ExeName, 'running command Add');
    LProjectDir := GetCurrentDir;
    LProjectName := FindProjectFile(LProjectDir, '');
    LSourceFound := FindSourceFile(LProjectDir) <> '';

    OutputInfo(ABuilder, 'Project', IfThen(LProjectName = '', 'not found', LProjectName));
    OutputInfo(ABuilder, 'Source', 
      IfThen(LSourceFound, 'at least one pascal file detected', 'No pascal source file found'));

    if (not (LProjectName <> '')) and (not LSourceFound) then
      raise Exception.Create('No project or source file found in current directory.');

    if LAddTests then
    begin
      if DirectoryExists(ConcatPaths([LProjectDir, 'tests'])) then
        raise Exception.Create('There is already a tests folder in the current directory.');
      CreateTestProjectFiles(ABuilder, ExtractFileName(LProjectDir), LProjectDir);
    end;

    if LAddDocs then
    begin
      if DirectoryExists(ConcatPaths([LProjectDir, 'docs'])) then
        raise Exception.Create('There is already a docs folder in the current directory.');
      CreateDocsFiles(ABuilder, ExtractFileName(LProjectDir), LProjectDir);
    end;
      
  except
    on E: Exception do
    begin
      OutputError(ABuilder, 'Error', E.Message);
    end;
  end;
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'add',
      'adds a unit test project or documentation capabilities to an existing project.'#13#10 +
      'For this command to work you need to provide at least one option. '#13#10 +
      'Ex: pasc add <option>',
      @AddCommand,
      [ccRequiresOneOption])
      .AddOption(
        't',
        'tests',
        'adds a unit test project based on fpcunit test framework. ',
        [])
      .AddOption(
        'd',
        'docs',
        'adds resources that automates the project documentation.',
        []);
end;

end.