/// <summary> This unit contains procedures to configure and execute a command to add 
/// a new free pascal/lazarus unit tests project based on fpcunit test framework.
/// Also allows to add code documentation capabilities to the project base on pasdoc
/// </summary>
unit Command.Add;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  /// <summary> This command adds features to the project like a unit tests project to
  /// tests subfolder, or adds code documentation capabilities to the project base on pasdoc
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
  /// <param name="AProjectDir">Project source path. </param>
  procedure CreateTestProjectFiles(const AProjectName, AProjectDir: string);

  /// <summary> Creates the project folders structure with a src and .vscode folders on 
  /// current path. Returns trough AProjectDir parameter the created folder. </summary>
  /// <param name="AProjectName">The project name that will be used as the main folder 
  /// name. </param>
  /// <param name="AProjectDir">Returns the created folder complete path that was created.
  /// </param>
  procedure CreateProjectFolders(const AProjectName: string; out AProjectDir: string);

  {$ENDIF}

implementation

uses
  Classes,
  SysUtils,
  Command.Colors,
  Utils.Shell,
  Utils.Resources,
  Utils.IO;

procedure CreateProjectFolders(const AProjectName: string; out AProjectDir: string);
begin
  AProjectDir := ConcatPaths([GetCurrentDir, AProjectName]);

  if AProjectName = '' then
    raise Exception.Create('Project name not provided.');
  
  if DirectoryExists(AProjectDir) then
    raise Exception.Create('There is already a folder with this name in the current directory.');
  
  if FileExists(AProjectDir) then
    raise Exception.Create('There is already a file with this name in the current directory.');

  CreateDir(AProjectName);
  SetCurrentDir(AProjectDir);
  CreateDir('src');
  CreateDir('.vscode');
end;

procedure CreateTestProjectFiles(const AProjectName, AProjectDir: string);
var
  LFile: TStringList = nil;
  LContent: string;
begin
  try
    WriteLn('Creating project files'); 
    SetCurrentDir(AProjectDir);
   
    LFile := TStringList.Create;

    LContent := GetResource('projectlpr');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, AProjectName + '.lpr']));

    LContent := GetResource('projectlpi');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, AProjectName + '.lpi']));

    LFile.Free;
  except
    on E: Exception do
    begin
      FreeAndNil(LFile);
      raise;
    end;
  end;  
end;

procedure CreateSupportFilesForVSCode(const AProjectName, AProjectDir: string);
var
  LFile: TStringList = nil;
  LContent: string;
begin
  try
    WriteLn('Creating vs code taks and launch files'); 
    SetCurrentDir(AProjectDir);
   
    LFile := TStringList.Create;

    LContent := GetResource('launchjson');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, '.vscode', 'launch.json']));

    LContent := GetResource('tasksjson');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, '.vscode', 'tasks.json']));

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
begin
  ABuilder.GetParsedArguments;

  if not ABuilder.HasArguments then
  begin
    ABuilder.OutputColor('New command requires builder to accept an argument.', ABuilder.ColorTheme.Error);
    ABuilder.OutputColor('', ABuilder.ColorTheme.Other);
    exit;
  end;

  if ABuilder.HasCommands then
  begin
    ABuilder.OutputColor('Running pasc new command for ', ABuilder.ColorTheme.Other);
    ABuilder.OutputColor(ABuilder.Arguments[0].Value, ABuilder.ColorTheme.Title);
  end;

  try
    CreateProjectFolders(ABuilder.Arguments[0].Value, LProjectDir);
    CreateProjectFiles(ABuilder.Arguments[0].Value, LProjectDir);
    CreateSupportFilesForVSCode(ABuilder.Arguments[0].Value, LProjectDir);
    InitializeGit(LProjectDir);
    InitializeBoss(LProjectDir);
  except
    on E: Exception do
    begin
      ABuilder.OutputColor(E.Message, ABuilder.ColorTheme.Error);
    end;
  end;
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'add',
      'adds a unit test project or documentation capabilities.'#13#10 +
      'From the current path create a subfolder with the given project name, '#13#10 +
      'initialize git, initialize the boss dependency manager, create build '#13#10 +
      'and debug support files for use with vscode and create a project based '#13#10 +
      'on a simple free pascal template.'#13#10 +
      'Ex: pasc new <project name>',
      @NewCommand,
      [ccRequiresOneArgument]);
end;

end.