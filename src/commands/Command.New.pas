/// <summary> This unit contains procedures to configure and execute a command to create 
/// a new free pascal/lazarus project. With also some basic repo initialization, boss
/// package manager ready and support to use vscode with pre configured tasks.
/// </summary>
unit Command.New;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  /// <summary> This command creates a freepascal/lazarus project respecting a previously 
  /// organized folder structure so that it is easier to operate the project in vscode, 
  /// with git previously initialized and ready to work with boss dependency manager.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output user instructions about the execution state of this command. </param>
  procedure NewCommand(ABuilder: ICommandBuilder);

  /// <summary> Registry a new command using the command builder from pascli. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// registry the command. </param>
  procedure Registry(ABuilder: ICommandBuilder);

  {$IF DEFINED(TESTAPP)}

  /// <summary> Creates the project folders structure with a src and .vscode folders on 
  /// current path. Returns trough AProjectDir parameter the created folder. </summary>
  /// <param name="AProjectName">The project name that will be used as the main folder 
  /// name. </param>
  /// <param name="AProjectDir">Returns the created folder complete path that was created.
  /// </param>
  procedure CreateSupportFilesForVSCode(const AProjectName, AProjectDir: string);

  /// <summary> Create the task and launch files for vscode. Within the tasks created 
  /// we have build, test build, test task using pasc to output the results. </summary>
  /// <param name="AProjectName">The project name that will be used as the file name
  /// for tasks and launch file. </param>
  /// <param name="AProjectDir">Project source path. </param>
  procedure CreateProjectFiles(const AProjectName, AProjectDir: string);

  /// <summary> Configures the boss dependency manager to facilitate the installation of 
  /// new project dependencies.</summary>
  /// <param name="AProjectDir">Project source path. </param>
  procedure InitializeBoss(const AProjectDir: string);

  /// <summary> Init git repo on specified project folder. </summary>
  /// <param name="AProjectDir">Project source path. </param>
  procedure ChangeBossFileSourcePath(const AProjectDir: string);

  /// <summary> Adjust boss files, just change the source parameter to subfolder ./src.
  /// </summary>
  /// <param name="AProjectDir">Project source path. </param>
  procedure InitializeGit(const AProjectDir: string);

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

procedure InitializeGit(const AProjectDir: string);
var
  LFile: TStringList = nil;
begin
  try
    SetCurrentDir(AProjectDir);
    WriteLn(ShellCommand('git', ['init']));

    LFile := TStringList.Create;
    LFile.AddText(GetResource('gitignore'));
    LFile.SaveToFile(ConcatPaths([AProjectDir, '.gitignore']));
    LFile.Free;
  except
    on E: Exception do
    begin
      FreeAndNil(LFile);
      raise;
    end;
  end;
end;

procedure ChangeBossFileSourcePath(const AProjectDir: string);
var
  LFile, LContent: string;
begin
  LFile := ConcatPaths([AProjectDir, 'boss.json']);
  LContent := GetFileContent(LFile);
  LContent := StringReplace(LContent, '"mainsrc": "./"', '"mainsrc": "src/"', [rfReplaceAll]);
  SaveFileContent(LFile, LContent);
end;

procedure InitializeBoss(const AProjectDir: string);
begin
  SetCurrentDir(AProjectDir);
  WriteLn('boss init --quiet');
  ShellCommand('boss', ['init', '--quiet']);
  WriteLn('adjusting "mainsrc" boss file attribute');
  ChangeBossFileSourcePath(AProjectDir);
end;

procedure CreateProjectFiles(const AProjectName, AProjectDir: string);
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

procedure NewCommand(ABuilder: ICommandBuilder);
var
  LProjectDir: string = '';
begin
  ABuilder.GetParsedArguments;

  if ABuilder.HasCommands then
    WriteLn('Running pasc new command for ', ABuilder.Arguments[0].Value);

  try
    CreateProjectFolders(ABuilder.Arguments[0].Value, LProjectDir);
    CreateProjectFiles(ABuilder.Arguments[0].Value, LProjectDir);
    CreateSupportFilesForVSCode(ABuilder.Arguments[0].Value, LProjectDir);
    InitializeGit(LProjectDir);
    InitializeBoss(LProjectDir);
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
    end;
  end;
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'new',
      'Creates a new free pascal program.'#13#10 +
      'Ex: pasc new <project name>',
      @NewCommand,
      [ccRequiresOneArgument]);
end;

end.
