/// <summary> This unit contains procedures for setting up and executing a watch command 
/// in the current directory. If there are changes to files in that directory, specified 
/// commands are executed. </sumamry>
unit Command.Watch;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces,
  Utils.Interfaces;

  /// <summary> This command aims to monitor for changes in the current directory. Changes 
  /// considered are new files, changes to existing files, and file deletions.
  /// 
  /// The ignored folders are the ones that start with ".", the ones named "lib", "modules", 
  /// "backup". Ignored files start with ".", with no extension, .exe, .dll and .trc.
  /// 
  /// Once the change condition is satisfied, the requested commands are executed so that 
  /// their output is generated in the console.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output user instructions about the execution state of this command. </param>
  procedure WatchCommand(ABuilder: ICommandBuilder);

  /// <summary> Registry a watch command using the command builder from pascli. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// registry the command. </param>  
  procedure Registry(ABuilder: ICommandBuilder);

  /// <summary> Execute the command requested by the user, it will be passed to IPathWatcher 
  /// so that it executes it. It is prepared to run the project build and tests as long as 
  /// there is a respective executable. . </summary>
  /// <param name="AFile"> Filename that triggered the action change. </param>  
  function RunUserCommandAsRequested(const AFile: string): Boolean;

var
  /// <summary> Allows to define another function to process the changes detected by the 
  /// Watcher. The main purpose is to use this variable to easily implement unit tests.
  /// </summary>
  RunWatcherCallback: TWatcherRunCallback;

  /// <summary> Allows to set a custom timeout to wait for an change event. The main purpose 
  /// is to use this variable to easily implement unit tests.
  /// </summary>
  CommandWatchTimeout: LongInt = 3600000;

implementation

uses
  SysUtils,
  Command.Test,
  Command.Build,
  Utils.IO,
  Utils.Watcher;

var
  Builder: ICommandBuilder;
  ProjectFile: string;
  IsTest, IsBuild: Boolean;

function GetProjectFileName(ABuilder: ICommandBuilder): string;
var
  LProjectFile: string;
begin
  if ABuilder.CheckOption('test') then
  begin
    LProjectFile := FindProjectFile(GetCurrentDir, 'TestRunner');
    if not FileExists(LProjectFile) then
      LProjectFile := FindProjectFile(ConcatPaths([GetCurrentDir, 'tests']), 'TestRunner');

    if not FileExists(LProjectFile) then
      raise Exception.Create(
        'Test project not found on current dir or tests sub folder. ' +
        'Projet type should use fpcunit');
  end
  else
  begin
    LProjectFile := FindProjectFile(GetCurrentDir, '');
    if not FileExists(LProjectFile) then
      raise Exception.Create('no project found on current path');
  end;
  Result := LProjectFile;
end;

function RunUserCommandAsRequested(const AFile: string): Boolean;
begin
  WriteLn('ProjectFile: ', ProjectFile);
  WriteLn(AFile);

  if IsBuild then
  begin
    Builder.UseArguments(['build', ProjectFile]);
    Builder.Parse;
    BuildCommand(Builder);
  end;

  if IsTest then
  begin
    Builder.UseArguments(['test']);
    Builder.Parse;
    TestCommand(Builder);
  end;

  Result := False;
end;

procedure WatchCommand(ABuilder: ICommandBuilder);
var
  LPathWatcher: IPathWatcher;
begin
  ABuilder.OutputColor(GetCurrentDir + #13#10, ABuilder.ColorTheme.Value);
  ABuilder.OutputColor('', ABuilder.ColorTheme.Other);

  Builder := ABuilder;
  ProjectFile := GetProjectFileName(ABuilder);
  IsTest := Builder.CheckOption('test');
  IsBuild := Builder.CheckOption('build');

  LPathWatcher := 
    TPathWatcher
      .New
      .Path(GetCurrentDir)
      .Ignore(ikStartsText, ['.'])
      .Ignore(ikFolder, ['lib', 'backup'])
      .Ignore(ikExtension, ['.exe', '', '.dll', '.so', '.trc', '.xml', '.res'])
      .Timeout(CommandWatchTimeout)
      .Run(RunWatcherCallback);
  LPathWatcher.Start;
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'watch',
      'watch for project folder changes to build, tests, or just runs the app.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' watch --build --test .'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' watch --build --run',
      @WatchCommand,
      [ccRequiresOneArgument])
      .AddOption(
          'b', 'build', 
          'build the main project', [])
      .AddOption(
          'r', 'run', 
          'run main project', ['t'])
      .AddOption(
          't', 'test', 
          'build test project and run it', ['r']);
end;

initialization
  RunWatcherCallback := RunUserCommandAsRequested;

end.