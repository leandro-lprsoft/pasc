/// <summary> This unit contains procedures for setting up and executing a watch command 
/// in the current directory. If there are changes to files in that directory, specified 
/// commands are executed. </sumamry>
unit Command.Watch;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

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

implementation

uses
  SysUtils,
  Utils.Interfaces,
  Utils.IO,
  Utils.Watcher,
  Command.Test;

var
  Builder: ICommandBuilder;
  ProjectFile: string;

function RunUserCommandAsRequested(const AFile: string): Boolean;
begin
  WriteLn(AFile);
  Result := False;
end;

procedure WatchCommand(ABuilder: ICommandBuilder);
var
  LIsTest, LIsRun: boolean;
  LProjectFile: string;

  LPathWatcher: IPathWatcher;
begin
  LIsTest := ABuilder.CheckOption('t');
  LIsRun := ABuilder.CheckOption('r');

  if LIsTest then
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

  ABuilder.OutputColor(GetCurrentDir + #13#10, ABuilder.ColorTheme.Value);
  ABuilder.OutputColor('', ABuilder.ColorTheme.Other);

  Builder := ABuilder;
  ProjectFile := LProjectFile;

  //Create TPathWatcherInstance and configure it
  //Start Watcher
  LPathWatcher := 
    TPathWatcher
      .New
      .Path(GetCurrentDir)
      .Ignore(ikStartsText, ['.'])
      .Ignore(ikExtension, ['.exe', '', '.dll', '.so'])
      .Run(RunUserCommandAsRequested);
      
  LPathWatcher.Start;
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'watch',
      'watch for project folder changes to build, tests, or just runs the app.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' watch'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' watch sample1.lpr',
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

end.