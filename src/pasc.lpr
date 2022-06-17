program pasc;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Command.Interfaces,
  Command.App,
  Command.Usage;

var
  Application: TCommandApp;

{$R *.res}

begin
  Application := TCommandApp.Create(nil);
  Application.Title := 'Object pascal CLI tool for use with vscode.';
  Application
    .Command
      .AddCommand(
          'help', 
          'Shows information about how to use this tool or about a specific command.'#13#10 +
          'Ex: pasc help clean ', 
          @UsageCommand, 
          [ccDefault, ccNoArgumentsButCommands])
      .AddCommand(
          'clean',
          'Removes the following folders lib, backup, to make possible a clean build.'#13#10 +
          'Ex: pasc clean',
          @UsageCommand,
          [ccNoParameters])
      .AddCommand(
          'new',
          'Creates a new free pascal program.'#13#10 +
          'Ex: pasc clean',
          @UsageCommand,
          [])          
      .AddCommand(
          'build',
          'build the project in current directory or one provided as an argument.'#13#10 +
          'Ex: pasc build'#13#10 +
          'Ex: pasc build sample1.lpr',
          @UsageCommand,
          [ccRequiresOneArgument])
          .AddOption(
              'd', 'debug', 
              'build the project in debug mode, .lpi file must have a debug mode', ['r'])
          .AddOption(
              'r', 'release', 
              'build the project in release mode, .lpi file must have a release mode', ['d'])
      .AddArgument('project file or path', acOptional);

  Application.Run;
  Application.Free;

{
  Application.
    Commands
      .Add('clean', @CleanCallback, 'clear files in folders lib and backup')
        .Option('all', 'a', 'clear all files in lib folder and backup folder')
      .Add('build', @BuildCallback)
        .Option('r', 'release', 'build in release mode', ['d'])
        .Option('d', 'debug', 'build in release mode', ['r'])
      .Add('new', @NewCallback, 'creates a new dir from current directory with simple project for free pascal', [crOneArgument]);
      .Add('test', @BuildCallback, 'run tests projects inside tests sub folder')
      .Add('watch', @WatchCallback, 'watch for changes in directory project and runs build, tests, or just runs the application.');
        .option('t', 'test', 'build and run tests projects')
        .option('b', 'build', 'build source project')
        .option('r', 'run', 'run the application')
      .Add('version', 'shows version info about pascode')
}
end.