unit Command.Watch;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure WatchCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

procedure WatchCommand(ABuilder: ICommandBuilder);
begin
  if ABuilder.HasCommands then
    WriteLn('You called watch command');
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'watch',
      'watch for changes in directory project and runs build, tests, or just runs the application.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' watch'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' watch sample1.lpr',
      @WatchCommand,
      [ccRequiresOneArgument])
      .AddOption(
          't', 'test', 
          'build test project and run it', [])
      .AddOption(
          'b', 'build', 
          'build the main project', [])
      .AddOption(
          'r', 'run', 
          'run main project', []);
end;

end.