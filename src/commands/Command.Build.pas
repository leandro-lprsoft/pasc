unit Command.Build;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure BuildCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

procedure BuildCommand(ABuilder: ICommandBuilder);
begin
  if ABuilder.HasCommands then
    WriteLn('You called build command');
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'build',
      'build the project in current directory or one provided as an argument.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' build'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' build sample1.lpr',
      @BuildCommand,
      [ccRequiresOneArgument])
      .AddOption(
          'd', 'debug', 
          'build the project in debug mode, .lpi file must have a debug mode', ['r'])
      .AddOption(
          'r', 'release', 
          'build the project in release mode, .lpi file must have a release mode', ['d'])
end;

end.