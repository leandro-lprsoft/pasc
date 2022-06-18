unit Command.New;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure NewCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

procedure NewCommand(ABuilder: ICommandBuilder);
begin
  if ABuilder.HasCommands then
    WriteLn('You called new command');
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'new',
      'Creates a new free pascal program.'#13#10 +
      'Ex: pasc clean',
      @NewCommand,
      []);
end;

end.