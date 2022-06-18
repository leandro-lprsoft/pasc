unit Command.Clean;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure CleanCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

procedure CleanCommand(ABuilder: ICommandBuilder);
begin
  if ABuilder.HasCommands then
    WriteLn('You called clean command');
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'clean',
      'Removes the following folders lib, backup, to make possible a clean build.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' clean',
      @CleanCommand,
      [ccNoParameters]);
end;

end.