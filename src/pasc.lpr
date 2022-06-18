program pasc;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cmem, cthreads,
  {$ENDIF}
  Command.Interfaces,
  Command.App,
  Command.Usage,
  Command.Build,
  Command.Clean,
  Command.New,
  Command.Watch;

var
  PascApp: TCommandApp;

{$R *.res}

begin
  PascApp := TCommandApp.Create(nil);
  try
    PascApp.Title := 'Object pascal CLI tool for use with vscode.';

    PascApp
      .CommandBuilder
        .AddCommand(
            'help', 
            'Shows information about how to use this tool or about a specific command.'#13#10 +
            'Ex: pasc help clean ', 
            @UsageCommand, 
            [ccDefault, ccNoArgumentsButCommands]);  

    Command.Build.Registry(PascApp.CommandBuilder);
    Command.Clean.Registry(PascApp.CommandBuilder);
    Command.New.Registry(PascApp.CommandBuilder);
    Command.Watch.Registry(PascApp.CommandBuilder);

    PascApp.CommandBuilder.AddArgument('project file or path', acOptional);

    PascApp.Run;
  finally
    PascApp.Free;
  end;
end.