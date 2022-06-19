program pasc;

{$MODE DELPHI}{$H+}

uses
  {$IFDEF UNIX}
  cmem,
  cthreads,
  {$ENDIF}
  Command.Interfaces,
  Command.App,
  Command.Usage,
  Command.Version,
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

    Command.Build.Registry(PascApp.CommandBuilder);
    Command.Clean.Registry(PascApp.CommandBuilder);
    Command.New.Registry(PascApp.CommandBuilder);
    Command.Usage.Registry(PascApp.CommandBuilder);
    Command.Version.Registry(PascApp.CommandBuilder);
    Command.Watch.Registry(PascApp.CommandBuilder);

    PascApp.CommandBuilder.AddArgument('project file name', acOptional);

    PascApp.Run;
  finally
    PascApp.Free;
  end;
end.
