unit Command.New;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure NewCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

uses
  SysUtils,
  Utils.Shell;

procedure CreateProjectFolder(const AProjectName: string);
var
  LCurrentDir: string;
begin
  LCurrentDir := GetCurrentDir;
  LCurrentDir := ConcatPaths([LCurrentDir, AProjectName]);

  if AProjectName = '' then
    raise Exception.Create('Project name not provided.');
  
  if DirectoryExists(LCurrentDir) then
    raise Exception.Create('There is already a folder with this name in the current directory.');
  
  if FileExists(LCurrentDir) then
    raise Exception.Create('There is already a file with this name in the current directory.');

  CreateDir(AProjectName);
  SetCurrentDir(LCurrentDir);
end;

procedure NewCommand(ABuilder: ICommandBuilder);
begin
  ABuilder.GetParsedArguments;

  if ABuilder.HasCommands then
    WriteLn('Creating object pascal project on: ', ABuilder.Arguments[0].Value);

  try
    CreateProjectFolder(ABuilder.Arguments[0].Value);
    WriteLn(ShellCommand('git', ['init']));

  except
    on E: Exception do
    begin
      WriteLn(E.Message);
    end;
  end;
  
  // generate a .gitignore file with freepascal template
  // create a .lpr file
  // create a .lpi file
  // init vscode tasks.json, launch.json
  // init boss?

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