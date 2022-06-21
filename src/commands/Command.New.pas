unit Command.New;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure NewCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

uses
  Classes,
  SysUtils,
  Utils.Shell,
  Utils.Resources;

const
  GITIGNORE: array [0..16] of string = (
    '# files',
    '*.lps',
    '*.compiled',
    '*.[oa]',
    '*.ppu',
    '*.rst',
    '*.cgi',
    '*.exe',
    '*.log',
    '*.bak*',
    'fp.ini',
    'fp.cfg',
    'fp.dsk',
    '# folders',
    '**/lib/',
    '**/backup/',
    '**/modules/'
  );

procedure CreateProjectFolders(const AProjectName: string; out AProjectDir: string);
begin
  AProjectDir := ConcatPaths([GetCurrentDir, AProjectName]);

  if AProjectName = '' then
    raise Exception.Create('Project name not provided.');
  
  if DirectoryExists(AProjectDir) then
    raise Exception.Create('There is already a folder with this name in the current directory.');
  
  if FileExists(AProjectDir) then
    raise Exception.Create('There is already a file with this name in the current directory.');

  CreateDir(AProjectName);
  SetCurrentDir(AProjectDir);
  CreateDir('src');
  CreateDir('.vscode');

end;

procedure InitializeGit(const AProjectDir: string);
var
  LFile: TStringList = nil;
begin
  try
    SetCurrentDir(AProjectDir);
    WriteLn(ShellCommand('git', ['init']));

    LFile := TStringList.Create;
    LFile.AddText(string.Join(#13#10, GITIGNORE));
    LFile.SaveToFile(ConcatPaths([AProjectDir, '.gitignore']));
    LFile.Free;
  except
    on E: Exception do
    begin
      FreeAndNil(LFile);
      raise;
    end;
  end;
end;

procedure InitializeBoss(const AProjectDir: string);
begin
  SetCurrentDir(AProjectDir);
  WriteLn(ShellCommand('boss', ['init', '--quiet']));
end;

procedure CreateProjectFiles(const AProjectName, AProjectDir: string);
var
  LFile: TStringList = nil;
  LContent: string;
begin
  try
    WriteLn('Creating project files'); 
    SetCurrentDir(AProjectDir);
   
    LFile := TStringList.Create;

    LContent := GetResource('projectlpr');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, 'src', AProjectName + '.lpr']));

    LContent := GetResource('projectlpi');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, 'src', AProjectName + '.lpi']));

    LFile.Free;
  except
    on E: Exception do
    begin
      FreeAndNil(LFile);
      raise;
    end;
  end;  
end;

procedure CreateSupportFilesForVSCode(const AProjectName, AProjectDir: string);
var
  LFile: TStringList = nil;
  LContent: string;
begin
  try
    WriteLn('Creating vs code taks and launch files'); 
    SetCurrentDir(AProjectDir);
   
    LFile := TStringList.Create;

    LContent := GetResource('launchjson');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, '.vscode', 'launch.json']));

    LContent := GetResource('tasksjson');
    LContent := StringReplace(LContent, '{PROJECTNAME}', AProjectName, [rfReplaceAll]);
    LFile.Clear;
    LFile.AddText(LContent);
    LFile.SaveToFile(ConcatPaths([AProjectDir, '.vscode', 'tasks.json']));

    LFile.Free;
  except
    on E: Exception do
    begin
      FreeAndNil(LFile);
      raise;
    end;
  end;  
end;

procedure NewCommand(ABuilder: ICommandBuilder);
var
  LProjectDir: string = '';
begin
  ABuilder.GetParsedArguments;

  if ABuilder.HasCommands then
    WriteLn('Running pasc new command for ', ABuilder.Arguments[0].Value);

  try
    CreateProjectFolders(ABuilder.Arguments[0].Value, LProjectDir);
    CreateProjectFiles(ABuilder.Arguments[0].Value, LProjectDir);
    CreateSupportFilesForVSCode(ABuilder.Arguments[0].Value, LProjectDir);
    InitializeGit(LProjectDir);
    InitializeBoss(LProjectDir);
  except
    on E: Exception do
    begin
      WriteLn(E.Message);
    end;
  end;
  
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
