unit Command.Install;

interface

uses
  Command.Interfaces;

  procedure InstallCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

uses
  Classes,
  SysUtils,
  FileUtil,
  Utils.Resources,
  Utils.Shell;

procedure CreateFolder(var AFolder: string);
begin
  AFolder := ConcatPaths([GetUserDir, '.pasc']);
  WriteLn('creating destination folder ', AFolder);
  if not DirectoryExists(AFolder) then
  begin
    SetCurrentDir(GetUserDir);
    CreateDir('.pasc');
  end;
end;

procedure CopyApp(const AFolder: string);
begin
  WriteLn('copying ', ApplicationName);
  if not SameText(ParamStr(0), ConcatPaths([AFolder, ExtractFileName(ParamStr(0))])) then
    CopyFile(ParamStr(0), ConcatPaths([AFolder, ExtractFileName(ParamStr(0))]), [cffOverwriteFile]);  
end;

procedure UpdateEnvironmentPath(const AFolder: string);
var
  LScript, LName: string;
  LFile: TStringList = nil;
begin
  WriteLn('updating environment path, this may require administration privileges ', ApplicationName);

  {$IF DEFINED(WINDOWS)}
    LScript := GetResource('update-path-ps1');
    LName := 'update-path.ps1';
  {$ELSE}
    LScript := GetResource('update-path-sh');
    LName := 'update-path.sh';
  {$ENDIF}

  WriteLn('creating shell script file: ', LName);
  LFile := TStringList.Create;
  try
    LFile.AddText(LScript);
    LFile.SaveToFile(ConcatPaths([AFolder, LName]));
  finally
    LFile.Free;
  end;

  WriteLn('running shell script file: ', LName);
  SetCurrentDir(AFolder);

  {$IF DEFINED(WINDOWS)}
  ShellCommand('powershell', [ConcatPaths([AFolder, LName])]);
  {$ELSE}
  ShellCommand('bash', [ConcatPaths([AFolder, LName])]);
  {$ENDIF}

  WriteLn('please restart your terminal app', LName);
end;

procedure InstallCommand(ABuilder: ICommandBuilder);
var
  LAppFolder: string = '';
begin
  CreateFolder(LAppFolder);
  CopyApp(LAppFolder);
  UpdateEnvironmentPath(LAppFolder);
  WriteLn('done.');
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'install',
      'Install pasc on user home folder and add it to path.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' install',
      @InstallCommand,
      [ccNoParameters]);  
end;

end.