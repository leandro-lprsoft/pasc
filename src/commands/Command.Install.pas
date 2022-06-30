unit Command.Install;

interface

uses
  Command.Interfaces;

  procedure InstallCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

uses
  Classes,
  StrUtils,
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

procedure UpdateEnvironmentPathLinux(const AFolder: string);
var
  LFile: TStringList = nil;
  LProfile: string;
  I: Integer;
  LFound: Boolean = false;
begin
  WriteLn('updating environment path, this may require administration privileges ', ApplicationName);

  LFile := TStringList.Create;
  try
    LProfile := ConcatPaths([GetUserDir, '.profile']);
    LFile.LoadFromFile(LProfile);
    
    WriteLn('changing file ~/.profile ', ApplicationName);

    for I := 0 to LFile.Count -1 do 
      if ContainsText(LFile.Strings[I], '.pasc:$PATH') then 
      begin
        LFound := True;
        break;
      end;

    if not LFound then
    begin
      LFile.Add('PATH="$HOME/.pasc:$PATH"');
      LFile.SaveToFile(LProfile);
    end;
    
    WriteLn('running source ~/.profile to make path changes to take effect ', ApplicationName);

    LFile.Clear;
    LFile.AddText(GetResource('update-path-sh'));
    LFile.SaveToFile(ConcatPaths([GetUserDir, '.pasc', 'update-path.sh']));

    WriteLn(ShellCommand('bash', [ConcatPaths([GetUserDir, '.pasc', 'update-path.sh'])]));
  finally
    LFile.Free;
  end; 
end;

procedure UpdateEnvironmentPathMacos(const AFolder: string);
var
  LFile: TStringList = nil;
  LProfile: string;
  I: Integer;
  LFound: Boolean = false;
begin
  WriteLn('updating environment path, this may require administration privileges ', ApplicationName);

  LFile := TStringList.Create;
  try
    LProfile := ConcatPaths(['/', 'etc', 'paths']);
    LFile.LoadFromFile(LProfile);
    
    WriteLn('changing file /etc/paths ', ApplicationName);

    for I := 0 to LFile.Count -1 do 
      if ContainsText(LFile.Strings[I], '/.pasc') then 
      begin
        LFound := True;
        break;
      end;

    if not LFound then
    begin
      LFile.Add(ConcatPaths([GetUserDir, '.pasc']));
      LFile.SaveToFile(LProfile);
    end;

    WriteLn('running source ~/.profile to make path changes to take effect ', ApplicationName);

    LFile.Clear;
    LFile.AddText(GetResource('update-path-sh'));
    LFile.SaveToFile(ConcatPaths([GetUserDir, '.pasc', 'update-path.sh']));

    WriteLn(ShellCommand('bash', [ConcatPaths([GetUserDir, '.pasc', 'update-path.sh'])]));

  finally
    LFile.Free;
  end; 
end;

procedure UpdateEnvironmentPathWindows(const AFolder: string);
var
  LScript, LName: string;
  LFile: TStringList = nil;
begin
  WriteLn('updating environment path, this may require administration privileges ', ApplicationName);

  LScript := GetResource('update-path-ps1');
  LName := 'update-path.ps1';

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

  ShellCommand('powershell', [ConcatPaths([AFolder, LName])]);

  WriteLn('please restart your terminal app', LName);
end;

procedure InstallCommand(ABuilder: ICommandBuilder);
var
  LAppFolder: string = '';
begin
  CreateFolder(LAppFolder);
  CopyApp(LAppFolder);
  {$IF DEFINED(WINDOWS)}
  UpdateEnvironmentPathWindows(LAppFolder);
  {$ENDIF}
  {$IF DEFINED(LINUX)}
  UpdateEnvironmentPathLinux(LAppFolder);
  {$ELSE}
  UpdateEnvironmentPathMacos(LAppFolder);
  {$ENDIF}
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