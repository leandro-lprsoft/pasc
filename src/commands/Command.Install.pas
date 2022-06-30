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
  Utils.Shell,
  Utils.IO;

procedure CreateFolder(ABuilder: ICommandBuilder; var AFolder: string);
begin
  AFolder := ConcatPaths([GetUserDir, '.pasc']);
  ABuilder.OutputColor('creating destination folder ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor(AFolder + #13#10, ABuilder.ColorTheme.Title);
  if not DirectoryExists(AFolder) then
  begin
    SetCurrentDir(GetUserDir);
    CreateDir('.pasc');
  end;
end;

procedure CopyApp(ABuilder: ICommandBuilder; const AFolder: string);
begin
  ABuilder.OutputColor('copying ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor(ApplicationName, ABuilder.ColorTheme.Value);
  ABuilder.OutputColor(' to destination folder' + #13#10, ABuilder.ColorTheme.Other);
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

procedure UpdateEnvironmentPathWindows(ABuilder: ICommandBuilder; const AFolder: string);
var
  LName: string = 'update-path.ps1';
begin
  ABuilder.OutputColor(
    'updating environment path, this may require administration privileges'#13#10, 
    ABuilder.ColorTheme.Other);

  SaveFileContent(ConcatPaths([AFolder, LName]), GetResource('update-path-ps1'));

  ABuilder.OutputColor('running shell script file: ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor(LName + #13#10, ABuilder.ColorTheme.Title);

  ShellCommand('powershell', [ConcatPaths([AFolder, LName])]);

  ABuilder.OutputColor(
    'please restart your terminal app to path variable take effect'#13#10, 
    ABuilder.ColorTheme.Other);
end;

procedure InstallCommand(ABuilder: ICommandBuilder);
var
  LAppFolder: string = '';
begin
  CreateFolder(ABuilder, LAppFolder);
  CopyApp(ABuilder, LAppFolder);
  SetCurrentDir(LAppFolder);
  {$IF DEFINED(WINDOWS)}
  UpdateEnvironmentPathWindows(ABuilder, LAppFolder);
  {$ENDIF}
  {$IF DEFINED(UNIX)}
  UpdateEnvironmentPathLinux(LAppFolder);
  {$ENDIF}
  {$IF DEFINED(MACOS)}
  UpdateEnvironmentPathMacos(LAppFolder);
  {$ENDIF}
  ABuilder.OutputColor('install complete.'#13#10, ABuilder.ColorTheme.Title);
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