/// <summary> This unit contains procedures to configure and execute a command to install 
/// pasc app in the following path $HOME/.pasc. 
/// </summary>
unit Command.Install;

interface

uses
  Command.Interfaces;

  /// <summary> Copy the pasc application to the "home" folder and create the folder 
  /// if it doesn't exist. Updates the OS-equivalent path environment variable with 
  /// the new folder so that pasc is visible on the command line. </summary>
  /// <param name="ABuilder"> Instance o the builder that will be use to output
  /// instructions to user about the installation status </param>
  procedure InstallCommand(ABuilder: ICommandBuilder);

  /// <summary> Registry a install command using the command builder from pascli, also sets. 
  /// options and usage help info. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// registry the command. </param>
  procedure Registry(ABuilder: ICommandBuilder);

  {$IF DEFINED(TESTAPP)}

  /// <summary> Creates a subfolder in the user's home folder with the application name. 
  /// Set the home folder to the new current path. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output info about folder creation. </param>
  /// <param name="AFolder"> Output parameter with the folder name created. </param>
  procedure CreateFolder(ABuilder: ICommandBuilder; var AFolder: string);

  /// <summary> Copy the application itself to the given path. Overwrite the file if it exists.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output info about app copy. </param>
  /// <param name="AFolder"> Target full path to copy the application </param>
  procedure CopyApp(ABuilder: ICommandBuilder; const AFolder: string);

  /// <summary> Adds the given path to the .profile user file. It also displays state 
  /// information to the user about the success or failure of this action. The user can be 
  /// guided to perform administration elevation procedures.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output info about path variable change. </param>
  /// <param name="AFolder"> Full path that will be added to path environment variable. </param>
  procedure UpdateEnvironmentPathLinux(ABuilder: ICommandBuilder; const AFolder: string);

  /// <summary> Adds the given path to the /etc/paths user file. It also displays state 
  /// information to the user about the success or failure of this action. The user can be 
  /// guided to perform administration elevation procedures.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output info about path variable change. </param>
  /// <param name="AFolder"> Full path that will be added to path environment variable. </param>
  procedure UpdateEnvironmentPathMacos(ABuilder: ICommandBuilder; const AFolder: string);

  /// <summary> Adds the given path to the path environment variable. It also displays state 
  /// information to the user about the success or failure of this action. The user can be 
  /// guided to perform administration elevation procedures.
  /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// output info about path variable change. </param>
  /// <param name="AFolder"> Full path that will be added to path environment variable. </param>
  procedure UpdateEnvironmentPathWindows(ABuilder: ICommandBuilder; const AFolder: string);

  {$ENDIF}

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
var
  LSubFolder: string;
begin
  LSubFolder := '.' + ChangeFileExt(ABuilder.ExeName, '');
  AFolder := ConcatPaths([GetUserDir, LSubFolder]);
  ABuilder.OutputColor('creating destination folder ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor(AFolder + #13#10, ABuilder.ColorTheme.Title);
  if not DirectoryExists(AFolder) then
  begin
    SetCurrentDir(GetUserDir);
    CreateDir(LSubFolder);
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

procedure UpdateEnvironmentPathLinux(ABuilder: ICommandBuilder; const AFolder: string);
var
  LProfile, LContent: string;
begin
  ABuilder.OutputColor(
    'updating environment path, this may require administration privileges '#13#10, 
    ABuilder.ColorTheme.Other);

  LProfile := ConcatPaths([GetUserDir, '.profile']);
  LContent := GetFileContent(LProfile);

  ABuilder.OutputColor('changing file ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor('~/.profile '#13#10, ABuilder.ColorTheme.Title);
  
  if not ContainsText(LContent, '.pasc:$PATH') then 
  begin
    LContent := LContent + #13#10 + 'PATH="$HOME/.pasc:$PATH"';
    SaveFileContent(LProfile, LContent);
  end;

  ABuilder.OutputColor('running ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor('source ~/.profile ', ABuilder.ColorTheme.Title);
  ABuilder.OutputColor('to make path changes to take effect '#13#10, ABuilder.ColorTheme.Other);
    
  LContent := GetResource('update-path-sh'); 
  SaveFileContent(ConcatPaths([GetUserDir, '.pasc', 'update-path.sh']), LContent);

  LContent := ShellCommand('bash', [ConcatPaths([GetUserDir, '.pasc', 'update-path.sh'])]);
  if LContent <> '' then
    ABuilder.OutputColor(LContent + #13#10, ABuilder.ColorTheme.Other);
end;

procedure UpdateEnvironmentPathMacos(ABuilder: ICommandBuilder; const AFolder: string);
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

    WriteLn('running shell script to make allow pasc to ben run on new path ', ApplicationName);

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

  ShellCommand('powershell', [ConcatPaths([AFolder, LName]), AFolder]);

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
  UpdateEnvironmentPathLinux(ABuilder, LAppFolder);
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