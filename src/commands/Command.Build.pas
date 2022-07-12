/// <summary> This unit contains procedures to configure and execute a command to build the
/// specified project or to build the first project found on current dir. 
/// </summary>
unit Command.Build;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  /// <summary> This command uses the lazbuild tool to build the specified project. If one
  /// was not provided, try to find one on current directory and builds it.
  /// 
  /// The iead is just to have a build command to facilitate the watch command that pasc 
  /// implements.
  /// </summary>
  /// <param name="ABuilder"> Command builder that will provide the output callback to print
  /// info about the command execution </param>
  procedure BuildCommand(ABuilder: ICommandBuilder);

  /// <summary> Registry a build command using the command builder from pascli, also sets. 
  /// options and usage help info. </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// registry the command. </param>  
  procedure Registry(ABuilder: ICommandBuilder);

  /// <summary> Checks if specified project exists, if a project file is not provided
  /// try to find one on current directory. Returns complete file name if a project
  /// is found. </summary>
  /// <param name="AProjectDir"> Folder to find the lazarus project </param>
  /// <param name="AProjectFile"> Project file specified. </param>
  function FindProject(const AProjectDir, AProjectFile: string): string;

implementation

uses
  Classes,
  SysUtils,
  Command.Colors,
  Utils.Shell;

function FindProject(const AProjectDir, AProjectFile: string): string;
var
  LSearch: TSearchRec;
begin
  Result := '';
  if not DirectoryExists(AProjectDir) then
    exit;

  if FileExists(ConcatPaths([AProjectDir, AProjectFile])) then
    exit(ConcatPaths([AProjectDir, AProjectFile]));
  
  if FindFirst(ConcatPaths([AProjectDir, '*.lpr']), faAnyFile, LSearch) = 0 then
    try
      repeat
        if ((LSearch.Attr and faAnyFile) <> 0) and (ExtractFileExt(LSearch.Name) = '.lpr') then
        begin
          FindClose(LSearch);
          Exit(ConcatPaths([AProjectDir, LSearch.Name]));
        end;
      until FindNext(LSearch) <> 0;
    finally
      FindClose(LSearch);
    end;  
end;

procedure BuildCommand(ABuilder: ICommandBuilder);
var
  LProjectFile: string = '';
  LOutput: string;
begin
  if ABuilder.HasArguments then
    LProjectFile := ABuilder.Arguments[0].Value;
  LProjectFile := FindProject(GetCurrentDir, LProjectFile);

  if Trim(LProjectFile) = '' then
  begin
    ABuilder.OutputColor(
      'There is no project file on current directory or the specified file ',
      ABuilder.ColorTheme.Other);
    ABuilder.OutputColor(
      'does not exist.'#13#10,
      LightRed);
    exit;      
  end;
  
  try
    LOutput := ShellExecute('lazbuild', ['-v']);
  except
    on E: Exception do
    begin
      ABuilder.OutputColor('lazbuild does not exist or its path is not configure to be in PATH environment.',
      LightRed);
      exit;      
    end;
  end;
  
  // shell command to build the project
  try
    LOutput := ShellExecute('lazbuild', [LProjectFile]);
    ABuilder.OutputColor(LOutput + #13#10, ABuilder.ColorTheme.Other);
  except
    on E: Exception do
    begin
      ABuilder.OutputColor('error trying to build the project file.',
      LightRed);
      exit;      
    end;
  end;

end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'build',
      'build the project in current directory or one provided as an argument.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' build'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' build sample1.lpr',
      @BuildCommand,
      [ccRequiresOneArgument])
      .AddOption(
          'd', 'debug', 
          'build the project in debug mode, .lpi file must have a debug mode', ['r'])
      .AddOption(
          'r', 'release', 
          'build the project in release mode, .lpi file must have a release mode', ['d'])
end;

end.