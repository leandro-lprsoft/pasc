/// <summary> This unit contains procedures to configure and execute a command to clean folders. 
/// </summary>
unit Command.Clean;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  /// <summary> This command recursively from the current path searches for folders named 
  /// "lib" and "backup" to remove them. However, before removing them, it asks the user 
  /// for confirmation. Confirmation that can be suppressed if the --force option is given.
  /// </summary>
  /// <param name="ABuilder"> Command builder that will provide the output callback to print
  /// info about the command execution </param>
  procedure CleanCommand(ABuilder: ICommandBuilder);

  /// <summary> Registry a clean command using the command builder from pascli, also sets. 
  /// options and usage help info. /// </summary>
  /// <param name="ABuilder"> Command builder of the main application that will be used to
  /// registry the command. </param>
  procedure Registry(ABuilder: ICommandBuilder);

implementation

uses
  SysUtils,
  StrUtils,
  FileUtil,
  Command.Colors;

const
  TARGETS: array [0..1] of AnsiString = ('lib', 'backup');
  IGNORE: array [0..3] of AnsiString = ('.', '..', '.git', '.vscode');

var
  AnsweredAll: boolean;

procedure CheckAnswer(ABuilder: ICommandBuilder);
var
  LKey: char;
  LInvalidKey: Boolean;
begin
  if AnsweredAll then
    Exit;
  
  ABuilder.OutputColor(' Are you sure? ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor('[c]ancel ', LightRed);
  ABuilder.OutputColor('[y]es, [a]ll: ', ABuilder.ColorTheme.Value);
  ABuilder.OutputColor(': ', ABuilder.ColorTheme.Other);
  LInvalidKey := True;
  while LInvalidKey do
  begin
    Read(LKey);
    if (LKey = #13) or (LKey = #10) then
      continue;

    if (LKey = 'c') then
      //WriteLn; //for linux/macos?
      raise Exception.Create('Cleaning aborted.');

    AnsweredAll := LKey = 'a';
    LInvalidKey := not AnsiMatchText(LKey, ['a', 'y', 'c']);

    if LInvalidKey then 
      ABuilder.OutputColor(
        'Invalid input. Are you sure? [c]ancel, [y]es, [a]ll: ', 
        ABuilder.ColorTheme.Other);
  end;    
end;

procedure DeleteFolder(ABuilder: ICommandBuilder; const ACurrentDir, AFolder: string);
var
  LCurrentDir: string;
begin
  ABuilder.OutputColor(
    'folder will be removed: ' + IncludeTrailingPathDelimiter(ACurrentDir), 
    ABuilder.ColorTheme.Other);
  ABuilder.OutputColor(AFolder, ABuilder.ColorTheme.Value);

  CheckAnswer(ABuilder);
  //WriteLn; //for linux/macos?

  LCurrentDir := ConcatPaths([ACurrentDir, AFolder]);
  if DeleteDirectory(LCurrentDir, True) then 
  begin
    RemoveDir(LCurrentDir);
    ABuilder.OutputColor('...removed.', ABuilder.ColorTheme.Other);
  end;
end;

procedure CleanDirectory(ABuilder: ICommandBuilder; const ACurrentDir: string);
var
  LSearch: TSearchRec;
begin
  if FindFirst(ConcatPaths([ACurrentDir, AllFilesMask]), faDirectory, LSearch) <> 0 then
    exit;

  try
    repeat
      if ((LSearch.Attr and faDirectory) <> 0) and 
         (not AnsiMatchText(LSearch.Name, IGNORE)) then
      begin
        if AnsiMatchText(LSearch.Name, TARGETS) then
          DeleteFolder(ABuilder, ACurrentDir, LSearch.Name)
        else
          CleanDirectory(ABuilder, ConcatPaths([ACurrentDir, LSearch.Name]));
      end;
    until FindNext(LSearch) <> 0;
  finally
    FindClose(LSearch);
  end;
end;

procedure CleanCommand(ABuilder: ICommandBuilder);
begin
  AnsweredAll := ABuilder.CheckOption('f');

  ABuilder.OutputColor('Cleanning current path: ', ABuilder.ColorTheme.Other);
  ABuilder.OutputColor(GetCurrentDir + #13#10, ABuilder.ColorTheme.Title);
  
  try
    CleanDirectory(ABuilder, GetCurrentDir);
  except 
    on E: Exception do
    begin
      if not SameText(E.Message, 'Cleaning aborted.') then
        raise;
      ABuilder.OutputColor(E.Message + #13#10, LightRed);
    end;
  end;
end;

procedure Registry(ABuilder: ICommandBuilder);
begin
  ABuilder
    .AddCommand(
      'clean',
      'Removes the following folders lib, backup, to make possible a clean build.'#13#10 +
      'Ex: ' + ABuilder.ExeName + ' clean',
      @CleanCommand,
      [ccNoParameters])
      .AddOption('f', 'force', 'do not ask for confirmation before delete.', []);
end;

end.