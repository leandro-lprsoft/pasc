unit Command.Clean;

{$MODE DELPHI}{$H+}

interface

uses
  Command.Interfaces;

  procedure CleanCommand(ABuilder: ICommandBuilder);
  procedure Registry(ABuilder: ICommandBuilder);

implementation

uses
  SysUtils,
  StrUtils,
  FileUtil,
  Command.Colors;

const
  TARGETS: array [0..1] of string = ('lib', 'backup');

var
  AnsweredAll: boolean;

procedure CleanDirectory(ABuilder: ICommandBuilder; const ACurrentDir: string);
var
  LSearch: TSearchRec;
  LCurrentDir: string;
  LKey: char;
  LInvalidKey: Boolean;
begin
  if FindFirst(ConcatPaths([ACurrentDir, AllFilesMask]), faDirectory, LSearch) = 0 then
    try
      repeat
        if (LSearch.Attr and faDirectory) <> 0 then
        begin
          LCurrentDir := ConcatPaths([ACurrentDir, LSearch.Name]);
          if AnsiMatchText(LSearch.Name, TARGETS) then
          begin
            ABuilder.OutputColor(
              'folder will be removed: ' + IncludeTrailingPathDelimiter(ACurrentDir), 
              ABuilder.ColorTheme.Other);
            ABuilder.OutputColor(LSearch.Name, ABuilder.ColorTheme.Value);
            if not AnsweredAll then
            begin
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
                  Write('Invalid input. Are you sure? [c]ancel, [y]es, [a]ll: ');
              end;    
            end;   
            //WriteLn; //for linux/macos?

            if DeleteDirectory(LCurrentDir, True) then 
            begin
              RemoveDir(LCurrentDir);
              WriteLn('...removed.');
            end;
          end
          else
            if LeftStr(LSearch.Name, 1) <> '.' then
            begin
              CleanDirectory(ABuilder, LCurrentDir);
            end;
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