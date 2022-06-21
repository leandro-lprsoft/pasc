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
  FileUtil;

const
  TARGETS: array [0..1] of string = ('lib', 'backup');

var
  AnsweredAll: boolean;

procedure WriteGitIgnore(const ACurrentDir: string);
begin
end;

procedure CleanDirectory(const ACurrentDir: string);
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
            Write('directory will be removed: ', LCurrentDir);
            if not AnsweredAll then
            begin
              Write(' Are you sure? [c]abort, [y]es, [a]ll: ');
              LInvalidKey := True;
              while LInvalidKey do
              begin
                Read(LKey);
                if (LKey = #13) or (LKey = #10) then
                  continue;

                case LKey of 
                  'a': 
                    begin
                      AnsweredAll := True;
                      LInvalidKey := False;
                    end;
                  'y': 
                    begin
                      AnsweredAll := False;
                      LInvalidKey := False;
                    end;
                  'c', #3: 
                    begin
                      LInvalidKey := False;
                      WriteLn;
                      raise Exception.Create('Cleaning aborted.');
                    end;
                  else
                  begin
                    WriteLn;
                    Write('Invalid input. Are you sure? [a]bort, [y]es, [a]ll: ');
                  end;
                end;    
              end;
            end;   
            WriteLn;  

            if DeleteDirectory(LCurrentDir, True) then 
            begin
              RemoveDir(LCurrentDir);
              WriteLn('  ...removed.');
            end;
          end
          else
            if LeftStr(LSearch.Name, 1) <> '.' then
            begin
              CleanDirectory(LCurrentDir);
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
  
  WriteLn('Cleanning current path: ', GetCurrentDir);
  try
    CleanDirectory(GetCurrentDir);
  except 
    on E: Exception do
    begin
      if not SameText(E.Message, 'Cleaning aborted.') then
        raise;
      WriteLn(E.Message);
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