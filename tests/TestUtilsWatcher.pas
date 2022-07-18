unit TestUtilsWatcher;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  fpcunit,
  testregistry,
  Command.Interfaces,  
  Utils.Interfaces,
  Utils.Watcher;

type

  TTestUtilsWatcher = class(TTestCase)
  private
    FCurrentDir: string;
    FExeName: string;
    FWorkingFolder: string;
    FBuilder: ICommandBuilder;
    FPathWatcher: IPathWatcher;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestBasicWatch;
    procedure TestIgnoreStartText;
    procedure TestIgnoreFolder;
    procedure TestIgnoreFile;
    procedure TestIgnoreExtension;
  end;

implementation

uses
  Command.Builder,
  Command.Clean,
  MockCommandBuilder;

var
  CaptureWatchExecute: string;

function MockWatchExecute(const AContent: string): Boolean;
begin
  CaptureWatchExecute := AContent;
  Result := True;
end;

procedure TTestUtilsWatcher.Setup;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);

  FPathWatcher := TPathWatcher.New;
  CaptureWatchExecute := '';

  FWorkingFolder := ConcatPaths([GetTempDir, '.' + ChangeFileExt(FExeName, '')]);
  AnsweredAll := True;
    
  // clean old data
  if DirectoryExists(FWorkingFolder) then
    DeleteFolder(FBuilder, GetTempDir, '.' + ChangeFileExt(FExeName, ''));

  // create new working folder
  SetCurrentDir(GetTempDir);
  CreateDir('.' + ChangeFileExt(FExeName, ''));
  SetCurrentDir(FWorkingFolder);
end;

procedure TTestUtilsWatcher.TearDown;
begin
  SetCurrentDir(FCurrentDir);
  AnsweredAll := False;
end;

procedure TTestUtilsWatcher.TestBasicWatch;
begin

end;

procedure TTestUtilsWatcher.TestIgnoreStartText;
begin
  
end;

procedure TTestUtilsWatcher.TestIgnoreFolder;
begin
  
end;

procedure TTestUtilsWatcher.TestIgnoreFile;
begin
  
end;

procedure TTestUtilsWatcher.TestIgnoreExtension;
begin
  
end;

initialization
  RegisterTest(TTestUtilsWatcher);

end.