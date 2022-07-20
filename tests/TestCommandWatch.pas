unit TestCommandWatch;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  fpcunit, 
  testregistry,
  Command.Interfaces,
  Command.Builder;

type

  TTestCommandWatch= class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FExeName: string;
    FWorkingFolder: string;
    FCurrentDir: string;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommandWatchRegistry;    
    procedure TestCommandWatchCheckIfWatcherCallbackIsCalled;
    procedure TestCommandWatchCheckIfShellExecuteCallbackIsCalled;
  end;

implementation

uses
  Resources,
  StrUtils,
  MockCommandBuilder,
  Command.Clean,
  Command.Watch,
  Utils.IO,
  Utils.Shell;

var
  Finished: LongInt = 0;
  Builder: ICommandBuilder;

procedure RunWatchCommand;
begin
  try
    WatchCommand(Builder);
  finally
    InterLockedIncrement(Finished);
  end;
end;

procedure TTestCommandWatch.SetUp;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.Watch.Registry(FBuilder);

  MockCaptureWatchExecute := '';
  MockShellCapture := '';
  CommandWatchTimeout := 3000;
  Finished := 0;
  Builder := FBuilder;

  FWorkingFolder := ConcatPaths([GetTempDir, ChangeFileExt(FExeName, '')]);
  
  AnsweredAll := True;
    
  // clean old data
  if DirectoryExists(FWorkingFolder) then
    DeleteFolder(FBuilder, GetTempDir, ChangeFileExt(FExeName, ''));

  // create new working folder
  SetCurrentDir(GetTempDir);
  CreateDir(ChangeFileExt(FExeName, ''));
  SetCurrentDir(FWorkingFolder);
end;

procedure TTestCommandWatch.TearDown;
begin
  AnsweredAll := False;
  SetCurrentDir(FCurrentDir);
  ShellExecute := @ShellCommand;
  RunWatcherCallback := @RunUserCommandAsRequested;
  Builder := nil;
end;

procedure TTestCommandWatch.TestCommandWatchRegistry;
begin
  AssertEquals('watch', FBuilder.Commands[0].Name);
end;

procedure TTestCommandWatch.TestCommandWatchCheckIfWatcherCallbackIsCalled;
var
  LThread: TThread;
begin
  // arrange
  FBuilder.UseArguments(['watch', '.']);
  FBuilder.Parse;

  // cannot call TestPasc again because it would produce recursive calls indenitely
  // so we need to change ShellCommand and check if it's being called.
  ShellExecute := @MockShell;
  RunWatcherCallback := @MockWatchExecute;

  SaveFileContent(ConcatPaths([FWorkingFolder, 'test_for_watch.lpr']), 'test content');
  
  LThread := TThread.CreateAnonymousThread(RunWatchCommand);
  LThread.FreeOnTerminate := True;
  LThread.Start;

  Sleep(50);
  SaveFileContent(ConcatPaths([FWorkingFolder, 'test_for_watch.txt']), 'test content');

  while Finished = 0 do
    Sleep(50);
      
  // assert
  AssertTrue(
    'Output should contain keyword: "test_for_watch.txt" ' + MockCaptureWatchExecute,
     ContainsText(MockCaptureWatchExecute, 'test_for_watch.txt'));
end;

procedure TTestCommandWatch.TestCommandWatchCheckIfShellExecuteCallbackIsCalled;
var
  LThread: TThread;
begin
  // arrange
  CommandWatchTimeout := 1000;
  FBuilder.UseArguments(['watch', '--build', '--test', '.']);
  FBuilder.Parse;
  
  // cannot call TestPasc again because it would produce recursive calls indenitely
  // so we need to change ShellCommand and check if it's being called.
  ShellExecute := @MockShell;
  RunWatcherCallback := @RunUserCommandAsRequested; // original callback

  SaveFileContent(ConcatPaths([FWorkingFolder, 'test_for_watch.lpr']), 'TTestPascRunner = class(TTestRunner)');
  SaveFileContent(ConcatPaths([FWorkingFolder, 'test_for_watch.exe']), 'just for test');
  
  LThread := TThread.CreateAnonymousThread(RunWatchCommand);
  LThread.FreeOnTerminate := True;
  LThread.Start;

  Sleep(50);
  SaveFileContent(ConcatPaths([FWorkingFolder, 'test_for_watch.txt']), 'test content');

  while Finished = 0 do
    Sleep(50);
      
  // assert
  AssertTrue(
    'Output should contain keyword: "lazbuild" - Check capture: ' + MockShellCapture,
     ContainsText(MockShellCapture, 'lazbuild'));
  AssertTrue(
    'Output should contain keyword: "test_for_watch.exe" - Check capture: ' + MockShellCapture,
     ContainsText(MockShellCapture, 'test_for_watch.exe'));
end;

initialization
  RegisterTest(TTestCommandWatch);
  
end.