unit TestCommandTest;

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

  TTestCommandTest= class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FExeName: string;
    FWorkingFolder: string;
    FCurrentDir: string;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommandTestRegistry;    
    procedure TestCommandTestBasic;
    procedure TestGetTestExecutable;
    procedure TestFindTestProject;
  end;

implementation

uses
  Resources,
  StrUtils,
  MockCommandBuilder,
  Command.Test,
  Utils.IO,
  Utils.Shell;

procedure TTestCommandTest.SetUp;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.Test.Registry(FBuilder);

  FWorkingFolder := ConcatPaths([GetTempDir, '.' + ChangeFileExt(FExeName, '')]);
end;

procedure TTestCommandTest.TearDown;
begin
  SetCurrentDir(FCurrentDir);
end;

procedure TTestCommandTest.TestCommandTestRegistry;
begin
  AssertEquals('test', FBuilder.Commands[0].Name);
end;

procedure TTestCommandTest.TestCommandTestBasic;
begin
  // arrange
  FBuilder.UseArguments(['test']);
  FBuilder.Parse;

  // cannot call TestPasc again because it would produce recursive calls indenitely
  // so we need to change ShellCommand and check if it's being called.
  ShellExecute := @MockShell;
  
  TestCommand(FBuilder);
    
  // assert
  AssertTrue(
    'Output should contain keyword: "Starting" ',
    ContainsText(MockOutputCaptured, 'Starting'));
  AssertTrue(
    'Should call the test app: "TestPasc" ',
    ContainsText(MockShellCapture, 'TestPasc'));
end;

procedure TTestCommandTest.TestGetTestExecutable;
begin
  AssertEquals(ParamStr(0), GetTestExecutable);
end;

procedure TTestCommandTest.TestFindTestProject;
var
  LPath: string;
begin
  LPath := FCurrentDir;
  if not ((EndsText('tests', LPath) or EndsText('tests' + PathDelim, LPath))) then
    LPath := ConcatPaths([LPath, 'tests']);
  AssertEquals(
    ChangeFileExt(ParamStr(0), '.lpr'), 
    FindTestProject(LPath));
end;

initialization
  RegisterTest(TTestCommandTest);
  
end.