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
    procedure TestCommandWatchBasic;
  end;

implementation

uses
  Resources,
  StrUtils,
  MockCommandBuilder,
  Command.Watch,
  Utils.IO,
  Utils.Shell;

procedure TTestCommandWatch.SetUp;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.Watch.Registry(FBuilder);

  FWorkingFolder := ConcatPaths([GetTempDir, '.' + ChangeFileExt(FExeName, '')]);
end;

procedure TTestCommandWatch.TearDown;
begin
  SetCurrentDir(FCurrentDir);
end;

procedure TTestCommandWatch.TestCommandWatchRegistry;
begin
  AssertEquals('watch', FBuilder.Commands[0].Name);
end;

procedure TTestCommandWatch.TestCommandWatchBasic;
begin
  // arrange
  FBuilder.UseArguments(['test']);
  FBuilder.Parse;

  // cannot call TestPasc again because it would produce recursive calls indenitely
  // so we need to change ShellCommand and check if it's being called.
  ShellExecute := @MockShell;
  
  WatchCommand(FBuilder);
    
  // assert
  AssertTrue(
    'Output should contain keyword: "Starting" ',
    ContainsText(MockOutputCaptured, 'Starting'));
  AssertTrue(
    'Should call the test app: "TestPasc" ',
    ContainsText(MockShellCapture, 'TestPasc'));
end;

initialization
  RegisterTest(TTestCommandWatch);
  
end.