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
  
  // act
  // can´t call TestCommand, it will create an infinete loop
  // mock shell command on builder
  // create a property ShellCallback = procedure (const ACommand: string; params: TArray<string>)
  // command test and others should use injected ShellCallback
  // TestCommand(FBuilder);
  
  // assert
  AssertTrue(
    'Output should contain keyword: "Starting" ',
    ContainsText(MockOutputCaptured, 'sample_test'));
end;

procedure TTestCommandTest.TestGetTestExecutable;
begin
  AssertTrue('todo!', false);
end;

procedure TTestCommandTest.TestFindTestProject;
begin
  AssertTrue('todo!', false);
end;

initialization
  RegisterTest(TTestCommandTest);
  
end.