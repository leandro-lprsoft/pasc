unit TestCommandClean;

{$mode objfpc}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  fpcunit, 
  testregistry,
  Command.Interfaces,
  Command.Builder;

type

  TTestCommandClean= class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FExeName: string;
    FCheckFile: string;
    FCurrentDir: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRegistry;
    procedure TestCleanForce;
    procedure TestCleanMockInput;
  end;

implementation

uses
  StrUtils,
  MockCommandBuilder,
  Command.Clean,
  Utils.IO;

procedure TTestCommandClean.SetUp;
var
  LTempDir: string;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFilePath(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.Clean.Registry(FBuilder);

  SetCurrentDir(GetTempDir);
  LTempDir := ConcatPaths([GetTempDir, 'TestPasc']);
  if not DirectoryExists(LTempDir) then
    CreateDir('TestPasc');
  SetCurrentDir(LTempDir);

  if not DirectoryExists(ConcatPaths([LTempDir, 'lib'])) then
    CreateDir('lib');

  FCheckFile := ConcatPaths([LTempDir, 'lib', 'test.txt']);

  SaveFileContent(FCheckFile, 'test content');  
end;

procedure TTestCommandClean.TearDown;
begin
  SetCurrentDir(FCurrentDir);
end;

procedure TTestCommandClean.TestRegistry;
begin
  AssertEquals('1 equals 1', 1, 1);
end;

procedure TTestCommandClean.TestCleanForce;
begin
  FBuilder.UseArguments(['clean', '--force']);
  FBuilder.Parse;

  CleanCommand(FBuilder);

  // assert
  AssertTrue(
    'File should not exist.', 
    not FileExists(FCheckFile));
end;

procedure TTestCommandClean.TestCleanMockInput;
begin
  FBuilder.UseArguments(['clean']);
  FBuilder.Parse;

  MockInputLnResult := 'y';
  CleanCommand(FBuilder);

  // assert
  AssertTrue('Should ask for confirmation', ContainsText(MockOutputCaptured, 'Are you sure?'));
  AssertTrue(
    'File should not exist.', 
    not FileExists(FCheckFile));
end;

initialization

  RegisterTest(TTestCommandClean);
end.

