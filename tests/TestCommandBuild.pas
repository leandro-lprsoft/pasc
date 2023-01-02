unit TestCommandBuild;

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

  TTestCommandBuild= class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FExeName: string;
    FWorkingFolder: string;
    FCurrentDir: string;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommandBuildRegistry;    
    procedure TestCommandBuildBasic;
    procedure TestFindProject;
  end;

implementation

uses
  Resources,
  StrUtils,
  MockCommandBuilder,
  Command.Build,
  Utils.Shell;

procedure TTestCommandBuild.SetUp;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.Build.Registry(FBuilder);

  FWorkingFolder := ConcatPaths([GetTempDir, '.' + ChangeFileExt(FExeName, '')]);
end;

procedure TTestCommandBuild.TearDown;
begin
  SetCurrentDir(FCurrentDir);
end;

procedure TTestCommandBuild.TestCommandBuildRegistry;
begin
  AssertEquals('build', FBuilder.Commands[0].Name);
end;

procedure TTestCommandBuild.TestCommandBuildBasic;
begin
  // arrange
  FBuilder.UseArguments(['build']);
  FBuilder.Parse;

  // cannot call TestPasc again because it would produce recursive calls indenitely
  // so we need to change ShellCommand and check if it's being called.
  ShellExecute := @MockShell;
  BuildCommand(FBuilder);
    
  // assert
  AssertTrue(
    'Output should contain keyword: "lazbuild" ',
    ContainsText(MockOutputCaptured, 'lazbuild'));
  AssertTrue(
    'Output should contain keyword: "lazbuild" ',
    ContainsText(MockShellCapture, 'lazbuild'));
end;

procedure TTestCommandBuild.TestFindProject;
begin
  AssertEquals(
    ConcatPaths([ExtractFilePath(ParamStr(0)), 'TestPasc.lpr']),
    FindProject(ExtractFilePath(ParamStr(0)), ''));
end;

initialization
  RegisterTest(TTestCommandBuild);
  
end.