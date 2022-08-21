unit TestCommandAdd;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  fpcunit, 
  testregistry,
  Command.Interfaces,
  Command.Builder,
  Command.Add;

type

  TTestCommandAdd= class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FExeName: string;
    FWorkingFolder: string;
    FCurrentDir: string;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCommandAddRegistry;    
    procedure TestCommandAddBasic;
    procedure TestCommandAddDocs;
    procedure TestCommandAddNoOptions;
  end;

implementation

uses
  Resources,
  StrUtils,
  MockCommandBuilder,
  Command.Build,
  Utils.IO;

procedure TTestCommandAdd.SetUp;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.Add.Registry(FBuilder);

  FWorkingFolder := ConcatPaths([GetTempDir, '.' + ChangeFileExt(FExeName, '')]);
end;

procedure TTestCommandAdd.TearDown;
begin
  SetCurrentDir(FCurrentDir);
end;

procedure TTestCommandAdd.TestCommandAddRegistry;
begin
  AssertEquals('add', FBuilder.Commands[0].Name);
end;

procedure TTestCommandAdd.TestCommandAddBasic;
begin
  // arrange
  FBuilder.UseArguments(['add', '--tests']);
  FBuilder.Parse;

  SetCurrentDir(FWorkingFolder);
  SaveFileContent(ConcatPaths([FWorkingFolder, 'myproject.lpr']), 'program myproject;');

  // act
  AddCommand(FBuilder);

  // assert
  AssertTrue(
    'tests sub folder should exist in ' + FWorkingFolder, 
    DirectoryExists(ConcatPaths([FWorkingFolder, 'tests'])));
  AssertTrue(
    'Output should contain keyword: "test project added with success" ',
    ContainsText(MockOutputCaptured, 'test project added with success'));
end;

procedure TTestCommandAdd.TestCommandAddNoOptions;
begin
  // arrange
  FBuilder.UseArguments(['add']);
  FBuilder.Parse;

  SetCurrentDir(FWorkingFolder);
  SaveFileContent(ConcatPaths([FWorkingFolder, 'myproject.lpr']), 'program myproject;');
  
  // act
  AddCommand(FBuilder);

  // assert
  AssertTrue(
    'Output should contain keyword: "you must specify at least one of the options" ',
    ContainsText(MockOutputCaptured, 'you must specify at least one of the options'));
end;

procedure TTestCommandAdd.TestCommandAddDocs;
begin
  // arrange
  FBuilder.UseArguments(['add', '--docs']);
  FBuilder.Parse;

  SetCurrentDir(FWorkingFolder);
  SaveFileContent(ConcatPaths([FWorkingFolder, 'myproject.lpr']), 'program myproject;');

  // act
  AddCommand(FBuilder);

  // assert
  AssertTrue(
    'tests sub folder should exist in ' + FWorkingFolder, 
    DirectoryExists(ConcatPaths([FWorkingFolder, 'docs'])));
  AssertTrue(
    'Output should contain keyword: "documentation resources added with success" ',
    ContainsText(MockOutputCaptured, 'documentation resources added with success')); 
end;

initialization
  RegisterTest(TTestCommandAdd);
  
end.