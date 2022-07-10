unit TestCommandNew;

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

  TTestCommandNew= class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FExeName: string;
    FProjectsFolder: string;
    FCurrentDir: string;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNewCommandRegistry;    
    procedure TestNewCommandBasic;
    procedure TestCreateSupportFilesForVSCode;
    procedure TestCreateProjectFiles;
    procedure TestInitializeBoss;
    procedure TestChangeBossFileSourcePath;
    procedure TestInitializeGit;
    procedure TestCreateProjectFolders;
  end;

implementation

uses
  Resources,
  StrUtils,
  MockCommandBuilder,
  Command.New,
  Command.Clean,
  Utils.IO,
  Utils.Shell;

procedure TTestCommandNew.SetUp;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.New.Registry(FBuilder);

  FProjectsFolder := ConcatPaths([GetTempDir, '.' + ChangeFileExt(FExeName, '')]);
  AnsweredAll := True;
  
  if DirectoryExists(FProjectsFolder) then
    DeleteFolder(FBuilder, GetTempDir, '.' + ChangeFileExt(FExeName, ''));
end;

procedure TTestCommandNew.TearDown;
begin
  SetCurrentDir(FCurrentDir);
  AnsweredAll := False;
end;

procedure TTestCommandNew.TestNewCommandRegistry;
begin
  AssertEquals('new', FBuilder.Commands[0].Name);
end;

procedure TTestCommandNew.TestNewCommandBasic;
begin
  AssertTrue('todo', false);
  
  //NewCommand(FBuilder);
  //AssertTrue('todo!', false);
end;

procedure TTestCommandNew.TestCreateSupportFilesForVSCode;
begin
  AssertTrue('todo', false);
end;

procedure TTestCommandNew.TestCreateProjectFiles;
begin
  AssertTrue('todo', false);
end;

procedure TTestCommandNew.TestInitializeBoss;
begin
  AssertTrue('todo', false);
end;

procedure TTestCommandNew.TestChangeBossFileSourcePath;
begin
  AssertTrue('todo', false);
end;

procedure TTestCommandNew.TestInitializeGit;
begin
  AssertTrue('todo', false);
end;

procedure TTestCommandNew.TestCreateProjectFolders;
begin
  AssertTrue('todo', false);
end;

initialization
  RegisterTest(TTestCommandNew);
  
end.