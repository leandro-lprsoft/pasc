unit TestCommandInstall;

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

  TTestCommandInstall= class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FExeName: string;
    FAppFolder: string;
    FAppFile: string;
    FCheckFile: string;
    FCurrentDir: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInstallCommandRegistry;
    procedure TestCreateFolder;
    procedure TestCopyApp;
    procedure TestUpdateEnvironmentPathLinux;
    procedure TestUpdateEnvironmentPathMacos;
    procedure TestUpdateEnvironmentPathWindows;
    procedure TestInstallCommand;
  end;

implementation

uses
  Resources,
  StrUtils,
  MockCommandBuilder,
  Command.Install,
  Utils.IO,
  Utils.Shell;

procedure TTestCommandInstall.SetUp;
begin
  FCurrentDir := GetCurrentDir;
  FExeName := ExtractFileName(ParamStr(0));
  FBuilder := TCommandBuilder.Create(FExeName);
  MockSetup(FBuilder);
  Command.Install.Registry(FBuilder);

  FAppFolder := ConcatPaths([GetUserDir, '.' + ChangeFileExt(FExeName, '')]);
  FAppFile := ConcatPaths([FAppFolder, FExeName]);

  if FileExists(FAppFile) then
    DeleteFile(FAppFile);
  if DirectoryExists(FAppFolder) then
    RemoveDir(FAppFolder);
end;

procedure TTestCommandInstall.TearDown;
begin
  SetCurrentDir(FCurrentDir);
end;

procedure TTestCommandInstall.TestInstallCommandRegistry;
begin
  AssertEquals('install', FBuilder.Commands[0].Name);
end;

procedure TTestCommandInstall.TestCreateFolder;
var
  LCreatedFolder: string;
begin
  CreateFolder(FBuilder, LCreatedFolder);
  AssertTrue('Folder ' + LCreatedFolder + ' should exist', DirectoryExists(LCreatedFolder));
end;

procedure TTestCommandInstall.TestCopyApp;
var
  LCreatedFolder, LTargetFile: string;
begin
  CreateFolder(FBuilder, LCreatedFolder);
  CopyApp(FBuilder, LCreatedFolder);
  LTargetFile := ConcatPaths([LCreatedFolder, FExeName]);
  AssertTrue('App should exist on app folder', FileExists(LTargetFile));
end;

procedure TTestCommandInstall.TestUpdateEnvironmentPathLinux;
begin
  {$IFDEF LINUX}
  AssertTrue('todo!', False);
  {$ENDIF}
end;

procedure TTestCommandInstall.TestUpdateEnvironmentPathMacos;
begin
  {$IFDEF MACOS}
  AssertTrue('todo!', False);
  {$ENDIF}
end;

procedure TTestCommandInstall.TestUpdateEnvironmentPathWindows;
var
  LOutput, LCreatedFolder: string;
begin
  {$IFDEF WINDOWS}
  CreateFolder(FBuilder, LCreatedFolder);
  UpdateEnvironmentPathWindows(FBuilder, FAppFolder);

  //SaveFileContent(ConcatPaths([GetCurrentDir, 'get-path.ps1'], ))

  //LOutput := ShellCommand('powershell', ['']);
  AssertEquals('output', LOutput);
  AssertTrue('should contain app folder in path', ContainsText(LOutput, FAppFolder));
  {$ENDIF}
end;

procedure TTestCommandInstall.TestInstallCommand;
begin
  AssertTrue('todo!', False);
end;

initialization

  RegisterTest(TTestCommandInstall);
end.

