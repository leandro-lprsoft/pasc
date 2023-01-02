unit TestUtilsShell;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  StrUtils,
  fpcunit,
  testregistry,
  Utils.Shell;

type

  TTestUtilsShell = class(TTestCase)
  private
    FExePath: string;
  public
    procedure Setup; override;
  published
    procedure TestShellCommand;
    procedure TestShellCommandNotExists;
    procedure TestGetParametersFromDoubleQuotes;
    procedure TestGetParametersFromSingleQuotes;
  end;

implementation

procedure TTestUtilsShell.Setup;
begin
  FExePath := ExtractFilePath(ParamStr(0));
end;

procedure TTestUtilsShell.TestShellCommand;
var
  LActual, LExpected: string;
begin
  LActual := StringReplace(ShellCommand('pwd', []), #13#10, '', [rfReplaceAll]);
  LExpected := GetCurrentDir;
  AssertEquals(GetCurrentDir, LActual);
end;

procedure TTestUtilsShell.TestShellCommandNotExists;
var
  LActual: string;
begin
  LActual := StringReplace(ShellCommand('not_exists_command', []), #13#10, '', [rfReplaceAll]);
  AssertTrue('Should return an error message: "Error when executing', ContainsText(LActual, 'Error when executing'));
end;

procedure TTestUtilsShell.TestGetParametersFromDoubleQuotes;
var
  LArray: TArray<string> = [];
  LParams: string;
begin
  // arrange
  LParams := 'command "hello world!"';

  // act
  LArray := GetParametersFrom(LParams);

  // assert
  AssertEquals(2, Length(LArray));
  AssertEquals('command', LArray[0]);
  AssertEquals('"hello world!"', LArray[1]);
end;

procedure TTestUtilsShell.TestGetParametersFromSingleQuotes;
var
  LArray: TArray<string> = [];
  LParams: string;
begin
  // arrange
  LParams := 'command ''hello world!''';

  // act
  LArray := GetParametersFrom(LParams);

  // assert
  AssertEquals(2, Length(LArray));
  AssertEquals('command', LArray[0]);
  AssertEquals('"hello world!"', LArray[1]);
end;


initialization
  RegisterTest(TTestUtilsShell);

end.
