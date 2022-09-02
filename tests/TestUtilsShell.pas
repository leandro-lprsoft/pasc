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
  end;

implementation

procedure TTestUtilsShell.Setup;
begin
  FExePath := ExtractFilePath(ParamStr(0));
end;

procedure TTestUtilsShell.TestShellCommand;
var
  LActual: string;
begin
  LActual := StringReplace(ShellCommand('pwd', []), #13#10, '', [rfReplaceAll]);
  AssertEquals(GetCurrentDir, LActual);
end;

procedure TTestUtilsShell.TestShellCommandNotExists;
var
  LActual: string;
begin
  LActual := StringReplace(ShellCommand('not_exists_command', []), #13#10, '', [rfReplaceAll]);
  AssertTrue('Should return an error message: "Error when executing', ContainsText(LActual, 'Error when executing'));
end;

initialization
  RegisterTest(TTestUtilsShell);

end.
