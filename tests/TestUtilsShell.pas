unit TestUtilsShell;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  fpcunit,
  testregistry,
  Command.Interfaces,
  Command.Builder,
  Utils.Shell;

type

  TTestUtilsShell = class(TTestCase)
  private
    FExePath: string;
    procedure Setup; override;
  published
    procedure TestShellCommand;
  end;

implementation

uses
  StrUtils;

procedure TTestUtilsShell.Setup;
begin
  FExePath := ExtractFilePath(ParamStr(0));
end;

procedure TTestUtilsShell.TestShellCommand;
var
  LActual: string;
begin
  LActual := ShellCommand('pwd', []);
  AssertEquals(GetCurrentDir, LActual);
end;

initialization
  RegisterTest(TTestUtilsShell);

end.