unit TestUtilsShell;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
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

initialization
  RegisterTest(TTestUtilsShell);

end.
