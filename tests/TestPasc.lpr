program TestPasc;

{$mode objfpc}{$H+}

uses
  Classes, 
  consoletestrunner, 
  Resources,
  TestCommandTest,
  TestUtilsLeak;

type

  { TTestPascRunner }

  TTestPascRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

{$R *.res}

var
  Application: TTestPascRunner;

begin
  Application := TTestPascRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'pasc console test runner';
  Application.Run;
  Application.Free;
end.
