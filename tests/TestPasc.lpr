program TestPasc;

{$mode objfpc}{$H+}

uses
  Classes, 
  SysUtils,
  consoletestrunner, 
  Resources,
  TestUtilsIO,
  TestUtilsLeak,
  TestUtilsResources,
  TestUtilsShell,
  TestUtilsTests,
  TestUtilsWatcher,  
  TestCommandBuild,
  TestCommandClean,
  TestCommandInstall,
  TestCommandNew,
  TestCommandTest, 
  TestCommandWatch,
  MockCommandBuilder;

type

  { TTestPascRunner }

  TTestPascRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

{$R *.res}

var
  Application: TTestPascRunner;
  HeapFileName: string;

begin
  {$IF DECLARED(UseHeapTrace)}
  HeapFileName := ConcatPaths([ExtractFilePath(ParamStr(0)), 'heap.trc']);
  if FileExists(HeapFileName) then
    DeleteFile(HeapFileName);
  SetHeapTraceOutput(HeapFileName);
  {$ENDIF}
  Application := TTestPascRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'pasc console test runner';
  Application.Run;
  Application.Free;
end.
