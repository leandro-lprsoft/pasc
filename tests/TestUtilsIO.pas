unit TestUtilsIO;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  fpcunit,
  testregistry,
  Command.Interfaces,
  Command.Builder,
  Utils.IO;

type

  TTestUtilsIO = class(TTestCase)
  published
    procedure TestFindFile;
  end;

implementation

uses
  StrUtils;

procedure TTestUtilsIO.TestFindFile;
begin
  AssertTrue(
    'Should find the file in the specified path',
    ContainsText(
      FindFile(ConcatPaths([GetCurrentDir, 'tests']), 'compile.ps1'),
      ConcatPaths(['tests', 'resources', 'compile.ps1'])
    )
  );
end;

initialization
  RegisterTest(TTestUtilsIO);

end.