unit TestUtilsLeak;

{$MODE DELPHI}{$H+}

interface

uses
  Classes, 
  SysUtils, 
  fpcunit,
  testregistry,
  Command.Interfaces,
  Command.Builder,
  Utils.Leak;

type

  TTestUtilsLeak = class(TTestCase)
  private
    FBuilder: ICommandBuilder;
    FLeakReport: TLeakReport;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddRelativePath;
    procedure TestGetNextStringOf;
    procedure TestParseContentLeakSimple;
    procedure TestParseContentLeakNone;
    procedure TestLeak;
  end;

implementation

uses
  Resources;

procedure TTestUtilsLeak.SetUp;
begin
  FBuilder := TCommandBuilder.Create(ParamStr(0));
  FLeakReport := TLeakReport.New(FBuilder, ExtractFilePath(ParamStr(0)));
end;

procedure TTestUtilsLeak.TearDown;
begin
  FLeakReport.Free;
end;

procedure TTestUtilsLeak.TestAddRelativePath;
begin
  AssertEquals(
    ConcatPaths(['.', 'tests', 'resources', 'leak_none.txt']),
    FLeakReport.AddRelativePath('leak_none.txt')    
  );
end;

procedure TTestUtilsLeak.TestGetNextStringOf;
begin
  AssertEquals(
    FLeakReport.GetNextStringOf('Call trace for block $0000000001518E30 size 8', 'size'),
    '8');

  AssertEquals(
    FLeakReport.GetNextStringOf('Call trace for block $0000000001518E30 size 8', 'block'),
    '$0000000001518E30');    
end;

procedure TTestUtilsLeak.TestParseContentLeakSimple;
begin
  FLeakReport.ParseHeapTrace(GetResource('leak_simple'));
  AssertEquals('Should report one leak', 1, Length(FLeakReport.LeakData));
end;

procedure TTestUtilsLeak.TestParseContentLeakNone;
begin
  FLeakReport.ParseHeapTrace(GetResource('leak_none'));
  AssertEquals('Should report one leak', 0, Length(FLeakReport.LeakData));
end;

procedure SecondLevelLeak;
var
  LObject: TObject;
begin
  LObject := TObject.Create;
end;

procedure TTestUtilsLeak.TestLeak;
begin
  SecondLevelLeak;
end;

initialization
  RegisterTest(TTestUtilsLeak);

end.

