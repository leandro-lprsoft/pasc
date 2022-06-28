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
    procedure TestGetNextStringOf;
    procedure TestParseContentSimpleLeak;
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

procedure TTestUtilsLeak.TestGetNextStringOf;
begin
  AssertEquals(
    FLeakReport.GetNextStringOf('Call trace for block $0000000001518E30 size 8', 'size'),
    '8');

  AssertEquals(
    FLeakReport.GetNextStringOf('Call trace for block $0000000001518E30 size 8', 'block'),
    '$0000000001518E30');    
end;

procedure TTestUtilsLeak.TestParseContentSimpleLeak;
begin
  FLeakReport.ParseHeapTrace(GetResource('leak_simple'));

  AssertEquals('Should report one leak', 1, Length(FLeakReport.LeakData));
end;

initialization

  RegisterTest(TTestUtilsLeak);
end.

