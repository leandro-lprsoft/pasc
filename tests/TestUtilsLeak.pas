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
    procedure TestAddItem;
    procedure TestAddRelativePath;
    procedure TestCreateLeakItem;
    procedure TestGetNextStringOf;
    procedure TestGetNextStringOfEmpty;
    procedure TestParseContentLeakSimple;
    procedure TestParseContentLeakNone;
    procedure TestLeak;
  end;

implementation

uses
  StrUtils,
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

procedure TTestUtilsLeak.TestAddItem;
var
  LItem: TLeakItem;
begin
  LItem := TLeakItem.New('Leak', '8');
  LItem.Source := 'source';
  FLeakReport.AddItem(LItem);

  AssertTrue('Should have an item on LeakData array', Length(FLeakReport.LeakData) = 1);
  AssertEquals('Leak', FLeakReport.LeakData[0].Status);
  AssertEquals('8', FLeakReport.LeakData[0].Size);
  AssertEquals('source', FLeakReport.LeakData[0].Source);
end;

procedure TTestUtilsLeak.TestAddRelativePath;
begin
  AssertEquals(
    ConcatPaths(['.', 'tests', 'resources', 'leak_none.txt']),
    FLeakReport.AddRelativePath('leak_none.txt')    
  );
end;

procedure TTestUtilsLeak.TestCreateLeakItem;
var
  LContent: TStringList;
begin
  LContent := TStringList.Create;
  try
    LContent.Text := GetTestResource('leak_simple');
    FLeakReport.CreateLeakItem(LContent, 5);
  finally
    LContent.Free;
  end;
  AssertEquals('Should have an item on LeakData array', 1, Length(FLeakReport.LeakData));
  AssertEquals('Leak', FLeakReport.LeakData[0].Status);
  AssertEquals('8', FLeakReport.LeakData[0].Size);
  AssertTrue(
    'Source should contain text CallCalcTest', 
    ContainsText(FLeakReport.LeakData[0].Source, 'CallCalcTest'));
  AssertTrue(
    'Source should contain text 16:1', 
    ContainsText(FLeakReport.LeakData[0].Source, '16:1'));
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

procedure TTestUtilsLeak.TestGetNextStringOfEmpty;
begin
  AssertEquals('', FLeakReport.GetNextStringOf('', ''));
  AssertEquals('', FLeakReport.GetNextStringOf('', 'field'));
  AssertEquals('', FLeakReport.GetNextStringOf('line string with content', 'field'));
  AssertEquals('', FLeakReport.GetNextStringOf('line string with content', ''));
  AssertEquals('', FLeakReport.GetNextStringOf('line string with content field', 'field'));
end;

procedure TTestUtilsLeak.TestParseContentLeakSimple;
begin
  FLeakReport.ParseHeapTrace(GetTestResource('leak_simple'));
  AssertEquals('Should report one leak', 1, Length(FLeakReport.LeakData));
end;

procedure TTestUtilsLeak.TestParseContentLeakNone;
begin
  FLeakReport.ParseHeapTrace(GetTestResource('leak_none'));
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

