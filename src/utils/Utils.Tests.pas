unit Utils.Tests;

{$MODE DELPHI}{$H+}

interface

uses
  Classes,
  SysUtils,
  DOM, 
  XmlRead,
  Command.Interfaces;

type

  TTestCaseItem = class
  private
    FStatus: string;
    FTime: string;
    FTestSuite: string;
    FTestCase: string;
    FSource: string;
    FLine: Integer;
    FCode: Integer;
    FError: string;
    FDetail: string;
  public
    class function New(const ATestSuite, ATestCase: string): TTestCaseItem;

    property Status: string read FStatus write FStatus;
    property Time: string read FTime write FTime;
    property TestSuite: string read FTestSuite write FTestSuite;
    property TestCase: string read FTestCase write FTestCase;
    property Source: string read FSource write FSource;
    property Line: Integer read FLine write FLine;
    property Code: Integer read FCode write FCode;
    property Error: string read FError write FError;
    property Detail: string read FDetail write FDetail;
  end;

  TTestReport = class
  private
    FBuilder: ICommandBuilder;

    FExecutable: string;
    FProjectSource: string;
    FTestCaseCount: Integer;
    FTestSuiteCount: Integer;
    FTotalTime: string;
    FTestsPassed: Integer;
    FTestsFailed: Integer;
    FTestCaseData: TArray<TTestCaseItem>;

    function FindInCodeFile(ACodeFile: TStringList; const AText: string): string;
    function GetCodeFileForTestSuite(const ACurrentDir, ATestSuite: string; out AFoundOutput: string): TStringList;
    function GetString(ANode: TDOMNode; const AAttribute: string): string;
    function TrimTime(const ATime: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    class function New(ABuider: ICommandBuilder; const AProjectSource: string): TTestReport;

    /// parse xml tests file 
    function ParseXmlTestsFile(const ATestApp, AFileName: string): TTestReport;

    /// adds a new TestCaseItem
    function AddItem(AItem: TTestCaseItem): TTestCaseItem;

    /// outputs the report to console
    procedure Output;

    property Executable: string read FExecutable write FExecutable;
    property ProjectSource: string read FProjectSource write FProjectSource;
    property TestCaseCount: Integer read FTestCaseCount write FTestCaseCount;
    property TestSuiteCount: Integer read FTestSuiteCount write FTestSuiteCount;
    property TotalTime: string read FTotalTime write FTotalTime;
    property TestsPassed: Integer read FTestsPassed write FTestsPassed;
    property TestsFailed: Integer read FTestsFailed write FTestsFailed;
    property TestCaseData: TArray<TTestCaseItem> read FTestCaseData write FTestCaseData;
  end;

implementation

uses
  StrUtils,
  Math,
  Command.Colors;

constructor TTestReport.Create;
begin
  SetLength(FTestCaseData, 0);
end;

destructor TTestReport.Destroy;
var
  LItem: TTestCaseItem;
begin
  for LItem in FTestCaseData do
    LItem.Free;
  SetLength(FTestCaseData, 0);
  inherited Destroy;
end;

class function TTestReport.New(ABuider: ICommandBuilder; const AProjectSource: string): TTestReport;
begin
  Result := Self.Create;
  Result.ProjectSource := AProjectSource;
  Result.FBuilder := ABuider;
end;

function TTestReport.GetCodeFileForTestSuite(const ACurrentDir, ATestSuite: string; out AFoundOutput: string): TStringList;
var
  LResult: TStringList;
  LSearch: TSearchRec;
  LCurrentDir, LExt, LCodeFile: string;
begin
  Result := nil;
  if not DirectoryExists(ACurrentDir) then
    Exit;

  LResult := TStringList.Create;
  if FindFirst(ConcatPaths([ACurrentDir, AllFilesMask]), faAnyFile or faDirectory, LSearch) = 0 then
    try
      repeat
        if ((LSearch.Attr and faDirectory) <> 0) and (not AnsiMatchText(LSearch.Name, ['.', '..'])) then
        begin
          LCurrentDir := ConcatPaths([ACurrentDir, LSearch.Name]);
          GetCodeFileForTestSuite(LCurrentDir, ATestSuite, AFoundOutput);
          if AFoundOutput <> '' then
            Exit(LResult);
        end 
        else
        begin
          LExt := ExtractFileExt(LSearch.Name);
          if ((LSearch.Attr and faAnyFile) <> 0) and AnsiMatchText(LExt, ['.pp', '.pas', '.lpr']) then
          begin
            LCodeFile := ConcatPaths([ACurrentDir, LSearch.Name]);
            LResult.LoadFromFile(LCodeFile);
            AFoundOutput := FindInCodeFile(LResult, ATestSuite);
            if AFoundOutput <> '' then
            begin
              AFoundOutput := LCodeFile + ':' + AFoundOutput;
              FindClose(LSearch);
              Exit(LResult);
            end;
            LResult.Clear;
          end;
        end; 
      until FindNext(LSearch) <> 0;
    finally
      FindClose(LSearch);
    end;  
end;

function TTestReport.FindInCodeFile(ACodeFile: TStringList; const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ACodeFile.Count - 1 do
    if ContainsText(ACodeFile.Strings[I], AText) then
      Exit(IntToStr(I + 1) + ':1');
end;

function TTestReport.ParseXmlTestsFile(const ATestApp, AFileName: string): TTestReport;
var
  LXml: TXMLDocument;
  LTestSuiteList: TDOMNodeList;
  LTestSuite, LTestCase: TDOMNode;
  I, J, K: Integer;
  LItem: TTestCaseItem;
  LSourceInfo: string;
  LSourceUnit: TStringList = nil;
begin
  Result := Self;
  try
    if not FileExists(AFileName) then
    begin
      FBuilder.OutputColor(
        'Arquivo: ' + AFileName + ' não encontrado.'#13#10, 
        FBuilder.ColorTheme.Text);
      Exit;
    end;

    ReadXMLFile(LXml, AFileName);

    LTestSuiteList := LXml.DocumentElement.GetElementsByTagName('TestSuite');
    Executable := ATestApp;
    TestSuiteCount := LTestSuiteList.Count;
    TestCaseCount := 0;
    TestsPassed := 0;
    TestsFailed := 0;
    for I := 0 to LTestSuiteList.Count -1 do
    begin
      LTestSuite := LTestSuiteList[I];

      if I = 0 then
        TotalTime := GetString(LTestSuite, 'ElapsedTime');

      if GetString(LTestSuite, 'Name') = '' then
        continue;

      if (LTestSuite.HasAttributes) and (SameText(String(LTestSuite.Attributes[0].NodeName), 'name')) then
      begin
        // determina qual é a unit que possui essa suite de testes
        for j := 0 to LTestSuite.ChildNodes.Count - 1 do
        begin
          LTestCase := LTestSuite.ChildNodes[J];

          Inc(FTestCaseCount);
          LItem := TTestCaseItem.New(GetString(LTestSuite, 'Name'), GetString(LTestCase, 'Name'));
          AddItem(LItem);

          LItem.Status := GetString(LTestCase, 'Result');
          LItem.Time := GetString(LTestCase, 'ElapsedTime');

          if SameText('OK', LItem.Status) then
          begin
            Inc(FTestsPassed)
          end
          else
          begin
            Inc(FTestsFailed);
            
            LItem.Error := '';
            
            LSourceUnit := GetCodeFileForTestSuite(ProjectSource, LItem.TestSuite + '.' + LItem.TestCase, LSourceInfo);
            LItem.Error := LSourceInfo;
            FreeAndNil(LSourceUnit);                

            for K := 0 to LTestCase.ChildNodes.Count - 1 do
              LItem.Error := LItem.Error + 
                IfThen(LItem.Error = '', '', #13#10) + 
                String(LTestCase.ChildNodes[K].TextContent);
          end;
        end;
      end;
    end;
  finally
    LXml.Free;
    FreeAndNil(LSourceUnit);
  end;
end;

function TTestReport.AddItem(AItem: TTestCaseItem): TTestCaseItem;
begin
  SetLength(FTestCaseData, Length(FTestCaseData) + 1);
  FTestCaseData[Length(FTestCaseData) - 1] := AItem;
  Result := AItem;
end;

function TTestReport.GetString(ANode: TDOMNode; const AAttribute: string): string;
var
  LAttr: TDOMNode;
begin
  Result := '';
  LAttr := ANode.Attributes.GetNamedItem(UnicodeString(AAttribute));
  if Assigned(LAttr) then
    Result := String(LAttr.NodeValue);
end;

function TTestReport.TrimTime(const ATime: string): string;
begin
  Result := ATime;
  if StartsText('00:', Result) then
    Result := TrimTime(Copy(Result, 4));
end;

procedure TTestReport.Output;
var
  LItem: TTestCaseItem;
  LColor: byte;
begin
  FBuilder.OutputColor(PadLeft('Executable ', 13), FBuilder.ColorTheme.Value);
  FBuilder.OutputColor(Executable + #13#10, FBuilder.ColorTheme.Text);

  FBuilder.OutputColor(PadLeft('Starting ', 13), FBuilder.ColorTheme.Value);
  FBuilder.OutputColor(
    IntToStr(TestCaseCount) + ' test cases across ' +
    IntToStr(TestSuiteCount) + ' test suites'#13#10, 
    FBuilder.ColorTheme.Other);
  
  for LItem in TestCaseData do
  begin
    LColor := IfThen(LItem.Status <> 'OK', LightRed, LightGreen);

    FBuilder.OutputColor(PadLeft(LItem.Status + ' ', 13), LColor);
    FBuilder.OutputColor('[' + PadLeft(TrimTime(LItem.Time), 12) + ']', FBuilder.ColorTheme.Other);
    FBuilder.OutputColor(
      ' ' + LItem.TestSuite + '.' + LItem.TestCase + #13#10, 
      FBuilder.ColorTheme.Other);

    if LItem.Status <> 'OK' then
    begin
      WriteLn;
      FBuilder.OutputColor(LItem.Error, FBuilder.ColorTheme.Other);
      WriteLn;
      WriteLn;
    end;
  end;

  FBuilder.OutputColor(PadLeft('Summary ', 13), FBuilder.ColorTheme.Value);
  FBuilder.OutputColor('[' + PadLeft(TrimTime(TotalTime), 12) + ']', FBuilder.ColorTheme.Other);
  FBuilder.OutputColor(' ' +
    IntToStr(TestCaseCount) + ' tests cases run: ' +
    IntToStr(TestsPassed) + ' passed, ' +
    IntToStr(TestsFailed) + ' failed.',
    FBuilder.ColorTheme.Other);
  FBuilder.Output('');

  // checar for leaks
end;

class function TTestCaseItem.New(const ATestSuite: string; const ATestCase: string): TTestCaseItem;
begin
  Result := Self.Create;
  Result.TestSuite := ATestSuite;
  Result.TestCase := ATestCase;
end;

end.
