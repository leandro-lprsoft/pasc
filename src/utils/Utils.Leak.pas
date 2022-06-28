unit Utils.Leak;

{$MODE DELPHI}{$H+}

interface

uses
  Classes,
  SysUtils,
  Command.Interfaces;

type
  TLeakItem = class
  private
    FStatus: string;
    FSize: string;
    FSource: string;
    FLine: Integer;
    FColumn: Integer;
  public
    class function New(const AStatus, ASize: string): TLeakItem;

    property Status: string read FStatus write FStatus;
    property Size: string read FSize write FSize;
    property Source: string read FSource write FSource;
    property Line: Integer read FLine write FLine;
    property Column: Integer read FColumn write FColumn;
  end;

  TLeakReport = class
  private
    FBuilder: ICommandBuilder;

    FExecutable: string;
    FProjectSource: string;
    FHeapDataIsMissing: Boolean;
    FTotalLeakSize: Integer;
    FTotalLeakInfo: string;
    FLeakData: TArray<TLeakItem>;
   
  public
    constructor Create;
    destructor Destroy; override;

    class function New(ABuider: ICommandBuilder; const AProjectSource: string): TLeakReport;

    /// adds a new leak item to the report data
    function AddItem(AItem: TLeakItem): TLeakItem;

    /// create leak item using content from TStringList and an starting index
    procedure CreateLeakItem(AContent: TStringList; AStart: Integer);

    /// gets string value next to a field like string text passed to AField param.
    function GetNextStringOf(const AText, AField: string): string;

    /// adjust file name with relative path
    function AddRelativePath(const AFile: string): string;

    /// outputs the report to console
    procedure Output;

    /// parse xml tests file 
    function ParseHeapTrace(const AContent: string): TLeakReport;    

    property Executable: string read FExecutable write FExecutable;
    property ProjectSource: string read FProjectSource write FProjectSource;
    property LeakData: TArray<TLeakItem> read FLeakData write FLeakData;
  end;

implementation

uses
  StrUtils,
  Command.Colors,
  Utils.IO;

constructor TLeakReport.Create;
begin
  SetLength(FLeakData, 0);
  FHeapDataIsMissing := True;
  FTotalLeakSize := 0;
  FTotalLeakInfo := '';
end;

destructor TLeakReport.Destroy;
var
  LItem: TLeakItem;
begin
  for LItem in FLeakData do
    LItem.Free;
  SetLength(FLeakData, 0);
  inherited Destroy;
end;

class function TLeakReport.New(ABuider: ICommandBuilder; const AProjectSource: string): TLeakReport;
begin
  Result := Self.Create;
  Result.ProjectSource := AProjectSource;
  Result.FBuilder := ABuider;
end;

function TLeakReport.AddItem(AItem: TLeakItem): TLeakItem;
begin
  SetLength(FLeakData, Length(FLeakData) + 1);
  FLeakData[Length(FLeakData) - 1] := AItem;
  Result := AItem;
end;

function TLeakReport.GetNextStringOf(const AText: string; const AField: string): string;
var
  LIndex: Integer;
  LValue: string;
begin
  LIndex := Pos(AField, AText);
  if LIndex <= 0 then
    exit('');

  LIndex := LIndex + Length(AField);
  LValue := Trim(Copy(AText, LIndex));
  Result := SplitString(LValue, ' ')[0];
end;

procedure TLeakReport.CreateLeakItem(AContent: TStringList; AStart: Integer);
var
  LText, LDetail: string;
  LItem: TLeakItem;
  LIndex, I: Integer;
begin
  LText := AContent.Strings[AStart];
  if not ContainsText(LText, 'Call trace for block') then
    exit;

  LDetail := '';
  LItem := TLeakItem.New('Leak', GetNextStringOf(LText, 'size'));
  for I := AStart + 1 to AContent.Count - 1 do
  begin
    LText := AContent.Strings[I];
    if ContainsText(LText, 'Call trace for block') then
      break;

    if Length(LText) < 22 then
      continue;

    LText := Copy(LText, 20);
    LDetail := LDetail +
      StringReplace(GetNextStringOf(LText, ' '), ',', ' ', [rfReplaceAll]) +
      AddRelativePath(GetNextStringOf(LText, ' of ')) + ':' +
      GetNextStringOf(LText, 'line') + ':1' + #13#10;

  end;
  LItem.FSource := LDetail;
  AddItem(LItem);
end;

function TLeakReport.ParseHeapTrace(const AContent: string): TLeakReport;
var
  I: Integer;
  LContent: TStringList;
  LText: string;
begin
  Result := Self;
  LContent := TStringList.Create;
  try
    if AContent = '' then
      LContent.LoadFromFile(ConcatPaths([ProjectSource, 'heap.trc']))
    else
      LContent.Text := AContent;

    for I := 0 to LContent.Count - 1 do
    begin
      LText := LContent.Strings[I];

      if ContainsText(LText, 'unfreed memory blocks') then
      begin
        FHeapDataIsMissing := False;
        FTotalLeakInfo := LText;
      end;

      if ContainsText(LText, 'Call trace for block') then
        CreateLeakItem(LContent, I);
    end;
  finally
    LContent.Free;
  end;
end;

procedure TLeakReport.Output;
var
  LItem: TLeakItem;
  LSourceItem, LOutputItem: string;
  LIndex: Integer;
begin
  FBuilder.OutputColor(PadLeft('Inspecting ', 13), FBuilder.ColorTheme.Value);
  FBuilder.OutputColor(
    'heap.trc file for possible leaks'#13#10, 
    FBuilder.ColorTheme.Other);

  if FHeapDataIsMissing then
  begin
    FBuilder.OutputColor(PadLeft('missing ', 13), FBuilder.ColorTheme.Title);
    FBuilder.OutputColor(
      'data with leak information'#13#10, 
      FBuilder.ColorTheme.Other);
    FBuilder.OutputColor('', StartupColor);
    Exit;
  end;

  if Length(FLeakData) = 0 then
  begin
    FBuilder.OutputColor(PadLeft('OK ', 13), FBuilder.ColorTheme.Value);
    FBuilder.OutputColor('[       0] 0 unfreed memory blocks.'#13#10, FBuilder.ColorTheme.Other);
    FBuilder.OutputColor(PadLeft('Summary ', 13), FBuilder.ColorTheme.Value);
    FBuilder.OutputColor('[       0] no memory leaks detected.'#13#10, FBuilder.ColorTheme.Other);
    FBuilder.Output('');
    Exit;
  end;
  
  for LItem in LeakData do
  begin
    FBuilder.OutputColor(PadLeft('Leak ', 13), LightRed);
    FBuilder.OutputColor('[' + PadLeft(LItem.Size, 12) + '] ', FBuilder.ColorTheme.Other);

    LIndex := 0;

    for LSourceItem in SplitString(LItem.Source, #10) do
    begin
      LOutputItem := StringReplace(LSourceItem, #13, '', [rfReplaceAll]);

      if Trim(LOutputItem) = '' then
        continue;  
      
      if LIndex > 0 then
        LOutputItem := StringOfChar(' ', 28) + LOutputItem;
      FBuilder.OutputColor(LOutputItem + #13#10, FBuilder.ColorTheme.Other);
      Inc(LIndex);
    end;
  end;

  FBuilder.OutputColor(PadLeft('Summary ', 13), FBuilder.ColorTheme.Value);
  FBuilder.OutputColor(
    '[' + PadLeft(IntToStr(FTotalLeakSize), 12) + ']', 
    FBuilder.ColorTheme.Other);
  FBuilder.OutputColor(' ' + FTotalLeakInfo, FBuilder.ColorTheme.Other);
  FBuilder.Output('');
end;

class function TLeakItem.New(const AStatus, ASize: string): TLeakItem;
begin
  Result := Self.Create;
  Result.Status := AStatus;
  Result.Size := ASize;
end;

function TLeakReport.AddRelativePath(const AFile: string): string;
var
  LRelativePath: string;
  LQualifiedPath: string;
begin
  // try to locate the file project source path
  LQualifiedPath := FindFile(ProjectSource, AFile);
  if LQualifiedPath = '' then
    LQualifiedPath := FindFile(GetCurrentDir, AFile);

  if LQualifiedPath = '' then
    LRelativePath := './'
  else
    LRelativePath := '.' + StringReplace(LQualifiedPath, GetCurrentDir, '', [rfReplaceAll, rfIgnoreCase]);

  Result := LRelativePath;
end;

end.
