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
  Math,
  Command.Colors;

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
  for I := 1 to AContent.Count - 1 do
  begin
    LText := AContent.Strings[I];
    if ContainsText(LText, 'Call trace for block') then
      break;

    if Length(LText) < 22 then
      continue;

    LText := Copy(LText, 20);
    LDetail := LDetail +
      GetNextStringOf(LText, ' ') +
      GetNextStringOf(LText, ' of ') + ':' +
      GetNextStringOf(LText, 'line') + ':1'#1310;

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
  LColor: byte;
  LSourceItem, LOutputItem: string;
  LIndex: Integer;
begin
  FBuilder.OutputColor(PadLeft('Inspecting ', 13), FBuilder.ColorTheme.Value);
  FBuilder.OutputColor(
    Executable + ' output or ' + ChangeFileExt(Executable, '.trc') + ' file for possible leaks'#13#10, 
    FBuilder.ColorTheme.Other);

  if FHeapDataIsMissing then
  begin
    FBuilder.OutputColor(PadLeft('missing ', 13), FBuilder.ColorTheme.Title);
    FBuilder.OutputColor(
      'data with leak information'#13#10, 
      FBuilder.ColorTheme.Other);
    FBuilder.OutputColor(PadLeft('summary ', 13), FBuilder.ColorTheme.Title);
    FBuilder.OutputColor(
      'try to config the test project file to use debug option "use heap trace"'#13#10, 
      FBuilder.ColorTheme.Other);
    FBuilder.OutputColor('', StartupColor);
    Exit;
  end;

  if Length(FLeakData) = 0 then
  begin
    FBuilder.OutputColor(PadLeft('OK ', 13), FBuilder.ColorTheme.Value);
    FBuilder.OutputColor('[       0] 0 unfreed memory blocks.'#13#10, FBuilder.ColorTheme.Text);
    FBuilder.OutputColor(PadLeft('summary ', 13), FBuilder.ColorTheme.Value);
    FBuilder.OutputColor('[       0] 0 unfreed memory blocks.'#13#10, FBuilder.ColorTheme.Text);
    FBuilder.Output('');
    Exit;
  end;


  {
   Inspecting test app output or project.trc file for possible leaks
      missing data for leaks --no summary
      summary [       -] try to config the test project to use HeapTrace Debug option
         leak [     144] main$, file.pas:100:1
         leak [     132] main$, file.pas:20:1
      summary [     276] 2 unfreed memory blocks : 152 leaks.trc:10:1         
           ok [       0] no memory leaks found.
      summary [       0] 0 unfreed memory blocks
  }
  
  for LItem in LeakData do
  begin
    FBuilder.OutputColor(PadLeft('Leak ', 13), LightRed);
    FBuilder.OutputColor('[' + PadLeft(LItem.Size, 12) + ']', FBuilder.ColorTheme.Other);

    LIndex := 0;
    for LSourceItem in SplitString(LItem.Source, #10) do
    begin
      LOutputItem := StringReplace(LSourceItem, #13, '', [rfReplaceAll]);
      if LIndex > 0 then
        LOutputItem := StringOfChar(' ', 25) + LOutputItem;
      FBuilder.OutputColor(LOutputItem + #13#10, FBuilder.ColorTheme.Other);
    end;
  end;

  FBuilder.OutputColor(PadLeft('Summary ', 13), FBuilder.ColorTheme.Value);
  FBuilder.OutputColor('[' + PadLeft(IntToStr(FTotalLeakSize), 12) + ']', FBuilder.ColorTheme.Other);
  FBuilder.OutputColor(' ' + FTotalLeakInfo,
    FBuilder.ColorTheme.Other);
  FBuilder.Output('');


end;

class function TLeakItem.New(const AStatus, ASize: string): TLeakItem;
begin
  Result := Self.Create;
  Result.Status := AStatus;
  Result.Size := ASize;
end;

end.
