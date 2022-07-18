/// <summary>This unit contains a standard implmentation for IPathWatcher.
/// </summary>
unit Utils.Watcher;

{$MODE DELPHI}{$H+}

interface

uses
  Classes,
  Utils.Interfaces;

type
  TPathWatcher = class(TInterfacedObject, IPathWatcher)
  private
    FPath: String;

    FIgnoreStartText: TArray<string>;
    FIgnoreFolders: TArray<string>;
    FIgnoreFiles: TArray<string>;
    FIgnoreExt: TArray<string>;

    FWatcherRun: TWatcherRunCallback;

    /// <summary>Add all items from AData array to AArray without duplicates. </summary>
    procedure AppendDataToArray(AArray, AData: TArray<string>);

  public

    /// <summary>Default class constructor. Initialize interanl arrays. Use new method for class
    /// instantiation. </summary>
    constructor Create;

    /// <summary>Factory method for TPathWatcher, this is the recommended method to create a new
    /// instance of the class. </summary>
    class function New: IPathWatcher;

    /// <summary>Sets the path to watch for changes. Returns self to allow chained watcher calls.
    /// </summary>
    /// <param name="APath">Path that will be monitored</param>
    function Path(const APath: string): IPathWatcher;

    /// <summary>Defines a set of values to be ignored according to the type provided as a parameter.
    /// Ex: to ignore all items, folders or files that start with "." just call the function as follows:
    /// Watcher.Ignore(ikStartsText, ['.']);
    /// Ex: to ignore all folders with specific names, call the function as follows:
    /// Watcher.Ignore(ikFolder, ['bin', 'lib', 'modules']);
    ///    
    /// Returns self to allow chained watcher calls.
    /// </summary>
    /// <param name="AIgnoreKind">Kind of item to be ignored by the watcher</param>
    /// <param name="AItems">An array containing strings to be ignored according to AIgnoreKind
    /// </param>
    function Ignore(const AIgnoreKind: TIgnoreKind; AItems: TArray<string>): IPathWatcher;

    /// <summary>Defines the callback procedure that should be executed when a change is detected.
    /// </summary>
    /// <param name="AProc">A procedure that will be called after a change</param>
    /// </param>
    function Run(const AWatcherRun: TWatcherRunCallback): IPathWatcher;

    /// <summary>Start watching the path considering the ignore parameters that have been configured. 
    /// As soon as a change is detected the callback routine will be called.
    /// </summary>
    function Start: IPathWatcher;

  end;

implementation

uses
  SysUtils;

constructor TPathWatcher.Create;
begin
  FPath := '';
  SetLength(FIgnoreStartText, 0);
  SetLength(FIgnoreFolders, 0);
  SetLength(FIgnoreFiles, 0);
  SetLength(FIgnoreExt, 0);  
  FWatcherRun := nil;
end;

class function TPathWatcher.New: IPathWatcher;
begin
  Result := Self.Create;
end;

function TPathWatcher.Path(const APath: string): IPathWatcher;
begin
  if not DirectoryExists(APath) then
    raise Exception.Create('Path "' + APath + '" doest not exist');

  FPath := APath;  

  Result := Self;
end;

procedure TPathWatcher.AppendDataToArray(AArray: TArray<string>; AData: TArray<string>);
var
  LNewItem, LOldItem: string;
  LFound: Boolean;
begin
  for LNewItem in AData do
  begin
    LFound := False;
    for LOldItem in AArray do
      if SameText(LNewItem, LOldItem) then
      begin
        LFound := True;
        break;
      end;

    if not LFound then
    begin
      SetLength(AArray, Length(AArray) + 1);
      AArray[Length(AArray) - 1] := LNewItem;
    end;
  end;
end;

function TPathWatcher.Ignore(const AIgnoreKind: TIgnoreKind; AItems: TArray<string>): IPathWatcher;
begin
  case AIgnoreKind of 
  ikStartsText:
    AppendDataToArray(FIgnoreStartText, AItems);
  ikFolder:
    AppendDataToArray(FIgnoreFolders, AItems);
  ikFile:
    AppendDataToArray(FIgnoreFiles, AItems);
  ikExtension:
    AppendDataToArray(FIgnoreExt, AItems);
  end;
  Result := Self;
end;

function TPathWatcher.Run(const AWatcherRun: TWatcherRunCallback): IPathWatcher;
begin
  FWatcherRun := AWatcherRun;
  Result := Self;
end;

function TPathWatcher.Start: IPathWatcher;
begin
  if (not DirectoryExists(FPath)) or (FPath = '') then
    raise Exception.Create('Path "' + FPath + '" doest not exist');

  if not Assigned(FWatcherRun) then
    raise Exception.Create('Watcher callback not assigned, call Run method to assign one.');

  // start a loop

  // from current path build a file name list with full file names that will be the key of the list
  // store file age of each file also
  // BuildFileList(FOldFiles, FNewFiles, FChangeMessage);

  // during the list creation if exists an old list check
  // 1. new item is added: triggers the FWatcherRun
  // 2. item exists and file age is different: triggers the FWatcherRun
  // 3. item exists and file size is different: triggers the FWatcherRun
  // 3. old list count is different from new list count: triggers the FWatcherRun

  // if FChangeMessage <> '' then
  // begin
  //   if FWatcherRun(FChangeMessage) then
  //     break;
  //   FreeAndNil(FOldFiles);
  //   FreeAndNil(FNewFiles);
  // end;


  // if FWactherRun was called
  // 1. If FWatcherRun returns true, the loop should end.
  // 2. Clear FOldList e FNewLIst, the next iteration will populate a new list

  // if FOldList does not exists, FOldList := FNewList; FNewList := nil;
  Result := Self;
end;

end.