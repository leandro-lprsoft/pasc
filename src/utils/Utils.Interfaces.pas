/// <summary>This unit contains the declaration of the interfaces used in the application.
/// </summary>
unit Utils.Interfaces;

{$MODE DELPHI}{$H+}

interface

type

  TWatcherEvent = (
    weFirstRun,     /// The first run of the application
    weFileChanged,  /// The file has changed
    weTimeout,      /// The file has been deleted
    weNoChange      /// No changes were detected
  );

  /// <summary>A procedure that will be called after a change. The file name that triggered 
  /// this action will be passed as a parameter. If function returns true, watcher stops
  /// running. </summary>
  /// <param name="AFile">The file that triggered the event. It will be empty for first 
  /// run and no change.</param>
  /// <param name="AEvent">The event that triggered this procedure.</param>
  TWatcherRunCallback = function (const AFile: string; const AEvent: TWatcherEvent): boolean;

  /// <summary>Enum type to identify the type of item to be ignored by the Watcher. </summary>
  TIgnoreKind = (
    ikStartsText, /// itens of any kind that starts with the text specified
    ikFolder,     /// ignore folders with the specified name
    ikFile,       /// ignore files with the specified name
    ikExtension   /// ignore files with the specified extension
    );

  /// <summary>Interface that represents an object to monitor a path, with methods to configure
  /// parameters and callback in addition to initializing the watch itself. </summary>
  IPathWatcher = interface
    ['{E995078A-19C1-4785-AE53-8A3113574A72}']

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

    /// <summary>Sets maximum timeout in milliseconds to call the watcher run callback. Every time run 
    /// is called this timer is reset. If the maximum timeout is reached TPathWatcher is terminated.
    /// Default value is 3600 seconds or 3,600,000 milliseconds.
    /// </summary>
    /// <param name="ATime">Number of milliseconds to set the maximum timeout.</param>
    function Timeout(const ATime: Longint): IPathWatcher;

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

end.