/// <summary> Unit which has functions to work with system operational process execution
/// </summary>
unit Utils.Shell;

{$MODE DELPHI}{$H+}

interface

uses
  Classes,
  SysUtils,
  Process;

type

  /// <summary> function type to execute programs or commands. See ShellCommand function
  /// for more info. </summary>
  TShellCommandFunc = function (const AProgram: string; AParams: TArray<string>): string;

  /// <summary> executes a program and returns the output as funciton result and also provide a execution status
  /// through AStatus out parameter </summary>
  /// <param name="AProgram"> Program name or command to be executed. </param>
  /// <param name="AParams"> Array of argumentos to be passed to the 
  /// program or command being called </param>
  function ShellCommand(const AProgram: string; AParams: TArray<string>): string;

var
  ShellExecute: TShellCommandFunc = @ShellCommand;

implementation

const
  MAX_BUFFER = 2048; // Buffer size for reading the output in chunks

function ShellCommand(const AProgram: string; AParams: TArray<string>): string;
var
  LProcess: TProcess = nil;
  LStream: TStream = nil;
  LString: TStringList = nil;
  LBytesRead: LongInt;
  LBuffer: array [1..MAX_BUFFER] of Byte;
  LParam: string;
begin
  Result := '';
  try
    LProcess := TProcess.Create(nil);
    LProcess.Executable := AProgram;

    for LParam in AParams do
      LProcess.Parameters.Add(LParam);

    LProcess.Options := [poUsePipes, poStdErrToOutPut];
    LProcess.Execute;

    LStream := TMemoryStream.Create;

    repeat
      LBytesRead := LProcess.Output.Read(LBuffer, MAX_BUFFER);
      LStream.Write(LBuffer, LBytesRead);
    until LBytesRead = 0;
    
    LStream.Position := 0; 
    LString := TStringList.Create;
    LString.LoadFromStream(LStream);
    LString.TrailingLineBreak := False;

    Result := LString.Text;

    FreeAndNil(LProcess);
    FreeAndNil(LStream);      
    FreeAndNil(LString);

  except
    on E: Exception do
    begin
      FreeAndNil(LProcess);
      FreeAndNil(LStream);      
      FreeAndNil(LString);
      raise;
    end;
  end;
end;

end.