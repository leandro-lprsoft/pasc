unit MockCommandBuilder;

{$MODE DELPHI}{$H+}

interface

uses
  SysUtils, 
  Classes,
  rtti, 
  typinfo,
  Command.Interfaces;

  procedure MockSetup(ABuilder: ICommandBuilder);
  procedure MockCommand(ABuilder: ICommandBuilder);
  function MockInputLn: string;
  procedure MockOutput(const AMessage: string);
  procedure MockOutputColor(const AMessage: string; const AColor: byte);

var
  MockCommandCapture, MockInputLnResult, MockOutputCaptured: string;

implementation

uses
  StrUtils;

procedure MockSetup(ABuilder: ICommandBuilder);
begin
  MockCommandCapture := '';
  MockInputLnResult := '';
  MockOutputCaptured := '';
  ABuilder.InputLn := MockInputLn;
  ABuilder.Output := MockOutput;
  ABuilder.OutputColor := MockOutputColor;
end;

procedure MockCommand(ABuilder: ICommandBuilder);
begin  
  MockCommandCapture := 'executed';
end;

function MockInputLn: string;
begin
  Result := MockInputLnResult;
end;

procedure MockOutput(const AMessage: string);
begin
  MockOutputCaptured := MockOutputCaptured + #13#10 + AMessage;
end;

procedure MockOutputColor(const AMessage: string; const AColor: byte);
begin
  MockOutputCaptured := MockOutputCaptured + AMessage;
end;

end.