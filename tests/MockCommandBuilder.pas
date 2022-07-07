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
  MockStart, MockTimeout: QWord;

implementation

procedure MockSetup(ABuilder: ICommandBuilder);
begin
  MockCommandCapture := '';
  MockInputLnResult := '';
  MockOutputCaptured := '';
  ABuilder.InputLn := MockInputLn;
  ABuilder.Output := MockOutput;
  ABuilder.OutputColor := MockOutputColor;
  MockStart := GetTickCount64;
  MockTimeout := 1000;
end;

procedure MockCommand(ABuilder: ICommandBuilder);
begin  
  if not Assigned(ABuilder) then
    MockCommandCapture := 'ABuilder not assigned'
  else
    MockCommandCapture := 'executed';
end;

function MockInputLn: string;
begin
  Result := MockInputLnResult;
  if (GetTickCount64 - MockStart) > MockTimeout then
    raise Exception.Create('Timeout of ' + IntToStr(MockTimeout) + ' exceeded');
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