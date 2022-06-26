unit Utils.IO;

{$MODE DELPHI}{$H+}

interface

uses
  Classes,
  SysUtils;

  function GetFileContent(const AFileName: string): string;
  procedure SaveFileContent(const AFileName, AContent: string);

implementation

function GetFileContent(const AFileName: string): string;
var
  LFile: TStringList = nil;
begin
  Result := '';
  LFile := TStringList.Create;
  try
    LFile.LoadFromFile(AFileName);
    Result := LFile.Text;
    LFile.Free;
  except
    on E: Exception do
    begin
      LFile.Free;
      raise;
    end;
  end;
end;

procedure SaveFileContent(const AFileName, AContent: string);
var
  LFile: TStringList = nil;
begin
  LFile := TStringList.Create;
  try
    LFile.Text := AContent;
    LFile.SaveToFile(AFileName);
    LFile.Free;
  except
    on E: Exception do
    begin
      LFile.Free;
      raise;
    end;
  end;
end;

end.