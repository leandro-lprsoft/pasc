unit TestCommandClean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  TTestCommandClean= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRegistry;
    procedure TestClean;
  end;

implementation

procedure TTestCommandClean.SetUp;
begin

end;

procedure TTestCommandClean.TearDown;
begin

end;

procedure TTestCommandClean.TestRegistry;
begin
  AssertEquals('1 equals 1', 1, 1);
end;

procedure TTestCommandClean.TestClean;
begin
  
end;

initialization

  RegisterTest(TTestCommandClean);
end.

