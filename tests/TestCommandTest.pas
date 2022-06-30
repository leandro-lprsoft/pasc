unit TestCommandTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type

  TTestCommandTest= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestAnother;
  end;

implementation

procedure TTestCommandTest.TestHookUp;
begin
  AssertEquals('1 equals 1', 1, 1);
end;

procedure TTestCommandTest.SetUp;
begin

end;

procedure TTestCommandTest.TearDown;
begin

end;

procedure TTestCommandTest.TestAnother;
begin
  AssertEquals('1 equals 1', 1, 1);
end;

initialization

  RegisterTest(TTestCommandTest);
end.

