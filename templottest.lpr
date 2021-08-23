program templottest;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, dummy_vehicle_test, path_interface, matrix_2d,
  matrix_2d_test, point_ex_test;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title:='templottest';
  Application.Run;
  Application.Free;
end.
