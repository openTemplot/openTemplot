program templottest;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, dummy_vehicle_test, path_interface, matrix_2d,
  matrix_2d_test, point_ex_test, utils_test, curve, curve_test, fresnel_unit,
  fresnel_test, curve_calculator, curve_segment, curve_segment_test;

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
