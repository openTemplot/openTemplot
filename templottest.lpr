program templottest;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  dummy_vehicle_test,
  path_interface,
  matrix_2d,
  matrix_2d_test,
  point_ex_test,
  utils_test,
  curve,
  curve_test,
  curve_calculator,
  curve_segment,
  transition_segment_test,
  straight_segment_test,
  straight_segment,
  circle_segment,
  circle_segment_test,
  transition_segment,
  curve_segment_calculator;

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
  Application.Title := 'templottest';
  Application.Run;
  Application.Free;
end.
