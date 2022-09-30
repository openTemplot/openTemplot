
(*
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2019  OpenTemplot project contributors

    This program is free software: you may redistribute it and/or modify
    it under the terms of the GNU General Public Licence as published by
    the Free Software Foundation, either version 3 of the Licence, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public Licence for more details.

    You should have received a copy of the GNU General Public Licence
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

                >>>     NOTE TO DEVELOPERS     <<<
                     DO NOT EDIT THIS COMMENT
              It is inserted in this file by running
                  'python3 scripts/addComment.py'
         The original text lives in scripts/addComment.py.

====================================================================================
*)

program templottest;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  dummy_vehicle_test,
  extended_utils,
  extended_utils_test,
  path_interface,
  matrix_2d, shoved_timber,
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
  curve_segment_calculator,
  slew_calculator,
  slew_calculator_test,
  curve_parameters_interface,
  curve_segment_calculator_test, shoved_timber_test;

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
