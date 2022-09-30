
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

unit slew_calculator_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  slew_calculator,
  point_ex,
  curve_parameters_interface;

type

  { TTestSlewCalculator }

  TTestSlewCalculator = class(TTestCase, ICurveParameters)
  private
    fixedRadius: double;
    transitionStartRadius: double;
    transitionEndRadius: double;
    distanceToTransition: double;
    transitionLength: double;
    isSpiral: boolean;

    isSlewing: boolean;
    distanceToStartOfSlew: double;
    slewLength: double;
    slewAmount: double;
    slewMode: ESlewMode;
    slewFactor: double;

  protected
    slew: TSlewCalculator;

    procedure Setup; override;
    procedure TearDown; override;

    function GetIsSpiral: boolean;
    function GetFixedRadius: double;
    function GetTransitionStartRadius: double;
    function GetTransitionEndRadius: double;
    function GetDistanceToTransition: double;
    function GetTransitionLength: double;

    function GetIsSlewing: boolean;
    function GetDistanceToStartOfSlew: double;
    function GetSlewLength: double;
    function GetSlewAmount: double;
    function GetSlewMode: ESlewMode;
    function GetSlewFactor: double;

    procedure do_test_slew_cosine;
    procedure do_test_slew_tanh;

  published
    procedure test_straight_line_slewed_left_cosine;
    procedure test_straight_line_slewed_right_cosine;

    procedure test_straight_line_slewed_left_tanh;

      (*
      procedure test_single_radius_positive_slewed_left;
      procedure test_single_radius_positive_slewed_right;
      procedure test_single_radius_negative_slewed_left;
      procedure test_single_radius_negative_slewed_right;
      procedure test_transition_curve_positive_positive_increasing_slewed_left;
      procedure test_transition_curve_positive_positive_decreasing_slewed_right;
      procedure test_transition_curve_negative_negative_increasing_slewed_left;
      procedure test_transition_curve_negative_negative_decreasing_slewed_right;
      procedure test_transition_curve_positive_negative_slewed_left;
      procedure test_transition_curve_negative_positive_slewed_right;
      *)
  end;

implementation

uses
  Math,
  curve,
  curve_segment_calculator;

procedure TTestSlewCalculator.Setup;
begin
end;

procedure TTestSlewCalculator.TearDown;
begin
  slew.Free;
end;

function TTestSlewCalculator.GetIsSpiral: boolean;
begin
  Result := isSpiral;
end;

function TTestSlewCalculator.GetFixedRadius: double;
begin
  Result := fixedRadius;
end;

function TTestSlewCalculator.GetTransitionStartRadius: double;
begin
  Result := transitionStartRadius;
end;

function TTestSlewCalculator.GetTransitionEndRadius: double;
begin
  Result := transitionEndRadius;
end;

function TTestSlewCalculator.GetDistanceToTransition: double;
begin
  Result := distanceToTransition;
end;

function TTestSlewCalculator.GetTransitionLength: double;
begin
  Result := transitionLength;
end;

function TTestSlewCalculator.GetIsSlewing: boolean;
begin
  Result := isSlewing;
end;

function TTestSlewCalculator.GetDistanceToStartOfSlew: double;
begin
  Result := distanceToStartOfSlew;
end;

function TTestSlewCalculator.GetSlewLength: double;
begin
  Result := slewLength;
end;

function TTestSlewCalculator.GetSlewAmount: double;
begin
  Result := slewAmount;
end;

function TTestSlewCalculator.GetSlewMode: ESlewMode;
begin
  Result := slewMode;
end;

function TTestSlewCalculator.GetSlewFactor: double;
begin
  Result := slewFactor;
end;

procedure TTestSlewCalculator.do_test_slew_cosine;
const
  smallTolerance = 1e-6;
var
  curveSegmentCalculator: TCurveSegmentCalculator;
  distance: double;
  pt: Tpex;
  direction: Tpex;
  radius: double;
  angle: double;
  expectedOffset: double;
  expectedSlope: double;
  expectedDirection: Tpex;
begin
  curveSegmentCalculator := TCurveSegmentCalculator.Create(self);
  slew := TSlewCalculator.Create(self, curveSegmentCalculator);

  distance := 0;
  while distance < distanceToStartOfSlew + slewLength + 100 do begin
    slew.CalculateCurveAt(distance, pt, direction, radius);

    if distance < distanceToStartOfSlew then begin
      CheckEquals(0, pt.y, smallTolerance, format('before slew: pt.y at %f', [distance]));
      CheckEquals(distance, pt.x, smallTolerance, format('before slew: pt.x at %f', [distance]));
      CheckEquals(max_rad, radius, smallTolerance, format('before slew: radius at %f', [distance]));
      CheckEquals(1, direction.x, smallTolerance, format('before slew: expectedDirection.x at %f', [distance]));
      CheckEquals(0, direction.y, smallTolerance, format('before slew: expectedDirection.y at %f', [distance]));
    end
    else
    if distance < distanceToStartOfSlew + slewLength then begin
      angle := (distance - distanceToStartOfSlew) * Pi() / slewLength;
      expectedOffset := slewAmount * (1 - cos(angle)) / 2;
      expectedSlope := (Pi() * slewAmount * sin(angle)) / (2 * slewLength);
      expectedDirection := Tpex.xy(1, expectedSlope).normalise();

      CheckEquals(expectedOffset, pt.y, smallTolerance, format('in slew: pt.y at %f', [distance]));
      CheckEquals(distance, pt.x, smallTolerance, format('in slew: pt.x at %f', [distance]));
      CheckEquals(max_rad, radius, smallTolerance, format('in slew: radius at %f', [distance]));
      CheckEquals(expectedDirection.x, direction.x, smallTolerance,
        format('in slew: expectedDirection.x at %f', [distance]));
      CheckEquals(expectedDirection.y, direction.y, smallTolerance,
        format('in slew: expectedDirection.y at %f', [distance]));

    end
    else begin
      CheckEquals(slewAmount, pt.y, smallTolerance, format('after slew: pt.y at %f', [distance]));
      CheckEquals(distance, pt.x, smallTolerance, format('after slew: pt.x at %f', [distance]));
      CheckEquals(max_rad, radius, smallTolerance, format('after slew: radius at %f', [distance]));
      CheckEquals(1, direction.x, smallTolerance, format('after slew: expectedDirection.x at %f', [distance]));
      CheckEquals(0, direction.y, smallTolerance, format('after slew: expectedDirection.y at %f', [distance]));
    end;

    distance := distance + 5;
  end;
end;

procedure TTestSlewCalculator.do_test_slew_tanh;
const
  smallTolerance = 1e-6;
var
  curveSegmentCalculator: TCurveSegmentCalculator;
  distance: double;
  pt: Tpex;
  direction: Tpex;
  radius: double;
  angle: double;
  expectedOffset: double;
  expectedSlope: double;
  expectedDirection: Tpex;
  yAtSlewFactor: double;
  yDashAtSlewFactor: double;
  rotation: Tpex;
  yMax: double;
begin
  curveSegmentCalculator := TCurveSegmentCalculator.Create(self);
  slew := TSlewCalculator.Create(self, curveSegmentCalculator);

  // initial calculations for Tanh...
  yAtSlewFactor := tanh(slewFactor);
  yDashAtSlewFactor := 1 - sqr(tanh(slewFactor));
  rotation := Tpex.xy(1, yDashAtSlewFactor).normalise;
  yMax := -rotation.y * slewFactor + rotation.x * yAtSlewFactor;

  distance := 0;
  while distance < distanceToStartOfSlew + slewLength + 100 do begin
    slew.CalculateCurveAt(distance, pt, direction, radius);

    if distance < distanceToStartOfSlew then begin
      CheckEquals(0, pt.y, smallTolerance, format('before slew: pt.y at %f', [distance]));
      CheckEquals(distance, pt.x, smallTolerance, format('before slew: pt.x at %f', [distance]));
      CheckEquals(max_rad, radius, smallTolerance, format('before slew: radius at %f', [distance]));
      CheckEquals(1, direction.x, smallTolerance, format('before slew: expectedDirection.x at %f', [distance]));
      CheckEquals(0, direction.y, smallTolerance, format('before slew: expectedDirection.y at %f', [distance]));
    end
    else
    if distance <= distanceToStartOfSlew + slewLength then begin
      angle := 2 * slewFactor * ((distance - distanceToStartOfSlew) / slewLength - 0.5);

      expectedOffset := (-rotation.y * angle + rotation.x * tanh(angle) + yMax) *
        slewAmount / (2 * yMax);
      expectedSlope := (slewAmount * slewFactor / (yMax * slewLength)) *
        (-rotation.y + rotation.x * (1 - sqr(tanh(angle))));

      expectedDirection := Tpex.xy(1, expectedSlope).normalise();

      CheckEquals(expectedOffset, pt.y, smallTolerance, format('in slew: pt.y at %f', [distance]));
      CheckEquals(distance, pt.x, smallTolerance, format('in slew: pt.x at %f', [distance]));
      CheckEquals(max_rad, radius, smallTolerance, format('in slew: radius at %f', [distance]));
      CheckEquals(expectedDirection.x, direction.x, smallTolerance,
        format('in slew: expectedDirection.x at %f', [distance]));
      CheckEquals(expectedDirection.y, direction.y, smallTolerance,
        format('in slew: expectedDirection.y at %f', [distance]));

    end
    else begin
      CheckEquals(slewAmount, pt.y, smallTolerance, format('after slew: pt.y at %f', [distance]));
      CheckEquals(distance, pt.x, smallTolerance, format('after slew: pt.x at %f', [distance]));
      CheckEquals(max_rad, radius, smallTolerance, format('after slew: radius at %f', [distance]));
      CheckEquals(1, direction.x, smallTolerance, format('after slew: expectedDirection.x at %f', [distance]));
      CheckEquals(0, direction.y, smallTolerance, format('after slew: expectedDirection.y at %f', [distance]));
    end;

    distance := distance + 1;
  end;
end;

procedure TTestSlewCalculator.test_straight_line_slewed_left_cosine;
begin
  fixedRadius := max_rad;
  isSpiral := False;

  isSlewing := True;
  distanceToStartOfSlew := 100;
  slewLength := 100;
  slewAmount := 10;
  slewMode := smCosine;

  do_test_slew_cosine;
end;

procedure TTestSlewCalculator.test_straight_line_slewed_right_cosine;
begin
  fixedRadius := max_rad;
  isSpiral := False;

  isSlewing := True;
  distanceToStartOfSlew := 50;
  slewLength := 100;
  slewAmount := -20;
  slewMode := smCosine;

  do_test_slew_cosine;
end;

procedure TTestSlewCalculator.test_straight_line_slewed_left_tanh;
begin
  fixedRadius := max_rad;
  isSpiral := False;

  isSlewing := True;
  distanceToStartOfSlew := 100;
  slewLength := 100;
  slewAmount := 10;
  slewMode := smTanH;
  slewFactor := 1;

  do_test_slew_tanh;
end;


initialization
  RegisterTest(TTestSlewCalculator);

end.
