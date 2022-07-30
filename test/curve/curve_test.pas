unit curve_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  curve,
  point_ex;

type

  { TTestableCurve }

  TTestableCurve = class(TCurve)
  public
    // expose property for testing
    property curveCalculator;
  end;

  { TTestCurve }

  TTestCurve = class(TTestCase)
  protected
    curve: TTestableCurve;

    procedure Setup; override;
    procedure TearDown; override;

    procedure do_test_transition(r1, r2, initialLength, transitionLength: double);

  published
    procedure test_straight_line;
    procedure test_single_radius_positive;
    procedure test_single_radius_negative;
    procedure test_transition_curve_straight_positive;
    procedure test_transition_curve_straight_negative;
    procedure test_transition_curve_positive_straight;
    procedure test_transition_curve_negative_straight;
    procedure test_transition_curve_positive_positive_increasing;
    procedure test_transition_curve_positive_positive_decreasing;
    procedure test_transition_curve_negative_negative_increasing;
    procedure test_transition_curve_negative_negative_decreasing;
    procedure test_transition_curve_positive_to_larger_negative;
    procedure test_transition_curve_positive_to_smaller_negative;
    procedure test_transition_curve_negative_to_larger_positive;
    procedure test_transition_curve_negative_to_smaller_positive;

    procedure test_slew_creation;

    procedure test_CopyFrom;
  end;

implementation

uses
  curve_parameters_interface,
  slew_calculator;

procedure TTestCurve.Setup;
begin
  curve := TTestableCurve.Create;
end;

procedure TTestCurve.TearDown;
begin
  curve.Free;
end;

procedure TTestCurve.test_straight_line;
var
  pt: Tpex;
  direction: Tpex;
  radius: double;
  i: integer;
  distance: double;
begin
  // Given a curve that is defined as straight
  //    ( radius = max_radius, not spiral )
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is on a straight line
  //

  curve.fixedRadius := max_rad_limit;
  curve.isSpiral := False;

  distance := 0;
  for i := 0 to 20 do begin
    curve.CalculateCurveAt(distance, pt, direction, radius);

    CheckEquals(i, pt.X, 1e-6, format('pt.X at %f', [distance]));
    CheckEquals(0, pt.Y, 1e-6, format('pt.Y at %f', [distance]));
    CheckEquals(1, direction.X, 1e-6, format('direction.X at %f', [distance]));
    CheckEquals(0, direction.y, 1e-6, format('direction.Y at %f', [distance]));
    CheckEquals(max_rad, radius, 1, format('radius at %f', [distance]));

    distance := distance + 1.0;
  end;

end;

procedure TTestCurve.test_single_radius_positive;
const
  testRadius = 5000;
var
  i: integer;
  distance: double;
  pt: Tpex;
  direction: Tpex;
  radius: double;
  circleOrigin: Tpex;
  expectedDirection: Tpex;
  angle: double;
  distanceFromOrigin: double;
begin
  // Given a curve that is defined as a single radius
  //    ( radius = 5m, not spiral )
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is a distance of 5m from (0, 5)
  //
  curve.fixedRadius := testRadius;
  curve.isSpiral := False;

  circleOrigin.set_xy(0, testRadius);

  distance := 0;
  for i := 0 to 20 do begin
    curve.CalculateCurveAt(distance, pt, direction, radius);

    angle := distance / testRadius;
    expectedDirection.set_xy(cos(angle), sin(angle));

    distanceFromOrigin := (circleOrigin - pt).magnitude;

    CheckEquals(testRadius, distanceFromOrigin, 1e-6,
      format('distance from origin at %f', [distance]));
    CheckEquals(expectedDirection.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
    CheckEquals(expectedDirection.Y, direction.y, 1e-6, format('direction.Y at %f', [distance]));
    CheckEquals(testRadius, radius, 1, format('radius at %f', [distance]));

    distance := distance + 100;
  end;
end;

procedure TTestCurve.test_single_radius_negative;
const
  testRadius = -8000;
var
  i: integer;
  distance: double;
  pt: Tpex;
  direction: Tpex;
  radius: double;
  circleOrigin: Tpex;
  expectedDirection: Tpex;
  angle: double;
  distanceFromOrigin: double;
begin
  // Given a curve that is defined as a single negative radius, turning to the right
  //    ( radius = -8m, not spiral )
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is a distance of 8m from (0, -8)
  //
  curve.fixedRadius := testRadius;
  curve.isSpiral := False;

  circleOrigin.set_xy(0, testRadius);

  distance := 0;
  for i := 0 to 20 do begin
    curve.CalculateCurveAt(distance, pt, direction, radius);

    angle := distance / testRadius;
    expectedDirection.set_xy(cos(angle), sin(angle));

    distanceFromOrigin := (circleOrigin - pt).magnitude;

    CheckEquals(abs(testRadius), distanceFromOrigin, 1e-6,
      format('distance from origin at %f', [distance]));
    CheckEquals(expectedDirection.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
    CheckEquals(expectedDirection.Y, direction.y, 1e-6, format('direction.Y at %f', [distance]));
    CheckEquals(testRadius, radius, 1, format('radius at %f', [distance]));

    distance := distance + 100;
  end;
end;


procedure TTestCurve.do_test_transition(r1, r2, initialLength, transitionLength: double);
const
  testStepSize = 5;
  testStepTolerance = 0.05;
var
  distance: double;
  pt: Tpex;
  direction: Tpex;
  radius: double;
  previousPoint: Tpex;
  delta: Tpex;
  expectedCurvature: double;
  expectedRadius: double;
  distanceFromPrevious: double;
  curvature1: double;
  curvature2: double;
begin
  curve.transitionRadius1 := r1;
  curve.transitionRadius2 := r2;
  curve.transitionLength := transitionLength;
  curve.distanceToTransition := initialLength;
  curve.isSpiral := True;

  if Abs(r1) < max_rad_test then
    curvature1 := 1 / r1
  else
    curvature1 := 0;
  if Abs(r2) < max_rad_test then
    curvature2 := 1 / r2
  else
    curvature2 := 0;

  curve.CalculateCurveAt(0, previousPoint, direction, radius);
  distance := 5;
  while distance < initialLength * 2 + transitionLength do begin
    curve.CalculateCurveAt(distance, pt, direction, radius);

    // linear interpolation to determine expected curvature
    if distance <= initialLength then begin
      expectedRadius := r1;
    end
    else
    if distance <= initialLength + transitionLength then begin
      // linear interpolation to determine expected curvature
      expectedCurvature := ((curvature2 - curvature1) *
        (distance - initialLength) / transitionLength) + curvature1;
      if abs(expectedCurvature) >= 1 / max_rad_test then
        expectedRadius := 1 / expectedCurvature
      else
        expectedRadius := max_rad;
    end
    else begin
      expectedRadius := r2;
    end;

    delta := pt - previousPoint;
    distanceFromPrevious := delta.magnitude;

    //  distance between testStepSize - 1 and testStepSize...
    CheckEquals(testStepSize - testStepTolerance, distanceFromPrevious, 1,
      format('distance from previous point at %f', [distance]));
    CheckEquals(expectedRadius, radius, testStepTolerance, format('radius at %f', [distance]));

    // check the delta values make sense
    Check(delta.x > 0, format('delta.x at %f = %f', [distance, delta.x]));

    distance := distance + testStepSize;
    previousPoint := pt;
  end;

end;

procedure TTestCurve.test_transition_curve_straight_positive;
begin
  // Given a curve that is defined as a transition from straight to a positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(max_rad, 1000, 100, 100);
end;

procedure TTestCurve.test_transition_curve_straight_negative;
begin
  // Given a curve that is defined as a transition from straight to a negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(max_rad, -1000, 100, 100);
end;


procedure TTestCurve.test_transition_curve_positive_straight;
begin
  // Given a curve that is defined as a transition from a positive radius to straight
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(1000, max_rad, 100, 100);
end;

procedure TTestCurve.test_transition_curve_negative_straight;
begin
  // Given a curve that is defined as a transition from a negative radius to straight
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(-1000, max_rad, 100, 100);
end;


procedure TTestCurve.test_transition_curve_positive_positive_increasing;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a larger positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(1000, 2000, 100, 100);
end;

procedure TTestCurve.test_transition_curve_positive_positive_decreasing;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a smaller positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(2000, 1000, 100, 100);
end;

procedure TTestCurve.test_transition_curve_negative_negative_increasing;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a larger negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(-1000, -2000, 100, 100);
end;

procedure TTestCurve.test_transition_curve_negative_negative_decreasing;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a smaller negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(-2000, -1000, 100, 100);
end;

procedure TTestCurve.test_transition_curve_positive_to_larger_negative;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a larger negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(1000, -2000, 100, 150);
end;

procedure TTestCurve.test_transition_curve_positive_to_smaller_negative;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a larger negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(2000, -1000, 100, 150);
end;

procedure TTestCurve.test_transition_curve_negative_to_larger_positive;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a smaller positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(-1000, 2000, 100, 150);
end;

procedure TTestCurve.test_transition_curve_negative_to_smaller_positive;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a smaller positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(-2000, 1000, 100, 150);
end;

procedure TTestCurve.test_slew_creation;
var
  pt: Tpex;
  direction: Tpex;
  radius: double;
begin
  // Given a curve defined with a slew
  //
  // When I ask for a point along the curve
  //
  // Then the curveCalculator is a TSlewCalculator

  curve.isSlewing := True;
  curve.isSpiral := False;
  curve.fixedRadius := max_rad;

  curve.CalculateCurveAt(0, pt, direction, radius);

  Check(curve.curveCalculator is TSlewCalculator, 'curveCalculator not expected class');
end;

procedure TTestCurve.test_CopyFrom;
var
  curve2: TCurve;
begin
  // Given 2 curves with different parameters
  //
  // When I call CopyFrom
  //
  // Then the curve parameters are copied

  curve.isSpiral := True;
  curve.isSlewing := True;
  curve.fixedRadius := max_rad;
  curve.transitionRadius1 := 3456;
  curve.transitionRadius2 := 7890;
  curve.distanceToTransition := 123;
  curve.transitionLength := 234;
  curve.distanceToStartOfSlew := 333;
  curve.slewAmount := 23;
  curve.slewLength := 145;
  curve.slewFactor := 1.5;
  curve.slewMode := eSM_TanH;

  curve2 := TCurve.Create;
  try
    curve2.CopyFrom(curve);

    CheckEquals(curve.isSpiral, curve2.isSpiral, 'isSpiral');
    CheckEquals(curve.isSlewing, curve2.isSlewing, 'isSlewing');
    CheckEquals(curve.fixedRadius, curve2.fixedRadius, 'fixedRadius');
    CheckEquals(curve.transitionRadius1, curve2.transitionRadius1, 'transitionRadius1');
    CheckEquals(curve.transitionRadius2, curve2.transitionRadius2, 'transitionRadius2');
    CheckEquals(curve.distanceToTransition, curve2.distanceToTransition, 'distanceToTransition');
    CheckEquals(curve.transitionLength, curve2.transitionLength, 'transitionLength');
    CheckEquals(curve.distanceToStartOfSlew, curve2.distanceToStartOfSlew, 'distanceToStartOfSlew');
    CheckEquals(curve.slewAmount, curve2.slewAmount, 'slewAmount');
    CheckEquals(curve.slewLength, curve2.slewLength, 'slewLength');
    CheckEquals(curve.slewFactor, curve2.slewFactor, 'slewFactor');
    CheckEquals(Ord(curve.slewMode), Ord(curve2.slewMode), 'slewMode');

  finally
    curve2.Free;
  end;
end;

initialization
  RegisterTest(TTestCurve);

end.
