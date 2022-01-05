unit transition_segment_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  transition_segment,
  point_ex;

type
  { TTestTransitionSegment }

  TTestTransitionSegment = class(TTestCase)
  protected
    segment: TTransitionSegment;

    procedure TearDown; override;
    procedure do_test_transition(r1, r2, transitionLength: double);

  published
    procedure test_straight_positive;
    procedure test_straight_negative;
    procedure test_positive_straight;
    procedure test_negative_straight;
    procedure test_positive_positive_increasing;
    procedure test_positive_positive_decreasing;
    procedure test_negative_negative_increasing;
    procedure test_negative_negative_decreasing;
    procedure test_positive_to_larger_negative;
    procedure test_positive_to_smaller_negative;
    procedure test_negative_to_larger_positive;
    procedure test_negative_to_smaller_positive;
  end;


implementation

uses
  curve;

{ TTestTransitionSegment }

procedure TTestTransitionSegment.TearDown;
begin
  segment.Free;
  inherited TearDown;
end;

procedure TTestTransitionSegment.do_test_transition(r1, r2, transitionLength: double);
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
  initialPoint: Tpex;
  initialDirection: Tpex;
begin
  initialPoint.set_xy(3, 0);
  initialDirection.set_xy(1, 0);
  segment := TTransitionSegment.Create(transitionLength, initialPoint, initialDirection, r1, r2);

  if Abs(r1) < max_rad_test then
    curvature1 := 1 / r1
  else
    curvature1 := 0;
  if Abs(r2) < max_rad_test then
    curvature2 := 1 / r2
  else
    curvature2 := 0;

  segment.CalculateCurveAt(0, previousPoint, direction, radius);
  distance := 5;
  while distance < transitionLength do begin
    segment.CalculateCurveAt(distance, pt, direction, radius);

    // linear interpolation to determine expected curvature
    expectedCurvature := ((curvature2 - curvature1) * distance / transitionLength) +
      curvature1;
    if abs(expectedCurvature) >= 1 / max_rad_test then
      expectedRadius := 1 / expectedCurvature
    else
      expectedRadius := max_rad;

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

procedure TTestTransitionSegment.test_straight_positive;
begin
  // Given a curve that is defined as a transition from straight to a positive radius
  //
  // When I ask for a point along the segment
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(max_rad, 1000, 100);
end;

procedure TTestTransitionSegment.test_straight_negative;
begin
  // Given a curve that is defined as a transition from straight to a negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(max_rad, -1000, 100);
end;

procedure TTestTransitionSegment.test_positive_straight;
begin
  // Given a curve that is defined as a transition from a positive radius to straight
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(1000, max_rad, 100);
end;

procedure TTestTransitionSegment.test_negative_straight;
begin
  // Given a curve that is defined as a transition from a negative radius to straight
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (decreasing) radius
  //
  do_test_transition(-1000, max_rad, 100);
end;

procedure TTestTransitionSegment.test_positive_positive_increasing;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a larger positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(1000, 2000, 100);
end;

procedure TTestTransitionSegment.test_positive_positive_decreasing;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a smaller positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(2000, 1000, 100);
end;

procedure TTestTransitionSegment.test_negative_negative_increasing;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a larger negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(-1000, -2000, 100);
end;

procedure TTestTransitionSegment.test_negative_negative_decreasing;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a smaller negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected (increasing) radius
  //
  do_test_transition(-2000, -1000, 100);
end;

procedure TTestTransitionSegment.test_positive_to_larger_negative;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a larger negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(1000, -2000, 150);
end;

procedure TTestTransitionSegment.test_positive_to_smaller_negative;
begin
  // Given a curve that is defined as a transition from a positive radius to
  //   a larger negative radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(2000, -1000, 150);
end;

procedure TTestTransitionSegment.test_negative_to_larger_positive;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a smaller positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(-1000, 2000, 150);
end;

procedure TTestTransitionSegment.test_negative_to_smaller_positive;
begin
  // Given a curve that is defined as a transition from a negative radius to
  //   a smaller positive radius
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is the expected distance from the previous point
  //  and the radius is the expected  radius
  //
  do_test_transition(-2000, 1000, 150);
end;


initialization
  RegisterTest(TTestTransitionSegment);

end.


