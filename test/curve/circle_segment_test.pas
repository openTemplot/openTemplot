unit circle_segment_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  circle_segment,
  point_ex;

type
  { TTestCirleSegment }

  TTestCircleSegment = class(TTestCase)
  protected
    segment: TCircleSegment;

    procedure TearDown; override;
  published
    procedure test_radius_positive;
    procedure test_radius_negative;
  end;

implementation

uses
  Math;

{ TTestCircleSegment }

procedure TTestCircleSegment.TearDown;
begin
  segment.Free;
  inherited TearDown;
end;

procedure TTestCircleSegment.test_radius_positive;
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
  testPoint: Tpex;
  testDirection: Tpex;
  normalDirection: Tpex;
begin
  // Given a CircleSegment that is defined as a positive radius
  //    ( radius = 5m )
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is a distance of 5m from calculated origin
  //

  testPoint.set_xy(7, 4);
  testDirection.set_xy(1, 1);
  testDirection.normalise;

  segment := TCircleSegment.Create(1000, testPoint, testDirection, testRadius);

  normalDirection.set_xy(-testDirection.y, testDirection.x);
  circleOrigin := testPoint + normalDirection * testRadius;

  distance := 0;
  for i := 0 to 20 do begin
    segment.CalculateCurveAt(distance, pt, direction, radius);

    angle := distance / testRadius - DegToRad(45);
    expectedDirection.set_xy(cos(angle + Pi / 2), sin(angle + Pi / 2));

    distanceFromOrigin := (circleOrigin - pt).magnitude;

    CheckEquals(testRadius, distanceFromOrigin, 1e-6,
      format('distance from origin at %f', [distance]));
    CheckEquals(expectedDirection.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
    CheckEquals(expectedDirection.Y, direction.y, 1e-6, format('direction.Y at %f', [distance]));
    CheckEquals(testRadius, radius, 1, format('radius at %f', [distance]));

    distance := distance + 10;
  end;
end;

procedure TTestCircleSegment.test_radius_negative;
const
  testRadius = -3000;
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
  testPoint: Tpex;
  testDirection: Tpex;
  normalDirection: Tpex;
begin
  // Given a CircleSegment that is defined as a negative radius
  //    ( radius = -3m )
  //
  // When I ask for a point along the "curve"
  //
  // Then that point is a distance of 3m from calculated origin
  //

  testPoint.set_xy(100, -40);
  testDirection.set_xy(-1, 1);
  testDirection.normalise;

  segment := TCircleSegment.Create(1000, testPoint, testDirection, testRadius);

  normalDirection.set_xy(-testDirection.y, testDirection.x);
  circleOrigin := testPoint + normalDirection * testRadius;

  distance := 0;
  for i := 0 to 20 do begin
    segment.CalculateCurveAt(distance, pt, direction, radius);

    angle := distance / testRadius + DegToRad(225);
    expectedDirection.set_xy(cos(angle - Pi / 2), sin(angle - Pi / 2));

    distanceFromOrigin := (circleOrigin - pt).magnitude;

    CheckEquals(Abs(testRadius), distanceFromOrigin, 1e-6,
      format('distance from origin at %f', [distance]));
    CheckEquals(expectedDirection.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
    CheckEquals(expectedDirection.Y, direction.y, 1e-6, format('direction.Y at %f', [distance]));
    CheckEquals(testRadius, radius, 1, format('radius at %f', [distance]));

    distance := distance + 10;
  end;
end;


initialization
  RegisterTest(TTestCircleSegment);

end.


