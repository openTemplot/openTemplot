unit straight_segment_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  straight_segment,
  point_ex;

type
  TTestStraightSegment = class(TTestCase)
  protected
    segment: TStraightSegment;

    procedure TearDown; override;
  published
    procedure test_straight_line;
  end;

implementation

uses
  curve;

procedure TTestStraightSegment.TearDown;
begin
  segment.Free;
  inherited TearDown;
end;

procedure TTestStraightSegment.test_straight_line;
var
  pt: Tpex;
  direction: Tpex;
  radius: double;
  i: integer;
  distance: double;
  testOrigin: Tpex;
  testDirection: Tpex;
  expectedPoint: Tpex;
begin
  // Given a straight segment that is defined by an origin and a direction
  //    ( radius = max_radius, not spiral )
  //
  // When I ask for a point along the segment
  //
  // Then that point is on a the defined line
  //
  testOrigin.set_xy(-1.5, 7);
  testDirection.set_xy(3, -2);
  testDirection.normalise;
  segment := TStraightSegment.Create(100, testOrigin, testDirection);


  distance := 0;
  for i := 0 to 20 do begin
    segment.CalculateCurveAt(distance, pt, direction, radius);

    expectedPoint := testOrigin + testDirection * distance;

    CheckEquals(expectedPoint.X, pt.X, 1e-6, format('pt.X at %f', [distance]));
    CheckEquals(expectedPoint.Y, pt.Y, 1e-6, format('pt.Y at %f', [distance]));
    CheckEquals(testDirection.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
    CheckEquals(testDirection.Y, direction.Y, 1e-6, format('direction.Y at %f', [distance]));
    CheckEquals(max_rad, radius, 1, format('radius at %f', [distance]));

    distance := distance + 1.0;
  end;

end;


initialization
  RegisterTest(TTestStraightSegment);

end.

