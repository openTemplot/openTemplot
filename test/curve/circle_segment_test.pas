
(*  v1
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  OpenTemplot project contributors

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


