
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

