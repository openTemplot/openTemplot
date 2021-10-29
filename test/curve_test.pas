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
  TTestCurve = class(TTestCase)
    protected
      curve: TCurve;

      procedure Setup; override;
      procedure TearDown; override;

    published
      procedure test_straight_line;
      procedure test_single_radius_positive;
      procedure test_single_radius_negative;
      procedure test_transition_curve_positive_positive_increasing;
      procedure test_transition_curve_positive_positive_decreasing;
      procedure test_transition_curve_negative_negative_increasing;
      procedure test_transition_curve_negative_negative_decreasing;
      procedure test_transition_curve_positive_negative;
      procedure test_transition_curve_negative_positive;
      (*
      procedure test_straight_line_slewed_left;
      procedure test_straight_line_slewed_right;
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

procedure TTestCurve.Setup;
begin
  curve := TCurve.Create;
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
  distance : double;
  offset: double;
begin
 // Given a curve that is defined as straight
 //    ( radius = max_radius, not spiral )
 //
 // When I ask for a point along the "curve"
 //
 // Then that point is on a straight line
 //

 curve.nominalRadius := max_rad_limit;
 curve.isSpiral := false;

 distance := 0;
 offset := 0;
 for i := 0 to 20 do
 begin
   curve.DoCalculation(distance, offset, pt, direction, radius);

   CheckEquals( i, pt.X, 1e-6, format('pt.X at %f', [distance]));
   CheckEquals( offset, pt.Y, 1e-6, format('pt.Y at %f', [distance]));
   CheckEquals( 1, direction.X, 1e-6, format('direction.X at %f', [distance]));
   CheckEquals( 0, direction.y, 1e-6, format('direction.Y at %f', [distance]));
   CheckEquals( max_rad, radius, 1, format('radius at %f', [distance]));

   distance := distance + 1.0;
   offset := offset + 0.25;
 end;

end;

procedure TTestCurve.test_single_radius_positive;
const
  testRadius = 5000;
var
  i : integer;
  distance: double;
  offset: double;
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
 curve.nominalRadius := testRadius;
 curve.isSpiral := false;

 circleOrigin.set_xy(0, testRadius);

 distance := 0;
 offset := 0;
 for i := 0 to 20 do
 begin
   curve.DoCalculation(distance, offset, pt, direction, radius);

   angle := distance / testRadius;
   expectedDirection.set_xy(cos(angle), sin(angle));

   distanceFromOrigin := (circleOrigin - pt).magnitude;

   CheckEquals( testRadius, distanceFromOrigin, 1e-6, format('distance from origin at %f', [distance]));
   CheckEquals( expectedDirection.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
   CheckEquals( expectedDirection.Y, direction.y, 1e-6, format('direction.Y at %f', [distance]));
   CheckEquals( testRadius, radius, 1, format('radius at %f', [distance]));

   distance := distance + 100;
 end;
end;

procedure TTestCurve.test_single_radius_negative;
const
  testRadius = -8000;
var
  i : integer;
  distance: double;
  offset: double;
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
 curve.nominalRadius := testRadius;
 curve.isSpiral := false;

 circleOrigin.set_xy(0, testRadius);

 distance := 0;
 offset := 0;
 for i := 0 to 20 do
 begin
   curve.DoCalculation(distance, offset, pt, direction, radius);

   angle := distance / testRadius;
   expectedDirection.set_xy(cos(angle), sin(angle));

   distanceFromOrigin := (circleOrigin - pt).magnitude;

   CheckEquals( abs(testRadius), distanceFromOrigin, 1e-6, format('distance from origin at %f', [distance]));
   CheckEquals( expectedDirection.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
   CheckEquals( expectedDirection.Y, direction.y, 1e-6, format('direction.Y at %f', [distance]));
   CheckEquals( testRadius, radius, 1, format('radius at %f', [distance]));

   distance := distance + 100;
 end;
end;

procedure TTestCurve.test_transition_curve_positive_positive_increasing;
begin
 Check(false, 'Not implemented');
end;

procedure TTestCurve.test_transition_curve_positive_positive_decreasing;
begin
 Check(false, 'Not implemented');
end;

procedure TTestCurve.test_transition_curve_negative_negative_increasing;
begin
 Check(false, 'Not implemented');
end;

procedure TTestCurve.test_transition_curve_negative_negative_decreasing;
begin
 Check(false, 'Not implemented');
end;

procedure TTestCurve.test_transition_curve_positive_negative;
begin
 Check(false, 'Not implemented');
end;

procedure TTestCurve.test_transition_curve_negative_positive;
begin
 Check(false, 'Not implemented');
end;


(*
procedure Ttest_curve.test_straight_line_slewed_left;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_straight_line_slewed_right;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_single_radius_positive_slewed_left;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_single_radius_positive_slewed_right;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_single_radius_negative_slewed_left;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_single_radius_negative_slewed_right;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_positive_positive_increasing_slewed_left;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_positive_positive_decreasing_slewed_right;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_negative_negative_increasing_slewed_left;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_negative_negative_decreasing_slewed_right;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_positive_negative_slewed_left;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_negative_positive_slewed_right;
begin
 Check(false, 'Not implemented');
end;
*)


initialization
  RegisterTest(TTestCurve);

end.


