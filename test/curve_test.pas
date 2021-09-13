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
  Ttest_curve = class(TTestCase)
    protected
      curve: Tcurve;

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

procedure Ttest_curve.Setup;
begin
  curve := Tcurve.Create;
end;

procedure Ttest_curve.TearDown;
begin
  curve.Free;
end;

procedure Ttest_curve.test_straight_line;
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

 curve.nominal_radius := max_rad_limit;
 curve.isSpiral := false;

 distance := 0;
 offset := 0;
 for i := 0 to 20 do
 begin
   curve.do_calculation(distance, offset, pt, direction, radius);

   CheckEquals( i, pt.X, 1e-6, format('pt.X at %f', [distance]));
   CheckEquals( offset, pt.Y, 1e-6, format('pt.Y at %f', [distance]));
   CheckEquals( 1, direction.X, 1e-6, format('direction.X at %f', [distance]));
   CheckEquals( 0, direction.y, 1e-6, format('direction.Y at %f', [distance]));
   CheckEquals( max_rad, radius, 1, format('radius at %f', [distance]));

   distance := distance + 1.0;
   offset := offset + 0.25;
 end;

end;

procedure Ttest_curve.test_single_radius_positive;
const
  test_radius = 5000;
var
  i : integer;
  distance: double;
  offset: double;
  pt: Tpex;
  direction: Tpex;
  radius: double;
  circle_origin: Tpex;
  expected_direction: Tpex;
  angle: double;
  distance_from_origin: double;
begin
 // Given a curve that is defined as a single radius
 //    ( radius = 5m, not spiral )
 //
 // When I ask for a point along the "curve"
 //
 // Then that point is a distance of 5m from (0, 5)
 //

 curve.nominal_radius := test_radius;
 curve.isSpiral := false;

 circle_origin.set_xy(0, test_radius);

 distance := 0;
 offset := 0;
 for i := 0 to 20 do
 begin
   curve.do_calculation(distance, offset, pt, direction, radius);

   angle := distance / 5000;
   expected_direction.set_xy(cos(angle), sin(angle));

   distance_from_origin := (circle_origin - pt).magnitude;

   CheckEquals( test_radius, distance_from_origin, 1e-6, format('distance from origin at %f', [distance]));
   CheckEquals( expected_direction.X, direction.X, 1e-6, format('direction.X at %f', [distance]));
   CheckEquals( expected_direction.Y, direction.y, 1e-6, format('direction.Y at %f', [distance]));
   CheckEquals( test_radius, radius, 1, format('radius at %f', [distance]));

   distance := distance + 100;
 end;

end;

procedure Ttest_curve.test_single_radius_negative;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_positive_positive_increasing;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_positive_positive_decreasing;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_negative_negative_increasing;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_negative_negative_decreasing;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_positive_negative;
begin
 Check(false, 'Not implemented');
end;

procedure Ttest_curve.test_transition_curve_negative_positive;
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
  RegisterTest(Ttest_curve);

end.


