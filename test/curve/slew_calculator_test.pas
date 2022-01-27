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
    nominalRadius: double;
    nominalRadius2: double;
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
    function GetNominalRadius: double;
    function GetNominalRadius2: double;
    function GetDistanceToTransition: double;
    function GetTransitionLength: double;

    function GetIsSlewing: boolean;
    function GetDistanceToStartOfSlew: double;
    function GetSlewLength: double;
    function GetSlewAmount: double;
    function GetSlewMode: ESlewMode;
    function GetSlewFactor: double;

    procedure do_test_slew;

  published
    procedure test_straight_line_slewed_left_cosine;
    procedure test_straight_line_slewed_right_cosine;
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

function TTestSlewCalculator.GetNominalRadius: double;
begin
  Result := nominalRadius;
end;

function TTestSlewCalculator.GetNominalRadius2: double;
begin
  Result := nominalRadius2;
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

procedure TTestSlewCalculator.do_test_slew;
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
      CheckEquals( 0, pt.y );
      CheckEquals( distance, pt.x );
      CheckEquals( max_rad, radius);
      CheckEquals( 1, direction.x, format('expectedDirection.x at %f', [distance]));
      CheckEquals( 0, direction.y, format('expectedDirection.y at %f', [distance]));
    end
    else
    if distance < distanceToStartOfSlew + slewLength then begin
      angle := (distance - distanceToStartOfSlew) * Pi()/slewLength;
      expectedOffset := slewAmount * (1 - cos(angle)) / 2;
      expectedSlope := (Pi()*slewAmount * sin(angle)) / (2 * slewLength);
      expectedDirection := Tpex.xy( 1, expectedSlope ).normalise();

      CheckEquals(expectedOffset, pt.y);
      CheckEquals(distance, pt.x);
      CheckEquals(max_rad, radius);
      CheckEquals(expectedDirection.x, direction.x, format('expectedDirection.x at %f', [distance]));
      CheckEquals(expectedDirection.y, direction.y, format('expectedDirection.y at %f', [distance]));

    end
    else begin
      CheckEquals( slewAmount, pt.y );
      CheckEquals( distance, pt.x );
      CheckEquals( max_rad, radius);
      CheckEquals( 1, direction.x, format('expectedDirection.x at %f', [distance]));
      CheckEquals( 0, direction.y, format('expectedDirection.y at %f', [distance]));
    end;

    distance := distance + 5;
  end;
end;

procedure TTestSlewCalculator.test_straight_line_slewed_left_cosine;
begin
  nominalRadius := max_rad;
  isSpiral := False;

  isSlewing := True;
  distanceToStartOfSlew := 100;
  slewLength := 100;
  slewAmount := 10;
  slewMode := eSM_Cosine;

  do_test_slew;
end;

procedure TTestSlewCalculator.test_straight_line_slewed_right_cosine;
begin
  nominalRadius := max_rad;
  isSpiral := False;

  isSlewing := True;
  distanceToStartOfSlew := 50;
  slewLength := 100;
  slewAmount := -20;
  slewMode := eSM_Cosine;

  do_test_slew;
end;

initialization
  RegisterTest(TTestSlewCalculator);

end.

