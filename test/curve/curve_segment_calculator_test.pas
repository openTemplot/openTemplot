unit curve_segment_calculator_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  curve_segment_calculator,
  curve_parameters_interface,
  point_ex;

type

  { TTestableCurveSegmentCalculator }

  TTestableCurveSegmentCalculator = class(TCurveSegmentCalculator)
  public
    constructor Create(ACurveParameters: ICurveParameters);

    property CurveSegments;
  end;

  { TTestCurveSegmentCalculator }

  TTestCurveSegmentCalculator = class(TTestCase, ICurveParameters)
  private
    fixedRadius: double;
    transitionRadius1: double;
    transitionRadius2: double;
    distanceToTransition: double;
    transitionLength: double;
    isSpiral: boolean;

  protected
    calculator: TTestableCurveSegmentCalculator;

    procedure Setup; override;
    procedure TearDown; override;

    function GetIsSpiral: boolean;
    function GetFixedRadius: double;
    function GetTransitionRadius1: double;
    function GetTransitionRadius2: double;
    function GetDistanceToTransition: double;
    function GetTransitionLength: double;

    function GetIsSlewing: boolean;
    function GetDistanceToStartOfSlew: double;
    function GetSlewLength: double;
    function GetSlewAmount: double;
    function GetSlewMode: ESlewMode;
    function GetSlewFactor: double;

  published
    procedure test_straight;
    procedure test_circle;
    procedure test_straight_transition_circle;
    procedure test_circle_transition_circle;
    procedure test_circle_transition_straight;
    procedure test_transition_straight;
    procedure test_transition_circle;
  end;

implementation

uses
  curve,
  straight_segment,
  circle_segment,
  transition_segment;

{ TTestableCurveSegmentCalculator }

constructor TTestableCurveSegmentCalculator.Create(ACurveParameters: ICurveParameters);
begin
  inherited Create(ACurveParameters);
end;

{ TTestCurveSegmentCalculator }

procedure TTestCurveSegmentCalculator.Setup;
begin
  inherited Setup;
end;

procedure TTestCurveSegmentCalculator.TearDown;
begin
  calculator.Free;
  inherited TearDown;
end;

function TTestCurveSegmentCalculator.GetIsSpiral: boolean;
begin
  Result := isSpiral;
end;

function TTestCurveSegmentCalculator.GetFixedRadius: double;
begin
  Result := fixedRadius;
end;

function TTestCurveSegmentCalculator.GetTransitionRadius1: double;
begin
  Result := transitionRadius1;
end;

function TTestCurveSegmentCalculator.GetTransitionRadius2: double;
begin
  Result := transitionRadius2;
end;

function TTestCurveSegmentCalculator.GetDistanceToTransition: double;
begin
  Result := distanceToTransition;
end;

function TTestCurveSegmentCalculator.GetTransitionLength: double;
begin
  Result := transitionLength;
end;

function TTestCurveSegmentCalculator.GetIsSlewing: boolean;
begin
  Result := False;
end;

function TTestCurveSegmentCalculator.GetDistanceToStartOfSlew: double;
begin
  Result := 0;
end;

function TTestCurveSegmentCalculator.GetSlewLength: double;
begin
  Result := 0;
end;

function TTestCurveSegmentCalculator.GetSlewAmount: double;
begin
  Result := 0;
end;

function TTestCurveSegmentCalculator.GetSlewMode: ESlewMode;
begin
  Result := eSM_Cosine;
end;

function TTestCurveSegmentCalculator.GetSlewFactor: double;
begin
  Result := 1.0;
end;

procedure TTestCurveSegmentCalculator.test_straight;
begin
  fixedRadius := max_rad;
  isSpiral := False;

  calculator := TTestableCurveSegmentCalculator.Create(self);

  CheckEquals(1, calculator.CurveSegments.Count);

  Check(calculator.CurveSegments[0] is TStraightSegment);
end;

procedure TTestCurveSegmentCalculator.test_circle;
begin
  fixedRadius := -1234;
  isSpiral := False;

  calculator := TTestableCurveSegmentCalculator.Create(self);

  CheckEquals(1, calculator.CurveSegments.Count);

  Check(calculator.CurveSegments[0] is TCircleSegment);
end;

procedure TTestCurveSegmentCalculator.test_straight_transition_circle;
begin
  transitionRadius1 := max_rad;
  transitionRadius2 := 2000;
  distanceToTransition := 100;
  transitionLength := 150;
  isSpiral := True;

  calculator := TTestableCurveSegmentCalculator.Create(self);

  CheckEquals(3, calculator.CurveSegments.Count);

  Check(calculator.CurveSegments[0] is TStraightSegment);
  Check(calculator.CurveSegments[1] is TTransitionSegment);
  Check(calculator.CurveSegments[2] is TCircleSegment);
end;

procedure TTestCurveSegmentCalculator.test_circle_transition_circle;
begin
  transitionRadius1 := 1000;
  transitionRadius2 := 2000;
  distanceToTransition := 100;
  transitionLength := 150;
  isSpiral := True;

  calculator := TTestableCurveSegmentCalculator.Create(self);

  CheckEquals(3, calculator.CurveSegments.Count);

  Check(calculator.CurveSegments[0] is TCircleSegment);
  Check(calculator.CurveSegments[1] is TTransitionSegment);
  Check(calculator.CurveSegments[2] is TCircleSegment);
end;

procedure TTestCurveSegmentCalculator.test_circle_transition_straight;
begin
  transitionRadius1 := 1000;
  transitionRadius2 := max_rad;
  distanceToTransition := 100;
  transitionLength := 150;
  isSpiral := True;

  calculator := TTestableCurveSegmentCalculator.Create(self);

  CheckEquals(3, calculator.CurveSegments.Count);

  Check(calculator.CurveSegments[0] is TCircleSegment);
  Check(calculator.CurveSegments[1] is TTransitionSegment);
  Check(calculator.CurveSegments[2] is TStraightSegment);
end;

procedure TTestCurveSegmentCalculator.test_transition_straight;
begin
  transitionRadius1 := 1000;
  transitionRadius2 := max_rad;
  distanceToTransition := 0;
  transitionLength := 150;
  isSpiral := True;

  calculator := TTestableCurveSegmentCalculator.Create(self);

  CheckEquals(2, calculator.CurveSegments.Count);

  Check(calculator.CurveSegments[0] is TTransitionSegment);
  Check(calculator.CurveSegments[1] is TStraightSegment);
end;

procedure TTestCurveSegmentCalculator.test_transition_circle;
begin
  transitionRadius1 := 1000;
  transitionRadius2 := 2000;
  distanceToTransition := 0;
  transitionLength := 150;
  isSpiral := True;


  calculator := TTestableCurveSegmentCalculator.Create(self);

  CheckEquals(2, calculator.CurveSegments.Count);

  Check(calculator.CurveSegments[0] is TTransitionSegment);
  Check(calculator.CurveSegments[1] is TCircleSegment);
end;

initialization
  RegisterTest(TTestCurveSegmentCalculator);

end.
