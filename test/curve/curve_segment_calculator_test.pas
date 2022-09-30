
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

unit curve_segment_calculator_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  curve_segment_calculator,
  curve_parameters_interface;

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
    transitionStartRadius: double;
    transitionEndRadius: double;
    distanceToTransition: double;
    transitionLength: double;
    isSpiral: boolean;

  protected
    calculator: TTestableCurveSegmentCalculator;

    procedure Setup; override;
    procedure TearDown; override;

    function GetIsSpiral: boolean;
    function GetFixedRadius: double;
    function GetTransitionStartRadius: double;
    function GetTransitionEndRadius: double;
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

function TTestCurveSegmentCalculator.GetTransitionStartRadius: double;
begin
  Result := transitionStartRadius;
end;

function TTestCurveSegmentCalculator.GetTransitionEndRadius: double;
begin
  Result := transitionEndRadius;
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
  Result := smCosine;
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
  transitionStartRadius := max_rad;
  transitionEndRadius := 2000;
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
  transitionStartRadius := 1000;
  transitionEndRadius := 2000;
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
  transitionStartRadius := 1000;
  transitionEndRadius := max_rad;
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
  transitionStartRadius := 1000;
  transitionEndRadius := max_rad;
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
  transitionStartRadius := 1000;
  transitionEndRadius := 2000;
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
