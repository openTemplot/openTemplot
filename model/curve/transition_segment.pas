unit transition_segment;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  curve_segment,
  point_ex,
  matrix_2d;

type
  TTransitionSegment = class(TCurveSegment)
  private
    // transform point from Cornu Spiral to Curve Space
    FTransform: Tmatrix_2d;
    // Scaling factor to change from Curve Space to Cornu Spiral
    FScalingFactor: double;
    // Offset of starting point in transition from Spiral Origin (pre-scaling)
    FStartOffsetFromOrigin: double;
    // Curvature at some point on the transition (pre-scaling)
    FCurvature: double;
    // Distance from Spiral Origin to the specified curvature point (pre-scaling)
    FLength: double;
    // 1 or -1 for +ve or -ve radius curves
    FDirectionSign: double;

  public
    constructor Create(segLength: double; initialPoint, initialDirection: Tpex;
      initialRadius, finalRadius: double);
    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex;
      out radius: double); override;
  end;

implementation

uses
  fresnel_unit,
  curve;

function RadiusToCurvature(radius: double): double;
begin
  if (radius > max_rad_test) then
    Result := 0
  else
    Result := 1 / radius;
end;

function CurvatureToRadius(curvature: double): double;
begin
  if (curvature = 0) then
    Result := max_rad
  else
    Result := 1 / curvature;
end;

//
// TTransitionSegment
//
constructor TTransitionSegment.Create(segLength: double; initialPoint, initialDirection: Tpex;
  initialRadius, finalRadius: double);
var
  initialCurvature: double;
  finalCurvature: double;
  startDistance: double;
  endDistance: double;
  arcLength: double;
  radius: double;
  curveStart: Tpex;
  curveDirection: Tpex;
  dummyRadius: double;
begin
  inherited Create(segLength);

  // calculate the start and end "distances" along the curve
  initialCurvature := RadiusToCurvature(initialRadius);
  finalCurvature := RadiusToCurvature(finalRadius);

  if Abs(initialRadius) < Abs(finalRadius) then begin
    radius := Abs(initialRadius);
    startDistance := -initialCurvature * segLength / (initialCurvature - finalCurvature);
    arcLength := Abs(startDistance);
  end
  else begin
    radius := Abs(finalRadius);
    endDistance := segLength * finalCurvature / (finalCurvature - initialCurvature);
    startDistance := endDistance - segLength;
    arcLength := Abs(endDistance);
  end;

  if initialCurvature < finalCurvature then begin
    FDirectionSign := 1;
  end
  else begin
    FDirectionSign := -1;
  end;

  // calculate scaling factor
  FScalingFactor := 1 / sqrt(Pi * arcLength * radius);

  // set up ready for calculations...
  FLength := arcLength;
  FCurvature := RadiusToCurvature(radius);
  FStartOffsetFromOrigin := startDistance;
  FTransform := Tmatrix_2d.CreateIdentity;


  // calculate the start point/direction
  CalculateCurveAt(0, curveStart, curveDirection, dummyRadius);

  // calculate the transform required
  FTransform.translate_by(-curveStart);
  FTransform.rotate_by(-initialDirection.angleFromVectortoVector(curveDirection));
  FTransform.translate_by(initialPoint);
end;

procedure TTransitionSegment.CalculateCurveAt(distance: double; out pt, direction: Tpex;
  out radius: double);
var
  distanceFromOrigin: double;
  scaledDistance: double;
  curvatureAtDistance: double;
  s: double;
  c: double;
  theta: double;
  p: Tpex;
  d: Tpex;
begin
  distanceFromOrigin := distance + FStartOffsetFromOrigin;
  scaledDistance := distanceFromOrigin * FScalingFactor;

  Fresnel(scaledDistance, s, c);

  p.set_xy(c, s * FDirectionSign);
  p := p / FScalingFactor;

  curvatureAtDistance := FDirectionSign * FCurvature * distanceFromOrigin / FLength;
  radius := CurvatureToRadius(curvatureAtDistance);

  theta := distanceFromOrigin * curvatureAtDistance / 2.0;
  d.set_xy(cos(theta), sin(theta));

  pt := FTransform.transform_point(p);
  direction := FTransform.transform_vector(d);
end;


end.

