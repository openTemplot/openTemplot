unit curve;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  point_ex;

const
  maximum_segment_length = 1e100;

  // the maximum value for a radius (approx 62 miles rad)
  // this dimension is in millimetres, and is independent of any scale settings
  // in openTemplot
  //
  // note: this is untyped, so the following typed consts will compile...
  maximum_radius_value = 1e08;

  // typed constant - maximum value for a radius.
  max_rad_limit: double = maximum_radius_value;

  // radius equivalent to "straight", maximum_radius_value - 5000 to allow for offsets without exceeding 1E8 max_rad_limit.
  max_rad: double = maximum_radius_value - 5000;

  // used for testing maximum radius.
  max_rad_test: double = maximum_radius_value - 10000;


type
  TCurveSegment = class
  private
    FSegmentLength: double;
  public
    property segmentLength: double Read FSegmentLength;

    constructor Create(segLength: double);
    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
      virtual; abstract;
  end;

  TCurveSegmentList = class(TObjectList<TCurveSegment>)
  end;

  TCurve = class
  private
    FModified: boolean;
    FNominalRadius: double;
    FNominalRadius2: double;
    FDistanceToTransition: double;
    FTransitionLength: double;
    FIsSpiral: boolean;
    FIsStraight: boolean;
    FIsSimpleCurve: boolean;

    FSegments: TCurveSegmentList;

    procedure UpdateIfModified;
    procedure CalculateCurveSegments;
    procedure SetNominalRadius(const newRadius: double);
    procedure SetNominalRadius2(const newRadius: double);
    procedure SetDistanceToTransition(const newDistance: double);
    procedure SetTransitionLength(const newLength: double);
    procedure SetIsSpiral(const newIsSpiral: boolean);
    function GetIsStraight: boolean;
    function GetIsSimpleCurve: boolean;

  public
    constructor Create;

    property nominalRadius: double Read FNominalRadius Write SetNominalRadius;
    property nominalRadius2: double Read FNominalRadius2 Write SetNominalRadius2;
    property distanceToTransition: double Read FDistanceToTransition Write SetDistanceToTransition;
    property transitionLength: double Read FTransitionLength Write SetTransitionLength;
    property isSpiral: boolean Read FIsSpiral Write SetIsSpiral;
    property isStraight: boolean Read GetIsStraight;
    property isSimpleCurve: boolean Read GetIsSimpleCurve;

    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
  end;

implementation

uses
  Math,
  matrix_2d,
  fresnel_unit;

type
  TStraightSegment = class(TCurveSegment)
  private
    FOrigin: Tpex;
    FDirection: Tpex;

  public
    constructor Create(segLength: double; origin, direction: Tpex);
    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex;
      out radius: double); override;
  end;

  TCircleSegment = class(TCurveSegment)
  private
    FOrigin: Tpex;
    FRadius: double;
    FAngleOffset: double;

  public
    constructor Create(segLength: double; initialPoint, direction: Tpex; radius: double);
    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex;
      out radius: double); override;
  end;

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
//  TCurveSegment
//
constructor TCurveSegment.Create(segLength: double);
begin
  FSegmentLength := segLength;
end;

//
// TStraightSegment
//
constructor TStraightSegment.Create(segLength: double; origin, direction: Tpex);
begin
  inherited Create(segLength);
  FOrigin := origin;
  FDirection := direction;
end;

procedure TStraightSegment.CalculateCurveAt(distance: double; out pt, direction: Tpex;
  out radius: double);
begin
  direction := FDirection;
  pt := FOrigin + FDirection * distance;
  radius := max_rad;
end;

//
// TCircleSegment
//
constructor TCircleSegment.Create(segLength: double; initialPoint, direction: Tpex;
  radius: double);
var
  normal: Tpex;
begin
  inherited Create(segLength);

  // rotate by 90 degrees anticlockwise
  normal.set_xy(-direction.y, direction.x);

  FOrigin := initialPoint + normal * radius;
  FRadius := radius;

  // angle of initialPoint from mathematical 0 (East)
  FAngleOffset := ArcTan2(-normal.y, -normal.x);
end;

procedure TCircleSegment.CalculateCurveAt(distance: double; out pt, direction: Tpex;
  out radius: double);
var
  angle: double;
  sinAngle: double;
  cosAngle: double;
begin
  angle := distance / FRadius + FAngleOffset;

  SinCos(angle, sinAngle, cosAngle);

  pt := FOrigin + Tpex.xy(cosAngle, sinAngle) * FRadius;
  direction.set_xy(-sinAngle, cosAngle);
  radius := FRadius;
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

//
// TCurve
//
constructor TCurve.Create;
begin
  FSegments := TCurveSegmentList.Create;

  // default is straight line
  FModified := True;
  FNominalRadius := max_rad;
  FNominalRadius2 := max_rad;
  FDistanceToTransition := 0;
  FTransitionLength := 100;
  FIsSpiral := False;
end;

procedure TCurve.SetNominalRadius(const newRadius: double);
begin
  if (newRadius <> FNominalRadius) then begin
    FModified := True;
    FNominalRadius := newRadius;
  end;
end;

procedure TCurve.SetNominalRadius2(const newRadius: double);
begin
  if (newRadius <> FNominalRadius2) then begin
    FModified := True;
    FNominalRadius2 := newRadius;
  end;
end;

procedure TCurve.SetDistanceToTransition(const newDistance: double);
begin
  if (newDistance <> FDistanceToTransition) then begin
    FModified := True;
    FDistanceToTransition := newDistance;
  end;
end;

procedure TCurve.SetTransitionLength(const newLength: double);
begin
  if (newLength <> FTransitionLength) then begin
    FModified := True;
    FTransitionLength := newLength;
  end;
end;

procedure TCurve.SetIsSpiral(const newIsSpiral: boolean);
begin
  if (newIsSpiral <> FIsSpiral) then begin
    FModified := True;
    FIsSpiral := newIsSpiral;
  end;
end;

procedure TCurve.UpdateIfModified;
begin
  if FModified then begin
    CalculateCurveSegments;
    FModified := False;
  end;
end;

procedure TCurve.CalculateCurveSegments;
var
  transitionStartPoint: Tpex;
  transitionStartDirection: Tpex;
  transitionEndPoint: Tpex;
  transitionEndDirection: Tpex;
  radius: double;
begin
  FIsStraight := (Abs(FNominalRadius) > max_rad_test) and not FIsSpiral;
  FIsSimpleCurve := not FIsStraight and not FIsSpiral;

  FSegments.Clear;

  if FIsStraight then begin
    // nothing special to calculate for a straight line
    FSegments.Add(TStraightSegment.Create(maximum_segment_length, Tpex.xy(0, 0), Tpex.xy(1, 0)));
  end
  else
  if FIsSimpleCurve then begin
    FSegments.Add(TCircleSegment.Create(maximum_segment_length, Tpex.xy(0, 0),
      Tpex.xy(1, 0), FNominalRadius));
  end
  else begin

    if FDistanceToTransition > 0 then begin
      // do something about initial radius
      if (Abs(FNominalRadius) > max_rad_test) then begin
        FSegments.Add(TStraightSegment.Create(FDistanceToTransition, Tpex.xy(0, 0),
          Tpex.xy(1, 0)));
      end
      else begin
        FSegments.Add(TCircleSegment.Create(FDistanceToTransition, Tpex.xy(0, 0),
          Tpex.xy(1, 0), FNominalRadius));
      end;
      FSegments.Items[0].CalculateCurveAt(FDistanceToTransition, transitionStartPoint,
        transitionStartDirection, radius);
    end
    else begin
      transitionStartPoint.set_xy(0, 0);
      transitionStartDirection.set_xy(1, 0);
    end;

    FSegments.Add(TTransitionSegment.Create(FTransitionLength, transitionStartPoint,
      transitionStartDirection, FNominalRadius, FNominalRadius2));

    FSegments.Items[FSegments.Count - 1].CalculateCurveAt(FTransitionLength,
      transitionEndPoint, transitionEndDirection, radius);

    if (Abs(FNominalRadius2) > max_rad_test) then begin
      FSegments.Add(TStraightSegment.Create(maximum_segment_length,
        transitionEndPoint, transitionEndDirection));
    end
    else begin
      FSegments.Add(TCircleSegment.Create(maximum_segment_length,
        transitionEndPoint, transitionEndDirection, FNominalRadius2));
    end;
  end;
end;

procedure TCurve.CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
var
  i: integer;
  s: TCurveSegment;
begin
  UpdateIfModified;

  for i := 0 to FSegments.Count - 1 do begin
    s := FSegments[i];
    if (distance < s.segmentLength) then begin
      s.CalculateCurveAt(distance, pt, direction, radius);
      Exit;
    end;
    distance := distance - s.segmentLength;
  end;
  pt.set_xy(NaN, NaN);
  direction.set_xy(NaN, NaN);
  radius := NaN;
end;

function TCurve.GetIsStraight: boolean;
begin
  UpdateIfModified;
  Result := FIsStraight;
end;

function TCurve.GetIsSimpleCurve: boolean;
begin
  UpdateIfModified;
  Result := FIsSimpleCurve;
end;

end.
