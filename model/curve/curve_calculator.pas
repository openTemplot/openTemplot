unit curve_calculator;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  point_ex,
  curve_segment;

type
  TCurveCalculator = class
  public
    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
      virtual; abstract;
  end;

  { TCurveSegmentCalculator }

  TCurveSegmentCalculator = class(TCurveCalculator)
  private
    FSegments: TCurveSegmentList;

    procedure CalculateCurveSegments(ACurve: TObject);

  public
    constructor Create(ACurve: TObject);
    destructor Destroy; override;

    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
      override;
  end;

implementation

uses
  Math,
  curve,
  straight_segment,
  circle_segment,
  transition_segment;

{ TCurveSegmentCalculator }

constructor TCurveSegmentCalculator.Create(ACurve: TObject);
begin
  CalculateCurveSegments(ACurve);
end;

destructor TCurveSegmentCalculator.Destroy;
begin
  FSegments.Free;
  inherited;
end;

procedure TCurveSegmentCalculator.CalculateCurveSegments(ACurve: TObject);
var
  curve: TCurve;
  transitionStartPoint: Tpex;
  transitionStartDirection: Tpex;
  transitionEndPoint: Tpex;
  transitionEndDirection: Tpex;
  radius: double;
begin
  curve := ACurve as TCurve;
  if not Assigned(curve) then
    raise Exception.Create('Not passed a TCurveClass instance');

  FSegments := TCurveSegmentList.Create;

  if curve.isStraight then begin
    // nothing special to calculate for a straight line
    FSegments.Add(TStraightSegment.Create(maximum_segment_length, Tpex.xy(0, 0), Tpex.xy(1, 0)));
  end
  else
  if curve.isSimpleCurve then begin
    FSegments.Add(TCircleSegment.Create(maximum_segment_length, Tpex.xy(0, 0),
      Tpex.xy(1, 0), curve.nominalRadius));
  end
  else begin

    if curve.distanceToTransition > 0 then begin
      // do something about initial radius
      if (Abs(curve.nominalRadius) > max_rad_test) then begin
        FSegments.Add(TStraightSegment.Create(curve.distanceToTransition, Tpex.xy(0, 0),
          Tpex.xy(1, 0)));
      end
      else begin
        FSegments.Add(TCircleSegment.Create(curve.distanceToTransition, Tpex.xy(0, 0),
          Tpex.xy(1, 0), curve.nominalRadius));
      end;
      FSegments.Items[0].CalculateCurveAt(curve.distanceToTransition, transitionStartPoint,
        transitionStartDirection, radius);
    end
    else begin
      transitionStartPoint.set_xy(0, 0);
      transitionStartDirection.set_xy(1, 0);
    end;

    FSegments.Add(TTransitionSegment.Create(curve.transitionLength, transitionStartPoint,
      transitionStartDirection, curve.nominalRadius, curve.nominalRadius2));

    FSegments.Items[FSegments.Count - 1].CalculateCurveAt(curve.transitionLength,
      transitionEndPoint, transitionEndDirection, radius);

    if (Abs(curve.nominalRadius2) > max_rad_test) then begin
      FSegments.Add(TStraightSegment.Create(maximum_segment_length,
        transitionEndPoint, transitionEndDirection));
    end
    else begin
      FSegments.Add(TCircleSegment.Create(maximum_segment_length,
        transitionEndPoint, transitionEndDirection, curve.nominalRadius2));
    end;
  end;
end;


procedure TCurveSegmentCalculator.CalculateCurveAt(distance: double;
  out pt, direction: Tpex; out radius: double);
var
  i: integer;
  s: TCurveSegment;
begin
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

end.

