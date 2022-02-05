unit slew_calculator;

{$mode delphi}{$H+}


interface

uses
  Classes,
  SysUtils,
  point_ex,
  curve_calculator,
  curve_parameters_interface;

type
  { TSlewCalculator }

  TSlewCalculator = class(TCurveCalculator)
  private
    FSegmentCalculator: TCurveCalculator;

    FDistanceToStartOfSlew: double;
    FSlewLength: double;
    FSlewAmount: double;
    FSlewMode: ESlewMode;
    FSlewFactor: double;

    FRotationSin: double;
    FRotationCos: double;
    FYMax: double;

    procedure CalculateSlewSegment(ACurve: ICurveParameters);

  public
    constructor Create(ACurve: ICurveParameters; ASegmentCalculator: TCurveCalculator);
    destructor Destroy; override;

    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
      override;
  end;

implementation

uses
  Math,
  matrix_2d;

{ TSlewCalculator }

constructor TSlewCalculator.Create(ACurve: ICurveParameters;
  ASegmentCalculator: TCurveCalculator);
begin
  CalculateSlewSegment(ACurve);
  // don't take ownership until we're sure we're not going to fail with an exception...
  FSegmentCalculator := ASegmentCalculator;
end;

destructor TSlewCalculator.Destroy;
begin
  FSegmentCalculator.Free;
  inherited Destroy;
end;

procedure TSlewCalculator.CalculateSlewSegment(ACurve: ICurveParameters);
var
  y: double;
  yDash: double;
  rotation: Tpex;
begin
  FDistanceToStartOfSlew := ACurve.distanceToStartOfSlew;
  FSlewLength := ACurve.slewLength;
  FSlewAmount := ACurve.slewAmount;
  FSlewMode := ACurve.slewMode;
  FSlewFactor := ACurve.slewFactor;

  if FSlewMode = eSM_Tanh then begin
    y := tanh(FSlewFactor);
    yDash := 1 - sqr(tanh(FSlewFactor));
    rotation := Tpex.xy(1, yDash).normalise;

    FRotationCos := rotation.x;
    FRotationSin := rotation.y;

    FYMax := FRotationSin * FSlewFactor + FRotationCos * y;
  end;

end;

procedure TSlewCalculator.CalculateCurveAt(distance: double; out pt, direction: Tpex;
  out radius: double);
var
  slewDistance: double;
  curveNormal: Tpex;
  offset: double;
  slope: double;
  slopeDirection: Tpex;
  rotation: Tmatrix_2d;
  angle: double;
begin
  FSegmentCalculator.CalculateCurveAt(distance, pt, direction, radius);

  if distance < FDistanceToStartOfSlew then
    Exit;

  curveNormal.set_xy(-direction.y, direction.x);

  slewDistance := distance - FDistanceToStartOfSlew;
  if slewDistance >= FSlewLength then begin
    pt := pt + curveNormal * FSlewAmount;
    Exit;
  end;

  if FSlewMode = eSM_Cosine then begin
    offset := FSlewAmount * (1 - cos(slewDistance * Pi / FSlewLength)) / 2;
    slope := (Pi * FSlewAmount * sin(slewDistance * Pi / FSlewLength)) / (2 * FSlewLength);
  end
  else begin
    angle := 2 * FSlewFactor * (slewDistance / FSlewLength - 0.5);
    offset := (FRotationSin * angle + FRotationCos * tanh(angle) + FYMax) * FSlewAmount / (2 * FYMax);
    slope := (FSlewAmount * FSlewFactor/(FYMax * FSlewLength)) * (FRotationSin + FRotationCos * (1 - sqr(tanh(angle))));
  end;

  pt := pt + curveNormal * offset;
  slopeDirection := Tpex.xy(1, slope).normalise;

  rotation := Tmatrix_2d.CreateIdentity;
  try
    rotation.rotate_by(slopeDirection);
    direction := rotation.transform_vector(direction);
  finally
    rotation.Free;
  end;

end;

end.

