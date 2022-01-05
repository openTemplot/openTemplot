unit circle_segment;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  curve_segment,
  point_ex;

type
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

implementation

uses
  Math;

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

end.

