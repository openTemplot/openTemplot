unit straight_segment;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  curve_segment,
  point_ex;

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

implementation

uses
  curve;

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

end.

