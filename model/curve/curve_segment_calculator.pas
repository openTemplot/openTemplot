
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

unit curve_segment_calculator;

{$mode delphi}{$H+}


interface

uses
  Classes,
  SysUtils,
  point_ex,
  curve_calculator,
  curve_segment,
  curve_parameters_interface;

type
  { TCurveSegmentCalculator }

  TCurveSegmentCalculator = class(TCurveCalculator)
  protected
    FIsStraight: boolean;
    FIsSimpleCurve: boolean;
    FSegments: TCurveSegmentList;

    procedure CalculateCurveSegments(ACurveParameters: ICurveParameters);

    // for unit tests...
    property CurveSegments: TCurveSegmentList read FSegments;

  public
    constructor Create(ACurveParameters: ICurveParameters);
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

constructor TCurveSegmentCalculator.Create(ACurveParameters: ICurveParameters);
begin
  CalculateCurveSegments(ACurveParameters);
end;

destructor TCurveSegmentCalculator.Destroy;
begin
  FSegments.Free;
  inherited;
end;

procedure TCurveSegmentCalculator.CalculateCurveSegments(ACurveParameters: ICurveParameters);
var
  transitionStartPoint: Tpex;
  transitionStartDirection: Tpex;
  transitionEndPoint: Tpex;
  transitionEndDirection: Tpex;
  radius: double;
begin
  FIsStraight := (Abs(ACurveParameters.fixedRadius) > max_rad_test) and not ACurveParameters.isSpiral;
  FIsSimpleCurve := not FIsStraight and not ACurveParameters.isSpiral;

  FSegments := TCurveSegmentList.Create;

  if FIsStraight then begin
    // nothing special to calculate for a straight line
    FSegments.Add(TStraightSegment.Create(maximum_segment_length, Tpex.xy(0, 0), Tpex.xy(1, 0)));
  end
  else
  if FIsSimpleCurve then begin
    FSegments.Add(TCircleSegment.Create(maximum_segment_length, Tpex.xy(0, 0),
      Tpex.xy(1, 0), ACurveParameters.fixedRadius));
  end
  else begin

    if ACurveParameters.distanceToTransition > 0 then begin
      // do something about initial radius
      if (Abs(ACurveParameters.transitionStartRadius) > max_rad_test) then begin
        FSegments.Add(TStraightSegment.Create(ACurveParameters.distanceToTransition, Tpex.xy(0, 0),
          Tpex.xy(1, 0)));
      end
      else begin
        FSegments.Add(TCircleSegment.Create(ACurveParameters.distanceToTransition, Tpex.xy(0, 0),
          Tpex.xy(1, 0), ACurveParameters.transitionStartRadius));
      end;
      FSegments.Items[0].CalculateCurveAt(ACurveParameters.distanceToTransition, transitionStartPoint,
        transitionStartDirection, radius);
    end
    else begin
      transitionStartPoint.set_xy(0, 0);
      transitionStartDirection.set_xy(1, 0);
    end;

    FSegments.Add(TTransitionSegment.Create(ACurveParameters.transitionLength, transitionStartPoint,
      transitionStartDirection, ACurveParameters.transitionStartRadius, ACurveParameters.transitionEndRadius));

    FSegments.Items[FSegments.Count - 1].CalculateCurveAt(ACurveParameters.transitionLength,
      transitionEndPoint, transitionEndDirection, radius);

    if (Abs(ACurveParameters.transitionEndRadius) > max_rad_test) then begin
      FSegments.Add(TStraightSegment.Create(maximum_segment_length,
        transitionEndPoint, transitionEndDirection));
    end
    else begin
      FSegments.Add(TCircleSegment.Create(maximum_segment_length,
        transitionEndPoint, transitionEndDirection, ACurveParameters.transitionEndRadius));
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

