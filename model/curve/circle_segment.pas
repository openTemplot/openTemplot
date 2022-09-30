
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

