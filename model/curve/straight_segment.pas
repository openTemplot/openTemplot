
(*  v1
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  OpenTemplot project contributors

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

====================================================================================
*)

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

