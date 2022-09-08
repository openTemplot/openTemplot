
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

unit point_ex;

{$mode delphi}
{$ALIGN OFF}

interface

uses
  Classes,
  SysUtils;

type
  Tpex = record                      // x,y point floats (TPoint is integer).
    x: double;
    y: double;
    class function xy(x, y: double): Tpex; static;

    procedure set_xy(x, y: double);

    function magnitude: double;
    function normalise: Tpex;
    function dot(const b: Tpex): double;
    function angleFromVectorToVector( const toVector: Tpex): double;

    class operator negative(a: Tpex): Tpex;
    class operator +(a, b: Tpex): Tpex;
    class operator -(a, b: Tpex): Tpex;
    class operator * (a: TPex; b: double): TPex;
    class operator / (a: TPex; b: double): TPex;
  end;
  Ppex = ^Tpex;

  Textents = record           // 0.93.a
    min: Tpex;
    max: Tpex;
  end;


implementation

uses
  Math;

class function Tpex.xy(x, y: double): Tpex;
begin
  Result.x := x;
  Result.y := y;
end;

procedure Tpex.set_xy(x, y: double);
begin
  self.x := x;
  self.y := y;
end;

class operator Tpex.negative(a: Tpex): Tpex;
begin
  Result.x := -a.x;
  Result.y := -a.y;
end;

class operator Tpex. +(a, b: Tpex): Tpex;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

class operator Tpex. -(a, b: Tpex): Tpex;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;

class operator Tpex. * (a: Tpex; b: double): Tpex;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
end;

class operator Tpex. / (a: Tpex; b: double): Tpex;
begin
  Result.x := a.x / b;
  Result.y := a.y / b;
end;

function Tpex.magnitude: double;
begin
  Result := sqrt(x * x + y * y);
end;

function Tpex.normalise: Tpex;
var
  mag: double;
begin
  mag := magnitude;
  Result.x := x / mag;
  Result.y := y / mag;
end;

function Tpex.dot(const b: Tpex): double;
begin
  Result := x*b.x + y*b.y;
end;

function Tpex.angleFromVectorToVector(const toVector: Tpex): double;
var
  xx: double;
  yy: double;
  normal: Tpex;
begin
  xx := self.dot(toVector);
  normal.set_xy(-y, x);
  yy := normal.dot(toVector);
  Result := arctan2(yy, xx);
end;

end.



