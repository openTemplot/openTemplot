(*

    This file is part of Templot3, a computer program for the design of model railway track.
    Copyright (C) 2021  Martin Wynne.  email: martin@templot.com


    This program is free software: you may redistribute it and/or modify
    it under the terms of the GNU General Public Licence as published by
    the Free Software Foundation, either version 3 of the Licence, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public Licence for more details.

    You should have received a copy of the GNU General Public Licence
    along with this program. See the files: licence.txt or templotmec.lpr

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
    x: extended;
    y: extended;
    class function xy(x, y: extended): Tpex; static;

    procedure set_xy(x, y: extended);

    function magnitude: double;
    function normalise: Tpex;
    class operator +(a, b: Tpex): Tpex;
    class operator -(a, b: Tpex): Tpex;
  end;

  Textents = record           // 0.93.a
    min: Tpex;
    max: Tpex;
  end;



implementation

class function Tpex.xy(x, y: extended): Tpex;
begin
  Result.x := x;
  Result.y := y;
end;

procedure Tpex.set_xy(x, y: extended);
begin
  self.x := x;
  self.y := y;
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

end.


