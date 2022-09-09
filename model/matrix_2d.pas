
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

unit matrix_2d;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  point_ex;

type
  T3x2_array = array[0..2, 0..1] of double;

  Tmatrix_2d = class
    Data: T3x2_array;

    constructor Create;
    constructor CreateIdentity;

    procedure premultiply_by(t: Tmatrix_2d);
    procedure multiply_by(t: Tmatrix_2d);
    procedure rotate_by(radians: double); overload;
    procedure rotate_by(v: Tpex); overload;
    procedure translate_by(v: Tpex); overload;
    procedure translate_by(x, y: double); overload;
    function transform_point(pt: Tpex): Tpex;
    function transform_vector(pt: Tpex): Tpex;

    procedure set_x(x_column: Tpex);
    procedure set_y(y_column: Tpex);
    procedure set_t(t_column: Tpex);

    function get_x: Tpex;
    function get_y: Tpex;
    function get_t: TPex;
  end;

implementation

constructor Tmatrix_2d.Create;
var
  x, y: Integer;
begin
  for x := 0 to 2 do
    for y := 0 to 1 do
      Data[x, y] := 0;
end;

constructor Tmatrix_2d.CreateIdentity;
var
  x, y: Integer;
begin
  for x := 0 to 2 do
    for y := 0 to 1 do
      Data[x, y] := 0;
  Data[0, 0] := 1;
  Data[1, 1] := 1;
end;

function Tmatrix_2d.get_x: Tpex;
begin
  Result.x := Data[0, 0];
  Result.y := Data[0, 1];
end;

function Tmatrix_2d.get_y: Tpex;
begin
  Result.x := Data[1, 0];
  Result.y := Data[1, 1];
end;

function Tmatrix_2d.get_t: Tpex;
begin
  Result.x := Data[2, 0];
  Result.y := Data[2, 1];
end;

procedure Tmatrix_2d.set_x(x_column: Tpex);
begin
  Data[0, 0] := x_column.x;
  Data[0, 1] := x_column.y;
end;

procedure Tmatrix_2d.set_y(y_column: Tpex);
begin
  Data[1, 0] := y_column.x;
  Data[1, 1] := y_column.y;
end;

procedure Tmatrix_2d.set_t(t_column: Tpex);
begin
  Data[2, 0] := t_column.x;
  Data[2, 1] := t_column.y;
end;

procedure Tmatrix_2d.rotate_by(radians: double);
var
  t: Tmatrix_2d;
begin
  t := Tmatrix_2d.Create;
  try
    t.Data[0, 0] := cos(radians);
    t.Data[1, 1] := t.Data[0, 0];
    t.Data[0, 1] := sin(radians);
    t.Data[1, 0] := -t.Data[0, 1];
    premultiply_by(t);
  finally
    t.Free;
  end;
end;

procedure Tmatrix_2d.rotate_by(v: Tpex);
var
  t: Tmatrix_2d;
begin
  t := Tmatrix_2d.Create;
  try
    t.Data[0, 0] := v.x;
    t.Data[1, 1] := t.Data[0, 0];
    t.Data[0, 1] := v.y;
    t.Data[1, 0] := -t.Data[0, 1];
    premultiply_by(t);
  finally
    t.Free;
  end;
end;

procedure Tmatrix_2d.premultiply_by(t: Tmatrix_2d);
var
  a: T3x2_array;
begin
  a := Data;

  Data[0, 0] := t.Data[0, 0] * a[0, 0] + t.Data[1, 0] * a[0, 1];
  Data[1, 0] := t.Data[0, 0] * a[1, 0] + t.Data[1, 0] * a[1, 1];
  Data[2, 0] := t.Data[0, 0] * a[2, 0] + t.Data[1, 0] * a[2, 1] + t.Data[2, 0];
  Data[0, 1] := t.Data[0, 1] * a[0, 0] + t.Data[1, 1] * a[0, 1];
  Data[1, 1] := t.Data[0, 1] * a[1, 0] + t.Data[1, 1] * a[1, 1];
  Data[2, 1] := t.Data[0, 1] * a[2, 0] + t.Data[1, 1] * a[2, 1] + t.Data[2, 1];
end;

procedure Tmatrix_2d.multiply_by(t: Tmatrix_2d);
var
  a: T3x2_array;
begin
  a := Data;

  Data[0, 0] := a[0, 0] * t.Data[0, 0] + a[1, 0] * t.Data[0, 1];
  Data[1, 0] := a[0, 0] * t.Data[1, 0] + a[1, 0] * t.Data[1, 1];
  Data[2, 0] := a[0, 0] * t.Data[2, 0] + a[1, 0] * t.Data[2, 1] + a[2, 0];
  Data[0, 1] := a[0, 1] * t.Data[0, 0] + a[1, 1] * t.Data[0, 1];
  Data[1, 1] := a[0, 1] * t.Data[1, 0] + a[1, 1] * t.Data[1, 1];
  Data[2, 1] := a[0, 1] * t.Data[2, 0] + a[1, 1] * t.Data[2, 1] + a[2, 1];
end;

procedure Tmatrix_2d.translate_by(v: Tpex);
begin
  translate_by(v.x, v.y);
end;

procedure Tmatrix_2d.translate_by(x, y: double);
var
  t: Tmatrix_2d;
begin
  t := Tmatrix_2d.CreateIdentity;
  try
    t.Data[2, 0] := x;
    t.Data[2, 1] := y;
    premultiply_by(t);
  finally
    t.Free;
  end;
end;

function Tmatrix_2d.transform_point(pt: Tpex): Tpex;
begin
  Result.x := Data[0, 0] * pt.x + Data[1, 0] * pt.y + Data[2, 0];
  Result.y := Data[0, 1] * pt.x + Data[1, 1] * pt.y + Data[2, 1];
end;

function Tmatrix_2d.transform_vector(pt: Tpex): Tpex;
begin
  Result.x := Data[0, 0] * pt.x + Data[1, 0] * pt.y;
  Result.y := Data[0, 1] * pt.x + Data[1, 1] * pt.y;
end;

end.

