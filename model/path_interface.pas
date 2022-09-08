
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

unit path_interface;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  point_ex;

type
{$interfaces corba}
  IPath = interface
    ['IPath']
    function get_curve_length: double;
    function get_minimum_radius: double;
    function get_xy(dist: double): Tpex;

    property curve_length: double read get_curve_length;
    property minimum_radius: double read get_minimum_radius;
  end;

implementation

end.

