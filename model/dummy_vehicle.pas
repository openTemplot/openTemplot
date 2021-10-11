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

{ }
unit dummy_vehicle;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  point_ex,
  path_interface;

const
  dv_copies_c = 31;                  //  max 32 bgnd dummy vehicles 0-31 0.98.a

  dv_outlines_c = 4999;              //  max 5000 dummy vehicles to draw outline envelope

  dv_envelopes_c = 7;                //  max 8 envelopes


type
  //----------------------------

  Tdummy_vehicle_corners = record         // 0.98.a    mm on grid.
    pt1: Tpex;
    pt2: Tpex;
    b1: Tpex;
    b2: Tpex;
    b3: Tpex;
    b4: Tpex;
    c1: Tpex;
    c2: Tpex;
    c3: Tpex;
    c4: Tpex;
    m1: Tpex;
    m2: Tpex;
    o1: Tpex;
    o2: Tpex;
    o3: Tpex;
    o4: Tpex;
  end;

  Tdummy_vehicle_envelope = record         // 215c    mm on grid.
    b1: Tpex;
    b2: Tpex;
    b3: Tpex;
    b4: Tpex;
    c1: Tpex;
    c2: Tpex;
    c3: Tpex;
    c4: Tpex;
    o1: Tpex;
    o2: Tpex;
    o3: Tpex;
    o4: Tpex;
  end;

  Tdv_envelope = class         // 215c
    dv_outlines: array of Tdummy_vehicle_envelope;
  end;


  Tdv_envelope_list = class(TObjectList<Tdv_envelope>)
  end;

  Tdummy_vehicle_info = class          // 0.98.a  dummy vehicle for clearance checks
  public
    // all in prototype inches...

    dv_start: double;   // to first axle/bogie-pin from CTRL-0
    dv_length: double;   // body length
    dv_width: double;
    dv_clearance: double;
    dv_wheelbase: double;

    constructor Create;
    function generate_envelope(inscale: double; path: IPath): Tdv_envelope;
    procedure calculate_dummy_vehicle_corners(pos_inches: double; inscale: double; path: IPath;
      out corners: Tdummy_vehicle_corners);

  private
    function calculate_2nd_pin(path: IPath; dv1: Tpex; dvs_mm: double; wheelbase_mm: double): Tpex;

  end;



implementation

uses
  matrix_2d;

constructor Tdummy_vehicle_info.Create;
begin
  dv_start := 111;    // to first axle/bogie-pin from CTRL-0  111" = 9ft-3in
  dv_length := 780;     // body length 780" = 65ft
  dv_width := 111;      // body width 111" = 9ft-3in
  dv_clearance := 6;    // clearance on width 6" per vehicle per side
  dv_wheelbase := 558; // wheelbase / bogie centres 558" = 46ft-6in
end;

function Tdummy_vehicle_info.generate_envelope(inscale: double; path: IPath): Tdv_envelope;
const
  step_size: double = 3; // inches per step
var
  dve: Tdv_envelope;
  dv_pos: double;
  number_of_outlines: integer;
  i: integer;
  corners: Tdummy_vehicle_corners;
  p_outline: ^Tdummy_vehicle_envelope;
begin

  dve := Tdv_envelope.Create;
  try
    dv_pos := 0 - dv_length;

    number_of_outlines := Trunc((path.get_curve_length + dv_length * inscale) / (step_size * inscale)) + 1;

    SetLength(dve.dv_outlines, number_of_outlines);

    for i := 0 to High(dve.dv_outlines) do
    begin
      calculate_dummy_vehicle_corners(dv_pos, inscale, path, corners);

      p_outline := @dve.dv_outlines[i];

      p_outline^.b1 := corners.b1;
      p_outline^.b2 := corners.b2;
      p_outline^.b3 := corners.b3;
      p_outline^.b4 := corners.b4;

      p_outline^.c1 := corners.c1;
      p_outline^.c2 := corners.c2;
      p_outline^.c3 := corners.c3;
      p_outline^.c4 := corners.c4;

      p_outline^.o1 := corners.o1;
      p_outline^.o2 := corners.o2;
      p_outline^.o3 := corners.o3;
      p_outline^.o4 := corners.o4;

      dv_pos := dv_pos + step_size;
    end;

  except
    dve.Free;
    raise;
  end;

  Result := dve;
end;


procedure Tdummy_vehicle_info.calculate_dummy_vehicle_corners(pos_inches: double; inscale: double;
  path: IPath; out corners: Tdummy_vehicle_corners);

var
  dv1: Tpex;
  dv2: Tpex;
  dv_direction: Tpex;
  dvs_mm, wheelbase_mm: double;

  body_length, half_body_width, half_clearance_width, end_length_from_pins: double;
  overdraw_width: double;
  transform: Tmatrix_2d;

begin
  if pos_inches < (0 - dv_length) then
    pos_inches := 0 - dv_length;  // 205a limits

  if pos_inches > (path.curve_length / inscale + dv_length - dv_wheelbase) then
    pos_inches := path.curve_length / inscale + dv_length - dv_wheelbase;  // 205a limits

  wheelbase_mm := dv_wheelbase * inscale;  // diagonal, init target wheelbase chord

  dvs_mm := pos_inches * inscale;       //init

  // limit to 1.75 x radius for wheelbase chord ...

  if wheelbase_mm > ABS(path.minimum_radius * 1.75) then
    EXIT;

  dv1 := path.get_xy(dvs_mm);
  dv2 := calculate_2nd_pin(path, dv1, dvs_mm, wheelbase_mm);

  // set up transformation
  transform := Tmatrix_2d.Create;
  try
    dv_direction := (dv2 - dv1).normalise;

    transform.set_x(dv_direction);
    transform.set_y(Tpex.xy(-dv_direction.y, dv_direction.x));
    transform.set_t(dv1);

    // outline calcs ...
    half_clearance_width := (dv_width / 2 + dv_clearance) * inscale;   // half the full clearance width
    half_body_width := dv_width / 2 * inscale;                  // half body width
    end_length_from_pins := (dv_length - dv_wheelbase) / 2 * inscale;   // end length from pins
    body_length := dv_length * inscale;                   // body length

    with corners do
    begin
      // calcs for control template     global results for making copies

      // set up all the points in dummy vehicle space (pin at origin, oriented along x axis)
      // and then apply a transform to all the points

      pt1.set_xy(0, 0);
      pt2.set_xy(wheelbase_mm, 0);

      // calc body outline b1 - b2 - b3 - b4  clockwise corners from bottom left ...
      b1.set_xy(-end_length_from_pins, -half_body_width);
      b2.set_xy(-end_length_from_pins, half_body_width);
      b3.set_xy(b1.x + body_length, b2.y);
      b4.set_xy(b2.x + body_length, b1.y);

      // overdraw blanking corners  33" arbitrary (used only for envelope)...
      overdraw_width := 33 * inscale;
      o1.set_xy(b1.x, b1.y - overdraw_width);
      o2.set_xy(b2.x, b2.y + overdraw_width);
      o3.set_xy(b3.x, b3.y + overdraw_width);
      o4.set_xy(b4.x, b4.y - overdraw_width);

      // calc clearance extensions c1 - c2 - c3 - c4  clockwise corners from bottom left ...
      c1.set_xy(-end_length_from_pins, -half_clearance_width);
      c2.set_xy(-end_length_from_pins, half_clearance_width);
      c3.set_xy(c2.x + body_length, c2.y);
      c4.set_xy(c1.x + body_length, c1.y);

      m1.set_xy(c1.x + body_length / 2, c1.y);
      m2.set_xy(c2.x + body_length / 2, c2.y);


      // now do all the transformations...
      pt1 := transform.transform_point(pt1);
      pt2 := transform.transform_point(pt2);
      b1 := transform.transform_point(b1);
      b2 := transform.transform_point(b2);
      b3 := transform.transform_point(b3);
      b4 := transform.transform_point(b4);
      o1 := transform.transform_point(o1);
      o2 := transform.transform_point(o2);
      o3 := transform.transform_point(o3);
      o4 := transform.transform_point(o4);
      c1 := transform.transform_point(c1);
      c2 := transform.transform_point(c2);
      c3 := transform.transform_point(c3);
      c4 := transform.transform_point(c4);
      m1 := transform.transform_point(m1);
      m2 := transform.transform_point(m2);
    end;//with corners

  finally
    transform.Free;
  end;

end;

function Tdummy_vehicle_info.calculate_2nd_pin(path: IPath; dv1: Tpex; dvs_mm: double; wheelbase_mm: double): Tpex;
var
  dv2: Tpex;
  dv: double;
  error: double;
  chord_length: double;
begin
  // initial guess at offset along curve
  dv := dvs_mm + wheelbase_mm;

  while True do
  begin
    // re-calc position of 2nd bogie pin until chord matches required wheelbase ...
    dv2 := path.get_xy(dv);

    chord_length := SQRT(SQR(dv2.x - dv1.x) + SQR(dv2.y - dv1.y));
    error := wheelbase_mm - chord_length;

    if ABS(error) < 0.005 then
    begin
      // near enough
      Result := dv2;
      Exit;
    end;

    dv := dv + error;
  end;
end;

end.
