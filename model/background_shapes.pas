
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

unit background_shapes;

{$mode delphi}

// records defined in this file are written directly to files

{$ALIGN OFF}

interface

uses
  LCLType,
  Classes,
  SysUtils,
  Graphics,
  point_ex;


//----------------------------

// bgnd shapes ...

type
  Temf = record
    emf_HDC: HDC;
    emf_width_mm: double;    // frame size in mm
    emf_height_mm: double;
  end;

  Timage_shape = record
    image_bitmap: TBitmap;           // image bitmap.
    rotated_bitmap: TBitmap;
    // rotated bitmap for printing. // 0.93.a also used for curving bitmap

    rotated_picture: TPicture;       // picture object to contain rotated bitmap.

    image_width: integer;
    image_height: integer;

    image_metafile: Temf;       // OT-FIRST  219a
  end;

  Tbgimage = class(TPersistent)             // 3-2-01

  public                         // 0.85.a

    image_shape: Timage_shape;

  end;//class


  Tbgnd_shape = record

    // as in BGS file...

    shape_name: string[46];    // name or text string.  205e

    wrap_offset: integer;      // 1/100th mm for picture shape wrapping  205e

    show_transparent: boolean;
    // 0.93.a  for bitmap image    //alignment_byte_1:byte;   // D5 0.81 12-06-05

    shape_code: integer;
    // 0=line, 1=rectangle, 2=circle, 3=text, 4=target mark, -1=picture (bitmap image or metafile)

    shape_style: byte;
    // 0=transparent, 1=blank/solid, 2=cross-hatched;  // 213b was integer (stored MSB (byte) first)

    picture_is_metafile: boolean;      // 213b     // spare1:boolean;

    hide_bits: byte;
    // 214a  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both // spare2:byte;

    option_bits: byte;         // 219b.1 spare3:byte;

    p1: Tpex;
    p2: Tpex;
  end;


  Tbgshape = class(TPersistent)

  public                         // 0.85.a

    bgimage: Tbgimage;           // pointer to image object. 3-2-01.
    bgnd_shape: Tbgnd_shape;     // data for a background shape.

  end;//class

  Tshapefile = file of Tbgnd_shape;       // the shapes data file   .bgs3



implementation

end.
