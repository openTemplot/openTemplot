(*

    This file is part of Templot3, a computer program for the design of model railway track.
    Copyright (C) 221  Martin Wynne.  email: martin@templot.com


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

interface

uses
  Classes, SysUtils;

type
  Tpex = record                      // x,y point floats (TPoint is integer).
    x: extended;
    y: extended;
  end;

  Textents = record           // 0.93.a
    min: Tpex;
    max: Tpex;
  end;



implementation

end.

