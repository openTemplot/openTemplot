
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

unit extended_utils_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  extended_utils;

type
  Ttest_extended_utils = class(TTestCase)
  published
    procedure test_e2d_success;
    procedure test_e2d_underflow;
    procedure test_e2d_overflow;
  end;

{
Data on floating point formats:
  Format 	                                  Single 	        Double 	                    Extended
  Length (bits) 	                          32 	            64 	                        80
  Exponent bits 	                          8 	            11 	                        15
  Exponent bias 	                          127 	          1023 	                      16383
  Smallest exponent 	                      -126 	          -1022 	                    -16382
  Largest exponent 	                        +127 	          +1023 	                    +16383
  Precision 	                              24 	            53 	                        64
  Smallest positive value 	                1.4012985e-45 	2.4703282292062327e-324 	  1.82259976594123730126e-4951
  Smallest positive normalized value 	      1.1754944e-38 	2.2250738585072010e-308 	  3.36210314311209350626e-4932
  Largest positive value 	                  3.4028235e+38 	1.7976931348623157e+308 	  1.18973149535723176502e+4932
}

implementation

const
  delta = 0.0000000000000001;
  bias = 16383;

{ First a helper function to create TExtBytes values:
  sign     - 0/1 to indicate +ve/-ve
  exponent - actual (binary) exponent to be stored
  mantissa - Mantissa bit-pattern to be stored
}
function make_extended(sign: byte; exponent:word; mantissa:Qword): TExtBytes;
var
  rslt: TExtBytes;
  signAndExp: Uint16 absolute rslt[8];
  mant: Qword absolute rslt[0];
begin
  signAndExp := sign shl 15;
  signAndExp := signAndExp or ((exponent+bias) and $7FFF);
  mant := mantissa;
  Result := rslt;
end;

{ then the actual tests }
procedure Ttest_extended_utils.test_e2d_success;
var
  eBytes: TExtBytes;
begin
  eBytes := make_extended(0, -bias, 0);
  CheckEquals(0.0, extendedToDouble(eBytes), delta, 'zero value');

  eBytes := make_extended(0, 3, $c000000000000000);
  CheckEquals(12.0, extendedToDouble(eBytes), delta, '12.0 value');

  eBytes := make_extended(1, -4, $cccccccccccccccd);
  CheckEquals(-1/10, extendedToDouble(eBytes), delta, 'minus 1/10');

  eBytes := make_extended(0, 1023, $ffffffffffffffff);
  CheckEquals(1.7976931348623157e+308, extendedToDouble(eBytes), delta, 'largest number');


end;

procedure Ttest_extended_utils.test_e2d_underflow;
var
  eBytes: TExtBytes;
begin
  eBytes := make_extended(0, -1023, $c000000000000000);
  try
    extendedToDouble(eBytes);
  except
    exit;
  end;
  raise Exception.create('Error: Exception not thrown on underflow');
end;

procedure Ttest_extended_utils.test_e2d_overflow;
var
  eBytes: TExtBytes;
begin
  eBytes := make_extended(0, 1024, $c000000000000000);
  try
    extendedToDouble(eBytes);
  except
    exit;
  end;
  raise Exception.create('Error: Exception not thrown on overflow');
end;


initialization
  RegisterTest(Ttest_extended_utils);
end.

