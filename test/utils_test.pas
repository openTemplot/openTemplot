
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

unit utils_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  utils;

type
  Ttest_utils = class(TTestCase)
  published
    procedure test_CheckInt_OK;
    procedure test_CheckInt_outside_range;
    procedure test_CheckInt_malformed_string;
    procedure test_CheckFloat_OK;
    procedure test_CheckFloat_outside_range;
    procedure test_CheckFloat_malformed_string;
  private
    procedure check_CheckInt(str: string; lower, upper: integer; expectRslt: TnumberCheckRslt; value: integer; msg: String);
    procedure check_CheckFloat(str: string; lower, upper: double; expectRslt: TnumberCheckRslt; value: double; msg: String);
  end;


implementation

// -------- 'helper' functions ----------
// Execute a single test of the CheckInt function
procedure Ttest_utils.check_CheckInt(
  str: string;                  // the candidate string
  lower, upper: integer;        // the valid range boundaries
  expectRslt: TnumberCheckRslt; // the expected result
  value: integer;               // the value expected to be set (valid strings only)
  msg: String);                 // a message describing the test

const
  INITIAL = 13579;  // 'magic' number - Avoid using this as a test value !!!

var
  testRslt: TnumberCheckRslt;
  testInt: integer;
begin
  testInt := INITIAL;

  testRslt := CheckInt(str, lower, upper, testInt);

  AssertEquals(msg, ord(expectRslt), ord(testRslt));
  if expectRslt = ncValid then
    AssertEquals(msg + ' value', value, testInt)
  else
    AssertEquals(msg + ' value', INITIAL, testInt);

  end;


// Executes a single test of the CheckFloat function
procedure Ttest_utils.check_CheckFloat(
  str: string;                  // the candidate string
  lower, upper: double;         // the valid range boundaries
  expectRslt: TnumberCheckRslt; // the expected resule
  value: double;                // the value expected to be set (valid strings only)
  msg: String);                 // a message describing the test

const
  INITIAL = 3.141592653589793;  // 'magic' number - Avoid using this as a test value !!!

var
  testRslt:  TNumberCheckRslt;
  testFloat: double;

  begin
  testFloat := INITIAL;

  testRslt := CheckFloat(str, lower, upper, testFloat);

  AssertEquals(msg, ord(expectRslt), ord(testRslt));
  if expectRslt = ncValid then
    AssertEquals(msg + ' value', value, testFloat)
  else
    AssertEquals(msg + ' value', INITIAL, testFloat);

  end;

// -------- CheckInt test cases ----------

// Test valid input strings within range
procedure Ttest_utils.test_CheckInt_OK;

  begin
  //             str    min  max   rslt  val  description
  check_CheckInt('',    -13,  47,  ncValid,   0, 'nullstr zero');
  check_CheckInt('',     23,  47,  ncValid,  23, 'nullstr bottom value');
  check_CheckInt('',    -13,  -4,  ncValid,  -4, 'nullstr top value');
  check_CheckInt('13',   13,  47,  ncValid,  13, 'bottom value');
  check_CheckInt('31',   13,  47,  ncValid,  31, 'middle value');
  check_CheckInt('-31', -53,  47,  ncValid, -31, 'middle value -ve');
  check_CheckInt('47',   13,  47,  ncValid,  47, 'top value');

  end;

// Test valid input strings outside range
procedure Ttest_utils.test_CheckInt_outside_range;

  begin
  //             str    min  max   rslt   val  description
  check_CheckInt('12',   13,  47,  ncOutOfRange,   0, 'too low');
  check_CheckInt('48',   13,  47,  ncOutOfRange,   0, 'too high');

end;

// Test valid malformed strings
procedure Ttest_utils.test_CheckInt_malformed_string;

  begin
  //             str    min  max   rslt   val  description
  check_CheckInt('31n',  13,  47,  ncMalformed,   0, 'ends in n');
  check_CheckInt('n31',  13,  47,  ncMalformed,   0, 'starts with n');
  check_CheckInt('3.1',  13,  47,  ncMalformed,   0, 'has a point');

end;

// -------- CheckFloat test cases ----------

procedure Ttest_utils.test_CheckFloat_OK;

begin
  //                str      min    max   rslt   val  description
  check_CheckFloat('',     -13.7,  47.2, ncValid,   0.0, 'null string in range');
  check_CheckFloat('',     -13.7,  -4.2, ncValid,  -4.2, 'null string upperBound');
  check_CheckFloat('',      13.7,  47.2, ncValid,  13.7, 'null string lowerBound');
  check_CheckFloat('13.7',  13.7,  47.2, ncValid,  13.7, 'bottom value');
  check_CheckFloat('31.5',  13.7,  47.2, ncValid,  31.5, 'middle value');
  check_CheckFloat('-31.5',-53.7,  47.2, ncValid, -31.5, 'middle value -ve');
  check_CheckFloat('-31.5',-53.7,  47.2, ncValid, -31.5, 'middle value -ve');
  check_CheckFloat('.31',   0.0,   47.2, ncValid,  0.31, 'no integer part');
  check_CheckFloat('31.',   13.7,  47.2, ncValid,  31.0, 'no decimal part');
  check_CheckFloat('29',    13.7,  47.2, ncValid,  29.0, 'integer only');
  check_CheckFloat('47.2',  13.7,  47.2, ncValid,  47.2, 'top value');

end;

procedure Ttest_utils.test_CheckFloat_outside_range;

begin
  //                str      min    max   rslt   val  description
  check_CheckFloat('13.699',13.7,  47.2, ncOutOfRange, 0.0, 'too low');
  check_CheckFloat('47.201',13.7,  47.2, ncOutOfRange, 0.0, 'too high');

end;

procedure Ttest_utils.test_CheckFloat_malformed_string;

begin
  //                str      min    max   rslt   val  description
  check_CheckFloat('13.2x', 13.7,  47.2, ncMalformed, 0.0, 'ends with x');
  check_CheckFloat('x13.2', 13.7,  47.2, ncMalformed, 0.0, 'starts with x');
  check_CheckFloat('6..31', 13.7,  47.2, ncMalformed, 0.0, 'double point');
  check_CheckFloat('6.3.1', 13.7,  47.2, ncMalformed, 0.0, 'two points');

end;

initialization

  RegisterTest(Ttest_utils);

end.

