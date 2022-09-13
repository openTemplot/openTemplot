
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

                >>>     NOTE TO DEVELOPERS     <<<
                     DO NOT EDIT THIS COMMENT
              It is inserted in this file by running
                  'python3 scripts/addComment.py'
         The original text lives in scripts/addComment.py.

====================================================================================
*)

unit point_ex_test;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  point_ex;

type
  Ttest_point_ex = class(TTestCase)
  published
    procedure test_xy;
    procedure test_set_xy;
    procedure test_negative;
    procedure test_add;
    procedure test_subtract;
    procedure test_multiply;
    procedure test_divide;
    procedure test_magnitude;
    procedure test_normalise;
    procedure test_dot;
    procedure test_angleFromVectorToVector;
  end;


implementation

procedure Ttest_point_ex.test_xy;
var
  p: Tpex;
begin
  p := Tpex.xy(1, 0);

  AssertEquals('p.x', 1, p.x);
  AssertEquals('p.y', 0, p.y);

  p := Tpex.xy(0, 3);

  AssertEquals('p.x', 0, p.x);
  AssertEquals('p.y', 3, p.y);

  p := Tpex.xy(6, 7);

  AssertEquals('p.x', 6, p.x);
  AssertEquals('p.y', 7, p.y);
end;

procedure Ttest_point_ex.test_set_xy;
var
  p: Tpex;
begin
  p.set_xy(1, 0);

  AssertEquals('p.x', 1, p.x);
  AssertEquals('p.y', 0, p.y);

  p.set_xy(0, 3);

  AssertEquals('p.x', 0, p.x);
  AssertEquals('p.y', 3, p.y);

  p.set_xy(6, 7);

  AssertEquals('p.x', 6, p.x);
  AssertEquals('p.y', 7, p.y);
end;


procedure Ttest_point_ex.test_negative;
var
  p1: Tpex;
  p2: Tpex;
begin
  p1.set_xy(1, 1);
  p2 := -p1;

  AssertEquals('(-{1,1}.x', -1, p2.x);
  AssertEquals('(-{1,1}.y', -1, p2.y);

  p1.set_xy(-3, 0);
  p2 := -p1;

  AssertEquals('(-{-3,0}.x', 3, p2.x);
  AssertEquals('(-{-3,0}.y', 0, p2.y);

end;

procedure Ttest_point_ex.test_add;
var
  p1: Tpex;
  p2: Tpex;
  p3: Tpex;
begin
  p1.set_xy(1, 1);
  p2.set_xy(3, 4);

  p3 := p1 + p2;

  // check operands haven't changed
  AssertEquals('p1.x', 1, p1.x);
  AssertEquals('p1.y', 1, p1.y);
  AssertEquals('p2.x', 3, p2.x);
  AssertEquals('p2.y', 4, p2.y);

  // check sum
  AssertEquals('p3.x', 4, p3.x);
  AssertEquals('p3.y', 5, p3.y);
end;

procedure Ttest_point_ex.test_subtract;
var
  p1: Tpex;
  p2: Tpex;
  p3: Tpex;
begin
  p1.set_xy(1, 1);
  p2.set_xy(3, 4);

  p3 := p1 - p2;

  // check operands haven't changed
  AssertEquals('p1.x', 1, p1.x);
  AssertEquals('p1.y', 1, p1.y);
  AssertEquals('p2.x', 3, p2.x);
  AssertEquals('p2.y', 4, p2.y);

  // check difference
  AssertEquals('p3.x', -2, p3.x);
  AssertEquals('p3.y', -3, p3.y);
end;

procedure Ttest_point_ex.test_multiply;
var
  p: Tpex;
begin
  p := Tpex.xy(1, 2) * 3;

  AssertEquals('p.x', 3, p.x);
  AssertEquals('p.y', 6, p.y);
end;

procedure Ttest_point_ex.test_divide;
var
  p: Tpex;
begin
  p := Tpex.xy(3, 6) / 3;

  AssertEquals('p.x', 1, p.x);
  AssertEquals('p.y', 2, p.y);
end;

procedure Ttest_point_ex.test_magnitude;
var
  m: double;
begin
  m := Tpex.xy(1, 0).magnitude;
  AssertEquals('(1,0)', 1, m);

  m := Tpex.xy(0, -1).magnitude;
  AssertEquals('(0,-1)', 1, m);

  m := Tpex.xy(-3, 4).magnitude;
  AssertEquals('(-3,4)', 5, m);
end;

procedure Ttest_point_ex.test_normalise;
var
  p: Tpex;
begin
  p := Tpex.xy(1, 0).normalise;
  AssertEquals('(1,0).x', 1, p.x);
  AssertEquals('(1,0).y', 0, p.y);

  p := Tpex.xy(2, 0).normalise;
  AssertEquals('(2,0).x', 1, p.x);
  AssertEquals('(2,0).y', 0, p.y);

  p := Tpex.xy(-3, 0).normalise;
  AssertEquals('(-3,0).x', -1, p.x);
  AssertEquals('(-3,0).y', 0, p.y);

  p := Tpex.xy(0, 2).normalise;
  AssertEquals('(0,2).x', 0, p.x);
  AssertEquals('(0,2).y', 1, p.y);

  p := Tpex.xy(3, -3).normalise;
  AssertEquals('(3,-3).x', 1 / sqrt(2), p.x);
  AssertEquals('(3,-3).y', -1 / sqrt(2), p.y);
end;

procedure Ttest_point_ex.test_dot;
var
  a: Tpex;
  b: Tpex;
begin
  a.set_xy(1, 0);

  AssertEquals('a.dot(a)', 1, a.dot(a));
  AssertEquals('a.dot(-a)', -1, a.dot(-a));

  b.set_xy(0, 1);

  AssertEquals('{1,0}.dot({1, 0})', 0, a.dot(b));

  b.set_xy(1 / sqrt(2), 1 / sqrt(2));

  AssertEquals('{1,0}.dot(1/root2, 1/root', 1 / sqrt(2), a.dot(b));
end;

procedure Ttest_point_ex.test_angleFromVectorToVector;
var
  a: Tpex;
  b: Tpex;
  angle: double;

  function radtodeg(a:double): double;
  begin
    Result := a*180/Pi;
  end;

begin
  a.set_xy(1, 0);
  b.set_xy(1, 0);

  angle := a.angleFromVectorToVector(b);
  AssertEquals(0, angle);

  b.set_xy( 1/sqrt(2), 1/sqrt(2));

  angle := a.angleFromVectorToVector(b);
  AssertEquals( 'a to b', 45, radtodeg(angle));

  angle := b.angleFromVectorToVector(a);
  AssertEquals( 'a to b', -45, radtodeg(angle));
end;


initialization

  RegisterTest(Ttest_point_ex);
end.

