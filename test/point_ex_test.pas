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
    procedure test_add;
    procedure test_subtract;
    procedure test_magnitude;
    procedure test_normalise;
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

initialization

  RegisterTest(Ttest_point_ex);
end.

