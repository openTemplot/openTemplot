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
    procedure test_ValidInt;
    procedure test_ValidFloat;
  end;


implementation

procedure Ttest_utils.test_ValidInt;

var
  str: string;
  int: integer;
begin
  // null input string is equivalent to '0'
  str := '';
  int := 23;
  AssertEquals('nullstr low', False, ValidInt(str, 13, 47, int));
  AssertEquals('nullstr low int', 23, int);

  str := '';
  int := 23;
  AssertEquals('nullstr OK', True, ValidInt(str, -13, 47, int));
  AssertEquals('nullstr OK int', 0, int);

  // Values in range
  str := '13';
  AssertEquals('bottom value', True, ValidInt(str, 13, 47, int));
  AssertEquals('bottom value int', 13, int);

  str := '31';
  AssertEquals('middle value', True, ValidInt(str, 13, 47, int));
  AssertEquals('middle value int', 31, int);

  str := '47';
  AssertEquals('top value', True, ValidInt(str, 13, 47, int));
  AssertEquals('top value int', 47, int);

  // Values outside range
  int := 23;

  str := '12';
  AssertEquals('too low', False, ValidInt(str, 13, 47, int));
  AssertEquals('too low int', 23, int);

  str := '48';
  AssertEquals('too high', False, ValidInt(str, 13, 47, int));
  AssertEquals('too high int', 23, int);

  // Mal-formed strings
  int := 23;

  str := '31x';
  AssertEquals('ends in x', False, ValidInt(str, 13, 47, int));
  AssertEquals('ends in x int', 23, int);

  str := 'x31';
  AssertEquals('starts with x', False, ValidInt(str, 13, 47, int));
  AssertEquals('starts with x int', 23, int);

  str := '.31';
  AssertEquals('starts with dot', False, ValidInt(str, 13, 47, int));
  AssertEquals('starts with dot int', 23, int);

end;

procedure Ttest_utils.test_ValidFloat;

var
  str: string;
  flt: double;
  pi: double = 3.141592653589793;
begin
  // null input string is equivalent to '0.0'
  str := '';
  flt := pi;
  AssertEquals('null string under range', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('null string under range flt', pi, flt);

  str := '';
  flt := pi;
  AssertEquals('null string in range', True, ValidFloat(str, -13.7, 47.2, flt));
  AssertEquals('null string in range flt', 0, flt);

  // Values in range
  str := '13.7';
  AssertEquals('bottom value', True, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('bottom value flt', 13.7, flt);

  str := '31.5';
  AssertEquals('middle value', True, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('middle value flt', 31.5, flt);

  str := '47.2';
  AssertEquals('top value', True, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('top value flt', 47.2, flt);

  // Values outside range
  flt := pi;

  str := '13.699999';
  AssertEquals('too low', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('too low flt', pi, flt);

  str := '47.200001';
  AssertEquals('too high', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('too high flt', pi, flt);

  // Mal-formed strings
  flt := pi;

  str := '31.x';
  AssertEquals('ends with x', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('ends with x flt', pi, flt);

  str := 'x.31';
  AssertEquals('starts with x', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('starts with x flt', pi, flt);

  str := '.31';
  AssertEquals('missing integer part', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('missing integer part flt', pi, flt);

  // Missing decimal part is not considered an error
  //str := '31.';
  //AssertEquals('missing decimal part', False, ValidFloat(str, 13.7, 47.2, flt));
  //AssertEquals('missing decimal part flt', 31.0, flt);

  str := '6..31';
  AssertEquals('double point', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('double point flt', pi, flt);

  str := '6.4.31';
  AssertEquals('two points', False, ValidFloat(str, 13.7, 47.2, flt));
  AssertEquals('two points flt', pi, flt);

end;

initialization

  RegisterTest(Ttest_utils);

end.

