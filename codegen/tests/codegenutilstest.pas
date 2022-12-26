unit CodeGenUtilsTest;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  OTCodeGenUtils;

type
  TTestCodeGenUtils = class(TTestCase)

  published
    procedure TestFirstUpper;
    procedure TestFirstLower;
    procedure TestAllUpper;
    procedure TestAllUpperToCamelCase;
  end;


implementation

{ TTestCodeGenUtils }

procedure TTestCodeGenUtils.TestAllUpper;
begin
  AssertEquals('', AllUpper(''));
  AssertEquals('TERRAIN_BLOCK', AllUpper('terrainBlock'));
  AssertEquals('BLOCK', AllUpper('Block'));
end;

procedure TTestCodeGenUtils.TestAllUpperToCamelCase;
begin
  AssertEquals('', AllUpperToCamelCase(''));
  AssertEquals('terrainBlock', AllUpperToCamelCase('TERRAIN_BLOCK'));
  AssertEquals('block', AllUpperToCamelCase('BLOCK'));
  AssertEquals('fred', AllUpperToCamelCase('Fred'));
  AssertEquals('Fred', AllUpperToCamelCase('_FRED'));

end;

procedure TTestCodeGenUtils.TestFirstLower;
begin
  AssertEquals('', FirstLower(''));
  AssertEquals('terrainBlock', FirstLower('terrainBlock'));
  AssertEquals('block', FirstLower('Block'));
end;

procedure TTestCodeGenUtils.TestFirstUpper;
begin
  AssertEquals('', FirstUpper(''));
  AssertEquals('TerrainBlock', FirstUpper('terrainBlock'));
  AssertEquals('Block', FirstUpper('Block'));
end;


initialization
  RegisterTest(TTestCodeGenUtils);

end.
