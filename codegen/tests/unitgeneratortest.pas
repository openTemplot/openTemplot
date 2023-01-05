unit unitgeneratortest;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  otunitgenerator;

type

  TTestUnitGenerator = class(TTestCase)
  published
    procedure TestConstruction;
    procedure TestGenerator;
  end;

implementation

uses
  OTTestUtils;

procedure TTestUnitGenerator.TestConstruction;
var
  tg: TOTUnitGenerator;
begin
  // Given
  // When a TemplateGenerator constructor is called
  // Then a new object is create
  //  and the className property matches the constructor parameter

  tg := TOTUnitGenerator.Create('TEMPLATE_NEW_CLASS', 'Fred');
  try
    AssertEquals('Fred', tg.templateName);
  finally
    tg.Free;
  end;
end;

procedure TTestUnitGenerator.TestGenerator;
var
  tg: TOTUnitGenerator;
begin
  // Given a TemplateGenerator with specified
  // When Generate is called
  // Then a new file is created
  //  and the file contents match the expected

  tg := TOTUnitGenerator.Create('TEMPLATE_NEW_CLASS', 'Fred');
  try
    tg.Generate;

    AssertTrue('File Fred.pas exists?', FileExists('fred.pas'));
    AssertTrue('Fred.pas matches expected data?', FileCompare('fred.pas', 'testdata/fred.pas'));
  finally
    tg.Free;
  end;
end;


initialization

  RegisterTest(TTestUnitGenerator);
end.

