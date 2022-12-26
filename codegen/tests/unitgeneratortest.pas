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

function FileCompare(const AFile1, AFile2: string): boolean;
var
  str1: string;
  str2: string;
  file1: TStringStream;
  file2: TStringStream;
begin
  file1 := TStringStream.Create;
  try
    file1.LoadFromFile(AFile1);
    str1 := file1.DataString;
  finally
    file1.Free;
  end;

  file2 := TStringStream.Create;
  try
    file2.LoadFromFile(AFile2);
    str2 := file2.DataString;
  finally
    file2.Free;
  end;

  Result := (str1 = str2);
end;

procedure TTestUnitGenerator.TestConstruction;
var
  tg: TOTUnitGenerator;
begin
  // Given
  // When a TemplateGenerator constructor is called
  // Then a new object is create
  //  and the className property matches the constructor parameter

  tg := TOTUnitGenerator.Create('testdata/testgenerator.template', 'Fred');
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

  tg := TOTUnitGenerator.Create('testdata/testgenerator.template', 'Fred');
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

