unit TemplateGeneratorTest;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  OTTemplateGenerator;

type
  TTestTemplateGenerator = class(TTestCase)
  published
    procedure TestSimpleSubstitution;
    procedure TestSimpleCondition;
    procedure TestMultiSubstitution;
  end;

implementation

{ TTestTemplateGenerator }

procedure TTestTemplateGenerator.TestSimpleCondition;
var
  testGen: TOTTemplateGenerator;
  inp: TOTTemplateInput;
  dest: TStringList;
begin
  testGen := TOTTemplateGenerator.Create;
  try
    testGen.LoadTemplate('testdata/simple_condition.template');

    CheckEquals(1, testGen.numberOfInputs);

    inp := testGen.input[0];
    Check(inp is TOTTemplateCondition);
    CheckEquals('Include', (inp as TOTTemplateCondition).Name);
    CheckEquals(True, (inp as TOTTemplateCondition).defaultValue);

    (inp as TOTTemplateCondition).userValue := True;
    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(5, dest.Count);
      AssertEquals('Line1', dest[0]);
      AssertEquals('Line2', dest[1]);
      AssertEquals('Line3', dest[2]);
      AssertEquals('Line5', dest[3]);
      AssertEquals('Line6', dest[4]);
    finally
      dest.Free;
    end;

    (inp as TOTTemplateCondition).userValue := False;
    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(5, dest.Count);
      AssertEquals('Line1', dest[0]);
      AssertEquals('Line3', dest[1]);
      AssertEquals('Line4', dest[2]);
      AssertEquals('Line5', dest[3]);
      AssertEquals('Line7', dest[4]);
    finally
      dest.Free;
    end;

  finally
    testGen.Free;
  end;
end;

procedure TTestTemplateGenerator.TestSimpleSubstitution;
var
  testGen: TOTTemplateGenerator;
  inp: TOTTemplateInput;
  dest: TStringList;
begin
  testGen := TOTTemplateGenerator.Create;
  try
    testGen.LoadTemplate('testdata/simple_substitution.template');

    AssertEquals(1, testGen.numberOfInputs);

    inp := testGen.input[0];
    AssertTrue(inp is TOTTemplateSubstitution);
    AssertEquals('Name', (inp as TOTTemplateSubstitution).Name);

    (inp as TOTTemplateSubstitution).userValue := 'Fred';
    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(1, dest.Count);
      AssertEquals('Simple test of Fred, fred, FRED and fred.', dest[0]);
    finally
      dest.Free;
    end;

    (inp as TOTTemplateSubstitution).userValue := 'TomDickHarry';
    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(1, dest.Count);
      AssertEquals('Simple test of TomDickHarry, tomDickHarry, TOM_DICK_HARRY and tom_dick_harry.',
        dest[0]);
    finally
      dest.Free;
    end;

  finally
    testGen.Free;
  end;
end;

procedure TTestTemplateGenerator.TestMultiSubstitution;
var
  testGen: TOTTemplateGenerator;
  inp: TOTTemplateInput;
  subInput: TOTTemplateSubstitution;
  subList: TStringList;
  dest: TStringList;
begin
  testGen := TOTTemplateGenerator.Create;
  try
    testGen.LoadTemplate('testdata/multi_substitution.template');

    AssertEquals(2, testGen.numberOfInputs);

    inp := testGen.input[0];
    AssertTrue(inp is TOTTemplateMultiSubstitution);
    AssertEquals('Comments', (inp as TOTTemplateMultiSubstitution).Name);

    subList := TStringList.Create;
    try
      subList.Add('Entry 1');
      subList.Add('Entry 2');
      subList.Add('Entry 3');

      (inp as TOTTemplateMultiSubstitution).userValues.SetStrings(subList);
    finally
      subList.Free;
    end;

    inp := testGen.input[1];
    AssertTrue(inp is TOTTemplateSubstitution);
    AssertEquals('Name', (inp as TOTTemplateSubstitution).Name);
    (inp as TOTTemplateSubstitution).userValue := 'Fred';

    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(3, dest.Count);
      AssertEquals('Fred Entry 1', dest[0]);
      AssertEquals('Fred Entry 2', dest[1]);
      AssertEquals('Fred Entry 3', dest[2]);
    finally
      dest.Free;
    end;


  finally
    testGen.Free;
  end;
end;

initialization
  RegisterTest(TTestTemplateGenerator);

end.
