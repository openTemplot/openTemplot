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
    procedure TestSubstitutionDefaultValues;
    procedure TestSimpleCondition;
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

      AssertEquals(4, dest.Count);
      AssertEquals('Line1', dest[0]);
      AssertEquals('Line2', dest[1]);
      AssertEquals('Line3', dest[2]);
      AssertEquals('Line5', dest[3]);
    finally
      dest.Free;
    end;

    (inp as TOTTemplateCondition).userValue := False;
    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(4, dest.Count);
      AssertEquals('Line1', dest[0]);
      AssertEquals('Line3', dest[1]);
      AssertEquals('Line4', dest[2]);
      AssertEquals('Line5', dest[3]);
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
    AssertEquals(0, (inp as TOTTemplateSubstitution).defaultValues.Count);

    (inp as TOTTemplateSubstitution).userValue := 'Fred';
    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(1, dest.Count);
      AssertEquals('Simple test of Fred, fred and FRED.', dest[0]);
    finally
      dest.Free;
    end;

    (inp as TOTTemplateSubstitution).userValue := 'TomDickHarry';
    dest := TStringList.Create;
    try
      testGen.Generate(dest);

      AssertEquals(1, dest.Count);
      AssertEquals('Simple test of TomDickHarry, tomDickHarry and TOM_DICK_HARRY.',
        dest[0]);
    finally
      dest.Free;
    end;

  finally
    testGen.Free;
  end;
end;

{!
    Test reading default values for a substitution
}
procedure TTestTemplateGenerator.TestSubstitutionDefaultValues;
var
  testGen: TOTTemplateGenerator;
  inp: TOTTemplateInput;
  subInput: TOTTemplateSubstitution;
begin
  testGen := TOTTemplateGenerator.Create;
  try
    testGen.LoadTemplate('testdata/substitution_default_values.template');

    AssertEquals(1, testGen.numberOfInputs);

    inp := testGen.input[0];
    Check(inp is TOTTemplateSubstitution);
    subInput := inp as TOTTemplateSubstitution;

    AssertEquals('Name', subInput.Name);
    AssertEquals(3, subInput.defaultValues.Count);
    AssertEquals('Option1', subInput.defaultValues[0]);
    AssertEquals('Option2', subInput.defaultValues[1]);
    AssertEquals('Option3', subInput.defaultValues[2]);


  finally
    testGen.Free;
  end;
end;

initialization
  RegisterTest(TTestTemplateGenerator);

end.
