unit ClassRegeneratorTest;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  OTClassRegenerator;

type
  TTestableClassRegenerator = class(TOTClassRegenerator);

  TTestClassRegenerator = class(TTestCase)
  published
    procedure TestLoadInput;
    procedure TestParseInput;
    procedure TestParseYaml;

    procedure TestGenerateMemberVars;
  end;

implementation

procedure TTestClassRegenerator.TestLoadInput;
var
  cg: TTestableClassRegenerator;
begin
  // Given a regen object with an associated file name
  // When LoadInput is called
  // Then the Input property contains the contents of the file

  cg := TTestableClassRegenerator.Create('testdata/loadinput.txt');
  try
    cg.LoadInput;

    AssertEquals(5, cg.input.count);
    AssertEquals('Line 1', cg.input[0]);
    AssertEquals('Line 2', cg.input[1]);
    AssertEquals('Line 3', cg.input[2]);
    AssertEquals('Line 4', cg.input[3]);
    AssertEquals('Line 5', cg.input[4]);
  finally
    cg.Free;
  end;
end;

procedure TTestClassRegenerator.TestParseInput;
var
  cg: TTestableClassRegenerator;
begin
  // Given a regen object with sample input loaded
  // When ParseInput is called
  // Then all the required blocks are loaded

  cg := TTestableClassRegenerator.Create('testdata/testparserinput.pas');
  try
     cg.LoadInput;
     AssertEquals( 91, cg.input.Count );

     // When...
     cg.ParseInput;

     // Then...
     AssertNotNull('blocks[biClassYaml]', cg.blocks[biClassYaml]);
     AssertEquals('blocks[biClassYaml].startLine', 10, cg.blocks[biClassYaml].startLine);
     AssertEquals('blocks[biClassYaml].followingLine', 12, cg.blocks[biClassYaml].followingLine);

     AssertNotNull('blocks[biMemberVars]', cg.blocks[biMemberVars]);
     AssertEquals('blocks[biMemberVars].startLine', 19, cg.blocks[biMemberVars].startLine);
     AssertEquals('blocks[biMemberVars].followingLine', 19, cg.blocks[biMemberVars].followingLine);

     AssertNotNull('blocks[biCollections]', cg.blocks[biCollections]);
     AssertEquals('blocks[biCollections].startLine', 22, cg.blocks[biCollections].startLine);
     AssertEquals('blocks[biCollections].followingLine', 22, cg.blocks[biCollections].followingLine);

     AssertNotNull('blocks[biGetSetDeclarations]', cg.blocks[biGetSetDeclarations]);
     AssertEquals('blocks[biGetSetDeclarations].startLine', 30, cg.blocks[biGetSetDeclarations].startLine);
     AssertEquals('blocks[biGetSetDeclarations].followingLine', 30, cg.blocks[biGetSetDeclarations].followingLine);

     AssertNotNull('blocks[biProperty]', cg.blocks[biProperty]);
     AssertEquals('blocks[biProperty].startLine', 37, cg.blocks[biProperty].startLine);
     AssertEquals('blocks[biProperty].followingLine', 37, cg.blocks[biProperty].followingLine);

     AssertNotNull('blocks[biRestoreYamlVars]', cg.blocks[biRestoreYamlVars]);
     AssertEquals('blocks[biRestoreYamlVars].startLine', 55, cg.blocks[biRestoreYamlVars].startLine);
     AssertEquals('blocks[biRestoreYamlVars].followingLine', 55, cg.blocks[biRestoreYamlVars].followingLine);

     AssertNotNull('blocks[biRestoreVars]', cg.blocks[biRestoreVars]);
     AssertEquals('blocks[biRestoreVars].startLine', 64, cg.blocks[biRestoreVars].startLine);
     AssertEquals('blocks[biRestoreVars].followingLine', 64, cg.blocks[biRestoreVars].followingLine);

     AssertNotNull('blocks[biSaveVars]', cg.blocks[biSaveVars]);
     AssertEquals('blocks[biSaveVars].startLine', 72, cg.blocks[biSaveVars].startLine);
     AssertEquals('blocks[biSaveVars].followingLine', 72, cg.blocks[biSaveVars].followingLine);

     AssertNotNull('blocks[biSaveYamlVars]', cg.blocks[biSaveYamlVars]);
     AssertEquals('blocks[biSaveYamlVars].startLine', 80, cg.blocks[biSaveYamlVars].startLine);
     AssertEquals('blocks[biSaveYamlVars].followingLine', 80, cg.blocks[biSaveYamlVars].followingLine);

     AssertNotNull('blocks[biGetSetMethods]', cg.blocks[biGetSetMethods]);
     AssertEquals('blocks[biGetSetMethods].startLine', 84, cg.blocks[biGetSetMethods].startLine);
     AssertEquals('blocks[biGetSetMethods].followingLine', 84, cg.blocks[biGetSetMethods].followingLine);

  finally
    cg.Free;
  end;

end;

procedure TTestClassRegenerator.TestParseYaml;
var
  cg: TTestableClassRegenerator;
begin
  // Given a regen object with sample input loaded and Parsed
  // When ParseYaml is called
  // Then all the attributes are created

  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
     cg.LoadInput;
     cg.ParseInput;

     // When...
     cg.ParseYaml;

     // Then...
     AssertEquals(3, cg.attributeCount);
  finally
    cg.Free;
  end;

end;

procedure TTestClassRegenerator.TestGenerateMemberVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GenerateMemberVars is called
  // Then the expected textis returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateMemberVars;

    // Then
    AssertEquals(3, s.Count);
    AssertEquals('    FTom: Integer;', s[0]);
    AssertEquals('    FDick: Double;', s[1]);
    AssertEquals('    FHarry: String;', s[2]);
  finally
    cg.Free;
    s.Free;
  end;
end;

initialization
  RegisterTest(TTestClassRegenerator);

end.
