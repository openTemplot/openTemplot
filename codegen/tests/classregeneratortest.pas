unit ClassRegeneratorTest;

{$mode Delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  OTTestUtils,
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
    procedure TestGenerateGetSetDeclarations;
    procedure TestGeneratePropertyDeclarations;
    procedure TestGenerateRestoreYamlVars;
    procedure TestGenerateRestoreVars;
    procedure TestGenerateSaveVars;
    procedure TestGenerateSaveYamlVars;
    procedure TestGenerateGetSetMethods;

    procedure TestGenerate;

  end;

implementation

uses
  FileUtil;

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

procedure TTestClassRegenerator.TestGenerateGetSetDeclarations;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GenerateGetSetDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetDeclarations;

    // Then
    AssertEquals(3, s.Count);
    AssertEquals('    procedure SetTom(const AValue: Integer);', s[0]);
    AssertEquals('    procedure SetDick(const AValue: Double);', s[1]);
    AssertEquals('    procedure SetHarry(const AValue: String);', s[2]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGeneratePropertyDeclarations;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GeneratePropertyDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GeneratePropertyDeclarations;

    // Then
    AssertEquals(3, s.Count);
    AssertEquals('    property tom: Integer read FTom write SetTom;', s[0]);
    AssertEquals('    property dick: Double read FDick write SetDick;', s[1]);
    AssertEquals('    property harry: String read FHarry write SetHarry;', s[2]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreYamlVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GenerateRestoreYamlVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateRestoreYamlVars;

    // Then
    AssertEquals(9, s.Count);
    AssertEquals('  if AName = ''tom'' then', s[0]);
    AssertEquals('    FTom := StrToInteger(AValue)', s[1]);
    AssertEquals('  else', s[2]);
    AssertEquals('  if AName = ''dick'' then', s[3]);
    AssertEquals('    FDick := StrToDouble(AValue)', s[4]);
    AssertEquals('  else', s[5]);
    AssertEquals('  if AName = ''harry'' then', s[6]);
    AssertEquals('    FHarry := StrToString(AValue)', s[7]);
    AssertEquals('  else', s[8]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GenerateRestoreVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateRestoreVars;

    // Then
    AssertEquals(3, s.Count);
    AssertEquals('  RestoreInteger(AStream, FTom);', s[0]);
    AssertEquals('  RestoreDouble(AStream, FDick);', s[1]);
    AssertEquals('  RestoreString(AStream, FHarry);', s[2]);
  finally
    cg.Free;
    s.Free;
  end;
end;


procedure TTestClassRegenerator.TestGenerateSaveVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GenerateSaveVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateSaveVars;

    // Then
    AssertEquals(3, s.Count);
    AssertEquals('  SaveInteger(AStream, FTom);', s[0]);
    AssertEquals('  SaveDouble(AStream, FDick);', s[1]);
    AssertEquals('  SaveString(AStream, FHarry);', s[2]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveYamlVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GenerateSaveYamlVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateSaveYamlVars;

    // Then
    AssertEquals(3, s.Count);
    AssertEquals('  SaveYamlInteger(AEmitter, ''tom'', FTom);', s[0]);
    AssertEquals('  SaveYamlDouble(AEmitter, ''dick'', FDick);', s[1]);
    AssertEquals('  SaveYamlString(AEmitter, ''harry'', FHarry);', s[2]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetMethods;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
begin
  // Given a regen object with defined attributes
  // When GenerateGetSetMethods is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetMethods;

    // Then
    AssertEquals(27, s.Count);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[0]);
    AssertEquals('procedure TSample.SetTom(const AValue: Integer);', s[1]);
    AssertEquals('begin', s[2]);
    AssertEquals('  if AValue <> FTom then begin', s[3]);
    AssertEquals('    SetModified;', s[4]);
    AssertEquals('    FTom := AValue;', s[5]);
    AssertEquals('  end;', s[6]);
    AssertEquals('end;', s[7]);
    AssertEquals('', s[8]);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[9]);
    AssertEquals('procedure TSample.SetDick(const AValue: Double);', s[10]);
    AssertEquals('begin', s[11]);
    AssertEquals('  if AValue <> FDick then begin', s[12]);
    AssertEquals('    SetModified;', s[13]);
    AssertEquals('    FDick := AValue;', s[14]);
    AssertEquals('  end;', s[15]);
    AssertEquals('end;', s[16]);
    AssertEquals('', s[17]);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[18]);
    AssertEquals('procedure TSample.SetHarry(const AValue: String);', s[19]);
    AssertEquals('begin', s[20]);
    AssertEquals('  if AValue <> FHarry then begin', s[21]);
    AssertEquals('    SetModified;', s[22]);
    AssertEquals('    FHarry := AValue;', s[23]);
    AssertEquals('  end;', s[24]);
    AssertEquals('end;', s[25]);
    AssertEquals('', s[26]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerate;
var
  cg: TTestableClassRegenerator;
begin
  // Given a ClassRegenerator with specified
  // When Generate is called
  // Then a new file is created
  //  and the file contents match the expected

  CopyFile('testdata/testparseyaml.pas', 'sample.pas');
  cg := TTestableClassRegenerator.Create('sample.pas');
  try
    cg.Generate;

    AssertTrue('File sample.pas exists?', FileExists('sample.pas'));
    AssertTrue('sample.pas matches expected data?', FileCompare('sample.pas', 'testdata/sample.pas'));
  finally
    cg.Free;
  end;
end;

initialization
  RegisterTest(TTestClassRegenerator);

end.
