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

  { TTestClassRegenerator }

  TTestClassRegenerator = class(TTestCase)
  private
    procedure AssertEquals(const expected: TArray<string>; actual: TStrings); overload;

  published
    procedure TestLoadInput;
    procedure TestParseInput;
    procedure TestParseYaml;
    procedure TestParseYamlCollectionOperations;

    procedure TestGenerateMemberVars;
    procedure TestGenerateGetSetDeclarations;
    procedure TestGeneratePropertyDeclarations;
    procedure TestGenerateRestoreYamlVars;
    procedure TestGenerateRestoreVars;
    procedure TestGenerateSaveVars;
    procedure TestGenerateSaveYamlVars;
    procedure TestGenerateGetSetMethods;

    procedure TestGenerate;

    procedure TestParseYamlCollections;
    procedure TestGenerateMemberVarsCollections;
    procedure TestGenerateGetSetDeclarationsCollections;
    procedure TestGeneratePropertyDeclarationsCollections;
    procedure TestGenerateRestoreYamlVarsCollections;
    procedure TestGenerateRestoreVarsCollections;
    procedure TestGenerateSaveVarsCollections;
    procedure TestGenerateSaveYamlVarsCollections;
    procedure TestGenerateGetSetMethodsCollections;

    procedure TestGeneratePublicDeclarations;
    procedure TestGenerateGetSetMethodsCollectionOperations;

    procedure TestGenerateGetSetDeclarationsAccessControl;
    procedure TestGeneratePropertyDeclarationsAccessControl;
    procedure TestGenerateGetSetMethodsAccessControl;

    procedure TestGenerateMemberVarsOwns;
    procedure TestGenerateGetSetDeclarationsOwns;
    procedure TestGeneratePropertyDeclarationsOwns;
    procedure TestGenerateRestoreYamlVarsOwns;
    procedure TestGenerateRestoreVarsOwns;
    procedure TestGenerateSaveVarsOwns;
    procedure TestGenerateSaveYamlVarsOwns;
    procedure TestGenerateCreateOwns;
    procedure TestGenerateDestroyOwns;
    procedure TestGenerateGetSetMethodsOwns;

  end;

implementation

uses
  FileUtil;

procedure TTestClassRegenerator.AssertEquals(const expected: TArray<string>; actual: TStrings);
var
  i: Integer;
begin
  AssertEquals(Length(expected), actual.Count);
  for i := 0 to High(expected) do begin
    AssertEquals(expected[i], actual[i]);
  end;
end;

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

    AssertEquals(5, cg.input.Count);
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
    AssertEquals(108, cg.input.Count);

    // When...
    cg.ParseInput;

    // Then...
    AssertNotNull('blocks[biClassYaml]', cg.blocks[biClassYaml]);
    AssertEquals('blocks[biClassYaml].startLine', 10, cg.blocks[biClassYaml].startLine);
    AssertEquals('blocks[biClassYaml].followingLine', 12, cg.blocks[biClassYaml].followingLine);

    AssertNotNull('blocks[biMemberVars]', cg.blocks[biMemberVars]);
    AssertEquals('blocks[biMemberVars].startLine', 19, cg.blocks[biMemberVars].startLine);
    AssertEquals('blocks[biMemberVars].followingLine', 19, cg.blocks[biMemberVars].followingLine);

    AssertNotNull('blocks[biGetSetDeclarations]', cg.blocks[biGetSetDeclarations]);
    AssertEquals('blocks[biGetSetDeclarations].startLine', 27,
      cg.blocks[biGetSetDeclarations].startLine);
    AssertEquals('blocks[biGetSetDeclarations].followingLine', 27,
      cg.blocks[biGetSetDeclarations].followingLine);

    AssertNotNull('blocks[biPublicDeclarations]', cg.blocks[biPublicDeclarations]);
    AssertEquals('blocks[biPublicDeclarations].startLine', 34,
      cg.blocks[biPublicDeclarations].startLine);
    AssertEquals('blocks[biPublicDeclarations].followingLine', 34,
      cg.blocks[biPublicDeclarations].followingLine);

    AssertNotNull('blocks[biProperty]', cg.blocks[biProperty]);
    AssertEquals('blocks[biProperty].startLine', 40, cg.blocks[biProperty].startLine);
    AssertEquals('blocks[biProperty].followingLine', 40, cg.blocks[biProperty].followingLine);

    AssertNotNull('blocks[biCreate]', cg.blocks[biCreate]);
    AssertEquals('blocks[biCreate].startLine', 59, cg.blocks[biCreate].startLine);
    AssertEquals('blocks[biCreate].followingLine', 59, cg.blocks[biCreate].followingLine);

    AssertNotNull('blocks[biDestroy]', cg.blocks[biDestroy]);
    AssertEquals('blocks[biDestroy].startLine', 65, cg.blocks[biDestroy].startLine);
    AssertEquals('blocks[biDestroy].followingLine', 65, cg.blocks[biDestroy].followingLine);

    AssertNotNull('blocks[biRestoreYamlVars]', cg.blocks[biRestoreYamlVars]);
    AssertEquals('blocks[biRestoreYamlVars].startLine', 72,
      cg.blocks[biRestoreYamlVars].startLine);
    AssertEquals('blocks[biRestoreYamlVars].followingLine', 72,
      cg.blocks[biRestoreYamlVars].followingLine);

    AssertNotNull('blocks[biRestoreVars]', cg.blocks[biRestoreVars]);
    AssertEquals('blocks[biRestoreVars].startLine', 81, cg.blocks[biRestoreVars].startLine);
    AssertEquals('blocks[biRestoreVars].followingLine', 81,
      cg.blocks[biRestoreVars].followingLine);

    AssertNotNull('blocks[biSaveVars]', cg.blocks[biSaveVars]);
    AssertEquals('blocks[biSaveVars].startLine', 89, cg.blocks[biSaveVars].startLine);
    AssertEquals('blocks[biSaveVars].followingLine', 89, cg.blocks[biSaveVars].followingLine);

    AssertNotNull('blocks[biSaveYamlVars]', cg.blocks[biSaveYamlVars]);
    AssertEquals('blocks[biSaveYamlVars].startLine', 97, cg.blocks[biSaveYamlVars].startLine);
    AssertEquals('blocks[biSaveYamlVars].followingLine', 97,
      cg.blocks[biSaveYamlVars].followingLine);

    AssertNotNull('blocks[biGetSetMethods]', cg.blocks[biGetSetMethods]);
    AssertEquals('blocks[biGetSetMethods].startLine', 101, cg.blocks[biGetSetMethods].startLine);
    AssertEquals('blocks[biGetSetMethods].followingLine', 101,
      cg.blocks[biGetSetMethods].followingLine);

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
    AssertEquals('#0', False, cg.attribute[0].isCollection);
    AssertEquals('#1', False, cg.attribute[1].isCollection);
    AssertEquals('#2', False, cg.attribute[2].isCollection);
  finally
    cg.Free;
  end;

end;

procedure TTestClassRegenerator.TestParseYamlCollectionOperations;
var
  cg: TTestableClassRegenerator;
begin
  // Given sample input with attribute containing a sequence of values
  // When ParseYaml is called
  // Then all the attributes are created with the values populated

  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collection_operations.pas');
  try
    cg.LoadInput;
    cg.ParseInput;

    // When...
    cg.ParseYaml;

    // Then...
    AssertEquals(3, cg.attributeCount);
    AssertEquals('#0', True, cg.attribute[0].isCollection);
    AssertEquals('#1', True, cg.attribute[1].isCollection);
    AssertEquals('#2', True, cg.attribute[2].isCollection);

    AssertTrue(opAdd in cg.attribute[0].operations);
    AssertTrue(opDelete in cg.attribute[0].operations);
    AssertTrue(opClear in cg.attribute[0].operations);

    AssertTrue(opAdd in cg.attribute[1].operations);
    AssertFalse(opDelete in cg.attribute[1].operations);
    AssertFalse(opClear in cg.attribute[1].operations);

    AssertTrue(opAdd in cg.attribute[2].operations);
    AssertFalse(opDelete in cg.attribute[2].operations);
    AssertTrue(opClear in cg.attribute[2].operations);
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
const
  expected: TArray<string> = [
    '    procedure SetTom(const AValue: Integer);',
    '    procedure SetDick(const AValue: Double);',
    '    procedure SetHarry(const AValue: String);'
    ];
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
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGeneratePropertyDeclarations;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    property tom: Integer read FTom write SetTom;',
    '    property dick: Double read FDick write SetDick;',
    '',
    '    // A single line comment',
    '    property harry: String read FHarry write SetHarry;'
    ];
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
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreYamlVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  if AName = ''tom'' then',
    '    FTom := StrToInteger(AValue)',
    '  else',
    '  if AName = ''dick'' then',
    '    FDick := StrToDouble(AValue)',
    '  else',
    '  if AName = ''harry'' then',
    '    FHarry := StrToString(AValue)',
    '  else'
    ];
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
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  AStream.ReadBuffer(FTom, sizeof(Integer));',
    '  AStream.ReadBuffer(FDick, sizeof(Double));',
    '  FHarry := AStream.ReadAnsiString;'
    ];
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
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  AStream.WriteBuffer(FTom, sizeof(Integer));',
    '  AStream.WriteBuffer(FDick, sizeof(Double));',
    '  AStream.WriteAnsiString(FHarry);'
    ];
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
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveYamlVars;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  SaveYamlInteger(AEmitter, ''tom'', FTom);',
    '  SaveYamlDouble(AEmitter, ''dick'', FDick);',
    '  SaveYamlString(AEmitter, ''harry'', FHarry);'
    ];
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
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetMethods;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetTom(const AValue: Integer);',
    'begin',
    '  if AValue <> FTom then begin',
    '    SetModified;',
    '    FTom := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetDick(const AValue: Double);',
    'begin',
    '  if AValue <> FDick then begin',
    '    SetModified;',
    '    FDick := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetHarry(const AValue: String);',
    'begin',
    '  if AValue <> FHarry then begin',
    '    SetModified;',
    '    FHarry := AValue;',
    '  end;',
    'end;',
    ''
    ];
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
    AssertEquals(expected, s);
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
    AssertTrue('sample.pas matches expected data?', FileCompare('sample.pas',
      'testdata/sample.pas'));
  finally
    cg.Free;
  end;
end;

procedure TTestClassRegenerator.TestParseYamlCollections;
var
  cg: TTestableClassRegenerator;
begin
  // Given a regen object with sample input containing collections loaded and Parsed
  // When ParseYaml is called
  // Then all the attributes are created
  // and  attribute properties are set

  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;

    // When...
    cg.ParseYaml;

    // Then...
    AssertEquals(3, cg.attributeCount);

    AssertEquals('#0', True, cg.attribute[0].isCollection);
    AssertEquals('#0', True, cg.attribute[0].isDynamic);
    AssertEquals('#0', 'integer', cg.attribute[0].indexType);
    AssertEquals('#1', True, cg.attribute[1].isCollection);
    AssertEquals('#1', False, cg.attribute[1].isDynamic);
    AssertEquals('#1', '0..maxCount', cg.attribute[1].bounds);
    AssertEquals('#1', 'integer', cg.attribute[1].indexType);
    AssertEquals('#2', True, cg.attribute[2].isCollection);
    AssertEquals('#2', False, cg.attribute[2].isDynamic);
    AssertEquals('#2', 'ESpecialEnum', cg.attribute[2].bounds);
    AssertEquals('#2', 'ESpecialEnum', cg.attribute[2].indexType);
  finally
    cg.Free;
  end;

end;

procedure TTestClassRegenerator.TestGenerateMemberVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    FTom: array of Integer;',
    '    FDick: array[0..maxCount] of Double;',
    '    FHarry: array[ESpecialEnum] of String;'
    ];
begin
  // Given a regen object with collection attributes
  // When GenerateMemberVars is called
  // Then the expected textis returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateMemberVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetDeclarationsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    function GetTom(AIndex: Integer): Integer;',
    '    function GetTomCount: Integer;',
    '    function GetDick(AIndex: Integer): Double;',
    '    function GetHarry(AIndex: ESpecialEnum): String;',
    '    procedure SetTom(AIndex: Integer; const AValue: Integer);',
    '    procedure SetDick(AIndex: Integer; const AValue: Double);',
    '    procedure SetHarry(AIndex: ESpecialEnum; const AValue: String);'
    ];
begin
  // Given a regen object with collection attributes
  // When GenerateGetSetDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetDeclarations;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGeneratePropertyDeclarationsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '',
    '    // A single line comment',
    '    property tom[AIndex: Integer]: Integer read GetTom write SetTom;',
    '    property tomCount: Integer read GetTomCount;',
    '',
    '    // A multi-line comment',
    '    // about Dick!',
    '    // (not necessarily very complimentary)',
    '    property dick[AIndex: Integer]: Double read GetDick write SetDick;',
    '    property harry[AIndex: ESpecialEnum]: String read GetHarry write SetHarry;'
    ];
begin
  // Given a regen object with collection attributes
  // When GeneratePropertyDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GeneratePropertyDeclarations;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreYamlVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  if AName = ''tom-length'' then',
    '    SetLength(FTom, StrToInteger(AValue))',
    '  else',
    '  if AName = ''tom'' then',
    '    FTom[Integer(Ord(Low(FTom))+AIndex)] := StrToInteger(AValue)',
    '  else',
    '  if AName = ''dick'' then',
    '    FDick[Integer(Ord(Low(FDick))+AIndex)] := StrToDouble(AValue)',
    '  else',
    '  if AName = ''harry'' then',
    '    FHarry[ESpecialEnum(Ord(Low(FHarry))+AIndex)] := StrToString(AValue)',
    '  else'
    ];
begin
  // Given a regen object with collection attributes
  // When GenerateRestoreYamlVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateRestoreYamlVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  SetLength(FTom, AStream.ReadDWord);',
    '  AStream.ReadBuffer(FTom[Low(FTom)], (Ord(High(FTom))-Ord(Low(FTom)) + 1)*sizeof(Integer));',
    '  AStream.ReadBuffer(FDick[Low(FDick)], (Ord(High(FDick))-Ord(Low(FDick)) + 1)*sizeof(Double));',
    '  for i := Ord(Low(FHarry)) to Ord(High(FHarry)) do',
    '    FHarry[ESpecialEnum(i)] := AStream.ReadAnsiString;'
    ];
begin
  // Given a regen object with collection attributes
  // When GenerateRestoreVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateRestoreVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  AStream.WriteDWord(Length(FTom));',
    '  AStream.WriteBuffer(FTom[Low(FTom)], (Ord(High(FTom))-Ord(Low(FTom)) + 1)*sizeof(Integer));',
    '  AStream.WriteBuffer(FDick[Low(FDick)], (Ord(High(FDick))-Ord(Low(FDick)) + 1)*sizeof(Double));',
    '  for i := Ord(Low(FHarry)) to Ord(High(FHarry)) do',
    '    AStream.WriteAnsiString(FHarry[ESpecialEnum(i)]);'
    ];
begin
  // Given a regen object with collection attributes
  // When GenerateSaveVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateSaveVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveYamlVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  SaveYamlInteger(AEmitter, ''tom-length'', Length(FTom));',
    '  SaveYamlSequence(AEmitter, ''tom'');',
    '  for i := Ord(Low(FTom)) to Ord(High(FTom)) do',
    '    SaveYamlSequenceInteger(AEmitter, FTom[Integer(i)]);',
    '  SaveYamlEndSequence(AEmitter);',
    '  SaveYamlSequence(AEmitter, ''dick'');',
    '  for i := Ord(Low(FDick)) to Ord(High(FDick)) do',
    '    SaveYamlSequenceDouble(AEmitter, FDick[Integer(i)]);',
    '  SaveYamlEndSequence(AEmitter);',
    '  SaveYamlSequence(AEmitter, ''harry'');',
    '  for i := Ord(Low(FHarry)) to Ord(High(FHarry)) do',
    '    SaveYamlSequenceString(AEmitter, FHarry[ESpecialEnum(i)]);',
    '  SaveYamlEndSequence(AEmitter);'
    ];
begin
  // Given a regen object with defined attributes
  // When GenerateSaveYamlVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateSaveYamlVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetMethodsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetTom(AIndex: Integer): Integer;',
    'begin',
    '  Result := FTom[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetTom(AIndex: Integer; const AValue: Integer);',
    'begin',
    '  if AValue <> FTom[AIndex] then begin',
    '    SetModified;',
    '    FTom[AIndex] := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetTomCount: Integer;',
    'begin',
    '  Result := Length(FTom);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetDick(AIndex: Integer): Double;',
    'begin',
    '  Result := FDick[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetDick(AIndex: Integer; const AValue: Double);',
    'begin',
    '  if AValue <> FDick[AIndex] then begin',
    '    SetModified;',
    '    FDick[AIndex] := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetHarry(AIndex: ESpecialEnum): String;',
    'begin',
    '  Result := FHarry[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetHarry(AIndex: ESpecialEnum; const AValue: String);',
    'begin',
    '  if AValue <> FHarry[AIndex] then begin',
    '    SetModified;',
    '    FHarry[AIndex] := AValue;',
    '  end;',
    'end;',
    ''
    ];
begin
  // Given a regen object with collection attributes
  // When GenerateGetSetMethods is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collections.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetMethods;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGeneratePublicDeclarations;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    function AddTom(AValue: Integer): Integer;',
    '    procedure DeleteTom(AIndex: Integer);',
    '    procedure ClearTom;',
    '    function AddDick(AValue: Double): Integer;',
    '    function AddHarry(AValue: String): Integer;',
    '    procedure ClearHarry;'
    ];
begin
  // Given a regen object with collection attributes
  // When GeneratePropertyDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collection_operations.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GeneratePublicDeclarations;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetMethodsCollectionOperations;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetTom(AIndex: Integer): Integer;',
    'begin',
    '  Result := FTom[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetTom(AIndex: Integer; const AValue: Integer);',
    'begin',
    '  if AValue <> FTom[AIndex] then begin',
    '    SetModified;',
    '    FTom[AIndex] := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetTomCount: Integer;',
    'begin',
    '  Result := Length(FTom);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.AddTom(AValue: Integer): Integer;',
    'begin',
    '  SetModified;',
    '  SetLength(FTom, Length(FTom) + 1);',
    '  FTom[High(FTom)] := AValue;',
    '  Result := High(FTom);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.DeleteTom(AIndex: Integer);',
    'begin',
    '  SetModified;',
    '  Delete(FTom, AIndex, 1);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.ClearTom;',
    'begin',
    '  SetModified;',
    '  SetLength(FTom, 0);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetDick(AIndex: Integer): Double;',
    'begin',
    '  Result := FDick[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetDick(AIndex: Integer; const AValue: Double);',
    'begin',
    '  if AValue <> FDick[AIndex] then begin',
    '    SetModified;',
    '    FDick[AIndex] := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetDickCount: Integer;',
    'begin',
    '  Result := Length(FDick);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.AddDick(AValue: Double): Integer;',
    'begin',
    '  SetModified;',
    '  SetLength(FDick, Length(FDick) + 1);',
    '  FDick[High(FDick)] := AValue;',
    '  Result := High(FDick);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetHarry(AIndex: Integer): String;',
    'begin',
    '  Result := FHarry[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetHarry(AIndex: Integer; const AValue: String);',
    'begin',
    '  if AValue <> FHarry[AIndex] then begin',
    '    SetModified;',
    '    FHarry[AIndex] := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetHarryCount: Integer;',
    'begin',
    '  Result := Length(FHarry);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.AddHarry(AValue: String): Integer;',
    'begin',
    '  SetModified;',
    '  SetLength(FHarry, Length(FHarry) + 1);',
    '  FHarry[High(FHarry)] := AValue;',
    '  Result := High(FHarry);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.ClearHarry;',
    'begin',
    '  SetModified;',
    '  SetLength(FHarry, 0);',
    'end;',
    ''
    ];
begin
  // Given a regen object with collection attributes
  // When GenerateGetSetMethods is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_collection_operations.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetMethods;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetDeclarationsAccessControl;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    function GetTom(AIndex: Integer): Integer;',
    '    function GetTomCount: Integer;',
    '    function GetHarry(AIndex: ESpecialEnum): String;',
    '    procedure SetDick(AIndex: Integer; const AValue: Double);',
    '    procedure SetHarry(AIndex: ESpecialEnum; const AValue: String);'
    ];
begin
  // Given a regen object with access-controlled attributes
  // When GenerateGetSetDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_accesscontrol.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetDeclarations;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGeneratePropertyDeclarationsAccessControl;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    property tom[AIndex: Integer]: Integer read GetTom;',
    '    property tomCount: Integer read GetTomCount;',
    '    property dick[AIndex: Integer]: Double write SetDick;',
    '    property harry[AIndex: ESpecialEnum]: String read GetHarry write SetHarry;'
    ];
begin
  // Given a regen object with access control attributes
  // When GeneratePropertyDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_accesscontrol.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GeneratePropertyDeclarations;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetMethodsAccessControl;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetTom(AIndex: Integer): Integer;',
    'begin',
    '  Result := FTom[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetTomCount: Integer;',
    'begin',
    '  Result := Length(FTom);',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetDick(AIndex: Integer; const AValue: Double);',
    'begin',
    '  if AValue <> FDick[AIndex] then begin',
    '    SetModified;',
    '    FDick[AIndex] := AValue;',
    '  end;',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetHarry(AIndex: ESpecialEnum): String;',
    'begin',
    '  Result := FHarry[AIndex];',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetHarry(AIndex: ESpecialEnum; const AValue: String);',
    'begin',
    '  if AValue <> FHarry[AIndex] then begin',
    '    SetModified;',
    '    FHarry[AIndex] := AValue;',
    '  end;',
    'end;',
    ''
    ];
begin
  // Given a regen object with access control attributes
  // When GenerateGetSetMethods is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_accesscontrol.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetMethods;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateMemberVarsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    FTom: TOID;',
    '    FDick: TOID;'
    ];
begin
  // Given a regen object with owning/ref attributes
  // When GenerateMemberVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateMemberVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetDeclarationsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    function GetTom: TTomOwningList;',
    '    function GetDick: TOther;',
    '    procedure SetDick(const AValue: TOther);'
    ];
begin
  // Given a regen object with owns attributes
  // When GenerateGetSetDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetDeclarations;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGeneratePropertyDeclarationsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '    property tom: TTomOwningList read GetTom;',
    '    property dick: TOther read GetDick write SetDick;'
    ];
begin
  // Given a regen object with owns attributes
  // When GeneratePropertyDeclarations is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GeneratePropertyDeclarations;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreYamlVarsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  if AName = ''tom'' then',
    '    RestoreYamlObjectOwn(FTom, StrToInteger(AValue))',
    '  else',
    '  if AName = ''dick'' then',
    '    RestoreYamlObjectRef(FDick, StrToInteger(AValue))',
    '  else'
    ];
begin
  // Given a regen object with owns attributes
  // When GenerateRestoreYamlVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateRestoreYamlVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreVarsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  AStream.ReadBuffer(FTom, sizeof(TOID));',
    '  AStream.ReadBuffer(FDick, sizeof(TOID));'
    ];
begin
  // Given a regen object with owned attributes
  // When GenerateRestoreVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateRestoreVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveVarsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  AStream.WriteBuffer(FTom, sizeof(TOID));',
    '  AStream.WriteBuffer(FDick, sizeof(TOID));'
    ];
begin
  // Given a regen object with owned attributes
  // When GenerateSaveVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateSaveVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveYamlVarsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  SaveYamlObject(AEmitter, ''tom'', FTom);',
    '  SaveYamlObjectReference(AEmitter, ''dick'', FDick);'
    ];
begin
  // Given a regen object with owns attributes
  // When GenerateSaveYamlVars is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateSaveYamlVars;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateCreateOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  FTom := 0;',
    '  FDick := 0;'
    ];
begin
  // Given a regen object with owns attributes
  // When GenerateCreate is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateCreate;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateDestroyOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '  SetOwned(FTom, nil);',
    '  SetReference(FDick, nil);'
    ];
begin
  // Given a regen object with owns attributes
  // When GenerateDestroy is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateDestroy;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetMethodsOwns;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
const
  expected: TArray<string> = [
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetTom: TTomOwningList;',
    'begin',
    '  Result := TTomOwningList(FromOID(FTom));',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'function TSample.GetDick: TOther;',
    'begin',
    '  Result := TOther(FromOID(FDick));',
    'end;',
    '',
    '// GENERATED METHOD - DO NOT EDIT',
    'procedure TSample.SetDick(const AValue: TOther);',
    'begin',
    '  SetReference(FDick, AValue);',
    'end;',
    ''
    ];
begin
  // Given a regen object with owned attributes
  // When GenerateGetSetMethods is called
  // Then the expected text is returned

  s := nil;
  cg := TTestableClassRegenerator.Create('testdata/testparseyaml_owns.pas');
  try
    cg.LoadInput;
    cg.ParseInput;
    cg.ParseYaml;

    // When...
    s := cg.GenerateGetSetMethods;

    // Then
    AssertEquals(expected, s);
  finally
    cg.Free;
    s.Free;
  end;
end;

initialization
  RegisterTest(TTestClassRegenerator);

end.
