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

    procedure TestParseYamlCollections;
    procedure TestGenerateMemberVarsCollections;
    procedure TestGenerateGetSetDeclarationsCollections;
    procedure TestGeneratePropertyDeclarationsCollections;
    procedure TestGenerateRestoreYamlVarsCollections;
    procedure TestGenerateRestoreVarsCollections;
    procedure TestGenerateSaveVarsCollections;
    procedure TestGenerateSaveYamlVarsCollections;
    procedure TestGenerateGetSetMethodsCollections;


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
     AssertEquals( 88, cg.input.Count );

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
     AssertEquals('blocks[biGetSetDeclarations].startLine', 27, cg.blocks[biGetSetDeclarations].startLine);
     AssertEquals('blocks[biGetSetDeclarations].followingLine', 27, cg.blocks[biGetSetDeclarations].followingLine);

     AssertNotNull('blocks[biProperty]', cg.blocks[biProperty]);
     AssertEquals('blocks[biProperty].startLine', 34, cg.blocks[biProperty].startLine);
     AssertEquals('blocks[biProperty].followingLine', 34, cg.blocks[biProperty].followingLine);

     AssertNotNull('blocks[biRestoreYamlVars]', cg.blocks[biRestoreYamlVars]);
     AssertEquals('blocks[biRestoreYamlVars].startLine', 52, cg.blocks[biRestoreYamlVars].startLine);
     AssertEquals('blocks[biRestoreYamlVars].followingLine', 52, cg.blocks[biRestoreYamlVars].followingLine);

     AssertNotNull('blocks[biRestoreVars]', cg.blocks[biRestoreVars]);
     AssertEquals('blocks[biRestoreVars].startLine', 61, cg.blocks[biRestoreVars].startLine);
     AssertEquals('blocks[biRestoreVars].followingLine', 61, cg.blocks[biRestoreVars].followingLine);

     AssertNotNull('blocks[biSaveVars]', cg.blocks[biSaveVars]);
     AssertEquals('blocks[biSaveVars].startLine', 69, cg.blocks[biSaveVars].startLine);
     AssertEquals('blocks[biSaveVars].followingLine', 69, cg.blocks[biSaveVars].followingLine);

     AssertNotNull('blocks[biSaveYamlVars]', cg.blocks[biSaveYamlVars]);
     AssertEquals('blocks[biSaveYamlVars].startLine', 77, cg.blocks[biSaveYamlVars].startLine);
     AssertEquals('blocks[biSaveYamlVars].followingLine', 77, cg.blocks[biSaveYamlVars].followingLine);

     AssertNotNull('blocks[biGetSetMethods]', cg.blocks[biGetSetMethods]);
     AssertEquals('blocks[biGetSetMethods].startLine', 81, cg.blocks[biGetSetMethods].startLine);
     AssertEquals('blocks[biGetSetMethods].followingLine', 81, cg.blocks[biGetSetMethods].followingLine);

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
     AssertEquals('#0', false, cg.attribute[0].isCollection);
     AssertEquals('#1', false, cg.attribute[1].isCollection);
     AssertEquals('#2', false, cg.attribute[2].isCollection);
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
    AssertEquals(5, s.Count);
    AssertEquals('    property tom: Integer read FTom write SetTom;', s[0]);
    AssertEquals('    property dick: Double read FDick write SetDick;', s[1]);
    AssertEquals('', s[2]);
    AssertEquals('    // A single line comment', s[3]);
    AssertEquals('    property harry: String read FHarry write SetHarry;', s[4]);
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
    AssertEquals('  AStream.ReadBuffer(FTom, sizeof(Integer));', s[0]);
    AssertEquals('  AStream.ReadBuffer(FDick, sizeof(Double));', s[1]);
    AssertEquals('  FHarry := AStream.ReadAnsiString;', s[2]);
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
    AssertEquals('  AStream.WriteBuffer(FTom, sizeof(Integer));', s[0]);
    AssertEquals('  AStream.WriteBuffer(FDick, sizeof(Double));', s[1]);
    AssertEquals('  AStream.WriteAnsiString(FHarry);', s[2]);
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

     AssertEquals('#0', true, cg.attribute[0].isCollection);
     AssertEquals('#0', true, cg.attribute[0].isDynamic);
     AssertEquals('#0', 'integer', cg.attribute[0].indexType);
     AssertEquals('#1', true, cg.attribute[1].isCollection);
     AssertEquals('#1', false, cg.attribute[1].isDynamic);
     AssertEquals('#1', '0..maxCount', cg.attribute[1].bounds);
     AssertEquals('#1', 'integer', cg.attribute[1].indexType);
     AssertEquals('#2', true, cg.attribute[2].isCollection);
     AssertEquals('#2', false, cg.attribute[2].isDynamic);
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
    AssertEquals(3, s.Count);
    AssertEquals('    FTom: array of Integer;', s[0]);
    AssertEquals('    FDick: array[0..maxCount] of Double;', s[1]);
    AssertEquals('    FHarry: array[ESpecialEnum] of String;', s[2]);
  finally
    cg.Free;
    s.Free;
  end;
end;

  procedure TTestClassRegenerator.TestGenerateGetSetDeclarationsCollections;
  var
    cg: TTestableClassRegenerator;
    s: TStringList;
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
      AssertEquals(6, s.Count);
      AssertEquals('    function GetTom(AIndex: Integer): Integer;', s[0]);
      AssertEquals('    function GetDick(AIndex: Integer): Double;', s[1]);
      AssertEquals('    function GetHarry(AIndex: ESpecialEnum): String;', s[2]);
      AssertEquals('    procedure SetTom(AIndex: Integer; const AValue: Integer);', s[3]);
      AssertEquals('    procedure SetDick(AIndex: Integer; const AValue: Double);', s[4]);
      AssertEquals('    procedure SetHarry(AIndex: ESpecialEnum; const AValue: String);', s[5]);
    finally
      cg.Free;
      s.Free;
    end;
  end;

procedure TTestClassRegenerator.TestGeneratePropertyDeclarationsCollections;
  var
    cg: TTestableClassRegenerator;
    s: TStringList;
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
      AssertEquals(9, s.Count);
      AssertEquals('', s[0]);
      AssertEquals('    // A single line comment', s[1]);
      AssertEquals('    property tom[AIndex: Integer]: Integer read GetTom write SetTom;', s[2]);
      AssertEquals('', s[3]);
      AssertEquals('    // A multi-line comment', s[4]);
      AssertEquals('    // about Dick!', s[5]);
      AssertEquals('    // (not necessarily very complimentary)', s[6]);
      AssertEquals('    property dick[AIndex: Integer]: Double read GetDick write SetDick;', s[7]);
      AssertEquals('    property harry[AIndex: ESpecialEnum]: String read GetHarry write SetHarry;', s[8]);
    finally
      cg.Free;
      s.Free;
    end;
  end;

procedure TTestClassRegenerator.TestGenerateRestoreYamlVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
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
    AssertEquals(12, s.Count);
    AssertEquals('  if AName = ''tom-length'' then', s[0]);
    AssertEquals('    SetLength(FTom, StrToInteger(AValue))', s[1]);
    AssertEquals('  else', s[2]);
    AssertEquals('  if AName = ''tom'' then', s[3]);
    AssertEquals('    FTom[Integer(Ord(Low(FTom))+AIndex)] := StrToInteger(AValue)', s[4]);
    AssertEquals('  else', s[5]);
    AssertEquals('  if AName = ''dick'' then', s[6]);
    AssertEquals('    FDick[Integer(Ord(Low(FDick))+AIndex)] := StrToDouble(AValue)', s[7]);
    AssertEquals('  else', s[8]);
    AssertEquals('  if AName = ''harry'' then', s[9]);
    AssertEquals('    FHarry[ESpecialEnum(Ord(Low(FHarry))+AIndex)] := StrToString(AValue)', s[10]);
    AssertEquals('  else', s[11]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateRestoreVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
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
    AssertEquals(5, s.Count);
    AssertEquals('  SetLength(FTom, AStream.ReadDWord);', s[0]);
    AssertEquals('  AStream.ReadBuffer(FTom[Low(FTom)], (Ord(High(FTom))-Ord(Low(FTom)) + 1)*sizeof(Integer));', s[1]);
    AssertEquals('  AStream.ReadBuffer(FDick[Low(FDick)], (Ord(High(FDick))-Ord(Low(FDick)) + 1)*sizeof(Double));', s[2]);
    AssertEquals('  for i := Ord(Low(FHarry)) to Ord(High(FHarry)) do', s[3]);
    AssertEquals('    FHarry[ESpecialEnum(i)] := AStream.ReadAnsiString;', s[4]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
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
    AssertEquals(5, s.Count);
    AssertEquals('  AStream.WriteDWord(Length(FTom));', s[0]);
    AssertEquals('  AStream.WriteBuffer(FTom[Low(FTom)], (Ord(High(FTom))-Ord(Low(FTom)) + 1)*sizeof(Integer));', s[1]);
    AssertEquals('  AStream.WriteBuffer(FDick[Low(FDick)], (Ord(High(FDick))-Ord(Low(FDick)) + 1)*sizeof(Double));', s[2]);
    AssertEquals('  for i := Ord(Low(FHarry)) to Ord(High(FHarry)) do', s[3]);
    AssertEquals('    AStream.WriteAnsiString(FHarry[ESpecialEnum(i)]);', s[4]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateSaveYamlVarsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
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
    AssertEquals(13, s.Count);
    AssertEquals('  SaveYamlInteger(AEmitter, ''tom-length'', Length(FTom));', s[0]);
    AssertEquals('  SaveYamlSequence(AEmitter, ''tom'');', s[1]);
    AssertEquals('  for i := Ord(Low(FTom)) to Ord(High(FTom)) do', s[2]);
    AssertEquals('    SaveYamlSequenceInteger(AEmitter, FTom[Integer(i)]);', s[3]);
    AssertEquals('  SaveYamlEndSequence(AEmitter);', s[4]);
    AssertEquals('  SaveYamlSequence(AEmitter, ''dick'');', s[5]);
    AssertEquals('  for i := Ord(Low(FDick)) to Ord(High(FDick)) do', s[6]);
    AssertEquals('    SaveYamlSequenceDouble(AEmitter, FDick[Integer(i)]);', s[7]);
    AssertEquals('  SaveYamlEndSequence(AEmitter);', s[8]);
    AssertEquals('  SaveYamlSequence(AEmitter, ''harry'');', s[9]);
    AssertEquals('  for i := Ord(Low(FHarry)) to Ord(High(FHarry)) do', s[10]);
    AssertEquals('    SaveYamlSequenceString(AEmitter, FHarry[ESpecialEnum(i)]);', s[11]);
    AssertEquals('  SaveYamlEndSequence(AEmitter);', s[12]);
  finally
    cg.Free;
    s.Free;
  end;
end;

procedure TTestClassRegenerator.TestGenerateGetSetMethodsCollections;
var
  cg: TTestableClassRegenerator;
  s: TStringList;
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
    AssertEquals(45, s.Count);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[0]);
    AssertEquals('function TSample.GetTom(AIndex: Integer): Integer;', s[1]);
    AssertEquals('begin', s[2]);
    AssertEquals('  Result := FTom[AIndex];', s[3]);
    AssertEquals('end;', s[4]);
    AssertEquals('', s[5]);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[6]);
    AssertEquals('function TSample.GetDick(AIndex: Integer): Double;', s[7]);
    AssertEquals('begin', s[8]);
    AssertEquals('  Result := FDick[AIndex];', s[9]);
    AssertEquals('end;', s[10]);
    AssertEquals('', s[11]);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[12]);
    AssertEquals('function TSample.GetHarry(AIndex: ESpecialEnum): String;', s[13]);
    AssertEquals('begin', s[14]);
    AssertEquals('  Result := FHarry[AIndex];', s[15]);
    AssertEquals('end;', s[16]);
    AssertEquals('', s[17]);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[18]);
    AssertEquals('procedure TSample.SetTom(AIndex: Integer; const AValue: Integer);', s[19]);
    AssertEquals('begin', s[20]);
    AssertEquals('  if AValue <> FTom[AIndex] then begin', s[21]);
    AssertEquals('    SetModified;', s[22]);
    AssertEquals('    FTom[AIndex] := AValue;', s[23]);
    AssertEquals('  end;', s[24]);
    AssertEquals('end;', s[25]);
    AssertEquals('', s[26]);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[27]);
    AssertEquals('procedure TSample.SetDick(AIndex: Integer; const AValue: Double);', s[28]);
    AssertEquals('begin', s[29]);
    AssertEquals('  if AValue <> FDick[AIndex] then begin', s[30]);
    AssertEquals('    SetModified;', s[31]);
    AssertEquals('    FDick[AIndex] := AValue;', s[32]);
    AssertEquals('  end;', s[33]);
    AssertEquals('end;', s[34]);
    AssertEquals('', s[35]);
    AssertEquals('// GENERATED METHOD - DO NOT EDIT', s[36]);
    AssertEquals('procedure TSample.SetHarry(AIndex: ESpecialEnum; const AValue: String);', s[37]);
    AssertEquals('begin', s[38]);
    AssertEquals('  if AValue <> FHarry[AIndex] then begin', s[39]);
    AssertEquals('    SetModified;', s[40]);
    AssertEquals('    FHarry[AIndex] := AValue;', s[41]);
    AssertEquals('  end;', s[42]);
    AssertEquals('end;', s[43]);
    AssertEquals('', s[44]);
  finally
    cg.Free;
    s.Free;
  end;
end;



initialization
  RegisterTest(TTestClassRegenerator);

end.
