unit OTClassRegenerator;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type

  TBlock = class
  private
    FStartLine: Integer;
    FFollowingLine: Integer;

  public
    constructor Create(AStartLine, AFollowingLine: Integer);

    property startLine: Integer Read FStartLine;
    property followingLine: Integer Read FFollowingLine;
  end;

  EBlockIdentifier = (
    biClassYaml,
    biMemberVars,
    biCollections,
    biGetSetDeclarations,
    biProperty,
    biRestoreYamlVars,
    biRestoreVars,
    biSaveVars,
    biSaveYamlVars,
    biGetSetMethods
  );

  TAttribute = class
    private
      FName: string;
      FTypeName: string;
    public
      constructor Create;

      property name: string read FName write FName;
      property typeName: string read FTypeName write FTypeName;

  end;

  { TOTClassRegenerator }

  TOTClassRegenerator = class
  private
    FFileName: string;
    FBackupFileName: string;
    FInput: TStringList;
    FBlocks: array[EBlockIdentifier] of TBlock;
    FAttributes: TObjectList<TAttribute>;
    FClassName: String;

    function GetBlock(bi: EBlockIdentifier): TBlock;
    function GetAttribute(i: Integer): TAttribute;
    function GetAttributeCount: Integer;

  protected
    // protected to make accessible for unit tests
    procedure LoadInput;
    procedure ParseInput;
    procedure ParseYaml;
    procedure Regenerate;

    function GenerateMemberVars: TStringList;
    function GenerateGetSetDeclarations: TStringList;
    function GeneratePropertyDeclarations: TStringList;
    function GenerateRestoreYamlVars: TStringList;
    function GenerateRestoreVars: TStringList;
    function GenerateSaveVars: TStringList;
    function GenerateSaveYamlVars: TStringList;
    function GenerateGetSetMethods: TStringList;

  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure Generate;

    property input: TStringList Read FInput;
    property blocks[bi: EBlockIdentifier]: TBlock read GetBlock;
    property attribute[i: Integer]: TAttribute read GetAttribute;
    property attributeCount: Integer read GetAttributeCount;
    property className: String read FClassName;
  end;

implementation

uses
  LazFileUtils,
  OTYamlParser,
  OTYamlEvent,
  OTTemplateGenerator;

const
  BLOCK_LABELS: array[EBlockIdentifier] of string = (
    '',
    'MemberVars',
    'Collections',
    'GetSetDeclarations',
    'Property',
    'RestoreYamlVars',
    'RestoreVars',
    'SaveVars',
    'SaveYamlVars',
    'GetSetMethods'
  );

function MatchBlockName(AName: string): EBlockIdentifier;
var
  bi: EBlockIdentifier;
begin
  for bi in EBlockIdentifier do begin
    if BLOCK_LABELS[bi] = AName then
      Exit(bi);
  end;
  raise Exception.CreateFmt('Unknown block name: %s', [AName]);
end;

{ TBlock}

constructor TBlock.Create(AStartLine, AFollowingLine: Integer);
begin
  inherited Create;

  FStartLine := AStartLine;
  FFollowingLine := AFollowingLine;
end;

{ TAttribute }

constructor TAttribute.Create;
begin
  inherited Create;
end;

{ TOTClassRegenerator }

constructor TOTClassRegenerator.Create(const AFileName: string);
var
  blockId: EBlockIdentifier;
begin
  inherited Create;

  FFileName := AFileName;
  FBackupFileName := ExtractFileNameWithoutExt(AFileName) + '.bak';

  for blockId in EBlockIdentifier do begin
    FBlocks[blockId] := nil;
  end;

  FAttributes := TObjectList<TAttribute>.Create;
end;

destructor TOTClassRegenerator.Destroy;
var
  bi: EBlockIdentifier;
begin
  FInput.Free;

  for bi in EBlockIdentifier do
    FBlocks[bi].Free;

  FAttributes.Free;

  inherited;
end;

function TOTClassRegenerator.GetBlock(bi: EBlockIdentifier): TBlock;
begin
  Result := FBlocks[bi];
end;

function TOTClassRegenerator.GetAttribute(i: Integer): TAttribute;
begin
  Result := FAttributes[i];
end;

function TOTClassRegenerator.GetAttributeCount: Integer;
begin
  Result := FAttributes.Count;
end;

procedure TOTClassRegenerator.Generate;
begin
  LoadInput;
  ParseInput;
  ParseYaml;
  Regenerate;
end;

procedure TOTClassRegenerator.LoadInput;
begin
  FInput := TStringList.Create;
  FInput.LoadFromFile(FFileName);
end;

procedure TOTClassRegenerator.ParseInput;
var
  i: Integer;
  line: string;
  state: (piStartComment, piEndComment, piCloseComment);
  startLine: Integer;
  currentBlock: EBlockIdentifier;
  blockName: string;
  endBlock: string;
begin
  state := piStartComment;

  for i := 0 to FInput.Count -1 do begin
    line := FInput[i].TrimLeft;
    case state of
      piStartComment:
      begin
        if line.StartsWith('{#') then begin
           startLine := i+1;
           state := piCloseComment;
        end
        else if line.StartsWith('//# gen') then begin
          startLine := i+1;
          blockName := Copy(line, Length('//# gen') + 1);
          currentBlock := MatchBlockName(blockName);
          endBlock := '//# endGen' + blockName;
          state := piEndComment;
        end;
      end;

      piEndComment:
      begin
        if line = endBlock then begin
          FBlocks[currentBlock] := TBlock.Create(startLine, i);
          state := piStartComment;
        end;
      end;

      piCloseComment:
      begin
        if line = '}' then begin
           FBlocks[biClassYaml] := TBlock.Create(startLine, i);
           state := piStartComment;
        end;
      end;
    end;
  end;
end;

procedure TOTClassRegenerator.ParseYaml;
var
  yaml: string;
  yamlStream: TStringStream;
  i: Integer;
  yamlBlock: TBlock;
  parser: TYamlParser;
  event: TYamlEvent;
  state:
    (pyInitial,
    pyExpectingDocumentStart,
    pyExpectingMappingStart,
    pyExpectingClassInfo,
    pyExpectingClassInfoValue,
    pyExpectingSequenceStart,
    pyExpectingAttributeStart,
    pyExpectingName,
    pyExpectingValue);
  newAttr: TAttribute;
  currentName: string;
begin
  yamlBlock := FBlocks[biClassYaml];
  if not Assigned(yamlBlock) then
    Exception.Create('class yaml block not defined');

  yaml := '';
  for i := yamlBlock.startLine to yamlBlock.followingLine -1 do begin
    yaml := yaml + FInput[i] + #$0A;
  end;

  yamlStream := TStringStream.Create(yaml);
  try
    parser := TYamlParser.Create;
    try
      parser.SetInput(yamlStream);

      state := pyInitial;
      event := parser.parse;
      while not (event is TStreamEndEvent) do begin
        case state of
          pyInitial:
            begin
              if not (event is TStreamStartEvent) then
                raise Exception.Create('Expected Yaml Stream Start');
              state := pyExpectingDocumentStart;
            end;

          pyExpectingDocumentStart:
            begin
              if not (event is TDocumentStartEvent) then
                raise Exception.Create('Expected Yaml Document Start');
              state := pyExpectingMappingStart;
            end;

          pyExpectingMappingStart:
            begin
              if not (event is TMappingStartEvent) then
                raise Exception.Create('Expected Yaml Mapping Start');
              state := pyExpectingClassInfo;
            end;

          pyExpectingClassInfo:
            begin
              if (event is TMappingEndEvent) then begin
                 // only expecting one class definition
                 break;
                 end
              else if not (event is TScalarEvent) then
                raise Exception.Create('Expected Yaml Scalar Event (classinfo name)');

              currentName := TScalarEvent(event).value;
              if currentName = 'class' then
                state := pyExpectingClassInfoValue
              else if currentName = 'attributes' then
                state := pyExpectingSequenceStart;
            end;

          pyExpectingClassInfoValue:
            begin
              if not (event is TScalarEvent) then
                raise Exception.Create('Expecting Yaml Scalar Event (classinfo value)');
              FClassName := TScalarEvent(event).value;
              state := pyExpectingClassInfo;
            end;

          pyExpectingSequenceStart:
            begin
              if not (event is TSequenceStartEvent) then
                raise Exception.Create('Expected Yaml Sequence Start');
              state := pyExpectingAttributeStart;
            end;

          pyExpectingAttributeStart:
            begin
              if (event is TSequenceEndEvent) then begin
                state := pyExpectingClassInfo;
              end
              else begin
                if not (event is TMappingStartEvent) then
                  raise Exception.Create('Expected Yaml Mapping Start');
                newAttr := TAttribute.Create;
                state := pyExpectingName;
              end;
            end;

          pyExpectingName:
            begin
              if (event is TMappingEndEvent) then begin
                 FAttributes.Add(newAttr);
                 newAttr := nil;
                 state := pyExpectingAttributeStart;
              end
              else begin
                if not (event is TScalarEvent) then
                  raise Exception.Create('Expected Yaml Scalar (name)');

                currentName := TScalarEvent(event).value;
                state := pyExpectingValue;
              end;
            end;

          pyExpectingValue:
            begin
              if not (event is TScalarEvent) then
                raise Exception.Create('Expected Yaml Scalar (value)');

              if currentName = 'name' then begin
                 newAttr.name := TScalarEvent(event).value;
              end
              else if currentName = 'type' then begin
                newAttr.typeName:= TScalarEvent(event).value;
              end;
              currentName := '';
              state := pyExpectingName;
            end;
          end;
        event.Free;
        event := parser.parse;
      end;
    finally
      parser.Free;
    end;
  finally
    yamlStream.Free;
  end;
end;

procedure TOTClassRegenerator.Regenerate;
var
  code: array[EBlockIdentifier] of TStringList;
  bi: EBlockIdentifier;
  output: TStringList;
  startIndex: Integer;
  i: Integer;
begin
  for bi in EBlockIdentifier do
    code[bi] := nil;
  output := TStringList.Create;
  try
    code[biMemberVars] := GenerateMemberVars;
    // code[biCollections] := ...,
    code[biGetSetDeclarations] := GenerateGetSetDeclarations;
    code[biProperty] := GeneratePropertyDeclarations;
    code[biRestoreYamlVars] := GenerateRestoreYamlVars;
    code[biRestoreVars] := GenerateRestoreVars;
    code[biSaveVars] := GenerateSaveVars;
    code[biSaveYamlVars] := GenerateSaveYamlVars;
    code[biGetSetMethods] := GenerateGetSetMethods;

    bi := biMemberVars;
    startIndex := 0;

    while true do begin
      for i := startIndex to FBlocks[bi].startLine - 1 do begin
        output.Add(FInput[i]);
      end;
      output.AddStrings(code[bi]);
      startIndex := FBlocks[bi].followingLine;

      if bi = biGetSetMethods then
        break;

      Inc(bi);
      while (FBlocks[bi] = nil) or (code[bi] = nil) do
        Inc(bi);
    end;
    for i := startIndex to FInput.Count- 1 do
      output.Add(FInput[i]);

    output.SaveToFile('temp.xxx');
    if FileExists(FBackupFileName) then
      DeleteFile(FBackupFileName);
    RenameFile(FFileName, FBackupFileName);
    RenameFile('temp.xxx', FFileName);
  finally
    for bi in EBlockIdentifier do
      code[bi].Free;
    output.Free;
  end;
end;

function TOTClassRegenerator.GenerateMemberVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      t.LoadTemplateFromResource('TEMPLATE_MEMBERVAR_SIMPLE');
      (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GenerateGetSetDeclarations: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      t.LoadTemplateFromResource('TEMPLATE_SETPROPERTY_SIMPLE_DECL');
      (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GeneratePropertyDeclarations: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      t.LoadTemplateFromResource('TEMPLATE_PROPERTY_SIMPLE');
      (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GenerateRestoreYamlVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  t := TOTTemplateGenerator.Create;
  try
    t.LoadTemplateFromResource('TEMPLATE_RESTORE_YAML_VARS');

    for attr in FAttributes do begin
      (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;

      t.Generate(result);
    end;
  finally
    t.Free;
  end;
end;

function TOTClassRegenerator.GenerateRestoreVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  t := TOTTemplateGenerator.Create;
  try
    t.LoadTemplateFromResource('TEMPLATE_RESTORE_VARS');

    for attr in FAttributes do begin
      (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;

      t.Generate(result);
    end;
  finally
    t.Free;
  end;
end;

function TOTClassRegenerator.GenerateSaveVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  t := TOTTemplateGenerator.Create;
  try
    t.LoadTemplateFromResource('TEMPLATE_SAVE_VARS');

    for attr in FAttributes do begin
      (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;

      t.Generate(result);
    end;
  finally
    t.Free;
  end;
end;

function TOTClassRegenerator.GenerateSaveYamlVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  t := TOTTemplateGenerator.Create;
  try
    t.LoadTemplateFromResource('TEMPLATE_SAVE_YAML_VARS');

    for attr in FAttributes do begin
      (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;

      t.Generate(result);
    end;
  finally
    t.Free;
  end;
end;

function TOTClassRegenerator.GenerateGetSetMethods: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  t := TOTTemplateGenerator.Create;
  try
    t.LoadTemplateFromResource('TEMPLATE_SETPROPERTY_SIMPLE');

    for attr in FAttributes do begin
      (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      (t['Class'] as TOTTemplateSubstitution).userValue := FClassName;

      t.Generate(result);
    end;
  finally
    t.Free;
  end;
end;

end.
