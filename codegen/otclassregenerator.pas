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
    biGetSetDeclarations,
    biPublicDeclarations,
    biProperty,
    biRestoreYamlVars,
    biRestoreVars,
    biSaveVars,
    biSaveYamlVars,
    biGetSetMethods
  );

  TOperation = (opAdd, opDelete, opClear);
  TOperationSet = set of TOperation;

  TAttribute = class
    private
      FName: string;
      FTypeName: string;
      FIsCollection: boolean;
      FIsDynamic: boolean;
      FIsString: boolean;
      FBounds: string;
      FIndexType: string;
      FHasComment: boolean;
      FComment: TStringList;
      FOperations: TOperationSet;

    public
      constructor Create;
      destructor Destroy; override;

      procedure SetProperty(const AName, AValue: String); overload;
      procedure SetProperty(const AName: String; const AValues: TArray<String>); overload;

      property name: string read FName;
      property typeName: string read FTypeName;
      property isCollection: boolean read FIsCollection;
      property isDynamic: boolean read FIsDynamic;
      property isString: boolean read FIsString;
      property bounds: string read FBounds;
      property indexType: string read FIndexType;
      property hasComment: boolean read FHasComment;
      property comment: TStringList read FComment;
      property operations: TOperationSet read FOperations;

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
    function GeneratePublicDeclarations: TStringList;
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
    'GetSetDeclarations',
    'PublicDeclarations',
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

  FIsCollection := false;
  FIsDynamic := false;
  FBounds := '';
  FIndexType := 'integer';
  FHasComment := false;
  FComment := nil;
end;

destructor TAttribute.Destroy;
begin
  FComment.Free;

  inherited;
end;

procedure TAttribute.SetProperty(const AName, AValue: String);
begin
  if AName = 'name' then
    FName := AValue
  else if AName = 'type' then begin
    FTypeName := AValue;
    FIsString := (FTypeName = 'String') or (FTypeName = 'string');
  end
  else if AName = 'array' then begin
     if AValue = 'false' then
       FIsCollection := false
     else begin
       FIsCollection := true;
       if AValue = 'dynamic' then
         FIsDynamic := true
       else if Pos('..', AValue) <> 0 then
         FBounds := AValue
       else begin
         FBounds := AValue;
         FIndexType := AValue;
       end;
     end;
  end
  else if AName = 'comment' then begin
      FHasComment := true;
      if not Assigned(FComment) then
        FComment := TStringList.Create;
      FComment.AddText(AValue);
  end
  else
    raise Exception.CreateFmt('unexpected attribute name/value-- %s: %s', [AName, AValue]);
end;

procedure TAttribute.SetProperty(const AName: String; const AValues: TArray<String>);
var
  i: Integer;
begin
  if AName = 'operations' then begin
    FOperations := [];
    for i := 0 to High(AValues) do begin
      if AValues[i] = 'add' then
        Include(FOperations, opAdd)
      else if AVAlues[i] = 'delete' then
        Include(FOperations, opDelete)
      else if AValues[i] = 'clear' then
        Include(FOperations, opClear)
      else
        raise Exception.CreateFmt('unexpected operation value -- %s', [AValues[i]]);
    end;
  end
  else
    raise Exception.CreateFmt('unexpected attribute name/value(s)-- %s', [AName]);

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
    pyExpectingValue,
    pyExpectingValueSequence);
  newAttr: TAttribute;
  currentName: string;
  currentSequence: TArray<string>;
begin
  yamlBlock := FBlocks[biClassYaml];
  if not Assigned(yamlBlock) then
    Exception.Create('class yaml block not defined');

  yaml := '';
  for i := yamlBlock.startLine to yamlBlock.followingLine -1 do begin
    yaml := yaml + FInput[i] + #$0A;
  end;

  parser := TYamlParser.Create;
  try
    parser.SetInput(TStringStream.Create(yaml));

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
            if (event is TSequenceStartEvent) then begin
               SetLength(currentSequence, 0);
               state := pyExpectingValueSequence;
            end
            else begin
              if not (event is TScalarEvent) then
                raise Exception.Create('Expected Yaml Scalar (value)');

              newAttr.SetProperty(currentName, TScalarEvent(event).value);
              currentName := '';
              state := pyExpectingName;
            end;
          end;

        pyExpectingValueSequence:
          begin
            if (event is TSequenceEndEvent) then begin
              newAttr.SetProperty(currentName, currentSequence);
              currentName := '';
              SetLength(currentSequence, 0);
              state := pyExpectingName;
            end
            else begin
              if not (event is TScalarEvent) then
                raise Exception.Create('Expected Yaml Scalar (value sequence)');
              SetLength(currentSequence, Length(currentSequence)+1);
              currentSequence[High(currentSequence)] := TScalarEvent(event).value;
            end;
          end;
        end;
      event.Free;
      event := parser.parse;
    end;
  finally
    parser.Free;
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
    code[biGetSetDeclarations] := GenerateGetSetDeclarations;
    code[biPublicDeclarations] := GeneratePublicDeclarations;
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
      if attr.isCollection then begin
        if attr.isDynamic then begin
          t.LoadTemplateFromResource('TEMPLATE_MEMBERVAR_DYNAMIC');
          (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
          (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        end
        else begin
          t.LoadTemplateFromResource('TEMPLATE_MEMBERVAR_ARRAY');
          (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
          (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
          (t['Bounds'] as TOTTemplateSubstitution).userValue := attr.bounds;
        end;
      end
      else begin
        t.LoadTemplateFromResource('TEMPLATE_MEMBERVAR_SIMPLE');
        (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      end;
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
    if attr.isCollection then begin
      t := TOTTemplateGenerator.Create;
      try
        t.LoadTemplateFromResource('TEMPLATE_GETPROPERTY_ARRAY_DECL');
        (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
        (t['IsDynamic'] as TOTTemplateCondition).userValue := attr.isDynamic;

        t.Generate(result);
      finally
        t.Free;
      end;
    end;
  end;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      if attr.isCollection then begin
        t.LoadTemplateFromResource('TEMPLATE_SETPROPERTY_ARRAY_DECL');
        (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
      end
      else begin
        t.LoadTemplateFromResource('TEMPLATE_SETPROPERTY_SIMPLE_DECL');
        (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      end;

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GeneratePublicDeclarations: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      if attr.isCollection and attr.isDynamic then begin
        t.LoadTemplateFromResource('TEMPLATE_PUBLIC_DECL');
        (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['HasAdd'] as TOTTemplateCondition).userValue := (opAdd in attr.operations);
        (t['HasDelete'] as TOTTemplateCondition).userValue := (opDelete in attr.operations);
        (t['HasClear'] as TOTTemplateCondition).userValue := (opClear in attr.operations);

        t.Generate(result);
      end;
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
      if attr.isCollection then begin
        t.LoadTemplateFromResource('TEMPLATE_PROPERTY_ARRAY');
        (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
        (t['IsDynamic'] as TOTTemplateCondition).userValue := attr.isDynamic;
      end
      else begin
        t.LoadTemplateFromResource('TEMPLATE_PROPERTY_SIMPLE');
        (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      end;

      (t['HasComment'] as TOTTemplateCondition).userValue := attr.hasComment;
      if (attr.hasComment) then begin
        (t['Comment'] as TOTTemplateMultiSubstitution).userValues.SetStrings(attr.comment);
      end;

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


  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      if attr.isCollection then begin
        t.LoadTemplateFromResource('TEMPLATE_RESTORE_YAML_VARS_ARRAY');
        (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
        (t['IsDynamic'] as TOTTemplateCondition).userValue := attr.isDynamic;
      end
      else begin
        t.LoadTemplateFromResource('TEMPLATE_RESTORE_YAML_VARS');
        (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      end;

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GenerateRestoreVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      if attr.isCollection then begin
        t.LoadTemplateFromResource('TEMPLATE_RESTORE_VARS_ARRAY');
        (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
        (t['IsDynamic'] as TOTTemplateCondition).userValue := attr.isDynamic;
        (t['IsString'] as TOTTemplateCondition).userValue := attr.isString;
      end
      else begin
        t.LoadTemplateFromResource('TEMPLATE_RESTORE_VARS');
        (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['IsString'] as TOTTemplateCondition).userValue := attr.isString;
      end;

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GenerateSaveVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try

    if attr.isCollection then begin
      t.LoadTemplateFromResource('TEMPLATE_SAVE_VARS_ARRAY');
      (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
      (t['IsDynamic'] as TOTTemplateCondition).userValue := attr.isDynamic;
      (t['IsString'] as TOTTemplateCondition).userValue := attr.isString;
    end
    else begin
      t.LoadTemplateFromResource('TEMPLATE_SAVE_VARS');
      (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      (t['IsString'] as TOTTemplateCondition).userValue := attr.isString;
    end;

    t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GenerateSaveYamlVars: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      if attr.isCollection then begin
        t.LoadTemplateFromResource('TEMPLATE_SAVE_YAML_VARS_ARRAY');
        (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
        (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
        (t['IsDynamic'] as TOTTemplateCondition).userValue := attr.isDynamic;
      end
      else begin
        t.LoadTemplateFromResource('TEMPLATE_SAVE_YAML_VARS');
        (t['Name'] as TOTTemplateSubstitution).userValue := attr.name;
        (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      end;

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

function TOTClassRegenerator.GenerateGetSetMethods: TStringList;
var
  attr: TAttribute;
  t: TOTTemplateGenerator;
begin
  result := TStringList.Create;

  for attr in FAttributes do begin
    t := TOTTemplateGenerator.Create;
    try
      t.LoadTemplateFromResource('TEMPLATE_GETSET_METHODS');
      (t['Name'] as TOTTemplateSubstitution).userValue:= attr.name;
      (t['Type'] as TOTTemplateSubstitution).userValue := attr.typeName;
      (t['Class'] as TOTTemplateSubstitution).userValue := FClassName;
      (t['IsCollection'] as TOTTemplateCondition).userValue := attr.isCollection;
      (t['IndexType'] as TOTTemplateSubstitution).userValue := attr.indexType;
      (t['IsDynamic'] as TOTTemplateCondition).userValue := attr.isDynamic;
      (t['HasAdd'] as TOTTemplateCondition).userValue := (opAdd in attr.operations);
      (t['HasDelete'] as TOTTemplateCondition).userValue := (opDelete in attr.operations);
      (t['HasClear'] as TOTTemplateCondition).userValue := (opClear in attr.operations);

      t.Generate(result);
    finally
      t.Free;
    end;
  end;
end;

end.
