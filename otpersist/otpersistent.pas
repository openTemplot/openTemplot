unit OTPersistent;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  OTYaml,
  OTYamlEmitter,
  OTYamlParser,
  OTYamlEvent;

type

  TOID = Cardinal;
  POID = ^TOID;

  TOTPersistentLoader = class;

  { TOTPersistent }

  TOTPersistent = class
  private
    FOID: TOID;
    FParent: TOID;
    FIsCalculated: Boolean;
    FReferences: TList<TOID>;

    procedure AddReference(AOID: TOID);
    procedure DeleteReference(AOID: TOID);
    function GetReferencesCount: Integer;

    function GetParent: TOTPersistent;

  protected
    procedure RestoreAttributes(AStream: TStream); virtual;
    procedure SaveAttributes(AStream: TStream); virtual;

    procedure RestoreYamlObjectOwn(var AOid: TOID; ASourceOID: TOID; ALoader: TOTPersistentLoader);
    procedure RestoreYamlObjectRef(var AOID: TOID; ASourceOID: TOID; ALoader: TOTPersistentLoader);

    procedure SetModified;
    procedure CheckCalculated;
    procedure SetOwned(var AOID: TOID; ANew: TOTPersistent);
    procedure SetReference(var AOID: TOID; ANew: TOTPersistent);

    procedure Calculate; virtual;

  public
    constructor Create(AParent: TOTPersistent); virtual;
    destructor Destroy; override;

    procedure SaveToYaml(AEmitter: TYamlEmitter);
    procedure SaveToStream(AStream: TStream);

    procedure RestoreYamlAttribute(AName, AValue: String; AIndex: Integer; ALoader: TOTPersistentLoader); virtual;
    procedure SaveYamlAttributes(AEmitter: TYamlEmitter); virtual;

    class procedure RegisterClass;
    class function RestoreYamlObject(AParent: TOTPersistent; AParser: TYamlParser;
      AEvent: TMappingStartEvent; ALoader: TOTPersistentLoader): TOTPersistent;
    class function RestoreYamlFromStream(AStream: TStream): TOTPersistent;

    class function RestoreStreamedObject(AParent: TOTPersistent; AStream: TStream): TOTPersistent;
    class function FromOID(AOid: TOID): TOTPersistent;

    property oid: TOID Read FOID;
    property parent: TOTPersistent Read GetParent;
    property isCalculated: Boolean Read FIsCalculated;
    property referencesCount: Integer Read GetReferencesCount;
  end;

  TOTPersistentClassRef = class of TOTPersistent;

  { TOTPersistentLoader }

  TOTPersistentLoader = class
  private
    FMap: TDictionary<TOID, TOTPersistent>;
    FDeferredMap: TDictionary<TOID, TList<POID>>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(AObject: TOTPersistent; ALoadedOID: TOID);
    procedure AddDeferredObject(var AOID: TOID; ALoadedOID: TOID);
    function HasOID(AOID: TOID): Boolean;
    function GetObject(AOID: TOID): TOTPersistent;
  end;

function StrToInteger(const AValue: String): Integer;
function StrToDouble(const AValue: String): Double;
function StrToBoolean(const AValue: String): Boolean;
function StrToString(const AValue: String): String;

procedure SaveYamlInteger(AEmitter: TYamlEmitter; const AName: String; AValue: Integer);
procedure SaveYamlDouble(AEmitter: TYamlEmitter; const AName: String; AValue: Double);
procedure SaveYamlBoolean(AEmitter: TYamlEmitter; const AName: String; AValue: Boolean);
procedure SaveYamlString(AEmitter: TYamlEmitter; const AName: String; AValue: String);
procedure SaveYamlObject(AEmitter: TYamlEmitter; const AName: String; AValue: TOID);
procedure SaveYamlObjectReference(AEmitter: TYamlEmitter; const AName: String; AValue: TOID);
procedure SaveYamlSequence(AEmitter: TYamlEmitter; const AName: String);
procedure SaveYamlSequenceInteger(AEmitter: TYamlEmitter; AValue: Integer);
procedure SaveYamlSequenceDouble(AEmitter: TYamlEmitter; AValue: Double);
procedure SaveYamlSequenceBoolean(AEmitter: TYamlEmitter; AValue: Boolean);
procedure SaveYamlSequenceString(AEmitter: TYamlEmitter; const AValue: String);
procedure SaveYamlSequenceObject(AEmitter: TYamlEmitter; AValue: TOID);
procedure SaveYamlSequenceObjectReference(AEmitter: TYamlEmitter; AValue: TOID);
procedure SaveYamlEndSequence(AEmitter: TYamlEmitter);

implementation

uses
  OTOIDManager;

var
  Registry: THashMap<String, TOTPersistentClassRef>;

function StrToInteger(const AValue: String): Integer;
begin
  Result := StrToInt(AValue);
end;

function StrToDouble(const AValue: String): Double;
begin
  Result := StrToFloat(AValue);
end;

function StrToBoolean(const AValue: String): Boolean;
begin
  Result := StrToBool(AValue);
end;

function StrToString(const AValue: String): String;
begin
  Result := AValue;
end;

procedure SaveYamlInteger(AEmitter: TYamlEmitter; const AName: String; AValue: Integer);
begin
  AEmitter.ScalarEvent('', '', AName, True, False, yssPlainScalar);
  AEmitter.ScalarEvent('', '', IntToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlDouble(AEmitter: TYamlEmitter; const AName: String; AValue: Double);
begin
  AEmitter.ScalarEvent('', '', AName, True, False, yssPlainScalar);
  AEmitter.ScalarEvent('', '', FloatToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlBoolean(AEmitter: TYamlEmitter; const AName: String; AValue: Boolean);
begin
  AEmitter.ScalarEvent('', '', AName, True, False, yssPlainScalar);
  AEmitter.ScalarEvent('', '', BoolToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlString(AEmitter: TYamlEmitter; const AName: String; AValue: String);
begin
  AEmitter.ScalarEvent('', '', AName, True, False, yssPlainScalar);
  AEmitter.ScalarEvent('', '', AValue, True, True, yssDoubleQuotedScalar);
end;

procedure SaveYamlObject(AEmitter: TYamlEmitter; const AName: String; AValue: TOID);
begin
  AEmitter.ScalarEvent('', '', AName, True, False, yssPlainScalar);
  if (AValue = 0) then begin
    AEmitter.MappingStartEvent('', '', False, ympFlowMapping);
    AEmitter.MappingEndEvent;
  end
  else begin
    TOTPersistent.FromOID(AValue).SaveToYaml(AEmitter);
  end;
end;

procedure SaveYamlObjectReference(AEmitter: TYamlEmitter; const AName: String; AValue: TOID);
begin
  AEmitter.ScalarEvent('', '', AName, True, False, yssPlainScalar);
  AEmitter.ScalarEvent('', '', IntToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlSequence(AEmitter: TYamlEmitter; const AName: String);
begin
  AEmitter.ScalarEvent('', '', AName, True, False, yssPlainScalar);
  AEmitter.SequenceStartEvent('', '', False, ysqAnyStyle);
end;

procedure SaveYamlSequenceInteger(AEmitter: TYamlEmitter; AValue: Integer);
begin
  AEmitter.ScalarEvent('', '', IntToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlSequenceDouble(AEmitter: TYamlEmitter; AValue: Double);
begin
  AEmitter.ScalarEvent('', '', FloatToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlSequenceBoolean(AEmitter: TYamlEmitter; AValue: Boolean);
begin
  AEmitter.ScalarEvent('', '', BoolToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlSequenceString(AEmitter: TYamlEmitter; const AValue: String);
begin
  AEmitter.ScalarEvent('', '', AValue, True, True, yssDoubleQuotedScalar);
end;

procedure SaveYamlSequenceObject(AEmitter: TYamlEmitter; AValue: TOID);
begin
  TOTPersistent.FromOID(AValue).SaveToYaml(AEmitter);
end;

procedure SaveYamlSequenceObjectReference(AEmitter: TYamlEmitter; AValue: TOID);
begin
  AEmitter.ScalarEvent('', '', IntToStr(AValue), True, False, yssPlainScalar);
end;

procedure SaveYamlEndSequence(AEmitter: TYamlEmitter);
begin
  AEmitter.SequenceEndEvent;
end;

{ TOTPersistentLoader }

constructor TOTPersistentLoader.Create;
begin
  inherited;

  FMap := TDictionary<TOID, TOTPersistent>.Create;
  FDeferredMap := TDictionary<TOID, TList<POID>>.Create;
end;

destructor TOTPersistentLoader.Destroy;
begin
  FMap.Free;
  FDeferredMap.Free;

  inherited Destroy;
end;

function TOTPersistentLoader.HasOID(AOID: TOID): Boolean;
begin
  Result := FMap.ContainsKey(AOID);
end;

procedure TOTPersistentLoader.AddObject(AObject: TOTPersistent; ALoadedOID: TOID);
var
  p: POID;
  pair: TPair<TOID,TList<POID>>;
begin
  if FMap.ContainsKey(ALoadedOID) then
    raise Exception.CreateFmt('Loader: duplicate OID: %d', [ALoadedOID]);

  FMap.Add(ALoadedOID, AObject);

  if FDeferredMap.ContainsKey(ALoadedOID) then begin
    pair := FDeferredMap.ExtractPair(ALoadedOID);
    for p in pair.value do begin
      p^ := AObject.oid;
    end;

    pair.value.Free;
  end;
end;

procedure TOTPersistentLoader.AddDeferredObject(var AOID: TOID; ALoadedOID: TOID);
var
  refList: TList<POID>;
begin
  if FDeferredMap.ContainsKey(ALoadedOID) then begin
    refList := FDeferredMap[ALoadedOID];
  end
  else begin
    refList := TList<POID>.Create;
    FDeferredMap.Add(ALoadedOID, refList);
  end;

  refList.Add(@AOID);
end;

function TOTPersistentLoader.GetObject(AOID: TOID): TOTPersistent;
begin
  result := FMap[AOID];
end;

{ TOTPersistent }

constructor TOTPersistent.Create(AParent: TOTPersistent);
begin
  inherited Create;

  FOID := OIDManager.AllocateOID(self);
  if Assigned(AParent) then
    FParent := AParent.oid
  else
    FParent := 0;
  FIsCalculated := False;
  FReferences := TList<TOID>.Create;
end;

destructor TOTPersistent.Destroy;
begin
  FReferences.Free;
  OIDManager.FreeOID(FOID);

  inherited Destroy;
end;

function TOTPersistent.GetParent: TOTPersistent;
begin
  Result := FromOID(FParent);
end;

procedure TOTPersistent.AddReference(AOID: TOID);
begin
  FReferences.Add(AOID);
end;

procedure TOTPersistent.DeleteReference(AOID: TOID);
begin
  FReferences.Remove(AOID);
end;

function TOTPersistent.GetReferencesCount: Integer;
begin
  Result := FReferences.Count;
end;

procedure TOTPersistent.RestoreAttributes(AStream: TStream);
begin

end;

procedure TOTPersistent.SaveAttributes(AStream: TStream);
begin

end;

procedure TOTPersistent.SetModified;
var
  oid: TOID;
begin
  if FIsCalculated then begin
    FIsCalculated := False;

    if FParent <> 0 then begin
      FromOID(FParent).SetModified;
    end;

    for oid in FReferences do begin
      if oid <> 0 then
        FromOID(oid).SetModified;
    end;
  end;
end;

procedure TOTPersistent.CheckCalculated;
begin
  if not FIsCalculated then begin
    FIsCalculated := True;
    Calculate;
  end;
end;

procedure TOTPersistent.SetOwned(var AOID: TOID; ANew: TOTPersistent);
begin
  if (AOID = 0) and (ANew = nil) then
    // equal and nil, so no change...
    Exit;

  if Assigned(ANew) and (AOID = ANew.FOID) then
    // oid's the same, so no change...
    Exit;

  SetModified;

  if (AOID <> 0) then begin
    OIDManager.FreeOID(AOID);
  end;
  if (ANew <> nil) then begin
    AOID := ANew.oid;
    ANew.FParent := FOID;
  end
  else begin
    AOID := 0;
  end;
end;

procedure TOTPersistent.SetReference(var AOID: TOID; ANew: TOTPersistent);
begin
  if (AOID = 0) and (ANew = nil) then
    // equal and nil, so no change...
    Exit;

  if Assigned(ANew) and (AOID = ANew.FOID) then
    // oid's the same, so no change...
    Exit;

  SetModified;

  if (AOID <> 0) then begin
    FromOID(AOID).DeleteReference(FOID);
  end;
  if Assigned(ANew) then begin
    AOID := ANew.oid;
    ANew.AddReference(FOID);
  end
  else begin
    AOID := 0;
  end;
end;

procedure TOTPersistent.Calculate;
begin
  // nothing to do here...
end;

procedure TOTPersistent.RestoreYamlAttribute(AName, AValue: String; AIndex: Integer; ALoader: TOTPersistentLoader);
begin
  if AName = 'oid' then begin
    ALoader.AddObject(self, StrToInteger(AValue));
  end;
end;

procedure TOTPersistent.SaveYamlAttributes(AEmitter: TYamlEmitter);
begin
  SaveYamlObjectReference(AEmitter, 'oid', FOID);
end;

procedure TOTPersistent.SaveToYaml(AEmitter: TYamlEmitter);
begin
  AEmitter.MappingStartEvent('', ClassName, False, ympBlockMapping);
  SaveYamlAttributes(AEmitter);
  AEmitter.MappingEndEvent;
end;

procedure TOTPersistent.SaveToStream(AStream: TStream);
var
  classref: TOTPersistentClassRef;
begin
  classref := TOTPersistentClassRef(ClassType);
  AStream.WriteBuffer(classref, sizeof(classref));
  SaveAttributes(AStream);
end;

class procedure TOTPersistent.RegisterClass;
begin
  // do something
  if Registry.ContainsKey(ClassName) then
    raise Exception.CreateFmt('Class %s already registered', [ClassName]);

  Registry.Add(ClassName, TOTPersistentClassRef(ClassType));
end;

class function TOTPersistent.RestoreYamlObject(AParent: TOTPersistent;
  AParser: TYamlParser; AEvent: TMappingStartEvent; ALoader: TOTPersistentLoader): TOTPersistent;
var
  objClass: TOTPersistentClassRef;
  event: TYamlEvent;
  key: String;
  Value: String;
  index: Integer;
  objectFromYaml: TOTPersistent;
begin
  if (AEvent.tag = '') then begin
    // empty object, return nil
    event := AParser.Parse;
    if not (event is TMappingEndEvent) then
      raise Exception.Create('MappingStart without a tag must be empty');
    event.Free;
    Exit(nil);
  end;

  objClass := Registry.Items[AEvent.tag];
  if not Assigned(objClass) then
    raise Exception.CreateFmt('Unknown class: %s', [AEvent.tag]);

  Result := objClass.Create(AParent);
  try
    event := AParser.Parse;
    while not (event is TMappingEndEvent) do begin
      if not (event is TScalarEvent) then
        raise Exception.Create('Expected a Scalar key');

      key := TScalarEvent(event).Value;
      FreeAndNil(event);
      event := AParser.Parse;

      if (event is TMappingStartEvent) then begin
        // this is the start of an owned sub-object,
        // so let's go recursive...
        objectFromYaml := RestoreYamlObject(Result, AParser, TMappingStartEvent(event), ALoader);
        if Assigned(ObjectFromYaml) then begin
          Value := IntToStr(objectFromYaml.oid);
        end
        else begin
          Value := '0';
        end;
        Result.RestoreYamlAttribute(key, Value, 0, ALoader);
      end
      else
      if (event is TSequenceStartEvent) then begin
        // start of a collection
        index := 0;
        FreeAndNil(event);
        event := AParser.Parse;
        while not (event is TSequenceEndEvent) do begin
          if (event is TScalarEvent) then begin
            Value := TScalarEvent(event).Value;
            Result.RestoreYamlAttribute(key, Value, index, ALoader);
          end
          else
          if (event is TMappingStartEvent) then begin
            objectFromYaml := RestoreYamlObject(Result, AParser, TMappingStartEvent(event), ALoader);
            Value := IntToStr(objectFromYaml.oid);
            Result.RestoreYamlAttribute(key, Value, index, ALoader);
          end
          else begin
            raise Exception.Create('Expected Scalar or Mapping value in sequence');
          end;

          Inc(index);
          FreeAndNil(event);
          event := AParser.Parse;
        end;
      end
      else
      if (event is TScalarEvent) then begin
        Value := TScalarEvent(event).Value;
        Result.RestoreYamlAttribute(key, Value, 0, ALoader);
      end
      else
        raise Exception.Create('Expected MappingStart, SequenceStart or Scalar value');
      FreeAndNil(event);
      event := AParser.Parse;
    end;
    event.Free;
  except
    Result.Free;
    raise;
  end;
end;

procedure TOTPersistent.RestoreYamlObjectOwn(var AOid: TOID; ASourceOID: TOID; ALoader: TOTPersistentLoader);
begin
  if (AOid <> 0) then begin
    OIDManager.FreeOID(AOid);
  end;
  AOid := ASourceOID;
  if ASourceOID <> 0 then begin
    FromOid(ASourceOID).FParent := FOID;
  end;
end;

procedure TOTPersistent.RestoreYamlObjectRef(var AOID: TOID; ASourceOID: TOID; ALoader: TOTPersistentLoader);
var
  referencedObject: TOTPersistent;
begin
  if ASourceOID = 0 then begin
    AOID := 0;
  end
  else if ALoader.HasOID(ASourceOID) then begin
    referencedObject := ALoader.GetObject(ASourceOID);
    AOID := referencedObject.OID;
    referencedObject.AddReference(FOid);
  end
  else begin
    ALoader.AddDeferredObject(AOID, ASourceOID);
  end;
end;

class function TOTPersistent.RestoreYamlFromStream(AStream: TStream): TOTPersistent;
var
  parser: TYamlParser;
  event: TYamlEvent;
  state: (
    stExpectStreamStart,
    stExpectDocumentStart,
    stExpectMappingStart,
    stExpectDocumentEnd
  );
  loader: TOTPersistentLoader;
begin
  Result := nil;
  loader := nil;
  parser := TYamlParser.Create;
  try
    parser.SetInput(AStream);
    loader := TOTPersistentLoader.Create;

    state := stExpectStreamStart;
    event := parser.Parse;
    while not (event is TStreamEndEvent) do begin
      case state of
        stExpectStreamStart: begin
          if not (event is TStreamStartEvent) then
            raise Exception.Create('Expected Stream Start');
          state := stExpectDocumentStart;
        end;
        stExpectDocumentStart: begin
          if not (event is TDocumentStartEvent) then
            raise Exception.Create('Expected Document Start');
          state := stExpectMappingStart;
        end;
        stExpectMappingStart: begin
          if not (event is TMappingStartEvent) then
            raise Exception.Create('Expected Mapping Start');

          Result := TOTPersistent.RestoreYamlObject(nil, parser, event as TMappingStartEvent, loader);
          state := stExpectDocumentEnd;
        end;
        stExpectDocumentEnd: begin
          if not (event is TDocumentEndEvent) then
            raise Exception.Create('Expected Document End');
        end;
      end;

      FreeAndNil(event);
      event := parser.Parse;
    end;

  finally
    loader.Free;
    parser.Free;
    event.Free;
  end;
end;

class function TOTPersistent.RestoreStreamedObject(AParent: TOTPersistent;
  AStream: TStream): TOTPersistent;
var
  classref: TOTPersistentClassRef;
  obj: TOTPersistent;
begin
  AStream.ReadBuffer(classref, sizeof(classref));
  obj := classref.Create(AParent);
  try
    obj.RestoreAttributes(AStream);
  except
    obj.Free;
    raise;
  end;
  Result := obj;
end;

class function TOTPersistent.FromOID(AOid: TOID): TOTPersistent;
begin
  Result := OIDManager.FromOID(AOID);
end;

initialization
  Registry := THashMap<String, TOTPersistentClassRef>.Create;

finalization
  Registry.Free;

end.
