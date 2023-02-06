unit OTPersistent;

{$mode Delphi}

interface

uses
  Classes,
  SysUtils,
  OTYaml,
  OTYamlEmitter,
  OTYamlParser,
  OTYamlEvent;

type

  { TOTPersistent }

  TOTPersistent = class

  protected
    procedure RestoreAttributes(AStream: TStream); virtual;
    procedure SaveAttributes(AStream: TStream); virtual;

    procedure SetModified;

  public
    constructor Create(AParent: TOTPersistent); virtual;
    destructor Destroy; override;

    procedure SaveYamlObject(AEmitter: TYamlEmitter);

    procedure RestoreYamlAttribute(AName, AValue: String; AIndex: Integer); virtual;
    procedure SaveYamlAttributes(AEmitter: TYamlEmitter); virtual;

    class procedure RegisterClass;
    class function RestoreYamlObject(AParent: TOTPersistent; AParser: TYamlParser; AEvent: TMappingStartEvent): TOTPersistent;
    class function RestoreYamlFromStream(AStream: TStream): TOTPersistent;

  end;

  TOTPersistentClassRef = class of TOTPersistent;

function StrToInteger(const AValue: String): Integer;
function StrToDouble(const AValue: String): Double;
function StrToBoolean(const AValue: String): Boolean;
function StrToString(const AValue: String): String;

procedure SaveYamlInteger(AEmitter: TYamlEmitter; const AName: String; AValue: Integer);
procedure SaveYamlDouble(AEmitter: TYamlEmitter; const AName: String; AValue: Double);
procedure SaveYamlBoolean(AEmitter: TYamlEmitter; const AName: String; AValue: Boolean);
procedure SaveYamlString(AEmitter: TYamlEmitter; const AName: String; AValue: String);
procedure SaveYamlSequence(AEmitter: TYamlEmitter; const AName: String);
procedure SaveYamlSequenceInteger(AEmitter: TYamlEmitter; AValue: Integer);
procedure SaveYamlSequenceDouble(AEmitter: TYamlEmitter; AValue: Double);
procedure SaveYamlSequenceBoolean(AEmitter: TYamlEmitter; AValue: Boolean);
procedure SaveYamlSequenceString(AEmitter: TYamlEmitter; const AValue: String);
procedure SaveYamlEndSequence(AEmitter: TYamlEmitter);

implementation

uses
  Generics.Collections;

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
  AEmitter.ScalarEvent('', '', AName, true, false, yssPlainScalar);
  AEmitter.ScalarEvent('', '', IntToStr(AValue), true, false, yssPlainScalar);
end;

procedure SaveYamlDouble(AEmitter: TYamlEmitter; const AName: String; AValue: Double);
begin
  AEmitter.ScalarEvent('', '', AName, true, false, yssPlainScalar);
  AEmitter.ScalarEvent('', '', FloatToStr(AValue), true, false, yssPlainScalar);
end;

procedure SaveYamlBoolean(AEmitter: TYamlEmitter; const AName: String; AValue: Boolean);
begin
  AEmitter.ScalarEvent('', '', AName, true, false, yssPlainScalar);
  AEmitter.ScalarEvent('', '', BoolToStr(AValue), true, false, yssPlainScalar);
end;

procedure SaveYamlString(AEmitter: TYamlEmitter; const AName: String; AValue: String);
begin
  AEmitter.ScalarEvent('', '', AName, true, false, yssPlainScalar);
  AEmitter.ScalarEvent('', '', AValue, true, true, yssDoubleQuotedScalar);
end;

procedure SaveYamlSequence(AEmitter: TYamlEmitter; const AName: String);
begin
  AEmitter.ScalarEvent('', '', AName, true, false, yssPlainScalar);
  AEmitter.SequenceStartEvent('', '', false, ysqAnyStyle);
end;

procedure SaveYamlSequenceInteger(AEmitter: TYamlEmitter; AValue: Integer);
begin
  AEmitter.ScalarEvent('', '', IntToStr(AValue), true, false, yssPlainScalar);
end;

procedure SaveYamlSequenceDouble(AEmitter: TYamlEmitter; AValue: Double);
begin
  AEmitter.ScalarEvent('', '', FloatToStr(AValue), true, false, yssPlainScalar);
end;

procedure SaveYamlSequenceBoolean(AEmitter: TYamlEmitter; AValue: Boolean);
begin
  AEmitter.ScalarEvent('', '', BoolToStr(AValue), true, false, yssPlainScalar);
end;

procedure SaveYamlSequenceString(AEmitter: TYamlEmitter; const AValue: String);
begin
  AEmitter.ScalarEvent('', '', AValue, true, true, yssDoubleQuotedScalar);
end;

procedure SaveYamlEndSequence(AEmitter: TYamlEmitter);
begin
  AEmitter.SequenceEndEvent;
end;

{ TOTPersistent }

constructor TOTPersistent.Create(AParent: TOTPersistent);
begin
  inherited Create;
end;

destructor TOTPersistent.Destroy;
begin
  inherited Destroy;
end;

procedure TOTPersistent.RestoreAttributes(AStream: TStream);
begin

end;

procedure TOTPersistent.SaveAttributes(AStream: TStream);
begin

end;

procedure TOTPersistent.SetModified;
begin
  // do something!
end;

procedure TOTPersistent.RestoreYamlAttribute(AName, AValue: String;
  AIndex: Integer);
begin

end;

procedure TOTPersistent.SaveYamlAttributes(AEmitter: TYamlEmitter);
begin

end;

procedure TOTPersistent.SaveYamlObject(AEmitter: TYamlEmitter);
begin
  AEmitter.MappingStartEvent('', className, false, ympBlockMapping);
  SaveYamlAttributes(AEmitter);
  AEmitter.MappingEndEvent;
end;

class procedure TOTPersistent.RegisterClass;
begin
  // do something
  if Registry.ContainsKey(className) then
    raise Exception.CreateFmt('Class %s already registered', [className]);

  Registry.Add(className, TOTPersistentClassRef(ClassType));
end;

class function TOTPersistent.RestoreYamlObject(AParent: TOTPersistent; AParser: TYamlParser; AEvent: TMappingStartEvent): TOTPersistent;
var
  objClass: TOTPersistentClassRef;
  event: TYamlEvent;
  key: String;
  value: String;
  index: Integer;
begin
  objClass := Registry.Items[AEvent.tag];
  if not Assigned(objClass) then
    raise Exception.CreateFmt('Unknown class: %s', [AEvent.tag]);

  result := objClass.Create(AParent);
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
        raise Exception.Create('RestoreYamlObject sub-objects not implemented yet!');
      end
      else if (event is TSequenceStartEvent) then begin
        // start of a collection
        index := 0;
        FreeAndNil(event);
        event := AParser.Parse;
        while not (event is TSequenceEndEvent) do begin
          if not (event is TScalarEvent) then
            raise Exception.Create('Expected Scalar value in sequence');

          value := TScalarEvent(event).value;
          result.RestoreYamlAttribute(key, value, index);
          Inc(index);
          FreeAndNil(event);
          event := AParser.Parse;
        end;
      end
      else if (event is TScalarEvent) then begin
        value := TScalarEvent(event).Value;
        result.RestoreYamlAttribute(key, value, 0);
      end
      else
        raise Exception.Create('Expected MappingStart, SequenceStart or Scalar value');
      FreeAndNil(event);
      event := AParser.Parse;
    end;
    event.Free;
  except
    result.Free;
    raise;
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
begin
  Result := nil;
  parser := TYamlParser.Create;
  try
    parser.SetInput(AStream);

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

          Result := TOTPersistent.RestoreYamlObject(nil, parser, event as TMappingStartEvent);
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
    parser.Free;
    event.Free;
  end;
end;

initialization
  Registry := THashMap<String, TOTPersistentClassRef>.Create;

finalization
  Registry.Free;

end.
