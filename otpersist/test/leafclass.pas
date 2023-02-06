unit LeafClass;

{$MODE Delphi}

// This is a class specifically created for use in unit
// tests for OT Persistence Framework.
//
// This is simple class (no references to other classes)
// with basic properties
//

interface

uses
  Classes,
  SysUtils,
  OTPersistent,
  OTYamlEmitter;


{# class TLeafClass
---
class: TLeafClass
attributes:
  - name: int1
    type: Integer
  - name: int2
    type: Integer
  - name: str1
    type: String
  - name: array1
    type: Double
    array: dynamic
    operations: [add, delete, clear]
  - name: array2
    type: String
    array: 0..4
...
}

type

  TLeafClass = class(TOTPersistent)
  private
    //# genMemberVars
    FInt1: Integer;
    FInt2: Integer;
    FStr1: String;
    FArray1: array of Double;
    FArray2: array[0..4] of String;
    //# endGenMemberVars

  protected

    procedure   RestoreAttributes(AStream : TStream); override;
    procedure   SaveAttributes(AStream : TStream); override;

    //# genGetSetDeclarations
    function GetArray1(AIndex: Integer): Double;
    function GetArray1Count: Integer;
    function GetArray2(AIndex: Integer): String;
    procedure SetInt1(const AValue: Integer);
    procedure SetInt2(const AValue: Integer);
    procedure SetStr1(const AValue: String);
    procedure SetArray1(AIndex: Integer; const AValue: Double);
    procedure SetArray2(AIndex: Integer; const AValue: String);
    //# endGenGetSetDeclarations

  public
    constructor Create(AParent: TOTPersistent); override;
    destructor Destroy; override;

    //# genPublicDeclarations
    function AddArray1(AValue: Double): Integer;
    procedure DeleteArray1(AIndex: Integer);
    procedure ClearArray1;
    //# endGenPublicDeclarations

    procedure   RestoreYamlAttribute(AName, AValue : String; AIndex: Integer); override;
    procedure   SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    //# genProperty
    property int1: Integer read FInt1 write SetInt1;
    property int2: Integer read FInt2 write SetInt2;
    property str1: String read FStr1 write SetStr1;
    property array1[AIndex: Integer]: Double read GetArray1 write SetArray1;
    property array1Count: Integer read GetArray1Count;
    property array2[AIndex: Integer]: String read GetArray2 write SetArray2;
    //# endGenProperty
  end;


implementation

uses
  TLoggerUnit;

var
  log : ILogger;


{ TLeafClass }

constructor TLeafClass.Create(AParent: TOTPersistent);
begin
  inherited Create(AParent);
end;

destructor TLeafClass.Destroy;
begin
  inherited;
end;

procedure TLeafClass.RestoreYamlAttribute(AName, AValue : String; AIndex: Integer);
  begin
  //# genRestoreYamlVars
  if AName = 'int1' then
    FInt1 := StrToInteger(AValue)
  else
  if AName = 'int2' then
    FInt2 := StrToInteger(AValue)
  else
  if AName = 'str1' then
    FStr1 := StrToString(AValue)
  else
  if AName = 'array1-length' then
    SetLength(FArray1, StrToInteger(AValue))
  else
  if AName = 'array1' then
    FArray1[Integer(Ord(Low(FArray1))+AIndex)] := StrToDouble(AValue)
  else
  if AName = 'array2' then
    FArray2[Integer(Ord(Low(FArray2))+AIndex)] := StrToString(AValue)
  else
  //# endGenRestoreYamlVars
   inherited RestoreYamlAttribute(AName, AValue, AIndex);
  end;

procedure TLeafClass.RestoreAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genRestoreVars
  AStream.ReadBuffer(FInt1, sizeof(Integer));
  AStream.ReadBuffer(FInt2, sizeof(Integer));
  FStr1 := AStream.ReadAnsiString;
  SetLength(FArray1, AStream.ReadDWord);
  AStream.ReadBuffer(FArray1[Low(FArray1)], (Ord(High(FArray1))-Ord(Low(FArray1)) + 1)*sizeof(Double));
  for i := Ord(Low(FArray2)) to Ord(High(FArray2)) do
    FArray2[Integer(i)] := AStream.ReadAnsiString;
  //# endGenRestoreVars
  end;

procedure TLeafClass.SaveAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genSaveVars
  AStream.WriteBuffer(FInt1, sizeof(Integer));
  AStream.WriteBuffer(FInt2, sizeof(Integer));
  AStream.WriteAnsiString(FStr1);
  AStream.WriteDWord(Length(FArray1));
  AStream.WriteBuffer(FArray1[Low(FArray1)], (Ord(High(FArray1))-Ord(Low(FArray1)) + 1)*sizeof(Double));
  for i := Ord(Low(FArray2)) to Ord(High(FArray2)) do
    AStream.WriteAnsiString(FArray2[Integer(i)]);
  //# endGenSaveVars
  end;
  
procedure TLeafClass.SaveYamlAttributes(AEmitter : TYamlEmitter);
  var
    i: Integer;
  begin
  inherited;
  
  //# genSaveYamlVars
  SaveYamlInteger(AEmitter, 'int1', FInt1);
  SaveYamlInteger(AEmitter, 'int2', FInt2);
  SaveYamlString(AEmitter, 'str1', FStr1);
  SaveYamlInteger(AEmitter, 'array1-length', Length(FArray1));
  SaveYamlSequence(AEmitter, 'array1');
  for i := Ord(Low(FArray1)) to Ord(High(FArray1)) do
    SaveYamlSequenceDouble(AEmitter, FArray1[Integer(i)]);
  SaveYamlEndSequence(AEmitter);
  SaveYamlSequence(AEmitter, 'array2');
  for i := Ord(Low(FArray2)) to Ord(High(FArray2)) do
    SaveYamlSequenceString(AEmitter, FArray2[Integer(i)]);
  SaveYamlEndSequence(AEmitter);
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
// GENERATED METHOD - DO NOT EDIT
procedure TLeafClass.SetInt1(const AValue: Integer);
begin
  if AValue <> FInt1 then begin
    SetModified;
    FInt1 := AValue;
  end;
end;

// GENERATED METHOD - DO NOT EDIT
procedure TLeafClass.SetInt2(const AValue: Integer);
begin
  if AValue <> FInt2 then begin
    SetModified;
    FInt2 := AValue;
  end;
end;

// GENERATED METHOD - DO NOT EDIT
procedure TLeafClass.SetStr1(const AValue: String);
begin
  if AValue <> FStr1 then begin
    SetModified;
    FStr1 := AValue;
  end;
end;

// GENERATED METHOD - DO NOT EDIT
function TLeafClass.GetArray1(AIndex: Integer): Double;
begin
  Result := FArray1[AIndex];
end;

// GENERATED METHOD - DO NOT EDIT
procedure TLeafClass.SetArray1(AIndex: Integer; const AValue: Double);
begin
  if AValue <> FArray1[AIndex] then begin
    SetModified;
    FArray1[AIndex] := AValue;
  end;
end;

// GENERATED METHOD - DO NOT EDIT
function TLeafClass.GetArray1Count: Integer;
begin
  Result := Length(FArray1);
end;

// GENERATED METHOD - DO NOT EDIT
function TLeafClass.AddArray1(AValue: Double): Integer;
begin
  SetModified;
  SetLength(FArray1, Length(FArray1) + 1);
  FArray1[High(FArray1)] := AValue;
  Result := High(FArray1);
end;

// GENERATED METHOD - DO NOT EDIT
procedure TLeafClass.DeleteArray1(AIndex: Integer);
begin
  SetModified;
  Delete(FArray1, AIndex, 1);
end;

// GENERATED METHOD - DO NOT EDIT
procedure TLeafClass.ClearArray1;
begin
  SetModified;
  SetLength(FArray1, 0);
end;

// GENERATED METHOD - DO NOT EDIT
function TLeafClass.GetArray2(AIndex: Integer): String;
begin
  Result := FArray2[AIndex];
end;

// GENERATED METHOD - DO NOT EDIT
procedure TLeafClass.SetArray2(AIndex: Integer; const AValue: String);
begin
  if AValue <> FArray2[AIndex] then begin
    SetModified;
    FArray2[AIndex] := AValue;
  end;
end;

//# endGenGetSetMethods

initialization
  TLeafClass.RegisterClass;

  //log := Logger.GetInstance('TLeafClass');
end.
