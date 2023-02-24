unit OwningClass;

{$MODE Delphi}

interface

uses
  Classes,
  SysUtils,
  OTPersistent,
  OTPersistentList,
  OTYamlEmitter,
  LeafClass;


{# class TOwningClass
---
class: TOwningClass
attributes:
- name: leaf
  type: TLeafClass
  owns: own
...
}

type

  TOwningClass = class(TOTPersistent)
  private
    //# genMemberVars
    FLeaf: TOID;
    //# endGenMemberVars

    // calculated...
    FLeafSum: Integer;

  protected
    procedure Calculate; override;
    procedure RestoreAttributes(AStream : TStream); override;
    procedure SaveAttributes(AStream : TStream); override;

    //# genGetSetDeclarations
    function GetLeaf: TLeafClass;
    procedure SetLeaf(const AValue: TLeafClass);
    //# endGenGetSetDeclarations

    function GetLeafSum: Integer;

  public
    constructor Create(AParent: TOTPersistent); override;
    destructor Destroy; override;

    //# genPublicDeclarations
    //# endGenPublicDeclarations

    procedure   RestoreYamlAttribute(AName, AValue : String; AIndex: Integer); override;
    procedure   SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    //# genProperty
    property leaf: TLeafClass read GetLeaf write SetLeaf;
    //# endGenProperty

    property leafSum: Integer Read GetLeafSum;
  end;

  TOwningClassOwningList = class(TOTOwningList<TOwningClass>);
  TOwningClassReferenceList = class(TOTReferenceList<TOwningClass>);


implementation

uses
  TLoggerUnit;

var
  log : ILogger;


{ TOwningClass }

constructor TOwningClass.Create(AParent: TOTPersistent);
begin
  inherited Create(AParent);
  //# genCreate
  //# endGenCreate
end;

destructor TOwningClass.Destroy;
begin
  //# genDestroy
  //# endGenDestroy
  inherited;
end;

procedure TOwningClass.Calculate;
begin
  inherited;

  // Add your calculation code here, and cache the results...
  if Assigned(leaf) then
    FLeafSum := leaf.sum
  else
    FLeafSum := 0;
end;

procedure TOwningClass.RestoreYamlAttribute(AName, AValue : String; AIndex: Integer);
begin
  //# genRestoreYamlVars
  if AName = 'leaf' then
    RestoreYamlObjectOwn(FLeaf, StrToInteger(AValue))
  else
  //# endGenRestoreYamlVars
    inherited RestoreYamlAttribute(AName, AValue, AIndex);
end;

procedure TOwningClass.RestoreAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genRestoreVars
  AStream.ReadBuffer(FLeaf, sizeof(TOID));
  //# endGenRestoreVars
  end;

procedure TOwningClass.SaveAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genSaveVars
  AStream.WriteBuffer(FLeaf, sizeof(TOID));
  //# endGenSaveVars
  end;
  
procedure TOwningClass.SaveYamlAttributes(AEmitter : TYamlEmitter);
  var
    i: Integer;
  begin
  inherited;
  
  //# genSaveYamlVars
  SaveYamlObject(AEmitter, 'leaf', FLeaf);
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
// GENERATED METHOD - DO NOT EDIT
function TOwningClass.GetLeaf: TLeafClass;
begin
  Result := TLeafClass(FromOID(FLeaf));
end;

// GENERATED METHOD - DO NOT EDIT
procedure TOwningClass.SetLeaf(const AValue: TLeafClass);
begin
  SetOwned(FLeaf, AValue);
end;

//# endGenGetSetMethods

function TOwningClass.GetLeafSum: Integer;
begin
  CheckCalculated;

  Result := FLeafSum;
end;

initialization
  TOwningClass.RegisterClass;
  TOwningClassOwningList.RegisterClass;
  TOwningClassReferenceList.RegisterClass;

  //log := Logger.GetInstance('TOwningClass');
end.
