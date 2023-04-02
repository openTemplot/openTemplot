unit ReferringClass;

{$MODE Delphi}

interface

uses
  Classes,
  SysUtils,
  OTPersistent,
  OTPersistentList,
  OTYamlEmitter,
  LeafClass;


{# class TReferringClass
---
class: TReferringClass
attributes:
- name: leaf
  type: TLeafClass
  owns: ref
...
}

type

  TReferringClass = class(TOTPersistent)
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
    constructor Create(AParent: TOTPersistent; AOID: TOID = 0); override;
    destructor Destroy; override;

    //# genPublicDeclarations
    //# endGenPublicDeclarations

    procedure   RestoreYamlAttribute(AName, AValue : String; AIndex: Integer; ALoader: TOTPersistentLoader); override;
    procedure   SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    //# genProperty
    property leaf: TLeafClass read GetLeaf write SetLeaf;
    //# endGenProperty

    property leafSum: Integer Read GetLeafSum;
  end;

  TReferringClassOwningList = class(TOTOwningList<TReferringClass>);
  TReferringClassReferenceList = class(TOTReferenceList<TReferringClass>);


implementation

uses
  TLoggerUnit;

var
  log : ILogger;


{ TReferringClass }

constructor TReferringClass.Create(AParent: TOTPersistent; AOID: TOID);
begin
  inherited Create(AParent, AOID);
  //# genCreate
  FLeaf := 0;
  //# endGenCreate
end;

destructor TReferringClass.Destroy;
begin
  //# genDestroy
  SetReference(FLeaf, nil);
  //# endGenDestroy
  inherited;
end;

procedure TReferringClass.Calculate;
begin
  // Add your calculation code here, and cache the results...
  if Assigned(leaf) then
    FLeafSum := leaf.sum
  else
    FLeafSum := 0;
end;

procedure TReferringClass.RestoreYamlAttribute(AName, AValue : String; AIndex: Integer; ALoader: TOTPersistentLoader);
begin
  //# genRestoreYamlVars
  if AName = 'leaf' then
    RestoreYamlObjectRef(FLeaf, StrToInteger(AValue), ALoader)
  else
  //# endGenRestoreYamlVars
    inherited RestoreYamlAttribute(AName, AValue, AIndex, ALoader);
end;

procedure TReferringClass.RestoreAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genRestoreVars
  AStream.ReadBuffer(FLeaf, sizeof(TOID));
  //# endGenRestoreVars
  end;

procedure TReferringClass.SaveAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genSaveVars
  AStream.WriteBuffer(FLeaf, sizeof(TOID));
  //# endGenSaveVars
  end;
  
procedure TReferringClass.SaveYamlAttributes(AEmitter : TYamlEmitter);
  var
    i: Integer;
  begin
  inherited;
  
  //# genSaveYamlVars
  SaveYamlObjectReference(AEmitter, 'leaf', FLeaf);
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
// GENERATED METHOD - DO NOT EDIT
function TReferringClass.GetLeaf: TLeafClass;
begin
  Result := TLeafClass(FromOID(FLeaf));
end;

// GENERATED METHOD - DO NOT EDIT
procedure TReferringClass.SetLeaf(const AValue: TLeafClass);
begin
  SetReference(FLeaf, AValue);
end;

//# endGenGetSetMethods

function TReferringClass.GetLeafSum: Integer;
begin
  CheckCalculated;

  Result := FLeafSum;
end;

initialization
  TReferringClass.RegisterClass;
  TReferringClassOwningList.RegisterClass;
  TReferringClassReferenceList.RegisterClass;

  //log := Logger.GetInstance('TReferringClass');
end.
