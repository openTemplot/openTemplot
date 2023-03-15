unit OwningClass;

{$MODE Delphi}

interface

uses
  Classes,
  SysUtils,
  OTPersistent,
  OTPersistentList,
  OTYamlEmitter,
  LeafClass,
  ReferringClass;


{# class TOwningClass
---
class: TOwningClass
attributes:
- name: leaf
  type: TLeafClass
  owns: own
- name: referrer
  type: TReferringClass
  owns: own
...
}

type

  TOwningClass = class(TOTPersistent)
  private
    //# genMemberVars
    FLeaf: TOID;
    FReferrer: TOID;
    //# endGenMemberVars

    // calculated...
    FLeafSum: Integer;

  protected
    procedure Calculate; override;
    procedure RestoreAttributes(AStream : TStream); override;
    procedure SaveAttributes(AStream : TStream); override;

    //# genGetSetDeclarations
    function GetLeaf: TLeafClass;
    function GetReferrer: TReferringClass;
    procedure SetLeaf(const AValue: TLeafClass);
    procedure SetReferrer(const AValue: TReferringClass);
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
    property referrer: TReferringClass read GetReferrer write SetReferrer;
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

constructor TOwningClass.Create(AParent: TOTPersistent; AOID: TOID);
begin
  inherited Create(AParent, AOID);
  //# genCreate
  FLeaf := 0;
  FReferrer := 0;
  //# endGenCreate
end;

destructor TOwningClass.Destroy;
begin
  //# genDestroy
  SetOwned(FLeaf, nil);
  SetOwned(FReferrer, nil);
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

procedure TOwningClass.RestoreYamlAttribute(AName, AValue : String; AIndex: Integer; ALoader: TOTPersistentLoader);
begin
  //# genRestoreYamlVars
  if AName = 'leaf' then
    RestoreYamlObjectOwn(FLeaf, StrToInteger(AValue), ALoader)
  else
  if AName = 'referrer' then
    RestoreYamlObjectOwn(FReferrer, StrToInteger(AValue), ALoader)
  else
  //# endGenRestoreYamlVars
    inherited RestoreYamlAttribute(AName, AValue, AIndex, ALoader);
end;

procedure TOwningClass.RestoreAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genRestoreVars
  AStream.ReadBuffer(FLeaf, sizeof(TOID));
  AStream.ReadBuffer(FReferrer, sizeof(TOID));
  //# endGenRestoreVars
  end;

procedure TOwningClass.SaveAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genSaveVars
  AStream.WriteBuffer(FLeaf, sizeof(TOID));
  AStream.WriteBuffer(FReferrer, sizeof(TOID));
  //# endGenSaveVars
  end;
  
procedure TOwningClass.SaveYamlAttributes(AEmitter : TYamlEmitter);
  var
    i: Integer;
  begin
  inherited;
  
  //# genSaveYamlVars
  SaveYamlObject(AEmitter, 'leaf', FLeaf);
  SaveYamlObject(AEmitter, 'referrer', FReferrer);
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

// GENERATED METHOD - DO NOT EDIT
function TOwningClass.GetReferrer: TReferringClass;
begin
  Result := TReferringClass(FromOID(FReferrer));
end;

// GENERATED METHOD - DO NOT EDIT
procedure TOwningClass.SetReferrer(const AValue: TReferringClass);
begin
  SetOwned(FReferrer, AValue);
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
