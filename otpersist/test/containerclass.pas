unit ContainerClass;

{$MODE Delphi}

interface

uses
  Classes,
  SysUtils,
  OTPersistent,
  OTPersistentList,
  OTYamlEmitter,
  LeafClass;


{# class TContainerClass
---
class: TContainerClass
attributes:
  - name: leafs
    type: TLeafClassOwningList
    owns: own
    access: [get]
...
}

type

  TContainerClass = class(TOTPersistent)
  private
    //# genMemberVars
    FLeafs: TOID;
    //# endGenMemberVars

  protected
    procedure Calculate; override;
    procedure RestoreAttributes(AStream : TStream); override;
    procedure SaveAttributes(AStream : TStream); override;

    //# genGetSetDeclarations
    function GetLeafs: TLeafClassOwningList;
    //# endGenGetSetDeclarations

  public
    constructor Create(AParent: TOTPersistent); override;
    destructor Destroy; override;

    //# genPublicDeclarations
    //# endGenPublicDeclarations

    procedure   RestoreYamlAttribute(AName, AValue : String; AIndex: Integer); override;
    procedure   SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    //# genProperty
    property leafs: TLeafClassOwningList read GetLeafs;
    //# endGenProperty
  end;

  TContainerClassOwningList = class(TOTOwningList<TContainerClass>);
  TContainerClassReferenceList = class(TOTReferenceList<TContainerClass>);


implementation

uses
  TLoggerUnit;

var
  log : ILogger;


{ TContainerClass }

constructor TContainerClass.Create(AParent: TOTPersistent);
begin
  inherited Create(AParent);
  //# genCreate
  FLeafs := 0;
  //# endGenCreate
end;

destructor TContainerClass.Destroy;
begin
  //# genDestroy
  GetLeafs.Free;
  //# endGenDestroy
  inherited;
end;

procedure TContainerClass.Calculate;
begin
  // Add your calculation code here, and cache the results...
end;

procedure TContainerClass.RestoreYamlAttribute(AName, AValue : String; AIndex: Integer);
begin
  //# genRestoreYamlVars
  if AName = 'leafs' then
    RestoreYamlObjectOwn(FLeafs, StrToInteger(AValue))
  else
  //# endGenRestoreYamlVars
    inherited RestoreYamlAttribute(AName, AValue, AIndex);
end;

procedure TContainerClass.RestoreAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genRestoreVars
  AStream.ReadBuffer(FLeafs, sizeof(TOID));
  //# endGenRestoreVars
  end;

procedure TContainerClass.SaveAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genSaveVars
  AStream.WriteBuffer(FLeafs, sizeof(TOID));
  //# endGenSaveVars
  end;
  
procedure TContainerClass.SaveYamlAttributes(AEmitter : TYamlEmitter);
  var
    i: Integer;
  begin
  inherited;
  
  //# genSaveYamlVars
  SaveYamlObject(AEmitter, 'leafs', FLeafs);
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
// GENERATED METHOD - DO NOT EDIT
function TContainerClass.GetLeafs: TLeafClassOwningList;
begin
  Result := TLeafClassOwningList(FromOID(FLeafs));
end;

//# endGenGetSetMethods

initialization
  TContainerClass.RegisterClass;

  //log := Logger.GetInstance('TContainerClass');
end.
