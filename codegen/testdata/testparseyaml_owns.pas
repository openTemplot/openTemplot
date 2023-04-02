unit Sample;

interface

uses
  Classes,
  SysUtils;


{# class TSample
---
class: TSample
attributes:
  - name: tom
    type: TTomOwningList
    access: [get]
    owns: create
  - name: dick
    type: TOther
    owns: ref
  - name: harry
    type: TSomethingElse
    owns: own
...
}

type

  TSample = class(TOTPersistent)
  private
    //# genMemberVars
    //# endGenMemberVars

  protected

    procedure   RestoreAttributes(AStream : TStream); override;
    procedure   SaveAttributes(AStream : TStream); override;

    //# genGetSetDeclarations
    //# endGenGetSetDeclarations

  public
    constructor Create(AParent: TOTPersistent; AOID: TOID = 0); override;
    destructor Destroy; override;

    //# genPublicDeclarations
    //# endGenPublicDeclarations

    procedure   RestoreYamlAttribute(AName, AValue : string); override;
    procedure   SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    //# genProperty
    //# endGenProperty
  end;


implementation

uses
  TLoggerUnit;

var
  log : ILogger;


{ TSample }

constructor TSample.Create(AParent: TOTPersistent; AOID: TOID);
begin
  inherited Create(AParent);
  //# genCreate
  //# endGenCreate
end;

destructor TSample.Destroy;
begin
  //# genDestroy
  //# endGenDestroy
  inherited;
end;

procedure TSample.RestoreYamlAttribute(AName, AValue : string);
  begin
  //# genRestoreYamlVars
  //# endGenRestoreYamlVars
   inherited RestoreYamlAttribute(AName, AValue);
  end;

procedure TSample.RestoreAttributes(AStream : TStream);
  begin
  inherited;

  //# genRestoreVars
  //# endGenRestoreVars
  end;

procedure TSample.SaveAttributes(AStream : TStream);
  begin
  inherited;

  //# genSaveVars
  //# endGenSaveVars

  end;
  
procedure TSample.SaveYamlAttributes(AEmitter : TYamlEmitter);
  begin
  inherited;
  //# genSaveYamlVars
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
//# endGenGetSetMethods

initialization
  TSample.RegisterClass;

  log := Logger.GetInstance('TSample');
end.
