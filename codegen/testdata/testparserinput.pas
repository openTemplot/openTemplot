unit Sample;

interface

uses
  Classes,
  SysUtils;


{# class TSample
---
...
}

type

  TSample = class( TOTPersistent )
  private
    //# genMemberVars
    //# endGenMemberVars

  protected

    procedure   RestoreAttributes( stream : TStream ); override;
    procedure   SaveAttributes( stream : TStream ); override;

    //# genGetSetDeclarations
    //# endGenGetSetDeclarations

  public
    constructor Create(AParent: TOTPersistent); override;
    destructor Destroy; override;

    //# genPublicDeclarations
    //# endGenPublicDeclarations

    procedure   RestoreYamlAttribute( name, value : string ); override;
    procedure   SaveYamlAttributes( xml : TAttributesImpl ); override;

    //# genProperty
    //# endGenProperty
  end;


implementation

uses
  TLoggerUnit;

var
  log : ILogger;


{ TSample }

constructor TSample.Create(AParent: TOTPersistent);
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

procedure TSample.RestoreYamlAttribute( name, value : string );
  begin
  //# genRestoreYamlVars
  //# endGenRestoreYamlVars
  inherited;
  end;

procedure TSample.RestoreAttributes( stream : TStream );
  begin
  inherited;

  //# genRestoreVars
  //# endGenRestoreVars
  end;

procedure TSample.SaveAttributes( stream : TStream );
  begin
  inherited;

  //# genSaveVars
  //# endGenSaveVars

  end;

procedure TSample.SaveYamlAttributes( xml : TAttributesImpl );
  begin
  inherited;
  //# genSaveYamlVars
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
//# endGenGetSetMethods

initialization
  TSample.RegisterClass;

  log := Logger.GetInstace('TSample');
end.
