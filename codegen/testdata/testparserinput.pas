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
