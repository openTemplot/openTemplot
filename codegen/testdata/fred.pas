unit Fred;

interface

uses
  Classes,
  SysUtils;


{# class TFred
+++
...
}

type

  TFred = class( TOTPersistent )
  private
    //# genMemberVars
    //# endGenMemberVars

    //# genCollections
    //# endGenCollections

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


{ TFred }

procedure TFred.RestoreYamlAttribute( name, value : string );
  begin
  //# genRestoreYamlVars
  //# endGenRestoreYamlVars
  inherited;
  end;

procedure TFred.RestoreAttributes( stream : TStream );
  begin
  inherited;

  //# genRestoreVars
  //# endGenRestoreVars
  end;

procedure TFred.SaveAttributes( stream : TStream );
  begin
  inherited;

  //# genSaveVars
  //# endGenSaveVars

  end;

procedure TFred.SaveYamlAttributes( xml : TAttributesImpl );
  begin
  inherited;
  //# genSaveYamlVars
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
//# endGenGetSetMethods

initialization
  TFred.RegisterClass;

  log := Logger.GetInstace('TFred');
end.
