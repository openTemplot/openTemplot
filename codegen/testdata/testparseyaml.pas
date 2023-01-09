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
    type: integer
  - name: dick
    type: double
  - name: harry
    type: string
    comment: A single line comment
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
