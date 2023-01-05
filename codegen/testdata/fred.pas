unit Fred;

interface

uses
  Classes,
  SysUtils;


{# class TFred
---
class: TFred
attributes:
...
}

type

  TFred = class(TOTPersistent)
  private
    //# genMemberVars
    //# endGenMemberVars

  protected

    procedure   RestoreAttributes(AStream : TStream); override;
    procedure   SaveAttributes(AStream : TStream); override;

    //# genGetSetDeclarations
    //# endGenGetSetDeclarations

  public
    procedure   RestoreYamlAttribute(AName, AValue : String; AIndex: Integer); override;
    procedure   SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    //# genProperty
    //# endGenProperty
  end;


implementation

uses
  TLoggerUnit;

var
  log : ILogger;


{ TFred }

procedure TFred.RestoreYamlAttribute(AName, AValue : String; AIndex: Integer);
  begin
  //# genRestoreYamlVars
  //# endGenRestoreYamlVars
   inherited RestoreYamlAttribute(AName, AValue, AIndex);
  end;

procedure TFred.RestoreAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genRestoreVars
  //# endGenRestoreVars
  end;

procedure TFred.SaveAttributes(AStream : TStream);
  var
    i: Integer;
  begin
  inherited;

  //# genSaveVars
  //# endGenSaveVars
  end;
  
procedure TFred.SaveYamlAttributes(AEmitter : TYamlEmitter);
  begin
  inherited;
  
  //# genSaveYamlVars
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
//# endGenGetSetMethods

initialization
  TFred.RegisterClass;

  log := Logger.GetInstance('TFred');
end.
