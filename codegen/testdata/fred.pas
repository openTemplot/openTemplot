unit Fred;

{$MODE Delphi}

interface

uses
  Classes,
  SysUtils,
  OTPersistent;


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
    constructor Create(AParent: TOTPersistent); override;
    destructor Destroy; override;

    //# genPublicDeclarations
    //# endGenPublicDeclarations

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

constructor TFred.Create(AParent: TOTPersistent);
begin
  inherited Create(AParent);
end;

destructor TFred.Destroy;
begin
  inherited;
end;

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
  var
    i: Integer;
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
