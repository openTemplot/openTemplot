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
...
}

type

  TSample = class(TOTPersistent)
  private
    //# genMemberVars
    FTom: Integer;
    FDick: Double;
    FHarry: String;
    //# endGenMemberVars

  protected

    procedure   RestoreAttributes(AStream : TStream); override;
    procedure   SaveAttributes(AStream : TStream); override;

    //# genGetSetDeclarations
    procedure SetTom(const AValue: Integer);
    procedure SetDick(const AValue: Double);
    procedure SetHarry(const AValue: String);
    //# endGenGetSetDeclarations

  public
    procedure   RestoreYamlAttribute(AName, AValue : string); override;
    procedure   SaveYamlAttributes(AEmitter: TYamlEmitter); override;

    //# genProperty
    property tom: Integer read FTom write SetTom;
    property dick: Double read FDick write SetDick;
    property harry: String read FHarry write SetHarry;
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
  if AName = 'tom' then
    FTom := StrToInteger(AValue)
  else
  if AName = 'dick' then
    FDick := StrToDouble(AValue)
  else
  if AName = 'harry' then
    FHarry := StrToString(AValue)
  else
  //# endGenRestoreYamlVars
   inherited RestoreYamlAttribute(AName, AValue);
  end;

procedure TSample.RestoreAttributes(AStream : TStream);
  begin
  inherited;

  //# genRestoreVars
  RestoreInteger(AStream, FTom);
  RestoreDouble(AStream, FDick);
  RestoreString(AStream, FHarry);
  //# endGenRestoreVars
  end;

procedure TSample.SaveAttributes(AStream : TStream);
  begin
  inherited;

  //# genSaveVars
  SaveInteger(AStream, FTom);
  SaveDouble(AStream, FDick);
  SaveString(AStream, FHarry);
  //# endGenSaveVars

  end;
  
procedure TSample.SaveYamlAttributes(AEmitter : TYamlEmitter);
  begin
  inherited;
  //# genSaveYamlVars
  SaveYamlInteger(AEmitter, 'tom', FTom);
  SaveYamlDouble(AEmitter, 'dick', FDick);
  SaveYamlString(AEmitter, 'harry', FHarry);
  //# endGenSaveYamlVars
  end;

//# genGetSetMethods
// GENERATED METHOD - DO NOT EDIT
procedure TSample.SetTom(const AValue: Integer);
begin
  if AValue <> FTom then begin
    SetModified;
    FTom := AValue;
  end;
end;

// GENERATED METHOD - DO NOT EDIT
procedure TSample.SetDick(const AValue: Double);
begin
  if AValue <> FDick then begin
    SetModified;
    FDick := AValue;
  end;
end;

// GENERATED METHOD - DO NOT EDIT
procedure TSample.SetHarry(const AValue: String);
begin
  if AValue <> FHarry then begin
    SetModified;
    FHarry := AValue;
  end;
end;

//# endGenGetSetMethods

initialization
  TSample.RegisterClass;

  log := Logger.GetInstance('TSample');
end.
