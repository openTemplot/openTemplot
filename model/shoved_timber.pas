unit shoved_timber;

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  TShoveCode = (svcOmit = -1, svcEmpty = 0, svcShove = 1);

  TShovedTimber = class             // v: 0.71.a  27-4-01.

  private
    FTimberString: string;
    FShoveCode: TShoveCode;
    FXtbModifier: double;
    FAngleModifier: double;
    FOffsetModifier: double;
    FLengthModifier: double;
    FWidthModifier: double;
    FCrabModifier: double;

  public
    constructor Create;
    constructor CreateFrom(f: TShovedTimber); overload;

    property timberString: string Read FTimberString Write FTimberString;
    property shoveCode: TShoveCode Read FShoveCode Write FShoveCode;
    property xtbModifier: double Read FXtbModifier Write FXtbModifier;
    property angleModifier: double Read FAngleModifier Write FAngleModifier;
    property offsetModifier: double Read FOffsetModifier Write FOffsetModifier;
    property lengthModifier: double Read FLengthModifier Write FLengthModifier;
    property widthModifier: double Read FWidthModifier Write FWidthModifier;
    property crabModifier: double Read FCrabModifier Write FCrabModifier;


    procedure make_shoved;
    procedure make_omit;

    procedure set_shovex(shovex: double);
    procedure set_offset(offset: double);
    procedure set_crab(crab: double);
    procedure set_length(length: double);
    procedure set_width(Width: double);
    procedure set_twist(twist: double);

    procedure adjust_shovex(adjustment: double);
    procedure adjust_width(adjustment: double);
    procedure adjust_offset(adjustment: double);
    procedure adjust_length(adjustment: double);
    procedure adjust_twist_degrees(adjustment: double);
    procedure adjust_crab(adjustment: double);
    procedure rescale(mod_scale_ratio: double);
    function can_restore: boolean;

  end;//class


  TShovedTimberList = class(TObjectList<TShovedTimber>)
  public
    procedure CopyFrom(from: TShovedTimberList);
  end;


implementation

constructor TShovedTimber.Create;
begin
  FTimberString := '';
  FShoveCode := svcEmpty;
  FXtbModifier := 0;        // xtb modifier.
  FAngleModifier := 0;        // angle modifier.
  FOffsetModifier := 0;        // offset modifier (near end).
  FLengthModifier := 0;        // length modifier (far end).
  FWidthModifier := 0;        // width modifier (per side).
  FCrabModifier := 0;        // crab modifier.
end;

constructor TShovedTimber.CreateFrom(f: TShovedTimber);
begin
  FTimberString := f.FTimberString;
  FShoveCode := f.shoveCode;
  FXtbModifier := f.xtbModifier;
  FAngleModifier := f.angleModifier;
  FOffsetModifier := f.offsetModifier;
  FLengthModifier := f.lengthModifier;
  FWidthModifier := f.widthModifier;
  FCrabModifier := f.crabModifier;
end;

procedure TShovedTimber.make_shoved;
begin
  if FShoveCode = svcEmpty then begin
    FShoveCode := svcShove;
  end;
end;

procedure TShovedTimber.make_omit;
begin
  FShoveCode := svcOmit;
  FXtbModifier := 0;        // xtb modifier.
  FAngleModifier := 0;        // angle modifier.
  FOffsetModifier := 0;        // offset modifier (near end).
  FLengthModifier := 0;        // length modifier (far end).
  FWidthModifier := 0;        // width modifier (per side).
  FCrabModifier := 0;        // crab modifier.
end;

procedure TShovedTimber.set_shovex(shovex: double);
begin
  FShoveCode := svcShove;
  FXtbModifier := shovex;
end;

procedure TShovedTimber.set_offset(offset: double);
begin
  FShoveCode := svcShove;
  FOffsetModifier := offset;
end;

procedure TShovedTimber.set_crab(crab: double);
begin
  FShoveCode := svcShove;
  FCrabModifier := crab;
end;

procedure TShovedTimber.set_length(length: double);
begin
  FShoveCode := svcShove;
  FLengthModifier := length;
end;

procedure TShovedTimber.set_width(Width: double);
begin
  FShoveCode := svcShove;
  FWidthModifier := Width;
end;

procedure TShovedTimber.set_twist(twist: double);
begin
  FShoveCode := svcShove;
  FAngleModifier := twist;
end;

procedure TShovedTimber.adjust_shovex(adjustment: double);
begin
  FShoveCode := svcShove;
  FXtbModifier := FXtbModifier + adjustment;
end;

procedure TShovedTimber.adjust_width(adjustment: double);
begin
  FShoveCode := svcShove;
  FWidthModifier := FWidthModifier + adjustment;
end;

procedure TShovedTimber.adjust_offset(adjustment: double);
begin
  FShoveCode := svcShove;
  FOffsetModifier := FOffsetModifier + adjustment;
end;

procedure TShovedTimber.adjust_length(adjustment: double);
begin
  FShoveCode := svcShove;
  FLengthModifier := FLengthModifier + adjustment;
end;

procedure TShovedTimber.adjust_twist_degrees(adjustment: double);
begin
  FShoveCode := svcShove;
  FAngleModifier := FAngleModifier + adjustment * Pi / 180;
end;

procedure TShovedTimber.adjust_crab(adjustment: double);
begin
  FShoveCode := svcShove;
  FCrabModifier := FCrabModifier + adjustment;
end;

procedure TShovedTimber.rescale(mod_scale_ratio: double);
begin
  FXtbModifier := FXtbModifier * mod_scale_ratio;        // xtb modifier.
  //sv_k                             // angle modifier (no change).
  FOffsetModifier := FOffsetModifier * mod_scale_ratio;        // offset modifier (near end).
  FLengthModifier := FLengthModifier * mod_scale_ratio;        // length modifier (far end).
  FWidthModifier := FWidthModifier * mod_scale_ratio;        // width modifier (per side).
end;

function TShovedTimber.can_restore: boolean;
begin
  Result := (FShoveCode = svcOmit) or
    (((FXtbModifier <> 0)       // xtb modifier.
    or (FAngleModifier <> 0)       // angle modifier.
    or (FOffsetModifier <> 0)       // offset modifier (near end).
    or (FLengthModifier <> 0)       // length modifier (far end).
    or (FWidthModifier <> 0)       // width modifier (per side).
    or (FCrabModifier <> 0)       // crab modifier.  0.78.c  01-02-03.

    ) and (FShoveCode = svcShove));
end;


procedure TShovedTimberList.CopyFrom(from: TShovedTimberList);
var
  f: TShovedTimber;
  t: TShovedTimber;
begin
  Clear;

  if not Assigned(from) then
    EXIT;  // return empty list.

  for f in from do begin
    t := TShovedTimber.CreateFrom(f);
    Add(t);
  end;//next
end;

end.
