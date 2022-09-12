
(*  v1
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  OpenTemplot project contributors

    This program is free software: you may redistribute it and/or modify
    it under the terms of the GNU General Public Licence as published by
    the Free Software Foundation, either version 3 of the Licence, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public Licence for more details.

    You should have received a copy of the GNU General Public Licence
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

                >>>     NOTE TO DEVELOPERS     <<<
                     DO NOT EDIT THIS COMMENT
              It is inserted in this file by running
                  'python3 scripts/addComment.py'
         The original text lives in scripts/addComment.py.

====================================================================================
*)

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

    procedure SetXtbModifier(shovex: double);
    procedure SetOffsetModifier(offset: double);
    procedure SetCrabModifier(crab: double);
    procedure SetLengthModifier(length: double);
    procedure SetWidthModifier(Width: double);
    procedure SetAngleModifier(twist: double);

  public
    constructor Create;
    constructor CreateFrom(f: TShovedTimber); overload;

    property timberString: string Read FTimberString Write FTimberString;
    property shoveCode: TShoveCode Read FShoveCode Write FShoveCode;
    property xtbModifier: double Read FXtbModifier Write SetXtbModifier;
    property angleModifier: double Read FAngleModifier Write SetAngleModifier;
    property offsetModifier: double Read FOffsetModifier Write SetOffsetModifier;
    property lengthModifier: double Read FLengthModifier Write SetLengthModifier;
    property widthModifier: double Read FWidthModifier Write SetWidthModifier;
    property crabModifier: double Read FCrabModifier Write SetCrabModifier;


    procedure MakeShoved;
    procedure MakeOmit;

    procedure AdjustXtb(adjustment: double);
    procedure AdjustWidth(adjustment: double);
    procedure AdjustOffset(adjustment: double);
    procedure AdjustLength(adjustment: double);
    procedure AdjustAngle(adjustment: double);
    procedure AdjustCrab(adjustment: double);
    procedure Rescale(scaleRatio: double);
    function CanRestore: boolean;

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

procedure TShovedTimber.MakeShoved;
begin
  if FShoveCode = svcEmpty then begin
    FShoveCode := svcShove;
  end;
end;

procedure TShovedTimber.MakeOmit;
begin
  FShoveCode := svcOmit;
  FXtbModifier := 0;        // xtb modifier.
  FAngleModifier := 0;        // angle modifier.
  FOffsetModifier := 0;        // offset modifier (near end).
  FLengthModifier := 0;        // length modifier (far end).
  FWidthModifier := 0;        // width modifier (per side).
  FCrabModifier := 0;        // crab modifier.
end;

procedure TShovedTimber.SetXtbModifier(shovex: double);
begin
  FShoveCode := svcShove;
  FXtbModifier := shovex;
end;

procedure TShovedTimber.SetOffsetModifier(offset: double);
begin
  FShoveCode := svcShove;
  FOffsetModifier := offset;
end;

procedure TShovedTimber.SetCrabModifier(crab: double);
begin
  FShoveCode := svcShove;
  FCrabModifier := crab;
end;

procedure TShovedTimber.SetLengthModifier(length: double);
begin
  FShoveCode := svcShove;
  FLengthModifier := length;
end;

procedure TShovedTimber.SetWidthModifier(Width: double);
begin
  FShoveCode := svcShove;
  FWidthModifier := Width;
end;

procedure TShovedTimber.SetAngleModifier(twist: double);
begin
  FShoveCode := svcShove;
  FAngleModifier := twist;
end;

procedure TShovedTimber.AdjustXtb(adjustment: double);
begin
  FShoveCode := svcShove;
  FXtbModifier := FXtbModifier + adjustment;
end;

procedure TShovedTimber.AdjustWidth(adjustment: double);
begin
  FShoveCode := svcShove;
  FWidthModifier := FWidthModifier + adjustment;
end;

procedure TShovedTimber.AdjustOffset(adjustment: double);
begin
  FShoveCode := svcShove;
  FOffsetModifier := FOffsetModifier + adjustment;
end;

procedure TShovedTimber.AdjustLength(adjustment: double);
begin
  FShoveCode := svcShove;
  FLengthModifier := FLengthModifier + adjustment;
end;

procedure TShovedTimber.AdjustAngle(adjustment: double);
begin
  FShoveCode := svcShove;
  FAngleModifier := FAngleModifier + adjustment;
end;

procedure TShovedTimber.AdjustCrab(adjustment: double);
begin
  FShoveCode := svcShove;
  FCrabModifier := FCrabModifier + adjustment;
end;

procedure TShovedTimber.Rescale(scaleRatio: double);
begin
  FXtbModifier := FXtbModifier * scaleRatio;
  // angle modifier (no change).
  FOffsetModifier := FOffsetModifier * scaleRatio;
  FLengthModifier := FLengthModifier * scaleRatio;
  FWidthModifier := FWidthModifier * scaleRatio;
  FCrabModifier := FCrabModifier * scaleRatio;
end;

function TShovedTimber.CanRestore: boolean;
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
