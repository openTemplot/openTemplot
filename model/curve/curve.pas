
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

====================================================================================
*)

unit curve;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  point_ex,
  curve_calculator,
  curve_parameters_interface;

const
  maximum_segment_length = 1e100;

  // the maximum value for a radius (approx 62 miles rad)
  // this dimension is in millimetres, and is independent of any scale settings
  // in openTemplot
  //
  // note: this is untyped, so the following typed consts will compile...
  maximum_radius_value = 1e08;

  // typed constant - maximum value for a radius.
  max_rad_limit: double = maximum_radius_value;

  // radius equivalent to "straight", maximum_radius_value - 5000 to allow for offsets without exceeding 1E8 max_rad_limit.
  max_rad: double = maximum_radius_value - 5000;

  // used for testing maximum radius.
  max_rad_test: double = maximum_radius_value - 10000;


type

  { TCurve }

  TCurve = class(ICurveParameters)
  private
    FModified: boolean;
    FFixedRadius: double;
    FTransitionStartRadius: double;
    FTransitionEndRadius: double;
    FDistanceToTransition: double;
    FTransitionLength: double;
    FIsSpiral: boolean;

    FIsSlewing: boolean;
    FDistanceToStartOfSlew: double;
    FSlewLength: double;
    FSlewAmount: double;
    FSlewMode: ESlewMode;
    FSlewFactor: double;

    FCurveCalculator: TCurveCalculator;

    procedure UpdateIfModified;
    procedure CreateCurveCalculator;
    procedure SetFixedRadius(const newRadius: double);
    procedure SetTransitionStartRadius(const newRadius: double);
    procedure SetTransitionEndRadius(const newRadius: double);
    procedure SetDistanceToTransition(const newDistance: double);
    procedure SetTransitionLength(const newLength: double);
    procedure SetIsSpiral(const newIsSpiral: boolean);
    function GetIsSpiral: boolean;
    function GetFixedRadius: double;
    function GetTransitionStartRadius: double;
    function GetTransitionEndRadius: double;
    function GetDistanceToTransition: double;
    function GetTransitionLength: double;
    function GetIsSlewing: boolean;
    function GetDistanceToStartOfSlew: double;
    function GetSlewLength: double;
    function GetSlewAmount: double;
    function GetSlewMode: ESlewMode;
    function GetSlewFactor: double;
    procedure SetIsSlewing(const newIsSlewing: boolean);
    procedure SetDistanceToStartOfSlew(const newDistance: double);
    procedure SetSlewLength(const newLength: double);
    procedure SetSlewAmount(const newAmount: double);
    procedure SetSlewMode(const newMode: ESlewMode);
    procedure SetSlewFactor(const newFactor: double);

  protected
    property curveCalculator: TCurveCalculator read FCurveCalculator;

  public
    constructor Create;

    property fixedRadius: double Read GetFixedRadius Write SetFixedRadius;
    property transitionStartRadius: double Read GetTransitionStartRadius Write SetTransitionStartRadius;
    property transitionEndRadius: double Read GetTransitionEndRadius Write SetTransitionEndRadius;
    property distanceToTransition: double Read GetDistanceToTransition
      Write SetDistanceToTransition;
    property transitionLength: double Read GetTransitionLength Write SetTransitionLength;
    property isSpiral: boolean Read GetIsSpiral Write SetIsSpiral;

    property isSlewing: boolean Read GetIsSlewing Write SetIsSlewing;
    property distanceToStartOfSlew: double Read GetDistanceToStartOfSlew
      Write SetDistanceToStartOfSlew;
    property slewLength: double Read GetSlewLength Write SetSlewLength;
    property slewAmount: double Read GetSlewAmount Write SetSlewAmount;
    property slewMode: ESlewMode Read GetSlewMode Write SetSlewMode;
    property slewFactor: double Read GetSlewFactor Write SetSlewFactor;

    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);

    procedure CopyFrom(from: TCurve);
  end;

implementation

uses
  Math,
  curve_segment_calculator,
  slew_calculator;

//
// TCurve
//
constructor TCurve.Create;
begin
  // default is straight line
  FModified := True;
  FFixedRadius := max_rad;
  FTransitionStartRadius := max_rad;
  FTransitionEndRadius := max_rad;
  FDistanceToTransition := 0;
  FTransitionLength := 100;
  FIsSpiral := False;
end;

procedure TCurve.CopyFrom(from: TCurve);
begin
  FFixedRadius := from.fixedRadius;
  FTransitionStartRadius := from.transitionStartRadius;
  FTransitionEndRadius := from.transitionEndRadius;
  FDistanceToTransition := from.distanceToTransition;
  FTransitionLength := from.transitionLength;
  FIsSpiral := from.isSpiral;

  FIsSlewing := from.isSlewing;
  FDistanceToStartOfSlew := from.distanceToStartOfSlew;
  FSlewLength := from.slewLength;
  FSlewAmount := from.slewAmount;
  FSlewMode := from.slewMode;
  FSlewFactor := from.slewFactor;

  FModified := true;
end;

procedure TCurve.SetFixedRadius(const newRadius: double);
begin
  if (newRadius <> FFixedRadius) then begin
    FModified := True;
    FFixedRadius := newRadius;
  end;
end;

procedure TCurve.SetTransitionStartRadius(const newRadius: double);
begin
  if (newRadius <> FTransitionStartRadius) then begin
    FModified := True;
    FTransitionStartRadius := newRadius;
  end;
end;

procedure TCurve.SetTransitionEndRadius(const newRadius: double);
begin
  if (newRadius <> FTransitionEndRadius) then begin
    FModified := True;
    FTransitionEndRadius := newRadius;
  end;
end;

procedure TCurve.SetDistanceToTransition(const newDistance: double);
begin
  if (newDistance <> FDistanceToTransition) then begin
    FModified := True;
    FDistanceToTransition := newDistance;
  end;
end;

procedure TCurve.SetTransitionLength(const newLength: double);
begin
  if (newLength <> FTransitionLength) then begin
    FModified := True;
    FTransitionLength := newLength;
  end;
end;

procedure TCurve.SetIsSpiral(const newIsSpiral: boolean);
begin
  if (newIsSpiral <> FIsSpiral) then begin
    FModified := True;
    FIsSpiral := newIsSpiral;
  end;
end;

procedure TCurve.UpdateIfModified;
begin
  if FModified then begin
    FModified := False;
    CreateCurveCalculator;
  end;
end;

procedure TCurve.CreateCurveCalculator;
begin
  FreeAndNil(FCurveCalculator);
  FCurveCalculator := TCurveSegmentCalculator.Create(self);

  if FIsSlewing then begin
    FCurveCalculator := TSlewCalculator.Create(self, FCurveCalculator);
  end;
end;

procedure TCurve.CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
begin
  UpdateIfModified;

  if Assigned(FCurveCalculator) then
    FCurveCalculator.CalculateCurveAt(distance, pt, direction, radius)
  else begin
    pt.set_xy(NaN, NaN);
    direction.set_xy(NaN, NaN);
    radius := NaN;
  end;
end;

function TCurve.GetIsSpiral: boolean;
begin
  Result := FIsSpiral;
end;

function TCurve.GetFixedRadius: double;
begin
  Result := FFixedRadius;
end;

function TCurve.GetTransitionStartRadius: double;
begin
  Result := FTransitionStartRadius;
end;

function TCurve.GetTransitionEndRadius: double;
begin
  Result := FTransitionEndRadius;
end;

function TCurve.GetDistanceToTransition: double;
begin
  Result := FDistanceToTransition;
end;

function TCurve.GetTransitionLength: double;
begin
  Result := FTransitionLength;
end;

function TCurve.GetIsSlewing: boolean;
begin
  Result := FIsSlewing;
end;

function TCurve.GetDistanceToStartOfSlew: double;
begin
  Result := FDistanceToStartOfSlew;
end;

function TCurve.GetSlewLength: double;
begin
  Result := FSlewLength;
end;

function TCurve.GetSlewAmount: double;
begin
  Result := FSlewAmount;
end;

function TCurve.GetSlewMode: ESlewMode;
begin
  Result := FSlewMode;
end;

function TCurve.GetSlewFactor: double;
begin
  Result := FSlewFactor;
end;

procedure TCurve.SetIsSlewing(const newIsSlewing: boolean);
begin
  if FIsSlewing <> newIsSlewing then begin
    FModified := True;
    FIsSlewing := newIsSlewing;
  end;
end;

procedure TCurve.SetDistanceToStartOfSlew(const newDistance: double);
begin
  if FDistanceToStartOfSlew <> newDistance then begin
    FModified := True;
    FDistanceToStartOfSlew := newDistance;
  end;
end;

procedure TCurve.SetSlewLength(const newLength: double);
begin
  if FSlewLength <> newLength then begin
    FModified := True;
    FSlewLength := newLength;
  end;
end;

procedure TCurve.SetSlewAmount(const newAmount: double);
begin
  if FSlewAmount <> newAmount then begin
    FModified := True;
    FSlewAmount := newAmount;
  end;
end;

procedure TCurve.SetSlewMode(const newMode: ESlewMode);
begin
  if FSlewMode <> newMode then begin
    FModified := True;
    FSlewMode := newMode;
  end;
end;

procedure TCurve.SetSlewFactor(const newFactor: double);
begin
  if FSlewFactor <> newFactor then begin
    FModified := True;
    FSlewFactor := newFactor;
  end;
end;

end.
