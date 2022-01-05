unit curve;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  point_ex,
  curve_calculator;

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
  TCurve = class
  private
    FModified: boolean;
    FNominalRadius: double;
    FNominalRadius2: double;
    FDistanceToTransition: double;
    FTransitionLength: double;
    FIsSpiral: boolean;
    FIsStraight: boolean;
    FIsSimpleCurve: boolean;

    FCurveCalculator: TCurveCalculator;

    procedure UpdateIfModified;
    procedure CreateCurveCalculator;
    procedure SetNominalRadius(const newRadius: double);
    procedure SetNominalRadius2(const newRadius: double);
    procedure SetDistanceToTransition(const newDistance: double);
    procedure SetTransitionLength(const newLength: double);
    procedure SetIsSpiral(const newIsSpiral: boolean);
    function GetIsStraight: boolean;
    function GetIsSimpleCurve: boolean;

  public
    constructor Create;

    property nominalRadius: double Read FNominalRadius Write SetNominalRadius;
    property nominalRadius2: double Read FNominalRadius2 Write SetNominalRadius2;
    property distanceToTransition: double Read FDistanceToTransition Write SetDistanceToTransition;
    property transitionLength: double Read FTransitionLength Write SetTransitionLength;
    property isSpiral: boolean Read FIsSpiral Write SetIsSpiral;
    property isStraight: boolean Read GetIsStraight;
    property isSimpleCurve: boolean Read GetIsSimpleCurve;

    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
  end;

implementation

uses
  math;

//
// TCurve
//
constructor TCurve.Create;
begin
  // default is straight line
  FModified := True;
  FNominalRadius := max_rad;
  FNominalRadius2 := max_rad;
  FDistanceToTransition := 0;
  FTransitionLength := 100;
  FIsSpiral := False;
end;

procedure TCurve.SetNominalRadius(const newRadius: double);
begin
  if (newRadius <> FNominalRadius) then begin
    FModified := True;
    FNominalRadius := newRadius;
  end;
end;

procedure TCurve.SetNominalRadius2(const newRadius: double);
begin
  if (newRadius <> FNominalRadius2) then begin
    FModified := True;
    FNominalRadius2 := newRadius;
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
var
  transitionStartPoint: Tpex;
  transitionStartDirection: Tpex;
  transitionEndPoint: Tpex;
  transitionEndDirection: Tpex;
  radius: double;
begin
  FIsStraight := (Abs(FNominalRadius) > max_rad_test) and not FIsSpiral;
  FIsSimpleCurve := not FIsStraight and not FIsSpiral;

  FreeAndNil(FCurveCalculator);


  FCurveCalculator := TCurveSegmentCalculator.Create(self);
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

function TCurve.GetIsStraight: boolean;
begin
  UpdateIfModified;
  Result := FIsStraight;
end;

function TCurve.GetIsSimpleCurve: boolean;
begin
  UpdateIfModified;
  Result := FIsSimpleCurve;
end;

end.
