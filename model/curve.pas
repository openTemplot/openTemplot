unit curve;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  point_ex;

const
  // the maximum value for a radius
  // note: this is untyped, so the following typed consts will compile...
  maximum_radius_value = 1e08;

  // typed constant - maximum value for a radius.
  max_rad_limit: double = maximum_radius_value;

  // radius equivalent to "straight", maximum_radius_value - 5000 to allow for offsets without exceeding 1E8 max_rad_limit.
  max_rad: double = maximum_radius_value - 5000;

  // used for testing maximum radius (approx 62 miles rad).
  max_rad_test: double = maximum_radius_value - 10000;


type
  TCurve = class
  private
    FModified: boolean;
    FNominalRadius: double;
    FIsSpiral: boolean;
    FIsStraight: boolean;
    FIsSimpleCurve: boolean;

    FOrigin1: Tpex;

    procedure CheckModified;
    procedure CalculateCurveConstants;
    procedure SetNominalRadius(const newRadius: double);
    procedure SetIsSpiral(const newIsSpiral: boolean);
    function GetIsStraight: boolean;
    function GetIsSimpleCurve: boolean;

  public
    constructor Create;

    property nominalRadius: double Read FNominalRadius Write SetNominalRadius;
    property isSpiral: boolean Read FIsSpiral Write SetIsSpiral;
    property isStraight: boolean Read GetIsStraight;
    property isSimpleCurve: boolean Read GetIsSimpleCurve;

    procedure DoCalculation(distance, offset: double; out pt, direction: Tpex; out radius: double);
  end;

implementation

constructor TCurve.Create;
begin
  // default is straight line
  FModified := True;
  FNominalRadius := max_rad;
  FIsSpiral := False;
end;

procedure TCurve.SetNominalRadius(const newRadius: double);
begin
  if (newRadius <> FNominalRadius) then begin
    FModified := True;
    FNominalRadius := newRadius;
  end;
end;

procedure TCurve.SetIsSpiral(const newIsSpiral: boolean);
begin
  if (newIsSpiral <> FIsSpiral) then begin
    FModified := True;
    FIsSpiral := newIsSpiral;
  end;
end;

procedure TCurve.CheckModified;
begin
  if FModified then begin
    CalculateCurveConstants;
    FModified := False;
  end;
end;

procedure TCurve.CalculateCurveConstants;
begin
  FIsStraight := (Abs(FNominalRadius) > max_rad_test) and not FIsSpiral;
  FIsSimpleCurve := not FIsStraight and not FIsSpiral;

  if FIsStraight then begin
    // nothing special to calculate for a straight line
  end
  else
  if FIsSimpleCurve then begin
    // calculate origin of circle
    FOrigin1.set_xy(0, FNominalRadius);
  end
  else begin
  end;
end;

procedure TCurve.DoCalculation(distance, offset: double; out pt, direction: Tpex;
  out radius: double);
var
  turnAngle: double;
  actualRadius: double;
begin
  CheckModified;

  if FIsStraight then begin
    // Straight line
    pt.set_xy(distance, offset);
    direction.set_xy(1, 0);
    radius := max_rad;
    Exit;
  end;

  // simple curve
  turnAngle := distance / FNominalRadius;
  direction.set_xy(cos(turnAngle), sin(turnAngle));

  actualRadius := FNominalRadius - offset;

  pt.set_xy( FOrigin1.x + actualRadius * direction.y, FOrigin1.y - actualRadius * direction.x);
  radius := FNominalRadius;
end;

function TCurve.GetIsStraight: boolean;
begin
  CheckModified;
  Result := FIsStraight;
end;

function TCurve.GetIsSimpleCurve: boolean;
begin
  CheckModified;
  Result := FIsSimpleCurve;
end;

end.


