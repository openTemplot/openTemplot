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
  Tcurve = class
  private
    Fnominal_radius: double;
    Fis_spiral: boolean;

  public
    property nominal_radius: double read Fnominal_radius write Fnominal_radius;
    property isSpiral: boolean read Fis_spiral write Fis_spiral;

    procedure do_calculation(distance, offset: double; out pt, direction: Tpex; out radius: double);
  end;

implementation

procedure Tcurve.do_calculation(distance, offset: double; out pt, direction: Tpex; out radius: double);
begin
 if (ABS(Fnominal_radius) > max_rad_test) and (not Fis_spiral) then
 begin
   // Straight line
   pt.set_xy(distance, offset);
   direction.set_xy(1, 0);
   radius := max_rad;
   Exit;
 end;

 pt.set_xy(0,0);
 direction.set_xy(1,0);
 radius := max_rad;
end;

end.

