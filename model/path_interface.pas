unit path_interface;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  point_ex;

type
{$interfaces corba}
  IPath = interface
    ['IPath']
    function get_curve_length: double;
    function get_minimum_radius: double;
    function get_xy(dist: double): Tpex;

    property curve_length: double read get_curve_length;
    property minimum_radius: double read get_minimum_radius;
  end;

implementation

end.

