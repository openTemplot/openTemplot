unit curve_calculator;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  point_ex;

type
  TCurveCalculator = class
  public
    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
      virtual; abstract;
  end;

implementation

end.

