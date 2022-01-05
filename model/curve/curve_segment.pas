unit curve_segment;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  point_ex;

type
  TCurveSegment = class
  private
    FSegmentLength: double;
  public
    property segmentLength: double Read FSegmentLength;

    constructor Create(segLength: double);
    procedure CalculateCurveAt(distance: double; out pt, direction: Tpex; out radius: double);
      virtual; abstract;
  end;

  TCurveSegmentList = class(TObjectList<TCurveSegment>)
  end;

implementation

//
//  TCurveSegment
//
constructor TCurveSegment.Create(segLength: double);
begin
  FSegmentLength := segLength;
end;

end.

