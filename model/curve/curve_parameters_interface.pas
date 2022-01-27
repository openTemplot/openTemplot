unit curve_parameters_interface;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  ESlewMode = (eSM_Cosine, eSM_TanH);

  {$interfaces corba}
  ICurveParameters = interface
    ['ICurveParameters']

    function GetIsSpiral: boolean;
    function GetNominalRadius: double;
    function GetNominalRadius2: double;
    function GetDistanceToTransition: double;
    function GetTransitionLength: double;
    function GetIsSlewing: boolean;
    function GetDistanceToStartOfSlew: double;
    function GetSlewLength: double;
    function GetSlewAmount: double;
    function GetSlewMode: ESlewMode;
    function GetSlewFactor: double;

    property isSpiral: boolean Read GetIsSpiral;
    property nominalRadius: double Read GetNominalRadius;
    property nominalRadius2: double Read GetNominalRadius2;
    property distanceToTransition: double Read GetDistanceToTransition;
    property transitionLength: double Read GetTransitionLength;

    property isSlewing: boolean read GetIsSlewing;
    property distanceToStartOfSlew: double read GetDistanceToStartOfSlew;
    property slewLength: double read GetSlewLength;
    property slewAmount: double read GetSlewAmount;
    property slewMode: ESlewMode read GetSlewMode;
    property slewFactor: double read GetSlewFactor;

  end;

implementation

end.

