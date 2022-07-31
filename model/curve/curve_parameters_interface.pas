unit curve_parameters_interface;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  ESlewMode = (smCosine, smTanH);

  {$interfaces corba}
  ICurveParameters = interface
    ['ICurveParameters']

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

    property isSpiral: boolean Read GetIsSpiral;
    property fixedRadius: double Read GetFixedRadius;
    property transitionStartRadius: double Read GetTransitionStartRadius;
    property transitionEndRadius: double Read GetTransitionEndRadius;
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

