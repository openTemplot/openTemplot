
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

