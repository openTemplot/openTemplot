// This unit should not be used to house funxtionality unless it is CERTAIN that
// there is no other reasonable home for it

{ This unit provides a variety of utility functions that do not fit into other units. }
unit utils;

{$mode delphi}

interface

uses
  Classes, Math, SysUtils;

type

  TNumberCheckRslt = (
    ncValid,
    ncOutOfRange,
    ncMalformed );

// Checks a string to ensure it contains the valid representation of
// a floating point number which lies within specified limits.
// - If it does, the function returns 'True' and writes the value to a field
//   provided by the caller.
// - If it does not, the function returns 'False' and the client's field is untouched.
// Entry of a null string is equivalent to entering zero, if zero is between the limits,
// and otherwise is equivalent to entering whichever of the limits is nearer to zero.
function CheckFloat(input: String; lowerBound, upperBound: double; var target: double): TNumberCheckRslt;

// Checks a string to ensure it contains the valid representation of
// an integer which lies within specified limits.
// - If it does, the function returns 'True' and writes the value to a field
//   provided by the caller.
// - If it does not, the function returns 'False' and the client's field is untouched.
// Entry of a null string is equivalent to entering zero, if zero is between the limits,
// and otherwise equivalent to entering whichever of the limits is nearer to zero.
function CheckInt(input: String; lowerBound, upperBound: Integer; var target: Integer): TNumberCheckRslt;

implementation

function CheckFloat(input: String; lowerBound, upperBound: double; var target: double): TNumberCheckRslt;
var
value: double;

begin

  if length(input) = 0 then begin
    value := max(lowerBound, 0.0);
    value := min(value, upperBound);
    input := FloatToStr(value);
  end;

  try
    value := StrToFloat(input);
    if (value < lowerBound) or (value > upperBound) then
      result := ncOutOfRange
    else begin
      result := ncValid;
    end;
  except
    result := ncMalformed
  end;
  if result = ncValid then
    target := value;
end;

function CheckInt(input: String; lowerBound, upperBound: Integer; var target: Integer): TNumberCheckRslt;

var
  Value: Integer;

begin

  if length(input) = 0 then begin
    value := max(lowerBound, 0);
    value := min(value, upperBound);
    input := IntToStr(value);
  end;

  try
    value := StrToInt(input);
    if (value < lowerBound) or (value > upperBound) then
      result := ncOutOfRange
    else
      result := ncValid;
  except
    result := ncMalformed
  end;
  if result = ncValid then
     target := value;
end;

end.

