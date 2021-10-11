unit utils;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function ValidFloat(input: String; min, max: double; var target: double): Boolean;
function ValidInt(input: String; min, max: Integer; var target: Integer): Boolean;

implementation

function ValidFloat(input: String; min, max: double; var target: double): Boolean;
var
value: double;

begin

  if length(input) = 0 then
    input := '0';
  try
    value := StrToFloat(input);
    if (value < min) or (value > max) then
      exit(False)
    else
      target := value;
    exit(True);
  except
    exit(False)
  end;
end;

function ValidInt(input: String; min, max: Integer; var target: Integer): Boolean;

var
  Value: Integer;

begin

  if length(input) = 0 then
    input := '0';
  try
    value := StrToInt(input);
    if (value < min) or (value > max) then
      exit(False)
    else
      target := value;
    exit(True);
  except
    exit(False)
  end;
end;

end.

