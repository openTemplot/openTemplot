unit helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,
  utils;

type

  TeditHelper = class helper for Tedit
    function ValidFloat(lowerBound, upperBound: double; var target: double): boolean;
    function ValidInteger(lowerBound, upperBound: integer; var target: integer): boolean;
  end;

implementation

// Checks that the text in the Text attribute represents a valid floating point value
// which falls in a range defined by the specified upper and lower bounds.
// If it does, the target is updated with that value and the function returns True
// if the string is a valid integer which is outside the range then the target is unchanged,
// the background colour of the text box is set to yellow and the function returns false
// if the string is not a valid integer then the target is unchanged, the background colour of
// the text box is set to pale red and the function returns false
function TeditHelper.ValidFloat(lowerBound, upperBound: double; var target: double): boolean;

begin
  case CheckFloat(self.Text, lowerBound, upperBound, target) of
    ncValid: begin
      Result := True;
      self.color := $00FFFFFF;   // white - all good
    end;
    ncOutOfRange: begin
      Result := False;
      self.color := $0055FFFF;   // pale yellow - caution
    end;
    ncMalformed: begin
      Result := False;
      self.color := $00CCAAFF;   // pale red - bad news;
    end;
  end;

end;

// Checks that the text in the Text attribute represents a valid integer value
// which falls in a range defined by the specified upper and lower bounds.
// If it does, the target is updated with that value and the function returns True
// If the string is a valid integer which is outside the range then the target is unchanged,
// the background colour of the text box is set to pale yellow and the function returns false
// if the string is not a valid integer then the target is unchanged, the background colour of
// the text box is set to pale red and the function returns false
function TeditHelper.ValidInteger(lowerBound, upperBound: integer; var target: integer): boolean;

begin
  case CheckInt(self.Text, lowerBound, upperBound, target) of
    ncValid: begin
      Result := True;
      self.color := $00FFFFFF;   // white - all good
    end;
    ncOutOfRange: begin
      Result := False;
      self.color := $0055FFFF;   // pale yellow - caution
    end;
    ncMalformed: begin
      Result := False;
      self.color := $00CCAAFF;   // pale red - bad news;
    end;
  end;

end;

end.

