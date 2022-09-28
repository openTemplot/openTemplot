
(*
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2019  OpenTemplot project contributors

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

