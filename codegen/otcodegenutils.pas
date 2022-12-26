unit OTCodeGenUtils;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils;


function FirstUpper(const AString: String): String;
function FirstLower(const AString: String): String;
function AllUpper(const AString: String): String;
function AllUpperToCamelCase(const AString: String): String;
function StripUnderscores(const AString: String): String;
function QuotedValue(const AString: String): String;


implementation

function AllUpper(const AString: String): String;
var
  i: Integer;
  ch: Char;
begin
  Result := '';
  for i := 1 to Length(AString) do begin
    ch := AString[i];

    if CharInSet(ch, ['A'..'Z']) and (i > 1) then
      Result := Result + '_';

    if CharInSet(ch, ['a'..'z']) then
      Result := Result + Char(Ord(ch) - Ord('a') + Ord('A'))
    else
      Result := Result + ch;
  end;
end;


function FirstLower(const AString: String): String;
begin
  Result := AString;
  if (Length(Result) > 0) and CharInSet(Result[1], ['A'..'Z']) then
    Result[1] := Char(Ord(Result[1]) - Ord('A') + Ord('a'));
end;

function FirstUpper(const AString: String): String;
begin
  Result := AString;
  if (Length(Result) > 0) and CharInSet(Result[1], ['a'..'z']) then
    Result[1] := Char(Ord(Result[1]) - Ord('a') + Ord('A'));
end;

function QuotedValue(const AString: String): String;
begin
  Result := '''' + AString + '''';
end;

function AllUpperToCamelCase(const AString: String): String;
var
  ch: Char;
  nextInCaps: Boolean;
begin
  Result := '';
  nextInCaps := False;
  for ch in AString do begin
    if ch = '_' then
      nextInCaps := True
    else
    if CharInSet(ch, ['A'..'Z']) then begin
      if nextInCaps then
        Result := Result + ch
      else
        Result := Result + Char(Ord(ch) - Ord('A') + Ord('a'));
      nextInCaps := False;
    end
    else
      Result := Result + ch;
  end;
end;

function StripUnderscores(const AString: String): String;
var
  ch: Char;
begin
  Result := '';
  for ch in AString do begin
    if ch <> '_' then
      Result := Result + ch;
  end;
end;


end.
