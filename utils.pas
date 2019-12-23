unit utils;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function check_float(input: String; min, max: Extended; var target: Extended): Boolean;
function check_int(input: String; min, max: Integer; var target: Integer): Boolean;

implementation

// Checks the first char for '+' or '-' and sets caller's variables appropriately
procedure pull_sign(input: String; var negative: Boolean; var start: Integer);

begin
  case input[1] of
    '-': begin
      negative := True;
      start := 2;
    end;
    '+': begin
      negative := False;
      start := 2;
    end;
    else begin
      negative := False;
      start := 1;
    end;
  end;
end;

// Pulls decimal digits from part of a string and forms an integer which it
// pushes to the caller returning True if all is OK, False if a non-digit is found.
function pull_digits(input: String; start_ix, end_ix: Integer;
  var target: Integer): Boolean;

var
  ix: Integer;
  Value: Integer = 0;

begin
  for ix := start_ix to end_ix do begin
    if input[ix] in ['0' .. '9'] then begin
      Value := (Value * 10) + StrToInt(input[ix]);
    end
    else
      EXIT(False);
  end;

  target := Value;
  EXIT(True);
end;

function check_float(input: String; min, max: Extended; var target: Extended): Boolean;

var
  ix, dot_ix, int_end_ix: Integer;
  Value: Extended = 0;
  start: Integer;
  intval: Integer;
  negative: Boolean;

begin
  if length(input) = 0 then begin
    target := 0;
    EXIT(True);  // A zero-length string is fine
  end;

  pull_sign(input, negative, start);

  dot_ix := pos('.', input);

  if dot_ix = 0 then
    int_end_ix := length(input)
  else
    int_end_ix := dot_ix - 1;

  if pull_digits(input, start, int_end_ix, intval) = False then
    EXIT(False);

  if dot_ix > 0 then
    for ix := length(input) downto dot_ix + 1 do begin
      if input[ix] in ['0' .. '9'] then
        Value := (Value + Ord(input[ix]) - 48) / 10   // 48 = ord('0')
      else
        EXIT(False);
    end;

  Value := Value + intval;

  if negative then
    Value := Value * -1;

  if (Value < min) or (Value > max) then
    EXIT(False);

  target := Value;
  EXIT(True);
end;

function check_int(input: String; min, max: Integer; var target: Integer): Boolean;

var
  i: Integer;
  Value: Integer;
  start: Integer;
  negative: Boolean;

begin
  if length(input) = 0 then begin
    target := 0;
    EXIT(True);  // A zero-length string is fine
  end;

  pull_sign(input, negative, start);

  if pull_digits(input, start, length(input), Value) = False then // invalid char found
    EXIT(False);

  if negative then
    Value := Value * -1;

  if (Value < min) or (Value > max) then
    EXIT(False);

  target := Value;
  EXIT(True);
end;

end.
unit utils;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function check_int(input: String; min, max: Integer; var target: Integer): Boolean;

implementation

function pull_digits(input: String; start_ix, end_ix: Integer; target: Integer): Boolean;
var
  ix, Value: Integer;
begin
  for ix := end_ix downto start_ix do begin
    if input[ix] in ['0' .. '9'] then
      Value := Value * 10 + StrToInt(input[ix])
    else
      EXIT(False);
  end;

  target := Value;
  EXIT(True);
end;

function check_float(input: String; min, max: Extended; var target: Extended): Boolean;
var
  ix: Integer;
  val: Extended = 0;
  start: Integer;
  negative: Boolean;
begin
  case input[1] of
    '-': begin
      negative := True;
      start := 2;
    end;
    '+': begin
      negative := False;
      start := 2;
    end;
    else begin
      negative := False;
      start := 1;
    end;
  end;

  for ix := length(input) downto start do begin
    if input[ix] in ['0' .. '9'] then
      val := val * 10 + StrToInt(input[ix])
    else
      EXIT(False);

    if val > max then
      EXIT(False);
  end;

  if negative then
    val := val * -1

  if val<min or val>max then
    EXIT(False);

  target := val;
  EXIT(True);
end;

function check_int(input: String; min, max: Integer; var target: Integer): Boolean;
var
  i, val = 0: Integer;
  start: Integer;
  negative: Boolean;
begin
  case input[1] of
    '-': begin
      negative := True;
      start := 2;
    end;
    '+': begin
      negative := False;
      start := 2;
    end;
    else begin
      negative := False;
      start := 1;
    end;
  end;
  for i := length(input) downto start do begin
    if input[i] in ['0' .. '9'] then
      val := val * 10 + StrToInt(input[i])
    else
      EXIT(False);
  end;

  if val < min or val > max then
    EXIT(False);

  target := val;
  EXIT(True);
end;

end.



