unit t2box_parsers_unit;

{$mode objfpc}{$H+}

interface

uses

  Classes,
  Dialogs,
  Math,
  SysUtils,

  extended_utils;

type
  TExtBytes = array [0..9] of byte;

function parse_blob(var box_file: file; fldname: string; size: integer): Tbytes;
function parse_boolean(var box_file: file; fldname: string): boolean;
function parse_byte(var box_file: file; fldname: string): byte;
function parse_double(var box_file: file; fldname: string): double;
function parse_extended(var box_file: file; fldname: string): double;
function parse_float(var box_file: file; fldname: string): single;
function parse_integer(var box_file: file; fldname: string): integer;
function parse_string(var box_file: file; fldname: string; strlen: integer): string;
procedure parse_skip(var box_file: file; skipCount: Uint64);

var
  logging: boolean = false;

implementation

uses
  t2box_unit;

function parse_blob(var box_file: file; fldname: string; size: integer): Tbytes;
var
  value: Tbytes;
  number_read: integer;
begin
  setlength(value, size);
  BlockRead(box_file, value, size, number_read);
  //if logging then
  // ... need to use hexdump here
  Result := value;
end;

function parse_boolean(var box_file: file; fldname: string): boolean;
var
  value: boolean;
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(value), number_read);
  if logging then
    t2box_log.Info('Parsed bool : ' + fldname + ' : ' + booltostr(value));
  Result := value;
end;

function parse_byte(var box_file: file; fldname: string): byte;
var
  value: byte;
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(value), number_read);
  if logging then
    t2box_log.Info('Parsed byte : ' + fldname + ' : ' + inttostr(value));
  Result := value;
end;

function parse_double(var box_file: file; fldname: string): double;
var
  value: double;
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(value), number_read);
  if logging then
    t2box_log.Info('Parsed double : ' + fldname + ' : ' + floattostr(value));
  Result := value;
end;

function parse_extended(var box_file: file; fldname: string): double;
var
  bytes: TExtBytes;
  value: double;
  number_read: integer;
begin
  BlockRead(box_file, bytes, SizeOf(bytes), number_read);
  value := ExtendedToDouble(bytes);
  if logging then
    t2box_log.Info('Parsed extended : ' + fldname + ' : ' + floattostr(value));
  Result := value;
end;

function parse_float(var box_file: file; fldname: string): single;
var
  value: single;
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(value), number_read);
  if logging then
    t2box_log.Info('Parsed float : ' + fldname + ' : ' + floattostr(value));
  Result := value;
end;

function parse_integer(var box_file: file; fldname: string): integer;
var
  value: int32;                      // integers are 32 bits in T2 box files
  number_read: integer;
begin
  BlockRead(box_file, value, SizeOf(value), number_read);
  if logging then
    t2box_log.Info('Parsed integer : ' + fldname + ' : ' + inttostr(value));
  Result := value;
end;

function parse_string(var box_file: file; fldname: string; strlen: integer): string;
var
  value: string[255];
  number_read: integer;
begin
  BlockRead(box_file, value, strlen+1, number_read);
  if logging then
    t2box_log.Info('Parsed string : ' + fldname + ' : ' + value);
  Result := value;
end;


procedure parse_skip(var box_file: file; skipCount: Uint64);
var
  posn: Uint64;
begin
  posn := FilePos(box_file);
  seek(box_file, posn+skipCount)
  //if logging then
  //  ...  need to use hexdump here
end;

end.

