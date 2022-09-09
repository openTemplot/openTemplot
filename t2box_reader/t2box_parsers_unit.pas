
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

====================================================================================
*)

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

