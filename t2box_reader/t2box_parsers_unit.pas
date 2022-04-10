unit t2box_parsers_unit;

{$mode objfpc}{$H+}

interface

uses

  Classes,
  Dialogs,
  Math,
  SysUtils;

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

function extendedToDouble(bytes: TExtBytes): double;
function doubleToExtended(value: double): TExtBytes;

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
  BlockRead(box_file, value, 255, number_read);
  //need to use hexdump here
  //if logging then
  //  showmessage('Parsed bool : ' + fldname + ' : ' + booltostr(value));
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

// This function takes an array of bytes containing a bit pattern representing
// an 'extended' float value.
// The extended number is expected to be little-endian since it is assumed
// to be produced by Templot2 on an Intel processor.
function extendedToDouble(bytes: TExtBytes): double;
  //const
var
  rslt: double;
  mantissa: Qword;
  exponent: integer;
  sign: byte;
begin
  mantissa := Pqword(@bytes[0])^;         //first get the mantissa

  exponent := (Puint16(@bytes[8]))^;  // next get sign+exponent ...
  sign := exponent shr 15;             // ... split off the sign ...
  exponent := exponent and $7fff;      // ... and drop it from the exponent
  case exponent of
    0:
      rslt := 0;
    $7fff:        // TODO : This should interpret all sorts of odd values
                  // (although not needed for Templot2 files)
    begin
      writeln('=== INVALID FLOAT - $7fff  ============>');
      writeln('=== exponent = ', exponent);
      //dump(Tbytes(bytes), 0, 10);
      rslt := 0; // WRONG!!!  This should deal with various forms of infinity etc
                 // (although not needed for Templot2 files)
    end;
    else begin
      exponent := exponent - 16383;               // un-bias the exponent
      if exponent < -1022 then begin
        writeln('INVALID FLOAT - UNDERFLOW ============>');
        writeln('=== exponent = ', exponent);
        //dump(bytes, 0, 10);
        rslt := 0;
      end
      else
      if exponent > 1023 then begin
        writeln('INVALID FLOAT - OVERFLOW =============>');
        writeln('=== exponent = ', exponent);
        //dump(bytes, 0, 10);
        rslt := 0;
      end
      else
        try
          rslt := mantissa / intpower(2, 63 - exponent);
        except
          begin
            writeln('INVALID FLOAT ========================>');
            writeln('=== exponent = ', exponent);
            //dump(bytes, 0, 10);
            rslt := 0;
          end;
        end;
    end;
  end;

  if sign = 1 then
    rslt := -rslt;

  Result := rslt;
end;


function doubleToExtended(value: double): TExtBytes;
  //const
var
  dBytes: Qword;
  rslt: TExtBytes;
  zeroRslt: TExtBytes = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  mantissa: Qword;
  exponent: smallint;
  sign: byte;
begin
  move(value, dBytes, 8);               // first get the value as bytes
  mantissa := dBytes and $fffffffffffff; //first get the mantissa
  mantissa := mantissa shl 11;          // ... move it into place
  mantissa := mantissa or $8000000000000000; // ... and add the integer bit
  exponent := dBytes shr 52;           // next get sign+exponent ...
  sign := exponent shr 11;              // ... split off the sign ...
  exponent := exponent and $7ff;        // ... and drop it from the exponent
  exponent := exponent - 1023;          // Then un-bias exponent
  //writeln('mantissa => ', mantissa);
  //writeln('sign     => ', sign);
  //writeln('exponent => ', exponent);
  case exponent of
    0:
      rslt := zeroRslt;
    $7ff:        // TODO : This needs to interpret all sorts of odd values
    begin
      //dump(Tbytes(dBytes), 0, 10);
      rslt := zeroRslt; // WRONG!!!  This needs to deal with various forms of infinity etc
    end;
    else begin
      exponent := exponent + 16383;               // bias the exponent
      move(mantissa, rslt[0], 8);
      exponent := (exponent) or (sign shl 15);
      //writeln('exp-2     => ', exponent);
      move(exponent, rslt[8], 2);
    end;
  end;

  //dump(Tbytes(rslt), 0, 10);

  Result := rslt;
end;


end.

