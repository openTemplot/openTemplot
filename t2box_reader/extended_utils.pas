unit extended_utils;

{$mode objfpc}{$H+}

interface

uses

  Classes,
  Dialogs,
  Math,
  SysUtils;

type
  TExtBytes = array [0..9] of byte;

function extendedToDouble(bytes: TExtBytes): double;
//function doubleToExtended(value: double): TExtBytes;


implementation

{ This function takes an array of bytes containing a bit pattern representing
 an 'extended' float value. (See https://en.wikipedia.org/wiki/Extended_precision)

 After splitting apart the pieces of the Extended value, the coreresponding
 Double is constructed by shifting the parts in from the right.
 (see https://en.wikipedia.org/wiki/Double-precision_floating-point_format)

 ::: NOTE :::
 1. The extended number is expected to be little-endian since it is assumed
    to be produced by Templot2 on an Intel processor.

 2. The excess bits of the mantissa are discarded (truncated rather than rounded)
}

function extendedToDouble(bytes: TExtBytes): double;
var
  rslt: Uint64;
  rsltDouble: double absolute rslt;
  signAndExp: Int16;
  sign: byte;
  exponent: Int64;
  mantissa: Qword;

begin
  mantissa := Pqword(@bytes[0])^;        // first get the mantissa
  signAndExp := (Puint16(@bytes[8]))^;   // next get sign+exponent ...
  sign := signAndExp shr 15;             // ... split off the sign ...
  exponent := signAndExp and $7fff;      // ... and drop it from the exponent

  case exponent of
    0:            // Special case - zero exponent = zero number (sign ignored)
      rslt := 0;

    $7fff:        // Special case - All ones exponent = all sorts of odd values
      // When/if implemented this section should deal with various
      // forms of infinity etc which are not needed for Templot2 files
      raise Exception.Create('====== INVALID FLOAT - $7fff  =======>');

    else begin
      exponent := exponent - 16383;               // un-bias the exponent (ext)

      if exponent < -1022 then
        raise Exception.Create('====== INVALID FLOAT - UNDERFLOW - ' +
                                       IntTohex(exponent) +
                                       '=======>')

      else
      if exponent > 1023 then
        raise Exception.Create('====== INVALID FLOAT - OVERFLOW - ' +
                                       IntTohex(exponent) +
                                       '=======>')

      else begin
        exponent := exponent + 1023;         // bias the exponent (double)
        mantissa := mantissa shl 1;          // Drop mantissa's leading '1'
        mantissa := mantissa shr 12;         // ... and isolate the upper 52 bits
        // Then build up the result:
        rslt := sign;                        // start with the sign
        rslt := (rslt shl 11) or exponent;   // ... add in the exponent
        rslt := (rslt shl 52) or mantissa;   // ... then add the mantissa
      end;
    end;
  end;

  Result := rsltDouble;
end;


//function doubleToExtended(value: double): TExtBytes;
//  //const
//var
//  dBytes: Qword;
//  rslt: TExtBytes;
//  zeroRslt: TExtBytes = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
//  mantissa: Qword;
//  exponent: smallint;
//  sign: byte;
//begin
//  move(value, dBytes, 8);               // first get the value as bytes
//  mantissa := dBytes and $fffffffffffff; //first get the mantissa
//  mantissa := mantissa shl 11;          // ... move it into place
//  mantissa := mantissa or $8000000000000000; // ... and add the integer bit
//  exponent := dBytes shr 52;           // next get sign+exponent ...
//  sign := exponent shr 11;              // ... split off the sign ...
//  exponent := exponent and $7ff;        // ... and drop it from the exponent
//  exponent := exponent - 1023;          // Then un-bias exponent
//  //writeln('mantissa => ', mantissa);
//  //writeln('sign     => ', sign);
//  //writeln('exponent => ', exponent);
//  case exponent of
//    0:
//      rslt := zeroRslt;
//    $7ff:        // TODO : This needs to interpret all sorts of odd values
//    begin
//      //dump(Tbytes(dBytes), 0, 10);
//      rslt := zeroRslt; // WRONG!!!  This needs to deal with various forms of infinity etc
//    end;
//    else begin
//      exponent := exponent + 16383;               // bias the exponent
//      move(mantissa, rslt[0], 8);
//      exponent := (exponent) or (sign shl 15);
//      //writeln('exp-2     => ', exponent);
//      move(exponent, rslt[8], 2);
//    end;
//  end;
//
//  //dump(Tbytes(rslt), 0, 10);
//
//  Result := rslt;
//end;


end.


