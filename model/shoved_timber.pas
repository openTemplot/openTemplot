unit shoved_timber;

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type

  Tshove_data = record     // shove data for a single timber ( version 0.71 11-4-01 ).

    sv_code: integer;
    // 0=empty slot, -1=omit this timber,  1=shove this timber.
    sv_x: extended;    // xtb modifier.
    sv_k: extended;    // angle modifier.
    sv_o: extended;    // offset modifier (near end).
    sv_l: extended;    // length modifier (far end).
    sv_w: extended;    // width modifier (per side).
    sv_c: extended;    // crab modifier.  0.78.c  01-02-03.
    sv_t: extended;    // spare (thickness 3-D modifier - nyi).

    alignment_byte_1: byte;   // D5 0.81 12-06-05
    alignment_byte_2: byte;   // D5 0.81 12-06-05

    sv_sp_int: integer;     // spare integer.

  end;//record

  Tshoved_timber = class             // v: 0.71.a  27-4-01.

  public                         // 0.85.a
    timber_string: string;
    shove_data: Tshove_data;

  end;//class


  Tshoved_timber_list = class(TObjectList<Tshoved_timber>)
  end;


implementation

end.
