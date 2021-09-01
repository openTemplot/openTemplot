unit shoved_timber;

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections;

type
  Tshove_code = (svcOmit = -1, svcEmpty = 0, svcShove = 1);

  Tshove_data = record     // shove data for a single timber ( version 0.71 11-4-01 ).

    sv_code: Tshove_code;
    sv_x: double;    // xtb modifier.
    sv_k: double;    // angle modifier.
    sv_o: double;    // offset modifier (near end).
    sv_l: double;    // length modifier (far end).
    sv_w: double;    // width modifier (per side).
    sv_c: double;    // crab modifier.  0.78.c  01-02-03.
    sv_t: double;    // spare (thickness 3-D modifier - nyi).

    alignment_byte_1: byte;   // D5 0.81 12-06-05
    alignment_byte_2: byte;   // D5 0.81 12-06-05

    sv_sp_int: integer;     // spare integer.

  end;//record

  Tshoved_timber = class;

  Tshove_for_file = record    // Used in the SHOVE DATA BLOCKS in the 071 files.
    // But not used within the program - see Ttimber_shove.shove_data instead.
    // Conversion takes place in 071 on loading.

    sf_str: string[6];           // timber number string.

    alignment_byte_1: byte;   // D5 0.81 12-06-05

    sf_shove_data: Tshove_data;  // all the data.

    procedure copy_from(src: Tshoved_timber);

  end;//record

  Tshoved_timber = class             // v: 0.71.a  27-4-01.

  private
    Ftimber_string: string;
    shove_data: Tshove_data;

  public
    constructor Create;
    constructor CreateFrom(f: Tshoved_timber); overload;
    constructor CreateFrom(f: Tshove_for_file); overload;

    property timber_string: string Read Ftimber_string Write Ftimber_string;
    property sv_code: Tshove_code Read shove_data.sv_code Write shove_data.sv_code;
    property sv_x: extended Read shove_data.sv_x Write shove_data.sv_x;
    property sv_k: extended Read shove_data.sv_k Write shove_data.sv_k;
    property sv_o: extended Read shove_data.sv_o Write shove_data.sv_o;
    property sv_l: extended Read shove_data.sv_l Write shove_data.sv_l;
    property sv_w: extended Read shove_data.sv_w Write shove_data.sv_w;
    property sv_c: extended Read shove_data.sv_c Write shove_data.sv_c;
    property sv_t: extended Read shove_data.sv_t Write shove_data.sv_t;
    property sv_sp_int: integer Read shove_data.sv_sp_int Write shove_data.sv_sp_int;


    procedure make_shoved;
    procedure make_omit;

    procedure set_shovex(shovex: double);
    procedure set_offset(offset: double);
    procedure set_crab(crab: double);
    procedure set_length(length: double);
    procedure set_width(Width: double);
    procedure set_twist(twist: double);

    procedure adjust_shovex(adjustment: double);
    procedure adjust_width(adjustment: double);
    procedure adjust_offset(adjustment: double);
    procedure adjust_length(adjustment: double);
    procedure adjust_twist_degrees(adjustment: double);
    procedure adjust_crab(adjustment: double);
    procedure rescale(mod_scale_ratio: double);
    function can_restore: boolean;

  end;//class


  Tshoved_timber_list = class(TObjectList<Tshoved_timber>)
  end;


implementation

procedure Tshove_for_file.copy_from(src: Tshoved_timber);
begin
  sf_str := src.timber_string;
  sf_shove_data := src.shove_data;
end;


constructor Tshoved_timber.Create;
begin
  Ftimber_string := '';
  shove_data.sv_code := svcEmpty;
  shove_data.sv_x := 0;        // xtb modifier.
  shove_data.sv_k := 0;        // angle modifier.
  shove_data.sv_o := 0;        // offset modifier (near end).
  shove_data.sv_l := 0;        // length modifier (far end).
  shove_data.sv_w := 0;        // width modifier (per side).
  shove_data.sv_c := 0;        // crab modifier.
  shove_data.sv_t := 0;        // spare (thickness 3-D modifier - nyi)
  shove_data.sv_sp_int := 0;   // spare integer.
end;

constructor Tshoved_timber.CreateFrom(f: Tshoved_timber);
begin
  Ftimber_string := f.Ftimber_string;
  shove_data := f.shove_data;
end;

constructor Tshoved_timber.CreateFrom(f: Tshove_for_file);
begin
  Ftimber_string := f.sf_str;
  shove_data := f.sf_shove_data;
end;

procedure Tshoved_timber.make_shoved;
begin
  if shove_data.sv_code = svcEmpty then begin
    shove_data.sv_code := svcShove;
  end;
end;

procedure Tshoved_timber.make_omit;
begin
  shove_data.sv_code := svcOmit;
  shove_data.sv_x := 0;        // xtb modifier.
  shove_data.sv_k := 0;        // angle modifier.
  shove_data.sv_o := 0;        // offset modifier (near end).
  shove_data.sv_l := 0;        // length modifier (far end).
  shove_data.sv_w := 0;        // width modifier (per side).
  shove_data.sv_c := 0;        // crab modifier.
  shove_data.sv_t := 0;        // spare (thickness 3-D modifier - nyi)
  shove_data.sv_sp_int := 0;   // spare integer.
end;

procedure Tshoved_timber.set_shovex(shovex: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_x := shovex;
end;

procedure Tshoved_timber.set_offset(offset: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_o := offset;
end;

procedure Tshoved_timber.set_crab(crab: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_c := crab;
end;

procedure Tshoved_timber.set_length(length: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_l := length;
end;

procedure Tshoved_timber.set_width(Width: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_w := Width;
end;

procedure Tshoved_timber.set_twist(twist: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_k := twist;
end;

procedure Tshoved_timber.adjust_shovex(adjustment: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_x := shove_data.sv_x + adjustment;
end;

procedure Tshoved_timber.adjust_width(adjustment: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_w := shove_data.sv_w + adjustment;
end;

procedure Tshoved_timber.adjust_offset(adjustment: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_o := shove_data.sv_o + adjustment;
end;

procedure Tshoved_timber.adjust_length(adjustment: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_l := shove_data.sv_l + adjustment;
end;

procedure Tshoved_timber.adjust_twist_degrees(adjustment: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_k := shove_data.sv_k + adjustment * Pi / 180;
end;

procedure Tshoved_timber.adjust_crab(adjustment: double);
begin
  shove_data.sv_code := svcShove;
  shove_data.sv_c := shove_data.sv_c + adjustment;
end;

procedure Tshoved_timber.rescale(mod_scale_ratio: double);
begin
  shove_data.sv_x := shove_data.sv_x * mod_scale_ratio;        // xtb modifier.
  //sv_k                             // angle modifier (no change).
  shove_data.sv_o := shove_data.sv_o * mod_scale_ratio;        // offset modifier (near end).
  shove_data.sv_l := shove_data.sv_l * mod_scale_ratio;        // length modifier (far end).
  shove_data.sv_w := shove_data.sv_w * mod_scale_ratio;        // width modifier (per side).
  shove_data.sv_t := shove_data.sv_t * mod_scale_ratio;        // thickness modifier (nyi).
end;

function Tshoved_timber.can_restore: boolean;
begin
  Result := (shove_data.sv_code = svcOmit) or
    (((shove_data.sv_x <> 0)       // xtb modifier.
    or (shove_data.sv_k <> 0)       // angle modifier.
    or (shove_data.sv_o <> 0)       // offset modifier (near end).
    or (shove_data.sv_l <> 0)       // length modifier (far end).
    or (shove_data.sv_w <> 0)       // width modifier (per side).
    or (shove_data.sv_c <> 0)       // crab modifier.  0.78.c  01-02-03.
    or (shove_data.sv_t <> 0)       // spare (thickness 3-D modifier - nyi).
    or (shove_data.sv_sp_int <> 0)  // spare integer.

    ) and (shove_data.sv_code = svcShove));
end;

end.
