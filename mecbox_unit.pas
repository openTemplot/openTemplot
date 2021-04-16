(*

    This file is part of Templot3, a computer program for the design of model railway track.
    Copyright (C) 2019  Martin Wynne.  email: martin@templot.com


    This program is free software: you may redistribute it and/or modify
    it under the terms of the GNU General Public Licence as published by
    the Free Software Foundation, either version 3 of the Licence, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public Licence for more details.

    You should have received a copy of the GNU General Public Licence
    along with this program. See the files: licence.txt or templotmec.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

====================================================================================
*)

unit mecbox_unit;          // 290a  file transfers via MECBOX text-based format

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  Tmecbox_form = class(TForm)
    load_mecbox_dialog: TOpenDialog;
    save_mecbox_dialog: TSaveDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mecbox_form: Tmecbox_form;

  function export_mecbox(show_export_result:boolean; export_str:string):boolean;

  function import_mecbox(file_str:string):boolean; // 290a

//______________________________________________________________________________

implementation

uses keep_select, wait_message, pad_unit, info_unit, control_room, alert_unit,
     math_unit, shove_timber,  rail_options_unit,
     shoved_timber;

{$R *.lfm}

//______________________________________________________________________________

function export_mecbox(show_export_result:boolean; export_str:string):boolean;

var
  output_list,encoded_list:TStringList;

  next_ti:Ttemplate_info;
  i:integer;

  num_templates:integer;

  mecbox_str:string;

                ////////////////////////////////////////////////////////////////

                procedure encode_template(i:integer);

                var
                  st:integer;
                  shove_timbers_list:TStringList;

                                //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                function force_dot_decimal(x:extended):string;

                                var
                                  s:string;

                                begin
                                  s:=FloatToStr(x);

                                  RESULT:=StringReplace(s,',','.',[rfReplaceAll,rfIgnoreCase]);
                                end;
                                //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                begin

                  with encoded_list do begin

                    Clear;   // init next   !!! can't add to it because Values[] overwrites

                    Add(' ');     // spacer

                    Add('```');   // mark start of a next template

                    Add(' ');     // spacer

                    Values['0999-i']:=IntToStr(i);     // keeps_list index

                    with next_ti.keep_dims do begin    // all the template dimensions...

                      Values['1000-s']:=StringReplace(box_dims1.box_ident,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1005-i']:=IntToStr(box_dims1.now_time);
                      Values['1010-s']:=StringReplace(box_dims1.keep_date,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1015-s']:=StringReplace(box_dims1.keep_time,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1020-s']:=StringReplace(box_dims1.top_label,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1025-s']:=StringReplace(box_dims1.project_for,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1030-s']:=StringReplace(box_dims1.reference_string,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1035-b']:=IntToStr(integer(box_dims1.this_was_control_template));
                      Values['1040-i']:=IntToStr(box_dims1.rail_info.flared_ends_ri);
                      Values['1045-i']:=IntToStr(box_dims1.rail_info.spare_int1);
                      Values['1050-i']:=IntToStr(box_dims1.rail_info.knuckle_code_ri);
                      Values['1055-e']:=force_dot_decimal(box_dims1.rail_info.knuckle_radius_ri);
                      Values['1060-e']:=force_dot_decimal(box_dims1.rail_info.spare_float2);
                      Values['1065-b']:=IntToStr(integer(box_dims1.rail_info.spare_bool1));
                      Values['1070-b']:=IntToStr(integer(box_dims1.rail_info.spare_bool2));
                      Values['1075-b']:=IntToStr(integer(box_dims1.rail_info.isolated_crossing_sw));
                      Values['1080-b']:=IntToStr(integer(box_dims1.rail_info.k_diagonal_side_check_rail_sw));
                      Values['1085-b']:=IntToStr(integer(box_dims1.rail_info.k_main_side_check_rail_sw));
                      Values['1090-b']:=IntToStr(integer(box_dims1.rail_info.switch_drive_sw));
                      Values['1095-b']:=IntToStr(integer(box_dims1.rail_info.track_centre_lines_sw));
                      Values['1100-b']:=IntToStr(integer(box_dims1.rail_info.turnout_road_stock_rail_sw));
                      Values['1105-b']:=IntToStr(integer(box_dims1.rail_info.turnout_road_check_rail_sw));
                      Values['1110-b']:=IntToStr(integer(box_dims1.rail_info.turnout_road_crossing_rail_sw));
                      Values['1115-b']:=IntToStr(integer(box_dims1.rail_info.crossing_vee_sw));
                      Values['1120-b']:=IntToStr(integer(box_dims1.rail_info.main_road_crossing_rail_sw));
                      Values['1125-b']:=IntToStr(integer(box_dims1.rail_info.main_road_check_rail_sw));
                      Values['1130-b']:=IntToStr(integer(box_dims1.rail_info.main_road_stock_rail_sw));
                      Values['1135-b']:=IntToStr(integer(box_dims1.auto_restore_on_startup));
                      Values['1140-b']:=IntToStr(integer(box_dims1.ask_restore_on_startup));
                      Values['1145-b']:=IntToStr(integer(box_dims1.pre077_bgnd_flag));
                      Values['1150-i']:=IntToStr(box_dims1.templot_version);
                      Values['1155-i']:=IntToStr(box_dims1.file_format_code);
                      Values['1160-i']:=IntToStr(box_dims1.gauge_index);
                      Values['1165-b']:=IntToStr(integer(box_dims1.gauge_exact));
                      Values['1170-b']:=IntToStr(integer(box_dims1.gauge_custom));
                      Values['1175-s']:=StringReplace(box_dims1.proto_info.name_str_pi,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1180-e']:=force_dot_decimal(box_dims1.proto_info.scale_pi);
                      Values['1185-e']:=force_dot_decimal(box_dims1.proto_info.gauge_pi);
                      Values['1190-e']:=force_dot_decimal(box_dims1.proto_info.fw_pi);
                      Values['1195-e']:=force_dot_decimal(box_dims1.proto_info.fwe_pi);
                      Values['1200-e']:=force_dot_decimal(box_dims1.proto_info.xing_fl_pi);
                      Values['1205-e']:=force_dot_decimal(box_dims1.proto_info.railtop_pi);
                      Values['1210-e']:=force_dot_decimal(box_dims1.proto_info.trtscent_pi);
                      Values['1215-e']:=force_dot_decimal(box_dims1.proto_info.trmscent_pi);
                      Values['1220-e']:=force_dot_decimal(box_dims1.proto_info.retcent_pi);
                      Values['1225-e']:=force_dot_decimal(box_dims1.proto_info.min_radius_pi);
                      Values['1230-e']:=force_dot_decimal(box_dims1.proto_info.old_winglongs_pi);
                      Values['1235-e']:=force_dot_decimal(box_dims1.proto_info.old_winglongl_pi);
                      Values['1240-e']:=force_dot_decimal(box_dims1.proto_info.old_cklongs_pi);
                      Values['1245-e']:=force_dot_decimal(box_dims1.proto_info.old_cklongm_pi);
                      Values['1250-e']:=force_dot_decimal(box_dims1.proto_info.old_cklongl_pi);
                      Values['1255-e']:=force_dot_decimal(box_dims1.proto_info.old_cklongxl_pi);
                      Values['1260-e']:=force_dot_decimal(box_dims1.proto_info.tbwide_pi);
                      Values['1265-e']:=force_dot_decimal(box_dims1.proto_info.slwide_pi);
                      Values['1270-e']:=force_dot_decimal(box_dims1.proto_info.xtimbsp_pi);
                      Values['1275-e']:=force_dot_decimal(box_dims1.proto_info.ftimbspmax_pi);
                      Values['1280-e']:=force_dot_decimal(box_dims1.proto_info.tb_pi);
                      Values['1285-b']:=IntToStr(integer(box_dims1.proto_info.mainside_ends_pi));
                      Values['1290-g']:=force_dot_decimal(box_dims1.proto_info.jt_slwide_pi);
                      Values['1295-e']:=force_dot_decimal(box_dims1.proto_info.random_end_pi);
                      Values['1300-e']:=force_dot_decimal(box_dims1.proto_info.random_angle_pi);
                      Values['1305-e']:=force_dot_decimal(box_dims1.proto_info.ck_ms_working1_pi);
                      Values['1310-e']:=force_dot_decimal(box_dims1.proto_info.ck_ms_working2_pi);
                      Values['1315-e']:=force_dot_decimal(box_dims1.proto_info.ck_ms_working3_pi);
                      Values['1320-e']:=force_dot_decimal(box_dims1.proto_info.ck_ts_working_mod_pi);
                      Values['1325-e']:=force_dot_decimal(box_dims1.proto_info.ck_ms_ext1_pi);
                      Values['1330-e']:=force_dot_decimal(box_dims1.proto_info.ck_ms_ext2_pi);
                      Values['1335-e']:=force_dot_decimal(box_dims1.proto_info.ck_ts_ext_mod_pi);
                      Values['1340-e']:=force_dot_decimal(box_dims1.proto_info.wing_ms_reach1_pi);
                      Values['1345-e']:=force_dot_decimal(box_dims1.proto_info.wing_ms_reach2_pi);
                      Values['1350-e']:=force_dot_decimal(box_dims1.proto_info.wing_ts_reach_mod_pi);
                      Values['1355-e']:=force_dot_decimal(box_dims1.proto_info.railbottom_pi);
                      Values['1360-e']:=force_dot_decimal(box_dims1.proto_info.rail_height_pi);
                      Values['1365-e']:=force_dot_decimal(box_dims1.proto_info.old_tb_pi);
                      Values['1370-e']:=force_dot_decimal(box_dims1.proto_info.rail_inclination_pi);
                      Values['1375-e']:=force_dot_decimal(box_dims1.railtop_inches);
                      Values['1380-e']:=force_dot_decimal(box_dims1.railbottom_inches);
                      Values['1385-i']:=IntToStr(box_dims1.version_as_loaded);
                      Values['1390-i']:=IntToStr(box_dims1.bgnd_code_077);
                      Values['1395-i']:=IntToStr(box_dims1.print_mapping_colour);
                      Values['1400-i']:=IntToStr(box_dims1.pad_marker_colour);
                      Values['1405-b']:=IntToStr(integer(box_dims1.use_print_mapping_colour));
                      Values['1410-b']:=IntToStr(integer(box_dims1.use_pad_marker_colour));
                      Values['1415-i']:=IntToStr(box_dims1.grid_units_code);
                      Values['1420-e']:=force_dot_decimal(box_dims1.x_grid_spacing);
                      Values['1425-e']:=force_dot_decimal(box_dims1.y_grid_spacing);
                      Values['1430-e']:=force_dot_decimal(box_dims1.total_length_of_timbering);
                      Values['1435-i']:=IntToStr(box_dims1.id_number);
                      Values['1440-s']:=StringReplace(box_dims1.id_number_str,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1445-e']:=force_dot_decimal(box_dims1.transform_info.datum_y);
                      Values['1450-e']:=force_dot_decimal(box_dims1.transform_info.x_go_limit);
                      Values['1455-e']:=force_dot_decimal(box_dims1.transform_info.x_stop_limit);
                      Values['1460-b']:=IntToStr(integer(box_dims1.transform_info.transforms_apply));
                      Values['1465-e']:=force_dot_decimal(box_dims1.transform_info.x1_shift);
                      Values['1470-e']:=force_dot_decimal(box_dims1.transform_info.y1_shift);
                      Values['1475-e']:=force_dot_decimal(box_dims1.transform_info.k_shift);
                      Values['1480-e']:=force_dot_decimal(box_dims1.transform_info.x2_shift);
                      Values['1485-e']:=force_dot_decimal(box_dims1.transform_info.y2_shift);
                      Values['1490-e']:=force_dot_decimal(box_dims1.transform_info.peg_pos.x);
                      Values['1495-e']:=force_dot_decimal(box_dims1.transform_info.peg_pos.y);
                      Values['1500-i']:=IntToStr(box_dims1.transform_info.peg_point_code);
                      Values['1505-i']:=IntToStr(box_dims1.transform_info.peg_point_rail);
                      Values['1510-b']:=IntToStr(integer(box_dims1.transform_info.mirror_on_x));
                      Values['1515-b']:=IntToStr(integer(box_dims1.transform_info.mirror_on_y));
                      Values['1520-e']:=force_dot_decimal(box_dims1.transform_info.notch_info.notch_x);
                      Values['1525-e']:=force_dot_decimal(box_dims1.transform_info.notch_info.notch_y);
                      Values['1530-e']:=force_dot_decimal(box_dims1.transform_info.notch_info.notch_k);
                      Values['1535-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.adjacent_edges_keep));
                      Values['1540-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ms_trackbed_edge_keep));
                      Values['1545-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ts_trackbed_edge_keep));
                      Values['1550-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.OUT_OF_USE_trackbed_width_ins_keep);
                      Values['1555-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ts_platform_keep));
                      Values['1560-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ts_platform_start_edge_keep));
                      Values['1565-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ts_platform_end_edge_keep));
                      Values['1570-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ts_platform_rear_edge_keep));
                      Values['1575-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ts_front_edge_ins_keep);
                      Values['1580-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ts_start_width_ins_keep);
                      Values['1585-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ts_end_width_ins_keep);
                      Values['1590-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ts_start_mm_keep);
                      Values['1595-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ts_length_mm_keep);
                      Values['1600-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ms_platform_keep));
                      Values['1605-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ms_platform_start_edge_keep));
                      Values['1610-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ms_platform_end_edge_keep));
                      Values['1615-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ms_platform_rear_edge_keep));
                      Values['1620-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ms_front_edge_ins_keep);
                      Values['1625-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ms_start_width_ins_keep);
                      Values['1630-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ms_end_width_ins_keep);
                      Values['1635-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ms_start_mm_keep);
                      Values['1640-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ms_length_mm_keep);
                      Values['1645-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.OUT_OF_USE_cess_width_ins_keep);
                      Values['1650-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.OUT_OF_USE_draw_trackbed_cess_edge_keep));
                      Values['1655-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ms_start_skew_mm_keep);
                      Values['1660-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ms_end_skew_mm_keep);
                      Values['1665-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ts_start_skew_mm_keep);
                      Values['1670-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.platform_ts_end_skew_mm_keep);
                      Values['1675-g']:=force_dot_decimal(box_dims1.platform_trackbed_info.trackbed_ms_width_ins_keep);
                      Values['1680-g']:=force_dot_decimal(box_dims1.platform_trackbed_info.trackbed_ts_width_ins_keep);
                      Values['1685-g']:=force_dot_decimal(box_dims1.platform_trackbed_info.cess_ms_width_ins_keep);
                      Values['1690-g']:=force_dot_decimal(box_dims1.platform_trackbed_info.cess_ts_width_ins_keep);
                      Values['1695-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep));
                      Values['1700-b']:=IntToStr(integer(box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep));
                      Values['1705-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.trackbed_ms_start_mm_keep);
                      Values['1710-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.trackbed_ms_length_mm_keep);
                      Values['1715-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.trackbed_ts_start_mm_keep);
                      Values['1720-e']:=force_dot_decimal(box_dims1.platform_trackbed_info.trackbed_ts_length_mm_keep);
                      Values['1725-b']:=IntToStr(integer(box_dims1.align_info.curving_flag));
                      Values['1730-b']:=IntToStr(integer(box_dims1.align_info.trans_flag));
                      Values['1735-e']:=force_dot_decimal(box_dims1.align_info.fixed_rad);
                      Values['1740-e']:=force_dot_decimal(box_dims1.align_info.trans_rad1);
                      Values['1745-e']:=force_dot_decimal(box_dims1.align_info.trans_rad2);
                      Values['1750-e']:=force_dot_decimal(box_dims1.align_info.trans_length);
                      Values['1755-e']:=force_dot_decimal(box_dims1.align_info.trans_start);
                      Values['1760-e']:=force_dot_decimal(box_dims1.align_info.rad_offset);
                      Values['1765-d']:=force_dot_decimal(box_dims1.align_info.tanh_kmax);
                      Values['1770-b']:=IntToStr(integer(box_dims1.align_info.slewing_flag));
                      Values['1775-b']:=IntToStr(integer(box_dims1.align_info.cl_only_flag));
                      Values['1780-y']:=IntToStr(box_dims1.align_info.slew_type);
                      Values['1785-b']:=IntToStr(integer(box_dims1.align_info.dummy_template_flag));
                      Values['1790-e']:=force_dot_decimal(box_dims1.align_info.slew_start);
                      Values['1795-e']:=force_dot_decimal(box_dims1.align_info.slew_length);
                      Values['1800-e']:=force_dot_decimal(box_dims1.align_info.slew_amount);
                      Values['1805-i']:=IntToStr(box_dims1.align_info.cl_options_code_int);
                      Values['1810-e']:=force_dot_decimal(box_dims1.align_info.cl_options_custom_offset_ext);
                      Values['1815-b']:=IntToStr(integer(box_dims1.align_info.reminder_flag));
                      Values['1820-i']:=IntToStr(box_dims1.align_info.reminder_colour);
                      Values['1825-s']:=StringReplace(box_dims1.align_info.reminder_str,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['1830-i']:=IntToStr(box_dims1.rail_type);
                      Values['1835-i']:=IntToStr(box_dims1.fb_kludge_template_code);
                      Values['1840-b']:=IntToStr(integer(box_dims1.box_save_done));
                      Values['1845-b']:=IntToStr(integer(box_dims1.uninclined_rails));
                      Values['1850-b']:=IntToStr(integer(box_dims1.disable_f7_snap));
                      Values['1855-e']:=force_dot_decimal(box_dims1.mod_text_x);
                      Values['1860-e']:=force_dot_decimal(box_dims1.mod_text_y);
                      Values['1865-e']:=force_dot_decimal(box_dims1.flatbottom_width);
                      Values['1870-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mw.len_diff);
                      Values['1875-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mw.flr_diff);
                      Values['1880-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mw.gap_diff);
                      Values['1885-y']:=IntToStr(box_dims1.check_diffs.end_diff_mw.type_diff);
                      Values['1890-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_me.len_diff);
                      Values['1895-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_me.flr_diff);
                      Values['1900-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_me.gap_diff);
                      Values['1905-y']:=IntToStr(box_dims1.check_diffs.end_diff_me.type_diff);
                      Values['1910-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mr.len_diff);
                      Values['1915-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mr.flr_diff);
                      Values['1920-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mr.gap_diff);
                      Values['1925-y']:=IntToStr(box_dims1.check_diffs.end_diff_mr.type_diff);
                      Values['1930-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_tw.len_diff);
                      Values['1935-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_tw.flr_diff);
                      Values['1940-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_tw.gap_diff);
                      Values['1945-y']:=IntToStr(box_dims1.check_diffs.end_diff_tw.type_diff);
                      Values['1950-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_te.len_diff);
                      Values['1955-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_te.flr_diff);
                      Values['1960-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_te.gap_diff);
                      Values['1965-y']:=IntToStr(box_dims1.check_diffs.end_diff_te.type_diff);
                      Values['1970-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_tr.len_diff);
                      Values['1975-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_tr.flr_diff);
                      Values['1980-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_tr.gap_diff);
                      Values['1985-y']:=IntToStr(box_dims1.check_diffs.end_diff_tr.type_diff);
                      Values['1990-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mk.len_diff);
                      Values['1995-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mk.flr_diff);
                      Values['2000-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_mk.gap_diff);
                      Values['2005-y']:=IntToStr(box_dims1.check_diffs.end_diff_mk.type_diff);
                      Values['2010-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_dk.len_diff);
                      Values['2015-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_dk.flr_diff);
                      Values['2020-e']:=force_dot_decimal(box_dims1.check_diffs.end_diff_dk.gap_diff);
                      Values['2025-y']:=IntToStr(box_dims1.check_diffs.end_diff_dk.type_diff);
                      Values['2030-b']:=IntToStr(integer(box_dims1.retain_diffs_on_make_flag));
                      Values['2035-b']:=IntToStr(integer(box_dims1.retain_diffs_on_mint_flag));
                      Values['2040-b']:=IntToStr(integer(box_dims1.retain_entry_straight_on_make_flag));
                      Values['2045-b']:=IntToStr(integer(box_dims1.retain_entry_straight_on_mint_flag));
                      Values['2050-b']:=IntToStr(integer(box_dims1.retain_shoves_on_make_flag));
                      Values['2055-b']:=IntToStr(integer(box_dims1.retain_shoves_on_mint_flag));
                      Values['2060-b']:=IntToStr(integer(box_dims1.turnout_info1.plain_track_flag));
                      Values['2065-b']:=IntToStr(integer(box_dims1.turnout_info1.rolled_in_sleepered_flag));
                      Values['2070-b']:=IntToStr(integer(box_dims1.turnout_info1.front_timbers_flag));
                      Values['2075-b']:=IntToStr(integer(box_dims1.turnout_info1.approach_rails_only_flag));
                      Values['2080-i']:=IntToStr(box_dims1.turnout_info1.hand);
                      Values['2085-b']:=IntToStr(integer(box_dims1.turnout_info1.timbering_flag));
                      Values['2090-b']:=IntToStr(integer(box_dims1.turnout_info1.switch_timbers_flag));
                      Values['2095-b']:=IntToStr(integer(box_dims1.turnout_info1.closure_timbers_flag));
                      Values['2100-b']:=IntToStr(integer(box_dims1.turnout_info1.xing_timbers_flag));
                      Values['2105-i']:=IntToStr(box_dims1.turnout_info1.exit_timbering);
                      Values['2110-i']:=IntToStr(box_dims1.turnout_info1.turnout_road_code);
                      Values['2115-e']:=force_dot_decimal(box_dims1.turnout_info1.turnout_length);
                      Values['2120-e']:=force_dot_decimal(box_dims1.turnout_info1.origin_to_toe);
                      Values['2125-e']:=force_dot_decimal(box_dims1.turnout_info1.step_size);
                      Values['2130-b']:=IntToStr(integer(box_dims1.turnout_info1.turnout_road_is_adjustable));
                      Values['2135-b']:=IntToStr(integer(box_dims1.turnout_info1.turnout_road_is_minimum));
                      Values['2140-i']:=IntToStr(turnout_info2.switch_info.old_size);
                      Values['2145-s']:=StringReplace(turnout_info2.switch_info.sw_name_str,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['2150-i']:=IntToStr(turnout_info2.switch_info.sw_pattern);
                      Values['2155-e']:=force_dot_decimal(turnout_info2.switch_info.planing);
                      Values['2160-e']:=force_dot_decimal(turnout_info2.switch_info.planing_angle);
                      Values['2165-e']:=force_dot_decimal(turnout_info2.switch_info.switch_radius_inchormax);
                      Values['2170-e']:=force_dot_decimal(turnout_info2.switch_info.switch_rail);
                      Values['2175-e']:=force_dot_decimal(turnout_info2.switch_info.stock_rail);
                      Values['2180-e']:=force_dot_decimal(turnout_info2.switch_info.heel_lead_inches);
                      Values['2185-e']:=force_dot_decimal(turnout_info2.switch_info.heel_offset_inches);
                      Values['2190-e']:=force_dot_decimal(turnout_info2.switch_info.switch_front_inches);
                      Values['2195-e']:=force_dot_decimal(turnout_info2.switch_info.planing_radius);
                      Values['2200-e']:=force_dot_decimal(turnout_info2.switch_info.sleeper_j1);
                      Values['2205-e']:=force_dot_decimal(turnout_info2.switch_info.sleeper_j2);
                      Values['2210-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[0]);
                      Values['2215-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[1]);
                      Values['2220-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[2]);
                      Values['2225-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[3]);
                      Values['2230-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[4]);
                      Values['2235-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[5]);
                      Values['2240-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[6]);
                      Values['2245-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[7]);
                      Values['2250-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[8]);
                      Values['2255-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[9]);
                      Values['2260-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[10]);
                      Values['2265-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[11]);
                      Values['2270-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[12]);
                      Values['2275-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[13]);
                      Values['2280-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[14]);
                      Values['2285-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[15]);
                      Values['2290-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[16]);
                      Values['2295-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[17]);
                      Values['2300-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[18]);
                      Values['2305-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[19]);
                      Values['2310-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[20]);
                      Values['2315-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[21]);
                      Values['2320-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[22]);
                      Values['2325-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[23]);
                      Values['2330-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[24]);
                      Values['2335-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[25]);
                      Values['2340-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[26]);
                      Values['2345-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[27]);
                      Values['2350-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[28]);
                      Values['2355-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[29]);
                      Values['2360-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[30]);
                      Values['2365-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[31]);
                      Values['2370-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[32]);
                      Values['2375-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[33]);
                      Values['2380-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[34]);
                      Values['2385-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[35]);
                      Values['2390-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[36]);
                      Values['2395-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[37]);
                      Values['2400-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[38]);
                      Values['2405-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[39]);
                      Values['2410-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[40]);
                      Values['2415-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[41]);
                      Values['2420-e']:=force_dot_decimal(turnout_info2.switch_info.timber_centres[42]);
                      Values['2425-i']:=IntToStr(turnout_info2.switch_info.group_code);
                      Values['2430-i']:=IntToStr(turnout_info2.switch_info.size_code);
                      Values['2435-e']:=force_dot_decimal(turnout_info2.switch_info.joggle_depth);
                      Values['2440-e']:=force_dot_decimal(turnout_info2.switch_info.joggle_length);
                      Values['2445-i']:=IntToStr(turnout_info2.switch_info.group_count);
                      Values['2450-b']:=IntToStr(integer(turnout_info2.switch_info.joggled_stock_rail));
                      Values['2455-b']:=IntToStr(integer(turnout_info2.switch_info.valid_data));
                      Values['2460-b']:=IntToStr(integer(turnout_info2.switch_info.front_timbered));
                      Values['2465-y']:=IntToStr(turnout_info2.switch_info.num_bridge_chairs_main_rail);
                      Values['2470-y']:=IntToStr(turnout_info2.switch_info.num_bridge_chairs_turnout_rail);
                      Values['2475-e']:=force_dot_decimal(turnout_info2.switch_info.fb_tip_offset);
                      Values['2480-e']:=force_dot_decimal(turnout_info2.switch_info.sleeper_j3);
                      Values['2485-e']:=force_dot_decimal(turnout_info2.switch_info.sleeper_j4);
                      Values['2490-e']:=force_dot_decimal(turnout_info2.switch_info.sleeper_j5);
                      Values['2495-y']:=IntToStr(turnout_info2.switch_info.num_slide_chairs);
                      Values['2500-y']:=IntToStr(turnout_info2.switch_info.num_block_slide_chairs);
                      Values['2505-y']:=IntToStr(turnout_info2.switch_info.num_block_heel_chairs);
                      Values['2510-i']:=IntToStr(turnout_info2.crossing_info.pattern);
                      Values['2515-i']:=IntToStr(turnout_info2.crossing_info.sl_mode);
                      Values['2520-i']:=IntToStr(turnout_info2.crossing_info.retcent_mode);
                      Values['2525-e']:=force_dot_decimal(turnout_info2.crossing_info.k3n_unit_angle);
                      Values['2530-e']:=force_dot_decimal(turnout_info2.crossing_info.fixed_st);
                      Values['2535-i']:=IntToStr(turnout_info2.crossing_info.hd_timbers_code);
                      Values['2540-i']:=IntToStr(turnout_info2.crossing_info.hd_vchecks_code);
                      Values['2545-e']:=force_dot_decimal(turnout_info2.crossing_info.k_check_length_1);
                      Values['2550-e']:=force_dot_decimal(turnout_info2.crossing_info.k_check_length_2);
                      Values['2555-e']:=force_dot_decimal(turnout_info2.crossing_info.k_check_mod_ms);
                      Values['2560-e']:=force_dot_decimal(turnout_info2.crossing_info.k_check_mod_ds);
                      Values['2565-e']:=force_dot_decimal(turnout_info2.crossing_info.k_check_flare);
                      Values['2570-b']:=IntToStr(integer(turnout_info2.crossing_info.curviform_timbering_keep));
                      Values['2575-i']:=IntToStr(turnout_info2.crossing_info.main_road_code);
                      Values['2580-i']:=IntToStr(turnout_info2.crossing_info.tandem_timber_code);
                      Values['2585-e']:=force_dot_decimal(turnout_info2.crossing_info.blunt_nose_width);
                      Values['2590-e']:=force_dot_decimal(turnout_info2.crossing_info.blunt_nose_to_timb);
                      Values['2595-e']:=force_dot_decimal(turnout_info2.crossing_info.vee_joint_half_spacing);
                      Values['2600-e']:=force_dot_decimal(turnout_info2.crossing_info.wing_joint_spacing);
                      Values['2605-e']:=force_dot_decimal(turnout_info2.crossing_info.wing_timber_spacing);
                      Values['2610-e']:=force_dot_decimal(turnout_info2.crossing_info.vee_timber_spacing);
                      Values['2615-y']:=IntToStr(turnout_info2.crossing_info.vee_joint_space_co1);
                      Values['2620-y']:=IntToStr(turnout_info2.crossing_info.vee_joint_space_co2);
                      Values['2625-y']:=IntToStr(turnout_info2.crossing_info.vee_joint_space_co3);
                      Values['2630-y']:=IntToStr(turnout_info2.crossing_info.vee_joint_space_co4);
                      Values['2635-y']:=IntToStr(turnout_info2.crossing_info.vee_joint_space_co5);
                      Values['2640-y']:=IntToStr(turnout_info2.crossing_info.vee_joint_space_co6);
                      Values['2645-y']:=IntToStr(turnout_info2.crossing_info.wing_joint_space_co1);
                      Values['2650-y']:=IntToStr(turnout_info2.crossing_info.wing_joint_space_co2);
                      Values['2655-y']:=IntToStr(turnout_info2.crossing_info.wing_joint_space_co3);
                      Values['2660-y']:=IntToStr(turnout_info2.crossing_info.wing_joint_space_co4);
                      Values['2665-y']:=IntToStr(turnout_info2.crossing_info.wing_joint_space_co5);
                      Values['2670-y']:=IntToStr(turnout_info2.crossing_info.wing_joint_space_co6);
                      Values['2675-e']:=force_dot_decimal(turnout_info2.crossing_info.main_road_endx_infile);
                      Values['2680-e']:=force_dot_decimal(turnout_info2.crossing_info.hdkn_unit_angle);
                      Values['2685-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_flare_ext_ms);
                      Values['2690-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_flare_ext_ts);
                      Values['2695-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_flare_work_ms);
                      Values['2700-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_flare_work_ts);
                      Values['2705-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.wing_flare_ms);
                      Values['2710-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.wing_flare_ts);
                      Values['2715-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_flare_k_ms);
                      Values['2720-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_flare_k_ds);
                      Values['2725-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_fwe_ext_ms);
                      Values['2730-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_fwe_ext_ts);
                      Values['2735-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_fwe_work_ms);
                      Values['2740-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_fwe_work_ts);
                      Values['2745-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.wing_fwe_ms);
                      Values['2750-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.wing_fwe_ts);
                      Values['2755-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_fwe_k_ms);
                      Values['2760-e']:=force_dot_decimal(turnout_info2.crossing_info.check_flare_info_081.check_fwe_k_ds);
                      Values['2765-e']:=force_dot_decimal(turnout_info2.crossing_info.k_custom_wing_long_keep);
                      Values['2770-e']:=force_dot_decimal(turnout_info2.crossing_info.k_custom_point_long_keep);
                      Values['2775-b']:=IntToStr(integer(turnout_info2.crossing_info.use_k_custom_wing_rails_keep));
                      Values['2780-b']:=IntToStr(integer(turnout_info2.crossing_info.use_k_custom_point_rails_keep));
                      Values['2785-b']:=IntToStr(integer(turnout_info2.plain_track_info.pt_custom));
                      Values['2790-i']:=IntToStr(turnout_info2.plain_track_info.list_index);
                      Values['2795-e']:=force_dot_decimal(turnout_info2.plain_track_info.rail_length);
                      Values['2800-i']:=IntToStr(turnout_info2.plain_track_info.sleepers_per_length);
                      Values['2805-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[0]);
                      Values['2810-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[1]);
                      Values['2815-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[2]);
                      Values['2820-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[3]);
                      Values['2825-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[4]);
                      Values['2830-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[5]);
                      Values['2835-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[6]);
                      Values['2840-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[7]);
                      Values['2845-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[8]);
                      Values['2850-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[9]);
                      Values['2855-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[10]);
                      Values['2860-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[11]);
                      Values['2865-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[12]);
                      Values['2870-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[13]);
                      Values['2875-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[14]);
                      Values['2880-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[15]);
                      Values['2885-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[16]);
                      Values['2890-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[17]);
                      Values['2895-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[18]);
                      Values['2900-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[19]);
                      Values['2905-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[20]);
                      Values['2910-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[21]);
                      Values['2915-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[22]);
                      Values['2920-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[23]);
                      Values['2925-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[24]);
                      Values['2930-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[25]);
                      Values['2935-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[26]);
                      Values['2940-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[27]);
                      Values['2945-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[28]);
                      Values['2950-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[29]);
                      Values['2955-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[30]);
                      Values['2960-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[31]);
                      Values['2965-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[32]);
                      Values['2970-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[33]);
                      Values['2975-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[34]);
                      Values['2980-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[35]);
                      Values['2985-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[36]);
                      Values['2990-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[37]);
                      Values['2995-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[38]);
                      Values['3000-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[39]);
                      Values['3005-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[40]);
                      Values['3010-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[41]);
                      Values['3015-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[42]);
                      Values['3020-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[43]);
                      Values['3025-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[44]);
                      Values['3030-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[45]);
                      Values['3035-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[46]);
                      Values['3040-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[47]);
                      Values['3045-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[48]);
                      Values['3050-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[49]);
                      Values['3055-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[50]);
                      Values['3060-e']:=force_dot_decimal(turnout_info2.plain_track_info.sleeper_centres[51]);
                      Values['3065-i']:=IntToStr(turnout_info2.plain_track_info.rail_joints_code);
                      Values['3070-i']:=IntToStr(turnout_info2.plain_track_info.user_peg_rail);
                      Values['3075-b']:=IntToStr(integer(turnout_info2.plain_track_info.user_peg_data_valid));
                      Values['3080-e']:=force_dot_decimal(turnout_info2.plain_track_info.user_pegx);
                      Values['3085-e']:=force_dot_decimal(turnout_info2.plain_track_info.user_pegy);
                      Values['3090-e']:=force_dot_decimal(turnout_info2.plain_track_info.user_pegk);
                      Values['3095-s']:=StringReplace(turnout_info2.plain_track_info.pt_spacing_name_str,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['3100-y']:=IntToStr(turnout_info2.plain_track_info.alignment_byte_6);
                      Values['3105-e']:=force_dot_decimal(turnout_info2.plain_track_info.pt_tb_rolling_percent);
                      Values['3110-e']:=force_dot_decimal(turnout_info2.plain_track_info.gaunt_sleeper_mod_inches);
                      Values['3115-i']:=IntToStr(turnout_info2.diamond_auto_code);
                      Values['3120-i']:=IntToStr(turnout_info2.bonus_timber_count);
                      Values['3125-b']:=IntToStr(integer(turnout_info2.equalizing_fixed_flag));
                      Values['3130-b']:=IntToStr(integer(turnout_info2.no_timbering_flag));
                      Values['3135-b']:=IntToStr(integer(turnout_info2.angled_on_flag));
                      Values['3140-b']:=IntToStr(integer(turnout_info2.chairing_flag));
                      Values['3145-e']:=force_dot_decimal(turnout_info2.start_draw_x);
                      Values['3150-e']:=force_dot_decimal(turnout_info2.timber_length_inc);
                      Values['3155-b']:=IntToStr(integer(turnout_info2.omit_switch_front_joints));
                      Values['3160-b']:=IntToStr(integer(turnout_info2.omit_switch_rail_joints));
                      Values['3165-b']:=IntToStr(integer(turnout_info2.omit_stock_rail_joints));
                      Values['3170-b']:=IntToStr(integer(turnout_info2.omit_wing_rail_joints));
                      Values['3175-b']:=IntToStr(integer(turnout_info2.omit_vee_rail_joints));
                      Values['3180-b']:=IntToStr(integer(turnout_info2.omit_k_crossing_stock_rail_joints));
                      Values['3185-b']:=IntToStr(integer(turnout_info2.diamond_switch_timbering_flag));
                      Values['3190-b']:=IntToStr(integer(turnout_info2.gaunt_flag));
                      Values['3195-b']:=IntToStr(integer(turnout_info2.diamond_proto_timbering_flag));
                      Values['3200-b']:=IntToStr(integer(turnout_info2.semi_diamond_flag));
                      Values['3205-b']:=IntToStr(integer(turnout_info2.diamond_fixed_flag));
                      Values['3210-e']:=force_dot_decimal(turnout_info2.hdk_check_rail_info.k_check_ms_1);
                      Values['3215-e']:=force_dot_decimal(turnout_info2.hdk_check_rail_info.k_check_ms_2);
                      Values['3220-e']:=force_dot_decimal(turnout_info2.hdk_check_rail_info.k_check_ds_1);
                      Values['3225-e']:=force_dot_decimal(turnout_info2.hdk_check_rail_info.k_check_ds_2);
                      Values['3230-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ms_working1);
                      Values['3235-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ms_working2);
                      Values['3240-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ms_working3);
                      Values['3245-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ts_working1);
                      Values['3250-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ts_working2);
                      Values['3255-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ts_working3);
                      Values['3260-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ms_ext1);
                      Values['3265-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ms_ext2);
                      Values['3270-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ts_ext1);
                      Values['3275-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_check_ts_ext2);
                      Values['3280-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_wing_ms_reach1);
                      Values['3285-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_wing_ms_reach2);
                      Values['3290-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_wing_ts_reach1);
                      Values['3295-e']:=force_dot_decimal(turnout_info2.vee_check_rail_info.v_wing_ts_reach2);
                      Values['3300-e']:=force_dot_decimal(turnout_info2.turnout_road_endx_infile);
                      Values['3305-s']:=StringReplace(turnout_info2.template_type_str,'`','_',[rfReplaceAll,rfIgnoreCase]);
                      Values['3310-e']:=force_dot_decimal(turnout_info2.smallest_radius_stored);
                      Values['3315-e']:=force_dot_decimal(turnout_info2.dpx_stored);
                      Values['3320-e']:=force_dot_decimal(turnout_info2.ipx_stored);
                      Values['3325-e']:=force_dot_decimal(turnout_info2.fpx_stored);
                      Values['3330-e']:=force_dot_decimal(turnout_info2.gaunt_offset_inches);
                      Values['3335-b']:=IntToStr(integer(turnout_info2.dxf_connector_0));
                      Values['3340-b']:=IntToStr(integer(turnout_info2.dxf_connector_t));
                      Values['3345-b']:=IntToStr(integer(turnout_info2.dxf_connector_9));

                    end;//with keep_dims

                    Add(' ');  // spacer

                  end;//with encoded_list

                  if next_ti.keep_shove_list.Count>0   // any shoved timbers?
                     then begin

                            shove_timbers_list:=TStringList.Create;

                            for st:=0 to next_ti.keep_shove_list.Count-1 do begin

                              with shove_timbers_list do begin

                                Clear;   // init next   !!! can't add to it because Values[] overwrites

                                Add(' ');  // spacer

                                Add('`_`_`');   // mark start of a shoved timber

                                Add(' ');  // spacer

                                Values['9800-s']:=next_ti.keep_shove_list.Strings[st];   // timber number string.

                                Values['9900-i']:=IntToStr(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_code);
                                Values['9905-e']:=force_dot_decimal(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_x);
                                Values['9910-e']:=force_dot_decimal(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_k);
                                Values['9915-e']:=force_dot_decimal(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_o);
                                Values['9920-e']:=force_dot_decimal(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_l);
                                Values['9925-e']:=force_dot_decimal(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_w);
                                Values['9930-e']:=force_dot_decimal(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_c);
                                Values['9935-e']:=force_dot_decimal(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_t);
                                Values['9940-i']:=IntToStr(Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_sp_int);

                                Add(' ');  // spacer

                              end;//with

                              encoded_list.Text:=encoded_list.Text+shove_timbers_list.Text;

                            end;//next timber

                            shove_timbers_list.Free;

                            encoded_list.Add('`_`_`');   // mark end of shoved timbers

                          end;  //shoved timbers


                  with encoded_list do begin
                    Add(' ');  // spacer
                    Add('`!`!`');      // mark start of info text

                    Add(StringReplace(keeps_list.Strings[i],'`','_',[rfReplaceAll,rfIgnoreCase]));

                    Add(' ');  // spacer
                    Add('`@`@`');      // mark start of memo text

                    Add(StringReplace(memo_list.Strings[i],'`','_',[rfReplaceAll,rfIgnoreCase]));

                    Add(' ');  // spacer
                    Add('`~`~`');      // mark end of memo text

                    Add(' ');  // spacer
                  end;//with

                  output_list.Text:=output_list.Text+encoded_list.Text;

                end;
                ////////////////////////////////////////////////////////////////

begin
  RESULT:=False;  // init

  if keeps_list.Count<1
     then begin
            alert(6,'     storage  box  is  empty',
                  'There are no stored templates to export.'
                 +'||The storage box is empty.',
                  '','','','','cancel','',0);
            EXIT;
          end;

  if export_str<>''
     then mecbox_str:=export_str
     else begin
            with mecbox_form.save_mecbox_dialog do begin         // set up the save dialog...

              if his_save_file_name<>'' then InitialDir:=ExtractFilePath(his_save_file_name)   // use his previous folder.
                                        else InitialDir:=exe_str+'BOX-FILES\';                 // or the default one.

              Filter:= ' storage  box  contents  in  transfer  format  (*.mecbox)|*.mecbox';

              Filename:=remove_invalid_str(Copy(Trim(box_project_title_str),1,20)+FormatDateTime(' yyyy_mm_dd_hhmm_ss',Date+Time))+'.mecbox';   // 0.79.a  20 chars was 15

              Title:='    export  all  templates  as ...';

              Filename:=lower_case_filename(Filename);   // 0.79.a   to underscores and lower case.

              if Execute=False then EXIT;    // show the dialog

              FileName:=ChangeFileExt(FileName,'.mecbox');   // force extension

              mecbox_str:=FileName;

            end;//with

            if invalid_85a_file_name(mecbox_str)=True then EXIT;

            his_save_file_name:=mecbox_str;    // so can use same folder next time.

          end;

  Screen.Cursor:=crHourGlass;  // might take a while

  num_templates:=0;   // init

  output_list:=TStringList.Create;

  output_list.Clear;

  output_list.Add('DO NOT EDIT THIS FILE');
  output_list.Add('=====================');
  output_list.Add('template data - saved from '+program_name_str+' version '+GetVersionString(voFull));
  output_list.Add('saved at '+FormatDateTime('hh:nn:ss "on" dd/mm/yyyy',Date+Time));
  output_list.Add('0 templates');     // strings[4]

  output_list.Add(' '); // spacer

  output_list.Values['0000-i']:='0';      // init number of templates

  encoded_list:=TStringList.Create;

  next_ti.keep_shove_list:=TStringList.Create;   // init local stringlist

  for i:=0 to keeps_list.Count-1 do begin     // first write the template data.

   next_ti.keep_shove_list.Clear;

   copy_template_info_from_to(False,Ttemplate(keeps_list.Objects[i]).template_info,next_ti);  // next template in list.

   if i=keeps_list.Count-1
      then next_ti.keep_dims.box_dims1.box_ident:='NX'+IntToStr(i)    // last one in file
      else next_ti.keep_dims.box_dims1.box_ident:='N '+IntToStr(i);

   encode_template(i);

   INC(num_templates);

  end;//next template

  if num_templates>0
     then begin
            output_list.Strings[4]:=IntToStr(num_templates)+' templates from '+StringReplace(next_ti.keep_dims.box_dims1.project_for,'`','_',[rfReplaceAll,rfIgnoreCase]);
            output_list.Values['0000-i']:=IntToStr(num_templates);  // overwrite

            output_list.Add(' ');     // spacer
            output_list.Add('```');   // mark end of all templates
            output_list.Add(' ');     // spacer
          end;

  output_list.SaveToFile(mecbox_str);

  next_ti.keep_shove_list.Free;

  encoded_list.Free;

  output_list.Free;

  Screen.Cursor:=crDefault;

  if show_export_result=True then ShowMessage(IntToStr(num_templates)+' templates have been exported to'+#13+#13+mecbox_str);

end;
//______________________________________________________________________________

function do_import(file_str:string):boolean;

var
  input_list,decode_list:TStringList;

  okd:Told_keep_data;  // for version mismatch check

  i,j,n,m:integer;

  num_templates:integer;

  next_ti:Ttemplate_info;

  input_str,template_str,info_str,memo_str:string;

  loaded_str,test_str:string;

  valid_template:boolean;


            ////////////////////////////////////////////////////////////////////

            function decode_template:boolean;

            label
              100;

            var
              h,st:integer;
              val_code:integer;
              next_str:string;

            begin
              RESULT:=False;  // init
              val_code:=0;    // init

              h:=POS('`_`_`',template_str);   // start of shove timbers

              if h=0
                 then next_str:=template_str
                 else begin
                        next_str:=Copy(template_str,1,h-1);
                        Delete(template_str,1,h+4);          // remove it up to shoved timbers
                      end;

              decode_list.Text:=next_str;

              with next_ti.keep_dims do begin

                     // Pascal VAL function ignores local decimal separator, reads always dot for decimals...

                repeat   // one-time loop
                  try

                    with decode_list do begin

                      if Values['1000-s']<>'' then box_dims1.box_ident:=Values['1000-s'];
                      if Values['1005-i']<>'' then box_dims1.now_time:=StrToInt(Values['1005-i']);
                      if Values['1010-s']<>'' then box_dims1.keep_date:=Values['1010-s'];
                      if Values['1015-s']<>'' then box_dims1.keep_time:=Values['1015-s'];
                      if Values['1020-s']<>'' then box_dims1.top_label:=Values['1020-s'];
                      if Values['1025-s']<>'' then box_dims1.project_for:=Values['1025-s'];
                      if Values['1030-s']<>'' then box_dims1.reference_string:=Values['1030-s'];
                      if Values['1035-b']<>'' then box_dims1.this_was_control_template:=(Values['1035-b']='1');
                      if Values['1040-i']<>'' then box_dims1.rail_info.flared_ends_ri:=StrToInt(Values['1040-i']);
                      if Values['1045-i']<>'' then box_dims1.rail_info.spare_int1:=StrToInt(Values['1045-i']);
                      if Values['1050-i']<>'' then box_dims1.rail_info.knuckle_code_ri:=StrToInt(Values['1050-i']);
                      if Values['1055-e']<>'' then begin VAL(Values['1055-e'],box_dims1.rail_info.knuckle_radius_ri,val_code); if val_code<>0 then BREAK; end;
                      if Values['1060-e']<>'' then begin VAL(Values['1060-e'],box_dims1.rail_info.spare_float2,val_code); if val_code<>0 then BREAK; end;
                      if Values['1065-b']<>'' then box_dims1.rail_info.spare_bool1:=(Values['1065-b']='1');
                      if Values['1070-b']<>'' then box_dims1.rail_info.spare_bool2:=(Values['1070-b']='1');
                      if Values['1075-b']<>'' then box_dims1.rail_info.isolated_crossing_sw:=(Values['1075-b']='1');
                      if Values['1080-b']<>'' then box_dims1.rail_info.k_diagonal_side_check_rail_sw:=(Values['1080-b']='1');
                      if Values['1085-b']<>'' then box_dims1.rail_info.k_main_side_check_rail_sw:=(Values['1085-b']='1');
                      if Values['1090-b']<>'' then box_dims1.rail_info.switch_drive_sw:=(Values['1090-b']='1');
                      if Values['1095-b']<>'' then box_dims1.rail_info.track_centre_lines_sw:=(Values['1095-b']='1');
                      if Values['1100-b']<>'' then box_dims1.rail_info.turnout_road_stock_rail_sw:=(Values['1100-b']='1');
                      if Values['1105-b']<>'' then box_dims1.rail_info.turnout_road_check_rail_sw:=(Values['1105-b']='1');
                      if Values['1110-b']<>'' then box_dims1.rail_info.turnout_road_crossing_rail_sw:=(Values['1110-b']='1');
                      if Values['1115-b']<>'' then box_dims1.rail_info.crossing_vee_sw:=(Values['1115-b']='1');
                      if Values['1120-b']<>'' then box_dims1.rail_info.main_road_crossing_rail_sw:=(Values['1120-b']='1');
                      if Values['1125-b']<>'' then box_dims1.rail_info.main_road_check_rail_sw:=(Values['1125-b']='1');
                      if Values['1130-b']<>'' then box_dims1.rail_info.main_road_stock_rail_sw:=(Values['1130-b']='1');
                      if Values['1135-b']<>'' then box_dims1.auto_restore_on_startup:=(Values['1135-b']='1');
                      if Values['1140-b']<>'' then box_dims1.ask_restore_on_startup:=(Values['1140-b']='1');
                      if Values['1145-b']<>'' then box_dims1.pre077_bgnd_flag:=(Values['1145-b']='1');
                      if Values['1150-i']<>'' then box_dims1.templot_version:=StrToInt(Values['1150-i']);
                      if Values['1155-i']<>'' then box_dims1.file_format_code:=StrToInt(Values['1155-i']);
                      if Values['1160-i']<>'' then box_dims1.gauge_index:=StrToInt(Values['1160-i']);
                      if Values['1165-b']<>'' then box_dims1.gauge_exact:=(Values['1165-b']='1');
                      if Values['1170-b']<>'' then box_dims1.gauge_custom:=(Values['1170-b']='1');
                      if Values['1175-s']<>'' then box_dims1.proto_info.name_str_pi:=Values['1175-s'];
                      if Values['1180-e']<>'' then begin VAL(Values['1180-e'],box_dims1.proto_info.scale_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1185-e']<>'' then begin VAL(Values['1185-e'],box_dims1.proto_info.gauge_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1190-e']<>'' then begin VAL(Values['1190-e'],box_dims1.proto_info.fw_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1195-e']<>'' then begin VAL(Values['1195-e'],box_dims1.proto_info.fwe_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1200-e']<>'' then begin VAL(Values['1200-e'],box_dims1.proto_info.xing_fl_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1205-e']<>'' then begin VAL(Values['1205-e'],box_dims1.proto_info.railtop_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1210-e']<>'' then begin VAL(Values['1210-e'],box_dims1.proto_info.trtscent_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1215-e']<>'' then begin VAL(Values['1215-e'],box_dims1.proto_info.trmscent_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1220-e']<>'' then begin VAL(Values['1220-e'],box_dims1.proto_info.retcent_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1225-e']<>'' then begin VAL(Values['1225-e'],box_dims1.proto_info.min_radius_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1230-e']<>'' then begin VAL(Values['1230-e'],box_dims1.proto_info.old_winglongs_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1235-e']<>'' then begin VAL(Values['1235-e'],box_dims1.proto_info.old_winglongl_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1240-e']<>'' then begin VAL(Values['1240-e'],box_dims1.proto_info.old_cklongs_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1245-e']<>'' then begin VAL(Values['1245-e'],box_dims1.proto_info.old_cklongm_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1250-e']<>'' then begin VAL(Values['1250-e'],box_dims1.proto_info.old_cklongl_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1255-e']<>'' then begin VAL(Values['1255-e'],box_dims1.proto_info.old_cklongxl_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1260-e']<>'' then begin VAL(Values['1260-e'],box_dims1.proto_info.tbwide_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1265-e']<>'' then begin VAL(Values['1265-e'],box_dims1.proto_info.slwide_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1270-e']<>'' then begin VAL(Values['1270-e'],box_dims1.proto_info.xtimbsp_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1275-e']<>'' then begin VAL(Values['1275-e'],box_dims1.proto_info.ftimbspmax_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1280-e']<>'' then begin VAL(Values['1280-e'],box_dims1.proto_info.tb_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1285-b']<>'' then box_dims1.proto_info.mainside_ends_pi:=(Values['1285-b']='1');
                      if Values['1290-g']<>'' then begin VAL(Values['1290-g'],box_dims1.proto_info.jt_slwide_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1295-e']<>'' then begin VAL(Values['1295-e'],box_dims1.proto_info.random_end_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1300-e']<>'' then begin VAL(Values['1300-e'],box_dims1.proto_info.random_angle_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1305-e']<>'' then begin VAL(Values['1305-e'],box_dims1.proto_info.ck_ms_working1_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1310-e']<>'' then begin VAL(Values['1310-e'],box_dims1.proto_info.ck_ms_working2_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1315-e']<>'' then begin VAL(Values['1315-e'],box_dims1.proto_info.ck_ms_working3_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1320-e']<>'' then begin VAL(Values['1320-e'],box_dims1.proto_info.ck_ts_working_mod_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1325-e']<>'' then begin VAL(Values['1325-e'],box_dims1.proto_info.ck_ms_ext1_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1330-e']<>'' then begin VAL(Values['1330-e'],box_dims1.proto_info.ck_ms_ext2_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1335-e']<>'' then begin VAL(Values['1335-e'],box_dims1.proto_info.ck_ts_ext_mod_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1340-e']<>'' then begin VAL(Values['1340-e'],box_dims1.proto_info.wing_ms_reach1_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1345-e']<>'' then begin VAL(Values['1345-e'],box_dims1.proto_info.wing_ms_reach2_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1350-e']<>'' then begin VAL(Values['1350-e'],box_dims1.proto_info.wing_ts_reach_mod_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1355-e']<>'' then begin VAL(Values['1355-e'],box_dims1.proto_info.railbottom_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1360-e']<>'' then begin VAL(Values['1360-e'],box_dims1.proto_info.rail_height_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1365-e']<>'' then begin VAL(Values['1365-e'],box_dims1.proto_info.old_tb_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1370-e']<>'' then begin VAL(Values['1370-e'],box_dims1.proto_info.rail_inclination_pi,val_code); if val_code<>0 then BREAK; end;
                      if Values['1375-e']<>'' then begin VAL(Values['1375-e'],box_dims1.railtop_inches,val_code); if val_code<>0 then BREAK; end;
                      if Values['1380-e']<>'' then begin VAL(Values['1380-e'],box_dims1.railbottom_inches,val_code); if val_code<>0 then BREAK; end;
                      if Values['1385-i']<>'' then box_dims1.version_as_loaded:=StrToInt(Values['1385-i']);
                      if Values['1390-i']<>'' then box_dims1.bgnd_code_077:=StrToInt(Values['1390-i']);
                      if Values['1395-i']<>'' then box_dims1.print_mapping_colour:=StrToInt(Values['1395-i']);
                      if Values['1400-i']<>'' then box_dims1.pad_marker_colour:=StrToInt(Values['1400-i']);
                      if Values['1405-b']<>'' then box_dims1.use_print_mapping_colour:=(Values['1405-b']='1');
                      if Values['1410-b']<>'' then box_dims1.use_pad_marker_colour:=(Values['1410-b']='1');
                      if Values['1415-i']<>'' then box_dims1.grid_units_code:=StrToInt(Values['1415-i']);
                      if Values['1420-e']<>'' then begin VAL(Values['1420-e'],box_dims1.x_grid_spacing,val_code); if val_code<>0 then BREAK; end;
                      if Values['1425-e']<>'' then begin VAL(Values['1425-e'],box_dims1.y_grid_spacing,val_code); if val_code<>0 then BREAK; end;
                      if Values['1430-e']<>'' then begin VAL(Values['1430-e'],box_dims1.total_length_of_timbering,val_code); if val_code<>0 then BREAK; end;
                      if Values['1435-i']<>'' then box_dims1.id_number:=StrToInt(Values['1435-i']);
                      if Values['1440-s']<>'' then box_dims1.id_number_str:=Values['1440-s'];
                      if Values['1445-e']<>'' then begin VAL(Values['1445-e'],box_dims1.transform_info.datum_y,val_code); if val_code<>0 then BREAK; end;
                      if Values['1450-e']<>'' then begin VAL(Values['1450-e'],box_dims1.transform_info.x_go_limit,val_code); if val_code<>0 then BREAK; end;
                      if Values['1455-e']<>'' then begin VAL(Values['1455-e'],box_dims1.transform_info.x_stop_limit,val_code); if val_code<>0 then BREAK; end;
                      if Values['1460-b']<>'' then box_dims1.transform_info.transforms_apply:=(Values['1460-b']='1');
                      if Values['1465-e']<>'' then begin VAL(Values['1465-e'],box_dims1.transform_info.x1_shift,val_code); if val_code<>0 then BREAK; end;
                      if Values['1470-e']<>'' then begin VAL(Values['1470-e'],box_dims1.transform_info.y1_shift,val_code); if val_code<>0 then BREAK; end;
                      if Values['1475-e']<>'' then begin VAL(Values['1475-e'],box_dims1.transform_info.k_shift,val_code); if val_code<>0 then BREAK; end;
                      if Values['1480-e']<>'' then begin VAL(Values['1480-e'],box_dims1.transform_info.x2_shift,val_code); if val_code<>0 then BREAK; end;
                      if Values['1485-e']<>'' then begin VAL(Values['1485-e'],box_dims1.transform_info.y2_shift,val_code); if val_code<>0 then BREAK; end;
                      if Values['1490-e']<>'' then begin VAL(Values['1490-e'],box_dims1.transform_info.peg_pos.x,val_code); if val_code<>0 then BREAK; end;
                      if Values['1495-e']<>'' then begin VAL(Values['1495-e'],box_dims1.transform_info.peg_pos.y,val_code); if val_code<>0 then BREAK; end;
                      if Values['1500-i']<>'' then box_dims1.transform_info.peg_point_code:=StrToInt(Values['1500-i']);
                      if Values['1505-i']<>'' then box_dims1.transform_info.peg_point_rail:=StrToInt(Values['1505-i']);
                      if Values['1510-b']<>'' then box_dims1.transform_info.mirror_on_x:=(Values['1510-b']='1');
                      if Values['1515-b']<>'' then box_dims1.transform_info.mirror_on_y:=(Values['1515-b']='1');
                      if Values['1520-e']<>'' then begin VAL(Values['1520-e'],box_dims1.transform_info.notch_info.notch_x,val_code); if val_code<>0 then BREAK; end;
                      if Values['1525-e']<>'' then begin VAL(Values['1525-e'],box_dims1.transform_info.notch_info.notch_y,val_code); if val_code<>0 then BREAK; end;
                      if Values['1530-e']<>'' then begin VAL(Values['1530-e'],box_dims1.transform_info.notch_info.notch_k,val_code); if val_code<>0 then BREAK; end;
                      if Values['1535-b']<>'' then box_dims1.platform_trackbed_info.adjacent_edges_keep:=(Values['1535-b']='1');
                      if Values['1540-b']<>'' then box_dims1.platform_trackbed_info.draw_ms_trackbed_edge_keep:=(Values['1540-b']='1');
                      if Values['1545-b']<>'' then box_dims1.platform_trackbed_info.draw_ts_trackbed_edge_keep:=(Values['1545-b']='1');
                      if Values['1550-e']<>'' then begin VAL(Values['1550-e'],box_dims1.platform_trackbed_info.OUT_OF_USE_trackbed_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1555-b']<>'' then box_dims1.platform_trackbed_info.draw_ts_platform_keep:=(Values['1555-b']='1');
                      if Values['1560-b']<>'' then box_dims1.platform_trackbed_info.draw_ts_platform_start_edge_keep:=(Values['1560-b']='1');
                      if Values['1565-b']<>'' then box_dims1.platform_trackbed_info.draw_ts_platform_end_edge_keep:=(Values['1565-b']='1');
                      if Values['1570-b']<>'' then box_dims1.platform_trackbed_info.draw_ts_platform_rear_edge_keep:=(Values['1570-b']='1');
                      if Values['1575-e']<>'' then begin VAL(Values['1575-e'],box_dims1.platform_trackbed_info.platform_ts_front_edge_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1580-e']<>'' then begin VAL(Values['1580-e'],box_dims1.platform_trackbed_info.platform_ts_start_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1585-e']<>'' then begin VAL(Values['1585-e'],box_dims1.platform_trackbed_info.platform_ts_end_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1590-e']<>'' then begin VAL(Values['1590-e'],box_dims1.platform_trackbed_info.platform_ts_start_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1595-e']<>'' then begin VAL(Values['1595-e'],box_dims1.platform_trackbed_info.platform_ts_length_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1600-b']<>'' then box_dims1.platform_trackbed_info.draw_ms_platform_keep:=(Values['1600-b']='1');
                      if Values['1605-b']<>'' then box_dims1.platform_trackbed_info.draw_ms_platform_start_edge_keep:=(Values['1605-b']='1');
                      if Values['1610-b']<>'' then box_dims1.platform_trackbed_info.draw_ms_platform_end_edge_keep:=(Values['1610-b']='1');
                      if Values['1615-b']<>'' then box_dims1.platform_trackbed_info.draw_ms_platform_rear_edge_keep:=(Values['1615-b']='1');
                      if Values['1620-e']<>'' then begin VAL(Values['1620-e'],box_dims1.platform_trackbed_info.platform_ms_front_edge_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1625-e']<>'' then begin VAL(Values['1625-e'],box_dims1.platform_trackbed_info.platform_ms_start_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1630-e']<>'' then begin VAL(Values['1630-e'],box_dims1.platform_trackbed_info.platform_ms_end_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1635-e']<>'' then begin VAL(Values['1635-e'],box_dims1.platform_trackbed_info.platform_ms_start_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1640-e']<>'' then begin VAL(Values['1640-e'],box_dims1.platform_trackbed_info.platform_ms_length_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1645-e']<>'' then begin VAL(Values['1645-e'],box_dims1.platform_trackbed_info.OUT_OF_USE_cess_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1650-b']<>'' then box_dims1.platform_trackbed_info.OUT_OF_USE_draw_trackbed_cess_edge_keep:=(Values['1650-b']='1');
                      if Values['1655-e']<>'' then begin VAL(Values['1655-e'],box_dims1.platform_trackbed_info.platform_ms_start_skew_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1660-e']<>'' then begin VAL(Values['1660-e'],box_dims1.platform_trackbed_info.platform_ms_end_skew_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1665-e']<>'' then begin VAL(Values['1665-e'],box_dims1.platform_trackbed_info.platform_ts_start_skew_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1670-e']<>'' then begin VAL(Values['1670-e'],box_dims1.platform_trackbed_info.platform_ts_end_skew_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1675-g']<>'' then begin VAL(Values['1675-g'],box_dims1.platform_trackbed_info.trackbed_ms_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1680-g']<>'' then begin VAL(Values['1680-g'],box_dims1.platform_trackbed_info.trackbed_ts_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1685-g']<>'' then begin VAL(Values['1685-g'],box_dims1.platform_trackbed_info.cess_ms_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1690-g']<>'' then begin VAL(Values['1690-g'],box_dims1.platform_trackbed_info.cess_ts_width_ins_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1695-b']<>'' then box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep:=(Values['1695-b']='1');
                      if Values['1700-b']<>'' then box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep:=(Values['1700-b']='1');
                      if Values['1705-e']<>'' then begin VAL(Values['1705-e'],box_dims1.platform_trackbed_info.trackbed_ms_start_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1710-e']<>'' then begin VAL(Values['1710-e'],box_dims1.platform_trackbed_info.trackbed_ms_length_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1715-e']<>'' then begin VAL(Values['1715-e'],box_dims1.platform_trackbed_info.trackbed_ts_start_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1720-e']<>'' then begin VAL(Values['1720-e'],box_dims1.platform_trackbed_info.trackbed_ts_length_mm_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['1725-b']<>'' then box_dims1.align_info.curving_flag:=(Values['1725-b']='1');
                      if Values['1730-b']<>'' then box_dims1.align_info.trans_flag:=(Values['1730-b']='1');
                      if Values['1735-e']<>'' then begin VAL(Values['1735-e'],box_dims1.align_info.fixed_rad,val_code); if val_code<>0 then BREAK; end;
                      if Values['1740-e']<>'' then begin VAL(Values['1740-e'],box_dims1.align_info.trans_rad1,val_code); if val_code<>0 then BREAK; end;
                      if Values['1745-e']<>'' then begin VAL(Values['1745-e'],box_dims1.align_info.trans_rad2,val_code); if val_code<>0 then BREAK; end;
                      if Values['1750-e']<>'' then begin VAL(Values['1750-e'],box_dims1.align_info.trans_length,val_code); if val_code<>0 then BREAK; end;
                      if Values['1755-e']<>'' then begin VAL(Values['1755-e'],box_dims1.align_info.trans_start,val_code); if val_code<>0 then BREAK; end;
                      if Values['1760-e']<>'' then begin VAL(Values['1760-e'],box_dims1.align_info.rad_offset,val_code); if val_code<>0 then BREAK; end;
                      if Values['1765-d']<>'' then begin VAL(Values['1765-d'],box_dims1.align_info.tanh_kmax,val_code); if val_code<>0 then BREAK; end;
                      if Values['1770-b']<>'' then box_dims1.align_info.slewing_flag:=(Values['1770-b']='1');
                      if Values['1775-b']<>'' then box_dims1.align_info.cl_only_flag:=(Values['1775-b']='1');
                      if Values['1780-y']<>'' then box_dims1.align_info.slew_type:=StrToInt(Values['1780-y']);
                      if Values['1785-b']<>'' then box_dims1.align_info.dummy_template_flag:=(Values['1785-b']='1');
                      if Values['1790-e']<>'' then begin VAL(Values['1790-e'],box_dims1.align_info.slew_start,val_code); if val_code<>0 then BREAK; end;
                      if Values['1795-e']<>'' then begin VAL(Values['1795-e'],box_dims1.align_info.slew_length,val_code); if val_code<>0 then BREAK; end;
                      if Values['1800-e']<>'' then begin VAL(Values['1800-e'],box_dims1.align_info.slew_amount,val_code); if val_code<>0 then BREAK; end;
                      if Values['1805-i']<>'' then box_dims1.align_info.cl_options_code_int:=StrToInt(Values['1805-i']);
                      if Values['1810-e']<>'' then begin VAL(Values['1810-e'],box_dims1.align_info.cl_options_custom_offset_ext,val_code); if val_code<>0 then BREAK; end;
                      if Values['1815-b']<>'' then box_dims1.align_info.reminder_flag:=(Values['1815-b']='1');
                      if Values['1820-i']<>'' then box_dims1.align_info.reminder_colour:=StrToInt(Values['1820-i']);
                      if Values['1825-s']<>'' then box_dims1.align_info.reminder_str:=Values['1825-s'];
                      if Values['1830-i']<>'' then box_dims1.rail_type:=StrToInt(Values['1830-i']);
                      if Values['1835-i']<>'' then box_dims1.fb_kludge_template_code:=StrToInt(Values['1835-i']);
                      if Values['1840-b']<>'' then box_dims1.box_save_done:=(Values['1840-b']='1');
                      if Values['1845-b']<>'' then box_dims1.uninclined_rails:=(Values['1845-b']='1');
                      if Values['1850-b']<>'' then box_dims1.disable_f7_snap:=(Values['1850-b']='1');
                      if Values['1855-e']<>'' then begin VAL(Values['1855-e'],box_dims1.mod_text_x,val_code); if val_code<>0 then BREAK; end;
                      if Values['1860-e']<>'' then begin VAL(Values['1860-e'],box_dims1.mod_text_y,val_code); if val_code<>0 then BREAK; end;
                      if Values['1865-e']<>'' then begin VAL(Values['1865-e'],box_dims1.flatbottom_width,val_code); if val_code<>0 then BREAK; end;
                      if Values['1870-e']<>'' then begin VAL(Values['1870-e'],box_dims1.check_diffs.end_diff_mw.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1875-e']<>'' then begin VAL(Values['1875-e'],box_dims1.check_diffs.end_diff_mw.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1880-e']<>'' then begin VAL(Values['1880-e'],box_dims1.check_diffs.end_diff_mw.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1885-y']<>'' then box_dims1.check_diffs.end_diff_mw.type_diff:=StrToInt(Values['1885-y']);
                      if Values['1890-e']<>'' then begin VAL(Values['1890-e'],box_dims1.check_diffs.end_diff_me.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1895-e']<>'' then begin VAL(Values['1895-e'],box_dims1.check_diffs.end_diff_me.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1900-e']<>'' then begin VAL(Values['1900-e'],box_dims1.check_diffs.end_diff_me.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1905-y']<>'' then box_dims1.check_diffs.end_diff_me.type_diff:=StrToInt(Values['1905-y']);
                      if Values['1910-e']<>'' then begin VAL(Values['1910-e'],box_dims1.check_diffs.end_diff_mr.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1915-e']<>'' then begin VAL(Values['1915-e'],box_dims1.check_diffs.end_diff_mr.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1920-e']<>'' then begin VAL(Values['1920-e'],box_dims1.check_diffs.end_diff_mr.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1925-y']<>'' then box_dims1.check_diffs.end_diff_mr.type_diff:=StrToInt(Values['1925-y']);
                      if Values['1930-e']<>'' then begin VAL(Values['1930-e'],box_dims1.check_diffs.end_diff_tw.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1935-e']<>'' then begin VAL(Values['1935-e'],box_dims1.check_diffs.end_diff_tw.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1940-e']<>'' then begin VAL(Values['1940-e'],box_dims1.check_diffs.end_diff_tw.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1945-y']<>'' then box_dims1.check_diffs.end_diff_tw.type_diff:=StrToInt(Values['1945-y']);
                      if Values['1950-e']<>'' then begin VAL(Values['1950-e'],box_dims1.check_diffs.end_diff_te.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1955-e']<>'' then begin VAL(Values['1955-e'],box_dims1.check_diffs.end_diff_te.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1960-e']<>'' then begin VAL(Values['1960-e'],box_dims1.check_diffs.end_diff_te.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1965-y']<>'' then box_dims1.check_diffs.end_diff_te.type_diff:=StrToInt(Values['1965-y']);
                      if Values['1970-e']<>'' then begin VAL(Values['1970-e'],box_dims1.check_diffs.end_diff_tr.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1975-e']<>'' then begin VAL(Values['1975-e'],box_dims1.check_diffs.end_diff_tr.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1980-e']<>'' then begin VAL(Values['1980-e'],box_dims1.check_diffs.end_diff_tr.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1985-y']<>'' then box_dims1.check_diffs.end_diff_tr.type_diff:=StrToInt(Values['1985-y']);
                      if Values['1990-e']<>'' then begin VAL(Values['1990-e'],box_dims1.check_diffs.end_diff_mk.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['1995-e']<>'' then begin VAL(Values['1995-e'],box_dims1.check_diffs.end_diff_mk.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['2000-e']<>'' then begin VAL(Values['2000-e'],box_dims1.check_diffs.end_diff_mk.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['2005-y']<>'' then box_dims1.check_diffs.end_diff_mk.type_diff:=StrToInt(Values['2005-y']);
                      if Values['2010-e']<>'' then begin VAL(Values['2010-e'],box_dims1.check_diffs.end_diff_dk.len_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['2015-e']<>'' then begin VAL(Values['2015-e'],box_dims1.check_diffs.end_diff_dk.flr_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['2020-e']<>'' then begin VAL(Values['2020-e'],box_dims1.check_diffs.end_diff_dk.gap_diff,val_code); if val_code<>0 then BREAK; end;
                      if Values['2025-y']<>'' then box_dims1.check_diffs.end_diff_dk.type_diff:=StrToInt(Values['2025-y']);
                      if Values['2030-b']<>'' then box_dims1.retain_diffs_on_make_flag:=(Values['2030-b']='1');
                      if Values['2035-b']<>'' then box_dims1.retain_diffs_on_mint_flag:=(Values['2035-b']='1');
                      if Values['2040-b']<>'' then box_dims1.retain_entry_straight_on_make_flag:=(Values['2040-b']='1');
                      if Values['2045-b']<>'' then box_dims1.retain_entry_straight_on_mint_flag:=(Values['2045-b']='1');
                      if Values['2050-b']<>'' then box_dims1.retain_shoves_on_make_flag:=(Values['2050-b']='1');
                      if Values['2055-b']<>'' then box_dims1.retain_shoves_on_mint_flag:=(Values['2055-b']='1');
                      if Values['2060-b']<>'' then box_dims1.turnout_info1.plain_track_flag:=(Values['2060-b']='1');
                      if Values['2065-b']<>'' then box_dims1.turnout_info1.rolled_in_sleepered_flag:=(Values['2065-b']='1');
                      if Values['2070-b']<>'' then box_dims1.turnout_info1.front_timbers_flag:=(Values['2070-b']='1');
                      if Values['2075-b']<>'' then box_dims1.turnout_info1.approach_rails_only_flag:=(Values['2075-b']='1');
                      if Values['2080-i']<>'' then box_dims1.turnout_info1.hand:=StrToInt(Values['2080-i']);
                      if Values['2085-b']<>'' then box_dims1.turnout_info1.timbering_flag:=(Values['2085-b']='1');
                      if Values['2090-b']<>'' then box_dims1.turnout_info1.switch_timbers_flag:=(Values['2090-b']='1');
                      if Values['2095-b']<>'' then box_dims1.turnout_info1.closure_timbers_flag:=(Values['2095-b']='1');
                      if Values['2100-b']<>'' then box_dims1.turnout_info1.xing_timbers_flag:=(Values['2100-b']='1');
                      if Values['2105-i']<>'' then box_dims1.turnout_info1.exit_timbering:=StrToInt(Values['2105-i']);
                      if Values['2110-i']<>'' then box_dims1.turnout_info1.turnout_road_code:=StrToInt(Values['2110-i']);
                      if Values['2115-e']<>'' then begin VAL(Values['2115-e'],box_dims1.turnout_info1.turnout_length,val_code); if val_code<>0 then BREAK; end;
                      if Values['2120-e']<>'' then begin VAL(Values['2120-e'],box_dims1.turnout_info1.origin_to_toe,val_code); if val_code<>0 then BREAK; end;
                      if Values['2125-e']<>'' then begin VAL(Values['2125-e'],box_dims1.turnout_info1.step_size,val_code); if val_code<>0 then BREAK; end;
                      if Values['2130-b']<>'' then box_dims1.turnout_info1.turnout_road_is_adjustable:=(Values['2130-b']='1');
                      if Values['2135-b']<>'' then box_dims1.turnout_info1.turnout_road_is_minimum:=(Values['2135-b']='1');
                      if Values['2140-i']<>'' then turnout_info2.switch_info.old_size:=StrToInt(Values['2140-i']);
                      if Values['2145-s']<>'' then turnout_info2.switch_info.sw_name_str:=Values['2145-s'];
                      if Values['2150-i']<>'' then turnout_info2.switch_info.sw_pattern:=StrToInt(Values['2150-i']);
                      if Values['2155-e']<>'' then begin VAL(Values['2155-e'],turnout_info2.switch_info.planing,val_code); if val_code<>0 then BREAK; end;
                      if Values['2160-e']<>'' then begin VAL(Values['2160-e'],turnout_info2.switch_info.planing_angle,val_code); if val_code<>0 then BREAK; end;
                      if Values['2165-e']<>'' then begin VAL(Values['2165-e'],turnout_info2.switch_info.switch_radius_inchormax,val_code); if val_code<>0 then BREAK; end;
                      if Values['2170-e']<>'' then begin VAL(Values['2170-e'],turnout_info2.switch_info.switch_rail,val_code); if val_code<>0 then BREAK; end;
                      if Values['2175-e']<>'' then begin VAL(Values['2175-e'],turnout_info2.switch_info.stock_rail,val_code); if val_code<>0 then BREAK; end;
                      if Values['2180-e']<>'' then begin VAL(Values['2180-e'],turnout_info2.switch_info.heel_lead_inches,val_code); if val_code<>0 then BREAK; end;
                      if Values['2185-e']<>'' then begin VAL(Values['2185-e'],turnout_info2.switch_info.heel_offset_inches,val_code); if val_code<>0 then BREAK; end;
                      if Values['2190-e']<>'' then begin VAL(Values['2190-e'],turnout_info2.switch_info.switch_front_inches,val_code); if val_code<>0 then BREAK; end;
                      if Values['2195-e']<>'' then begin VAL(Values['2195-e'],turnout_info2.switch_info.planing_radius,val_code); if val_code<>0 then BREAK; end;
                      if Values['2200-e']<>'' then begin VAL(Values['2200-e'],turnout_info2.switch_info.sleeper_j1,val_code); if val_code<>0 then BREAK; end;
                      if Values['2205-e']<>'' then begin VAL(Values['2205-e'],turnout_info2.switch_info.sleeper_j2,val_code); if val_code<>0 then BREAK; end;
                      if Values['2210-e']<>'' then begin VAL(Values['2210-e'],turnout_info2.switch_info.timber_centres[0],val_code); if val_code<>0 then BREAK; end;
                      if Values['2215-e']<>'' then begin VAL(Values['2215-e'],turnout_info2.switch_info.timber_centres[1],val_code); if val_code<>0 then BREAK; end;
                      if Values['2220-e']<>'' then begin VAL(Values['2220-e'],turnout_info2.switch_info.timber_centres[2],val_code); if val_code<>0 then BREAK; end;
                      if Values['2225-e']<>'' then begin VAL(Values['2225-e'],turnout_info2.switch_info.timber_centres[3],val_code); if val_code<>0 then BREAK; end;
                      if Values['2230-e']<>'' then begin VAL(Values['2230-e'],turnout_info2.switch_info.timber_centres[4],val_code); if val_code<>0 then BREAK; end;
                      if Values['2235-e']<>'' then begin VAL(Values['2235-e'],turnout_info2.switch_info.timber_centres[5],val_code); if val_code<>0 then BREAK; end;
                      if Values['2240-e']<>'' then begin VAL(Values['2240-e'],turnout_info2.switch_info.timber_centres[6],val_code); if val_code<>0 then BREAK; end;
                      if Values['2245-e']<>'' then begin VAL(Values['2245-e'],turnout_info2.switch_info.timber_centres[7],val_code); if val_code<>0 then BREAK; end;
                      if Values['2250-e']<>'' then begin VAL(Values['2250-e'],turnout_info2.switch_info.timber_centres[8],val_code); if val_code<>0 then BREAK; end;
                      if Values['2255-e']<>'' then begin VAL(Values['2255-e'],turnout_info2.switch_info.timber_centres[9],val_code); if val_code<>0 then BREAK; end;
                      if Values['2260-e']<>'' then begin VAL(Values['2260-e'],turnout_info2.switch_info.timber_centres[10],val_code); if val_code<>0 then BREAK; end;
                      if Values['2265-e']<>'' then begin VAL(Values['2265-e'],turnout_info2.switch_info.timber_centres[11],val_code); if val_code<>0 then BREAK; end;
                      if Values['2270-e']<>'' then begin VAL(Values['2270-e'],turnout_info2.switch_info.timber_centres[12],val_code); if val_code<>0 then BREAK; end;
                      if Values['2275-e']<>'' then begin VAL(Values['2275-e'],turnout_info2.switch_info.timber_centres[13],val_code); if val_code<>0 then BREAK; end;
                      if Values['2280-e']<>'' then begin VAL(Values['2280-e'],turnout_info2.switch_info.timber_centres[14],val_code); if val_code<>0 then BREAK; end;
                      if Values['2285-e']<>'' then begin VAL(Values['2285-e'],turnout_info2.switch_info.timber_centres[15],val_code); if val_code<>0 then BREAK; end;
                      if Values['2290-e']<>'' then begin VAL(Values['2290-e'],turnout_info2.switch_info.timber_centres[16],val_code); if val_code<>0 then BREAK; end;
                      if Values['2295-e']<>'' then begin VAL(Values['2295-e'],turnout_info2.switch_info.timber_centres[17],val_code); if val_code<>0 then BREAK; end;
                      if Values['2300-e']<>'' then begin VAL(Values['2300-e'],turnout_info2.switch_info.timber_centres[18],val_code); if val_code<>0 then BREAK; end;
                      if Values['2305-e']<>'' then begin VAL(Values['2305-e'],turnout_info2.switch_info.timber_centres[19],val_code); if val_code<>0 then BREAK; end;
                      if Values['2310-e']<>'' then begin VAL(Values['2310-e'],turnout_info2.switch_info.timber_centres[20],val_code); if val_code<>0 then BREAK; end;
                      if Values['2315-e']<>'' then begin VAL(Values['2315-e'],turnout_info2.switch_info.timber_centres[21],val_code); if val_code<>0 then BREAK; end;
                      if Values['2320-e']<>'' then begin VAL(Values['2320-e'],turnout_info2.switch_info.timber_centres[22],val_code); if val_code<>0 then BREAK; end;
                      if Values['2325-e']<>'' then begin VAL(Values['2325-e'],turnout_info2.switch_info.timber_centres[23],val_code); if val_code<>0 then BREAK; end;
                      if Values['2330-e']<>'' then begin VAL(Values['2330-e'],turnout_info2.switch_info.timber_centres[24],val_code); if val_code<>0 then BREAK; end;
                      if Values['2335-e']<>'' then begin VAL(Values['2335-e'],turnout_info2.switch_info.timber_centres[25],val_code); if val_code<>0 then BREAK; end;
                      if Values['2340-e']<>'' then begin VAL(Values['2340-e'],turnout_info2.switch_info.timber_centres[26],val_code); if val_code<>0 then BREAK; end;
                      if Values['2345-e']<>'' then begin VAL(Values['2345-e'],turnout_info2.switch_info.timber_centres[27],val_code); if val_code<>0 then BREAK; end;
                      if Values['2350-e']<>'' then begin VAL(Values['2350-e'],turnout_info2.switch_info.timber_centres[28],val_code); if val_code<>0 then BREAK; end;
                      if Values['2355-e']<>'' then begin VAL(Values['2355-e'],turnout_info2.switch_info.timber_centres[29],val_code); if val_code<>0 then BREAK; end;
                      if Values['2360-e']<>'' then begin VAL(Values['2360-e'],turnout_info2.switch_info.timber_centres[30],val_code); if val_code<>0 then BREAK; end;
                      if Values['2365-e']<>'' then begin VAL(Values['2365-e'],turnout_info2.switch_info.timber_centres[31],val_code); if val_code<>0 then BREAK; end;
                      if Values['2370-e']<>'' then begin VAL(Values['2370-e'],turnout_info2.switch_info.timber_centres[32],val_code); if val_code<>0 then BREAK; end;
                      if Values['2375-e']<>'' then begin VAL(Values['2375-e'],turnout_info2.switch_info.timber_centres[33],val_code); if val_code<>0 then BREAK; end;
                      if Values['2380-e']<>'' then begin VAL(Values['2380-e'],turnout_info2.switch_info.timber_centres[34],val_code); if val_code<>0 then BREAK; end;
                      if Values['2385-e']<>'' then begin VAL(Values['2385-e'],turnout_info2.switch_info.timber_centres[35],val_code); if val_code<>0 then BREAK; end;
                      if Values['2390-e']<>'' then begin VAL(Values['2390-e'],turnout_info2.switch_info.timber_centres[36],val_code); if val_code<>0 then BREAK; end;
                      if Values['2395-e']<>'' then begin VAL(Values['2395-e'],turnout_info2.switch_info.timber_centres[37],val_code); if val_code<>0 then BREAK; end;
                      if Values['2400-e']<>'' then begin VAL(Values['2400-e'],turnout_info2.switch_info.timber_centres[38],val_code); if val_code<>0 then BREAK; end;
                      if Values['2405-e']<>'' then begin VAL(Values['2405-e'],turnout_info2.switch_info.timber_centres[39],val_code); if val_code<>0 then BREAK; end;
                      if Values['2410-e']<>'' then begin VAL(Values['2410-e'],turnout_info2.switch_info.timber_centres[40],val_code); if val_code<>0 then BREAK; end;
                      if Values['2415-e']<>'' then begin VAL(Values['2415-e'],turnout_info2.switch_info.timber_centres[41],val_code); if val_code<>0 then BREAK; end;
                      if Values['2420-e']<>'' then begin VAL(Values['2420-e'],turnout_info2.switch_info.timber_centres[42],val_code); if val_code<>0 then BREAK; end;
                      if Values['2425-i']<>'' then turnout_info2.switch_info.group_code:=StrToInt(Values['2425-i']);
                      if Values['2430-i']<>'' then turnout_info2.switch_info.size_code:=StrToInt(Values['2430-i']);
                      if Values['2435-e']<>'' then begin VAL(Values['2435-e'],turnout_info2.switch_info.joggle_depth,val_code); if val_code<>0 then BREAK; end;
                      if Values['2440-e']<>'' then begin VAL(Values['2440-e'],turnout_info2.switch_info.joggle_length,val_code); if val_code<>0 then BREAK; end;
                      if Values['2445-i']<>'' then turnout_info2.switch_info.group_count:=StrToInt(Values['2445-i']);
                      if Values['2450-b']<>'' then turnout_info2.switch_info.joggled_stock_rail:=(Values['2450-b']='1');
                      if Values['2455-b']<>'' then turnout_info2.switch_info.valid_data:=(Values['2455-b']='1');
                      if Values['2460-b']<>'' then turnout_info2.switch_info.front_timbered:=(Values['2460-b']='1');
                      if Values['2465-y']<>'' then turnout_info2.switch_info.num_bridge_chairs_main_rail:=StrToInt(Values['2465-y']);
                      if Values['2470-y']<>'' then turnout_info2.switch_info.num_bridge_chairs_turnout_rail:=StrToInt(Values['2470-y']);
                      if Values['2475-e']<>'' then begin VAL(Values['2475-e'],turnout_info2.switch_info.fb_tip_offset,val_code); if val_code<>0 then BREAK; end;
                      if Values['2480-e']<>'' then begin VAL(Values['2480-e'],turnout_info2.switch_info.sleeper_j3,val_code); if val_code<>0 then BREAK; end;
                      if Values['2485-e']<>'' then begin VAL(Values['2485-e'],turnout_info2.switch_info.sleeper_j4,val_code); if val_code<>0 then BREAK; end;
                      if Values['2490-e']<>'' then begin VAL(Values['2490-e'],turnout_info2.switch_info.sleeper_j5,val_code); if val_code<>0 then BREAK; end;
                      if Values['2495-y']<>'' then turnout_info2.switch_info.num_slide_chairs:=StrToInt(Values['2495-y']);
                      if Values['2500-y']<>'' then turnout_info2.switch_info.num_block_slide_chairs:=StrToInt(Values['2500-y']);
                      if Values['2505-y']<>'' then turnout_info2.switch_info.num_block_heel_chairs:=StrToInt(Values['2505-y']);
                      if Values['2510-i']<>'' then turnout_info2.crossing_info.pattern:=StrToInt(Values['2510-i']);
                      if Values['2515-i']<>'' then turnout_info2.crossing_info.sl_mode:=StrToInt(Values['2515-i']);
                      if Values['2520-i']<>'' then turnout_info2.crossing_info.retcent_mode:=StrToInt(Values['2520-i']);
                      if Values['2525-e']<>'' then begin VAL(Values['2525-e'],turnout_info2.crossing_info.k3n_unit_angle,val_code); if val_code<>0 then BREAK; end;
                      if Values['2530-e']<>'' then begin VAL(Values['2530-e'],turnout_info2.crossing_info.fixed_st,val_code); if val_code<>0 then BREAK; end;
                      if Values['2535-i']<>'' then turnout_info2.crossing_info.hd_timbers_code:=StrToInt(Values['2535-i']);
                      if Values['2540-i']<>'' then turnout_info2.crossing_info.hd_vchecks_code:=StrToInt(Values['2540-i']);
                      if Values['2545-e']<>'' then begin VAL(Values['2545-e'],turnout_info2.crossing_info.k_check_length_1,val_code); if val_code<>0 then BREAK; end;
                      if Values['2550-e']<>'' then begin VAL(Values['2550-e'],turnout_info2.crossing_info.k_check_length_2,val_code); if val_code<>0 then BREAK; end;
                      if Values['2555-e']<>'' then begin VAL(Values['2555-e'],turnout_info2.crossing_info.k_check_mod_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2560-e']<>'' then begin VAL(Values['2560-e'],turnout_info2.crossing_info.k_check_mod_ds,val_code); if val_code<>0 then BREAK; end;
                      if Values['2565-e']<>'' then begin VAL(Values['2565-e'],turnout_info2.crossing_info.k_check_flare,val_code); if val_code<>0 then BREAK; end;
                      if Values['2570-b']<>'' then turnout_info2.crossing_info.curviform_timbering_keep:=(Values['2570-b']='1');
                      if Values['2575-i']<>'' then turnout_info2.crossing_info.main_road_code:=StrToInt(Values['2575-i']);
                      if Values['2580-i']<>'' then turnout_info2.crossing_info.tandem_timber_code:=StrToInt(Values['2580-i']);
                      if Values['2585-e']<>'' then begin VAL(Values['2585-e'],turnout_info2.crossing_info.blunt_nose_width,val_code); if val_code<>0 then BREAK; end;
                      if Values['2590-e']<>'' then begin VAL(Values['2590-e'],turnout_info2.crossing_info.blunt_nose_to_timb,val_code); if val_code<>0 then BREAK; end;
                      if Values['2595-e']<>'' then begin VAL(Values['2595-e'],turnout_info2.crossing_info.vee_joint_half_spacing,val_code); if val_code<>0 then BREAK; end;
                      if Values['2600-e']<>'' then begin VAL(Values['2600-e'],turnout_info2.crossing_info.wing_joint_spacing,val_code); if val_code<>0 then BREAK; end;
                      if Values['2605-e']<>'' then begin VAL(Values['2605-e'],turnout_info2.crossing_info.wing_timber_spacing,val_code); if val_code<>0 then BREAK; end;
                      if Values['2610-e']<>'' then begin VAL(Values['2610-e'],turnout_info2.crossing_info.vee_timber_spacing,val_code); if val_code<>0 then BREAK; end;
                      if Values['2615-y']<>'' then turnout_info2.crossing_info.vee_joint_space_co1:=StrToInt(Values['2615-y']);
                      if Values['2620-y']<>'' then turnout_info2.crossing_info.vee_joint_space_co2:=StrToInt(Values['2620-y']);
                      if Values['2625-y']<>'' then turnout_info2.crossing_info.vee_joint_space_co3:=StrToInt(Values['2625-y']);
                      if Values['2630-y']<>'' then turnout_info2.crossing_info.vee_joint_space_co4:=StrToInt(Values['2630-y']);
                      if Values['2635-y']<>'' then turnout_info2.crossing_info.vee_joint_space_co5:=StrToInt(Values['2635-y']);
                      if Values['2640-y']<>'' then turnout_info2.crossing_info.vee_joint_space_co6:=StrToInt(Values['2640-y']);
                      if Values['2645-y']<>'' then turnout_info2.crossing_info.wing_joint_space_co1:=StrToInt(Values['2645-y']);
                      if Values['2650-y']<>'' then turnout_info2.crossing_info.wing_joint_space_co2:=StrToInt(Values['2650-y']);
                      if Values['2655-y']<>'' then turnout_info2.crossing_info.wing_joint_space_co3:=StrToInt(Values['2655-y']);
                      if Values['2660-y']<>'' then turnout_info2.crossing_info.wing_joint_space_co4:=StrToInt(Values['2660-y']);
                      if Values['2665-y']<>'' then turnout_info2.crossing_info.wing_joint_space_co5:=StrToInt(Values['2665-y']);
                      if Values['2670-y']<>'' then turnout_info2.crossing_info.wing_joint_space_co6:=StrToInt(Values['2670-y']);
                      if Values['2675-e']<>'' then begin VAL(Values['2675-e'],turnout_info2.crossing_info.main_road_endx_infile,val_code); if val_code<>0 then BREAK; end;
                      if Values['2680-e']<>'' then begin VAL(Values['2680-e'],turnout_info2.crossing_info.hdkn_unit_angle,val_code); if val_code<>0 then BREAK; end;
                      if Values['2685-e']<>'' then begin VAL(Values['2685-e'],turnout_info2.crossing_info.check_flare_info_081.check_flare_ext_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2690-e']<>'' then begin VAL(Values['2690-e'],turnout_info2.crossing_info.check_flare_info_081.check_flare_ext_ts,val_code); if val_code<>0 then BREAK; end;
                      if Values['2695-e']<>'' then begin VAL(Values['2695-e'],turnout_info2.crossing_info.check_flare_info_081.check_flare_work_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2700-e']<>'' then begin VAL(Values['2700-e'],turnout_info2.crossing_info.check_flare_info_081.check_flare_work_ts,val_code); if val_code<>0 then BREAK; end;
                      if Values['2705-e']<>'' then begin VAL(Values['2705-e'],turnout_info2.crossing_info.check_flare_info_081.wing_flare_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2710-e']<>'' then begin VAL(Values['2710-e'],turnout_info2.crossing_info.check_flare_info_081.wing_flare_ts,val_code); if val_code<>0 then BREAK; end;
                      if Values['2715-e']<>'' then begin VAL(Values['2715-e'],turnout_info2.crossing_info.check_flare_info_081.check_flare_k_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2720-e']<>'' then begin VAL(Values['2720-e'],turnout_info2.crossing_info.check_flare_info_081.check_flare_k_ds,val_code); if val_code<>0 then BREAK; end;
                      if Values['2725-e']<>'' then begin VAL(Values['2725-e'],turnout_info2.crossing_info.check_flare_info_081.check_fwe_ext_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2730-e']<>'' then begin VAL(Values['2730-e'],turnout_info2.crossing_info.check_flare_info_081.check_fwe_ext_ts,val_code); if val_code<>0 then BREAK; end;
                      if Values['2735-e']<>'' then begin VAL(Values['2735-e'],turnout_info2.crossing_info.check_flare_info_081.check_fwe_work_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2740-e']<>'' then begin VAL(Values['2740-e'],turnout_info2.crossing_info.check_flare_info_081.check_fwe_work_ts,val_code); if val_code<>0 then BREAK; end;
                      if Values['2745-e']<>'' then begin VAL(Values['2745-e'],turnout_info2.crossing_info.check_flare_info_081.wing_fwe_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2750-e']<>'' then begin VAL(Values['2750-e'],turnout_info2.crossing_info.check_flare_info_081.wing_fwe_ts,val_code); if val_code<>0 then BREAK; end;
                      if Values['2755-e']<>'' then begin VAL(Values['2755-e'],turnout_info2.crossing_info.check_flare_info_081.check_fwe_k_ms,val_code); if val_code<>0 then BREAK; end;
                      if Values['2760-e']<>'' then begin VAL(Values['2760-e'],turnout_info2.crossing_info.check_flare_info_081.check_fwe_k_ds,val_code); if val_code<>0 then BREAK; end;
                      if Values['2765-e']<>'' then begin VAL(Values['2765-e'],turnout_info2.crossing_info.k_custom_wing_long_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['2770-e']<>'' then begin VAL(Values['2770-e'],turnout_info2.crossing_info.k_custom_point_long_keep,val_code); if val_code<>0 then BREAK; end;
                      if Values['2775-b']<>'' then turnout_info2.crossing_info.use_k_custom_wing_rails_keep:=(Values['2775-b']='1');
                      if Values['2780-b']<>'' then turnout_info2.crossing_info.use_k_custom_point_rails_keep:=(Values['2780-b']='1');
                      if Values['2785-b']<>'' then turnout_info2.plain_track_info.pt_custom:=(Values['2785-b']='1');
                      if Values['2790-i']<>'' then turnout_info2.plain_track_info.list_index:=StrToInt(Values['2790-i']);
                      if Values['2795-e']<>'' then begin VAL(Values['2795-e'],turnout_info2.plain_track_info.rail_length,val_code); if val_code<>0 then BREAK; end;
                      if Values['2800-i']<>'' then turnout_info2.plain_track_info.sleepers_per_length:=StrToInt(Values['2800-i']);
                      if Values['2805-e']<>'' then begin VAL(Values['2805-e'],turnout_info2.plain_track_info.sleeper_centres[0],val_code); if val_code<>0 then BREAK; end;
                      if Values['2810-e']<>'' then begin VAL(Values['2810-e'],turnout_info2.plain_track_info.sleeper_centres[1],val_code); if val_code<>0 then BREAK; end;
                      if Values['2815-e']<>'' then begin VAL(Values['2815-e'],turnout_info2.plain_track_info.sleeper_centres[2],val_code); if val_code<>0 then BREAK; end;
                      if Values['2820-e']<>'' then begin VAL(Values['2820-e'],turnout_info2.plain_track_info.sleeper_centres[3],val_code); if val_code<>0 then BREAK; end;
                      if Values['2825-e']<>'' then begin VAL(Values['2825-e'],turnout_info2.plain_track_info.sleeper_centres[4],val_code); if val_code<>0 then BREAK; end;
                      if Values['2830-e']<>'' then begin VAL(Values['2830-e'],turnout_info2.plain_track_info.sleeper_centres[5],val_code); if val_code<>0 then BREAK; end;
                      if Values['2835-e']<>'' then begin VAL(Values['2835-e'],turnout_info2.plain_track_info.sleeper_centres[6],val_code); if val_code<>0 then BREAK; end;
                      if Values['2840-e']<>'' then begin VAL(Values['2840-e'],turnout_info2.plain_track_info.sleeper_centres[7],val_code); if val_code<>0 then BREAK; end;
                      if Values['2845-e']<>'' then begin VAL(Values['2845-e'],turnout_info2.plain_track_info.sleeper_centres[8],val_code); if val_code<>0 then BREAK; end;
                      if Values['2850-e']<>'' then begin VAL(Values['2850-e'],turnout_info2.plain_track_info.sleeper_centres[9],val_code); if val_code<>0 then BREAK; end;
                      if Values['2855-e']<>'' then begin VAL(Values['2855-e'],turnout_info2.plain_track_info.sleeper_centres[10],val_code); if val_code<>0 then BREAK; end;
                      if Values['2860-e']<>'' then begin VAL(Values['2860-e'],turnout_info2.plain_track_info.sleeper_centres[11],val_code); if val_code<>0 then BREAK; end;
                      if Values['2865-e']<>'' then begin VAL(Values['2865-e'],turnout_info2.plain_track_info.sleeper_centres[12],val_code); if val_code<>0 then BREAK; end;
                      if Values['2870-e']<>'' then begin VAL(Values['2870-e'],turnout_info2.plain_track_info.sleeper_centres[13],val_code); if val_code<>0 then BREAK; end;
                      if Values['2875-e']<>'' then begin VAL(Values['2875-e'],turnout_info2.plain_track_info.sleeper_centres[14],val_code); if val_code<>0 then BREAK; end;
                      if Values['2880-e']<>'' then begin VAL(Values['2880-e'],turnout_info2.plain_track_info.sleeper_centres[15],val_code); if val_code<>0 then BREAK; end;
                      if Values['2885-e']<>'' then begin VAL(Values['2885-e'],turnout_info2.plain_track_info.sleeper_centres[16],val_code); if val_code<>0 then BREAK; end;
                      if Values['2890-e']<>'' then begin VAL(Values['2890-e'],turnout_info2.plain_track_info.sleeper_centres[17],val_code); if val_code<>0 then BREAK; end;
                      if Values['2895-e']<>'' then begin VAL(Values['2895-e'],turnout_info2.plain_track_info.sleeper_centres[18],val_code); if val_code<>0 then BREAK; end;
                      if Values['2900-e']<>'' then begin VAL(Values['2900-e'],turnout_info2.plain_track_info.sleeper_centres[19],val_code); if val_code<>0 then BREAK; end;
                      if Values['2905-e']<>'' then begin VAL(Values['2905-e'],turnout_info2.plain_track_info.sleeper_centres[20],val_code); if val_code<>0 then BREAK; end;
                      if Values['2910-e']<>'' then begin VAL(Values['2910-e'],turnout_info2.plain_track_info.sleeper_centres[21],val_code); if val_code<>0 then BREAK; end;
                      if Values['2915-e']<>'' then begin VAL(Values['2915-e'],turnout_info2.plain_track_info.sleeper_centres[22],val_code); if val_code<>0 then BREAK; end;
                      if Values['2920-e']<>'' then begin VAL(Values['2920-e'],turnout_info2.plain_track_info.sleeper_centres[23],val_code); if val_code<>0 then BREAK; end;
                      if Values['2925-e']<>'' then begin VAL(Values['2925-e'],turnout_info2.plain_track_info.sleeper_centres[24],val_code); if val_code<>0 then BREAK; end;
                      if Values['2930-e']<>'' then begin VAL(Values['2930-e'],turnout_info2.plain_track_info.sleeper_centres[25],val_code); if val_code<>0 then BREAK; end;
                      if Values['2935-e']<>'' then begin VAL(Values['2935-e'],turnout_info2.plain_track_info.sleeper_centres[26],val_code); if val_code<>0 then BREAK; end;
                      if Values['2940-e']<>'' then begin VAL(Values['2940-e'],turnout_info2.plain_track_info.sleeper_centres[27],val_code); if val_code<>0 then BREAK; end;
                      if Values['2945-e']<>'' then begin VAL(Values['2945-e'],turnout_info2.plain_track_info.sleeper_centres[28],val_code); if val_code<>0 then BREAK; end;
                      if Values['2950-e']<>'' then begin VAL(Values['2950-e'],turnout_info2.plain_track_info.sleeper_centres[29],val_code); if val_code<>0 then BREAK; end;
                      if Values['2955-e']<>'' then begin VAL(Values['2955-e'],turnout_info2.plain_track_info.sleeper_centres[30],val_code); if val_code<>0 then BREAK; end;
                      if Values['2960-e']<>'' then begin VAL(Values['2960-e'],turnout_info2.plain_track_info.sleeper_centres[31],val_code); if val_code<>0 then BREAK; end;
                      if Values['2965-e']<>'' then begin VAL(Values['2965-e'],turnout_info2.plain_track_info.sleeper_centres[32],val_code); if val_code<>0 then BREAK; end;
                      if Values['2970-e']<>'' then begin VAL(Values['2970-e'],turnout_info2.plain_track_info.sleeper_centres[33],val_code); if val_code<>0 then BREAK; end;
                      if Values['2975-e']<>'' then begin VAL(Values['2975-e'],turnout_info2.plain_track_info.sleeper_centres[34],val_code); if val_code<>0 then BREAK; end;
                      if Values['2980-e']<>'' then begin VAL(Values['2980-e'],turnout_info2.plain_track_info.sleeper_centres[35],val_code); if val_code<>0 then BREAK; end;
                      if Values['2985-e']<>'' then begin VAL(Values['2985-e'],turnout_info2.plain_track_info.sleeper_centres[36],val_code); if val_code<>0 then BREAK; end;
                      if Values['2990-e']<>'' then begin VAL(Values['2990-e'],turnout_info2.plain_track_info.sleeper_centres[37],val_code); if val_code<>0 then BREAK; end;
                      if Values['2995-e']<>'' then begin VAL(Values['2995-e'],turnout_info2.plain_track_info.sleeper_centres[38],val_code); if val_code<>0 then BREAK; end;
                      if Values['3000-e']<>'' then begin VAL(Values['3000-e'],turnout_info2.plain_track_info.sleeper_centres[39],val_code); if val_code<>0 then BREAK; end;
                      if Values['3005-e']<>'' then begin VAL(Values['3005-e'],turnout_info2.plain_track_info.sleeper_centres[40],val_code); if val_code<>0 then BREAK; end;
                      if Values['3010-e']<>'' then begin VAL(Values['3010-e'],turnout_info2.plain_track_info.sleeper_centres[41],val_code); if val_code<>0 then BREAK; end;
                      if Values['3015-e']<>'' then begin VAL(Values['3015-e'],turnout_info2.plain_track_info.sleeper_centres[42],val_code); if val_code<>0 then BREAK; end;
                      if Values['3020-e']<>'' then begin VAL(Values['3020-e'],turnout_info2.plain_track_info.sleeper_centres[43],val_code); if val_code<>0 then BREAK; end;
                      if Values['3025-e']<>'' then begin VAL(Values['3025-e'],turnout_info2.plain_track_info.sleeper_centres[44],val_code); if val_code<>0 then BREAK; end;
                      if Values['3030-e']<>'' then begin VAL(Values['3030-e'],turnout_info2.plain_track_info.sleeper_centres[45],val_code); if val_code<>0 then BREAK; end;
                      if Values['3035-e']<>'' then begin VAL(Values['3035-e'],turnout_info2.plain_track_info.sleeper_centres[46],val_code); if val_code<>0 then BREAK; end;
                      if Values['3040-e']<>'' then begin VAL(Values['3040-e'],turnout_info2.plain_track_info.sleeper_centres[47],val_code); if val_code<>0 then BREAK; end;
                      if Values['3045-e']<>'' then begin VAL(Values['3045-e'],turnout_info2.plain_track_info.sleeper_centres[48],val_code); if val_code<>0 then BREAK; end;
                      if Values['3050-e']<>'' then begin VAL(Values['3050-e'],turnout_info2.plain_track_info.sleeper_centres[49],val_code); if val_code<>0 then BREAK; end;
                      if Values['3055-e']<>'' then begin VAL(Values['3055-e'],turnout_info2.plain_track_info.sleeper_centres[50],val_code); if val_code<>0 then BREAK; end;
                      if Values['3060-e']<>'' then begin VAL(Values['3060-e'],turnout_info2.plain_track_info.sleeper_centres[51],val_code); if val_code<>0 then BREAK; end;
                      if Values['3065-i']<>'' then turnout_info2.plain_track_info.rail_joints_code:=StrToInt(Values['3065-i']);
                      if Values['3070-i']<>'' then turnout_info2.plain_track_info.user_peg_rail:=StrToInt(Values['3070-i']);
                      if Values['3075-b']<>'' then turnout_info2.plain_track_info.user_peg_data_valid:=(Values['3075-b']='1');
                      if Values['3080-e']<>'' then begin VAL(Values['3080-e'],turnout_info2.plain_track_info.user_pegx,val_code); if val_code<>0 then BREAK; end;
                      if Values['3085-e']<>'' then begin VAL(Values['3085-e'],turnout_info2.plain_track_info.user_pegy,val_code); if val_code<>0 then BREAK; end;
                      if Values['3090-e']<>'' then begin VAL(Values['3090-e'],turnout_info2.plain_track_info.user_pegk,val_code); if val_code<>0 then BREAK; end;
                      if Values['3095-s']<>'' then turnout_info2.plain_track_info.pt_spacing_name_str:=Values['3095-s'];
                      if Values['3100-y']<>'' then turnout_info2.plain_track_info.alignment_byte_6:=StrToInt(Values['3100-y']);
                      if Values['3105-e']<>'' then begin VAL(Values['3105-e'],turnout_info2.plain_track_info.pt_tb_rolling_percent,val_code); if val_code<>0 then BREAK; end;
                      if Values['3110-e']<>'' then begin VAL(Values['3110-e'],turnout_info2.plain_track_info.gaunt_sleeper_mod_inches,val_code); if val_code<>0 then BREAK; end;
                      if Values['3115-i']<>'' then turnout_info2.diamond_auto_code:=StrToInt(Values['3115-i']);
                      if Values['3120-i']<>'' then turnout_info2.bonus_timber_count:=StrToInt(Values['3120-i']);
                      if Values['3125-b']<>'' then turnout_info2.equalizing_fixed_flag:=(Values['3125-b']='1');
                      if Values['3130-b']<>'' then turnout_info2.no_timbering_flag:=(Values['3130-b']='1');
                      if Values['3135-b']<>'' then turnout_info2.angled_on_flag:=(Values['3135-b']='1');
                      if Values['3140-b']<>'' then turnout_info2.chairing_flag:=(Values['3140-b']='1');
                      if Values['3145-e']<>'' then begin VAL(Values['3145-e'],turnout_info2.start_draw_x,val_code); if val_code<>0 then BREAK; end;
                      if Values['3150-e']<>'' then begin VAL(Values['3150-e'],turnout_info2.timber_length_inc,val_code); if val_code<>0 then BREAK; end;
                      if Values['3155-b']<>'' then turnout_info2.omit_switch_front_joints:=(Values['3155-b']='1');
                      if Values['3160-b']<>'' then turnout_info2.omit_switch_rail_joints:=(Values['3160-b']='1');
                      if Values['3165-b']<>'' then turnout_info2.omit_stock_rail_joints:=(Values['3165-b']='1');
                      if Values['3170-b']<>'' then turnout_info2.omit_wing_rail_joints:=(Values['3170-b']='1');
                      if Values['3175-b']<>'' then turnout_info2.omit_vee_rail_joints:=(Values['3175-b']='1');
                      if Values['3180-b']<>'' then turnout_info2.omit_k_crossing_stock_rail_joints:=(Values['3180-b']='1');
                      if Values['3185-b']<>'' then turnout_info2.diamond_switch_timbering_flag:=(Values['3185-b']='1');
                      if Values['3190-b']<>'' then turnout_info2.gaunt_flag:=(Values['3190-b']='1');
                      if Values['3195-b']<>'' then turnout_info2.diamond_proto_timbering_flag:=(Values['3195-b']='1');
                      if Values['3200-b']<>'' then turnout_info2.semi_diamond_flag:=(Values['3200-b']='1');
                      if Values['3205-b']<>'' then turnout_info2.diamond_fixed_flag:=(Values['3205-b']='1');
                      if Values['3210-e']<>'' then begin VAL(Values['3210-e'],turnout_info2.hdk_check_rail_info.k_check_ms_1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3215-e']<>'' then begin VAL(Values['3215-e'],turnout_info2.hdk_check_rail_info.k_check_ms_2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3220-e']<>'' then begin VAL(Values['3220-e'],turnout_info2.hdk_check_rail_info.k_check_ds_1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3225-e']<>'' then begin VAL(Values['3225-e'],turnout_info2.hdk_check_rail_info.k_check_ds_2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3230-e']<>'' then begin VAL(Values['3230-e'],turnout_info2.vee_check_rail_info.v_check_ms_working1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3235-e']<>'' then begin VAL(Values['3235-e'],turnout_info2.vee_check_rail_info.v_check_ms_working2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3240-e']<>'' then begin VAL(Values['3240-e'],turnout_info2.vee_check_rail_info.v_check_ms_working3,val_code); if val_code<>0 then BREAK; end;
                      if Values['3245-e']<>'' then begin VAL(Values['3245-e'],turnout_info2.vee_check_rail_info.v_check_ts_working1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3250-e']<>'' then begin VAL(Values['3250-e'],turnout_info2.vee_check_rail_info.v_check_ts_working2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3255-e']<>'' then begin VAL(Values['3255-e'],turnout_info2.vee_check_rail_info.v_check_ts_working3,val_code); if val_code<>0 then BREAK; end;
                      if Values['3260-e']<>'' then begin VAL(Values['3260-e'],turnout_info2.vee_check_rail_info.v_check_ms_ext1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3265-e']<>'' then begin VAL(Values['3265-e'],turnout_info2.vee_check_rail_info.v_check_ms_ext2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3270-e']<>'' then begin VAL(Values['3270-e'],turnout_info2.vee_check_rail_info.v_check_ts_ext1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3275-e']<>'' then begin VAL(Values['3275-e'],turnout_info2.vee_check_rail_info.v_check_ts_ext2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3280-e']<>'' then begin VAL(Values['3280-e'],turnout_info2.vee_check_rail_info.v_wing_ms_reach1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3285-e']<>'' then begin VAL(Values['3285-e'],turnout_info2.vee_check_rail_info.v_wing_ms_reach2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3290-e']<>'' then begin VAL(Values['3290-e'],turnout_info2.vee_check_rail_info.v_wing_ts_reach1,val_code); if val_code<>0 then BREAK; end;
                      if Values['3295-e']<>'' then begin VAL(Values['3295-e'],turnout_info2.vee_check_rail_info.v_wing_ts_reach2,val_code); if val_code<>0 then BREAK; end;
                      if Values['3300-e']<>'' then begin VAL(Values['3300-e'],turnout_info2.turnout_road_endx_infile,val_code); if val_code<>0 then BREAK; end;
                      if Values['3305-s']<>'' then turnout_info2.template_type_str:=Values['3305-s'];
                      if Values['3310-e']<>'' then begin VAL(Values['3310-e'],turnout_info2.smallest_radius_stored,val_code); if val_code<>0 then BREAK; end;
                      if Values['3315-e']<>'' then begin VAL(Values['3315-e'],turnout_info2.dpx_stored,val_code); if val_code<>0 then BREAK; end;
                      if Values['3320-e']<>'' then begin VAL(Values['3320-e'],turnout_info2.ipx_stored,val_code); if val_code<>0 then BREAK; end;
                      if Values['3325-e']<>'' then begin VAL(Values['3325-e'],turnout_info2.fpx_stored,val_code); if val_code<>0 then BREAK; end;
                      if Values['3330-e']<>'' then begin VAL(Values['3330-e'],turnout_info2.gaunt_offset_inches,val_code); if val_code<>0 then BREAK; end;
                      if Values['3335-b']<>'' then turnout_info2.dxf_connector_0:=(Values['3335-b']='1');
                      if Values['3340-b']<>'' then turnout_info2.dxf_connector_t:=(Values['3340-b']='1');
                      if Values['3345-b']<>'' then turnout_info2.dxf_connector_9:=(Values['3345-b']='1');

                    end;//with list

                    if h=0 then BREAK;   // no shoved timbers

                          // get shoved timbers ...

                    st:=0;  // keep compiler happy

                    100:

                    h:=POS('`_`_`',template_str);   // start of next shoved timber or end of them

                    if h=0
                       then BREAK
                       else begin
                              next_str:=Copy(template_str,1,h-1);
                              Delete(template_str,1,h+4);            // remove it
                            end;

                    decode_list.Text:=next_str;

                    with decode_list do begin

                      if Values['9800-s']<>''
                         then begin
                                try
                                  st:=next_ti.keep_shove_list.AddObject(Values['9800-s'],Tshoved_timber.Create);   // create a new line in shove list
                                except
                                  val_code:=1;
                                  BREAK;
                                end;

                                if Values['9900-i']<>'' then Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_code:=StrToInt(Values['9900-i']);
                                if Values['9905-e']<>'' then begin VAL(Values['9905-e'],Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_x,val_code); if val_code<>0 then BREAK; end;
                                if Values['9910-e']<>'' then begin VAL(Values['9910-e'],Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_k,val_code); if val_code<>0 then BREAK; end;
                                if Values['9915-e']<>'' then begin VAL(Values['9915-e'],Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_o,val_code); if val_code<>0 then BREAK; end;
                                if Values['9920-e']<>'' then begin VAL(Values['9920-e'],Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_l,val_code); if val_code<>0 then BREAK; end;
                                if Values['9925-e']<>'' then begin VAL(Values['9925-e'],Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_w,val_code); if val_code<>0 then BREAK; end;
                                if Values['9930-e']<>'' then begin VAL(Values['9930-e'],Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_c,val_code); if val_code<>0 then BREAK; end;
                                if Values['9935-e']<>'' then begin VAL(Values['9935-e'],Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_t,val_code); if val_code<>0 then BREAK; end;
                                if Values['9940-i']<>'' then Tshoved_timber(next_ti.keep_shove_list.Objects[st]).shove_data.sv_sp_int:=StrToInt(Values['9940-i']);

                              end;

                    end;//with list

                    goto 100;

                  except
                    val_code:=1;
                  end;//try

                  BREAK;      // once only
                until 0<>0;

              end;//with next_ti

              if val_code=0 then RESULT:=True

            end;
            ////////////////////////////////////////////////////////////////////


begin
  RESULT:=False;  // init..
  loaded_str:='';
  num_templates:=0;
  valid_template:=False;

  input_list:=TStringList.Create;
  decode_list:=TStringList.Create;

  next_ti.keep_shove_list:=TStringList.Create;  // local stringlist

  input_list.LoadFromFile(file_str);

  Screen.Cursor:=crHourGlass;  // might take a while

  try

    if input_list.Count<1 then EXIT;

    n:=0;  // keep compiler happy

    input_str:=input_list.Text;

    i:=POS('```',input_str);
    Delete(input_str,1,i+2);  // remove header

    try
      num_templates:=StrToInt(input_list.Values['0000-i']);
    except
      ShowMessage('error: MECBOX import failed. No templates have been imported.');
      EXIT;
    end;

    if num_templates<1 then EXIT;

    for m:=0 to num_templates-1 do begin     // get next template

      valid_template:=False;  // init

      template_str:='';  // init..
      info_str:='';
      memo_str:='';

      fill_kd(next_ti);          // seed with the current control template

      i:=POS('```',input_str);   // end of this one

      if i=0 then BREAK;

      j:=POS('`!`!`',input_str);   // start of info text

      if j=0 then BREAK;

      template_str:=Copy(input_str,1,j-1);

      Delete(input_str,1,j+4);  // remove it

      j:=POS('`@`@`',input_str);   // start of memo text

      if j=0 then BREAK;

      info_str:=Copy(input_str,1,j-1);

      Delete(input_str,1,j+4);  // remove it

      j:=POS('`~`~`',input_str);   // end of memo text

      if j=0 then BREAK;

      memo_str:=Copy(input_str,1,j-1);

      Delete(input_str,1,j+4);  // remove it

      j:=POS('```',input_str);  // to next template

      if j=0 then BREAK;

      Delete(input_str,1,j+2);  // remove to start of next

      if decode_template=False
         then begin
                clear_keeps(False,False);   // first clear any loaded, don't save it for undo
                ShowMessage('error: MECBOX import failed at template '+IntToStr(m)+#13+#13+'There is a problem in the file. No templates have been imported.');
                EXIT;
              end;

      test_str:=next_ti.keep_dims.box_dims1.box_ident;      // double check...

      if NOT ( (test_str=('N '+IntToStr(m))) or (test_str=('NX'+IntToStr(num_templates-1))) )  // error reading, or this is not a template.
         then begin
                clear_keeps(False,False);   // first clear any loaded, don't save it for undo
                ShowMessage('error: MECBOX import failed at template '+IntToStr(m)+#13+#13+'There is a problem in the file. No templates have been imported.');
                EXIT;
              end;

      try
        n:=keeps_list.AddObject(info_str,Ttemplate.Create);   // create and append a new line in keeps list
        if memo_list.Add(memo_str)<>n then run_error(197);    // and memo list. Ensure indices correspond

      except
        alert(1,'      memory  problem',
                'Unable to import templates from the file into your storage box because of memory problems.'
               +'||(Importing terminated after  '+IntToStr(m)+'  templates.)',
                '','','','','cancel  import','',0);
        EXIT;
      end;//try

      init_ttemplate(n);   // sets flags and creates the shoves StringList

        // check imported data for version update needed...

      okd.old_keep_dims1.box_dims1:=next_ti.keep_dims.box_dims1;            // this old data format used by version_mismatch()
      okd.old_keep_dims2.turnout_info2:=next_ti.keep_dims.turnout_info2;

      if version_mismatch(okd)=True    // mismatch found
         then begin
                next_ti.keep_dims.box_dims1:=okd.old_keep_dims1.box_dims1;
                next_ti.keep_dims.turnout_info2:=okd.old_keep_dims2.turnout_info2;
              end;

        // data updated if needed - now can copy it into the box list..

      copy_template_info_from_to(True,next_ti,Ttemplate(keeps_list.Objects[n]).template_info);  // True = free next_ti shovelist and its data objects

        // and recreate shove list for next template..

      next_ti.keep_shove_list:=TStringList.Create;

      valid_template:=True;
      
    end;//next template

    if valid_template=False
       then begin
              clear_keeps(False,False);   // first clear any loaded, don't save it for undo
              ShowMessage('error: MECBOX import failed at template '+IntToStr(m)+#13+#13+'There is a problem in the file.');
              EXIT;
            end;

    loaded_str:=file_str;

    with next_ti.keep_dims.box_dims1 do begin

      box_project_title_str:=project_for;  // change the title to the one loaded last.

            // saved grid info -- read from last template only...
            // 0.91.d -- read these only if prefs not being used on startup.

      if (grid_units_code<>0) and (user_prefs_in_use=False)    // change grid to as loaded...
         then begin

                grid_labels_code_i:=grid_units_code;

                grid_spacex:=x_grid_spacing;
                grid_spacey:=y_grid_spacing;

                if ruler_units=0 then update_ruler_div;   // ruler as grid option

              end;

    end;//with

    keep_form.box_file_label.Caption:=' last reloaded from :  '+loaded_str;
    keep_form.box_file_label.Hint:=keep_form.box_file_label.Caption;  // in case too long for caption

    saved_box_str:=loaded_str;                 // for print of box contents list.
    reloaded_box_str:='|    '+loaded_str;      // ditto.

    current_state(0);     // update or create listbox entries

    if keeps_list.Count<1 then EXIT;  // cleared on error or nothing loaded.

    for n:=0 to (keeps_list.Count-1) do begin
      if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077=1
         then begin
                list_position:=n;                       // put new keep on background.
                copy_keep_to_background(n,False,True);  // don't update info, reloading=True.
              end;
    end;//for

    pad_form.fit_bgnd_menu_entry.Click;     // show the new background templates.

    backup_wanted:=True;                    // new file loaded, update the backup.

    RESULT:=True;                           // file loaded.

  finally
    next_ti.keep_shove_list.Free;
    input_list.Free;
    decode_list.Free;

    Screen.Cursor:=crDefault;
  end;
end;
//______________________________________________________________________________

function import_mecbox(file_str:string):boolean; // 290a

var
  mecbox_str:string;

  i:integer;

begin
  RESULT:=False;  // init

  if (keeps_list.Count>0) and (save_done=False) // something already there and not saved... ?
     then begin
            i:=alert(7,'    import  MECBOX  into  storage  box  -  save  first ?',
                       'Your storage box contains one or more templates which have not yet been saved.'
                      +'||Importing a MECBOX file into your storage box will replace all of the existing templates and background track plan.'
                      +'||These existing templates can be restored later by clicking the `0FILES > UNDO RELOAD / UNDO CLEAR`1 menu item.'
                      +' But if any of these templates may be needed again, you should first save them in a named data file.',
                       '','','','replace  existing  contents  without  saving','cancel  import','save  existing  contents  before  importing',0);
            case i of
                5: EXIT;
                6: if save_box(0,0,0,'')=False then EXIT;     // go save all the keeps box.
            end;//case
          end;

  if file_str=''
     then begin

            with mecbox_form.load_mecbox_dialog do begin

            Title:='    load  or  reload  storage  box  from  MECBOX  file ..';

            if his_load_file_name<>'' then InitialDir:=ExtractFilePath(his_load_file_name)
                                      else InitialDir:=exe_str+'BOX-FILES\';

            Filter:=' storage  box  contents  in  transfer  format  (*.mecbox)|*.mecbox';
            Filename:='*.mecbox';

            if Execute=False then EXIT;          // get the file name.

            mecbox_str:=FileName;

            his_load_file_name:=mecbox_str;       // so we can use the same folder next time.

            end;//with

          end
     else mecbox_str:=file_str;

  if FileExists(mecbox_str)=False
   then begin
          alert(5,'    error  -  file  not  found',
                  '||The file :'
                 +'||'+mecbox_str
                 +'||is not available. Please check that the file you require exists in the named folder. Then try again.'
                 +'||No changes have been made to your storage box.',
                 '','','','','cancel  reload','',0);
          EXIT;
        end;

      // init for version_mismatch..

  loaded_version:=50000;   // init for lowest template version in the file
  later_file:=False;       // init

  loading_in_progress:=True;   // lock out any auto backups while loading -- in case any dialogs get shown and OnIdle fires

  wait_form.cancel_button.Hide;
  wait_form.waiting_label.Caption:='importing  MECBOX  templates ...';
  wait_form.waiting_label.Width:=wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // bug fix for Wine
  wait_form.Show;

  if Application.Terminated=False then Application.ProcessMessages;           // let the wait form fully paint.

  clear_keeps(False,True);   // first clear all existing (sets save_done:=True), and save existing for undo.

  if do_import(mecbox_str)=False                             // do the import
     then ShowMessage('error: file : '+mecbox_str
                      +#13+#13+'did not load correctly');

  // out for import - save_done:= NOT resave_needed;    // this boxful matches file.

  if keeps_list.Count>0 then save_done:=False;      // if any imported, need saving in .box format after import

  boxmru_update(mecbox_str);   // update the mru list.

  RESULT:=True;                // file loaded.

  if later_file=True
     then begin
            alert(1,'php/980    later  file   -   ( from  version  '+FormatFloat('0.00',loaded_version/100)+' )',
                    'The file which you just reloaded contained one or more templates from a later version of Templot than this one.'
                   +' Some features may not be available or may be drawn differently.'
                   +'||The earliest loaded template was from version  '+FormatFloat('0.00',loaded_version/100)
                   +'|This version of Templot is  '+GetVersionString(voShort)
                   +'||Please refer to the Templot web site at  templot.com  for information about upgrading to the latest version, or click| <A HREF="online_ref980.85a">more information online</A> .',
                   '','','','','','continue',0);
          end;

  if loaded_version<200        // loaded from ealier version than this
     then begin
            i:=alert(2,'php/980    old  file   -   ( from  version  '+FormatFloat('0.00',loaded_version/100)+' )',
                    'The file which you just reloaded contained one or more templates from an earlier version of Templot.'
                   +'||These have been modified to make them compatible with this version, but some features may now be drawn differently or require adjustment.'
                   +'||The earliest loaded template was from version  '+FormatFloat('0.00',loaded_version/100)
                   +'|This version of Templot is  '+GetVersionString(voShort)
                   +'||Click for <A HREF="online_ref980.85a">more information online</A> about the differences between these two versions.'
                   +'||green_panel_begin tree.gif The template name labels are now shown in the boxed style by default.'
                   +' To revert to the previous style click the `0trackpad > trackpad background options > background name labels > transparent`1 menu item,|or click below.'
                   +'||To hide the name labels, press the `0END`2 key on the keyboard, or the `0SHIFT+ENTER`2 keys, or click the `0trackpad > hide name labels`1 menu item, or click below.green_panel_end',
                   '','','hide  name  labels','change  to  transparent  name  labels','','continue',0);

            if i=3 then hide_name_labels:=True;

            if i=4 then pad_form.transparent_names_menu_entry.Checked:=True;    // radio item.
          end;

  wait_form.Close;
  current_state(-1);

  loading_in_progress:=False;  // allow backups only after dialogs

  show_and_redraw(True,True);  // redraw pad when ready.
end;
//______________________________________________________________________________

end.
