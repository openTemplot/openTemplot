{
                          ===== WARNING =====

                  This include file is generated.
    It should not be edited by hand unless the idea of generating it is being
                       abandoned for good. 
  Once the generation process is discarded there will probably be no going back.
        This may be fine - just make sure that is what you want.  ;-)
}
// 0 0  >>>  
  // 1 1  >>>  old_keep_dims1
    // 2 1  >>>  box_dims1
      old_keep_dims1.box_dims1.box_ident := parse_string(box_file, 'box_ident', 10);
      old_keep_dims1.box_dims1.id_byte := parse_byte(box_file, 'id_byte');
      old_keep_dims1.box_dims1.now_time := parse_integer(box_file, 'now_time');
      old_keep_dims1.box_dims1.keep_date := parse_string(box_file, 'keep_date', 20);
      old_keep_dims1.box_dims1.keep_time := parse_string(box_file, 'keep_time', 20);
      old_keep_dims1.box_dims1.top_label := parse_string(box_file, 'top_label', 100);
      old_keep_dims1.box_dims1.project_for := parse_string(box_file, 'project_for', 50);
      old_keep_dims1.box_dims1.reference_string := parse_string(box_file, 'reference_string', 100);
      old_keep_dims1.box_dims1.this_was_control_template := parse_boolean(box_file, 'this_was_control_template');
      // 3 10  >>>  rail_info
        old_keep_dims1.box_dims1.rail_info.flared_ends_ri := parse_integer(box_file, 'flared_ends_ri');
        old_keep_dims1.box_dims1.rail_info.spare_int1 := parse_integer(box_file, 'spare_int1');
        old_keep_dims1.box_dims1.rail_info.knuckle_code_ri := parse_integer(box_file, 'knuckle_code_ri');
        old_keep_dims1.box_dims1.rail_info.knuckle_radius_ri := parse_extended(box_file, 'knuckle_radius_ri');
        old_keep_dims1.box_dims1.rail_info.spare_float2 := 0;   // spare_float2
         parse_skip(box_file,  10 );
        old_keep_dims1.box_dims1.rail_info.spare_bool1 := parse_boolean(box_file, 'spare_bool1');
        old_keep_dims1.box_dims1.rail_info.spare_bool2 := parse_boolean(box_file, 'spare_bool2');
        old_keep_dims1.box_dims1.rail_info.isolated_crossing_sw := parse_boolean(box_file, 'isolated_crossing_sw');
        old_keep_dims1.box_dims1.rail_info.k_diagonal_side_check_rail_sw := parse_boolean(box_file, 'k_diagonal_side_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.k_main_side_check_rail_sw := parse_boolean(box_file, 'k_main_side_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.switch_drive_sw := parse_boolean(box_file, 'switch_drive_sw');
        old_keep_dims1.box_dims1.rail_info.track_centre_lines_sw := parse_boolean(box_file, 'track_centre_lines_sw');
        old_keep_dims1.box_dims1.rail_info.turnout_road_stock_rail_sw := parse_boolean(box_file, 'turnout_road_stock_rail_sw');
        old_keep_dims1.box_dims1.rail_info.turnout_road_check_rail_sw := parse_boolean(box_file, 'turnout_road_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.turnout_road_crossing_rail_sw := parse_boolean(box_file, 'turnout_road_crossing_rail_sw');
        old_keep_dims1.box_dims1.rail_info.crossing_vee_sw := parse_boolean(box_file, 'crossing_vee_sw');
        old_keep_dims1.box_dims1.rail_info.main_road_crossing_rail_sw := parse_boolean(box_file, 'main_road_crossing_rail_sw');
        old_keep_dims1.box_dims1.rail_info.main_road_check_rail_sw := parse_boolean(box_file, 'main_road_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.main_road_stock_rail_sw := parse_boolean(box_file, 'main_road_stock_rail_sw');
        old_keep_dims1.box_dims1.rail_info.alignment_byte_1 := parse_byte(box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.rail_info.alignment_byte_2 := parse_byte(box_file, 'alignment_byte_2');
      // 3 10  <<<  rail_info
      old_keep_dims1.box_dims1.auto_restore_on_startup := parse_boolean(box_file, 'auto_restore_on_startup');
      old_keep_dims1.box_dims1.ask_restore_on_startup := parse_boolean(box_file, 'ask_restore_on_startup');
      old_keep_dims1.box_dims1.pre077_bgnd_flag := parse_boolean(box_file, 'pre077_bgnd_flag');
      old_keep_dims1.box_dims1.alignment_byte_3 := parse_byte(box_file, 'alignment_byte_3');
      old_keep_dims1.box_dims1.templot_version := parse_integer(box_file, 'templot_version');
      old_keep_dims1.box_dims1.file_format_code := parse_integer(box_file, 'file_format_code');
      old_keep_dims1.box_dims1.gauge_index := parse_integer(box_file, 'gauge_index');
      old_keep_dims1.box_dims1.gauge_exact := parse_boolean(box_file, 'gauge_exact');
      old_keep_dims1.box_dims1.gauge_custom := parse_boolean(box_file, 'gauge_custom');
      // 3 20  >>>  proto_info
        old_keep_dims1.box_dims1.proto_info.name_str_pi := parse_string(box_file, 'name_str_pi', 15);
        old_keep_dims1.box_dims1.proto_info.spare_str_pi := parse_string(box_file, 'spare_str_pi', 75);
        old_keep_dims1.box_dims1.proto_info.scale_pi := parse_extended(box_file, 'scale_pi');
        old_keep_dims1.box_dims1.proto_info.gauge_pi := parse_extended(box_file, 'gauge_pi');
        old_keep_dims1.box_dims1.proto_info.fw_pi := parse_extended(box_file, 'fw_pi');
        old_keep_dims1.box_dims1.proto_info.fwe_pi := parse_extended(box_file, 'fwe_pi');
        old_keep_dims1.box_dims1.proto_info.xing_fl_pi := parse_extended(box_file, 'xing_fl_pi');
        old_keep_dims1.box_dims1.proto_info.railtop_pi := parse_extended(box_file, 'railtop_pi');
        old_keep_dims1.box_dims1.proto_info.trtscent_pi := parse_extended(box_file, 'trtscent_pi');
        old_keep_dims1.box_dims1.proto_info.trmscent_pi := parse_extended(box_file, 'trmscent_pi');
        old_keep_dims1.box_dims1.proto_info.retcent_pi := parse_extended(box_file, 'retcent_pi');
        old_keep_dims1.box_dims1.proto_info.min_radius_pi := parse_extended(box_file, 'min_radius_pi');
        old_keep_dims1.box_dims1.proto_info.old_winglongs_pi := parse_extended(box_file, 'old_winglongs_pi');
        old_keep_dims1.box_dims1.proto_info.old_winglongl_pi := parse_extended(box_file, 'old_winglongl_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongs_pi := parse_extended(box_file, 'old_cklongs_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongm_pi := parse_extended(box_file, 'old_cklongm_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongl_pi := parse_extended(box_file, 'old_cklongl_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongxl_pi := parse_extended(box_file, 'old_cklongxl_pi');
        old_keep_dims1.box_dims1.proto_info.tbwide_pi := parse_extended(box_file, 'tbwide_pi');
        old_keep_dims1.box_dims1.proto_info.slwide_pi := parse_extended(box_file, 'slwide_pi');
        old_keep_dims1.box_dims1.proto_info.xtimbsp_pi := parse_extended(box_file, 'xtimbsp_pi');
        old_keep_dims1.box_dims1.proto_info.ftimbspmax_pi := parse_extended(box_file, 'ftimbspmax_pi');
        old_keep_dims1.box_dims1.proto_info.tb_pi := parse_extended(box_file, 'tb_pi');
        old_keep_dims1.box_dims1.proto_info.mainside_ends_pi := parse_boolean(box_file, 'mainside_ends_pi');
        old_keep_dims1.box_dims1.proto_info.jt_slwide_pi := parse_float(box_file, 'jt_slwide_pi');
        old_keep_dims1.box_dims1.proto_info.alignment_byte_1 := parse_byte(box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.proto_info.random_end_pi := parse_extended(box_file, 'random_end_pi');
        old_keep_dims1.box_dims1.proto_info.timber_thick_pi := parse_extended(box_file, 'timber_thick_pi');
        old_keep_dims1.box_dims1.proto_info.random_angle_pi := parse_extended(box_file, 'random_angle_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_working1_pi := parse_extended(box_file, 'ck_ms_working1_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_working2_pi := parse_extended(box_file, 'ck_ms_working2_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_working3_pi := parse_extended(box_file, 'ck_ms_working3_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ts_working_mod_pi := parse_extended(box_file, 'ck_ts_working_mod_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_ext1_pi := parse_extended(box_file, 'ck_ms_ext1_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_ext2_pi := parse_extended(box_file, 'ck_ms_ext2_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ts_ext_mod_pi := parse_extended(box_file, 'ck_ts_ext_mod_pi');
        old_keep_dims1.box_dims1.proto_info.wing_ms_reach1_pi := parse_extended(box_file, 'wing_ms_reach1_pi');
        old_keep_dims1.box_dims1.proto_info.wing_ms_reach2_pi := parse_extended(box_file, 'wing_ms_reach2_pi');
        old_keep_dims1.box_dims1.proto_info.wing_ts_reach_mod_pi := parse_extended(box_file, 'wing_ts_reach_mod_pi');
        old_keep_dims1.box_dims1.proto_info.railbottom_pi := parse_extended(box_file, 'railbottom_pi');
        old_keep_dims1.box_dims1.proto_info.rail_height_pi := parse_extended(box_file, 'rail_height_pi');
        old_keep_dims1.box_dims1.proto_info.seat_thick_pi := parse_extended(box_file, 'seat_thick_pi');
        old_keep_dims1.box_dims1.proto_info.old_tb_pi := parse_extended(box_file, 'old_tb_pi');
        old_keep_dims1.box_dims1.proto_info.rail_inclination_pi := parse_extended(box_file, 'rail_inclination_pi');
        old_keep_dims1.box_dims1.proto_info.foot_height_pi := parse_extended(box_file, 'foot_height_pi');
        old_keep_dims1.box_dims1.proto_info.chair_outlen_pi := parse_extended(box_file, 'chair_outlen_pi');
        old_keep_dims1.box_dims1.proto_info.chair_inlen_pi := parse_extended(box_file, 'chair_inlen_pi');
        old_keep_dims1.box_dims1.proto_info.chair_width_pi := parse_extended(box_file, 'chair_width_pi');
        old_keep_dims1.box_dims1.proto_info.chair_corner_pi := parse_extended(box_file, 'chair_corner_pi');
        old_keep_dims1.box_dims1.proto_info.spare_byte1 := parse_byte(box_file, 'spare_byte1');
        old_keep_dims1.box_dims1.proto_info.spare_byte2 := parse_byte(box_file, 'spare_byte2');
        old_keep_dims1.box_dims1.proto_info.spare_byte3 := parse_byte(box_file, 'spare_byte3');
        old_keep_dims1.box_dims1.proto_info.spare_byte4 := parse_byte(box_file, 'spare_byte4');
        old_keep_dims1.box_dims1.proto_info.spare_byte5 := parse_byte(box_file, 'spare_byte5');
        old_keep_dims1.box_dims1.proto_info.alignment_byte_2 := parse_byte(box_file, 'alignment_byte_2');
      // 3 20  <<<  proto_info
      old_keep_dims1.box_dims1.railtop_inches := parse_extended(box_file, 'railtop_inches');
      old_keep_dims1.box_dims1.railbottom_inches := parse_extended(box_file, 'railbottom_inches');
      old_keep_dims1.box_dims1.alignment_byte_4 := parse_byte(box_file, 'alignment_byte_4');
      old_keep_dims1.box_dims1.alignment_byte_5 := parse_byte(box_file, 'alignment_byte_5');
      old_keep_dims1.box_dims1.version_as_loaded := parse_integer(box_file, 'version_as_loaded');
      old_keep_dims1.box_dims1.bgnd_code_077 := parse_integer(box_file, 'bgnd_code_077');
      old_keep_dims1.box_dims1.print_mapping_colour := parse_integer(box_file, 'print_mapping_colour');
      old_keep_dims1.box_dims1.pad_marker_colour := parse_integer(box_file, 'pad_marker_colour');
      old_keep_dims1.box_dims1.use_print_mapping_colour := parse_boolean(box_file, 'use_print_mapping_colour');
      old_keep_dims1.box_dims1.use_pad_marker_colour := parse_boolean(box_file, 'use_pad_marker_colour');
      old_keep_dims1.box_dims1.spare_bool1 := parse_boolean(box_file, 'spare_bool1');
      old_keep_dims1.box_dims1.spare_bool2 := parse_boolean(box_file, 'spare_bool2');
      old_keep_dims1.box_dims1.spare_int2 := parse_integer(box_file, 'spare_int2');
      old_keep_dims1.box_dims1.grid_units_code := parse_integer(box_file, 'grid_units_code');
      old_keep_dims1.box_dims1.x_grid_spacing := parse_extended(box_file, 'x_grid_spacing');
      old_keep_dims1.box_dims1.y_grid_spacing := parse_extended(box_file, 'y_grid_spacing');
      old_keep_dims1.box_dims1.total_length_of_timbering := parse_extended(box_file, 'total_length_of_timbering');
      old_keep_dims1.box_dims1.id_number := parse_integer(box_file, 'id_number');
      old_keep_dims1.box_dims1.id_number_str := parse_string(box_file, 'id_number_str', 7);
      old_keep_dims1.box_dims1.spare_boolean1 := parse_boolean(box_file, 'spare_boolean1');
      old_keep_dims1.box_dims1.spare_boolean2 := parse_boolean(box_file, 'spare_boolean2');
      // 3 42  >>>  transform_info
        old_keep_dims1.box_dims1.transform_info.datum_y := parse_extended(box_file, 'datum_y');
        old_keep_dims1.box_dims1.transform_info.x_go_limit := parse_extended(box_file, 'x_go_limit');
        old_keep_dims1.box_dims1.transform_info.x_stop_limit := parse_extended(box_file, 'x_stop_limit');
        old_keep_dims1.box_dims1.transform_info.transforms_apply := parse_boolean(box_file, 'transforms_apply');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_1 := parse_byte(box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.transform_info.x1_shift := parse_extended(box_file, 'x1_shift');
        old_keep_dims1.box_dims1.transform_info.y1_shift := parse_extended(box_file, 'y1_shift');
        old_keep_dims1.box_dims1.transform_info.k_shift := parse_extended(box_file, 'k_shift');
        old_keep_dims1.box_dims1.transform_info.x2_shift := parse_extended(box_file, 'x2_shift');
        old_keep_dims1.box_dims1.transform_info.y2_shift := parse_extended(box_file, 'y2_shift');
        // 4 11  >>>  peg_pos
          old_keep_dims1.box_dims1.transform_info.peg_pos.x := parse_extended(box_file, 'x');
          old_keep_dims1.box_dims1.transform_info.peg_pos.y := parse_extended(box_file, 'y');
        // 4 11  <<<  peg_pos
        old_keep_dims1.box_dims1.transform_info.alignment_byte_2 := parse_byte(box_file, 'alignment_byte_2');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_3 := parse_byte(box_file, 'alignment_byte_3');
        old_keep_dims1.box_dims1.transform_info.peg_point_code := parse_integer(box_file, 'peg_point_code');
        old_keep_dims1.box_dims1.transform_info.peg_point_rail := parse_integer(box_file, 'peg_point_rail');
        old_keep_dims1.box_dims1.transform_info.mirror_on_x := parse_boolean(box_file, 'mirror_on_x');
        old_keep_dims1.box_dims1.transform_info.mirror_on_y := parse_boolean(box_file, 'mirror_on_y');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_4 := parse_byte(box_file, 'alignment_byte_4');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_5 := parse_byte(box_file, 'alignment_byte_5');
        old_keep_dims1.box_dims1.transform_info.spare_int1 := parse_integer(box_file, 'spare_int1');
        old_keep_dims1.box_dims1.transform_info.spare_int2 := parse_integer(box_file, 'spare_int2');
        old_keep_dims1.box_dims1.transform_info.spare_flag1 := parse_boolean(box_file, 'spare_flag1');
        old_keep_dims1.box_dims1.transform_info.spare_flag2 := parse_boolean(box_file, 'spare_flag2');
        old_keep_dims1.box_dims1.transform_info.spare_flag3 := parse_boolean(box_file, 'spare_flag3');
        old_keep_dims1.box_dims1.transform_info.spare_flag4 := parse_boolean(box_file, 'spare_flag4');
        // 4 26  >>>  notch_info
          old_keep_dims1.box_dims1.transform_info.notch_info.notch_x := parse_extended(box_file, 'notch_x');
          old_keep_dims1.box_dims1.transform_info.notch_info.notch_y := parse_extended(box_file, 'notch_y');
          old_keep_dims1.box_dims1.transform_info.notch_info.notch_k := parse_extended(box_file, 'notch_k');
        // 4 26  <<<  notch_info
        old_keep_dims1.box_dims1.transform_info.spare_str := parse_string(box_file, 'spare_str', 10);
        old_keep_dims1.box_dims1.transform_info.alignment_byte_6 := parse_byte(box_file, 'alignment_byte_6');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_7 := parse_byte(box_file, 'alignment_byte_7');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_8 := parse_byte(box_file, 'alignment_byte_8');
      // 3 42  <<<  transform_info
      // 3 43  >>>  platform_trackbed_info
        old_keep_dims1.box_dims1.platform_trackbed_info.adjacent_edges_keep := parse_boolean(box_file, 'adjacent_edges_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_trackbed_edge_keep := parse_boolean(box_file, 'draw_ms_trackbed_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_trackbed_edge_keep := parse_boolean(box_file, 'draw_ts_trackbed_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool1 := parse_boolean(box_file, 'spare_bool1');
        old_keep_dims1.box_dims1.platform_trackbed_info.OUT_OF_USE_trackbed_width_ins_keep := parse_extended(box_file, 'OUT_OF_USE_trackbed_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_keep := parse_boolean(box_file, 'draw_ts_platform_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_start_edge_keep := parse_boolean(box_file, 'draw_ts_platform_start_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_end_edge_keep := parse_boolean(box_file, 'draw_ts_platform_end_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_rear_edge_keep := parse_boolean(box_file, 'draw_ts_platform_rear_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_front_edge_ins_keep := parse_extended(box_file, 'platform_ts_front_edge_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_start_width_ins_keep := parse_extended(box_file, 'platform_ts_start_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_end_width_ins_keep := parse_extended(box_file, 'platform_ts_end_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_start_mm_keep := parse_extended(box_file, 'platform_ts_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_length_mm_keep := parse_extended(box_file, 'platform_ts_length_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_keep := parse_boolean(box_file, 'draw_ms_platform_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_start_edge_keep := parse_boolean(box_file, 'draw_ms_platform_start_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_end_edge_keep := parse_boolean(box_file, 'draw_ms_platform_end_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_rear_edge_keep := parse_boolean(box_file, 'draw_ms_platform_rear_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_front_edge_ins_keep := parse_extended(box_file, 'platform_ms_front_edge_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_start_width_ins_keep := parse_extended(box_file, 'platform_ms_start_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_end_width_ins_keep := parse_extended(box_file, 'platform_ms_end_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_start_mm_keep := parse_extended(box_file, 'platform_ms_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_length_mm_keep := parse_extended(box_file, 'platform_ms_length_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.OUT_OF_USE_cess_width_ins_keep := parse_extended(box_file, 'OUT_OF_USE_cess_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.OUT_OF_USE_draw_trackbed_cess_edge_keep := parse_boolean(box_file, 'OUT_OF_USE_draw_trackbed_cess_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_start_skew_mm_keep := parse_extended(box_file, 'platform_ms_start_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_end_skew_mm_keep := parse_extended(box_file, 'platform_ms_end_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_start_skew_mm_keep := parse_extended(box_file, 'platform_ts_start_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_end_skew_mm_keep := parse_extended(box_file, 'platform_ts_end_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool2 := parse_boolean(box_file, 'spare_bool2');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool3 := parse_boolean(box_file, 'spare_bool3');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool4 := parse_boolean(box_file, 'spare_bool4');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool5 := parse_boolean(box_file, 'spare_bool5');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool6 := parse_boolean(box_file, 'spare_bool6');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool7 := parse_boolean(box_file, 'spare_bool7');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool8 := parse_boolean(box_file, 'spare_bool8');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ms_width_ins_keep := parse_float(box_file, 'trackbed_ms_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ts_width_ins_keep := parse_float(box_file, 'trackbed_ts_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.cess_ms_width_ins_keep := parse_float(box_file, 'cess_ms_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.cess_ts_width_ins_keep := parse_float(box_file, 'cess_ts_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep := parse_boolean(box_file, 'draw_ms_trackbed_cess_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep := parse_boolean(box_file, 'draw_ts_trackbed_cess_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare1 := parse_boolean(box_file, 'spare1');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare2 := parse_boolean(box_file, 'spare2');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ms_start_mm_keep := parse_extended(box_file, 'trackbed_ms_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ms_length_mm_keep := parse_extended(box_file, 'trackbed_ms_length_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ts_start_mm_keep := parse_extended(box_file, 'trackbed_ts_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ts_length_mm_keep := parse_extended(box_file, 'trackbed_ts_length_mm_keep');
      // 3 43  <<<  platform_trackbed_info
      // 3 44  >>>  align_info
        old_keep_dims1.box_dims1.align_info.curving_flag := parse_boolean(box_file, 'curving_flag');
        old_keep_dims1.box_dims1.align_info.trans_flag := parse_boolean(box_file, 'trans_flag');
        old_keep_dims1.box_dims1.align_info.fixed_rad := parse_extended(box_file, 'fixed_rad');
        old_keep_dims1.box_dims1.align_info.trans_rad1 := parse_extended(box_file, 'trans_rad1');
        old_keep_dims1.box_dims1.align_info.trans_rad2 := parse_extended(box_file, 'trans_rad2');
        old_keep_dims1.box_dims1.align_info.trans_length := parse_extended(box_file, 'trans_length');
        old_keep_dims1.box_dims1.align_info.trans_start := parse_extended(box_file, 'trans_start');
        old_keep_dims1.box_dims1.align_info.rad_offset := parse_extended(box_file, 'rad_offset');
        old_keep_dims1.box_dims1.align_info.alignment_byte_1 := parse_byte(box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.align_info.alignment_byte_2 := parse_byte(box_file, 'alignment_byte_2');
        old_keep_dims1.box_dims1.align_info.tanh_kmax := parse_double(box_file, 'tanh_kmax');
        old_keep_dims1.box_dims1.align_info.slewing_flag := parse_boolean(box_file, 'slewing_flag');
        old_keep_dims1.box_dims1.align_info.cl_only_flag := parse_boolean(box_file, 'cl_only_flag');
        old_keep_dims1.box_dims1.align_info.slew_type := parse_byte(box_file, 'slew_type');
        old_keep_dims1.box_dims1.align_info.dummy_template_flag := parse_boolean(box_file, 'dummy_template_flag');
        old_keep_dims1.box_dims1.align_info.slew_start := parse_extended(box_file, 'slew_start');
        old_keep_dims1.box_dims1.align_info.slew_length := parse_extended(box_file, 'slew_length');
        old_keep_dims1.box_dims1.align_info.slew_amount := parse_extended(box_file, 'slew_amount');
        old_keep_dims1.box_dims1.align_info.cl_options_code_int := parse_integer(box_file, 'cl_options_code_int');
        old_keep_dims1.box_dims1.align_info.cl_options_custom_offset_ext := parse_extended(box_file, 'cl_options_custom_offset_ext');
        old_keep_dims1.box_dims1.align_info.reminder_flag := parse_boolean(box_file, 'reminder_flag');
        old_keep_dims1.box_dims1.align_info.reminder_colour := parse_integer(box_file, 'reminder_colour');
        old_keep_dims1.box_dims1.align_info.reminder_str := parse_string(box_file, 'reminder_str', 200);
        old_keep_dims1.box_dims1.align_info.spare_float1 := 0;   // spare_float1
         parse_skip(box_file,  10 );
        old_keep_dims1.box_dims1.align_info.spare_float2 := 0;   // spare_float2
         parse_skip(box_file,  10 );
        old_keep_dims1.box_dims1.align_info.spare_float3 := 0;   // spare_float3
         parse_skip(box_file,  10 );
        old_keep_dims1.box_dims1.align_info.spare_int := parse_integer(box_file, 'spare_int');
      // 3 44  <<<  align_info
      old_keep_dims1.box_dims1.rail_type := parse_integer(box_file, 'rail_type');
      old_keep_dims1.box_dims1.fb_kludge_template_code := parse_integer(box_file, 'fb_kludge_template_code');
      old_keep_dims1.box_dims1.box_save_done := parse_boolean(box_file, 'box_save_done');
      old_keep_dims1.box_dims1.uninclined_rails := parse_boolean(box_file, 'uninclined_rails');
      old_keep_dims1.box_dims1.disable_f7_snap := parse_boolean(box_file, 'disable_f7_snap');
      old_keep_dims1.box_dims1.spare_bool4 := parse_boolean(box_file, 'spare_bool4');
      old_keep_dims1.box_dims1.mod_text_x := parse_extended(box_file, 'mod_text_x');
      old_keep_dims1.box_dims1.mod_text_y := parse_extended(box_file, 'mod_text_y');
      old_keep_dims1.box_dims1.flatbottom_width := parse_extended(box_file, 'flatbottom_width');
      // 3 54  >>>  check_diffs
        // 4 1  >>>  end_diff_mw
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.type_diff := parse_byte(box_file, 'type_diff');
        // 4 1  <<<  end_diff_mw
        // 4 2  >>>  end_diff_me
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.type_diff := parse_byte(box_file, 'type_diff');
        // 4 2  <<<  end_diff_me
        // 4 3  >>>  end_diff_mr
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.type_diff := parse_byte(box_file, 'type_diff');
        // 4 3  <<<  end_diff_mr
        // 4 4  >>>  end_diff_tw
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.type_diff := parse_byte(box_file, 'type_diff');
        // 4 4  <<<  end_diff_tw
        // 4 5  >>>  end_diff_te
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.type_diff := parse_byte(box_file, 'type_diff');
        // 4 5  <<<  end_diff_te
        // 4 6  >>>  end_diff_tr
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.type_diff := parse_byte(box_file, 'type_diff');
        // 4 6  <<<  end_diff_tr
        // 4 7  >>>  end_diff_mk
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.type_diff := parse_byte(box_file, 'type_diff');
        // 4 7  <<<  end_diff_mk
        // 4 8  >>>  end_diff_dk
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.len_diff := parse_extended(box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.flr_diff := parse_extended(box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.gap_diff := parse_extended(box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.type_diff := parse_byte(box_file, 'type_diff');
        // 4 8  <<<  end_diff_dk
      // 3 54  <<<  check_diffs
      old_keep_dims1.box_dims1.retain_diffs_on_make_flag := parse_boolean(box_file, 'retain_diffs_on_make_flag');
      old_keep_dims1.box_dims1.retain_diffs_on_mint_flag := parse_boolean(box_file, 'retain_diffs_on_mint_flag');
      old_keep_dims1.box_dims1.retain_entry_straight_on_make_flag := parse_boolean(box_file, 'retain_entry_straight_on_make_flag');
      old_keep_dims1.box_dims1.retain_entry_straight_on_mint_flag := parse_boolean(box_file, 'retain_entry_straight_on_mint_flag');
      old_keep_dims1.box_dims1.retain_shoves_on_make_flag := parse_boolean(box_file, 'retain_shoves_on_make_flag');
      old_keep_dims1.box_dims1.retain_shoves_on_mint_flag := parse_boolean(box_file, 'retain_shoves_on_mint_flag');
      // 3 61  >>>  turnout_info1
        old_keep_dims1.box_dims1.turnout_info1.plain_track_flag := parse_boolean(box_file, 'plain_track_flag');
        old_keep_dims1.box_dims1.turnout_info1.rolled_in_sleepered_flag := parse_boolean(box_file, 'rolled_in_sleepered_flag');
        old_keep_dims1.box_dims1.turnout_info1.front_timbers_flag := parse_boolean(box_file, 'front_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.approach_rails_only_flag := parse_boolean(box_file, 'approach_rails_only_flag');
        old_keep_dims1.box_dims1.turnout_info1.hand := parse_integer(box_file, 'hand');
        old_keep_dims1.box_dims1.turnout_info1.timbering_flag := parse_boolean(box_file, 'timbering_flag');
        old_keep_dims1.box_dims1.turnout_info1.switch_timbers_flag := parse_boolean(box_file, 'switch_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.closure_timbers_flag := parse_boolean(box_file, 'closure_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.xing_timbers_flag := parse_boolean(box_file, 'xing_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.exit_timbering := parse_integer(box_file, 'exit_timbering');
        old_keep_dims1.box_dims1.turnout_info1.turnout_road_code := parse_integer(box_file, 'turnout_road_code');
        old_keep_dims1.box_dims1.turnout_info1.turnout_length := parse_extended(box_file, 'turnout_length');
        old_keep_dims1.box_dims1.turnout_info1.origin_to_toe := parse_extended(box_file, 'origin_to_toe');
        old_keep_dims1.box_dims1.turnout_info1.step_size := parse_extended(box_file, 'step_size');
        old_keep_dims1.box_dims1.turnout_info1.turnout_road_is_adjustable := parse_boolean(box_file, 'turnout_road_is_adjustable');
        old_keep_dims1.box_dims1.turnout_info1.turnout_road_is_minimum := parse_boolean(box_file, 'turnout_road_is_minimum');
      // 3 61  <<<  turnout_info1
    // 2 1  <<<  box_dims1
  // 1 1  <<<  old_keep_dims1
  // 1 2  >>>  old_keep_shoves
    for ix := 0 to shovedim_c_048 do begin
       parse_integer(box_file, 'sv_code['+inttostr(ix)+']');;     // 0=empty slot, -1=omit this timber,  1=shove this timber.
       parse_string(box_file, 'sv_str['+inttostr(ix)+']',8);   // timber number string.
       parse_byte(box_file, 'sv_alignment_byte['+inttostr(ix)+']');   // D5 0.81 12-06-05
       parse_extended(box_file, 'sv_x['+inttostr(ix)+']');;    // xtb modifier.
       parse_extended(box_file, 'sv_k['+inttostr(ix)+']');;    // angle modifier.
       parse_extended(box_file, 'sv_o['+inttostr(ix)+']');;    // offset modifier (near end).
       parse_extended(box_file, 'sv_l['+inttostr(ix)+']');;    // length modifier (far end).
       parse_extended(box_file, 'sv_w['+inttostr(ix)+']');;    // width modifier (per side).
       parse_integer(box_file, 'sv_t['+inttostr(ix)+']');;     // nyi - thickness modifier in 1000ths of mm. (was spare integer).
    end;
  // 1 2  <<<  old_keep_shoves
  // 1 3  >>>  old_keep_dims2
    // 2 1  >>>  turnout_info2
      // 3 1  >>>  switch_info
        old_keep_dims2.turnout_info2.switch_info.old_size := parse_integer(box_file, 'old_size');
        old_keep_dims2.turnout_info2.switch_info.sw_name_str := parse_string(box_file, 'sw_name_str', 100);
        old_keep_dims2.turnout_info2.switch_info.alignment_byte_1 := parse_byte(box_file, 'alignment_byte_1');
        old_keep_dims2.turnout_info2.switch_info.alignment_byte_2 := parse_byte(box_file, 'alignment_byte_2');
        old_keep_dims2.turnout_info2.switch_info.alignment_byte_3 := parse_byte(box_file, 'alignment_byte_3');
        old_keep_dims2.turnout_info2.switch_info.sw_pattern := parse_integer(box_file, 'sw_pattern');
        old_keep_dims2.turnout_info2.switch_info.planing := parse_extended(box_file, 'planing');
        old_keep_dims2.turnout_info2.switch_info.planing_angle := parse_extended(box_file, 'planing_angle');
        old_keep_dims2.turnout_info2.switch_info.switch_radius_inchormax := parse_extended(box_file, 'switch_radius_inchormax');
        old_keep_dims2.turnout_info2.switch_info.switch_rail := parse_extended(box_file, 'switch_rail');
        old_keep_dims2.turnout_info2.switch_info.stock_rail := parse_extended(box_file, 'stock_rail');
        old_keep_dims2.turnout_info2.switch_info.heel_lead_inches := parse_extended(box_file, 'heel_lead_inches');
        old_keep_dims2.turnout_info2.switch_info.heel_offset_inches := parse_extended(box_file, 'heel_offset_inches');
        old_keep_dims2.turnout_info2.switch_info.switch_front_inches := parse_extended(box_file, 'switch_front_inches');
        old_keep_dims2.turnout_info2.switch_info.planing_radius := parse_extended(box_file, 'planing_radius');
        old_keep_dims2.turnout_info2.switch_info.sleeper_j1 := parse_extended(box_file, 'sleeper_j1');
        old_keep_dims2.turnout_info2.switch_info.sleeper_j2 := parse_extended(box_file, 'sleeper_j2');
        for ix := 0 to swtimbco_c-1 do begin
          old_keep_dims2.turnout_info2.switch_info.timber_centres[ix] := parse_extended(box_file, 'timber_centres['+inttostr(ix)+']');
           if old_keep_dims2.turnout_info2.switch_info. timber_centres[ix] = 0 then break;
         end;
        for ix := ix to swtimbco_c-1 do begin
          old_keep_dims2.turnout_info2.switch_info.timber_centres[ix] := 0;   // timber_centres
         parse_skip(box_file,  10 );
        end;
        old_keep_dims2.turnout_info2.switch_info.group_code := parse_integer(box_file, 'group_code');
        old_keep_dims2.turnout_info2.switch_info.size_code := parse_integer(box_file, 'size_code');
        old_keep_dims2.turnout_info2.switch_info.joggle_depth := parse_extended(box_file, 'joggle_depth');
        old_keep_dims2.turnout_info2.switch_info.joggle_length := parse_extended(box_file, 'joggle_length');
        old_keep_dims2.turnout_info2.switch_info.group_count := parse_integer(box_file, 'group_count');
        old_keep_dims2.turnout_info2.switch_info.joggled_stock_rail := parse_boolean(box_file, 'joggled_stock_rail');
        old_keep_dims2.turnout_info2.switch_info.alignment_byte_4 := parse_byte(box_file, 'alignment_byte_4');
        old_keep_dims2.turnout_info2.switch_info.alignment_byte_5 := parse_byte(box_file, 'alignment_byte_5');
        old_keep_dims2.turnout_info2.switch_info.alignment_byte_6 := parse_byte(box_file, 'alignment_byte_6');
        old_keep_dims2.turnout_info2.switch_info.spare_int2 := parse_integer(box_file, 'spare_int2');
        old_keep_dims2.turnout_info2.switch_info.spare_int1 := parse_integer(box_file, 'spare_int1');
        old_keep_dims2.turnout_info2.switch_info.valid_data := parse_boolean(box_file, 'valid_data');
        old_keep_dims2.turnout_info2.switch_info.front_timbered := parse_boolean(box_file, 'front_timbered');
        old_keep_dims2.turnout_info2.switch_info.num_bridge_chairs_main_rail := parse_byte(box_file, 'num_bridge_chairs_main_rail');
        old_keep_dims2.turnout_info2.switch_info.num_bridge_chairs_turnout_rail := parse_byte(box_file, 'num_bridge_chairs_turnout_rail');
        old_keep_dims2.turnout_info2.switch_info.fb_tip_offset := parse_extended(box_file, 'fb_tip_offset');
        old_keep_dims2.turnout_info2.switch_info.sleeper_j3 := parse_extended(box_file, 'sleeper_j3');
        old_keep_dims2.turnout_info2.switch_info.sleeper_j4 := parse_extended(box_file, 'sleeper_j4');
        old_keep_dims2.turnout_info2.switch_info.sleeper_j5 := parse_extended(box_file, 'sleeper_j5');
        old_keep_dims2.turnout_info2.switch_info.spare_float4 := 0;   // spare_float4
         parse_skip(box_file,  10 );
        old_keep_dims2.turnout_info2.switch_info.spare_float3 := 0;   // spare_float3
         parse_skip(box_file,  10 );
        old_keep_dims2.turnout_info2.switch_info.spare_float2 := 0;   // spare_float2
         parse_skip(box_file,  10 );
        old_keep_dims2.turnout_info2.switch_info.spare_float1 := 0;   // spare_float1
         parse_skip(box_file,  10 );
        old_keep_dims2.turnout_info2.switch_info.spare_str := parse_string(box_file, 'spare_str', 200);
        old_keep_dims2.turnout_info2.switch_info.num_slide_chairs := parse_byte(box_file, 'num_slide_chairs');
        old_keep_dims2.turnout_info2.switch_info.num_block_slide_chairs := parse_byte(box_file, 'num_block_slide_chairs');
        old_keep_dims2.turnout_info2.switch_info.num_block_heel_chairs := parse_byte(box_file, 'num_block_heel_chairs');
      // 3 1  <<<  switch_info
      // 3 2  >>>  crossing_info
        old_keep_dims2.turnout_info2.crossing_info.pattern := parse_integer(box_file, 'pattern');
        old_keep_dims2.turnout_info2.crossing_info.sl_mode := parse_integer(box_file, 'sl_mode');
        old_keep_dims2.turnout_info2.crossing_info.retcent_mode := parse_integer(box_file, 'retcent_mode');
        old_keep_dims2.turnout_info2.crossing_info.k3n_unit_angle := parse_extended(box_file, 'k3n_unit_angle');
        old_keep_dims2.turnout_info2.crossing_info.fixed_st := parse_extended(box_file, 'fixed_st');
        old_keep_dims2.turnout_info2.crossing_info.spare_int3 := parse_integer(box_file, 'spare_int3');
        old_keep_dims2.turnout_info2.crossing_info.hd_timbers_code := parse_integer(box_file, 'hd_timbers_code');
        old_keep_dims2.turnout_info2.crossing_info.hd_vchecks_code := parse_integer(box_file, 'hd_vchecks_code');
        old_keep_dims2.turnout_info2.crossing_info.k_check_length_1 := parse_extended(box_file, 'k_check_length_1');
        old_keep_dims2.turnout_info2.crossing_info.k_check_length_2 := parse_extended(box_file, 'k_check_length_2');
        old_keep_dims2.turnout_info2.crossing_info.k_check_mod_ms := parse_extended(box_file, 'k_check_mod_ms');
        old_keep_dims2.turnout_info2.crossing_info.k_check_mod_ds := parse_extended(box_file, 'k_check_mod_ds');
        old_keep_dims2.turnout_info2.crossing_info.k_check_flare := parse_extended(box_file, 'k_check_flare');
        old_keep_dims2.turnout_info2.crossing_info.curviform_timbering_keep := parse_boolean(box_file, 'curviform_timbering_keep');
        old_keep_dims2.turnout_info2.crossing_info.alignment_byte_2 := parse_byte(box_file, 'alignment_byte_2');
        old_keep_dims2.turnout_info2.crossing_info.main_road_code := parse_integer(box_file, 'main_road_code');
        old_keep_dims2.turnout_info2.crossing_info.tandem_timber_code := parse_integer(box_file, 'tandem_timber_code');
        old_keep_dims2.turnout_info2.crossing_info.blunt_nose_width := parse_extended(box_file, 'blunt_nose_width');
        old_keep_dims2.turnout_info2.crossing_info.blunt_nose_to_timb := parse_extended(box_file, 'blunt_nose_to_timb');
        old_keep_dims2.turnout_info2.crossing_info.vee_joint_half_spacing := parse_extended(box_file, 'vee_joint_half_spacing');
        old_keep_dims2.turnout_info2.crossing_info.wing_joint_spacing := parse_extended(box_file, 'wing_joint_spacing');
        old_keep_dims2.turnout_info2.crossing_info.wing_timber_spacing := parse_extended(box_file, 'wing_timber_spacing');
        old_keep_dims2.turnout_info2.crossing_info.vee_timber_spacing := parse_extended(box_file, 'vee_timber_spacing');
        old_keep_dims2.turnout_info2.crossing_info.vee_joint_space_co1 := parse_byte(box_file, 'vee_joint_space_co1');
        old_keep_dims2.turnout_info2.crossing_info.vee_joint_space_co2 := parse_byte(box_file, 'vee_joint_space_co2');
        old_keep_dims2.turnout_info2.crossing_info.vee_joint_space_co3 := parse_byte(box_file, 'vee_joint_space_co3');
        old_keep_dims2.turnout_info2.crossing_info.vee_joint_space_co4 := parse_byte(box_file, 'vee_joint_space_co4');
        old_keep_dims2.turnout_info2.crossing_info.vee_joint_space_co5 := parse_byte(box_file, 'vee_joint_space_co5');
        old_keep_dims2.turnout_info2.crossing_info.vee_joint_space_co6 := parse_byte(box_file, 'vee_joint_space_co6');
        old_keep_dims2.turnout_info2.crossing_info.wing_joint_space_co1 := parse_byte(box_file, 'wing_joint_space_co1');
        old_keep_dims2.turnout_info2.crossing_info.wing_joint_space_co2 := parse_byte(box_file, 'wing_joint_space_co2');
        old_keep_dims2.turnout_info2.crossing_info.wing_joint_space_co3 := parse_byte(box_file, 'wing_joint_space_co3');
        old_keep_dims2.turnout_info2.crossing_info.wing_joint_space_co4 := parse_byte(box_file, 'wing_joint_space_co4');
        old_keep_dims2.turnout_info2.crossing_info.wing_joint_space_co5 := parse_byte(box_file, 'wing_joint_space_co5');
        old_keep_dims2.turnout_info2.crossing_info.wing_joint_space_co6 := parse_byte(box_file, 'wing_joint_space_co6');
        old_keep_dims2.turnout_info2.crossing_info.spare_flag1 := parse_boolean(box_file, 'spare_flag1');
        old_keep_dims2.turnout_info2.crossing_info.spare_flag2 := parse_boolean(box_file, 'spare_flag2');
        old_keep_dims2.turnout_info2.crossing_info.main_road_endx_infile := parse_extended(box_file, 'main_road_endx_infile');
        old_keep_dims2.turnout_info2.crossing_info.hdkn_unit_angle := parse_extended(box_file, 'hdkn_unit_angle');
        // 4 40  >>>  check_flare_info_081
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_flare_ext_ms := parse_extended(box_file, 'check_flare_ext_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_flare_ext_ts := parse_extended(box_file, 'check_flare_ext_ts');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_flare_work_ms := parse_extended(box_file, 'check_flare_work_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_flare_work_ts := parse_extended(box_file, 'check_flare_work_ts');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.wing_flare_ms := parse_extended(box_file, 'wing_flare_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.wing_flare_ts := parse_extended(box_file, 'wing_flare_ts');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_flare_k_ms := parse_extended(box_file, 'check_flare_k_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_flare_k_ds := parse_extended(box_file, 'check_flare_k_ds');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_fwe_ext_ms := parse_extended(box_file, 'check_fwe_ext_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_fwe_ext_ts := parse_extended(box_file, 'check_fwe_ext_ts');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_fwe_work_ms := parse_extended(box_file, 'check_fwe_work_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_fwe_work_ts := parse_extended(box_file, 'check_fwe_work_ts');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.wing_fwe_ms := parse_extended(box_file, 'wing_fwe_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.wing_fwe_ts := parse_extended(box_file, 'wing_fwe_ts');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_fwe_k_ms := parse_extended(box_file, 'check_fwe_k_ms');
          old_keep_dims2.turnout_info2.crossing_info.check_flare_info_081.check_fwe_k_ds := parse_extended(box_file, 'check_fwe_k_ds');
        // 4 40  <<<  check_flare_info_081
        old_keep_dims2.turnout_info2.crossing_info.k_custom_wing_long_keep := parse_extended(box_file, 'k_custom_wing_long_keep');
        old_keep_dims2.turnout_info2.crossing_info.k_custom_point_long_keep := parse_extended(box_file, 'k_custom_point_long_keep');
        old_keep_dims2.turnout_info2.crossing_info.use_k_custom_wing_rails_keep := parse_boolean(box_file, 'use_k_custom_wing_rails_keep');
        old_keep_dims2.turnout_info2.crossing_info.use_k_custom_point_rails_keep := parse_boolean(box_file, 'use_k_custom_point_rails_keep');
        old_keep_dims2.turnout_info2.crossing_info.spare_str := parse_string(box_file, 'spare_str', 10);
        old_keep_dims2.turnout_info2.crossing_info.alignment_byte_3 := parse_byte(box_file, 'alignment_byte_3');
      // 3 2  <<<  crossing_info
      // 3 3  >>>  plain_track_info
        old_keep_dims2.turnout_info2.plain_track_info.pt_custom := parse_boolean(box_file, 'pt_custom');
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_1 := parse_byte(box_file, 'alignment_byte_1');
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_2 := parse_byte(box_file, 'alignment_byte_2');
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_3 := parse_byte(box_file, 'alignment_byte_3');
        old_keep_dims2.turnout_info2.plain_track_info.list_index := parse_integer(box_file, 'list_index');
        old_keep_dims2.turnout_info2.plain_track_info.rail_length := parse_extended(box_file, 'rail_length');
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_4 := parse_byte(box_file, 'alignment_byte_4');
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_5 := parse_byte(box_file, 'alignment_byte_5');
        old_keep_dims2.turnout_info2.plain_track_info.sleepers_per_length := parse_integer(box_file, 'sleepers_per_length');
        for ix := 0 to psleep_c-1 do begin
          old_keep_dims2.turnout_info2.plain_track_info.sleeper_centres[ix] := parse_extended(box_file, 'sleeper_centres['+inttostr(ix)+']');
           if old_keep_dims2.turnout_info2.plain_track_info. sleeper_centres[ix] = 0 then break;
         end;
        for ix := ix to psleep_c-1 do begin
          old_keep_dims2.turnout_info2.plain_track_info.sleeper_centres[ix] := 0;   // sleeper_centres
         parse_skip(box_file,  10 );
        end;
        old_keep_dims2.turnout_info2.plain_track_info.rail_joints_code := parse_integer(box_file, 'rail_joints_code');
        old_keep_dims2.turnout_info2.plain_track_info.user_peg_rail := parse_integer(box_file, 'user_peg_rail');
        old_keep_dims2.turnout_info2.plain_track_info.pt_spare_flag1 := parse_boolean(box_file, 'pt_spare_flag1');
        old_keep_dims2.turnout_info2.plain_track_info.pt_spare_flag2 := parse_boolean(box_file, 'pt_spare_flag2');
        old_keep_dims2.turnout_info2.plain_track_info.pt_spare_flag3 := parse_boolean(box_file, 'pt_spare_flag3');
        old_keep_dims2.turnout_info2.plain_track_info.user_peg_data_valid := parse_boolean(box_file, 'user_peg_data_valid');
        old_keep_dims2.turnout_info2.plain_track_info.user_pegx := parse_extended(box_file, 'user_pegx');
        old_keep_dims2.turnout_info2.plain_track_info.user_pegy := parse_extended(box_file, 'user_pegy');
        old_keep_dims2.turnout_info2.plain_track_info.user_pegk := parse_extended(box_file, 'user_pegk');
        old_keep_dims2.turnout_info2.plain_track_info.pt_spacing_name_str := parse_string(box_file, 'pt_spacing_name_str', 200);
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_6 := parse_byte(box_file, 'alignment_byte_6');
        old_keep_dims2.turnout_info2.plain_track_info.pt_tb_rolling_percent := parse_extended(box_file, 'pt_tb_rolling_percent');
        old_keep_dims2.turnout_info2.plain_track_info.gaunt_sleeper_mod_inches := parse_extended(box_file, 'gaunt_sleeper_mod_inches');
        old_keep_dims2.turnout_info2.plain_track_info.pt_spare_ext3 := parse_extended(box_file, 'pt_spare_ext3');
        old_keep_dims2.turnout_info2.plain_track_info.pt_spare_ext2 := parse_extended(box_file, 'pt_spare_ext2');
        old_keep_dims2.turnout_info2.plain_track_info.pt_spare_ext1 := parse_extended(box_file, 'pt_spare_ext1');
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_7 := parse_byte(box_file, 'alignment_byte_7');
        old_keep_dims2.turnout_info2.plain_track_info.alignment_byte_8 := parse_byte(box_file, 'alignment_byte_8');
      // 3 3  <<<  plain_track_info
      old_keep_dims2.turnout_info2.diamond_auto_code := parse_integer(box_file, 'diamond_auto_code');
      old_keep_dims2.turnout_info2.bonus_timber_count := parse_integer(box_file, 'bonus_timber_count');
      old_keep_dims2.turnout_info2.equalizing_fixed_flag := parse_boolean(box_file, 'equalizing_fixed_flag');
      old_keep_dims2.turnout_info2.no_timbering_flag := parse_boolean(box_file, 'no_timbering_flag');
      old_keep_dims2.turnout_info2.angled_on_flag := parse_boolean(box_file, 'angled_on_flag');
      old_keep_dims2.turnout_info2.chairing_flag := parse_boolean(box_file, 'chairing_flag');
      old_keep_dims2.turnout_info2.start_draw_x := parse_extended(box_file, 'start_draw_x');
      old_keep_dims2.turnout_info2.timber_length_inc := parse_extended(box_file, 'timber_length_inc');
      old_keep_dims2.turnout_info2.omit_switch_front_joints := parse_boolean(box_file, 'omit_switch_front_joints');
      old_keep_dims2.turnout_info2.omit_switch_rail_joints := parse_boolean(box_file, 'omit_switch_rail_joints');
      old_keep_dims2.turnout_info2.omit_stock_rail_joints := parse_boolean(box_file, 'omit_stock_rail_joints');
      old_keep_dims2.turnout_info2.omit_wing_rail_joints := parse_boolean(box_file, 'omit_wing_rail_joints');
      old_keep_dims2.turnout_info2.omit_vee_rail_joints := parse_boolean(box_file, 'omit_vee_rail_joints');
      old_keep_dims2.turnout_info2.omit_k_crossing_stock_rail_joints := parse_boolean(box_file, 'omit_k_crossing_stock_rail_joints');
      old_keep_dims2.turnout_info2.spare_flag14 := parse_boolean(box_file, 'spare_flag14');
      old_keep_dims2.turnout_info2.spare_flag13 := parse_boolean(box_file, 'spare_flag13');
      old_keep_dims2.turnout_info2.spare_flag12 := parse_boolean(box_file, 'spare_flag12');
      old_keep_dims2.turnout_info2.diamond_switch_timbering_flag := parse_boolean(box_file, 'diamond_switch_timbering_flag');
      old_keep_dims2.turnout_info2.gaunt_flag := parse_boolean(box_file, 'gaunt_flag');
      old_keep_dims2.turnout_info2.diamond_proto_timbering_flag := parse_boolean(box_file, 'diamond_proto_timbering_flag');
      old_keep_dims2.turnout_info2.semi_diamond_flag := parse_boolean(box_file, 'semi_diamond_flag');
      old_keep_dims2.turnout_info2.diamond_fixed_flag := parse_boolean(box_file, 'diamond_fixed_flag');
      // 3 26  >>>  hdk_check_rail_info
        old_keep_dims2.turnout_info2.hdk_check_rail_info.k_check_ms_1 := parse_extended(box_file, 'k_check_ms_1');
        old_keep_dims2.turnout_info2.hdk_check_rail_info.k_check_ms_2 := parse_extended(box_file, 'k_check_ms_2');
        old_keep_dims2.turnout_info2.hdk_check_rail_info.k_check_ds_1 := parse_extended(box_file, 'k_check_ds_1');
        old_keep_dims2.turnout_info2.hdk_check_rail_info.k_check_ds_2 := parse_extended(box_file, 'k_check_ds_2');
      // 3 26  <<<  hdk_check_rail_info
      // 3 27  >>>  vee_check_rail_info
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ms_working1 := parse_extended(box_file, 'v_check_ms_working1');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ms_working2 := parse_extended(box_file, 'v_check_ms_working2');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ms_working3 := parse_extended(box_file, 'v_check_ms_working3');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ts_working1 := parse_extended(box_file, 'v_check_ts_working1');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ts_working2 := parse_extended(box_file, 'v_check_ts_working2');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ts_working3 := parse_extended(box_file, 'v_check_ts_working3');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ms_ext1 := parse_extended(box_file, 'v_check_ms_ext1');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ms_ext2 := parse_extended(box_file, 'v_check_ms_ext2');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ts_ext1 := parse_extended(box_file, 'v_check_ts_ext1');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_check_ts_ext2 := parse_extended(box_file, 'v_check_ts_ext2');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_wing_ms_reach1 := parse_extended(box_file, 'v_wing_ms_reach1');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_wing_ms_reach2 := parse_extended(box_file, 'v_wing_ms_reach2');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_wing_ts_reach1 := parse_extended(box_file, 'v_wing_ts_reach1');
        old_keep_dims2.turnout_info2.vee_check_rail_info.v_wing_ts_reach2 := parse_extended(box_file, 'v_wing_ts_reach2');
      // 3 27  <<<  vee_check_rail_info
      old_keep_dims2.turnout_info2.turnout_road_endx_infile := parse_extended(box_file, 'turnout_road_endx_infile');
      old_keep_dims2.turnout_info2.template_type_str := parse_string(box_file, 'template_type_str', 6);
      old_keep_dims2.turnout_info2.smallest_radius_stored := parse_extended(box_file, 'smallest_radius_stored');
      old_keep_dims2.turnout_info2.dpx_stored := parse_extended(box_file, 'dpx_stored');
      old_keep_dims2.turnout_info2.ipx_stored := parse_extended(box_file, 'ipx_stored');
      old_keep_dims2.turnout_info2.fpx_stored := parse_extended(box_file, 'fpx_stored');
      old_keep_dims2.turnout_info2.gaunt_offset_inches := parse_extended(box_file, 'gaunt_offset_inches');
      old_keep_dims2.turnout_info2.dxf_connector_0 := parse_boolean(box_file, 'dxf_connector_0');
      old_keep_dims2.turnout_info2.dxf_connector_t := parse_boolean(box_file, 'dxf_connector_t');
      old_keep_dims2.turnout_info2.dxf_connector_9 := parse_boolean(box_file, 'dxf_connector_9');
    // 2 1  <<<  turnout_info2
  // 1 3  <<<  old_keep_dims2
// 0 0  <<<  
