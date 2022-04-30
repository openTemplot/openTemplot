{
								=====  NOTE  =====
								
	This file is created by manually editing 'load_t2keep_data.inc' to retain only the first part, 
	i.e. the lines relating to old_keep_dims1.
	If 'load_t2keep_data.inc' is changed, this file needs to have corresponding changes made.
}

// 0 0  >>>  
  // 1 1  >>>  old_keep_dims1
    // 2 1  >>>  box_dims1
logging := True;
      old_keep_dims1.box_dims1.box_ident := parse_string(test_box_file, 'box_ident', 10);
      old_keep_dims1.box_dims1.id_byte := parse_byte(test_box_file, 'id_byte');
      old_keep_dims1.box_dims1.now_time := parse_integer(test_box_file, 'now_time');
      old_keep_dims1.box_dims1.keep_date := parse_string(test_box_file, 'keep_date', 20);
      old_keep_dims1.box_dims1.keep_time := parse_string(test_box_file, 'keep_time', 20);
      old_keep_dims1.box_dims1.top_label := parse_string(test_box_file, 'top_label', 100);
      old_keep_dims1.box_dims1.project_for := parse_string(test_box_file, 'project_for', 50);
      old_keep_dims1.box_dims1.reference_string := parse_string(test_box_file, 'reference_string', 100);
      old_keep_dims1.box_dims1.this_was_control_template := parse_boolean(test_box_file, 'this_was_control_template');
      // 3 10  >>>  rail_info
        old_keep_dims1.box_dims1.rail_info.flared_ends_ri := parse_integer(test_box_file, 'flared_ends_ri');
        old_keep_dims1.box_dims1.rail_info.spare_int1 := parse_integer(test_box_file, 'spare_int1');
        old_keep_dims1.box_dims1.rail_info.knuckle_code_ri := parse_integer(test_box_file, 'knuckle_code_ri');
        old_keep_dims1.box_dims1.rail_info.knuckle_radius_ri := parse_extended(test_box_file, 'knuckle_radius_ri');
        old_keep_dims1.box_dims1.rail_info.spare_float2 := parse_extended(test_box_file, 'spare_float2');
        old_keep_dims1.box_dims1.rail_info.spare_bool1 := parse_boolean(test_box_file, 'spare_bool1');
        old_keep_dims1.box_dims1.rail_info.spare_bool2 := parse_boolean(test_box_file, 'spare_bool2');
        old_keep_dims1.box_dims1.rail_info.isolated_crossing_sw := parse_boolean(test_box_file, 'isolated_crossing_sw');
        old_keep_dims1.box_dims1.rail_info.k_diagonal_side_check_rail_sw := parse_boolean(test_box_file, 'k_diagonal_side_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.k_main_side_check_rail_sw := parse_boolean(test_box_file, 'k_main_side_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.switch_drive_sw := parse_boolean(test_box_file, 'switch_drive_sw');
        old_keep_dims1.box_dims1.rail_info.track_centre_lines_sw := parse_boolean(test_box_file, 'track_centre_lines_sw');
        old_keep_dims1.box_dims1.rail_info.turnout_road_stock_rail_sw := parse_boolean(test_box_file, 'turnout_road_stock_rail_sw');
        old_keep_dims1.box_dims1.rail_info.turnout_road_check_rail_sw := parse_boolean(test_box_file, 'turnout_road_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.turnout_road_crossing_rail_sw := parse_boolean(test_box_file, 'turnout_road_crossing_rail_sw');
        old_keep_dims1.box_dims1.rail_info.crossing_vee_sw := parse_boolean(test_box_file, 'crossing_vee_sw');
        old_keep_dims1.box_dims1.rail_info.main_road_crossing_rail_sw := parse_boolean(test_box_file, 'main_road_crossing_rail_sw');
        old_keep_dims1.box_dims1.rail_info.main_road_check_rail_sw := parse_boolean(test_box_file, 'main_road_check_rail_sw');
        old_keep_dims1.box_dims1.rail_info.main_road_stock_rail_sw := parse_boolean(test_box_file, 'main_road_stock_rail_sw');
        old_keep_dims1.box_dims1.rail_info.alignment_byte_1 := parse_byte(test_box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.rail_info.alignment_byte_2 := parse_byte(test_box_file, 'alignment_byte_2');
      // 3 10  <<<  rail_info
      old_keep_dims1.box_dims1.auto_restore_on_startup := parse_boolean(test_box_file, 'auto_restore_on_startup');
      old_keep_dims1.box_dims1.ask_restore_on_startup := parse_boolean(test_box_file, 'ask_restore_on_startup');
      old_keep_dims1.box_dims1.pre077_bgnd_flag := parse_boolean(test_box_file, 'pre077_bgnd_flag');
      old_keep_dims1.box_dims1.alignment_byte_3 := parse_byte(test_box_file, 'alignment_byte_3');
      old_keep_dims1.box_dims1.templot_version := parse_integer(test_box_file, 'templot_version');
      old_keep_dims1.box_dims1.file_format_code := parse_integer(test_box_file, 'file_format_code');
      old_keep_dims1.box_dims1.gauge_index := parse_integer(test_box_file, 'gauge_index');
      old_keep_dims1.box_dims1.gauge_exact := parse_boolean(test_box_file, 'gauge_exact');
      old_keep_dims1.box_dims1.gauge_custom := parse_boolean(test_box_file, 'gauge_custom');
      // 3 20  >>>  proto_info
        old_keep_dims1.box_dims1.proto_info.name_str_pi := parse_string(test_box_file, 'name_str_pi', 15);
        old_keep_dims1.box_dims1.proto_info.spare_str_pi := parse_string(test_box_file, 'spare_str_pi', 75);
        old_keep_dims1.box_dims1.proto_info.scale_pi := parse_extended(test_box_file, 'scale_pi');
        old_keep_dims1.box_dims1.proto_info.gauge_pi := parse_extended(test_box_file, 'gauge_pi');
        old_keep_dims1.box_dims1.proto_info.fw_pi := parse_extended(test_box_file, 'fw_pi');
        old_keep_dims1.box_dims1.proto_info.fwe_pi := parse_extended(test_box_file, 'fwe_pi');
        old_keep_dims1.box_dims1.proto_info.xing_fl_pi := parse_extended(test_box_file, 'xing_fl_pi');
        old_keep_dims1.box_dims1.proto_info.railtop_pi := parse_extended(test_box_file, 'railtop_pi');
        old_keep_dims1.box_dims1.proto_info.trtscent_pi := parse_extended(test_box_file, 'trtscent_pi');
        old_keep_dims1.box_dims1.proto_info.trmscent_pi := parse_extended(test_box_file, 'trmscent_pi');
        old_keep_dims1.box_dims1.proto_info.retcent_pi := parse_extended(test_box_file, 'retcent_pi');
        old_keep_dims1.box_dims1.proto_info.min_radius_pi := parse_extended(test_box_file, 'min_radius_pi');
        old_keep_dims1.box_dims1.proto_info.old_winglongs_pi := parse_extended(test_box_file, 'old_winglongs_pi');
        old_keep_dims1.box_dims1.proto_info.old_winglongl_pi := parse_extended(test_box_file, 'old_winglongl_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongs_pi := parse_extended(test_box_file, 'old_cklongs_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongm_pi := parse_extended(test_box_file, 'old_cklongm_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongl_pi := parse_extended(test_box_file, 'old_cklongl_pi');
        old_keep_dims1.box_dims1.proto_info.old_cklongxl_pi := parse_extended(test_box_file, 'old_cklongxl_pi');
        old_keep_dims1.box_dims1.proto_info.tbwide_pi := parse_extended(test_box_file, 'tbwide_pi');
        old_keep_dims1.box_dims1.proto_info.slwide_pi := parse_extended(test_box_file, 'slwide_pi');
        old_keep_dims1.box_dims1.proto_info.xtimbsp_pi := parse_extended(test_box_file, 'xtimbsp_pi');
        old_keep_dims1.box_dims1.proto_info.ftimbspmax_pi := parse_extended(test_box_file, 'ftimbspmax_pi');
        old_keep_dims1.box_dims1.proto_info.tb_pi := parse_extended(test_box_file, 'tb_pi');
        old_keep_dims1.box_dims1.proto_info.mainside_ends_pi := parse_boolean(test_box_file, 'mainside_ends_pi');
        old_keep_dims1.box_dims1.proto_info.jt_slwide_pi := parse_float(test_box_file, 'jt_slwide_pi');
        old_keep_dims1.box_dims1.proto_info.alignment_byte_1 := parse_byte(test_box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.proto_info.random_end_pi := parse_extended(test_box_file, 'random_end_pi');
        old_keep_dims1.box_dims1.proto_info.timber_thick_pi := parse_extended(test_box_file, 'timber_thick_pi');
        old_keep_dims1.box_dims1.proto_info.random_angle_pi := parse_extended(test_box_file, 'random_angle_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_working1_pi := parse_extended(test_box_file, 'ck_ms_working1_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_working2_pi := parse_extended(test_box_file, 'ck_ms_working2_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_working3_pi := parse_extended(test_box_file, 'ck_ms_working3_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ts_working_mod_pi := parse_extended(test_box_file, 'ck_ts_working_mod_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_ext1_pi := parse_extended(test_box_file, 'ck_ms_ext1_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ms_ext2_pi := parse_extended(test_box_file, 'ck_ms_ext2_pi');
        old_keep_dims1.box_dims1.proto_info.ck_ts_ext_mod_pi := parse_extended(test_box_file, 'ck_ts_ext_mod_pi');
        old_keep_dims1.box_dims1.proto_info.wing_ms_reach1_pi := parse_extended(test_box_file, 'wing_ms_reach1_pi');
        old_keep_dims1.box_dims1.proto_info.wing_ms_reach2_pi := parse_extended(test_box_file, 'wing_ms_reach2_pi');
        old_keep_dims1.box_dims1.proto_info.wing_ts_reach_mod_pi := parse_extended(test_box_file, 'wing_ts_reach_mod_pi');
        old_keep_dims1.box_dims1.proto_info.railbottom_pi := parse_extended(test_box_file, 'railbottom_pi');
        old_keep_dims1.box_dims1.proto_info.rail_height_pi := parse_extended(test_box_file, 'rail_height_pi');
        old_keep_dims1.box_dims1.proto_info.seat_thick_pi := parse_extended(test_box_file, 'seat_thick_pi');
        old_keep_dims1.box_dims1.proto_info.old_tb_pi := parse_extended(test_box_file, 'old_tb_pi');
        old_keep_dims1.box_dims1.proto_info.rail_inclination_pi := parse_extended(test_box_file, 'rail_inclination_pi');
        old_keep_dims1.box_dims1.proto_info.foot_height_pi := parse_extended(test_box_file, 'foot_height_pi');
        old_keep_dims1.box_dims1.proto_info.chair_outlen_pi := parse_extended(test_box_file, 'chair_outlen_pi');
        old_keep_dims1.box_dims1.proto_info.chair_inlen_pi := parse_extended(test_box_file, 'chair_inlen_pi');
        old_keep_dims1.box_dims1.proto_info.chair_width_pi := parse_extended(test_box_file, 'chair_width_pi');
        old_keep_dims1.box_dims1.proto_info.chair_corner_pi := parse_extended(test_box_file, 'chair_corner_pi');
        old_keep_dims1.box_dims1.proto_info.spare_byte1 := parse_byte(test_box_file, 'spare_byte1');
        old_keep_dims1.box_dims1.proto_info.spare_byte2 := parse_byte(test_box_file, 'spare_byte2');
        old_keep_dims1.box_dims1.proto_info.spare_byte3 := parse_byte(test_box_file, 'spare_byte3');
        old_keep_dims1.box_dims1.proto_info.spare_byte4 := parse_byte(test_box_file, 'spare_byte4');
        old_keep_dims1.box_dims1.proto_info.spare_byte5 := parse_byte(test_box_file, 'spare_byte5');
        old_keep_dims1.box_dims1.proto_info.alignment_byte_2 := parse_byte(test_box_file, 'alignment_byte_2');
      // 3 20  <<<  proto_info
      old_keep_dims1.box_dims1.railtop_inches := parse_extended(test_box_file, 'railtop_inches');
      old_keep_dims1.box_dims1.railbottom_inches := parse_extended(test_box_file, 'railbottom_inches');
      old_keep_dims1.box_dims1.alignment_byte_4 := parse_byte(test_box_file, 'alignment_byte_4');
      old_keep_dims1.box_dims1.alignment_byte_5 := parse_byte(test_box_file, 'alignment_byte_5');
      old_keep_dims1.box_dims1.version_as_loaded := parse_integer(test_box_file, 'version_as_loaded');
      old_keep_dims1.box_dims1.bgnd_code_077 := parse_integer(test_box_file, 'bgnd_code_077');
      old_keep_dims1.box_dims1.print_mapping_colour := parse_integer(test_box_file, 'print_mapping_colour');
      old_keep_dims1.box_dims1.pad_marker_colour := parse_integer(test_box_file, 'pad_marker_colour');
      old_keep_dims1.box_dims1.use_print_mapping_colour := parse_boolean(test_box_file, 'use_print_mapping_colour');
      old_keep_dims1.box_dims1.use_pad_marker_colour := parse_boolean(test_box_file, 'use_pad_marker_colour');
      old_keep_dims1.box_dims1.spare_bool1 := parse_boolean(test_box_file, 'spare_bool1');
      old_keep_dims1.box_dims1.spare_bool2 := parse_boolean(test_box_file, 'spare_bool2');
      old_keep_dims1.box_dims1.spare_int2 := parse_integer(test_box_file, 'spare_int2');
      old_keep_dims1.box_dims1.grid_units_code := parse_integer(test_box_file, 'grid_units_code');
      old_keep_dims1.box_dims1.x_grid_spacing := parse_extended(test_box_file, 'x_grid_spacing');
      old_keep_dims1.box_dims1.y_grid_spacing := parse_extended(test_box_file, 'y_grid_spacing');
      old_keep_dims1.box_dims1.total_length_of_timbering := parse_extended(test_box_file, 'total_length_of_timbering');
      old_keep_dims1.box_dims1.id_number := parse_integer(test_box_file, 'id_number');
      old_keep_dims1.box_dims1.id_number_str := parse_string(test_box_file, 'id_number_str', 7);
      old_keep_dims1.box_dims1.spare_boolean1 := parse_boolean(test_box_file, 'spare_boolean1');
      old_keep_dims1.box_dims1.spare_boolean2 := parse_boolean(test_box_file, 'spare_boolean2');
      // 3 42  >>>  transform_info
        old_keep_dims1.box_dims1.transform_info.datum_y := parse_extended(test_box_file, 'datum_y');
        old_keep_dims1.box_dims1.transform_info.x_go_limit := parse_extended(test_box_file, 'x_go_limit');
        old_keep_dims1.box_dims1.transform_info.x_stop_limit := parse_extended(test_box_file, 'x_stop_limit');
        old_keep_dims1.box_dims1.transform_info.transforms_apply := parse_boolean(test_box_file, 'transforms_apply');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_1 := parse_byte(test_box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.transform_info.x1_shift := parse_extended(test_box_file, 'x1_shift');
        old_keep_dims1.box_dims1.transform_info.y1_shift := parse_extended(test_box_file, 'y1_shift');
        old_keep_dims1.box_dims1.transform_info.k_shift := parse_extended(test_box_file, 'k_shift');
        old_keep_dims1.box_dims1.transform_info.x2_shift := parse_extended(test_box_file, 'x2_shift');
        old_keep_dims1.box_dims1.transform_info.y2_shift := parse_extended(test_box_file, 'y2_shift');
        // 4 11  >>>  peg_pos
          old_keep_dims1.box_dims1.transform_info.peg_pos.x := parse_extended(test_box_file, 'x');
          old_keep_dims1.box_dims1.transform_info.peg_pos.y := parse_extended(test_box_file, 'y');
        // 4 11  <<<  peg_pos
        old_keep_dims1.box_dims1.transform_info.alignment_byte_2 := parse_byte(test_box_file, 'alignment_byte_2');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_3 := parse_byte(test_box_file, 'alignment_byte_3');
        old_keep_dims1.box_dims1.transform_info.peg_point_code := parse_integer(test_box_file, 'peg_point_code');
        old_keep_dims1.box_dims1.transform_info.peg_point_rail := parse_integer(test_box_file, 'peg_point_rail');
        old_keep_dims1.box_dims1.transform_info.mirror_on_x := parse_boolean(test_box_file, 'mirror_on_x');
        old_keep_dims1.box_dims1.transform_info.mirror_on_y := parse_boolean(test_box_file, 'mirror_on_y');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_4 := parse_byte(test_box_file, 'alignment_byte_4');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_5 := parse_byte(test_box_file, 'alignment_byte_5');
        old_keep_dims1.box_dims1.transform_info.spare_int1 := parse_integer(test_box_file, 'spare_int1');
        old_keep_dims1.box_dims1.transform_info.spare_int2 := parse_integer(test_box_file, 'spare_int2');
        old_keep_dims1.box_dims1.transform_info.spare_flag1 := parse_boolean(test_box_file, 'spare_flag1');
        old_keep_dims1.box_dims1.transform_info.spare_flag2 := parse_boolean(test_box_file, 'spare_flag2');
        old_keep_dims1.box_dims1.transform_info.spare_flag3 := parse_boolean(test_box_file, 'spare_flag3');
        old_keep_dims1.box_dims1.transform_info.spare_flag4 := parse_boolean(test_box_file, 'spare_flag4');
        // 4 26  >>>  notch_info
          old_keep_dims1.box_dims1.transform_info.notch_info.notch_x := parse_extended(test_box_file, 'notch_x');
          old_keep_dims1.box_dims1.transform_info.notch_info.notch_y := parse_extended(test_box_file, 'notch_y');
          old_keep_dims1.box_dims1.transform_info.notch_info.notch_k := parse_extended(test_box_file, 'notch_k');
        // 4 26  <<<  notch_info
        old_keep_dims1.box_dims1.transform_info.spare_str := parse_string(test_box_file, 'spare_str', 10);
        old_keep_dims1.box_dims1.transform_info.alignment_byte_6 := parse_byte(test_box_file, 'alignment_byte_6');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_7 := parse_byte(test_box_file, 'alignment_byte_7');
        old_keep_dims1.box_dims1.transform_info.alignment_byte_8 := parse_byte(test_box_file, 'alignment_byte_8');
      // 3 42  <<<  transform_info
      // 3 43  >>>  platform_trackbed_info
        old_keep_dims1.box_dims1.platform_trackbed_info.adjacent_edges_keep := parse_boolean(test_box_file, 'adjacent_edges_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_trackbed_edge_keep := parse_boolean(test_box_file, 'draw_ms_trackbed_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_trackbed_edge_keep := parse_boolean(test_box_file, 'draw_ts_trackbed_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool1 := parse_boolean(test_box_file, 'spare_bool1');
        old_keep_dims1.box_dims1.platform_trackbed_info.OUT_OF_USE_trackbed_width_ins_keep := parse_extended(test_box_file, 'OUT_OF_USE_trackbed_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_keep := parse_boolean(test_box_file, 'draw_ts_platform_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_start_edge_keep := parse_boolean(test_box_file, 'draw_ts_platform_start_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_end_edge_keep := parse_boolean(test_box_file, 'draw_ts_platform_end_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_platform_rear_edge_keep := parse_boolean(test_box_file, 'draw_ts_platform_rear_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_front_edge_ins_keep := parse_extended(test_box_file, 'platform_ts_front_edge_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_start_width_ins_keep := parse_extended(test_box_file, 'platform_ts_start_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_end_width_ins_keep := parse_extended(test_box_file, 'platform_ts_end_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_start_mm_keep := parse_extended(test_box_file, 'platform_ts_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_length_mm_keep := parse_extended(test_box_file, 'platform_ts_length_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_keep := parse_boolean(test_box_file, 'draw_ms_platform_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_start_edge_keep := parse_boolean(test_box_file, 'draw_ms_platform_start_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_end_edge_keep := parse_boolean(test_box_file, 'draw_ms_platform_end_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_platform_rear_edge_keep := parse_boolean(test_box_file, 'draw_ms_platform_rear_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_front_edge_ins_keep := parse_extended(test_box_file, 'platform_ms_front_edge_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_start_width_ins_keep := parse_extended(test_box_file, 'platform_ms_start_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_end_width_ins_keep := parse_extended(test_box_file, 'platform_ms_end_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_start_mm_keep := parse_extended(test_box_file, 'platform_ms_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_length_mm_keep := parse_extended(test_box_file, 'platform_ms_length_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.OUT_OF_USE_cess_width_ins_keep := parse_extended(test_box_file, 'OUT_OF_USE_cess_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.OUT_OF_USE_draw_trackbed_cess_edge_keep := parse_boolean(test_box_file, 'OUT_OF_USE_draw_trackbed_cess_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_start_skew_mm_keep := parse_extended(test_box_file, 'platform_ms_start_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ms_end_skew_mm_keep := parse_extended(test_box_file, 'platform_ms_end_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_start_skew_mm_keep := parse_extended(test_box_file, 'platform_ts_start_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.platform_ts_end_skew_mm_keep := parse_extended(test_box_file, 'platform_ts_end_skew_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool2 := parse_boolean(test_box_file, 'spare_bool2');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool3 := parse_boolean(test_box_file, 'spare_bool3');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool4 := parse_boolean(test_box_file, 'spare_bool4');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool5 := parse_boolean(test_box_file, 'spare_bool5');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool6 := parse_boolean(test_box_file, 'spare_bool6');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool7 := parse_boolean(test_box_file, 'spare_bool7');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare_bool8 := parse_boolean(test_box_file, 'spare_bool8');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ms_width_ins_keep := parse_float(test_box_file, 'trackbed_ms_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ts_width_ins_keep := parse_float(test_box_file, 'trackbed_ts_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.cess_ms_width_ins_keep := parse_float(test_box_file, 'cess_ms_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.cess_ts_width_ins_keep := parse_float(test_box_file, 'cess_ts_width_ins_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep := parse_boolean(test_box_file, 'draw_ms_trackbed_cess_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep := parse_boolean(test_box_file, 'draw_ts_trackbed_cess_edge_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare1 := parse_boolean(test_box_file, 'spare1');
        old_keep_dims1.box_dims1.platform_trackbed_info.spare2 := parse_boolean(test_box_file, 'spare2');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ms_start_mm_keep := parse_extended(test_box_file, 'trackbed_ms_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ms_length_mm_keep := parse_extended(test_box_file, 'trackbed_ms_length_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ts_start_mm_keep := parse_extended(test_box_file, 'trackbed_ts_start_mm_keep');
        old_keep_dims1.box_dims1.platform_trackbed_info.trackbed_ts_length_mm_keep := parse_extended(test_box_file, 'trackbed_ts_length_mm_keep');
      // 3 43  <<<  platform_trackbed_info
      // 3 44  >>>  align_info
        old_keep_dims1.box_dims1.align_info.curving_flag := parse_boolean(test_box_file, 'curving_flag');
        old_keep_dims1.box_dims1.align_info.trans_flag := parse_boolean(test_box_file, 'trans_flag');
        old_keep_dims1.box_dims1.align_info.fixed_rad := parse_extended(test_box_file, 'fixed_rad');
        old_keep_dims1.box_dims1.align_info.trans_rad1 := parse_extended(test_box_file, 'trans_rad1');
        old_keep_dims1.box_dims1.align_info.trans_rad2 := parse_extended(test_box_file, 'trans_rad2');
        old_keep_dims1.box_dims1.align_info.trans_length := parse_extended(test_box_file, 'trans_length');
        old_keep_dims1.box_dims1.align_info.trans_start := parse_extended(test_box_file, 'trans_start');
        old_keep_dims1.box_dims1.align_info.rad_offset := parse_extended(test_box_file, 'rad_offset');
        old_keep_dims1.box_dims1.align_info.alignment_byte_1 := parse_byte(test_box_file, 'alignment_byte_1');
        old_keep_dims1.box_dims1.align_info.alignment_byte_2 := parse_byte(test_box_file, 'alignment_byte_2');
        old_keep_dims1.box_dims1.align_info.tanh_kmax := parse_double(test_box_file, 'tanh_kmax');
        old_keep_dims1.box_dims1.align_info.slewing_flag := parse_boolean(test_box_file, 'slewing_flag');
        old_keep_dims1.box_dims1.align_info.cl_only_flag := parse_boolean(test_box_file, 'cl_only_flag');
        old_keep_dims1.box_dims1.align_info.slew_type := parse_byte(test_box_file, 'slew_type');
        old_keep_dims1.box_dims1.align_info.dummy_template_flag := parse_boolean(test_box_file, 'dummy_template_flag');
        old_keep_dims1.box_dims1.align_info.slew_start := parse_extended(test_box_file, 'slew_start');
        old_keep_dims1.box_dims1.align_info.slew_length := parse_extended(test_box_file, 'slew_length');
        old_keep_dims1.box_dims1.align_info.slew_amount := parse_extended(test_box_file, 'slew_amount');
        old_keep_dims1.box_dims1.align_info.cl_options_code_int := parse_integer(test_box_file, 'cl_options_code_int');
        old_keep_dims1.box_dims1.align_info.cl_options_custom_offset_ext := parse_extended(test_box_file, 'cl_options_custom_offset_ext');
        old_keep_dims1.box_dims1.align_info.reminder_flag := parse_boolean(test_box_file, 'reminder_flag');
        old_keep_dims1.box_dims1.align_info.reminder_colour := parse_integer(test_box_file, 'reminder_colour');
        old_keep_dims1.box_dims1.align_info.reminder_str := parse_string(test_box_file, 'reminder_str', 200);
        old_keep_dims1.box_dims1.align_info.spare_float1 := parse_extended(test_box_file, 'spare_float1');
        old_keep_dims1.box_dims1.align_info.spare_float2 := parse_extended(test_box_file, 'spare_float2');
        old_keep_dims1.box_dims1.align_info.spare_float3 := parse_extended(test_box_file, 'spare_float3');
        old_keep_dims1.box_dims1.align_info.spare_int := parse_integer(test_box_file, 'spare_int');
      // 3 44  <<<  align_info
      old_keep_dims1.box_dims1.rail_type := parse_integer(test_box_file, 'rail_type');
      old_keep_dims1.box_dims1.fb_kludge_template_code := parse_integer(test_box_file, 'fb_kludge_template_code');
      old_keep_dims1.box_dims1.box_save_done := parse_boolean(test_box_file, 'box_save_done');
      old_keep_dims1.box_dims1.uninclined_rails := parse_boolean(test_box_file, 'uninclined_rails');
      old_keep_dims1.box_dims1.disable_f7_snap := parse_boolean(test_box_file, 'disable_f7_snap');
      old_keep_dims1.box_dims1.spare_bool4 := parse_boolean(test_box_file, 'spare_bool4');
      old_keep_dims1.box_dims1.mod_text_x := parse_extended(test_box_file, 'mod_text_x');
      old_keep_dims1.box_dims1.mod_text_y := parse_extended(test_box_file, 'mod_text_y');
      old_keep_dims1.box_dims1.flatbottom_width := parse_extended(test_box_file, 'flatbottom_width');
      // 3 54  >>>  check_diffs
        // 4 1  >>>  end_diff_mw
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mw.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 1  <<<  end_diff_mw
        // 4 2  >>>  end_diff_me
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_me.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 2  <<<  end_diff_me
        // 4 3  >>>  end_diff_mr
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mr.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 3  <<<  end_diff_mr
        // 4 4  >>>  end_diff_tw
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tw.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 4  <<<  end_diff_tw
        // 4 5  >>>  end_diff_te
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_te.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 5  <<<  end_diff_te
        // 4 6  >>>  end_diff_tr
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_tr.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 6  <<<  end_diff_tr
        // 4 7  >>>  end_diff_mk
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_mk.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 7  <<<  end_diff_mk
        // 4 8  >>>  end_diff_dk
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.len_diff := parse_extended(test_box_file, 'len_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.flr_diff := parse_extended(test_box_file, 'flr_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.gap_diff := parse_extended(test_box_file, 'gap_diff');
          old_keep_dims1.box_dims1.check_diffs.end_diff_dk.type_diff := parse_byte(test_box_file, 'type_diff');
        // 4 8  <<<  end_diff_dk
      // 3 54  <<<  check_diffs
      old_keep_dims1.box_dims1.retain_diffs_on_make_flag := parse_boolean(test_box_file, 'retain_diffs_on_make_flag');
      old_keep_dims1.box_dims1.retain_diffs_on_mint_flag := parse_boolean(test_box_file, 'retain_diffs_on_mint_flag');
      old_keep_dims1.box_dims1.retain_entry_straight_on_make_flag := parse_boolean(test_box_file, 'retain_entry_straight_on_make_flag');
      old_keep_dims1.box_dims1.retain_entry_straight_on_mint_flag := parse_boolean(test_box_file, 'retain_entry_straight_on_mint_flag');
      old_keep_dims1.box_dims1.retain_shoves_on_make_flag := parse_boolean(test_box_file, 'retain_shoves_on_make_flag');
      old_keep_dims1.box_dims1.retain_shoves_on_mint_flag := parse_boolean(test_box_file, 'retain_shoves_on_mint_flag');
      // 3 61  >>>  turnout_info1
        old_keep_dims1.box_dims1.turnout_info1.plain_track_flag := parse_boolean(test_box_file, 'plain_track_flag');
        old_keep_dims1.box_dims1.turnout_info1.rolled_in_sleepered_flag := parse_boolean(test_box_file, 'rolled_in_sleepered_flag');
        old_keep_dims1.box_dims1.turnout_info1.front_timbers_flag := parse_boolean(test_box_file, 'front_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.approach_rails_only_flag := parse_boolean(test_box_file, 'approach_rails_only_flag');
        old_keep_dims1.box_dims1.turnout_info1.hand := parse_integer(test_box_file, 'hand');
        old_keep_dims1.box_dims1.turnout_info1.timbering_flag := parse_boolean(test_box_file, 'timbering_flag');
        old_keep_dims1.box_dims1.turnout_info1.switch_timbers_flag := parse_boolean(test_box_file, 'switch_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.closure_timbers_flag := parse_boolean(test_box_file, 'closure_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.xing_timbers_flag := parse_boolean(test_box_file, 'xing_timbers_flag');
        old_keep_dims1.box_dims1.turnout_info1.exit_timbering := parse_integer(test_box_file, 'exit_timbering');
        old_keep_dims1.box_dims1.turnout_info1.turnout_road_code := parse_integer(test_box_file, 'turnout_road_code');
        old_keep_dims1.box_dims1.turnout_info1.turnout_length := parse_extended(test_box_file, 'turnout_length');
        old_keep_dims1.box_dims1.turnout_info1.origin_to_toe := parse_extended(test_box_file, 'origin_to_toe');
        old_keep_dims1.box_dims1.turnout_info1.step_size := parse_extended(test_box_file, 'step_size');
        old_keep_dims1.box_dims1.turnout_info1.turnout_road_is_adjustable := parse_boolean(test_box_file, 'turnout_road_is_adjustable');
        old_keep_dims1.box_dims1.turnout_info1.turnout_road_is_minimum := parse_boolean(test_box_file, 'turnout_road_is_minimum');
      // 3 61  <<<  turnout_info1
    // 2 1  <<<  box_dims1
  // 1 1  <<<  old_keep_dims1
// 0 0  <<<
