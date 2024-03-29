
(*
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2019  OpenTemplot project contributors

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

                >>>     NOTE TO DEVELOPERS     <<<
                     DO NOT EDIT THIS COMMENT
              It is inserted in this file by running
                  'python3 scripts/addComment.py'
         The original text lives in scripts/addComment.py.

====================================================================================
*)

type

  						'The following definitions are copied out of keep_select.pas version 292a'


Told_shove=record    // old Tshove record - shove data for a single timber.
                     // as used in program pre version 0.71.a and still used for the
                     // old_shove_info entry in the turnout_info part of a keep_data record on file.
                     // (array of 0..29 of these records). For compatibility when loaded into versions pre 071.
                     // (The first 30 shoved timbers in a template).
                     // 68 bytes per record.

         sv_code :integer;     // 0=empty slot, -1=omit this timber,  1=shove this timber.
         sv_str  :string[8];   // timber number string.

         alignment_byte_1:byte;   // D5 0.81 12-06-05

         sv_x    :extended;    // xtb modifier.
         sv_k    :extended;    // angle modifier.
         sv_o    :extended;    // offset modifier (near end).
         sv_l    :extended;    // length modifier (far end).
         sv_w    :extended;    // width modifier (per side).
         sv_t    :integer;     // nyi - thickness modifier in 1000ths of mm. (was spare integer).
       end;

T048_shoves=record 		' Made into a record to allow extraction by graeme''s hacky extractor, which only pulls out records'
		shoves : array[0..shovedim_c_048] of Told_shove;   // timber shove info for this turnout (first 30 shoved timbers only).
                                                      // this data goes in the files for compatibility with old program versions,
                                                      // but is not used in the program.
       end;


Tshove_for_file=record    // Used in the SHOVE DATA BLOCKS in the 071 files.
                          // But not used within the program - see Ttimber_shove.shove_data instead.
                          // Conversion takes place in 071 on loading.

                  sf_str:string[6];           // timber number string.

                  alignment_byte_1:byte;   // D5 0.81 12-06-05

                  sf_shove_data:Tshove_data;  // all the data.

                end;//record

// Tkeep_dims has the shove timber data omitted.  v:0.71.a  29-4-01.

Tkeep_dims1=record      // first part of Tkeep_dims

              box_dims1:Tbox_dims1;

            end;//record Tkeep_dims1

Tkeep_dims2=record

              turnout_info2:Tturnout_info2;

            end;//record Tkeep_dims2

Told_keep_data=record    // this matches the old Tkeep_data record pre 071 including the timber shove data.
                         // used on loading files.

                 old_keep_dims1:Tkeep_dims1;
                 old_keep_shoves:T048_shoves;        // removed from OT format 
                 old_keep_dims2:Tkeep_dims2;

               end;//record.



  						'The following definitions are copied out of pad_unit.pas version 292a'


  Tdtp_shape_tag=record                         // 0.93.a type cast for dtpShape.Tag (integer)
                   no_update:         boolean;  // Tag=0 means all default to False
                   copyright_mark:    boolean;
                   zooming_rectangle: boolean;
                   sparebool:         boolean;
                 end;

  Tmark=record                     // mark from p1 to p2.
        p1        :TPoint;
	p2        :TPoint;
        code      :integer;
        end;

  Tpex=record                      // x,y point floats (TPoint is integer).
       x :extended;
       y :extended;
       end;

  Textents=record           // 0.93.a
             min:Tpex;
             max:Tpex;
           end;


  Tshove_data=record     // shove data for a single timber ( version 0.71 11-4-01 ).

           sv_code     :integer;     // 0=empty slot, -1=omit this timber,  1=shove this timber.
           sv_x        :extended;    // xtb modifier.
           sv_k        :extended;    // angle modifier.
           sv_o        :extended;    // offset modifier (near end).
           sv_l        :extended;    // length modifier (far end).
           sv_w        :extended;    // width modifier (per side).
           sv_c        :extended;    // crab modifier.  0.78.c  01-02-03.
           sv_t        :extended;    // spare (thickness 3-D modifier - nyi).

           alignment_byte_1:byte;   // D5 0.81 12-06-05
           alignment_byte_2:byte;   // D5 0.81 12-06-05

           sv_sp_int   :integer;     // spare integer.

         end;//record

  Tshoved_timber=class(TPersistent)             // v: 0.71.a  27-4-01.

                 public                         // 0.85.a

                   shove_data:Tshove_data;

                 end;//class


  Tnotch=record      //  a notch position.
           notch_x:extended;
           notch_y:extended;
           notch_k:extended;
         end;


  Tboundary_info=record            // 213b  for extend to boundary function

                    loc_0:Tnotch;     // CTRL-0
                    loc_6:Tnotch;     // CTRL-6
                    loc_9:Tnotch;     // CTRL-9
                    loc_240:Tnotch;   // TMINP
                    loc_241:Tnotch;   // TEXITP
                    loc_260:Tnotch;   // MMINP     // 217a
                    loc_261:Tnotch;   // MEXITP    // 217a
                    loc_600:Tnotch;   // TOLP

                    boundary_diag:extended; // diagonal length between boundaries
                 end;


  Tpad_view_data=record                 // 0.91.c  a zoom/pan setting.
                   offset_x:extended;
                   offset_y:extended;
                   width_x:extended;
                 end;

  Tpad_view=class(TPersistent)          // 0.91.c    object in pad_view_list.

                 public

                   pad_view_data:Tpad_view_data;

                 end;//class


  Thtml_history=record    // html viewer history record.  // 0.91

                  src_code: integer;       // 1=src_str is file name.  0 or negative =src_str is actual html source (help code).
                  src_position:integer;    // html viewer position within scr_str.
                  src_str:string;
                end;



  Trail_info=record     // rail switch settings.  23-5-01.

                        // !!! 17-1-00 - exhaustive testing done to get file match with previous version.
                        // !!! with both same file size and correct reading of bgnd_flag.
                        // !!! Due to Delphi2 aligning boundaries. Don't change anything!!! ...

              flared_ends_ri:integer;  // 0=straight bent, 1=straight machined

                          // spares..

              spare_int1:integer;

              knuckle_code_ri:integer;      // 214a spare_int2:integer;     0=normal, -1=sharp, 1=use custom knuckle_radius_ri
              knuckle_radius_ri:extended;   // 214a spare_float1:extended;  custom setting - inches full-size

              spare_float2:extended;

              spare_bool1:boolean;
              spare_bool2:boolean;

              isolated_crossing_sw:boolean;            //  217a   spare_bool3:boolean;

                           // rail switches ..

              k_diagonal_side_check_rail_sw:boolean;    // added 0.93.a
              k_main_side_check_rail_sw:boolean;        // added 0.93.a

              switch_drive_sw:boolean;   // 0.82.a  13-10-06

                          // rail switches...

              track_centre_lines_sw:        boolean;
              turnout_road_stock_rail_sw:   boolean;
              turnout_road_check_rail_sw:   boolean;
              turnout_road_crossing_rail_sw:boolean;
              crossing_vee_sw:              boolean;
              main_road_crossing_rail_sw:   boolean;
              main_road_check_rail_sw:      boolean;
              main_road_stock_rail_sw:      boolean;

              alignment_byte_1:byte;   // D5 0.81 12-06-05
              alignment_byte_2:byte;   // D5 0.81 12-06-05


            end;

  //----------------------------

       // bgnd shapes ...

  Temf=record
         emf_HDC:HDC;
         emf_width_mm:double;    // frame size in mm
         emf_height_mm:double;
       end;

  Timage_shape=record
                 image_bitmap:TBitmap;           // image bitmap.
                 rotated_bitmap:TBitmap;         // rotated bitmap for printing. // 0.93.a also used for curving bitmap

                 rotated_picture:TPicture;       // picture object to contain rotated bitmap.

                 image_width:integer;
                 image_height:integer;

                 image_metafile:Temf;       // T3-FIRST  219a
               end;

  Tbgimage=class(TPersistent)             // 3-2-01

           public                         // 0.85.a

               image_shape:Timage_shape;

           end;//class


  Tbgnd_shape=record

                    // as in BGS file...

                shape_name:string[46];    // name or text string.  205e

                wrap_offset:integer;      // 1/100th mm for picture shape wrapping  205e

                show_transparent:boolean; // 0.93.a  for bitmap image    //alignment_byte_1:byte;   // D5 0.81 12-06-05

                shape_code:integer;       // 0=line, 1=rectangle, 2=circle, 3=text, 4=target mark, -1=picture (bitmap image or metafile)

                  shape_style:byte;         // 0=transparent, 1=blank/solid, 2=cross-hatched;  // 213b was integer (stored MSB (byte) first)

                  picture_is_metafile:boolean;      // 213b     // spare1:boolean;

                  hide_bits:byte;           // 214a  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both // spare2:byte;

                  option_bits:byte;         // 219b.1 spare3:byte;

                p1:Tpex;
                p2:Tpex;
              end;


  Tbgshape=class(TPersistent)

           public                         // 0.85.a

              bgimage:Tbgimage;           // pointer to image object. 3-2-01.
              bgnd_shape:Tbgnd_shape;     // data for a background shape.

            end;//class

  Tshapefile = file of Tbgnd_shape;       // the shapes data file   .bgs3

  //----------------------------

  Tdummy_vehicle_corners=record         // 0.98.a    mm on grid.
                           pt1:Tpex;
                           pt2:Tpex;
                           pb1:Tpex;
                           pc1:Tpex;
                           pb2:Tpex;
                           pc2:Tpex;
                           b1:Tpex;
                           b2:Tpex;
                           b3:Tpex;
                           b4:Tpex;
                           c1:Tpex;
                           c2:Tpex;
                           c3:Tpex;
                           c4:Tpex;
                           m1:Tpex;
                           m2:Tpex;
                           o1:Tpex;
                           o2:Tpex;
                           o3:Tpex;
                           o4:Tpex;
                         end;

  Tdummy_vehicle_envelope=record         // 215c    mm on grid.
                           b1:Tpex;
                           b2:Tpex;
                           b3:Tpex;
                           b4:Tpex;
                           c1:Tpex;
                           c2:Tpex;
                           c3:Tpex;
                           c4:Tpex;
                           o1:Tpex;
                           o2:Tpex;
                           o3:Tpex;
                           o4:Tpex;
                         end;

  Tdv_envelopes=record         // 215c
                  dv_outlines:array[0..dv_outlines_c] of Tdummy_vehicle_envelope;  // string of 5000 dummy vehicle copies to create outline envelope 215c.
                  dv_outlines_limit:integer;
                end;


  Tbgnd_keep=record

               xlist_max:integer;     // max and min list values for printing calcs and DXF,
               xlist_min:integer;     // and pad reset...
               ylist_max:integer;
               ylist_min:integer;

               planing_end_aq1:integer;   // list index for these locations for printing.
               planing_end_aq2:integer;

               text_begin_X:integer;    // name label text locations for mouse select (screen pixels).
               text_begin_Y:integer;

               text_end_X:integer;      // rectangle for mouse hover detect.
               text_end_Y:integer;

               requested_label_string:string;   // his settings.
               full_label_string:string;        // full number+name.
               showing_label_string:string;     // actual string showing.

               text_font_height:integer;   // 211b was textfontsize:integer;

               timber_numbers_string:string;    // the complete numbers sequence, separated by ESC ($1B) characters.

               list_bgnd_marks:array[0..4] of Pointer;               // pointers only, so can't save this data in a file (of Tbgnd_keep).
               list_bgnd_rails:array[0..aq_max_c, 0..1] of Pointer;

               bgnd_endmarks:array[0..aq_max_c, 0..1] of TPoint;     // rail end mark points. 1/100th mm , curved ready for drawing.
               bgnd_endmarks_yn:array[0..aq_max_c, 0..1] of boolean; // flag end points exist.
              end;

  //-----------------------

  Tspares=record         // some spare slots for patching future changes...

            spare_int1:integer;
            spare_int2:integer;

            spare_flag1:boolean;
            spare_flag2:boolean;
            spare_flag3:boolean;
            spare_flag4:boolean;

            spare_float1:extended;
            spare_float2:extended;
            spare_float3:extended;

            spare_str:string[250];

            alignment_byte_1:byte;   // D5 0.81 12-06-05
            alignment_byte_2:byte;   // D5 0.81 12-06-05
            alignment_byte_3:byte;   // D5 0.81 12-06-05

          end;//record

  Tgauge_scale=record              // was part of Tgauge_info.
                                   // now used only for the list, not in file. 0.71.a


                 name_str_glist:string;           // 215a gauge designation.


                      scale_glist:extended;       // mm per ft.
                      gauge_glist:extended;       // mm.
                         fw_glist:extended;       // mm flangeway.
                        fwe_glist:extended;       // mm flangeway end gap (flangeway+flare).

                    old_fwe_glist:extended;       // old pre-215a flangeway end gap.

                   trtscent_glist:extended;       // mm track centres, turnout side.
                   trmscent_glist:extended;       // mm ditto, main side.
                 min_radius_glist:extended;       // mm minimum radius for check.
               end;

  Tproto_info=record              // was Tgauge_info.

                name_str_pi:string[15];       // gauge designation: 9 chars max actually used

                spare_str_pi:string[75];      // now spares 215a   was  list_str_pi

                scale_pi     :extended;       // mm per ft.
                gauge_pi     :extended;       // mm.
                fw_pi        :extended;       // mm flangeway.
                fwe_pi       :extended;       // mm flangeway end (flangeway+flare).
                xing_fl_pi   :extended;       // mm length of flares (not h-d).
                railtop_pi   :extended;       // mm width of rail top (and bottom if bullhead).
                trtscent_pi  :extended;       // mm track centres, turnout side.
                trmscent_pi  :extended;       // mm ditto, main side.
                retcent_pi   :extended;       // mm ditto, return curve.
                min_radius_pi:extended;       // mm minimum radius for check.


                // these 6 wing/check rail lengths used only in pre 0.71.a versions...

                   old_winglongs_pi :extended;       // inches full-size length of short wing rail from centre of timber A.
                   old_winglongl_pi :extended;       // inches full-size length of long wing rail from centre of timber A.

                   old_cklongs_pi   :extended;       // inches full-size length of short check rails.
                   old_cklongm_pi   :extended;       // inches full-size length of medium check rails.
                   old_cklongl_pi   :extended;       // inches full-size length of long check rails.
                   old_cklongxl_pi  :extended;       // inches full_size length of extra long check rails.

                tbwide_pi    :extended;       // inches full-size width of turnout timbers.
                slwide_pi    :extended;       // inches full-size width of plain sleepers (not at rail joints 212a).

                xtimbsp_pi   :extended;   // !!! disused in 0.75.a 14-10-01. inches full-size timber-spacing at crossing.
                                          // retained in files when loaded by versions prior to 0.75.a

                ftimbspmax_pi:extended;       // inches full-size max timber-spacing for closure space.

                tb_pi        :extended;       // plain sleeper length mm.

                                // added in version 0.71.a 11-5-01...

                      // !!! 11-5-01 - v:0.71.a
                      // !!! exhaustive testing done to get file match with previous version.
                      // !!! Due to Delphi aligning boundaries. Don't change anything!!! ...

                mainside_ends_pi:       boolean;    //  True=main side ends in line,
                                                    //  False=ends centralized.


                jt_slwide_pi:single;       // !!! single. inches full-size width of plain sleepers at rail joints. // 212a


                alignment_byte_1:byte;   // D5 0.81 12-06-05

                random_end_pi:          extended;    //  amount of timber-end randomising.
                timber_thick_pi:        extended;    //  timber thickness (for DXF 3D).
                random_angle_pi:        extended;    //  amount of timber_angle randomising.

                                 // new check and wing dimensioning : v:0.71.a 24-5-01...

                ck_ms_working1_pi:        extended;   // full-size inches - size 1 MS check rail working length (back from "A").
                ck_ms_working2_pi:        extended;   // full-size inches - size 2 MS check rail working length (back from "A").
                ck_ms_working3_pi:        extended;   // full-size inches - size 3 MS check rail working length (back from "A").

                ck_ts_working_mod_pi:     extended;   // full-size inches - TS check rail working length modifier.
                                                         // out of use 0.94.a but loaded in old files.

                ck_ms_ext1_pi:            extended;   // full-size inches - size 1 MS check rail extension length (forward from "A").
                ck_ms_ext2_pi:            extended;   // full-size inches - size 2 MS check rail extension length (forward from "A").

                ck_ts_ext_mod_pi:         extended;   // full-size inches - TS check rail extension length modifier.
                                                         // out of use 0.94.a but loaded in old files.

                wing_ms_reach1_pi:        extended;   // full-size inches - size 1 MS wing rail reach length (forward from "A").
                wing_ms_reach2_pi:        extended;   // full-size inches - size 2 MS wing rail reach length (forward from "A").

                wing_ts_reach_mod_pi:     extended;   // full-size inches - TS wing rail reach length modifier.
                                                         // out of use 0.94.a but loaded in old files.

                   // new rail section dims 0.71.a...

                railbottom_pi:          extended;    // mm width of railfoot (FB).                                   // spare_float4:extended;

                                 // these are for 3-D in DXF...

                rail_height_pi:         extended;    // full-size inches rail height (for 3D in DXF).
                seat_thick_pi:          extended;    // full-size inches chair seating thickness (for 3D in DXF).

                old_tb_pi:              extended;    // inches full-size (unlike tb_pi which is mm). used internally for gauge changes (no meaning in file).

                rail_inclination_pi:    extended;    // radians.
                foot_height_pi:         extended;    // inches full-size  edge thickness.
                chair_outlen_pi:        extended;    // inches full-size  from rail gauge-face
                chair_inlen_pi:         extended;    // inches full-size
                chair_width_pi:         extended;    // inches full-size
                chair_corner_pi:        extended;    // inches full-size  corner rad.

                spare_byte1:         byte;   //  !!! don't replace these an integer !!!
                spare_byte2:         byte;   //  !!! Delphi will upset the align boundaries for proto_info within template_info. !!!
                spare_byte3:         byte;
                spare_byte4:         byte;
                spare_byte5:         byte;

                alignment_byte_2:byte;   // D5 0.81 12-06-05

              end;

  Ttransform_info=record             //  datums, shifts and rotations ...
                                 //  (yes I know the plural of datum is data !)

                datum_y:       extended;  // y_datum, y datum point (green dot).

                x_go_limit:    extended;  // (nyi) print cropping limits (paper inches)...
                x_stop_limit:  extended;

                transforms_apply:boolean; // !!! no longer used.  // False = ignore transform data.

                alignment_byte_1:byte;   // D5 0.81 12-06-05

                x1_shift:      extended;  //  mm    shift info...
                y1_shift:      extended;  //  mm
                k_shift:       extended;  //  radians.
                x2_shift:      extended;  //  mm
                y2_shift:      extended;  //  mm

                peg_pos:       Tpex;      //  mm  peg position.

                alignment_byte_2:byte;   // D5 0.81 12-06-05
                alignment_byte_3:byte;   // D5 0.81 12-06-05

                peg_point_code:integer;   //  peg_code.
                peg_point_rail:integer;   //  peg_rail.

                mirror_on_x:   boolean;   //  True= invert on x.
                mirror_on_y:   boolean;   //  True= invert on y. (swap hand).

                alignment_byte_4:byte;   // D5 0.81 12-06-05
                alignment_byte_5:byte;   // D5 0.81 12-06-05

                spare_int1:integer;
                spare_int2:integer;

                spare_flag1:boolean;
                spare_flag2:boolean;
                spare_flag3:boolean;
                spare_flag4:boolean;

                notch_info:Tnotch;      {spare_float1:extended;}    // 11-4-00 version 0.53
                                        {spare_float2:extended;}
                                        {spare_float3:extended;}

                spare_str:string[10];

                alignment_byte_6:byte;   // D5 0.81 12-06-05
                alignment_byte_7:byte;   // D5 0.81 12-06-05
                alignment_byte_8:byte;   // D5 0.81 12-06-05

              end;//record

  Tplatform_trackbed_info=record   // 0.93.a was  Tcheck_rail_mints=record

              adjacent_edges_keep:boolean;    // False=adjacent tracks,  True=trackbed edges and platform edges.

              draw_ms_trackbed_edge_keep:boolean;
              draw_ts_trackbed_edge_keep:boolean;

              spare_bool1:boolean;

              OUT_OF_USE_trackbed_width_ins_keep:extended;    // 180 inches full-size 15ft.  // not used 215a  TS and MS separated, see below

              draw_ts_platform_keep:boolean;
              draw_ts_platform_start_edge_keep:boolean;
              draw_ts_platform_end_edge_keep:boolean;
              draw_ts_platform_rear_edge_keep:boolean;

              platform_ts_front_edge_ins_keep:extended;      // centre-line to platform front edge 57 inches   4ft-9in  215a
              platform_ts_start_width_ins_keep:extended;
              platform_ts_end_width_ins_keep:extended;

              platform_ts_start_mm_keep:extended;
              platform_ts_length_mm_keep:extended;


              draw_ms_platform_keep:boolean;
              draw_ms_platform_start_edge_keep:boolean;
              draw_ms_platform_end_edge_keep:boolean;
              draw_ms_platform_rear_edge_keep:boolean;

              platform_ms_front_edge_ins_keep:extended;      // centre-line to platform front edge 57 inches   4ft-9in  215a
              platform_ms_start_width_ins_keep:extended;
              platform_ms_end_width_ins_keep:extended;

              platform_ms_start_mm_keep:extended;
              platform_ms_length_mm_keep:extended;

              OUT_OF_USE_cess_width_ins_keep:extended;            // 206a     // not used 215a  TS and MS separated, see below
              OUT_OF_USE_draw_trackbed_cess_edge_keep:boolean;    // 206a     // not used 215a  TS and MS separated, see below

                // platform skews added 207a...

              platform_ms_start_skew_mm_keep:extended;      // 207a
              platform_ms_end_skew_mm_keep:extended;        // 207a

              platform_ts_start_skew_mm_keep:extended;      // 207a
              platform_ts_end_skew_mm_keep:extended;        // 207a


              spare_bool2:boolean;
              spare_bool3:boolean;
              spare_bool4:boolean;
              spare_bool5:boolean;
              spare_bool6:boolean;
              spare_bool7:boolean;
              spare_bool8:boolean;


                  // new trackbed edge functions 215a ...   split MS and TS settings  -  using Single floats to fit available file space ...

              trackbed_ms_width_ins_keep:Single;
              trackbed_ts_width_ins_keep:Single;

              cess_ms_width_ins_keep:Single;
              cess_ts_width_ins_keep:Single;

              draw_ms_trackbed_cess_edge_keep:boolean;
              draw_ts_trackbed_cess_edge_keep:boolean;

              spare1:boolean;
              spare2:boolean;                        // 215a spare_extended1:extended; spare_extended2:extended;

              trackbed_ms_start_mm_keep:extended;    // 215a spare_extended3:extended;    // need to be extendeds for def_req
              trackbed_ms_length_mm_keep:extended;   // 215a spare_extended4:extended;

              trackbed_ts_start_mm_keep:extended;    // 215a spare_extended5:extended;
              trackbed_ts_length_mm_keep:extended;   // 215a spare_extended6:extended;

            end;


  Talignment_info=record              //  curving and transition info...

               curving_flag:   boolean;    // !!! no longer used 0.77.a !!! True=curved, False=straight.
                                           // but needed for check on loading older files.
                                           // - all templates now curved (straight=max_rad).

               trans_flag:     boolean;    // True=transition, False=fixed radius curving.

	       fixed_rad:      extended;   // fixed radius mm.
               trans_rad1:     extended;   // first transition radius mm.
               trans_rad2:     extended;   // second transition radius mm.
               trans_length:   extended;   // length of transition mm.
               trans_start:    extended;   // start of transition mm.
               rad_offset:     extended;   // curving line offset mm. no longer used

               alignment_byte_1:byte;   // D5 0.81 12-06-05
               alignment_byte_2:byte;   // D5 0.81 12-06-05

               tanh_kmax:double;          {spare_int1:integer;}   // factor for mode 2 slews.
                                          {spare_int2:integer;}   // !!! double used because only 8 bytes available in existing file format (2 integers).

               slewing_flag:   boolean;   {spare_flag1:boolean;}  // slewing flag.
               cl_only_flag:   boolean;   {spare_flag2:boolean;}  // draw track centre-line only for bgnd

               slew_type:byte;            {spare_flag3:boolean;}  // !!! byte used because only 1 byte available in existing file format 1-11-99.

               dummy_template_flag:boolean;  // 212a       //spare_flag4:boolean;

               slew_start:     extended;  {spare_float1:extended;}  // slewing zone start mm.
               slew_length:    extended;  {spare_float2:extended;}  // slewing zone length mm.
               slew_amount:    extended;  {spare_float3:extended;}  // amount of slew mm.


               cl_options_code_int:integer;            // 206a
               cl_options_custom_offset_ext:extended;  // 206a

                // 216a ...

               reminder_flag:boolean;
               reminder_colour:integer;

               reminder_str:string[200];



               spare_float1:extended;
               spare_float2:extended;
               spare_float3:extended;

               spare_int:integer;

	     end;//record
  //_________________________________________________

  // plain-track record includes user-defined peg data...

  Tplain_track_info=record

                 pt_custom:boolean;        // custom plain track flag.

                 alignment_byte_1:byte;   // D5 0.81 12-06-05
                 alignment_byte_2:byte;   // D5 0.81 12-06-05
                 alignment_byte_3:byte;   // D5 0.81 12-06-05

                 list_index:integer;
                 rail_length:extended;         // rail length in inches.

                 alignment_byte_4:byte;   // D5 0.81 12-06-05
                 alignment_byte_5:byte;   // D5 0.81 12-06-05

                 sleepers_per_length:integer;                     // number of sleepers per length.
                 sleeper_centres:array[0..psleep_c] of extended;  // spacings in inches for custom.

                 rail_joints_code:integer;   // 0=normal, 1=staggered, -1=none (cwr).

                 user_peg_rail:integer;   // was pt_spare_int2:integer; 13-3-01.

                 pt_spare_flag1:boolean;
                 pt_spare_flag2:boolean;
                 pt_spare_flag3:boolean;

                 user_peg_data_valid:boolean;    // was pt_spare_flag4:boolean;  13-3-01

                 user_pegx:extended;          // was pt_spare_float1:extended;  13-3-01
                 user_pegy:extended;          // was pt_spare_float2:extended;  13-3-01
                 user_pegk:extended;          // was pt_spare_float3:extended;  13-3-01

                 pt_spacing_name_str:string[200];     // was spare_str:string[250];   17-1-01.

                 alignment_byte_6:byte;   // D5 0.81 12-06-05

                 pt_tb_rolling_percent:extended;      // 0.76.a  17-5-02.

                 gaunt_sleeper_mod_inches:extended;       // 0.93.a ex 0.81 pt_spare_ext4:extended;

                 pt_spare_ext3:extended;
                 pt_spare_ext2:extended;
                 pt_spare_ext1:extended;

                 alignment_byte_7:byte;   // D5 0.81 12-06-05
                 alignment_byte_8:byte;   // D5 0.81 12-06-05

               end;//record

  //________________________________________________

  //  these record types apply to turnouts only...

  Tswitch_info=record      // switch stuff...

            old_size:        integer;       // old index into list of switches (pre 0.77.a).
            sw_name_str:     string[100];   // name of switch.

            alignment_byte_1:byte;   // D5 0.81 12-06-05
            alignment_byte_2:byte;   // D5 0.81 12-06-05
            alignment_byte_3:byte;   // D5 0.81 12-06-05

            sw_pattern:         integer;    // type of switch.
            planing:         extended;   // (B) planing length (inches).
	    planing_angle:   extended;   // unit planing angle.
            switch_radius_inchormax:extended;   // switch radius (inches!) (or max_rad (in mm) for straight switch).
            switch_rail:     extended;   // (C) length of switch rail (inches).
            stock_rail:      extended;   // (S) length of stock rail (inches).
            heel_lead_inches:       extended;   // (L) lead to heel (incl. planing) (inches).
            heel_offset_inches:     extended;   // (H) heel-offset (inches).
            switch_front_inches:    extended;   // stock-rail-end to toe (inches).
            planing_radius:  extended;   // planing radius for double-curved switch.
            sleeper_j1:      extended;   // first switch-front sleeper spacing back from TOE (NEGATIVE inches).
            sleeper_j2:      extended;   // second switch-front sleeper spacing back from the first (NEGATIVE inches).

            timber_centres:  array[0..swtimbco_c] of extended;  // list of timber centres (in inches).

            group_code:      integer;    //  which group of switches.        0.77.a  7-6-02.
            size_code:       integer;    //  size within group (1=shortest). 0.77.a  7-6-02.

            joggle_depth:    extended;   //  depth of joggle. 0.71.a 13-4-01.
	    joggle_length:   extended;   //  length of joggle in front of toe (+ve). 0.71.a 13-4-01.

            group_count:     integer;    // number of switches in this group (max size_code in this group, min size is always 1).

            joggled_stock_rail:   boolean;    //  True = joggled stock rail.

            alignment_byte_4:byte;   // D5 0.81 12-06-05
            alignment_byte_5:byte;   // D5 0.81 12-06-05
            alignment_byte_6:byte;   // D5 0.81 12-06-05

            spare_int2:integer;
            spare_int1:integer;

            valid_data:      boolean;    // True = valid data here. 0.77.a 9-6-02...
            front_timbered:  boolean;    // True = switch front sleepers are timber width.

            num_bridge_chairs_main_rail:byte;     // not used in experimental chairing   // 214a              spare_byte
            num_bridge_chairs_turnout_rail:byte;  // not used in experimental chairing   // 214a              spare_byte

            fb_tip_offset:   extended;   // 0.76.a  2-1-02. fbtip dimension (FB foot from gauge-face at tip).

            sleeper_j3:      extended;   //  third switch-front sleeper spacing back from the second (NEGATIVE inches).
            sleeper_j4:      extended;   //  fourth switch-front sleeper spacing back from the third (NEGATIVE inches).
            sleeper_j5:      extended;   //  fifth switch-front sleeper spacing back from the fourth (NEGATIVE inches).

            spare_float4:extended;
            spare_float3:extended;
            spare_float2:extended;
            spare_float1:extended;

            spare_str:string[200];

            num_slide_chairs:byte;           // 214a alignment_byte_7:byte;   // D5 0.81 12-06-05
            num_block_slide_chairs:byte;     // 214a alignment_byte_8:byte;   // D5 0.81 12-06-05
            num_block_heel_chairs:byte;      // 214a alignment_byte_9:byte;   // D5 0.81 12-06-05

          end;//record

  Tcheck_flare_info_081=record
                   // not used 0.93.a

                   // 0.81 new flare lengths.  04-08-03.

              check_flare_ext_ms:extended;      // flare length (inches), MS check rail extension end.
              check_flare_ext_ts:extended;      // flare length (inches), TS check rail extension end.
              check_flare_work_ms:extended;     // flare length (inches), MS check rail working end.
              check_flare_work_ts:extended;     // flare length (inches), TS check rail working end.
              wing_flare_ms:extended;           // flare length (inches), MS wing rail.
              wing_flare_ts:extended;           // flare length (inches), TS wing rail.
              check_flare_k_ms:extended;        // flare length (inches), MS K-crossing check rail.
              check_flare_k_ds:extended;        // flare length (inches), DS K-crossing check rail.

                            // 0.81 new flare offsets (flangeway end gap).  04-08-03.

              check_fwe_ext_ms:extended;      // flangeway end gap (mm), MS check rail extension end.
              check_fwe_ext_ts:extended;      // flangeway end gap (mm), TS check rail extension end.
              check_fwe_work_ms:extended;     // flangeway end gap (mm), MS check rail working end.
              check_fwe_work_ts:extended;     // flangeway end gap (mm), TS check rail working end.
              wing_fwe_ms:extended;           // flangeway end gap (mm), MS wing rail.
              wing_fwe_ts:extended;           // flangeway end gap (mm), TS wing rail.
              check_fwe_k_ms:extended;        // flangeway end gap (mm), MS K-crossing check rail.
              check_fwe_k_ds:extended;        // flangeway end gap (mm), DS K-crossing check rail.

                    end;//record

  Tcrossing_info=record        // crossing stuff...

              pattern:           integer;     // 0=straight, 1=curviform, 2=parallel, -1=generic.

	      sl_mode:           integer;     // 0=auto_fit, 1=use fixed_sl.
              retcent_mode:      integer;     // 0=return centres as adjacent track, 1=use custom centres.
              k3n_unit_angle:    extended;    // k3n angle in units.
              fixed_st:          extended;    // length of knuckle straight. mm.

              spare_int3:        integer;

              hd_timbers_code:   integer;     // extended half-diamond timbers for slip road.
              hd_vchecks_code:   integer;     // shortening code for half-diamond v-crossing check rails.

              k_check_length_1:  extended;    // length of size 1 k-crossing check rail (inches).
	      k_check_length_2:  extended;    // length of size 2 k-crossing check rail (inches).
              k_check_mod_ms:    extended;    // main side modifer.
              k_check_mod_ds:    extended;    // diamond side modifer.
              k_check_flare:     extended;    // length of flare on k-crossing check rails.

              curviform_timbering_keep:boolean;  // 215a                           alignment_byte_1:byte;   // D5 0.81 12-06-05
              
              alignment_byte_2:byte;   // D5 0.81 12-06-05

              main_road_code:integer;      //  length of main-side exit road.      //  217a  spare_int2:integer;

              tandem_timber_code:integer;   //   218a      spare_int1:        integer;

                  // 0.75.a  9-10-01...

              blunt_nose_width:    extended;    // full-size inches.
              blunt_nose_to_timb:  extended;    // full-size inches - to "A" timber centre.

              vee_joint_half_spacing: extended;    // full-size inches - rail overlap at vee point rail joint.
              wing_joint_spacing:     extended;    // full-size inches - timber spacing at wing rail joint.

              wing_timber_spacing: extended;    // full-size inches - timber spacing for wing rail front part of crossing (up to "A").
              vee_timber_spacing:  extended;    // full-size inches - timber spacing for vee point rail part of crossing (on from "A").

                   // number of timbers spanned by vee rail incl. "A" timber.

              vee_joint_space_co1:byte;
              vee_joint_space_co2:byte;
              vee_joint_space_co3:byte;
              vee_joint_space_co4:byte;
              vee_joint_space_co5:byte;
              vee_joint_space_co6:byte;

                   // number of timbers spanned by wing rail front excl. "A" timber...

              wing_joint_space_co1:byte;
              wing_joint_space_co2:byte;
              wing_joint_space_co3:byte;
              wing_joint_space_co4:byte;
              wing_joint_space_co5:byte;
              wing_joint_space_co6:byte;

              spare_flag1:boolean;
              spare_flag2:boolean;

              main_road_endx_infile:extended;  // 217a

              hdkn_unit_angle:extended;    // half-diamond hdkn angle in units.

              check_flare_info_081:Tcheck_flare_info_081;   // not used 0.93.a

              k_custom_wing_long_keep:extended;   // 0.95.a inches full-size k-crossing wing rails
              k_custom_point_long_keep:extended;  // 0.95.a inches full-size k-crossing point rails   NYI

              use_k_custom_wing_rails_keep:boolean;   // 0.95.a
              use_k_custom_point_rails_keep:boolean;  // 0.95.a  NYI

              spare_str:string[10];    // 0.95.a was 30

              alignment_byte_3:byte;   // D5 0.81 12-06-05

            end;//record

  Tturnout_info1=record          // data for the turnout size...

                   plain_track_flag: boolean;      //  True=plain track only.

                   rolled_in_sleepered_flag:boolean;  // 223a  alignment_byte_1:byte;   // D5 0.81 12-06-05

                   front_timbers_flag:boolean;     //  218a    alignment_byte_2:byte;   // D5 0.81 12-06-05

                   approach_rails_only_flag:boolean;   //  218a    alignment_byte_3:byte;   // D5 0.81 12-06-05

                   hand:             integer;      //  hand of turnout.
                   timbering_flag:   boolean;      //  True = equalized timbering.

                   switch_timbers_flag:boolean;    //  218a    alignment_byte_4:byte;   // D5 0.81 12-06-05
                   closure_timbers_flag:boolean;   //  218a    alignment_byte_5:byte;   // D5 0.81 12-06-05
                   xing_timbers_flag:boolean;      //  218a    alignment_byte_6:byte;   // D5 0.81 12-06-05

                   exit_timbering:   integer;      //  exit timbering style.
                   turnout_road_code:integer;      //  length of turnout exit road.

                   turnout_length:   extended;     //  turnoutx.
                   origin_to_toe:    extended;     //  xorg.
                   step_size:        extended;     //  incx. (use saved step-size on reloading - not default).

                   turnout_road_is_adjustable:boolean;  // 211a    alignment_byte_7:byte;   // D5 0.81 12-06-05

                   turnout_road_is_minimum:boolean;     // 217a    alignment_byte_8:byte;   // D5 0.81 12-06-05

                 end;//tturnout_info1 record

  Thdk_check_rail_info=record         // K-crossing check and wing rail lengths. 0.79.a

                        k_check_ms_1:        extended;   // full-size inches - size 1 MS k-crossing check rail length.
                        k_check_ms_2:        extended;   // full-size inches - size 2 MS k-crossing check rail length.

                        k_check_ds_1:        extended;   // full-size inches - size 1 DS k-crossing check rail length.
                        k_check_ds_2:        extended;   // full-size inches - size 2 DS k-crossing check rail length.
                     end;

  Tvee_check_rail_info=record         // V-crossing check and wing rail lengths. 0.79.a

                        v_check_ms_working1:        extended;   // full-size inches - size 1 MS check rail working length (back from "A").
                        v_check_ms_working2:        extended;   // full-size inches - size 2 MS check rail working length (back from "A").
                        v_check_ms_working3:        extended;   // full-size inches - size 3 MS check rail working length (back from "A").

                        v_check_ts_working1:        extended;   // full-size inches - size 1 TS check rail working length (back from "A").
                        v_check_ts_working2:        extended;   // full-size inches - size 2 TS check rail working length (back from "A").
                        v_check_ts_working3:        extended;   // full-size inches - size 3 TS check rail working length (back from "A").

                        v_check_ms_ext1:            extended;   // full-size inches - size 1 MS check rail extension length (forward from "A").
                        v_check_ms_ext2:            extended;   // full-size inches - size 2 MS check rail extension length (forward from "A").

                        v_check_ts_ext1:            extended;   // full-size inches - size 1 TS check rail extension length (forward from "A").
                        v_check_ts_ext2:            extended;   // full-size inches - size 2 TS check rail extension length (forward from "A").

                        v_wing_ms_reach1:           extended;   // full-size inches - size 1 MS wing rail reach length (forward from "A").
                        v_wing_ms_reach2:           extended;   // full-size inches - size 2 MS wing rail reach length (forward from "A").

                        v_wing_ts_reach1:           extended;   // full-size inches - size 1 TS wing rail reach length (forward from "A").
                        v_wing_ts_reach2:           extended;   // full-size inches - size 2 TS wing rail reach length (forward from "A").
                     end;

  Tcheck_end_diff=record    // 0.94.a
                    len_diff:extended;   // length differ  inches f-s
                    flr_diff:extended;   // flare length   inches f-s
                    gap_diff:extended;   // end gap        model mm

                    type_diff:byte;      // 0=no diff   1=change to bent flare    2=change to machined flare   3= change to no flare
                  end;

  Tcheck_diffs=record    // 0.94.a
                 end_diff_mw:Tcheck_end_diff;
                 end_diff_me:Tcheck_end_diff;
                 end_diff_mr:Tcheck_end_diff;
                 end_diff_tw:Tcheck_end_diff;
                 end_diff_te:Tcheck_end_diff;
                 end_diff_tr:Tcheck_end_diff;
                 end_diff_mk:Tcheck_end_diff;
                 end_diff_dk:Tcheck_end_diff;
               end;

                                                                          // but is not used in the program.
  Tturnout_info2=record
                   switch_info:      Tswitch_info;      //  all the switch dimensions.
                   crossing_info:    Tcrossing_info;    //  all the crossing dimensions.
                   plain_track_info: Tplain_track_info; //  need the plain track info for approach and exit tracks.

                   diamond_auto_code:integer;      // 0.77.a 0=auto, 1=fixed diamond, 2=switch diamond.

                   bonus_timber_count:integer;     // 0.76.a number of bonus timbers.

                   equalizing_fixed_flag:boolean;  {spare_flag1:boolean;}   // equalizing style 1-4-00
                   no_timbering_flag:boolean;      {spare_flag2:boolean;}   // no timbering option 7-9-00

                   angled_on_flag:boolean;         {spare_flag3:boolean;}   // angled-on style 29-7-01.

                   chairing_flag:boolean;          // 214a    //spare_flag2:boolean;

                   start_draw_x:extended;          {spare_float3:extended;}   // startx.

                   timber_length_inc:extended;     // timbinc timber length step size.

                   //------
                   omit_switch_front_joints:boolean;  // 0.79.a spare_float1:extended;...
                   omit_switch_rail_joints:boolean;
                   omit_stock_rail_joints:boolean;
                   omit_wing_rail_joints:boolean;
                   omit_vee_rail_joints:boolean;
                   omit_k_crossing_stock_rail_joints:boolean;

                   spare_flag14:boolean;
                   spare_flag13:boolean;
                   spare_flag12:boolean;

                   diamond_switch_timbering_flag:boolean;  // 213a spare_flag11:boolean;

                   //------


                   gaunt_flag:boolean;    // True = gaunt template 0.81.a   //spare_flag10:boolean;

                   diamond_proto_timbering_flag:boolean;    // 0.77.b

                   semi_diamond_flag:boolean;      // True = half-diamond template.
                   diamond_fixed_flag:boolean;     // True = fixed-diamond.


                   hdk_check_rail_info:Thdk_check_rail_info;

                   vee_check_rail_info:Tvee_check_rail_info;

                   turnout_road_endx_infile:extended;   // 209a length of turnout road from CTRL-1   //spare_float:extended;

                        // 208c added to aid debugging of box files in text editor (never read):

                   template_type_str:string[6];  // 208c was spare_str[16]        208a was spare_str:string[56]

                   smallest_radius_stored:extended; // 208a needed for box data -- not loaded to the control

                   dpx_stored:extended;      // 208a needed for ID number creation -- not loaded to the control
                   ipx_stored:extended;      // 208a needed for ID number creation -- not loaded to the control
                   fpx_stored:extended;      // 208a needed for ID number creation -- not loaded to the control


                   gaunt_offset_inches:extended;  // 0.81

                      // 219a  include connectors for XTrackCAD in export DXF file  -- not loaded to the control  ...

                   dxf_connector_0:boolean;  // CTRL-0   // alignment_byte_1:byte;   // D5 0.81 12-06-05
                   dxf_connector_t:boolean;  // TEXITP   // alignment_byte_2:byte;   // D5 0.81 12-06-05
                   dxf_connector_9:boolean;  // CTRL-9   // alignment_byte_3:byte;   // D5 0.81 12-06-05

                 end;//Tturnout_info2 record

  //___________________________________________

  //!!!  was Tkeep_data=record  , Tkeep_dims has the shove timber data omitted.  v:0.71.a  29-4-01.

  Tbox_dims1=record

                box_ident:string[10];   // first 11 bytes. in BOX3,   (string[11], 12 bytes in BOX)

                id_byte:byte;          // set to 255  $FF in BOX3 files - not read.

                now_time:integer;      // date/time/random code at which template added to keep box. (from Delphi float format - fractional days since 1-1-1900).
                                       // this is used to detect duplicates on loading.

                keep_date:string[20];   // ditto as conventional strings.
                keep_time:string[20];

                top_label:         string[100];  // template info label.
                project_for:           string[50];   // his project title string for the boxful. (only read from the last template in the box).

                reference_string:  string[100];  // template name.

                this_was_control_template:boolean;   // 0.93.a // alignment_byte_2:byte;   // D5 0.81 12-06-05

                rail_info:          Trail_info;  // 23-5-01.

                auto_restore_on_startup:boolean;      // these two only read from the first keep in the file..
                ask_restore_on_startup:boolean;

                //---------------------

                pre077_bgnd_flag:boolean;  // no longer used, 0.77.a 2-sep-02. When true, this keep is to be drawn on the background.

                alignment_byte_3:byte;   // D5 0.81 12-06-05

                templot_version:integer;   // program version number (*100, e.g Templot0 v:1.3 = 130).

                file_format_code:integer;  // 0= D5 format,    1= OT format  //spare_int1

                gauge_index: integer;      // current index into the gauge list.

                gauge_exact: Boolean;      // nyi  // If true this is an exact-scale template.
                gauge_custom:Boolean;      // nyi  // If true this is (or was when saved) a custom gauge setting.

                proto_info:           Tproto_info;    // !!! modified for 0.71.a 11-5-01. was Tgauge_info.

                railtop_inches:extended;    // full-size inches railtop width - was spare_float1:extended;
                railbottom_inches:extended; // full-size inches railbottom width - was spare_float2:extended;

                alignment_byte_4:byte;   // D5 0.81 12-06-05
                alignment_byte_5:byte;   // D5 0.81 12-06-05

                version_as_loaded:integer;      // mod 0.78.d  14-Feb-2003. the version number as loaded.

                bgnd_code_077:integer;          // 0=unused, 1=bgnd, -1=library   0.77.a  2-Sep-02.

                print_mapping_colour:integer;   // 0.76.a  27-10-01 //spare_inta:integer;
                pad_marker_colour:integer;      // 0.76.a  27-10-01 //spare_intb:integer;

                use_print_mapping_colour:boolean;  //spare_boola:boolean;
                use_pad_marker_colour:boolean;     //spare_boolb:boolean;

                //-------------------------

                      //  0.79.a 20-05-06  -- saved grid info -- read from last template only...

                spare_bool1:boolean;

                spare_bool2:boolean;  // out 0.93.a   was show_page_margins_on_pad:boolean;

                spare_int2:integer;

                grid_units_code:integer;

                x_grid_spacing:extended;
                y_grid_spacing:extended;

                total_length_of_timbering:extended;  // 0.96.a


                id_number:integer;         // 208a
                id_number_str:string[7];   // 208a     -N00000  

                spare_boolean1:boolean;    // 208a
                spare_boolean2:boolean;    // 208a      //spare_str:string[13];


                transform_info:         Ttransform_info;

                platform_trackbed_info: Tplatform_trackbed_info;  // 0.93.a  was check_rail_mints:Tcheck_rail_mints;

                align_info:             Talignment_info;


                rail_type:integer;     // 0=no rails, 1=head only (bullhead), 2=head+foot (flatbottom).   // spare_int1:integer

                fb_kludge_template_code:integer;       // 0.94.a   0=normal template, 1=inner foot lines, 2=outer foot lines   //spare_int3:integer;

                box_save_done:boolean;   // read only from first keep on restore previous contents. 23-6-00 v:0.62.a      //spare_flag1:boolean;

                uninclined_rails:boolean;      // True = rails vertical.

                disable_f7_snap:boolean;       //  0.82.a  spare_bool3:boolean;

                spare_bool4:boolean;

                mod_text_x:extended;     // (mm) label position modifiers..   //spare_float1:extended;
                mod_text_y:extended;                                          //spare_float2:extended;

                flatbottom_width:extended;     // width of flatbottom rail base (mm).    //spare_float3:extended;

                check_diffs:Tcheck_diffs;      // 0.94.a check rail end modifiers - 248 bytes


                retain_diffs_on_make_flag:boolean;    // 0.94.a check rail diffs
                retain_diffs_on_mint_flag:boolean;    // 0.94.a check rail diffs

                retain_entry_straight_on_make_flag:boolean;   // 213a  spare_byte1:byte;   // 0.94.a
                retain_entry_straight_on_mint_flag:boolean;   // 213a  spare_byte2:byte;   // 0.94.a

                    // 0.94.a timber shoving mods..

                retain_shoves_on_make_flag:boolean;
                retain_shoves_on_mint_flag:boolean;

                turnout_info1: Tturnout_info1;

               end;//record

  Tkeep_dims=record

               box_dims1:      Tbox_dims1;
               turnout_info2:  Tturnout_info2;

             end;//record

  Ttemplate_info=record       // template data.

                    keep_dims:Tkeep_dims;           // all the template dimemsions.
                    keep_shove_list:TStringList;    // the list of shoved timbers. (v:0.71.a  27-4-01).
                 end;//record


  Tsnap_peg_positions=record        // snapping positions for F7 shift mouse action  0.79.a  27-05-06
                                    // and background popup snap options.

                        ctrl_peg_now_pos:Tnotch;
                        ctrl_0_pos:Tnotch;
                        ctrl_1_pos:Tnotch;

                        ctrl_2_pos:Tnotch;  // added 205c

                        ctrl_planing_pos:Tnotch;   // added 205e for obtain turnout radius to control
                        ctrl_heel_pos:Tnotch;      // added 205e for obtain turnout radius to control

                        ctrl_3_pos:Tnotch;

                        ctrl_cesp_pos:Tnotch;      // added 205e for obtain turnout radius to control

                        ctrl_4_pos:Tnotch;
                        ctrl_5_pos:Tnotch;
                        ctrl_6_pos:Tnotch;
                        ctrl_7_pos:Tnotch;
                        ctrl_8_pos:Tnotch;
                        ctrl_9_pos:Tnotch;
                        ctrl_tcp_pos:Tnotch;   // TCP
                        ctrl_mcp_pos:Tnotch;   // MCP
                        ctrl_tolp_pos:Tnotch;  // TOLP

                        ctrl_tminp_pos:Tnotch;     // TMINP     // 213b
                        ctrl_texitp_pos:Tnotch;    // TEXITP    // 213b

                        ctrl_mminp_pos:Tnotch;     // MMINP     // 217a
                        ctrl_mexitp_pos:Tnotch;    // MEXITP    // 217a

                        ctrl_tsmidp_pos:Tnotch;    // TS curve mid-point  218a

                        ctrl_knucklebend_pos:Tnotch;  // start of knuckle bend  218a

                        ctrl_atimb_pos:Tnotch;     // "A" timber 218a

                        ctrl_mid_pos:Tnotch;      // mid-length  216a

                        ctrl_user_pos:Tnotch;  // user-defined peg pos    added 205c

                      end;//record


  Ttemplate=class(TPersistent)       // a whole stored template

            public                         // 0.85.a

              bg_copied:boolean;         // True=has been copied to the background. (not included in file).
              group_selected:boolean;    // True=selected as one of a group.
              new_stamp_wanted:boolean;  // True=has been shifted/rotated/mirrored, needs a new timestamp on rebuilding.

              snap_peg_positions:Tsnap_peg_positions;    // snapping positions for F7 shift mouse action  0.79.a  27-05-06
              boundary_info:Tboundary_info;              // 213b for extend to boundary

              bgnd_half_diamond:boolean;        // used for peg snapping checks. (also in the template_info for file).  0.79.a  27-05-06
              bgnd_plain_track:boolean;         // ditto
              bgnd_retpar:boolean;              // ditto parallel crossing
              bgnd_peg_on_zero:boolean;         // ditto Ctrl-0 or not.

                   // added 205e for obtain tradius to control...

              bgnd_xing_type:integer;
              bgnd_spiral:boolean;
              bgnd_turnout_radius:extended;

              bgnd_gaunt:boolean;               // 218a

              bgnd_is_in_rect:boolean;          // 218d   temp flag   template is within a rectangle (e.g. on screen)

                   // 208a temp strings used while sorting the storage box...

              sort_swap_info_str:string;
              sort_swap_memo_str:string;

                   // 211b position of name label...

              bgnd_label_x:extended;   // mm
              bgnd_label_y:extended;   // mm

              bgnd_blanked:boolean;        // 215a
              bgnd_no_xing:boolean;        // 215a

              this_is_tandem_first:boolean;  // 218a


              template_info:Ttemplate_info;    // the template data.

              bgnd_keep:Tbgnd_keep;    // drawn data for a background template.


	    end;//class


  Tswitch=class(TPersistent)       // a switch stored in the list.     0.77.a  7-6-02.

          public                         // 0.85.a

              list_switch_info:Tswitch_info;    // the data.

	  end;//class


  Troll_back=record
               valid_flag:boolean;
               rollback_info:Ttemplate_info;
               rollback_name_str:string;       // added 0.93.a
               rollback_memo_str:string;       // ...
             end;

//-----------------------------------------

