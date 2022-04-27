unit box3_file_unit;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  template,
  box_file_unit,
  math_unit;

type
  ExSaveBox = class(Exception)
  end;

  ExLoadBox = class(Exception)
  end;

  TGridInfo = record
    unitsCode: integer;
    spaceX: double;
    spaceY: double;
  end;

  TBackupRestoreOptions = record
    autoRestoreOnStartup: boolean;
    askRestoreOnStartup: boolean;
    saveDone: boolean;
  end;

procedure SaveBox3(templatesToSave: TTemplateList;
  saveOption: ESaveOption;
  saveDone: boolean;
  const boxFilename: string;
  const projectTitle: string;
  const gridInfo: TGridInfo);

procedure LoadBox3(
  const boxFilename: string;
  var projectTitle: string;
  var gridInfo: TGridInfo;
  out loadedTemplates: TTemplateList);

function LoadBox3BackupRestoreOptions(const boxFilename: string): TBackupRestoreOptions;

implementation

uses
  Generics.Collections,
  control_room,
  shoved_timber;

// new file format including text 17-2-00. (v:0.48 on).
// newer file format including unlimited shoves in StringList 1-5-01 (v:0.71.a on).

// The 071 file format is the same as the 048 format with the addition of "Data Blocks" at the end of the file.

// The Data Blocks section commences with a byte containing an underscore character '_',
// Then 7 more bytes containing '85A_|.x' , where x is the version build letter  (ASCII single-byte characters).
// Then a 16-byte starter record containing the version info and 12 bytes of zeroes (spares):

//        Tblock_start=record
//                       version_number:integer; // the Templot0 version number.
//                       zero1:integer;          // 12 spares (zero)...
//                       zero2:integer;
//                       zero3:integer;
//                     end;

// Each DATA BLOCK comprises:

// 16 byte Tblock_ident comprising...

// 4 bytes = length of data segment x.
// 4 bytes = template index count.
// 4 bytes = code indicating content of block.
// 4 bytes = spare - set to zero.

// then x bytes = data segment.

// DATA BLOCKS are repeated until the END BLOCK, which comprises

// 16 byte Tblock_ident comprising all zeroes (segment length=0).

type

  TBox3PointEx = record
    x: double;
    y: double;
  end;

  TBox3Notch = record      //  a notch position.
    notch_x: double;
    notch_y: double;
    notch_k: double;
  end;


  TBox3SnapPegPositions = record
    // snapping positions for F7 shift mouse action  0.79.a  27-05-06
    // and background popup snap options.

    ctrl_peg_now_pos: TBox3Notch;
    ctrl_0_pos: TBox3Notch;
    ctrl_1_pos: TBox3Notch;

    ctrl_2_pos: TBox3Notch;  // added 205c

    ctrl_planing_pos: TBox3Notch;
    // added 205e for obtain turnout radius to control
    ctrl_heel_pos: TBox3Notch;
    // added 205e for obtain turnout radius to control

    ctrl_3_pos: TBox3Notch;

    ctrl_cesp_pos: TBox3Notch;
    // added 205e for obtain turnout radius to control

    ctrl_4_pos: TBox3Notch;
    ctrl_5_pos: TBox3Notch;
    ctrl_6_pos: TBox3Notch;
    ctrl_7_pos: TBox3Notch;
    ctrl_8_pos: TBox3Notch;
    ctrl_9_pos: TBox3Notch;
    ctrl_tcp_pos: TBox3Notch;   // TCP
    ctrl_mcp_pos: TBox3Notch;   // MCP
    ctrl_tolp_pos: TBox3Notch;  // TOLP

    ctrl_tminp_pos: TBox3Notch;     // TMINP     // 213b
    ctrl_texitp_pos: TBox3Notch;    // TEXITP    // 213b

    ctrl_mminp_pos: TBox3Notch;     // MMINP     // 217a
    ctrl_mexitp_pos: TBox3Notch;    // MEXITP    // 217a

    ctrl_tsmidp_pos: TBox3Notch;    // TS curve mid-point  218a

    ctrl_knucklebend_pos: TBox3Notch;  // start of knuckle bend  218a

    ctrl_atimb_pos: TBox3Notch;     // "A" timber 218a

    ctrl_mid_pos: TBox3Notch;      // mid-length  216a

    ctrl_user_pos: TBox3Notch;  // user-defined peg pos    added 205c

  end;//record

  TBox3BoundaryInfo = record            // 213b  for extend to boundary function

    loc_0: TBox3Notch;     // CTRL-0
    loc_6: TBox3Notch;     // CTRL-6
    loc_9: TBox3Notch;     // CTRL-9
    loc_240: TBox3Notch;   // TMINP
    loc_241: TBox3Notch;   // TEXITP
    loc_260: TBox3Notch;   // MMINP     // 217a
    loc_261: TBox3Notch;   // MEXITP    // 217a
    loc_600: TBox3Notch;   // TOLP

    boundary_diag: double; // diagonal length between boundaries
  end;


  TBox3ProtoInfo = record              // was Tgauge_info.

    name_str_pi: string[15];       // gauge designation: 9 chars max actually used

    spare_str_pi: string[75];      // now spares 215a   was  list_str_pi

    scale_pi: double;       // mm per ft.
    gauge_pi: double;       // mm.
    fw_pi: double;       // mm flangeway.
    fwe_pi: double;       // mm flangeway end (flangeway+flare).
    xing_fl_pi: double;       // mm length of flares (not h-d).
    railtop_pi: double;       // mm width of rail top (and bottom if bullhead).
    trtscent_pi: double;       // mm track centres, turnout side.
    trmscent_pi: double;       // mm ditto, main side.
    retcent_pi: double;       // mm ditto, return curve.
    min_radius_pi: double;       // mm minimum radius for check.


    // these 6 wing/check rail lengths used only in pre 0.71.a versions...

    old_winglongs_pi: double;
    // inches full-size length of short wing rail from centre of timber A.
    old_winglongl_pi: double;
    // inches full-size length of long wing rail from centre of timber A.

    old_cklongs_pi: double;
    // inches full-size length of short check rails.
    old_cklongm_pi: double;
    // inches full-size length of medium check rails.
    old_cklongl_pi: double;
    // inches full-size length of long check rails.
    old_cklongxl_pi: double;
    // inches full_size length of extra long check rails.

    tbwide_pi: double;       // inches full-size width of turnout timbers.
    slwide_pi: double;
    // inches full-size width of plain sleepers (not at rail joints 212a).

    xtimbsp_pi: double;
    // !!! disused in 0.75.a 14-10-01. inches full-size timber-spacing at crossing.
    // retained in files when loaded by versions prior to 0.75.a

    ftimbspmax_pi: double;
    // inches full-size max timber-spacing for closure space.

    tb_pi: double;       // plain sleeper length mm.

    // added in version 0.71.a 11-5-01...

    // !!! 11-5-01 - v:0.71.a
    // !!! exhaustive testing done to get file match with previous version.
    // !!! Due to Delphi aligning boundaries. Don't change anything!!! ...

    mainside_ends_pi: boolean;    //  True=main side ends in line,
    //  False=ends centralized.


    jt_slwide_pi: single;
    // !!! single. inches full-size width of plain sleepers at rail joints. // 212a


    alignment_byte_1: byte;   // D5 0.81 12-06-05

    random_end_pi: double;    //  amount of timber-end randomising.
    timber_thick_pi: double;    //  timber thickness (for DXF 3D).
    random_angle_pi: double;    //  amount of timber_angle randomising.

    // new check and wing dimensioning : v:0.71.a 24-5-01...

    ck_ms_working1_pi: double;
    // full-size inches - size 1 MS check rail working length (back from "A").
    ck_ms_working2_pi: double;
    // full-size inches - size 2 MS check rail working length (back from "A").
    ck_ms_working3_pi: double;
    // full-size inches - size 3 MS check rail working length (back from "A").

    ck_ts_working_mod_pi: double;
    // full-size inches - TS check rail working length modifier.
    // out of use 0.94.a but loaded in old files.

    ck_ms_ext1_pi: double;
    // full-size inches - size 1 MS check rail extension length (forward from "A").
    ck_ms_ext2_pi: double;
    // full-size inches - size 2 MS check rail extension length (forward from "A").

    ck_ts_ext_mod_pi: double;
    // full-size inches - TS check rail extension length modifier.
    // out of use 0.94.a but loaded in old files.

    wing_ms_reach1_pi: double;
    // full-size inches - size 1 MS wing rail reach length (forward from "A").
    wing_ms_reach2_pi: double;
    // full-size inches - size 2 MS wing rail reach length (forward from "A").

    wing_ts_reach_mod_pi: double;
    // full-size inches - TS wing rail reach length modifier.
    // out of use 0.94.a but loaded in old files.

    // new rail section dims 0.71.a...

    railbottom_pi: double;
    // mm width of railfoot (FB).                                   // spare_float4:double;

    // these are for 3-D in DXF...

    rail_height_pi: double;
    // full-size inches rail height (for 3D in DXF).
    seat_thick_pi: double;
    // full-size inches chair seating thickness (for 3D in DXF).

    old_tb_pi: double;
    // inches full-size (unlike tb_pi which is mm). used internally for gauge changes (no meaning in file).

    rail_inclination_pi: double;    // radians.
    foot_height_pi: double;    // inches full-size  edge thickness.
    chair_outlen_pi: double;    // inches full-size  from rail gauge-face
    chair_inlen_pi: double;    // inches full-size
    chair_width_pi: double;    // inches full-size
    chair_corner_pi: double;    // inches full-size  corner rad.

    spare_byte1: byte;   //  !!! don't replace these an integer !!!
    spare_byte2: byte;
    //  !!! Delphi will upset the align boundaries for proto_info within template_info. !!!
    spare_byte3: byte;
    spare_byte4: byte;
    spare_byte5: byte;

    alignment_byte_2: byte;   // D5 0.81 12-06-05

  end;


  //-----------------------

  TBox3TransformInfo = record             //  datums, shifts and rotations ...
    //  (yes I know the plural of datum is data !)

    datum_y: double;  // y_datum, y datum point (green dot).

    x_go_limit: double;  // (nyi) print cropping limits (paper inches)...
    x_stop_limit: double;

    transforms_apply: boolean; // !!! no longer used.  // False = ignore transform data.

    alignment_byte_1: byte;   // D5 0.81 12-06-05

    x1_shift: double;  //  mm    shift info...
    y1_shift: double;  //  mm
    k_shift: double;  //  radians.
    x2_shift: double;  //  mm
    y2_shift: double;  //  mm

    peg_pos: TBox3PointEx;      //  mm  peg position.

    alignment_byte_2: byte;   // D5 0.81 12-06-05
    alignment_byte_3: byte;   // D5 0.81 12-06-05

    peg_point_code: integer;   //  peg_code.
    peg_point_rail: integer;   //  peg_rail.

    mirror_on_x: boolean;   //  True= invert on x.
    mirror_on_y: boolean;   //  True= invert on y. (swap hand).

    alignment_byte_4: byte;   // D5 0.81 12-06-05
    alignment_byte_5: byte;   // D5 0.81 12-06-05

    spare_int1: integer;
    spare_int2: integer;

    spare_flag1: boolean;
    spare_flag2: boolean;
    spare_flag3: boolean;
    spare_flag4: boolean;

    notch_info: TBox3Notch;      {spare_float1:double;}    // 11-4-00 version 0.53
    {spare_float2:double;}
    {spare_float3:double;}

    spare_str: string[10];

    alignment_byte_6: byte;   // D5 0.81 12-06-05
    alignment_byte_7: byte;   // D5 0.81 12-06-05
    alignment_byte_8: byte;   // D5 0.81 12-06-05

  end;//record

  TBox3PlatformTrackbedInfo = record   // 0.93.a was  Tcheck_rail_mints=record

    adjacent_edges_keep: boolean;
    // False=adjacent tracks,  True=trackbed edges and platform edges.

    draw_ms_trackbed_edge_keep: boolean;
    draw_ts_trackbed_edge_keep: boolean;

    spare_bool1: boolean;

    OUT_OF_USE_trackbed_width_ins_keep: double;
    // 180 inches full-size 15ft.  // not used 215a  TS and MS separated, see below

    draw_ts_platform_keep: boolean;
    draw_ts_platform_start_edge_keep: boolean;
    draw_ts_platform_end_edge_keep: boolean;
    draw_ts_platform_rear_edge_keep: boolean;

    platform_ts_front_edge_ins_keep: double;
    // centre-line to platform front edge 57 inches   4ft-9in  215a
    platform_ts_start_width_ins_keep: double;
    platform_ts_end_width_ins_keep: double;

    platform_ts_start_mm_keep: double;
    platform_ts_length_mm_keep: double;


    draw_ms_platform_keep: boolean;
    draw_ms_platform_start_edge_keep: boolean;
    draw_ms_platform_end_edge_keep: boolean;
    draw_ms_platform_rear_edge_keep: boolean;

    platform_ms_front_edge_ins_keep: double;
    // centre-line to platform front edge 57 inches   4ft-9in  215a
    platform_ms_start_width_ins_keep: double;
    platform_ms_end_width_ins_keep: double;

    platform_ms_start_mm_keep: double;
    platform_ms_length_mm_keep: double;

    OUT_OF_USE_cess_width_ins_keep: double;
    // 206a     // not used 215a  TS and MS separated, see below
    OUT_OF_USE_draw_trackbed_cess_edge_keep: boolean;
    // 206a     // not used 215a  TS and MS separated, see below

    // platform skews added 207a...

    platform_ms_start_skew_mm_keep: double;      // 207a
    platform_ms_end_skew_mm_keep: double;        // 207a

    platform_ts_start_skew_mm_keep: double;      // 207a
    platform_ts_end_skew_mm_keep: double;        // 207a


    spare_bool2: boolean;
    spare_bool3: boolean;
    spare_bool4: boolean;
    spare_bool5: boolean;
    spare_bool6: boolean;
    spare_bool7: boolean;
    spare_bool8: boolean;


    // new trackbed edge functions 215a ...   split MS and TS settings  -  using Single floats to fit available file space ...

    trackbed_ms_width_ins_keep: Single;
    trackbed_ts_width_ins_keep: Single;

    cess_ms_width_ins_keep: Single;
    cess_ts_width_ins_keep: Single;

    draw_ms_trackbed_cess_edge_keep: boolean;
    draw_ts_trackbed_cess_edge_keep: boolean;

    spare1: boolean;
    spare2: boolean;
    // 215a spare_extended1:double; spare_extended2:double;

    trackbed_ms_start_mm_keep: double;
    // 215a spare_extended3:double;    // need to be extendeds for def_req
    trackbed_ms_length_mm_keep: double;   // 215a spare_extended4:double;

    trackbed_ts_start_mm_keep: double;    // 215a spare_extended5:double;
    trackbed_ts_length_mm_keep: double;   // 215a spare_extended6:double;

  end;


  TBox3AlignmentInfo = record              //  curving and transition info...

    curving_flag: boolean;
    // !!! no longer used 0.77.a !!! True=curved, False=straight.
    // but needed for check on loading older files.
    // - all templates now curved (straight=max_rad).

    trans_flag: boolean;    // True=transition, False=fixed radius curving.

    fixed_rad: double;   // fixed radius mm.
    trans_rad1: double;   // first transition radius mm.
    trans_rad2: double;   // second transition radius mm.
    trans_length: double;   // length of transition mm.
    trans_start: double;   // start of transition mm.
    rad_offset: double;   // curving line offset mm. no longer used

    alignment_byte_1: byte;   // D5 0.81 12-06-05
    alignment_byte_2: byte;   // D5 0.81 12-06-05

    tanh_kmax: double;          {spare_int1:integer;}   // factor for mode 2 slews.
    {spare_int2:integer;}
    // !!! double used because only 8 bytes available in existing file format (2 integers).

    slewing_flag: boolean;   {spare_flag1:boolean;}  // slewing flag.
    cl_only_flag: boolean;
    {spare_flag2:boolean;}// draw track centre-line only for bgnd

    slew_type: byte;            {spare_flag3:boolean;}
    // !!! byte used because only 1 byte available in existing file format 1-11-99.

    dummy_template_flag: boolean;  // 212a       //spare_flag4:boolean;

    slew_start: double;  {spare_float1:double;}  // slewing zone start mm.
    slew_length: double;  {spare_float2:double;}  // slewing zone length mm.
    slew_amount: double;  {spare_float3:double;}  // amount of slew mm.


    cl_options_code_int: integer;            // 206a
    cl_options_custom_offset_ext: double;  // 206a

    // 216a ...

    reminder_flag: boolean;
    reminder_colour: integer;

    reminder_str: string[200];


    spare_float1: double;
    spare_float2: double;
    spare_float3: double;

    spare_int: integer;

  end;//record


  TBox3RailInfo = record     // rail switch settings.  23-5-01.

    // !!! 17-1-00 - exhaustive testing done to get file match with previous version.
    // !!! with both same file size and correct reading of bgnd_flag.
    // !!! Due to Delphi2 aligning boundaries. Don't change anything!!! ...

    flared_ends_ri: integer;  // 0=straight bent, 1=straight machined

    // spares..

    spare_int1: integer;

    knuckle_code_ri: integer;
    // 214a spare_int2:integer;     0=normal, -1=sharp, 1=use custom knuckle_radius_ri
    knuckle_radius_ri: double;
    // 214a spare_float1:double;  custom setting - inches full-size

    spare_float2: double;

    spare_bool1: boolean;
    spare_bool2: boolean;

    isolated_crossing_sw: boolean;            //  217a   spare_bool3:boolean;

    // rail switches ..

    k_diagonal_side_check_rail_sw: boolean;    // added 0.93.a
    k_main_side_check_rail_sw: boolean;        // added 0.93.a

    switch_drive_sw: boolean;   // 0.82.a  13-10-06

    // rail switches...

    track_centre_lines_sw: boolean;
    turnout_road_stock_rail_sw: boolean;
    turnout_road_check_rail_sw: boolean;
    turnout_road_crossing_rail_sw: boolean;
    crossing_vee_sw: boolean;
    main_road_crossing_rail_sw: boolean;
    main_road_check_rail_sw: boolean;
    main_road_stock_rail_sw: boolean;

    alignment_byte_1: byte;   // D5 0.81 12-06-05
    alignment_byte_2: byte;   // D5 0.81 12-06-05


  end;


  // plain-track record includes user-defined peg data...

  TBox3PlainTrackInfo = record

    pt_custom: boolean;        // custom plain track flag.

    alignment_byte_1: byte;   // D5 0.81 12-06-05
    alignment_byte_2: byte;   // D5 0.81 12-06-05
    alignment_byte_3: byte;   // D5 0.81 12-06-05

    list_index: integer;
    rail_length: double;         // rail length in inches.

    alignment_byte_4: byte;   // D5 0.81 12-06-05
    alignment_byte_5: byte;   // D5 0.81 12-06-05

    sleepers_per_length: integer;                     // number of sleepers per length.
    sleeper_centres: array[0..psleep_c] of double;  // spacings in inches for custom.

    rail_joints_code: integer;   // 0=normal, 1=staggered, -1=none (cwr).

    user_peg_rail: integer;   // was pt_spare_int2:integer; 13-3-01.

    pt_spare_flag1: boolean;
    pt_spare_flag2: boolean;
    pt_spare_flag3: boolean;

    user_peg_data_valid: boolean;    // was pt_spare_flag4:boolean;  13-3-01

    user_pegx: double;          // was pt_spare_float1:double;  13-3-01
    user_pegy: double;          // was pt_spare_float2:double;  13-3-01
    user_pegk: double;          // was pt_spare_float3:double;  13-3-01

    pt_spacing_name_str: string[200];     // was spare_str:string[250];   17-1-01.

    alignment_byte_6: byte;   // D5 0.81 12-06-05

    pt_tb_rolling_percent: double;      // 0.76.a  17-5-02.

    gaunt_sleeper_mod_inches: double;       // 0.93.a ex 0.81 pt_spare_ext4:double;

    pt_spare_ext3: double;
    pt_spare_ext2: double;
    pt_spare_ext1: double;

    alignment_byte_7: byte;   // D5 0.81 12-06-05
    alignment_byte_8: byte;   // D5 0.81 12-06-05

  end;//record

  //________________________________________________

  //  these record types apply to turnouts only...

  TBox3SwitchInfo = record      // switch stuff...

    old_size: integer;       // old index into list of switches (pre 0.77.a).
    sw_name_str: string[100];   // name of switch.

    alignment_byte_1: byte;   // D5 0.81 12-06-05
    alignment_byte_2: byte;   // D5 0.81 12-06-05
    alignment_byte_3: byte;   // D5 0.81 12-06-05

    sw_pattern: integer;    // type of switch.
    planing: double;   // (B) planing length (inches).
    planing_angle: double;   // unit planing angle.
    switch_radius_inchormax: double;
    // switch radius (inches!) (or max_rad (in mm) for straight switch).
    switch_rail: double;   // (C) length of switch rail (inches).
    stock_rail: double;   // (S) length of stock rail (inches).
    heel_lead_inches: double;   // (L) lead to heel (incl. planing) (inches).
    heel_offset_inches: double;   // (H) heel-offset (inches).
    switch_front_inches: double;   // stock-rail-end to toe (inches).
    planing_radius: double;   // planing radius for double-curved switch.
    sleeper_j1: double;
    // first switch-front sleeper spacing back from TOE (NEGATIVE inches).
    sleeper_j2: double;
    // second switch-front sleeper spacing back from the first (NEGATIVE inches).

    timber_centres: array[0..swtimbco_c] of double;
    // list of timber centres (in inches).

    group_code: integer;    //  which group of switches.        0.77.a  7-6-02.
    size_code: integer;    //  size within group (1=shortest). 0.77.a  7-6-02.

    joggle_depth: double;   //  depth of joggle. 0.71.a 13-4-01.
    joggle_length: double;   //  length of joggle in front of toe (+ve). 0.71.a 13-4-01.

    group_count: integer;
    // number of switches in this group (max size_code in this group, min size is always 1).

    joggled_stock_rail: boolean;    //  True = joggled stock rail.

    alignment_byte_4: byte;   // D5 0.81 12-06-05
    alignment_byte_5: byte;   // D5 0.81 12-06-05
    alignment_byte_6: byte;   // D5 0.81 12-06-05

    spare_int2: integer;
    spare_int1: integer;

    valid_data: boolean;    // True = valid data here. 0.77.a 9-6-02...
    front_timbered: boolean;    // True = switch front sleepers are timber width.

    num_bridge_chairs_main_rail: byte;
    // not used in experimental chairing   // 214a              spare_byte
    num_bridge_chairs_turnout_rail: byte;
    // not used in experimental chairing   // 214a              spare_byte

    fb_tip_offset: double;
    // 0.76.a  2-1-02. fbtip dimension (FB foot from gauge-face at tip).

    sleeper_j3: double;
    //  third switch-front sleeper spacing back from the second (NEGATIVE inches).
    sleeper_j4: double;
    //  fourth switch-front sleeper spacing back from the third (NEGATIVE inches).
    sleeper_j5: double;
    //  fifth switch-front sleeper spacing back from the fourth (NEGATIVE inches).

    spare_float4: double;
    spare_float3: double;
    spare_float2: double;
    spare_float1: double;

    spare_str: string[200];

    num_slide_chairs: byte;           // 214a alignment_byte_7:byte;   // D5 0.81 12-06-05
    num_block_slide_chairs: byte;     // 214a alignment_byte_8:byte;   // D5 0.81 12-06-05
    num_block_heel_chairs: byte;      // 214a alignment_byte_9:byte;   // D5 0.81 12-06-05

  end;//record

  TBox3CheckFlareInfo_081 = record
    // not used 0.93.a

    // 0.81 new flare lengths.  04-08-03.

    check_flare_ext_ms: double;
    // flare length (inches), MS check rail extension end.
    check_flare_ext_ts: double;
    // flare length (inches), TS check rail extension end.
    check_flare_work_ms: double;
    // flare length (inches), MS check rail working end.
    check_flare_work_ts: double;
    // flare length (inches), TS check rail working end.
    wing_flare_ms: double;           // flare length (inches), MS wing rail.
    wing_flare_ts: double;           // flare length (inches), TS wing rail.
    check_flare_k_ms: double;        // flare length (inches), MS K-crossing check rail.
    check_flare_k_ds: double;        // flare length (inches), DS K-crossing check rail.

    // 0.81 new flare offsets (flangeway end gap).  04-08-03.

    check_fwe_ext_ms: double;
    // flangeway end gap (mm), MS check rail extension end.
    check_fwe_ext_ts: double;
    // flangeway end gap (mm), TS check rail extension end.
    check_fwe_work_ms: double;     // flangeway end gap (mm), MS check rail working end.
    check_fwe_work_ts: double;     // flangeway end gap (mm), TS check rail working end.
    wing_fwe_ms: double;           // flangeway end gap (mm), MS wing rail.
    wing_fwe_ts: double;           // flangeway end gap (mm), TS wing rail.
    check_fwe_k_ms: double;        // flangeway end gap (mm), MS K-crossing check rail.
    check_fwe_k_ds: double;        // flangeway end gap (mm), DS K-crossing check rail.

  end;//record

  TBox3CrossingInfo = record        // crossing stuff...

    pattern: integer;     // 0=straight, 1=curviform, 2=parallel, -1=generic.

    sl_mode: integer;     // 0=auto_fit, 1=use fixed_sl.
    retcent_mode: integer;
    // 0=return centres as adjacent track, 1=use custom centres.
    k3n_unit_angle: double;    // k3n angle in units.
    fixed_st: double;    // length of knuckle straight. mm.

    spare_int3: integer;

    hd_timbers_code: integer;     // extended half-diamond timbers for slip road.
    hd_vchecks_code: integer;
    // shortening code for half-diamond v-crossing check rails.

    k_check_length_1: double;    // length of size 1 k-crossing check rail (inches).
    k_check_length_2: double;    // length of size 2 k-crossing check rail (inches).
    k_check_mod_ms: double;    // main side modifer.
    k_check_mod_ds: double;    // diamond side modifer.
    k_check_flare: double;    // length of flare on k-crossing check rails.

    curviform_timbering_keep: boolean;
    // 215a                           alignment_byte_1:byte;   // D5 0.81 12-06-05

    alignment_byte_2: byte;   // D5 0.81 12-06-05

    main_road_code: integer;
    //  length of main-side exit road.      //  217a  spare_int2:integer;

    tandem_timber_code: integer;   //   218a      spare_int1:        integer;

    // 0.75.a  9-10-01...

    blunt_nose_width: double;    // full-size inches.
    blunt_nose_to_timb: double;    // full-size inches - to "A" timber centre.

    vee_joint_half_spacing: double;
    // full-size inches - rail overlap at vee point rail joint.
    wing_joint_spacing: double;
    // full-size inches - timber spacing at wing rail joint.

    wing_timber_spacing: double;
    // full-size inches - timber spacing for wing rail front part of crossing (up to "A").
    vee_timber_spacing: double;
    // full-size inches - timber spacing for vee point rail part of crossing (on from "A").

    // number of timbers spanned by vee rail incl. "A" timber.

    vee_joint_space_co1: byte;
    vee_joint_space_co2: byte;
    vee_joint_space_co3: byte;
    vee_joint_space_co4: byte;
    vee_joint_space_co5: byte;
    vee_joint_space_co6: byte;

    // number of timbers spanned by wing rail front excl. "A" timber...

    wing_joint_space_co1: byte;
    wing_joint_space_co2: byte;
    wing_joint_space_co3: byte;
    wing_joint_space_co4: byte;
    wing_joint_space_co5: byte;
    wing_joint_space_co6: byte;

    spare_flag1: boolean;
    spare_flag2: boolean;

    main_road_endx_infile: double;  // 217a

    hdkn_unit_angle: double;    // half-diamond hdkn angle in units.

    check_flare_info_081: TBox3CheckFlareInfo_081;   // not used 0.93.a

    k_custom_wing_long_keep: double;   // 0.95.a inches full-size k-crossing wing rails
    k_custom_point_long_keep: double;
    // 0.95.a inches full-size k-crossing point rails   NYI

    use_k_custom_wing_rails_keep: boolean;   // 0.95.a
    use_k_custom_point_rails_keep: boolean;  // 0.95.a  NYI

    spare_str: string[10];    // 0.95.a was 30

    alignment_byte_3: byte;   // D5 0.81 12-06-05

  end;//record

  TBox3TurnoutInfo1 = record          // data for the turnout size...

    plain_track_flag: boolean;      //  True=plain track only.

    rolled_in_sleepered_flag: boolean;
    // 223a  alignment_byte_1:byte;   // D5 0.81 12-06-05

    front_timbers_flag: boolean;
    //  218a    alignment_byte_2:byte;   // D5 0.81 12-06-05

    approach_rails_only_flag: boolean;
    //  218a    alignment_byte_3:byte;   // D5 0.81 12-06-05

    hand: integer;      //  hand of turnout.
    timbering_flag: boolean;      //  True = equalized timbering.

    switch_timbers_flag: boolean;
    //  218a    alignment_byte_4:byte;   // D5 0.81 12-06-05
    closure_timbers_flag: boolean;
    //  218a    alignment_byte_5:byte;   // D5 0.81 12-06-05
    xing_timbers_flag: boolean;
    //  218a    alignment_byte_6:byte;   // D5 0.81 12-06-05

    exit_timbering: integer;      //  exit timbering style.
    turnout_road_code: integer;      //  length of turnout exit road.

    turnout_length: double;     //  turnoutx.
    origin_to_toe: double;     //  xorg.
    step_size: double;
    //  incx. (use saved step-size on reloading - not default).

    turnout_road_is_adjustable: boolean;
    // 211a    alignment_byte_7:byte;   // D5 0.81 12-06-05

    turnout_road_is_minimum: boolean;
    // 217a    alignment_byte_8:byte;   // D5 0.81 12-06-05

  end;//tturnout_info1 record

  TBox3HdkCheckRailInfo = record         // K-crossing check and wing rail lengths. 0.79.a

    k_check_ms_1: double;
    // full-size inches - size 1 MS k-crossing check rail length.
    k_check_ms_2: double;
    // full-size inches - size 2 MS k-crossing check rail length.

    k_check_ds_1: double;
    // full-size inches - size 1 DS k-crossing check rail length.
    k_check_ds_2: double;
    // full-size inches - size 2 DS k-crossing check rail length.
  end;

  TBox3VeeCheckRailInfo = record         // V-crossing check and wing rail lengths. 0.79.a

    v_check_ms_working1: double;
    // full-size inches - size 1 MS check rail working length (back from "A").
    v_check_ms_working2: double;
    // full-size inches - size 2 MS check rail working length (back from "A").
    v_check_ms_working3: double;
    // full-size inches - size 3 MS check rail working length (back from "A").

    v_check_ts_working1: double;
    // full-size inches - size 1 TS check rail working length (back from "A").
    v_check_ts_working2: double;
    // full-size inches - size 2 TS check rail working length (back from "A").
    v_check_ts_working3: double;
    // full-size inches - size 3 TS check rail working length (back from "A").

    v_check_ms_ext1: double;
    // full-size inches - size 1 MS check rail extension length (forward from "A").
    v_check_ms_ext2: double;
    // full-size inches - size 2 MS check rail extension length (forward from "A").

    v_check_ts_ext1: double;
    // full-size inches - size 1 TS check rail extension length (forward from "A").
    v_check_ts_ext2: double;
    // full-size inches - size 2 TS check rail extension length (forward from "A").

    v_wing_ms_reach1: double;
    // full-size inches - size 1 MS wing rail reach length (forward from "A").
    v_wing_ms_reach2: double;
    // full-size inches - size 2 MS wing rail reach length (forward from "A").

    v_wing_ts_reach1: double;
    // full-size inches - size 1 TS wing rail reach length (forward from "A").
    v_wing_ts_reach2: double;
    // full-size inches - size 2 TS wing rail reach length (forward from "A").
  end;

  TBox3CheckEndDiff = record    // 0.94.a
    len_diff: double;   // length differ  inches f-s
    flr_diff: double;   // flare length   inches f-s
    gap_diff: double;   // end gap        model mm

    type_diff: byte;
    // 0=no diff   1=change to bent flare    2=change to machined flare   3= change to no flare
  end;

  TBox3CheckDiffs = record    // 0.94.a
    end_diff_mw: TBox3CheckEndDiff;
    end_diff_me: TBox3CheckEndDiff;
    end_diff_mr: TBox3CheckEndDiff;
    end_diff_tw: TBox3CheckEndDiff;
    end_diff_te: TBox3CheckEndDiff;
    end_diff_tr: TBox3CheckEndDiff;
    end_diff_mk: TBox3CheckEndDiff;
    end_diff_dk: TBox3CheckEndDiff;
  end;

  TBox3TurnoutInfo2 = record
    switch_info: TBox3SwitchInfo;      //  all the switch dimensions.
    crossing_info: TBox3CrossingInfo;    //  all the crossing dimensions.
    plain_track_info: TBox3PlainTrackInfo;
    //  need the plain track info for approach and exit tracks.

    diamond_auto_code: integer;
    // 0.77.a 0=auto, 1=fixed diamond, 2=switch diamond.

    bonus_timber_count: integer;     // 0.76.a number of bonus timbers.

    equalizing_fixed_flag: boolean;
    {spare_flag1:boolean;}// equalizing style 1-4-00
    no_timbering_flag: boolean;
    {spare_flag2:boolean;}// no timbering option 7-9-00

    angled_on_flag: boolean;
    {spare_flag3:boolean;}// angled-on style 29-7-01.

    chairing_flag: boolean;          // 214a    //spare_flag2:boolean;

    start_draw_x: double;          {spare_float3:double;}   // startx.

    timber_length_inc: double;     // timbinc timber length step size.

    //------
    omit_switch_front_joints: boolean;  // 0.79.a spare_float1:double;...
    omit_switch_rail_joints: boolean;
    omit_stock_rail_joints: boolean;
    omit_wing_rail_joints: boolean;
    omit_vee_rail_joints: boolean;
    omit_k_crossing_stock_rail_joints: boolean;

    spare_flag14: boolean;
    spare_flag13: boolean;
    spare_flag12: boolean;

    diamond_switch_timbering_flag: boolean;  // 213a spare_flag11:boolean;

    //------


    gaunt_flag: boolean;    // True = gaunt template 0.81.a   //spare_flag10:boolean;

    diamond_proto_timbering_flag: boolean;    // 0.77.b

    semi_diamond_flag: boolean;      // True = half-diamond template.
    diamond_fixed_flag: boolean;     // True = fixed-diamond.


    hdk_check_rail_info: TBox3HdkCheckRailInfo;

    vee_check_rail_info: TBox3VeeCheckRailInfo;

    turnout_road_endx_infile: double;
    // 209a length of turnout road from CTRL-1   //spare_float:double;

    // 208c added to aid debugging of box files in text editor (never read):

    template_type_str: string[6];
    // 208c was spare_str[16]        208a was spare_str:string[56]

    smallest_radius_stored: double;
    // 208a needed for box data -- not loaded to the control

    dpx_stored: double;
    // 208a needed for ID number creation -- not loaded to the control
    ipx_stored: double;
    // 208a needed for ID number creation -- not loaded to the control
    fpx_stored: double;
    // 208a needed for ID number creation -- not loaded to the control


    gaunt_offset_inches: double;  // 0.81

    // 219a  include connectors for XTrackCAD in export DXF file  -- not loaded to the control  ...

    dxf_connector_0: boolean;
    // CTRL-0   // alignment_byte_1:byte;   // D5 0.81 12-06-05
    dxf_connector_t: boolean;
    // TEXITP   // alignment_byte_2:byte;   // D5 0.81 12-06-05
    dxf_connector_9: boolean;
    // CTRL-9   // alignment_byte_3:byte;   // D5 0.81 12-06-05

  end;//Tturnout_info2 record


  TBox3Dims1 = record

    box_ident: string[10];   // first 11 bytes. in BOX3,   (string[11], 12 bytes in BOX)

    id_byte: byte;          // set to 255  $FF in BOX3 files - not read.

    now_time: integer;
    // date/time/random code at which template added to keep box. (from Delphi float format - fractional days since 1-1-1900).
    // this is used to detect duplicates on loading.

    keep_date: string[20];   // ditto as conventional strings.
    keep_time: string[20];

    top_label: string[100];  // template info label.
    project_for: string[50];
    // his project title string for the boxful. (only read from the last template in the box).

    reference_string: string[100];  // template name.

    this_was_control_template: boolean;
    // 0.93.a // alignment_byte_2:byte;   // D5 0.81 12-06-05

    rail_info: Trail_info;  // 23-5-01.

    auto_restore_on_startup: boolean;
    // these two only read from the first keep in the file..
    ask_restore_on_startup: boolean;

    //---------------------

    pre077_bgnd_flag: boolean;
    // no longer used, 0.77.a 2-sep-02. When true, this keep is to be drawn on the background.

    alignment_byte_3: byte;   // D5 0.81 12-06-05

    templot_version: integer;
    // program version number (*100, e.g Templot0 v:1.3 = 130).

    file_format_code: integer;  // 0= D5 format,    1= OT format  //spare_int1

    gauge_index: integer;      // current index into the gauge list.

    gauge_exact: Boolean;      // nyi  // If true this is an exact-scale template.
    gauge_custom: Boolean;
    // nyi  // If true this is (or was when saved) a custom gauge setting.

    proto_info: Tproto_info;
    // !!! modified for 0.71.a 11-5-01. was Tgauge_info.

    railtop_inches: double;
    // full-size inches railtop width - was spare_float1:double;
    railbottom_inches: double;
    // full-size inches railbottom width - was spare_float2:double;

    alignment_byte_4: byte;   // D5 0.81 12-06-05
    alignment_byte_5: byte;   // D5 0.81 12-06-05

    version_as_loaded: integer;
    // mod 0.78.d  14-Feb-2003. the version number as loaded.

    bgnd_code_077: integer;          // 0=unused, 1=bgnd, -1=library   0.77.a  2-Sep-02.

    print_mapping_colour: integer;   // 0.76.a  27-10-01 //spare_inta:integer;
    pad_marker_colour: integer;      // 0.76.a  27-10-01 //spare_intb:integer;

    use_print_mapping_colour: boolean;  //spare_boola:boolean;
    use_pad_marker_colour: boolean;     //spare_boolb:boolean;

    //-------------------------

    //  0.79.a 20-05-06  -- saved grid info -- read from last template only...

    spare_bool1: boolean;

    spare_bool2: boolean;  // out 0.93.a   was show_page_margins_on_pad:boolean;

    spare_int2: integer;

    grid_units_code: integer;

    x_grid_spacing: double;
    y_grid_spacing: double;

    total_length_of_timbering: double;  // 0.96.a


    id_number: integer;         // 208a
    id_number_str: string[7];   // 208a     -N00000

    spare_boolean1: boolean;    // 208a
    spare_boolean2: boolean;    // 208a      //spare_str:string[13];


    transform_info: TBox3TransformInfo;

    platform_trackbed_info: TBox3PlatformTrackbedInfo;
    // 0.93.a  was check_rail_mints:Tcheck_rail_mints;

    align_info: TBox3AlignmentInfo;


    rail_type: integer;
    // 0=no rails, 1=head only (bullhead), 2=head+foot (flatbottom).   // spare_int1:integer

    fb_kludge_template_code: integer;
    // 0.94.a   0=normal template, 1=inner foot lines, 2=outer foot lines   //spare_int3:integer;

    box_save_done: boolean;
    // read only from first keep on restore previous contents. 23-6-00 v:0.62.a      //spare_flag1:boolean;

    uninclined_rails: boolean;      // True = rails vertical.

    disable_f7_snap: boolean;       //  0.82.a  spare_bool3:boolean;

    spare_bool4: boolean;

    mod_text_x: double;
    // (mm) label position modifiers..   //spare_float1:double;
    mod_text_y: double;
    //spare_float2:double;

    flatbottom_width: double;
    // width of flatbottom rail base (mm).    //spare_float3:double;

    check_diffs: TBox3CheckDiffs;      // 0.94.a check rail end modifiers - 248 bytes


    retain_diffs_on_make_flag: boolean;    // 0.94.a check rail diffs
    retain_diffs_on_mint_flag: boolean;    // 0.94.a check rail diffs

    retain_entry_straight_on_make_flag: boolean;
    // 213a  spare_byte1:byte;   // 0.94.a
    retain_entry_straight_on_mint_flag: boolean;
    // 213a  spare_byte2:byte;   // 0.94.a

    // 0.94.a timber shoving mods..

    retain_shoves_on_make_flag: boolean;
    retain_shoves_on_mint_flag: boolean;

    turnout_info1: TBox3TurnoutInfo1;

  end;//record


  TBox3KeepDims = record
    box_dims1: TBox3Dims1;
    turnout_info2: TBox3TurnoutInfo2;
  end;

  TBox3ShoveForFile = record    // Used in the SHOVE DATA BLOCKS in the 071 files.
    // But not used within the program - see Ttimber_shove.shove_data instead.
    // Conversion takes place in 071 on loading.

    sf_str: string[6];           // timber number string.
    alignment_byte_1: byte;   // D5 0.81 12-06-05
    sf_shove_data: Tshove_data;  // all the data.

    procedure CopyFrom(src: Tshoved_timber);
    procedure CopyTo(dest: Tshoved_timber);
  end;//record


  TBox3Template = class
    Name: string;
    memo: string;
    keepDims: TBox3KeepDims;
    shovedTimbers: array of TBox3ShoveForFile;
  end;

  TBox3TemplateList = class(TObjectList<TBox3Template>)
  end;


  // start record for trailing data blocks...
  TBox3BlockStart = record
    versionNumber: integer; // the Templot0 version number.
    zero1: integer;          // 12 spares (zero)...
    zero2: integer;
    zero3: integer;
  end;

  TBox3BlockIdent = record
    segmentLength: integer;
    templateIndex: integer;
    blockCode: integer;   // 10 = timber shove data.
    spareZeroes: integer;
  end;

procedure TBox3ShoveForFile.CopyFrom(src: Tshoved_timber);
begin
  sf_str := src.timber_string;
  sf_shove_data.sv_code := src.sv_code;
  sf_shove_data.sv_x := src.sv_x;
  sf_shove_data.sv_k := src.sv_k;
  sf_shove_data.sv_o := src.sv_o;
  sf_shove_data.sv_l := src.sv_l;
  sf_shove_data.sv_w := src.sv_w;
  sf_shove_data.sv_c := src.sv_c;
  sf_shove_data.sv_t := src.sv_t;
  sf_shove_data.sv_sp_int := src.sv_sp_int;
end;

procedure TBox3ShoveForFile.CopyTo(dest: Tshoved_timber);
begin
  dest.timber_string := sf_str;
  dest.sv_code := sf_shove_data.sv_code;
  dest.sv_x := sf_shove_data.sv_x;
  dest.sv_k := sf_shove_data.sv_k;
  dest.sv_o := sf_shove_data.sv_o;
  dest.sv_l := sf_shove_data.sv_l;
  dest.sv_w := sf_shove_data.sv_w;
  dest.sv_c := sf_shove_data.sv_c;
  dest.sv_t := sf_shove_data.sv_t;
  dest.sv_sp_int := sf_shove_data.sv_sp_int;
end;

procedure ConvertShovedTimberToBox3(shoveList: Tshoved_timber_list; box3Template: TBox3Template);
var
  i: integer;
begin
  SetLength(box3Template.shovedTimbers, shoveList.Count);
  for i := 0 to shoveList.Count - 1 do begin
    box3Template.shovedTimbers[i].CopyFrom(shoveList[i]);
  end;
end;

function ConvertTemplateToBox3(template: TTemplate): TBox3Template;
begin
  Result := TBox3Template.Create;
  try
    Assert(sizeof(template.template_info.keep_dims) = sizeof(Result.keepDims));
    Result.Name := template.Name;
    Result.memo := template.memo;
    Move(template.template_info.keep_dims, Result.keepDims, sizeof(Result.keepDims));

    ConvertShovedTimberToBox3(template.template_info.keep_shove_list, Result);
  except
    Result.Free;
    raise;
  end;
end;

function ConvertTemplatesToBox3(templates: TTemplateList): TBox3TemplateList;
var
  t: TTemplate;
  bt: TBox3Template;
begin
  Result := TBox3TemplateList.Create;
  try
    for t in templates do begin
      bt := ConvertTemplateToBox3(t);
      Result.Add(bt);
    end;
  except
    Result.Free;
    raise;
  end;
end;


procedure ConvertBox3ToShovedTimbers(const box3Timbers: array of TBox3ShoveForFile;
  template: TTemplate);
var
  i: integer;
  timbers: Tshoved_timber_list;
  t: Tshoved_timber;
begin
  timbers := Tshoved_timber_list.Create;
  try
    for i := 0 to High(box3Timbers) do begin
      t := Tshoved_timber.Create;
      box3Timbers[i].CopyTo(t);
      timbers.Add(t);
    end;
  except
    timbers.Free;
    raise;
  end;
  template.template_info.keep_shove_list := timbers;
end;

function ConvertBox3ToTemplate(box3Template: TBox3Template): TTemplate;
begin
  Result := TTemplate.Create('');
  try
    Assert(sizeof(Result.template_info.keep_dims) = sizeof(box3Template.keepDims));
    Result.Name := box3Template.Name;
    Result.memo := box3Template.memo;
    Move(box3Template.keepDims, Result.template_info.keep_dims, sizeof(box3Template.keepDims));

    ConvertBox3ToShovedTimbers(box3Template.shovedTimbers, Result);
  except
    Result.Free;
    raise;
  end;
end;

function ConvertBox3ToTemplates(box3Templates: TBox3TemplateList): TTemplateList;
var
  t: TTemplateList;
  b: TBox3Template;
begin
  t := TTemplateList.Create;
  try
    for b in box3Templates do begin
      t.Add(ConvertBox3ToTemplate(b));
    end;
  except
    t.Free;
    raise;
  end;
  Result := t;
end;

procedure FileWriteError;
begin
  raise ExSaveBox.Create('File write error');
end;

procedure WriteTemplateRecords(
  var boxFile: file;
  box3ToSave: TBox3TemplateList;
  saveOption: ESaveOption;
  saveDone: boolean;
  const projectTitle: string;
  const gridInfo: TGridInfo);
var
  i: integer;
  numberWritten: integer;
  template: TBox3Template;
begin
  for i := 0 to box3ToSave.Count - 1 do begin     // first write the template data.
    template := box3ToSave[i];

    template.keepDims.box_dims1.file_format_code := 1;
    // OT format      // put format in file

    if i = (box3ToSave.Count - 1) then
      template.keepDims.box_dims1.box_ident := 'NX' + IntToStr(i)
    // last one in file. (string[10])
    else
      template.keepDims.box_dims1.box_ident := 'N ' + IntToStr(i);

    template.keepDims.box_dims1.id_byte := 255;
    // identify file as BOX3 rather than BOX      290a


    case saveOption of
      eSO_BackupOnExit: begin    // final backup on exit ..
        template.keepDims.box_dims1.auto_restore_on_startup := False;
        // these three only read from the first keep in the file,
        template.keepDims.box_dims1.ask_restore_on_startup := True;
        // but go in every one.
        template.keepDims.box_dims1.box_save_done := saveDone;
      end;

      eSO_Normal: begin    // normal box save (these are never read) ..
        template.keepDims.box_dims1.auto_restore_on_startup := False;
        // not used for normal file save/reload
        template.keepDims.box_dims1.ask_restore_on_startup := False;
        // not used for normal file save/reload
        template.keepDims.box_dims1.box_save_done := False;
      end;

      eSO_RollingBackup: begin    // rolling backup..
        template.keepDims.box_dims1.auto_restore_on_startup := True;
        // if both True on loading = abnormal termination.
        template.keepDims.box_dims1.ask_restore_on_startup := True;
        template.keepDims.box_dims1.box_save_done := False;
      end;

    end;//case

    //  these go in every template but only the first or last in is read back...

    template.keepDims.box_dims1.project_for := Copy(projectTitle, 1, 49);
    // goes in every template but only the last in is read back.

    // 0.79.a  20-05-06  save grid info -- to be read from final template...

    //%%%% 0.91.d -- now also in user preferences, these used only if not prefs.

    template.keepDims.box_dims1.grid_units_code := gridInfo.unitsCode;
    template.keepDims.box_dims1.x_grid_spacing := gridInfo.spaceX;
    template.keepDims.box_dims1.y_grid_spacing := gridInfo.spaceY;

    //--------------------

    BlockWrite(boxFile, template.keepDims, SizeOf(TBox3KeepDims), numberWritten);
    // write all the data.

    if numberWritten <> SizeOf(TBox3KeepDims) then begin
      FileWriteError;
    end;
  end;//next i

end;

procedure WriteStrings(var boxFile: file; box3ToSave: TBox3TemplateList);
var
  i: integer;
  template: TBox3Template;
  numberWritten: integer;
  len: integer;
  s: string;
begin
  for i := 0 to box3ToSave.Count - 1 do begin        // now add the texts.
    template := box3ToSave[i];

    // 0.94.a  fb_kludge templates are created on printing, and destroyed afterwards. Don't save any remaining..

    if template.keepDims.box_dims1.fb_kludge_template_code <> 0 then
      CONTINUE;

    s := remove_esc_str(template.Name) + Char($1B) + remove_esc_str(
      template.Memo) + Char($1B) + Char($1B);
    // use ESC chars as terminators, plus one for luck on the end.

    UniqueString(s);  // make sure it's in continuous memory.

    len := Length(s) * SizeOf(Char);

    BlockWrite(boxFile, len, SizeOf(integer), numberWritten);
    // first the length as an integer (4 bytes)
    if numberWritten <> SizeOf(integer) then begin
      FileWriteError;
    end;

    BlockWrite(boxFile, s[1], len, numberWritten);   // then the text.
    if numberWritten <> len then begin
      FileWriteError;
    end;

  end;//next i
end;

procedure WriteDataBlocks(var boxFile: file; box3ToSave: TBox3TemplateList);
var
  s: string;
  i: integer;
  numberWritten: integer;
  template: TBox3Template;

  blockStart: TBox3BlockStart;
  blockIdent: TBox3BlockIdent;

  shoveCount: integer;
  st: integer;
begin
  // now add the DATA BLOCKS section...

  s := '_85A_|    ';  // start marker.
  BlockWrite(boxFile, s[1], 8, numberWritten);
  // 8 bytes of '_85A_|  ' as a DATA BLOCKS start marker.
  if numberWritten <> 8 then begin
    FileWriteError;
  end;

  with blockStart do begin
    versionNumber := file_version;
    zero1 := 0;
    zero2 := 0;
    zero3 := 0;
  end;//with

  BlockWrite(boxFile, blockStart, SizeOf(blockStart), numberWritten);
  // 16 bytes = version number + 12 bytes of zero (spares).
  if numberWritten <> SizeOf(blockStart) then begin
    FileWriteError;
  end;

  // now the data blocks for each of the loaded templates...
  for i := 0 to box3ToSave.Count - 1 do begin
    template := box3ToSave[i];

    // first block is the shove timber data...

    // shove data = code 10. 4 bytes containing the count of shoved timbers,
    //                       + a series of Tshove_for_file data records for each one.

    shoveCount := Length(template.shovedTimbers);

    blockIdent.segmentLength := SizeOf(integer) + shoveCount * SizeOf(TBox3ShoveForFile);
    blockIdent.templateIndex := i;
    blockIdent.blockCode := 10;         // = timber shove data.
    blockIdent.spareZeroes := 0;

    BlockWrite(boxFile, blockIdent, SizeOf(blockIdent), numberWritten);
    // the data block ident.
    if numberWritten <> SizeOf(blockIdent) then begin
      FileWriteError;
    end;

    // now the shove data segment itself..

    BlockWrite(boxFile, shoveCount, SizeOf(integer), numberWritten);
    // first the count of shoved timbers.
    if numberWritten <> SizeOf(integer) then begin
      FileWriteError;
    end;

    BlockWrite(boxFile, template.shovedTimbers[0], shoveCount * sizeof(TBox3ShoveForFile),
      numberWritten);      // first the count of shoved timbers.
    if numberWritten <> shoveCount * sizeof(TBox3ShoveForFile) then begin
      FileWriteError;
    end;//next st

    // no more DATA BLOCKS yet defined for this template, so on to the next..

  end;//next i

  // all templates done, so add the end zeroes ident (zero-length data segment).

  with blockIdent do begin
    segmentLength := 0;
    templateIndex := 0;
    blockCode := 0;
    spareZeroes := 0;
  end;//with

  BlockWrite(boxFile, blockIdent, SizeOf(blockIdent), numberWritten);
  // finally the end zeroes.
  if numberWritten <> SizeOf(blockIdent) then begin
    FileWriteError;
  end;
end;

procedure SaveBox3(templatesToSave: TTemplateList;
  saveOption: ESaveOption;
  saveDone: boolean;
  const boxFilename: string;
  const projectTitle: string;
  const gridInfo: TGridInfo);
var
  fileSize: integer;
  boxFile: file;               // untyped file.
  box3ToSave: TBox3TemplateList;
begin
  if (boxFilename = '') then begin
    raise Exception.Create('No filename provided');
  end;

  box3ToSave := ConvertTemplatesToBox3(templatesToSave);
  try
    try
      AssignFile(boxFile, boxFilename);
      Rewrite(boxFile, 1);               // open for writing, record size = 1 byte.
      try
        WriteTemplateRecords(boxFile, box3ToSave, saveOption, saveDone, projectTitle, gridInfo);
        WriteStrings(boxFile, box3ToSave);
        WriteDataBlocks(boxFile, box3ToSave);

        fileSize := System.FileSize(boxFile);      // (file must be open to get the size).
        if (not FileExists(boxFilename)) or (fileSize = 0) then begin
          FileWriteError;
        end;

      finally
        CloseFile(boxFile);
      end;

    except
      on EInOutError do begin
        FileWriteError;
      end;
    end;//try-except


  finally
    box3ToSave.Free;
  end;//try
end;


procedure ReadFileError;
begin
  raise ExLoadBox.Create('Error reading file');
end;

procedure ReadFirstTemplateRecord(var boxFile: file; var box3KeepDims: TBox3KeepDims);
var
  numberRead: integer;
begin
  BlockRead(boxFile, box3KeepDims, SizeOf(TBox3KeepDims), numberRead);
  if (numberRead <> Sizeof(TBox3KeepDims)) then begin
    ReadFileError;
  end;
end;

procedure ReadTemplateRecords(var boxFile: file; var box3Templates: TBox3TemplateList);
var
  n: integer;
  template: TBox3Template;
  numberRead: integer;
  s: string;
begin
  repeat
    n := box3Templates.Add(TBox3Template.Create());
    template := box3Templates[n];
    BlockRead(boxFile, template.keepDims, SizeOf(TBox3KeepDims), numberRead);
    s := template.keepDims.box_dims1.box_ident;
    if (numberRead <> SizeOf(TBox3KeepDims)) or
      ((s <> ('N ' + IntToStr(n))) and (s <> ('NX' + IntToStr(n)))) then begin
      // error reading, or this is not a template.
      ReadFileError;
    end;
  until Copy(s, 1, 2) = 'NX';      // last template marker.
end;


procedure ReadStrings(var boxFile: file; var box3Templates: TBox3TemplateList);
var
  n: integer;
  numberRead: integer;
  stringLength: integer;
  s: string;
  i: integer;
  infoString: string;
  memoString: string;
begin
  for n := 0 to box3Templates.Count - 1 do begin
    // now get the proper texts.

    // first get the length as an integer (4 bytes)
    BlockRead(boxFile, stringLength, SizeOf(integer), numberRead);
    if numberRead <> SizeOf(integer) then begin
      ReadFileError;
    end;

    // read len bytes into string s...

    s := StringOfChar('0', stringLength);
    BlockRead(boxFile, s[1], stringLength, numberRead);

    if numberRead <> stringLength then begin
      ReadFileError;
    end;

    i := Pos(Char($1B), s);          // find info part terminator.

    if i <> 0 then begin
      infoString := Copy(s, 1, i - 1); // info string (don't include the ESC).
      Delete(s, 1, i);          // remove info string and terminator from input.

      i := Pos(Char($1B), s);             // find memo part terminator.

      if i <> 0 then begin
        memoString := Copy(s, 1, i - 1); // memo string (don't incude the ESC).

        // we don't change either unless we've got both..
        box3Templates[n].Name := remove_esc_str(infoString);
        // remove any ESC is belt and braces...
        box3Templates[n].Memo := remove_esc_str(memoString);
      end;
    end;
  end;//next n
end;

procedure ReadShoveBlock(var boxFile: file; box3Template: TBox3Template; segmentLength: integer);
var
  numberRead: integer;
  shoveCount: integer;
begin
  // first get the count of shoved timbers for this template...
  BlockRead(boxFile, shoveCount,
    SizeOf(integer), numberRead);
  if (numberRead <> SizeOf(integer)) then begin
    ReadFileError;
  end;

  if segmentLength <> (SizeOf(integer) + shoveCount * SizeOf(TBox3ShoveForFile)) then
    ReadFileError;  // the integer is the shove count just read.

  if shoveCount > 0 then begin
    // now get the data for all the shoved timbers...
    SetLength(box3Template.shovedTimbers, shoveCount);

    BlockRead(boxFile, box3Template.shovedTimbers[0], SizeOf(TBox3ShoveForFile) *
      shoveCount, numberRead);
    if (numberRead <> SizeOf(TBox3ShoveForFile) * shoveCount) then begin
      ReadFileError;
    end;
  end;
end;

procedure ReadDataBlocks(var boxFile: file; var box3Templates: TBox3TemplateList);
var
  s: string;
  numberRead: integer;
  blockStart: TBox3BlockStart;
  blockIdent: TBox3BlockIdent;
begin
  s := StringOfChar(' ', 8);
  BlockRead(boxFile, s[1], 8, numberRead);
  if numberRead <> 8 then begin
    ReadFileError;
  end;

  if Copy(s, 1, 6) <> '_85A_|' then begin
    ReadFileError;
  end;

  BlockRead(boxFile, blockStart, SizeOf(TBox3BlockStart), numberRead);
  if numberRead <> SizeOf(TBox3BlockStart) then begin
    ReadFileError;
  end;

  // get all the data blocks
  while not EOF(boxFile) do begin
    // get the ident for the next data block..

    BlockRead(boxFile, blockIdent, SizeOf(TBox3BlockIdent), numberRead);
    if numberRead <> SizeOf(TBox3BlockIdent) then begin
      ReadFileError;
    end;

    if blockIdent.segmentLength = 0 then
      Exit;    // end of data blocks.

    if (blockIdent.templateIndex < 0) or (blockIdent.templateIndex >= box3Templates.Count) then
    begin
      ReadFileError;
    end;

    case blockIdent.blockCode of

      10:
        ReadShoveBlock(boxFile, box3Templates[blockIdent.templateIndex], blockIdent.segmentLength);
      else begin    // no other codes defined for version 071. 6-5-01.
        Seek(boxFile, FilePos(boxFile) + blockIdent.segmentLength);
      end;
    end;//case  // no other codes defined for version 071. 6-5-01.
  end;
  // shouldn't get here, EXITs on a zero segment length.
end;

function ExtractProjectTitle(box3: TBox3TemplateList): string;
begin
  if box3.Count > 0 then
    Result := box3[0].keepDims.box_dims1.project_for
  else
    Result := '';
end;

function ExtractGridInfo(box3: TBox3TemplateList): TGridInfo;
var
  b: TBox3Template;
begin
  if box3.Count > 0 then begin
    b := box3[0];
    Result.unitsCode := b.keepDims.box_dims1.grid_units_code;
    Result.spaceX := b.keepDims.box_dims1.x_grid_spacing;
    Result.spaceY := b.keepDims.box_dims1.y_grid_spacing;
  end
  else begin
    Result.unitsCode := 0; // this means the following will not be used..
    Result.spaceX := 50;
    Result.spaceY := 50;
  end;

end;

procedure LoadBox3(
  const boxFilename: string;
  var projectTitle: string;
  var gridInfo: TGridInfo;
  out loadedTemplates: TTemplateList);
var
  boxFile: file;                // new format untyped file.
  n, i, len: integer;
  box3Templates: TBox3TemplateList;
begin
  loadedTemplates := nil;
  box3Templates := nil;
  try
    try
      AssignFile(boxFile, boxFilename);
      Reset(boxFile, 1);              // open for reading, record size = 1 byte.
      try
        box3Templates := TBox3TemplateList.Create;

        ReadTemplateRecords(boxFile, box3Templates);
        ReadStrings(boxFile, box3Templates);
        ReadDataBlocks(boxFile, box3Templates);
      finally
        CloseFile(boxFile);
      end;

      projectTitle := ExtractProjectTitle(box3Templates);
      gridInfo := ExtractGridInfo(box3Templates);
      loadedTemplates := ConvertBox3ToTemplates(box3Templates);
    except
      loadedTemplates.Free;
      raise;
    end
  finally
    box3Templates.Free;
  end;
end;

function LoadBox3BackupRestoreOptions(const boxFilename: string): TBackupRestoreOptions;
var
  boxFile: file;                // new format untyped file.
  box3KeepDims: TBox3KeepDims;
  n, i, len: integer;
begin
  AssignFile(boxFile, boxFilename);
  Reset(boxFile, 1);              // open for reading, record size = 1 byte.
  try
    ReadFirstTemplateRecord(boxFile, box3KeepDims);
  finally
    CloseFile(boxFile);
  end;

  Result.askRestoreOnStartup:= box3KeepDims.box_dims1.ask_restore_on_startup;
  Result.autoRestoreOnStartup:= box3KeepDims.box_dims1.auto_restore_on_startup;
  Result.saveDone:= box3KeepDims.box_dims1.box_save_done;
end;

end.
