
(*

    This file is part of Templot3, a computer program for the design of model railway track.
    Copyright (C) 2018  Martin Wynne.  email: martin@templot.com


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

{ }
unit control_room;

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Menus, ExtCtrls, ComCtrls, FileCtrl, { OT-FIRST, Psock, NMHttp}
  shoved_timber;

type

  { Tcontrol_room_form }

  Tcontrol_room_form = class(TForm)
    control_room_menu_bar: TMainMenu;
    help_menu: TMenuItem;
    about_templot_version_menu_entry: TMenuItem;
    MenuItem1: TMenuItem;
    ot_logo_image: TImage;
    open_panel: TPanel;
    open_button: TButton;
    pad_button: TButton;
    storage_box_button: TButton;
    session_menu: TMenuItem;
    quit_menu_entry: TMenuItem;
    N1: TMenuItem;
    printersetup1: TMenuItem;
    project_title_menu_entry: TMenuItem;
    null_menu_bar: TMainMenu;
    picture_panel: TPanel;
    box_image: TImage;
    printer_calibration_menu_entry: TMenuItem;
    new_gauge_menu_entry: TMenuItem;
    program_menu: TMenuItem;
    step_size_menu_entry: TMenuItem;
    re_org_menu_entry: TMenuItem;
    data_distortions_menu_entry: TMenuItem;
    aspect_distortion_menu_entry: TMenuItem;
    ot_logo_panel: TPanel;
    t3_logo_image: TImage;
    t3_logo_panel: TPanel;
    x_skewing_menu_entry: TMenuItem;
    x_coning_menu_entry: TMenuItem;
    distortion_help_menu_entry: TMenuItem;
    N3: TMenuItem;
    curvingmethod1: TMenuItem;
    standard_wrap_menu_entry: TMenuItem;
    draw_curved_menu_entry: TMenuItem;
    transitionmaths1: TMenuItem;
    auto_terms_menu_entry: TMenuItem;
    custom_terms_menu_entry: TMenuItem;
    expert1: TMenuItem;
    N8: TMenuItem;
    cancel_all_distortions_menu_entry: TMenuItem;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    chat_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    test_menu_entry: TMenuItem;
    number_format_menu_entry: TMenuItem;
    unitangles1: TMenuItem;
    ram1: TMenuItem;
    CLM1: TMenuItem;
    IM1: TMenuItem;
    SM1: TMenuItem;
    x_scaling_menu_entry: TMenuItem;
    y_scaling_menu_entry: TMenuItem;
    background_menu: TMenuItem;
    shapes_menu_entry: TMenuItem;
    y_coning_menu_entry: TMenuItem;
    y_skewing_menu_entry: TMenuItem;
    mirror_x_menu_entry: TMenuItem;
    mirror_y_menu_entry: TMenuItem;
    reorigination1: TMenuItem;
    cancel_re_org_menu_entry: TMenuItem;
    N9: TMenuItem;
    printer_font_menu_entry: TMenuItem;
    auto_mouse_dir_menu_entry: TMenuItem;
    clm_help_menu_entry: TMenuItem;
    N10: TMenuItem;
    logo_bumf_label: TLabel;
    peg_arm_menu_entry: TMenuItem;
    max_explode_menu_entry: TMenuItem;
    custom_input_factors_menu_entry: TMenuItem;
    run_slow_menu_entry: TMenuItem;
    N4: TMenuItem;
    reload_button: TButton;
    reload_menu_entry: TMenuItem;
    clear_templates_menu_entry: TMenuItem;
    clear_shapes_menu_entry: TMenuItem;
    N12: TMenuItem;
    clear_button: TButton;
    control_sketchboard_button: TButton;
    mouse_action_menu_entry: TMenuItem;
    click_move_click_menu_entry: TMenuItem;
    button_down_menu_entry: TMenuItem;
    either_action_menu_entry: TMenuItem;
    N13: TMenuItem;
    storage_box_menu_entry: TMenuItem;
    N7: TMenuItem;
    max_spiral_menu_entry: TMenuItem;
    cpuusage1: TMenuItem;
    fast_100_menu_entry: TMenuItem;
    allow_full_idle_menu_entry: TMenuItem;
    N15: TMenuItem;
    allow_idle_help_menu_entry: TMenuItem;
    mouse_100_menu_entry: TMenuItem;
    version_label: TLabel;
    please_read_first_menu_entry: TMenuItem;
    program_panel_help_menu_entry: TMenuItem;
    graphics_limits_menu_entry: TMenuItem;
    graphics_16_bit_limits_menu_entry: TMenuItem;
    graphics_no_limits_menu_entry: TMenuItem;
    file_menu: TMenuItem;
    save_all_menu_entry: TMenuItem;
    N18: TMenuItem;
    open_storage_box_menu_entry: TMenuItem;
    exports_menu_entry: TMenuItem;
    N11: TMenuItem;
    sliding_wrap_menu_entry: TMenuItem;
    N2: TMenuItem;
    graphics_24_bit_limits_menu_entry: TMenuItem;
    graphics_32_bit_limits_menu_entry: TMenuItem;
    graphics_limits_help_menu_entry: TMenuItem;
    tmec_logo_panel: TPanel;
    tmec_logo_image: TImage;
    com_label: TLabel;
    recent_button: TButton;
    recent_menu_entry: TMenuItem;
    templatenamelabels1: TMenuItem;
    very_random_labels_menu_entry: TMenuItem;
    fixed_labels_menu_entry: TMenuItem;
    random_labels_menu_entry: TMenuItem;
    statusbar_panel: TPanel;
    go_prefs_button: TButton;
    saved_preferences_setup_menu_entry: TMenuItem;
    N5: TMenuItem;
    startup_label: TLabel;
    reminder_label: TLabel;
    reminder_menu_entry: TMenuItem;
    delete_reminder_menu_entry: TMenuItem;
    preferences_menu: TMenuItem;
    prefs_include_gen_settings_menu_entry: TMenuItem;
    update_prefs_on_quit_menu_entry: TMenuItem;
    N17: TMenuItem;
    prefs_exclude_gen_settings_menu_entry: TMenuItem;
    N6: TMenuItem;
    abandon_prefs_menu_entry: TMenuItem;
    older_computers_menu_entry: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    N16: TMenuItem;
    data_panel_font_menu_entry: TMenuItem;
    N14: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    wraphelp1: TMenuItem;
    labelshelp1: TMenuItem;
    make_donation_menu_entry: TMenuItem;
    N25: TMenuItem;
    first_time_here_label: TLabel;
    click_mode_options_menu_entry: TMenuItem;
    click_mode_help_menu_entry: TMenuItem;
    N27: TMenuItem;
    make_on_click_mode_menu_entry: TMenuItem;
    classic_templot_mode_menu_entry: TMenuItem;
    room_file_viewer_menu_entry: TMenuItem;
    filevieweroptions1: TMenuItem;
    viewer_bitmaps_menu_entry: TMenuItem;
    viewer_png_menu_entry: TMenuItem;
    N28: TMenuItem;
    file_viewer_help_menu_entry: TMenuItem;
    viewer_button: TButton;
    about_label: TLabel;
    N29: TMenuItem;
    sketchboard_menu_entry: TMenuItem;
    previous_labels_menu_entry: TMenuItem;
    Label1: TLabel;
    use_wine_fonts_menu_entry: TMenuItem;
    popup_menu_position_menu_entry: TMenuItem;
    blank_spaces_in_menus_menu_entry: TMenuItem;
    watchavideo1: TMenuItem;
    video1: TMenuItem;
    TemplotCompanionUserGuide1: TMenuItem;
    pleaseclickthehelpmenuonthetrackpadwindow1: TMenuItem;
    jpg_menu_entry: TMenuItem;
    trackpad_position_menu_entry: TMenuItem;
    show_time_ticker_menu_entry: TMenuItem;
    procedure open_buttonClick(Sender: TObject);
    procedure about_templot_version_menu_entryClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure quit_menu_entryClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chat_panelClick(Sender: TObject);
    procedure how_panelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure project_title_menu_entryClick(Sender: TObject);
    procedure printer_setup_menu_entryClick(Sender: TObject);
    procedure pad_buttonClick(Sender: TObject);

    procedure AppActivate(Sender: TObject);

    procedure AppIdle(Sender: TObject; var Done: Boolean);

    procedure AppRestore(Sender: TObject);  // 0.91.b

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure printer_calibration_menu_entryClick(Sender: TObject);
    procedure colour_buttonClick(Sender: TObject);
    procedure new_gauge_menu_entryClick(Sender: TObject);
    procedure aspect_distortion_menu_entryClick(Sender: TObject);
    procedure auto_terms_menu_entryClick(Sender: TObject);
    procedure custom_terms_menu_entryClick(Sender: TObject);
    procedure step_size_menu_entryClick(Sender: TObject);
    procedure cancel_all_distortions_menu_entryClick(Sender: TObject);
    procedure re_org_menu_entryClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure number_format_menu_entryClick(Sender: TObject);
    procedure test_menu_entryClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure distortion_help_menu_entryClick(Sender: TObject);
    procedure x_scaling_menu_entryClick(Sender: TObject);
    procedure y_scaling_menu_entryClick(Sender: TObject);
    procedure mirror_x_menu_entryClick(Sender: TObject);
    procedure mirror_y_menu_entryClick(Sender: TObject);
    procedure x_coning_menu_entryClick(Sender: TObject);
    procedure y_coning_menu_entryClick(Sender: TObject);
    procedure cancel_re_org_menu_entryClick(Sender: TObject);
    procedure x_skewing_menu_entryClick(Sender: TObject);
    procedure y_skewing_menu_entryClick(Sender: TObject);
    procedure printer_font_menu_entryClick(Sender: TObject);
    procedure shapes_menu_entryClick(Sender: TObject);
    procedure auto_mouse_dir_menu_entryClick(Sender: TObject);
    procedure clm_help_menu_entryClick(Sender: TObject);
    procedure peg_arm_menu_entryClick(Sender: TObject);
    procedure max_explode_menu_entryClick(Sender: TObject);
    procedure custom_input_factors_menu_entryClick(Sender: TObject);
    procedure run_slow_menu_entryClick(Sender: TObject);
    procedure reload_buttonClick(Sender: TObject);
    procedure clear_templates_menu_entryClick(Sender: TObject);
    procedure clear_shapes_menu_entryClick(Sender: TObject);
    procedure keeps_list_change(Sender: TObject);
    procedure click_move_click_menu_entryClick(Sender: TObject);
    procedure button_down_menu_entryClick(Sender: TObject);
    procedure either_action_menu_entryClick(Sender: TObject);
    procedure storage_box_menu_entryClick(Sender: TObject);
    procedure program_menuClick(Sender: TObject);
    procedure max_spiral_menu_entryClick(Sender: TObject);
    procedure fast_100_menu_entryClick(Sender: TObject);
    procedure allow_full_idle_menu_entryClick(Sender: TObject);
    procedure allow_idle_help_menu_entryClick(Sender: TObject);
    procedure mouse_100_menu_entryClick(Sender: TObject);
    procedure please_read_first_menu_entryClick(Sender: TObject);
    procedure graphics_16_bit_limits_menu_entryClick(Sender: TObject);
    procedure graphics_no_limits_menu_entryClick(Sender: TObject);
    procedure save_all_menu_entryClick(Sender: TObject);
    procedure standard_wrap_menu_entryClick(Sender: TObject);
    procedure control_sketchboard_buttonClick(Sender: TObject);
    procedure graphics_24_bit_limits_menu_entryClick(Sender: TObject);
    procedure graphics_32_bit_limits_menu_entryClick(Sender: TObject);
    procedure graphics_limits_help_menu_entryClick(Sender: TObject);
    procedure com_labelClick(Sender: TObject);
    procedure recent_buttonClick(Sender: TObject);
    procedure very_random_labels_menu_entryClick(Sender: TObject);
    procedure fixed_labels_menu_entryClick(Sender: TObject);
    procedure random_labels_menu_entryClick(Sender: TObject);
    procedure go_prefs_buttonClick(Sender: TObject);
    procedure saved_preferences_setup_menu_entryClick(Sender: TObject);
    procedure reminder_menu_entryClick(Sender: TObject);
    procedure delete_reminder_menu_entryClick(Sender: TObject);
    procedure update_prefs_on_quit_menu_entryClick(Sender: TObject);
    procedure prefs_include_gen_settings_menu_entryClick(Sender: TObject);
    procedure prefs_exclude_gen_settings_menu_entryClick(Sender: TObject);
    procedure preferences_menuClick(Sender: TObject);
    procedure abandon_prefs_menu_entryClick(Sender: TObject);
    procedure mouse_action_menu_entryClick(Sender: TObject);
    procedure older_computers_menu_entryClick(Sender: TObject);
    procedure data_panel_font_menu_entryClick(Sender: TObject);
    procedure exports_menu_entryClick(Sender: TObject);
    procedure make_donation_menu_entryClick(Sender: TObject);
    procedure classic_templot_mode_menu_entryClick(Sender: TObject);
    procedure make_on_click_mode_menu_entryClick(Sender: TObject);
    procedure click_mode_help_menu_entryClick(Sender: TObject);
    procedure click_mode_options_menu_entryClick(Sender: TObject);
    procedure room_file_viewer_menu_entryClick(Sender: TObject);
    procedure viewer_bitmaps_menu_entryClick(Sender: TObject);
    procedure viewer_png_menu_entryClick(Sender: TObject);
    procedure file_viewer_help_menu_entryClick(Sender: TObject);
    procedure previous_labels_menu_entryClick(Sender: TObject);
    procedure use_wine_fonts_menu_entryClick(Sender: TObject);
    procedure popup_menu_position_menu_entryClick(Sender: TObject);
    procedure blank_spaces_in_menus_menu_entryClick(Sender: TObject);
    procedure jpg_menu_entryClick(Sender: TObject);
    procedure trackpad_position_menu_entryClick(Sender: TObject);
    procedure show_time_ticker_menu_entryClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;//class

  Tversion_options = (voFull, voShort);

var
  control_room_form: Tcontrol_room_form;

//-----------------------------------------

const

  program_name_str: string = 'TemplotMEC';

  //program_name_str:string='OpenTemplot';

  //program_name_str:string='Templot3';

  //
  // Version information
  //
  // program_version: the release version number (*100, e.g. v:1.03 = 103)
  // version_build:   sub-build for the release, or .dev for in-development
  // file_version:    version number for saved files. This is a simple integer that should be incremented
  //                  for any change to the saved file format.
  //
  // program_version and version_build should be set appropriately when preparing a release version,
  //   otherwise set to 0.00.dev during development
  //
  // file_version may increase as required during development, as has no direct correlation to the
  //   program_version
  //
  program_version: integer = 0;
  version_build: string = '.dev';
  file_version: integer = 3000;


  distortion_help_str: string = '      Expert  Help  -  Data  Distortions' +
    '||These data distortion functions are intended primarily for use when exporting templates in DXF file format for transfer to CAD software.' + '||There should normally be no need to distort (modify) the drawing data when printing templates directly from Templot0.' + ' Any necessary corrections for your printer are intended to be made using the PRINTER CALIBRATION function, and the ENLARGE / REDUCE print scaling function' + ' is available to change the size of the printed template. These functions are accessed from the OUTPUT menu on the TRACKPAD. You should have a good' + ' reason therefore for using these distortions instead and be aware that your printed templates may become inaccurate or unusable if you do so.' + '||Any data-distortions or re-origination should be made BEFORE copying templates to the background. It is not possible to make changes to the data-distortion or re-origination settings' + ' while background templates are currently being drawn.' + '||Because the DXF files from Templot0 ignore the printer calibration and output scaling functions, these data distortions are provided instead as a means of making' + ' similar adjustments for templates exported to a CAD program. You will need to do some trial-and-error printing from your CAD software to establish the required distortion factors.' + '||This is best done by printing a short length of straight plain track which has very long sleepers (say 500 inches) so that the effect of the distortions' + ' can be more easily seen and measured (GEOMETRY > STRAIGHT and REAL > TIMBERING > TIMBERING DATA... menu items). If printing direct from Templot, remember that the printed' + ' grid lines remain undistorted, and thus aid comparison between the distorted and undistorted pages.' + ' Unless you are using a flat-panel display it is difficult to judge the distortion effects on the curved screen.' + '||All these distortions apply globally for as long as they remain in force, so all templates drawn on the trackpad or in the storage box will be' + ' distorted but will return to normal when distortion is cancelled. The distortion data is not included in the template files' + ' when they are saved from the box, and must be re-entered as required for each working session. All distortions are cancelled when you' + ' do a B-6 TURNOUT RESET (CTRL-Z).' + '||( Handy hint - make a note of your distortion settings in your memo text. They can then be quickly copied and pasted back into the data entry form after reloading.)' + '||These distortion functions are listed under the EXPERT menu item, which means that there is little error-checking. It might be posssible to hang or crash Templot' + ' by entering silly figures, so be sure to save your work before experimenting.' + '||These distortions do not apply to the grid lines or printed trim margins, which are not included in DXF files. If you are using these' + ' distortions when printing directly from Templot0 the grid will therefore be inaccurate and should be turned off.' + '||If multiple distortions and/or re-origination are used, they are applied to the drawing data in the following order:' + '||Scaling and Aspect-distortions' + '|Coning' + '|Skewing' + '|Mirroring' + '|Re-origination' + '||Take care with your calculation of the various factors when making multiple distortions. If for some weird reason you choose to use multiple' + ' negative factors you will need a clear head to keep track of your template''s position and the direction of the mouse actions.' + '||The X coning and skewing distortions are intended for use when printing in upright (portrait) format and the Y coning and skewing distortions' + ' are intended for use when printing in sideways (landscape) format. Templot0 does NOT check this however and will distort both ways at once if you' + ' set factors for both. It is difficult to imagine a reason to do this !' + '||Unlike the normal shift functions, re-origination takes place after all distortions and so is unaffected by them. This allows you to position the' + ' distorted template relative to the zero datum to suit the requirements of your CAD application.' + '||The pre-sets for these distortion factors correspond to the cancelled condition in each case and are as follows:' + '||X-scaling factor = 100 %' + '|Y-scaling factor = 100 %' + '|Aspect-distortion factor = 100 %' + '|X-Coning factor = 0' + '|Y-Coning factor = 0' + '|X-Skewing factor = 0' + '|Y-Skewing factor = 0' + '||For more information about each of the above, click the help flags for each on the data entry form.' + '||Mirroring requires no factor and is toggled on-off on the menu as required. X-Mirroring mirrors the drawing leftwards about the data origin.' + ' Y-Mirroring mirrors the drawing downwards about the data origin.' + '||Bear in mind that if you use mirroring or negative distortion factors, what appears to be a left-hand turnout may have been calculated as a right-hand one' + ' and be labelled as such in the information panel and storage box (or vice-versa).' + '||Even if distortions, mirroring or re-origination shift your template completely off-screen, the DXF file will still contain it. ( This is the opposite of the behaviour when' + ' printing directly from Templot0, where any part of the drawing to the left of or below the page outlines is cropped.)';

  coning_help_str: string = '      Coning  Distortions' +
    '||Coning distortion is an attempt to correct for a worn printer in which the paper roller has become slightly tapered.'
    + '||A correction shift forwards or backwards is applied to one dimension which is proportional to the product of both dimensions.' + '||Coning takes place after any scaling distortions, so for X-coning for example, the basic equation is:-' + '||x-coned = x-scaled + ( x-coning-factor * x-scaled * y-scaled )' + '||and setting a coning-factor of zero cancels any effect.' + '||It will be necessary to establish the coning factor by trial and error for the individual printer being used.' + ' This is best done by printing a short length of straight plain track which has very long sleepers (say 500 inches) so that the effect of the distortions' + ' can be more easily seen and measured (GEOMETRY > STRAIGHT and REAL > TIMBERING > TIMBERING DATA... menu items). If printing direct from Templot, remember that the printed' + ' grid lines remain undistorted, and thus aid comparison between the distorted and undistorted pages.' + '||The coning factor is always extremely small. For convenience it is internally scaled so that the figures to be entered are' + ' positive or negative and in the range from 0 to perhaps 100. Factors greater than this will produce significant distortion' + ' which is unlikely to print correctly on any printer. A typical setting might be, say, +16.7 or -9.4 which will produce a measurable effect on' + ' a full printed page but not be very evident on the screen.' + '||( It can be amusing to enter a figure such as -5000 and then experiment with' + ' the mouse actions. This will give you an appreciation of the effect of coning distortion, but it won''t progress your railway very much.)' + '||Coning distortion applies only to the template drawing, so the grid lines will be inaccurate and should be turned off.' + '||Coning is cancelled on a B-6 TURNOUT RESET, but otherwise remains in force until you come back here and change it.' + '||Coning distortions apply globally for as long as they remain in force, so all templates drawn on the trackpad or in the storage box will be' + ' distorted but will return to normal when the distortion is cancelled. The distortion data is not included in the template files' + ' when they are saved from the box, and must be re-entered as required for each working session.' + '||Please read the F1 general help notes below and the help notes for the other distortions before using coning for the first time.';

  skewing_help_str: string = '      Skewing  Distortions' +
    '||Skewing distortion is an attempt to correct for a worn printer in which movement of the print head is not exactly square across the direction of paper feed.' + '||A correction shift forwards or backwards is applied to one dimension which is proportional to the other dimension.' + '||Skewing takes place after any coning distortions, so for Y-skewing for example, the basic equation is:-' + '||y-skewed = y-coned + ( y-skewing-factor * x-coned )' + '||and setting a skewing-factor of zero cancels any effect.' + '||It will be necessary to establish the skewing factor by trial and error for the individual printer being used.' + ' This is best done by printing a short length of straight plain track which has very long sleepers (say 500 inches) so that the effect of the distortions' + ' can be more easily seen and measured (GEOMETRY > STRAIGHT and REAL > TIMBERING > TIMBERING DATA... menu items). If printing direct from Templot, remember that the printed' + ' grid lines remain undistorted, and thus aid comparison between the distorted and undistorted pages.' + '||The skewing factor is always very small. For convenience it is internally scaled so that the figures to be entered are' + ' positive or negative and in the range from 0 to perhaps 100. Factors greater than this will produce very significant distortion' + ' which is unlikely to print correctly on any printer. A typical setting might be, say, +16.7 or -9.4 which will produce a measurable effect on' + ' a full printed page but not be very evident on the screen.' + '||( It can be amusing to enter a figure such as -5000 and then experiment with' + ' the mouse actions. This will give you an appreciation of the effect of skewing distortion, but it won''t progress your railway very much.)' + '||Skewing distortion applies only to the template drawing, so the grid lines will be inaccurate and should be turned off.' + '||Skewing is cancelled on a B-6 TURNOUT RESET, but otherwise remains in force until you come back here and change it.' + '||Skewing distortions apply globally for as long as they remain in force, so all templates drawn on the trackpad or in the storage box will be' + ' skewed but will return to normal when the distortion is cancelled. The distortion data is not included in the template files' + ' when they are saved from the box, and must be re-entered as required for each working session.' + '||Please read the F1 general help notes below and the help notes for the other distortions before using skewing for the first time.';


  // custom cursor constants..

  adjust_ne_cursor_invert = 1;        // F7 shift.
  adjust_corners_cursor_invert = 2;   // both mouse action.
  adjust_ns_cursor_invert = 3;        // F6 curving, F8 rotate, slew amount.
  adjust_we_cursor_invert = 4;        // F3, F4, F5
  mouse_action_cursor = 5;            // mouse actions.
  cross_size_cursor = 6;              // CTRL-F4 pad move.
  target_rings_cursor = 7;
  open_slider_cursor = 8;
  zoom_rectangle_cursor = 9;        // added 13-1-01.
  cross_hairs_cursor = 10;          // added 13-1-01.
  group_rectangle_cursor = 11;      // added 22-2-01.

  maxfp: double = 1.0E300;         // max float value for our calcs.
  minfp: double = 1.0E-12;         // min float value for our calcs
  // (less than this is regarded as zero to avoid rounding errors).

  max_single: single = 1.0E10;       // used for sketchboard calcs. 212a

  minfp_big: double = 1.0E-6;      // min float value for rounding and tolerancing.

  def_req: double = 0 - 1.0E300;
  // DON'T CHANGE (used directly as the value in some places 0.93.a)  // this value in a float requests a default to be used instead.
  def_req_i: integer = Integer($80000001);    // this value in an integer ditto.

  def_req_single: single = 0 - 1.0E10;       // 212a for jt_slwide

  maxint: double = Integer($7FFFFFFF);
  //  these are floats - max 32-bit value for range checks.
  minint: double = Integer($80000000);      //             ditto - min 32-bit value.

  h_maxint: double = Integer($3FFFFFFF);
  //  maxint/2   these are floats. 31-bit range to leave room for some arithmetic on the values.
  h_minint: double = Integer($C0000000);    //  minint/2

// defaults for Win 95/98/ME graphics limits


var

  max_draw_int: integer = 16000;
  //  these are less than 15-bit integers for screen drawing limits...
  min_draw_int: integer = -16000;       //  (divide 16-bit by 2 and a bit as safety margin).

  // 0.93.a initial defaults for export limits..

  max_export_x: integer = 16000;
  min_export_x: integer = -16000;

  max_export_y: integer = 16000;
  min_export_y: integer = -16000;


  loaded_version: integer = 50000;   // init the loaded data file versions..
  later_file: boolean = False;

  he_wants_multiple_monitors: boolean = False;  // 0.91.b

  ebk1_str: string = '';             // 1st emergency backup file name.
  ebk2_str: string = '';             // 2nd emergency backup file name.
  pb_str: string = '';               // renamed previous data backup file.
  pbo_str: string = '';              // renamed prior-previous backup file.

  html_path_str: string = '';        // for html viewer. 0.91

  keeps_list: TStringList;                    // 8-2-99
  memo_list: TStringList;

  tag_list: TStringList;                  // 206b

  printer_list: TStringList;
  current_shove_list: Tshoved_timber_list;
  info_text_list: TStringList;            // 0.78.a    15-11-02.

  custom_colour_list: TStringList;        // 0.91

  pad_view_list: TStringList;             // 0.91.c

  html_help_list: TStringList;            // 0.91
  html_printheader_list: TStringList;
  html_printfooter_list: TStringList;

  boxmru_list: TStringList;               // 0.82.a  22-08-06
  bgsmru_list: TStringList;               // 0.82.a  22-08-06

  // 0.91.d pref_options...

  user_prefs_list: TStringList;        // 0.91.d
  user_prefs_in_use: boolean = False;
  prefs_existed_on_startup: boolean = False;
  prefs_available: boolean = False;


  initdone_flag: boolean = False;   // flag no init done.
  quit_code: integer = 0;           // instant abandon if he wants on startup.
  under_way: boolean = False;       // pad repaints, etc not yet permitted.

  prog_running: boolean = True;      // for run_error no repaints.
  slow_run: double = 0;            // for slow running.

  quit_alert_done: boolean = False;

  allow_idle: boolean = True;         // 0.73.a 17-9-01.

  graphics_limits: boolean = True;    // 0.76.a 25-5-02.

  export_limits: boolean = False;     // 0.93.a

  pad_form_window_state: TWindowState = wsMaximized;  // 0.91.b

  on_idle_can_run: boolean = False;
  // not yet ready for on_idle to do saves and redraws. 0.73.a 16-9-01.

  box_project_title_str, current_name_str: string;
  //exe_str: string;                                       // execution file path.
  reminder_file_path: string;


  hi_color: boolean = False;
  start_colours: integer = 0;

  distortions: word;    // flag bits.  LSB 0  x-scaling
  //                 1  y-scaling
  //                 2  aspect
  //                 3  unused

  //                 4  x-coning
  //                 5  y-coning
  //                 6  x-skewing
  //                 7  y-skewing

  //                 8  mirror-x
  //                 9  mirror-y
  //                10..  unused

  aspect_distortion_factor, re_org_x, re_org_y: double;
  x_distortion_factor, y_distortion_factor: double;
  x_coning_distortion_factor, y_coning_distortion_factor: double;
  x_skewing_distortion_factor, y_skewing_distortion_factor: double;
  mirror_x, mirror_y: double;

  auto_dir: boolean = True;
  backup_wanted: boolean = False;

  external_window_showing: boolean = False;  // 0.93.a  for files showing containing folder

  mouse_click_action: integer = -1;   // either click-move-click or drag allowed.

  printer_text_left_margin: double = 10;   // 10mm page margins when printing.
  printer_text_right_margin: double = 10;

  jpg_quality: integer = 100;  // 214a

  // alert message preferences...  //  0.91.d

  res_msg_pref: boolean = False;
  ppi_msg_pref: boolean = False;
  start_scheme_msg_pref: boolean = False;
  multi_monitors_msg_pref: boolean = False;
  refresh_msg_pref: boolean = False;
  quit_confirm_msg_pref: boolean = False;
  prefs_warn_msg_pref: boolean = False;
  wine_warn_msg_pref: boolean = False;    // 212b

  delete_to_current_msg_pref: boolean = False;
  k180_message_msg_pref: boolean = False;
  k_notch_msg_pref: boolean = False;
  mirrory_msg_pref: boolean = False;
  mirrorx_msg_pref: boolean = False;
  dupg_msg_pref: boolean = False;

  small_print_msg_pref: boolean = False;
  spiral_mouse_msg_pref: boolean = True;  // not used
  group_colour_msg_pref: boolean = False;

  b6_msg_pref: boolean = False;
  no_delete_msg_pref: boolean = False;
  no_unused_msg_pref: boolean = False;
  no_library_msg_pref: boolean = False;
  no_undo_clear_msg_pref: boolean = False;
  no_cut_msg_pref: boolean = False;

  trans_msg_pref: boolean = False;
  cancel_entries_msg_pref: boolean = False;
  no_extra_colours_msg_pref: boolean = False;
  omit_all_msg_pref: boolean = False;
  drop_msg_pref: boolean = False;
  yellow_msg_pref: boolean = False;
  auto_fit_msg_pref: boolean = False;

  screenshot_msg_pref: boolean = False;         // 215a

  confirm_detail_mode_1_msg_pref: boolean = False;
  confirm_detail_mode_2_msg_pref: boolean = False;

  full_draw_trans_pref: boolean = False;  // not used
  startup_full_draw_pref: boolean = True;
  startup_offscreen_pref: boolean = True;

  irreg_crossings_msg_pref: boolean = False;

  sb_rvf_add_msg_pref: boolean = False;    // 0.97.a
  sb_no_select_msg_pref: boolean = False;  // 0.98.b

  too_many_files_msg_pref: boolean = False;  // 208e

  retain_prefix_msg_pref: boolean = False;   // 213b
  retain_prefix_pref: boolean = False;       // 213b


  running_under_wine: boolean = False;  // 205a
  arial_str: string = 'Arial';          // 205a modified for Wine

  using_native_colours: boolean = False;   // 212a

  current_scaling_position: integer = 4;        // 214c    4=medium
  old_scaling_position: integer = 4;            // 214c
  scaling_done_at_least_once: boolean = False;  // 214c

  http_get_server1_str: string = 'http://templot.co.uk/';    // 206b     try UK server first.
  http_get_server2_str: string = 'http://templot.com/';      // on Ohio server 0.97.a

  companion_viewer_str: string = '';  // 214a  set dpi aware in startup_unit

procedure run_error(code: integer);

procedure set_printer_font_margins(calling_form: TForm; set_font: boolean);  // 0.91

procedure go_to_templot_com;         // 0.93.a
procedure go_to_templot_club;

procedure go_to_templot_companion;
procedure go_to_templot_companion_page(dest_page_str: string);    // 214a

procedure go_to_donation;            // 0.96.a Templot2
procedure go_to_upgrade;             // 0.96.a Templot2

procedure go_to_video_list;          // 211b Templot2

procedure go_to_url(url: string);     // 212a


procedure wine_modal_warning(str: string);  // 206d


function change_jpeg_filename(in_str: string): string; // 214a

function get_path_to_Windows_folder(folder_id: integer): string;   // 214a

procedure open_MyPictures;    // 214a
procedure open_MyDocuments;   // 214a

procedure do_open_source_bang(str: string);  // OT-FIRST

function GetVersionString(version_options: Tversion_options): string;

//______________________________________________________________________________

implementation

{$BOOLEVAL ON}

uses
  LCLType, LCLIntf, Math, Clipbrd, FileUtil, {FileCtrl,} {@demo Clipbrd,} Printers,
  TLoggerUnit,
  alert_unit,
  config_unit,
  help_sheet, math_unit,
  pad_unit, info_unit,
  gauge_unit, chat_unit, entry_sheet, print_unit, keep_select,
  switch_select, metric_unit, colour_unit, xing_select,
  dxf_unit, bgnd_unit, bgkeeps_unit, panning_unit, grid_unit,
  shove_timber, stay_visible_unit, action_unit, mint_unit, jotter_unit, print_settings_unit,
  prefs_unit, plain_track_unit, calibration_unit, startup_unit,


  { OT-FIRST dtpGR32,} edit_memo_unit, { OT-FIRST dtp_unit, dtp_settings_unit,} platform_unit,
  data_memo_unit, { OT-FIRST sketchboard_unit,} check_diffs_unit, rail_options_unit,
  export_unit, file_viewer, wait_message, { OT-FIRST companion_load_unit,}

  trackbed_unit,

  {IcsMD5,} make_slip_unit, create_tandem,     // 217a
  curve;

{$R *.lfm}

{$R custom_cursors.res}

var
  // OT-FIRST ...
  log: ILogger;

  licence_str: string = 'Copyright &copy; 2018  Martin Wynne.  email: martin@templot.com .' +
    ' This program is free software: you may redistribute it and/or modify' +
    ' it under the terms of the GNU General Public Licence as published by' +
    ' the Free Software Foundation, either version 3 of the Licence, or' +
    ' (at your option) any later version.' +
    ' This program is distributed in the hope that it will be useful,' +
    ' but WITHOUT ANY WARRANTY; without even the implied warranty of' +
    ' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' +
    ' See the GNU General Public Licence for more details.<br>' +
    'You should have received a copy of the GNU General Public Licence with this program.<br>' +
    'If not, refer to the web site: https://www.gnu.org/licenses/';

  w_len: integer = 0;
  legal_agree: boolean = False;
  welcome_done: boolean = False;
  room_scaling: boolean = False;
  // flag - otherwise statusbar ScrollInView on resize prevents form rescaling properly.

  closing_flag: boolean;

  // OT-FIRST wait_internet_form:Tstartup_form;  // 205c

  open_button_clicked: boolean = False;     // 205d

  md5_str: string = '';  // 217a

procedure check_colours; forward;
procedure create_backup_file(final: boolean); forward;

function cleared_bgnd: boolean; forward;


//__________________________________________________________________________________________
function GetVersionString(version_options: Tversion_options): string;
begin
  case version_options of
    voShort:
      Result := FormatFloat('0.00', program_version / 100);
    voFull:
      Result := FormatFloat('0.00', program_version / 100) + version_build;
    else
      Result := GetVersionString(voFull);
  end;
end;

//__________________________________________________________________________________________

procedure startup_modal_warning(over_ride: boolean);

// over_ride True when called from data-entry form

begin
  if (wine_warn_msg_pref = False) or (over_ride = True) then begin
    if over_ride = False then begin
      alert_box.preferences_checkbox.Checked := False;
      alert_box.preferences_checkbox.Show;
    end;

    alert(2, '   Running  on   Linux / Wine / CrossOver',

      '`0important  information  -  please  read`9' +
      '||This program is running on Linux / Wine / CrossOver.' +
      '||In the event the active window becomes hidden behind other windows, the program may stop responding to mouse clicks.'
      + '||If this happens, press the `0ESC`2 key on the keyboard (top left) to restore normal working.'
      + '||If that doesn''t work, you must drag the topmost window(s) out of the way to reveal the active window or dialog. Or try pressing `0ALT+F7`2.' + '||To prevent these problems, take care to avoid clicking anywhere other than on the topmost window, dialog or message box.' + '||(These problems do not arise when running on Windows.)' + '||<HR STYLE="COLOR:GRAY; HEIGHT:3px; NOSHADE">' + '|By default this program uses Windows native fonts.' + '||If these do not display properly you can try changing to Wine open-source  fonts.' + '||On the `0program panel`1 window click the `0program > use Wine fonts`z menu item.' + '||To return to using Windows native fonts it will be necessary to restart Templot0.',
      '', '', '', '', '', 'OK', 0);

    if over_ride = False then begin
      wine_warn_msg_pref := alert_box.preferences_checkbox.Checked;
      alert_box.preferences_checkbox.Hide;
    end;
  end;
end;
//______________________________________________________________________________

procedure wine_modal_warning(str: string);

begin
  if alert(1, 'php/990    running  under  Linux / Wine / CrossOver',
    'You are running Templot0 under Linux/Wine/CrossOver.' + '||The ' +
    str + ' dialog is required to be modal, which means that it must be completed before Templot0 can continue.' + '||Modal dialogs are not fully supported under Wine, so while the dialog window is showing you must click on the dialog window ONLY.' + '||If you click elsewhere on the screen the dialog may become hidden behind other windows, after which Templot0 will not be able to continue until you find the dialog again and complete it.' + '||If you cannot do that, you can press the `0ESC`2 key (top-left) on the keyboard to cancel the operation and try again.' + '||The same applies to this message dialog, while it is showing you must not click anywhere else.' + '||This is a feature of Wine beyond Templot0''s control.' + '||If you have problems with this, please post a message on the <A HREF="go_to_templot_club.85a"><SPAN STYLE="color:#0000FF;"><U>Templot&nbsp;Club</U></SPAN></A> user forum.', '', '', '', 'more  information', '', 'continue', 0) = 4 then
    startup_modal_warning(True); // true=over-ride msg prefs
end;
//______________________________________________________________________________

procedure run_error(code: integer);

begin
  prog_running := False;              // prevent pad repaints, etc.
  wipe_pad;                         // blank it all.

  alert(5, '    fatal  error',
    '||Sorry, Templot0 has suffered a fatal internal error and cannot continue.' +
    '||Please report fail code  ' + IntToStr(code) +
    '||If you save your work on quitting, please use a fresh file name as a precaution against corrupting your existing file.'
    + '||Please quit and restart Templot0.' +
    '||If the problem recurs, please post a message at 85a.co.uk/forum/ quoting the above fail code.',
    '', '', '', '', '', 'quit  Templot', 0);

  quit_code := 2;       //  flag no cancel possible, and delete backups.

  control_room_form.Close;      //  this is the main form,
  //  so the application closes (via the quit query alert).


  RUNERROR(99);           // mustn't return from this routine.
end;
//_______________________________________________________________________________________

function quit_alert: Boolean;       //  he wants to quit...
  //  returns True if quit permitted.
const
  quit_help: string = '    Saving  your  work  between  sessions.' +
    '||"Storing" a template in your STORAGE BOX is not the same thing as "saving" it to a file on a disk drive.'
    + ' "Stored" templates are held only in Templot0''s memory, so that each template is instantly available without the need to access files.' + '||Templot0 can automatically restore the contents of your storage box and your background track plan (if any) between sessions, but it can only "remember" one set of box contents at any one time.' + '||For future reference and reloading whenever you need them, you may also want to save the current box contents in a separate named data file before quitting Templot0.' + '||For more information about saving data files, click the ? HELP button in the storage box.' + '|---------------------------------' + '||If you want to prevent an automatic restore on the next startup, change the OPTIONS > RESTORE ON STARTUP menu options in the storage box menus.' + '||To restore the previous box contents at any time after startup, click the FILES > RESTORE PREVIOUS menu item in the storage box menus.' + ' This works independently of the option settings for automatic restore on startup. You can click RESTORE PREVIOUS as often as you wish, the data from your previous Templot0 session remains available throughout the current session.' + '||Likewise, the FILES > RESTORE PRIOR-PREVIOUS menu item restores the box contents from the Templot0 session prior to the previous one, i.e. from two sessions back.' + '||The restore feature works independently of your own saved data files, and makes no changes to them.' + '||The restore feature works correctly even if your previous session terminated abnormally as a result of a power failure or system malfunction, so there is no need to perform repeated saves as a precaution against these events.' + '||N.B. Background Shapes are not included in the automatic restore feature, and must always be saved separately.' + '||N.B. If you run two instances of Templot0 concurrently (not recommended for Windows 95/98/ME) from the same TEMPLOT folder,' + ' the restore data will be held in common between the two. To prevent this happening, create and run the second instance from a different folder (directory).';

var
  i: integer;
  save_str, no_save_str, spacer_str, backup_str: string;

begin
  if quit_alert_done = True
  // been here once and he already quit. (in case duplicate call on Windows terminate message).
  then begin
    Result := True;
    EXIT;
  end;

  Result := False;                  // default init, don't quit.
  case quit_code of

    0:
      Result := True;    //  instant abandon.

    2: begin
      Result := True;           // code 2 - run_error  - don't permit cancel.
      DeleteFile(ebk1_str);   // and clear backup files - might be corrupted..
      DeleteFile(ebk2_str);

      if save_done = False then begin
        repeat
          i :=
            alert(1, '   session  must  finish   -   save  templates ?',
            '|An error has occurred and Templot0 is about to quit.' +
            '||To avoid corrupted data, the storage box contents will not be restored next time.'
            +
            '||Your storage box contains one or more templates which you wanted to keep, but which have not yet been saved.'
            + '||Do you want to save your work ?', '', '', '?  help',
            'no  -  abandon  templates  and  quit    ', '',
            'yes  -  save  all  templates  and  quit    ', 3);
          if i = 3 then
            alert_help(0, quit_help, '');
        until i <> 3;

        case i of
          6:
            save_box(0, 0, 0, '');   //  go do the save.
        end;//case
      end;

      if shapes_saved = False then begin
        i := alert(7, '      session  must  finish   -   save  shapes ?',
          '||Templot0 is about to quit.' +
          '||You have one or more background shapes which have not been saved.' +
          '||Do you want to save your background shapes ?', '', '', '',
          'no  -  abandon  shapes  and  quit    ', '',
          'yes  -  save  all  shapes  and  quit    ', 0);

        case i of
          6:
            bgnd_form.save_all_menu_entry.Click;
        end;//case
      end;
    end;

    3: begin                             // code 3 - normal quit..

      create_backup_file(True);       // one last time.

      if shapes_saved = False then begin
        i := alert(7, '      session  finished ?   -   save  shapes ?',
          '||Templot0 is about to quit.' +
          '||You have one or more background shapes which have not been saved.' +
          '||Do you want to save your background shapes ?', '', '', '',
          'no  -  abandon  shapes  and  quit    ', 'cancel  quit   -   continue  running      ',
          'yes  -  save  all  shapes  and  quit    ', 0);

        case i of
          4:
            Result := True;
          5: begin
            Result := False;
            EXIT;
          end;
          6: begin
            bgnd_form.save_all_menu_entry.Click;
            Result := shapes_saved;
            if Result = False then
              EXIT;      //  he cancelled the save.
          end;
        end;//case
      end;

      if (keeps_list.Count = 0) and (Result = True)   // nothing else to save.
      then
        EXIT                                // so don't alert him again, just quit.
      else begin
        spacer_str := '|||||                      '; // defaults..
        save_str := 'QUIT        ';
        no_save_str := '';
        backup_str := '';

        if (keeps_list.Count > 0) and (save_done = False) then begin
          save_str :=
            'save  all  templates  in  a  named  file  and  QUIT            ';
          no_save_str := 'QUIT        ';
          spacer_str := '|                  ';

          backup_str :=
            '||Your storage box contains one or more templates which have not yet been saved in a named data file.'
            + '||These and your background track plan can be restored when you next run Templot,'
            + ' but you may also want to save them in a named data file before quitting.';
        end;


        if (quit_confirm_msg_pref = False) or (no_save_str <> '')
        // pref by-passed if nothing to save.
        then begin

          if no_save_str = ''
          // nothing to save, but pref is to show dialog (default).
          then begin
            alert_box.preferences_checkbox.Checked := False;
            //%%%%
            if user_prefs_in_use = True then
              alert_box.preferences_checkbox.Show;
          end;

          repeat
            i :=
              alert(7, '      session  finished ?',
              '|                Templot0  is  about  to  QUIT.' + backup_str,
              '', '', 'important  information  ', no_save_str,
              'cancel  quit   -   continue  running    ', save_str, 3);
            if i = 3 then
              alert_help(0, quit_help, '');
          until i <> 3;

          if no_save_str = '' then begin
            quit_confirm_msg_pref :=
              alert_box.preferences_checkbox.Checked;    //%%%%
            alert_box.preferences_checkbox.Hide;
          end;
        end
        else
          i := 6;    // by-passed.

        case i of
          4:
            Result := True;
          5:
            Result := False;
          6:
            if save_done = False then
              Result := save_box(0, 0, 0, '')  //  go do the save.
            else
              Result := True;
        end;//case
      end;
      EXIT;
    end;

    else
      Result := False;  // !!! no other cases of quit_code yet 14-6-98.
  end;//case
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.AppRestore(Sender: TObject);   // 0.91.b

begin
  pad_form.WindowState := pad_form_window_state;  // saved in AppIdle
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.AppIdle(Sender: TObject; var Done: Boolean);

var
  exit_exists: boolean;

  i: integer;                  // 0.93.a sketchboard..

  indicator_prefix_str: string;      // 212a
  curving_label_prefix_str: string;  // 213a

begin

  try
    if Application.Terminated = True then
      EXIT;

    if pad_form.WindowState <> wsMinimized then
      pad_form_window_state := pad_form.WindowState;   // 0.91.b   for use in AppRestore

    if (on_idle_can_run = False) or (under_way = False) or (prog_running = False) then
      EXIT;

    with pad_form do begin

      if (plain_track = True) or (half_diamond = False) or (tradius_is_straight = True) then
        indicator_prefix_str := ''
      else
        indicator_prefix_str := 'I';    // irregular half-diamond

      if plain_track = True                                    // 212a
      then
        xing_indicator_panel.Font.Color := clSilver
      else
        xing_indicator_panel.Font.Color := clGreen;

      if retpar_i = 1 then
        xing_indicator_panel.Caption := indicator_prefix_str + 'P'    // 212a ...
      else begin
        case xing_type_i of
          -1:
            xing_indicator_panel.Caption := indicator_prefix_str + 'G';   // generic xing
          0:
            xing_indicator_panel.Caption := indicator_prefix_str + 'R';   // regular
          1:
            xing_indicator_panel.Caption := indicator_prefix_str + 'C';   // curviform
        end;//case
      end;


      // 211b moved from math_unit gocalc...

      if slewing = True  // warn him this template includes a slew.
      then begin
        with info_form do begin
          if (plain_track = False) or (slew_mode = 2)
          // min rad info not available for slewed turnouts or any mode 2.
          then begin
            min_rad_box.Hide;
            // so can't focus for min rad change button...
            limit_rad_box.Hide;
            slew_warn_panel.Show;           // show warning in info form.
          end
          else begin
            // OK to show min rad if plain track and mode 1.
            slew_warn_panel.Hide;      // uncover min rad box.
            min_rad_box.Show;          // restore rad change button...
            limit_rad_box.Show;
          end;
        end;//with

        if ABS(slew_angle) > (Pi / 12)  // 15 degrees
        then
          slewing_panel.Font.Color := clYellow
        else
          slewing_panel.Font.Color := clAqua;

        slewing_panel.Show;                // and on pad.

        if (info_form.Left < (slewing_panel.Left + slewing_panel.Width + 4)) and
          (info_form.Top < (slewing_panel.Top + slewing_panel.Height + 4)) then
          info_form.Top := slewing_panel.Top + slewing_panel.Height + 4;
        // ensure warning not obscured by info.
      end
      else begin
        with info_form do begin
          slew_warn_panel.Hide;           // uncover min rad box.
          min_rad_box.Show;               // restore rad change button...
          limit_rad_box.Show;
        end;//with
        slewing_panel.Hide;
      end;


      if distortions = 0 then
        distortion_warning_panel.Hide
      else begin
        distortion_warning_panel.Show;
        if (info_form.Left < (distortion_warning_panel.Left +
          distortion_warning_panel.Width)) and (info_form.Top <
          (distortion_warning_panel.Top + distortion_warning_panel.Height)) then
          info_form.Top := distortion_warning_panel.Top + distortion_warning_panel.Height * 2;
        // ensure warning not obscured by info.
      end;

      if (re_org_x = 0) and (re_org_y = 0) then
        reorg_warning_panel.Hide
      else begin
        reorg_warning_panel.Show;
        if (info_form.Left < (reorg_warning_panel.Left + reorg_warning_panel.Width)) and
          (info_form.Top < (reorg_warning_panel.Top + reorg_warning_panel.Height)) then
          info_form.Top := reorg_warning_panel.Top + reorg_warning_panel.Height * 2;
        // ensure warning not obscured by info.
      end;

      // 0.93.a ...

      delete_to_current_popup_entry.Enabled := classic_templot;
      copy_bg_to_current_popup_entry.Enabled := classic_templot;
      wipe_to_current_popup_entry.Enabled := classic_templot;


      snap_to_EGTP_menu_entry.Enabled :=
        not (plain_track or spiral or slewing or (egpx > (xorg - minfp_big)));
      snap_to_IGTP_menu_entry.Enabled :=
        not (plain_track or spiral or slewing or (igpx > (xorg - minfp_big)));
      crop_approach_menu_entry.Enabled := not (plain_track or (xorg < minfp_big));
      shorten_approach_one_menu_entry.Enabled := not (xorg < minfp_big);
      snap_approach_to_nearest_menu_entry.Enabled := not (xorg < minfp_big);
      snap_approach_to_railjoint_menu_entry.Enabled := not (xorg < minfp_big);

      exit_exists := (plain_track = False) and (turnoutx > (mvjpx + minfp_big));

      snap_exit_to_railjoint_menu_entry.Enabled := exit_exists;
      crop_exit_to_vee_joint_menu_entry.Enabled := exit_exists;
      snap_exit_to_nearest_menu_entry.Enabled := exit_exists;
      shorten_exit_one_menu_entry.Enabled := exit_exists;
      extend_exit_one_menu_entry.Enabled := exit_exists;

      enable_peg_positions;  // update peg menu settings.

      notch_on_radial_centre_menu_entry.Enabled := (ABS(nomrad) < max_rad_test) and (not spiral);
      notch_on_1st_radial_centre_menu_entry.Enabled := (ABS(nomrad1) < max_rad_test) and spiral;
      notch_on_2nd_radial_centre_menu_entry.Enabled := (ABS(nomrad2) < max_rad_test) and spiral;

      make_turnout_road_menu_entry.Enabled :=
        not (plain_track or spiral or slewing or half_diamond);

      if keeps_list.Count < 1 then begin
        toggle_bgnd_menu_entry.Enabled := False;
        group_menu.Enabled := False;                            // 0.82.a
      end
      else begin
        toggle_bgnd_menu_entry.Enabled := True;
        group_menu.Enabled := True;                              // 0.82.a
      end;

      if slewing = True then begin
        curving_label_prefix_str := '~';            // 213a
        slew_nudge_menu_entry.Checked := True;

        adjust_slew_start_menu_entry.Enabled := True;
        adjust_slew_length_menu_entry.Enabled := True;
        adjust_slew_amount_menu_entry.Enabled := True;

        case slew_mode of
          1: begin
            slew_mode1_menu_entry.Checked := True;            // radio item.
            adjust_slew2_factor_menu_entry.Enabled := False;
            slewing_panel.Caption :=
              'caution :  template  contains  a  SLEW  ( mode  1 )';
            info_form.slew_caution_mode_label.Caption :=
              'caution :    this  template  contains  a  SLEW  ( mode  1 )';
          end;
          2: begin
            slew_mode2_menu_entry.Checked := True;            // radio item.
            adjust_slew2_factor_menu_entry.Enabled := True;
            slewing_panel.Caption :=
              'caution :  template  contains  a  SLEW  ( mode  2 )';
            info_form.slew_caution_mode_label.Caption :=
              'caution :    this  template  contains  a  SLEW  ( mode  2 )';
          end;
        end;//case

      end
      else begin
        curving_label_prefix_str := '';             // 213a
        disable_slewing_menu_entry.Checked := True;     // radio item.
        slew_nudge_menu_entry.Checked := False;

        adjust_slew_start_menu_entry.Enabled := False;
        adjust_slew_length_menu_entry.Enabled := False;
        adjust_slew_amount_menu_entry.Enabled := False;
        adjust_slew2_factor_menu_entry.Enabled := False;
      end;

      if spiral = True then begin
        transition_template_menu_entry.Checked := True;    // radio item
        info_form.curving_label.Caption := curving_label_prefix_str + 'tr';
        // 213a
      end
      else
      if ABS(nomrad) < max_rad_test then begin
        constant_radius_menu_entry.Checked := True;  // radio item.
        info_form.curving_label.Caption := curving_label_prefix_str + 'co';
      end
      else begin
        straight_template_menu_entry.Checked := True;  // radio item.
        info_form.curving_label.Caption := curving_label_prefix_str + 'st';
      end;

      if dummy_template = True then
        dummy_template_menu_radio.Checked := True   // radio items  211c
      else
      if cl_only = True then
        centre_lines_only_menu_radio.Checked := True       // radio items  0.93.a mods
      else
      if track_centre_lines_flag = True then
        normal_track_centre_lines_menu_radio.Checked := True
      else
        no_track_centre_lines_menu_radio.Checked := True;   // radio items

      switch_drive_menu_entry.Checked :=
        (switch_drive_flag = True) and (plain_track = False) and (half_diamond = False);  // 0.82.a
      switch_drive_menu_entry.Enabled := (plain_track = False) and (half_diamond = False);
      // 0.82.a

      isolate_crossing_menu_entry.Checked := (isolated_crossing = True) and (plain_track = False);
      // 217a
      isolate_crossing_menu_entry.Enabled := (plain_track = False);
      // 217a

      swings_in_degs_menu_entry.Enabled := ((ABS(nomrad) < max_rad_test) or spiral or slewing);

      adjust_trans_length_menu_entry.Enabled := spiral;  // transition mouse actions..
      adjust_trans_start_menu_entry.Enabled := spiral;

      swell_menu_entry.Enabled := not (spiral or slewing);
      // no swell mouse action for transition curves or slews.

      if plain_track = True then begin
        template_menu_top_entry.Caption := 'PLAIN  TRACK :';
        adjust_turnout_approach_menu_entry.Caption := 'adjust  pl&ain-track  length';
        // F3   205a
        adjust_length_menu_entry.Caption := 'adjust  pl&ain-track  length';
        // F4   205a
      end
      else begin
        if half_diamond = False then
          template_menu_top_entry.Caption := 'TURNOUT :'
        else
          template_menu_top_entry.Caption := 'HALF-DIAMOND :';

        adjust_turnout_approach_menu_entry.Caption := 'adjust  turnout  &approach  length';
        // F3   205a
        adjust_length_menu_entry.Caption := 'adjust  overall  template  l&ength';
        // F4   205a

      end;

      adjust_turnout_approach_menu_entry.Enabled := not half_diamond;   // 205a


      insert_half_diamond_menu_entry.Enabled := plain_track;

      convert_turnout_to_half_diamond_menu_entry.Enabled :=
        (plain_track = False) and (half_diamond = False);
      convert_half_diamond_to_turnout_menu_entry.Enabled := half_diamond;
      snap_to_catch_points_menu_entry.Enabled := (plain_track = False) and (half_diamond = False);
      snap_to_heel_menu_entry.Enabled := (plain_track = False) and (half_diamond = False);
      snap_approach_track_menu_entry.Enabled := not half_diamond;

      // 0.93.a ...

      gaunt_options_menu_entry.Enabled := not (half_diamond or plain_track);
      gaunt_options_menu_entry.Checked := (gaunt_options_menu_entry.Enabled and gaunt);

      gaunt_offset_menu_entry.Enabled := gaunt;
      gaunt_radius_menu_entry.Enabled := gaunt;             // 217a
      gaunt_sleeper_length_menu_entry.Enabled := gaunt;

      normal_non_gaunt_turnout_menu_entry.Checked := not gaunt;   // radio item
      gaunt_turnout_menu_entry.Checked := gaunt;                   // radio item

      if half_diamond = False then
        mint_menu_entry.Caption := '&mint  from  current'
      else
        mint_menu_entry.Caption := '&mint  turnout  from  current';

      if timbinc < minfp    // gradual lengths
      then begin
        timbers_centralized_menu_entry.Checked := False;
        timbers_in_line_menu_entry.Checked := False;
      end
      else begin
        if (ms_ends = False) or (half_diamond = True) then
          timbers_centralized_menu_entry.Checked := True  // radio item.
        else
          timbers_in_line_menu_entry.Checked := True;     // radio item.
      end;

      if no_timbering = True then
        no_timbering_menu_entry.Checked := True  // radio item
      else begin
        if half_diamond = True then
          half_diamond_timbering_style_menu_entry.Checked := True
        else begin
          if timbers_equalized = True then begin
            if equalizing_fixed = False then
              equalized_incremental_menu_entry.Checked := True  // radio item.
            else
              equalized_constant_menu_entry.Checked := True;    // radio item.
          end
          else begin
            if square_on_angled = False then
              square_on_menu_entry.Checked := True              // radio item.
            else
              angled_on_menu_entry.Checked := True;             // radio item. 29-7-01.
          end;
        end;
      end;

      equalized_incremental_menu_entry.Enabled := not half_diamond;
      equalized_constant_menu_entry.Enabled := not half_diamond;
      square_on_menu_entry.Enabled := not half_diamond;
      angled_on_menu_entry.Enabled := not half_diamond;

      half_diamond_timbering_style_menu_entry.Enabled := half_diamond;

      timbers_centralized_menu_entry.Enabled := (timbinc > minfp);
      // (nine_foot=True) or (eight_foot_six=True);
      timbers_in_line_menu_entry.Enabled :=
        (timbinc > minfp) { ((nine_foot=True) or (eight_foot_six=True))} and not half_diamond;

      if platform_form.Showing = True then begin
        platform_buttons;   // 0.93.a update the platforms form
        if (draw_ts_platform = True) or (draw_ms_platform = True) then begin
          adjacent_trackbed_platforms_menu_entry.Checked := True;  // radio item
          adjacent_edges := True;
          do_railedges;
        end;
      end;

      if trackbed_form.Showing = True then begin
        trackbed_buttons;   // 215a update the trackbed form
        if (draw_ts_trackbed_edge = True) or (draw_ms_trackbed_edge = True) then begin
          adjacent_trackbed_platforms_menu_entry.Checked := True;  // radio item
          adjacent_edges := True;
          do_railedges;
        end;
      end;

      // 0.94.a ...

      if shove_timber_form.Showing = True then begin
        shove_timber_form.retain_shoves_on_mint_checkbox.Checked := retain_shoves_on_mint;
        shove_timber_form.retain_shoves_on_make_checkbox.Checked := retain_shoves_on_make;
      end;

      make_tools_retain_shoved_menu_entry.Checked := retain_shoves_on_make;
      make_tools_restore_shoved_menu_entry.Checked := not retain_shoves_on_make;

      if check_diffs_form.Showing = True then begin
        check_diffs_form.retain_diffs_on_mint_checkbox.Checked := retain_diffs_on_mint;
        check_diffs_form.retain_diffs_on_make_checkbox.Checked := retain_diffs_on_make;
      end;

      make_tools_retain_check_rails_menu_entry.Checked := retain_diffs_on_make;
      make_tools_restore_check_rails_menu_entry.Checked := not retain_diffs_on_make;

    end;//with

    if export_form.Showing = True       // 208a
    then begin
      export_form.export_dxf_button.Enabled :=
        not export_form.export_control_template_radiobutton.Checked;
    end;


    if grid_form.Showing = True      // 0.98.a
    then begin
      grid_form.dummy_vehicle_make_copy_button.Enabled :=
        grid_form.show_dummy_vehicles_radio_button.Checked;

      if half_diamond = True then
        grid_form.turnout_road_dummy_vehicle_radio_button.Caption := 'on diagonal road'
      else
        grid_form.turnout_road_dummy_vehicle_radio_button.Caption := 'on turnout road';

      if plain_track = True then begin
        grid_form.main_road_dummy_vehicle_radio_button.Checked := True;
        // radio item
        grid_form.turnout_road_dummy_vehicle_radio_button.Enabled := False;
      end
      else
        grid_form.turnout_road_dummy_vehicle_radio_button.Enabled := True;
    end;

    do_info_colours; // indicate if control template visible and accessible...


    if print_busy = True then
      EXIT; //  do nothing if printer busy using data.

    if data_changed = True       //  only re-calc and draw on data change (from redraw;),
    //  not if error in calc occurred.
    then begin
      if abandon_calcs = True then begin
        case alert(2, '    calculations abandoned',
            '||Sorry, your data confused Templot0. The turnout calculations had to be abandoned.'
            +
            '||( The laws of mathematics prevent a viable turnout being generated for every'
            +
            ' possible combination of the various settings.)||You now have 3 options ...'  ,
            '', '', '', 're-calculate  and  show  diagnostic  information',
            'cancel  -  restart  with  new  B-6  turnout', 'try  nearest  turnout  to  fit', 0)
          of
          4: begin
            draw_mode := 0;    // this allows all the alerts to show.
            calcturnout;
          end;
          5: begin
            error_b6_lh_reset;
          end;
          6:
            pad_form.nearest_menu_entry.Click;
          else
            pad_form.nearest_menu_entry.Click;
        end;//case
      end
      else begin                    // normal redraw on changed data.
        gocalc(2, 0);

        if (do_rollback = True) and (pad_form.Active = True) then
          update_rollback_register;   // maintain the roll-back register.
        do_rollback := True;
        // default for next call.                                      // only one redraw per undo.
      end;
      data_changed := False;    //  not again until flagchange (also set False in gocalc).

    end;

    // sketchboard -- delete dummy zooming rectangle, if any, and not drawing it ...

    { OT-FIRST
  if go_sketchboard=True  // 205a if sketchboard in use
     then begin

            if sb_marker_size_trackbar_changed=True   // 212a  size trackbar on edit_outline_form
               then begin
                      sb_modify_markers_size;

                      sb_marker_size_trackbar_changed:=False;
                    end;

            with dtp_form do begin

              pad_form.sb_show_items_on_pad_menu_entry.Checked:=show_dtp_on_pad_checkbox.Checked;  // 205e

              if dtp_settings_form.display_quality_radiobutton.Checked=True    // 205a
                 then dtp_document.Quality:=sfLanczos                          // display quality slow
                 else dtp_document.Quality:=sfNearest;                         // design quality fast

              if (Assigned(dtp_document)=True) and (Active=True) and (dtp_form.zoom_rectangle_latching_toolbutton.Down=False)
              and (updating_now=False)
                   then begin

                          dtp_document.BeginUpdate;   // otherwise ruler updates on every change.

                          if dtp_document.CurrentPage.ShapeCount>0
                             then begin
                                    i:=0;
                                    while i<dtp_document.CurrentPage.ShapeCount do begin
                                      if Tdtp_shape_tag(dtp_document.CurrentPage.Shapes[i].Tag).zooming_rectangle=True    // dummy zooming rectangle
                                         then dtp_document.CurrentPage.ShapeDelete(i);
                                      INC(i);
                                    end;//while
                                  end;

                          dtp_document.EndUpdate;

                          update_model_rulers;
                        end;
            end;//with

            if ((dtp_form.Active=True) or (dtp_settings_form.Active=True) or (sketchboard_form.Active=True))
               then begin

                      if sketchboard_form.Showing=True
                         then begin
                                dtp_form.copyboard_button.Caption:='hide copyboard';
                                dtp_form.show_copyboard_menu_entry.Caption:='hide  copyboard';
                              end
                         else begin
                                dtp_form.copyboard_button.Caption:='show copyboard';
                                dtp_form.show_copyboard_menu_entry.Caption:='show  copyboard';
                              end;

                      if dtp_settings_form.Showing=True
                         then begin
                                dtp_form.control_panel_button.Caption:='hide control panel';
                                dtp_form.show_control_panel_menu_entry.Caption:='hide  control  panel';
                              end
                         else begin
                                dtp_form.control_panel_button.Caption:='show control panel';
                                dtp_form.show_control_panel_menu_entry.Caption:='show  control  panel';
                              end;

                    //end;

                      sb_update_all_tabs_and_menus;  // 205e now called from here (OnIdle) instead of on changes to document.

                      update_model_rulers;

                    end;//sketchboad showing


            if dtp_settings_form.Showing=True
               then begin
                      with dtp_settings_form do sb_reduced_output_res_width_edit.Enabled:=render_output_reduced_resolution_radiobutton.Checked;
                    end;

          end;//sketchboard in use
  }

    if backup_wanted = True then
      create_backup_file(False);     // sets backup_wanted False;


  finally

    if Application.Active = False then
      Done := True           // not active.
    else
      Done := allow_idle;    // True means abandon time-slice and back to Windows.

  end;//try

end;
//__________________________________________________________________________________________

procedure create_bitmaps;    // come here only once.

begin
  try
    offdraw_bmp := TBitmap.Create;               // drawing for off-screen refresh.
    backdrop_bmp := TBitmap.Create;              // backdrop for unchanging background.

    if he_wants_multiple_monitors = True         // 0.91.b  08-02-07
    then begin
      offdraw_bmp.Width := Screen.DesktopWidth;
      offdraw_bmp.Height := Screen.DesktopHeight;

      backdrop_bmp.Width := Screen.DesktopWidth;
      backdrop_bmp.Height := Screen.DesktopHeight;
    end
    else begin
      offdraw_bmp.Width := Screen.Width;      // single or primary monitor
      offdraw_bmp.Height := Screen.Height;

      backdrop_bmp.Width := Screen.Width;
      backdrop_bmp.Height := Screen.Height;
    end;

  except

    offdraw_bmp.Free;
    backdrop_bmp.Free;

    ShowMessage('Sorry, your computer does not have enough free memory to run ' +
      Application.Title + '.' + #13 + #13 + 'Try restarting ' + Application.Title +
      ' with no other programs running.');  // 0.93.a

    Application.ProcessMessages;

    on_idle_can_run := False;
    under_way := False;
    prog_running := False;

    Application.Terminate;

  end;//try
end;
//___________________________________________________________________________________________

// 0.91.d pref_options...

procedure Tcontrol_room_form.go_prefs_buttonClick(Sender: TObject);

begin
  user_prefs_in_use := True;    // user prefs to be used.
  open_button.Click;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.open_buttonClick(Sender: TObject);

// he clicked GO ...

var
  param_str: string;
  i, index: integer;
  release_txt: TextFile;
  file_str, str: string;
  code_str: string;

  default_file_str: string;  // 206b ...

  { window_handle: HWND; }

  title_str: string;

begin
  if open_button_clicked = True then
    EXIT;  // 205d

  open_button_clicked := True;

  reminder_file_path := Config.GetFilePath(csfiReminder);

  BorderIcons := [];  // 205c prevent him clicking X, wanting only to hide splash, not quit.

  // 205c in case Enter key held down ...

  Enabled := False;

  open_panel.Hide;

  // only one of these is visible ...

  t3_logo_panel.BringToFront;    // OT-FIRST
  tmec_logo_panel.BringToFront;  // OT-FIRST
  ot_logo_panel.BringToFront;    // OT-FIRST

  picture_panel.Visible := True;
  picture_panel.SendToBack;

  Hide;  // hide the form until ready

  ebk1_str := Config.GetFilePath(csfiBackup1); // 1st emergency backup file.
  ebk2_str := Config.GetFilePath(csfiBackup2); // 2nd emergency backup file.
  pb_str := Config.GetFilePath(csfiCopy1);     // copied backup file
  pbo_str := Config.GetFilePath(csfiCopy2);    // copied backup file

  html_path_str := Config.GetDir(csdiHelp);         // for html viewer / local browser.

  // 0.91  ...

  // may need the help viewer during startup, so first thing is to init the history...

  for index := 0 to html_back_c do begin
    with html_back[index] do begin
      src_code := -100;                      // -100 this code means an empty slot.

      src_str := '<FONT COLOR="RED">Sorry, this page slot is empty.</FONT>' +
        '||Please click the &nbsp;`0&lt;`1 &nbsp;or &nbsp;`0&gt;`1 &nbsp;buttons (back or forward) repeatedly until you find your previous pages.' + '||There are 8 cyclic page slots (&nbsp;labelled a&nbsp;-&nbsp;h&nbsp;) in the roll-back register for this Help viewer.' + '||For more help information visit the full indexed web help pages at: <A HREF="go_to_templot_companion.85a">Templot Companion</A>.' + '||To return to the program, click the `0continue`1 or `0hide`1 button below.';
      src_position := 0;
    end;//with
  end;//next

  rc_ok := True;

  with control_room_form do begin

    { OT-FIRST
  if Win32Platform=VER_PLATFORM_WIN32_NT   // running under Windows NT/2000/XP/Vista/8/10 , set 32-bit default...  0.93.a was 24-bit
     then begin}

    graphics_24_bit_limits_menu_entry.Enabled := True;
    graphics_32_bit_limits_menu_entry.Enabled := True;
    graphics_no_limits_menu_entry.Enabled := True;

    max_draw_int := 1000000000;
    //  these are actually less than 31-bit integers for drawing limits...
    min_draw_int := -1000000000;  //  (divide 32-bit by 2 and a bit as safety margin).

    // 0.93.a same defaults for export limits..

    max_export_x := 1000000000;
    min_export_x := -1000000000;

    max_export_y := 1000000000;
    min_export_y := -1000000000;

    graphics_32_bit_limits_menu_entry.Checked := True;      // radio item.

    { OT-FIRST          end
     else begin                    // running under Windows 95/98/ME , set 16-bit default...

            graphics_24_bit_limits_menu_entry.Enabled:=False;
            graphics_32_bit_limits_menu_entry.Enabled:=False;
            graphics_no_limits_menu_entry.Enabled:=False;

            max_draw_int:=16000;        //  these are actually less than 15-bit integers for drawing limits...
            min_draw_int:=-16000;       //  (divide 16-bit by 2 and a bit as safety margin).

                             // 0.93.a same defaults for export limits..

            max_export_x:=16000;
            min_export_x:=-16000;

            max_export_y:=16000;
            min_export_y:=-16000;

            graphics_16_bit_limits_menu_entry.Checked:=True;      // radio item.
          end;
}

    graphics_limits := True;


    // load prefs for startup dialogs only (other prefs loaded later, in case defaults reset on startup).

    if user_prefs_in_use = True                // GO button clicked with prefs
    then
      load_prefs('', True, False, True);  // get file, don't show changes, startup msg prefs only

    // 211b out       if Screen.PixelsPerInch=96 then pad_form.pad_timber_font_label.Font.Size:=7;   // (bitmap font.)


    if Screen.DesktopWidth < 700
    // was .Width 0.91.b  640 x 480 is better with bitmap screen fonts...
    then begin
      stay_visible_form.text_label.Font.Name := 'MS Sans Serif';

      chat_form.chat_box.Font.Name := 'MS Sans Serif';
      alert_box.Font.Name := 'MS Sans Serif';
      switch_select_form.switch_selector_listbox.Font.Name := 'MS Sans Serif';
      xing_select_form.xing_angle_listbox.Font.Name := 'MS Sans Serif';
      xing_select_form.xing_geo_listbox.Font.Name := 'MS Sans Serif';
      plain_track_form.plain_track_spacings_listbox.Font.Name := 'MS Sans Serif';
    end;

    check_colours;     //  get hi_color (more than 8-bit) True or False for screen colours.

    if hi_color = False                   //  8-bit (or lower)
    then begin                       //  set the form colours for lo-colour...

      chat_form.Color := clBtnFace;
      metric_form.Color := clBtnFace;
      plain_track_form.Color := clBtnFace;
      info_form.Color := clBtnFace;
      switch_select_form.Color := clGray;
      xing_select_form.Color := clGray;

      keep_form.Color := clBtnFace;
      keep_form.keepform_listbox.Color := clWhite;

      gauge_form.Color := clGray;
      dxf_form.Color := clBtnFace;
      bgnd_form.Color := clBtnFace;
      bgkeeps_form.Color := clBtnFace;
      panning_form.Color := clGray;
      action_form.Color := clSilver;
      grid_form.Color := clGray;
      shove_timber_form.Color := clSilver;
      print_form.Color := clBtnFace;

      mint_form.Color := clBtnFace;

      jotter_form.jotter_memo.Color := clWhite;

      // avoid colours lime, yellow, aqua, white, which are used for the response bars.

      alert_colour[0] := clBlack;         // proram alert
      alert_colour[1] := clBtnFace;       // warning
      alert_colour[2] := clBtnFace;       // information
      alert_colour[3] := clBtnFace;       // handy hint
      alert_colour[4] := clGray;          // question
      alert_colour[5] := clSilver;        // error
      alert_colour[6] := clGray;          // invalid request
      alert_colour[7] := clBtnFace;       // confirm
      alert_colour[8] := clWhite;         // spare

    end
    else begin  // hi-colour (more than 8-bit) ...

      stay_visible_form.help_scrollbox.Color := $00FFE0B0;   //  sky blue
      start_colours := 0;
      // hi-color, so normal scheme anyway.

    end;

    if initdone_flag = False then
      templot_init;       //  only initialise once.

    //and start up on the pad...

    if (Screen.MonitorCount > 1) and (multi_monitors_msg_pref = False)     //%%%%
    then begin
      alert_box.preferences_checkbox.Checked := False;       //%%%%
      if user_prefs_in_use = True then
        alert_box.preferences_checkbox.Show;

      if alert(7, '   Templot0   -   Multiple  Monitors',
        'Your computer is currently set to use multiple monitors.' +
        '||Do you want the Templot0 trackpad to allow use of the full screen area across all monitors?'
        + '||A small margin will be allowed initially at the right and bottom borders for easier resizing to a smaller area.'
        + '||Be aware that if you answer yes, Templot0 will use more memory and the screen response may be slower on older systems.'
        + ' For the fastest screen response, arrange the primary monitor to be on the left.' +
        '||If you answer no, the maximum usable size of the Templot0 trackpad will be restricted to match your primary monitor only.'
        +
        ' If you then extend the trackpad beyond that, part of the trackpad area will be unusable.| ',
        '', '', '', 'no  -  match  primary  monitor  only', '', 'yes  -  use  all  available  monitors',
        0) = 6 then begin
        he_wants_multiple_monitors := True;
        log.Info('Selected multiple monitors');
      end
      else begin
        log.Info('Selected primary monitor only');
      end;

      multi_monitors_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
      alert_box.preferences_checkbox.Hide;
    end;

    if (GetKeyState(VK_CAPITAL) and 1) <> 0    // caps lock on at startup...
    then begin
      alert(3, '   Templot0   -   Caps Lock  ON',
        'Your computer currently has the CapsLock ON (keyboard indicator light showing).' +
        '||While the Caps Lock is on Templot0 will highlight any background templates on the drawing as the mouse pointer'
        +
        ' passes over their name labels.' +
        '||If you have just started Templot0 for the first time you may find this confusing.' +
        '||You can prevent this by switching the Caps Lock OFF. To do this press the  `0CAPS LOCK`2  key once so that the indicator light is no longer showing.' + '||Please click the green OK bar to continue.',
        '', '', '', '', '', 'O K', 0);
    end;


    create_bitmaps;
    if Application.Terminated = True then
      EXIT;   // he quit on "no memory for bitmaps" alert

    full_draw := True;


    // do the first draw...

    if (he_wants_multiple_monitors = True) or (running_under_wine = True)
    // monitors 0.91.b,   Wine 212a to allow dragging to find hidden modal dialogs
    then begin
      pad_form.Left := Screen.DesktopLeft;
      pad_form.Top := Screen.DesktopTop;
      pad_form.Width := Screen.DesktopWidth - 16;    // initial margins for easier resizing.
      pad_form.Height := Screen.DesktopHeight - 50;  // allow for taskbar.
    end
    else
      pad_form.WindowState := wsMaximized;           // single or primary monitor.


    do_toolbars;    // 217a

    set_pad_start_colours(False);      //  set up the colours (not if already done).

    pad_form.Show;                     //  ready to start,

    repeat
      if Application.Terminated = False then
        Application.ProcessMessages     //  make sure the pad is showing,
      else
        BREAK;
    until pad_form.Showing = True;

    pad_form.Menu := pad_form.pad_menu_bar;         //  and add the menu bar.

    if Application.Terminated = False then
      Application.ProcessMessages;

    panning_form.Left := pad_form.ClientWidth - panning_form.Width - 10;  // 0.95.a
    panning_form.Show;
    //13-12-99   (now a child form).

    if Screen.Width <= (make_slip_form.Width + pad_form.top_toolbar_panel.Width) then
      make_slip_form.Hide;  // see do_toolbars

    //  and draw the default turnout...

    draw_mode := 0;             // setup for initial screen scaling calcs, allow alerts, etc.
    gocalc(2, 0);              // clear previous drawing, do the calcs, and draw it.

    pad_form.lock_scaling_menu_entry.Click;   // now ready for adjusts.

    info_form.Top := 80;
    // 216a   deeper toolbar    was 60 0.91.d     //was as above. problems with offset multiple monitors.
    info_form.Left := 20;  // 0.93.a

    info_form.shrink_button.Click;                //  and shrunk;
    pad_form.show_info_menu_entry.Click;          //  put up the info panel.

    pad_form.unlock_both_popup_entry.Click;       //  init locking;
    under_way := True;                              //  now can permit pad re-paints, etc.

    if FileExists(pbo_str) = False then
      first_time_here_label.Show;  // 0.97.b  // beginner first few times (no prior-previous)

    init_rollbacks;         // also inits notch rollback and parking bays.

    if (FileExists(ebk1_str) = True) and (FileExists(ebk2_str) = True)
    // faulty backup (e.g. power failure while writing disk?)
    then begin
      if FileAge(ebk1_str) > FileAge(ebk2_str) then
        DeleteFile(ebk1_str)     // so delete the latest.
      else
        DeleteFile(ebk2_str);
    end;

    // make new prior-previous file if we have an existing previous file and will have a new previous file...

    if ((FileExists(ebk1_str) = True) or (FileExists(ebk2_str) = True)) and
      (FileExists(pb_str) = True) then begin
      DeleteFile(pbo_str);         // delete any old prior-previous file.
      RenameFile(pb_str, pbo_str);
      // the previous data becomes prior-previous, and we copy the backup file on startup as new previous.
    end;

    // make new previous file if we have data...

    if FileExists(ebk1_str) = True then
      CopyFile(PChar(ebk1_str), PChar(pb_str), False)
    // copy previous backup file as new previous box file.
    else
    if FileExists(ebk2_str) = True then
      CopyFile(PChar(ebk2_str), PChar(pb_str), False)
    else
    if FileExists(pb_str) = True
    // or no backups there (quit with empty box?), so create it from existing previous box file.
    then
      CopyFile(PChar(pb_str), PChar(ebk1_str), False);

    load_backup_file;             // get any backup wanted.
    create_backup_file(False);    // update a new file (or delete it if he didn't want the backup).

    Tag := 1;         // flag we are all done.

    blue_corner_panel.Show;

    Caption := '        ' + Application.Title + '       program  panel';

    Menu := control_room_menu_bar;       //  and now safe to show the main menu.

    if (ParamCount > 0)    // command line parameters?
    then begin
      param_str := ParamStr(ParamCount);
      if ExtractFileExt(param_str) = '.box3' then begin
        if FileExists(param_str) = True then
          reload_specified_file(True, False, param_str);
      end
      else begin
        if ExtractFileExt(param_str) = '.bgs3' then begin
          if FileExists(param_str) = True then
            load_shapes(param_str, False, False, False);
        end
        else begin
          if ExtractFileExt(param_str) = '.sk9' then begin
            { OT-FIRST if FileExists(param_str)=True then load_sketchboard_file(param_str);}
          end;
        end;
      end;
    end;


    update_rollback_register;   // so can roll back to the T-55 startup.

    for i := 0 to 2 do
      fill_kd(parking_bay[i]);   // ditto initial fill all parking bays per current pad.

    on_idle_can_run := True;

    if user_prefs_in_use = True
    // GO button clicked with prefs, file already got (for startup messages).
    then begin
      load_prefs('', False, True, False); // get_file,show_them,startup_msg_prefs

      if prefs_warn_msg_pref = False then begin
        alert_box.preferences_checkbox.Checked := False;       //%%%%
        alert_box.preferences_checkbox.Show;

        if alert(3, '   Templot0  started  using  saved  program  preferences',
          ' |Templot0 started using saved program preferences.' +
          '||If you are a new or infrequent user of Templot0 please be aware that running Templot0 using saved program preferences may mean that you won''t see important help messages and hints.' + '||If you experience difficulty you may prefer to quit Templot0 and restart without using saved preferences (by clicking the initial GO button).| ', '', '', '', 'more  information', '', 'O K', 4) = 4 then
          saved_preferences_setup_menu_entry.Click;

        prefs_warn_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
        alert_box.preferences_checkbox.Hide;
      end;
    end;

  end;//with control_room

  control_room_form.Enabled := True;

  with control_room_form do begin
    pad_button.Show;
    control_sketchboard_button.Show;  // 0.93.a
    storage_box_button.Show;
    reload_button.Show;
    recent_button.Show;
    clear_button.Show;
    viewer_button.Show;
    about_label.Show;

    pad_button.Enabled := True;
    control_sketchboard_button.Enabled := True;  // 0.93.a
    storage_box_button.Enabled := True;
    reload_button.Enabled := True;
    recent_button.Enabled := True;
    clear_button.Enabled := True;
    viewer_button.Enabled := True;
    about_label.Enabled := True;

    statusbar_panel.Enabled := True;      // 211b

    // OT-FIRST color:=$0066EEBB;  // 214a lime green

    //Color:=$00FFEECC;  // OpenTemplot  water blue

    //Color:=$00A8FFF8;  // TemplotMEC  unripe banana

    Show;

    if pad_button.Showing = True then
      pad_button.SetFocus;

  end;//with

  // 206b if nothing loaded, load the default files...

  default_file_str := Config.GetFilePath(csfiStartBox);

  if (gauge_i = t_T55_i) and (keeps_list.Count < 1) and (FileExists(default_file_str) = True)
  // T-55 startup check in case box is empty after reloading file having control template only.
  then
    reload_specified_file(False, False, default_file_str);

  default_file_str := Config.GetFilePath(csfiStartBgnd);

  if (bgnd_form.bgnd_shapes_listbox.Items.Count < 1) and (FileExists(default_file_str) = True) then
    load_shapes(default_file_str, False, False, False);

  if running_under_wine = True then begin
    control_room_form.use_Wine_fonts_menu_entry.Enabled := True;
    data_entry_form.wine_label.Visible := True;

    startup_modal_warning(False);  // 212b
  end;

  get_custom_gauges;  // load custom gauge/scale from previous session   215a in prefs_unit
  {
  redraw(False);

  Application.ProcessMessages;

  title_str:='TemplotMEC';
  window_handle:=FindWindow(nil,PChar(title_str));

  title_str:=Application.Title;
  if window_handle=0
     then window_handle:=FindWindow(nil,PChar(title_str));

  title_str:=Application.ExeName;
  if window_handle=0
     then window_handle:=FindWindow(nil,PChar(title_str));

  if window_handle<>0
     then ShowWindow(window_handle,SW_MINIMIZE);
}
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.use_wine_fonts_menu_entryClick(Sender: TObject);

var
  i, j: integer;
  temp_comp_i, temp_comp_j: TComponent;

begin
  arial_str := 'Liberation Sans';   // for Wine, in place of Arial

  for j := Application.ComponentCount - 1 downto 0 do begin

    temp_comp_j := Application.Components[j];

    if (temp_comp_j is TForm) then begin

      TForm(temp_comp_j).Font.Name := 'Liberation Sans';

      for i := temp_comp_j.ComponentCount - 1 downto 0 do begin

        temp_comp_i := temp_comp_j.Components[i];


        if (temp_comp_i is TButton) then begin
          if TButton(temp_comp_i).Font.Name = 'Arial' then
            TButton(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TPanel) then begin
          if TPanel(temp_comp_i).Font.Name = 'Arial' then
            TPanel(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TLabel) then begin
          if TLabel(temp_comp_i).Font.Name = 'Arial' then
            TLabel(temp_comp_i).Font.Name := 'Liberation Sans';

          TLabel(temp_comp_i).AutoSize := False;
        end;


        if (temp_comp_i is TGroupBox) then begin
          if TGroupBox(temp_comp_i).Font.Name = 'Arial' then
            TGroupBox(temp_comp_i).Font.Name := 'Liberation Sans';
        end;

        if (temp_comp_i is TRadioGroup) then begin
          if TRadioGroup(temp_comp_i).Font.Name = 'Arial' then
            TRadioGroup(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TCheckBox) then begin
          if TCheckBox(temp_comp_i).Font.Name = 'Arial' then
            TCheckBox(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TRadioButton) then begin
          if TRadioButton(temp_comp_i).Font.Name = 'Arial' then
            TRadioButton(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TTabSheet) then begin
          if TTabSheet(temp_comp_i).Font.Name = 'Arial' then
            TTabSheet(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TPageControl) then begin
          if TPageControl(temp_comp_i).Font.Name = 'Arial' then
            TPageControl(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TMemo) then begin
          if TMemo(temp_comp_i).Font.Name = 'Arial' then
            TMemo(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TEdit) then begin
          if TEdit(temp_comp_i).Font.Name = 'Arial' then
            TEdit(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TListBox) then begin
          if TListBox(temp_comp_i).Font.Name = 'Arial' then
            TListBox(temp_comp_i).Font.Name := 'Liberation Sans';
        end;


        if (temp_comp_i is TComboBox) then begin
          if TComboBox(temp_comp_i).Font.Name = 'Arial' then
            TComboBox(temp_comp_i).Font.Name := 'Liberation Sans';
        end;

      end;//next i
    end;

  end;//next j

  alert_box.alert_html_viewer.DefFontName := 'Liberation Sans';
  help_form.html_view.DefFontName := 'Liberation Sans';
  mint_form.mint_html_view.DefFontName := 'Liberation Sans';

  pad_form.redo_toolbutton.Font.Name := 'Liberation Sans';
  pad_form.undo_toolbutton.Font.Name := 'Liberation Sans';

  pad_form.redo_toolbutton.Font.Style := [fsBold];
  pad_form.undo_toolbutton.Font.Style := [fsBold];

  pad_form.redo_toolbutton.Caption := '•>';
  pad_form.undo_toolbutton.Caption := '<•';


  pad_form.fit_current_toolbutton.Font.Name := 'Liberation Sans';
  pad_form.fit_current_toolbutton.Font.Style := [fsBold];
  pad_form.fit_current_toolbutton.Caption := '•';


  pad_form.store_bgnd_insert_toolbutton.Font.Name := 'Liberation Sans';

  // 211c now a bitmap (added 211b)..

  panning_form.previous_view_button.Font.Name := 'Liberation Sans';
  panning_form.next_view_button.Font.Name := 'Liberation Sans';

  panning_form.previous_view_button.Font.Style := [fsBold];
  panning_form.next_view_button.Font.Style := [fsBold];

  panning_form.previous_view_button.Caption := '<<<';
  panning_form.next_view_button.Caption := '>>>';

  info_form.info_memo.Font.Name := 'Liberation Sans';

  data_entry_form.wine_label.Font.Name := 'Liberation Sans';

end;
//______________________________________________________________________________

procedure Tcontrol_room_form.about_templot_version_menu_entryClick(Sender: TObject);

var
  about_str: string;
  logo_img_path: string;

begin

  if Application.Title = 'TemplotMEC'
  then
    logo_img_path := Config.GetFilePath(csfiTMlogo)
  else
  if Application.Title = 'OpenTemplot' then
    logo_img_path := Config.GetFilePath(csfiOTlogo)
  else
    logo_img_path := Config.GetFilePath(csfiT3logo);

  about_str := '<P STYLE="text-align:center; margin-top:20px;"><IMG SRC="' +
    logo_img_path + '"></P>' +
    '<P STYLE="text-align:center; margin-top:20px; color:blue; font-family:''Trebuchet MS''; font-size:19px; font-weight:bold; font-style:italic;">precision track design for model railways</P>' + '<P STYLE="text-align:center; margin-top:20px; margin-bottom:20px; color:#dd6600; font-size:16px; font-weight:bold;">track &nbsp;plan &nbsp;design&nbsp; &nbsp; • &nbsp; &nbsp;precision &nbsp;construction &nbsp;templates</P>' + '<HR NOSHADE>' + '<P CLASS="mainheading" STYLE="text-align:center; font-size:20px; color:#0077DD;">' + Application.Title + ' &nbsp;Version &nbsp;' + GetVersionString(voFull) + '</P>' + '<P CLASS="centerbold"><A HREF="go_to_templot_com.85a">templot • com</A></P>' + '<P CLASS="center"><SPAN STYLE="font-size:12px; color:#555555;">&copy; 2018 &nbsp;released under open-source licence: GNU/GPLv3+<br>program from: &nbsp;https://sourceforge.net/projects/opentemplot/<br>' + 'licence at: https://www.gnu.org/licenses/<br></SPAN></P>' + '<P STYLE="font-size:12px; color:#555555;">' + licence_str + '</P>';

  no_new_help_sizes := True;      // don't change the user's default sizes.

  if Screen.PixelsPerInch > 120      // 211b
  then begin
    help_form.ClientWidth := 742;     //  *1.4 ..
    help_form.ClientHeight := 952;
  end
  else begin
    help_form.ClientWidth := 530;
    help_form.ClientHeight := 680;
  end;

  resize_help_form;

  if help(-250, about_str, 'make  a  donation') = 1 then
    go_to_donation;

  no_new_help_sizes := False;    // allow user drag resize again.
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

var
  i: integer;

begin
  if quit_alert_done = False then
    quit_alert_done := quit_alert;     //  ask if o.k to close

  // finally prefs (in case changed in quit_alert)...

  if (quit_code = 3) and (quit_alert_done = True)        // normal quit..
  then
    CanClose := prefs_quit                       // so go save preferences
  else
    CanClose := quit_alert_done;                 // abnormal quit, or he cancelled in quit_alert.

  if CanClose = True then begin
    if under_way = True then begin
      { OT-FIRST
                      if Assigned(dtp_form.dtp_document)=True     // 0.93.a ...
                         then begin
                                if dtp_form.dtp_document.Modified=True
                                   then begin
                                          i:=alert(7,'php/501    save  sketchboard ?',
                                                     ' |Save sketchboard?'
                                                    +'||There are unsaved changes on the sketchboard which will be lost if not saved before you quit Templot0.'
                                                    +'||Do you want to save them to a file before quitting?| ',
                                                     '','','','no  -  quit  Templot  now','','yes  -  save  sketchboard  file  and  quit',0);
                                          //if i=5 then CanClose:=False;
                                          if i=6 then dtp_form.save_dtp_as_menu_entry.Click;
                                        end;
                              end;}

      if CanClose = True then begin
        with jotter_form.jotter_memo.Lines do begin
          if Count > 0 then
            SaveToFile(Config.GetFilePath(csfiJotterBkp));  // jotter text for restore.
        end;//with

        with boxmru_list do begin
          if Count > 0 then
            SaveToFile(Config.GetFilePath(csfiBoxMRU));   // 0.82.a 22-08-06 mru list for box files restore.
        end;//with

        with bgsmru_list do begin
          if Count > 0 then
            SaveToFile(Config.GetFilePath(csfiBgndMRU));   // 0.82.a 22-08-06 mru list for bgs files restore.
        end;//with

        save_custom_gauges;
        // save custom gauge/scale for next session   215a   in prefs_unit

        info_form.Close;
        //  !!! bug in Delphi - address violation if child forms overlapping the bottom of the pad on quit.
        grid_form.Close;
        shove_timber_form.Close;
        stay_visible_form.Close;
        panning_form.Close;
        action_form.Close;
        platform_form.Close;
        data_child_form.Close;
        check_diffs_form.Close;
        rail_options_form.Close;  // 206b
        trackbed_form.Close;      // 215a
        make_slip_form.Close;     // 217a

      end;
    end;
    //  !!! above lines not needed if pad_form.autoscroll is false.
    if CanClose = True then
      under_way := False;          //  no more redraws
  end;
  closing_flag := CanClose;   //  Templot closes if true but we keep our own flag.
end;
//____________________________________________________________________________________________

procedure Tcontrol_room_form.quit_menu_entryClick(Sender: TObject);

begin
  quit_code := 3;               //  flag.
  control_room_form.Close;    //  this is the main form.
  // the application closes (via the quit query alert).
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.chat_panelClick(Sender: TObject);

const
  chat_str: string = '      Program  Panel  Chat' +
    '||Clicking the green  " = "  buttons in Templot0 brings up this "chat box",' +
    ' containing some words from me, invariably of no great consequence!' + '||Martin.';

begin
  chat(chat_str);
end;
//____________________________________________________________________________________________

procedure Tcontrol_room_form.how_panelClick(Sender: TObject);

const
  help_str: string = '            Program  Panel.' +
    '||This window is called the `0program panel`3.' +
    '||If you have just started TEMPLOT for the first time, click the `0trackpad`1 button to get going, and then click the `0help`1 menu items on the trackpad.' + ' Later on, you will probably start by clicking the `0RECENT FILES`1 or `0FILE...`1 buttons which load or reload your template storage box with the templates from a previously saved data file.' + '||The `0SESSION`1 menu items are functions which you normally need to access only once per working session. You can set the track gauge and modelling scale' + ' which you wish to use, and choose a title for the project.' + '||You can return to the program panel at any time by clicking the `0MAIN > PROGRAM PANEL`1 menu item on the trackpad.' + ' The program panel can be kept in view by resizing the trackpad window. Closing the program panel will quit Templot0.' + '||The `0PROGRAM`1 menu includes various options for experienced users to customise the way Templot0 works.' + '||green_panel_begin tree.gif Until you become an experienced user of Templot0 you will probably have little use for the program panel, as most of the menu items are repeated elsewhere in Templot0.green_panel_end';

begin
  help(-1, help_str, '');
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.FormResize(Sender: TObject);

// manual drag resizing only...

begin
  if (initdone_flag = False) or (room_scaling = True) then
    EXIT;   // not if resizing from scaling buttons or for dpi-aware at startup

  statusbar_panel.Width := ClientWidth;                          // 211b  now alNone
  statusbar_panel.Top := ClientHeight - statusbar_panel.Height;
end;
//____________________________________________________________________________________________

procedure Tcontrol_room_form.project_title_menu_entryClick(Sender: TObject);

begin
  with math_form do begin
    Caption := '    project  title ...';
    big_label.Caption := insert_crlf_str('|      Project  Title' +
      '||Enter below a title for your track plan project.' +
      '||This will appear on each printed template page.' +
      '||This name will also be shown in the title bar of your storage box.' +
      '||When reloading the box from a file, the title will be changed to the name under which the box contents were saved in the file.' + ' If you want to use a different name, change it after reloading, not before.' + '||If you leave the box blank, the name "untitled project" will be used.');
    math_editbox.Text := box_project_title_str;

    do_show_modal(math_form);    // 212a

    if ModalResult = mrOk then begin
      box_project_title_str := Trim(math_editbox.Text);
      if box_project_title_str = '' then
        box_project_title_str := 'untitled project';
      save_done := False;
      backup_wanted := True;
    end;

    Caption := '    ' + Application.Title;   // reset form caption.
  end;//with
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.printer_setup_menu_entryClick(Sender: TObject);

begin
  pad_form.printer_setup_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.pad_buttonClick(Sender: TObject);

begin
  first_time_here_label.Hide; // 0.97.b

  pad_form.Show;

  cancel_adjusts(False);

  redraw_pad(False, False);  // direct draw (not via idle loop in case not running yet).
end;
//_______________________________________________________________________________________

procedure Tcontrol_room_form.printer_calibration_menu_entryClick(Sender: TObject);

begin
  pad_form.calibrate_printer_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.colour_buttonClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  program  panel', Color);
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.new_gauge_menu_entryClick(Sender: TObject);

begin
  pad_form.other_gauges_menu_entry.Click;
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.auto_terms_menu_entryClick(Sender: TObject);

begin
  auto_terms_menu_entry.Checked := True;       // radio item.
  trans_auto := True;
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.custom_terms_menu_entryClick(Sender: TObject);

const
  help_str: string = '      Expert  Help  -  Transition Maths' +
    '||      Custom  Number  of  Terms' +
    '||If the normal AUTO TERMS setting is not producing the desired results,' +
    ' use this form to set a custom number of terms which Templot0 will use in expanding the Ideal Transition Spiral.'
    + ' Using more terms will produce a more accurate transition curve, but may significantly slow down the screen'
    + ' redraw process, especially for older processors.' +
    '||The recommendations are as follows:' +
    '||For a swing of 30 degrees or less: use at least 3 terms.' +
    '|For a swing of 30 degrees to 60 degrees: use at least 4 terms.' +
    '|For a swing of 60 degrees to 90 degrees: use at least 6 terms.' +
    '|For a swing of 90 degrees to 180 degrees: use at least 8 terms.' +
    '|For a swing exceeding 180 degrees: use at least 20 terms.' +
    '||Where the "swing" is the angle turned through along the length of the transition section.'
    +
    '||To draw several turns of an ever-decreasing spiral, use at least 40 terms, and be prepared to wait several'
    + ' seconds for each re-draw.' +
    '||( The normal AUTO TERMS setting is restored on a B-6 TURNOUT RESET.)' +
    '||Please note that the above recommendations are a very rough guide only but have proved useful in practice.'
    + ' In general, for a transition in which the radius changes significantly in a short length, use fewer terms;'
    + ' for a transition where the radius changes little in a longer length, use more terms.' +
    '||If you get strange results or sudden kinks in the transition - use a few more terms.' +
    '||The preset number of custom terms is 8, which should be sufficient in the majority of cases.'
    + '||You should be aware that if you use a great many terms, the calculation capabilities of your system may be exceeded.'
    + ' This will not cause any harm, but may give unpredictable results, or cause Windows to terminate the running of Templot0.'
    + ' For this reason, please save your previous work before experimenting. To minimise the possibility of this happening,'
    + ' Templot0 actually performs the transition calculations in kilometres, and does its best to recover gracefully from'
    + ' an overflow condition.' +
    '||If you are unsure about how to proceed, please revert to the normal AUTO TERMS option which will draw satisfactory'
    + ' transitions in the vast majority of cases which arise in actual practice.';

var
  i: integer;
  n: integer;
  od: Toutdim;    // [0..7] array of double;
  revert_auto: boolean;

begin
  revert_auto := False;
  custom_terms_menu_entry.Checked := True;     // radio item.
  trans_auto := False;

  repeat
    i := alert(4, '      transition - custom  terms ...',
      'Select the custom number of terms to be used in expanding the transition spiral.' +
      ' The current custom setting is ' + IntToStr(trans_terms) +
      ' terms. You should have a good reason for not using the normal' +
      ' AUTO TERMS setting.', 'important  information  -  please  read',
      'enter  some  other  setting', '3  terms  -  30  degree  swing',
      '4  terms  -  60  degree  swing', 'cancel  -  use  AUTO  TERMS  instead',
      '6  terms  -  90  degree  swing', 1);

    case i of
      1:
        if alert_help(0, help_str + '||Do you want to do this now ?', 'yes  AUTO  TERMS') = 1 then
        begin
          revert_auto := True;
          auto_terms_menu_entry.Click;
        end;
      2: begin
        n := putdim(help_str, 0, 'number of transition terms', trans_terms,
          True, False, True, False);
        // no neg, preset ok, no zero, don't terminate on zero.
        if n <> 0 then
          EXIT;

        if getdims('transition  terms', '', control_room_form, n, od) = True then begin
          if od[0] = def_req then
            trans_terms := 8                  // default 8 transition terms.
          else
            trans_terms := Round(ABS(od[0]));
        end;
      end;
      3:
        trans_terms := 3;
      4:
        trans_terms := 4;
      5:
        auto_terms_menu_entry.Click;
      6:
        trans_terms := 6;
      else
        EXIT;
    end;//case
  until (i <> 1) or (revert_auto = True);
end;
//___________________________________________________________________________________________

procedure Tcontrol_room_form.run_slow_menu_entryClick(Sender: TObject);

const
  slow_str: string = '      Slow  Running' +
    '||You can slow down Templot0''s drawing functions by setting a count figure greater than zero.'
    + '||This is useful if an error has occurred and you need to check each feature as it is drawn.'
    + '||A suitable count for your computer will need to be established by trial and error. Higher counts cause Templot0 to run more slowly.' + ' A count in the range 20-100 would be a starting point.' + '||To properly see the effect of slow running you will need to be using the LOW MEMORY refresh mode, which requires a Templot0 restart.' + '||N.B. ! Remember to come back here and reset the count to zero after use !' + '||The count is reset to zero on a B-6 turnout reset, or when printing.';

var
  n: integer;
  od: Toutdim;

begin
  n := putdim(slow_str, 0, 'slow  running  count', slow_run, True, False, False, False);
  // no neg, preset ok, zero ok, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('slow  running', '', control_room_form, n, od) = True then begin
    if od[0] = def_req then
      slow_run := 0
    else
      slow_run := ABS(od[0]);
  end;

  if slow_run = 0 then
    run_slow_menu_entry.Checked := False
  else
    run_slow_menu_entry.Checked := True;
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.max_spiral_menu_entryClick(Sender: TObject);

const
  max_spiral_str: string = '    Expert  Help  -  Max  Spiral' +
    '||There is a limit to the "gentleness" of transition curves (spirals) which can be calculated. A transition which is too gentle (i.e. too small a change of radius and/or in too long a transition zone)' + ' is likely to be distorted, possibly severely.' + '||Templot0 therefore limits transitions to a maximum "gentleness", i.e. a maximum value for the "spiral constant" showing in the INFORMATION panel.' + '||You can change the value of this limit according to the capabilities of your processor and your acceptable distortion. Experience suggests that AMD processors (Athlon, Duron) can accept a higher limit than Pentium processors.' + '||The pre-set limit is 500. Higher figures will allow more gentle transitions, but also more distortion.' + '||If distortion is a problem, try reducing the smaller radius slightly. Often only a small change is needed. Shortening the transition zone length can also be tried, but in this case a more significant reduction may be needed.' + '||CAUTION: Setting too high a limit could cause Templot0 to appear to "hang" when adjusting transition curves by mouse action. A figure in excess of 2000 is unlikely to produce a usable template.' + ' An alternative to a very gentle transition is to use a series of constant radius templates, each varying very slightly from the next in radius. In some cases a small amount of slew will also give the desired result.';

var
  n: integer;
  od: Toutdim;

begin
  n := putdim(max_spiral_str, 0, 'max  spiral  constant', max_spiral_constant /
    1.0E6, True, False, False, False);
  // no neg, preset ok, zero ok, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('max  spiral', '', control_room_form, n, od) = True then begin
    if od[0] = def_req then
      max_spiral_constant := 500.0E6
    else
      max_spiral_constant := ABS(od[0] * 1.0E6);
  end;
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.step_size_menu_entryClick(Sender: TObject);

const
  help_str: string = '      Expert  Help  -  Step  Size' +
    '||Templot0 calculates the alignment of the rails by stepping along them. The size of each step can be changed, although there is normally' + ' no reason to do this. The pre-set step size is calculated as the square root of the scale equivalent of 9 feet in mm. For example, in' + ' 4 mm scale, 9 feet is 36 mm, and the step size is therefore 6 mm. In 7 mm scale the step size is the square root of 63, i.e. approximately 8 mm.' + '||If you are having problems with a slow processor, try setting a significantly larger step size to reduce the amount of calculations required for each' + ' screen re-draw. If you set a very large step size, remember to return to the pre-set step size before printing the template, otherwise your curved rails' + ' will be noticeably dog-legged. A sensible maximum step size is about 8ft scale (32 mm in 4mm scale), beyond which the drawing is likely to become significantly disfigured.' + '||There is no advantage in using a step size smaller than the pre-set unless you are printing very sharp curves on an extremely high resolution printer, or using the print' + ' output scaling function to enlarge the drawing by several hundred percent.' + '||A radius of 500 mm would need to be printed at more than 2500 dpi ( 8 times better than many inkjets ) to resolve the difference between a' + ' perfect curve and one drawn in 6 mm steps. And a radius of 900 mm ( a practical minimum in 4 mm scale ) would require better than 5000 dpi.' + ' Do not be misled by the advertised resolution of some printers, which refers to their image manipulation software - only very expensive models can be' + ' expected to physically position the print head to better than 600 dpi.' + '||If you do set a smaller step size, the extra calculations required may slow down your screen re-draws, or lead to memory problems.' + '||The step size used is saved as part of the template specification in the storage box and re-used on reloading. The pre-set step size is' + ' restored when you do a B-6 TURNOUT RESET or change the scale.' + '||The range of permitted step size is from 3" scale to 25ft scale ( 1-100 mm in 4mm scale ).';

var
  i, n: integer;
  od: Toutdim;
  m: double;

begin
  repeat
    m := incx;
    n := putdim(help_str, 1, 'calculation step size along rail-lines', m, True,
      False, True, False);
    // no neg, preset ok, no zero, don't terminate on zero.
    if n <> 0 then
      EXIT;
    if getdims('step  size', '', control_room_form, n, od) = True then begin
      m := od[0];
      if m = def_req then
        Break;

      if (m < (scale / 4)) or (m > (25 * scale)) or (m < 0.01)
      // 3" to 25 ft max sensible range (1-100 mm for 4 mm scale).
      then begin
        i := alert(0, '', '||The step size is outside the permitted range :||  ' +
          round_str(scale / 4, 2) + '  to  ' + round_str(25 * scale, 2) + '  mm',
          '', '', '', '', 'cancel', 're-try', 0);
        if i = 5 then
          EXIT;
      end
      else
        Break;
    end
    else
      Break;
  until 0 <> 0;

  if ABS(incx - m) > minfp then
    incx := m;     // set any new incx.

  turnoutx_max := xy_pts_c * incx;           // limit overall length.

  if turnoutx > turnoutx_max then
    turnoutx := turnoutx_max;
  if xorg > turnoutx then
    xorg := turnoutx;

  redraw(False); // 0.98.a
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.cancel_all_distortions_menu_entryClick(Sender: TObject);

begin
  if distortions = 0 then
    EXIT;       // might be a B-6 reset.
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  x_distortion_factor := 1;
  y_distortion_factor := 1;
  aspect_distortion_factor := 1;
  x_coning_distortion_factor := 0;
  y_coning_distortion_factor := 0;
  x_skewing_distortion_factor := 0;
  y_skewing_distortion_factor := 0;
  mirror_x := 100;                         // mirror does conversion to 1/100th mm.
  mirror_y := 100;

  x_scaling_menu_entry.Checked := False;
  y_scaling_menu_entry.Checked := False;
  aspect_distortion_menu_entry.Checked := False;
  x_coning_menu_entry.Checked := False;
  y_coning_menu_entry.Checked := False;
  x_skewing_menu_entry.Checked := False;
  y_skewing_menu_entry.Checked := False;
  mirror_x_menu_entry.Checked := False;
  mirror_y_menu_entry.Checked := False;

  distortions := 0;    // clear all the warning flag bits.
  redraw(True);
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.re_org_menu_entryClick(Sender: TObject);

// get permanent shift data
const
  help_str: string = '      Expert  Help  -  Re-origination' +
    '||Re-origination causes all drawing data to be shifted to a new origin (zero point).' +
    '||This is similar in effect to a normal shift (e.g. with mouse action F7), except that:' +
    '||a. Re-origination data entered here remains in force until you come back here and change it or do a B-6 TURNOUT RESET.'
    +
    ' It is not affected by mouse shifts and is not cancelled by clicking OMIT or CLEAR for shifts and rotations.'
    +
    '||b. Re-origination takes place after any data distortions and is therefore unaffected by them.'
    +
    ' Normal shifts take place before any data distortions, and are therefore distorted.' +
    '||c. Re-origination applies globally for as long as it remains in force, so all templates drawn on the trackpad or in the storage box will be' + ' re-originated, but will return to normal when re-origination is cancelled. The re-origination data is not included in the template files' + ' when they are saved from the box, and must be re-entered as required for each working session.' + '||( Handy hint - make a note of your re-origination settings in your memo text. They can then be quickly copied and pasted back into the data entry form after reloading.)' + '||This re-origination function is intended primarily for use with data distortions when templates' + ' are being output for use elsewhere (e.g. exported in DXF format) - there should normally be no reason to use it when printing directly' + ' from Templot0. For more information select the PROGRAM > EXPERT > DATA DISTORTIONS > ? HELP menu item.' + '||Enter X and Y settings ( + / -  mm ) for the amount of shift required. X dimensions are positive screen left-to-right,' + ' Y dimensions are positive screen bottom-to-top.' + '||Note that re-origination affects only the drawing data. Grid lines and printed trim margins are unaffected (and are not included in DXF files).';

var
  n: integer;
  od: Toutdim;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  putdim(help_str, 1, 're - origination  shift  X   ( from left )  ', re_org_x /
    100, False, False, False, False); // neg ok, preset ok, zero ok, don't terminate on zero.
  n := putdim(help_str, 1, 're - origination  shift  Y  ( from bottom )',
    re_org_y / 100, False, False, False, False);
  // neg ok, preset ok, zero ok, don't terminate on zero.
  if n <> 1 then
    EXIT;
  if getdims('re - origination', '', control_room_form, n, od) = True then begin
    if od[0] = def_req then
      re_org_x := 0
    else
      re_org_x := od[0] * 100;     // in 1/100th mm.

    if od[1] = def_req then
      re_org_y := 0
    else
      re_org_y := od[1] * 100;
  end;

  if (re_org_x = 0) and (re_org_y = 0) then
    re_org_menu_entry.Checked := False
  else
    re_org_menu_entry.Checked := True;
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  room_scaling := True;            // flag to prevent status bar re-sizing while we scale the form.

  if size_updown.Position > size_updown.Tag
  // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                                           // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                                           // scale the form contents up.

  ClientHeight := VertScrollBar.Range;                               // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range;
  //+4;                              // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;
  // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;
  // and save for the next click.

  room_scaling := False;
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.number_format_menu_entryClick(Sender: TObject);

const
  zero_help_str: string = '   Trailing  Zeros' +
    '||For the drawing calculations Templot0 always uses the maximum precision available from your processor, but normally rounds results to two decimal places' + ' when displaying or printing them, with trailing zeros omitted but at least one decimal place always shown, e.g.' + '||4.566  rounds to  4.57  and shows as  4.57' + '|8.203  rounds to  8.20  but shows as  8.2' + '|0.998  rounds to  1.00  but shows as  1.0' + '|3027   shows as   3027.0' + '||All trailing zeros can be omitted if preferred, or both decimal places shown in full.';

var
  i: integer;

begin
  i := alert(4, '   negative  numbers',
    '|Templot0 encloses negative values within square brackets as [ -000.0 ] when displaying them,'
    +
    ' and allows data entry in this format also.' +
    '||Mistakes are less likely if the minus sign is made more conspicuous in this way.' +
    '||The brackets can be omitted if preferred.', '', '', '',
    'omit  brackets  from  negative  numbers', 'cancel  -  no  change',
    'show  negative  numbers  in  brackets', 0);
  case i of
    4:
      omit_neg_brackets := True;
    6:
      omit_neg_brackets := False;
  end;//case

  repeat
    i := alert(4, '   trailing  zeros',
      '|||How would you prefer any trailing zeros on displayed results to be handled?',
      '', '?  help', 'always show two decimal places (e.g. 8.20, 1.00)',
      'omit all trailing zeros (e.g. 8.2, 1 )', 'cancel  -  no  change',
      'omit second place zero only (e.g. 8.2, 1.0)', 2);
    case i of
      2:
        alert_help(0, zero_help_str, '');
      3:
        zero_supp := 0;    // omit no zero decimal places.
      4:
        zero_supp := 2;    // omit both zero decimal places.
      5:
        EXIT;
      6:
        zero_supp := 1;    // omit second zero decimal place only.
      else
        run_error(30);
    end;//case
  until i <> 2;
end;
//_______________________________________________________________________________________

procedure check_colours;      // check colour depth for more than 8-bit.

const
  // these overide global declarations so that I can use these names elsewhere if nec.
  TECHNOLOGY = 2;
  HORZRES = 8;
  VERTRES = 10;
  BITSPIXEL = 12;
  PLANES = 14;
  LOGPIXELSX = 88;
  LOGPIXELSY = 90;

var
  i, n: integer;
  total_colour_bits: double;     // just might overflow integers !!

begin

  i := GetDeviceCaps(control_room_form.Canvas.Handle, PLANES);      //  number of colour planes.
  n := GetDeviceCaps(control_room_form.Canvas.Handle, BITSPIXEL);
  //  number of colour bits per pixel per plane.
  total_colour_bits := i * n;
  //  total colour depth per pixel.

  if total_colour_bits > 9 then begin
    hi_color := True;
    //pad_form.bright_night_scheme_menu_entry.Enabled:=True;
  end
  else
    hi_color := False;

end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.test_menu_entryClick(Sender: TObject);

const
  // these overide global declarations so that I can use these names elsewhere if nec.
  TECHNOLOGY = 2;
  HORZRES = 8;
  VERTRES = 10;
  BITSPIXEL = 12;
  PLANES = 14;
  LOGPIXELSX = 88;
  LOGPIXELSY = 90;

var
  i, n: integer;
  heap_status: THeapStatus;
  s: string;

begin

  chat('You have clicked the TEST menu item which is used for experimental items and for testing Templot0 during development.'
    + '||Sorry there''s nothing exciting here at present.'


    );

  heap_status := GetHeapStatus;

  s := '|Memory  Status|------------------------';

  with heap_status do begin

    s := s + '|| $ ' + IntToHex(TotalAddrSpace, 8) + '    =    ' + FormatFloat(
      '0000000,', TotalAddrSpace) + '    Total Requested Address Space';
    s := s + '||of which:';

    s := s + '|| $ ' + IntToHex(TotalUncommitted, 8) + '    =    ' + FormatFloat(
      '0000000,', TotalUncommitted) + '    is Uncommitted';
    s := s + '| $ ' + IntToHex(TotalCommitted, 8) + '    =    ' + FormatFloat(
      '0000000,', TotalCommitted) + '    is Committed';

    s := s + '||------------------------------';

    s := s + '|| $ ' + IntToHex(FreeSmall, 8) + '    =    ' +
      FormatFloat('0000000,', FreeSmall) + '    Free in Small Blocks';
    s := s + '| $ ' + IntToHex(FreeBig, 8) + '    =    ' + FormatFloat('0000000,', FreeBig) +
      '    Free in Large Blocks';
    s := s + '| $ ' + IntToHex(Unused, 8) + '    =    ' + FormatFloat('0000000,', Unused) +
      '    Free Unused';
    s := s + '| $ ' + IntToHex(TotalFree, 8) + '    =    ' +
      FormatFloat('0000000,', TotalFree) + '    Total Free';

    s := s + '|| $ ' + IntToHex(Overhead, 8) + '    =    ' + FormatFloat('0000000,', Overhead) +
      '    Manager Overhead';

    s := s + '|| $ ' + IntToHex(TotalAllocated, 8) + '    =    ' + FormatFloat(
      '0000000,', TotalAllocated) + '    Total Allocated';

    s := s + '||( total free + overhead + total allocated  =  total committed )';

    s := s + '|| error code = ' + IntToStr(HeapErrorCode);

  end;//with

  help(0, s, '');

end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.FormDestroy(Sender: TObject);

var
  n, aq, index: integer;

begin
  if keeps_list.Count > 0      // free all memory used...
  then begin
    for n := 0 to keeps_list.Count - 1 do begin

      with Ttemplate(keeps_list.Objects[n]) do begin

        if bg_copied = True then begin
          with bgnd_keep do begin
            SetLength(list_bgnd_marks, 0);
            for aq := 0 to aq_max_c do begin
              SetLength(list_bgnd_rails[aq], 0);
            end;//for next aq

          end;//with bgnd_keep
        end;

        template_info.keep_shove_list.Free;

      end;//with template.

      Ttemplate(keeps_list.Objects[n]).Free;

    end;//next n
  end;//if any

  with printer_list do begin
    if Count > 0 then
      for n := 0 to printer_list.Count - 1 do
        Tprint_cal(printer_list.Objects[n]).Free;
  end;//with

  info_text_list.Free;
  printer_list.Free;
  keeps_list.Free;
  memo_list.Free;

  tag_list.Free;     // 206b

  custom_colour_list.Free; // 0.91

  if pad_view_list.Count > 0 then
    for n := 0 to pad_view_list.Count - 1 do
      Tpad_view(pad_view_list.Objects[n]).Free;  // 0.91.c
  pad_view_list.Free;      // 0.91.c

  html_help_list.Free;          // 0.91
  html_printheader_list.Free;
  html_printfooter_list.Free;

  boxmru_list.Free;  // 0.82.a  22-08-06
  bgsmru_list.Free;  // 0.82.a  22-08-06

  // 0.91.d pref_options...

  user_prefs_list.Free;        // 0.91.d

  for index := 0 to undo_c do
    rollback_reg[index].rollback_info.keep_shove_list.Free;

  current_shove_list.Free;

  for index := 0 to 2 do
    parking_bay[index].keep_shove_list.Free;

  offdraw_bmp.Free;
  backdrop_bmp.Free;
end;
//_______________________________________________________________________________________

procedure Tcontrol_room_form.distortion_help_menu_entryClick(Sender: TObject);

const
  chat_str: string = '    Data  Distortions  for  DXF  Files' +
    '||These data distortions are fun to play with, but unless you know what you are doing and why you are doing it are unlikely'
    + ' to result in usable templates. Strictly speaking they ought not to be needed as CAD software should be able to print accurately.' + ' I have been advised, however, that this is not always the case.' + '||The coning and skewing distortions are my attempt to offer a correction for worn or poor quality printers. How well they work' + ' in practice rather depends on the individual printer of course. Have plenty of scrap paper to hand before experimenting !';

begin
  if help(0, distortion_help_str, 'DXF  distortions  chat') = 1 then
    chat(chat_str);
end;
//___________________________________________________________________________________________

procedure Tcontrol_room_form.x_scaling_menu_entryClick(Sender: TObject);

const
  help_str: string = '             X - Scaling  Distortion' +
    '||The X-scaling factor is provided to allow corrections to be made to' +
    ' the accuracy of a template which is being output elsewhere, for example when generating a DXF file for transfer to CAD software.' + '||If your intention is simply to enlarge or reduce a printed template, cancel this and select instead the PRINT > PRINTED OUTPUT SCALING menu item.' + '||If your printer is not producing accurate results, use the PRINT > PRINTER CALIBRATION function to make the necessary' + ' corrections. If the problem with your printer persists, due perhaps to wear and tear, try using the SKEWING' + ' or CONING distortions.' + '||X-dimensions (across the screen from the left) are multiplied by the X-scaling factor.' + '||The result of setting a X-scaling factor of 200%, for example, is that the drawing expands to double its length.' + ' The X-scaling factor has no effect on the width of the drawing. To make small corrections a typical X-scaling factor setting would be' + ' close to 100% and look like 100.71% or 98.35%, for example. If you make large distortions, be aware that the results of the mouse actions and' + ' other settings may be unexpected or become unpredictable.' + '||The effect of setting a negative X-scaling factor is that the drawing is' + ' mirrored 180 degrees about the left margin, and will not be visible without re-origination or a shift.' + ' Also, the mouse actions and other settings will be reversed in direction, causing much confusion.' + '||The X-scaling factor is reset to 100% when you do a B-6 TURNOUT RESET - otherwise it will remain in force until you' + ' come here again and change it. So please remember to reset it to 100% after experimenting. If you forget to do so,' + ' all your subsequently printed templates will be useless!' + ' ||When using X-scaling distortion the printed grid (which is unaffected by distortions) will no' + ' longer be accurate - so please heed the warnings about not using these data distortions when printing directly from Templot0.' + '||When exporting in DXF format the normal printer calibration and output scaling settings are ignored and the DXF file' + ' does not include the grid or page trim lines.' + '||X-scaling distortion applies globally for as long as it remains in force, so all templates drawn on the trackpad or in the storage box will be' + ' distorted but will return to normal when the distortion is cancelled. The X-scaling factor is not included in the template files' + ' when they are saved from the box, and must be re-entered as required for each working session.' + '||( Handy hint - make a note of your scaling settings in your memo text. They can then be quickly copied and pasted back into the data entry form after reloading.)';

var
  n, i: integer;
  od: Toutdim;
  temp: double;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  try
    repeat
      n := putdim(help_str, 4, 'X - scaling  factor', x_distortion_factor *
        100, False, False, True, False);
      // neg ok, preset ok, no zero, don't terminate on zero.
      if n <> 0 then
        EXIT;
      if getdims('X - scaling  factor', distortion_help_str, control_room_form, 0, od) =
        True then begin
        if od[0] = def_req then
          x_distortion_factor := 1
        else begin
          if ABS(od[0]) < minfp then
            od[0] := 1;
          x_distortion_factor := od[0] / 100;
          x_scaling_menu_entry.Checked := True;
        end;
      end
      else
        x_distortion_factor := 1;

      if x_distortion_factor = 1 then begin
        x_scaling_menu_entry.Checked := False;
        EXIT;
      end
      else begin
        temp := x_distortion_factor;
        i :=
          alert(7, '        X - scaling  distortion ?',
          'Setting the X-scaling factor to any value other than 100% will cause' +
          ' your template to be unusable when printed. This function is intended as' +
          ' a means of correction only when the output will be elsewhere.' +
          '||If your printer is not producing accurate results, do a printer calibration instead.'
          +
          '||Are you sure you want to distort your data ?', '', '', 'yes  -  use  ' +
          round_str(temp * 100, 2) + ' %   X - scaling  distortion', '?  help',
          'no  -  cancel  distortion', 'no  -  do  printer  calibration  instead', 4);
        if i = 3 then
          EXIT
        else begin
          x_distortion_factor := 1;
          // he's not sure so cancel.
          x_scaling_menu_entry.Checked := False;
          case i of
            4: begin
              if alert_help(0,
                'Clicking HELP implies that you are unsure about the X-scaling factor' +
                ' ( ' + round_str(temp * 100, 2) + '% ) which you have set.' +
                '||It has therefore been cancelled. Clicking CONTINUE below will return you to the data entry form,'
                + ' where you can re-enter the factor if your intentions remain unchanged.'
                + '|||' + help_str, 'cancel  distortion') = 1 then
                EXIT;
            end;
            5:
              EXIT;
            6: begin
              printer_calibration_menu_entry.Click;
              EXIT;
            end;
            else
              run_error(92);
          end;//case
        end;
      end;
    until 0 <> 0;
  finally
    if x_distortion_factor = 1 then
      distortions := distortions and $FFFE    // clear x bit in the flags byte.
    else
      distortions := distortions or $0001;   // set it.
  end;//try
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.y_scaling_menu_entryClick(Sender: TObject);

const
  help_str: string = '             Y - Scaling  Distortion' +
    '||The Y-scaling factor is provided to allow corrections to be made to' +
    ' the accuracy of a template which is being output elsewhere, for example when generating a DXF file for transfer to CAD software.' + '||If your intention is simply to enlarge or reduce a printed template, cancel this and select instead the PRINT > PRINTED OUTPUT SCALING menu item.' + '||If your printer is not producing accurate results, use the PRINT > PRINTER CALIBRATION function to make the necessary' + ' corrections. If the problem with your printer persists, due perhaps to wear and tear, try using the SKEWING' + ' or CONING distortions.' + '||Y-dimensions (up the screen from the bottom) are multiplied by the Y-scaling factor.' + '||The result of setting a Y-scaling factor of 200%, for example, is that the drawing expands to double its width.' + ' The Y-scaling factor has no effect on the length of the drawing. To make small corrections a typical Y-scaling factor setting would be' + ' close to 100% and look like 100.71% or 98.35%, for example. If you make large distortions, be aware that the results of the mouse actions and' + ' other settings may be unexpected or become unpredictable.' + '||The effect of setting a negative Y-scaling factor is that the drawing is' + ' mirrored 180 degrees about the bottom margin, and will not be visible without re-origination or a shift.' + ' Also, the mouse actions and other settings will be reversed in direction, causing much confusion.' + '||The Y-scaling factor is reset to 100% when you do a B-6 TURNOUT RESET - otherwise it will remain in force until you' + ' come here again and change it. So please remember to reset it to 100% after experimenting. If you forget to do so,' + ' all your subsequently printed templates will be useless!' + ' ||When using Y-scaling distortion the printed grid (which is unaffected by distortions) will no' + ' longer be accurate - so please heed the warnings about not using these data distortions when printing directly from Templot0.' + '||When exporting in DXF format the normal printer calibration and output scaling settings are ignored and the DXF file' + ' does not include the grid or page trim lines.' + '||Y-scaling distortion applies globally for as long as it remains in force, so all templates drawn on the trackpad or in the storage box will be' + ' distorted but will return to normal when the distortion is cancelled. The Y-scaling factor is not included in the template files' + ' when they are saved from the box, and must be re-entered as required for each working session.' + '||( Handy hint - make a note of your scaling settings in your memo text. They can then be quickly copied and pasted back into the data entry form after reloading.)';

var
  n, i: integer;
  od: Toutdim;
  temp: double;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  try
    repeat
      n := putdim(help_str, 4, 'Y - scaling  factor', y_distortion_factor *
        100, False, False, True, False);
      // neg ok, preset ok, no zero, don't terminate on zero.
      if n <> 0 then
        EXIT;
      if getdims('Y - scaling  factor', distortion_help_str, control_room_form, 0, od) =
        True then begin
        if od[0] = def_req then
          y_distortion_factor := 1
        else begin
          if ABS(od[0]) < minfp then
            od[0] := 1;
          y_distortion_factor := od[0] / 100;
          y_scaling_menu_entry.Checked := True;
        end;
      end
      else
        y_distortion_factor := 1;

      if y_distortion_factor = 1 then begin
        y_scaling_menu_entry.Checked := False;
        EXIT;
      end
      else begin
        temp := y_distortion_factor;
        i :=
          alert(7, '        Y - scaling  distortion ?',
          'Setting the Y-scaling factor to any value other than 100% will cause' +
          ' your template to be unusable when printed. This function is intended as' +
          ' a means of correction only when the output will be elsewhere.' +
          '||If your printer is not producing accurate results, do a printer calibration instead.'
          +
          '||Are you sure you want to distort your data ?', '', '', 'yes  -  use  ' +
          round_str(temp * 100, 2) + ' %   Y - scaling  distortion', '?  help',
          'no  -  cancel  distortion', 'no  -  do  printer  calibration  instead', 4);
        if i = 3 then
          EXIT
        else begin
          y_distortion_factor := 1;
          // he's not sure so cancel.
          y_scaling_menu_entry.Checked := False;
          case i of
            4: begin
              if alert_help(0,
                'Clicking HELP implies that you are unsure about the Y-scaling factor' +
                ' ( ' + round_str(temp * 100, 2) + '% ) which you have set.' +
                '||It has therefore been cancelled. Clicking CONTINUE below will return you to the data entry form,'
                + ' where you can re-enter the factor if your intentions remain unchanged.'
                + '|||' + help_str, 'cancel  distortion') = 1 then
                EXIT;
            end;
            5:
              EXIT;
            6: begin
              printer_calibration_menu_entry.Click;
              EXIT;
            end;
            else
              run_error(92);
          end;//case
        end;
      end;
    until 0 <> 0;
  finally
    if y_distortion_factor = 1 then
      distortions := distortions and $FFFD    // clear Y bit in the flags byte.
    else
      distortions := distortions or $0002;   // set it.
  end;//try
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.aspect_distortion_menu_entryClick(Sender: TObject);

const
  help_str: string = '             Aspect  Distortion' +
    '||The aspect-distortion factor is provided for printer testing purposes, and to allow corrections to be made to'
    + ' the accuracy or aspect ratio of a template which is being output elsewhere, for example when generating a DXF file for transfer to CAD software.' + '||There should NEVER be any need to apply an aspect-distortion factor when printing directly from Templot0 - leave the factor setting at 100%' + '||If your printer is not producing accurate results, use the printer calibration function to make the necessary' + ' corrections - see under the PRINT menu. If the problem with your printer persists, due perhaps to wear and tear, try using the SKEWING' + ' or CONING distortions instead.' + '||The aspect-distortion factor is applied to the data as follows:' + '||X-dimensions (across the screen from the left) are multiplied by the aspect-distortion factor; and' + '|Y-dimensions (up the screen from the bottom) are divided by the aspect-distortion factor;' + '||with the result that the area occupied by the drawing remains constant.' + '||The result of setting a aspect-distortion factor of 50%, for example, is that the drawing shrinks to half its length' + ' and is doubled in width. To make small corrections a typical aspect-distortion factor setting would be close to 100% and' + ' look like 100.71% or 98.35%, for example. If you make large distortions, be aware that the results of the mouse actions and' + ' other settings may be unexpected or become unpredictable.' + '||The effect of setting a negative aspect-distortion factor is that the drawing is' + ' additionally rotated 180 degrees about the origin, and will not be visible without re-origination or a shift.' + ' Also, the mouse actions and other settings will be reversed in direction, causing much confusion.' + '||The aspect-distortion factor is reset to 100% when you do a B-6 TURNOUT RESET - otherwise it will remain in force until you' + ' come here again and change it. So please remember to reset it to 100% after experimenting. If you forget to do so,' + ' all your subsequently printed templates will be useless!' + '||Having corrected the aspect ratio by means of the aspect-distortion factor, it is usual to restore the drawing to its correct' + ' size by means of the scaling-distortion factors. This will mean that the printed grid (which is unaffected by distortions) will no' + ' longer be accurate - so please heed the warnings above about not using data distortions when printing directly from Templot0.' + '||Note also that when exporting in DXF format the normal printer calibration and output scaling settings are ignored, so restoration' + ' of the correct drawing size can only be done using the scaling-distortions - or by means of' + ' the destination CAD program''s own scaling or zoom functions. The DXF file does not include the grid or page trim lines.' + '||Aspect distortion applies globally for as long as it remains in force, so all templates drawn on the trackpad or in the storage box will be' + ' distorted but will return to normal when distortion is cancelled. The distortion data is not included in the template files' + ' when they are saved from the box, and must be re-entered as required for each working session.' + '||( Handy hint - make a note of your aspect-distortion setting in your memo text. It can then be quickly copied and pasted back into the data entry form after reloading.)';

var
  n, i: integer;
  od: Toutdim;
  temp: double;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  try
    repeat
      n := putdim(help_str, 4, 'aspect - distortion  factor', aspect_distortion_factor *
        100, False, False, True, False);  // neg ok, preset ok, no zero, don't terminate on zero.
      if n <> 0 then
        EXIT;
      if getdims('aspect  distortion  factor', distortion_help_str, control_room_form, 0, od) =
        True then begin
        if od[0] = def_req then
          aspect_distortion_factor := 1
        else begin
          if ABS(od[0]) < minfp then
            od[0] := 1;
          aspect_distortion_factor := od[0] / 100;
          aspect_distortion_menu_entry.Checked := True;
        end;
      end
      else
        aspect_distortion_factor := 1;

      if aspect_distortion_factor = 1 then begin
        aspect_distortion_menu_entry.Checked := False;
        EXIT;
      end
      else begin
        temp := aspect_distortion_factor;
        i :=
          alert(7, '        aspect - distortion ?',
          'Setting the aspect-distortion factor to any value other than 100% will cause' +
          ' your template to be unusable when printed. This function is intended as' +
          ' a means of correction only when the output will be elsewhere.' +
          '||If your printer is not producing accurate results, do a printer calibration instead.'
          +
          '||Are you sure you want to distort your data ?', '', '', 'yes  -  use  ' +
          round_str(temp * 100, 2) + ' %   aspect - distortion', '?  help',
          'no  -  cancel  distortion', 'no  -  do  printer  calibration  instead', 4);
        if i = 3 then
          EXIT
        else begin
          aspect_distortion_factor := 1;
          // he's not sure so cancel.
          aspect_distortion_menu_entry.Checked := False;
          case i of
            4: begin
              if alert_help(0,
                'Clicking HELP implies that you are unsure about the aspect-distortion factor' +
                ' ( ' + round_str(temp * 100, 2) + '% ) which you have set.' +
                '||It has therefore been cancelled. Clicking CONTINUE below will return you to the data entry form,'
                + ' where you can re-enter the factor if your intentions remain unchanged.'
                + '|||' + help_str, 'cancel  distortion') = 1 then
                EXIT;
            end;
            5:
              EXIT;
            6: begin
              printer_calibration_menu_entry.Click;
              EXIT;
            end;
            else
              run_error(92);
          end;//case
        end;
      end;
    until 0 <> 0;
  finally
    if aspect_distortion_factor = 1 then
      distortions := distortions and $FFFB    // clear our bit in the flag byte.
    else
      distortions := distortions or $0004;   // set it.
  end;//try

  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.mirror_x_menu_entryClick(Sender: TObject);

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  mirror_x_menu_entry.Checked := not mirror_x_menu_entry.Checked;
  if mirror_x_menu_entry.Checked = True then begin
    mirror_x := -100;                        // mirror does conversion to 1/100th mm.
    distortions := distortions or $0100;    // set flag bit.
  end
  else begin
    mirror_x := 100;                         // mirror does conversion to 1/100th mm.
    distortions := distortions and $FEFF;    // clear our bit in the flag byte.
  end;
  redraw(True);
end;
//___________________________________________________________________________________

procedure Tcontrol_room_form.mirror_y_menu_entryClick(Sender: TObject);

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  mirror_y_menu_entry.Checked := not mirror_y_menu_entry.Checked;
  if mirror_y_menu_entry.Checked = True then begin
    mirror_y := -100;                        // mirror does conversion to 1/100th mm.
    distortions := distortions or $0200;    // set flag bit.
  end
  else begin
    mirror_y := 100;                         // mirror does conversion to 1/100th mm.
    distortions := distortions and $FDFF;    // clear our bit in the flag byte.
  end;
  redraw(True);
end;
//_____________________________________________________________________________________

procedure Tcontrol_room_form.x_coning_menu_entryClick(Sender: TObject);

var
  n: integer;
  od: Toutdim;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  try
    n := putdim(coning_help_str, 0, 'X - coning  factor', x_coning_distortion_factor *
      1.0E6, False, False, False, False);  // neg ok, preset ok, zero ok, don't terminate on zero.
    if n <> 0 then
      EXIT;
    if getdims('X - coning  factor', distortion_help_str, control_room_form, 0, od) = True then
    begin
      if od[0] = def_req then
        x_coning_distortion_factor := 0
      else begin
        x_coning_distortion_factor := od[0] / 1.0E6;
        x_coning_menu_entry.Checked := True;
      end;
    end
    else
      x_coning_distortion_factor := 0;

    if x_coning_distortion_factor = 0 then
      x_coning_menu_entry.Checked := False;

  finally
    if x_coning_distortion_factor = 0 then
      distortions := distortions and $FFEF    // clear bit in the flags byte.
    else
      distortions := distortions or $0010;   // set it.
  end;//try
  redraw(True);
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.y_coning_menu_entryClick(Sender: TObject);

var
  n: integer;
  od: Toutdim;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  try
    n := putdim(coning_help_str, 0, 'Y - coning  factor', y_coning_distortion_factor *
      1.0E6, False, False, False, False);  // neg ok, preset ok, zero ok, don't terminate on zero.
    if n <> 0 then
      EXIT;
    if getdims('Y - coning  factor', distortion_help_str, control_room_form, 0, od) = True then
    begin
      if od[0] = def_req then
        y_coning_distortion_factor := 0
      else begin
        y_coning_distortion_factor := od[0] / 1.0E6;
        y_coning_menu_entry.Checked := True;
      end;
    end
    else
      y_coning_distortion_factor := 0;

    if y_coning_distortion_factor = 0 then
      y_coning_menu_entry.Checked := False;

  finally
    if y_coning_distortion_factor = 0 then
      distortions := distortions and $FFDF    // clear bit in the flags byte.
    else
      distortions := distortions or $0020;   // set it.
  end;//try
  redraw(True);
end;
//___________________________________________________________________________________________

procedure Tcontrol_room_form.cancel_re_org_menu_entryClick(Sender: TObject);

begin
  re_org_menu_entry.Checked := False;
  if (re_org_x = 0) and (re_org_y = 0) then
    EXIT;    // might be a B-6 reset.
  if cleared_bgnd = False then
    EXIT;               // first clear any background keeps.

  re_org_x := 0;
  re_org_y := 0;
  redraw(True);
end;
//___________________________________________________________________________________

procedure Tcontrol_room_form.x_skewing_menu_entryClick(Sender: TObject);

var
  n: integer;
  od: Toutdim;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  try
    n := putdim(skewing_help_str, 0, 'X - skewing  factor', x_skewing_distortion_factor *
      5.0E3, False, False, False, False);  // neg ok, preset ok, zero ok, don't terminate on zero.
    if n <> 0 then
      EXIT;
    if getdims('X - skewing  factor', distortion_help_str, control_room_form, 0, od) = True then
    begin
      if od[0] = def_req then
        x_skewing_distortion_factor := 0
      else begin
        x_skewing_distortion_factor := od[0] / 5.0E3;  // 5E3 arbitrary.
        x_skewing_menu_entry.Checked := True;
      end;
    end
    else
      x_skewing_distortion_factor := 0;

    if x_skewing_distortion_factor = 0 then
      x_skewing_menu_entry.Checked := False;

  finally
    if x_skewing_distortion_factor = 0 then
      distortions := distortions and $FFBF    // clear bit in the flags byte.
    else
      distortions := distortions or $0040;   // set it.
  end;//try
  redraw(True);
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.y_skewing_menu_entryClick(Sender: TObject);

var
  n: integer;
  od: Toutdim;

begin
  if cleared_bgnd = False then
    EXIT;  // first clear any background keeps.

  try
    n := putdim(skewing_help_str, 0, 'Y - skewing  factor', y_skewing_distortion_factor *
      5.0E3, False, False, False, False);  // neg ok, preset ok, zero ok, don't terminate on zero.
    if n <> 0 then
      EXIT;
    if getdims('Y - skewing  factor', distortion_help_str, control_room_form, 0, od) = True then
    begin
      if od[0] = def_req then
        y_skewing_distortion_factor := 0
      else begin
        y_skewing_distortion_factor := od[0] / 5.0E3;  // 5E3 arbitrary.
        y_skewing_menu_entry.Checked := True;
      end;
    end
    else
      y_skewing_distortion_factor := 0;

    if y_skewing_distortion_factor = 0 then
      y_skewing_menu_entry.Checked := False;

  finally
    if y_skewing_distortion_factor = 0 then
      distortions := distortions and $FF7F    // clear bit in the flags byte.
    else
      distortions := distortions or $0080;   // set it.
  end;//try
  redraw(True);
end;
//__________________________________________________________________________________________

procedure set_printer_font_margins(calling_form: TForm; set_font: boolean);

const
  margin_help_str: string = 'Printer  Page  Margins  for  Text`6' +
    '||Enter widths in mm for the left and right page margins when printing the help notes and template information.'
    + '||Text is left-aligned and unjustified, so the right margin dimension will be approximate.'
    + '||You can also change the formatting of text by changing the paper size setting in the Print Setup dialog for your printer.'
    + '||green_panel_begin tree.gif Margin settings made here have no effect when printing track templates.'
    + ' To change the trim margin dimensions for track templates, click the `0OUTPUT > TRIM MARGINS > SET TRIM MARGINS...`1 menu item on the `0trackpad`3.green_panel_end';

var
  n: integer;
  od: Toutdim;

begin
  if set_font = True then
    printer_text_font.Assign(get_font('choose  a  new  font  and  colour  for  text  printing',
      printer_text_font, True));

  putdim(margin_help_str, 1, 'width  of  left  page  margin', printer_text_left_margin,
    True, True, False, False);  // no neg, no preset, zero ok, don't terminate on zero.
  n := putdim(margin_help_str, 1, 'width  of  right  page  margin',
    printer_text_right_margin, True, True, False, False);
  // no neg, no preset, zero ok, don't terminate on zero.

  if n <> 1 then
    EXIT;
  if getdims('printer  page  margins  for  text', '', calling_form, n, od) = True then begin
    printer_text_left_margin := od[0];       // in mm.
    printer_text_right_margin := od[1];

    help_form.html_view.PrintMarginLeft := printer_text_left_margin / 10;
    // in cm for viewer.
    help_form.html_view.PrintMarginRight := printer_text_right_margin / 10;

    keep_form.keep_html_view.PrintMarginLeft := printer_text_left_margin / 10;
    // in cm for viewer.
    keep_form.keep_html_view.PrintMarginRight := printer_text_right_margin / 10;

    alert_box.alert_html_viewer.PrintMarginLeft := printer_text_left_margin / 10;
    // in cm for viewer.
    alert_box.alert_html_viewer.PrintMarginRight := printer_text_right_margin / 10;

    mint_form.mint_html_view.PrintMarginLeft := printer_text_left_margin / 10;
    // in cm for viewer.
    mint_form.mint_html_view.PrintMarginRight := printer_text_right_margin / 10;

  end;
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.printer_font_menu_entryClick(Sender: TObject);

begin
  set_printer_font_margins(control_room_form, True);
end;
//___________________________________________________________________________________________________________

procedure Tcontrol_room_form.shapes_menu_entryClick(Sender: TObject);

begin
  pad_form.bgnd_shapes_menu_entry.Click;
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.storage_box_menu_entryClick(Sender: TObject);

begin
  pad_form.view_box_menu_entry.Click;
end;
//________________________________________________________________________________________

function cleared_bgnd: boolean;

begin
  Result := False;       // init default.

  if any_bgnd = 0 then begin
    Result := True;
    EXIT;
  end;

  case alert(7, '    background  templates  showing',
      '||It is not possible to make changes to the data-distortion or re-origination settings'
      +
      ' while there are background templates shown on the trackpad.' +
      '||All background templates must first be wiped. After making changes to the data-distortion or'
      + ' re-origination settings, the background templates can be re-drawn as required by copying'
      + ' from the storage box in the normal way.', '', '', '', '',
      'cancel  changes    ', 'wipe  all  background  templates      ', 0)
    of
    5:
      EXIT;
    6: begin
      keep_form.wipe_all_menu_entry.Click;  //bgkeeps_form.clear_button.Click;
      Result := True;
    end;
  end;//case
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.auto_mouse_dir_menu_entryClick(Sender: TObject);

begin
  auto_dir := not auto_dir;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.click_move_click_menu_entryClick(Sender: TObject);

begin
  mouse_click_action := 1;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.button_down_menu_entryClick(Sender: TObject);

begin
  mouse_click_action := 0;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.either_action_menu_entryClick(Sender: TObject);

begin
  mouse_click_action := -1;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.mouse_action_menu_entryClick(Sender: TObject);

begin
  case mouse_click_action of
    -1:
      either_action_menu_entry.Checked := True;     // radio item.
    0:
      button_down_menu_entry.Checked := True;       // radio item.
    1:
      click_move_click_menu_entry.Checked := True;  // radio item.
  end;//case

  auto_mouse_dir_menu_entry.Checked := auto_dir;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.clm_help_menu_entryClick(Sender: TObject);

const
  clm_help_str: string = '      `0Expert  Help  -  Unit  Angles`9' +
    '||The present version of Templot0 does all calculations on unit angles using RAM (Right-Angle Measure),'
    + ' where the angle for a 1:N crossing is ARCTAN( 1 / N ). This is by far the most convenient measure for model trackwork design,' + ' where Cartesian coordinates are the norm on a squared grid.' + '||In full-size practice CLM (Centre-Line Measure) is more usual, as this is better suited to use on the ground.' + ' In CLM the angle for a 1:N crossing is 2 x ARCTAN( 1 / (2 x N) ). The difference between these two angles is very slight,' + ' and decreases with increasing N.' + '||Few modellers will be concerned with this distinction, but if you are one of them you can convert to CLM equivalents' + ' using the `0real > V-crossing options > convert RAM to CLM`z menu item on the trackpad. After setting a crossing unit angle, say 1:7,' + ' this tool will change the crossing angle to 1:7 CLM. The angle will, however, continue to be displayed in RAM units,' + ' showing as, say, B - 6.96 as the turnout size in the information panel heading. The information text always shows the current crossing angle in both' + ' RAM and CLM units.' + '||Be aware that after every change you make to the crossing angle, either by direct entry or by mouse action (F5), it will be' + ' necessary to use the converter tool again if you want to have the new unit angle in CLM units.' + '||Later versions of Templot0 will permit a global change of unit measure (here in this menu), and the converter tool will then' + ' be no longer required. Some railways used other measures, e.g. IM (Isosceles Measure) or SM (Sine Measure), and some non-U.K. railways have other measures (e.g. the grade system),' + ' all of which will also be available later.';

begin
  help(0, clm_help_str, '');
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.max_explode_menu_entryClick(Sender: TObject);

const
  explode_help_str: string = '      Maximum  Explode   ( Zoom-In )' +
    '||Enter a dimension in mm which represents the scale width of the trackpad when fully exploded (zoomed all the way in).'
    + '||This is an inverse measure - meaning that reducing this dimension increases the magnification of the drawing.'
    + '||The minimum dimension which Templot0 will accept is 0.5 mm.' +
    '|The maximum dimension which Templot0 will accept is 1000 mm.' +
    '|The pre-set dimension is 10 mm.'
    // 0.93.a was 15mm
    + '||Setting a figure greater than about 150 mm will handicap some of Templot0''s functions to no advantage.'
    + '|------------------' + '|Caution:' +
    '||It has been found possible to "hang" the graphics system in some early versions of Windows if this dimension'
    + ' is set too low. You should quit all other applications and save your work before experimenting'
    + ' with dimensions less than 10 mm. There is seldom any practical need to zoom in more than this.'
    + '||If you experience problems this dimension can be increased to 25 or 50 mm without any great detriment in 4mm/ft scale and above.' + '||N.B. For Windows 95/98/ME some features of your templates are likely to be omitted from the trackpad as you zoom in to trackpad widths less than approximately 1ft scale size.' + ' They will re-appear as you zoom out again. This is a feature of the Windows graphics system, not Templot, and does not apply to Windows NT/2000/XP/Vista/7.' + '||The recommended minimum settings are 5 mm for working at 4mm/ft scale, and 9 mm for 7mm/ft scale.';

var
  n: integer;
  od: Toutdim;
  m: double;
  dummy_i: integer;

begin
  m := screenx_min;

  n := putdim(explode_help_str, 1, 'trackpad  width  at  maximum  explode',
    m, True, False, True, False);
  // no neg, preset ok, no zero, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('maximum  explode  ( zoom-in )', '', control_room_form, n, od) = True then begin
    m := od[0];
    if m = def_req then
      m := 10;    // default 10mm.   0.93.a was 15mm
    screenx_min := limits(0.5, 1000, ABS(m), dummy_i);
  end;
  if screenx < screenx_min then
    screenx := screenx_min;
  redraw(True);
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.peg_arm_menu_entryClick(Sender: TObject);

const
  peg_help_str: string = '      Peg  Arms' +
    '||The length of the angled arms on the fixing peg can be changed. This is sometimes useful when aligning templates by eye on the pad' + ' to ensure maximum accuracy.' + '||Enter the required arm length in mm. The pre-set arm length is equivalent to a scale length of 3ft.' + '||The new length will appear immediately on the control template, but it is necessary to rebuild the background templates before the new arm length' + ' will take effect on them (GENERATOR > REBUILD ALL BACKGROUND menu item).' + '||N.B. The peg has two arms, one each side of the centre. The dimension here applies to each side, so the overall length is double this figure.' + ' The angled arms on the pegging notch will be set to the same length.';

var
  n: integer;
  od: Toutdim;
  m: double;

begin
  m := peg_arm_length;

  n := putdim(peg_help_str, 1, 'length  of  angled  arms  on  fixing  peg',
    m, True, False, False, False);
  // no neg, preset ok, zero ok, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('peg  arm  length', '', control_room_form, n, od) = True then begin
    m := od[0];
    if m = def_req then
      m := 3 * scale;    // default 3ft.
    peg_arm_length := m;
  end;
  redraw(True);
end;
//___________________________________________________________________________________________

procedure Tcontrol_room_form.FormCreate(Sender: TObject);

var
  reminder_list: TStringList;

begin
  log := Logger.GetInstance('ControlRoom');

  // OT-FIRST ClientWidth:=750;
  // OT-FIRST ClientHeight:=328;

  AutoScroll := True;

  Screen.Cursors[adjust_ne_cursor_invert] := LoadCursor(HInstance, 'CURSOR_ADJUST_NE_INVERT');
  Screen.Cursors[adjust_corners_cursor_invert] :=
    LoadCursor(HInstance, 'CURSOR_ADJUST_CORNERS_INVERT');
  Screen.Cursors[adjust_ns_cursor_invert] := LoadCursor(HInstance, 'CURSOR_ADJUST_NS_INVERT');
  Screen.Cursors[adjust_we_cursor_invert] := LoadCursor(HInstance, 'CURSOR_ADJUST_WE_INVERT');
  Screen.Cursors[mouse_action_cursor] := LoadCursor(HInstance, 'CURSOR_MOUSE_ACTION');
  Screen.Cursors[cross_size_cursor] := LoadCursor(HInstance, 'CURSOR_CROSS_SIZE');
  Screen.Cursors[target_rings_cursor] := LoadCursor(HInstance, 'CURSOR_RINGS');
  Screen.Cursors[open_slider_cursor] := LoadCursor(HInstance, 'CURSOR_OPEN_SLIDER');
  Screen.Cursors[zoom_rectangle_cursor] := LoadCursor(HInstance, 'CURSOR_ZOOM_RECTANGLE');
  Screen.Cursors[cross_hairs_cursor] := LoadCursor(HInstance, 'CURSOR_CROSS_HAIRS');
  Screen.Cursors[group_rectangle_cursor] := LoadCursor(HInstance, 'CURSOR_GROUP_RECTANGLE');

  info_text_list := TStringList.Create;            // 0.78.a  15-11-02.
  printer_list := TStringList.Create;
  keeps_list := TStringList.Create;
  memo_list := TStringList.Create;


  tag_list := TStringList.Create;                  // 206b

  current_shove_list := Tshoved_timber_list.Create;

  custom_colour_list := TStringList.Create;        // 0.91

  pad_view_list := TStringList.Create;             // 0.91.c

  html_help_list := TStringList.Create;            // 0.91
  html_printheader_list := TStringList.Create;
  html_printfooter_list := TStringList.Create;

  boxmru_list := TStringList.Create;                  // 0.82.a  22-08-06
  bgsmru_list := TStringList.Create;                  // 0.82.a  22-08-06

  // 0.91.d pref_options...

  user_prefs_list := TStringList.Create;        // 0.91.d

  keeps_list.OnChange := keeps_list_change;

  Application.OnActivate := AppActivate;

  Application.OnIdle := AppIdle;      // 0.73.a 15-9-01.

  Application.OnRestore := AppRestore;  // 0.91.b

  Application.ShowHint := True;         // 0.93.a

  Application.HintPause := 750;         // 3/4 second.   // 0.93.a

  Application.HintHidePause := 5000;     // 5 seconds  // 0.93.a

  // 0.91.d pref_options...

  //prefs_existed_on_startup := FileExists(ExtractFilePath(Application.ExeName) + prefs_pointer_str);
  prefs_existed_on_startup := FileExists(Config.GetFilePath(csfiPrefsPointer));
  prefs_available := prefs_existed_on_startup;

  if prefs_existed_on_startup = True then begin
    go_prefs_button.Show;
    open_button.Width := 50;
  end
  else begin
    go_prefs_button.Hide;
    open_button.Width := 125;
  end;

  reminder_list := TStringList.Create;

  if FileExists(reminder_file_path) then begin
    reminder_list.LoadFromFile(reminder_file_path);
    reminder_label.Caption := 'Reminder message:' + #13 + #13 + reminder_list.Text;
  end
  else
    reminder_label.Caption := 'Reminder message:' + #13 + #13 +
      'A reminder message from the previous working session can be shown here.';

  reminder_list.Free;

  about_templot_version_menu_entry.Caption :=
    'about  ' + Application.Title + '    ( v : ' + GetVersionString(voFull) + ' )';

  version_label.Caption := GetVersionString(voFull);

  if Application.Title = 'TemplotMEC' then begin
    Color := $00A8FFF8;                   // TemplotMEC  unripe banana
    tmec_logo_panel.Visible := True;
  end
  else
  if Application.Title = 'OpenTemplot' then begin
    Color := $00FFEECC;                   // OpenTemplot  water blue
    ot_logo_panel.Visible := True;
  end
  else begin
    Color := $00B0FFF0;                   // 291a Templot3  cream
    t3_logo_panel.Visible := True;
  end;

  Caption := '        . . .  welcome  to  ' + Application.Title;

  startup_label.Caption := '. . .  welcome  to  ' + Application.Title;

  quit_menu_entry.Caption := 'quit  ' + Application.Title;

end;
//______________________________________________________________________________________

procedure create_backup_file(final: boolean);

var
  code: integer;

begin
  if loading_in_progress = True then
    EXIT;  // 208c

  if final = True then
    code := -1       // final backup on exit
  else
    code := 1;       // normal rolling backup.

  save_box(0, 0, code, '');
  // create a new emergency backup file.  Including the control

  backup_wanted := False;             // backup has been updated.
end;
//______________________________________________________________________________________

procedure Tcontrol_room_form.keeps_list_change(Sender: TObject);        // list OnChange event

begin
  backup_wanted := True;
end;
//_______________________________________________________________________________________

procedure Tcontrol_room_form.AppActivate(Sender: TObject);

begin
  external_window_showing := False;     // after showing file folders

  if initdone_flag = True then
    bgnd_form.paste_button.Enabled := Clipboard.HasFormat(CF_BITMAP);  // 211b
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  Action := caFree;
end;
//________________________________________________________________________________________

procedure Tcontrol_room_form.custom_input_factors_menu_entryClick(Sender: TObject);

begin
  get_custom_input_factors;
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.reload_buttonClick(Sender: TObject);

begin
  pad_button.Click;
  pad_form.pad_reload_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.recent_buttonClick(Sender: TObject);   // 0.82.a

begin
  pad_button.Click;
  pad_form.pad_reload_recent_file_menu_entry.Click;    // 0.82.a
end;
//___________________________________________________________________________________________

procedure Tcontrol_room_form.clear_templates_menu_entryClick(Sender: TObject);

begin
  keep_form.clear_menu_entry.Click;
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.clear_shapes_menu_entryClick(Sender: TObject);

begin
  bgnd_form.delete_all_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.program_menuClick(Sender: TObject);

begin
  delete_reminder_menu_entry.Enabled := FileExists(reminder_file_path);
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.fast_100_menu_entryClick(Sender: TObject);

begin
  fast_100_menu_entry.Checked := True;   // radio item.
  allow_idle := False;
end;
//__________________________________________________________________________________________

procedure Tcontrol_room_form.mouse_100_menu_entryClick(Sender: TObject);

begin
  mouse_100_menu_entry.Checked := True;   // radio item.
  allow_idle := True;                     // mouse action not currently in force.
end;
//___________________________________________________________________________________________

procedure Tcontrol_room_form.allow_full_idle_menu_entryClick(Sender: TObject);

begin
  allow_full_idle_menu_entry.Checked := True;   // radio item.
  allow_idle := True;
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.allow_idle_help_menu_entryClick(Sender: TObject);

const
  idle_help_str: string = '      Expert  Help  -  CPU  Usage' +
    '||These options allow you to control the amount of cpu processor time allocated to Templot0.'
    +
    '||The 100% ALWAYS (FAST) option causes Templot0 to use all of the processor time permitted by the Windows operating system.'
    + ' This option will give the fastest response to mouse activity and keyboard keys, but your system cpu processor will be running at near 100% capacity, with consequent power consumption and heat generation.' + ' This option is not recommended for laptops and battery-powered systems.' + '||The ALLOW FULL IDLE (SLOW) option causes Templot0 to relinquish control of the cpu whenever there is no further processing to be done. This option will give a slower response to mouse activity and keyboard keys,' + ' but your system cpu processor will be running at much lower capacity, with consequent savings in power and battery life.' + ' This option is the default setting and recommended for most users.' + '||The 100% FOR MOUSE ACTIONS ONLY option is a half-way compromise setting. While a mouse action is being used the cpu will be switched to 100% usage to give the best response to the mouse movements.' + ' At all other times the cpu will be allowed to idle to conserve power.' + '||The extent to which these options vary the response time on your system will be determined by your hardware configuration and the number of other software applications and background programs which are running.' + ' On many systems there may be little difference in perceived response time between these various options.' + '||n.b. In all versions of Templot0 prior to 0.73.a the only option available was 100% ALWAYS (FAST).';

begin
  help(0, idle_help_str, '');
end;

procedure Tcontrol_room_form.please_read_first_menu_entryClick(Sender: TObject);

begin
  pad_form.read_first_menu_entry.Click;
end;
//_______________________________________________________________________________________

procedure Tcontrol_room_form.graphics_16_bit_limits_menu_entryClick(Sender: TObject);

begin
  max_draw_int := 16000;
  //  these are actually less than 15-bit integers for drawing limits...
  min_draw_int := -16000;       //  (divide 16-bit by 2 and a bit as safety margin).

  // 0.93.a same defaults for export limits..

  max_export_x := 16000;
  min_export_x := -16000;

  max_export_y := 16000;
  min_export_y := -16000;

  graphics_limits := True;
  graphics_16_bit_limits_menu_entry.Checked := True;      // radio item.
end;
//___________________________________________

procedure Tcontrol_room_form.graphics_24_bit_limits_menu_entryClick(Sender: TObject);

begin
  max_draw_int := 4000000;
  //  these are actually less than 23-bit integers for drawing limits...
  min_draw_int := -4000000;     //  (divide 24-bit by 2 and a bit as safety margin).

  // 0.93.a same defaults for export limits..

  max_export_x := 4000000;
  min_export_x := -4000000;

  max_export_y := 4000000;
  min_export_y := -4000000;

  graphics_limits := True;
  graphics_24_bit_limits_menu_entry.Checked := True;      // radio item.
end;
//____________________________________________

procedure Tcontrol_room_form.graphics_32_bit_limits_menu_entryClick(Sender: TObject);

begin
  max_draw_int := 1000000000;
  //  these are actually less than 31-bit integers for drawing limits...
  min_draw_int := -1000000000;  //  (divide 32-bit by 2 and a bit as safety margin).

  // 0.93.a same defaults for export limits..

  max_export_x := 1000000000;
  min_export_x := -1000000000;

  max_export_y := 1000000000;
  min_export_y := -1000000000;

  graphics_limits := True;
  graphics_32_bit_limits_menu_entry.Checked := True;      // radio item.
end;
//______________________________________________

procedure Tcontrol_room_form.graphics_no_limits_menu_entryClick(Sender: TObject);

begin
  graphics_limits := False;
  graphics_no_limits_menu_entry.Checked := True;      // radio item.
end;
//______________________________________________

procedure Tcontrol_room_form.graphics_limits_help_menu_entryClick(Sender: TObject);

const
  limits_help_str: string = '      `0Graphics  Limits`9' +
    '||Use these settings to select the level of graphics parameter limit checking.' +
    ' This controls the maximum size of object which can be drawn on the screen or printed.' +
    '||For <B>Windows NT / 2000 / XP / Vista / Windows 7</B> the recommended setting is 32-bit.'
    +
    '||Change to the 16-bit setting if you are using an older printer and experience printing problems.'
    + '||Change to the 24-bit setting if you are using Windows NT / 2000 with original printer drivers.'
    + '||Change to the no-limits setting if you are content to leave all parameter checking to the Windows graphics systems. This will give the fastest program response,' + ' but may cause problems and/or graphics hardware malfunctions when working at extreme zoom-in or printing very large picture shape images.' + '|<HR NOSHADE COLOR="#EE7700">' + 'For <B>Windows 95 / 98 / ME</B> the setting is fixed at 16-bit and cannot be changed. This is a limitation in Windows, not Templot0.' + ' When zooming-in on the screen and printing at high resolution you may find that you lose some drawing features (e.g. large background shapes and very long timbers).' + ' Background picture shape images may not print on full-size templates. For more information click the `0PICTURE SHAPES`1 tab on the `0print pages`3 window when it appears.' + '|<HR NOSHADE COLOR="#EE7700">' + 'To change the zoom-in limit, click the `0PROGRAM > MAX EXPLODE (ZOOM-IN)...`1 menu item.';

begin
  help(0, limits_help_str, '');
end;
//_________________________________________________________________________________________

procedure Tcontrol_room_form.save_all_menu_entryClick(Sender: TObject);

begin
  pad_form.pad_save_all_menu_entry.Click;
end;
//_______________________________________________________________________________________

procedure Tcontrol_room_form.standard_wrap_menu_entryClick(Sender: TObject);

begin
  standard_wrap_menu_entry.Checked := True;       // radio item.
  override_file_check_for_grid := True;
end;
//____________________________________________________________________________________

procedure Tcontrol_room_form.control_sketchboard_buttonClick(Sender: TObject);

begin
  pad_button.Click;
  pad_form.sketchboard_button.Click;       // 0.93.a
end;
//______________________________________________________________________________

procedure go_to_templot_com;

const
  url_str: string = 'http://templot.com/';    // 0.79.a

begin
  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the Templot web site.',
      '', '', '', '', '', 'continue', 0);
  end;

end;
//______________________________________________________________________________

procedure go_to_templot_club;

const
  url_str: string = 'http://85a.co.uk/forum/';      //%%%%

begin
  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the Templot Club web site.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//______________________________________________________________________________

procedure go_to_templot_companion;

const
  url_str: string = 'http://templot.com/companion/';    // 0.93.a

begin
  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the Templot web site.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//______________________________________________________________________________

procedure go_to_templot_companion_page(dest_page_str: string);    // 214a

var
  url_str: string;

begin
  url_str := 'http://templot.com/companion/' + dest_page_str;  // needs to be local

  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the Templot web site.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//______________________________________________________________________________

procedure go_to_donation;      // 0.96.a Templot2

const
  url_str: string = 'http://templot.com/martweb/templot_donate.htm';

begin
  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the Templot web site.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//______________________________________________________________________________

procedure go_to_upgrade;      // 0.96.a Templot2

const
  url_str: string = 'http://templot.com/martweb/templot_upgrade.htm';

begin
  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the Templot web site.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//______________________________________________________________________________

procedure go_to_video_list;      // 211b Templot2

const
  url_str: string = 'http://templot.com/martweb/video_list.htm';

begin
  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the Templot web site.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//______________________________________________________________________________

procedure go_to_url(url: string);      // 212a

var
  url_str: string;

begin
  url_str := url;  // needs to be local

  if not OpenURL(url_str) then begin
    alert(2, '    connection  failed',
      'Sorry, unable to open your browser window and connect to the web site.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.com_labelClick(Sender: TObject);

begin
  go_to_templot_com;
end;
//___________________________________________________________________________________________

procedure Tcontrol_room_form.previous_labels_menu_entryClick(Sender: TObject);

// 211b
begin
  previous_labels_menu_entry.Checked := True;  // radio item.
end;
//------------------------------------------------

procedure Tcontrol_room_form.very_random_labels_menu_entryClick(Sender: TObject);

// 0.82.a
begin
  very_random_labels_menu_entry.Checked := True;  // radio item.
end;
//________________________________________________

procedure Tcontrol_room_form.random_labels_menu_entryClick(Sender: TObject);

// 0.82.a
begin
  random_labels_menu_entry.Checked := True;  // radio item.
end;
//________________________________________________

procedure Tcontrol_room_form.fixed_labels_menu_entryClick(Sender: TObject);

// 0.82.a
begin
  fixed_labels_menu_entry.Checked := True;  // radio item.
end;
//_____________________________________________________________________________________________

procedure Tcontrol_room_form.saved_preferences_setup_menu_entryClick(Sender: TObject);

begin
  show_prefs_dialog(False);
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.reminder_menu_entryClick(Sender: TObject);

begin
  with edit_memo_form do begin

    Close;       // in case already showing (we want Modal).

    Caption := '  write  a  reminder  message ...';

    vis_edit_memo.Clear;

    if FileExists(reminder_file_path) then
      vis_edit_memo.Lines.LoadFromFile(reminder_file_path)
    else begin
      vis_edit_memo.Lines.Add('Enter here a reminder message.');
      vis_edit_memo.Lines.Add('');
      vis_edit_memo.Lines.Add(
        'The message will be displayed when you next start a Templot session.');
      vis_edit_memo.SelectAll;
    end;

    if do_show_modal(edit_memo_form) = mrOk    // 212a
    then begin
      if vis_edit_memo.Lines.Count > 0 then
        vis_edit_memo.Lines.SaveToFile(reminder_file_path);
    end;

  end;//with
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.delete_reminder_menu_entryClick(Sender: TObject);

begin
  if DeleteFile(reminder_file_path) then
    ShowMessage('Your reminder message has been deleted.');
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.update_prefs_on_quit_menu_entryClick(Sender: TObject);

begin
  update_prefs_on_quit_menu_entry.Checked := not update_prefs_on_quit_menu_entry.Checked;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.prefs_include_gen_settings_menu_entryClick(Sender: TObject);

begin
  prefs_include_gen_settings_menu_entry.Checked := True;  // radio item
end;
//___________________________________

procedure Tcontrol_room_form.prefs_exclude_gen_settings_menu_entryClick(Sender: TObject);

begin
  prefs_exclude_gen_settings_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.preferences_menuClick(Sender: TObject);

begin
  update_prefs_on_quit_menu_entry.Enabled := prefs_available;
  prefs_exclude_gen_settings_menu_entry.Enabled := prefs_available;
  prefs_include_gen_settings_menu_entry.Enabled := prefs_available;
  abandon_prefs_menu_entry.Enabled := prefs_available;

  if prefs_available = True then
    saved_preferences_setup_menu_entry.Caption := '&saved  preferences ...'
  else
    saved_preferences_setup_menu_entry.Caption := 'begin  &saving  preferences ...';
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.abandon_prefs_menu_entryClick(Sender: TObject);

begin
  abandon_prefs(False);
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.older_computers_menu_entryClick(Sender: TObject);

const
  col_help_str: string =
    '||If your computer screen is currently using 256 colours (8-bit palette colour) or fewer, you may want to try changing to 12-bit colour depth' + ' or better (Hi-Color or Tru-Color). You will then find it easier to adjust individual colours.' + '||If you prefer not to change the colour settings on your computer, Templot0 provides a few pre-set colour schemes using only basic colours. Click the button below to try them.' + '||<SPAN STYLE="FONT-SIZE:12px;">( To find out if you can change the colour depth, click START > SETTINGS > CONTROL PANEL > DISPLAY > SETTINGS and explore the entries' + ' under COLORS or COLOR DEPTH or COLOR PALETTE. If the memory available to your video card is limited, selecting extra colours may cause a trade-off' + ' against the screen resolution. There is little to be gained in Templot0 by setting the colour depth greater than 16-bit, and doing so may significantly' + ' reduce the mouse response speed when using the mouse actions to adjust your templates.' + ' If you make any changes you will need to quit and restart Templot0 for the changes to take effect.)</SPAN>';

  res_help_str: string =
    '||Templot0 was designed to run best at 1024 x 768 screen resolution or higher. If your screen resolution is currently lower than this, Templot0''s windows and forms (such as this one) may appear quite large and chunky.' + '||If you prefer not to change the screen resolution settings on your computer, you can scale the size of each window by repeatedly clicking the up-down arrow buttons' + ' which appear in a blue panel at or near the top left corner of each window. Try this now on this window.' + '||For many of Templot0''s windows the visible area can also be enlarged, reduced, and scrolled in the usual way by dragging the edges. This is not the same effect as scaling the size with the up-down buttons.' + '||If you change the scale size several times, rounding effects within Windows may cause parts of the window to become misaligned or obscured. The functional working will not be affected.' + '||Scaling the size of a window may produce an unsatisfactory font size, so windows which contain a significant' + ' amount of text also have a <SPAN STYLE="COLOR:#0000FF">font</SPAN> button (usually top right) with which the text font size and colour can also be changed.' + '||<SPAN STYLE="FONT-SIZE:12px;">( To find out if you can change the screen resolution, click START > SETTINGS > CONTROL PANEL > DISPLAY > SETTINGS and explore the entries' + ' under SCREEN or RESOLUTION or DISPLAY AREA and under FONTS or FONT SIZE. You may have to click ADVANCED or ADVANCED SETTINGS to find these. If the memory available to your graphics card is limited, selecting a higher resolution' + ' may cause a trade-off against the number of colours available. If you make any changes you will need to quit and restart Templot0 for the changes to take effect.)</SPAN>';

  ppi_help_str: string =
    '||Templot0 was developed using Large Fonts (120 pixels per inch). If your screen setting is currently for Small Fonts (96 pixels per inch),' + ' you may find that Templot0''s windows and fonts appear larger than you have been using in other software.' + '||If you prefer to have them smaller, you can scale the size of each window by repeatedly clicking the up-down arrow buttons' + ' which appear in a blue panel at or near the top left corner of each window. Try this now on this window.' + '||For many of Templot0''s windows the visible area can also be enlarged, reduced, and scrolled in the usual way by dragging the edges. This is not the same effect as scaling the size with the up-down buttons.' + '||If you change the scale size several times, rounding effects within Windows may cause parts of the window to become misaligned or obscured. The functional working will not be affected.' + '||Scaling the size of a window may produce an unsatisfactory font size, so windows which contain a significant' + ' amount of text also have a <SPAN STYLE="COLOR:#0000FF">font</SPAN> button (usually top right) with which the text font size and colour can also be changed.' + '||<SPAN STYLE="FONT-SIZE:12px;">( You may prefer to change your screen font size to Large Fonts, which are much easier on the eyes. To find out if you can change the font size,' + ' click START > SETTINGS > CONTROL PANEL > DISPLAY > SETTINGS and explore the entries' + ' under FONTS or FONT SIZE. You may have to click ADVANCED or ADVANCED SETTINGS to find these. If you make any changes you will need to restart your computer for the changes to take effect.)</SPAN>';

  header_style_str1: string =
    '<SPAN STYLE="font-weight:bold; color:#0077DD; font-family:''Verdana''; font-size:20px; font-style:italic;">    ';
  header_style_str2: string = '</SPAN><HR COLOR="#AAAAAA" NOSHADE><BR>';

var
  cols_str: string;
  ppi_str: string;
  res_str: string;

begin
  if Screen.DesktopWidth < 1024 then
    res_str := '<SPAN STYLE="COLOR:#990000;"><I>Your computer screen resolution is currently lower than 1024 pixels wide.</I></SPAN>'
  else
    res_str := '<SPAN STYLE="COLOR:#990000;"><I>Your computer screen resolution is currently 1024 pixels wide or higher, so the following notes can be disregarded.</I></SPAN>';

  help(0, header_style_str1 + 'Older  Computers  -  Screen  Resolution' +
    header_style_str2 + res_str + res_help_str, '');

  if Screen.PixelsPerInch = 96 then
    ppi_str := '<SPAN STYLE="COLOR:#990000;"><I>Your computer screen is currently set to 96 pixels per inch.</I></SPAN>'
  else
    ppi_str := '<SPAN STYLE="COLOR:#990000;"><I>Your computer screen is currently set to ' +
      IntToStr(Screen.PixelsPerInch) +
      ' pixels per inch, so the following notes can be disregarded.</I></SPAN>';

  help(0, header_style_str1 + 'Older  Computers  -  Screen  Fonts' +
    header_style_str2 + ppi_str + ppi_help_str, '');

  if hi_color = False then
    cols_str := '<SPAN STYLE="COLOR:#990000;"><I>Your computer screen is currently using 256 colours or fewer.</I></SPAN>'
  else
    cols_str := '<SPAN STYLE="COLOR:#990000;"><I>Your computer screen is currently using more than 256 colours, so the following notes can be disregarded.</I></SPAN>';

  if help(0, header_style_str1 + 'Older  Computers  -  Screen  Colours' +
    header_style_str2 + cols_str + col_help_str, 'try  a  different  colour  scheme') = 1 then
    pad_form.preset_schemes_menu_entry.Click;

end;
//______________________________________________________________________________

procedure Tcontrol_room_form.data_panel_font_menu_entryClick(Sender: TObject);

// 0.93.a

begin
  data_child_form.data_memo.Font.Assign(
    get_font('choose  a  new  text  font  for  the data  panel',
    data_child_form.data_memo.Font, True));
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.exports_menu_entryClick(Sender: TObject);

begin
  pad_form.export_file_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.make_donation_menu_entryClick(Sender: TObject);

begin
  go_to_donation;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.make_on_click_mode_menu_entryClick(Sender: TObject);

// 0.93.a   was called Quick mode

begin
  cancel_adjusts(False);
  classic_templot := False;
  make_on_click_mode_menu_entry.Checked := True;       // radio item

  if pad_form.show_bgnd_keeps_in_rect_menu_entry.Checked = False then begin
    pad_form.top_toolbar_panel.ParentColor := False;
    pad_form.top_toolbar_panel.Color := $003399FF;       // shows orangey
  end;

  pad_form.second_toolbar_panel.ParentColor := False;     // 217a
  pad_form.second_toolbar_panel.Color := $003399FF;       // shows orangey

end;
//______________________________________________________________________________

procedure Tcontrol_room_form.classic_templot_mode_menu_entryClick(Sender: TObject);

// 0.93.a   now called Normal mode.

begin
  cancel_adjusts(False);
  classic_templot := True;
  classic_templot_mode_menu_entry.Checked := True;     // radio item

  if pad_form.show_bgnd_keeps_in_rect_menu_entry.Checked = False then
    pad_form.top_toolbar_panel.ParentColor := True;      // shows trackpad colour

  pad_form.second_toolbar_panel.ParentColor := True;
  // 217a shows trackpad colour

end;
//______________________________________________________________________________

procedure Tcontrol_room_form.click_mode_help_menu_entryClick(Sender: TObject);

begin
  //
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.click_mode_options_menu_entryClick(Sender: TObject);

begin
  if classic_templot = True then
    classic_templot_mode_menu_entry.Checked := True     // radio items
  else
    make_on_click_mode_menu_entry.Checked := True;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.room_file_viewer_menu_entryClick(Sender: TObject);    // 208d

begin
  //keep_form_was_showing:=False;
  do_show_modal(file_viewer_form);  // 212a
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.viewer_bitmaps_menu_entryClick(Sender: TObject);  // 208e

begin
  do_open_source_bang('FILE VIEWER');  // OT-FIRST
  { OT-FIRST
  viewer_bitmaps_menu_entry.Checked:=True;  // radio item
  fv_has_been_active:=False;                // force refresh of viewer
  }
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.viewer_png_menu_entryClick(Sender: TObject);  // 208e

begin
  do_open_source_bang('FILE VIEWER');  // OT-FIRST
  { OT-FIRST
  viewer_png_menu_entry.Checked:=True;  // radio item
  fv_has_been_active:=False;            // force refresh of viewer
  }
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.file_viewer_help_menu_entryClick(Sender: TObject);

const
  fvo_help_str: string = 'php/950      `0file  viewer  options`9' +
    '||These options control how the screenshot images in the file viewer are generated.' +
    '||The `0images in memory`1 option is recommended.' +
    '||Change to the `0images as files`1 option if your system is low on RAM memory, or you have many hundreds of .box3 files in a single folder, or you have many other programs running concurrently with Templot0.' + '||The screenshot images will then be saved as image files on the disk drive, instead of being stored in memory. Be aware that this option will cause the viewer to work more slowly.';

begin
  do_open_source_bang('FILE VIEWER');  // OT-FIRST
  { OT-FIRST
  help(0,fvo_help_str,'');
  }
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.popup_menu_position_menu_entryClick(Sender: TObject);  // 213a

const
  popup_help_str: string =
    'Enter a screen position for the pop-up menu which appears when you click on a background template.'
    + '||These co-ordinates are in screen dots from the top left corner of the screen: X across from the left, Y down from the top.'
    + '||These co-ordinates refer to your full computer display screen for Windows, the position of Templot0 on the screen (if it is not full-screen) is not relevant.' + '||Take care to enter sensible figures, otherwise the pop-up menu may not be visible.' + '||If the menu position does not correspond to your entered co-ordinates, most likely your dimensions are not valid and are being adjusted by Windows to fit your screen.' + '||To revert to using the default menu position on the left, enter -1 (minus 1) for X and Y.';

var
  n: integer;
  od: Toutdim;

begin
  putdim(popup_help_str, 0, 'template  context  menu  position  X  ( from  left )',
    user_popup_X, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.
  n := putdim(popup_help_str, 0, 'template  context  menu  position  Y  ( from  top )',
    user_popup_Y, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.

  if n <> 1 then
    EXIT;

  if getdims('template  context  popup  menu  position', '', control_room_form, n, od) = True then
  begin
    user_popup_X := Round(od[0]);
    user_popup_Y := Round(od[1]);
  end;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.trackpad_position_menu_entryClick(Sender: TObject);

const
  trackpad_help_str: string =
    'Enter a screen position for the trackpad window when it is not maximized.' +
    '||These co-ordinates are in screen dots from the top left corner of the screen: X across from the left, Y down from the top.'
    + '||These co-ordinates refer to your full computer display screen for Windows, the current position of Templot0 on the screen is not relevant.' + '||Take care to enter sensible figures, otherwise the trackpad window may not be visible.';

var
  n: integer;
  od: Toutdim;

begin
  if pad_form.WindowState <> wsNormal then begin
    ShowMessage(
      'The trackpad window position cannot be changed because the trackpad window is currently maximized to full-screen.'
      + #13 + #13 + 'Restore Down on the trackpad window first.');
    EXIT;
  end;


  putdim(trackpad_help_str, 0, 'trackpad  window  position  X  ( from  left )',
    pad_form.Left, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.
  putdim(trackpad_help_str, 0, 'trackpad  window  position  Y  ( from  top )',
    pad_form.Top, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.

  putdim(trackpad_help_str, 0, 'trackpad  window  width', pad_form.Width, True, True, True, False);
  // no neg, no preset, no zero, don't terminate on zero.
  n := putdim(trackpad_help_str, 0, 'trackpad  window  height', pad_form.Height,
    True, True, True, False);
  // no neg, no preset, no zero, don't terminate on zero.

  if n <> 3 then
    EXIT;

  if getdims('trackpad  window  position', '', control_room_form, n, od) = True then begin
    pad_form.Left := Round(od[0]);
    pad_form.Top := Round(od[1]);
    pad_form.Width := Round(od[2]);
    pad_form.Height := Round(od[3]);
  end;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.show_time_ticker_menu_entryClick(Sender: TObject);        // 216a

begin
  show_time_ticker_menu_entry.Checked := not show_time_ticker_menu_entry.Checked;

  pad_form.time_now_panel.Visible := show_time_ticker_menu_entry.Checked;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.blank_spaces_in_menus_menu_entryClick(Sender: TObject);

var
  i, j: integer;
  temp_comp_i, temp_comp_j: TComponent;

  show_blanks: boolean;

begin

  blank_spaces_in_menus_menu_entry.Checked := not blank_spaces_in_menus_menu_entry.Checked;

  show_blanks := blank_spaces_in_menus_menu_entry.Checked;

  for j := Application.ComponentCount - 1 downto 0 do begin

    temp_comp_j := Application.Components[j];

    if (temp_comp_j is TForm) then begin

      for i := temp_comp_j.ComponentCount - 1 downto 0 do begin

        temp_comp_i := temp_comp_j.Components[i];

        if (temp_comp_i is TMenuItem) then begin
          if (TMenuItem(temp_comp_i).Caption = ' ') or
            (TMenuItem(temp_comp_i).Caption = ' ') then
            TMenuItem(temp_comp_i).Visible := show_blanks;
        end;

      end;//next i

    end;

  end;//next j
end;
//______________________________________________________________________________

function change_jpeg_filename(in_str: string): string;    // 214a

begin
  Result := in_str;  // init

  if LowerCase(ExtractFileExt(in_str)) = '.jpeg' then
    Result := ChangeFileExt(in_str, '.jpg');

end;
//______________________________________________________________________________

function get_path_to_Windows_folder(folder_id: integer): string;   // 214a
begin
  Result := GetUserDir();
end;
//______________________________________________________________________________

procedure open_MyPictures;   // 214a

var
  folder_str: string;

begin
  folder_str := get_path_to_Windows_folder(39);  // 39 = CSIDL_MYPICTURES (0x27)

  if folder_str = '' then begin
    ShowMessage('Sorry, unable to find the folder.');
    EXIT;
  end;

  folder_str := folder_str + PathDelim;

  if not OpenDocument(folder_str) then
    ShowMessage('Sorry, unable to open the folder.')
  else
    external_window_showing := True;
end;
//______________________________________________________________________________

procedure open_MyDocuments;   // 214a

var
  folder_str: string;

begin
  folder_str := GetUserDir();

  if folder_str = '' then begin
    ShowMessage('Sorry, unable to find the folder.');
    EXIT;
  end;

  folder_str := folder_str + PathDelim;

  if not OpenDocument(folder_str) then
    ShowMessage('Sorry, unable to open the folder.')
  else
    external_window_showing := True;
end;
//______________________________________________________________________________

procedure Tcontrol_room_form.jpg_menu_entryClick(Sender: TObject);  // 214a

const
  jpg_help_str: string = '      `0JPG  Image  Quality`9' +
    '||Enter a setting between 1% and 100%' +
    '||This setting controls the image quality when saving image files in JPG format from Templot0.'
    +
    '||Higher settings create a better-quality image but also a larger file size.' +
    '||The default setting is for 100% best quality.' +
    '||You can reduce this if you need to create a smaller file size. A setting below about 80% will cause a noticeable reduction in image quality.' + '||green_panel_begin tree.gif Generally it is better to use PNG format rather than JPG when saving image files from Templot0.' + '||In most cases PNG will create a smaller file size with 100% image quality. The JPG format is intended only for photographic images from a camera or a scanner.green_panel_end';

var
  n: integer;
  od: Toutdim;

begin
  n := putdim(jpg_help_str, 4, 'JPG  image  quality', jpg_quality, True, True, True, False);
  // no neg, no preset, no zero, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('JPG  image  quality', '', Screen.ActiveForm, n, od) = True then begin
    if (od[0] < 1) or (od[0] > 100) then begin
      ShowMessage('error - this setting must be between 1% and 100%');
      EXIT;
    end
    else
      jpg_quality := Round(od[0]);
  end;
end;
//______________________________________________________________________________

procedure do_open_source_bang(str: string);  // OT-FIRST

begin
  ShowMessage('              ' + Application.Title + '   first release November 2019' +
    #13 + #13 + 'The function you have selected is not available in this first release of ' +
    Application.Title + ':' + #13 + #13 + '                ' + str + #13 +
    #13 + 'This release of ' + Application.Title + ' is not intended for practical use.' +
    #13 + #13 + 'A full working version of Templot2 is available (free) from the templot.com web site.');

















end;
//______________________________________________________________________________

initialization

  Randomize;                  // init random generator for the session.

  //______________________________________________________________________________

end.
