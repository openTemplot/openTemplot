
(*  v1
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  OpenTemplot project contributors

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

unit bgnd_unit;

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Menus, ExtDlgs, Buttons,
  point_ex,
  background_shapes,
  pad_unit,

  { OT-FIRST AcquireImage,}

  LCLtype;  // OT-FIRST

type

  { Tbgnd_form }

  Tbgnd_form = class(TForm)
    add_picture_shape_button: TButton;
    datestamp_label: TLabel;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    Label46: TLabel;
    open_mystuff_button: TButton;
    picture_buttons_panel: TPanel;
    pictures_sheet: TTabSheet;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    close_panel: TPanel;
    close_button: TButton;
    p1_label: TLabel;
    p2_label: TLabel;
    empty_label: TLabel;
    type_label: TLabel;
    filesave_dialog: TSaveDialog;
    bgs_file_label: TLabel;
    fileload_dialog: TOpenDialog;
    bgnd_shapes_listbox: TListBox;
    count_label: TLabel;
    tab_panel: TPanel;
    shapes_pages: TPageControl;
    modify_all_sheet: TTabSheet;
    shift_all_by_button: TButton;
    mirror_groupbox: TGroupBox;
    mirror_x_button: TButton;
    mirror_y_button: TButton;
    scale_all_by_button: TButton;
    rotate_all_by_button: TButton;
    dxf_sheet: TTabSheet;
    import_dxf_button: TButton;
    dxf_units_groupbox: TGroupBox;
    dxf_mm_radio: TRadioButton;
    dxf_inches_radio: TRadioButton;
    modify_on_import_groupbox: TGroupBox;
    scale_by_label: TLabel;
    shift_x_label: TLabel;
    shift_y_label: TLabel;
    dxf_set_button: TButton;
    dxf_limits_checkbox: TCheckBox;
    font_sheet: TTabSheet;
    label_font_button: TButton;
    colour_groupbox: TGroupBox;
    pad_colour_button: TButton;
    print_colour_button: TButton;
    options_sheet: TTabSheet;
    close_checkbox: TCheckBox;
    reload_limits_checkbox: TCheckBox;
    trackpad_grid_in_front_checkbox: TCheckBox;
    mm_grid_button: TButton;
    dxf_file_dialog: TOpenDialog;
    picture_borders_checkbox: TCheckBox;
    line_width_groupbox: TGroupBox;
    pad_shapes_linewidth_1_radiobutton: TRadioButton;
    pad_shapes_linewidth_2_radiobutton: TRadioButton;
    pad_shapes_linewidth_3_radiobutton: TRadioButton;
    use_notch_fixed_marker_checkbox: TCheckBox;
    recent_popup_menu: TPopupMenu;
    recent_1_popup_entry: TMenuItem;
    recent_2_popup_entry: TMenuItem;
    recent_3_popup_entry: TMenuItem;
    recent_4_popup_entry: TMenuItem;
    recent_5_popup_entry: TMenuItem;
    recent_6_popup_entry: TMenuItem;
    N1: TMenuItem;
    recent_bgsfiles_caption_popup_entry: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    clear_recent_bgs_popup_entry: TMenuItem;
    N4: TMenuItem;
    bgsmru_cancel_popup_entry: TMenuItem;
    picture_load_dialog: TOpenPictureDialog;
    picture_save_dialog: TSavePictureDialog;
    move_up_button: TSpeedButton;
    move_down_button: TSpeedButton;
    move_to_top_button: TSpeedButton;
    move_to_bottom_button: TSpeedButton;
    shape_page_control: TPageControl;
    new_tab_sheet: TTabSheet;
    modify_tab_sheet: TTabSheet;
    Label3: TLabel;
    modify_panel: TPanel;
    modify_button: TButton;
    mouse_action_groupbox: TGroupBox;
    corner1_button: TButton;
    mouse_shift_one_button: TButton;
    corner2_button: TButton;
    mouse_scale_one_button: TButton;
    scale_one_by_button: TButton;
    picture_groupbox: TGroupBox;
    reload_picture_button: TButton;
    twist_picture_button: TButton;
    save_picture_button: TButton;
    marker_line_fixed_button: TButton;
    marker_line_twist_button: TButton;
    trans_checkbox: TCheckBox;
    wrap_picture_button: TButton;
    straighten_picture_button: TButton;
    rotate_one_by_button: TButton;
    new_panel: TPanel;
    name_label: TLabel;
    Label1: TLabel;
    name_editbox: TEdit;
    shape_groupbox: TGroupBox;
    line_radio_button: TRadioButton;
    rect_radio_button: TRadioButton;
    circle_radio_button: TRadioButton;
    label_radio_button: TRadioButton;
    target_radio_button: TRadioButton;
    picture_radio_button: TRadioButton;
    style_groupbox: TGroupBox;
    clear_radio_button: TRadioButton;
    solid_infill_radio_button: TRadioButton;
    hatched_radio_button: TRadioButton;
    line_groupbox: TGroupBox;
    solid_radio_button: TRadioButton;
    dotted_radio_button: TRadioButton;
    shift_one_by_button: TButton;
    all_mouse_groupbox: TGroupBox;
    mouse_shift_all_button: TButton;
    mouse_rotate_all_button: TButton;
    mouse_scale_all_button: TButton;
    TabSheet1: TTabSheet;
    delete_25_button: TButton;
    delete_50_button: TButton;
    Label4: TLabel;
    Label5: TLabel;
    lock_groupbox: TGroupBox;
    lock_to_origin_radiobutton: TRadioButton;
    lock_to_notch_radiobutton: TRadioButton;
    lock_to_ring_radiobutton: TRadioButton;
    output_grid_in_front_checkbox: TCheckBox;
    Label6: TLabel;
    output_shapes_in_front_of_sb_checkbox: TCheckBox;
    paste_button: TButton;
    { OT-FIRST twain_acquire: TAcquireImage;}
    all_90degs_groupbox: TGroupBox;
    all_90degs_acw_button: TButton;
    all_90degs_cw_button: TButton;
    crop_rectangle_button: TButton;
    bgnd_shapes_menu: TMainMenu;
    bgnd_files_menu: TMenuItem;
    bgnd_edit_menu: TMenuItem;
    rename_menu_entry: TMenuItem;
    reload_menu_entry: TMenuItem;
    add_file_menu_entry: TMenuItem;
    N5: TMenuItem;
    save_all_menu_entry: TMenuItem;
    N6: TMenuItem;
    hide_menu_entry: TMenuItem;
    N7: TMenuItem;
    delete_menu_entry: TMenuItem;
    N8: TMenuItem;
    delete_all_menu_entry: TMenuItem;
    bgnd_draw_menu: TMenuItem;
    draw_mouse_menu_entry: TMenuItem;
    N9: TMenuItem;
    bgnd_help_menu: TMenuItem;
    background_shapes_help_menu_entry: TMenuItem;
    N10: TMenuItem;
    picture_shapes_help_menu_entry: TMenuItem;
    draw_spacing_ring_menu_entry: TMenuItem;
    recent_files_menu_entry: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    dimensions_menu_entry: TMenuItem;
    Label24: TLabel;
    bgnd_zoom_menu: TMenuItem;
    zoom_fit_shape_menu_entry: TMenuItem;
    N20: TMenuItem;
    zoom_fit_all_shapes_menu_entry: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    Label7: TLabel;
    Label8: TLabel;
    N24: TMenuItem;
    N25: TMenuItem;
    N26: TMenuItem;
    break_rectangle_menu_entry: TMenuItem;
    combine_pictures_menu_entry: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    N28: TMenuItem;
    N31: TMenuItem;
    scan_button: TButton;
    Label9: TLabel;
    copy_image_button: TButton;
    auto_fit_picture_button: TButton;
    apply_button: TButton;
    buttons_panel: TPanel;
    Label2: TLabel;
    shapes_help_label: TLabel;
    add_shape_button: TButton;
    enter_dims_radiobutton: TRadioButton;
    clicked_locs_radiobutton: TRadioButton;
    by_drawing_radiobutton: TRadioButton;
    new_picture_shape1: TShape;
    new_picture_shape2: TShape;
    bgsfilescanalsobe1: TMenuItem;
    draggedontothetrackpad1: TMenuItem;
    go_to_my_documents_menu_entry: TMenuItem;
    N32: TMenuItem;
    Label12: TLabel;
    shift_one_to_button: TButton;
    scale_one_to_button: TButton;
    new_picture_shape3: TShape;
    show_tick_image: TImage;
    hide_tick_image: TImage;
    show_output_tick_image: TImage;
    hide_output_tick_image: TImage;
    show_on_trackpad_static: TStaticText;
    show_on_output_static: TStaticText;
    bgnd_show_menu: TMenuItem;
    N33: TMenuItem;
    show_on_trackpad_menu_entry: TMenuItem;
    N34: TMenuItem;
    show_on_output_menu_entry: TMenuItem;
    N35: TMenuItem;
    N36: TMenuItem;
    bgnd_recent_menu: TMenuItem;
    saved_jpg_quality_menu_entry: TMenuItem;
    N37: TMenuItem;
    N38: TMenuItem;
    calculate_map_size_menu_entry: TMenuItem;
    bgnd_map_menu: TMenuItem;
    N39: TMenuItem;
    N40: TMenuItem;
    N41: TMenuItem;
    N42: TMenuItem;
    N27: TMenuItem;
    bgnd_all_menu: TMenuItem;
    break_all_rectangles_menu_entry: TMenuItem;
    N44: TMenuItem;
    N45: TMenuItem;
    rotate_around_groupbox: TGroupBox;
    rotate_grid_radiobutton: TRadioButton;
    rotate_notch_radiobutton: TRadioButton;
    rotate_ring_radiobutton: TRadioButton;
    twist_groupbox: TGroupBox;
    twist_centre_radiobutton: TRadioButton;
    twist_as_all_radiobutton: TRadioButton;
    Label13: TLabel;
    all_pictures_transparent_menu_entry: TMenuItem;
    no_pictures_transparent_menu_entry: TMenuItem;
    N46: TMenuItem;
    N47: TMenuItem;
    N48: TMenuItem;
    allow_sync_checkbox: TCheckBox;
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure how_panelClick(Sender: TObject);
    procedure line_radio_buttonClick(Sender: TObject);
    procedure rect_radio_buttonClick(Sender: TObject);
    procedure add_shape_buttonClick(Sender: TObject);
    //procedure clear_buttonClick(Sender: TObject);
    //procedure delete_buttonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure modify_buttonClick(Sender: TObject);
    //procedure save_buttonClick(Sender: TObject);
    //procedure reload_buttonClick(Sender: TObject);
    procedure bgnd_shapes_listboxClick(Sender: TObject);
    procedure label_font_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure label_radio_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    //procedure mouse_draw_buttonClick(Sender: TObject);
    //procedure clicked_shape_buttonClick(Sender: TObject);
    procedure pad_colour_buttonClick(Sender: TObject);
    procedure shift_all_by_buttonClick(Sender: TObject);
    procedure corner1_buttonClick(Sender: TObject);
    procedure corner2_buttonClick(Sender: TObject);
    procedure mouse_shift_one_buttonClick(Sender: TObject);
    procedure mouse_shift_all_buttonClick(Sender: TObject);
    procedure scale_all_by_buttonClick(Sender: TObject);
    procedure mirror_x_buttonClick(Sender: TObject);
    procedure mirror_y_buttonClick(Sender: TObject);
    procedure print_colour_buttonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure target_radio_buttonClick(Sender: TObject);
    //procedure add_file_buttonClick(Sender: TObject);
    //procedure rec_line_buttonClick(Sender: TObject);
    //procedure rec_line_all_buttonClick(Sender: TObject);
    procedure rotate_all_by_buttonClick(Sender: TObject);
    procedure mm_grid_buttonClick(Sender: TObject);
    procedure mouse_scale_all_buttonClick(Sender: TObject);
    procedure mouse_rotate_all_buttonClick(Sender: TObject);
    procedure dxf_set_buttonClick(Sender: TObject);
    procedure import_dxf_buttonClick(Sender: TObject);
    procedure mouse_scale_one_buttonClick(Sender: TObject);
    procedure picture_radio_buttonClick(Sender: TObject);
    procedure reload_picture_buttonClick(Sender: TObject);
    procedure scale_one_by_buttonClick(Sender: TObject);
    procedure delete_50_buttonClick(Sender: TObject);
    procedure delete_25_buttonClick(Sender: TObject);
    procedure twist_picture_buttonClick(Sender: TObject);
    procedure save_picture_buttonClick(Sender: TObject);
    procedure marker_line_fixed_buttonClick(Sender: TObject);
    procedure marker_line_twist_buttonClick(Sender: TObject);
    procedure trackpad_grid_in_front_checkboxClick(Sender: TObject);
    procedure trans_checkboxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure use_notch_fixed_marker_checkboxClick(Sender: TObject);
    procedure rotate_grid_radiobuttonClick(Sender: TObject);
    procedure rotate_notch_radiobuttonClick(Sender: TObject);
    procedure rotate_ring_radiobuttonClick(Sender: TObject);
    procedure recent_buttonClick(Sender: TObject);
    procedure recent_1_popup_entryClick(Sender: TObject);
    procedure recent_2_popup_entryClick(Sender: TObject);
    procedure recent_3_popup_entryClick(Sender: TObject);
    procedure recent_4_popup_entryClick(Sender: TObject);
    procedure recent_5_popup_entryClick(Sender: TObject);
    procedure recent_6_popup_entryClick(Sender: TObject);
    procedure clear_recent_bgs_popup_entryClick(Sender: TObject);
    procedure wrap_picture_buttonClick(Sender: TObject);
    procedure move_to_top_buttonClick(Sender: TObject);
    procedure move_to_bottom_buttonClick(Sender: TObject);
    procedure move_up_buttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure move_up_buttonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure move_down_buttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure move_down_buttonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure straighten_picture_buttonClick(Sender: TObject);
    procedure shift_one_by_buttonClick(Sender: TObject);
    procedure rotate_one_by_buttonClick(Sender: TObject);
    procedure paste_buttonClick(Sender: TObject);
    procedure all_90degs_acw_buttonClick(Sender: TObject);
    procedure all_90degs_cw_buttonClick(Sender: TObject);
    //procedure crop_rectangle_buttonClick(Sender: TObject);
    //procedure crop_picture_buttonClick(Sender: TObject);
    procedure reload_menu_entryClick(Sender: TObject);
    //procedure bgs_file_labelClick(Sender: TObject);
    procedure add_file_menu_entryClick(Sender: TObject);
    procedure recent_files_menu_entryClick(Sender: TObject);
    procedure save_all_menu_entryClick(Sender: TObject);
    procedure delete_menu_entryClick(Sender: TObject);
    procedure delete_all_menu_entryClick(Sender: TObject);
    procedure background_shapes_help_menu_entryClick(Sender: TObject);
    procedure draw_mouse_menu_entryClick(Sender: TObject);
    procedure picture_shapes_help_menu_entryClick(Sender: TObject);
    procedure rename_menu_entryClick(Sender: TObject);
    procedure zoom_fit_all_shapes_menu_entryClick(Sender: TObject);
    procedure zoom_fit_shape_menu_entryClick(Sender: TObject);
    procedure draw_spacing_ring_menu_entryClick(Sender: TObject);
    procedure enter_dims_radiobuttonClick(Sender: TObject);
    procedure clicked_locs_radiobuttonClick(Sender: TObject);
    procedure by_drawing_radiobuttonClick(Sender: TObject);
    procedure break_rectangle_menu_entryClick(Sender: TObject);
    procedure combine_pictures_menu_entryClick(Sender: TObject);
    procedure shapes_help_labelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure new_panelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure shapes_help_labelClick(Sender: TObject);
    procedure scan_buttonClick(Sender: TObject);
    procedure copy_image_buttonClick(Sender: TObject);
    procedure auto_fit_picture_buttonClick(Sender: TObject);
    procedure apply_buttonClick(Sender: TObject);
    procedure add_picture_shape_buttonClick(Sender: TObject);
    procedure go_to_my_documents_menu_entryClick(Sender: TObject);
    procedure open_mystuff_buttonClick(Sender: TObject);
    procedure shift_one_to_buttonClick(Sender: TObject);
    procedure bgnd_shapes_listboxDblClick(Sender: TObject);
    procedure hide_tick_imageClick(Sender: TObject);
    procedure show_tick_imageClick(Sender: TObject);
    procedure show_output_tick_imageClick(Sender: TObject);
    procedure hide_output_tick_imageClick(Sender: TObject);
    procedure bgnd_shapes_listboxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure show_on_trackpad_staticClick(Sender: TObject);
    procedure show_on_output_staticClick(Sender: TObject);
    procedure scale_one_to_buttonClick(Sender: TObject);
    procedure saved_jpg_quality_menu_entryClick(Sender: TObject);
    procedure calculate_map_size_menu_entryClick(Sender: TObject);
    procedure bgnd_map_menuClick(Sender: TObject);
    // OT-FIRST procedure convert_24bit_menu_entryClick(Sender: TObject);
    procedure break_all_rectangles_menu_entryClick(Sender: TObject);
    procedure all_pictures_transparent_menu_entryClick(Sender: TObject);
    procedure no_pictures_transparent_menu_entryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  bgnd_form: Tbgnd_form;

  //-----------------------------
  shapes_label_font: TFont;
  shapes_saved: boolean = True;          // nothing there yet to be saved.
  bgnd_no_update: boolean = False;


  // 214a for drawn shapes...

  shape_rectangle_x1: double = 0;     // export image rectangle mm...
  shape_rectangle_y1: double = 0;

  shape_rectangle_x2: double = 600;   // arbitrary default 600mm x 300mm (2ft x 1ft)
  shape_rectangle_y2: double = 300;

  rotate_centre_code: integer = 3;       // 1:notch  2:spacing-ring  3:grid     219a rotate all

procedure do_bgnd(modify: boolean);
procedure scale_all_shapes(xfactor, yfactor: double);
procedure shift_all_shapes(xshift, yshift: double);
procedure rotate_all_shapes(sync, pictures: boolean; rot_k: double);

procedure scale_this_shape(xfactor, yfactor: double);
procedure shift_this_shape(xshift, yshift: double; corners: integer);
procedure shapes_current_state;
procedure load_shapes(file_str: string; append, mru, dropped: boolean);
procedure bgnd_move_up_button_click;           // 205a
procedure bgnd_move_down_button_click;         // 205a

procedure normalize_line(var p1, p2: Tpex);       //214a
procedure normalize_rectangle(var p1, p2: Tpex);  //214a

function do_twain(var img_bmp: TBitmap): boolean;       // 213b

procedure add_shape(add_option: integer);

procedure add_picture_expert;   // 214a

procedure free_shape_object(n: integer);  // moved to interface 215a

function get_EMF_from_file_to_memory(file_str: string; var new_met_DC_handle: HDC): boolean;
// OT-FIRST  219a

function create_picture_shape_image_from_file(image_file_str: string;
  bgshape: Tbgshape; var img_width, img_height: integer): boolean;    // 291a


//________________________________________________________________________________________

implementation

{$R *.lfm}

uses
  LCLIntf, Math, Clipbrd,
  config_unit,
  control_room, grid_unit, colour_unit, help_sheet, chat_unit, alert_unit,
  entry_sheet, math_unit, wait_message, image_viewer_unit, xml_unit, map_loader_unit, action_unit;

//________________________________________________________________________________________

type

  Tsize = packed record      // same as TPoint in effect
    X: integer;
    Y: integer;
  end;

  Temf_header = packed record
    emf_type: dword;          // record type - this is a header record
    emf_size: dword;
    // size of each record in bytes - could be greater than the size of Temf_header
    emf_bounds: TRect;        // records bounds rect in dots
    emf_frame: TRect;         // image rect in 1/100th mm units
    emf_signature: dword;     // ???
    emf_version: dword;       // ???
    emf_bytes: dword;         // size of metafile
    emf_records: dword;       // number of records in the metafile
    emf_handles: word;
    // !NOT dword  number of handles in the metafile (not zero, which is reserved)
    emf_reserved: word;       // !NOT dword  reserved - must be zero
    emf_description: dword;
    // number of chars in the description string (unicode), or zero if no description
    emf_description_offset: dword;  // offset to the description in the metafile, or zero ditto
    emf_palette_entries: dword;     // number of entries in the palette
    emf_ref_size_dots: Tsize;       // size of the reference device in dots
    emf_ref_size_mm: Tsize;
    // size of the reference device in mm (??? rounded, see size in microns below)
    emf_pixel_format: dword;
    // size of pixel format info, or zero if no pixel formats used (bitmaps)
    emf_pixel_format_offset: dword; // offset to pixel format info, or zero ditto
    emf_OpenGL: dword;              // True (not zero) if any OpenGL commands are used in the metafile
    emf_ref_size_micron: Tsize;     // size of reference device in microns (mm/1000)
  end;

  Temf_pict = packed record   // Windows CF_METAFILEPICT clipboard format
    emf_map: integer;        // mapping mode
    emf_X: integer;
    emf_Y: integer;
    emf_handle: HDC;
  end;


const
  bgs_help1_str: string = '    `0Background  Shapes`9' +
    '||These simple background drawing functions allow the addition to the track plan of such features as baseboard edges and joint lines,' + ' or other fixed obstructions such as a control panel space or turntable well.' + '||Simple outline drawings of buildings and structures such as a signal box or goods shed can also be added,' + ' but to create more detailed and colourful layout drawings of structures and scenic features, see instead the `0sketchboard`3 function.' + '||In addition to the geometrical background shapes, a "picture" shape is a rectangle which can contain an image, such as may be created in Windows Paint or similar programs, or obtained from a scanner.' + '||Scanned images from maps, published track plans, hand-drawn sketches and tracings can in this way be used as a background guide to your track design.' + ' In addition, scanned images of printed track templates, rail-rubbings or even scanned items of actual trackwork can be displayed full-size and' + ' incorporated into your track plan.' + '||Please remember that scanning published material may require the permission of the copyright owner.' + '||You can also add short labels to identify different parts of the drawing. "Short" means not more than 46 characters. If you want to' + ' make detailed notes about the drawing you can enter these in the `0memo`3 text for each template.' + '||It is also possible to add target mark shapes (small cross-hairs symbols) to the drawing as alignment and reference markers.' + '||rp.gif These background shapes will not actually appear on the trackpad unless the `0TRACKPAD > TRACKPAD BACKGROUND OPTIONS > SHOW BACKGROUND SHAPES`1 menu is selected.' + '||rp.gif Background shapes can be quickly toggled on and off by pressing the `0SHIFT+HOME`2 keys on the keyboard. This is very useful when working over picture shapes.' + '||Your collection of background shapes and labels can be saved (in a .bgs3 file) and reloaded again whenever needed.' + ' It is saved separately from your track template data files (.box3 files), so that you can easily re-use the baseboard outlines with different track plans.' + '||Shapes and labels can be created in several different ways:||';


  add_options_help_str: string =
    'green_panel_begin tree.gif  Before adding shapes you may want to zoom out on the trackpad enough to see where you want to place the shape, clear of the dialog window.green_panel_end' + '|    `0Adding  Background  Shapes`9' + '||First select the type of background shape in the lists on the `0NEW SHAPE`1 tab, and the required appearance. For a label shape, enter the required text in the box. For other shapes you can enter an optional name for the shape.' + '||After selecting a `0picture shape`3, the option buttons will change. Click the `0ADD PICTURE SHAPE`1 button and follow the dialogs which appear.' + '||For all other shapes, you have 3 options when adding the new shape:' + '||1. `0by drawing`1 After selecting this option, click the `0add shape`1 button. You can then draw on the trackpad with the mouse or pen to create the new shape.' + '||If you are drawing `0line`1 shapes, you can draw as many lines as needed. To finish drawing lines click the `0STOP`z button on the trackpad, or press the `0ESC`2 key on the keyboard.' + '||For all other shapes drag a rectangle to contain it.' + '||2. `0by entering dimensions`1 After selecting this option, click the `0add shape`1 button. You can then enter the required dimensions for the shape.' + '||3. `0at the clicked locations`1 If this option is selected, you must first click two locations on the trackpad <u>before</u> clicking the `0add shape`1 button.' + ' The new shape will be created between these two locations when you click the button.' + '||If you make a mistake in clicking locations, you can keep clicking new ones. Only the final two clicked locations will be used.' + '||If you wish you can click locations on the trackpad before going to this background shapes dialog. By this means you can zoom and pan the trackpad as needed to create large shapes accurately.' + ' However such clicked locations will not be visible on the trackpad until the shape is created.' + '||green_panel_begin tree.gif  Any background shape can be adjusted for size and position after it has been added. Click the `0modify shape`1 tab to do that.green_panel_end' + '|green_panel_begin tree.gif  The shapes are drawn on the trackpad in the order shown in the list, and later ones in the list will obscure earlier ones.' + '||You can re-arrange the order later by clicking the brown up-down buttons which appear alongside the list when there is more than one shape listed. The buttons repeat if held down.' + '||The shapes will always be overdrawn by any overlapping part of the track drawing.' + '||rp.gif When a shape is clicked in the list, it is temporarily drawn in front of all the others and outlined in red. It returns to its place in the order when you use the brown up-down buttons.green_panel_end';

  inv_shape_help: string = '      Invalid  Shape  Data' +
    '||Make sure when entering shapes that the data is valid.' + '||For a LINE :'
    + '|If  X1 = X2 , the line will be vertical.' + '|If  Y1 = Y2 , the line will be horizontal.'
    + '|But if  X1 = X2  AND  Y1 = Y2  the line will have zero length and cannot be drawn.' +
    '||For a RECTANGLE or CIRCLE or PICTURE :' +
    '|If  X1 = X2  OR  Y1 = Y2  the shape will have zero width or zero height and cannot be drawn.';

  x_str: string = '      X  Dimensions' +
    '||X dimensions are measured across the screen from the left, and read on the bottom grid scale.'
    +
    '||Dimensions measured forwards to the right from the origin (0 mark) are positive; dimensions measured backwards to the left from the origin are negative.' + '||The figures which appear here initially represent the most recently clicked location(s) on the trackpad. If you want your shape or label to be at this location, just press the ENTER key for each dimension.' + '||Note that unless you use conversion factors the dimension should be entered in mm. To avoid confusion, set the grid spacing in mm before adding background shapes (click the GENERAL OPTIONS tab > GRID... button).' + '||For more information about using conversion factors click the ? HELP button.';


  y_str: string = '      Y Dimensions' +
    '||Y dimensions are measured up the screen from the bottom, and read on the left grid scale.'
    + '||Dimensions measured upwards from the origin (0 mark) are positive; dimensions measured downwards from the origin are negative.' + '||The figures which appear here initially represent the most recently clicked location(s) on the trackpad. If you want your shape or label to be at this location, just press the ENTER key for each dimension.' + '||Note that unless you use conversion factors the dimension should be entered in mm. To avoid confusion, set the grid spacing in mm before adding background shapes (click the GENERAL OPTIONS tab > GRID... button).' + '||For more information about using conversion factors click the ? HELP button.';

  w_str: string = '      Width  Dimensions' +
    '||Width dimensions are measured across the screen, representing the distance between the left and right edges of the background shape.' + '||Width dimensions are always positive. Negative or zero width dimensions are invalid.' + '||The figures which appear here initially represent the current width of the selected background shape, or the suggested width for a new background shape.' + '||For a new picture shape, you can enter an approximate width initially or leave the suggested width. There are subsequent functions to scale a picture shape to the exact size required after the image has been loaded into it.';


  get_str: string = '      Data  for  Background  Shapes' +
    '||Shapes are defined by entering the X,Y co-ordinate dimensions for 2 separate points as follows:'
    +
    '||For a LINE, enter the dimensions to the 2 end points of the line, (X1,Y1) and (X2,Y2).'
    + '||For a RECTANGLE, enter the dimensions to the bottom-left corner (X1,Y1) and the top-right corner (X2,Y2).'
    + '||For a CIRCLE, enter the dimensions to the bottom-left corner (X1,Y1) and the top-right corner (X2,Y2) of an imaginary enclosing rectangle. If this enclosing rectangle is not a square,' + ' (with all sides of equal length), the circle will become an ellipse.' + '||( When exported in DXF file format, an ellipse will be "averaged" to a circle. There are few elliptical features on a model railway.)' + '||For a LABEL, enter the dimensions to the top-left corner (X,Y).' + '||For a TARGET MARK, enter the dimensions to the centre (X,Y), and the length (each way) of the horizontal and vertical arms.' + '||For a PICTURE SHAPE enter the requested dimensions according to the source of the image.' + '||Note that unless you use conversion factors all dimensions should be entered in mm. To avoid confusion, set the grid spacing in mm before adding background shapes (click the GRID... button).' + '||For more information about using conversion factors click the ? HELP button.' + '||Handy Hint :' + '||The figures which appear initially represent the most recently clicked location(s) on the trackpad. If you want your shape or label to be at this location, just press the ENTER key for each dimension.' + '||An easier way to do this is simply to click the ADD CLICKED SHAPE button.' + ' Click the two ends or opposite corners of the required location first.' + ' For a LABEL, just click the top-left corner. For a TARGET MARK, click the centre location.' + '||If you request a pre-set dimension by entering a slash "/" the relevant dimension will be taken from the positions of the most recent copies of the spacing-ring tool.' + ' This is useful if you need a shape which is a specified distance from the rails, for example. Set the size of the ring accordingly.' + '||If there are insufficient ring copies the current spacing-ring position will be used instead.' + ' It is not necessary to request the pre-set for all four dimensions, any figures entered will be used instead, which might be useful occasionally.' + '||Select the UTILS > DUMMY VEHICLE • SPACING-RING menu item and click the ? HELP button for more information about using the spacing-ring and making ring copies.';

  empty_picture_str: string = '(empty picture)';

  twist_help_str: string = '    Twist  Bitmap  Image' +
    '||This function will twist (rotate) the bitmap image contained in a picture shape.'
    + '||There are 4 options for the rotation centre point around which the image will be twisted:'
    + '||a) the centre of the picture shape.' + '|b) the zero origin of the trackpad grid.' +
    '|c) the current position of the notch.' + '|d) the current position of the spacing-ring tool.'
    + '||These options are selected in the `0PICTURE OPTIONS tab > TWIST SELECT PICTURE AROUND:`1 panel and the `0MODIFY ALL tab > ROTATE ALL AROUND:`1 panel.' + '||If necessary the picture shape outline will be enlarged to accommodate the twisted image. The `0crop/combine`3 function can be used to reduce its size subsequently.' + '||If you need to re-position the picture shape subsequently - click the `0MODIFY SHAPE tab > SHIFT BY...`1 button or the `0MODIFY SHAPE tab > MOUSE ACTIONS: > SHIFT`1 button.' + '||Remember to save a new BGS data file containing the new positions of the picture shape outlines and a new image file.' + '||The primary purpose of this twist function for bitmap images is to align the sections of scanned maps and track plans which cannot be scanned in one piece, the angle of twist needed in each case being relatively small.' + '||If you know the exact twist angle required, this can be entered directly.' + '||Usually the angle is not known. In this case you can add "marker lines" to the trackpad which Templot0 will use to calculate the required twist angle.' + ' This is what to do to align two bitmap images:' + '||1. Scan the sections with plenty of overlap, so that there is some chosen feature which appears on both of them.' + '||2. Create picture shapes to contain the images and load the image files into them. Click the `0HELP`1 menu items for more information about how to do this.' + '||If images overlap, remember that picture shapes are displayed in the order listed. When a new shape is added to the list it is inserted immediately below the one currently selected in the list.' + ' In this way you can arrange the images to overlap in the desired order.' + '||3. It is usually helpful to display one of the images transparently. Select it in the list and then tick the MODIFY SHAPE tab > PICTURE SHAPE IMAGE : > TRANSPARENT tickbox.' + ' You may need to set a lighter trackpad "paper" colour for this to be effective.' + '||4. Using any of the normal LINE SHAPE functions add a marker line along the chosen feature on the image which is to remain fixed (DRAW WITH MOUSE or click the end points and then ADD CLICKED SHAPE button).' + ' The marker line can be adjusted in the usual way using the MODIFY SHAPE tab > MOUSE ACTIONS: > CORNER 1 and CORNER 2 mouse actions.' + '||5. Make sure this marker line is shown selected in the list, and then click the MODIFY SHAPE tab > PICTURE SHAPE IMAGE :: > MARKER LINE - FIXED button.' + '||6. Repeat the process to mark the same chosen feature on the image which is to be twisted. Then click the MODIFY SHAPE tab > PICTURE SHAPE IMAGE :: > MARKER LINE - TWIST button.' + '||7. Click to select the picture shape which is to be twisted in the list.' + '||8. Click the MODIFY SHAPE tab > PICTURE SHAPE IMAGE : > TWIST... button. Choose the USE MARKER LINES option (and make a note of the calculated angle between them which is shown).' + '||For a large image, this process may take several minutes to complete. The picture shape outline will be enlarged to accommodate the twisted image.' + '||9. You will now need to re-position the twisted picture shape to bring the chosen feature into alignment with the fixed image - click the MODIFY SHAPE tab > MOUSE ACTIONS: > SHIFT button.' + ' It may be helpful to add additional marker lines over the fixed image to aid alignment.' + '||If you do not set a FIXED marker line, Templot0 will assume a horizontal one, or use the current angle of the pegging notch if the PICTURE OPTIONS tab > TWIST USING NOTCH-ANGLE AS FIXED-MARKER tickbox is ticked.' + ' This is useful if you have already aligned track to an existing picture shape, and wish to align another picture shape to that track.' + ' You must set a TWIST marker line on the image to be twisted.'
    {
  +'||There are two options for the method used to twist the bitmap image:'

  +'||The PICTURE OPTIONS tab > TWIST QUALITY > LOW/B-W setting is faster, but may degrade the image by leaving flecks of white across it. This effect will increase as the twist angle increases, to a maximum at 45 degrees.'
  +' This option is therefore more suitable for black and white images.'

  +'||The PICTURE OPTIONS tab > TWIST QUALITY > HI/GREY setting is slower, but avoids this effect at the expense of some softening of the image. This option is more suitable for grey-scale and colour images.'
} + '||Handy Hints:' + '|Twisting images makes great demands on your system''s memory and resources. Don''t have more picture shapes on the trackpad than you need, and keep image files as small as possible.' + ' There is seldom any need to scan at resolutions greater than 300 dpi. Memory requirements will be significantly reduced if you scan in black && white only, rather than grey-scale or colour.' + '||If lengthy disk drive activity takes place - please be patient, remember that you have to align your images only once. If you experience problems, quit Templot, free up some disk space by deleting unwanted files,' + ' restart Templot0 and try again. If problems persist, restart Templot0 and do not use this function.' + '||If your system fails to twist the bitmap image here in Templot, you may have other graphics software which is able to do it when Templot0 is not running.' + ' If you have made a note of the calculated angle between the marker lines, you can enter the angle in that software. It may be necessary to change the sign of the angle (to the opposite direction).' + ' The twisted bitmap file can then be reloaded into a new picture shape.' + '||When drawing marker lines on zoomed-in images, it can be helpful to use wider lines - click the FONTS / COLOURS tab > TRACKPAD SHAPES LINE WIDTH options.' + '||If you can bear to do it, it is very useful to draw a thin straight line in pencil down the full length of your original track plan before scanning it in sections, as a guide for the marker lines.' + '||Every time an image is twisted, the enclosing picture shape outline is enlarged by the addition of "white triangles" in the corners. If the same image is repeatedly twisted, it will soon become very large.' + ' If a twist is not successful it is better to delete the picture shape, and then reload the original image into a new one for another try.' + '||When you have successfully twisted a bitmap image to the correct angle, the `0crop/combine`3 function can be used to reduce its size.' + '||If the FONTS / COLOURS tab > TRACKPAD SHAPES LINE WIDTH option is set to 1, any border lines are entirely within the image area, not around the outside of it.' + ' This means that when aligning two images side-by-side with the mouse action, the border lines should also be side-by-side, not overlapping.' + ' (If the line width is set to more than 1, the border lines should overlap by half the additional width.)' + ' When aligning images it may be helpful to de-select the borders (PICTURE SHAPES tab).' + '||N.B. Please bear in mind that Templot0 is not fully-fledged graphics imaging software - this "bare-bones" twist function is intended primarily to permit the alignment of scanned track plans.' + ' Some degradation of the image quality is inevitable if twist angles exceed a few degrees.';

var
  cursor_saved: TCursor;
  form_scaling: boolean = False;
  // flag - otherwise ScrollInView on resize prevents form rescaling properly.
  bgs_save_hide: boolean = False;

  user_save_shapes_path: string = '';   // 0.93.a ...
  user_load_shapes_path: string = '';

  user_save_img_path: string = '';
  user_load_img_path: string = '';

  target_arm: double = 10.0;      // default arm length on the target marks (10mm each way).
  circle_dia: double = 250;       // default circle diameter 250mm.

  input_factor: double = 1.0;
  dxf_file: TextFile;
  code_str: string = '';
  value_str: string = '';

  comma_dp: boolean = False;   // 0.94.a True = system is using comma as decimal point

  shape_count: integer = 0;

  his_dxf_file_name: string = '';

  input_scale_factor: double = 1.0;
  input_shift_x: double = 0;
  input_shift_y: double = 0;

  xmax: double = 1000;     // default drawing limits.
  ymax: double = 1000;
  xmin: double = -25;
  ymin: double = -25;

  marker_angle_fixed: double = 0;
  marker_angle_twist: double = 0;

  marker_angle_twist_is_defined: boolean = False;

  count_limit: integer = 32000;
// max number of loaded shapes allowed (leaving 768 spaces for additional drawing).


procedure bgfile_error(str: string); forward;
procedure import_dxf; forward;

function reload_picture_image(scanning, pasting, meta, dropped, adjust_aspect: boolean): boolean;
  forward;

procedure twist_picture(i: integer; krot: double; do_container, all, sync: boolean); forward;
// rotate bitmap supplied krot clockwise.

//______________________________________________________________________________


function get_EMF_from_file_to_memory(file_str: string; var new_met_DC_handle: HDC): boolean;

  // return metafile handle

// EMF
//var
//  met_DC_handle: HDC;
//  failed: boolean;
//  met_size: integer;
//  met_copied: integer;
//  p: Pointer;
//
begin
//EMF
//  Result := False;  // init
//  failed := False;
//
//  try
//    try
//      met_DC_handle := GetEnhMetaFile(PChar(file_str));   // load EMF file
//    except
//      failed := True;
//      EXIT;
//    end;//try
//
//    try
//      met_size := GetEnhMetaFileBits(met_DC_handle, 0, nil);   // get size of EMF data
//    except
//      failed := True;
//      EXIT;
//    end;//try
//
//    try
//      GetMem(p, met_size);    // memory space for it
//    except
//      memory_alert;          // tell him what's happened.
//      p := nil;
//      EXIT;
//    end;//try
//
//    try
//      met_copied := GetEnhMetaFileBits(met_DC_handle, met_size, p);   // get the EMF contents
//    except
//      failed := True;
//      EXIT;
//    end;//try
//
//    try
//      new_met_DC_handle := SetEnhMetaFileBits(met_copied, p);    // put them in memory
//    except
//      failed := True;
//      EXIT;
//    end;//try
//
//    try
//      DeleteEnhMetaFile(met_DC_handle);
//      // release the file handle     (n.b. CloseEnh.. after saving a file.  DeleteEnh.. after loading a file)
//      FreeMem(p);
//    except
//      failed := True;
//      EXIT;
//    end;//try
//
//    Result := not failed;
//
//  finally
//    if failed = True then
//      show_modal_message('error: sorry, unable to load the EMF metafile');
//  end;
end;
//______________________________________________________________________________

function get_metafile_for_existing_shape(image_file_str: string; bgshape: Tbgshape): boolean;
// EMF
//var
//  emf_header: Temf_header;

begin
// EMF
//  Result := False;    // init
//
//  with bgshape do begin
//    with bgimage.image_shape do begin
//
//      if get_EMF_from_file_to_memory(image_file_str, image_metafile.emf_HDC) = True then begin
//        if GetEnhMetaFileHeader(image_metafile.emf_HDC, SizeOf(emf_header), @emf_header) <>
//          0 then begin
//          with emf_header.emf_frame do begin                  //  full image frame
//            image_metafile.emf_width_mm := ABS(Right - Left) / 100;
//            image_metafile.emf_height_mm := ABS(Bottom - Top) / 100;
//          end;//with
//
//          if image_metafile.emf_width_mm > minfp   // no zero div or neg
//          then begin
//            image_width := 6000;
//            // arbitrary    not meaningful for EMF, used to set aspect ratio for display
//            image_height :=
//              Round(image_width * image_metafile.emf_height_mm / image_metafile.emf_width_mm);
//
//            bgnd_shape.picture_is_metafile := True;  // into file
//
//            Result := True;
//          end;
//        end;
//      end;
//
//    end;//with
//  end;//with
end;
//______________________________________________________________________________

function create_picture_shape_image_from_file(image_file_str: string; bgshape: Tbgshape;
  var img_width, img_height: integer): boolean;    // 291a

var
  load_picture: TPicture;
  ext_str: string;

begin
  Result := False;  // init...
  img_width := 0;
  img_height := 0;

  ext_str := LowerCase(ExtractFileExt(image_file_str));

  with bgshape do begin

    if bgimage <> nil       // clear any existing
    then begin
      bgimage.image_shape.image_bitmap.Free;      // free the bitmaps.
      bgimage.image_shape.rotated_bitmap.Free;

      bgimage.image_shape.rotated_picture.Free;

      bgimage.Free;                               // and free the image object.
    end;

    bgimage := Tbgimage.Create;     // create new image  3-2-01.

    bgnd_shape.show_transparent := False;     // 0.93.a
    bgnd_shape.picture_is_metafile := False;  // 213b

    with bgimage.image_shape do begin

      // these never used if metafile...

      image_bitmap := TBitmap.Create;

      rotated_bitmap := TBitmap.Create;

      rotated_picture := TPicture.Create;

      // OT-FIRST   EMF metafile ...

      if ext_str = '.emf' then
        Result := get_metafile_for_existing_shape(image_file_str, bgshape)
      else begin    // not EMF

        load_picture := TPicture.Create; //0.93.a

        try
          load_picture.LoadFromFile(image_file_str);

          if load_picture.Graphic is TIcon    // convert it to bitmap
          then begin
            image_bitmap.Width := load_picture.Graphic.Width;
            image_bitmap.Height := load_picture.Graphic.Height;

            image_bitmap.Canvas.Draw(0, 0, load_picture.Graphic);
          end
          else
            image_bitmap.Assign(load_picture.Graphic);

          Result := True;
        except
          on EInvalidGraphic do
            show_modal_message('error - the ' + UpperCase(StringReplace(ext_str, '.', '', [])) +
              ' file format is not supported');
        end;

        image_width := image_bitmap.Width;
        image_height := image_bitmap.Height;

        bgnd_shape.picture_is_metafile := False;  // into file

        load_picture.Free;
      end;

      if Result = False then begin
        bgfile_error(image_file_str);  // failed load
        image_bitmap.Width := 200;       // arbitrary.
        image_bitmap.Height := 150;      // arbitrary.

        image_width := image_bitmap.Width;
        image_height := image_bitmap.Height;

        with image_bitmap.Canvas do begin     // blank the picture area...
          Brush.Color := clWhite;
          Brush.Style := bsSolid;
          FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
        end;//with

        bgnd_shape.picture_is_metafile := False;
      end;

      img_width := image_width;      // return these
      img_height := image_height;

    end;//with bgimage
  end;//with bgshape
end;
//______________________________________________________________________________

procedure normalize_line(var p1, p2: Tpex);  //214a

// adjust line ends p1,p2, so that p1.x is left of p2.x

var
  temp: double;

begin
  if p2.x < p1.x          // make sure p2 end is on right.
  then begin
    temp := p1.x;
    p1.x := p2.x;
    p2.x := temp;

    temp := p1.y;
    p1.y := p2.y;
    p2.y := temp;
  end;
end;
//______________________________________________________________________________

procedure normalize_rectangle(var p1, p2: Tpex);  //214a

// adjust rectangle corners p1,p2, so that p1 is bottom-left, p2 is top-right

var
  in_p1, in_p2: Tpex;

begin
  in_p1 := p1;
  in_p2 := p2;

  p1.x := Min(in_p1.x, in_p2.x);
  p1.y := Min(in_p1.y, in_p2.y);

  p2.x := Max(in_p1.x, in_p2.x);
  p2.y := Max(in_p1.y, in_p2.y);
end;
//______________________________________________________________________________

procedure enable_style_box;

begin
  with bgnd_form do begin
    style_groupbox.Enabled := True;
    clear_radio_button.Enabled := True;
    solid_infill_radio_button.Enabled := True;
    hatched_radio_button.Enabled := True;
  end;//with
end;
//_______________________________________________________________________________________

procedure disable_style_box;

begin
  with bgnd_form do begin
    style_groupbox.Enabled := False;
    clear_radio_button.Enabled := False;
    solid_infill_radio_button.Enabled := False;
    hatched_radio_button.Enabled := False;
  end;//with
end;
//________________________________________________________________________________________

procedure shapes_current_state;
// set the current state of the form to match the selected shape.

var
  n: integer;
  now_shape: Tbgnd_shape;
  code_str, shape_str, meta_str: string;

begin
  with bgnd_form do begin

    if (line_radio_button.Checked = True) and (by_drawing_radiobutton.Checked = True) then
      add_shape_button.Caption := 'add  shapes'
    else
      add_shape_button.Caption := 'add  shape';

    with bgnd_shapes_listbox do begin

      if Items.Count = 1 then
        count_label.Caption := '1  shape'
      else
        count_label.Caption := IntToStr(Items.Count) + '  shapes';

      move_to_top_button.Visible := (Items.Count > 1);      // 205a ...
      move_to_bottom_button.Visible := (Items.Count > 1);
      move_up_button.Visible := (Items.Count > 1);
      move_down_button.Visible := (Items.Count > 1);

      bgnd_edit_menu.Enabled := (Items.Count > 0);    // 214a
      bgnd_zoom_menu.Enabled := (Items.Count > 0);    // 214a
      bgnd_all_menu.Enabled := (Items.Count > 0);     // 219a

      if Items.Count < 1                              // list empty...
      then begin
        Hide;                               // reveal "no shapes" label.

        // 214a ...

        hide_tick_image.Hide;
        show_tick_image.Hide;
        show_on_trackpad_static.Hide;

        hide_output_tick_image.Hide;
        show_output_tick_image.Hide;
        show_on_output_static.Hide;

        bgnd_show_menu.Enabled := False;

        delete_menu_entry.Enabled := False;
        delete_all_menu_entry.Enabled := False;
        save_all_menu_entry.Enabled := False;

        rename_menu_entry.Enabled := False;
        dimensions_menu_entry.Enabled := False;

        zoom_fit_shape_menu_entry.Enabled := False;
        zoom_fit_all_shapes_menu_entry.Enabled := False;

        pad_form.save_shapes_menu_entry.Enabled := False;

        add_file_menu_entry.Enabled := False;
        pad_form.add_shapes_menu_entry.Enabled := False;

        //all_groupbox.Visible:=False;

        all_90degs_acw_button.Enabled := False;     // 213b..
        all_90degs_cw_button.Enabled := False;

        mirror_x_button.Enabled := False;
        mirror_y_button.Enabled := False;

        break_all_rectangles_menu_entry.Enabled := False;

        scale_all_by_button.Enabled := False;
        shift_all_by_button.Enabled := False;
        rotate_all_by_button.Enabled := False;
        delete_50_button.Enabled := False;
        delete_25_button.Enabled := False;

        mouse_scale_all_button.Enabled := False;
        mouse_shift_all_button.Enabled := False;
        mouse_rotate_all_button.Enabled := False;

        modify_panel.Visible := False;
        pad_form.mouse_actions_shapes_menu_entry.Enabled := False;

        type_label.Caption := '';
        p1_label.Caption := '';
        p2_label.Caption := '';
        disable_style_box;

        trans_checkbox.Checked := False;

        name_editbox.Enabled := True;  // 0.93.a


        EXIT;
      end;

      Show;   // show box, and cover label.

      // 214a ...

      hide_tick_image.Show;
      show_tick_image.Show;
      show_on_trackpad_static.Show;

      hide_output_tick_image.Show;
      show_output_tick_image.Show;
      show_on_output_static.Show;

      bgnd_show_menu.Enabled := True;

      n := ItemIndex;      // line selected.
      if n < 0 then
        n := 0;  // if none make it the top one.

      now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;
    end;//with

    with now_shape do begin

      // show current shape details...

      show_tick_image.Visible := ((hide_bits and $01) = 0);
      // byte,  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both
      show_output_tick_image.Visible := ((hide_bits and $02) = 0);

      show_on_trackpad_menu_entry.Checked := ((hide_bits and $01) = 0);
      show_on_output_menu_entry.Checked := ((hide_bits and $02) = 0);

      name_editbox.Text := '';    //shape_name;

      trans_checkbox.Checked := ((show_transparent = True) and (shape_code = -1));
      // 0.93.a now in file

      with bgnd_shapes_listbox do begin
        Items.Strings[n] := shape_name;         // name might have been modified.
        ItemIndex := n;                         // show selected the new name.
      end;//with

      case shape_code of
        -1: begin                      // picture.

          solid_radio_button.Enabled := False;
          dotted_radio_button.Enabled := False;

          disable_style_box;

          if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgnd_shape.picture_is_metafile = True then
            meta_str := ' (EMF) '
          else
            meta_str := '';

          if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgimage = nil then
            code_str := 'picture'
          else begin
            with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgimage.image_shape do begin
              code_str :=
                'picture' + meta_str + ' :  ' + IntToStr(image_width) + '  x  ' + IntToStr(image_height) + '  native  dots';
            end;//with
          end;

          marker_line_twist_button.Enabled := False;
          marker_line_fixed_button.Enabled := False;

          picture_radio_button.Checked := True;
          reload_picture_button.Enabled := True;
          save_picture_button.Enabled := True;
          twist_picture_button.Enabled := True;

          // OT-FIRST convert_24bit_button.Enabled:=True;         // 215b
          // OT-FIRST convert_24bit_menu_entry.Enabled:=True;     // 215b

          auto_fit_picture_button.Enabled := True;
          copy_image_button.Enabled :=
            not Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgnd_shape.picture_is_metafile;

          crop_rectangle_button.Enabled := False; // 214a

          paste_button.Enabled := Clipboard.HasFormat(CF_BITMAP);
          scan_button.Enabled := True;

          straighten_picture_button.Enabled := True;
          wrap_picture_button.Enabled := True;
          trans_checkbox.Enabled := True;


          corner2_button.Enabled := True;

          break_rectangle_menu_entry.Enabled := False;
          combine_pictures_menu_entry.Enabled := False;

          pad_form.shift_corner2_menu_entry.Enabled := True;
        end;

        0: begin                      // line.

          solid_radio_button.Enabled := True;
          dotted_radio_button.Enabled := True;

          disable_style_box;

          name_editbox.Enabled := True;  // 0.93.a

          code_str := 'line';
          line_radio_button.Checked := True;

          marker_line_twist_button.Enabled := True;
          marker_line_fixed_button.Enabled :=
            not use_notch_fixed_marker_checkbox.Checked;

          reload_picture_button.Enabled := False;
          save_picture_button.Enabled := False;
          twist_picture_button.Enabled := False;

          // OT-FIRST convert_24bit_button.Enabled:=False;        // 215b
          // OT-FIRST convert_24bit_menu_entry.Enabled:=False;    // 215b

          auto_fit_picture_button.Enabled := False;
          copy_image_button.Enabled := False;

          crop_rectangle_button.Enabled := False;  // 214a

          paste_button.Enabled := False;
          scan_button.Enabled := False;

          straighten_picture_button.Enabled := False;
          wrap_picture_button.Enabled := False;
          trans_checkbox.Enabled := False;

          if shape_style = 2 then begin
            shape_str := 'dotted  ';
            dotted_radio_button.Checked := True;
          end
          else begin
            shape_str := 'solid  ';
            solid_radio_button.Checked := True;
          end;
          corner2_button.Enabled := True;

          break_rectangle_menu_entry.Enabled := False;
          combine_pictures_menu_entry.Enabled := False;

          pad_form.shift_corner2_menu_entry.Enabled := True;
        end;

        1: begin                      // rectangle.

          solid_radio_button.Enabled := False;
          dotted_radio_button.Enabled := False;

          enable_style_box;

          name_editbox.Enabled := True;  // 0.93.a

          code_str := 'rectangle';
          rect_radio_button.Checked := True;
          reload_picture_button.Enabled := False;
          save_picture_button.Enabled := False;
          twist_picture_button.Enabled := False;

          // OT-FIRST convert_24bit_button.Enabled:=False;      // 215b
          // OT-FIRST convert_24bit_menu_entry.Enabled:=False;  // 215b

          auto_fit_picture_button.Enabled := False;
          copy_image_button.Enabled := False;

          crop_rectangle_button.Enabled := True;  // 214a

          paste_button.Enabled := False;
          scan_button.Enabled := False;

          straighten_picture_button.Enabled := False;
          wrap_picture_button.Enabled := False;
          trans_checkbox.Enabled := False;

          marker_line_twist_button.Enabled := False;
          marker_line_fixed_button.Enabled := False;

          case shape_style of
            0: begin
              shape_str := 'clear  ';
              clear_radio_button.Checked := True;
            end;
            1: begin
              shape_str := 'solid  ';
              solid_infill_radio_button.Checked := True;
            end;
            2: begin
              shape_str := 'hatched  ';
              hatched_radio_button.Checked := True;
            end;
          end;//case
          corner2_button.Enabled := True;

          break_rectangle_menu_entry.Enabled := True;
          combine_pictures_menu_entry.Enabled := True;

          pad_form.shift_corner2_menu_entry.Enabled := True;
        end;

        2: begin                      // circle.

          solid_radio_button.Enabled := False;
          dotted_radio_button.Enabled := False;

          enable_style_box;

          name_editbox.Enabled := True;  // 0.93.a

          if ABS(ABS(p2.x - p1.x) - ABS(p2.y - p1.y)) > minfp  // elliptical?   214a
          then
            code_str := 'ellipse'
          else
            code_str := 'circle';

          circle_radio_button.Checked := True;
          reload_picture_button.Enabled := False;
          save_picture_button.Enabled := False;
          twist_picture_button.Enabled := False;

          // OT-FIRST convert_24bit_button.Enabled:=False;        // 215b
          // OT-FIRST convert_24bit_menu_entry.Enabled:=False;    // 215b

          auto_fit_picture_button.Enabled := False;
          copy_image_button.Enabled := False;

          crop_rectangle_button.Enabled := False;  // 214a

          paste_button.Enabled := False;
          scan_button.Enabled := False;

          straighten_picture_button.Enabled := False;
          wrap_picture_button.Enabled := False;
          trans_checkbox.Enabled := False;

          marker_line_twist_button.Enabled := False;
          marker_line_fixed_button.Enabled := False;

          case shape_style of
            0: begin
              shape_str := 'clear  ';
              clear_radio_button.Checked := True;
            end;
            1: begin
              shape_str := 'solid  ';
              solid_infill_radio_button.Checked := True;
            end;
            2: begin
              shape_str := 'hatched  ';
              hatched_radio_button.Checked := True;
            end;
          end;//case
          corner2_button.Enabled := True;

          break_rectangle_menu_entry.Enabled := False;
          combine_pictures_menu_entry.Enabled := False;

          pad_form.shift_corner2_menu_entry.Enabled := True;
        end;

        3: begin                      // label.

          solid_radio_button.Enabled := False;
          dotted_radio_button.Enabled := False;

          disable_style_box;

          name_editbox.Enabled := True;  // 0.93.a

          code_str := 'label';
          label_radio_button.Checked := True;
          reload_picture_button.Enabled := False;
          save_picture_button.Enabled := False;
          twist_picture_button.Enabled := False;

          // OT-FIRST convert_24bit_button.Enabled:=False;        // 215b
          // OT-FIRST convert_24bit_menu_entry.Enabled:=False;    // 215b

          auto_fit_picture_button.Enabled := False;
          copy_image_button.Enabled := False;

          crop_rectangle_button.Enabled := False;  // 214a

          paste_button.Enabled := False;
          scan_button.Enabled := False;

          straighten_picture_button.Enabled := False;
          wrap_picture_button.Enabled := False;
          trans_checkbox.Enabled := False;

          marker_line_twist_button.Enabled := False;
          marker_line_fixed_button.Enabled := False;

          shape_str := '';
          corner2_button.Enabled := False;

          break_rectangle_menu_entry.Enabled := False;
          combine_pictures_menu_entry.Enabled := False;

          pad_form.shift_corner2_menu_entry.Enabled := False;
        end;

        4: begin                      // target mark

          solid_radio_button.Enabled := False;
          dotted_radio_button.Enabled := False;

          disable_style_box;

          name_editbox.Enabled := True;  // 0.93.a

          code_str := 'target  mark';
          target_radio_button.Checked := True;
          reload_picture_button.Enabled := False;
          save_picture_button.Enabled := False;
          twist_picture_button.Enabled := False;

          // OT-FIRST convert_24bit_button.Enabled:=False;        // 215b
          // OT-FIRST convert_24bit_menu_entry.Enabled:=False;    // 215b

          auto_fit_picture_button.Enabled := False;
          copy_image_button.Enabled := False;

          crop_rectangle_button.Enabled := False;  // 214a

          paste_button.Enabled := False;
          scan_button.Enabled := False;

          straighten_picture_button.Enabled := False;
          wrap_picture_button.Enabled := False;
          trans_checkbox.Enabled := False;

          marker_line_twist_button.Enabled := False;
          marker_line_fixed_button.Enabled := False;

          shape_str := '';
          corner2_button.Enabled := False;

          break_rectangle_menu_entry.Enabled := False;
          combine_pictures_menu_entry.Enabled := False;

          pad_form.shift_corner2_menu_entry.Enabled := False;
        end;

      end;//case

      if shape_code = 3 then
        rename_menu_entry.Caption := 'edit  label  text ...'
      else
        rename_menu_entry.Caption := 'rename  shape ...';

      type_label.Caption := shape_str + code_str;
      if shape_code < 3 then begin
        p1_label.Caption :=
          insert_crlf_str('   from :|X1 =  ' + round_str(p1.x, 2) + ' mm'
          + '|Y1 =  ' + round_str(p1.y, 2) + ' mm');

        p2_label.Caption :=
          insert_crlf_str('   to :|X2 =  ' + round_str(p2.x, 2) + ' mm'
          + '|Y2 =  ' + round_str(p2.y, 2) + ' mm');
      end
      else begin                               // label or target
        p1_label.Caption :=
          insert_crlf_str('     at :|X1 =  ' + round_str(p1.x, 2) + ' mm'
          + '|Y1 =  ' + round_str(p1.y, 2) + ' mm');

        p2_label.Caption := '';
      end;
    end;//with

    delete_menu_entry.Enabled := True;
    delete_all_menu_entry.Enabled := True;
    save_all_menu_entry.Enabled := True;

    rename_menu_entry.Enabled := True;
    dimensions_menu_entry.Enabled := True;

    zoom_fit_shape_menu_entry.Enabled := True;
    zoom_fit_all_shapes_menu_entry.Enabled := True;

    pad_form.save_shapes_menu_entry.Enabled := True;

    add_file_menu_entry.Enabled := True;
    pad_form.add_shapes_menu_entry.Enabled := True;

    all_90degs_acw_button.Enabled := True;     // 213b..
    all_90degs_cw_button.Enabled := True;

    mirror_x_button.Enabled := True;
    mirror_y_button.Enabled := True;

    break_all_rectangles_menu_entry.Enabled := True;

    scale_all_by_button.Enabled := True;
    shift_all_by_button.Enabled := True;
    rotate_all_by_button.Enabled := True;

    mouse_scale_all_button.Enabled := True;
    mouse_shift_all_button.Enabled := True;
    mouse_rotate_all_button.Enabled := True;

    modify_panel.Visible := True;
    pad_form.mouse_actions_shapes_menu_entry.Enabled := True;

    if bgnd_shapes_listbox.Items.Count > 3 then
      delete_25_button.Enabled := True
    else
      delete_25_button.Enabled := False;

    if bgnd_shapes_listbox.Items.Count > 1 then
      delete_50_button.Enabled := True
    else
      delete_50_button.Enabled := False;
  end;//with
end;
//______________________________________________________________________________________

procedure Tbgnd_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  form_scaling := True;            // flag to prevent status bar re-sizing while we scale the form.

  if size_updown.Position > size_updown.Tag
  // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                                           // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                                           // scale the form contents up.

  ClientHeight := VertScrollBar.Range;                               // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 4;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;
  // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;                           // and save for the next click.

  form_scaling := False;
end;
//______________________________________________________________________________

procedure Tbgnd_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  background  shapes  dialog', Color);
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.close_buttonClick(Sender: TObject);

begin
  Close;//Hide;
end;
//_______________________________________________________________________________________

procedure Tbgnd_form.how_panelClick(Sender: TObject);

begin
  help(0, bgs_help1_str + add_options_help_str, '');
end;
//____________________________________________________________________________________________

procedure Tbgnd_form.line_radio_buttonClick(Sender: TObject);

begin
  solid_radio_button.Enabled := True;
  dotted_radio_button.Enabled := True;

  picture_buttons_panel.Visible := False;   // OT-FIRST
  buttons_panel.Visible := True;            // hide add picture button


  if by_drawing_radiobutton.Checked = True then
    add_shape_button.Caption := 'add  shapes'
  else
    add_shape_button.Caption := 'add  shape';

  disable_style_box;
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.rect_radio_buttonClick(Sender: TObject);

begin
  solid_radio_button.Enabled := False;
  dotted_radio_button.Enabled := False;

  picture_buttons_panel.Visible := False;   // OT-FIRST
  buttons_panel.Visible := True;  // hide add picture button

  add_shape_button.Caption := 'add  shape';

  enable_style_box;
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.label_radio_buttonClick(Sender: TObject);

begin
  solid_radio_button.Enabled := False;
  dotted_radio_button.Enabled := False;

  picture_buttons_panel.Visible := False;   // OT-FIRST
  buttons_panel.Visible := True;  // hide add picture button

  add_shape_button.Caption := 'add  shape';

  disable_style_box;
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.target_radio_buttonClick(Sender: TObject);

begin
  solid_radio_button.Enabled := False;
  dotted_radio_button.Enabled := False;

  picture_buttons_panel.Visible := False;   // OT-FIRST
  buttons_panel.Visible := True;  // hide add picture button

  add_shape_button.Caption := 'add  shape';

  disable_style_box;
end;
//___________________________________________________________________________________________

procedure Tbgnd_form.picture_radio_buttonClick(Sender: TObject);

begin
  solid_radio_button.Enabled := False;
  dotted_radio_button.Enabled := False;

  //add_shape_button.Caption:='add  shape';

  picture_buttons_panel.Visible := True;   // OT-FIRST
  buttons_panel.Visible := False;  // show add picture button

  disable_style_box;
end;
//___________________________________________________________________________________________

function get_shape_dims(modifying: boolean; code: integer; var p1, p2: Tpex): boolean;

const
  target_str: string = '      Target Mark Arm Length' +
    '||Set the size of this target mark by entering the length (each way from the centre) of the horizontal and vertical arms.'
    + '||Note that unless you use conversion factors the dimension should be entered in mm. To avoid confusion, set the grid spacing in mm before adding background shapes (click the GRID... button).' + '||For more information about using conversion factors click the ? HELP button.' + '||The pre-set arm length (enter a slash / ) is 10mm.';

  circle_str: string = '      Circle Diameter' +
    '||Set the size of this circle by entering the diameter.' +
    '||Note that unless you use conversion factors the dimension should be entered in mm. To avoid confusion, set the grid spacing in mm before adding background shapes (click the GRID... button).' + '||For more information about using conversion factors click the ? HELP button.' + '||The pre-set diameter (enter a slash / ) is 250mm.' + '|||To create an ellipse instead of a circle, first create a circle. Then click the `0DIMENSIONS...`1 button on the `0MODIFY SHAPE``1 tab, to enter the dimensions for the ellipse.';

var
  str1, str2: string;
  i: integer;
  od: Toutdim;
  used1: boolean;
  ring_list_index: integer;
  type_code: integer;

  picture_done: boolean;

begin
  Result := False;       // init
  picture_done := False;

  type_code := code;     // init

  if (modifying = True) and (type_code = 2)   // allow conversion of circle to ellipse  214a
  then begin
    if ABS(ABS(p2.x - p1.x) - ABS(p2.y - p1.y)) > minfp then
      type_code := 99              // already an ellipse
    else begin
      i := alert(4, '    circle  or  ellipse ?',
        'Do you want this background shape to be a circle or an ellipse?'
        +
        '||For a circle, enter the X and Y dimensions at the centre, and the diameter.'
        +
        '||For an ellipse, enter the X and Y dimensions for opposite corners of the containing rectangle.',
        '', '', '', 'ellipse', 'cancel', 'circle', 0);
      case i of
        4:
          type_code := 99;   // over-ride code 2 below
        5:
          EXIT;
      end;//case
    end;
  end;

  if (modifying = False) and (type_code = -1)   // new picture, set default height= 0.75 x width
  then begin
    putdim(x_str, 1, 'picture :  first  corner  X  dimension  X1 ', p1.x,
      False, False, False, False);
    // neg ok, preset ok, allow zero, don't terminate on zero.
    putdim(y_str, 1, 'picture :  first  corner  Y  dimension  Y1 ', p1.y,
      False, False, False, False);   // ditto.
    i := putdim(w_str, 1, 'picture :  width ', p2.x - p1.x, True, True, True, False);
    // no neg, no preset, no zero, don't terminate on zero.

    if i <> 2 then
      EXIT;
    if getdims('background  picture  shape', get_str, bgnd_form, i, od) = True then
    begin
      p1.x := od[0];
      p1.y := od[1];
      p2.x := p1.x + ABS(od[2]);
      p2.y := p1.y + ABS(od[2]) * 3 / 4;
    end
    else
      EXIT;
    picture_done := True;
  end;

  if picture_done = False then begin
    case type_code of

      2: begin      // circle changes 214a

        putdim(x_str, 1, 'circle  centre  X  dimension',
          (p1.x + p2.x) / 2, False, False, False, False);  // neg ok, preset ok, allow zero, don't terminate on zero.
        putdim(y_str, 1, 'circle  centre  Y  dimension',
          (p1.y + p2.y) / 2, False, False, False, False);  // ditto.
        i := putdim(circle_str, 1, 'circle  diameter', ABS(p2.x - p1.x),
          True, False, True, False);
        // no neg, preset ok, no zero, don't terminate on zero.

        if i <> 2 then
          EXIT;
        if getdims('background  circle', get_str, bgnd_form, i, od) =
          True then begin
          if od[2] = def_req then
            circle_dia := 250           // reset to default (arbitrary)
          else
            circle_dia := ABS(od[2]);

          p1.x := od[0] - circle_dia / 2;
          p1.y := od[1] - circle_dia / 2;

          p2.x := p1.x + circle_dia;
          p2.y := p1.y + circle_dia;
        end
        else
          EXIT;
      end;

      3: begin
        putdim(x_str, 1, 'label  X  dimension  ( top - left ) ',
          p1.x, False, False, False, False);  // neg ok, preset ok, allow zero, don't terminate on zero.
        i := putdim(y_str, 1, 'label  Y  dimension  ( top - left ) ',
          p1.y, False, False, False, False);  // ditto.
        if i <> 1 then
          EXIT;
        if getdims('background  label', get_str, bgnd_form, i, od) =
          True then begin
          p1.x := od[0];
          p1.y := od[1];
          p2.x := 0;
          p2.y := 0;
        end
        else
          EXIT;
      end;

      4: begin
        putdim(x_str, 1, 'target  mark  centre  X  dimension',
          p1.x, False, False, False, False);  // neg ok, preset ok, allow zero, don't terminate on zero.
        putdim(y_str, 1, 'target  mark  centre  Y  dimension',
          p1.y, False, False, False, False);  // ditto.
        i := putdim(target_str, 1, 'target  arm  lengths', p2.x, True, False, True, False);
        // no neg, preset ok, no zero, don't terminate on zero.

        if i <> 2 then
          EXIT;
        if getdims('background  target  mark', get_str, bgnd_form, i, od) =
          True then begin
          p1.x := od[0];
          p1.y := od[1];
          if od[2] = def_req then
            p2.x := 10.0        // default (arbitrary).  // use p2 for the arm length.
          else
            p2.x := ABS(od[2]);
          p2.y := 0;
          target_arm := p2.x;
          // so can show the same for the next target.
        end
        else
          EXIT;
      end;

      else begin
        case code of
          -1: begin
            str1 := 'picture :  ';
            str2 := '  corner  ';
          end;
          0: begin
            str1 := 'line :  ';
            str2 := '  end  ';
          end;
          1: begin
            str1 := 'rectangle :  ';
            str2 := '  corner  ';
          end;
          2: begin
            str1 := 'ellipse :  ';
            str2 := '  corner  ';
          end;
          else begin
            str1 := '';
            str2 := '';
          end;
        end;//case

        putdim(x_str, 1, str1 + 'first' + str2 + 'X  dimension  X1 ',
          p1.x, False, False, False, False);   // neg ok, preset ok, allow zero, don't terminate on zero.
        putdim(y_str, 1, str1 + 'first' + str2 + 'Y  dimension  Y1 ',
          p1.y, False, False, False, False);   // ditto.
        putdim(x_str, 1, str1 + 'second' + str2 + 'X  dimension  X2 ',
          p2.x, False, False, False, False);  // ditto.
        i := putdim(y_str, 1, str1 + 'second' + str2 + 'Y  dimension  Y2 ',
          p2.y, False, False, False, False);  // ditto.

        if i <> 3 then
          EXIT;
        if getdims('background  ' + str1, get_str, bgnd_form, i, od) =
          True then begin
          p1.x := od[0];
          p1.y := od[1];
          p2.x := od[2];
          p2.y := od[3];
        end
        else
          EXIT;
      end;
    end;//case code
  end;

  ring_list_index := ring_index;          // local copy.
  used1 := False;                         // init.

  // for p1 preset use the latest copy or the ring (index=0) itself...
  if p1.x = def_req then begin
    p1.X := rings[ring_list_index, 0];
    used1 := True;
  end;
  if p1.y = def_req then begin
    p1.y := rings[ring_list_index, 1];
    used1 := True;
  end;

  if used1 = True then
    Dec(ring_list_index);   // ignore the copy used, or the ring.

  if ring_list_index > -1     // and for p2 use the previous copy or the ring itself if it's still there.
  // or the latest copy if it wasn't used for p1.
  then begin
    if p2.x = def_req then
      p2.X := rings[ring_list_index, 0];
    if p2.y = def_req then
      p2.y := rings[ring_list_index, 1];
  end
  else begin
    if p2.x = def_req then
      p2.X := screenx / 2;   // arbitrary. (pre-sets requested but all data already used)..
    if p2.y = def_req then
      p2.y := screeny / 2;
  end;

  Result := True;
end;
//_____________________________________________________________________________________

function do_twain(var img_bmp: TBitmap): boolean;       // 213b

var
  ok_flag: boolean;
  str: string;

  acquire_code: integer;

  img_stream: TMemoryStream;

  his_clipboard_bmp: TBitmap;
  his_clipboard_text: string;

  his_bmp_exists: boolean;

begin
  do_open_source_bang('SCAN PICTURE SHAPE');  // OT-FIRST

  Result := False;             // init...
  ok_flag := False;
  his_bmp_exists := False;
  his_clipboard_text := '';

  EXIT; // OT-FIRST

  (* OT-FIRST

  img_bmp:=TBitmap.Create;
  his_clipboard_bmp:=TBitmap.Create;

  if Clipboard.HasFormat(CF_TEXT) then his_clipboard_text:=Clipboard.AsText;

  if Clipboard.HasFormat(CF_BITMAP) then begin
                                           his_clipboard_bmp.Assign(Clipboard);
                                           his_bmp_exists:=True;
                                         end;

      // any other clipboard content will be lost.

  show_modal_message('Your usual scanner dialog will now appear.'
                     +#13+#13+'Make sure that your scanner is switched on.'
                     +#13+#13+'Consult your scanner instructions for more information.');

  try

    with bgnd_form do begin

      if twain_acquire.LoadTwainModule=True
         then begin
                try
                  twain_acquire.OpenSourceManager;
                  str:=twain_acquire.GetSource(False);
                  twain_acquire.SelectSource(str);
                  twain_acquire.OpenSource;

                  acquire_code:=twain_acquire.AcquireBmp(img_bmp);  // uses the clipboard

                  if acquire_code=SUCCESS then ok_flag:=True;

                finally
                  twain_acquire.CloseTwainSession;
                  twain_acquire.UnloadTwainModule;
                end;//try

                if ok_flag=True
                   then begin

                          // OT-FIRST if img_bmp.PixelFormat<>pf8bit then img_bmp.PixelFormat:=pf24bit;    // 215b  down from 32bit for deep zooming (also workaround for TPngImage on lower than 8bit)

                          image_viewer_form.viewer_image.Picture.Bitmap:=img_bmp;

                          show_modal_message('A preview of the scanned image will now be displayed full size.'
                                             +#13+#13+'You may need to scroll the viewer to see all of it.'
                                             +#13+#13+'Click OK above the image to accept it, or click CANCEL to reject it.');

                          image_viewer_form.Caption:='    scanned  image  preview   -   image  size : '+IntToStr(img_bmp.Width)+' x '+IntToStr(img_bmp.Height)+' pixels';

                          with image_viewer_form do begin

                            cancel_menu.Visible:=True;
                            options_menu.Visible:=False;

                          end;//with

                          if do_show_modal(image_viewer_form)=mrOk then RESULT:=True;

                        end
                  else show_modal_message('Sorry, unable to acquire the scanned image.'
                                  +#13+#13+'Check your scanner connections.');

              end
         else show_modal_message('Sorry, unable to load the file TWAIN_32.DLL from Windows.'
                                 +#13+#13+'You may need to run Windows Update on your system.');

    end;//with

  finally

    if his_clipboard_text<>'' then Clipboard.AsText:=his_clipboard_text;

    if his_bmp_exists=True then Clipboard.Assign(his_clipboard_bmp);

  //img_bmp.Free;

    his_clipboard_bmp.Free;
  end;//try
*)

end;
//______________________________________________________________________________

function get_user_image(dropped, meta, scan_wanted, paste_clicked, reloading: boolean;
  n: integer; var img_width, img_height: integer): string;

  // dropped = image dropped on alert_box  214a

  // meta = he said metafile, but might change if he loads something else
  // n=shape index
  // ask user for file and return file name part for the list, or empty_picture_str if he cancelled or failure
  // if reloading=True, don't change image on cancel.

  // 211b or paste image from clipboard if present

  // 213b metafiles added

  // 214a TWAIN scanning added

const
  paste_help_str: string = 'php/402    `0Paste copied image`9' +
    '||You are seeing this dialog because there is currently a copied image on the Windows clipboard.'
    +
    '||You may have copied the image by clicking `0Edit > Copy`z or pressing `0CTRL+C`2 while using an image editor program.'
    + '||Or you may have captured a screenshot in another program by pressing the `0PRINT SCREEN`2 key.'
    + '||If you answer yes, the copied image will be pasted directly into this picture shape.'
    +
    '||If you answer no, you will see the usual Windows file dialog to load a saved image file into the picture shape.';

var
  i: integer;
  was_cancelled: boolean;  // 211b

  image_file_str: string;
  dotpos: integer;

  dropped_picture, load_picture: TPicture;

  new_bmp: TBitmap;

  result_str: string;

  emf_ok: boolean;

  //emf_header:Temf_header;

  bgshape: Tbgshape;

begin
  Result := '';           // keep compiler happy
  img_width := 0;         // default init...
  img_height := 0;
  was_cancelled := False;
  result_str := empty_picture_str;   // const

  Result := result_str;

  try

    if dropped = True       // 214a
    then begin

      dropped_picture := alert_box.drop_image.Picture;

      with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]) do begin

        if bgimage <> nil       // clear any existing (this may be a reload).
        then begin
          bgimage.image_shape.image_bitmap.Free;      // free the bitmaps.
          bgimage.image_shape.rotated_bitmap.Free;

          bgimage.image_shape.rotated_picture.Free;

          bgimage.Free;                               // and free the image object.
        end;

        bgimage := Tbgimage.Create;     // create new image  3-2-01.

        bgnd_shape.show_transparent := False;     // not transparent.

        bgnd_shape.picture_is_metafile := False;  // default not a metafile 213b

      end;//with

      with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgimage.image_shape do
      begin

        image_bitmap := TBitmap.Create;
        rotated_bitmap := TBitmap.Create;

        rotated_picture := TPicture.Create;

        try
          Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).
            bgnd_shape.picture_is_metafile := False;

          if dropped_picture.Graphic is TIcon    // convert it to bitmap
          then begin
            image_bitmap.Width := dropped_picture.Graphic.Width;
            image_bitmap.Height := dropped_picture.Graphic.Height;

            image_bitmap.Canvas.Draw(0, 0, dropped_picture.Graphic);
          end
          else
            image_bitmap.Assign(dropped_picture.Graphic);    // load bitmap image

          image_width := image_bitmap.Width;
          image_height := image_bitmap.Height;

        except
          image_bitmap.Width := 200;            // arbitrary.
          image_bitmap.Height := 150;           // arbitrary.

          image_width := image_bitmap.Width;
          image_height := image_bitmap.Height;

          with image_bitmap.Canvas do begin     // blank the picture area...
            Brush.Color := clWhite;
            Brush.Style := bsSolid;
            FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
          end;//with

          Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).
            bgnd_shape.picture_is_metafile := False;

          show_modal_message('Sorry, unable to create picture shape from the dropped image.');
        end;//try

        img_width := image_width;        // return these to caller...
        img_height := image_height;

      end;//with image_shape

      shapes_saved := False;

      Result := 'dropped picture';     // for entry in list.

      EXIT;
    end;


    // 214a get TWAIN scanned image

    if scan_wanted = True then begin

      new_bmp := TBitmap.Create;

      if do_twain(new_bmp) = True    // get scan
      then begin

        with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]) do begin

          if bgimage <> nil       // clear any existing (this may be a reload).
          then begin
            bgimage.image_shape.image_bitmap.Free;
            // free the bitmaps.
            bgimage.image_shape.rotated_bitmap.Free;

            bgimage.image_shape.rotated_picture.Free;

            bgimage.Free;
            // and free the image object.
          end;

          bgimage := Tbgimage.Create;               // create new image  3-2-01.
          bgnd_shape.show_transparent := False;     // not transparent.
          bgnd_shape.picture_is_metafile := False;  // not a metafile 213b

        end;//with

        with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgimage.image_shape do begin

          image_bitmap := TBitmap.Create;
          rotated_bitmap := TBitmap.Create;

          rotated_picture := TPicture.Create;

          image_bitmap.Assign(new_bmp);

          image_width := image_bitmap.Width;
          image_height := image_bitmap.Height;

          img_width := image_width;
          img_height := image_height;

        end;//with

        shapes_saved := False;
        Result := 'scanned picture';     // for entry in list.

      end
      else
        was_cancelled := True;    // scan failed

      new_bmp.Free;

      EXIT;
    end;// scan wanted

    // 211b option to paste image from clibboard

    if Clipboard.HasFormat(CF_BITMAP) = True     // 211b paste from clipboard
    then begin
      repeat
        if paste_clicked = True then
          i := 6                           // paste button clicked

        else
          i := alert(4, 'php/402    paste  image  into  picture  shape ?',
            'You have previously copied an image.'
            +
            '||Do you want to paste the copied image into this picture shape?',
            '', '', 'more  information', 'no  -  load  image  from  a  file', 'cancel',
            'yes  -  paste  copied  image', 3);

        case i of
          3:
            alert_help(0, paste_help_str, '');

          //4: falls through

          5: begin
            was_cancelled := True;
            EXIT;
          end;

          6: begin     // paste image from clibboard...

            with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]) do begin

              if bgimage <> nil       // clear any existing (this may be a reload).
              then begin
                bgimage.image_shape.image_bitmap.Free;
                // free the bitmaps.
                bgimage.image_shape.rotated_bitmap.Free;

                bgimage.image_shape.rotated_picture.Free;

                bgimage.Free;
                // and free the image object.
              end;

              bgimage := Tbgimage.Create;     // create new image  3-2-01.

              bgnd_shape.show_transparent := False;     // not transparent.

              bgnd_shape.picture_is_metafile := False;  // not a metafile 213b

            end;//with

            with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgimage.image_shape do begin

              image_bitmap := TBitmap.Create;
              rotated_bitmap := TBitmap.Create;

              rotated_picture := TPicture.Create;

              image_bitmap.Assign(Clipboard);

              image_width := image_bitmap.Width;
              image_height := image_bitmap.Height;

              img_width := image_width;        // return these to caller...
              img_height := image_height;

            end;//with

            shapes_saved := False;

            Result := 'pasted picture';     // for entry in list.

            EXIT;
          end;
        end;//case

      until i <> 3;
    end;//image on clipboard

    if was_cancelled = False     // 211b
    then begin
      // load from file...

      with bgnd_form.picture_load_dialog do begin  // 0.93.a

        if (user_load_img_path = '') or (user_load_img_path = Config.GetDir(cudiEmfs))
          or (user_load_img_path = Config.GetDir(cudiImages)) then begin
          if meta = True then
            InitialDir := Config.GetDir(cudiEmfs)
          else
            InitialDir := Config.GetDir(cudiImages);
        end
        else
          InitialDir := user_load_img_path;

        FileName := '';

        if meta = True then begin
          show_modal_message('The file dialog will display EMF metafiles as requested.'
            + #13 + #13 +
            'To load a bitmap image file instead, change the File Type in the dialog.');
          FilterIndex := 6;    // init EMF in dialog
        end
        else
          FilterIndex := 1;      // init all bimtap files in dialog

      end;//with dialog

      if bgnd_form.picture_load_dialog.Execute = True then begin
        image_file_str := bgnd_form.picture_load_dialog.FileName;

        if FileExists(image_file_str) = True then begin
          Application.ProcessMessages;
          // allow repaints - may take some time.

          with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n])
            do begin

            if bgimage <> nil
            // clear any existing (this may be a reload).
            then begin
              bgimage.image_shape.image_bitmap.Free;
              // free the bitmaps.
              bgimage.image_shape.rotated_bitmap.Free;

              bgimage.image_shape.rotated_picture.Free;

              bgimage.Free;
              // and free the image object.
            end;

            bgimage := Tbgimage.Create;     // create new image  3-2-01.

            bgnd_shape.show_transparent := False;
            // init not transparent.
            bgnd_shape.picture_is_metafile := False;  // init  213b

          end;//with

          bgshape :=
            Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]);

          with bgshape do begin
            with bgimage.image_shape do begin

              // these never used if metafile...

              image_bitmap := TBitmap.Create;
              rotated_bitmap := TBitmap.Create;

              rotated_picture := TPicture.Create;

              load_picture := TPicture.Create; //0.93.a

              try
                // OT-FIRST   EMF metafile ...

                if (LowerCase(ExtractFileExt(image_file_str)) = '.emf')
                then begin
                  if get_metafile_for_existing_shape(
                    image_file_str, bgshape) = False then begin
                    bgfile_error(image_file_str);
                    was_cancelled := True;
                    EXIT;
                  end;
                end
                else begin

                  load_picture.LoadFromFile(image_file_str);

                  image_bitmap.Assign(load_picture.Graphic);

                  image_width := image_bitmap.Width;
                  image_height := image_bitmap.Height;

                  bgnd_shape.picture_is_metafile := False;
                  // into file   213b
                end;
              finally
                load_picture.Free;
              end;//try

              img_width := image_width;        // return these to caller...
              img_height := image_height;

            end;//with image_shape

          end;//with shape object

          shapes_saved := False;

          user_load_img_path := ExtractFilePath(image_file_str);
          // for next time.

          Result := 'picture : ' + Copy(ExtractFileName(image_file_str), 1, 36);
          // for entry in list. 46 chars max

          EXIT;
        end
        else begin  // file doesn't exist
          bgfile_error(image_file_str);
          was_cancelled := True;
          EXIT;
        end;
      end//dialog execute
      else
        was_cancelled := True;  // cancelled reload

    end;
  finally

    // if loading failed, or he cancelled, use defaults...

    if (reloading = False) and (was_cancelled = True)  // don't change existing or new image
    then begin

      // cancelled, set default empty image...

      with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]) do begin

        if bgimage <> nil       // clear any existing
        then begin
          bgimage.image_shape.image_bitmap.Free;      // free the bitmaps.
          bgimage.image_shape.rotated_bitmap.Free;

          bgimage.image_shape.rotated_picture.Free;

          bgimage.Free;                               // and free the image object.
        end;

        bgimage := Tbgimage.Create;     // create new image  3-2-01.

        bgnd_shape.show_transparent := False;     // 0.93.a
        bgnd_shape.picture_is_metafile := False;  // 213b
      end;//with

      with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgimage.image_shape do
      begin

        image_bitmap := TBitmap.Create;
        rotated_bitmap := TBitmap.Create;

        rotated_picture := TPicture.Create;

        image_bitmap.Width := 20;            // arbitrary.
        image_bitmap.Height := 10;           // arbitrary.

        image_width := image_bitmap.Width;
        image_height := image_bitmap.Height;

        img_width := image_width;        // return these to caller...
        img_height := image_height;

        shapes_saved := False;

        with image_bitmap.Canvas do begin     // and blank the picture area...
          Brush.Color := clWhite;
          Brush.Style := bsSolid;
          FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
        end;//with

      end;//with
    end;

  end;//try

end;
//_______________________________________________________________________________________

function get_sk7_image(n: integer; sk7_str: string; var img_width, img_height: integer): string;
  // 3-2-01.

  // n=shape index, sk7_str is the file name to load,
  // if load fails return empty_picture_string, otherwise null.
var
  bgshape: Tbgshape;

begin
  Result := empty_picture_str;     // init...
  img_width := 20;
  img_height := 10;

  bgshape := Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]);

  with bgshape do begin

    bgnd_shape.show_transparent := False;     // not transparent

    if bgimage <> nil       // clear any existing (this may be a reload).
    then begin
      bgimage.image_shape.image_bitmap.Free;      // free the bitmaps.
      bgimage.image_shape.rotated_bitmap.Free;

      bgimage.image_shape.rotated_picture.Free;
      bgimage.Free;                               // and free the image object.
    end;

    bgimage := Tbgimage.Create;     // create new image  3-2-01.

    with bgimage.image_shape do begin

      image_bitmap := TBitmap.Create;
      rotated_bitmap := TBitmap.Create;

      rotated_picture := TPicture.Create;

      if FileExists(sk7_str) = True then begin
        if get_metafile_for_existing_shape(sk7_str, bgshape) = False then
        begin
          // loading failed, or he cancelled, so use defaults...

          image_bitmap.Width := 20;            // arbitrary.
          image_bitmap.Height := 10;           // arbitrary.

          image_width := image_bitmap.Width;
          image_height := image_bitmap.Height;

          with image_bitmap.Canvas do begin     // and blank the picture area...

            Brush.Color := clWhite;
            Brush.Style := bsSolid;
            FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
          end;//with

          bgnd_shape.picture_is_metafile := False;  // not a metafile

          shapes_saved := False;   // failed to load, so shape is modified

        end
        else
          Result := '';       // ok, got metafile

        img_width := image_width;
        img_height := image_height;
      end;

    end;//with image_shape
  end;//with shape object
end;
//______________________________________________________________________________

function get_sk8_image(n: integer; old_bmp_file_str, sk8_str: string;
  var img_width, img_height: integer): string;    // 3-2-01.

  // n=shape index, sk8_str is the file name to load,
  // and old_bmp_file_str is the bmp file to try loading if sk8_str fails (i.e. BGS file is pre-0.93.a).
  // if load fails return empty_picture_string, otherwise null.

var
  sk8_exists, bmp_exists: boolean;
  { OT-FIRST create_png:TPNGObject;}
  create_png: TPortableNetworkGraphic;  // OT-FIRST

begin
  Result := empty_picture_str;     // default init...
  img_width := 20;
  img_height := 10;

  with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]) do begin

    bgnd_shape.picture_is_metafile := False;  // not a metafile (in case loading old file) 213b

    if bgimage <> nil       // clear any existing (this may be a reload).
    then begin
      bgimage.image_shape.image_bitmap.Free;      // free the bitmaps.
      bgimage.image_shape.rotated_bitmap.Free;

      { OT-FIRST
              bgimage.image_shape.image_metafile.Free;    // 213b
              bgimage.image_shape.rotated_metafile.Free;  // 213b
              }

      bgimage.image_shape.rotated_picture.Free;
      bgimage.Free;                               // and free the image object.
    end;

    bgimage := Tbgimage.Create;     // create new image  3-2-01.

    with bgimage.image_shape do begin

      image_bitmap := TBitmap.Create;
      rotated_bitmap := TBitmap.Create;

      { OT-FIRST
      image_metafile:=TMetafile.Create;    // 213b
      rotated_metafile:=TMetafile.Create;  // 213b
      }

      rotated_picture := TPicture.Create;

      //draw_trans:=False;            // not transparent.

      sk8_exists := FileExists(sk8_str);         // 0.93.a

      if sk8_exists = True     // png format, 0.93.a
      then begin
        { OT-FIRST create_png:=TPNGObject.Create;}
        create_png := TPortableNetworkGraphic.Create;  // OT-FIRST

        try

          try
            create_png.LoadFromFile(sk8_str);
          except
            sk8_exists := False;
            bgfile_error(sk8_str);
          end;//try

          if sk8_exists = True then
            image_bitmap.Assign(create_png);

        finally
          create_png.Free;
        end;//try

        if sk8_exists = True then begin
          image_width := image_bitmap.Width;
          image_height := image_bitmap.Height;

          img_width := image_width;        // return these to caller...
          img_height := image_height;

          // OT-FIRST if image_bitmap.PixelFormat<>pf8bit then image_bitmap.PixelFormat:=pf24bit;    // 215b  down from 32bit for deep zooming (also workaround for TPngImage on lower than 8bit)

          Result := '';
          EXIT;            // job done.
        end;
      end;

      if sk8_exists = False   // old format? or sk8 failed? try load a bmp file instead.
      then begin
        bmp_exists := FileExists(old_bmp_file_str);

        if bmp_exists = True then begin
          try
            image_bitmap.LoadFromFile(old_bmp_file_str);
          except
            bmp_exists := False;
            bgfile_error(old_bmp_file_str);
          end;//try

          if bmp_exists = True then begin
            image_width := image_bitmap.Width;
            image_height := image_bitmap.Height;

            img_width := image_width;        // return these to caller...
            img_height := image_height;

            // OT-FIRST if image_bitmap.PixelFormat<>pf8bit then image_bitmap.PixelFormat:=pf24bit;    // 215b  down from 32bit for deep zooming (also workaround for TPngImage on lower than 8bit)

            Result := '';
            EXIT;         // job done.
          end;
        end;
      end;

      // loading failed, or he cancelled, so use defaults...

      image_bitmap.Width := 20;            // arbitrary.
      image_bitmap.Height := 10;           // arbitrary.

      image_width := image_bitmap.Width;
      image_height := image_bitmap.Height;

      // OT-FIRST  image_bitmap.PixelFormat:=pf24bit;    // 215b    was 8bit

      with image_bitmap.Canvas do begin     // and blank the picture area...

        Brush.Color := clWhite;
        Brush.Style := bsSolid;
        FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
      end;//with

    end;//with image shape

  end;//with shape object

  shapes_saved := False;   // failed to load, so shape is modified
end;
//______________________________________________________________________________

procedure auto_fit_picture;

const
  pic_width_str: string = '    `0Picture  Width`9' +
    '||Enter the required width of this picture shape on your plan. Templot0 will scale the bitmap image to fit.';

  map_scale_str: string = '    `0Map  Scale  Ratio`9' +
    '||Enter the scale of the original scanned map as a unit ratio. For example, a scale of 40ft to one inch of map should be entered as 480.' + '||For the well-known OS 25-inch maps, the scale is 1:2500 and should be entered as 2500.';

  model_scale_str: string = '    `0Model  Track  Plan  Ratio`9' +
    '||Enter the scale of the original scanned track plan as a unit ratio. For example, a scale of one model foot per 1/2 inch of plan is a scale of 1:24 and should be entered as 24.';

  picture_sizing_help_str: string =
    '    `0Sizing  Method`9' +
    '||If you choose `0SCANNED MAP OR PROTOTYPE TRACK PLAN`1 you will be asked for the scanner DPI resolution and the scale of the map in relation to the full-size railway.' + ' Templot0 will then scale the picture shape correctly to match your drawing.' + '||If you choose `0SCANNED MODEL TRACK PLAN`1 you will be asked for the scanner DPI resolution and the scale of the plan in relation to the model.' + ' Templot0 will then scale the picture shape correctly to match your drawing.' + '||For the above two options, it is essential to have previously set your required model gauge and scale.' + '||If you do not know the scanner resolution, some trial and error will be needed to get the image to the correct size. A good starting point would be 300 dpi.' + '|<HR NOSHADE SIZE=1 COLOR=GRAY>' + '|For more information and tutorials about the use of background images, please refer to the <A HREF="go_to_templot_companion.85a">Templot Companion</A> web site.|&nbsp;';

var
  new_shape: Tbgnd_shape;
  i, n, j: integer;
  valid: boolean;

  width_mm_per_dot, height_mm_per_dot: double;

  dpi_width, dpi_height: double;
  map_scale, model_scale: double;

  picture_option: integer;

  scaling_wanted: boolean;

  x1, y1, x2, y2: double;

  scanned_bmp: TBitmap;

  od: Toutdim;

  paste_wanted, file_wanted, metafile_wanted, scan_wanted: boolean;

  paste1_str, paste2_str, drawn_str, scan_str: string;

  dpi_str: string;

begin
  if bgnd_form.bgnd_shapes_listbox.Items.Count < 1 then
    EXIT;  // ???

  n := bgnd_form.bgnd_shapes_listbox.ItemIndex;

  if (n < 0) or (n > (bgnd_form.bgnd_shapes_listbox.Items.Count - 1)) then
    EXIT; // ???

  if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgnd_shape.shape_code <> -1 then
  begin
    show_modal_message('The currently selected shape is not a picture shape.'
      + #13 + #13 + 'The auto-fit applies to picture shapes only.');
    EXIT;
  end;

  if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgnd_shape.shape_name =
    empty_picture_str then begin
    show_modal_message('The currently selected picture shape is empty.'
      + #13 + #13 + 'There is no image to auto-fit.' +
      #13 + #13 + 'Click one of the "new image from :" buttons to load an image into this picture shape.');
    EXIT;
  end;

  dpi_str := 'rp.gif  The width-wise and height-wise DPI settings are entered separately. In the vast majority of cases the setting is the same for both, for example from a scanner.' +
  '||The settings can sometimes differ, for example for an image received via a fax machine.' +
  '|||    `0Scanner  DPI`9' + '||Enter the resolution in dots-per-inch (DPI) at which this image was scanned. If you do not know this figure, some trial and error will be needed to get the image to the correct size. A good starting point would be 300 dpi.' + '||Refer to the documentation for your scanner for information about the scanner resolution.' + '||The width-wise and height-wise DPI settings are entered separately. In the vast majority of cases the setting is the same for both, for example from a scanner.' + ' The settings can sometimes differ, for example for an image received via a fax machine.' + '||Many scanners use the popular ScanGear software:' + '||       <img src="' + Config.GetFilePath(csfiScangearDPI) + '">' + '||To find or set the DPI resolution:' + '||1. click the `0Advanced Mode`z tab.|2. Set or make a note of the `0Output Resolution`z setting.' + '||green_panel_begin tree.gif Do not set very high resolutions as this may severely slow down the screen refresh and zooming in Templot0.' + '||300 DPI (dots per inch) is a good option on most systems. Use a lower setting on older computers.green_panel_end';

  picture_option := 0; // keep compiler happy

  if auto_fit_msg_pref = False then begin

    repeat
      alert_box.preferences_checkbox.Checked := False;
      alert_box.preferences_checkbox.Show;

      i := alert(2, '        auto - fit  picture  shape',
        'This auto-fit function can automatically adjust the size of this picture shape to fit the image it contains.' + ' This is primarily intended for scaling (re-sizing) scanned maps and track plans to use as a background guide.' + '||After the picture shape has been re-sized you will probably want to move it to a new position on the trackpad.' + '||To do that click the `0shift`1 button to move it by mouse action, or the `0shift to...`1 button to set the new position directly.' + '||But first, auto-fit re-sizing:' + '||Do you know the DPI resolution at which the image was scanned?|If not click `0more information`1 below.' + '|`0for example: 300 DPI`n.' + '||Do you know the scale of the scanned map or track plan?' + '|`0for example: 1:2500 for an OS 25" prototype map,' + ' or 1:24 for a model layout plan at 1/2" per foot`n.' + '||Have you already set you model gauge and scale?' + '|`0for example: EM`n.' + '||If you answered yes to all of the above Templot0 can automatically set the picture shape to the' + ' correct scaled size for your track. Click `0auto-fit`1 below.' + '||If you want to go back and find the information or set the gauge and scale, click `0cancel`1 below, then click the `0auto-fit`1 button again when you are ready.' + '||Alternatively, the height of this picture shape can be adjusted to match the aspect ratio' + ' of the image and remove any distortion, leaving the width of the picture shape unchanged. Click `0auto-fit`1 below.' + '||If you want to set a known width for the picture shape, click `0set known width`1 below, or click the `0re-size...`1 button.', '', '', 'more  information', 'set  known  width', 'cancel', 'auto - fit', 3);

      auto_fit_msg_pref := alert_box.preferences_checkbox.Checked;
      alert_box.preferences_checkbox.Hide;

      if i = 3 then
        alert_help(0, dpi_str, '');

      if i = 4 then begin
        bgnd_form.scale_one_to_button.Click;
        EXIT;
      end;

      if i = 5 then
        EXIT;

    until i <> 3;

  end;


  repeat

    i := alert(4, 'php/401    sizing  method ?',
      'Please select the auto-fit sizing method to be applied to this picture shape.'
      +
      '||If you click one of the blue bars Templot0 can size the picture shape automatically, but you will need to enter some information about the scanner DPI setting and the real-world scale of the track plan image.' + ' Also make sure that you have set your model scale and gauge.' + '||Otherwise, click the green bar to adjust only the height of the picture shape to match the image, leaving the width unchanged.' + '||For more explanation of these options, click the white `0more information`1 bar.', '', 'more  information', 'scanned  map  or  prototype  track  plan', 'scanned  model  layout  plan', 'cancel', 'adjust  aspect  ratio  only', 2);
    case i of
      2:
        alert_help(0, picture_sizing_help_str, '');
      3:
        picture_option := 1;
      4:
        picture_option := 2;
      5:
        EXIT;
      6:
        picture_option := 0;
    end;//case

  until i <> 2;

  // init some defaults...

  width_mm_per_dot := 1.0;
  height_mm_per_dot := 1.0;

  dpi_width := 300;      // init values showing...
  dpi_height := 300;
  map_scale := 480;
  model_scale := 24;

  try

    with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]) do begin

      with bgimage.image_shape do begin

        if picture_option = 0  // aspect ratio only
        then begin
          with bgnd_shape do
            p2.y := p1.y + (p2.x - p1.x) * image_height / image_width;   // adjust height to aspect ratio of loaded image
          EXIT;
        end;

        // scale it for him ...

        putdim(dpi_str, 0, 'width-wise  scanned  dpi', dpi_width, True, True, True, False);
        // no neg, no preset, no zero, don't terminate on zero.
        putdim(dpi_str, 0, 'height-wise  scanned  dpi', dpi_height, True, True, True, False);
        // no neg, no preset, no zero, don't terminate on zero.

        if picture_option = 1 then
          j := putdim(map_scale_str, 0, 'map  scale  ratio  1 :', map_scale, True, True, True, False)
        // no neg, no preset, no zero, don't terminate on zero.
        else
          j := putdim(model_scale_str, 0, 'model  track  plan  scale  ratio  1 :', model_scale,
            True, True, True, False);
        // no neg, no preset, no zero, don't terminate on zero.

        if j <> 2 then
          EXIT;
        if getdims('auto - fit  picture  shape', get_str, bgnd_form, j, od) = False then
          EXIT;

        case picture_option of

          1: begin                                      // map...
            width_mm_per_dot := od[2] * inscale / od[0];
            height_mm_per_dot := od[2] * inscale / od[1];
          end;

          2: begin                                     // model track plan...
            width_mm_per_dot := od[2] * 25.4 / od[0];
            height_mm_per_dot := od[2] * 25.4 / od[1];
          end;

          else
            EXIT;  // ???
        end;//case

        with bgnd_shape do begin
          p2.x := p1.x + image_width * width_mm_per_dot;
          p2.y := p1.y + image_height * height_mm_per_dot;
        end;//with

      end;//with
    end;//with

  finally
    shapes_saved := False;      // need a resave.
    shapes_current_state;

    do_rollback := False;       // no need to put this change in rollback register on redraw.
    redraw(False);
  end;//try
end;
//____________________________________________________________________________________________

procedure add_shape(add_option: integer);

//  !!! NO LONGER USED for picture shapes, see add_picture_shape_button  214a

// add_option   0 = by drawing (already drawn)
//              1 = by entering dims (not yet)
//              2 = at clicked locations (already clicked)
//              3 = at default locations (empty picture shape)  not used  // 214a

var
  new_shape: Tbgnd_shape;
  i, n: integer;
  valid: boolean;
  x1, y1, x2, y2: double;
  drawn_str: string;

  //pad_was_maximized:boolean;

begin
  // first check pad has been clicked since startup...

  if add_option = 2        // add at clicked corners.
  then begin
    if (pad_click_X = -1) or (pad_click_Y = -1) or (prev_pad_click_X = -1) or
      (prev_pad_click_Y = -1) then begin
      alert(6, 'php/301    invalid  clicked  shape',
        'Invalid Clicked Shape.' +
        '||You have not yet clicked 2 locations on the trackpad to mark the position of the new shape.'
        +
        '||Click the trackpad in 2 places to mark the corners or ends or enclosing rectangle of the new shape. Then click the `0ADD SHAPE`1 button again.' + '||Or alternatively select the `0BY DRAWING`1 option to draw the outline of the new shape after clicking the `0ADD SHAPE`1 button.' + '||Or select the `0BY ENTERING DIMENSIONS`1 option to enter directly the required dimensions for the new shape.' + '||For more information about adding background shapes click the `0HELP`1 menu items.',
        '', '', '', '', '', 'continue', 0);
      EXIT;
    end;
  end;

  valid := False;  // init

  with bgnd_form do begin

    with new_shape do begin

      shape_code := 0;    // default line. 0=line, 1=rectangle, 2=circle, 3=label, 4=target, -1=picture
      shape_style := 0;
      // default transparent.  0=transparent, 1=blank/solid, 2=cross-hatched or dotted

      wrap_offset := 0;               // default (used only for image wrapping)
      show_transparent := False;      // default (used only for images)
      picture_is_metafile := False;   // default (used only for images)

      if target_radio_button.Checked = True then
        shape_code := 4;
      if label_radio_button.Checked = True then
        shape_code := 3;
      if circle_radio_button.Checked = True then
        shape_code := 2;
      if rect_radio_button.Checked = True then
        shape_code := 1;
      if picture_radio_button.Checked = True then
        shape_code := -1;     //  not used, see add_picture_shape_button

      shape_name := name_editbox.Text;        // name or label,

      case shape_code of
        0:
          if dotted_radio_button.Checked = True then
            shape_style := 2;

        1, 2: begin
          if solid_infill_radio_button.Checked = True then
            shape_style := 1;
          if hatched_radio_button.Checked = True then
            shape_style := 2;
        end;
      end;//case

      // init to saved mouse_down or drawn pad positions...

      case add_option of

        0: begin                     // as already drawn
          x1 := MIN(shape_rectangle_x1, shape_rectangle_x2);
          y1 := MIN(shape_rectangle_y1, shape_rectangle_y2);

          x2 := MAX(shape_rectangle_x1, shape_rectangle_x2);
          y2 := MAX(shape_rectangle_y1, shape_rectangle_y2);
        end;

        1: begin        // entering dims, set default 4ft x 2ft for data entry
          x1 := 0;
          y1 := 0;

          x2 := 1200;
          y2 := 600;
        end;

        2: begin                    // as already clicked
          case shape_code of
            -1, 1, 2: begin     // picture, rectangle, circle
              x1 := MIN(mouse_x(pad_click_X), mouse_x(prev_pad_click_X));
              y1 :=
                MIN(mouse_y(pad_click_X, pad_click_Y), mouse_y(prev_pad_click_X, prev_pad_click_Y));

              x2 := MAX(mouse_x(pad_click_X), mouse_x(prev_pad_click_X));
              y2 :=
                MAX(mouse_y(pad_click_X, pad_click_Y), mouse_y(prev_pad_click_X, prev_pad_click_Y));
            end;

            else begin
              x1 := mouse_x(pad_click_X);
              y1 := mouse_y(pad_click_X, pad_click_Y);

              x2 := mouse_x(prev_pad_click_X);
              y2 := mouse_y(prev_pad_click_X, prev_pad_click_Y);
            end;
          end;//case
        end;

        else begin        // 3 = default dims for empty picture shape  not used
          x1 := 0;
          y1 := 0;

          x2 := 1200;  // 4:3 aspect ratio
          y2 := 900;
        end;
      end;//case

      // over-rides...

      case shape_code of

        2: begin                   // circle      changes 214a   centre+rad
          p1.x := x1;
          p1.y := y1;

          p2.x := x2;
          p2.y := p1.y + ABS(x2 - x1);
        end;


        3: begin        // label
          p1.x := x1;
          p1.y := y1;

          p2.x := 0;
          p2.y := 0;
        end;

        4: begin        // target mark
          p1.x := x1;
          p1.y := y1;

          p2.x := target_arm;
          p2.y := 0;
        end;

        else begin
          p1.x := x1;
          p1.y := y1;

          p2.x := x2;
          p2.y := y2;
        end;
      end;//case

      case add_option of

        0, 2: begin   // drawn rectangle or clicked corners

          if ((shape_code = 0) and (p1.x = p2.x) and (p1.y = p2.y)) or
            (((shape_code = -1) or (shape_code = 1) or (shape_code = 2)) and ((p1.x = p2.x) or (p1.y = p2.y)))
          then begin
            alert(6, 'php/301    invalid  shape',
              'This shape can not appear because the length of the line or one of the sides is zero.'
              +
              '||If you were drawing this shape, the most likely reason is that you simply clicked the trackpad instead of drawing with the mouse or pen.',
              '', '', '', '', 'cancel  shape', '', 0);
            EXIT;
          end
          else
            valid := True;
        end;

        1: begin      // add_option=1  enter dimensions...
          repeat
            if get_shape_dims(False, shape_code, p1, p2) = False then
              EXIT;

            if ((shape_code = 0) and (p1.x = p2.x) and (p1.y = p2.y)) or
              (((shape_code = -1) or (shape_code = 1) or (shape_code = 2)) and ((p1.x = p2.x) or (p1.y = p2.y)))
            then begin
              repeat
                i := alert(6, '    invalid  shape',
                  '||This shape can not appear because the length of the line or of one of the sides is zero.', '', '', '', '?  help', 'cancel  shape', 'try  again', 4);
                case i of
                  4:
                    alert_help(0, inv_shape_help, '');
                  5:
                    EXIT;
                end;//case
              until i <> 4;
            end
            else
              valid := True;

          until valid = True;
        end;

        else
          valid := True;  // add_option=3  default picture shape dims          not used

      end;//case

      case shape_code of
        0:
          normalize_line(p1, p2);        // 214a line
        -1, 1, 2:
          normalize_rectangle(p1, p2);   // 214a picture, rectangle, circle
      end;//case

      if shape_name = ''         // nothing entered
      then begin
        if add_option = 0 then
          drawn_str := 'drawn '
        else
          drawn_str := '';

        case shape_code of
          -1:
            shape_name := drawn_str + 'picture';
          0:
            shape_name := drawn_str + 'line';
          1:
            shape_name := drawn_str + 'rectangle';
          2:
            shape_name := drawn_str + 'circle/ellipse';
          3:
            shape_name := drawn_str + 'label';
          4:
            shape_name := drawn_str + 'target mark';

        end;//case
      end;

      hide_bits := 0;  // 214a  normal visibility
      option_bits := 0;     // byte;

      with bgnd_form.bgnd_shapes_listbox do begin
        with Items do begin

          n := AddObject(shape_name, Tbgshape.Create);
          // create and insert a new entry in the shapes list.

          Tbgshape(Objects[n]).bgimage := nil;
          Tbgshape(Objects[n]).bgnd_shape := new_shape;   // put data in list.
        end;//with

        ItemIndex := n;     // make it current.

        if shape_code = -1  // new picture shape. fill with default bitmap..
        then begin
          with Tbgshape(Items.Objects[n]) do begin

            bgnd_shape.show_transparent := False;
            bgnd_shape.picture_is_metafile := False;

            bgimage := Tbgimage.Create;     // create new image  3-2-01.

            with bgimage.image_shape do begin

              image_bitmap := TBitmap.Create;
              rotated_bitmap := TBitmap.Create;

              rotated_picture := TPicture.Create;

              try
                image_bitmap.LoadFromFile(Config.GetFilePath(csfiEmptyPic));

                image_width := image_bitmap.Width;
                image_height := image_bitmap.Height;
              except
                image_bitmap.Width := 200;            // arbitrary.
                image_bitmap.Height := 150;           // arbitrary.

                image_width := image_bitmap.Width;
                image_height := image_bitmap.Height;

                with image_bitmap.Canvas do begin     // blank the picture area...
                  Brush.Color := clWhite;
                  Brush.Style := bsSolid;
                  FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
                end;//with

              end;//try

            end;//with
          end;//with
        end;//new picture

      end;//with listbox

    end;//with new_shape

    shapes_saved := False;      // need a resave.
    shapes_current_state;
    do_rollback := False;       // no need to put this change in rollback register on redraw.

    redraw(False);
    // changes to default pad cursor.
    if check_grey_paper = False then
      pad_form.Cursor := cross_hairs_cursor; // so change it back again.

    //zoom_fit_shape_menu_entry.Click;

  end;//with form
end;
//____________________________________________________________________________________________

procedure Tbgnd_form.add_shape_buttonClick(Sender: TObject);

var
  i, add_code: integer;

begin

  if picture_radio_button.Checked = True          // ??? should not get here (button not visible)
  then begin
    add_picture_shape_button.Click;     // 214a
    EXIT;
  end;

  if by_drawing_radiobutton.Checked = True   // go do drawing
  then begin
    if line_radio_button.Checked = True                // 214a  line drawing
    then
      pad_form.mouse_draw_menu_entry.Click
    else
      drawn_shape_rectangle := True;
    EXIT;
  end;

  if enter_dims_radiobutton.Checked = True then
    add_code := 1                      // enter dims
  else
    add_code := 2;                     // as clicked

  add_shape(add_code);
end;
//______________________________________________________________________________

procedure free_shape_object(n: integer);

var
  bgshape: Tbgshape;

begin
  with bgnd_form.bgnd_shapes_listbox.Items do begin

    if (Count < 1) or (n < 0) or (n > (Count - 1)) then
      EXIT;

    bgshape := Tbgshape(Objects[n]);

    with bgshape do begin

      if bgnd_shape.shape_code = -1    // picture shape
      then begin
        if bgimage <> nil then begin
          with bgimage.image_shape do begin

            if image_bitmap <> nil then
              image_bitmap.Free;      // free the bitmaps.
            if rotated_bitmap <> nil then
              rotated_bitmap.Free;

            if rotated_picture <> nil then
              rotated_picture.Free;

            //  EMF
            //if bgnd_shape.picture_is_metafile = True              // 291a
            //then
            //  DeleteEnhMetaFile(image_metafile.emf_HDC);  // release the metafile handle and free memory

          end;//with

          bgimage.Free;     // and free the image object.
          bgimage := nil;
        end;

      end;//picture shape

      Free;  // free the bgshape object
    end;//with

  end;//with listbox
end;
//________________________________________________________________________________________

procedure add_picture_expert;   // 214a

const
  xpic_str: string = '      `0X  Dimensions`9' +
    '||X dimensions are measured across the screen from the left, and read on the bottom grid scale.'
    +
    '||Dimensions measured forwards to the right from the origin (0 mark) are positive; dimensions measured backwards to the left from the origin are negative.' + '||Unless you use conversion factors the dimension should be entered in mm.' + '||For more information about using conversion factors click the `0? HELP`1 button.';

  ypic_str: string = '      `0Y Dimensions`9' +
    '||Y dimensions are measured up the screen from the bottom, and read on the left grid scale.'
    + '||Dimensions measured upwards from the origin (0 mark) are positive; dimensions measured downwards from the origin are negative.' + '||Unless you use conversion factors the dimension should be entered in mm.' + '||For more information about using conversion factors click the `0? HELP`1 button.';

  wpic_str: string = '      `0Width  Dimensions`9' +
    '||Width dimensions are measured across the screen, representing the distance between the left and right edges of the picture shape.' + '||Width dimensions are always positive. Negative or zero width dimensions are invalid.' + '||For a picture shape, you can enter the required width initially or leave the suggested width.' + ' There are subsequent `0AUTO-FIT`1 functions to scale (re-size) a picture shape to the exact size required after the image has been loaded into it.';

var
  new_shape: Tbgnd_shape;
  od: Toutdim;
  i, n: integer;

begin

  with new_shape do begin

    // defaults ...

    shape_code := -1;   // -1=picture
    shape_style := 0;   // not used

    wrap_offset := 0;               // default
    show_transparent := False;      // default
    picture_is_metafile := False;   // default

    shape_name := empty_picture_str;

    hide_bits := 0;  // normal visibility
    option_bits := 0;     // byte;

    p1.x := 0;
    p1.y := 0;

    p2.x := p1.x + screenx / 2;     // arbitrary
    p2.y := p1.y + screenx * 9 / 32;  // 16:9 aspect ratio


    putdim(xpic_str, 1, 'picture :  bottom  left  corner  X  dimension ', p1.x,
      False, True, False, False);
    // neg ok, no preset, allow zero, don't terminate on zero.
    putdim(ypic_str, 1, 'picture :  bottom  left  corner  Y  dimension ', p1.y,
      False, True, False, False);
    // ditto.
    i := putdim(wpic_str, 1, 'picture :  width ', p2.x - p1.x, True, True, True, False);
    // no neg, no preset, no zero, don't terminate on zero.

    if i <> 2 then
      EXIT;
    if getdims('background  picture  shape', get_str, Screen.ActiveForm, i, od) = True then begin
      p1.x := od[0];
      p1.y := od[1];
      p2.x := p1.x + ABS(od[2]);
      p2.y := p1.y + ABS(od[2]) * 9 / 16;
    end
    else
      EXIT;

  end;//with

  with bgnd_form.bgnd_shapes_listbox do begin

    n := Items.AddObject(new_shape.shape_name, Tbgshape.Create);
    // create and insert a new entry in the shapes list.

    Tbgshape(Items.Objects[n]).bgnd_shape := new_shape;          // put data in list.

    ItemIndex := n;                                              // make it current.

    with Tbgshape(Items.Objects[n]) do begin

      bgimage := Tbgimage.Create;     // create new image  3-2-01.

      with bgimage.image_shape do begin

        image_bitmap := TBitmap.Create;
        rotated_bitmap := TBitmap.Create;

        { OT-FIRST
        image_metafile:=TMetafile.Create;    // 213b
        rotated_metafile:=TMetafile.Create;  // 213b
        }

        rotated_picture := TPicture.Create;

      end;//with
    end;//with
  end;//with listbox

  if reload_picture_image(False, False, False, False, True) = False
  // scanning,pasting,meta,dropped,adjust_aspect
  then begin
    with bgnd_form.bgnd_shapes_listbox.Items do begin
      if (Count < 1) or (n < 0) or (n > (Count - 1)) then
        EXIT;  // ???

      free_shape_object(n);

      Delete(n);    // delete the new entry.

      if Count < 1 then
        EXIT;                                                 // list now empty.
      if n < Count then
        bgnd_form.bgnd_shapes_listbox.ItemIndex := n            // select next line
      else
        bgnd_form.bgnd_shapes_listbox.ItemIndex := Count - 1;
    end;//with
  end
  else
    shapes_saved := False;      // need a resave.

  shapes_current_state;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure clear_shapes;

var
  n: integer;

begin
  with bgnd_form do begin
    with bgnd_shapes_listbox.Items do begin
      if Count > 0 then begin
        for n := 0 to Count - 1 do begin
          free_shape_object(n);  // free any picture bitmaps and the shape object.
        end;//for

        Clear;
      end;
    end;//with
    name_editbox.Text := '';
  end;//with
  shapes_saved := True;       // nothing to save.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.delete_all_menu_entryClick(Sender: TObject);

begin
  if bgnd_shapes_listbox.Items.Count > 0 then begin
    case alert(7, '    delete  all  shapes',
        '||You are about to delete all background shapes from the trackpad.'
        +
        '||If you will need these shapes again they should first be saved to a file.'
        + '||Are you sure you want to delete all the shapes ?',
        '', '', '', 'yes  -  delete  all  shapes', 'no  -  cancel  delete',
        'save  first,  then  delete  all', 0) of
      4:
        clear_shapes;
      6: begin
        shapes_saved := False;
        save_all_menu_entry.Click;
        if shapes_saved = True then
          clear_shapes;
      end;
    end;//case
  end;
end;
//____________________________________________________________________________________________

procedure delete_percent(skip_count: integer; str: string);

var
  n: integer;

begin
  if skip_count < 1 then
    EXIT;
  if bgnd_form.bgnd_shapes_listbox.Items.Count < (1 + skip_count) then
    EXIT;

  case alert(7, '    pruning  -  delete  ' + str + '  of  all  shapes',
      '||You are about to remove ' + str +
      ' of all background shapes from the drawing by deleting alternate shapes from the list.'
      + '||This pruning function is mainly intended as a thinning-out operation after a large DXF file import to make room for additional shapes or to speed up the screen re-draws.' + '||If you will need all these shapes again they should first be saved to a file.' + '||Are you sure you want to delete ' + str + ' of all your shapes ?', '', '', '', 'yes  -  delete  ' + str + '  of  shapes', 'no  -  cancel  delete', 'save  first,  then  delete  ' + str, 0) of
    5:
      EXIT;
    6: begin
      shapes_saved := False;
      bgnd_form.save_all_menu_entry.Click;
      if shapes_saved = False then
        EXIT;
    end;
  end;//case

  with bgnd_form.bgnd_shapes_listbox.Items do begin

    n := 1;  // start at second line.

    repeat

      free_shape_object(n);  // free any picture bitmaps and the shape object.

      Delete(n);                              // delete the entry.

      n := n + skip_count;

    until n > (Count - 1);

    if Count > 0 then
      bgnd_form.bgnd_shapes_listbox.ItemIndex := 0;  // should be - we started at the second line!!!
  end;//with

  shapes_saved := False;                    // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.delete_50_buttonClick(Sender: TObject);

begin
  delete_percent(1, '50%');
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.delete_25_buttonClick(Sender: TObject);

begin
  delete_percent(3, '25%');
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.delete_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  try
    with bgnd_shapes_listbox do begin
      n := ItemIndex;
      with Items do begin
        if (Count < 1) or (n < 0) or (n > (Count - 1)) then
          EXIT;

        if alert(7, '      delete  this  shape ?',
          '|' + Strings[n] +
          '||You are about to delete this shape from the list.' +
          '||It is not possible to restore a shape which has been deleted.' +
          '||Are you sure you want to delete this shape ?', '', '', '',
          'yes  -  delete  ' + Strings[n], 'no  -  cancel  delete', '', 0) = 5 then
          EXIT;

        free_shape_object(n);  // free picture bitmaps if any, and the shape object

        Delete(n);                              // delete the entry.
        shapes_saved := False;                    // need a resave.
        if Count < 1 then
          EXIT;                   // list now empty.
        if n < Count then
          ItemIndex := n            // select next line
        else
          ItemIndex := Count - 1;
      end;//with
    end;//with
  finally
    shapes_current_state;
    do_rollback := False;       // no need to put this change in rollback register on redraw.
    redraw(True);
  end;//try
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.

  if (Key = VK_DELETE) and (name_editbox.Focused = False) then begin
    delete_menu_entry.Click;
    Key := 0;
  end;

end;
//_______________________________________________________________________________________

procedure Tbgnd_form.modify_buttonClick(Sender: TObject);

var
  now_shape: Tbgnd_shape;
  i, n: integer;
  img_file_str: string;
  old_shape_code: integer;
  dummy1, dummy2: integer;

begin
  with bgnd_shapes_listbox do begin
    n := ItemIndex;

    if (n < 0) or (n > (Items.Count - 1)) then begin
      show_modal_message('No background shape is currently selected in the list.');
      EXIT;
    end;

    now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;      // get existing data.

    with now_shape do begin

      old_shape_code := shape_code;

      // now set up new shape data...

      if name_editbox.Text <> '' then
        shape_name := name_editbox.Text;        // new name or label.

      shape_code := 0;
      // default line. 0=line, 1=rectangle, 2=circle, 3=label, 4=target, -1=picture.

      if target_radio_button.Checked = True then
        shape_code := 4;
      if label_radio_button.Checked = True then
        shape_code := 3;
      if circle_radio_button.Checked = True then
        shape_code := 2;
      if rect_radio_button.Checked = True then
        shape_code := 1;
      if picture_radio_button.Checked = True then
        shape_code := -1;

      shape_style := 0;
      // default transparent.  0=transparent, 1=blank/solid, 2=cross-hatched or dotted;

      case shape_code of
        0:
          if dotted_radio_button.Checked = True then
            shape_style := 2;

        1, 2: begin
          if solid_infill_radio_button.Checked = True then
            shape_style := 1;
          if hatched_radio_button.Checked = True then
            shape_style := 2;
        end;
      end;//case

      if get_shape_dims(True, shape_code, p1, p2) = False then
        EXIT;       // get the data for this shape. True=modifying

      if ((shape_code = 0) and (p1.x = p2.x) and (p1.y = p2.y)) or
        (((shape_code = -1) or (shape_code = 1) or (shape_code = 2)) and ((p1.x = p2.x) or (p1.y = p2.y)))
      then
        repeat
          i := alert(6, '    invalid  shape',
            'This shape will not appear because the length of the line or of one of the sides is zero.',
            '', '', '', '?  help', 'cancel  modify  -  shape  unchanged',
            'try  again', 4);
          case i of
            4:
              alert_help(0, inv_shape_help, '');
            5:
              EXIT;
          end;//case
        until i <> 4;

      if (shape_code <> -1) and (old_shape_code = -1)             // was a picture, but not now.
      then begin
        // so free the image (but not the shape object).
        if Tbgshape(Items.Objects[n]).bgimage <> nil    // ???
        then begin
          Tbgshape(Items.Objects[n]).bgimage.image_shape.image_bitmap.Free;
          // free the bitmaps.
          Tbgshape(Items.Objects[n]).bgimage.image_shape.rotated_bitmap.Free;

          { OT-FIRST
                            Tbgshape(Items.Objects[n]).bgimage.image_shape.image_metafile.Free;    // 213b
                            Tbgshape(Items.Objects[n]).bgimage.image_shape.rotated_metafile.Free;  // 213b
                            }

          Tbgshape(Items.Objects[n]).bgimage.image_shape.rotated_picture.Free;
          Tbgshape(Items.Objects[n]).bgimage.Free;
          Tbgshape(Items.Objects[n]).bgimage := nil;
          // and free the image object. 3-2-01.
        end;
      end;

      if (shape_code = -1) and (old_shape_code <> -1)        // changed to a picture - get picture.
      then begin
        img_file_str := get_user_image(False, False, False, False, False, n, dummy1, dummy2);
        // get image file and file name for list.
        shape_name := img_file_str;
        // in file.
        Items.Strings[n] := img_file_str;
        // and in list.
      end;

    end;//with

    Tbgshape(Items.Objects[n]).bgnd_shape := now_shape;      // put new data in list.

    ItemIndex := n;   // Delphi bug? - changing the strings changes this.

  end;//with
  shapes_saved := False;      // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure rectangle_to_lines(index: integer);

var
  x1, y1, x2, y2: double;
  new_shape: Tbgnd_shape;
  name_str: string;
  n: integer;

begin
  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if (index < 0) or (index > (Count - 1)) or (Count < 1) then
      EXIT;

    if Tbgshape(Objects[index]).bgnd_shape.shape_code <> 1 then
      EXIT;   // ??? not a rectangle.

    name_str := Strings[index];   // name of rectangle

    try
      with Tbgshape(Objects[index]).bgnd_shape do begin
        x1 := p1.x;
        y1 := p1.y;
        x2 := p2.x;
        y2 := p2.y;
      end;//with

      Tbgshape(Objects[index]).Free;
      Delete(index);                     // delete the entry.

      with new_shape do begin

        shape_code := 0;      // 0=line, 1=rectangle, 2=circle.

        if bgnd_form.dotted_radio_button.Checked = True then
          shape_style := 2
        else
          shape_style := 0;     // 0=transparent, 1=blank/solid, 2=cross-hatched or dotted.

        shape_name := name_str + ' ~ rec-line 1';

        hide_bits := 0;  // 214a  normal visibility
        option_bits := 0;     // byte;

        p1.x := x1;
        p1.y := y1;
        p2.x := x1;
        p2.y := y2;     // change y only for first line (vertical).
      end;//with

      n := AddObject(new_shape.shape_name, Tbgshape.Create);
      // create and add entry in the shapes list.
      Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.

      with new_shape do begin
        shape_name := name_str + ' ~ rec-line 2';

        hide_bits := 0;  // 214a  normal visibility
        option_bits := 0;     // byte;

        p1 := p2;
        p2.x := x2;    // change x only for second line (horizontal).
      end;//with

      n := AddObject(new_shape.shape_name, Tbgshape.Create);
      // create and add entry in the shapes list.
      Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.

      with new_shape do begin
        shape_name := name_str + ' ~ rec-line 3';

        hide_bits := 0;  // 214a  normal visibility
        option_bits := 0;     // byte;

        p1 := p2;
        p2.y := y1;    // change y only for third line (vertical).
      end;//with

      n := AddObject(new_shape.shape_name, Tbgshape.Create);
      // create and add entry in the shapes list.
      Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.

      with new_shape do begin
        shape_name := name_str + ' ~ rec-line 4';

        hide_bits := 0;  // 214a  normal visibility
        option_bits := 0;     // byte;

        p1 := p2;
        p2.x := x1;    // change x only for fourth line (horizontal).
      end;//with

      n := AddObject(new_shape.shape_name, Tbgshape.Create);
      // create and add entry in the shapes list.
      Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.

      shapes_saved := False;                    // will need a resave.
    finally
      shapes_current_state;
      do_rollback := False;       // no need to put this change in rollback register on redraw.
      redraw(True);
    end;//try

  end;//with
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.break_rectangle_menu_entryClick(Sender: TObject);

//procedure Tbgnd_form.rec_line_buttonClick(Sender: TObject);

var
  n: integer;
  name_str: string;

begin
  with bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    n := ItemIndex;
    if n < 0 then
      n := 0;                                      // nothing was selected.
    if n > (Items.Count - 1) then
      n := Items.Count - 1;            // ???

    if Tbgshape(Items.Objects[n]).bgnd_shape.shape_code <> 1 then
      EXIT;   // ??? not a rectangle.

    name_str := Items.Strings[n];   // name of rectangle

  end;//with

  if alert(7, '      convert  rectangle ?', '|' + name_str +
    '||You are about to convert this rectangle to 4 separate lines.' +
    '||This will make it easier to modify, but will remove any hatched or solid filling.'
    + '||The lines will be drawn solid or dotted according to the current setting.'
    + '||Are you sure you want to convert this rectangle ?',
    '', '', '', '', 'no  -  cancel    ', 'yes  -  convert  ' + name_str + '    ', 0) = 5 then
    EXIT;

  rectangle_to_lines(n);
end;
//__________________________________________________________________________________________

procedure do_bgnd(modify: boolean);  // show the form.

begin
  with bgnd_form do begin

    if modify = True then
      shape_page_control.ActivePage := modify_tab_sheet   // 214a
    else
      shape_page_control.ActivePage := new_tab_sheet;     // 206d

    Show;
    BringToFront;
  end;//with
end;
//________________________________________________________________________________________

procedure Tbgnd_form.mm_grid_buttonClick(Sender: TObject);

var
  str: string;

begin
  if grid_labels_code_i <> 6 then
    str := 'change  to  mm  on  existing  spacings'
  else
    str := '';

  case alert(3, '    grid  spacing  for  shapes',
      'For the dimensioning of background shapes, mm are the most convenient units for the grid line spacings.',
      '', '', '', str, 'no  change', 'change  grid  spacings ...', 0) of
    4: begin
      grid_labels_code_i := 6;
      do_rollback := False;
      // no need to put this change in rollback register on redraw.
      redraw(True);
    end;
    6:
      set_grid_spacings(bgnd_form);
  end;//case
end;
//________________________________________________________________________________________

procedure empty_bgsmru;   // 0.82.a

var
  n: integer;

begin
  bgsmru_list.Clear;
  for n := 0 to 5 do
    bgsmru_list.Add('');    // always 6 items.

  bgnd_form.clear_recent_bgs_popup_entry.Enabled := False;
end;
//___________________________________________________________________________________________

procedure bgsmru_update(file_str: string);   // update the mru list.  0.82.a  22-08-06

// remove one then add one - maintain 6 entries in list.
// bottom of list is top of menu.

var
  n: integer;

begin
  n := bgsmru_list.IndexOf(file_str);     // already in list?

  if n < 0 then
    bgsmru_list.Delete(0)     // no, remove oldest from top.
  else
    bgsmru_list.Delete(n);    // yes, remove it.

  bgsmru_list.Add(file_str);   // add new (or again) at bottom.

end;
//__________________________________________________________________________________________

procedure bgfile_error(str: string);     // generic error message.

begin
  alert(5, '     file  error',
    '||' + str + '||Sorry, the requested operation on this file has failed. Please check the file and folder names, and the drive letter.' + '||If this is a save operation, check the available space on the hard drive or USB memory stick.' + ' If overwriting an existing file, check that it is not read-only.' + '||If this is a reload operation, check that the named file exists in the named folder on the named drive.',
    '', '', '', '', '', 'O K', 0);
end;
//______________________________________________________________________________

function save_all_shapes_to_file(file_str: string; readable: boolean): boolean;
  // 291a  new BGS3 XML format  25-NOV-2019

var
  xml_doc: TNativeXml;
  shape_node, header_node: TXmlNode;

  n: integer;
  met_size: integer;
  met_copied: integer;
  p: Pointer;

  failed: boolean;
  waitMessage: IAutoWaitMessage;

  ////////////////////////////////////////////////////////////////////

  function create_named_node(parent_node: TXmlNode; name_str: string): TXmlNode;

  begin
    Result := TXmlNode.CreateName(xml_doc, name_str);
    parent_node.NodeAdd(Result);
  end;
  ////////////////////////////////////////////////////////////////////

begin
  Result := False; // init
  failed := False;

  waitMessage := TWaitForm.ShowWaitMessage('saving  background  shapes ...');

  if Application.Terminated = False then
    Application.ProcessMessages;     // let the wait form fully paint.

  xml_doc := TNativeXml.CreateName('BGS3');

  xml_doc.UseFullNodes := True;

  if readable = True then
    xml_doc.XmlFormat := xfReadable   // insert CRLF after each node
  else
    xml_doc.XmlFormat := xfCompact;   // image data on one line

  xml_doc.WriteOnDefault := True;       // ignore defaults and always write anyway
  xml_doc.FloatSignificantDigits := 9;  // sig digs in floats

  try
    with bgnd_form.bgnd_shapes_listbox.Items do begin

      if Count < 1 then
        EXIT; // no shapes

      header_node := create_named_node(xml_doc.Root, 'HEADER');

      with header_node do begin

        WriteString('program_name', 'OpenTemplot background shapes, saved from version ' +
          GetVersionString(voFull));

        WriteInteger('file_version', file_version, 0);

        WriteString('file_date', DateTimeToStr(Now), '');

        WriteInteger('shapes_count', Count, 0);

      end;//with header

      for n := 0 to Count - 1 do begin

        shape_node := create_named_node(xml_doc.Root, 'BGND_SHAPE_' + IntToStr(n));

        with Tbgshape(Objects[n]) do begin

          with bgnd_shape do begin

            // first ensure all fields valid, in case loaded from very old BGS files...  225b

            if shape_code <> -1  // not a picture..
            then begin
              wrap_offset := 0;            // used only for pictures..
              show_transparent := False;
              picture_is_metafile := False;
            end
            else begin     // picture..
              shape_style := 0;  // not used for picture

              show_transparent := (integer(show_transparent) = 1);
              picture_is_metafile := (integer(picture_is_metafile) = 1);
            end;

            if hide_bits > 3 then
              hide_bits := 0;

            option_bits := 0;  // spare

            with shape_node do begin

              WriteString('shape_name', shape_name, '');

              WriteInteger('wrap_offset', wrap_offset, 0);  // 1/100ths mm

              WriteBool('show_transparent', show_transparent, False);

              WriteInteger('shape_code', shape_code, 0);
              // 0=line, 1=rectangle, 2=circle, 3=text, 4=target mark, -1=picture (bitmap image or metafile)

              WriteInteger('shape_style', shape_style, 0);
              // byte  0=transparent, 1=blank/solid, 2=cross-hatched

              WriteBool('picture_is_metafile', picture_is_metafile, False);

              WriteInteger('hide_bits', hide_bits, 0);
              // byte  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both

              WriteInteger('option_bits', option_bits, 0);  // byte  spare

              WriteFloat('p1.x', p1.x, 0);       // Tpex...
              WriteFloat('p1.y', p1.y, 0);

              WriteFloat('p2.x', p2.x, 0);
              WriteFloat('p2.y', p2.y, 0);

              if shape_code = -1    // picture shape
              then begin
                with bgimage.image_shape do begin

                  WriteInteger('image_width', image_width, 0);    // dots
                  WriteInteger('image_height', image_height, 0);  // dots

                  if picture_is_metafile = True   // metafile...
                  then begin

                    WriteFloat('emf_width_mm', image_metafile.emf_width_mm, 0);
                    // EMF frame size in mm
                    WriteFloat('emf_height_mm', image_metafile.emf_height_mm, 0);

                    //  EMF
                    //met_size := GetEnhMetaFileBits(image_metafile.emf_HDC, 0, nil);
                    //// get size of EMF data

                    if met_size = 0 then begin
                      failed := True;
                      EXIT;
                    end;

                    try
                      GetMem(p, met_size);    // get memory buffer for it
                    except
                      memory_alert;          // say what happened
                      p := nil;
                      failed := True;
                      EXIT;
                    end;//try

                    // EMF
                    //met_copied :=
                    //  GetEnhMetaFileBits(image_metafile.emf_HDC, met_size, p);   // get the EMF contents into buffer

                    if met_copied = 0    // size of data copied
                    then begin
                      failed := True;
                      EXIT;
                    end;

                    XmlWriteHCKBuf(shape_node, 'EMF', p, met_size);
                    // Write buffer to XML node using HCK compression

                    FreeMem(p);   // free the buffer

                  end
                  else
                    XmlWriteBitmap(shape_node, 'BITMAP', image_bitmap); // not a metafile

                end;//with image
              end;//picture shape

            end;//with node
          end;//with bgnd_shape
        end;//with shape object
      end;//next shape
    end;//with listbox.Items

  finally
    if failed = True then
      show_modal_message('error: sorry unable to save background shapes')
    else
      xml_doc.SaveToFile(file_str);

    Result := not failed;
    xml_doc.Free;
  end;//try
end;
//______________________________________________________________________________


procedure Tbgnd_form.save_all_menu_entryClick(Sender: TObject);

// save all shapes to file
var
  bgs_str: string;

begin
  shapes_saved := False;                      // default result flag (for clear)

  if bgnd_shapes_listbox.Items.Count < 1 then
    EXIT;

  with filesave_dialog do begin             // set up the save dialog.

    if user_save_shapes_path = '' then
      InitialDir := Config.GetDir(cudiShapes)
    else
      InitialDir := user_save_shapes_path;

    Title := '    save  background  shapes  as ...';
    Filter := 'background shapes|*.bgs3';
    DefaultExt := '.bgs3';

    // mods 0.79.a

    FileName := remove_invalid_str(Copy(Trim(box_project_title_str), 1, 20) +
      FormatDateTime(' yy_mm_dd hhmm ss', Date + Time)) + '.bgs3';

    FileName := lower_case_filename(FileName);   // 0.79.a   to underscores and lower case.
  end;//with

  if filesave_dialog.Execute = True                // get her file name.
  then begin
    bgs_str := filesave_dialog.FileName;

    if invalid_85a_file_name(bgs_str) = True then
      EXIT;

    user_save_shapes_path := ExtractFilePath(bgs_str);   // for next time

    bgs_str := ChangeFileExt(bgs_str, '.bgs3');   // force extension

    if save_all_shapes_to_file(bgs_str, False) = False
    // False = compact XML format (not readable)
    then begin
      bgfile_error(bgs_str);
      user_save_shapes_path := '';
      EXIT;
    end;

    shapes_saved := True;

    bgs_file_label.Caption := ' shapes last saved to :  ' + bgs_str;
    bgs_file_label.Hint := bgs_file_label.Caption;  // in case too long for caption

    bgsmru_update(bgs_str);   // update the mru list.

    show_modal_message('Your background shapes have been saved to' + #13 + #13 + bgs_str);

  end;//if save dialog execute
end;
//______________________________________________________________________________

function load_shapes_from_file(file_str: string; reload_limits: boolean): boolean;
  // 292  new BGS3 XML format  25-NOV-2019

var
  xml_doc: TNativeXml;
  shape_node, header_node: TXmlNode;

  i, n, shapes_count: integer;

  next_shape: Tbgnd_shape;

  load_stream: TMemoryStream;
  waitMessage: IAutoWaitMessage;

begin
  Result := False;         // init

  waitMessage := TWaitForm.ShowWaitMessage('loading  background  shapes ...');

  Screen.Cursor := crHourGlass;  // might be slow if bitmaps

  if Application.Terminated = False then
    Application.ProcessMessages;   // let the wait form fully paint, show cursor

  xmin := zoom_offsetx;    // init limits for check
  ymin := zoom_offsety;

  xmax := xmin + screenx;
  ymax := ymin + screeny;

  xml_doc := TNativeXml.Create;

  xml_doc.LoadFromFile(file_str);    // this is slow

  header_node := xml_doc.Root.FindNode('HEADER');

  shapes_count := header_node.ReadInteger('shapes_count', 0);

  try
    if shapes_count < 1 then
      EXIT;

    for n := 0 to shapes_count - 1 do begin

      shape_node := xml_doc.Root.FindNode('BGND_SHAPE_' + IntToStr(n));

      with next_shape do begin
        with shape_node do begin

          shape_name := ReadString('shape_name', '');

          wrap_offset := ReadInteger('wrap_offset', 0);  // 1/100ths mm

          show_transparent := ReadBool('show_transparent', False);

          shape_code := ReadInteger('shape_code', 0);
          // 0=line, 1=rectangle, 2=circle, 3=text, 4=target mark, -1=picture (bitmap image or metafile)

          shape_style := ReadInteger('shape_style', 0);
          // byte  0=transparent, 1=blank/solid, 2=cross-hatched

          picture_is_metafile := ReadBool('picture_is_metafile', False);

          hide_bits := ReadInteger('hide_bits', 0);
          // byte  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both

          option_bits := ReadInteger('option_bits', 0);  // byte  spare

          p1.x := ReadFloat('p1.x', 0);                // Tpex...
          p1.y := ReadFloat('p1.y', 0);

          p2.x := ReadFloat('p2.x', 0);
          p2.y := ReadFloat('p2.y', 0);

          if reload_limits = True     // only within limits?
          then begin
            if shape_code < 3           // check both corners...
            then begin
              if (p1.x > xmax) or (p2.x > xmax) or (p1.y > ymax) or
                (p2.y > ymax) or (p1.x < xmin) or (p2.x < xmin) or
                (p1.y < ymin) or (p2.y < ymin) then
                CONTINUE;
            end
            else begin             // check only p1...
              if (p1.x > xmax) or (p1.y > ymax) or
                (p1.x < xmin) or (p1.y < ymin) then
                CONTINUE;
            end;
          end;

          with bgnd_form.bgnd_shapes_listbox.Items do begin

            i := AddObject(shape_name, Tbgshape.Create);
            // create and add a new line in the shapes list.

            Tbgshape(Objects[i]).bgnd_shape := next_shape;   // put data in list.

            Tbgshape(Objects[i]).bgimage := nil;  // init if not a picture

            if shape_code = -1    // picture shape
            then begin
              with Tbgshape(Objects[i]) do begin

                bgimage := Tbgimage.Create;

                with bgimage.image_shape do begin

                  image_width := ReadInteger('image_width', 0);    // dots
                  image_height := ReadInteger('image_height', 0);  // dots

                  image_bitmap := TBitmap.Create;
                  rotated_bitmap := TBitmap.Create;

                  rotated_picture := TPicture.Create;

                  if picture_is_metafile = True then begin

                    image_metafile.emf_width_mm := ReadFloat('emf_width_mm', 0);
                    // EMF frame size in mm
                    image_metafile.emf_height_mm := ReadFloat('emf_height_mm', 0);

                    load_stream := TMemoryStream.Create;
                    load_stream.Position := 0;

                    XmlReadHCKStream(shape_node, 'EMF', load_stream);

                    if load_stream.Size > 0
                    then
                    begin
                      load_stream.Position := 0;
                      // EMF
                      //image_metafile.emf_HDC :=
                      //  SetEnhMetaFileBits(load_stream.Size, load_stream.Memory);
                    end;
                    load_stream.Free;
                  end
                  else
                    XmlReadBitmap(shape_node, 'BITMAP', image_bitmap);

                end;//with image shape
              end;//with shape object
            end;//if picture shape

          end;//with list
        end;//with shape_node
      end;//with shape
    end;//next shape

    Result := True;

  finally
    Screen.Cursor := crDefault;
    xml_doc.Free;
  end;//try
end;
//______________________________________________________________________________

procedure load_shapes(file_str: string; append, mru, dropped: boolean);

// if file_str not empty, it is the filename to load.
// if mru=True, file_str is from mru list, not dialog.  // 0.82.a
// if dropped=True, file_str was dropped on the trackpad.  // 214a

var
  shape_file: Tshapefile;
  i, n: integer;
  bgs_str: string;
  next_shape: Tbgnd_shape;
  dummy1, dummy2: integer;
  top_str: string;

  sk7_image_load_count: integer;     // 213b
  sk8_image_load_count: integer;

  ext_str, sk7_str, sk8_str: string;    // 0.93.a

  drop_str: string;

begin
  if dropped = True                                         // 214a
  then
    drop_str := 'Dropped file:||`0' + file_str + '`f'
  else
    drop_str := '';

  with bgnd_form do begin
    try

      if (file_str = '')   // no file supplied, ask him...
        or (mru = True) or (dropped = True) then begin

        if (append = False) and (bgnd_shapes_listbox.Items.Count > 0)
        // reload clicked, and something already there ?
        then begin
          if shapes_saved = False                 // and not saved...
          then begin
            i :=
              alert(7, '      reload  shapes  -  save  first ?',
              drop_str +
              '||You have one or more background shapes which have not been saved.'
              +
              ' Reloading shapes from a file will replace all of the existing shapes.'
              + '||Do you want to save the existing shapes first?'
              + '||Or add new shapes to the existing ones instead?',
              '', '', 'add  new  shapes  to  existing    ',
              'replace  existing  shapes  without  saving    ', 'cancel  reload      ',
              'save  existing  shapes  before  reloading      ', 0);
            case i of
              3:
                append := True;
              5:
                EXIT;
              6: begin
                shapes_saved := False;
                save_all_menu_entry.Click;
                if shapes_saved = False then
                  EXIT;     // save cancelled.
              end;
            end;//case
          end
          else begin
            i :=
              alert(7, '      reload  shapes  -  delete  all  first ?',
              drop_str
              +
              '||Reloading shapes from a file will replace all of the existing shapes.'
              + '||Are you sure you want to do this?'
              + '||Or add new shapes to the existing ones instead?',
              '', '', '', 'add  new  shapes  to  existing  ',
              'cancel  reload      ', 'reload  and  replace  existing  shapes      ', 0);
            case i of
              4:
                append := True;
              5:
                EXIT;
            end;//case
          end;
        end;

        if (mru = False) and (dropped = False) then begin
          with fileload_dialog do begin

            if user_load_shapes_path = '' then
              InitialDir := Config.GetDir(cudiShapes)
            else
              Initialdir := user_load_shapes_path;

            if append = True then
              Title := '    add  background  shapes  from  file ...'
            else
              Title := '    reload  background  shapes  from  file ...';

            Filter := 'background shapes|*.bgs3;*.otbgs';

            //Filename:='*.bgs3';
            //DefaultExt:='.bgs3';

          end;//with

          if fileload_dialog.Execute = True                  // get the file name.
          then
            bgs_str := fileload_dialog.FileName
          else
            EXIT;
        end
        else
          bgs_str := file_str;  // file name is from mru list or a dropped file.

      end
      else
        bgs_str := file_str;    // file name supplied, e.g. startup parameter.

      user_load_shapes_path := ExtractFilePath(bgs_str); // for next time.

      if FileExists(bgs_str) = False then begin
        bgfile_error(bgs_str);
        user_load_shapes_path := '';
        EXIT;
      end
      else begin
        if append = False then
          clear_shapes;   // first clear all existing.

        if LowerCase(ExtractFileExt(bgs_str)) = '.bgs3'   // 291a new XML format
        then begin
          if load_shapes_from_file(bgs_str, reload_limits_checkbox.Checked) =
            False then begin
            bgfile_error(bgs_str);
            user_load_shapes_path := '';
            if append = False then
              clear_shapes;    // remove any added
            EXIT;
          end;
        end
        else begin                                               // old BGS format ...
          if LowerCase(ExtractFileExt(bgs_str)) <> '.otbgs'   // ???
          then begin
            bgfile_error(bgs_str);
            user_load_shapes_path := '';
            if append = False then
              clear_shapes;    // remove any added
            EXIT;
          end;

          xmin := zoom_offsetx;    // init for check
          ymin := zoom_offsety;

          xmax := xmin + screenx;
          ymax := ymin + screeny;

          sk7_image_load_count := 0;  // 213b init
          sk8_image_load_count := 0;  // 0.93.a init

          try
            AssignFile(shape_file, bgs_str);  // set the file name.
            Reset(shape_file);               // open the file.

            while EOF(shape_file) = False do begin

              with bgnd_shapes_listbox do begin

                if Items.Count >= count_limit then
                begin
                  alert(2, '    too  many  shapes',
                    '||You have now reached the limit of  ' +
                    IntToStr(count_limit) + '  background shapes. It is not possible to continue loading shapes from:'
                    + '|| ' + bgs_str
                    +
                    '||Try breaking the file into smaller sections and reloading each separately.',
                    '', '', '', '',
                    'cancel  loading  of  remaining  shapes        ', '', 0);
                  BREAK;
                end;

                Read(shape_file, next_shape);               // read next shape data.

                if reload_limits_checkbox.Checked = True     // only within limits?
                then begin
                  with next_shape do begin
                    if shape_code < 3           // check both corners...
                    then begin
                      if (p1.x > xmax) or
                        (p2.x > xmax) or (p1.y > ymax) or (p2.y > ymax) or
                        (p1.x < xmin) or (p2.x < xmin) or (p1.y < ymin) or (p2.y < ymin)
                      then
                        CONTINUE;
                    end
                    else begin             // check only p1...
                      if (p1.x > xmax) or
                        (p1.y > ymax) or (p1.x < xmin) or
                        (p1.y < ymin) then
                        CONTINUE;
                    end;
                  end;//with
                end;

                n := Items.AddObject(next_shape.shape_name, Tbgshape.Create);
                // create and insert a new line in the shapes list.

                Tbgshape(Items.Objects[n]).bgimage := nil;  // 3-2-01.

                if next_shape.shape_code = -1 then
                begin

                  if next_shape.picture_is_metafile =
                    True then begin
                    Inc(sk7_image_load_count);        // 213b

                    ext_str := '.sk7' + IntToStr(sk7_image_load_count);
                    // 213b
                    sk7_str := ChangeFileExt(bgs_str, ext_str);
                    // change file extension to .sk7n

                    if get_sk7_image(
                      n, sk7_str, dummy1, dummy2) = empty_picture_str
                    then
                    begin
                      next_shape.picture_is_metafile :=
                        False;
                      next_shape.shape_name :=
                        empty_picture_str;
                      Items.Strings[n] := empty_picture_str;
                    end;

                  end
                  else begin
                    Inc(sk8_image_load_count);        // 0.93.a

                    ext_str := '.sk8' + IntToStr(sk8_image_load_count);
                    // 0.93.a
                    sk8_str := ChangeFileExt(bgs_str, ext_str);
                    // change file extension to .sk8n

                    if get_sk8_image(
                      n, ExtractFilePath(bgs_str) + next_shape.shape_name, sk8_str, dummy1, dummy2) =
                      empty_picture_str    // 0.82.a look in same folder as loaded bgs file
                    then begin
                      next_shape.shape_name :=
                        empty_picture_str;
                      Items.Strings[n] := empty_picture_str;
                    end;

                  end;
                end;

                Tbgshape(Items.Objects[n]).bgnd_shape := next_shape;
                // put data in list.

                ItemIndex := n;
                // make it current.
              end;//with
            end;//while
            CloseFile(shape_file);
          except
            on EInOutError do begin
              bgfile_error(bgs_str);
              user_load_shapes_path := '';
              if append = False then
                clear_shapes;
              EXIT;
            end;
          end;//try-except

        end;//old BGS format

        if append = False then begin
          top_str := ' shapes last reloaded from :  ';
          shapes_saved := True;
          // list matches file.
          if close_checkbox.Checked = True then
            close_button.Click;
        end
        else
          top_str := ' shapes last added from :  ';

        bgs_file_label.Caption := top_str + bgs_str;
        bgs_file_label.Hint := bgs_file_label.Caption;  // in case too long for caption

        bgsmru_update(bgs_str);   // 0.82.a  update the mru list.

        pad_form.fit_shapes_menu_entry.Click; // 0.82.a  show loaded shapes.

      end;//if file exists

    finally
      shapes_current_state;
      do_rollback := False;       // no need to put this change in rollback register on redraw.
      redraw(True);
    end;//try
  end;//with form
end;
//______________________________________________________________________________________________

procedure Tbgnd_form.bgnd_shapes_listboxClick(Sender: TObject);

var
  j: integer;

begin
  shapes_current_state;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(False);            // now. remove any previous highlighting

  with bgnd_shapes_listbox do
    draw_bg_shapes(pad_form.Canvas, ItemIndex, clRed);
  // highlight this one in red, directly on the pad.

end;
//______________________________________________________________________________

procedure Tbgnd_form.bgnd_shapes_listboxDblClick(Sender: TObject);

begin

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[bgnd_shapes_listbox.ItemIndex]).bgnd_shape do
  begin

    if (hide_bits and $01) = $01
    // byte,  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both
    then
      hide_bits := hide_bits and $FE    // turn bit off, don't hide shape
    else
      hide_bits := hide_bits or $01;    // turn bit on, hide shape
  end;//with

  shapes_current_state;

  do_rollback := False;    // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tbgnd_form.label_font_buttonClick(Sender: TObject);

begin
  shapes_label_font.Assign(get_font('choose  a  new  font  and  text  colour  for  labels',
    shapes_label_font, True));
  redraw(True);
end;
//___________________________________________________________________________________________

procedure Tbgnd_form.FormCreate(Sender: TObject);

begin
  shapes_label_font := TFont.Create;

  if Screen.Height < 500 then begin
    Top := 2;    // move form top left of screen for lo-res.
    Left := 2;
  end;

  // OT-FIRST ClientWidth:=640;
  // OT-FIRST ClientHeight:=610;
  AutoScroll := True;

  if Screen.PixelsPerInch > 120 then
    bgnd_shapes_listbox.ItemHeight := 26;  // 214a  for owner-draw

  // 0.82.a  22-08-06  recent files...

  // 208d bug fix - can't use exe_str because not yet initialised

  if FileExists(Config.GetFilePath(csfiBgndMRU)) then
    bgsmru_list.LoadFromFile(Config.GetFilePath(csfiBgndMRU))
  else
    empty_bgsmru;

  if bgsmru_list.Count <> 6 then
    empty_bgsmru;   // file tampered with?

end;
//__________________________________________________________________________________________

procedure Tbgnd_form.FormShow(Sender: TObject);

begin
  shapes_current_state;

  new_picture_shape1.Visible := False;    // 214a
  new_picture_shape2.Visible := False;    // 214a
  new_picture_shape3.Visible := False;    // 214a

end;
//_____________________________________________________________________________________________

procedure Tbgnd_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  save_hide := bgs_save_hide;        // restore pad flag.
  pad_form.Cursor := cursor_saved;   // restore cursor.
  redraw(False);
  // if no mouse action in force, cancel any highlighting (immediate so can change cursor for mouse drawing).
end;
//____________________________________________________________________________________________

procedure Tbgnd_form.FormActivate(Sender: TObject);

begin
  with bgnd_shapes_listbox do begin
    if Items.Count < 1 then
      Hide        // list empty, reveal "no shapes" label.
    else
      Show;
  end;//with

  if bgnd_no_update = False then
    shapes_current_state;   // re-showing after click on the form, he might have set the options for the clicked shape.
  bgnd_no_update := False;                               // but only skip it once.

  bgs_save_hide := save_hide;                            // so can restore on close.
  save_hide := False;
  // otherwise pad redraws on clicking (pad_form.Activate).

  if check_grey_paper = False then begin
    cursor_saved := pad_form.Cursor;
    pad_form.Cursor := cross_hairs_cursor;
    //crCross;       // so can select shape locations.
  end;

  paste_button.Enabled := Clipboard.HasFormat(CF_BITMAP);
end;
//_____________________________________________________________________________________________

procedure Tbgnd_form.draw_mouse_menu_entryClick(Sender: TObject);

begin
  pad_form.mouse_draw_menu_entry.Click;
end;
//_____________________________________________________________________________________________

procedure Tbgnd_form.pad_colour_buttonClick(Sender: TObject);

begin
  pad_form.shapes_colour_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.print_colour_buttonClick(Sender: TObject);

begin
  pad_form.printed_shapes_colour_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.shift_all_by_buttonClick(Sender: TObject);

const
  help_xshift_str: string = '     Shift  all  shapes  on  X.' +
    '||Enter an X-dimension in millimetres for the amount to shift all your background shapes across the drawing to left or right.'
    + '||X-dimensions are measured across the width of the screen, positive from left towards the right.'
    + '||( Enter zero if you want to change Y-dimensions up or down only.)'
    + '||To change the dimensions for a single shape only, enter zero here then select it in the list and click the MODIFY SHAPE tab > CHANGE... button.' + '||Shifting shapes can also be done with a mouse action, click the MOUSE ACTIONS ALL tab.';

  help_yshift_str: string = '     Shift  all  shapes  on  Y.' +
    '||Enter a Y-dimension in millimetres for the amount to shift all your background shapes up or down on the drawing.'
    + '||Y-dimensions are measured vertically on the screen, the positive direction is upwards from the bottom.'
    + '||( Enter zero if you want to change X-dimensions across the screen only.)'
    + '||To change the dimensions for a single shape only, enter zero here then select it in the list and click the MODIFY SHAPE tab > CHANGE... button.' + '||Shifting shapes can also be done with a mouse action, click the  MOUSE ACTIONS ALL tab.';
var
  n: integer;
  od: Toutdim;

begin

  if bgnd_form.bgnd_shapes_listbox.Items.Count < 1 then
    EXIT;

  putdim(help_xshift_str, 1, 'shift  all  background  shapes  by  X  ', 0, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.
  n := putdim(help_yshift_str, 1, 'shift  all  background  shapes  by  Y  ', 0, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.

  if n <> 1 then
    EXIT;
  if getdims('shift  all  background  shapes', '', bgnd_form, n, od) = True then
    shift_all_shapes(od[0], od[1]);

  shapes_current_state;
  shapes_saved := False;      // need a resave.
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//____________________________________________________________________________________________

procedure scale_this_shape(xfactor, yfactor: double);

var
  n: integer;
  now_shape: Tbgnd_shape;
  shape_width, shape_height: double;

begin
  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    n := ItemIndex;      // list line selected.
    if n < 0 then
      EXIT;

    now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;      // get existing data.

    with now_shape do begin

      if shape_code < 3                   // picture, line, rectangle, circle.
      then begin
        shape_width := p2.x - p1.x;           // scale the size, don't modify p1.
        shape_height := p2.y - p1.y;

        if shape_code = -1  // picture
        then begin
          if bgnd_form.lock_to_notch_radiobutton.Checked = True
          then begin
            p1.x := notchx + (p1.x - notchx) * xfactor;      // lock to notch
            p1.y := notchy + (p1.y - notchy) * yfactor;
          end;

          if bgnd_form.lock_to_ring_radiobutton.Checked = True
          then begin
            p1.x := rings[0, 0] + (p1.x - rings[0, 0]) * xfactor;
            // lock to spacing-ring
            p1.y := rings[0, 1] + (p1.y - rings[0, 1]) * yfactor;
          end;
        end;

        p2.x := p1.x + shape_width * xfactor;
        p2.y := p1.y + shape_height * yfactor;
      end
      else begin
        p1.x := p1.x * xfactor;     // label or target - scale the position.
        p1.y := p1.y * yfactor;
      end;
    end;//with

    Tbgshape(Items.Objects[n]).bgnd_shape := now_shape;      // put new data in list.

  end;//with
  shapes_saved := False;
end;
//_________________________________________________________________________________________

procedure scale_all_shapes(xfactor, yfactor: double);

var
  n: integer;
  now_shape: Tbgnd_shape;
  shape_width, shape_height: double;

begin
  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    for n := 0 to (Items.Count - 1) do begin

      now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;      // get existing data.

      with now_shape do begin

        if (shape_code = -1) and (bgnd_form.lock_to_origin_radiobutton.Checked = False)
        // picture   205e
        then begin

          shape_width := p2.x - p1.x;           // scale the size, don't modify p1.
          shape_height := p2.y - p1.y;

          if bgnd_form.lock_to_notch_radiobutton.Checked = True then
          begin
            p1.x := notchx + (p1.x - notchx) * xfactor;      // lock to notch
            p1.y := notchy + (p1.y - notchy) * yfactor;
          end;

          if bgnd_form.lock_to_ring_radiobutton.Checked = True then begin
            p1.x := rings[0, 0] + (p1.x - rings[0, 0]) * xfactor;   // lock to spacing-ring
            p1.y := rings[0, 1] + (p1.y - rings[0, 1]) * yfactor;
          end;

          p2.x := p1.x + shape_width * xfactor;
          p2.y := p1.y + shape_height * yfactor;

        end
        else begin

          p1.x := p1.x * xfactor;
          p1.y := p1.y * yfactor;

          if shape_code < 3 then begin
            p2.x := p2.x * xfactor;
            p2.y := p2.y * yfactor;
          end;
        end;

      end;//with

      Tbgshape(Items.Objects[n]).bgnd_shape := now_shape;      // put new data in list.

    end;//next shape
  end;//with
  shapes_saved := False;
end;
//_______________________________________________________________________________________

procedure shift_all_shapes(xshift, yshift: double);

var
  n: integer;
  now_shape: Tbgnd_shape;

begin
  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    for n := 0 to (Items.Count - 1) do begin

      now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;      // get existing data.

      with now_shape do begin

        p1.x := p1.x + xshift;
        p1.y := p1.y + yshift;

        if shape_code < 3 then begin
          p2.x := p2.x + xshift;
          p2.y := p2.y + yshift;
        end;

      end;//with

      Tbgshape(Items.Objects[n]).bgnd_shape := now_shape;      // put new data in list.
    end;//next shape
  end;//with
  shapes_saved := False;
end;
//_______________________________________________________________________________________

procedure shift_this_shape(xshift, yshift: double; corners: integer);

// corners  to shift : 0=both , 1=p1,  2=p2.

var
  n: integer;
  now_shape: Tbgnd_shape;

begin
  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    n := ItemIndex;      // line selected.
    if n < 0 then
      EXIT;

    now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;      // get existing data.

    with now_shape do begin

      if corners < 2 then begin
        p1.x := p1.x + xshift;
        p1.y := p1.y + yshift;
      end;

      if (shape_code < 3) and (corners <> 1) then begin
        p2.x := p2.x + xshift;
        p2.y := p2.y + yshift;
      end;
    end;//with

    Tbgshape(Items.Objects[n]).bgnd_shape := now_shape;      // put new data in list.
  end;//with
  shapes_saved := False;
end;
//________________________________________________________________________________________

procedure rotate_this_shape(n: integer; rot_k: double);

// rotate this shape by rot_k around xrot,yrot
// picture shape not done here
var
  recp: Tpex;
  xway, yway: double;

  xrot, yrot: double;

begin
  with bgnd_form.bgnd_shapes_listbox.Items do begin

    if (Count < 1) or (n < 0) or (n > (Count - 1)) then
      EXIT;

    case rotate_centre_code of
      1: begin
        xrot := notchx;
        yrot := notchy;
      end;  // rotate shape around notch
      2: begin
        xrot := rings[0, 0];
        yrot := rings[0, 1];
      end;  // rotate shape around spacing ring
      else begin
        xrot := 0;
        yrot := 0;
      end;  // 3: rotate around grid origin
    end;//case

    with Tbgshape(Objects[n]).bgnd_shape do begin      // get existing data.

      case shape_code of

        0: begin                                   // line shape.
          dotransform(rot_k, xrot, yrot, p1, p1);   // rotate end points..
          dotransform(rot_k, xrot, yrot, p2, p2);
        end;

        1, 2: begin
          // rectangle, circle shapes.     (rectangle should have been broken to lines)
          recp.x := (p1.x + p2.x) / 2;    // centre of rectangle..
          recp.y := (p1.y + p2.y) / 2;

          xway := ABS(recp.x - p1.x);   // x size each way from centre.
          yway := ABS(recp.y - p1.y);   // y size each way from centre.

          dotransform(rot_k, xrot, yrot, recp, recp);    // rotate centre.

          p1.x := recp.x - xway;   // restore rectangle...
          p1.y := recp.y - yway;

          p2.x := recp.x + xway;
          p2.y := recp.y + yway;

        end;

        3, 4: begin                                   // label shapes, target marks.
          dotransform(rot_k, xrot, yrot, p1, p1);   // rotate corner or point.
        end;
      end;//case

    end;//with shape
  end;//with list Items

  shapes_saved := False;      // need a resave.
end;
//______________________________________________________________________________

procedure rotate_all_shapes(sync, pictures: boolean; rot_k: double);

var
  n: integer;
  rectangles_exist: boolean;

  twist_angle: double;

begin
  with bgnd_form.bgnd_shapes_listbox.Items do begin

    if Count < 1 then
      EXIT;

    rectangles_exist := False; // init

    for n := 0 to (Count - 1) do begin
      if Tbgshape(Objects[n]).bgnd_shape.shape_code = 1 then
        rectangles_exist := True;
    end;//for

    if rectangles_exist = True then begin
      if alert(7, '      convert  all  rectangles ?',
        '|You are about to convert all rectangle shapes, each to 4 separate lines.'
        + '||This is necessary to permit rotation of the rectangle outlines, but will remove any hatched or solid filling.' + '||The lines will be drawn solid or dotted according to the current setting.' + '||Are you sure you want to convert all rectangles ?', '', '', '', '', 'no  -  cancel  rotations', 'yes  -  convert  all  rectangles  and  rotate', 0) = 5 then
        EXIT;

      n := 0;
      while n < Count do begin
        // (Count increases by 3 for every rectangle converted.)
        if Tbgshape(Objects[n]).bgnd_shape.shape_code = 1  // code 1 = rectangle.
        then
          rectangle_to_lines(n)                   // don't increment n, this entry gets deleted.
        else
          Inc(n);
      end;//while
    end;

    for n := 0 to (Count - 1) do begin
      if (Tbgshape(Objects[n]).bgnd_shape.shape_code = -1) and (pictures = True) then begin
        twist_angle := 0 - rot_k;

        if twist_angle > 0 then begin
          while twist_angle > (Pi / 2) do begin          // 90 deg steps first
            twist_picture(n, Pi / 2, True, True, sync);
            twist_angle := twist_angle - Pi / 2;
          end;
        end;

        if twist_angle < 0 then begin
          while twist_angle < (0 - Pi / 2) do begin        // 90 deg steps first
            twist_picture(n, 0 - Pi / 2, True, True, sync);
            twist_angle := twist_angle + Pi / 2;
          end;
        end;

        twist_picture(n, twist_angle, True, True, sync);
      end
      else
        rotate_this_shape(n, rot_k);
    end;

  end;//with
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.shift_one_to_buttonClick(Sender: TObject);  // 214a

const
  not_this_str: string = '     `0shift  shape  to ...`9' +
    '||This function applies to rectangles and picture shapes only.' +
    '||The currently selected shape is not a rectangle or a picture shape.' +
    '||To change the position of this shape please click the|`0SHIFT BY...`1 button or the `0CHANGE DIMENSIONS...`1 button.'
    + '||green_panel_begin tree.gif  Shapes can also be shifted using the mouse, click the `0SHIFT`1 button on the `0MOUSE ACTIONS:`1 panel.green_panel_end';

var
  n, nn: integer;
  now_shape: Tbgnd_shape;
  od: Toutdim;

  xshift_to_str, yshift_to_str, type_str: string;

  shape_width, shape_height: double;

begin
  type_str := '';

  with bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    n := ItemIndex;      // line selected.
    if n < 0 then
      EXIT;

    now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;      // get existing data.

    with now_shape do begin

      if (shape_code <> 1) and (shape_code <> -1) then begin
        help(0, not_this_str, '');
        EXIT;
      end;

      case shape_code of
        -1:
          type_str := 'picture';

        1:
          type_str := 'rectangle';

      end;//case


      xshift_to_str := '     `0shift  ' + type_str + '  shape  horizontally  to  X`9'
        + '||Enter an X-dimension in millimetres for the new position of the left edge of this '
        +
        type_str + ' shape.' +
        '||X-dimensions are measured horizontally across screen, positive from the left towards the right.'
        + '||Enter zero 0 if you want to shift this ' + type_str +
        ' shape to the left grid origin line.' +
        '||Leave the X-dimension unchanged if you do not want to shift this ' + type_str +
        ' shape horizontally.' +
        '||green_panel_begin tree.gif  Shapes can also be shifted using the mouse, click the `0SHIFT`1 button on the `0MOUSE ACTIONS:`1 panel.green_panel_end';


      yshift_to_str := '     `0shift  ' + type_str + '  shape  vertically  to  Y`9'
        + '||Enter a Y-dimension in millimetres for the new position of the bottom edge of this '
        +
        type_str + ' shape.' +
        '||Y-dimensions are measured vertically on the screen, the positive direction is upwards from the bottom.'
        + '||Enter zero 0 if you want to shift this ' + type_str +
        ' shape to the bottom grid origin line.' +
        '||Leave the Y-dimension unchanged if you do not want to shift this ' + type_str +
        ' shape vertically.' +
        '||green_panel_begin tree.gif  Shapes can also be shifted using the mouse, click the `0SHIFT`1 button on the `0MOUSE ACTIONS:`1 panel.green_panel_end';


      normalize_rectangle(p1, p2);

      shape_width := p2.x - p1.x;
      shape_height := p2.y - p1.y;

      putdim(xshift_to_str, 1, 'shift  ' + type_str + '  shape  horizontally  to  X ',
        p1.x, False, True, False, False);   // negative ok, no preset, zero ok, don't terminate on zero.
      nn := putdim(yshift_to_str, 1, 'shift  ' + type_str + '  shape  vertically  to  Y ',
        p1.y, False, True, False, False);   // negative ok, no preset, zero ok, don't terminate on zero.

      if nn <> 1 then
        EXIT;
      if getdims('shift  background  ' + type_str + '  shape  to ...', '', bgnd_form, nn, od) =
        True then begin

        p1.x := od[0];
        p1.y := od[1];

        p2.x := p1.x + shape_width;
        p2.y := p1.y + shape_height;
      end;

    end;//with

    Tbgshape(Items.Objects[n]).bgnd_shape := now_shape;      // put new data in list.

  end;//with

  shapes_current_state;
  shapes_saved := False;      // need a resave.

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(False);            // now

  draw_bg_shapes(pad_form.Canvas, n, clRed);   // highlight this one in red, directly on the pad.
end;
//______________________________________________________________________________

procedure Tbgnd_form.scale_one_to_buttonClick(Sender: TObject);   // 214a

const
  not_this_str: string = '     `0re-size  shape  to  new  width ...`9' +
    '||This function applies to rectangles and picture shapes only.' +
    '||The currently selected shape is not a rectangle or a picture shape.' +
    '||To change the size of this shape please click the|`0SCALE BY...`1 button or the `0CHANGE DIMENSIONS...`1 button.'
    + '||green_panel_begin tree.gif  Shapes can also be re-sized using the mouse, click the `0SIZE`1 button on the `0MOUSE ACTIONS:`1 panel.green_panel_end';

var
  n, nn: integer;
  od: Toutdim;

  scale_to_str, type_str: string;

  shape_width, scale_factor: double;

begin
  type_str := '';

  with bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    n := ItemIndex;      // line selected.
    if n < 0 then
      EXIT;

    with Tbgshape(Items.Objects[n]).bgnd_shape do begin

      if (shape_code <> 1) and (shape_code <> -1) then begin
        help(0, not_this_str, '');
        EXIT;
      end;

      case shape_code of
        -1:
          type_str := 'picture';
        1:
          type_str := 'rectangle';

      end;//case


      scale_to_str := '     `0re-size  ' + type_str + '  shape  to  new  width`9'
        + '||Enter a dimension in millimetres for the new width of this ' +
        type_str + ' shape.' +
        '||The height will be adjusted to maintain the existing aspect ratio.'
        + '||green_panel_begin tree.gif  Shapes can also be re-sized using the mouse, click the `0SIZE`1 button on the `0MOUSE ACTIONS:`1 panel.' + '||Or to change the position, size and aspect ratio of this ' + type_str + ' shape in one go, click the `0CHANGE DIMENSIONS...`1 button.green_panel_end';

      if shape_code = -1 then
        scale_to_str := scale_to_str +
          '|green_panel_begintree.gif A picture shape can be locked to the position on the trackpad of the pegging notch or the spacing-ring by selecting' + ' the `0when scaling (re-sizing) lock to:`1 options on the `0picture options`1 tab.' + '||This is useful when scaling (re-sizing) a map or track plan image to match the model scale of your track templates.green_panel_end';

      normalize_rectangle(p1, p2);

      shape_width := p2.x - p1.x;

    end;//with shape
  end;//with listbox

  if shape_width < minfp then begin
    help(0, '||Error - this ' + type_str +
      ' shape has zero width and cannot be scaled to a new width.' +
      '||Click the `0CHANGE DIMENSIONS...`1 button to change the width.', '');
    EXIT;
  end;

  nn := putdim(scale_to_str, 1, 're-size  ' + type_str + '  shape  to  new  width ',
    shape_width, True, True, True, False);   // no negative, no preset, no zero, don't terminate on zero.

  if nn <> 0 then
    EXIT;
  if getdims('re-size  background  ' + type_str + '  shape ...', '', bgnd_form, nn, od) = True then begin
    scale_factor := od[0] / shape_width;
    scale_this_shape(scale_factor, scale_factor);
  end;

  shapes_current_state;
  shapes_saved := False;      // need a resave.

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(False);            // now

  draw_bg_shapes(pad_form.Canvas, n, clRed);   // highlight this one in red, directly on the pad.
end;
//______________________________________________________________________________

procedure Tbgnd_form.shift_one_by_buttonClick(Sender: TObject);

const
  xshift_str: string = '     `0Shift  shape  horizontally  on  X`9' +
    '||Enter an X-dimension in millimetres for the amount to shift this background shape sideways across the trackpad to left or right.'
    + '||X-dimensions are measured horizontally across screen, positive from the left towards the right.'
    + '||(Enter zero 0 if you want to shift this shape up or down only.)';

  yshift_str: string = '     `0Shift  shape  vertically  on  Y`9' +
    '||Enter a Y-dimension in millimetres for the amount to shift this background shape up or down on the trackpad.'
    + '||Y-dimensions are measured vertically on the screen, the positive direction is upwards from the bottom.'
    + '||(Enter zero 0 if you want to shift this shape sideways only.)';

  all_shift_str: string =
    '||green_panel_begin tree.gif  To shift all the shapes in one go by the same amount, click the `0SHIFT ALL BY...`1 button on the `0MODIFY ALL`1 tab.' + '||Shapes can also be shifted by mouse action, click the `0SHIFT`1 button on the `0MOUSE ACTIONS:`1 panel.green_panel_end';

var
  n: integer;
  od: Toutdim;

begin

  if bgnd_shapes_listbox.Items.Count < 1 then
    EXIT;

  putdim(xshift_str + all_shift_str, 1, 'shift  shape  horizontally  by  X ',
    0, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.
  n := putdim(yshift_str + all_shift_str, 1, 'shift  shape  vertically  by  Y ',
    0, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.

  if n <> 1 then
    EXIT;
  if getdims('shift  background  shape', '', bgnd_form, n, od) = True then
    shift_this_shape(od[0], od[1], 0);

  shapes_current_state;
  shapes_saved := False;      // need a resave.

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(False);            // now

  draw_bg_shapes(pad_form.Canvas, bgnd_shapes_listbox.ItemIndex, clRed);
  // highlight this one in red, directly on the pad.
end;
//____________________________________________________________________________________________

procedure Tbgnd_form.rotate_one_by_buttonClick(Sender: TObject);

const
  rotshap_str: string = '     `0rotate  background  shape`9' +
    '||Enter an angle in degrees for the amount by which to rotate this background shape.'
    + '||Angles are measured positive in the anti-clockwise direction, and negative in the clockwise direction.'
    + '||This operation can be undone by rotating in the opposite direction by the same amount.'
    + '||The centre of rotation can be the grid origin, or the current position of the notch, or the current position of the spacing-ring.' + ' These options are set on the `0modify all`1 tab > `0rotate all around:`1 panel.' + '||The current setting is to rotate this background shape around the';

var
  n, i, j: integer;
  od: Toutdim;

  new_count, old_count: integer;

  cen_str, rot_cen_str: string;

  rotshap_k: double;

  // shape_code: 0=line, 1=rectangle, 2=circle, 3=label, 4=target, -1=picture

begin
  old_count := bgnd_form.bgnd_shapes_listbox.Items.Count;

  if old_count < 1 then
    EXIT;

  j := bgnd_form.bgnd_shapes_listbox.ItemIndex;

  if j < 0 then
    EXIT;
  if j > (old_count - 1) then
    EXIT;

  if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[j]).bgnd_shape.shape_code = -1
  // picture shape not done here
  then begin
    bgnd_form.twist_picture_button.Click;
    EXIT;
  end;

  case rotate_centre_code of
    1:
      rot_cen_str := '||                              <b>notch</b>.';
    2:
      rot_cen_str := '||                              <b>spacing-ring</b>.';
    else
      rot_cen_str := '||                              <b>grid origin</b>.';
  end;//case

  case rotate_centre_code of
    1:
      cen_str := 'notch';
    2:
      cen_str := 'spacing-ring';
    else
      cen_str := 'grid origin';
  end;//case

  if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[j]).bgnd_shape.shape_code = 1  // rectangle
  then begin
    if alert(7, '      rotate  rectangle  shape',
      'The selected background shape is a rectangle.'
      + '||A rectangle must be broken into 4 separate lines in order to be rotated. Any existing infill will be discarded.', '', '', '', '', 'cancel  rotate', 'OK  break  rectangle  to  4  lines  and  rotate', 0) = 5 then
      EXIT;

    rectangle_to_lines(j);
  end;

  rotshap_k := 0;  // init

  n := putdim(rotshap_str + rot_cen_str, 3, 'rotate  background  shape  around  ' +
    cen_str + '  by', rotshap_k * 180 / Pi, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.

  if n <> 0 then
    EXIT;
  if getdims('rotate  background  shape', '', bgnd_form, n, od) = True then begin
    rotshap_k := od[0] * Pi / 180;
    normalize_angle(rotshap_k);
  end
  else
    EXIT;

  new_count := bgnd_form.bgnd_shapes_listbox.Items.Count;

  if new_count > old_count      // rectangle got converted to 4 lines
  then begin
    rotate_this_shape(new_count - 4, rotshap_k);
    rotate_this_shape(new_count - 3, rotshap_k);
    rotate_this_shape(new_count - 2, rotshap_k);
    rotate_this_shape(new_count - 1, rotshap_k);
  end
  else
    rotate_this_shape(j, rotshap_k);

  shapes_saved := False;      // need a resave.
  shapes_current_state;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(False);            // now

  draw_bg_shapes(pad_form.Canvas, bgnd_shapes_listbox.ItemIndex, clRed);
  // highlight this one in red, directly on the pad.

end;
//______________________________________________________________________________

procedure Tbgnd_form.scale_one_by_buttonClick(Sender: TObject);

const
  help_scale_one_str: string = '     `0scale  ( re-size )  this  shape`9'
    + '||Enter a scaling factor % for the amount by which to scale (enlarge or reduce) the currently selected shape.'
    + '||For example, a factor of 200% will double the size of the shape.'
    + '||If the shape is a label or target mark, scaling changes its position. For other shapes the position on the grid is unchanged and only the size of the shape changes.' + '||green_panel_begintree.gif A picture shape can be locked instead to the position on the trackpad of the pegging notch or the spacing-ring by selecting' + ' the `0when scaling (re-sizing) lock to:`1 options on the `0picture options`1 tab.' + '||This is useful when scaling (re-sizing) a map or track plan image to match the model scale of your track templates.green_panel_end' + '|To change the position after scaling click the `0SHIFT TO...`1 or `0SHIFT BY...`1 or `0CHANGE DIMENSIONS...`1 buttons on the `0MODIFY SHAPE`1 tab.' + '||If a negative factor is entered, the shape will additionally be rotated 180 degrees. For a label or target mark, this rotation will be around the grid origin.' + '||A zero factor is not valid. To delete the shape click the `0EDIT > DELETE SHAPE`1 menu item.' + '||To change the dimensions for all the shapes, cancel this and then click the `0SCALE ALL BY...`1 button on the `0MODIFY ALL`1 tab.';
var
  n: integer;
  od: Toutdim;

begin

  if bgnd_shapes_listbox.Items.Count < 1 then
    EXIT;

  n := putdim(help_scale_one_str, 4, 'scale  this  background  shape  by  ', 100, False, True, True, False);
  // negative ok, no preset, no zero, don't terminate on zero.

  if n <> 0 then
    EXIT;
  if getdims('scale  background  shape', '', bgnd_form, n, od) = True then
    scale_this_shape(od[0] / 100, od[0] / 100);

  shapes_current_state;
  shapes_saved := False;      // need a resave.

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(False);            // now

  draw_bg_shapes(pad_form.Canvas, bgnd_shapes_listbox.ItemIndex, clRed);
  // highlight this one in red, directly on the pad.
end;
//______________________________________________________________________________

procedure Tbgnd_form.scale_all_by_buttonClick(Sender: TObject);

const
  help_scale_all_str: string = '     Scale  all  shapes.' +
    '||Enter a scaling factor % for the amount by which to scale (enlarge or reduce) all your background shapes.'
    + '||For example, a factor of 200% will double the size of all your shapes.'
    + '||Scaling all shapes also changes their position on the drawing. To adjust the positions after scaling use the buttons on the MODIFY ALL tab.' + '||If a negative factor is entered, the shapes will additionally be rotated 180 degrees around the drawing origin.' + '||A zero factor is not valid. To delete all your shapes click the DELETE ALL button instead.' + '||To change the dimensions for a single shape only, cancel this, select the shape in the list and then click the buttons on the MODIFY SHAPE tab.';
var
  n: integer;
  od: Toutdim;

begin

  if bgnd_form.bgnd_shapes_listbox.Items.Count < 1 then
    EXIT;

  n := putdim(help_scale_all_str, 4, 'scale  all  background  shapes  by  ', 100, False, True, True, False);
  // negative ok, no preset, no zero, don't terminate on zero.

  if n <> 0 then
    EXIT;
  if getdims('scale  all  background  shapes', '', bgnd_form, n, od) = True then
    scale_all_shapes(od[0] / 100, od[0] / 100);

  shapes_current_state;
  shapes_saved := False;      // need a resave.
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//____________________________________________________________________________________________

procedure Tbgnd_form.break_all_rectangles_menu_entryClick(Sender: TObject);

var
  n: integer;
  rectangles_exist: boolean;

begin
  with bgnd_form.bgnd_shapes_listbox.Items do begin

    if Count < 1 then
      EXIT;

    rectangles_exist := False; // init

    for n := 0 to (Count - 1) do begin
      if Tbgshape(Objects[n]).bgnd_shape.shape_code = 1 then
        rectangles_exist := True;
    end;//for

    if rectangles_exist = False then begin
      alert(6, '      no  rectangle  shapes',
        '||||There are no rectangle shapes currently defined.'
        + '|||Nothing to convert.',
        '', '', '', '', 'cancel    ', '', 0);
      EXIT;
    end;

    if alert(7, '      convert  all  rectangles ?',
      '|You are about to convert all rectangle shapes, each to 4 separate lines.'
      +
      '||This will make them easier to modify, and will permit rotation of the rectangle outlines, but will remove any hatched or solid filling.' + '||The lines will be drawn solid or dotted according to the current setting.' + '||Are you sure you want to convert all rectangles ?', '', '', '', '', 'no  -  cancel    ', 'yes  -  convert  all  rectangles    ', 0) = 5 then
      EXIT;

    n := 0;
    while n < Count do begin
      // (Count increases by 3 for every rectangle converted.)
      if Tbgshape(Objects[n]).bgnd_shape.shape_code = 1  // code 1 = rectangle.
      then
        rectangle_to_lines(n)                   // don't increment n, this entry gets deleted.
      else
        Inc(n);
    end;//while

  end;//with
end;
//_______________________________________________________________________________________

procedure Tbgnd_form.corner1_buttonClick(Sender: TObject);

begin
  pad_form.shift_corner1_menu_entry.Click;
end;
//________________________________________________________________________________________

procedure Tbgnd_form.corner2_buttonClick(Sender: TObject);

begin
  pad_form.shift_corner2_menu_entry.Click;
end;
//________________________________________________________________________________________

procedure Tbgnd_form.mouse_shift_one_buttonClick(Sender: TObject);

begin
  pad_form.shift_shape_menu_entry.Click;
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.mouse_scale_one_buttonClick(Sender: TObject);

begin
  pad_form.scale_shape_menu_entry.Click;
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.mouse_shift_all_buttonClick(Sender: TObject);

begin
  pad_form.shift_all_shapes_menu_entry.Click;
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.mouse_scale_all_buttonClick(Sender: TObject);

begin
  pad_form.scale_all_shapes_menu_entry.Click;
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.mouse_rotate_all_buttonClick(Sender: TObject);

begin
  pad_form.rotate_all_shapes_menu_entry.Click;
end;
//___________________________________________________________________________________________

procedure Tbgnd_form.mirror_x_buttonClick(Sender: TObject);

begin
  scale_all_shapes(-1, 1);
  shapes_current_state;
  shapes_saved := False;      // need a resave.
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//____________________________________________________________________________________________

procedure Tbgnd_form.mirror_y_buttonClick(Sender: TObject);

begin
  scale_all_shapes(1, -1);
  shapes_current_state;
  shapes_saved := False;      // need a resave.
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//___________________________________________________________________________________________

procedure Tbgnd_form.FormResize(Sender: TObject);

begin
  bgs_file_label.Width := ClientWidth;
end;
//____________________________________________________________________________________________

procedure Tbgnd_form.rotate_all_by_buttonClick(Sender: TObject);

const
  help_rotshap_str: string = '     rotate  all  shapes' +
    '||Enter an angle in degrees for the amount by which to rotate all background shapes.'
    +
    '||Angles are measured positive in the anti-clockwise direction, and negative in the clockwise direction.'
    + '||This operation can be undone by rotating in the opposite direction by the same amount.'
    +
    '||The centre of rotation can be the grid origin, or the current position of the notch, or the current position of the spacing-ring.' + ' These options are set on the `0modify all`1 tab > `0rotate all around:`1 panel.' + '||The current setting is to rotate all background shapes around the';

var
  n, i: integer;
  od: Toutdim;

  rotshap_k: double;

  cen_str, rot_cen_str: string;

begin
  if bgnd_shapes_listbox.Items.Count < 1 then
    EXIT;     // no shapes to rotate.

  rotshap_k := 0;

  case rotate_centre_code of
    1:
      rot_cen_str := '||                              <b>notch</b>.';
    2:
      rot_cen_str := '||                              <b>spacing-ring</b>.';
    else
      rot_cen_str := '||                              <b>grid origin</b>.';
  end;//case

  case rotate_centre_code of
    1:
      cen_str := 'notch';
    2:
      cen_str := 'spacing-ring';
    else
      cen_str := 'grid origin';
  end;//case

  n := putdim(help_rotshap_str + rot_cen_str, 3, 'rotate  all  shapes  around  ' +
    cen_str + '  by', rotshap_k * 180 / Pi, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.

  if n <> 0 then
    EXIT;
  if getdims('rotate  all  background  shapes  around  ' + cen_str, '', bgnd_form, n, od) = True then
  begin
    rotshap_k := od[0] * Pi / 180;
    normalize_angle(rotshap_k);
  end
  else
    EXIT;

  rotate_all_shapes(False, True, rotshap_k);
  // do the calcs    False = not sync from group rotate   True = include pictures

  shapes_saved := False;      // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_______________________________________________________________________________________

procedure Tbgnd_form.dxf_set_buttonClick(Sender: TObject);

const
  shift_str: string = '    Shift  DXF  on  Import' +
    '||Enter a dimension in mm by which data in imported DXF files will be shifted.'
    + '||Positive dimensions on X will move the DXF drawing towards the right on the screen.'
    + '||Positive dimensions on Y will move the DXF drawing upwards on the screen.'
    + '||Any import scaling will take place before the shift is done.';

  scale_str: string = '    Scale  DXF  on  Import' +
    '||Enter a % value by which data in imported DXF files will be scaled.' +
    '||Figures greater than 100% will cause the DXF drawing to be increased in size.'
    + '||Figures less than 100%  will cause the DXF drawing to be reduced in size.'
    + '||Negative figures will cause the DXF drawing to be rotated 180 degrees about the drawing origin.'
    + ' This can be combined with X and Y shifts to import the DXF drawing facing in the opposite direction.';

var
  i: integer;
  od: Toutdim;

begin
  putdim(shift_str, 1, 'shift  imported  DXF  X  dimensions  by', input_shift_x,
    False, True, False, False);
  // neg ok, no preset, allow zero, don't terminate on zero.
  putdim(shift_str, 1, 'shift  imported  DXF  Y  dimensions  by', input_shift_y,
    False, True, False, False);
  // ditto.
  i := putdim(scale_str, 4, 'scale  all  imported  DXF  dimensions  by', input_scale_factor *
    100, False, True, True, False);  // neg ok, no preset, no zero, don't etc.
  if i <> 2 then
    EXIT;
  if getdims('modify  DXF  on  import', '', bgnd_form, i, od) = True then begin
    input_shift_x := od[0];
    input_shift_y := od[1];
    input_scale_factor := od[2] / 100;

    shift_x_label.Caption := 'shift  X  by :  ' + round_str(input_shift_x, 2) + '  mm';
    shift_y_label.Caption := 'shift  Y  by :  ' + round_str(input_shift_y, 2) + '  mm';
    scale_by_label.Caption := 'scale  by :  ' + round_str(input_scale_factor * 100, 2) + ' %';

  end
  else
    EXIT;
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.import_dxf_buttonClick(Sender: TObject);

begin
  import_dxf;
  shapes_saved := False;      // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//__________________________________________________________________________________________

function reload_picture_image(scanning, pasting, meta, dropped, adjust_aspect: boolean): boolean;

var
  n: integer;
  img_file_name_str: string;
  dummy1, dummy2: integer;

begin
  Result := False; // init

  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;   // no shapes.

    n := ItemIndex;

    if n < 0 then
      n := 0;

    if Tbgshape(Items.Objects[n]).bgnd_shape.shape_code <> -1 then
      EXIT;                  // not a picture shape.

    img_file_name_str := get_user_image(dropped, meta, scanning, pasting, True, n, dummy1, dummy2);
    // get image file and file name for list.
    if img_file_name_str = empty_picture_str then
      EXIT
    else
      Result := True;

    Items.Strings[n] := img_file_name_str;                                   // name in list.
    Tbgshape(Items.Objects[n]).bgnd_shape.shape_name := img_file_name_str;   // name in file.

    if adjust_aspect = True then begin
      with Tbgshape(Items.Objects[n]) do begin
        with bgimage.image_shape do begin
          with bgnd_shape do
            p2.y := p1.y + (p2.x - p1.x) * image_height / image_width;   // adjust height to aspect ratio of loaded image
        end;//with
      end;//with
    end;

    copy_draw_to_pad; // remove any previous highlighting

    ItemIndex := n;
    draw_bg_shapes(pad_form.Canvas, ItemIndex, clRed);
    // show new bitmap and highlight in red, directly on the pad.
  end;//with

  shapes_saved := False;      // need a resave.
  shapes_current_state;
end;
//______________________________________________________________________________

procedure Tbgnd_form.reload_picture_buttonClick(Sender: TObject);

begin
  reload_picture_image(False, False, False, False, False);
  // scanning,pasting,meta,dropped,adjust_aspect
end;
//______________________________________________________________________________

procedure Tbgnd_form.paste_buttonClick(Sender: TObject);

begin
  reload_picture_image(False, True, False, False, False);
  // scanning,pasting,meta,dropped,adjust_aspect
end;
//______________________________________________________________________________

procedure Tbgnd_form.scan_buttonClick(Sender: TObject);

begin
  reload_picture_image(True, False, False, False, False);
  // scanning,pasting,meta,dropped,adjust_aspect
end;
//______________________________________________________________________________


//  DXF import functions... 14-3-01

//___________________________________________________________________

function add_dxf_shape(dxf_shape: TBgnd_shape): boolean;

  // return True if added, False if reached limit.
var
  n: integer;

begin
  Result := False;   // init.

  dxf_shape.hide_bits := 0;  // 214a  normal visibility
  dxf_shape.option_bits := 0;     // byte;

  with bgnd_form.bgnd_shapes_listbox.Items do begin

    if Count >= count_limit then begin
      alert(2, '    dxf  import  -  too  many  shapes',
        '||You have now reached the limit of  ' + IntToStr(
        count_limit) + '  background shapes. It is not possible to continue importing shapes from:'
        + '|| ' + his_dxf_file_name +
        '||Try breaking the file into smaller sections and importing each separately.',
        '', '', '', '',
        'cancel  importing  of  remaining  shapes        ', '', 0);
      EXIT;
    end;

    n := AddObject(dxf_shape.shape_name, Tbgshape.Create);
    // create and add a new line in the shapes list.
    Tbgshape(Objects[n]).bgnd_shape := dxf_shape;          // put data in list.
    Tbgshape(Objects[n]).bgimage := nil;                   // not bitmap image.

  end;//with

  Result := True;
end;
//______________________________________________________________________________

function convert_to_comma(in_str: string): string;

  // 0.94.a convert DXF format to comma if running on system using comma decimal point

begin
  Result := in_str;  // init

  if comma_dp = False then
    EXIT;  // using dot decimal point

  // convert to a comma for StrTofloat function

  Result := StringReplace(in_str, '.', ',', []);
  // 0.94.a  StringReplace(str,'old','new',[rfReplaceAll, rfIgnoreCase]);
end;
//______________________________________________________________________________

function add_a_line_shape(x1_str, y1_str, x2_str, y2_str: string): boolean;

  // return True unless not added beacuse reached count limit.
var
  x1, y1, x2, y2: double;
  new_shape: Tbgnd_shape;

  x1_s, y1_s, x2_s, y2_s: string;   // 0.94.a

begin
  Result := True; // init

  x1_s := convert_to_comma(x1_str);    // 0.94.a
  x2_s := convert_to_comma(x2_str);    // 0.94.a
  y1_s := convert_to_comma(y1_str);    // 0.94.a
  y2_s := convert_to_comma(y2_str);    // 0.94.a

  try
    x1 := StrToFloat(x1_s) * input_factor;
    y1 := StrToFloat(y1_s) * input_factor;
    x2 := StrToFloat(x2_s) * input_factor;
    y2 := StrToFloat(y2_s) * input_factor;
  except
    x1 := 0;
    y1 := 0;
    x2 := 0;
    y2 := 0;
  end;//try

  with new_shape do begin                    // put line shape in list...

    p1.x := x1 * input_scale_factor + input_shift_x;
    p1.y := y1 * input_scale_factor + input_shift_y;
    p2.x := x2 * input_scale_factor + input_shift_x;
    p2.y := y2 * input_scale_factor + input_shift_y;

    if bgnd_form.dxf_limits_checkbox.Checked = True     // only within limits?
    then begin
      if (p1.x > xmax) or (p2.x > xmax) or (p1.y > ymax) or (p2.y > ymax) or
        (p1.x < xmin) or (p2.x < xmin) or (p1.y < ymin) or (p2.y < ymin) then
        EXIT;
    end;

    Inc(shape_count);

    shape_name := ExtractFileName(bgnd_form.dxf_file_dialog.FileName) + ' L# ' + IntToStr(shape_count);
    shape_code := 0;      // 0=line, 1=rectangle, 2=circle, 3=text.
    shape_style := 0;     // 0=transparent, 1=blank/solid, 2=cross-hatched;

  end;//with

  Result := add_dxf_shape(new_shape);
end;
//_________________________________________________________________________________________

function add_a_circle_shape(x1_str, y1_str, rad_str: string): boolean;

var
  x1, y1, rad: double;
  new_shape: Tbgnd_shape;

  x1_s, y1_s, rad_s: string;  // 0.94.a

begin
  Result := True;     // init.

  x1_s := convert_to_comma(x1_str);    // 0.94.a
  y1_s := convert_to_comma(y1_str);    // 0.94.a
  rad_s := convert_to_comma(rad_str);    // 0.94.a


  try
    x1 := StrToFloat(x1_s) * input_factor;
    y1 := StrToFloat(y1_s) * input_factor;
    rad := StrToFloat(rad_s) * input_factor;
  except
    x1 := 0;
    y1 := 0;
    rad := 1;
  end;//try

  with new_shape do begin                    // put circle shape in list...

    p1.x := x1 - rad;
    p1.y := y1 - rad;
    p2.x := x1 + rad;
    p2.y := y1 + rad;

    p1.x := p1.x * input_scale_factor + input_shift_x;
    p1.y := p1.y * input_scale_factor + input_shift_y;
    p2.x := p2.x * input_scale_factor + input_shift_x;
    p2.y := p2.y * input_scale_factor + input_shift_y;


    if bgnd_form.dxf_limits_checkbox.Checked = True     // only within limits?
    then begin
      if (p1.x > xmax) or (p2.x > xmax) or (p1.y > ymax) or (p2.y > ymax) or
        (p1.x < xmin) or (p2.x < xmin) or (p1.y < ymin) or (p2.y < ymin) then
        EXIT;
    end;

    Inc(shape_count);

    shape_name := ExtractFileName(bgnd_form.dxf_file_dialog.FileName) + ' C# ' + IntToStr(shape_count);
    shape_code := 2;      // 0=line, 1=rectangle, 2=circle, 3=text.
    shape_style := 1;     // 0=transparent, 1=blank/solid, 2=cross-hatched;

  end;//with

  Result := add_dxf_shape(new_shape);
end;
//_________________________________________________________________________________________

function add_an_arc_shape(x1_str, y1_str, rad_str, k1_str, k2_str: string): boolean;

var
  x1, y1, rad, k1, k2, k, inc_k: double;
  new_shape: Tbgnd_shape;

  x1_s, y1_s, rad_s, k1_s, k2_s: string;  // 0.94.a

begin
  Result := True;    // init.

  x1_s := convert_to_comma(x1_str);    // 0.94.a
  y1_s := convert_to_comma(y1_str);    // 0.94.a
  rad_s := convert_to_comma(rad_str);  // 0.94.a
  k1_s := convert_to_comma(k1_str);    // 0.94.a
  k2_s := convert_to_comma(k2_str);    // 0.94.a


  try
    x1 := StrToFloat(x1_s) * input_factor;
    y1 := StrToFloat(y1_s) * input_factor;
    rad := StrToFloat(rad_s) * input_factor;
    k1 := StrToFloat(k1_s) * Pi / 180;  // convert to radians.
    k2 := StrToFloat(k2_s) * Pi / 180;

    //-------------
    if k2 = k1 then
      EXIT;

    k := k1;                     // init for steppping along arc.

    if k2 < k1 then
      inc_k := 0 - 0.004        // 0.004 rads = 1/4 degree steps (arbitrary).
    else
      inc_k := 0.004;

    with new_shape do begin

      p1.x := x1 + rad * COS(k1);
      p1.y := y1 + rad * SIN(k1);

      p1.x := p1.x * input_scale_factor + input_shift_x;
      p1.y := p1.y * input_scale_factor + input_shift_y;

      repeat

        k := k + inc_k;

        p2.x := x1 + rad * COS(k);
        p2.y := y1 + rad * SIN(k);

        p2.x := p2.x * input_scale_factor + input_shift_x;
        p2.y := p2.y * input_scale_factor + input_shift_y;

        if bgnd_form.dxf_limits_checkbox.Checked = True     // only within limits?
        then begin
          if (p1.x > xmax) or (p2.x > xmax) or (p1.y > ymax) or
            (p2.y > ymax) or (p1.x < xmin) or (p2.x < xmin) or (p1.y < ymin) or
            (p2.y < ymin) then begin
            p1 := p2;      // next line step along arc.
            CONTINUE;    // ignore this one.
          end;
        end;

        Inc(shape_count);
        // put arc line step shape in list...

        shape_name := ExtractFileName(bgnd_form.dxf_file_dialog.FileName) + ' A# ' +
          IntToStr(shape_count);
        shape_code := 0;      // 0=line, 1=rectangle, 2=circle, 3=text.
        shape_style := 0;     // 0=transparent, 1=blank/solid, 2=cross-hatched;

        Result := add_dxf_shape(new_shape);

        if Result = False then
          EXIT;       // reached limit

        p1 := p2;           // next line step along arc...

      until ((k2 >= k1) and (k >= k2)) or ((k2 <= k1) and (k <= k2));

    end;//with

  except
    EXIT;
  end;//try
end;
//_________________________________________________________________________________________

function add_a_target_shape(x1_str, y1_str: string): boolean;

var
  x1, y1: double;
  new_shape: Tbgnd_shape;

  x1_s, y1_s: string;  // 0.94.a

begin
  Result := True;     // init.

  x1_s := convert_to_comma(x1_str);    // 0.94.a
  y1_s := convert_to_comma(y1_str);    // 0.94.a

  try
    x1 := StrToFloat(x1_s) * input_factor;
    y1 := StrToFloat(y1_s) * input_factor;
  except
    x1 := 0;
    y1 := 0;
  end;//try


  with new_shape do begin                    // put target mark shape in list...
    p1.x := x1;
    p1.y := y1;
    p2.x := 10;           // target arm length 10mm.
    p2.y := 0;

    p1.x := p1.x * input_scale_factor + input_shift_x;
    p1.y := p1.y * input_scale_factor + input_shift_y;
    p2.x := p2.x * input_scale_factor + input_shift_x;

    if bgnd_form.dxf_limits_checkbox.Checked = True     // only within limits?
    then begin
      if (p1.x > xmax) or (p1.y > ymax) or (p1.x < xmin) or
        (p1.y < ymin) then
        EXIT;
    end;

    Inc(shape_count);

    shape_name := ExtractFileName(bgnd_form.dxf_file_dialog.FileName) + ' T# ' + IntToStr(shape_count);
    shape_code := 4;      // 0=line, 1=rectangle, 2=circle, 3=text, 4=target mark.
    shape_style := 0;     // 0=transparent, 1=blank/solid, 2=cross-hatched;

  end;//with

  Result := add_dxf_shape(new_shape);
end;
//_________________________________________________________________________________________

function add_a_label_shape(x1_str, y1_str, label_str: string): boolean;

var
  x1, y1: double;
  new_shape: Tbgnd_shape;

  x1_s, y1_s: string;  // 0.94.a

begin
  Result := True;

  x1_s := convert_to_comma(x1_str);    // 0.94.a
  y1_s := convert_to_comma(y1_str);    // 0.94.a

  try
    x1 := StrToFloat(x1_s) * input_factor;
    y1 := StrToFloat(y1_s) * input_factor;
  except
    x1 := 0;
    y1 := 0;
  end;//try

  with new_shape do begin                    // put label shape in list...
    p1.x := x1;
    p1.y := y1;
    p2.x := 0;            // don't leave invalid data in the file..
    p2.y := 0;

    p1.x := p1.x * input_scale_factor + input_shift_x;
    p1.y := p1.y * input_scale_factor + input_shift_y;

    if bgnd_form.dxf_limits_checkbox.Checked = True     // only within limits?
    then begin
      if (p1.x > xmax) or (p1.y > ymax) or (p1.x < xmin) or
        (p1.y < ymin) then
        EXIT;
    end;

    Inc(shape_count);

    shape_name := label_str;
    shape_code := 3;      // 0=line, 1=rectangle, 2=circle, 3=text.
    shape_style := 0;     // 0=transparent, 1=blank/solid, 2=cross-hatched;

  end;//with

  Result := add_dxf_shape(new_shape);
end;
//_________________________________________________________________________________________

function read_2_input_lines: boolean;

  // return either 2 upper-case string values and result=False (not end of file)
  // or result=True (end of file)
var
  s1, s2: string;

begin
  Result := False;

  if EOF(dxf_file) = True then begin
    code_str := '';
    value_str := '';
    Result := True;
    EXIT;
  end;
  ReadLn(dxf_file, s1);

  if EOF(dxf_file) = True then begin
    code_str := '';
    value_str := '';
    Result := True;
    EXIT;
  end;
  ReadLn(dxf_file, s2);

  code_str := UpperCase(Trim(s1));
  value_str := UpperCase(Trim(s2));
end;
//____________________________________________________________________________________________

procedure import_dxf;

label
  100, 123, 200, 456;

var
  s: string;
  x1_str, x2_str, y1_str, y2_str, rad_str, startangle_str, endangle_str: string;
  x3_str, x4_str, y3_str, y4_str: string;
  vertices_count: integer;
  test_str: string;         // 0.94.a

begin
  with bgnd_form.dxf_file_dialog do begin                     // set up the open dialog.

    if bgnd_form.dxf_mm_radio.Checked = True then begin
      Title := '    import  DXF  file  ( in  mm )';
      input_factor := 1.0;
    end
    else begin
      Title := '    import  DXF  file  ( in  inches )';
      input_factor := 25.4;
    end;

    if his_dxf_file_name <> '' then
      InitialDir := ExtractFilePath(his_dxf_file_name)   // use his previous folder.
    else
      InitialDir := Config.GetDir(cudiDxfs);

    FileName := '*.dxf';
    Filter := 'DXF files (*.dxf)|*.dxf';
    DefaultExt := 'dxf';

    if Execute = False then
      EXIT;
    his_dxf_file_name := FileName;              // so can use same folder next time.

  end;//with

  Application.ProcessMessages;
  Screen.Cursor := crHourglass;
  shape_count := 0;                 // init...

  test_str := FormatFloat('0.0', 1.2);   // 0.94.a
  comma_dp := (Pos(',', test_str) > 0);
  // True=running on a system with comma decimal point separator


  xmin := zoom_offsetx;
  ymin := zoom_offsety;

  xmax := xmin + screenx;
  ymax := ymin + screeny;

  try
    try
      AssignFile(dxf_file, bgnd_form.dxf_file_dialog.Filename);
      Reset(dxf_file);                                   // open dxf file.

      repeat                               // first get synchronised for 2-line groups.
        if EOF(dxf_file) = True then
          EXIT;
        ReadLn(dxf_file, s);                // (we are ignoring headers and sections.)
      until Trim(s) = '0';
      if EOF(dxf_file) = True then
        EXIT;
      ReadLn(dxf_file, s);


      if EOF(dxf_file) = True then
        EXIT;

      repeat
        if read_2_input_lines = True then
          EXIT;             // end of file
      until (code_str = '2') and (value_str = 'ENTITIES');    // find ENTITIES section

      100:

        if EOF(dxf_file) = True then
          EXIT;

      repeat                                    // find next 0 group.
        if read_2_input_lines = True then
          EXIT;   // end of file
      until code_str = '0';

      123:

        if value_str = 'LINE' then begin
          repeat
            if read_2_input_lines = True then
              EXIT;   // end of file
          until (code_str = '10') or (code_str = '0');
          // find x1 value (or not present - next entity found)
          if code_str = '0' then
            goto 123;
          x1_str := value_str;

          repeat
            if read_2_input_lines = True then
              EXIT;   // end of file
          until (code_str = '20') or (code_str = '0');
          // find y1 value (or not present - next entity found)
          if code_str = '0' then
            goto 123;
          y1_str := value_str;

          repeat
            if read_2_input_lines = True then
              EXIT;   // end of file
          until (code_str = '11') or (code_str = '0');
          // find x2 value (or not present - next entity found)
          if code_str = '0' then
            goto 123;
          x2_str := value_str;

          repeat
            if read_2_input_lines = True then
              EXIT;   // end of file
          until (code_str = '21') or (code_str = '0');
          // find y2 value (or not present - next entity found)
          if code_str = '0' then
            goto 123;
          y2_str := value_str;

          if add_a_line_shape(x1_str, y1_str, x2_str, y2_str) = False then
            EXIT;  // reached list limit.
        end;//line

      if value_str = 'ARC' then begin
        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '10') or (code_str = '0');
        // find x1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '20') or (code_str = '0');
        // find y1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '40') or (code_str = '0');
        // find rad value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        rad_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '50') or (code_str = '0');
        // find start angle value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        startangle_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '51') or (code_str = '0');
        // find end angle value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        endangle_str := value_str;

        if add_an_arc_shape(x1_str, y1_str, rad_str, startangle_str, endangle_str) = False then
          EXIT;  // reached list limit.
      end;//arc

      if (value_str = 'TRACE') or (value_str = 'SOLID') then begin
        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '10') or (code_str = '0');
        // find x1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '20') or (code_str = '0');
        // find y1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '11') or (code_str = '0');
        // find x2 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x2_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '21') or (code_str = '0');
        // find y2 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y2_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '12') or (code_str = '0');
        // find x3 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x3_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '22') or (code_str = '0');
        // find y3 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y3_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '13') or (code_str = '0');
        // find x4 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x4_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '23') or (code_str = '0');
        // find y4 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y4_str := value_str;

        if add_a_line_shape(x1_str, y1_str, x2_str, y2_str) = False then
          EXIT;   // add 4 lines marking the outline of a trace or solid.
        if add_a_line_shape(x2_str, y2_str, x4_str, y4_str) = False then
          EXIT;   // (this sequence arrived at by trial and error).
        if add_a_line_shape(x4_str, y4_str, x3_str, y3_str) = False then
          EXIT;
        if add_a_line_shape(x3_str, y3_str, x1_str, y1_str) = False then
          EXIT;

      end;//trace or solid

      if value_str = 'POLYLINE' then begin
        vertices_count := 0;        // init

        200:

          if EOF(dxf_file) = True then
            EXIT;

        repeat                                    // find next 0 group.
          if read_2_input_lines = True then
            EXIT;   // end of file
        until code_str = '0';

        456:

          if value_str = 'VERTEX' then begin
            repeat
              if read_2_input_lines = True then
                EXIT;   // end of file
            until (code_str = '10') or (code_str = '0');
            // find x1 value (or not present - next entity found)
            if value_str = 'SEQEND' then
              goto 100;      // end of polyline, return to outer loop.
            if code_str = '0' then
              goto 456;            // polyline not finished, ignore this and look for next vertex.

            if vertices_count > 0 then
              x1_str := x2_str;  // previous vertex.
            x2_str := value_str;                        // new vertex.

            repeat
              if read_2_input_lines = True then
                EXIT;   // end of file
            until (code_str = '20') or (code_str = '0');
            // find y1 value (or not present - next entity found)
            if value_str = 'SEQEND' then
              goto 100;      // end of polyline, return to outer loop.
            if code_str = '0' then
              goto 456;            // polyline not finished, ignore this and look for next vertex.

            if vertices_count > 0 then
              y1_str := y2_str;  // previous vertex.
            y2_str := value_str;                        // new vertex.

            if vertices_count > 0 then
              if add_a_line_shape(x1_str, y1_str, x2_str, y2_str) = False then
                EXIT;  // reached list limit.

            Inc(vertices_count);
          end;//vertex

        if (value_str = 'LINE') or (value_str = 'POLYLINE') or
          (value_str = 'CIRCLE') or (value_str = 'TEXT') or (value_str = 'POINT') or
          (value_str = 'TRACE') or (value_str = 'SOLID') then
          goto 123;

        goto 200;  // polyline loop

      end;//polyline

      if value_str = 'CIRCLE' then begin
        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '10') or (code_str = '0');
        // find x1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '20') or (code_str = '0');
        // find y1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '40') or (code_str = '0');
        // find rad value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        rad_str := value_str;

        if add_a_circle_shape(x1_str, y1_str, rad_str) = False then
          EXIT;  // reached list limit.
      end;//circle

      if value_str = 'TEXT' then begin
        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '10') or (code_str = '0');
        // find x1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '20') or (code_str = '0');
        // find y1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '1') or (code_str = '0');
        // find text string (or not present - next entity found)
        if code_str = '0' then
          goto 123;

        if add_a_label_shape(x1_str, y1_str, value_str) = False then
          EXIT;  // reached list limit.
      end;//text

      if value_str = 'POINT' then begin
        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '10') or (code_str = '0');
        // find x1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        x1_str := value_str;

        repeat
          if read_2_input_lines = True then
            EXIT;   // end of file
        until (code_str = '20') or (code_str = '0');
        // find y1 value (or not present - next entity found)
        if code_str = '0' then
          goto 123;
        y1_str := value_str;

        if add_a_target_shape(x1_str, y1_str) = False then
          EXIT;  // reached list limit.
      end;//point

      goto 100;  // main loop.

    finally
      CloseFile(dxf_file);
      Screen.Cursor := crDefault;
    end;//try
  except
    CloseFile(dxf_file);
    Screen.Cursor := crDefault;
  end;//try
end;

//______________________________________________________________________________

procedure Tbgnd_form.trans_checkboxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

const
  transpic_str: string = '      Transparent  Picture  Shape' +
    '||green_panel_begin tree.gif  The following notes do not apply to EMF metafile picture shapes, which can be freely set as transparent if desired.green_panel_end' + '|The transparent option for the picture shapes is useful when aligning scanned images but will cause slower re-draws when zooming and panning on the trackpad.' + '||It is therefore recommended that this option is selected for only one or two picture shapes at any one time, and cancelled once the images have been aligned and saved.' + '||Click the PICTURE SHAPE IMAGE : > SAVE... button to save each bitmap image which has been twisted. Then click the SAVE ALL AS... button to save the new positions of the picture shape outlines and the names of the new bitmap image files.' + '||For comfortable working with transparent picture shapes you may want to change the background colour of the trackpad. Click the `0TRACKPAD > TRACKPAD COLOURS > TRACKPAD COLOUR...`1 menu item.' + '||For monochrome images (black and white drawings), any trackpad colour other than very dark colours or black can be used.' + '||For grey-scale images (black and white photographs), a light pastel colour or white is preferable.' + '||For colour images, the only suitable trackpad colour is white.' + '||Handy Hints:' + '|A similar effect with faster re-draws can be obtained by selecting instead the GENERAL OPTIONS tab > GRID IN FRONT OF SHAPES tickbox.' + '||The transparent option has no effect on printed track templates. Click the PICTURE SHAPES tab on the PRINT window for more information about printing picture shapes.' + '||The transparent effect applies only to the way the image is displayed in Templot, the original bitmap image file is not modified in any way.' + '||Please bear in mind that Templot0 is not fully-fledged graphics imaging software - this "bare-bones" function is intended primarily to permit the alignment of scanned track plans.' + ' Some degradation of the image quality is likely.';

var
  i, n: integer;

begin
  with bgnd_shapes_listbox do begin
    if Items.Count < 1 then
      EXIT;        // ??  check box should be disabled.
    n := ItemIndex;
    if (n < 0) or (n > (Items.Count - 1)) then
      EXIT;  // ??

    if Tbgshape(Items.Objects[n]).bgnd_shape.shape_code <> -1 then
      EXIT; // ?? not  picture shape.
  end;//with

  if trans_checkbox.Checked = True then begin

    if trans_msg_pref = False then begin
      alert_box.preferences_checkbox.Checked := False;       //%%%%
      alert_box.preferences_checkbox.Show;

      repeat

        i := alert(3, '    transparent  picture  shape',
          'You have selected the transparent option for this picture shape.'
          +
          '||This option is useful when aligning scanned images but will cause slower re-draws when zooming and panning on the trackpad.'
          +
          '||It may also restrict the maximum zoom-in which is possible for this image. If the image disappears when zooming in, try cancelling the transparent option.' + '||To see the image clearly you may need to change the trackpad colours.' + '||tree.gif The above does not apply to EMF metafile picture shapes, which can be freely set as transparent if desired.', '', '', '', 'important  information', 'cancel  transparent  picture  shape', 'continue', 4);

        if i = 4 then
          alert_help(0, transpic_str, '');

      until i <> 4;

      trans_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
      alert_box.preferences_checkbox.Hide;


      if i = 5 then
        trans_checkbox.Checked := False;  // and continue below.

    end;
  end;

  Tbgshape(bgnd_shapes_listbox.Items.Objects[n]).bgnd_shape.show_transparent :=
    trans_checkbox.Checked;   // 0.93.a now in file

  do_rollback := False;
  redraw(True);
end;
//___________________________________________________________________________________________

procedure twist_picture(i: integer; krot: double; do_container, all, sync: boolean);
// rotate bitmap supplied krot clockwise.

// krot limits +/- 90degs

// 219a  if do_container=True re-size and re-locate picture shape accordingly.

var
  inrow, incol, outrow, outcol: integer;

  pixel_colour: TColor;
  saved_screen_cursor: TCursor;

  diagonal: double;
  inwidth, inheight, inaspectk: double;
  outwidth, outheight: double;
  pin, pout, shape_cen: Tpex;

  old_shapewidth, old_shapeheight: double;
  new_shapewidth, new_shapeheight: double;

  meta_str: string;
  waitMessage: IAutoWaitMessage;

begin

  if krot < (0 - Pi / 2) then
    krot := 0 - Pi / 2;      // 90 deg limits
  if krot > (Pi / 2) then
    krot := Pi / 2;

  if (i < 0) or (i > bgnd_form.bgnd_shapes_listbox.Items.Count - 1) then
    EXIT;

  with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]) do begin

    if bgnd_shape.shape_code <> -1 then
      EXIT;      // not a picture shape.

    if (bgnd_shape.shape_name = empty_picture_str) or (bgimage = nil)   // no existing bitmap.
    then
      EXIT;

    { OT-FIRST
    if bgnd_shape.picture_is_metafile=True
       then begin
              if  (bgimage.image_shape.image_metafile.Width>4800)    // arbitrary
              and (bgimage.image_shape.image_metafile.Height>3600)   // arbitrary
                  then meta_str:='||This picture shape is a large image. Conversion and twisting may be slow.'
                  else meta_str:='';

              if alert(4,'    twist  metafile ?',
                   'The selected picture shape||'
                   +bgnd_shape.shape_name
                  +'||contains a metafile image (EMF file). Metafile (vector) images can be zoomed without becoming fuzzy, pixelated or blocky.'
                  +'||Twisting this picture shape will convert it to a conventional raster image (as in a scanned image).'
                  +'||Raster (bitmap) images can become fuzzy, pixelated or blocky when zoomed.'
                  +meta_str
                  +'||Do you want to proceed with the conversion and twist?',
                   '','','','','cancel  twist','continue  -  twist  picture  shape',0)=5
                 then EXIT;

              try
                with bgimage.image_shape do begin

                  image_bitmap.Width:=image_metafile.Width;
                  image_bitmap.Height:=image_metafile.Height;

                  with image_bitmap.Canvas do begin     // first blank the picture area...
                    Brush.Color:=clWhite;
                    Brush.Style:=bsSolid;
                    FillRect(Rect(0,0,image_bitmap.Width-1,image_bitmap.Height-1));

                    Draw(0,0,image_metafile);      // draw the metafile on it
                  end;//with

                end;//with
              except
                ShowMessage('error - unable to convert matafile to raster');
                EXIT;
              end;//try

              bgnd_shape.picture_is_metafile:=False;
            end;
    }

    saved_screen_cursor := Screen.Cursor;
    Screen.Cursor := crHourglass;

    try
      with bgimage.image_shape do begin

        inwidth := image_bitmap.Width;
        inheight := image_bitmap.Height;

        if sync = False then begin
          waitMessage := TWaitForm.ShowWaitMessageWithProgress('twisting  image  ' +
            IntToStr(i) + ' ...', 0, Round(inheight), 0, 1);
        end
        else
          action_form.action_label.Caption := 'twisting  image  ' + IntToStr(i) + ' ...';

        Application.ProcessMessages;  // allow some repaints before we start.

        inaspectk := ARCTAN(inheight / inwidth);
        diagonal := SQRT(SQR(inwidth) + SQR(inheight));

        outwidth := ABS(diagonal * COS(inaspectk - ABS(krot)));
        // size change is same in both directions of krot.
        outheight := ABS(diagonal * SIN(inaspectk + ABS(krot)));

        try
          rotated_bitmap.Assign(image_bitmap);        // set same properties, bits/pixel, etc.
          rotated_bitmap.Width := Round(outwidth);
          // ensure output is large enough to contain it.
          rotated_bitmap.Height := Round(outheight);

          with rotated_bitmap do begin
            with Canvas do begin
              Brush.Color := clWhite;          //  this clears the new bitmap for starters.
              Brush.Style := bsSolid;
              FillRect(Rect(0, 0, Width, Height));

              Pen.Color := clBlack;
              Pen.Mode := pmCopy;
              Pen.Style := psSolid;
              Pen.Width := 1;

              Font.Assign(pad_form.Font);

              TextOut(0, 0, ' ');

            end;//with
          end;//with

          for inrow := 0 to Trunc(inheight - 1) do begin
            for incol := 0 to Trunc(inwidth - 1) do begin
              pixel_colour := image_bitmap.Canvas.Pixels[incol, inrow];

              pin.x := incol;
              pin.y := inrow;
              dotransform(krot, 0, 0, pin, pout);   // rotate around bitmap origin.

              if krot > 0 then
                pout.x := pout.x + inheight * SIN(krot);   // shift origin across.
              if krot < 0 then
                pout.y := pout.y + inwidth * SIN(0 - krot);  // shift origin down.

              with rotated_bitmap.Canvas do begin

                outcol := Round(pout.x);
                outrow := Round(pout.y);
                Pen.Color := pixel_colour;
                MoveTo(outcol, outrow);
                LineTo(outcol + 1, outrow);        // most of this will get overwritten later.
                LineTo(outcol + 1, outrow + 1);
                LineTo(outcol, outrow + 1);

              end;//with
            end;//for

            if sync = False then begin
              waitMessage.StepIt;
              Application.ProcessMessages;
            end;

          end;//for

          rotated_bitmap.SaveToFile(Config.GetFilePath(csfi_85a_temp));
          // !!! this way corrects a bug with the display of monochrome bitmaps.

          image_bitmap.LoadFromFile(Config.GetFilePath(csfi_85a_temp));

          DeleteFile(Config.GetFilePath(csfi_85a_temp));

          rotated_bitmap.Free;             // release memory.
          rotated_bitmap := TBitmap.Create;  // and re-create for next time.

          if do_container = True      // 219a
          then begin

            with bgnd_shape do begin     // re-size the shape to suit.

              shape_cen.x := (p1.x + p2.x) / 2;        // picture centre.
              shape_cen.y := (p1.y + p2.y) / 2;

              if (bgnd_form.twist_as_all_radiobutton.Checked = True) or
                (all = True) then begin
                case rotate_centre_code of
                  1:
                    dotransform(0 - krot, notchx, notchy, shape_cen, shape_cen);
                  // rotate shape centre around notch.
                  2:
                    dotransform(0 - krot, rings[0, 0], rings[0, 1], shape_cen, shape_cen);
                  // rotate shape centre around spacing ring.
                  3:
                    dotransform(0 - krot, 0, 0, shape_cen, shape_cen);
                  // rotate shape centre around grid origin.
                end;// case
              end;

              old_shapewidth := p2.x - p1.x;
              old_shapeheight := p2.y - p1.y;

              new_shapewidth := old_shapewidth * outwidth / inwidth;
              new_shapeheight := old_shapeheight * outheight / inheight;

              p1.x := shape_cen.x - new_shapewidth / 2;    // new p1..
              p1.y := shape_cen.y - new_shapeheight / 2;

              p2.x := p1.x + new_shapewidth;             // new p2..
              p2.y := p1.y + new_shapeheight;

            end;//with
          end;


          image_width := image_bitmap.Width;     // native dots for readout.
          image_height := image_bitmap.Height;

          shapes_saved := False;          // need a resave.
          shapes_current_state;
          redraw_pad(True, False);
        except
          rotated_bitmap.Free;             // release memory.
          rotated_bitmap := TBitmap.Create;  // and re-create for next time.

          Application.ProcessMessages;
          alert(5, 'twist  bitmap  error',
            'Sorry, the twist bitmap function failed because of memory or resources limitations on your system.'
            + '||Try closing all other applications, temporarily deleting other picture shapes, or re-scanning smaller images or at a lower scan resolution (dpi setting).',
            '', '', '', '', 'cancel', '', 0);
        end;//try-except
      end;//with image
    finally
      Screen.Cursor := saved_screen_cursor;
      Application.ProcessMessages;
    end;//try-finally
  end;//with shape
end;
//______________________________________________________________________________

procedure twist_bitmap_by;

const
  help_twbmp_str: string = '     Twist  Picture  Shape  By ...' +
    '||Enter an angle in degrees for the amount by which to twist the bitmap image contained in the selected picture shape.'
    + '||Angles are measured positive for twists in the anti-clockwise direction, and negative for the clockwise direction.'
    + '||There are 4 options for the rotation centre point around which the image will be twisted:'
    + '||a) the centre of the picture shape.' + '|b) the zero origin of the trackpad grid.' +
    '|c) the current position of the notch.' + '|d) the current position of the spacing-ring tool.'
    + '||These options are selected in the `0PICTURE OPTIONS tab > TWIST SELECT PICTURE AROUND:`1 panel and the `0MODIFY ALL tab > ROTATE ALL AROUND:`1 panel.' + '||If necessary the picture shape outline will be enlarged to accommodate the twisted image. The `0crop/combine`3 function can be used to reduce its size subsequently.' + '||If you need to re-position the picture shape subsequently - click the `0MODIFY SHAPE tab > SHIFT BY...`1 button or the `0MODIFY SHAPE tab > MOUSE ACTIONS: > SHIFT`1 button.' + '||Remember to save a new BGS data file containing the new positions of the picture shape outlines and a new image file.' + '||For more information about twisting bitmap images click the button below.';

var
  n: integer;
  od: Toutdim;
  twist_angle: double;
  twist_cen_str: string;

begin
  twist_cen_str := '';    // keep compiler happy.

  if bgnd_form.twist_as_all_radiobutton.Checked = True then begin
    case rotate_centre_code of
      1:
        twist_cen_str := 'the  notch';
      2:
        twist_cen_str := 'spacing-ring';
      3:
        twist_cen_str := 'grid  origin';
    end;//case
  end
  else
    twist_cen_str := 'its  centre';

  n := putdim(help_twbmp_str, 3, 'twist  bitmap  image  around  ' + twist_cen_str +
    '  by', 0, False, True, False, False);   // negative ok, no preset, zero ok, don't terminate on zero.

  if n <> 0 then
    EXIT;
  if getdims('twist  bitmap  image', twist_help_str, bgnd_form, n, od) = True then begin
    twist_angle := 0 - (od[0] * Pi / 180);
    normalize_angle(twist_angle);

    n := bgnd_form.bgnd_shapes_listbox.ItemIndex;

    if twist_angle > 0 then begin
      while twist_angle > (Pi / 2) do begin          // 90 deg steps first
        twist_picture(n, Pi / 2, True, False, False);
        twist_angle := twist_angle - Pi / 2;
      end;
    end;

    if twist_angle < 0 then begin
      while twist_angle < (0 - Pi / 2) do begin        // 90 deg steps first
        twist_picture(n, 0 - Pi / 2, True, False, False);
        twist_angle := twist_angle + Pi / 2;
      end;
    end;

    twist_picture(n, twist_angle, True, False, False);
    // True,False,False = do picture rectangle too, this one only, not sync
  end
  else
    EXIT;
end;
//______________________________________________________________________________________

procedure minimize_angle(var k: double);

var
  deg90: double;

begin
  deg90 := Pi / 2;

  while k > deg90 do
    k := k - deg90;           // range +/- 90 degs.
  while k < (0 - deg90) do
    k := k + deg90;
end;
//______________________________________________________________________________________

procedure Tbgnd_form.twist_picture_buttonClick(Sender: TObject);

var
  i, n: integer;
  twist_angle: double;
  tw_str: string;

begin
  if marker_angle_twist_is_defined = True then begin
    if use_notch_fixed_marker_checkbox.Checked = False then begin
      twist_angle := marker_angle_twist - marker_angle_fixed;
      minimize_angle(twist_angle);
      tw_str := 'use  marker  lines  ( twist  angle : ' + round_str(
        0 - twist_angle * 180 / Pi, 2) + ' degrees )';
    end
    else begin
      twist_angle := marker_angle_twist - notch_angle;
      minimize_angle(twist_angle);
      tw_str := 'use marker line and notch (twist angle : ' +
        round_str(0 - twist_angle * 180 / Pi, 2) + ' degrees)';
    end;
  end
  else
    tw_str := '( no  twist  marker  line  has  been  set )';

  with bgnd_shapes_listbox do begin
    n := ItemIndex;
    with Items do begin

      if (Count < 1) or (n < 0) or (n > (Count - 1)) then
        EXIT;
      if Tbgshape(Objects[n]).bgnd_shape.shape_code <> -1 then
        EXIT;      // not a picture shape.
    end;//with
  end;//with

  repeat
    i := alert(4, '    twist  picture  shape',
      'This function will twist (rotate) the image contained in this picture shape. It may take several seconds to complete.' + '||You can choose to enter a twist angle in degrees, or let Templot0 use the calculated twist angle shown below to align your marker lines.' + '||(You may want to make a note of this angle and use it in other graphics software to rotate this or other bitmap images.)' + '||Before continuing you may want to save a BGS file, so that you can return to the untwisted original if necessary.', '', '', 'important  information', 'enter  twist  angle', 'cancel', tw_str, 3);
    case i of
      3:
        alert_help(0, twist_help_str, '');

      4:
        twist_bitmap_by;

      5:
        EXIT;

      6: begin
        if marker_angle_twist_is_defined = False then begin
          if alert(6, '    no  marker  line',
            'No twist marker line has been set.',
            '', '', '', 'more  information', 'cancel', '', 4) = 5 then
            EXIT;
          help(0, twist_help_str, '');
          EXIT;
        end;

        if twist_angle > 0 then begin
          while twist_angle > (Pi / 2) do begin          // 90 deg steps first
            twist_picture(n, Pi / 2, True, False, False);
            twist_angle := twist_angle - Pi / 2;
          end;
        end;

        if twist_angle < 0 then begin
          while twist_angle < (0 - Pi / 2) do begin        // 90 deg steps first
            twist_picture(n, 0 - Pi / 2, True, False, False);
            twist_angle := twist_angle + Pi / 2;
          end;
        end;

        twist_picture(n, twist_angle, True, False, False);
        // True,False,False = do picture rectangle too, this one only,  not sync
      end;
    end;//case
  until i <> 3;

  shapes_saved := False;      // need a resave.
  shapes_current_state;

  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________

procedure Tbgnd_form.copy_image_buttonClick(Sender: TObject);

var
  n: integer;

begin

  with bgnd_shapes_listbox do begin
    n := ItemIndex;
    with Items do begin

      if (Count < 1) or (n < 0) or (n > (Count - 1)) then
        EXIT;

      with Tbgshape(Objects[n]) do begin

        if (bgnd_shape.shape_code <> -1)      // not a picture shape.
          or (bgnd_shape.shape_name = empty_picture_str) or (bgimage = nil)
        // no existing bitmap.
        then begin
          show_modal_message('No picture content to copy.');
          EXIT;
        end;
        try
          Clipboard.Assign(bgimage.image_shape.image_bitmap);
        except
          show_modal_message('Sorry, unable to copy this image to the Windows clipboard.');
          EXIT;
        end;
      end;//with
    end;//with
  end;//with

  shapes_current_state;

  show_modal_message('The image has been copied to the Windows clipboard.'
    + #13 + #13 + 'It can now be pasted into any graphics program,'
    + #13 + 'or into a different picture shape.');
end;
//______________________________________________________________________________

procedure Tbgnd_form.save_picture_buttonClick(Sender: TObject);

var
  n: integer;

  create_jpg: TJpegImage;
  //create_gif:TGIFImage;
  create_png: TPortableNetworkGraphic;

  folder_str: string;
  emf_str: string;

  //temp_bitmap:TBitmap;

  img_saved: boolean;

begin
  img_saved := False;   // init

  with bgnd_shapes_listbox do begin
    n := ItemIndex;
    with Items do begin

      if (Count < 1) or (n < 0) or (n > (Count - 1)) then
        EXIT;   //???

      with Tbgshape(Objects[n]) do begin

        if (bgnd_shape.shape_code <> -1)                 // not a picture shape
          or (bgnd_shape.shape_name = empty_picture_str) or (bgimage = nil)
        // no existing image
        then begin
          show_modal_message('No picture content to save.');
          EXIT;
        end;

        with picture_save_dialog do begin  // set up the save dialog.

          if user_save_img_path = '' then begin
            if bgnd_shape.picture_is_metafile = True then
              InitialDir := Config.GetDir(cudiEmfs)
            else
              InitialDir := Config.GetDir(cudiImages);
          end
          else
            InitialDir := user_save_img_path;

          if bgnd_shape.picture_is_metafile = True then
            Filter := 'EMF metafile ( .emf)|*.emf'
          else begin
            Filter :=
              'PNG image ( .png)|*.png|JPEG image ( .jpg  .jp  .jpeg)|*.jpg;*.jp;*.jpeg|BMP image ( .bmp)|*.bmp';
            FilterIndex := 1;
          end;

          FileName := '';

          if Execute = True    // get the file name.
          then begin
            if invalid_85a_file_name(FileName) = True then
              EXIT;

            user_save_img_path := ExtractFilePath(FileName); // for next time.

            if bgnd_shape.picture_is_metafile = True then begin

              with bgimage.image_shape.image_metafile do begin

                emf_str := ChangeFileExt(FileName, '.emf');   // force extension

                //  EMF
                //if CopyEnhMetaFile(emf_HDC, PChar(emf_str)) <>
                //  0 then begin
                //  if alert(2, '   picture  shape  content  saved',
                //    'The EMF metafile was created successfully:||' + emf_str
                //    + '| ', '', '', '',
                //    'open  the  containing  folder', '', 'continue', 0) = 4
                //  then
                //  begin
                //    folder_str := ExtractFilePath(emf_str);
                //
                //    if not OpenDocument(folder_str) then
                //      show_modal_message('Sorry, unable to open the folder.')
                //    else
                //      external_window_showing := True;
                //  end;
                //end
                //else begin
                  user_save_img_path := '';
                  show_modal_message(
                    'Sorry, an error occurred in saving the metafile to ' + #13 + #13 + emf_str);
                //end;
              end;//with metafile
            end
            else begin     // bitmap...
              try
                if (LowerCase(ExtractFileExt(FileName)) =
                  '.jpg') or (LowerCase(ExtractFileExt(FileName)) = '.jpeg')
                then begin
                  create_jpg := TJpegImage.Create;
                  try
                    create_jpg.Assign(bgimage.image_shape.image_bitmap);

                    create_jpg.CompressionQuality := jpg_quality;
                    // global on control_room

                    create_jpg.SaveToFile(FileName);
                  finally
                    create_jpg.Free;
                  end;//try
                  img_saved := True;
                end;

                { OT-FIRST
                                if LowerCase(ExtractFileExt(FileName))='.gif'
                                    then begin
                                           temp_bitmap:=TBitmap.Create;
                                           create_gif:=TGIFImage.Create;
                                           try
                                             temp_bitmap.Assign(bgimage.image_shape.image_bitmap);

                                             with temp_bitmap do begin
                                               PixelFormat:=pf8bit;
                                               TransparentColor:=clWhite;
                                               Transparent:=bgnd_form.trans_checkbox.Checked;
                                             end;//with


                                             create_gif.Assign(temp_bitmap);


                                             create_gif.Transparent:=bgnd_form.trans_checkbox.Checked;

                                             create_gif.Interlaced:=False;

                                             create_gif.BitsPerPixel:=8;


                                             create_gif.SaveToFile(FileName);
                                           finally
                                             temp_bitmap.Free;
                                             create_gif.Free;
                                           end;//try
                                           img_saved:=True;
                                         end;
                                }

                if LowerCase(ExtractFileExt(FileName)) = '.png'
                then begin
                  create_png := TPortableNetworkGraphic.Create;
                  try
                    create_png.Assign(bgimage.image_shape.image_bitmap);
                    create_png.SaveToFile(FileName);
                  finally
                    create_png.Free;
                  end;//try
                  img_saved := True;
                end;

                if LowerCase(ExtractFileExt(FileName)) = '.bmp'
                then begin
                  bgimage.image_shape.image_bitmap.SaveToFile(FileName);
                  img_saved := True;
                end;

                if img_saved = True then begin
                  if alert(2, '   picture  shape  content  saved',
                    'The image file was created successfully:||' + FileName
                    + '| ', '', '', '',
                    'open  the  containing  folder', '', 'continue', 0) = 4 then
                  begin
                    folder_str := ExtractFilePath(FileName);

                    if not OpenDocument(folder_str) then
                      show_modal_message('Sorry, unable to open the folder.')
                    else
                      external_window_showing := True;
                  end;

                end
                else
                  show_modal_message('Sorry, an error occurred in saving the image to ' + #13 + #13 + FileName);

              except
                user_save_img_path := '';
                show_modal_message('Sorry, an error occurred in saving the image to '
                  + #13 + #13 + FileName);
              end;//try

            end;//metafile/bitmap
          end;//if Execute
        end;//with dialog
      end;//with shape object
    end;//with Items
  end;//with list
end;
//_________________________________________________________________________________________

procedure Tbgnd_form.marker_line_fixed_buttonClick(Sender: TObject);

var
  n: integer;
  hyp: double;

begin
  with bgnd_shapes_listbox do begin
    n := ItemIndex;

    if (Items.Count < 1) or (n < 0) or (n > (Items.Count - 1)) then
      EXIT;

    with Tbgshape(Items.Objects[n]).bgnd_shape do begin

      if shape_code <> 0 then
        EXIT;  // ??? not a line - button should be disabled.

      shape_name := 'marker-line (fixed line)';
      Items.Strings[n] := shape_name;

      try
        normalize_line(p1, p2); // make sure p2 end is on right.

        hyp := SQRT(SQR(p2.x - p1.x) + SQR(p2.y - p1.y));
        if hyp < minfp then
          marker_angle_fixed := 0                         // single point.
        else
          marker_angle_fixed := ARCSIN((p2.y - p1.y) / hyp);
      except
        marker_angle_fixed := 0;
      end;//try

    end;//with

    ItemIndex := n;
  end;//with

  show_modal_message('The fixed marker angle is ' + round_str(marker_angle_fixed * 180 / Pi, 2) +
    ' degrees.');
  // added 214a
end;
//___________________________________________________________________________________

procedure Tbgnd_form.marker_line_twist_buttonClick(Sender: TObject);

var
  n: integer;
  hyp: double;

begin
  with bgnd_shapes_listbox do begin
    n := ItemIndex;

    if (Items.Count < 1) or (n < 0) or (n > (Items.Count - 1)) then
      EXIT;

    with Tbgshape(Items.Objects[n]).bgnd_shape do begin

      if shape_code <> 0 then
        EXIT;  // ??? not a line - button should be disabled.

      shape_name := 'marker-line (twist line)';
      Items.Strings[n] := shape_name;

      try
        normalize_line(p1, p2); // make sure p2 end is on right.

        hyp := SQRT(SQR(p2.x - p1.x) + SQR(p2.y - p1.y));
        if hyp < minfp then
          marker_angle_twist := 0                         // single point.
        else
          marker_angle_twist := ARCSIN((p2.y - p1.y) / hyp);
        marker_angle_twist_is_defined := True;
      except
        marker_angle_twist := 0;
      end;//try

    end;//with

    ItemIndex := n;
  end;//with

  if use_notch_fixed_marker_checkbox.Checked = False then
    show_modal_message('The twist marker angle is ' + round_str(marker_angle_twist * 180 / Pi, 2) +
      ' degrees.' + #13 + #13 +
      'The difference from the fixed marker angle is ' + round_str(
      (marker_angle_fixed - marker_angle_twist) * 180 / Pi, 2) + ' degrees.' +
      #13 + #13 + 'A picture shape would be twisted by this amount.')  // added 214a

  else
    show_modal_message('The twist marker angle is ' + round_str(marker_angle_twist * 180 / Pi, 2) +
      ' degrees.');
end;
//______________________________________________________________________________

procedure Tbgnd_form.trackpad_grid_in_front_checkboxClick(Sender: TObject);

begin
  pad_form.grid_in_front_of_shapes_menu_entry.Checked := trackpad_grid_in_front_checkbox.Checked;
  do_rollback := False;
  redraw(True);
end;
//_____________________________________________________________________________________

procedure Tbgnd_form.use_notch_fixed_marker_checkboxClick(Sender: TObject);

begin
  shapes_current_state;
  //marker_line_fixed_button.Enabled:= NOT use_notch_fixed_marker_checkbox.Checked;
end;
//_____________________________________________________________________________________

procedure Tbgnd_form.rotate_grid_radiobuttonClick(Sender: TObject);

begin
  if rotate_grid_radiobutton.Checked = True then
    rotate_centre_code := 3;     // default
end;
//____________________________________________________________________

procedure Tbgnd_form.rotate_notch_radiobuttonClick(Sender: TObject);

begin
  if rotate_notch_radiobutton.Checked = True then
    rotate_centre_code := 1;
end;
//_____________________________________________________________________

procedure Tbgnd_form.rotate_ring_radiobuttonClick(Sender: TObject);

begin
  if rotate_ring_radiobutton.Checked = True then
    rotate_centre_code := 2;
end;
//_____________________________________________________________________________________

procedure reload_recent_bgs;  // 0.82.a  set up mru menu

var
  i: integer; // dummy

  ///////////////////////////////////////

  procedure do_item(n: integer; popup_item: TMenuItem);

  var
    num_str: string;

  begin
    num_str := IntToStr(6 - n);    // number showing in list.

    with bgsmru_list do begin

      if Strings[n] <> '' then begin
        popup_item.Caption :=
          num_str + '      ' + ExtractFileName(Strings[n]) + '      •      ' + ExtractFilePath(Strings[n]);
        popup_item.Enabled := True;
        bgnd_form.clear_recent_bgs_popup_entry.Enabled := True;
      end
      else begin
        popup_item.Caption := num_str + '      ( empty )';
        popup_item.Enabled := False;
      end;
    end;//with
  end;
  /////////////////////////////////////////

begin
  if bgsmru_list.Count <> 6 then
    EXIT;  // ??? error

  // menu is in reverse order to list, list has latest at bottom.

  do_item(5, bgnd_form.recent_1_popup_entry);
  do_item(4, bgnd_form.recent_2_popup_entry);
  do_item(3, bgnd_form.recent_3_popup_entry);
  do_item(2, bgnd_form.recent_4_popup_entry);
  do_item(1, bgnd_form.recent_5_popup_entry);
  do_item(0, bgnd_form.recent_6_popup_entry);

  with bgnd_form do
    recent_popup_menu.PopUp(Left + ClientWidth div 10, Top + ClientHeight div 5);    // 0.82.a
end;
//________________________________________

procedure Tbgnd_form.recent_buttonClick(Sender: TObject);       // 0.82.a  22-08-06

begin
  reload_recent_bgs;
end;
//________________________________________

procedure Tbgnd_form.recent_1_popup_entryClick(Sender: TObject);

begin
  if bgsmru_list.Strings[5] <> '' then
    load_shapes(bgsmru_list.Strings[5], False, True, False);
end;
//_____________________________

procedure Tbgnd_form.recent_2_popup_entryClick(Sender: TObject);

begin
  if bgsmru_list.Strings[4] <> '' then
    load_shapes(bgsmru_list.Strings[4], False, True, False);
end;
//_____________________________

procedure Tbgnd_form.recent_3_popup_entryClick(Sender: TObject);

begin
  if bgsmru_list.Strings[3] <> '' then
    load_shapes(bgsmru_list.Strings[3], False, True, False);
end;
//_____________________________

procedure Tbgnd_form.recent_4_popup_entryClick(Sender: TObject);

begin
  if bgsmru_list.Strings[2] <> '' then
    load_shapes(bgsmru_list.Strings[2], False, True, False);
end;
//_____________________________

procedure Tbgnd_form.recent_5_popup_entryClick(Sender: TObject);

begin
  if bgsmru_list.Strings[1] <> '' then
    load_shapes(bgsmru_list.Strings[1], False, True, False);
end;
//_____________________________

procedure Tbgnd_form.recent_6_popup_entryClick(Sender: TObject);

begin
  if bgsmru_list.Strings[0] <> '' then
    load_shapes(bgsmru_list.Strings[0], False, True, False);
end;
//___________________________________________________________________________

procedure Tbgnd_form.clear_recent_bgs_popup_entryClick(Sender: TObject);

begin
  empty_bgsmru;      // 0.82.a
end;
//____________________________________________________________________________

function straighten_picture: boolean;  // 205e

const
  fill_colour: TColor = $00E0E0E0;  // lightest grey

  banding_help_str: string = 'php/810    `0straighten  picture  shape  -  banding`9'
    + '||You can if you wish restrict the straightening of your picture shape image to a band on each side of the control template.'
    + ' The remainder of the original image will be discarded.' +
    '||In many cases this will cause the straightened image to be much smaller than would otherwise be the case.'
    + '||This will speed up the straightening function, and reduce screen clutter if you have several straightened images.'
    + '||The dimensions in mm for the depth of the band on the TS and MS sides of the control template are entered separately.'
    + '||If `00`2 (zero) is entered, the full image area will be used on that side.'
    + '||green_panel_begintree.gif If the control template is plain track you can quickly see which side is TS and which is MS by inserting a turnout. This will have no effect on the straightening function.' + '||It is normally best to arrange the control template as a LH (left-hand) template running from left to right across the image with the `0CTRL-0`2 position on the left.' + '||In which case the TS (turnout) side band will be above the control template and the MS (main) side band will be below it.green_panel_end';

var
  n, i, j: integer;
  od: Toutdim;

  inrow, incol, outrow, outcol: integer;

  pixel_colour: TColor;
  saved_screen_cursor: TCursor;

  inwidth_dots, inheight_dots: integer;
  outwidth_dots, outheight_dots: integer;

  old_shapewidth_mm, old_shapeheight_mm: double;    //mm
  new_shapewidth_mm, new_shapeheight_mm: double;    //mm

  old_diagonal_mm: double;

  this_shape: Tbgnd_shape;

  picture_scale_width, picture_scale_height: double;

  x_on_pad, y_on_pad, dummy_k, dummy_r: double;

  xs_on_control, xs_step: double;
  ys_on_control, ys_step: double;

  x1, y1, x2, y2: double;

  max_y, min_y: double;

  x_org_mm, y_org_mm, x_end_mm: double;

  ycurved: double;

  ms_limit_mm, ts_limit_mm: double;

  destination_bitmap: TBitmap;
  waitMessage: IAutoWaitMessage;

  //////////////////////////////////////////////////////////

  function get_col_row(x, y: double; var col, row: integer): boolean;

    // return image column and row for this x,y on pad
    // return False if x,y not inside image

  begin
    Result := False;  // init

    if (x < x1) or (y < y1) or (x > x2) or (y > y2)  // x,y not inside image
    then begin
      col := 0;
      row := 0;

      EXIT;
    end;

    col := Round((x - x1) * picture_scale_width);
    row := Round((y2 - y) * picture_scale_height);  // row from top

    Result := True;

  end;
  //////////////////////////////////////////////////////////

begin
  Result := False;

  if pegx >= turnoutx then
    EXIT;

  i := bgnd_form.bgnd_shapes_listbox.ItemIndex;

  if (i < 0) or (i > bgnd_form.bgnd_shapes_listbox.Items.Count - 1) then
    EXIT;

  with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]) do begin

    if bgnd_shape.shape_code <> -1 then
      EXIT;      // not a picture shape.

    if (bgnd_shape.shape_name = empty_picture_str) or (bgimage = nil)   // no existing bitmap.
    then
      EXIT;

    this_shape := bgnd_shape;  // copy from list

    with this_shape do begin
      old_shapewidth_mm := ABS(p2.x - p1.x);    // mm on pad
      old_shapeheight_mm := ABS(p2.y - p1.y);

      if (old_shapewidth_mm < minfp) or (old_shapeheight_mm < minfp) then begin
        show_modal_message(
          'The selected picture shape has zero width or zero height and cannot be straightened.');
        EXIT;
      end;

      old_diagonal_mm := SQRT(SQR(old_shapewidth_mm) + SQR(old_shapeheight_mm));

      ms_limit_mm := 0;
      ts_limit_mm := 0;

      repeat

        j := alert(4, 'php/810   apply  banding  limits ?',
          'Do you want to restrict the straightened image to a band along each side of the control template?',
          '', '', 'more  information', 'yes  -  enter  band  dimensions',
          'cancel', 'no  -  straighten  entire  image', 3);
        case j of
          3:
            alert_help(0, banding_help_str, '');
          4: begin
            putdim(banding_help_str, 1, 'band  depth  TS  side', ts_limit_mm,
              True, True, False, False);    // no neg, no preset, zero ok, don't terminate on zero.
            n := putdim(banding_help_str, 1, 'band  depth  MS  side', ms_limit_mm,
              True, True, False, False);    // ditto.

            if n <> 1 then
              run_error(159);

            if getdims(
              'straighten  picture  shape  along  the  control  template  -  banding  limits',
              '', bgnd_form, n, od) = False then
              EXIT;

            ts_limit_mm := od[0];
            ms_limit_mm := od[1];

          end;

          5:
            EXIT;
        end;//case
      until j <> 3;

      if alert(7, 'php/810 straighten  picture  shape  along  the  control  template',
        'This straightening function cannot be reversed.' +
        '||Before proceeding you may want to save your current background shapes so that you can revert to them if the straightening results are not as expected or intended .' + '||Cancel this and click the `0save all as...`1 button to save your background shapes.', '', '', '', '', 'cancel', 'continue  -  straighten  picture  shape', 0) = 5 then
        EXIT;


      // x1,y1 image origin bottom left
      // x2,y2 top right...

      if p1.x < p2.x then begin
        x1 := p1.x;
        x2 := p2.x;
      end
      else begin
        x1 := p2.x;
        x2 := p1.x;
      end;

      if p1.y < p2.y then begin
        y1 := p1.y;
        y2 := p2.y;
      end
      else begin
        y1 := p2.y;
        y2 := p1.y;
      end;

    end;//with shape

    // init..

    max_y := 0 - maxfp;
    min_y := maxfp;

    saved_screen_cursor := Screen.Cursor;
    Screen.Cursor := crHourglass;

    destination_bitmap := TBitmap.Create;

    try
      with bgimage.image_shape do begin

        inwidth_dots := image_bitmap.Width;       // dots
        inheight_dots := image_bitmap.Height;

        if (inwidth_dots < 1) or (inheight_dots < 1) then
          EXIT;

        picture_scale_width := inwidth_dots / old_shapewidth_mm;     // dots per model mm.
        picture_scale_height := inheight_dots / old_shapeheight_mm;  // dots per model mm.

        waitMessage := TWaitForm.ShowWaitMessageWithProgress('straightening  bitmap ...', 0,
          Round(inwidth_dots * 2), 0, 1);
        Application.ProcessMessages;  // allow some repaints before we start.

        xs_step := ABS(old_shapewidth_mm / inwidth_dots);   // mm
        ys_step := ABS(old_shapeheight_mm / inheight_dots); // mm

        // run along control template centre-line from peg to end of template...

        // 2 separate runs, first to create output bitmap size...

        xs_on_control := pegx;

        repeat
          // TS, do each side of centreline separately ...

          ys_on_control := g / 2;  // init on control centre-line

          repeat
            docurving(True, True, xs_on_control, ys_on_control, x_on_pad, ycurved, dummy_k, dummy_r);

            y_on_pad := ycurved * hand_i + y_datum;

            if get_col_row(x_on_pad, y_on_pad, incol, inrow) = True then begin
              if max_y < (ys_on_control - g / 2) then
                max_y := ys_on_control - g / 2;       // from centre-line
              if min_y > (ys_on_control - g / 2) then
                min_y := ys_on_control - g / 2;
            end;

            ys_on_control := ys_on_control + ys_step;

          until (ys_on_control > old_diagonal_mm) or ((ts_limit_mm > 0) and
              (ys_on_control > (g / 2 + ts_limit_mm)));


          // MS, do each side of centreline separately ...

          ys_on_control := g / 2;  // init on control centre-line

          repeat
            docurving(True, True, xs_on_control, ys_on_control, x_on_pad, ycurved, dummy_k, dummy_r);

            y_on_pad := ycurved * hand_i + y_datum;

            if get_col_row(x_on_pad, y_on_pad, incol, inrow) = True then begin
              if max_y < (ys_on_control - g / 2) then
                max_y := ys_on_control - g / 2;        // from centre-line
              if min_y > (ys_on_control - g / 2) then
                min_y := ys_on_control - g / 2;
            end;

            ys_on_control := ys_on_control - ys_step;

          until (ys_on_control < (0 - old_diagonal_mm)) or ((ms_limit_mm > 0) and
              (ys_on_control < (g / 2 - ms_limit_mm)));

          // next along..

          xs_on_control := xs_on_control + xs_step;

          waitMessage.StepIt;  // 1st half of bar

        until xs_on_control > turnoutx;

        if (max_y = 0 - maxfp) or (min_y = maxfp) then begin
          alert(6, 'php/810    control  template  in  wrong  position',
            '`0straighten image along control template`9'
            +
            '||rp.gif The control template is in the wrong position. No part of the control template is currently over the selected picture shape.' + '||In order to straighten the image along the control template, all or part of the control template main-road centre-line must be positioned over the image.' + '||Please check that you have selected the correct picture shape in the list.' + '||Otherwise please click the `0more information online`1 button above for more information and explanation.',
            '', '', '', '', 'cancel', '', 0);
          EXIT;
        end;

        new_shapewidth_mm := turnoutx - pegx;
        new_shapeheight_mm := ABS(max_y - min_y);

        outwidth_dots := Round(new_shapewidth_mm * picture_scale_width);
        outheight_dots := Round(new_shapeheight_mm * picture_scale_height);

        try
          destination_bitmap.Assign(image_bitmap);        // set same properties, bits/pixel, etc.
          destination_bitmap.Width := outwidth_dots;
          // ensure output is large enough to contain it.
          destination_bitmap.Height := outheight_dots;

          with destination_bitmap do begin
            with Canvas do begin
              Brush.Color := fill_colour;  //  this clears the new bitmap for starters
              Brush.Style := bsSolid;
              FillRect(Rect(0, 0, Width, Height));

              Pen.Color := clBlack;
              Pen.Mode := pmCopy;
              Pen.Style := psSolid;
              Pen.Width := 1;

              Font.Assign(pad_form.Font);

              TextOut(0, 0, ' ');

            end;//with
          end;//with

          // repeat to create actual image ...

          xs_on_control := pegx;

          repeat
            // TS, do each side of centreline separately ...

            ys_on_control := g / 2;  // init on control centre-line

            repeat
              docurving(True, True, xs_on_control, ys_on_control, x_on_pad, ycurved, dummy_k, dummy_r);

              y_on_pad := ycurved * hand_i + y_datum;

              if get_col_row(x_on_pad, y_on_pad, incol, inrow) = True    // on the image
              then begin
                if not ((incol < 0) or (incol > (inwidth_dots - 1)) or
                  (inrow < 0) or (inrow > (inheight_dots - 1))) then begin
                  pixel_colour := image_bitmap.Canvas.Pixels[incol, inrow];

                  with destination_bitmap.Canvas do begin

                    outcol := Round((xs_on_control - pegx) * picture_scale_width);

                    outrow := outheight_dots - 1 - Round(
                      (ys_on_control - g / 2 - min_y) * picture_scale_height);  // top-down

                    Pen.Color := pixel_colour;
                    MoveTo(outcol, outrow);
                    LineTo(outcol + 1, outrow);
                    // most of this will get overwritten later.
                    LineTo(outcol + 1, outrow + 1);
                    LineTo(outcol, outrow + 1);

                  end;//with
                end;
              end;

              ys_on_control := ys_on_control + ys_step;

            until (ys_on_control > old_diagonal_mm) or ((ts_limit_mm > 0) and
                (ys_on_control > (g / 2 + ts_limit_mm)));

            // MS, do each side of centreline separately ...

            ys_on_control := g / 2;  // init on control centre-line

            repeat
              docurving(True, True, xs_on_control, ys_on_control, x_on_pad, ycurved, dummy_k, dummy_r);

              y_on_pad := ycurved * hand_i + y_datum;

              if get_col_row(x_on_pad, y_on_pad, incol, inrow) = True    // on the image
              then begin
                if not ((incol < 0) or (incol > (inwidth_dots - 1)) or
                  (inrow < 0) or (inrow > (inheight_dots - 1))) then begin
                  pixel_colour := image_bitmap.Canvas.Pixels[incol, inrow];

                  with destination_bitmap.Canvas do begin

                    outcol := Round((xs_on_control - pegx) * picture_scale_width);

                    outrow := outheight_dots - 1 - Round(
                      (ys_on_control - g / 2 - min_y) * picture_scale_height);  // top-down

                    Pen.Color := pixel_colour;
                    MoveTo(outcol, outrow);
                    LineTo(outcol + 1, outrow);
                    // most of this will get overwritten later.
                    LineTo(outcol + 1, outrow + 1);
                    LineTo(outcol, outrow + 1);

                  end;//with
                end;
              end;

              ys_on_control := ys_on_control - ys_step;

            until (ys_on_control < (0 - old_diagonal_mm)) or ((ms_limit_mm > 0) and
                (ys_on_control < (g / 2 - ms_limit_mm)));


            // next along..

            xs_on_control := xs_on_control + xs_step;

            waitMessage.StepIt;  // 2nd half of bar

          until xs_on_control > turnoutx;

          destination_bitmap.SaveToFile(Config.GetFilePath(csfi_85a_temp));
          // !!! this way corrects a bug with the display of monochrome bitmaps.

          image_bitmap.LoadFromFile(Config.GetFilePath(csfi_85a_temp));

          DeleteFile(Config.GetFilePath(csfi_85a_temp));


          // get offsets...

          docurving(True, True, pegx, g / 2, x_org_mm, ycurved, dummy_k, dummy_r);   // peg position on pad

          y_org_mm := ycurved * hand_i + y_datum;

          docurving(True, True, turnoutx, g / 2, x_end_mm, ycurved, dummy_k, dummy_r);
          // CTRL-9 on pad (to get direction from peg)

          with this_shape do begin     // re-size the shape to suit.

            if x_end_mm > x_org_mm then
              p2.x := p1.x + new_shapewidth_mm   // p1 unchanged x, new p2
            else
              p1.x := p2.x - new_shapewidth_mm;  // p2 unchanged x, new p1

            p1.y := y_org_mm + min_y;
            p2.y := p1.y + new_shapeheight_mm;

            wrap_offset := Round(0 - min_y * 100);   // 205e     integer 1/100th mm

          end;//with

          image_width := image_bitmap.Width;     // native dots for readout.
          image_height := image_bitmap.Height;

          bgnd_shape := this_shape;  // into list

          shapes_saved := False;          // need a resave.
          shapes_current_state;
          redraw_pad(True, False);

          Result := True;
        except
          alert(5, 'straighten  bitmap  error',
            'Sorry, the straighten bitmap function failed because of memory or resources limitations on your system.'
            + '||Try closing all other applications, temporarily deleting other picture shapes, or re-scanning smaller images or at a lower scan resolution (dpi setting).',
            '', '', '', '', 'cancel', '', 0);
        end;//try-except

      end;//with image

    finally
      destination_bitmap.Free;
      Screen.Cursor := saved_screen_cursor;

    end;//try-finally
  end;//with shape
end;
//______________________________________________________________________________

procedure wrap_picture(datum_offset_mm, x_factor, y_factor: double);
// curve bitmap along datum line to align with the control template.

// datum line offset up from bottom edge.

// re-size and re-locate picture shape accordingly.

const
  fill_colour: TColor = $00D0D0D0;  // light grey

var
  i: integer;
  inrow, incol, outrow, outcol: integer;

  pixel_colour: TColor;
  saved_screen_cursor: TCursor;

  inwidth, inheight: double;
  outwidth, outheight: double;

  old_shapewidth, old_shapeheight: double;    //mm
  new_shapewidth, new_shapeheight: double;    //mm

  this_shape: Tbgnd_shape;

  xc, yc, tn, rn: double;

  yc_on_pad: double;

  picture_scale_width, picture_scale_height: double;

  max_extent_x, max_extent_y, min_extent_x, min_extent_y: double;

  destination_bitmap_height: integer;

  destination_bitmap: TBitmap;
  waitMessage: IAutoWaitMessage;

begin

  i := bgnd_form.bgnd_shapes_listbox.ItemIndex;

  if (i < 0) or (i > bgnd_form.bgnd_shapes_listbox.Items.Count - 1) then
    EXIT;

  with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]) do begin

    if bgnd_shape.shape_code <> -1 then
      EXIT;      // not a picture shape.

    if (bgnd_shape.shape_name = empty_picture_str) or (bgimage = nil)   // no existing bitmap.
    then
      EXIT;

    this_shape := bgnd_shape;  // copy from list

    with this_shape do begin
      old_shapewidth := ABS(p2.x - p1.x);    // mm on pad
      old_shapeheight := ABS(p2.y - p1.y);
    end;//with

    if (old_shapewidth < minfp) or (old_shapeheight < minfp) then begin
      show_modal_message(
        'The selected picture shape has zero width or zero height and cannot be wrapped.');
      EXIT;
    end;

    saved_screen_cursor := Screen.Cursor;
    Screen.Cursor := crHourglass;

    destination_bitmap := TBitmap.Create;

    try
      with bgimage.image_shape do begin

        inwidth := image_bitmap.Width;       // dots
        inheight := image_bitmap.Height;

        picture_scale_width := inwidth / old_shapewidth;
        picture_scale_height := inheight / old_shapeheight;  // dots per model mm.

        waitMessage := TWaitForm.ShowWaitMessageWithProgress('wrapping  bitmap ...',
          0, Round(inheight * 2), 0, 1);
        Application.ProcessMessages;  // allow some repaints we start.

        max_extent_x := 0 - maxfp;   //init
        max_extent_y := 0 - maxfp;
        min_extent_x := maxfp;
        min_extent_y := maxfp;

        // from top-left of bitmap ...

        // first get the new size ...

        for inrow := 0 to Trunc(inheight - 1) do begin
          for incol := 0 to Trunc(inwidth - 1) do begin

            xs := pegx + (incol / inwidth * old_shapewidth) * x_factor;

            ys := g / 2 + (old_shapeheight - inrow / inheight * old_shapeheight - datum_offset_mm) *
              y_factor * hand_i;

            // get xc,yc,tn,rn ...

            docurving(True, True, xs, ys, xc, yc, tn, rn);
            // calc curving to control template, and call any transforms wanted.

            yc_on_pad := yc * hand_i + y_datum;

            if xc > max_extent_x then
              max_extent_x := xc;
            if yc_on_pad > max_extent_y then
              max_extent_y := yc_on_pad;

            if xc < min_extent_x then
              min_extent_x := xc;
            if yc_on_pad < min_extent_y then
              min_extent_y := yc_on_pad;

          end;//next col

          waitMessage.StepIt;  // 1st half of bar
          Application.ProcessMessages;

        end;//next row

        new_shapewidth := ABS(max_extent_x - min_extent_x);
        new_shapeheight := ABS(max_extent_y - min_extent_y);

        outwidth := new_shapewidth * picture_scale_width;
        outheight := new_shapeheight * picture_scale_height;

        try
          destination_bitmap.Assign(image_bitmap);        // set same properties, bits/pixel, etc.
          destination_bitmap.Width := Round(outwidth);
          // ensure output is large enough to contain it.
          destination_bitmap.Height := Round(outheight);

          destination_bitmap_height := destination_bitmap.Height;

          with destination_bitmap do begin
            with Canvas do begin
              Brush.Color := fill_colour;  //  this clears the new bitmap for starters
              Brush.Style := bsSolid;
              FillRect(Rect(0, 0, Width, Height));

              Pen.Color := clBlack;
              Pen.Mode := pmCopy;
              Pen.Style := psSolid;
              Pen.Width := 1;

              Font.Assign(pad_form.Font);

              TextOut(0, 0, ' ');

            end;//with
          end;//with

          // repeat to create actual image ...

          for inrow := 0 to Trunc(inheight - 1) do begin
            for incol := 0 to Trunc(inwidth - 1) do begin
              pixel_colour := image_bitmap.Canvas.Pixels[incol, inrow];

              xs := pegx + (incol / inwidth * old_shapewidth) * x_factor;

              ys := g / 2 + (old_shapeheight - inrow / inheight * old_shapeheight - datum_offset_mm) *
                y_factor * hand_i;

              // get xc,yc,tn,rn ...

              docurving(True, True, xs, ys, xc, yc, tn, rn);
              // calc curving to control template, and call any transforms wanted.

              yc_on_pad := yc * hand_i + y_datum;

              with destination_bitmap.Canvas do begin
                outcol := Trunc((xc - min_extent_x) * picture_scale_width);
                // slow method - makes better job of filling all pixels.
                outrow := destination_bitmap_height - Trunc(
                  (yc_on_pad - min_extent_y) * picture_scale_height{-0.5});
                Pen.Color := pixel_colour;
                MoveTo(outcol, outrow);
                LineTo(outcol + 1, outrow);        // most of this will get overwritten later.
                LineTo(outcol + 1, outrow + 1);
                LineTo(outcol, outrow + 1);
              end;//with
            end;//for

            waitMessage.StepIt;  // 2nd half of bar
            Application.ProcessMessages;

          end;//for

          destination_bitmap.SaveToFile(Config.GetFilePath(csfi_85a_temp));
          // !!! this way corrects a bug with the display of monochrome bitmaps.

          image_bitmap.LoadFromFile(Config.GetFilePath(csfi_85a_temp));

          DeleteFile(Config.GetFilePath(csfi_85a_temp));

          with this_shape do begin     // re-size the shape to suit.

            p1.x := min_extent_x;
            p1.y := min_extent_y;

            p2.x := p1.x + new_shapewidth;             // new p2..
            p2.y := p1.y + new_shapeheight;

          end;//with

          image_width := image_bitmap.Width;     // native dots for readout.
          image_height := image_bitmap.Height;

          bgnd_shape := this_shape;  // into list

          shapes_saved := False;          // need a resave.
          shapes_current_state;
          redraw_pad(True, False);
        except
          alert(5, 'wrap  bitmap  error',
            'Sorry, the wrap bitmap function failed because of memory or resources limitations on your system.'
            + '||Try closing all other applications, temporarily deleting other picture shapes, or re-scanning smaller images or at a lower scan resolution (dpi setting).',
            '', '', '', '', 'cancel', '', 0);
        end;//try-except

      end;//with image

    finally

      destination_bitmap.Free;

      Screen.Cursor := saved_screen_cursor;
    end;//try-finally
  end;//with shape

end;
//________________________________________________________________________________________

procedure Tbgnd_form.straighten_picture_buttonClick(Sender: TObject);   // 205e

begin
  if straighten_picture = False then
    EXIT;   // failed or cancelled

  // straighten the control template to match:

  store_and_background(False, False);  // onto background as-is.

  pad_form.straight_template_menu_entry.Click;

  new_notch(get_peg_for_notch, False);

  notch_angle := 0;

  shift_onto_notch(False, True);

end;
//______________________________________________________________________________

procedure Tbgnd_form.wrap_picture_buttonClick(Sender: TObject);

const
  wrapping_help_str: string = 'php/810    `0wrapping  line  offset`9' +
    '||Enter a dimension in mm from the bottom edge of the picture shape to the position of a horizontal reference line through the image which will be used to wrap the image along the control template.' + '||green_panel_begin tree.gif If you have previously straightened this image using the `0straighten along the control template`1 function you should normally leave the dimension showing unchanged - click `0enter`1.green_panel_end';

  wrap_shrinkx_help_str: string = 'php/810    `0length - wise  shrink / stretch  factor`9' +
    '||While the image is being wrapped along the control template it can be shrunk or stretched length-wise. Enter the % change required. For example entering 75 will reduce the image length to 75% of its unwrapped length.' + '||This can be useful in fitting prototype track plans in a restricted model space.' + '||Enter 100 to leave the image at its original size (100%).';

  wrap_shrinky_help_str: string = 'php/810    `0width - wise  shrink / stretch  factor`9'
    + '||While the image is being wrapped along the control template it can be shrunk or stretched width-wise (i.e. in image height). Enter the % change required.' + ' For example entering 80 will reduce the image width (height) to 80% of its unwrapped width (height).' + '||This can be useful in fitting prototype track plans in a restricted model space.' + '||Enter 100 to leave the image at its original size (100%).' + '||rp.gif If any width-wise shrinking or stretching is done, after wrapping the offset in mm to the reference line will have been modified accordingly. Bear this in mind when checking the finished dimensions.';

var
  datum_offset_mm, x_factor, y_factor: double;

  n, i: integer;
  od: Toutdim;

begin
  i := bgnd_shapes_listbox.ItemIndex;

  if (i < 0) or (i > bgnd_shapes_listbox.Items.Count - 1) then
    EXIT;

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[i]) do begin

    if bgnd_shape.shape_code <> -1 then
      EXIT;      // not a picture shape.

    if (bgnd_shape.shape_name = empty_picture_str) or (bgimage = nil)   // no existing bitmap.
    then
      EXIT;

    datum_offset_mm := bgnd_shape.wrap_offset / 100; // 205e integer 1/100th mm in file

    if (datum_offset_mm < 0) or (datum_offset_mm > 10000)
    // must be old BGS file before 205e - 10000mm arbitrary sensible limit
    then begin
      datum_offset_mm := 0;
      bgnd_shape.wrap_offset := 0;
    end;
  end;//with

  x_factor := 1.0;
  y_factor := 1.0;

  putdim(wrapping_help_str, 1, 'wrapping  line  offset  from  bottom  of  image',
    datum_offset_mm, False, True, False, False);   // neg ok, no preset, zero ok, don't terminate on zero.
  putdim(wrap_shrinkx_help_str, 4, 'length - wise  shrink / stretch  factor',
    (x_factor * 100), True, True, True, False);
  // no neg, no preset, no zero, don't terminate on zero.
  n := putdim(wrap_shrinky_help_str, 4, 'width - wise  shrink / stretch  factor',
    (y_factor * 100), True, True, True, False);                // ditto.

  if n <> 2 then
    run_error(159);
  if getdims('wrap  picture  shape  along  the  control  template', '', bgnd_form, n, od) = False then
    EXIT;

  datum_offset_mm := od[0];
  x_factor := od[1] / 100;
  y_factor := od[2] / 100;

  if alert(7, 'php/810 wrap  picture  shape  along  the  control  template',
    'This wrapping function cannot be reversed.' +
    '||Before proceeding you may want to save your current background shapes so that you can revert to them if the wrapping results are not as expected or intended .' + '||Cancel this and click the `0save all as...`1 button to save your background shapes.', '', '', '', '', 'cancel', 'continue  -  wrap  picture  shape', 0) = 5 then
    EXIT;

  Tbgshape(bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.wrap_offset :=
    Round(datum_offset_mm * 100);
  // 205e save offset in file  in 1/100mm integer

  wrap_picture(datum_offset_mm, x_factor, y_factor);
end;
//______________________________________________________________________________

procedure bgnd_move_up_button_click;

var
  t: integer;
  n: integer;

begin

  with bgnd_form.bgnd_shapes_listbox do begin
    n := ItemIndex;

    if (n < 1) or (n > (Items.Count - 1)) or (Items.Count < 2) then
      EXIT;

    t := TopIndex;                          // index of top displayed item.
    if (t < 0) or (t > (Items.Count - 1)) then
      EXIT;  // ???

    Items.Exchange(n - 1, n);
    n := n - 1;

    if ItemHeight < 1 then
      EXIT;     // div zero check.

    Visible := False;                // avoid flicker.

    if (n >= t) and (n < (t + (Height div ItemHeight)))   // keep same lines showing if poss.
    then
      TopIndex := t
    else
    if n < t then
      TopIndex := n;

    Visible := True;

  end;//with

  shapes_saved := False;      // need a resave.

  // show the changed order..

  redraw_pad(True, False);
end;
//______________________________________________________________________________

procedure bgnd_move_down_button_click;

var
  t: integer;
  n: integer;

begin

  with bgnd_form.bgnd_shapes_listbox do begin
    n := ItemIndex;

    if (n > (Items.Count - 2)) or (n < 0) or (Items.Count < 2) then
      EXIT;

    t := TopIndex;                          // index of top displayed item.
    if (t < 0) or (t > (Items.Count - 1)) then
      EXIT;  // ???

    Items.Exchange(n, n + 1);

    n := n + 1;

    if ItemHeight < 1 then
      EXIT;    // div zero check.

    Visible := False;  // avoid flicker.
    if (n >= t) and (n < (t + (Height div ItemHeight))) then
      TopIndex := t;  // keep same lines showing if poss.
    Visible := True;

  end;//with

  shapes_saved := False;      // need a resave.

  // show the changed order..

  redraw_pad(True, False);
end;
//______________________________________________________________________________

procedure Tbgnd_form.move_to_top_buttonClick(Sender: TObject);

var
  n: integer;

begin
  with bgnd_shapes_listbox do begin

    n := ItemIndex;

    if (n < 1) or (n > (Items.Count - 1)) or (Items.Count < 2) then
      EXIT;

    Items.Move(n, 0);

    ItemIndex := 0;
  end;//with

  shapes_saved := False;      // need a resave.

  // show the changed order..

  redraw_pad(True, False);
end;
//______________________________________________________________________________

procedure Tbgnd_form.move_to_bottom_buttonClick(Sender: TObject);

var
  n: integer;

begin
  with bgnd_shapes_listbox do begin

    n := ItemIndex;

    if (n > (Items.Count - 2)) or (n < 0) or (Items.Count < 2) then
      EXIT;

    Items.Move(n, Items.Count - 1);

    ItemIndex := Items.Count - 1;
  end;//with

  shapes_saved := False;      // need a resave.

  // show the changed order..

  redraw_pad(True, False);
end;
//______________________________________________________________________________

procedure Tbgnd_form.move_up_buttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

// do first move here, then via timer if held down.
begin
  move_up_button.Tag := 1;
  Application.ProcessMessages;                        // to show button down or quick back up.
  if move_up_button.Tag = 1 then
    bgnd_move_up_button_click;
end;
//______________________________________________________________________________

procedure Tbgnd_form.move_up_buttonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  move_up_button.Tag := 0;
end;
//______________________________________________________________________________

procedure Tbgnd_form.move_down_buttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

// do first move here, then via timer if held down.
begin
  move_down_button.Tag := 1;
  Application.ProcessMessages;                        // to show button down or quick back up.
  if move_down_button.Tag = 1 then
    bgnd_move_down_button_click;
end;
//______________________________________________________________________________

procedure Tbgnd_form.move_down_buttonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  move_down_button.Tag := 0;
end;
//______________________________________________________________________________

procedure Tbgnd_form.all_90degs_acw_buttonClick(Sender: TObject);  // 213b

begin

  rotate_all_shapes(False, True, Pi / 2);  // do the calcs    True = include pictures

  shapes_saved := False;      // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;

procedure Tbgnd_form.all_90degs_cw_buttonClick(Sender: TObject);   // 213b

begin

  rotate_all_shapes(False, True, 0 - Pi / 2);  // do the calcs    True = include pictures

  shapes_saved := False;      // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;

procedure Tbgnd_form.reload_menu_entryClick(Sender: TObject);

begin
  load_shapes('', False, False, False);
end;
//______________________________________________________________________________

procedure Tbgnd_form.add_file_menu_entryClick(Sender: TObject);

begin
  load_shapes('', True, False, False);
end;
//______________________________________________________________________________

procedure Tbgnd_form.recent_files_menu_entryClick(Sender: TObject);

begin
  reload_recent_bgs;
end;
//______________________________________________________________________________

procedure Tbgnd_form.picture_shapes_help_menu_entryClick(Sender: TObject);

begin
  //
end;
//______________________________________________________________________________

procedure Tbgnd_form.background_shapes_help_menu_entryClick(Sender: TObject);

var
  hs: string;

begin
  hs := bgs_help1_str + add_options_help_str;
  help(0, hs, '');
end;
//______________________________________________________________________________

procedure Tbgnd_form.rename_menu_entryClick(Sender: TObject);

var
  n: integer;
  caption_str: string;
  big_label_str: string;

begin

  with bgnd_shapes_listbox do begin
    n := ItemIndex;

    if (Items.Count < 1) or (n < 0) or (n > (Items.Count - 1)) then
      EXIT;

    if Tbgshape(Items.Objects[n]).bgnd_shape.shape_code = 3      // label
    then begin
      caption_str := '    edit  label  text ...';
      big_label_str := insert_crlf_str('            Edit  Label  Text'
        + '||||Enter below the text for the currently selected label shape.'
        + '|||This text will also be the name of this background shape.'
        + '||||The entered text should not exceed 46 characters.');
    end
    else begin
      caption_str := '    rename  background  shape ...';
      big_label_str := insert_crlf_str('            Rename  Background  Shape'
        + '||||Enter below a new name for the currently selected background shape.'
        + '|||this shape is a  ' + type_label.Caption +
        '||||The name should not exceed 46 characters.');
    end;

    with math_form do begin
      Caption := caption_str;
      big_label.Caption := big_label_str;

      math_editbox.Text := Items.Strings[n];

      do_show_modal(math_form);     // 212a  ShowModal

      if ModalResult = mrOk then begin

        Tbgshape(Items.Objects[n]).bgnd_shape.shape_name := Trim(math_editbox.Text);
        Items.Strings[n] := Tbgshape(Items.Objects[n]).bgnd_shape.shape_name;

      end;
      Caption := '    ' + Application.Title;   // reset form caption.
    end;//with math_form
  end;//with listbox
end;
//______________________________________________________________________________

procedure Tbgnd_form.zoom_fit_all_shapes_menu_entryClick(Sender: TObject);

begin
  pad_form.fit_shapes_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tbgnd_form.zoom_fit_shape_menu_entryClick(Sender: TObject);

var
  n: integer;
  xmax, xmin, ymax, ymin: double;
  wl_factor: double;
  margin_factor: double;

  reduced_screeny: double;   // 216c

begin
  cancel_adjusts(False);

  // mods 216c for increased size of top toolbar ...

  if ABS(fy) < minfp then
    EXIT;   // ??  div 0

  reduced_screeny := screeny - pad_form.top_toolbar_panel.Height / ABS(fy);  // 217a

  if ABS(reduced_screeny) < minfp then
    EXIT;    // ??  div 0

  wl_factor := screenx / reduced_screeny;


  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    xmax := 0 - maxfp;      // init for limits check.
    ymax := 0 - maxfp;
    xmin := maxfp;
    ymin := maxfp;

    n := ItemIndex;

    with Tbgshape(Items.Objects[n]).bgnd_shape do begin

      if xmax < p1.x then
        xmax := p1.x;
      if xmin > p1.x then
        xmin := p1.x;

      if ymax < p1.y then
        ymax := p1.y;
      if ymin > p1.y then
        ymin := p1.y;

      if shape_code < 3 then begin
        if xmax < p2.x then
          xmax := p2.x;
        if xmin > p2.x then
          xmin := p2.x;

        if ymax < p2.y then
          ymax := p2.y;
        if ymin > p2.y then
          ymin := p2.y;
      end;
    end;//with

  end;//with

  margin_factor := 1.05;         // arbitrary 5% extra for margins.

  screenx := (xmax - xmin) * margin_factor;

  if screenx < ((ymax - ymin) * margin_factor * wl_factor) then
    screenx := (ymax - ymin) * margin_factor * wl_factor;

  if screenx < screenx_min then
    screenx := screenx_min; // minimum for screen width (max zoom in).
  if screenx > screenx_max then
    screenx := screenx_max; // maximum zoom out.

  // centralize on pad..

  zoom_offsetx := xmin - (screenx - (xmax - xmin)) / 2;
  if wl_factor > minfp then
    zoom_offsety := ymin - (screenx / wl_factor - (ymax - ymin)) / 2;

  pad_form.lock_scaling_menu_entry.Click;       // lock pad zoom.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tbgnd_form.draw_spacing_ring_menu_entryClick(Sender: TObject);

begin
  close_button.Click;
  pad_form.spacing_ring_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tbgnd_form.enter_dims_radiobuttonClick(Sender: TObject);

begin
  add_shape_button.Caption := 'add  shape';
end;
//______________________________________________________________________________

procedure Tbgnd_form.clicked_locs_radiobuttonClick(Sender: TObject);

begin
  add_shape_button.Caption := 'add  shape';
end;
//______________________________________________________________________________

procedure Tbgnd_form.by_drawing_radiobuttonClick(Sender: TObject);

begin
  if line_radio_button.Checked = True then
    add_shape_button.Caption := 'add  shapes'
  else
    add_shape_button.Caption := 'add  shape';
end;
//______________________________________________________________________________

procedure draw_picture_shapes(canv: TCanvas; xorg, yorg, dpmm: double; yheight: integer);

// draw any background picture shapes -- for crop/combine

// dpmm = dots per mm

var
  i: integer;
  x1, y1, x2, y2: double;
  raster_rect: TRect;
  move_to, line_to: TPoint;

begin
  with canv do begin

    for i := 0 to bgnd_form.bgnd_shapes_listbox.Items.Count - 1 do begin

      with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape do
      begin     // next shape.

        if shape_code <> -1 then
          CONTINUE;   // not a picture shape

        x1 := (p1.x - xorg) * dpmm;
        y1 := (p1.y - yorg) * dpmm;

        x2 := (p2.x - xorg) * dpmm;
        y2 := (p2.y - yorg) * dpmm;

        move_to.X := Round(x1);
        move_to.Y := yheight - Round(y1);

        line_to.X := Round(x2);
        line_to.Y := yheight - Round(y2);

        if check_limits(move_to, line_to) = True then begin
          try
            raster_rect.Left := move_to.X;
            raster_rect.Bottom := move_to.Y;
            raster_rect.Right := line_to.X;
            raster_rect.Top := line_to.Y;

            { OT-FIRST
                    if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile=True
                       then begin
                              pad_form.bgnd_shape_image.Picture.Graphic:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_metafile;
                              CopyMode:=cmSrcCopy;  // normal
                            end
                       else begin}
            pad_form.bgnd_shape_image.Picture.Graphic :=
              Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap;
            if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.show_transparent = True  // 0.93.a moved into file
            then
              CopyMode := cmSrcAnd    // (destination Canvas) transparent if on white background.
            else
              CopyMode := cmSrcCopy;  // normal
            { OT-FIRST end;}

            StretchDraw(raster_rect, pad_form.bgnd_shape_image.Picture.Graphic);
            // needs TGraphic parameter to work reliably.

            CopyMode := cmSrcCopy;   // reset normal for destination Canvas.

          except
            CopyMode := cmSrcCopy;       // reset normal for destination Canvas.

            Brush.Color := Pen.Color;    // stretch failed - draw hatched outline.
            Brush.Style := bsBDiagonal;
            Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
          end;//try
        end;

      end;//with shape

    end;//for next i shape

  end;//with canvas
end;
//__________________________________________________________________________________________

procedure Tbgnd_form.combine_pictures_menu_entryClick(Sender: TObject);

// combine any picture shapes wholly contained within the selected rectange to a single bitmap
// convert the selected rectangle to a new picture shape

var
  j, n: integer;
  xmax, xmin, ymax, ymin: double;

  new_bitmap: TBitmap;

  xdots_per_mm: double;
  max_xdots_per_mm: double;

  ydots_per_mm: double;
  max_ydots_per_mm: double;

  new_dpmm: double;

  pic_found: boolean;
  pic_count: integer;

begin
  cancel_adjusts(False);

  with bgnd_shapes_listbox do begin
    j := ItemIndex;
    with Items do begin

      if (Count < 1) or (j < 0) or (j > (Count - 1)) then
        EXIT;    // ??? menu should be disabled

      with Tbgshape(Objects[j]).bgnd_shape do begin

        if shape_code <> 1 then
          EXIT; // not a rectangle ??? menu should be disabled

        normalize_rectangle(p1, p2); // make sure p2 corner is top-right.

        xmax := p2.x;      // init for limits check.
        ymax := p2.y;
        xmin := p1.x;
        ymin := p1.y;

      end;//with rectangle

      pic_found := False;   // init ...
      pic_count := 0;
      max_xdots_per_mm := 0;
      max_ydots_per_mm := 0;

      for n := 0 to Count - 1 do begin

        with Tbgshape(Objects[n]).bgnd_shape do begin

          if shape_code <> -1 then
            CONTINUE; // not a picture shape

          pic_found := True;

          normalize_rectangle(p1, p2);

          // check if any part of picture shape is within rectangle...

          if p2.x < xmin then
            CONTINUE;
          if p1.x > xmax then
            CONTINUE;

          if p2.y < ymin then
            CONTINUE;
          if p1.y > ymax then
            CONTINUE;

          Inc(pic_count);  // found picture shape

          with Tbgshape(Objects[n]).bgimage.image_shape do begin
            xdots_per_mm := ABS(image_width / (p2.x - p1.x));
            if max_xdots_per_mm < xdots_per_mm then
              max_xdots_per_mm := xdots_per_mm;

            ydots_per_mm := ABS(image_height / (p2.y - p1.y));
            if max_ydots_per_mm < ydots_per_mm then
              max_ydots_per_mm := ydots_per_mm;
          end;//with

        end;//with shape

      end;//next shape

      if pic_found = False then begin
        if alert(6, '    crop / combine  picture  shapes',
          'There are no picture shapes currently defined.',
          '', '', '', 'more  information', 'cancel', '', 4) = 4 then
          companion_help('crop_combine.php');
        EXIT;
      end;

      if pic_count = 0 then begin
        if alert(6, '    crop / combine  picture  shapes',
          'No part of a picture shape falls within this rectangle.',
          '', '', '', 'more  information', 'cancel', '', 4) = 4 then
          companion_help('crop_combine.php');
        EXIT;
      end;

      new_bitmap := TBitmap.Create;

      new_dpmm := MAX(ABS(max_xdots_per_mm), ABS(max_ydots_per_mm));

      new_bitmap.Width := Round(ABS(new_dpmm * (xmax - xmin)));
      new_bitmap.Height := Round(ABS(new_dpmm * (ymax - ymin)));

      // draw background shapes on the new bitmap canvas...

      draw_picture_shapes(new_bitmap.Canvas, xmin, ymin, new_dpmm, new_bitmap.Height);

      with Tbgshape(Objects[j]) do begin

        bgnd_shape.shape_code := -1;              // change rectangle to a picture shape.
        bgnd_shape.show_transparent := False;     // not transparent.
        bgnd_shape.picture_is_metafile := False;  // not a metafile 213b

        bgimage := Tbgimage.Create;     // create new image

      end;//with

      with Tbgshape(Objects[j]).bgimage.image_shape do begin

        image_bitmap := new_bitmap;

        rotated_bitmap := TBitmap.Create;

        { OT-FIRST
        image_metafile:=TMetafile.Create;     // created but not used  213b
        rotated_metafile:=TMetafile.Create;   // created but not used  213b
        }

        rotated_picture := TPicture.Create;

        image_width := image_bitmap.Width;
        image_height := image_bitmap.Height;

      end;//with

      Strings[j] := 'combined picture';                                   // name in list.
      Tbgshape(Objects[j]).bgnd_shape.shape_name := 'combined picture';   // name in file.

      draw_bg_shapes(pad_form.Canvas, j, clRed);
      // show new bitmap and highlight in red, directly on the pad.


      if alert(4, '    delete  cropped / combined  picture  shapes ?',
        'Do you now want to delete the original picture shapes contained in this new picture shape?',
        '', '', '', '', 'no  thanks', 'yes  please  -  delete  originals', 0) = 6 then
      begin

        Strings[j] := '_ _ _ _ _';   // temp modify new name

        repeat

          pic_found := False;    // init ...

          for n := 0 to Count - 1 do begin

            if Strings[n] = '_ _ _ _ _' then
              CONTINUE;   // this is the cropped one

            with Tbgshape(Objects[n]).bgnd_shape do begin

              if shape_code <> -1 then
                CONTINUE;      // not a picture shape

              normalize_rectangle(p1, p2);

              // check if any part of picture shape is within rectangle...

              if p2.x < xmin then
                CONTINUE;
              if p1.x > xmax then
                CONTINUE;

              if p2.y < ymin then
                CONTINUE;
              if p1.y > ymax then
                CONTINUE;

            end;//with shape

            pic_found := True;

            BREAK;   // go delete this one

          end;//next shape

          if pic_found = True then begin
            free_shape_object(n);
            // free any picture bitmaps and the shape object.
            Delete(n);             // delete the entry.
          end;

        until ((pic_found = False) or (Count = 0));


        for n := 0 to Count - 1 do begin

          if Strings[n] = '_ _ _ _ _' then begin
            Strings[n] := 'combined picture';   // this is the cropped one
            BREAK;
          end;
        end;//next

      end;

    end;//with Items
  end;//with listbox

  shapes_saved := False;

  shapes_current_state;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tbgnd_form.shapes_help_labelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if shapes_help_label.Font.Color <> clRed then
    shapes_help_label.Font.Color := clRed;
end;
//______________________________________________________________________________

procedure Tbgnd_form.new_panelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if shapes_help_label.Font.Color <> clBlue then
    shapes_help_label.Font.Color := clBlue;
end;
//______________________________________________________________________________

procedure Tbgnd_form.shapes_help_labelClick(Sender: TObject);

begin
  help(0, add_options_help_str, '');
end;
//______________________________________________________________________________

procedure Tbgnd_form.auto_fit_picture_buttonClick(Sender: TObject);

begin
  auto_fit_picture;
end;
//______________________________________________________________________________

procedure Tbgnd_form.apply_buttonClick(Sender: TObject);     // 214a

// update shape without changing dims...

var
  now_shape: Tbgnd_shape;
  n: integer;
  old_shape_code: integer;

begin
  with bgnd_shapes_listbox do begin
    n := ItemIndex;

    if (n < 0) or (n > (Items.Count - 1)) then begin
      show_modal_message('No background shape is currently selected in the list.');
      EXIT;
    end;

    now_shape := Tbgshape(Items.Objects[n]).bgnd_shape;      // get existing data.

    with now_shape do begin

      old_shape_code := shape_code;

      // now set up new shape data...

      if name_editbox.Text <> '' then
        shape_name := name_editbox.Text;  // new name or label

      shape_code := 0;
      // default line. 0=line, 1=rectangle, 2=circle, 3=label, 4=target, -1=picture.

      if target_radio_button.Checked = True then
        shape_code := 4;
      if label_radio_button.Checked = True then
        shape_code := 3;
      if circle_radio_button.Checked = True then
        shape_code := 2;
      if rect_radio_button.Checked = True then
        shape_code := 1;
      if picture_radio_button.Checked = True then
        shape_code := -1;

      if (shape_code <> 4) and (old_shape_code = 4) then
        p2.x := 0;  // old was target_arm length

      case shape_code of
        -1, 1, 2:
          normalize_rectangle(p1, p2);            // new picture, rectangle, circle
        0:
          normalize_line(p1, p2);                 // new line
        3: begin
          p2.x := 0;
          p2.y := 0;
        end;           // new label
        4: begin
          p2.x := target_arm;
          p2.y := 0;
        end;  // new target

      end;//case

      shape_style := 0;
      // default transparent.  0=transparent, 1=blank/solid, 2=cross-hatched or dotted;

      case shape_code of
        0:
          if dotted_radio_button.Checked = True then
            shape_style := 2;

        1, 2: begin
          if solid_infill_radio_button.Checked = True then
            shape_style := 1;
          if hatched_radio_button.Checked = True then
            shape_style := 2;
        end;
      end;//case

      if (shape_code <> -1) and (old_shape_code = -1)             // was a picture, but not now.
      then begin
        // so free the image (but not the shape object).
        if Tbgshape(Items.Objects[n]).bgimage <> nil    // ???
        then begin
          Tbgshape(Items.Objects[n]).bgimage.image_shape.image_bitmap.Free;
          // free the bitmaps.
          Tbgshape(Items.Objects[n]).bgimage.image_shape.rotated_bitmap.Free;

          { OT-FIRST
                            Tbgshape(Items.Objects[n]).bgimage.image_shape.image_metafile.Free;    // 213b
                            Tbgshape(Items.Objects[n]).bgimage.image_shape.rotated_metafile.Free;  // 213b
                            }

          Tbgshape(Items.Objects[n]).bgimage.image_shape.rotated_picture.Free;
          Tbgshape(Items.Objects[n]).bgimage.Free;
          Tbgshape(Items.Objects[n]).bgimage := nil;
          // and free the image object. 3-2-01.
        end;
      end;

      if (shape_code = -1) and (old_shape_code <> -1)        // changed to a picture - get picture.
      then begin
        show_transparent := False;
        picture_is_metafile := False;

        with Tbgshape(Items.Objects[n]) do begin

          bgimage := Tbgimage.Create;     // create new image  3-2-01.

          with bgimage.image_shape do begin

            image_bitmap := TBitmap.Create;
            rotated_bitmap := TBitmap.Create;

            { OT-FIRST
                    image_metafile:=TMetafile.Create;    // 213b
                    rotated_metafile:=TMetafile.Create;  // 213b
                    }

            rotated_picture := TPicture.Create;

            try
              image_bitmap.LoadFromFile(Config.GetFilePath(csfiEmptyPic));

              image_width := image_bitmap.Width;
              image_height := image_bitmap.Height;
            except
              image_bitmap.Width := 200;            // arbitrary.
              image_bitmap.Height := 150;           // arbitrary.

              image_width := image_bitmap.Width;
              image_height := image_bitmap.Height;

              with image_bitmap.Canvas do begin     // blank the picture area...
                Brush.Color := clWhite;
                Brush.Style := bsSolid;
                FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
              end;//with

            end;//try

          end;//with
        end;//with
      end;//changed to picture shape

    end;//with

    Tbgshape(Items.Objects[n]).bgnd_shape := now_shape;      // put new data in list.

    ItemIndex := n;   // Delphi bug? - changing the strings changes this.

  end;//with

  shapes_saved := False;      // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tbgnd_form.add_picture_shape_buttonClick(Sender: TObject);    // 214a

const
  pic_shapes_help_str: string = 'php/401   `0background  images`9' +
    '||Sorry, notes not yet written.' +
    '||For more information please ask on the <A HREF="go_to_templot_club.85a">Templot&nbsp;Club</A> user forum.';

var
  new_shape: Tbgnd_shape;
  n: integer;
  i: integer;

begin

  if drop_msg_pref = False then begin

    alert_box.preferences_checkbox.Checked := False;
    alert_box.preferences_checkbox.Show;

    repeat
      i := alert(2, 'php/401    background  images',
        'Images such as scanned maps, layout plans, sketches and aerial photographs can be displayed on the trackpad background as a guide to your track planning.' + '||Metafile vector images in EMF format created by CAD drawing and graphics software can also be displayed on the trackpad. Vector images can zoom smoothly without becoming fuzzy or blocky.' + '||green_panel_begin tree.gif  Background images are called `0picture shapes`3 and are part of the `0background shapes`3 functions.' + '||A picture shape is a rectangle outline, which contains an image. The container rectangle can be set to any size, and the image will be stretched to fit.' + '||You can have as many picture shapes as you wish.green_panel_end' + '|An easy way to create a new picture shape is to drag and drop an image from your pictures folder onto the trackpad. The container rectangle is then created automatically.' + '||Dropped images can be in any of these file formats:  JPG , PNG , GIF , BMP , EMF , ICO .' + '||If you click `0I want to drop an image file on the trackpad`z below, this dialog window will close and your Windows pictures folder will open so that you easily drag and drop an image onto the trackpad.' + '|<HR NOSHADE SIZE=1 COLOR=GRAY>' + 'Alternatively if you click `0continue`1 below an empty picture shape container will first be created.' + '||After which you will be able to fill it by selecting an image file, or by pasting a copied image, or by directly scanning the image.' + '|<HR NOSHADE SIZE=1 COLOR=GRAY>' + 'Whichever method you choose, after the picture shape has been created you can adjust its size and position to your requirements.' + '||Or Templot0 can adjust it automatically for you.' + '||For more information and tutorial videos click the white bar.', '', '', 'more  information', 'I  want  to  drop  an  image  file  on  the  trackpad', 'cancel', 'continue', 3);

      if i = 3 then
        alert_help(0, pic_shapes_help_str, '');

    until i <> 3;

    drop_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
    alert_box.preferences_checkbox.Hide;

    case i of

      4: begin
        open_MyPictures;
        EXIT;
      end;

      5:
        EXIT;

    end;//case

  end;

  bgnd_form.solid_radio_button.Enabled := False;
  bgnd_form.dotted_radio_button.Enabled := False;

  bgnd_form.picture_radio_button.Checked := True;

  bgnd_form.bgnd_shapes_listbox.ItemIndex := -1;  // deselect any existing

  bgnd_form.by_drawing_radiobutton.Checked := True;


  with new_shape do begin

    shape_code := -1;   // -1=picture
    shape_style := 0;   // not used

    wrap_offset := 0;               // default
    show_transparent := False;      // default
    picture_is_metafile := False;   // default

    shape_name := name_editbox.Text;

    if shape_name = '' then
      shape_name := empty_picture_str;

    hide_bits := 0;  // normal visibility
    option_bits := 0;     // byte;

    p1.x := mouse_x(Screen.Width * 2 div 3) - Random(Round(screenx / 5));
    // arbitrary  random mod in case multiple overlaid shapes
    p1.y := mouse_y(Screen.Width * 2 div 3, Screen.Height div 2) - Random(Round(screeny / 5));
    // arbitrary

    p2.x := p1.x + screenx / 3;     // arbitrary
    p2.y := p1.y + screenx * 9 / 48;  // 16:9 aspect ratio
  end;//with

  with bgnd_form.bgnd_shapes_listbox do begin

    n := Items.AddObject(new_shape.shape_name, Tbgshape.Create);
    // create and insert a new entry in the shapes list.

    Tbgshape(Items.Objects[n]).bgnd_shape := new_shape;      // put data in list.

    ItemIndex := n;                                          // make it current.

    with Tbgshape(Items.Objects[n]) do begin

      bgimage := Tbgimage.Create;     // create new image  3-2-01.

      with bgimage.image_shape do begin

        image_bitmap := TBitmap.Create;
        rotated_bitmap := TBitmap.Create;

        { OT-FIRST
        image_metafile:=TMetafile.Create;    // 213b
        rotated_metafile:=TMetafile.Create;  // 213b
        }

        rotated_picture := TPicture.Create;

        try
          image_bitmap.LoadFromFile(Config.GetFilePath(csfiEmptyPic));
        except
          image_bitmap.Width := 200;            // arbitrary.
          image_bitmap.Height := 150;           // arbitrary.

          with image_bitmap.Canvas do begin     // blank the picture area...
            Brush.Color := clWhite;
            Brush.Style := bsSolid;
            FillRect(Rect(0, 0, image_bitmap.Width - 1, image_bitmap.Height - 1));
          end;//with

        end;//try

        image_width := image_bitmap.Width;
        image_height := image_bitmap.Height;

      end;//with
    end;//with
  end;//with listbox

  shapes_saved := False;      // need a resave.
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.

  redraw(False);            // changes to default pad cursor.

  alert_box.drop_panel.Visible := True;
  alert_box.drop_image.Visible := False;  // hide any previous dropped image until new one dropped

  i := alert(4, 'php/401    load  image  for  picture  shape',
    'You have now created a new empty picture shape. You may need to move this dialog window to see it.'
    + '||A picture shape is a container for an image, which is stretched to fit within it. Templot0 now needs the image which is to be contained within this one. You can provide the image in several ways.' + '||The easiest way is to drag and drop it into the box above from your pictures folder. If you make a mistake simply drop the correct image over the wrong one. Then click the green bar below.' + ' Dropped files can be in any of JPG, PNG, GIF, BMP, EMF, ICO image formats.' + '||Or you can select an image file on your computer in the usual way, or paste an image which you have copied, or if you have a scanner connected on your system you can scan the image directly now.' + '||After the image has been loaded into this picture shape, Templot0 can help you adjust the size and position of this picture shape to match your track plan.' + '||Because this is a new empty picture shape, its height will be adjusted initially to match the loaded image.' + '||If you decide to leave it empty, an image can be loaded into it later.' + '||Alternatively you may prefer to delete this picture shape, and create a new one by dragging and dropping an image file directly onto the trackpad.' + '||How do you want to proceed?      If you are unsure click the green bar.', 'delete  picture  shape', 'scan  into  picture  shape  now', 'paste  image  -  I  have  copied  an  image', 'I  have  a  metafile  ( EMF  file )', 'cancel  -  leave  picture  empty  for  now', 'I  have  an  image  file  ( JPG , PNG , GIF , BMP , ICO )   ', 0);

  alert_box.drop_panel.Visible := False;  // for next time

  if i = 5 then
    EXIT;

  if alert_box.drop_image.Visible = True then
    reload_picture_image(False, False, False, True, True)
  // dropped    // scanning,pasting,meta,dropped,adjust_aspect
  else begin
    case i of

      1: begin
        delete_menu_entry.Click;
        EXIT;
      end;

      2: begin  // scan
        show_modal_message('If the image is a map or track plan for which you know the scale,'
          + ' make a note of the scanner DPI setting when your scanner dialogs appear.'
          + #13 + #13 +
          'If you wish Templot0 will then be able to scale the image for you to match your model scale and gauge.');
        reload_picture_image(True, False, False, False, True);
        // scanning,pasting,meta,dropped,adjust_aspect
      end;

      3: begin  // paste
        if Clipboard.HasFormat(CF_BITMAP) = False then begin
          show_modal_message('No copied image is avaiable.'
            + #13 + #13 +
            'You must copy an image from somewhere on your computer before using the paste option.'
            + #13 + #13 +
            'That usually means right-clicking on the image itself and selecting Copy on the menu which appears.');
        end
        else
          reload_picture_image(False, True, False, False, True);   // scanning,pasting,meta,dropped,adjust_aspect
      end;

      4:
        if reload_picture_image(False, False, True, False, True) = False then
          EXIT;  // metafile   // scanning,pasting,meta,dropped,adjust_aspect

      //5: EXIT;

      6:
        if reload_picture_image(False, False, False, False, True) = False then
          EXIT; // file    // scanning,pasting,meta,dropped,adjust_aspect

    end;//case

  end;

  shapes_saved := False;      // need a resave.
  shapes_current_state;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(False);            // update pad now

  do_bgnd(True);  // show background shapes dialog with modify shape tab active

  if yellow_msg_pref = False then begin
    bgnd_form.new_picture_shape1.Visible := True;
    // show him which buttons to use (yellow patch)
    bgnd_form.new_picture_shape2.Visible := True;
    bgnd_form.new_picture_shape3.Visible := True;

    if help(0, '         `0Image  Loaded`9||An ' + picture_buttons_str,
      'don''t  show  this  again') = 1 then
      yellow_msg_pref := True;
  end;

end;
//______________________________________________________________________________

procedure Tbgnd_form.go_to_my_documents_menu_entryClick(Sender: TObject);

begin
  open_MyDocuments;
end;
//______________________________________________________________________________

procedure Tbgnd_form.open_mystuff_buttonClick(Sender: TObject);

begin
  open_MyPictures;
end;
//______________________________________________________________________________

procedure Tbgnd_form.hide_tick_imageClick(Sender: TObject);

begin
  show_tick_image.Visible := True;    // now show ticked it

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[bgnd_shapes_listbox.ItemIndex]).bgnd_shape do
  begin

    hide_bits := hide_bits and $FE;   // toggle bit off, show shape

  end;//with

  shapes_current_state;

  do_rollback := False;  // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//____________________________________________________

procedure Tbgnd_form.show_tick_imageClick(Sender: TObject);

begin
  show_tick_image.Visible := False;  // reveal unticked

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[bgnd_shapes_listbox.ItemIndex]).bgnd_shape do
  begin

    hide_bits := hide_bits or $01;   // toggle bit on, hide shape

  end;//with

  shapes_current_state;

  do_rollback := False;  // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//____________________________________________________

procedure Tbgnd_form.show_on_trackpad_staticClick(Sender: TObject);

begin
  show_tick_image.Visible := not show_tick_image.Visible;

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[bgnd_shapes_listbox.ItemIndex]).bgnd_shape do
  begin

    if show_tick_image.Visible = False then
      hide_bits := hide_bits or $01    // toggle bit on, hide shape
    else
      hide_bits := hide_bits and $FE;  // toggle bit off, show shape

  end;//with

  shapes_current_state;

  do_rollback := False;    // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tbgnd_form.hide_output_tick_imageClick(Sender: TObject);

begin
  show_output_tick_image.Visible := True;    // now show ticked it

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[bgnd_shapes_listbox.ItemIndex]).bgnd_shape do
  begin

    hide_bits := hide_bits and $FD;   // toggle bit off, show shape

  end;//with

  shapes_current_state;
end;
//___________________________________

procedure Tbgnd_form.show_output_tick_imageClick(Sender: TObject);

begin
  show_output_tick_image.Visible := False;  // reveal unticked

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[bgnd_shapes_listbox.ItemIndex]).bgnd_shape do
  begin

    hide_bits := hide_bits or $02;   // toggle bit on, hide shape

  end;//with

  shapes_current_state;
end;
//___________________________________

procedure Tbgnd_form.show_on_output_staticClick(Sender: TObject);

begin
  show_output_tick_image.Visible := not show_output_tick_image.Visible;

  with Tbgshape(bgnd_shapes_listbox.Items.Objects[bgnd_shapes_listbox.ItemIndex]).bgnd_shape do
  begin

    if show_output_tick_image.Visible = False then
      hide_bits := hide_bits or $02    // toggle bit on, hide shape
    else
      hide_bits := hide_bits and $FD;  // toggle bit off, show shape

  end;//with

  shapes_current_state;
end;
//______________________________________________________________________________

procedure Tbgnd_form.bgnd_shapes_listboxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

var
  mark_str: string;
  text_colour: TColor;

begin
  if (Index < 0) or (Index > (bgnd_shapes_listbox.Items.Count - 1)) then
    EXIT;  // ???

  with (Control as TListBox).Canvas do begin

    if (odSelected in State) = True then begin
      Brush.Color := $00FF7700;   // light blue
      text_colour := clWhite;
    end
    else begin
      Brush.Color := bgnd_shapes_listbox.Color;
      text_colour := clBlack;
    end;

    mark_str := ' ';

    try
      with Tbgshape((Control as TListBox).Items.Objects[Index]).bgnd_shape do begin

        if (hide_bits and $01) <> 0
        // 214a  byte,  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both
        then
          mark_str := mark_str + '! ';  // hidden on trackpad

        if (hide_bits and $02) <> 0
        // 214a  byte,  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both
        then
          mark_str := mark_str + '• ';  // hidden on output

        if (hide_bits and $03) <> 0
        // 214a  byte,  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both
        then begin
          if text_colour = clWhite then
            text_colour := clSilver
          else
            text_colour := clGray;
        end;
      end;//with

    except
      // do nothing   -- fixes strange bug see Club message ref: 27596    program loops through here several times under condition mentioned there. 223c
    end;//try

    Font.Color := text_colour;

    TextOut(Rect.Left, Rect.Top, mark_str + (Control as TListBox).Items.Strings[Index]);

  end;//with
end;
//______________________________________________________________________________

procedure Tbgnd_form.saved_jpg_quality_menu_entryClick(Sender: TObject);

begin
  control_room_form.jpg_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tbgnd_form.calculate_map_size_menu_entryClick(Sender: TObject);

const
  map_str: string = '    `0calculate size of map screenshot`9' +
    '||This calculator function requires that the map screenshot was captured with the page zoom in the browser set for dot-for-dot image display.' + '||This may be less than the 100% page zoom setting if your Windows screen dpi setting is higher than 96dpi.' + '||If the page zoom was not dot-for-dot the calculation result will be incorrect.';
var
  n: integer;
  od: Toutdim;
  map_pixels, zoom_level: integer;
  lat_degs: double;
  map_width_mm: double;

begin
  help(0, map_str, '');

  map_pixels := 800;
  zoom_level := 19;
  lat_degs := 54.0;

  putdim('This information is shown below the shapes list for the selected picture shape.', 0,
    'width  of  map  image  in  pixels', map_pixels, True, True, True, False);
  // no neg, no preset, no zero, don't terminate on zero.
  putdim('This information is part of the the map URL.', 0, 'zoom  level  of  captured  image',
    zoom_level, True, True, True, False);
  // no neg, no preset, no zero, don't terminate on zero.
  n := putdim('This information is part of the the map URL.', 3,
    'latitude  at  centre  of  map  in  degrees', lat_degs, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.

  if n <> 2 then
    EXIT;

  if getdims('calculate  map  size', map_str, bgnd_form, n, od) = True then begin
    map_pixels := Round(od[0]);
    zoom_level := Round(od[1]);
    lat_degs := od[2];
  end
  else
    EXIT;  // cancelled

  map_width_mm := ABS(156543033.928 * COS(lat_degs * Pi / 180) * map_pixels * scale / 304.8 / Power(2, zoom_level));
  //resolution = 156543.033928 metres/pixel * cos(latitude) / (2 ^ zoomlevel)
  // 156543.03392804096153584694438047

  show_modal_message('The width of the map at your current scale is ' + FormatFloat(
    '0.00', map_width_mm) + ' mm' + #13 + #13 +
    'Assuming the page zoom in the browser was set for dot-for-dot image display.');
end;
//______________________________________________________________________________

procedure Tbgnd_form.bgnd_map_menuClick(Sender: TObject);

begin
  //do_open_source_bang('MAP LOADER');  // OT-FIRST
  //{ OT-FIRST
  map_loader_form.Show;
  //}
end;
//______________________________________________________________________________
{ OT-FIRST

procedure Tbgnd_form.convert_24bit_menu_entryClick(Sender: TObject);     // 215b

var
  n:integer;

begin
  with bgnd_shapes_listbox do begin
    n:=ItemIndex;
    with Items do begin

      if (Count<1) or (n<0) or (n>(Count-1)) then EXIT;

      with Tbgshape(Objects[n]) do begin

        if (bgnd_shape.shape_code<>-1)                // not a picture shape.
        or (bgnd_shape.shape_name=empty_picture_str)
        or (bgimage=nil)                              // no existing bitmap.
           then begin
                  ShowMessage(bgnd_shape.shape_name+' : no image content to convert.');
                  EXIT;
                end;

        if bgimage.image_shape.image_bitmap.PixelFormat=pf24bit
           then begin
                  ShowMessage(bgnd_shape.shape_name+' : image colour depth is already 24-bit.');
                  EXIT;
                end;

        try
          bgimage.image_shape.image_bitmap.PixelFormat:=pf24bit;
        except
          ShowMessage('Sorry, unable to convert image colour depth to 24-bit for '+bgnd_shape.shape_name);
          EXIT;
        end;//try

      end;//with shape
    end;//with items
  end;//with list

  redraw(True);
end;
//______________________________________________________________________________
}

procedure Tbgnd_form.all_pictures_transparent_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    for n := 0 to (Items.Count - 1) do begin

      with Tbgshape(Items.Objects[n]).bgnd_shape do begin
        if shape_code <> -1 then
          CONTINUE;
        show_transparent := True;
      end;//with
    end;//next
  end;//with

  shapes_saved := False;
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tbgnd_form.no_pictures_transparent_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  with bgnd_form.bgnd_shapes_listbox do begin

    if Items.Count < 1 then
      EXIT;

    for n := 0 to (Items.Count - 1) do begin

      with Tbgshape(Items.Objects[n]).bgnd_shape do begin
        if shape_code <> -1 then
          CONTINUE;
        show_transparent := False;
      end;//with
    end;//next
  end;//with

  shapes_saved := False;
  shapes_current_state;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

end.
