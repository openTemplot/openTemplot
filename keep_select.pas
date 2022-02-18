
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


unit keep_select;

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ExtCtrls, Printers, Buttons, ComCtrls,
  LCLtype,     // OT-FIRST   needed for (odSelected in State) for owner-draw stuff

  { OT-FIRST , WPPDFPRP,
  WPPDFR1, WPPDFR2,} Htmlview,
  shoved_timber,
  pad_unit,
  HtmlGlobals;      // moved 290a

type

  { Tkeep_form }

  Tkeep_form = class(TForm)
    keep_panel: TPanel;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    chat_panel: TPanel;
    keep_form_datestamp_label: TLabel;
    keep_image: TImage;
    MenuItem1: TMenuItem;
    import_mecbox_menu_entry: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    export_mecbox_menu_entry: TMenuItem;
    MenuItem5: TMenuItem;
    transfer_menu: TMenuItem;
    save_dialog: TSaveDialog;
    load_dialog: TOpenDialog;
    colour_panel: TPanel;
    colour_patch: TImage;
    size_updown: TUpDown;
    copy_panel: TPanel;
    copy_to_pad_button: TButton;
    to_control_label: TLabel;
    escape_panel: TPanel;
    escape_button: TButton;
    gauge_panel: TPanel;
    store_current_as_unused_button: TButton;
    ref_panel: TPanel;
    scale_radio_panel: TPanel;
    Label1: TLabel;
    fit_radio_button: TRadioButton;
    lock_radio_button: TRadioButton;
    show_list_button: TButton;
    list_panel: TPanel;
    print_list_button: TButton;
    show_dwg_button: TButton;
    Label2: TLabel;
    lock_at_button: TButton;
    distortion_warning_panel: TPanel;
    keeps_menu_bar: TMainMenu;
    disk_menu: TMenuItem;
    reload_menu_entry: TMenuItem;
    save_all_menu_entry: TMenuItem;
    N2: TMenuItem;
    export_dxf_menu_entry: TMenuItem;
    hide_menu_entry: TMenuItem;
    edit_menu: TMenuItem;
    rename_menu_entry: TMenuItem;
    delete_menu_entry: TMenuItem;
    clear_menu_entry: TMenuItem;
    options_menu: TMenuItem;
    open_on_keep_menu_entry: TMenuItem;
    keep_blind_menu_entry: TMenuItem;
    colours_menu: TMenuItem;
    dwg_font_menu_entry: TMenuItem;
    timber_colour_menu_entry: TMenuItem;
    rail_colour_menu_entry: TMenuItem;
    grid_colour_menu_entry: TMenuItem;
    paper_colour_menu_entry: TMenuItem;
    list_colour_menu_entry: TMenuItem;
    box_title_menu_entry: TMenuItem;
    help_menu: TMenuItem;
    what_next_menu_entry: TMenuItem;
    to_from_bgnd_panel: TPanel;
    copy_or_wipe_background_button: TButton;
    close_on_copy_menu_entry: TMenuItem;
    add_file_menu_entry: TMenuItem;
    box_menu: TMenuItem;
    print_list_menu_entry: TMenuItem;
    N8: TMenuItem;
    box_program_panel_menu_entry: TMenuItem;
    blind_copy_menu_entry: TMenuItem;
    save_all_button: TButton;
    onkeep1: TMenuItem;
    on_make_current_menu_entry: TMenuItem;
    N5: TMenuItem;
    N1: TMenuItem;
    copy_in_position_menu_entry: TMenuItem;
    copy_onto_notch_menu_entry: TMenuItem;
    copy_onto_datum_menu_entry: TMenuItem;
    on_reload_or_add_menu_entry: TMenuItem;
    update_background_menu_entry: TMenuItem;
    reload_ignore_bgnd_menu_entry: TMenuItem;
    reload_button: TButton;
    keepform_listbox: TListBox;
    delete_unused_menu_entry: TMenuItem;
    copy_name_menu_entry: TMenuItem;
    use_current_name_menu_entry: TMenuItem;
    new_name_menu_entry: TMenuItem;
    N6: TMenuItem;
    N9: TMenuItem;
    N11: TMenuItem;
    rename_button: TButton;
    print_info_menu_entry: TMenuItem;
    print_all_info_menu_entry: TMenuItem;
    print_all_memo_menu_entry: TMenuItem;
    print_all_text_menu_entry: TMenuItem;
    all_to_bgnd_menu_entry: TMenuItem;
    undo_delete_menu_entry: TMenuItem;
    onreloadonly1: TMenuItem;
    group_select_groupbox: TGroupBox;
    deselect_all_button: TButton;
    select_all_button: TButton;
    select_toggle_button: TButton;
    add_file_button: TButton;
    save_group_button: TButton;
    box_empty_label: TLabel;
    updown_panel: TPanel;
    down_button: TSpeedButton;
    up_button: TSpeedButton;
    bottom_button: TSpeedButton;
    top_button: TSpeedButton;
    slider_bar_shape: TShape;
    slider_panel: TPanel;
    copy_wipe_label: TLabel;
    slider_ref_label: TLabel;
    this_is_panel: TPanel;
    this_template_label: TLabel;
    template_numbering_panel: TPanel;
    total_label: TLabel;
    save_group_menu_entry: TMenuItem;
    wipe_all_menu_entry: TMenuItem;
    N12: TMenuItem;
    newclearall1: TMenuItem;
    N10: TMenuItem;
    new_clear_all_menu_entry: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    edit_memo_menu_entry: TMenuItem;
    N7: TMenuItem;
    gauge_change_menu_entry: TMenuItem;
    N15: TMenuItem;
    title_menu_entry: TMenuItem;
    N16: TMenuItem;
    move_down_button: TSpeedButton;
    move_up_button: TSpeedButton;
    box_colour_menu_entry: TMenuItem;
    flag_shape: TShape;
    group_menu: TMenuItem;
    select_menu_entry: TMenuItem;
    select_all_menu_entry: TMenuItem;
    clear_selections_menu_entry: TMenuItem;
    N17: TMenuItem;
    wipe_group_menu_entry: TMenuItem;
    rebuild_group_menu_entry: TMenuItem;
    group_save_menu_entry: TMenuItem;
    copy_group_menu_entry: TMenuItem;
    delete_group_menu_entry: TMenuItem;
    N18: TMenuItem;
    invert_selections_menu_entry: TMenuItem;
    N19: TMenuItem;
    group_select_shape: TShape;
    template_number_shape: TShape;
    template_number_label: TLabel;
    restore_previous_menu_entry: TMenuItem;
    slider_number_label: TLabel;
    slider_shape: TShape;
    N20: TMenuItem;
    box_quit_menu_entry: TMenuItem;
    select_all_bgnd_menu_entry: TMenuItem;
    select_all_unused_menu_entry: TMenuItem;
    save_bgnd_menu_entry: TMenuItem;
    save_unused_menu_entry: TMenuItem;
    close_on_reload_menu_entry: TMenuItem;
    show_on_reload_menu_entry: TMenuItem;
    onadd1: TMenuItem;
    add_to_group_menu_entry: TMenuItem;
    add_new_group_menu_entry: TMenuItem;
    add_ignore_group_menu_entry: TMenuItem;
    N22: TMenuItem;
    load_all_menu_entry: TMenuItem;
    ignore_unused_menu_entry: TMenuItem;
    delete_button: TButton;
    read_info_button: TButton;
    rebuild_button: TButton;
    onrebuild1: TMenuItem;
    N23: TMenuItem;
    timbering_as_stored_menu_entry: TMenuItem;
    timbering_as_control_menu_entry: TMenuItem;
    toggle_all_bgnd_menu_entry: TMenuItem;
    toggle_group_bgnd_menu_entry: TMenuItem;
    restore_prior_previous_menu_entry: TMenuItem;
    group_linked_warning_label: TLabel;
    delete_all_except_group_menu_entry: TMenuItem;
    add_jotter_to_memo_menu_entry: TMenuItem;
    undo_clear_menu_entry: TMenuItem;
    N24: TMenuItem;
    help_button: TButton;
    Shape1: TShape;
    rebuild_template_menu_entry: TMenuItem;
    N25: TMenuItem;
    rebuild_all_menu_entry: TMenuItem;
    add_library_button: TButton;
    add_library_menu_entry: TMenuItem;
    N26: TMenuItem;
    delete_library_templates_menu_entry: TMenuItem;
    library_label: TLabel;
    save_library_menu_entry: TMenuItem;
    make_library_template_menu_entry: TMenuItem;
    sort_library_templates_last_menu_entry: TMenuItem;
    obtain_to_control_menu_entry: TMenuItem;
    N27: TMenuItem;
    obtain_switch_menu_entry: TMenuItem;
    obtain_plain_track_menu_entry: TMenuItem;
    recent_popup_menu: TPopupMenu;
    recent_1_popup_entry: TMenuItem;
    recent_2_popup_entry: TMenuItem;
    recent_3_popup_entry: TMenuItem;
    recent_4_popup_entry: TMenuItem;
    recent_5_popup_entry: TMenuItem;
    recent_6_popup_entry: TMenuItem;
    recent_7_popup_entry: TMenuItem;
    recent_8_popup_entry: TMenuItem;
    recent_9_popup_entry: TMenuItem;
    reload_recent_file_menu_entry: TMenuItem;
    recent_files_menu_entry: TMenuItem;
    add_recent_file_menu_entry: TMenuItem;
    mru_caption_popup_entry: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    clear_mru_list_popup_entry: TMenuItem;
    file_label_panel: TPanel;
    box_file_label: TLabel;
    N31: TMenuItem;
    cancel_mru_popup_entry: TMenuItem;
    restore_previous_mru_popup_entry: TMenuItem;
    restore_prior_previous_mru_popup_entry: TMenuItem;
    N32: TMenuItem;
    reload_add_mru_popup_entry: TMenuItem;
    move_to_top_button: TSpeedButton;
    move_to_bottom_button: TSpeedButton;
    sort_group_templates_first_menu_entry: TMenuItem;
    find_template_menu_entry: TMenuItem;
    find_next_menu_entry: TMenuItem;
    find_and_group_menu_entry: TMenuItem;
    N21: TMenuItem;
    N33: TMenuItem;
    delete_t55_templates_menu_entry: TMenuItem;
    trackbed_edges_as_stored_menu_entry: TMenuItem;
    trackbed_edges_as_control_menu_entry: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    centre_lines_as_stored_menu_entry: TMenuItem;
    centre_lines_as_control_menu_entry: TMenuItem;
    box_info_menu_entry: TMenuItem;
    N34: TMenuItem;
    rail_section_data_as_stored_menu_entry: TMenuItem;
    rail_section_data_as_control_menu_entry: TMenuItem;
    N35: TMenuItem;
    N36: TMenuItem;
    help1: TMenuItem;
    N37: TMenuItem;
    obtain_to_control_help_menu_entry: TMenuItem;
    N38: TMenuItem;
    help2: TMenuItem;
    N39: TMenuItem;
    help3: TMenuItem;
    N40: TMenuItem;
    help4: TMenuItem;
    N41: TMenuItem;
    boxfileshelp1: TMenuItem;
    normalize_templates_menu_entry: TMenuItem;
    { OT-FIRST keep_pdf_printer: TWPPDFPrinter;}
    { OT-FIRST keep_pdf_setup_dialog: TWPPDFProperties;}
    save_pdf_file_dialog: TSaveDialog;
    keep_html_view: THTMLViewer;
    briefly_hide_on_store_menu_entry: TMenuItem;
    onstorebackgroundfromtrackpad1: TMenuItem;
    N43: TMenuItem;
    hide_on_store_menu_entry: TMenuItem;
    dont_hide_on_store_menu_entry: TMenuItem;
    on_store_help_menu_entry: TMenuItem;
    reveal_on_store_menu_entry: TMenuItem;
    show_tags_list_menu_entry: TMenuItem;
    alert_on_store_menu_entry: TMenuItem;
    N42: TMenuItem;
    radius_warning_limit_as_stored_menu_entry: TMenuItem;
    radius_warning_limit_as_control_menu_entry: TMenuItem;
    id_label: TLabel;
    N44: TMenuItem;
    sort_by_name_menu_entry: TMenuItem;
    sort_by_id_menu_entry: TMenuItem;
    N45: TMenuItem;
    sort_by_type_menu_entry: TMenuItem;
    sort_by_info_menu_entry: TMenuItem;
    reset_all_id_numbers_menu_entry: TMenuItem;
    copy_to_control_radiobutton: TRadioButton;
    wipe_to_control_radiobutton: TRadioButton;
    delete_to_control_radiobutton: TRadioButton;
    radio_box_shape: TShape;
    N46: TMenuItem;
    file_viewer_menu_entry: TMenuItem;
    sort_group_templates_last_menu_entry: TMenuItem;
    N47: TMenuItem;
    centre_line_offset_options_as_stored_menu_entry: TMenuItem;
    centre_line_offset_options_as_control_menu_entry: TMenuItem;
    N48: TMenuItem;
    N49: TMenuItem;
    customize_xing_as_control_menu_entry: TMenuItem;
    customize_xing_as_stored_menu_entry: TMenuItem;
    N50: TMenuItem;
    plain_track_as_control_menu_entry: TMenuItem;
    plain_track_as_stored_menu_entry: TMenuItem;
    N52: TMenuItem;
    sort_half_diamonds_to_first_menu_entry: TMenuItem;
    rem_memo: TMemo;
    rem_label: TLabel;
    reminder_message_menu_entry: TMenuItem;
    N51: TMenuItem;
    add_reminder_menu_entry: TMenuItem;
    edit_reminder_menu_entry: TMenuItem;
    reminder_colour_menu_entry: TMenuItem;
    N53: TMenuItem;
    remove_reminder_menu_entry: TMenuItem;
    procedure copy_to_pad_buttonClick(Sender: TObject);
    procedure escape_buttonClick(Sender: TObject);
    procedure export_mecbox_menu_entryClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure colour_patchClick(Sender: TObject);
    procedure import_mecbox_menu_entryClick(Sender: TObject);
    procedure keep_html_viewHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure store_current_as_unused_buttonClick(Sender: TObject);
    procedure info_radio_buttonClick(Sender: TObject);
    procedure memo_radio_buttonClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure fit_radio_buttonClick(Sender: TObject);
    procedure lock_radio_buttonClick(Sender: TObject);
    procedure chat_panelClick(Sender: TObject);
    procedure show_list_buttonClick(Sender: TObject);
    procedure show_dwg_buttonClick(Sender: TObject);
    procedure keepform_listboxClick(Sender: TObject);
    procedure print_list_buttonClick(Sender: TObject);
    procedure lock_at_buttonClick(Sender: TObject);
    procedure how_panelClick(Sender: TObject);
    procedure copy_or_wipe_background_buttonClick(Sender: TObject);
    procedure rename_menu_entryClick(Sender: TObject);
    procedure paper_colour_menu_entryClick(Sender: TObject);
    procedure grid_colour_menu_entryClick(Sender: TObject);
    procedure rail_colour_menu_entryClick(Sender: TObject);
    procedure timber_colour_menu_entryClick(Sender: TObject);
    procedure dwg_font_menu_entryClick(Sender: TObject);
    procedure delete_menu_entryClick(Sender: TObject);
    procedure list_colour_menu_entryClick(Sender: TObject);
    procedure box_title_menu_entryClick(Sender: TObject);
    procedure export_dxf_menu_entryClick(Sender: TObject);
    procedure clear_menu_entryClick(Sender: TObject);
    procedure save_all_menu_entryClick(Sender: TObject);
    procedure reload_menu_entryClick(Sender: TObject);
    procedure add_file_menu_entryClick(Sender: TObject);
    procedure box_program_panel_menu_entryClick(Sender: TObject);
    //procedure hide_current_on_copy_menu_entryClick(Sender: TObject);
    //procedure show_current_over_menu_entryClick(Sender: TObject);
    procedure print_list_menu_entryClick(Sender: TObject);
    procedure hide_menu_entryClick(Sender: TObject);
    procedure open_on_keep_menu_entryClick(Sender: TObject);
    procedure keep_blind_menu_entryClick(Sender: TObject);
    procedure close_on_copy_menu_entryClick(Sender: TObject);
    procedure blind_copy_menu_entryClick(Sender: TObject);
    procedure copy_in_position_menu_entryClick(Sender: TObject);
    //procedure copy_on_make_current_menu_entryClick(Sender: TObject);
    procedure copy_onto_notch_menu_entryClick(Sender: TObject);
    procedure copy_onto_datum_menu_entryClick(Sender: TObject);
    //procedure send_echo_menu_entryClick(Sender: TObject);
    //procedure cut_echo_menu_entryClick(Sender: TObject);
    //procedure fetch_echo_menu_entryClick(Sender: TObject);
    procedure update_background_menu_entryClick(Sender: TObject);
    procedure reload_ignore_bgnd_menu_entryClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure read_info_buttonClick(Sender: TObject);
    procedure delete_unused_menu_entryClick(Sender: TObject);
    procedure copy_name_menu_entryClick(Sender: TObject);
    procedure new_name_menu_entryClick(Sender: TObject);
    procedure use_current_name_menu_entryClick(Sender: TObject);
    procedure print_all_info_menu_entryClick(Sender: TObject);
    procedure print_all_memo_menu_entryClick(Sender: TObject);
    procedure print_all_text_menu_entryClick(Sender: TObject);
    procedure all_to_bgnd_menu_entryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure undo_delete_menu_entryClick(Sender: TObject);
    //procedure mint_from_final_and_show_menu_entryClick(Sender: TObject);
    //procedure current_template_unchanged_on_reload_menu_entryClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure up_buttonClick(Sender: TObject);
    procedure down_buttonClick(Sender: TObject);
    procedure slider_panelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure slider_panelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure slider_panelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bottom_buttonClick(Sender: TObject);
    procedure top_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edit_memo_menu_entryClick(Sender: TObject);
    procedure gauge_change_menu_entryClick(Sender: TObject);
    //procedure move_up_buttonClick(Sender: TObject);
    //procedure move_down_buttonClick(Sender: TObject);
    procedure keepform_listboxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure save_group_menu_entryClick(Sender: TObject);
    //procedure auto_ebk_load_menu_entryxClick(Sender: TObject);
    //procedure ask_menu_entryxClick(Sender: TObject);
    //procedure omit_ebk_load_menu_entryxClick(Sender: TObject);
    procedure restore_previous_menu_entryClick(Sender: TObject);
    procedure select_menu_entryClick(Sender: TObject);
    procedure select_all_menu_entryClick(Sender: TObject);
    procedure clear_selections_menu_entryClick(Sender: TObject);
    procedure invert_selections_menu_entryClick(Sender: TObject);
    procedure wipe_group_menu_entryClick(Sender: TObject);
    procedure copy_group_menu_entryClick(Sender: TObject);
    procedure rebuild_group_menu_entryClick(Sender: TObject);
    procedure delete_group_menu_entryClick(Sender: TObject);
    procedure wipe_all_menu_entryClick(Sender: TObject);
    procedure box_quit_menu_entryClick(Sender: TObject);
    procedure select_all_bgnd_menu_entryClick(Sender: TObject);
    procedure select_all_unused_menu_entryClick(Sender: TObject);
    procedure save_bgnd_menu_entryClick(Sender: TObject);
    procedure save_unused_menu_entryClick(Sender: TObject);
    procedure show_on_reload_menu_entryClick(Sender: TObject);
    procedure close_on_reload_menu_entryClick(Sender: TObject);
    procedure add_ignore_group_menu_entryClick(Sender: TObject);
    procedure add_to_group_menu_entryClick(Sender: TObject);
    procedure add_new_group_menu_entryClick(Sender: TObject);
    procedure load_all_menu_entryClick(Sender: TObject);
    procedure ignore_unused_menu_entryClick(Sender: TObject);
    procedure timbering_as_stored_menu_entryClick(Sender: TObject);
    procedure timbering_as_control_menu_entryClick(Sender: TObject);
    procedure toggle_all_bgnd_menu_entryClick(Sender: TObject);
    procedure toggle_group_bgnd_menu_entryClick(Sender: TObject);
    procedure restore_prior_previous_menu_entryClick(Sender: TObject);
    procedure group_linked_warning_labelClick(Sender: TObject);
    procedure delete_all_except_group_menu_entryClick(Sender: TObject);
    procedure add_jotter_to_memo_menu_entryClick(Sender: TObject);
    procedure undo_clear_menu_entryClick(Sender: TObject);
    procedure rebuild_template_menu_entryClick(Sender: TObject);
    procedure rebuild_all_menu_entryClick(Sender: TObject);
    procedure add_library_menu_entryClick(Sender: TObject);
    procedure delete_library_templates_menu_entryClick(Sender: TObject);
    procedure save_library_menu_entryClick(Sender: TObject);
    procedure make_library_template_menu_entryClick(Sender: TObject);
    procedure sort_library_templates_last_menu_entryClick(Sender: TObject);
    procedure obtain_switch_menu_entryClick(Sender: TObject);
    procedure obtain_plain_track_menu_entryClick(Sender: TObject);
    //procedure mint_from_final_and_hide_menu_entryClick(Sender: TObject);
    procedure recent_1_popup_entryClick(Sender: TObject);
    procedure recent_2_popup_entryClick(Sender: TObject);
    procedure recent_3_popup_entryClick(Sender: TObject);
    procedure recent_4_popup_entryClick(Sender: TObject);
    procedure recent_5_popup_entryClick(Sender: TObject);
    procedure recent_6_popup_entryClick(Sender: TObject);
    procedure recent_7_popup_entryClick(Sender: TObject);
    procedure recent_8_popup_entryClick(Sender: TObject);
    procedure recent_9_popup_entryClick(Sender: TObject);
    procedure reload_recent_file_menu_entryClick(Sender: TObject);
    procedure add_recent_file_menu_entryClick(Sender: TObject);
    procedure clear_mru_list_popup_entryClick(Sender: TObject);
    procedure reload_add_mru_popup_entryClick(Sender: TObject);
    procedure move_up_buttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure move_up_buttonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure move_down_buttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure move_down_buttonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure move_to_top_buttonClick(Sender: TObject);
    procedure move_to_bottom_buttonClick(Sender: TObject);
    procedure sort_group_templates_first_menu_entryClick(Sender: TObject);
    procedure find_template_menu_entryClick(Sender: TObject);
    procedure find_and_group_menu_entryClick(Sender: TObject);
    procedure find_next_menu_entryClick(Sender: TObject);
    procedure delete_t55_templates_menu_entryClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure trackbed_edges_as_stored_menu_entryClick(Sender: TObject);
    procedure trackbed_edges_as_control_menu_entryClick(Sender: TObject);
    procedure centre_lines_as_stored_menu_entryClick(Sender: TObject);
    procedure centre_lines_as_control_menu_entryClick(Sender: TObject);
    procedure box_info_menu_entryClick(Sender: TObject);
    procedure rail_section_data_as_stored_menu_entryClick(Sender: TObject);
    procedure rail_section_data_as_control_menu_entryClick(Sender: TObject);
    procedure normalize_templates_menu_entryClick(Sender: TObject);
    procedure briefly_hide_on_store_menu_entryClick(Sender: TObject);
    procedure hide_on_store_menu_entryClick(Sender: TObject);
    procedure dont_hide_on_store_menu_entryClick(Sender: TObject);
    procedure reveal_on_store_menu_entryClick(Sender: TObject);
    procedure show_tags_list_menu_entryClick(Sender: TObject);
    procedure alert_on_store_menu_entryClick(Sender: TObject);
    procedure radius_warning_limit_as_stored_menu_entryClick(Sender: TObject);
    procedure radius_warning_limit_as_control_menu_entryClick(Sender: TObject);
    procedure on_store_help_menu_entryClick(Sender: TObject);
    procedure sort_by_name_menu_entryClick(Sender: TObject);
    procedure sort_by_id_menu_entryClick(Sender: TObject);
    procedure sort_by_type_menu_entryClick(Sender: TObject);
    procedure sort_by_info_menu_entryClick(Sender: TObject);
    procedure reset_all_id_numbers_menu_entryClick(Sender: TObject);
    procedure file_viewer_menu_entryClick(Sender: TObject);
    procedure sort_group_templates_last_menu_entryClick(Sender: TObject);
    procedure centre_line_offset_options_as_stored_menu_entryClick(Sender: TObject);
    procedure centre_line_offset_options_as_control_menu_entryClick(Sender: TObject);
    procedure customize_xing_as_stored_menu_entryClick(Sender: TObject);
    procedure customize_xing_as_control_menu_entryClick(Sender: TObject);
    procedure plain_track_as_stored_menu_entryClick(Sender: TObject);
    procedure plain_track_as_control_menu_entryClick(Sender: TObject);
    procedure sort_half_diamonds_to_first_menu_entryClick(Sender: TObject);
    procedure reminder_message_menu_entryClick(Sender: TObject);
    procedure add_reminder_menu_entryClick(Sender: TObject);
    procedure edit_reminder_menu_entryClick(Sender: TObject);
    procedure reminder_colour_menu_entryClick(Sender: TObject);
    procedure remove_reminder_menu_entryClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }

  end;//class

var
  keep_form: Tkeep_form;

//_____________________________________________________________

// 290a - these type declarations moved into the interface for mecbox_unit

type

  // Tkeep_dims has the shove timber data omitted.  v:0.71.a  29-4-01.

  Tkeep_dims1 = record      // first part of Tkeep_dims

    box_dims1: Tbox_dims1;

  end;//record Tkeep_dims1

  Tkeep_dims2 = record

    turnout_info2: Tturnout_info2;

  end;//record Tkeep_dims2

  Told_keep_data = record
    // this matches the old Tkeep_data record pre 071 including the timber shove data.
    // used on loading files.

    old_keep_dims1: Tkeep_dims1;
    old_keep_dims2: Tkeep_dims2;

  end;//record.


var

  save_done: boolean = True;          // nothing to save at startup.
  name_highlighted: integer = -1;     // used in CTRL-Y for mouse select bgnd keep.

  list_position: integer = -1;
  // current index into the keeps list. 0= first one, -1= list empty.

  onto_notch: boolean = False;
  copy_datum: boolean = False;

  override_file_check_for_grid: boolean = False;

  startup_restore_pref: integer = 0; //%%%%   0=ask, 1=don't restore, 2=restore without asking

  loading_in_progress: boolean = False;  // 208c

  reloaded_box_str: string = '';

  saved_box_str: string = '';


  his_save_file_name: string = '';
  his_load_file_name: string = '';

//function echo_keep:boolean;                                          // save the current keep as an echo.
function save_box(this_one, which_ones, rolling_backup: integer; save_str: string): boolean;
function load_storage_box(normal_load, old_templot_folder: boolean; file_str: string;
  load_backup, make_lib: boolean; var append: boolean;
  var last_bgnd_loaded_index: integer): boolean;
// load a file of templates into the keeps box.


function any_selected: integer;                      // any templates selected?

//function any_on_background:boolean;   // any keeps on background?
function any_bgnd: integer;              // any templates on background?  return the count.
function any_unused: integer;            // any templates unused?  return count.
function any_library: integer;           // any library templates?  return count.
function any_t55: integer;               // 0.93.a any T-55 templates?  return count.

function edit_memo_str(in_str, name_str: string): string;

function wipe_it(n: integer): boolean;
//function rebuild_it(n:integer):boolean;
function rebuild_it(n, move_bgnd_peg_to_code, move_bgnd_peg_to_peg_rail: integer;
  move_bgnd_peg: boolean): boolean;     // rebuild template n.   // mods 205c

procedure box_what_next;                // show the what next? help.
procedure load_backup_file;             // called once only during startup.

procedure mouse_on_bgkeep(X, Y: integer; clear_highlights: boolean);
procedure file_error(str: string);
procedure keep_canvas_clear;
procedure copy_keep_to_background(list_index: integer; update_info, reloading: boolean);

procedure rebuild_background(use_modify_options, egg_timer: boolean);
// rebuild (re-draw) all the background keeps.
procedure rebuild_group(use_modify_options, egg_timer: boolean);
// rebuild (re-draw) the selected group of keeps.

procedure clear_all_selections;
procedure do_restore_swap;
procedure delete_keep(echo_cut, alerts: boolean);
// delete the current keep.
procedure copy_keep_to_current(on_reload, bgnd_options, name_options, mint_from: boolean);
// copy the current keep to the control template on the pad.

//procedure store_unused(library_template:boolean);
procedure store_unused(library_template, control_template_on_save: boolean);
// 0.93.a control template added

procedure copy_or_wipe_background;

procedure reload_specified_file(par, add: boolean; file_str: string);
// used for command line parameters on startup.
// 0.82.a also for mru list.
procedure normalize_templates;       // update box contents to latest file format.

procedure mint_final_or_copy_control(i: integer);
// i = index to highest loaded background template for pre 0.93.a files.

procedure keep_move_up_button_click;           // 0.93.a
procedure keep_move_down_button_click;         // 0.93.a

function build_tag_list(group_tags_only: boolean): integer;                        // 206b
procedure create_group_from_tag(tag_str: string; no_adding_alert: boolean);        // 206b
function return_prefix_tags_list(n: integer; var tags_list: TStringList): integer;
// original not modified   // 206b

procedure box_goto_smallest_bgnd_rad;   // 208a
procedure box_goto_smallest_group_rad;  // 208a

procedure current_state(code: integer);  // moved up 208c

function highest_id_number: integer;  // 208a

procedure clear_keeps(delete_backup, save_for_undo: boolean);  // moved up 208d

function get_readme_notes: string;    // 208d

function highest_bgnd_template: integer;
// 219a return highest index of a template on the background

procedure boxmru_update(file_str: string);   // update the mru list.  0.82.a  22-08-06

procedure init_ttemplate(n: integer);    // init list flags for a newly created keep.

function version_mismatch(var okd: Told_keep_data): boolean;
// check loaded template matches current program version.

//_____________________________________________________________________________________________

implementation

{$BOOLEVAL ON}

{$R *.lfm}

uses  LCLIntf, Math, control_room,
  config_unit,
  {pad_unit,} switch_select, help_sheet,
  alert_unit, math_unit,     // moved up 290a
  xing_select, entry_sheet, gauge_unit, colour_unit, info_unit, chat_unit, print_unit,
  dxf_unit, bgkeeps_unit, grid_unit, Clipbrd, edit_memo_unit, wait_message, shove_timber,
  jotter_unit, print_settings_unit, data_memo_unit,
  MetaFilePrinter, { OT-FIRST file_viewer,} panning_unit, mecbox_unit,
  curve,
  rail_data_unit;

const

  bgnd_ident_color: TColor = $0070FF;  // dark orange 223a

  chat_str: string = '      Storage  Box  Chat' +
    '||The "storage box" is simply a metaphor for a document folder or other container in which accumulated paper templates would be kept.' + '||But as "folder" has a specific meaning in computer-speak, I needed to devise an alternative term. This is because the entire contents of the box can be saved in' + ' a single disk file (rather than a disk folder or directory) for rapid access to the complete collection of templates.' + '||Once in the box they can be quickly flicked through as you would with' + ' a collection of paper ones. This would have been more laborious if each template was a separate file in a disk folder - although you can work this way' + ' if you prefer (click the GROUP SELECT > SELECT (TOGGLE) button for the template(s) you require and then click the SAVE GROUP button).';

type
  Tnew_keep_data = record           // this matches Tkeep_dims used in prog - see below.

    new_keep_dims1: Tkeep_dims1;
    new_keep_dims2: Tkeep_dims2;

  end;//record

  // start record for trailing data blocks...

  Tblock_start = record
    version_number: integer; // the Templot0 version number.
    zero1: integer;          // 12 spares (zero)...
    zero2: integer;
    zero3: integer;
  end;

  Tblock_ident = record
    segment_length: integer;
    f_index: integer;
    block_code: integer;   // 10 = timber shove data.
    spare_zeroes: integer;
  end;

var

  deleted_keep: Ttemplate_info;
  deleted_keep_string: string;
  deleted_memo_string: string;

  valid_calcs: boolean = False;

  box_width: double = 50000;     // startup box width in 1/100 mm.

  info_str: string = '';

  box_scaling: boolean = False;
  // flag - otherwise ScrollInView on resize prevents form rescaling properly.

  info_clicked: boolean = False;

  keep_copied_from_index: integer = -1;    // init for empty box.

  copy_custom_code: integer = 1;

  rollback_wanted: boolean = False;

  add_mru: boolean = False;  // append recent file instead of reload.  0.82.a

  search_str: string = '';
  searched_index: integer = -1;

  total_bgnd_template_length: double = 0;      // 0.93.a
  total_group_template_length: double = 0;      // 0.93.a

  total_bgnd_timbering_length: double = 0;      // 0.96.a
  total_group_timbering_length: double = 0;      // 0.96.a

  smallest_bgnd_radius: double = 1.0E8 - 5000;
  // 208a for box data    init=max_rad ="straight"
  smallest_group_radius: double = 1.0E8 - 5000;
  // 208a for box data    init=max_rad ="straight"

  smallest_bgnd_radius_index: integer = 0;      // 208a
  smallest_group_radius_index: integer = 0;     // 208a


procedure keep_draw(index: integer); forward;          // draw control template on the keep form.

procedure clear_keep(n: integer); forward;
// clear a single keep without asking - this is for errors on loading.

procedure highlight_bgkeep(index: integer); forward;
// highlight the peg on the current keep on the background.

function duplicates_removed: boolean; forward;
// remove any duplicate keeps after multiple appends, echo fetches, etc.

function slider_position(index: integer): integer; forward;
// move slider to match current index.
function slider_index(slider_left: integer): integer; forward;
// get list index for this slider position.

function any_non_library: integer; forward;   // any non-library templates?  return count.

function highest_non_library: integer; forward;
// return highest index of a non-library template.
function lowest_library: integer; forward;           // return lowest index of a library template.

function highest_group_template: integer; forward;
// 0.93.a return highest index of a group template.
function lowest_non_group_template: integer; forward;
// 0.93.a return lowest index of a non-group template.

function lowest_group_template: integer; forward;
// 213b return lowest index of a group template.
function highest_non_group_template: integer; forward;
// 213b return highest index of a non-group template.

//________________________________________________________________________________________

procedure file_error(str: string);     // generic error message.

begin
  alert(5, '     file  error',
    str + '||Sorry, the requested operation on this file has failed. Please check the file and folder names.' + '||If this is a save operation, check the available disk space,' + ' and if saving to a floppy disk, check that it is not write-protected.' + '||If this is a reload operation, check that the named file exists in the named folder.',
    '', '', '', '', '', 'O K', 0);
end;
//__________________________________________________________________________________________

procedure empty_boxmru;   // 0.82.a

var
  n: integer;

begin
  boxmru_list.Clear;
  for n := 0 to 8 do
    boxmru_list.Add('');    // always 9 items.

  keep_form.clear_mru_list_popup_entry.Enabled := False;
end;
//___________________________________________________________________________________________

procedure boxmru_update(file_str: string);   // update the mru list.  0.82.a  22-08-06

// remove one then add one - maintain 9 entries in list.
// bottom of list is top of menu.

var
  n: integer;

begin
  n := boxmru_list.IndexOf(file_str);     // already in list?

  if n < 0 then
    boxmru_list.Delete(0)     // no, remove oldest from top.
  else
    boxmru_list.Delete(n);    // yes, remove it.

  boxmru_list.Add(file_str);   // add new (or again) at bottom.
end;
//__________________________________________________________________________________________

procedure init_ttemplate(n: integer);    // init list flags for a newly created keep.

var
  i: integer;
  aq: ERailData;

begin
  with Ttemplate(keeps_list.Objects[n]) do begin
    bg_copied := False;                               // True=has been copied to the background.
    group_selected := False;                          // True=selected as one of a group.
    new_stamp_wanted := False;
    // True=has been shifted/rotated/mirrored, needs a new timestamp on rebuilding.

    { out 206b
    in_rem_groups[0]:=False;      // remembered groups.
    in_rem_groups[1]:=False;
    in_rem_groups[2]:=False;
    in_rem_groups[3]:=False;
    in_rem_groups[4]:=False;
    in_rem_groups[5]:=False;
    in_rem_groups[6]:=False;
    in_rem_groups[7]:=False;
}

    with bgnd_keep do begin
      SetLength(list_bgnd_marks, 0);
      for aq in ERailData do begin
        SetLength(list_bgnd_rails[aq], 0);
      end;//for
    end;//with

    template_info.keep_dims.box_dims1.pre077_bgnd_flag := False;
    // unused template. no longer used 0.77.a but needed in file if reloaded into older version.
    template_info.keep_shove_list := Tshoved_timber_list.Create;
  end;//with
end;
//_________________________________________________________________________________________

function wipe_it(n: integer): boolean;   // wipe template n from background.

  // return True if successfully wiped, or False if was already unused.
var
  index: integer;
  aq: ERailData;

begin
  Result := False;        // default init

  if any_bgnd = 0 then
    EXIT;  // not on background.

  with Ttemplate(keeps_list.Objects[n]) do begin

    template_info.keep_dims.box_dims1.bgnd_code_077 := 0;
    // bgnd_flag:=False;  // flag it is not now a background keep.
    template_info.keep_dims.box_dims1.pre077_bgnd_flag := False;
    // in case reloaded in older version than 0.77.a

    if bg_copied = False then
      EXIT;              // ??? not on background.

    bg_copied := False;      // will no longer be on background.

    with bgnd_keep do begin
      SetLength(list_bgnd_marks, 0);

      for aq in ERailData do begin
        SetLength(list_bgnd_rails[aq], 0);
      end;//for next aq

    end;//with bgnd_keep data
  end;//with template

  Result := True;
  save_done := False;
  backup_wanted := True;
  redraw_pad(True, False);
end;
//_________________________________________________________________________________________

function rebuild_it(n, move_bgnd_peg_to_code, move_bgnd_peg_to_peg_rail: integer;
  move_bgnd_peg: boolean): boolean;     // rebuild template n.

  // move_bgnd_peg_to_code:integer; move_bgnd_peg:boolean   added 205c

var
  save_template: Ttemplate_info;

begin
  Result := False;                                       // default init.
  save_template.keep_shove_list := Tshoved_timber_list.Create;
  fill_kd(save_template);
  // temporarily store the existing settings (control template or box).
  try

    if (n < 0) or (n > (keeps_list.Count - 1)) then
      EXIT;  // how did this happen ?

    // first wipe the existing...

    if wipe_it(n) = False then
      EXIT;        // not a background template.

    with Ttemplate(keeps_list.Objects[n]) do begin

      if move_bgnd_peg = True     // 205c
      then begin
        template_info.keep_dims.box_dims1.transform_info.peg_point_code :=
          move_bgnd_peg_to_code;
        template_info.keep_dims.box_dims1.transform_info.peg_point_rail :=
          move_bgnd_peg_to_peg_rail;
        template_info.keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
        // modify Delphi float time format to integer.
      end;

      // does he want it modified?...

      if keep_form.timbering_as_control_menu_entry.Checked = True then begin
        with template_info do begin
          update_timbering(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;

      if keep_form.plain_track_as_control_menu_entry.Checked = True then begin
        with template_info do begin
          update_lengths(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;

      if keep_form.customize_xing_as_control_menu_entry.Checked = True   // 214b
      then begin
        with template_info do begin
          update_customize_xing(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;

      if keep_form.trackbed_edges_as_control_menu_entry.Checked = True then begin
        with template_info do begin
          update_trackbed_edges(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;

      if keep_form.centre_lines_as_control_menu_entry.Checked = True then begin
        with template_info do begin
          update_centre_lines(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;

      if keep_form.centre_line_offset_options_as_control_menu_entry.Checked = True then begin
        with template_info do begin
          update_centre_line_offset_options(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;

      if keep_form.rail_section_data_as_control_menu_entry.Checked = True then begin
        with template_info do begin
          update_rail_section(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;

      if keep_form.radius_warning_limit_as_control_menu_entry.Checked = True then begin
        with template_info do begin
          update_radius_warning(keep_dims);
          // modify the stored data, so we need a new timestamp.
          keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer.
        end;//with

        save_done := False;
      end;


      copy_keep_to_background(n, True, False);   // then calc and copy again.

      if move_bgnd_peg = True then begin
        with template_info.keep_dims.box_dims1.transform_info do begin
          notch_info := get_peg_for_notch;   // 205c

          peg_pos.x := pegx;   //  mm  peg position.
          peg_pos.y := pegy;

        end;//with
      end;

    end;//with Ttemplate

    backup_wanted := True;
    Result := True;

  finally
    copy_keep(save_template);                       // restore previous settings
    save_template.keep_shove_list.Free;
  end;//try
end;
//______________________________________________________________________________________

function load_lines(append: boolean; file_name: string; var ident: string): boolean;
  // load text lines into info and user memos from a single file.

var
  file_txt: TextFile;
  n: integer;
  str: string;
  memo_flag: boolean;

begin
  Result := False;            // init default.

  if FileExists(file_name) = False then begin
    ident := '-1';    // return invalid
    EXIT;
  end;

  if (append = False) and ((keeps_list.Count <> 0) or (memo_list.Count <> 0)) then
    run_error(221); // lists not empty.

  if keeps_list.Count <> memo_list.Count then
    run_error(222);

  try
    AssignFile(file_txt, file_name);     // set up the file name.
    Reset(file_txt);                    // open a new file.
    memo_flag := False;

    while EOF(file_txt) = False do begin
      ReadLn(file_txt, str);
      if str[1] = '_' then begin
        ident := Copy(str, 2, 7);     // ident is max 7 chars
        memo_flag := True;
      end
      else
      if memo_flag = False then
        n := keeps_list.Add(str)    // add line to info.
      else
        n := memo_list.Add(str);    // add line to memo.
    end;//while
    Result := True;
  except
    on EInOutError do
      Result := False;
  end;//try-except

  try
    CloseFile(file_txt);
  except
    on EInOutError do
  end;      // close file if it's open.

  if keeps_list.Count <> memo_list.Count then begin
    alert(5, '      file  wrong',
      '|The contents of the file:' + '||' + file_name +
      '||are incorrect. Either there is a hardware problem, or the file has been modified since Templot0 created it.'
      + '||Reload will have to be abandoned.',
      '', '', '', '', '', 'continue', 0);
    Result := False;                          // file has been tampered with.
  end;
end;
//_______________________________________________________________________________________

procedure delete_keep(echo_cut, alerts: boolean);           // delete the current keep.

var
  n, i: integer;
  ti: Ttemplate_info;
  s, str: string;
  bf: boolean;

begin
  if keeps_list.Count < 1 then
    EXIT;
  if keeps_list.Count <> memo_list.Count then
    run_error(220);

  n := list_position;
  if (n >= keeps_list.Count) or (n < 0) then
    EXIT;

  ti.keep_shove_list := Tshoved_timber_list.Create;

  try
    copy_template_info_from_to(False, Ttemplate(keeps_list.Objects[n]).template_info, ti);
    // current keep data
    bf := Ttemplate(keeps_list.Objects[n]).bg_copied;
    // background flag.

    if bf = True then
      s := ' and from the background drawing.'
    else
      s := '.';

    if echo_cut = False then begin
      if (no_delete_msg_pref = False) and (alerts = True) then begin
        str := ti.keep_dims.box_dims1.reference_string;
        // Delphi bug? - must be a local string, otherwise limits length of alert string.

        alert_box.preferences_checkbox.Checked := False;       //%%%%
        alert_box.preferences_checkbox.Show;

        // 208b mods..

        i := alert(7, '      delete  this  template ?', '|' + str +
          '||You are about to delete this template from your storage box' +
          s + ' The remaining templates will be re-numbered.' +
          '||It can be restored by clicking the `0EDIT > UNDO DELETE`1 menu item on the storage box'
          + '||or the `0PROGRAM > UNDO DELETED TEMPLATE`1 menu item on the trackpad' +
          '||before another template is deleted.' +
          '||Are you sure you want to delete this template ?', '', '', '', '',
          'no  -  cancel  delete', 'yes  -  delete  ' + str + '       ', 0);

        //%%%%  was "today"

        no_delete_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
        alert_box.preferences_checkbox.Hide;

        if i = 5 then
          EXIT;
      end;

      if bgnd_clicked_in_quick_mode = False
      // 0.93.a  don't save for undo if background template clicked in Quick Mode
      then begin
        copy_template_info_from_to(False, ti, deleted_keep);
        // save for undo delete.
        deleted_keep_string := keeps_list.Strings[n];
        deleted_memo_string := memo_list.Strings[n];
        keep_form.undo_delete_menu_entry.Enabled := True;
      end;
    end
    else begin
      if (no_cut_msg_pref = False) and (alerts = True) then begin
        str := ti.keep_dims.box_dims1.reference_string;
        // Delphi bug? - must be a local string, otherwise limits length of alert string.

        alert_box.preferences_checkbox.Checked := False;       //%%%%
        alert_box.preferences_checkbox.Show;

        i := alert(7, '      cut  this  template ?', '|' + str +
          ' has been sent as an ECHO.' +
          ' You are now about to delete this template from your storage box' +
          s + '||It can be only restored by selecting the DISK > FETCH ECHO menu item before another echo is sent.' + '||The remaining templates will be re-numbered.' + '||Are you sure you want to delete this template ?', '', '', '', '', 'no  -  cancel  delete    ', 'yes  -  delete  ' + ti.keep_dims.box_dims1.reference_string + '      ', 0);

        //%%%%  was "today"

        no_cut_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
        alert_box.preferences_checkbox.Hide;

        if i = 5 then
          EXIT;

      end;
    end;

    if bf = True       // first remove it from the background and free drawing data...
    then begin
      if wipe_it(n) = False then begin
        alert(5, '    program  error',
          '||Sorry, there is a program error.' +
          '||The background will be cleared and rebuilt.' +
          '||No templates have been deleted.',
          '', '', '', '', '', 'O K', 0);
        rebuild_background(False, True);
        EXIT;
      end;
    end;

    Ttemplate(keeps_list.Objects[n]).template_info.keep_shove_list.Free;

    Ttemplate(keeps_list.Objects[n]).Free;
    keeps_list.Delete(n);
    memo_list.Delete(n);
    save_done := False;
    backup_wanted := True;

    current_state(-1);
  finally
    ti.keep_shove_list.Free;
  end;//try
end;
//_________________________________________________________________________________________

function version_mismatch(var okd: Told_keep_data): boolean;
  // check loaded template matches current program version.

  // use the old_keep_data format for compatibility when reloading old files.
  // return True if there is a version mismatch (re-save needed).

var
  n, list_index: integer;

  ////////////////////////////////////////////////////////

  function any_loaded_rails_omitted: boolean;  // 208a

  var
    pt_all, turnout_all, hd_all: boolean;

  begin
    Result := False;  // init

    with okd do begin

      with old_keep_dims1.box_dims1.rail_info do begin

        pt_all :=
          turnout_road_stock_rail_sw and main_road_stock_rail_sw;

        turnout_all := pt_all and turnout_road_check_rail_sw and
          turnout_road_crossing_rail_sw and crossing_vee_sw and
          main_road_check_rail_sw and main_road_crossing_rail_sw;

        hd_all := turnout_all and k_diagonal_side_check_rail_sw and
          k_main_side_check_rail_sw;

      end;//with

      if old_keep_dims1.box_dims1.turnout_info1.plain_track_flag = True
      // plain track template
      then
        Result := not pt_all
      else
      if old_keep_dims2.turnout_info2.semi_diamond_flag = True    // half-diamond template
      then
        Result := not hd_all
      else
        Result := not turnout_all;             // turnout template

    end;//with okd
  end;
  ////////////////////////////////////////////////////////

begin
  Result := False;      // init return, no mismatch

  with okd do begin
    with old_keep_dims1.box_dims1 do begin
      version_as_loaded := templot_version;
      // 0.78.d keep a note in the template of the version it was loaded as.

      if loaded_version > templot_version then
        loaded_version := templot_version;
      // loaded_version is a global reminder of the earliest loaded template.
      if templot_version > file_version then
        later_file := True;                 // file was produced by a later file than this.

      if templot_version <> file_version              // template not saved by this version.
      then begin
        if templot_version = 0 then
          align_info.cl_only_flag := False;  // so update the file..

        if templot_version < 33     // slew mods not in earlier versions than 0.33...
        then begin
          // so update the file with defaults...

          with align_info do begin
            tanh_kmax := 2;
            {:double;}{spare_int1:integer;}// stretch factor for mode 2 slews.
            {spare_int2:integer;}
            // !!! double used because only 8 bytes available in existing file format (2 integers).
            slew_type := 1;
            {:byte;}{spare_flag3:boolean;}
            // !!! byte used because only 1 byte available in existing file format 1-11-99.
          end;//with
        end;

        if templot_version < 35 then
          old_keep_dims2.turnout_info2.start_draw_x := 0;
        {spare_float1:double;}// startx  not in earlier than version 0.35

        if templot_version < 38 then begin
          mod_text_x := 0;      // position modifiers for labels.
          mod_text_y := 0;
        end;

        if templot_version < 52 then
          old_keep_dims2.turnout_info2.equalizing_fixed_flag := False;
        {spare_flag1:boolean;}// equalizing style 1-4-00 not before 0.52.

        if templot_version < 53         // notch info not in file.  pre 11-4-00.
        then begin
          with transform_info.notch_info do begin
            notch_x := 0;
            notch_y := 0;
            notch_k := 0;
          end;//with
        end;

        if templot_version < 65 then
          old_keep_dims2.turnout_info2.no_timbering_flag := False;
        {spare_flag2:boolean;}// 8-9-00 not before 0.65.

        if templot_version < 67 then
          old_keep_dims2.turnout_info2.plain_track_info.pt_spacing_name_str :=
            '  plain track rail length settings as loaded';  // 17-1-01.

        if templot_version < 68 then begin
          with old_keep_dims2.turnout_info2.plain_track_info do begin
            user_pegx := 0;
            // user-defined peg data (here to use former spare floats in file)
            user_pegy := 0;
            user_pegk := 0;
            user_peg_data_valid := False;
            user_peg_rail := 8;                // ms centre.
          end;//with
        end;

        if templot_version < 71 then begin

          with rail_info do begin

            flared_ends_ri := 0;  // 0=straight bent  1=straight machined.

            // rail switches...

            track_centre_lines_sw := True;

            turnout_road_stock_rail_sw := True;
            turnout_road_check_rail_sw := True;
            turnout_road_crossing_rail_sw := True;
            crossing_vee_sw := True;
            main_road_crossing_rail_sw := True;
            main_road_check_rail_sw := True;
            main_road_stock_rail_sw := True;

          end;//with

          with proto_info do begin

            old_tb_pi := 102;
            // 8ft-6in sleepers default (used internally for gauge changes - no meaning in file). (was 108in - 9ft pre 0.93.a)

            // convert old check rail lengths loaded..
            // to new check and wing dimensioning : v:0.71.a 24-5-01...

            ck_ms_working1_pi := old_cklongs_pi - old_winglongs_pi;
            // full-size inches - size 1 MS check rail working length (back from "A").
            ck_ms_working2_pi := old_cklongm_pi - old_winglongs_pi;
            // full-size inches - size 2 MS check rail working length (back from "A").
            ck_ms_working3_pi := old_cklongxl_pi - old_winglongl_pi;
            // full-size inches - size 3 MS check rail working length (back from "A").

            ck_ms_ext1_pi := old_winglongs_pi;
            // full-size inches - size 1 MS check rail extension length (forward from "A").
            ck_ms_ext2_pi := old_winglongl_pi;
            // full-size inches - size 2 MS check rail extension length (forward from "A").

            ck_ts_working_mod_pi := 0;
            // full-size inches - TS check rail working length modifier.
            ck_ts_ext_mod_pi := 0;
            // full-size inches - TS check rail extension length modifier.

            wing_ms_reach1_pi := old_winglongs_pi;
            // full-size inches - size 1 MS wing rail length.
            wing_ms_reach2_pi := old_winglongl_pi;
            // full-size inches - size 2 MS wing rail length.
            wing_ts_reach_mod_pi := 0;
            // full-size inches - TS wing rail reach length modifier.
          end;//with

          with old_keep_dims2.turnout_info2.switch_info do begin

            case old_size of   // was called size

              0..5:
                list_index := get_switch_list_index(1, old_size + 1);  // straight switches (6).
              6:
                list_index := -1;                                   // custom switch in file.
              7..12:
                list_index := get_switch_list_index(2, old_size - 6);  // REA switches A-F (6).
              13..20:
                list_index := get_switch_list_index(4, old_size - 12);
              // GWR old_type heel switches. 9ft - 20ft (8).
              21..24:
                list_index := get_switch_list_index(3, old_size - 20);
                // GWR curved switches B-D + 30ft straight switch (4).
              else
                list_index := -1;            //  ???
            end;//case

            if list_index < 0   // custom switch or switch not found in listbox...
            then begin
              planing_radius := 0;
              // planing radius for double-curved switch.
              joggle_depth := 0.375;
              // 3/8" depth of joggle. 0.71.a 13-4-01.
              joggle_length := 6;
              // 6" length of joggle in front of toe (+ve). 0.71.a 13-4-01.
              joggled_stock_rail := False;  // no joggle default.
            end
            else begin
              with Tswitch(switch_select_form.switch_selector_listbox.
                  Items.Objects[list_index]) do begin
                planing_radius := list_switch_info.planing_radius;
                // planing radius for double-curved switch.
                joggle_depth := list_switch_info.joggle_depth;
                // depth of joggle. 0.71.a 13-4-01.
                joggle_length := list_switch_info.joggle_length;
                // length of joggle in front of toe (+ve). 0.71.a 13-4-01.
                joggled_stock_rail := list_switch_info.joggled_stock_rail;
              end;//with
            end;
          end;//with
        end;

        if templot_version < 72 then
          old_keep_dims2.turnout_info2.angled_on_flag := False;  // 29-7-01.

        if templot_version < 74 then begin
          rail_type := 1;   // head only (bullhead rail).

          with proto_info do begin
            railbottom_pi := 5.5 * scale_pi / 12;   // 5.5" FB rail foot (mm).
            rail_height_pi := 5.71875;
            // 5.23/32" BS95R bullhead rail height (for 3D in DXF). (F-S inches).
            seat_thick_pi := 1.750;
            // bullhead chair seating thickness (for 3D in DXF). (F-S inches).

            rail_inclination_pi := 0.0499584;    // radians (1:20).
            foot_height_pi := 7 / 16;
            // 7/16" inches full-size  edge thickness.

            chair_outlen_pi := 9.25;
            // 9.25 inches full-size  from rail gauge-face
            chair_inlen_pi := 5.25;     // 5.25 inches full-size
            chair_width_pi := 8.0;      // 8 inches full-size
            chair_corner_pi := 1;       // 1 inch full-size  corner rad.

            timber_thick_pi := 5.0;    // 5 inches full-size timber thickness.
          end;//with
        end;

        if templot_version < 76 then begin
          uninclined_rails := True;      // True = rails vertical.

          print_mapping_colour := cur_prmap_col;
          // 0.76.a  27-10-01 //spare_inta:integer;
          pad_marker_colour := cur_padmark_col;
          // 0.76.a  27-10-01 //spare_intb:integer;

          use_print_mapping_colour := False;
          // 0.76.a  27-10-01 //spare_boola:boolean;
          use_pad_marker_colour := False;
          // 0.76.a  27-10-01 //spare_boolb:boolean;

          old_keep_dims2.turnout_info2.bonus_timber_count := 0;
          old_keep_dims2.turnout_info2.plain_track_info.rail_joints_code := 0;
          // 0=normal, 1=staggered, -1=none (cwr).

          with old_keep_dims2.turnout_info2.crossing_info do begin

            blunt_nose_width := 0.75;         // 3/4" full-size inches.
            blunt_nose_to_timb := 4.0;
            // full-size inches - 4" to A timber centre.

            vee_timber_spacing := proto_info.xtimbsp_pi;
            // full-size inches - timber spacing for vee point rail part of crossing (on from "A").
            wing_timber_spacing := proto_info.xtimbsp_pi;
            // full-size inches - timber spacing for wing rail front part of crossing (up to "A").

            vee_joint_half_spacing := 12.5;
            // full-size inches - 12.5" overlap at vee point rail joint.
            wing_joint_spacing := 25;
            // full-size inches - 25" timber spacing at wing rail joint.

            // number of timbers spanned by vee rail incl. "A" timber...

            vee_joint_space_co1 := 4;
            vee_joint_space_co2 := 5;
            vee_joint_space_co3 := 6;
            vee_joint_space_co4 := 7;
            vee_joint_space_co5 := 8;
            vee_joint_space_co6 := 9;

            // number of timbers spanned by wing rail front excl. "A" timber...

            wing_joint_space_co1 := 2;
            wing_joint_space_co2 := 3;
            wing_joint_space_co3 := 3;
            wing_joint_space_co4 := 4;
            wing_joint_space_co5 := 5;
            wing_joint_space_co6 := 6;

          end;//with

          with old_keep_dims2.turnout_info2.switch_info do begin
            fb_tip_offset := 0;
            // 0.76.a  2-1-02. fbtip dimension (FB foot from gauge-face at tip).
          end;//with

          old_keep_dims2.turnout_info2.plain_track_info.pt_tb_rolling_percent := 0;
          // timber rolling;
        end;

        if templot_version < 77 then begin

          if pre077_bgnd_flag = True then
            bgnd_code_077 := 1    // on bgnd.
          else
            bgnd_code_077 := 0;   // unused.

          with old_keep_dims2.turnout_info2.crossing_info do begin

            hd_vchecks_code := 0;
            // no shortening for half-diamond v-crossing check rails.

            k_check_length_1 := 185;
            // length of size 1 k-crossing check rail (inches).
            k_check_length_2 := 197;
            // length of size 2 k-crossing check rail (inches).
            k_check_mod_ms := 0;      // main side modifer.
            k_check_mod_ds := 0;      // diamond side modifer.
            k_check_flare := 36;
            // length of flare on k-crossing check rails (inches).
          end;//with

          with transform_info do begin
            if transforms_apply = False        // no longer used..
            then begin
              transforms_apply := True;
              x1_shift := 0;
              y1_shift := 0;
              k_shift := 0;
              x2_shift := 0;
              y2_shift := 0;
            end;
          end;//with

          with align_info do begin
            if curving_flag = False          // straight turnout..
            then begin
              curving_flag := True;
              trans_flag := False;
              fixed_rad := max_rad;  // now curved at max_rad.
              rad_offset := 0;
            end;
          end;//with

          with old_keep_dims2.turnout_info2 do begin

            diamond_auto_code := 0;
            // auto select switch-diamond or fixed-diamond.
            semi_diamond_flag := False;
            // not a half-diamond template, calc a normal switch.
            diamond_fixed_flag := True;  // default is a fixed diamond.

          end;//with

          with old_keep_dims2.turnout_info2.switch_info do begin

            if (ABS(switch_radius_inchormax) > (max_rad /
              20)) // buggy? in older files. 20 arbitrary.
              and (sw_pattern = 0)
            // straight or curved switch.
            then
              switch_radius_inchormax := max_rad;        // straight switch.

            case old_size of
              0..5: begin                    // straight switches (6).
                group_code := 1;
                size_code := old_size + 1;
                group_count := 6;
                front_timbered := False; // assume pre-grouping.
              end;

              6: begin                    // custom switch.
                group_code := 0;
                size_code := 1;
                group_count := 1;
                front_timbered := False;
              end;

              7..12: begin                    // REA switches A-F (6).
                group_code := 2;
                size_code := old_size - 6;
                group_count := 6;
                front_timbered := True;
              end;

              13..20: begin
                // GWR heel switches. 9ft - 20ft (8).
                group_code := 4;
                size_code := old_size - 12;
                group_count := 8;
                front_timbered := False; // assume pre-grouping.
              end;

              21..24: begin
                // GWR curved switches B-D + 30ft straight switch (4).
                group_code := 3;
                size_code := old_size - 20;
                group_count := 4;
                front_timbered := True;
              end;

              else begin
                // ???  No other sizes should be in file before 0.77.a
                group_code := 0;
                size_code := 0;
                group_count := 0;
                front_timbered := False;
              end;
            end;//case

            sleeper_j3 := 0;
            //  third switch-front sleeper spacing back from the second (NEGATIVE inches).
            sleeper_j4 := 0;
            //  fourth switch-front sleeper spacing back from the third (NEGATIVE inches).
            sleeper_j5 := 0;
            //  fifth switch-front sleeper spacing back from the fourth (NEGATIVE inches).

            valid_data := True;     //  True = valid data here. 0.77.a 9-6-02.

          end;//with
        end;

        if templot_version < 78       // 0.78.a 11-11-02.
        then begin
          with old_keep_dims2.turnout_info2 do begin
            timber_length_inc := 6.0;                    // 6" timber increments.
            diamond_proto_timbering_flag := True;
            // use prototype timbering for a diamond.
            crossing_info.hd_timbers_code := 0;
            // no extending of half-diamond timbers for slips.
          end;//with
        end;

        if templot_version < 79       // 0.79.a
        then begin
          with old_keep_dims2.turnout_info2 do begin
            omit_switch_front_joints := False;
            omit_switch_rail_joints := False;
            omit_stock_rail_joints := False;
            omit_wing_rail_joints := False;
            omit_vee_rail_joints := False;
            omit_k_crossing_stock_rail_joints := False;
          end;//with

          grid_units_code := 0;  // this means the following will not be used..
          x_grid_spacing := 50;
          y_grid_spacing := 50;
        end;

        if templot_version < 82 then begin

          rail_info.switch_drive_sw := True;   // 0.82.a

          disable_f7_snap := False;            //  0.82.a

        end;

        if templot_version < 93    // 0.93.a mods...    04-07-10
        then begin

          this_was_control_template := False;
          // all older versions don't save the control template

          with platform_trackbed_info do begin

            adjacent_edges_keep := True;
            // False=adjacent tracks,  True=trackbed edges and platform edges.

            draw_ms_trackbed_edge_keep := False;
            draw_ts_trackbed_edge_keep := False;

            OUT_OF_USE_trackbed_width_ins_keep := 180;
            // 180 inches full-size 15ft.      out of use 215a

            draw_ts_platform_keep := False;
            draw_ts_platform_start_edge_keep := True;
            draw_ts_platform_end_edge_keep := True;
            draw_ts_platform_rear_edge_keep := True;

            platform_ts_front_edge_ins_keep := 28.75;
            // 2ft-4.3/4" from rail 093a    changed to 57" from centre-line in 215a below
            platform_ts_start_width_ins_keep := 144;      // 12ft default
            platform_ts_end_width_ins_keep := 144;        // 12ft default

            platform_ts_start_mm_keep := 0;
            platform_ts_length_mm_keep := def_req;     // set to template end

            draw_ms_platform_keep := False;
            draw_ms_platform_start_edge_keep := True;
            draw_ms_platform_end_edge_keep := True;
            draw_ms_platform_rear_edge_keep := True;

            platform_ms_front_edge_ins_keep := 28.75;
            // 2ft-4.3/4" from rail 093a    changed to 57" from centre-line in 215a below
            platform_ms_start_width_ins_keep := 144;      // 12ft default
            platform_ms_end_width_ins_keep := 144;        // 12ft default

            platform_ms_start_mm_keep := 0;
            platform_ms_length_mm_keep := def_req;     // set to template end

          end;//with platform_trackbed_info

          with old_keep_dims2.turnout_info2 do begin

            gaunt_flag := False;
            gaunt_offset_inches := 12.0;
            // default offset 12" if converted to gaunt.
            plain_track_info.gaunt_sleeper_mod_inches := 12.0;
            // extend approach sleepers by 12" to match.

            with crossing_info do
              hdkn_unit_angle := k3n_unit_angle;
            // only regular half-diamonds in earlier versions.

          end;//with

          with rail_info do begin
            k_diagonal_side_check_rail_sw := main_road_check_rail_sw;
            // diagonal-side check rail is in main road.
            k_main_side_check_rail_sw := turnout_road_check_rail_sw;
            // main-side check rail is in diagonal road.
          end;//with

        end;

        if templot_version < 94    // 0.94.a mods...    11-08-11
        then begin

          with check_diffs do begin         // check-rail mouse modifiers
            end_diff_mw.len_diff := 0;
            end_diff_mw.flr_diff := 0;
            end_diff_mw.gap_diff := 0;
            end_diff_mw.type_diff := 0; // byte

            end_diff_me.len_diff := 0;
            end_diff_me.flr_diff := 0;
            end_diff_me.gap_diff := 0;
            end_diff_me.type_diff := 0; // byte

            end_diff_mr.len_diff := 0;
            end_diff_mr.flr_diff := 0;
            end_diff_mr.gap_diff := 0;
            end_diff_mr.type_diff := 0; // byte

            // set the V-crossing turnout-side check length diffs from the old...
            // old no longer used in the program.

            end_diff_tw.len_diff := proto_info.ck_ts_working_mod_pi;
            end_diff_tw.flr_diff := 0;
            end_diff_tw.gap_diff := 0;
            end_diff_tw.type_diff := 0; // byte

            end_diff_te.len_diff := proto_info.ck_ts_ext_mod_pi;
            end_diff_te.flr_diff := 0;
            end_diff_te.gap_diff := 0;
            end_diff_te.type_diff := 0; // byte

            end_diff_tr.len_diff := proto_info.wing_ts_reach_mod_pi;
            end_diff_tr.flr_diff := 0;
            end_diff_tr.gap_diff := 0;
            end_diff_tr.type_diff := 0; // byte

            // set the K-crossing check length diffs from the old...

            end_diff_mk.len_diff :=
              old_keep_dims2.turnout_info2.crossing_info.k_check_mod_ms;
            end_diff_mk.flr_diff := 0;
            end_diff_mk.gap_diff := 0;
            end_diff_mk.type_diff := 0; // byte

            end_diff_dk.len_diff :=
              old_keep_dims2.turnout_info2.crossing_info.k_check_mod_ds;
            end_diff_dk.flr_diff := 0;
            end_diff_dk.gap_diff := 0;
            end_diff_dk.type_diff := 0; // byte

          end;

          retain_diffs_on_make_flag := False;    // 0.94.a check rail diffs
          retain_diffs_on_mint_flag := False;    // 0.94.a check rail diffs

          // 0.94.a timber shoving mods..

          retain_shoves_on_make_flag := False;
          retain_shoves_on_mint_flag := False;

          fb_kludge_template_code := 0;  // normal template.  rail-foot kludge 0.94.a

        end;

        if templot_version < 95    // 0.95.a mods...    12-11-11
        then begin
          with old_keep_dims2.turnout_info2.crossing_info do begin

            // 0.95.a K wing rails ..

            k_custom_wing_long_keep := 185;
            // inches full-size 15'5" k-crossing wing rails, BH 1:6.5 - 1:8
            k_custom_point_long_keep := 144;
            // inches full-size 12' k-crossing point rails   NYI

            use_k_custom_wing_rails_keep := False;
            use_k_custom_point_rails_keep := False;

          end;//with

        end;

        if templot_version < 206    // 2.06.a mods...    18-10-2012
        then begin

          align_info.cl_options_code_int := 0;
          // 206a    // draw centre-line on main road
          align_info.cl_options_custom_offset_ext := 0;

          platform_trackbed_info.OUT_OF_USE_cess_width_ins_keep := 27;
          // 206a   was 30    out of use 215a
          platform_trackbed_info.OUT_OF_USE_draw_trackbed_cess_edge_keep := False;
          // 206a             out of use 215a

        end;


        if templot_version < 207    // 207a mods...    08-04-2013
        then begin

          with platform_trackbed_info do begin    // end skewing added...

            platform_ms_start_skew_mm_keep := 0;      // 207a
            platform_ms_end_skew_mm_keep := 0;        // 207a

            platform_ts_start_skew_mm_keep := 0;      // 207a
            platform_ts_end_skew_mm_keep := 0;        // 207a

          end;

        end;

        if templot_version < 208    // 208a mods...    28-04-2013
        then begin

          old_keep_dims2.turnout_info2.smallest_radius_stored := max_rad;
          // not available before 208a -- not loaded to the control, always calculated fresh

          old_keep_dims2.turnout_info2.dpx_stored := 0;
          // not available before 208a -- not loaded to the control, always calculated fresh
          old_keep_dims2.turnout_info2.ipx_stored := 0;
          // not available before 208a -- not loaded to the control, always calculated fresh
          old_keep_dims2.turnout_info2.fpx_stored := 0;
          // not available before 208a -- not loaded to the control, always calculated fresh

          // update template names to ID numbers...

          if Trim(reference_string) = 'no-name' then
            reference_string := '';   // 208a

          id_number := highest_id_number + 1;                                  // 208a
          id_number_str :=
            create_id_number_str(id_number, turnout_info1.hand,
            old_keep_dims2.turnout_info2.start_draw_x, turnout_info1.turnout_length,
            old_keep_dims2.turnout_info2.ipx_stored,   // 0
            old_keep_dims2.turnout_info2.fpx_stored,   // 0
            turnout_info1.plain_track_flag,
            old_keep_dims2.turnout_info2.semi_diamond_flag,
            any_loaded_rails_omitted);

        end;


        if templot_version < 209    // 209a mods...    23-04-2014
        then begin

          old_keep_dims2.turnout_info2.turnout_road_endx_infile :=
            turnout_info1.turnout_length - turnout_info1.origin_to_toe;
          //  mm default to overall length (from CTRL-1)

        end;

        // 210 does not exist !!!

        if templot_version < 211    // 211a mods...    3-07-2014
        then begin
          // adjustable turnout road compatibility mods 211a ...

          if turnout_info1.turnout_road_code = 2
          // adjustable - will be from 209 only
          then
            turnout_info1.turnout_road_is_adjustable := True
          else
            turnout_info1.turnout_road_is_adjustable := False;  // from 208 and earlier

        end;

        if templot_version < 212    // 212a mods...    5-03-2015
        then begin

          align_info.dummy_template_flag := False;   // 212a

          with proto_info do begin
            jt_slwide_pi := slwide_pi;
            // !!! single. inches full-size width of plain sleepers at rail joints. // 212a

            if name_str_pi = ' 00-SF    '    // 10 chars
            then begin
              name_str_pi := ' 4-SF     ';    // 10 chars
              //list_str_pi:=StringReplace(list_str_pi,'00-SF','4-SF ',[rfReplaceAll, rfIgnoreCase]);
              if fwe_pi = 1.75 then
                fwe_pi := 1.7;
            end;

            if name_str_pi = ' 00-BF    '    // 10 chars
            then begin
              if fw_pi = 1.25 then
                fw_pi := 1.3;
              if fwe_pi = 2.0 then
                fwe_pi := 1.9;
            end;

            if name_str_pi = ' 00-D0GAF '    // 10 chars
            then begin
              if fwe_pi = 1.75 then
                fwe_pi := 1.7;
            end;

            if name_str_pi = ' EM       '    // 10 chars
            then begin
              if fwe_pi = 1.75 then
                fwe_pi := 1.7;
            end;

            if name_str_pi = ' EM-18    '    // 10 chars
            then begin
              if fwe_pi = 1.75 then
                fwe_pi := 1.7;
            end;

          end;//with
        end;

        if templot_version < 213    // 213a mods...    13-10-2015
        then begin
          retain_entry_straight_on_make_flag := False;
          retain_entry_straight_on_mint_flag := False;

          old_keep_dims2.turnout_info2.diamond_switch_timbering_flag := False;
          // not a dummy Y-turnout
        end;

        if templot_version < 214    // 214a mods...    18-1-2016
        then begin

          with rail_info do begin
            knuckle_code_ri := 0;
            // integer;   0=normal, -1=sharp, 1=use custom knuckle_radius_ri
            knuckle_radius_ri := 72;
            // extended;  custom  - default 72 inches full-size
          end;

          with old_keep_dims2.turnout_info2.switch_info do begin

            num_slide_chairs := 0;                // byte 214a
            num_block_slide_chairs := 0;          // byte 214a
            num_block_heel_chairs := 0;           // byte 214a
            num_bridge_chairs_main_rail := 0;     // byte 214a
            num_bridge_chairs_turnout_rail := 0;  // byte 214a

          end;//with

          old_keep_dims2.turnout_info2.chairing_flag := False;

        end;

        if templot_version < 215    // 215a mods...    14-AUG-2017
        then begin

          with platform_trackbed_info do begin

            platform_ts_front_edge_ins_keep := platform_ts_front_edge_ins_keep + 28.25;
            // adjust to track centre-line   //  was :=28.75 from rail 093a  changed to 57" from centre-line in 215a
            platform_ms_front_edge_ins_keep := platform_ms_front_edge_ins_keep + 28.25;
            // adjust to track centre-line   //  was :=28.75 from rail 093a  changed to 57" from centre-line in 215a

            // 215a modified trackbed info, TS and MS separated ...

            trackbed_ms_width_ins_keep := OUT_OF_USE_trackbed_width_ins_keep / 2;
            //Single;
            trackbed_ts_width_ins_keep := OUT_OF_USE_trackbed_width_ins_keep / 2;
            //Single;

            cess_ms_width_ins_keep := OUT_OF_USE_cess_width_ins_keep;
            //Single;
            cess_ts_width_ins_keep := OUT_OF_USE_cess_width_ins_keep;
            //Single;

            if cess_ms_width_ins_keep = 30 then
              cess_ms_width_ins_keep := 27;  // update if still on default
            if cess_ts_width_ins_keep = 30 then
              cess_ts_width_ins_keep := 27;  // update if still on default

            draw_ms_trackbed_cess_edge_keep := OUT_OF_USE_draw_trackbed_cess_edge_keep;
            //boolean;
            draw_ts_trackbed_cess_edge_keep := OUT_OF_USE_draw_trackbed_cess_edge_keep;
            //boolean;

            trackbed_ms_start_mm_keep := 0;            //extended;
            trackbed_ms_length_mm_keep := def_req;     //extended;

            trackbed_ts_start_mm_keep := 0;            //extended;
            trackbed_ts_length_mm_keep := def_req;     //extended;

          end;//with


          with proto_info do begin

            name_str_pi := Copy(Trim(name_str_pi), 1, 9);
            // max 9 chars     gauge list name

            if fwe_pi > (fw_pi + 1.75 * scale_pi / 12) then
              fwe_pi := fw_pi + 1.75 * scale_pi / 12;
            // 215a  flangeway end gaps reduced to prototype flare angle if wider.

          end;//with


          old_keep_dims2.turnout_info2.crossing_info.curviform_timbering_keep :=
            False;   // 215a

        end;

        if templot_version < 216    // 216a mods...    15-NOV-2017
        then begin

          with align_info do begin

            reminder_flag := False;
            reminder_colour := clYellow;
            reminder_str := '';

          end;//with

        end;

        if templot_version < 217    // 217a mods...    4-12-2017
        then begin

          turnout_info1.turnout_road_is_minimum := False;

          old_keep_dims2.turnout_info2.crossing_info.main_road_endx_infile :=
            turnout_info1.turnout_length - turnout_info1.origin_to_toe;
          //  mm default to overall length (from CTRL-1)

          old_keep_dims2.turnout_info2.crossing_info.main_road_code := 0;
          // normal main-side exit

          rail_info.isolated_crossing_sw := False;
        end;

        if templot_version < 218    // 218a mods...    25-12-2017
        then begin

          turnout_info1.front_timbers_flag := True;
          turnout_info1.switch_timbers_flag := True;
          turnout_info1.closure_timbers_flag := True;
          turnout_info1.xing_timbers_flag := True;

          turnout_info1.approach_rails_only_flag := False;

          old_keep_dims2.turnout_info2.crossing_info.tandem_timber_code := 0;

        end;

        if templot_version < 219    // 219a mods...    06-03-2018
        then begin

          // include connectors for XTrackCAD in export DXF -- file only, not loaded to the control  ...

          with old_keep_dims2.turnout_info2 do begin

            dxf_connector_0 := False;   // CTRL-0
            dxf_connector_t := False;   // TEXITP
            dxf_connector_9 := False;   // CTRL-9

          end;//with

        end;

        if templot_version < 290   // TemplotMEC
        then begin
          turnout_info1.rolled_in_sleepered_flag := True;
          // default was True pre-223a
          file_format_code := 0;                             // D5 format
        end;

        if templot_version < 292   // Templot3
        then begin
          //
        end;

        templot_version := file_version;      // file now corresponds to current.
        Result := True;                          // and flag not saved.
      end;// if version differs

    end;//with old_keep_dims1
  end;//with okd
end;
//__________________________________________________________________________________________

function save_box(this_one, which_ones, rolling_backup: integer; save_str: string): boolean;

  // new file format including text 17-2-00. (v:0.48 on).
  // newer file format including unlimited shoves in StringList 1-5-01 (v:0.71.a on).

  // this_one       = if which_ones=-1, save only this index (echo, etc.)
  // which_ones     = -1 = this_one index only, 0=all templates  1=bgnd templates only  2=unused only  3=selected group only  4=library only.
  // rolling_backup = 0=normal save, 1=running backup,  -1=final backup on exit.
  // save_str       = if not '', is the path and file name to use, so don't ask him. (0.76.a 20-5-02).

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

var
  fsize: integer;
  box_str, backup_del_str: string;

  next_ti: Ttemplate_info;    // new 071 data type.
  group_count: integer;
  file_index: integer;

  box_file: file;               // untyped file.
  number_written: integer;
  i, len: integer;
  s: string;
  saved_cursor: TCursor;

  block_start: Tblock_start;
  block_ident: Tblock_ident;

  st, shove_count: integer;
  shove_timber_data: Tshove_for_file;

  save_bw: boolean; // 0.93.a

  /////////////////////////////////////////////////////////////

  procedure delete_any_control_templates;  // 0.93.a

  // goes only in file, so delete after saving.

  var
    i, n: integer;
    save_backw: boolean;

  begin

    save_backw := backup_wanted;
    // don't let this action change backup flag (list.OnChange)

    n := 0;
    while n < keeps_list.Count do begin

      if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.this_was_control_template = False  // normal template
      then begin
        Inc(n);
        CONTINUE;
        // leave this one.
      end;

      // delete it...

      Ttemplate(
        keeps_list.Objects[n]).template_info.keep_shove_list.Free;

      Ttemplate(keeps_list.Objects[n]).Free;
      keeps_list.Delete(n);
      memo_list.Delete(n);

    end;
    //while    // no need to increment n, it is now pointing to the next keep.

    backup_wanted := save_backw;   // restore backup flag
  end;
  /////////////////////////////////////////////////////////////

  procedure file_write_error;

  begin
    try
      CloseFile(box_file);
    except
      on EInOutError do
    end;  // close file if it's open.
    DeleteFile(box_str);
    if rolling_backup = 0 then
      file_error(box_str);
  end;
  /////////////////////////////////////////////////////////////

begin
  Result := False;      // init default.

  delete_any_control_templates;  // 0.93.a we may want to add a new one..

  // 0.93.a automatically add the control template to a file...

  // put it in the box to save file, then delete it after saving...

  if (which_ones = 0) and (turnoutx > 0) and ({check_if_abandoned=-1}abandon_calcs = False)
  // check not zero-length
  then begin
    save_bw := backup_wanted;     // don't let this action change backup flag (list.OnChange)

    store_unused(False, True);   // 0.93.a store the control template unused.

    backup_wanted := save_bw;     // restore backup flag
  end;

  try

    if keeps_list.Count < 1     // no control template added?
    then begin
      backup_wanted := False;
      save_done := True;
      Result := True;
      EXIT;
    end;

    case which_ones of
      -1:
        if (this_one >= 0) and (this_one < keeps_list.Count)   // index of only one to be saved.
        then
          group_count := 1
        else begin
          group_count := 0;             // keep compiler happy.
          EXIT;
        end;

      0:
        group_count := keeps_list.Count;      // save all.

      1: begin
        group_count := any_bgnd;            // save bgnd templates only.
        if group_count = 0 then begin
          alert_no_bgnd;
          EXIT;
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      2: begin
        group_count := any_unused;        // save unused templates only.
        if group_count = 0 then begin
          alert_no_unused;
          EXIT;
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      3: begin
        group_count := any_selected;        // save group members only.
        if group_count = 0 then begin
          if alert_no_group = True    // alert him, and does he want all?
          then
            EXIT
          else
            group_count := any_selected;   // now all of them.
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      4: begin
        group_count := any_library;        // save library templates only.
        if group_count = 0 then begin
          alert_no_library;
          EXIT;
        end;

        if group_count > keeps_list.Count then
          EXIT; // ???
      end;

      else begin               // ???
        group_count := 0;   // keep compiler happy.
        run_error(39);
        EXIT;
      end;
    end;//case

    if rolling_backup <> 0 then begin
      // set up to create alternate backup files..

      if FileExists(ebk1_str) = False then begin
        box_str := ebk1_str;         // use first file for backup.
        backup_del_str := ebk2_str;  // and delete the second one afterwards.
      end
      else begin
        box_str := ebk2_str;         // use second file for backup.
        backup_del_str := ebk1_str;  // and delete the first one afterwards.
      end;
    end
    else begin     // normal save...

      if save_str <> '' then
        box_str := save_str
      else begin
        with keep_form.save_dialog do begin         // set up the save dialog.

          if his_save_file_name <> '' then
            InitialDir := ExtractFilePath(his_save_file_name)   // use his previous folder.
          else
            InitialDir := Config.GetDir(cudiBoxes);              // or the default one.

          Filter := ' storage  box  contents  (*.box3)|*.box3';

          case which_ones of
            -1: begin                             // echo one only
              box_str := Config.GetFilePath(csfiE071Box);
              // echo goes in the folder we started in.
            end;

            // 0.79.a  yy_mm_dd  was yy-mm-dd

            0: begin
              Filename :=
                remove_invalid_str(Copy(Trim(box_project_title_str), 1, 20) +
                FormatDateTime(' yyyy_mm_dd_hhmm_ss', Date + Time)) + '.box3';
              // 0.79.a  20 chars was 15
              Title := '    save  all  templates  as ...';
            end;

            1: begin
              Filename :=
                remove_invalid_str('background' +
                FormatDateTime(' yyyy_mm_dd_hhmm_ss', Date + Time)) + '.box3';
              Title := '    save  background  templates  as ...';
            end;

            2: begin
              Filename :=
                remove_invalid_str('unused' + FormatDateTime(' yyyy_mm_dd_hhmm_ss',
                Date + Time)) + '.box3';
              Title := '    save  unused  templates  as ...';
            end;

            3: begin
              Filename :=
                remove_invalid_str('group' + FormatDateTime(' yyyy_mm_dd_hhmm_ss',
                Date + Time)) + '.box3';
              Title := '    save  selected  group  of  templates  as ...';
            end;

            4: begin
              Filename :=
                remove_invalid_str('library' + FormatDateTime(' yyyy_mm_dd_hhmm_ss',
                Date + Time)) + '.box3';
              Title := '    save  library  templates  as ...';
            end;

            else
              run_error(39);    // ???
          end;//case

          Filename := lower_case_filename(Filename);
          // 0.79.a   to underscores and lower case.

          if which_ones <> -1 then begin
            if Execute = False then
              EXIT;        // get his file name.
            box_str := FileName;

            if invalid_85a_file_name(box_str) = True then
              EXIT;

            box_str := ChangeFileExt(box_str, '.box3');   // force extension

            his_save_file_name := box_str;
            // so can use same folder next time.
          end;

        end;//with
      end;
    end;

    saved_cursor := Screen.Cursor;

    next_ti.keep_shove_list := Tshoved_timber_list.Create;

    try
      if rolling_backup = 0 then
        Screen.Cursor := crHourGlass;   // 0.93.a test added     // could take a while if big file.
      if Application.Terminated = False then
        Application.ProcessMessages;
      // so let the form repaint (if not called from quit_alert).


      fsize := 0;         // default init..
      file_index := 0;

      try

        try
          AssignFile(box_file, box_str);
          Rewrite(box_file, 1);               // open for writing, record size = 1 byte.

          for i := 0 to keeps_list.Count - 1 do begin     // first write the template data.

            Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.file_format_code
            := 1;
            // OT format      // put format in file

            // 0.94.a  fb_kludge templates are created on output/printing, and destroyed afterwards. Don't save any remaining..

            if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.fb_kludge_template_code <> 0 then
              CONTINUE;  // 0.94.a don't save kludge templates, if any found (error in print?)


            case which_ones of
              -1:
                if i <> this_one then
                  CONTINUE;
              1:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> 1 then
                  CONTINUE;  // bgnd only, ignore unused and library.
              2:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> 0 then
                  CONTINUE;  // unused only, ignore others.
              3:
                if Ttemplate(keeps_list.Objects[i]).group_selected = False then
                  CONTINUE;  // group only, ignore unselected.
              4:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> -1 then
                  CONTINUE;  // library only, ignore others.
            end;//case

            copy_template_info_from_to(False, Ttemplate(keeps_list.Objects[i]).template_info, next_ti);
            // next template.

            if file_index = (group_count - 1) then
              next_ti.keep_dims.box_dims1.box_ident := 'NX' + IntToStr(file_index)
            // last one in file. (string[10])
            else
              next_ti.keep_dims.box_dims1.box_ident := 'N ' + IntToStr(file_index);

            next_ti.keep_dims.box_dims1.id_byte := 255;
            // identify file as BOX3 rather than BOX      290a


            case rolling_backup of
              -1: begin    // final backup on exit ..
                next_ti.keep_dims.box_dims1.auto_restore_on_startup := False;
                // these three only read from the first keep in the file,
                next_ti.keep_dims.box_dims1.ask_restore_on_startup := True;
                // but go in every one.
                next_ti.keep_dims.box_dims1.box_save_done := save_done;
              end;

              0: begin    // normal box save (these are never read) ..
                next_ti.keep_dims.box_dims1.auto_restore_on_startup := False;
                // not used for normal file save/reload
                next_ti.keep_dims.box_dims1.ask_restore_on_startup := False;
                // not used for normal file save/reload
                next_ti.keep_dims.box_dims1.box_save_done := False;
              end;

              1: begin    // rolling backup..
                next_ti.keep_dims.box_dims1.auto_restore_on_startup := True;
                // if both True on loading = abnormal termination.
                next_ti.keep_dims.box_dims1.ask_restore_on_startup := True;
                next_ti.keep_dims.box_dims1.box_save_done := False;
              end;

            end;//case

            //  these go in every template but only the first or last in is read back...

            next_ti.keep_dims.box_dims1.project_for := Copy(box_project_title_str, 1, 49);
            // goes in every template but only the last in is read back.

            // 0.79.a  20-05-06  save grid info -- to be read from final template...

            //%%%% 0.91.d -- now also in user preferences, these used only if not prefs.

            next_ti.keep_dims.box_dims1.grid_units_code := grid_labels_code_i;
            next_ti.keep_dims.box_dims1.x_grid_spacing := grid_spacex;
            next_ti.keep_dims.box_dims1.y_grid_spacing := grid_spacey;

            //--------------------

            BlockWrite(box_file, next_ti.keep_dims, SizeOf(Tkeep_dims), number_written);
            // write all the data.

            if number_written <> SizeOf(Tkeep_dims) then begin
              file_write_error;
              Result := False;
              EXIT;
            end;

            Inc(file_index);     // first one = 0.
          end;//next i

          for i := 0 to keeps_list.Count - 1 do begin        // now add the texts.

            // 0.94.a  fb_kludge templates are created on printing, and destroyed afterwards. Don't save any remaining..

            if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.fb_kludge_template_code <> 0 then
              CONTINUE;

            case which_ones of
              -1:
                if i <> this_one then
                  CONTINUE;
              1:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> 1 then
                  CONTINUE;   // bgnd only, ignore unused and library.
              2:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> 0 then
                  CONTINUE;   // unused, ignore others
              3:
                if Ttemplate(keeps_list.Objects[i]).group_selected = False then
                  CONTINUE;   // group only, ignore unselected.
              4:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> -1 then
                  CONTINUE;   // library only, ignore others.
            end;//case

            s := remove_esc_str(keeps_list.Strings[i]) + Char($1B) + remove_esc_str(
              memo_list.Strings[i]) + Char($1B) + Char($1B);
            // use ESC chars as terminators, plus one for luck on the end.

            UniqueString(s);  // make sure it's in continuous memory.

            len := Length(s) * SizeOf(Char);

            BlockWrite(box_file, len, SizeOf(integer), number_written);
            // first the length as an integer (4 bytes)
            if number_written <> SizeOf(integer) then begin
              file_write_error;
              Result := False;
              EXIT;
            end;

            BlockWrite(box_file, s[1], len, number_written);   // then the text.
            if number_written <> len then begin
              file_write_error;
              Result := False;
              EXIT;
            end;

          end;//next i
          // now add the DATA BLOCKS section...

          s := '_85A_|    ';  // start marker.

          UniqueString(s);  // make sure it's in continuous memory.

          BlockWrite(box_file, s[1], 8, number_written);
          // 8 bytes of '_85A_|  ' as a DATA BLOCKS start marker.
          if number_written <> 8 then begin
            file_write_error;
            Result := False;
            EXIT;
          end;

          with block_start do begin
            version_number := file_version;
            zero1 := 0;
            zero2 := 0;
            zero3 := 0;
          end;//with

          BlockWrite(box_file, block_start, SizeOf(Tblock_start), number_written);
          // 16 bytes = version number + 12 bytes of zero (spares).
          if number_written <> SizeOf(Tblock_start) then begin
            file_write_error;
            Result := False;
            EXIT;
          end;

          // now the data blocks for each of the loaded templates...

          file_index := 0;     // re-init for the index count.

          for i := 0 to keeps_list.Count - 1 do begin

            if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.fb_kludge_template_code <> 0 then
              CONTINUE;

            case which_ones of
              -1:
                if i <> this_one then
                  CONTINUE;
              1:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> 1 then
                  CONTINUE;   // bgnd only, ignore unused and library.
              2:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> 0 then
                  CONTINUE;   // unused only.
              3:
                if Ttemplate(keeps_list.Objects[i]).group_selected = False then
                  CONTINUE;   // group only, ignore unselected.
              4:
                if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.bgnd_code_077
                  <> -1 then
                  CONTINUE;   // library only.
            end;//case

            copy_template_info_from_to(False, Ttemplate(keeps_list.Objects[i]).template_info, next_ti);
            // next template.

            // first block is the shove timber data...

            // shove data = code 10. 4 bytes containing the count of shoved timbers,
            //                       + a series of Tshove_for_file data records for each one.

            shove_count := next_ti.keep_shove_list.Count;

            with block_ident do begin

              segment_length := SizeOf(integer) + shove_count * SizeOf(Tshove_for_file);
              f_index := file_index;
              block_code := 10;         // = timber shove data.
              spare_zeroes := 0;

            end;//with

            BlockWrite(box_file, block_ident, SizeOf(Tblock_ident), number_written);
            // the data block ident.
            if number_written <> SizeOf(Tblock_ident) then begin
              file_write_error;
              Result := False;
              EXIT;
            end;

            // now the shove data segment itself..

            BlockWrite(box_file, shove_count, SizeOf(integer), number_written);
            // first the count of shoved timbers.
            if number_written <> SizeOf(integer) then begin
              file_write_error;
              Result := False;
              EXIT;
            end;

            if shove_count > 0 then begin

              for st := 0 to shove_count - 1 do begin
                shove_timber_data.copy_from(next_ti.keep_shove_list[st]);

                BlockWrite(box_file, shove_timber_data, SizeOf(Tshove_for_file),
                  number_written);      // first the count of shoved timbers.
                if number_written <> SizeOf(Tshove_for_file) then begin
                  file_write_error;
                  Result := False;
                  EXIT;
                end;
              end;//next st
            end;

            // no more DATA BLOCKS yet defined for this template, so on to the next..

            Inc(file_index);     // first one = 0.
          end;//next i

          // all templates done, so add the end zeroes ident (zero-length data segment).

          with block_ident do begin
            segment_length := 0;
            f_index := 0;
            block_code := 0;
            spare_zeroes := 0;
          end;//with

          BlockWrite(box_file, block_ident, SizeOf(Tblock_ident), number_written);
          // finally the end zeroes.
          if number_written <> SizeOf(Tblock_ident) then begin
            file_write_error;
            Result := False;
            EXIT;
          end;

        except
          on EInOutError do begin
            file_write_error;
            Result := False;
            EXIT;
          end;
        end;//try-except

        fsize := FileSize(box_file);      // (file must be open to get the size).

        if (FileExists(box_str) = False) or (fsize = 0)                  // ???
        then begin
          file_write_error;
          Result := False;
          EXIT;
        end;

      finally
        try
          CloseFile(box_file);
        except
          on EInOutError do
        end;  // close file if it's open.
      end;//try

      // file now exists and something in it...

      if (which_ones = 0) and (rolling_backup = 0) and (save_str = '')
      // normal save of all templates..
      then begin
        keep_form.box_file_label.Caption := ' last saved to :  ' + box_str;
        keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
        // in case too long for caption

        // 0.82.a  control_room_form.statusbar_label.Caption:=' templates'+ExtractFileName(keep_form.file_label.Caption);

        saved_box_str := box_str;  // for print of box contents list.
        reloaded_box_str := '';    // ditto.

        save_done := True;  // this boxful has been saved.
      end;

      if (which_ones <> -1) and (rolling_backup = 0) and (save_str = '')
      // normal save of any templates..
      then
        boxmru_update(box_str);                                 // 0.82.a  update the mru list.

      if rolling_backup <> 0 then
        DeleteFile(backup_del_str);   // delete the previous backup file.

      Result := True;

    finally
      next_ti.keep_shove_list.Free;
      Screen.Cursor := saved_cursor;
    end;//try

  finally

    // 0.93.a file saved or not, now remove any control templates...

    delete_any_control_templates;

  end;//try
end;
//______________________________________________________________________________________

function load_storage_box(normal_load, old_templot_folder: boolean; file_str: string;
  load_backup, make_lib: boolean; var append: boolean;
  var last_bgnd_loaded_index: integer): boolean;
  // load a file of templates into the keeps box.

  // normal_load True = for use. False = for file viewer.
  // if file_str not empty it is the file name to load.
  // return True any templates loaded/added.
  // also return any change to append.
  // also return last_bgnd_loaded_index, highest bgnd template loaded (for minting).

const
  ask_restore_str: string = '      `0Restore On Startup`9' +
    '||Your work in progress can be restored from your previous working session with Templot0.'
    +
    '||This means restoring your storage box contents `0(if any)`7, background track plan drawing `0(if any)`7, and control template.'
    +
    '||This is done independently of any saving to data files which you may have performed.' +
    '||If you answer "no thanks" the previous data can be restored later by selecting the `0FILES > RESTORE PREVIOUS`1 menu item on the storage box menus.' + '||tree.gif The restore feature works correctly even if your previous session terminated abnormally as a result of a power failure or system malfunction, so there is no need to perform repeated saves as a precaution against these events.' + '||rp.gif The restore feature does not include your Background Shapes or Sketchboard files, which must be saved and reloaded separately as required.' + '||rp.gif If you run two instances of Templot0 concurrently (not recommended for Windows 95/98/ME) from the same `0\TEMPLOT\`2 folder,' + ' the restore data will be held in common between the two. To prevent this happening, create and run the second instance from a different folder (directory).';

var
  test_box_file: file;                 // untyped file for testing format.
  i, n, fsize{,timb_index}: integer;
  loaded_str, box_str, ixt_str, ident: string;
  no_ixt: boolean;
  old_count: integer;                // for append.
  resave_needed: boolean;
  s, info_string, memo_string, _str: string;
  saved_cursor: TCursor;
  restored_save_done: boolean;

  old_next_data: Told_keep_data;
  new_next_data: Tnew_keep_data;

  this_ti: Ttemplate_info;
  _071_format: boolean;
  number_read: integer;

  inbyte: byte;

  saved_control: Ttemplate_info;
  saved_notch: Tnotch;

  saved_control_name_str: string;
  saved_control_memo_str: string;

  ///////////////////////////////////////////////////////////////

  function load_new_format: boolean;

  var
    box_file: file;                // new format untyped file.
    n, i, len: integer;
    n_valid: boolean;
    block_start: Tblock_start;
    block_ident: Tblock_ident;

    /////////////////////////////////

    procedure read_file_error;

    begin
      try
        CloseFile(box_file);
      except
        on EInOutError do
      end;  // close file if it's open.

      if append = False then
        clear_keeps(False, False)     // error reloading, clear all.
      else
        clear_keep(n);               // error adding, we already created the list entry for it.

      if load_backup = False then
        file_error(box_str);
    end;
    /////////////////////////////////

    function load_shove_block(t_index, seg_len: integer): boolean;
      // for a single template.

    var
      shove_timber_data: Tshove_for_file;

      shove_count, st: integer;
      total_read: integer;

    begin
      Result := False;    // default init.
      total_read := 0;    // number of bytes read.

      // first get the count of shoved timbers for this template...

      if EOF(box_file) = True then
        EXIT;
      BlockRead(box_file, shove_count,
        SizeOf(integer), number_read);
      total_read := total_read + number_read;
      if (number_read <> SizeOf(integer)) or (total_read > seg_len) then begin
        try
          CloseFile(box_file);
        except
          on EInOutError do
        end;  // close file if it's open.
        if load_backup = False then
          file_error(box_str);
        EXIT;
      end;

      if seg_len <> (SizeOf(integer) + shove_count * SizeOf(Tshove_for_file)) then
        EXIT;  // the integer is the shove count just read.

      if (t_index < 0) or (t_index > (keeps_list.Count - 1)) then
        EXIT;

      if shove_count > 0
      // now get the data for each shoved timber...
      then begin
        st := 0;    // keep compiler happy.

        with Ttemplate(keeps_list.Objects[t_index]).template_info.keep_shove_list do
        begin

          if Count <> 0 then
            EXIT;   // !!! shove list should be empty (created in init_ttemplate).

          repeat

            if EOF(box_file) = True then
              EXIT;
            BlockRead(box_file,
              shove_timber_data, SizeOf(Tshove_for_file), number_read);
            total_read := total_read + number_read;
            if (number_read <> SizeOf(Tshove_for_file)) or
              (total_read > seg_len) then begin
              try
                CloseFile(box_file);
              except
                on EInOutError do
              end;  // close file if it's open.
              if load_backup = False then
                file_error(box_str);
              EXIT;
            end;

            try
              st :=
                Add(Tshoved_timber.CreateFrom(shove_timber_data));
            except
              EXIT;       // memory problem?
            end;//try
          until (st = shove_count - 1) or (total_read = seg_len);
        end;//with
      end;
      Result := True;
    end;
    ///////////////////////////////////

  begin
    Result := False;    // init default
    try
      n_valid := False;   // default for error exits.
      n := old_count;     // keep compiler happy.
      try
        AssignFile(box_file, box_str);
        Reset(box_file, 1);              // open for reading, record size = 1 byte.

        repeat
          try
            n_valid := False;   // default for error exits.

            n := keeps_list.AddObject('no information available', Ttemplate.Create);
            // create and append a new line in keeps list (temporary strings).
            if memo_list.Add('no memo notes available') <> n then
              run_error(197);     // and memo list. Ensure indices correspond.
          except
            alert(1, '      memory  problem',
              '|||Unable to load templates from the file into your storage box because of memory problems.'
              + '||(Loading terminated after  ' + IntToStr(n - old_count) +
              '  templates.)',
              '', '', '', '', '', 'cancel  reload', 0);
            try
              CloseFile(box_file);
            except
              on EInOutError do
            end;  // close file if it's open.
            if append = False then
              clear_keeps(False, False);
            EXIT;
          end;//try

          init_ttemplate(n);

          n_valid := True;     // got a valid new index to the lists.

          BlockRead(box_file, old_next_data, SizeOf(Told_keep_data), number_read);
          // read bytes.


          s := old_next_data.old_keep_dims1.box_dims1.box_ident;

          if (number_read <> SizeOf(Told_keep_data)) or
            ((s <> ('N ' + IntToStr(n - old_count))) and (s <> ('NX' + IntToStr(n - old_count))))
          // error reading, or this is not a template.
          then begin
            read_file_error;
            EXIT;
          end;

          if version_mismatch(old_next_data) = True then
            resave_needed := True;  // check for version mismatch.

          // !!! version_mismatch must be done first - sets bgnd_code_077...

          if make_lib = True then
            old_next_data.old_keep_dims1.box_dims1.bgnd_code_077 := -1;
          // make it a library template.

          this_ti.keep_shove_list := Tshoved_timber_list.Create;

          this_ti.keep_dims := Tkeep_dims(old_next_data);

          copy_template_info_from_to(True, this_ti, Ttemplate(
            keeps_list.Objects[n]).template_info);  // True = free the shove list.

          if (append = True) and (make_lib = False) and
            (keep_form.add_ignore_group_menu_entry.Checked = False) then
            Ttemplate(keeps_list.Objects[n]).group_selected := True;
          // group select added template.

        until Copy(s, 1, 2) = 'NX';      // last template marker.

        Result := True;   // return good result, even if we don't get the texts.

        if EOF(box_file) = True then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          EXIT;
          // there was no text in the file
        end;

      except
        on EInOutError do begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);

          if append = False then
            clear_keeps(False, False)                   // error reloading, clear all.
          else
          if n_valid = True then
            clear_keep(n);  // error adding, we already created the list entry for it.
          EXIT;
        end;
      end;//try-except

      // got the data, now load the text strings (don't clear the data if it fails) ...

      for n := old_count to keeps_list.Count - 1 do begin
        // now get the proper texts.

        BlockRead(box_file, len, SizeOf(integer), number_read);
        // first get the length as an integer (4 bytes)

        if number_read <> SizeOf(integer) then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        // read len bytes into string s...

        s := '';
        while Length(s) < (len div SizeOf(Char)) do
          s := s + '0';

        s := s + '00';        // two more for safety.

        UniqueString(s);  // make sure it's in continuous memory

        if Length(s) > (len div SizeOf(Char)) then
          BlockRead(box_file, s[1], len, number_read);

        if number_read <> len then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        i := Pos(Char($1B), s);          // find info part terminator.

        if i <> 0 then begin
          info_string := Copy(s, 1, i - 1);
          // info string (don't include the ESC).
          Delete(s, 1, i);
          // remove info string and terminator from input.

          i := Pos(Char($1B), s);             // find memo part terminator.

          if i <> 0 then begin
            memo_string := Copy(s, 1, i - 1);
            // memo string (don't incude the ESC).

            // we don't change either unless we've got both..

            keeps_list.Strings[n] := remove_esc_str(info_string);
            // remove any ESC is belt and braces...
            memo_list.Strings[n] := remove_esc_str(memo_string);
          end;
        end;
      end;//next n
      // now can load any trailing data blocks...

      if (_071_format = True) and (EOF(box_file) = False) then begin

        // The 071 file format is the same as the 048 format with the addition of "Data Blocks" at the end of the file.
        // To create the 048 format the shove timber data is duplicated in the template data record (first 30 shoved timbers only).

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

        // DATA BLOCKS are then repeated until the END BLOCK, which comprises

        // 16 byte Tblock_ident comprising all zeroes (segment length=0).

        // first find the starting underscore character for the trailing data blocks...

        inbyte := 0;
        repeat
          if EOF(box_file) = True then
            EXIT;
          BlockRead(box_file, inbyte, 1, number_read);  // read 1 byte
          if number_read <> 1 then begin
            try
              CloseFile(box_file);
            except
              on EInOutError do
            end;  // close file if it's open.
            if load_backup = False then
              file_error(box_str);
            EXIT;
          end;
        until Chr(inbyte) = '_';

        // then the magic number...

        s := '12345678';    // 8 bytes  (1 extra for safety).
        UniqueString(s);  // make sure it's in continuous memory

        if EOF(box_file) = True then
          EXIT;
        BlockRead(box_file, s[1], 7, number_read);
        if number_read <> 7 then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        if Copy(s, 1, 5) <> '85A_|' then
          EXIT;  // magic number - ignore the final '.x' build letter.

        if EOF(box_file) = True then
          EXIT;
        BlockRead(box_file, block_start, SizeOf(Tblock_start), number_read);
        if number_read <> SizeOf(Tblock_start) then begin
          try
            CloseFile(box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        if block_start.version_number <> loaded_version then
          EXIT; // double check we are ok to continue.

        // ...ignore any other data in the block start record in this version (071).

        repeat     // get all the data blocks

          // get the ident for the next data block..

          if EOF(box_file) = True then
            EXIT;
          BlockRead(box_file, block_ident, SizeOf(Tblock_ident), number_read);
          if number_read <> SizeOf(Tblock_ident) then begin
            try
              CloseFile(box_file);
            except
              on EInOutError do
            end;  // close file if it's open.
            if load_backup = False then
              file_error(box_str);
            EXIT;
          end;

          if block_ident.segment_length = 0 then
            EXIT;    // end of data blocks.

          n := old_count + block_ident.f_index;   // loaded template list index.

          case block_ident.block_code of

            10:
              if load_shove_block(n, block_ident.segment_length) = False then
                EXIT;  // code 10 = shove data block for this template.

            else begin    // no other codes defined for version 071. 6-5-01.

              for i := 0 to (block_ident.segment_length - 1) do begin
                // so skip this block (later version file?).

                if EOF(box_file) = True then
                  EXIT;
                BlockRead(box_file, inbyte, 1, number_read);
                if number_read <> 1 then begin
                  try
                    CloseFile(box_file);
                  except
                    on EInOutError do
                  end;  // close file if it's open.
                  if load_backup = False then
                    file_error(box_str);
                  EXIT;
                end;
              end;//next i
            end;
          end;//case  // no other codes defined for version 071. 6-5-01.

        until EOF(box_file) = True;
        // shouldn't get here, EXITs on a zero segment length.

      end;//if 071 format

    finally
      try
        CloseFile(box_file);
      except
        on EInOutError do
      end;  // close file if it's open.

      if keep_form.ignore_unused_menu_entry.Checked = True
      // finally remove any unwanted unused templates which got loaded...
      then begin
        n := old_count;
        while n < keeps_list.Count do begin
          if (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 =
            0) and (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.this_was_control_template = False)  // 0.93.a

          then
            clear_keep(n)
          else
            Inc(n);
        end;//while
      end;
    end;//try
  end;
  /////////////////////////////////////////////////////////////

begin

  Result := False;               // init.
  last_bgnd_loaded_index := -1;  // init.

  if (append = False) and (keeps_list.Count > 0) and (load_backup = False) and
    (file_str = '') // something already there ?
  then begin
    if save_done = False                 // and not saved...
    then begin
      i := alert(7, '      reload  storage  box  -  save  first ?',
        'Your storage box contains one or more templates which have not yet been saved.' +
        ' Reloading your storage box will replace all of the existing contents and background drawing.'
        + '||These templates can be restored by clicking the `0UNDO RELOAD / UNDO CLEAR`1 menu item.'
        + ' But if any of these templates may be needed again, you should save them in a named data file.'
        + '||Do you want to save the existing contents before reloading?' +
        '||Or add the new templates to the existing contents instead?', '',
        '', 'add  new  templates  to  existing    ',
        'replace  existing  contents  without  saving    ', 'cancel  reload    ',
        'save  existing  contents  before  reloading      ', 0);
      case i of
        3:
          append := True;
        5:
          EXIT;
        6:
          if save_box(0, 0, 0, '') = False then
            EXIT;     // go save all the keeps box.
      end;//case
    end
    else begin      //  it has been saved...
      i := alert(7, '      reload  storage  box  -  clear  first ?',
        'Your storage box contains one or more existing templates.' +
        ' Reloading your storage box will replace all of the existing contents and background drawing.'
        + ' These templates can be restored by clicking the `0UNDO RELOAD / UNDO CLEAR`1 menu item.'
        + '||Are you sure you want to replace the existing templates?' +
        '||Or add the new templates to the existing contents instead?', '',
        '', '', 'add  new  templates  to  existing  ', 'cancel  reload    ',
        'reload  and  replace  existing  contents      ', 0);
      case i of
        4:
          append := True;
        5:
          EXIT;
      end;//case
    end;
  end;

  if load_backup = True then begin
    box_str := '';
    if FileExists(ebk1_str) = True then
      box_str := ebk1_str;
    if FileExists(ebk2_str) = True then
      box_str := ebk2_str;

    if box_str = '' then
      EXIT;    // no file to load.
  end
  else begin
    if file_str = '' then begin
      with keep_form.load_dialog do begin
        if append = False then
          Title := '    load  or  reload  storage  box  from  file ..'
        else begin
          if make_lib = True then
            Title := '    add  library  templates  from  file ..'
          else
            Title := '    add  templates  from  file ..';
        end;


        if his_load_file_name <> '' then
          InitialDir := ExtractFilePath(his_load_file_name)
        else
          InitialDir := Config.GetDir(cudiBoxes);

        Filter := ' storage  box  contents  (*.box3)|*.box3';
        Filename := '*.box3';

        if Execute = False then
          EXIT;          // get the file name.

        box_str := FileName;
        his_load_file_name := box_str;
        // so we can use the same folder next time.

      end;//with
    end
    else
      box_str := file_str;                       // file name supplied by caller.

    ixt_str := ChangeFileExt(box_str, '.ixt');

    if FileExists(box_str) = False then begin
      alert(5, '    error  -  file  not  found',
        '||The file :' + '||' + box_str +
        '||is not available. Please check that the file you require exists in the named folder. Then try again.'
        + '||No changes have been made to your storage box.',
        '', '', '', '', 'cancel  reload', '', 0);
      EXIT;
    end;
  end;

  // added 0.78.d 19-02-03...
  saved_control.keep_shove_list := Tshoved_timber_list.Create;
  fill_kd(saved_control);                             // save control template.
  saved_control_name_str := current_name_str;
  saved_control_memo_str := current_memo_str;

  resave_needed := False;                         // init.
  restored_save_done := False;                    // init.
  loaded_version := 50000;                        // init for lowest template version in the file.
  later_file := False;                            // init.

  loading_in_progress := True;
  // 208c lock out any auto backups while loading -- in case any dialogs shown and OnIdle fires

  try // 208c

    if (append = True) and (keep_form.add_new_group_menu_entry.Checked = True) then
      clear_all_selections;   // he wants added templates to form a new group.

    // begin loading...

    saved_cursor := Screen.Cursor;

    try
      Screen.Cursor := crHourGlass;        // could take a while if big file.
      if Application.Terminated = False then
        Application.ProcessMessages;       // so let the form repaint.

      if append = False then
        clear_keeps(False, True);
      // first clear all existing (sets save_done:=True), and save existing for undo.

      old_count := keeps_list.Count;          // for append.

      try
        AssignFile(test_box_file, box_str);      // set the file name (untyped file).
        FileMode := 0;
        // read only (this is a global setting for all subsequent Resets).
        Reset(test_box_file, 1);                 // open for reading, record size = 1 byte.

        //  Tkeep_dims1=record      // first part of a Tkeep_dims record.

        BlockRead(test_box_file, old_next_data.old_keep_dims1, SizeOf(Tkeep_dims1), number_read);
        // read bytes.
        if number_read <> SizeOf(Tkeep_dims1) then begin
          try
            CloseFile(test_box_file);
          except
            on EInOutError do
          end;  // close file if it's open.
          if load_backup = False then
            file_error(box_str);
          EXIT;
        end;

        CloseFile(test_box_file);    // and close the file. (Re-open later.)
      except
        on EInOutError do begin
          if load_backup = False then
            file_error(box_str);
          if append = False then
            clear_keeps(False, False);
          EXIT;
        end;
      end;//try-except

      with old_next_data.old_keep_dims1.box_dims1 do begin
        s := box_ident;
        if Copy(s, 1, 1) = 'N'              // it's in new file format (v:0.48 on, 17-2-00)
        then begin

          if templot_version > 70
          // mods for v:0.71.a  (shove-timber data blocks appended to file). 2-5-01.
          then
            _071_format := True
          else
            _071_format := False;

          if load_backup = True then begin

            if templot_version > 62 then
              restored_save_done := box_save_done;     // mods 23-6-00 for version 0.63

            if startup_restore_pref = 1 then
              EXIT;   //%%%%   0=ask, 1=don't restore, 2=restore without asking

            if (auto_restore_on_startup = False) or (ask_restore_on_startup = False)
            // if both True??? - must have been abnormal termination, so reload without asking.
            then begin

              if startup_restore_pref = 0
              //%%%%   0=ask, 1=don't restore, 2=restore without asking
              then begin
                if auto_restore_on_startup = False
                // these two only read from the first keep in the file..
                then begin
                  if ask_restore_on_startup = True
                  // he wanted to be asked first.
                  then begin

                    alert_box.
                      preferences_checkbox.Checked := False;       //%%%%
                    if user_prefs_in_use = True then
                      alert_box.preferences_checkbox.Show;

                    repeat
                      i :=
                        alert(4, '    restore  previous  work ?',
                        ' |Do you want to restore your work in progress from your previous Templot0 session?| ',
                        '', '', '', 'more  information', 'no  thanks', 'yes  please  -  restore  previous  work', 4);
                      case i of
                        4:
                          alert_help(0, ask_restore_str, '');
                        //%%%%% 5: EXIT;
                      end;//case
                    until i <> 4;

                    //%%%%   0=ask, 1=don't restore, 2=restore without asking

                    if alert_box.preferences_checkbox.Checked   //%%%%
                    then
                      startup_restore_pref := (i - 4)         // 5 or 6 = 1 or 2
                    else
                      startup_restore_pref := 0;

                    alert_box.
                      preferences_checkbox.Hide;

                    if i = 5 then
                      EXIT;  //%%%%

                  end
                  else
                    EXIT;     // restore not wanted.
                end;
                // 0.93.a else keep_form.auto_ebk_load_menu_entry.Checked:=True;      // radio item (maintain this option only for next time).

              end;// ask startup pref
            end;//not abnormal termination
          end;//reload backup

          // load the file...

          wait_form.cancel_button.Hide;
          wait_form.waiting_label.Caption := 'loading  templates ...';

          wait_form.waiting_label.Width :=
            wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // 205b bug fix for Wine

          if normal_load = True then
            wait_form.Show;  // 208d version warnings off for file viewer

          if Application.Terminated = False then
            Application.ProcessMessages;           // let the wait form fully paint.
          if load_new_format = False then
            EXIT;    // go get file in new format.
        end;

      end;//with
      // file loaded...

      if (file_str = '') then
        loaded_str := box_str     // file name from the "open" dialog.
      else begin
        if ExtractFileExt(file_str) = '.box3' then
          loaded_str := file_str
        else
          loaded_str := 'data file';        // don't confuse him with internal file names.
      end;

      if (ExtractFileExt(loaded_str) = '.box3') and (normal_load = True)
      // 208d not for file viewer
      then
        boxmru_update(loaded_str);                              // 0.82.a  update the mru list.

      // file loaded, check it and update the background drawing...

      if append = False then begin
        with old_next_data.old_keep_dims1.box_dims1 do begin

          box_project_title_str := project_for;  // change the title to the one loaded last.

          //     0.79.a 20-05-06  -- saved grid info -- read from last template only...
          //     0.91.d -- read these only if prefs not being used on startup.

          if (grid_units_code <> 0) and (user_prefs_in_use = False)
          // 0.79 file or later --- change grid to as loaded...
          then begin

            grid_labels_code_i := grid_units_code;

            grid_spacex := x_grid_spacing;
            grid_spacey := y_grid_spacing;

            if ruler_units = 0 then
              update_ruler_div;   // 0.93.a ruler as grid option

          end;// if 0.79 or later

        end;//with old_next_data.old_keep_dims1

        save_done := not resave_needed;        // this boxful matches file.
        if load_backup = False then begin
          keep_form.box_file_label.Caption := ' last reloaded from :  ' + loaded_str;
          keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
          // in case too long for caption

          saved_box_str := loaded_str;
          // for print of box contents list.
          reloaded_box_str := '|    ' + loaded_str;
          // ditto.
        end
        else begin
          save_done := restored_save_done;
          // had it been saved?
          keep_form.box_file_label.Caption := ' restored on startup';
          keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
          // in case too long for caption

          saved_box_str := 'startup restore';
          // for print of box contents list.
          reloaded_box_str := '|    startup restore';               // ditto.
        end;
      end
      else begin                 // appending...
        //save_done:=False;
        keep_form.box_file_label.Caption := ' last added from :  ' + loaded_str;
        keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
        // in case too long for caption

        reloaded_box_str := reloaded_box_str + '|    ' + loaded_str;
        // for print of box contents list.
      end;

      if append = False then
        current_state(0)       // update or create listbox entries, need names for refresh...
      else
        current_state(-1);

      // refresh or clear backgrounds for newly loaded keeps...

      if keeps_list.Count <= old_count then
        EXIT;   // cleared on error or nothing loaded.

      with keep_form do begin

        if append = False then
          i := 0
        else
          i := old_count;

        for n := i to (keeps_list.Count - 1) do begin
          if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 =
            1 then begin
            if update_background_menu_entry.Checked = True then begin
              last_bgnd_loaded_index := n;
              // update index to highest loaded bgnd (for minting).
              list_position := n;
              // put new keep on background.
              copy_keep_to_background(n, False, True);
              // don't update info, reloading=True.
            end
            else begin
              with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin
                bgnd_code_077 := 0;          // make it unused instead.
                pre077_bgnd_flag := False;
                // in case reloaded in older version than 0.77.a
              end;//with
            end;
          end;
        end;//for

        if update_background_menu_entry.Checked = True then
          pad_form.fit_bgnd_menu_entry.Click;  // show the new background.

      end;//with

      backup_wanted := True;                    // file loaded ok, update the backup.
      Result := True;                           // file loaded.

    finally
      wait_form.Close;
      Screen.Cursor := saved_cursor;
      current_state(-1);                   // tidy up after any error exits.

      copy_keep(saved_control);            // retrieve saved current...
      current_name_str := saved_control_name_str;
      current_memo_str := saved_control_memo_str;
      info_form.ref_name_label.Caption := current_name_str;

      saved_control.keep_shove_list.Free;

    end;//try

    if (later_file = True) and (normal_load = True)   // normal_load 208d (off for file viewer)
    then begin
      alert(1, 'php/980    later  file   -   ( from  version  ' + FormatFloat(
        '0.00', loaded_version / 100) + ' )',
        'The file which you just reloaded contained one or more templates from a later version of Templot0 than this one.'
        +
        ' Some features may not be available or may be drawn differently.' +
        '||The earliest loaded template was from version  ' + FormatFloat('0.00', loaded_version / 100) +
        '|This version of Templot0 is  ' + GetVersionString(voShort) +
        '||Please refer to the Templot web site at  templot.com  for information about upgrading to the latest version, or click| <A HREF="online_ref980.85a">more information online</A> .',
        '', '', '', '', '', 'continue', 0);
    end;

    if (loaded_version < 200) and (normal_load = True)   // normal_load 208d (off for file viewer)
    then begin
      i := alert(2, 'php/980    old  file   -   ( from  version  ' +
        FormatFloat('0.00', loaded_version / 100) + ' )',
        'The file which you just reloaded contained one or more templates from an earlier version of Templot0.'
        + '||These have been modified to make them compatible with this version, but some features may now be drawn differently or require adjustment.'
        //+'||To re-create the templates from scratch in line with this version, click the blue bar below or select the PROGRAM > NORMALIZE ALL TEMPLATES menu item on the PROGRAM PANEL window.'
        + '||The earliest loaded template was from version  ' +
        FormatFloat('0.00', loaded_version / 100) + '|This version of Templot0 is  ' +
        GetVersionString(voShort) +
        '||Click for <A HREF="online_ref980.85a">more information online</A> about the differences between these two versions.'
        //+'||Please refer to the Templot web site at  templot.com  for information about the differences between these two versions.'
        + '||green_panel_begin tree.gif The template name labels are now shown in the boxed style by default.'
        + ' To revert to the previous style click the `0trackpad > trackpad background options > background name labels > transparent`1 menu item,|or click below.' + '||To hide the name labels, press the `0END`2 key on the keyboard, or the `0SHIFT+ENTER`2 keys, or click the `0trackpad > hide name labels`1 menu item, or click below.green_panel_end', '', '', 'hide  name  labels', 'change  to  transparent  name  labels', '', 'continue', 0);

      if i = 3 then
        hide_name_labels := True;

      if i = 4 then
        pad_form.transparent_names_menu_entry.Checked := True;    // radio item.
    end;

  finally
    loading_in_progress := False;  // 208c allow backups only after dialogs
  end;//try
end;
//_________________________________________________________________________________________

procedure reload_specified_file(par, add: boolean; file_str: string);
// used for command line parameters on startup.
// also for mru list. // 0.82.a  06-09-06
// also for file viewer // 208d

// also for dropped .box3 files 214a

// par=true if command line parameter   0.82.a

var
  i, hl: integer;
  append: boolean;

  old_count: integer;    // 214a

begin
  hl := 0;                        // init
  old_count := keeps_list.Count;  // init

  append := add;

  if par = True then begin
    if alert(7, '    load  or  reload  from  file',
      'Do you want to load your storage box and background drawing from' +
      '||' + ExtractFilePath(file_str) + '|' + ExtractFileName(file_str) + '   ?',
      '', '', '', '', 'no  thanks', 'yes,  load   ' + ExtractFileName(
      file_str) + '     ', 0) = 5 then
      EXIT;
  end;

  if (append = False) and (save_done = False) and (keeps_list.Count > 0)
  // check if saved...
  then begin
    i := alert(7, '    reload  storage  box  -  save  first ?',
      'Your storage box contains one or more templates which have not yet been saved.' +
      ' Reloading your storage box will replace all of the existing contents.',
      '', '', '', 'replace  existing  contents  without  saving', 'cancel  reload',
      'save  existing  contents  before  reloading', 0);
    case i of
      5:
        EXIT;
      6:
        if save_box(0, 0, 0, '') = False then
          EXIT;     // go save all the keeps box.
    end;//case
  end;

  try  // 208d

    if load_storage_box(True, False, file_str, False, False, append, hl) = True
    // hl= highest loaded index
    then begin
      if keeps_list.Count > 0 then begin

        if append = False then begin
          save_done := True;  // box contents matches loaded file.
          keep_form.box_file_label.Caption :=
            ' last reloaded from :  ' + file_str;
          keep_form.box_file_label.Hint := keep_form.box_file_label.Caption;
          // in case too long for caption
        end
        else begin
          if keeps_list.Count > old_count   // 214a
          then begin
            save_done := False;
            keep_form.box_file_label.Caption :=
              ' last added from :  ' + file_str;
            keep_form.box_file_label.Hint :=
              keep_form.box_file_label.Caption;  // in case too long for caption
          end;
        end;

        // 0.82.a control_room_form.statusbar_label.Caption:=' templates'+ExtractFileName(keep_form.file_label.Caption);
        reloaded_box_str := reloaded_box_str + '|    ' + file_str;
        // for print of box contents list.

        //if (append=False) and (hl>-1) and (hl<keeps_list.Count) then mint_final(hl);    // if something loaded mint from highest bgnd if he so wants.
        if append = True then
          EXIT;
        if (loaded_version < 93) and (hl > -1) and (hl < keeps_list.Count) then
          mint_final_or_copy_control(hl);
        // if something loaded mint from highest bgnd if he so wants.
        if (loaded_version > 92) then
          mint_final_or_copy_control(hl);
        // copy the control template if there is one in the file.

      end;
    end;

  finally   // 208d
    redraw_pad(True, False);
  end;
end;
//_________________________________________________________________________________________

procedure load_backup_file;       // called once only during startup.

// load backup file if it exists and is wanted.
var
  append: boolean;
  hl: integer;

begin
  append := False;
  if load_storage_box(True, False, '', True, False, append, hl) = True then begin
    if append = True then
      EXIT;
    if (loaded_version < 93) and (hl > -1) and (hl < keeps_list.Count) then
      mint_final_or_copy_control(hl);
    // if something loaded mint from highest bgnd if he so wants.
    if (loaded_version > 92) then
      mint_final_or_copy_control(hl);
    // copy the control template if there is one in the file.
  end;
end;
//________________________________________________________________________________________

procedure Tkeep_form.copy_to_pad_buttonClick(Sender: TObject);
// copy the current keep to the pad.

begin
  copy_keep_to_current(False, True, True, False);
end;
//____________________________________________________________________________________________

procedure copy_keep_to_current(on_reload, bgnd_options, name_options, mint_from: boolean);
// copy the current keep to the control template on the pad.

var
  ti: Ttemplate_info;
  i, n: integer;

begin
  with keep_form do begin
    if (keeps_list.Count < 1) or (list_position < 0) then
      EXIT;

    n := list_position;           // index to this one.

    keep_copied_from_index := n;  // make a note of the index we got it from.

    current_memo_str := memo_list.Strings[n];

    ti.keep_shove_list := Tshoved_timber_list.Create;

    try
      copy_template_info_from_to(False, Ttemplate(keeps_list.Objects[n]).template_info, ti);
      copy_keep(ti);
      // and make it current.

      copied_ref_str := ti.keep_dims.box_dims1.reference_string;

      if on_reload = False then begin
        save_hide := False;      // he will want to see it.

        if (bgnd_options = True) and (wipe_to_control_radiobutton.Checked = True) and
          (Ttemplate(keeps_list.Objects[n]).bg_copied = True) then begin
          if wipe_it(n) = False      // wipe the background and free drawing data.
          then begin
            alert(5, '    program  error',
              '||Sorry, there is a program error.' +
              '||The background will be cleared and rebuilt.',
              '', '', '', '', '', 'O K', 0);
            rebuild_background(False, True);
          end;
        end;

        if (bgnd_options = True)                              // 208a
          and (delete_to_control_radiobutton.Checked = True) then
          delete_keep(False, True);

        if bgnd_options = True then begin
          onto_notch := copy_onto_notch_menu_entry.Checked;
          copy_datum := copy_onto_datum_menu_entry.Checked;

          if close_on_copy_menu_entry.Checked = True then
            Close;//Hide;     // deactivate does the redraw.
        end
        else begin
          onto_notch := False;
          copy_datum := False;
        end;
      end
      else begin                    // reloading...
        onto_notch := False;
        copy_datum := False;

        if close_on_reload_menu_entry.Checked = True then begin
          Close;             // deactivate does the redraw of last reloaded.
          current_name_str := ti.keep_dims.box_dims1.reference_string;
          if mint_from = True then
            mint_new_current(0);  // 0.93.a test added //mint from last reloaded.
          EXIT;
          // do't do the statusbar_click, mint clears the current name.
        end
        else begin
          current_name_str := ti.keep_dims.box_dims1.reference_string;
          if mint_from = True then
            mint_new_current(0);  // 0.93.a test added  // mint from last reloaded (now current).
          EXIT;
          // do't do the statusbar_click, mint clears the current name.
        end;
      end;

      if name_options = True then begin
        if copy_name_menu_entry.Checked = True then
          statusbar_click(False);
        if new_name_menu_entry.Checked = True then
          statusbar_click(True);
      end
      else
        statusbar_click(False);        // default is to copy the name.

    finally
      ti.keep_shove_list.Free;
    end;//try
  end;//with keep_form.

  // see if name contains a tag... 213b

  if ((Pos('[', current_name_str) > 0) or (Pos(']', current_name_str) > 0)) then begin
    if retain_prefix_msg_pref = False then begin
      i := 0;  // keep compiler happy.

      alert_box.preferences_checkbox.Checked := False;
      alert_box.preferences_checkbox.Show;

      if (Length(current_name_str) - Length(
        StringReplace(current_name_str, ']', '', [rfReplaceAll]))) < 2  // number of ] in string
      then begin
        i :=
          alert(4, '     prefix  tag  present',
          'The template being copied into the control template has a prefix tag:||' +
          current_name_str + '||Do you want to retain this prefix tag on this template?' +
          '|||If you answer yes, subsequently storing this template will transfer this tag to the stored template(s). The retained tag and the template name can be edited by clicking the `0name...`1 button on the information panel.' + '|||If you answer no, subsequently stored templates will not have this tag and will not therefore be included in a group which is created using this tag.' + '|||If you are not sure, click the green bar.', '', '', '', 'no  -  clear  tag', '', 'yes  -  retain  prefix  tag', 0);

      end
      else begin    //  more than one tag...

        i :=
          alert(4, '     prefix  tags  present',
          'The template being copied into the control template has prefix tags:||' +
          current_name_str + '||Do you want to retain these prefix tags on this template?'
          + '|||If you answer yes, subsequently storing this template will transfer these tags to the stored template(s). The retained tags and the template name can be edited by clicking the `0name...`1 button on the information panel.' + '|||If you answer no, subsequently stored templates will not have these tags and will not therefore be included in a group which is created using them.' + '|||If you are not sure, click the green bar.', '', '', '', 'no  -  clear  tags', '', 'yes  -  retain  prefix  tags', 0);

      end;

      retain_prefix_msg_pref := alert_box.preferences_checkbox.Checked;
      alert_box.preferences_checkbox.Hide;

      retain_prefix_pref := (i = 6);
    end;

    if retain_prefix_pref = False then begin
      current_name_str := '';
      // clear tag(s), assume to clear name also, if any
      info_form.ref_name_label.Caption := '';
    end;

  end;
end;
//________________________________________________________________________________________

procedure Tkeep_form.escape_buttonClick(Sender: TObject);

begin
  Close;//Hide;
end;
//_______________________________________________________________________________________

procedure Tkeep_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.
end;
//________________________________________________________________________________________

procedure current_state(code: integer);    // show current state of keeps box.
// code 0 = make top of box the current keep.
//      1 = get current from the up-down buttons.
//      2 = get current from the list index.
//     -1 = leave current keep unchanged.

// N.B. the visible list is recreated from scratch every time,
// so that the numbering is updated after any deletions.
var
  n, k: integer;
  name_str: string;
  memo_text_str: string;
  ref_str, bg_str, group_str: string;

  empty_str, panel_str: string;  // 208a

begin

  with keep_form do begin
    Caption := '    storage  box  for :   ' + box_project_title_str;

    if distortions <> 0 then
      keep_form.distortion_warning_panel.Show
    else
      keep_form.distortion_warning_panel.Hide;

    if group_notch_linked = True then
      group_linked_warning_label.Show
    else
      group_linked_warning_label.Hide;

    if (code <> 2) or (keeps_list.Count <> keepform_listbox.Items.Count) then begin
      //keepform_listbox.Visible:=False;  // avoid flicker,
      keepform_listbox.Items.Clear;
      // refill the listbox every time to keep it in sync (unless it's just been clicked).
    end;

    //if code=2 then make_selection_list;       // listbox clicked so make a new list.

    total_bgnd_template_length := 0;      // 0.93.a init
    total_group_template_length := 0;

    total_bgnd_timbering_length := 0;     // 0.96.a init
    total_group_timbering_length := 0;

    smallest_bgnd_radius := max_rad;      // 208a init
    smallest_group_radius := max_rad;

    smallest_bgnd_radius_index := 0;      // 208a init
    smallest_group_radius_index := 0;

    box_menu.Enabled := (keeps_list.Count > 0);  // 208b

    if keeps_list.Count > 0 then begin
      if keepform_listbox.Items.Count = 0 then begin
        for n := 0 to keeps_list.Count - 1 do begin
          with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims do begin

            with box_dims1 do begin

              if bgnd_code_077 = 1 then begin
                total_bgnd_template_length :=
                  total_bgnd_template_length + turnout_info1.turnout_length;
                // 0.93.a  mm overall length.
                total_bgnd_timbering_length :=
                  total_bgnd_timbering_length + total_length_of_timbering;     // 0.96.a

                if smallest_bgnd_radius > turnout_info2.smallest_radius_stored
                // 208a
                then begin
                  smallest_bgnd_radius :=
                    turnout_info2.smallest_radius_stored;
                  smallest_bgnd_radius_index := n;
                end;
              end;

              if Ttemplate(keeps_list.Objects[n]).group_selected = True then begin
                total_group_template_length :=
                  total_group_template_length + turnout_info1.turnout_length;    // 0.93.a
                total_group_timbering_length :=
                  total_group_timbering_length + total_length_of_timbering;     // 0.96.a

                if smallest_group_radius >
                  turnout_info2.smallest_radius_stored     // 208a
                then begin
                  smallest_group_radius :=
                    turnout_info2.smallest_radius_stored;
                  smallest_group_radius_index := n;
                end;
              end;

              name_str := Trim(reference_string);

              if name_str <> '' then
                k := keepform_listbox.Items.Add(name_str)     // 208a mods
              else begin
                empty_str := 'no-name';  // default init (should not appear)

                if Copy(id_number_str, 1, 1) = 'P' then
                  empty_str := 'plain track';
                if Copy(id_number_str, 1, 1) = 'T' then
                  empty_str := 'turnout';
                if Copy(id_number_str, 1, 1) = 'D' then
                  empty_str := 'half-diamond';

                k := keepform_listbox.Items.Add(empty_str);
              end;

            end;//with
          end;//with

        end;//for
      end;

      //with keep_updown do begin
      //Max:=keeps_list.Count-1;                               // limit the selection range to the list contents.
      if keeps_list.Count > 1 then begin
        updown_panel.Visible := True;
        // no use for these if less than 2 templates in box...
        move_up_button.Visible := True;
        move_down_button.Visible := True;
        move_to_top_button.Visible := True;      // 0.93.a
        move_to_bottom_button.Visible := True;   // 0.93.a
      end
      else begin
        updown_panel.Visible := False;
        move_up_button.Visible := False;
        move_down_button.Visible := False;
        move_to_top_button.Visible := False;     // 0.93.a
        move_to_bottom_button.Visible := False;  // 0.93.a
      end;


      //with keepform_listbox do begin

      case code of
        -1:
          keepform_listbox.ItemIndex := list_position;
        // the up-down position takes precedence if any mismatch.
        0: begin                                          // set to top template;
          list_position := keeps_list.Count - 1;
          keepform_listbox.ItemIndex := keepform_listbox.Items.Count - 1;
        end;
        1:
          keepform_listbox.ItemIndex := list_position;
        2:
          list_position := keepform_listbox.ItemIndex;
      end;//case

      //keepform_listbox.Visible:=True;  // was avoiding flicker.


      //end;//with

      if list_position > keeps_list.Count - 1 then
        list_position := keeps_list.Count - 1;  // double check that index is valid.
      if list_position < 0 then
        list_position := 0;                                    // ???

      n := list_position;

      //end;//with


      with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.align_info do
      begin   // 216a ...

        if reminder_flag = True then begin
          rem_memo.Color := reminder_colour;
          rem_memo.Lines.Text := reminder_str;

          rem_label.Visible := True;
          rem_memo.Visible := True;
        end
        else begin
          rem_label.Visible := False;
          rem_memo.Visible := False;
        end;

      end;//with


      template_number_label.Caption := IntToStr(n + 1);
      total_label.Caption := 'of   ' + IntToStr(keeps_list.Count);

      gauge_panel.Caption := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.top_label;

      id_label.Caption := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.id_number_str;  // 208a

      if remove_space_str(box_file_label.Caption) = 'boxempty' then begin
        box_file_label.Caption := '';
        box_file_label.Hint := '';
      end;


      with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin
        ref_str := reference_string + '  ' + id_number_str;

        // 208a mods...

        empty_str := 'no-name';  // default init (should not appear)

        if Copy(id_number_str, 1, 1) = 'P' then
          empty_str := 'plain track';
        if Copy(id_number_str, 1, 1) = 'T' then
          empty_str := 'turnout';
        if Copy(id_number_str, 1, 1) = 'D' then
          empty_str := 'half-diamond';
        if Copy(id_number_str, 1, 1) = 'S' then
          empty_str := 'switch';
        if Copy(id_number_str, 1, 1) = 'C' then
          empty_str := 'custom template';

        if Copy(id_number_str, 2, 1) = 'L' then
          empty_str := empty_str + '  LH';
        if Copy(id_number_str, 2, 1) = 'R' then
          empty_str := empty_str + '  RH';

        if Trim(reference_string) = '' then
          panel_str := empty_str
        else
          panel_str := Trim(reference_string);

        ref_panel.Caption := ' ' + id_number_str + '   ' + Copy(panel_str, 1, 36);  // 208a

        case bgnd_code_077 of // if bgnd_flag=True

          -1: begin    // library template...

            template_number_label.Font.Color := clGreen;
            template_number_shape.Brush.Color := clGreen;
            template_number_shape.Pen.Color := clGreen;

            to_from_bgnd_panel.Hide;
            select_toggle_button.Hide;
            select_menu_entry.Enabled := False;
            library_label.Show;

            flag_shape.Brush.Color := clGreen;
            bg_str := '(library)';

            // 208a make_label.Caption:='copy to the control template';        // can't wipe if it's a library template.
            copy_to_control_radiobutton.Checked := True;
            // 208a can't wipe or delete if it's a library template.

            copy_to_control_radiobutton.Enabled := True;
            wipe_to_control_radiobutton.Enabled := False;
            delete_to_control_radiobutton.Enabled := False;


            rebuild_button.Enabled := False;
            rebuild_template_menu_entry.Enabled := False;
            make_library_template_menu_entry.Enabled := False;    // already is.
          end;


          0: begin     // unused template...

            template_number_label.Font.Color := clBlue;
            template_number_shape.Brush.Color := clBlue;
            template_number_shape.Pen.Color := clBlue;

            to_from_bgnd_panel.Color := clNavy;
            flag_shape.Brush.Color := clBlue;
            bg_str := '(unused)';

            with copy_wipe_label do begin
              Font.Color := clWhite;
              Caption := '  copy  to &background';
            end;//with

            to_from_bgnd_panel.Show;
            select_toggle_button.Show;
            select_menu_entry.Enabled := True;
            library_label.Hide;

            // 208a make_label.Caption:='copy to the control template';        // can't wipe if it's unused.

            if wipe_to_control_radiobutton.Checked = True then
              copy_to_control_radiobutton.Checked := True;  // can't wipe if it's unused.

            copy_to_control_radiobutton.Enabled := True;
            wipe_to_control_radiobutton.Enabled := False;
            delete_to_control_radiobutton.Enabled := True;

            rebuild_button.Enabled := False;
            rebuild_template_menu_entry.Enabled := False;
            make_library_template_menu_entry.Enabled := True;
          end;

          1: begin    // bgnd template...

            template_number_label.Font.Color := bgnd_ident_color;
            template_number_shape.Brush.Color := bgnd_ident_color;
            template_number_shape.Pen.Color := bgnd_ident_color;

            to_from_bgnd_panel.Color := bgnd_ident_color;
            flag_shape.Brush.Color := bgnd_ident_color;
            bg_str := '';

            with copy_wipe_label do begin
              Font.Color := clBlack;
              Caption := ' wipe  from &background';
            end;//with

            to_from_bgnd_panel.Show;
            select_toggle_button.Show;
            select_menu_entry.Enabled := True;
            library_label.Hide;

            copy_to_control_radiobutton.Enabled := True;
            wipe_to_control_radiobutton.Enabled := True;
            delete_to_control_radiobutton.Enabled := True;


            rebuild_button.Enabled := True;
            rebuild_template_menu_entry.Enabled := True;
            make_library_template_menu_entry.Enabled := False;
          end;
        end;//case
      end;//with


      save_all_menu_entry.Enabled := True;

      save_all_menu_entry.Caption := '&save  all ...';    // mod 208c

      save_bgnd_menu_entry.Enabled := True;
      save_unused_menu_entry.Enabled := True;
      save_library_menu_entry.Enabled := True;
      save_group_menu_entry.Enabled := True;

      save_all_button.Show;

      delete_menu_entry.Enabled := True;
      delete_button.Show;

      delete_unused_menu_entry.Enabled := True;
      delete_t55_templates_menu_entry.Enabled := True;

      wipe_all_menu_entry.Enabled := True;
      rebuild_all_menu_entry.Enabled := True;
      sort_library_templates_last_menu_entry.Enabled := True;
      delete_library_templates_menu_entry.Enabled := True;

      rename_menu_entry.Enabled := True;
      rename_button.Show;

      read_info_button.Show;
      rebuild_button.Show;

      save_group_button.Show;

      control_room_form.save_all_menu_entry.Enabled := True;

      //copy_or_wipe_background_button.Enabled:=True;

      clear_menu_entry.Enabled := True;
      new_clear_all_menu_entry.Enabled := True;

      print_list_button.Show;
      //deselect_all_button.Enabled:=True;
      print_list_menu_entry.Enabled := True;
      print_info_menu_entry.Enabled := True;

      all_to_bgnd_menu_entry.Enabled := True;
      toggle_all_bgnd_menu_entry.Enabled := True;

      edit_memo_menu_entry.Enabled := True;
      reminder_message_menu_entry.Enabled := True;    // 216a
      add_jotter_to_memo_menu_entry.Enabled := True;
      group_menu.Enabled := True;

      obtain_to_control_menu_entry.Enabled := True;
      export_dxf_menu_entry.Enabled := True;


      copy_panel.Visible := True;

      // 208a...

      copy_to_control_radiobutton.Visible := True;
      wipe_to_control_radiobutton.Visible := True;
      delete_to_control_radiobutton.Visible := True;
      radio_box_shape.Visible := True;
      id_label.Visible := True;

      this_is_panel.Visible := True;

      slider_ref_label.Visible := True;
      slider_number_label.Visible := True;

      read_info_button.Visible := True;
      rebuild_button.Visible := True;
      group_select_groupbox.Visible := True;

      if Ttemplate(keeps_list.Objects[n]).group_selected = True then begin
        flag_shape.Show;
        template_number_shape.Show;
        select_menu_entry.Checked := True;
        group_str := '€';
        // "box" character for group selected in the info.
      end
      else begin
        flag_shape.Hide;
        template_number_shape.Hide;
        select_menu_entry.Checked := False;
        group_str := '';
      end;

      if (keep_form.Active = True) and (list_panel.Visible = False) then
        keep_draw(n);     // draw the keep if the box is open and drawing visible.

      if keep_form.Active = True then
        highlight_bgkeep(n);   // and highlight the peg on the current keep on the background.

      // new strings for the read info...


      memo_text_str := memo_list.Strings[list_position];

      info_str :={insert_cr_str( out 0.91.b}'   ' + group_str + '  ' + IntToStr(
        list_position + 1) + '  ' + bg_str + '   ' + ref_str + '||  ' +
        gauge_panel.Caption + '||--------------------------------------------------------------'
        + '||      Information  about  this  template :' +
        '||( all dimensions in millimetres )' + '||' + keeps_list.Strings[list_position] +
        '||--------------------------------------------------------------'
        //+'||      Your  memo  notes  for  this  template :'
        + '||Memo:  ' + memo_text_str{) out 0.91.b};


      if (info_clicked = True) and (data_child_form.Visible = True) and (keep_form.Active = True)
      //mods 0.93.a then help(-2,info_str,''); // ignore return  // 0.91 //was help_form.info_label.Caption:=info_str;
      then
        data_child_form.data_memo.Text := insert_crlf_str(info_str);
      //  replace embedded | chars with a CR.

      if slider_index(slider_panel.Left) <> n then
        slider_panel.Left := slider_position(n);
      // move slider to match current index, but only if it needs moving.

    end
    else begin
      list_position := -1;

      keep_canvas_clear;
      keepform_listbox.Items.Clear;

      //keepform_listbox.Items.Add('  box  empty');  // ! owner-draw ignores if box empty.

      rem_label.Visible := False;   // 216a ..
      rem_memo.Visible := False;

      template_number_label.Caption := '';
      total_label.Caption := '';
      //text_label.Caption:='    box  empty';
      gauge_panel.Caption := 'box  empty';
      box_file_label.Caption := '  box  empty';
      box_file_label.Hint := '  box  empty';

      // 0.82.a control_room_form.statusbar_label.Caption:='  storage  box  is  empty';
      //ref_label.Caption:='';
      ref_panel.Caption := '';

      keep_copied_from_index := -1;

      //add_file_button.Hide;
      //add_file_menu_entry.Enabled:=False;

      delete_menu_entry.Enabled := False;
      delete_button.Hide;

      delete_unused_menu_entry.Enabled := False;
      delete_t55_templates_menu_entry.Enabled := False;

      wipe_all_menu_entry.Enabled := False;
      rebuild_all_menu_entry.Enabled := False;
      sort_library_templates_last_menu_entry.Enabled := False;
      delete_library_templates_menu_entry.Enabled := False;


      rename_menu_entry.Enabled := False;
      rebuild_template_menu_entry.Enabled := False;
      make_library_template_menu_entry.Enabled := False;
      rename_button.Hide;

      read_info_button.Hide;
      rebuild_button.Hide;
      save_group_button.Hide;

      //save_all_menu_entry.Enabled:=False;                              // mod 208c
      save_all_menu_entry.Caption := '&save  the  control  template ...';  // mod 208c

      save_bgnd_menu_entry.Enabled := False;
      save_unused_menu_entry.Enabled := False;
      save_library_menu_entry.Enabled := False;
      save_group_menu_entry.Enabled := False;

      save_all_button.Hide;


      control_room_form.save_all_menu_entry.Enabled := False;


      clear_menu_entry.Enabled := False;
      new_clear_all_menu_entry.Enabled := False;

      print_list_button.Hide;
      print_list_menu_entry.Enabled := False;
      print_info_menu_entry.Enabled := False;

      all_to_bgnd_menu_entry.Enabled := False;
      toggle_all_bgnd_menu_entry.Enabled := False;

      edit_memo_menu_entry.Enabled := False;
      reminder_message_menu_entry.Enabled := False;    // 216a
      add_jotter_to_memo_menu_entry.Enabled := False;
      group_menu.Enabled := False;

      obtain_to_control_menu_entry.Enabled := False;
      export_dxf_menu_entry.Enabled := False;


      save_done := True;               // nothing to save.
      saved_box_str := 'box empty';
      reloaded_box_str := '';

      updown_panel.Visible := False;
      move_up_button.Visible := False;
      move_down_button.Visible := False;

      move_to_top_button.Visible := False;     // 0.93.a
      move_to_bottom_button.Visible := False;  // 0.93.a

      copy_panel.Visible := False;

      // 208a...

      copy_to_control_radiobutton.Visible := False;
      wipe_to_control_radiobutton.Visible := False;
      delete_to_control_radiobutton.Visible := False;
      radio_box_shape.Visible := False;
      id_label.Visible := False;


      to_from_bgnd_panel.Visible := False;
      library_label.Visible := False;

      this_is_panel.Visible := False;

      slider_ref_label.Visible := False;
      slider_number_label.Visible := False;

      read_info_button.Visible := False;
      rebuild_button.Visible := False;

      group_select_groupbox.Visible := False;

      flag_shape.Hide;


      if (info_clicked = True) and (data_child_form.Visible = True) and (keep_form.Active = True)
      // 0.93.a mods then help(-2,' `0 `5 box  empty','');  // ignore return // 0.91 // was help_form.info_label.Caption:='    box  empty';
      then
        data_child_form.data_memo.Text := insert_crlf_str(' ||      box  empty');
      //  replace embedded | chars with a CR.
    end;
  end;//with
end;
//_________________________________________________________________________________________

procedure keep_form_activate;   // extracted 208a for smallest rad linking

begin
  if paper_bunching = True then
    pad_form.paper_bunching_off_menu_entry.Click;
  // don't want bunching in the keeps box drawings.
  Screen.Cursor := crHourGlass;
  // could take a while if big file.
  try
    if Application.Terminated = False then
      Application.ProcessMessages;  // so let the form repaint.

    // ensure we have a valid control template (for "store current")..

    cancel_adjusts(False);
    // might have "opened on store" from popup menu "store this".
    pad_form.length_locked_popup_entry.Click;
    //  so that plain track or approach and exit tracks can be drawn.

    normalize_transforms;
    redraw_pad(False, False);  // and do a direct redraw to ensure valid calcs, no rollback needed.

    valid_calcs := calcs_done_and_valid;   //  in case no proper template on pad.
    rollback_wanted := False;              //  no change to control template yet.

    current_state(-1);      // show current box.

    if (list_position > -1) and (list_position < keeps_list.Count) and (keeps_list.Count > 0) then
      highlight_bgkeep(list_position);
    // and highlight the peg on the current keep on the background.

    //    if (data_child_form.Visible=True) {and (data_child_form.Parent=pad_form)}  // 290b  Graeme
    //       then keep_form.read_info_button.Click;


  finally
    Screen.Cursor := crDefault;
  end;//try
end;
//______________________________________________________________________________

procedure Tkeep_form.FormActivate(Sender: TObject);

begin
  keep_form_activate;
end;
//______________________________________________________________________________

procedure Tkeep_form.FormDeactivate(Sender: TObject);

begin
  Screen.Cursor := crDefault;    // in case changed for the slider.

  gocalc(0, 0);                 // direct recalc for shifts.

  if onto_notch = True then
    shift_onto_notch(False, False);
  if copy_datum = True then
    pad_form.transform_clear_menu_entry.Click;
  onto_notch := False;                                                  // only once...
  copy_datum := False;

  redraw_pad(True, rollback_wanted);   // redraw pad when ready.
end;
//___________________________________________________________________________________________

procedure keep_canvas_clear;

begin

  with keep_form.keep_image do begin
    with Canvas do begin
      Brush.Color := keep_paper_colour;
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, Width, Height));           //  this clears the canvas.
    end;//with
  end;//with
end;
//_________________________________________________________________________________________

procedure keep_draw(index: integer);              // draw this template on the keep form.

//!!! mods 16-5-99 to remove grid...
var

  pad_ti: Ttemplate_info;  // save existing control template on the pad.

  xrange, yrange: double;
  xmax, ymax: integer;
  peg_dim, keep_pegx, keep_pegy: integer;
  radcen_dim, radcenx, radceny: integer;

  sx, sy, ypd, x_offset, y_offset: double;

  bary, scony, incb: integer;
  scx, sbex, black, incbar: double;
  sb_str: string;
  grid_str: string;

  aq: ERailData;
  now: integer;

  label_now: integer;
  label_str: string;
  tw: integer;

  peg_linex, peg_liney: double;

  i: integer;
  code: EmarkCode;
  p1, p2: TPoint;

  move_to: Tpoint;
  line_to: TPoint;

  ptr: ^Tmark;          // pointer to a Tmark record.   ###
  markmax: integer;

  sb_colour1, sb_colour2: integer;

  marcol: integer;      // 213b  marker colour ...
  marcol_used: boolean;
  box_timber_col: integer;


  ////////////////////////////////////////////////////////////////////////

  procedure draw_rail(aq: ERailData);

  var
    now: integer;
    pt: TPoint;

  begin
    if aqyn[aq] = False then
      EXIT;  // added 0.93.a ...

    with keep_form.keep_image.Canvas do begin

      Pen.Style := psSolid;

      if (aq = eRD_AdjTrackTurnoutSideNearGaugeFace) and (adjacent_edges = True) and
        (draw_ts_platform_rear_edge = False) then
        Pen.Style := psDot;    // 0.93.a show dotted on screen if hidden on output.
      if (aq = eRD_AdjTrackMainSideNearGaugeFace) and (adjacent_edges = True) and
        (draw_ms_platform_rear_edge = False) then
        Pen.Style := psDot;

      pt := outoflist(aq, 0);
      move_to.X := Round((pt.X - xy_min[0]) * sx + x_offset);
      move_to.Y := Round((pt.Y - xy_min[1]) * sy + y_offset);

      for now := 1 to nlmax_array[aq] do begin

        pt := outoflist(aq, now);
        line_to.X := Round((pt.X - xy_min[0]) * sx + x_offset);
        line_to.Y := Round((pt.Y - xy_min[1]) * sy + y_offset);

        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;
        move_to := line_to;
      end;//for

    end;//with
  end;
  ////////////////////////////////////////////////////////////////////////

  procedure mark_end(aq1: ERailData; aq1end: integer; aq2: ERailData;
    aq2end: integer; pen_solid: boolean);
  // make the mark

  begin

    if (endmarks_yn[aq1, aq1end] = True) and (endmarks_yn[aq2, aq2end] = True) then begin
      p1 := endmarks[aq1, aq1end];
      p2 := endmarks[aq2, aq2end];

      p1.X := p1.X - xy_min[0];     //!!! 16-5-99...
      p1.Y := p1.Y - xy_min[1];
      p2.X := p2.X - xy_min[0];
      p2.Y := p2.Y - xy_min[1];

      with keep_form.keep_image.Canvas do begin

        if pen_solid = True then
          Pen.Style := psSolid   // 0.93.a mods for platforms
        else
          Pen.Style := psDot;

        move_to.X := Round(p1.X * sx + x_offset);
        move_to.Y := Round(p1.Y * sy + y_offset);
        line_to.X := Round(p2.X * sx + x_offset);
        line_to.Y := Round(p2.Y * sy + y_offset);
        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;
      end;//with
    end;
  end;
  ////////////////////////////////////////////////////////////

begin
  if (keep_form.Active = False) or (keeps_list.Count < 1) or (index < 0) or
    (index > (keeps_list.Count - 1)) then
    EXIT;   // no drawing if the box is closed or the box is empty or the index is out of range.
  // otherwise onActivate code will not have been done.

  pad_ti.keep_shove_list := Tshoved_timber_list.Create;
  try
    fill_kd(pad_ti);        // fill the keep record with the saved control template.

    copy_keep(Ttemplate(keeps_list.Objects[index]).template_info);    // get the current keep.

    with Ttemplate(keeps_list.Objects[index]).template_info.keep_dims.box_dims1 do
    begin      // 213b

      marcol := pad_marker_colour;
      marcol_used := use_pad_marker_colour;  // using it

    end;//with


    //show_and_redraw(True,False);    // (on idle, no rollback).

    gocalc(1, 0);     // and force immediate calcs for us.

    // dims in 1/100mm (floats)...

    ypd := y_datum * 100;                    // (y_datum is in mm from left edge of sheet).

    try
      xrange := ABS(xy_max[0] - xy_min[0]);
      // use range of relative x values in list for scaling.
      yrange := ABS(xy_max[1] - xy_min[1]);   // ditto y.
    except
      xrange := 50000;             // could be a template with all rails turned off.
      yrange := 25000;             // 500 x 250 mm arbitrary default.
    end;//try

    if xrange < 100 then
      xrange := 100;     // minimum length or width = 1mm for scaling.  205d
    if yrange < 100 then
      yrange := 100;


    keep_canvas_clear;   // first clear the canvas.

    with keep_form.keep_image.Canvas do begin

      Font.Assign(keep_form.keep_panel.Font);    // for labels.

      xmax := keep_form.keep_panel.Width - 2;    // single line border.
      ymax := keep_form.keep_panel.Height - 2;

      if xmax < 5 then
        xmax := 5;                // just in case we have a funny.
      if ymax < 5 then
        ymax := 5;

      if keep_form.fit_radio_button.Checked = True   // scale to fit..
      then begin
        sx := (xmax / xrange) * 4 / 5;          //  x scaling factor, 80% allowing for margins.

        if yrange < (2000 * scale) then
          sy := (ymax / (2000 * scale)) * 4 / 5   //  max y scaling factor (20ft scale arbitrary).
        else
          sy := (ymax / yrange) * 4 / 5;        //  normal y scaling factor.

        if sy < sx then
          sx := sy                //  use smallest of calculated scalings,
        else
        if sx < sy then
          sy := sx; //  to ensure all on screen.

        if sx < minfp then
          sx := 0.025;
        //  arbitrary div zero safety - in pixels per 1/100th mm. (2.5 pixels per mm).

        box_width := xmax / sx;
        // (1/100th mm) in case he locks the scaling.
      end
      // use fixed box width:
      else begin
        sx := xmax / box_width;
        sy := sx;
      end;

      sy := 0 - ABS(sy);                  //  but y must be negative.

      x_offset := xmax / 2 - ABS(xrange) * sx / 2;    // centralise on x.
      y_offset := ymax / 2 - ABS(yrange) * sy / 2;    // centralise on y (sy negative).

      Pen.Width := 1;            // default pen settings...
      Pen.Mode := pmCopy;
      Pen.Style := psSolid;
      Pen.Color := clBlack;

      //  first draw scalebar...

      scx := 1;                // keep compiler happy.
      incbar := 1;             // ditto.
      case scale_bar_i of
        //  200 mm scalebar (also when hidden on pad).
        0, 1: begin
          scx := sx * 100;
          //  adjust scaling to mm. (bar is 20 * 10 mm = 200 mm long).
          sb_str := '200 mm';
          incbar := 5;          //  long marks at 5 * 10 = 50 mm intervals.
        end;

        2: begin
          scx := sx * 63.5;        //  or inches (bar is 20 * 0.25 = 5 inches long).
          sb_str := '5"';
          incbar := 4;           //  long marks at 4 * 0.25 = 1 inch intervals.
        end;
      end;//case

      sbex := (xmax / 2) - 100 * scx;             //  start of bar to put it in middle of width.
      //if sbex<ex then sbex:=ex;

      sb_colour1 := keep_grid_colour;
      sb_colour2 := keep_paper_colour;

      scony := Round(ymax / 25); // scalebar near top of box

      Pen.Width := 1;
      Pen.Style := psSolid;
      Pen.Mode := pmCopy;
      Pen.Color := sb_colour1;

      move_to.X := Round(sbex);
      move_to.Y := scony;
      line_to.X := Round(sbex + 200 * scx);
      line_to.Y := scony;  // upper rule scale 200 mm or 10" long.
      if check_limits(move_to, line_to) = True then begin
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);
      end;

      for bary := (scony + 1) to (scony + 2) do begin
        black := 0;
        //  10 pairs of short intervals
        for incb := 1 to 10 do begin
          move_to.X := Round(sbex + black * scx);
          move_to.Y := bary;      // black/white scale 10 mm or 0.5" intervals.
          Pen.Color := sb_colour1;
          line_to.X := Round(sbex + (black + 10) * scx + 1);
          line_to.Y := bary;   // +1 is a bodge to make black/white look equal intervals.
          if check_limits(move_to, line_to) = True then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          move_to := line_to;
          Pen.Color := sb_colour2;
          line_to.X := Round(sbex + (black + 20) * scx);
          line_to.Y := bary;
          if check_limits(move_to, line_to) = True then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          black := black + 20;
        end;
      end;                           //  2 lines deep.

      for bary := (scony + 4) to (scony + 5) do begin
        black := 0;
        //  2 pairs of long intervals
        for incb := 1 to 2 do begin
          move_to.X := Round(sbex + black * scx);
          move_to.Y := bary;
          // black/white scale 10 mm or 0.5" intervals.
          Pen.Color := sb_colour1;
          line_to.X := Round(sbex + (black + 10 * incbar) * scx + 1);
          line_to.Y := bary;   // +1 is a bodge to make black/white look equal intervals.
          if check_limits(move_to, line_to) = True then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          move_to := line_to;
          Pen.Color := sb_colour2;
          line_to.X := Round(sbex + (black + 20 * incbar) * scx);
          line_to.Y := bary;                     // only do half of last pair (for inches).
          if check_limits(move_to, line_to) = True then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          black := black + 20 * incbar;
        end;
        if incbar = 4                   //   need another half pair if inches.
        then begin
          move_to.X := Round(sbex + black * scx);
          move_to.Y := bary;
          Pen.Color := sb_colour1;
          line_to.X := Round(sbex + (black + 10 * incbar) * scx);
          line_to.Y := bary;
          if check_limits(move_to, line_to) = True then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;
        end;

      end;                          // 2 lines deep.

      Pen.Color := sb_colour1;

      move_to.X := Round(sbex);
      move_to.Y := scony + 3;
      line_to.X := Round(sbex + 200 * scx);
      line_to.Y := scony + 3;    // middle rule
      if check_limits(move_to, line_to) = True then begin
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);
      end;

      move_to.X := Round(sbex);
      move_to.Y := scony + 6;
      line_to.X := Round(sbex + 200 * scx);
      line_to.Y := scony + 6;    // lower rule
      if check_limits(move_to, line_to) = True then begin
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);
      end;

      move_to := line_to;
      line_to.X := Round(sbex + 200 * scx);
      line_to.Y := scony - 1;    // close white end. (-1 finishes the corner).
      if check_limits(move_to, line_to) = True then begin
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);
      end;

      Font.Color := sb_colour1;
      TextOut(Round(sbex + 200 * scx + 4), scony + 2 + (Font.Height div 2), sb_str);
      // label end of bar. (font height is negative).
      //Font.Color:=keep_form.keep_panel.Font.Color;                        // reset the font colour.

      //  now draw turnout timbers and all marks (except rail ends) ...
      //###

      if marks_list_ptr = nil then
        EXIT;        // pointer to marks list not valid.
      markmax := High(marks_list_ptr);  // max index for the present list.

      if mark_index > markmax then
        mark_index := markmax;  // ??? shouldn't be.

      for i := 0 to (mark_index - 1) do    // (mark_index is always the next free slot)
      begin
        ptr := @marks_list_ptr[i];  // pointer to the next Tmark record.
        if ptr = nil then
          EXIT;

        code := ptr^.code;              // check this mark wanted.

        if (code <> eMC_0_Ignore)
          and (code <> eMC_99_TimberNumber)
          and (code < eMC_501_MSWorkingEnd)
        // 0.94.a  501-508 is check-rail labels   213b 777 is chair oulines// ignore text (timber labels)  // 206b 600,601-604  switch long marks and labels
        then begin
          p1 := ptr^.p1;    // x1,y1 in  1/100ths mm
          p2 := ptr^.p2;    // x2,y2 in  1/100ths mm

          if code = eMC__1_PegCentre    // fixing peg centres
          then begin
            peg_linex := p1.X / 100;             // mm
            peg_liney := p1.Y / 100 + y_datum;
          end;

          p1.X := p1.X - xy_min[0];     //!!! 16-5-99...
          p1.Y := p1.Y - xy_min[1];
          p2.X := p2.X - xy_min[0];
          p2.Y := p2.Y - xy_min[1];

          if marcol_used = True              // 213b using marker colour
          then
            box_timber_col := marcol
          else
            box_timber_col := keep_timber_colour;

          Pen.Style := psSolid;  // solid lines look better on screen.
          Pen.Width := 1;
          Pen.Mode := pmCopy;

          with bgkeeps_form do begin
            case code of
              eMC__5_Label,
              eMC__4_TimberSelector:
                CONTINUE;     // ignore label and timber selector.

              eMC__3_CurvingRadiusCentre_2,
              eMC__2_CurvingRadiusCentre_1:
                if marks_checkbox.Checked = True then
                  Pen.Color := keep_mark_colour  // curving rad centres.
                else
                  CONTINUE;
              eMC__1_PegCentre:
                Pen.Color := bgnd_ident_color; // fixing peg (changes again later).

              eMC_1_GuideMark,
              eMC_2_RadialEnd,            // 2
              eMC_7_TransitionAndSlewing,
              eMC_10_PlainTrackStart:
                if marks_checkbox.Checked = True then
                  Pen.Color := keep_mark_colour
                // guide marks, radial ends, transition marks, plain track start marks.
                else
                  CONTINUE;

              eMC_3_TimberOutline,
              eMC_33_ShovingTimberOutline,
              eMC_93_Infill_1:
                if timber_outlines_checkbox.Checked = True then
                  Pen.Color := box_timber_col   // timber outlines.
                else
                  CONTINUE;

              eMC_4_TimberCL,
              eMC_14_TimberCLSolid,
              eMC_44_ShovingTimberCL_1,
              eMC_54_ShovingTimberCL_2:
                if timber_centres_checkbox.Checked = True then
                  Pen.Color := box_timber_col   // timber centre-lines.
                else
                  CONTINUE;

              eMC_5_TimberReducedEnd,
              eMC_8_PegArm_1,
              eMC_9_PegArm_2,
              eMC_55_ReducedEnd,
              eMC_95_Infill_2,
              eMC_203_TimberInfill,
              eMC_233_Infill_3,
              eMC_293_Infill_4,
              eMC_493_Chair:
                CONTINUE;        // ignore timber reduced ends, peg arms, timber infill, chairs

              eMC_6_RailJoint:
                if joints_checkbox.Checked = True then
                  Pen.Color := keep_mark_colour  // rail joint marks.
                else
                  CONTINUE;

              else
                Pen.Color := clBlack;
            end;//case
          end;//with

          if code > eMC_0_Ignore                // draw timbers and marks...
          then begin
            move_to.X := Round(p1.X * sx + x_offset);
            move_to.Y := Round(p1.Y * sy + y_offset);
            line_to.X := Round(p2.X * sx + x_offset);
            line_to.Y := Round(p2.Y * sy + y_offset);
            if check_limits(move_to, line_to) = True then begin
              MoveTo(move_to.X, move_to.Y);
              LineTo(line_to.X, line_to.Y);
            end;
          end
          else begin
            if code = eMC__1_PegCentre    // code -1, draw fixing peg...
            then begin
              case Ttemplate(keeps_list.Objects[index]).template_info.keep_dims.box_dims1.bgnd_code_077 of
                -1:
                  Font.Color := clGreen;
                0:
                  Font.Color := clBlue;
                1:
                  Font.Color := bgnd_ident_color;
              end;//case

              keep_pegx := Round(p1.X * sx + x_offset);
              // peg centres...
              keep_pegy := Round(p1.Y * sy + y_offset);

              // first draw grid lines through peg...
              Pen.Color := keep_grid_colour;
              Brush.Color := keep_paper_colour;
              Brush.Style := bsSolid;

              move_to.X := keep_pegx;
              move_to.Y := 0;
              line_to.X := keep_pegx;
              line_to.Y := ymax;
              if check_limits(move_to, line_to) = True then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

              label_str := round_str(peg_linex, 2);
              tw := TextWidth(label_str);
              label_now := keep_pegx - (tw div 2);
              if label_now < 1 then
                label_now := 1;
              if (label_now + tw) > (xmax - 1) then
                label_now := xmax - tw - 1;

              TextOut(label_now, ymax + Font.Height - 1, label_str);
              // add peg bottom label.

              move_to.X := 0;
              move_to.Y := keep_pegy;
              line_to.X := xmax;
              line_to.Y := keep_pegy;
              if check_limits(move_to, line_to) = True then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

              //label_str:=FormatFloat('0.##',peg_liney);
              label_str := round_str(peg_liney, 2);
              if keep_pegx > (xmax div 2) then
                label_now := 1
              else
                label_now := xmax - TextWidth(label_str) - 1;

              TextOut(label_now, keep_pegy + (Font.Height div 2), label_str);
              // add peg right label.

              // now draw coloured peg...
              peg_dim := xmax div 32;
              // 32 arbitrary.
              if peg_dim > Round(scale * 250 * sx) then
                peg_dim := Round(scale * 250 * sx); // but not more than 4ft scale.

              case Ttemplate(keeps_list.Objects[index]).template_info.keep_dims.box_dims1.bgnd_code_077 of
                -1:
                  Pen.Color := clGreen;
                0:
                  Pen.Color := clBlue;
                1:
                  Pen.Color := bgnd_ident_color;
              end;//case

              Brush.Color := Pen.Color;
              // show solid peg in keeps box.
              Brush.Style := bsSolid;

              move_to.X := keep_pegx - peg_dim;
              move_to.Y := keep_pegy - peg_dim;
              line_to.X := keep_pegx + peg_dim;
              line_to.Y := keep_pegy + peg_dim;
              if check_limits(move_to, line_to) = True then
                Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);

              move_to.X := keep_pegx - peg_dim * 2;
              move_to.Y := keep_pegy;
              line_to.X := keep_pegx + peg_dim * 2;
              line_to.Y := keep_pegy;
              if check_limits(move_to, line_to) = True then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

              move_to.X := keep_pegx;
              move_to.Y := keep_pegy - peg_dim * 2;
              line_to.X := keep_pegx;
              line_to.Y := keep_pegy + peg_dim * 2;
              if check_limits(move_to, line_to) = True then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

            end;

            if (code = eMC__2_CurvingRadiusCentre_1)
              or (code = eMC__3_CurvingRadiusCentre_2)
            // draw curving rad centres...
            then begin
              radcen_dim := xmax div 150;
              // 150 arbitrary.
              if radcen_dim > Round(scale * 500 * sx) then
                radcen_dim := Round(scale * 500 * sx); // but not more than 5ft scale.

              radcenx := Round(p1.X * sx + x_offset);
              radceny := Round(p1.Y * sy + y_offset);

              move_to.X := radcenx - radcen_dim * 2;
              move_to.Y := radceny;
              line_to.X := radcenx + radcen_dim * 2;
              line_to.Y := radceny;
              if check_limits(move_to, line_to) = True then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

              move_to.X := radcenx;
              move_to.Y := radceny - radcen_dim * 2;
              line_to.X := radcenx;
              line_to.Y := radceny + radcen_dim * 2;
              if check_limits(move_to, line_to) = True then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;
            end;
          end;
        end;
      end;//next i
      //  draw turnout rails...

      Pen.Width := 1;
      Pen.Mode := pmCopy;               // solid thin lines with square ends.
      Pen.Style := psSolid;

      Brush.Color := keep_paper_colour;  // gaps in dotted lines.
      Brush.Style := bsSolid;

      TextOut(0, 0, '');  // needed for dotted lines - Delphi bug?

      // indicator colour

      case Ttemplate(keeps_list.Objects[index]).template_info.keep_dims.box_dims1.bgnd_code_077
        of
        -1:
          Pen.Color := clGreen;
        0:
          Pen.Color := clBlue;
        1:
          Pen.Color := bgnd_ident_color;
      end;//case

      if bgkeeps_form.centres_checkbox.Checked = True    // track centre-lines first...
      then begin
        for aq := eRD_MainRoadCentreLine to eRD_TurnoutRoadCentreLine do begin

          if (plain_track = False) or (aq = eRD_MainRoadCentreLine)
          // main-side only, if plain track
          then
            draw_rail(aq);
        end;//next aq
      end;

      // now platform edges in same colour...

      if (adjacent_edges = True) and (bgkeeps_form.platforms_checkbox.Checked = True)
      // platform edges
      then begin
        //                    if draw_ts_platform_rear_edge=True      // TS platform rear edge
        //                       then begin                           // data is always in list so can print infilled
        aq := eRD_AdjTrackTurnoutSideNearGaugeFace;
        draw_rail(aq);
        //                            end;

        aq := eRD_AdjTrackTurnoutSideNearOuterFace;             // TS platform front edge
        draw_rail(aq);

        //                    if draw_ms_platform_rear_edge=True    // MS platform rear edge
        //                       then begin                         // data is always in list so can print infilled
        aq := eRD_AdjTrackMainSideNearGaugeFace;
        draw_rail(aq);
        //                            end;

        aq := eRD_AdjTrackMainSideNearOuterFace;             // MS platform front edge
        draw_rail(aq);

        // 0.93.a platform ends ...

        if draw_ts_platform = True then begin
          mark_end(eRD_AdjTrackTurnoutSideNearGaugeFace, 0, eRD_AdjTrackTurnoutSideNearOuterFace,
            0, draw_ts_platform_start_edge);
          mark_end(eRD_AdjTrackTurnoutSideNearGaugeFace, 1, eRD_AdjTrackTurnoutSideNearOuterFace,
            1, draw_ts_platform_end_edge);
        end;

        if draw_ms_platform = True then begin
          mark_end(eRD_AdjTrackMainSideNearGaugeFace, 0, eRD_AdjTrackMainSideNearOuterFace,
            0, draw_ms_platform_start_edge);
          mark_end(eRD_AdjTrackMainSideNearGaugeFace, 1, eRD_AdjTrackMainSideNearOuterFace,
            1, draw_ms_platform_end_edge);
        end;
      end;

      // and trackbed edges in same colour...

      if (adjacent_edges = True) and (bgkeeps_form.trackbed_edges_checkbox.Checked = True)
      // trackbed edges
      then begin
        aq := eRD_AdjTrackTurnoutSideFarGaugeFace;
        draw_rail(aq);
        aq := eRD_AdjTrackTurnoutSideFarOuterFace;
        draw_rail(aq);
        aq := eRD_AdjTrackMainSideFarGaugeFace;
        draw_rail(aq);
        aq := eRD_AdjTrackMainSideFarOuterFace;
        draw_rail(aq);
      end;

      // finally the rails...

      if marcol_used = True              // using marker colour
      then
        Pen.Color := marcol
      else
        Pen.Color := keep_rail_colour;

      if bgkeeps_form.gauge_faces_checkbox.Checked = True then
        for aq := eRD_StraightStockGaugeFace to eRD_TurnoutSideCheckGaugeFace do
        begin                      // main rails gauge faces
          if (plain_track = False) or (aq = eRD_StraightStockGaugeFace) or
            (aq = eRD_CurvedStockGaugeFace) // stock rails only, if plain track
          then
            draw_rail(aq);
        end;//next aq

      if bgkeeps_form.outer_edges_checkbox.Checked = True then
        for aq := eRD_StraightStockOuterFace to eRD_TurnoutSideCheckOuterFace do
        begin                     // main rails outer edges
          if (plain_track = False) or (aq = eRD_StraightStockOuterFace) or
            (aq = eRD_CurvedStockOuterFace) then
            draw_rail(aq);
        end;//next aq

      if (adjacent_edges = False) and (bgkeeps_form.gauge_faces_checkbox.Checked = True) then
      begin
        for aq in eRD_AdjacentTracksGaugeFaces do begin
          draw_rail(aq);
        end;
      end;

      if (adjacent_edges = False) and (bgkeeps_form.outer_edges_checkbox.Checked = True) then
      begin
        for aq in eRD_AdjacentTracksOuterFaces do begin
          draw_rail(aq);
        end;
      end;

      // finally, draw the rail ends...

      if (plain_track = False) and (bgkeeps_form.gauge_faces_checkbox.Checked = True) and
        (bgkeeps_form.outer_edges_checkbox.Checked = True) then begin
        mark_end(eRD_StraightTurnoutWingGaugeFace, 1, eRD_StraightTurnoutWingOuterFace, 1, True);
        // turnout rail wing rail finish.
        mark_end(eRD_CurvedTurnoutWingGaugeFace, 1, eRD_CurvedTurnoutWingOuterFace, 1, True);
        // main rail wing rail finish.

        mark_end(eRD_MainSideCheckGaugeFace, 0, eRD_MainSideCheckOuterFace, 0, True);
        // main side check rail start.
        mark_end(eRD_MainSideCheckGaugeFace, 1, eRD_MainSideCheckOuterFace, 1, True);
        // main side check rail finish.

        mark_end(eRD_TurnoutSideCheckGaugeFace, 0, eRD_TurnoutSideCheckOuterFace, 0, True);
        // turnout side check rail start.
        mark_end(eRD_TurnoutSideCheckGaugeFace, 1, eRD_TurnoutSideCheckOuterFace, 1, True);
        // turnout side check rail finish.

        mark_end(eRD_VeePointGaugeFace, 0, eRD_VeeSpliceGaugeFace, 0, True);    // blunt nose.

        if (half_diamond = True) and (fixed_diamond = True) then begin
          mark_end(eRD_StraightTurnoutWingGaugeFace, 0, eRD_StraightTurnoutWingOuterFace, 0, True);
          // planed faced of point rails for a fixed-diamond.
          mark_end(eRD_CurvedTurnoutWingGaugeFace, 0, eRD_CurvedTurnoutWingOuterFace, 0, True);

          mark_end(eRD_KCrossingCheckMainSideGaugeFace, 1, eRD_KCrossingCheckMainSideOuterEdge,
            1, True);     // MS K-crossing check rails.
          mark_end(eRD_KCrossingCheckTurnoutSideGaugeFace, 1,
            eRD_KCrossingCheckTurnoutSideOuterEdge, 1, True);     // DS K-crossing check rails.
        end;
      end;

    end;//with Canvas
  finally
    copy_keep(pad_ti);                         // restore the control template on the pad.
    pad_ti.keep_shove_list.Free;
  end;//try
end;
//_______________________________________________________________________________________________________________________________
procedure Tkeep_form.colour_patchClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  storage  box', Color);
  SetFocus;
end;
//______________________________________________________________________________

procedure Tkeep_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  box_scaling := True;      // otherwise ScrollInView on resize prevents form rescaling properly.

  if size_updown.Position > size_updown.Tag
  // ! position goes up, size goes down.
  then begin
    ScaleBy(9, 10);
    // scale the form contents down.
    with keepform_listbox do
      ItemHeight := Round(ItemHeight * 9 / 10);  // owner-draw listbox.
  end;

  if size_updown.Position < size_updown.Tag then begin
    ScaleBy(10, 9);
    // scale the form contents up.
    with keepform_listbox do
      ItemHeight := Round(ItemHeight * 10 / 9);
  end;

  ClientHeight := VertScrollBar.Range;                               // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 4;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;
  // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;
  // and save for the next click.

  box_scaling := False;

  current_state(-1);
end;
//___________________________________________________________________________________________

procedure Tkeep_form.store_current_as_unused_buttonClick(Sender: TObject);
// create and add the control template to the keeps box.

begin
  store_unused(False, False);
end;
//_________________________________________________________________________________________

procedure store_unused(library_template, control_template_on_save: boolean);
// 0.93.a control template added

var
  n, i: integer;
  si: string;
  new_ti: Ttemplate_info;

begin

  if (keep_form.Active = True) and (control_template_on_save = False)
  // the box was opened unchanged.
  then begin
    if valid_calcs = False                //  calcs were not valid.
    then begin
      alert(6, '    nothing  to  store',
        '|||    There is no valid template to be stored.',
        '', '', '', '', 'cancel', '', 0);
      EXIT;
    end;

    if check_control_template_is_valid('store') = False then
      EXIT;  // 0.93.a  zero-length template

  end
  else begin   // called from pad...

    // or adding the control on save or backup  0.93.a

    normalize_transforms;          // first normalize transform data.


    if control_template_on_save = True             // 0.93.a
    then begin
      gocalc(0, 0);                       // 0.93.a only calcs wanted
      if {check_if_abandoned<>-1} abandon_calcs = True then
        EXIT;
    end
    else begin

      //show_and_redraw(False,False);  // make sure not hidden.

      gocalc(1, 0);  // 0.93.a instead of above
      // do a direct redraw to ensure valid calcs.
      // no rollback wanted.


      if calcs_done_and_valid = False        //  calcs not valid.
      then begin
        alert(6, '    nothing  to  store',
          '|||    There is no valid template to be stored.',
          '', '', '', '', 'cancel', '', 0);
        EXIT;
      end;
    end;
  end;

  new_ti.keep_shove_list := Tshoved_timber_list.Create;
  try
    fill_kd(new_ti);               // fill the keep record with the control template data.

    keeps_list.Sorted := False;      // ensure new line is appended.
    n := keeps_list.Count;           // for error message if AddObject fails.

    //with info_form do begin
    si := '';
    if info_text_list{info_memo.Lines}.Count < 1 then
      si := '    blank|'     // ???
    else begin
      for i := 0 to (info_text_list{info_memo.Lines}.Count - 1) do
        si := si + info_text_list{info_memo.Lines}.Strings[i] + '|';
      // info text goes in keeps_list.
      si := si + 'total timbering length on this template = ' + round_str(
        total_template_timber_length, 2);  //0.97.a
    end;

    try
      n := keeps_list.AddObject(si, Ttemplate.Create);
      // create and append a new line in keeps list.
      if memo_list.Add(current_memo_str) <> n then
        run_error(199);    // and memo list. Ensure indices correspond.

    except
      if control_template_on_save = False           // 0.93.a
      then
        alert(1, '      memory  problem',
          '|Unable to add this template to your storage box because of memory constraints.'
          +
          '||(There are  ' + IntToStr(n) + '  templates in your box already.)'
          // n+1-1 because not added.
          + '||Save the contents of your storage box to a file, then clear the contents to make space for more.'
          + '||Alternatively, delete one or more unwanted existing templates from the box.'
          + '||Then try again.',
          '', '', '', '', '', 'continue', 0);
      EXIT;
    end;//try
    init_ttemplate(n);           // init flags for new keep.

    if library_template = True then
      new_ti.keep_dims.box_dims1.bgnd_code_077 := -1   // make it a library template.
    else
      new_ti.keep_dims.box_dims1.bgnd_code_077 := 0;   // unused, not yet on background.
    new_ti.keep_dims.box_dims1.pre077_bgnd_flag := False;
    // in case reloaded in older version than 0.77.a

    new_ti.keep_dims.box_dims1.this_was_control_template := control_template_on_save;  // 0.93.a

    if control_template_on_save = True   // 208c
    then
      with new_ti.keep_dims.turnout_info2 do
        template_type_str := template_type_str + 'c';  // max 6 chars

    copy_template_info_from_to(False, new_ti, Ttemplate(keeps_list.Objects[n]).template_info);

    if control_template_on_save = False           // 0.93.a ...
    then begin

      save_done := False;     // need a fresh save.
      backup_wanted := True;  // update the backup.

      if keep_form.list_panel.Showing = True then
        keep_form.show_list_button.Click;          // update the list.

      current_state(0);  // add to listbox and set new current.

      if (keep_form.Active = True) and (keep_form.list_panel.Visible = False) then
        keep_draw(n);    // draw the control template in the keeps box.
    end;

  finally
    new_ti.keep_shove_list.Free;
  end;//try
end;
//____________________________________________________________________________________________

procedure exchange_keeps(n1, n2: integer);     // swap the position of 2 keeps in the box.

begin
  if keeps_list.Count <> memo_list.Count then
    EXIT;
  if keeps_list.Count < 2 then
    EXIT;
  if (n1 > keeps_list.Count - 1) or (n2 > keeps_list.Count - 1) or (n1 < 0) or (n2 < 0) then
    EXIT;
  if n1 = n2 then
    EXIT;

  keeps_list.Exchange(n1, n2);
  memo_list.Exchange(n1, n2);
  list_position := n1;        // make the first index current.
  current_state(1);
end;
//___________________________________________________________________________________________

procedure do_restore_swap;   // swap the top keep with the position of the last one copied out.

begin
  if (keeps_list.Count < 2) or (keep_copied_from_index = -1) then
    EXIT;      // don't know which to swap.
  exchange_keeps(keep_copied_from_index, keeps_list.Count - 1);
end;
//__________________________________________________________________________________________

procedure wipe_all_background; //bgkeeps_form.clear_button.Click;

var
  n: integer;

begin

  if any_bgnd = 0 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do
    wipe_it(n);  // ignore result.

  backup_wanted := True;
  redraw_pad(True, False);
end;
//___________________________________________________________________________________

procedure clear_keeps(delete_backup, save_for_undo: boolean);

// 0.96.a  delete_backup not used

var
  n, i: integer;
  sfu_str: string;

begin
  if (keeps_list.Count > 0) and (save_for_undo = True) then begin
    sfu_str := Config.GetFilePath(csfiSaveForUndo);
    DeleteFile(sfu_str);        // delete any previous undo file.
    save_box(0, 0, 0, sfu_str);    // save existing contents for possible undo later.
  end;

  wipe_all_background;  //bgkeeps_form.clear_button.Click;     // first clear all the background data.

  if keeps_list.Count > 0 then begin
    for n := 0 to keeps_list.Count - 1 do begin

      Ttemplate(keeps_list.Objects[n]).template_info.keep_shove_list.Free;
      Ttemplate(keeps_list.Objects[n]).Free;

    end;//next n
  end;

  keeps_list.Clear;

  memo_list.Clear;
  keeps_list.Sorted := False;    // append any new lines.
  memo_list.Sorted := False;
  keep_canvas_clear;


  backup_wanted := False;        // 0.93.a

  current_state(0);            // show empty box.
end;
//___________________________________________________________________________________________

procedure clear_keep(n: integer);     // clear a single keep without asking.
// this is for errors on loading,
// and ignore unused option.
var
  i: integer;

begin

  if keeps_list.Count < 1 then
    EXIT;
  if keeps_list.Count <> memo_list.Count then
    run_error(220);

  if (n >= 0) and (n < keeps_list.Count) then begin
    Ttemplate(keeps_list.Objects[n]).template_info.keep_shove_list.Free;
    Ttemplate(keeps_list.Objects[n]).Free;
    keeps_list.Delete(n);
    memo_list.Delete(n);
    save_done := False;
    backup_wanted := True;
  end;
end;
//__________________________________________________________________________________________

procedure Tkeep_form.info_radio_buttonClick(Sender: TObject);

begin
  current_state(-1);
end;
//__________________________________________________________________________________________

procedure Tkeep_form.memo_radio_buttonClick(Sender: TObject);

begin
  current_state(-1);
end;
//_________________________________________________________________________________________

procedure Tkeep_form.FormResize(Sender: TObject);

begin
  box_file_label.Width := ClientWidth;

  if (Showing = True) and (initdone_flag = True) and (box_scaling = False) then
    ScrollInView(escape_panel);
end;
//_____________________________________________________________________________

procedure Tkeep_form.fit_radio_buttonClick(Sender: TObject);

begin
  current_state(-1);
end;
//___________________________________________________________________________________

procedure Tkeep_form.lock_radio_buttonClick(Sender: TObject);

begin
  current_state(-1);
end;
//_____________________________________________________________________________________

procedure Tkeep_form.chat_panelClick(Sender: TObject);

begin
  chat(chat_str);
end;
//________________________________________________________________________________________

procedure Tkeep_form.show_list_buttonClick(Sender: TObject);

begin
  list_panel.Show;
  if Application.Terminated = False then
    Application.ProcessMessages;         // wait until it is showing.
  current_state(-1);
  if (keep_form.Active = True) and (list_panel.Showing = True) and (keeps_list.Count > 0) then
    keepform_listbox.SetFocus;
end;
//__________________________________________________________________________________________

procedure Tkeep_form.show_dwg_buttonClick(Sender: TObject);

begin
  list_panel.Hide;
  current_state(-1);
end;
//________________________________________________________________________________________

procedure Tkeep_form.keepform_listboxClick(Sender: TObject);

begin
  current_state(2);
end;
//________________________________________________________________________________________

procedure Tkeep_form.print_list_buttonClick(Sender: TObject);

var
  type_str: string;
  line_now: integer;

  html_str: string;
  i: integer;
  pdf_box_list: boolean;

  { metafile_printer: TMetafilePrinter; }

  page: integer;
  folder_str: string;
  note_str: string;

  all_done_full: boolean;
  full_string, next_string: string;

  show_str: string;

  bold_str, italic_str, underline_str: string;  // 208b

  export_list: TStringList;

begin
  if (keep_form.keepform_listbox.Items.Count <> keeps_list.Count) // then EXIT;  //??
    or (memo_list.Count <> keeps_list.Count)                        // then EXIT;  //??
    or (keep_form.keepform_listbox.Items.Count < 1)                 // then EXIT;
  then begin
    ShowMessage('The storage box is empty. There is nothing to list.');
    EXIT;
  end;

  if no_printer_available = True     // 0.93.a
  then begin
    ShowMessage('There is no printer available. (A printer is needed for PDF files to set the paper size.)');
    EXIT;
  end;


  pdf_box_list := False; // keep compiler happy

  // 205a mods ...

  repeat

    i := alert(4, '    box  list  to  printer  or  to  PDF  file ?',
      'Do you want to print the box list,||or create a PDF file?' +
      '|||green_panel_begin tree.gif To set the text size and margins click the `0program > printer font + margins...`1 menu item on the program panel, or click the bar below.green_panel_end', '', '', 'set  font  and  margins', 'PDF  file', 'cancel', 'print', 0);
    case i of
      3:
        set_printer_font_margins(keep_form, True);
      4:
        pdf_box_list := True;
      5:
        EXIT;
      6:
        pdf_box_list := False;
    end;//case

  until i <> 3;

  // changed to HTML  205a ...

  if fsBold in printer_text_font.Style then
    bold_str := 'bold'
  else
    bold_str := 'normal';

  if fsItalic in printer_text_font.Style then
    italic_str := 'italic'
  else
    italic_str := 'normal';

  if fsUnderline in printer_text_font.Style then
    underline_str := 'underline'
  else
    underline_str := 'none';


  html_str := '<HTML><HEAD>' + '<style> body, p, table { font-size:' +
    IntToStr(printer_text_font.Size) + 'pt; font-family:"' + printer_text_font.Name +
    '"; ' + 'font-weight:' + bold_str + '; font-style:' + italic_str +
    '; text-decoration:' + underline_str + '; }' +
    ' td {padding-left:8px; padding-right:8px;} </style>' +
    '</HEAD><BODY><TABLE ALIGN="CENTER" WIDTH="100%">' +
    '<TR><TD COLSPAN="6" ALIGN="CENTER" STYLE="font-size:' +
    IntToStr(printer_text_font.Size + 2) + 'pt;">' + 'The Templot0 storage box for :</TD></TR>' +
    '<TR><TD COLSPAN="6" ALIGN="CENTER" STYLE="font-size:' +
    IntToStr(printer_text_font.Size + 4) + 'pt;">' + box_project_title_str +
    '</TD></TR>' + '<TR><TD COLSPAN="6" ALIGN="CENTER">' + ' &nbsp;at &nbsp;' +
    TimeToStr(Time) + ' &nbsp;on &nbsp;' + DateToStr(Date) +
    ' &nbsp; &nbsp;contains the following &nbsp;' + IntToStr(keeps_list.Count) +
    ' &nbsp;templates :<BR><HR></TD></TR>';

  for line_now := 0 to keeps_list.Count - 1 do begin

    case Ttemplate(keeps_list.Objects[line_now]).template_info.keep_dims.box_dims1.bgnd_code_077 of
      -1:
        type_str := 'LIBRARY';
      0:
        type_str := 'UNUSED';
      1:
        type_str := 'ON BACKGROUND';
      else
        type_str := '';
    end;//case


    html_str := html_str + #13 + '<TR><TD>' + IntToStr(line_now + 1) +
      '</TD>' + '<TD>' + type_str + '</TD>' + '<TD>' + Ttemplate(
      keeps_list.Objects[line_now]).template_info.keep_dims.box_dims1.id_number_str +
      '</TD>' // 208b
      + '<TD>' + Trim(keepform_listbox.Items.Strings[line_now]) + '</TD>';

    show_str := Trim(Ttemplate(keeps_list.Objects[line_now]).template_info.keep_dims.box_dims1.top_label);

    if Pos('BH •', show_str) = 1 then begin
      html_str := html_str + '<TD>BH</TD>';
      show_str := StringReplace(show_str, 'BH •', '&nbsp;', [rfReplaceAll, rfIgnoreCase]);
    end
    else begin
      if Pos('FB •', show_str) = 1 then begin
        html_str := html_str + '<TD>FB</TD>';
        show_str :=
          StringReplace(show_str, 'FB •', '&nbsp;', [rfReplaceAll, rfIgnoreCase]);
      end
      else
        html_str := html_str + '<TD>&nbsp;</TD>';
    end;

    html_str := html_str + '<TD>' + show_str + '</TD></TR>';
  end;//for

  html_str := html_str + #13 + '<TR><TD COLSPAN="6" ALIGN="CENTER"><HR></TD></TR>';

  if save_done = True then
    html_str := html_str +
      '<TR><TD COLSPAN="6" ALIGN="CENTER">The above box contents have been saved to, or are unchanged since reloading from :</TD></TR>'
      + '<TR><TD COLSPAN="6" ALIGN="CENTER">' + saved_box_str + '</TD></TR>'
  else
    html_str := html_str +
      '<TR><TD COLSPAN="6" ALIGN="CENTER">The box has not yet been saved with the current contents.</TD></TR>';

  html_str := html_str + '<TR><TD COLSPAN="6">&nbsp;</TD></TR>';

  if reloaded_box_str <> '' then begin
    html_str := html_str +
      '<TR><TD COLSPAN="6" ALIGN="CENTER">Since startup or the box contents were last saved the following files have been loaded :</TD></TR>';

    full_string := reloaded_box_str;
    repeat
      all_done_full := parse_text_for_newline(full_string, next_string);

      html_str := html_str + '<TR><TD COLSPAN="6" ALIGN="CENTER">' +
        next_string + '</TD></TR>';

    until all_done_full = True;

  end
  else
    html_str := html_str +
      '<TR><TD COLSPAN="6" ALIGN="CENTER">Since startup or the box contents were last saved no files have been loaded.</TD></TR>';


  html_str := html_str + '</TABLE></BODY></HTML>';

  // 218c .. export HTML file

  export_list := TStringList.Create;
  export_list.Text := html_str;
  export_list.SaveToFile(Config.GetFilePath(csfiBoxListH));
  export_list.Free;

  keep_html_view.DefFontColor := printer_text_font.Color;  // 208b

  keep_html_view.LoadFromString(html_str);

  { OT-FIRST
  if pdf_box_list=True
     then begin

            with save_pdf_file_dialog do begin
              InitialDir:=Config.GetDir(cudiPdfs);
              FileName:=remove_invalid_str(Copy(Trim(box_project_title_str),1,18)+'_box_list'+FormatDateTime('_yyyy_mm_dd_hhmm_ss',Date+Time))+'.pdf';
              Title:='    save  PDF  file  as ...';

              if Execute=False then EXIT;

                // invalid entered chars removed by dialog

              keep_pdf_printer.FileName:=ExtractFilePath(FileName)+lower_case_filename(ExtractFileName(FileName));   // to underscores and lower case

            end;//with

            keep_pdf_printer.Info.Title:='Templot storage box list';
            keep_pdf_printer.Info.Subject:='List of templates for '+box_project_title_str;

            if keep_pdf_setup_dialog.Execute=False then EXIT;

            metafile_printer:=TMetafilePrinter.Create(nil);

            keep_html_view.PrintPreview(metafile_printer);

            num_of_print_pages:=keep_html_view.NumPrinterPages;   // for header

            keep_pdf_printer.BeginDoc;

            try
              for page:=0 to metafile_printer.LastAvailablePage-1 do begin

                keep_pdf_printer.StartPage(metafile_printer.PageWidth,metafile_printer.PageHeight,metafile_printer.PixelsPerInchX,metafile_printer.PixelsPerInchY,0);
                keep_pdf_printer.Canvas.Draw(0,0,metafile_printer.MetaFiles[page]);
                keep_pdf_printer.EndPage;

              end;//for
            finally
              keep_pdf_printer.EndDoc;
            end;//try

            note_str:='||'+IntToStr(num_of_print_pages)+' page(s)'
                     +'||page width: '+IntToStr(metafile_printer.PageWidth)+' dots at '+IntToStr(metafile_printer.PixelsPerInchX)+' dots per inch'
                     +'|page height: '+IntToStr(metafile_printer.PageHeight)+' dots at '+IntToStr(metafile_printer.PixelsPerInchY)+' dots per inch|';

            show_pdf_result(keep_pdf_printer.Filename,note_str);     // 205a

            metafile_printer.Free;

          end//pdf

     else begin}

  // print
  print_html_pages(2);

  { OT-FIRST end;}

end;
//______________________________________________________________________________

procedure Tkeep_form.lock_at_buttonClick(Sender: TObject);

const
  help_str: string = '    storage  box  zoom.' +
    '||The zoom options in the storage box are similar to those available on the trackpad.' +
    '||( Do not confuse these two settings, which are completely independent.)' +
    '||To lock the zoom size, enter a dimension in mm which corresponds to the required full width of the drawings in the storage box.'
    +
    ' All subsequent templates displayed in the storage box will be scaled to match this size. This makes it'
    + ' easier to see the comparative sizes of different templates. The valid range is from 5 mm to 5000 mm.'
    + '||The alternative setting is to click the zoom TO FIT option button, which causes each subsequent template'
    + ' to be displayed at a new scale which nearly fills the storage box drawing space. This makes it easier to see'
    + ' the detail in shorter templates, and ensures that very long templates are fully visible.' +
    '||If you click the zoom LOCK option button, the zoom scaling will be locked at whatever is the current size.';

var
  n: integer;
  od: Toutdim;    // [0..7] array of double;

begin
  if box_width < minfp then
    box_width := 10000 * scale;   // if a funny, make screen 100' scale wide (in 1/100 mm).
  if box_width < 500 then
    box_width := 500;             // 5 mm minimum for (max zoom in).

  n := putdim(help_str, 1, 'full  width  of  the  storage  box  represents',
    box_width / 100, True, True, True, False);
  // mm, no negative, no preset, no zero, don't terminate on zero.
  if n <> 0 then
    EXIT;

  if getdims('storage  box  zoom  lock', '', keep_form, n, od) = True then
    box_width := ABS(od[0]) * 100;
  if box_width < 500 then
    box_width := 500;             // 5 mm minimum for (max zoom in).
  if box_width > 500000 then
    box_width := 500000;       // 5 m maximum (max zoom out).

  lock_radio_button.Checked := True;
  current_state(-1);
end;
//__________________________________________________________________________________________

procedure Tkeep_form.how_panelClick(Sender: TObject);

begin
  box_what_next;
end;
//_________________________________________________________________________________________

procedure box_what_next;

const
  keep_str: string =
    'These help notes are longer than most - you may prefer to print them out. Click the PRINT button above.'
    + '||For additional notes and diagrams please refer to the Templot Companion pages on the Templot web site at  templot.com .'
    + '|-------------------------------------------------' +
    '||        Using  the  Storage  Box' +
    '||This is your STORAGE BOX, the container in which you accumulate templates as you work through a Templot0 session.'
    + '||When you have created a control template on the trackpad which you want to keep, you can add a copy of it to your storage box by selecting' + ' STORE AS UNUSED in the MAIN or RIGHT-CLICK menus. You can also add a copy of the control template to the box by clicking the STORE CONTROL button on the box.' + '||In addition, stored templates can be copied to the background drawing, so that they appear behind the control template on the trackpad.' + ' In this way you can build up a complete track plan. Templates appear on the background drawing reflecting the GENERATOR > GENERATOR SETTINGS > menu options which were in effect when' + ' they were copied to the background. They can be updated to correspond to the current GENERATOR SETTINGS by clicking the REBUILD button on the storage box.' + '||Templates can be added to the box and copied to the background in a single move by selecting STORE && BACKGROUND in the MAIN or RIGHT-CLICK menus on the trackpad.' + '||Stored templates which have not been copied to the background, or have been wiped from the background, are called unused templates.' + ' Stored templates can be copied to the background drawing and wiped from it as often as you wish in trying different track configurations.' + '||Stored templates can also be copied back to become the control template on the trackpad for further work,' + ' and you can choose whether this causes the existing stored template to remain unchanged, or be wiped from the background, or be deleted from the box completely.' + '||Unused templates can also be specified as LIBRARY templates, which means that they are intended as a reference resource of pre-designed custom templates for use in your track planning.' + ' Library templates cannot be copied directly to the background drawing, they must first be copied to the control template on the trackpad.' + '||You can quickly sort through your collection of templates using the left-right arrow buttons, or the slider device above them, and view the templates either as individual drawings or in list form.' + '||( When templates are added to the box they are normally placed at the end of the stored template list, and allocated a new template number.' + ' If you select instead the FILES > RESTORE && BACKGROUND menu item, the new template will be inserted in the list at the position from which the last one was copied out to the pad.' + ' In this way you can modify a stored template and restore it to the box without disturbing the numbering.)' + '||Templot0 maintains a continuous copy on file of your storage box contents and background drawing, and can restore them automatically at the start of the next working session.' + '||In addition, all or part of the contents of the storage box can be saved in a named data file and reloaded at any time.' + '||(The automatic restore function works even if the previous session terminated abnormally because of a system malfunction or power failure, so there is no need to repeatedly save your work simply as a precaution against these events.)' + '||Optionally, any saved file can be reloaded as library templates. In this way you can build up a library of custom templates for use in later track designs.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||The working of the storage box is as follows:' + '||The SHOW BOX LIST and SHOW DRAWING buttons let you choose to see either a drawing of each template, or a list of all the templates in the box (which can also be printed out).' + '||Each template is numbered in the list for reference, and the number of the selected template is displayed above the left-right arrow buttons.' + '||As you sort through your collection of templates, several things happen:' + '||1. If drawings are being shown, the selected template is drawn in the box.' + '||2. The location of the fixing peg on the selected template is highlighted, and its co-ordinate dimensions are shown on the drawing.' + '||3. If this template is currently a BACKGROUND TEMPLATE i.e. being shown in the trackpad background, the fixing peg will be shown in RED, and its position will also be' + ' highlighted in red on the trackpad (although you may need to move the storage box to see it).' + '||4. If this template is an UNUSED TEMPLATE, i.e. not currently being shown on your background drawing, the fixing peg will be shown in BLUE.' + '||5. If this template is a LIBRARY TEMPLATE, i.e. a reference template which is not itself part of your track plan, the fixing peg will be shown in GREEN.' + '||6. The template number also shows in red, blue or green similarly.' + '||7. If this template is currently a member of a GROUP, a square  symbol is shown alongside the number, in red or blue similarly. Library templates cannot be members of a group.' + '||8. The name of the selected template is shown in the panel above the number.' + '||9. The INFO button shows the information text for the selected template. This text is copied from the trackpad information panel when a template is added to the box or reloaded from a file.' + '||If drawings are being shown, the drawing in the box is made using the GENERATOR SETTINGS which are currently in force.' + ' These may differ from the settings which were in force when this template was last copied to the background. To update the background template to the current settings, click the REBUILD button.' + ' The scale of the drawing can be changed. Click the DRAW ZOOM > LOCK AT button and read the help information for more details.' + '||When the list is being shown, a red > mark appears against templates which are currently background templates, i.e. being shown on the trackpad, and the number also appears in red.' + ' For unused templates the number appears in blue. For library templates the number appears in green, and a bullet mark is added to aid quick identification in the list.' + '||In addition to the left-right arrow buttons and slider, templates can also be selected by clicking the required line in the list.' + '||The order of templates in the list can be changed by clicking the brown up and down arrow buttons alongside the list. The selected template is moved one position up or down accordingly for each click on the button.' + '|-------------------------------' + '||The storage box has its own MENUS and KEYBOARD SHORTCUTS which are independent of the trackpad. Most of the menu entries are self-explanatory. Here are some further notes about some of them:' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||The BOX menu includes items for deleting and clearing all templates, and for copying them to and wiping them from the background. The BOX > TOGGLE UNUSED/BACKGROUND menu item swaps the state of every stored template,' + ' i.e. all the unused templates are copied to the background, and all the existing background templates are wiped to become unused. This is useful when you want to compare two different designs by swapping between them.' + '||The BOX > SORT LIBRARY TEMPLATES menu item sorts the list of templates, putting all library templates at the end of the list to make finding them easier.' + '||The BOX menu also includes items for printing the details of the box contents. You can print a simple list of all the templates, or have a complete printout of the INFO and MEMO texts for all of them. Bear in mind that' + ' this could use several sheets of paper. The printer font used for these lists can be changed in the PROGRAM menu on the PROGRAM PANEL window. (If you want to print the texts for a single template,' + ' click the INFO button and then click the PRINT button in the text window.)' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||The FILES menu items concern the saving and reloading of the box contents from data files. When reloading, you can choose to ADD the loaded templates to the existing contents instead of replacing them.' + ' And when saving, you can choose to save all the stored templates, or only a selected group of them.' + '||In this way, by selectively saving templates and adding them from files, you can build up a fresh drawing from previously saved templates.' + '||There is no limit to the number of templates in the box, you can add templates from as many files as you wish in building up your track plan. But you should try not to have more of them copied to the background at any one time' + ' than you actually need to work on, otherwise screen re-draw times will become frustratingly slow.' + '||The difference between the FILES > RELOAD... and FILES > ADD... menu items is that RELOAD additionally gives you the opportunity to clear the existing box contents first.' + '||If the box is cleared or reloaded in error, the FILES > UNDO RELOAD / UNDO CLEAR menu item will restore the previous contents and background drawing.' + '||FILES > SAVE GROUP is only available if a group of templates have first been selected. For more information about using the GROUP SELECT functions, see the notes for the GROUP menu below.' + '||The RELOAD, ADD FILE, SAVE ALL and SAVE GROUP buttons duplicate the commonly used menu entries for convenience.' + '||The FILES > RESTORE PREVIOUS menu item restores the box contents and background drawing to the condition they were in when you quit the previous Templot0 session.' + ' You can do this it any time and as often as you wish, independently of any saving or reloading you may have done since. This function works correctly even if the previous session terminated abnormally as a result of a system malfunction.' + ' Be aware that this function re-instates the box contents as they were when you QUIT the previous session, NOT as they were when you last saved them to a data file.' + '||The FILES > RESTORE PRIOR-PREVIOUS menu item works similarly, but restores the box contents and background drawing to the condition they were in when you quit the Templot0 session prior to the previous one, i.e. the data from two sessions back.' + '||Selecting the FILES > EXPORT DXF... menu item generates a DXF drawing exchange file from the complete background for transfer to other CAD or drawing software. For more details click the help button in the DXF window which appears.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||The EDIT > MAKE LIBRARY TEMPLATE menu item converts an unused template to a library template. This function is available only for unused templates. If the required template is a background template, it must first be wiped from the background.' + ' ( An alternative method is to copy it to the control template, and then click the MAIN > STORE AS LIBRARY TEMPLATE menu item on the trackpad. This avoids the need to wipe a template which is in use.)' + '||The EDIT > EDIT MEMO NOTES... and ADD JOTTER TEXT TO MEMO menu items allow you to add notes about each individual template which will be included in the template data files.' + '||The EDIT > DELETE menu item deletes the selected template from the box, and from your background drawing if it is a background template. You can undo this by clicking EDIT > UNDO DELETE,' + ' provided you have made no subsequent deletions. The DELETE and REBUILD buttons duplicate the menu items for convenience.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||The GROUP menu items allow you to perform operations on a group of templates instead of on a single template. The group can include background and unused templates.' + ' Templates which are part of the group are indicated in the box with a square  symbol alongside the number, and are drawn in the group colour on the trackpad.' + '||To add or remove the selected template to or from the group, click the SELECT (TOGGLE) menu item. Other menu items permit forming a group comprised of all the stored templates,' + ' or all the background templates, or all the unused templates. To remove all templates from the group click the SELECT NONE menu item.' + '||The INVERT SELECTIONS menu item removes all the existing templates from the group and forms a new group comprising all the others.' + ' When you want to select a group containing all the templates except for a few, it is usually quicker to select just those few and then click INVERT SELECTIONS.' + '||This is also a useful way to split your drawing into two files, using the SAVE GROUP menu item, which creates a data file containing only the group templates.' + '||The GROUP SELECT buttons on the storage box duplicate some of the menu items for convenience.' + '||In adition to the save and copy functions in the GROUP menu, there are several shift and rotate functions in the trackpad menus which apply to the templates in the selected group.' + ' In this way the templates comprising a complex track formation can be re-positioned on the drawing as a single unit.' + '||Many of the group select functions are duplicated in the GROUP > GROUP SELECT menu items on the trackpad, but in this case apply only to background templates.' + ' To include unused templates in a group the storage box group functions must be used.' + '||Templates can also be added to a group as they are added to the box from a data file. This is useful if you know they will all need to be re-positioned, or simply to identify them in a different colour on the pad.' + ' For more information see the notes for the ON ADD ONLY option below.' + '||The GROUP > TOGGLE GROUP UNUSED/BACKGROUND menu item works in the same way as the item in the BOX menu (see above), but in this case applies only to the selected group of templates.' + ' By using this function in conjunction with INVERT SELECTIONS, or by forming a group as templates are added from a file, you can compare different possible designs by superimposing groups of templates.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||The OPTIONS menu items provide several options to let you customise the way your storage box works. The various settings are:' + '||ON STORE FROM PAD: The SHOW BOX setting shows the storage box whenever a template is added from the trackpad.' + '||ON STORE && COPY FROM PAD: These settings apply when a template is stored and copied immediately to the background from the trackpad.' + ' The HIDE CONTROL TEMPLATE setting causes the control template on the trackpad to be hidden, so that the background template newly created from it can be seen. If you select' + ' COPY BEHIND CONTROL, the new background template won''t be visible until you shift the control template from its position covering it.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||ON MAKE CONTROL: These settings apply when you click the green CONTROL TEMPLATE button to make the control template on the trackpad a copy of the currently selected template in the box:' + '||1. WIPE FROM BACKGROUND causes this template to be removed from the background drawing. It remains in the box, but becomes an unused template. Use this setting when you want to modify a template which is on the background.' + ' Store it again when you have modified it, and copy it again to the background.' + '||2. COPY OVER BACKGROUND. Use this setting when you intend to use the copied template elsewhere, and do not want to disturb the existing background.' + '||3. The COPY ONTO NOTCH and COPY ONTO DATUM settings cause the copied template to be shifted to these locations, saving the need to move them after copying.' + '||4. The NAME settings let you choose whether the template''s existing name should follow it to the pad, or a new name be used for the control template.' + '||5. If HIDE BOX AND REDRAW is selected, the storage box will be hidden after the template is copied to the pad, otherwise it remains showing.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||MODIFY ON REBUILD: These settings apply when you click the REBUILD, REBUILD GROUP or REBUILD ALL menu items and buttons.' + '||Normally when a background template is rebuilt,' + ' the stored settings for timbering and rail lengths remain unchanged, and are used for the new background drawing. But if the TIMBERING+LENGTHS AS CONTROL option is selected, the background templates being rebuilt' + ' will have these stored settings changed to match the control template:' + '||Plain track sleepers length and width.' + '|Plain track rail lengths and sleeper spacings.' + '|Turnout timbers length and width.' + '|Turnout timbers length increments setting.' + '|Turnout timber spacing, equalizing and centralizing settings.' + '|Timber randomizing settings.' + '|Rail joint settings.' + '||This option is useful when you want to update all or part of the background drawing to correspond with the control template, for example if a custom setting has been entered for the rail lengths.' + ' It also makes it easy to produce different versions of your drawing, for example one version with equalized turnout timbering and one with square-on timbering.' + '||Remember to reset the TIMBERING+LENGTHS AS STORED menu option afterwards, otherwise subsequent rebuilds may modify your background templates unintentionally.' + '||N.B. Only background templates can be modified on rebuild. To modify unused templates, temporarily copy them to the background and rebuild them as a group.' + ' In the GROUP menu, click the SELECT ALL UNUSED, COPY GROUP TO BGND, REBUILD GROUP, WIPE GROUP menu items in sequence.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||ON RELOAD OR ADD: When templates are saved a record is made in the data file of whether they are currently background templates, or currently unused templates.' + ' On reloading them, you can choose whether unused templates in the file should be loaded, and whether background templates in the file should be immediately copied to the background or loaded as unused.' + '||To have them copied immediately to the background select the UPDATE BACKGROUND menu item.' + ' If IGNORE BACKGROUND is chosen, they will be loaded as unused and not go on the background until you copy them to it. These settings have no effect on templates which were originally unused when saved.' + '||If the LOAD ALL TEMPLATES option is selected, all templates in the file will be loaded into the box. If IGNORE UNUSED TEMPLATES is selected, only background templates will be loaded into the box.' + ' Any unused templates in the file will be ignored.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||ON ADD ONLY: Templates added to the box from a file can be formed into a new selected group or added to an existing group.' + '||If the FORM NEW GROUP menu option is selected, any existing group selections will be cleared and the templates added will form a new group.' + ' If ADD TO EXISTING GROUP is selected, no changes will be made to any existing group selections, and the templates added will be selected in addition.' + ' If IGNORE GROUP is selected, no changes will be made to any existing group.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||ON RELOAD ONLY: These settings apply after reloading the storage box (but not for ADD). If MINT CONTROL FROM FINAL TEMPLATE is selected, a mint control template will be created from the final template loaded.' + ' This is useful at the start of a session as it automatically sets your previous gauge and scale ready for further work. For more information about mint templates,' + ' select the TEMPLATE > QUICK SET... menu item and click the ? HELP button on the quick set window.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||RESTORE ON STARTUP: These options permit you to customise the way the automatic restore on startup function works. Changes you make here will take effect the next time you start a Templot0 session, they have no effect on the current session.' + '||If AUTO RESTORE PREVIOUS DATA is selected, the next time you start a Templot0 session your storage box and background drawing will be automatically re-instated to the condition in which you leave it when you quit this session.' + ' If ASK is selected, you will first be asked whether you want this to happen. These option settings will persist for subsequent startups until you change them.' + '||If START WITH CLEAR BOX AND TRACKPAD is selected, the automatic restore function will be ignored for the next startup. This option setting will apply for one startup only,' + ' and will revert to ASK for subsequent startups unless you change it each time.' + '||Regardless of which option you choose, the previous box contents can be restored at any time by selecting the FILES > RESTORE PREVIOUS menu item. For more information see the notes for the FILES menu above.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||N.B.|The storage box contains the template specification for each template, not an actual drawing of it.' + '||When a template is selected it is only then redrawn in the box using the current settings from the trackpad GENERATOR > GENERATOR SETTINGS > menu options.' + ' This means that if, say, you have switched off the timber outlines since the currently selected template was added to the box, it will now' + ' appear without any timber outlines. If you then copy the drawing to the background these settings will be locked in, so that it will appear in the background without timber outlines, and remain so even after you switch them back on.' + '||But the template specification itself has not changed. You can restore the timber outlines to the storage box drawings simply by switching them back on in the GENERATOR SETTINGS. And you can also restore them to the' + ' background drawings by clicking the REBUILD button.' + '||Templot0 occasionally needs to use the control template itself in some storage box operations, so you may see some unexplained changes take place on the trackpad behind the storage box.' + ' But your control template is always restored when these operations are complete.' + '||              - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -' + '||Handy Hints :' + '||If you want to copy a background template back to the control template for further work, you can do so directly from the trackpad without hunting for it in the storage box. Just click on the template and select the desired' + ' copy option on the pop-up menu.' + '||Likewise, there is no need to use the storage box to select a group of background templates for shifting or rotation, or for export.' + ' On the trackpad click the GROUP > GROUP SELECT > CLICK TEMPLATES TO GROUP menu item. Then you can click on the name labels of the required background templates to select or de-select them to or from a group.' + ' Or click instead the GROUP SELECT (TOGGLE) item on the pop-up menu for each template, or use the group selection FENCE functions.' + ' Also, many of the other box GROUP menu functions are duplicated in the trackpad GROUP menu for convenience.' + '||If you are using Windows NT or 2000 or XP, it can be useful to run two instances of Templot0 concurrently.' + ' It is important to run the second instance as a separate copy of Templot0 from a different folder, otherwise the automatic restore functions will not work as intended.' + ' Choose different screen colours for each instance so that you don''t get confused. Use one to build up the complete background drawing,' + ' with its storage box containing only the finished templates needed for this. Use the other as a scratch trackpad for trial designs, with its storage box as a resource of assorted designs,' + ' oddments and templates reloaded from previous schemes.'
    //  +' You can quickly transfer templates between the two storage boxes using the ECHO functions in the FILES menu.'
    + ' Templot0 can be quickly minimized when swapping instances by pressing the Pause key on the keyboard.';

begin
  //help_form.info_label.Height:=help_form.info_label.Height*3;       // out 0.91 ensure room for this long text.
  if help(0, keep_str, 'more  storage  box  chat') = 1 then
    chat(chat_str);
  //help_form.info_label.Height:=help_form.info_label.Height div 3;   // out 0.91 he might have re-scaled the help form !! This way should be ok.
end;
//____________________________________________________________________________________________

procedure Tkeep_form.copy_or_wipe_background_buttonClick(Sender: TObject);

begin
  copy_or_wipe_background;
end;
//__________________________________________________________________________________________

procedure copy_or_wipe_background;

var
  n: integer;

  saved_current: Ttemplate_info;
  saved_name_str: string;
  saved_current_memo_str: string;

begin
  n := list_position;
  if (n < 0) or (n > (keeps_list.Count - 1)) or (keeps_list.Count < 1) then
    EXIT;  // safety.

  if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 = -1 then
    EXIT;   // library template, button should be disabled???

  if keep_form.Active = True then
    keep_form.Cursor := crHourGlass;        // might take a while.

  saved_current.keep_shove_list := Tshoved_timber_list.Create;
  fill_kd(saved_current);                              // save control template for restore.
  saved_name_str := current_name_str;
  saved_current_memo_str := current_memo_str;

  try
    if Ttemplate(keeps_list.Objects[n]).bg_copied = True       // wipe it...
    then begin
      if wipe_it(n) = False      // wipe background
      then begin
        alert(5, '    program  error',
          '||Sorry, there is a program error.' +
          '||The background will be cleared and rebuilt.',
          '', '', '', '', '', 'O K', 0);
        rebuild_background(False, True);
      end;
      current_state(-1);
    end
    else begin                                    // copy it...
      copy_keep_to_background(n, True, False);
      current_state(-1);
    end;
  finally
    copy_keep(saved_current);                 // restore his control template.
    current_name_str := saved_name_str;
    current_memo_str := saved_current_memo_str;
    info_form.ref_name_label.Caption := current_name_str;
    saved_current.keep_shove_list.Free;

    keep_form.Cursor := crDefault;
  end;//try

  backup_wanted := True;


  do_background(0);
  //  redraw the complete background (can't redraw current, we are using the keeps box drawings).
  copy_draw_to_pad;       //  pad would be wiped if low-memory option in force.
end;
//________________________________________________________________________________________

procedure copy_keep_to_background(list_index: integer; update_info, reloading: boolean);

// add this keep to the background keeps list.
var
  p: Pointer;
  i, now: integer;
  aq: ERailData;
  new_bgk: Tbgnd_keep;
  ti: Ttemplate_info;
  si, name_str: string;
  pt: TPoint;

  ptr: ^Tmark;          // pointer to a Tmark record.   ###
  markmax: integer;

  temp_rad: double;      // 213b..
  //boundary_centre:Tpex;
  temp: double;

  ///////////////////////////////////////////////////////////

  procedure abort_new_bgk;   // clear ALL requested memory if ANY requests fail.

  var
    n: ERailData;

  begin
    with new_bgk do begin
      SetLength(list_bgnd_marks, 0);
      for n in ERailData do begin
        SetLength(list_bgnd_rails[n], 0);
      end;//for
    end;//with
  end;
  ////////////////////////////////////////////////////////////////

begin

  with new_bgk do begin
    SetLength(list_bgnd_marks, 0);
    for aq in ERailData do begin
      SetLength(list_bgnd_rails[aq], 0);
    end;//for
  end;//with

  if keeps_list.Count > 0 then begin
    with keep_form do begin
      //n:=list_position;
      if (list_index < 0) or (list_index > (keeps_list.Count - 1)) then
        EXIT;  // how did this happen ?

      if Ttemplate(keeps_list.Objects[list_index]).bg_copied = True then
        EXIT;                            // already on background.
      if Ttemplate(keeps_list.Objects[list_index]).template_info.keep_dims.box_dims1.bgnd_code_077
        = -1 then
        EXIT;  // ??? library template.

      ti.keep_shove_list := Tshoved_timber_list.Create;

      try
        copy_template_info_from_to(False,
          Ttemplate(keeps_list.Objects[list_index]).template_info, ti);  // get the keep data.

        copy_keep(ti);    // make the keep data current.

        //show_and_redraw(True,False);                 // no rollback.
        pad_form.length_locked_popup_entry.Click;
        // so that plain track or approach and exit tracks can be drawn.

        normalize_transforms;   // first transform data.
        gocalc(1, 0);            // and force immediate calcs for us.

        name_str := IntToStr(list_index + 1) + '  ' + keepform_listbox.Items.Strings[list_index];
        // name for background list.

        // now create and fill a new entry in the background...

        with new_bgk do begin

          //timestamp:=ti.keep_dims.now_time;   // we need this to find it later.

          timber_numbers_string := '';   // default init.

          xlist_max := xy_max[0];     // max and min list values for printing calcs...
          xlist_min := xy_min[0];
          ylist_max := xy_max[1] + Round(y_datum * 100);
          // background data includes it's y datum...
          ylist_min := xy_min[1] + Round(y_datum * 100);

          planing_end_aq1 := list_planing_mark_aq1;
          // save list index for these locations for printing.
          planing_end_aq2 := list_planing_mark_aq2;


          //shiftrot_selected:=False;

          //                label_loc_X:=label_point.X;
          //                label_loc_Y:=label_point.Y+Round(y_datum*100);

          text_begin_X := 0;      // init label data
          text_begin_Y := 0;

          text_end_X := 0;
          text_end_Y := 0;

          requested_label_string := 'new';
          full_label_string := 'new';
          showing_label_string := '';

          text_font_height := 0 - 13;  // textfontsize:=8;


          // copy mark data from list (not rail ends) ...

          if marks_list_ptr = nil then
            EXIT;                 // pointer to marks list not valid.
          markmax := High(marks_list_ptr);           // max index for the present list.
          if mark_index > markmax then
            mark_index := markmax;  // ??? shouldn't be.

          SetLength(list_bgnd_marks, mark_index);

          for i := 0 to (mark_index - 1) do begin
            // (mark_index is always the next free slot)
            ptr := @marks_list_ptr[i];  // pointer to the next Tmark record.
            if ptr = nil then
              EXIT;

            list_bgnd_marks[i].p1.X := ptr^.p1.X;
            list_bgnd_marks[i].p1.Y := ptr^.p1.Y + Round(y_datum * 100);
            list_bgnd_marks[i].p2.X := ptr^.p2.X;
            list_bgnd_marks[i].p2.Y := ptr^.p2.Y + Round(y_datum * 100);
            list_bgnd_marks[i].code := ptr^.code;

            if ptr^.code = eMC_99_TimberNumber
            //then timber_numbers_string:=timber_numbers_string+ptr^.str+Chr($1B);  // add on the next timber numbering string, + separator.
            then
              timber_numbers_string := timb_numbers_str;  // copy from control template
          end;//for
          // copy rail end marks from lists ...

          for aq in ERailData do begin
            for i := 0 to 1 do begin
              bgnd_endmarks[aq, i] := endmarks[aq, i];
              // rail end mark points. 1/100th mm.
              bgnd_endmarks[aq, i].Y := bgnd_endmarks[aq, i].Y + Round(y_datum * 100);
              bgnd_endmarks_yn[aq, i] := endmarks_yn[aq, i];
              // flag end points exist.
            end;//for
          end;//for
          // copy rail data from list...
          for aq in ERailData do begin

            SetLength(list_bgnd_rails[aq], nlmax_array[aq] + 1);

            if ((plain_track = False) or (aq in eRD_StockRails) or (aq in eRD_AdjacentTracks)) and
              (aqyn[aq] = True)

            // stock rails, adjacent rails, centre-lines only if plain track, and data available ?
            then begin
              for now := 0 to nlmax_array[aq] do begin
                pt := outoflist(aq, now);
                pt.Y := pt.Y + Round(y_datum * 100);

                list_bgnd_rails[aq][now] := pt;
              end;//for next now
            end;

          end;//for next aq

        end;//with new_bgk          // filled local copy.


        with Ttemplate(keeps_list.Objects[list_index]) do begin
          bgnd_keep := new_bgk;                                      // background data.
          template_info.keep_dims.box_dims1.bgnd_code_077 := 1;
          // flag it is now a background keep (for file).
          template_info.keep_dims.box_dims1.pre077_bgnd_flag := True;
          // in case reloaded in older version than 0.77.a
          bg_copied := True;
          // and has been put there.

          // using the control template data, calculate the F7 snap_peg positions...    0.79.a  27-05-06

          with snap_peg_positions do begin

            ctrl_peg_now_pos := calc_snap_peg_data(peg_code);  // current peg.
            ctrl_0_pos := calc_snap_peg_data(0);               // ctrl-0

            ctrl_1_pos := calc_snap_peg_data(1);       // ctrl-1

            ctrl_2_pos := calc_snap_peg_data(2);       // ctrl-2  // TP added 205c

            ctrl_planing_pos := calc_snap_peg_data(100);
            // PLANING added 205e for obtain turnout radius
            ctrl_heel_pos := calc_snap_peg_data(104);
            // HEEL added 205e for obtain turnout radius

            ctrl_3_pos := calc_snap_peg_data(3);       // ctrl-3

            ctrl_cesp_pos := calc_snap_peg_data(108);
            // CESP added 205e for obtain turnout radius

            ctrl_4_pos := calc_snap_peg_data(4);       // ctrl-4
            ctrl_5_pos := calc_snap_peg_data(6);       // ctrl-5
            ctrl_6_pos := calc_snap_peg_data(18);      // ctrl-6
            ctrl_7_pos := calc_snap_peg_data(9);       // ctrl-7
            ctrl_8_pos := calc_snap_peg_data(17);      // ctrl-8
            ctrl_9_pos := calc_snap_peg_data(11);      // ctrl-9
            ctrl_tcp_pos := calc_snap_peg_data(5);     // TCP
            ctrl_mcp_pos := calc_snap_peg_data(8);     // MCP
            ctrl_tolp_pos := calc_snap_peg_data(600);  // TOLP

            ctrl_tminp_pos := calc_snap_peg_data(240);   // TMINP      213b
            ctrl_texitp_pos := calc_snap_peg_data(241);  // TEXITP     213b

            ctrl_mminp_pos := calc_snap_peg_data(260);   // MMINP      217a
            ctrl_mexitp_pos := calc_snap_peg_data(261);  // MEXITP     217a

            ctrl_tsmidp_pos := calc_snap_peg_data(270);  // 218a

            ctrl_knucklebend_pos := calc_snap_peg_data(279); // 218a

            ctrl_atimb_pos := calc_snap_peg_data(280);   // 218a

            ctrl_mid_pos := calc_snap_peg_data(19);      // mid-length  216a

            ctrl_user_pos := calc_snap_peg_data(999);  // user-defined  added 205c

            with boundary_info do begin
              // 213b  record boundary positions, radii and radial centres for extend to boundary function ...

              loc_0 := ctrl_0_pos;        // CTRL-0
              loc_6 := ctrl_6_pos;        // CTRL-6
              loc_9 := ctrl_9_pos;        // CTRL-9
              loc_240 := ctrl_tminp_pos;  // TMINP
              loc_241 := ctrl_texitp_pos; // TEXITP
              loc_260 := ctrl_mminp_pos;  // MMINP
              loc_261 := ctrl_mexitp_pos; // MEXITP


              loc_600 := ctrl_tolp_pos;   // TOLP

              temp := SQR(loc_9.notch_x - loc_0.notch_x) + SQR(loc_9.notch_y - loc_0.notch_y);
              if temp > minfp then
                boundary_diag := SQRT(temp)
              else
                boundary_diag := 0;

            end;//with boundary

          end;//with snaps

          bgnd_half_diamond := half_diamond;
          // used for peg snapping checks. (also in the template_info for file).  0.79.a  27-05-06
          bgnd_plain_track := plain_track;     // ditto
          bgnd_retpar := (retpar_i = 1);         // ditto parallel crossing.
          bgnd_peg_on_zero := (peg_code = 0);    // current peg on ctrl-0.

          // added 205e ...

          bgnd_xing_type := xing_type_i;
          bgnd_spiral := spiral;
          bgnd_turnout_radius := radius_for_obtain;

          bgnd_blanked := (startx > 0);        // 215a
          bgnd_no_xing := (turnoutx < fpx);    // 215a

          this_is_tandem_first := False;     // 218a
          bgnd_gaunt := gaunt;               // 218a

        end;//with Ttemplate

      finally
        ti.keep_shove_list.Free;
      end;//try
    end;//with keep_form

    if update_info = True then begin
      si := '';
      //with info_form do begin
      if info_text_list{info_memo.Lines}.Count > 0 then begin
        for i := 0 to (info_text_list{info_memo.Lines}.Count - 1) do
          si := si + info_text_list{info_memo.Lines}.Strings[i] + '|';
        // info text goes in keeps_list.
        si :=
          si + 'total timbering length on this template = ' +
          round_str(total_template_timber_length, 2);
        //0.97.a
      end;
      //end;//with

      // re-write the info (e.g. for mirror tools = swapped hand, shift data, etc.)..
      // (no need to change the memo or ref_label - won't have changed)

      keeps_list.Strings[list_index] := si;
      Ttemplate(keeps_list.Objects[list_index]).template_info.
        keep_dims.box_dims1.top_label := Copy(info_form.gauge_label.Caption, 1, 99);
      // (in case change of hand on mirror-rebuild.)
    end;

  end;// if count>0
  if reloading = False then
    save_done := False;   // need a re-save on changed box data.
  backup_wanted := True;

  if reloading = False then
    redraw_pad(True, False);
end;
//________________________________________________________________________________________

procedure Tkeep_form.plain_track_as_stored_menu_entryClick(Sender: TObject);     // 214c

begin
  plain_track_as_stored_menu_entry.Checked := True;     // radio item.
end;
//______________________________________________________________________________

procedure Tkeep_form.plain_track_as_control_menu_entryClick(Sender: TObject);    // 214c

begin
  plain_track_as_control_menu_entry.Checked := True;     // radio item.
end;
//______________________________________________________________________________

procedure Tkeep_form.timbering_as_stored_menu_entryClick(Sender: TObject);

begin
  timbering_as_stored_menu_entry.Checked := True;     // radio item.
end;
//________________________________________________________________________________________

procedure Tkeep_form.timbering_as_control_menu_entryClick(Sender: TObject);

begin
  timbering_as_control_menu_entry.Checked := True;     // radio item.
end;
//_________________________________________________________________________________________

procedure Tkeep_form.customize_xing_as_stored_menu_entryClick(Sender: TObject);    // 214b

begin
  customize_xing_as_stored_menu_entry.Checked := True;     // radio item.
end;
//______________________________________________________________________________

procedure Tkeep_form.customize_xing_as_control_menu_entryClick(Sender: TObject);   // 214b

begin
  customize_xing_as_control_menu_entry.Checked := True;     // radio item.
end;
//______________________________________________________________________________

procedure Tkeep_form.trackbed_edges_as_stored_menu_entryClick(Sender: TObject);      // 0.93.a

begin
  trackbed_edges_as_stored_menu_entry.Checked := True;  // radio item
end;
//_______________________________

procedure Tkeep_form.trackbed_edges_as_control_menu_entryClick(Sender: TObject);     // 0.93.a

begin
  trackbed_edges_as_control_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.centre_lines_as_stored_menu_entryClick(Sender: TObject);        // 0.93.a

begin
  centre_lines_as_stored_menu_entry.Checked := True;  // radio item
end;
//_______________________________

procedure Tkeep_form.centre_lines_as_control_menu_entryClick(Sender: TObject);       // 0.93.a

begin
  centre_lines_as_control_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.centre_line_offset_options_as_stored_menu_entryClick(Sender: TObject);
// 214a

begin
  centre_line_offset_options_as_stored_menu_entry.Checked := True;  // radio item
end;
//_______________________________

procedure Tkeep_form.centre_line_offset_options_as_control_menu_entryClick(Sender: TObject);
// 214a

begin
  centre_line_offset_options_as_control_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.rail_section_data_as_stored_menu_entryClick(Sender: TObject);   // 0.94.a

begin
  rail_section_data_as_stored_menu_entry.Checked := True;  // radio item
end;
//______________________________

procedure Tkeep_form.rail_section_data_as_control_menu_entryClick(Sender: TObject);  // 0.94.a

begin
  rail_section_data_as_control_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.radius_warning_limit_as_stored_menu_entryClick(Sender: TObject);  // 206e

begin
  radius_warning_limit_as_stored_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.radius_warning_limit_as_control_menu_entryClick(Sender: TObject); // 206e

begin
  radius_warning_limit_as_control_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.rename_menu_entryClick(Sender: TObject);

var
  n: integer;
  keep_name_str, idnum_str, s: string;

begin
  if keeps_list.Count < 1 then
    EXIT;

  n := list_position;
  if (n < 0) or (n > (keeps_list.Count - 1)) then
    EXIT;  // how did this happen ?

  keep_name_str := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.reference_string;

  idnum_str := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.id_number_str;
  // 208a ID number


  with math_form do begin
    Caption := '   name  or  rename  this  template ...';
    big_label.Caption := insert_crlf_str('|             Name or rename template :  ' +
      idnum_str + '|||' + keep_name_str +
      '|||Name or rename this template by entering a new name below.' +
      '|||If the existing name includes any prefix tags in square brackets,|take care not to delete or modify them unintentionally.' + '|Enter the new name after the square brackets.');

    math_editbox.Text := keep_name_str;

    do_show_modal(math_form);  // 212a

    if ModalResult = mrOk then begin
      s := Trim(math_editbox.Text);
      if s <> keep_name_str              // has he changed it?
      then begin

        keep_name_str := s;

        Ttemplate(
          keeps_list.Objects[n]).template_info.keep_dims.box_dims1.reference_string :=
          keep_name_str;
        save_done := False;
        backup_wanted := True;
      end;
    end;
    Caption := '    ' + Application.Title;   // reset form caption.
  end;//with

  current_state(-1);
end;
//________________________________________________________________________________________

procedure Tkeep_form.paper_colour_menu_entryClick(Sender: TObject);

begin
  keep_paper_colour := get_colour('choose  a  new  colour  for  the  storage  box  drawing  paper',
    keep_paper_colour);
  SetFocus;
  current_state(-1);
end;
//_____________________________________________________________________________________________

procedure Tkeep_form.grid_colour_menu_entryClick(Sender: TObject);

begin
  keep_grid_colour := get_colour('choose  a  new  colour  for  the  storage  box  grid  lines',
    keep_grid_colour);
  SetFocus;
  current_state(-1);
end;
//______________________________________________________________________________________

procedure Tkeep_form.rail_colour_menu_entryClick(Sender: TObject);

begin
  keep_rail_colour := get_colour('choose  a  new  colour  for  the  storage  box  rails',
    keep_rail_colour);
  SetFocus;
  current_state(-1);
end;
//_________________________________________________________________________________________

procedure Tkeep_form.timber_colour_menu_entryClick(Sender: TObject);

begin
  keep_timber_colour := get_colour('choose  a  new  colour  for  the  storage  box  timbers',
    keep_timber_colour);
  SetFocus;
  current_state(-1);
end;
//________________________________________________________________________________________

procedure Tkeep_form.dwg_font_menu_entryClick(Sender: TObject);

begin
  keep_panel.Font.Assign(get_font('choose  a  new  font  for  the  storage  box  drawings',
    keep_panel.Font, True));
  SetFocus;
  current_state(-1);
end;
//_______________________________________________________________________________________

procedure Tkeep_form.delete_menu_entryClick(Sender: TObject);

begin
  delete_keep(False, True);
end;
//________________________________________________________________________________________

procedure Tkeep_form.list_colour_menu_entryClick(Sender: TObject);

begin
  keepform_listbox.Color := get_colour('choose  a  new  colour  for  the  template  list',
    keepform_listbox.Color);
  SetFocus;
  ref_panel.Color := keepform_listbox.Color;
end;
//____________________________________________________________________________________

procedure Tkeep_form.box_title_menu_entryClick(Sender: TObject);

begin
  control_room_form.project_title_menu_entry.Click;
  current_state(-1);
  backup_wanted := True;
end;
//______________________________________________________________________________________

procedure Tkeep_form.export_dxf_menu_entryClick(Sender: TObject);

begin                          // export the background.
  export_templates_dxf;        // go export a DXF file.
end;
//____________________________________________________________________________________

procedure Tkeep_form.clear_menu_entryClick(Sender: TObject);

var
  i: integer;

begin
  if keeps_list.Count < 1 then begin
    alert(6, '    no  stored  templates',
      'Your storage box is currently empty, so there is nothing to clear.' +
      '||For more information about storing background templates click the ? HELP button.',
      '', '', '', '', '', 'O K  -  continue', 0);
    EXIT;
  end;

  if save_done = False   // something there and not saved ?
  then begin
    i := alert(7, '      clear  all  templates ?',
      'You are about to delete all the templates from your storage box, and from your background drawing.'
      + '||These templates can be restored by clicking the UNDO CLEAR / RELOAD menu item.'
      + ' But if any of these templates may be needed again, you should save them in a named data file.'
      + ' Click the the green bar below to save your existing background drawing and storage box contents.'
      + '||Are you sure you want to clear your drawing and delete the contents of your storage box?', '', '', '', 'yes  -  clear  all  templates  without  saving',
      'no  -  cancel', 'yes  -  but  save  first  before  clearing', 0);
    case i of
      5:
        EXIT;
      6:
        if save_box(0, 0, 0, '') = False then
          EXIT;    // save all.
    end;//case
  end
  else begin             // saved.
    if alert(7, '      clear  all  templates ?',
      'You are about to delete all the templates from your storage box, and from your background drawing.'
      + '||These templates can be restored by clicking the UNDO CLEAR / RELOAD menu item.'
      + '||( There have been no changes or additions to your storage box since you last saved or reloaded the contents. )'
      + '||Are you sure you want to clear your drawing and delete the contents of your storage box?',
      '', '', '', '', 'no  -  cancel', 'yes  -  clear  all  templates', 0) = 5 then
      EXIT;
  end;

  //clear_keeps(True,True);   // ok, clear all keeps and background, save existing for undo.

  clear_keeps(False, True);
  // ok, clear all keeps and background, save existing for undo. 0.96.a don't delete .ebk

  //show_list_button.Click;
end;
//________________________________________________________________________________________

procedure Tkeep_form.save_all_menu_entryClick(Sender: TObject);

begin
  save_box(0, 0, 0, '');         // don't need result flag.
end;
//________________________________________________________________________________________

procedure Tkeep_form.save_bgnd_menu_entryClick(Sender: TObject);
begin
  save_box(0, 1, 0, '');         // don't need result flag.
end;
//_________________________________________________________________________________________

procedure Tkeep_form.save_unused_menu_entryClick(Sender: TObject);

begin
  save_box(0, 2, 0, '');         // don't need result flag.
end;
//________________________________________________________________________________________

procedure Tkeep_form.save_library_menu_entryClick(Sender: TObject);

begin
  save_box(0, 4, 0, '');         // don't need result flag.
end;
//______________________________________________________________________________________

procedure Tkeep_form.save_group_menu_entryClick(Sender: TObject);

begin
  if any_selected = 0 then begin
    if alert_no_group = True    // alert him, and does he want all?
    then
      EXIT;
  end;

  save_box(0, 3, 0, '');        // don't need result flag.
end;
//____________________________________________________________________________________________

procedure mint_final_or_copy_control(i: integer);
// i = index to highest loaded background template for pre 0.93.a files

var
  n: integer;

begin
  // 0.93.a ...

  if keeps_list.Count = 0 then
    EXIT;

  if loaded_version < 93      // old file -- mint from last background template...
  then begin
    //ShowMessage('debug 3  '+IntToStr(loaded_version));

    list_position := i;
    current_state(-1);

    //procedure copy_keep_to_current(on_reload, bgnd_options, name_options, mint_from:boolean);   // copy the current keep to the control template on the pad.
    copy_keep_to_current(True, False, False, True);        // and make it mint on pad.
  end
  else begin      // 0.93.a copy the control template if there is one, otherwise mint as before

    n := keeps_list.Count - 1;
    if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.this_was_control_template
      = True then begin
      //ShowMessage('debug 1  '+IntToStr(loaded_version));

      list_position := n;
      current_state(-1);

      //procedure copy_keep_to_current(on_reload, bgnd_options, name_options, mint_from:boolean);   // copy the current keep to the control template on the pad.
      copy_keep_to_current(True, False, False, False);
      // and make it the control on pad.
    end
    else begin                 // no control template in file
      //ShowMessage('debug 2  '+IntToStr(loaded_version));

      list_position := i;
      current_state(-1);

      //procedure copy_keep_to_current(on_reload, bgnd_options, name_options, mint_from:boolean);   // copy the current keep to the control template on the pad.
      copy_keep_to_current(True, False, False, True);     // and make it mint on pad.
    end;
  end;

  pad_form.fit_bgnd_menu_entry.Click;
  // in case it's larger than the background, e.g. all background unused.
  save_hide := False;
end;
//___________________________________________________________________________________________

procedure Tkeep_form.reload_menu_entryClick(Sender: TObject);

var
  append: boolean;
  hl: integer;

begin
  append := False;                                                           // he might change it.
  if load_storage_box(True, False, '', False, False, append, hl) = False then
    EXIT;     // nothing was loaded.

  if list_panel.Visible = True then
    show_list_button.Click;      // update the list.

  //if (append=False) and (hl>-1) and (hl<keeps_list.Count) then mint_final(hl);    // if something loaded mint from highest bgnd if he so wants.
  if append = True then
    EXIT;
  if (loaded_version < 93) and (hl > -1) and (hl < keeps_list.Count) then
    mint_final_or_copy_control(hl);   // if something loaded mint from highest bgnd if he so wants.
  if (loaded_version > 92) then
    mint_final_or_copy_control(hl);
  // copy the control template if there is one in the file.

end;
//_________________________________________________________________________________________

procedure Tkeep_form.add_file_menu_entryClick(Sender: TObject);

var
  append: boolean;
  dummy: integer;

begin
  append := True;
  if load_storage_box(True, False, '', False, False, append, dummy) = False then
    EXIT;        // nothing was appended.
  if list_panel.Visible = True then
    show_list_button.Click;                  // update the list.
end;
//______________________________________________________________________________________

procedure Tkeep_form.add_library_menu_entryClick(Sender: TObject);

var
  append: boolean;
  dummy: integer;

begin
  append := True;
  if load_storage_box(True, False, '', False, True, append, dummy) = False then
    EXIT;         // nothing was appended.
  if list_panel.Visible = True then
    show_list_button.Click;                  // update the list.
end;
//_______________________________________________________________________________________

procedure Tkeep_form.box_program_panel_menu_entryClick(Sender: TObject);

begin
  //pad_form.control_room_menu_entry.Click;
  pad_form.program_panel_menu_entry.Click;  // 0.91.b
end;
//_______________________________________________________________________________________

procedure Tkeep_form.print_list_menu_entryClick(Sender: TObject);

begin
  print_list_button.Click;
end;
//_______________________________________________________________________________________

procedure Tkeep_form.hide_menu_entryClick(Sender: TObject);

begin
  escape_button.Click;
end;
//________________________________________________________________________________________

procedure Tkeep_form.open_on_keep_menu_entryClick(Sender: TObject);

begin
  open_on_keep_menu_entry.Checked := True;    // radio item.
end;
//________________________________________________________________________________________

procedure Tkeep_form.keep_blind_menu_entryClick(Sender: TObject);

begin
  keep_blind_menu_entry.Checked := True;     // radio item.
end;
//________________________________________________________________________________________

procedure Tkeep_form.close_on_copy_menu_entryClick(Sender: TObject);

begin
  close_on_copy_menu_entry.Checked := True;  // radio item.
end;
//_____________________________________________________________________________________

procedure Tkeep_form.blind_copy_menu_entryClick(Sender: TObject);

begin
  blind_copy_menu_entry.Checked := True;  // radio item.
end;
//____________________________________________________________________________________

procedure Tkeep_form.copy_in_position_menu_entryClick(Sender: TObject);     // 208a was wipe

begin
  copy_in_position_menu_entry.Checked := True;   // radio item.
  //if to_from_bgnd_panel.Color=bgnd_ident_color then make_label.Caption:='wipe to the control template';
end;
//______________________________________________________________________________________

procedure Tkeep_form.copy_onto_notch_menu_entryClick(Sender: TObject);

begin
  copy_onto_notch_menu_entry.Checked := True;   // radio item.
  //make_label.Caption:='copy to the control template';
end;
//______________________________________________________________________________________

procedure Tkeep_form.copy_onto_datum_menu_entryClick(Sender: TObject);

begin
  copy_onto_datum_menu_entry.Checked := True;   // radio item.
  //make_label.Caption:='copy to the control template';
end;
//_____________________________________________________________________________________

procedure Tkeep_form.update_background_menu_entryClick(Sender: TObject);

begin
  update_background_menu_entry.Checked := True;   // radio item
end;
//_______________________________________________________________________________________

procedure Tkeep_form.reload_ignore_bgnd_menu_entryClick(Sender: TObject);

begin
  reload_ignore_bgnd_menu_entry.Checked := True;     // radio item.
end;
//____________________________________________________________________________________

procedure Tkeep_form.add_ignore_group_menu_entryClick(Sender: TObject);

begin
  add_ignore_group_menu_entry.Checked := True;   // radio item
end;
//______________________________________________________________________________________

procedure Tkeep_form.add_to_group_menu_entryClick(Sender: TObject);

begin
  add_to_group_menu_entry.Checked := True;   // radio item
end;
//______________________________________________________________________________________

procedure Tkeep_form.add_new_group_menu_entryClick(Sender: TObject);

begin
  add_new_group_menu_entry.Checked := True;   // radio item
end;
//____________________________________________________________________________________

procedure Tkeep_form.load_all_menu_entryClick(Sender: TObject);

begin
  load_all_menu_entry.Checked := True;   // radio item
end;
//_______________________________________________________________________________________

procedure Tkeep_form.ignore_unused_menu_entryClick(Sender: TObject);

begin
  ignore_unused_menu_entry.Checked := True;   // radio item
end;
//__________________________________________________________________________________________

procedure Tkeep_form.show_on_reload_menu_entryClick(Sender: TObject);

begin
  show_on_reload_menu_entry.Checked := True;     // radio item.
end;
//______________________________________________________________________________________

procedure Tkeep_form.close_on_reload_menu_entryClick(Sender: TObject);

begin
  close_on_reload_menu_entry.Checked := True;     // radio item.
end;
//______________________________________________________________________________________

function duplicates_removed: boolean;
  // remove any duplicate keeps after multiple appends, echo fetches, etc.

  // n.b. this does not remove keeps which are dimensionally identical templates.
  // removes only keeps with the same timestamp, i.e. actual keeps box copies.
var
  n, i, nn: integer;
  ident: integer;

begin
  Result := False;                       // init default return.
  if keeps_list.Count < 1 then
    EXIT;     // empty list.

  n := 0;     // start at top.

  while n < keeps_list.Count do begin        // repeat search for all starting points.
    // n.b. Count decreases on any deletes.

    ident := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.now_time;
    // current search ident.

    i := n + 1;    // start searching at next line.

    while i < keeps_list.Count do begin
      if Ttemplate(keeps_list.Objects[i]).template_info.keep_dims.box_dims1.now_time =
        ident    // found a duplicate
      then begin

        Ttemplate(keeps_list.Objects[i]).template_info.keep_shove_list.Free;
        Ttemplate(keeps_list.Objects[i]).Free;
        keeps_list.Delete(i);
        // delete it. i now points to next line so no need to inc.
        memo_list.Delete(i);
        save_done := False;
        backup_wanted := True;
        Result := True;
      end
      else
        Inc(i);
    end;//while i

    Inc(n); // to next starting point.    (nothing is ever deleted above the starting point).

  end;//while n
end;
//_________________________________________________________________________________________

procedure highlight_bgkeep(index: integer);
// highlight the peg and the current keep on the background.

var
  i, array_max: integer;
  code: EmarkCode;
  now_keep: Tbgnd_keep;
  peg_dim, bg_pegx, bg_pegy: integer;

  p1, move_to, line_to: TPoint;
  //gauge:double;

begin
  if (index < 0) or (index > (keeps_list.Count - 1))
  //]]] or (bgkeeps_form.bgkeeps_listbox.Items.Count<1)
  then
    EXIT;


  copy_draw_to_pad;      // show the draw-bitmap (with the background keeps).
  // this deletes any previous peg highlighting if the current keep
  // is not on the background.
  // (pad would be wiped if low_memory option in force).

  if Ttemplate(keeps_list.Objects[index]).bg_copied = False then
    EXIT;     // not on the background.

  now_keep := Ttemplate(keeps_list.Objects[index]).bgnd_keep;

  draw_background_templates(pad_form.Canvas, 0, index, True, hover_colour);
  //  draw directly on pad and highlight this one. 26-10-99.

  with pad_form.Canvas do begin
    Pen.Width := 1;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Color := clRed;

    Brush.Style := bsSolid;
    Brush.Color := clRed;

    with now_keep do begin
      array_max := High(list_bgnd_marks);

      for i := 0 to array_max do begin
        code := list_bgnd_marks[i].code;

        if code = eMC__1_PegCentre              // code -1, draw highlight on bgnd fixing peg...
        then begin
          p1 := list_bgnd_marks[i].p1;    // x1,y1 in  1/100ths mm

          peg_dim := 10; // 0.91.b was pad_form.ClientWidth div 100;     // 100 arbitrary.

          bg_pegx := Round(p1.X * save_sx + save_ex - save_gx);
          bg_pegy := Round(p1.Y * save_sy + save_by - save_gy);

          move_to.X := bg_pegx - peg_dim;
          move_to.Y := bg_pegy - peg_dim;
          line_to.X := bg_pegx + peg_dim;
          line_to.Y := bg_pegy + peg_dim;

          if check_limits(move_to, line_to) = True then
            Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);
        end;
      end;//for
    end;//with keep.
  end;//with Canvas
end;
//___________________________________________________________________________________

procedure Tkeep_form.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if (Key = 'b') or (Key = 'B') then
    copy_or_wipe_background_button.Click;
end;
//_______________________________________________________________________________________

procedure Tkeep_form.read_info_buttonClick(Sender: TObject);

begin
  if (keeps_list.Count < 1) or (memo_list.Count < 1) then
    EXIT;

  // mods 0.93.a help(-2,info_str,'');

  info_clicked := True;

  data_child_form.Close; // if showing elsewhere

  data_child_form.reset_position();   // child on pad

  data_child_form.data_memo.Text := insert_crlf_str(info_str);
  //  replace embedded | chars with a CR.

  data_child_form.Show;
end;
//______________________________________________________________________________________

procedure Tkeep_form.delete_unused_menu_entryClick(Sender: TObject);

var
  i, n, keep_count: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_unused = 0 then begin
    alert_no_unused;
    EXIT;
  end;

  keep_count := keeps_list.Count;

  if no_unused_msg_pref = False then begin

    alert_box.preferences_checkbox.Checked := False;       //%%%%
    alert_box.preferences_checkbox.Show;

    i := alert(7, '      delete  unused  templates ?',
      '|You are about to delete all unused templates from your storage box.' +
      ' ( All the templates which are not currently being shown on the background and are not library templates.)'
      +
      ' It is not possible to restore the templates which are deleted.' +
      '||If any of these templates may be needed again, you should save them first.' +
      ' Click the SAVE ALL button to save the entire contents of your storage box.' +
      '||Are you sure you want to delete all unused templates ?', '', '', '', '',
      'no   -   cancel', 'yes   -   delete  all  unused  templates', 0);


    no_unused_msg_pref := alert_box.preferences_checkbox.Checked;
    alert_box.preferences_checkbox.Hide;

    if i = 5 then
      EXIT;
  end;

  if keeps_list.Count <> memo_list.Count then
    run_error(220);

  n := 0;
  while n < keeps_list.Count do begin

    if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 <> 0
    // background or library template.
    then begin
      Inc(n);
      CONTINUE;                                             // leave this one.
    end;

    if Ttemplate(keeps_list.Objects[n]).bg_copied = True then
      wipe_it(n);  // ??? not a background template but data on background!

    Ttemplate(keeps_list.Objects[n]).template_info.keep_shove_list.Free;
    Ttemplate(keeps_list.Objects[n]).Free;
    keeps_list.Delete(n);
    memo_list.Delete(n);

    save_done := False;
  end;//while              // no need to increment n, it is now pointing to the next keep.

  if (keep_count <> keeps_list.Count) and (keeps_list.Count > 0) then
    backup_wanted := True;
  current_state(0);

  if keeps_list.Count < 1 then begin
    ShowMessage('Your storage box is now empty.');
    EXIT;
  end;

  if keep_count <> keeps_list.Count then begin
    ShowMessage(IntToStr(keep_count - keeps_list.Count) + ' unused templates were deleted.');
    EXIT;
  end;

  ShowMessage('There are no unused templates.');     // shouldn't get here.
end;
//____________________________________________________________________________________

procedure Tkeep_form.delete_library_templates_menu_entryClick(Sender: TObject);

var
  i, n, keep_count: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_library = 0 then begin
    alert_no_library;
    EXIT;
  end;

  keep_count := keeps_list.Count;

  if no_library_msg_pref = False then begin

    alert_box.preferences_checkbox.Checked := False;       //%%%%
    alert_box.preferences_checkbox.Show;

    i := alert(7, '      delete  library  templates ?',
      'You are about to delete all library templates from your storage box.' +
      '||Are you sure you want to delete all library templates ?', '', '',
      '', '', 'no   -   cancel', 'yes   -   delete  all  library  templates', 0);


    no_library_msg_pref := alert_box.preferences_checkbox.Checked;
    alert_box.preferences_checkbox.Hide;

    if i = 5 then
      EXIT;
  end;

  if keeps_list.Count <> memo_list.Count then
    run_error(220);

  n := 0;
  while n < keeps_list.Count do begin

    if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 <> -1
    // not a library template.
    then begin
      Inc(n);
      CONTINUE;                                             // leave this one.
    end;

    if Ttemplate(keeps_list.Objects[n]).bg_copied = True then
      wipe_it(n);  // ??? library template but data on background!

    Ttemplate(keeps_list.Objects[n]).template_info.keep_shove_list.Free;
    Ttemplate(keeps_list.Objects[n]).Free;
    keeps_list.Delete(n);
    memo_list.Delete(n);

    save_done := False;
  end;//while              // no need to increment n, it is now pointing to the next keep.

  if (keep_count <> keeps_list.Count) and (keeps_list.Count > 0) then
    backup_wanted := True;
  current_state(0);

  if keeps_list.Count < 1 then begin
    ShowMessage('Your storage box is now empty.');
    EXIT;
  end;

  if keep_count <> keeps_list.Count then begin
    ShowMessage(IntToStr(keep_count - keeps_list.Count) + ' library templates were deleted.');
    EXIT;
  end;

  ShowMessage('There are no library templates.');   // shouldn't get here.
end;
//_______________________________________________________________________________________

procedure Tkeep_form.delete_t55_templates_menu_entryClick(Sender: TObject);

// 0.93.a ...

var
  i, n, keep_count, t55: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  t55 := any_t55;

  if t55 = 0 then begin
    ShowMessage('There are no T-55 templates in the storage box.');
    EXIT;
  end;

  if alert(7, 'php/320    delete  all  T - 55  templates ?', 'There are ' +
    IntToStr(t55) + ' T-55 templates currently in your storage box.' +
    '||These are templates which are still set to the fictional T-55 startup gauge.' +
    '||You are about to delete all these templates from your storage box.' +
    '||It is not possible to restore the templates which are deleted. If you think you may need them again you should first save a data file before deleting them.' + '||Are you sure you want to delete all T-55 templates ?', '', '', '', '', 'no   -   cancel', 'yes   -   delete  all  T-55  templates', 0) = 5 then
    EXIT;

  if keeps_list.Count <> memo_list.Count then
    run_error(220);

  keep_count := keeps_list.Count;

  n := 0;
  while n < keeps_list.Count do begin

    with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin

      if not ((proto_info.scale_pi = 5.5) and (proto_info.gauge_pi = 25.4) and
        (proto_info.fw_pi = 1.0) and (this_was_control_template =
        False)  // and not the control template

        ) then begin
        Inc(n);
        CONTINUE;      // leave this one.
      end;
    end;//with

    if Ttemplate(keeps_list.Objects[n]).bg_copied = True then
      wipe_it(n);  // any data on background

    Ttemplate(keeps_list.Objects[n]).template_info.keep_shove_list.Free;
    Ttemplate(keeps_list.Objects[n]).Free;
    keeps_list.Delete(n);
    memo_list.Delete(n);

    save_done := False;

  end;//while              // no need to increment n, it is now pointing to the next keep.

  if (keep_count <> keeps_list.Count) and (keeps_list.Count > 0) then
    backup_wanted := True;
  current_state(0);

  if keeps_list.Count < 1 then begin
    ShowMessage('Your storage box is now empty.');
    EXIT;
  end;

  if keep_count <> keeps_list.Count then begin
    ShowMessage(IntToStr(keep_count - keeps_list.Count) + ' T-55 templates were deleted.');
    EXIT;
  end;

  ShowMessage('There are no T-55 templates.');
end;
//____________________________________________________________________________________

procedure Tkeep_form.sort_library_templates_last_menu_entryClick(Sender: TObject);

var
  n, l_lib, h_nlib: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_library = 0 then begin
    alert_no_library;
    EXIT;
  end;

  // sort any library templates to the end of the list.

  repeat
    l_lib := lowest_library;
    h_nlib := highest_non_library;

    if (l_lib <> -1) and (h_nlib <> -1) and (l_lib < h_nlib) then begin
      keeps_list.Move(l_lib, keeps_list.Count - 1);     // move it to end
      memo_list.Move(l_lib, keeps_list.Count - 1);      // and any memo
    end
    else
      BREAK;
  until 0 <> 0;

  save_done := False;
  backup_wanted := True;

  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.sort_group_templates_first_menu_entryClick(Sender: TObject);

var
  n, l_ng, h_g: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_selected = 0 then begin
    ShowMessage('There are no group tempates currently selected.');
    EXIT;
  end;

  // sort any group templates to the start of the list.

  repeat
    l_ng := lowest_non_group_template;      // lowest index
    h_g := highest_group_template;          // highest index

    if (l_ng <> -1) and (h_g <> -1) and (l_ng < h_g) then begin
      keeps_list.Move(h_g, 0);     // move it to start
      memo_list.Move(h_g, 0);      // and any memo
    end
    else
      BREAK;
  until 0 <> 0;

  save_done := False;
  backup_wanted := True;

  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.sort_group_templates_last_menu_entryClick(Sender: TObject);    // 213b

var
  n, l_g, h_ng: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_selected = 0 then begin
    ShowMessage('There are no group tempates currently selected.');
    EXIT;
  end;

  // sort any group templates to the end of the list.

  repeat
    l_g := lowest_group_template;               // lowest index
    h_ng := highest_non_group_template;         // highest index

    if (l_g <> -1) and (h_ng <> -1) and (h_ng > l_g) then begin
      keeps_list.Move(l_g, keeps_list.Count - 1);     // move it to end
      memo_list.Move(l_g, keeps_list.Count - 1);      // and any memo
    end
    else
      BREAK;
  until 0 <> 0;

  save_done := False;
  backup_wanted := True;

  current_state(-1);
end;
//______________________________________________________________________________

function search_for_template(start_n: integer; str: string): integer;    // 0.93.a

  // search for template name containing string str, starting at index start_n
  // return index of it, or -1 if not found

var
  n: integer;
  s: string;

begin
  Result := -1; // init

  if (str = '') or (start_n < 0) or (start_n > keeps_list.Count - 1) then
    EXIT;

  for n := start_n to keeps_list.Count - 1 do begin

    with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do
      s := reference_string + ' ' + id_number_str;

    if Pos(LowerCase(str), LowerCase(s)) = 0 then
      CONTINUE;

    Result := n;
    BREAK;
  end;//next
end;
//______________________________________________________________________________

procedure Tkeep_form.find_template_menu_entryClick(Sender: TObject);    // 0.93.a

begin
  if keeps_list.Count < 1 then begin
    ShowMessage('Your storage box is empty. Search cancelled.');
    EXIT;
  end;

  with math_form do begin
    Caption := '    find  template ...';
    big_label.Caption := insert_crlf_str('                   Find  Template  or  Templates' +
      '|||Enter below the name of the template to search for, or part of the name.' +
      '||Do not use quotation marks unless you want to search for them in the name.' +
      '|||You can also search for prefix tags or template ID numbers, or part of them.');
    math_editbox.Text := search_str;  // re-use previous

    do_show_modal(math_form);  // 212a   ShowModal

    Caption := '    ' + Application.Title;   // reset form caption for other uses.

    if ModalResult <> mrOk then
      EXIT;

    if math_editbox.Text = '' then begin
      ShowMessage('No search term was entered. Search cancelled.');
      EXIT;
    end;

    search_str := math_editbox.Text;    // save as global

  end;//with

  searched_index := search_for_template(0, search_str);   // save global

  if searched_index = -1 then begin
    ShowMessage('Sorry, ''' + search_str +
      ''' not found in the template names or ID numbers.');
    EXIT;
  end;

  list_position := searched_index;    // make it current.
  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.find_next_menu_entryClick(Sender: TObject);

// re-use search_str, search from last result

begin
  if searched_index = -1        // no original, so can't do next.
  then begin
    find_template_menu_entry.Click;
    EXIT;
  end;

  if keeps_list.Count < 1 then begin
    ShowMessage('Your storage box is empty. Search cancelled.');
    EXIT;
  end;

  if search_str = '' then begin
    ShowMessage('No search term was entered. Search cancelled.');
    EXIT;
  end;

  searched_index := search_for_template(searched_index + 1, search_str);   // save global

  if searched_index = -1 then begin
    ShowMessage('Searching for ''' + search_str + '''.' + #13 + #13 +
      'There are no more matching templates in the storage box.');
    searched_index := 0;   // so can start again at beginning if he wants.
    EXIT;
  end;

  list_position := searched_index;    // make it current.
  current_state(-1);

end;
//______________________________________________________________________________

procedure Tkeep_form.find_and_group_menu_entryClick(Sender: TObject);

var
  n: integer;
  s: string;

begin
  if keeps_list.Count < 1 then begin
    ShowMessage('Your storage box is empty. Search cancelled.');
    EXIT;
  end;

  with math_form do begin
    Caption := '    find  and  group  templates ...';
    big_label.Caption := insert_crlf_str('                   Find  and  Group  Templates' +
      '||||Enter below the name of the template to search for, or part of the name or ID number.'
      +
      '||Any existing group will be cancelled and all templates which match your search will be selected as a new group.'
      + '||(Do not use quotation marks unless you want to search for them in the name.)');

    math_editbox.Text := search_str;  // re-use previous

    do_show_modal(math_form);  // 212a   ShowModal

    Caption := '    ' + Application.Title;   // reset form caption for other uses.

    if ModalResult <> mrOk then
      EXIT;

    if math_editbox.Text = '' then begin
      ShowMessage('No search term was entered. Search cancelled.');
      EXIT;
    end;

    search_str := math_editbox.Text;    // save as global
  end;//with

  for n := 0 to keeps_list.Count - 1 do begin

    with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do
      s := reference_string + ' ' + id_number_str;

    Ttemplate(keeps_list.Objects[n]).group_selected :=
      Pos(LowerCase(search_str), LowerCase(s)) <> 0;

  end;//next

  unlink_group;
  current_state(-1);

  for n := 0 to (keeps_list.Count - 1) do begin

    if Ttemplate(keeps_list.Objects[n]).group_selected = True then begin
      ShowMessage('A new group was created, matching ''' + search_str +
        ''' in the template name or ID number.');
      EXIT;
    end;

  end;//next

  ShowMessage('Sorry, ''' + search_str + ''' not found in the template names or ID numbers.');
end;
//______________________________________________________________________________

procedure clear_all_selections;

var
  n: integer;

begin
  pad_form.show_group_templates_menu_entry.Checked := True;
  // 209c for next group in case hidden - radio item

  if keeps_list.Count < 1 then
    EXIT;
  for n := 0 to keeps_list.Count - 1 do
    Ttemplate(keeps_list.Objects[n]).group_selected := False;

  unlink_group;
  current_state(-1);
end;
//_______________________________________________________________________________________

procedure rebuild_background(use_modify_options, egg_timer: boolean);
// rebuild all the background keeps.

var
  n, i: integer;
  aq: ERailData;
  save_current: Ttemplate_info;

begin

  save_current.keep_shove_list := Tshoved_timber_list.Create;

  try
    if egg_timer = True then
      Screen.Cursor := crHourglass;

    pad_form.length_locked_popup_entry.Click;
    //  so that plain track or approach and exit tracks can be drawn.

    fill_kd(save_current);               // temporarily store the control template.

    if keeps_list.Count < 1 then
      EXIT;

    for n := 0 to (keeps_list.Count - 1) do begin

      with Ttemplate(keeps_list.Objects[n]) do begin

        if template_info.keep_dims.box_dims1.bgnd_code_077 <> 1 then
          CONTINUE;  // not a background template.

        if bg_copied = True     // first scrub existing data...
        then begin
          with bgnd_keep do begin
            SetLength(list_bgnd_marks, 0);
            for aq in ERailData do begin
              SetLength(list_bgnd_rails[aq], 0);
            end;//for next aq

          end;//with bgnd_keep

          bg_copied := False;
        end;

        // !!! version 0.23 19-OCT-99. Update keep timestamp so can append existing file after shift keeps, mirror keeps, etc...

        // does he want it modified?...

        if (use_modify_options = True) and (keep_form.timbering_as_control_menu_entry.Checked =
          True) then begin
          update_timbering(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and (keep_form.plain_track_as_control_menu_entry.Checked =
          True) then begin
          update_lengths(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.customize_xing_as_control_menu_entry.Checked = True)   // 214b
        then begin
          update_customize_xing(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.trackbed_edges_as_control_menu_entry.Checked = True) then begin
          update_trackbed_edges(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.centre_lines_as_control_menu_entry.Checked = True) then begin
          update_centre_lines(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.centre_line_offset_options_as_control_menu_entry.Checked = True)  // 214a
        then begin
          update_centre_line_offset_options(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.rail_section_data_as_control_menu_entry.Checked = True) then begin
          update_rail_section(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.radius_warning_limit_as_control_menu_entry.Checked = True)  // 206e
        then begin
          update_radius_warning(template_info.keep_dims);
          new_stamp_wanted := True;
        end;


        if new_stamp_wanted = True
        // True=has been modified/shifted/rotated/mirrored, so needs a new timestamp on rebuilding.
        then begin
          template_info.keep_dims.box_dims1.now_time := time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer (will probably use the random figure - calls in quick succession).
          new_stamp_wanted := False;                                   // done it.
          save_done := False;
          // need a fresh save.   !!! version 0.23 19-10-99.
        end;

        list_position := n;                          // now put this keep back on the background.
        copy_keep_to_background(n, True, False);
      end;//with
    end;//for next n

  finally
    copy_keep(save_current);       // restore the control template.
    backup_wanted := True;
    redraw_pad(True, False);

    save_current.keep_shove_list.Free;
    if egg_timer = True then
      Screen.Cursor := crDefault;
  end;//try
end;
//__________________________________________________________________________________________

procedure Tkeep_form.copy_name_menu_entryClick(Sender: TObject);

begin
  copy_name_menu_entry.Checked := True;   // radio item.
end;
//__________________________________________________________________________________

procedure Tkeep_form.new_name_menu_entryClick(Sender: TObject);

begin
  new_name_menu_entry.Checked := True;   // radio item.
end;
//_________________________________________________________________________________________

procedure Tkeep_form.use_current_name_menu_entryClick(Sender: TObject);

begin
  use_current_name_menu_entry.Checked := True;   // radio item.
end;
//__________________________________________________________________________________________

procedure mouse_on_bgkeep(X, Y: integer; clear_highlights: boolean);

// this routine is called by all mouse moves on the pad when not doing mouse actions or drawing.

// find background keep if any at this location.
// and highlight it and the keep name.
// also return keeps list index for it.
// if clickable set clicks_accepted=True;

var
  n: integer;
  now_bgkeep: Tbgnd_keep;
  sel: boolean;
  select_str: string;

  ////////////////////////////////////////////////////////////////

  procedure de_highlight;        // remove highlighting.

  begin
    with pad_form.Canvas do begin
      with now_bgkeep do begin

        name_highlighted := -1;         // now none newly highlighted.
        hover_keep_index := -1;         // return that we are not on one..
        //]]]hover_bgnd_index:=-1;
        shift_click := False;           // nothing now to click on.

        if sel = False    // don't leave previous one showing selected
        then begin
          Font.Assign(pad_form.bgnd_keeps_font_label.Font);
          Brush.Style := bsSolid;
          Brush.Color := paper_colour;

          Font.Height := text_font_height;

          if pad_form.boxed_over_names_menu_entry.Checked = True then begin
            Pen.Color := Font.Color;
            Pen.Style := psSolid;     // 215b
            Rectangle(text_begin_X - 2,
              text_begin_Y - 2, text_end_X + 2, text_end_Y + 2);
          end;
          TextOut(text_begin_X, text_begin_Y, showing_label_string);
          // de-highlight it.
          caption_add('');
          // empty caption.

          draw_background_templates(pad_form.Canvas,
            0, n, False, clBlack);
          // clBlack is dummy, not used. draw directly on pad, this one back to normal. 26-10-99.

        end
        else begin                 // change to normal selection style.

          draw_background_templates(pad_form.Canvas,
            0, n, True, selection_colour);
          //  draw directly on pad and highlight this one. 26-10-99.

          Font.Assign(pad_form.bgnd_keeps_font_label.Font);
          Brush.Style := bsSolid;

          if paper_colour = clBlack then begin
            Brush.Color := clWhite;
            Font.Color := clBlack;
          end
          else begin
            Brush.Color := clBlack;
            Font.Color := clWhite;
          end;
          Font.Height := text_font_height;

          if pad_form.boxed_over_names_menu_entry.Checked = True then begin
            Pen.Color := Font.Color;
            Pen.Style := psSolid;     // 215b
            Rectangle(text_begin_X - 2,
              text_begin_Y - 2, text_end_X + 2, text_end_Y + 2);
          end;
          TextOut(text_begin_X, text_begin_Y, showing_label_string);

        end;
      end;//with keep
    end;//with Canvas
  end;
  ///////////////////////////////////////////////////////////////

begin
  clicks_accepted := False; // default init.

  with pad_form.Canvas do begin

    if keeps_list.Count < 1 then
      EXIT;

    for n := 0 to (keeps_list.Count - 1) do begin

      if Ttemplate(keeps_list.Objects[n]).bg_copied = False then
        CONTINUE;     // not a background template. bug fix 0.79.a  24-05-06

      sel := Ttemplate(keeps_list.Objects[n]).group_selected;

      now_bgkeep := Ttemplate(keeps_list.Objects[n]).bgnd_keep;   // next background keep.

      with now_bgkeep do begin

        if clear_highlights = True        // de-highlight them all..
        then begin
          de_highlight;
          CONTINUE;
        end;

        if (X > text_begin_X) and (X < text_end_X) and (Y > text_begin_Y) and
          (Y < text_end_Y)  // found mouse on one.
        then begin

          if n = name_highlighted then
            clicks_accepted := True;    // existing highlighted can be clicked on.

          if name_highlighted <> -1 then
            CONTINUE;  // another one still highlighted - he must move off it first (overlapping).
          // or still on the same one.

          // highlight a fresh one...

          draw_background_templates(pad_form.Canvas, 0, n, True, hover_colour);
          //  draw directly on pad and highlight this one. 26-10-99.
          name_highlighted := n;

          // set mouse-over styles...

          Font.Assign(pad_form.bgnd_keeps_font_label.Font);
          Font.Color := clBlack;

          Brush.Style := bsSolid;

          if click_bgnd_select = True        // clicking group selections directly.
          then begin
            if paper_colour = clLime then
              Brush.Color := clFuchsia
            else
              Brush.Color := clLime;
          end
          else begin
            if paper_colour = clAqua then
              Brush.Color := clFuchsia   // shift key down or caps lock on...
            else
              Brush.Color := clAqua;
          end;

          Font.Height := text_font_height;

          if click_bgnd_select = True then begin
            select_str := '';
            shift_click := False;
          end
          else begin
            select_str := 'select :  ';     // shift key down.
            shift_click := True;            // flag it.
          end;

          if showing_label_string = '' then
            showing_label_string := full_label_string;

          text_end_X := text_begin_X + TextWidth(showing_label_string);
          text_end_Y := text_begin_Y + TextHeight(showing_label_string);

          if pad_form.boxed_over_names_menu_entry.Checked = True then begin
            Pen.Color := Font.Color;
            Pen.Style := psSolid;     // 215b
            Rectangle(text_begin_X - 2, text_begin_Y - 2, text_end_X + 2, text_end_Y + 2);
          end;

          TextOut(text_begin_X, text_begin_Y, showing_label_string); // highlight it.
          caption_add(select_str + full_label_string);
          // and in the pad caption.

          hover_keep_index := n;        // return the keeps list index.
          clicks_accepted := True;      // can click on it.
        end
        else begin
          if n = name_highlighted then
            de_highlight;
          // mouse moved off this one - must de-highlight it unless it's now been selected by clicking.
        end;

      end;//with bgkeep
    end;//for next bgnd keep
  end;//with Canvas
end;
//_______________________________________________________________________________________

procedure print_texts(info, memo: boolean);        // print all the info texts

var
  type_str: string;

  html_str: string;
  i, n: integer;

  pdf_box_info: boolean;

  { metafile_printer: TMetafilePrinter; }

  page: integer;
  folder_str: string;
  note_str: string;

  all_done_full: boolean;
  full_string, next_string: string;

  show_str: string;

  bold_str, italic_str, underline_str: string;  // 208b

begin
  if (keep_form.keepform_listbox.Items.Count <> keeps_list.Count) // then EXIT;  //??
    or (memo_list.Count <> keeps_list.Count)                        // then EXIT;  //??
    or (keep_form.keepform_listbox.Items.Count < 1)                 // then EXIT;
  then begin
    ShowMessage('The storage box is empty. There is no information for stored templates.');
    EXIT;
  end;

  if no_printer_available = True     // 0.93.a
  then begin
    ShowMessage('There is no printer available. (A printer is needed for PDF files to set the paper size.)');
    EXIT;
  end;


  pdf_box_info := False; // keep compiler happy

  // 208b mods ...

  repeat

    i := alert(4, '    template  info  to  printer  or  to  PDF  file ?',
      'Do you want to print the template information,||or create a PDF file?' +
      '||<I>This function may take a few minutes to finish.</I>' +
      '|||green_panel_begin tree.gif To set the text size and margins click the `0program > printer font + margins...`1 menu item on the program panel, or click the bar below.green_panel_end', '', '', 'set  font  and  margins', 'PDF  file', 'cancel', 'print', 0);
    case i of
      3:
        set_printer_font_margins(keep_form, True);
      4:
        pdf_box_info := True;
      5:
        EXIT;
      6:
        pdf_box_info := False;
    end;//case

  until i <> 3;

  // changed to HTML  208b ...

  if fsBold in printer_text_font.Style then
    bold_str := 'bold'
  else
    bold_str := 'normal';

  if fsItalic in printer_text_font.Style then
    italic_str := 'italic'
  else
    italic_str := 'normal';

  if fsUnderline in printer_text_font.Style then
    underline_str := 'underline'
  else
    underline_str := 'none';


  html_str := '<HTML><HEAD>' + '<style> body, p, table { font-size:' + IntToStr(
    printer_text_font.Size) + 'pt; font-family:"' + printer_text_font.Name +
    '"; ' + 'font-weight:' + bold_str + '; font-style:' + italic_str +
    '; text-decoration:' + underline_str + '; }' +
    ' td {padding-left:8px; padding-right:8px;} </style>' +
    '</HEAD><BODY><TABLE ALIGN="CENTER" WIDTH="100%">' + '<TR><TD COLSPAN="5" STYLE="font-size:' +
    IntToStr(printer_text_font.Size + 4) + 'pt;">' +
    'TEMPLATE INFORMATION</TD><TD>created in Templot0 software from templot.com</TD></TR>' +
    '<TR><TD COLSPAN="6">&nbsp;</TD></TR>' + '<TR><TD COLSPAN="6" STYLE="font-size:' +
    IntToStr(printer_text_font.Size + 3) + 'pt;">' +
    'The Templot0 storage box for: <SPAN STYLE="font-weight:bold;">' +
    box_project_title_str + '</SPAN></TD></TR>' + '<TR><TD COLSPAN="6">' +
    ' &nbsp;at &nbsp;' + TimeToStr(Time) + ' &nbsp;on &nbsp;' + DateToStr(Date) +
    ' &nbsp; &nbsp;contains the following &nbsp;' + IntToStr(keeps_list.Count) +
    ' &nbsp;templates :<BR><HR></TD></TR>';

  for n := 0 to keeps_list.Count - 1 do begin

    case Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 of
      -1:
        type_str := 'LIBRARY';
      0:
        type_str := 'UNUSED';
      1:
        type_str := 'ON BACKGROUND';
      else
        type_str := '';
    end;//case

    html_str := html_str + '<TR STYLE="font-weight:bold;">' + '<TD>' +
      IntToStr(n + 1) + '</TD>' + '<TD>' + type_str + '</TD>' + '<TD>' +
      Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.id_number_str +
      '</TD>' // 208b
      + '<TD>' + Trim(keep_form.keepform_listbox.Items.Strings[n]) + '</TD>';

    show_str := Trim(Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.top_label);

    if Pos('BH •', show_str) = 1 then begin
      html_str := html_str + '<TD>BH</TD>';
      show_str := StringReplace(show_str, 'BH •', '&nbsp;', [rfReplaceAll, rfIgnoreCase]);
    end
    else begin
      if Pos('FB •', show_str) = 1 then begin
        html_str := html_str + '<TD>FB</TD>';
        show_str :=
          StringReplace(show_str, 'FB •', '&nbsp;', [rfReplaceAll, rfIgnoreCase]);
      end
      else
        html_str := html_str + '<TD>&nbsp;</TD>';
    end;

    html_str := html_str + '<TD>' + show_str + '</TD></TR>';

    if memo = True then begin
      full_string := StringReplace(memo_list.Strings[n], '  ', ' &nbsp;',
        [rfReplaceAll, rfIgnoreCase]);  // allow multiple spaces.

      full_string := StringReplace(full_string, ' > ', ' &gt; ', [rfReplaceAll, rfIgnoreCase]);
      // space both sides to remain literal >
      full_string := StringReplace(full_string, ' < ', ' &lt; ', [rfReplaceAll, rfIgnoreCase]);
      // space both sides to remain literal <

      full_string := StringReplace(full_string, '|', '<BR>', [rfReplaceAll, rfIgnoreCase]);

      full_string := StringReplace(full_string, '  ', ' &nbsp;', [rfReplaceAll, rfIgnoreCase]);
      // repeat - allow multiple spaces.

      html_str := html_str +
        '<TR><TD COLSPAN="6" STYLE="padding-left:24px;"><BR><I>memo notes:</I><BR><BR>' +
        full_string + '</TD></TR>';
    end;


    if info = True then begin
      full_string := StringReplace(keeps_list.Strings[n], '  ', ' &nbsp;',
        [rfReplaceAll, rfIgnoreCase]);  // allow multiple spaces.

      full_string := StringReplace(full_string, ' > ', ' &gt; ', [rfReplaceAll, rfIgnoreCase]);
      // space both sides to remain literal >
      full_string := StringReplace(full_string, ' < ', ' &lt; ', [rfReplaceAll, rfIgnoreCase]);
      // space both sides to remain literal <

      full_string := StringReplace(full_string, '|', '<BR>', [rfReplaceAll, rfIgnoreCase]);

      full_string := StringReplace(full_string, '  ', ' &nbsp;', [rfReplaceAll, rfIgnoreCase]);
      // repeat - allow multiple spaces.

      html_str := html_str +
        '<TR><TD COLSPAN="6" STYLE="padding-left:24px;"><BR><I>template information:</I><BR><BR>' +
        full_string + '</TD></TR>';
    end;


    html_str := html_str + '<TR><TD COLSPAN="6"><HR></TD></TR>';

  end;//next

  html_str := html_str + '<TR><TD COLSPAN="6">&nbsp;</TD></TR>';

  if save_done = True then
    html_str := html_str +
      '<TR><TD COLSPAN="6">The above box contents have been saved to, or are unchanged since reloading from :</TD></TR>'
      + '<TR><TD COLSPAN="6">' + saved_box_str + '</TD></TR>'
  else
    html_str := html_str +
      '<TR><TD COLSPAN="6">The box has not yet been saved with the current contents.</TD></TR>';

  html_str := html_str + '<TR><TD COLSPAN="6">&nbsp;</TD></TR>';

  if reloaded_box_str <> '' then begin
    html_str := html_str +
      '<TR><TD COLSPAN="6">Since startup or the box contents were last saved the following files have been loaded :</TD></TR>';

    full_string := reloaded_box_str;
    repeat
      all_done_full := parse_text_for_newline(full_string, next_string);

      html_str := html_str + '<TR><TD COLSPAN="6">' + next_string + '</TD></TR>';

    until all_done_full = True;

  end
  else
    html_str := html_str +
      '<TR><TD COLSPAN="6">Since startup or the box contents were last saved no files have been loaded.</TD></TR>';


  html_str := html_str + '</TABLE></BODY></HTML>';

  keep_form.keep_html_view.DefFontColor := printer_text_font.Color;  // 208b

  keep_form.keep_html_view.LoadFromString(html_str);

  { OT-FIRST
  if pdf_box_info=True    // do PDF
     then begin

            with keep_form.save_pdf_file_dialog do begin
              InitialDir:=Config.GetDir(cudiPdfs);
              FileName:=remove_invalid_str(Copy(Trim(box_project_title_str),1,18)+'_box_list'+FormatDateTime('_yyyy_mm_dd_hhmm_ss',Date+Time))+'.pdf';
              Title:='    save  PDF  file  as ...';

              if Execute=False then EXIT;

                // invalid entered chars removed by dialog

              keep_form.keep_pdf_printer.FileName:=ExtractFilePath(FileName)+lower_case_filename(ExtractFileName(FileName));   // to underscores and lower case

            end;//with

            keep_form.keep_pdf_printer.Info.Title:='Template information';
            keep_form.keep_pdf_printer.Info.Subject:='Full template info for '+box_project_title_str;

            if keep_form.keep_pdf_setup_dialog.Execute=False then EXIT;

            wait_form.cancel_button.Hide;
            wait_form.waiting_label.Caption:='preparing  PDF  file ...';

            wait_form.waiting_label.Width:=wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // 205b bug fix for Wine

            wait_form.Show;
            Application.ProcessMessages;


            metafile_printer:=TMetafilePrinter.Create(nil);

            keep_form.keep_html_view.PrintPreview(metafile_printer);

            num_of_print_pages:=keep_form.keep_html_view.NumPrinterPages;   // for header

            keep_form.keep_pdf_printer.BeginDoc;

            try
              for page:=0 to metafile_printer.LastAvailablePage-1 do begin

                keep_form.keep_pdf_printer.StartPage(metafile_printer.PageWidth,metafile_printer.PageHeight,metafile_printer.PixelsPerInchX,metafile_printer.PixelsPerInchY,0);
                keep_form.keep_pdf_printer.Canvas.Draw(0,0,metafile_printer.MetaFiles[page]);
                keep_form.keep_pdf_printer.EndPage;

              end;//for
            finally
              keep_form.keep_pdf_printer.EndDoc;
            end;//try

            note_str:='||'+IntToStr(num_of_print_pages)+' page(s)'
                     +'||page width: '+IntToStr(metafile_printer.PageWidth)+' dots at '+IntToStr(metafile_printer.PixelsPerInchX)+' dots per inch'
                     +'|page height: '+IntToStr(metafile_printer.PageHeight)+' dots at '+IntToStr(metafile_printer.PixelsPerInchY)+' dots per inch|';

            wait_form.Close;

            show_pdf_result(keep_form.keep_pdf_printer.Filename,note_str);     // 205a

            metafile_printer.Free;

          end//pdf

     else begin}
  // print

  print_html_pages(2);

  { OT-FIRST end;}

end;
//______________________________________________________________________________

procedure Tkeep_form.print_all_info_menu_entryClick(Sender: TObject);

begin
  print_texts(True, False);
end;
//________________________________________________________________________________________

procedure Tkeep_form.print_all_memo_menu_entryClick(Sender: TObject);

begin
  print_texts(False, True);
end;
//_____________________________________________________________________________________

procedure Tkeep_form.print_all_text_menu_entryClick(Sender: TObject);

begin
  print_texts(True, True);
end;
//__________________________________________________________________________________________

procedure Tkeep_form.all_to_bgnd_menu_entryClick(Sender: TObject);

// all unused keeps to background.
var
  n: integer;
  save_current: Ttemplate_info;

begin
  if any_unused = 0 then begin
    alert_no_unused;
    EXIT;
  end;

  if keeps_list.Count < 1 then
    EXIT;

  save_current.keep_shove_list := Tshoved_timber_list.Create;

  try
    Screen.Cursor := crHourGlass;        // might take a while.
    if Application.Terminated = False then
      Application.ProcessMessages;

    fill_kd(save_current);                  // fill with the control template data.

    for n := 0 to (keeps_list.Count - 1) do begin
      if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 =
        0 then begin
        list_position := n;                       // put this unused keep on background.
        copy_keep_to_background(n, True, False);
        // (updates the info in case unused have previously been modified on mirror-group etc.)
      end;
    end;//for

    copy_keep(save_current);
    // reset for pad redraws after copy to background calcs.
    pad_form.fit_bgnd_menu_entry.Click;     // show the new background.
    current_state(-1);
    //]]]if open_bgnd_list_menu_entry.Checked=True then do_bgkeeps;
  finally
    save_current.keep_shove_list.Free;
    Screen.Cursor := crDefault;
  end;//try
  backup_wanted := True;
end;
//__________________________________________________________________________________________

procedure Tkeep_form.wipe_all_menu_entryClick(Sender: TObject);

begin
  if any_bgnd = 0 then begin
    alert(6, '    no  templates  currently  on  background',
      '||There are no stored templates currently copied to the background drawing, and therefore none to be wiped from it.'
      +
      '||To copy a template to the background, select it and then click the `0COPY TO BACKGROUND`1 button.',
      '', '', '', '', '', 'O K', 0);
    EXIT;
  end;
  try
    Screen.Cursor := crHourGlass;        // might take a while.
    if Application.Terminated = False then
      Application.ProcessMessages;

    wipe_all_background;  //bgkeeps_form.clear_button.Click;

  finally
    Screen.Cursor := crDefault;
  end;//try

  current_state(-1);
  backup_wanted := True;
  redraw_pad(True, False);
end;
//___________________________________________________________________________________________

procedure toggle_unused_bgnd(group: boolean);

// swap all or group of unused keeps to background and vice versa.
var
  n: integer;
  save_current: Ttemplate_info;

begin
  if keeps_list.Count < 1 then
    EXIT;

  Screen.Cursor := crHourGlass;        // might take a while.
  if Application.Terminated = False then
    Application.ProcessMessages;

  save_current.keep_shove_list := Tshoved_timber_list.Create;
  fill_kd(save_current);

  try
    for n := 0 to (keeps_list.Count - 1) do begin

      with Ttemplate(keeps_list.Objects[n]) do begin

        if (group = True) and (group_selected = False) then
          CONTINUE;

        if bg_copied = True       // wipe it...
        then begin

          if wipe_it(n) = False      // wipe background data.
          then begin
            alert(5, '    program  error',
              '||Sorry, there is a program error.' +
              '||The background will be cleared and rebuilt.',
              '', '', '', '', '', 'O K', 0);
            rebuild_background(False, True);
          end;
        end
        else begin                                          // copy it...
          if template_info.keep_dims.box_dims1.bgnd_code_077 <> -1 then
            copy_keep_to_background(n, True, False);
        end;
      end;//with
    end;//next n

  finally
    current_state(-1);
    copy_keep(save_current);
    save_current.keep_shove_list.Free;
    Screen.Cursor := crDefault;
  end;//try


  backup_wanted := True;
  redraw(True);
end;
//_____________________________________________________________________________________________

procedure Tkeep_form.toggle_all_bgnd_menu_entryClick(Sender: TObject);

begin
  toggle_unused_bgnd(False);
end;
//_____________________________________________________________________________________________

procedure Tkeep_form.FormCreate(Sender: TObject);

begin

  slider_panel.Cursor := open_slider_cursor;

  if Screen.Height < 700 then begin
    Top := 2;    // move form top left of screen for lo-res.
    Left := 2;
  end;

  // OT-FIRST ClientWidth:=656;
  // OT-FIRST ClientHeight:=434;
  AutoScroll := True;

  if Screen.PixelsPerInch > 120 then
    keepform_listbox.ItemHeight := 26;  // 214a

  // 0.82.a  22-08-06  recent files...

  // 208d bug fix - can't use exe_str because not yet initialised

  if FileExists(Config.GetFilePath(csfiBoxMRU)) then
    boxmru_list.LoadFromFile(Config.GetFilePath(csfiBoxMRU))
  else
    empty_boxmru;

  if boxmru_list.Count <> 9 then
    empty_boxmru;   // file tampered with?

end;
//___________________________________________________________________________________________

procedure Tkeep_form.undo_delete_menu_entryClick(Sender: TObject);

var
  n, m: integer;
  str: string;

begin     // menu is not enabled until there is something there.

  n := -1;   // keep compiler happy
  m := -1;

  try
    n := keeps_list.Add(deleted_keep_string);        // add line to info.
  except
    alert(5, '    undo  delete  error',
      '|||There is an internal problem with undo.' + '||Please quote fail code 901.',
      '', '', '', '', 'cancel  undo', '', 0);
    EXIT;
  end;//try

  try
    m := memo_list.Add(deleted_memo_string);         // add line to memo.
  except
    alert(5, '    undo  delete  error',
      '|||There is an internal problem with undo.' + '||Please quote fail code 902.',
      '', '', '', '', 'cancel  undo', '', 0);
    if n > -1 then
      keeps_list.Delete(n);
    EXIT;
  end;//try

  if (m <> n) or (m < 0) or (m > (keeps_list.Count - 1)) then begin
    alert(5, '    undo  delete  error',
      '|||There is an internal problem with undo.' + '||Please quote fail code 903.',
      '', '', '', '', 'cancel  undo', '', 0);
    if n > -1 then
      keeps_list.Delete(n);
    if m > -1 then
      memo_list.Delete(m);

    EXIT;
  end;

  try
    keeps_list.Objects[n] := Ttemplate.Create;
  except
    alert(1, '      memory  problem',
      '||Unable to undo because of memory problems.' + '||Please quote fail code 904.',
      '', '', '', '', '', 'cancel  undo', 0);

    if n > -1 then
      keeps_list.Delete(n);
    if m > -1 then
      memo_list.Delete(m);
    EXIT;
  end;//try

  save_done := False;
  // need a fresh save.
  init_ttemplate(n);
  // init flags for new keep.
  copy_template_info_from_to(False, deleted_keep, Ttemplate(keeps_list.Objects[n]).template_info);
  // data into list.

  backup_wanted := True;

  if list_position > (keeps_list.Count - 1) then
    list_position := keeps_list.Count - 1;
  if n <= (keeps_list.Count - 1) then
    list_position := n;

  current_state(0);       // update the listbox before any copy to background.

  // refresh or clear backgrounds for newly loaded keep...

  if deleted_keep.keep_dims.box_dims1.bgnd_code_077 = 1 then begin
    if update_background_menu_entry.Checked = True then
      copy_keep_to_background(n, False, False)
    // put it in background.
    else begin
      with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin
        bgnd_code_077 := 0;             // set it unused instead.
        pre077_bgnd_flag := False;
        // in case reloaded in older version than 0.77.a
      end;//with
    end;
  end;

  current_state(1);
  if keep_form.list_panel.Visible = True then
    show_list_button.Click;      // update the list.

  if classic_templot = True     // 0.93.a
  then
    do_hide_current
  else begin
    pad_form.snap_to_zero_menu_entry.Click;   // 0.93.a invalidate after storing.
    xshift := zoom_offsetx + (screenx - turnoutx) / 2;
    // put it on the centre of the pad,
    yshift := (zoom_offsety + screeny / 2.5 - y_datum) * hand_i - g / 2;
    // 0.93.a was /2.0 // and on main centre-line (if straight turnout).
  end;
end;
//_________________________________________________________________________________________

function any_selected: integer;      // any templates group-selected?  return count.

var
  n: integer;

begin
  Result := 0;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).group_selected = True then
      Result := Result + 1;       // return count.
  end;//next keep
end;
//__________________________________________________________________________________________

function any_bgnd: integer;          // any templates on background?  return count.

var
  n: integer;

begin
  Result := 0;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).bg_copied = True then
      Result := Result + 1;       // return count.
  end;//next keep
end;
//__________________________________________________________________________________________

function any_unused: integer;      // any templates unused?  return count.

var
  n: integer;

begin
  Result := 0;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 = 0) and
      (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.this_was_control_template
      = False)   // added 208d for file viewer
    then
      Result := Result + 1;   // return count.
  end;//next keep
end;
//__________________________________________________________________________________________

function any_library: integer;      // any library templates?  return count.

var
  n: integer;

begin
  Result := 0;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 = -1 then
      Result := Result + 1;   // return count.
  end;//next template
end;
//______________________________________________________________________________

function any_t55: integer;      // any T-55 templates?  return count.

  // 0.93.a ...

var
  n: integer;

begin
  Result := 0;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin

    with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin

      if (proto_info.scale_pi = 5.5) and (proto_info.gauge_pi = 25.4) and
        (proto_info.fw_pi = 1.0) and (this_was_control_template = False)
      // and not the control template

      then
        Result := Result + 1;       // return count.

    end;//with

  end;//next
end;
//______________________________________________________________________________

function any_non_library: integer;      // any non_library templates?  return count.

var
  n: integer;

begin
  Result := 0;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 <> -1 then
      Result := Result + 1;   // return count.
  end;//next template
end;
//___________________________________________________________________________________________

function highest_non_library: integer;      // return highest index of a non-library template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := (keeps_list.Count - 1) downto 0 do begin
    if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 <>
      -1 then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//___________________________________________________________________________________________

function lowest_library: integer;      // return lowest index of a library template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077 =
      -1 then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//___________________________________________________________________________________________

function highest_group_template: integer;      // 0.93.a return highest index of a group template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := (keeps_list.Count - 1) downto 0 do begin
    if Ttemplate(keeps_list.Objects[n]).group_selected = True then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//___________________________________________________________________________________________

function lowest_non_group_template: integer;
  // 0.93.a return lowest index of a non-group template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).group_selected = False then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//___________________________________________________________________________________________

function lowest_group_template: integer;       // 213b return lowest index of a group template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).group_selected = True then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//______________________________________________________________________________

function highest_non_group_template: integer;
  // 213b return highest index of a non-group template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := (keeps_list.Count - 1) downto 0 do begin
    if Ttemplate(keeps_list.Objects[n]).group_selected = False then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//______________________________________________________________________________

function highest_bgnd_template: integer;
  // 219a return highest index of a template on the background
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := (keeps_list.Count - 1) downto 0 do begin
    if Ttemplate(keeps_list.Objects[n]).bg_copied = True then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//______________________________________________________________________________

procedure Tkeep_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  info_clicked := False;    // don't change the read texts if the box is not open.
end;
//_________________________________________________________________________________________

procedure Tkeep_form.up_buttonClick(Sender: TObject);

begin
  if keeps_list.Count < 1 then
    EXIT;  //  ???  button shouldn't be visible.

  list_position := list_position + 1;   // up to the next keep.

  if list_position > (keeps_list.Count - 1) then
    list_position := keeps_list.Count - 1;    // top limit.

  current_state(1);      // update the box.
end;
//_________________________________________________________________________________________

procedure Tkeep_form.down_buttonClick(Sender: TObject);

begin
  if keeps_list.Count < 1 then
    EXIT;  //  ???  button shouldn't be visible.

  list_position := list_position - 1;   // down to the next keep.

  if list_position < 0 then
    list_position := 0;   // bottom limit.

  current_state(1);      // update the box.
end;
//_______________________________________________________________________________________

function slider_index(slider_left: integer): integer;  // get list index for this slider position.

begin
  if (keep_form.updown_panel.Width - keep_form.slider_panel.Width) > 0 then begin
    Result := Round(slider_left * (keeps_list.Count - 1) /
      (keep_form.updown_panel.Width - keep_form.slider_panel.Width));
    if Result < 0 then
      Result := 0;                                        // bottom limit.
    if Result > (keeps_list.Count - 1) then
      Result := keeps_list.Count - 1;    // top limit.
  end
  else
    Result := 0;  // ??? funny form sizes.
end;
//___________________________________________________________________________________________

function slider_position(index: integer): integer;     // move slider to match current index.

begin
  if keeps_list.Count > 1 then begin
    Result := Round(index * (keep_form.updown_panel.Width - keep_form.slider_panel.Width) /
      (keeps_list.Count - 1));
    if Result < 0 then
      Result := 0;
    // left limit.
    if Result > (keep_form.updown_panel.Width - keep_form.slider_panel.Width) then
      Result := keep_form.updown_panel.Width - keep_form.slider_panel.Width;    // right limit.
  end
  else
    Result := (keep_form.updown_panel.Width - keep_form.slider_panel.Width) div 2;
  // only one in box, put slider in middle.
end;
//________________________________________________________________________________________

procedure Tkeep_form.slider_panelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  n: integer;
  mpsx, mpsy: integer;
  sp: TPoint;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if Shift = [ssLeft]   // mouse down ?  prepare to drag slider...
  then begin
    Screen.Cursor := crNone;       // form cursor won't change while mouse down.

    with slider_panel do begin
      n := slider_index(Left);
      Color := clWhite;
      BevelInner := bvLowered;

      //slider_mousedown_X:=Width div 2;        // save for mouse move.

      sp.X := Left + (Width div 2);
      sp.Y := Top + (Height div 2);

      mpsx := updown_panel.ClientToScreen(sp).X;
      mpsy := updown_panel.ClientToScreen(sp).Y;

      if (n >= 0) and (n < keeps_list.Count) then begin
        if Ttemplate(keeps_list.Objects[n]).group_selected = True then
          slider_shape.Show
        else
          slider_shape.Hide;

        with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin

          case bgnd_code_077 of
            -1:
              Font.Color := clGreen;
            0:
              Font.Color := clBlue;
            1:
              Font.Color := bgnd_ident_color;
          end;//case

          Caption := IntToStr(n + 1);

          slider_ref_label.Caption := Trim(reference_string);
          slider_number_label.Caption := IntToStr(n + 1);

          this_is_panel.Visible := False;
        end;//with
      end;
    end;//with

    SetCursorPos(mpsx, mpsy);
  end;
end;
//_______________________________________________________________________________________

procedure Tkeep_form.slider_panelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

var
  mps: TPoint;
  n, slider_left: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if Shift = [ssLeft]   // mouse down ?
  then begin
    if GetCursorPos(mps) = False then begin
      mps.X := 0;
      mps.Y := 0;
    end;     // make the slider track the mouse..

    slider_left := (keep_form.updown_panel.ScreenToClient(mps)).X - (slider_panel.Width div 2);
    if slider_left < 0 then
      slider_left := 0;
    if slider_left > (updown_panel.Width - slider_panel.Width) then
      slider_left := updown_panel.Width - slider_panel.Width;

    slider_panel.Left := slider_left;

    with slider_panel do begin
      n := slider_index(Left);

      if (n >= 0) and (n < keeps_list.Count) then begin
        if Ttemplate(keeps_list.Objects[n]).group_selected = True then
          slider_shape.Show
        else
          slider_shape.Hide;

        with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin

          case bgnd_code_077 of
            -1:
              Font.Color := clGreen;
            0:
              Font.Color := clBlue;
            1:
              Font.Color := bgnd_ident_color;
          end;//case

          Caption := IntToStr(n + 1);

          slider_ref_label.Caption := Trim(reference_string);
          slider_number_label.Caption := IntToStr(n + 1);
        end;//with
      end;
    end;//with
  end;
end;
//________________________________________________________________________________________

procedure Tkeep_form.slider_panelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  Screen.Cursor := crDefault;

  with slider_panel do begin
    BevelInner := bvNone;
    Color := clBtnFace;
    Font.Color := clBlack;
    Caption := '•';              // bullet.
  end;//with

  this_is_panel.Visible := True;

  if keeps_list.Count < 1 then
    EXIT;

  list_position := slider_index(slider_panel.Left);      // convert slider X to list index.
  current_state(1);                                    // update the box.
end;
//________________________________________________________________________________________

procedure Tkeep_form.bottom_buttonClick(Sender: TObject);

begin
  if keeps_list.Count < 1 then
    EXIT;  //  ???  button shouldn't be visible.

  list_position := 0;   // bottom limit.
  current_state(1);   // update the box.
end;
//________________________________________________________________________________________

procedure Tkeep_form.top_buttonClick(Sender: TObject);

begin
  if keeps_list.Count < 1 then
    EXIT;  //  ???  button shouldn't be visible.

  list_position := keeps_list.Count - 1;    // top limit.
  current_state(1);                     // update the box.
end;
//___________________________________________________________________________________

procedure Tkeep_form.FormShow(Sender: TObject);

begin
  list_panel.Hide;                                               //  in case left showing...
  if hi_color = True then
    ref_panel.Color := keepform_listbox.Color; //alert_colour[5];  // ice-blue or white.

  with keepform_listbox do
    ItemHeight := Round(Canvas.TextHeight(' ') * 8 / 7); // owner-draw listbox, 8/7 arbitrary.


  if (data_child_form.Visible = True) //and (data_child_form.Parent=pad_form)  // 290b  Graeme
  //and (Windows.GetParent(data_child_form.Handle)=pad_form.Handle)
  then
    keep_form.read_info_button.Click;

  //showmessage(inttostr(Windows.GetParent(data_child_form.Handle))+'  '+inttostr(pad_form.Handle));   // debug

end;
//____________________________________________________________________________________

function edit_memo_str(in_str, name_str: string): string;

var
  i: integer;
  s, su: string;

begin
  Result := in_str;             // default init

  with edit_memo_form do begin

    Caption := '    edit  memo  text  for :  ' + name_str;

    vis_edit_memo.Clear;

    s := in_str;

    repeat                      //  find memo lines separated by | chars.
      i := Pos('|', s);
      if i > 0 then begin
        vis_edit_memo.Lines.Add(Copy(s, 1, i - 1));    // extract a string line for the memo.
        Delete(s, 1, i);
        // and delete it from the input string.
      end;
    until (i = 0) or (Length(s) = 0);

    if do_show_modal(edit_memo_form) = mrOk  // 212a   ShowModal // let him change it.
    then begin
      su := '';
      if vis_edit_memo.Lines.Count > 0 then
        for i := 0 to vis_edit_memo.Lines.Count - 1 do
          su := su + vis_edit_memo.Lines.Strings[i] + '|'; // add line separators.

      su := remove_esc_str(su);  // remove ESC is belt and braces for new file format.

      Result := su;
    end;
  end;//with
end;
//_____________________________________________________________________________________

procedure Tkeep_form.edit_memo_menu_entryClick(Sender: TObject);

var
  new_str: string;

begin
  if (list_position < 0) or (memo_list.Count < 1) or (list_position > (memo_list.Count - 1)) then
    EXIT;

  new_str := edit_memo_str(memo_list.Strings[list_position], Ttemplate(
    keeps_list.Objects[list_position]).template_info.keep_dims.box_dims1.reference_string);

  memo_list.Strings[list_position] := new_str;

  current_state(-1);       // to update the info string.
  backup_wanted := True;
end;
//______________________________________________________________________________________

procedure Tkeep_form.add_jotter_to_memo_menu_entryClick(Sender: TObject);

var
  i: integer;
  su: string;

begin
  if memo_list.Count < 1 then
    EXIT;

  su := memo_list.Strings[list_position];

  with jotter_form do begin
    if jotter_memo.Lines.Count > 0 then
      for i := 0 to jotter_memo.Lines.Count - 1 do
        su := su + jotter_memo.Lines.Strings[i] + '|'; // add line separators.
  end;//with

  memo_list.Strings[list_position] := remove_esc_str(su);
  // remove ESC is belt and braces for new file format.
  current_state(-1);                                     // to update the info string.
  backup_wanted := True;
end;
//______________________________________________________________________________________

procedure Tkeep_form.gauge_change_menu_entryClick(Sender: TObject);

begin
  pad_form.other_gauges_menu_entry.Click;
end;
//________________________________________________________________________________________

procedure Tkeep_form.move_to_top_buttonClick(Sender: TObject);

begin
  if (list_position < 1) or (list_position > (keeps_list.Count - 1)) or (keeps_list.Count < 2) then
    EXIT;

  keeps_list.Move(list_position, 0);
  memo_list.Move(list_position, 0);
  list_position := 0;

  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.move_to_bottom_buttonClick(Sender: TObject);

begin
  if (list_position > (keeps_list.Count - 2)) or (list_position < 0) or (keeps_list.Count < 2) then
    EXIT;

  keeps_list.Move(list_position, keeps_list.Count - 1);
  memo_list.Move(list_position, keeps_list.Count - 1);
  list_position := keeps_list.Count - 1;

  current_state(-1);
end;
//______________________________________________________________________________

procedure keep_move_up_button_click;

var
  t: integer;

begin
  if (list_position < 1) or (list_position > (keeps_list.Count - 1)) or (keeps_list.Count < 2) then
    EXIT;

  t := keep_form.keepform_listbox.TopIndex;                    // index of top displayed item.
  if (t < 0) or (t > (keeps_list.Count - 1)) then
    EXIT;  // ???

  keeps_list.Exchange(list_position - 1, list_position);
  memo_list.Exchange(list_position - 1, list_position);
  list_position := list_position - 1;

  with keep_form.keepform_listbox do begin
    if ItemHeight < 1 then
      EXIT;     // div zero check.
    Visible := False;                // avoid flicker.
    current_state(-1);
    if (list_position >= t) and (list_position < (t + (Height div ItemHeight)))
    // keep same lines showing if poss.
    then
      TopIndex := t
    else
    if list_position < t then
      TopIndex := list_position;

    Visible := True;
  end;//with
end;
//______________________________________________________________________________

procedure Tkeep_form.move_up_buttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

// do first move here, then via timer if held down.
begin
  move_up_button.Tag := 1;
  Application.ProcessMessages;                        // to show button down or quick back up.
  if move_up_button.Tag = 1 then
    keep_move_up_button_click;
end;
//_______________________________

procedure Tkeep_form.move_up_buttonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  move_up_button.Tag := 0;
end;
//______________________________________________________________________________

procedure keep_move_down_button_click;

var
  t: integer;

begin
  if (list_position > (keeps_list.Count - 2)) or (list_position < 0) or (keeps_list.Count < 2) then
    EXIT;

  t := keep_form.keepform_listbox.TopIndex;                    // index of top displayed item.
  if (t < 0) or (t > (keeps_list.Count - 1)) then
    EXIT;  // ???

  keeps_list.Exchange(list_position, list_position + 1);
  memo_list.Exchange(list_position, list_position + 1);

  list_position := list_position + 1;

  with keep_form.keepform_listbox do begin
    if ItemHeight < 1 then
      EXIT;    // div zero check.
    Visible := False;  // avoid flicker.
    current_state(-1);
    if (list_position >= t) and (list_position < (t + (Height div ItemHeight))) then
      TopIndex := t;  // keep same lines showing if poss.
    Visible := True;
  end;//with
end;
//______________________________________________________________________________

procedure Tkeep_form.move_down_buttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

// do first move here, then via timer if held down.
begin
  move_down_button.Tag := 1;
  Application.ProcessMessages;                        // to show button down or quick back up.
  if move_down_button.Tag = 1 then
    keep_move_down_button_click;
end;
//______________________________________________________________________________

procedure Tkeep_form.move_down_buttonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  move_down_button.Tag := 0;
end;
//______________________________________________________________________________

procedure Tkeep_form.keepform_listboxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

var
  tw1, tw2: integer;
  text_colour, used_colour: integer;
  gsel: boolean;
  bg_flag: integer;
  idnum_str: string;      // 208a
  font_name_str: string;  // 208a

  marcol: integer;      // 213b  marker colour ...
  marcol_used: boolean;

  marcol_rect_left, marcol_rect_right: integer;

  rem_flag: boolean;    // 216a..
  rem_col: integer;

begin
  if (Index < 0) or (Index > (keeps_list.Count - 1)) then
    EXIT;  // ???

  with Ttemplate(keeps_list.Objects[Index]) do begin
    idnum_str := template_info.keep_dims.box_dims1.id_number_str;   // 208a
    bg_flag := template_info.keep_dims.box_dims1.bgnd_code_077;
    gsel := group_selected;

    marcol := template_info.keep_dims.box_dims1.pad_marker_colour;           // 213b..
    marcol_used := template_info.keep_dims.box_dims1.use_pad_marker_colour;  // using it

    rem_flag := template_info.keep_dims.box_dims1.align_info.reminder_flag;    // 216a..
    rem_col := template_info.keep_dims.box_dims1.align_info.reminder_colour;

  end;//with

  with keepform_listbox do begin
    with Canvas do begin

      tw1 := 2 + TextWidth('> ');           // bgnd indicator, to start of group identifier.
      tw2 := tw1 + TextWidth('M 123  ');
      // group identifier and template number, to start of string.

      Brush.Style := bsSolid;
      Pen.Width := 1;           // 213b

      used_colour := bgnd_ident_color;     // keep compiler happy.

      if (odSelected in State) = True then begin
        Brush.Color := clBlack;

        case bg_flag of
          -1:
            used_colour := clLime;
          0:
            used_colour := clAqua;
          1:
            used_colour := $000077FF;
          //clRed;   //clFuchsia;       // show brighter against black..
        end;//case

        text_colour := clWhite;
      end
      else begin
        Brush.Color := keepform_listbox.Color;

        case bg_flag of
          -1:
            used_colour := clGreen;
          0:
            used_colour := clBlue;
          1:
            used_colour := bgnd_ident_color;
        end;//case

        text_colour := clBlack;
      end;
      if (odSelected in State) = True then begin
        Brush.Color := clBlack;

        case bg_flag of
          -1:
            used_colour := clLime;
          0:
            used_colour := clAqua;
          1:
            used_colour := $000077FF;
          //clRed;   //clFuchsia;       // show brighter against black..
        end;//case

        text_colour := clWhite;
      end
      else begin
        Brush.Color := keepform_listbox.Color;

        case bg_flag of
          -1:
            used_colour := clGreen;
          0:
            used_colour := clBlue;
          1:
            used_colour := bgnd_ident_color;
        end;//case

        text_colour := clBlack;
      end;

      FillRect(Rect);  // blank the entry first.

      Font.Color := used_colour;

      case bg_flag of     // show the markers
        -1:
          TextOut(Rect.Left + 2, Rect.Top, '    •');
        1:
          TextOut(Rect.Left + 2, Rect.Top, '>');
      end;//case

      TextOut(Rect.Left + tw2 - TextWidth(IntToStr(Index + 1) + '  '), Rect.Top,
        IntToStr(Index + 1));

      Font.Color := text_colour;
      TextOut(Rect.Left + tw2, Rect.Top, Items.Strings[Index]);


      font_name_str := Font.Name;
      Font.Name := 'Courier New';

      TextOut(Rect.Right - TextWidth(idnum_str) - 5, Rect.Top, idnum_str);
      // 208a  template ID number  -5 arbitrary

      Font.Name := font_name_str;


      Font.Color := text_colour;         // 208a

      if gsel = True          // add the group selected marker...
      then begin
        Brush.Color := used_colour;
        Pen.Color := used_colour;
        Rectangle(Rect.Left + tw1, Rect.Top + (ItemHeight div 4), Rect.Left +
          tw1 + (Itemheight div 2), Rect.Top + (ItemHeight * 3 div 4));
        // square bullet indicates group member.

        if (odSelected in State) = True then
          Brush.Color := clBlack                 // restore brush and pen for focusing..
        else
          Brush.Color := keepform_listbox.Color;
      end;


      marcol_rect_left := TextWidth('000000000') + 7;    // clear of the ID number,  7 arbitrary
      marcol_rect_right := TextWidth('000000') + 7;


      if marcol_used = True          // add the marker-colour marker...
      then begin
        Brush.Color := marcol;
        Pen.Color := keepform_listbox.Color;
        // in case marker colour matches list selection colour

        Rectangle(Rect.Right - marcol_rect_left, Rect.Top + (ItemHeight div 4),
          Rect.Right - marcol_rect_right, Rect.Top + (ItemHeight * 3 div 4));

        if (odSelected in State) = True then
          Brush.Color := clBlack                 // restore brush and pen for focusing..
        else
          Brush.Color := keepform_listbox.Color;
      end;

      if rem_flag = True          // finally add the reminder marker...
      then begin
        Brush.Color := rem_col;
        Pen.Color := clBlack;

        Ellipse(Rect.Right - marcol_rect_left - ItemHeight - 4, Rect.Top + 1,
          Rect.Right - marcol_rect_left - 6, Rect.Top + ItemHeight - 1);

        if (odSelected in State) = True then
          Brush.Color := clBlack                 // restore brush and pen for focusing..
        else
          Brush.Color := keepform_listbox.Color;
      end;


      Pen.Color := clBlack;      // 208a
      TextOut(0, 0, '');          // 208a     // Delphi bug - needed for focus dotted lines.

    end;//with
  end;//with
end;
//________________________________________________________________________________________

procedure Tkeep_form.restore_previous_menu_entryClick(Sender: TObject);

var
  i, hl: integer;
  append: boolean;
  prior_str: string;

begin
  if FileExists(pb_str) = False then begin

    if FileExists(pbo_str) = True then
      prior_str := 'restore  prior - previous  data  instead'
    else
      prior_str := '';

    if alert(2, '    no  data  available',
      'Sorry, there is no previous box contents data available.' +
      '||The most likely reason is that your storage box was empty when you last quit Templot0.',
      '', '', '', prior_str, '', 'O K', 0) = 4 then
      restore_prior_previous_menu_entry.Click;

    EXIT;
  end
  else begin
    if keeps_list.Count > 0 then begin
      i := alert(7, '    restore  previous  box  contents',
        'You are about to restore your storage box contents and background drawing to the state they were in when you quit the previous Templot0 session.' + '||This will replace your existing box contents and drawing. If you will need these again you should save them first.', '', '', '', 'restore  previous  contents  without  saving', 'cancel  restore', 'save  first  and  restore  previous  contents', 0);
      case i of
        5:
          EXIT;
        6:
          if save_box(0, 0, 0, '') = False then
            EXIT;     // go save all the keeps box.
      end;//case
    end;

    append := False;
    if load_storage_box(True, False, pb_str, False, False, append, hl) = True then begin
      if keeps_list.Count > 0 then begin
        with Ttemplate(keeps_list.Objects[0]).template_info.keep_dims.box_dims1 do
        begin   // read only from first keep.
          if version_as_loaded > 62 then
            save_done := box_save_done      // mods 23-6-00 for version 0.63
          else
            save_done := False;
        end;//with
        //if (append=False) and (hl>-1) and (hl<keeps_list.Count) then mint_final(hl);    // if something loaded mint from highest bgnd if he so wants.
        if append = True then
          EXIT;
        if (loaded_version < 93) and (hl > -1) and (hl < keeps_list.Count) then
          mint_final_or_copy_control(hl);
        // if something loaded mint from highest bgnd if he so wants.
        if (loaded_version > 92) then
          mint_final_or_copy_control(hl);
        // copy the control template if there is one in the file.
      end;
    end;
  end;
end;
//______________________________________________________________________________________________

procedure Tkeep_form.restore_prior_previous_menu_entryClick(Sender: TObject);

var
  i, hl: integer;
  append: boolean;

begin
  if FileExists(pbo_str) = False then begin
    alert(2, '    no  data  available',
      'Sorry, there is no prior-previous box contents data available.' +
      '||The most likely reason is that your storage box was empty when you quit the Templot0 session prior to the last one.',
      '', '', '', '', '', 'O K', 0);
    EXIT;
  end
  else begin
    if keeps_list.Count > 0 then begin
      i := alert(7, '    restore  prior - previous  box  contents',
        'You are about to restore your storage box contents and background drawing to the state they were in when you quit the Templot0 session PRIOR to the PREVIOUS one, i.e. from two sessions back.' + '||This will replace your existing box contents and drawing. If you will need these again you should save them first.', '', '', '', 'restore  prior - previous  contents  without  saving', 'cancel  restore', 'save  first  and  restore  prior - previous  contents', 0);
      case i of
        5:
          EXIT;
        6:
          if save_box(0, 0, 0, '') = False then
            EXIT;     // go save all the keeps box.
      end;//case
    end;

    append := False;
    if load_storage_box(True, False, pbo_str, False, False, append, hl) = True then begin
      if keeps_list.Count > 0 then begin
        with Ttemplate(keeps_list.Objects[0]).template_info.keep_dims.box_dims1 do
        begin   // read only from first keep.
          if version_as_loaded > 62 then
            save_done := box_save_done      // mods 23-6-00 for version 0.63
          else
            save_done := False;
        end;//with
        //if (append=False) and (hl>-1) and (hl<keeps_list.Count) then mint_final(hl);    // if something loaded mint from highest bgnd if he so wants.
        if append = True then
          EXIT;
        if (loaded_version < 93) and (hl > -1) and (hl < keeps_list.Count) then
          mint_final_or_copy_control(hl);
        // if something loaded mint from highest bgnd if he so wants.
        if (loaded_version > 92) then
          mint_final_or_copy_control(hl);
        // copy the control template if there is one in the file.
      end;
    end;
  end;
end;
//___________________________________________________________________________________________

procedure Tkeep_form.select_menu_entryClick(Sender: TObject);

begin
  with Ttemplate(keeps_list.Objects[list_position]) do begin
    if template_info.keep_dims.box_dims1.bgnd_code_077 <> -1 then
      group_selected := not group_selected
    else
      group_selected := False;                // library template, menu should be disabled???
  end;//with

  if (any_selected < 1) and (group_notch_linked = True) then
    unlink_group;

  current_state(-1);
end;
//___________________________________________________________________________________________

procedure Tkeep_form.select_all_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to keeps_list.Count - 1 do begin
    with Ttemplate(keeps_list.Objects[n]) do
      group_selected := (template_info.keep_dims.box_dims1.bgnd_code_077 <> -1);
    // select if not a library template.
  end;

  current_state(-1);
end;
//____________________________________________________________________________________________

procedure Tkeep_form.select_all_bgnd_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to keeps_list.Count - 1 do begin
    with Ttemplate(keeps_list.Objects[n]) do
      group_selected := (template_info.keep_dims.box_dims1.bgnd_code_077 = 1);
  end;//next n

  if (any_selected < 1) and (group_notch_linked = True) then
    unlink_group;

  current_state(-1);
end;
//____________________________________________________________________________________________

procedure Tkeep_form.select_all_unused_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to keeps_list.Count - 1 do begin
    with Ttemplate(keeps_list.Objects[n]) do
      group_selected := (template_info.keep_dims.box_dims1.bgnd_code_077 = 0);
  end;//next n

  if (any_selected < 1) and (group_notch_linked = True) then
    unlink_group;

  current_state(-1);
end;
//__________________________________________________________________________________________

procedure Tkeep_form.clear_selections_menu_entryClick(Sender: TObject);

begin
  clear_all_selections;
end;
//__________________________________________________________________________________________

procedure Tkeep_form.invert_selections_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to keeps_list.Count - 1 do begin
    with Ttemplate(keeps_list.Objects[n]) do begin
      if template_info.keep_dims.box_dims1.bgnd_code_077 <> -1 then
        group_selected := not group_selected
      else
        group_selected := False;                       // library template???
    end;//with
  end;//for

  unlink_group;
  current_state(-1);
end;
//____________________________________________________________________________________________

procedure Tkeep_form.wipe_group_menu_entryClick(Sender: TObject);

var
  n: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_selected = 0 then begin
    if alert_no_group = True    // alert him, and does he want all?
    then
      EXIT;
  end;

  for n := 0 to (keeps_list.Count - 1) do begin

    with Ttemplate(keeps_list.Objects[n]) do begin

      if (group_selected = True) and (bg_copied = True)
      // selected and currently on background?
      then begin
        if wipe_it(n) = False      // wipe the background.
        then begin
          alert(5, '    program  error',
            '||Sorry, there is a program error.' +
            '||The background will be cleared and rebuilt.',
            '', '', '', '', '', 'O K', 0);
          rebuild_background(False, True);
        end;
      end;
    end;//with
  end;//next n
  current_state(-1);

  save_done := False;
  backup_wanted := True;
  redraw_pad(True, False);
end;
//_________________________________________________________________________________________

procedure Tkeep_form.copy_group_menu_entryClick(Sender: TObject);   // copy group to background.

var
  n: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_selected = 0 then begin
    if alert_no_group = True    // alert him, and does he want all?
    then
      EXIT;
  end;

  for n := 0 to keeps_list.Count - 1 do begin

    with Ttemplate(keeps_list.Objects[n]) do begin

      // selected and not currently on background...?

      if (group_selected = True) and (bg_copied = False) and
        (template_info.keep_dims.box_dims1.bgnd_code_077 <> -1) then
        copy_keep_to_background(n, True, False);  // copy there.

    end;//with
  end;//next n
  current_state(-1);

  save_done := False;
  backup_wanted := True;
  redraw(True);
end;
//____________________________________________________________________________________________

procedure Tkeep_form.toggle_group_bgnd_menu_entryClick(Sender: TObject);

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_selected = 0 then begin
    if alert_no_group = True    // alert him, and does he want all?
    then
      EXIT;
  end;

  toggle_unused_bgnd(True);
end;
//_________________________________________________________________________________________

procedure rebuild_group(use_modify_options, egg_timer: boolean);

var
  n: integer;
  save_current: Ttemplate_info;

begin
  if any_selected = 0 then
    EXIT;

  save_current.keep_shove_list := Tshoved_timber_list.Create;

  try
    if egg_timer = True then
      Screen.Cursor := crHourglass;

    pad_form.length_locked_popup_entry.Click;
    //  so that plain track or approach and exit tracks can be drawn.

    fill_kd(save_current);                        // temporarily store the control template.

    for n := 0 to (keeps_list.Count - 1) do begin

      with Ttemplate(keeps_list.Objects[n]) do begin

        if (group_selected = False) or (bg_copied = False) then
          CONTINUE;     // not selected or on background.

        if wipe_it(n) = False      // first wipe the background.
        then begin
          alert(5, '    program  error',
            '||Sorry, there is a program error.' +
            '||The background will be cleared and rebuilt.',
            '', '', '', '', '', 'O K', 0);
          rebuild_background(False, True);

          save_done := False;
          backup_wanted := True;
          EXIT;
        end;

        bg_copied := False;                      // flag not yet on background.

        // does he want it modified?...

        if (use_modify_options = True) and (keep_form.timbering_as_control_menu_entry.Checked =
          True) then begin
          update_timbering(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.plain_track_as_control_menu_entry.Checked = True)      // 214c
        then begin
          update_lengths(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.customize_xing_as_control_menu_entry.Checked = True)   // 214b
        then begin
          update_customize_xing(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.trackbed_edges_as_control_menu_entry.Checked = True) then begin
          update_trackbed_edges(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.centre_lines_as_control_menu_entry.Checked = True) then begin
          update_centre_lines(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.centre_line_offset_options_as_control_menu_entry.Checked = True)  // 214a
        then begin
          update_centre_line_offset_options(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.rail_section_data_as_control_menu_entry.Checked = True) then begin
          update_rail_section(template_info.keep_dims);
          new_stamp_wanted := True;
        end;

        if (use_modify_options = True) and
          (keep_form.radius_warning_limit_as_control_menu_entry.Checked = True)  // 206e
        then begin
          update_radius_warning(template_info.keep_dims);
          new_stamp_wanted := True;
        end;


        if new_stamp_wanted = True
        // True=has been modified/shifted/rotated/mirrored, so needs a new timestamp on rebuilding.
        then begin
          template_info.keep_dims.box_dims1.now_time :=
            time_now_modified(Random($7FFFFFFF));
          // modify Delphi float time format to integer (will probably use the random figure - calls in quick succession).
          new_stamp_wanted := False;                                   // done it.
          save_done := False;
          // need a fresh save.   !!! version 0.23 19-10-99.
        end;

        list_position := n;                      // put this keep on background.
        copy_keep_to_background(n, True, False);

      end;//with template
    end;//next n
  finally
    copy_keep(save_current);                         // draw the old template on the pad.
    save_current.keep_shove_list.Free;
    backup_wanted := True;
    redraw_pad(True, False);
    if egg_timer = True then
      Screen.Cursor := crDefault;
  end;//try
end;
//________________________________________________________________________________________

procedure Tkeep_form.rebuild_group_menu_entryClick(Sender: TObject);

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_selected = 0 then begin
    if alert_no_group = True    // alert him, and does he want all?
    then
      EXIT;
  end;

  rebuild_group(True, True);

  current_state(-1);
end;
//_________________________________________________________________________________________

procedure delete_group(all_except: boolean);

var
  n, keep_count: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if any_selected = 0 then begin
    if alert_no_group = True    // alert him, and does he want all?
    then
      EXIT;
  end;

  if all_except = True then begin
    if alert(7, '    delete  all  except  selected  group ?',
      'You are about to delete all templates which are not part of the selected group.' +
      '||It is not possible to undo this delete.' +
      '||The remaining templates will be re-numbered.' +
      '||Are you sure you want to delete all templates except the selected group ?',
      '', '', '', '', 'no  -  cancel  delete', 'yes  -  delete  all  except  group', 0) = 5 then
      EXIT;
  end
  else begin
    if alert(7, '      delete  selected  group ?',
      'You are about to delete the selected group of templates.' +
      '||It is not possible to undo a group delete.' +
      '||The remaining templates will be re-numbered.' +
      '||Are you sure you want to delete this group ?', '', '', '', '',
      'no  -  cancel  delete', 'yes  -  delete  selected  group  of  templates', 0) = 5 then
      EXIT;
  end;

  keep_count := keeps_list.Count;

  n := 0;
  while n < keeps_list.Count do begin

    if Ttemplate(keeps_list.Objects[n]).group_selected = all_except then begin
      Inc(n);
      CONTINUE;  // leave this one.
    end;

    list_position := n;
    delete_keep(False, False);   // delete this one

    save_done := False;

  end;//while              // no need to increment n, it is now pointing to the next keep.

  if all_except = False then
    unlink_group;

  if (keep_count <> keeps_list.Count) and (keeps_list.Count > 0) then
    backup_wanted := True;

  current_state(0);

  if keeps_list.Count < 1 then begin
    ShowMessage('Your storage box is now empty.');
    EXIT;
  end;

  if keep_count <> keeps_list.Count then begin
    ShowMessage(IntToStr(keep_count - keeps_list.Count) + ' group templates were deleted.');
    EXIT;
  end;

  ShowMessage('There are no templates selected as a group.');     // shouldn't get here.
end;
//____________________________________________________________________________________

procedure Tkeep_form.delete_group_menu_entryClick(Sender: TObject);

begin
  delete_group(False);
end;
//_____________________________________________________________________________________

procedure Tkeep_form.delete_all_except_group_menu_entryClick(Sender: TObject);

begin
  delete_group(True);
end;
//_____________________________________________________________________________________

procedure Tkeep_form.box_quit_menu_entryClick(Sender: TObject);

begin
  control_room_form.quit_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure normalize_templates;        // update box contents to latest file format.

var
  save_current: Ttemplate_info;
  save_current_memo_str: string;
  n, Count: integer;
  bgnd: integer;
  save_bgnd_option: boolean;

  save_label_x, save_label_y: double;   // 0.82.d

begin
  if keeps_list.Count < 1 then
    EXIT;

  if alert(7, '    normalize  all  templates',
    'The normalize function is used to bring templates which have been loaded from earlier versions of Templot0 into line with the current version.' + '||There is no effect on templates which were created by the current version, other than a rebuild of the background drawing using the current generator settings.' + '||When templates are loaded from earlier versions, any essential changes are automatically made, but the original template specification remains otherwise unchanged.' + ' This function re-creates the template specifications from scratch, so that they correspond in every respect to all other templates created by the current version.' + '||This process may take some time to complete, after which you will probably want to save the templates in a new data file.', '', '', '', '', 'cancel', 'continue   -   normalize  all  templates', 0) = 5 then
    EXIT;

  keep_form.Close;             // so the store current button works without alerts.

  pad_form.Show;               // so he can see what's happening.
  pad_form.BringToFront;

  save_bgnd_option := pad_form.show_bgnd_keeps_menu_entry.Checked;
  // don't want a complete redraw for every template.
  pad_form.hide_bgnd_keeps_menu_entry.Checked := True;
  // so switch bgnd off radio item.

  if Application.Terminated = False then
    Application.ProcessMessages;

  Count := keeps_list.Count;

  save_current.keep_shove_list := Tshoved_timber_list.Create;
  fill_kd(save_current);
  save_current_memo_str := current_memo_str;

  try
    n := 0;

    while n < Count do begin

      with Ttemplate(keeps_list.Objects[0]).template_info.keep_dims.box_dims1 do
      begin    // always top of list
        bgnd := bgnd_code_077;
        // remember if it's on bgnd.

        // 0.82.d  save label position...

        save_label_x := mod_text_x;
        save_label_y := mod_text_y;

      end;//with

      list_position := 0;
      copy_keep_to_current(False, False, False, False);   // copy to pad.
      delete_keep(False, False);                        // and delete from box.

      store_unused((bgnd = -1), False);     // (does a recalc) put back in.

      if Count <> keeps_list.Count then begin
        alert(5, '    normalizing  error',
          'An error occurred while normalizing template number  ' +
          IntToStr(n + 1) + ' .' +
          '||It is not possible to normalize this template. It has been deleted from your storage box.'
          + '||Normalizing will be aborted. You can reload the original templates from the file again, or restart the normalizing process on the remaining templates.',
          '', '', '', '', 'cancel', '', 0);
        EXIT;
      end;

      with Ttemplate(keeps_list.Objects[Count - 1]).template_info.keep_dims.box_dims1 do
      begin    // always bottom of list

        // 0.82.d  restore label position...

        mod_text_x := save_label_x;
        mod_text_y := save_label_y;

      end;//with

      if bgnd = 1 then
        copy_or_wipe_background;    // and on background.

      Inc(n);
    end;//while

    keep_form.reset_all_id_numbers_menu_entry.Click;   // 208a

    pad_form.show_bgnd_keeps_menu_entry.Checked := save_bgnd_option;   // restore, radio item.
    do_rollback := False;
    redraw(False);              // show background again before the save dialog.

    save_box(0, 0, 0, '');   // don't need result flag.

  finally
    copy_keep(save_current);                                         // restore control template.
    pad_form.show_bgnd_keeps_menu_entry.Checked := save_bgnd_option;   // restore, radio item.
    current_memo_str := save_current_memo_str;

    save_current.keep_shove_list.Free;
    do_rollback := False;
    redraw(True);
  end;//try
end;
//________________________________________________________________________________________


procedure Tkeep_form.group_linked_warning_labelClick(Sender: TObject);

begin
  unlink_group;
  group_linked_warning_label.Hide;
end;
//_______________________________________________________________________________________

procedure Tkeep_form.undo_clear_menu_entryClick(Sender: TObject);

var
  append: boolean;
  i, hl: integer;
  clear_str: string;

begin
  if FileExists(Config.GetFilePath(csfiSaveForUndo)) or
    FileExists(Config.GetFilePath(csfiSaveForUndoZ))
  // original or a preserved copy.
  then begin
    if no_undo_clear_msg_pref = False then begin

      alert_box.preferences_checkbox.Checked := False;       //%%%%
      alert_box.preferences_checkbox.Show;

      i := alert(7, '    undo  last  clear  or  reload',
        'You are about to undo the most recent CLEAR or RELOAD and restore the previous contents to your storage box and background drawing.' + '||You can toggle back and forth between the previous contents and the present contents by clicking UNDO CLEAR / RELOAD repeatedly.', '', '', '', '', 'cancel  undo', 'O K', 0);


      no_undo_clear_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
      alert_box.preferences_checkbox.Hide;

      if i = 5 then
        EXIT;
    end;

    if FileExists(Config.GetFilePath(csfiSaveForUndo)) // original undo file exists
    then begin
      DeleteFile(Config.GetFilePath(csfiSaveForUndoZ));
      // delete any preserved copy of the previous undo file.
      RenameFile(Config.GetFilePath(csfiSaveForUndo), Config.GetFilePath(csfiSaveForUndoZ));
      // preserve the undo file by renaming.
    end;

    if FileExists(Config.GetFilePath(csfiSaveForUndoZ))
    // preserved copy (a new undo file is created on reload).
    then begin
      append := False;
      // var parameter.
      if not load_storage_box(True, False, Config.GetFilePath(csfiSaveForUndoZ),
        False, False, append, hl) then
        EXIT;     // nothing was loaded.

      if list_panel.Visible = True then
        show_list_button.Click;      // update the list.
      //if (append=False) and (hl>-1) and (hl<keeps_list.Count) then mint_final(hl);    // if something loaded mint from highest bgnd if he so wants.
      if append = True then
        EXIT;
      if (loaded_version < 93) and (hl > -1) and (hl < keeps_list.Count) then
        mint_final_or_copy_control(hl);
      // if something loaded mint from highest bgnd if he so wants.
      if (loaded_version > 92) then
        mint_final_or_copy_control(hl);
      // copy the control template if there is one in the file.
    end;
  end
  else begin
    if keeps_list.Count > 0 then
      clear_str := 'clear  all  existing  templates'
    else
      clear_str := '';

    if alert(2, '    nothing  to  undo',
      'No templates have previously been cleared or replaced by reloading, so there is nothing to undo and no data to restore.',
      '', '', '', clear_str, '', 'O K  -  continue', 0) = 4 then
      clear_menu_entry.Click;
  end;
end;
//___________________________________________________________________________________________

procedure Tkeep_form.rebuild_template_menu_entryClick(Sender: TObject);

begin
  if (keeps_list.Count < 1) or (list_position < 0) or (list_position > (keeps_list.Count - 1)) then
    EXIT;

  if Ttemplate(keeps_list.Objects[list_position]).template_info.keep_dims.box_dims1.bgnd_code_077
    <> 1 then
    EXIT;

  Screen.Cursor := crHourGlass;     // might take a while.
  try
    rebuild_it(list_position, 0, 0, False);    // rebuild the background template (ignore result).
  finally
    Screen.Cursor := crDefault;
  end;//try
end;
//__________________________________________________________________________________________

procedure Tkeep_form.rebuild_all_menu_entryClick(Sender: TObject);

begin
  if any_bgnd = 0 then begin
    alert(6, '    no  templates  currently  on  background',
      '||There are no stored templates currently copied to the background drawing, and therefore none to be rebuilt.'
      +
      '||To copy a template to the background, select it and then click the `0COPY TO BACKGROUND`1 button.',
      '', '', '', '', '', 'O K', 0);
    EXIT;
  end;

  rebuild_background(True, True);
end;
//__________________________________________________________________________________________

procedure Tkeep_form.make_library_template_menu_entryClick(Sender: TObject);

begin
  if (keeps_list.Count < 1) or (list_position < 0) or (list_position > (keeps_list.Count - 1)) then
    EXIT;

  with Ttemplate(keeps_list.Objects[list_position]).template_info.keep_dims.box_dims1 do begin

    if bgnd_code_077 <> 0 then
      EXIT                // already library or on bgnd???  menu should be disabled.
    else
      bgnd_code_077 := -1;  // library template.
  end;//with
  current_state(-1);
end;
//______________________________________________________________________________________

procedure Tkeep_form.obtain_switch_menu_entryClick(Sender: TObject);

begin
  obtain_switch(list_position);
end;
//_________________________________________________________________________________________

procedure Tkeep_form.obtain_plain_track_menu_entryClick(Sender: TObject);

begin
  obtain_plain_track(list_position);
end;
//__________________________________________________________________________________________

procedure Tkeep_form.recent_1_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[8] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[8]);
end;
//________________________

procedure Tkeep_form.recent_2_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[7] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[7]);
end;
//________________________

procedure Tkeep_form.recent_3_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[6] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[6]);
end;
//________________________

procedure Tkeep_form.recent_4_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[5] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[5]);
end;
//________________________

procedure Tkeep_form.recent_5_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[4] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[4]);
end;
//________________________

procedure Tkeep_form.recent_6_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[3] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[3]);
end;
//________________________

procedure Tkeep_form.recent_7_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[2] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[2]);
end;
//________________________

procedure Tkeep_form.recent_8_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[1] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[1]);
end;
//________________________

procedure Tkeep_form.recent_9_popup_entryClick(Sender: TObject);

begin
  if boxmru_list.Strings[0] <> '' then
    reload_specified_file(False, add_mru, boxmru_list.Strings[0]);
end;
//_____________________________________________________________________________________

procedure reload_recent;  // 0.82.a  set up mru menu

var
  i: integer; // dummy

  ///////////////////////////////////////

  procedure do_item(n: integer; popup_item: TMenuItem);

  var
    num_str: string;

  begin
    num_str := IntToStr(9 - n);    // number showing in list.

    with boxmru_list do begin

      if Strings[n] <> '' then begin
        popup_item.Caption :=
          num_str + '      ' + ExtractFileName(Strings[n]) + '      •      ' +
          ExtractFilePath(Strings[n]);
        popup_item.Enabled := True;
        keep_form.clear_mru_list_popup_entry.Enabled := True;
      end
      else begin
        popup_item.Caption := num_str + '      ( empty )';
        popup_item.Enabled := False;
      end;
    end;//with
  end;
  /////////////////////////////////////////

begin
  if boxmru_list.Count <> 9 then
    EXIT;  // ??? error

  // menu is in reverse order to list, list has latest at bottom.

  do_item(8, keep_form.recent_1_popup_entry);
  do_item(7, keep_form.recent_2_popup_entry);
  do_item(6, keep_form.recent_3_popup_entry);
  do_item(5, keep_form.recent_4_popup_entry);
  do_item(4, keep_form.recent_5_popup_entry);
  do_item(3, keep_form.recent_6_popup_entry);
  do_item(2, keep_form.recent_7_popup_entry);
  do_item(1, keep_form.recent_8_popup_entry);
  do_item(0, keep_form.recent_9_popup_entry);

  keep_form.recent_popup_menu.PopUp(pad_form.Left + 350, pad_form.Top + 250);
  // 0.82.a  350, 250 arbitrary.

end;
//________________________________________

procedure Tkeep_form.reload_recent_file_menu_entryClick(Sender: TObject);     // 0.82.a

begin
  mru_caption_popup_entry.Caption :=
    '#                    load  or  reload  templates  from  a  recent  box  file :';
  reload_add_mru_popup_entry.Caption := 'load  or  reload  from ...';   // 0.91.d
  //cancel_mru_popup_entry.Caption:='cancel';


  restore_previous_mru_popup_entry.Enabled := True;
  restore_prior_previous_mru_popup_entry.Enabled := True;

  add_mru := False;
  reload_recent;
end;
//__________________________________________

procedure Tkeep_form.add_recent_file_menu_entryClick(Sender: TObject);        // 0.82.a

begin
  mru_caption_popup_entry.Caption :=
    '#                        add  templates  from  a  recent  box  file :';
  reload_add_mru_popup_entry.Caption := 'add  from ...';    // 0.91.d

  restore_previous_mru_popup_entry.Enabled := False;
  restore_prior_previous_mru_popup_entry.Enabled := False;

  add_mru := True;
  reload_recent;
end;
//______________________________________________________________________________

procedure Tkeep_form.clear_mru_list_popup_entryClick(Sender: TObject);

begin
  empty_boxmru;
end;
//______________________________________________________________________________

procedure Tkeep_form.reload_add_mru_popup_entryClick(Sender: TObject);

// 0.91.d

begin
  if add_mru = True then
    add_file_menu_entry.Click
  else
    reload_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tkeep_form.FormHide(Sender: TObject);

begin
  info_clicked := False;    // don't change the read texts if the box is not open.
end;
//______________________________________________________________________________

procedure box_goto_smallest_bgnd_rad;    // 208a

begin
  if (smallest_bgnd_radius_index >= 0) and (smallest_bgnd_radius_index < keeps_list.Count) then
  begin
    help_form.continue_button.Click;
    keep_form.Show;

    Application.ProcessMessages;

    list_position := smallest_bgnd_radius_index;
    current_state(-1);
  end;
end;
//______________________________________________________________________________

procedure box_goto_smallest_group_rad;    // 208a

begin
  if (smallest_group_radius_index >= 0) and (smallest_group_radius_index < keeps_list.Count) then
  begin
    help_form.continue_button.Click;
    keep_form.Show;

    Application.ProcessMessages;

    list_position := smallest_group_radius_index;
    current_state(-1);
  end;
end;
//______________________________________________________________________________

procedure Tkeep_form.box_info_menu_entryClick(Sender: TObject);    // 0.93.a

label
  123;

var
  msg_str: string;

begin
  current_state(-1);

  msg_str := '<P STYLE="COLOR:#CC0000; FONT-SIZE:13px; FONT-WEIGHT:BOLD;">If templates have been loaded from earlier versions of Templot, some data may be missing -- click the [normalize templates] button below.</P>';

  123:

    msg_str := msg_str + '<P ALIGN="CENTER" STYLE="COLOR:BLUE;">Storage Box Data:</P><HR>' +
      '<P><TABLE STYLE="FONT-SIZE:15px; FONT-WEIGHT:BOLD;"><TR><TD WIDTH="66%">TOTAL LENGTH: The total length of all background templates is currently:</TD>' + '<TD>&nbsp; &nbsp;</TD><TD>' + round_str(total_bgnd_template_length, 2) + ' mm<BR>(approx ' + round_str(total_bgnd_template_length / 304.8, 0) + ' ft)</TD></TR>';

  msg_str := msg_str + '<TR><TD COLSPAN="3" ALIGN="CENTER">&nbsp;</TD></TR>';

  if classic_templot = False then begin
    msg_str := msg_str +
      '<TR><TD>Including the current control template, the total length is:</TD>'
      + '<TD>&nbsp; &nbsp;</TD><TD>' + round_str(total_bgnd_template_length + turnoutx, 2) +
      ' mm<BR>(approx ' + round_str((total_bgnd_template_length + turnoutx) / 304.8, 0) +
      ' ft)</TD></TR>';

    msg_str := msg_str + '<TR><TD COLSPAN="3" ALIGN="CENTER">&nbsp;</TD></TR>';
  end;

  msg_str := msg_str + '<TR><TD>GROUP LENGTH: The total length of all group-selected templates is currently:</TD>' + '<TD>&nbsp; &nbsp;</TD><TD>' + round_str(total_group_template_length, 2) + ' mm<BR>(approx ' + round_str(total_group_template_length / 304.8, 0) + ' ft)</TD></TR>';

  msg_str := msg_str + '<TR><TD COLSPAN="3" ALIGN="CENTER"><HR></TD></TR>';


  msg_str := msg_str + '<TR><TD>TOTAL TIMBERING: The total length of timbering on all background templates is currently:</TD>' + '<TD>&nbsp; &nbsp;</TD><TD>' + round_str(total_bgnd_timbering_length, 2) + ' mm<BR>(approx ' + round_str(total_bgnd_timbering_length / 304.8, 0) + ' ft)</TD></TR>';

  msg_str := msg_str + '<TR><TD COLSPAN="3" ALIGN="CENTER">&nbsp;</TD></TR>';

  if classic_templot = False then begin
    msg_str := msg_str +
      '<TR><TD>Including the current control template, the total timbering length is:</TD>' +
      '<TD>&nbsp; &nbsp;</TD><TD>' + round_str(total_bgnd_timbering_length +
      total_template_timber_length, 2) + ' mm<BR>(approx ' + round_str(
      (total_bgnd_timbering_length + total_template_timber_length) / 304.8, 0) + ' ft)</TD></TR>';

    msg_str := msg_str + '<TR><TD COLSPAN="3" ALIGN="CENTER">&nbsp;</TD></TR>';
  end;

  msg_str := msg_str + '<TR><TD>GROUP TIMBERING: The total length of timbering on all group-selected templates is currently:</TD>' + '<TD>&nbsp; &nbsp;</TD><TD>' + round_str(total_group_timbering_length, 2) + ' mm<BR>(approx ' + round_str(total_group_timbering_length / 304.8, 0) + ' ft)</TD></TR>';


  msg_str := msg_str + '<TR><TD COLSPAN="3" ALIGN="CENTER"><HR></TD></TR>';


  msg_str := msg_str + '<TR><TD>SMALLEST RADIUS: The smallest radius on any background template is currently:</TD><TD>&nbsp; &nbsp;</TD>';

  if smallest_bgnd_radius < max_rad_test then
    msg_str := msg_str + '<TD>' + round_str(smallest_bgnd_radius, 2) +
      ' mm<BR>(approx ' + round_str(smallest_bgnd_radius / 25.4, 1) +
      ' in)<BR><A HREF="smallest_bgnd_rad.85a">show template</A></TD></TR>'
  else
    msg_str := msg_str + '<TD>straight</TD></TR>';

  msg_str := msg_str + '<TR><TD COLSPAN="3" ALIGN="CENTER">&nbsp;</TD></TR>';

  msg_str := msg_str + '<TR><TD>SMALLEST RADIUS IN GROUP: The smallest radius on any group-selected template is currently:</TD><TD>&nbsp; &nbsp;</TD>';

  if smallest_group_radius < max_rad_test then
    msg_str := msg_str + '<TD>' + round_str(smallest_group_radius, 2) +
      ' mm<BR>(approx ' + round_str(smallest_group_radius / 25.4, 1) +
      ' in)<BR><A HREF="smallest_group_rad.85a">show template</A></TD></TR>'
  else
    msg_str := msg_str + '<TD>straight</TD></TR>';


  msg_str := msg_str + '</TABLE></P>';


  if help(-350, msg_str, 'normalize  templates') = 1 then begin
    normalize_templates;
    msg_str := '';
    goto 123;
  end;
end;
//______________________________________________________________________________

procedure Tkeep_form.normalize_templates_menu_entryClick(Sender: TObject);

begin
  normalize_templates;
end;
//______________________________________________________________________________

procedure Tkeep_form.briefly_hide_on_store_menu_entryClick(Sender: TObject);  // 205c

begin
  briefly_hide_on_store_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.hide_on_store_menu_entryClick(Sender: TObject);

begin
  hide_on_store_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.dont_hide_on_store_menu_entryClick(Sender: TObject);

begin
  dont_hide_on_store_menu_entry.Checked := True;  // radio item
end;
//______________________________________________________________________________

procedure Tkeep_form.reveal_on_store_menu_entryClick(Sender: TObject);

begin
  reveal_on_store_menu_entry.Checked := True;  // radio item  206a
end;
//______________________________________________________________________________

procedure Tkeep_form.alert_on_store_menu_entryClick(Sender: TObject);

begin
  alert_on_store_menu_entry.Checked := True;  // radio item  206c
end;
//______________________________________________________________________________

function return_prefix_tags_list(n: integer; var tags_list: TStringList): integer;
  // original not modified   // 206b

  //  return with tags_list containing all the template's tags (no square brackets), and number of tags

var
  tagged_str: string;
  i, j: integer;

begin
  Result := 0;  // init

  if keeps_list.Count < 1 then
    EXIT;

  if (n < 0) or (n >= keeps_list.Count) then
    EXIT;

  tagged_str := Trim(Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.reference_string);

  repeat
    i := Pos('[', tagged_str);
    j := Pos(']', tagged_str);

    if (i <> 1) or (j < 3) then
      BREAK;  // no more tags or none, or at least, none properly formatted

    tags_list.Add(Copy(tagged_str, 2, j - 2));   // add tag to list

    Delete(tagged_str, 1, j);                 // and then remove it

    tagged_str := Trim(tagged_str);           // any separating spaces

    Result := Result + 1;

    Application.ProcessMessages;

  until 0 <> 0;

end;
//______________________________________________________________________________

function build_tag_list(group_tags_only: boolean): integer;   // 206b

var
  new_tags_list: TStringList;
  i, n: integer;

begin
  Result := 0;  // init

  if keeps_list.Count < 1 then
    EXIT;

  tag_list.Clear;  // old global tag list

  tag_list.Sorted := True;     // add tags in alphabetical order

  tag_list.Duplicates := dupIgnore;   // and ignore duplicates

  new_tags_list := TStringList.Create;

  for n := 0 to keeps_list.Count - 1 do begin

    if (group_tags_only = True) and (Ttemplate(keeps_list.Objects[n]).group_selected = False) then
      CONTINUE;

    if return_prefix_tags_list(n, new_tags_list) > 0  // get any tags for this one in new_tag_list
    then begin
      if new_tags_list.Count > 0 then
        for i := 0 to new_tags_list.Count - 1 do
          tag_list.Add(new_tags_list.Strings[i]); // and add them to the global list
    end;

    new_tags_list.Clear;   // for next one

  end;//next template

  new_tags_list.Free;

  Result := tag_list.Count;
end;
//______________________________________________________________________________

procedure Tkeep_form.show_tags_list_menu_entryClick(Sender: TObject);    // 206b

var
  tag_count: integer;

begin
  tag_count := build_tag_list(False);

  ShowMessage('There are ' + IntToStr(tag_count) + ' prefix tags in use:' +
    #13 + #13 + tag_list.Text);
end;
//______________________________________________________________________________

procedure create_group_from_tag(tag_str: string; no_adding_alert: boolean);   // 206b

// enter with [tag] in square brackets

var
  i, n: integer;
  add_to_existing_group: boolean;

begin
  if keeps_list.Count < 1 then
    EXIT;

  if no_adding_alert = True then
    add_to_existing_group := True   // add without alert if necessary
  else begin
    if any_selected > 0 then begin
      i := alert(4, 'php/330    add  templates  to  existing  group ?',
        '`0Select all templates tagged  ' + tag_str + '  as a group.`9' +
        '||There is an existing group of one or more selected templates.' +
        '||Do you want to create a new group, or add the tagged templates to the existing group?',

        '', '', '', 'add  to  existing  group', 'cancel', 'create  new  group', 0);

      if i = 5 then
        EXIT;

      add_to_existing_group := (i = 4);
    end
    else
      add_to_existing_group := False;
  end;

  if add_to_existing_group = False then
    unlink_group;

  for n := 0 to keeps_list.Count - 1 do begin

    if (add_to_existing_group = True) and
      (Ttemplate(keeps_list.Objects[n]).group_selected = True) then
      CONTINUE;

    with Ttemplate(keeps_list.Objects[n]) do
      group_selected := (Pos(tag_str, template_info.keep_dims.box_dims1.reference_string) <> 0);

  end;//next template
end;
//______________________________________________________________________________

function highest_id_number: integer;  // 208a    return highest ID number in box.

var
  n, id: integer;

begin
  Result := 0;    // init

  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to keeps_list.Count - 1 do begin

    id := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.id_number;

    if Result < id then
      Result := id;

  end;//next

  // don't use a deleted ID in case it is subsequently undeleted...

  if keep_form.undo_delete_menu_entry.Enabled = True    // 208b
  then begin
    if Result < deleted_keep.keep_dims.box_dims1.id_number then
      Result := deleted_keep.keep_dims.box_dims1.id_number;
  end;
end;
//______________________________________________________________________________

procedure Tkeep_form.on_store_help_menu_entryClick(Sender: TObject);

begin
  help(0, get_store_beginner_help, '');  // 208a
end;
//______________________________________________________________________________

procedure sort_box(code: integer);

// 208a temp swap the template name (reference_string) to the list strings, so can sort it.

// code  1=template name, 2=ID number, 3=template type, 4=info

var
  n: integer;

begin
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin

    Ttemplate(keeps_list.Objects[n]).sort_swap_info_str := keeps_list.Strings[n];
    Ttemplate(keeps_list.Objects[n]).sort_swap_memo_str := memo_list.Strings[n];

    case code of

      1:
        keeps_list.Strings[n] :=
          LowerCase(Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.reference_string);

      2:
        keeps_list.Strings[n] :=
          FormatFloat('000000', Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.id_number);

      3:
        keeps_list.Strings[n] := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.id_number_str;

      4:
        keeps_list.Strings[n] := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.top_label;

    end;//case

  end;//next

  keeps_list.Sort;

  // swap back

  for n := 0 to (keeps_list.Count - 1) do begin

    keeps_list.Strings[n] := Ttemplate(keeps_list.Objects[n]).sort_swap_info_str;
    memo_list.Strings[n] := Ttemplate(keeps_list.Objects[n]).sort_swap_memo_str;

  end;//next

  save_done := False;
  backup_wanted := True;

  current_state(0);    // set to highest
end;
//______________________________________________________________________________

procedure Tkeep_form.sort_by_name_menu_entryClick(Sender: TObject);     // 208a

begin
  sort_box(1);
end;
//______________________________________________________________________________

procedure Tkeep_form.sort_by_id_menu_entryClick(Sender: TObject);

begin
  sort_box(2);
end;
//______________________________________________________________________________

procedure Tkeep_form.sort_by_type_menu_entryClick(Sender: TObject);

begin
  sort_box(3);
end;
//______________________________________________________________________________

procedure Tkeep_form.sort_by_info_menu_entryClick(Sender: TObject);

begin
  sort_box(4);
end;
//______________________________________________________________________________

function any_stored_rails_omitted(n: integer): boolean;     // 208a

var
  pt_all, turnout_all, hd_all: boolean;

begin
  Result := False;  // init

  with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims do begin

    with box_dims1.rail_info do begin

      pt_all := turnout_road_stock_rail_sw and main_road_stock_rail_sw;

      turnout_all := pt_all and turnout_road_check_rail_sw and
        turnout_road_crossing_rail_sw and crossing_vee_sw and main_road_check_rail_sw and
        main_road_crossing_rail_sw;

      hd_all := turnout_all and k_diagonal_side_check_rail_sw and k_main_side_check_rail_sw;

    end;//with

    if box_dims1.turnout_info1.plain_track_flag = True   // plain track template
    then
      Result := not pt_all
    else
    if turnout_info2.semi_diamond_flag = True    // half-diamond template
    then
      Result := not hd_all
    else
      Result := not turnout_all;             // turnout template

  end;//with
end;
//______________________________________________________________________________

function any_deleted_keep_rails_omitted: boolean;     // 208b

var
  pt_all, turnout_all, hd_all: boolean;

begin
  Result := False;  // init

  with deleted_keep.keep_dims do begin

    with box_dims1.rail_info do begin

      pt_all := turnout_road_stock_rail_sw and main_road_stock_rail_sw;

      turnout_all := pt_all and turnout_road_check_rail_sw and
        turnout_road_crossing_rail_sw and crossing_vee_sw and main_road_check_rail_sw and
        main_road_crossing_rail_sw;

      hd_all := turnout_all and k_diagonal_side_check_rail_sw and k_main_side_check_rail_sw;

    end;//with

    if box_dims1.turnout_info1.plain_track_flag = True   // plain track template
    then
      Result := not pt_all
    else
    if turnout_info2.semi_diamond_flag = True    // half-diamond template
    then
      Result := not hd_all
    else
      Result := not turnout_all;             // turnout template

  end;//with
end;
//______________________________________________________________________________

procedure Tkeep_form.reset_all_id_numbers_menu_entryClick(Sender: TObject);  // 208a

var
  n: integer;
  any_reset: boolean;

begin
  any_reset := False;  // init

  if keeps_list.Count > 0 then begin

    any_reset := True;

    for n := 0 to (keeps_list.Count - 1) do begin

      with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims do begin

        box_dims1.id_number := n + 1;


        box_dims1.id_number_str :=
          create_id_number_str(box_dims1.id_number, box_dims1.turnout_info1.hand,
          turnout_info2.start_draw_x, box_dims1.turnout_info1.turnout_length,
          turnout_info2.ipx_stored, turnout_info2.fpx_stored,
          box_dims1.turnout_info1.plain_track_flag,
          turnout_info2.semi_diamond_flag, any_stored_rails_omitted(n));

      end;//with

    end;//next

  end;

  if keep_form.undo_delete_menu_entry.Enabled = True
  // 208b   reset deleted keep, if any, to next ID number
  then begin

    any_reset := True;

    with deleted_keep.keep_dims do begin

      box_dims1.id_number := keeps_list.Count + 1;   // next ID

      box_dims1.id_number_str :=
        create_id_number_str(box_dims1.id_number, box_dims1.turnout_info1.hand,
        turnout_info2.start_draw_x, box_dims1.turnout_info1.turnout_length,
        turnout_info2.ipx_stored, turnout_info2.fpx_stored,
        box_dims1.turnout_info1.plain_track_flag, turnout_info2.semi_diamond_flag,
        any_deleted_keep_rails_omitted);

    end;//with
  end;

  if any_reset = True then begin
    save_done := False;
    backup_wanted := True;
  end;

  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.file_viewer_menu_entryClick(Sender: TObject);

begin
  { OT-FIRST
  keep_form_was_showing:=True;
  do_show_modal(file_viewer_form);    // 212a ShowModal;
  }
end;
//______________________________________________________________________________

function get_readme_notes: string;    // 208d

var
  i, n: integer;
  name_str: string;

begin
  Result := ''; // init

  if keeps_list.Count < 1 then
    EXIT;
  if keeps_list.Count <> memo_list.Count then begin
    run_error(220);
    EXIT;
  end;

  for n := 0 to keeps_list.Count - 1 do begin

    name_str := Trim(LowerCase(Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.reference_string));

    while Pos(']', name_str) > 0 do
      Delete(name_str, 1, 1);    // remove any tags

    name_str := Trim(name_str);

    if (name_str = 'read me') or (name_str = 'readme') or (name_str = 'read-me') or
      (name_str = 'read_me') then begin
      Result := memo_list.Strings[n];
      BREAK;
    end;

  end;//next
end;
//______________________________________________________________________________

function lowest_non_hd_template: integer;
  // 215a return lowest index of a half-diamond template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := 0 to (keeps_list.Count - 1) do begin
    if Ttemplate(keeps_list.Objects[n]).bgnd_half_diamond = False then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//___________________________________________________________________________________________

function highest_hd_template: integer;
  // 215a return highest index of a half-diamond template.
  // return -1 if none.
var
  n: integer;

begin
  Result := -1;                        // init default.
  if keeps_list.Count < 1 then
    EXIT;

  for n := (keeps_list.Count - 1) downto 0 do begin
    if Ttemplate(keeps_list.Objects[n]).bgnd_half_diamond = True then begin
      Result := n;   // return index.
      EXIT;
    end;
  end;//next template
end;
//___________________________________________________________________________________________

procedure Tkeep_form.sort_half_diamonds_to_first_menu_entryClick(Sender: TObject);       // 215a

var
  n, l_nhd, h_hd: integer;

begin
  if highest_hd_template < 0 then begin
    ShowMessage('There are no half-diamond templates in the storage box.');
    EXIT;
  end;

  // sort any half_diamond templates to the start of the list.

  repeat
    l_nhd := lowest_non_hd_template;      // lowest index
    h_hd := highest_hd_template;          // highest index

    if (l_nhd <> -1) and (h_hd <> -1) and (l_nhd < h_hd) then begin
      keeps_list.Move(h_hd, 0);     // move it to start
      memo_list.Move(h_hd, 0);      // and any memo
    end
    else
      BREAK;
  until 0 <> 0;

  save_done := False;
  backup_wanted := True;

  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.reminder_message_menu_entryClick(Sender: TObject);    // 216a

begin
  if (list_position < 0) or (list_position > (keeps_list.Count - 1)) or (keeps_list.Count < 1) then
    EXIT;

  edit_reminder_menu_entry.Enabled :=
    Ttemplate(keeps_list.Objects[list_position]).template_info.keep_dims.box_dims1.align_info.reminder_flag;
  remove_reminder_menu_entry.Enabled :=
    Ttemplate(keeps_list.Objects[list_position]).template_info.keep_dims.box_dims1.align_info.reminder_flag;

  add_reminder_menu_entry.Enabled :=
    not Ttemplate(keeps_list.Objects[list_position]).template_info.keep_dims.box_dims1.align_info.reminder_flag;

end;
//______________________________________________________________________________

procedure Tkeep_form.add_reminder_menu_entryClick(Sender: TObject);

begin
  add_reminder_click(list_position);     // 216a       in pad_unit
  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.edit_reminder_menu_entryClick(Sender: TObject);

begin
  edit_reminder_click(list_position);     // 216a       in pad_unit
  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.reminder_colour_menu_entryClick(Sender: TObject);

begin
  reminder_colour_click(list_position);     // 216a       in pad_unit
  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.remove_reminder_menu_entryClick(Sender: TObject);

begin
  remove_reminder_click(list_position);     // 216a       in pad_unit
  current_state(-1);
end;
//______________________________________________________________________________

procedure Tkeep_form.import_mecbox_menu_entryClick(Sender: TObject);

begin
  import_mecbox('');    // in mecbox_unit
end;

procedure Tkeep_form.keep_html_viewHotSpotClick(Sender: TObject; const SRC: ThtString;
  var Handled: Boolean);
begin
  htmlviewer_hot_spot_clicked(Sender, SRC, Handled);
end;

//________________________________________________________________________________________


procedure Tkeep_form.export_mecbox_menu_entryClick(Sender: TObject);

begin
  export_mecbox(True, '');     // True=show export result
end;
//______________________________________________________________________________

end.
