"""
	This include file contains two lists of modules:
	- those which were originally inherited	from the Templot2 sources released by Martin Wynne.
	    These need to include the original copyright notice.
	- those which came from other sources and are not changed by OpenTemplot
	    These should not be written to at all

	These lists are used to ensure that appropriate copyright notices can be inserted into
	all source files.

	It is included by addLicense.py
"""

# pascal source files at the top level which are not to be changed when updating license comment
leaveAloneUnits = [
    "fresnel_unit.pas"
]

# Entire directories of pascal source files which are not to be changed when updating license comment
leaveAloneDirs = [
    "html_viewer_units",
    "log4delphi",
    "synapse_units",
    "xml_units"]

# Pascal source files which are to be updated but which need the T2 copyright notice
T2Units = [
    "opentemplot.lpr",

    "action_unit.pas",
    "alert_unit.pas",
    "background_shapes.pas",
    "bgkeeps_unit.pas",
    "bgnd_unit.pas",
    "box_file_unit.pas",
    "box3_file_unit.pas",
    "calibration_unit.pas",
    "chat_unit.pas",
    "check_diffs_unit.pas",
    "colour_unit.pas",
    "control_room.pas",
    "create_tandem.pas",
    "data_memo_unit.pas",
    "dummy_vehicle.pas",
    "dxf_unit.pas",
    "edit_memo_unit.pas",
    "enter_timber.pas",
    "entry_sheet.pas",
    "export_draw_unit.pas",
    "export_unit.pas",
    "file_viewer.pas",
    "gauge_unit.pas",
    "grid_unit.pas",
    "help_sheet.pas",
    "image_viewer_unit.pas",
    "info_unit.pas",
    "jotter_unit.pas",
    "keep_select.pas",
    "make_slip_unit.pas",
    "map_loader_unit.pas",
    "mark_unit.pas",
    "math_unit.pas",
    "math2_unit.pas",
    "mecbox_unit.pas",
    "metric_unit.pas",
    "mint_unit.pas",
    "mouse_colour_unit.pas",
    "pad_unit.pas",
    "panning_unit.pas",
    "pdf_unit.pas",
    "plain_track_unit.pas",
    "platform_unit.pas",
    "prefs_unit.pas",
    "preview_unit.pas",
    "print_now_box.pas",
    "print_settings_unit_old.pas",
    "print_settings_unit.pas",
    "print_unit.pas",
    "rail_data_unit.pas",
    "rail_options_unit.pas",
    "shove_timber.pas",
    "shoved_timber.pas",
    "startup_unit.pas",
    "stay_visible_unit.pas",
    "switch_select.pas",
    "template.pas",
    "trackbed_unit.pas",
    "wait_message.pas",
    "web_capture_unit.pas",
    "web_map_help_unit.pas",
    "xing_select.pas",
    "xtc_unit.pas",
]
