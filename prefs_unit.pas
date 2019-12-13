
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


unit prefs_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Clipbrd,
  StdCtrls, FileCtrl;

type
  Tprefs_form = class(TForm)
    file_list_box: TFileListBox;
    ok_button: TButton;
    cancel_button: TButton;
    delete_button: TButton;
    hidden_label: TLabel;
    datestamp_label: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure delete_buttonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure hidden_labelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  prefs_pointer_str:string='goprefs.txt';   // pointer file to current prefs file name.

var
  prefs_form: Tprefs_form;

  loading_his_prefs:boolean=False;  // 208a

  procedure show_prefs_dialog(help_showing:boolean);

  procedure load_prefs(prefs_file_str:string; get_file,show_them,startup_msg_prefs:boolean);
  procedure save_prefs(prefs_file_str:string; confirm_it:boolean);

  procedure change_prefs_file_and_load(load_file:boolean);
  procedure new_prefs_file_and_save;

  procedure abandon_prefs(show_dialog:boolean);
  procedure prefs_include_gen;
  procedure prefs_exclude_gen;

  function prefs_quit:boolean;

  function get_prefs_file_name:string;

  function prepare_prefs_html_str:string;

  procedure save_custom_gauges;
  procedure get_custom_gauges;

//______________________________________________________________________________

implementation

uses
  control_room,pad_unit,alert_unit,bgnd_unit,math_unit,print_settings_unit,help_sheet,keep_select,

  { OT-FIRST dtpFreehandShape,dtp_settings_unit,dtp_unit,} gauge_unit,colour_unit,preview_unit,

  bgkeeps_unit, startup_unit, action_unit, make_slip_unit;

{$R *.lfm}

//______________________________________________________________________________

  //alert_box.preferences_checkbox

const
  str_prefix:string='| ';

var
  prefs_font:TFont;


//______________________________________________________________________________

function prepare_prefs_html_str:string;

const
  html_header_str:string='<P CLASS="heading">Saved &nbsp;Program &nbsp;Preferences'
  +'<BR>&nbsp; &nbsp; &nbsp; &nbsp;  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;  &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;  &nbsp; &nbsp; &nbsp;'
  +'<SPAN CLASS="alert">please scroll this page to read all of it&nbsp; &nbsp; &nbsp; click the <SPAN STYLE="color:blue;">blue links</SPAN> to make your settings</SPAN></P>'

  +'<HR NOSHADE>'

  +'<P STYLE="COLOR:#773300; FONT-STYLE:ITALIC; line-height:140%;">'
  +'This function is intended for experienced Templot users only. There is a risk of losing some program functionality, making it impossible to follow the videos and tutorials.'
  +' To cancel saving preferences, click <A HREF="abandon_prefs.85a">abandon preferences</A> , and then re-start Templot.</P>'

  +'<P><TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="12" CELLPADDING="0" STYLE="line-height:140%;">'   // outer table

  +'<TR><TD WIDTH="40%" VALIGN="TOP" STYLE="BACKGROUND-COLOR:#DDFFFF;">';    // left column

  html_left1_str:string='<TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="0" CELLPADDING="12">'
  +'<TR><TD>You have no saved program preferences.</TD></TR>'
  +'<TR><TD>Click <A HREF="init_prefs.85a">save preferences</A> to save your current program preferences. You will have an option to reload them when you next start a Templot session.</TD></TR>'
  +'</TABLE>';

  html_left2_str:string='<TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="0" CELLPADDING="12">'
  +'<TR><TD>Your program preferences are currently being saved to the file named: ~~~1</TD></TR>'
  +'<TR><TD>Click <A HREF="load_prefs.85a">reload preferences</A> to reload your saved preferences from that file now.</TD></TR>'
  +'<TR><TD>Click <A HREF="change_prefs_file_and_load.85a">change file and reload</A> to choose a different existing file to be used instead, and reload the preferences from it now.</TD></TR>'

  +'<TR><TD>&nbsp;<BR>Click <A HREF="save_prefs.85a">save preferences</A> to update your preferences in the current preferences file now.</TD></TR>'
  +'<TR><TD>Click <A HREF="new_prefs_file_and_save.85a">new file and save</A> to create a new preferences file to be used instead, and save your current preferences to it now.</TD></TR>'

  +'<TR><TD>&nbsp;<BR>Click <A HREF="change_prefs_file_only.85a">change file only</A> to choose a different existing file to be used instead when preferences are next reloaded or saved.</TD></TR>'

  +'<TR><TD>&nbsp;<BR>Click <A HREF="prefs_include_gen.85a">include generator settings</A> to include the generator settings in your saved preferences the next time they are saved.'
  +' Click <A HREF="prefs_exclude_gen.85a">exclude generator settings</A> to exclude the generator settings from your saved preferences the next time they are saved.</TD></TR>'

  +'<TR><TD>&nbsp;<BR>Click <A HREF="abandon_prefs.85a">abandon preferences</A> to abandon your saved preferences. You won''t see the option to use your preferences when you start the next Templot session.</TD></TR>'

  +'</TABLE>';

  html_right_str:string='</TD><TD VALIGN="TOP">'    // right column
  +'<TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="0" CELLPADDING="12">'
  +'<TR><TD STYLE="PADDING-TOP:NONE;">Program preferences are your settings for the various options in Templot which affect its appearance on the screen and control the way the program operates and displays your work.'
  +'<BR><BR><SPAN STYLE="FONT-SIZE:16PX; COLOR:#A00000;">(In the current version of Templot only a limited number of program settings are saved in your preferences. More will be added in later versions.)</SPAN>'
  +'<BR><BR>'
  +'Program preferences do not include matters related to track design such as your model scale and track gauge, timbering styles, etc. These are saved in your template data files.'
  +'<BR><BR>'
  +'When you quit a Templot session, your program preferences can be saved, and they can be restored at the start of the next session.'
  +'<BR><BR>'
  +'The controls for saving your program preferences are accessed from the <SPAN STYLE="COLOR:#773300; FONT-STYLE:ITALIC;">preferences</SPAN> menu on the <SPAN STYLE="COLOR:#773300; FONT-STYLE:ITALIC;">program panel</SPAN>:'
  +'<BR><BR>'
  +'<IMG SRC="~~~3">'
  +'<BR><BR>'
  +'It is also possible to create several different preferences files and choose which one to use at any time.'
  +'<BR><BR>'
  +'The template generator settings can be included in your saved preferences. Use this option with care.'
  +' The generator settings are intended for short-term changes while working in Templot. Including them in your saved preferences may cause essential functions to be disabled or unavailable.'
  +' Your generator settings are currently being ~~~4.'
  +'<BR><BR>'
  +'Preferences files can have any name of your choosing but always have file extension <B><I>.sk1</I></B> and are always saved in the program folder: ~~~2'
  +'<BR><BR>'
  +'Your preferences files can be shared with other Templot users, provided they are saved to the user''s program folder. If you do this, it is strongly recommended that you exclude the generator settings.'
  +'</TD></TR></TABLE>'
  +'</TD></TR></TABLE></P>';    // outer table - final closing of body tags is in help() routine;

var
  file_loc_str:string;
  current_prefs_str:string;

  help_str:string;

begin
  if prefs_available=False
     then help_str:=html_header_str+html_left1_str+html_right_str
     else begin
            help_str:=html_header_str+html_left2_str+html_right_str;  // must be var to modify

              // !!! creates pointer if not present, current file, in keys style..
            current_prefs_str:='<SPAN STYLE="FONT-WEIGHT:BOLD; COLOR:#3000D0; FONT-FAMILY:Courier New; FONT-SIZE:17PX;">'+get_prefs_file_name+'</SPAN>';

            help_str:=StringReplace(help_str,'~~~1',current_prefs_str,[rfIgnoreCase]);
          end;

  file_loc_str:='<SPAN STYLE="FONT-WEIGHT:BOLD; COLOR:#3000D0; FONT-FAMILY:Courier New; FONT-SIZE:17PX;">'+exe_str+'</SPAN>';  // program folder, in keys style.

  help_str:=StringReplace(help_str,'~~~2',file_loc_str,[rfIgnoreCase]);
  help_str:=StringReplace(help_str,'~~~3',exe_str+'internal\hlp\saved_prefs.png',[rfIgnoreCase]);

  if control_room_form.prefs_include_gen_settings_menu_entry.Checked=True
     then help_str:=StringReplace(help_str,'~~~4','<B><I>included</I></B>',[rfIgnoreCase])
     else help_str:=StringReplace(help_str,'~~~4','<B><I>excluded</I></B>',[rfIgnoreCase]);

  RESULT:=help_str;
end;
//______________________________________________________________________________

procedure show_prefs_dialog(help_showing:boolean);

begin
  no_new_help_sizes:=True;      // don't change the user's default sizes.

  if Screen.PixelsPerInch>120     // 211b
     then begin
            help_form.ClientWidth:=1330;    // *1.4 ..
            help_form.ClientHeight:=952;
          end
     else begin
            help_form.ClientWidth:=950;
            help_form.ClientHeight:=680;
          end;

  resize_help_form;

  if help_showing=True
     then do_help(-200,prepare_prefs_html_str,False)  // update the contents
     else help(-200,prepare_prefs_html_str,'');       // shows modal

  no_new_help_sizes:=False;    // allow user drag resize again.
end;
//______________________________________________________________________________


function get_prefs_file_name:string;   // return current file name (without extension)

var
  prefs_txt:TextFile;
  prefs_file_str:string;

begin
  prefs_file_str:='prefs';   // default init

     // get the prefs file name from the pointer file...

  if FileExists(exe_str+prefs_pointer_str)=False     // no pointer file
     then begin                                      // so create it
            try
              AssignFile(prefs_txt,exe_str+prefs_pointer_str); // set the file name.
              Rewrite(prefs_txt);                              // open a new file.
              WriteLn(prefs_txt,prefs_file_str);               // write the file data -- default prefs file name
              WriteLn(prefs_txt,'Do not edit or delete this file.');
              CloseFile(prefs_txt);                            // and close the file.
            except
              DeleteFile(exe_str+prefs_pointer_str);
              show_modal_message('File error. Unable to create preferences files.');
            end;//try
          end
     else begin    // pointer file exists, get the required prefs file
            try
              AssignFile(prefs_txt,exe_str+prefs_pointer_str);
              Reset(prefs_txt);
              ReadLn(prefs_txt,prefs_file_str);   // Read the first line out of the file
              CloseFile(prefs_txt);
            except
              prefs_file_str:='prefs';   // reset default.
              show_modal_message('File error. Unable to access preferences files.');
            end;//try
          end;
  RESULT:=prefs_file_str;
end;
//______________________________________________________________________________

function prepare_string_for_prefs(str:string):string;

begin
  RESULT:=str_prefix+StringReplace(str,'=','%#%#%',[rfReplaceAll, rfIgnoreCase]);   // no '=' in string
end;
//______________________________________________________________________________

procedure save_font_to_prefs(str:string; prefs_font:TFont);

begin
  with prefs_font do begin
    user_prefs_list.Values[str+'0s']:=str_prefix+Name;
    user_prefs_list.Values[str+'1i']:=IntToStr(Size);
    user_prefs_list.Values[str+'2i']:=IntToStr(Color);
    user_prefs_list.Values[str+'3b']:=IntToStr(integer(fsBold in Style));
    user_prefs_list.Values[str+'4b']:=IntToStr(integer(fsItalic in Style));
    user_prefs_list.Values[str+'5b']:=IntToStr(integer(fsUnderline in Style));
    user_prefs_list.Values[str+'6b']:=IntToStr(integer(fsStrikeout in Style));
  end;//with
end;
//______________________________________________________________________________

procedure save_prefs(prefs_file_str:string; confirm_it:boolean);

 // if prefs_file_str is empty, save to existing file -- if confirm_it=True, ask first

var
  n,index,rail_prefs:integer;

  print_colour_prefs:integer;  // 207a

  save_str:string;

begin

  if prefs_file_str=''
     then begin
            prefs_file_str:=get_prefs_file_name;
            if confirm_it=True
               then begin
                      if alert(7,'      save  program  preferences ?',
                         '||You are about to replace your previous program preferences in file: '+prefs_file_str
                        +'||OK?| ',
                          '','','','','no  -  cancel','yes  -  save  preferences    ',0)=5 then EXIT;
                    end;
          end;

  with user_prefs_list do begin

    Clear;  // init

    Values['templot_user_preferences']:=str_prefix+'from '+Application.Title+' version '+FormatFloat('0.00',program_version/100)+version_build;
    Values['saved_at']:=str_prefix+FormatDateTime('hh:nn:ss "on" dd/mm/yyyy',Date+Time);
    Add('');

    Add('--------  grid/ruler/outlines:  --------');
    Values['00-00i']:=IntToStr(grid_labels_code_i);   // grid
    Values['00-10f']:=FloatToStr(grid_spacex);
    Values['00-20f']:=FloatToStr(grid_spacey);

    Values['00-30i']:=IntToStr(show_margins);           // page outlines

    Values['00-40i']:=IntToStr(ruler_units);            // ruler units code
    Values['00-50f']:=FloatToStr(ruler_div);            // ruler division spacing
    Add('');

    Add('--------  other globals:  --------');
    // out 213b Values['00-70b']:=IntToStr(integer(enable_gen_menu_pref));  // generator menu enabled, and pref wanted
    Values['00-80i']:=IntToStr(wheel_zoom_code);                // mouse wheel zoom mode 0.97.d

    Values['00-82i']:=IntToStr(user_popup_X);   // 213a     pop-up menu position
    Values['00-83i']:=IntToStr(user_popup_Y);   // 213a

    Values['00-85b']:=IntToStr(integer(control_room_form.blank_spaces_in_menus_menu_entry.Checked));    // 213b

    Values['00-87b']:=IntToStr(integer(retain_prefix_pref));    // 213b

  with cdvi do begin    // dummy vehicle dims  0.98.a

    Values['00-90f']:=FloatToStr(dv_start);      // to first axle/bogie-pin from CTRL-0
    Values['00-91f']:=FloatToStr(dv_length);     // body length
    Values['00-92f']:=FloatToStr(dv_width);      // body width
    Values['00-93f']:=FloatToStr(dv_clearance);  // clearance on width
    Values['00-94f']:=FloatToStr(dv_wheelbase);  // wheelbase / bogie centres

  end;//with

    Add('');

    Add('--------  background shapes:  --------');
    Values['01-01b']:=IntToStr(integer(bgnd_form.pad_shapes_linewidth_1_radiobutton.Checked));
    Values['01-02b']:=IntToStr(integer(bgnd_form.pad_shapes_linewidth_2_radiobutton.Checked));
    Values['01-03b']:=IntToStr(integer(bgnd_form.pad_shapes_linewidth_3_radiobutton.Checked));

    Values['01-10i']:=IntToStr(shapes_colour);  // on pad
    Values['01-20i']:=IntToStr(save_psc);       // on output

    save_font_to_prefs('01-3',shapes_label_font);

    Values['01-40b']:=IntToStr(integer(bgnd_form.trackpad_grid_in_front_checkbox.Checked));
    Add('');

    Add('--------  startup dialogs:  --------');
    Values['02-01i']:=IntToStr(start_colours);
    Values['02-05i']:=IntToStr(startup_restore_pref); // 0=ask, 1=don't restore, 2=restore without asking

    Values['02-10b']:=IntToStr(integer(res_msg_pref));
    Values['02-20b']:=IntToStr(integer(ppi_msg_pref));
    Values['02-30b']:=IntToStr(integer(he_wants_multiple_monitors));
    Values['02-40b']:=IntToStr(integer(start_scheme_msg_pref));
    Values['02-50b']:=IntToStr(integer(multi_monitors_msg_pref));
    Values['02-60b']:=IntToStr(integer(refresh_msg_pref));
    Values['02-70b']:=IntToStr(integer(startup_full_draw_pref));
    Values['02-80b']:=IntToStr(integer(startup_offscreen_pref));
    Values['02-90b']:=IntToStr(integer(prefs_warn_msg_pref));
    Values['02-92b']:=IntToStr(integer(wine_warn_msg_pref));
    Add('');

    Add('--------  trackpad name labels:  --------');

    save_font_to_prefs('03-3',pad_form.bgnd_keeps_font_label.Font);

    Values['03-41b']:=IntToStr(integer(pad_form.transparent_names_menu_entry.Checked));  // 208a ...
    Values['03-42b']:=IntToStr(integer(pad_form.boxed_over_names_menu_entry.Checked));

    Values['03-43b']:=IntToStr(integer(pad_form.names_scaled_menu_entry.Checked));
    Values['03-44b']:=IntToStr(integer(pad_form.names_fullsize_menu_entry.Checked));

    Add('');

    Add('--------  trackpad control template:  --------');

    Values['03-60b']:=IntToStr(integer(pad_form.peg_blank_menu_entry.Checked));
    Values['03-61b']:=IntToStr(integer(pad_form.peg_solid_red_menu_entry.Checked));
    Values['03-62b']:=IntToStr(integer(pad_form.peg_hollow_menu_entry.Checked));

    Values['03-70b']:=IntToStr(integer(pad_form.no_f7_snapping_menu_entry.Checked));                       // 218a radio item
    Values['03-71b']:=IntToStr(integer(pad_form.snap_on_background_templates_menu_entry.Checked));         // 218a radio item
    Values['03-72b']:=IntToStr(integer(pad_form.snap_always_on_background_templates_menu_entry.Checked));  // 218a radio item

    Add('');

    Add('--------  trackpad background templates:  --------');    // 208a ...

    Values['03-80b']:=IntToStr(integer(bgkeeps_form.timber_outlines_checkbox.Checked));
    Values['03-81b']:=IntToStr(integer(bgkeeps_form.timber_centres_checkbox.Checked));
    Values['03-82b']:=IntToStr(integer(bgkeeps_form.gauge_faces_checkbox.Checked));
    Values['03-83b']:=IntToStr(integer(bgkeeps_form.marks_checkbox.Checked));
    Values['03-84b']:=IntToStr(integer(bgkeeps_form.joints_checkbox.Checked));
    Values['03-85b']:=IntToStr(integer(bgkeeps_form.centres_checkbox.Checked));
    Values['03-86b']:=IntToStr(integer(bgkeeps_form.peg_checkbox.Checked));
    Values['03-87b']:=IntToStr(integer(bgkeeps_form.outer_edges_checkbox.Checked));
    Values['03-88b']:=IntToStr(integer(bgkeeps_form.template_number_checkbox.Checked));
    Values['03-89b']:=IntToStr(integer(bgkeeps_form.template_name_checkbox.Checked));
    Values['03-90b']:=IntToStr(integer(bgkeeps_form.reduced_ends_checkbox.Checked));
    Values['03-91b']:=IntToStr(integer(bgkeeps_form.timber_infill_checkbox.Checked));
    Values['03-92b']:=IntToStr(integer(bgkeeps_form.timber_numbering_checkbox.Checked));
    Values['03-93b']:=IntToStr(integer(bgkeeps_form.platforms_checkbox.Checked));
    Values['03-94b']:=IntToStr(integer(bgkeeps_form.trackbed_edges_checkbox.Checked));
    Values['03-96b']:=IntToStr(integer(bgkeeps_form.template_id_checkbox.Checked));
    Values['03-97b']:=IntToStr(integer(bgkeeps_form.bold_timber_outlines_checkbox.Checked));    // 219a

    Add('');

    Add('--------  alert dialogs:  --------');

    Values['04-10b']:=IntToStr(integer(quit_confirm_msg_pref));
    Values['04-15b']:=IntToStr(integer(delete_to_current_msg_pref));
    Values['04-24b']:=IntToStr(integer(k180_message_msg_pref));
    Values['04-26b']:=IntToStr(integer(k_notch_msg_pref));
    Values['04-28b']:=IntToStr(integer(mirrory_msg_pref));
    Values['04-30b']:=IntToStr(integer(mirrorx_msg_pref));
    Values['04-32b']:=IntToStr(integer(dupg_msg_pref));
    Values['04-34b']:=IntToStr(integer(small_print_msg_pref));
    Values['04-36b']:=IntToStr(integer(spiral_mouse_msg_pref));
    Values['04-38b']:=IntToStr(integer(group_colour_msg_pref));

    Values['04-40b']:=IntToStr(integer(b6_msg_pref));
    Values['04-42b']:=IntToStr(integer(no_delete_msg_pref));
    Values['04-46b']:=IntToStr(integer(no_unused_msg_pref));
    Values['04-48b']:=IntToStr(integer(no_library_msg_pref));
    Values['04-50b']:=IntToStr(integer(no_undo_clear_msg_pref));
    Values['04-52b']:=IntToStr(integer(no_cut_msg_pref));

    Values['04-54b']:=IntToStr(integer(trans_msg_pref));
    Values['04-56b']:=IntToStr(integer(cancel_entries_msg_pref));
    Values['04-58b']:=IntToStr(integer(no_extra_colours_msg_pref));
    Values['04-60b']:=IntToStr(integer(omit_all_msg_pref));

    Values['04-62b']:=IntToStr(integer(confirm_detail_mode_1_msg_pref));
    Values['04-64b']:=IntToStr(integer(confirm_detail_mode_2_msg_pref));

    Values['04-66b']:=IntToStr(integer(irreg_crossings_msg_pref));  // 0.93.a

    Values['04-68b']:=IntToStr(integer(sb_rvf_add_msg_pref));    // 0.97.a
    Values['04-70b']:=IntToStr(integer(sb_no_select_msg_pref));  // 0.98.b

    Values['04-72b']:=IntToStr(integer(too_many_files_msg_pref));  // 208e

    Values['04-74b']:=IntToStr(integer(retain_prefix_msg_pref));   // 213b

    Values['04-76b']:=IntToStr(integer(drop_msg_pref));            // 214a
    Values['04-78b']:=IntToStr(integer(yellow_msg_pref));          // 214a
    Values['04-80b']:=IntToStr(integer(auto_fit_msg_pref));        // 214a

    Values['04-82b']:=IntToStr(integer(screenshot_msg_pref));      // 215a


    Add('');

    Add('--------  cpu / mouse actions / menus:  --------');
    Values['05-05b']:=IntToStr(integer(classic_templot));
    Values['05-10b']:=IntToStr(integer(full_draw_trans_pref));
    Values['05-15b']:=IntToStr(integer(auto_dir));
    Values['05-20i']:=IntToStr(mouse_click_action);           // -1 = either click-move-click or drag allowed.

    Values['05-30b']:=IntToStr(integer(control_room_form.fast_100_menu_entry.Checked));         // allow_idle = False
    Values['05-31b']:=IntToStr(integer(control_room_form.mouse_100_menu_entry.Checked));        // allow_idle = True
    Values['05-32b']:=IntToStr(integer(control_room_form.allow_full_idle_menu_entry.Checked));  // allow_idle = True

    Values['05-40b']:=IntToStr(integer(control_room_form.xp_menus_menu_entry.Checked));      // XP   style menus
    Values['05-41b']:=IntToStr(integer(control_room_form.win7_menus_menu_entry.Checked));    // Win7 style menus

    Values['05-50i']:=IntToStr(action_form.ClientHeight);      // 215b


    Add('');

    if control_room_form.prefs_include_gen_settings_menu_entry.Checked=True
       then begin
              Add('--------  generator:  --------');
              Values['06-00b']:='1';   //control_room_form.prefs_include_gen_settings_menu_entry.Checked (so can save again).

              Values['06-05b']:=IntToStr(integer(pad_form.timber_chairs_menu_entry.Checked));
              Values['06-10b']:=IntToStr(integer(pad_form.centre_lines_menu_entry.Checked));
              Values['06-15b']:=IntToStr(integer(pad_form.rad_end_marks_menu_entry.Checked));
              Values['06-20b']:=IntToStr(integer(pad_form.timber_outlines_menu_entry.Checked));
              Values['06-25b']:=IntToStr(integer(pad_form.outline_extensions_menu_entry.Checked));
              Values['06-30b']:=IntToStr(integer(pad_form.timber_centres_menu_entry.Checked));
              Values['06-35b']:=IntToStr(integer(pad_form.timber_numbers_menu_entry.Checked));
              Values['06-40b']:=IntToStr(integer(pad_form.timbering_infill_menu_entry.Checked));
              Values['06-45b']:=IntToStr(integer(pad_form.reduced_ends_menu_entry.Checked));
              Values['06-50b']:=IntToStr(integer(pad_form.stock_rails_menu_entry.Checked));

              Values['06-52b']:=IntToStr(integer(pad_form.crossing_rails_menu_entry.Checked));
              Values['06-54b']:=IntToStr(integer(pad_form.check_rails_menu_entry.Checked));
              Values['06-56b']:=IntToStr(integer(pad_form.overscale_joggles_menu_entry.Checked));
              Values['06-58b']:=IntToStr(integer(pad_form.joint_marks_menu_entry.Checked));
              Values['06-60b']:=IntToStr(integer(pad_form.guide_marks_menu_entry.Checked));
              Values['06-62b']:=IntToStr(integer(pad_form.switch_drive_markx_menu_entry.Checked));

              Values['06-65b']:=IntToStr(integer(pad_form.gen_platforms_trackbed_edges_menu_entry.Checked));

              rail_prefs:=-1; // init
              if (pad_form.both_edges_menu_entry.Checked=True) then rail_prefs:=0;
              if (pad_form.gauge_faces_only_menu_entry.Checked=True) then rail_prefs:=1;
              if (pad_form.outer_edges_only_menu_entry.Checked=True) then rail_prefs:=2;
              if (pad_form.rail_centrelines_only_menu_entry.Checked=True) then rail_prefs:=3;
              Values['06-80i']:=IntToStr(rail_prefs);

              Add('');
            end;

    Add('--------  help texts:  --------');

    //Values['07-00i']:=IntToStr(help_form.text_updown.Position);   // text zoom level    out 215a

    Values['07-02i']:=IntToStr(help_font_style);       // 0=normal  1=bold  215a
    Values['07-04i']:=IntToStr(help_font_size);        // px  215a

    Values['07-10i']:=IntToStr(Round(help_client_width_as_user/global_factor));
    Values['07-20i']:=IntToStr(Round(help_client_height_as_user/global_factor));

    Add('');

    Add('--------  print output:  --------');
    save_font_to_prefs('08-0',print_corner_page_numbers_font);

    { OT-FIRST Values['08-10f']:=FloatToStr(track_bgnd_width_in);}   // 206a
    Values['08-11i']:=IntToStr(save_sb_track_bgnd);      // colour 206a
    Values['08-12i']:=IntToStr(save_sb_diagram_col);     // colour 209c

    print_colour_prefs:=0; // default = colour printing     // 207a ...
    if grey_shade=True then print_colour_prefs:=1;
    if black_white=True then print_colour_prefs:=2;
    Values['08-15i']:=IntToStr(print_colour_prefs);

    Values['08-17b']:=IntToStr(integer(pad_form.printed_grid_solid_menu_entry.Checked));   // 207a radio item
    Values['08-18b']:=IntToStr(integer(pad_form.printed_grid_dotted_menu_entry.Checked));

    Values['08-20b']:=IntToStr(integer(pad_form.thin_printed_lines_menu_entry.Checked));   // 207a radio item
    Values['08-21b']:=IntToStr(integer(pad_form.normal_printed_lines_menu_entry.Checked)); // 207a radio item
    Values['08-22b']:=IntToStr(integer(pad_form.thick_printed_lines_menu_entry.Checked));  // 207a radio item
    Values['08-23b']:=IntToStr(integer(pad_form.set_line_thicknesses_menu_entry.Checked)); // 207a radio item

    Values['08-25b']:=IntToStr(integer(pad_form.adjust_line_thickness_menu_entry.Checked));   // 207a

    Values['08-31f']:=FloatToStr(printgrid_thick);             // mm 207a ...
    Values['08-32f']:=FloatToStr(printmargin_thick);           // mm.
    Values['08-33f']:=FloatToStr(printtimber_thick);           // mm.
    Values['08-34f']:=FloatToStr(printrail_thick);             // mm
    Values['08-35f']:=FloatToStr(printmark_thick);             // mm.
    Values['08-36f']:=FloatToStr(printshape_thick);            // mm.
    Values['08-37f']:=FloatToStr(printpicborder_thick);        // mm.
    Values['08-38f']:=FloatToStr(printcl_thick);               // mm.

    Values['08-41i']:=IntToStr(rail_infill_i);
    Values['08-42i']:=IntToStr(print_timb_infill_style);
    Values['08-43i']:=IntToStr(print_platform_infill_style);

    Values['08-45b']:=IntToStr(integer(pad_form.timber_numbering_on_plain_track_menu_entry.Checked)); // 208a


    Add('');

    Add('--------  sketchboard:  --------');
    { OT-FIRST Values['09-05f']:=FloatToStr(freehand_tracking);}

    { OT-FIRST Values['09-10b']:=IntToStr(integer(dtp_settings_form.design_quality_radiobutton.Checked));
    Values['09-11b']:=IntToStr(integer(dtp_settings_form.display_quality_radiobutton.Checked));}

    Add('');

    Add('--------  storage box:  --------');
    Values['10-00b']:=IntToStr(integer(keep_form.briefly_hide_on_store_menu_entry.Checked));     // 205e now radio item.  205c
    Values['10-01b']:=IntToStr(integer(keep_form.hide_on_store_menu_entry.Checked));             // 205e radio item
    Values['10-02b']:=IntToStr(integer(keep_form.dont_hide_on_store_menu_entry.Checked));        // 205e radio item
    Values['10-03b']:=IntToStr(integer(keep_form.reveal_on_store_menu_entry.Checked));           // 206a radio item
    Values['10-04b']:=IntToStr(integer(keep_form.alert_on_store_menu_entry.Checked));            // 206c radio item

    Add('');

    Add('--------  custom colours:  --------');      // 208a

    with colour_form.colour_dialog do begin     // !!! NOT .CustomColors  -- Values is the prefs list

      Values['12-00s']:=prepare_string_for_prefs(CustomColors.Strings[0]);
      Values['12-01s']:=prepare_string_for_prefs(CustomColors.Strings[1]);
      Values['12-02s']:=prepare_string_for_prefs(CustomColors.Strings[2]);
      Values['12-03s']:=prepare_string_for_prefs(CustomColors.Strings[3]);
      Values['12-04s']:=prepare_string_for_prefs(CustomColors.Strings[4]);
      Values['12-05s']:=prepare_string_for_prefs(CustomColors.Strings[5]);
      Values['12-06s']:=prepare_string_for_prefs(CustomColors.Strings[6]);
      Values['12-07s']:=prepare_string_for_prefs(CustomColors.Strings[7]);
      Values['12-08s']:=prepare_string_for_prefs(CustomColors.Strings[8]);
      Values['12-09s']:=prepare_string_for_prefs(CustomColors.Strings[9]);
      Values['12-10s']:=prepare_string_for_prefs(CustomColors.Strings[10]);
      Values['12-11s']:=prepare_string_for_prefs(CustomColors.Strings[11]);
      Values['12-12s']:=prepare_string_for_prefs(CustomColors.Strings[12]);
      Values['12-13s']:=prepare_string_for_prefs(CustomColors.Strings[13]);
      Values['12-14s']:=prepare_string_for_prefs(CustomColors.Strings[14]);
      Values['12-15s']:=prepare_string_for_prefs(CustomColors.Strings[15]);
    end;//with

    Add('');

    Add('--------  output page origin:  --------');  // 208b
    Values['13-00b']:=IntToStr(integer(staggered_pages));

    Values['13-01f']:=FloatToStr(print_pages_top_origin);
    Values['13-02f']:=FloatToStr(print_pages_left_origin);

    Add('');

    Add('--------  toolbars:  --------');  // 217a
    Values['13-10b']:=IntToStr(integer(toolbars_2rows));

    Values['13-12i']:=IntToStr(make_slip_form.Left);
    Values['13-13i']:=intToStr(make_slip_form.Top);


    Add('--------  end  --------');

  end;//with

  if user_prefs_list.Count>0
     then begin
            try
              save_str:=exe_str+prefs_file_str+'.sk1';
              user_prefs_list.SaveToFile(save_str);
              prefs_available:=True;
              user_prefs_in_use:=True;    // added 207a
              if help_form.Showing=True then do_help(-200,prepare_prefs_html_str,False); // update details
            except
              show_modal_message('Sorry, preferences could not be saved.');
            end;//try
          end
     else show_modal_message('Sorry, preferences could not be saved.');
end;
//______________________________________________________________________________

function get_pref_int(str:string; def_int:integer):integer;

var
  val_str:string;
  temp_int:integer;

begin
  RESULT:=def_int;  // default init.

  val_str:=user_prefs_list.Values[str];

  if val_str='' then EXIT;   // not in list

  try
    temp_int:=StrToInt(val_str);
  except
    EXIT;   // invalid value
  end;//try

  RESULT:=temp_int;  // return result
end;
//______________________________________________________________________________

function get_pref_bool(str:string; def_bool:boolean):boolean;

var
  val_str:string;

begin
  RESULT:=def_bool;  // default init.

  val_str:=user_prefs_list.Values[str];

  if val_str='' then EXIT;   // not in list

  RESULT:=(val_str='1');  // return result
end;
//______________________________________________________________________________

function get_pref_float(str:string; def_float:extended):extended;

var
  val_str:string;
  temp_float:extended;

begin
  RESULT:=def_float;  // default init.

  val_str:=user_prefs_list.Values[str];

  if val_str='' then EXIT;   // not in list

  try
    temp_float:=StrToFloat(val_str);
  except
    EXIT;   // invalid value
  end;//try

  RESULT:=temp_float;  // return result
end;
//______________________________________________________________________________

function get_pref_string(str:string; def_string:string):string;

var
  val_str:string;

begin
  RESULT:=def_string;  // default init.

  val_str:=user_prefs_list.Values[str];

  if val_str='' then EXIT;   // not in list

  val_str:=StringReplace(val_str,'%#%#%','=',[rfReplaceAll, rfIgnoreCase]);    // replace '=' in string

  RESULT:=StringReplace(val_str,str_prefix,'',[rfReplaceAll, rfIgnoreCase]);   // remove string prefix
end;
//______________________________________________________________________________

function get_pref_font(str:string; def_font:TFont):TFont;

// str+ ...
// 0s  // Name
// 1i  // Size
// 2i  // Color
// 3b  // fsBold
// 4b  // fsItalic
// 5b  // fsUnderline
// 6b  // fsStrikeout

var
  val_str:string;
  temp_int:integer;

begin
  RESULT:=def_font;  // default init.

  with prefs_font do begin  // temp result font

    val_str:=user_prefs_list.Values[str+'0s'];
    if val_str='' then Name:=def_font.Name  // not in list, use default
                  else Name:=StringReplace(val_str,str_prefix,'',[rfReplaceAll, rfIgnoreCase]);   // remove string prefix

    val_str:=user_prefs_list.Values[str+'1i'];
    if val_str='' then Size:=def_font.Size  // not in list, use default
                  else begin
                         try
                           temp_int:=StrToInt(val_str);
                         except
                           EXIT;   // invalid value
                         end;//try
                         Size:=temp_int;
                       end;

    val_str:=user_prefs_list.Values[str+'2i'];
    if val_str='' then Color:=def_font.Color      // not in list, use default
                  else begin
                         try
                           temp_int:=StrToInt(val_str);
                         except
                           EXIT;   // invalid value
                         end;//try
                         Color:=temp_int;  // return result
                       end;

    Style:=def_font.Style;  // init

    val_str:=user_prefs_list.Values[str+'3b'];
    if val_str='1' then Style:=Style+[fsBold];
    if val_str='0' then Style:=Style-[fsBold];

    val_str:=user_prefs_list.Values[str+'4b'];
    if val_str='1' then Style:=Style+[fsItalic];
    if val_str='0' then Style:=Style-[fsItalic];

    val_str:=user_prefs_list.Values[str+'5b'];
    if val_str='1' then Style:=Style+[fsUnderline];
    if val_str='0' then Style:=Style-[fsUnderline];

    val_str:=user_prefs_list.Values[str+'6b'];
    if val_str='1' then Style:=Style+[fsStrikeout];
    if val_str='0' then Style:=Style-[fsStrikeout];

  end;//with

  RESULT:=prefs_font; // return result
end;
//______________________________________________________________________________

procedure load_prefs(prefs_file_str:string; get_file,show_them,startup_msg_prefs:boolean);

var
  n,index,rail_prefs:integer;

  print_colour_prefs:integer;    // 207a


begin
  if get_file=True
     then begin
            if prefs_file_str='' then prefs_file_str:=get_prefs_file_name;  // get his existing preferences file...

            try
              user_prefs_list.LoadFromFile(exe_str+prefs_file_str+'.sk1');
            except
              user_prefs_list.Clear;
              user_prefs_in_use:=False;
              show_modal_message('Sorry, preferences file not found.');
              EXIT;
            end;//try
          end;

  if startup_msg_prefs=True
     then begin
                //-------- startup dialogs

            start_colours:=               get_pref_int('02-01i',start_colours);
            startup_restore_pref:=        get_pref_int('02-05i',startup_restore_pref); // 0=ask, 1=don't restore, 2=restore without asking

            res_msg_pref:=               get_pref_bool('02-10b',res_msg_pref);
            ppi_msg_pref:=               get_pref_bool('02-20b',ppi_msg_pref);
            he_wants_multiple_monitors:= get_pref_bool('02-30b',he_wants_multiple_monitors);
            start_scheme_msg_pref:=      get_pref_bool('02-40b',start_scheme_msg_pref);
            multi_monitors_msg_pref:=    get_pref_bool('02-50b',multi_monitors_msg_pref);
            refresh_msg_pref:=           get_pref_bool('02-60b',refresh_msg_pref);
            startup_full_draw_pref:=     get_pref_bool('02-70b',startup_full_draw_pref);
            startup_offscreen_pref:=     get_pref_bool('02-80b',startup_offscreen_pref);
            prefs_warn_msg_pref:=        get_pref_bool('02-90b',prefs_warn_msg_pref);
            wine_warn_msg_pref:=         get_pref_bool('02-92b',wine_warn_msg_pref);

          end
     else begin

            loading_his_prefs:=True;  // 208a

            try

              //-------- grid/ruler/outlines
              grid_labels_code_i:= get_pref_int('00-00i',grid_labels_code_i);   // grid
              grid_spacex:=      get_pref_float('00-10f',grid_spacex);
              grid_spacey:=      get_pref_float('00-20f',grid_spacey);

              show_margins:=       get_pref_int('00-30i',show_margins);   // page outlines

              ruler_units:=        get_pref_int('00-40i',ruler_units);   // ruler units code
              ruler_div:=        get_pref_float('00-50f',ruler_div);     // ruler division spacing

              //-------- other globals

              wheel_zoom_code:=    get_pref_int('00-80i',wheel_zoom_code);  // mouse wheel zoom mode 0.97.d

              user_popup_X:=       get_pref_int('00-82i',user_popup_X);   // 213a     pop-up menu position
              user_popup_Y:=       get_pref_int('00-83i',user_popup_Y);

              if control_room_form.blank_spaces_in_menus_menu_entry.Checked<>get_pref_bool('00-85b',control_room_form.blank_spaces_in_menus_menu_entry.Checked)  // 213b
                 then control_room_form.blank_spaces_in_menus_menu_entry.Click;

              retain_prefix_pref:=get_pref_bool('00-87b',retain_prefix_pref);  // 213b

              with cdvi do begin    // dummy vehicle dims  0.98.a

                dv_start:=     get_pref_float('00-90f',dv_start);      // to first axle/bogie-pin from CTRL-0
                dv_length:=    get_pref_float('00-91f',dv_length);     // body length
                dv_width:=     get_pref_float('00-92f',dv_width);      // body width
                dv_clearance:= get_pref_float('00-93f',dv_clearance);  // clearance on width
                dv_wheelbase:= get_pref_float('00-94f',dv_wheelbase);  // wheelbase / bogie centres

              end;//with


              //-------- bgnd shapes
              with bgnd_form.pad_shapes_linewidth_1_radiobutton do Checked:=get_pref_bool('01-01b',Checked);
              with bgnd_form.pad_shapes_linewidth_2_radiobutton do Checked:=get_pref_bool('01-02b',Checked);
              with bgnd_form.pad_shapes_linewidth_3_radiobutton do Checked:=get_pref_bool('01-03b',Checked);

              shapes_colour:=  get_pref_int('01-10i',shapes_colour);  // on pad
              save_psc:=       get_pref_int('01-20i',save_psc);       // on output

              shapes_label_font.Assign(get_pref_font('01-3',shapes_label_font));

              with bgnd_form.trackpad_grid_in_front_checkbox do begin
                Checked:=get_pref_bool('01-40b',Checked);
                pad_form.grid_in_front_of_shapes_menu_entry.Checked:=Checked;
              end;//with


              //-------- trackpad background templates
              with pad_form.bgnd_keeps_font_label do Font.Assign(get_pref_font('03-3',Font));

          with pad_form.transparent_names_menu_entry do Checked:=get_pref_bool('03-41b',Checked);
           with pad_form.boxed_over_names_menu_entry do Checked:=get_pref_bool('03-42b',Checked);

               with pad_form.names_scaled_menu_entry do Checked:=get_pref_bool('03-43b',Checked);
             with pad_form.names_fullsize_menu_entry do Checked:=get_pref_bool('03-44b',Checked);


              //-------- trackpad control template
                  with pad_form.peg_blank_menu_entry do Checked:=get_pref_bool('03-60b',Checked);
              with pad_form.peg_solid_red_menu_entry do Checked:=get_pref_bool('03-61b',Checked);
                 with pad_form.peg_hollow_menu_entry do Checked:=get_pref_bool('03-62b',Checked);


                      with pad_form.no_f7_snapping_menu_entry do Checked:=get_pref_bool('03-70b',Checked);     // 218a radio item
        with pad_form.snap_on_background_templates_menu_entry do Checked:=get_pref_bool('03-71b',Checked);     // 218a radio item
 with pad_form.snap_always_on_background_templates_menu_entry do Checked:=get_pref_bool('03-72b',Checked);     // 218a radio item


              //--------  trackpad background templates    // 208a ...

             with bgkeeps_form.timber_outlines_checkbox do Checked:=get_pref_bool('03-80b',Checked);
              with bgkeeps_form.timber_centres_checkbox do Checked:=get_pref_bool('03-81b',Checked);
                 with bgkeeps_form.gauge_faces_checkbox do Checked:=get_pref_bool('03-82b',Checked);
                       with bgkeeps_form.marks_checkbox do Checked:=get_pref_bool('03-83b',Checked);
                      with bgkeeps_form.joints_checkbox do Checked:=get_pref_bool('03-84b',Checked);
                     with bgkeeps_form.centres_checkbox do Checked:=get_pref_bool('03-85b',Checked);
                         with bgkeeps_form.peg_checkbox do Checked:=get_pref_bool('03-86b',Checked);
                 with bgkeeps_form.outer_edges_checkbox do Checked:=get_pref_bool('03-87b',Checked);
             with bgkeeps_form.template_number_checkbox do Checked:=get_pref_bool('03-88b',Checked);
               with bgkeeps_form.template_name_checkbox do Checked:=get_pref_bool('03-89b',Checked);
                with bgkeeps_form.reduced_ends_checkbox do Checked:=get_pref_bool('03-90b',Checked);
               with bgkeeps_form.timber_infill_checkbox do Checked:=get_pref_bool('03-91b',Checked);
            with bgkeeps_form.timber_numbering_checkbox do Checked:=get_pref_bool('03-92b',Checked);
                   with bgkeeps_form.platforms_checkbox do Checked:=get_pref_bool('03-93b',Checked);
              with bgkeeps_form.trackbed_edges_checkbox do Checked:=get_pref_bool('03-94b',Checked);
                 with bgkeeps_form.template_id_checkbox do Checked:=get_pref_bool('03-96b',Checked);
        with bgkeeps_form.bold_timber_outlines_checkbox do Checked:=get_pref_bool('03-97b',Checked);    // 219a


              //--------  alert dialogs
                    quit_confirm_msg_pref:=get_pref_bool('04-10b',quit_confirm_msg_pref);
               delete_to_current_msg_pref:=get_pref_bool('04-15b',delete_to_current_msg_pref);
                    k180_message_msg_pref:=get_pref_bool('04-24b',k180_message_msg_pref);
                         k_notch_msg_pref:=get_pref_bool('04-26b',k_notch_msg_pref);
                         mirrory_msg_pref:=get_pref_bool('04-28b',mirrory_msg_pref);
                         mirrorx_msg_pref:=get_pref_bool('04-30b',mirrorx_msg_pref);
                            dupg_msg_pref:=get_pref_bool('04-32b',dupg_msg_pref);
                     small_print_msg_pref:=get_pref_bool('04-34b',small_print_msg_pref);
                    spiral_mouse_msg_pref:=get_pref_bool('04-36b',spiral_mouse_msg_pref);
                    group_colour_msg_pref:=get_pref_bool('04-38b',group_colour_msg_pref);
                              b6_msg_pref:=get_pref_bool('04-40b',b6_msg_pref);
                       no_delete_msg_pref:=get_pref_bool('04-42b',no_delete_msg_pref);
                       no_unused_msg_pref:=get_pref_bool('04-46b',no_unused_msg_pref);
                      no_library_msg_pref:=get_pref_bool('04-48b',no_library_msg_pref);
                   no_undo_clear_msg_pref:=get_pref_bool('04-50b',no_undo_clear_msg_pref);
                          no_cut_msg_pref:=get_pref_bool('04-52b',no_cut_msg_pref);
                           trans_msg_pref:=get_pref_bool('04-54b',trans_msg_pref);
                  cancel_entries_msg_pref:=get_pref_bool('04-56b',cancel_entries_msg_pref);
                no_extra_colours_msg_pref:=get_pref_bool('04-58b',no_extra_colours_msg_pref);
                        omit_all_msg_pref:=get_pref_bool('04-60b',omit_all_msg_pref);
           confirm_detail_mode_1_msg_pref:=get_pref_bool('04-62b',confirm_detail_mode_1_msg_pref);
           confirm_detail_mode_2_msg_pref:=get_pref_bool('04-64b',confirm_detail_mode_2_msg_pref);
                 irreg_crossings_msg_pref:=get_pref_bool('04-66b',irreg_crossings_msg_pref);        // 0.93.a
                      sb_rvf_add_msg_pref:=get_pref_bool('04-68b',sb_rvf_add_msg_pref);             // 0.97.a
                    sb_no_select_msg_pref:=get_pref_bool('04-70b',sb_no_select_msg_pref);           // 0.98.b
                  too_many_files_msg_pref:=get_pref_bool('04-72b',too_many_files_msg_pref);         // 208e
                   retain_prefix_msg_pref:=get_pref_bool('04-74b',retain_prefix_msg_pref);          // 213b
                            drop_msg_pref:=get_pref_bool('04-76b',drop_msg_pref);                   // 214a
                          yellow_msg_pref:=get_pref_bool('04-78b',yellow_msg_pref);                 // 214a
                        auto_fit_msg_pref:=get_pref_bool('04-80b',auto_fit_msg_pref);               // 214a
                      screenshot_msg_pref:=get_pref_bool('04-82b',screenshot_msg_pref);             // 215a



              //--------  cpu / mouse actions / menus
                                 if get_pref_bool('05-05b',classic_templot)=True then control_room_form.classic_templot_mode_menu_entry.Click
                                                                                 else control_room_form.make_on_click_mode_menu_entry.Click;
              full_draw_trans_pref:=get_pref_bool('05-10b',full_draw_trans_pref);
                          auto_dir:=get_pref_bool('05-15b',auto_dir);

                 mouse_click_action:=get_pref_int('05-20i',mouse_click_action);    // -1 = either click-move-click or drag allowed.

              with control_room_form.fast_100_menu_entry do        Checked:=get_pref_bool('05-30b',Checked);    // radio items
              with control_room_form.mouse_100_menu_entry do       Checked:=get_pref_bool('05-31b',Checked);
              with control_room_form.allow_full_idle_menu_entry do Checked:=get_pref_bool('05-32b',Checked);
              allow_idle:= NOT control_room_form.fast_100_menu_entry.Checked;

              with control_room_form do begin
                if get_pref_bool('05-40b',xp_menus_menu_entry.Checked)<>xp_menus_menu_entry.Checked then xp_menus_menu_entry.Click;
                if get_pref_bool('05-41b',win7_menus_menu_entry.Checked)<>win7_menus_menu_entry.Checked then win7_menus_menu_entry.Click;
              end;

              action_form.ClientHeight:=get_pref_int('05-50i',action_form.ClientHeight);     // 215b


              //--------  generator
              with pad_form do begin
                if get_pref_bool('06-05b',timber_chairs_menu_entry.Checked)<>timber_chairs_menu_entry.Checked then timber_chairs_menu_entry.Click;
                if get_pref_bool('06-10b',centre_lines_menu_entry.Checked)<>centre_lines_menu_entry.Checked then centre_lines_menu_entry.Click;
                if get_pref_bool('06-15b',rad_end_marks_menu_entry.Checked)<>rad_end_marks_menu_entry.Checked then rad_end_marks_menu_entry.Click;
                if get_pref_bool('06-20b',timber_outlines_menu_entry.Checked)<>timber_outlines_menu_entry.Checked then timber_outlines_menu_entry.Click;
                if get_pref_bool('06-25b',outline_extensions_menu_entry.Checked)<>outline_extensions_menu_entry.Checked then outline_extensions_menu_entry.Click;
                if get_pref_bool('06-30b',timber_centres_menu_entry.Checked)<>timber_centres_menu_entry.Checked then timber_centres_menu_entry.Click;
                if get_pref_bool('06-35b',timber_numbers_menu_entry.Checked)<>timber_numbers_menu_entry.Checked then timber_numbers_menu_entry.Click;
                if get_pref_bool('06-40b',timbering_infill_menu_entry.Checked)<>timbering_infill_menu_entry.Checked then timbering_infill_menu_entry.Click;
                if get_pref_bool('06-45b',reduced_ends_menu_entry.Checked)<>reduced_ends_menu_entry.Checked then reduced_ends_menu_entry.Click;

                if get_pref_bool('06-50b',stock_rails_menu_entry.Checked)<>stock_rails_menu_entry.Checked then stock_rails_menu_entry.Click;
                if get_pref_bool('06-52b',crossing_rails_menu_entry.Checked)<>crossing_rails_menu_entry.Checked then crossing_rails_menu_entry.Click;
                if get_pref_bool('06-54b',check_rails_menu_entry.Checked)<>check_rails_menu_entry.Checked then check_rails_menu_entry.Click;

                if get_pref_bool('06-56b',overscale_joggles_menu_entry.Checked)<>overscale_joggles_menu_entry.Checked then overscale_joggles_menu_entry.Click;
                if get_pref_bool('06-58b',joint_marks_menu_entry.Checked)<>joint_marks_menu_entry.Checked then joint_marks_menu_entry.Click;
                if get_pref_bool('06-60b',guide_marks_menu_entry.Checked)<>guide_marks_menu_entry.Checked then guide_marks_menu_entry.Click;
                if get_pref_bool('06-62b',switch_drive_markx_menu_entry.Checked)<>switch_drive_markx_menu_entry.Checked then switch_drive_markx_menu_entry.Click;

                if get_pref_bool('06-65b',gen_platforms_trackbed_edges_menu_entry.Checked)<>gen_platforms_trackbed_edges_menu_entry.Checked then gen_platforms_trackbed_edges_menu_entry.Click;

                rail_prefs:=get_pref_int('06-80i',-1); // -1 init, rail prefs not present

                case rail_prefs of
                  0: both_edges_menu_entry.Click;
                  1: gauge_faces_only_menu_entry.Click;
                  2: outer_edges_only_menu_entry.Click;
                  3: rail_centrelines_only_menu_entry.Click;
                end;//case

              end;//with


              if get_pref_bool('06-00b',False)=True   // default to False if not present.
                 then begin
                        control_room_form.prefs_include_gen_settings_menu_entry.Checked:=True;  // radio item // so can save again.
                        if any_bgnd>0 then show_modal_message('Saved program preferences included generator settings.'+#13+#13+'Click the "generator > rebuild all background" menu item'+#13+'to apply the new settings to the background templates.');
                      end
                 else control_room_form.prefs_exclude_gen_settings_menu_entry.Checked:=True; // radio item



              //--------  help texts

                         help_font_style:=get_pref_int('07-02i',help_font_style);            // 0=normal  1=bold  215a
                          help_font_size:=get_pref_int('07-04i',help_font_size);             // px  215a

               help_client_width_as_user:=Round(get_pref_int('07-10i',Round(help_client_width_as_user/global_factor))*global_factor);   // user set sizes, adjust for any change in scaling
              help_client_height_as_user:=round(get_pref_int('07-20i',Round(help_client_height_as_user/global_factor))*global_factor);

              help_form.ClientWidth:=help_client_width_as_user;
              help_form.ClientHeight:=help_client_height_as_user;
              resize_help_form;


              //--------  print output
              print_corner_page_numbers_font.Assign(get_pref_font('08-0',print_corner_page_numbers_font));

              { OT-FIRST
              track_bgnd_width_in:=get_pref_float('08-10f',track_bgnd_width_in);    //206a
                 save_sb_track_bgnd:=get_pref_int('08-11i',save_sb_track_bgnd);     // colour 206a
                save_sb_diagram_col:=get_pref_int('08-12i',save_sb_diagram_col); }   // colour 209c

                 print_colour_prefs:=get_pref_int('08-15i',0); // 207a 0 = default colour printing

              case print_colour_prefs of
                0: begin grey_shade:=False; black_white:=False; pad_form.colour_printing_menu_entry.Checked:=True; end;           // radio item
                1: begin grey_shade:=True;  black_white:=False; pad_form.grey_shade_printing_menu_entry.Checked:=True; end;       // radio item
                2: begin grey_shade:=False; black_white:=True;  pad_form.black_and_white_printing_menu_entry.Checked:=True; end;  // radio item
              end;//case

              with pad_form.printed_grid_solid_menu_entry   do Checked:=get_pref_bool('08-17b',Checked); // 207a radio item
              with pad_form.printed_grid_dotted_menu_entry  do Checked:=get_pref_bool('08-18b',Checked); // 207a radio item

              with pad_form.thin_printed_lines_menu_entry   do Checked:=get_pref_bool('08-20b',Checked); // 207a radio item
              with pad_form.normal_printed_lines_menu_entry do Checked:=get_pref_bool('08-21b',Checked); // 207a radio item
              with pad_form.thick_printed_lines_menu_entry  do Checked:=get_pref_bool('08-22b',Checked); // 207a radio item
              with pad_form.set_line_thicknesses_menu_entry do Checked:=get_pref_bool('08-23b',Checked); // 207a radio item

              with pad_form.adjust_line_thickness_menu_entry do Checked:=get_pref_bool('08-25b',Checked);   // 207a

              printgrid_thick:=     get_pref_float('08-31f',printgrid_thick);             // mm 207a ...
              printmargin_thick:=   get_pref_float('08-32f',printmargin_thick);           // mm.
              printtimber_thick:=   get_pref_float('08-33f',printtimber_thick);           // mm.
              printrail_thick:=     get_pref_float('08-34f',printrail_thick);             // mm
              printmark_thick:=     get_pref_float('08-35f',printmark_thick);             // mm.
              printshape_thick:=    get_pref_float('08-36f',printshape_thick);            // mm.
              printpicborder_thick:=get_pref_float('08-37f',printpicborder_thick);        // mm.
              printcl_thick:=       get_pref_float('08-38f',printcl_thick);               // mm.

                       rail_infill_i:=get_pref_int('08-41i',rail_infill_i);
             print_timb_infill_style:=get_pref_int('08-42i',print_timb_infill_style);
         print_platform_infill_style:=get_pref_int('08-43i',print_platform_infill_style);

              with pad_form.timber_numbering_on_plain_track_menu_entry do Checked:=get_pref_bool('08-45b',Checked); // 208a


              //--------  sketchboard
              { OT-FIRST
              freehand_tracking:=get_pref_float('09-05f',freehand_tracking);

              with dtp_settings_form.design_quality_radiobutton  do Checked:=get_pref_bool('09-10b',Checked);    // radio items
              with dtp_settings_form.display_quality_radiobutton do Checked:=get_pref_bool('09-11b',Checked); }


              //--------  storage box
              with keep_form.briefly_hide_on_store_menu_entry do Checked:=get_pref_bool('10-00b',Checked); // 205e now radio item. 205c
              with keep_form.hide_on_store_menu_entry         do Checked:=get_pref_bool('10-01b',Checked); // 205e radio item
              with keep_form.dont_hide_on_store_menu_entry    do Checked:=get_pref_bool('10-02b',Checked); // 205e radio item
              with keep_form.reveal_on_store_menu_entry       do Checked:=get_pref_bool('10-03b',Checked); // 206a radio item
              with keep_form.alert_on_store_menu_entry        do Checked:=get_pref_bool('10-04b',Checked); // 206c radio item

              //--------  custom colours:     // 208a

              with colour_form.colour_dialog.CustomColors do begin
                Strings[0]:=get_pref_string('12-00s',Strings[0]);
                Strings[1]:=get_pref_string('12-01s',Strings[1]);
                Strings[2]:=get_pref_string('12-02s',Strings[2]);
                Strings[3]:=get_pref_string('12-03s',Strings[3]);
                Strings[4]:=get_pref_string('12-04s',Strings[4]);
                Strings[5]:=get_pref_string('12-05s',Strings[5]);
                Strings[6]:=get_pref_string('12-06s',Strings[6]);
                Strings[7]:=get_pref_string('12-07s',Strings[7]);
                Strings[8]:=get_pref_string('12-08s',Strings[8]);
                Strings[9]:=get_pref_string('12-09s',Strings[9]);
               Strings[10]:=get_pref_string('12-10s',Strings[10]);
               Strings[11]:=get_pref_string('12-11s',Strings[11]);
               Strings[12]:=get_pref_string('12-12s',Strings[12]);
               Strings[13]:=get_pref_string('12-13s',Strings[13]);
               Strings[14]:=get_pref_string('12-14s',Strings[14]);
               Strings[15]:=get_pref_string('12-15s',Strings[15]);
              end;//with


              //--------  output page origin:     // 208b

                       staggered_pages:=get_pref_bool('13-00b',staggered_pages);

              pad_form.pages_staggered_menu_entry.Checked:=staggered_pages;       // radio item.
              pad_form.pages_inline_menu_entry.Checked:= NOT staggered_pages;     // radio item.


               print_pages_top_origin:=get_pref_float('13-01f',print_pages_top_origin);
              print_pages_left_origin:=get_pref_float('13-02f',print_pages_left_origin);



               //--------  toolbars:                          // 217a

                        toolbars_2rows:=get_pref_bool('13-10b',toolbars_2rows);

                    make_slip_form.Left:=get_pref_int('13-12i',make_slip_form.Left);
                     make_slip_form.Top:=get_pref_int('13-13i',make_slip_form.Top);

              do_toolbars;


              //--------

            finally

              loading_his_prefs:=False;  // 208a

            end;//try
          end;

  if show_them=True then show_and_redraw(True,False); // no rollback.
end;
//______________________________________________________________________________

function prefs_quit:boolean;

          // come here after quit_alert to finally save preferences at quit
          // return False to cancel the quitting
var
  i:integer;

begin
  RESULT:=False;  // default init

  if user_prefs_in_use=False                    // no prefs loaded.
     then begin
            if (prefs_existed_on_startup=False) or (prefs_available=False)
               then RESULT:=True                // beginner user, not using prefs, or prefs since abandoned.

               else begin                       // not running with available prefs, ask about saving them now
                      repeat
                        i:=alert(4,'      save  program  preferences ?',
                                   '||Templot0 is about to quit.'
                                  +'||You are seeing this message because you started this Templot0 session without using your previously saved program preferences.'
                                  +'||Do you now want to save your current preferences ?'
                                  +'||If you answer yes, your previously saved preferences will be replaced if you have not changed the current preferences file name.'
                                  +'||If you choose to abandon your preferences, you won''t see the option to use your preferences when you start the next Templot0 session.'
                                  +'||If you are unsure, click the green bar.| ',
                                    '','more  options  and  information','abandon  my  saved  preferences','yes  -  save  my  current  preferences','cancel  quit   -   continue  running',
                                                    'no  -  quit  without  changing  my  saved  preferences   ',2);

                                         case i of
                                              2: control_room_form.saved_preferences_setup_menu_entry.Click;
                                              3: begin
                                                   RESULT:=True;
                                                   abandon_prefs(False);
                                                 end;
                                              4: begin
                                                   RESULT:=True;
                                                   save_prefs('',False);
                                                 end;
                                              5: EXIT;
                                              6: RESULT:=True;
                                         end;//case
                      until i<>2;
                    end;

          end
     else begin                // prefs were loaded, save them without asking.
            RESULT:=True;
            if (prefs_available=True) and (control_room_form.update_prefs_on_quit_menu_entry.Checked=True) then save_prefs('',False);
          end;
end;
//______________________________________________________________________________

procedure change_prefs_file(prefs_file_str:string); // update pointer file

        // set the prefs file name in the pointer file...
var
  prefs_txt:TextFile;

begin
  try
    AssignFile(prefs_txt,exe_str+prefs_pointer_str); // set the file name.
    Rewrite(prefs_txt);                              // reopen file.
    WriteLn(prefs_txt,prefs_file_str);               // write the file data -- new prefs file name
    WriteLn(prefs_txt,'Do not edit or delete this file.');
    CloseFile(prefs_txt);                            // and close the file.
  except
    on EInOutError do DeleteFile(exe_str+prefs_pointer_str);
  end;//try
end;
//______________________________________________________________________________


procedure abandon_prefs(show_dialog:boolean);

begin
  if FileExists(exe_str+prefs_pointer_str)
     then begin
            DeleteFile(exe_str+prefs_pointer_str);
            show_modal_message('Your saved preferences have been abandoned.');
          end
     else show_modal_message('Error - You have no saved preferences to abandon.');

  prefs_available:=False;

             // update dialog..

  if show_dialog=False then EXIT;

  if help_form.Showing=True
     then do_help(-200,prepare_prefs_html_str,False)
     else help(-200,prepare_prefs_html_str,'');
end;
//______________________________________________________________________________

procedure prefs_include_gen;

begin
  control_room_form.prefs_include_gen_settings_menu_entry.Checked:=True;  // radio item.

           // update dialog..

  if help_form.Showing=True
     then do_help(-200,prepare_prefs_html_str,False)
     else help(-200,prepare_prefs_html_str,'');
end;
//______________________________________________________________________________

procedure prefs_exclude_gen;

begin
  control_room_form.prefs_exclude_gen_settings_menu_entry.Checked:=True;  // radio item

           // update dialog..

  if help_form.Showing=True
     then do_help(-200,prepare_prefs_html_str,False)
     else help(-200,prepare_prefs_html_str,'');
end;
//______________________________________________________________________________

procedure change_prefs_file_and_load(load_file:boolean);

var
  str:string;
  prefs_file_name:string;

begin
  if load_file=True then prefs_form.Caption:='  reload  preferences  file ...'
                    else prefs_form.Caption:='  select  preferences  file ...';

  prefs_form.file_list_box.Directory:=exe_str;
  prefs_form.file_list_box.Update;                                // in case previously shown
  prefs_form.file_list_box.FileName:=get_prefs_file_name+'.sk1';  // show existing prefs file selected (otherwise FileName not initialised).

  try
    if do_show_modal(prefs_form)=mrOk      // 212a ShowModal
       then begin
              str:=ExtractFileName(prefs_form.file_list_box.FileName); // name part only wanted

              if str=''
                 then begin
                        show_modal_message('No file was selected - click a file in the list to select it.'+#13+#13+'Action cancelled.');
                        EXIT;
                      end;

              Delete(str,(Length(str)-3),4);                           // remove extension
              prefs_file_name:=str;
              if load_file=True then load_prefs(prefs_file_name,True,True,False);     // load file, show changes
              change_prefs_file(prefs_file_name); // update pointer file
            end;

  finally
         // update dialog..

    if help_form.Showing=True
       then do_help(-200,prepare_prefs_html_str,False)
       else help(-200,prepare_prefs_html_str,'');
    
  end;//try
end;
//______________________________________________________________________________

procedure new_prefs_file_and_save;

var
  prefs_file_name:string;

begin
  with math_form do begin
    Caption:='    create  new  preferences  file ...';
    big_label.Caption:=insert_crlf_str('|||Enter below a name for your new preferences file.'
                                      +'||Your program preferences will now be saved to and reloaded from this file, unless or until you change to a different preferences file.');

    math_editbox.Text:=get_prefs_file_name+'_new';   // default new name

    if do_show_modal(math_form)=mrOk      // 212a ShowModal
       then begin

              prefs_file_name:=lower_case_filename(remove_invalid_str(Trim(math_editbox.Text)));
              if prefs_file_name='' then prefs_file_name:=get_prefs_file_name+'_new';  // default new name

              Caption:='    '+Application.Title;   // reset form caption.

              save_prefs(prefs_file_name,False);
              change_prefs_file(prefs_file_name); // update pointer file
            end;
  end;//with

    // update dialog..

  if help_form.Showing=True
     then do_help(-200,prepare_prefs_html_str,False)
     else help(-200,prepare_prefs_html_str,'');

end;
//______________________________________________________________________________

procedure Tprefs_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=280;
  // OT-FIRST ClientHeight:=260;

  prefs_font:=TFont.Create;      // RESULT font for prefs.
end;
//______________________________________________________________________________

procedure Tprefs_form.delete_buttonClick(Sender: TObject);

var
  str:string;

begin
  str:=ExtractFileName(prefs_form.file_list_box.FileName); // name part only wanted
  Delete(str,(Length(str)-3),4);                           // remove extension

  if str=get_prefs_file_name
     then begin
            show_modal_message('The selected file is in current use and cannot be deleted.'+#13+#13+'Please first change to using a different current file.');
            EXIT;
          end;

  if FileExists(exe_str+str+'.sk1')=True
     then begin
            if alert(7,'      delete  program  preferences ?',
                         '||You are about to delete your saved program preferences in file: '+str
                        +'||OK?| ',
                          '','','','','no  -  cancel','yes  -  delete  preferences  file  '+str+'  ',0)=5 then EXIT;
            DeleteFile(exe_str+str+'.sk1');
            prefs_form.file_list_box.Update;
            prefs_form.file_list_box.FileName:=get_prefs_file_name+'.sk1';  // re-select existing prefs file (otherwise FileName returns empty on OK).
          end;
end;
//______________________________________________________________________________

procedure Tprefs_form.FormDestroy(Sender: TObject);

begin
  prefs_font.Free;      // RESULT font for prefs.
end;
//______________________________________________________________________________

procedure Tprefs_form.hidden_labelClick(Sender: TObject);

begin
  Clipboard.AsText:=current_load_str;
  Beep;
end;
//______________________________________________________________________________

procedure save_custom_gauges;      // 215a

var
  n,index:integer;
  cgs_list:TStringList;

begin
  cgs_list:=TStringList.Create;

  try
    with cgs_list do begin

      Clear;  // init

      for n:=0 to 3 do begin    // custom A to D ...

         //  '11-00' to '11-38'

        index:=gauge_form.gauge_listbox.Items.Count-5+n;

        Values['11-'+IntToStr(n)+'0s']:=prepare_string_for_prefs(gauge[index].name_str_glist);    // gauge designation: 9 chars max

        Values['11-'+IntToStr(n)+'1s']:=prepare_string_for_prefs(gauge_form.gauge_listbox.Items.Strings[index]);    // 215a    full line in selection list.

        Values['11-'+IntToStr(n)+'2f']:=FloatToStr(gauge[index].scale_glist);      // mm per ft.
        Values['11-'+IntToStr(n)+'3f']:=FloatToStr(gauge[index].gauge_glist);      // mm.
        Values['11-'+IntToStr(n)+'4f']:=FloatToStr(gauge[index].fw_glist);         // mm flangeway.
        Values['11-'+IntToStr(n)+'5f']:=FloatToStr(gauge[index].fwe_glist);        // mm flangeway end (flangeway+flare).
        Values['11-'+IntToStr(n)+'6f']:=FloatToStr(gauge[index].trtscent_glist);   // mm track centres, turnout side.
        Values['11-'+IntToStr(n)+'7f']:=FloatToStr(gauge[index].trmscent_glist);   // mm ditto, main side.
        Values['11-'+IntToStr(n)+'8f']:=FloatToStr(gauge[index].min_radius_glist); // mm minimum radius for check.

      end;//next

      SaveToFile(exe_str+'cgs.cgs');

    end;//with

  finally
    cgs_list.Free;
  end;//try

end;
//______________________________________________________________________________

function get_pref_string_cgs(list:TStringList; str:string; def_string:string):string;

var
  val_str:string;

begin
  RESULT:=def_string;  // default init.

  val_str:=list.Values[str];

  if val_str='' then EXIT;   // not in list

  val_str:=StringReplace(val_str,'%#%#%','=',[rfReplaceAll, rfIgnoreCase]);    // replace '=' in string

  RESULT:=StringReplace(val_str,str_prefix,'',[rfReplaceAll, rfIgnoreCase]);   // remove string prefix
end;
//______________________________________________________________________________

function get_pref_float_cgs(list:TStringList; str:string; def_float:extended):extended;

var
  val_str:string;
  temp_float:extended;

begin
  RESULT:=def_float;  // default init.

  val_str:=list.Values[str];

  if val_str='' then EXIT;   // not in list

  try
    temp_float:=StrToFloat(val_str);
  except
    EXIT;   // invalid value
  end;//try

  RESULT:=temp_float;  // return result
end;
//______________________________________________________________________________

procedure get_custom_gauges;

var
  n,index:integer;
  cgs_list:TStringList;

begin
  if FileExists(exe_str+'cgs.cgs')=False then EXIT;

  cgs_list:=TStringList.Create;

  try
    with cgs_list do begin

      Clear;  // init

      LoadFromFile(exe_str+'cgs.cgs');

      for n:=0 to 3 do begin    // custom A to D ...

          //  '11-00' to '11-38'

        index:=gauge_form.gauge_listbox.Items.Count-5+n;

        with gauge[index] do begin

           name_str_glist:=get_pref_string_cgs(cgs_list,'11-'+IntToStr(n)+'0s',name_str_glist);   // gauge designation: 9 chars

         gauge_form.gauge_listbox.Items.Strings[index]:=get_pref_string_cgs(cgs_list,'11-'+IntToStr(n)+'1s',gauge_form.gauge_listbox.Items.Strings[index]);   // 215a   full line in selector list.

               scale_glist:=get_pref_float_cgs(cgs_list,'11-'+IntToStr(n)+'2f',scale_glist);      // mm per ft.
               gauge_glist:=get_pref_float_cgs(cgs_list,'11-'+IntToStr(n)+'3f',gauge_glist);      // mm.
                  fw_glist:=get_pref_float_cgs(cgs_list,'11-'+IntToStr(n)+'4f',fw_glist);         // mm flangeway.
                 fwe_glist:=get_pref_float_cgs(cgs_list,'11-'+IntToStr(n)+'5f',fwe_glist);        // mm flangeway end (flangeway+flare).
            trtscent_glist:=get_pref_float_cgs(cgs_list,'11-'+IntToStr(n)+'6f',trtscent_glist);   // mm track centres, turnout side.
            trmscent_glist:=get_pref_float_cgs(cgs_list,'11-'+IntToStr(n)+'7f',trmscent_glist);   // mm ditto, main side.
          min_radius_glist:=get_pref_float_cgs(cgs_list,'11-'+IntToStr(n)+'8f',min_radius_glist); // mm minimum radius for check.

        end;//with

      end;//next

    end;//with

  finally
    cgs_list.Free;
  end;//try

end;
//______________________________________________________________________________


end.

