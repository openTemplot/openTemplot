
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

unit gauge_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls
  , LCLtype;     // OT-FIRST

type
  Tgauge_form = class(TForm)
    ok_button: TButton;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    chat_panel: TPanel;
    exact_scale_button: TButton;
    ok_large_panel: TPanel;
    custom_groupbox: TGroupBox;
    custom_a_button: TButton;
    custom_b_button: TButton;
    custom_c_button: TButton;
    custom_d_button: TButton;
    help_button: TButton;
    colour_panel: TPanel;
    colour_patch: TImage;
    size_updown: TUpDown;
    datestamp_label: TLabel;
    gauge_panel: TPanel;
    gauge_header_label: TLabel;
    gauge_listbox: TListBox;
    warning_panel: TPanel;
    cancel_panel: TPanel;
    cancel_button: TButton;
    group_button: TButton;
    match_original_radio_button: TRadioButton;
    mint_new_radio_button: TRadioButton;
    retain_length_checkbox: TCheckBox;
    info_header_label: TLabel;
    print_button: TButton;
    bold_panel: TPanel;
    info_scrollbox: TScrollBox;
    info_listbox: TListBox;
    copy_button: TButton;
    chat_button: TButton;
    ok_panel: TPanel;
    procedure how_panelClick(Sender: TObject);
    procedure ok_buttonClick(Sender: TObject);
    procedure gauge_listboxClick(Sender: TObject);
    procedure cancel_buttonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure gauge_listboxDblClick(Sender: TObject);
    procedure chat_panelClick(Sender: TObject);
    procedure exact_scale_buttonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure colour_patchClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure custom_a_buttonClick(Sender: TObject);
    procedure custom_b_buttonClick(Sender: TObject);
    procedure custom_c_buttonClick(Sender: TObject);
    procedure custom_d_buttonClick(Sender: TObject);
    procedure print_buttonClick(Sender: TObject);
    procedure copy_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure info_listboxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure group_buttonClick(Sender: TObject);
    procedure match_original_radio_buttonClick(Sender: TObject);
    procedure mint_new_radio_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  gauge_form: Tgauge_form;
  //----------------------

  t_T55_i:integer=0;              //  index = T-55 gauge.
  gauge_i:integer=0;

  t_TT3_i:integer=0;
  t_TTI_i:integer=0;
  t_TTF_i:integer=0;
  t_TMS_i:integer=0;
  t_TMF_i:integer=0;

  t_H0US_i:integer=0;
  t_H0EU_i:integer=0;

  t_00SF_i:integer=0;
  t_00BF_i:integer=0;
  t_00H0_i:integer=0;
  t_00DGF_i:integer=0;
  t_00DGI_i:integer=0;
  t_00BRMSB_i:integer=0;

  t_EM_i:integer=0;
  t_S4P4_i:integer=0;
  t_0SF_i:integer=0;  // 0.79.a
  t_GOGF_i:integer=0;
  t_S7_i:integer=0;


    // added in 0.93.a ...

  t_N_NMRA_i:integer=0;
  t_S2_i:integer=0;
  t_N_UK_i:integer=0;
  t_S3p5_i:integer=0;
  t_S_gauge_i:integer=0;
  t_0MF_i:integer=0;
  t_1F_i:integer=0;


  t_P32_i:integer=0;  // added in 212a

  //gauge_str:string;

  show_gauge_details:boolean=False;

  procedure init_gauge_list;          //  fill all gauge list data


//_____________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  Printers, Clipbrd, control_room, pad_unit, entry_sheet, help_sheet, chat_unit, math_unit, alert_unit,
  colour_unit, keep_select, info_unit, print_unit, shove_timber, print_settings_unit;

const

  scale_help_str:string='     Scale'
  +'||Templot0 is optimised for model railway applications, using the hybrid scales based on millimetres per foot (mm/ft).'
  +'||Track dimensions which are usually modelled exactly to scale, for example rail lengths, are normally entered in INCHES full-size on the prototype.'
  +'||Track dimensions which are commonly modelled to a non-scale size, for example the track gauge, are normally entered in actual MILLIMETRES on the model.'
  +'||For the purpose of printing construction templates and track plans, a printed size of 100% means 100% of this model size.'

  +'||The maximum scale Templot0 will accept is 500 mm/ft or 1:0.6 (which is nearly twice full-size!).'
  +'||The minumum acceptable scale is 0.1 mm/ft or 1:3048.'
  +'||For engineering applications the scale should be entered as a ratio, for example 1:16 or 1:50.';

  gauge_help_str:string='      Gauge / Scale  Selection  List'
        +'||Scroll the list and click on your required combination of scale, track gauge and flangeway standards.'
        +'||Then click the green OK bar or press the keyboard ENTER key to change the control template to the new setting.'
        +'||Alternatively, double-click on the list.'

        +'||To convert a selected group of background templates to the new gauge and scale setting, click the CONVERT GROUP button. This function ignores the MINT option box (see below).'

        +'||Click the SHOW INFO button to see full details of the dimensions which apply to the currently selected gauge.'
        +'||Click SET EXACT SCALE... if you want the track gauge, check-rail and flangeway dimensions'
        +' to be scaled exactly from British full-size practice (bull-head rail).'

        +'  Or click one of the SET CUSTOM... buttons to enter your own custom dimensions.'
        +' You can have up to 4 different custom settings available for use. These will appear at the bottom of the list.'

        +'||If the gauge which you are using has not yet been implemented in this upgrade version of Templot, please'
        +' enter your requirements as a custom setting instead. More information is available by clicking the chat (=) button.'

        +'||If the MINT NEW option box is selected, the new control template at the new gauge and scale will be a mint template. If the MATCH ORIGINAL option box is selected,'
        +' Templot0 will match the new control template at the new gauge and scale to the size and position of the original control template.'
        +'||These option settings also apply when using the TEMPLATE > GAUGE AND SCALE menu items, and can also be changed using the TEMPLATE > GAUGE AND SCALE > MINT NEW and MATCH ORIGINAL menu options.'
        +'||For an explanation of a mint template, select the TEMPLATE > QUICK SET... menu item and then click the ?HELP button on the QUICK SET window.'

        +'||If you close the form or click the CANCEL button the previous gauge and scale settings'
        +' will remain unchanged.'

        +'||When this form first appears, it will show the gauge/scale setting corresponding to the control template on the trackpad. If there are any differences between the dimensions in use for the control template'
        +' and the standard dimensions for this gauge/scale setting, a red warning label "N.B. Dimensions now in use differ" will appear above the list. Then clicking the SHOW INFO button will show the list of standard dimensions'
        +' for this gauge/scale setting, and any which curently differ will be shown in RED.'
        +'||These dimensions can also be checked by selecting the TEMPLATE > GAUGE AND SCALE > DETAILS... menu item.'

        +'||Handy Hint:'
        +'|After reloading templates from a data file which you have not saved yourself (e.g. a downloaded file or one from another Templot0 user) it is sensible to check for any dimension differences before continuing to use the templates'
        +' for your own designs (click the TEMPLATE > GAUGE AND SCALE > DETAILS... menu item).'
        +' To restore the control template to the standard dimensions for this gauge and scale setting, simply click the green OK bar. To continue using the modified dimensions, close the form or click CANCEL.';

  sorry_str:string='| Sorry, the preset dimensions for this gauge have not yet| been implemented.'
                  +'|| Until then, please click one of the SET CUSTOM buttons| and enter your required dimensions as a custom setting.';

type
  Tdiff_flags=record
                flags:   array[0..6] of boolean;     // flag False if current value differs from list
                cur_vals:array[0..6] of extended;    // current value in use.
              end;//record

var
  gauge_select_i:integer;
  no_onresize:boolean=False;
  diff_flags:Tdiff_flags;

  procedure gauge_list_click;forward;
  procedure show_gauge_info;forward;
  procedure chat_click;forward;
  procedure custom_click(custom:integer);forward;
  procedure init_gauge_defaults(index:integer);forward;
  procedure g_defaults(scale,inscale:extended; var gs:Tgauge_scale);forward;    // set all g-defaults

//______________________________________________________________________________

procedure Tgauge_form.how_panelClick(Sender: TObject);

begin
 if help(-1,gauge_help_str,'chat  -  more  information')=1 then chat_click;
end;
//____________________________________________________________________________________________

procedure group_scale_change;        // change a selected group to a new scale/gauge setting.

var
  save_current:Ttemplate_info;
  n,count:integer;
  bgnd:integer;
  save_bgnd_option:boolean;
  mod_ratio:extended;
  save_name:string;
  index:integer;

begin
  if keeps_list.Count<1 then EXIT;
  index:=gauge_form.gauge_listbox.ItemIndex;  // may get changed if the form is re-activated after help or alerts.

  cancel_adjusts(False);
  if any_selected=0
     then begin
            if alert_no_group=True    // alert him, and does he want all?
               then EXIT
               else begin
                      do_rollback:=False;
                      redraw(False);           //  to show newly selected all.
                    end;
          end;

  gauge_form.gauge_listbox.ItemIndex:=index;    // may have been changed if the form is re-activated after help or alerts.
  gauge_list_click;

  if alert(7,'    convert  group  to  '+Trim(gauge_form.ok_large_panel.Caption),
             'You are about to change the gauge/scale setting to '+Trim(gauge_form.ok_large_panel.Caption)+' for all the templates in the currently selected group.'
            +'||If these templates may be needed again they should first be saved (cancel this and click the SAVE GROUP button in the Storage Box).'
            +'||This CONVERT GROUP function does not change the control template to the new gauge/scale setting. To do that, click the OK button when this function finishes.'
            +' To leave the control template unchanged, click the CANCEL button when this function finishes.'
            +'||This CONVERT GROUP function ignores the MINT tickbox.'
            +'||This function may take some time to complete.',
             '','','','','cancel','O K    -    continue',0)=5 then EXIT;

  gauge_form.gauge_listbox.ItemIndex:=index;    // may have been changed if the form is re-activated after help or alerts.
  gauge_list_click;

  keep_form.Close;    // (might only be behind the pad) so the store current button works without alerts.

  save_bgnd_option:=pad_form.show_bgnd_keeps_menu_entry.Checked;   // don't want a complete redraw for every template.
  pad_form.hide_bgnd_keeps_menu_entry.Checked:=True;               // so switch bgnd off radio item.

  count:=keeps_list.Count;

  save_current.keep_shove_list:=TStringList.Create;   // local stringlist not initialised.
  fill_kd(save_current);
  save_name:=current_name_str;

  try
    Screen.Cursor:=crHourglass;
    n:=0;

    while n<count do begin

      if Ttemplate(keeps_list.Objects[n]).group_selected=False
         then begin
                INC(n);
                CONTINUE;     // don't change this one.
              end;

      bgnd:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.bgnd_code_077;  // remember if it's on bgnd.

      list_position:=n;
      copy_keep_to_current(False,False,False,False);    // copy to current.
      gocalc(0,0);      //redraw(False);                // for notch data.

      pad_form.notch_under_peg_menu_entry.Click;  // so can put the new template back on it.

      gauge_i:=gauge_select_i;               //  set new gauge index,
      mod_ratio:=gauge_dims(True,True,True); //  and get new dimensions.

      set_y_datum;                         //  need to change the pad datum.
      gocalc(0,0);    //redraw(False);     //  force immediate redraw.

      rescale_notch(mod_ratio);
      shift_onto_notch(False,False);

      store_unused(False,False);    // (does a recalc) put back in.

      if bgnd=1 then keep_form.copy_or_wipe_background_button.Click;    // and on background.

      INC(n);
    end;//while

                     // now delete the original group...
    n:=0;
    while n<keeps_list.Count do begin

      if Ttemplate(keeps_list.Objects[n]).group_selected=False
         then begin
                INC(n);
                CONTINUE;  // leave this one.
              end;

      list_position:=n;
      delete_keep(False, False);   // delete this one

    end;//while              // no need to increment n, it is now pointing to the next keep.

    save_done:=False;
    backup_wanted:=True;

    pad_form.show_bgnd_keeps_menu_entry.Checked:=save_bgnd_option;   // restore, radio item.

  finally
    copy_keep(save_current);        // restore control template...
    current_name_str:=save_name;
    info_form.ref_name_label.Caption:=current_name_str;

    pad_form.show_bgnd_keeps_menu_entry.Checked:=save_bgnd_option;   // restore, radio item.

    free_shove_list(save_current.keep_shove_list);   // free the local stringlist.

    Screen.Cursor:=crDefault;
    do_rollback:=False;
    redraw(True);
  end;//try
end;
//__________________________________________________________________________________________

procedure ok_click(change_group:boolean);

var
  mod_ratio:extended;
  custom_gauge,not_mint:boolean;

begin
  with gauge_form do begin
    if (gauge_select_i<(gauge_listbox.Items.Count-5) ) and (gauge[gauge_select_i].scale_glist=0)
       then begin
              if alert(2,'    '+gauge[gauge_select_i].name_str_glist+'  not  yet  implemented',
                         ok_large_panel.Caption+sorry_str,
                         '','','','','cancel  and  continue','chat  -  more  information',0)=6 then chat_click;
              EXIT;// form remains showing
            end;

    if ( gauge_select_i<(gauge_listbox.Items.Count-1) ) and (gauge[gauge_select_i].scale_glist=0)
       then begin
              if alert(2,'                         '+gauge[gauge_select_i].name_str_glist,
                         '|No dimensions for this custom setting have yet been entered.'
                        +'||Click below or the appropriate `0SET CUSTOM`1 button to enter your required dimensions.'
                        ,'','','','','cancel','enter '+gauge[gauge_select_i].name_str_glist+'  data',0)=6
                 then custom_click(gauge_select_i-(gauge_listbox.Items.Count-5));
              EXIT;// form remains showing
            end;

    if (gauge_select_i=(gauge_listbox.Items.Count-1) ) and (gauge[gauge_select_i].scale_glist=0)
       then begin
              if alert(2,'                         exact  scale',
                         '|No scale has yet been entered for this setting.'
                        +'||Click below or the `0SET EXACT SCALE`1 button to enter the required scale.'
                        ,'','','','','cancel','enter  EXACT  SCALE  data',0)=6
                 then exact_scale_button.Click;
              EXIT; // form remains showing
            end;

    if (gauge_select_i<(gauge_listbox.Items.Count-1)) and (gauge_select_i>(gauge_listbox.Items.Count-6))
       then custom_gauge:=True
       else custom_gauge:=False;

  end;//with form

  if change_group=True
     then group_scale_change
     else begin
            not_mint:=NOT gauge_form.mint_new_radio_button.Checked;

            if not_mint=True then pad_form.notch_under_peg_menu_entry.Click;  // so can put the new template back on it.

            gauge_i:=gauge_select_i;                      //  set new gauge index,
            mod_ratio:=gauge_dims(True,True,not_mint);    //  and get new dimensions.

            gauge_form.Close;

            set_y_datum;                 //  need to change the pad datum.
            gocalc(0,0);

            if not_mint=True
               then begin
                      rescale_notch(mod_ratio);
                      shift_onto_notch(False,False);
                    end
               else begin
                      if gauge_form.retain_length_checkbox.Checked=True   // 208d test added, was (0)
                         then mint_new_current(1)
                         else mint_new_current(0);
                    end;

            redraw(True);
          end;
end;
//_______________________________________________________________________________________

procedure Tgauge_form.ok_buttonClick(Sender: TObject);

begin
  ok_click(False);
end;
//_________________________________________________________________________________________

procedure Tgauge_form.group_buttonClick(Sender: TObject);

begin
  ok_click(True);
end;
//________________________________________________________________________________________

procedure Tgauge_form.gauge_listboxClick(Sender: TObject);

begin
  gauge_list_click;
end;
//_____________________________________________________________________________________

procedure gauge_list_click;

begin
  with gauge_form do begin
    if gauge_form.Showing=True then gauge_listbox.SetFocus;  // in case come here from data entry (CUSTOM / EXACT).
    gauge_select_i:=gauge_listbox.ItemIndex;

    ok_panel.Caption:='   '+Copy(gauge_listbox.Items.Strings[gauge_select_i],11,200);   //  215a

    ok_large_panel.Caption:=gauge[gauge_select_i].name_str_glist;

    if gauge_select_i<>gauge_i then warning_panel.Visible:=False;

    show_gauge_info;

  end;//with
end;
//________________________________________________________________________________________
procedure Tgauge_form.cancel_buttonClick(Sender: TObject);

begin
  Close;
end;
//_________________________________________________________________________________________

procedure Tgauge_form.FormActivate(Sender: TObject);

var
  i,n:integer;
  same:boolean;

begin

      // set the index into the gauge list...
      // use this only to show the correct line in the list.
      // actual data comes from the file.

      // mods 212a - index from the gauge name part only...

  gauge_i:=0; //init

  for n:=0 to gauge_form.gauge_listbox.Items.Count-1 do begin

    if gauge[n].name_str_glist=cpi.name_str_pi
       then begin
              gauge_i:=n;
              BREAK;
            end;
  end;//next

  if Tag=0            // only when first called
     then begin
                      // compare 2 gauge settings.
                      // return array of True or False flags and current values for offending items if not same.

            same:=True;  //init.
            Tag:=1;
            with gauge[gauge_i] do begin
              with cpi do begin


                with diff_flags do begin

                  for i:=0 to High(flags) do begin
                    flags[i]:=True;                           // default inits.
                    cur_vals[i]:=0;
                  end;//for

                  if ABS(scale_pi-scale_glist)>minfp           then begin  flags[0]:=False;  cur_vals[0]:=scale_pi;      same:=False; end;  // mm/ft.
                  if ABS(gauge_pi-gauge_glist)>minfp           then begin  flags[1]:=False;  cur_vals[1]:=gauge_pi;      same:=False; end;  // mm.
                  if ABS(fw_pi-fw_glist)>minfp                 then begin  flags[2]:=False;  cur_vals[2]:=fw_pi;         same:=False; end;  // mm flangeway.
                  if ABS(fwe_pi-fwe_glist)>minfp               then begin  flags[3]:=False;  cur_vals[3]:=fwe_pi;        same:=False; end;  // mm flangeway end (flangeway+flare).
                  if ABS(trtscent_pi-trtscent_glist)>minfp     then begin  flags[4]:=False;  cur_vals[4]:=trtscent_pi;   same:=False; end;  // mm track centres, turnout side.
                  if ABS(trmscent_pi-trmscent_glist)>minfp     then begin  flags[5]:=False;  cur_vals[5]:=trmscent_pi;   same:=False; end;  // mm ditto, main side.
                  if ABS(min_radius_pi-min_radius_glist)>minfp then begin  flags[6]:=False;  cur_vals[6]:=min_radius_pi; same:=False; end;  // mm minimum radius for check.

                end;//with diff_flags
              end;//with
            end;//with

            if same=True
               then warning_panel.Hide                       // same.
               else warning_panel.Show;                      // different.
          end
     else warning_panel.Hide;

  gauge_listbox.ItemIndex:=gauge_i;            //  initialise selection
  gauge_list_click;

  show_gauge_info;

end;
//________________________________________________________________________________________

procedure Tgauge_form.gauge_listboxDblClick(Sender: TObject);

begin                                         // double click on list.
  gauge_list_click;
  gauge_form.ok_button.Click;
end;
//______________________________________________________________________________________

procedure chat_click;

const
  chat_str:string='    Gauge  and  Scale  Selection  List'
      +'||I have tried to make this list as comprehensive as possible, but it has not been possible to include any narrow-gauge settings because there are just too many variations.'
      +' Narrow-gauge templates can be created as custom settings.'
      +'||If you are using a gauge or scale combination for standard-gauge not shown here I would be pleased to know about it, and the various dimensions which you have adopted.'
      +' In the meantime you can enter them as a custom setting.'

      +'||If you can help to fill in any obvious gaps in this list I would be pleased to hear from you. The information needed is:'
      +'||  gauge (minimum) and scale'
      +'|  nominal flangeway gap and check rail flare-out (end gap)'
      +'|  minimum double-track centres'
      +'|  recommended minimum radius'
      +'||The gauge also needs a NAME. I have had to invent some of these because there seems to be no established name in use.'
      +'||Please note that for the EXACT scale setting the track gauge is assumed to be the traditional 56.5 inches.'
      +' Some tracks in recent years have been set to 1432 mm gauge (about 3 mm less) - if you need this please enter your dimensions as a custom setting.'
      +'||Engineers coming across this list for the first time will no doubt be amazed'
      +' at the weird scale ratios and mixture of metric and imperial dimensions that we go in for! All the gauges listed have been'
      +' used by someone somewhere or referred to in print (if the internet counts as print).'
      +'||Except, of course, the T-55 Templot0 Startup gauge which is 5.5 mm/ft on 1" gauge with EM clearances. This is intended to be'
      +' 100% fictitious - unless you know different !';
begin
  chat(chat_str);
end;
//_______________________________________________________________________________________

procedure Tgauge_form.chat_panelClick(Sender: TObject);

begin
  chat_click;
end;
//_______________________________________________________________________________________

procedure Tgauge_form.exact_scale_buttonClick(Sender: TObject);

const
  help_str:string='      Exact  Scale'
           +'||This scale size will be used whenever you subsequently select EXACT SCALE on the gauge list.'
           +' The track gauge, flangeways and all other pointwork dimensions will then be scaled'
           +' exactly from full-size British (bullhead) practice.'
           +'||The figures which you enter here have no relevance if any other gauge/scale combination is subsequently selected from the list.'
           +'||If you are using a dead scale gauge (e.g. S4, ScaleSeven, etc.) you should select the appropriate size from the list, as there'
           +' are slight variations in the standards to accommodate manufacturing tolerances ( although these are likely to show up only if you'
           +' are printing at an enlarged scale on a very-high-resolution printer ).'
           +'||The maximum scale Templot0 will accept is 500 mm/ft or 1:0.6 (which is nearly twice full-size!). The minimum scale is 0.1 mm/ft or 1:3048.'
           +'||When you save and reload EXACT SCALE templates via your storage box, the exact scale settings for each one will be preserved, but will not'
           +' be reflected here in the gauge list. Once you have such an EXACT SCALE control template on the trackpad, its settings will continue to be used for'
           +' any subsequent templates until you come here and select another gauge size, or copy a template from the background or storage box, so there is no need to re-enter the EXACT SCALE setting for them unless'
           +' you want to change the scale, or swap between different scales. The settings from an EXACT SCALE template can also be adopted into to one of the'
           +' CUSTOM slots if you are not using them for other custom settings.';

var
  n, i:integer;
  sc,ins:extended;
  od:Toutdim;
  dummy_i:integer;

begin
  repeat
    i:=alert(4,'           exact  scale ...',
               'The scale size for the EXACT SCALE setting can be entered in mm/ft, or as a scale ratio.'
              +'||Please select your preference.',
               '','','help','scale  ratio  1 : n','cancel','scale  in  mm / ft',3);
    case i of
        3: alert_help(0,scale_help_str,'');

        4: begin
             n:=putdim(help_str,0,'EXACT  SCALE  ratio  1 :',304.8/scale,True,True,True,False);  // no negative, no pre-set, no zero, don't terminate on zero.
             if n<>0 then EXIT;
             if getdims('exact  scale  setting',scale_help_str,gauge_form,n,od)=True
                then begin
                        gauge_select_i:=gauge_listbox.Items.Count-1;
                        gauge_i:=gauge_select_i;

                        init_gauge_defaults(gauge_i);                    // first clear any previous exact setting.
                        gauge[gauge_i].scale_glist:=ABS(304.8/od[0]);    //  1 foot = 304.8 mm
                      end
                 else EXIT;
           end;

        5: EXIT;

        6: begin
              n:=putdim(help_str,1,'EXACT  SCALE  in  mm / ft',scale,True,True,True,False);  // no negative, no pre-set, no zero, don't terminate on zero.
              if n<>0 then EXIT;
              if getdims('exact  scale  setting',scale_help_str,gauge_form,n,od)=True
                 then begin
                        gauge_select_i:=gauge_listbox.Items.Count-1;
                        gauge_i:=gauge_select_i;

                        init_gauge_defaults(gauge_i);              // first clear any previous exact setting.
                        gauge[gauge_i].scale_glist:=ABS(od[0]);    //  mm/ft
                      end
                 else EXIT;
           end;
      else EXIT;
    end;//case

  until i<>3;
  gauge[gauge_i].scale_glist:=limits(0.1,500,gauge[gauge_i].scale_glist,dummy_i);  // maximum scale 500 mm to the foot (nearly twice full-size).


  sc:=gauge[gauge_i].scale_glist;       // scale
  ins:=sc/12.0;                         // inscale
  g_defaults(sc,ins,gauge[gauge_i]);    // set all the defaults.

  gauge_listbox.ItemIndex:=gauge_i;
  gauge_list_click;

  if gauge_form.Showing=True then ok_button.SetFocus;   // 0.93.a

end;
//______________________________________________________________________________________

procedure Tgauge_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key=VK_F10
     then begin
            Key:=0;      //  otherwise selects the menus.
          end;

  if Key=VK_PAUSE then Application.Minimize;   //  hide TEMPLOT on PAUSE key.
end;
//_________________________________________________________________________________________

procedure Tgauge_form.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if (Key=Chr(13)) and (gauge_listbox.Focused=True) then ok_button.Click; //  ENTER key clicks OK.
end;
//______________________________________________________________________________________

procedure Tgauge_form.colour_patchClick(Sender: TObject);

begin
    gauge_listbox.Color:=get_colour('choose  a  new  colour  for  the  gauge  list',gauge_listbox.Color);
end;
//____________________________________________________________________________________________

procedure Tgauge_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  no_onresize:=True;                            // don't permit on-resize until finished.

  if size_updown.Position>size_updown.Tag                          // ! position goes up, size goes down.
     then begin
            ScaleBy(9,10);                                         // scale the form contents down.

          end;

  if size_updown.Position<size_updown.Tag
     then begin
            ScaleBy(10,9);                                         // scale the form contents up.
          end;

  ClientHeight:=VertScrollBar.Range;                               // allow 4 pixel right margin.
  ClientWidth:=HorzScrollBar.Range+4;                              // don't need bottom margin - datestamp label provides this.
  ClientHeight:=VertScrollBar.Range;                               // do this twice, as each affects the other.

  size_updown.Tag:=size_updown.Position;                           // and save for the next click.

  no_onresize:=False;
  Resize;
end;
//__________________________________________________________________________________________

procedure g_defaults(scale,inscale:extended; var gs:Tgauge_scale);    // set all glist-defaults

begin
  with gs do begin

    if gauge_glist=def_req then gauge_glist:=56.5*inscale;        // default 4' 8.5" gauge.
    if fw_glist=def_req then fw_glist:=1.75*inscale;              // default 1.75" dead-scale flangeway.
    if fwe_glist=def_req then fwe_glist:=fw_glist+1.75*inscale;   // default +1.75" flare.
    if trtscent_glist=def_req then trtscent_glist:=134*inscale;   // 11' 2" track centres turnout-side
    if trmscent_glist=def_req then trmscent_glist:=134*inscale;   // ditto main-side
    if min_radius_glist=def_req then min_radius_glist:=152*scale; //  152 ft. minimum radius warning.
                                                                  //  = 608 mm (24" approx) in 4mm scale.
                                                                  //  = 1064 mm (42" approx) in 7 mm scale.
  end;//with
end;
//________________________________________________________________________________________

procedure init_gauge_defaults(index:integer);

begin
  with gauge[index] do begin
         scale_glist:=0;         // scale 0 means this gauge not yet implemented.

           // initialise the rest of the list to defaults...

      name_str_glist:='';

        gauge_glist:=def_req;       // mm.
           fw_glist:=def_req;       // mm flangeway.
          fwe_glist:=def_req;       // mm flangeway end gap (flangeway+flare).

      old_fwe_glist:=def_req;       // pre 215a    modified after setting up the list

     trtscent_glist:=def_req;       // mm track centres, turnout side.
     trmscent_glist:=def_req;       // mm ditto, main side.
    min_radius_glist:=def_req;      // mm minimum radius for check.
  end;//with
end;
//_________________________________________________________________________________________

procedure init_gauge_list;                     //  fill all gauge list data

var
  i,j:integer;
  sc,ins:extended;
  listbox_str:string;
  temp_str:string;

begin
  gauge_form.gauge_listbox.Items.Clear;  // 215a

  for i:=0 to gauge_indexmax_c do init_gauge_defaults(i);   // first fill with defaults.

                 // then set specifics as reqd...
  i:=0;

           listbox_str:=' (n/a)                                       (none)                 ';

                                gauge_form.gauge_listbox.Items.Add(listbox_str);   Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' Z-NMRA    6.5  mm    1.385mm/ft  1:220     Z  NMRA and Märklin     ';
           scale_glist:=304.8/220;    // mm per ft.
           gauge_glist:=6.5;          // mm.
           fw_glist:=0.64;            // mm flangeway.
           old_fwe_glist:=1.25;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=20;        // mm track centres, turnout side.
           trmscent_glist:=20;        // mm ditto, main side.
           //retcent_glist:=20;         // mm ditto, return curve.
           min_radius_glist:=225;     // mm minimum radius for check (9" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' P-220     6.52 mm    1.385mm/ft  1:220     Proto-220               ';
           scale_glist:=304.8/220;    // mm per ft.
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' Z-UK      6.5  mm    1.5 mm/ft   1:203.2   Z  UK only              ';
           scale_glist:=1.5;          // mm per ft.
           gauge_glist:=6.5;          // mm.
           fw_glist:=0.65;            // mm flangeway.
           old_fwe_glist:=1.25;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=20;        // mm track centres, turnout side.
           trmscent_glist:=20;        // mm ditto, main side.
           //retcent_glist:=20;         // mm ditto, return curve.
           min_radius_glist:=225;     // mm minimum radius for check (9" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S1.5      7.06 mm    1.5 mm/ft   1:203.2   1.5mm Fine              ';
           scale_glist:=1.5;          // mm per ft.
           gauge_glist:=7.0625;       // mm.
           fw_glist:=0.35;            // mm flangeway.
           old_fwe_glist:=0.6;            // mm flangeway ends.
           min_radius_glist:=375;     // mm minimum radius for warning (15" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' P-160     8.97 mm    1.905mm/ft  1:160     Proto-160               ';
           scale_glist:=304.8/160;    // mm per ft.
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

                   // added 211b (Jim Guthrie request)..

  with gauge[i] do begin
           listbox_str:=' FS160     9.0  mm    1.905mm/ft  1:160     fiNe-scale  2FS dims    ';
           scale_glist:=304.8/160;    // mm per ft.
           gauge_glist:=9.0;          // mm.
           fw_glist:=0.5;             // mm flangeway.
           old_fwe_glist:=0.85;           // mm flangeway ends.
           min_radius_glist:=500;     // mm minimum radius for warning (20" approx).

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' N-NMRA    9.0  mm    0.075"/ft   1:160     N  USA and Europe       ';
           scale_glist:=1.905;        // mm per ft.
           gauge_glist:=9.0;          // mm.
           fw_glist:=0.85;            // mm flangeway.
           old_fwe_glist:=1.65;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=25;        // mm track centres, turnout side.
           trmscent_glist:=25;        // mm ditto, main side.
           //retcent_glist:=25;         // mm ditto, return curve.
           min_radius_glist:=300;     // mm minimum radius for check (12" approx).

           t_N_NMRA_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S2        9.42 mm    2 mm/ft     1:152.4   2FS 2mm Fine            ';
           scale_glist:=2;            // mm per ft.
           gauge_glist:=9.42;         // mm.
           fw_glist:=0.5;             // mm flangeway.
           old_fwe_glist:=0.85;           // mm flangeway ends.
           min_radius_glist:=500;     // mm minimum radius for warning (20" approx).

           t_S2_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S2(I)    10.5  mm    2 mm/ft     1:152.4   Irish 5''3" S2 dims     ';
           scale_glist:=2;            // mm per ft.
           gauge_glist:=10.5;         // mm.
           fw_glist:=0.5;             // mm flangeway.
           old_fwe_glist:=0.85;           // mm flangeway ends.
           trtscent_glist:=23.4167;   // mm track centres, turnout side.
           trmscent_glist:=23.4167;   // mm ditto, main side.
           //retcent_glist:=23.4167;    // mm ditto, return curve.
           min_radius_glist:=500;     // mm minimum radius for warning (20" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' N         9.0  mm    2.06 mm/ft  1:148     N for UK only           ';
           scale_glist:=2.05946;      // mm per ft.
           gauge_glist:=9.0;          // mm.
           fw_glist:=0.85;            // mm flangeway.
           old_fwe_glist:=1.65;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=25;        // mm track centres, turnout side.
           trmscent_glist:=25;        // mm ditto, main side.
           //retcent_glist:=25;         // mm ditto, return curve.
           min_radius_glist:=300;     // mm minimum radius for check (12" approx).

           t_N_UK_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' TT-120   12.0  mm    2.54 mm/ft  1:120     TT USA / Europe         ';
           scale_glist:=2.54;         // mm per ft.
           gauge_glist:=12.0;         // mm.
           fw_glist:=0.91;            // mm flangeway.
           old_fwe_glist:=1.75;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=30;        // mm track centres, turnout side.
           trmscent_glist:=30;        // mm ditto, main side.
           //retcent_glist:=30;         // mm ditto, return curve.
           min_radius_glist:=375;     // mm minimum radius for check (15" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' TT-I     12.025mm    3 mm/ft     1:101.6   TT Intermediate TM-S dim';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=12.025;       // mm.
           fw_glist:=0.95;            // mm flangeway.
           old_fwe_glist:=1.8;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=37.5;      // mm track centres, turnout side.
           trmscent_glist:=37.5;      // mm ditto, main side.
           //retcent_glist:=37.5;     // mm ditto, return curve.
           min_radius_glist:=450;     // mm minimum radius for check (18" approx).
                   end;{with}

                   t_TTI_i:=i;            // for gauge checks.

                   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' TT-F     12.025mm    3 mm/ft     1:101.6   TT Fine  FM dims        ';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=12.025;       // mm.
           fw_glist:=0.8;             // mm flangeway.
           old_fwe_glist:=1.5;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=37.5;      // mm track centres, turnout side.
           trmscent_glist:=37.5;      // mm ditto, main side.
           //retcent_glist:=37.5;     // mm ditto, return curve.
           min_radius_glist:=600;     // mm minimum radius for check (34" approx).
                   end;{with}

                   t_TTF_i:=i;            // for gauge checks.

                   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' TT3      12.05 mm    3 mm/ft     1:101.6   TT Triang Standard      ';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=12.05;        // mm.
           fw_glist:=1.1;             // mm flangeway.
           old_fwe_glist:=2.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=37.5;      // mm track centres, turnout side.
           trmscent_glist:=37.5;      // mm ditto, main side.
           //retcent_glist:=37.5;     // mm ditto, return curve.
           min_radius_glist:=450;     // mm minimum radius for check (18" approx).
                   end;{with}

                   t_TT3_i:=i;            // for gauge checks.

                   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' TM-S     13.525mm    3 mm/ft     1:101.6   TM Standard             ';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=13.525;       // mm.
           fw_glist:=0.95;            // mm flangeway.
           old_fwe_glist:=1.8;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=35;        // mm track centres, turnout side.
           trmscent_glist:=35;        // mm ditto, main side.
           //retcent_glist:=35;       // mm ditto, return curve.
           min_radius_glist:=600;     // mm minimum radius for check (24" approx).
                   end;{with}

                   t_TMS_i:=i;            // for gauge checks.

                   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' TM-F     13.525mm    3 mm/ft     1:101.6   TM Fine  FM dims        ';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=13.525;       // mm.
           fw_glist:=0.8;             // mm flangeway.
           old_fwe_glist:=1.5;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=35;        // mm track centres, turnout side.
           trmscent_glist:=35;        // mm ditto, main side.
           //retcent_glist:=35;       // mm ditto, return curve.
           min_radius_glist:=600;     // mm minimum radius for check (24" approx).
                   end;{with}

                   t_TMF_i:=i;            // for gauge checks.

                   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' FM       14.125mm    3 mm/ft     1:101.6   3 mm Fine (14.2)        ';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=14.125;       // mm.
           fw_glist:=0.8;             // mm flangeway.
           old_fwe_glist:=1.5;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=750;     // mm minimum radius for check (30" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S3       14.125mm    3 mm/ft     1:101.6   ScaleThree              ';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=14.125;       // mm.
           fw_glist:=0.5;             // mm flangeway.
           old_fwe_glist:=0.95;           // mm flangeway end (flangeway+flare).
           min_radius_glist:=900;     // mm minimum radius for check (36" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' FM-OLD   14.2  mm    3 mm/ft     1:101.6   3 mm (14.2 OLD)         ';
           scale_glist:=3;            // mm per ft.
           gauge_glist:=14.2;         // mm.
           fw_glist:=0.8;             // mm flangeway.
           old_fwe_glist:=1.5;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=750;     // mm minimum radius for check (30" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin        // added 0.93.a ...

           listbox_str:=' H0-SF    16.25 mm    3.5  mm/ft  1:87.09   H0 Special Fine  (AMRA) ';
           scale_glist:=3.5;          // mm per ft.
           gauge_glist:=16.25;        // mm.
           fw_glist:=1.05;            // mm flangeway.
           old_fwe_glist:=1.7;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           min_radius_glist:=750;     // mm minimum radius for check (30" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' S3.5     16.48 mm    3.5  mm/ft  1:87.09   Proto-87 (US + UK)      ';
           scale_glist:=3.5;          // mm per ft.
           gauge_glist:=16.48;        // mm.
           fw_glist:=0.58;            // mm flangeway.
           old_fwe_glist:=1.1;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=850;     // mm minimum radius for check (33" approx).

           t_S3p5_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' H0-NMRA  16.5  mm    3.5  mm/ft  1:87.09   H0 (US + UK)            ';
           scale_glist:=3.5;          // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=1.27;            // mm flangeway.
           old_fwe_glist:=1.9;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           //retcent_glist:=50;       // mm ditto, return curve.
           min_radius_glist:=600;     // mm minimum radius for check (24" approx).

           t_H0US_i:=i;           // for gauge quick-set menu.
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' P-87     16.50 mm    3.503mm/ft  1:87      Proto-87 (Europe)       ';
           scale_glist:=304.8/87;     // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=0.6;             // mm flangeway.
           old_fwe_glist:=1.1;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=850;     // mm minimum radius for check (33" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' H0-NEM   16.5  mm    3.503mm/ft  1:87      H0 (Europe)             ';
           scale_glist:=304.8/87;     // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=1.3;             // mm flangeway.
           old_fwe_glist:=1.9;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           //retcent_glist:=50;       // mm ditto, return curve.
           min_radius_glist:=600;     // mm minimum radius for check (24" approx).

           t_H0EU_i:=i;           // for gauge quick-set menu.
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' H00-DN   16.2  mm    3.75 mm/ft  1:81.28   H0-00 hybrid   EMGS dims';  // 212a
           scale_glist:=3.75;         // mm per ft.
           gauge_glist:=16.2;         // mm.
           fw_glist:=1.0;             // mm flangeway.
           old_fwe_glist:=1.7;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=44.67;     // mm track centres, turnout side.
           trmscent_glist:=44.67;     // mm ditto, main side.
           min_radius_glist:=750;     // mm minimum radius for check (30" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' 4-SF     16.2  mm    4 mm/ft     1:76.2    EMGS dims UK   (00-SF)  ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=16.2;         // mm.
           fw_glist:=1.0;             // mm flangeway.
           old_fwe_glist:=1.7;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=750;     // mm minimum radius for check (30" approx).
                   end;{with}

           t_00SF_i:=i;            // for gauge checks.

                   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 00-D0GAF 16.5  mm    4 mm/ft     1:76.2    00 D0GA Fine UK         ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=1.0;             // mm flangeway.
           old_fwe_glist:=1.7;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           //retcent_glist:=50;       // mm ditto, return curve.
           min_radius_glist:=750;     // mm minimum radius for check (30" approx).

           t_00DGF_i:=i;         // for sleeper sizes.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 00-D0GAI 16.5  mm    4 mm/ft     1:76.2    00 D0GA Intermediate UK ';  // added 212a
           scale_glist:=4;            // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=1.2;             // mm flangeway.
           old_fwe_glist:=1.9;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           //retcent_glist:=50;       // mm ditto, return curve.
           min_radius_glist:=675;     // mm minimum radius for check (27" approx).

           t_00DGI_i:=i;         // for sleeper sizes.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' 00-BRMSB 16.5  mm    4 mm/ft     1:76.2    00 BRMSB 1951 UK        ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=1.25;            // mm flangeway.
           old_fwe_glist:=1.9;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           //retcent_glist:=50;       // mm ditto, return curve.
           min_radius_glist:=675;     // mm minimum radius for check (27" approx).

           t_00BRMSB_i:=i;     // for sleeper sizes.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 00-BF    16.5  mm    4 mm/ft     1:76.2    00 Commercial Fine UK   ';   // 212a fw was 1.25
           scale_glist:=4;            // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=1.3;             // mm flangeway.
           old_fwe_glist:=1.9;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           //retcent_glist:=50;       // mm ditto, return curve.
           min_radius_glist:=675;     // mm minimum radius for check (24" approx).

           t_00BF_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 00/H0    16.5  mm    4 mm/ft     1:76.2    00/H0 Universal UK      ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=16.5;         // mm.
           fw_glist:=1.5;             // mm flangeway.
           old_fwe_glist:=2.3;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=50;        // mm track centres, turnout side.
           trmscent_glist:=50;        // mm ditto, main side.
           //retcent_glist:=50;       // mm ditto, return curve.
           min_radius_glist:=600;     // mm minimum radius for check (24" approx).

           t_00H0_i:=i;         // for sleeper sizes

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


   with gauge[i] do begin
           listbox_str:=' EM-SF    18.0  mm    4 mm/ft     1:76.2    EM Special Fine         ';   // added 212a
           scale_glist:=4;            // mm per ft.
           gauge_glist:=18.0;         // mm.
           fw_glist:=0.8;             // mm flangeway.
           old_fwe_glist:=1.4;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=900;     // mm minimum radius for check (36" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' EM-18    18.0  mm    4 mm/ft     1:76.2    original EM BRMSB 1951  ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=18.0;         // mm.
           fw_glist:=1.0;             // mm flangeway.
           old_fwe_glist:=1.7;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=900;     // mm minimum radius for check (36" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' EM       18.2  mm    4 mm/ft     1:76.2    EMGS dimensions         ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=18.2;         // mm.
           fw_glist:=1.0;             // mm flangeway.
           old_fwe_glist:=1.7;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=750;     // mm minimum radius for check (30" approx).

           t_EM_i:=i;           // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

                         // added 211b..

  with gauge[i] do begin
           listbox_str:=' EM4      18.8  mm    4 mm/ft     1:76.2    EM Extra Fine           ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=18.8;         // mm.
           fw_glist:=0.8;             // mm flangeway.
           old_fwe_glist:=1.4;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=900;     // mm minimum radius for check (36" approx).

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' S4/P4    18.83 mm    4 mm/ft     1:76.2    P4 dimensions           ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=18.83;        // mm.
           fw_glist:=0.67;            // mm flangeway.
           min_radius_glist:=1000;    // mm minimum radius for warning (39" approx).

           t_S4P4_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S4-X     18.83 mm    4 mm/ft     1:76.2    exact dimensions        ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=18.83;        // mm.
           min_radius_glist:=1100;     // mm minimum radius for warning (43" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' EM(I)    20.2  mm    4 mm/ft     1:76.2    Irish 5''3" EMGS dims   ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=20.2;         // mm.
           fw_glist:=1.0;             // mm flangeway.
           old_fwe_glist:=1.75;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=46.8333;   // mm track centres, turnout side.
           trmscent_glist:=46.8333;   // mm ditto, main side.
           //retcent_glist:=46.8333;    // mm ditto, return curve.
           min_radius_glist:=1000;     // mm minimum radius for check (39" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S4(I)    21.0  mm    4 mm/ft     1:76.2    Irish 5''3" S4/P4 dims  ';
           scale_glist:=4;            // mm per ft.
           gauge_glist:=21;           // mm.
           fw_glist:=0.67;            // mm flangeway.
           trtscent_glist:=46.8333;   // mm track centres, turnout side.
           trmscent_glist:=46.8333;   // mm ditto, main side.
           //retcent_glist:=46.8333;    // mm ditto, return curve.
           min_radius_glist:=1100;     // mm minimum radius for warning (43" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' P-64      0.883"     3/16"/ft    1:64      Proto-64                ';
           scale_glist:=4.7625;       // mm per ft.
           gauge_glist:=22.4234375;   // mm.
           fw_glist:=0.70;            // mm flangeway.
           old_fwe_glist:=1.40;           // mm flangeway end (flangeway+flare).
           min_radius_glist:=1300;    // mm minimum radius for check (51" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S         0.884"     3/16"/ft    1:64      S Gauge                 ';
           scale_glist:=4.7625;       // mm per ft.
           gauge_glist:=22.45;        // mm.
           fw_glist:=0.71;            // mm flangeway.   .028"
           old_fwe_glist:=1.40;           // mm flangeway end (flangeway+flare).
           min_radius_glist:=1200;     // mm minimum radius for check (48" approx).

           t_S_gauge_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S(I)      0.984"     3/16"/ft    1:64      Irish 5''3" S dims      ';
           scale_glist:=4.7625;       // mm per ft.
           gauge_glist:=25.00;        // mm.
           fw_glist:=0.71;            // mm flangeway.   .028"
           old_fwe_glist:=1.40;           // mm flangeway end (flangeway+flare).
           min_radius_glist:=1300;    // mm minimum radius for check (51" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' T-55      1.000"     5.5 mm/ft   1:55.42   Templot0 startup         ';
           scale_glist:=5.5;            // mm per ft.
           gauge_glist:=25.4;           // mm.
           fw_glist:=1.0;               // mm flangeway. (EM dims)
           min_radius_glist:=1000;      // mm minimum radius for warning (39" approx).

           t_T55_i:=i;            // for gauge checks.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' P-55     25.9  mm    5.5 mm/ft   1:55.42   Proto-55                ';
           scale_glist:=5.5;            // mm per ft.
           gauge_glist:=25.9;           // mm.
           fw_glist:=0.8;               // mm flangeway.
           min_radius_glist:=1375;       // mm minimum radius for warning (54" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' P-48     29.9  mm    1/4"/ft     1:48      Proto-48                ';
           scale_glist:=6.35;           // mm per ft.
           gauge_glist:=29.9;           // mm.
           fw_glist:=1.0;               // mm flangeway.
           old_fwe_glist:=2.0;              // mm flangeway end (flangeway+flare).
           min_radius_glist:=1600;      // mm minimum radius for warning (63" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin              // added 0.93.a ...

           listbox_str:=' 0-NMRA    1.252"     1/4"/ft     1:48      0 NMRA dimensions       ';
           scale_glist:=6.35;         // mm per ft.
           gauge_glist:=31.8;         // mm.
           fw_glist:=1.8;             // mm flangeway.
           old_fwe_glist:=3.6;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=90;        // mm track centres, turnout side.
           trmscent_glist:=90;        // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=1000;    // mm minimum radius for check (39" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);



  with gauge[i] do begin
           listbox_str:=' P-45     32.00 mm    6.77 mm/ft  1:45      Proto-45 Europe         ';
           scale_glist:=304.8/45;     // mm per ft.
           gauge_glist:=32.0;         // mm.
           fw_glist:=1.0;             // mm flangeway.
           old_fwe_glist:=2.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=4000/45;   // mm track centres, turnout side.
           trmscent_glist:=4000/45;   // mm ditto, main side.
           //retcent_glist:=4000/45;    // mm ditto, return curve.

           min_radius_glist:=1675;    // mm minimum radius for check (66" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' 0-SF     31.20 mm    7 mm/ft     1:43.54   0 SPECIAL-FINE          ';    // added 0.79.a
           scale_glist:=7;            // mm per ft.
           gauge_glist:=31.2;         // mm.
           fw_glist:=1.2;            // mm flangeway.   221a  was 1.25
           old_fwe_glist:=2.5;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=1750;    // mm minimum radius for check (69" approx).

           t_0SF_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 0-MF     31.50 mm    7 mm/ft     1:43.54   0 MODIFIED-FINE         ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=31.5;         // mm.
           fw_glist:=1.5;             // mm flangeway.
           old_fwe_glist:=3.0;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=1750;    // mm minimum radius for check (69" approx).

           t_0MF_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 0-AMRA   31.875mm    7 mm/ft     1:43.54   0 AMRA Australia        ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=31.875;       // mm.
           fw_glist:=2.0;             // mm flangeway.
           old_fwe_glist:=4.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=90;        // mm track centres, turnout side.
           trmscent_glist:=90;        // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=1000;     // mm minimum radius for check (39" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' GOG-F    32.0  mm    7 mm/ft     1:43.54   0 GOG dims FINE         ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=32.0;         // mm.
           fw_glist:=1.75;            // mm flangeway.
           old_fwe_glist:=3.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=80;        // mm track centres, turnout side.
           trmscent_glist:=80;        // mm ditto, main side.
           //retcent_glist:=80;         // mm ditto, return curve.
           min_radius_glist:=1200;    // mm minimum radius for check (47" approx).

           t_GOGF_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' GOG-C    32.0  mm    7 mm/ft     1:43.54   0 GOG dims COARSE       ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=32.0;         // mm.
           fw_glist:=2.2;             // mm flangeway.
           old_fwe_glist:=4.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=90;        // mm track centres, turnout side.
           trmscent_glist:=90;        // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=1000;     // mm minimum radius for check (39" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 0-NEM    32.0  mm    7 mm/ft     1:43.54   EurO NEM 43             ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=32.0;         // mm.
           fw_glist:=2.15;            // mm flangeway.
           old_fwe_glist:=4.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=90;        // mm track centres, turnout side.
           trmscent_glist:=90;        // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=1000;     // mm minimum radius for check (39" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 0-BC     32.0  mm    7 mm/ft     1:43.54   0 BRMSB Coarse          ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=32.0;         // mm.
           fw_glist:=2.5;             // mm flangeway.
           old_fwe_glist:=4.5;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=90;        // mm track centres, turnout side.
           trmscent_glist:=90;        // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=1000;     // mm minimum radius for check (39" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S7       33.00 mm    7 mm/ft     1:43.54   ScaleSeven              ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=33.0;         // mm.
           fw_glist:=1.05;            // mm flangeway.
           old_fwe_glist:=2.1;            // mm flangeway end (flangeway+flare).
           min_radius_glist:=1750;    // mm minimum radius for check (69" approx).

           t_S7_i:=i;           // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' S7(I)    36.75 mm    7 mm/ft     1:43.54   Irish 5''3" S7 dims     ';
           scale_glist:=7;            // mm per ft.
           gauge_glist:=36.75;        // mm.
           fw_glist:=1.05;            // mm flangeway.
           old_fwe_glist:=2.1;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=81.9583;   // mm track centres, turnout side.
           trmscent_glist:=81.9583;   // mm ditto, main side.
           //retcent_glist:=81.9583;    // mm ditto, return curve.
           min_radius_glist:=1850;     // mm minimum radius for check (73" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' 0-US-OLD  1.250"     9/32"/ft    1:42.67   0 US old dimensions     ';
           scale_glist:=7.14375;      // mm per ft.
           gauge_glist:=31.75;        // mm.
           fw_glist:=1.75;            // mm flangeway.
           old_fwe_glist:=3.5;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=90;        // mm track centres, turnout side.
           trmscent_glist:=90;        // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=1000;    // mm minimum radius for check (39" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' P-32     45.0  mm    3/8"/ft     1:32      ScaleOne32              ';
           scale_glist:=9.525;        // mm per ft.
           gauge_glist:=45.0;         // mm.
           fw_glist:=1.5;             // mm flangeway.
           old_fwe_glist:=2.8;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=106.36;    // mm track centres, turnout side.
           trmscent_glist:=106.36;    // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=2500;    // mm minimum radius for check (98" approx).

           t_P32_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);



  with gauge[i] do begin
           listbox_str:=' 1C        1.750"     10 mm/ft    1:30.48   Gauge 1 Coarse          ';
           scale_glist:=10;           // mm per ft.
           gauge_glist:=44.45;        // mm.
           fw_glist:=3.0;             // mm flangeway.
           old_fwe_glist:=5.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=125;       // mm track centres, turnout side.
           trmscent_glist:=125;       // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=2000;    // mm minimum radius for check (78" approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);



  with gauge[i] do begin
           listbox_str:=' 1F       45.0  mm    10 mm/ft    1:30.48   Gauge 1 Fine            ';
           scale_glist:=10;           // mm per ft.
           gauge_glist:=45;           // mm.
           fw_glist:=1.75;            // mm flangeway.
           old_fwe_glist:=3.5;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=115;       // mm track centres, turnout side.
           trmscent_glist:=115;       // mm ditto, main side.
           //retcent_glist:=90;         // mm ditto, return curve.
           min_radius_glist:=2500;    // mm minimum radius for check (98" approx).

           t_1F_i:=i;         // for gauge quick-set menu.

                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' No.3       2.5"      13.5 mm/ft  1:22.58   Gauge 3                 ';
           scale_glist:=13.5;         // mm per ft.
           gauge_glist:=63.5;         // mm.
           fw_glist:=2.75;            // mm flangeway.
           old_fwe_glist:=5.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=175;       // mm track centres, turnout side.
           trmscent_glist:=175;       // mm ditto, main side.
           //retcent_glist:=90;       // mm ditto, return curve.
           min_radius_glist:=3000;    // mm minimum radius for check (10ft approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' No.4       3.5"      19 mm/ft    1:16.04   model engineers / trams ';
           scale_glist:=19.0;         // mm per ft.
           gauge_glist:=89;           // mm.
           fw_glist:=4.0;             // mm flangeway.
           old_fwe_glist:=8.0;            // mm flangeway end (flangeway+flare).
           trtscent_glist:=250;       // mm track centres, turnout side.
           trmscent_glist:=250;       // mm ditto, main side.
           //retcent_glist:=90;       // mm ditto, return curve.
           min_radius_glist:=4500;    // mm minimum radius for check (15ft approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  with gauge[i] do begin
           listbox_str:=' No.5-A     4.750"    1"/ft       1:12      model engineers USA     ';
           scale_glist:=25.4;         // mm per ft.
           gauge_glist:=120.65;       // mm.
           fw_glist:=5.5;             // mm flangeway.
           old_fwe_glist:=10.0;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=350;       // mm track centres, turnout side.
           trmscent_glist:=350;       // mm ditto, main side.
           //retcent_glist:=90;       // mm ditto, return curve.
           min_radius_glist:=6000;    // mm minimum radius for check (20ft approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  with gauge[i] do begin
           listbox_str:=' No.5       5"        27 mm/ft    1:11.29   model engineers         ';
           scale_glist:=27.0;         // mm per ft.
           gauge_glist:=127.0;        // mm.
           fw_glist:=5.5;             // mm flangeway.
           old_fwe_glist:=10.0;           // mm flangeway end (flangeway+flare).
           trtscent_glist:=350;       // mm track centres, turnout side.
           trmscent_glist:=350;       // mm ditto, main side.
           //retcent_glist:=90;       // mm ditto, return curve.
           min_radius_glist:=6000;    // mm minimum radius for check (20ft approx).
                   end;{with}   gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);


  listbox_str:=' CUSTOM-a                                custom settings (a)';  gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);
  listbox_str:=' CUSTOM-b                                custom settings (b)';  gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);
  listbox_str:=' CUSTOM-c                                custom settings (c)';  gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);
  listbox_str:=' CUSTOM-d                                custom settings (d)';  gauge_form.gauge_listbox.Items.Add(listbox_str);    Inc(i);if i>gauge_indexmax_c then run_error(39);

  listbox_str:=' EXACT                                      exact scale     ';  gauge_form.gauge_listbox.Items.Add(listbox_str);

  gauge[i].scale_glist:=0;   // EXACT mm per ft.     // to be entered on gauge form.

  for j:=0 to i do begin
    temp_str:=Trim(Copy(gauge_form.gauge_listbox.Items.Strings[j],1,10));  // copy from within first 10 chars.

    gauge[j].name_str_glist:=Copy(temp_str,1,9);  // and crop to 9 if nec.

    sc:=gauge[j].scale_glist;                       // scale
    if sc<>0 then begin
                    ins:=sc/12.0;                   // inscale

                    gauge[j].fwe_glist:=gauge[j].fw_glist+1.75*ins;   // 215a   use prototype flare angle for all (flangeway + 1.75" to end gap)


                    g_defaults(sc,ins,gauge[j]);    // set all g-defaults
                  end;
  end;//next
end;
//___________________________________________________________________________________________

procedure custom_click(custom:integer);

const
  help_all_str:string='||With most standard gauge tracks you will probably only need to set the track gauge and flangeway gaps.'
   +' For the remaining dimensions the pre-sets will usually give satisfactory results (enter a "/" slash).'
   +'||The existing dimensions showing in the input box are Templot0''s best guess based on the current gauge/scale combination, so it is'
   +' useful to select the nearest existing size before entering your custom dimensions.'
   +'||When you save and reload templates which use these custom settings via your storage box, the custom settings for each one will be preserved, but will not'
   +' be reflected here in the gauge list.'
   +'||Once you have such a custom control template on the trackpad, its settings will continue to be used for'
   +' any subsequent templates until you come here and select another gauge size, or copy a different template from the background or storage box.'
   +'||So there is no real need to adopt the custom settings for them into this list. But doing so makes it convenient to swap between several set-ups without having to remember which template to copy or reload.'
   +'||Or in other words, do this:'
   +'||1. Enter here your required custom setting dimensions.'
   +'||2. Click OK to create a template using them.'
   +'||3. Save the template via your storage box.'
   +'||4. When you next need to use these settings, simply reload this template as a starting point.'
   +'||5. There is no need to come here again until you want to create a different custom setting.'
   +'||6. But you can if you want to, and simply adopt the settings into one of the custom slots. It is not necessary to re-enter the actual dimensions.';

   help_adopt_str:string='||If the control template dimensions have been modified from a standard gauge/scale setting you can adopt these dimensions in their entirety'
   +' to create your new custom gauge/scale setting.'
   +'||( If the control template dimensions have not been modified since a standard setting was selected, adopting them will simply duplicate the standard setting, to no advantage.)'
   +'||When you store (and/or then save) a template using modified dimensions, these dimensions are retained and will be restored when you reload and copy the template back from your storage box.'
   +' And unless you change the dimensions, they will continue to be used for any subsequent templates.'
   +'||So the need to make them a custom setting is not immediately obvious. But doing so makes it'
   +' convenient to swap between several set-ups without having to remember which template to copy or reload.'
   +'||For example, the standard gauge/scale settings include the standard dimension for adjacent track centres (normally based on 6ft way between the rails,'
   +' i.e. 11ft 2ins centres for standard gauge running lines).'
   +'  You could create an alternative custom setting for loops and sidings (9ft or 10ft way), and quickly swap between the two without having to re-enter the dimensions or reload templates.';


  help_cen_str:string='    Adjacent  Track  Spacing.'
  +'||Enter the centre-to-centre distance in millimetres to adjacent track. It is possible'
  +' to use different spacing dimensions for the tracks on each side of the template.'

  +'||TS is "turnout-side", i.e. the same side as the hand of the template.'
  +'||MS is "main-side", i.e. the opposite side to the hand of the template.'

  +'||Separate templates for these adjacent tracks are created by selecting the TOOLS > MAKE CROSSOVER or TOOLS > MAKE DOUBLE-TRACK menu items.'

  +'||Adjacent track rails can also be placed on the control template as planning aids, by selecting the GENERATOR > ADJACENT TRACKS menu items. It is necessary to select each rail separately.'

  +'||The minimum spacing on straight track should normally give a 6ft way between the inner rails, i.e. 11ft 2in (134in) (scale) minimum centre-to-centre spacing for standard-gauge tracks.'
  +'||Where there are sharp curves or superelevation, this distance must be increased to allow for vehicle overhang.'
  +'||The minimum spacing should also be increased when using the TOOLS > MAKE DOUBLE-TRACK functions on a transition curve, as it is not then mathematically possible to create an exact uniformly spaced adjacent track.'

  +'||Where the adjacent track forms a loop or siding, the spacing should normally be increased by 4ft to 15ft 2in (182in) (scale).'
  +' This is to provide a space for signal posts and other obstructions, and to ensure the safety of shunting staff.'

  +'||If preferred, full-size dimensions can be entered (in inches) by using the P conversion factor. For example, to enter the scale equivalent of 15ft 2in, enter p182.'
  +' For more information about conversion factors, click the ?HELP button.'

  +'||Many model railway gauge standards incorporate overscale nominal spacings to allow for the use of sharp curves.'

  +'||In constructing a full track plan these spacing dimensions may frequently need to be modified for individual templates. This is done using he GEOMETRY > ADJACENT TRACK CENTRES... menu item.';

  help_gauge_str:string='    Track  Gauge'
  +'||Enter a dimension in mm on the model for the distance between the inner edges of the running rails.'
  +'||The preset dimension is the scale equivalent of 56.5" full-size (4ft 8.5in).';

  help_gap_str:string='      Flangeway  Gap'
  +'||Enter a dimension in mm on the model for the flangeway gap between the nose of the crossing and the wing rails, and also between the running rail and the check rails.'
  +'||The preset dimension is the scale equivalent of 1.75" full-size.';

  help_end_gap_str:string='      Flangeway  End  Gap'
  +'||Enter a dimension in mm on the model for the flangeway end gap between the running rail and the flared ends of check and wing rails.'
  +'||The preset dimension is the scale equivalent of 3.5" full-size.';

  help_rad_str:string='      Warning  Radius'
  +'||Enter a dimension in mm on the model for the radius (at the track centre-line) below which the warning lamp in the INFORMATION panel will flash red.'
  +'||The preset dimension is the scale equivalent of 152ft radius full size.';

var
  i,n,index:integer;
  sc,ins:extended;
  od:Toutdim;
  help_str:string;
  dummy_i:integer;
  old_list_scale:extended;
  old_scale,new_scale:extended;

  name_str,list_str,dummy_str,listbox_str:string;

  got_all_custom:boolean;

label
  100,200;

begin
  index:=gauge_form.gauge_listbox.Items.Count-5+custom;              //  set current line.
  help_str:='    '+gauge[index].name_str_glist+'  Settings'+help_all_str;
  old_scale:=scale;                                                  //  save control template scale.

  new_scale:=old_scale;  // default init.

  case custom of
     0: dummy_str:='a';
     1: dummy_str:='b';
     2: dummy_str:='c';
   else dummy_str:='d';
  end;//case

  with math_form do begin

    if gauge[index].scale_glist<>0
       then begin
              math_editbox.Text:=gauge[index].name_str_glist;

              i:=alert(4,'    custom ('+dummy_str+')  gauge / scale ...',
                 'The custom ('+dummy_str+') gauge/scale setting is already in use as '+gauge[index].name_str_glist
                +'||Do you want to delete it or replace it?',
                 '','','','delete  '+gauge[index].name_str_glist,'cancel','replace  '+gauge[index].name_str_glist,0);

              if i=5 then EXIT;

              if (i=4) or (i=6)     // delete ...
                 then begin
                        init_gauge_defaults(index);
                        listbox_str:=' CUSTOM-'+dummy_str+'                                custom settings ('+dummy_str+')';
                        gauge_form.gauge_listbox.Items.Strings[index]:=listbox_str;
                      end;

              if i=4 then EXIT;

            end
       else math_editbox.Text:='';

    Caption:='   custom ('+dummy_str+')  gauge / scale  designation  name ...';
    big_label.Caption:=insert_crlf_str('|||Enter a short designation for this custom ('+dummy_str+')  gauge/scale setting.||Maximum 9 characters with no spaces.'
                                      +'||For example a designation EM-SDNGS might be for EM Gauge track in sidings, with an increased adjacent track spacing and a smaller radius warning limit.'
                                      +'||0-14MM might be for 14mm narrow-gauge (2ft gauge) in 7mm/ft scale.'
                                      +'||Do not use long words or spaces - maximum is 9 characters.'
                                      +'||Do not use a designation name which is already in use in the list.');



    100:

    do_show_modal(math_form);     // 212a  ShowModal

    if ModalResult<>mrOK
       then begin
              Caption:='    '+Application.Title;   // reset form caption.
              EXIT;
            end;

    if Trim(math_editbox.Text)<>''
       then begin
              name_str:=Copy(Trim(math_editbox.Text),1,9);  // max 9 chars
            end
       else begin
              ShowMessage('error - A blank designation name is not valid.');
              GoTo 100;
            end;

         // check not already in use ..

    n:=0;   // init

    200:

    if name_str=gauge[n].name_str_glist
       then begin
                ShowMessage('error - The designation '+name_str+' is already in use.'+#13+#13+'Please enter a different designation.');
                math_editbox.Text:='';
                GoTo 100;
            end;
    INC(n);

    if n<=gauge_indexmax_c then GoTo 200;

    Caption:='    '+Application.Title;   // reset form caption.

  end;//with

  got_all_custom:=False;   // init

  repeat
    i:=alert(4,'    '+name_str+'  gauge / scale ...',
               'Do you want to adopt the current control template dimensions for this '+name_str+' custom setting?',
               '','','more  information','yes  -  adopt  the  control  template  dimensions','cancel','no  -  enter  custom  settings  individually',3);
    case i of
        3: alert_help(0,'    '+gauge[index].name_str_glist+'  Settings'+help_adopt_str+'||Custom  Settings'+help_all_str,'');
        4: begin
             with gauge[index] do begin
               with cpi do begin
                scale_glist:=scale_pi;            // mm/ft.
                gauge_glist:=gauge_pi;            // mm.
                fw_glist:=fw_pi;                  // mm flangeway.
                fwe_glist:=fwe_pi;                // mm flangeway end (flangeway+flare).
                trtscent_glist:=trtscent_pi;      // mm track centres, turnout side.
                trmscent_glist:=trmscent_pi;      // mm ditto, main side.
                min_radius_glist:=min_radius_pi;  // mm minimum radius for check.
               end;//with cpi
             end;//with

             gauge_form.warning_panel.Visible:=False;  // just loaded, so there can't be any mismatch.

             got_all_custom:=True;
           end;
        5: EXIT;
    end;//case
  until i<>3;

  if got_all_custom=False
     then begin

            repeat
              i:=alert(4,'    '+name_str+'  gauge / scale ...',
                         'The scale size for this '+name_str+' custom setting can be entered in mm/ft, or as a scale ratio.'
                        +'||Please select your preference.',
                         '','','help','scale  ratio  1 : n','cancel','scale  in  mm / ft',3);
              case i of
                  3: alert_help(0,scale_help_str,'');

                  4: begin
                        n:=putdim(scale_help_str,0,'scale  ratio  1 :',304.8/old_scale,True,True,True,False); // no negative, no pre-set, no zero, don't terminate on zero.   {mods 21-3-01, 0.68.b}
                        if n<>0 then run_error(231);
                        if getdims('scale  ratio','',gauge_form,n,od)=True
                           then new_scale:=ABS(304.8/od[0])  //  1 foot = 304.8 mm
                           else EXIT;
                     end;

                  5: EXIT;

                  6: begin
                        n:=putdim(scale_help_str,1,'scale  in  mm / ft',old_scale,True,True,True,False);  // no negative, no pre-set, no zero, don't terminate on zero.  {mods 21-3-01, 0.68.b}
                        if n<>0 then run_error(232);
                        if getdims('scale  mm / ft','',gauge_form,n,od)=True
                           then new_scale:=ABS(od[0])   //  mm/ft.
                           else EXIT;
                     end;

                else EXIT;
              end;//case

            until i<>3;

            new_scale:=limits(0.1,500,new_scale,dummy_i);  // maximum scale 500 mm to the foot (nearly twice full-size).

            with cpi do begin // use existing control template settings. {mods 21-3-01, 0.68.b}

              putdim(help_gauge_str,  1,'track  gauge',                           gauge_pi      *new_scale/old_scale,True,False,True,False);   // no negative, pre-set ok, no zero, don't terminate on zero.
              putdim(help_gap_str,    1,'flangeway  gap',                         fw_pi         *new_scale/old_scale,True,False,False,False);  // no negative, pre-set ok, zero ok, don't terminate on zero.
              putdim(help_end_gap_str,1,'flangeway  end gap ( at  flared  ends )',  fwe_pi        *new_scale/old_scale,True,False,False,False);  // no negative, pre-set ok, zero ok, don't terminate on zero.

              putdim(help_cen_str,    1,'adjacent  track  centres  TS ( turnout-side )', trtscent_pi   *new_scale/old_scale,True,False,True,False);   // no negative, pre-set ok, no zero, don't terminate on zero.
              putdim(help_cen_str,    1,'adjacent  track  centres  MS ( main-side )',    trmscent_pi   *new_scale/old_scale,True,False,True,False);   // no negative, pre-set ok, no zero, don't terminate on zero.
           n:=putdim(help_rad_str,    1,'minimum  radius  ( for  warning )',             min_radius_pi *new_scale/old_scale,True,False,True,False);   // no negative, pre-set ok, no zero, don't terminate on zero.

            end;//with

            if n<>5 then EXIT;
            if getdims(name_str+'  custom  settings',help_str,gauge_form,n,od)=True
               then begin
                      with gauge[index] do begin

                        scale_glist:=new_scale;

                        gauge_glist     :=od[0];       // mm.
                        fw_glist        :=od[1];       // mm flangeway.
                        fwe_glist       :=od[2];       // mm flangeway end (flangeway+flare).
                        trtscent_glist  :=od[3];       // mm track centres, turnout side.
                        trmscent_glist  :=od[4];       // mm ditto, main side.
                        min_radius_glist:=od[5];       // mm minimum radius for check.
                      end;//with
                    end
               else EXIT;
          end;

          // got the custom settings, or defaults ...

  if gauge[index].scale_glist>minfp
     then begin
            sc:=gauge[index].scale_glist;       // scale
            ins:=sc/12.0;                       // inscale
            g_defaults(sc,ins,gauge[index]);    // set any defaults he requested.


            gauge[index].name_str_glist:=name_str;  // add the name


               // format new line in listbox...


            dummy_str:='S7(I)    36.75 mm    7 mm/ft            custom settings (';

            dummy_str:=StringReplace(dummy_str,'S7(I)    ',Copy(name_str+'         ',1,9),[rfIgnoreCase]);

            dummy_str:=StringReplace(dummy_str,'36.75 mm   ',FormatFloat('00.00',gauge[index].gauge_glist)+' mm   ',[rfIgnoreCase]);

            dummy_str:=StringReplace(dummy_str,' 7 mm/ft     ',FormatFloat('00.00',gauge[index].scale_glist)+' mm/ft  ',[rfIgnoreCase]);

            dummy_str:=StringReplace(dummy_str,' 0','  ',[rfReplaceAll,rfIgnoreCase]);        // remove leading zeroes

            dummy_str:=StringReplace(dummy_str,'0 mm','  mm',[rfReplaceAll,rfIgnoreCase]);    // remove trailing zeroes

            case custom of

             0: dummy_str:=dummy_str+'a)';
             1: dummy_str:=dummy_str+'b)';
             2: dummy_str:=dummy_str+'c)';
             3: dummy_str:=dummy_str+'d)';

            end;//case

            gauge_form.gauge_listbox.Items.Strings[index]:=' '+dummy_str;     // update listbox line

            gauge_form.gauge_listbox.ItemIndex:=index;     //  initialise selection

            gauge_i:=index;
            gauge_list_click;

          end;

  if gauge_form.Showing=True then gauge_form.ok_button.SetFocus;   // 0.93.a
end;
//________________________________________________________________________________________

procedure Tgauge_form.custom_a_buttonClick(Sender: TObject);

begin
  custom_click(0);     // enter custom data A.
end;
//___________________________________________________________________________________________

procedure Tgauge_form.custom_b_buttonClick(Sender: TObject);

begin
  custom_click(1);     // enter custom data B.
end;
//________________________________________________________________________________________

procedure Tgauge_form.custom_c_buttonClick(Sender: TObject);

begin
  custom_click(2);     // enter custom data C.
end;
//__________________________________________________________________________________________

procedure Tgauge_form.custom_d_buttonClick(Sender: TObject);

begin
  custom_click(3);     // enter custom data D.
end;
//________________________________________________________________________________________

procedure show_gauge_info;

var
  i:integer;
  no_data_str:string;

begin
  show_gauge_details:=True;     // for OnActivate after alerts.

  with gauge_form do begin
    info_listbox.Items.Clear;
    i:=gauge_select_i;

    info_scrollbox.VertScrollBar.Position:=0;
    info_scrollbox.HorzScrollBar.Position:=0;

    if (i<0) or (i>(gauge_listbox.Items.Count-1)) then EXIT;  // ???

    if gauge[i].scale_glist<minfp                     // entry nyi.
       then begin

              if i<(gauge_listbox.Items.Count-5)
                 then begin
                        if i=0 then no_data_str:=insert_crlf_str(' (invalid dummy item)'
                                                              +'|| Please click the gauge in the list which you require'
                                                              +'| or click the [ set custom... ] buttons to enter your'
                                                              +'| required custom gauge and scale dimensions.'
                                                              +'|| For more information please click the [ ? help ] button.')

                               else no_data_str:=insert_crlf_str(ok_large_panel.Caption+sorry_str+'| Please click the chat button for more information.');
                      end
                 else begin
                        if i<(gauge_listbox.Items.Count-1)
                           then no_data_str:=insert_crlf_str(ok_large_panel.Caption+'|| No dimensions for this custom setting have yet been'
                                                                                 +'| entered. Click the [ set custom... ] buttons to enter your'
                                                                                 +'| required custom gauge and scale dimensions.'
                                                                                 +'|| For more information please click the [ ? help ] button.')
                           else no_data_str:=insert_crlf_str(ok_large_panel.Caption+'|| No EXACT scale size has yet been entered.'
                                                                                 +'| Click the [ set exact scale... ] button to enter your'
                                                                                 +'| required exact scale size.'
                                                                                 +'|| For more information please click the [ ? help ] button.');
                      end;

              with info_listbox do begin
                Font.Name:='Arial';
                Items.Text:=no_data_str;
              end;//with

              bold_panel.Caption:='no  info';
              print_button.Enabled:=False;
              copy_button.Enabled:=False;
            end
       else begin

              print_button.Enabled:=True;
              copy_button.Enabled:=True;

              bold_panel.Caption:=ok_large_panel.Caption;

              info_listbox.Font.Name:='Courier New';

              with info_listbox.Items do begin
                with gauge[i] do begin
                  Add(space_lead(FormatFloat('00000.00',scale_glist     ))+'  mm  SCALE per ft.  ( RATIO 1:'+round_str(304.8/scale_glist,2)+' )');
                  Add(space_lead(FormatFloat('00000.00',gauge_glist     ))+'  mm  GAUGE between running rails.');
                  Add(space_lead(FormatFloat('00000.00',fw_glist        ))+'  mm  flangeway gap.');
                  Add(space_lead(FormatFloat('00000.00',fwe_glist       ))+'  mm  flangeway end gap (check rail flared ends).');
                  Add(space_lead(FormatFloat('00000.00',trtscent_glist  ))+'  mm  adjacent track centres, turnout side.');
                  Add(space_lead(FormatFloat('00000.00',trmscent_glist  ))+'  mm  adjacent track centres, main side.');
                  Add(space_lead(FormatFloat('00000.00',min_radius_glist))+'  mm  minimum radius limit for warnings.');
                end;//with
              end;//with
            end;
  end;//with form
end;
//________________________________________________________________________________________

procedure Tgauge_form.print_buttonClick(Sender: TObject);

var
  line_now:integer;
  pf:TextFile;                      // text file to be redirected to printer.

begin

  do_open_source_bang('PRINT GAUGE LIST');  // OT-FIRST

(* OT-FIRST

  if info_listbox.Items.Count<1 then EXIT;

  if no_printer_available=True     // 0.93.a
     then begin
            ShowMessage('No printer available.');
            EXIT;
          end;

  with TPrintDialog.Create(nil) do begin   // 0.93.a created in code because of startup error if no printer available.
    try
      if Execute=False then EXIT;
    finally
      Free;
    end;//try
  end;//with

//  if (print_dialog.Execute=True) and (info_listbox.Items.Count>0)
//     then begin

  AssignPrn(pf);                       // redirect file to printer.
  Rewrite(pf);                         // open the file for writing.

  with Printer.Canvas do begin

    Font.Assign(printer_text_font);

    WriteLn(pf,'');
    WriteLn(pf,'  TEMPLOT  INFORMATION');
    WriteLn(pf,'');

    WriteLn(pf,'  gauge and scale dimensions for :');
    WriteLn(pf,'');

    Font.Size:=14;                        // enlarge gauge header.
    WriteLn(pf,' '+bold_panel.Caption);

    Font.Assign(set_font('Courier New',12,[fsBold],clBlack));    // Courier - monospaced.
    WriteLn(pf,'');
    WriteLn(pf,' '+ok_panel.Caption);
    WriteLn(pf,'');
    Font.Size:=10;

    WriteLn(pf,'---------------------------------------------------------------------------');
    WriteLn(pf,'');

    for line_now:=0 to info_listbox.Items.Count-1 do WriteLn(pf,info_listbox.Items.Strings[line_now]);

    WriteLn(pf,'');
    WriteLn(pf,'---------------------------------------------------------------------------');

    CloseFile(pf);
  end;//with
          //end;

*)

end;
//________________________________________________________________________________________

procedure Tgauge_form.copy_buttonClick(Sender: TObject);

begin
  with info_listbox do begin
    if (ItemIndex<0) or (ItemIndex>(Items.Count-1)) then EXIT;
    Clipboard.AsText:=Copy(Items.Strings[ItemIndex],1,9);
  end;//with
end;
//________________________________________________________________________________________

procedure Tgauge_form.help_buttonClick(Sender: TObject);

begin
  if help(0,gauge_help_str,'chat  -  more  information')=1 then chat_click;
end;
//_________________________________________________________________________________________

procedure Tgauge_form.FormCreate(Sender: TObject);

begin
  if Screen.Height<500
     then begin
            Top:=2;    // move form top left of screen for lo-res.
            Left:=2;
          end;

  // OT-FIRST ClientWidth:=730;
  // OT-FIRST ClientHeight:=620;

  AutoScroll:=True;
end;
//______________________________________________________________________________

procedure Tgauge_form.FormResize(Sender: TObject);

begin
  if (Showing=True) and (no_onresize=False)
     then begin
            gauge_panel.Width:=ClientWidth;
            gauge_listbox.Width:=gauge_panel.Width-gauge_listbox.Left*2;
          end;
end;
//______________________________________________________________________________

procedure Tgauge_form.info_listboxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

var
  s:string;

begin

  s:=info_listbox.Items.Strings[Index];

  with info_listbox.Canvas do begin

    if (odSelected in State)=True
       then Brush.Color:=clBlack
       else Brush.Color:=info_listbox.Color;     // (yellow)

    FillRect(Rect);	{ clear the rectangle }
    if (diff_flags.flags[Index]=True) or (warning_panel.Visible=False) or (gauge[gauge_listbox.ItemIndex].scale_glist=0)
       then begin
              if Brush.Color=clBlack
                 then Font.Color:=info_listbox.Color  // (invert) selected for copying
                 else Font.Color:=clBlack;
            end
       else begin
              Font.Color:=clRed;   // current differs.
              s:=s+' ('+round_str(diff_flags.cur_vals[Index],2)+' in use).';
            end;
    TextOut(Rect.Left,Rect.Top,s);
  end;//with
end;
//______________________________________________________________________________

procedure Tgauge_form.match_original_radio_buttonClick(Sender: TObject);

begin
  retain_length_checkbox.Enabled:=False;                    // 208d
  pad_form.retain_length_on_mint_menu_entry.Enabled:=False; // 208d
end;
//______________________________________________________________________________

procedure Tgauge_form.mint_new_radio_buttonClick(Sender: TObject);

begin
  retain_length_checkbox.Enabled:=True;                    // 208d
  pad_form.retain_length_on_mint_menu_entry.Enabled:=True; // 208d
end;
//______________________________________________________________________________
end.

