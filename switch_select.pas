
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

unit switch_select;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Tswitch_select_form = class(TForm)
    datestamp_label: TLabel;
    ok_panel: TPanel;
    switch_top_label: TLabel;
    ok_button: TButton;
    switch_selector_listbox: TListBox;
    cancel_panel: TPanel;
    cancel_button: TButton;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    chat_panel: TPanel;
    colour_panel: TPanel;
    colour_patch: TImage;
    custom_switch_button: TButton;
    joggled_checkbox: TCheckBox;
    show_info_button: TButton;
    help_button: TButton;
    help_label: TLabel;
    Label1: TLabel;
    switch_info_drag_label: TLabel;
    full_size_ins_radiobutton: TRadioButton;
    full_size_mm_radiobutton: TRadioButton;
    Label3: TLabel;
    help_shape: TShape;
    restore_settings_button: TButton;

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ok_panelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure colour_panelClick(Sender: TObject);
    procedure custom_switch_buttonClick(Sender: TObject);
    procedure how_panelClick(Sender: TObject);
    procedure chat_panelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure switch_selector_listboxClick(Sender: TObject);
    procedure show_info_buttonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cancel_panelClick(Sender: TObject);
    procedure restore_settings_buttonClick(Sender: TObject);

    private
      { Private declarations }
    public
      { Public declarations }
  end;

var
  switch_select_form: Tswitch_select_form;

  //---------------------------

const
  switch_help_str:string='          Switch  Size  and  Type'
  +'||-----------------------------------------'
  +'|Important note for INTERNATIONAL users:'
  +'||Templot0 uses UK track terminology. In Templot0 the term SWITCH applies to ONLY that part of a turnout comprising the moving switch blades (points).'
  +'||A complete TURNOUT comprises a SWITCH (also known as a set of points), a V-CROSSING (also known as a frog), and some closure rails (also known as closer or filler rails) linking between them.'
  +' For more information and diagrams, please refer to the Templot0 Companion pages on the Templot web site at  templot.com .'
  +'|-----------------------------------------'
  +'||Use this window to select the switch to be used for your turnout when you want to change the type of switch (see below), or when you want to set the size directly instead of using the ADJUST SIZE (F5) mouse action'
  +' or one of the common REA bullhead turnout sizes available on the TEMPLATE > QUICK SET... window.'

  +'||First select the switch you require by clicking the list.'
  +'||You may then possibly want to tick or untick the JOGGLED STOCK RAILS option box as required.'
  +'||Then click the OK button.'
  +'||Clicking the RESTORE button will restore the previous setting.'
  +'||Clicking the SHOW SWITCH INFO button lists the dimensions of the currently selected switch.'

  +'||To re-use a custom switch which has previously been reloaded or copied from a stored template, click the bottom line in the list.'

  +'||To enter your own custom switch dimensions, first click one of the 8 custom switch slots (towards the bottom of the list), and then click the SET CUSTOM SWITCH... button. For more information click the HELP buttons which then appear.'
  +' You can create up to 8 different custom switches.'
  +'||To save your custom switches in a storage box data file for re-use later, ensure that there is at least one template in the box using each custom switch.'
  +'||    Switch  Sizes  and  Selection:'

  +'||Templot0 currently provides 8 groups of pre-defined switches. These are:'

  +'||Group 1. Straight loose-heel switches in 6 sizes - 9ft, 12ft, 15ft, 18ft, 24ft and 30ft. These are suitable for bullhead or flat-bottom rail.'

  +'||Group 2. REA semi-curved flexible switches in 6 sizes - A, B, C, D, E and F. These are for bullhead rail only.'

  +'||Group 3. GWR curved flexible switches in 3 sizes - B, C and D, plus GWR straight loose-heel switch in 1 30ft size. These are for bullhead rail only.'

  +'||Group 4. GWR old-type curved loose-heel switches in 6 sizes - 9ft, 10ft, 12ft, 14ft, 15ft and 16ft, plus GWR old-type straight loose-heel switches in 2 sizes - 18ft and 20ft. These are for bullhead rail only.'

  +'||Group 5. FB-109 semi-curved flexible switches in 6 sizes - SA (for industrial sidings only), SB, SC, SD, SE and SF. These are for flat-bottom rail only.'

  +'||Group 6. BS-110A && BS-113A(inclined) curved flexible switches in 6 sizes - CA, CB, CC, CD, CE and CF. These are for flat-bottom rail only.'

  +'||Group 7. Non-prototype straight-planed model switches having a virtual heel at the end of the planing. These are suitable for bullhead or flat-bottom rail.'

  +'||Group 8. Non-prototype-specific straight-planed slip switches comprising only the planing length. These are suitable for bullhead or flat-bottom rail.'

  +'||Group 9. Custom switches used by Templot0 in creating tandem turnouts. These are suitable for bullhead or flat-bottom rail.'

  +'||When you select a switch from one of these groups, subsequent changes to the switch size using the F5 mouse action will be limited to switches within the same group.'

  +'||In each group, the switches are listed in order of increasing size (i.e. length).'
  +'||The shortest switch sizes (less than size B or 12ft) are generally only suitable for yards and sidings, and cannot usually be'
  +' curved very much, if at all, without infringing your minimum radius. (Unless the curving radius is negative, i.e. to produce a Y-turnout.)'

  +'||Size B or 12ft is the size most commonly used on small model railways. Although hardly a running-line size on the prototype,'
  +' it can serve as such on the model without looking out of place, and can be gently curved if required.'

  +'||Longer sizes are a more realistic choice for running lines where space is less of a problem, and a necessary choice when significant curving is needed.'

  +'||The longest sizes in each group are used for high-speed junctions and for long curved turnouts on sharply curved running lines. In model terms'
  +' they will be found to need a great deal of space.'

  +'||The first group in the list are straight loose-heel switches (see below). These are generally suitable for running lines on non-GWR pre-grouping railways,'
  +' and for use in yards and sidings up to the present day. Using a straight switch instead of a semi-curved one can usually save some space.'
  +' These straight switch designs are suitable for both bullhead (BH) and flat-bottom (FB) rails.'

  +'||The second group of switches are the REA (British Standard) semi-curved flexible switch designs for bullhead rail. These are the sizes which are familiar to many modellers who'
  +' have been using pre-printed plans, and are suitable for use on non-GWR lines from the grouping (1923) to the present day.'

  +'||The third and fourth groups contain bullhead switches specific to the GWR. The third group are the more modern sizes, adopted on running lines since'
  +' about 1930. The fourth group of switches are the older-pattern GWR loose-heel switches, suitable for yards and sidings and lesser used lines.'

  +'||The fifth group contains the FB-109 flat-bottom semi-curved switches introduced about 1950 as direct replacements for the REA bullhead switches.'

  +'||The sixth group contains the BS-110A and BS-113A(inclined) flat-bottom single-curved switches introduced about 1959 as an improved version of the earlier FB-109 switches.'

  +'||The seventh group contains non-prototype model switches for use in shortened turnouts when space constraints prevent the use of correct prototype switches.'
  +' Using these switches can not only save some space, but also ease the turnout-road radius.'

  +'||The eighth group contains generic non-prototype-specific switches for use when adding slip roads to a diamond-crossing to create single or double slips.'

  +'||N.B. The above notes are very general and should be taken as a rough guide only. As in all modelling, correct pointwork has to be based on'
  +' observation of the actual prototype being modelled. ("the grouping" refers to the amalgamations of U.K. railways which occurred in 1923.)';

  switch_geo_help_str:string='The type and geometry of switches can take several forms:'

  +'||a) Curved Switch:|This type is normal on the GWR (except for very long switches) and for modern flat-bottom track, but is unusual elsewhere in bullhead track.'
  +'|The switch blade is curved at a constant "switch radius" from the blade tips (called the "toe") to the "heel"'
  +' (the point where the "turnout radius" begins). The switch radius is not normally less than the turnout radius, and is often greater.'
  +' If the switch radius and turnout radius are equal, this size of switch is known as the "natural" size for the crossing angle.'
  +'||Curved switches can be either loose-heel or flexible pattern - see below.'
  +'||In model form, curved switches need careful construction to ensure that the switch blade seats properly against the stock rail,'
  +' and maintains the correct gauge.'

  +'||b) Semi-Curved Switch:|This type represents the final development of bullhead switches on non-GWR lines. These switches are'
  +' also known as REA-pattern British Standard switches.'
  +'|For the length from the tip of the blade to the end of the machined area, called the "planing", the switch blade is straight, and inclined to the'
  +' stock rail at the "planing angle". From the end of the planing to the heel the switch blade is curved at the switch radius.'
  +'||Semi-curved switches are usually of flexible construction - e.g. the REA design.'
  +'||This is the type of switch commonly used in model form, and represented on many ready-printed plans.'

  +'||c) Straight Switch:|This is the older (pre-grouping, non-GWR) type of switch and is also commonly found on narrow-gauge and industrial lines,'
  +' and Light Railways. Many straight switches can still be found in sidings and yards today.'
  +'|The switch blade is straight from the tip to the heel, which is normally the point where the offset from the stock rail is 4.5 inches,'
  +' and this length is the size of the switch. There is no switch radius, the turnout radius begins directly from the straight at the heel.'
  +'||The simple geometry of straight switches is more easily reproduced in model form, and can save some space. If you are designing a custom switch'
  +' to save space, a straight switch will probably be the most useful.'

  +'||In addition, switches can be of the older "loose-heel" pattern, or the more modern "flexible" pattern. This does not affect the length of turnouts'
  +' or the geometry of the rail edges, but does determine the position of the rail joints, which can also affect the timber spacings.'
  +'||In a loose-heel switch the switch blade is shorter and the fish-bolts at the heel are not fully tightened, so that the blade can pivot slightly at the heel'
  +' to give the required movement.'
  +' Most straight switches and n.g. and industrial switches are of this pattern.'
  +'||On post-grouping running lines flexible switches are more usual, although the GWR was later in introducing them than other main-line companies.'
  +' The switch blade is longer and is firmly held in several chairs at the heel. The movement of the blade'
  +' is achieved by actually flexing (bending) the rail from side to side. This requires that the point rodding is more robust than is necessary for loose-heel switches.'
  +' Flexible switches are also sometimes known as "spring" or "heel-less" switches.'
  +'||In model form flexible switches are the norm and generally more reliable. It''s quite posible to represent the geometry of loose-heel switches using flexible'
  +' construction - dummy rail joints can be added to represent the shorter switch blade. Templot0 is not concerned with the construction of switches, only their dimensions'
  +' and geometry, so the choice of loose-heel or flexible construction can be made after the template has been printed.'

  +'||For all these switches there is additionally the option of using joggled stock rails to accommodate the thickness of the switch blade tips. This is normally done only for facing'
  +' turnouts in running lines, except on the GWR where joggled stock rails were standard for all switches. The sideways depth of the joggle is quite small, typically 3/8" (scale), but is often'
  +' modelled overscale for functional reasons. If you wish to create your templates with overscale joggles, click the GENERATOR > GENERATOR SETTINGS > RAILS > OVERSCALE JOGGLES menu item.'

  +'||For diagrams and more prototype information about switches, see "Real Track" in the Templot0 Companion pages on the Templot web site at  templot.com .';

var
  x0001:integer;                             // keep the compiler happy.

  switch_info_showing:boolean=False;  // 0.93.a

  procedure init_switch_data;                // initial fill switch data.
  function get_switch(sw:integer):integer;   // show switch selector form.

  function get_switch_list_index(sw_group,sw_size:integer):integer;  // get switch listbox index, or -1 if not found in list.
  function set_csi_data(sw_group,sw_size:integer):boolean;           // set control template switch data from listbox entries.

//________________________________________________________

implementation

{$BOOLEVAL ON}

{$R *.lfm}

uses pad_unit, control_room, entry_sheet, colour_unit, alert_unit, help_sheet, chat_unit,
  math_unit, data_memo_unit, make_slip_unit;

var
  switch_index:integer=0;

  function get_switch_data(sw_group,sw_size:integer; var sw_info:Tswitch_info):boolean;forward;  // return switch data from listbox entries.

//______________________________________________________________________________________

function get_switch(sw:integer):integer;   // called from pad menu item.

begin
  switch_index:=sw;
  do_show_modal(switch_select_form);   // 212a ShowModal;     // get new switch size.
  RESULT:=switch_index;
end;
//_______________________________________________________________________________________

procedure Tswitch_select_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key=VK_PAUSE then Application.Minimize;    //  hide TEMPLOT on PAUSE key.
  if Key=VK_F1 then help_button.Click;
end;
//____________________________________________________________________________________________

function check_valid_switch_selected:boolean;

begin
  RESULT:=False;  // default init.

  with switch_select_form do begin
    with switch_selector_listbox do begin
      if (ItemIndex<0) or (ItemIndex>(Items.Count-1)) then EXIT;

         with Tswitch(Items.Objects[ItemIndex]) do begin

           if list_switch_info.size_code=0
              then begin
                     if alert(6,'    no  switch  selected',
                                'Please click the list for the switch which you require.',
                                '','','','?  help','','continue',4)=4 then help_button.Click;
                     restore_settings_button.Click;
                     EXIT;
                   end;

           if list_switch_info.valid_data=False
              then begin
                     if alert(6,'     no  data',
                                'No data is currently available for the switch which you have selected.'
                               +'||Please choose another.'
                               +'||Or click the SET CUSTOM SWITCH... button to enter your own switch dimensions.',
                                '','','','?  help','','continue',4)=4 then help_button.Click;
                     if list_switch_info.group_code>-1 then restore_settings_button.Click;    // restore unless a custom slot.
                     EXIT;
                   end;
         end;//with
    end;//with
  end;//with
  RESULT:=True;
end;//func
//_________________________________________________________________________________________

procedure Tswitch_select_form.ok_panelClick(Sender: TObject);

   // 205d mods to check FB or BH

var
  listed_str:string;
  i:integer;

begin
  if check_valid_switch_selected=False then EXIT;  // no switch or no data for it.

  switch_index:=switch_selector_listbox.ItemIndex;

  listed_str:=switch_selector_listbox.Items[switch_index];

  if ((Pos('REA',listed_str)>0) or (Pos('GWR',listed_str)>0)) and (rail_section=2)
     then begin
            i:=alert(4,'php/702    bullhead  switch  selected',
                       'green_panel_begintree.gif  The switch which you have selected:||`0'
                      +Trim(listed_str)
                      +'`z||is a UK prototype design for BH (bullhead) rail.|&nbsp;green_panel_end'
                      +'|The control template is currently set for FB (flat-bottom) rail.'
                      +'||The type of rail is set at the `0real > rails >`1 menu options.'
                      +'||Change to BH (bullhead) rail now?',
                       '','','','leave  as  FB  rail  and  continue','cancel','change  to  BH  rail  and  continue',0);

            if i=5 then EXIT;  // leave list showing to try another

            if i=6 then pad_form.bullhead_rails_menu_entry.Click;

          end;

  if ((Pos('FB-109',listed_str)>0) or (Pos('BS-110A',listed_str)>0) or (Pos('BS-113A',listed_str)>0)) and (rail_section=1)
     then begin
            i:=alert(4,'php/702    flat-bottom  switch  selected',
                       'green_panel_begintree.gif  The switch which you have selected:||`0'
                      +Trim(listed_str)
                      +'`z||is a UK prototype design for FB (flat-bottom) rail.|&nbsp;green_panel_end'
                      +'|The control template is currently set for BH (bullhead) rail.'
                      +'||The type of rail is set at the `0real > rails >`1 menu options.'
                      +'||Change to FB (flat-bottom) rail now?',
                       '','','','leave  as  BH  rail  and  continue','cancel','change  to  FB  rail  and  continue',0);

            if i=5 then EXIT;  // leave list showing to try another

            if i=6 then pad_form.flatbottom_rails_menu_entry.Click;

          end;


  ModalResult:=mrOk;   // not set at design time, as panel click comes here.
end;
//______________________________________________________________________________

procedure Tswitch_select_form.cancel_panelClick(Sender: TObject);   // 212a

begin
  restore_settings_button.Click;
  ModalResult:=mrCancel;
end;
//______________________________________________________________________________

procedure Tswitch_select_form.FormShow(Sender: TObject);

  // init...

begin
  switch_selector_listbox.ItemIndex:=switch_index;
  switch_selector_listbox.SetFocus;
  joggled_checkbox.Checked:=csi.joggled_stock_rail;         // show as control template.
end;
//______________________________________________________________________________

procedure Tswitch_select_form.restore_settings_buttonClick(Sender: TObject);

  // 212a  re-init...

begin
  switch_selector_listbox.ItemIndex:=switch_index;
  switch_selector_listbox.SetFocus;
  joggled_checkbox.Checked:=csi.joggled_stock_rail;         // show as control template.

  if switch_info_showing=True then show_info_button.Click;  // close it
end;
//______________________________________________________________________________

procedure Tswitch_select_form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin
  switch_selector_listbox.ItemIndex:=switch_index;     // restore previous
end;
//________________________________________________________________________________________

procedure Tswitch_select_form.colour_panelClick(Sender: TObject);

begin
  Color:=get_colour('choose  a  new  colour  for  the  switch  selector  dialog',Color);
end;
//________________________________________________________________________________________

procedure Tswitch_select_form.custom_switch_buttonClick(Sender: TObject);

const
   custom_help_str:string='Custom  Switch'

    +'||Before entering data for a custom switch you ideally need to have access to the appropriate prototype information. It is possible to save some'
    +' space in model form by designing non-prototype short switches, but unless these are based on correct practice the results are unlikely to be satisfactory.'
    +' If you enter incompatible dimensions Templot0 may decline to draw your switch.'
    +'||When entering custom switch data, the dimensions refer to the right-hand blade of a left-hand switch (looking from the tips towards the heel), and are all in FULL-SIZE inches'
    +' (including the switch radius, which is normally shown on drawings in feet, so multiply by 12).';

  lead_help_str:string='This is the distance from the tip of the blade (switch rail) to the point known as the "heel". This point is used in the calculation of the turnout radius.';

  offset_help_str:string='This is the dimension from the running face of the stock rail to the running face of the switch rail at the heel point. This dimension is normally 4.5 inches for loose-heel'
                        +' switches in standard 2.75 inch wide bullhead rail (thus giving the standard 1.75 inch flangeway between the two rails at this point).||For flexible switches the heel offset'
                        +' is usually about 10-11 inches.';

  joint_help_str:string='This is the switch front distance from the stock rail joint at the left end of the turnout to the "toe" (blade tips). For GWR switches this dimension is normally 64 inches, elsewhere it'
                       +' is usually 65 inches. There are normally 2 standard sleepers occupying this space.'
                       +'||Handy Hint:'
                       +'|For some custom switches you may not want to have a switch front section. In this case a distance of about 2 inches should be entered. (Entering zero may prevent the switch blade tips from being generated properly.)'
                       +' Then use the CTRL-2 TOE position for template pegging purposes.';

  rail_help_str:string='This rail-length dimension is for reference only and does not affect the geometry or calculations. It is needed to enable the rail joint to be marked in the correct place.'
                      +'||The timber spacings should be so arranged that the joint lies centrally between a pair timbers. For standard bullhead track joint timbers are normally at 24" or 25" centres.'
                      +' For UK flat-bottom track joint timbers are normally at 25" or 26" centres.'
                      +'||If a rail length of zero is entered, no rail joint mark will be generated.';

  radius_help_str:string='This is the switch radius, the radius of curvature of the switch blade. This is normally shown on drawings in feet, but should be entered'
                        +' here in INCHES (multiply by 12).'
                        +'||For a curved switch, this radius applies from the blade tips to the switch heel.'
                        +'||For a semi-curved switch, this radius applies from the end of the planing (machined section) to the switch heel. The planing section is straight.';

  planing_help_str:string='This is the distance from the tip of the blade (switch rail) to the end of the machined section, i.e. to the point where the switch rail is the full rail-width.';

  angle_help_str:string='This is the switch deflection angle, and the angle machined on the switch rail (blade) to reduce it to a sharp point at the tip. This angle should be entered as a unit value,'
                       +' which can be found by dividing the planing length (in inches) by the rail-width (in inches). (For standard-gauge bullhead track the rail-width is normally 2.75 inches.)';

  fixed_spacing_help_str:string='      Fixed  Timber  Spacings'
  +'||If you are simply experimenting with a custom switch, Templot0 can fill in the timber positions for you at a constant spacing.'
  +' This will not be per-prototype of course, but it will look the part and save time.'
  +'||The first 3 timbers from the rail joint to the toe (blade tips) will be spaced according to the current settings.'
  +'||The maximum fill spacing for the remaining timbers can be set from the REAL > TIMBERING > TIMBERING DATA... menu item.'
  +'||If you intend to actually construct this switch you will probably need to adjust the rail lengths so that the rail joints lie centrally between a pair of timbers.';

  joggled_help_str:string='      Joggled  Stock  Rails'
  +'||For a diagram and further notes explaining the use and dimensioning of joggles, see also the "real track" page on the Templot web site at  templot.com .'
  +'||The switch stock rails are sometimes joggled outwards by a small amount to create a housing for the switch blade tips and so protect them from wheel damage.'
  +' The joggled section is always created by bending the rail rather than by machining a notch.'
  +'||REA and straight switches can be either joggled or not - normally joggled switches are used only in facing positions on running lines. In trailing positions there is a danger of rough running'
  +' when wheels hit the joggle on the open switch blade side.'
  +'||However, all GWR switches are joggled.'
  +'||Having entered joggle dimensions for your custom switch, you can choose whether to actually use the joggles for an individual template by means of the JOGGLED STOCK RAILS tickbox.';

  joggle_length_help_str:string='      Return  Length  of  Joggle'
  +'||Enter a dimension in full-size prototype INCHES for the return length of the joggle. This is the distance in front of the switch blade tips in which the rail returns to its normal alignment.'
  +'||For REA and GWR flexible switches this dimension is 6 inches.'
  +'||For GWR old-type switches this dimension is 4 inches.'
  +'||Templot0 puts a guide mark across the rail at this position.';

  joggle_depth_help_str:string='      Joggle  Depth'
  +'||Enter a dimension in full-size prototype INCHES for the maximum depth of the joggle. This is the amount by which the rail is deflected sideways at the position of the switch blade tips.'
  +'||For REA and GWR old-type switches this dimension is 3/8" (0.375 inches).'
  +'||For GWR flexible switches this dimension is 1/4" (0.25 inches).'
  +'||These dimensions scale down to only a few thou in the common model sizes and are frequently exaggerated for functional reasons.';

  adopt_help_str:string='      Adopt  Switch  from  Control  Template  as  Custom  Switch'
  +'||This option is useful when a stored template containing a custom switch has been reloaded from a data file.'
  +'||Copy it to the control template, and then use this option to adopt the switch data into one of the custom slots.'
  +'||The data will also be automatically entered into the bottom slot in the list, but will not be preserved there'
  +' if a subsequent control template has a different custom switch.'
  +'||N.B. Although adopting the switch from the control template serves no apparent purpose if the control template contains a standard switch,'
  +' this can be a useful way of pre-setting the custom switch data before modifying it.';

  front_timb_help_str:string='    Switch  Front  Timbering'
  +'||The timbering in the switch front section can use either plain track sleepers or turnout timbers.'
  +'||These are the timbers (usually two of them) between the joint in the stock rail (CTRL-1 peg position) and the switch toe (blade tips) (CTRL-2 peg position). They are numbered J1 and J2 on the templates.'
  +'||For most pre-grouping practice they should be plain track sleepers, i.e. normally 10" wide and 5" thick.'
  +'||For most post-grouping practice they should be turnout timbers, i.e. normally 12" wide and 6" thick.'
  +'||These dimensions apply to British standard-gauge track. The actual sizes used will correspond to the settings made in the REAL > TIMBERING > TIMBERING DATA... menu item.'
  +'||In both cases the length of these timbers matches the current length of plain track sleepers.'
  +'||If more specific prototype information is available, sizes can be adjusted for each individual template using the SHOVE TIMBER functions.'
  +'||In most cases there are 2 timbers in the switch front, but Templot0 can generate up to 5 of them if needed (J1-J5). If more than this are needed they can be added on each individual template as BONUS timbers.';

  { Tswitch_info:

  planing        // (B) planing length (inches).
  heel_lead_inches      // (L) lead to heel (incl. planing) (inches).
  heel_offset_inches    // (H) heel-offset (inches).
  pattern        // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  toe_end        // switch-front (stock-rail-end to toe) (inches).

  valid_data     // True = valid data here.

  planing_angle  // unit planing angle.
  switch_radius_inchormax  // switch radius (inches!) (or max_rad (in mm) for straight switch).
  switch_rail    // (C) length of switch rail (inches).
  stock_rail     // (S) length of stock rail (inches).

  sleeper_j1     // first switch-front sleeper spacing back from TOE (NEGATIVE inches).
  sleeper_j2     // second switch-front sleeper spacing back from the first (NEGATIVE inches).
  sleeper_j3     // third switch-front sleeper spacing back from the second (NEGATIVE inches).
  sleeper_j4     // fourth switch-front sleeper spacing back from the third (NEGATIVE inches).
  sleeper_j5     // fifth switch-front sleeper spacing back from the fourth (NEGATIVE inches).

  front_timbered // True = switch front sleepers are timber width.

  planing_radius // planing radius for double-curved switch.

  joggle_depth   // depth of joggle. 0.71.a 13-4-01.
  joggle_length  // length of joggle in front of toe (+ve). 0.71.a 13-4-01.
  joggled_stock  // joggle flag.
  fb_tip_offset  // 0.76.a  2-1-02. fbtip dimension (FB foot from gauge-face at tip).
}

var
  i,ii,m,n:integer;
  item_index:integer;
  od:Toutdim;
  temp_flag:boolean;
  help_str:string;
  data_caption_str:string;
  temp_str,type_str,exists_str,custom_str,front_str:string;
  existing_pattern:integer;

  new_switch_info:Tswitch_info;

begin
  help_str:=custom_help_str+'||'+switch_geo_help_str;

  with switch_selector_listbox do begin

    if (ItemIndex<0) or (ItemIndex>(Items.Count-1)) then EXIT;

    if ItemIndex=(Items.Count-1)
       then begin
              alert(6,'    control  template  custom  slot  selected',
                      'The bottom slot in the list is reserved for the custom switch settings from the most recent control template which contained a custom switch.'
                     +'||The switch data in this slot is maintained automatically by Templot, it is not possible to change it manually.'
                     +'||You can select this custom switch for the control template if the curent template is currently using one of the standard switches.'
                     +'||To create your own custom switch, please first click one of the 8 custom slots in the list to select the one for which you wish to enter custom switch data.'
                     +'||You can have a different custom switch in each of the 8 slots.',
                      '','','','','','O K',0);
              restore_settings_button.Click;
              EXIT;
            end;

    with Tswitch(Items.Objects[ItemIndex]) do begin

      if list_switch_info.group_code>-1
         then begin
                alert(6,'    no  custom  slot  selected',
                        'Please click one of the 8 custom slots in the list (near the bottom of the list) to select the one for which you wish to enter custom switch data.'
                       +'||You can have a different custom switch in each of the 8 slots.'
                       +'||It is not possible to change the data for the standard switches in the upper slots.',
                        '','','','','','O K',0);
                restore_settings_button.Click;
                EXIT;
              end;

      if list_switch_info.valid_data=True
         then begin
                temp_str:=list_switch_info.sw_name_str;   // !!! (Delphi bug - compiler gets confused about parameter string length if not assigned to a local string.)

                if alert(7,'    existing  custom  switch  ( slot  '+IntToStr(ABS(list_switch_info.group_code))+' )',
                           '||You have an existing custom switch in this slot:||  '
                          +temp_str
                          +'||Entering new data will replace or modify the existing custom switch in this slot.'
                          +'||If you will need this switch again, you may prefer to cancel and select a different slot.'
                          +' You can have a different custom switch in each of the 8 slots.',
                           '','','','','cancel  -  no  change','enter  new  or  modified  custom  switch  data  in  slot  '+IntToStr(ABS(list_switch_info.group_code))+'      ',0)=5
                   then EXIT;
              end;

      item_index:=ItemIndex;  // save for reset.

      repeat
        i:=alert(4,'  adopt  switch  from  control  template ?',
                   'Do you want to adopt the switch from the control template as this custom switch?',
                   '','','?  help','yes  -  adopt  switch  from  control  template  for  slot  '+IntToStr(ABS(list_switch_info.group_code))+'      ','cancel  -  no  change','no  -  enter  new  custom  switch  for  slot  '+IntToStr(ABS(list_switch_info.group_code)),3);

        if i=3 then alert_help(0,adopt_help_str,'');
      until i<>3;

      if i=4              // adopt control template switch (now in csi)...
         then begin
                new_switch_info:=csi;

                Items.Strings[ItemIndex]:='  custom  '+IntToStr(ABS(list_switch_info.group_code))+' :  '+csi.sw_name_str;

                new_switch_info.group_code:=list_switch_info.group_code;  // -1 to -8 for custom switches.
                new_switch_info.size_code:=1;         // size within group (0=empty slot, 1=shortest). 0.77.a  7-6-02.
                new_switch_info.group_count:=1;       // number of switches in this group (max csi.size_code in this group, min size is always 1).

                list_switch_info:=new_switch_info;
                ItemIndex:=item_index;               // Delphi bug? - needed to show new string selected.

                switch_selector_listbox.ItemIndex:=item_index;  // 208a
                if switch_info_showing=True then show_switch_info(full_size_mm_radiobutton.Checked,switch_info_showing);  // 208a
                switch_selector_listbox.SetFocus;
                EXIT;
              end;

      if i=5 then begin
                    restore_settings_button.Click;

                    switch_selector_listbox.ItemIndex:=item_index;  // 208a
                    if switch_info_showing=True then show_switch_info(full_size_mm_radiobutton.Checked,switch_info_showing);  // 208a
                    EXIT;
                  end;

      with math_form do begin

        if list_switch_info.valid_data=False
           then math_editbox.Text:='my custom switch'
           else math_editbox.Text:=list_switch_info.sw_name_str;

        Caption:='  custom  switch  name ...';
        big_label.Caption:=insert_crlf_str('|||||Enter below a name or reference for your custom switch.'
                                          +'||(If you cancel or leave this blank your switch will be named "untitled custom switch".)');

        do_show_modal(math_form);  // 212a   ShowModal

        if (math_editbox.Text='') or (ModalResult<>mrOK)
           then custom_str:='untitled custom switch'
           else custom_str:=math_editbox.Text;

        Caption:='    '+Application.Title;   // reset form caption.
      end;//with math_form

      if list_switch_info.valid_data=True
         then begin
                case list_switch_info.sw_pattern of
                   -1: begin
                         type_str:='semi - curved';
                         existing_pattern:=-1;
                       end;

                    0: if list_switch_info.switch_radius_inchormax>max_rad_test
                          then begin
                                 type_str:='straight';
                                 existing_pattern:=0;
                               end
                          else begin
                                 type_str:='curved';
                                 existing_pattern:=2;
                               end;

                    1: begin
                         type_str:='double - curved';
                         existing_pattern:=1;
                       end;

                  else begin                // ???
                         type_str:='';
                         existing_pattern:=0;
                       end;

                end;//case
                temp_str:='as  existing  custom  switch  ( '+type_str+'  switch )';
                exists_str:='||If you are intending to modify the existing custom switch in this slot, click AS EXISTING CUSTOM SWITCH.';
              end
         else begin
                temp_str:='';
                exists_str:='';
                existing_pattern:=0;     // should be ignored.
              end;

      repeat
        i:=alert(4,'  data  for  '+custom_str+' ?',
                   '||Please first select the type of switch for'
                  +'||'+custom_str+exists_str,
                   'more  information',temp_str,'curved  switch','semi - curved  switch','cancel','straight  switch',1);
        if i=1 then alert_help(0,help_str,'');
      until i<>1;

      if i=5
         then begin
                restore_settings_button.Click;
                switch_selector_listbox.ItemIndex:=item_index;  // 208a
                if switch_info_showing=True then show_switch_info(full_size_mm_radiobutton.Checked,switch_info_showing);  // 208a
                EXIT;
              end;

      if i=2           // same as existing type...
         then begin
                case existing_pattern of
                                   -1: i:=4;   // semi-curved.
                                    0: i:=6;   // straight.
                                  1,2: i:=3;   // double-curved, curved.
                                  else i:=6;   // ??? straight.
                end;//case
              end;

      if i=3
         then begin    // curved switch...

                 if (list_switch_info.sw_pattern<>0)                        // 0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
                 or (list_switch_info.switch_radius_inchormax>max_rad_test) // straight switch.
                 or (list_switch_info.valid_data=False)
                    then begin
                           if get_switch_data(3,1,new_switch_info)=False  // set GWR B default curved switch if existing data not a curved switch.
                              then run_error(82);                         // ?????? no GWR B switch in list?
                         end
                    else new_switch_info:=list_switch_info;               // use existing data as default.

                 data_caption_str:='custom  curved  switch';

                 new_switch_info.planing:=0;         // no planing length specified - curved switch.
                 new_switch_info.sw_pattern:=0;         // flag curved switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
                 new_switch_info.planing_angle:=0;   // no planing angle specified - curved switch.

                 putdim(  lead_help_str,2,'lead length to heel (incl. planing, full-size inches)', new_switch_info.heel_lead_inches,       True,True,True,False);  // no neg, no preset, no zero, don't terminate on zero.
                 putdim(offset_help_str,2,'offset at the heel (full-size inches)',                 new_switch_info.heel_offset_inches,     True,True,True,False);  // ditto...
                 putdim(radius_help_str,2,'switch radius in FULL-SIZE inches',                     new_switch_info.switch_radius_inchormax,True,True,True,False);
                 putdim( joint_help_str,2,'stock-rail joint to toe (full-size inches)',            new_switch_info.switch_front_inches,    True,True,False,False); // no neg, no preset, zero ok, don't terminate on zero.
                 putdim(  rail_help_str,2,'length of switch rail (blade) (full-size inches)',      new_switch_info.switch_rail,            True,True,False,False); // ditto...
              n:=putdim(  rail_help_str,2,'length of stock rail from joint (full-size inches)',    new_switch_info.stock_rail,             True,True,False,False);

                 if n<>5 then run_error(201);
                 if getdims(data_caption_str,help_str,switch_select_form,n,od)=True
                    then begin
                           new_switch_info.heel_lead_inches:=od[0];         // lead to heel.
                           new_switch_info.heel_offset_inches:=od[1];       // offset at heel.
                           new_switch_info.switch_radius_inchormax:=od[2];  // switch radius (INCHES).
                           new_switch_info.switch_front_inches:=od[3];      // joint to toe.
                           new_switch_info.switch_rail:=od[4];      // switch rail length.
                           new_switch_info.stock_rail:=od[5];       // stock rail length.
                         end;
              end;//3:

      if i=4
         then begin    // semi-curved switch ...

                 if (list_switch_info.sw_pattern<>-1)          // 0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
                 or (list_switch_info.valid_data=False)
                    then begin
                           if get_switch_data(2,2,new_switch_info)=False  // set REA B default semi-curved switch if existing data not a semi-curved switch.
                              then run_error(82);                         // ?????? no REA B switch in list?
                         end
                    else new_switch_info:=list_switch_info;               // use existing data as default.

                 data_caption_str:='custom  semi - curved  switch';

                 new_switch_info.heel_offset_inches:=0;   // heel_offset calculated for semi-curved switch.
                 new_switch_info.sw_pattern:=-1;      // flag semi-curved switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.

                 putdim(planing_help_str,2,'straight planing length (full-size inches)',            new_switch_info.planing,                True,True,True,False);
                 putdim(  angle_help_str,0,'straight planing unit angle 1:',                        new_switch_info.planing_angle,          True,True,True,False);
                 putdim(   lead_help_str,2,'lead length to heel (incl. planing, full-size inches)', new_switch_info.heel_lead_inches,       True,True,True,False);  // no neg, no preset, no zero, don't terminate on zero.
                 putdim( radius_help_str,2,'switch radius in FULL-SIZE inches',                     new_switch_info.switch_radius_inchormax,True,True,True,False);
                 putdim(  joint_help_str,2,'stock-rail joint to toe (full-size inches)',            new_switch_info.switch_front_inches,    True,True,False,False); // no neg, no preset, zero ok, don't terminate on zero.
                 putdim(   rail_help_str,2,'length of switch rail (blade) (full-size inches)',      new_switch_info.switch_rail,            True,True,False,False);
              n:=putdim(   rail_help_str,2,'length of stock rail from joint (full-size inches)',    new_switch_info.stock_rail,             True,True,False,False);

                 if n<>6 then run_error(202);
                 if getdims(data_caption_str,help_str,switch_select_form,n,od)=True
                    then begin
                           new_switch_info.planing:=od[0];        // planing length.
                           new_switch_info.planing_angle:=od[1];  // planing unit angle.
                           new_switch_info.heel_lead_inches:=od[2];        // lead to heel.
                           new_switch_info.switch_radius_inchormax:=od[3]; // switch radius (INCHES).
                           new_switch_info.switch_front_inches:=od[4];     // joint to toe.
                           new_switch_info.switch_rail:=od[5];    // switch rail length.
                           new_switch_info.stock_rail:=od[6];     // stock rail length.
                         end;
              end;//4:


      if i=6
         then begin                    // straight switch...

                 if (list_switch_info.sw_pattern<>0)                        // 0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
                 or (list_switch_info.switch_radius_inchormax<max_rad_test) // curved switch.
                 or (list_switch_info.valid_data=False)
                    then begin
                           if get_switch_data(1,2,new_switch_info)=False  // set 12ft default straight switch if existing data not a straight switch.
                              then run_error(82);                         // ?????? no 12ft straight switch in list?
                         end
                    else new_switch_info:=list_switch_info;               // use existing data as default.

                 data_caption_str:='custom  straight  switch';

                 new_switch_info.planing:=0;         // no planing length specified - straight switch is calculated.
                 new_switch_info.sw_pattern:=0;         // flag curved switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
                 new_switch_info.planing_angle:=0;   // no planing angle specified - straight switch is calculated.

                 putdim(  lead_help_str,2,'lead length to heel (incl. planing, full-size inches)', new_switch_info.heel_lead_inches,   True,True,True,False);  // no neg, no preset, no zero, don't terminate on zero.
                 putdim(offset_help_str,2,'offset at the heel (full-size inches)',                 new_switch_info.heel_offset_inches, True,True,True,False);  // ditto.
                 putdim( joint_help_str,2,'stock-rail joint to toe (full-size inches)',            new_switch_info.switch_front_inches,True,True,False,False); // no neg, no preset, zero ok, don't terminate on zero.
                 putdim(  rail_help_str,2,'length of switch rail (blade) (full-size inches)',      new_switch_info.switch_rail, True,True,False,False); // ditto..
              n:=putdim(  rail_help_str,2,'length of stock rail from joint (full-size inches)',    new_switch_info.stock_rail,  True,True,false,False);

                 if n<>4 then run_error(203);
                 if getdims(data_caption_str,help_str,switch_select_form,n,od)=True
                    then begin
                           new_switch_info.heel_lead_inches:=od[0];           // lead to heel.
                           new_switch_info.heel_offset_inches:=od[1];         // offset at heel.
                           new_switch_info.switch_radius_inchormax:=max_rad;  // for straight switch.
                           new_switch_info.switch_front_inches:=od[2];        // joint to toe.
                           new_switch_info.switch_rail:=od[3];                // switch rail length.
                           new_switch_info.stock_rail:=od[4];                 // stock rail length.
                         end;
              end;//6:

      n:=calc_switch(new_switch_info,False,False);    // check dims will calculate ok.
      if n<>0
         then begin
                alert(6,'    invalid  switch',
                        '||Sorry, your custom switch data is unusable.'
                       +'||The switch error code is  '+IntToStr(n)+' .'
                       +'||Please refer to the Templot0 documentation for information about this switch error code.',
                        '','','','','O K  -  cancel  custom  switch','',0);
                new_switch_info.valid_data:=False;
                restore_settings_button.Click;
                switch_selector_listbox.ItemIndex:=item_index;  // 208a
                if switch_info_showing=True then show_switch_info(full_size_mm_radiobutton.Checked,switch_info_showing);  // 208a
                EXIT;
              end;

      repeat
        i:=alert(4,'    joggled  stock  rails ?',
                   'Do you want to specify the joggled stock rails option for this custom switch?',
                   '','','?  help','yes  -  make  joggled  stock  rails  optional','','no  -  plain  stock  rails  only',3);
        if i=3 then alert_help(0,joggled_help_str,'');
      until i<>3;

      case i of
               4: begin   // get joggle data.

                    putdim( joggle_depth_help_str,2,'sideways depth of joggle (full-size inches)',            new_switch_info.joggle_depth, True,True,True,False);   // no neg, no preset, no zero, don't terminate on zero.
                 n:=putdim(joggle_length_help_str,2,'joggle length in front of blade tips (full-size inches)',new_switch_info.joggle_length,True,True,True,False);   // no neg, no preset, no zero, don't terminate on zero.

                    if n<>1 then run_error(229);
                    if getdims(data_caption_str,joggled_help_str,switch_select_form,n,od)=True
                       then begin
                              new_switch_info.joggle_depth:=ABS(od[0]);  // joggle depth.
                              new_switch_info.joggle_length:=ABS(od[1]); // joggle length.
                              new_switch_info.joggled_stock_rail:=True;  // use joggle.
                              switch_select_form.joggled_checkbox.Checked:=True;
                            end;
                  end;

             else begin
                    new_switch_info.joggle_depth:=0.375;          // joggle depth (zero not valid).
                    new_switch_info.joggle_length:=6;             // joggle length (zero not valid).
                    new_switch_info.joggled_stock_rail:=False;    // not joggled.
                    switch_select_form.joggled_checkbox.Checked:=False;
                  end;
      end;//case

      repeat
        i:=alert(4,'    custom  switch  timbering ?',
                   'Do you want to enter the timber spacings for this custom switch?'
                  +'||( All dimensions should be entered in FULL-SIZE inches, and to the centre of the timber.)',
                   '','','','?  help','no  -  let  Templot0  use  fixed  spacings','yes  -  let  me  enter  the  spacings',4);
        if i=4 then alert_help(0,fixed_spacing_help_str,'');
      until i<>4;

      if i=6
         then begin        // get spacings.
                repeat
                  ii:=alert(4,'    switch  front  timbering ?',
                              'Is the switch front section timbered or sleepered?',
                              '','','?  help','plain  track  sleepers','','turnout  timbers',3);

                  if ii=3 then alert_help(0,front_timb_help_str,'');
                until ii<>3;

                if ii=4
                   then begin
                          new_switch_info.front_timbered:=False;
                          front_str:='sleeper';
                        end
                   else begin
                          new_switch_info.front_timbered:=True;
                          front_str:='timber';
                        end;

                putdim('',2,'spacing back from TOE to first front '+front_str+' J1',  ABS(new_switch_info.sleeper_j1),True,True,False,False); // no neg, no preset, zero ok, don't terminate on zero.
                putdim('',2,'spacing back from J1 to next front '+front_str+' J2',  ABS(new_switch_info.sleeper_j2),True,True,False,False);          // ditto...
                putdim('',2,'spacing back from J2 to next front '+front_str+' J3',  ABS(new_switch_info.sleeper_j3),True,True,False,False);          // ditto...
                putdim('',2,'spacing back from J3 to next front '+front_str+' J4',  ABS(new_switch_info.sleeper_j4),True,True,False,False);          // ditto...
                putdim('',2,'spacing back from J4 to next front '+front_str+' J5',  ABS(new_switch_info.sleeper_j5),True,True,False,False);          // ditto...

             n:=putdim('    Switch  Toe ( Blade  Tips )  to  Timber  Centre  S1.||For bullhead REA semi-curved switches and flat-bottom switches this dimension is 3.5 inches. For GWR practice it is 4 inches.'
                       +' Otherwise it is normally 4 inches or 1/3rd of the timber width.'
                       +'||( But on some pre-grouping switches the blade tips lie on the timber centre-line (e.g. NER), in which case this dimension would be 0.)',
                        2,'spacing forward from TOE to first timber S1',new_switch_info.timber_centres[0],False,True,False,False);  // 0.91.b  allow neg or zero.
                if n<>5 then run_error(211);
                temp_flag:=getdims('custom  switch  front  spacings',front_timb_help_str,switch_select_form,n,od);
              end
         else temp_flag:=False;

      if temp_flag=True           // user spacings, and got some...
         then begin
                new_switch_info.sleeper_j1:=0-ABS(od[0]);      // toe to first sleeper.
                new_switch_info.sleeper_j2:=0-ABS(od[1]);      // first to next sleeper.
                new_switch_info.sleeper_j3:=0-ABS(od[2]);      // first to next sleeper.
                new_switch_info.sleeper_j4:=0-ABS(od[3]);      // first to next sleeper.
                new_switch_info.sleeper_j5:=0-ABS(od[4]);      // first to next sleeper.

                new_switch_info.timber_centres[0]:=ABS(od[5]); // toe to toe-timber centre.

                i:=1;        // starting point for next 8 spacings.
                repeat

                  for m:=0 to 7 do
                  n:=putdim('Terminate this list of spacings by entering a zero.',
                             2,'spacing forward to timber number '+IntToStr(i+m+1),new_switch_info.timber_centres[(i+m)],True,True,False,True);  // zero ok - terminates the list.
                  if n<>7 then run_error(212);
                  if getdims('custom  switch  timber  spacings','',switch_select_form,n,od)=True
                     then begin
                            if (new_switch_info.timber_centres[(i-1)]<>0) or (i=1) then new_switch_info.timber_centres[i  ]:=ABS(od[0]) else BREAK;  // 0.91.c  "or (i=1)" bug fix to allow zero toe-to-timber (NER switches).
                            if new_switch_info.timber_centres[ i   ]<>0 then new_switch_info.timber_centres[i+1]:=ABS(od[1]) else BREAK;
                            if new_switch_info.timber_centres[(i+1)]<>0 then new_switch_info.timber_centres[i+2]:=ABS(od[2]) else BREAK;
                            if new_switch_info.timber_centres[(i+2)]<>0 then new_switch_info.timber_centres[i+3]:=ABS(od[3]) else BREAK;
                            if new_switch_info.timber_centres[(i+3)]<>0 then new_switch_info.timber_centres[i+4]:=ABS(od[4]) else BREAK;
                            if new_switch_info.timber_centres[(i+4)]<>0 then new_switch_info.timber_centres[i+5]:=ABS(od[5]) else BREAK;
                            if new_switch_info.timber_centres[(i+5)]<>0 then new_switch_info.timber_centres[i+6]:=ABS(od[6]) else BREAK;
                            if new_switch_info.timber_centres[(i+6)]<>0 then new_switch_info.timber_centres[i+7]:=ABS(od[7]) else BREAK;
                          end
                     else begin
                            new_switch_info.timber_centres[i]:=0;    // cancelled form, so terminate list right here.
                            BREAK;
                          end;
                INC(i,8);
                until i>(swtimbco_c-8);   // list full (no space for another 8 plus end marker).

                new_switch_info.timber_centres[swtimbco_c]:=0;  // ensure there is always an end marker.
              end
         else begin                       // fixed spacings, or spacing entry abandoned...

                new_switch_info.front_timbered:=True;

                new_switch_info.sleeper_j1:=0-24.5;
                new_switch_info.sleeper_j2:=0-27.5;
                new_switch_info.sleeper_j3:=0;
                new_switch_info.sleeper_j4:=0;
                new_switch_info.sleeper_j5:=0;

                new_switch_info.timber_centres[0]:=4;        // 4" tips to first timber.
                new_switch_info.timber_centres[1]:=0;        // starts fill timbering.
              end;

      new_switch_info.sw_name_str:=custom_str;  // add the name;

      new_switch_info.group_code:=list_switch_info.group_code;  // -1 to -8 for custom switches.
      new_switch_info.size_code:=1;         // size within group (0=empty slot, 1=shortest). 0.77.a  7-6-02.
      new_switch_info.group_count:=1;       // number of switches in this group (max size_code in this group, min size is always 1).

      list_switch_info:=new_switch_info;    // put it all in the slot.
      Items.Strings[ItemIndex]:='  custom  '+IntToStr(ABS(list_switch_info.group_code))+' :  '+custom_str;     // put new name in list.
      ItemIndex:=item_index;               // Delphi bug? - needed to show new string selected.
    end;//with
  end;//with

  switch_selector_listbox.ItemIndex:=item_index;  // 208a
  if switch_info_showing=True then show_switch_info(full_size_mm_radiobutton.Checked,switch_info_showing);  // 208a

  switch_selector_listbox.SetFocus;
end;
//________________________________________________________________________________________

procedure Tswitch_select_form.how_panelClick(Sender: TObject);

begin
  help(0,switch_help_str+'||'+switch_geo_help_str,'');
end;
//_____________________________________________________________________________________________

procedure Tswitch_select_form.chat_panelClick(Sender: TObject);

const
  chat_str:string='    Switches Chat'
  +'||If you are using a custom switch based on prototype information, I would be very pleased to learn the details,'
  +' so that I can offer an even wider choice of pre-set switch sizes in this list in later versions.'
  +'||Please also consider sharing your custom switch with other users via the Templot0 Club forum - details are on the Templot web site at templot.com';

begin
  chat(chat_str);
end;
//_______________________________________________________________________________________

procedure Tswitch_select_form.FormCreate(Sender: TObject);

begin
  if Screen.Height<500 then Top:=4;    // move form up the screen for lo-res.

  AutoScroll:=False;
end;
//______________________________________________________________________________

procedure Tswitch_select_form.switch_selector_listboxClick(Sender: TObject);

              // set joggled tickbox from selected switch..
begin
  with switch_selector_listbox do begin
    if (ItemIndex<0) or (ItemIndex>(Items.Count-1)) then EXIT;
    joggled_checkbox.Checked:=Tswitch(Items.Objects[ItemIndex]).list_switch_info.joggled_stock_rail;

          // 0.93.a mods ...

    if (switch_info_showing=True) and (data_child_form.Visible=True)
       then begin
              data_child_form.data_memo.Text:='';

              with Tswitch(Items.Objects[ItemIndex]).list_switch_info do begin

                if size_code=0 then EXIT;

                if valid_data=False then EXIT;

              end;//with

              show_switch_info(full_size_mm_radiobutton.Checked,switch_info_showing);
            end;
  end;//with
end;
//______________________________________________________________________________________

procedure Tswitch_select_form.show_info_buttonClick(Sender: TObject);

begin
  if check_valid_switch_selected=False then EXIT;  // no switch or no data for it.

  if switch_info_showing=True
     then begin
            data_child_form.Close;
            show_info_button.Caption:='show  switch  &info';
            switch_info_showing:=False;
          end
     else begin
            show_switch_info(full_size_mm_radiobutton.Checked,switch_info_showing);
            show_info_button.Caption:='hide  switch  &info';
            switch_info_showing:=True;
          end;

  if switch_selector_listbox.Visible then switch_selector_listbox.SetFocus;
end;
//______________________________________________________________________________________

function get_switch_list_index(sw_group,sw_size:integer):integer;  // get switch listbox index, or -1 if not found in list.

var
  n:integer;

begin
  RESULT:=-1;  // default init.
  try
    with switch_select_form.switch_selector_listbox.Items do begin

      if Count<1 then EXIT;

      for n:=0 to (Count-1) do begin

        with Tswitch(Objects[n]).list_switch_info do begin

          if (group_code<>sw_group) or (size_code<>sw_size) then CONTINUE;

               // found list entry...

          if valid_data=False then EXIT;

          RESULT:=n;
          EXIT;

        end;//with
      end;//for
    end;//with Items
  except
    EXIT;
  end;//try
end;//func;
//__________________________________________________________________________________________

function get_switch_data(sw_group,sw_size:integer; var sw_info:Tswitch_info):boolean;  // return switch data from listbox entries.

var
  n:integer;

begin
  RESULT:=False;      // default init.
  try
    n:=get_switch_list_index(sw_group,sw_size);  // get switch listbox index, or -1 if not found in list.
    if n<0 then EXIT;

    sw_info:=Tswitch(switch_select_form.switch_selector_listbox.Items.Objects[n]).list_switch_info;
    RESULT:=sw_info.valid_data;
  except
    EXIT;
  end;//try
end;
//___________________________________________________________________________________________

function set_csi_data(sw_group,sw_size:integer):boolean; // set control template switch data from listbox entries.

var
  csi_switch_info:Tswitch_info;

begin
  RESULT:=False;      // default init.
  try
    if get_switch_data(sw_group,sw_size,csi_switch_info)=False then EXIT;    // get data to csi_switch_info.
    if set_csi_from_switch_info(csi_switch_info)=False then EXIT;            // and make it current in csi.
    RESULT:=True;
  except
    EXIT;
  end;//try
end;//func
//________________________________________________________________________________________

procedure init_switch_data;    // this routine runs once only on startup.
                               // called from templot_init.

{               old sizes of switch (pre 0.77.a)...

                   0-5  = straight switches - 6 sizes 9', 12', 15', 18', 24', 30'.
                    6   = user-defined custom switch.

                   7-12 = REA semi-curved switches - 6 sizes A+, B+, C+, D+, E+, F*.

                  13-20 = GWR old-type heel switches - 8 sizes 9', 10', 12', 14', 15', 16', 18', 20'.

                    21  = GWR B curved switch.
                    22  = GWR C ditto.
                    23  = GWR D ditto.
                    24  = GWR 30ft straight switch.

old 20 dims for each switch. :

     0 = straight planing length (inches) for semi-curved P (0 for curved planing or straight switch - we calculate it).
     1 = lead to heel (incl. planing) (inches). LH
     2 = heel-offset (inches). H (0 for semi-curved - we calculate it).
     3 = type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
     4 = JOINT (stock-rail-end) to toe (inches) (stock rail front).

     5 = valid data. 0=nothing here. <>0=valid data here.

     6 = planing unit angle for semi-curved k1n (0 for curved planing or straight switch - we calculate it).
     7 = switch radius (inches!). (max_rad mm for straight switch).
     8 = length of switch rail (inches) TOE to rail joint.
     9 = length of stock rail (inches) JOINT to rail joint.

    10 = timb_j1 dimension (negative spacing from toe to sleeper towards joint).
    11 = timb_j2 dimension (negative from ditto to next sleeper).
    12 = timb_j3 dimension (negative from ditto to next sleeper).
    13 = timb_j4 dimension (negative from ditto to next sleeper).
    14 = timb_j5 dimension (negative from ditto to next sleeper).

    15 = planing radius (double-curved switches).

    16 = joggle depth (to be used only if joggled).
    17 = joggle length (from toe, +ve) (to be used only if joggled).
    18 = if joggled? 0=no, 1=yes.

    19 = fbtip dimension (FB foot from gauge-face at tip).

list entries:

  BH or FB     9 ft.  straight  heel  switch
  BH or FB   12 ft.  straight  heel  switch
  BH or FB   15 ft.  straight  heel  switch
  BH or FB   18 ft.  straight  heel  switch
  BH or FB   24 ft.  straight  heel  switch
  BH or FB   30 ft.  straight  heel  switch

  BH  size  A  semi-curved  flexible  switch  ( REA )
  BH  size  B  semi-curved  flexible  switch  ( REA )
  BH  size  C  semi-curved  flexible  switch  ( REA )
  BH  size  D  semi-curved  flexible  switch  ( REA )
  BH  size  E  semi-curved  flexible  switch  ( REA )
  BH  size  F  semi-curved  flexible  switch  ( REA )

  BH  GWR  size  B  curved  flexible  switch
  BH  GWR  size  C  curved  flexible  switch
  BH  GWR  size  D  curved  flexible  switch
  BH  GWR  30 ft. straight  heel  switch

  BH  GWR  old-type    9 ft.  heel  switch  (curved)
  BH  GWR  old-type  10 ft.  heel  switch  (curved)
  BH  GWR  old-type  12 ft.  heel  switch  (curved)
  BH  GWR  old-type  14 ft.  heel  switch  (curved)
  BH  GWR  old-type  15 ft.  heel  switch  (curved)
  BH  GWR  old-type  16 ft.  heel  switch  (curved)
  BH  GWR  old-type  18 ft.  heel  switch  (straight)
  BH  GWR  old-type  20 ft.  heel  switch  (straight)

  FB  size  SB  semi-curved  flexible  switch  ( FB-109 )
  FB  size  SC  semi-curved  flexible  switch  ( FB-109 )
  FB  size  SD  semi-curved  flexible  switch  ( FB-109 )
  FB  size  SE  semi-curved  flexible  switch  ( FB-109 )
  FB  size  SF  semi-curved  flexible  switch  ( FB-109 )

  FB  size  CA  curved  flexible  switch  ( BS-110A,  BS-113A )
  FB  size  CB  curved  flexible  switch  ( BS-110A,  BS-113A )
  FB  size  CC  curved  flexible  switch  ( BS-110A,  BS-113A )
  FB  size  CD  curved  flexible  switch  ( BS-110A,  BS-113A )
  FB  size  CE  curved  flexible  switch  ( BS-110A,  BS-113A )
  FB  size  CF  curved  flexible  switch  ( BS-110A,  BS-113A )

  custom  switch  -   slot  1
  custom  switch  -   slot  2
  custom  switch  -   slot  3
  custom  switch  -   slot  4
  custom  switch  -   slot  5
  custom  switch  -   slot  6
  custom  switch  -   slot  7
  custom  switch  -   slot  8

  settings  as  most  recent  custom  switch
}

var
  sw_init_info:Tswitch_info;

  mf:extended;  // metric_factor 25.4

  vert_spacing,vert_joint,vert_front,vert_tips:extended;


                      /////////////////////////////////////////////////////////////

                      procedure clear_sw_init_info;   // clear all data.

                      var
                        n:integer;

                      begin
                        sw_init_info.valid_data:=False;      // NO valid data here !!

                        sw_init_info.sw_name_str:='';
                        sw_init_info.group_code:=0;
                        sw_init_info.size_code:=0;
                        sw_init_info.group_count:=0;

                        for n:=0 to swtimbco_c do sw_init_info.timber_centres[n]:=0;    // switch timbering spacings.

                        sw_init_info.old_size:=0;

                        sw_init_info.planing:=0;
                        sw_init_info.heel_lead_inches:=0;
                        sw_init_info.heel_offset_inches:=0;
                        sw_init_info.sw_pattern:=0;
                        sw_init_info.switch_front_inches:=0;
                        sw_init_info.planing_angle:=0;
                        sw_init_info.switch_radius_inchormax:=0;
                        sw_init_info.switch_rail:=0;
                        sw_init_info.stock_rail:=0;

                        sw_init_info.sleeper_j1:=0;
                        sw_init_info.sleeper_j2:=0;
                        sw_init_info.sleeper_j3:=0;
                        sw_init_info.sleeper_j4:=0;
                        sw_init_info.sleeper_j5:=0;
                        sw_init_info.front_timbered:=False;

                        sw_init_info.planing_radius:=0;
                        sw_init_info.joggle_depth:=0;
                        sw_init_info.joggle_length:=0;
                        sw_init_info.joggled_stock_rail:=False;
                        sw_init_info.fb_tip_offset:=0;

                        sw_init_info.num_slide_chairs:=0;                // byte 214a
                        sw_init_info.num_block_slide_chairs:=0;          // byte 214a
                        sw_init_info.num_block_heel_chairs:=0;           // byte 214a
                        sw_init_info.num_bridge_chairs_main_rail:=0;     // not used in experimental chairing  // byte 214a
                        sw_init_info.num_bridge_chairs_turnout_rail:=0;  // not used in experimental chairing  // byte 214a

                      end;
                      /////////////////////////////////////////////////////////////

                      function add_to_list(sw_group, sw_size, sw_size_max:integer; list_str,sw_str:string):integer;    // 215a for make_slip functions access to slip switches

                      var
                        i:integer;

                      begin
                        sw_init_info.sw_name_str:=sw_str;        // name of switch.
                        sw_init_info.group_code:=sw_group;       // which group of switches.   0.77.a  7-6-02.
                        sw_init_info.size_code:=sw_size;         // size within group (0=empty slot, 1=shortest). 0.77.a  7-6-02.
                        sw_init_info.group_count:=sw_size_max;   // number of switches in this group (max sw_init_info.size_code in this group, min size is always 1).

                        with switch_select_form.switch_selector_listbox.Items do begin
                          i:=AddObject(list_str,Tswitch.Create);
                          Tswitch(Objects[i]).list_switch_info:=sw_init_info;
                        end;//with

                        RESULT:=i;    // 215a added
                      end;//proc
                      ///////////////////////////////////////////////////////////////////

begin
  switch_select_form.switch_selector_listbox.Items.Clear;  // init.
  // -----------------------
  // 9ft Standard Straight Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=0;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=108;           // 9ft Standard Straight Switch
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;             // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=138;             // switch rail length.
  sw_init_info.stock_rail:=232;          // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;       // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

     // switch timbers centre to centre in inches. [first from toe].
     //  9ft Standard Straight Switch - first timber 4" from toe...

  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=30;
  sw_init_info.timber_centres[2]:=30;
  sw_init_info.timber_centres[3]:=30;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=28.5;    // switch rail joint.
  sw_init_info.timber_centres[6]:=27;      // stock rail joint.
  sw_init_info.timber_centres[7]:=0;       // zero list terminator.

           // group 1, size 1 of 6, ...

  add_to_list(1,1,6,'  BH  or  FB     9 ft.  straight  heel  switch',' 9 ft.  straight heel');

  // ----------------------------------
  // 12ft Standard Straight Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=1;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=144;            // 12'
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=174;             // switch rail length.
  sw_init_info.stock_rail:=360;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;       // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=4;       //  12ft Standard Straight Switch - first timber 4" from toe.
  sw_init_info.timber_centres[1]:=26;
  sw_init_info.timber_centres[2]:=26;
  sw_init_info.timber_centres[3]:=26;
  sw_init_info.timber_centres[4]:=26;
  sw_init_info.timber_centres[5]:=26.5;
  sw_init_info.timber_centres[6]:=26.5;
  sw_init_info.timber_centres[7]:=25.5;    // switch rail joint.
  sw_init_info.timber_centres[8]:=24;
  sw_init_info.timber_centres[9]:=24;
  sw_init_info.timber_centres[10]:=24;
  sw_init_info.timber_centres[11]:=24;
  sw_init_info.timber_centres[12]:=25;     // stock rail joint.
  sw_init_info.timber_centres[13]:=0;      // zero list terminator.

             // group 1, size 2 of 6, ...

  add_to_list(1,2,6,'  BH  or  FB   12 ft.  straight  heel  switch','12 ft.  straight heel');

  // ----------------------------------
  // 15ft Standard Straight Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=2;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=180;            // 15'
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=210;             // switch rail length.
  sw_init_info.stock_rail:=360;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).

                   //  15ft Standard Straight Switch - first timber 4" from toe.
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=27;
  sw_init_info.timber_centres[2]:=27.5;
  sw_init_info.timber_centres[3]:=27.5;
  sw_init_info.timber_centres[4]:=27.5;
  sw_init_info.timber_centres[5]:=27.5;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28;
  sw_init_info.timber_centres[8]:=25.5;   // switch rail joint
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=25;    // stock rail joint
  sw_init_info.timber_centres[12]:=0;     // zero list terminator.

            // group 1, size 3 of 6, ...

  add_to_list(1,3,6,'  BH  or  FB   15 ft.  straight  heel  switch','15 ft.  straight heel');

  // ----------------------------------
  // 18ft Standard Straight Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=3;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=216;            // 18'
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=360;             // switch rail length.
  sw_init_info.stock_rail:=540;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).

                   //  18ft Standard Straight Switch - first timber 4" from toe.
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=26;
  sw_init_info.timber_centres[2]:=26;
  sw_init_info.timber_centres[3]:=26;
  sw_init_info.timber_centres[4]:=26;
  sw_init_info.timber_centres[5]:=26;
  sw_init_info.timber_centres[6]:=26;
  sw_init_info.timber_centres[7]:=26;
  sw_init_info.timber_centres[8]:=26;
  sw_init_info.timber_centres[9]:=27;
  sw_init_info.timber_centres[10]:=27;
  sw_init_info.timber_centres[11]:=27;
  sw_init_info.timber_centres[12]:=27;
  sw_init_info.timber_centres[13]:=27;
  sw_init_info.timber_centres[14]:=25.5;  // switch rail joint
  sw_init_info.timber_centres[15]:=30;
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=25;    // stock rail joint
  sw_init_info.timber_centres[19]:=0;     // zero list terminator.

  // group 1, size 4 of 6, ...

  add_to_list(1,4,6,'  BH  or  FB   18 ft.  straight  heel  switch','18 ft.  straight heel');

  // ----------------------------------
  // 24ft Standard Straight Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=4;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=288;            // 24'
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=355;             // switch rail length.
  sw_init_info.stock_rail:=540;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).

                   //  24ft Standard Straight Switch - first timber 4" from toe.
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=25;
  sw_init_info.timber_centres[2]:=26;
  sw_init_info.timber_centres[3]:=26;
  sw_init_info.timber_centres[4]:=26;
  sw_init_info.timber_centres[5]:=26;
  sw_init_info.timber_centres[6]:=26;
  sw_init_info.timber_centres[7]:=26;
  sw_init_info.timber_centres[8]:=26;
  sw_init_info.timber_centres[9]:=26;
  sw_init_info.timber_centres[10]:=26;
  sw_init_info.timber_centres[11]:=26;
  sw_init_info.timber_centres[12]:=26;
  sw_init_info.timber_centres[13]:=26;
  sw_init_info.timber_centres[14]:=25.5;  // switch rail joint
  sw_init_info.timber_centres[15]:=24;
  sw_init_info.timber_centres[16]:=24;
  sw_init_info.timber_centres[17]:=24;
  sw_init_info.timber_centres[18]:=24;
  sw_init_info.timber_centres[19]:=25;    // stock rail joint
  sw_init_info.timber_centres[20]:=0;     // zero list terminator.

  // group 1, size 5 of 6, ...

  add_to_list(1,5,6,'  BH  or  FB   24 ft.  straight  heel  switch','24 ft.  straight heel');

  // ----------------------------------
  // 30ft Standard Straight Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=5;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=360;            // 30'
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=384;             // switch rail length.
  sw_init_info.stock_rail:=534;          // stock rail length.     ( 44'6"  error on Paddington R 3794a ?)
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).

                   //  30ft Standard Straight Switch - first timber 4" from toe.
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=26;
  sw_init_info.timber_centres[2]:=26;
  sw_init_info.timber_centres[3]:=26;
  sw_init_info.timber_centres[4]:=26;
  sw_init_info.timber_centres[5]:=26;
  sw_init_info.timber_centres[6]:=26;
  sw_init_info.timber_centres[7]:=26;
  sw_init_info.timber_centres[8]:=26;
  sw_init_info.timber_centres[9]:=26.5;
  sw_init_info.timber_centres[10]:=26.5;
  sw_init_info.timber_centres[11]:=26.5;
  sw_init_info.timber_centres[12]:=26.5;
  sw_init_info.timber_centres[13]:=26.5;
  sw_init_info.timber_centres[14]:=26.5;
  sw_init_info.timber_centres[15]:=25.5;  // switch rail joint
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=25;    // stock rail joint
  sw_init_info.timber_centres[19]:=0;     // zero list terminator.

  // group 1, size 6 of 6, ...

  add_to_list(1,6,6,'  BH  or  FB   30 ft.  straight  heel  switch','30 ft.  straight heel');

  // ----------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;     // clear all.

  add_to_list(0,0,0,'','');

  // ----------------------------------
  // BH  size  A+  semi-curved  flexible  switch  ( REA )

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=7;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=66;
  sw_init_info.heel_lead_inches:=227;            // A+
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=24;
  sw_init_info.switch_radius_inchormax:=5784;
  sw_init_info.switch_rail:=240;           // switch rail length.
  sw_init_info.stock_rail:=330;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.num_slide_chairs:=5;         // per side   byte 214a...
  sw_init_info.num_block_slide_chairs:=2;              // byte 214a
  sw_init_info.num_block_heel_chairs:=2;               // byte 214a
  sw_init_info.num_bridge_chairs_main_rail:=1;         // byte 214a
  sw_init_info.num_bridge_chairs_turnout_rail:=2;      // byte 214a

                            //  A  REA
  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=27.5;
  sw_init_info.timber_centres[2]:=28;
  sw_init_info.timber_centres[3]:=28;
  sw_init_info.timber_centres[4]:=28;
  sw_init_info.timber_centres[5]:=28;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28;
  sw_init_info.timber_centres[8]:=28;
  sw_init_info.timber_centres[9]:=26;     // switch rail joint
  sw_init_info.timber_centres[10]:=24;    // stock rail joint
  sw_init_info.timber_centres[11]:=0;     // zero list terminator.

  // group 2, size 1 of 6, ...

  add_to_list(2,1,6,'  BH  size  A  semi-curved  flexible  switch  ( REA )','REA semi-curved  A-size');

  // ----------------------------------
  // BH  size  B+  semi-curved  flexible  switch  ( REA )

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=8;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=88;
  sw_init_info.heel_lead_inches:=257;            // B+
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=32;
  sw_init_info.switch_radius_inchormax:=7356;
  sw_init_info.switch_rail:=270;           // switch rail length.
  sw_init_info.stock_rail:=360;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.num_slide_chairs:=6;         // per side   byte 214a...
  sw_init_info.num_block_slide_chairs:=2;              // byte 214a
  sw_init_info.num_block_heel_chairs:=2;               // byte 214a
  sw_init_info.num_bridge_chairs_main_rail:=2;         // byte 214a
  sw_init_info.num_bridge_chairs_turnout_rail:=3;      // byte 214a

                              //  B  REA
  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28;
  sw_init_info.timber_centres[2]:=28;
  sw_init_info.timber_centres[3]:=28;
  sw_init_info.timber_centres[4]:=28;
  sw_init_info.timber_centres[5]:=28;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28.5;
  sw_init_info.timber_centres[8]:=28.5;
  sw_init_info.timber_centres[9]:=28.5;
  sw_init_info.timber_centres[10]:=26;    // switch rail joint
  sw_init_info.timber_centres[11]:=24;    // stock rail joint
  sw_init_info.timber_centres[12]:=0;     // zero list terminator.

  // group 2, size 2 of 6, ...

  add_to_list(2,2,6,'  BH  size  B  semi-curved  flexible  switch  ( REA )','REA semi-curved  B-size');

  // ----------------------------------
  // BH  size  C+  semi-curved  flexible  switch  ( REA )

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=9;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=110;
  sw_init_info.heel_lead_inches:=329;            // C+
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=40;
  sw_init_info.switch_radius_inchormax:=11520;
  sw_init_info.switch_rail:=342;           // switch rail length.
  sw_init_info.stock_rail:=432;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.num_slide_chairs:=7;        // byte  per side 214a...
  sw_init_info.num_block_slide_chairs:=2;              // byte 214a
  sw_init_info.num_block_heel_chairs:=3;               // byte 214a
  sw_init_info.num_bridge_chairs_main_rail:=2;         // byte 214a
  sw_init_info.num_bridge_chairs_turnout_rail:=3;      // byte 214a

                              //  C  REA
  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=26;    // switch rail joint
  sw_init_info.timber_centres[13]:=24;    // stock rail joint
  sw_init_info.timber_centres[14]:=0;     // zero list terminator.

  // group 2, size 3 of 6, ...

  add_to_list(2,3,6,'  BH  size  C  semi-curved  flexible  switch  ( REA )','REA semi-curved  C-size');

  // ----------------------------------
  // BH  size  D+  semi-curved  flexible  switch  ( REA )

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=10;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=132;
  sw_init_info.heel_lead_inches:=379;           // D+
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=48;
  sw_init_info.switch_radius_inchormax:=16548;
  sw_init_info.switch_rail:=342;           // switch rail length.
  sw_init_info.stock_rail:=432;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.num_slide_chairs:=8;           // per side   214a...
  sw_init_info.num_block_slide_chairs:=2;              // byte 214a
  sw_init_info.num_block_heel_chairs:=4;               // byte 214a
  sw_init_info.num_bridge_chairs_main_rail:=3;         // byte 214a
  sw_init_info.num_bridge_chairs_turnout_rail:=4;      // byte 214a

                               //  D  REA
  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=26;    // switch rail joint
  sw_init_info.timber_centres[13]:=24;    // stock rail joint
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=0;     // zero list terminator.

  // group 2, size 4 of 6, ...

  add_to_list(2,4,6,'  BH  size  D  semi-curved  flexible  switch  ( REA )','REA semi-curved  D-size');

  // ----------------------------------
  // BH  size  E+  semi-curved  flexible  switch  ( REA )

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=11;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=176;
  sw_init_info.heel_lead_inches:=529;           // E+
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=64;
  sw_init_info.switch_radius_inchormax:=29352;
  sw_init_info.switch_rail:=342;           // switch rail length.
  sw_init_info.stock_rail:=432;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.num_slide_chairs:=10;        // byte  per side 214a...
  sw_init_info.num_block_slide_chairs:=0;              // byte 214a
  sw_init_info.num_block_heel_chairs:=9;               // byte 214a
  sw_init_info.num_bridge_chairs_main_rail:=3;         // byte 214a
  sw_init_info.num_bridge_chairs_turnout_rail:=5;      // byte 214a


                               //  E  REA
  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=26;    // switch rail joint
  sw_init_info.timber_centres[13]:=24;    // stock rail joint
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=30;
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=30;
  sw_init_info.timber_centres[19]:=30;
  sw_init_info.timber_centres[20]:=0;     // zero list terminator.

  // group 2, size 5 of 6, ...

  add_to_list(2,5,6,'  BH  size  E  semi-curved  flexible  switch  ( REA )','REA semi-curved  E-size');

  // ----------------------------------
  // BH  size  F*  semi-curved  flexible  switch  ( REA )

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=12;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=220;
  sw_init_info.heel_lead_inches:=649;           // F*
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=80;
  sw_init_info.switch_radius_inchormax:=45900;
  sw_init_info.switch_rail:=402;           // switch rail length.
  sw_init_info.stock_rail:=492;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.num_slide_chairs:=14;        // byte  per side 214a...
  sw_init_info.num_block_slide_chairs:=0;              // byte 214a
  sw_init_info.num_block_heel_chairs:=11;              // byte 214a
  sw_init_info.num_bridge_chairs_main_rail:=3;         // byte 214a
  sw_init_info.num_bridge_chairs_turnout_rail:=5;      // byte 214a


                               //  F*  REA
  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=30;
  sw_init_info.timber_centres[14]:=26;    // switch rail joint
  sw_init_info.timber_centres[15]:=24;    // stock rail joint
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=30;
  sw_init_info.timber_centres[19]:=30;
  sw_init_info.timber_centres[20]:=30;
  sw_init_info.timber_centres[21]:=30;
  sw_init_info.timber_centres[22]:=30;
  sw_init_info.timber_centres[23]:=30;
  sw_init_info.timber_centres[24]:=30;
  sw_init_info.timber_centres[25]:=30;
  sw_init_info.timber_centres[26]:=0;     // zero list terminator.

  // group 2, size 6 of 6, ...

  add_to_list(2,6,6,'  BH  size  F  semi-curved  flexible  switch  ( REA )','REA semi-curved  F-size');

  // ----------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------
  //  GWR curved  B-size switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=21;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=270;            // B CURVED GWR
  sw_init_info.heel_offset_inches:=10.03356;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=8760;
  sw_init_info.switch_rail:=270;           // switch rail length.
  sw_init_info.stock_rail:=360;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27.5;       // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.250;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                            // GWR curved B
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=28;
  sw_init_info.timber_centres[2]:=28;
  sw_init_info.timber_centres[3]:=28;
  sw_init_info.timber_centres[4]:=28;
  sw_init_info.timber_centres[5]:=28;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28;
  sw_init_info.timber_centres[8]:=28.5;
  sw_init_info.timber_centres[9]:=28.5;
  sw_init_info.timber_centres[10]:=26;    // switch rail joint
  sw_init_info.timber_centres[11]:=26;    // stock rail joint
  sw_init_info.timber_centres[12]:=0;     // zero list terminator.

  // group 3, size 1 of 4, ...

  add_to_list(3,1,4,'  BH  GWR  size  B  curved  flexible  switch','GWR  curved  B-size');

  // ----------------------------------
  //  GWR curved  C-size switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=22;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=342;            // C CURVED GWR
  sw_init_info.heel_offset_inches:=10.22868;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=13560;
  sw_init_info.switch_rail:=342;           // switch rail length.
  sw_init_info.stock_rail:=432;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27.5;       // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.250;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                               // GWR curved C
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=28;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=26;    // switch rail joint
  sw_init_info.timber_centres[13]:=26;    // stock rail joint
  sw_init_info.timber_centres[14]:=0;     // zero list terminator.

  // group 3, size 2 of 4, ...

  add_to_list(3,2,4,'  BH  GWR  size  C  curved  flexible  switch','GWR  curved  C-size');

  // ----------------------------------
  //  GWR curved  D-size switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=23;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=416;           // D CURVED GWR (heel at end of stock rail)
  sw_init_info.heel_offset_inches:=10;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=24000;
  sw_init_info.switch_rail:=360;           // switch rail length.
  sw_init_info.stock_rail:=480;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe).
  sw_init_info.sleeper_j2:=0-27.5;       // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.250;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                               // GWR curved D
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=28;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=28.5;
  sw_init_info.timber_centres[5]:=28.5;
  sw_init_info.timber_centres[6]:=28.5;
  sw_init_info.timber_centres[7]:=28.5;
  sw_init_info.timber_centres[8]:=28.5;
  sw_init_info.timber_centres[9]:=28.5;
  sw_init_info.timber_centres[10]:=28.5;
  sw_init_info.timber_centres[11]:=28.5;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=26;    // switch rail joint
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=26;    // stock rail joint
  sw_init_info.timber_centres[16]:=0;     // zero list terminator.

  // group 3, size 3 of 4, ...

  add_to_list(3,3,4,'  BH  GWR  size  D  curved  flexible  switch','GWR  curved  D-size');

  // ----------------------------------
  //  GWR 30 ft. Straight Switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=24;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=360;           // GWR 30 ft. Straight Switch
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=384;             // switch rail length.
  sw_init_info.stock_rail:=532;           // stock rail length.       (44'4"   error on Paddington R 3794a ?)
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                             //  GWR  30ft Straight
  sw_init_info.timber_centres[0]:=4;              //  first timber 4" from toe.
  sw_init_info.timber_centres[1]:=26.5;
  sw_init_info.timber_centres[2]:=26.5;
  sw_init_info.timber_centres[3]:=26.5;
  sw_init_info.timber_centres[4]:=26.5;
  sw_init_info.timber_centres[5]:=26;
  sw_init_info.timber_centres[6]:=26;
  sw_init_info.timber_centres[7]:=26;
  sw_init_info.timber_centres[8]:=26;
  sw_init_info.timber_centres[9]:=26;
  sw_init_info.timber_centres[10]:=26;
  sw_init_info.timber_centres[11]:=26;
  sw_init_info.timber_centres[12]:=26;
  sw_init_info.timber_centres[13]:=26;
  sw_init_info.timber_centres[14]:=26;
  sw_init_info.timber_centres[15]:=26.5;    // switch rail joint
  sw_init_info.timber_centres[16]:=29.5;
  sw_init_info.timber_centres[17]:=29;
  sw_init_info.timber_centres[18]:=26;      // stock rail joint
  sw_init_info.timber_centres[19]:=0;       // zero list terminator.

  // group 3, size 4 of 4, ...

  add_to_list(3,4,4,'  BH  GWR  30 ft. straight  heel  switch','GWR   30 ft.  straight');

  //--------------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------
  //  GWR  9ft heel switch (curved).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=13;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                           // GWR  9ft heel switch (curved).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=108;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=3600;          // 300ft switch radius.
  sw_init_info.switch_rail:=132;           // switch rail length.
  sw_init_info.stock_rail:=280;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                            //  GWR 9ft Heel Switch (curved).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=29;
  sw_init_info.timber_centres[2]:=29;
  sw_init_info.timber_centres[3]:=29;
  sw_init_info.timber_centres[4]:=28.5;

  sw_init_info.timber_centres[5]:=25;     // switch rail joint.
  sw_init_info.timber_centres[6]:=29.5;
  sw_init_info.timber_centres[7]:=29.5;
  sw_init_info.timber_centres[8]:=25;     // stock rail joint.
  sw_init_info.timber_centres[9]:=0;      // zero list terminator.

  // group 4, size 1 of 8, ...

  add_to_list(4,1,8,'  BH  GWR  old-type    9 ft.  heel  switch  (curved)','GWR   9 ft. heel switch (curved)');

  //------------------------------------------------------
  //  GWR  10ft heel switch (curved).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=14;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                              // GWR  10ft heel switch (curved).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=120;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;             // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=5760;          // 480ft switch radius.
  sw_init_info.switch_rail:=144;           // switch rail length.
  sw_init_info.stock_rail:=292;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                            //  GWR 10ft Heel Switch (curved).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=25.5;
  sw_init_info.timber_centres[2]:=25.5;
  sw_init_info.timber_centres[3]:=25.5;
  sw_init_info.timber_centres[4]:=25.5;
  sw_init_info.timber_centres[5]:=25.5;

  sw_init_info.timber_centres[6]:=25;     // switch rail joint.
  sw_init_info.timber_centres[7]:=29.5;
  sw_init_info.timber_centres[8]:=29.5;
  sw_init_info.timber_centres[9]:=25;     // stock rail joint.
  sw_init_info.timber_centres[10]:=0;     // zero list terminator.

  // group 4, size 2 of 8, ...

  add_to_list(4,2,8,'  BH  GWR  old-type  10 ft.  heel  switch  (curved)','GWR  10 ft. heel switch (curved)');

  //------------------------------------------------------
  //  GWR  12ft heel switch (curved).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=15;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                       // GWR  12ft heel switch (curved).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=144;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=8400;          // 700ft switch radius.
  sw_init_info.switch_rail:=168;           // switch rail length.
  sw_init_info.stock_rail:=316;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                        //  GWR 12ft Heel Switch (curved).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=25;
  sw_init_info.timber_centres[2]:=25;
  sw_init_info.timber_centres[3]:=25;
  sw_init_info.timber_centres[4]:=25;
  sw_init_info.timber_centres[5]:=25;
  sw_init_info.timber_centres[6]:=26.5;

  sw_init_info.timber_centres[7]:=25;     // switch rail joint.
  sw_init_info.timber_centres[8]:=29.5;
  sw_init_info.timber_centres[9]:=29.5;
  sw_init_info.timber_centres[10]:=25;    // stock rail joint.
  sw_init_info.timber_centres[11]:=0;     // zero list terminator.

  // group 4, size 3 of 8, ...

  add_to_list(4,3,8,'  BH  GWR  old-type  12 ft.  heel  switch  (curved)','GWR  12 ft. heel switch (curved)');

  //------------------------------------------------------
  //  GWR  14ft heel switch (curved).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=16;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                               // GWR  14ft heel switch (curved).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=168;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=11400;         // 950ft switch radius.
  sw_init_info.switch_rail:=192;           // switch rail length.
  sw_init_info.stock_rail:=340;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                       //  GWR 14ft Heel Switch (curved).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=25;
  sw_init_info.timber_centres[2]:=25;
  sw_init_info.timber_centres[3]:=25;
  sw_init_info.timber_centres[4]:=25;
  sw_init_info.timber_centres[5]:=25;
  sw_init_info.timber_centres[6]:=25;
  sw_init_info.timber_centres[7]:=25.5;

  sw_init_info.timber_centres[8]:=25;     // switch rail joint.
  sw_init_info.timber_centres[9]:=29.5;
  sw_init_info.timber_centres[10]:=29.5;
  sw_init_info.timber_centres[11]:=25;    // stock rail joint.
  sw_init_info.timber_centres[12]:=0;     // zero list terminator.

  // group 4, size 4 of 8, ...

  add_to_list(4,4,8,'  BH  GWR  old-type  14 ft.  heel  switch  (curved)','GWR  14 ft. heel switch (curved)');

  //------------------------------------------------------
  //  GWR  15ft heel switch (curved).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=17;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                     // GWR  15ft heel switch (curved).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=180;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=14400;         // 1200ft switch radius.
  sw_init_info.switch_rail:=204;           // switch rail length.
  sw_init_info.stock_rail:=352;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                        //  GWR 15ft Heel Switch (curved).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=27;
  sw_init_info.timber_centres[2]:=27;
  sw_init_info.timber_centres[3]:=27;
  sw_init_info.timber_centres[4]:=27;
  sw_init_info.timber_centres[5]:=27;
  sw_init_info.timber_centres[6]:=26.5;
  sw_init_info.timber_centres[7]:=26;

  sw_init_info.timber_centres[8]:=25;     // switch rail joint.
  sw_init_info.timber_centres[9]:=29.5;
  sw_init_info.timber_centres[10]:=29.5;
  sw_init_info.timber_centres[11]:=25;    // stock rail joint.
  sw_init_info.timber_centres[12]:=0;     // zero list terminator.

  // group 4, size 5 of 8, ...

  add_to_list(4,5,8,'  BH  GWR  old-type  15 ft.  heel  switch  (curved)','GWR  15 ft. heel switch (curved)');

  //------------------------------------------------------
  //  GWR  16ft heel switch (curved).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=18;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                      // GWR  16ft heel switch (curved).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=192;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=17280;         // 1440ft switch radius.
  sw_init_info.switch_rail:=216;           // switch rail length.
  sw_init_info.stock_rail:=364;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                        //  GWR 16ft Heel Switch (curved).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=25;
  sw_init_info.timber_centres[2]:=25;
  sw_init_info.timber_centres[3]:=25;
  sw_init_info.timber_centres[4]:=25;
  sw_init_info.timber_centres[5]:=25;
  sw_init_info.timber_centres[6]:=25;
  sw_init_info.timber_centres[7]:=25;
  sw_init_info.timber_centres[8]:=24.5;

  sw_init_info.timber_centres[9]:=25;     // switch rail joint.
  sw_init_info.timber_centres[10]:=29.5;
  sw_init_info.timber_centres[11]:=29.5;
  sw_init_info.timber_centres[12]:=25;    // stock rail joint.
  sw_init_info.timber_centres[13]:=0;     // zero list terminator.

  // group 4, size 6 of 8, ...

  add_to_list(4,6,8,'  BH  GWR  old-type  16 ft.  heel  switch  (curved)','GWR  16 ft. heel switch (curved)');

  //------------------------------------------------------
  //  GWR  18ft heel switch (straight).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=19;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                     // GWR  18ft heel switch (straight).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=216;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch.
  sw_init_info.switch_rail:=240;             // switch rail length.
  sw_init_info.stock_rail:=388;          // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;            // fbtip dimension (FB foot from gauge-face at tip).

                       //  GWR 18ft Heel Switch (straight).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=25;
  sw_init_info.timber_centres[2]:=25;
  sw_init_info.timber_centres[3]:=25;
  sw_init_info.timber_centres[4]:=25;
  sw_init_info.timber_centres[5]:=24.5;
  sw_init_info.timber_centres[6]:=24.5;
  sw_init_info.timber_centres[7]:=24.5;
  sw_init_info.timber_centres[8]:=24.5;
  sw_init_info.timber_centres[9]:=24;

  sw_init_info.timber_centres[10]:=26.5;  // switch rail joint.
  sw_init_info.timber_centres[11]:=29.5;
  sw_init_info.timber_centres[12]:=29.5;
  sw_init_info.timber_centres[13]:=25;    // stock rail joint.
  sw_init_info.timber_centres[14]:=0;     // zero list terminator.

  // group 4, size 7 of 8, ...

  add_to_list(4,7,8,'  BH  GWR  old-type  18 ft.  heel  switch  (straight)','GWR  18 ft. heel switch (straight)');

  //------------------------------------------------------
  //  GWR  20ft heel switch (straight).

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=20;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).

                        // GWR  20ft heel switch (straight).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=240;
  sw_init_info.heel_offset_inches:=4.5;
  sw_init_info.sw_pattern:=0;             // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=64;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;  // straight switch.
  sw_init_info.switch_rail:=264;           // switch rail length.
  sw_init_info.stock_rail:=412;          // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-27;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=4;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=True;        // joggled stock rails?
  sw_init_info.fb_tip_offset:=0;           // fbtip dimension (FB foot from gauge-face at tip).

                         //  GWR 20ft Heel Switch (straight).
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=25;
  sw_init_info.timber_centres[2]:=25;
  sw_init_info.timber_centres[3]:=25;
  sw_init_info.timber_centres[4]:=25;
  sw_init_info.timber_centres[5]:=24.5;
  sw_init_info.timber_centres[6]:=24.5;
  sw_init_info.timber_centres[7]:=24.5;
  sw_init_info.timber_centres[8]:=24.5;
  sw_init_info.timber_centres[9]:=24;
  sw_init_info.timber_centres[10]:=24;

  sw_init_info.timber_centres[11]:=26.5;  // switch rail joint.
  sw_init_info.timber_centres[12]:=29.5;
  sw_init_info.timber_centres[13]:=29.5;
  sw_init_info.timber_centres[14]:=25;    // stock rail joint.
  sw_init_info.timber_centres[15]:=0;     // zero list terminator.

  // group 4, size 8 of 8, ...

  add_to_list(4,8,8,'  BH  GWR  old-type  20 ft.  heel  switch  (straight)','GWR  20 ft. heel switch (straight)');

  //-----------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------

    // FB  size  SA  semi-curved  flexible  switch.

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=66;
  sw_init_info.heel_lead_inches:=258.75;   // SA  21'-6.3/4"
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;          // valid data here
  sw_init_info.planing_angle:=24;
  sw_init_info.switch_radius_inchormax:=5725;    // 477'-1"
  sw_init_info.switch_rail:=270;         // switch rail length.
  sw_init_info.stock_rail:=360;          // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;             // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;           // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;              // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;     // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28;
  sw_init_info.timber_centres[2]:=28;
  sw_init_info.timber_centres[3]:=28;
  sw_init_info.timber_centres[4]:=28;
  sw_init_info.timber_centres[5]:=28;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28;
  sw_init_info.timber_centres[8]:=28;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=25;    // switch rail joint.
  sw_init_info.timber_centres[11]:=25;    // stock rail joint.
  sw_init_info.timber_centres[12]:=0;     // zero list terminator.

  // group 5, size 1 of 6, ...

  add_to_list(5,1,6,'  FB  size  SA  semi-curved  flexible  switch  ( BS-110A sidings)','FB semi-curved SA');

  // ----------------------------------
  // FB SB semi-curved  flexible  switch

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=88;
  sw_init_info.heel_lead_inches:=324;     // SB 27'-0"
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;          // valid data here
  sw_init_info.planing_angle:=32;
  sw_init_info.switch_radius_inchormax:=7369;    // 614'-1"
  sw_init_info.switch_rail:=342;                 // 28'-6" switch rail length.
  sw_init_info.stock_rail:=432;                  // 36'-0" stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;  // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;        // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=29;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=25;
  sw_init_info.timber_centres[13]:=25;
  sw_init_info.timber_centres[14]:=0;     // zero list terminator.

      // group 5, size 2 of 6, ...

  add_to_list(5,2,6,'  FB  size  SB  semi-curved  flexible  switch  ( FB-109 )','FB semi-curved SB');

  // ----------------------------------
  // FB SC semi-curved  flexible  switch

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=110;
  sw_init_info.heel_lead_inches:=359.5;   // SC  29'-11.5"
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;          // valid data here
  sw_init_info.planing_angle:=40;
  sw_init_info.switch_radius_inchormax:=11497;  // 958'-1"
  sw_init_info.switch_rail:=342;                // 28'-6" switch rail length.
  sw_init_info.stock_rail:=432;                 // 36'-0" stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=29;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=25;
  sw_init_info.timber_centres[13]:=25;
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=0;

     // group 5, size 3 of 6, ...

  add_to_list(5,3,6,'  FB  size  SC  semi-curved  flexible  switch  ( FB-109 )','FB semi-curved SC');

  // ----------------------------------
  // FB SD semi-curved  flexible  switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=132;
  sw_init_info.heel_lead_inches:=414.5;     // SD 34'-6.5"
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;            // valid data here
  sw_init_info.planing_angle:=48;
  sw_init_info.switch_radius_inchormax:=16542; // 1378'-6"
  sw_init_info.switch_rail:=342;               // 28'-6" switch rail length.
  sw_init_info.stock_rail:=432;                // 36'-0" stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=29;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=25;    // switch rail joint
  sw_init_info.timber_centres[13]:=25;    // stock rail joint
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=30;
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=0;

       // group 5, size 4 of 6, ...

  add_to_list(5,4,6,'  FB  size  SD  semi-curved  flexible  switch  ( FB-109 )','FB semi-curved SD');

  // ----------------------------------
  // FB SE semi-curved  flexible  switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;       // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=176;
  sw_init_info.heel_lead_inches:=559.5;   // SE 46'-7.5"
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;          // valid data here
  sw_init_info.planing_angle:=64;
  sw_init_info.switch_radius_inchormax:=29386;  // 2448'-10"
  sw_init_info.switch_rail:=392;           // 32'-8" switch rail length.
  sw_init_info.stock_rail:=432;            // 36'-0" stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;         // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;           // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;              // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;              // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;              // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=28.5;
  sw_init_info.timber_centres[5]:=29;
  sw_init_info.timber_centres[6]:=29;
  sw_init_info.timber_centres[7]:=29;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=25;
  sw_init_info.timber_centres[14]:=25;
  sw_init_info.timber_centres[15]:=30;
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=30;
  sw_init_info.timber_centres[19]:=30;
  sw_init_info.timber_centres[20]:=30;
  sw_init_info.timber_centres[21]:=30;
  sw_init_info.timber_centres[22]:=30;
  sw_init_info.timber_centres[23]:=0;

       // group 5, size 5 of 6, ...

  add_to_list(5,5,6,'  FB  size  SE  semi-curved  flexible  switch  ( FB-109 )','FB semi-curved SE');

  // ----------------------------------
  // FB SF  semi-curved  flexible  switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=220;
  sw_init_info.heel_lead_inches:=709.5;   // SF 59'-1.5"
  sw_init_info.heel_offset_inches:=0;
  sw_init_info.sw_pattern:=-1;            // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=80;
  sw_init_info.switch_radius_inchormax:=45898;  // 3824'-10"
  sw_init_info.switch_rail:=452;             // 37'-8" switch rail length.
  sw_init_info.stock_rail:=492;              // 41'-0" stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;  // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;        // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=28.5;
  sw_init_info.timber_centres[4]:=28.5;
  sw_init_info.timber_centres[5]:=29;
  sw_init_info.timber_centres[6]:=29;
  sw_init_info.timber_centres[7]:=29;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=30;
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=25;
  sw_init_info.timber_centres[16]:=25;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=30;
  sw_init_info.timber_centres[19]:=30;
  sw_init_info.timber_centres[20]:=30;
  sw_init_info.timber_centres[21]:=30;
  sw_init_info.timber_centres[22]:=30;
  sw_init_info.timber_centres[23]:=30;
  sw_init_info.timber_centres[24]:=30;
  sw_init_info.timber_centres[25]:=30;
  sw_init_info.timber_centres[26]:=30;
  sw_init_info.timber_centres[27]:=30;
  sw_init_info.timber_centres[28]:=30;
  sw_init_info.timber_centres[29]:=30;
  sw_init_info.timber_centres[30]:=0;

       // group 5, size 6 of 6, ...

  add_to_list(5,6,6,'  FB  size  SF  semi-curved  flexible  switch  ( FB-109 )','FB semi-curved SF');

  //--------------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------

    //  FB CA curved switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=295;            // 24'-7" CA
  sw_init_info.heel_offset_inches:=14.640625;    // 14.41/64"       // was 14.765625;    // 14.49/64"
  sw_init_info.sw_pattern:=0;                    // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=5725;  // 477'-1"
  sw_init_info.switch_rail:=342;               // 28'-6" switch rail length.
  sw_init_info.stock_rail:=432;                // 36'-0"stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28.5;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=29;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=25;
  sw_init_info.timber_centres[13]:=25;
  sw_init_info.timber_centres[14]:=0;     // zero list terminator.

       // group 6, size 1 of 6, ...

  add_to_list(6,1,6,'  FB  size  CA  curved  flexible  switch  ( BS-110A,  BS-113A )','FB BS-110A curved CA');

  //-----------------------------------

    //  FB CB curved switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=354.5;          // 29'-6.5" CB
  sw_init_info.heel_offset_inches:=13.921875;    // 13.59/64"
  sw_init_info.sw_pattern:=0;                    // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=7369;  // 614'-1"
  sw_init_info.switch_rail:=372.5;             // 31'-0.1/2" switch rail length.
  sw_init_info.stock_rail:=462.5;              // 38'-6.1/2"stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=28;
  sw_init_info.timber_centres[2]:=28.5;
  sw_init_info.timber_centres[3]:=30;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=30;
  sw_init_info.timber_centres[6]:=30;
  sw_init_info.timber_centres[7]:=30;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=25;
  sw_init_info.timber_centres[14]:=25;
  sw_init_info.timber_centres[15]:=0;

         // group 6, size 2 of 6, ...

  add_to_list(6,2,6,'  FB  size  CB  curved  flexible  switch  ( BS-110A,  BS-113A )','FB BS-110A curved CB');

  //-------------------------

    //  FB CC curved switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=397.75;         // 33'-1.75" CC
  sw_init_info.heel_offset_inches:=11.703125;    // 11.45/64"
  sw_init_info.sw_pattern:=0;                    // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=11497; // 958'-1"
  sw_init_info.switch_rail:=380.25;            // 31'-8.1/4" switch rail length.
  sw_init_info.stock_rail:=470.25;             // 39'-2.1/4"stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=26.25;
  sw_init_info.timber_centres[2]:=26.25;
  sw_init_info.timber_centres[3]:=26.25;
  sw_init_info.timber_centres[4]:=26.25;
  sw_init_info.timber_centres[5]:=26.25;
  sw_init_info.timber_centres[6]:=26.5;
  sw_init_info.timber_centres[7]:=26.5;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=30;
  sw_init_info.timber_centres[14]:=25;
  sw_init_info.timber_centres[15]:=25;
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=0;

         // group 6, size 3 of 6, ...

  add_to_list(6,3,6,'  FB  size  CC  curved  flexible  switch  ( BS-110A,  BS-113A )','FB BS-110A curved CC');

  //--------------------------------------

    //  FB CD curved switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=460.5;          // 38'-4.5" CD
  sw_init_info.heel_offset_inches:=11.05;        // 11.3/64" mod to match SD
  sw_init_info.sw_pattern:=0;                    // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=16542; // 1378'-6"
  sw_init_info.switch_rail:=388;               // 32'-4" switch rail length.
  sw_init_info.stock_rail:=478;                // 39'-10"stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=27;
  sw_init_info.timber_centres[2]:=27;
  sw_init_info.timber_centres[3]:=27;
  sw_init_info.timber_centres[4]:=27;
  sw_init_info.timber_centres[5]:=28;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28;
  sw_init_info.timber_centres[8]:=30;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=30;
  sw_init_info.timber_centres[14]:=25;
  sw_init_info.timber_centres[15]:=25;
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=0;

         // group 6, size 4 of 6, ...

  add_to_list(6,4,6,'  FB  size  CD  curved  flexible  switch  ( BS-110A,  BS-113A )','FB BS-110A curved CD');

  //----------------------------

    //  FB CE curved switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=620.875;        // 51'-8.7/8" CE
  sw_init_info.heel_offset_inches:=11.246;       // 11.1/4" mod to match SE
  sw_init_info.sw_pattern:=0;                    // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=29386; // 2448'-10"
  sw_init_info.switch_rail:=453.375;           // 37'-9.3/8" switch rail length.
  sw_init_info.stock_rail:=493.375;            // 41'-1.3/8"stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=29;
  sw_init_info.timber_centres[2]:=29;
  sw_init_info.timber_centres[3]:=29;
  sw_init_info.timber_centres[4]:=29;
  sw_init_info.timber_centres[5]:=29;
  sw_init_info.timber_centres[6]:=29;
  sw_init_info.timber_centres[7]:=29;
  sw_init_info.timber_centres[8]:=29.375;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=30;
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=25;
  sw_init_info.timber_centres[16]:=25;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=30;
  sw_init_info.timber_centres[19]:=30;
  sw_init_info.timber_centres[20]:=30;
  sw_init_info.timber_centres[21]:=30;
  sw_init_info.timber_centres[22]:=30;
  sw_init_info.timber_centres[23]:=30;
  sw_init_info.timber_centres[24]:=30;
  sw_init_info.timber_centres[25]:=0;

         // group 6, size 5 of 6, ...

  add_to_list(6,5,6,'  FB  size  CE  curved  flexible  switch  ( BS-110A,  BS-113A )','FB BS-110A curved CE');

  //---------------------------------------

    //  FB CF curved switch

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=787;          // 65'-4.1/4" CF mod to match SF
  sw_init_info.heel_offset_inches:=11.484375;  // 11.31/64"
  sw_init_info.sw_pattern:=0;                  // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;                // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=45898;  // 3824'-10"
  sw_init_info.switch_rail:=526.75;             // 43'-10.3/4" switch rail length.
  sw_init_info.stock_rail:=566.75;              // 47'-2.3/4"stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=26.5;
  sw_init_info.timber_centres[2]:=26.5;
  sw_init_info.timber_centres[3]:=26.5;
  sw_init_info.timber_centres[4]:=26.5;
  sw_init_info.timber_centres[5]:=26.5;
  sw_init_info.timber_centres[6]:=26.5;
  sw_init_info.timber_centres[7]:=27.75;
  sw_init_info.timber_centres[8]:=29;
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=30;
  sw_init_info.timber_centres[12]:=30;
  sw_init_info.timber_centres[13]:=30;
  sw_init_info.timber_centres[14]:=30;
  sw_init_info.timber_centres[15]:=30;
  sw_init_info.timber_centres[16]:=30;
  sw_init_info.timber_centres[17]:=30;
  sw_init_info.timber_centres[18]:=25;
  sw_init_info.timber_centres[19]:=25;
  sw_init_info.timber_centres[20]:=30;
  sw_init_info.timber_centres[21]:=30;
  sw_init_info.timber_centres[22]:=30;
  sw_init_info.timber_centres[23]:=30;
  sw_init_info.timber_centres[24]:=30;
  sw_init_info.timber_centres[25]:=30;
  sw_init_info.timber_centres[26]:=30;
  sw_init_info.timber_centres[27]:=30;
  sw_init_info.timber_centres[28]:=30;
  sw_init_info.timber_centres[29]:=30;
  sw_init_info.timber_centres[30]:=30;
  sw_init_info.timber_centres[31]:=30;
  sw_init_info.timber_centres[32]:=30;
  sw_init_info.timber_centres[33]:=0;

         // group 6, size 6 of 6, ...

  add_to_list(6,6,6,'  FB  size  CF  curved  flexible  switch  ( BS-110A,  BS-113A )','FB BS-110A curved CF');


  (*

  //--------------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------


  mf:=25.4;  // metric factor (prototype info in mm)

  vert_spacing:=710/mf;
  vert_joint:=660/mf;
  vert_front:=3070/mf;
  vert_tips:=90/mf;



    //  AVm vertical 1432mm switch    m = model approximated as single-curved switch     213b

  clear_sw_init_info;             // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).



  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=7317/mf;      // AV L1
  sw_init_info.heel_offset_inches:=278.54/mf;
  sw_init_info.sw_pattern:=0;                  // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=3070/mf;
  sw_init_info.valid_data:=True;                // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=5808;   // inches calculated to result in correct heel angle for AV
  sw_init_info.switch_rail:=8890/mf;            // switch rail length.
  sw_init_info.stock_rail:=11300/mf;            // stock rail length.
  sw_init_info.sleeper_j1:=0-620/mf;            // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-vert_spacing;      // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0-vert_spacing;      // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0-vert_spacing;      // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;                   // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;           // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;         // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;   // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;         // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=vert_tips;
  sw_init_info.timber_centres[1]:=vert_spacing;
  sw_init_info.timber_centres[2]:=vert_spacing;
  sw_init_info.timber_centres[3]:=vert_spacing;
  sw_init_info.timber_centres[4]:=vert_spacing;
  sw_init_info.timber_centres[5]:=vert_spacing;
  sw_init_info.timber_centres[6]:=vert_spacing;
  sw_init_info.timber_centres[7]:=vert_spacing;
  sw_init_info.timber_centres[8]:=vert_spacing;
  sw_init_info.timber_centres[9]:=vert_spacing;
  sw_init_info.timber_centres[10]:=vert_spacing;
  sw_init_info.timber_centres[11]:=vert_spacing;
  sw_init_info.timber_centres[12]:=vert_joint;
  sw_init_info.timber_centres[13]:=vert_joint;
  sw_init_info.timber_centres[14]:=0;

         // group 10, size 1 of 7, ...

  add_to_list(10,1,7,'  FB  size  AVm  vertical  switch  1432mm  gauge  ( BS-113A )','FB BS-113A vertical AVm');

  *)








  //--------------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------

    // 1:24 Model Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=66;      // 1:24 Model Switch, heel at planing.
  sw_init_info.heel_offset_inches:=2.75;
  sw_init_info.sw_pattern:=0;             // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=138;                    // switch rail length.
  sw_init_info.stock_rail:=232;          // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;       // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

     // switch timbers centre to centre in inches. [first from toe].
     // same as 9ft Standard Straight Switch - first timber 4" from toe...

  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=30;
  sw_init_info.timber_centres[2]:=30;
  sw_init_info.timber_centres[3]:=30;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=28.5;    // switch rail joint.
  sw_init_info.timber_centres[6]:=27;      // stock rail joint.
  sw_init_info.timber_centres[7]:=0;       // zero list terminator.

           // group 7, size 1 of 3, ...

  add_to_list(7,1,3,'  BH  or  FB    1:24  ( A )  non-prototype  short  model  switch',' 1:24  model-only');

  // ----------------------------------
  // 1:32 Model Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=88;       // 1:32
  sw_init_info.heel_offset_inches:=2.75;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=174;             // switch rail length.
  sw_init_info.stock_rail:=360;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;       // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=4;       //  same as 12ft Standard Straight Switch - first timber 4" from toe.
  sw_init_info.timber_centres[1]:=26;
  sw_init_info.timber_centres[2]:=26;
  sw_init_info.timber_centres[3]:=26;
  sw_init_info.timber_centres[4]:=26;
  sw_init_info.timber_centres[5]:=26.5;
  sw_init_info.timber_centres[6]:=26.5;
  sw_init_info.timber_centres[7]:=25.5;    // switch rail joint.
  sw_init_info.timber_centres[8]:=24;
  sw_init_info.timber_centres[9]:=24;
  sw_init_info.timber_centres[10]:=24;
  sw_init_info.timber_centres[11]:=24;
  sw_init_info.timber_centres[12]:=25;     // stock rail joint.
  sw_init_info.timber_centres[13]:=0;      // zero list terminator.

             // group 7, size 2 of 3, ...

  add_to_list(7,2,3,'  BH  or  FB    1:32  ( B )  non-prototype  short  model  switch',' 1:32  model-only');

  // ----------------------------------
  // 1:40 Model Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=110;      // 1:40
  sw_init_info.heel_offset_inches:=2.75;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=210;             // switch rail length.
  sw_init_info.stock_rail:=360;           // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).

                   //  same as 15ft Standard Straight Switch - first timber 4" from toe.
  sw_init_info.timber_centres[0]:=4;
  sw_init_info.timber_centres[1]:=27;
  sw_init_info.timber_centres[2]:=27.5;
  sw_init_info.timber_centres[3]:=27.5;
  sw_init_info.timber_centres[4]:=27.5;
  sw_init_info.timber_centres[5]:=27.5;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28;
  sw_init_info.timber_centres[8]:=25.5;   // switch rail joint
  sw_init_info.timber_centres[9]:=30;
  sw_init_info.timber_centres[10]:=30;
  sw_init_info.timber_centres[11]:=25;    // stock rail joint
  sw_init_info.timber_centres[12]:=0;     // zero list terminator.

             // group 7, size 3 of 3, ...

  add_to_list(7,3,3,'  BH  or  FB    1:40  ( C )  non-prototype  short  model  switch',' 1:40  model-only');

  //--------------------------------------------------------

  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------

                  // slip switches...

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=66;           // 1:24 slip Switch
  sw_init_info.heel_offset_inches:=2.75;       // heel at end of planing.
  sw_init_info.sw_pattern:=0;                  // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=76.5;      // arbitrary to suit timbering (1:6 slip).
  sw_init_info.valid_data:=True;               // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;  // straight switch
  sw_init_info.switch_rail:=0;                    // switch rail length.
  sw_init_info.stock_rail:=0;                     // stock rail length.
  sw_init_info.sleeper_j1:=0;            // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0;            // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;  // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=120;     // 1st timber beyond heel (will be blanked).
  sw_init_info.timber_centres[1]:=0;       // list terminator.

           // group 8, size 1 of 4, ...

  slip_switch_6:=add_to_list(8,1,4,'  BH  or  FB    1:24  ( A )  switch  for  1:6  slip',' 1:24  ( 1:6  slip )');     // 215a  slip_switch_6  global   index for make_slip functions

  //---------------

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=88;           // 1:32 slip Switch
  sw_init_info.heel_offset_inches:=2.75;       // heel at end of planing.
  sw_init_info.sw_pattern:=0;                  // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=76.5;      // arbitrary to suit timbering (1:7 slip).
  sw_init_info.valid_data:=True;               // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;  // straight switch
  sw_init_info.switch_rail:=0;                    // switch rail length.
  sw_init_info.stock_rail:=0;                     // stock rail length.
  sw_init_info.sleeper_j1:=0;            // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0;            // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;  // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=150;     // 1st timber beyond heel (will be blanked).
  sw_init_info.timber_centres[1]:=0;       // list terminator.

           // group 8, size 2 of 4, ...

  slip_switch_7:=add_to_list(8,2,4,'  BH  or  FB    1:32  ( B )  switch  for  1:7  slip',' 1:32  ( 1:7  slip )');             // 215a

  //-----------------------------

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=88;           // 1:32 slip Switch
  sw_init_info.heel_offset_inches:=2.75;       // heel at end of planing.
  sw_init_info.sw_pattern:=0;                  // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=103.5;     // 215a        was 99.5       // arbitrary to suit timbering (1:8 slip).
  sw_init_info.valid_data:=True;               // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;  // straight switch
  sw_init_info.switch_rail:=0;                    // switch rail length.
  sw_init_info.stock_rail:=0;                     // stock rail length.
  sw_init_info.sleeper_j1:=0;            // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0;            // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;  // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=150;     // 1st timber beyond heel (will be blanked).
  sw_init_info.timber_centres[1]:=0;       // list terminator.

           // group 8, size 3 of 4, ...

  slip_switch_8:=add_to_list(8,3,4,'  BH  or  FB    1:32  ( B )  switch  for  1:8  slip',' 1:32  ( 1:8  slip )');            // 215a

  //----------------------

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=110;          // 1:40 slip Switch
  sw_init_info.heel_offset_inches:=2.75;       // heel at end of planing.
  sw_init_info.sw_pattern:=0;                  // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=102;       // 215a        was 98.5      // arbitrary to suit timbering (1:10 slip).
  sw_init_info.valid_data:=True;               // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;  // straight switch
  sw_init_info.switch_rail:=0;                    // switch rail length.
  sw_init_info.stock_rail:=0;                     // stock rail length.
  sw_init_info.sleeper_j1:=0;            // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0;            // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=False;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;  // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).


  sw_init_info.timber_centres[0]:=200;     // 1st timber beyond heel (will be blanked).
  sw_init_info.timber_centres[1]:=0;       // list terminator.

           // group 8, size 4 of 4, ...

  slip_switch_10:=add_to_list(8,4,4,'  BH  or  FB    1:40  ( C )  switch  for  1:10  slip', ' 1:40  ( 1:10  slip )');        // 215a
  //--------------------------------------

      // 218a ...     group 9

      // tandem switches for 2nd switch in a tandem. Same as model switch, but with 3.5" toe, fewer timbers and no stock rail joints ...

  //--------------------------------------
  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------

    // 1:24 Tandem Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=66;      // 1:24 Tandem Switch, heel at planing.
  sw_init_info.heel_offset_inches:=2.75;
  sw_init_info.sw_pattern:=0;             // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=138;                    // switch rail length.
  sw_init_info.stock_rail:=0;            // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;       // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;           // fbtip dimension (FB foot from gauge-face at tip).

     // switch timbers centre to centre in inches. [first from toe].
     // same as 9ft Standard Straight Switch - except first timber 3.5" from toe...

  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=30;
  sw_init_info.timber_centres[2]:=30.5;
  sw_init_info.timber_centres[3]:=30;
  sw_init_info.timber_centres[4]:=30;
  sw_init_info.timber_centres[5]:=28.5;    // switch rail joint.
  sw_init_info.timber_centres[6]:=30;      // stagger 179" + 3.5" = 182.5" total
  sw_init_info.timber_centres[7]:=0;       // zero list terminator.

           // group 9, size 1 of 3, ...

  add_to_list(9,1,3,'  BH  or  FB    1:24  ( A )  switch  for  tandem',' 1:24  tandem  switch');

  // ----------------------------------
  // 1:32 Tandem Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=88;       // 1:32
  sw_init_info.heel_offset_inches:=2.75;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=174;             // switch rail length.
  sw_init_info.stock_rail:=0;            // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper).
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;          // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;           // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;  // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;        // fbtip dimension (FB foot from gauge-face at tip).

  sw_init_info.timber_centres[0]:=3.5;       //  same as 12ft Standard Straight Switch - except first timber 3.5" from toe.
  sw_init_info.timber_centres[1]:=26;
  sw_init_info.timber_centres[2]:=26;
  sw_init_info.timber_centres[3]:=26;
  sw_init_info.timber_centres[4]:=26.5;
  sw_init_info.timber_centres[5]:=26.5;
  sw_init_info.timber_centres[6]:=26.5;
  sw_init_info.timber_centres[7]:=25.5;    // switch rail joint.
  sw_init_info.timber_centres[8]:=25.5;
  sw_init_info.timber_centres[9]:=25.5;
  sw_init_info.timber_centres[10]:=25.5;   // 259.5" stagger +3.5"  = 263" total
  sw_init_info.timber_centres[11]:=0;

             // group 9, size 2 of 3, ...

  add_to_list(9,2,3,'  BH  or  FB    1:32  ( B )  switch  for  tandem',' 1:32  tandem  switch');

  // ----------------------------------
  // 1:40 Tandem Switch:

  clear_sw_init_info;            // first clear all.

  sw_init_info.old_size:=6;      // no longer used, but needed if file is reloaded in old version (pre 0.77.a).


  sw_init_info.planing:=0;
  sw_init_info.heel_lead_inches:=110;      // 1:40
  sw_init_info.heel_offset_inches:=2.75;
  sw_init_info.sw_pattern:=0;              // type of switch.  0 = curved planing or straight switch; -1 = semi-curved switch;  1 = double-curved switch.
  sw_init_info.switch_front_inches:=65;
  sw_init_info.valid_data:=True;             // valid data here
  sw_init_info.planing_angle:=0;
  sw_init_info.switch_radius_inchormax:=max_rad;    // straight switch
  sw_init_info.switch_rail:=210;             // switch rail length.
  sw_init_info.stock_rail:=0;            // stock rail length.
  sw_init_info.sleeper_j1:=0-24.5;       // sleeper J1 dimension (negative spacing from toe to first swith-front sleeper.
  sw_init_info.sleeper_j2:=0-28;         // sleeper J2 dimension (negative spacing from J1).
  sw_init_info.sleeper_j3:=0;            // sleeper J3 dimension (negative spacing from J2).
  sw_init_info.sleeper_j4:=0;            // sleeper J4 dimension (negative spacing from J3).
  sw_init_info.sleeper_j5:=0;            // sleeper J5 dimension (negative spacing from J4).
  sw_init_info.front_timbered:=True;

  sw_init_info.planing_radius:=0;            // planing radius (double-curved switch).
  sw_init_info.joggle_depth:=0.375;        // joggle depth (to be used only if joggled).
  sw_init_info.joggle_length:=6;            // joggle length (from toe, +ve) (to be used only if joggled).
  sw_init_info.joggled_stock_rail:=False;         // joggled stock rails?
  sw_init_info.fb_tip_offset:=2.75;            // fbtip dimension (FB foot from gauge-face at tip).

                   //  same as 15ft Standard Straight Switch - except first timber 3.5" from toe.

  sw_init_info.timber_centres[0]:=3.5;
  sw_init_info.timber_centres[1]:=27.5;
  sw_init_info.timber_centres[2]:=27.5;
  sw_init_info.timber_centres[3]:=27.5;
  sw_init_info.timber_centres[4]:=27.5;
  sw_init_info.timber_centres[5]:=27.5;
  sw_init_info.timber_centres[6]:=28;
  sw_init_info.timber_centres[7]:=28;
  sw_init_info.timber_centres[8]:=25.5;   // switch rail joint
  sw_init_info.timber_centres[9]:=28;
  sw_init_info.timber_centres[10]:=28;
  sw_init_info.timber_centres[11]:=28;
  sw_init_info.timber_centres[12]:=28;    // 331" stagger +3.5"  = 334.5" total
  sw_init_info.timber_centres[13]:=0;     // zero list terminator.

             // group 9, size 3 of 3, ...

  add_to_list(9,3,3,'  BH  or  FB    1:40  ( C )  switch  for  tandem',' 1:40  tandem  switch');

  //--------------------------------------------------------


  // group separator, blank line in list.

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');

  //-------------------------------------
  // custom switches, all size 1 of 1...

  clear_sw_init_info;         // clear all.

  sw_init_info.old_size:=6;   // custom switch - no longer used, but needed if file is reloaded in old version (pre 0.77.a).

  add_to_list(-1,1,1,'  custom  switch  -   slot  1','');
  add_to_list(-2,1,1,'  custom  switch  -   slot  2','');
  add_to_list(-3,1,1,'  custom  switch  -   slot  3','');
  add_to_list(-4,1,1,'  custom  switch  -   slot  4','');
  add_to_list(-5,1,1,'  custom  switch  -   slot  5','');
  add_to_list(-6,1,1,'  custom  switch  -   slot  6','');
  add_to_list(-7,1,1,'  custom  switch  -   slot  7','');
  add_to_list(-8,1,1,'  custom  switch  -   slot  8','');
  //----------------------------------------------

  clear_sw_init_info;   // clear all.

  add_to_list(0,0,0,'','');     // group separator, blank line in list.

  //------------------------------------------------
  // add bottom slot...

  clear_sw_init_info;         // clear all.

  sw_init_info.old_size:=6;   // custom switch - no longer used, but needed if file is reloaded in old version (pre 0.77.a).

  add_to_list(0,1,1,'  settings  as  most  recent  custom  switch','');    // bottom line.
end;
// ________________________________________________________

procedure Tswitch_select_form.FormDestroy(Sender: TObject);

var
  n:integer;

begin
  with switch_selector_listbox.Items do begin
    if Count>0 then for n:=0 to (Count-1) do Tswitch(Objects[n]).Free;
  end;//with
end;
//______________________________________________________________________________

end.

