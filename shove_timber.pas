
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

====================================================================================
*)

unit shove_timber;

{$MODE Delphi}

interface

uses
  LCLType, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons,
  shoved_timber;

type
  Tshove_timber_form = class(TForm)
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    hide_panel: TPanel;
    hide_button: TButton;
    restore_button: TButton;
    omit_button: TButton;
    restore_all_button: TButton;
    widen_button: TButton;
    mouse_groupbox: TGroupBox;
    mouse_along_button: TButton;
    mouse_throw_button: TButton;
    xtb_panel: TPanel;
    lengthen_button: TButton;
    shorten_button: TButton;
    narrow_button: TButton;
    number_panel: TPanel;
    drop_button: TBitBtn;
    normal_radio: TRadioButton;
    fine_radio: TRadioButton;
    extra_fine_radio: TRadioButton;
    Label1: TLabel;
    data_button: TButton;
    twist_cw_button: TButton;
    twist_acw_button: TButton;
    forward_button: TButton;
    backward_button: TButton;
    show_all_blue_checkbox: TCheckBox;
    normal_label: TLabel;
    fine_label: TLabel;
    extra_fine_label: TLabel;
    length_panel: TPanel;
    twist_panel: TPanel;
    zero_button: TButton;
    throw_panel: TPanel;
    width_panel: TPanel;
    help_shape: TShape;
    help_button: TButton;
    mouse_twist_button: TButton;
    mouse_length_button: TButton;
    mouse_width_button: TButton;
    twist_origin_button: TButton;
    add_bonus_button: TButton;
    reset_button: TButton;
    crab_panel: TPanel;
    mouse_crab_button: TButton;
    crab_left_button: TButton;
    crab_right_button: TButton;
    readout_label: TLabel;
    omit_all_button: TButton;
    inches_radio_buton: TRadioButton;
    mm_radio_button: TRadioButton;
    retain_shoves_on_mint_checkbox: TCheckBox;
    retain_shoves_on_make_checkbox: TCheckBox;
    timbering_length_label: TLabel;
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure hide_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure omit_buttonClick(Sender: TObject);
    procedure restore_buttonClick(Sender: TObject);
    procedure restore_all_buttonClick(Sender: TObject);
    procedure mouse_along_buttonClick(Sender: TObject);
    procedure mouse_throw_buttonClick(Sender: TObject);
    procedure how_panelClick(Sender: TObject);
    procedure widen_buttonClick(Sender: TObject);
    procedure lengthen_buttonClick(Sender: TObject);
    procedure shorten_buttonClick(Sender: TObject);
    procedure narrow_buttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure drop_buttonClick(Sender: TObject);
    procedure data_buttonClick(Sender: TObject);
    procedure twist_cw_buttonClick(Sender: TObject);
    procedure twist_acw_buttonClick(Sender: TObject);
    procedure normal_radioClick(Sender: TObject);
    procedure fine_radioClick(Sender: TObject);
    procedure extra_fine_radioClick(Sender: TObject);
    procedure backward_buttonClick(Sender: TObject);
    procedure forward_buttonClick(Sender: TObject);
    procedure show_all_blue_checkboxClick(Sender: TObject);
    procedure zero_buttonClick(Sender: TObject);
    procedure xtb_panelClick(Sender: TObject);
    procedure length_panelClick(Sender: TObject);
    procedure twist_panelClick(Sender: TObject);
    procedure throw_panelClick(Sender: TObject);
    procedure width_panelClick(Sender: TObject);
    procedure mouse_length_buttonClick(Sender: TObject);
    procedure mouse_width_buttonClick(Sender: TObject);
    procedure mouse_twist_buttonClick(Sender: TObject);
    procedure twist_origin_buttonClick(Sender: TObject);
    procedure add_bonus_buttonClick(Sender: TObject);
    procedure reset_buttonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure crab_left_buttonClick(Sender: TObject);
    procedure crab_right_buttonClick(Sender: TObject);
    procedure crab_panelClick(Sender: TObject);
    procedure mouse_crab_buttonClick(Sender: TObject);
    procedure mm_radio_buttonClick(Sender: TObject);
    procedure inches_radio_butonClick(Sender: TObject);
    procedure omit_all_buttonClick(Sender: TObject);
    procedure retain_shoves_on_mint_checkboxClick(Sender: TObject);
    procedure retain_shoves_on_make_checkboxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  shove_timber_form: Tshove_timber_form;

  //----------------------

  show_shove_fs: boolean = False;       // True = show dims in full-size inches.
  show_origin_k: boolean = False;       // True = show timber twist from template datum.

procedure shove_buttons(able: boolean; index: integer);    // enable/disable timber shove buttons.

procedure copy_shove_list(delete_list: boolean; var from_list, to_list: TShovedTimberList);

procedure shove_xtb_panel_click;
procedure shove_throw_panel_click;
procedure shove_twist_panel_click;
procedure shove_crab_panel_click;
procedure shove_length_panel_click;
procedure shove_width_panel_click;
//__________________________________________________________________________________

implementation

uses
  control_room, pad_unit, colour_unit, {grid_unit,} alert_unit, help_sheet, math_unit,
  enter_timber, {bgkeeps_unit,} entry_sheet, mark_unit;

{$R *.lfm}
//_______________________________________________________________________________________________

const
  shovetimb_help_str: string =
    'green_panel_begintree.gif These help notes are longer than most -- you may prefer to print them out. Click the `0PRINT`1 button above.green_panel_end' + '|      `0Shoving  Timbers  and  Sleepers`9' + '||Templot0 normally draws templates with the timbers of the correct sizes and in positions based on full-size railway practice, or in accordance with your size and spacing settings.' + '||The SHOVE functions permit you to change the position, alignment and size of individual timbers and sleepers within the control template, or to omit them from the template.' + '||"Timbers" are normally used for pointwork (switches, crossings and turnouts). The standard timber size for British standard-gauge railways is 12 inches wide by 6 inches thick. In Templot0 their numbers are prefixed S, T or X.' + '||"Sleepers" are normally used for plain track. The standard sleeper size for British standard-gauge railways is 10 inches wide by 5 inches thick. In Templot0 their numbers are prefixed A, B, E, J, N or R.' + '||The length of timbers varies as required to support the rails. Sleepers are normally of a standard fixed length,' + ' typically 9ft in the pre-grouping era and 8ft 6ins later. Sleepers can be shortened, but it is not usual for them to be longer than this; a timber would be used instead.' + ' For more information, see the help notes for the REAL > TIMBERING > TIMBERING DATA... menu item.' + '||You can shove or omit as many sleepers and timbers as you wish, or add additional "bonus timbers" to a template.' + '||Some of the reasons you might want to do this are these:' + '||Timbers may be shoved along the track to increase the space for point-rodding runs, etc.' + '||Timbers may be shoved along and/or twisted slightly to avoid conflicts where tracks come together in crossovers, junctions, etc.' + '||A minimum timber spacing of 2ft (centre-to-centre) is needed for rail joints and fishplates.' + '||Lengthened timbers can be used to support hand point-lever boxes, ground signals and detection devices, etc.' + '||Wider than standard timbers are sometimes used when this is necessary to accommodate chairs or baseplates which are offset or set at an angle.' + '||In the pre-grouping era, some railways used 12 inch wide timbers in place of standard sleepers adjacent to the rail joints in plain track.' + '||Timbers might be omitted over level (grade) crossings, inspection pits, inside engine sheds, etc., or where longitudinal baulk timbers (waybeams) are used (on the deck of girder bridges, over coal drops, etc.).' + '||Frequently at crossovers and junctions adjacent running lines are carried on long timbers across both tracks. This can be done by omitting timbers on one template, and extending the' + ' length of the timbers on the adjoining template accordingly.' + '||When a short stubby turnout for narrow-gauge or industrial sidings is required, it is sometimes difficult to find a timber spacing setting (in the REAL > TIMBERING > TIMBERING DATA... menu item) which gives the required result.' + ' It is often easier to leave the pre-set spacing and shove timbers manually to the required positions.' + '||There are also some purely model reasons for shoving timbers - to avoid baseboard joints, for example, or to clear point-motor linkages.' + '||In addition to the normal use for these shove timber functions, bonus "timbers" can be added to a template and heavily modified to represent a trackside structure such as a goods shed, cattle dock, coal bins, buffer stop, etc.' + '||Handy Hint :' + '|Do not begin shoving timbers until you are sure that the track plan is finalised.' + ' Shoving individual timbers is a slow process, and changing the size or alignment of a template nearly always means that a new timbering layout is needed.' + '||            Using  the  Shove  Timber  Functions :' + '||To change the size or position of a timber ("shove" it), the timber or sleeper which you want to shove must first be selected.' + '||There are two ways to do this. The easiest and quickest is to click on the NUMBER of the required timber (not the timber itself).' + ' The timber numbers will be highlighted as the mouse moves over them, and the timber selected for shoving shows in red.' + '||Sometimes it is not possible to click the timber number, for example when zoomed a long way in or out, or when one number is obscured by another,' + ' so the timber to be shoved can also be selected by entering its number directly.' + ' Click the blue down-arrow button to show an additional window, and enter the timber number in the box,' + ' or select a previously shoved timber from the drop-down list. Then click the OK button.' + '||When a timber has been selected you can shove it, i.e. change its size and/or position. First try doing this using the buttons:' + '||Clicking the FORWARD button causes the selected timber to be moved forwards along the track centre-line by 1 inch (scale) with each click. The forward direction is away from the CTRL-0 datum end of the template.' + '||Clicking the BACKWARD button causes the selected timber to be moved backwards along the track centre-line by 1 inch (scale) with each click. The backward direction is towards the CTRL-0 datum end of the template.' + '||Clicking the ACW button causes the selected timber to be twisted by 1 degree anticlockwise with each click.' + '||Clicking the CW button causes the selected timber to be twisted by 1 degree clockwise with each click.' + '||Clicking the CRAB-RIGHT button causes the selected timber to be moved sideways by 1 inch (scale) with each click. The crab-right direction is away from the CTRL-0 datum end of the template.' + '||Clicking the CRAB-LEFT button causes the selected timber to be moved sideways by 1 inch (scale) with each click. The crab-left direction is towards the CTRL-0 datum end of the template.' + '||N.B. Unlike FORWARD and BACKWARD moves, CRAB moves apply after any twisting. If the timber has not been twisted or EQUALIZED, it is better to use FORWARD or BACKWARD instead.' + '||Clicking the LENGTHEN button causes the selected timber to be lengthened by 6 inches (scale) with each click.' + '||Clicking the SHORTEN button causes the selected timber to be shortened by 6 inches (scale) with each click.' + '||Changes in length normally take place at the turnout-side end of the timber only, so that a timber can easily be extended across double tracks.' + ' If the SHIFT key is held down on the keyboard, the change in length will instead take place at the main-side end of the timber only.' + '||To throw a timber endways without changing its length, alternately lengthen it at one end and shorten it at the other.' + '||Clicking the WIDEN button causes the selected timber to be widened by 2 inches (scale) with each click.' + ' So a standard 10 inch wide sleeper can be converted to a 12 inch wide timber with a single click.' + '||Clicking the NARROW button causes the selected timber to be narrowed by 2 inches (scale) with each click.' + ' So a standard 12 inch wide timber can be converted to a 10 inch wide sleeper with a single click.' + '||To restore a shoved timber to its normal size and position, select it and then click the RESTORE TIMBER button.' + '||To remove a selected timber from the template, click the OMIT TIMBER button. Timbers which have been omitted retain their numbers, so that they can be selected for RESTORE TIMBER by clicking on the number.' + '||It is usually quicker to "click" the buttons by using the underlined accelerator keys shown on them. To quickly lengthen a timber for example, just keep pressing or hold down the L key on the keyboard.' + '||Now also try shoving timbers using the mouse actions. Select a timber by clicking on its number, and then click one of the ALONG, THROW, CRAB, TWIST, LENGTH or WIDTH mouse action buttons.' + ' To get a more precise response from the mouse, change the RESPONSE RATE option settings,' + ' or use the arrow keys on the number-pad to move the mouse pointer slowly (not the normal arrow keys - NUMLOCK must be on - see the ACTION > ? MOUSE ACTIONS HELP menu item).' + '||If the SHOW ALL SHOVED TIMBERS option box is ticked, all the timbers in the control template which have been shoved from their normal size or position will be shown in blue.' + '||Ocasionally a need arises for an additional timber. A "bonus" timber can be added to the template by clicking the ADD BONUS TIMBER button. You can add as many bonus timbers as you wish.' + ' Each one will appear initially as a plain track sleeper at the CTRL-1 rail joint position. It can then be shoved to the desired size and position in the usual way.' + ' To remove added bonus timbers, click the REAL > TIMBERING > BONUS TIMBERS > menu items.' + '||The RESTORE ALL button restores all the shoved timbers on the control template to their normal size and position.' + '||To change the size or spacings of ALL of the timbers, you should not use these shove timber functions, but select instead the REAL > TIMBERING >TIMBERING DATA...,' + ' or the REAL > PLAIN TRACK OPTIONS > RAIL LENGTHS AND SLEEPER SPACINGS... menu items.' + '||To omit ALL of the timbers, you should not use these shove timber functions, but select instead the REAL > TIMBERING > NO TIMBERING menu option.' + '||The read-out panels show the current dimension to the centre of the selected timber, its length, width and other dimensions. By clicking these panels you can set these dimensions directly.' + '||By clicking the ZERO button you can set a zero datum on a selected timber for the centre dimensions, which makes it easy to check or set timber-to-timber spacings.' + ' Click the RESET ZERO button to reset the timber centre dimensions to be from the template rail-end. For a turnout, this button additionally toggles between using the CTRL-0 rail end and the CTRL-1 stock rail joint positions as the zero datum.' + ' For plain track this button additionally toggles between using the CTRL-0 rail end position and the first rail joint (which may have been rolled-in from the CTRL-1 rail end) as the zero datum.' + '||You can swap between having these dimensions shown and set in model sizes (in mm) or full-size prototype sizes (in inches) by clicking the MODEL MM or FULL-SIZE INCHES option buttons.' + '||There are two options for the way the twist angle is shown and set. If the dimension is shown suffixed by a ''¬'' symbol, the angle is measured from the normal square-on position across the rails.' + ' If the dimension is shown suffixed by a ''÷'' symbol, the angle is measured from the CTRL-0 template datum. For a straight template there is no difference between these two options.' + ' For a curved template, these options can be toggled by clicking the ¬ / ÷ button.' + '||If you prefer to shove timbers by entering the shove data directly, click the DATA... button.' + '||Handy Hints :' + '||Do not begin shoving timbers until you are sure that the track plan is finalised.' + ' Shoving individual timbers is a slow process, and changing the size or alignment of a template nearly always means that a new timbering layout is needed.' + '||Clicking the timber number works only while the SHOVE TIMBER window is showing (REAL > SHOVE TIMBERS menu item, or SHIFT-F10), and the shove functions work only on the CONTROL TEMPLATE.' + ' If the timbers to be shoved are part of a background template, you must first make it the control template. For notes on how to do this, click the HELP > YOUR TEMPLATE STORAGE BOX menu item.' + '||While the SHOVE TIMBER window is showing and the control template is not hidden, it is not possible to get the pop-up menu for a background template simply by clicking on it.' + ' Instead, hold down the SHIFT key and click the template''s NAME LABEL, or first hide the control template (HOME key).' + '||Make a note of the underlined accelerator keys shown on the buttons. Then you can resize the form much smaller to avoid obstructing the drawing and still be able to use the buttons by pressing keys on the keyboard.' + '||For example, to widen a timber you can simply press the W key.' + '||For a useful list of all the timbers currently shoved on this template, click the blue down-arrow button, and then the down-arrow for the drop-down list.' + '||Do not switch off TIMBER CENTRE-LINES in the GENERATOR menu while shoving timbers.' + '||Using the shove timber functions is confusing if an excessive amount of timber randomizing is in force. Select the REAL > TIMBERING menu items to cancel randomizing while shoving timbers.' + '|----------------------------------------------------' + '||N.B. Selecting the SHOVE TIMBERS function has ensured that timber centre-lines and numbers are switched on in the GENERATOR menu while the SHOVE TIMBERS function is in use.' + '||The timber infill is also shown on the trackpad for the control template. If you do not want this while shoving,' + ' select the TRACKPAD > TRACKPAD CONTROL TEMPLATE OPTIONS > TIMBER INFILL > NONE menu item.' + '||Using the mouse actions for shoving will also enable the FULL MOUSE DRAW screen refresh option so that you can see the timbers being shoved.' + ' You may want to revert to the skeleton draw option when you have finished shoving timbers. To do this select the ACTION > MOUSE ACTION OPTIONS > SKELETON MOUSE DRAW menu item or SHIFT+CTRL-F12, or just press the ; (semi-colon) key.' + '||Bonus timbers are not affected by any blanking of turnout templates and unlike ordinary timbers they can be shoved into the blanked area.' + '||Bonus timbers and plain track sleepers can be shoved out beyond the CTRL-0 template datum end position if necessary.' + '||To shove an ordinary turnout timber out beyond the CTRL-0 datum end of a turnout template, it is first necessary to apply sufficient negative blanking to accommodate the shove.' + ' This can be done by moving the fixing peg out as far as needed (CTRL-F8 mouse action), and then clicking the DO > BLANK UP TO PEG menu item.';

var
  saved_info_panel: integer = 0;

  saved_timber_centres_flag: boolean = False;
  saved_timber_numbers_flag: boolean = False;
  saved_timber_outlines_flag: boolean = True;

  saved_timb_infill_style: integer = 3;
  saved_gen_infill: boolean = True;

  window_scaling: boolean = False;
// flag - otherwise ScrollInView on resize prevents form rescaling properly.

//________________________________________________________________________________________

procedure Tshove_timber_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  window_scaling := True;
  // otherwise ScrollInView on resize prevents form rescaling properly.

  if size_updown.Position > size_updown.Tag       // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                        // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                        // scale the form contents up.

  ClientHeight := VertScrollBar.Range;            // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 4;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;            // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;        // and save for the next click.

  window_scaling := False;
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  shove  timbers  dialog', Color);
  Show;             // ensure form doesn't get hidden.
  BringToFront;
end;
//________________________________________________________________________________________

procedure Tshove_timber_form.hide_buttonClick(Sender: TObject);

begin
  Close;
end;
//_________________________________________________________________________________________

procedure Tshove_timber_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(shove_timber_form);

  AutoScroll := True;
end;
//______________________________________________________________________________

procedure shove_buttons(able: boolean; index: integer);    // enable/disable timber shove buttons.

var
  omit: boolean;
  enable_restore: boolean;

begin
  if (index > -1) and (index < current_shove_list.Count) then begin
    omit := (current_shove_list[index].shoveCode = svcOmit);
    enable_restore := current_shove_list[index].CanRestore;
  end
  else begin
    omit := False;
    enable_restore := False;
  end;

  with shove_timber_form do begin

    retain_shoves_on_mint_checkbox.Checked := retain_shoves_on_mint;
    retain_shoves_on_make_checkbox.Checked := retain_shoves_on_make;

    if able = True then begin
      Caption := '      shove   timber / sleeper  ' + current_shove_str;
      if omit then begin
        Caption := Caption + '  ( omitted )';
      end;
      number_panel.Caption := current_shove_str;
    end
    else begin
      xtb_panel.Caption := '';
      length_panel.Caption := '';
      width_panel.Caption := '';
      throw_panel.Caption := '';
      twist_panel.Caption := '';
      crab_panel.Caption := '';
      Caption := '      shove   timber / sleeper';
      number_panel.Caption := '';
    end;

    restore_button.Enabled := enable_restore;
    // selected timber has been shoved.

    restore_all_button.Enabled := (current_shove_list.Count > 0);
    // 0.95.a was >1     // there are others in list.

    zero_button.Enabled := able;

    if (omit) then begin
      able := False;    // don't enable these if it's omitted.
    end;
    omit_button.Enabled := able;
    widen_button.Enabled := able;
    narrow_button.Enabled := able;
    lengthen_button.Enabled := able;
    shorten_button.Enabled := able;

    mouse_along_button.Enabled := able;
    mouse_throw_button.Enabled := able;
    mouse_length_button.Enabled := able;
    mouse_crab_button.Enabled := able;
    mouse_width_button.Enabled := able;
    mouse_twist_button.Enabled := able;

    twist_cw_button.Enabled := able;
    crab_left_button.Enabled := able;
    crab_right_button.Enabled := able;
    twist_acw_button.Enabled := able;
    forward_button.Enabled := able;
    backward_button.Enabled := able;

    xtb_panel.Enabled := able;
    length_panel.Enabled := able;
    width_panel.Enabled := able;
    throw_panel.Enabled := able;
    twist_panel.Enabled := able;
    crab_panel.Enabled := able;
    readout_label.Visible := able;

    data_button.Enabled := able;

  end;//with
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.FormShow(Sender: TObject);

begin

  with pad_form do begin
    if timber_centres_menu_entry.Checked = False then
      timber_centres_menu_entry.Click;
    // must have the centre-lines for timber selection to work (toggle).
    if timber_numbers_menu_entry.Checked = False then
      timber_numbers_menu_entry.Click;  // need the numbers (toggle).

    saved_gen_infill := timbering_infill;
    if timbering_infill = False then
      timbering_infill_menu_entry.Click;  // put timbering infill on in generator.

    saved_timb_infill_style := pad_timb_infill_style;
    if pad_timb_infill_style = 0 then
      solid_timbering_infill_menu_entry.Click;


    saved_timber_centres_flag := current_timber_centres_menu_entry.Checked;
    saved_timber_numbers_flag := current_timber_numbers_menu_entry.Checked;
    saved_timber_outlines_flag := current_timber_outlines_menu_entry.Checked;

    current_timber_centres_menu_entry.Checked := True;
    current_timber_numbers_menu_entry.Checked := True;
    current_timber_outlines_menu_entry.Checked := True;

  end;//with

  current_shove_str := '';
  shovetimbx := 0;
  shovetimbx_zero := 0;
  shove_buttons(False, -1);

  show_and_redraw(True, False);   // in case hidden (when idle, no rollback).
end;
//_______________________________________________________________________________________

procedure Tshove_timber_form.omit_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    current_shove_list[n].MakeOmit;
    shove_buttons(True, n);
    cancel_adjusts(False);        // can't continue to adjust it.
    show_and_redraw(True, True);
  end;
end;
//_________________________________________________________________________________________

procedure Tshove_timber_form.widen_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    current_shove_list[n].AdjustWidth(inscale);
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//_________________________________________________________________________________________

procedure Tshove_timber_form.narrow_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                           // valid slot.
  then begin
    current_shove_list[n].AdjustWidth(-inscale);
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//___________________________________________________________________________________________

procedure Tshove_timber_form.lengthen_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                           // valid slot.
  then begin
    current_shove_list[n].AdjustLength(scale / 2); // 6 inches
    if GetKeyState(VK_SHIFT) < 0 then         // shift key down = msb set...
    begin
      current_shove_list[n].AdjustOffset(-scale / 2);
      // shove back by same amount to lengthen main side.
    end;
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.shorten_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                           // valid slot.
  then begin
    current_shove_list[n].AdjustLength(-scale / 2);
    if GetKeyState(VK_SHIFT) < 0 then         // shift key down = msb set...
    begin
      current_shove_list[n].AdjustOffset(scale / 2);
      // shove over by same amount to shorten main side.
    end;
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.twist_cw_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                           // valid slot.
  then begin
    current_shove_list[n].AdjustAngle(DegreesToRadians(-hand_i)); // clockwise 1 degree.
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//________________________________________________________________________________________

procedure Tshove_timber_form.twist_acw_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                           // valid slot.
  then begin
    current_shove_list[n].AdjustAngle(DegreesToRadians(hand_i)); // anti-clockwise 1 degree.
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//___________________________________________________________________________________________

procedure Tshove_timber_form.backward_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                           // valid slot.
  then begin
    current_shove_list[n].AdjustXtb(-inscale);
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//______________________________________________________________________________________

procedure Tshove_timber_form.forward_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                           // valid slot.
  then begin
    current_shove_list[n].AdjustXtb(inscale);
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//____________________________________________________________________________________

procedure Tshove_timber_form.crab_left_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                                  // valid slot.
  then begin
    current_shove_list[n].AdjustCrab(-inscale);
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//_____________________________________________________________________________________

procedure Tshove_timber_form.crab_right_buttonClick(Sender: TObject);

var
  n: integer;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0                                  // valid slot.
  then begin
    current_shove_list[n].AdjustCrab(inscale);
    shove_buttons(True, n);
    show_and_redraw(True, True);
  end;
end;
//______________________________________________________________________________________

procedure Tshove_timber_form.restore_buttonClick(Sender: TObject);

var
  n: integer;

begin
  cancel_adjusts(False);

  repeat
    n := find_shove(current_shove_str, False);   // don't create it if not present.
    if n >= 0                                   // valid slot.
    then begin
      current_shove_list.Delete(n);
    end;
  until n < 0;        // might be in more than one slot, so don't BREAK.

  shove_buttons(True, -1);
  show_and_redraw(True, True);
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.restore_all_buttonClick(Sender: TObject);

begin
  if alert(7, '    restore  all  timbers',
    'You are about to restore any shoved or omitted timbers on this template to their normal size and position.'
    + '||This will delete all existing shove-timber data for this template.'
    + '||Any bonus timbers which have been added and shoved will be restored to their normal size and starting position, but will remain on the template.' + ' To remove bonus timbers from the template, click the REAL > TIMBERING > BONUS TIMBERS > REMOVE menu items.', '', '', '', '', 'cancel  restore   -   no  changes', 'O K', 0) = 5 then
    EXIT;
  Show;
  BringToFront;
  cancel_adjusts(False);
  clear_shovedata;
  shove_buttons(True, -1);
  show_and_redraw(True, True);
end;
//_____________________________________________________________________________________

procedure Tshove_timber_form.mouse_along_buttonClick(Sender: TObject);

begin
  shove_along_mouse_action;
end;
//____________________________________________________________________________________

procedure Tshove_timber_form.mouse_throw_buttonClick(Sender: TObject);

begin
  shove_throw_mouse_action;
end;
//______________________________________________________________________________________

procedure Tshove_timber_form.mouse_crab_buttonClick(Sender: TObject);

begin
  shove_crab_mouse_action;
end;
//_____________________________________________________________________________________

procedure Tshove_timber_form.mouse_length_buttonClick(Sender: TObject);

begin
  shove_length_mouse_action;
end;
//______________________________________________________________________________________

procedure Tshove_timber_form.mouse_width_buttonClick(Sender: TObject);

begin
  shove_width_mouse_action;
end;
//______________________________________________________________________________________

procedure Tshove_timber_form.mouse_twist_buttonClick(Sender: TObject);

begin
  shove_twist_mouse_action;
end;
//______________________________________________________________________________________

procedure Tshove_timber_form.how_panelClick(Sender: TObject);

begin
  help(0, shovetimb_help_str, '');
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.drop_buttonClick(Sender: TObject);

// he wants to enter the timber number...
begin
  cancel_adjusts(False);
  enter_timber_form.Show;
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  delete_null_shove_entries;   // tidy the list.

  enter_timber_form.Close;

  if Showing = True    // (might be called when not showing) first restore pad settings we found onShow.
  then begin

    with pad_form do begin

      if saved_timber_centres_flag = False then
        current_timber_centres_menu_entry.Checked := False;
      if saved_timber_numbers_flag = False then
        current_timber_numbers_menu_entry.Checked := False;
      if saved_timber_outlines_flag = False then
        current_timber_outlines_menu_entry.Checked := False;

      if (saved_gen_infill = False) and (timbering_infill = True) then
        timbering_infill_menu_entry.Click;  // generator setting restored.

      case saved_timb_infill_style of
        0:
          no_timbering_infill_menu_entry.Click;
        1:
          hatched_timbering_infill_menu_entry.Click;
        2:
          crosshatched_timbering_infill_menu_entry.Click;
        3:
          solid_timbering_infill_menu_entry.Click;
        4:
          blank_timbering_infill_menu_entry.Click;
      end;//case

    end;//with

    cancel_adjusts(False);
    show_and_redraw(True, False);
  end;
end;
//________________________________________________________________________________________

procedure Tshove_timber_form.data_buttonClick(Sender: TObject);

const
  mod_str: string =
    '||Enter a dimension by which the normal size and position of this timber should be modified.'
    +
    '||To return this timber to its normal size and position, enter 0 (zero).' +
    '||Handy hints:' + '||It is usually much easier to position a timber by using the buttons and mouse actions, instead of direct entry of data here.' + '||These dimensions are relative to the normal size and position of the timber, not the present size and position if it has been shoved previously.' + '||Instead of these relative dimensions, you can enter the actual timber centre dimension, timber length, width or twist angle, by clicking the relevant read-out panel.';

var
  n, i: integer;
  od: Toutdim;
  help_str: string;
  shove: TShovedTimber;

begin
  n := find_shove(current_shove_str, True);
  if n >= 0 then                              // valid slot.
  begin
    help_str := '    Data for Shoving Timber ' + current_shove_str + mod_str;

    shove := current_shove_list[n];

    putdim(help_str, 1, 'shove  timber  along  by', shove.xtbModifier,
      False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
    putdim(help_str, 3, 'twist  timber  by', shove.angleModifier *
      180 / Pi * hand_i, False, True, False, False);
    // neg ok, no preset, 0 ok, don't terminate on zero.
    putdim(help_str, 1, 'crab  timber  sideways  by', shove.crabModifier,
      False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
    putdim(help_str, 1, 'throw  timber  endways  by', shove.offsetModifier,
      False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
    putdim(help_str, 1, 'lengthen / shorten  timber  by  ( +/- )', shove.lengthModifier,
      False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
    i := putdim(help_str, 1, 'widen / narrow  timber  by  ( +/- per  side )',
      shove.widthModifier, False, True, False, False);
    // neg ok, no preset, 0 ok, don't terminate on zero.

    if i <> 5 then
      EXIT;

    if getdims('shoving  timber  ' + current_shove_str, '', pad_form, i, od) then begin
      shove.xtbModifier := od[0];
      shove.angleModifier := od[1] * Pi / 180 * hand_i;
      shove.crabModifier := od[2];
      shove.offsetModifier := od[3];
      shove.lengthModifier := od[4];
      shove.widthModifier := od[5];

      shove.shoveCode := svcShove;                 // might previously have been be omitted.
      shove_buttons(True, n);
      show_and_redraw(True, True);
    end;
  end;
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.normal_radioClick(Sender: TObject);

begin
  shove_mouse_factor := 2.0;    // arbitrary.
end;
//__________________________

procedure Tshove_timber_form.fine_radioClick(Sender: TObject);

begin
  shove_mouse_factor := 6.0;    // arbitrary.
end;
//___________________________

procedure Tshove_timber_form.extra_fine_radioClick(Sender: TObject);

begin
  shove_mouse_factor := 20.0;   // arbitrary.
end;
//_________________________________________________________________________________________

procedure Tshove_timber_form.show_all_blue_checkboxClick(Sender: TObject);

begin
  show_and_redraw(True, False);
end;
//____________________________________________________________________________________________

procedure Tshove_timber_form.mm_radio_buttonClick(Sender: TObject);

begin
  show_shove_fs := False;
  show_and_redraw(True, False);
end;
//_________________________________

procedure Tshove_timber_form.inches_radio_butonClick(Sender: TObject);

begin
  show_shove_fs := True;
  show_and_redraw(True, False);
end;
//____________________________________________________________________________________________

procedure Tshove_timber_form.twist_origin_buttonClick(Sender: TObject);

begin
  show_origin_k := not show_origin_k;
  if show_origin_k = True then begin
    twist_origin_button.Caption := '¬';
    twist_origin_button.Hint := ' show  or  set  twist  angle  from  square-on ';
  end
  else begin
    twist_origin_button.Caption := '÷';
    twist_origin_button.Hint := ' show  or  set  twist  angle  from  template  datum ';
  end;

  show_and_redraw(True, False);
end;
//____________________________________________________________________________________________

procedure Tshove_timber_form.zero_buttonClick(Sender: TObject);

begin
  shovetimbx_zero := shovetimbx;     // set zero datum for centre read-out on timber.
  show_and_redraw(True, False);
end;
//___________________________________________________________________________________________

procedure Tshove_timber_form.reset_buttonClick(Sender: TObject);

begin
  if shovetimbx_zero = 0    // already on datum?
  then begin                                   // yes, toggle to rail-joint.
    if plain_track = False then
      shovetimbx_zero := xorg
    // Ctrl-1 for a turnout.
    else
      shovetimbx_zero := turnoutx - tb_roll_percent * railen[pt_i] * inscale / 100;
    // rolled rail joint for plain track.
  end
  else
    shovetimbx_zero := 0;     // reset on Ctrl-0 datum for centre read-out.
  show_and_redraw(True, False);
end;
//___________________________________________________________________________________________

procedure Tshove_timber_form.xtb_panelClick(Sender: TObject);

begin
  shove_xtb_panel_click;
end;
//_________________________________

procedure shove_xtb_panel_click;

const
  mod_str: string = '||Enter a new centre dimension for this timber.' +
    '||If you have previously clicked the ZERO button to set a zero datum for the timber centres, this dimension will be from that datum position.' + '||If you have not previously set a zero datum for the timber centres, or you have previously clicked the RESET ZERO button, this dimension will be from the CTRL-0 template datum.' + '||Handy hints:' + '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the MODEL MM or FULL-SIZE INCHES option buttons.' + '||To enter shove data relative to the normal position of this timber, cancel this and click the DATA... button instead.' + '||It is usually much easier to position a timber by using the buttons and mouse actions, instead of direct entry of data here.';

var
  n, i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;
  shove: TShovedTimber;

begin
  if show_shove_fs = True then begin
    fs_convert := inscale;
    fs_str := '  ( full  size  inches )  ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := '  ( model  mm )  ';
    fs_code := 1;
  end;

  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    shove := current_shove_list[n];

    i := putdim('    Set Centre of Timber ' + current_shove_str + mod_str,
      fs_code, 'timber  ' + current_shove_str + '  centre' + fs_str,
      (shovetimbx - shovetimbx_zero) / fs_convert, False, True, False, False);
    // neg ok, no preset, 0 ok, don't terminate on zero.
    if i <> 0 then
      EXIT;

    if getdims('timber  ' + current_shove_str + '  centre  dimension', '', pad_form, i, od) =
      True then begin
      shove.AdjustXtb(od[0] * fs_convert - shovetimbx + shovetimbx_zero);
      // modify shove data.
      shove_buttons(True, n);
      show_and_redraw(True, True);
    end;
  end;
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.length_panelClick(Sender: TObject);

begin
  shove_length_panel_click;
end;
//_________________________________

procedure shove_length_panel_click;

const
  mod_str: string = '||Enter a new length for this timber.' + '||Handy hints:'
    + '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the MODEL MM or FULL-SIZE INCHES option buttons.' + '||To enter shove data relative to the normal length of this timber, cancel this and click the DATA... button instead.' + '||It is usually much easier to lengthen or shorten a timber by using the buttons and mouse actions, instead of direct entry of data here.';

var
  n, i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;
  shove: TShovedTimber;
begin
  if show_shove_fs = True then begin
    fs_convert := inscale;
    fs_str := '  ( full  size  inches )  ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := '  ( model  mm )  ';
    fs_code := 1;
  end;

  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    shove := current_shove_list[n];

    i := putdim('    Set Length of Timber ' + current_shove_str + mod_str,
      fs_code, 'timber  ' + current_shove_str + '  length' + fs_str, shovetimb_len / fs_convert,
      False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
    if i <> 0 then
      EXIT;

    if getdims('timber  ' + current_shove_str + '  length', '', pad_form, i, od) =
      True then begin
      shove.AdjustLength(od[0] * fs_convert - shovetimb_len);
      // modify shove data.
      shove_buttons(True, n);
      show_and_redraw(True, True);
    end;
  end;
end;
//________________________________________________________________________________________

procedure Tshove_timber_form.crab_panelClick(Sender: TObject);

begin
  shove_crab_panel_click;
end;
//_________________________________

procedure shove_crab_panel_click;

const
  mod_str: string =
    '||Enter a dimension for the amount by which this timber is to be shoved sideways after any twisting.'
    + '||N.B If this timber has not been twisted, i.e. it is SQUARE-ON to the main road, the effect of a crab sideways shove will be visually the same as shoving its centre ALONG.' + ' However, any crab sideways shove will not be reflected in the CENTRE read-out for this timber, nor in setting a ZERO datum on it.' + ' To avoid confusion, it is recommended that crab shoves should only be applied to timbers which have been twisted or are EQUALIZED.' + ' To shove SQUARE-ON timbers sideways, instead use the ALONG mouse action or the FORWARD and BACKWARD buttons, or click the CENTRE read-out.' + '||Handy hints:' + '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the MODEL MM or FULL-SIZE INCHES option buttons.' + '||It is usually much easier to shove a timber by using the buttons and mouse actions, instead of direct entry of data here.';

var
  n, i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;
  shove: TShovedTimber;

begin
  if show_shove_fs = True then begin
    fs_convert := inscale;
    fs_str := ' ( full  size  inches ) ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := ' ( model  mm ) ';
    fs_code := 1;
  end;

  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    shove := current_shove_list[n];

    i := putdim('    Set Crab Sideways Shove for Timber ' + current_shove_str +
      mod_str, fs_code, 'timber ' + current_shove_str + ' crab sideways shove' + fs_str,
      shovetimb_crab / fs_convert, False, True, False, False);
    // neg ok, no preset, 0 ok, don't terminate on zero.
    if i <> 0 then
      EXIT;

    if getdims('timber  ' + current_shove_str + '  crab  sideways  shove', '', pad_form, i, od) =
      True then begin
      shove.AdjustCrab(od[0] * fs_convert - shovetimb_crab);
      // modify shove data.
      shove_buttons(True, n);
      show_and_redraw(True, True);
    end;
  end;
end;
//_________________________________________________________________________________________

procedure Tshove_timber_form.width_panelClick(Sender: TObject);

begin
  shove_width_panel_click;
end;
//_________________________________

procedure shove_width_panel_click;

const
  mod_str: string = '||Enter a new width for this timber.' + '||Handy hints:' +
    '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the MODEL MM or FULL-SIZE INCHES option buttons.' + '||To enter shove data relative to the normal width of this timber, cancel this and click the DATA... button instead.' + '||It is usually much easier to widen or narrow a timber by using the buttons and mouse actions, instead of direct entry of data here.';

var
  n, i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;
  shove: TShovedTimber;

begin
  if show_shove_fs = True then begin
    fs_convert := inscale;
    fs_str := '  ( full  size  inches )  ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := '  ( model  mm )  ';
    fs_code := 1;
  end;

  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    shove := current_shove_list[n];

    i := putdim('    Set Width of Timber ' + current_shove_str + mod_str,
      fs_code, 'timber  ' + current_shove_str + '  width' + fs_str, shovetimb_wide / fs_convert,
      False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
    if i <> 0 then
      EXIT;

    if getdims('timber  ' + current_shove_str + '  width', '', pad_form, i, od) =
      True then begin
      shove.AdjustWidth(od[0] * fs_convert - shovetimb_wide / 2);
      // modify shove data (/2 because sv_w is per side).
      shove_buttons(True, n);
      show_and_redraw(True, True);
    end;
  end;
end;
//_________________________________________________________________________________________

procedure Tshove_timber_form.twist_panelClick(Sender: TObject);

begin
  shove_twist_panel_click;
end;
//_________________________________

procedure shove_twist_panel_click;

const
  mod_str1: string = '||Enter a new twist angle in degrees for this timber.' +
    '||There are two options for the way the twist angle is shown and set. If the dimension is shown suffixed by a ''¬'' symbol, the angle is measured from the normal square-on position across the rails.' + ' If the dimension is shown suffixed by a ''÷'' symbol, the angle is measured from the CTRL-0 template datum. For a straight template there is no difference between these two options.' + ' For a curved template, these options can be toggled by clicking the ¬ / ÷ button.';

  mod_str_sq: string = 'This means that:' +
    '||Positive angles rotate the timber anti-clockwise from a square-on position across the rails.'
    + '||Negative angles rotate the timber clockwise from a square-on position across the rails.'
    + '||Entering a zero twist angle will set the timber square to the rails (turnout main road).';

  mod_str_0: string = 'This means that:' +
    '||Positive angles rotate the timber anti-clockwise from the template datum.'
    + '||Negative angles rotate the timber clockwise from the template datum.'
    + '||Entering a zero twist angle will set the timber square to the track centre-line at the template datum (CTRL-0 peg position).';

  mod_str2: string = '||Handy hints:' +
    '|To enter the twist angle as a unit angle instead of degrees, prefix it with a letter n. For example, entering "n8" will set a twist angle of 1:8, "n-20" will set a twist angle of [-1:20].' + '||To enter shove data relative to the normal twist angle for this timber, cancel this and click the DATA... button instead.' + '||It is usually much easier to rotate a timber by using the buttons and mouse action, instead of direct entry of data here.';

var
  n, i: integer;
  od: Toutdim;
  opt_str: string;
  code_str: string;
  shove: TShovedTimber;
begin
  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    if show_origin_k = True then begin
      opt_str := '||The current option setting is "÷ from template datum". ' +
        mod_str_0;
      code_str := '( from  datum  ÷ )';
    end
    else begin
      opt_str := '||The current option setting is "¬ from square-on". ' + mod_str_sq;
      code_str := '( from  square  ¬ )';
    end;

    shove := current_shove_list[n];

    i := putdim('    Set Twist Angle for Timber ' + current_shove_str +
      mod_str1 + opt_str + mod_str2, 3, 'timber  ' + current_shove_str + '  twist  angle  ' +
      code_str, shovetimb_keq * 180 / Pi, False, True, False, False);
    // neg ok, no preset, 0 ok, don't terminate.
    if i <> 0 then
      EXIT;

    if getdims('timber  ' + current_shove_str + '  twist  angle', '', pad_form, i, od) = True
    then begin
      shove.AdjustAngle(DegreesToRadians((od[0] - shovetimb_keq * 180 / Pi) * hand_i));
      // modify shove data.
      shove_buttons(True, n);
      show_and_redraw(True, True);
    end;
  end;
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.throw_panelClick(Sender: TObject);

begin
  shove_throw_panel_click;
end;
//_________________________________

procedure shove_throw_panel_click;

const
  mod_str: string =
    '||Enter a dimension for the amount by which this timber is to be thrown endways after any twisting.'
    + '||N.B If this timber has been twisted to lie parallel to the rails, i.e. twisted 90 degress from SQUARE-ON, the effect of a endwways throw will be visually the same as shoving its centre ALONG.' + ' However, any endways throw will not be reflected in the CENTRE read-out for this timber, nor in setting a ZERO datum on it.' + ' To avoid confusion, it is recommended that end throws should not be applied to timbers which have been so twisted.' + ' To shove such timbers endways, instead use the ALONG mouse action or the FORWARD and BACKWARD buttons, or click the CENTRE read-out.' + '||Handy hints:' + '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the MODEL MM or FULL-SIZE INCHES option buttons.' + '||It may be easier to throw a timber endways by lengthening it at one end and then shortening it at the other using the buttons (hold down the SHIFT key to modify the numbered end), or by using the THROW mouse action,' + ' instead of direct entry of data here.';

var
  n, i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;
  shove: TShovedTimber;

begin
  if show_shove_fs = True then begin
    fs_convert := inscale;
    fs_str := ' ( full  size  inches ) ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := ' ( model  mm ) ';
    fs_code := 1;
  end;

  n := find_shove(current_shove_str, True);
  if n >= 0                               // valid slot.
  then begin
    shove := current_shove_list[n];

    i := putdim('    Set Endways Throw for Timber ' + current_shove_str +
      mod_str, fs_code, 'timber ' + current_shove_str + ' endways  throw' + fs_str,
      shovetimb_throw / fs_convert, False, True, False, False);
    // neg ok, no preset, 0 ok, don't terminate on zero.
    if i <> 0 then
      EXIT;

    if getdims('timber  ' + current_shove_str + '  endways  throw', '', pad_form, i, od) =
      True then begin
      shove.AdjustOffset(od[0] * fs_convert - shovetimb_throw);
      // modify shove data.
      shove_buttons(True, n);
      show_and_redraw(True, True);
    end;
  end;
end;
//____________________________________________________________________________________________

procedure copy_shove_list(delete_list: boolean; var from_list, to_list: TShovedTimberList);
begin
  if to_list = nil then
    to_list := TShovedTimberList.Create;    // first create or clear the destination...

  to_list.CopyFrom(from_list);

  if delete_list then
    from_list.Free;
end;
//_______________________________________________________________________________________


procedure Tshove_timber_form.add_bonus_buttonClick(Sender: TObject);

var
  n: integer;
  shove: TShovedTimber;

begin
  cancel_adjusts(False);
  pad_form.add_bonus_timber_menu_entry.Click;
  show_and_redraw(False, True);                 // force a redraw to create the added timber.

  if bontimb < 1 then
    EXIT;     // ???

  current_shove_str := 'B' + IntToStr(bontimb);

  n := find_shove(current_shove_str, True);     // find it or create an empty slot.
  if n >= 0                                    // valid slot.
  then begin
    shove := current_shove_list[n];
    if shove.shoveCode = svcEmpty              // new slot.
    then begin
      shove.shoveCode := svcShove;
      // flag to shove this timber.
    end;
    shove_buttons(True, n);
  end;
  show_and_redraw(True, False);
end;
//________________________________________________________________________________________

procedure Tshove_timber_form.FormResize(Sender: TObject);

begin
  if (Showing = True) and (initdone_flag = True) and (window_scaling = False) then
    ScrollInView(hide_panel);
end;
//__________________________________________________________________________________________

procedure Tshove_timber_form.omit_all_buttonClick(Sender: TObject);

var
  i, n: integer;

  code: EMarkCode;
  ptr_1st: ^Tmark;         // pointer to a Tmark record..
  markmax: integer;
  num_str, tbnum_str: string;

begin
  if marks_list_ptr = nil then
    EXIT;        // pointer to marks list not valid.

  if omit_all_msg_pref = False then begin

    alert_box.preferences_checkbox.Checked := False;       //%%%%
    alert_box.preferences_checkbox.Show;

    i := alert(7, '    omit  all  timbers',
      'You are about to omit all the timbers and/or sleepers on this template.'
      +
      '||This function is intended to be used when required before any other timber shoving is done. This function is used to omit many unwanted timbers before restoring the fewer individual timbers which are actually wanted.' + ' To create a template without any timbers, cancel this and click instead the REAL > TIMBERING > NO TIMBERING menu item.' + '||Any previously shoved timbers will first be restored to their normal size and position, and then omitted.' + ' If omitted timbers are subsequently restored, they will therefore reappear at their normal size and position.' + '||Any bonus timbers which have been added and shoved will also be first restored to their normal size and starting position.' + ' To completely remove bonus timbers from the template, click the REAL > TIMBERING > BONUS TIMBERS > REMOVE menu items.', '', '', '', '', 'cancel  omit', 'O K', 0);

    //%%%%  was"today"

    omit_all_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
    alert_box.preferences_checkbox.Hide;

    if i = 5 then
      EXIT;
  end;

  markmax := High(marks_list_ptr);  // max index for the present list.

  if mark_index > markmax then
    mark_index := markmax;  // ??? shouldn't be.

  tbnum_str := timb_numbers_str;
  // the full string of timber numbering for the control template.

  n := 0;                        // keep compiler happy.
  num_str := current_shove_str;  // keep compiler happy.

  for i := 0 to (mark_index - 1) do begin     // (mark_index is always the next free slot)
    try
      ptr_1st := @marks_list_ptr[i];  // pointer to the next Tmark record.
      if ptr_1st = nil then
        EXIT;

      code := ptr_1st^.code;

      if code <> eMC_99_TimberNumber then
        CONTINUE;   // we are only looking for timber number entries.

      num_str := timb_num_strip(extract_tbnumber_str(tbnum_str));
      // get next timber numbering string from the acummulated string.

      if num_str = '' then
        CONTINUE;

      n := find_shove(num_str, True);     // find it or create an empty slot.
      if n >= 0                          // valid slot.
      then begin
        current_shove_list[n].MakeOmit;
      end
      else
        CONTINUE;
    except
      CONTINUE;
    end;//try
  end;//next i

  current_shove_str := num_str;   // last one omitted.
  shove_buttons(True, n);
  cancel_adjusts(False);        // can't continue to adjust it.
  show_and_redraw(True, True);
end;
//_________________________________________________________________________________________

procedure Tshove_timber_form.retain_shoves_on_mint_checkboxClick(Sender: TObject); // 0.94.a

begin
  retain_shoves_on_mint := retain_shoves_on_mint_checkbox.Checked;
end;
//______________________________________________________________________________

procedure Tshove_timber_form.retain_shoves_on_make_checkboxClick(Sender: TObject); // 0.94a

begin
  retain_shoves_on_make := retain_shoves_on_make_checkbox.Checked;
end;
//______________________________________________________________________________

end.
