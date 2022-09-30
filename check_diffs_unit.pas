
(*
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  Martin Wynne.  email: martin@templot.com
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

unit check_diffs_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  Tcheck_diffs_form = class(TForm)
    datestamp_label: TLabel;
    help_shape: TShape;
    readout_label: TLabel;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    hide_panel: TPanel;
    hide_button: TButton;
    reset_button: TButton;
    reset_all_button: TButton;
    mouse_groupbox: TGroupBox;
    mouse_length_button: TButton;
    mouse_flare_button: TButton;
    mouse_gap_button: TButton;
    lengthen_button: TButton;
    shorten_button: TButton;
    number_panel: TPanel;
    data_button: TButton;
    length_panel: TPanel;
    flare_panel: TPanel;
    help_button: TButton;
    gap_panel: TPanel;
    inches_radio_button: TRadioButton;
    mm_radio_button: TRadioButton;
    flare_groupbox: TGroupBox;
    bent_button: TButton;
    machined_button: TButton;
    flare_restore_button: TButton;
    omit_button: TButton;
    no_flare_button: TButton;
    unadjusted_settings_button: TButton;
    retain_diffs_on_mint_checkbox: TCheckBox;
    retain_diffs_on_make_checkbox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure hide_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure colour_panelClick(Sender: TObject);
    procedure reset_all_buttonClick(Sender: TObject);
    procedure mm_radio_buttonClick(Sender: TObject);
    procedure inches_radio_buttonClick(Sender: TObject);
    procedure data_buttonClick(Sender: TObject);
    procedure bent_buttonClick(Sender: TObject);
    procedure machined_buttonClick(Sender: TObject);
    procedure flare_restore_buttonClick(Sender: TObject);
    procedure length_panelClick(Sender: TObject);
    procedure flare_panelClick(Sender: TObject);
    procedure gap_panelClick(Sender: TObject);
    procedure reset_buttonClick(Sender: TObject);
    procedure omit_buttonClick(Sender: TObject);
    procedure mouse_length_buttonClick(Sender: TObject);
    procedure mouse_flare_buttonClick(Sender: TObject);
    procedure mouse_gap_buttonClick(Sender: TObject);
    procedure no_flare_buttonClick(Sender: TObject);
    procedure unadjusted_settings_buttonClick(Sender: TObject);
    procedure lengthen_buttonClick(Sender: TObject);
    procedure shorten_buttonClick(Sender: TObject);
    procedure retain_diffs_on_make_checkboxClick(Sender: TObject);
    procedure retain_diffs_on_mint_checkboxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  check_diffs_form: Tcheck_diffs_form;

  show_diffs_fs: boolean = False;       // True = show dims in full-size inches.

  current_diffed_len: double = 0;
  // all in model mm... (set in procedure update_check_diff_panels)
  current_diffed_fl_len: double = 0;
  current_diffed_end_gap: double = 0;

procedure diffs_length_mouse_action;
procedure diffs_flare_mouse_action;
procedure diffs_gap_mouse_action;

//______________________________________________________________________________

implementation

uses pad_unit, math_unit, control_room, help_sheet, colour_unit, alert_unit,
  entry_sheet, template, mark_unit;

{$R *.lfm}

const
  check_diffs_help_str: string = 'php/601        `0Check Rail and Wing Rail Adjustments`9' +
    '||Each end of a check rail is adjusted separately.' +
    '||Please click one of the labels showing on the rail ends to select that rail for adjustment.'
    + '||green_panel_begintree.gif the labels are:||MS1  MS2  MS3  TS1  TS2  TS3  MS4  DS4green_panel_end'
    + '||If the `0retain on new mint`1 box is ticked, any adjustments will be preserved when creating a new mint template. (`0template > new template (quick set)...`1 menu item, or `0NEW`z tool-button.)' + '||If the `0retain on tools: make`1 box is ticked, any adjustments will be preserved when creating a new template using the `0tools > make ---`1 menu items.' + ' This is the same setting as the `0tools > make tools: options > retain adjusted check rails`1 menu option.' + '||Sorry, no further help notes here yet. Please click the link above for more information and diagrams.' + '||green_panel_begintree.gif continuous check rails:' + '||It is not possible for a plain track template to have check rails. However, a length of plain track with a continuous check rail on one side (for use on sharp curves), or both sides (for use in roadways and level crossings),' + ' can be created by modifying a turnout template.' + '||First add some approach track (`0F3`2 mouse action) of suitable length.' + '||Then increase the MS1 or TS1 check rail working length sufficiently to extend the check rail(s) back into the approach track.' + '||Then put the fixing peg at the toe joint (`0CTRL-1`2) and click `0DO > SNAP TO PEG`1 menu item.' + '||To create a full length of continuous check rail with flared ends at both ends, peg two such templates together at their `0CTRL-1`2 positions.' + '||Or tick the `0retain on tools: make`1 box and then click the `0TOOLS > MAKE MIRROR ON PEG`1 menu item.' + '||To swap a single check rail to the opposite side, click the `0TEMPLATE > INVERT HANDING`1 menu item (or `0CTRL-X`2).' + '||Remember that despite appearances these are <U>turnout</U> templates, so to adjust their length use the `0F3`2 `0approach length`3 mouse action (not `0F4`2).' + 'green_panel_end';

var
  window_scaling: boolean = False;
// flag - otherwise ScrollInView on resize prevents form rescaling properly.


{

                          with ccd do begin         // check-rail diffs (mouse modifiers)
                            end_diff_mw.len_diff:=0;
                            end_diff_mw.flr_diff:=0;
                            end_diff_mw.gap_diff:=0;
                            end_diff_mw.add_diff:=False;

                            end_diff_me.len_diff:=0;
                            end_diff_me.flr_diff:=0;
                            end_diff_me.gap_diff:=0;
                            end_diff_me.add_diff:=False;

                            end_diff_mr.len_diff:=0;
                            end_diff_mr.flr_diff:=0;
                            end_diff_mr.gap_diff:=0;
                            end_diff_mr.add_diff:=False;

                            end_diff_tw.len_diff:=0;
                            end_diff_tw.flr_diff:=0;
                            end_diff_tw.gap_diff:=0;
                            end_diff_tw.add_diff:=False;

                            end_diff_te.len_diff:=0;
                            end_diff_te.flr_diff:=0;
                            end_diff_te.gap_diff:=0;
                            end_diff_te.add_diff:=False;

                            end_diff_tr.len_diff:=0;
                            end_diff_tr.flr_diff:=0;
                            end_diff_tr.gap_diff:=0;
                            end_diff_tr.add_diff:=False;

                            end_diff_mk.len_diff:=0;
                            end_diff_mk.flr_diff:=0;
                            end_diff_mk.gap_diff:=0;
                            end_diff_mk.add_diff:=False;

                            end_diff_tk.len_diff:=0;
                            end_diff_tk.flr_diff:=0;
                            end_diff_tk.gap_diff:=0;
                            end_diff_tk.add_diff:=False;

                          end;


}

//______________________________________________________________________________

procedure Tcheck_diffs_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(check_diffs_form);

  AutoScroll := True;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  cancel_adjusts(False);
  show_and_redraw(True, False);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.FormResize(Sender: TObject);

begin
  if (Showing = True) and (initdone_flag = True) and (window_scaling = False) then
    ScrollInView(hide_panel);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.FormShow(Sender: TObject);

begin
  current_diff_code := eMC_0_Ignore;
  show_and_redraw(True, False);   // in case hidden (when idle, no rollback).
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

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
//______________________________________________________________________________

procedure Tcheck_diffs_form.hide_buttonClick(Sender: TObject);

begin
  Close;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.help_buttonClick(Sender: TObject);

begin
  cancel_adjusts(False);
  help(0, check_diffs_help_str, '');
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  check  rails  dialog', Color);
  Show;             // ensure form doesn't get hidden.
  BringToFront;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.reset_all_buttonClick(Sender: TObject);

begin
  if alert(7, '    restore  all  adjusted  check  rails',
    'You are about to restore any adjusted check rails on this template to their normal size and position.'
    + '||This will delete all existing check-rail adjustment data for this template.',
    '', '', '', '', 'cancel  restore   -   no  changes', 'O K', 0) = 5 then
    EXIT;
  Show;
  BringToFront;
  cancel_adjusts(False);
  clear_check_diffs;
  show_and_redraw(True, True);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.mm_radio_buttonClick(Sender: TObject);

begin
  show_diffs_fs := False;
  show_and_redraw(True, False);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.inches_radio_buttonClick(Sender: TObject);

begin
  show_diffs_fs := True;
  show_and_redraw(True, False);
end;
//______________________________________________________________________________

function get_checkrail_end_str: string;

var
  num_str: string;

begin
  num_str := '';  // init

  case current_diff_code of

    eMC_501_MSWorkingEnd:
      num_str := 'MS1';
    eMC_502_MSExtensionEnd:
      num_str := 'MS2';
    eMC_503_MSWingRail:
      num_str := 'MS3';
    eMC_504_TSWorkingEnd:
      if half_diamond = True then
        num_str := 'DS1'
      else
        num_str := 'TS1';
    eMC_505_TSExtensionEnd:
      if half_diamond = True then
        num_str := 'DS2'
      else
        num_str := 'TS2';
    eMC_506_TSWingRail:
      if half_diamond = True then
        num_str := 'DS3'
      else
        num_str := 'TS3';
    eMC_507_MSKCheckRail:
      if half_diamond = True then
        num_str := 'MS4';
    eMC_508_DSWingRail:
      if half_diamond = True then
        num_str := 'DS4';

  end;//case

  Result := num_str;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.data_buttonClick(Sender: TObject);

const
  mod_str: string =
    '||Enter a dimension by which the normal dimensions of this check!!! rail end should be modified.'
    +
    '||To return this check!!! rail end to its normal dimensions, enter 0 (zero).' +
    '||green_panel_begintree.gif  handy hints:' +
    '||It is usually much easier to adjust a check or wing rail by using the buttons and mouse actions, instead of direct entry of data here.' + '||These settings are relative to the normal dimensions of the check!!! rail end, not the present dimensions if it has been adjusted previously.' + '||Instead of these relative dimensions, you can enter the actual length, flare length, or end gap dimension by clicking the relevant read-out panel.' + 'green_panel_end';

var
  i: integer;
  od: Toutdim;

  this_diff: Tcheck_end_diff;

  num_str, help_str, end_str: string;

begin
  cancel_adjusts(False);
  num_str := get_checkrail_end_str;

  {
  case code of

    501: num_str:='MS1';
    502: num_str:='MS2';
    503: num_str:='MS3';
    504: if half_diamond=True then num_str:='DS1' else num_str:='TS1';
    505: if half_diamond=True then num_str:='DS2' else num_str:='TS2';
    506: if half_diamond=True then num_str:='DS3' else num_str:='TS3';
    507: if half_diamond=True then num_str:='MS4';
    508: if half_diamond=True then num_str:='DS4';

  end;//case
}

  if num_str = '' then
    EXIT;

  if (current_diff_code = eMC_503_MSWingRail)
    or (current_diff_code = eMC_506_TSWingRail) then
    end_str := StringReplace(mod_str, 'check!!!', 'wing', [rfReplaceAll, rfIgnoreCase])
  else
    end_str := StringReplace(mod_str, 'check!!!', 'check', [rfReplaceAll, rfIgnoreCase]);

  help_str := '    `0Data for Adjusting Rail End ' + num_str + '`9' + end_str;

  this_diff := get_checkrail_diff(current_diff_code);

  putdim(help_str, 2, 'adjust  ' + num_str + '  full - size  length  by',
    this_diff.len_diff, False, True, False, False);
  // neg ok, no preset, 0 ok, don't terminate on zero.
  putdim(help_str, 2, 'adjust  ' + num_str + '  full - size  flare  length  by',
    this_diff.flr_diff, False, True, False, False);
  // neg ok, no preset, 0 ok, don't terminate on zero.
  i := putdim(help_str, 1, 'adjust  ' + num_str + '  model  end  gap  by',
    this_diff.gap_diff, False, True, False, False);
  // neg ok, no preset, 0 ok, don't terminate on zero.

  if i <> 2 then
    EXIT;

  if getdims('adjust  rail  end  ' + num_str, '', pad_form, i, od) = True then begin
    this_diff.len_diff := od[0];
    this_diff.flr_diff := od[1];
    this_diff.gap_diff := od[2];

    set_checkrail_diff(current_diff_code, this_diff);

    show_and_redraw(True, True);
  end;
end;
//______________________________________________________________________________

procedure flare_buttons_click(new_flare_type: byte);   // override the flare type

var
  this_diff: Tcheck_end_diff;

begin
  cancel_adjusts(False);
  if (current_diff_code < eMC_501_MSWorkingEnd)
    or (current_diff_code > eMC_508_DSWingRail) then
    EXIT;

  this_diff := get_checkrail_diff(current_diff_code);

  this_diff.type_diff := new_flare_type;
  //byte; // 0=no type diff   1=change to bent flare    2=change to machined flare

  get_current_diffed_dims;  // get current results, return string not wanted
  if current_diffed_fl_len < minfp then
    this_diff.flr_diff := 0;           // make sure there is some flare length
  if (current_diffed_end_gap - fw) < minfp then
    this_diff.gap_diff := 0;     // and some bend-out

  set_checkrail_diff(current_diff_code, this_diff);

  show_and_redraw(True, True);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.bent_buttonClick(Sender: TObject);

begin
  flare_buttons_click(1);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.machined_buttonClick(Sender: TObject);

begin
  flare_buttons_click(2);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.no_flare_buttonClick(Sender: TObject);

// set flare length and bend-out to zero
var
  this_diff: Tcheck_end_diff;

begin
  cancel_adjusts(False);
  if (current_diff_code < eMC_501_MSWorkingEnd)
    or (current_diff_code > eMC_508_DSWingRail) then
    EXIT;

  this_diff := get_checkrail_diff(current_diff_code);

  get_current_diffed_dims;  // get current results, return string not wanted

  this_diff.flr_diff := this_diff.flr_diff - current_diffed_fl_len / inscale;
  // reduce diff to make flare length zero

  this_diff.gap_diff := this_diff.gap_diff - (current_diffed_end_gap - fw);
  // reduce diff to make bend-out zero

  set_checkrail_diff(current_diff_code, this_diff);

  show_and_redraw(True, True);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.flare_restore_buttonClick(Sender: TObject);

begin
  flare_buttons_click(0);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.length_panelClick(Sender: TObject);

const
  mod_str: string = '||Enter a new length for this check!!! rail end.' +
    '||All check and wing rail lengths a measured from the centre of the ''A'' timber (the timber which carries the nose of the crosing vee). Click the link above for more information and diagrams.' + '||green_panel_begintree.gif  handy hints:' + '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the `0MODEL MM`1 or `0FULL-SIZE INCHES`1 option buttons.' + '||To enter adjustment data relative to the normal length of this check!!! rail end, cancel this and click the `0DATA...`1 button instead.' + '||It is usually much easier to lengthen or shorten a check or wing rail by using the buttons and mouse actions, instead of direct entry of data here.' + 'green_panel_end';

var
  i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;

  this_diff: Tcheck_end_diff;
  end_str, num_str, help_str: string;

  mod_diff_by_mm: double;

begin
  cancel_adjusts(False);
  num_str := get_checkrail_end_str;

  {
  num_str:='';  // init

  case current_diff_code of

    501: num_str:='MS1';
    502: num_str:='MS2';
    503: num_str:='MS3';
    504: if half_diamond=True then num_str:='DS1' else num_str:='TS1';
    505: if half_diamond=True then num_str:='DS2' else num_str:='TS2';
    506: if half_diamond=True then num_str:='DS3' else num_str:='TS3';
    507: if half_diamond=True then num_str:='MS4';
    508: if half_diamond=True then num_str:='DS4';

  end;//case
}


  if num_str = '' then
    EXIT;

  if (current_diff_code = eMC_503_MSWingRail)
    or (current_diff_code = eMC_506_TSWingRail) then
    end_str := StringReplace(mod_str, 'check!!!', 'wing', [rfReplaceAll, rfIgnoreCase])
  else
    end_str := StringReplace(mod_str, 'check!!!', 'check', [rfReplaceAll, rfIgnoreCase]);

  if show_diffs_fs = True then begin
    fs_convert := inscale;
    fs_str := '  ( full  size  inches )  ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := '  ( model  mm )  ';
    fs_code := 1;
  end;

  help_str := 'php/601        `0Set Length of ' + num_str + ' Rail End`9' + end_str;

  this_diff := get_checkrail_diff(current_diff_code);

  i := putdim(help_str, fs_code, num_str + '  end  length' + fs_str, current_diffed_len /
    fs_convert, False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
  if i <> 0 then
    EXIT;

  if getdims(num_str + '  rail  end  length', '', pad_form, i, od) = True then begin
    mod_diff_by_mm := od[0] * fs_convert - current_diffed_len;

    this_diff.len_diff := this_diff.len_diff + mod_diff_by_mm / inscale;
    // len_diff is in fs ins

    set_checkrail_diff(current_diff_code, this_diff);

    show_and_redraw(True, True);
  end;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.flare_panelClick(Sender: TObject);

const
  mod_str: string = '||Enter a new flare length for this check!!! rail end.' +
    '||The flare length is the length of the angled section at the end of the check!!! rail. Click the link above for more information and diagrams.' + '||green_panel_begintree.gif  handy hints:' + '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the `0MODEL MM`1 or `0FULL-SIZE INCHES`1 option buttons.' + '||To enter adjustment data relative to the normal flare length of this check!!! rail end, cancel this and click the `0DATA...`1 button instead.' + '||It is usually much easier to lengthen or shorten a flare length by using the buttons and mouse actions, instead of direct entry of data here.' + 'green_panel_end';

var
  i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;

  this_diff: Tcheck_end_diff;
  end_str, num_str, help_str: string;

  mod_diff_by_mm: double;

begin
  cancel_adjusts(False);
  num_str := get_checkrail_end_str;

  {

  num_str:='';  // init

  case current_diff_code of

    501: num_str:='MS1';
    502: num_str:='MS2';
    503: num_str:='MS3';
    504: if half_diamond=True then num_str:='DS1' else num_str:='TS1';
    505: if half_diamond=True then num_str:='DS2' else num_str:='TS2';
    506: if half_diamond=True then num_str:='DS3' else num_str:='TS3';
    507: if half_diamond=True then num_str:='MS4';
    508: if half_diamond=True then num_str:='DS4';

  end;//case
}


  if num_str = '' then
    EXIT;

  if (current_diff_code = eMC_503_MSWingRail)
    or (current_diff_code = eMC_506_TSWingRail) then
    end_str := StringReplace(mod_str, 'check!!!', 'wing', [rfReplaceAll, rfIgnoreCase])
  else
    end_str := StringReplace(mod_str, 'check!!!', 'check', [rfReplaceAll, rfIgnoreCase]);

  if show_diffs_fs = True then begin
    fs_convert := inscale;
    fs_str := '  ( full  size  inches )  ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := '  ( model  mm )  ';
    fs_code := 1;
  end;

  help_str := 'php/601        `0Set Flare Length of ' + num_str + ' Rail End`9' + end_str;

  this_diff := get_checkrail_diff(current_diff_code);

  i := putdim(help_str, fs_code, num_str + '  flare  length' + fs_str, current_diffed_fl_len /
    fs_convert, False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
  if i <> 0 then
    EXIT;

  if getdims(num_str + '  rail  flare  length', '', pad_form, i, od) = True then begin
    mod_diff_by_mm := od[0] * fs_convert - current_diffed_fl_len;

    this_diff.flr_diff := this_diff.flr_diff + mod_diff_by_mm / inscale;
    // flr_diff is in fs ins

    set_checkrail_diff(current_diff_code, this_diff);

    show_and_redraw(True, True);
  end;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.gap_panelClick(Sender: TObject);
const
  mod_str: string = '||Enter a new end gap dimension for this check!!! rail end.'
    + '||The end gap is the widened opening at each end of a check rail or at the far end of a wing rail. Click the link above for more information and diagrams.' + '||For most UK prototypes this dimension is 3.5" full-size.' + '||For model use it needs to be increased from the scale size where wider than scale flangeway gaps are used.' + '||green_panel_begintree.gif  handy hints:' + '||You can change between using model sizes (in mm) and full-size prototype dimensions (in inches) by clicking the `0MODEL MM`1 or `0FULL-SIZE INCHES`1 option buttons.' + '||To enter adjustment data relative to the normal end gap for this check!!! rail end, cancel this and click the `0DATA...`1 button instead.' + 'green_panel_end';

var
  i: integer;
  od: Toutdim;
  fs_convert: double;
  fs_str: string;
  fs_code: integer;

  this_diff: Tcheck_end_diff;
  end_str, num_str, help_str: string;

  mod_diff_by_mm: double;

begin
  cancel_adjusts(False);
  num_str := get_checkrail_end_str;

  {
  num_str:='';  // init

  case current_diff_code of

    501: num_str:='MS1';
    502: num_str:='MS2';
    503: num_str:='MS3';
    504: if half_diamond=True then num_str:='DS1' else num_str:='TS1';
    505: if half_diamond=True then num_str:='DS2' else num_str:='TS2';
    506: if half_diamond=True then num_str:='DS3' else num_str:='TS3';
    507: if half_diamond=True then num_str:='MS4';
    508: if half_diamond=True then num_str:='DS4';

  end;//case
}


  if num_str = '' then
    EXIT;

  if (current_diff_code = eMC_503_MSWingRail)
    or (current_diff_code = eMC_506_TSWingRail) then
    end_str := StringReplace(mod_str, 'check!!!', 'wing', [rfReplaceAll, rfIgnoreCase])
  else
    end_str := StringReplace(mod_str, 'check!!!', 'check', [rfReplaceAll, rfIgnoreCase]);

  if show_diffs_fs = True then begin
    fs_convert := inscale;
    fs_str := '  ( full  size  inches )  ';
    fs_code := 2;
  end
  else begin
    fs_convert := 1;
    fs_str := '  ( model  mm )  ';
    fs_code := 1;
  end;

  help_str := 'php/601        `0Set End Gap for ' + num_str + ' Rail End`9' + end_str;

  this_diff := get_checkrail_diff(current_diff_code);

  i := putdim(help_str, fs_code, num_str + '  end  gap' + fs_str, current_diffed_end_gap /
    fs_convert, False, True, False, False); // neg ok, no preset, 0 ok, don't terminate on zero.
  if i <> 0 then
    EXIT;

  if getdims(num_str + '  rail  end  gap', '', pad_form, i, od) = True then begin
    mod_diff_by_mm := od[0] * fs_convert - current_diffed_end_gap;

    this_diff.gap_diff := this_diff.gap_diff + mod_diff_by_mm;  // gap_diff is in model mm

    set_checkrail_diff(current_diff_code, this_diff);

    show_and_redraw(True, True);
  end;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.reset_buttonClick(Sender: TObject);

var
  this_diff: Tcheck_end_diff;

begin
  cancel_adjusts(False);
  this_diff := get_checkrail_diff(current_diff_code);

  this_diff.len_diff := 0;
  this_diff.flr_diff := 0;
  this_diff.gap_diff := 0;
  this_diff.type_diff := 0; // byte

  set_checkrail_diff(current_diff_code, this_diff);

  show_and_redraw(True, True);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.omit_buttonClick(Sender: TObject);

begin
  cancel_adjusts(False);
  pad_form.omit_rails_joints_menu_entry.Click;
end;
//______________________________________________________________________________

procedure diffs_length_mouse_action;

var
  end_str: string;

begin
  cancel_adjusts(True);

  end_str := get_checkrail_end_str;

  if end_str = '' then
    EXIT;

  mouse_diff := get_checkrail_diff(current_diff_code);

  mouse_action_selected('    adjust  ' + end_str + '  length ...', 'adjust  ' + end_str +
    '  length', 'adjust  by : ' + captext(mouse_diff.len_diff * inscale) + ' mm');

  check_diffs_len_mod := 1;
end;
//______________________________________________________________________________

procedure diffs_flare_mouse_action;

var
  end_str: string;

begin
  cancel_adjusts(True);

  end_str := get_checkrail_end_str;

  if end_str = '' then
    EXIT;

  mouse_diff := get_checkrail_diff(current_diff_code);

  mouse_action_selected('    adjust  ' + end_str + '  flare  length ...', 'adjust  ' +
    end_str + '  flare  length', 'adjust  by : ' + captext(mouse_diff.flr_diff * inscale) + ' mm');

  check_diffs_flare_mod := 1;
end;
//______________________________________________________________________________

procedure diffs_gap_mouse_action;

var
  end_str: string;

begin
  cancel_adjusts(True);

  end_str := get_checkrail_end_str;

  if end_str = '' then
    EXIT;

  mouse_diff := get_checkrail_diff(current_diff_code);

  mouse_action_selected('    adjust  ' + end_str + '  end  gap ...', 'adjust  ' +
    end_str + '  end  gap', 'adjust  by : ' + captext(mouse_diff.gap_diff) + ' mm');

  check_diffs_gap_mod := 1;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.mouse_length_buttonClick(Sender: TObject);

begin
  diffs_length_mouse_action;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.mouse_flare_buttonClick(Sender: TObject);

begin
  diffs_flare_mouse_action;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.mouse_gap_buttonClick(Sender: TObject);

begin
  diffs_gap_mouse_action;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.unadjusted_settings_buttonClick(Sender: TObject);

begin
  cancel_adjusts(False);
  pad_form.wing_check_rails_menu_entry.Click;
  if half_diamond = True then
    pad_form.k_crossing_check_rails_menu_entry.Click;
  pad_form.checks_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.lengthen_buttonClick(Sender: TObject);

var
  this_diff: Tcheck_end_diff;

begin
  cancel_adjusts(False);
  this_diff := get_checkrail_diff(current_diff_code);

  this_diff.len_diff := this_diff.len_diff + 30;   // lengthen by 30" scale

  set_checkrail_diff(current_diff_code, this_diff);

  show_and_redraw(True, True);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.shorten_buttonClick(Sender: TObject);

var
  this_diff: Tcheck_end_diff;
  min_diff: double;

begin
  cancel_adjusts(False);

  this_diff := get_checkrail_diff(current_diff_code);

  min_diff := (current_diffed_fl_len - current_diffed_len) / inscale + this_diff.len_diff;
  // minimum

  this_diff.len_diff := this_diff.len_diff - 30;   // shorten by 30" scale

  if this_diff.len_diff < min_diff then
    this_diff.len_diff := min_diff;

  set_checkrail_diff(current_diff_code, this_diff);

  show_and_redraw(True, True);
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.retain_diffs_on_make_checkboxClick(Sender: TObject);

begin
  retain_diffs_on_make := retain_diffs_on_make_checkbox.Checked;
end;
//______________________________________________________________________________

procedure Tcheck_diffs_form.retain_diffs_on_mint_checkboxClick(Sender: TObject);

begin
  retain_diffs_on_mint := retain_diffs_on_mint_checkbox.Checked;
end;
//______________________________________________________________________________

end.
