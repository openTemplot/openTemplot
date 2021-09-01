
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

unit trackbed_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  Ttrackbed_form = class(TForm)
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    hide_panel: TPanel;
    hide_button: TButton;
    trackbed_pagecontrol: TPageControl;
    ms_trackbed_tabsheet: TTabSheet;
    ms_panel: TPanel;
    ms_mouse_groupbox: TGroupBox;
    mouse_ms_length_button: TButton;
    mouse_ms_start_button: TButton;
    ms_position_button: TButton;
    ms_trackbed_checkbox: TCheckBox;
    ts_trackbed_tabsheet: TTabSheet;
    ts_panel: TPanel;
    ts_mouse_groupbox: TGroupBox;
    mouse_ts_length_button: TButton;
    mouse_ts_start_button: TButton;
    ts_position_button: TButton;
    ts_trackbed_checkbox: TCheckBox;
    ms_cut_radio_button: TRadioButton;
    ms_cess_radio_button: TRadioButton;
    ts_cut_radio_button: TRadioButton;
    ts_cess_radio_button: TRadioButton;
    ms_cess_width_button: TButton;
    ts_cess_width_button: TButton;
    trackbed_widths_button: TButton;
    modify_group_button: TButton;
    ms_help_button: TButton;
    ts_help_button: TButton;
    help_button: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ts_trackbed_checkboxClick(Sender: TObject);
    procedure trackbed_widths_buttonClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure ms_cess_width_buttonClick(Sender: TObject);
    procedure ts_cess_width_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ms_cess_radio_buttonClick(Sender: TObject);
    procedure ms_cut_radio_buttonClick(Sender: TObject);
    procedure ts_cess_radio_buttonClick(Sender: TObject);
    procedure ts_cut_radio_buttonClick(Sender: TObject);
    procedure modify_group_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure ms_help_buttonClick(Sender: TObject);
    procedure ts_help_buttonClick(Sender: TObject);
    procedure ms_position_buttonClick(Sender: TObject);
    procedure ts_position_buttonClick(Sender: TObject);
    procedure hide_buttonClick(Sender: TObject);
    procedure mouse_ts_start_buttonClick(Sender: TObject);
    procedure mouse_ts_length_buttonClick(Sender: TObject);
    procedure mouse_ms_start_buttonClick(Sender: TObject);
    procedure mouse_ms_length_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  trackbed_form: Ttrackbed_form;

procedure trackbed_buttons;   // update the trackbed form
//procedure cancel_trackbeds;   // called by make tools

implementation

uses control_room, pad_unit, help_sheet, colour_unit, math_unit, entry_sheet, alert_unit,
  keep_select;

{$R *.lfm}

const
  trackbed_help_str: string = 'php/660      `0trackbed  edges`9' +
    '||If you are using open-top baseboard construction, this function draws cutting lines for your trackbed material on each side of the track template.' + '||Alternatively you can use this function to represent the ballast edge and cess width for prototype track.' + '||The trackbed edges can be drawn for one or both sides of a template to allow for single or double track. It is possible to have a cutting line on one side of the template, and ballast and cess lines on the other side.' + '||Click the `0real > trackbed edges...`1  menu item or the `0geometry > trackbed edges...`1 menu item on the trackpad to add trackbed edges to the control template. The trackbed settings dialog will then appear.' + '||Click the `0trackbed widths...`1 button to set the width between the trackbed edge lines in mm. Each side of the template is set separately.' + ' The pre-set dimensions are equivalent to a full-size trackbed width of 15ft at your current scale, i.e. 7ft-6in (90 inches) on each side of the track centre-line.' + '||TS is the turnout side of the template, MS is the main side. Select the relevant tab for the settings for each side.' + '||Tick the box to `0add trackbed edge`1 on that side. The full controls will then appear.' + '||For use as a cutting line, the trackbed edges are drawn in a style similar to the rails but reduced to half the current rail width. This makes a bold line for cutting the trackbed material.' + '||To represent instead the ballast and cess, click the `0ballast + cess`1 option button. To set the width of the cess area beyond the edge of the ballast, click the `0cess width...`1 button.' + ' This dimension is set in full-size prototype inches. The pre-set dimension is 2ft-3in (27 inches) which is typical of UK practice, although this dimension can vary widely.' + '||Trackbed edge lines are intended as a guide only and are likely to conflict through pointwork and junctions. To avoid this the position and length of the trackbed edge can be adjusted as required.' + ' Click the `0position and length...`1 button to set the starting position for the trackbed edge from the `0CTRL-0`2 template datum, and the length of the trackbed edge beyond the starting position.' + '||rp.gif The dimensions apply along the track centre-line. The actual trackbed edge length will be shorter along the inside of a curve, and longer along the outside.' + '||In most cases it will be more convenient to set the start position and length by mouse action, by clicking the `0mouse actions :`1 buttons.' + '||To change the trackbed settings on all or part of your track plan in one go, click the `0modify group to match`1 button.' + ' The selected group of templates will then be changed to match the current trackbed width settings on the control template. The starting position and length settings must then be adjusted separately on each template if required.' + '||Setting a trackbed width of 0 (zero) will create a bold track centre-line, for track planning purposes or to represent 3-rail track. Use both TS and MS trackbed edges together for this.' + ' But see also the `0output > line thickness >`1 menu items to change the thickness of the normal track centre-lines on the output.' + '||Trackbed edges can be exported to a DXF file and re-imported into your Background Shapes as part of your baseboard design.' + ' On the DXF dialog, select any colour for `0PLATFORM AND TRACKBED EDGES`1, and `0NONE`1 as the colour for all other features.' + '||rp.gif If you are currently using the `0adjacent rails`3 option as a design guide, adding one or both trackbed edges will cancel the adjacent rails.' + ' It is not possible for Templot0 to show adjacent rails and trackbed edges on the same template.' + ' The adjacent rails can be restored by clicking the `0geometry > adjacent options > adjacent rails as guide lines`1 menu option.';


var
  form_showing: boolean = False;

//______________________________________________________________________________

procedure Ttrackbed_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(trackbed_form);

  AutoScroll := False;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.trackbed_widths_buttonClick(Sender: TObject);

// both width buttons come here

const
  trackbed_width_str: string = 'php/660      `0trackbed  half-widths`9' +
    '||Enter a dimension in mm for the distance from the track centre-line to the trackbed edge line or to the extent of the ballast.'
    + '||Each side of the track centre-line is entered separately so that the trackbed half-widths can differ if needed.'
    + '||MS is the main-side of the track. TS is the turnout-side.' +
    '||The pre-set dimensions are equivalent to a full-size trackbed width of 15ft at your current scale, i.e. 7ft-6in on each side of the track centre-line.' + '||Click the `0more general information`1 button below for more information about trackbed edges.';

var
  n: integer;
  od: Toutdim;
  ms, ts: double;

begin
  ms := trackbed_ms_width_ins * inscale;  // stored as inches full-size.
  ts := trackbed_ts_width_ins * inscale;

  putdim(trackbed_width_str, 1, 'track  centre  to  turnout-side  TS  trackbed  edge', ts,
    True, False, False, False);  // no neg, preset ok, zero ok, don't terminate on zero.
  n := putdim(trackbed_width_str, 1, 'track  centre  to  main-side  MS  trackbed  edge',
    ms, True, False, False, False);     // no neg, preset ok, zero ok, don't terminate on zero.

  if n <> 1 then
    EXIT;
  if getdims('trackbed  half - widths', trackbed_help_str, pad_form, n, od) = True then begin
    ts := od[0];
    if ts = def_req then
      ts := 90 * inscale;  // default 7ft-6in
    trackbed_ts_width_ins := ts / inscale;  // stored as full-size inches.

    ms := od[1];
    if ms = def_req then
      ms := 90 * inscale;  // default 7ft-6in
    trackbed_ms_width_ins := ms / inscale;  // stored as full-size inches.
  end;

  redraw_pad(True, True);
end;
//______________________________________________________________________________

// common to both checkbox clicks...

procedure Ttrackbed_form.ts_trackbed_checkboxClick(Sender: TObject);

var
  save_ts, save_ms: boolean;

begin

  if form_showing = False then
    EXIT;

  save_ts := draw_ts_trackbed_edge;
  save_ms := draw_ms_trackbed_edge;

  with trackbed_form do begin

    draw_ts_trackbed_edge := ts_trackbed_checkbox.Checked;
    draw_ms_trackbed_edge := ms_trackbed_checkbox.Checked;


    if ts_trackbed_checkbox.Checked = True then
      ts_trackbed_checkbox.Color := ts_panel.Color
    else
      ts_trackbed_checkbox.Color := Color;

    if ms_trackbed_checkbox.Checked = True then
      ms_trackbed_checkbox.Color := ms_panel.Color
    else
      ms_trackbed_checkbox.Color := Color;

  end;//with form

  if (draw_ts_trackbed_edge = True) or (draw_ms_trackbed_edge = True) then
    pad_form.adjacent_trackbed_platforms_menu_entry.Click;  // switch adjacent tracks off if necessary

  if (save_ts = False) and (draw_ts_trackbed_edge = True)
  // just added a trackbed edge, set default lengths (leave widths).
  then begin
    trackbed_ts_start_mm := 0;
    trackbed_ts_length_mm := def_req;
    // def_req converted in strails(); to full template length.
  end;

  if (save_ms = False) and (draw_ms_trackbed_edge = True)
  // just added a  trackbed edge, set default lengths (leave widths).
  then begin
    trackbed_ms_start_mm := 0;
    trackbed_ms_length_mm := def_req;
    // def_req converted in strails(); to full template length.
  end;

  do_railedges;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure trackbed_buttons;   // update the trackbed form

// OnIdle comes here also

begin
  form_showing := False;   // needed to prevent these changes generating a click on the boxes.

  with trackbed_form do begin
    ts_trackbed_checkbox.Checked := draw_ts_trackbed_edge;
    ms_trackbed_checkbox.Checked := draw_ms_trackbed_edge;

    if draw_ts_trackbed_edge = True then
      ts_panel.Visible := True
    else
      ts_panel.Visible := False;

    if draw_ms_trackbed_edge = True then
      ms_panel.Visible := True
    else
      ms_panel.Visible := False;

    ms_cess_radio_button.Checked := draw_ms_trackbed_cess_edge;
    ms_cut_radio_button.Checked := not draw_ms_trackbed_cess_edge;

    ts_cess_radio_button.Checked := draw_ts_trackbed_cess_edge;
    ts_cut_radio_button.Checked := not draw_ts_trackbed_cess_edge;

    if draw_ts_trackbed_edge = True then
      ts_trackbed_checkbox.Color := ts_panel.Color
    else
      ts_trackbed_checkbox.Color := Color;

    if draw_ms_trackbed_edge = True then
      ms_trackbed_checkbox.Color := ms_panel.Color
    else
      ms_trackbed_checkbox.Color := Color;

  end;//with

  form_showing := True;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag         // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                          // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                          // scale the form contents up.

  with hide_panel do begin
    trackbed_form.ClientWidth := Left + Width;
    trackbed_form.ClientHeight := Top + Height;
  end;//with

  size_updown.Tag := size_updown.Position;   // and save for the next click.
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ms_cess_width_buttonClick(Sender: TObject);

const
  ms_cess_help_str: string = 'php/660    `0MS  cess  width`9' +
    '||Enter a dimension in full-size prototype INCHES for the width of the cess area beyond the main-side (MS) trackbed edge (or beyond the extent of the ballast).' + '||A typical dimension for UK standard-gauge practice is 27 inches, although this can vary widely.' + '||rp.gif The cess width is set in full-size prototype inches. The trackbed width is set in model mm. This allows for the option to use the trackbed width as a cutting line for open-top baseboard construction.' + '||For more information about the trackbed options, click the button below.';

var
  n: integer;
  od: Toutdim;

begin
  n := putdim(ms_cess_help_str, 2, 'cess  width  on  MS  side  ( full - size  inches )',
    cess_ms_width_ins, True, True, False, False);
  // no negative, no preset, zero ok, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('MS  cess  width', trackbed_help_str, pad_form, n, od) = True then
    cess_ms_width_ins := od[0]
  else
    EXIT;

  ms_cess_radio_button.Checked := True;
  draw_ms_trackbed_cess_edge := True;

  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ts_cess_width_buttonClick(Sender: TObject);

const
  ts_cess_help_str: string = 'php/660    `0TS  cess  width`9' +
    '||Enter a dimension in full-size prototype INCHES for the width of the cess area beyond the turnout-side (TS) trackbed edge (or beyond the extent of the ballast).' + '||A typical dimension for UK standard-gauge practice is 27 inches, although this can vary widely.' + '||rp.gif The cess width is set in full-size prototype inches. The trackbed width is set in model mm. This allows for the option to use the trackbed width as a cutting line for open-top baseboard construction.' + '||For more information about the trackbed options, click the button below.';

var
  n: integer;
  od: Toutdim;

begin
  n := putdim(ts_cess_help_str, 2, 'cess  width  on  TS  side  ( full - size  inches )',
    cess_ts_width_ins, True, True, False, False);
  // no negative, no preset, zero ok, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('TS  cess  width', trackbed_help_str, pad_form, n, od) = True then
    cess_ts_width_ins := od[0]
  else
    EXIT;

  ts_cess_radio_button.Checked := True;
  draw_ts_trackbed_cess_edge := True;

  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.FormShow(Sender: TObject);

begin
  trackbed_buttons;

  if (draw_ts_trackbed_edge = True) or (draw_ms_trackbed_edge = True) then
    pad_form.adjacent_trackbed_platforms_menu_entry.Click;  // switch adjacent tracks off if necessary
end;
//______________________________________________________________________________

procedure Ttrackbed_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  cancel_adjusts(False);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ms_cess_radio_buttonClick(Sender: TObject);

begin
  draw_ms_trackbed_cess_edge := True;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ms_cut_radio_buttonClick(Sender: TObject);

begin
  draw_ms_trackbed_cess_edge := False;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ts_cess_radio_buttonClick(Sender: TObject);

begin
  draw_ts_trackbed_cess_edge := True;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ts_cut_radio_buttonClick(Sender: TObject);

begin
  draw_ts_trackbed_cess_edge := False;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.modify_group_buttonClick(Sender: TObject);

var
  his_option: boolean;

begin
  if any_bgnd = 0 then begin
    alert_no_bgnd;
    EXIT;
  end;

  if any_selected = 0 then begin
    if alert_no_group = True    // alert him, and does he want all?
    then
      EXIT;
  end;

  if alert(7, 'php/205    modify  group  to  match  trackbed  edge  widths',
    'This function will modify the trackbed edge width settings on the selected group of background templates to match the current settings on the control template.' + '||The trackbed edge starting position and length settings must then be adjusted separately on each template if required.', '', '', '', '', 'cancel', 'modify  group  to  match  trackbed  edges', 0) = 5 then
    EXIT;

  his_option := keep_form.trackbed_edges_as_stored_menu_entry.Checked;
  keep_form.trackbed_edges_as_control_menu_entry.Checked := True;             // radio item.

  rebuild_group(True, True);

  if his_option = True then
    keep_form.trackbed_edges_as_stored_menu_entry.Checked := True;   // radio item.

  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.help_buttonClick(Sender: TObject);

begin
  help(0, trackbed_help_str, '');
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ms_help_buttonClick(Sender: TObject);

begin
  if help(0, 'php/660      `0MS trackbed edge`9||The settings on this tab relate to the trackbed edge on the MS (main-side) of the template.||Click the other tab for the TS (turnout-side) settings.||Click the button below for a full explanation of the controls.', 'more  help  information') = 1 then
    help_button.Click;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ts_help_buttonClick(Sender: TObject);

begin
  if help(0, 'php/660      `0TS trackbed edge`9||The settings on this tab relate to the trackbed edge on the TS (turnout-side) of the template.||Click the other tab for the MS (main-side) settings.||Click the button below for a full explanation of the controls.', 'more  help  information') = 1 then
    help_button.Click;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ms_position_buttonClick(Sender: TObject);

const
  ms_start_str: string = 'Enter the start position for the main-side (MS) trackbed edge in model mm.';
  ms_length_str: string = 'Enter the length of main-side (MS) trackbed edge in model mm.';

var
  n: integer;
  od: Toutdim;

begin
  putdim(ms_start_str, 1, 'MS  main-side  trackbed  edge  starts  at', trackbed_ms_start_mm,
    True, False, False, False);  // no neg, preset ok, zero ok, don't terminate on zero.
  n := putdim(ms_length_str, 1, 'MS  main-side  trackbed  edge  length',
    trackbed_ms_length_mm, True, False, True, False);
  // no neg, preset ok, no zero, don't terminate on zero.

  if n <> 1 then
    EXIT;

  if getdims('MS  main - side  trackbed  edge  position', trackbed_help_str, trackbed_form, n, od) =
    True then begin
    trackbed_ms_start_mm := od[0];
    if trackbed_ms_start_mm = def_req then
      trackbed_ms_start_mm := 0;

    trackbed_ms_length_mm := od[1];
    // return def_req, converted in strails(); to full template length.
  end;

  do_railedges;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.ts_position_buttonClick(Sender: TObject);

const
  ts_start_str: string = 'Enter the start position for the turnout-side (TS) trackbed edge in model mm.';
  ts_length_str: string = 'Enter the length of turnout-side (TS) trackbed edge in model mm.';

var
  n: integer;
  od: Toutdim;

begin
  putdim(ts_start_str, 1, 'TS  turnout-side  trackbed  edge  starts  at', trackbed_ts_start_mm,
    True, False, False, False);  // no neg, preset ok, zero ok, don't terminate on zero.
  n := putdim(ts_length_str, 1, 'TS  turnout-side  trackbed  edge  length',
    trackbed_ts_length_mm, True, False, True, False);
  // no neg, preset ok, no zero, don't terminate on zero.

  if n <> 1 then
    EXIT;

  if getdims('TS  turnout - side  trackbed  edge  position', trackbed_help_str, trackbed_form, n, od) =
    True then begin
    trackbed_ts_start_mm := od[0];
    if trackbed_ts_start_mm = def_req then
      trackbed_ts_start_mm := 0;

    trackbed_ts_length_mm := od[1];
    // return def_req, converted in strails(); to full template length.
  end;

  do_railedges;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Ttrackbed_form.hide_buttonClick(Sender: TObject);

begin
  Hide;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.mouse_ts_start_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  turnout - side  trackbed  edge  start ...',
    'turnout - side  trackbed  edge  start', captext(trackbed_ts_start_mm) + ' mm');
  edge_ts_start_mod := 1;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.mouse_ts_length_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  // using mouse action cancels default full template length ...
  // turnoutx may have been reduced since the length was set ...

  if trackbed_ts_length_mm = def_req then
    trackbed_ts_length_mm := turnoutx - trackbed_ts_start_mm;

  if turnoutx < (trackbed_ts_start_mm + trackbed_ts_length_mm) then
    trackbed_ts_length_mm := turnoutx - trackbed_ts_start_mm;

  if trackbed_ts_length_mm < 0 then
    trackbed_ts_length_mm := 0;

  mouse_action_selected('    adjust  turnout - side  trackbed  edge  length ...',
    'turnout - side  trackbed  edge  length', captext(trackbed_ts_length_mm) + ' mm');
  edge_ts_length_mod := 1;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.mouse_ms_start_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  main - side  trackbed  edge  start ...',
    'main - side  trackbed  edge  start', captext(trackbed_ms_start_mm) + ' mm');
  edge_ms_start_mod := 1;
end;
//______________________________________________________________________________

procedure Ttrackbed_form.mouse_ms_length_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  // using mouse action cancels default full template length ...
  // turnoutx may have been reduced since the length was set ...

  if trackbed_ms_length_mm = def_req then
    trackbed_ms_length_mm := turnoutx - trackbed_ms_start_mm;

  if turnoutx < (trackbed_ms_start_mm + trackbed_ms_length_mm) then
    trackbed_ms_length_mm := turnoutx - trackbed_ms_start_mm;

  if trackbed_ms_length_mm < 0 then
    trackbed_ms_length_mm := 0;

  mouse_action_selected('    adjust  main - side  trackbed  edge  length ...',
    'main - side  trackbed  edge  length', captext(trackbed_ms_length_mm) + ' mm');
  edge_ms_length_mod := 1;
end;
//______________________________________________________________________________


end.
