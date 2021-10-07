
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
unit platform_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Tplatform_form = class(TForm)
    hide_panel: TPanel;
    hide_button: TButton;
    help_shape: TShape;
    help_button: TButton;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    platform_pagecontrol: TPageControl;
    ts_platform_tabsheet: TTabSheet;
    ms_platform_tabsheet: TTabSheet;
    ts_panel: TPanel;
    ts_width_button: TButton;
    ts_mouse_groupbox: TGroupBox;
    mouse_ts_length_button: TButton;
    mouse_ts_start_button: TButton;
    ts_position_button: TButton;
    ts_spacing_button: TButton;
    ts_edge_groupbox: TGroupBox;
    ts_platform_start_edge_checkbox: TCheckBox;
    ts_platform_end_edge_checkbox: TCheckBox;
    ts_platform_rear_edge_checkbox: TCheckBox;
    ts_platform_checkbox: TCheckBox;
    ms_panel: TPanel;
    ms_mouse_groupbox: TGroupBox;
    mouse_ms_length_button: TButton;
    mouse_ms_start_button: TButton;
    ms_width_button: TButton;
    ms_position_button: TButton;
    ms_spacing_button: TButton;
    ms_edge_groupbox: TGroupBox;
    ms_platform_start_edge_checkbox: TCheckBox;
    ms_platform_rear_edge_checkbox: TCheckBox;
    ms_platform_end_edge_checkbox: TCheckBox;
    ms_platform_checkbox: TCheckBox;
    mouse_ts_skew2_button: TButton;
    mouse_ts_skew1_button: TButton;
    ts_mouse_width_groupbox: TGroupBox;
    mouse_ts_width1_button: TButton;
    mouse_ts_width2_button: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mouse_ms_skew2_button: TButton;
    mouse_ms_skew1_button: TButton;
    ms_mouse_width_groupbox: TGroupBox;
    Label4: TLabel;
    mouse_ms_width1_button: TButton;
    mouse_ms_width2_button: TButton;
    procedure ts_platform_checkboxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mouse_ts_start_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure hide_panelClick(Sender: TObject);
    procedure ts_width_buttonClick(Sender: TObject);
    procedure ts_position_buttonClick(Sender: TObject);
    procedure ms_position_buttonClick(Sender: TObject);
    procedure ms_width_buttonClick(Sender: TObject);
    procedure ts_spacing_buttonClick(Sender: TObject);
    procedure ms_spacing_buttonClick(Sender: TObject);
    procedure mouse_ts_length_buttonClick(Sender: TObject);
    procedure mouse_ts_width2_buttonClick(Sender: TObject);
    procedure mouse_ms_start_buttonClick(Sender: TObject);
    procedure mouse_ms_length_buttonClick(Sender: TObject);
    procedure mouse_ms_width2_buttonClick(Sender: TObject);
    procedure colour_patchClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure mouse_ts_width1_buttonClick(Sender: TObject);
    procedure mouse_ms_width1_buttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mouse_ms_skew1_buttonClick(Sender: TObject);
    procedure mouse_ms_skew2_buttonClick(Sender: TObject);
    procedure mouse_ts_skew1_buttonClick(Sender: TObject);
    procedure mouse_ts_skew2_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  platform_form: Tplatform_form;

procedure platform_buttons;   // update the platforms form
procedure cancel_platforms;   // called by make tools


implementation

{$BOOLEVAL ON}

uses
  control_room, pad_unit, help_sheet, colour_unit, math_unit, entry_sheet, alert_unit;


{$R *.lfm}

const
  platform_help_str: string = '      `0Platforms`9' +
    '||TS is the turnout-side of the template. MS is the main-side.' +
    '||Sorry, detailed help notes for platforms are not yet written. Please refer to the <A HREF="go_to_templot_club.85a">Templot&nbsp;Club</A> user forum for help.' + '||rp.gif If you are currently using the `0adjacent rails`3 option as a design guide, clicking the `0real > platforms...`1 menu item will cancel the adjacent rails.' + ' It is not possible for Templot0 to show adjacent rails and platforms on the same template.' + ' The adjacent rails can be restored by clicking the `0geometry > adjacent options > adjacent rails as guide lines`1 menu option.';


var
  form_showing: boolean = False;

//______________________________________________________________________________

procedure cancel_platforms;

begin
  draw_ts_platform := False;
  draw_ms_platform := False;

  do_railedges;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Tplatform_form.ts_platform_checkboxClick(Sender: TObject);

// common to all checkbox clicks...

var
  save_ts, save_ms: boolean;

begin

  if form_showing = False then
    EXIT;

  save_ts := draw_ts_platform;
  save_ms := draw_ms_platform;

  with platform_form do begin

    draw_ts_platform := ts_platform_checkbox.Checked;  // 0.93.a ...
    draw_ts_platform_rear_edge := ts_platform_rear_edge_checkbox.Checked;
    draw_ts_platform_start_edge := ts_platform_start_edge_checkbox.Checked;
    draw_ts_platform_end_edge := ts_platform_end_edge_checkbox.Checked;

    draw_ms_platform := ms_platform_checkbox.Checked;
    draw_ms_platform_rear_edge := ms_platform_rear_edge_checkbox.Checked;
    draw_ms_platform_start_edge := ms_platform_start_edge_checkbox.Checked;
    draw_ms_platform_end_edge := ms_platform_end_edge_checkbox.Checked;


    if ts_platform_checkbox.Checked = True then
      ts_platform_checkbox.Color := ts_panel.Color
    else
      ts_platform_checkbox.Color := Color;

    if ms_platform_checkbox.Checked = True then
      ms_platform_checkbox.Color := ms_panel.Color
    else
      ms_platform_checkbox.Color := Color;

  end;//with form

  if (draw_ts_platform = True) or (draw_ms_platform = True) then
    pad_form.adjacent_trackbed_platforms_menu_entry.Click;  // switch adjacent tracks off if necessary

  if (save_ts = False) and (draw_ts_platform = True)
  // just added a platform, set default lengths (leave widths).
  then begin
    platform_ts_start_mm := 0;
    platform_ts_length_mm := def_req;
    // def_req converted in strails(); to full template length.
  end;

  if (save_ms = False) and (draw_ms_platform = True)
  // just added a platform, set default lengths (leave widths).
  then begin
    platform_ms_start_mm := 0;
    platform_ms_length_mm := def_req;
    // def_req converted in strails(); to full template length.
  end;

  do_railedges;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure platform_buttons;   // update the platforms form

// OnIdle comes here also

begin
  form_showing := False;   // needed to prevent these changes generating a click on the boxes.

  with platform_form do begin
    ts_platform_checkbox.Checked := draw_ts_platform;
    ts_platform_rear_edge_checkbox.Checked := draw_ts_platform_rear_edge;
    ts_platform_start_edge_checkbox.Checked := draw_ts_platform_start_edge;
    ts_platform_end_edge_checkbox.Checked := draw_ts_platform_end_edge;

    ms_platform_checkbox.Checked := draw_ms_platform;
    ms_platform_rear_edge_checkbox.Checked := draw_ms_platform_rear_edge;
    ms_platform_start_edge_checkbox.Checked := draw_ms_platform_start_edge;
    ms_platform_end_edge_checkbox.Checked := draw_ms_platform_end_edge;

    if draw_ts_platform = True then
      ts_panel.Visible := True
    else
      ts_panel.Visible := False;

    if draw_ms_platform = True then
      ms_panel.Visible := True
    else
      ms_panel.Visible := False;


    if draw_ts_platform = True then
      ts_platform_checkbox.Color := ts_panel.Color
    else
      ts_platform_checkbox.Color := Color;

    if draw_ms_platform = True then
      ms_platform_checkbox.Color := ms_panel.Color
    else
      ms_platform_checkbox.Color := Color;

  end;//with

  form_showing := True;
end;
//______________________________________________________________________________

procedure Tplatform_form.FormShow(Sender: TObject);

begin
  platform_buttons;

  if (draw_ts_platform = True) or (draw_ms_platform = True) then
    pad_form.adjacent_trackbed_platforms_menu_entry.Click;  // switch adjacent tracks off if necessary
end;
//______________________________________________________________________________

procedure Tplatform_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(platform_form);

  AutoScroll := False;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ts_start_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  turnout - side  platform  start ...',
    'turnout - side  platform  start', captext(platform_ts_start_mm) + ' mm');
  plat_ts_start_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ts_length_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  // using mouse action cancels default full template length ...
  // turnoutx may have been reduced since the length was set ...

  if platform_ts_length_mm = def_req then
    platform_ts_length_mm := turnoutx - platform_ts_start_mm;

  if turnoutx < (platform_ts_start_mm + platform_ts_length_mm) then
    platform_ts_length_mm := turnoutx - platform_ts_start_mm;

  if platform_ts_length_mm < 0 then
    platform_ts_length_mm := 0;

  mouse_action_selected('    adjust  turnout - side  platform  length ...',
    'turnout - side  platform  length', captext(platform_ts_length_mm) + ' mm');
  plat_ts_length_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ts_width1_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  turnout - side  platform  starting  width ...',
    'turnout - side  platform  starting  width', captext(platform_ts_start_width_ins) +
    '  inches full-size');
  plat_ts_width1_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ts_width2_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  turnout - side  platform  ending  width ...',
    'turnout - side  platform  ending  width', captext(platform_ts_end_width_ins) + '  inches full-size');
  plat_ts_width2_mod := 1;
end;
//______________________________________________________________________________


procedure Tplatform_form.mouse_ms_start_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  main - side  platform  start ...',
    'main - side  platform  start', captext(platform_ms_start_mm) + ' mm');
  plat_ms_start_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ms_length_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  // using mouse action cancels default full template length ..
  // turnoutx may have been reduced since the length was set ...

  if platform_ms_length_mm = def_req then
    platform_ms_length_mm := turnoutx - platform_ms_start_mm;

  if turnoutx < (platform_ms_start_mm + platform_ms_length_mm) then
    platform_ms_length_mm := turnoutx - platform_ms_start_mm;

  if platform_ms_length_mm < 0 then
    platform_ms_length_mm := 0;

  mouse_action_selected('    adjust  main - side  platform  length ...',
    'main - side  platform  length', captext(platform_ms_length_mm) + ' mm');
  plat_ms_length_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ms_width1_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  main - side  platform  starting  width ...',
    'main - side  platform  starting  width', captext(platform_ms_start_width_ins) +
    '  inches full-size');
  plat_ms_width1_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ms_width2_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  main - side  platform  ending  width ...',
    'main - side  platform  ending  width', captext(platform_ms_end_width_ins) + '  inches full-size');
  plat_ms_width2_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.help_buttonClick(Sender: TObject);

begin
  help(0, platform_help_str, '');
end;
//______________________________________________________________________________

procedure Tplatform_form.hide_panelClick(Sender: TObject);

begin
  Hide;
end;
//______________________________________________________________________________

procedure Tplatform_form.ts_position_buttonClick(Sender: TObject);

const
  ts_start_str: string = 'Enter the start position for the turnout-side (TS) platform in model mm.';
  ts_length_str: string = 'Enter the length of turnout-side (TS) platform in model mm.';

  ts_start_skew_str: string =
    'Enter the amount by which the start of the turnout-side platform is skewed, in model mm.'
    + '||A negative dimension shortens the rear edge of the platform at the start.'
    +
    '||A positive dimension extends the rear edge of the platform at the start, but not beyond the template boundary.'
    + '||For a square-ended platform enter 0 (zero) to cancel skewing.';

  ts_end_skew_str: string =
    'Enter the amount by which the end of the turnout-side platform is skewed, in model mm.'
    + '||A negative dimension shortens the rear edge of the platform at the end.'
    +
    '||A positive dimension extends the rear edge of the platform at the end, but not beyond the template boundary.'
    + '||For a square-ended platform enter 0 (zero) to cancel skewing.';

var
  n: integer;
  od: Toutdim;

begin
  putdim(ts_start_str, 1, 'turnout-side  platform  starts  at', platform_ts_start_mm,
    True, False, False, False);  // no neg, preset ok, zero ok, don't terminate on zero.
  putdim(ts_length_str, 1, 'turnout-side  platform  length',
    platform_ts_length_mm, True, False, True, False);
  // no neg, preset ok, no zero, don't terminate on zero.

  putdim(ts_start_skew_str, 1, 'turnout-side  platform  start  skewed  by',
    platform_ts_start_skew_mm, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.
  n := putdim(ts_end_skew_str, 1, 'turnout-side  platform  end  skewed  by',
    platform_ts_end_skew_mm, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.

  if n <> 3 then
    EXIT;
  if getdims('turnout - side  platform  position', platform_help_str, platform_form, n, od) =
    True then
  begin
    platform_ts_start_mm := od[0];
    if platform_ts_start_mm = def_req then
      platform_ts_start_mm := 0;

    platform_ts_length_mm := od[1];  // return def_req, converted in strails();

    platform_ts_start_skew_mm := od[2];  // 207a
    platform_ts_end_skew_mm := od[3];    // 207a
  end;

  do_railedges;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Tplatform_form.ts_width_buttonClick(Sender: TObject);

const
  ts_width1_str: string = 'starting width of turnout-side platform in full-size inches';
  ts_width2_str: string = 'ending width of turnout-side platform in full-size inches';

var
  n: integer;
  od: Toutdim;

begin
  putdim(ts_width1_str, 2, 'turnout-side  platform  starting  width  (F-S inches)',
    platform_ts_start_width_ins, True, False, False, False);
  // no neg, preset ok, zero ok, don't terminate on zero.
  n := putdim(ts_width2_str, 2, 'turnout-side  platform  ending  width  (F-S inches)',
    platform_ts_end_width_ins, True, False, False, False);
  // no neg, preset ok, zero ok, don't terminate on zero.
  if n <> 1 then
    EXIT;
  if getdims('turnout - side  platform  widths', platform_help_str, platform_form, n, od) = True then
  begin
    platform_ts_start_width_ins := od[0];
    if platform_ts_start_width_ins = def_req then
      platform_ts_start_width_ins := 144;  // preset 12ft

    platform_ts_end_width_ins := od[1];
    if platform_ts_end_width_ins = def_req then
      platform_ts_end_width_ins := 144;  // preset 12ft
  end;

  redraw_pad(True, True);
end;
//____________________________________________________________________________________________

procedure Tplatform_form.ts_spacing_buttonClick(Sender: TObject);

const
  ts_spacing_str: string =
    'Enter the spacing of turnout-side (TS) platform edge from the track centre-line, in full-size inches.||For UK standard-gauge practice this dimension should be 57 inches.';

var
  n: integer;
  od: Toutdim;

begin
  n := putdim(ts_spacing_str, 2, 'turnout-side  (TS)  platform  spacing  (F-S inches)',
    platform_ts_front_edge_ins, False, False, False, False);
  // neg ok, preset ok, zero ok, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('turnout - side  platform  spacing', platform_help_str, platform_form, n, od) =
    True then
  begin
    platform_ts_front_edge_ins := od[0];
    if platform_ts_front_edge_ins = def_req then
      platform_ts_front_edge_ins := 57;   // 4ft-9in default
  end;

  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Tplatform_form.ms_position_buttonClick(Sender: TObject);

const
  ms_start_str: string = 'Enter the start position for the main-side (MS) platform in model mm.';
  ms_length_str: string = 'Enter the length of main-side (MS) platform in model mm.';

  ms_start_skew_str: string =
    'Enter the amount by which the start of the main-side platform is skewed, in model mm.'
    + '||A negative dimension shortens the rear edge of the platform at the start.'
    +
    '||A positive dimension extends the rear edge of the platform at the start, but not beyond the template boundary.'
    + '||For a square-ended platform enter 0 (zero) to cancel skewing.';

  ms_end_skew_str: string =
    'Enter the amount by which the end of the main-side platform is skewed, in model mm.'
    + '||A negative dimension shortens the rear edge of the platform at the end.'
    +
    '||A positive dimension extends the rear edge of the platform at the end, but not beyond the template boundary.'
    + '||For a square-ended platform enter 0 (zero) to cancel skewing.';

var
  n: integer;
  od: Toutdim;

begin
  putdim(ms_start_str, 1, 'main-side  platform  starts  at', platform_ms_start_mm,
    True, False, False, False);  // no neg, preset ok, zero ok, don't terminate on zero.
  putdim(ms_length_str, 1, 'main-side  platform  length',
    platform_ms_length_mm, True, False, True, False);
  // no neg, preset ok, no zero, don't terminate on zero.

  putdim(ms_start_skew_str, 1, 'main-side  platform  start  skewed  by',
    platform_ms_start_skew_mm, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.
  n := putdim(ms_end_skew_str, 1, 'main-side  platform  end  skewed  by',
    platform_ms_end_skew_mm, False, True, False, False);
  // neg ok, no preset, zero ok, don't terminate on zero.

  if n <> 3 then
    EXIT;

  if getdims('main - side  platform  position', platform_help_str, platform_form, n, od) = True then
  begin
    platform_ms_start_mm := od[0];
    if platform_ms_start_mm = def_req then
      platform_ms_start_mm := 0;

    platform_ms_length_mm := od[1];
    // return def_req, converted in strails(); to full template length.

    platform_ms_start_skew_mm := od[2];  // 207a
    platform_ms_end_skew_mm := od[3];    // 207a

  end;

  do_railedges;
  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Tplatform_form.ms_width_buttonClick(Sender: TObject);

const
  ms_width1_str: string = 'starting width of main-side platform in full-size inches';
  ms_width2_str: string = 'ending width of main-side platform in full-size inches';

var
  n: integer;
  od: Toutdim;

begin
  putdim(ms_width1_str, 2, 'main-side  platform  starting  width  (F-S inches)',
    platform_ms_start_width_ins, True, False, False, False);
  // no neg, preset ok, zero ok, don't terminate on zero.
  n := putdim(ms_width2_str, 2, 'main-side  platform  ending  width  (F-S inches)',
    platform_ms_end_width_ins, True, False, False, False);
  // no neg, preset ok, zero ok, don't terminate on zero.
  if n <> 1 then
    EXIT;
  if getdims('main - side  platform  widths', platform_help_str, platform_form, n, od) = True then
  begin
    platform_ms_start_width_ins := od[0];
    if platform_ms_start_width_ins = def_req then
      platform_ms_start_width_ins := 144;  // preset 12ft

    platform_ms_end_width_ins := od[1];
    if platform_ms_end_width_ins = def_req then
      platform_ms_end_width_ins := 144;  // preset 12ft
  end;

  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Tplatform_form.ms_spacing_buttonClick(Sender: TObject);

const
  ms_spacing_str: string =
    'Enter the spacing of main-side (MS) platform edge from the track centre-line, in full-size inches.||For UK standard-gauge practice this dimension should be 57 inches.';

var
  n: integer;
  od: Toutdim;

begin
  n := putdim(ms_spacing_str, 2, 'main-side  (MS)  platform  spacing  (F-S inches)',
    platform_ms_front_edge_ins, False, False, False, False);
  // neg ok, preset ok, zero ok, don't terminate on zero.
  if n <> 0 then
    EXIT;
  if getdims('main - side  platform  spacing', platform_help_str, platform_form, n, od) = True then
  begin
    platform_ms_front_edge_ins := od[0];
    if platform_ms_front_edge_ins = def_req then
      platform_ms_front_edge_ins := 57;   // 4ft-9in default
  end;

  redraw_pad(True, True);
end;
//______________________________________________________________________________

procedure Tplatform_form.colour_patchClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  platforms  dialog', Color);
  Show;             // ensure form doesn't get hidden.
  BringToFront;
end;
//______________________________________________________________________________

procedure Tplatform_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag         // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                          // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                          // scale the form contents up.

  with hide_panel do begin
    platform_form.ClientWidth := Left + Width;
    platform_form.ClientHeight := Top + Height;
  end;//with

  size_updown.Tag := size_updown.Position;   // and save for the next click.
end;
//______________________________________________________________________________

procedure Tplatform_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  cancel_adjusts(False);
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ms_skew1_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  main - side  platform  skew  at  start ...',
    'main - side  platform  skew  at  start', captext(platform_ms_start_skew_mm) + ' mm');
  plat_ms_skew1_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ms_skew2_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  main - side  platform  skew  at  end ...',
    'main - side  platform  skew  at  end', captext(platform_ms_end_skew_mm) + ' mm');
  plat_ms_skew2_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ts_skew1_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  turnout - side  platform  skew  at  start ...',
    'turnout - side  platform  skew  at  start', captext(platform_ts_start_skew_mm) + ' mm');
  plat_ts_skew1_mod := 1;
end;
//______________________________________________________________________________

procedure Tplatform_form.mouse_ts_skew2_buttonClick(Sender: TObject);

begin
  cancel_adjusts(True);

  mouse_action_selected('    adjust  turnout - side  platform  skew  at  end ...',
    'turnout - side  platform  skew  at  end', captext(platform_ts_end_skew_mm) + ' mm');
  plat_ts_skew2_mod := 1;
end;
//______________________________________________________________________________


end.
