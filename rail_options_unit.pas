
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

unit rail_options_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Trail_options_form = class(TForm)
    top_label: TLabel;
    turnout_groupbox: TGroupBox;
    joints_groupbox: TGroupBox;
    turnout_road_check_rail_checkbox: TCheckBox;
    turnout_road_crossing_rail_checkbox: TCheckBox;
    crossing_vee_checkbox: TCheckBox;
    main_road_crossing_rail_checkbox: TCheckBox;
    main_road_check_rail_checkbox: TCheckBox;
    main_road_stock_rail_checkbox: TCheckBox;
    switch_rail_joints_checkbox: TCheckBox;
    switch_stock_rail_joints_checkbox: TCheckBox;
    switch_front_joints_checkbox: TCheckBox;
    wing_rail_joints_checkbox: TCheckBox;
    vee_rail_joints_checkbox: TCheckBox;
    k_crossing_joints_checkbox: TCheckBox;
    plain_track_groupbox: TGroupBox;
    plain_track_ts_rail_checkbox: TCheckBox;
    plain_track_ms_rail_checkbox: TCheckBox;
    turnout_road_stock_rail_checkbox: TCheckBox;
    restore_all_button: TButton;
    help_button: TButton;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    help_shape: TShape;
    hide_panel: TPanel;
    hide_button: TButton;
    k_ms_check_rail_checkbox: TCheckBox;
    k_ds_check_rail_checkbox: TCheckBox;
    invert_rails_button: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure switch_front_joints_checkboxClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure restore_all_buttonClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure hide_buttonClick(Sender: TObject);
    procedure invert_rails_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  rail_options_form: Trail_options_form;

procedure update_rail_settings;
// 206b  update all the checkboxes (called from compile_info on pad)


implementation

{$BOOLEVAL ON}


uses
  pad_unit, help_sheet, colour_unit, math_unit;

{$R *.lfm}

var
  form_showing: boolean = False;

//______________________________________________________________


procedure update_rail_settings;
// 206b  update all the checkboxes (called from compile_info on pad)

begin
  form_showing := False;   // needed to prevent these changes generating a click on the boxes.

  with rail_options_form do begin

    if plain_track = True then begin
      plain_track_groupbox.Font.Color := clNavy;
      turnout_groupbox.Font.Color := clGray;
      joints_groupbox.Font.Color := clGray;
    end
    else begin
      plain_track_groupbox.Font.Color := clGray;
      turnout_groupbox.Font.Color := clNavy;
      joints_groupbox.Font.Color := clNavy;
    end;

    if half_diamond = False then begin
      turnout_groupbox.Caption := ' turnout  rails :  ';
      turnout_road_stock_rail_checkbox.Caption := 'turnout-road  stock  rail';
      turnout_road_check_rail_checkbox.Caption := 'turnout-road  check  rail';
      turnout_road_crossing_rail_checkbox.Caption := 'turnout-road  crossing  rail';

      //main_road_check_rail_checkbox.Caption:='main-road  check  rail';
    end
    else begin
      turnout_groupbox.Caption := ' half-diamond  rails :  ';
      turnout_road_stock_rail_checkbox.Caption := 'diagonal-road  stock  rail';
      turnout_road_check_rail_checkbox.Caption := 'diagonal-road  check  rail';
      turnout_road_crossing_rail_checkbox.Caption := 'diagonal-road  crossing  rail';

      //main_road_check_rail_checkbox.Caption:='main-road  check  rails';
    end;

    plain_track_ms_rail_checkbox.Enabled := plain_track;
    plain_track_ts_rail_checkbox.Enabled := plain_track;

    turnout_road_stock_rail_checkbox.Enabled := not plain_track;
    turnout_road_check_rail_checkbox.Enabled := not plain_track;
    turnout_road_crossing_rail_checkbox.Enabled := not plain_track;
    crossing_vee_checkbox.Enabled := not plain_track;
    main_road_crossing_rail_checkbox.Enabled := not plain_track;
    main_road_check_rail_checkbox.Enabled := not plain_track;
    main_road_stock_rail_checkbox.Enabled := not plain_track;

    switch_front_joints_checkbox.Enabled := not plain_track;
    switch_rail_joints_checkbox.Enabled := not plain_track;
    switch_stock_rail_joints_checkbox.Enabled := not plain_track;
    wing_rail_joints_checkbox.Enabled := not plain_track;
    vee_rail_joints_checkbox.Enabled := not plain_track;

    k_crossing_joints_checkbox.Enabled := (plain_track = False) and (half_diamond = True);

    k_ds_check_rail_checkbox.Enabled := (plain_track = False) and (half_diamond = True);
    k_ms_check_rail_checkbox.Enabled := (plain_track = False) and (half_diamond = True);

    //-------

    switch_front_joints_checkbox.Checked := not omit_swfj_marks;
    switch_rail_joints_checkbox.Checked := not omit_swrj_marks;
    switch_stock_rail_joints_checkbox.Checked := not omit_skj_marks;
    wing_rail_joints_checkbox.Checked := not omit_wj_marks;
    vee_rail_joints_checkbox.Checked := not omit_vj_marks;
    k_crossing_joints_checkbox.Checked := not omit_kx_marks;

    turnout_road_stock_rail_checkbox.Checked := turnout_road_stock_rail_flag;
    turnout_road_check_rail_checkbox.Checked := turnout_road_check_rail_flag;
    turnout_road_crossing_rail_checkbox.Checked := turnout_road_crossing_rail_flag;
    crossing_vee_checkbox.Checked := crossing_vee_flag;
    main_road_stock_rail_checkbox.Checked := main_road_stock_rail_flag;
    main_road_check_rail_checkbox.Checked := main_road_check_rail_flag;
    main_road_crossing_rail_checkbox.Checked := main_road_crossing_rail_flag;

    plain_track_ts_rail_checkbox.Checked := turnout_road_stock_rail_flag;
    plain_track_ms_rail_checkbox.Checked := main_road_stock_rail_flag;

    k_ds_check_rail_checkbox.Checked := k_diagonal_side_check_rail_flag;
    k_ms_check_rail_checkbox.Checked := k_main_side_check_rail_flag;

  end;//with form

  form_showing := True;
end;
//________________________________________________________________

procedure apply_settings;

var
  cancel_diff: boolean;

begin
  if form_showing = False then
    EXIT;

  with rail_options_form do begin

    omit_swfj_marks := not switch_front_joints_checkbox.Checked;
    omit_swrj_marks := not switch_rail_joints_checkbox.Checked;
    omit_skj_marks := not switch_stock_rail_joints_checkbox.Checked;
    omit_wj_marks := not wing_rail_joints_checkbox.Checked;
    omit_vj_marks := not vee_rail_joints_checkbox.Checked;
    omit_kx_marks := not k_crossing_joints_checkbox.Checked;

    if plain_track = True       // mods 0.94.a
    then begin
      turnout_road_stock_rail_flag := plain_track_ts_rail_checkbox.Checked;
      main_road_stock_rail_flag := plain_track_ms_rail_checkbox.Checked;
    end
    else begin
      turnout_road_stock_rail_flag := turnout_road_stock_rail_checkbox.Checked;
      main_road_stock_rail_flag := main_road_stock_rail_checkbox.Checked;
    end;

    turnout_road_check_rail_flag := turnout_road_check_rail_checkbox.Checked;
    turnout_road_crossing_rail_flag := turnout_road_crossing_rail_checkbox.Checked;

    crossing_vee_flag := crossing_vee_checkbox.Checked;

    main_road_check_rail_flag := main_road_check_rail_checkbox.Checked;
    main_road_crossing_rail_flag := main_road_crossing_rail_checkbox.Checked;

    k_diagonal_side_check_rail_flag := k_ds_check_rail_checkbox.Checked;
    k_main_side_check_rail_flag := k_ms_check_rail_checkbox.Checked;

  end;//with

  cancel_diff := False;  // init

  case current_diff_code of   // 0.94.a  check rail diffs ...

    501, 502:
      cancel_diff := not main_road_check_rail_flag;          // MS1 MS2
    503:
      cancel_diff := not turnout_road_crossing_rail_flag;    // MS3
    504, 505:
      cancel_diff := not turnout_road_check_rail_flag;       // TS1 TS2
    506:
      cancel_diff := not main_road_crossing_rail_flag;       // TS3
    507:
      cancel_diff := not k_main_side_check_rail_flag;        // MS4
    508:
      cancel_diff := not k_diagonal_side_check_rail_flag;    // DS4

  end;//case

  if cancel_diff = True then
    current_diff_code := 0;    // no longer a check rail to adjust.

  do_railedges;
  redraw_pad(True, True);
end;
//_________________________________________________________________

procedure Trail_options_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  apply_settings;
  form_showing := False;
end;
//__________________________________________________________________

procedure Trail_options_form.switch_front_joints_checkboxClick(Sender: TObject);

// common to all checkbox clicks...

begin
  if form_showing = False then
    EXIT;   // 0.94.a

  apply_settings;
end;
//_____________________________________________________________________

procedure Trail_options_form.help_buttonClick(Sender: TObject);

const
  rail_options_help_str: string = '    `0Omit  Individual  Rails  and  Joints`9'
    + '||Untick these boxes to omit individual rails or rail-joint marks from the control template.'
    + '||These options are intended for use when overlaying partial templates. To remove all the rails click instead the `0REAL > RAILS > NO RAILS`1 menu item.' + '||To change the settings for plain-track joint marks, click the `0REAL > PLAIN TRACK OPTIONS > RAIL-JOINT MARKS >`1 menu options.' + '||Click the `0RESTORE ALL`1 button to restore all rails and joint marks omitted here. (This will have no effect if `0REAL > RAILS > NO RAILS`1 has been selected.)';

begin
  help(0, rail_options_help_str, '');
end;
//__________________________________________________________________________________________

procedure Trail_options_form.restore_all_buttonClick(Sender: TObject);

begin
  form_showing := False;   // needed to prevent these changes generating a click on the boxes.

  switch_front_joints_checkbox.Checked := True;
  switch_rail_joints_checkbox.Checked := True;
  switch_stock_rail_joints_checkbox.Checked := True;
  wing_rail_joints_checkbox.Checked := True;
  vee_rail_joints_checkbox.Checked := True;
  k_crossing_joints_checkbox.Checked := True;

  turnout_road_stock_rail_checkbox.Checked := True;
  turnout_road_check_rail_checkbox.Checked := True;
  turnout_road_crossing_rail_checkbox.Checked := True;
  crossing_vee_checkbox.Checked := True;
  main_road_stock_rail_checkbox.Checked := True;
  main_road_check_rail_checkbox.Checked := True;
  main_road_crossing_rail_checkbox.Checked := True;

  plain_track_ts_rail_checkbox.Checked := True;
  plain_track_ms_rail_checkbox.Checked := True;

  k_ds_check_rail_checkbox.Checked := True;
  k_ms_check_rail_checkbox.Checked := True;

  form_showing := True;

  apply_settings;
end;
//____________________________________________________________________________________

procedure Trail_options_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
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
end;
//__________________________________________________________________________________________

procedure Trail_options_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  window  colour  for  the  rail  options', Color);
end;
//__________________________________________________________________________________________

procedure Trail_options_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(rail_options_form);

  AutoScroll := True;
end;
//______________________________________________________________________________

procedure Trail_options_form.hide_buttonClick(Sender: TObject);

begin
  Hide;
end;
//______________________________________________________________________________

procedure Trail_options_form.invert_rails_buttonClick(Sender: TObject);  // 0.94.a

begin
  form_showing := False;   // needed to prevent these changes generating a click on the boxes.

  if plain_track = False then begin
    turnout_road_stock_rail_checkbox.Checked :=
      not turnout_road_stock_rail_checkbox.Checked;

    turnout_road_check_rail_checkbox.Checked :=
      not turnout_road_check_rail_checkbox.Checked;
    turnout_road_crossing_rail_checkbox.Checked :=
      not turnout_road_crossing_rail_checkbox.Checked;
    crossing_vee_checkbox.Checked := not crossing_vee_checkbox.Checked;

    main_road_stock_rail_checkbox.Checked :=
      not main_road_stock_rail_checkbox.Checked;
    main_road_check_rail_checkbox.Checked :=
      not main_road_check_rail_checkbox.Checked;
    main_road_crossing_rail_checkbox.Checked :=
      not main_road_crossing_rail_checkbox.Checked;
  end
  else begin
    plain_track_ts_rail_checkbox.Checked := not plain_track_ts_rail_checkbox.Checked;
    plain_track_ms_rail_checkbox.Checked := not plain_track_ms_rail_checkbox.Checked;
  end;

  if half_diamond = True then begin
    k_ds_check_rail_checkbox.Checked := not k_ds_check_rail_checkbox.Checked;
    k_ms_check_rail_checkbox.Checked := not k_ms_check_rail_checkbox.Checked;
  end;

  form_showing := True;

  apply_settings;
end;
//______________________________________________________________________________

procedure Trail_options_form.FormShow(Sender: TObject);

begin
  update_rail_settings;       // 206b
end;
//______________________________________________________________________________

end.
