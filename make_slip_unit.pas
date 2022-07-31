
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
unit make_slip_unit;

{$MODE Delphi}

interface

// form is used for the toolbars dragging

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  StdCtrls;

type

  { Tmake_slip_form }

  Tmake_slip_form = class(TForm)
    datestamp_label: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  make_slip_form: Tmake_slip_form;

  slip_switch_6: integer = 0;      // global index into switch list.  set in switch_selelect unit
  slip_switch_7: integer = 0;
  slip_switch_8: integer = 0;
  slip_switch_10: integer = 0;

  mod_double_slip: integer = 1;       // 0=normal,  1=reduced gauges only   2=all gauges

function make_slip(sides: integer; making_crossover: boolean): boolean;  // 215a

implementation

{$R *.lfm}

uses
  pad_unit, math_unit, keep_select, alert_unit, info_unit, control_room, shove_timber,
  switch_select,
  shoved_timber,
  curve,
  template{ OT-FIRST , web_browser_unit};

//______________________________________________________________________________

function make_slip(sides: integer; making_crossover: boolean): boolean;        // 215a

  // sides   double-slip = 0,    single-slip same side = 1,    single-slip opposite side = -1.

var
  dummy1, dummy2: double;
  i, n: integer;
  savedControl: TTemplate;

  saved_notch: Tnotch;
  saved_name_str: string;
  saved_memo_str: string;

  sw_info: Tswitch_info;

  sw_index: integer;

  switch_length: double;

  nom_sw_len: double;

  nom_heel_pegx_on_pad, nom_heel_pegy_on_pad: double;

  sw_heel_pegx_on_pad, sw_heel_pegy_on_pad: double;

  fp_pegx_on_pad, fp_pegy_on_pad: double;

  first_hd_index, second_hd_index: integer;
  first_sw_index, second_sw_index: integer;

  first_bgnd_template: integer;

  rot_move, rot_k: double;

  x1, y1, x2, y2, x3, y3: double;

  id_str: string;

  slip_road_pos1: Tnotch;
  slip_road_pos2: Tnotch;
  slip_road_pos3: Tnotch;
  slip_road_pos4: Tnotch;

  slip_chord, slip_angle, slip_rad: double;

  slip_arc_len: double;

  slip_offset, slip_turn: double;

  offset_pos_sq, offset_neg_sq: double;

  doing_2nd_side: boolean;

  max_index: integer;

  switch_mid_inches, switch_mid_mm: double;

  switch_mid_rad1, switch_mid_rad2: double;

  org_was_spiral: boolean;
  spiral_safe: boolean;
  spiral_ok: boolean;

  temp, temp1, temp2: double;

  slip_rad1_str, slip_rad2_str: string;

  xover_str: string;


  ////////////////////////////////////////////////////////////////

  procedure restore_current;

  begin
    copy_keep(savedControl);
    // retrieve saved original control template.
    current_name_str := saved_name_str;
    current_memo_str := saved_memo_str;

    info_form.ref_name_label.Caption := current_name_str;
  end;
  ////////////////////////////////////////////////////////////////

  procedure do_template_names;

  var
    z: integer;
    str: string;

  begin
    if keeps_list.Count > first_bgnd_template then begin
      for z := first_bgnd_template to keeps_list.Count - 1 do begin

        if keeps_list[z].bgnd_half_diamond =
          True then
          str := ' slip half-diamond'
        else
        if keeps_list[z].bgnd_plain_track = True
        then
          str := ' slip road'
        else
          str := ' slip switch';

        keeps_list[z].template_info.keep_dims.box_dims1.reference_string := id_str + ' ' + str;
      end;//next
    end;
  end;
  ////////////////////////////////////////////////////////////////

  procedure delete_slip;

  begin
    if keeps_list.Count > first_bgnd_template then
      repeat
        list_position := keeps_list.Count - 1;
        delete_keep(False, False);
      until keeps_list.Count = first_bgnd_template;
  end;
  ////////////////////////////////////////////////////////////////

label
  111;

begin
  Result := False;  // init

  if plain_track = True then begin
    alert(6, '    make  slip  -  plain  track',
      'Sorry, this function is not available because the control template is plain track.'
      + '||This function requires a turnout or half-diamond template as its starting point.'
      + '||You must first change to a turnout or half-diamond template, by clicking the `0TEMPLATE > INSERT TURNOUT IN PLAIN TRACK`1 menu item, or the `0TEMPLATE > INSERT HALF-DIAMOND IN PLAIN TRACK`1 menu item.' + '||And then setting the hand and facing/trailing direction as required.',
      '', '', '', '', 'cancel', '', 0);
    EXIT;
  end;

  if controlTemplate.curve.isSlewing then begin
    alert(6, '    make  slip  -  slewed  track',
      'Sorry, this function is not available because the control template contains a slew.'
      + '||A slip could be created manually, but it is generally unwise to create a slip if any part of it will be within a slewing zone.' + '||The slewing function is intended primarily for plain track.',
      '', '', '', '', 'cancel', '', 0);
    EXIT;
  end;

  if (half_diamond = False) and (xing_calc_i = 1)    // curviform
  then begin
    if alert(2, '    make  regular  slip',
      '||Your control template contains a curviform V-crossing.'
      +
      '||This tools function can make a regular slip only.||An irregular slip with curviform V-crossings could be created manually.'
      + '||If you click `0continue`1 below, the V-crossings will be changed to regular pattern.'
      + '||This means that the alignment of the turnout-road exit will be modified.',
      '', '', '', '', 'cancel', 'continue  -  make  regular  slip', 0) = 5 then
      EXIT;
  end;

  if (half_diamond = True) and (hdkn <> k3n) then begin
    if alert(2, '    make  regular  slip',
      '||The control template is an irregular half-diamond|(V-crossing and K-crossing angles differ).'
      + '||This tools function can make a regular slip only.||An irregular slip could be created manually.'
      + '||If you continue the K-crossing angle will be modified to match the V-crossing angle, to create a regular slip.', '', '', '', '', 'cancel', 'convert  to  regular  and  continue', 0) = 5 then
      EXIT;
  end;

  if k3n < 5 then begin
    if alert(1, '    make  regular  inside  slip',
      '||The V-crossing angle in the control template is shorter than 1:5 (it is 1:'
      +
      FormatFloat('0.##', k3n) + ').' +
      '||It is very unlikely that a satisfactory inside slip can be created at such a short angle. The radius of the slip road will be far too small.' + '||If you continue you may encounter unexpected results.' + '||Generally at such short angles an outside slip or half-scissors would be used instead.', '', '', '', '', 'cancel', 'continue  anyway', 0) = 5 then
      EXIT;
  end
  else
  if k3n < 6 then begin
    if alert(2, '    make  regular  inside  slip',
      '||The V-crossing angle in the control template is shorter than 1:6 (it is 1:'
      +
      FormatFloat('0.##', k3n) + ').' +
      '||Inside slips shorter than 1:6 are unusual on the prototype. Generally at such short angles an outside slip or half-scissors would be used instead.' + '||If you continue you may find that the radius in the slip road is smaller than you wanted.', '', '', '', '', 'cancel', 'continue  anyway', 0) = 5 then
      EXIT;
  end;


  // ready to go ...

  slip_rad1_str := '';   // init...
  slip_rad2_str := '';

  if making_crossover = True then
    xover_str := 'crossover '
  else
    xover_str := '';

  org_was_spiral := controlTemplate.curve.isSpiral;

  doing_2nd_side := False;

  id_str := '[slip ' + FormatDateTime('hhmmss', Time) + ']';

  savedControl := TTemplate.Create('');
  try
    fill_kd(savedControl);                              // save control template
    saved_name_str := current_name_str;
    saved_memo_str := current_memo_str;

    pad_form.reset_peg_menu_entry.Enabled := True;
    pad_form.reset_peg_menu_entry.Click;           //peg on CTRL-0
    gocalc(0, 0);

    if turnoutx = 0 then
      extend_template_from_zero;

    if making_crossover = True       // make it first ...
    then begin
      if make_crossover(True, False, True) = False then begin
        restore_current;
        EXIT;
      end;
    end;

    retain_on_make;    // do blanking, shoves, diffs, crossing entry straight, cancel platforms  213a

    convert_to_regular_half_diamond;

    gocalc(0, 0);

    if controlTemplate.curve.isSpiral then begin
      if (controlTemplate.curve.distanceToTransition > fpx) or ((controlTemplate.curve.distanceToTransition + controlTemplate.curve.transitionLength) < (toex - (fpx - toex)))
      // allow for other half_diamond to be created
      then begin
        spiral_safe := True;              // slip section not in transition zone
        spiral_ok := True;
      end
      else begin
        spiral_safe := False;                              // slip in transition zone
        spiral_ok := (trans_k / SQR(scale)) > 24000;
        // transition constant gentle enough (24000 arbitrary)
      end;
    end
    else begin
      spiral_safe := True;      // keep compiler happy
      spiral_ok := True;
    end;

    if (controlTemplate.curve.isSpiral) and (not spiral_safe) and (not spiral_ok) then begin
      if alert(2, '    make  slip  -  sharp  transition  curve',
        '||This slip will contain a sharp transition zone.'
        + '||The slip road(s) created by this function may not align perfectly with the slip switches. You can correct this if necessary by creating the slip road(s) manually using the `0make transition`3 functions.' + '||You could avoid this difficulty by making the transition zone more gentle, or by moving the slip away from it.' + '||For more information please ask on the <A HREF="go_to_templot_club.85a"><U>Templot&nbsp;Club</U></A> user forum.', '', '', '', '', 'cancel', 'continue  -  make  slip', 0) = 5 then begin
        restore_current;
        EXIT;
      end;

    end;

    // shorten check rails if below 1:8 ...

    if k3n < 8.0 then begin

      case sides of
        -1:
          hd_vcheck_rails := 2;       // opposite hand, main side.
        0:
          hd_vcheck_rails := 3;       // double slip
        1:
          hd_vcheck_rails := 1;       // same hand, diagonal side.
        else
          hd_vcheck_rails := 0;       // ?? normal
      end;//case
    end
    else
      hd_vcheck_rails := 0;  // normal length

    if (sides = 0) and (k3n > 6.51)      // double-slip  overrides ...
    then begin
      case mod_double_slip of
        1:
          if g < (56 * inscale) then
            hd_vcheck_rails := 0;
        // not P4 etc.     switch moved so shortened check rails not needed
        2:
          hd_vcheck_rails := 0;                          // for all gauges
      end;//case
    end;

    // extend timbering ( for all crossing angles) ...

    case sides of
      -1:
        hd_timbers := 2;       // opposite hand, main side.
      0:
        hd_timbers := 3;       // double slip
      1:
        hd_timbers := 1;       // same hand, diagonal side.
      else
        hd_timbers := 0;       // ?? normal
    end;//case

    gocalc(0, 0);

    first_bgnd_template := keeps_list.Count;  // will be added next

    // select which slip switch will be used, and get approx distance to middle of planing (for spiral radius) ...

    if k3n < 7 then begin
      sw_index := slip_switch_6;
      with Tswitch(switch_select_form.switch_selector_listbox.Items.Objects[sw_index]).list_switch_info do
        switch_mid_inches := switch_front_inches + heel_lead_inches / 2;
    end
    else
    if k3n < 8 then begin
      sw_index := slip_switch_7;
      with Tswitch(
          switch_select_form.switch_selector_listbox.Items.Objects[sw_index]).list_switch_info do
        switch_mid_inches := switch_front_inches + heel_lead_inches / 2;
    end
    else
    if k3n < 10 then begin
      sw_index := slip_switch_8;
      with Tswitch(
          switch_select_form.switch_selector_listbox.Items.Objects[sw_index]).list_switch_info do
        switch_mid_inches := switch_front_inches + heel_lead_inches / 2;
    end
    else begin
      sw_index := slip_switch_10;
      with Tswitch(
          switch_select_form.switch_selector_listbox.Items.Objects[sw_index]).list_switch_info do
        switch_mid_inches := switch_front_inches + heel_lead_inches / 2;
    end;

    if k3n > 10.125 then
      switch_mid_inches := switch_mid_inches + 28;
    // adding some extra approach track   1 timber space
    if k3n > 15.125 then
      switch_mid_inches := switch_mid_inches + 28;
    // adding more extra approach track   2 timber space

    if (sides = 0) and (k3n > 6.51)
    // double-slip - move switch forward
    then begin
      case mod_double_slip of
        1:
          if g < (56 * inscale) then
            switch_mid_inches := switch_mid_inches + 28;
        // not P4 etc.    greater tip clearance.  28" typical fill spacing
        2:
          switch_mid_inches := switch_mid_inches + 28;                          // for all gauges
      end;//case
    end;

    switch_mid_mm := switch_mid_inches * inscale;

    sw_info := Tswitch(switch_select_form.switch_selector_listbox.Items.Objects[sw_index]).list_switch_info;   // get the slip switch


    // come back here for double_slip (doing_2nd_side=True) ...

    111:

      omit_wj_marks := True;
    // and omit the wing rail joint marks      // restored in retain_on_make

    if controlTemplate.curve.isSpiral then
      switch_mid_rad1 := clrad_at_x(mcpx - switch_mid_mm)
    // back from MCP-1 to middle of switch location
    else
      switch_mid_rad1 := controlTemplate.curve.fixedRadius;

    if make_diamond_crossing = False      // make the underlying diamond
    then begin
      ShowMessage('error - Unable to store background templates. Try closing ' +
        Application.Title + ' and restarting.');
      restore_current;
      EXIT;
    end;

    omit_wj_marks := True;  // and omit the wing rail joint marks      // restored in retain_on_make

    if controlTemplate.curve.isSpiral then
      switch_mid_rad2 := clrad_at_x(mcpx - switch_mid_mm)
    // back from MCP-2 to middle of switch location
    else
      switch_mid_rad2 := controlTemplate.curve.fixedRadius;

    first_hd_index := keeps_list.Count - 1;

    if first_hd_index < 0     // ????  failed store
    then begin
      ShowMessage('error - Unable to store background templates. Try closing ' +
        Application.Title + ' and restarting.');
      restore_current;
      EXIT;
    end;

    gocalc(0, 0);

    if sides < 1      // opposite hand or double-slip wanted, so do opposite side first      //sides=-1
    then begin
      if making_crossover = True then
        turnout_road_i := -1;  // for the crossover

      omit_wj_marks := True;
      // and omit the wing rail joint marks      // restored in retain_on_make


      clicked_keep_index := first_hd_index;

      pad_form.make_control_popup_entry.Enabled := True;
      pad_form.make_control_popup_entry.Click;
      // opposite hand wanted -- so swap back to the first half-diamond

      omit_wj_marks := True;
      // and omit the wing rail joint marks      // restored in retain_on_make

      if making_crossover = True then
        turnout_road_i := 0;  // normal


      temp := switch_mid_rad2;                // swap the switch radii
      switch_mid_rad2 := switch_mid_rad1;
      switch_mid_rad1 := temp;
    end;

    gocalc(0, 0);

    store_and_background(False, False);    // now both half_diamonds stored

    if keep_added = False                   // failed store
    then begin
      restore_current;
      EXIT;
    end;

    second_hd_index := keeps_list.Count - 1;

    pad_form.no_track_centre_lines_menu_radio.Enabled := True;
    pad_form.no_track_centre_lines_menu_radio.Click;
    // remove centre lines from now on for neater result

    convert_to_turnout;

    pad_form.reset_peg_menu_entry.Enabled := True;
    pad_form.reset_peg_menu_entry.Click;             //ensure peg on CTRL-0
    gocalc(0, 0);

    // no transitions from now on ...

    controlTemplate.curve.isSpiral := False;
    controlTemplate.curve.fixedRadius := switch_mid_rad2;

    gocalc(0, 0);

    if k3n > 10.125 then
      xorg := xorg + 28 * inscale;     // add some extra approach track   1 timber space
    if k3n > 15.125 then
      xorg := xorg + 28 * inscale;     // add more extra approach track   2 timber space

    if (sides = 0) and (k3n > 6.51)        // double-slip - move switch forward
    then begin
      case mod_double_slip of
        1:
          if g < (56 * inscale) then
            xorg := xorg + 28 * inscale;
        // not P4 etc.    greater tip clearance.  28" typical fill spacing
        2:
          xorg := xorg + 28 * inscale;                          // for all gauges
      end;//case
    end;

    normalize_transforms;
    kform_now := kform;
    docurving(True, True, pegx, pegy, now_peg_x, now_peg_y, now_peg_k, dummy2);
    // save current peg data for peg_curve calcs.

    if set_csi_from_switch_info(sw_info) = False   // slip switch into the control template
    then begin
      restore_current;        //  switch data invalid ????
      EXIT;
    end;

    csi.joggled_stock_rail := False;    //  slip switches are REA-based

    gocalc(0, 0);          // calc new pegx.
    peg_curve;            // adjust shifts and rotates for current peg position.

    invert_curving;

    gocalc(0, 0);

    clicked_keep_index := second_hd_index;       // onto the last-stored half_diamond at MCP

    pad_form.snap_to_mcp_popup_entry.Enabled := True;
    pad_form.snap_to_mcp_popup_entry.Click;

    gocalc(0, 0);

    pad_form.blank_to_toe_menu_entry.Enabled := True;
    pad_form.blank_to_toe_menu_entry.Click;

    main_road_stock_rail_flag := False;            // don't need these
    main_road_crossing_rail_flag := False;

    gocalc(0, 0);

    if org_was_spiral = True then begin

      // adjust by rotation for any spiral discrepancy ...

      switch_length := heelx;     // main stock rail gauge-face

      // swap to the half_diamond..

      clicked_keep_index := second_hd_index;

      pad_form.make_control_popup_entry.Enabled := True;
      pad_form.make_control_popup_entry.Click;

      omit_wj_marks := True;
      // and omit the wing rail joint marks      // restored in retain_on_make

      gocalc(0, 0);

      first_sw_index := keeps_list.Count - 1;

      // get 2 points on main crossing rail ..

      normalize_transforms;
      docurving(True, True, fpx, g, fp_pegx_on_pad, fp_pegy_on_pad, dummy1, dummy2);
      // at FP

      x1 := fp_pegx_on_pad;
      y1 := fp_pegy_on_pad * hand_i + y_datum;

      normalize_transforms;
      docurving(True, True, fpx - switch_length, g, nom_heel_pegx_on_pad,
        nom_heel_pegy_on_pad, dummy1, dummy2);   // at approx switch heel

      x2 := nom_heel_pegx_on_pad;
      y2 := nom_heel_pegy_on_pad * hand_i + y_datum;

      temp := SQR(x2 - x1) + SQR(y2 - y1);

      if temp > minfp then
        nom_sw_len := ABS(SQRT(temp))       // actual distance between them
      else begin
        restore_current;        //  ????
        EXIT;
      end;

      // swap back to the switch  (y_datum changes) ...

      clicked_keep_index := first_sw_index;

      pad_form.make_control_popup_entry.Enabled := True;
      pad_form.make_control_popup_entry.Click;

      omit_wj_marks := True;
      // and omit the wing rail joint marks      // restored in retain_on_make

      gocalc(0, 0);

      second_hd_index := keeps_list.Count - 1;   // stored again, update index

      normalize_transforms;
      docurving(True, True, nom_sw_len * (controlTemplate.curve.fixedRadius - g / 2) / controlTemplate.curve.fixedRadius, 0,
        sw_heel_pegx_on_pad, sw_heel_pegy_on_pad, dummy1, dummy2);

      x3 := sw_heel_pegx_on_pad;
      y3 := sw_heel_pegy_on_pad * hand_i + y_datum;

      temp1 := SQR(x3 - x2) + SQR(y3 - y2);

      if temp1 > minfp then begin
        rot_move := ABS(SQRT(temp1));  // distance to move by rotation

        rot_k := 0 - rot_move * hand_i / (nom_sw_len * controlTemplate.curve.fixedRadius / (controlTemplate.curve.fixedRadius - g / 2));

        rotate_turnout(rot_k, False);

        gocalc(0, 0);

        // check rotated in correct direction ...

        normalize_transforms;
        docurving(True, True, nom_sw_len * (controlTemplate.curve.fixedRadius - g / 2) / controlTemplate.curve.fixedRadius,
          0, sw_heel_pegx_on_pad, sw_heel_pegy_on_pad, dummy1, dummy2);

        x3 := sw_heel_pegx_on_pad;
        y3 := sw_heel_pegy_on_pad * hand_i + y_datum;

        temp2 := SQR(x3 - x2) + SQR(y3 - y2);

        if temp2 > minfp then begin
          if ABS(SQRT(temp2)) > rot_move    // went wrong way!
          then begin
            rotate_turnout(0 - 2 * rot_k, False);   // reverse out
            gocalc(0, 0);
          end;
        end;
      end;

      gocalc(0, 0);

    end;// was spiral

    pad_form.make_slip_road_menu_item.Enabled := True;
    pad_form.make_slip_road_menu_item.Click;

    omit_wj_marks := True;  // and omit the wing rail joint marks      // restored in retain_on_make

    gocalc(0, 0);

    first_sw_index := keeps_list.Count - 1;

    slip_road_pos1 := keeps_list[first_sw_index].snap_peg_positions.ctrl_planing_pos;
    // location of end of planing on pad

    // get the switch back
    // and make it the control (discard the slip road)
    copy_keep(keeps_list[first_sw_index]);

    gocalc(0, 0);

    pad_form.reset_peg_menu_entry.Enabled := True;
    pad_form.reset_peg_menu_entry.Click;           //peg on CTRL-0

    gocalc(0, 0);

    swap_end_for_end;     // swap facing-trailing

    gocalc(0, 0);

    controlTemplate.curve.fixedRadius := switch_mid_rad1;

    clicked_keep_index := first_hd_index;       // onto the first-stored half_diamond at TCP

    pad_form.snap_to_tcp_popup_entry.Enabled := True;
    pad_form.snap_to_tcp_popup_entry.Click;

    gocalc(0, 0);

    // adjust by rotation for any curve-wrapping or spiral discrepancy ...

    switch_length := heelx;     // main stock rail gauge-face

    // swap to the half_diamond..

    clicked_keep_index := first_hd_index;

    pad_form.make_control_popup_entry.Enabled := True;
    pad_form.make_control_popup_entry.Click;

    omit_wj_marks := True;  // and omit the wing rail joint marks      // restored in retain_on_make

    gocalc(0, 0);

    second_sw_index := keeps_list.Count - 1;

    // get 2 points on diagonal crossing rail ..

    normalize_transforms;
    docurving(True, True, fpx, g, fp_pegx_on_pad, fp_pegy_on_pad, dummy1, dummy2);       // at FP

    x1 := fp_pegx_on_pad;
    y1 := fp_pegy_on_pad * hand_i + y_datum;

    normalize_transforms;
    docurving(True, True, fpx - switch_length * COS(k3), g - switch_length * SIN(k3),
      nom_heel_pegx_on_pad, nom_heel_pegy_on_pad, dummy1, dummy2);   // at approx switch heel

    x2 := nom_heel_pegx_on_pad;
    y2 := nom_heel_pegy_on_pad * hand_i + y_datum;

    temp := SQR(x2 - x1) + SQR(y2 - y1);

    if temp > minfp then
      nom_sw_len := ABS(SQRT(temp))       // actual distance between them
    else begin
      restore_current;        //  ????
      EXIT;
    end;

    // swap back to the switch  (y_datum changes) ...

    clicked_keep_index := second_sw_index;

    pad_form.make_control_popup_entry.Enabled := True;
    pad_form.make_control_popup_entry.Click;

    omit_wj_marks := True;  // and omit the wing rail joint marks      // restored in retain_on_make

    gocalc(0, 0);

    first_hd_index := keeps_list.Count - 1;   // stored again, update index

    normalize_transforms;
    docurving(True, True, nom_sw_len * (controlTemplate.curve.fixedRadius - g / 2) / controlTemplate.curve.fixedRadius, 0, sw_heel_pegx_on_pad,
      sw_heel_pegy_on_pad, dummy1, dummy2);

    x3 := sw_heel_pegx_on_pad;
    y3 := sw_heel_pegy_on_pad * hand_i + y_datum;

    temp1 := SQR(x3 - x2) + SQR(y3 - y2);

    if temp1 > minfp then begin
      rot_move := ABS(SQRT(temp1));  // distance to move by rotation

      rot_k := 0 - rot_move * hand_i / (nom_sw_len * controlTemplate.curve.fixedRadius / (controlTemplate.curve.fixedRadius - g / 2));

      rotate_turnout(rot_k, False);

      gocalc(0, 0);

      // check rotated in correct direction ...

      normalize_transforms;
      docurving(True, True, nom_sw_len * (controlTemplate.curve.fixedRadius - g / 2) / controlTemplate.curve.fixedRadius, 0,
        sw_heel_pegx_on_pad, sw_heel_pegy_on_pad, dummy1, dummy2);

      x3 := sw_heel_pegx_on_pad;
      y3 := sw_heel_pegy_on_pad * hand_i + y_datum;

      temp2 := SQR(x3 - x2) + SQR(y3 - y2);

      if temp2 > minfp then begin
        if ABS(SQRT(temp2)) > rot_move    // went wrong way!
        then begin
          rotate_turnout(0 - 2 * rot_k, False);   // reverse out
          gocalc(0, 0);
        end;
      end;

    end;

    gocalc(0, 0);

    // now make slip road, initially straight ...

    pad_form.make_slip_road_menu_item.Enabled := True;
    pad_form.make_slip_road_menu_item.Click;

    omit_wj_marks := True;  // and omit the wing rail joint marks      // restored in retain_on_make

    second_sw_index := keeps_list.Count - 1;

    slip_road_pos2 := keeps_list[second_sw_index].snap_peg_positions.ctrl_planing_pos;
    // location of end of planing on pad

    gocalc(0, 0);

    // calculate slip road to fit ...

    x1 := slip_road_pos1.notch_x;
    y1 := slip_road_pos1.notch_y;

    x2 := slip_road_pos2.notch_x;
    y2 := slip_road_pos2.notch_y;

    temp := SQR(x2 - x1) + SQR(y2 - y1);

    if ABS(temp) > minfp then
      slip_chord := SQRT(temp)
    else
      EXIT;                      // ????

    pad_form.reset_peg_menu_entry.Enabled := True;
    pad_form.reset_peg_menu_entry.Click;           //peg now on CTRL-0

    gocalc(0, 0);

    swap_end_for_end;     // peg back on switch, now CTRL-0

    xorg := slip_chord;     // length = chord-length
    turnoutx := xorg;

    gocalc(0, 0);

    // get location of opposite end  x3,y3

    normalize_transforms;
    docurving(True, True, turnoutx, g / 2, x3, temp, dummy1, dummy2);
    // get pad location data for end.
    y3 := temp * hand_i + y_datum;

    temp := SQR(x3 - x1) + SQR(y3 - y1);

    if temp > minfp then
      slip_offset := ABS(SQRT(temp))       // curving correction need
    else
      slip_offset := 0;                     // ???

    slip_angle := 2 * ARCSIN(slip_offset / 2 / turnoutx);   // between now and chord

    slip_turn := ABS(2 * slip_angle);   // angle turned along slip road

    normalize_angle(slip_turn);

    try
      slip_rad := ABS(turnoutx / 2 / SIN(slip_angle));
    except
      slip_rad := max_rad;   // straight
    end;//try

    gocalc(0, 0);

    slip_arc_len := ABS(slip_rad * slip_turn);

    xorg := slip_arc_len;
    turnoutx := xorg;

    gocalc(0, 0);

    // try both +ve and -ve rads to see which fits ...

    controlTemplate.curve.fixedRadius := slip_rad;      // first +ve

    gocalc(0, 0);

    // get location of end  x3,y3

    normalize_transforms;
    docurving(True, True, turnoutx, g / 2, x3, temp, dummy1, dummy2);
    // get pad location data for end.
    y3 := temp * hand_i + y_datum;

    offset_pos_sq := SQR(x3 - x1) + SQR(y3 - y1);


    controlTemplate.curve.fixedRadius := 0 - slip_rad;      // -ve

    gocalc(0, 0);

    // get location of end  x3,y3

    normalize_transforms;
    docurving(True, True, turnoutx, g / 2, x3, temp, dummy1, dummy2);
    // get pad location data for end.
    y3 := temp * hand_i + y_datum;

    offset_neg_sq := SQR(x3 - x1) + SQR(y3 - y1);

    if offset_pos_sq < offset_neg_sq       // use smallest
    then
      controlTemplate.curve.fixedRadius := slip_rad;

    gocalc(0, 0);

    if doing_2nd_side = True
    // get rads for end report
    then
      slip_rad2_str := '||        ' + round_str(ABS(controlTemplate.curve.fixedRadius), 0) + ' mm   ( ' +
        round_str(ABS(controlTemplate.curve.fixedRadius) / 25.4, 1) + '" )'
    else
      slip_rad1_str := '||        ' + round_str(ABS(controlTemplate.curve.fixedRadius), 0) + ' mm   ( ' + round_str(
        ABS(controlTemplate.curve.fixedRadius) / 25.4, 1) + '" )';

    store_and_background(False, False);    // slip road stored

    if (sides = 0) and (doing_2nd_side = False)
    // double-slip, now for the opposite side if not already done...

    // delete the half-diamonds and start again with the opposite one in the control template

    then begin

      list_position := first_bgnd_template;
      delete_keep(False, False);

      clicked_keep_index := first_bgnd_template + 1;
      delete_to_current_popup_entry_click(True);

      doing_2nd_side := True;

      goto 111;
    end;

    // now ensure both half-diamonds are above slip switches ...

    max_index := keeps_list.Count - 1;

    if (first_bgnd_template < max_index) and (max_index > 1)   // if not, why not ???
    then begin

      if keeps_list[max_index - 2].bgnd_half_diamond =
        True   // if not, why not ???
      then begin
        keeps_list.Exchange(first_bgnd_template + 1, max_index - 2);
      end;
    end;

    // ok for single slip, now again for double ...

    if ((max_index - first_bgnd_template) > 4) and (max_index > 3)   // must be double slip
    then begin
      if keeps_list[max_index - 4].bgnd_half_diamond =
        True   // if not, why not ???
      then begin
        keeps_list.Exchange(first_bgnd_template, max_index - 4);
      end;
    end;

    do_template_names;

    pad_form.reset_notch_menu_entry.Enabled := True;     // tidy up the notch
    pad_form.reset_notch_menu_entry.Click;

    redraw(False);

    if sides = 0 then begin
      if alert(7, '    make  double-slip  ' + xover_str,
        '||Is this the required double-slip ' + xover_str + '?'
        + '||The crossing angle is:  1:' + FormatFloat(
        '#.##', k3n) + '||The slip road radii are:' + slip_rad1_str +
        slip_rad2_str +
        '|||green_panel_begintree.gif  If you answer yes, the original control template will be moved to a new position. This is for your convenience should you need to refer to it again.' + '||If you answer no, the original control template will be restored.green_panel_end', '', '', '', '', 'no  -  cancel  double-slip', 'yes  -  continue', 0) = 5 then begin
        delete_slip;
        restore_current;
        redraw(False);

        if (making_crossover = True) and (keeps_list.Count > 0)
        // delete the first crossover turnout too..
        then begin
          list_position := keeps_list.Count - 1;
          delete_keep(False, False);
        end;
        EXIT;
      end;
    end
    else begin
      if alert(7, '    make  single-slip  ' + xover_str,
        '||Is this the required single-slip ' + xover_str + '?'
        + '||The crossing angle is:  1:' + FormatFloat(
        '#.##', k3n) + '||The slip road radius is:' + slip_rad1_str +
        slip_rad2_str +
        '|||green_panel_begintree.gif  If you answer yes, the original control template will be moved to a new position. This is for your convenience should you need to refer to it again.' + '||If you answer no, the original control template will be restored.green_panel_end', '', '', '', '', 'no  -  cancel  single-slip', 'yes  -  continue', 0) = 5 then begin
        delete_slip;
        restore_current;
        redraw(False);

        if (making_crossover = True) and (keeps_list.Count > 0)
        // delete the first crossover turnout too..
        then begin
          list_position := keeps_list.Count - 1;
          delete_keep(False, False);
        end;
        EXIT;
      end;
    end;

    restore_current;
    gocalc(0, 0);

    xshift := xshift - 7 * g;
    yshift := yshift - 7 * g * hand_i;      // move it down the pad and left. 7*g arbitrary

    redraw(False);

  finally
    savedControl.Free;

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    redraw(True);
  end;//try

  save_done := False;
  backup_wanted := True;

  Result := True;
end;
//______________________________________________________________________________

procedure Tmake_slip_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(make_slip_form);

  AutoScroll := False;
end;
//______________________________________________________________________________

procedure Tmake_slip_form.Button1Click(Sender: TObject);

begin
  toolbars_2rows := False;
  do_toolbars;
end;
//______________________________________________________________________________

procedure Tmake_slip_form.Button2Click(Sender: TObject);

begin
  toolbars_2rows := True;
  do_toolbars;
end;
//______________________________________________________________________________

procedure Tmake_slip_form.FormChangeBounds(Sender: TObject);
begin
  do_toolbars;
end;
//______________________________________________________________________________

end.
