
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
unit create_tandem;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Math;

type
  Ttandem_form = class(TForm)
    go_button: TButton;
    xing_type_groupbox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    regular_xing_radio: TRadioButton;
    curviform_xing_radio: TRadioButton;
    cancel_button: TButton;
    datestamp_label: TLabel;
    click_first_button: TButton;
    Label3: TLabel;
    Label4: TLabel;
    comprised_label: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    derived_label: TLabel;
    ds_label: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure click_first_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  tandem_form: Ttandem_form;

  swbegin_for_tandem: double = 0;
  // x to S1 timber on current control template        set in switchtimbers (math_unit)
  swend_for_tandem: double = 0;     // x to last S timber on current control template

  creating_tandem: boolean = False;

function create_double_sided_tandem(curviform, jump_forward: boolean): boolean;

function create_single_sided_tandem(jump_forward: boolean): boolean;


implementation

{$BOOLEVAL ON}

{$R *.lfm}

uses
  point_ex, pad_unit, math_unit, math2_unit, keep_select, alert_unit, info_unit,
  control_room, shove_timber,
  switch_select, { OT-FIRST web_browser_unit,} wait_message, help_sheet,
  shoved_timber, template;

var
  saved_current: Ttemplate_info;
  saved_name_str: string;
  saved_memo_str: string;

  trad_1st: double;

  first_atx: double;
  first_fpx: double;
  first_torgx, first_torgy, first_trad: double;

  first_heel_notch, first_mid_notch, first_xing_notch, far_notch: Tnotch;

//______________________________________________________________________________

function create_double_sided_tandem(curviform, jump_forward: boolean): boolean;

  // jump_forward = second template already exists in control template

var
  i, n: integer;

  equivalent_rad: double;

  stagger: double;
  new_atx: double;

  check_count: integer;

  trad_2nd: double;

  second_heel_notch, second_mid_notch, second_xing_notch: Tnotch;

  knuckle_notch, isol_notch: Tnotch;

  upto, onfrom: double;

  p1_1, p2_1, p3_1, centre_1: Tpex;
  p1_2, p2_2, p3_2, centre_2: Tpex;

  qx1, qy1, k1_r1, k1_r2, qx2, qy2, k2_r1, k2_r2: double;

  a1, a2, a3, k_v, k_s: double;

  temp_notch_v, temp_notch_s: Tnotch;

  k3n_v, k3n_s: double;

  modsw: integer;

  gaunt_offset: double;

  k3n_1, k3n_2, k3n_3: double;

  first_bgnd_template: integer;

  left_rad, middle_rad, right_rad: double;
  left_rad_ins, right_rad_ins, middle_rad_ins: double;
  left_rad_str, middle_rad_str, right_rad_str: string;

  curvi_str: string;

  try_gaunt: boolean;

  rcurv_heel: double;
  dummy_notch1, dummy_notch2, dummy_notch3: Tnotch;


  ////////////////////////////////////////////////////////////////

  procedure restore_current;

  begin
    copy_keep(saved_current);
    // retrieve saved original control template.
    current_name_str := saved_name_str;
    current_memo_str := saved_memo_str;

    info_form.ref_name_label.Caption := current_name_str;
  end;
  ////////////////////////////////////////////////////////////////

  procedure do_template_names;

  var
    z: integer;
    id_str, str: string;

  begin
    id_str := '[tandem ' + FormatDateTime('hhmmss', Time) + ']';

    if keeps_list.Count > first_bgnd_template then begin
      for z := first_bgnd_template to keeps_list.Count - 1 do begin

        if keeps_list[z].bgnd_gaunt =
          True then
          str := 'middle V-crossing'
        else
          str := 'turnout';

        keeps_list[z].template_info.keep_dims.box_dims1.reference_string := id_str + '  ' + str;
      end;//next
    end;
  end;
  ////////////////////////////////////////////////////////////////

  procedure delete_tandem;

  begin
    if keeps_list.Count > first_bgnd_template then
      repeat
        list_position := keeps_list.Count - 1;
        delete_keep(False, False);
      until keeps_list.Count = first_bgnd_template;

    //tandem_in_progress_id_str:='';

    show_and_redraw(True, True);
  end;
  ////////////////////////////////////////////////////////////////

  function get_k3n(increment: double): boolean;
    // get xing angle to match intersection of turnout curves

    // this incremental method provides better results than direct calc from the curves

    // a3 is distance from the end of the vee splice (bnox peg position 285) to the intersection of the outer rail edges (temp_notch_s)
    // increment k3n xing angle and recalc until a3 distance ceases to reduce

    // enter with very short k3n (e.g. 1.5) and starting increment (e.g. 0.1) and initial a3
    // and FP already on intersection of rail gauge faces

  var
    old_a3: double;

  begin
    Result := False;     // init

    old_a3 := 0;         // keep compiler happy
    check_count := 0;    // safety loop escape

    try
      repeat
        Inc(check_count);
        if (check_count > 500) or (k3n > 50) then
          BREAK;

        k3n := k3n + increment;
        k3 := ARCTAN(1 / k3n);

        equivalent_rad := ABS(equiv_rad_calc(trad_2nd, clrad1)) + g / 2;
        gaunt_offset := equivalent_rad * COS(k3) - equivalent_rad + g;
        gaunt_offset_in := gaunt_offset / inscale;

        gocalc(0, 0);

        shift_onto_notch(True, True);

        turnoutx := atx + 2 * 30 * inscale;  // to centre C timber

        old_a3 := a3;

        a3 := get_notch_distance(temp_notch_s, get_snap_peg_xy_data(285));

      until a3 >= old_a3;

      k3n := k3n - 2 * increment;      // jump back far enough to try again

    except
      EXIT;
    end;//try

    Result := True;
  end;
  ////////////////////////////////////////////////////////////////

  procedure re_do_for_k3n;

  begin
    try
      k3 := ARCTAN(1 / k3n);
      gocalc(0, 0);
      equivalent_rad := ABS(equiv_rad_calc(trad_2nd, clrad1)) + g / 2;
      gaunt_offset := equivalent_rad * COS(k3) - equivalent_rad + g;
      gaunt_offset_in := gaunt_offset / inscale;
      gocalc(0, 0);

      shift_onto_notch(True, True);
      gocalc(0, 0);

      a3 := get_notch_distance(temp_notch_s, get_snap_peg_xy_data(285));
    except
      EXIT;
    end;//try
  end;
  ////////////////////////////////////////////////////////////////

begin
  Result := False;  // init

  if jump_forward = False        // not re-entering
  then begin                // create first 2 turnouts ...

    first_bgnd_template := keeps_list.Count - 1;     // safety init in case of delete tandem

    pad_form.ds_tandem_begin_menu_entry.Enabled := False;
    // no re-entry until we are ready ..
    pad_form.ds_tandem_continue_menu_entry.Enabled := False;

    pad_form.ss_tandem_begin_menu_entry.Enabled := False;
    // no re-entry until we are ready ..
    pad_form.ss_tandem_continue_menu_entry.Enabled := False;

    saved_current.keep_shove_list := Tshoved_timber_list.Create;

    fill_kd(saved_current);                              // save existing control template
    saved_name_str := current_name_str;
    saved_memo_str := current_memo_str;

    pad_form.reset_peg_menu_entry.Enabled := True;
    pad_form.reset_peg_menu_entry.Click;                 // peg on CTRL-0

    gocalc(0, 0);

    if turnoutx = 0 then
      extend_template_from_zero;

    gocalc(0, 0);

    pad_form.square_on_menu_entry.Click;
    // square-on timbers for a double-sided tandem
    pad_form.timbers_in_line_menu_entry.Click;  // in-line ends for tandem_timb modifier

    tandem_timb := 1;    // extended MS timbering for the first turnout

    omit_wj_marks := True;    // no wing rail joints

    stagger := swend_for_tandem - swbegin_for_tandem;
    // globals set in switchtimbers;

    if csi.group_code = 1 then
      stagger := stagger + 0.5 * inscale;
    // straight switch, correct stagger for 4" toe to S1 instead of REA 3.5"

    if turnoutx < (mvjpx + stagger) then
      turnoutx := mvjpx + stagger;
    // lengthen if nec. to allow exit track space for the second turnout
    turnout_i := 1;
    // length locked at turnoutx.

    include_closure_timbers := False;
    main_road_stock_rail_flag := False;
    do_railedges;

    exittb_i := 0;  // no exit sleepers

    gocalc(0, 0);

    first_atx := atx;             // x to "A" timber centre

    trad_1st := calculate_turnout_radius(clrad1, first_heel_notch,
      first_mid_notch, first_xing_notch);
    // get turnout rad and 3 points on it     globals for re-entry

    store_and_background(False, False);     // first turnout  copy to background.

    first_bgnd_template := keeps_list.Count - 1;     // index to it

    keeps_list[first_bgnd_template].this_is_tandem_first := True;
    // find it on re-entry

    // now create 2nd turnout ...

    if retpar_i = 1 then begin
      turnout_road_i := 0;
      // changing from parallel, assume long turnout road not now needed.
      retpar_i := 0;
      pad_form.snap_exit_to_return_curve_menu_entry.Enabled := False;
    end;

    if curviform = True             // he wants curviform xing
    then begin
      xing_type_i := 1;
      curvi_str := ' RAM curviform.';
    end
    else begin
      xing_type_i := 0;
      entry_straight_code := 0;             // auto fit
      curvi_str := ' RAM regular.';
    end;

    if csi.size_code < 1 then
      modsw := 1  // ??? should never be zero
    else begin
      if (clrad1 < 0) and (clrad1 > (0 - 200 * g)) and (csi.size_code = 1)
      // "A" switch with contraflexure sharper than about 12ft radius in 4mm/ft scale  (will be swapping to +ve curving)
      then
        modsw := 1
      else
        modsw := 0;
    end;

    if set_csi_data(9, Round(MIN((csi.size_code + modsw), 3))) = False
    // set a tandem switch of the same size, or 1:32, or max 1:40.
    then begin
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    approach_rails_only := True;

    include_front_timbers := False;     // no J joint timbers
    include_switch_timbers := True;
    include_closure_timbers := False;

    tandem_timb := 2;               // extended for the second turnout

    exittb_i := 2;       // exit sleepers back on

    retain_on_make;
    // do blanking, shoves, diffs, crossing entry straight, cancel platforms  213a

    xorg := xorg + stagger;    // add approach track (needed for stock rail)

    invert_handing;

    gocalc(0, 0);

    new_atx := first_atx + 5 * 30 * inscale;  // to first timber F

    creating_tandem := True;   // prevent pad warning and modify calcs

    k3n := 1.99;         // init very short turnout
    gocalc(0, 0);
    if atx >= new_atx then begin
      ShowMessage(
        'Sorry, it is not possible to create a tandem turnout from the current starting turnout.');
      delete_tandem;
      restore_current;

      creating_tandem := False;

      //tandem_in_progress_id_str:='';    // tandem finished

      saved_current.keep_shove_list.Free;

      pad_form.ds_tandem_begin_menu_entry.Enabled := True;
      // so can start a new one later
      pad_form.ss_tandem_begin_menu_entry.Enabled := True;
      // so can start a new one later

      pad_form.reset_notch_menu_entry.Enabled := True;
      pad_form.reset_notch_menu_entry.Click;

      redraw(True);
      EXIT;
    end;

    wait_form.waiting_label.Caption := 'please  wait ...';
    wait_form.waiting_label.Width :=
      wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // bug fix for Wine

    wait_form.wait_progressbar.Visible := False;
    wait_form.cancel_button.Visible := False;
    wait_form.Show;

    Application.ProcessMessages;

    check_count := 0;    // safety loop escape

    repeat             // increase xing angle until first F timber is passed
      if check_count > 5000 then
        BREAK;
      k3n := k3n + 0.01;
      gocalc(0, 0);
      Inc(check_count);
    until atx > new_atx;

    repeat             // reduce xing angle until first F timber is not reached
      if check_count > 5000 then
        BREAK;
      k3n := k3n - 0.001;
      gocalc(0, 0);
      Inc(check_count);
    until atx < new_atx;

    repeat             // increase xing angle again until first F timber is reached or passed
      if check_count > 5000 then
        BREAK;
      k3n := k3n + 0.0001;
      gocalc(0, 0);
      Inc(check_count);
    until atx >= new_atx;

    wait_form.Hide;

    Application.ProcessMessages;

    show_and_redraw(False, False);

    rcurv_heel := calculate_turnout_radius(clrad1, dummy_notch1, dummy_notch2, dummy_notch3);

    repeat
      i := alert(4, '    make  3-way  double-sided  tandem  turnout',
        'Is the size of this new 2nd turnout satisfactory ?'
        + '||(ignore the timbering)' +
        '||The V-crossing angle is  1:' + round_str(k3n, 2) + curvi_str +
        '||The turnout-road radius is  ' + rad_str(rcurv_heel, 2) + ' mm',
        '', '', 'more  information',
        'no  -  I  want  to  adjust  the  new  2nd  turnout', 'no  -  cancel  tandem',
        'yes  -  create  tandem', 3);

      if i = 3 then
        go_to_templot_companion_page('3_way_tandem_turnouts.php');

      if i = 4 then begin
        if help(0, '    `0Adjusting the new 2nd turnout`9||'
          + '    `0in a double-sided tandem`9'
          +
          '||You may want to adjust the radius of the new 2nd turnout for a better fit to your track plan (use `0F9`2 mouse action), or change the type of V-crossing, or change the switch size.' + ' Do not make changes to the main-road alignment.' + '||Be aware that if you change it too much it may not be possible to create a viable tandem turnout, or you may get unexpected results.' + '||If you create additional track plan templates from it, you must delete it back into the control template before continuing to create the tandem turnout.' + ' For those new templates you will need to click the `0tools > make 3-way tandem turnout > reset derived partial template`1 menu item.' + '||Do not delete or make any changes to the original 1st turnout until the tandem has been completed.' + '||When you are ready to continue creating the tandem, click the|`0tools > make 3-way tandem turnout > ...continue`1 menu item.' + '||Or if you have changed your mind about making adjustments, click the bar below.', 'create  tandem  with  new  2nd  turnout  as  it  stands') <> 1 then begin
          pad_form.ds_tandem_continue_menu_entry.Enabled := True;
          EXIT;
        end;
      end;

      if i = 5 then begin
        delete_tandem;
        restore_current;

        creating_tandem := False;

        //tandem_in_progress_id_str:='';    // tandem finished

        saved_current.keep_shove_list.Free;

        pad_form.ds_tandem_begin_menu_entry.Enabled := True;
        // so can start a new one later
        pad_form.ss_tandem_begin_menu_entry.Enabled := True;
        // so can start a new one later

        pad_form.reset_notch_menu_entry.Enabled := True;
        pad_form.reset_notch_menu_entry.Click;

        redraw(True);
        EXIT;
      end;

    until i <> 3;

  end;//no jump forward


  // possible re-entry here

  pad_form.ds_tandem_continue_menu_entry.Enabled := False;     // only once, or try again

  first_bgnd_template := -1;   // safety re-init

  if keeps_list.Count > 0 then begin
    for n := 0 to (keeps_list.Count - 1) do begin
      if keeps_list[n].this_is_tandem_first = True then begin
        first_bgnd_template := n;
        BREAK;
      end;
    end;//next
  end;

  if first_bgnd_template = -1 then begin
    ShowMessage('error: The original tandem turnout is not found in the storage box.'
      + #13 + #13 + 'Have you selected the wrong template to continue?'
      + #13 + #13 +
      'In order to continue a tandem turnout, the original turnout should be on the background,'
      + #13 + 'and the new turnout in the control template.'
      + #13 + #13 +
      'Sorry, it will be necessary to delete this tandem turnout and start again.');

    creating_tandem := False;

    //tandem_in_progress_id_str:='';    // tandem finished

    saved_current.keep_shove_list.Free;

    pad_form.ds_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later
    pad_form.ss_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    redraw(True);
    EXIT;
  end;

  list_position := first_bgnd_template;
  // move first template to bottom to keep them contiguous
  keep_form.move_to_bottom_button.Click;
  first_bgnd_template := keeps_list.Count - 1;

  keeps_list[first_bgnd_template].this_is_tandem_first := False;
  // to allow further tandems

  gocalc(0, 0);

  trad_2nd := calculate_turnout_radius(clrad1, second_heel_notch, second_mid_notch,
    second_xing_notch);
  // get turnout rad and 3 points on it

  if hand_i = 1 then begin
    left_rad := trad_2nd;
    right_rad := trad_1st;
  end
  else begin
    left_rad := trad_1st;
    right_rad := trad_2nd;
  end;

  middle_rad := clrad1;

  left_rad_ins := ABS(left_rad / 25.4);
  right_rad_ins := ABS(right_rad / 25.4);
  middle_rad_ins := ABS(middle_rad / 25.4);

  left_rad_str := rad_str(left_rad, 0);
  middle_rad_str := rad_str(middle_rad, 0);
  right_rad_str := rad_str(right_rad, 0);

  if left_rad_str <> 'straight' then
    left_rad_str := left_rad_str + ' mm  ( ' + rad_str(left_rad_ins, 1) + ' " )';
  if middle_rad_str <> 'straight' then
    middle_rad_str := middle_rad_str + ' mm  ( ' + rad_str(middle_rad_ins, 1) + ' " )';
  if right_rad_str <> 'straight' then
    right_rad_str := right_rad_str + ' mm  ( ' + rad_str(right_rad_ins, 1) + ' " )';

  store_and_background(False, False);     // second turnout  copy to background.

  //second_bgnd_template:=keeps_list.Count-1;

  // find arc centres on pad ...

  p1_1 := tpex_from_tnotch(first_heel_notch);
  p2_1 := tpex_from_tnotch(first_mid_notch);
  p3_1 := tpex_from_tnotch(first_xing_notch);

  p1_2 := tpex_from_tnotch(second_heel_notch);
  p2_2 := tpex_from_tnotch(second_mid_notch);
  p3_2 := tpex_from_tnotch(second_xing_notch);

  try

    if get_arc_centre(p1_1, p2_1, p3_1, centre_1) = False then begin
      ShowMessage('Sorry, it has not been possible to create a tandem turnout.');
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    if get_arc_centre(p1_2, p2_2, p3_2, centre_2) = False then begin
      ShowMessage('Sorry, it has not been possible to create a tandem turnout.');
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    if get_circle_intersections(centre_1.x, centre_1.y, ABS(trad_1st + g / 2),
      centre_2.x, centre_2.y, ABS(trad_2nd + g / 2), qx1, qy1, k1_r1, k1_r2,
      qx2, qy2, k2_r1, k2_r2) <> 2
    // return code: 2 = OK, two usable intersections
    then begin
      ShowMessage('Sorry, it has not been possible to create a tandem turnout.');  // debug
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    // choose intersection nearer to first turnout xing ...

    a1 := SQRT(SQR(p3_1.x - qx1) + SQR(p3_1.y - qy1));
    a2 := SQRT(SQR(p3_1.x - qx2) + SQR(p3_1.y - qy2));

    if a1 < a2 then begin
      with temp_notch_v do begin
        notch_x := qx1;
        notch_y := qy1;
        notch_k := k1_r1 - Pi / 2;
      end;//with
    end
    else begin
      with temp_notch_v do begin
        notch_x := qx2;
        notch_y := qy2;
        notch_k := k2_r1 - Pi / 2;
      end;//with
    end;

    normalize_angle(temp_notch_v.notch_k);
    new_notch(temp_notch_v, False);             // put the notch there, and don't link group

    // repeat with outer edge of rails, to get required location of end of vee splice,  rail-width j ...

    // move out by j/2 and repeat ...

    if get_circle_intersections(centre_1.x, centre_1.y, ABS(trad_1st + g / 2 + j),
      centre_2.x, centre_2.y, ABS(trad_2nd + g / 2 + j), qx1, qy1, k1_r1, k1_r2,
      qx2, qy2, k2_r1, k2_r2) <> 2 // return code: 2 = OK, two usable intersections
    then begin
      ShowMessage('Sorry, it has not been possible to create a tandem turnout.');
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    // choose intersection nearer to first turnout xing ...

    a1 := SQRT(SQR(p3_1.x - qx1) + SQR(p3_1.y - qy1));
    a2 := SQRT(SQR(p3_1.x - qx2) + SQR(p3_1.y - qy2));

    if a1 < a2 then begin
      with temp_notch_s do begin
        notch_x := qx1;
        notch_y := qy1;
        notch_k := k1_r1 - Pi / 2;
      end;//with
    end
    else begin
      with temp_notch_s do begin
        notch_x := qx2;
        notch_y := qy2;
        notch_k := k2_r1 - Pi / 2;
      end;//with
    end;

    tandem_timb := 0;               // reset normal for middle xing

    omit_wj_marks := True;    // no wing rail joints

    fix_radius(0 - trad_1st, False);
    // main road curving for middle xing  (double-sided tandem)

    gocalc(0, 0);

    convert_to_or_from_gaunt(True);
    // to curviform gaunt   hand is already correct to align over the first turnout
    xing_type_i := 1;
    retpar_i := 0;

    gocalc(0, 0);

    isolated_crossing := False; // init
    pad_form.isolate_crossing_menu_entry.Click;

    pad_form.equalized_incremental_menu_entry.Click;

    pad_form.peg_on_fp_menu_entry.Click;

    gocalc(0, 0);

    shift_onto_notch(True, True);

    gocalc(0, 0);

    wait_form.waiting_label.Caption := 'please  wait ...';
    wait_form.waiting_label.Width := wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);
    // bug fix for Wine

    wait_form.wait_progressbar.Visible := False;
    wait_form.cancel_button.Visible := False;
    wait_form.Show;

    Application.ProcessMessages;

    // find xing angle k3n which fits at FP and at splice ...

    k3n := 1.5;               // minimum init
    re_do_for_k3n;

    try_gaunt := get_k3n(0.1);

    if try_gaunt = True then begin
      re_do_for_k3n;
      try_gaunt := get_k3n(0.01);
    end;

    if try_gaunt = True then begin
      re_do_for_k3n;
      try_gaunt := get_k3n(0.001);
    end;

    if try_gaunt = True then begin
      re_do_for_k3n;
      try_gaunt := get_k3n(0.0001);
    end;

    if try_gaunt = True then
      re_do_for_k3n      // one last time
    else begin              // or no joy
      wait_form.Hide;

      Application.ProcessMessages;

      ShowMessage(
        'Sorry, it has not been possible to create a double-sided tandem turnout from the current starting turnout.'
        + #13 + #13 + 'It may be possible to create a tandem turnout manually.');

      delete_tandem;
      restore_current;
      EXIT;
    end;

    wait_form.Hide;

    Application.ProcessMessages;

    pad_form.peg_on_mcp_menu_entry.Click;   // move peg to centre-line MCP
    gocalc(0, 0);

    k3n_3 := k3n;  // for final report

    store_and_background(False, False);     // middle crossing  copy to background.

    knuckle_notch := get_snap_peg_xy_data(279);  // start of knuckle bend radius
    isol_notch := get_snap_peg_xy_data(260);     // MMINP peg position

    // create gaps in partial templates ...

    clicked_keep_index := first_bgnd_template;
    // get first turnout back (discard middle xing in control)
    delete_to_current_popup_entry_click(True);

    turnout_road_stock_rail_flag := True;
    turnout_road_check_rail_flag := True;
    crossing_vee_flag := True;
    main_road_check_rail_flag := True;
    main_road_crossing_rail_flag := True;

    main_road_stock_rail_flag := False;
    turnout_road_crossing_rail_flag := False;

    do_railedges;

    omit_wj_marks := True;    // no wing rail joints

    startx := 0;    // no blanking
    gocalc(0, 0);
    store_and_background(False, False);     // put first back without crossing rail

    // swap them ...

    turnout_road_stock_rail_flag := False;
    turnout_road_check_rail_flag := False;
    crossing_vee_flag := False;
    main_road_check_rail_flag := False;
    main_road_crossing_rail_flag := False;
    main_road_stock_rail_flag := False;

    turnout_road_crossing_rail_flag := True;

    do_railedges;

    no_timbering := True;
    pad_form.no_track_centre_lines_menu_radio.Click;
    gocalc(0, 0);

    upto := get_notch_distance(first_heel_notch, knuckle_notch);
    onfrom := get_notch_distance(first_heel_notch, isol_notch);

    if clrad1 < 0 then
      onfrom := onfrom - inscale;   // arbitrary to fill gaps if contraflexure    trial and error

    startx := heelx + onfrom;   // blank up to MMINP
    gocalc(0, 0);
    store_and_background(False, False);
    // put first back again with only crossing rail, far end

    startx := 0;

    omit_wj_marks := True;    // no wing rail joints

    turnoutx := heelx + upto;
    gocalc(0, 0);
    store_and_background(False, False);
    // put first back again with only crossing rail, near end

    k3n_1 := k3n;  // for final report

    // and the second turnout (now at the first index) ...

    clicked_keep_index := first_bgnd_template;
    // get first turnout back (discard middle xing in control)
    delete_to_current_popup_entry_click(True);

    turnout_road_stock_rail_flag := True;
    turnout_road_check_rail_flag := True;
    crossing_vee_flag := True;
    main_road_check_rail_flag := True;
    main_road_crossing_rail_flag := True;

    main_road_stock_rail_flag := False;
    turnout_road_crossing_rail_flag := False;

    do_railedges;

    startx := 0;    // no blanking

    omit_wj_marks := True;    // no wing rail joints

    gocalc(0, 0);
    store_and_background(False, False);     // put first back without crossing rail

    // swap them ...

    turnout_road_stock_rail_flag := False;
    turnout_road_check_rail_flag := False;
    crossing_vee_flag := False;
    main_road_check_rail_flag := False;
    main_road_crossing_rail_flag := False;
    main_road_stock_rail_flag := False;

    turnout_road_crossing_rail_flag := True;

    do_railedges;

    no_timbering := True;
    pad_form.no_track_centre_lines_menu_radio.Click;
    gocalc(0, 0);

    upto := get_notch_distance(second_heel_notch, knuckle_notch);
    onfrom := get_notch_distance(second_heel_notch, isol_notch);

    if clrad1 < 0 then
      onfrom := onfrom - inscale;   // arbitrary to fill gaps if contraflexure    trial and error

    startx := heelx + onfrom;   // blank up to MMINP

    omit_wj_marks := True;    // no wing rail joints

    gocalc(0, 0);
    store_and_background(False, False);
    // put second back again with only crossing rail, far end

    startx := 0;

    omit_wj_marks := True;    // no wing rail joints

    turnoutx := heelx + upto;
    gocalc(0, 0);

    k3n_2 := k3n;  // for final report

    pad_form.make_branch_track_menu_item.Click;
    // put second back again with only crossing rail, near end,   and make branch track from it in the control
    pad_form.reset_tandem_turnout_menu_entry.Click;  // remove partial settings

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    show_and_redraw(True, True);

    redraw(False);

    do_template_names;

    if alert(4, '    make  3-way  tandem  turnout',
      '||Is this the required tandem turnout ?' +
      '||V-crossing angles are (RAM) :  1:' + round_str(k3n_3, 2) + '   1:' + round_str(
      k3n_1, 2) + '   1:' + round_str(k3n_2, 2) + '|||Left-side turnout radius :   ' +
      left_rad_str + '||Middle-road radius :         ' + middle_rad_str
      + '||Right-side turnout radius :  ' + right_rad_str,
      '', '', '', '', 'no  -  cancel  tandem  turnout', 'yes  -  continue', 0) = 5 then begin
      delete_tandem;
      restore_current;
      Result := False;
    end
    else begin

      if help(0, '   `0Tandem  Turnout`9' +
        '||Before you print this tandem turnout for construction, it needs to be finished by removing any conflicts.'
        + '||Check that the check rail and wing rail ends are not obstructing a running clearance for wheel flanges. They can be shortened or modified by clicking the `0real > adjust check rails...`1 menu item.' + '||If check rail or wing rail ends are overlapping they can be straightened or combined into one rail by clicking the `0real > adjust check rails...`1 menu item.' + '||Some timbers may need to be lengthened or shortened, and overlapping timbers will need to be omitted. Click the `0real > shove timbers...`1 menu item.' + '||Templot0 cannot make these final adjustments for you because it requires a human eye to see what is needed.' + '||If you are still track planning these final adjustments can be left until later, to avoid wasting your work if you need to make changes to this tandem turnout.' + '||For more information about how to make these adjustments click the bar below.', 'more  information') = 1 then
        pad_form.tandems_click_first_menu_entry.Click;

      save_done := False;
      backup_wanted := True;
      Result := True;
    end;

  finally
    creating_tandem := False;

    saved_current.keep_shove_list.Free;

    pad_form.ds_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later
    pad_form.ss_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    redraw(True);
  end;//try

end;
//______________________________________________________________________________

procedure Ttandem_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=600;
  // OT-FIRST ClientHeight:=330;

  AutoScroll := False;
end;
//______________________________________________________________________________

procedure Ttandem_form.click_first_buttonClick(Sender: TObject);

begin
  go_to_templot_companion_page('3_way_tandem_turnouts.php');
end;
//______________________________________________________________________________

//______________________________________________________________________________

function create_single_sided_tandem(jump_forward: boolean): boolean;

  // jump_forward = 1st turnout template already exists in control template

var
  i, n: integer;

  equivalent_rad: double;

  stagger: double;

  second_fpx: double;
  second_torgx, second_torgy, second_trad: double;

  check_count: integer;

  trad_2nd: double;

  second_end_notch, second_mid_notch, second_xing_notch: Tnotch;

  isol_notch, fpx_notch: Tnotch;

  p1_1, p2_1, p3_1, centre_1: Tpex;
  p1_2, p2_2, p3_2, centre_2: Tpex;

  qx1, qy1, k1_r1, k1_r2, qx2, qy2, k2_r1, k2_r2: double;

  a1, a2, a3, k_v, k_s: double;

  temp_notch_v, temp_notch_s: Tnotch;

  k3n_v, k3n_s: double;

  modsw: integer;

  k3n_1, k3n_2, k3n_3: double;

  first_bgnd_template: integer;

  left_rad, middle_rad, right_rad: double;
  left_rad_ins, right_rad_ins, middle_rad_ins: double;
  left_rad_str, middle_rad_str, right_rad_str: string;

  dummy: double;
  temp_k: double;
  temp_y: double;

  swsize: integer;

  midx, midy: double;
  midsplicex, midsplicey: double;

  old_turnoutx: double;

  gaunt_offset: double;

  try_hd, try_gaunt: boolean;

  rcurv_heel: double;
  dummy_notch1, dummy_notch2, dummy_notch3: Tnotch;


  ////////////////////////////////////////////////////////////////

  procedure restore_current;

  begin
    copy_keep(saved_current);
    // retrieve saved original control template.
    current_name_str := saved_name_str;
    current_memo_str := saved_memo_str;

    info_form.ref_name_label.Caption := current_name_str;
  end;
  ////////////////////////////////////////////////////////////////

  procedure do_template_names;

  var
    z: integer;
    id_str, str: string;

  begin
    id_str := '[tandem ' + FormatDateTime('hhmmss', Time) + ']';

    if keeps_list.Count > first_bgnd_template then begin
      for z := first_bgnd_template to keeps_list.Count - 1 do begin

        if (keeps_list[z].bgnd_half_diamond = True) or
          (keeps_list[z].bgnd_gaunt = True) then
          str := 'V-crossing'
        else
          str := 'turnout';

        keeps_list[z].template_info.keep_dims.box_dims1.reference_string := id_str + '  ' + str;
      end;//next
    end;
  end;
  ////////////////////////////////////////////////////////////////

  procedure delete_tandem;

  begin
    if keeps_list.Count > first_bgnd_template then
      repeat
        list_position := keeps_list.Count - 1;
        delete_keep(False, False);
      until keeps_list.Count = first_bgnd_template;

    //tandem_in_progress_id_str:='';

    show_and_redraw(True, True);
  end;
  ////////////////////////////////////////////////////////////////

  function get_k3n_hd(increment: double): boolean;
    // irregular half-diamond    get xing angle to match intersection of turnout curves

    // this incremental method provides better results than direct calc from the curves

    // a3 is distance from the end of the vee splice (bnox peg position 285) to the intesection of the outer rail edges (temp_notch_s)
    // increment k3n xing angle and recalc until a3 distance ceases to reduce

    // enter with very short k3n (e.g. 1.5) and starting increment (e.g. 0.1) and initial a3
    // and FP already on intersection of rail gauge faces

  var
    old_a3: double;

  begin
    Result := False;     // init

    old_a3 := 0;         // keep compiler happy
    check_count := 0;    // safety loop escape

    try
      repeat
        Inc(check_count);
        if (check_count > 500) or (k3n > 50) then
          EXIT;

        k3n := k3n + increment;
        k3 := ARCTAN(1 / k3n);

        hdk := ARCCOS(COS(k3) + g / equivalent_rad);
        hdkn := 1 / TAN(hdk);
        gocalc(0, 0);

        shift_onto_notch(True, True);

        gocalc(0, 0);

        old_a3 := a3;

        a3 := get_notch_distance(temp_notch_s, get_snap_peg_xy_data(285));

      until a3 >= old_a3;

      k3n := k3n - 2 * increment;      // jump back far enough to try again

    except
      //ShowMessage('exception :  '+floattostr(k3n)+'  '+floattostr(hdkn)+'  '+floattostr(increment)+'  '+floattostr(equivalent_rad));
      EXIT;
    end;//try

    Result := True;
  end;
  ////////////////////////////////////////////////////////////////

  procedure re_do_for_k3n_hd;

  begin
    try
      k3 := ARCTAN(1 / k3n);

      hdk := ARCCOS(COS(k3) + g / equivalent_rad);
      hdkn := 1 / TAN(hdk);
      gocalc(0, 0);

      shift_onto_notch(True, True);
      gocalc(0, 0);

      a3 := get_notch_distance(temp_notch_s, get_snap_peg_xy_data(285));
    except
      //ShowMessage('exception :  '+floattostr(a3));
      EXIT;
    end;//try
  end;
  ////////////////////////////////////////////////////////////////

  function get_k3n_gaunt(increment: double): boolean;
    // gaunt turnout   get xing angle to match intersection of turnout curves

    // this incremental method provides better results than direct calc from the curves

    // a3 is distance from the end of the vee splice (bnox peg position 285) to the intesection of the outer rail edges (temp_notch_s)
    // increment k3n xing angle and recalc until a3 distance ceases to reduce

    // enter with very short k3n (e.g. 1.5) and starting increment (e.g. 0.1) and initial a3
    // and FP already on intersection of rail gauge faces

  var
    old_a3: double;

  begin
    Result := False;     // init

    old_a3 := 0;         // keep compiler happy
    check_count := 0;    // safety loop escape

    try
      repeat
        Inc(check_count);
        if (check_count > 500) or (k3n > 50) then
          EXIT;

        k3n := k3n + increment;
        k3 := ARCTAN(1 / k3n);

        equivalent_rad := ABS(equiv_rad_calc(trad_2nd, clrad1)) + g / 2;
        gaunt_offset := equivalent_rad * COS(k3) - equivalent_rad + g;
        gaunt_offset_in := gaunt_offset / inscale;

        gocalc(0, 0);

        shift_onto_notch(True, True);

        old_a3 := a3;

        a3 := get_notch_distance(temp_notch_s, get_snap_peg_xy_data(285));

      until a3 >= old_a3;

      k3n := k3n - 2 * increment;      // jump back far enough to try again

    except
      EXIT;
    end;//try

    Result := True;
  end;
  ////////////////////////////////////////////////////////////////

  procedure re_do_for_k3n_gaunt;

  begin
    try
      k3 := ARCTAN(1 / k3n);
      gocalc(0, 0);
      equivalent_rad := ABS(equiv_rad_calc(trad_2nd, clrad1)) + g / 2;
      gaunt_offset := equivalent_rad * COS(k3) - equivalent_rad + g;
      gaunt_offset_in := gaunt_offset / inscale;
      gocalc(0, 0);

      shift_onto_notch(True, True);
      gocalc(0, 0);

      a3 := get_notch_distance(temp_notch_s, get_snap_peg_xy_data(285));
    except
      //ShowMessage('exception :  '+floattostr(a3));
      EXIT;
    end;//try

  end;
  ////////////////////////////////////////////////////////////////


  // SINGLE-SIDED = ORIGINAL TURNOUT IS 2ND TANDEM TURNOUT ...

begin
  Result := False;  // init

  if jump_forward = False        // not re-entering
  then begin                // create first 2 turnouts ...

    first_bgnd_template := keeps_list.Count - 1;     // safety init in case of delete tandem

    pad_form.ds_tandem_begin_menu_entry.Enabled := False;
    // no re-entry until we are ready ..
    pad_form.ds_tandem_continue_menu_entry.Enabled := False;

    pad_form.ss_tandem_begin_menu_entry.Enabled := False;
    // no re-entry until we are ready ..
    pad_form.ss_tandem_continue_menu_entry.Enabled := False;


    saved_current.keep_shove_list := Tshoved_timber_list.Create;

    fill_kd(saved_current);                              // save existing control template
    saved_name_str := current_name_str;
    saved_memo_str := current_memo_str;

    pad_form.reset_peg_menu_entry.Enabled := True;
    pad_form.reset_peg_menu_entry.Click;                 // peg on CTRL-0

    gocalc(0, 0);

    if turnoutx = 0 then
      extend_template_from_zero;

    turnout_i := 1;    // length locked at turnoutx.

    include_front_timbers := False;
    include_closure_timbers := False;

    omit_wj_marks := True;    // no wing rail joints

    pad_form.angled_on_menu_entry.Click;
    // angled-on timbers for a single-sided tandem
    pad_form.timbers_in_line_menu_entry.Click;  // in-line ends for tandem_timb modifier

    swsize := csi.size_code;

    if (swsize < 1) or (csi.group_code = 3)
    // ??? should never be zero   GWR flexibles start at "B" 1:32
    then
      modsw := 1
    else begin
      if (clrad1 > 0) and (clrad1 < (200 * g)) and (swsize = 1)
      // "A" switch with similar flexure sharper than about 12ft radius in 4mm/ft scale
      then
        modsw := 1
      else
        modsw := 0;
    end;

    swsize := swsize + modsw;     // switch size for 2nd template (1st turnout)

    // assume straight offset of around 8" at second switch will be sufficiently increased by the effect of the turnout curve (big approximation)
    // don't change these without also changing the timber spacings in the tandem switches ...

    case swsize of
      1:
        stagger := 179 * inscale;    // for 1:24 tandem switch
      2:
        stagger := 259.5 * inscale;  // for 1:32 tandem switch
      else
        stagger := 331 * inscale;    // for 1:40 tandem switch

    end;//case

    // make sure enough approach track for 2nd template (1st turnout) ...

    if xorg < stagger then begin

      pad_form.peg_on_joint_end_menu_entry.Click;   // peg on CTRL-1
      gocalc(0, 0);

      kform_now := kform;
      docurving(True, True, pegx, pegy, now_peg_x, now_peg_y, now_peg_k, dummy);
      // save current peg data for peg_curve calcs.
      gocalc(0, 0);

      turnoutx := turnoutx + (stagger - xorg);
      xorg := stagger;

      gocalc(0, 0);

      peg_curve;  // lock to peg

      gocalc(0, 0);

    end;

    pad_form.reset_peg_menu_entry.Click;  // to CTRL-0
    gocalc(0, 0);

    startx := setx - inscale; // blank up to toe

    tandem_timb := 3;  // extend far end of timbers

    redraw(False);

    first_fpx := fpx;             // x to FP
    first_atx := atx;             // x to "A" timber

    first_torgx := torgx;
    first_torgy := torgy;
    first_trad := tradius - g;

    trad_1st := calculate_turnout_radius(clrad1, first_heel_notch,
      first_mid_notch, first_xing_notch);
    // get turnout rad and 3 points on it     globals for re-entry

    far_notch := get_snap_peg_xy_data(18);  // TVJP

    store_and_background(False, False);     // first turnout  copy to background.

    first_bgnd_template := keeps_list.Count - 1;     // index to it

    keeps_list[first_bgnd_template].this_is_tandem_first := True;
    // find it on re-entry

    // now create 2nd template (1st turnout) ...

    tandem_timb := 0;  // normal length timbers

    startx := 0;        // show approach track
    xing_type_i := 1;   // curviform only

    turnout_road_i := 1;  // long turnout side exit

    include_front_timbers := True;   // back on

    omit_wj_marks := True;    // no wing rail joints

    exittb_i := 0;   // no exit sleepers

    if retpar_i = 1      // cancel parallel xing
    then begin
      retpar_i := 0;
      pad_form.snap_exit_to_return_curve_menu_entry.Enabled := False;
    end;

    if set_csi_data(9, Round(MIN(swsize, 3))) = False
    // set a tandem switch of the same size, or 1:32, or max 1:40.
    then begin
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    xorg := xorg - stagger;
    gocalc(0, 0);

    pad_form.equalized_incremental_menu_entry.Click;  // equalized timbers
    pad_form.timbers_in_line_menu_entry.Click;        // but in-line to match first turnout

    turnoutx := first_atx + 120 * inscale;
    // match first turnout exit  arbitrary temporary to show him
    gocalc(0, 0);

    wait_form.waiting_label.Caption := 'please  wait ...';
    wait_form.waiting_label.Width :=
      wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // bug fix for Wine

    wait_form.wait_progressbar.Visible := False;
    wait_form.cancel_button.Visible := False;
    wait_form.Show;

    Application.ProcessMessages;

    creating_tandem := True;   // prevent pad warning and modify calcs

    check_count := 0;    // safety loop escape

    k3n := 1.99;         // init very short turnout

    midx := 0;  // keep compiler happy

    repeat
      // increase xing angle until intersection is mid-way between xings
      if check_count > 5000 then
        BREAK;
      k3n := k3n + 0.01;
      gocalc(0, 0);

      if (fpx > first_fpx) or
        (get_circle_intersections(first_torgx, first_torgy, ABS(first_trad),
        torgx, torgy, ABS(tradius),
        qx1, qy1, k1_r1, k1_r2, qx2, qy2, k2_r1, k2_r2) <> 2)
      // return code: 2 = OK, two usable intersections
      then begin
        ShowMessage(
          'Sorry, it has not been possible to create a single-sided tandem turnout from the current starting turnout.'
          + #13 + #13 + 'It may be possible to create a tandem turnout manually.');
        delete_tandem;
        restore_current;

        creating_tandem := False;

        //tandem_in_progress_id_str:='';    // tandem finished

        saved_current.keep_shove_list.Free;

        pad_form.ds_tandem_begin_menu_entry.Enabled := True;
        // so can start a new one later
        pad_form.ss_tandem_begin_menu_entry.Enabled := True;
        // so can start a new one later

        pad_form.reset_notch_menu_entry.Enabled := True;
        pad_form.reset_notch_menu_entry.Click;

        redraw(True);
        EXIT;
      end;

      // choose intersection nearer to first turnout xing ...

      a1 := SQRT(SQR(first_fpx - qx1) + SQR(g - qy1));
      a2 := SQRT(SQR(first_fpx - qx2) + SQR(g - qy2));

      if a1 < a2 then begin
        midx := qx1;
        midy := qy1;
      end
      else begin
        midx := qx2;
        midy := qy2;
      end;

      Inc(check_count);

    until midx > ((first_fpx + fpx) / 2);   // gone past mid-way between them, so stop

    wait_form.Hide;

    Application.ProcessMessages;

    show_and_redraw(False, False);

    rcurv_heel := calculate_turnout_radius_beyond(clrad1, dummy_notch1,
      dummy_notch2, dummy_notch3);

    repeat
      i := alert(4, '    make  3-way  single-sided  tandem  turnout',
        'Is the size of this new 1st turnout satisfactory ?'
        + '||(ignore the timbering)' +
        '||The V-crossing angle is  1:' + round_str(k3n, 2) + ' RAM curviform.' +
        '||The turnout-road radius is  ' + rad_str(rcurv_heel, 2) + ' mm',
        '', '', 'more  information',
        'no  -  I  want  to  adjust  the  new  1st  turnout', 'no  -  cancel  tandem',
        'yes  -  create  tandem', 3);

      if i = 3 then
        go_to_templot_companion_page('3_way_tandem_turnouts.php');

      if i = 4 then begin
        if help(0, '    `0Adjusting the new 1st turnout`9||'
          + '    `0in a single-sided tandem`9'
          +
          '||You may want to adjust the radius of the new 1st turnout for a better fit to your track plan. Use the `0F9`2 mouse action to do that.' + '||Do not make any other changes to the new turnout. Be aware that if you change it too much it may not be possible to create a viable tandem turnout, or you may get unexpected results.' + ' The turnout road should intersect the original 2nd turnout between the two V-crossings, and not too close to either of them.' + '||If you create additional track plan templates from the new turnout, you must delete it back into the control template before continuing to create the tandem turnout.' + ' For those new templates you will need to click the `0tools > make 3-way tandem turnout > reset derived partial template`1 menu item.' + '||Do not delete or make any changes to the original 2nd turnout until the tandem has been completed.' + '||When you are ready to continue creating the tandem, click the|`0tools > make 3-way tandem turnout > ...continue`1 menu item.' + '||Or if you have changed your mind about making adjustments, click the bar below.', 'create  tandem  with  new  1st  turnout  as  it  stands') <> 1 then begin
          pad_form.ss_tandem_continue_menu_entry.Enabled := True;
          EXIT;
        end;
      end;

      if i = 5 then begin
        delete_tandem;
        restore_current;

        creating_tandem := False;

        saved_current.keep_shove_list.Free;

        pad_form.ds_tandem_begin_menu_entry.Enabled := True;
        // so can start a new one later
        pad_form.ss_tandem_begin_menu_entry.Enabled := True;
        // so can start a new one later

        pad_form.reset_notch_menu_entry.Enabled := True;
        pad_form.reset_notch_menu_entry.Click;

        redraw(True);
        EXIT;
      end;

    until i <> 3;

  end;//no jump forward


  // possible re-entry here

  pad_form.ss_tandem_continue_menu_entry.Enabled := False;     // only once, or try again

  first_bgnd_template := -1;   // safety re-init

  if keeps_list.Count > 0 then begin
    for n := 0 to (keeps_list.Count - 1) do begin
      if keeps_list[n].this_is_tandem_first = True then begin
        first_bgnd_template := n;
        BREAK;
      end;
    end;//next
  end;

  if first_bgnd_template = -1 then begin
    ShowMessage('error: The original tandem turnout is not found in the storage box.'
      + #13 + #13 + 'Have you selected the wrong template to continue?'
      + #13 + #13 +
      'In order to continue a single-sided tandem turnout, the original 2nd turnout should be unchanged on the background,'
      + #13 + 'and the new 1st turnout should be in the control template.'
      + #13 + #13 +
      'Sorry, it will be necessary to delete this tandem turnout and start again.');

    creating_tandem := False;

    saved_current.keep_shove_list.Free;

    pad_form.ds_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later
    pad_form.ss_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    redraw(True);
    EXIT;
  end;

  list_position := first_bgnd_template;
  // move first template to bottom to keep them contiguous
  keep_form.move_to_bottom_button.Click;
  first_bgnd_template := keeps_list.Count - 1;

  keeps_list[first_bgnd_template].this_is_tandem_first := False;
  // to allow further tandems

  // get midx again in case he moved the crossing on the control template ...

  if (fpx > first_fpx) or (get_circle_intersections(first_torgx, first_torgy, ABS(
    first_trad), torgx, torgy, ABS(tradius), qx1, qy1, k1_r1, k1_r2, qx2, qy2, k2_r1, k2_r2) <> 2)
  // return code: 2 = OK, two usable intersections
  then begin
    ShowMessage(
      'Sorry, it has not been possible to create a single-sided tandem turnout from the current starting turnout.'
      + #13 + #13 + 'It may be possible to create a tandem turnout manually.');
    delete_tandem;
    restore_current;

    creating_tandem := False;

    saved_current.keep_shove_list.Free;

    pad_form.ds_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later
    pad_form.ss_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    redraw(True);
    EXIT;
  end;

  k3n_1 := k3n;  // for report

  // choose intersection nearer to first turnout xing ...

  a1 := SQRT(SQR(first_fpx - qx1) + SQR(g - qy1));
  a2 := SQRT(SQR(first_fpx - qx2) + SQR(g - qy2));

  if a1 < a2 then begin
    midx := qx1;
    midy := qy1;
  end
  else begin
    midx := qx2;
    midy := qy2;
  end;


  // find corresponding splice intersection ...

  if get_circle_intersections(first_torgx, first_torgy, ABS(first_trad - j),
    torgx, torgy, ABS(tradius + j),
    qx1, qy1, k1_r1, k1_r2, qx2, qy2, k2_r1, k2_r2) <> 2 then begin
    ShowMessage(
      'Sorry, it has not been possible to create a single-sided tandem turnout from the current starting turnout.'
      + #13 + #13 + 'It may be possible to create a tandem turnout manually.');
    delete_tandem;
    restore_current;

    creating_tandem := False;

    saved_current.keep_shove_list.Free;

    pad_form.ds_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later
    pad_form.ss_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    redraw(True);
    EXIT;
  end;

  // choose intersection nearer to first turnout xing ...

  a1 := SQRT(SQR(first_fpx - qx1) + SQR(g - qy1));
  a2 := SQRT(SQR(first_fpx - qx2) + SQR(g - qy2));

  if a1 < a2 then begin
    midsplicex := qx1;
    midsplicey := qy1;
  end
  else begin
    midsplicex := qx2;
    midsplicey := qy2;
  end;

  gocalc(0, 0);

  trad_2nd := calculate_turnout_radius_beyond(clrad1, second_xing_notch,
    second_mid_notch, second_end_notch);  // get turnout rad and 3 points on it

  equivalent_rad := equiv_rad_calc(trad_2nd, trad_1st) + g / 2;  //  for 3rd xing calcs

  if hand_i = 1 then begin
    left_rad := trad_2nd;
    right_rad := clrad1;
  end
  else begin
    right_rad := trad_2nd;
    left_rad := clrad1;
  end;

  middle_rad := trad_1st;

  left_rad_ins := ABS(left_rad / 25.4);
  right_rad_ins := ABS(right_rad / 25.4);
  middle_rad_ins := ABS(middle_rad / 25.4);

  left_rad_str := rad_str(left_rad, 0);
  middle_rad_str := rad_str(middle_rad, 0);
  right_rad_str := rad_str(right_rad, 0);

  if left_rad_str <> 'straight' then
    left_rad_str := left_rad_str + ' mm  ( ' + rad_str(left_rad_ins, 1) + ' " )';
  if middle_rad_str <> 'straight' then
    middle_rad_str := middle_rad_str + ' mm  ( ' + rad_str(middle_rad_ins, 1) + ' " )';
  if right_rad_str <> 'straight' then
    right_rad_str := right_rad_str + ' mm  ( ' + rad_str(right_rad_ins, 1) + ' " )';

  turnoutx := atx + 60 * inscale;   // to C timber to include check rail ends
  gocalc(0, 0);

  store_and_background(False, False);     // 2nd template (1st turnout)  copy to background.

  try

    // get x,y for new FP notch

    with temp_notch_v do begin

      docurving(True, True, midx, midy, notch_x, temp_y, temp_k, dummy);
      // get notch on pad data from intersection
      notch_y := temp_y * hand_i + y_datum;
      notch_k := 0;                          // angle not yet
    end;//with

    // get x,y for new splice notch

    with temp_notch_s do begin

      docurving(True, True, midsplicex, midsplicey, notch_x, temp_y, temp_k, dummy);
      // get notch on pad data from intersection
      notch_y := temp_y * hand_i + y_datum;
      notch_k := 0;                          // angle not needed
    end;//with

    // find vee intersection angle notch_k ...

    p1_1 := tpex_from_tnotch(first_heel_notch);
    p2_1 := tpex_from_tnotch(first_mid_notch);
    p3_1 := tpex_from_tnotch(first_xing_notch);

    p1_2 := tpex_from_tnotch(second_xing_notch);
    p2_2 := tpex_from_tnotch(second_mid_notch);
    p3_2 := tpex_from_tnotch(second_end_notch);


    if get_arc_centre(p1_1, p2_1, p3_1, centre_1) = False then begin

      ShowMessage(
        'Sorry, it has not been possible to create a single-sided tandem turnout from the current starting turnout.'
        + #13 + #13 + 'It may be possible to create a tandem turnout manually.');
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    if get_arc_centre(p1_2, p2_2, p3_2, centre_2) = False then begin
      ShowMessage(
        'Sorry, it has not been possible to create a single-sided tandem turnout from the current starting turnout.'
        + #13 + #13 + 'It may be possible to create a tandem turnout manually.');
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    if get_circle_intersections(centre_1.x, centre_1.y, ABS(trad_1st - g / 2),
      centre_2.x, centre_2.y, ABS(trad_2nd + g / 2), qx1, qy1, k1_r1, k1_r2,
      qx2, qy2, k2_r1, k2_r2) <> 2
    // return code: 2 = OK, two usable intersections
    then begin
      ShowMessage(
        'Sorry, it has not been possible to create a single-sided tandem turnout from the current starting turnout.'
        + #13 + #13 + 'It may be possible to create a tandem turnout manually.');
      delete_tandem;
      restore_current;
      redraw(False);
      EXIT;
    end;

    // choose intersection nearer to first turnout xing ...

    a1 := SQRT(SQR(p3_1.x - qx1) + SQR(p3_1.y - qy1));
    a2 := SQRT(SQR(p3_1.x - qx2) + SQR(p3_1.y - qy2));

    if a1 < a2 then
      temp_notch_v.notch_k := k1_r1 - Pi / 2
    else
      temp_notch_v.notch_k := k2_r1 - Pi / 2;

    normalize_angle(temp_notch_v.notch_k);
    new_notch(temp_notch_v, False);             // put the notch there, and don't link group


    tandem_timb := 4;                            // extend on MS
    cpi.mainside_ends_pi := True;                // in-line (menu disabled for half-diamond)

    omit_wj_marks := True;    // no wing rail joints

    pad_form.timbers_in_line_menu_entry.Click;

    fix_radius(trad_1st, False);     // main road curving for middle xing  (single-sided tandem)

    gocalc(0, 0);

    convert_to_regular_half_diamond;
    // to half-diamond   hand is already correct to align over the first template
    xing_type_i := 1;                      // curviform
    retpar_i := 0;

    auto_diamond := 1;      // fixed K-crossing

    gocalc(0, 0);

    pad_form.peg_on_fp_menu_entry.Click;

    gocalc(0, 0);

    shift_onto_notch(True, True);

    gocalc(0, 0);

    wait_form.waiting_label.Caption := 'please  wait ...';
    wait_form.waiting_label.Width := wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);
    // bug fix for Wine

    wait_form.wait_progressbar.Visible := False;
    wait_form.cancel_button.Visible := False;
    wait_form.Show;

    Application.ProcessMessages;

    // find xing angle k3n which fits at FP and at splice ...

    k3n := 1.5;               // minimum init

    re_do_for_k3n_hd;      // init for current k3n

    try_hd := get_k3n_hd(0.1);

    if try_hd = True then begin
      re_do_for_k3n_hd;
      try_hd := get_k3n_hd(0.01);
    end;

    if try_hd = True then begin
      re_do_for_k3n_hd;
      try_hd := get_k3n_hd(0.001);
    end;

    if try_hd = True then begin
      re_do_for_k3n_hd;
      try_hd := get_k3n_hd(0.0001);
    end;

    if try_hd = True then
      re_do_for_k3n_hd      // one last time
    else begin                 // or try gaunt instead

      equivalent_rad := equiv_rad_calc(trad_2nd, trad_1st) + g / 2;  // reset

      half_diamond := False;    // now a turnout
      gocalc(0, 0);

      pad_form.peg_on_fp_menu_entry.Click;
      gocalc(0, 0);

      shift_onto_notch(True, True);
      gocalc(0, 0);

      convert_to_or_from_gaunt(True);
      gocalc(0, 0);

      pad_form.peg_on_fp_menu_entry.Click;

      new_notch(temp_notch_v, False);

      gocalc(0, 0);

      shift_onto_notch(True, True);

      gocalc(0, 0);

      Application.ProcessMessages;

      // find xing angle k3n which fits at FP and at splice ...

      k3n := 1.5;               // minimum init
      re_do_for_k3n_gaunt;

      try_gaunt := get_k3n_gaunt(0.1);

      if try_gaunt = True then begin
        re_do_for_k3n_gaunt;
        try_gaunt := get_k3n_gaunt(0.01);
      end;

      if try_gaunt = True then begin
        re_do_for_k3n_gaunt;
        try_gaunt := get_k3n_gaunt(0.001);
      end;

      if try_gaunt = True then begin
        re_do_for_k3n_gaunt;
        try_gaunt := get_k3n_gaunt(0.0001);
      end;

      if try_gaunt = True then
        re_do_for_k3n_gaunt      // one last time
      else begin                    // or no joy
        wait_form.Hide;

        Application.ProcessMessages;

        ShowMessage(
          'Sorry, it has not been possible to create a single-sided tandem turnout from the current starting turnout.'
          + #13 + #13 +
          'It may be possible to create a tandem turnout manually.');

        delete_tandem;
        restore_current;
        EXIT;
      end;
    end;

    wait_form.Hide;

    Application.ProcessMessages;

    main_road_i := 3;   // minimum

    turnout_road_i := 0;  // normal exit

    main_road_stock_rail_flag := False;

    include_closure_timbers := True;

    xorg := 0;
    turnoutx := fpx + get_notch_distance(temp_notch_v, far_notch);

    gocalc(0, 0);

    shift_onto_notch(True, True);   // needed for gaunt

    k3n_2 := k3n;  // for final report

    fpx_notch := get_snap_peg_xy_data(4);

    startx := fpx - get_notch_distance(second_xing_notch, fpx_notch) + 45 * inscale;
    // 45" arbitrary

    gocalc(0, 0);

    store_and_background(False, False);     // middle crossing  copy to background.

    isol_notch := get_snap_peg_xy_data(260);     // MMINP peg position

    // create partial templates ...

    clicked_keep_index := first_bgnd_template;
    // get first turnout back (discard middle xing in control)
    delete_to_current_popup_entry_click(True);

    k3n_3 := k3n;  // for report

    turnout_road_stock_rail_flag := True;
    turnout_road_check_rail_flag := True;
    crossing_vee_flag := True;
    main_road_check_rail_flag := True;
    turnout_road_crossing_rail_flag := True;

    main_road_crossing_rail_flag := False;
    main_road_stock_rail_flag := False;

    do_railedges;

    old_turnoutx := turnoutx;

    include_closure_timbers := False;

    omit_wj_marks := True;    // no wing rail joints

    turnoutx := heelx + get_notch_distance(first_heel_notch, second_xing_notch) + 4 * scale;
    // 4ft arbitrary
    gocalc(0, 0);
    store_and_background(False, False);     // put first back shortened without crossing rail

    startx := turnoutx;  //heelx+get_notch_distance(first_heel_notch,second_xing_notch)+3*scale;

    turnoutx := old_turnoutx;

    main_road_stock_rail_flag := True;      // back on
    main_road_crossing_rail_flag := True;   // back on

    turnout_road_stock_rail_flag := False;  // off

    gocalc(0, 0);
    store_and_background(False, False);     // and then onwards again

    // final exit rail (might be regular or generic) ...

    turnout_road_check_rail_flag := False;
    crossing_vee_flag := False;
    main_road_check_rail_flag := False;
    main_road_crossing_rail_flag := False;
    main_road_stock_rail_flag := False;
    turnout_road_crossing_rail_flag := False;

    turnout_road_stock_rail_flag := True;

    do_railedges;

    no_timbering := True;

    omit_wj_marks := True;    // no wing rail joints

    pad_form.no_track_centre_lines_menu_radio.Click;

    startx := tvjpx - get_notch_distance(far_notch, isol_notch) - 4 * inscale;    // 4" arbitrary

    gocalc(0, 0);

    store_and_background(False, False);     // final rail

    clicked_keep_index := first_bgnd_template + 1;     // get middle xing again
    delete_to_current_popup_entry_click(True);

    pad_form.make_branch_track_menu_item.Click;
    // put middle xing back, and make branch track from it in the control

    keeps_list.Exchange(keeps_list.Count - 2, keeps_list.Count - 1);
    // 219a  swap last in list to plain rail to avoid bold timbering

    pad_form.reset_tandem_turnout_menu_entry.Click;  // remove partial settings

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    show_and_redraw(True, True);

    redraw(False);

    do_template_names;

    if alert(4, '    make  3-way  tandem  turnout',
      '||Is this the required tandem turnout ?' +
      '||V-crossing angles are (RAM) :  1:' + round_str(k3n_1, 2) + '   1:' + round_str(
      k3n_2, 2) + '   1:' + round_str(k3n_3, 2) + '|||Left-side radius :    ' +
      left_rad_str + '||Middle-road radius :  ' + middle_rad_str +
      '||Right-side radius :    ' + right_rad_str, '', '', '', '',
      'no  -  cancel  tandem  turnout', 'yes  -  continue', 0) = 5 then begin
      delete_tandem;
      restore_current;
      Result := False;
    end
    else begin

      if help(0, '   `0Tandem  Turnout`9' +
        '||Before you print this tandem turnout for construction, it needs to be finished by removing any conflicts.'
        + '||Check that the check rail and wing rail ends are not obstructing a running clearance for wheel flanges. They can be shortened or modified by clicking the `0real > adjust check rails...`1 menu item.' + '||If check rail or wing rail ends are overlapping they can be straightened or combined into one rail by clicking the `0real > adjust check rails...`1 menu item.' + '||Some timbers may need to be lengthened or shortened, and overlapping timbers will need to be omitted. Click the `0real > shove timbers...`1 menu item.' + '||Templot0 cannot make these final adjustments for you because it requires a human eye to see what is needed.' + '||If you are still track planning these final adjustments can be left until later, to avoid wasting your work if you need to make changes to this tandem turnout.' + '||For more information about how to make these adjustments click the bar below.', 'more  information') = 1 then
        pad_form.tandems_click_first_menu_entry.Click;

      save_done := False;
      backup_wanted := True;
      Result := True;
    end;

  finally

    creating_tandem := False;

    saved_current.keep_shove_list.Free;

    pad_form.ds_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later
    pad_form.ss_tandem_begin_menu_entry.Enabled := True;     // so can start a new one later

    pad_form.reset_notch_menu_entry.Enabled := True;
    pad_form.reset_notch_menu_entry.Click;

    redraw(True);
  end;//try

end;
//______________________________________________________________________________

end.
