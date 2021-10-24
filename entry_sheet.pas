
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
unit entry_sheet;

{$MODE Delphi}

interface

uses
  LCLType, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Spin, Menus;

type
  Tdata_entry_form = class(TForm)
    enter_button: TButton;
    restore_button: TButton;
    input_prompt_label0: TLabel;
    input_dims_label0: TLabel;
    input_spacer_label0: TLabel;
    blue_corner_panel: TPanel;
    datestamp_label: TLabel;
    size_updown: TUpDown;
    options_panel: TPanel;
    metric_button: TButton;
    input_panel0: TPanel;
    help_what_next_button: TButton;
    ok_panel: TPanel;
    ok_button: TButton;
    conversions_checkbox: TCheckBox;
    jot_all_button: TButton;
    help_shape: TShape;
    more_info_button: TButton;
    cancel_button: TButton;
    more_info_shape: TShape;
    cancel_shape: TShape;
    enter_shape: TShape;
    show_selected_checkbox: TCheckBox;
    help_button0: TButton;
    input_panel1: TPanel;
    input_prompt_label1: TLabel;
    input_dims_label1: TLabel;
    input_spacer_label1: TLabel;
    input_box1: TEdit;
    help_button1: TButton;
    input_panel2: TPanel;
    input_prompt_label2: TLabel;
    input_dims_label2: TLabel;
    input_spacer_label2: TLabel;
    input_box2: TEdit;
    help_button2: TButton;
    input_panel3: TPanel;
    input_prompt_label3: TLabel;
    input_dims_label3: TLabel;
    input_spacer_label3: TLabel;
    input_box3: TEdit;
    help_button3: TButton;
    input_panel4: TPanel;
    input_prompt_label4: TLabel;
    input_dims_label4: TLabel;
    input_spacer_label4: TLabel;
    input_box4: TEdit;
    help_button4: TButton;
    input_panel5: TPanel;
    input_prompt_label5: TLabel;
    input_dims_label5: TLabel;
    input_spacer_label5: TLabel;
    input_box5: TEdit;
    help_button5: TButton;
    input_panel6: TPanel;
    input_prompt_label6: TLabel;
    input_dims_label6: TLabel;
    input_spacer_label6: TLabel;
    input_box6: TEdit;
    help_button6: TButton;
    input_panel7: TPanel;
    input_prompt_label7: TLabel;
    input_dims_label7: TLabel;
    input_spacer_label7: TLabel;
    input_box7: TEdit;
    help_button7: TButton;
    ps_label: TLabel;
    nops_label: TLabel;
    restore_all_button: TButton;
    jot_button: TButton;
    wine_label: TLabel;
    input_box0: TEdit;
    paste_button: TButton;
    jot_always_checkbox: TCheckBox;
    full_fraction_checkbox: TCheckBox;
    overwrite_checkbox: TCheckBox;
    procedure restore_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure enter_buttonClick(Sender: TObject);
    procedure help_button0Click(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure metric_buttonClick(Sender: TObject);
    procedure more_info_buttonClick(Sender: TObject);
    procedure help_what_next_buttonClick(Sender: TObject);
    procedure jot_all_buttonClick(Sender: TObject);
    procedure wine_labelClick(Sender: TObject);
    procedure ok_panelClick(Sender: TObject);
    procedure input_box0Exit(Sender: TObject);
    procedure input_box1Exit(Sender: TObject);
    procedure input_box2Exit(Sender: TObject);
    procedure input_box3Exit(Sender: TObject);
    procedure input_box4Exit(Sender: TObject);
    procedure input_box5Exit(Sender: TObject);
    procedure input_box6Exit(Sender: TObject);
    procedure input_box7Exit(Sender: TObject);
    procedure help_button1Click(Sender: TObject);
    procedure help_button2Click(Sender: TObject);
    procedure help_button3Click(Sender: TObject);
    procedure help_button4Click(Sender: TObject);
    procedure help_button5Click(Sender: TObject);
    procedure help_button6Click(Sender: TObject);
    procedure help_button7Click(Sender: TObject);
    procedure conversions_checkboxClick(Sender: TObject);
    procedure show_selected_checkboxClick(Sender: TObject);
    procedure ok_buttonClick(Sender: TObject);
    procedure input_box0Change(Sender: TObject);
    procedure restore_all_buttonClick(Sender: TObject);
    procedure paste_buttonClick(Sender: TObject);
    procedure jot_buttonClick(Sender: TObject);
    procedure cancel_buttonClick(Sender: TObject);
    procedure jot_always_checkboxClick(Sender: TObject);
    procedure full_fraction_checkboxClick(Sender: TObject);
    procedure input_box0KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  data_entry_form: Tdata_entry_form;

//_____________________

const
  dims_i_c = 7;                  // max 8 lines on form.

type
  Toutdim = array[0..dims_i_c] of double;

function putdim(h_str: string; units: integer; p_str: string;
  m: double; no_neg, no_ps, no_zero, zero_terminate: boolean): integer;
function getdims(head_str, help_str: string; calling_form: TForm; n_max: integer;
  var outdims: Toutdim): boolean;

procedure get_custom_input_factors;

//_________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  Clipbrd, Math, control_room, pad_unit, gauge_unit, alert_unit, math_unit, help_sheet, chat_unit,
  metric_unit, keep_select, stay_visible_unit, jotter_unit, startup_unit;

type
  Tdims = record
    help_str: string;         // help string.
    prompt_str: string;         // prompt string.
    units_str: string;         // units string.
    orgdim: double;       // original value.
    newdim: double;       // new value.
    noneg: boolean;        // True=negative entry not allowed.
    nops: boolean;        // True=no preset possible.
    nozero: boolean;        // True=zero entry not allowed.
    zeroterm: boolean;        // True=terminate input on zero entered.
  end;

const
  entry_help_str: string = 'php/460    `0Data-Entry  Form`9' +
    '||This is Templot0''s main data-entry form, which appears whenever dimensional information needs to be entered.'
    + ' Up to 8 items of information may be entered on one form. If more than this is needed the form will'
    + ' appear several times as necessary.' +
    '||To enter data, edit or enter the required information in the white panel on the current line,'
    +
    ' and then press the `0ENTER`2 key on the keyboard, or click the `0enter`1 button.'
    + '||To use the dimension already showing, just press `0ENTER`2. To revert to this dimension after making'
    + ' unwanted changes to it, click the `0restore`1 button.' +
    ' To enter a previously copied dimension (for example from the metric calculator) click the `0paste`1 button.'
    + '||To amend an item already entered, simply click back on the entered dimension.'
    + '||For more details and explanation about the current item, click the `0?&nbsp;info`1 button on each line.'
    + '||For more general help about all these items click the bottom `0more info F1`z button (if showing) or press `0F1`2.'
    + ' This may include a link to more information online.' +
    '||If you enter a slash ( / ) at the left of the white panel, Templot0 will in most cases ignore anything else'
    + ' in the panel and use a pre-set value for the dimension required. This will usually be the exact scale equivalent of'
    + ' full-size British (bullhead) practice for your chosen gauge and scale. If no pre-set is possible, a note'
    + ' will appear near the bottom of the form.' +
    '||If you enter a completely blank value, Templot0 will assume that you mean zero ( 0 ). Note that in some cases,'
    + ' zero is not a valid entry (as a radius dimension, for example). If the figures you enter are unacceptable,'
    + ' Templot0 will ask you to try again.' +
    '||You can leave or insert spaces in your figures if you wish. They will be ignored.'
    + '||If you prefix your entry with an opening square bracket as :' + '||[ -123.45'
    + '||your entry will be taken as negative, regardless of the presence or absence of a subsequent minus sign.'
    + ' This corresponds to Templot0''s normal display of negative numbers within square brackets.'
    + ' The closing square bracket is optional. If you have switched off the square brackets (using the `0PROGRAM > NUMBER FORMAT`1'
    + ' menu item on the `0PROGRAM PANEL`1 menus) the bracket(s) will be removed when you press `0ENTER`1.'
    + ' Note that in many cases a negative dimension is invalid.' +
    '||Be careful to note the required units - some entries are in millimetres on the model, some in inches'
    + ' from the full-size prototype to save you unnecessary calculations. They will be converted to millimetres'
    + ' at the current scale. Do not add other unit names to your entry. For example, if you type "2 ft" when'
    + ' asked for millimetres, Templot0 will ignore the "ft" and assume that you mean 2 mm.' +
    '||To enter dimensions in units other than those asked for you can instead prefix your figures with'
    + ' the code letters for one or more input conversion factors.' +
    ' For example when asked for a radius dimension in mm you could enter 48 inches as i48. You can also'
    + ' specify your own custom conversion factors for other units.' +
    ' For the conversion factors to work it is necessary for the `0INPUT CONVERSIONS`1 tickbox to be ticked.'
    + ' For more information click `0ABOUT CONVERSION FACTORS`1 below.' +
    '||If you prefix your entered data with the number-sign (hash) symbol #, your entered data will be used'
    + ' to modify the existing data instead of replacing it.' +
    ' For example, if the radius is showing as 1200 mm, entering #100 would increase it to 1300 mm,'
    + ' or entering #-200 would reduce it to 1000 mm.' +
    ' The # prefix can be combined with conversion factors, so that for example #i-2 would reduce the'
    +
    ' radius by 2 inches, even though the dimension is showing in mm.' +
    ' A semicolon symbol (;) can be used as an alternative to the # symbol if preferred. This may be more'
    + ' convenient on non-UK keyboards.' +
    '||If you click the `0CANCEL ALL`1 button or press the `0ESC`2 escape key, ALL your entries on the form'
    + ' will be cancelled, and ALL the previous settings will be restored.' +
    '||An `0OK`z button will appear at the end of the form, so that you can check all the entries before continuing.'
    + ' Dimensions shown in blue have been modified; dimensions shown in black remain unchanged.'
    + ' If you close the form before the `0OK`z button appears, or without clicking it, ALL your entries'
    + ' will be cancelled as if you clicked the `0CANCEL ALL`1 button.' +
    '||If the `0OVERWRITE`1 tickbox is ticked, or the `0INSERT`2 key is pressed, the existing data can be overwritten'
    + ' with new data as you enter it. This is often a convenient way to edit existing data. Pressing the `0INSERT`2 key'
    + ' again will deselect this option.' +
    '||If the `0SHOW DATA PRE-SELECTED`1 tickbox is ticked, the existing data will be shown selected (highlighted)'
    + ' in the Windows style, and will be replaced immediately when you begin entering new data.'
    + ' If this tickbox is unticked, the existing data must be deleted or edited to create the new data.'
    + ' The `0OVERWRITE`1 mode is not available when this tickbox is ticked.' +
    '||Clicking the `0JOT`1 or `0JOT ALL`1 buttons copies the current item or all the items onto the'
    +
    ' Templot0 jotter for future reference. To show or hide the jotter, press the `0CTRL+J`2 keys.'
    + '||If the `0JOT ALWAYS`1 tickbox is ticked, all items are copied onto the jotter every time the'

    + ' `0OK`z button is clicked. In this way a complete record can be kept of all your entered data'
    + ' during the Templot0 session.';


var

  dec_places: integer = 2;   // default show 2 decimal places

  dims_i: integer = 0;       // index to input array for putdim

  dims: array[0..dims_i_c] of Tdims;

  helpstring: string = '';
  help_all_str: string = '';

  save_info: boolean = False;

  custom_input_factor1: double = 1.0;
  custom_input_factor2: double = 1.0;
  custom_input_factor3: double = 1.0;
  custom_input_factor4: double = 1.0;

  current_line: integer = 0;
  bottom_line: integer = 0;
  max_line: integer = 0;

procedure show_factors_help; forward;

//______________________________________________________________________________

procedure init_box(n: integer; input_box: TEdit);

begin

  if data_entry_form.full_fraction_checkbox.Checked = True    // 206e
  then
    dec_places := 20
  else
    dec_places := 2;

  with dims[n] do begin
    if orgdim = def_req then
      input_box.Text := '/ pre-set'
    else begin
      if (ABS(orgdim) >= max_rad_test) and (nops = False)
      then
        input_box.Text := '/ straight'
      else
        input_box.Text := round_str(orgdim, dec_places);
    end;
  end;//with

  input_box.Font.Color := clBlack;  // no new data yet

  input_box.AutoSelect := data_entry_form.show_selected_checkbox.Checked;

end;
//______________________________________________________________________________

procedure restore_input(n: integer);

begin
  with data_entry_form do begin

    case n of

      0:
        init_box(n, input_box0);
      1:
        init_box(n, input_box1);
      2:
        init_box(n, input_box2);
      3:
        init_box(n, input_box3);
      4:
        init_box(n, input_box4);
      5:
        init_box(n, input_box5);
      6:
        init_box(n, input_box6);
      7:
        init_box(n, input_box7);

    end;//case
  end;//with

  with dims[n] do
    newdim := orgdim;
end;
//______________________________________________________________________________

procedure set_outlined_panel(n: integer);

// adjust Top and Left to keep contents in the same place

// enter with n= -1 to clear all outlines

const
  outlined_colour: integer = $00E8E8CC;

begin
  with data_entry_form do begin

    if Showing = True then begin

      if n <> 0 then
        with input_panel0 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(52 * global_factor);
          help_button0.Visible := False;
          input_box0.Color := $00ECECEC;
          input_spacer_label0.Color := $00ECECEC;
        end;
      if n <> 1 then
        with input_panel1 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(84 * global_factor);
          help_button1.Visible := False;
          input_box1.Color := $00ECECEC;
          input_spacer_label1.Color := $00ECECEC;
        end;
      if n <> 2 then
        with input_panel2 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(116 * global_factor);
          help_button2.Visible := False;
          input_box2.Color := $00ECECEC;
          input_spacer_label2.Color := $00ECECEC;
        end;
      if n <> 3 then
        with input_panel3 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(148 * global_factor);
          help_button3.Visible := False;
          input_box3.Color := $00ECECEC;
          input_spacer_label3.Color := $00ECECEC;
        end;
      if n <> 4 then
        with input_panel4 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(180 * global_factor);
          help_button4.Visible := False;
          input_box4.Color := $00ECECEC;
          input_spacer_label4.Color := $00ECECEC;
        end;
      if n <> 5 then
        with input_panel5 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(212 * global_factor);
          help_button5.Visible := False;
          input_box5.Color := $00ECECEC;
          input_spacer_label5.Color := $00ECECEC;
        end;
      if n <> 6 then
        with input_panel6 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(244 * global_factor);
          help_button6.Visible := False;
          input_box6.Color := $00ECECEC;
          input_spacer_label6.Color := $00ECECEC;
        end;
      if n <> 7 then
        with input_panel7 do begin
          BorderStyle := bsNone;
          ParentColor := True;
          Left := 8;
          Top := Round(276 * global_factor);
          help_button7.Visible := False;
          input_box7.Color := $00ECECEC;
          input_spacer_label7.Color := $00ECECEC;
        end;

      //  extra PostMessage needed to ensure SetFocus with flashing caret (Delphi bug)...

      if n = 0 then
        with input_panel0 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(51 * global_factor);
          help_button0.Visible := True;
          Visible := True;
          input_box0.Color := clWhite;
          input_spacer_label0.Color := clWhite;
          input_box0.SetFocus;
          PostMessage(input_box0.Handle, WM_SETFOCUS, 0, 0);
        end;
      if n = 1 then
        with input_panel1 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(83 * global_factor);
          help_button1.Visible := True;
          Visible := True;
          input_box1.Color := clWhite;
          input_spacer_label1.Color := clWhite;
          input_box1.SetFocus;
          PostMessage(input_box1.Handle, WM_SETFOCUS, 0, 0);
        end;
      if n = 2 then
        with input_panel2 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(115 * global_factor);
          help_button2.Visible := True;
          Visible := True;
          input_box2.Color := clWhite;
          input_spacer_label2.Color := clWhite;
          input_box2.SetFocus;
          PostMessage(input_box2.Handle, WM_SETFOCUS, 0, 0);
        end;
      if n = 3 then
        with input_panel3 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(147 * global_factor);
          help_button3.Visible := True;
          Visible := True;
          input_box3.Color := clWhite;
          input_spacer_label3.Color := clWhite;
          input_box3.SetFocus;
          PostMessage(input_box3.Handle, WM_SETFOCUS, 0, 0);
        end;
      if n = 4 then
        with input_panel4 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(179 * global_factor);
          help_button4.Visible := True;
          Visible := True;
          input_box4.Color := clWhite;
          input_spacer_label4.Color := clWhite;
          input_box4.SetFocus;
          PostMessage(input_box4.Handle, WM_SETFOCUS, 0, 0);
        end;
      if n = 5 then
        with input_panel5 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(211 * global_factor);
          help_button5.Visible := True;
          Visible := True;
          input_box5.Color := clWhite;
          input_spacer_label5.Color := clWhite;
          input_box5.SetFocus;
          PostMessage(input_box5.Handle, WM_SETFOCUS, 0, 0);
        end;
      if n = 6 then
        with input_panel6 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(243 * global_factor);
          help_button6.Visible := True;
          Visible := True;
          input_box6.Color := clWhite;
          input_spacer_label6.Color := clWhite;
          input_box6.SetFocus;
          PostMessage(input_box6.Handle, WM_SETFOCUS, 0, 0);
        end;
      if n = 7 then
        with input_panel7 do begin
          BorderStyle := bsSingle;
          Color := outlined_colour;
          Left := 7;
          Top := Round(275 * global_factor);
          help_button7.Visible := True;
          Visible := True;
          input_box7.Color := clWhite;
          input_spacer_label7.Color := clWhite;
          input_box7.SetFocus;
          PostMessage(input_box7.Handle, WM_SETFOCUS, 0, 0);
        end;

    end
    else
      with input_panel0 do begin
        BorderStyle := bsSingle;
        Color := outlined_colour;
        Left := 7;
        Top := Round(51 * global_factor);
        help_button0.Visible := True;
        Visible := True;
        input_box0.Color := clWhite;
        input_spacer_label0.Color := clWhite;
        ActiveControl := input_box0;
      end; // not showing so init first line in readiness

    case n of

      0..7: begin
        data_entry_form.nops_label.Visible := dims[n].nops;
        data_entry_form.ps_label.Visible := not dims[n].nops;
      end;

      else begin
        data_entry_form.nops_label.Visible := False;
        data_entry_form.ps_label.Visible := False;
      end;

    end;//case

  end;//with

  current_line := n;
end;
//______________________________________________________________________________

procedure init_panel(n: integer; input_panel: TPanel; input_prompt_label, input_dims_label: TLabel;
  input_box: TEdit);     // get the info from putdim

begin

  init_box(n, input_box);

  with dims[n] do begin
    input_prompt_label.Caption := prompt_str + '  = ';

    input_dims_label.Caption := units_str;

  end;//with

  input_box.Font.Color := clBlack;  // no new data yet

  input_box.AutoSelect := data_entry_form.show_selected_checkbox.Checked;

end;
//______________________________________________________________________________

procedure Tdata_entry_form.restore_buttonClick(Sender: TObject);

begin
  restore_input(current_line);
  set_outlined_panel(current_line);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.restore_all_buttonClick(Sender: TObject);

var
  i: integer;

begin
  for i := 0 to max_line do
    restore_input(i);

  current_line := 0;          // 206e
  bottom_line := 0;           // 206e
  set_outlined_panel(0);    // 206e was current_line
end;
//______________________________________________________________________________

function putdim(h_str: string; units: integer; p_str: string;
  m: double; no_neg, no_ps, no_zero, zero_terminate: boolean): integer;

  //  prepare for a call to getdims - build the dims list.
  //  return the index used.

  //                      m = current value

  //                   no_neg = False, show negative entry as is.
  //                   no_neg = True, negative entry not allowed.

  //                   no_ps = False, show '/' entry as PRE-SET.
  //                   no_ps = True,  no pre-set available.

  //                   no_zero = False, show 0 entry as 0.
  //                   no_zero = True,  0 entry not allowed.
  //
  //                   zero_terminate = False, return the zero and continue.
  //                   zero_terminate = True,  return the zero and terminate input.

begin
  if data_entry_form.Visible = True          //  should never get here !
  then begin
    Result := -1;
    EXIT;
  end;

  if dims_i > dims_i_c then
    run_error(65);

  with dims[dims_i] do begin
    help_str := h_str;       // help string.
    prompt_str := p_str;     // prompt string.
    orgdim := m;             // original value.
    case units of
      0:
        units_str := '';
      1:
        units_str := ' mm';      // units string.
      2:
        units_str := ' inches';
      3:
        units_str := ' degrees';
      4:
        units_str := ' %';
      5:
        units_str := ' cm';
      6:
        units_str := ' feet';
      7:
        units_str := ' proto-feet';   // prototype feet.
      8:
        units_str := ' metres';
      else
        run_error(66);
    end;//case
    noneg := no_neg;            // True = negative entry not allowed.
    nops := no_ps;              // True = no preset possible.
    nozero := no_zero;          // True = zero entry not allowed.
    zeroterm := zero_terminate; // True = terminate input on zero entered.
  end;//with

  Result := dims_i;

  Inc(dims_i);                // to next free line, ready for next call.
end;
//______________________________________________________________________________

function getdims(head_str, help_str: string; calling_form: TForm; n_max: integer;
  var outdims: Toutdim): boolean;

var
  i, n: integer;

begin
  with data_entry_form do begin

    Result := False;  // init

    ok_panel.Visible := False;

    max_line := n_max;   // global

    help_all_str := help_str;

    Caption := '  enter  data  for :        ' + head_str;

    if help_all_str <> '' then begin
      more_info_button.Enabled := True;
      more_info_shape.Show;
    end
    else begin
      more_info_button.Enabled := False;
      more_info_shape.Hide;
    end;

    if (calling_form = pad_form) and (info_show_i = 1) then begin
      save_info := True;                         //  so we can re-show if nec.
      pad_form.hide_info_menu_entry.Click;
      //  and hide the info panel to avoid confusion.
    end
    else
      save_info := False;


    input_panel0.Visible := False;   // init bars
    input_panel1.Visible := False;
    input_panel2.Visible := False;
    input_panel3.Visible := False;
    input_panel4.Visible := False;
    input_panel5.Visible := False;
    input_panel6.Visible := False;
    input_panel7.Visible := False;


    init_panel(0, input_panel0, input_prompt_label0, input_dims_label0, input_box0);
    init_panel(1, input_panel1, input_prompt_label1, input_dims_label1, input_box1);
    init_panel(2, input_panel2, input_prompt_label2, input_dims_label2, input_box2);
    init_panel(3, input_panel3, input_prompt_label3, input_dims_label3, input_box3);
    init_panel(4, input_panel4, input_prompt_label4, input_dims_label4, input_box4);
    init_panel(5, input_panel5, input_prompt_label5, input_dims_label5, input_box5);
    init_panel(6, input_panel6, input_prompt_label6, input_dims_label6, input_box6);
    init_panel(7, input_panel7, input_prompt_label7, input_dims_label7, input_box7);


    for i := 0 to n_max do
      outdims[i] := dims[i].orgdim;   // init return values

    // start here...

    set_outlined_panel(0);   // init first line
    bottom_line := 0;

    repeat

      do_show_modal(data_entry_form);       // 212a  ShowModal

      if ModalResult = mrOk then begin
        for n := 0 to n_max do
          outdims[n] := dims[n].newdim;   // return new values.
        Result := True;
        BREAK;
      end
      else begin
        if cancel_entries_msg_pref = True then
          i := 5
        else begin

          alert_box.preferences_checkbox.Checked := False;       //%%%%
          alert_box.preferences_checkbox.Show;


          i := alert(7, '        cancel  all  entries ?',
            'Closing this form before data entry is complete'
            +
            ' will cancel ALL your entries on the form, and restore ALL the previous settings.',
            '', '', '', '', 'restore  previous  settings  and  close',
            'continue  data  entry ...', 0);

          cancel_entries_msg_pref := alert_box.preferences_checkbox.Checked;
          alert_box.preferences_checkbox.Hide;
        end;

        if i = 5 then begin
          for n := 0 to n_max do
            outdims[n] := dims[n].orgdim;  // restore old values.
          Result := False;
          BREAK;
        end;
      end;

    until 0 <> 0;

  end;//with form

  Application.ProcessMessages;

  dims_i := 0; // for putdim next time

  if save_info = True then
    pad_form.show_info_menu_entry.Click;        //  and the info panel.
  save_info := False;                                                  //  for next time.

  // following needed to restore proper working   207a
  // if data-entry is done all on keyboard...

  if calling_form.Showing = True then begin
    if calling_form = pad_form then begin
      pad_form.arrow_button_dummy_trackbar.SetFocus;
      PostMessage(pad_form.arrow_button_dummy_trackbar.Handle, WM_SETFOCUS, 0, 0);
    end
    else begin
      calling_form.SetFocus;
      PostMessage(calling_form.Handle, WM_SETFOCUS, 0, 0);
    end;
  end
  else begin
    if pad_form.Showing = True then begin
      pad_form.arrow_button_dummy_trackbar.SetFocus;
      PostMessage(pad_form.arrow_button_dummy_trackbar.Handle, WM_SETFOCUS, 0, 0);
    end;
  end;

  Application.ProcessMessages;
end;
//_______________________________________________________________________________________

procedure Tdata_entry_form.FormCreate(Sender: TObject);

begin
  if Screen.Height < 500 then begin
    Top := 2;    // move form top left of screen for lo-res.
    Left := 2;
  end;

  // OT-FIRST ClientWidth:=660;
  // OT-FIRST ClientHeight:=400;
  AutoScroll := True;

end;
//________________________________________________________________________________________

procedure Tdata_entry_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.

  if Key = VK_F1 then
    if more_info_button.Enabled = True then
      more_info_button.Click;

  if Key = VK_ESCAPE then begin
    Key := 0;
    Close;
  end;

  if ((Key = VK_J) or (Key = VK_j)) and (Shift = [ssCtrl])   // CTRL-J.
  then begin
    if jotter_form.Showing = True  // toggle
    then
      jotter_form.Close
    else begin
      jotter_form.Show;
      if jotter_form.Left < (Left + Width + 10) then
        jotter_form.Left := Left + Width + 10;
    end;
    Key := 0;
  end;

  if (Key = VK_INSERT) and (overwrite_checkbox.Enabled = True)      // 214a
  then begin
    overwrite_checkbox.Checked := not overwrite_checkbox.Checked;
    Key := 0;
  end;

end;
//________________________________________________________________________________________

procedure Tdata_entry_form.FormShow(Sender: TObject);

begin

  if (stay_visible_form.Showing = True) and (stay_visible_form.Left < (Left + Width - 8)) then begin
    Left := 0;
    stay_visible_form.Left := Width - 8;
    // (=630) make sure it doesn't obscure us, otherwise can't continue (we always do BringToFront).
  end;
end;
//________________________________________________________________________________________

procedure Tdata_entry_form.enter_buttonClick(Sender: TObject);

//   CR in the input box comes here.

begin
  // force onExit from the input box..

  PostMessage(enter_button.Handle, WM_SETFOCUS, 0, 0);

  if ok_panel.Showing = True then begin
    if jot_always_checkbox.Checked = True then
      jot_all_button.Click;

    ok_button.Click;
  end;
end;
//__________________________________________________________________________________________

procedure Tdata_entry_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag
  // ! position goes up, size goes down.
  then begin
    ScaleBy(9, 10);                                         // scale the form contents down.
  end;

  if size_updown.Position < size_updown.Tag then begin
    ScaleBy(10, 9);                                         // scale the form contents up.
  end;

  ClientHeight := VertScrollBar.Range;                               // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 4;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;
  // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;                           // and save for the next click.
end;
//__________________________________________________________________________________________

procedure Tdata_entry_form.metric_buttonClick(Sender: TObject);

begin

  if metric_form.Showing = True then
    metric_form.Close
  else begin
    metric_form.Show;
    if metric_form.Top < (Top + Height + 10) then
      metric_form.Top := Top + Height + 10;  // ensure visible -- must be after showing the first time
  end;

  set_outlined_panel(current_line);
end;
//_________________________________________________________________________________________

procedure Tdata_entry_form.more_info_buttonClick(Sender: TObject);

begin
  help(0, help_all_str, '');

  set_outlined_panel(current_line);
end;
//__________________________________________________________________________________________

procedure Tdata_entry_form.help_what_next_buttonClick(Sender: TObject);

begin
  if help(0, entry_help_str, 'about  conversion  factors') = 1 then
    show_factors_help;
  set_outlined_panel(current_line);
end;
//___________________________________________________________________________________________

procedure get_custom_input_factors;

const
  a_str: string = '    Custom  Input  Conversion  Factor  A  or  a.' +
    '||Enter the factor by which you want you input figures to be multiplied when you prefix them with a code letter A or a.';

  b_str: string = '    Custom  Input  Conversion  Factor  B  or  b.' +
    '||Enter the factor by which you want you input figures to be multiplied when you prefix them with a code letter B or b.';

  c_str: string = '    Custom  Input  Conversion  Factor  C  or  c.' +
    '||Enter the factor by which you want you input figures to be multiplied when you prefix them with a code letter C or c.';

  d_str: string = '    Custom  Input  Conversion  Factor  D  or  d.' +
    '||Enter the factor by which you want you input figures to be multiplied when you prefix them with a code letter D or d.';

  more_str: string = '    Custom  Input  Conversion  Factors' +
    '||For more information about using these and the other conversion factors available, click the ? HELP button.'
    + '||N.B. The INPUT CONVERSIONS option box must be ticked for conversion factors to work.';

var
  n: integer;
  od: Toutdim;    // [0..7] array of double;

begin
  putdim(a_str, 0, 'custom  input  conversion  factor  A  or  a', custom_input_factor1,
    True, True, True, False);  // no neg, no preset, no zero, don't terminate on zero.
  putdim(b_str, 0, 'custom  input  conversion  factor  B  or  b', custom_input_factor2,
    True, True, True, False);  // no neg, no preset, no zero, don't terminate on zero.
  putdim(c_str, 0, 'custom  input  conversion  factor  C  or  c', custom_input_factor3,
    True, True, True, False);  // no neg, no preset, no zero, don't terminate on zero.
  n := putdim(d_str, 0, 'custom  input  conversion  factor  D  or  d', custom_input_factor4,
    True, True, True, False);  // no neg, no preset, no zero, don't terminate on zero.
  if n <> 3 then
    EXIT;

  if getdims('custom  input  conversion  factors', more_str, control_room_form, n, od) =
    True   // called from program menu.
  then begin
    custom_input_factor1 := ABS(od[0]);
    // negative factors not allowed to avoid confusion with [- when testing for invalid negatives.
    custom_input_factor2 := ABS(od[1]);
    custom_input_factor3 := ABS(od[2]);
    custom_input_factor4 := ABS(od[3]);
  end;
end;
//_________________________________________________________________________________________

procedure show_factors_help;

const
  conv_help_str: string = '      Input  Conversion  Factors' +
    '||Templot0 uses the following convention when data is to be entered:' +
    '||Actual dimensions on the model or the drawing are always entered in MILLIMETRES.'
    + '||Full-size dimensions on the prototype railway are always entered in INCHES, and the program does the necessary conversion to your chosen scale.' + '||This is a useful distinction and convenient for modellers of traditional steam-era British railways.' + '||There is just one exception to this; the grid-line spacings on the drawing can be entered in any of these units: m, cm, mm, feet, inches.' + '|---------------------------------' + '||For modellers of modern railways and for users in continental Europe and elsewhere where full-size information is frequently in metric units, and for users in the USA where model dimensions' + ' are frequently in decimal or fractional inches, Templot0 also provides "input conversion factors".' + '||By prefixing your entered figures with a code letter, dimensions can be entered in any units of your choice.' + '||Here is a list of all the code letters available (widen this window for easier reading):' + '<SPAN STYLE="FONT-FAMILY:''Courier New'';">' + '|| CODE      DATA            IS' + '|LETTER:  ENTERED IN:   CONVERTED TO:               EXAMPLE:' + '|| m         mm            inches          sleeper width:  m250  =  9.843  inches  ( 250 mm)' + '|| f        feet           inches          sleeper length: f8.5  =   102   inches  ( 8ft-6in )' + '||| i       inches            mm               radius:      i48   =  1219.2  mm     (  48"  )' + '|| e    8ths of inch         mm             track gauge:   e5    =  15.88   mm     ( 5/8"  )' + '|| h   16ths of inch         mm                scale:      h3    =   4.76   mm     ( 3/16" )' + '|| t   32nds of inch         mm             track gauge:   t17   =  13.49   mm     ( 17/32")' + '||| p  prototype inches   scale model mm     track gauge:   p56.5 =  18.83   mm     ( 56.5" )' + '|| s  scale model mm    prototype inches   sleeper length: s32   =    96   inches  ( 32 mm )' + '||| k    degrees         unit angle (RAM)   crossing angle: k9.5  =  1:5.98 RAM     (9.5 deg)' + '|| q  unit angle (CLM)  unit angle (RAM)   crossing angle: q7    =  1:6.96 RAM     (1:7 CLM)' + '|| n  unit angle (RAM)     degrees            rotation:    n6    =  9.46  degrees  ( 1:6   )' + '|| g    gradient %      unit angle (RAM)   crossing angle: g25   =  1:4            ( 25 %  )' + '</SPAN>' + '|||To use conversion factors with a negative dimension, the code letter must precede the minus sign. For example i-48 sets a radius to -1219.2mm. Note that in many cases a negative dimension is invalid.' + '||Conversion factors can also be combined.' + '|For example Templot0 expects the sleeper length to be entered in full-size prototype inches.' + ' If you are using model sleepers 1.25 inches long you could enter this as si1.25' + '||The i prefix then converts the inches dimension to mm, and the s prefix converts model dimensions in mm to full-size inches.' + ' So for 4mm/ft scale si1.25 = 95.25 inches full-size.' + '||Input conversion factors can be combined in any order and can be upper or lower case, so IS1.25 gives the same result.' + '||It is also possible to specify your own custom conversion factors for code letters a,b,c,d.' + ' You can then use any units you choose.' + '||For example British prototype track radius dimensions are often quoted in chains (1 chain = 66 feet), whereas Templot0 expects the radius to be entered in the model size in mm (because it is nearly always underscale).' + '||If you set up custom conversion factors like this:' + '|A = 66' + '|B = 12' + '|you could enter a radius of 10 chains directly as PBA10. The A prefix converts chains to feet, the B prefix converts feet to inches, and the P prefix converts full-size inches to model mm.' + ' So for 4mm/ft scale pba10 = 2640 mm radius on the model.' + '||To set the custom conversion factors select the PROGRAM > CUSTOM INPUT FACTORS... menu item on the PROGRAM PANEL menus.' + '||Important: The INPUT CONVERSIONS option box must be ticked for the above conversion factors to work.' + ' Templot0 makes no check that the code letters used are appropriate for the units requested, since only you know what your figures represent.' + ' The k , n , g , code letters are not valid in conjunction with the # symbol prefix to modify existing data.';

begin
  help(0, conv_help_str, '');
end;
//_______________________________________________________________________________________

procedure input_box_exit(n: integer; input_panel: TPanel; input_prompt_label: TLabel;
  input_box: TEdit);

// all input boxes come here

var
  intxt: string;

  conv_factor: double;

  make_neg: boolean;
  conversions_done: boolean;
  k_ram: boolean;
  ram_k: boolean;
  g_ram: boolean;
  clm_ram: boolean;   // 216a

  modify_data_prefixed: boolean;

  good_value: boolean;
  in_char1: Char;

  invalue: double;
  ok_flag: boolean;

  brackpos: integer;

begin

  with data_entry_form do begin

    // only if enter button clicked, or an existing input line...

    if ActiveControl = help_button0 then
      EXIT;
    if ActiveControl = help_button1 then
      EXIT;
    if ActiveControl = help_button2 then
      EXIT;
    if ActiveControl = help_button3 then
      EXIT;
    if ActiveControl = help_button4 then
      EXIT;
    if ActiveControl = help_button5 then
      EXIT;
    if ActiveControl = help_button6 then
      EXIT;
    if ActiveControl = help_button7 then
      EXIT;

    if ActiveControl = help_what_next_button then
      EXIT;
    if ActiveControl = restore_all_button then
      EXIT;
    if ActiveControl = restore_button then
      EXIT;
    if ActiveControl = paste_button then
      EXIT;
    if ActiveControl = metric_button then
      EXIT;
    if ActiveControl = jot_all_button then
      EXIT;
    if ActiveControl = jot_button then
      EXIT;
    if ActiveControl = show_selected_checkbox then
      EXIT;
    if ActiveControl = jot_always_checkbox then
      EXIT;
    if ActiveControl = full_fraction_checkbox then
      EXIT;
    if ActiveControl = conversions_checkbox then
      EXIT;
    if ActiveControl = size_updown then
      EXIT;
    if ActiveControl = blue_corner_panel then
      EXIT;
    if ActiveControl = options_panel then
      EXIT;
    if ActiveControl = more_info_button then
      EXIT;
    if ActiveControl = cancel_button then
      EXIT;
    if ActiveControl = ok_panel then
      EXIT;
    if ActiveControl = ok_button then
      EXIT;

    with dims[n] do begin

      intxt := input_box.Text;
      if intxt = '' then
        intxt := '0';           //  null input is zero

      intxt := remove_space_str(intxt);        //  strip all spaces.
      if intxt = '' then
        intxt := '0';           //  and then allow null as zero.

      input_box.Text := intxt;                 //  show any changes

      conv_factor := 1.0;                      //  init all these...
      make_neg := False;

      conversions_done := False;
      k_ram := False;
      ram_k := False;
      g_ram := False;
      clm_ram := False;  // 216a

      modify_data_prefixed := False;


      if Copy(intxt, 1, 1) = '/' then begin
        if nops = False                   //  preset allowed ?
        then begin
          invalue := def_req;
          input_box.Text := '/pre-set';
          ok_flag := True;               //  got valid input in invalue.
        end
        else begin
          alert(6, 'php/470    no  pre-set  available',
            'Sorry, no pre-set dimension is available for this item.'
            + '||Please enter a dimension without the ''/'' prefix.',
            '', '', '', '', '', 'O K', 0);

          set_outlined_panel(current_line);
          // return focus when form becomes active again

          EXIT;
        end;
      end
      else begin

        repeat
          brackpos := Pos(']', intxt);
          //  remove any trailing bracket (clobbers conversion).
          if brackpos > 0 then
            Delete(intxt, brackpos, 1);
        until brackpos = 0;

        repeat                  // do all conversions
          if Length(intxt) < 1    // avoid string errors below.
          then begin
            intxt := '0';
            input_box.Text := '0';    // show it
            BREAK;
          end;

          in_char1 := intxt[1];
          // first character might be a conversion factor code or negative...
          // multiply all the conversion factors and / or set the trig flags.

          if (in_char1 = '[') or (in_char1 = '-') then
          begin                         // either makes it neg.
            Delete(intxt, 1, 1);
            make_neg := True;
          end;


          if conversions_checkbox.Checked = True then
            case in_char1 of

              'a', 'A': begin                     // custom factors a,b,c,d ...
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * custom_input_factor1;
              end;

              'b', 'B': begin
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * custom_input_factor2;
              end;

              'c', 'C': begin
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * custom_input_factor3;
              end;

              'd', 'D': begin
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * custom_input_factor4;
              end;

              'm', 'M': begin                              //  m = mm input to inches
                Delete(intxt, 1, 1);
                conv_factor := conv_factor / 25.4;
              end;

              'i', 'I': begin                     //  i = inches input to mm
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * 25.4;
              end;

              'e', 'E': begin                     //  e =  8ths of inch input to mm
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * 25.4 / 8;
              end;

              'h', 'H': begin
                //  h = sixteenths of inch input to mm
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * 25.4 / 16;
              end;

              't', 'T': begin                     //  t =  32nds of inch input to mm
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * 25.4 / 32;
              end;

              'f', 'F': begin                     //  f =  ft input to inches   206a
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * 12;    // 206a
                //conv_factor:=conv_factor*25.4/64;
              end;

              'p', 'P': begin
                //  p = prototype inches input to model (scale) mm
                Delete(intxt, 1, 1);
                conv_factor := conv_factor * inscale;
              end;

              's', 'S': begin
                //  s = model (scale) mm input to prototytpe (full-size) inches
                Delete(intxt, 1, 1);
                conv_factor := conv_factor / inscale;
              end;

              'k', 'K': begin                     //  k =  degrees input to RAM
                Delete(intxt, 1, 1);
                k_ram := True;
              end;

              'n', 'N': begin                     //  n = RAM input to degrees
                Delete(intxt, 1, 1);
                ram_k := True;
              end;

              'g', 'G': begin                     //  g = gradient % to RAM
                Delete(intxt, 1, 1);
                g_ram := True;
              end;

              'q', 'Q': begin                     //  q = CLM to RAM    added 216a
                Delete(intxt, 1, 1);
                clm_ram := True;
              end;

              ';', '#': begin                          // ';' added 0.82.b
                Delete(intxt, 1, 1);
                modify_data_prefixed := True;  // 0.82.a  default no # prefix
              end;

              else
                conversions_done := True;
            end//case

          else
            conversions_done := True;

        until conversions_done = True;

        if intxt = '' then begin
          intxt := '0';
          input_box.Text := '0';
        end;

        try                               // now convert to a float.
          invalue := StrToFloat(intxt);
          good_value := True;
        except
          invalue := orgdim;
          input_box.Clear;
          good_value := False;
        end;//try

        invalue := invalue * ABS(conv_factor);
        // do any conversions (factor is always +ve, non-zero).
        // (must do his conversion immediately here so can do checks, redo displays valid input, etc.)

        if make_neg = True then
          invalue := 0 - ABS(invalue);      //  ensure negative if no minus sign.

        try
          if k_ram = True then
            invalue := 1 / TAN(invalue * Pi / 180);
          if ram_k = True then
            invalue := ARCTAN(1 / invalue) * 180 / Pi;
          if g_ram = True then
            invalue := 100 / invalue;
          if clm_ram = True then
            invalue := 1 / TAN(2 * ARCTAN(1 / (2 * invalue)));      // 216a  covert CLM units to RAM units
        except
          invalue := orgdim;  //error if any trig fails.
          input_box.Clear;
          good_value := False;
        end;//try

        // 0.82.a modify existing data on ; or # prefix

        if modify_data_prefixed = True then begin
          if (k_ram = True) or (ram_k = True) or (g_ram = True) or
            (clm_ram = True) then begin
            invalue := orgdim;                               // error.
            input_box.Clear;
            good_value := False;
          end
          else begin
            invalue := orgdim + invalue;  // modify existing data
            if invalue > max_rad_test then
              invalue := max_rad;
            if invalue < (0 - max_rad_test) then
              invalue := 0 - max_rad;
            if ABS(invalue) < minfp then
              invalue := 0;
          end;
        end;

        if (good_value = False) or ((ABS(invalue) < minfp) and (nozero = True)) or
          ((invalue < 0) and (noneg = True)) then begin
          ok_flag := False;
          //  invalid input characters, or zero or negative not allowed.

          input_panel.Color := $009999FF;      // red -- tell him.

          if good_value = False then
            show_modal_message('Invalid data was entered. Please try again or click ''restore''.')
          else begin

            if (ABS(invalue) < minfp) and (nozero = True)
            then
              show_modal_message('This dimension can not be zero.');

            if (invalue < 0) and (noneg = True)
            then
              show_modal_message('This dimension can not be negative.');

          end;

          input_box.SetFocus;

        end
        else
          ok_flag := True;    //  got valid input in invalue.
      end;

      if ok_flag = True     // set ouput data...
      then begin

        input_panel.ParentColor := True;                   //  in case it was changed to red.

        if (ABS(invalue - round_float(orgdim, dec_places)) > minfp)
          // return data only if modified
          or (data_entry_form.full_fraction_checkbox.Checked = True)   // 206e

        then
          newdim := invalue
        else
          newdim := orgdim;    //  this way cancels any rounding.

        if ABS(newdim - orgdim) < minfp then
          input_box.Font.Color := clBlack     //  no change.
        else
          input_box.Font.Color := clBlue;     //  new value entered.


        if ActiveControl = enter_button then begin
          if bottom_line = max_line then begin
            ok_panel.Show;
            // covers enter button (which remains active)
            set_outlined_panel(bottom_line);   // return focus to this line
          end;


          if bottom_line < max_line then begin
            Inc(bottom_line);
            set_outlined_panel(bottom_line);  // move focus to next line
          end;
        end;


        if ActiveControl = input_box0 then begin
          set_outlined_panel(0);
        end;

        if ActiveControl = input_box1 then begin
          set_outlined_panel(1);
        end;

        if ActiveControl = input_box2 then begin
          set_outlined_panel(2);
        end;

        if ActiveControl = input_box3 then begin
          set_outlined_panel(3);
        end;

        if ActiveControl = input_box4 then begin
          set_outlined_panel(4);
        end;

        if ActiveControl = input_box5 then begin
          set_outlined_panel(5);
        end;

        if ActiveControl = input_box6 then begin
          set_outlined_panel(6);
        end;

        if ActiveControl = input_box7 then begin
          set_outlined_panel(7);
        end;

      end;

    end;//with dims[n]

  end;//with form
end;
//______________________________________________________________________________

procedure Tdata_entry_form.wine_labelClick(Sender: TObject);

begin
  wine_modal_warning('data-entry');
  wine_label.Visible := False;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box0Exit(Sender: TObject);
// CR  form enter_button Default=True

begin
  input_box_exit(0, input_panel0, input_prompt_label0, input_box0);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box1Exit(Sender: TObject);

begin
  input_box_exit(1, input_panel1, input_prompt_label1, input_box1);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box2Exit(Sender: TObject);

begin
  input_box_exit(2, input_panel2, input_prompt_label2, input_box2);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box3Exit(Sender: TObject);

begin
  input_box_exit(3, input_panel3, input_prompt_label3, input_box3);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box4Exit(Sender: TObject);

begin
  input_box_exit(4, input_panel4, input_prompt_label4, input_box4);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box5Exit(Sender: TObject);

begin
  input_box_exit(5, input_panel5, input_prompt_label5, input_box5);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box6Exit(Sender: TObject);

begin
  input_box_exit(6, input_panel6, input_prompt_label6, input_box6);
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box7Exit(Sender: TObject);

begin
  input_box_exit(7, input_panel7, input_prompt_label7, input_box7);
end;
//______________________________________________________________________________

procedure show_item_help(n: integer);

var
  f1_str: string;

begin
  if data_entry_form.more_info_button.Enabled = True then
    f1_str := 'more  general  information'
  else
    f1_str := '';

  if help(0, dims[n].help_str, f1_str) = 1 then
    data_entry_form.more_info_button.Click;

end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button0Click(Sender: TObject);

begin
  show_item_help(0);
  if input_panel0.Showing = True then
    input_box0.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button1Click(Sender: TObject);

begin
  show_item_help(1);
  if input_panel1.Showing = True then
    input_box1.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button2Click(Sender: TObject);

begin
  show_item_help(2);
  if input_panel2.Showing = True then
    input_box2.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button3Click(Sender: TObject);

begin
  show_item_help(3);
  if input_panel3.Showing = True then
    input_box3.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button4Click(Sender: TObject);

begin
  show_item_help(4);
  if input_panel4.Showing = True then
    input_box4.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button5Click(Sender: TObject);

begin
  show_item_help(5);
  if input_panel5.Showing = True then
    input_box5.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button6Click(Sender: TObject);

begin
  show_item_help(6);
  if input_panel6.Showing = True then
    input_box6.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.help_button7Click(Sender: TObject);

begin
  show_item_help(7);
  if input_panel7.Showing = True then
    input_box7.SetFocus;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.conversions_checkboxClick(Sender: TObject);

begin
  if data_entry_form.Showing = True then
    set_outlined_panel(current_line); // return focus
end;
//______________________________________________________________________________

procedure Tdata_entry_form.show_selected_checkboxClick(Sender: TObject);

begin
  input_box0.AutoSelect := show_selected_checkbox.Checked;
  input_box1.AutoSelect := show_selected_checkbox.Checked;
  input_box2.AutoSelect := show_selected_checkbox.Checked;
  input_box3.AutoSelect := show_selected_checkbox.Checked;
  input_box4.AutoSelect := show_selected_checkbox.Checked;
  input_box5.AutoSelect := show_selected_checkbox.Checked;
  input_box6.AutoSelect := show_selected_checkbox.Checked;
  input_box7.AutoSelect := show_selected_checkbox.Checked;

  if show_selected_checkbox.Checked then
    overwrite_checkbox.Checked := False;    // 214a..
  overwrite_checkbox.Enabled := not show_selected_checkbox.Checked;

  if data_entry_form.Showing = True then
    set_outlined_panel(current_line);  // return focus
end;
//______________________________________________________________________________

procedure Tdata_entry_form.ok_buttonClick(Sender: TObject);

begin
  if jot_always_checkbox.Checked = True then
    jot_all_button.Click;
  ModalResult := mrOk;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.ok_panelClick(Sender: TObject);

begin
  if jot_always_checkbox.Checked = True then
    jot_all_button.Click;
  ModalResult := mrOk;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box0Change(Sender: TObject);

// all input boxes come here

begin
  ok_panel.Hide;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.paste_buttonClick(Sender: TObject);

begin
  case current_line of

    0:
      with input_box0 do begin
        Clear;
        PasteFromClipboard;
      end;
    1:
      with input_box1 do begin
        Clear;
        PasteFromClipboard;
      end;
    2:
      with input_box2 do begin
        Clear;
        PasteFromClipboard;
      end;
    3:
      with input_box3 do begin
        Clear;
        PasteFromClipboard;
      end;
    4:
      with input_box4 do begin
        Clear;
        PasteFromClipboard;
      end;
    5:
      with input_box5 do begin
        Clear;
        PasteFromClipboard;
      end;
    6:
      with input_box6 do begin
        Clear;
        PasteFromClipboard;
      end;
    7:
      with input_box7 do begin
        Clear;
        PasteFromClipboard;
      end;

  end;//case

  set_outlined_panel(current_line);  // return focus
end;
//______________________________________________________________________________

procedure Tdata_entry_form.jot_buttonClick(Sender: TObject);

begin
  with jotter_form.jotter_memo.Lines do begin

    Add('');
    Add('_______________________');
    Add('');
    Add(DateToStr(Date) + '   ' + TimeToStr(Time) + '   entered data:');
    Add('');

    case current_line of

      0:
        Add(input_prompt_label0.Caption + ' ' + input_box0.Text + ' ' + input_dims_label0.Caption);
      1:
        Add(input_prompt_label1.Caption + ' ' + input_box1.Text + ' ' + input_dims_label1.Caption);
      2:
        Add(input_prompt_label2.Caption + ' ' + input_box2.Text + ' ' + input_dims_label2.Caption);
      3:
        Add(input_prompt_label3.Caption + ' ' + input_box3.Text + ' ' + input_dims_label3.Caption);
      4:
        Add(input_prompt_label4.Caption + ' ' + input_box4.Text + ' ' + input_dims_label4.Caption);
      5:
        Add(input_prompt_label5.Caption + ' ' + input_box5.Text + ' ' + input_dims_label5.Caption);
      6:
        Add(input_prompt_label6.Caption + ' ' + input_box6.Text + ' ' + input_dims_label6.Caption);
      7:
        Add(input_prompt_label7.Caption + ' ' + input_box7.Text + ' ' + input_dims_label7.Caption);

    end;//case

    Add('_______________________');

  end;//with

  if jotter_form.Showing = False then
    show_modal_message('A copy of the selected line of data has been added to the jotter.'
      + #13 + #13 + 'Press CTRL+J to see the jotter.');

  set_outlined_panel(current_line);  // return focus
end;
//______________________________________________________________________________

procedure Tdata_entry_form.jot_all_buttonClick(Sender: TObject);

begin

  with jotter_form.jotter_memo.Lines do begin

    Add('');
    Add('_______________________');
    Add('');
    Add(DateToStr(Date) + '   ' + TimeToStr(Time) + '   entered data:');
    Add('');

    Add(input_prompt_label0.Caption + ' ' + input_box0.Text + ' ' + input_dims_label0.Caption);

    if current_line > 0 then
      Add(input_prompt_label1.Caption + ' ' + input_box1.Text + ' ' + input_dims_label1.Caption);
    if current_line > 1 then
      Add(input_prompt_label2.Caption + ' ' + input_box2.Text + ' ' + input_dims_label2.Caption);
    if current_line > 2 then
      Add(input_prompt_label3.Caption + ' ' + input_box3.Text + ' ' + input_dims_label3.Caption);
    if current_line > 3 then
      Add(input_prompt_label4.Caption + ' ' + input_box4.Text + ' ' + input_dims_label4.Caption);
    if current_line > 4 then
      Add(input_prompt_label5.Caption + ' ' + input_box5.Text + ' ' + input_dims_label5.Caption);
    if current_line > 5 then
      Add(input_prompt_label6.Caption + ' ' + input_box6.Text + ' ' + input_dims_label6.Caption);
    if current_line > 6 then
      Add(input_prompt_label7.Caption + ' ' + input_box7.Text + ' ' + input_dims_label7.Caption);

    Add('_______________________');

  end;//with

  if jotter_form.Showing = False then
    show_modal_message('A copy of all visible lines of data have been added to the jotter.'
      + #13 + #13 + 'Press CTRL+J to see the jotter.');

  set_outlined_panel(current_line);  // return focus
end;
//______________________________________________________________________________

procedure Tdata_entry_form.cancel_buttonClick(Sender: TObject);

begin
  set_outlined_panel(current_line);

  Close;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.jot_always_checkboxClick(Sender: TObject);

begin
  if data_entry_form.Showing = True then
    set_outlined_panel(current_line); // return focus
end;
//______________________________________________________________________________

procedure Tdata_entry_form.full_fraction_checkboxClick(Sender: TObject);    //206e

begin
  if data_entry_form.Showing = True then
    restore_all_button.Click;
end;
//______________________________________________________________________________

procedure Tdata_entry_form.input_box0KeyPress(Sender: TObject; var Key: Char);

// all input boxes come here

// 214a  overwrite mode ...

begin
  if (Sender is TCustomEdit) and (overwrite_checkbox.Checked = True) and
    (overwrite_checkbox.Enabled = True) then begin
    with TCustomEdit(Sender) do begin
      if (SelLength = 0) and (SelStart < Length(Text)) then begin
        case Key of
          ' '..#126, #128..#255:
            SelLength := 1;    // select next character
        end;//case
      end;
    end;//with
  end;
end;
//______________________________________________________________________________

end.
