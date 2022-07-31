
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
unit xing_select;

{$MODE Delphi}

interface

uses
  LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Txing_select_form = class(TForm)
    datestamp_label: TLabel;
    ok_panel: TPanel;
    xing_geo_label: TLabel;
    xing_angle_label: TLabel;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    xing_geo_listbox: TListBox;
    xing_angle_listbox: TListBox;
    ok_button: TButton;
    straight_radio_panel: TPanel;
    straight_default_radio: TRadioButton;
    straight_ask_radio: TRadioButton;
    return_curve_panel: TPanel;
    adjacent_radio: TRadioButton;
    other_radio: TRadioButton;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    other_label: TLabel;
    help_button: TButton;
    cancel_panel: TPanel;
    cancel_button: TButton;
    restore_settings_button: TButton;
    retain_entry_straight_on_make_checkbox: TCheckBox;
    return_label: TLabel;
    straight_label: TLabel;
    retain_entry_straight_on_mint_checkbox: TCheckBox;
    help_shape: TShape;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure xing_geo_listboxClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ok_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cancel_buttonClick(Sender: TObject);
    procedure restore_settings_buttonClick(Sender: TObject);
    procedure straight_ask_radioClick(Sender: TObject);
    procedure straight_default_radioClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  xing_dialog_help_str: string = 'php/120    `0V-Crossing  Size  and  Type`9' +
    '||Use this window to select the size and type of V-crossing to be used for your turnout template when you don''t want to'
    + ' set the turnout size by using either the `0F5`2 `0size`3 mouse action, or on the `0TEMPLATE > NEW TEMPLATE (QUICK SET)...`1 window.' + '||Select the crossing you require by clicking the lists, and then click the `0OK`z button.' + '||Clicking the `0RESTORE`1 button will restore the previous settings.' + '||The list of crossing angles contains the most common sizes for model use. To use a size not listed' + ' click `0OTHER`1 and `0OK`z. The data-entry form will appear for you to enter the angle you require. Alternatvely click the `0real > V-crossing options > V-crossing angle...`z menu item.' + '||This form is concerned only with the geometry and alignment of the rail running-edges. To change the appearance of the V-crossing, click the' + ' `0real > V-crossing options > customize V-crossing >`z menu items to set the blunt nose, wing rail and check rail lengths, and the timber spacings.' + ' Click the `0REAL > TIMBERING >`1 menu items to set the timbering style and timber sizes.';

  v_xing_angle_help_str: string = 'php/120    `0V-Crossing  Sizes  and  Unit  Angles`9'
    + '||The V-crossing part of a turnout is the arrangement of rails and fittings which permit the left-hand rail of one diverging road' + ' to cross the right-hand rail of the other diverging road. This part of a turnout is sometimes called the "frog".' + '||The size of a crossing is determined by specifying the angle between these two rails at their intersection point.' + ' In British practice this angle is always specified as a ratio or unit angle. A crossing angle of 1:6, for example, means that the' + ' running edges of the rails are 1ft apart at a distance of 6ft from the intersection point. Note that this is an inverse measure,' + ' meaning that as the unit angle figure increases, so the angle between the rails decreases.' + '||In small-scale models we are always short of space, and generally have to settle for shorter turnouts than full-size practice would use in the same' + ' situation. So the suggested minimum crossing angles for model use are these:' + '||In yards and sidings 1:5 minimum.' + '||Slow-speed crossovers in running lines with little curving or straight 1:6 minimum.' + '||Higher-speed crossovers in running lines or where there is moderate curving 1:8 minimum.' + '||Crossovers and junctions in sharply curved running lines 1:10 minimum.' + '||If the curving radius is moderately negative, one full size less can be used, i.e. 1:4 minimum for yards, etc.' + ' If there is significant negative curving, producing a Y-turnout effect, two full sizes less can be used, i.e. 1:3 minimum for yards, etc.' + '||The intersection point mentioned above is called the "Fine Point", and Templot0 marks this point with a small cross. Note that in full-size' + ' practice this point is not the tip of the vee rails. If the vee was made infinitely sharp the tip would be damaged by the first wheel which ran over it.' + ' The normal practice in bullhead track is to blunt the vee back to a nose width at the tip of 3/4".' + '||If you are using exact-scale flangeways you can follow prototype practice and blunt the nose of your vees accordingly. If you are using wider' + ' than scale flangeways, you will usually get better running by using sharp vees located at or near the fine point mark.' + ' Templot0 can draw vees blunt or sharp, click the REAL > CUSTOMIZE V-CROSSING > BLUNT NOSE... menu item to set the nose width you require.' + '||Because the rail chairs and other fittings have to be made to match the crossing angle, crossings normally conform to a' + ' fixed set of sizes increasing in 1/4 unit steps. For example  1:5.75,  1:6,  1:6.25, etc. If the calculations for a complex junction' + ' require an in-between size, the normal practice is to use the nearest standard size and adjust the rails slightly to fit.' + '||When you are adjusting the crossing angle with the mouse action (F5), Templot0 normally snaps the crossing angle to the nearest 1/4 unit step.' + ' You can change this behaviour by selecting the ACTION > V-CROSSING ANGLE OPTIONS menu items.' + '||( N.B. Within Templot, all crossing angles use the R.A.M. unit measure. For an explanation of this and how to specify C.L.M. units instead,' + ' see the help information in the PROGRAM > EXPERT > UNIT ANGLES menu item on the PROGRAM PANEL menus.)';


  v_xing_types_help_str: string = 'php/120    `0Types  of  V-Crossing`9' +
    '||Templot0 offers you four types of V-crossing geometry. For diagrams and further notes explaining the dimensioning of V-crossings,' + ' see "Real Track" in the Templot0 Companion pages on the Templot web site at  templot.com .' + '||The four types of V-crossing geometry are:' + '|--------------------' + '||REGULAR V-CROSSING : This is the normal type of crossing used in crossovers and junctions. The radius in the turnout road between the switch and the crossing' + ' finishes a little way short of the crossing, and both roads through the crossing then continue at the radius of the main road.' + '||In other words, if the whole turnout is on a straight, both roads through the crosssing will be straight.' + ' If the turnout is on a curve, the curving radius applies to both roads through the crossing, i.e. the turnout road is curved to the same radius through the crossing as the main road.' + '||Templot0 generates regular V-crossings with a short length of entry "straight" in front of the fine point, sufficient to ensure that the knuckle point between' + ' the wing rails is on the "straight" portion. The limit of the turnout curve is marked by a RADIAL END mark across between the rails.' + '||By entry "straight" in quotes is meant that this section will be straight only if the whole turnout is on a straight. If the turnout is on a curve, this section will be curved to the same radius as the main road.' + '||If the `0AUTO-FIT`1 option button is selected (recommended), Templot0 will calculate a suitable (usually short) length for the entry "straight".' + ' If a short switch size is combined with a very flat crossing angle, the entry "straight" length will be automatically increased to ensure that the turnout radius does not exceed the switch radius, in accordance with prototype practice.' + '||If the `0FIXED AT...`1 option button is selected, after clicking OK, you can enter some other (longer) length for the entry "straight".' + ' This makes it possible to shorten the length of a turnout if necessary and will give an easier run through the crossing, but at the expense of a significantly smaller turnout curve radius.' + ' It will usually be more convenient to adjust the entry-straight by means of the `0SHIFT-F11`2 mouse action. The `0FIXED AT...`1 option is not available for regular-type half-diamond templates.' + '||If the `0RETAIN ON NEW MINT`1 box is ticked, your fixed length entry-straight setting will be retained when you create a new mint template using the `0NEW`z tool-button or the `0TEMPLATE > NEW TEMPLATE (QUICK SET)...`1 menu item.' + ' Otherwise, the setting will return to `0AUTO-FIT`1 on new mint templates. If you tick this box, be sure to untick it when no longer needed.' + '||If the `0RETAIN ON TOOLS:MAKE`1 box is ticked, your fixed length entry-straight setting will be retained on subsequent templates created using the `0TOOLS > MAKE...`1 functions. For example on the second turnout of a crossover.' + ' Otherwise, the setting will return to `0AUTO-FIT`1 on such templates. If you tick this box, be sure to untick it when no longer needed.' + '|--------------------' + '||GENERIC V-CROSSING : This is similar to a regular crossing, but in this type of V-crossing there is no entry "straight" and the turnout curve always finishes at the fine point.' + ' This gives the maximum possible turnout radius.' + '||Be aware, however, that if you select a short switch size with a very flat generic crossing a situation could arise in which the turnout radius is greater than the switch radius, which is incorrect prototype practice.' + ' You can check the actual switch and turnout radii in the INFO area on the information panel.' + '|--------------------' + '||CURVIFORM V-CROSSING : In this type of V-crossing the turnout radius continues through and beyond the crossing unchanged. There is no radial end mark and no entry "straight".' + ' A curviform V-crossing requires greater care in construction to ensure correct rail alignments through the crossing, but is useful when the tracks need to diverge more quickly,' + ' as often in yards and sidings. Curviform V-crossings are also used in irregular-type diamond-crossings and also when creating symmetrical Y-turnouts with negative curving.' + '||Do not use curviform V-crossings in turnouts forming a crossover, as this would produce an instant reverse curve at the mid-point, leading to rough running.' + '|--------------------' + '||PARALLEL V-CROSSING : This is a special-purpose type of regular crossing in which after passing through the crossing the turnout road curves back towards the main road' + ' until both roads are parallel forming double track. This is a useful way of getting a smooth entry into a running loop, especially when the turnout is significantly curved or on a transition.' + '||The curving back part of the turnout road is called a "return curve". If the turnout itself is sharply curved it is possible for the return curve to be actually straight, or curved in the same direction as the main road.' + '||If the AS ADJACENT TRACK option button is set, the spacing between the two roads will correspond to your current turnout-side adjacent track centres setting (GEOMETRY > ADJACENT TRACK CENTRES... menu item).' + '||If the OTHER... option button is set, after clicking OK, you can enter some other dimension for the track spacing for parallel crossings.' + '||You may prefer to use instead the TOOLS > MAKE RETURN CURVE menu item to produce parallel tracks, which gives you more control over the alignment and timbering' + ' (but unlike a parallel crossing is not available when the template contains a transition curve or slewing).';


var
  xing_select_form: Txing_select_form;

procedure get_xing(var xing_k_i: integer; var xing_list_i: integer;
  var xing_sl_i: integer; var xing_ret_i: integer);

//______________________________________________________________________________

implementation

{$R *.lfm}

uses pad_unit, math_unit, control_room, colour_unit, help_sheet, alert_unit;

var
  si, li, ki: integer;
  reti: integer;

  retain_mint, retain_make: boolean;    // 212b

//____________________________________________________________________________________


procedure get_xing(var xing_k_i: integer; var xing_list_i: integer; var xing_sl_i: integer;
  var xing_ret_i: integer);

begin
  ki := xing_k_i;
  li := xing_list_i;
  si := xing_sl_i;
  reti := xing_ret_i;

  if tradius_is_straight = True then
    si := 0;   // 212b

  retain_mint := retain_entry_straight_on_mint;   // 212b
  retain_make := retain_entry_straight_on_make;   // 212b

  do_show_modal(xing_select_form);     // 212a  ShowModal

  xing_k_i := ki;
  xing_list_i := li;
  xing_sl_i := si;
  xing_ret_i := reti;

  retain_entry_straight_on_mint := retain_mint;   // 212b
  retain_entry_straight_on_make := retain_make;   // 212b

end;
//_______________________________________________________________________________________

procedure enable_radio;

begin
  with xing_select_form do begin
    if (xing_geo_listbox.ItemIndex < 2) and (tradius_is_straight = False)
    // 212b allow irregular half-diamonds  was (half_diamond=False)       //  no straight for curviform or generic crossings.
    then begin
      straight_default_radio.Enabled := True;
      straight_ask_radio.Enabled := True;
      retain_entry_straight_on_mint_checkbox.Enabled := straight_ask_radio.Checked;
      // 212b
      retain_entry_straight_on_make_checkbox.Enabled := straight_ask_radio.Checked;
      // 212b
      straight_label.Enabled := True;
    end
    else begin
      straight_default_radio.Enabled := False;
      straight_ask_radio.Enabled := False;
      retain_entry_straight_on_mint_checkbox.Enabled := False;  // 212b
      retain_entry_straight_on_make_checkbox.Enabled := False;  // 212b
      straight_label.Enabled := False;
    end;

    if xing_geo_listbox.ItemIndex = 1 then begin
      adjacent_radio.Enabled := True;   // parallel crossing.
      other_radio.Enabled := True;
      return_label.Enabled := True;
    end
    else begin
      adjacent_radio.Enabled := False;
      other_radio.Enabled := False;
      return_label.Enabled := False;
    end;
  end;//with
end;
//__________________________________________________________________________________________

procedure restore_click;

begin
  with xing_select_form do begin
    xing_angle_listbox.ItemIndex := ki;
    xing_angle_listbox.SetFocus;
    xing_geo_listbox.ItemIndex := li;

    case si of
      0:
        straight_default_radio.Checked := True;
      1:
        straight_ask_radio.Checked := True;
      else
        run_error(46);
    end;//case

    case reti of
      0:
        adjacent_radio.Checked := True;
      1:
        other_radio.Checked := True;
      else
        run_error(48);
    end;//case

    retain_entry_straight_on_mint_checkbox.Checked := retain_mint;
    retain_entry_straight_on_make_checkbox.Checked := retain_make;

  end;//with
  enable_radio;
end;
//_________________________________________________________________________________________

procedure Txing_select_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.
  if Key = VK_F1 then
    help_button.Click;
end;
//____________________________________________________________________________________________

procedure Txing_select_form.xing_geo_listboxClick(Sender: TObject);

begin
  enable_radio;
end;
//________________________________________________________________________________________

procedure Txing_select_form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin                                      //  don't let him close the form.
  restore_click;
end;
//__________________________________________________________________________________________

procedure Txing_select_form.ok_buttonClick(Sender: TObject);

begin
  ki := xing_angle_listbox.ItemIndex;
  li := xing_geo_listbox.ItemIndex;

  if straight_default_radio.Checked = True then
    si := 0;
  if straight_ask_radio.Checked = True then
    si := 1;

  reti := 0;

  retain_mint := retain_entry_straight_on_mint_checkbox.Checked;   // 212b
  retain_make := retain_entry_straight_on_make_checkbox.Checked;   // 212b

  ModalResult := mrOk;     // not set at design time, as the OK panel click comes here.
end;
//___________________________________________________________________________________________

procedure Txing_select_form.FormShow(Sender: TObject);

begin
  restore_click;
end;
//_____________________________________________________________________________________________

procedure Txing_select_form.restore_settings_buttonClick(Sender: TObject);

begin
  restore_click;
end;
//________________________________________________________________________________________

procedure Txing_select_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag
  // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                                           // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                                           // scale the form contents up.


  size_updown.Tag := size_updown.Position;                           // and save for the next click.
end;
//__________________________________________________________________________________________

procedure Txing_select_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  crossing  selector  dialog', Color);
end;
//____________________________________________________________________________________________

procedure Txing_select_form.help_buttonClick(Sender: TObject);

begin
  help(0, xing_dialog_help_str + '||' + v_xing_angle_help_str + '||' + v_xing_types_help_str, '');
  // 0.93.a
end;
//__________________________________________________________________________________________

procedure Txing_select_form.FormCreate(Sender: TObject);

begin
  if Screen.Height < 500 then
    Top := 2;    // move form up the screen for lo-res.

  // OT-FIRST ClientWidth:=482;
  // OT-FIRST ClientHeight:=398;
  AutoScroll := False;
end;
//______________________________________________________________________________

procedure Txing_select_form.cancel_buttonClick(Sender: TObject);

begin
  restore_settings_button.Click;
  ModalResult := mrCancel;
end;
//______________________________________________________________________________

procedure Txing_select_form.straight_ask_radioClick(Sender: TObject);   // 212b

begin
  retain_entry_straight_on_mint_checkbox.Enabled := True;
  retain_entry_straight_on_make_checkbox.Enabled := True;
end;
//______________________________________________________________________________

procedure Txing_select_form.straight_default_radioClick(Sender: TObject);  // 212b

begin
  retain_entry_straight_on_mint_checkbox.Checked := False;
  retain_entry_straight_on_mint_checkbox.Enabled := False;

  retain_entry_straight_on_make_checkbox.Checked := False;
  retain_entry_straight_on_make_checkbox.Enabled := False;
end;
//______________________________________________________________________________

end.
