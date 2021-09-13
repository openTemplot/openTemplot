
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

unit mint_unit;

{$MODE Delphi}

interface

uses
  LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Htmlview, Grids, Outline{, DirOutln}, HtmlGlobals;

type

  { Tmint_form }

  Tmint_form = class(TForm)
    size_groupbox: TGroupBox;
    curving_groupbox: TGroupBox;
    left_hand_radio_button: TRadioButton;
    right_hand_radio_button: TRadioButton;
    posrad_radio_button: TRadioButton;
    negrad_radio_button: TRadioButton;
    radius_combo: TComboBox;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    datestamp_label: TLabel;
    ok_panel: TPanel;
    ok_button: TButton;
    turnout_combo: TComboBox;
    cancel_panel: TPanel;
    cancel_button: TButton;
    rad_label: TLabel;
    model_rad_label: TLabel;
    joggled_rails_checkbox: TCheckBox;
    help_button: TButton;
    try_button: TButton;
    plain_track_button: TButton;
    straight_button: TButton;
    mint_html_view: THTMLViewer;
    mint_current_button: TButton;
    zoom_fit_button: TButton;
    red_label: TLabel;
    reset_button: TButton;
    procedure mint_html_viewHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure cancel_panelClick(Sender: TObject);
    procedure ok_panelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure radius_comboChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure plain_track_buttonClick(Sender: TObject);
    procedure straight_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure zoom_fit_buttonClick(Sender: TObject);
    procedure try_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mint_form: Tmint_form;

//------------------------

function quick_set_radius(var radius: double): boolean;

//____________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


uses
  pad_unit, colour_unit, help_sheet, math_unit, control_room, styleun;

{$R *.lfm}

const
  radii: array[0..13] of integer = (0, 200, 250, 300, 400, 500, 600, 750, 1000, 1500, 2000, 330, 660, 1320);

//__________________________________________________________________________________________

procedure Tmint_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  mint  template  dialog', Color);
end;
//_________________________________________________________________________________________

procedure Tmint_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag       // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                        // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                       // scale the form contents up.

  ClientHeight := VertScrollBar.Range;           // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 4;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;           // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;       // and save for the next click.
end;

procedure Tmint_form.mint_html_viewHotSpotClick(Sender: TObject; const SRC: ThtString;
  var Handled: Boolean);
begin
  htmlviewer_hot_spot_clicked(Sender, SRC, Handled);
end;

//___________________________________________________________________________________________

procedure Tmint_form.help_buttonClick(Sender: TObject);

const
  mint_str: string = '      `0New  Template       Quick - Set  Panel`9' +
    '||Use the controls on this panel to set a new mint control template matching the most common UK-pattern bullhead turnout sizes.'
    + '||For an explanation of what is meant by the control template, please click <A HREF="go_to_templot_explained.85a">Templot Explained</A> .' + '||A `0mint`3 template is a new control template with the following settings:' + '||  small_bullet  regular-pattern V-crossing' + '|  small_bullet  auto-fit V-crossing-entry straight' + '|  small_bullet  normal length turnout road' + '|  small_bullet  no approach track' + '|  small_bullet  2 sleepers of exit track' + '|  small_bullet  no slewing or transition curving' + '|  small_bullet  no blanking' + '|  small_bullet  no gaunt turnout' + '|  small_bullet  rails on' + '|  small_bullet  timbering on' + '|  small_bullet  no shoved timbers' + '|  small_bullet  no bonus timbers' + '|  small_bullet  no timber randomizing' + '|  small_bullet  normal plain-track rail joints' + '|  small_bullet  fixing peg in the CTRL-0 reset position' + '|  small_bullet  normal centre-lines' + '|  small_bullet  no template name' + '|  small_bullet  memo notes cleared' + '||  small_bullet  A plain track template will be set to 2 chains (132ft) scale length.' + '||  small_bullet  If an REA switch size is current, any customizing of the V-crossing will be cancelled and returned to the pre-set REA bullhead dimensions.' + '||  small_bullet  If the existing control template is a half-diamond, it will be converted to a turnout template. It can be converted back to a half-diamond by clicking the `0TEMPLATE > CONVERT TURNOUT TO HALF-DIAMOND`1 menu item.' + '||If the `0MINT FROM CURRENT`1 button is clicked, other settings remain unchanged from your previous control template.' + '||Otherwise use the option buttons and drop-down lists to select the new turnout size and curving radius for the control template.' + '||The turnout sizes shown are based on the REA-pattern* switches (A,B,C, etc.) which are familiar to UK modellers from published pointwork plans and commercially available components.' + ' You can choose to have the stock rails joggled for straight-cut switch planing by ticking the `0JOGGLED STOCK RAILS`1 option box.' + '||Setting a `0positive`3 radius causes the main road to curve in the same direction as the hand. A `0negative`3 radius curves in the opposite direction (Y-turnout).' + ' The radii shown in the list are full-size prototype dimensions. The equivalent model radius at your current scale is shown above it.' + '||When you click `0OK`z the new control template will appear in the centre of the trackpad, ready for you to print (`0F11`2), adjust as required, or incorporate into your background track plan drawing.' + '||If the existing control template is on the datum (green dot), the new template will also be located on the datum and the trackpad view will zoom in or out as necessary to just fit the new template.' + '||Otherwise the zoom settings will remain unchanged.' + '||tree.gif  `0handy hints:`4' + '||A much wider range of settings, including flat-bottom, loose-heel, and GWR-pattern switches, and different types of V-crossing, is available by clicking the' + '||`0TEMPLATE > SWITCH SETTINGS...`1' + '|`0template > V-crossing settings...`z' + '|`0GEOMETRY > RADIUS AND CURVING DATA...`1' + '||and other menu items, or the corresponding links on this panel.' + '||If plain track is set, the above turnout settings will apply if the `0TEMPLATE > INSERT TURNOUT IN PLAIN TRACK`1 menu item is clicked subsequently.' + '||* `0REA - Railway Engineers Association. The REA designs were used for bullhead renewals on all non-GWR and non-BR(W) lines from about 1925 on. Earlier designs remained in use for many years and can be set by clicking the above menu items.`7';

var
  str: string;

begin
  red_label.Hide;   // only first time

  str := StringReplace(mint_str, 'tree.gif', '<img src="' + exe_str +
    'internal\hlp\tree_symbol.gif">', [rfReplaceAll, rfIgnoreCase]);

  help(0, str, '');
end;
//_____________________________________________________________________________________________

procedure Tmint_form.cancel_panelClick(Sender: TObject);

begin
  ModalResult := mrCancel;
end;
//________________________________________________________________________________________

procedure Tmint_form.ok_panelClick(Sender: TObject);

begin
  ModalResult := mrOk;
end;
//__________________________________________________________________________________________

procedure update_model_radius;

var
  i: integer;

begin
  with mint_form do begin
    i := radius_combo.ItemIndex;
    case i of
      0:
        model_rad_label.Caption := 'straight';
      1..13:
        model_rad_label.Caption := round_str(radii[i] * scale, 0) + ' mm  ( ' + round_str(
          radii[i] * scale / 25.4, 1) + ' ins )';
      else
        model_rad_label.Caption := '';    // ?? 0..13 hard-coded for array and combo-box.
    end;//case
  end;//with
end;
//________________________________________________________________________________

procedure Tmint_form.FormActivate(Sender: TObject);

begin
  update_model_radius;
end;
//_____________________________________________________________________________________

procedure Tmint_form.radius_comboChange(Sender: TObject);

begin
  update_model_radius;
end;
//_____________________________________________________________________________________

function quick_set_radius(var radius: double): boolean;

var
  rad_index: integer;
  rad_sgn: double;

begin
  Result := False;     // defaults init..
  radius := 0;         // straight

  rad_index := mint_form.radius_combo.ItemIndex;

  if mint_form.posrad_radio_button.Checked = True then
    rad_sgn := 1
  else
    rad_sgn := -1;

  case rad_index of
    0..13:
      radius := radii[rad_index] * scale * rad_sgn;
    else
      EXIT;
  end;//case

  Result := True;
end;
//_________________________________________________________________________________________

procedure Tmint_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.
end;
//__________________________________________________________________________________________

procedure Tmint_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=682;
  // OT-FIRST ClientHeight:=330;
  AutoScroll := True;
end;
//______________________________________________________________________________

procedure Tmint_form.plain_track_buttonClick(Sender: TObject);

begin
  turnout_combo.ItemIndex := 0;
  ok_button.SetFocus;
end;
//______________________________________________________________________________

procedure Tmint_form.straight_buttonClick(Sender: TObject);

begin
  radius_combo.ItemIndex := 0;
  model_rad_label.Caption := 'straight';
  ok_button.SetFocus;
end;
//______________________________________________________________________________

procedure Tmint_form.FormShow(Sender: TObject);

var
  html_str: string;
  save_ppi: integer;

begin

  html_str := '<TABLE STYLE="font-family:''' + arial_str +
    '''"><TR><TD STYLE="padding-left:22px; padding-bottom:16px; font-size:13px; color:#000000; line-height:125%;">'
    + 'Use the quick controls on the right to set a new mint control template which matches the most common UK-pattern bullhead turnout sizes.' + '<BR><BR>A mint turnout template has no approach track, and 2 sleepers of exit track.</TD></TR>' + '<TR><TD STYLE="border:solid 1px #C0C0C0; padding:2px 0px 6px 6px; color:#800000; font-size:13px; background-color:#FFF0D0;">' + '<img src="' + exe_str + 'internal\hlp\tree_symbol.gif"> &nbsp;Alternatively, for a much wider range of sizes, including GWR and flat-bottom switches, click:' + '<SPAN STYLE="font-size:12px; font-weight:bold;"><BR><BR><A HREF="switch_options.85a">switch options</A>' + '&nbsp; &nbsp;<A HREF="crossing_options.85a">V-crossing options</A>' + '&nbsp; &nbsp;<A HREF="curving_radius.85a">curving radius</A><BR><BR></SPAN>' + 'In addition ' + Application.Title + ' can create turnouts of almost any size or prototype using the custom settings.' + '</TD><TD>&nbsp;</TD></TR></TABLE>';

  mint_html_view.LoadFromString(html_str);
end;
//______________________________________________________________________________

procedure Tmint_form.zoom_fit_buttonClick(Sender: TObject);

begin
  pad_form.fit_current_only_menu_entry.Click;
  zoom_fit_button.Enabled := False;
  ok_button.SetFocus;
end;
//______________________________________________________________________________

procedure Tmint_form.try_buttonClick(Sender: TObject);

begin
  zoom_fit_button.Enabled := True;
  mint_current_button.Enabled := False;
end;
//______________________________________________________________________________

end.
