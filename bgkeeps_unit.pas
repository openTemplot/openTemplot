
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

                >>>     NOTE TO DEVELOPERS     <<<
                     DO NOT EDIT THIS COMMENT
              It is inserted in this file by running
                  'python3 scripts/addComment.py'
         The original text lives in scripts/addComment.py.

====================================================================================
*)

unit bgkeeps_unit;

{$MODE Delphi}

interface

uses
  LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Tbgkeeps_form = class(TForm)
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    datestamp_label: TLabel;
    help_button: TButton;
    close_panel: TPanel;
    close_button: TButton;
    show_groupbox: TGroupBox;
    timber_outlines_checkbox: TCheckBox;
    timber_centres_checkbox: TCheckBox;
    gauge_faces_checkbox: TCheckBox;
    marks_checkbox: TCheckBox;
    joints_checkbox: TCheckBox;
    centres_checkbox: TCheckBox;
    peg_checkbox: TCheckBox;
    outer_edges_checkbox: TCheckBox;
    template_number_checkbox: TCheckBox;
    template_name_checkbox: TCheckBox;
    reduced_ends_checkbox: TCheckBox;
    timber_infill_checkbox: TCheckBox;
    Label1: TLabel;
    timber_numbering_checkbox: TCheckBox;
    help_shape: TShape;
    apply_button: TButton;
    platforms_checkbox: TCheckBox;
    trackbed_edges_checkbox: TCheckBox;
    template_id_checkbox: TCheckBox;
    Label2: TLabel;
    bold_timber_outlines_checkbox: TCheckBox;
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure how_panelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure close_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure timber_numbering_checkboxClick(Sender: TObject);
    procedure timber_infill_checkboxClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure apply_buttonClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  bgkeeps_form: Tbgkeeps_form;

//__________________________________________________________________________________________

implementation

{$R *.lfm}

uses
  control_room, colour_unit, help_sheet, pad_unit, alert_unit, keep_select, math_unit, prefs_unit;

//_________________________________________________________________________________________

procedure Tbgkeeps_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

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

procedure Tbgkeeps_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  window  colour  for  the  background  templates  detail',
    Color);
end;
//__________________________________________________________________________________________

procedure Tbgkeeps_form.how_panelClick(Sender: TObject);

const
  bgkeeps_help: string = '      `0Background  Templates  Detail`9' +
    '||These tick boxes let you customize the way in which the detail elements of background templates are shown on the trackpad (screen).' + '||If you are building a complete track plan in the background, it is often less confusing to display the background templates' + ' in skeleton form only, with the rail outer-edges and perhaps the timbers omitted.' + '||Clicking the `0APPLY NOW`1 button causes the trackpad to be immediately re-drawn reflecting any changes you have made to the tick boxes.' + '||green_panel_begin tree.gif These tick boxes apply to the trackpad (screen) only and have no effect on the printed templates, PDF files, exported image files, sketchboard, or any other output.' + '||To make similar changes on the output, change the settings in the `0OUTPUT`1 menu and sub-menus.green_panel_end';

begin
  help(0, bgkeeps_help, '');
end;
//______________________________________________________________________________________

procedure Tbgkeeps_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.
end;
//____________________________________________________________________________________

procedure Tbgkeeps_form.close_buttonClick(Sender: TObject);

begin
  Close;
  redraw_pad(True, False);
end;
//_____________________________________________________________________________________

procedure Tbgkeeps_form.FormCreate(Sender: TObject);

begin
  ClientWidth := 730;
  ClientHeight := 248;
  AutoScroll := True;
end;
//______________________________________________________________________________________

procedure Tbgkeeps_form.timber_numbering_checkboxClick(Sender: TObject);

begin
  if (timber_numbering_checkbox.Checked = True) and (loading_his_prefs = False)  // 208a
  then
    alert(3, '    `0timber  numbering`9',
      '|You have selected timber numbering to be shown on the screen for all the background templates.'
      + '||Bear in mind that the numbers are likely to obscure most of the drawing as you zoom out.'
      + '||green_panel_begintree.gif There is seldom any need to have the background timber numbers on the screen, timber numbering is intended primarily for printed full-size track construction templates.' + '||And for the control template when using the shove timber functions.green_panel_end' + '|You can change the font used for the numbers, click the `0TRACKPAD > TRACKPAD CONTROL TEMPLATE OPTIONS > FONT FOR TIMBER NUMBERING...`1 menu item.',
      '', '', '', '', '', 'O K', 0);
end;
//________________________________________________________________________________________

procedure Tbgkeeps_form.timber_infill_checkboxClick(Sender: TObject);

begin
  if bgkeeps_form.timber_infill_checkbox.Checked = True then begin
    if bgpad_timb_infill_style = 0 then
      pad_form.bg_solid_timber_infill_menu_entry.Click;
  end;
  //redraw(True);
end;
//_____________________________________________________________________________________

procedure Tbgkeeps_form.FormDeactivate(Sender: TObject);

begin
  redraw_pad(True, False);
end;
//___________________________________________________________________________________________

procedure Tbgkeeps_form.apply_buttonClick(Sender: TObject);

begin
  if any_bgnd = 0 then begin
    alert(6, '    no  templates  currently  on  background',
      'There are no stored templates currently copied to the background drawing, and therefore none to have these settings applied to them.',
      '', '', '', '', '', 'O K', 0);
    EXIT;
  end;

  redraw_pad(True, False);
end;
//__________________________________________________________________________________________

end.
