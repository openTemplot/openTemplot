
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
unit panning_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons;

type
  Tpanning_form = class(TForm)
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    coarse_trackbar: TTrackBar;
    close_panel: TPanel;
    close_button: TButton;
    coarse_label: TLabel;
    direction_groupbox: TGroupBox;
    paper_option_button: TRadioButton;
    scroll_option_button: TRadioButton;
    more_button: TButton;
    less_button: TButton;
    edge_panning_groupbox: TGroupBox;
    edge_on_radio_button: TRadioButton;
    edge_off_radio_button: TRadioButton;
    zoom_out_toolbutton: TSpeedButton;
    zoom_in_toolbutton: TSpeedButton;
    zoom_rectangle_latching_toolbutton: TSpeedButton;
    up_panbutton: TBitBtn;
    down_panbutton: TBitBtn;
    left_panbutton: TBitBtn;
    right_panbutton: TBitBtn;
    pad_view_groupbox: TGroupBox;
    view1_button: TButton;
    view2_button: TButton;
    view3_button: TButton;
    view4_button: TButton;
    set_label1: TLabel;
    set_label2: TLabel;
    set_label3: TLabel;
    set_label4: TLabel;
    mid_bar_shape: TShape;
    rollback_panel: TPanel;
    roll_label: TLabel;
    previous_view_button: TButton;
    next_view_button: TButton;
    mouse_wheel_view_checkbox: TCheckBox;
    help_button: TButton;
    mouse_zoom_button: TButton;
    fit_extents_toolbutton: TSpeedButton;
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure more_buttonClick(Sender: TObject);
    procedure less_buttonClick(Sender: TObject);
    procedure edge_off_radio_buttonClick(Sender: TObject);
    procedure edge_on_radio_buttonClick(Sender: TObject);
    procedure zoom_out_toolbuttonClick(Sender: TObject);
    procedure zoom_in_toolbuttonClick(Sender: TObject);
    procedure zoom_rectangle_latching_toolbuttonClick(Sender: TObject);
    procedure up_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure up_panbuttonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure down_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure left_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure right_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure paper_option_buttonClick(Sender: TObject);
    procedure set_label4MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure set_label4Click(Sender: TObject);
    procedure set_label3Click(Sender: TObject);
    procedure set_label2Click(Sender: TObject);
    procedure set_label1Click(Sender: TObject);
    procedure view4_buttonClick(Sender: TObject);
    procedure view3_buttonClick(Sender: TObject);
    procedure view2_buttonClick(Sender: TObject);
    procedure view1_buttonClick(Sender: TObject);
    procedure previous_view_buttonClick(Sender: TObject);
    procedure next_view_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure mouse_zoom_buttonClick(Sender: TObject);
    procedure fit_extents_toolbuttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  panning_form: Tpanning_form;
  //--------------------------------

  pad_view_index: integer = 0;  // 0.91.c

procedure pan_button_click(scroll: double; dir: integer; max_jump: boolean);
// called from pad for edge-panning.

//_________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  control_room, pad_unit, colour_unit, help_sheet, bgkeeps_unit, math_unit;

var
  left_button_down: boolean = False;
  right_button_down: boolean = False;
  up_button_down: boolean = False;
  down_button_down: boolean = False;

//__________________________________________________________________________________________

procedure Tpanning_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag       // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                        // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                        // scale the form contents up.

  ClientHeight := close_panel.top + close_panel.Height + 2;            // allow 2 pixel bottom margin.
  ClientWidth := blue_corner_panel.Left + blue_corner_panel.Width;

  size_updown.Tag := size_updown.Position;        // and save for the next click.
end;
//____________________________________________________________________________________________

procedure Tpanning_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  panning  dialog', Color);
end;
//_____________________________________________________________________________________________

procedure Tpanning_form.close_buttonClick(Sender: TObject);

begin
  Close;//Hide;
end;
//_______________________________________________________________________________________

procedure pan_button_click(scroll: double; dir: integer; max_jump: boolean);
// also called from pad for edge-panning.

// dir= 1=left, 2=right, 3=up, 4=down.   // scroll=-1.0 is paper, +1.0 is scroll.
var
  jump_factor: double;

begin
  if mouse_modify > 0 then
    EXIT;   // mouse actio in progress.

  if draw_mode <> 2 then
    pad_form.lock_scaling_menu_entry.Click;   // 13-12-99.

  with panning_form do begin

    if max_jump = False then
      jump_factor := (coarse_trackbar.Position * 10 + 1) / 3000    // 211b
    else
      jump_factor := 0.1;
  end;//with

  case dir of
    1:
      zoom_offsetx := zoom_offsetx - jump_factor * screenx * scroll;
    2:
      zoom_offsetx := zoom_offsetx + jump_factor * screenx * scroll;
    3:
      zoom_offsety := zoom_offsety + jump_factor * screeny * scroll;
    4:
      zoom_offsety := zoom_offsety - jump_factor * screeny * scroll;
  end;//case

  gocalc(2, 0);           // was mode:=10
end;
//_________________________________________________________________________________________

procedure Tpanning_form.help_buttonClick(Sender: TObject);

const
  panning_str: string = '    `0zoom / panning  controls`9' +
    '||Most of the time in Templot0 you will probably prefer to pan across the trackpad by dragging it with the mouse or by using the arrow keys on the keyboard.' + '||And most of the time in Templot0 you will probably prefer to zoom the trackpad by means of the mouse wheel.' + '||The zoom/pan dialog is provided for occasions when you want more control, or your system does not support the above functions.|<HR>' + '|To use less screen space click the `0<BIG>«</BIG>`1 button repeatedly to shrink the zoom/pan dialog window. To expand the dialog again click the `0<BIG>»</BIG>`1 button repeatedly.' + ' If the zoom/pan dialog has been hidden, press `0CTRL+F2`2 or the `02`2 key, or click the `0pad > zoom/pan options > show zoom/panning controls`1 menu item to show the dialog again.' + '||The `0+`1 and `0-`1 zoom in/out buttons correspond to the `0+`2 and `0-2 (ADD/SUBTRACT) number pad keys.' + ' The amount by which the zoom setting changes for each click can be set by selecting the `0TRACKPAD > ZOOM (EXPLODE/SHRINK) > EXPLODE/SHRINK STEP SIZE...`1 menu item.' + '||Alternatively, click the `0+.:`1 zoom rectangle button and draw a rectangle on the trackpad around the area which you want to zoom into. (Click down on one corner and drag to the other.)' + '||For precise zooming, click the `0zoom by mouse action`1 button to zoom with the mouse.' + '||The panning function moves the field of view across the trackpad continuously.' + '||Click or hold down one of the four blue arrow buttons to pan across the pad. Or alternatively, press or hold down the ARROW keys on the keyboard.' + '||If the `0PAPER`1 option button has been selected the trackpad image' + ' will move in the same direction as the button. If the `0SCROLL`1 option button has been selected the trackpad image will move in the' + ' opposite direction, in the same way as a normal Windows scroll bar.' + '||The speed of movement is controlled by the `0panning speed`1 slider. If the `0CTRL`2 key on the keyboard is held down while using the panning buttons, panning will be at the maximum speed, regardless of the slider setting.' + '||The actual speed of movement in mm per second will be determined by the amount of detail on the pad, the current zoom setting and the speed of your processor and graphics system.' + ' If the keyboard arrow keys are used, the speed will also be determined by the current keyboard repetition rate setting.' + '||If `0SCROLL LOCK`2 key is OFF and the `0EDGE-PANNING > ON`1 option button is currently selected (or the `0TRACKPAD > ZOOM AND PAN OPTIONS > EDGE PANNING ON PER SCROLL LOCK`1 menu item is currently selected),' + ' you can also pan across the trackpad simply by moving the mouse pointer against the edge of the pad.' + '||To allow for access to the menus, this works for the left-hand edge of the trackpad only if the mouse pointer is in the lower 2/3rds of the trackpad, and for the top edge of the trackpad only at the right-hand end.' + ' The speed slider and `0CTRL`2 key operate as for the panning buttons, but the `0PAPER/SCROLL`1 direction buttons are ignored for edge-panning.' + '||Edge panning is occasionally a nuisance, and can be temporarily disabled by pressing the `0SCROLL LOCK`2 key to ON.' + '||Some combinations of speed setting and grid-line spacing will cause the trackpad to appear to be moving in the wrong direction, due to stroboscopic effects.' + ' To avoid this change the speed setting or grid spacing, or switch the trackpad grid off while panning (select the `0TRACKPAD > TRACKPAD GRID OPTIONS`1 menu item).' + '||Handy Hints:' + '||When using the faster speeds it is usually better to make repeated clicks on the panning buttons instead of holding them down. Otherwise you may quickly lose your bearings.' + ' Be aware that the trackpad is virtually limitless in area, you will not come to a stop at the "edge of the paper".' + '||If you do lose your way on the pad, click the `0TRACKPAD > ZOOM/VIEW > FIT ALL BACKGROUND TEMPLATES`1 menu item (or press `0SHIFT-F12`2 or just the `0PAGE UP`2 key).' + '||Alternatively, click the `0TRACKPAD > ZOOM/VIEW > TRACKPAD VIEW RESET MAX`1 menu item. This zooms out to the full available extent, with the drawing origin' + ' located at the centre of the pad. You can then zoom back in as required (using the `0+`2 button or the `0ADD`2 key on the number pad).';

begin
  help(-1, panning_str, '');
end;
//___________________________________________________________________________________________

procedure Tpanning_form.FormCreate(Sender: TObject);

begin

  pad_form.InsertControl(panning_form);

  AutoScroll := False;
end;
//______________________________________________________________________________

procedure Tpanning_form.FormShow(Sender: TObject);    // 0.95.a mods

begin
  if Top < 56 then
    Top := 56;
  // leave room for edge-panning or shrunk jotter.
  if Left > (pad_form.ClientWidth - Width - 10) then
    Left := pad_form.ClientWidth - Width - 10;
end;
//______________________________________________________________________________

procedure Tpanning_form.more_buttonClick(Sender: TObject);

// expand window width in two stages. 0.91.c

begin
  if ClientWidth = (up_panbutton.Left + up_panbutton.Width + 2) then
    ClientWidth := pad_view_groupbox.Left + pad_view_groupbox.Width + 2           // 0.91.c
  else
    ClientWidth := blue_corner_panel.Left + blue_corner_panel.Width;


  if (Left > pad_form.ClientWidth - Width - 10) then
    Left := pad_form.ClientWidth - Width - 10;
  close_button.Cancel := True;   // close on ESC.
  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//_________________________________________________________________________________________

procedure Tpanning_form.less_buttonClick(Sender: TObject);

// reduce window width in two stages. 0.91.c

begin

  if ClientWidth = (blue_corner_panel.Left + blue_corner_panel.Width) then
    ClientWidth := pad_view_groupbox.Left + pad_view_groupbox.Width + 2           // 0.91.c
  else
    ClientWidth := up_panbutton.Left + up_panbutton.Width + 2;

  if (Left > pad_form.ClientWidth - Width - 10) then
    Left := pad_form.ClientWidth - Width - 10;
  close_button.Cancel := False;  // no close on ESC if button not visible.
  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//_________________________________________________________________________________________

procedure Tpanning_form.edge_off_radio_buttonClick(Sender: TObject);

begin
  pad_form.edge_panning_off_menu_entry.Checked := True;   //  radio item.
  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//________________________________________________________________________________________

procedure Tpanning_form.edge_on_radio_buttonClick(Sender: TObject);

begin
  pad_form.edge_panning_on_menu_entry.Checked := True;   //  radio item.
  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//_______________________________________________________________________________________

procedure Tpanning_form.zoom_out_toolbuttonClick(Sender: TObject);

begin
  pad_form.shrink_normal_menu_entry.Click;
end;
//______________________________________________________________________________________
procedure Tpanning_form.zoom_in_toolbuttonClick(Sender: TObject);

begin
  pad_form.explode_normal_menu_entry.Click;
end;
//_____________________________________________________________________________________

procedure Tpanning_form.zoom_rectangle_latching_toolbuttonClick(Sender: TObject);

begin
  pad_form.click_zoom_rectangle_menu_entry.Click;
end;
//___________________________________________________________________________________________

procedure Tpanning_form.up_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  scroll: double;
  ctrl: boolean;

begin
  if scroll_option_button.Checked = True then
    scroll := 1.0
  else
    scroll := -1.0;

  ctrl := (ssCtrl in Shift);      // True if Ctrl-Key down.
  up_button_down := True;
  while up_button_down = True do begin
    pan_button_click(scroll, 3, ctrl);
    if Application.Terminated = False then
      Application.ProcessMessages;
  end;
end;
//_________________________________

procedure Tpanning_form.up_panbuttonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

// one routine for all mouse ups...

// also called from alert_box  205d
begin
  up_button_down := False;
  down_button_down := False;
  left_button_down := False;
  right_button_down := False;

  if panning_form.Active = True then
    blue_corner_panel.SetFocus;        // don't want it here.

end;
//___________________________________________________________________________________________

procedure Tpanning_form.down_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  scroll: double;
  ctrl: boolean;

begin
  if scroll_option_button.Checked = True then
    scroll := 1.0
  else
    scroll := -1.0;

  ctrl := (ssCtrl in Shift);       // True if Ctrl-Key down.
  down_button_down := True;
  while down_button_down = True do begin
    pan_button_click(scroll, 4, ctrl);
    if Application.Terminated = False then
      Application.ProcessMessages;
  end;
end;
//__________________________________________________________________________________________

procedure Tpanning_form.left_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  scroll: double;
  ctrl: boolean;

begin
  if scroll_option_button.Checked = True then
    scroll := 1.0
  else
    scroll := -1.0;

  ctrl := (ssCtrl in Shift);     // True if Ctrl-Key down.
  left_button_down := True;
  while left_button_down = True do begin
    pan_button_click(scroll, 1, ctrl);
    if Application.Terminated = False then
      Application.ProcessMessages;
  end;
end;
//____________________________________________________________________________________________

procedure Tpanning_form.right_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

var
  scroll: double;
  ctrl: boolean;

begin
  if scroll_option_button.Checked = True then
    scroll := 1.0
  else
    scroll := -1.0;

  ctrl := (ssCtrl in Shift);     // True if Ctrl-Key down.
  right_button_down := True;
  while right_button_down = True do begin
    pan_button_click(scroll, 2, ctrl);
    if Application.Terminated = False then
      Application.ProcessMessages;
  end;
end;
//______________________________________________________________________________

procedure Tpanning_form.paper_option_buttonClick(Sender: TObject);

begin
  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//______________________________________________________________________________

// 0.91.c  pad view functions...

procedure Tpanning_form.set_label4MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label4.Color := clBlack;
  set_label4.Font.Color := clWhite;
end;
//________________________________________________________

procedure Tpanning_form.set_label4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label4.Color := pad_view_groupbox.Color;
  set_label4.Font.Color := clBlue;
end;
//______________________________________________________________________________

procedure Tpanning_form.set_label3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label3.Color := clBlack;
  set_label3.Font.Color := clWhite;
end;
//________________________________________________________

procedure Tpanning_form.set_label3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label3.Color := pad_view_groupbox.Color;
  set_label3.Font.Color := clBlue;
end;
//______________________________________________________________________________

procedure Tpanning_form.set_label2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label2.Color := clBlack;
  set_label2.Font.Color := clWhite;
end;
//_______________________________________________________

procedure Tpanning_form.set_label2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label2.Color := pad_view_groupbox.Color;
  set_label2.Font.Color := clBlue;
end;
//______________________________________________________________________________

procedure Tpanning_form.set_label1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label1.Color := clBlack;
  set_label1.Font.Color := clWhite;
end;
//________________________________________________________

procedure Tpanning_form.set_label1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  set_label1.Color := pad_view_groupbox.Color;
  set_label1.Font.Color := clBlue;
end;
//______________________________________________________________________________

procedure Tpanning_form.set_label4Click(Sender: TObject);

begin
  pad_view4.offset_x := zoom_offsetx;
  pad_view4.offset_y := zoom_offsety;
  pad_view4.width_x := screenx;

  view4_button.Enabled := True;
end;
//____________________________

procedure Tpanning_form.set_label3Click(Sender: TObject);

begin
  pad_view3.offset_x := zoom_offsetx;
  pad_view3.offset_y := zoom_offsety;
  pad_view3.width_x := screenx;

  view3_button.Enabled := True;
end;
//____________________________

procedure Tpanning_form.set_label2Click(Sender: TObject);

begin
  pad_view2.offset_x := zoom_offsetx;
  pad_view2.offset_y := zoom_offsety;
  pad_view2.width_x := screenx;

  view2_button.Enabled := True;
end;
//____________________________

procedure Tpanning_form.set_label1Click(Sender: TObject);

begin
  pad_view1.offset_x := zoom_offsetx;
  pad_view1.offset_y := zoom_offsety;
  pad_view1.width_x := screenx;

  view1_button.Enabled := True;
end;
//______________________________________________________________________________

procedure Tpanning_form.view4_buttonClick(Sender: TObject);

begin
  zoom_offsetx := pad_view4.offset_x;
  zoom_offsety := pad_view4.offset_y;
  screenx := pad_view4.width_x;

  do_rollback := False;       // no need to put this in rollback register on redraw.
  redraw(False);

  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//_________________________________________

procedure Tpanning_form.view3_buttonClick(Sender: TObject);

begin
  zoom_offsetx := pad_view3.offset_x;
  zoom_offsety := pad_view3.offset_y;
  screenx := pad_view3.width_x;

  do_rollback := False;       // no need to put this in rollback register on redraw.
  redraw(False);

  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//_________________________________________

procedure Tpanning_form.view2_buttonClick(Sender: TObject);

begin
  zoom_offsetx := pad_view2.offset_x;
  zoom_offsety := pad_view2.offset_y;
  screenx := pad_view2.width_x;

  do_rollback := False;       // no need to put this in rollback register on redraw.
  redraw(False);

  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//__________________________________________

procedure Tpanning_form.view1_buttonClick(Sender: TObject);

begin
  zoom_offsetx := pad_view1.offset_x;
  zoom_offsety := pad_view1.offset_y;
  screenx := pad_view1.width_x;

  do_rollback := False;       // no need to put this in rollback register on redraw.
  redraw(False);

  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//______________________________________________________________________________

procedure Tpanning_form.previous_view_buttonClick(Sender: TObject);

// 0.91.c

begin
  if pad_view_list.Count < 1 then
    EXIT;  // no views in list.

  next_view_button.Enabled := True;  // enable reverse for 5 seconds.

  Inc(pad_view_index);
  if pad_view_index > (pad_view_list.Count - 1) then
    pad_view_index := 0;  // back to top.

  with Tpad_view(pad_view_list.Objects[pad_view_index]).pad_view_data do begin

    zoom_offsetx := offset_x;
    zoom_offsety := offset_y;
    screenx := width_x;
  end;//with

  do_rollback := False;       // no need to put this in rollback register on redraw.
  redraw(True);

  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//______________________________________________________________________________

procedure Tpanning_form.next_view_buttonClick(Sender: TObject);

begin
  if pad_view_list.Count < 1 then
    EXIT;  // no views in list.

  Dec(pad_view_index);
  if pad_view_index < 0 then
    pad_view_index := pad_view_list.Count - 1;  // back to bottom.

  with Tpad_view(pad_view_list.Objects[pad_view_index]).pad_view_data do begin

    zoom_offsetx := offset_x;
    zoom_offsety := offset_y;
    screenx := width_x;
  end;//with

  do_rollback := False;       // no need to put this in rollback register on redraw.
  redraw(True);

  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//______________________________________________________________________________

procedure Tpanning_form.mouse_zoom_buttonClick(Sender: TObject);

begin
  pad_form.adjust_spot_zoom_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tpanning_form.fit_extents_toolbuttonClick(Sender: TObject);

begin
  pad_form.fit_bgnd_menu_entry.Click;
end;
//______________________________________________________________________________


end.
