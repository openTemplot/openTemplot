
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

unit info_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls;

type

  { Tinfo_form }

  Tinfo_form = class(TForm)
    min_rad_box: TGroupBox;
    min_rad_now_label: TLabel;
    min_rad_lamp_panel: TPanel;
    pad_data_groupbox: TGroupBox;
    blue_corner_panel: TPanel;
    what_next_panel: TPanel;
    chat_panel: TPanel;
    colour_panel: TPanel;
    colour_patch: TImage;
    size_updown: TUpDown;
    corner_dot_panel: TPanel;
    shrink_button: TButton;
    expand_button: TButton;
    limit_rad_box: TGroupBox;
    limit_rad_label: TLabel;
    change_limit_button: TButton;
    info_gauge_panel: TPanel;
    gauge_label: TLabel;
    print_button: TButton;
    write_button: TButton;
    save_dialog: TSaveDialog;
    mouse_now_panel: TPanel;
    rename_button: TButton;
    view_button: TButton;
    slew_warn_panel: TPanel;
    slew_caution_mode_label: TLabel;
    cancel_slewing_button: TButton;
    pad_data_scrollbox: TScrollBox;
    zoom_lock_label: TLabel;
    unlock_button: TButton;
    lock_zoom_button: TButton;
    pad_width_label: TLabel;
    pad_height_label: TLabel;
    pad_zoom_label: TLabel;
    pad_notch_label: TLabel;
    notchx_label: TLabel;
    notchy_label: TLabel;
    notchk_label: TLabel;
    grid_org_label: TLabel;
    offsetx_label: TLabel;
    offsety_label: TLabel;
    edit_button: TButton;
    info_mm_label: TLabel;
    info_font_button: TButton;
    metric_button: TButton;
    info_scrollbox: TScrollBox;
    info_memo: TMemo;
    resize_button: TButton;
    jot_visible_button: TButton;
    ref_label: TLabel;
    ref_name_label: TLabel;
    memo_button: TButton;
    rings_groupbox: TGroupBox;
    ring_infringed_warning_label: TLabel;
    ring_infringed_by_label: TLabel;
    ring_lamp_panel: TPanel;
    sp_ring_warn_checkbox: TCheckBox;
    ring_copies_warn_checkbox: TCheckBox;
    rings_boxline_shape: TShape;
    help_button: TButton;
    jot_all_button: TButton;
    radius_shape: TShape;
    hide_panel: TPanel;
    curving_label: TLabel;
    gauge_bang_label: TLabel;
    companion_button: TButton;
    procedure FormCreate(Sender: TObject);
    procedure limit_rad_labelClick(Sender: TObject);
    procedure info_font_buttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure colour_panelClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormShow(Sender: TObject);
    procedure shrink_buttonClick(Sender: TObject);
    procedure expand_buttonClick(Sender: TObject);
    procedure lock_zoom_buttonClick(Sender: TObject);
    procedure print_buttonClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure unlock_buttonClick(Sender: TObject);
    procedure metric_buttonClick(Sender: TObject);
    procedure write_buttonClick(Sender: TObject);
    procedure what_next_panelClick(Sender: TObject);
    procedure chat_panelClick(Sender: TObject);
    procedure rename_buttonClick(Sender: TObject);
    procedure view_buttonClick(Sender: TObject);
    procedure cancel_slewing_buttonClick(Sender: TObject);
    procedure edit_buttonClick(Sender: TObject);
    procedure resize_buttonClick(Sender: TObject);
    procedure jot_visible_buttonClick(Sender: TObject);
    procedure memo_buttonClick(Sender: TObject);
    procedure sp_ring_warn_checkboxClick(Sender: TObject);
    procedure ring_copies_warn_checkboxClick(Sender: TObject);
    procedure jot_all_buttonClick(Sender: TObject);
    procedure info_gauge_panelClick(Sender: TObject);
    procedure hide_panelClick(Sender: TObject);
    procedure companion_buttonClick(Sender: TObject);
    procedure gauge_bang_labelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  info_form: Tinfo_form;

  //_______________________________

  back_colour: integer;
  copied_ref_str: string = '';
  current_memo_str: string = ' your memo notes for this template ...|';  // 5-08-01.

procedure statusbar_click(new: boolean);          // copied name becomes new reference.

//__________________________________________________________________________________________
implementation

{$BOOLEVAL ON}


uses config_unit, pad_unit, alert_unit, control_room, gauge_unit, print_unit, math_unit, colour_unit,
  Menus, Printers, keep_select, help_sheet, chat_unit, grid_unit,
  shove_timber, edit_memo_unit, jotter_unit, data_memo_unit;

{$R *.lfm}

var
  dummy_bool: boolean = False;

procedure get_new_ref_name; forward;

//___________________________________________________________________________________

procedure Tinfo_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(info_form);

  AutoScroll := True;
end;

//______________________________________________________________________________________

procedure Tinfo_form.limit_rad_labelClick(Sender: TObject);

begin
  pad_form.min_rad_menu_entry.Click;
end;
//_______________________________________________________________________________________

procedure Tinfo_form.info_font_buttonClick(Sender: TObject);

begin
  pad_form.info_font_menu_entry.Click;
end;
//______________________________________________________________________________________

procedure Tinfo_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  pad_form.hide_info_menu_entry.Click;    //  to update the menu.
end;
//_______________________________________________________________________________________

procedure Tinfo_form.colour_panelClick(Sender: TObject);

begin
  pad_form.back_colour_menu_entry.Click;
end;
//________________________________________________________________________________________

procedure Tinfo_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag
  // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                                           // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                                           // scale the form contents up.

  size_updown.Tag := size_updown.Position;                           // and save for the next click.
  expand_button.Click;                                             // full-size form.

end;
//____________________________________________________________________________________________

procedure Tinfo_form.FormShow(Sender: TObject);

begin
  ScrollinView(corner_dot_panel);      // to the top left.
end;
//__________________________________________________________________________________________

procedure Tinfo_form.shrink_buttonClick(Sender: TObject);

// shrink the form back to the corner.
var
  new_width, new_width1, new_width2: integer;

begin
  new_width1 := mouse_now_panel.Left - 1;
  new_width2 := min_rad_box.Left + min_rad_box.Width + 1;

  if new_width1 > new_width2 then
    new_width := new_width1
  else
    new_width := new_width2;

  ScrollinView(corner_dot_panel);                     // to the top left.

  ClientWidth := new_width;
  ClientHeight := rename_button.Top;       // 0.93.a was -1
  ClientWidth := new_width;                // do width twice for proper calc of scrollbars.
end;
//_________________________________________________________________________________________

procedure Tinfo_form.expand_buttonClick(Sender: TObject);

begin
  ClientHeight := VertScrollBar.Range + 2;     // allow 2 pixel right/bottom margins.
  ClientWidth := HorzScrollBar.Range + 2;
  ClientHeight := VertScrollBar.Range + 2;     // do this twice, as each affects the other.
end;
//__________________________________________________________________________________________

procedure Tinfo_form.lock_zoom_buttonClick(Sender: TObject);

begin
  pad_form.lock_scaling_at_menu_entry.Click;
end;
//_________________________________________________________________________________________

procedure Tinfo_form.print_buttonClick(Sender: TObject);

begin
  if no_printer_available = True     // 0.93.a
  then begin
    ShowMessage('No printer available.');
    EXIT;
  end;

  { OT-FIRST
  with TPrintDialog.Create(nil) do begin   // 0.93.a created in code because of startup error if no printer available.
    try
      if Execute=False then EXIT;
    finally
      Free;
    end;//try
  end;//with

  print_info('','');
  }

end;
//_________________________________________________________________________________________

procedure Tinfo_form.FormPaint(Sender: TObject);

begin
  ref_name_label.Caption := current_name_str;
end;
//__________________________________________________________________________________________

procedure Tinfo_form.unlock_buttonClick(Sender: TObject);

begin
  pad_form.free_scaling_menu_entry.Click;
end;
//___________________________________________________________________________________________

procedure Tinfo_form.metric_buttonClick(Sender: TObject);

begin
  pad_form.metric_calc_menu_entry.Click;
end;
//________________________________________________________________________________________

procedure Tinfo_form.write_buttonClick(Sender: TObject);

var
  info_file: TextFile;
  i, n: integer;
  s: string;

begin
  save_dialog.Filename := Config.MakeFilePath(cudiData, Copy(Trim(remove_invalid_str(current_name_str)),
    1, 20){+remove_invalid_str(suffix_str)} + '.txt');
  save_dialog.Filter := 'text files (*.txt)|*.txt';
  if save_dialog.Execute = True then begin
    save_dialog.FileName := ChangeFileExt(save_dialog.FileName, '.txt');   // force extension

    AssignFile(info_file, save_dialog.FileName);      // set the file name.
    Rewrite(info_file);                              // open a new file.

    if box_project_title_str <> '' then
      WriteLn(info_file, ' ' + gauge_label.Caption + '    for  ' + box_project_title_str +
        '    ref : ' + current_name_str)
    else
      WriteLn(info_file, ' ' + gauge_label.Caption + '    ref : ' + current_name_str);

    WriteLn(info_file, '');
    WriteLn(info_file, '_____________________________________________');
    WriteLn(info_file, 'INFO :');
    WriteLn(info_file, '');

    if info_memo.Lines.Count < 1                        // info empty (???).
    then
      WriteLn(info_file, '    blank')
    else
      for n := 0 to info_memo.Lines.Count - 1 do
        WriteLn(info_file, info_memo.Lines[n]);

    WriteLn(info_file, '');
    WriteLn(info_file, '_____________________________________________');


    WriteLn(info_file, 'MEMO :');
    WriteLn(info_file, '');

    s := current_memo_str;        // mod 0.72 6-8-01...

    repeat                      //  find memo lines separated by | chars.
      i := Pos('|', s);
      if i > 0 then begin
        WriteLn(info_file, Copy(s, 1, i - 1));
        // extract a string line from the memo.
        Delete(s, 1, i);
        // and delete it from the input string.
      end;
    until (i < 1) or (Length(s) = 0);

    CloseFile(info_file);         // and close the file.
  end;
end;
//______________________________________________________________________________________

procedure Tinfo_form.what_next_panelClick(Sender: TObject);

const
  what_next_str: string = '    `0Information  Panel`9' +
    '||This panel displays information about the control template showing on the trackpad. The data in this panel is continuously updated as the control template is adjusted by mouse action.' + '||The various items on this panel are:' + '||The `0HIDE`1 button removes the panel from view to give a full view of the trackpad area. To see' + ' the panel again, select the `0trackpad > SHOW INFORMATION PANEL`1 menu item. The panel can be more' + ' conveniently toggled into and out of view by pressing the `0F2`2 key repeatedly.' + '||The `0EXPAND`1 button expands the panel to its full size, if it is not already that size.' + '||The `0SHRINK`1 button reduces the panel to the bare minimum which contains useful information' + ' while remaining in view.' + '||The top-left bar (showing normally in pale blue) displays the current gauge/scale and size details for the control template. Clicking this bar will hide the control template, in a similar way to pressing the `0HOME`2 key.' + ' While the control template is hidden, this bar shows in pale grey. Clicking it again will show the control template again on the trackpad.' + '||The `0smallest radius now:`3 box shows the smallest track radius at any point on the control template. If this is less than the limit value' + ' set in the `0warn if radius under:`3 box, the warning lamp flashes red. Otherwise it shows green. To change the limit value, click the `0CHANGE..`1 button.' + ' This warning setting is specific to each template, which allows you to set different radius limits for different parts of your track plan.' + '||The radius warning function is not available when the template contains a turnout and a `0slew`3 (slewing is intended mainly for plain track.' + ' For more information about slewing, select the `0GEOMETRY > SLEW (NUDGE) > SLEWING DATA...`1 menu item.)' + '||The lower half of the information panel comprises the `0info`3 area which contains dimensional details about the control template.' + '||By adjusting the scroll bars on this area, and also the scroll bars for the whole information panel while re-sizing it,' + ' it is possible to keep the dimensions of interest in view while the main panel is at a much reduced size to give a clearer view of the trackpad.' + ' The `0FIT TO WINDOW`1 button resizes the info area to fit the available space in the main panel. Use this button after re-sizing the main panel in the usual way by dragging the edges or the bottom-right corner.' + '||The `0JOT ALL`1 button copies all the template info to the jotter as a handy future reference.' + ' The `0JOT VISIBLE`1 button copies only the currently visible lines of info to the jotter.' + '||The `0VIEW`1 button displays the current info data in a more easily readable form, which then remains unchanged and visible.' + '||The `0EDIT`1 button is similar but duplicates the info data in an editable form in a separate window which updates continuously.' + '||The `0WRITE...`1 button will save the info data to a text file, and the `0PRINT`1 button will print it on your printer.' + ' The current printer text font is used for this, to change it select the `0PROGRAM > PRINTER FONT + MARGINS...`1 menu item on the `0program panel`3 window.' + ' This info data is also printed out along with each printed control template if required.' + '||The `0name`3 of the control template is shown above the `0info`3 area. To change the name, click the `0RENAME..`1 button, or on the name itself.' + ' Or click on the status bar along the bottom of the panel to use the name showing there, which is the name of the last template copied back from your storage box.' + '||The `0MEMO`1 button opens a window in which you can edit or paste any notes you wish to make about the control template, which will be included with it when you store it or copy it to the background.' + ' (The memo notes can be further edited on the storage box later.)' + '||If the `0RINGS > RING`1 tickbox is ticked the ring warning lamp will flash if any part of the control template infringes (crosses into) the current size and position of the spacing-ring tool.' + ' This is useful when adjusting the template to be clear of structures, obstructions, baseboard joints and edges, etc. If the `0COPIES`1 tickbox is ticked any ring copies which have been created will also be checked for infringements.' + '||The amount by which the control template is clear of (shown in bue) or infringing (shown in red) the spacing-ring is shown below the warning lamp.' + ' Normally the outer-edges of the rails are checked against the inner diameter of the ring, and in this case the warning lamp flashes red when the ring is infringed and shows a steady green when the control template''s rails are clear of the ring.' + '||In the case of a `0centre-line only`3 template, the track centre-line is checked against the inner diameter of the ring,' + ' and in this case the warning lamp flashes white when the ring is infringed and shows a steady pale blue when the track centre-line is clear.' + ' If desired this mode can be selected for all templates by clicking the `0trackpad > INFORMATION PANEL OPTIONS > RING INFRINGEMENTS > WARN IF CENTRE-LINES INFRINGE`1 menu item.' + '||N.B. The figures shown apply only to the spacing-ring itself, not to any ring copies. It is possible for the warning lamp to be flashing (because of an infringed ring copy) while the figures show clear (because the spacing-ring itself is clear).' + ' For more information about using the spacing-ring tool, click the `0UTILS > DUMMY VEHICLE • SPACING-RING`1 menu item, and then click the `0? HELP`1 button.' + '||The `0trackpad data:`3 box shows the current zoom scale of the trackpad, i.e. the model size represented by the full visible width of the trackpad. If this is free to change' + ' as changes are made to the length of the control template the label will show ZOOM FREE in green. If the label shows LOCKED AT in white, then the current trackpad zoom setting' + ' will remain fixed. To change to free zooming, click the `0UNLOCK`1 button.' + ' To lock the trackpad zoom setting at any size, click the `0LOCK AT..`1 button. (These zoom options are also available on the `0trackpad`1 menu and on the right-click menu.)' + '||N.B. The free zooming option is useful when adjusting stock templates which are on their drawing datum (green dot). When working on a complete track plan you will normally' + ' want to have the zoom setting locked, and use the + and - buttons and other zoom controls and mouse actions to change it.' + '||The `0trackpad data:`3 box can be scrolled to see more zoom data and the current position of the pegging notch.';

begin
  help(-1, what_next_str, '');
end;
//_________________________________________________________________________________________

procedure Tinfo_form.chat_panelClick(Sender: TObject);

const
  info_chat_str: string = '     The  Information  Panel' +
    '||I have tried to pack as much info into this area as possible. It was originally intended to be in a status bar'
    + ' along the bottom of the trackpad, but there was just too much information to fit. I realise that when expanded'
    + ' the panel obscures most of the trackpad, but I have made it as convenient as possible to toggle it into and out'
    + ' of view, or to re-size it to show just the item of interest. As ever, your feedback is welcome - what have I left out?';

begin
  chat(info_chat_str);
end;
//___________________________________________________________________________________________

procedure statusbar_click(new: boolean);          // copied name becomes new reference.

begin
  current_name_str := copied_ref_str;
  if new = True then
    get_new_ref_name;

  info_form.ref_name_label.Caption := current_name_str;
end;
//___________________________________________________________________________________________

procedure Tinfo_form.view_buttonClick(Sender: TObject);

var
  memo_str: string;

begin

  memo_str := '    ' + ref_name_label.Caption + '||' + gauge_label.Caption +
    '||--------------------------------------------------------------' +
    '||    Information  about  this  template :' + '||( all dimensions in millimetres )'
    + '||' + info_memo.Text + '||--------------------------------------------------------------'
    + '|| Memo:' + '||' + current_memo_str;

  data_child_form.Close; // if showing elsewhere

  data_child_form.reset_position();   // child on pad

  data_child_form.data_memo.Text := insert_crlf_str(memo_str);
  //  replace embedded | chars with a CR.

  data_child_form.Show;
end;
//__________________________________________________________________________________________

procedure Tinfo_form.rename_buttonClick(Sender: TObject);

begin
  get_new_ref_name;
end;
//_______________________________________________________________________________________

procedure get_new_ref_name;    // get a new name for the control template.

const
  box_str: string =
    '            Control  Template  Name' +
    '||Enter below an optional reference name for the control template.' +
    '||This can be any combination of numbers and letters, or if preferred a meaningful name, for example :'
    + '||K-49|LOOP # 2|PENWYLLT SILICA BRICK WORKS SIDING No. 3|etc.' +
    '||But do not use any square brackets.' +
    '||If you store this template it will also be given an ID reference number as part of the name.';
  //+'||If you leave the box blank, the name "project-title - xxx" will be used, where "project-title" is your current project title, and "xxx" is a 3-figure reference generated at random.';

begin
  with math_form do begin
    Caption := '    control  template  name ...';
    big_label.Caption := insert_crlf_str(box_str);
    math_editbox.Text := current_name_str;

    do_show_modal(math_form);     // 212a  ShowModal

    if ModalResult = mrOk then begin

      current_name_str := Trim(math_editbox.Text);

      info_form.ref_name_label.Caption := current_name_str;
    end;
    Caption := '    ' + Application.Title;   // reset form caption.
  end;//with
end;
//__________________________________________________________________________________________

procedure Tinfo_form.cancel_slewing_buttonClick(Sender: TObject);

begin
  pad_form.disable_slewing_menu_entry.Click;
end;
//__________________________________________________________________________________________

procedure Tinfo_form.edit_buttonClick(Sender: TObject);

begin
  with edit_memo_form do begin

    Caption := '    information  -  the  control  template :';

    vis_edit_memo.Text := info_form.info_memo.Text;            // show the info instead.

  end;//with

  do_show_modal(edit_memo_form);

end;
//________________________________________________________________________________________

procedure Tinfo_form.resize_buttonClick(Sender: TObject);

begin
  info_scrollbox.Width := ClientWidth - info_scrollbox.Left * 2;
  info_scrollbox.Height := ClientHeight - info_scrollbox.Top - info_scrollbox.Left;
  info_scrollbox.Width := ClientWidth - info_scrollbox.Left * 2;  // do this twice.
end;
//_________________________________________________________________________________________

procedure Tinfo_form.jot_visible_buttonClick(Sender: TObject);

var
  bmp: TBitmap;
  line_height, n, n1, n2, n3: integer;

begin
  bmp := TBitmap.Create;
  try
    bmp.Canvas.Font := info_memo.Font;
    line_height := bmp.Canvas.TextHeight('A');
  finally
    bmp.Free;
  end;//try

  if line_height < 1 then
    EXIT;

  n1 := Round(info_scrollbox.VertScrollBar.Position / line_height);    // starting line index.

  if (n1 > -1) and (n1 < info_memo.Lines.Count) then begin
    n2 := Round((info_scrollbox.VertScrollBar.Position + info_scrollbox.ClientHeight) /
      line_height) - 1;       // ending line if whole scrollbox visible.
    n3 := Round((info_scrollbox.VertScrollBar.Position + ClientHeight - info_scrollbox.Top) /
      line_height) - 1;   // ending line if part scrollbox obscured.

    if n2 > n3 then
      n2 := n3; // use shortest number of lines.

    if n2 < 0 then
      EXIT;
    if n2 > (info_memo.Lines.Count - 1) then
      n2 := info_memo.Lines.Count - 1;
    if n2 < n1 then
      EXIT;


    jotter_form.jotter_memo.Lines.Add('__');

    for n := n1 to n2 do
      jotter_form.jotter_memo.Lines.Add(info_memo.Lines[n]);
  end;
end;
//__________________________________________________________________________________________

procedure Tinfo_form.memo_buttonClick(Sender: TObject);

begin
  current_memo_str := edit_memo_str(current_memo_str, current_name_str);
end;
//___________________________________________________________________________________________

procedure Tinfo_form.sp_ring_warn_checkboxClick(Sender: TObject);

begin
  ring_warn := sp_ring_warn_checkbox.Checked;
  if ring_warn = False then begin
    ring_infringed_by_label.Hide;
    ring_infringed_warning_label.Hide;
    if ring_copies_warn = False then begin
      ring_lamp_panel.Color := info_form.Color;
      ring_lamp_panel.Tag := 2;                   // flashing disabled.
    end;
  end
  else begin
    ring_infringed_by_label.Show;
    ring_infringed_warning_label.Show;
  end;
  show_and_redraw(True, False);
end;
//_________________________________________________________________________________________

procedure Tinfo_form.ring_copies_warn_checkboxClick(Sender: TObject);

var
  i: integer;

begin
  if (ring_copies_warn_checkbox.Checked = True) and (ring_index < 1)   // no copies.
  then begin
    repeat
      i := alert(6, '    no  ring  copies',
        'You requested a warning if any copies of the spacing-ring tool are infringed by the control template.'
        + '||But currently no ring copies have been created.',
        '', '', '', '? spacing - ring  help', 'cancel  warning', '', 4);
      if i = 4 then
        help(0, ring_help_str, '');
    until i <> 4;
    ring_copies_warn_checkbox.Checked := False;
    ring_copies_warn := False;
    EXIT;
  end;

  ring_copies_warn := ring_copies_warn_checkbox.Checked;
  if (ring_warn = False) and (ring_copies_warn = False) then begin
    ring_lamp_panel.Color := info_form.Color;
    ring_lamp_panel.Tag := 2;                   // flashing disabled.
  end;
  show_and_redraw(True, False);
end;
//____________________________________________________________________________________________

procedure Tinfo_form.jot_all_buttonClick(Sender: TObject);

begin
  jotter_form.jotter_add_current_info_popup_entry.Click;
end;
//___________________________________________________________________________________________

procedure Tinfo_form.info_gauge_panelClick(Sender: TObject);

begin
  if pad_form.show_control_template_menu_entry.Checked = True             // 209c mods
  then
    pad_form.hide_control_template_menu_entry.Click
  else
    pad_form.show_control_template_menu_entry.Click;
end;
//___________________________________________________________________________________________

procedure Tinfo_form.hide_panelClick(Sender: TObject);

begin
  pad_form.hide_info_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tinfo_form.companion_buttonClick(Sender: TObject);

begin
  go_to_templot_companion;       // 218d
end;
//______________________________________________________________________________

procedure Tinfo_form.gauge_bang_labelClick(Sender: TObject);

begin
  pad_form.other_gauges_menu_entry.Click;   // 218d
end;
//______________________________________________________________________________

end.
