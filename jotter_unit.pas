
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
unit jotter_unit;

{$MODE Delphi}

interface

uses
  LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, PrintersDlgs;

type

  { Tjotter_form }

  Tjotter_form = class(TForm)
    jotter_memo: TMemo;
    jotter_popup_menu: TPopupMenu;
    jotter_fonts_popup_entry: TMenuItem;
    jotter_colours_popup_entry: TMenuItem;
    jotter_clear_popup_entry: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    jotter_add_current_info_popup_entry: TMenuItem;
    jotter_cut_popup_entry: TMenuItem;
    jotter_copy_popup_entry: TMenuItem;
    jotter_paste_popup_entry: TMenuItem;
    jotter_select_all_popup_entry: TMenuItem;
    jotter_copy_all_popup_entry: TMenuItem;
    N3: TMenuItem;
    jotter_abs_xy_readout_panel: TPanel;
    jotter_rel_xy_readout_panel: TPanel;
    jotter_print_popup_entry: TMenuItem;
    jotter_readouts_only_popup_entry: TMenuItem;
    N4: TMenuItem;
    jotter_help_popup_entry: TMenuItem;
    N5: TMenuItem;
    jotter_restore_previous_popup_entry: TMenuItem;
    jotter_save_popup_entry: TMenuItem;
    jotter_obtain_popup_entry: TMenuItem;
    jotter1: TMenuItem;
    N6: TMenuItem;
    jotter_set_readout_origin_popup_entry: TMenuItem;
    jotter_open_dialog: TOpenDialog;
    jotter_save_dialog: TSaveDialog;
    jotter_wrap_lines_popup_entry: TMenuItem;
    expand_jotter_popup_entry: TMenuItem;
    jotter_print_dialog: TPrintDialog;
    readout_units_popup_entry: TMenuItem;
    mm_units_popup_entry: TMenuItem;
    inches_units_popup_entry: TMenuItem;
    as_grid_units_popup_entry: TMenuItem;
    print_memo: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure jotter_fonts_popup_entryClick(Sender: TObject);
    procedure jotter_colours_popup_entryClick(Sender: TObject);
    procedure jotter_memoDblClick(Sender: TObject);
    procedure jotter_clear_popup_entryClick(Sender: TObject);
    procedure jotter_add_current_info_popup_entryClick(Sender: TObject);
    procedure jotter_cut_popup_entryClick(Sender: TObject);
    procedure jotter_copy_popup_entryClick(Sender: TObject);
    procedure jotter_copy_all_popup_entryClick(Sender: TObject);
    procedure jotter_paste_popup_entryClick(Sender: TObject);
    procedure jotter_select_all_popup_entryClick(Sender: TObject);
    procedure jotter_restore_previous_popup_entryClick(Sender: TObject);
    procedure jotter_obtain_popup_entryClick(Sender: TObject);
    procedure jotter_save_popup_entryClick(Sender: TObject);
    procedure jotter_wrap_lines_popup_entryClick(Sender: TObject);
    procedure jotter_print_popup_entryClick(Sender: TObject);
    procedure jotter_readouts_only_popup_entryClick(Sender: TObject);
    procedure expand_jotter_popup_entryClick(Sender: TObject);
    procedure jotter_set_readout_origin_popup_entryClick(Sender: TObject);
    procedure jotter_help_popup_entryClick(Sender: TObject);
    procedure as_grid_units_popup_entryClick(Sender: TObject);
    procedure mm_units_popup_entryClick(Sender: TObject);
    procedure inches_units_popup_entryClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  jotter_form: Tjotter_form;

  //____________________________

  jotter_dx_org: double = 0;        // for relative x,y mouse read-out on jotter.
  jotter_dy_org: double = 0;

  jot_readout_units: integer = 0;

implementation

{$R *.lfm}

uses
  config_unit,
  colour_unit, info_unit, pad_unit, control_room, alert_unit, preview_unit,
  print_unit, calibration_unit, Printers,
  help_sheet, math_unit, entry_sheet, print_settings_unit;

//_____________________________________________________________________________________

procedure Tjotter_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;         //  hide TEMPLOT on PAUSE key.

  if Key = VK_ESCAPE then begin
    Key := 0;
    pad_form.Show;
    pad_form.BringToFront;
  end;

  if Key = VK_F2 then begin
    Key := 0;
    pad_form.Show;
    pad_form.BringToFront;
    if info_show_i = 1 then
      pad_form.hide_info_menu_entry.Click     // radio items.
    else
      pad_form.show_info_menu_entry.Click;
  end;

  if Key = VK_F1 then begin
    Key := 0;
    jotter_help_popup_entry.Click;
  end;

  if ((Key = VK_J) and (Shift = [ssCtrl]))   // CTRL-J.
  then begin
    Key := 0;
    Close;
  end;

end;
//_________________________________________________________________________________________

procedure Tjotter_form.FormCreate(Sender: TObject);

begin
  AutoScroll := False;
  // OT-FIRST ClientWidth:=400;
  // OT-FIRST ClientHeight:=250;
end;
//______________________________________________________________________________

procedure Tjotter_form.jotter_fonts_popup_entryClick(Sender: TObject);

begin
  jotter_memo.Font.Assign(get_font('choose  a  new  font  for  the  jotter  text',
    jotter_memo.Font, True));
  jotter_abs_xy_readout_panel.Font.Assign(
    get_font('choose  a  new  font  for  the  X-Y  grid  read-out',
    jotter_abs_xy_readout_panel.Font, True));
  jotter_abs_xy_readout_panel.Height := ABS(jotter_abs_xy_readout_panel.Font.Height) + 6;

  // rel read-out uses the form font (so we can use the form canvas to calc the width...

  Font.Assign(get_font('choose  a  new  font  for  the  X-Y-D  moved-by  read-out', Font, True));
  jotter_rel_xy_readout_panel.Height := ABS(Font.Height) + 6;

  if jotter_memo.Showing = True then
    jotter_memo.SetFocus;
end;
//______________________________________________________________________________

procedure Tjotter_form.jotter_colours_popup_entryClick(Sender: TObject);

begin
  jotter_memo.Color := get_colour('choose  a  new  colour  for  the  jotter', jotter_memo.Color);
  jotter_abs_xy_readout_panel.Color :=
    get_colour('choose  a  new  colour  for  the  X-Y  grid  read-out',
    jotter_abs_xy_readout_panel.Color);
  jotter_rel_xy_readout_panel.Color :=
    get_colour('choose  a  new  colour  for  the  X-Y-D  moved-by  read-out',
    jotter_rel_xy_readout_panel.Color);
  if jotter_memo.Showing = True then
    jotter_memo.SetFocus;
end;
//______________________________________________________________________________

procedure Tjotter_form.jotter_wrap_lines_popup_entryClick(Sender: TObject);

begin
  jotter_wrap_lines_popup_entry.Checked := not jotter_wrap_lines_popup_entry.Checked;
  jotter_memo.WordWrap := jotter_wrap_lines_popup_entry.Checked;
end;
//__________________________________________________________________________________________

procedure Tjotter_form.jotter_memoDblClick(Sender: TObject);

begin
  jotter_popup_menu.Popup(jotter_form.Left + 20, jotter_form.Top + 50);
end;
//________________________________________________________________________________________

procedure Tjotter_form.jotter_clear_popup_entryClick(Sender: TObject);

begin
  jotter_memo.Clear;
end;
//___________________________________________________________________________________________

procedure Tjotter_form.jotter_add_current_info_popup_entryClick(Sender: TObject);

begin
  jotter_memo.Lines.Add('__');
  jotter_memo.Lines.Add('');
  jotter_memo.Text := jotter_memo.Text + info_form.info_memo.Text;
end;
//_________________________________________________________________________________________

procedure Tjotter_form.jotter_cut_popup_entryClick(Sender: TObject);

begin
  jotter_memo.CutToClipboard;
end;
//__________________________________________________________________________________________

procedure Tjotter_form.jotter_copy_popup_entryClick(Sender: TObject);

begin
  jotter_memo.CopyToClipboard;
end;
//_________________________________________________________________________________________

procedure Tjotter_form.jotter_copy_all_popup_entryClick(Sender: TObject);

begin
  with jotter_memo do begin
    SelectAll;
    CopyToClipboard;
    SelStart := SelLength;      // de-select..
    SelLength := 0;
  end;//with
end;
//__________________________________________________________________________________________

procedure Tjotter_form.jotter_paste_popup_entryClick(Sender: TObject);

begin
  jotter_memo.PasteFromClipboard;
end;
//__________________________________________________________________________________________

procedure Tjotter_form.jotter_select_all_popup_entryClick(Sender: TObject);

begin
  jotter_memo.SelectAll;
end;
//_____________________________________________________________________________________________

procedure Tjotter_form.jotter_readouts_only_popup_entryClick(Sender: TObject);

var
  readout_width: integer;

begin
  ClientHeight := jotter_abs_xy_readout_panel.Height + jotter_rel_xy_readout_panel.Height;

  // narrow the form if he's got it unnecessarily wide...

  readout_width := Canvas.TextWidth(' dX = ' + captext(0 - 100) + '   dY = ' + captext(
    0 - 100) + '   Diag = ' + captext(100) + ' ') + 4;
  if ClientWidth > readout_width then
    ClientWidth := readout_width;

  // put it in top-right corner..

  Top := 0;
  Left := Screen.DesktopLeft + Screen.DesktopWidth - Width;

  ShowMessage('The jotter read-outs are now in the top-right corner of the screen.');
end;
//__________________________________________________________________________________________

procedure Tjotter_form.expand_jotter_popup_entryClick(Sender: TObject);

begin
  Height := Screen.DesktopHeight div 2;
  Width := Screen.DesktopWidth div 2;
  Left := (Screen.DesktopLeft + Screen.DesktopWidth - Width) div 2;
  Top := Screen.DesktopHeight div 4;
end;
//________________________________________________________________________________________

procedure Tjotter_form.jotter_restore_previous_popup_entryClick(Sender: TObject);

begin
  if FileExists(Config.GetFilePath(csfiJotterBkp)) then
    jotter_memo.Lines.LoadFromFile(Config.GetFilePath(csfiJotterBkp))  // bug fix 0.82.a  22-08-06
  else
    jotter_memo.Lines.Add('!!! No previous text available');
end;
//_____________________________________________________________________________________________

procedure Tjotter_form.jotter_obtain_popup_entryClick(Sender: TObject);

begin
  if jotter_memo.Lines.Count > 0 then begin
    if alert(3, '    obtain  jotter  text  from  file ...',
      '|||Obtaining the contents of your jotter from a file will overwrite (delete) all of the existing contents.' + '||To preserve the existing contents, cut or copy them to the Windows clipboard first.' + '||Paste them back into the jotter after obtaining the file.', '', '', '', '', 'cancel  obtain', 'obtain  jotter  text  from  file', 0) = 5 then
      EXIT;
  end;

  jotter_open_dialog.Filename := Config.GetFilePath(csfiJotter);
  jotter_open_dialog.Filter := 'text files (*.txt)|*.txt';
  if jotter_open_dialog.Execute = True then begin
    jotter_memo.Lines.LoadFromFile(jotter_open_dialog.Filename);
    if jotter_memo.Showing = True then
      jotter_memo.SetFocus;
  end;
end;
//____________________________________________________________________________________________

procedure Tjotter_form.jotter_save_popup_entryClick(Sender: TObject);

begin
  if jotter_memo.Lines.Count < 1 then
    EXIT;

  jotter_save_dialog.Filename := Config.GetFilePath(csfiJotter);
  jotter_save_dialog.Filter := 'text files (*.txt)|*.txt';
  if jotter_save_dialog.Execute = True then begin
    jotter_save_dialog.FileName := ChangeFileExt(jotter_save_dialog.FileName, '.txt');
    // force extension

    jotter_memo.Lines.SaveToFile(jotter_save_dialog.Filename);
    if jotter_memo.Showing = True then
      jotter_memo.SetFocus;
  end;
end;
//______________________________________________________________________________________________

procedure Tjotter_form.jotter_print_popup_entryClick(Sender: TObject);

var
  prindex: integer;
  pf: TextFile;                // text file to be redirected to printer.
  line_now: integer;
  prinor: TPrinterOrientation;
  text_width_dots: double;
  left_margin_str: string;

begin
  if no_printer_available = True     // 0.93.a
  then begin
    ShowMessage('No printer available.');
    EXIT;
  end;

  (* OT-FIRST

  prinor:=Printer.Orientation;              // save his setting
  Printer.Orientation:=poPortrait;


  with TPrintDialog.Create(nil) do begin   // 0.93.a created in code because of startup error if no printer available.
    try
      if Execute=False
         then begin
                Printer.Orientation:=prinor;    // restore his setting.
                EXIT;
              end;
    finally
      Free;
    end;//try
  end;//with


  page_info(True,True,False,0);         // get page width.

  if nom_width_dpi<1 then EXIT;        // ??? div zero.

  try
    AssignPrn(pf);                           // redirect file to printer.
    Rewrite(pf);                             // open the file for writing.

    with Printer.Canvas do begin

      Font.Assign(printer_text_font);

      if get_prindex(prindex)=False then EXIT;                                                      // get current printer index or none available.

      if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact>0 then Font.Style:=[];   // not bold on impact printer.

      text_width_dots:=printer_width_indexmax_dots-(printer_text_left_margin+printer_text_right_margin)*nom_width_dpi/25.4-TextWidth(' ');  // width between margins in printer dots.

      {help_form.}print_memo.ClientWidth:=Round(text_width_dots*PixelsPerInch/nom_width_dpi);     // width between margins in screen pixels.

      left_margin_str:='';

      while TextWidth(left_margin_str)<Round(printer_text_left_margin*nom_width_dpi/25.4) do left_margin_str:=left_margin_str+' ';   // left margin approximated to spaces.

    end;//with

    {help_form.}print_memo.Clear;
    {help_form.}print_memo.Font.Assign(printer_text_font);
    {help_form.}print_memo.Text:=AdjustLineBreaks(jotter_memo.Text);

    WriteLn(pf,'');
    WriteLn(pf,left_margin_str+'            JOTTER  TEXT      at  '+TimeToStr(Time)+'    on  '+DateToStr(Date)+'  :');
    WriteLn(pf,'');
    for line_now:=0 to {help_form.}print_memo.Lines.Count-1 do WriteLn(pf,left_margin_str+{help_form.}print_memo.Lines[line_now]);
  finally
    CloseFile(pf);
    Printer.Orientation:=prinor;    // restore his previous setting.
  end;//try
*)

end;
//________________________________________________________________________________________

procedure Tjotter_form.jotter_set_readout_origin_popup_entryClick(Sender: TObject);

const
  help_rox_str: string = '     Set  Read-Out  X  Origin' +
    '||Enter a new X-dimension in millimetres for the position on the grid of the datum point from which the MOVED-BY: dX read-out is calculated.';

  help_roy_str: string = '     Set  Read-Out  Y  Origin' +
    '||Enter a new Y-dimension in millimetres for the position on the grid of the datum point from which the MOVED-BY: dY read-out is calculated.';

var
  n: integer;
  od: Toutdim;

begin
  putdim(help_rox_str, 1, 'read-out  origin  X - dimension', jotter_dx_org, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.
  n := putdim(help_roy_str, 1, 'read-out  origin  Y - dimension', jotter_dy_org, False, True, False, False);
  // negative ok, no preset, zero ok, don't terminate on zero.
  if n <> 1 then
    EXIT;
  if getdims('set  X-Y-D  read-out  origin', '', jotter_form, n, od) = True then begin
    jotter_dx_org := od[0];
    jotter_dy_org := od[1];
  end;
end;
//________________________________________________________________________________________

procedure Tjotter_form.jotter_help_popup_entryClick(Sender: TObject);

const
  jot_help_str: string = 'Jotter  +  X-Y  Read-outs' +
    '||Use this jotter as a handy place to jot down and copy/paste notes, dimensions and reminders. The jotter window can be dragged anywhere on the screen and toggled on and off by pressing CTRL-J. It will always remain visible in front of the pad.' + '||To begin typing on the jotter, LEFT-click on it.|To access a menu of options, RIGHT-click on it.' + '|To return to the trackpad, press the ESC key or the F2 key.|To hide the jotter press CTRL-J.' + '||There are several ways in which notes can be added automatically to the jotter while you work:' + '||Holding down the ALT key and clicking the middle button (or holding down both ALT and CTRL and left-clicking) copies the current dimensions showing in the X-Y read-outs onto the jotter.' + '||If a mouse action is in force, holding down both ALT and CTRL keys and right-clicking copies the data currently showing in the mouse action panel onto the jotter.' + '||Clicking the JOT VISIBLE button on the INFORMATION panel adds a copy of the currently visible info lines onto the jotter.' + '||While using the Templot0 data-entry form, clicking the JOT LINE / JOT ALL button adds a copy of the current input line or all the input lines onto the jotter.' + '||The Templot0 HELP texts can be added to the jotter by right-button clicking on them and them clicking the JOT ALL menu item. They include long text lines, so you will probably want to have the WRAP LINES option selected for the jotter.' + '||These functions work regardless of whether the jotter window is currently visible.' + '||Clicking RESTORE PREVIOUS TEXT on the jotter right-click menu restores the jotter text from your previous Templot0 working session. In this way a complete journal of your design work can be maintained if you wish.' + '||The text currently on the jotter can be added to the memo notes for an individual stored template by clicking ADD JOTTER TO MEMO on the template''s pop-up menu or the EDIT > ADD JOTTER TEXT TO MEMO item on the STORAGE BOX menus.' + ' This is quicker than copying and pasting, and is a convenient way to include your jotter notes in a template data file for future reference.' + '|------------------------' + '|This jotter window also includes two X-Y read-outs of the current mouse position. These can be useful when using the DRAW WITH MOUSE function and also when making template adjustments by mouse action.' + ' The mouse pointer can be changed to cross-hairs for accurate positioning and measurement by clicking the TRACKPAD > MOUSE OPTIONS > CROSS-HAIRS POINTER menu item.' + '||The upper GRID read-out shows the current mouse position on the grid in absolute X,Y co-ordinate dimensions.' + '||The lower MOVED BY read-out shows the current mouse position in relative dX,dY dimensions from a set reference point, and also the diagonal dimension from that point.' + ' The reference point can be set by holding down the ALT key and LEFT-clicking on the trackpad, or by clicking the SET READ-OUT ORIGIN... item on the jotter right-click menu.' + ' Note that if the ruler tool is showing, ALT-LEFT click also positions its 1st end (and ALT-RIGHT click positions its 2nd end) at the mouse pointer. If you do not want this to happen,' + ' temporarily hide the ruler (UTILS > RULER > HIDE RULER menu item).' + '||The read-outs can display the dimensions in MM, INCHES or in the same units as are curently being used for the grid (AS GRID). Click READ-OUT UNITS > on the jotter right-click menu to select these options.' + '||You can choose to have either or both of the read-outs visible by selecting the options on the jotter right-click menu.' + ' If you select the READ-OUT(S) ONLY option, the window moves to the top right-hand corner of the screen, but can be subsequently moved elsewhere.';

begin
  help(0, jot_help_str, '');
end;
//____________________________________________________________________________________

procedure Tjotter_form.as_grid_units_popup_entryClick(Sender: TObject);

begin
  jot_readout_units := 0;
  as_grid_units_popup_entry.Checked := True;     // radio item.
  show_and_redraw(True, False);
end;
//__________________________

procedure Tjotter_form.mm_units_popup_entryClick(Sender: TObject);

begin
  jot_readout_units := 1;
  mm_units_popup_entry.Checked := True;     // radio item.
  show_and_redraw(True, False);
end;
//___________________________

procedure Tjotter_form.inches_units_popup_entryClick(Sender: TObject);

begin
  jot_readout_units := 2;
  inches_units_popup_entry.Checked := True;     // radio item.
  show_and_redraw(True, False);
end;
//______________________________________________________________________________________

procedure Tjotter_form.FormResize(Sender: TObject);

begin
  jotter_memo.Width := ClientWidth;
  jotter_memo.Height := ClientHeight - jotter_memo.Top;

  jotter_abs_xy_readout_panel.Width := ClientWidth;
  jotter_rel_xy_readout_panel.Width := ClientWidth;
end;
//______________________________________________________________________________

end.
