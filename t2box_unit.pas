(*

    This file is part of Templot3, a computer program for the design of model railway track.
    Copyright (C) 2019  Martin Wynne.  email: martin@templot.com


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
unit t2box_unit;          // Loading a Templot2 '.box' format

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  Tt2box_form = class(TForm)
    load_t2box_dialog: TOpenDialog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  t2box_form: Tt2box_form;

function import_t2box(file_str: string): boolean; // 290a

//______________________________________________________________________________

implementation

uses
  config_unit,
  keep_select, wait_message, pad_unit, info_unit, control_room, alert_unit,
  math_unit, shove_timber, rail_options_unit,
  shoved_timber, template;

{$R *.lfm}

//______________________________________________________________________________

function import_t2box(file_str: string): boolean; // 290a

var
  t2box_str: string;

  i: integer;

begin
  Result := False;  // init

  if (keeps_list.Count > 0) and (save_done = False) // something already there and not saved... ?
  then begin
    i := alert(7, '    import  Templot2  box  file  into  storage  box  -  save  first ?',
      'Your storage box contains one or more templates which have not yet been saved.'
      + '||Importing a T2 box file into your storage box will replace all of the existing templates and background track plan.'
      + '||These existing templates can be restored later by clicking the `0FILES > UNDO�RELOAD / UNDO CLEAR`1 menu item.'
      + ' But if any of these templates may be needed again, you should first save them in a named data file.', '', '', '', 'replace  existing  contents  without  saving', 'cancel  import',
      'save  existing  contents  before  importing', 0);
    case i of
      5:
        EXIT;
      6:
        if save_box(0, 0, 0, '') = False then
          EXIT;     // go save all the keeps box.
    end;//case
  end;

  if file_str = '' then begin

    with t2box_form.load_t2box_dialog do begin

      Title := '    load  or  reload  storage  box  from  T2 box  file ..';

      if his_load_file_name <> '' then
        InitialDir := ExtractFilePath(his_load_file_name)
      else
        InitialDir := Config.GetDir(cudiBoxes);

      Filter := ' storage  box  in  Templot2  format  (*.box)|*.box';
      Filename := '*.box';

      if Execute = False then
        EXIT;          // get the file name.

      t2box_str := FileName;

      his_load_file_name := t2box_str;       // so we can use the same folder next time.

    end;//with

  end
  else
    t2box_str := file_str;

  if FileExists(t2box_str) = False then begin
    alert(5, '    error  -  file  not  found',
      '||The file :' + '||' + t2box_str +
      '||is not available. Please check that the file you require exists in the named folder. Then try again.'
      + '||No changes have been made to your storage box.',
      '', '', '', '', 'cancel  reload', '', 0);
    EXIT;
  end;

  // init for version_mismatch..

  loaded_version := 50000;   // init for lowest template version in the file
  later_file := False;       // init

  loading_in_progress := True;
  // lock out any auto backups while loading -- in case any dialogs get shown and OnIdle fires

  wait_form.cancel_button.Hide;
  wait_form.waiting_label.Caption := 'importing  T2 box  templates ...';
  wait_form.waiting_label.Width := wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);
  // bug fix for Wine
  wait_form.Show;

  if Application.Terminated = False then
    Application.ProcessMessages;           // let the wait form fully paint.

  clear_keeps(False, True);
  // first clear all existing (sets save_done:=True), and save existing for undo.

  //if do_import(t2box_str) = False                             // do the import
  //then
  //  ShowMessage('error: file : ' + t2box_str + #13 + #13 + 'did not load correctly');
    ShowMessage('Sorry. This facility is not yet implemented');

  // out for import - save_done:= NOT resave_needed;    // this boxful matches file.

  if keeps_list.Count > 0 then
    save_done := False;      // if any imported, need saving in .box format after import

  boxmru_update(t2box_str);   // update the mru list.

  Result := True;                // file loaded.

  if later_file = True then begin
    alert(1, 'php/980    later  file   -   ( from  version  ' + FormatFloat(
      '0.00', loaded_version / 100) + ' )',
      'The file which you just reloaded contained one or more templates from a later version of Templot than this one.'
      +
      ' Some features may not be available or may be drawn differently.' +
      '||The earliest loaded template was from version  ' + FormatFloat('0.00', loaded_version / 100) +
      '|This version of Templot is  ' + GetVersionString(voShort) +
      '||Please refer to the Templot web site at  templot.com  for information about upgrading to the latest version, or click| <A HREF="online_ref980.85a">more information online</A> .',
      '', '', '', '', '', 'continue', 0);
  end;

  if loaded_version < 200        // loaded from ealier version than this
  then begin
    i := alert(2, 'php/980    old  file   -   ( from  version  ' + FormatFloat(
      '0.00', loaded_version / 100) + ' )',
      'The file which you just reloaded contained one or more templates from an earlier version of Templot.'
      + '||These have been modified to make them compatible with this version, but some features may now be drawn differently or require adjustment.' + '||The earliest loaded template was from version  ' + FormatFloat('0.00', loaded_version / 100) + '|This version of Templot is  ' + GetVersionString(voShort) + '||Click for <A HREF="online_ref980.85a">more information online</A> about the differences between these two versions.' + '||green_panel_begin tree.gif The template name labels are now shown in the boxed style by default.' + ' To revert to the previous style click the `0trackpad > trackpad background options > background name labels > transparent`1 menu item,|or click below.' + '||To hide the name labels, press the `0END`2 key on the keyboard, or the `0SHIFT+ENTER`2 keys, or click the `0trackpad > hide name labels`1 menu item, or click below.green_panel_end', '', '', 'hide  name  labels', 'change  to  transparent  name  labels', '', 'continue', 0);

    if i = 3 then
      hide_name_labels := True;

    if i = 4 then
      pad_form.transparent_names_menu_entry.Checked := True;    // radio item.
  end;

  wait_form.Close;
  current_state(-1);

  loading_in_progress := False;  // allow backups only after dialogs

  show_and_redraw(True, True);  // redraw pad when ready.
end;
//______________________________________________________________________________

end.
