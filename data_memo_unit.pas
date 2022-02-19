
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

================================================================================
*)

{ }
unit data_memo_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Clipbrd,
  control_room, math_unit,
  StdCtrls;

type

  { Tdata_child_form }

  Tdata_child_form = class(TForm)
    data_memo: TMemo;
    copy_all_button: TButton;
    datestamp_label: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure copy_all_buttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure show_template_info(list_position: Integer);
    procedure set_content(list_position: Integer);
    procedure reset_position();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  data_child_form: Tdata_child_form;

implementation

{$R *.lfm}

uses
  pad_unit, switch_select, template;

var
  template_showing: boolean = False;

//______________________________________________________________________________

procedure Tdata_child_form.FormCreate(Sender: TObject);

begin
  AutoScroll := False;

  reset_position();   // child on pad
end;
//______________________________________________________________________________

procedure Tdata_child_form.FormResize(Sender: TObject);

begin
  copy_all_button.Left := ClientWidth - copy_all_button.Width - 40;

  datestamp_label.Top := ClientHeight - datestamp_label.Height;
  datestamp_label.Width := ClientWidth;

  data_memo.Width := ClientWidth - data_memo.Left;
  data_memo.Height := ClientHeight - data_memo.Top - datestamp_label.Height;

end;
//______________________________________________________________________________

procedure Tdata_child_form.copy_all_buttonClick(Sender: TObject);

begin
  Clipboard.AsText := data_memo.Text;
end;
//______________________________________________________________________________

procedure Tdata_child_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  switch_info_showing := False;             // added 208a

  switch_select_form.show_info_button.Caption := 'show  switch  info';
  switch_select_form.ClientWidth := switch_select_form.datestamp_label.Width;
  switch_select_form.ClientHeight :=
    switch_select_form.datestamp_label.Top + switch_select_form.datestamp_label.Height;
  // 211b  // added 208a
  switch_select_form.AutoScroll := False;
end;
//______________________________________________________________________________

procedure Tdata_child_form.show_template_info(list_position: Integer);

begin
  set_content(list_position);
  data_child_form.Show;
  template_showing := True;
end;
//______________________________________________________________________________

procedure Tdata_child_form.set_content(list_position: Integer);

var
  info_str, memo_text_str: string;

begin
  if (keeps_list.Count < 1) or (memo_list.Count < 1) or (list_position < 0) or
    (list_position > (keeps_list.Count - 1)) then
    EXIT;

  if not template_showing then
    reset_position();

  if list_position = -1 then // Box is empty
  begin
    if (template_showing = True) and (Visible = True) then
      data_memo.Text := insert_crlf_str(' ||      box  empty');  //  replace embedded | chars with a CR.
  end

  else                       // Box has keeps
  begin
    memo_text_str := memo_list.Strings[list_position];

    with keeps_list[list_position].template_info.keep_dims.box_dims1 do begin
      info_str := '    ' + IntToStr(list_position + 1) + '  ' + reference_string + '   ' +
        id_number_str + '||  ' + top_label +
        '||--------------------------------------------------------------' +
        '||      Information  about  this  template :' + '||( all dimensions in millimetres )'
        + '||' + keeps_list[list_position].name +
        '||--------------------------------------------------------------' +
        '||      Your  memo  notes  for  this  template :' + '||' + memo_text_str;
    end;//with

    data_memo.Text := insert_crlf_str(info_str);  //  replace embedded | chars with a CR

  end;
end;
//______________________________________________________________________________

procedure Tdata_child_form.reset_position();   // on pad

var
  margin_size: integer;

begin
  if Parent <> nil then
    Parent.RemoveControl(data_child_form);

  pad_form.InsertControl(data_child_form);

  // child forms have wide borders ...

  margin_size := datestamp_label.Height;   // for program sizing

  Left := pad_form.ClientWidth - Width - margin_size * 6;    // 6 arbitrary for child borders
  if Left < 10 then
    Left := 10;

  Top := pad_form.ClientHeight - Height - margin_size * 14;  // 14 arbitrary for child caption bar
  if Top < 10 then
    Top := 10;

end;
//______________________________________________________________________________

end.
