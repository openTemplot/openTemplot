
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
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

====================================================================================
*)


unit data_memo_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Clipbrd,
  StdCtrls;

type
  Tdata_child_form = class(TForm)
    data_memo: TMemo;
    copy_all_button: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure copy_all_buttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  data_child_form: Tdata_child_form;

implementation

uses pad_unit,switch_select;

{$R *.lfm}

//______________________________________________________________________________

procedure Tdata_child_form.FormCreate(Sender: TObject);

begin
  Windows.SetParent(Handle,pad_form.Handle); // OT-FIRST
  // OT-FIRST  Parent:=pad_form;

  AutoScroll:=False;
  // OT-FIRST ClientWidth:=565;
  // OT-FIRST ClientHeight:=570;
end;
//______________________________________________________________________________

procedure Tdata_child_form.FormResize(Sender: TObject);

begin
  copy_all_button.Left:=ClientWidth-copy_all_button.Width-30;

  data_memo.Width:=ClientWidth;
  data_memo.Height:=ClientHeight-data_memo.Top;
end;
//______________________________________________________________________________

procedure Tdata_child_form.copy_all_buttonClick(Sender: TObject);

begin
  Clipboard.AsText:=data_memo.Text;
end;
//______________________________________________________________________________

procedure Tdata_child_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  switch_info_showing:=False;             // added 208a

  switch_select_form.show_info_button.Caption:='show  switch  info';
  switch_select_form.ClientWidth:=switch_select_form.datestamp_label.Width;
  switch_select_form.ClientHeight:=switch_select_form.datestamp_label.Top+switch_select_form.datestamp_label.Height; // 211b  // added 208a
  switch_select_form.AutoScroll:=False;
end;
//______________________________________________________________________________

end.
