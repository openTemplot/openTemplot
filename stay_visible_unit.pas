
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

unit stay_visible_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

type
  Tstay_visible_form = class(TForm)
    font_button: TButton;
    help_scrollbox: TScrollBox;
    text_label: TLabel;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    scroll_groupbox: TGroupBox;
    slow_button: TRadioButton;
    fast_button: TRadioButton;
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure font_buttonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure fast_buttonClick(Sender: TObject);
    procedure slow_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  stay_visible_form: Tstay_visible_form;

implementation

{$R *.lfm}

uses
  colour_unit, math_unit, pad_unit;

var
  no_onresize: boolean = False;

//__________________________________________________________________________________________

procedure Tstay_visible_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  no_onresize := True;                            // don't permit on-resize until finished.

  if size_updown.Position > size_updown.Tag       // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                        // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                        // scale the form contents up.

  if fast_button.Checked = True then
    help_scrollbox.VertScrollBar.Increment := Canvas.TextHeight(' ') div 2;
  // scrolling jumps 1/2 line at a time.

  size_updown.Tag := size_updown.Position;        // and save for the next click.
  no_onresize := False;                           // can now resize the contents again if he wants to.
  Resize;                                       // do it anyway to normalize the contents.
end;
//__________________________________________________________________________________________

procedure Tstay_visible_form.colour_panelClick(Sender: TObject);

begin
  help_scrollbox.Color := get_colour('choose  a  new  colour  for  the  stay-visible  text  panel',
    help_scrollbox.Color);
end;
//___________________________________________________________________________________________

procedure Tstay_visible_form.font_buttonClick(Sender: TObject);

begin
  text_label.Font.Assign(get_font(
    'choose  a  new  font  and  text  colour  for  the  stay-visible  text', text_label.Font, True));
  Canvas.Font.Assign(text_label.Font);
  if fast_button.Checked = True then
    help_scrollbox.VertScrollBar.Increment := Canvas.TextHeight(' ') div 2;
  // scrolling jumps 1/2 line at a time.
end;
//__________________________________________________________________________________________

procedure Tstay_visible_form.FormResize(Sender: TObject);

var
  label_height: integer;

begin
  if (Showing = True) and (no_onresize = False) then begin
    help_scrollbox.Height := ClientHeight - help_scrollbox.Top;
    help_scrollbox.Width := ClientWidth;
    text_label.Width := help_scrollbox.ClientWidth - text_label.Left * 2;
    if text_label.Width > 1 then
      label_height := limits_i(12000, 32000, 2400000 div text_label.Width)
    // maintain nominal area 12000 x 200
    else
      label_height := 32000;
    text_label.Height := label_height;
  end;
end;
//______________________________________________________________________________________

procedure Tstay_visible_form.FormShow(Sender: TObject);

begin
  help_scrollbox.VertScrollBar.Position := 0;    // reset the scrollbar.
  Canvas.Font.Assign(text_label.Font);
  if fast_button.Checked = True then
    help_scrollbox.VertScrollBar.Increment := Canvas.TextHeight(' ') div 2;
  // scrolling jumps 1/2 line at a time.
end;
//________________________________________________________________________________________

procedure Tstay_visible_form.fast_buttonClick(Sender: TObject);

begin
  help_scrollbox.VertScrollBar.Increment := Canvas.TextHeight(' ') div 2;
  // scrolling jumps 1/2 line at a time.
end;
//________________________________________________________________________________________

procedure Tstay_visible_form.slow_buttonClick(Sender: TObject);

begin
  help_scrollbox.VertScrollBar.Increment := 1;
end;
//_________________________________________________________________________________________

procedure Tstay_visible_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(stay_visible_form);

  AutoScroll := True;
end;
//______________________________________________________________________________

end.
