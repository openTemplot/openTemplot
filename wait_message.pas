
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
unit wait_message;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Twait_form = class(TForm)
    waiting_label: TLabel;
    signal_image: TImage;
    cancel_button: TButton;
    wait_progressbar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure cancel_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  wait_form: Twait_form;

procedure wait_form_onshow;

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  pad_unit;

//__________________________________________________________________________________________

procedure Twait_form.FormCreate(Sender: TObject);

begin
  if Screen.DesktopWidth < 1000 then begin
    Top := 160;
    Left := 240;
  end;

  // OT-FIRST ClientWidth:=234;
  // OT-FIRST ClientHeight:=64;
  AutoScroll := False;
end;
//______________________________________________________________________________

procedure Twait_form.cancel_buttonClick(Sender: TObject);

begin
  wait_cancel_clicked := True;
  Close;
end;
//______________________________________________________________________________

procedure wait_form_onshow;

var
  new_width, min_width: integer;

  // 0.95.a      // 205b  label_width bug fix in Wine
begin
  with wait_form do begin
    waiting_label.Width := Canvas.TextWidth(waiting_label.Caption) + 10;

    new_width := waiting_label.Left + waiting_label.Width + 2;  // make sure it is all visible

    min_width := cancel_button.Left + cancel_button.Width + 12;

    if cancel_button.Visible = True        // bug-fix 208d
    then begin
      if new_width < min_width then
        new_width := min_width;  // ensure cancel button is visible.
    end
    else begin
      if new_width < cancel_button.Left then
        new_width := cancel_button.Left;  // minimum width.
    end;

    ClientWidth := new_width;
  end;//with
end;
//______________________________________________________________________________

procedure Twait_form.FormShow(Sender: TObject);

begin
  wait_form_onshow;
end;
//______________________________________________________________________________

end.
