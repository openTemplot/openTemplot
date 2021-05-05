
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

unit action_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type

  { Taction_form }

  Taction_form = class(TForm)
    action_label: TLabel;
    trail_dim_label: TLabel;
    image_panel: TPanel;
    action_1_image: TImage;
    action_2_image: TImage;
    action_panel_timer: TTimer;
    finish_button: TButton;

    procedure FormCreate(Sender: TObject);
    procedure action_1_imageClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure trail_dim_labelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure action_panel_timerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure finish_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  action_form: Taction_form;

//-----------------

procedure action_panel_resize;

//_______________________________________________________________________________________
implementation

{$BOOLEVAL ON}


uses pad_unit, math_unit, wait_message;

{$R *.lfm}

var
  action_resizing: boolean = True;        // True in case we get a resize call on create or show.

//_______________________________________________________________________________________

procedure Taction_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(action_form);

  AutoScroll := False;

  ClientWidth := 170;
  ClientHeight := 80;
end;
//________________________________________________________________________________________

procedure action_panel_resize;
// !!! must do width first for some reason (to do with autosizing labels?)

var
  new_width, new_height: integer;
  //screen_dpi:integer;              // 211b

begin
  //screen_dpi:=Screen.PixelsPerInch;    // 211b for dpi-aware
  with action_form do begin
    if Tag = 0 then begin
      action_resizing := True;
      // flag OnResize that we are changing the size, not him.

      trail_dim_label.Top := action_label.Top + Canvas.TextHeight(' ') + 2;
      new_width := image_panel.Width + max_i(
        Canvas.TextWidth('  •  ' + action_label.Caption), Canvas.TextWidth('  ' + trail_dim_label.Caption));
      new_height := trail_dim_label.Top + Canvas.TextHeight(' ') + 2;

      image_panel.Height := new_height;     // 215a mods...

      finish_button.Top := new_height + 4;
      finish_button.Width := new_width;

      ClientWidth := new_width;
      ClientHeight := finish_button.Top + finish_button.Height;

      if Application.Terminated = False then
        Application.ProcessMessages;  // to make sure it updates immediately.
      action_resizing := False;
    end;
  end;//with
end;
//___________________________________________________________________________________________

procedure Taction_form.action_1_imageClick(Sender: TObject);

begin
  mouse_symbol_click;
end;
//__________________________________________________________________________________________

procedure Taction_form.FormClick(Sender: TObject);

begin
  Hide;
end;
//_____________________________________________________________________________________

procedure Taction_form.trail_dim_labelClick(Sender: TObject);

// cancel adjusts and go adjust dimension by direct entry...
begin
  action_label_click;     // (pad_unit).
end;
//________________________________________________________________________________________

procedure Taction_form.FormResize(Sender: TObject);

begin
  if (action_resizing = False) and (action_form.Showing = True) then
    action_form.Tag := 1; // flag he resized it, so don't change it again.

  finish_button.Width := ClientWidth;
  finish_button.Height := ClientHeight - finish_button.Top;
end;
//______________________________________________________________________________

procedure Taction_form.FormActivate(Sender: TObject);

begin
  action_resizing := False;     // so he can resize it now.
end;
//______________________________________________________________________________

procedure Taction_form.action_panel_timerTimer(Sender: TObject);

begin
  action_panel_timer.Enabled := False;  //  one-shot.
  if mouse_modify = -1 then
    Close;
end;
//______________________________________________________________________________

procedure Taction_form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin
  if mouse_modify <> -1 then begin
    // mouse action in force, so close via a timer delay..
    cancel_adjusts(False);    // enables the close timer.
    CanClose := False;          // wait for it.
  end;
end;
//______________________________________________________________________________

procedure Taction_form.finish_buttonClick(Sender: TObject);   // 215a

begin
  cancel_adjusts(False);
  if pad_form.arrow_button_dummy_trackbar.Showing = True then
    pad_form.arrow_button_dummy_trackbar.SetFocus;
end;
//______________________________________________________________________________

end.
