
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

====================================================================================
*)

unit mouse_colour_unit;

{$MODE Delphi}

interface

uses
  LCLType, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  Tmouse_colour_form = class(TForm)
    Label2: TLabel;
    add_label: TLabel;
    mouse_colour_shape: TShape;
    ok_panel: TPanel;
    ok_button: TButton;
    cancel_panel: TPanel;
    datestamp_label: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cancel_panelClick(Sender: TObject);
    procedure ok_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mouse_colour_form: Tmouse_colour_form;

implementation

uses colour_unit, control_room;

{$R *.lfm}

var
  got_colour: TColor = clWhite;


//______________________________________________________________________________

procedure Tmouse_colour_form.FormShow(Sender: TObject);

begin
  mouse_colour_shape.Brush.Color := Color;  // hidden at first
  add_label.Visible := False;
  ok_panel.Visible := False;
end;
//______________________________________________________________________________

function get_colour_at_mouse: TColor;

var
  canv: TCanvas;
  mouse_point: TPoint;

begin
  Result := clWhite;       // init

  canv := TCanvas.Create;
  try
    canv.Handle := GetDC(0);
    GetCursorPos(mouse_point);
    Result := canv.Pixels[mouse_point.X, mouse_point.Y];
    ReleaseDC(0, canv.Handle);
  finally
    canv.Free;
  end;//try
end;
//______________________________________________________________________________

procedure Tmouse_colour_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_SPACE then begin
    got_colour := get_colour_at_mouse;
    mouse_colour_shape.Brush.Color := got_colour;
    add_label.Visible := True;
    ok_panel.Visible := True;
    Key := 0;
  end;
end;
//______________________________________________________________________________

procedure Tmouse_colour_form.FormCreate(Sender: TObject);

begin

  AutoScroll := False;

  // OT-FIRST ClientWidth:=440;
  // OT-FIRST ClientHeight:=180;
end;
//______________________________________________________________________________

procedure Tmouse_colour_form.cancel_panelClick(Sender: TObject);

begin
  ModalResult := mrCancel;
end;
//______________________________________________________________________________

procedure Tmouse_colour_form.ok_buttonClick(Sender: TObject);

var
  n: integer;
  name_str: string;
  colour_changed: boolean;

begin

  colour_changed := False;

  with colour_form.colour_dialog.CustomColors do begin

    for n := 0 to 15 do begin

      name_str := 'Color' + Char(Ord('A') + n);

      if Values[name_str] = 'FFFFFF'                          // replace first clWhite found
      then begin
        Values[name_str] := IntToHex(got_colour, 6);
        colour_changed := True;
        BREAK;
      end;
    end;//next

    if colour_changed = False then
      Values['ColorP'] := IntToHex(got_colour, 6);     // or overwrite final one if all used

  end;//with

  ModalResult := mrOk;
end;
//______________________________________________________________________________

end.
