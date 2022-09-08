
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

unit colour_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  Tcolour_form = class(TForm)
    colour_label: TLabel;
    font_dialog: TFontDialog;
    colour_dialog: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure colour_dialogShow(Sender: TObject);
    procedure font_dialogShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  colour_form: Tcolour_form;

  //------------------------

  //no_extra_colours:boolean=False;  //%%%%

  colour_dialog_caption_str: string = '';
  font_dialog_caption_str: string = '';

function get_colour(str: string; colour: TColor): TColor;                  //  get a new colour.
function get_font(str: string; fon: TFont; more_colours: boolean): TFont;   //  get a new font.

//__________________________________________________________________________________________

implementation

{$R *.lfm}

uses
  pad_unit, alert_unit, control_room, math_unit;

//________________________________________________________________________________________

function get_colour(str: string; colour: TColor): TColor;     //  get a new colour.

var
  dlg: TColorDialog;
begin
  dlg := TColorDialog.Create(nil);
  try
    dlg.Color := colour;
    dlg.Title := '   ' + str;

    showing_dialog := True;        // 212a Wine bug

    if dlg.Execute = True then
      Result := dlg.Color
    else
      Result := colour;

    showing_dialog := False;

  finally
    dlg.Close;
  end;

  show_and_redraw(True, False);    // no rollback.
end;
//_________________________________________________________________________________________

function get_font(str: string; fon: TFont; more_colours: boolean): TFont;    //  get a new font.

begin
  with colour_form do begin
    Caption := '    ' + Application.Title + '  fonts  and  text  colours ...';
    colour_label.Caption := '  ' + str + ' ...';
    font_dialog.Font.Assign(fon);
    Show;
    BringToFront;

    font_dialog_caption_str := str;

    showing_dialog := True;   // 212a Wine bug

    if font_dialog.Execute = True then
      Result := font_dialog.Font
    else begin
      showing_dialog := False;

      Result := fon;
      Hide;
      EXIT;
    end;

    showing_dialog := False;

    Hide;
  end;//with

  if (no_extra_colours_msg_pref = False) and (more_colours = True) and (hi_color = True) then
  begin
    // alert_box.font_button.Visible:=False;         // don't confuse him.

    alert_box.preferences_checkbox.Checked := False;       //%%%%
    alert_box.preferences_checkbox.Show;

    if alert(4, '    more  font  colours ...',
      '||||Would you prefer a wider choice of colours for this font?',
      '', '', '', '', 'no  thanks', 'yes  please', 0) = 6 then
      Result.Color := get_colour(str, Result.Color);

    //%%%%  was"today"

    no_extra_colours_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
    alert_box.preferences_checkbox.Hide;

    // alert_box.font_button.Visible:=True;
  end;
end;
//______________________________________________________________________________

procedure Tcolour_form.FormCreate(Sender: TObject);

//var
//  custom_colours:TStrings;

begin
  // OT-FIRST ClientWidth:=900;
  // OT-FIRST ClientHeight:=36;
  AutoScroll := False;

  { now done in Object Inspector

    // must init the custom colours list in the dialog
    // in case it is accessed (e.g. from mouse colours) before the first time execute.

  custom_colours:=TStringList.Create;   // yes TStringLIST here according to the docs

  with custom_colours do begin
    Add('ColorA=EEAA55');        // water blue
    Add('ColorB=55AAEE');        // sand
    Add('ColorC=55EEAA');        // greenery
    Add('ColorD=EE55AA');        // granite
    Add('ColorE=DDDDDD');        // light grey
    Add('ColorF=AADD55');        // sea
    Add('ColorG=557799');        // earth
    Add('ColorH=BBBBBB');        // mid grey
    Add('ColorI=FFFFFF');        // all others white...
    Add('ColorJ=FFFFFF');
    Add('ColorK=FFFFFF');
    Add('ColorL=FFFFFF');
    Add('ColorM=FFFFFF');
    Add('ColorN=FFFFFF');
    Add('ColorO=FFFFFF');
    Add('ColorP=FFFFFF');
  end;//with

  colour_dialog.CustomColors.Assign(custom_colours);

  custom_colours.Free;

}

end;
//______________________________________________________________________________

procedure Tcolour_form.colour_dialogShow(Sender: TObject);

var
  new_name_str: string;   // must be locals

begin
  new_name_str := '    ' + colour_dialog_caption_str;

  if colour_dialog.Handle <> 0 then
    colour_dialog.Title := new_name_str;

end;
//______________________________________________________________________________

procedure Tcolour_form.font_dialogShow(Sender: TObject);

var
  new_name_str: string;   // must be locals

begin
  new_name_str := '    ' + font_dialog_caption_str;

  if font_dialog.Handle <> 0 then
    font_dialog.Title := new_name_str;
end;
//______________________________________________________________________________

end.
