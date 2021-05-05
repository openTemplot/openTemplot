
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

unit metric_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls { OT-FIRST , ReadHTML, framview};

type
  Tmetric_form = class(TForm)
    mm_box_edit: TEdit;
    inch_box_edit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    size_panel: TPanel;
    size_button: TButton;
    scale_panel: TPanel;
    scale_button: TButton;
    feet_inches_groupbox: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    feet_box_edit: TEdit;
    sixteenths_box_edit: TEdit;
    dec_inches_box_edit: TEdit;
    calculate_panel: TPanel;
    calculate_button: TButton;
    whole_inch_panel: TPanel;
    Label8: TLabel;
    approx_label: TLabel;
    decimal_places_groupbox: TGroupBox;
    decimal_updown: TUpDown;
    places_label: TLabel;
    scale_groupbox: TGroupBox;
    Label10: TLabel;
    scale_label: TLabel;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    datestamp_label: TLabel;
    copy_button: TButton;
    paste_button: TButton;
    close_panel: TPanel;
    close_button: TButton;
    close_label: TLabel;
    Label9: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure calculate_buttonClick(Sender: TObject);
    procedure mm_box_editExit(Sender: TObject);
    procedure inch_box_editExit(Sender: TObject);
    procedure feet_box_editExit(Sender: TObject);
    procedure sixteenths_box_editExit(Sender: TObject);
    procedure dec_inches_box_editExit(Sender: TObject);
    procedure mm_box_editEnter(Sender: TObject);
    procedure inch_box_editEnter(Sender: TObject);
    procedure feet_box_editEnter(Sender: TObject);
    procedure dec_inches_box_editEnter(Sender: TObject);
    procedure sixteenths_box_editEnter(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure scale_buttonClick(Sender: TObject);
    procedure size_buttonClick(Sender: TObject);
    procedure calculate_panelClick(Sender: TObject);
    procedure decimal_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormActivate(Sender: TObject);
    procedure colour_patchClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure copy_buttonClick(Sender: TObject);
    procedure paste_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  metric_form: Tmetric_form;

  metric_form_mm_now: extended = 0;

procedure metric_form_do_update;
procedure restore_metric_formsize_as_previous(previous_position: integer);

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  control_room, pad_unit, math_unit, colour_unit;

var

  current_box: TEdit;


procedure font_all(font_colour: integer); forward;
procedure blank_top2; forward;

//___________________________________________________________________________________

procedure metric_form_do_update;

var
  feet_now, whole_inch_now, sixt_now: integer;
  thou_now, thou_inches_now, sixteenths_now: integer;
  dec_inches_now: extended;
  mm_format_str, inch_format_str: string;

begin
  if ABS(metric_form_mm_now) < minfp then
    metric_form_mm_now := 0;
  if ABS(metric_form_mm_now) > 50.0E6 then
    metric_form_mm_now := 50.0E6 * SGZ(metric_form_mm_now); // otherwise integer overflow during calcs.
  try
    with metric_form do begin

      // first fill top 2 boxes...

      case decimal_updown.Position of
        // number of decimal places precision to display.
        0: begin
          mm_format_str := '0';
          inch_format_str := '0.0';      // one extra for inches.
        end;

        1: begin
          mm_format_str := '0.0';
          inch_format_str := '0.00';
        end;

        2: begin                          // default
          mm_format_str := '0.00';
          inch_format_str := '0.000';
        end;

        3: begin
          mm_format_str := '0.000';
          inch_format_str := '0.0000';
        end;

        4: begin
          mm_format_str := '0.0000';
          inch_format_str := '0.00000';
        end;

        5: begin
          mm_format_str := '0.00000';
          inch_format_str := '0.000000';
        end;

        6: begin
          mm_format_str := '0.000000';
          inch_format_str := '0.0000000';
        end;

      end;//case

      mm_box_edit.Text := '  ' + FormatFloat(mm_format_str, metric_form_mm_now);
      inch_box_edit.Text := '  ' + FormatFloat(inch_format_str, metric_form_mm_now / 25.4);

      // convert to thou ...

      thou_now := Round(metric_form_mm_now / 25.4 * 1000);     //  round to nearest thou for starters.

      feet_now := thou_now div 12000;          //  number of whole feet in this.
      thou_inches_now := thou_now mod 12000;   //  and number of thou over.

      dec_inches_now := thou_inches_now / 1000;  //  to decimal inches (extended).

      // convert to sixteenths...


      sixteenths_now := Round(metric_form_mm_now / 25.4 * 16) - (feet_now * 12 * 16);
      //  round to nearest sixteenth over.

      whole_inch_now := sixteenths_now div 16;     // number of whole inches.
      sixt_now := sixteenths_now mod 16;           // number of sixteenths over.

      // fill the remaining boxes...

      feet_box_edit.Text := '  ' + IntToStr(feet_now);
      whole_inch_panel.Caption := '  ' + IntToStr(whole_inch_now);
      sixteenths_box_edit.Text := '  ' + IntToStr(sixt_now);

      dec_inches_box_edit.Text := '  ' + FormatFloat(inch_format_str, dec_inches_now);

      if ABS(round_float(metric_form_mm_now / 25.4 * 16, 6) - round_float(metric_form_mm_now / 25.4 * 16, 0)) <
        minfp then
        approx_label.Hide      // rounding error on the sixteenths lees than 1 millionth of a sixteenth.
      else
        approx_label.Show;

      font_all(clRed);
      metric_form.scale_panel.Show;
      metric_form.size_panel.Show;
      //scale_groupbox.Show;

    end;//with
  except
    metric_form_mm_now := 0;       // conversion errors
  end;//try
end;
//_____________________________________________________________________________________

function docalc(nowtext: string): extended;

begin
  nowtext := remove_space_str(nowtext);  // strip all spaces.
  if nowtext = '' then
    nowtext := '0';     //  and then allow null as zero.

  try
    Result := ABS(StrToFloat(nowtext));  //  convert input text to float (no negs).
  except
    Result := 0;
  end;//try
end;
//_________________________________________________________________________________________

procedure Tmetric_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;         //  hide TEMPLOT on PAUSE key.
end;
//_____________________________________________________________________________________

procedure Tmetric_form.calculate_buttonClick(Sender: TObject);

begin
  // do nothing, clicking here generates an on exit event for the edit box just left.
end;
//_______________________________________________________________________________________

procedure Tmetric_form.mm_box_editExit(Sender: TObject);

begin
  metric_form_mm_now := docalc(mm_box_edit.Text);
  metric_form_do_update;                             // go update all boxes
end;
//________________________________________________________________

procedure Tmetric_form.inch_box_editExit(Sender: TObject);

begin
  metric_form_mm_now := docalc(inch_box_edit.Text) * 25.4;
  metric_form_do_update;
end;
//_______________________________________________________________

procedure Tmetric_form.feet_box_editExit(Sender: TObject);

begin
  metric_form_mm_now := (docalc(feet_box_edit.Text) * 12 + docalc(dec_inches_box_edit.Text)) * 25.4;
  metric_form_do_update;
end;
//_______________________________________________________________

procedure Tmetric_form.dec_inches_box_editExit(Sender: TObject);

begin
  metric_form_mm_now := (docalc(feet_box_edit.Text) * 12 + docalc(dec_inches_box_edit.Text)) * 25.4;
  metric_form_do_update;
end;
//_______________________________________________________________

procedure Tmetric_form.sixteenths_box_editExit(Sender: TObject);

begin
  metric_form_mm_now := (docalc(feet_box_edit.Text) * 12 + docalc(whole_inch_panel.Caption) +
    docalc(sixteenths_box_edit.Text) / 16) * 25.4;
  metric_form_do_update;
end;
//______________________________________________________________

procedure box_entered;

begin
  if metric_form.ActiveControl is TEdit then
    current_box := TEdit(metric_form.ActiveControl)     // for copy and paste.
  else
    run_error(231);
  metric_form.scale_panel.Hide;
  metric_form.size_panel.Hide;
end;
//_________________________________________________________________

procedure Tmetric_form.mm_box_editEnter(Sender: TObject);

begin
  box_entered;
  font_all(clBlack);
  mm_box_edit.Font.Color := clRed;
end;
//____________________________________________________________________________________

procedure Tmetric_form.inch_box_editEnter(Sender: TObject);

begin
  box_entered;
  font_all(clBlack);
  inch_box_edit.Font.Color := clRed;
end;
//______________________________________________________________________________________

procedure Tmetric_form.feet_box_editEnter(Sender: TObject);

begin
  box_entered;
  blank_top2;
  feet_box_edit.Font.Color := clRed;
end;
//______________________________________________________________________________________

procedure Tmetric_form.dec_inches_box_editEnter(Sender: TObject);

begin
  box_entered;
  blank_top2;
  whole_inch_panel.Font.Color := Color;
  sixteenths_box_edit.Font.Color := clBlack;
  dec_inches_box_edit.Font.Color := clRed;
end;
//________________________________________________________________________________________

procedure Tmetric_form.sixteenths_box_editEnter(Sender: TObject);

begin
  box_entered;
  blank_top2;
  dec_inches_box_edit.Font.Color := clBlack;
  sixteenths_box_edit.Font.Color := clRed;
end;
//_________________________________________________________________________________________

procedure font_all(font_colour: integer);     // change text colour in all boxes.

begin
  with metric_form do begin
    mm_box_edit.Font.Color := font_colour;
    inch_box_edit.Font.Color := font_colour;
    feet_box_edit.Font.Color := font_colour;
    sixteenths_box_edit.Font.Color := font_colour;
    dec_inches_box_edit.Font.Color := font_colour;

    if font_colour = clBlack then
      whole_inch_panel.Font.Color := metric_form.Color
    else
      whole_inch_panel.Font.Color := clBlue;
  end;//with
end;
//_________________________________________________________________________________________

procedure blank_top2;                        // blank text in top 2 boxes.

var
  font_colour: integer;

begin
  font_colour := clBlack;
  with metric_form do begin
    mm_box_edit.Font.Color := font_colour;
    inch_box_edit.Font.Color := font_colour;
  end;//with
end;
//______________________________________________________________________________________

procedure Tmetric_form.close_buttonClick(Sender: TObject);

begin
  Close;
end;
//________________________________________________________________________________________

procedure Tmetric_form.scale_buttonClick(Sender: TObject);

var
  ratio: extended;

begin
  ratio := 304.8 / scale;
  metric_form_mm_now := metric_form_mm_now / ratio;
  metric_form_do_update;
  font_all(clFuchsia);
end;
//_________________________________________________________________________________________

procedure Tmetric_form.size_buttonClick(Sender: TObject);

var
  ratio: extended;

begin
  ratio := 304.8 / scale;
  metric_form_mm_now := metric_form_mm_now * ratio;
  metric_form_do_update;
  font_all(clAqua);
end;
//_____________________________________________________________________________________

procedure Tmetric_form.calculate_panelClick(Sender: TObject);

begin
  calculate_button.SetFocus; // this generates an onExit event from the box just used.
end;
//______________________________________________________________________________________

procedure Tmetric_form.decimal_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  places_label.Caption := IntToStr(decimal_updown.Position);
  metric_form_do_update;
end;
//_______________________________________________________________________________________

procedure Tmetric_form.FormActivate(Sender: TObject);

begin
  scale_label.Caption := round_str(scale, 2);      // current scale.
  current_box := mm_box_edit;                     // for copy and paste.
end;
//___________________________________________________________________________________

procedure Tmetric_form.colour_patchClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  metric  calculator', Color);
end;
//_______________________________________________________________________________________

procedure Tmetric_form.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = Char(13) then begin
    Key := Char(0);                // otherwise we get a beep.
    calculate_button.SetFocus;   // generate onExit event for the current control.
  end;
end;
//___________________________________________________________________________________________

procedure Tmetric_form.copy_buttonClick(Sender: TObject);

begin
  current_box.SelectAll;
  current_box.CopyToClipboard;
  current_box.SelLength := 0;             // undo selection.
  current_box.SelStart := 2;              // move cursor past first 2 blank spaces.
  current_box.SetFocus;                 // don't want focus on the copy button.
end;
//_______________________________________________________________________________________
procedure Tmetric_form.paste_buttonClick(Sender: TObject);

begin
  current_box.Clear;
  current_box.PasteFromClipboard;
  current_box.SetFocus;                 // so he can edit it.
end;
//______________________________________________________________________________

procedure Tmetric_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=600;
  // OT-FIRST ClientHeight:=336;
  AutoScroll := True;
end;
//______________________________________________________________________________

procedure metric_form_size_updown_click;

begin
  with metric_form do begin

    if size_updown.Position > size_updown.Tag  // ! position goes up, size goes down.
    then
      ScaleBy(9, 10);                   // scale the form contents down.

    if size_updown.Position < size_updown.Tag then
      ScaleBy(10, 9);                   // scale the form contents up.

    ClientHeight := VertScrollBar.Range;
    // don't need bottom margin - datestamp label provides this (aligned alBottom).
    ClientWidth := HorzScrollBar.Range + 4;      // allow 4 pixel right margin.
    ClientHeight := VertScrollBar.Range;
    // do this twice, as each affects the other (autoscroll).

    size_updown.Tag := size_updown.Position;   // and save for the next click.
  end;//with
end;//proc
//______________________________________________________________________________

procedure Tmetric_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  metric_form_size_updown_click;
end;
//______________________________________________________________________________

procedure restore_metric_formsize_as_previous(previous_position: integer);

begin
  with metric_form.size_updown do begin

    if (previous_position > Max) or (previous_position < Min) then
      EXIT;

    while Position < previous_position do begin
      Position := Position + 1;
      metric_form_size_updown_click;
    end;//while

    while Position > previous_position do begin
      Position := Position - 1;
      metric_form_size_updown_click;
    end;//while

  end;//with
end;//proc
//____________________________________________________________________


end.
