
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
unit chat_unit;

{$MODE Delphi}

interface

uses
  LCLType, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, PrintersDlgs;

type

  { Tchat_form }

  Tchat_form = class(TForm)
    chat_exit_button: TButton;
    datestamp_label: TLabel;
    font_button: TButton;
    chat_scrollbox: TScrollBox;
    chat_box: TLabel;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure font_buttonClick(Sender: TObject);
    procedure colour_patchClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  chat_form: Tchat_form;

procedure chat(msg_str: string);
procedure do_fkey_chart;          // print function key chart.

//____________________________________________________________________________________________

implementation

{$R *.lfm}

uses Printers, math_unit, control_room, colour_unit, calibration_unit, alert_unit,
  pad_unit, print_unit;

//_________________________________________________________________________________________

procedure chat(msg_str: string);       //  show message in chat box.

begin
  chat_form.chat_box.Caption := insert_crlf_str(msg_str + '||[]| ');
  do_show_modal(chat_form);     // 212a  ShowModal
end;
//_________________________________________________________________________________________

procedure Tchat_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.
end;
//__________________________________________________________________________________________

procedure Tchat_form.font_buttonClick(Sender: TObject);

begin
  chat_box.Font.Assign(get_font('choose  a  new  font  and  text  colour  for  the  chat  window',
    chat_box.Font, True));
end;
//_________________________________________________________________________________________

procedure Tchat_form.colour_patchClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  chat  box', Color);
end;
//___________________________________________________________________________________________

procedure Tchat_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag
  // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                                           // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                                           // scale the form contents up.

  ClientHeight := VertScrollBar.Range;                               // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 4;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;
  // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;                           // and save for the next click.
end;
//_______________________________________________________________________________________

procedure Tchat_form.FormShow(Sender: TObject);

begin
  chat_scrollbox.VertScrollBar.Position := 0;    // reset the scrollbar.
end;
//____________________________________________________________________________________

procedure do_fkey_chart;                  // print function key chart.

const   //  WINAPI GetDeviceCaps Constants

  HORZRES = 8;
  VERTRES = 10;
  BITSPIXEL = 12;
  LOGPIXELSX = 88;
  LOGPIXELSY = 90;

var
  n: double;

  width_dots: integer;    // printer page-width in dots.
  length_dots: integer;   // printer page-length in dots.

  width_dpi: integer;     // reported dpi across width.
  length_dpi: integer;    // reported dpi down length.

  save_po: TPrinterOrientation;

  x, y, y0, y1, y2: double;
  str: string;

  str_len: integer;

  ////////////////////////////////////////////////////////////////////

  procedure do_text;

  begin
    with Printer.Canvas do
      TextOut(Round(x / 25.4 * width_dpi), Round(y / 25.4 * length_dpi), str);
  end;
  ///////////////////////////////////////////////////////////////////

begin

  if no_printer_available = True     // 0.93.a
  then begin
    show_modal_message('No printer available.');
    EXIT;
  end;

  with TPrintDialog.Create(nil) do begin
    // 0.93.a created in code because of startup error if no printer available.
    try
      if Execute = False then
        EXIT;
    finally
      Free;
    end;//try
  end;//with

  //if chat_form.print_dialog.Execute=False then EXIT;

  save_po := Printer.Orientation;
  Printer.Orientation := poLandscape;
  if Application.Terminated = False then
    Application.ProcessMessages;

  // Templot0 getting printer resolution
  width_dpi := Printer.XDPI;
  length_dpi := Printer.YDPI;

  // papersize dimensions
  width_dots := Printer.PaperSize.Width;
  length_dots := Printer.PaperSize.Height;

  if (width_dpi < 1) or (length_dpi < 1) or (width_dots < 1) or (length_dots < 1)
  // division by zero, or negative.
  then begin
    alert(0, '   printer  software  problem ..',
      '|||Templot0 is unable to access your printer software.'
      + '||Please check your printer installation.',
      '', '', '', '', 'cancel  printing', '', 0);

    Printer.Orientation := save_po;
    EXIT;
  end;

  with Printer do begin
    with Canvas do begin

      Font.Name := 'Arial';
      Font.Color := clBlack;

      Brush.Color := clWhite;       // text white background.
      Brush.Style := bsSolid;

      Pen.Color := clGray;  // 0.97.a clBlack;
      Pen.Mode := pmCopy;
      Pen.Style := psSolid;
      Pen.Width := 3;
      // this chart has been limited to 266 mm long in order to fit US letter paper.
      BeginDoc;

      MoveTo(0, 0);
      LineTo(0, Round(120 / 25.4 * length_dpi));

      n := 20;                                      // left margin
      while n < 270 do begin
        MoveTo(Round(n / 25.4 * width_dpi), 0);                           // vertical grid bar top.
        LineTo(Round(n / 25.4 * width_dpi), Round(145 / 25.4 * length_dpi));  // vertical grid bar bottom.
        n := n + 20.5;
      end;//while

      n := 0;
      while n <= 120 do begin
        MoveTo(0, Round(n / 25.4 * length_dpi));                          // horizontal grid bar left.
        LineTo(Round(266 / 25.4 * width_dpi), Round(n / 25.4 * length_dpi));  // horizontal grid bar right.
        n := n + 40;
      end;//while

      MoveTo(Round(20 / 25.4 * width_dpi), Round(145 / 25.4 * length_dpi));
      LineTo(Round(266 / 25.4 * width_dpi), Round(145 / 25.4 * length_dpi));


      Font.Size := 20;

      Font.Style := [fsBold];

      x := 5;
      y := 147;

      x := x + 20.5;
      str := 'F1';
      do_text;
      x := x + 20.5;
      str := 'F2';
      do_text;
      x := x + 20.5;
      str := 'F3';
      do_text;
      x := x + 20.5;
      str := 'F4';
      do_text;
      x := x + 20.5;
      str := 'F5';
      do_text;
      x := x + 20.5;
      str := 'F6';
      do_text;
      x := x + 20.5;
      str := 'F7';
      do_text;
      x := x + 20.5;
      str := 'F8';
      do_text;
      x := x + 20.5;
      str := 'F9';
      do_text;
      x := x + 20.5;
      str := 'F10';
      do_text;
      x := x + 20.5;
      str := 'F11';
      do_text;
      x := x + 20.5;
      str := 'F12';
      do_text;


      Font.Size := 15;
      x := 0.5;
      y := 19;
      str := ' SHIFT';
      do_text;
      y := 25;
      str := '+CTRL';
      do_text;
      y := 65;
      str := ' SHIFT';
      do_text;
      y := 105;
      str := '  CTRL';
      do_text;

      Font.Size := 14;
      Font.Style := [];
      x := 7.5;
      y := 121;
      str := '^';
      do_text;

      Font.Size := 11;
      Font.Style := [fsItalic];
      x := 0;
      y := 125;
      str := 'hold down';
      do_text;

      x := 8;
      y := 149;
      str := 'press >';
      do_text;

      //---------------------------------

      x := 1;
      y0 := 124;
      y1 := 130;
      y2 := 136;

      Font.Size := 13;
      Font.Style := [];

      x := x + 20.5;
      y := y2;
      str := '  help';
      do_text;   // F1
      x := x + 20.5;
      y := y1;
      str := '   info';
      do_text;
      y := y2;
      str := ' toggle';
      do_text;   // F2

      Font.Style := [fsBold, fsItalic];  // mouse actions...

      x := x + 18;
      y := y1;
      str := 'approach';
      do_text;
      x := x + 2.5;
      y := y2;
      str := ' length';
      do_text;   // F3
      x := x + 20.5;
      y := y1;
      str := ' overall';
      do_text;
      y := y2;
      str := ' length';
      do_text;   // F4
      x := x + 20.5;
      y := y2;
      str := '   size';
      do_text;   // F5
      x := x + 20.5;
      y := y2;
      str := 'curving';
      do_text;   // F6
      x := x + 20.5;
      y := y2;
      str := '   shift';
      do_text;   // F7
      x := x + 20.5;
      y := y2;
      str := '  rotate';
      do_text;   // F8
      x := x + 20.5;
      y := y2;
      str := 'V-angle';
      do_text;   // F9
      x := x + 20.5;
      y := y2;
      str := 'K-angle';
      do_text;   // F10

      Font.Style := [];

      x := x + 20.5;
      y := y0;
      str := '   print';
      do_text;
      y := y1;
      str := ' control';
      do_text;
      y := y2;
      str := 'template';
      do_text;   // F11
      x := x + 20.5;
      y := y1;
      str := ' redraw';
      do_text;
      y := y2;
      str := ' & show';
      do_text;   // F12

      //-----------------------------------

      x := 1;

      Font.Size := 13;

      y0 := 98;
      y1 := 104;
      y2 := 110;

      Font.Style := [fsBold, fsItalic];  // mouse actions...

      x := x + 20.5;
      y := y1;
      str := '  spot';
      do_text;
      y := y2;
      str := ' zoom';
      do_text;  // CTRL-F1

      Font.Style := [];

      x := x + 20.5;
      y := y0;
      str := '  zoom';
      do_text;
      y := y1;
      str := 'and pan';
      do_text;
      y := y2;
      str := 'controls';
      do_text;  // CTRL-F2

      Font.Style := [fsBold, fsItalic];

      x := x + 19.5;
      y := y1;
      str := 'blanking';
      do_text;
      x := x + 1.0;
      y := y2;
      str := ' length';
      do_text;  // CTRL-F3
      x := x + 19.5;
      y := y0;
      str := '    roll';
      do_text;
      y := y1;
      str := 'rails and';
      do_text;
      y := y2;
      str := 'sleepers';
      do_text;  // CTRL-F4
      x := x + 1.0;
      x := x + 20.5;
      y := y2;
      str := '  orbit';
      do_text;  // CTRL-F5
      x := x + 20.5;
      y := y2;
      str := '  snake';
      do_text;  // CTRL-F6
      x := x + 20.5;
      y := y1;
      str := '   slew';
      do_text;
      y := y2;
      str := 'amount';
      do_text;  // CTRL-F7
      x := x + 20.5;
      y := y1;
      str := '  move';
      do_text;
      y := y2;
      str := '   peg';
      do_text;  // CTRL-F8
      x := x + 20.5;
      y := y1;
      str := '  roam';
      do_text;
      y := y2;
      str := '  along';
      do_text;  // CTRL-F9
      x := x + 20.5;
      y := y2;
      str := '  swell';
      do_text;  // CTRL-F10

      Font.Style := [];

      x := x + 20;
      y := y0;
      str := '   print';
      do_text;
      y := y1;
      str := 'backgnd';
      do_text;
      y := y2;
      str := 'templates';
      do_text;  // CTRL-F11

      Font.Style := [fsBold, fsItalic];

      x := x + 21.0;
      y := y0;
      str := 'turnout-road';
      do_text;
      y := y1;
      str := '    exit';
      do_text;
      y := y2;
      str := '  length';
      do_text;  // CTRL-F12    209a

      Font.Size := 11;

      Font.Style := [fsBold];

      x := 1;
      y := 90;

      x := x + 20.5;
      str := 'or  1  key';
      do_text;
      x := x + 20.5;
      str := 'or  2  key';
      do_text;
      x := x + 20.5;
      str := 'or  3  key';
      do_text;
      x := x + 20.5;
      str := 'or  4  key';
      do_text;
      x := x + 20.5;
      str := 'or  5  key';
      do_text;
      x := x + 20.5;
      str := 'or  6  key';
      do_text;
      x := x + 20.5;
      str := 'or  7  key';
      do_text;
      x := x + 20.5;
      str := 'or  8  key';
      do_text;
      x := x + 20.5;
      str := 'or  9  key';
      do_text;
      x := x + 20.5;
      str := 'or  0  key';
      do_text;
      x := x + 20.5;
      x := x + 20.5;
      //-----------------------------------

      x := 1;

      Font.Size := 13;

      Font.Style := [];

      y0 := 58;
      y1 := 64;
      y2 := 70;

      x := x + 20.5;
      y := y0;
      str := '  make';
      do_text;
      y := y1;
      str := 'diamond';
      do_text;
      y := y2;
      str := 'crossing';
      do_text;   // SHIFT-F1
      x := x + 20.5;
      y := y0;
      str := '  make';
      do_text;
      y := y1;
      str := '  cross-';
      do_text;
      y := y2;
      str := '  over';
      do_text;   // SHIFT-F2
      x := x + 20.5;
      y := y0;
      str := '  make';
      do_text;
      y := y1;
      str := 'double-';
      do_text;
      y := y2;
      str := 'track TS';
      do_text;   // SHIFT-F3
      x := x + 20.5;
      y := y0;
      str := '  make';
      do_text;
      y := y1;
      str := 'double-';
      do_text;
      y := y2;
      str := 'track MS';
      do_text;   // SHIFT-F4
      x := x + 20.5;
      y := y1;
      str := 'calibrate';
      do_text;
      y := y2;
      str := '  printer';
      do_text;   // SHIFT-F5
      x := x + 20.5;
      y := y0;
      str := '  rotate';
      do_text;
      y := y1;
      str := '  group';
      do_text;
      y := y2;
      str := '180 deg';
      do_text;   // SHIFT-F6
      x := x + 20.5;
      y := y0;
      str := '  make';
      do_text;
      y := y1;
      str := '  branch';
      do_text;
      y := y2;
      str := '  track';
      do_text;   // SHIFT-F7
      x := x + 20.5;
      y := y0;
      str := '  rotate';
      do_text;
      y := y1;
      str := ' control';
      do_text;
      y := y2;
      str := '180 deg';
      do_text;   // SHIFT-F8

      Font.Style := [fsBold, fsItalic];

      x := x + 20.5;
      y := y2;
      str := '  slide';
      do_text;   // SHIFT-F9

      Font.Style := [];

      x := x + 20.5;
      y := y1;
      str := '  shove';
      do_text;
      y := y2;
      str := ' timbers';
      do_text;   // SHIFT-F10

      Font.Style := [fsBold, fsItalic];

      x := x + 19.5;
      y := y0;
      str := 'crossing';
      do_text;
      y := y1;
      str := '  entry';
      do_text;
      y := y2;
      str := ' straight';
      do_text;   // SHIFT-F11
      x := x + 22.0;
      y := y1;
      str := '  gaunt';
      do_text;
      y := y2;
      str := '  offset';
      do_text;   // SHIFT-F12

      Font.Size := 11;

      Font.Style := [fsBold];

      x := 1 + 9 * 20.5;

      {                   y:=41; str:='     or';    do_text;
                    y:=46; str:=' decimal';   do_text;
                    y:=51; str:='    key';    do_text;
}

      x := x + 20.5;
      y := 50;
      str := 'or  ,  key';
      do_text;

      {
      x:=1+11*20.5; y:=41; str:='     or';   do_text;
                    y:=46; str:='  space';   do_text;
                    y:=51; str:='    bar';   do_text;

      x:=x+20.5; y:=50; str:='or  `  key';   do_text;
}

      //-----------------------------------

      x := 1;

      Font.Size := 13;

      Font.Style := [];

      y0 := 18;
      y1 := 24;
      y2 := 30;

      x := x + 20.5;
      y := y0;
      str := '   1st';
      do_text;
      y := y1;
      str := '  trans.';
      do_text;
      y := y2;
      str := ' radius';
      do_text;   // SHIFT-CTRL-F1
      x := x + 20.5;
      y := y0;
      str := '   2nd';
      do_text;
      y := y1;
      str := '  trans.';
      do_text;
      y := y2;
      str := ' radius';
      do_text;   // SHIFT-CTRL-F2

      Font.Style := [fsBold, fsItalic];   // mouse actions...

      x := x + 20.5;
      y := y1;
      str := '  trans.';
      do_text;
      y := y2;
      str := '  start';
      do_text;   // SHIFT-CTRL-F3
      x := x + 20.5;
      y := y1;
      str := '  trans.';
      do_text;
      y := y2;
      str := ' length';
      do_text;   // SHIFT-CTRL-F4
      x := x + 20.5;
      y := y1;
      str := 'slewing';
      do_text;
      y := y2;
      str := '  start';
      do_text;   // SHIFT-CTRL-F5
      x := x + 20.5;
      y := y1;
      str := 'slewing';
      do_text;
      y := y2;
      str := ' length';
      do_text;   // SHIFT-CTRL-F6
      x := x + 20.5;
      y := y1;
      str := '   shift';
      do_text;
      y := y2;
      str := '  group';
      do_text;   // SHIFT-CTRL-F7
      x := x + 20.5;
      y := y1;
      str := '  rotate';
      do_text;
      y := y2;
      str := '  group';
      do_text;   // SHIFT-CTRL-F8

      Font.Style := [];

      x := x + 20.5;
      y := y0;
      str := ' adjust';
      do_text;
      y := y1;
      str := ' check';
      do_text;
      y := y2;
      str := '  rails';
      do_text;   // SHIFT-CTRL-F9

      Font.Style := [fsBold, fsItalic];    // mouse action

      x := x + 20.5;
      y := y0;
      str := '  move';
      do_text;
      y := y1;
      str := '  page';
      do_text;
      y := y2;
      str := '  origin';
      do_text;   // SHIFT-CTRL-F10

      x := x + 20.5;
      y := y0;
      str := '   track';
      do_text;
      y := y1;
      str := ' centres';
      do_text;
      y := y2;
      str := '     TS';
      do_text;   // SHIFT-CTRL-F11

      Font.Style := [];

      x := x + 20.5;
      y := y0;
      str := 'skeleton';
      do_text;
      y := y1;
      str := '  mouse';
      do_text;
      y := y2;
      str := '   draw';
      do_text;   // SHIFT-CTRL-F12 toggle

      Font.Size := 11;

      Font.Style := [fsBold];

      x := 1;
      y := 8;

      x := x + 20.5;
      str := 'or  -  key';
      do_text;
      x := x + 20.5;
      str := 'or  =  key';
      do_text;
      x := x + 20.5;
      str := 'or  [  key';
      do_text;
      x := x + 20.5;
      str := 'or  ]  key';
      do_text;
      x := x + 20.5;
      str := 'or  ''  key';
      do_text;
      x := x + 20.5;
      str := 'or  #  key';
      do_text;
      x := x + 20.5;
      str := 'or  \  key';
      do_text;
      x := x + 20.5;
      str := 'or  /  key';
      do_text;
      x := x + 20.5;
      str := 'or  .  key';
      do_text;
      x := x + 20.5;
      x := x + 20.5;
      x := x + 20.5;
      str := 'or  ;  key  toggle';
      do_text;

      //--------------------------------------
      Font.Size := 15;
      Font.Style := [fsBold];

      x := 96;
      y := 162;
      str := 'OPENTEMPLOT  FUNCTION  KEY  CHART';
      do_text;

      Font.Size := 11;
      Font.Style := [];

      x := 36;
      y := 172;
      str := 'This chart is intended to be placed behind your keyboard, and shows only the menu shortcuts which use the function keys.';
      do_text;

      x := 45;
      y := 179;
      str := 'A full list of all the keyboard shortcuts is available in the HELP menu on the trackpad, and can be printed out.';
      do_text;

      Font.Style := [fsBold, fsItalic];
      x := 53;
      y := 186;
      str := 'Entries in bold italic are mouse actions.';
      do_text;
      str_len := Printer.Canvas.TextWidth(str + '  ');

      Font.Style := [fsItalic];
      Font.Size := 10;
      str := '(To adjust the length of plain track either F3 or F4 can be used.)';

      with Printer.Canvas do
        TextOut(Round(x / 25.4 * width_dpi) + str_len, Round(y / 25.4 * length_dpi), str);

      Font.Style := [];
      Font.Size := 11;
      x := 97;
      y := 193;
      str := 'The alternative single-key shortcuts are also shown.';
      do_text;

      Font.Size := 6;
      Font.Style := [];

      x := 0;
      y := 197;
      str := 'OPENTEMPLOT  v:' + GetVersionString(voShort) + '  ©  Martin  Wynne';
      do_text;      // © = copyright symbol.

      EndDoc;

    end;//with Canvas
  end;//with Printer

  Printer.Orientation := save_po;    // restore his printer.

end;
//______________________________________________________________________________________

procedure Tchat_form.FormCreate(Sender: TObject);

begin
  ClientWidth := 568;
  ClientHeight := 330;
  AutoScroll := True;
end;
//________________________________________________________________________________________

end.
