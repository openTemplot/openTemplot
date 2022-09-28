
(*
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  Martin Wynne.  email: martin@templot.com
    Copyright (C) 2019  OpenTemplot project contributors

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

unit help_sheet;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, Menus, Clipbrd,
  // RVScroll, RichView, RVStyle, RVFuncs,
  { OT-FIRST WPPDFPRP, WPPDFR1, WPPDFR2,} Htmlview, MetaFilePrinter, styleun,
  PrintersDlgs, HtmlGlobals;

type

  { Thelp_form }

  Thelp_form = class(TForm)
    continue_button: TButton;
    help_popup_menu: TPopupMenu;
    copy_all_popup_entry: TMenuItem;
    print_popup_entry: TMenuItem;
    stay_visible_popup_entry: TMenuItem;
    printer_margins_popup_entry: TMenuItem;
    N1: TMenuItem;
    jot_all_popup_entry: TMenuItem;
    html_view: THTMLViewer;
    copy_selected_popup_entry: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    top_panel: TPanel;
    help_top_label: TLabel;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    print_button: TButton;
    scroll_groupbox: TGroupBox;
    slow_scroll_radiobutton: TRadioButton;
    fast_scroll_radiobutton: TRadioButton;
    gentle_scroll_radiobutton: TRadioButton;
    options_button: TButton;
    goto_companion_panel: TPanel;
    forward_page_popup_entry: TMenuItem;
    N5: TMenuItem;
    go_companion_popup_entry: TMenuItem;
    scroll_slow_popup_entry: TMenuItem;
    scroll_gentle_popup_entry: TMenuItem;
    scroll_fast_popup_entry: TMenuItem;
    hide_top_controls_popup_entry: TMenuItem;
    N7: TMenuItem;
    back_page_popup_entry: TMenuItem;
    index_label: TLabel;
    N6: TMenuItem;
    print_setup_dialog: TPrinterSetupDialog;
    printer_setup_popup_entry: TMenuItem;
    page_colour_popup_entry: TMenuItem;
    more_button: TButton;
    top_line_shape: TShape;
    forward_history_button: TBitBtn;
    back_history_button: TBitBtn;
    pdf_button: TButton;
    { OT-FIRST html_pdf_printer: TWPPDFPrinter;}
    { OT-FIRST pdf_setup_dialog: TWPPDFProperties;}
    create_pdf_file_popup_entry: TMenuItem;
    N4: TMenuItem;
    scroll_label: TLabel;
    go_companion_label: TLabel;
    web_label: TLabel;
    save_file_dialog: TSaveDialog;
    escape_button: TButton;
    create_emf_file_popup_entry: TMenuItem;
    help_font_popup_entry: TMenuItem;
    help_font_bold_popup_entry: TMenuItem;
    help_font_normal_popup_entry: TMenuItem;
    N9: TMenuItem;
    size1: TMenuItem;
    help_font_13_popup_entry: TMenuItem;
    help_font_14_popup_entry: TMenuItem;
    help_font_15_popup_entry: TMenuItem;
    help_font_16_popup_entry: TMenuItem;
    help_font_17_popup_entry: TMenuItem;
    help_font_18_popup_entry: TMenuItem;
    help_font_12_popup_entry: TMenuItem;
    help_font_19_popup_entry: TMenuItem;
    help_font_20_popup_entry: TMenuItem;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure help_popup_menuPopup(Sender: TObject);
    procedure html_viewHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure print_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure slow_scroll_radiobuttonClick(Sender: TObject);
    procedure fast_scroll_radiobuttonClick(Sender: TObject);
    procedure continue_buttonClick(Sender: TObject);
    procedure copy_all_popup_entryClick(Sender: TObject);
    procedure printer_margins_popup_entryClick(Sender: TObject);
    procedure jot_all_popup_entryClick(Sender: TObject);
    procedure gentle_scroll_radiobuttonClick(Sender: TObject);
    procedure stay_visible_popup_entryClick(Sender: TObject);
    procedure options_buttonClick(Sender: TObject);
    procedure copy_selected_popup_entryClick(Sender: TObject);
    procedure hide_top_controls_popup_entryClick(Sender: TObject);
    { OT-FIRST procedure html_viewPrintHTMLHeader(Sender:TObject; HFViewer:THTMLViewer; NumPage:Integer; LastPage:Boolean; var XL,XR:Integer; var StopPrinting:Boolean);}
    procedure printer_setup_popup_entryClick(Sender: TObject);
    procedure page_colour_popup_entryClick(Sender: TObject);
    procedure more_buttonClick(Sender: TObject);
    procedure forward_history_buttonClick(Sender: TObject);
    procedure back_history_buttonClick(Sender: TObject);
    procedure pdf_buttonClick(Sender: TObject);
    procedure go_companion_labelClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure scroll_labelDblClick(Sender: TObject);
    procedure create_emf_file_popup_entryClick(Sender: TObject);
    procedure help_font_normal_popup_entryClick(Sender: TObject);
    procedure help_font_bold_popup_entryClick(Sender: TObject);
    procedure help_font_12_popup_entryClick(Sender: TObject);
    procedure help_font_13_popup_entryClick(Sender: TObject);
    procedure help_font_14_popup_entryClick(Sender: TObject);
    procedure help_font_15_popup_entryClick(Sender: TObject);
    procedure help_font_16_popup_entryClick(Sender: TObject);
    procedure help_font_17_popup_entryClick(Sender: TObject);
    procedure help_font_18_popup_entryClick(Sender: TObject);
    procedure help_font_19_popup_entryClick(Sender: TObject);
    procedure help_font_20_popup_entryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  help_form: Thelp_form;


  help_client_width_as_user: integer = 530;
  // default unless resized by user.     // 211b modified for hi-dpi
  help_client_height_as_user: integer = 680;

  no_new_help_sizes: boolean = False;          // so can prevent changing the above

  current_load_str: string = '';      // HTML string currently in viewer.  0.93.a

  num_of_print_pages: integer = 1;

  help_font_style: integer = 0;   // 0=normal  1=bold  215a
  help_font_size: integer = 17;   // 17px  215a

  html_pixels_per_inch: integer = 108;  // for HtmlView


function help(cap_code: integer; msg, yes_msg: string): integer;
function alert_help(cap_code: integer; msg, yes_msg: string): integer;

procedure do_help(cap_code: integer; full_path_or_msg: string; add_to_history: boolean);

procedure url_clicked(new_url: string; old_position, new_position: integer);  // 212a

procedure htmlviewer_hot_spot_clicked(Sender: TObject; const SRC: ThtString;
  var Handled: Boolean);

procedure view_htm(file_str: string);

procedure resize_help_form;

function convert_tagged_string_to_html(cap_code: integer; tagged_str: string;
  for_alert: boolean): string; // 0.97.d

procedure print_html_pages(viewer_code: integer);  // 205a

function parse_text_for_newline(var full_string, next_string: string): boolean;  // 205a

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, math_unit, alert_unit,
  config_unit, control_room, colour_unit, preview_unit, print_unit, calibration_unit,
  stay_visible_unit, grid_unit, jotter_unit, pad_unit, keep_select, chat_unit,
  entry_sheet, prefs_unit,
  mint_unit { OT-FIRST , file_viewer} {, docs_unit};

var
  no_onresize: boolean = False;
  printer_margins_wanted: boolean = False;

  html_colour: TColor = $00EEFFFF;     // cream.
  html_font_colour: TColor = clBlack;  // for alerts

  html_back_index: integer = 0;     // 0.91

  beginner_str: string = '';       // 0.93.a the current page in the Quick Intro

procedure help_update; forward;  // 215a

function parse_text_for_style(var full_string, next_string: string;
  var style_index: Char): boolean; forward;

//______________________________________________________________________________

function convert_tagged_string_to_html(cap_code: integer; tagged_str: string;
  for_alert: boolean): string;

var
  red, green, blue: integer;
  html_colour_str, html_font_colour_str: string;
  full_string, next_string, part_string: string;

  all_done_full, all_done_next: boolean;
  style_index: Char;
  add_str: string;

  style_str: string;

  //////////////////////////////////////////////////////////////

  function do_online_help(in_str: string): string;    // 0.93.a.

  var
    n: integer;

  begin
    repeat

      n := Pos('php/', in_str);
      if n = 0 then
        BREAK;

      Insert('.85a">more information online</A><BR><BR>', in_str, n + 7);

      in_str := StringReplace(in_str, 'php/', '<A HREF="online_ref', [rfIgnoreCase]);

    until 0 <> 0;

    Result := in_str;
  end;
  //////////////////////////////////////////////////////////////

begin
  Result := '';  // init


  if for_alert = True then
    html_colour := alert_box.Color   // match the alert background   // 0.97.d
  else
    html_colour := $00EEFFFF;        // cream for help notes

  red := (html_colour and $000000FF);
  green := (html_colour and $0000FF00) div $100;
  blue := (html_colour and $00FF0000) div $10000;

  html_colour_str := IntToHex(red, 2) + IntToHex(green, 2) + IntToHex(blue, 2);


  if for_alert = True then
    html_font_colour := alert_box.dummy_label.Font.Color   // match the alert code   // 0.97.d
  else
    html_font_colour := clBlack;                           // not used.

  red := (html_font_colour and $000000FF);
  green := (html_font_colour and $0000FF00) div $100;
  blue := (html_font_colour and $00FF0000) div $10000;

  html_font_colour_str := IntToHex(red, 2) + IntToHex(green, 2) + IntToHex(blue, 2);


  full_string := StringReplace(tagged_str, '  ', ' &nbsp;', [rfReplaceAll, rfIgnoreCase]);
  // allow multiple spaces.

  full_string := StringReplace(full_string, ' > ', ' &gt; ', [rfReplaceAll, rfIgnoreCase]);
  // space both sides to remain literal >
  full_string := StringReplace(full_string, ' < ', ' &lt; ', [rfReplaceAll, rfIgnoreCase]);
  // space both sides to remain literal <

  // 0.93.a ...

  full_string := StringReplace(full_string, 'tree.gif', '<img src="' +
    Config.GetFilePath(csfiTreeSymbol) + '">', [rfReplaceAll, rfIgnoreCase]);
  full_string := StringReplace(full_string, 'rp.gif', '<img src="' +
    Config.GetFilePath(csfiRedPointer) + '">', [rfReplaceAll, rfIgnoreCase]);
  full_string := StringReplace(full_string, 'smile.gif', '<img src="' +
    Config.GetFilePath(csfiSmile) + '">', [rfReplaceAll, rfIgnoreCase]);

  full_string := StringReplace(full_string, 'green_bullet',
    '<span style="font-weight:bold; font-family:''Verdana''; color:green;">•</span>',
    [rfReplaceAll, rfIgnoreCase]);
  full_string := StringReplace(full_string, 'blue_bullet',
    '<span style="font-weight:bold; font-family:''Verdana''; color:#0077DD;">•</span>',
    [rfReplaceAll, rfIgnoreCase]);
  full_string := StringReplace(full_string, 'small_bullet',
    '<span style="font-weight:bold; font-family:''Verdana''; color:#EE7700; font-size:11px;">•</span>',
    [rfReplaceAll, rfIgnoreCase]);  // small orange bullet for lists

  if for_alert = True then
    full_string := StringReplace(full_string, 'green_panel_begin',
      '<TABLE STYLE="BACKGROUND-COLOR:#F0FFF0; BORDER:1px SOLID #AADDAA;"><TR><TD STYLE="COLOR:#006000; PADDING:6px 10px 4px 10px;">',
      [rfReplaceAll, rfIgnoreCase])
  else
    full_string := StringReplace(full_string, 'green_panel_begin',
      '<TABLE STYLE="BACKGROUND-COLOR:#F0FFF0; BORDER:1px SOLID #AADDAA;"><TR><TD STYLE="COLOR:#006000; FONT-STYLE:italic; PADDING:6px 10px 4px 10px;">', [rfReplaceAll, rfIgnoreCase]);

  full_string := StringReplace(full_string, 'green_panel_end', '</TD></TR></TABLE>',
    [rfReplaceAll, rfIgnoreCase]);

  full_string := do_online_help(full_string);  // 0.93.a

  with html_help_list do begin
    Clear;

    if help_font_style = 0            // 215a
    then
      style_str := 'NORMAL'
    else
      style_str := 'BOLD';

    if running_under_wine = True  // 212a
    then begin
      if for_alert = True then begin
        Add('<HTML><HEAD><TITLE>alert</TITLE></HEAD><BODY BGCOLOR="#' +
          html_colour_str + '"' + ' STYLE="FONT-FAMILY:''' + arial_str +
          '''; FONT-WEIGHT:NORMAL; COLOR:#' + html_font_colour_str + '; FONT-SIZE:' + IntToStr(
          help_font_size + 1) + 'PX;">');
      end
      else begin
        if (cap_code = -2) or (cap_code = -3) or (cap_code = -4)
        then
          Add('<HTML><HEAD><TITLE>help  notes</TITLE></HEAD><BODY BGCOLOR="#' + html_colour_str +
            '"' +
            ' STYLE="FONT-WEIGHT:NORMAL; FONT-FAMILY:''Comic Sans MS''; FONT-SIZE:16PX; FONT-WEIGHT:BOLD; COLOR:#600000;">')
        // template info.

        else
          Add('<HTML><HEAD><TITLE>help  notes</TITLE></HEAD><BODY BGCOLOR="#' + html_colour_str +
            '"' + ' STYLE="FONT-FAMILY:''' + arial_str +
            '''; FONT-WEIGHT:NORMAL; COLOR:#000000; FONT-SIZE:' + IntToStr(help_font_size + 2) +
            'PX; LINE-HEIGHT:125%;">');
      end;
    end
    else begin    // Windows

      if for_alert = True then begin
        Add('<HTML><HEAD><TITLE>alert</TITLE></HEAD><BODY BGCOLOR="#' +
          html_colour_str + '"' + ' STYLE="COLOR:#' + html_font_colour_str +
          '; FONT-SIZE:' + IntToStr(help_font_size) + 'PX; FONT-WEIGHT:' + style_str + ';">');
      end
      else begin
        if (cap_code = -2) or (cap_code = -3) or (cap_code = -4)
        then
          Add('<HTML><HEAD><TITLE>help  notes</TITLE></HEAD><BODY BGCOLOR="#' + html_colour_str +
            '"' +
            ' STYLE="FONT-WEIGHT:NORMAL; FONT-FAMILY:''Comic Sans MS''; FONT-SIZE:16PX; FONT-WEIGHT:BOLD; COLOR:#600000;">')
        // template info.

        else
          Add('<HTML><HEAD><TITLE>help  notes</TITLE></HEAD><BODY BGCOLOR="#' + html_colour_str +
            '"' + ' STYLE="FONT-WEIGHT:' + style_str +
            '; COLOR:#000000; FONT-SIZE:' + IntToStr(help_font_size) + 'PX; LINE-HEIGHT:125%;">');

      end;

    end;

    repeat
      all_done_full := parse_text_for_newline(full_string, next_string);
      if next_string = ''                                                    // multiple CR.
      then
        {n:=}Add('<BR><SPAN STYLE="FONT-SIZE:10PX;">&nbsp;</SPAN>')  // init para spacing.
      else begin               // do some more text...
        add_str := '<BR>';  // to next line.
        repeat
          all_done_next := parse_text_for_style(next_string, part_string, style_index);
          // get string and required style

          case style_index of

            '1':
              if for_alert = True then
                add_str := add_str + '[&nbsp;<SPAN STYLE="COLOR:#0000FF; FONT-SIZE:' + IntToStr(
                  help_font_size) + 'PX;">' + LowerCase(part_string) + '</SPAN>&nbsp;]'   // clicks style on alerts
              else
                add_str := add_str + '[&nbsp;<SPAN STYLE="COLOR:#0000FF; FONT-FAMILY:''Arial''; FONT-SIZE:' +
                  IntToStr(help_font_size) + 'PX; FONT-WEIGHT:BOLD;">' + LowerCase(part_string) + '</SPAN>&nbsp;]';
            // clicks style on help

            '2':
              if for_alert = True then
                add_str := add_str + '<SPAN STYLE="FONT-WEIGHT:BOLD; COLOR:#0000FF; FONT-FAMILY:''Courier New''; FONT-SIZE:' +
                  IntToStr(help_font_size + 1) + 'PX;">' + UpperCase(part_string) + '</SPAN>'    // keys style on alerts
              else
                add_str := add_str + '<SPAN STYLE="FONT-WEIGHT:BOLD; COLOR:#0000FF; FONT-FAMILY:''Courier New''; FONT-SIZE:' +
                  IntToStr(help_font_size + 1) + 'PX;">' + UpperCase(part_string) + '</SPAN>';   // keys style on help

            '3':
              if for_alert = True then
                add_str := add_str + '<SPAN STYLE="FONT-WEIGHT:BOLD; COLOR:#800000; FONT-STYLE:ITALIC;">' +
                  part_string + '</SPAN>'   // emphasis style on alerts
              else
                add_str := add_str + '<SPAN STYLE="FONT-WEIGHT:BOLD; COLOR:#800000; FONT-STYLE:ITALIC;">' +
                  part_string + '</SPAN>';  // emphasis style on help

            '4':
              add_str := add_str + '<SPAN STYLE="FONT-WEIGHT:NORMAL; COLOR:#008000; FONT-FAMILY:''Comic Sans MS''; FONT-SIZE:17PX;">' + part_string + '</SPAN>';  // hints style

            '5':
              add_str := add_str + '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;' + part_string;  // indent 5 spaces.

            '6':
              add_str := add_str + '<TABLE ALIGN="CENTER" WIDTH="100%" BGCOLOR="#E8FFFF" BORDERCOLOR="#80A0FF" BORDER="1" CELLSPACING="0" CELLPADDING="10">' + '<TR><TD STYLE="FONT-WEIGHT:BOLD; COLOR:#0000A0; FONT-FAMILY:''' + arial_str + '''; FONT-SIZE:20PX; FONT-STYLE:ITALIC;">&nbsp;&nbsp;' + part_string + '</TD></TR></TABLE>';   // header style in table.

            '7':
              add_str := add_str + '<SPAN STYLE="FONT-SIZE:' + IntToStr(help_font_size - 2) + 'PX;">' +
                part_string + '</SPAN>';
            // normal small style

            '8':
              add_str := add_str + '<SPAN STYLE="COLOR:#FF0000;">' + part_string + '</SPAN>';  // red text    0.93.a

            '9':
              if for_alert = True then
                add_str := add_str + '<SPAN STYLE="font-weight:bold; font-family:''Arial''; font-size:' +
                  IntToStr(help_font_size + 2) + 'px;">' + part_string + '</SPAN>'
              // header for alerts
              else
                add_str := add_str + '<SPAN STYLE="color:#C05000; font-family:''Trebuchet MS''; font-size:' +
                  IntToStr(help_font_size + 5) + 'px; font-style:italic; font-weight:bold;">' + part_string + '</SPAN>';
            // header

            'f':
              add_str := add_str + '<SPAN STYLE="COLOR:#000000; FONT-WEIGHT:NORMAL; FONT-STYLE:NORMAL; FONT-FAMILY:''Courier New''; FONT-SIZE:' + IntToStr(help_font_size) + 'PX;">' + part_string + '</SPAN>';   // file paths (could be very long non-wrapping)

            'w':
              add_str := add_str + '<SPAN STYLE="COLOR:#0000FF; FONT-FAMILY:''Wingdings 2''; FONT-SIZE:' +
                IntToStr(help_font_size + 1) + 'PX;">' + part_string + '</SPAN>';  // symbols from wingdings 2

            'z':
              if for_alert = True then
                add_str := add_str + '[&nbsp;<SPAN STYLE="COLOR:#0000FF; FONT-SIZE:' + IntToStr(
                  help_font_size) + 'PX;">' + part_string + '</SPAN>&nbsp;]'
              // clicks style on alerts (no case change)
              else
                add_str := add_str + '[&nbsp;<SPAN STYLE="COLOR:#0000FF; FONT-FAMILY:''Arial''; FONT-SIZE:' +
                  IntToStr(help_font_size) + 'PX; FONT-WEIGHT:BOLD;">' + part_string + '</SPAN>&nbsp;]';
            // clicks style on help (no case change)

            'x':
              add_str := add_str + '<SPAN STYLE="font-weight:bold; color:#FF0000; font-family:''Verdana''; font-size:'
                +
                IntToStr(help_font_size + 3) + 'px; font-style:italic;">' + part_string + '</SPAN>';
            // red header    0.97.a

            'm':
              add_str := add_str + '<SPAN STYLE="font-weight:bold; color:#800000; font-size:' +
                IntToStr(help_font_size + 6) + 'px; font-style:italic;">' + part_string + '</SPAN>';
            // maroon header   214a

            'n':
              add_str := add_str + '<SPAN STYLE="font-weight:normal; font-style:italic;">' + part_string + '</SPAN>';
              // for examples   214a

            else
              add_str := add_str + part_string;    // add text item as is

          end;//case

        until all_done_next = True;

        {n:=}Add(add_str);
      end;

    until all_done_full = True;

    Add('</BODY></HTML>');

  end;//with StringList

  Result := title_swap(html_help_list.Text);    // OT-FIRST

  //RESULT:=html_help_list.Text;
end;
//______________________________________________________________________________

procedure do_help(cap_code: integer; full_path_or_msg: string; add_to_history: boolean);

//  show help information.  cap_code selects the form captions.
//  if cap_code 0 or negative down to -99, full_path_or_msg is a tagged string for the viewer.
//  cap_code= -100 means empty slot in history.
//  if cap_code is less than -100 down to -400, full_path_or_msg is plain HTML code, add BODY headers etc.
//  if cap_code is less than -400, full_path_or_msg is HTML full page, display as-is.
//  if cap_code>0, full_path_or_msg is a file name to load into the viewer (html files + optional # target).

var

  html_src_str: string;

  header_str: string;
  footer_str: string;

  file_content_list: TStringList;

  needed_viewer_height, bottom_margin: integer;  // 214a

begin

  header_str := '<HTML><HEAD><TITLE>Templot Information</TITLE><STYLE>'
    + 'hr {color:#AAAAAA; margin-left:12px; margin-right:12px;}'
    + 'body {font-size:18px; font-family:''' + arial_str +
    '''; line-height:133%;}' + 'p {margin:12px 6px 0px 12px;}'
    + '.clicks {color:#0000FF;}' +
    '.firm {font-style:italic; font-weight:bold; color:#800000;}' +
    '.heading {font-weight:bold; color:#0077DD; font-size:21px; font-style:italic;}'
    +
    '.mainheading {font-weight:bold; color:#EE7700; font-size:23px; font-style:italic;}'
    +
    '.subheading {font-style:italic; font-weight:bold; color:#0077DD; font-size:20px;}'
    +
    '.keys {font-family:''Courier New''; font-weight:bold; color:#0000FF;}'
    + '.notes {font-size:12px; color:#777777;}' +
    '.version {text-align:right; font-size:13px; color:#777777;}' +
    '.alert {font-size:15px; color:#FF0000; font-weight:bold;}' +
    '.centerbold {text-align:center; font-size:16px; font-weight:bold;}'
    + '.center {text-align:center;}' +
    '.spacer {font-size:2px;}' + '</STYLE></HEAD><BODY>';


  footer_str := '<HR NOSHADE><P CLASS="version">v: ' + GetVersionString(voFull) +
    '&nbsp; &copy; &nbsp;</P></BODY></HTML>';

  with help_form do begin

    case cap_code of

      //------------------------------------------------------

      // -401... full page source, no headers needed...

      -500:
        Caption := '    upgrade  information';          // 0.96.a

      //------------------------------------------------------

      // -101..-400 add styles header and footer only...

      -350:
        Caption := '    storage  box  information';     // 0.96.a

      -300:
        Caption := '    program  information';          // 0.93.a

      -250:
        Caption := '    about  ' + Application.Title + '  v: ' + GetVersionString(voFull);
      -200:
        Caption := '    saved  program  preferences';
      -150:
        Caption := '    welcome  to  ' + Application.Title;

      //-----------------------------------------------------

      // 0..-100  tagged strings

      -100:
        Caption := '    empty  page  slot';
      -8:
        Caption := '    prefix  tags';          // 208e
      -7:
        Caption := '    read  me';              // 208d
      -6:
        Caption := '    what''s  this ?';
      -5:
        Caption := '    information';                              // !!! startup, no valid lock-release.
      -4:
        Caption := '    switch  information  and  dimensions';
      -3, -2:
        Caption := '    template  information';
      -1:
        Caption := '    what  next ?';               // main form captions.
      0:
        Caption := '    help  information';

        //------------------------------------------------------

        // +1...  file to load, not a string...

      else
        Caption := '    ' + Application.Title + '  information';          // files.

    end;//case

    if cap_code > 0 then begin
      // html file name (possibly including a # target, so don't use ExtractFileExt, etc.)...

      html_view.LoadFromFile(full_path_or_msg);
      // load as file (so that relative links work).

      file_content_list := TStringList.Create;
      file_content_list.LoadFromFile(full_path_or_msg);
      // but also get the file as HTML text (for copy) 0.93.a
      current_load_str := file_content_list.Text;
      file_content_list.Free;

      if add_to_history = True        // loaded, put in history.
      then begin
        if html_back[html_back_index].src_code <> -100
        // overwrite current slot if it is empty.
        then begin
          Inc(html_back_index);
          // foward to next slot.
          if html_back_index > html_back_c then
            html_back_index := 0;
        end;

        index_label.Caption := Chr(html_back_index + 97);  // show current index a-j.

        with html_back[html_back_index] do begin
          src_code := cap_code;
          src_str := full_path_or_msg;
          src_position := html_view.Position;
          // may not be top if msg included # target.
        end;//with
      end;
    end
    else begin

      if cap_code < -100     // plain HTML code..
      then begin
        if cap_code < -400 then
          html_src_str := full_path_or_msg                           // no headers needed
        else
          html_src_str := header_str + full_path_or_msg + footer_str;    // add style headers
        current_load_str := html_src_str;
        html_view.LoadFromString(current_load_str);
      end
      else begin       // show tagged string...

        current_load_str :=
          convert_tagged_string_to_html(cap_code, full_path_or_msg, False);
        // 0.97.d  extracted to separate routine

        html_view.LoadFromString(current_load_str);
        // plain file - no reference path for images or links.

      end;// tagged string

      if add_to_history = True then begin
        if html_back[html_back_index].src_code <> -100
        // overwrite current slot if it is empty.
        then begin
          Inc(html_back_index);
          // foward to next slot.
          if html_back_index > html_back_c then
            html_back_index := 0;
        end;

        index_label.Caption := Chr(html_back_index + 97);  // show current index a-j.

        with html_back[html_back_index] do begin
          src_code := cap_code;

          if cap_code < -100                         // plain HTML code..  //%%%%
          then
            src_str := full_path_or_msg        // header and footer not included (will be inserted when displayed).
          else
            src_str := current_load_str;       // 0.97.a html_help_list.Text;

          src_position := 0;
          // always starts at top of tagged string page.
        end;//with
      end;
    end;//not file

    // 214a  avoid scrollbar if possible by increasing form height.

    if no_new_help_sizes = False then begin

      if Screen.PixelsPerInch > 120 then
        bottom_margin := 84
      else
        bottom_margin := 64;

      needed_viewer_height := html_view.FullDisplaySize(html_view.Width).cy;
      // make sure no vertical scrollbar needed
      ClientHeight := html_view.Top + needed_viewer_height + bottom_margin;       // force resize

      if Height > (Screen.Height - 120)
      // resulting form height, 120 arbitrary for convenience, 20 top default, 100 bottom
      then begin
        Top := 20;
        Height := Screen.Height - 120;   // Scrollbar appears if necessary
      end;
    end;

  end;//with

end;
//______________________________________________________________________________

function help(cap_code: integer; msg, yes_msg: string): integer;

var
  i, dummy: integer;  // keep compiler happy

  //////////////////////////////////////////////////////////////
  procedure show_help_form;

  begin
    do_help(cap_code, msg, (cap_code <> -100));
    // show the captions and viewer content. Add to history if code not -100.

    with help_form do begin

      if yes_msg = '' then begin
        more_button.Visible := False;   // 0.93.a was Hide;
      end
      else begin
        // show modal if there is a yes message, so we can return which button was clicked.
        more_button.Caption := title_swap(yes_msg);
        more_button.Visible := True;    // 0.93.a was Show;
      end;

      continue_button.Caption := '&continue';

      do_show_modal(help_form);
      // 212a   ShowModal  // wait for user to respond.

      if ModalResult = mrYes then
        Result := 1
      else
        Result := 0;
      //end;
    end;//with
  end;
  //////////////////////////////////////////////////////////////

begin

  if msg = ''           // ???
  then begin
    msg := 'error - no text  ' + IntToStr(cap_code);
    cap_code := 0;
  end;

  // update history for current page position before changing to new page..

  html_back[html_back_index].src_position := help_form.html_view.Position;

  if cap_code > 0 then
    msg := html_path_str + msg   // make full file path

  else begin
    if cap_code > -100 then
      msg := msg + '<SPAN STYLE="FONT-WEIGHT:NORMAL; FONT-SIZE:13px;">|____________| v: ' +
        GetVersionString(voFull) + ' &nbsp; ©</SPAN>';  // add footer
  end;

  show_help_form;    // now show it.

  if printer_margins_wanted = True      // modal form was closed to get print margins.
  then begin
    printer_margins_wanted := False;
    repeat
      set_printer_font_margins(help_form, False);
      show_help_form;                              // show it again, and again
    until printer_margins_wanted = False;
  end;

end;
//________________________________________________________________________________________

function alert_help(cap_code: integer; msg, yes_msg: string): integer;

  // same as help function above, but shows the alert box non-modal at the same time,
  // and the Help form shows modal instead.

begin
  if Application.Terminated = False then
    Application.ProcessMessages;   // for alert_box to close (was showing modal).

  alert_box.Show;
  alert_box.BringToFront;

  if cap_code = -2 then
    cap_code := 0;      // don't allow non-modal.

  Result := help(cap_code, msg, yes_msg);   // show the help.

  alert_box.Close;                      // this is essential so can show modal again.

  if Application.Terminated = False then
    Application.ProcessMessages;

end;
//_____________________________________________________________

procedure Thelp_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.

  if (Key = $43) and (Shift = [ssCtrl])   // Ctrl-C
  then begin
    Key := 0;
    html_view.CopyToClipboard;
  end;
end;
//___________________________________________________________________________________

procedure Thelp_form.FormActivate(Sender: TObject);

begin
  printer_margins_wanted := False;

  //  if (yes_flag=True) and (yes_button.Showing=True) then yes_button.SetFocus       // need the flag to keep Delphi happy.
  //                                                   else continue_button.SetFocus;

  // removed to allow focus on html_view (so that arrow keys work immediately). 0.81

  if html_view.Visible = True then
    html_view.SetFocus;
end;
//______________________________________________________________________________

procedure Thelp_form.help_popup_menuPopup(Sender: TObject);

begin
  if help_font_style = 0 then
    help_font_normal_popup_entry.Checked := True   // radio items
  else
    help_font_bold_popup_entry.Checked := True;

  case help_font_size of

    12:
      help_font_12_popup_entry.Checked := True;  // radio items
    13:
      help_font_13_popup_entry.Checked := True;  // radio items
    14:
      help_font_14_popup_entry.Checked := True;  // radio items
    15:
      help_font_15_popup_entry.Checked := True;  // radio items
    16:
      help_font_16_popup_entry.Checked := True;  // radio items
    17:
      help_font_17_popup_entry.Checked := True;  // radio items
    18:
      help_font_18_popup_entry.Checked := True;  // radio items
    19:
      help_font_19_popup_entry.Checked := True;  // radio items
    20:
      help_font_20_popup_entry.Checked := True;  // radio items

  end;//case
end;

procedure Thelp_form.html_viewHotSpotClick(Sender: TObject; const SRC: ThtString;
  var Handled: Boolean);
begin
  htmlviewer_hot_spot_clicked(Sender, SRC, Handled);
end;

//______________________________________________________________________________________

procedure Thelp_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  no_onresize := True;                            // don't permit on-resize until finished.

  if size_updown.Position > size_updown.Tag       // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                        // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                        // scale the form contents up.

  size_updown.Tag := size_updown.Position;        // and save for the next click.

  no_onresize := False;                           // can now resize the contents again if he wants to.
  resize_help_form;                             // do it anyway to normalize the contents.
end;
//_________________________________________________________________________________________

procedure Thelp_form.FormShow(Sender: TObject);

begin
  html_view.Position := 0;     // return scrollbar to top
end;
//___________________________________________________________________________________________

procedure resize_help_form;    // 0.91.d     //%%%%

begin

  // for html viewer 0.81 ...

  with help_form do begin

    if no_onresize = False then begin

      top_panel.Left := 0;
      top_panel.Top := 0;
      top_panel.Width := ClientWidth;

      html_view.Left := 0;

      if top_panel.Visible = True then
        html_view.Top := top_panel.Height + 2
      else
        html_view.Top := 0;

      html_view.Width := ClientWidth;

      continue_button.Top := ClientHeight - continue_button.Height - 6;
      more_button.Top := continue_button.Top;          //+2;
      escape_button.Top := continue_button.Top + 4;

      html_view.Height := continue_button.Top - html_view.Top - scroll_label.Height;

      continue_button.Left := ClientWidth - continue_button.Width - 6;
      more_button.Width := continue_button.Left - more_button.Left - 12;

      scroll_label.Width := ClientWidth;
      scroll_label.Top := html_view.Top + html_view.Height + 6;

      top_line_shape.Left := 0;

      if top_panel.Visible = True then
        top_line_shape.Top := top_panel.Height
      else
        top_line_shape.Top := 0;

      top_line_shape.Height := 1;
      top_line_shape.Width := ClientWidth;

      go_companion_label.Left := web_label.Left + web_label.Width + 2;


      scroll_label.Width := ClientWidth;

    end;
  end;//with

end;
//______________________________________________________________________________

procedure Thelp_form.FormResize(Sender: TObject);   // 0.91.d

begin
  if no_new_help_sizes = False then begin
    help_client_width_as_user := ClientWidth;  // dragged by user, modify default.
    help_client_height_as_user := ClientHeight;
  end;
  resize_help_form;
end;
//______________________________________________________________________________

procedure Thelp_form.FormCreate(Sender: TObject);

begin
  AutoScroll := False;

  ClientWidth := help_client_width_as_user;   // see also FormHide 0.93.a
  ClientHeight := help_client_height_as_user;

  { OT-FIRST scroll_groupbox.Ctl3D:=True;}

  html_view.VScrollBar.SmallChange := 4;     // gentle scrolling by default.

  resize_help_form;
end;
//______________________________________________________________________________

procedure Thelp_form.stay_visible_popup_entryClick(Sender: TObject);

begin
  if stay_visible_form.Left < (Left + Width + 4) then begin
    Left := 4;
    stay_visible_form.Left := Width + 8;
    // make sure it doesn't obscure us, otherwise can't continue (showing modal).
  end;

  html_view.SelectAll;

  stay_visible_form.text_label.Caption := title_swap(html_view.SelText);
  stay_visible_form.help_scrollbox.VertScrollBar.Position := 0;   // reset the scrollbar.
  stay_visible_form.Show;

end;
//______________________________________________________________________________

procedure Thelp_form.slow_scroll_radiobuttonClick(Sender: TObject);

begin
  html_view.VScrollBar.SmallChange := 1;
  if html_view.Visible = True then
    html_view.SetFocus;  // so that the arrow keys work.

  scroll_slow_popup_entry.Checked := True;  // radio item
  slow_scroll_radiobutton.Checked := True;  // in case "clicked" via popup menu.
end;
//______________________________________________________________________________

procedure Thelp_form.gentle_scroll_radiobuttonClick(Sender: TObject);

begin
  html_view.VScrollBar.SmallChange := 4;
  if html_view.Visible = True then
    html_view.SetFocus;  // so that the arrow keys work.

  scroll_gentle_popup_entry.Checked := True;  // radio item
  gentle_scroll_radiobutton.Checked := True;
end;
//______________________________________________________________________________

procedure Thelp_form.fast_scroll_radiobuttonClick(Sender: TObject);

begin
  html_view.VScrollBar.SmallChange := 32;  // normal ThtmlViewer setting is 16.

  if html_view.Visible = True then
    html_view.SetFocus;  // so that the arrow keys work.

  scroll_fast_popup_entry.Checked := True;  // radio item
  fast_scroll_radiobutton.Checked := True;

end;
//______________________________________________________________________________

procedure Thelp_form.continue_buttonClick(Sender: TObject);

begin
  Close;
end;
//______________________________________________________________________________

procedure Thelp_form.copy_selected_popup_entryClick(Sender: TObject);

begin
  html_view.CopyToClipboard;
end;
//______________________________________________________________________________

procedure Thelp_form.copy_all_popup_entryClick(Sender: TObject);

begin
  html_view.SelectAll;
  html_view.CopyToClipboard;
end;
//____________________________________________________________________________________

procedure Thelp_form.printer_margins_popup_entryClick(Sender: TObject);

begin

  printer_margins_wanted := True;
  ModalResult := mrOk;
  Close;

end;
//_____________________________________________________________________________________

procedure Thelp_form.jot_all_popup_entryClick(Sender: TObject);

begin
  html_view.SelectAll;
  html_view.CopyToClipboard;
  jotter_form.jotter_memo.PasteFromClipboard;
end;
//____________________________________________________________________________________

procedure view_htm(file_str: string);

begin
  help(1, file_str, '');   // ignore return
end;
//______________________________________________________________________________

function parse_text_for_newline(var full_string, next_string: string): boolean;

  // input is full string, return next part up to but not including CR.
  // return also remainder of full string minus CR.
  // CR represented by '|'.
  // RESULT is True when all done (no more text).

var
  i: integer;

begin
  Result := True;  // default init.

  i := Pos('|', full_string);
  if i > 0 then begin
    next_string := Copy(full_string, 1, i - 1);
    Delete(full_string, 1, i);               // remove the '|' also.
    Result := False;
  end
  else begin
    next_string := full_string;
    Result := True;
  end;
end;
//______________________________________________________________________________

function parse_text_for_style(var full_string, next_string: string; var style_index: Char): boolean;

  // input is full string (cannot include CRs), return next part up to but not including
  // delimiter and code.
  // return also remainder of full string minus delimiter and code.
  // delimiter is '`'. code is following single character. return code as style_index (Char).
  // if no delimiter, return whole string and style_index='0'
  // RESULT is True when all done (no more text).

  // NB. delimiter and style is at END of relevant text block.

var
  i: integer;
  s: string[1];

begin
  Result := True;  // default init.

  i := Pos('`', full_string);
  if i > 0 then begin
    next_string := Copy(full_string, 1, i - 1);
    s := Copy(full_string, i + 1, 1);
    style_index := s[1];                     // index is Char
    Delete(full_string, 1, i + 1);             // remove the '|' also.
    Result := False;
  end
  else begin
    next_string := full_string;
    style_index := '0';
    Result := True;
  end;
end;
//______________________________________________________________________________

procedure print_html_pages(viewer_code: integer);

// viewer_code : 0=help viewer,  2=storage box lists

var
  all_done: boolean;

begin
  if no_printer_available = True     // 0.93.a
  then begin
    show_modal_message('No printer available.');
    EXIT;
  end;

  repeat  // in case he changes the paper size -- need to recalculate number of pages

    all_done := False;       // init
    num_of_print_pages := 0; // init

    if viewer_code = 0 then
      num_of_print_pages := help_form.html_view.NumPrinterPages;                    // for the header
    if viewer_code = 2 then
      num_of_print_pages := keep_form.keep_html_view.NumPrinterPages;

    if num_of_print_pages = 0 then
      EXIT; // ??? nothing to print?

    with TPrintDialog.Create(nil) do begin
      // 0.93.a created in code because of startup error if no printer available.
      try

        Options := [poPageNums];

        FromPage := 1;
        MinPage := 1;
        ToPage := num_of_print_pages;
        MaxPage := num_of_print_pages;

        PrintRange := prAllPages;    // default all pages.

        showing_dialog := True;   // 212a Wine bug

        if Execute = True then begin

          showing_dialog := False;   // 212a Wine bug

          if viewer_code = 0 then begin

            if num_of_print_pages = help_form.html_view.NumPrinterPages
            // he hasn't changed paper size
            then begin
              // EMF (Kinda) THTMLViewer only has a print procedure under Windows
              //if PrintRange = prAllPages
              //then
              //  help_form.html_view.Print(1, 9999)
              //else
              //  help_form.html_view.Print(FromPage, ToPage);
              all_done := True;
            end;
          end;


          if viewer_code = 2 then begin

            if num_of_print_pages = keep_form.keep_html_view.NumPrinterPages
            // he hasn't changed paper size
            then begin
              // EMF (Kinda) THTMLViewer only has a print procedure under Windows
              //if PrintRange = prAllPages
              //then
              //  keep_form.keep_html_view.Print(1, 9999)
              //else
              //  keep_form.keep_html_view.Print(FromPage, ToPage);
              all_done := True;
            end;
          end;

        end //if execute
        else
          all_done := True;   // he cancelled

        showing_dialog := False;   // 212a Wine bug
      finally
        Free;
      end;//try

    end;//with

  until all_done = True;
end;
//______________________________________________________________________________

procedure Thelp_form.print_buttonClick(Sender: TObject);

begin
  print_html_pages(0);
end;
//______________________________________________________________________________

procedure Thelp_form.printer_setup_popup_entryClick(Sender: TObject);

begin
  showing_dialog := True;   // 212a Wine bug

  print_setup_dialog.Execute;

  showing_dialog := False;   // 212a Wine bug
end;
//______________________________________________________________________________

function create_graphics(graphic_class: TGraphicClass): TGraphic;

  // Delphi bug fix - see RichView Help for 3rd party graphics...

begin
  if graphic_class = TGIFImage then
    Result := TGIFImage.Create
  else
    Result := graphic_class.Create;
end;
//______________________________________________________________________________

procedure Thelp_form.options_buttonClick(Sender: TObject);

var
  pop_loc: TPoint;

begin
  pop_loc.X := html_view.Left + 50;
  pop_loc.Y := html_view.Top + 50;
  help_popup_menu.Popup(ClientToScreen(pop_loc).X, ClientToScreen(pop_loc).Y);
end;
//______________________________________________________________________________

procedure Thelp_form.hide_top_controls_popup_entryClick(Sender: TObject);

begin
  if top_panel.Visible = True then begin
    top_panel.Visible := False;
    hide_top_controls_popup_entry.Caption := 'show  top  controls';
  end
  else begin
    top_panel.Visible := True;
    hide_top_controls_popup_entry.Caption := 'hide  top  controls';
  end;

  resize_help_form;

  if (html_view.Visible = True) and (help_form.Showing = True) then
    html_view.SetFocus;
end;
//______________________________________________________________________________

procedure online_help_click(ref_number_str: string);    // 0.93.a

var
  url_str: string;

begin
  url_str := 'http://templot.com/companion/info.php?ref=' + ref_number_str;

  if not OpenURL(url_str) then
    show_modal_message('Sorry, unable to open your browser window and connect to the Templot web site.'
      + #13 + #13 + 'Please check your internet connection.');

end;
//______________________________________________________________________________

procedure web_link_clicked(new_url: string); // 212a

begin
  go_to_url(new_url);      // 212a
end;
//______________________________________________________________________________

procedure program_link_clicked(new_url: string);

// procedure called from Htmlview unit on a clicked link
// this is a click for a program function.

var
  n, i: integer;
  video_name_str, video_list_path_str, video_str: string;
  fbr_list: TStringList;

begin

  // first test for online help. link = online_ref123.85a  ..

  if Copy(new_url, 1, 10) = 'online_ref' then begin
    online_help_click(Copy(new_url, 11, 3));
    EXIT;
  end;  // 0.93.a

  if new_url = 'go_to_templot_explained.85a' then begin
    go_to_templot_companion_page('templot_explained.php');
    EXIT;
  end;   // 215a

  if new_url = 'go_to_video_list.85a' then begin
    go_to_video_list;
    EXIT;
  end;   // 211b

  if new_url = 'go_to_templot_com.85a' then begin
    go_to_templot_com;
    EXIT;
  end;  // 0.93.a
  if new_url = 'go_to_templot_club.85a' then begin
    go_to_templot_club;
    EXIT;
  end;  // 0.93.a
  if new_url = 'go_to_templot_companion.85a' then begin
    go_to_templot_companion;
    EXIT;
  end;
  if new_url = 'go_to_donation.85a' then begin
    go_to_donation;
    EXIT;
  end;  // 0.96.a
  if new_url = 'go_to_upgrade.85a' then begin
    go_to_upgrade;
    EXIT;
  end;  // 0.96.a

  if new_url = 'init_prefs.85a' then begin
    save_prefs('', False);
    EXIT;
  end;
  if new_url = 'change_prefs_file_and_load.85a' then begin
    change_prefs_file_and_load(True);
    EXIT;
  end;
  if new_url = 'new_prefs_file_and_save.85a' then begin
    new_prefs_file_and_save;
    EXIT;
  end;
  if new_url = 'load_prefs.85a' then begin
    load_prefs('', True, True, False);
    EXIT;
  end;
  if new_url = 'save_prefs.85a' then begin
    save_prefs('', True);
    EXIT;
  end;
  if new_url = 'change_prefs_file_only.85a' then begin
    change_prefs_file_and_load(False);
    EXIT;
  end;
  if new_url = 'abandon_prefs.85a' then begin
    abandon_prefs(True);
    EXIT;
  end;

  if new_url = 'prefs_include_gen.85a' then begin
    prefs_include_gen;
    EXIT;
  end;
  if new_url = 'prefs_exclude_gen.85a' then begin
    prefs_exclude_gen;
    EXIT;
  end;

  if new_url = 'show_prefs_dialog.85a' then begin
    show_prefs_dialog(True);
    EXIT;
  end;

  if new_url = 'beginner_bright_night.85a' then begin
    pad_form.bright_night_scheme_menu_entry.Click;
    EXIT;
  end; // 0.93.a
  if new_url = 'beginner_sky_blue.85a' then begin
    pad_form.startup_colours_menu_entry.Click;
    EXIT;
  end; // 0.93.a

  if new_url = 'switch_options.85a' then begin
    mint_form.ModalResult := 100;
    EXIT;
  end;    // 0.93.a
  if new_url = 'crossing_options.85a' then begin
    mint_form.ModalResult := 101;
    EXIT;
  end;    // 0.93.a
  if new_url = 'curving_radius.85a' then begin
    mint_form.ModalResult := 102;
    EXIT;
  end;    // 0.93.a

  if new_url = 'alert_1.85a' then begin
    clicked_i := 1;
    alert_box.Close;
    EXIT;
  end;    // 0.97.d
  if new_url = 'alert_2.85a' then begin
    clicked_i := 2;
    alert_box.Close;
    EXIT;
  end;    // 0.97.d
  if new_url = 'alert_3.85a' then begin
    clicked_i := 3;
    alert_box.Close;
    EXIT;
  end;    // 0.97.d
  if new_url = 'alert_4.85a' then begin
    clicked_i := 4;
    alert_box.Close;
    EXIT;
  end;    // 0.97.d
  if new_url = 'alert_5.85a' then begin
    clicked_i := 5;
    alert_box.Close;
    EXIT;
  end;    // 0.97.d
  if new_url = 'alert_6.85a' then begin
    clicked_i := 6;
    alert_box.Close;
    EXIT;
  end;    // 0.97.d

  if new_url = 'alert_online.85a' then begin
    alert_box.online_help_button.Click;
    EXIT;
  end;    // 0.97.d

  if new_url = 'smallest_bgnd_rad.85a' then begin
    box_goto_smallest_bgnd_rad;
    EXIT;
  end;    // 208a
  if new_url = 'smallest_group_rad.85a' then begin
    box_goto_smallest_group_rad;
    EXIT;
  end;    // 208a

end;

procedure htmlviewer_hot_spot_clicked(Sender: TObject; const SRC: ThtString;
  var Handled: Boolean);
var
  S, Dest: ThtString;
  OldPos: Integer;
  ft: THtmlFileType;

  url_str: string;     // 85A
  url_ext: string;     // 85A

begin

  url_str := SRC;
  Handled := False;

  if (Pos('http://', url_str) = 1) or (Pos('https://', url_str) = 1)    //  https 212b
  then begin
    web_link_clicked(url_str); // 212a  // 85A in help_sheet unit
    Handled := True;
    EXIT;
  end;

  url_ext := Uppercase(ExtractFileExt(url_str));

  if url_ext = '.85A' then begin
    program_link_clicked(url_str); // 212a  // 85A in help_sheet unit
    Handled := True;
    EXIT;
  end;
end;

//______________________________________________________________________________

procedure url_clicked(new_url: string; old_position, new_position: integer);

// procedure called from Htmlview unit on a clicked link (after loading new url if HTML or image).

begin
  html_back[html_back_index].src_position := old_position;   // update existing file position.

  Inc(html_back_index);                                    // foward to next slot.
  if html_back_index > html_back_c then
    html_back_index := 0;
  help_form.index_label.Caption := Chr(html_back_index + 97);  // show current index a-j.

  with html_back[html_back_index] do begin
    src_code := 1;    // file loaded.
    src_str := new_url;
    src_position := new_position;
  end;//with

  help_form.more_button.Hide;  // more button return no longer matched to content.
end;
//______________________________________________________________________________

procedure Thelp_form.go_companion_labelClick(Sender: TObject);

begin
  go_to_templot_companion;
end;
//______________________________________________________________________________

{ OT-FIRST
procedure Thelp_form.html_viewPrintHTMLHeader(Sender:TObject; HFViewer:THTMLViewer; NumPage:Integer; LastPage:Boolean;
                                              var XL,XR:Integer; var StopPrinting:Boolean);

  // header for print output...

begin
  with html_printheader_list do begin
    Clear;
    Add('<HTML><BODY BGCOLOR="#FFFFFF">');
    Add('<SPAN STYLE="FONT-WEIGHT:NORMAL; COLOR:#000000; FONT-FAMILY:'''+arial_str+''';">');
    Add('<TABLE WIDTH="100%" COLS="3" ROWS="1"><TR>');
    Add('<TD ALIGN=LEFT><SPAN STYLE="FONT-SIZE:11PX;">printed from Templot0 v: '+FormatFloat('0.00',program_version/100)+version_build+' &nbsp;on  &nbsp;'+DateToStr(Date)+' &nbsp;at &nbsp;'+TimeToStr(Time)+'</SPAN></TD>');
    Add('<TD ALIGN=CENTER><SPAN STYLE="FONT-SIZE:14PX;">'+html_view.DocumentTitle+'</SPAN></TD>');
    Add('<TD WIDTH="25%" ALIGN=RIGHT><SPAN STYLE="FONT-SIZE:14PX;">page&nbsp;&nbsp;&nbsp;<SPAN STYLE="FONT-SIZE:18PX;">'+IntToStr(NumPage)+'</SPAN>&nbsp;&nbsp;&nbsp;of&nbsp;&nbsp;'+IntToStr(num_of_print_pages)+'</SPAN></TD>');
    add('</TR></TABLE><HR NOSHADE COLOR=BLACK SIZE=1>');
    Add('</SPAN></BODY></HTML>');
  end;//with

  HFViewer.LoadStrings(html_printheader_list,''); // plain file - no reference path for images or links.
end;
//______________________________________________________________________________
}

procedure Thelp_form.page_colour_popup_entryClick(Sender: TObject);

var
  current_pos: integer;

begin
  html_colour := get_colour('choose  a  new  colour  for  the  page  background', html_colour);

  current_pos := html_view.Position;
  with html_back[html_back_index] do
    do_help(src_code, src_str, False);     // show the file or text (already in history).
  html_view.Position := current_pos;
  // and re-position it as near as poss.
end;
//______________________________________________________________________________

procedure Thelp_form.more_buttonClick(Sender: TObject);

begin
  ModalResult := mrYes;
end;
//______________________________________________________________________________

procedure Thelp_form.forward_history_buttonClick(Sender: TObject);

begin
  html_back[html_back_index].src_position := html_view.Position;
  // update history for current page position.

  Inc(html_back_index);                                     // foward to next slot.
  if html_back_index > html_back_c then
    html_back_index := 0;
  index_label.Caption := Chr(html_back_index + 97);  // show current index a-j.

  with html_back[html_back_index] do begin
    do_help(src_code, src_str, False);     // show the file or text (already in history).
    html_view.Position := src_position;    // and position it.
  end;//with

  more_button.Hide;  // more button return no longer matched to content.

  if html_view.Visible = True then
    html_view.SetFocus;  // so that the arrow keys work.

end;
//______________________________________

procedure Thelp_form.back_history_buttonClick(Sender: TObject);

begin
  html_back[html_back_index].src_position := html_view.Position;
  // update history for current page position.

  Dec(html_back_index);        // roll back to previous slot.
  if html_back_index < 0 then
    html_back_index := html_back_c;
  index_label.Caption := Chr(html_back_index + 97);  // show current index a-j.

  with html_back[html_back_index] do begin
    do_help(src_code, src_str, False);     // show the file or text (already in history).
    html_view.Position := src_position;    // and position it.
  end;//with

  more_button.Hide;  // more button return no longer matched to content.

  if html_view.Visible = True then
    html_view.SetFocus;  // so that the arrow keys work.
end;
//______________________________________________________________________________

procedure Thelp_form.pdf_buttonClick(Sender: TObject);

var
  { OT-FIRST metafile_printer:TMetafilePrinter;}
  page: integer;
  folder_str: string;

begin
  do_open_source_bang('PDF TEXT');  // OT-FIRST

  { OT-FIRST
  num_of_print_pages:=html_view.NumPrinterPages;   // for print header

  with save_file_dialog do begin
    InitialDir:=Config.GetDir(cudiPdfs);
    FileName:=remove_invalid_str(Copy(Trim(help_form.Caption),1,24)+FormatDateTime('_yyyy_mm_dd_hhmm_ss',Date+Time))+'.pdf';
    Title:='    save  PDF  file  as ...';

    showing_dialog:=True;   // 212a Wine bug

    if Execute=False
       then begin
              showing_dialog:=False;   // 212a Wine bug
              EXIT;
            end;

    showing_dialog:=False;   // 212a Wine bug

          // invalid entered chars removed by dialog

    html_pdf_printer.FileName:=ExtractFilePath(FileName)+lower_case_filename(ExtractFileName(FileName));   // to underscores and lower case

  end;//with

  showing_dialog:=True;   // 212a Wine bug

  if pdf_setup_dialog.Execute=False
     then begin
            showing_dialog:=False;   // 212a Wine bug
            EXIT;
          end;

  showing_dialog:=False;   // 212a Wine bug

  metafile_printer:=TMetafilePrinter.Create(Self);

  try
    html_view.PrintPreview(metafile_printer);

    html_pdf_printer.BeginDoc;
    try
      for page:=0 to metafile_printer.LastAvailablePage-1 do begin

        html_pdf_printer.StartPage(metafile_printer.PageWidth,metafile_printer.PageHeight,metafile_printer.PixelsPerInchX,metafile_printer.PixelsPerInchY,0);
        html_pdf_printer.Canvas.Draw(0,0,metafile_printer.MetaFiles[page]);
        html_pdf_printer.EndPage;

      end;//for
    finally
      html_pdf_printer.EndDoc;
    end;
  finally

    if alert(2,'    PDF  file  created',
               ' |PDF file created ( '+IntToStr(num_of_print_pages)+' pages ):'
              +'||'+ExtractFileName(html_pdf_printer.Filename)
              +'||  page width: '+IntToStr(metafile_printer.PageWidth)+' dots at '+IntToStr(metafile_printer.PixelsPerInchX)+' dots per inch'
              +'||page height: '+IntToStr(metafile_printer.PageHeight)+' dots at '+IntToStr(metafile_printer.PixelsPerInchY)+' dots per inch|| ',
               '','','','open  the  containing  folder','','continue',0)=4
       then begin
              folder_str:=ExtractFilePath(html_pdf_printer.Filename);

              if ShellExecute(0,'explore',PChar(folder_str),nil,nil,SW_SHOWNORMAL)<=32
                 then show_modal_message('Sorry, unable to open the folder.')
                 else external_window_showing:=True;
            end;

    metafile_printer.Free;
  end;//try
}

end;
//______________________________________________________________________________

procedure Thelp_form.FormHide(Sender: TObject);     // 0.93.a

begin
  ClientWidth := help_client_width_as_user;
  ClientHeight := help_client_height_as_user;
  resize_help_form;
end;
//______________________________________________________________________________

procedure Thelp_form.scroll_labelDblClick(Sender: TObject);  // 0.93.a

begin
  Clipboard.AsText := current_load_str;
  Beep;
end;
//______________________________________________________________________________

procedure Thelp_form.create_emf_file_popup_entryClick(Sender: TObject);    // 214a  test...

var
  { OT-FIRST help_metafile:TMetafile;}
  file_str, folder_str: string;

begin
  do_open_source_bang('CREATE EMF');  // OT-FIRST

  (* OT-FIRST
  file_str:=Config.MakeFilePath('emfs', 'help_notes_metafile.emf');

  help_metafile:=html_view.MakeMetafile(0, html_view.Width, html_view.Width+30, html_view.Height);

  try
    help_metafile.Enhanced:=True;
    help_metafile.SaveToFile(file_str);

    if alert(2,'    metafile  created',
                  ' |The metafile was successfully created:||'+file_str+'| ',
                  '','','','open  the  containing  folder','','continue',0)=4
       then begin
              folder_str:=ExtractFilePath(file_str);

              if ShellExecute(0,'explore',PChar(folder_str),nil,nil,SW_SHOWNORMAL)<=32
                 then ShowMessage('Sorry, unable to open the folder.')
                 else external_window_showing:=True;
              end;

  except
    ShowMessage('Sorry, an error occurred in creating the metafile.');
  end;//try

  if Assigned(help_metafile) then help_metafile.Free;

*)

end;
//______________________________________________________________________________

procedure help_update;

var
  current_pos: integer;

begin
  with help_form do begin

    if Showing = True then begin
      current_pos := html_view.Position;

      with html_back[html_back_index] do
        do_help(src_code, src_str, False);     // show the file or text (already in history).

      html_view.Position := current_pos;
      // and re-position it as near as poss.

      if html_view.Visible = True then
        html_view.SetFocus;                      // so that the arrow keys work.
    end;

  end;//with
end;
//______________________________________________________________________________

procedure Thelp_form.help_font_normal_popup_entryClick(Sender: TObject);

begin
  help_font_style := 0;
  help_update;
end;
//______________________________________________________________________________

procedure Thelp_form.help_font_bold_popup_entryClick(Sender: TObject);

begin
  help_font_style := 1;
  help_update;
end;
//______________________________________________________________________________

procedure Thelp_form.help_font_12_popup_entryClick(Sender: TObject);

begin
  help_font_size := 12;
  help_update;
end;

procedure Thelp_form.help_font_13_popup_entryClick(Sender: TObject);
begin
  help_font_size := 13;
  help_update;
end;

procedure Thelp_form.help_font_14_popup_entryClick(Sender: TObject);
begin
  help_font_size := 14;
  help_update;
end;

procedure Thelp_form.help_font_15_popup_entryClick(Sender: TObject);
begin
  help_font_size := 15;
  help_update;
end;

procedure Thelp_form.help_font_16_popup_entryClick(Sender: TObject);
begin
  help_font_size := 16;
  help_update;
end;

procedure Thelp_form.help_font_17_popup_entryClick(Sender: TObject);
begin
  help_font_size := 17;
  help_update;
end;

procedure Thelp_form.help_font_18_popup_entryClick(Sender: TObject);
begin
  help_font_size := 18;
  help_update;
end;

procedure Thelp_form.help_font_19_popup_entryClick(Sender: TObject);
begin
  help_font_size := 19;
  help_update;
end;

procedure Thelp_form.help_font_20_popup_entryClick(Sender: TObject);
begin
  help_font_size := 20;
  help_update;
end;
//______________________________________________________________________________

initialization

  //RegisterClass(TGIFImage);
  //TPicture.RegisterFileFormat('gif','GIF image',TGIFImage);
  //______________________________________________________________________________

end.
