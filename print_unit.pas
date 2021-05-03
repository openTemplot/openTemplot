
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

unit print_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls

  { OT-FIRST ,dtpShape,dtpGR32};  // 206e

type
  Tprint_form = class(TForm)
    info_scrollbox: TScrollBox;
    printer_info_label: TLabel;
    page_panel: TPanel;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    omit_all_panel: TPanel;
    omit_all_button: TButton;
    all_panel: TPanel;
    all_button: TButton;
    in_progress_label: TLabel;
    next_row_button: TButton;
    omit_panel: TPanel;
    print_panel: TPanel;
    omit_page_button: TButton;
    ok_button: TButton;
    page_map_button: TButton;
    header_label: TLabel;
    page_label: TLabel;
    font_button: TButton;
    origin_label: TLabel;
    printer_label: TLabel;
    row_progressbar: TProgressBar;
    options_page_control: TPageControl;
    options_tab_sheet: TTabSheet;
    warnings_checkbox: TCheckBox;
    info_checkbox: TCheckBox;
    banner_fill_checkbox: TCheckBox;
    black_edges_checkbox: TCheckBox;
    intensity_tab_sheet: TTabSheet;
    intensity_label: TLabel;
    light_label: TLabel;
    dark_label: TLabel;
    reset_button: TButton;
    print_intensity_trackbar: TTrackBar;
    no_intensity_panel: TPanel;
    picture_tab_sheet: TTabSheet;
    picture_stretch_radio: TRadioButton;
    picture_dots_radio: TRadioButton;
    picture_outlines_radio: TRadioButton;
    picture_help_button: TButton;
    picture_borders_checkbox: TCheckBox;
    help_button: TButton;
    Shape1: TShape;
    banner_pause_final_page_checkbox: TCheckBox;
    cancel_panel: TPanel;
    cancel_button: TButton;
    detail_mode_radiobutton: TRadioButton;
    diagram_mode_radiobutton: TRadioButton;
    print_sketchboard_items_checkbox: TCheckBox;
    omit_pictures_radio: TRadioButton;
    datestamp_label: TLabel;
    page_ident_checkbox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure colour_panelClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure omit_all_buttonClick(Sender: TObject);
    procedure omit_page_buttonClick(Sender: TObject);
    procedure ok_buttonClick(Sender: TObject);
    procedure all_buttonClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure banner_fill_checkboxClick(Sender: TObject);
    procedure next_row_buttonClick(Sender: TObject);
    procedure page_map_buttonClick(Sender: TObject);
    procedure font_buttonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure black_edges_checkboxClick(Sender: TObject);
    procedure print_intensity_trackbarChange(Sender: TObject);
    procedure reset_buttonClick(Sender: TObject);
    procedure dark_labelClick(Sender: TObject);
    procedure light_labelClick(Sender: TObject);
    procedure picture_dots_radioClick(Sender: TObject);
    procedure picture_help_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure diagram_mode_radiobuttonClick(Sender: TObject);
    procedure detail_mode_radiobuttonClick(Sender: TObject);
    procedure print_sketchboard_items_checkboxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  print_form: Tprint_form;

//________________________


  //-----------------------

  print_busy:boolean=False;        // checked by endless loop to avoid changing data in mid-print.

  want_bottom_margin:boolean=False;  // bottom margin on last page not wanted unless multiple print runs.

   // OT-FIRST moved to interface...

  printgrid_wide:integer=1;      // dots line thickness default.
  printpicborder_wide:integer=1; // dots.

  printmargin_wide:integer=3;    // dots.
  printshape_wide:integer=2;     // dots.
  printrail_wide:integer=2;      // dots.
  printtimber_wide:integer=2;    // dots.
  printmark_wide:integer=2;      // dots.

  printcl_wide:integer=1;        // dots.  track centre-line  0.79.a


  procedure print_info(top_str, bottom_str:string);

  function print_draw:boolean;

  procedure show_output_mode_panel;

  function calc_intensity(colour:integer):integer;

//___________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  Printers, calibration_unit, preview_unit, control_room, pad_unit, alert_unit, keep_select, math_unit, gauge_unit,
  info_unit, colour_unit, bgnd_unit, bgkeeps_unit, help_sheet,
  stay_visible_unit, panning_unit, shove_timber, grid_unit, edit_memo_unit,
  print_settings_unit, print_now_box, { OT-FIRST dtp_settings_unit,} export_unit,
  { OT-FIRST dtp_unit,} rail_options_unit, platform_unit, check_diffs_unit,
  data_memo_unit,

  trackbed_unit, make_slip_unit;  // 214b

const
  print_help_str:string='      Printing  Pages'
  +'||Behind this PRINT PAGES window you can see the layout of pages comprising your drawing. Drag and resize this window as necessary to get a clear view.'
  +' (It is often useful to have resized the trackpad window beforehand to less than the full screen.)'
  +'||The tracks are drawn in skeleton form with rails only, but will print out fully detailed according to your current settings in the GENERATOR and PRINT menus.'
  +' The rails are shown in different colours for the control template and background templates, the latter being shown as single rail-edges only.'
  +'||( The TOP of the printed pages corresponds to the LEFT MARGIN on the screen, so selecting between upright (portrait) and sideways (landscape) paper orientation might at first seem confusing.'
  +' In most cases the pre-set upright (portrait) setting gives the best fit on the pages.)'

  +'||If PRINT CONTROL TEMPLATE was selected only pages containing it will be printed, along with any background items which happen to be on them.'
  +'||If PRINT ALL TEMPLATES was selected, all the pages needed to contain any background templates will be printed.'
  +'||To enlarge or reduce the scale of the printed template, or change the printed colours or line thicknesses, select the various PRINT menu items.'
  +' The page layout shown here (and on the trackpad) will change to reflect the new size.'
  +'||You can choose to print any or all of the pages shown:'
  +'||To print all the pages click the PRINT ALL REMAINING PAGES bar.'
  +'||To cancel any printing, click the OMIT ALL REMAINING PAGES bar or press the F12 or Esc keys.'
  +'||To print individual page(s), click the OMIT PAGE button until the NEXT PAGE display shows the page you require. Then click the PRINT PAGE button.'
  +'||To omit all the pages in a row, or the remaining pages in the current row, click the NEXT ROW button.'
  +'||Continue clicking the OMIT PAGE, PRINT PAGE and NEXT ROW buttons as required. If no more pages are required click the OMIT ALL REMAINING PAGES bar. To print'
  +' all the remaining pages click the PRINT ALL REMAINING PAGES bar.'
  +'|--------------'
  +'|Handy Hint:'
  +'|To rapidly run through the pages of a large plan you can repeatedly select the OMIT PAGE, PRINT PAGE or NEXT ROW buttons by using the accelerator keys marked on the buttons'
  +' (i.e. hold down the O key on the keyboard to omit a run of pages, the P key to print a run of pages, or the N key to omit several rows of pages).'
  +'|--------------'
  +'||To change the font used for the page labels on the preview screen click the FONT... button.'
  +'||To print a copy of this preview screen click the PRINT PAGE MAP button. This is a useful guide to joining the pages when you are printing a large plan.'
  +' (If the button is clicked after page printing has started, the page map will be printed when page printing has finished. The font used for the page labels on the page map'
  +' can be changed by selecting the PROGRAM > PRINTER FONT + MARGINS... menu item on the PROGRAM PANEL window.)'

  +'||If the page margins do not conveniently fit the paper size or grid spacings being used, the trim margins can be changed. Return to the trackpad (F12) and then click the PRINT > TRIM MARGINS > ? TRIM MARGINS HELP menu item for more information.'
  +'||If the drawing is not conveniently placed between the page margins, the page origin can be moved. Return to the trackpad (F12) and then click either the PRINT > PAGE ORIENTATION / ORIGIN > SET PAGE ORIGIN... menu item'
  +' to enter the new page origin position directly, or the ACTION > MOUSE ACTIONS:PAD > MOVE PAGE ORIGIN menu item (SHIFT+CTRL-F10) to move the page origin with the mouse.'
  +'||Likewise when printing the entire trackpad at a reduced size, the print size can be changed by mouse action to achieve a convenient fit to the pages. Click the ACTION > MOUSE ACTIONS:PAD > ADJUST PRINT SIZE menu item.'
  +' The page outlines on the trackpad will change accordingly.'
  +'|--------------'
  +'||There are four OPTIONS > tickboxes:'

  +'||If INFORMATION PAGE is ticked, a page containing the INFO details from the information panel will be printed to accompany the control template.'
  +'||This is useful when printing final construction templates as a permanent written record, but you will probably want to untick the box when doing trial templates to save paper.'
  +' Any MEMO notes for the control template will also be added.'
  +'||This option box has no effect when the entire trackpad is being printed, even if the control template is included.'
  +' The text font used for this page can be changed - select the PROGRAM > PRINTER FONT + MARGINS... menu item on the PROGRAM PANEL window.'

  +'||If BLACK RAIL-EDGES is ticked, all rail edges will be printed in black regardless of any other colour, print-intensity or grey-shade settings which you may have made.'
  +'||This option is useful when the print intensity has been reduced (see below),'
  +' or when printing the background templates in their mapping colours, or in a single non-black colour ( PRINT > PRINTED DRAWING OPTIONS > CLOLOUR OPTIONS > PRINT ALL TEMPLATES IN A SINGLE COLOUR menu item),'
  +' but you want to retain a full black for the rail edge lines.'

  +'||If WARNINGS is ticked, a warning will be printed on each page if the printer is uncalibrated, or if data distortions are in force. For more information about'
  +' printer calibration, select the PRINT > PRINTER CALIBRATION > CALIBRATE PRINTER... menu item.'
  +'||For information about data distortions, select the PROGRAM > EXPERT menu items on the PROGRAM PANEL menus.'

  +'||When printing on single sheets of paper Templot0 ignores any empty (blank) pages and prints only the pages of your drawing which actually contain track.'
  +'||If BANNER FILL is ticked, any such empty pages will be included in the print run when printing on banner or roll paper, so that no lengthwise page joins are needed. Bear in mind that for some track plans'
  +' this could mean printing a great many blank pages (e.g. for a circular layout you would be printing blank pages to fill the whole of the centre space).'
  +' In such cases you will probably want to untick the box and separate out the individual pages from the banner print run.'
  +' This option box has no effect when printing on single sheets.'

  +'||The PRINT INTENSITY adjuster is useful to darken trial templates when using the printer''s Draft/Fast print-quality mode, without upsetting your colour settings for final templates using the Best/Letter-Quality mode.'
  +'||Change the setting by sliding the adjuster, or by clicking the LIGHT and DARK labels. The RESET button restores the normal print intensity setting.'
  +'||The setting is also reset to normal after changing any of the print colours (PRINT > PRINTED DRAWING OPTIONS menu items).'

  +'||The intensity setting modifies the print colours, not the line thicknesses (line widths) which can be set separately using the PRINT > PRINTED LINE THICKNESS menu items.'

  +'||When using a reduced print intensity, you can ensure that the rail edges remain a full black by ticking the BLACK RAIL-EDGES option box (see above).'

  +'||The print intensity adjustment is not available when printing in black && white only. Select GREY-SHADE PRINTING instead.'

  +'||Printers vary considerably in performance and you may need to experiment to achieve the optimum results from your particular printer.'
  +'||This print intensity setting is internal to Templot0. Many printers also include setup options to vary the ink intensity and to print'
  +' in grey-shades (gray-scale). You can compare the results from these hardware options with those from changing the settings here in Templot0.'

  +'||For some additional notes about colour options for printing, click the ABOUT MAPPING COLOURS button below.'

  +'|-----------------'
  +'||N.B. If PRINT ALL REMAINING PAGES has been selected you can abort the sending of pages to the printer by pressing the F12 or Esc keys, but any already sent will continue to be printed.'

  +'||After all the pages have been sent, Templot0 returns to the trackpad so that you can continue working while the pages are being printed. To abort printing at this stage you must use the Windows'
  +' printer controls (double-click on the printer icon in the taskbar, in the window which appears click the TEMPLOT PAGES entry, then in the DOCUMENT menu, select CANCEL PRINTING).'

  +'||When printing all the pages of a complete track plan at full-size, you may be printing several dozen pages of graphics. If your system memory or disk space is limited, the Windows'
  +' printer spooler and/or your printer driver software may have problems. Try changing the spooler settings, or printing directly to the printer.'
  +' To make these changes, from the Windows taskbar click Start > Settings > Printers > File menu > Properties > Details tab > Spool Settings, or consult Windows Help and your printer documentation for further details.'

  +'||If you print directly to the printer Templot0 sends pages for printing one at a time, so you will be able to abort printing after each page by pressing F12 or Esc,'
  +' but you will not be able to continue working in Templot0 until all of the required pages have been printed.'

  +'||( The difference between pressing F12 or Esc is that F12 additionally unlocks the trackpad redraw if you have been printing with the redraw locked. For more information about locking the redraw, see'
  +' the help notes in the REAL > TIMBERING > TIMBERING DATA... menu item. Locking the redraw is unnecessary when timber randomizing is switched off, or when printing only background templates between rebuilds.'
  +' If you have not locked the redraw, there is no difference between F12 and Esc.)'


  +'||Handy Hints :'

  +'||When printing the entire trackpad the control template is normally omitted and only the templates comprising the background drawing are printed. To include the control template,'
  +' select the PRINT > ALL TEMPLATES OPTIONS > INCLUDE CONTROL TEMPLATE menu item.'

  +'||If your background contains a great many templates, it could take up to a minute or more before the printer starts printing. This is true even if you are printing only the control template on a single page.'
  +' To speed up printing, temporarily wipe unwanted background templates from the trackpad.'

  +'||If your printer is capable of printing continuous banners on Z-fold or roll paper this can usefully eliminate most (or all) of the page-joins for a large template.'
  +' It is important to make the correct settings for the printer. Click the PRINT > BANNER / ROLL PAPER menu item, and read the help notes.';

  picture_help_str:string='    `0Printing  Background  Picture  Shapes`9'
  +'||The background picture shapes are intended mainly for use on the screen as design guides. It is recommended that you normally omit them or print them as rectangle outlines only on your track templates.'
  +'||Including large bitmap images in the printed pages may significantly increase printing time, and is not supported on all printers.'

  +'||If possible the `0PRINT PICTURES NORMAL`1 option should be used to print them. This method uses the Windows StretchDraw function which may fail at high magnifications of the original image, and is not available on some printers.'

  +'||The alternative `0PRINT PICTURES BY DOTS`1 option will work at all magnifications and on most printers, but is extremely slow, and should be regarded as a last resort. It is significantly faster for images scanned in black and white only.'
  +' In many cases the `0BY DOTS`1 method will work better if you change the printer spooler setting to RAW data (see below).'

  +'||If the `0TRANSPARENT`1 option is set for a picture shape (in the `0BACKGROUND SHAPES`1 dialog) it will be printed with underlying detail showing through (if the printer supports raster functions).'
  +' Otherwise underlying detail will be covered over, including the grid lines and page trim margins.'
  +' To avoid this, ensure that the `0GRID IN FRONT`1 tickbox is selected.'

  +'||rp.gif The `0TRANSPARENT`1 option requires significantly more graphics memory and may fail for large picture shapes. If the picture shape does not appear in the print, please deselect the `0TRANSPARENT`1 option and try again.'

  +'||rp.gif For the `0BY DOTS`1 method the `0TRANSPARENT`1 option does not apply and is ignored. However, any white areas of the image will always be printed as transparent.'

  +'||Both methods adjust the intensity of the image according to the current `0PRINT INTENSITY`1 setting.'

  +'||green_panel_begin tree.gif There is an alternative method of including picture shapes on the printed templates via the `0sketchboard`1 functions. Refer to the sketchboard notes for more information.'
  +'||If you adopt this method, be sure to select the `0omit pictures`1 option here, otherwise the picture shapes will be output to the printer twice.green_panel_end'

  +'|Printing large bitmaps makes great demands on your system''s memory and resources. Don''t have more picture shapes on the trackpad than you need,'
  +' and keep bitmap files as small as possible by cropping off unwanted areas or by scanning at a lower resolution or in grey-scale instead of colour.'
  +' Close any other applications which you have running. If lengthy disk drive activity takes place - please be patient.'
  +' If you experience problems, please save your work, quit Templot, restart Windows, restart Templot0 and try again.'

  +'||Printing performance may also be improved by changing from EMF to RAW data, or by printing directly to the printer instead of via the Windows spooler.'
  +' To make these changes, from the Windows taskbar click Start > Settings > Printers > select printer > File menu > Properties > Details tab > Spool Settings, or consult Windows Help and your printer documentation for further details.'

  +'||If problems persist, restart Templot0 and print background picture shapes as outlines only.'

  +'||N.B. Please be aware that printing scanned images may require the permission of the copyright owner.'

  +'||For more information about using Background Shapes and bitmap images, click `0MORE ABOUT PICTURE SHAPES`1 below.';

var
  button_clicked:boolean=False;
  banner_changed:boolean=False;
  printer_printing:boolean=False;
  index_sheet_wanted:boolean=False;

  info_was_showing:boolean=False;
  panning_was_showing:boolean=False;
  shove_was_showing:boolean=False;
  spacing_was_showing:boolean=False;
  rail_options_was_showing:boolean=False;
  platform_was_showing:boolean=False;
  trackbed_was_showing:boolean=False;
  check_diffs_was_showing:boolean=False;
  data_child_was_showing:boolean=False;
  stay_visible_was_showing:boolean=False;


  form_scaling:boolean=False;

  print_intensity:extended=50.0;   // ink intensity 0-100.

  impact:integer=0;



  procedure print_bgnd(grid_left,grid_top:extended);forward;        // print background items.
  procedure print_bgnd_shapes(grid_left,grid_top:extended);forward; // print all background shapes.

  procedure print_sketchboard_items(on_canvas:TCanvas; grid_left,grid_top:extended);forward;   // 206e

  procedure print_info_banner(top_str, bottom_str:string);forward;  // print the info page on banner paper.

  { OT-FIRST procedure rotate_metafile(i:integer);forward;   // rotate metafile for picture shape.}

  procedure rotate_bitmap(i:integer);forward;     // rotate bitmap for picture shape.

  procedure rotate_bitmap_by_dots(i:integer; p1,p2:Tpex; grid_left,grid_top:extended; source_canv,dest_canv:TCanvas);forward;

  function check_if_on_page(corner1,corner2:TPoint; check_within_limits:boolean):boolean;forward;

//_______________________________________________________________________________________

procedure disable_buttons;

begin
  with print_form do begin
    omit_page_button.Enabled:=False;
    next_row_button.Enabled:=False;
    ok_button.Enabled:=False;
    all_button.Enabled:=False;

    banner_fill_checkbox.Enabled:=False;              // can't click this once printing under way.
    banner_pause_final_page_checkbox.Enabled:=False;  // 0.93.a

    all_panel.Enabled:=False;

  end;//with
end;
//___________________________________________________________________________________

procedure enable_buttons(next_row_enabled:boolean);

begin
  with print_form do begin
    omit_all_button.Enabled:=True;
    omit_page_button.Enabled:=True;
    next_row_button.Enabled:=next_row_enabled;

    ok_button.Enabled:=True;
    all_button.Enabled:=True;

    omit_all_panel.Enabled:=True;
    all_panel.Enabled:=True;

    if (banner_paper=True) and (printer_printing=False)
       then begin
              banner_fill_checkbox.Enabled:=True;               // might have been disabled if this is a change.
              banner_pause_final_page_checkbox.enabled:=True;   // 0.93.a
            end;

  end;//with
end;
//___________________________________________________________________________________

function calc_intensity(colour:integer):integer;

var
  red,green,blue,av:integer;    // colour components (0-255).
  mod_col:extended;

begin
  if colour=clWhite     // no change to white (bare paper).
     then begin
            RESULT:=clWhite;
            EXIT;
          end;

  if black_white=True
     then begin
            RESULT:=clBlack;
            EXIT;
          end;
                        // defaults init..

  if colour=virtual_black_colour then RESULT:=virtual_black_colour
                                 else RESULT:=0;

  try
    red:=(colour AND $000000FF);
    green:=(colour AND $0000FF00) div $100;
    blue:=(colour AND $00FF0000) div $10000;

    if grey_shade=True     // average to a grey..
       then begin
              av:=(red+green+blue) div 3;
              red:=av;
              green:=av;
              blue:=av;
            end;

                    // modify colours according to intensity setting...

    mod_col:=(50-print_intensity)*5;   // *5 arbitrary (gives range +250 to -250 as colour modifiers for intensity range 0-100).

    red:=limits_i(0,255,red+Round(mod_col));
    green:=limits_i(0,255,green+Round(mod_col));
    blue:=limits_i(0,255,blue+Round(mod_col));

    RESULT:=(blue*$10000)+(green*$100)+red;
  except
    EXIT;
  end;

  if (colour=virtual_black_colour) and (RESULT=clBlack) then RESULT:=virtual_black_colour;  // don't set black for rail edges. (HP bug)
end;
//____________________________________________________________________________________

procedure print_colours_setup;  // set grey shades and/or print intensity.

var
  dummy:integer;

begin
  if black_white=True
     then begin
            print_railedge_colour:=clBlack;

            printcurail_colour:=clBlack;
            printbgrail_colour:=clBlack;

            printtimber_colour:=clBlack;

            printrail_infill_colour_cu:=clWhite;      // !!! used for solid infill.
            printrail_infill_colour_bg:=clWhite;      // !!! used for solid infill.

            printtimber_infill_colour:=clBlack;       // !!! only used for hatched infill.
            printgrid_colour:=clBlack;
            printmargin_colour:=clBlack;
            printguide_colour:=clBlack;
            printjoint_colour:=clBlack;
            printalign_colour:=clBlack;

            print_labels_font.Color:=clBlack;
            print_timber_numbers_font.Color:=clBlack;

            printshape_colour:=clBlack;
            printbg_single_colour:=clBlack;

            printplat_edge_colour:=clBlack;    // platform edges
            printplat_infill_colour:=clBlack;  // platform infill

            sb_track_bgnd_colour:=clWhite; // 206a
            sb_diagram_colour:=clWhite;    // 209c

          end
     else begin
            print_intensity:=limits(0,100,print_form.print_intensity_trackbar.Position*2,dummy);  // trackbar 0-50, normal 25. print_intensity 0-100, normal 50.

            print_railedge_colour:=calc_intensity(save_prc);       //  calc colours or shades...
            printtimber_colour:=calc_intensity(save_ptc);

            printrail_infill_colour_cu:=calc_intensity(save_priccu);
            printrail_infill_colour_bg:=calc_intensity(save_pricbg);

            printtimber_infill_colour:=calc_intensity(save_ptic);
            printgrid_colour:=calc_intensity(save_grc);
            printmargin_colour:=calc_intensity(save_pmc);
            printguide_colour:=calc_intensity(save_pgc);
            printjoint_colour:=calc_intensity(save_pjc);
            printalign_colour:=calc_intensity(save_pac);

            print_labels_font.Color:=calc_intensity(save_fc);
            print_timber_numbers_font.Color:=calc_intensity(save_tnfc);

            print_corner_page_numbers_font.Color:=calc_intensity(save_cpnfc);     // 0.93.a added

            printshape_colour:=calc_intensity(save_psc);
            printbg_single_colour:=calc_intensity(save_pbg);

            if print_form.black_edges_checkbox.Checked=True then print_railedge_colour:=virtual_black_colour;

            if print_railedge_colour=0 then print_railedge_colour:=virtual_black_colour;    // HP bug. (0=black, don't mix with colours).

            printcurail_colour:=print_railedge_colour;
            printbgrail_colour:=print_railedge_colour;

            printplat_edge_colour:=calc_intensity(save_priplatedge);   // platform edges
            printplat_infill_colour:=calc_intensity(save_priplatfill); // platform infill

            sb_track_bgnd_colour:=calc_intensity(save_sb_track_bgnd); // 206a
            sb_diagram_colour:=calc_intensity(save_sb_diagram_col);   // 209c

          end;
end;
//__________________________________________________________________________________________

procedure print_line_thickness_setup;

var
  av_dpi:extended;

                 ///////////////////////////////////////////////

                 function calc_thick(mm_thick:extended; adjust:boolean):integer;  // calc line thickness in dots.

                 var
                   line_dots:extended;

                 begin
                   line_dots:=mm_thick*av_dpi/25.4;
                   if (adjust=True) and (pad_form.adjust_line_thickness_menu_entry.Checked=True) then line_dots:=line_dots*out_factor;

                   RESULT:=Round(line_dots);
                   if RESULT<1 then RESULT:=1;
                 end;
                 ///////////////////////////////////////////////

begin
  av_dpi:=(nom_width_dpi+nom_length_dpi)/2;

     printgrid_wide:=calc_thick(  printgrid_thick,False);  // don't reduce line thickness for scaled output - the paper size is still the same.
   printmargin_wide:=calc_thick(printmargin_thick,False);  // ditto.

        printshape_wide:=calc_thick(    printshape_thick,True);
    printpicborder_wide:=calc_thick(printpicborder_thick,True);
         printrail_wide:=calc_thick(     printrail_thick,True);
       printtimber_wide:=calc_thick(   printtimber_thick,True);
           printcl_wide:=calc_thick(       printcl_thick,True);    // 0.79.a
         printmark_wide:=calc_thick(     printmark_thick,True);

end;
//______________________________________________________________________________

procedure make_print_preview_screenshot;   // 214b

var
  { OT-FIRST create_png:TPNGObject;}
  create_png:TPortableNetworkGraphic;    // OT-FIRST

  file_str:string;       // including path

begin
  file_str:=exe_str+'PRINT-PREVIEW-FILES\print_preview'+FormatDateTime('_yyyy_mm_dd_hhmm_ss',Date+Time)+'.png';

  { OT-FIRST create_png:=TPNGObject.Create;}
  create_png:=TPortableNetworkGraphic.Create;     // OT-FIRST
  try
    try
      create_png.Assign(offdraw_bmp);
      create_png.SaveToFile(file_str);
    except
      ShowMessage('Sorry, an error occurred in creating the preview record file.'
                  +#13+#13+'This doesn''t prevent printing.');
    end;//try

  finally
    create_png.Free;
  end;//try
end;
//______________________________________________________________________________

function print_draw:boolean;     // draw control template or entire pad on the printer.

     // n.b. this code tries to draw the complete template on every sheet,
     // and relies on the GDI to crop it to the current page rectangle.

     // return true if any printing done, false if she cancels all.

label
  123;

var
  infill_points:array[0..3] of TPoint;   // array of corners for infilled timbers.

  cur_cal:Tcal_data;
  cal_ok:boolean;
  prindex:integer;

  gridx, gridy, now_gridx, now_gridy:extended;
  grid_label:extended;

  down:extended;                  // temp x and y from list.
  across:extended;

  sheet_down:integer;             // current sheet index.
  sheet_across:integer;           // ditto

  max_sheet_across:integer;       // highest used.

  max_sheet_down:array[0..sheet_across_c] of integer;  // 0.93.a keep track of highest non-empty page in each row.

  gridco:integer;
  grid_now_dots:integer;

  i, aq, rail, now, now_max, dots_index, mark_code:integer;

  w_dots, l_dots, w_dots1, l_dots1, w_dots2, l_dots2:integer;

  w1,l1,w2,l2,wmid,lmid:integer;

  all_pages:boolean;              // False = print one page at a time.
  page_count:integer;
  row_count:integer;

  page_num_str:string;
  page_str, info_str, top_str, bottom_str:string;
  xing_str:string;
  temp_str:string;
  pgco_str:string;
  stry:integer;

  p1,p2,p3,p4:Tpoint;
  radcen_arm:extended;

  move_to, line_to: TPoint;

  l_dims_valid:boolean;
  w_dims_valid:boolean;

  banner_top_done:boolean;

  x_dots, y_dots:integer;
  bangrid_dots:integer;

  pen_width:integer;

  ptr_1st,ptr_2nd:^Tmark;          // pointers to a Tmark record.
  markmax:integer;

  fontsize:extended;
  num_str, tbnum_str:string;

  grid_str:string;
  grid_label_str:string;

  switch_label_str:string;  // 206b

  all_pages_origin_str,this_page_begin_str,this_page_end_str:string;  // 208g

  last_file_str:string;  // 214a

  ident_left,ident_top,wm_shift:integer;  // 214a

  preview_record_file_made:boolean;   // 214b



              /////////////////////////////

              procedure begin_doc;

              begin
                printer_printing:=True;
                Printer.BeginDoc;
              end;

              /////////////////////////////

              procedure end_doc(no_cancel:boolean);

              begin
                with print_form do begin

                  if banner_paper=False
                     then begin
                            Printer.EndDoc;             // any further single sheet printing uses file assignment printing on a fresh page.
                            printer_printing:=False;

                            if (info_checkbox.Checked=True) and (print_entire_pad_flag=False) and (no_cancel=True)
                               then begin
                                      Printer.Title:='Templot Info Page';
                                      print_info(top_str, bottom_str);
                                    end;
                          end
                     else begin    // but for banner printing, info runs on...

                            if (info_checkbox.Checked=True) and (print_entire_pad_flag=False) and (no_cancel=True)
                               then print_info_banner(top_str, bottom_str);

                            Printer.EndDoc;              // banner finished.
                            printer_printing:=False;
                          end;

                  if (no_cancel=True) and (index_sheet_wanted=True) then print_index_sheet(False);
                end;//with

                print_now_bang:=False;                          // reset for next time  0.93.a
                print_now_form.print_data_sent_label.Show;      // 0.93.a

              end;
              /////////////////////////////

              procedure draw_marks(grid_left,grid_top:extended; rail_joints:boolean);

                // if rail_joints=True draw only the rail joints, otherwise omit them.
              var
                i:integer;
                s:string;  // 208a

              begin
                markmax:=High(marks_list_ptr);  // max index for the present list.

                if mark_index>markmax then mark_index:=markmax;  // ??? shouldn't be.

                tbnum_str:=timb_numbers_str;      // the full string of timber numbering for the control template.

                with Printer.Canvas do begin

                  for i:=0 to (mark_index-1) do begin   // (mark_index is always the next free slot)

                    ptr_1st:=@marks_list_ptr[i];  // pointer to the next Tmark record.
                    if ptr_1st=nil then BREAK;

                    mark_code:=ptr_1st^.code;              // check this mark wanted.

                    if mark_code=0 then CONTINUE;     // ignore mark entries with code zero (might be the second or third of a multi-mark entry, e.g. for timber infill).

                    if rail_joints=(mark_code<>6) then CONTINUE;  // do only the rail joints if rail_joints=True and ignore them otherwise.

                    if print_settings_form.output_timbering_checkbox.Checked=False
                       then begin
                              case mark_code of
                                 3,4,5,14,33,44,54,55,93,95,99,203,233,293: CONTINUE;     // no timbering wanted.
                              end;//case
                            end;

                    if print_settings_form.output_radial_ends_checkbox.Checked=False
                       then begin
                              case mark_code of
                                 2,7: CONTINUE;     // no radial ends wanted  206a
                              end;//case
                            end;

                    if print_settings_form.output_switch_labels_checkbox.Checked=False
                       then begin
                              case mark_code of
                                 600,601..605: CONTINUE;     // no long marks or switch labels wanted  206b
                              end;//case
                            end;

                    if print_settings_form.output_xing_labels_checkbox.Checked=False
                       then begin
                              case mark_code of
                                 700..703: CONTINUE;     // no long marks or crossing labels wanted  211b
                              end;//case
                            end;

                    if ((mark_code=203) or (mark_code=233) or (mark_code=293)) and (i<(mark_index-1))      // timber infill
                       then begin
                              ptr_2nd:=@marks_list_ptr[i+1];        // pointer to the second infill Tmark record.
                              if ptr_2nd=nil then BREAK;

                              p1:=ptr_1st^.p1;              // x1,y1 in  1/100ths mm
                              p2:=ptr_1st^.p2;              // x2,y2 in  1/100ths mm
                              p3:=ptr_2nd^.p1;              // x3,y3 in  1/100ths mm
                              p4:=ptr_2nd^.p2;              // x4,y4 in  1/100ths mm
                            end
                       else ptr_2nd:=nil;    // keep compiler happy.

                    if ( (mark_code>0) and (mark_code<200) and (mark_code<>8) and (mark_code<>9) and (mark_code<>10) ) // ignore peg, rad centres, timber selector and peg arms, plain track start, label.
                    or (mark_code=600) or (mark_code=700)  // 206b 211b overwrite switch/xing marks on output
                       then begin

                              if ((mark_code=5) or (mark_code=55) or (mark_code=95) or (mark_code=600) or (mark_code=700)) and (out_factor<>1.0) then CONTINUE;   // reduced ends are meaningless if not full-size. 206b 600 added, 211b 700 added

                              p1:=ptr_1st^.p1;              // x1,y1 in  1/100ths mm

                              if mark_code<>99
                                 then begin
                                        p2:=ptr_1st^.p2;    // x2,y2 in  1/100ths mm

                                        Pen.Style:=psSolid; // default init.

                                        Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                                        Brush.Style:=bsClear;

                                        case mark_code of
                                               1: Pen.Width:=printmark_wide;    // guide marks.
                                               2: Pen.Width:=printmark_wide;    // rad end marks.
                                         3,33,93: Pen.Width:=printtimber_wide;  // timber outlines.
                                            4,44: begin
                                                    Pen.Width:=1;                  // timber centre-lines.
                                                    Pen.Style:=psDash;
                                                  end;
                                         5,55,95: begin
                                                    Pen.Width:=1;                  // timber reduced ends.
                                                    Pen.Style:=psDot;
                                                  end;
                                               6: Pen.Width:=printmark_wide;    // rail joint marks.
                                               7: Pen.Width:=printmark_wide;    // transition ends.

                                           14,54: begin
                                                    Pen.Width:=printrail_wide;  // timber centre-lines with rail centre-lines (for rivet locations?).
                                                    Pen.Style:=psSolid;
                                                  end;

                                         600,700: Pen.Width:=printrail_wide + printrail_wide div 2;  //  206b 211b long marks

                                             else Pen.Width:=1;
                                        end;//case
                                                     // overides...

                                        // out 0.73.a 12-8-01 (now done in thickness setup) if out_factor<1 then Pen.Width:=Round(Pen.Width*out_factor); // scale down the line width.

                                        if Pen.Width<1 then Pen.Width:=1;
                                        if Pen.Style<>psSolid then Pen.Width:=1;   // delphi bug? (patterns only work for lines 1 dot wide.)
                                        if impact>0 then Pen.Width:=1;             // overide for impact printer or plotter.

                                        if black_white=True
                                           then Pen.Color:=clBlack
                                           else case mark_code of
                                           1,600,700: Pen.Color:=printguide_colour;  // guide marks.    206b 600 added, 211b 700 added
                                                   2: Pen.Color:=printalign_colour;  // rad end marks.
                                             3,33,93: Pen.Color:=printtimber_colour; // timber outlines.
                                                   6: Pen.Color:=printjoint_colour;  // rail joint marks.
                                                   7: Pen.Color:=printalign_colour;         // transition/slewing ends.
                                                 else Pen.Color:=calc_intensity(clBlack);   // thin dotted lines in black only.
                                                end;//case

                                        Pen.Mode:=pmCopy;

                                        move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                        line_to.X:=Round((p2.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;
                                        if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                      end

                                 else begin
                                        if (pad_form.print_timber_numbering_menu_entry.Checked=True)
                                           or ((out_factor>0.99) and (pad_form.numbering_fullsize_only_menu_entry.Checked=True))
                                           then begin
                                                  move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;     // 208a use the screen number position p1 for the control template (no ID number).
                                                  move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                                  num_str:=extract_tbnumber_str(tbnum_str); // get next timber numbering string from the acummulated string.
                                                  if num_str='' then CONTINUE;              // no string available??

                                                  if  (pad_form.timber_numbering_on_plain_track_menu_entry.Checked=False)   // 208a
                                                  and (num_str<>'A1')
                                                     then begin
                                                            s:=Copy(num_str,1,1);

                                                            if (s='A') or (s='E') or (s='R') or (s='N')    // not wanted on plain track,
                                                               then CONTINUE;
                                                          end;

                                                  if check_limit(False,False,move_to)=True
                                                     then begin
                                                            Font.Assign(print_timber_numbers_font);

                                                            if pad_form.scale_timber_numbering_menu_entry.Checked=True
                                                               then begin
                                                                      fontsize:=Font.Size*out_factor;
                                                                      if fontsize<4 then CONTINUE;      // minimum to be legible.
                                                                      Font.Size:=Round(fontsize);
                                                                    end;

                                                            Brush.Style:=bsSolid;
                                                            Brush.Color:=clWhite;

                                                            TextOut(move_to.X-(TextWidth(num_str) div 2),
                                                                    move_to.Y-(TextHeight(num_str) div 2),
                                                                    ' '+num_str+' ');

                                                            Font.Assign(print_labels_font);      // reset for grid labels
                                                          end;
                                                end;
                                      end;
                            end
                       else begin   // other codes...

                              if ( (mark_code=-2) or (mark_code=-3) )  and {(pad_form.print_radial_centres_menu_entry.Checked=True)}  // 0.82.b
                                 (print_settings_form.output_radial_centres_checkbox.Checked=True)        // draw curving rad centres...

                                 then begin
                                        if impact>0 then Pen.Width:=1                 // impact printer or plotter.
                                                    else Pen.Width:=printmark_wide;  // guide marks.

                                        // out 0.73.a 12-8-01 (now done in thickness setup) if out_factor<1 then Pen.Width:=Round(Pen.Width*out_factor); // scale down the line width.

                                        if Pen.Width<1 then Pen.Width:=1;

                                        Pen.Style:=psSolid;
                                        Pen.Mode:=pmCopy;
                                        Pen.Color:=calc_intensity(clBlack);

                                        p1:=ptr_1st^.p1;        // x1,y1 in  1/100ths mm
                                        radcen_arm:=400*scale;  // 4ft scale arbitrary (scale is for control template).

                                        move_to.X:=Round((p1.Y+radcen_arm+ypd-grid_left)*scaw_out)+page_left_dots;      // mark centre widthwise.
                                        move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                        line_to.X:=Round((p1.Y-radcen_arm+ypd-grid_left)*scaw_out)+page_left_dots;
                                        line_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;
                                        if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                                        move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;                 // mark centre lengthwise
                                        move_to.Y:=Round((p1.X+radcen_arm-grid_top)*scal_out)+page_top_dots;

                                        line_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        line_to.Y:=Round((p1.X-radcen_arm-grid_top)*scal_out)+page_top_dots;
                                        if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                      end;

                              if ((mark_code=203) or (mark_code=233) or (mark_code=293))  and (ptr_2nd<>nil)        // timber infill...
                                 then begin
                                        infill_points[0].X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[0].Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                        infill_points[1].X:=Round((p2.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[1].Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;

                                        infill_points[2].X:=Round((p3.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[2].Y:=Round((p3.X-grid_top)*scal_out)+page_top_dots;

                                        infill_points[3].X:=Round((p4.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[3].Y:=Round((p4.X-grid_top)*scal_out)+page_top_dots;

                                        if (check_limits(infill_points[0],infill_points[1])=True) and (check_limits(infill_points[2],infill_points[3])=True)
                                           then begin
                                                  Pen.Width:=1;
                                                  Pen.Style:=psSolid;
                                                  Pen.Mode:=pmCopy;

                                                  Pen.Color:=clWhite;  // so no overdrawing of timber outlines.

                                                  if black_white=True
                                                     then Brush.Color:=clBlack
                                                     else begin
                                                            if impact>0 then Brush.Color:=printtimber_colour            // colour plotter.
                                                                        else Brush.Color:=printtimber_infill_colour;
                                                          end;

                                                  case print_timb_infill_style of
                                                                  0: CONTINUE;
                                                                  1: Brush.Style:=bsFDiagonal;           // hatched. Forward diagonal for the foreground (control template).
                                                                  2: Brush.Style:=bsDiagCross;

                                                                  3: if (impact>0) or (black_white=True)
                                                                        then CONTINUE  // 209c now no fill   was Brush.Style:=bsFDiagonal    // impact printer or plotter, or printing black and white.
                                                                        else Brush.Style:=bsSolid;

                                                                  4: begin                         // blank.
                                                                       Brush.Style:=bsSolid;
                                                                       Brush.Color:=clWhite;       // overide.
                                                                     end;
                                                                else CONTINUE;
                                                  end;//case

                                                  Polygon(infill_points);
                                                end;
                                      end;

                              case mark_code of     // switch labels 206b

                        601..605,701..703: begin

                                            if out_factor<>1.0 then CONTINUE;     // on full size prints only

                                            p1:=ptr_1st^.p1;              // x1,y1 in  1/100ths mm

                                            move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                            move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                            if check_limit(False,False,move_to)=True
                                               then begin
                                                      Font.Assign(print_timber_numbers_font);

                                                      Font.Style:=[fsBold,fsItalic];
                                                      Font.Color:=printguide_colour;

                                                      if scale>3 then Font.Size:=Font.Size+1; // a bit bigger above 3mm/ft

                                                      Brush.Style:=bsSolid;
                                                      Brush.Color:=clWhite;

                                                      case mark_code of
                                                            601: switch_label_str:='tips';
                                                            602: switch_label_str:='set (bend)';
                                                            603: switch_label_str:='planing';
                                                            604: switch_label_str:='stock gauge';
                                                            605: switch_label_str:='joggles';

                                                            701: switch_label_str:='intersection FP';
                                                            702: switch_label_str:='blunt nose';
                                                            703: switch_label_str:='blunt tips';

                                                            else switch_label_str:='';
                                                      end;//case

                                                      TextOut(move_to.X-(TextWidth(switch_label_str) div 2),  // div 2 allows for rotation of template
                                                              move_to.Y-(TextHeight(switch_label_str) div 2),
                                                              ' '+switch_label_str+' ');

                                                      Font.Assign(print_labels_font);      // reset for grid labels
                                                    end;

                                          end;

                              end;//case

                            end;
                  end;//next mark i
                end;//with Printer.Canvas
              end;
              ///////////////////////////////

              function get_w_dots(q,n:integer):integer;

              begin
                with sheet[sheet_down,sheet_across] do begin // (grid_left)
                  RESULT:=Round((outoflist(q,n,1)+ypd-grid_left)*scaw_out)+page_left_dots;
                end;//with
                w_dims_valid:=check_draw_dim_w(RESULT);
              end;
              ////////////////////////////

              function get_l_dots(q,n:integer):integer;

              begin
                with sheet[sheet_down,sheet_across] do begin // (grid_top)
                  RESULT:=Round((outoflist(q,n,0)-grid_top)*scal_out)+page_top_dots;
                end;//with
                l_dims_valid:=check_draw_dim_l(RESULT);
              end;
              ////////////////////////////

              procedure draw_outline_railedge(aq,pencol:integer);

              var
                now:integer;

              begin
                if ( (plain_track=False) or (aq=0) or (aq=8) or (aq=3) or (aq=11) or ((aq>15) and (aq<24)) ) and (aqyn[aq]=True)

                   then begin
                          with Printer.Canvas do begin
                            Pen.Color:=pencol;
                            Pen.Mode:=pmCopy;
                            Pen.Style:=psSolid;

                            move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                            for now:=1 to nlmax_array[aq] do begin
                              line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                              if check_limits(move_to, line_to)=True
                                 then begin
                                        MoveTo(move_to.X, move_to.Y);
                                        LineTo(line_to.X, line_to.Y);
                                      end;
                              move_to:=line_to;
                            end;//for
                          end;//with Canvas
                        end;
              end;
              ////////////////////////////

              procedure draw_fill_rail(outer_add:integer);    // draw a complete filled rail.

              const
                dots_max_c=xy_pts_c*2;

              var
                dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                  //    (xy_pts_c=3000;)
                  //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                  //   template max is 4500' scale length.
                  //   ( = 18000 mm or 59ft approx in 4 mm scale).
                  //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                  // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                  // because needed for the Polygon function.

                  // total memory = 6000*8 bytes = 48kb.

                now, start, now_max:integer;
                edge_started:boolean;
                mid_dots_index:integer;
                edge_colour, blanking_colour:integer;


                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure modify_rail_end(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with Printer.Canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(move_to.X, move_to.Y);

                                                          MoveTo(line_to.X, line_to.Y);
                                                          LineTo(line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


              begin
                with Printer.Canvas do begin

                  if (rail=16) or (rail=20)   // 0.93.a platforms
                     then Pen.Color:=printplat_edge_colour
                     else Pen.Color:=printcurail_colour;         //  1 = virtual black. Bug in HP driver if black (0) specified.

                  Pen.Mode:=pmCopy;
                  Pen.Style:=psSolid;

                  aq:=rail;  // gauge-faces.

                  if ( (plain_track=False) or (aq=0) or (aq=3) or ((aq>15) and (aq<24)) )

                         // if plain track, stock rails and adjacent tracks only.

                     then begin
                            if (aqyn[aq]=False) or (aqyn[aq+outer_add]=False)  // data not for both edges?
                               then begin
                                      if aqyn[aq]=True then draw_outline_railedge(aq,Pen.Color);
                                      if aqyn[aq+outer_add]=True then draw_outline_railedge(aq+outer_add,Pen.Color);
                                      EXIT;
                                    end;

                            now_max:=nlmax_array[aq];

                            if gaunt=False    // 0.93.a normal turnout.
                               then begin
                                      case aq of
                                           1: begin
                                                start:=list_planing_mark_aq1;  // start from end of planing - no infill in planing.

                                                if (start<0) or (start>now_max) then EXIT;  // ???

                                                //out 18-8-01 0.73.a   if ((start+1)<=now_max) and (now_max>1) then drawn_full_aq1:=False;  // ok to overdraw planing.

                                              end;

                                           2: begin                         // ditto
                                                start:=list_planing_mark_aq2;

                                                if (start<0) or (start>now_max) then EXIT;  // ???

                                                //if ((start+1)<=now_max) and (now_max>1) then drawn_full_aq2:=False;

                                              end;
                                         else start:=0;     // whole list.
                                      end;//case
                                    end
                               else start:=0;  // 0.93.a gaunt template, no planing. 0.81 09-Jul-2005



                            dots_index:=0-1;                    // first increment is to zero.

                            edge_started:=False;
                            for now:=start to now_max do
                              begin
                                x_dots:=get_w_dots(aq,now);
                                y_dots:=get_l_dots(aq,now);
                                if (w_dims_valid=True) and (l_dims_valid=True)
                                   then begin
                                          edge_started:=True;

                                          Inc(dots_index);
                                          if dots_index>dots_max_c then dots_index:=dots_max_c;

                                          dots[dots_index].X:=x_dots;
                                          dots[dots_index].Y:=y_dots;
                                        end
                                   else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                              end;//next now

                            mid_dots_index:=dots_index;

                            aq:=rail+outer_add;             // outer-edges.

                            now_max:=nlmax_array[aq];

                            edge_started:=False;
                            for now:=now_max downto 0 do
                              begin
                                x_dots:=get_w_dots(aq,now);
                                y_dots:=get_l_dots(aq,now);
                                if (w_dims_valid=True) and (l_dims_valid=True)
                                   then begin
                                          edge_started:=True;

                                          Inc(dots_index);
                                          if dots_index>dots_max_c then dots_index:=dots_max_c;

                                          dots[dots_index].X:=x_dots;
                                          dots[dots_index].Y:=y_dots;
                                        end
                                   else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                              end;//next now

                            if (black_white=True) or (impact>0)
                               then begin
                                      Brush.Color:=clWhite;
                                      Brush.Style:=bsSolid;       // solid infill white.
                                    end
                               else begin
                                      if (rail=16) or (rail=20)   // 0.93.a platforms
                                         then begin
                                                Brush.Color:=printplat_infill_colour;

                                                case print_platform_infill_style of
                                                        0: Brush.Style:=bsClear;
                                                        1: Brush.Style:=bsBDiagonal;    // hatched. backward diagonal (forward diagonal on control template timbers).
                                                        2: Brush.Style:=bsDiagCross;

                                                        3: if mapping_colours_print<0          // solid.
                                                              then Brush.Style:=bsBDiagonal    // impact printer or plotter, or printing black and white or in a single colour.
                                                              else Brush.Style:=bsSolid;

                                                      else begin                         // 4 = blank.
                                                             Brush.Style:=bsSolid;
                                                             Brush.Color:=clWhite;       // overide.
                                                           end;

                                                end;//case
                                              end
                                         else begin
                                                if ((draw_ts_trackbed_cess_edge=True) and (rail=18))
                                                or ((draw_ms_trackbed_cess_edge=True) and (rail=22))   // 215a
                                                   then begin
                                                          Brush.Color:=sb_track_bgnd_colour; // cess use same colour as track background
                                                          Brush.Style:=bsFDiagonal;
                                                        end
                                                   else begin   // normal rails...

                                                          Brush.Color:=printrail_infill_colour_cu;

                                                          case rail_infill_i of
                                                                1: Brush.Style:=bsBDiagonal;   // hatched
                                                                2: Brush.Style:=bsSolid;       // solid
                                                                3: Brush.Style:=bsDiagCross;   // cross_hatched
                                                                4: begin                       // blank
                                                                     Brush.Style:=bsSolid;
                                                                     Brush.Color:=clWhite;
                                                                   end;
                                                              else Brush.Style:=bsSolid;       // ??? solid
                                                          end;//case

                                                        end;
                                              end;
                                    end;

                            if dots_index>2
                               then begin
                                      Polygon(Slice(dots,dots_index+1));   // +1, number of points, not index.  must have 4 points.

                                      edge_colour:=Pen.Color;  // existing rail edges.

                                      if Brush.Style=bsSolid
                                         then blanking_colour:=Brush.Color       // infill colour.
                                         else blanking_colour:=clWhite;          // assume white background (print, PDF)

                                            // remove polygon line across end of planing (not for fixed-diamond)..
                                            // (for gaunt template this removes the polygon line across the rail end)

                                      if ((half_diamond=False) or (fixed_diamond=False)) and ((rail=1) or (rail=2)) then modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                            // remove polygon lines across stock rail ends...
                                            // and trackbed ends  206b

                                      if (rail=0) or (rail=3) or (rail=18) or (rail=22)   // 18,22 added 206b
                                         then begin
                                                modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // toe or approach end.

                                                modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);  // exit end.
                                              end;

                                      if adjacent_edges=True  // platforms
                                       then begin

                                                   // 0.93.a blank platform rear edges ...

                                              if (rail=16) and (draw_ts_platform=True) and (draw_ts_platform_rear_edge=False)   // 0.93.a TS platform start
                                                 then draw_outline_railedge(16,blanking_colour);    // blank rear edge

                                              if (rail=20) and (draw_ms_platform=True) and (draw_ms_platform_rear_edge=False)   // 0.93.a TS platform start
                                                 then draw_outline_railedge(20,blanking_colour);    // blank rear edge

                                                   // 0.93.a blank platform ends ...

                                              if (rail=16) and (draw_ts_platform=True) and (draw_ts_platform_start_edge=False)   // 0.93.a TS platform start
                                                 then modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=16) and (draw_ts_platform=True) and (draw_ts_platform_end_edge=False)     // 0.93.a TS platform end
                                                 then modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform=True) and (draw_ms_platform_start_edge=False)   // 0.93.a MS platform start
                                                 then modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform=True) and (draw_ms_platform_end_edge=False)     // 0.93.a MS platform end
                                                 then modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                            end;

                                      if (rail=26) or (rail=28)
                                         then begin
                                                modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // centre of K-crossing check rails.
                                              end;

                                    end;
                          end;
                end;//with
              end;
              //////////////////////////////////

              procedure draw_fill_vee;    // do complete vee in one go ...

              const
                dots_max_c=xy_pts_c*2;

              var
                dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                  //    (xy_pts_c=3000;)
                  //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                  //   template max is 4500' scale length.
                  //   ( = 18000 mm or 59ft approx in 4 mm scale).
                  //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                  // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                  // because needed for the Polygon function.

                  // total memory = 6000*8 bytes = 48kb.

                now:integer;
                edge_started:boolean;
                dots_index:integer;
                x_dots,y_dots:integer;
                aq:integer;
                point_mid_dots_index, splice_mid_dots_index:integer;
                edge_colour, blanking_colour:integer;

                              //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                              procedure modify_vee_end(start_index,stop_index,edge,blank:integer);

                              begin

                                if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                   then begin
                                          move_to:=dots[start_index];
                                          line_to:=dots[stop_index];

                                          if check_limits(move_to, line_to)=True
                                             then begin
                                                    with Printer.Canvas do begin
                                                      Pen.Color:=blank;                // first blank across..
                                                      MoveTo(move_to.X, move_to.Y);
                                                      LineTo(line_to.X, line_to.Y);

                                                      Pen.Color:=edge;                 // then restore the corner points..
                                                      MoveTo(move_to.X, move_to.Y);
                                                      LineTo(move_to.X, move_to.Y);

                                                      MoveTo(line_to.X, line_to.Y);
                                                      LineTo(line_to.X, line_to.Y);
                                                    end;//with
                                                  end;
                                        end;
                              end;
                              //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


              begin
                if plain_track=True then EXIT;   // not if plain track.

                if (aqyn[4]=False) or (aqyn[5]=False) or (aqyn[12]=False) or (aqyn[13]=False)            // not enough data for filled vee.
                or (nlmax_array[4]=0) or (nlmax_array[5]=0) or (nlmax_array[12]=0) or (nlmax_array[13]=0)
                   then begin
                          if aqyn[4]=True then draw_outline_railedge(4,printcurail_colour);       // draw outline vee...
                          if aqyn[5]=True then draw_outline_railedge(5,printcurail_colour);
                          if aqyn[12]=True then draw_outline_railedge(12,printcurail_colour);
                          if aqyn[13]=True then draw_outline_railedge(13,printcurail_colour);
                        end
                   else begin
                          dots_index:=0-1;   // first increment is to zero.

                          aq:=4;
                          edge_started:=False;
                          for now:=0 to nlmax_array[aq] do    // vee main-side, gauge_face, start from the tip.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          point_mid_dots_index:=dots_index;

                          aq:=12;
                          edge_started:=False;
                          for now:=nlmax_array[aq] downto 0 do  // back along outer-edge.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          aq:=13;
                          edge_started:=False;
                          for now:=0 to nlmax_array[aq] do    // and then turnout side outer edge.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          splice_mid_dots_index:=dots_index;

                          aq:=5;
                          edge_started:=False;
                          for now:=nlmax_array[aq] downto 0 do  // and back along the gauge face to the tip.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          with Printer.Canvas do begin

                            Pen.Color:=printcurail_colour;         //  1 = virtual black. Bug in HP printer driver if black (0) specified.
                                                                   //  (but not on "econofast" print !).
                            Pen.Mode:=pmCopy;
                            Pen.Style:=psSolid;

                            Brush.Color:=printrail_infill_colour_cu;

                            if (black_white=True) or (impact>0)
                               then begin
                                      Brush.Style:=bsSolid;              // solid infill white.
                                      Brush.Color:=clWhite;
                                    end
                               else case rail_infill_i of
                                          1: Brush.Style:=bsBDiagonal;   // hatched
                                          2: Brush.Style:=bsSolid;       // solid
                                          3: Brush.Style:=bsDiagCross;   // cross_hatched
                                          4: begin                       // blank
                                               Brush.Style:=bsSolid;
                                               Brush.Color:=clWhite;
                                             end;
                                        else Brush.Style:=bsSolid;       // solid
                                    end;//case

                            if dots_index>4
                               then begin
                                      Polygon(Slice(dots,dots_index+1));   // +1, number of points, not index.  must have at least 5 points.

                                      edge_colour:=Pen.Color;  // existing rail edges.

                                      if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
                                                             else blanking_colour:=clWhite;

                                            // remove polygon lines across vee rail ends...

                                      modify_vee_end(point_mid_dots_index,point_mid_dots_index+1,edge_colour,blanking_colour); // point rail end.

                                      modify_vee_end(splice_mid_dots_index,splice_mid_dots_index+1,edge_colour,blanking_colour); // splice rail end.
                                    end;

                          end;//with Canvas
                        end;
              end;
              ////////////////////////////////////////////////////////////////////////

              procedure mark_end(aq1, aq1end, aq2, aq2end:integer);      // make a rail end mark

              begin
                if (endmarks_yn[aq1,aq1end]=True) and (endmarks_yn[aq2,aq2end]=True)
                   then begin
                          p1:=endmarks[aq1,aq1end];
                          p2:=endmarks[aq2,aq2end];


                          with sheet[sheet_down,sheet_across] do begin
                            move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;  move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;
                            line_to.X:=Round((p2.Y+ypd-grid_left)*scaw_out)+page_left_dots;  line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;
                          end;//with

                          with Printer.Canvas do begin
                            Pen.Color:=printcurail_colour;         //  1 = virtual black. Bug in HP driver if black (0) specified.
                                                                   //  (but not on "econofast" print !).
                            Pen.Mode:=pmCopy;
                            Pen.Style:=psSolid;

                            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                          end;//with
                        end;
              end;
              ////////////////////////////////////////////////////////////

              procedure outline_railends;       // draw the rail ends in outline mode.

              begin
                if plain_track=False
                   then begin                                       // mark rail-ends...

                          mark_end(1,1,9,1);    // turnout rail wing rail finish.
                          mark_end(2,1,10,1);   // main rail wing rail finish.

                          mark_end(6,0,14,0);   // main side check rail start.
                          mark_end(6,1,14,1);   // main side check rail finish.

                          mark_end(7,0,15,0);   // turnout side check rail start.
                          mark_end(7,1,15,1);   // turnout side check rail finish.

                          mark_end(4,0,5,0);    // blunt nose.

                          if (half_diamond=True) and (fixed_diamond=True)     // planed faced of point rails for a fixed-diamond.
                             then begin
                                    mark_end(1,0,9,0);
                                    mark_end(2,0,10,0);

                                    mark_end(26,1,27,1);     // MS K-crossing check rails.
                                    mark_end(28,1,29,1);     // DS K-crossing check rails.
                                  end;
                        end;
              end;
              //////////////////////////////////////////////////////////////////

              procedure pause_banner;
                                            // 0.93.a   pause banner ..
              begin
                if (banner_paper=True) and (all_pages=True) and (sheet_across=max_sheet_across) and (sheet_down=max_sheet_down[max_sheet_across]) and (print_form.banner_pause_final_page_checkbox.Checked=True)
                   then ShowMessage('Banner printing.'+#13+#13+'Now paused before sending page '+Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1)+' to the printer.'
                           +#13+#13+'This will be the final printed page.'
                           +#13+#13+'Click OK when ready to send it to the printer.');
              end;
              //////////////////////////////////////////////////////////////////

              procedure print_shapes_and_sketchboard_items(grid_left,grid_top:extended);    // 206e

              begin
                if bgnd_form.output_shapes_in_front_of_sb_checkbox.Checked=True
                   then print_sketchboard_items(Printer.Canvas,grid_left,grid_top);  // 206e

                print_bgnd_shapes(grid_left,grid_top);    // print all background shapes

                if bgnd_form.output_shapes_in_front_of_sb_checkbox.Checked=False
                   then print_sketchboard_items(Printer.Canvas,grid_left,grid_top);  // 206e

              end;
              //////////////////////////////////////////////////////////////////


begin
  RESULT:=False;          // init default.

  print_colours_setup;    // first set up the colours.

try
  123:                            // may want to come back if banner infill changes.

  if Printer.Printing=True
     then begin
            alert(6,'    printer  busy',
                    '||||The printer is currently in use.'
                   +'||Please try again later.',
                    '','','','','','O K',0);
            EXIT;
          end
     else begin
            printer_printing:=False;             // init for new job.
            print_form.in_progress_label.Hide;
          end;

  print_busy:=True;                  // lock-out the loop while printing.
  Printer.Title:='Templot Pages';

  if calcs_done_and_valid=False then redraw(False);    //  first do a direct redraw if nec to ensure valid calcs.
  if calcs_done_and_valid=False then EXIT;             //  calcs still not valid.

  if get_prindex(prindex)=False then EXIT;                           // get current printer index or none available.

  preview_record_file_made:=False;   // 214b   init

  print_form.printer_label.Caption:=printer_list.Strings[prindex];   // put printer name on print form.
  cur_cal:=Tprint_cal(printer_list.Objects[prindex]).cal_data;
  cal_ok:=cur_cal.printer_calibrated;                          // flag for warning needed.
  impact:=cur_cal.printer_impact;                              // use thin lines for impact printer or plotter.

  if banner_paper=True
     then begin
            print_form.banner_fill_checkbox.Enabled:=True;               // might have been disabled if this is a change.
            print_form.banner_pause_final_page_checkbox.Enabled:=True;   // 0.93.a
          end;

  if print_preview(False,False,0)=False  // calc the sheet sizes from the printer and preview the output.
                                         // (also gets ypd y datum offset).
     then EXIT;

  print_line_thickness_setup;  // needs the dpi via print_preview.  mod 0.73.a 12-9-01.

  gridx:=grid_spacex*100;  //gridsizex*2540;   // grid line spacings. in 1/100th mm.  (any output scaling is done later).
  gridy:=grid_spacey*100;  //gridsizey*2540;

  while gridx*out_factor<500 do gridx:=gridx*2;      // 5 mm closest grid spacing permitted down the page.
  while gridy*out_factor<1000 do gridy:=gridy*2;     // 10 mm ditto across page to allow for labels.

  all_pages:=False;        // init for one page at a time.

  slow_run:=0;                                            // cancel any slow-running.
  control_room_form.run_slow_menu_entry.Checked:=False;

  info_str:='';                                        // init for info show...
  print_form.info_scrollbox.VertScrollBar.Position:=0;

  banner_top_done:=False;
                                       //  now ready to start printing ...
  with Printer do begin
    with Canvas do begin

      Font.Assign(print_labels_font);         // for labels

      page_count:=0;


               // check every sheet for any content, all 858 sheets (26*33)...

      max_sheet_across:=0;  // init

      for sheet_across:=0 to sheet_across_c do begin
        info_str:=info_str+'in row '+Chr(sheet_across+97)+' :   ';
        row_count:=0;

        max_sheet_down[sheet_across]:=0;  // 0.93.a  init

        for sheet_down:=0 to sheet_down_c do begin              // every sheet in row.

            if sheet[sheet_down,sheet_across].empty=True then CONTINUE;
            Inc(page_count);

            max_sheet_across:=sheet_across;  // keep track of highest non-empty page row.

            max_sheet_down[sheet_across]:=sheet_down;  // 0.93.a keep track of highest non-empty page in each row.

            page_num_str:=Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1);
            if row_count=10
               then begin
                      info_str:=info_str+'|                  ';   // add another line for every 10 page nunbers in a row.
                      row_count:=0;
                    end;

            info_str:=info_str+page_num_str+'  ';
            Inc(row_count);

        end;//next down
        info_str:=info_str+'||';             // new line for each row of pages.
      end;//next across

      if page_count<1 then page_count:=1;    // how did that happen?

      if page_count>1 then pgco_str:=''+IntToStr(page_count)+'  pages'
                      else pgco_str:='one  page';

      if banner_paper=True then pgco_str:=pgco_str+'  of  banner  paper :'
                           else pgco_str:=pgco_str+' :';

      with print_form do begin

        if print_entire_pad_flag=True
           then begin
                  if print_group_only_flag=True
                     then header_label.Caption:='print group templates only     scaled at : '+round_str(out_factor*100,2)+' %'
                     else header_label.Caption:='print background templates     scaled at : '+round_str(out_factor*100,2)+' %';
                end
           else header_label.Caption:=Trim(gauge_str)+'  '+round_str(scale,2)+' mm/ft     scaled at : '+round_str(out_factor*100,2)+' %';

        page_label.Caption:=pgco_str;
        printer_info_label.Caption:=insert_crlf_str(info_str);

        origin_label.Caption:='page origin :  X = '+round_str(print_pages_top_origin,2)+' mm    Y = '+round_str(print_pages_left_origin,2)+' mm';

      end;//with
      //---------------------------------------------------

      print_form.row_progressbar.Max:=(max_sheet_across+1)*(sheet_down_c+1);
      print_form.row_progressbar.Position:=0;

      for sheet_across:=0 to max_sheet_across do begin
        for sheet_down:=0 to sheet_down_c do begin              // every sheet in row (all 33).

          print_form.row_progressbar.StepIt;

          with sheet[sheet_down,sheet_across] do begin

            if empty=True then CONTINUE;

            case xing_type_i of
                      0: xing_str:=' regular V-crossing';
                      1: xing_str:=' curviform V-crossing';
                     -1: xing_str:=' generic V-crossing';
                    else xing_str:='';
            end;//case

            page_num_str:=Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1);


            page_str:=' page  '+page_num_str+'  ';

            if print_entire_pad_flag=True
               then bottom_str:=' of  '+IntToStr(page_count)+'  pages for  '+box_project_title_str
               else begin
                      bottom_str:=' ref :  '+current_name_str+'    '+info_form.gauge_label.Caption;

                      if plain_track=False then bottom_str:=bottom_str+xing_str;

                      if (ABS(nomrad)<max_rad_test) or (spiral=True)
                         then begin
                                if spiral=False then bottom_str:=bottom_str+'    curved onto '+round_str(nomrad,0)+' mm radius'
                                                else bottom_str:=bottom_str+'    spiral transition curve';
                              end
                         else bottom_str:=bottom_str+'    straight (no curving)';
                    end;

            if out_factor<>1 then bottom_str:=bottom_str+'    scaled at '+round_str(out_factor*100,2)+' %';   // page number and scaling.

            bottom_str:=bottom_str+'    '+DateToStr(Date)+'  '+TimeToStr(Time);

            top_str:=' Templot3  v:'+GetVersionString(voFull)+'  templot.com  This drawing contains design elements and data © 2018 Martin Wynne.';
            if box_project_title_str<>'' then top_str:=top_str+'   Project : '+box_project_title_str;

            temp_str:=Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1);
            print_form.page_panel.Caption:='next  page  is   '+temp_str;
            print_form.ok_button.Caption:='&print  page  '+temp_str;
            print_form.omit_page_button.Caption:='&omit  page  '+temp_str;

            if all_pages=False
               then begin
                      button_clicked:=False;
                      banner_changed:=False;
                      enable_buttons(sheet_across<max_sheet_across);


                      if print_now_bang=True   // 0.93.a  simulate clicking "print all"
                         then begin
                                print_now_form.print_data_sent_label.Hide;
                                if print_now_form.dont_show_again_checkbox.Checked=False then print_now_form.Show;

                                if preview_record_file_made=False   // 214b   first page only
                                   then begin
                                          make_print_preview_screenshot;
                                          preview_record_file_made:=True;
                                        end;

                                Application.ProcessMessages;

                                button_clicked:=True;
                                disable_buttons;
                                all_pages:=True;
                                if printer_printing=True
                                   then NewPage
                                   else begin
                                          begin_doc;
                                          RESULT:=True;
                                        end;


                              end
                         else begin

                                print_form.Show;
                                print_form.BringToFront;

                                if Application.Terminated=False then Application.ProcessMessages; // otherwise first red outline doesn't show properly.
                                outline_in_red(sheet_down,sheet_across); // show which sheet we mean.

                                repeat
                                  if Application.Terminated=False then Application.ProcessMessages
                                                                  else BREAK;
                                until button_clicked=True;

                                disable_buttons;

                                if Application.Terminated=False then Application.ProcessMessages;

                                if (banner_paper=True) and (banner_changed=True)        // switch banner infill on-off
                                   then begin
                                          if printer_printing=True then end_doc(True);
                                          goto 123;                                     // re start.
                                        end;

                                case print_form.ModalResult of

                                  mrIgnore: CONTINUE;     // omit page - do next down

                                   mrRetry: BREAK;        // omit row - do next across

                                      mrOk: begin
                                              print_form.page_panel.Caption:='preparing  page   '+temp_str+'  ...';

                                              if preview_record_file_made=False   // 214b   first page only
                                                 then begin
                                                        make_print_preview_screenshot;
                                                        preview_record_file_made:=True;
                                                      end;

                                              if printer_printing=True then NewPage             // start new page,
                                                                       else begin
                                                                              begin_doc;          // or the first.
                                                                              RESULT:=True;
                                                                            end;
                                            end;

                                  mrCancel: begin
                                              if printer_printing=True then end_doc(True);
                                              EXIT;
                                            end;

                                     mrYes: begin
                                              all_pages:=True;
                                              print_form.in_progress_label.Show;
                                              print_form.page_panel.Caption:='preparing  pages';

                                              pause_banner;

                                              if preview_record_file_made=False   // 214b   first page only
                                                 then begin
                                                        make_print_preview_screenshot;
                                                        preview_record_file_made:=True;
                                                      end;

                                              if printer_printing=True then NewPage
                                                                       else begin
                                                                              begin_doc;
                                                                              RESULT:=True;
                                                                            end;
                                            end;
                                       else run_error(31);
                                end;//case

                              end;
                    end
               else begin
                      if Application.Terminated=False then Application.ProcessMessages;

                      if (print_now_bang=False) and (print_form.ModalResult=mrCancel)   // he's aborted all pages on F12 or Esc
                         then begin
                                if printer_printing=True then end_doc(False);
                                EXIT;
                              end;

                      pause_banner;

                      if preview_record_file_made=False   // 214b   first page only
                         then begin
                                make_print_preview_screenshot;
                                preview_record_file_made:=True;
                              end;

                      if printer_printing=True then NewPage             // start new page,
                                               else begin
                                                      begin_doc;        // or the first.
                                                      RESULT:=True;
                                                    end;
                    end;

            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;

            FillRect(Rect(0,0,printer_width_indexmax_dots,printer_length_indexmax_dots)); //  this clears the canvas.

            TextOut(0,0,'');      // !!! Delphi bug? This seems to be necessary before dotted lines will draw properly.
                                  // TextOut obviously initialises some background mask property which I have been unable
                                  // to find or set any other way.

            if (print_form.page_ident_checkbox.Checked=True) and (page_count>3)  // 214a show large page ident if many pages
               then begin
                      Brush.Style:=bsClear;

                      Font.Name:='Courier New';
                      Font.Height:=0-ABS((page_left_dots-page_right_dots)*9 div 23);  // 9/23 trial and error for this font up to z/99

                      if black_white=True
                         then begin
                                wm_shift:=1;             // watermark outline shift
                                Font.Color:=clBlack;
                              end
                         else begin
                                wm_shift:=Round(nom_width_dpi/30);   // watermark outline shift /30 arbitrary
                                Font.Color:=$00D0D0D0;               // pale-ish grey
                              end;

                      ident_left:=Round(nom_width_dpi/10)+page_left_dots+(page_right_dots-page_left_dots-TextWidth(page_num_str)) div 2;      // nom_width_dpi/10   arbitrary for neatness
                      ident_top:=page_top_dots+(page_bottom_dots-page_top_dots+Font.Height) div 2;

                      TextOut(ident_left-wm_shift,ident_top-wm_shift,page_num_str);
                      TextOut(ident_left+wm_shift,ident_top-wm_shift,page_num_str);
                      TextOut(ident_left-wm_shift,ident_top+wm_shift,page_num_str);
                      TextOut(ident_left+wm_shift,ident_top+wm_shift,page_num_str);

                      Font.Color:=clWhite;
                      TextOut(ident_left,ident_top,page_num_str);        // ink saving, make watermark outline

                      Brush.Style:=bsSolid;  // reset..
                      Font.Color:=clBlack;
                    end;

            if bgnd_form.output_grid_in_front_checkbox.Checked=True            // do shapes and sb first
               then print_shapes_and_sketchboard_items(grid_left,grid_top);    // 206e

                                 //  draw grid and trim margins ...

            Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.   // 206e moved here
            Brush.Style:=bsSolid;

            Font.Assign(print_labels_font);

            if printgrid_i=1
               then begin
                      case grid_labels_code_i of
                                        1: grid_str:='feet';     //  labels in feet.
                                        2: grid_str:='inches';   //  labels in inches.
                                        3: grid_str:='proto-feet'; //  labels in prototype feet.
                                        4: grid_str:='cm';       //  labels in cm.
                                        6: grid_str:='mm';       //  labels in mm.
                                      else run_error(213);
                      end;//case

                      Pen.Color:=printgrid_colour;           // for grid lines.
                      Pen.Mode:=pmCopy;

                      if pad_form.printed_grid_dotted_menu_entry.Checked=True
                         then begin
                                Pen.Style:=psDot;
                                pen_width:=1;         // must be 1 for dots.
                              end
                         else begin
                                Pen.Style:=psSolid;
                                if impact>0 then pen_width:=1                   // impact printer or plotter.
                                            else pen_width:=printgrid_wide;
                                if pen_width<1 then pen_width:=1;
                              end;

                         //  draw horizontal grid lines (across width)...

                      if (banner_paper=True) or (print_pages_top_origin<>0)
                         then now_gridx:=0-gridx
                         else now_gridx:=0;        //  init grid lines. no need for first line (gets overwritten by trim margins).

                      repeat
                        now_gridx:=now_gridx+gridx;
                        grid_now_dots:=Round((now_gridx-grid_top)*scal_out)+page_top_dots;
                        if grid_now_dots<0 then CONTINUE;

                        if (now_gridx=0) and (Pen.Style=psSolid)
                           then Pen.Width:=pen_width+2    // thicker datum line (only appears if page origin is negative).
                           else Pen.Width:=pen_width;

                        move_to.X:=left_blanking_dots;          move_to.Y:=grid_now_dots;
                        line_to.X:=printer_width_indexmax_dots; line_to.Y:=grid_now_dots;
                        if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                        case grid_labels_code_i of
                                          1: grid_label:=now_gridx/30480;       //  labels in feet.
                                          2: grid_label:=now_gridx/2540;        //  labels in inches.
                                          3: grid_label:=now_gridx/(100*scale); //  labels in prototype feet.
                                          4: grid_label:=now_gridx/1000;        //  labels in cm.
                                          6: grid_label:=now_gridx/100;         //  labels in mm.
                                        else begin
                                               grid_label:=0;   // keep the compiler happy.
                                               run_error(223);
                                             end;
                        end;//case

                        grid_label_str:=FormatFloat('0.###',grid_label);

                        TextOut(left_blanking_dots,grid_now_dots-(TextHeight('A') div 2),grid_label_str+' '); //  add labels.

                      until grid_now_dots>page_bottom_dots;

                             //  draw vertical grid lines (down length)...

                      if print_pages_left_origin<>0
                         then now_gridy:=0-gridy
                         else now_gridy:=0;        //  init grid lines. no need for first line (gets overwritten by trim margin).

                      repeat
                        now_gridy:=now_gridy+gridy;
                        grid_now_dots:=Round((now_gridy-grid_left)*scaw_out)+page_left_dots;
                        if grid_now_dots<0 then CONTINUE;

                        if (now_gridy=0) and (Pen.Style=psSolid)
                           then Pen.Width:=pen_width+2    // thicker datum line (only appears if page origin is negative).
                           else Pen.Width:=pen_width;

                        move_to.X:=grid_now_dots; move_to.Y:=top_blanking_dots;
                        line_to.X:=grid_now_dots; line_to.Y:=printer_length_indexmax_dots;
                        if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                        case grid_labels_code_i of
                                          1: grid_label:=now_gridy/30480;       //  labels in feet.
                                          2: grid_label:=now_gridy/2540;        //  labels in inches.
                                          3: grid_label:=now_gridy/(100*scale); //  labels in prototype feet.
                                          4: grid_label:=now_gridy/1000;        //  labels in cm.
                                          6: grid_label:=now_gridy/100;         //  labels in mm.
                                        else begin
                                               grid_label:=0;   // keep the compiler happy.
                                               run_error(224);
                                             end;
                        end;//case

                        grid_label_str:=FormatFloat('0.###',grid_label);

                        if banner_paper=False
                           then TextOut(grid_now_dots-(TextWidth(grid_label_str) div 2),page_top_dots-(printmargin_wide div 2)-halfmm_dots-TextHeight('A'),grid_label_str) //  add labels.
                           else begin
                                  if (sheet_down mod 2)=0     // alternate pages only for banners.
                                     then begin
                                            if staggered_pages=True
                                               then begin
                                                      if (sheet_across mod 2)=0                 // keep grid labels in line across rows.
                                                         then bangrid_dots:=page_quarter_dots
                                                         else bangrid_dots:=page_3quarter_dots;
                                                    end
                                               else bangrid_dots:=page_mid_dots;  // pages not staggered.

                                            bangrid_dots:=bangrid_dots+TextHeight('A');  // to avoid obstructing the alignment marks.

                                            TextOut(grid_now_dots-(TextWidth(grid_label_str) div 2),bangrid_dots,grid_label_str);
                                          end;
                                end;

                      until grid_now_dots>page_right_dots;

                                // finally add the units string...

                      if banner_paper=False then TextOut(left_blanking_dots,page_top_dots-(printmargin_wide div 2)-halfmm_dots-TextHeight('A'),grid_str)  // add the units string.
                                            else begin
                                                  if (sheet_down mod 2)=0     // alternate pages only for banners.
                                                     then begin
                                                            if staggered_pages=True
                                                               then begin
                                                                      if (sheet_across mod 2)=0                     // keep grid labels in line across rows.
                                                                         then bangrid_dots:=page_quarter_dots
                                                                         else bangrid_dots:=page_3quarter_dots;
                                                                    end
                                                               else bangrid_dots:=page_mid_dots;   // pages not staggered.

                                                            bangrid_dots:=bangrid_dots+TextHeight('A');  // to avoid obstructing the alignment marks.

                                                            TextOut(left_blanking_dots,bangrid_dots,grid_str);
                                                          end;
                                                 end;
                      Pen.Style:=psSolid;  // reset in case of dotted.
                    end;

                    // grid finished.

                //----------------------------------------

            if bgnd_form.output_grid_in_front_checkbox.Checked=False           // now do shapes and sb over the grid
               then print_shapes_and_sketchboard_items(grid_left,grid_top);    // 206e

            print_bgnd(grid_left,grid_top);       // now print any background templates.


                          //  control template - draw timbers and all marks except rail joints...

            if (print_entire_pad_flag=False) // printing the control template
            and (output_diagram_mode=False)  // 0.93.a  no control template if diagram mode
            and (turnoutx>0)                 // no data if invalid template

                  // 0.93.a if printing background templates in Quick mode, the control template has been put on the background

               then begin

                      if marks_list_ptr=nil then BREAK;       // pointer to marks list not valid, exit all sheets.

                      draw_marks(grid_left,grid_top,False);   // print all the background timbering and marks except rail joints.

                        if ( (print_settings_form.output_centrelines_checkbox.Checked=True) and (dummy_template=False) )       // 212a
                        or ( (print_settings_form.output_bgnd_shapes_checkbox.Checked=True) and (dummy_template=True) )

                           then begin

                                  Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                                  Brush.Style:=bsClear;
                                  TextOut(0,0,'');

                                  Pen.Mode:=pmCopy;

                                  if dummy_template=True   // 212a
                                     then begin
                                            Pen.Style:=psSolid;
                                            Pen.Color:=printshape_colour;

                                            if impact>0 then Pen.Width:=1                 // impact printer or plotter.
                                                        else Pen.Width:=printshape_wide;

                                            if Pen.Width<1 then Pen.Width:=1;
                                          end
                                     else begin
                                            Pen.Color:=printcurail_colour;

                                            if impact>0 then Pen.Width:=1                // impact printer or plotter.
                                                        else Pen.Width:=printcl_wide;

                                            if Pen.Width<1 then Pen.Width:=1;

                                            if Pen.Width=1 then Pen.Style:=psDash
                                                           else Pen.Style:=psSolid;

                                          end;

                                for aq:=24 to 25 do begin
                                  if ( (plain_track=False) or (aq=24) ) and (aqyn[aq]=True)

                                          // main side only only if plain track, and data available ?

                                     then begin
                                            move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                                            for now:=1 to nlmax_array[aq] do begin
                                              line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                                              if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                              move_to:=line_to;
                                            end;//for
                                          end;
                                end;//for-next aq
                              end;//if track centre-lines.

                      if print_settings_form.output_rails_checkbox.Checked=True
                         then begin

                                //drawn_full_aq1:=True;    // default init flags.
                                //drawn_full_aq2:=True;

                                            //  draw turnout rails...

                                if impact>0 then Pen.Width:=1        // impact printer or plotter.
                                            else begin
                                                    {if out_factor<1  //  scale down the line width.

                                                       then Pen.Width:=Round(printrail_thick*out_factor)
                                                       else}
                                                    // out 0.73.a 12-8-01 (now done in thickness setup)

                                                    Pen.Width:=printrail_wide;
                                                    if Pen.Width<1 then Pen.Width:=1;
                                                 end;

                                if (rail_infill_i=0) or ((scale*out_factor)<0.1)  // 0.93.a was 0.75 // less than 2.5% for 4mm scale (control template) (1.43% for 7mm).
                                   then begin           //  outline (pen) mode ...
                                                        //  n.b. this mode does not automatically close the rail-ends.

                                          for aq:=0 to 23 do begin                                // 24, 25 centre-lines already done.
                                            if (adjacent_edges=False) and (aq>15) then CONTINUE;  // no adjacent tracks in output  // 206b
                                            draw_outline_railedge(aq,printcurail_colour);
                                          end;//next aq

                                          for aq:=26 to aq_max_c do draw_outline_railedge(aq,printcurail_colour);  // K-crossing check rails.

                                          outline_railends;     // finally do the rail ends for outline mode
                                        end
                                   else begin      // infill (polygon) mode ...

                                                   // do blades first - neater result.

                                          for rail:=1 to 3 do draw_fill_rail(8);  // closure rails and curved stock rail.

                                          rail:=0;                                // straight stock rail.
                                          draw_fill_rail(8);

                                          for rail:=6 to 7 do draw_fill_rail(8);  // check rails

                                          if adjacent_edges=True    // 206b
                                             then begin
                                                    rail:=16;
                                                    repeat
                                                      draw_fill_rail(1);   // platforms and trackbed edges
                                                      rail:=rail+2;
                                                    until rail>22;
                                                  end;

                                          rail:=26;
                                          repeat
                                            draw_fill_rail(1);      // K-crossing MS check rails.
                                            rail:=rail+2;
                                          until rail>28;

                                          draw_fill_vee;   // now do the vee.

                                                    // finally draw in or overdraw the planing gauge-faces - (no infill) ...
                                          aq:=1;
                                          if (plain_track=False) and (gaunt=False) and (aqyn[1]=True) and (list_planing_mark_aq1>0) {and (drawn_full_aq1=False)}    // not if already drawn.
                                             then begin
                                                    move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                                                    for now:=1 to list_planing_mark_aq1{+1} do begin                    // +1 to overdraw
                                                      line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                                                      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                                      move_to:=line_to;
                                                    end;//for
                                                  end;

                                          aq:=2;
                                          if (plain_track=False) and (gaunt=False) and (aqyn[2]=True)  and (list_planing_mark_aq2>0) {and (drawn_full_aq2=False)}    // not if already drawn.
                                             then begin
                                                    move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                                                    for now:=1 to list_planing_mark_aq2{+1} do begin                      // +1 to overdraw
                                                      line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                                                      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                                      move_to:=line_to;
                                                    end;//for
                                                  end;

                                        end;//polygon mode

                                              // finally add rail joint marks across rails (will now mark over rail infill)...

                                draw_marks(grid_left,grid_top,True);

                              end;//if rails

                    end;// if control template

                    // now the trim margins....

            Pen.Color:=printmargin_colour;
            Pen.Mode:=pmCopy;
            Pen.Style:=psSolid;
            if impact>0 then Pen.Width:=1                  // impact printer or plotter.
                        else Pen.Width:=printmargin_wide;

            if banner_paper=False       // draw top trim margin...
               then begin
                      move_to.X:=left_blanking_dots;          move_to.Y:=page_top_dots;   // paper top left.
                      line_to.X:=printer_width_indexmax_dots; line_to.Y:=page_top_dots;   // paper top margin.
                      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                    end;

                                        // and right-hand alignment targets...

            move_to.X:=page_right_dots; move_to.Y:=top_blanking_dots;
            line_to.X:=page_right_dots; line_to.Y:=printer_length_indexmax_dots;                    // paper right margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            move_to.X:=page_right_dots-alignmarks_inner_dots;
            line_to.X:=printer_width_indexmax_dots;

            move_to.Y:=page_quarter_dots;  // right 1/4 target.
            line_to.Y:=page_quarter_dots;
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            move_to.Y:=page_mid_dots;      // right centre target.
            line_to.Y:=page_mid_dots;
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            move_to.Y:=page_3quarter_dots; // right 3/4 target.
            line_to.Y:=page_3quarter_dots;
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            if banner_paper=True
               then begin
                      move_to.Y:=page_top_dots; // right banner page break target.
                      line_to.Y:=page_top_dots;
                      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                    end;

                //  don't show bottom trim line on the last sheet down, otherwise he might trim off the info line,
                //  and no bottom trim lines on any sheet for banners, unless for multiple print runs.

            if (banner_paper=False) and (sheet_down<sheet_co_long)
               then begin
                      if sheet[sheet_down+1,sheet_across].empty=False   // something on next page down ?
                         then begin
                                move_to.X:=left_blanking_dots;          move_to.Y:=page_bottom_dots;
                                line_to.X:=printer_width_indexmax_dots; line_to.Y:=page_bottom_dots;    // paper bottom margin.
                                if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                               end;
                    end;
            if (want_bottom_margin=True) and (sheet_down=sheet_co_long)   // wanted for multiple print runs...
               then begin
                      move_to.X:=left_blanking_dots;          move_to.Y:=page_bottom_dots;
                      line_to.X:=printer_width_indexmax_dots; line_to.Y:=page_bottom_dots;    // paper bottom margin.
                      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                    end;

            move_to.X:=page_left_dots; move_to.Y:=top_blanking_dots;
            line_to.X:=page_left_dots; line_to.Y:=printer_length_indexmax_dots;               // paper left margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            move_to.X:=page_left_dots-alignmarks_inner_dots;   // default 3 mm each way.
            line_to.X:=page_left_dots+alignmarks_inner_dots;
            if move_to.X<left_blanking_dots then move_to.X:=left_blanking_dots;

            move_to.Y:=page_quarter_dots;    // left 1/4 target.
            line_to.Y:=page_quarter_dots;
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            move_to.Y:=page_mid_dots;        // left centre target.
            line_to.Y:=page_mid_dots;
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            move_to.Y:=page_3quarter_dots;   // left 3/4 target.
            line_to.Y:=page_3quarter_dots;
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            if banner_paper=True
               then begin
                      move_to.Y:=page_top_dots;   // left banner page break target.
                      line_to.Y:=page_top_dots;
                      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                    end;

                // trim margins finished.

            //----------------------

               //  now any margin blanking (outside the trim margins)...
               //  (+1 to ensure complete blanking on all printers.)

            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;

            Pen.Color:=clWhite;
            Pen.Style:=psSolid;
            Pen.Width:=1;
            Pen.Mode:=pmCopy;

            if left_blanking_dots>0 then Rectangle(0,0,left_blanking_dots,printer_length_indexmax_dots+1);
            if right_blanking_dots<=printer_width_indexmax_dots then Rectangle(right_blanking_dots,0,printer_width_indexmax_dots+1,printer_length_indexmax_dots+1);

            if (top_blanking_dots>0) and (banner_paper=False) then Rectangle(0,0,printer_width_indexmax_dots+1,top_blanking_dots);
            if (bottom_blanking_dots<=printer_length_indexmax_dots) and (banner_paper=False) then Rectangle(0,bottom_blanking_dots,printer_width_indexmax_dots+1,printer_length_indexmax_dots+1);

                     // top branding...

            Font.Assign(set_font('Arial',6,[],calc_intensity(clBlack)));

            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;

            if banner_paper=False
               then TextOut(left_blanking_dots,top_blanking_dots,top_str)  // name and "who for?" string at topleft.
               else begin
                      if banner_top_done=False  //sheet_down=0     // only once for banners.
                         then begin
                                banner_top_done:=True;
                                stry:=0-Font.Height*4;                       //  (Font.Height is negative).
                                TextOut(left_blanking_dots,stry,top_str);    // name and "who for?" string at top left.

                                stry:=0-Font.Height*7;
                                Font.Assign(print_labels_font);                              // font for bottom string.
                                Font.Color:=calc_intensity(clBlack);                                         // but in black.
                                if print_entire_pad_flag=True then Textout(left_blanking_dots,stry,' start'+bottom_str)  // add the bottom string also.
                                                              else Textout(left_blanking_dots,stry,' '+bottom_str);
                              end;
                    end;

                    // small text inside the margins...

                    // mods 208g  20-04-2014  show page origin dims on templates...


            if print_entire_pad_flag=True        // 214a  for Gordon, see Templot Club message ref: 19595   // background templates
               then begin
                      if keep_form.box_file_label.Caption<>''
                         then last_file_str:='  printing from: '+ExtractFileName(keep_form.box_file_label.Caption)
                         else last_file_str:='  printing background templates';
                    end
               else last_file_str:='  printing the control template';

            if pad_form.show_margin_coordinates_menu_entry.Checked=True
               then begin
                      all_pages_origin_str:='all pages origin (a/1): top(X)='+round_str(print_pages_top_origin,2)+'mm, left(Y)='+round_str(print_pages_left_origin,2)+'mm      ';     // 208g
                      this_page_begin_str:='this page begins: top(X)='+round_str(grid_top/100,2)+'mm, left(Y)='+round_str(grid_left/100,2)+'mm'+last_file_str;                        // 208g
                      this_page_end_str:='this page ends: bottom(X)='+round_str(grid_bottom/100,2)+'mm, right(Y)='+round_str(grid_right/100,2)+'mm';                                  // 208g
                    end
               else begin
                      all_pages_origin_str:='';
                      this_page_begin_str:=last_file_str;
                      this_page_end_str:='';
                    end;

            Font.Assign(print_corner_page_numbers_font);                           // 0.93.a
            if print_corner_page_numbers_font.Size>8 then Brush.Style:=bsClear;

            Textout(page_left_dots+printmargin_wide+3, page_top_dots+printmargin_wide+2, this_page_begin_str);                                                                                // top left corner
            Textout(page_left_dots+printmargin_wide+3, page_bottom_dots+Font.Height-printmargin_wide-4, page_num_str+'   '+box_project_title_str+'   '+DateToStr(Date)+' '+TimeToStr(Time));  // bottom left corner
            Textout(page_right_dots-printmargin_wide-TextWidth(all_pages_origin_str+page_num_str)-3,page_top_dots+printmargin_wide+2,all_pages_origin_str+page_num_str);                      // top right corner
            Textout(page_right_dots-printmargin_wide-TextWidth(this_page_end_str)-3,page_bottom_dots+Font.Height-printmargin_wide-4,this_page_end_str);                                       // bottom right corner

            if (cal_ok=False) and (print_form.warnings_checkbox.Checked=True)     //  no calibration done for this printer.
               then begin
                      Font.Assign(set_font('Arial',7,[],printmargin_colour));

                      Textout(page_left_dots+printmargin_wide,page_top_dots+printmargin_wide-(Font.Height*3),
                              '  Warning :  '+printer_list.Strings[prindex]+'  printer has not been calibrated.  This template may not be dimensionally accurate.  ( press Shift+F5 ).');
                    end;

            if (distortions<>0) and (print_form.warnings_checkbox.Checked=True)
               then begin
                      Font.Assign(set_font('Arial',7,[],printmargin_colour));

                      Textout(page_left_dots+printmargin_wide,page_top_dots+printmargin_wide-(Font.Height*5),
                              '  Warning :  Data distortions are in force.  This template may not be dimensionally accurate.');
                    end;

            Font.Assign(print_labels_font);  // reset for labels.

            Font.Color:=calc_intensity(clBlack);
            Brush.Color:=clWhite;
            Brush.Style:=bsClear;   // transparent over detail.

            if banner_paper=False then Textout(left_blanking_dots,page_bottom_dots+(printmargin_wide div 2)+halfmm_dots,page_str+bottom_str); // add the bottom string last.

            Font.Assign(print_labels_font);  // reset for labels.
            Brush.Style:=bsSolid;

          end;//with sheet

        end;//for-next sheet across
      end;//for-next sheet down

      if printer_printing=True then end_doc(True);      // last or only page.

    end;//with
  end;//with
finally

  Printer.Title:='Templot0';    // reset default for printing help texts, etc.
  print_busy:=False;
  enable_buttons(True);
  print_form.Close;
end;//try
end;
//______________________________________________________________________________

procedure show_output_mode_panel;

begin
  with pad_form.output_mode_panel do begin

    if output_diagram_mode=True
       then begin
              Color:=$00FF0080;             // violet
              Font.Color:=clWhite;
              Caption:='diagram  mode';
            end
       else begin
              Color:=clYellow;
              Font.Color:=clBlack;
              Caption:='detail  mode';
            end;

    Top:=pad_form.ClientHeight-200;
    Left:=(pad_form.ClientWidth-Width) div 2;
    Visible:=True;
  end;//with
end;
//______________________________________________________________________________

procedure Tprint_form.FormShow(Sender: TObject);

begin

  detail_mode_radiobutton.Checked:= NOT output_diagram_mode;
  diagram_mode_radiobutton.Checked:= output_diagram_mode;


  if info_form.Showing=True                // don't want the info cluttering the print preview.
     then begin
            info_was_showing:=True;
            pad_form.hide_info_menu_entry.Click;
          end
     else info_was_showing:=False;

  if panning_form.Showing=True             // nor the panning controls.
     then begin
            panning_was_showing:=True;
            panning_form.Hide;
          end
     else panning_was_showing:=False;

  if shove_timber_form.Showing=True        // nor the shove timbers form.
     then begin
            shove_was_showing:=True;
            shove_timber_form.Hide;
          end
     else shove_was_showing:=False;

  if grid_form.Showing=True                // nor the spacing ring form.
     then begin
            spacing_was_showing:=True;
            grid_form.Hide;
          end
     else spacing_was_showing:=False;

       // bugs fixed -- 208d ...

  if rail_options_form.Showing=True            // nor this
     then begin
            rail_options_was_showing:=True;
            rail_options_form.Hide;
          end
     else rail_options_was_showing:=False;

  if platform_form.Showing=True                // nor this
     then begin
            platform_was_showing:=True;
            platform_form.Hide;
          end
     else platform_was_showing:=False;

  if trackbed_form.Showing=True                // nor this
     then begin
            trackbed_was_showing:=True;
            trackbed_form.Hide;
          end
     else trackbed_was_showing:=False;

  if check_diffs_form.Showing=True             // nor this
     then begin
            check_diffs_was_showing:=True;
            check_diffs_form.Hide;
          end
     else check_diffs_was_showing:=False;

  if data_child_form.Showing=True              // nor this
     then begin
            data_child_was_showing:=True;
            data_child_form.Hide;
          end
     else data_child_was_showing:=False;

  if stay_visible_form.Showing=True            // nor this
     then begin
            stay_visible_was_showing:=True;
            stay_visible_form.Hide;
          end
     else stay_visible_was_showing:=False;

       //----------


  info_scrollbox.VertScrollBar.Position:=0;
  info_scrollbox.HorzScrollBar.Position:=0;
  if all_button.Enabled=True then all_button.SetFocus;

  if print_entire_pad_flag=True
    then begin
           if print_group_only_flag=True
              then Caption:='    print  group  templates  only'
              else Caption:='    print  background  templates';
           info_checkbox.Enabled:=False;
         end
    else begin
           Caption:='        print  the  control  template';
           info_checkbox.Enabled:=True;
         end;

  if fit_single_sheet=True then Caption:=Caption+'  on  a  single  page';

  if banner_paper=True
     then begin
            Caption:=Caption+'  on  banner  paper';
            banner_fill_checkbox.Enabled:=True;
            banner_pause_final_page_checkbox.Enabled:=True; // 0.93.a
          end
     else begin
            banner_fill_checkbox.Enabled:=False;
            banner_pause_final_page_checkbox.Enabled:=False;  // 0.93.a
          end;

  if black_white=True then no_intensity_panel.Visible:=True
                      else no_intensity_panel.Visible:=False;

  pad_form.top_toolbar_panel.Hide;
  pad_form.second_toolbar_panel.Hide;    // 217a

  make_slip_form.Hide;   // toolbars marker

  show_output_mode_panel;
end;
//_________________________________________________________________________________________

procedure Tprint_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key=VK_F10
     then begin
            Key:=0;      //  otherwise selects the menus.
          end;

  if Key=VK_PAUSE then Application.Minimize;        //  hide TEMPLOT on PAUSE key.

  if Key=VK_F12                  // same as cancel, but unlocks the redraw
     then begin
            Key:=0;
            print_form.omit_all_button.Click;
            pad_form.redraw_menu_entry.Click;       // unlock the redraw if locked.
            //Close;
          end;
end;
//________________________________________________________________________________________

procedure Tprint_form.colour_panelClick(Sender: TObject);

begin
  Color:=get_colour('choose  a  new  colour  for  the  print  information  dialog',Color);
  button_clicked:=False;     // overide the print form Deactivate.
end;
//_________________________________________________________________________________________

procedure Tprint_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  form_scaling:=True;    // no ScrollInView on resize.

  if size_updown.Position>size_updown.Tag                          // ! position goes up, size goes down.
     then ScaleBy(9,10);                                           // scale the form contents down.

  if size_updown.Position<size_updown.Tag
     then ScaleBy(10,9);                                           // scale the form contents up.

  ClientHeight:=VertScrollBar.Range;                               // allow 4 pixel right margin.
  ClientWidth:=HorzScrollBar.Range+4;                              // don't need bottom margin - datestamp label provides this.
  ClientHeight:=VertScrollBar.Range;                               // do this twice, as each affects the other.

  size_updown.Tag:=size_updown.Position;                           // and save for the next click.

  form_scaling:=False;
end;
//__________________________________________________________________________________________

procedure print_info(top_str, bottom_str:string);        // print the info page (cut sheets printing).
                                                         // also called from the info panel.
var
  prindex:integer;
  line_now:integer;
  pf:TextFile;                      // text file to be redirected to printer.
  prinor:TPrinterOrientation;
  left_margin_str:string;
  s:string;
  i:integer;

begin

  do_open_source_bang('PRINT INFO');  // OT-FIRST

(* OT-FIRST

  prinor:=Printer.Orientation;              // save his setting.
  Printer.Orientation:=poPortrait;

  page_info(True,True,False,0);        // get page width.
  if nom_width_dpi<1 then EXIT;        // ??? div zero.

  AssignPrn(pf);          // redirect file to printer.
  Rewrite(pf);            // open the file for writing.
  try
    with Printer.Canvas do begin
      if top_str<>''
         then begin
                Font.Assign(set_font('Arial',6,[],clBlack));

                left_margin_str:='';
                while TextWidth(left_margin_str)<Round(printer_text_left_margin*nom_width_dpi/25.4) do left_margin_str:=left_margin_str+' ';   // left margin approximated to spaces.

                WriteLn(pf,left_margin_str+top_str);     // print the top string first.
              end;

      WriteLn(pf,'');

      Font.Assign(printer_text_font);
      Font.Name:='Arial';                    // use Arial for heading.
      Font.Size:=Font.Size+2;                // and 2 sizes bigger.

      if get_prindex(prindex)=False then EXIT;                                                      // get current printer index or none available.
      if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact>0 then Font.Style:=[];   // not bold on impact printer.

      left_margin_str:='';
      while TextWidth(left_margin_str)<Round(printer_text_left_margin*nom_width_dpi/25.4) do left_margin_str:=left_margin_str+' ';   // left margin approximated to spaces.

      if box_project_title_str<>'' then WriteLn(pf,left_margin_str+Trim(info_form.gauge_label.Caption)+'   for : '+box_project_title_str)
                                   else WriteLn(pf,left_margin_str+Trim(info_form.gauge_label.Caption));
      WriteLn(pf,'');
      WriteLn(pf,left_margin_str+current_name_str);

      Font.Assign(printer_text_font);

      if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact>0 then Font.Style:=[];   // not bold on impact printer.

      left_margin_str:='';
      while TextWidth(left_margin_str)<Round(printer_text_left_margin*nom_width_dpi/25.4) do left_margin_str:=left_margin_str+' ';   // left margin approximated to spaces.

      WriteLn(pf,'');
      WriteLn(pf,left_margin_str+'            all dimensions in mm :');
      WriteLn(pf,'');

      if info_text_list{info_form.info_memo.Lines}.Count<1     // ???
         then WriteLn(pf,left_margin_str+'    blank')
         else for line_now:=0 to (info_text_list{info_form.info_memo.Lines}.Count-1) do WriteLn(pf,left_margin_str+info_text_list{info_form.info_memo.Lines}.Strings[line_now]);


      WriteLn(pf,left_margin_str+'---------------------------------------------------');

      Font.Name:=edit_memo_form.vis_edit_memo.Font.Name;
      Font.Style:=edit_memo_form.vis_edit_memo.Font.Style;

      //Font.Size:=info_form.user_memo.Font.Size;    // can't set printer size from a screen size - use existing font size.

      if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact>0 then Font.Style:=[];   // not bold or italic on impact printer.

      left_margin_str:='';
      while TextWidth(left_margin_str)<Round(printer_text_left_margin*nom_width_dpi/25.4) do left_margin_str:=left_margin_str+' ';   // left margin approximated to spaces.

      WriteLn(pf,left_margin_str+'  MEMO :');
      WriteLn(pf,'');

      s:=current_memo_str;        // mod 0.72 6-8-01...

      repeat                      //  find memo lines separated by | chars.
        i:=Pos('|',s);
        if i>0
           then begin
                  WriteLn(pf,Copy(s,1,i-1));    // extract a string line from the memo.
                  Delete(s,1,i);                // and delete it from the input string.
                end;
      until (i<1) or (Length(s)=0);

      {
      if info_form.user_memo.Lines.Count<1     // memo empty.
         then WriteLn(pf,left_margin_str+'    blank')
         else if Copy(info_form.user_memo.Lines[0],1,1)=' '  // alt+0160, memo has not been used.
                 then WriteLn(pf,left_margin_str+'    blank')
                 else for line_now:=0 to info_form.user_memo.Lines.Count-1 do WriteLn(pf,left_margin_str+info_form.user_memo.Lines[line_now]);
      }

      if bottom_str<>''
         then begin
                WriteLn(pf,left_margin_str+'---------------------------------------------------');
                Font.Assign(print_labels_font);
                //Font.Size:=Font.Size-1;
                Font.Color:=clBlack;

                left_margin_str:='';
                while TextWidth(left_margin_str)<Round(printer_text_left_margin*nom_width_dpi/25.4) do left_margin_str:=left_margin_str+' ';   // left margin approximated to spaces.

                WriteLn(pf,left_margin_str+bottom_str);               // add the bottom string last.
              end;
    end;//with
  finally
    CloseFile(pf);
    Printer.Orientation:=prinor;              // restore his setting.
  end;//try

*)

end;
//_________________________________________________________________________________________

procedure print_info_banner(top_str, bottom_str:string);  // print the info page on banner paper.
                                                          // (called while printing in progress)
var
  prindex:integer;
  line_now:integer;
  ydots:integer;
  text_left_margin_dots:integer;
  i:integer;
  s:string;

                         /////////////////////////////////////////////////////////////////////

                         procedure do_text(s:string);

                         begin
                           with Printer.Canvas do begin
                             TextOut(text_left_margin_dots,ydots,s);
                             ydots:=ydots+TextHeight(' ');

                             if ydots>(printer_length_indexmax_dots-TextHeight(' ')*2)
                                then begin
                                       Printer.NewPage;          // need a new page.
                                       ydots:=TextHeight(' ');
                                     end;
                           end;//with
                         end;
                         //////////////////////////////////////////////////////////////////

begin
  if banner_paper=False then EXIT;   // this routine only called while printing control template on banner paper is in progress.

  text_left_margin_dots:=Round(printer_text_left_margin*nom_width_dpi/25.4);

  Printer.NewPage;                // printing is already in progress

  with Printer.Canvas do begin

    Font.Assign(set_font('Arial',6,[],clBlack));     // small font for top string.
    ydots:=TextHeight(' ');                          // init top line spacing (arbitrary).

    do_text(top_str);                                // print the top string first.
    do_text('');

    Font.Assign(printer_text_font);       // his requested font.
    Font.Name:='Arial';                   // use Arial for header.
    Font.Size:=Font.Size+2;

    if get_prindex(prindex)=False then EXIT;                                                      // get current printer index or none available.
    if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact>0 then Font.Style:=[];   // not bold on impact printer.

    if box_project_title_str<>'' then do_text(Trim(info_form.gauge_label.Caption)+'   for : '+box_project_title_str)
                                 else do_text(Trim(info_form.gauge_label.Caption));

    do_text('');
    do_text(current_name_str);

    Font.Assign(printer_text_font);       // his requested font.
    if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact>0 then Font.Style:=[];   // not bold on impact printer.

    do_text('');
    do_text('          all dimensions in mm :');
    do_text('');

    if info_text_list{info_form.info_memo.Lines}.Count<1     // ???
       then do_text('    blank')
       else for line_now:=0 to (info_text_list{info_form.info_memo.Lines}.Count-1) do do_text(info_text_list{info_form.info_memo.Lines}.Strings[line_now]);

    do_text('---------------------------------------------------');

    Font.Name:=edit_memo_form.vis_edit_memo.Font.Name;
    Font.Style:=edit_memo_form.vis_edit_memo.Font.Style;

    if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact>0 then Font.Style:=[];   // not bold or italic on impact printer.

    do_text('  MEMO :');
    do_text('');

    s:=current_memo_str;        // mod 0.72 6-8-01...

    repeat                      //  find memo lines separated by | chars.
      i:=Pos('|',s);
      if i>0
         then begin
                do_text(Copy(s,1,i-1));    // extract a string line from the memo.
                Delete(s,1,i);             // and delete it from the input string.
              end;
    until (i<1) or (Length(s)=0);

    if bottom_str<>''
       then begin
              do_text('---------------------------------------------------');
              Font.Assign(print_labels_font);
              Font.Color:=clBlack;
              if print_entire_pad_flag=True then do_text(' end'+bottom_str)    // add the bottom string last.
                                            else do_text(' '+bottom_str);
            end;
  end;//with
end;
//_________________________________________________________________________________________

procedure Tprint_form.FormCreate(Sender: TObject);

begin
  print_labels_font:=TFont.Create;   // (initial properties set in templot_init)
  printer_text_font:=TFont.Create;
  print_timber_numbers_font:=TFont.Create;
  print_corner_page_numbers_font:=TFont.Create;    // 0.93.a

  // OT-FIRST ClientWidth:=560;
  // OT-FIRST ClientHeight:=322;
  AutoScroll:=True;
end;
//______________________________________________________________________________

procedure Tprint_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  button_clicked:=True;
  ModalResult:=mrCancel;

  if spacing_was_showing=True then pad_form.spacing_ring_menu_entry.Click;
  if shove_was_showing=True then pad_form.shove_timbers_menu_entry.Click;
  if panning_was_showing=True then pad_form.show_pan_controls_menu_entry.Click;
  if info_was_showing=True then pad_form.show_info_menu_entry.Click;

    // bug fixes 208d ...

  if rail_options_was_showing=True then pad_form.omit_rails_joints_menu_entry.Click;
  if platform_was_showing=True then pad_form.platform_edges_menu_entry.Click;
  if trackbed_was_showing=True then pad_form.trackbed_ballast_edges_menu_entry.Click;
  if check_diffs_was_showing=True then pad_form.differ_check_rails_menu_entry.Click;
  if data_child_was_showing=True then data_child_form.Show;
  if stay_visible_was_showing=True then stay_visible_form.Show;


  pad_form.top_toolbar_panel.Show;
  pad_form.second_toolbar_panel.Show;       // 217a

  if Screen.Width>(make_slip_form.Width+pad_form.top_toolbar_panel.Width) then make_slip_form.Show;   // toolbars marker

  pad_form.output_mode_panel.Visible:=False;

end;
//________________________________________________________________________________________

procedure Tprint_form.omit_all_buttonClick(Sender: TObject);

begin
  button_clicked:=True;
  ModalResult:=mrCancel;  // clicking the panel instead of the button doesn't set this.
end;
//___________________________________________________________________________________________

procedure Tprint_form.next_row_buttonClick(Sender: TObject);

begin
  button_clicked:=True;       // ModalResult = mrRetry
end;
//_________________________________________________________________________________________

procedure Tprint_form.omit_page_buttonClick(Sender: TObject);

begin
  button_clicked:=True;       // ModalResult = mrIgnore
end;
//________________________________________________________________________________________

procedure Tprint_form.ok_buttonClick(Sender: TObject);

begin
  button_clicked:=True;       // ModalResult = mrOK
end;
//_________________________________________________________________________________________

procedure Tprint_form.all_buttonClick(Sender: TObject);

begin
  button_clicked:=True;
  ModalResult:=mrYes;     // clicking the panel instead of the button doesn't set this.
end;
//_________________________________________________________________________________________

procedure Tprint_form.FormDeactivate(Sender: TObject);

begin
  if print_busy=True          // don't let him click off the form.
     then begin
            Beep;
            Show;
            BringToFront;
            EXIT;
          end;

  button_clicked:=True;
  ModalResult:=mrCancel;
end;
//______________________________________________________________________________

procedure print_sketchboard_items(on_canvas:TCanvas; grid_left,grid_top:extended);   // 206e

var
  dtp_rect:TRect;
  dtp_width,dtp_height:extended;
  p1,p2:Tpex;

  move_to,line_to:TPoint;   // 208a
  raster_rect:TRect;        // 208a
  low_res_bitmap:TBitmap;   // 208a
  this_graphic:TGraphic;    // 208a


  saved_cursor:TCursor;

begin

(* OT-FIRST

  if print_settings_form.output_sketchboard_items_checkbox.Checked=False then EXIT;

  if print_form.print_sketchboard_items_checkbox.Checked=False then EXIT;

  if dtp_form.Active=True then EXIT;

  if go_sketchboard=False then EXIT;  // 205e   sketchboard not in use

  dtp_form.dtp_document.ZoomPage;   // needed to print items -- fresh calc from rulers.

  update_model_rulers;

  if trackplan_exists=False then EXIT; // can't scale the items without a trackplan item

  if stretch_factor_wide<minfp then EXIT;    // no division by zero
  if stretch_factor_high<minfp then EXIT;

  if dtp_form.dtp_document.CurrentPage.PageWidth<minfp then EXIT;

  saved_cursor:=Screen.Cursor;
  Screen.Cursor:=crHourGlass;

  dtp_form.dtp_document.CurrentPage.PageColor:=dtp_settings_form.sb_page_colour_panel.Color;  // sb normal colour into print.

  Application.ProcessMessages;  // allow to redraw page colour

  //render_page_background_transparent:=True;     not used

  omit_trackplan_from_rendering:= NOT dtp_settings_form.render_trackplan_in_output_checkbox.Checked;   // to omit sketchboard trackplans from output

  dtp_width:=dtp_form.dtp_document.CurrentPage.PageWidth/stretch_factor_wide;
  dtp_height:=dtp_form.dtp_document.CurrentPage.PageHeight/stretch_factor_high;

        // use p1,p2 as for background shapes...

  p1.x:=model_ruler_x_offset;
  p2.x:=p1.x+dtp_width;

  p1.y:=0-model_ruler_y_offset-dtp_height;
  p2.y:=p1.y+dtp_height;

      // mods 208a ...

  if dtp_settings_form.render_output_reduced_resolution_radiobutton.Checked=True    // output sb items at lower res.
     then begin
            low_res_bitmap:=TBitmap.Create;

            copying_sb_to_pad:=False;      //      flag for dtpPage.Print
            copying_sb_to_printer:=False;  // 206e flag for dtpPage.Print  !!! for full-res direct print to output only
            copying_sb_to_pdf:=False;      // 206e flag for dtpPage.Print

            if draw_sb_low_res_output(low_res_bitmap)=True     // in dtp_unit
               then begin
                      move_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                      move_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                      line_to.X:=Round((p2.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                      line_to.Y:=Round((p2.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                      dtp_rect.Left:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                      dtp_rect.Top:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                      dtp_rect.Right:=Round((p2.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                      dtp_rect.Bottom:=Round((p2.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;



                      {
                      if (move_to.X<0) and (line_to.X<0) then CONTINUE;                                                        // not on this page.
                      if (move_to.X>printer_width_indexmax_dots) and (line_to.X>printer_width_indexmax_dots) then CONTINUE;    // not on this page.

                      if (move_to.Y<0) and (line_to.Y<0) then CONTINUE;                                                        // not on this page.
                      if (move_to.Y>printer_length_indexmax_dots) and (line_to.Y>printer_length_indexmax_dots) then CONTINUE;  // not on this page.
                      }

                      if check_limits(move_to, line_to)=True
                         then begin
                                raster_rect.Left:=move_to.X;
                                raster_rect.Top:=move_to.Y;

                                raster_rect.Right:=line_to.X;
                                raster_rect.Bottom:=line_to.Y;

                                on_canvas.Pen.Width:=1;
                                on_canvas.Pen.Style:=psSolid;
                                on_canvas.Pen.Color:=clBlack;

                                on_canvas.Brush.Style:=bsSolid;
                                on_canvas.Brush.Color:=clWhite;

                                on_canvas.CopyMode:=cmSrcCopy;  // reset normal for destination Canvas.

                                       // Delphi funny: StretchDraw requires TGraphic parameter instead of TBitmap to work reliably...

                                this_graphic:=low_res_bitmap;

                                on_canvas.StretchDraw(raster_rect,this_graphic);

                              end;
                    end;

            low_res_bitmap.Free;
          end
     else begin  // full res output...

            copying_sb_to_pad:=False;      //      flag for dtpPage.Print
            copying_sb_to_printer:=True;   // 206e flag for dtpPage.Print
            copying_sb_to_pdf:=False;      // 206e flag for dtpPage.Print

            dtp_rect.Left:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
            dtp_rect.Top:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

            dtp_rect.Right:=Round((p2.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
            dtp_rect.Bottom:=Round((p2.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

            on_canvas.Pen.Width:=1;
            on_canvas.Pen.style:=psSolid;
            on_canvas.Pen.Color:=clBlack;

            on_canvas.Brush.Style:=bsSolid;
            on_canvas.Brush.Color:=clWhite;

            dtp_form.dtp_document.CurrentPage.Print(on_canvas,dtp_rect,0,3,False,False);  // 3 = rotate clockwise

          end;

        // restore...

  // render_page_background_transparent:=False;  not used (fails)

  omit_trackplan_from_rendering:=False;

  copying_sb_to_printer:=False;  // 206e flag for dtpPage.Print

  on_canvas.Pen.Width:=1;
  on_canvas.Pen.style:=psSolid;
  on_canvas.Pen.Color:=clBlack;

  on_canvas.Brush.Style:=bsSolid;
  on_canvas.Brush.Color:=clWhite;

  Screen.Cursor:=saved_cursor;
*)

end;
//______________________________________________________________________________

procedure print_bgnd_shapes(grid_left,grid_top:extended);  // print all background shapes.

var
  i,maxbg_index:integer;
  font_size:integer;

  arm,diamond:extended;

  now_shape:Tbgnd_shape;
  move_to,line_to:TPoint;
  raster_rect:TRect;

begin
  if print_settings_form.output_bgnd_shapes_checkbox.Checked=False
     then EXIT;

  maxbg_index:=bgnd_form.bgnd_shapes_listbox.Items.Count-1;

  if maxbg_index<0 then EXIT;

  with Printer.Canvas do begin

    Font.Assign(shapes_label_font);
    Font.Color:=printshape_colour;

    font_size:=Round(Font.Size*out_factor);
    if font_size<2 then font_size:=2;
    Font.Size:=font_size;

    Pen.Mode:=pmCopy;

    for i:=0 to maxbg_index do begin
      Pen.Style:=psSolid;
      Pen.Color:=printshape_colour;   // it changes for a label or monochrome picture.
      Pen.Width:=printshape_wide;     // it changes for a picture border.

      now_shape:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape;     // next shape.

      with now_shape do begin

        if (hide_bits AND $02)<>0 then CONTINUE;   // shape hidden for output

        if shape_code<>4    // not a target mark
           then begin

                  case shape_style of
                                0: begin
                                     Brush.Color:=clWhite;
                                     Brush.Style:=bsClear;      // transparent. (also lines).
                                   end;
                                1: begin                        // solid infill 0.93.a
                                     if black_white=True        // 0.93.a   -- use timber infill colour
                                        then Brush.Color:=clWhite
                                        else begin
                                               if impact>0 then Brush.Color:=printtimber_colour            // colour plotter.
                                                           else Brush.Color:=printtimber_infill_colour;
                                             end;
                                     // out 0.93.a Brush.Color:=clWhite;

                                     Brush.Style:=bsSolid;      // blank out.
                                   end;

                                2: begin
                                     Brush.Color:=Pen.Color;
                                     Brush.Style:=bsDiagCross;  // cross-hatched.

                                     TextOut(0,0,'');      // !!! Delphi bug? This seems to be necessary before dotted lines will draw properly.
                                                           // TextOut obviously initialises some background mask property which I have been unable
                                                           // to find or set any other way.

                                     if shape_code=0 then Pen.Style:=psDot;  // dashed line.

                                   end;

                              else begin
                                     Brush.Color:=clWhite;
                                     Brush.Style:=bsClear;      // transparent.
                                   end;
                  end;//case

                  move_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  move_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if shape_code=3      // label rectangle..
                     then begin
                            Font.Color:=printshape_colour;
                            line_to.X:=move_to.X+TextWidth(shape_name+'    ');  // 0.93.a add 4 spaces  // was +5
                            line_to.Y:=move_to.Y+ABS(Font.Height*4 div 3);      // 0.93.a // was +5
                            Brush.Color:=clWhite;
                            Brush.Style:=bsSolid;             // blank rectangle box for label.
                            Pen.Color:=Font.Color;
                            Pen.Width:=ABS(Font.Height div 24);   // 0.93.a added arbitrary
                            if Pen.Width<1 then Pen.Width:=1;
                          end
                     else begin
                            line_to.X:=Round((p2.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                            line_to.Y:=Round((p2.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;
                          end;

                  if (move_to.X<0) and (line_to.X<0) then CONTINUE;                                              // not on this page.
                  if (move_to.X>printer_width_indexmax_dots) and (line_to.X>printer_width_indexmax_dots) then CONTINUE;    // not on this page.

                  if (move_to.Y<0) and (line_to.Y<0) then CONTINUE;                                              // not on this page.
                  if (move_to.Y>printer_length_indexmax_dots) and (line_to.Y>printer_length_indexmax_dots) then CONTINUE;  // not on this page.

                  if check_limits(move_to, line_to)=True
                      then begin
                             case shape_code of

                            -1: if print_form.omit_pictures_radio.Checked=False    // 206e
                                   then begin

                                          raster_rect.Left:=move_to.X;
                                          raster_rect.Top:=move_to.Y;

                                          raster_rect.Right:=line_to.X;
                                          raster_rect.Bottom:=line_to.Y;

                                          if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile=True
                                             then begin

                                                    { OT-FIRST
                                                         // metafile...

                                                    if print_form.picture_outlines_radio.Checked=False
                                                       then begin
                                                              try
                                                                if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_picture.Width=0  // empty Graphic
                                                                   then begin
                                                                          rotate_metafile(i);
                                                                          Application.ProcessMessages;   // this seems to be necessary for StretchDraw to work first time.
                                                                        end;

                                                                StretchDraw(raster_rect,Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_picture.Graphic);
                                                              except
                                                                CopyMode:=cmSrcCopy;          // reset normal for destination Canvas.
                                                                Pen.Color:=printshape_colour;
                                                                Brush.Color:=Pen.Color;       // metafile failed - draw hatched outline.
                                                                Brush.Style:=bsBDiagonal;
                                                                Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                              end;//try

                                                            end;}

                                                    do_nothing; // OT-FIRST
                                                  end
                                             else begin   // bitmap...

                                                    if print_form.picture_stretch_radio.Checked=True
                                                       then begin
                                                              try
                                                                if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_bitmap.Empty=True
                                                                   then begin
                                                                          rotate_bitmap(i);
                                                                          Application.ProcessMessages;   // this seems to be necessary for StretchDraw to work first time.
                                                                        end;

                                                                if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.show_transparent=True  // 0.93.a moved into file

                                                                   then CopyMode:=cmSrcAnd    // (destination Canvas) transparent if on white background.
                                                                   else CopyMode:=cmSrcCopy;  // reset normal for destination Canvas.

                                                                if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_bitmap.Monochrome=True
                                                                   then begin
                                                                          Brush.Style:=bsSolid;    //!!! these are all needed to get StretchDraw to work with monochrome bitmaps
                                                                          Brush.Color:=clWhite;
                                                                          Pen.Color:=clBlack;
                                                                          Font.Color:=clBlack;  // !!!! including this.
                                                                          TextOut(0,0,'');      // !!! Delphi bug?
                                                                                                // TextOut obviously initialises some background mask property which I have been unable
                                                                                                // to find or set any other way.
                                                                        end;

                                                                        // Delphi funny: StretchDraw requires TGraphic parameter instead of TBitmap to work reliably...

                                                                StretchDraw(raster_rect,Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_picture.Graphic);

                                                                CopyMode:=cmSrcCopy;   // reset normal for destination Canvas.

                                                              except
                                                                CopyMode:=cmSrcCopy;          // reset normal for destination Canvas.
                                                                Pen.Color:=printshape_colour;
                                                                Brush.Color:=Pen.Color;       // stretch failed - draw hatched outline.
                                                                Brush.Style:=bsBDiagonal;
                                                                Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                              end;//try
                                                            end;//stretch

                                                    if print_form.picture_dots_radio.Checked=True
                                                       then rotate_bitmap_by_dots(i,p1,p2,grid_left,grid_top, Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap.Canvas, Printer.Canvas);

                                                  end;//bitmap

                                          if (print_form.picture_borders_checkbox.Checked=True) or (print_form.picture_outlines_radio.Checked=True)
                                             then begin
                                                    if print_form.picture_outlines_radio.Checked=False then Pen.Width:=printpicborder_wide;    // picture borders thinner unless an outline.
                                                    Pen.Color:=printshape_colour;
                                                    Brush.Color:=clWhite;
                                                    Brush.Style:=bsClear;
                                                    Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                  end;

                                          if print_form.picture_outlines_radio.Checked=True
                                             then begin
                                                    MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y);  // printing picture outlines only - draw diagonal line.
                                                    MoveTo(move_to.X, line_to.Y); LineTo(line_to.X, move_to.Y);  // and other diagonal line.
                                                  end;
                                        end;//-1



                                     0: begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                   1,3: Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                     2: Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);
                             end;//case

                             if shape_code=3
                                then begin
                                       Brush.Color:=clWhite;
                                       Brush.Style:=bsClear;
                                       TextOut(move_to.X,move_to.Y,'   '+shape_name);    // 0.93.a 2 spaces  // was +1  // insert label text in rectangle box.
                                     end;
                           end;

                end
           else begin    // shape_code=4, draw a target mark

                  arm:=p2.x;        // cross arm length.
                  diamond:=arm/2;   // size of centre diamond.

                  move_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;          // lengthwise arms...
                  move_to.Y:=Round(((p1.x-arm)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  line_to.X:=move_to.X;
                  line_to.Y:=Round(((p1.x+arm)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            MoveTo(move_to.X, move_to.Y);
                            LineTo(line_to.X, line_to.Y);   // draw lengthwise arms.
                          end;

                  move_to.X:=Round(((p1.y-arm)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;    // widthwise arms...
                  move_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  line_to.X:=Round(((p1.y+arm)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=move_to.Y;

                  if check_limits(move_to, line_to)=True
                     then begin
                            MoveTo(move_to.X, move_to.Y);
                            LineTo(line_to.X, line_to.Y);   // draw widthwise arms.
                          end;

                     // now do 4 diamond lines...

                                // NW line...

                  move_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  move_to.Y:=Round(((p1.x-diamond)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  line_to.X:=Round(((p1.y+diamond)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            MoveTo(move_to.X, move_to.Y);
                            LineTo(line_to.X, line_to.Y);
                          end;

                  move_to:=line_to;      // NE line...
                  line_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round(((p1.x+diamond)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            MoveTo(move_to.X, move_to.Y);
                            LineTo(line_to.X, line_to.Y);
                          end;

                  move_to:=line_to;      // SE line...
                  line_to.X:=Round(((p1.y-diamond)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            MoveTo(move_to.X, move_to.Y);
                            LineTo(line_to.X, line_to.Y);
                          end;

                  move_to:=line_to;      // SW line...
                  line_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round(((p1.x-diamond)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            MoveTo(move_to.X, move_to.Y);
                            LineTo(line_to.X, line_to.Y);
                          end;
                end;

      end;//with now_shape
    end;//for next i
  end;//with Printer.Canvas
end;
//_______________________________________________________________________________________

procedure print_bgnd_marks(grid_left,grid_top:extended; maxbg_index:integer; rail_joints:boolean);  // print all the background timbering and marks.

                // if rail_joints=True print only the rail joints, otherwise omit them.
var
  i,n:integer;

  move_to,line_to:TPoint;
  p1,p2,p3,p4: TPoint;

  now_keep:Tbgnd_keep;

  array_max:integer;
  code:integer;

  radcen_arm:extended;

  infill_points:array [0..3] of TPoint;

  fontsize:extended;
  num_str:string;
  tbnum_str:string;

  mapping_colour:integer;
  using_mapping_colour:boolean;

  switch_label_str:string;  // 206b

  s,idnum_str,idtb_str:string;  // 208a
  num:integer;                  // 208a

begin
  with Printer.Canvas do begin

    Pen.Mode:=pmCopy;   // defaults.
    Pen.Style:=psSolid;

    for n:=0 to maxbg_index do begin

      if Ttemplate(keeps_list.Objects[n]).bg_copied=False then CONTINUE;  // no data, not on background.

      if (Ttemplate(keeps_list.Objects[n]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

      if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.fb_kludge_template_code>0 then CONTINUE;  // 209c no marks for fb_kludge templates

      now_keep:=Ttemplate(keeps_list.Objects[n]).bgnd_keep;    // next background keep.

      with now_keep do begin

                  // mapping_colours_print: 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.

        using_mapping_colour:=False;  // default init.
        mapping_colour:=clBlack;      // init - keep compiler happy.

        with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin

          idnum_str:=id_number_str;  // 208a

          if (use_print_mapping_colour=True)
             and ( (mapping_colours_print=2) or (mapping_colours_print=3) )
             and (black_white=False)
             and (grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(print_mapping_colour);
                        using_mapping_colour:=True;
                      end;

          if (use_pad_marker_colour=True)
             and (mapping_colours_print=4)   // use pad settings instead
             and (black_white=False)
             and (grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(pad_marker_colour);
                        using_mapping_colour:=True;
                      end;
        end;//with

        tbnum_str:=timber_numbers_string;      // the full string of timber numbering.

             // first draw bgnd marks and timbers ...

        array_max:=intarray_max(list_bgnd_marks[0]);

        for i:=0 to array_max do begin

          code:=intarray_get(list_bgnd_marks[4],i);

          case code of
            -5,-4,-1,0,8,9,10,501..508: CONTINUE;     // no name label, timber selector, peg centre, blank, peg arms, plain-track end marks. // 0.94.a no check-rail labels
          end;//case

          if rail_joints=(code<>6) then CONTINUE;  // do only the rail joints if rail_joints=True and ignore them otherwise.

          if print_settings_form.output_timbering_checkbox.Checked=False
             then begin
                    case code of
                      3,4,5,14,33,44,54,55,93,95,99,203,233,293: CONTINUE;     // no timbering wanted.
                    end;//case
                  end;

          if print_settings_form.output_radial_ends_checkbox.Checked=False
             then begin
                    case code of
                       2,7: CONTINUE;     // no radial ends wanted  206a
                    end;//case
                  end;

          if print_settings_form.output_switch_labels_checkbox.Checked=False
             then begin
                    case code of
                       600,601..605: CONTINUE;     // no long marks or switch labels wanted  206b
                    end;//case
                  end;

          if print_settings_form.output_xing_labels_checkbox.Checked=False
             then begin
                    case code of
                       700..703: CONTINUE;     // no long marks or crossing labels wanted  211b
                    end;//case
                  end;

          if ((code=5) or (code=55) or (code=95) or (code=600) or (code=700)) and (out_factor<>1.0) then CONTINUE;   // reduced ends are meaningless if not full-size.   206b 600 added. 211b 700 added

          if ((code=203) or (code=233) or (code=293)) and (i<array_max)         // timber infill
             then begin
                     p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm
                     p1.Y:=intarray_get(list_bgnd_marks[1],i);

                     p2.X:=intarray_get(list_bgnd_marks[2],i);    // x2,y2 in  1/100ths mm
                     p2.Y:=intarray_get(list_bgnd_marks[3],i);

                     p3.X:=intarray_get(list_bgnd_marks[0],i+1);    // x3,y3 in  1/100ths mm
                     p3.Y:=intarray_get(list_bgnd_marks[1],i+1);

                     p4.X:=intarray_get(list_bgnd_marks[2],i+1);    // x4,y4 in  1/100ths mm
                     p4.Y:=intarray_get(list_bgnd_marks[3],i+1);
                  end
             else begin         // keep compiler happy...
                     p1.X:=0;
                     p1.Y:=0;

                     p2.X:=0;
                     p2.Y:=0;

                     p3.X:=0;
                     p3.Y:=0;

                     p4.X:=0;
                     p4.Y:=0;
                  end;

          if ( (code>0) and (code<99) )
          or (code=600) or (code=700)                    // 206b 211b overwrite switch/xing marks on output

             then begin
                    Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                    Brush.Style:=bsClear;
                    TextOut(0,0,'');

                     p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm
                     p1.Y:=intarray_get(list_bgnd_marks[1],i);

                     p2.X:=intarray_get(list_bgnd_marks[2],i);    // x2,y2 in  1/100ths mm
                     p2.Y:=intarray_get(list_bgnd_marks[3],i);

                     if impact>0 then Pen.Width:=1        // impact printer or plotter.
                                 else begin
                                       case code of
                                             1: Pen.Width:=printmark_wide;    // guide marks.
                                             2: Pen.Width:=printmark_wide;    // rad end marks.
                                       3,33,93: Pen.Width:=printtimber_wide;  // timber outlines.
                                          4,44: Pen.Width:=1;                  // timber centre-lines.
                                       5,55,95: Pen.Width:=1;                  // timber reduced ends.
                                             6: Pen.Width:=printmark_wide;    // rail joint marks.
                                             7: Pen.Width:=printmark_wide;    // transition ends.
                                         14,54: Pen.Width:=printrail_wide;    // timber centre-lines with rail centre-lines (for rivet locations?).

                                       600,700: Pen.Width:=printrail_wide + printrail_wide div 2;   //  206b 211b long switch/xing marks

                                           else Pen.Width:=1;                  // others not drawn.
                                       end;//case

                                       // out 0.73.a 12-8-01 (now done in thickness setup) if out_factor<1 then Pen.Width:=Round(Pen.Width*out_factor); // scale down the line width.
                                       if Pen.Width<1 then Pen.Width:=1;
                                     end;
                    case code of
                        4,44: Pen.Style:=psDash;    // timber centre-lines (not for rivets).
                     5,55,95: Pen.Style:=psDot;     // timber reduced ends.
                         else Pen.Style:=psSolid;   // all the rest.
                    end;//case

                    if Pen.Style<>psSolid then Pen.Width:=1;   // delphi bug? (patterns only work for lines 1 dot wide.)

                    if black_white=True
                       then Pen.Color:=clBlack
                       else begin
                              if using_mapping_colour=True
                                 then Pen.Color:=mapping_colour
                                 else begin
                                        if mapping_colours_print<0    // 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.
                                           then Pen.Color:=printbg_single_colour     // single colour for all of background templates.
                                           else begin
                                                  case code of
                                               1,600,700: Pen.Color:=printguide_colour;  // guide marks.  206b 600 added, 211b 700 added
                                                       2: Pen.Color:=printalign_colour;  // rad end marks.
                                                 3,33,93: Pen.Color:=printtimber_colour; // timber outlines.
                                                       6: Pen.Color:=printjoint_colour;  // rail joints.
                                                       7: Pen.Color:=printalign_colour;        // transition ends.
                                                     else Pen.Color:=calc_intensity(clBlack);  // thin dotted lines in black only for timber centres and reduced ends.
                                                  end;//case
                                                end;
                                      end;
                            end;

                    Pen.Mode:=pmCopy;

                    move_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                    move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                    line_to.X:=Round((p2.Y-grid_left)*scaw_out)+page_left_dots;
                    line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;
                    if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                  end
             else begin
                    if ((code=-2) or (code=-3)) and {(pad_form.print_radial_centres_menu_entry.Checked=True)}  // 0.82.b
                       (print_settings_form.output_radial_centres_checkbox.Checked=True)    // draw curving rad centres...
                       then begin
                              if impact>0 then Pen.Width:=1                // impact printer or plotter.
                                          else Pen.Width:=printmark_wide;  // guide marks.

                              // out 0.73.a 12-8-01 (now done in thickness setup) if out_factor<1 then Pen.Width:=Round(Pen.Width*out_factor); // scale down the line width.
                              if Pen.Width<1 then Pen.Width:=1;

                              Pen.Style:=psSolid;
                              Pen.Mode:=pmCopy;

                              if black_white=True
                                 then Pen.Color:=clBlack  // overide.
                                 else begin
                                        //if single_colour_flag=False then Pen.Color:=clBlack
                                        if mapping_colours_print<>-1
                                           then Pen.Color:=calc_intensity(clBlack)
                                           else Pen.Color:=printbg_single_colour;
                                      end;

                              p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm
                              p1.Y:=intarray_get(list_bgnd_marks[1],i);

                              radcen_arm:=400*scale;  // 4ft scale arbitrary (scale is for control template).

                              move_to.X:=Round((p1.Y+radcen_arm-grid_left)*scaw_out)+page_left_dots;      // mark centre widthwise.
                              move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                              line_to.X:=Round((p1.Y-radcen_arm-grid_left)*scaw_out)+page_left_dots;
                              line_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;
                              if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                              move_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;                 // mark centre lengthwise
                              move_to.Y:=Round((p1.X+radcen_arm-grid_top)*scal_out)+page_top_dots;

                              line_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                              line_to.Y:=Round((p1.X-radcen_arm-grid_top)*scal_out)+page_top_dots;
                              if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                            end;

                    if (code=203) or (code=233) or (code=293)       // timber infill...
                       then begin
                              infill_points[0].X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[0].Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                              infill_points[1].X:=Round((p2.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[1].Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;

                              infill_points[2].X:=Round((p3.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[2].Y:=Round((p3.X-grid_top)*scal_out)+page_top_dots;

                              infill_points[3].X:=Round((p4.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[3].Y:=Round((p4.X-grid_top)*scal_out)+page_top_dots;

                              if (check_limits(infill_points[0],infill_points[1])=True) and (check_limits(infill_points[2],infill_points[3])=True)
                                 then begin
                                        Pen.Width:=1;
                                        Pen.Style:=psSolid;
                                        Pen.Mode:=pmCopy;

                                        Pen.Color:=clWhite;  // so no overdrawing of timber outlines.

                                        if black_white=True
                                           then Brush.Color:=clBlack
                                           else begin
                                                  //if single_colour_flag=False
                                                  if mapping_colours_print<>-1
                                                     then begin
                                                            if impact>0 then Brush.Color:=printtimber_colour            // colour plotter, use same as timber outlines.
                                                                        else Brush.Color:=printtimber_infill_colour;
                                                          end
                                                     else Brush.Color:=printbg_single_colour;
                                                end;

                                        case print_timb_infill_style of
                                                        0: CONTINUE;
                                                        1: Brush.Style:=bsBDiagonal;    // hatched. Backward diagonal for the background templates.
                                                        2: Brush.Style:=bsDiagCross;

                                                        3: if (impact>0) or (black_white=True) or (mapping_colours_print<0)
                                                              then CONTINUE  // 209c now no fill   was Brush.Style:=bsBDiagonal    // impact printer or plotter, or printing black and white or in a single colour.
                                                              else Brush.Style:=bsSolid;

                                                        4: begin                         // blank.
                                                             Brush.Style:=bsSolid;
                                                             Brush.Color:=clWhite;       // overide.
                                                           end;

                                                      else CONTINUE;
                                        end;//case

                                        Polygon(infill_points);
                                      end;
                            end;

                    if (code=99)
                       and ( (pad_form.print_timber_numbering_menu_entry.Checked=True)
                            or ((out_factor>0.99) and (pad_form.numbering_fullsize_only_menu_entry.Checked=True)) )

                       then begin
                              p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm  numbers on screen
                              p1.Y:=intarray_get(list_bgnd_marks[1],i);

                                    // 208a mods...

                              p2.X:=intarray_get(list_bgnd_marks[2],i);    // x2,y2 in  1/100ths mm  numbers on output
                              p2.Y:=intarray_get(list_bgnd_marks[3],i);

                              move_to.X:=Round((p2.Y-grid_left)*scaw_out)+page_left_dots;
                              move_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;

                              num_str:=extract_tbnumber_str(tbnum_str); // get next timber numbering string from the acummulated string.
                              if num_str='' then CONTINUE;              // no string available??

                              if pad_form.timber_numbering_on_plain_track_menu_entry.Checked=False   // 208a
                                 then begin
                                        s:=Copy(num_str,1,1);

                                        if (s='A') or (s='E') or (s='R') or (s='N')    // not wanted on plain track,
                                           then begin
                                                  if Copy(num_str,Length(num_str),1)='1'   // every 10 sleepers
                                                     then num_str:=''                      // show template ID only
                                                     else CONTINUE;
                                                end;
                                      end;

                              if check_limit(False,False,move_to)=True
                                 then begin
                                        Font.Assign(print_timber_numbers_font);

                                        if black_white=True           // overides..
                                           then Font.Color:=clBlack
                                           else begin
                                                  if mapping_colours_print<0 then Font.Color:=printbg_single_colour;
                                                end;

                                        if pad_form.scale_timber_numbering_menu_entry.Checked=True
                                           then begin
                                                  fontsize:=Font.Size*out_factor;
                                                  if fontsize<4 then CONTINUE;      // minimum to be legible.
                                                  Font.Size:=Round(fontsize);
                                                end;

                                        Brush.Style:=bsSolid;
                                        Brush.Color:=clWhite;

                                            // 208a mods...

                                        if num_str='' then idtb_str:=' '+idnum_str+' '               // 208a  template ID only
                                                      else idtb_str:=' '+idnum_str+'.'+num_str+' ';  // 208a  timber number output with template ID

                                        TextOut(move_to.X-(TextWidth(idtb_str) div 2),
                                                move_to.Y-(TextHeight(idtb_str) div 2),
                                                idtb_str);

                                        Font.Assign(print_labels_font);      // reset for grid labels
                                      end;
                            end;//numbering

                    case code of     // switch labels 206b

              601..605,701..703: begin

                                  if out_factor<>1.0 then CONTINUE;     // on full size prints only

                                  p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm
                                  p1.Y:=intarray_get(list_bgnd_marks[1],i);

                                  move_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                                  move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                  if check_limit(False,False,move_to)=True
                                     then begin
                                            Font.Assign(print_timber_numbers_font);

                                            Font.Style:=[fsBold,fsItalic];
                                            Font.Color:=printguide_colour;

                                            if scale>3 then Font.Size:=Font.Size+1; // a bit bigger above 3mm/ft

                                            Brush.Style:=bsSolid;
                                            Brush.Color:=clWhite;

                                            case code of
                                                  601: switch_label_str:='tips';
                                                  602: switch_label_str:='set (bend)';
                                                  603: switch_label_str:='planing';
                                                  604: switch_label_str:='stock gauge';
                                                  605: switch_label_str:='joggles';

                                                  701: switch_label_str:='intersection FP';
                                                  702: switch_label_str:='blunt nose';
                                                  703: switch_label_str:='blunt tips';

                                                  else switch_label_str:='';
                                            end;//case

                                            TextOut(move_to.X-(TextWidth(switch_label_str) div 2),  // div 2 allows for rotation of template
                                                    move_to.Y-(TextHeight(switch_label_str) div 2),
                                                    ' '+switch_label_str+' ');

                                            Font.Assign(print_labels_font);      // reset for grid labels
                                          end;
                                end;

                    end;//case

                  end;//other codes

        end;//next i background mark

      end;//with now_keep
    end;//next n template
  end;//with Printer.Canvas
end;
//__________________________________________________________________________________________

procedure print_bgnd(grid_left,grid_top:extended);        // print background templates.

var
  max_list_index:integer;
  move_to,line_to:TPoint;
  p1,p2: TPoint;
  now_keep:Tbgnd_keep;

  n,aq,nk:integer;
  array_max:integer;

  xint,yint:integer;

  l_dims_valid:boolean;
  w_dims_valid:boolean;

  now,rail:integer;

  mapping_colour:integer;
  using_mapping_colour:boolean;

  fixed_diamond_ends:boolean;

  gaunt_template:boolean;  // 0.93.a  ex 0.81

  fb_kludge_this:integer;

  this_one_platforms_trackbed:boolean;  // 206b

  this_one_trackbed_cess_ms:boolean;       // 215a
  this_one_trackbed_cess_ts:boolean;       // 215a

                  ////////////////////////////////////////////////////////////

                  procedure set_pen_railcolour(rail_edges:boolean);  // 0.76.a 3-11-01.

                  begin
                    with Printer.Canvas do begin

                      if black_white=True
                         then begin
                                Pen.Color:=clBlack;
                                EXIT;
                              end;

                      if output_diagram_mode=True    // 0.94.a  don't use mapping colour for rail edges (used for infill instead).
                         then begin
                                if (rail=16) or (rail=20)                  // 0.93.a platforms
                                   then Pen.Color:=printplat_edge_colour
                                   else Pen.Color:=printbgrail_colour;
                                EXIT;
                              end;

                      if (using_mapping_colour=True) and ( (print_form.black_edges_checkbox.Checked=False) or (rail_edges=False) )
                         then begin
                                Pen.Color:=mapping_colour;
                                EXIT;
                              end;

                      if (mapping_colours_print<0) and ( (print_form.black_edges_checkbox.Checked=False) or (rail_edges=False) )                    // 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.
                         then begin
                                Pen.Color:=printbg_single_colour;     // single colour for all of background templates.
                                EXIT;
                              end;

                          // normal print...

                      if (rail=16) or (rail=20)                   // 0.93.a platforms
                         then Pen.Color:=printplat_edge_colour
                         else Pen.Color:=printbgrail_colour;

                    end;//with
                  end;
                  ///////////////////////////////////////////////////////////////

                  function pbg_get_w_dots(q,n:integer):integer;

                  var
                    yint:integer;

                  begin
                    yint:=intarray_get(now_keep.list_bgnd_rails[q,1],n);

                    RESULT:=Round((yint-grid_left)*scaw_out)+page_left_dots;

                    w_dims_valid:=check_draw_dim_w(RESULT);
                  end;
                  ////////////////////////////

                  function pbg_get_l_dots(q,n:integer):integer;

                  var
                    xint:integer;

                  begin
                    xint:=intarray_get(now_keep.list_bgnd_rails[q,0],n);

                    RESULT:=Round((xint-grid_top)*scal_out)+page_top_dots;

                    l_dims_valid:=check_draw_dim_l(RESULT);
                  end;
                  ////////////////////////////////////////////////

                  procedure pbg_outline_railedge(aq,blanking_colour:integer; blank_it:boolean);

                  var
                    nk:integer;

                  begin
                    with now_keep do begin
                      array_max:=intarray_max(list_bgnd_rails[aq,0]);
                      if array_max=0 then EXIT;                         // empty rail.

                      xint:=intarray_get(list_bgnd_rails[aq,0],0);
                      yint:=intarray_get(list_bgnd_rails[aq,1],0);

                      move_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                      move_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                      with Printer.Canvas do begin

                        if blank_it=True
                           then Pen.Color:=blanking_colour
                           else set_pen_railcolour(True);

                        for nk:=1 to array_max do begin

                          xint:=intarray_get(list_bgnd_rails[aq,0],nk);
                          yint:=intarray_get(list_bgnd_rails[aq,1],nk);

                          line_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                          line_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                          if check_limits(move_to, line_to)=True
                             then begin
                                    MoveTo(move_to.X, move_to.Y);
                                    LineTo(line_to.X, line_to.Y);
                                  end;
                          move_to:=line_to;
                        end;//next nk

                      end;//with Canvas
                    end;//with bgnd template
                  end;
                  ////////////////////////////

                  procedure pbg_draw_fill_rail(outer_add:integer);    // draw a complete filled rail.

                  const
                    dots_max_c=xy_pts_c*2;

                  var
                    dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                      //    (xy_pts_c=3000;)
                      //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                      //   template max is 4500' scale length.
                      //   ( = 18000 mm or 59ft approx in 4 mm scale).
                      //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                      // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                      // because needed for the Polygon function.

                      // total memory = 6000*8 bytes = 48kb.

                    now, start, now_max:integer;
                    edge_started:boolean;
                    dots_index:integer;
                    x_dots,y_dots:integer;
                    aq:integer;
                    mid_dots_index:integer;
                    edge_colour, blanking_colour:integer;

                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure pbg_modify_rail_end(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with Printer.Canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(move_to.X, move_to.Y);

                                                          MoveTo(line_to.X, line_to.Y);
                                                          LineTo(line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                  begin
                    aq:=rail;  // gauge-faces.

                    with now_keep do begin

                      if (intarray_max(list_bgnd_rails[aq,0])=0) or (intarray_max(list_bgnd_rails[aq+outer_add,0])=0)  // data not for both edges?
                         then begin
                                if intarray_max(list_bgnd_rails[aq,0])<>0 then pbg_outline_railedge(aq,0,False);
                                if intarray_max(list_bgnd_rails[aq+outer_add,0])<>0 then pbg_outline_railedge(aq+outer_add,0,False);
                                EXIT;
                              end;

                      now_max:=intarray_max(list_bgnd_rails[aq,0]);

                      if gaunt_template=False
                         then begin
                                case aq of
                                     1: begin
                                          start:=planing_end_aq1; // start from end of planing - no infill in planing.

                                          if (start<0) or (start>now_max) then EXIT;  // ???
                                        end;

                                     2: begin                         // ditto
                                          start:=planing_end_aq2; // start from end of planing - no infill in planing.

                                          if (start<0) or (start>now_max) then EXIT;  // ???
                                        end;

                                   else start:=0;                   // whole list.
                                end;//case
                              end
                         else start:=0;  // gaunt template, no planing. 0.81 09-Jul-2005

                      dots_index:=0-1;                    // first increment is to zero.

                      edge_started:=False;

                      for now:=start to now_max do begin
                        x_dots:=pbg_get_w_dots(aq,now);
                        y_dots:=pbg_get_l_dots(aq,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      mid_dots_index:=dots_index;

                      aq:=rail+outer_add;             // outer-edges.

                      now_max:=intarray_max(list_bgnd_rails[aq,0]);

                      edge_started:=False;

                      for now:=now_max downto 0 do begin
                        x_dots:=pbg_get_w_dots(aq,now);
                        y_dots:=pbg_get_l_dots(aq,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      with Printer.Canvas do begin

                        set_pen_railcolour(True);

                        if (rail=16) or (rail=20)   // 0.93.a platforms
                           then begin
                                  if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )   // 206b
                                     then Brush.Color:=Pen.Color
                                     else Brush.Color:=printplat_infill_colour;

                                  case print_platform_infill_style of
                                          0: Brush.Style:=bsClear;
                                          1: Brush.Style:=bsFDiagonal;    // hatched. forward diagonal (backward diagonal on bgnd template timbers).
                                          2: Brush.Style:=bsDiagCross;

                                          3: if mapping_colours_print<0          // solid option.
                                                then Brush.Style:=bsFDiagonal    // single colour.
                                                else Brush.Style:=bsSolid;

                                        else begin                         // 4 = blank.
                                               Brush.Style:=bsSolid;
                                               Brush.Color:=clWhite;       // overide.
                                             end;

                                  end;//case
                                end
                           else begin
                                  if ((this_one_trackbed_cess_ts=True) and (rail=18))  // 215a
                                  or ((this_one_trackbed_cess_ms=True) and (rail=22))  // 215a

                                     then begin
                                            if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
                                               then Brush.Color:=Pen.Color
                                               else Brush.Color:=sb_track_bgnd_colour;      // cess use same colour as track background
                                            Brush.Style:=bsBDiagonal;
                                          end
                                     else begin   // normal rails...

                                            if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
                                               then Brush.Color:=calc_intensity(clSilver)  // 214b  - was clGray
                                               else begin
                                                      if fb_kludge_this>0 then Brush.Color:=printrail_infill_colour_cu    // 0.94.a
                                                                          else Brush.Color:=printrail_infill_colour_bg;
                                                    end;

                                            case rail_infill_i of
                                                            1: Brush.Style:=bsBDiagonal;   // hatched
                                                            2: Brush.Style:=bsSolid;       // solid
                                                            3: Brush.Style:=bsDiagCross;   // cross_hatched
                                                            4: begin                       // blank
                                                                 Brush.Style:=bsSolid;
                                                                 Brush.Color:=clWhite;     // overide
                                                               end;
                                                          else Brush.Style:=bsSolid;       // solid
                                            end;//case

                                          end;
                                end;

                        if (black_white=True) or (impact>0) {or (mapping_colours_print<0)}
                           then begin
                                  Brush.Style:=bsSolid;               // solid infill white.
                                  Brush.Color:=clWhite;               // overide
                                end;

                        if dots_index>2
                           then begin
                                  Polygon(Slice(dots,dots_index+1));   // +1, number of points, not index.  must have 4 points.

                                  edge_colour:=Pen.Color;  // existing rail edges.

                                  if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
                                                         else blanking_colour:=clWhite;

                                        // remove polygon line across end of planing (not for fixed-diamond)..
                                        // (for gaunt template this removes the polygon line across the rail end)

                                  if (fixed_diamond_ends=False) and ((rail=1) or (rail=2)) then pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                        // remove polygon lines across stock rail ends...
                                        // and trackbed ends  206b

                                  if (rail=0) or (rail=3) or (rail=18) or (rail=22)   // 18,22 added 206b
                                     then begin
                                            pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // toe or approach end.

                                            pbg_modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);  // exit end.
                                          end;

                                           // 0.93.a blank platform edges ...

                                  with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info do begin

                                    if adjacent_edges_keep=True  // platforms
                                       then begin
                                                    // 0.93.a blank platform rear edges ...

                                              if (rail=16) and (draw_ts_platform_keep=True) and (draw_ts_platform_rear_edge_keep=False)   // 0.93.a TS platform start
                                                 then pbg_outline_railedge(16,blanking_colour,True);     // blank rear edge

                                              if (rail=20) and (draw_ms_platform_keep=True) and (draw_ms_platform_rear_edge_keep=False)   // 0.93.a TS platform start
                                                 then pbg_outline_railedge(20,blanking_colour,True);     // blank rear edge

                                                    // 0.93.a blank platform ends ...

                                              if (rail=16) and (draw_ts_platform_keep=True) and (draw_ts_platform_start_edge_keep=False)   // 0.93.a TS platform start
                                                 then pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=16) and (draw_ts_platform_keep=True) and (draw_ts_platform_end_edge_keep=False)     // 0.93.a TS platform end
                                                 then pbg_modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform_keep=True) and (draw_ms_platform_start_edge_keep=False)   // 0.93.a MS platform start
                                                 then pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform_keep=True) and (draw_ms_platform_end_edge_keep=False)     // 0.93.a MS platform end
                                                 then pbg_modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                            end;
                                  end;//with

                                  if (rail=26) or (rail=28)
                                     then begin
                                            pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // centre of K-crossing check rails.
                                          end;

                                end;
                      end;//with Canvas
                    end;//with background template
                  end;
                  ////////////////////////////////////////////////////////////////////////

                  procedure pbg_draw_fill_vee;    // do complete vee in one go ...

                  const
                    dots_max_c=xy_pts_c*2;

                  var
                    dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                      //    (xy_pts_c=3000;)
                      //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                      //   template max is 4500' scale length.
                      //   ( = 18000 mm or 59ft approx in 4 mm scale).
                      //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                      // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                      // because needed for the Polygon function.

                      // total memory = 6000*8 bytes = 48kb.


                    now:integer;
                    edge_started:boolean;
                    dots_index:integer;
                    x_dots,y_dots:integer;
                    aq:integer;
                    point_mid_dots_index, splice_mid_dots_index:integer;
                    edge_colour, blanking_colour:integer;

                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure pbg_modify_vee_end(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with Printer.Canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(move_to.X, move_to.Y);

                                                          MoveTo(line_to.X, line_to.Y);
                                                          LineTo(line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                  begin
                    with now_keep do begin

                      if (intarray_max(list_bgnd_rails[4,0])=0)
                      or (intarray_max(list_bgnd_rails[5,0])=0)
                      or (intarray_max(list_bgnd_rails[12,0])=0)
                      or (intarray_max(list_bgnd_rails[13,0])=0)  // not enough data for filled vee.
                         then begin
                                if intarray_max(list_bgnd_rails[4,0])<>0  then pbg_outline_railedge(4,0,False);       // draw outline vee...
                                if intarray_max(list_bgnd_rails[5,0])<>0  then pbg_outline_railedge(5,0,False);
                                if intarray_max(list_bgnd_rails[12,0])<>0 then pbg_outline_railedge(12,0,False);
                                if intarray_max(list_bgnd_rails[13,0])<>0 then pbg_outline_railedge(13,0,False);
                              end
                         else begin                // polygon mode...

                                dots_index:=0-1;   // first increment is to zero.

                                aq:=4;
                                edge_started:=False;
                                for now:=0 to intarray_max(list_bgnd_rails[aq,0]) do begin    // vee main-side, gauge_face, start from the tip.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                point_mid_dots_index:=dots_index;

                                aq:=12;
                                edge_started:=False;
                                for now:=intarray_max(list_bgnd_rails[aq,0]) downto 0 do begin // back along outer-edge.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                aq:=13;
                                edge_started:=False;
                                for now:=0 to intarray_max(list_bgnd_rails[aq,0]) do begin    // and then turnout side outer edge.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                splice_mid_dots_index:=dots_index;

                                aq:=5;
                                edge_started:=False;
                                for now:=intarray_max(list_bgnd_rails[aq,0]) downto 0 do begin // and back along the gauge face to the tip.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                with Printer.Canvas do begin

                                  set_pen_railcolour(True);

                                  if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
                                     then Brush.Color:=calc_intensity(clSilver)  // 214b  - was clGray
                                     else begin
                                            if fb_kludge_this>0 then Brush.Color:=printrail_infill_colour_cu   //0.94.a
                                                                else Brush.Color:=printrail_infill_colour_bg;
                                          end;

                                  case rail_infill_i of
                                                  1: Brush.Style:=bsBDiagonal;   // hatched
                                                  2: Brush.Style:=bsSolid;       // solid
                                                  3: Brush.Style:=bsDiagCross;   // cross_hatched
                                                  4: begin                       // blank
                                                       Brush.Style:=bsSolid;
                                                       Brush.Color:=clWhite;     // overide
                                                     end;
                                                else Brush.Style:=bsSolid;       // solid
                                  end;//case

                                  if (black_white=True) or (impact>0) {or (mapping_colours_print<0)}
                                     then begin
                                            Brush.Style:=bsSolid;               // solid infill white.
                                            Brush.Color:=clWhite;               // overide
                                          end;

                                  if dots_index>4
                                     then begin
                                            Polygon(Slice(dots,dots_index+1));   // +1, number of points, not index.  must have at least 5 points.

                                            edge_colour:=Pen.Color;  // existing rail edges.

                                            if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
                                                                   else blanking_colour:=clWhite;

                                                  // remove polygon lines across vee rail ends...

                                            pbg_modify_vee_end(point_mid_dots_index,point_mid_dots_index+1,edge_colour,blanking_colour); // point rail end.

                                            pbg_modify_vee_end(splice_mid_dots_index,splice_mid_dots_index+1,edge_colour,blanking_colour); // splice rail end.

                                          end;

                                end;//with Canvas

                            end;//polygon mode
                    end;//with background template
                  end;
                  ////////////////////////////////////////////////////////////////////////

                  procedure pbg_mark_end(aq1, aq1end, aq2, aq2end:integer);    // print the background rail end mark.

                  begin
                    with now_keep do begin
                      if (bgnd_endmarks_yn[aq1,aq1end]=True) and (bgnd_endmarks_yn[aq2,aq2end]=True)
                         then begin
                                p1:=bgnd_endmarks[aq1,aq1end];
                                p2:=bgnd_endmarks[aq2,aq2end];

                                with Printer.Canvas do begin

                                  set_pen_railcolour(True);

                                  move_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                                  move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                  line_to.X:=Round((p2.Y-grid_left)*scaw_out)+page_left_dots;
                                  line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;

                                  if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                end;//with
                              end;
                    end;//with
                  end;
                  ////////////////////////////////////////////////////////////

                  procedure  pbg_outline_railends;  // draw in the rail ends using existing pen settings...

                  begin
                    pbg_mark_end(1,1,9,1);    // main rail wing rail finish.
                    pbg_mark_end(2,1,10,1);   // turnout rail wing rail finish.

                    pbg_mark_end(6,0,14,0);   // main side check rail start.
                    pbg_mark_end(6,1,14,1);   // main side check rail finish.

                    pbg_mark_end(7,0,15,0);   // turnout side check rail start.
                    pbg_mark_end(7,1,15,1);   // turnout side check rail finish.

                    pbg_mark_end(4,0,5,0);    // blunt nose.

                    if fixed_diamond_ends=True
                             then begin
                                    pbg_mark_end(1,0,9,0);   // planed faced of point rails for a fixed-diamond.
                                    pbg_mark_end(2,0,10,0);

                                    pbg_mark_end(26,1,27,1);     // MS K-crossing check rails.
                                    pbg_mark_end(28,1,29,1);     // DS K-crossing check rails.
                                  end;

                  end;
                  ////////////////////////////////////////////////////////////////


                  procedure pbg_draw_diagram_mode;    // 0.93.a draw a complete template in diagrammatic mode

                  const
                    dots_max_c=xy_pts_c*2;

                  var
                    dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                      //    (xy_pts_c=3000;)
                      //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                      //   template max is 4500' scale length.
                      //   ( = 18000 mm or 59ft approx in 4 mm scale).
                      //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                      // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                      // because needed for the Polygon function.

                      // total memory = 6000*8 bytes = 48kb.

                    now,now_max:integer;
                    edge_started:boolean;
                    dots_index:integer;
                    x_dots,y_dots:integer;
                    ms_mid_dots_index,ts_mid_dots_index:integer;
                    edge_colour, blanking_colour:integer;

                    no_vee:boolean;

                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure pbg_modify_boundary(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with Printer.Canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          MoveTo(move_to.X, move_to.Y);
                                                          LineTo(move_to.X, move_to.Y);

                                                          MoveTo(line_to.X, line_to.Y);
                                                          LineTo(line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                  begin
                    with now_keep do begin

                      if intarray_max(list_bgnd_rails[0,0])=0 then EXIT;    // no data for straight stock rail
                      if intarray_max(list_bgnd_rails[3,0])=0 then EXIT;    // no data for curved stock rail

                      if (intarray_max(list_bgnd_rails[4,0])=0) or (intarray_max(list_bgnd_rails[5,0])=0)   // no data for vee rails
                         then no_vee:=True
                         else no_vee:=False;

                      dots_index:=0-1;                    // first increment is to zero.


                      now_max:=intarray_max(list_bgnd_rails[0,0]);    // straight stock rail
                      edge_started:=False;

                      for now:=0 to now_max do begin
                        x_dots:=pbg_get_w_dots(0,now);
                        y_dots:=pbg_get_l_dots(0,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      ms_mid_dots_index:=dots_index;

                      if no_vee=False
                         then begin
                                now_max:=intarray_max(list_bgnd_rails[4,0]);    // point rail
                                edge_started:=False;

                                for now:=now_max downto 0 do begin
                                  x_dots:=pbg_get_w_dots(4,now);
                                  y_dots:=pbg_get_l_dots(4,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now


                                now_max:=intarray_max(list_bgnd_rails[5,0]);    // splice rail
                                edge_started:=False;

                                for now:=0 to now_max do begin
                                  x_dots:=pbg_get_w_dots(5,now);
                                  y_dots:=pbg_get_l_dots(5,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now
                              end;//if vee

                      ts_mid_dots_index:=dots_index;

                      now_max:=intarray_max(list_bgnd_rails[3,0]);    // curved stock rail
                      edge_started:=False;

                      for now:=now_max downto 0 do begin
                        x_dots:=pbg_get_w_dots(3,now);
                        y_dots:=pbg_get_l_dots(3,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      with Printer.Canvas do begin

                        set_pen_railcolour(True);

                        Pen.Width:=printrail_wide;
                        if Pen.Width<1 then Pen.Width:=1;

                        if using_mapping_colour=True
                           then Brush.Color:=mapping_colour
                           else Brush.Color:=sb_diagram_colour;  // 209c was printrail_infill_colour_bg;

                                //end;


                        case rail_infill_i of
                                        1: Brush.Style:=bsBDiagonal;   // hatched
                                        2: Brush.Style:=bsSolid;       // solid
                                        3: Brush.Style:=bsDiagCross;   // cross_hatched
                                        4: begin                       // blank
                                             Brush.Style:=bsSolid;
                                             Brush.Color:=clWhite;     // overide
                                           end;
                                      else Brush.Style:=bsSolid;       // solid
                        end;//case

                        if {(} black_white=True {) or (impact>0)} {or (mapping_colours_print<0)}
                           then begin
                                  Brush.Style:=bsSolid;               // solid infill white.
                                  Brush.Color:=clWhite;               // overide
                                end;

                        if dots_index>2
                           then begin
                                  Polygon(Slice(dots,dots_index+1));   // +1, number of points, not index.  must have 4 points.

                                               // blank out template boundaries...

                                  if output_include_boundaries=False
                                     then begin
                                            edge_colour:=Pen.Color;  // existing rail edges.

                                            if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
                                                                   else blanking_colour:=clWhite;

                                            pbg_modify_boundary(0,dots_index,edge_colour,blanking_colour);  // toe or approach end.

                                            pbg_modify_boundary(ms_mid_dots_index,ms_mid_dots_index+1,edge_colour,blanking_colour);  // exit end (turnout) or Ctrl-1 end (plain track).

                                            if no_vee=False then pbg_modify_boundary(ts_mid_dots_index,ts_mid_dots_index+1,edge_colour,blanking_colour);  // turnout road end.
                                          end;
                                end;

                        if output_show_points_mark=True  // mark position of points
                           then begin

                                   if  (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.turnout_info1.plain_track_flag=False)   // not for plain track
                                   and (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.turnout_info2.semi_diamond_flag=False)            // not for half-diamond
                                   and (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.turnout_info2.gaunt_flag=False)                   // not for gaunt turnout

                                   and (intarray_max(list_bgnd_rails[1,0])<>0)    // data for straight switch rail
                                   and (intarray_max(list_bgnd_rails[2,0])<>0)    // data for curved stock rail
                                       then begin
                                              if Brush.Color=clWhite
                                                 then Pen.Color:=clBlack
                                                 else Pen.Color:=clWhite;     // white points mark

                                              x_dots:=pbg_get_w_dots(1,0);  // ms toe.
                                              y_dots:=pbg_get_l_dots(1,0);

                                              if (w_dims_valid=True) and (l_dims_valid=True)
                                              then begin
                                                     MoveTo(x_dots,y_dots);

                                                     x_dots:=pbg_get_w_dots(2,0);  // ts toe.
                                                     y_dots:=pbg_get_l_dots(2,0);

                                                     if (w_dims_valid=True) and (l_dims_valid=True)
                                                        then LineTo(x_dots,y_dots);
                                                   end;

                                            end;
                                end;//mark points

                      end;//with Canvas
                    end;//with background template
                  end;
                  ////////////////////////////////////////////////////////////////////////


begin          // print background templates...

  max_list_index:=keeps_list.Count-1;

  if max_list_index<0 then EXIT;  // no templates in box.

  if output_diagram_mode=False then print_bgnd_marks(grid_left,grid_top,max_list_index,False);  // 0.93.a // first print all the background timbering and marks except rail joints.

  with Printer.Canvas do begin

    Pen.Mode:=pmCopy;     // default

                  //  now print bgnd track centre-lines and turnout rails...

    for n:=0 to max_list_index do begin

      if Ttemplate(keeps_list.Objects[n]).bg_copied=False then CONTINUE;  // no data, not on background.

      if (Ttemplate(keeps_list.Objects[n]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

      if  (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.fb_kludge_template_code>0)  // 209c
      and (print_settings_form.output_fb_foot_lines_checkbox.Checked=False)
          then CONTINUE;                                                      // foot lines not wanted.

      this_one_platforms_trackbed:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.adjacent_edges_keep;           // True = platforms and trackbed edges   206b

      this_one_trackbed_cess_ms:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep;  // True = cess width instead of trackbed cutting line 215a
      this_one_trackbed_cess_ts:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep;  // True = cess width instead of trackbed cutting line 215a

      now_keep:=Ttemplate(keeps_list.Objects[n]).bgnd_keep;    // next background keep.

      with now_keep do begin

         // mapping_colours_print: 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.

        using_mapping_colour:=False;  // default init.

        with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims do begin

          if box_dims1.bgnd_code_077<>1 then CONTINUE;    // 0.77.b BUG???

          with turnout_info2 do begin
            fixed_diamond_ends:=(semi_diamond_flag=True) and (diamond_fixed_flag=True);    // need end marks on fixed diamond point rails.
            gaunt_template:=gaunt_flag;                                                    // 0.93.a ex 0.81
          end;//with

          if (box_dims1.use_print_mapping_colour=True)
             and ( (mapping_colours_print=1) or (mapping_colours_print=3) )
             and (black_white=False)
             and (grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(box_dims1.print_mapping_colour);
                        using_mapping_colour:=True;
                      end;

          if (box_dims1.use_pad_marker_colour=True)
             and (mapping_colours_print=4)   // use pad settings instead
             and (black_white=False)
             and (grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(box_dims1.pad_marker_colour);
                        using_mapping_colour:=True;
                      end;

          fb_kludge_this:=box_dims1.fb_kludge_template_code;  // 0.94.a

          if fb_kludge_this=0   // no track centre-lines or diagram mode for kludge templates  212a
             then begin

                    if output_diagram_mode=True then pbg_draw_diagram_mode;  // first draw template in diagrammatic mode.

                    if ((print_settings_form.output_centrelines_checkbox.Checked=True) and (output_diagram_mode=False) and (box_dims1.align_info.dummy_template_flag=False))
                    or ((print_settings_form.output_bgnd_shapes_checkbox.Checked=True) and (box_dims1.align_info.dummy_template_flag=True))  // 212a dummy templates not part of track plan

                       then begin

                              Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                              Brush.Style:=bsClear;
                              TextOut(0,0,'');

                              Pen.Mode:=pmCopy;

                              if box_dims1.align_info.dummy_template_flag=True   // 212a   dummy template as bgnd shapes
                                 then begin
                                        Pen.Style:=psSolid;
                                        Pen.Color:=printshape_colour;

                                        if impact>0 then Pen.Width:=1             // impact printer or plotter.
                                                    else Pen.Width:=printshape_wide;

                                        if Pen.Width<1 then Pen.Width:=1;
                                      end
                                 else begin

                                        set_pen_railcolour(False);

                                        if impact>0 then Pen.Width:=1             // impact printer or plotter.
                                                    else Pen.Width:=printcl_wide;

                                        if Pen.Width<1 then Pen.Width:=1;

                                        if Pen.Width=1 then Pen.Style:=psDash
                                                       else Pen.Style:=psSolid;

                                      end;

                              for aq:=24 to 25 do begin         // track centre-lines.

                                array_max:=intarray_max(list_bgnd_rails[aq,0]);
                                if array_max=0 then CONTINUE;                      // empty rail.

                                xint:=intarray_get(list_bgnd_rails[aq,0],0);
                                yint:=intarray_get(list_bgnd_rails[aq,1],0);

                                move_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                                move_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                                for nk:=1 to array_max do begin

                                  xint:=intarray_get(list_bgnd_rails[aq,0],nk);
                                  yint:=intarray_get(list_bgnd_rails[aq,1],nk);

                                  line_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                                  line_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                                  if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                  move_to:=line_to;
                                end;//next nk

                              end;//next aq
                            end;//if track-centres
                  end;// if not kludge

        end;//with template

        if print_settings_form.output_rails_checkbox.Checked=True
           then begin

                  //drawn_full_aq1:=True;    // default init flags.
                  //drawn_full_aq2:=True;

                  Pen.Mode:=pmCopy;
                  Pen.Style:=psSolid;

                  if impact>0 then Pen.Width:=1        // impact printer or plotter.
                              else begin
                                     Pen.Width:=printrail_wide;
                                     if Pen.Width<1 then Pen.Width:=1;
                                   end;

                  if ((rail_infill_i=0) or ((scale*out_factor)<0.1))   // 0.93.a was 0.75 // less than 2.5% for 4mm scale (control template) (1.43% for 7mm).
                  and (output_diagram_mode=False)
                     then begin           //  outline (pen) mode ...
                                          //  n.b. this mode does not automatically close the rail-ends.
                                          //  no infill for platforms

                            set_pen_railcolour(True);

                            for aq:=0 to 23 do begin                                             // 24, 25 centre-lines already done.
                              if (this_one_platforms_trackbed=False) and (aq>15) then CONTINUE;  // no adjacent tracks in output  // 206b
                              pbg_outline_railedge(aq,0,False);
                            end;

                            for aq:=26 to aq_max_c do pbg_outline_railedge(aq,0,False);   // K-crossing check rails.

                            pbg_outline_railends;  // next, draw in the rail ends using same pen settings...
                          end
                     else begin      // infill (polygon) mode ...

                                     // do blades first - neater result.

                            if output_diagram_mode=False // detail mode only
                               then begin
                                      for rail:=1 to 3 do pbg_draw_fill_rail(8);  // closure rails and curved stock rail.

                                      rail:=0;                                    // straight stock rail.
                                      pbg_draw_fill_rail(8);

                                      for rail:=6 to 7 do pbg_draw_fill_rail(8);  // check rails
                                    end;

                            if this_one_platforms_trackbed=True   // no adjacent tracks in output 206b
                               then begin
                                      rail:=16;     // detail mode or diagram mode (for platforms/trackbed)
                                      repeat
                                        pbg_draw_fill_rail(1);    // adjacent rails
                                        rail:=rail+2;
                                        if (output_diagram_mode=True) and (output_include_trackbed_edges=False) and ((rail=18) or (rail=22)) then rail:=rail+2;  // 206b no trackbed/cess in diagram mode
                                      until rail>22;
                                    end;

                            if output_diagram_mode=False // detail mode only
                               then begin

                                      rail:=26;
                                      repeat
                                        pbg_draw_fill_rail(1);                    // K-crossing check rails.
                                        rail:=rail+2;
                                      until rail>28;

                                      pbg_draw_fill_vee;    // do complete vee in one go ...

                                                            // finally draw in the planing gauge-faces - no infill...

                                      set_pen_railcolour(True);

                                      aq:=1;

                                      if (intarray_max(list_bgnd_rails[aq,0])<>0) and (gaunt_template=False) and (planing_end_aq1>0) { and (drawn_full_aq1=False)}    // 0.93.a ex 081
                                         then begin
                                                move_to.X:=pbg_get_w_dots(aq,0); move_to.Y:=pbg_get_l_dots(aq,0);
                                                for now:=1 to planing_end_aq1{+1} do begin
                                                  line_to.X:=pbg_get_w_dots(aq,now); line_to.Y:=pbg_get_l_dots(aq,now);
                                                  if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                                  move_to:=line_to;
                                                end;//for
                                              end;

                                      aq:=2;

                                      if (intarray_max(list_bgnd_rails[aq,0])<>0) and (gaunt_template=False) and (planing_end_aq2>0) { and (drawn_full_aq2=False)}
                                         then begin
                                                move_to.X:=pbg_get_w_dots(aq,0); move_to.Y:=pbg_get_l_dots(aq,0);
                                                for now:=1 to planing_end_aq2{+1} do begin
                                                  line_to.X:=pbg_get_w_dots(aq,now); line_to.Y:=pbg_get_l_dots(aq,now);
                                                  if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                                  move_to:=line_to;
                                                end;//for
                                              end;

                                    end;//detail mode

                          end;//polygon mode

                end;//if rails

      end;//with bgnd_keep
    end;//for next n template
  end;//with Printer.Canvas

                         // finally add the rail-joint marks over the rail infill...   // 209c moved outside loop

  if (print_settings_form.output_rails_checkbox.Checked=True) and (output_diagram_mode=False)
     then print_bgnd_marks(grid_left,grid_top,max_list_index,True);

end;
//________________________________________________________________________________________

procedure Tprint_form.FormCloseQuery(Sender: TObject;  var CanClose: Boolean);

begin
  if print_busy=True
     then begin
            CanClose:=False;                    // can''t close if still busy.
            omit_all_button.Click;
          end
     else CanClose:=True;
end;
//___________________________________________________________________________________________

procedure Tprint_form.banner_fill_checkboxClick(Sender: TObject);

  // also banner_pause_final_page_checkbox click.  0.93.a

begin
  button_clicked:=True;
  banner_changed:=True;
end;
//_______________________________________________________________________________________

procedure Tprint_form.page_map_buttonClick(Sender: TObject);

begin
  if (Printer.Printing=True) or (printer_printing=True)
     then index_sheet_wanted:=True                          // wanted later.
     else begin
            index_sheet_wanted:=False;                      // print immediately.
            print_index_sheet(False);
          end;
end;
//___________________________________________________________________________________________

procedure Tprint_form.font_buttonClick(Sender: TObject);

begin
  preview_form.Font.Assign(get_font('choose  a  new  font  and  text  colour  for  the  preview  page  labels',preview_form.Font,True));

  if printer_printing=False then print_preview(True,False,0);  // redraw in new font.
end;
//__________________________________________________________________________________________

procedure Tprint_form.black_edges_checkboxClick(Sender: TObject);

        // need to repeat the colour setup, already set before form shows.
begin
  print_colours_setup;
end;
//_______________________________________________________________________________________

procedure Tprint_form.FormResize(Sender: TObject);

begin
  if (Showing=True) and (initdone_flag=True) and (form_scaling=False)    //  otherwise clobbers Windows on startup or quit.
     then ScrollInView(all_button);
end;
//___________________________________________________________________________________________

procedure intensity_changed;

var
  n:integer;

begin
  print_colours_setup;            // set up the new colours.

      // clear any existing rotated bitmaps, so new ones are created with new colours...

  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if Count>0
       then begin
              for n:=0 to Count-1 do begin

                if Tbgshape(Objects[n]).bgnd_shape.shape_code<>-1 then CONTINUE;     // not a picture

                Tbgshape(Objects[n]).bgimage.image_shape.rotated_bitmap.Free;            // clear the bitmap...
                Tbgshape(Objects[n]).bgimage.image_shape.rotated_bitmap:=TBitmap.Create;
              end;//for
            end;
  end;//with
end;
//__________________________________________________________________________________________

procedure Tprint_form.print_intensity_trackbarChange(Sender: TObject);

begin
  intensity_changed;
end;
//__________________________________________________________________________________________

procedure Tprint_form.reset_buttonClick(Sender: TObject);

begin
  print_intensity_trackbar.Position:=25;
  intensity_changed;
end;
//___________________________________________________________________________________________

procedure Tprint_form.dark_labelClick(Sender: TObject);

begin
  if print_intensity_trackbar.Position<46 then print_intensity_trackbar.Position:=print_intensity_trackbar.Position+5;
  intensity_changed;
end;
//_______________________________________________________________________________________

procedure Tprint_form.light_labelClick(Sender: TObject);

begin
  if print_intensity_trackbar.Position>4 then print_intensity_trackbar.Position:=print_intensity_trackbar.Position-5;
  intensity_changed;
end;
//______________________________________________________________________________

(* OT-FIRST

procedure rotate_metafile(i:integer);   // rotate metafile supplied 90degs clockwise.   213b

var
  dest_metafile:TMetafile;
  dest_canvas:TMetafileCanvas;
  transform_matrix:tagXFORM;

  saved_screen_cursor:TCursor;

  inwidth,inheight:integer;
  str:string;
  in_picture:TPicture;  // need a TGraphic for drawing

begin
  if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile=False then EXIT;

  saved_screen_cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;

  dest_metafile:=TMetafile.Create;
  dest_metafile.Enhanced:=True;

  in_picture:=TPicture.Create;

  try
    with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]) do begin

      str:=bgnd_shape.shape_name;

      with bgimage.image_shape do begin

        in_picture.Assign(image_metafile);

        inwidth:=image_metafile.Width;
        inheight:=image_metafile.Height;

        try
          dest_metafile.Width:=inheight;
          dest_metafile.Height:=inwidth;
        except
          alert(2,'    image  preparation  failure',
                  'Preparation of the following picture shape for printing has failed:'
                 +'||'+str
                 +'||The failure was caused by insufficient resources in your system.',
                  '','','','','','O K',0);
          EXIT;
        end;//try

        dest_canvas:=TMetafileCanvas.Create(dest_metafile,0);    // 0 = use screen as reference device

        //dest_canvas.Lock;

        SetGraphicsMode(dest_canvas.Handle,GM_ADVANCED);
        SetMapMode(dest_canvas.Handle,MM_TEXT);

        FillChar(transform_matrix,SizeOf(transform_matrix),0);

              // clockwise 90 degs...

        transform_matrix.eM11:=0;  // Cos(Angle);
        transform_matrix.eM12:=1;  // Sin(Angle);
        transform_matrix.eM21:=-1; // -Sin(Angle);
        transform_matrix.eM22:=0;  // Cos(Angle);

        transform_matrix.eDx:=inheight;
        transform_matrix.eDy:=0;

        SetWorldTransform(dest_canvas.Handle,transform_matrix);

        dest_canvas.Draw(0,0,in_picture.Graphic);   // draw it rotated

        //dest_canvas.Unlock;

        dest_canvas.Free;   // freeing metafile canvas to create the metafile

        rotated_picture.Assign(dest_metafile);   // return rotated metafile in a Graphic

                                                 // rotate_metafile in shape not used

      end;//with image_shape

    end;//with shape object

  finally
    dest_metafile.Free;
    in_picture.Free;
    Screen.Cursor:=saved_screen_cursor;
  end;//try

end;
//______________________________________________________________________________
*)

procedure rotate_bitmap(i:integer);     // rotate bitmap supplied 90degs clockwise.

var
  inrow,incol:integer;
  inwidth,inheight:integer;
  pixel_colour:TColor;
  saved_screen_cursor:TCursor;
  str:string;

begin
  if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile=True then EXIT;   // 213b

  saved_screen_cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;

  try
    with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]) do begin

      str:=bgnd_shape.shape_name;

      with bgimage.image_shape do begin

        inwidth:=image_bitmap.Width;
        inheight:=image_bitmap.Height;

        rotated_bitmap.Monochrome:=image_bitmap.Monochrome;

        try
          rotated_bitmap.Width:=inheight;
          rotated_bitmap.Height:=inwidth;
        except
          alert(2,'    image  preparation  failure',
                  'Preparation of the following picture shape for printing has failed:'
                 +'||'+str
                 +'||The failure was caused by insufficient resources in your system.'
                 +'||Try re-scanning the image at a lower resolution, or if it is colour image try re-scanning in grey-scale or black and white.',
                  '','','','','','O K',0);
          EXIT;
        end;//try

        with rotated_bitmap do begin
          with Canvas do begin
            Brush.Color:=clWhite;          //  this clears the new bitmap for starters.
            Brush.Style:=bsSolid;
            FillRect(Rect(0, 0, Width-1, Height-1));
          end;//with
        end;//with

        if inheight<32767 then print_form.row_progressbar.Max:=inheight    // Delphi 16-bit limits..
                          else print_form.row_progressbar.Max:=32767;

        print_form.row_progressbar.Position:=0;

        for inrow:=0 to (inheight-1) do begin
          for incol:=0 to (inwidth-1) do begin
            pixel_colour:=image_bitmap.Canvas.Pixels[incol,inrow];
            if pixel_colour<>clWhite then rotated_bitmap.Canvas.Pixels[inheight-1-inrow,incol]:=calc_intensity(pixel_colour);
          end;//for
          print_form.row_progressbar.StepIt;
        end;//for

        rotated_picture.Graphic:=rotated_bitmap;
      end;//with
    end;//with
  finally
    print_form.row_progressbar.Position:=0;
    Screen.Cursor:=saved_screen_cursor;
  end;//try
end;
//________________________________________________________________________________________

procedure rotate_bitmap_by_dots(i:integer; p1,p2:Tpex; grid_left,grid_top:extended; source_canv,dest_canv:TCanvas);

       // rotate bitmap image and draw directly on printer, dot by dot.
       // this way of rotating the bitmap is incredibly slow.

       // dots and pixels are squares as on graph paper.
       // they are referenced by their top left corner. =  0 to (width-1) index.
       // the perceived position of a dot is its centre,
       // so all perceived images and printed material is actually shifted by a half-dot both ways from the logical origin (0,0).

var
  saved_screen_cursor:TCursor;
  saved_caption_str,name_str:string;
  inrow,incol:integer;

  width_pixels:integer;
  height_pixels:integer;

  width_mm_per_pixel:extended;
  height_mm_per_pixel:extended;

  xmm,ymm:extended;

  out1,out2:TPoint;

  pixel_X,pixel_Y:integer;
  blob_width, blob_height:integer;

  pixel_colour:TColor;

begin
  saved_screen_cursor:=Screen.Cursor;
  Screen.Cursor:=crHourglass;

  try
    name_str:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.shape_name;
    width_pixels:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap.Width;
    height_pixels:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap.Height;

    if (width_pixels<1) or (height_pixels<1) then EXIT;

    width_mm_per_pixel:=(p2.x-p1.x)/width_pixels;
    height_mm_per_pixel:=(p2.y-p1.y)/height_pixels;

    blob_height:=Round(100*width_mm_per_pixel*scal_out);  // y on output.
    blob_width:=Round(100*height_mm_per_pixel*scaw_out);  // x on output.

    if height_pixels<32767 then print_form.row_progressbar.Max:=height_pixels    // Delphi 16-bit limits..
                           else print_form.row_progressbar.Max:=32767;
    print_form.row_progressbar.Position:=0;

    saved_caption_str:=print_form.Caption;

    with dest_canv do begin    // set these once only..

      CopyMode:=cmSrcCopy;     // reset normal for destination Canvas.

      Brush.Style:=bsSolid;
      Pen.Width:=1;
      Pen.Mode:=pmCopy;
      Pen.Style:=psSolid;

    end;//with

           // p1 mm is bottom-left corner of image (top-left corner of bottom pixel).

    for inrow:=0 to (height_pixels-1) do begin
      for incol:=0 to (width_pixels-1) do begin

        xmm:=p1.x+incol*width_mm_per_pixel;                        // picture co-ordinates in mm..
        ymm:=p1.y+(height_pixels-1-inrow)*height_mm_per_pixel;     // (top-left corner of pixel).

        pixel_X:=Round((ymm*100+re_org_y-grid_left)*scaw_out)+page_left_dots;  // printer canvas co-ordinates..
        pixel_Y:=Round((xmm*100+re_org_x-grid_top)*scal_out)+page_top_dots;

        out1.X:=pixel_X;     // top left of enlarged dot..
        out1.Y:=pixel_Y;

        out2.X:=pixel_X+blob_width;       // bottom right of enlarged dot..
        out2.Y:=pixel_Y+blob_height;

        if check_if_on_page(out1,out2,True)=False then CONTINUE;  // blob not on this page or outside limits.

        print_form.Caption:='  '+name_str+'  row: '+IntToStr(inrow)+'  col: '+InttoStr(incol);

        pixel_colour:=calc_intensity(source_canv.Pixels[incol,inrow]);
        if pixel_colour=clWhite then CONTINUE;  // no need to print white! (make transparent).

        with dest_canv do begin

          Brush.Color:=pixel_colour;
          Pen.Color:=pixel_colour;

          Rectangle(out1.X, out1.Y, out2.X, out2.Y);      // print the blob.
        end;//with

      end;//for

      print_form.row_progressbar.StepIt;
    end;//for
  finally
    print_form.row_progressbar.Position:=0;
    print_form.Caption:=saved_caption_str;
    Screen.Cursor:=saved_screen_cursor;
  end;//try
end;
//_________________________________________________________________________________________

procedure Tprint_form.picture_dots_radioClick(Sender: TObject);

var
  i:integer;

begin
  repeat
    i:=alert(3,'    print  picture  shapes  ( dots  method )',
               'The BY DOTS method of printing background picture shapes is EXTREMELY slow.',
               '','more  information','omit  pictures','O K  -  print  pictures  using  dots  method','print  pictures  as  outlines  only','print  pictures  using  normal  method  instead',2);
    case i of
          2: if help(0,picture_help_str,'more  about  picture  shapes')=1 then bgnd_form.picture_shapes_help_menu_entry.Click;          // !!! don't use alert_help, print_form won't permit being deactivated.
          3: omit_pictures_radio.Checked:=True;
          5: picture_outlines_radio.Checked:=True;
          6: picture_stretch_radio.Checked:=True;
    end;//case
  until i<>2;
end;
//_____________________________________________________________________________________________

procedure Tprint_form.picture_help_buttonClick(Sender: TObject);

begin
  if help(0,picture_help_str,'more  about  picture  shapes')=1 then bgnd_form.picture_shapes_help_menu_entry.Click;
end;
//__________________________________________________________________________________________

function check_if_on_page(corner1,corner2:TPoint; check_within_limits:boolean):boolean;

begin
  RESULT:=False;  // default int.

  if (corner1.X<0)                   and (corner2.X<0)                   then EXIT;  // not on this page.
  if (corner1.X>printer_width_indexmax_dots)  and (corner2.X>printer_width_indexmax_dots)  then EXIT;  // not on this page.

  if (corner1.Y<0)                   and (corner2.Y<0)                   then EXIT;  // not on this page.
  if (corner1.Y>printer_length_indexmax_dots) and (corner2.Y>printer_length_indexmax_dots) then EXIT;  // not on this page.

  if check_within_limits=True
     then begin
            if check_limits(corner1, corner2)=False then EXIT;
          end;
  RESULT:=True;
end;
//___________________________________________________________________________________________

procedure Tprint_form.help_buttonClick(Sender: TObject);

begin
   if help(0,print_help_str,'about  mapping  colours')=1 then pad_form.marker_and_mapping_colours_help_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tprint_form.diagram_mode_radiobuttonClick(Sender: TObject);

begin
  if diagram_mode_radiobutton.Checked=True then pad_form.output_diagram_mode_menu_entry.Click;
  show_output_mode_panel;
end;
//______________________________________________________________________________

procedure Tprint_form.detail_mode_radiobuttonClick(Sender: TObject);

begin
  if detail_mode_radiobutton.Checked=True then pad_form.output_detail_mode_menu_entry.Click;
  show_output_mode_panel;
end;
//______________________________________________________________________________

procedure Tprint_form.print_sketchboard_items_checkboxClick(Sender:TObject);  // 206e

begin
  if print_sketchboard_items_checkbox.Checked=False then EXIT;

  do_open_source_bang('PRINT SKETCHBOARD ITEMS');  // OT-FIRST

  { OT-FIRST

  if go_sketchboard=True    // sketchboard in use?
     then begin
            dtp_form.dtp_document.ZoomPage;   // needed to print items -- calc from rulers.
            update_model_rulers;

            if trackplan_exists=False
               then begin
                      alert(6,'php/560    no  sketchboard  trackplan',
                              'You have selected the option to include sketchboard items in the printed output, but there is currently no trackplan item on the sketchboard.'
                             +'||Templot0 is unable to print sketchboard items if there is no trackplan reference available to set the scaled size.'
                             +'||To include items from the sketchboard in the printed output, add a trackplan item to the sketchboard.',
                              '','','','','cancel','',0);
                      print_sketchboard_items_checkbox.Checked:=False;   // and untick it.
                    end;
          end
     else begin
            alert(6,'php/500    sketchboard  not  in  use',
                    'You have selected the option to include sketchboard items in the printed output, but the sketchboard is not currently in use.'
                   +'||To start using the sketchboard click the `0sketchboard`1 button on the trackpad (top left).',
                    '','','','','cancel','',0);
            print_sketchboard_items_checkbox.Checked:=False;   // and untick it.
          end;}

end;
//______________________________________________________________________________

end.

