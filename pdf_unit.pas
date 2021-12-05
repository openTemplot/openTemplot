{
This unit is still very much a

             -----   WORK IN PROGRESS   ------

Aside from the fact that it is far from being a thing of beauty,
I have a list of things still to do, including:

- missing funtionality
 o do "mapping" colours
 o do timber infill styles
 o do selectable fonts
 o do scaling of fonts
 o etc

- general code refactoring
 o factor out a pdf_control() procedure as a mirror of pdf_bgnd()
 o merge pdf_control() and pdf_bgnd() into s aingle procedure

- minor niggles
 o and lots of etc

}
unit pdf_unit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, FPCanvas,
  pdf_lib_unit;

// T3-OUT WPPDFPRP, WPPDFR1, WPPDFR2, dtpShape,dtpGR32;

type

  { Tpdf_form }

  Tpdf_form = class(TForm)
    info_scrollbox: TScrollBox;
    printer_info_label: TLabel;
    page_panel: TPanel;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    datestamp_label: TLabel;
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
    header_label: TLabel;
    page_label: TLabel;
    font_button: TButton;
    origin_label: TLabel;
    printer_label: TLabel;
    row_progressbar: TProgressBar;
    help_button: TButton;
    help_shape: TShape;
    pdf_doc: TPDF_Document;
    pdf_save_dialog: TSaveDialog;
    warnings_checkbox: TCheckBox;
    black_edges_checkbox: TCheckBox;
    picture_borders_checkbox: TCheckBox;
    include_pictures_checkbox: TCheckBox;
    detail_mode_radiobutton: TRadioButton;
    diagram_mode_radiobutton: TRadioButton;
    include_sketchboard_items_checkbox: TCheckBox;
    row_label: TLabel;
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
    procedure how_panelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure banner_fill_checkboxClick(Sender: TObject);
    procedure next_row_buttonClick(Sender: TObject);
    procedure font_buttonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure black_edges_checkboxClick(Sender: TObject);
    procedure detail_mode_radiobuttonClick(Sender: TObject);
    procedure diagram_mode_radiobuttonClick(Sender: TObject);
    procedure include_sketchboard_items_checkboxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  pdf_form: Tpdf_form;

  //________________________
  // All set inside Texport_form.create_pdf_buttonClick

  pdf_width_mm: double = 180.0;    // same defaults as sketchboard
  pdf_height_mm: double = 260.0;

  pdf_width_dots: integer = 4252;    // 180mm at 600dpi
  pdf_height_dots: integer = 6142;   // 260mm at 600dpi

  pdf_width_dpi: integer = 600;      // default
  pdf_height_dpi: integer = 600;

  pdf_black_white: boolean = False;
  pdf_grey_shade: boolean = False;

procedure pdf_draw;     // draw control template or entire pad on the output

//___________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  // Some not needed at the moment - commented out
  // - reintroduce as/if the need arises ...
  //Printers, calibration_unit,
  config_unit,
  preview_unit, control_room, pad_unit, alert_unit, keep_select, math_unit,
  //gauge_unit,

  info_unit, colour_unit, bgnd_unit,
  //bgkeeps_unit,
  help_sheet, stay_visible_unit, panning_unit, shove_timber, grid_unit,
  //edit_memo_unit,
  print_settings_unit, print_unit,
  //entry_sheet,
  export_unit,

  // T3-OUT dtp_settings_unit, dtp_unit,
  background_shapes,
  rail_options_unit, platform_unit, check_diffs_unit, data_memo_unit,
  trackbed_unit, make_slip_unit;  // 214b

const
  pdf_help_str: string = '      Printing  Pages' +
    '||Behind this PRINT PAGES window you can see the layout of pages comprising your drawing. Drag and resize this window as necessary to get a clear view.' + ' (It is often useful to have resized the trackpad window beforehand to less than the full screen.)' + '||The tracks are drawn in skeleton form with rails only, but will print out fully detailed according to your current settings in the GENERATOR and PRINT menus.' + ' The rails are shown in different colours for the control template and background templates, the latter being shown as single rail-edges only.' + '||( The TOP of the printed pages corresponds to the LEFT MARGIN on the screen, so selecting between upright (portrait) and sideways (landscape) paper orientation might at first seem confusing.' + ' In most cases the pre-set upright (portrait) setting gives the best fit on the pages.)' + '||If PRINT CONTROL TEMPLATE was selected only pages containing it will be printed, along with any background items which happen to be on them.' + '||If PRINT ENTIRE TRACKPAD was selected, all the pages needed to contain any background templates will be printed.' + '||To enlarge or reduce the scale of the printed template, or change the printed colours or line thicknesses, select the various PRINT menu items.' + ' The page layout shown here (and on the trackpad) will change to reflect the new size.' + '||You can choose to print any or all of the pages shown:' + '||To create all the pages click the CREATE ALL REMAINING PAGES bar.' + '||To cancel any printing, click the OMIT ALL REMAINING PAGES bar or press the F12 or Esc keys.' + '||To create individual page(s), click the OMIT PAGE button until the NEXT PAGE display shows the page you require. Then click the CREATE PAGE button.' + '||To omit all the pages in a row, or the remaining pages in the current row, click the NEXT ROW button.' + '||Continue clicking the OMIT PAGE, CREATE PAGE and NEXT ROW buttons as required. If no more pages are required click the OMIT ALL REMAINING PAGES bar. To print' + ' all the remaining pages click the CREATE ALL REMAINING PAGES bar.' + '|--------------' + '|Handy Hint:' + '|To rapidly run through the pages of a large plan you can repeatedly select the OMIT PAGE, CREATE PAGE or NEXT ROW buttons by using the accelerator keys marked on the buttons' + ' (i.e. hold down the O key on the keyboard to omit a run of pages, the C key to create a run of pages, or the N key to omit several rows of pages).' + '|--------------' + '||To change the font used for the page labels on the preview screen click the FONT... button.' + '||To print a copy of this preview screen click the PRINT PAGE MAP button. This is a useful guide to joining the pages when you are printing a large plan.' + ' (If the button is clicked after page printing has started, the page map will be printed when page printing has finished. The font used for the page labels on the page map' + ' can be changed by selecting the PROGRAM > PRINTER FONT + MARGINS... menu item on the PROGRAM PANEL window.)' + '||If the page margins do not conveniently fit the paper size or grid spacings being used, the trim margins can be changed. Return to the trackpad (F12) and then click the PRINT > TRIM MARGINS > ?�TRIM MARGINS HELP menu item for more information.' + '||If the drawing is not conveniently placed between the page margins, the page origin can be moved. Return to the trackpad (F12) and then click either the PRINT > PAGE ORIENTATION / ORIGIN > SET PAGE ORIGIN... menu item' + ' to enter the new page origin position directly, or the ACTION > MOUSE ACTIONS:PAD > MOVE PAGE ORIGIN menu item (SHIFT+CTRL-F10) to move the page origin with the mouse.' + '||Likewise when printing the entire trackpad at a reduced size, the print size can be changed by mouse action to achieve a convenient fit to the pages. Click the ACTION > MOUSE ACTIONS:PAD > ADJUST PRINT SIZE menu item.' + ' The page outlines on the trackpad will change accordingly.' + '|--------------' + '||There are four OPTIONS > tickboxes:' + '||If INFORMATION PAGE is ticked, a page containing the INFO details from the information panel will be printed to accompany the control template.' + '||This is useful when printing final construction templates as a permanent written record, but you will probably want to untick the box when doing trial templates to save paper.' + ' Any MEMO notes for the control template will also be added.' + '||This option box has no effect when the entire trackpad is being printed, even if the control template is included.' + ' The text font used for this page can be changed - select the PROGRAM > PRINTER FONT + MARGINS... menu item on the PROGRAM PANEL window.' + '||If BLACK RAIL-EDGES is ticked, all rail edges will be printed in black regardless of any other colour, print-intensity or grey-shade settings which you may have made.' + '||This option is useful when the print intensity has been reduced (see below),' + ' or when printing the background templates in their mapping colours, or in a single non-black colour ( PRINT > PRINTED DRAWING OPTIONS > CLOLOUR OPTIONS > PRINT ALL TEMPLATES IN A SINGLE COLOUR menu item),' + ' but you want to retain a full black for the rail edge lines.' + '||If WARNINGS is ticked, a warning will be printed on each page if the printer is uncalibrated, or if data distortions are in force. For more information about' + ' printer calibration, select the PRINT > PRINTER CALIBRATION > CALIBRATE PRINTER... menu item.' + '||For information about data distortions, select the PROGRAM > EXPERT menu items on the PROGRAM PANEL menus.' + '||When printing on single sheets of paper Templot ignores any empty (blank) pages and prints only the pages of your drawing which actually contain track.' + '||If BANNER FILL is ticked, any such empty pages will be included in the print run when printing on banner or roll paper, so that no lengthwise page joins are needed. Bear in mind that for some track plans' + ' this could mean printing a great many blank pages (e.g. for a circular layout you would be printing blank pages to fill the whole of the centre space).' + ' In such cases you will probably want to untick the box and separate out the individual pages from the banner print run.' + ' This option box has no effect when printing on single sheets.' + '||The PRINT INTENSITY adjuster is useful to darken trial templates when using the printer''s Draft/Fast print-quality mode, without upsetting your colour settings for final templates using the Best/Letter-Quality mode.' + '||Change the setting by sliding the adjuster, or by clicking the LIGHT and DARK labels. The RESET button restores the normal print intensity setting.' + '||The setting is also reset to normal after changing any of the print colours (PRINT > PRINTED DRAWING OPTIONS menu items).' + '||The intensity setting modifies the print colours, not the line thicknesses (line widths) which can be set separately using the PRINT > PRINTED LINE THICKNESS menu items.' + '||When using a reduced print intensity, you can ensure that the rail edges remain a full black by ticking the BLACK RAIL-EDGES option box (see above).' + '||The print intensity adjustment is not available when printing in black && white only. Select GREY-SHADE PRINTING instead.' + '||Printers vary considerably in performance and you may need to experiment to achieve the optimum results from your particular printer.' + '||This print intensity setting is internal to Templot. Many printers also include setup options to vary the ink intensity and to print' + ' in grey-shades (gray-scale). You can compare the results from these hardware options with those from changing the settings here in Templot.' + '||For some additional notes about colour options for printing, click the ABOUT MAPPING COLOURS button below.' + '|-----------------' + '||N.B. If PRINT ALL REMAINING PAGES has been selected you can abort the sending of pages to the printer by pressing the F12 or Esc keys, but any already sent will continue to be printed.' + '||After all the pages have been sent, Templot returns to the trackpad so that you can continue working while the pages are being printed. To abort printing at this stage you must use the Windows' + ' printer controls (double-click on the printer icon in the taskbar, in the window which appears click the TEMPLOT PAGES entry, then in the DOCUMENT menu, select CANCEL PRINTING).' + '||When printing all the pages of a complete track plan at full-size, you may be printing several dozen pages of graphics. If your system memory or disk space is limited, the Windows' + ' printer spooler and/or your printer driver software may have problems. Try changing the spooler settings, or printing directly to the printer.' + ' To make these changes, from the Windows taskbar click Start > Settings > Printers > File menu > Properties > Details tab > Spool Settings, or consult Windows Help and your printer documentation for further details.' + '||If you print directly to the printer Templot sends pages for printing one at a time, so you will be able to abort printing after each page by pressing F12 or Esc,' + ' but you will not be able to continue working in Templot until all of the required pages have been printed.' + '||( The difference between pressing F12 or Esc is that F12 additionally unlocks the trackpad redraw if you have been printing with the redraw locked. For more information about locking the redraw, see' + ' the help notes in the REAL > TIMBERING > TIMBERING DATA... menu item. Locking the redraw is unnecessary when timber randomizing is switched off, or when printing only background templates between rebuilds.' + ' If you have not locked the redraw, there is no difference between F12 and Esc.)' + '||Handy Hints :' + '||When printing the entire trackpad the control template is normally omitted and only the templates comprising the background drawing are printed. To include the control template,' + ' select the PRINT > ENTIRE TRACKPAD OPTIONS > INCLUDE CONTROL TEMPLATE menu item.' + '||If your background contains a great many templates, it could take up to a minute or more before the printer starts printing. This is true even if you are printing only the control template on a single page.' + ' To speed up printing, temporarily wipe unwanted background templates from the trackpad.' + '||If your printer is capable of printing continuous banners on Z-fold or roll paper this can usefully eliminate most (or all) of the page-joins for a large template.' + ' It is important to make the correct settings for the printer. Click the PRINT > BANNER / ROLL PAPER menu item, and read the help notes.';

  picture_help_str: string = '      Printing  Background  Picture  Shapes' +
    '||The background picture shapes are intended mainly for use on the screen. It is recommended that you normally print them as rectangle outlines only on your track templates.' + '||Including large bitmap images in the printed pages may significantly increase printing time, and is not supported on all printers.' + '||If possible the STRETCH option should be used to print them. However, this is a Windows function which may fail at high magnifications of the original image, and is not available on some printers.' + ' If you are using Windows NT, 2000 or XP, see also the PROGRAM > EXPERT > GRAPHICS LIMITS > ?�GRAPHICS LIMITS - HELP menu item on the PROGRAM PANEL window.' + '||The DOTS option will work at all magnifications and on most printers, but is extremely slow, and should be regarded as a last resort. It is significantly faster for images scanned in black and white only.' + ' In many cases the DOTS method will work better if you change the printer spooler setting to RAW data (see below).' + '||For the STRETCH method, if the TRANSPARENT option is set for a picture shape (in the BACKGROUND SHAPES window) it will be printed with underlying detail showing through (if the printer supports raster functions).' + ' Otherwise underlying detail will be covered over, including the grid lines and page trim margins.' + ' To avoid this, ensure that the GRID IN FRONT tickbox is selected.' + '||For the DOTS method the TRANSPARENT option does not apply and is ignored. However, any white areas of the image will always be printed as transparent.' + '||Both methods adjust the intensity of the image according to the current PRINT INTENSITY setting.' + '||Printing large bitmaps makes great demands on your system''s memory and resources. Don''t have more picture shapes on the trackpad than you need,' + ' and keep bitmap files as small as possible by cropping off unwanted areas or by scanning at a lower resolution or in grey-scale instead of colour.' + ' Close any other applications which you have running. If lengthy disk drive activity takes place - please be patient.' + ' If you experience problems, quit Templot, restart Windows, restart Templot and try again.' + '||Printing performance may also be improved by changing from EMF to RAW data, or by printing directly to the printer instead of via the Windows spooler.' + ' To make these changes, from the Windows taskbar click Start > Settings > Printers > select printer > File menu > Properties > Details tab > Spool Settings, or consult Windows Help and your printer documentation for further details.' + '||If problems persist, restart Templot and print background picture shapes as outlines only.' + '||N.B. PLEASE BE AWARE that printing scanned images may require the permission of the copyright owner.' + '||For more information about using Background Shapes and bitmap images, click MORE ABOUT PICTURE SHAPES below.';

  pen_color: Integer = -1;

  min_penwidth: TPDF_PenWidth = 0.1; // in mm, of course, like all line thicknesses

var
  button_clicked: boolean = False;
  banner_changed: boolean = False;
  printer_printing: boolean = False;
  index_sheet_wanted: boolean = False;

  info_was_showing: boolean = False;
  panning_was_showing: boolean = False;
  shove_was_showing: boolean = False;
  spacing_was_showing: boolean = False;

  rail_options_was_showing: boolean = False;
  platform_was_showing: boolean = False;
  trackbed_was_showing: boolean = False;
  check_diffs_was_showing: boolean = False;
  data_child_was_showing: boolean = False;
  stay_visible_was_showing: boolean = False;

  form_scaling: boolean = False;

      // all line thicknesses are in mm
  printgrid_wide: TPDF_PenWidth;      // default.
  printpicborder_wide: TPDF_PenWidth;
  printmargin_wide: TPDF_PenWidth;
  printshape_wide: TPDF_PenWidth;
  printrail_wide: TPDF_PenWidth;
  printtimber_wide: TPDF_PenWidth;
  printmark_wide: TPDF_PenWidth;
  printcl_wide: TPDF_PenWidth;        // track centre-line  0.79.a


  his_pdf_file_name: string = '';

// all standard pdf fonts - useage preferred to reduce file size
  pdf_font_times: Integer;
  pdf_font_times_B: Integer;
  //pdf_font_times_I: Integer;
  //pdf_font_times_BI: Integer;
  pdf_font_helv: Integer;
  //pdf_font_helv_B: Integer;
  //pdf_font_helv_I: Integer;
  //pdf_font_helv_BI: Integer;
  //pdf_font_courier: Integer;
  //pdf_font_courier_B: Integer;
  //pdf_font_courier_I: Integer;
  //pdf_font_courier_BI: Integer;
  //pdf_font_Symbol: Integer;
  //pdf_font_ZapfDingbats: Integer;

  lsGrid: Tpdf_LineStyle;
  lsWatermark: Tpdf_LineStyle;
  lsCentreline_normal: Tpdf_LineStyle;
  lsCentreline_dummy: Tpdf_LineStyle;

  lsMark_Default: Tpdf_LineStyle;
  lsMark_Guide: Tpdf_LineStyle;
  lsMark_CurvingRadCentre: Tpdf_LineStyle;
  lsMark_RadialEnd: Tpdf_LineStyle;
  lsMark_TimberOutline:     Tpdf_LineStyle;
  lsMark_TimberCentreline:  Tpdf_LineStyle;
  lsMark_TimberRivetCentreline:  Tpdf_LineStyle;
  lsMark_TimberReducedEnd:  Tpdf_LineStyle;
  lsMark_RailJoint: Tpdf_LineStyle;
  lsMark_Slewing: Tpdf_LineStyle;
  lsMark_PegArm: Tpdf_LineStyle;
  lsMark_PlainTrackStart: Tpdf_LineStyle;
  lsMark_ShovingCentreline: Tpdf_LineStyle;
  lsMark_SwitchDrive: Tpdf_LineStyle;
  lsMark_TimberLongmarks: Tpdf_LineStyle;

  lsRail: Tpdf_LineStyle;
  lsPlatform: Tpdf_LineStyle;
  lsBlanking: Tpdf_LineStyle;
  lsEdge: Tpdf_LineStyle;

procedure pdf_bgnd(grid_left, grid_top: double; pdf_page: TPDF_page); forward;
// print background items.
procedure pdf_bgnd_shapes(grid_left, grid_top: double); forward; // print all background shapes.
procedure pdf_rotate_bitmap(i: integer); forward;     // rotate bitmap for picture shape.

// T3-OUT  procedure pdf_sketchboard_items(on_canvas:TCanvas; grid_left,grid_top:double);forward;   // 206e
// T3-OUT  procedure pdf_rotate_metafile(i:integer);forward;   // rotate metafile supplied 90degs clockwise.   213b

//_______________________________________________________________________________________

procedure disable_buttons;

begin
  with pdf_form do begin
    omit_page_button.Enabled := False;
    next_row_button.Enabled := False;
    ok_button.Enabled := False;
    all_button.Enabled := False;
    all_panel.Enabled := False;
  end;//with
end;
//___________________________________________________________________________________

procedure enable_buttons(next_row_enabled: boolean);

begin
  with pdf_form do begin
    omit_all_button.Enabled := True;
    omit_page_button.Enabled := True;
    next_row_button.Enabled := next_row_enabled;

    ok_button.Enabled := True;
    all_button.Enabled := True;

    omit_all_panel.Enabled := True;
    all_panel.Enabled := True;

  end;//with
end;
//___________________________________________________________________________________

function mm_to_dots(mm: Double): Integer;
begin
  Result := round((mm / 25.4) * pdf_width_dpi);
end;

//___________________________________________________________________________________

function calc_intensity(colour: integer): integer;

var
  red, green, blue, av: integer;    // colour components (0-255).

begin
  Result := colour;  // default init

  if colour = clWhite then
    EXIT;

  if pdf_black_white or (colour = virtual_black_colour) then begin
    Result := clBlack;
    EXIT;
  end;

  if pdf_grey_shade            // average to a grey..
  then begin
    try
      red := (colour and $000000FF);
      green := (colour and $0000FF00) div $100;
      blue := (colour and $00FF0000) div $10000;

      // The 'magic numbers' below are explained at
      //     https://en.wikipedia.org/wiki/Grayscale
      av := round(min((red * 0.2126 + green * 0.7152 + blue * 0.0722), 255));

      red := av;
      green := av;
      blue := av;

      Result := (blue * $10000) + (green * $100) + red;

    except
      Result := clGray;
    end;//try
  end;
end;
//____________________________________________________________________________________

procedure print_colours_setup;  // set grey shades and/or print intensity.

begin
  if pdf_black_white then begin
    print_railedge_colour := clBlack;

    printcurail_colour := clBlack;
    printbgrail_colour := clBlack;

    printtimber_colour := clBlack;

    printrail_infill_colour_cu := clWhite;      // !!! used for solid infill.
    printrail_infill_colour_bg := clWhite;      // !!! used for solid infill.

    printtimber_infill_colour := clBlack;       // !!! only used for hatched infill.
    printgrid_colour := clBlack;
    printmargin_colour := clBlack;
    printguide_colour := clBlack;
    printjoint_colour := clBlack;
    printalign_colour := clBlack;

    print_labels_font.Color := clBlack;
    print_timber_numbers_font.Color := clBlack;

    printshape_colour := clBlack;
    printbg_single_colour := clBlack;

    printplat_edge_colour := clBlack;    // platform edges
    printplat_infill_colour := clBlack;  // platform infill

    sb_track_bgnd_colour := clWhite; // 206a
    sb_diagram_colour := clWhite;    // 209c

  end
  else begin
    //  calc grey shades if wanted...

    if pdf_form.black_edges_checkbox.Checked then
      print_railedge_colour := clBlack
    else
      print_railedge_colour := calc_intensity(save_prc);

    printcurail_colour := print_railedge_colour;
    printbgrail_colour := print_railedge_colour;

    printtimber_colour := calc_intensity(save_ptc);

    printrail_infill_colour_cu := calc_intensity(save_priccu);
    printrail_infill_colour_bg := calc_intensity(save_pricbg);

    printtimber_infill_colour := calc_intensity(save_ptic);
    printgrid_colour := calc_intensity(save_grc);
    printmargin_colour := calc_intensity(save_pmc);
    printguide_colour := calc_intensity(save_pgc);
    printjoint_colour := calc_intensity(save_pjc);
    printalign_colour := calc_intensity(save_pac);

    print_labels_font.Color := calc_intensity(save_fc);
    print_timber_numbers_font.Color := calc_intensity(save_tnfc);

    print_corner_page_numbers_font.Color := calc_intensity(save_cpnfc);     // 0.93.a added

    printshape_colour := calc_intensity(save_psc);
    printbg_single_colour := calc_intensity(save_pbg);

    printplat_edge_colour := calc_intensity(save_priplatedge);   // platform edges
    printplat_infill_colour := calc_intensity(save_priplatfill); // platform infill

    sb_track_bgnd_colour := calc_intensity(save_sb_track_bgnd);  // 206a
    sb_diagram_colour := calc_intensity(save_sb_diagram_col);    // 209c
  end;
end;
//__________________________________________________________________________________________

procedure print_line_thickness_setup;

var
  av_dpi: double;

  ///////////////////////////////////////////////

  function calc_thick(mm_thick: double; adjust: boolean): integer;
    // calc line thickness in dots.

  var
    line_dots: double;

  begin
    line_dots := mm_thick * av_dpi / 25.4;
    if adjust and pad_form.adjust_line_thickness_menu_entry.Checked then
      line_dots := line_dots * out_factor;

    Result := Round(line_dots);
    if Result < 1 then
      Result := 1;
  end;

  ///////////////////////////////////////////////

  function scale_thickness(thickness: double): double;
    // scale line thickness, if needed

  begin
    if pad_form.adjust_line_thickness_menu_entry.Checked then
      Result := thickness * out_factor
    else
      Result := thickness;

  end;
  ///////////////////////////////////////////////

begin
  av_dpi := (nom_width_dpi + nom_length_dpi) / 2;

  printgrid_wide := printgrid_thick;
  //// don't reduce line thickness for scaled output - the paper size is still the same.
  printmargin_wide := printmargin_thick;  // ditto.

  printshape_wide := scale_thickness(printshape_thick);
  printpicborder_wide := scale_thickness(printpicborder_thick);
  printrail_wide := scale_thickness(printrail_thick);
  printtimber_wide := scale_thickness(printtimber_thick);
  printcl_wide := scale_thickness(printcl_thick);    // 0.79.a
  printmark_wide := scale_thickness(printmark_thick);


end;
//__________________________________________________________________________________________

{ Take a point in 'pad' space coordinates (with an optional x/y adjustment)
  and relocate to an equivalent point in page space coordinates.
  This absracts a computation wihich occurs in MANY palces}
function page_locate(point: Tpoint; grid_left, grid_top: double; adj: array of double): Tpoint; overload;
var
  loc: Tpoint;
begin
  loc.X := Round((point.Y + adj[0] - grid_left) * scaw_out) + page_left_dots;
  loc.Y := Round((point.X + adj[1] - grid_top)  * scal_out) + page_top_dots;
  RESULT := loc;
end;

function page_locate(point: Tpoint; grid_left, grid_top: double): Tpoint; overload;
begin
  RESULT := page_locate(point, grid_left, grid_top, [0, 0]);
end;
//__________________________________________________________________________________________

function switch_label_text(code: Integer): String;
begin
  case code of
    601:
      RESULT := 'tips';
    602:
      RESULT := 'set (bend)';
    603:
      RESULT := 'planing';
    604:
      RESULT := 'stock gauge';
    605:
      RESULT := 'joggles';
    701:
      RESULT := 'intersection FP';
    702:
      RESULT := 'blunt nose';
    703:
      RESULT := 'blunt tips';
    else
      RESULT := 'code ' + inttostr(code) + ' mark';
  end;//case
end;
//__________________________________________________________________________________________

procedure make_pdf_preview_screenshot;   // 214b

var
  create_png: TPortableNetworkGraphic;

  file_str: string;       // including path

begin
  file_str := Config.FilePath(cudiData,
           'PDF-PAGEMAP-RECORD-FILES\pdf_pagemap' +
           FormatDateTime('_yyyy_mm_dd_hhmm_ss', Date + Time) + '.png');

  create_png :={TPNGObject}TPortableNetworkGraphic.Create;
  try
    try
      create_png.Assign(offdraw_bmp);
      create_png.SaveToFile(file_str);
    except
      ShowMessage('Sorry, an error occurred in creating the page-map record file.'
        + #13 + #13 + 'This doesn''t prevent a PDF file being created.');
    end;//try

  finally
    create_png.Free;
  end;//try
end;
//______________________________________________________________________________

function wanted_mark(code: Integer): Boolean;     // decide if a particular mark is wanted

// This function determines whether a particular mark (indicated by it's code)
// is required on the output as determined by the corresponding user settings

  // NOTE: it is used for both control template output and background output

begin
  RESULT := true;

  with print_settings_form do begin;
    case code of
      -5 .. 0: // no name label, timber selector, curve rad or peg centre, blank
        RESULT := False;
      1:
        RESULT := output_guide_marks_checkbox.Checked;
      2:
        RESULT := output_radial_ends_checkbox.Checked;
      3:
        RESULT := output_timbering_checkbox.Checked;
      4:
        RESULT := output_timbering_checkbox.Checked and
                  output_timber_centres_checkbox.Checked;
      5:
        RESULT := output_timbering_checkbox.Checked;
      6:
        RESULT := output_rail_joints_checkbox.Checked;
      7:
        RESULT := output_radial_ends_checkbox.Checked;
      8, 9, 10: // no peg arms, plain-track end marks
        RESULT := False;
      14:
        RESULT := output_timbering_checkbox.Checked and
                  output_timber_centres_checkbox.Checked;
      33:
        RESULT := output_timbering_checkbox.Checked;
      44, 54:
        RESULT := output_timbering_checkbox.Checked and
                  output_timber_centres_checkbox.Checked;
      55, 93, 95, 99:
        RESULT := output_timbering_checkbox.Checked;
      101:
        RESULT := output_switch_drive_checkbox.Checked;
      203, 233, 293:
        RESULT := output_timbering_checkbox.Checked;
      480..499:
        RESULT := output_chairs_checkbox.Checked;
      501..508: // no check-rail labels
        RESULT := False;
      600, 601..605:
        RESULT := output_switch_labels_checkbox.Checked;
      700..703:
        RESULT := output_xing_labels_checkbox.Checked;
      else
        show_modal_message('Unhandled code ' + IntToStr(code) + 'in pdf_unit.wanted_mark().');
    end;
  end;

end;
//______________________________________________________________________________

function choose_mark_linestyle(pdf_page: TPDF_page; code: Integer; control: boolean): Tpdf_LineStyle; overload;
// return the line style for a particular mark code

begin
  case code of
    -2, -3:                   // curving rad centres
      Result := lsMark_CurvingRadCentre;
    1, 101:                   // guide marks. switch drive
      Result := lsMark_Guide;
    2:                        // rad end marks
      Result := lsMark_RadialEnd;
    3, 33, 93:                // timber outlines
      Result := lsMark_TimberOutline;
    4,44:                     // timber centre-lines
      Result := lsMark_TimberCentreline;
    5,55,95:                  // timber reduced ends.
      Result := lsMark_TimberReducedEnd;
    6:                        // rail joint marks.
      Result := lsMArk_RailJoint;
    7:                        // transition & slewing ends
      Result := lsMark_Slewing;
    14:                       // timber centre-lines with rail centre-lines (for rivet locations?).
      Result := lsMark_TimberRivetCentreline;
    600, 700:                 //  long marks
      Result := lsMark_TimberLongmarks;
    else
      Result := lsMark_Default;
  end;


  // ! ! ! THIS SHOULD NOT BE IN HERE ! ! !  --  FIND IT A NEW HOME --

  // do fill colours
  with pdf_page do begin
      case code of
        99: begin                             // timber numbering
          set_fill_colour(clBlack);
          if not control and (mapping_colours_print < 0) then
              set_pen_colour(printbg_single_colour);
          end;
        203, 233, 293: begin                  // timber infill
          set_pen_colour(clwhite);             // so don't overdraw timber outlines.
          if pdf_black_white then
            set_fill_colour(clBlack)
          else
            set_fill_colour(printtimber_infill_colour);
          end;
        600..605, 700..703:                   // timber numbers
          set_fill_colour(printguide_colour);
        else                                  // -- DEFAULT --
          set_fill_colour(clRed);       // Let's make it obvious when we mess up!
      end;
  end; // with pdf_page

end;

function choose_mark_linestyle(pdf_page: TPDF_page; code: Integer): Tpdf_LineStyle; overload;
begin
  Result := choose_mark_linestyle(pdf_page, code, True);
end;
//______________________________________________________________________________


procedure pdf_draw;     // draw control template or entire pad on the output.  // 0.91.d pdf

// n.b. this code tries to draw the entire output on every sheet,
// and relies on fpPDF to crop it to the current page rectangle.

var
  infill_points: array[0..3] of TPoint;   // array of corners for infilled timbers.

  gridx, gridy, now_gridx, now_gridy: double;
  grid_label: double;

  down: double;                  // temp x and y from list.
  across: double;

  sheet_down: integer;             // current sheet index.
  sheet_across: integer;           // ditto

  max_sheet_across: integer;       // highest used.

  gridco: integer;
  grid_now_dots: integer;

  i, aq, rail, now, now_max, dots_index, mark_code: integer;

  w_dots, l_dots, w_dots1, l_dots1, w_dots2, l_dots2: integer;

  w1, l1, w2, l2, wmid, lmid: integer;

  all_pages: boolean;              // False = print one page at a time.
  page_count: integer;
  row_count: integer;

  page_num_str: string;
  page_str, info_str, top_str, bottom_str: string;
  xing_str: string;
  temp_str: string;
  pgco_str: string;
  stry: integer;

  p1, p2, p3, p4: Tpoint;
  radcen_arm: double;

  move_to, line_to: TPoint;

  l_dims_valid: boolean;
  w_dims_valid: boolean;

  banner_top_done: boolean;

  x_dots, y_dots: integer;
  bangrid_dots: integer;

  pen_width: TPDF_PenWidth;

  ptr_1st, ptr_2nd: ^Tmark;          // pointers to a Tmark record.   ###
  markmax: integer;

  fontsize: double;
  num_str, tbnum_str: string;

  grid_str: string;
  grid_label_str: string;

  folder_str: string;

  switch_label_str: string;  // 206b

  all_pages_origin_str, this_page_begin_str, this_page_end_str: string;  // 208g

  last_file_str: string;  // 214a

  ident_left, ident_top, wm_shift: integer;  // 214a

  preview_record_file_made: boolean;   // 214b

  file_str: string;  // 217b

  pdf_filename_str: string;

  pdf_page: TPDF_Page;

  /////////////////////////////

  procedure begin_page;
  begin
    pdf_page := pdf_form.pdf_doc.new_page; // Start a new page
    // Portrait by default
    if export_form.pdf_side_run_button.Checked then
      pdf_page.set_landscape;
  end;

  /////////////////////////////

  procedure end_page(no_cancel: boolean);

  //var

  begin
    //show_modal_message('end_page');
  end;

  /////////////////////////////

  procedure create_styles;

  var
    width:  TPDF_PenWidth;  // Pen width
    colour: Tcolor;         // Pen colour
    style:  TFPPenStyle;    // Pen style

    //+++++++++++++++++++++++++//
    function colour_using(colour: Integer): Integer;
    begin
      if pdf_black_white then
        Result := clBlack
      else
        Result := colour;
    end;
    //+++++++++++++++++++++++++//

  begin
    with pdf_form.pdf_doc do begin
      // Grid
      if pad_form.printed_grid_dotted_menu_entry.Checked then
        style := psDot
      else
        style := psSolid;
      lsGrid := create_LineStyle(printgrid_wide, printgrid_colour, style);

      // Watermark
      if pdf_black_white then
        lsWatermark := create_LineStyle(0.5, clBlack, psSolid)
      else
        lsWatermark := create_LineStyle(1, $00D0D0D0, psSolid); // pale-ish grey

      // Marks
      lsMark_Default := create_LineStyle(1, clBlack, psSolid);
      lsMark_CurvingRadCentre := create_LineStyle(printmark_wide, clBlack, psSolid);
      lsMark_Guide := create_LineStyle(printmark_wide, colour_using(printguide_colour), psSolid);
      lsMark_RadialEnd := create_LineStyle(printmark_wide, colour_using(printalign_colour), psSolid);
      lsMark_TimberOutline := create_LineStyle(printtimber_wide, colour_using(printtimber_colour), psSolid);
      lsMark_TimberCentreline := create_LineStyle(min_penwidth, clBlack, psDash);
      lsMark_TimberRivetCentreline := create_LineStyle(1, clBlack, psSolid);
      lsMark_TimberReducedEnd := create_LineStyle(1, clBlack, psDot);
      lsMark_RailJoint := create_LineStyle(printmark_wide, colour_using(printjoint_colour), psSolid);
      lsMark_Slewing := lsMark_RadialEnd;
      lsMark_PegArm := lsMark_Default;
      lsMark_PlainTrackStart := lsMark_Default;
      lsMark_ShovingCentreline := create_LineStyle(printrail_wide, clBlack, psSolid);
      lsMark_TimberLongmarks := create_LineStyle(round(printrail_wide * 1.5), colour_using(printguide_colour), psSolid);

      // Rails
      lsCentreline_normal  := create_LineStyle(printcl_wide, printbgrail_colour, psDash);
      lsCentreline_dummy  := create_LineStyle(printshape_wide, printshape_colour, psSolid);
      // Note that the colours of these styles may be changed on a per template basis
      lsRail := create_LineStyle(printrail_wide, clBlack, psSolid);
      lsPlatform := create_LineStyle(printrail_wide, clBlack, psSolid);
      lsBlanking := create_LineStyle(printrail_wide, clBlack, psSolid);
      lsEdge := create_LineStyle(printrail_wide, clBlack, psSolid);
    end;
  end;


  /////////////////////////////

  procedure begin_doc;

  begin
    printer_printing := True;
    pdf_form.pdf_doc := TPDF_Document.Create(nil);
    with pdf_form.pdf_doc do begin
      with Infos do begin
        Title := Application.Title;
        Author := 'Graeme Defty';
        Producer := 'Templot 3';
        ApplicationName := ApplicationName;
        CreationDate := Now;
      end;
      // add fonts to doc
      pdf_font_times := Addfont('Times-Roman');
      pdf_font_times_B := Addfont('Times-Bold');
      //pdf_font_times_I := Addfont('Times-Oblique');
      //pdf_font_times_BI := Addfont('Times-BoldOblique');
      pdf_font_helv := Addfont('Helvetica');
      //pdf_font_helv_B := Addfont('Helvetica-Bold');
      //pdf_font_helv_I := Addfont('Helvetica-Oblique');
      //pdf_font_helv_BI := Addfont('Helvetica-BoldOblique');
      //pdf_font_courier := Addfont('Courier');
      //pdf_font_courier_B := Addfont('Courier-Bold');
      //pdf_font_courier_I := Addfont('Courier-Oblique');
      //pdf_font_courier_BI := Addfont('Courier-BoldOblique');
      //pdf_font_Symbol
      //pdf_font_ZapfDingbats
    end;
    create_styles;
    begin_page;
  end;

  /////////////////////////////

  procedure end_doc(no_cancel: boolean);

  var
    pdf_size_str: string;
    i: integer;
    pdf_file: TFileStream;

  begin
    try
      // T3-OUT                  pdf_form.pdf_printer.EndPage;
      // T3-OUT                  pdf_form.pdf_printer.EndDoc;
      begin
        try
          pdf_file := TFileStream.Create(pdf_filename_str, fmCreate);
          pdf_form.pdf_doc.SaveToStream(pdf_file);
          show_modal_message('Document saved using ' +
            IntToStr(pdf_form.pdf_doc.ObjectCount) + ' PDF objects/commands');
        finally
          pdf_form.pdf_doc.Free;
        end;
      end;
      if (pdf_height_mm > 3000) or (pdf_width_mm > 3000) then
        pdf_size_str := '|||<TABLE><TR><TD VALIGN="TOP">rp.gif&nbsp;</TD><TD>green_panel_begintree.gif   Large PDF page sizes:'
          +
          '||If the PDF file does not display properly in Adobe Reader the most likely reason is that the page size exceeds the limit for Adobe Reader.' + '||Other free PDF reader programs are available which will display and print much larger page sizes.' + ' For more information and download links, click <A HREF="alert_online.85a">more information online</A> .' + 'green_panel_end</TD></TR></TABLE>'
      else
        pdf_size_str := '';

      i := alert(2, 'php/260   PDF  file  created',
        'The PDF file was created successfully:'
        // T3-OUT                   +'||`0'+pdf_form.pdf_printer.Filename+'`f'
        + '||Click <A HREF="alert_3.85a">open PDF file</A> to open the file in your PDF reader.'
        + pdf_size_str, '', '',
        'open  PDF  file', 'open  the  containing  folder', '', 'continue', 0);

      if i = 3 then begin
        // T3-OUT                   folder_str:=pdf_form.pdf_printer.Filename;
        if not OpenDocument(PChar(folder_str))
        // { *Converted from ShellExecute* }<=32
        then
          ShowMessage('Sorry, unable to open the file.')
        else
          external_window_showing := True;
      end;

      if i = 4 then begin
        // T3-OUT                   folder_str:=ExtractFilePath(pdf_form.pdf_printer.Filename);
        if not OpenDocument(PChar(folder_str))
        // { *Converted from ShellExecute* }<=32
        then
          ShowMessage('Sorry, unable to open the folder.')
        else
          external_window_showing := True;
      end;

    except
      ShowMessage('Sorry, an error occurred in creating the PDF file.');
    end;//try

    printer_printing := False;
  end;
  /////////////////////////////

  procedure draw_grid(grid_left, grid_top: double; page: TPDF_page);

  begin
  //          Font.Assign(print_labels_font);
    with page do begin
      write_comment(' ----==== Grid Start ====----');
      save_graphics_state();

      set_font(pdf_font_times_B, 8);
      if printgrid_i = 1 then begin
        case grid_labels_code_i of
          1:
            grid_str := 'feet';     //  labels in feet.
          2:
            grid_str := 'inches';   //  labels in inches.
          3:
            grid_str := 'proto-feet'; //  labels in prototype feet.
          4:
            grid_str := 'cm';       //  labels in cm.
          6:
            grid_str := 'mm';       //  labels in mm.
          else
            run_error(213);
        end;//case

        {if impact>0 then pen_width:=1                   // impact printer or plotter.
                    else}

        //  draw horizontal grid lines (across width)...

        if banner_paper or (print_pages_top_origin <> 0)
        then
          now_gridx := 0 - gridx
        else
          now_gridx := 0;        //  init grid lines. no need for first line (gets overwritten by trim margins).

        repeat
          now_gridx := now_gridx + gridx;
          grid_now_dots := Round((now_gridx - grid_top) * scal_out) + page_top_dots;
          if grid_now_dots < 0 then
            CONTINUE;

          if (now_gridx = 0) and (current_pen_style() = psSolid)
          then
            set_pen_width(pen_width*1.5);    // thicker datum line (only appears if page origin is negative).

          set_linestyle(lsGrid);

          draw_line_style(left_blanking_dots, grid_now_dots,
            printer_width_indexmax_dots, grid_now_dots, lsGrid);

          case grid_labels_code_i of
            1:
              grid_label := now_gridx / 30480;       //  labels in feet.
            2:
              grid_label := now_gridx / 2540;        //  labels in inches.
            3:
              grid_label := now_gridx / (100 * scale); //  labels in prototype feet.
            4:
              grid_label := now_gridx / 1000;        //  labels in cm.
            6:
              grid_label := now_gridx / 100;         //  labels in mm.
            else begin
              grid_label := 0;   // keep the compiler happy.
              run_error(223);
            end;
          end;//case

          grid_label_str := FormatFloat('0.###', grid_label);

          set_fill_colour(clLime);
          write_text(left_blanking_dots,
            grid_now_dots,
            ' ' + grid_label_str,
            tpMiddleLeft, True); //  add labels.

        until grid_now_dots > page_bottom_dots;

        //  draw vertical grid lines (down length)...

        if print_pages_left_origin <> 0 then
          now_gridy := 0 - gridy
        else
          now_gridy := 0;        //  init grid lines. no need for first line (gets overwritten by trim margin).

        repeat
          now_gridy := now_gridy + gridy;
          grid_now_dots := Round((now_gridy - grid_left) * scaw_out) + page_left_dots;
          if grid_now_dots < 0 then
            CONTINUE;

          if (now_gridy = 0) and (current_pen_style = psSolid) then
            set_pen_width(pen_width * 1.5)    // thicker datum line (only appears if page origin is negative).
          else
            set_pen_width(pen_width);

          draw_line(grid_now_dots, top_blanking_dots, grid_now_dots,
            printer_length_indexmax_dots);

          //    if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

          case grid_labels_code_i of
            1:
              grid_label := now_gridy / 30480;       //  labels in feet.
            2:
              grid_label := now_gridy / 2540;        //  labels in inches.
            3:
              grid_label := now_gridy / (100 * scale); //  labels in prototype feet.
            4:
              grid_label := now_gridy / 1000;        //  labels in cm.
            6:
              grid_label := now_gridy / 100;         //  labels in mm.
            else begin
              grid_label := 0;   // keep the compiler happy.
              run_error(224);
            end;
          end;//case

          grid_label_str := FormatFloat('0.###', grid_label);

          write_text(grid_now_dots{-(TextWidth(grid_label_str) div 2)},
            page_top_dots - (mm_to_dots(printmargin_wide)) - halfmm_dots{-TextHeight('A')},
            grid_label_str
            , tpBottomCentre, True); //  add labels.

        until grid_now_dots > page_right_dots;

        // finally add the units string...
        write_text(left_blanking_dots,
          page_top_dots - (mm_to_dots(printmargin_wide) div 2) - halfmm_dots{-TextHeight('A')},
          grid_str);  // add the units string.

      end;

      restore_graphics_state();
      write_comment(' ----==== Grid End ====----');
    end;

  end;
  /////////////////////////////

  procedure draw_marks(grid_left, grid_top: double; rail_joints: boolean);

  // if rail_joints=True draw only the rail joints, otherwise omit them.
  var
    i: integer;
    s: string;  // 208a
    linestyle: Tpdf_LineStyle;

  begin
    markmax := High(marks_list_ptr);  // max index for the present list.

    if mark_index > markmax then
      mark_index := markmax;  // ??? shouldn't be.

    tbnum_str := timb_numbers_str;
    // the full string of timber numbering for the control template.

    with pdf_page do begin

      for i := 0 to (mark_index - 1) do begin
        // (mark_index is always the next free slot)

        ptr_1st := @marks_list_ptr[i];
        // pointer to the next Tmark record.
        if ptr_1st = nil then
          BREAK;

        mark_code := ptr_1st^.code;              // check this mark wanted.

        if not wanted_mark(mark_code) then
        CONTINUE;     // skip this mark.

        // do only the rail joints if rail_joints=True and ignore them otherwise.
        if rail_joints = (mark_code <> 6) then
          CONTINUE;

        linestyle := choose_mark_linestyle(pdf_page, mark_code);

        case mark_code of  // Process based on mark type .....

          1..7, 11..98, 100..199, 600, 700: begin

            p1 := ptr_1st^.p1;      // x1,y1 in  1/100ths mm
            p2 := ptr_1st^.p2;      // x2,y2 in  1/100ths mm

            move_to := page_locate(p1, grid_left, grid_top, [ypd, 0]);
            line_to := page_locate(p2, grid_left, grid_top, [ypd, 0]);

            if check_limits(move_to, line_to) then
              draw_line_style(move_to, line_to, linestyle);

          end;

        99: begin                         // Timber numbering...
            p1 := ptr_1st^.p1;              // x1,y1 in  1/100ths mm

            if
            (pad_form.print_timber_numbering_menu_entry.Checked or
              ((out_factor > 0.99) and pad_form.numbering_fullsize_only_menu_entry.Checked))
             and
              print_settings_form.output_timber_numbers_checkbox.Checked   // 223d

            then begin

              move_to := page_locate(p1, grid_left, grid_top, [ypd, 0]);

              num_str := extract_tbnumber_str(tbnum_str);
              // get next timber numbering string from the acummulated string.
              if num_str = '' then
                CONTINUE;              // no string available??

              if (not pad_form.timber_numbering_on_plain_track_menu_entry.Checked)   // 208a
                and (num_str <> 'A1')
              then begin
                s := LeftStr(num_str, 1);
                if Pos(s, 'AERN') > 0 then CONTINUE;    // not wanted on plain track
              end;

              if check_limit(False, False, move_to)
                then begin
                //Font.Assign(print_timber_numbers_font);

                //if pad_form.scale_timber_numbering_menu_entry.Checked=True
                //   then begin
                //          fontsize:=Font.Size*out_factor;
                //          if fontsize<4 then CONTINUE;      // minimum to be legible.
                //          Font.Size:=Round(fontsize);
                //        end;

                //Brush.Style:=bsSolid;
                //Brush.Color:=clWhite;

                set_fill_colour(clBlack);    // !!! THIS IS WRONG !!!!
                set_font(pdf_font_helv, 8);

                write_text(move_to.X,
                  move_to.Y,
                  num_str,
                  tpBottomCentre, True);


                //Font.Assign(print_labels_font);      // reset for grid labels
                //SetFont(print_labels_font, 10);      // reset for grid labels
              end;
            end;
          end;

        -2, -3: begin                        // curving rad centres...

            p1 := ptr_1st^.p1;        // x1,y1 in  1/100ths mm
            radcen_arm := 400 * scale;
            // 4ft scale arbitrary (scale is for control template).

            move_to := page_locate(p1, grid_left, grid_top, [ypd + radcen_arm, 0]);
            line_to := page_locate(p1, grid_left, grid_top, [ypd - radcen_arm, 0]);

            if check_limits(move_to, line_to) then
              draw_line_style(move_to, line_to, linestyle);

            move_to := page_locate(p1, grid_left, grid_top, [ypd, radcen_arm]);// mark centre lengthwise
            line_to := page_locate(p1, grid_left, grid_top, [ypd, -radcen_arm]);

            if check_limits(move_to, line_to) then
              draw_line_style(move_to, line_to, linestyle);
          end;

        203, 233, 293: begin                 // timber infill
            if i >= (mark_index - 1) then
              BREAK; // Is this really needed???
            ptr_2nd := @marks_list_ptr[i+1];
            // pointer to the second infill Tmark record.
            if ptr_2nd = nil then
              BREAK;

            p1 := ptr_1st^.p1;              // x1,y1 in  1/100ths mm
            p2 := ptr_1st^.p2;              // x2,y2 in  1/100ths mm
            p3 := ptr_2nd^.p1;              // x3,y3 in  1/100ths mm
            p4 := ptr_2nd^.p2;              // x4,y4 in  1/100ths mm

            infill_points[0] := page_locate(p1, grid_left, grid_top, [ypd, 0]);
            infill_points[1] := page_locate(p2, grid_left, grid_top, [ypd, 0]);
            infill_points[2] := page_locate(p3, grid_left, grid_top, [ypd, 0]);
            infill_points[3] := page_locate(p4, grid_left, grid_top, [ypd, 0]);

            if check_limits(infill_points[0], infill_points[1]) and
              check_limits(infill_points[2], infill_points[3]) then begin

              case print_timb_infill_style of
                0:
                  CONTINUE;                         // no infill

                1: begin
                  // hatched infill
                  //if Brush.Color=clBlack then Brush.Color:=virtual_black_colour; // PDF bug fix -- hatching won't work if black
                  //Brush.Style:=bsFDiagonal;                                      // Forward diagonal for the foreground (control template).
                end;

                2: begin
                  // cross-hatched infill
                  //if Brush.Color=clBlack then Brush.Color:=virtual_black_colour;
                  //Brush.Style:=bsDiagCross;
                end;

                3:
                  if pdf_black_white           // solid infill
                  then
                    CONTINUE                  // 209c now no fill
                  //begin
                  //  if Brush.Color=clBlack then Brush.Color:=virtual_black_colour;
                  //  Brush.Style:=bsFDiagonal;  // for printing black and white.
                  //end
                  ;//else Brush.Style:=bsSolid;

                4: begin
                  // blank infill.
                  //Brush.Style:=bsSolid;
                  //Brush.Color:=clWhite;   // overide.
                end;
                else
                  CONTINUE;
              end;//case

              Polygon(infill_points);
            end;
          end;


            601..605, 701..703: begin

              if out_factor <> 1.0 then
                CONTINUE;     // on full size prints only

              p1 := ptr_1st^.p1;              // x1,y1 in  1/100ths mm

              move_to := page_locate(p1, grid_left, grid_top, [ypd, 0]);

              if check_limit(False, False, move_to)
                then begin
                //Font.Assign(print_timber_numbers_font);
                //
                //Font.Style:=[fsBold,fsItalic];
                //Font.Color:=printguide_colour;
                //
                //if scale>3 then Font.Size:=Font.Size+1; // a bit bigger above 3mm/ft

                                                      {
                                                      if pad_form.scale_timber_numbering_menu_entry.Checked=True
                                                         then begin
                                                                fontsize:=Font.Size*out_factor;
                                                                if fontsize<4 then CONTINUE;      // minimum to be legible.
                                                                Font.Size:=Round(fontsize);
                                                              end;
                                                      }

                //Brush.Style:=bsSolid;
                //Brush.Color:=clWhite;

                switch_label_str := switch_label_text(mark_code);
              //set_fill_color(printguide_colour);

                write_text(
                  move_to.X,//-(TextWidth(switch_label_str) div 2),  // div 2 allows for rotation of template
                  move_to.Y
                  ,//-(TextHeight(switch_label_str) div 2),
                  ' ' + switch_label_str + ' ',
                  tpMiddleCentre, True);

                //Font.Assign(print_labels_font);      // reset for grid labels
              end;

            end;
        end;//case
      end;//next mark i
    end;//with pdf_page
  end;
  ///////////////////////////////

  function get_w_dots(q, n: integer): integer;

  begin
    with sheet[sheet_down, sheet_across] do begin // (grid_left)
      Result := Round((outoflist(q, n).y + ypd - grid_left) * scaw_out) + page_left_dots;
    end;//with
    w_dims_valid := check_draw_dim_w(Result);
  end;
  ////////////////////////////

  function get_l_dots(q, n: integer): integer;

  begin
    with sheet[sheet_down, sheet_across] do begin // (grid_top)
      Result := Round((outoflist(q, n).x - grid_top) * scal_out) + page_top_dots;
    end;//with
    l_dims_valid := check_draw_dim_l(Result);
  end;
  ////////////////////////////

  procedure draw_outline_railedge(aq, pencol: integer);

  var
    now: integer;

  begin
    if ((not plain_track) or (aq = 0) or (aq = 8) or (aq = 3) or
      (aq = 11) or ((aq > 15) and (aq < 24))) and aqyn[aq] then begin
      with pdf_page do begin

        set_pen_colour(pencol);
        set_pen_style(psSolid);

        move_to.X := get_w_dots(aq, 0);
        move_to.Y := get_l_dots(aq, 0);
        for now := 1 to nlmax_array[aq] do begin
          line_to.X := get_w_dots(aq, now);
          line_to.Y := get_l_dots(aq, now);
          if check_limits(move_to, line_to) then
            draw_line(move_to, line_to);
          move_to := line_to;
        end;//for
      end;//with page
    end;
  end;
  ////////////////////////////

  procedure draw_fill_rail(outer_add: integer);    // draw a complete filled rail.

  const
    dots_max_c = xy_pts_c * 2;

  var
    dots: array[0..dots_max_c] of TPoint;
    // array of points for filled polygon mode.

    //    (xy_pts_c=3000;)
    //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
    //   template max is 4500' scale length.
    //   ( = 18000 mm or 59ft approx in 4 mm scale).
    //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

    // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
    // because needed for the Polygon function.

    // total memory = 6000*8 bytes = 48kb.

    now, start, now_max: integer;
    edge_started: boolean;
    mid_dots_index: integer;
    edge_colour, blanking_colour: integer;


    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    procedure modify_rail_end(
      start_index, stop_index, edge, blank: integer);

    var
      saved_pen_width: TPDF_PenWidth;    // 206b

    begin

      if (start_index >= 0) and (start_index <= dots_index) and
        (stop_index >= 0) and (stop_index <= dots_index) then begin
        move_to := dots[start_index];
        line_to := dots[stop_index];

        if check_limits(move_to, line_to) then begin
          with pdf_page do begin

            saved_pen_width := current_pen_width; // 206b
            set_pen_colour(blank);
            // first blank across..
            draw_line(move_to, line_to);

            set_pen_width(saved_pen_width);
            // 206b restore original width
            set_pen_colour(edge);
            // then restore the corner points..
            draw_line(move_to, move_to);

            draw_line(line_to, line_to);

          end;//with pdf_page
        end;
      end;
    end;
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  begin
    with pdf_page do begin

      if rail in [16, 20]             // 0.93.a platforms
      then
        set_pen_colour(printplat_edge_colour)
      else
        set_pen_colour(printcurail_colour);
      //  1 = virtual black. Bug in HP driver if black (0) specified.

      set_pen_style(psSolid);

      aq := rail;  // gauge-faces.

      if ((not plain_track) or (aq = 0) or (aq = 3) or ((aq > 15) and (aq < 24)))

      // if plain track, stock rails and adjacent tracks only.

      then begin
        if not (aqyn[aq] and aqyn[aq + outer_add])
        // data not for both edges?
        then begin
          if aqyn[aq] then
            draw_outline_railedge(aq, current_fill_colour);
          if aqyn[aq + outer_add] then
            draw_outline_railedge(aq + outer_add, current_fill_colour);
          EXIT;
        end;

        now_max := nlmax_array[aq];

        if not gaunt    // 0.93.a normal turnout.
        then begin

          case aq of
            1: begin
              start := list_planing_mark_aq1;
              // start from end of planing - no infill in planing.

              if (start < 0) or (start > now_max) then
                EXIT;  // ???

              //out 18-8-01 0.73.a   if ((start+1)<=now_max) and (now_max>1) then drawn_full_aq1:=False;  // ok to overdraw planing.

            end;

            2: begin                         // ditto
              start := list_planing_mark_aq2;

              if (start < 0) or (start > now_max) then
                EXIT;  // ???

              //if ((start+1)<=now_max) and (now_max>1) then drawn_full_aq2:=False;

            end;

            else
              start := 0;                   // whole list.
          end;//case

        end
        else
          start := 0;  // 0.93.a gaunt template, no planing. 0.81 09-Jul-2005


        dots_index := 0 - 1;                    // first increment is to zero.

        edge_started := False;
        for now := start to now_max do begin
          x_dots := get_w_dots(aq, now);
          y_dots := get_l_dots(aq, now);
          if w_dims_valid and l_dims_valid
          then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now

        mid_dots_index := dots_index;

        aq := rail + outer_add;             // outer-edges.

        now_max := nlmax_array[aq];

        edge_started := False;

        for now := now_max downto 0 do begin
          x_dots := get_w_dots(aq, now);
          y_dots := get_l_dots(aq, now);
          if w_dims_valid and l_dims_valid
          then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now

        if pdf_black_white then begin
          //Brush.Style:=bsSolid;               // solid infill white.
          //Brush.Color:=clWhite;
        end
        else begin
          if rail in [16, 20]   // 0.93.a platforms
          then begin
            set_pen_colour(printplat_infill_colour);

            //case print_platform_infill_style of
            //        0: Brush.Style:=bsClear;
            //        1: Brush.Style:=bsBDiagonal;    // hatched. backward diagonal (forward diagonal on control template timbers).
            //        2: Brush.Style:=bsDiagCross;
            //
            //        3: if mapping_colours_print<0          // solid.
            //              then Brush.Style:=bsBDiagonal    // impact printer or plotter, or printing black and white or in a single colour.
            //              else Brush.Style:=bsSolid;
            //
            //      else begin                         // 4 = blank.
            //             Brush.Style:=bsSolid;
            //             Brush.Color:=clWhite;       // overide.
            //           end;
            //
            //end;//case
          end
          else begin
            if (draw_ts_trackbed_cess_edge and (rail = 18)) or
              (draw_ms_trackbed_cess_edge and (rail = 22))   // 215a
            then begin
              set_pen_colour(sb_track_bgnd_colour);
              // cess use same colour as track background
              //Brush.Style:=bsFDiagonal;
            end
            else begin   // normal rails...

              //Brush.Color:=printrail_infill_colour_cu;
              set_fill_colour(
                printrail_infill_colour_cu);

              //case rail_infill_i of
              //      1: Brush.Style:=bsBDiagonal;   // hatched
              //      2: Brush.Style:=bsSolid;       // solid
              //      3: Brush.Style:=bsDiagCross;   // cross_hatched
              //      4: begin                       // blank
              //           Brush.Style:=bsSolid;
              //           Brush.Color:=clWhite;
              //         end;
              //    else Brush.Style:=bsSolid;       // ??? solid
              //end;//case

            end;
          end;
        end;

        if dots_index > 2 then begin
          Polygon(Slice(dots, dots_index + 1));
          // +1, number of points, not index.  must have 4 points.

          edge_colour := current_fill_colour;  // existing rail edges.

          //if Brush.Style=bsSolid
          //   then blanking_colour:=Brush.Color       // infill colour.
          //   else

                                               {begin   // 206b hatched fill...
                                                      // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

                                                case output_code of
                                                  1,2: blanking_colour:=dtp_settings_form.sb_page_colour_panel.Color;
                                                    3: blanking_colour:=export_form.img_bgnd_colour_panel.Color;
                                                    4: blanking_colour:=Brush.Color; // for metafile export
                                                 else}

          blanking_colour := clWhite;
          // assume white background (print, PDF)

          //end;//case
          //end;

          // remove polygon line across end of planing (not for fixed-diamond)..
          // (for gaunt template this removes the polygon line across the rail end)

          if (not (half_diamond and fixed_diamond)) and
            (rail in [1, 2]) then
            modify_rail_end(0, dots_index, edge_colour, blanking_colour);

          // remove polygon lines across stock rail ends...
          // and trackbed ends  206b

          if rail in [0, 3, 18, 22]   // 18,22 added 206b
          then begin
            modify_rail_end(
              0, dots_index, edge_colour, blanking_colour);  // toe or approach end.

            modify_rail_end(
              mid_dots_index, mid_dots_index + 1, edge_colour, blanking_colour);  // exit end.
          end;

          if adjacent_edges  // platforms
          then begin

            // 0.93.a blank platform rear edges ...

            if (rail = 16) and draw_ts_platform and
              (not draw_ts_platform_rear_edge)   // 0.93.a TS platform start
            then
              draw_outline_railedge(16, blanking_colour);    // blank rear edge

            if (rail = 20) and draw_ms_platform and
              (not draw_ms_platform_rear_edge)   // 0.93.a TS platform start
            then
              draw_outline_railedge(20, blanking_colour);    // blank rear edge

            // 0.93.a blank platform ends ...

            if (rail = 16) and draw_ts_platform and
              (not draw_ts_platform_start_edge)   // 0.93.a TS platform start
            then
              modify_rail_end(0, dots_index, edge_colour, blanking_colour);

            if (rail = 16) and draw_ts_platform and
              (not draw_ts_platform_end_edge)     // 0.93.a TS platform end
            then
              modify_rail_end(mid_dots_index, mid_dots_index + 1, edge_colour, blanking_colour);

            if (rail = 20) and draw_ms_platform and
              (not draw_ms_platform_start_edge)   // 0.93.a MS platform start
            then
              modify_rail_end(0, dots_index, edge_colour, blanking_colour);

            if (rail = 20) and draw_ms_platform and
              (not draw_ms_platform_end_edge)     // 0.93.a MS platform end
            then
              modify_rail_end(mid_dots_index, mid_dots_index + 1, edge_colour, blanking_colour);

          end;


          if rail in [26, 28]
          then begin
            modify_rail_end(
              0, dots_index, edge_colour, blanking_colour);  // centre of K-crossing check rails.
          end;

        end;
      end;
    end;//with
  end;
  //////////////////////////////////

  procedure draw_fill_vee;    // do complete vee in one go ...

  const
    dots_max_c = xy_pts_c * 2;

  var
    dots: array[0..dots_max_c] of TPoint;
    // array of points for filled polygon mode.

    //    (xy_pts_c=3000;)
    //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
    //   template max is 4500' scale length.
    //   ( = 18000 mm or 59ft approx in 4 mm scale).
    //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

    // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
    // because needed for the Polygon function.

    // total memory = 6000*8 bytes = 48kb.

    now: integer;
    edge_started: boolean;
    dots_index: integer;
    x_dots, y_dots: integer;
    aq: integer;
    point_mid_dots_index, splice_mid_dots_index: integer;
    edge_colour, blanking_colour: integer;

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    procedure modify_vee_end(start_index, stop_index, edge, blank: integer);

    begin

      if (start_index >= 0) and (start_index <= dots_index) and
        (stop_index >= 0) and (stop_index <= dots_index) then begin
        move_to := dots[start_index];
        line_to := dots[stop_index];

        if check_limits(move_to, line_to)
          then begin
          with pdf_page do begin

            set_pen_colour(blank);
            // first blank across..
            draw_line(move_to, line_to);

            set_pen_colour(edge);
            // then restore the corner points..
            draw_line(move_to, move_to);
            draw_line(line_to, line_to);

          end;//with
        end;
      end;
    end;
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  begin
    if plain_track then
      EXIT;   // not if plain track.

    if (not (aqyn[4] and aqyn[5] and aqyn[12]and aqyn[13])) // not enough data for filled vee.
      or (nlmax_array[4] = 0) or (nlmax_array[5] = 0) or (nlmax_array[12] = 0) or
      (nlmax_array[13] = 0) then begin
      if aqyn[4] then
        draw_outline_railedge(4, printcurail_colour);       // draw outline vee...
      if aqyn[5] then
        draw_outline_railedge(5, printcurail_colour);
      if aqyn[12] then
        draw_outline_railedge(12, printcurail_colour);
      if aqyn[13] then
        draw_outline_railedge(13, printcurail_colour);
    end
    else begin
      dots_index := 0 - 1;   // first increment is to zero.

      aq := 4;
      edge_started := False;
      for now := 0 to nlmax_array[aq] do
        // vee main-side, gauge_face, start from the tip.
      begin
        x_dots := get_w_dots(aq, now);
        y_dots := get_l_dots(aq, now);
        if w_dims_valid and l_dims_valid
        then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      point_mid_dots_index := dots_index;

      aq := 12;
      edge_started := False;
      for now := nlmax_array[aq] downto 0 do  // back along outer-edge.
      begin
        x_dots := get_w_dots(aq, now);
        y_dots := get_l_dots(aq, now);
        if w_dims_valid and l_dims_valid
        then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      aq := 13;
      edge_started := False;
      for now := 0 to nlmax_array[aq] do    // and then turnout side outer edge.
      begin
        x_dots := get_w_dots(aq, now);
        y_dots := get_l_dots(aq, now);
        if w_dims_valid and l_dims_valid
        then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      splice_mid_dots_index := dots_index;

      aq := 5;
      edge_started := False;
      for now := nlmax_array[aq] downto 0 do
        // and back along the gauge face to the tip.
      begin
        x_dots := get_w_dots(aq, now);
        y_dots := get_l_dots(aq, now);
        if w_dims_valid and l_dims_valid
        then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      with pdf_page do begin

        set_pen_colour(printcurail_colour);
        //  1 = virtual black. Bug in HP driver if black (0) specified.
        //  (but not on "econofast" print !).
        set_pen_style(psSolid);
        set_fill_colour(printrail_infill_colour_cu);

        if {(}pdf_black_white  {) or (impact>0)}
        then begin
          set_fill_colour(clWhite);
        end;
        //else case rail_infill_i of
        //           1: Brush.Style:=bsBDiagonal;   // hatched
        //           2: Brush.Style:=bsSolid;       // solid
        //           3: Brush.Style:=bsDiagCross;   // cross_hatched
        //           4: begin                       // blank
        //                Brush.Style:=bsSolid;
        //                Brush.Color:=clWhite;
        //              end;
        //         else Brush.Style:=bsSolid;       // solid
        //     end;//case

        if dots_index > 4 then begin
          Polygon(Slice(dots, dots_index + 1));
          // +1, number of points, not index.  must have at least 5 points.

          edge_colour := current_pen_colour;  // existing rail edges.

          //if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
          //                       else blanking_colour:=clWhite;

          // remove polygon lines across vee rail ends...

          modify_vee_end(point_mid_dots_index,
            point_mid_dots_index + 1, edge_colour, blanking_colour); // point rail end.

          modify_vee_end(splice_mid_dots_index,
            splice_mid_dots_index + 1, edge_colour, blanking_colour); // splice rail end.
        end;

      end;//with pdf_page
    end;
  end;
  ////////////////////////////////////////////////////////////////////////

  procedure mark_end(aq1, aq1end, aq2, aq2end: integer);      // make a rail end mark

  begin
    if endmarks_yn[aq1, aq1end] and endmarks_yn[aq2, aq2end]
    then begin
      p1 := endmarks[aq1, aq1end];
      p2 := endmarks[aq2, aq2end];


      with sheet[sheet_down, sheet_across] do begin
        move_to := page_locate(p1, grid_left, grid_top, [ypd, 0]);
        line_to := page_locate(p2, grid_left, grid_top, [ypd, 0]);

      end;//with

      with pdf_page do begin
        set_pen_colour(printcurail_colour);
        //  1 = virtual black. Bug in HP driver if black (0) specified.
        //  (but not on "econofast" print !).
        set_pen_style(psSolid);

        if check_limits(move_to, line_to) then
          draw_line(move_to, line_to);
      end;//with
    end;
  end;
  ////////////////////////////////////////////////////////////

  procedure outline_railends;       // draw the rail ends in outline mode.

  begin
    if not plain_track then
    begin                                       // mark rail-ends...

      mark_end(1, 1, 9, 1);    // turnout rail wing rail finish.
      mark_end(2, 1, 10, 1);   // main rail wing rail finish.

      mark_end(6, 0, 14, 0);   // main side check rail start.
      mark_end(6, 1, 14, 1);   // main side check rail finish.

      mark_end(7, 0, 15, 0);   // turnout side check rail start.
      mark_end(7, 1, 15, 1);   // turnout side check rail finish.

      mark_end(4, 0, 5, 0);    // blunt nose.

      if half_diamond and fixed_diamond
      // planed faced of point rails for a fixed-diamond.
      then begin
        mark_end(1, 0, 9, 0);
        mark_end(2, 0, 10, 0);

        mark_end(26, 1, 27, 1);     // MS K-crossing check rails.
        mark_end(28, 1, 29, 1);     // DS K-crossing check rails.
      end;
    end;
  end;
  ///////////////////////////////////////////////////////////////////

  procedure pdf_shapes_and_sketchboard_items(grid_left, grid_top: double);    // 206e

  begin

    // T3-OUT       if bgnd_form.output_shapes_in_front_of_sb_checkbox.Checked=True
    // T3-OUT          then pdf_sketchboard_items(pdf_form.pdf_printer.Canvas,grid_left,grid_top);  // 206e

    pdf_bgnd_shapes(grid_left, grid_top);    // output all background shapes

    // T3-OUT       if bgnd_form.output_shapes_in_front_of_sb_checkbox.Checked=False
    // T3-OUT          then pdf_sketchboard_items(pdf_form.pdf_printer.Canvas,grid_left,grid_top);  // 206e

  end;
  //////////////////////////////////////////////////////////////////

begin
  with pdf_form.pdf_save_dialog do begin
    if his_pdf_file_name <> '' then
      InitialDir := ExtractFilePath(his_pdf_file_name)
    else
      InitialDir := Config.GetDir(cudiPdfs);

    FileName := remove_invalid_str(Copy(Trim(box_project_title_str), 1, 20) +
      '_pages_' + FormatDateTime('yyyy_mm_dd_hhmm_ss', Date + Time)) + '.pdf';
    Title := '    save  PDF  file  as ...';

    if not Execute then
      EXIT;

    his_pdf_file_name := FileName;        // so we can use the same folder next time.

    // invalid entered chars removed by dialog

    file_str := ExtractFilePath(FileName) + lower_case_filename(ExtractFileName(FileName));
    // to underscores and lower case

    if FileExists(file_str)    // 217b
    then begin
      if not DeleteFile(file_str) then begin
        ShowMessage('Error:' + #13 + #13 + 'The PDF file "' +
          ExtractFileName(file_str) + '" cannot be created ' +
          #13 + 'because a file having the same name is currently in use '
          + #13 + 'by another program, possibly your PDF reader program. '
          + #13 + #13 + 'Please close that program and then try again. '
          + #13 + #13 + 'Or use a different file name (recommended). ');
        EXIT;
      end;
    end;

    pdf_filename_str := file_str;

  end;//with

  print_colours_setup;    // first set up the colours.

  preview_record_file_made := False;   // 214b  init

  pdf_form.all_panel.Caption := 'create  all  pages       ';       // init 223d
  pdf_form.omit_all_panel.Caption := 'cancel            F12';      // init 223d

  try
    print_busy := True;               // lock-out the loop while printing.

{ T3-OUT
  if pdf_form.pdf_printer.Printing=True
     then begin
            if alert(6,'    PDF  output  busy',
                    'The PDF output is currently in use.'
                   +'||Please try again later.',
                    '','','','','cancel  PDF  output','wait  until  ready',0)=5    // 208a mod
               then pdf_form.pdf_printer.Printing:=False
               else EXIT;
          end;
}

    printer_printing := False;             // init for new job.
    pdf_form.in_progress_label.Hide;

    if not calcs_done_and_valid then
      redraw(False);    //  first do a direct redraw if nec to ensure valid calcs.
    if not calcs_done_and_valid then
      EXIT;             //  calcs still not valid.

    if not print_preview(False, True, 0)
    // calc the sheet sizes from the printer and preview the output.
    then
      EXIT;                         // (also gets ypd y datum offset).

    print_line_thickness_setup;  // needs the dpi via print_preview.  mod 0.73.a 12-9-01.

    gridx := grid_spacex * 100;
    //gridsizex*2540;   // grid line spacings. in 1/100th mm.  (any output scaling is done later).
    gridy := grid_spacey * 100;  //gridsizey*2540;

    while gridx * out_factor < 500 do
      gridx := gridx * 2;      // 5 mm closest grid spacing permitted down the page.
    while gridy * out_factor < 1000 do
      gridy := gridy * 2;     // 10 mm ditto across page to allow for labels.

    all_pages := False;        // init for one page at a time.

    slow_run := 0;                                            // cancel any slow-running.
    control_room_form.run_slow_menu_entry.Checked := False;

    info_str := '';                                        // init for info show...
    pdf_form.info_scrollbox.VertScrollBar.Position := 0;

    banner_top_done := False;

    page_count := 0;

    // check every sheet for any content, all 858 sheets (26*33)...

    max_sheet_across := 0;  // init

    for sheet_across := 0 to sheet_across_c do begin
      info_str := info_str + 'in row ' + Chr(sheet_across + 97) + ' :   ';

      row_count := 0;

      for sheet_down := 0 to sheet_down_c do begin              // every sheet in row.

        if sheet[sheet_down, sheet_across].empty then
          CONTINUE;
        Inc(page_count);

        max_sheet_across := sheet_across;  // keep track of highest non-empty page row.

        page_num_str := Chr(sheet_across + 97) + '/' + IntToStr(sheet_down + 1);
        if row_count = 10 then begin
          info_str := info_str + '|                  ';
          // add another line for every 10 page nunbers in a row.
          row_count := 0;
        end;

        info_str := info_str + page_num_str + '  ';
        Inc(row_count);

      end;//next down
      info_str := info_str + '||';             // new line for each row of pages.
    end;//next across

    if page_count < 1 then
      page_count := 1;    // how did that happen?

    if page_count > 1 then
      pgco_str := '' + IntToStr(page_count) + '  PDF  pages'
    else
      pgco_str := 'one  PDF  page';

    pgco_str := pgco_str + ' :';

    with pdf_form do begin

      if print_entire_pad_flag then begin
        if print_group_only_flag then
          header_label.Caption := 'export group templates only     scaled at : ' + round_str(
            out_factor * 100, 2) + ' %'
        else
          header_label.Caption := 'export background templates     scaled at : ' + round_str(
            out_factor * 100, 2) + ' %';
      end
      else
        header_label.Caption := Trim(gauge_str) + '  ' + round_str(scale, 2) + ' mm/ft     scaled at : ' +
          round_str(out_factor * 100, 2) + ' %';

      page_label.Caption := pgco_str;
      printer_info_label.Caption := insert_crlf_str(info_str);

      origin_label.Caption := 'page origin :  X = ' + round_str(print_pages_top_origin, 2) +
        ' mm    Y = ' + round_str(print_pages_left_origin, 2) + ' mm';

    end;//with
    //---------------------------------------------------

    pdf_form.row_progressbar.Max := (max_sheet_across + 1) * (sheet_down_c + 1);
    pdf_form.row_progressbar.Position := 0;

    for sheet_across := 0 to max_sheet_across do begin
      for sheet_down := 0 to sheet_down_c do begin              // every sheet in row (all 33).

        pdf_form.row_progressbar.StepIt;

        with sheet[sheet_down, sheet_across] do begin

          if empty then
            CONTINUE;

          case xing_type_i of
            0:
              xing_str := ' regular V-crossing';
            1:
              xing_str := ' curviform V-crossing';
            -1:
              xing_str := ' generic V-crossing';
            else
              xing_str := '';
          end;//case

          page_num_str := Chr(sheet_across + 97) + '/' + IntToStr(sheet_down + 1);

          page_str := ' page  ' + page_num_str + '  ';

          if print_entire_pad_flag then
            bottom_str := ' of  ' + IntToStr(page_count) + '  pages for  ' + box_project_title_str
          else begin
            bottom_str := ' ref :  ' + current_name_str + '    ' + info_form.gauge_label.Caption;

            if not plain_track then
              bottom_str := bottom_str + xing_str;

            if (ABS(nomrad) < max_rad_test) or spiral then begin
              if not spiral then
                bottom_str := bottom_str + '    curved onto ' + round_str(nomrad, 0) + ' mm radius'
              else
                bottom_str := bottom_str + '    spiral transition curve';
            end
            else
              bottom_str := bottom_str + '    straight (no curving)';
          end;

          if out_factor <> 1 then
            bottom_str := bottom_str + '    scaled at ' + round_str(out_factor * 100, 2) + ' %';
          // page number and scaling.

          bottom_str := bottom_str + '    ' + DateToStr(Date) + '  ' + TimeToStr(Time);

          top_str := ' TEMPLOT  v:' + round_str(program_version / 100, 2) + version_build +
            '  templot.com  This drawing contains design elements and data � Martin Wynne.';
          if box_project_title_str <> '' then
            top_str := top_str + '   Project : ' + box_project_title_str;

          temp_str := Chr(sheet_across + 97) + '/' + IntToStr(sheet_down + 1);
          pdf_form.page_panel.Caption := 'next  page  is   ' + temp_str;
          pdf_form.ok_button.Caption := '&create  page  ' + temp_str;
          pdf_form.omit_page_button.Caption := '&omit  page  ' + temp_str;

          if not all_pages then begin
            //                  show_modal_message('--- NOT ALL_PAGES ---');
            button_clicked := False;
            banner_changed := False;
            enable_buttons(sheet_across < max_sheet_across);
            pdf_form.Show;//Modal;
            pdf_form.BringToFront;

            if not Application.Terminated then
              Application.ProcessMessages; // otherwise first red outline doesn't show properly.
            outline_in_red(sheet_down, sheet_across); // show which sheet we mean.

            repeat
              if not Application.Terminated then
                Application.ProcessMessages
              else
                BREAK;
            until button_clicked;

            disable_buttons;

            if not Application.Terminated then
              Application.ProcessMessages;

            case pdf_form.ModalResult of

              mrIgnore: begin
                pdf_form.all_panel.Caption := 'create  all  remaining  pages     ';
                // 223d
                CONTINUE;     // omit page - do next down
              end;

              mrRetry: begin
                pdf_form.all_panel.Caption := 'create  all  remaining  pages     ';
                // 223d
                BREAK;        // omit row - do next across
              end;

              mrOk: begin
                pdf_form.page_panel.Caption := 'preparing  page   ' + temp_str + '  ...';

                pdf_form.omit_all_panel.Caption :=
                  'omit  all  remaining  pages    F12     ';  // 223d

                if not preview_record_file_made   // 214b   first page only
                then begin
                  make_pdf_preview_screenshot;
                  preview_record_file_made := True;
                end;
                if printer_printing then begin
{ T3-OUT
                    pdf_form.pdf_printer.EndPage;
                    pdf_form.pdf_printer.StartPage(pdf_width_dots,pdf_height_dots,pdf_width_dpi,pdf_height_dpi,0);    // 0.91.d
}
                  end_page(False);
                  begin_page;
                end
                else begin
                  begin_doc;
                  // or the first.
                end;

              end;

              mrCancel: begin
                if printer_printing then
                  end_doc(True);
                EXIT;
              end;

              mrYes: begin
                all_pages := True;
                pdf_form.in_progress_label.Show;
                pdf_form.page_panel.Caption := 'preparing  pages';

                if not preview_record_file_made   // 214b   first page only
                then begin
                  make_pdf_preview_screenshot;
                  preview_record_file_made := True;
                end;
                if printer_printing then begin
{ T3-OUT
                    pdf_form.pdf_printer.EndPage;
                    pdf_form.pdf_printer.StartPage(pdf_width_dots,pdf_height_dots,pdf_width_dpi,pdf_height_dpi,0);    // 0.91.d
}
                  end_page(False);
                  begin_page;
                end
                else begin
                  begin_doc;
                end;

              end;
              else
                run_error(31);
            end;//case
          end
          else begin                       // all pages
            //             show_modal_message('--- ALL PAGES ---');
            if not Application.Terminated then
              Application.ProcessMessages;

            if pdf_form.ModalResult = mrCancel   // he's aborted all pages on F12 or Esc
            then begin
              if printer_printing then
                end_doc(False);
              EXIT;
            end;

            if not preview_record_file_made   // 214b   first page only
            then begin
              make_pdf_preview_screenshot;
              preview_record_file_made := True;
            end;

            if printer_printing then begin
                              { T3-OUT
                                                  pdf_form.pdf_printer.EndPage;
                                                  pdf_form.pdf_printer.StartPage(pdf_width_dots,pdf_height_dots,pdf_width_dpi,pdf_height_dpi,0);    // 0.91.d
                              }
              end_page(False);
              begin_page;
            end
            else begin
              begin_doc;          // or the first.
            end;

          end;

          with pdf_page do begin
            write_comment(' ----==== Start Page ' + page_num_str + ' ====----');

            //        Print watermark Page number if many pages
            if pdf_form.page_ident_checkbox.Checked and (page_count > 3)
            then begin
              Set_Font(pdf_font_helv, round(min(pdf_width_mm, pdf_height_mm) * 1.25));

              //if pdf_black_white = True then begin
              //  set_pen_color(clBlack);
              //  set_pen_width(2);
              //end
              //else begin
              //  set_pen_color($00D0D0D0);   // pale-ish grey
              //  set_pen_width(100);
              //end;
              //set_fill_color(clWhite);

              set_linestyle(lsWatermark);

              ident_left := round(pdf_width_dots / 2);
              ident_top := round(pdf_height_dots / 2);

              set_textmode(tmStroke);
              Write_Text(ident_left, ident_top, page_num_str, tpMiddleCentre);
              set_textmode(tmFill);
            end;

            // --- Background shapes and grid ---
            if bgnd_form.output_grid_in_front_checkbox.Checked then
              begin
                pdf_shapes_and_sketchboard_items(grid_left, grid_top);
                draw_grid(grid_left, grid_top, pdf_page);
              end
            else
              begin
                draw_grid(grid_left, grid_top, pdf_page);
                pdf_shapes_and_sketchboard_items(grid_left, grid_top);
              end;

            if print_entire_pad_flag // control template
            then
              pdf_bgnd(grid_left, grid_top, pdf_page);       // now print any background templates.


            //  control template - draw timbers and all marks except rail joints...
            //###

            if (not print_entire_pad_flag) // control template
              and (not output_diagram_mode)   // 0.93.a  no control template if diagram mode
              and (turnoutx > 0)                  // not if invalidated

            // 0.93.a if printing background templates in Quick mode, the control template has been put on the background

            then begin

              if marks_list_ptr = nil then
                BREAK;       // pointer to marks list not valid, exit all sheets.

              draw_marks(grid_left, grid_top, False);
              // print all the background timbering and marks except rail joints.

              if {pad_form.print_track_centre_lines_menu_entry.Checked=True}  // 0.82.b
              (print_settings_form.output_centrelines_checkbox.Checked and
                (not dummy_template))       // 212a
                or (print_settings_form.output_bgnd_shapes_checkbox.Checked and
                dummy_template) then begin

                if dummy_template   // 212a
                then begin
                  set_pen_style(psSolid);
                  set_pen_colour(printshape_colour);
                  set_pen_width(max(printshape_wide, min_penwidth));
                end
                else begin
                  set_pen_colour(printcurail_colour);
                  set_pen_width(max(printcl_wide, min_penwidth));
                end;

                for aq := 24 to 25 do begin
                  if ((not plain_track) or (aq = 24)) and aqyn[aq]
                  // main side only only if plain track, and data available ?
                  then begin
                    move_to.X := get_w_dots(aq, 0);
                    move_to.Y := get_l_dots(aq, 0);
                    for now := 1 to nlmax_array[aq] do begin
                      line_to.X := get_w_dots(aq, now);
                      line_to.Y := get_l_dots(aq, now);
                      if check_limits(move_to, line_to) then
                        draw_line(move_to, line_to);
                      move_to := line_to;
                    end;//for
                  end;
                end;//for-next aq
              end;//if track centre-lines.

              if {pad_form.print_rails_menu_entry.Checked=True}  // 0.82.b
              print_settings_form.output_rails_checkbox.Checked
                then begin
                //  draw turnout rails...
                set_pen_width(max(printrail_wide, min_penwidth));

                if (rail_infill_i = 0)
                // out for pdf, was  or ((scale*out_factor)<0.75)   // less than 18.75% for 4mm scale (control template) (10.71% for 7mm).
                then begin           //  outline (pen) mode ...
                  //  n.b. this mode does not automatically close the rail-ends.

                  for aq := 0 to 23 do begin
                    // 24, 25 centre-lines already done.
                    if (not adjacent_edges) and (aq > 15) then
                      CONTINUE;  // no adjacent tracks in output  // 206b

                    case aq of     // 223d
                      16, 17, 20, 21:
                        if not print_settings_form.output_platforms_checkbox.Checked then
                          CONTINUE;         // platforms not wanted
                      18, 19, 22, 23:
                        if not print_settings_form.output_trackbed_edges_checkbox.Checked then
                          CONTINUE;    // trackbed edges not wanted
                    end;//case

                    draw_outline_railedge(aq, printcurail_colour);
                  end;//next aq

                  for aq := 26 to aq_max_c do
                    draw_outline_railedge(aq, printcurail_colour);  // K-crossing check rails.

                  outline_railends;
                  // finally do the rail ends for outline mode
                end
                else begin      // infill (polygon) mode ...

                  // do blades first - neater result.
                  for rail := 1 to 3 do
                    draw_fill_rail(8);  // closure rails and curved stock rail.

                  rail := 0;
                  // straight stock rail.
                  draw_fill_rail(8);

                  for rail := 6 to 7 do
                    draw_fill_rail(8);  // check rails

                  if adjacent_edges    // 206b
                  then begin
                    rail := 16;
                    repeat
                      case rail of     // 223d
                        16, 20:
                          if print_settings_form.output_platforms_checkbox.Checked then
                            draw_fill_rail(1);        // platforms
                        18, 22:
                          if print_settings_form.output_trackbed_edges_checkbox.Checked then
                            draw_fill_rail(1);   // trackbed edges
                      end;//case
                      rail := rail + 2;
                    until rail > 22;
                  end;

                  rail := 26;
                  repeat
                    draw_fill_rail(1);      // K-crossing MS check rails.
                    rail := rail + 2;
                  until rail > 28;

                  draw_fill_vee;   // now do the vee.

                  // finally draw in or overdraw the planing gauge-faces - (no infill) ...
                  aq := 1;
                  if (not plain_track) and
                    (not gaunt) and aqyn[1] and (list_planing_mark_aq1 > 0)
                  {and (drawn_full_aq1=False)}// not if already drawn.
                  then begin
                    move_to.X := get_w_dots(aq, 0);
                    move_to.Y := get_l_dots(aq, 0);
                    for now := 1 to list_planing_mark_aq1{+1} do
                    begin                    // +1 to overdraw
                      line_to.X := get_w_dots(aq, now);
                      line_to.Y := get_l_dots(aq, now);
                      if check_limits(move_to, line_to) then
                        draw_line(move_to, line_to);
                      move_to := line_to;
                    end;//for
                  end;

                  aq := 2;
                  if (not plain_track) and
                    (not gaunt) and aqyn[2] and (list_planing_mark_aq2 > 0)
                  {and (drawn_full_aq2=False)}// not if already drawn.
                  then begin
                    move_to.X := get_w_dots(aq, 0);
                    move_to.Y := get_l_dots(aq, 0);
                    for now := 1 to list_planing_mark_aq2{+1} do
                    begin                      // +1 to overdraw
                      line_to.X := get_w_dots(aq, now);
                      line_to.Y := get_l_dots(aq, now);
                      if check_limits(move_to, line_to) then
                        draw_line(move_to, line_to);
                      move_to := line_to;
                    end;//for
                  end;

                  //  CAN'T GET FLOODFILL TO WORK ON THE PRINTER 26-8-98.
                  // and flood fill the planing with the margin colour ...

                  //Brush.Bitmap:=nil;     // so can use style again if it was dots.

                end;//polygon mode

                // finally add rail joint marks across rails (will now mark over rail infill)...
                //###

                draw_marks(grid_left, grid_top, True);

              end;//if rails

            end;// if control template

            // now the trim margins....

            set_pen_colour(printmargin_colour);
            set_pen_style(psSolid);
            set_pen_width(printmargin_wide);

            move_to.X := left_blanking_dots;
            move_to.Y := page_top_dots;   // paper top left.
            line_to.X := printer_width_indexmax_dots;
            line_to.Y := page_top_dots;   // paper top margin.
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);   // top left. top margin.

            // and right-hand alignment targets...
            //if sheet_across<>sheet_co_wide
            //   then begin

            move_to.X := page_right_dots;
            move_to.Y := top_blanking_dots;
            line_to.X := page_right_dots;
            line_to.Y := printer_length_indexmax_dots;                    // paper right margin.
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);   // paper top margin.

            move_to.X := page_right_dots - alignmarks_inner_dots;
            line_to.X := printer_width_indexmax_dots;

            move_to.Y := page_quarter_dots;  // right 1/4 target.
            line_to.Y := page_quarter_dots;
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);

            move_to.Y := page_mid_dots;      // right centre target.
            line_to.Y := page_mid_dots;
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);

            move_to.Y := page_3quarter_dots; // right 3/4 target.
            line_to.Y := page_3quarter_dots;
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);

            //        end;

            //  don't show bottom trim line on the last sheet down, otherwise he might trim off the info line,
            //  and no bottom trim lines on any sheet for banners, unless for multiple print runs.

            if {(banner_paper=False) and (} sheet_down < sheet_co_long {)} then
            begin
              if not sheet[sheet_down + 1, sheet_across].empty
              // something on next page down ?
              then begin
                move_to.X := left_blanking_dots;
                move_to.Y := page_bottom_dots;
                line_to.X := printer_width_indexmax_dots;
                line_to.Y := page_bottom_dots;    // paper bottom margin.
                if check_limits(move_to, line_to) then
                  draw_line(move_to, line_to);
              end;
            end;

            if want_bottom_margin and (sheet_down = sheet_co_long)
            // wanted for multiple print runs...
            then begin
              move_to.X := left_blanking_dots;
              move_to.Y := page_bottom_dots;
              line_to.X := printer_width_indexmax_dots;
              line_to.Y := page_bottom_dots;    // paper bottom margin.
              if check_limits(move_to, line_to) then
                draw_line(move_to, line_to);
            end;

            move_to.X := page_left_dots;
            move_to.Y := top_blanking_dots;
            line_to.X := page_left_dots;
            line_to.Y := printer_length_indexmax_dots;               // paper left margin.
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);

            move_to.X := page_left_dots - alignmarks_inner_dots;   // default 3 mm each way.
            line_to.X := page_left_dots + alignmarks_inner_dots;
            if move_to.X < left_blanking_dots then
              move_to.X := left_blanking_dots;

            move_to.Y := page_quarter_dots;    // left 1/4 target.
            line_to.Y := page_quarter_dots;
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);

            move_to.Y := page_mid_dots;        // left centre target.
            line_to.Y := page_mid_dots;
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);

            move_to.Y := page_3quarter_dots;   // left 3/4 target.
            line_to.Y := page_3quarter_dots;
            if check_limits(move_to, line_to) then
              draw_line(move_to, line_to);

            // trim margins finished.

            //----------------------

            //                 //  now any margin blanking (outside the trim margins)...
            //                 //  (+1 to ensure complete blanking on all printers.)
            //
            //              Brush.Color:=clWhite;
            //              Brush.Style:=bsSolid;
            //
            //              Pen.Color:=clWhite;
            //              Pen.Style:=psSolid;
            //              Pen.Width:=1;
            //              Pen.Mode:=pmCopy;
            //
            //              if left_blanking_dots>0 then Rectangle(0,0,left_blanking_dots,printer_length_indexmax_dots+1);
            //              if right_blanking_dots<=printer_width_indexmax_dots then Rectangle(right_blanking_dots,0,printer_width_indexmax_dots+1,printer_length_indexmax_dots+1);
            //
            //              if {(} top_blanking_dots>0 {) and (banner_paper=False)} then Rectangle(0,0,printer_width_indexmax_dots+1,top_blanking_dots);
            //              if {(} bottom_blanking_dots<=printer_length_indexmax_dots {) and (banner_paper=False)} then Rectangle(0,bottom_blanking_dots,printer_width_indexmax_dots+1,printer_length_indexmax_dots+1);
            //
            // top branding...

            //              Brush.Color:=clWhite;
            //              Brush.Style:=bsSolid;

            set_fill_colour(clBlack);
            set_font(pdf_font_helv, 7);

            write_text(left_blanking_dots, top_blanking_dots, top_str, tpTopLeft);
            // name and "who for?" string at topleft.

            // small text inside the margins...

            set_font(pdf_font_helv, 6);
            set_pen_colour(calc_intensity(clBlack));

            // mods 208g  20-04-2014  show page origin dims on templates...

                    {Tsheet=record
                             grid_top:double;          // 100th mm - grid means trim margin lines inside the sheet edges.
                             grid_bottom:double;
                             grid_left:double;
                             grid_right:double;}

            if print_entire_pad_flag
            // 214a  for Gordon, see message ref: 19595   // background templates
            then begin
              if keep_form.box_file_label.Caption <> '' then
                last_file_str := '  printing from: ' + ExtractFileName(keep_form.box_file_label.Caption)
              else
                last_file_str := '  printing background templates';
            end
            else
              last_file_str := '  printing the control template';

            if pad_form.show_margin_coordinates_menu_entry.Checked then
            begin
              all_pages_origin_str :=
                'all pages origin (a/1): top(X)=' + round_str(print_pages_top_origin, 2) +
                'mm, left(Y)=' + round_str(print_pages_left_origin, 2) + 'mm      ';     // 208g
              this_page_begin_str :=
                'this page begins: top(X)=' + round_str(grid_top / 100, 2) + 'mm, left(Y)=' + round_str(
                grid_left / 100, 2) + 'mm' + last_file_str;                        // 208g
              this_page_end_str :=
                'this page ends: bottom(X)=' + round_str(grid_bottom / 100, 2) + 'mm, right(Y)=' +
                round_str(grid_right / 100, 2) + 'mm';                                  // 208g
            end
            else begin
              all_pages_origin_str := '';
              this_page_begin_str := last_file_str;
              this_page_end_str := '';
            end;

            //if pad_form.show_corner_info_menu_entry.Checked=True    // 223d
            //   then begin

            //Font.Assign(print_corner_page_numbers_font);                           // 0.93.a
            //if print_corner_page_numbers_font.Size>8 then Brush.Style:=bsClear;

            write_text(page_left_dots + mm_to_dots(printmargin_wide) + 3,
              page_top_dots + mm_to_dots(printmargin_wide) + 2,
              this_page_begin_str,
              tpTopLeft);           // top left corner
            write_text(page_left_dots + mm_to_dots(printmargin_wide) + 3,
              page_bottom_dots{+Font.Height} - mm_to_dots(printmargin_wide) - 4,
              page_num_str + '   ' + box_project_title_str +
              '   ' + DateToStr(Date) + ' ' + TimeToStr(Time));  // bottom left corner
            write_text(
              page_right_dots - mm_to_dots(printmargin_wide){-TextWidth(all_pages_origin_str+page_num_str)} - 200,
              page_top_dots + mm_to_dots(printmargin_wide) + 2,
              all_pages_origin_str + page_num_str,
              tpTopRight);         // top right corner
            write_text(
              page_right_dots - mm_to_dots(printmargin_wide){-TextWidth(this_page_end_str)} - 200,
              page_bottom_dots{+Font.Height} - mm_to_dots(printmargin_wide) - 4,
              this_page_end_str,
              tpBottomRight);      // bottom right corner
            //        end;


            if (distortions <> 0) and pdf_form.warnings_checkbox.Checked
            then
            begin
              //                        Font.Assign(set_font('Arial',7,[],printmargin_colour));
              write_text(page_left_dots + mm_to_dots(printmargin_wide), page_top_dots +
                mm_to_dots(printmargin_wide){-(Font.Height*5)},
                '  Warning :  Data distortions are in force.  This template may not be dimensionally accurate.');
            end;

            //              Font.Assign(print_labels_font);  // reset for labels.

            //              Font.Color:=calc_intensity(clBlack);
            //              Brush.Color:=clWhite;
            //              Brush.Style:=bsClear;   // transparent over detail.

            write_text(left_blanking_dots,
              page_bottom_dots + (mm_to_dots(printmargin_wide) div 2) + halfmm_dots,
              page_str + bottom_str,
              tpTopLeft, True); // add the bottom string last.

            //              Font.Assign(print_labels_font);  // reset for labels.
            //              Brush.Style:=bsSolid;

          end;//with Canvas 0.91.d pdf

        end;//with sheet

      end;//for-next sheet across
    end;//for-next sheet down

    if printer_printing then
      end_doc(True);      // last or only page.
  finally
    print_busy := False;
    enable_buttons(True);        // ?? not really needed.
    pdf_form.Close;//Hide;
  end;//try
end;
//_____________________________________________________________________________________

procedure Tpdf_form.FormShow(Sender: TObject);

begin

  detail_mode_radiobutton.Checked := not output_diagram_mode;
  diagram_mode_radiobutton.Checked := output_diagram_mode;

  Left := pad_form.Left + pad_form.Width - Width - 20;

  if info_form.Showing                // don't want the info cluttering the print preview.
  then begin
    info_was_showing := True;
    pad_form.hide_info_menu_entry.Click;
  end
  else
    info_was_showing := False;

  if panning_form.Showing             // nor the panning controls.
  then begin
    panning_was_showing := True;
    panning_form.Hide;
  end
  else
    panning_was_showing := False;

  if shove_timber_form.Showing        // nor the shove timbers form.
  then begin
    shove_was_showing := True;
    shove_timber_form.Hide;
  end
  else
    shove_was_showing := False;

  if grid_form.Showing                // nor the spacing ring form.
  then begin
    spacing_was_showing := True;
    grid_form.Hide;
  end
  else
    spacing_was_showing := False;


  // bugs fixed -- 208d ...

  if rail_options_form.Showing            // nor this
  then begin
    rail_options_was_showing := True;
    rail_options_form.Hide;
  end
  else
    rail_options_was_showing := False;

  if platform_form.Showing                // nor this
  then begin
    platform_was_showing := True;
    platform_form.Hide;
  end
  else
    platform_was_showing := False;

  if trackbed_form.Showing                // nor this
  then begin
    trackbed_was_showing := True;
    trackbed_form.Hide;
  end
  else
    trackbed_was_showing := False;

  if check_diffs_form.Showing             // nor this
  then begin
    check_diffs_was_showing := True;
    check_diffs_form.Hide;
  end
  else
    check_diffs_was_showing := False;

  if data_child_form.Showing              // nor this
  then begin
    data_child_was_showing := True;
    data_child_form.Hide;
  end
  else
    data_child_was_showing := False;

  if stay_visible_form.Showing            // nor this
  then begin
    stay_visible_was_showing := True;
    stay_visible_form.Hide;
  end
  else
    stay_visible_was_showing := False;

  //----------


  info_scrollbox.VertScrollBar.Position := 0;
  info_scrollbox.HorzScrollBar.Position := 0;
  if all_button.Enabled then
    all_button.SetFocus;

  if print_entire_pad_flag then begin
    if print_group_only_flag then
      Caption := ' create  PDF  file :  group  templates  only'
    else
      Caption := ' create  PDF  file :  background  templates';
    //info_checkbox.Enabled:=False;
  end
  else begin
    Caption := ' create  PDF  file :  the  control  template';
    //info_checkbox.Enabled:=True;
  end;

  if fit_single_sheet then
    Caption := Caption + '  on  a  single  page';

  pad_form.top_toolbar_panel.Hide;
  pad_form.second_toolbar_panel.Hide;    // 217a

  make_slip_form.Hide;   // toolbars marker

  show_output_mode_panel;

end;
//_________________________________________________________________________________________

procedure Tpdf_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_F10 then begin
    Key := 0;      //  otherwise selects the menus.
  end;

  if Key = VK_PAUSE then
    Application.Minimize;        //  hide TEMPLOT on PAUSE key.

  if Key = VK_F12                  // same as cancel, but unlocks the redraw
  then begin
    Key := 0;
    pdf_form.omit_all_button.Click;
    pad_form.redraw_menu_entry.Click;       // unlock the redraw if locked.
    //Close;
  end;
end;
//________________________________________________________________________________________

procedure Tpdf_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  print  information  window', Color);
  button_clicked := False;     // overide the print form Deactivate.
end;
//_________________________________________________________________________________________

procedure Tpdf_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  form_scaling := True;    // no ScrollInView on resize.

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

  form_scaling := False;
end;
//__________________________________________________________________________________________

procedure Tpdf_form.FormCreate(Sender: TObject);

begin
  ClientWidth := 560;
  ClientHeight := 300;
  AutoScroll := True;
end;
//______________________________________________________________________________

procedure Tpdf_form.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  button_clicked := True;
  ModalResult := mrCancel;

  if spacing_was_showing then
    pad_form.spacing_ring_menu_entry.Click;
  if shove_was_showing then
    pad_form.shove_timbers_menu_entry.Click;
  if panning_was_showing then
    pad_form.show_pan_controls_menu_entry.Click;
  if info_was_showing then
    pad_form.show_info_menu_entry.Click;

  // bug fixes 208d ...

  if rail_options_was_showing then
    pad_form.omit_rails_joints_menu_entry.Click;
  if platform_was_showing then
    pad_form.platform_edges_menu_entry.Click;
  if trackbed_was_showing then
    pad_form.trackbed_ballast_edges_menu_entry.Click;
  if check_diffs_was_showing then
    pad_form.differ_check_rails_menu_entry.Click;
  if data_child_was_showing then
    data_child_form.Show;
  if stay_visible_was_showing then
    stay_visible_form.Show;

  pad_form.top_toolbar_panel.Show;
  pad_form.second_toolbar_panel.Show;    // 217a

  if Screen.Width > (make_slip_form.Width + pad_form.top_toolbar_panel.Width) then
    make_slip_form.Show;   // toolbars marker

  pad_form.output_mode_panel.Visible := False;

end;
//______________________________________________________________________________

procedure Tpdf_form.omit_all_buttonClick(Sender: TObject);

begin
  button_clicked := True;
  ModalResult := mrCancel;  // clicking the panel instead of the button doesn't set this.
end;
//______________________________________________________________________________

procedure Tpdf_form.next_row_buttonClick(Sender: TObject);

begin
  button_clicked := True;       // ModalResult = mrRetry
end;
//_________________________________________________________________________________________

procedure Tpdf_form.omit_page_buttonClick(Sender: TObject);

begin
  button_clicked := True;       // ModalResult = mrIgnore
end;
//________________________________________________________________________________________

procedure Tpdf_form.ok_buttonClick(Sender: TObject);

begin
  button_clicked := True;       // ModalResult = mrOK
end;
//_________________________________________________________________________________________

procedure Tpdf_form.all_buttonClick(Sender: TObject);

begin
  button_clicked := True;
  ModalResult := mrYes;     // clicking the panel instead of the button doesn't set this.
end;
//_________________________________________________________________________________________

procedure Tpdf_form.FormDeactivate(Sender: TObject);

begin
  if print_busy          // don't let him click off the form.
  then begin
    Beep;
    Show;
    BringToFront;
    EXIT;
  end;

  button_clicked := True;
  ModalResult := mrCancel;
end;
//________________________________________________________________________________________
(* T3-OUT

procedure pdf_sketchboard_items(on_canvas:TCanvas; grid_left,grid_top:double);   // 206e

var
  dtp_rect:TRect;
  dtp_width,dtp_height:double;
  p1,p2:Tpex;

  move_to,line_to:TPoint;   // 208a
  raster_rect:TRect;        // 208a
  low_res_bitmap:TBitmap;   // 208a
  this_graphic:TGraphic;    // 208a

  saved_cursor:TCursor;

begin
  if print_settings_form.output_sketchboard_items_checkbox.Checked=False then EXIT;

  if pdf_form.include_sketchboard_items_checkbox.Checked=False then EXIT;

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

  dtp_form.dtp_document.CurrentPage.PageColor:=dtp_settings_form.sb_page_colour_panel.Color;  // sb normal colour into PDF.

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
            copying_sb_to_printer:=False;  // 206e flag for dtpPage.Print
            copying_sb_to_pdf:=False;      // 206e flag for dtpPage.Print  !!! for full-res direct to PDF output only

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

                                       // StretchDraw requires TGraphic parameter instead of TBitmap to work reliably...

                                this_graphic:=low_res_bitmap;

                                on_canvas.StretchDraw(raster_rect,this_graphic);

                              end;

                    end;

            low_res_bitmap.Free;
          end
     else begin  // full res output...

            copying_sb_to_pad:=False;      //      flag for dtpPage.Print
            copying_sb_to_printer:=False;  // 206e flag for dtpPage.Print
            copying_sb_to_pdf:=True;       // 206e flag for dtpPage.Print

            dtp_rect.Left:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
            dtp_rect.Top:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

            dtp_rect.Right:=Round((p2.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
            dtp_rect.Bottom:=Round((p2.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

            on_canvas.Pen.Width:=1;
            on_canvas.Pen.Style:=psSolid;
            on_canvas.Pen.Color:=clBlack;

            on_canvas.Brush.Style:=bsSolid;
            on_canvas.Brush.Color:=clWhite;

            dtp_form.dtp_document.CurrentPage.Print(on_canvas,dtp_rect,0,3,False,False);  // 3 = rotate clockwise

          end;

        // restore...

  // render_page_background_transparent:=False;  not used (fails)

  omit_trackplan_from_rendering:=False;

  copying_sb_to_pdf:=False;  // 206e flag for dtpPage.Print

  on_canvas.Pen.Width:=1;
  on_canvas.Pen.style:=psSolid;
  on_canvas.Pen.Color:=clBlack;

  on_canvas.Brush.Style:=bsSolid;
  on_canvas.Brush.Color:=clWhite;

  Screen.Cursor:=saved_cursor;

end;
//______________________________________________________________________________
*)

procedure pdf_bgnd_shapes(grid_left, grid_top: double);  // print all background shapes.

var
  i, maxbg_index: integer;
  font_size: integer;

  arm, diamond: double;

  now_shape: Tbgnd_shape;
  move_to, line_to: TPoint;
  raster_rect: TRect;

  dummy_bitmap: TBitmap;
  dummy_rect: TRect;

  // T3-OUT  rotated_emf:TMetafile;

begin
  if not print_settings_form.output_bgnd_shapes_checkbox.Checked then
    EXIT;

  maxbg_index := bgnd_form.bgnd_shapes_listbox.Items.Count - 1;

  if maxbg_index < 0 then
    EXIT;

  // T3-OUT  with pdf_form.pdf_printer.Canvas do begin
  with pad_form.Canvas do begin  // T3 rubbish to allow test compilation

    Font.Assign(shapes_label_font);
    Font.Color := printshape_colour;

    font_size := Round(Font.Size * out_factor);
    if font_size < 2 then
      font_size := 2;
    Font.Size := font_size;

    Pen.Mode := pmCopy;

    for i := 0 to maxbg_index do begin
      Pen.Style := psSolid;
      Pen.Color := printshape_colour;   // it changes for a label or monochrome picture.
      Pen.Width := mm_to_dots(printshape_wide);     // it changes for a picture border.

      now_shape := Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape;
      // next shape.

      with now_shape do begin

        if (hide_bits and $02) <> 0 then
          CONTINUE;   // shape hidden for output

        if shape_code <> 4    // not a target mark
        then begin

          case shape_style of
            0: begin
              Brush.Color := clWhite;
              Brush.Style := bsClear;      // transparent. (also lines).
            end;
            1: begin
              if pdf_black_white
              // 0.93.a   -- use timber infill colour
              then
                Brush.Color := clWhite
              else begin
                                               {if impact>0 then Brush.Color:=printtimber_colour            // colour plotter.
                                                           else} Brush.Color :=
                  printtimber_infill_colour;
              end;
              // out 0.93.a Brush.Color:=clWhite;

              Brush.Style := bsSolid;      // blank out.
            end;

            2: begin
              Brush.Color := Pen.Color;
              Brush.Style := bsDiagCross;  // cross-hatched.

              TextOut(0, 0, '');
              // This seems to be necessary before dotted lines will draw properly.

              //if shape_code=0 then Pen.Style:=psDot; // out wPDF bug // dashed line.
              Pen.Style := psSolid;

            end;

            else begin
              Brush.Color := clWhite;
              Brush.Style := bsClear;      // transparent.
            end;
          end;//case

          move_to.X := Round((p1.y * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
          move_to.Y := Round((p1.x * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          if shape_code = 3      // label rectangle..
          then begin
            Font.Color := printshape_colour;
            line_to.X := move_to.X + TextWidth(shape_name + '    ');
            // 0.93.a add 4 spaces  // was +5
            line_to.Y := move_to.Y + ABS(Font.Height * 4 div 3);      // 0.93.a // was +5
            Brush.Color := clWhite;
            Brush.Style := bsSolid;             // blank rectangle box for label.
            Pen.Color := Font.Color;
            Pen.Width := ABS(Font.Height div 24);   // 0.93.a added arbitrary
            if Pen.Width < 1 then
              Pen.Width := 1;
          end
          else begin
            line_to.X :=
              Round((p2.y * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
            line_to.Y := Round((p2.x * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;
          end;

          if (move_to.X < 0) and (line_to.X < 0) then
            CONTINUE;                                              // not on this page.
          if (move_to.X > printer_width_indexmax_dots) and
            (line_to.X > printer_width_indexmax_dots) then
            CONTINUE;    // not on this page.

          if (move_to.Y < 0) and (line_to.Y < 0) then
            CONTINUE;                                              // not on this page.
          if (move_to.Y > printer_length_indexmax_dots) and
            (line_to.Y > printer_length_indexmax_dots) then
            CONTINUE;  // not on this page.

          if check_limits(move_to, line_to) then begin
            case shape_code of
              -1: begin     // picture = bitmap image.  !!! needs 90 deg rotate. 9-2-01.

                if pdf_form.include_pictures_checkbox.Checked
                then begin
                  raster_rect.Left := move_to.X;
                  raster_rect.Top := move_to.Y;

                  raster_rect.Right := line_to.X;
                  raster_rect.Bottom := line_to.Y;

                  if Tbgshape(
                    bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile
                    then begin
                    // metafile...     214a

                    // wPDF pdf_printer Canvas bug - stretchdrawing rotated metafiles gives angle error
                    // convert it to a bitmap and copy to pdf Canvas instead.
                    // this means metafiles no longer transparent on PDF...

                    dummy_bitmap := TBitmap.Create;

                    dummy_bitmap.Width :=
                      raster_rect.Right - raster_rect.Left;
                    dummy_bitmap.Height :=
                      raster_rect.Bottom - raster_rect.Top;

                    dummy_rect :=
                      Rect(0, 0, dummy_bitmap.Width, dummy_bitmap.Height);

                    // blank to start...

                    with dummy_bitmap.Canvas do begin
                      // T3-OUT                                                       Brush.Color:=pdf_form.pdf_printer.Canvas.Brush.Color;
                      Brush.Style := bsSolid;
                      FillRect(dummy_rect);
                      TextOut(0, 0, '');
                      // !!! Delphi bug?
                    end;//with

                    try
                      if Tbgshape(
                        bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_picture.Width = 0
                      // empty Graphic
                      then begin
                        // T3-OUT                                                                 pdf_rotate_metafile(i);
                        Application.ProcessMessages;   // this seems to be necessary for StretchDraw to work first time.
                      end;

                      dummy_bitmap.Canvas.StretchDraw(
                        dummy_rect, Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_picture.Graphic);   // draw rotated metafile on it

                      CopyMode := cmSrcCopy;
                      CopyRect(
                        raster_rect, dummy_bitmap.Canvas, dummy_rect); // and copy it to PDF.

                    except
                      Pen.Color := printshape_colour;
                      Brush.Color := Pen.Color;
                      // metafile failed - draw hatched outline.
                      Brush.Style := bsBDiagonal;
                      Rectangle(
                        move_to.X, move_to.Y, line_to.X, line_to.Y);
                    end;//try

                    dummy_bitmap.Free;

                  end     //end metafile

                  else begin   // bitmap...
                    try
                      if Tbgshape(
                        bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_bitmap.Empty
                      then begin
                        pdf_rotate_bitmap(i);
                        Application.ProcessMessages;   // this seems to be necessary for StretchDraw to work first time.
                      end;

                      if Tbgshape(
                        bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.show_transparent
                         // 0.93.a moved into file
                      then
                        CopyMode := cmSrcAnd        // (destination Canvas) transparent if on white background.
                      else
                        CopyMode := cmSrcCopy;  // reset normal for destination Canvas.

                      if Tbgshape(
                        bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_bitmap.Monochrome
                        then begin
                        Brush.Style := bsSolid;
                        //!!! these are all needed to get StretchDraw to work with monochrome bitmaps
                        Brush.Color := clWhite;
                        Pen.Color := clBlack;
                        Font.Color := clBlack;
                        // !!!! including this.
                        TextOut(0, 0, '');
                        // !!! Delphi bug?
                      end;

                      // StretchDraw requires TGraphic parameter instead of TBitmap to work reliably...

                      StretchDraw(
                        raster_rect, Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.rotated_picture.Graphic);

                      CopyMode := cmSrcCopy;
                      // reset normal for destination Canvas.

                    except
                      CopyMode := cmSrcCopy;
                      // reset normal for destination Canvas.
                      Pen.Color := printshape_colour;
                      Brush.Color := Pen.Color;
                      // stretch failed - draw hatched outline.
                      Brush.Style := bsBDiagonal;
                      Rectangle(
                        move_to.X, move_to.Y, line_to.X, line_to.Y);
                    end;//try

                  end;// bitmap
                end;//include pictures

                if pdf_form.picture_borders_checkbox.Checked or
                  (not pdf_form.include_pictures_checkbox.Checked)
                // or (pdf_form.picture_outlines_radio.Checked=True)
                then begin
                  if pdf_form.include_pictures_checkbox.Checked
                    {pdf_form.picture_outlines_radio.Checked=False}
                  then
                    Pen.Width := mm_to_dots(printpicborder_wide);    // picture borders thinner unless an outline only.
                  Pen.Color := printshape_colour;
                  Brush.Color := clWhite;
                  Brush.Style := bsClear;
                  Rectangle(move_to.X,
                    move_to.Y, line_to.X, line_to.Y);
                end;

                if not pdf_form.include_pictures_checkbox.Checked
                          // pdf_form.picture_outlines_radio.Checked=True
                then begin
                  MoveTo(move_to.X, move_to.Y);
                  LineTo(line_to.X, line_to.Y);  // printing picture outlines only - draw diagonal line.
                  MoveTo(move_to.X, line_to.Y);
                  LineTo(line_to.X, move_to.Y);  // and other diagonal line.
                end;
              end;//-1

              0: begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;
              1, 3:
                Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
              2:
                Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);
            end;//case

            if shape_code = 3 then begin
              Brush.Color := clWhite;
              Brush.Style := bsClear;
              TextOut(move_to.X, move_to.Y, '   ' + shape_name);
              // 0.93.a 2 spaces  // was +1  // insert label text in rectangle box.
            end;
          end;

        end
        else begin    // shape_code=4, draw a target mark

          arm := p2.x;        // cross arm length.
          diamond := arm / 2;   // size of centre diamond.

          move_to.X := Round((p1.y * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
          // lengthwise arms...
          move_to.Y := Round(((p1.x - arm) * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          line_to.X := move_to.X;
          line_to.Y := Round(((p1.x + arm) * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          if check_limits(move_to, line_to) then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);   // draw lengthwise arms.
          end;

          move_to.X := Round(((p1.y - arm) * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
          // widthwise arms...
          move_to.Y := Round((p1.x * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          line_to.X := Round(((p1.y + arm) * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
          line_to.Y := move_to.Y;

          if check_limits(move_to, line_to) then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);   // draw widthwise arms.
          end;

          // now do 4 diamond lines...

          // NW line...

          move_to.X := Round((p1.y * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
          move_to.Y := Round(((p1.x - diamond) * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          line_to.X := Round(((p1.y + diamond) * 100 + re_org_y - grid_left) * scaw_out) +
            page_left_dots;
          line_to.Y := Round((p1.x * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          if check_limits(move_to, line_to) then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          move_to := line_to;      // NE line...
          line_to.X := Round((p1.y * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
          line_to.Y := Round(((p1.x + diamond) * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          if check_limits(move_to, line_to) then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          move_to := line_to;      // SE line...
          line_to.X := Round(((p1.y - diamond) * 100 + re_org_y - grid_left) * scaw_out) +
            page_left_dots;
          line_to.Y := Round((p1.x * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          if check_limits(move_to, line_to) then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          move_to := line_to;      // SW line...
          line_to.X := Round((p1.y * 100 + re_org_y - grid_left) * scaw_out) + page_left_dots;
          line_to.Y := Round(((p1.x - diamond) * 100 + re_org_x - grid_top) * scal_out) + page_top_dots;

          if check_limits(move_to, line_to) then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;
        end;

      end;//with now_shape
    end;//for next i
  end;//with pdf_form.pdf_printer.Canvas
end;
//_______________________________________________________________________________________

procedure pdf_bgnd_marks(grid_left, grid_top: double; maxbg_index: integer;
  rail_joints: boolean; pdf_page: TPDF_page);  // print all the background timbering and marks.

// if rail_joints=True print only the rail joints, otherwise omit them.
var
  //single_colour_flag:boolean;
  i, n: integer;

  move_to, line_to: TPoint;
  p1, p2, p3, p4: TPoint;

  now_keep: Tbgnd_keep;

  array_max: integer;
  code: integer;

  radcen_arm: double;

  infill_points: array [0..3] of TPoint;

  fontsize: double;
  num_str: string;
  tbnum_str: string;

  mapping_colour: integer;
  using_mapping_colour: boolean;

  switch_label_str: string;  // 206b

  s, idnum_str, idtb_str: string;  // 208a
  num: integer;                  // 208a

  line_style: TPDF_LineStyle;

begin
  with pdf_page do begin
    write_comment('--- Starting Marks ---');

    //single_colour_flag:=pad_form.use_single_colour_menu_entry.Checked;

    set_pen_style(psSolid);

    for n := 0 to maxbg_index do begin

      if not Ttemplate(keeps_list.Objects[n]).bg_copied then
        CONTINUE;  // no data, not on background.

      if (not Ttemplate(keeps_list.Objects[n]).group_selected) and
        print_group_only_flag then
        CONTINUE;  // not in group. 0.78.b 10-12-02.

      if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.fb_kludge_template_code > 0 then
        CONTINUE;  // 209c no marks for fb_kludge templates

      now_keep := Ttemplate(keeps_list.Objects[n]).bgnd_keep;    // next background keep.

      with now_keep do begin

        // mapping_colours_print: 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.

        using_mapping_colour := False;  // default init.
        mapping_colour := clBlack;      // init - keep compiler happy.

        with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin

          idnum_str := id_number_str;  // 208a

          if use_print_mapping_colour and
            ((mapping_colours_print = 2) or (mapping_colours_print = 3)) and
            (not pdf_black_white) and (not pdf_grey_shade) then begin
            mapping_colour := calc_intensity(print_mapping_colour);
            using_mapping_colour := True;
          end;

          if use_pad_marker_colour and
            (mapping_colours_print = 4)   // use pad settings instead
            and (not pdf_black_white) and (not pdf_grey_shade)
          then
          begin
            mapping_colour := calc_intensity(pad_marker_colour);
            using_mapping_colour := True;
          end;
        end;//with

        tbnum_str := timber_numbers_string;      // the full string of timber numbering.

        // first draw bgnd marks and timbers ...

        array_max := High(list_bgnd_marks);

        for i := 0 to array_max do begin

          code := list_bgnd_marks[i].code;

          if not wanted_mark(code) then
            CONTINUE;     // skip this mark.

          // do only the rail joints if rail_joints=True and ignore them otherwise.
          if rail_joints = (code <> 6) then
            CONTINUE;

          if ((code = 5) or (code = 55) or (code = 95) or (code = 600) or (code = 700)) and
            (out_factor <> 1.0) then
            CONTINUE;   // reduced ends are meaningless if not full-size.   206b 600 added. 211b 700 added

          line_style := choose_mark_linestyle(pdf_page, code, False);

          case code  of
          1..98, 100.. 199, 600, 700: begin
          write_comment('Mark ' + inttostr(code) + ' using ' + line_style.str());
            p1 := list_bgnd_marks[i].p1;    // x1,y1 in  1/100ths mm
            p2 := list_bgnd_marks[i].p2;    // x2,y2 in  1/100ths mm

            if pdf_black_white then
              set_pen_colour(clBlack)
            else begin
              if using_mapping_colour then
                set_pen_colour(mapping_colour)
              else
                if mapping_colours_print < 0
                // 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.
                then
                  set_pen_colour(printbg_single_colour)
                // single colour for all of background templates.
            end;

            //Pen.Mode:=pmCopy;

            move_to := page_locate(p1, grid_left, grid_top);
            line_to := page_locate(p2, grid_left, grid_top);

            if check_limits(move_to, line_to) then
              draw_line_style(move_to, line_to, line_style);
          end;

        -2, -3: begin
              //if pdf_black_white=True
              //   then Pen.Color:=clBlack  // overide.
              //   else begin
              //          //if single_colour_flag=False then Pen.Color:=clBlack
              //          if mapping_colours_print<>-1
              //             then Pen.Color:=calc_intensity(clBlack)
              //             else Pen.Color:=printbg_single_colour;
              //        end;

              if pdf_black_white then
                set_pen_colour(clBlack)  // overide.
              else
                //if single_colour_flag=False then set_pen_color:=clBlack;
                //if mapping_colours_print <> -1 then
                //  set_pen_color(calc_intensity(clBlack))
                //else
                if mapping_colours_print = -1 then
                  set_pen_colour(printbg_single_colour);

              p1 := list_bgnd_marks[i].p1;    // x1,y1 in  1/100ths mm

              radcen_arm := 400 * scale;
              // 4ft scale arbitrary (scale is for control template).

              // mark centre widthwise.
              move_to := page_locate(p1, grid_left, grid_top, radcen_arm);
              line_to := page_locate(p2, grid_left, grid_top, -radcen_arm);

              if check_limits(move_to, line_to) then
                draw_line_style(move_to, line_to, line_style);

              // mark centre lengthwise
              move_to := page_locate(p1, grid_left, grid_top, [0, radcen_arm]);
              line_to := page_locate(p2, grid_left, grid_top, [0, -radcen_arm]);
              if check_limits(move_to, line_to) then
                draw_line_style(move_to, line_to, line_style);
            end;

            //if (code = 203) or (code = 233) or (code = 293)       // timber infill...
            //then begin
          203, 233, 293: begin              // timber infill...

              if i < array_max
              then begin
                p1 := list_bgnd_marks[i].p1;    // x1,y1 in  1/100ths mm
                p2 := list_bgnd_marks[i].p2;    // x2,y2 in  1/100ths mm

                p3 := list_bgnd_marks[i + 1].p1;    // x3,y3 in  1/100ths mm
                p4 := list_bgnd_marks[i + 1].p2;    // x4,y4 in  1/100ths mm
              end
              else begin         // keep compiler happy...
                p1.X := 0;
                p1.Y := 0;

                p2.X := 0;
                p2.Y := 0;

                p3.X := 0;
                p3.Y := 0;

                p4.X := 0;
                p4.Y := 0;
              end;

              infill_points[0] := page_locate(p1, grid_left, grid_top);
              infill_points[1] := page_locate(p2, grid_left, grid_top);
              infill_points[2] := page_locate(p3, grid_left, grid_top);
              infill_points[3] := page_locate(p4, grid_left, grid_top);

              if check_limits(infill_points[0], infill_points[1]) and
                 check_limits(infill_points[2], infill_points[3]) then begin

                if pdf_black_white then
                  set_pen_colour(clBlack)
                else begin
                  //if single_colour_flag=False
                  if mapping_colours_print <> -1 then
                    set_fill_colour(printtimber_infill_colour)
                  else
                    set_fill_colour(printbg_single_colour);
                end;

                // 0.95.a  PDF bug-fix...

                //case print_timb_infill_style of
                //
                //                0: CONTINUE;                         // no infill
                //
                //                1: begin                             // hatched infill
                //                     if Brush.Color=clBlack then Brush.Color:=virtual_black_colour; // PDF bug fix -- hatching won't work if black
                //                     Brush.Style:=bsBDiagonal;                                      // backward diagonal for the background templates
                //                   end;
                //
                //                2: begin                             // cross-hatched infill
                //                     if Brush.Color=clBlack then Brush.Color:=virtual_black_colour;
                //                     Brush.Style:=bsDiagCross;
                //                   end;
                //
                //                3: if (pdf_black_white=True) or (mapping_colours_print<0)   // solid infill
                //                      then CONTINUE  // 209c now no fill
                //                           //begin
                //                           //  if Brush.Color=clBlack then Brush.Color:=virtual_black_colour;
                //                           //  Brush.Style:=bsBDiagonal;  // for printing black and white or in a single colour.
                //                           //end
                //                      else Brush.Style:=bsSolid;
                //
                //                4: begin                             // blank infill.
                //                     Brush.Style:=bsSolid;
                //                     Brush.Color:=clWhite;   // overide.
                //                   end;
                //              else CONTINUE;
                //end;//case

                Polygon(infill_points);
              end;
            end;

          99: begin                           // Timber Labels
            if (pad_form.print_timber_numbering_menu_entry.Checked
              or  ((out_factor > 0.99)
                and pad_form.numbering_fullsize_only_menu_entry.Checked))
              and print_settings_form.output_timber_numbers_checkbox.Checked

            then begin
              p1 := list_bgnd_marks[i].p1;    // x1,y1 in  1/100ths mm
              p2 := list_bgnd_marks[i].p2;

              if print_settings_form.output_timb_id_prefix_checkbox.Checked
              then
                move_to := page_locate(p2, grid_left, grid_top)
              else
                // 223d  ID prefix not wanted, use p1 *screen* positions (as for control template)
                move_to := page_locate(p1, grid_left, grid_top);

              num_str := extract_tbnumber_str(tbnum_str);
              // get next timber numbering string from the acummulated string.
              if num_str = '' then
                CONTINUE;              // no string available??

              if not pad_form.timber_numbering_on_plain_track_menu_entry.Checked
                        // 208a
              then begin
                s := LeftStr(num_str, 1);
                if pos(s, 'AERN') > 0    // not wanted on plain track,
                then begin
                  if RightStr(num_str, 1) = '1'   // every 10 sleepers
                  then
                    num_str := ''                      // show template ID only
                  else
                    CONTINUE;
                end;
              end;

              if check_limit(False, False, move_to)
              then begin
                //Font.Assign(print_timber_numbers_font);


                if pad_form.scale_timber_numbering_menu_entry.Checked
                  then begin
                  //fontsize:=Font.Size*out_factor;
                  //if fontsize<4 then CONTINUE;      // minimum to be legible.
                  //Font.Size:=Round(fontsize);
                end;

                if
                print_settings_form.output_timb_id_prefix_checkbox.Checked    // 223d
                then begin
                  if num_str = '' then
                    idtb_str := idnum_str               // 208a  template ID only
                  else
                    idtb_str := idnum_str + '.' + num_str;  // 208a  timber number output with template ID
                end
                else begin
                  // IDs not wanted  223d
                  if num_str = '' then
                    CONTINUE;
                  idtb_str := num_str;
                end;

                set_fill_colour(clBlack);    // !!! THIS IS WRONG !!!!
                set_font(pdf_font_helv, 8);

                write_text(move_to.X,//-(TextWidth(idtb_str) div 2),
                  move_to.Y,//-(TextHeight(idtb_str) div 2),
                  idtb_str,
                  tpBottomCentre, True);

                //Font.Assign(print_labels_font);      // reset for grid labels
              end;
            end;
          end;//numbering


          601..605, 701..703: begin

                if out_factor <> 1.0 then
                  CONTINUE;     // on full size prints only

                p1 := list_bgnd_marks[i].p1;              // x1,y1 in  1/100ths mm

                move_to := page_locate(p1, grid_left, grid_top);

                if check_limit(False, False, move_to)
                then begin
                  //Font.Assign(print_timber_numbers_font);
                  //
                  //Font.Style:=[fsBold,fsItalic];
                  //set_pen_color(printguide_colour);

                  //if scale>3 then Font.Size:=Font.Size+1; // a bit bigger above 3mm/ft
                  //
                  //Brush.Style:=bsSolid;
                  //Brush.Color:=clWhite;
                  //set_fill_color(printguide_colour);

                  switch_label_str := switch_label_text(code);

                  write_text(move_to.X,
                    move_to.Y,
                    switch_label_str,
                    tpMiddleCentre, True);

                  //Font.Assign(print_labels_font);      // reset for grid labels
                end;
              end;

            end;//case

        end;//next i background mark
        write_comment('==> MARK CODES FINISHED <==');

      end;//with now_keep
    end;//next n template
  end;//with pdf_page
end;
//__________________________________________________________________________________________

procedure pdf_bgnd(grid_left, grid_top: double; pdf_page: TPDF_Page);
// print background templates.

var
  //single_colour_flag:boolean;
  max_list_index: integer;
  move_to, line_to: TPoint;
  p1, p2: TPoint;
  now_keep: Tbgnd_keep;

  n, aq, nk: integer;
  array_max: integer;

  pt: TPoint;
  xint, yint: integer;

  l_dims_valid: boolean;
  w_dims_valid: boolean;

  now, rail: integer;

  mapping_colour: integer;
  using_mapping_colour: boolean;

  fixed_diamond_ends: boolean;

  gaunt_template: boolean;  // 0.93.a  ex 0.81

  fb_kludge_this: integer;  // 0.94.a

  this_one_platforms_trackbed: boolean;  // 206b

  this_one_trackbed_cess_ms: boolean;       // 206b
  this_one_trackbed_cess_ts: boolean;       // 206b

  linestyle: Tpdf_linestyle;
  this_template: Ttemplate;

  ////////////////////////////////////////////////////////////

  procedure set_pen_railcolour(rail_edges: boolean);  // 0.76.a 3-11-01.

  begin
    with pdf_page do begin

      if pdf_black_white then begin
        set_pen_colour(clBlack);
        EXIT;
      end;

      if output_diagram_mode
      // 0.94.a  don't use mapping colour for rail edges (used for infill instead).
      then begin
        if rail in [16, 20]                  // 0.93.a platforms
        then
          set_pen_colour(printplat_edge_colour)
        else
          set_pen_colour(printbgrail_colour);
        EXIT;
      end;

      if using_mapping_colour and ((not pdf_form.black_edges_checkbox.Checked) or
        (not rail_edges)) then begin
        set_pen_colour(mapping_colour);
        EXIT;
      end;

      if (mapping_colours_print < 0) and ((not pdf_form.black_edges_checkbox.Checked) or
        (not rail_edges))
      // 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.
      then begin
        set_pen_colour(printbg_single_colour);
        // single colour for all of background templates.
        EXIT;
      end;

      // normal output...

      if rail in [16, 20]   // 0.93.a platforms
      then
        set_pen_colour(printplat_edge_colour)
      else
        set_pen_colour(printbgrail_colour);

    end;//with
  end;


  ////////////////////////////////////////////////////////////

  procedure set_rail_linestyle_colours();

  begin
    // Default to normal output...

    lsRail.colour := printbgrail_colour;
    lsPlatform.colour := printplat_edge_colour;
    lsBlanking.colour := printbgrail_colour;
    lsEdge.colour := printbgrail_colour;

    // ... then check for special cases ...

    if pdf_black_white then begin       // all black
      lsRail.colour := clBlack;
      lsPlatform.colour := clBlack;
      lsBlanking.colour := clBlack;
      lsEdge.colour := clBlack;
      EXIT;
    end;

    if output_diagram_mode then begin   // all black except platforms
        lsRail.colour := clBlack;
        lsPlatform.colour := printplat_edge_colour;
        lsBlanking.colour := clBlack;
        lsEdge.colour := clBlack;
        EXIT;
    end;

    if using_mapping_colour and (not pdf_form.black_edges_checkbox.Checked) then begin  // mapping colour
      lsRail.colour := mapping_colour;
      lsPlatform.colour := mapping_colour;
      lsBlanking.colour := mapping_colour;
      lsEdge.colour := mapping_colour;
      EXIT;
    end;

    if (mapping_colours_print < 0) and (not pdf_form.black_edges_checkbox.Checked)
    // 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.
    then begin                          // single colour
      lsRail.colour := printbg_single_colour;
      lsPlatform.colour := printbg_single_colour;
      lsBlanking.colour := printbg_single_colour;
      lsEdge.colour := printbg_single_colour;
      EXIT;
    end;

  end;
  ///////////////////////////////////////////////////////////////

  function pbg_get_w_dots(q, n: integer): integer;

  var
    yint: integer;

  begin
    yint := now_keep.list_bgnd_rails[q][n].Y;

    Result := Round((yint - grid_left) * scaw_out) + page_left_dots;

    w_dims_valid := check_draw_dim_w(Result);
  end;
  ////////////////////////////

  function pbg_get_l_dots(q, n: integer): integer;

  var
    xint: integer;

  begin
    xint := now_keep.list_bgnd_rails[q][n].X;

    Result := Round((xint - grid_top) * scal_out) + page_top_dots;

    l_dims_valid := check_draw_dim_l(Result);
  end;

  ////////////////////////////////////////////////////////////


  procedure pbg_outline_railedge(aq, blanking_colour: integer; blank_it: boolean);

  var
    nk: integer;

  begin
    with now_keep do begin
      if Length(list_bgnd_rails[aq]) = 0 then
        EXIT;                         // empty rail.

      pt := list_bgnd_rails[aq][0];
      xint := pt.X;
      yint := pt.Y;

      move_to.X := Round((yint - grid_left) * scaw_out) + page_left_dots;
      move_to.Y := Round((xint - grid_top) * scal_out) + page_top_dots;

      with pdf_page do begin

        if blank_it then
          //set_pen_colour(blanking_colour)
          linestyle := lsBlanking
        else
          //set_pen_railcolour(True);
          linestyle := lsRail;

        {if (single_colour_flag=True) and (pdf_form.black_edges_checkbox.Checked=False)
           then Pen.Color:=printbg_single_colour  // default colour for all background templates.
           else Pen.Color:=printbgrail_colour;}

        for nk := 1 to High(list_bgnd_rails[aq]) do begin

          pt := list_bgnd_rails[aq][nk];
          xint := pt.X;
          yint := pt.Y;

          line_to.X := Round((yint - grid_left) * scaw_out) + page_left_dots;
          line_to.Y := Round((xint - grid_top) * scal_out) + page_top_dots;

          if check_limits(move_to, line_to) then
          begin
            draw_line_style(move_to, line_to, linestyle);
          end;
          move_to := line_to;
        end;//next nk

      end;//with Canvas
    end;//with bgnd template
  end;
  ////////////////////////////

  procedure pbg_draw_fill_rail(outer_add: integer);
  // draw a complete filled rail.

  const
    dots_max_c = xy_pts_c * 2;

  var
    dots: array[0..dots_max_c] of TPoint;
    // array of points for filled polygon mode.

    //    (xy_pts_c=3000;)
    //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
    //   template max is 4500' scale length.
    //   ( = 18000 mm or 59ft approx in 4 mm scale).
    //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

    // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
    // because needed for the Polygon function.

    // total memory = 6000*8 bytes = 48kb.

    now, start, now_max: integer;
    edge_started: boolean;
    dots_index: integer;
    x_dots, y_dots: integer;
    aq: integer;
    mid_dots_index: integer;
    edge_colour, blanking_colour: integer;

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    procedure pbg_modify_rail_end(
      start_index, stop_index, edge, blank: integer);

    //var
    //  // Asuming PDF works, no need for screwing around with pen width here
    //  saved_pen_width: TPDF_PenWidth;    // 206b

    begin

      if (start_index >= 0) and (start_index <= dots_index) and
        (stop_index >= 0) and (stop_index <= dots_index) then begin
        move_to := dots[start_index];
        line_to := dots[stop_index];

        if check_limits(move_to, line_to) then begin

          with pdf_page do begin
            //saved_pen_width :=
            //  current_pen_width; // 206b
//            if current_fill_style<>bsSolid then set_pen_width(saved_pen_width+3);    // 206b  PDF bug, needs a wider line to ensure full blanking if hatched fill
            //set_pen_colour(blank);
            // first blank across..
            draw_line_style(move_to, line_to, lsBlanking);

            //set_pen_width(saved_pen_width);
            // 206b restore original width

            //set_pen_colour(edge);
            // then restore the corner points..
            draw_line_style(move_to, move_to, lsEdge);

            draw_line_style(line_to, line_to, lsEdge);
          end;//with
        end;
      end;
    end;
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  begin
    aq := rail;  // gauge-faces.

    with now_keep do begin

      if (Length(list_bgnd_rails[aq]) = 0) or
        (Length(list_bgnd_rails[aq + outer_add]) = 0)  // data not for both edges?
      then begin
        if Length(list_bgnd_rails[aq]) <> 0 then
          pbg_outline_railedge(aq, 0, False);
        if Length(list_bgnd_rails[aq + outer_add]) <> 0 then
          pbg_outline_railedge(aq + outer_add, 0, False);
        EXIT;
      end;

      now_max := High(list_bgnd_rails[aq]);

      if not gaunt_template then begin

        case aq of
          1: begin
            {start:=list_mark_straight;  // out - bug fix 0.73 11-8-01. }

            start := planing_end_aq1;
            // start from end of planing - no infill in planing.

            if (start < 0) or (start > now_max) then
              EXIT;  // ???

            //out 18-8-01 0.73.a   if ((start+1)<=now_max) and (now_max>1) then drawn_full_aq1:=False;  // ok to overdraw planing.
          end;

          2: begin                         // ditto
            {start:=list_mark_curve;  // out - bug fix 0.73 11-8-01. }

            start := planing_end_aq2;
            // start from end of planing - no infill in planing.

            if (start < 0) or (start > now_max) then
              EXIT;  // ???

            //out 18-8-01 0.73.a   if ((start+1)<=now_max) and (now_max>1) then drawn_full_aq2:=False;
          end;

          else
            start := 0;                   // whole list.
        end;//case

      end
      else
        start := 0;  // gaunt template, no planing. 0.81 09-Jul-2005


      dots_index := 0 - 1;                    // first increment is to zero.

      edge_started := False;

      for now := start to now_max do begin
        x_dots := pbg_get_w_dots(aq, now);
        y_dots := pbg_get_l_dots(aq, now);
        if w_dims_valid and l_dims_valid
        then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      mid_dots_index := dots_index;

      aq := rail + outer_add;             // outer-edges.

      now_max := High(list_bgnd_rails[aq]);

      edge_started := False;

      for now := now_max downto 0 do begin
        x_dots := pbg_get_w_dots(aq, now);
        y_dots := pbg_get_l_dots(aq, now);
        if w_dims_valid and l_dims_valid
        then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      with pdf_page do begin

//        set_pen_railcolour(True);
//        set_pen_width(printrail_wide);

                        {if (single_colour_flag=True) and (pdf_form.black_edges_checkbox.Checked=False)
                           then Pen.Color:=printbg_single_colour  // default colour for all background templates.
                           else Pen.Color:=printbgrail_colour;}

        if rail in [16, 20]   // 0.93.a platforms
        then begin
          //if ((using_mapping_colour = True) and
          //  (current_pen_colour = mapping_colour)) or ((mapping_colours_print < 0) and
          //  (current_pen_colour = printbg_single_colour))   // 206b
          //then
          //  set_fill_colour(current_pen_colour)
          //else
          //  set_fill_colour(printplat_infill_colour);

          //case print_platform_infill_style of
          //        0: Brush.Style:=bsClear;
          //        1: Brush.Style:=bsFDiagonal;    // hatched. forward diagonal (backward diagonal on bgnd template timbers).
          //        2: Brush.Style:=bsDiagCross;
          //
          //        3: if mapping_colours_print<0          // solid option.
          //              then Brush.Style:=bsFDiagonal    // single colour.
          //              else Brush.Style:=bsSolid;
          //
          //      else begin                         // 4 = blank.
          //             Brush.Style:=bsSolid;
          //             Brush.Color:=clWhite;       // overide.
          //           end;
          //
          //end;//case
        end
        else begin
          if (this_one_trackbed_cess_ts and (rail = 18))  // 215a
            or (this_one_trackbed_cess_ms and (rail = 22))  // 215a
          then begin
            if (using_mapping_colour and
              (current_pen_colour = mapping_colour)) or ((mapping_colours_print < 0) and
              (current_pen_colour = printbg_single_colour)) then
              set_fill_colour(current_pen_colour)
            else
              set_fill_colour(sb_track_bgnd_colour);      // cess use same colour as track background
            //  Brush.Style:=bsBDiagonal;
          end
          else begin   // normal rails...

            if (using_mapping_colour and
              (current_fill_colour = mapping_colour)) or ((mapping_colours_print < 0) and
              (current_fill_colour = printbg_single_colour)) then
              set_fill_colour(calc_intensity(clSilver))  // 214b  - was clGray
            else begin
              if fb_kludge_this > 0 then
                set_fill_colour(printrail_infill_colour_cu)  // 0.94.a
              else
                set_fill_colour(printrail_infill_colour_bg);
            end;

            //case rail_infill_i of
            //                1: Brush.Style:=bsBDiagonal;   // hatched
            //                2: Brush.Style:=bsSolid;       // solid
            //                3: Brush.Style:=bsDiagCross;   // cross_hatched
            //                4: begin                       // blank
            //                     Brush.Style:=bsSolid;
            //                     Brush.Color:=clWhite;     // overide
            //                   end;
            //              else Brush.Style:=bsSolid;       // solid
            //end;//case

          end;
        end;

        //if pdf_black_white = True then begin
        //  set_fill_colour(clWhite);               // overide
        //end;
        //set_pen_width(printrail_wide);

        case rail of
          1: set_pen_colour(clRed);
        end;

        if dots_index > 2 then begin
          polygon_style(Slice(dots, dots_index + 1), lsRail);
          // +1, number of points, not index.  must have 4 points.

          edge_colour := current_pen_colour;  // existing rail edges.

          //if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
          //                       else blanking_colour:=clWhite;
          //
          //      // remove polygon line across end of planing (not for fixed-diamond)..
          //      // (for gaunt template this removes the polygon line across the rail end)

          if (not fixed_diamond_ends) and (rail in [1, 2]) then
            pbg_modify_rail_end(0, dots_index, edge_colour, blanking_colour);

          // remove polygon lines across stock rail ends...
          // and trackbed ends  206b

          if rail in[0, 3, 18, 22]   // 18,22 added 206b
          then begin
            pbg_modify_rail_end(
              0, dots_index, edge_colour, blanking_colour);  // toe or approach end.

            pbg_modify_rail_end(
              mid_dots_index, mid_dots_index + 1, edge_colour, blanking_colour);  // exit end.
          end;

          // 0.93.a blank platform edges ...

          with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info do begin

            if adjacent_edges_keep  // platforms
            then begin
              // 0.93.a blank platform rear edges ...

              if (rail = 16) and
                draw_ts_platform_keep and (not draw_ts_platform_rear_edge_keep)
              // 0.93.a TS platform start
              then
                pbg_outline_railedge(16, blanking_colour, True);     // blank rear edge

              if (rail = 20) and
                draw_ms_platform_keep and (not draw_ms_platform_rear_edge_keep)
              // 0.93.a TS platform start
              then
                pbg_outline_railedge(20, blanking_colour, True);     // blank rear edge

              // 0.93.a blank platform ends ...

              if (rail = 16) and
                draw_ts_platform_keep and (not draw_ts_platform_start_edge_keep)
              // 0.93.a TS platform start
              then
                pbg_modify_rail_end(0, dots_index, edge_colour, blanking_colour);

              if (rail = 16) and
                draw_ts_platform_keep and (not draw_ts_platform_end_edge_keep)
              // 0.93.a TS platform end
              then
                pbg_modify_rail_end(mid_dots_index, mid_dots_index + 1, edge_colour, blanking_colour);

              if (rail = 20) and
                draw_ms_platform_keep and (not draw_ms_platform_start_edge_keep)
              // 0.93.a MS platform start
              then
                pbg_modify_rail_end(0, dots_index, edge_colour, blanking_colour);

              if (rail = 20) and
                draw_ms_platform_keep and (not draw_ms_platform_end_edge_keep)
              // 0.93.a MS platform end
              then
                pbg_modify_rail_end(mid_dots_index, mid_dots_index + 1, edge_colour, blanking_colour);

            end;
          end;//with

          if rail in [26, 28]
          then
          begin
            pbg_modify_rail_end(
              0, dots_index, edge_colour, blanking_colour);  // centre of K-crossing check rails.
          end;

        end;
      end;//with Page
    end;//with background template
  end;
  ////////////////////////////////////////////////////////////////////////

  procedure pbg_draw_fill_vee;    // do complete vee in one go ...

  const
    dots_max_c = xy_pts_c * 2;

  var
    dots: array[0..dots_max_c] of TPoint;
    // array of points for filled polygon mode.

    //    (xy_pts_c=3000;)
    //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
    //   template max is 4500' scale length.
    //   ( = 18000 mm or 59ft approx in 4 mm scale).
    //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

    // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
    // because needed for the Polygon function.

    // total memory = 6000*8 bytes = 48kb.

    now: integer;
    edge_started: boolean;
    dots_index: integer;
    x_dots, y_dots: integer;
    aq: integer;
    point_mid_dots_index, splice_mid_dots_index: integer;
    edge_colour, blanking_colour: integer;

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    procedure pbg_modify_vee_end(
      start_index, stop_index, edge, blank: integer);

    begin

      if (start_index >= 0) and (start_index <= dots_index) and
        (stop_index >= 0) and (stop_index <= dots_index) then begin
        move_to := dots[start_index];
        line_to := dots[stop_index];

        if check_limits(move_to, line_to) then begin
          with pdf_page do begin

            set_pen_colour(blank);
            // first blank across..
            draw_line(move_to, line_to);

            set_pen_colour(edge);
            // then restore the corner points..
            draw_line(move_to, move_to);

            draw_line(line_to, line_to);
          end;//with
        end;
      end;
    end;
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  begin
    with now_keep do begin

      if (Length(list_bgnd_rails[4]) = 0) or
        (Length(list_bgnd_rails[5]) = 0) or
        (Length(list_bgnd_rails[12]) = 0) or
        (Length(list_bgnd_rails[13]) = 0)  // not enough data for filled vee.
      then begin
        if Length(list_bgnd_rails[4]) <> 0 then
          pbg_outline_railedge(4, 0, False);       // draw outline vee...
        if Length(list_bgnd_rails[5]) <> 0 then
          pbg_outline_railedge(5, 0, False);
        if Length(list_bgnd_rails[12]) <> 0 then
          pbg_outline_railedge(12, 0, False);
        if Length(list_bgnd_rails[13]) <> 0 then
          pbg_outline_railedge(13, 0, False);
      end
      else begin                // polygon mode...

        dots_index := 0 - 1;   // first increment is to zero.

        aq := 4;
        edge_started := False;
        for now := 0 to High(list_bgnd_rails[aq]) do
        begin    // vee main-side, gauge_face, start from the tip.
          x_dots := pbg_get_w_dots(aq, now);
          y_dots := pbg_get_l_dots(aq, now);
          if w_dims_valid and l_dims_valid
          then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now

        point_mid_dots_index := dots_index;

        aq := 12;
        edge_started := False;
        for now := High(list_bgnd_rails[aq]) downto 0 do
        begin // back along outer-edge.
          x_dots := pbg_get_w_dots(aq, now);
          y_dots := pbg_get_l_dots(aq, now);
          if w_dims_valid and l_dims_valid
          then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now

        aq := 13;
        edge_started := False;
        for now := 0 to High(list_bgnd_rails[aq]) do
        begin    // and then turnout side outer edge.
          x_dots := pbg_get_w_dots(aq, now);
          y_dots := pbg_get_l_dots(aq, now);
          if w_dims_valid and l_dims_valid
          then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now

        splice_mid_dots_index := dots_index;

        aq := 5;
        edge_started := False;
        for now := High(list_bgnd_rails[aq]) downto 0 do
        begin // and back along the gauge face to the tip.
          x_dots := pbg_get_w_dots(aq, now);
          y_dots := pbg_get_l_dots(aq, now);
          if w_dims_valid and l_dims_valid
          then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now

        with pdf_page do begin

          set_pen_railcolour(True);
          set_pen_width(printrail_wide);


                                  {if (single_colour_flag=True) and (pdf_form.black_edges_checkbox.Checked=False)
                                     then Pen.Color:=printbg_single_colour  // default colour for all background templates.
                                     else Pen.Color:=printbgrail_colour;}

          //if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
          //   then Brush.Color:=calc_intensity(clSilver)  // 214b  - was clGray
          //
          //   else begin
          //          if fb_kludge_this>0 then Brush.Color:=printrail_infill_colour_cu
          //                              else Brush.Color:=printrail_infill_colour_bg;
          //        end;

          //case rail_infill_i of
          //                1: Brush.Style:=bsBDiagonal;   // hatched
          //                2: Brush.Style:=bsSolid;       // solid
          //                3: Brush.Style:=bsDiagCross;   // cross_hatched
          //                4: begin                       // blank
          //                     Brush.Style:=bsSolid;
          //                     Brush.Color:=clWhite;     // overide
          //                   end;
          //              else Brush.Style:=bsSolid;       // solid
          //end;//case

          if {(} pdf_black_white {) or (impact>0)}
          {or (mapping_colours_print<0)} then begin
            set_fill_colour(clWhite);              // overide
          end;

          if dots_index > 4 then begin
            polygon(Slice(dots, dots_index + 1));
            // +1, number of points, not index.  must have at least 5 points.

            edge_colour := current_pen_colour;
            // existing rail edges.

            //if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
            //                       else blanking_colour:=clWhite;

            // remove polygon lines across vee rail ends...

            pbg_modify_vee_end(
              point_mid_dots_index, point_mid_dots_index + 1, edge_colour, blanking_colour); // point rail end.

            pbg_modify_vee_end(
              splice_mid_dots_index, splice_mid_dots_index + 1, edge_colour, blanking_colour); // splice rail end.

          end;

        end;//with Canvas

      end;//polygon mode
    end;//with background template
  end;
  ////////////////////////////////////////////////////////////////////////

  procedure pbg_mark_end(aq1, aq1end, aq2, aq2end: integer);
  // print the background rail end mark.

  begin
    with now_keep do begin
      if bgnd_endmarks_yn[aq1, aq1end] and
         bgnd_endmarks_yn[aq2, aq2end] then begin
        p1 := bgnd_endmarks[aq1, aq1end];
        p2 := bgnd_endmarks[aq2, aq2end];

        with pdf_page do begin

          //set_pen_railcolour(True);
          linestyle := lsRail;

                                  {if (single_colour_flag=True) and (pdf_form.black_edges_checkbox.Checked=False)
                                     then Pen.Color:=printbg_single_colour  // default colour for all background templates.
                                     else Pen.Color:=printbgrail_colour;}

          move_to.X := Round((p1.Y - grid_left) * scaw_out) + page_left_dots;
          move_to.Y := Round((p1.X - grid_top) * scal_out) + page_top_dots;

          line_to.X := Round((p2.Y - grid_left) * scaw_out) + page_left_dots;
          line_to.Y := Round((p2.X - grid_top) * scal_out) + page_top_dots;

          if check_limits(move_to, line_to) then
            draw_line_style(move_to, line_to, linestyle);
        end;//with
      end;
    end;//with
  end;
  ////////////////////////////////////////////////////////////

  procedure pbg_outline_railends;
  // draw in the rail ends using existing pen settings...

  begin
    pbg_mark_end(1, 1, 9, 1);    // main rail wing rail finish.
    pbg_mark_end(2, 1, 10, 1);   // turnout rail wing rail finish.

    pbg_mark_end(6, 0, 14, 0);   // main side check rail start.
    pbg_mark_end(6, 1, 14, 1);   // main side check rail finish.

    pbg_mark_end(7, 0, 15, 0);   // turnout side check rail start.
    pbg_mark_end(7, 1, 15, 1);   // turnout side check rail finish.

    pbg_mark_end(4, 0, 5, 0);    // blunt nose.

    if fixed_diamond_ends then begin
      pbg_mark_end(1, 0, 9, 0);
      // planed faced of point rails for a fixed-diamond.
      pbg_mark_end(2, 0, 10, 0);

      pbg_mark_end(26, 1, 27, 1);     // MS K-crossing check rails.
      pbg_mark_end(28, 1, 29, 1);     // DS K-crossing check rails.
    end;

  end;
  ////////////////////////////////////////////////////////////////
  procedure pbg_draw_diagram_mode;    // 0.91.d draw a complete template in diagrammatic mode

  const
    dots_max_c = xy_pts_c * 2;

  var
    dots: array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

    //    (xy_pts_c=3000;)
    //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
    //   template max is 4500' scale length.
    //   ( = 18000 mm or 59ft approx in 4 mm scale).
    //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

    // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
    // because needed for the Polygon function.

    // total memory = 6000*8 bytes = 48kb.

    now, now_max: integer;
    edge_started: boolean;
    dots_index: integer;
    x_dots, y_dots: integer;
    ms_mid_dots_index, ts_mid_dots_index: integer;
    edge_colour, blanking_colour: integer;

    omitting_vee: boolean;

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    procedure pbg_modify_boundary(start_index, stop_index, edge, blank: integer);

    begin

      if (start_index >= 0) and (start_index <= dots_index) and
        (stop_index >= 0) and (stop_index <= dots_index) then begin
        move_to := dots[start_index];
        line_to := dots[stop_index];

        if check_limits(move_to, line_to)
        then begin
          with pdf_page do begin
            set_pen_colour(blank);
            // first blank across..
            draw_open_line(move_to, line_to, edge);
          end;//with
        end;
      end;
    end;
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  begin
    with now_keep do begin

      if Length(list_bgnd_rails[0]) = 0 then
        EXIT;    // no data for straight stock rail
      if Length(list_bgnd_rails[3]) = 0 then
        EXIT;    // no data for curved stock rail

      if (Length(list_bgnd_rails[4]) = 0) or (Length(list_bgnd_rails[5]) = 0)
      // no data for vee rails
      then
        omitting_vee := True
      else
        omitting_vee := False;

      dots_index := 0 - 1;                    // first increment is to zero.


      now_max := High(list_bgnd_rails[0]);    // straight stock rail
      edge_started := False;

      for now := 0 to now_max do begin
        x_dots := pbg_get_w_dots(0, now);
        y_dots := pbg_get_l_dots(0, now);
        if w_dims_valid and l_dims_valid then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      ms_mid_dots_index := dots_index;

      if not omitting_vee then begin
        now_max := High(list_bgnd_rails[4]);    // point rail
        edge_started := False;

        for now := now_max downto 0 do begin
          x_dots := pbg_get_w_dots(4, now);
          y_dots := pbg_get_l_dots(4, now);
          if w_dims_valid and l_dims_valid then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now


        now_max := High(list_bgnd_rails[5]);    // splice rail
        edge_started := False;

        for now := 0 to now_max do begin
          x_dots := pbg_get_w_dots(5, now);
          y_dots := pbg_get_l_dots(5, now);
          if w_dims_valid and l_dims_valid then begin
            edge_started := True;

            Inc(dots_index);
            if dots_index > dots_max_c then
              dots_index := dots_max_c;

            dots[dots_index].X := x_dots;
            dots[dots_index].Y := y_dots;
          end
          else
          if edge_started then
            BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
        end;//next now
      end;//if vee

      ts_mid_dots_index := dots_index;

      now_max := High(list_bgnd_rails[3]);    // curved stock rail
      edge_started := False;

      for now := now_max downto 0 do begin
        x_dots := pbg_get_w_dots(3, now);
        y_dots := pbg_get_l_dots(3, now);
        if w_dims_valid and l_dims_valid then begin
          edge_started := True;

          Inc(dots_index);
          if dots_index > dots_max_c then
            dots_index := dots_max_c;

          dots[dots_index].X := x_dots;
          dots[dots_index].Y := y_dots;
        end
        else
        if edge_started then
          BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
      end;//next now

      with pdf_page do begin

        set_pen_railcolour(True);

        set_pen_width(max(printrail_wide, min_penwidth));

        {if (single_colour_flag=True) and (pdf_form.black_edges_checkbox.Checked=False)
           then Pen.Color:=printbg_single_colour  // default colour for all background templates.
           else Pen.Color:=printbgrail_colour;}
{
        if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
           then Brush.Color:=calc_intensity(clGray)
           else Brush.Color:=printrail_infill_colour_bg;
}

        if using_mapping_colour then
          set_fill_colour(mapping_colour)
        else
          set_fill_colour(sb_diagram_colour);  // 209c  was printrail_infill_colour_bg;

        //        case rail_infill_i of
        //                        1: Brush.Style:=bsBDiagonal;   // hatched
        //                        2: Brush.Style:=bsSolid;       // solid
        //                        3: Brush.Style:=bsDiagCross;   // cross_hatched
        //                        4: begin                       // blank
        //                             Brush.Style:=bsSolid;
        //                             Brush.Color:=clWhite;     // overide
        //                           end;
        //                      else Brush.Style:=bsSolid;       // solid
        //        end;//case

        //if {(} pdf_black_white=True {) or (impact>0)} {or (mapping_colours_print<0)}
        //   then begin
        //          Brush.Style:=bsSolid;               // solid infill white.
        //          Brush.Color:=clWhite;               // overide
        //        end;

        if dots_index > 2 then begin
          Polygon(Slice(dots, dots_index + 1));
          // +1, number of points, not index.  must have 4 points.

          // blank out template boundaries...

          if not output_include_boundaries then begin
            edge_colour := current_pen_colour;  // existing rail edges.
            //
            //          if Brush.Style=bsSolid then blanking_colour:=Brush.Color   // infill colour.
            //                                 else blanking_colour:=clWhite;


            pbg_modify_boundary(0, dots_index, edge_colour, blanking_colour);
            // toe or approach end.

            pbg_modify_boundary(ms_mid_dots_index, ms_mid_dots_index + 1,
              edge_colour, blanking_colour);  // exit end (turnout) or Ctrl-1 end (plain track).

            if not omitting_vee then
              pbg_modify_boundary(ts_mid_dots_index, ts_mid_dots_index + 1, edge_colour, blanking_colour);
            // turnout road end.
          end;
        end;

        if output_show_points_mark  // mark position of points
        then begin

          if (not Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.turnout_info1.plain_track_flag)   // not for plain track
            and (not Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.turnout_info2.semi_diamond_flag)            // not for half-diamond
            and (not Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.turnout_info2.gaunt_flag)                   // not for gaunt turnout

            and (Length(list_bgnd_rails[1]) <> 0)    // data for straight switch rail
            and (Length(list_bgnd_rails[2]) <> 0)    // data for curved stock rail
          then begin
            if current_fill_colour = clWhite then
              set_pen_colour(clBlack)
            else
              set_pen_colour(clWhite);     // white points mark

            x_dots := pbg_get_w_dots(1, 0);  // ms toe.
            y_dots := pbg_get_l_dots(1, 0);

            if w_dims_valid and l_dims_valid
            then begin
              x_dots := pbg_get_w_dots(2, 0);  // ts toe.
              y_dots := pbg_get_l_dots(2, 0);

              if w_dims_valid and l_dims_valid
              then
                draw_line(pbg_get_w_dots(1, 0),
                  pbg_get_l_dots(1, 0),
                  pbg_get_w_dots(2, 0),
                  pbg_get_l_dots(2, 0));
            end;
          end;
        end;//mark points

      end;//with Canvas
    end;//with background template
  end;
  ////////////////////////////////////////////////////////////////////////

begin          // print background templates...

  //if pad_form.print_keeps_on_menu_entry.Checked=False then EXIT;

  max_list_index := keeps_list.Count - 1;

  if max_list_index < 0 then
    EXIT;  // no templates in box.

  if not output_diagram_mode then
    pdf_bgnd_marks(grid_left, grid_top, max_list_index, False, pdf_page);
  // 0.91.d if // first print all the background timbering and marks except rail joints.

  //single_colour_flag:=(mapping_colours_print<0); {pad_form.use_single_colour_menu_entry.Checked;}

  with pdf_page do begin

    //  now print bgnd track centre-lines and turnout rails...

    for n := 0 to max_list_index do begin         // step through templates
      this_template := Ttemplate(keeps_list.Objects[n]);
      if not this_template.bg_copied then
        CONTINUE;  // no data, not on background.

      if (not this_template.group_selected) and
        print_group_only_flag then
        CONTINUE;  // not in group. 0.78.b 10-12-02.

      //if (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.fb_kludge_template_code > 0)  // 209c
      if (this_template.template_info.keep_dims.box_dims1.fb_kludge_template_code > 0)  // 209c
        and (not print_settings_form.output_fb_foot_lines_checkbox.Checked) then
        CONTINUE;                                                      // foot lines not wanted.

      this_one_platforms_trackbed :=
      //Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.
      this_template.template_info.keep_dims.box_dims1.
        platform_trackbed_info.adjacent_edges_keep;           // True = platforms and trackbed edges   206b

      //this_one_trackbed_cess:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.draw_trackbed_cess_edge_keep;  // True = cess width instead of trackbed cutting line   206b

      //this_one_trackbed_cess_ms := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep;
      this_one_trackbed_cess_ms := this_template.template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep;
      // True = cess width instead of trackbed cutting line 215a
      //this_one_trackbed_cess_ts := Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep;
      this_one_trackbed_cess_ts := this_template.template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep;
      // True = cess width instead of trackbed cutting line 215a

      //now_keep := Ttemplate(keeps_list.Objects[n]).bgnd_keep;    // next background keep.
      now_keep := this_template.bgnd_keep;    // next background keep.

      with now_keep do begin

        // mapping_colours_print: 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.

        using_mapping_colour := False;  // default init.

        //with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims do begin
        with this_template.template_info.keep_dims do begin

          if box_dims1.bgnd_code_077 <> 1 then
            CONTINUE;    // 0.77.b BUG???

          with turnout_info2 do begin
            fixed_diamond_ends := semi_diamond_flag and diamond_fixed_flag;
            // need end marks on fixed diamond point rails.
            gaunt_template := gaunt_flag;
            // 0.93.a ex 0.81
          end;//with

          if box_dims1.use_print_mapping_colour and
            ((mapping_colours_print = 1) or (mapping_colours_print = 3)) and
            (not pdf_black_white) and (not pdf_grey_shade) then begin
            mapping_colour := calc_intensity(box_dims1.print_mapping_colour);
            using_mapping_colour := True;
          end;

          if box_dims1.use_pad_marker_colour and
            (mapping_colours_print = 4)   // use pad settings instead
            and (not pdf_black_white) and (not pdf_grey_shade)
          then
          begin
            mapping_colour := calc_intensity(box_dims1.pad_marker_colour);
            using_mapping_colour := True;
          end;

          fb_kludge_this := box_dims1.fb_kludge_template_code;  // 0.94.a

          set_rail_linestyle_colours();
          if fb_kludge_this = 0   // no track centre-lines or diagram mode for kludge templates  212a
          then begin

            if output_diagram_mode then
              pbg_draw_diagram_mode;  // first draw template in diagrammatic mode.

            if (print_settings_form.output_centrelines_checkbox.Checked and
              (not output_diagram_mode) and (not box_dims1.align_info.dummy_template_flag))
              or (print_settings_form.output_bgnd_shapes_checkbox.Checked and
                  box_dims1.align_info.dummy_template_flag)  // 212a dummy templates not part of track plan

            then begin

              if box_dims1.align_info.dummy_template_flag then
                linestyle := lsCentreline_dummy
              else
                linestyle := lsCentreline_normal;

              for aq := 24 to 25 do begin         // track centre-lines.


                if Length(list_bgnd_rails[aq]) = 0 then
                  CONTINUE;                      // empty rail.

                array_max := High(list_bgnd_rails[aq]);


                //                                move_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                //                                move_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;
                //
                for nk := 1 to array_max do begin
                  //                                  line_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                  //                                  line_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;
                  //
                  //                                  if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                  //                                  move_to:=line_to;
                  draw_line_style(
                    Round(
                    (list_bgnd_rails[aq][nk - 1].Y - grid_left) * scaw_out) + page_left_dots,
                    Round(
                    (list_bgnd_rails[aq][nk - 1].X - grid_top) * scaw_out) + page_top_dots,
                    Round(
                    (list_bgnd_rails[aq][nk].Y - grid_left) * scaw_out) + page_left_dots,
                    Round(
                    (list_bgnd_rails[aq][nk].X - grid_top) * scaw_out) + page_top_dots,
                    linestyle);
                end;//next nk

              end;//next aq
            end;//if track-centres

          end;//if not kludge

        end;//with template

        if print_settings_form.output_rails_checkbox.Checked then begin

          //drawn_full_aq1:=True;    // default init flags.
          //drawn_full_aq2:=True;


//          set_pen_style(psSolid);
          if (rail_infill_i =
            0)  // out for pdf, was  or ((scale*out_factor)<0.75)   // less than 18.75% for 4mm scale (control template) (10.71% for 7mm).
            and (not output_diagram_mode) then
          begin           //  outline (pen) mode ...
            //  n.b. this mode does not automatically close the rail-ends.
            //  no infill for platforms

//            set_pen_railcolour(True);

                            {if (single_colour_flag=True) and (pdf_form.black_edges_checkbox.Checked=False)
                               then Pen.Color:=printbg_single_colour  // default colour for all background templates.
                               else Pen.Color:=printbgrail_colour;}

            for aq := 0 to 23 do begin
              // 24, 25 centre-lines already done.
              if (not this_one_platforms_trackbed) and (aq > 15) then
                CONTINUE;  // no adjacent tracks in output  // 206b

              case aq of     // 223d
                16, 17, 20, 21:
                  if not print_settings_form.output_platforms_checkbox.Checked then
                    CONTINUE;         // platforms not wanted
                18, 19, 22, 23:
                  if not print_settings_form.output_trackbed_edges_checkbox.Checked then
                    CONTINUE;    // trackbed edges not wanted
              end;//case

              pbg_outline_railedge(aq, 0, False);
            end;

            for aq := 26 to aq_max_c do
              pbg_outline_railedge(aq, 0, False);   // K-crossing check rails.

            pbg_outline_railends;
            // next, draw in the rail ends using same pen settings...
          end
          else begin      // infill (polygon) mode ...
            // do blades first - neater result.

            if not output_diagram_mode // detail mode only
            then begin

              for rail := 1 to 3 do
                pbg_draw_fill_rail(8);  // closure rails and curved stock rail.

              rail := 0;
              // straight stock rail.
              pbg_draw_fill_rail(8);

              for rail := 6 to 7 do
                pbg_draw_fill_rail(8);  // check rails
            end;

            if this_one_platforms_trackbed
            // no adjacent tracks in output 206b
            then begin
              rail := 16;
              // detail mode or diagram mode (for platforms/trackbed)
              repeat
                case rail of     // 223d
                  16, 20:
                    if print_settings_form.output_platforms_checkbox.Checked then
                      pbg_draw_fill_rail(1);        // platforms
                  18, 22:
                    if print_settings_form.output_trackbed_edges_checkbox.Checked then
                      pbg_draw_fill_rail(1);   // trackbed edges
                end;//case
                rail := rail + 2;
                if output_diagram_mode and
                  (not output_include_trackbed_edges) and (rail in [18, 22]) then
                  rail := rail + 2;  // 206b no trackbed/cess in diagram mode
              until rail > 22;
            end;

            if not output_diagram_mode // detail mode only
            then begin

              rail := 26;
              repeat
                pbg_draw_fill_rail(1);
                // K-crossing check rails.
                rail := rail + 2;
              until rail > 28;

              pbg_draw_fill_vee;    // do complete vee in one go ...

              // finally draw in the planing gauge-faces - no infill...

              //set_pen_railcolour(True);

                                      {if (single_colour_flag=True) and (pdf_form.black_edges_checkbox.Checked=False)
                                        then Pen.Color:=printbg_single_colour  // default colour for all background templates.
                                        else Pen.Color:=printbgrail_colour;}

              aq := 1;

              if (Length(list_bgnd_rails[aq]) <> 0) and
                (planing_end_aq1 > 0) { and (drawn_full_aq1=False)} then
              begin
                move_to.X := pbg_get_w_dots(aq, 0);
                move_to.Y := pbg_get_l_dots(aq, 0);
                for now := 1 to planing_end_aq1{+1} do begin
                  line_to.X := pbg_get_w_dots(aq, now);
                  line_to.Y := pbg_get_l_dots(aq, now);
                  if check_limits(move_to, line_to)
                    then begin
                    draw_line_style(move_to, line_to, lsRail);
                  end;
                  move_to := line_to;
                end;//for
              end;

              aq := 2;
              if (Length(list_bgnd_rails[aq]) <> 0) and
                (planing_end_aq2 > 0) { and (drawn_full_aq2=False)} then
              begin
                move_to.X := pbg_get_w_dots(aq, 0);
                move_to.Y := pbg_get_l_dots(aq, 0);
                for now := 1 to planing_end_aq2{+1} do begin
                  line_to.X := pbg_get_w_dots(aq, now);
                  line_to.Y := pbg_get_l_dots(aq, now);
                  if check_limits(move_to, line_to) then
                      draw_line_style(move_to, line_to, lsRail);
                  move_to := line_to;
                end;//for
              end;
            end;//detail mode

          end;//polygon mode

          // 209c                  if output_diagram_mode=False then pdf_bgnd_marks(grid_left,grid_top,max_list_index,True);

        end;//if rails

      end;//with bgnd_keep
    end;//for next n template
  end;//with pdf_page

  // finally add the rail-joint marks over the rail infill...   // 209c moved outside loop

  if print_settings_form.output_rails_checkbox.Checked and
    (not output_diagram_mode) then
    pdf_bgnd_marks(grid_left, grid_top, max_list_index, True, pdf_page);

end;
//________________________________________________________________________________________

procedure Tpdf_form.how_panelClick(Sender: TObject);

begin
  if help(-1, pdf_help_str, 'about  mapping  colours') = 1 then
    pad_form.marker_and_mapping_colours_help_menu_entry.Click;
end;
//____________________________________________________________________________________________

procedure Tpdf_form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin
  if print_busy then begin
    CanClose := False;                    // can''t close if still busy.
    omit_all_button.Click;
  end
  else
    CanClose := True;
end;
//___________________________________________________________________________________________

procedure Tpdf_form.banner_fill_checkboxClick(Sender: TObject);

begin
  button_clicked := True;
  banner_changed := True;
end;
//_______________________________________________________________________________________

procedure Tpdf_form.font_buttonClick(Sender: TObject);

begin
  preview_form.Font.Assign(get_font(
    'choose  a  new  font  and  text  colour  for  the  preview  page  labels',
    preview_form.Font, True));

  if not printer_printing then
    print_preview(True, True, 0);  // redraw in new font.
end;
//__________________________________________________________________________________________

procedure Tpdf_form.black_edges_checkboxClick(Sender: TObject);

// need to repeat the colour setup, already set before form shows.
begin
  print_colours_setup;
end;
//_______________________________________________________________________________________

procedure Tpdf_form.FormResize(Sender: TObject);

begin
  if Showing and initdone_flag and (not form_scaling)
  //  otherwise clobbers Windows on startup or quit.
  then
    ScrollInView(all_button);
end;
//___________________________________________________________________________________________

procedure intensity_changed;

var
  n: integer;

begin
  print_colours_setup;            // set up the new colours.

  // clear any existing rotated bitmaps, so new ones are created with new colours...

  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if Count > 0 then begin
      for n := 0 to Count - 1 do begin

        if Tbgshape(Objects[n]).bgnd_shape.shape_code <> -1 then
          CONTINUE;     // not a picture

        Tbgshape(Objects[n]).bgimage.image_shape.rotated_bitmap.Free;
        // clear the bitmap...
        Tbgshape(Objects[n]).bgimage.image_shape.rotated_bitmap := TBitmap.Create;
      end;//for
    end;
  end;//with
end;
//__________________________________________________________________________________________
{ T3-OUT
procedure pdf_rotate_metafile(i:integer);   // rotate metafile supplied 90degs clockwise.   213b

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
  dest_metafile.Enhanced:=False;
  dest_metafile.Transparent:=True;

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
                  'Preparation of the following picture shape for PDF output has failed:'
                 +'||'+str
                 +'||The failure was caused by insufficient resources in your system.',
                  '','','','','','O K',0);
          EXIT;
        end;//try

        dest_canvas:=TMetafileCanvas.Create(dest_metafile,0);    // 0 = use screen as reference device

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
}

procedure pdf_rotate_bitmap(i: integer);     // rotate bitmap supplied 90degs clockwise.

var
  inrow, incol: integer;
  inwidth, inheight: integer;
  pixel_colour: TColor;
  saved_screen_cursor: TCursor;
  str: string;

begin

  saved_screen_cursor := Screen.Cursor;
  Screen.Cursor := crHourglass;

  try
    with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]) do begin

      str := bgnd_shape.shape_name;

      with bgimage.image_shape do begin

        inwidth := image_bitmap.Width;
        inheight := image_bitmap.Height;

        rotated_bitmap.Monochrome := image_bitmap.Monochrome;

        try
          rotated_bitmap.Width := inheight;
          rotated_bitmap.Height := inwidth;
        except
          alert(2, '    image  preparation  failure',
            'Sorry, preparation of the following picture shape for PDF output has failed:'
            + '||' + str +
            '||The failure was caused by insufficient resources in your system.' +
            '||Try re-scanning the image at a lower resolution, or if it is colour image try re-scanning in grey-scale or black and white.',
            '', '', '', '', '', 'O K', 0);
          EXIT;
        end;//try

        with rotated_bitmap do begin
          with Canvas do begin
            Brush.Color := clWhite;          //  this clears the new bitmap for starters.
            Brush.Style := bsSolid;
            FillRect(Rect(0, 0, Width, Height));
          end;//with
        end;//with

        if inheight < 32767 then
          pdf_form.row_progressbar.Max := inheight    // Delphi 16-bit limits..
        else
          pdf_form.row_progressbar.Max := 32767;

        pdf_form.row_progressbar.Position := 0;

        for inrow := 0 to (inheight - 1) do begin
          for incol := 0 to (inwidth - 1) do begin
            pixel_colour := image_bitmap.Canvas.Pixels[incol, inrow];
            if pixel_colour <> clWhite then
              rotated_bitmap.Canvas.Pixels[inheight - 1 - inrow, incol] := calc_intensity(pixel_colour);
          end;//for
          pdf_form.row_progressbar.StepIt;

          pdf_form.row_label.Caption := 'processing... ' + IntToStr(inrow);  // 208b
          Application.ProcessMessages;

        end;//for

        rotated_picture.Graphic := rotated_bitmap;

      end;//with
    end;//with
  finally
    pdf_form.row_progressbar.Position := 0;
    Screen.Cursor := saved_screen_cursor;
  end;//try
end;
//________________________________________________________________________________________

{
        mark codes :-

           -5: label position.

           -4: timber selector mark.

        -3,-2: curving rad centres.

           -1: fixing peg centre.

            1: guide marks.

            2: radial ends.

            3: timber outlines.

         4,14: timber centre-lines.    14=solid for rivet centres.

            5: timber reduced ends.

            6: rail joint marks.

            7: transition and slewing marks.

            8: peg 1st arm.

            9: peg 2nd arm.

           10: plain track start marks.

33,55,93,95,233,293: selected for shoving timber outlines, reduced ends and infill.

        44,54: selected for shoving timber centre-lines.

          101: switch drive mark   // 223d

          203: timber infilling.

}
//__________________________________________________________________________________________

procedure Tpdf_form.diagram_mode_radiobuttonClick(Sender: TObject);

begin
  if diagram_mode_radiobutton.Checked then
    pad_form.output_diagram_mode_menu_entry.Click;
  show_output_mode_panel;
end;
//______________________________________________________________________________

procedure Tpdf_form.detail_mode_radiobuttonClick(Sender: TObject);

begin
  if detail_mode_radiobutton.Checked then
    pad_form.output_detail_mode_menu_entry.Click;
  show_output_mode_panel;
end;
//______________________________________________________________________________

procedure Tpdf_form.include_sketchboard_items_checkboxClick(Sender: TObject);   // 206e

begin
{ T3-OUT

  if include_sketchboard_items_checkbox.Checked=False then EXIT;

  if go_sketchboard=True    // sketchboard in use?
     then begin
            dtp_form.dtp_document.ZoomPage;   // needed to print items -- calc from rulers.
            update_model_rulers;

            if trackplan_exists=False
               then begin
                      alert(6,'php/560    no  sketchboard  trackplan',
                              'You have selected the option to include sketchboard items in the PDF file, but there is currently no trackplan item on the sketchboard.'
                             +'||Templot is unable to include sketchboard items in the PDF file if there is no trackplan reference available to set the scaled size.'
                             +'||To include items from the sketchboard in the PDF file, add a trackplan item to the sketchboard.',
                              '','','','','cancel','',0);
                      include_sketchboard_items_checkbox.Checked:=False;   // and untick it.
                    end;
          end
     else begin
            alert(6,'php/500    sketchboard  not  in  use',
                    'You have selected the option to include sketchboard items in the PDF file, but the sketchboard is not currently in use.'
                   +'||To start using the sketchboard click the `0sketchboard`1 button on the trackpad (top left).',
                    '','','','','cancel','',0);
            include_sketchboard_items_checkbox.Checked:=False;   // and untick it.
          end;
}

end;
//______________________________________________________________________________

end.
