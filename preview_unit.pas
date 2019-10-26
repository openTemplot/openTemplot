
(*

    This file is part of OpenTemplot, a computer program for the design of model railway track.
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
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

====================================================================================
*)

unit preview_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  pad_unit;  // needs to be here for Tpex

type
  Tpreview_form = class(TForm)
    Label1: TLabel;
    scaling_label: TLabel;
    print_button: TButton;
    font_button: TButton;
    Panel1: TPanel;
    continue_button: TButton;
    origin_label: TLabel;
    datestamp_label: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure print_buttonClick(Sender: TObject);
    procedure continue_buttonClick(Sender: TObject);
    procedure font_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  preview_form: Tpreview_form;

  //----------------------------
{
  to avoid confusion, terms "x" and "y" are not used when referring to the printer.

  "length" is measured down the page and along the rails (turnout calc x dims).
  "width" is measured across the page and across the tracks (turnout calc y dims).

  N.B. this means that printer-length corresponds to screen-width,
       and printer-width corresponds to screen-height.

       also "length" can be less than "width", e.g. for Landscape paper-orientation.
}

type                              // from origin in 1/100th mm to:
  Tsheet=record
    grid_top:extended;          //  grid means trim margin lines inside the sheet edges.
    grid_bottom:extended;
    grid_left:extended;
    grid_right:extended;
    //grid_mid:extended;

    empty:boolean;              //  True means there is nothing to print on this sheet.
  end;

const
  sheet_down_c=32;              //  max printout 33 x 26 sheets
  sheet_across_c=25;            //  gives approx 9000 x 5000 mm on A4 single sheets portrait.

var
  sheet:array[0..sheet_down_c, 0..sheet_across_c] of Tsheet;

  fit_single_sheet:boolean=False;
  staggered_pages:boolean=True;

                 // API printer info (defaults in case no printer) ...

  nom_width_dpi:extended=300;         // nominal dpi across width.
  nom_length_dpi:extended=300;        // nominal dpi down length.


  printer_width_indexmax_dots:integer=2399;    // printer width in dots -1 (index 0-).
  printer_length_indexmax_dots:integer=3349;   // ditto length.

  page_width:extended=18000;  // 180mm 0.93.a was 19420     // in 1/100 mm. between trim margins (actual printer),
  page_length:extended=26000; // 260mm 0.93.a was 27370     // these sizes are used for the page outlines on the pad view.

  page_width_out:extended=18000;  // 180mm 0.93.a was 19420 // and data size per page at the output scaling,
  page_length_out:extended=26000; // 260mm 0.93.a was 27370 // these sizes are used for the printer preview and printing.

  print_width:extended=20320;        // full sheet sizes printable area including margins in 1/100th mm.
  print_length:extended=28420;

  scal:extended;               // scale dots per 1/100th mm at 100% output.
  scaw:extended;

  scaw_out:extended;           // scale dots per 1/100th mm at the output scaling.
  scal_out:extended;

         // default trim margins.

  page_margin_top_mm:extended=6.0;     // 6 mm top trim margin.       mod 0.76.a was 6.5 mm
  page_margin_bottom_mm:extended=4.5;  // 4.5 mm bottom trim margin.  mod 0.76.a was 4 mm
  page_margin_left_mm:extended=7.0;    // 7 mm left trim margin.
  page_margin_right_mm:extended=2.0;   // 2 mm right trim margin.
  alignmarks_inner_mm:extended=3.0;    // 3 mm extent of alignment marks inside the trim margins.
  halfmm_mm:extended=0.5;              // 0.5 mm general spacing dimension.
  left_blanking_mm:extended=0;         // no left blanking.
  top_blanking_mm:extended=0;          // no top blanking.

  right_blanking_mm:extended=5000;     // no right blanking (default 5 metres page width).
  bottom_blanking_mm:extended=5000;    // no bottom blanking. (ditto).

  per_printer_margins:boolean=True;    // use above defaults.

  page_left_dots:integer=0;            // all get calculated before use...
  page_top_dots:integer=0;
  page_right_dots:integer=0;
  page_bottom_dots:integer=0;
  page_mid_dots:integer=0;
  page_quarter_dots:integer=0;
  page_3quarter_dots:integer=0;

  alignmarks_inner_dots:integer=0;
  halfmm_dots:integer=0;
  left_blanking_dots:integer=0;
  right_blanking_dots:integer=0;
  top_blanking_dots:integer=0;
  bottom_blanking_dots:integer=0;

  ypd:extended;

  sheet_co_wide:integer;          // no of sheets wide.
  sheet_co_long:integer;          // no of sheets long.

  print_pages_top_origin:extended=0;      // mm..
  print_pages_left_origin:extended=0;

  page_top_offset:extended=0;            // actual offset used in 1/100th mm.
  page_left_offset:extended=0;

  metafile_width:integer=24000;        // 0.93.a
  metafile_height:integer=12000;
  metafile_dpi:extended=600;

  track_bmp_width:integer=3900;        // 0.93.a
  track_bmp_height:integer=2700;
  track_bmp_dpi:extended=381;

  previous_print_width:integer=0;   // 1/100th mm..
  previous_print_length:integer=0;


  function page_info(pad,no_alerts,non_print_output:boolean; output_code:integer):boolean;  // get current printer info and calc page limits etc.
  function print_preview(no_alerts,non_print_output:boolean; output_code:integer):boolean;  // get current printer info and calc page limits etc.

  function get_fit_all_templates_size_mm(include_control,include_bgnd,group_only:boolean):Tpex;    // extracted 0.93.a
  function get_fit_all_shapes_size_mm(showing_trackpad_only,showing_output_only:boolean):Textents;                                                    // 0.93.a

  procedure print_index_sheet(pdf:boolean);                   // 0.91.d pdf  // print a preview of printed sheets as a page index.
  procedure outline_in_red(sheet_down,sheet_across:integer);  //  draw next page outline in red.

//________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses Printers, control_room, grid_unit, alert_unit, math_unit, calibration_unit,
  bgkeeps_unit, bgnd_unit, print_unit, info_unit, help_sheet,
  print_settings_unit, { OT-FIRST pdf_unit, dtp_unit, dtp_settings_unit,} export_unit;

var
  pvsx:extended=1;
  pvsy:extended=1;
  left_dots:integer=0;
  bottom_dots:integer=0;

  function crop_alert(size_str:string):boolean;forward;

//______________________________________________________________________________

function get_fit_all_shapes_size_mm(showing_trackpad_only,showing_output_only:boolean):Textents;

   // 0.93.a  calc a rectangle to contain all background shapes

var
  max_x,max_y:extended;
  min_x,min_y:extended;
  n:integer;

begin
  RESULT.max.x:=4*scale;   // init, 8ft scale square, centred on origin
  RESULT.max.y:=4*scale;

  RESULT.min.x:=0-4*scale;
  RESULT.min.y:=0-4*scale;

  max_x:=0-maxfp;
  max_y:=0-maxfp;

  min_x:=maxfp;
  min_y:=maxfp;

  if bgnd_form.bgnd_shapes_listbox.Items.Count<1 then EXIT;  // no shapes

  for n:=0 to (bgnd_form.bgnd_shapes_listbox.Items.Count-1) do begin

    with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[n]).bgnd_shape do begin  // next shape.

            //  0=line, 1=rectangle, 2=circle, 3=text, 4=target mark, -1=picture (bitmap image).

      if (showing_trackpad_only=True) and ((hide_bits AND $01)<>0) then CONTINUE;   // 214a  byte,  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both
      if (showing_output_only=True) and ((hide_bits AND $02)<>0) then CONTINUE;

      if min_x>p1.x then min_x:=p1.x;
      if min_y>p1.y then min_y:=p1.y;

      if max_x<p1.x then max_x:=p1.x;
      if max_y<p1.y then max_y:=p1.y;

      if shape_code<3   // p2 not valid for labels and target marks.
         then begin
                if min_x>p2.x then min_x:=p2.x;
                if min_y>p2.y then min_y:=p2.y;

                if max_x<p2.x then max_x:=p2.x;
                if max_y<p2.y then max_y:=p2.y;
              end;
    end;//with
  end;//next n

  RESULT.max.x:=max_x;  // add 4ft scale margins
  RESULT.max.y:=max_y;

  RESULT.min.x:=min_x;
  RESULT.min.y:=min_y;
end;
//______________________________________________________________________________

function get_fit_all_templates_size_mm(include_control,include_bgnd,group_only:boolean):Tpex;    // extracted 0.93.a

  // calc extents to fit all, from grid origin (neg ignored), in mm

var
  max_wide,max_long:extended;
  yd:extended;
  n:integer;

begin
  RESULT.x:=1;   // init, minimum 1mm x 1mm
  RESULT.y:=1;

  max_wide:=100;    // in 1/100th mm
  max_long:=100;

     // first do the control template...

  if (include_control=True) and (output_diagram_mode=False)    // control template not included in diagram mode
     then begin

            yd:=y_datum*100;    // (y_datum is in mm from left edge of sheet).

            if      xy_max[0]>max_long then max_long:=xy_max[0];
            if (yd+xy_max[1])>max_wide then max_wide:=yd+xy_max[1];
          end;

     // then add background templates...

  if (include_bgnd=True) and (keeps_list.Count>0)
     then begin

            for n:=0 to (keeps_list.Count-1) do begin

              if Ttemplate(keeps_list.Objects[n]).bg_copied=False then CONTINUE;  // no data, not on background.

              if (Ttemplate(keeps_list.Objects[n]).group_selected=False) and (group_only=True) then CONTINUE;  // not in group.

              with Ttemplate(keeps_list.Objects[n]).bgnd_keep do begin
                if xlist_max>max_long then max_long:=xlist_max;
                if ylist_max>max_wide then max_wide:=ylist_max;
              end;//with

            end;//next n
          end;

  RESULT.x:=max_long/100;     // return mm
  RESULT.y:=max_wide/100;
end;
//______________________________________________________________________________

function page_info(pad,no_alerts,{pdf}non_print_output:boolean; output_code:integer):boolean;  // get current printer info and calc page limits etc.

                 // 0.91.d   if non_print_output=True, output_code specifies:
                 // 0.91.d   0=PDF, 1=sketchboard image, 2=sketchboard metafile, 3=image file, 4=metafile

                 // if pad=True this is a call for the pad page outlines only - no alerts and don't need printing info.
                 // if no_alerts=True this is a re-paint of the preview page, or printing the page key - no alerts.

                 // if print_entire_pad_flag=True include background keeps in the sheet calcs.

                 // return True if all ok.

           // n.b. printer calibration does not come here for printer info, uses own routine.
const
     HORZRES         =   8;
     VERTRES         =  10;
     BITSPIXEL       =  12;
     RASTERCAPS      =  38;
     LOGPIXELSX      =  88;
     LOGPIXELSY      =  90;
var
  cur_cal:Tcal_data;

  prindex:integer;
  head_factor,roller_factor:extended;

  cal_wide_dpi:extended;    // adjusted for calibration...
  cal_long_dpi:extended;

  printer_width_dots, printer_length_dots:integer;

  now:integer;

  max_wide:extended;
  max_long:extended;

  max_extents:Tpex;  // mm  0.93.a

  banner_top_offset:extended;  // for banner paper.

  out_single_factor_wide:extended;
  out_single_factor_long:extended;

  sheet_down:integer;             // current sheet index.
  sheet_across:integer;           // ditto

  down:extended;
  across:extended;

  any_sheets:boolean;             // False = nothing to print.
  column_started:boolean;

  next_page_down:integer;

  aq:integer;

  bgndk:Tbgnd_keep;
  array_max:integer;

  i,n:integer;

  colour_depth_bits:integer;
  valid_printer_data:boolean;

begin
  RESULT:=False;                             // init error return.

  if non_print_output=True                  // 0.91.d
     then begin
            { OT-FIRST

            if output_code=0       // pdf file
               then begin
                      printer_width_dots:=pdf_width_dots;
                      printer_length_dots:=pdf_height_dots;

                      nom_width_dpi:=pdf_width_dpi;
                      nom_length_dpi:=pdf_height_dpi;
                    end;

            if output_code=1       // sketchboard track plan as bitmap
               then begin
                      printer_length_dots:=track_bmp_width;    // swap height/width
                      printer_width_dots:=track_bmp_height;

                      nom_width_dpi:=track_bmp_dpi;
                      nom_length_dpi:=nom_width_dpi;
                    end;

            if output_code=2       // sketchboard track plan as metafile
               then begin
                      printer_length_dots:=metafile_width;    // swap height/width
                      printer_width_dots:=metafile_height;

                      nom_width_dpi:=metafile_dpi;
                      nom_length_dpi:=nom_width_dpi;
                    end;
            }

            if output_code=3       // track plan as an image file
               then begin
                      printer_length_dots:=create_image_width_dots;    // swap height/width
                      printer_width_dots:=create_image_height_dots;

                      nom_width_dpi:=create_image_dpi;
                      nom_length_dpi:=nom_width_dpi;
                    end;

            if output_code=4       // track plan as EMF file
               then begin
                      printer_length_dots:=metafile_width;    // swap height/width
                      printer_width_dots:=metafile_height;

                      nom_width_dpi:=metafile_dpi;
                      nom_length_dpi:=nom_width_dpi;
                    end;

            { OT-FIRST if (output_code<0) or (output_code>4)}

            if (output_code<3) or (output_code>4)  // OT-FIRST
               then begin
                      valid_printer_data:=False;
                      EXIT;
                    end;

            prindex:=0;        // not used
            head_factor:=1.0;
            roller_factor:=1.0;

            cal_wide_dpi:=nom_width_dpi;
            cal_long_dpi:=nom_length_dpi;

            colour_depth_bits:=24;

            valid_printer_data:=True;
          end
     else begin

            if no_printer_available=True
               then begin
                      show_margins:=0;
                      pad_form.page_outlines_off_menu_entry.Checked:=True; // radio item.
                      pad_form.page_outlines_printer_menu_entry.Enabled:=False;
                      EXIT;
                    end;

                    // first get info about the current Windows printer...

            if get_prindex(prindex)=False then EXIT;         // get current printer index or none available.
                                                             // first check if calibrated...

            cur_cal:=Tprint_cal(printer_list.Objects[prindex]).cal_data;

            with cur_cal do begin
              if printer_calibrated=True
                 then begin
                        head_factor:=printer_head_factor;
                        roller_factor:=printer_roller_factor;
                      end
                 else begin                         // no calibration.
                        head_factor:=1.0;
                        roller_factor:=1.0;
                      end;
            end;//with

            try

              { OT-FIRST
              print_width_dots:=GetDeviceCaps(Printer.Handle, HORZRES);    // printer page-width in dots.
              printer_length_dots:=GetDeviceCaps(Printer.Handle, VERTRES);   // printer page-length in dots.

              nom_width_dpi:=GetDeviceCaps(Printer.Handle, LOGPIXELSX);  // dpi across width.
              nom_length_dpi:=GetDeviceCaps(Printer.Handle, LOGPIXELSY); // dpi down length.
              }

              printer_width_dots:=Printer.PageWidth;    // printer page-width in dots.    // OT-FIRST
              printer_length_dots:=Printer.PageHeight;  // printer page-length in dots.   // OT-FIRST

              nom_width_dpi:=Printer.XDPI;  // dpi across width.  // OT-FIRST
              nom_length_dpi:=Printer.YDPI; // dpi down length.   // OT-FIRST

              valid_printer_data:=True;

            except
              valid_printer_data:=False;
            end;//try

            if (valid_printer_data=False) or (nom_width_dpi<1) or (nom_length_dpi<1) or (printer_width_dots<1) or (printer_length_dots<1)   // division by zero, or negative.
               then begin
                      if pad=False then alert(0,'   printer  software  problem ..',
                                                '||Templot0 is unable to access your printer software.'
                                               +'||Please check your printer installation and printer driver.',
                                                '','','','','cancel  printing','',0);
                      EXIT;
                    end;

                              // do calibration...

            case Printer.Orientation of

                  poPortrait: begin
                                cal_wide_dpi:=nom_width_dpi/head_factor;      // adjust to actual dpi...
                                cal_long_dpi:=nom_length_dpi/roller_factor;
                              end;
                 poLandscape: begin
                                cal_wide_dpi:=nom_width_dpi/roller_factor;
                                cal_long_dpi:=nom_length_dpi/head_factor;
                              end;
                        else  begin
                                cal_wide_dpi:=nom_width_dpi;
                                cal_long_dpi:=nom_length_dpi;
                              end;
            end;//case



          end;//if non_print/print


          // and calculate print area dimensions...

  print_width:=printer_width_dots/cal_wide_dpi*2540;     //  in 1/100th mm.
  print_length:=printer_length_dots/cal_long_dpi*2540;

  if (non_print_output=True) and (output_code=0) then per_printer_margins:=True;  // added 205e PDF  use default trim margins.

  if (Round(print_width)<>previous_print_width) or (Round(print_length)<>previous_print_length) then per_printer_margins:=True;    // use default trim margins.

  previous_print_width:=Round(print_width);        // for check next time (nearest 1/100th mm) ...
  previous_print_length:=Round(print_length);

  printer_width_indexmax_dots:=printer_width_dots-1;    // printer width max index in dots -1 (index 0-).
  printer_length_indexmax_dots:=printer_length_dots-1;  // ditto length.

      // mods 208g to improve rounding between margins in PDF...

  if non_print_output=True
     then begin
            case output_code of

               { OT-FIRST
               0: begin                          // PDF

                    page_margin_top_mm:=6.0;     // 6 mm top trim margin.
                    page_margin_left_mm:=7.0;    // 7 mm left trim margin.

                    page_margin_bottom_mm:=4.5+print_length/100-pdf_height_mm;  // bottom margin 4.5mm, adjust for any rounding
                    page_margin_right_mm:=2.0+print_width/100-pdf_width_mm;     // right margin 2mm, adjust for any rounding

                    if page_margin_bottom_mm<1.0 then page_margin_bottom_mm:=1.0;
                    if page_margin_right_mm<1.0 then  page_margin_right_mm:=1.0;    // 1mm arbitrary.

                    left_blanking_mm:=0;                       // no left blanking.
                    top_blanking_mm:=0;                        // no top blanking.
                    right_blanking_mm:=print_width/100+5.0;    // no right blanking (+5 mm arbitrary).
                    bottom_blanking_mm:=print_length/100+5.0;  // no bottom blanking.
                  end;

             1,2: begin                       // sketchboard   image file, metafile
                    page_margin_top_mm:=0;    // 0.93.a  1-3-2009  no trim margins on sketchboard
                    page_margin_bottom_mm:=0;
                    page_margin_left_mm:=0;
                    page_margin_right_mm:=0;
                    left_blanking_mm:=0;
                    top_blanking_mm:=0;
                    right_blanking_mm:=dtp_form.dtp_document.PageHeight+5.0;  // no right blanking (+5 mm arbitrary).
                    bottom_blanking_mm:=dtp_form.dtp_document.PageWidth+5.0;  // no bottom blanking (+5 mm arbitrary).
                  end;
               }

               3: begin                       // create image file
                    page_margin_top_mm:=0;    // 0.93.a  28-7-2010  no trim margins on created image files
                    page_margin_bottom_mm:=0;
                    page_margin_left_mm:=0;
                    page_margin_right_mm:=0;
                    left_blanking_mm:=0;
                    top_blanking_mm:=0;
                    right_blanking_mm:=(create_image_height_dots/create_image_dpi)*25.4+5.0;  // no right blanking (+5 mm arbitrary).
                    bottom_blanking_mm:=(create_image_width_dots/create_image_dpi)*25.4+5.0;  // no bottom blanking (+5 mm arbitrary).
                  end;

               { OT-FIRST
               4: begin                       // create EMF file
                    page_margin_top_mm:=0;    // 0.93.a  28-7-2010  no trim margins on created EMF files
                    page_margin_bottom_mm:=0;
                    page_margin_left_mm:=0;
                    page_margin_right_mm:=0;
                    left_blanking_mm:=0;
                    top_blanking_mm:=0;
                    right_blanking_mm:=(metafile_height/metafile_dpi)*25.4+5.0;  // no right blanking (+5 mm arbitrary).
                    bottom_blanking_mm:=(metafile_width/metafile_dpi)*25.4+5.0;  // no bottom blanking (+5 mm arbitrary).
                  end;
                }

                  // OT-FIRST Temp same as 3: ...

              else begin                       // create image file
                     page_margin_top_mm:=0;    // 0.93.a  28-7-2010  no trim margins on created image files
                     page_margin_bottom_mm:=0;
                     page_margin_left_mm:=0;
                     page_margin_right_mm:=0;
                     left_blanking_mm:=0;
                     top_blanking_mm:=0;
                     right_blanking_mm:=(create_image_height_dots/create_image_dpi)*25.4+5.0;  // no right blanking (+5 mm arbitrary).
                     bottom_blanking_mm:=(create_image_width_dots/create_image_dpi)*25.4+5.0;  // no bottom blanking (+5 mm arbitrary).
                   end;

            end;//case

          end
     else begin                            // printer...

            if per_printer_margins=True    // use default trim margins
               then begin
                      page_margin_top_mm:=6.0;     // 6 mm top trim margin.      mod 0.76.a was 6.5 mm
                      page_margin_bottom_mm:=4.5;  // 4.5 mm bottom trim margin. mod 0.76.a was 4 mm
                      page_margin_left_mm:=7.0;    // 7 mm left trim margin.
                      page_margin_right_mm:=2.0;   // 2 mm right trim margin.
                      left_blanking_mm:=0;                   // no left blanking.
                      top_blanking_mm:=0;                    // no top blanking.
                      right_blanking_mm:=print_width/100+5.0;    // no right blanking (+5 mm arbitrary).
                      bottom_blanking_mm:=print_length/100+5.0;  // no bottom blanking.
                    end
               else begin
                      left_blanking_mm:=page_margin_left_mm-7.0;       // adjust left blanking.
                      if left_blanking_mm<0 then left_blanking_mm:=0;

                      right_blanking_mm:=print_width/100-page_margin_right_mm+2.0; // adjust right blanking (2mm arbitrary).
                      if right_blanking_mm<0 then right_blanking_mm:=0;

                      top_blanking_mm:=page_margin_top_mm-6.0;         // adjust top blanking.
                      if top_blanking_mm<0 then top_blanking_mm:=0;

                      bottom_blanking_mm:=print_length/100-page_margin_bottom_mm+4.5; // adjust right blanking (4.5mm arbitrary).
                      if bottom_blanking_mm<0 then bottom_blanking_mm:=0;
                    end;
          end;


  if (non_print_output=False) and (banner_paper=True)     // overrides for banner paper...
     then begin
            page_margin_top_mm:=0;
            page_margin_bottom_mm:=0;
            top_blanking_mm:=0;
          end;

  page_width:=print_width-(page_margin_left_mm+page_margin_right_mm)*100;     // width per page between margins in 1/100th mm.
  page_length:=print_length-(page_margin_top_mm+page_margin_bottom_mm)*100;   // length ditto.


             // now do calcs for any output scaling, all dims in 1/100mm (floats)...

  ypd:=y_datum*100;    // (y_datum is in mm from left edge of sheet).
                       // (this is also needed for the printing).


  max_wide:=0;  // init
  max_long:=0;

  max_extents:=get_fit_all_templates_size_mm(True,print_entire_pad_flag,print_group_only_flag);

  max_long:=max_extents.x*100;     // extents in mm
  max_wide:=max_extents.y*100;

  if out_factor<minfp then out_factor:=1;       // can't go that small!

  if (non_print_output=False) and (banner_paper=True)
     then banner_top_offset:=2500   // banners start 25 mm from top of first page.
     else banner_top_offset:=0;     // single sheets all start at the top.

  page_top_offset:=print_pages_top_origin*100;        // 1/100th mm.
  page_left_offset:=print_pages_left_origin*100;

  if fit_single_sheet=True      // he wants it fitted to a single sheet (set out_factor)...
     then begin
            try
              out_single_factor_wide:=page_width/(max_wide-page_left_offset);
              out_single_factor_long:=(page_length-banner_top_offset)/(max_long-page_top_offset);  // banner first page has 25 mm top margin.
            except
              out_single_factor_wide:=out_factor/10;   // arbitrary size if calcs fail.
              out_single_factor_long:=out_factor/10;
            end;//try

            if out_single_factor_wide<out_single_factor_long    // set maximum factor to fit both ways...
               then out_factor:=out_single_factor_wide*0.98     // allow 2% right margin space
               else out_factor:=out_single_factor_long*0.98;

            if out_factor>1.0 then out_factor:=1.0;             // but don't enlarge it to fit page,
                                                                // he can do that manually if he wants (otherwise confusing).
          end;

  if pad=True                  //  return the page length / width for the screen grid.
     then begin
            RESULT:=True;      // and the output factor - 0.93.a moved here
            EXIT;
          end;


  page_top_offset:=page_top_offset-banner_top_offset/out_factor;   // 1/100th mm.  (banner offset needs to be actual, so the divide here cancels subsequent scaling).

  page_width_out:=page_width/out_factor;      // data sizes on the page at the output scaling.
  page_length_out:=page_length/out_factor;    // ditto.

  scal:=cal_long_dpi/2540;       //  length scaling factor in dots per 1/100th mm. Used for trim margins, etc.
  scaw:=cal_wide_dpi/2540;       //  ditto width scaling factor (+ve for the printer).

  scaw_out:=scaw*out_factor;     // dots per 1/100th mm. (at required output scaling). Used for grid and template.
  scal_out:=scal*out_factor;     // ditto.

  page_left_dots:=Round(page_margin_left_mm*100*scaw);     // all in dots ...
  page_top_dots:=Round(page_margin_top_mm*100*scal);
  page_right_dots:=page_left_dots+Round(page_width*scaw);
  page_bottom_dots:=page_top_dots+Round(page_length*scal);

  page_mid_dots:=Round((page_margin_top_mm*100+(page_length/2))*scal);       // for trim align marks ...
  page_quarter_dots:=Round((page_margin_top_mm*100+(page_length/4))*scal);
  page_3quarter_dots:=Round((page_margin_top_mm*100+(page_length*3/4))*scal);

  alignmarks_inner_dots:=Round(alignmarks_inner_mm*100*scaw);
  halfmm_dots:=Round(halfmm_mm*100*scaw);

  left_blanking_dots:=Round(left_blanking_mm*100*scaw);
  right_blanking_dots:=Round(right_blanking_mm*100*scaw);

  top_blanking_dots:=Round(top_blanking_mm*100*scal);
  bottom_blanking_dots:=Round(bottom_blanking_mm*100*scal);

  sheet_co_wide:=Trunc((max_wide-page_left_offset)/page_width_out);    // max sheet index (no of sheets -1) across (calc from left page margin).
  sheet_co_long:=Trunc((max_long-page_top_offset)/page_length_out);

  if (sheet_co_wide>0) and (staggered_pages=True) then sheet_co_long:=sheet_co_long+1;   // Add 1 sheet to allow for staggered joins.

  if sheet_co_wide>sheet_across_c
     then begin
            sheet_co_wide:=sheet_across_c;
            if no_alerts=False
               then if crop_alert('width')=True then EXIT;
          end;

  if sheet_co_long>sheet_down_c
     then begin
            sheet_co_long:=sheet_down_c;
            want_bottom_margin:=True;
            if no_alerts=False
               then if crop_alert('length')=True then EXIT;
          end
     else want_bottom_margin:=False;

  for sheet_down:=0 to sheet_down_c do begin              // every sheet (all 33*26 = 858 sheets).
    for sheet_across:=0 to sheet_across_c do begin

      sheet[sheet_down,sheet_across].empty:=True;        //  init for scanning data.
    end;//next across
  end;//next down

  any_sheets:=False;                                     //  init check for anything to print.

  for sheet_across:=0 to sheet_co_wide do begin
    for sheet_down:=0 to sheet_co_long do begin            //  only the sheets we need.

      with sheet[sheet_down,sheet_across] do begin

                   // first calc all sheet margins and joins ...  (all in 1/100th mm at the output factor)

        grid_top:=(sheet_down*page_length_out)+page_top_offset;                // data offset to the top of this page (1/100th mm) (25 mm start offset for banner printing).

        if (sheet_across mod 2 <>0) and (staggered_pages=True) then grid_top:=grid_top-page_length_out/2;   // stagger odd columns of sheets.

        grid_bottom:=grid_top+page_length_out;

        grid_left:=(sheet_across*page_width_out)+page_left_offset;
        grid_right:=grid_left+page_width_out;

                // then calc all sheet edges (for data overlaps) ...

                //  scan data to see if anything on this page ...

                // first check the control template...

        if  (print_entire_pad_flag=False) // control template
        and (output_diagram_mode=False)   // 0.93.a  no control template if diagram mode
        and (turnoutx>0)                  // not if invalidated

           then begin
                  for aq:=0 to aq_max_c do begin
                      if ( (plain_track=False) or (aq=0) or (aq=8) or (aq=3) or (aq=11) or ((aq>15) and (aq<25)) ) and ( aqyn[aq]=True )
                                          // stock rails and adjacent track only if plain track, and data available ?
                         then begin
                                for now:=0 to nlmax_array[aq] do
                                  begin
                                    across:=(outoflist(aq,now,1)+ypd);
                                    down:=outoflist(aq,now,0);

                                    if (across>grid_left) and (across<grid_right) and (down>grid_top) and (down<grid_bottom)
                                       then begin
                                              empty:=False;          //  something on this sheet in the control template.
                                              any_sheets:=True;      //  there is a template to print.
                                            end;
                                  end;
                              end;
                  end;//for next aq
                end;// if the control

                //  now check the background templates...

        if (print_entire_pad_flag=True) and (keeps_list.Count>0) { and (pad_form.print_keeps_on_menu_entry.Checked=True) }
           then begin

                  for n:=0 to (keeps_list.Count-1) do begin

                    if Ttemplate(keeps_list.Objects[n]).bg_copied=False then CONTINUE;  // no data, not on background.

                    if (Ttemplate(keeps_list.Objects[n]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

                    bgndk:=Ttemplate(keeps_list.Objects[n]).bgnd_keep;

                    with bgndk do begin
                      for aq:=0 to aq_max_c do begin

                        array_max:=intarray_max(list_bgnd_rails[aq,0]);
                        if array_max=0 then CONTINUE;                      // empty rail.

                        for now:=0 to array_max do begin
                          across:=intarray_get(list_bgnd_rails[aq,1],now);    // y datum already included in data.
                          down:=intarray_get(list_bgnd_rails[aq,0],now);

                          if (across>grid_left) and (across<grid_right) and (down>grid_top) and (down<grid_bottom)
                              then begin
                                     empty:=False;          //  something on this sheet in the background.
                                     any_sheets:=True;      //  there is a something to print.
                                   end;

                        end;//next now
                      end;//next aq

                    end;//with bgnd_keep

                  end;//for-next bgnd keep.
                end;//if any bgnd

      end;//with sheet
    end;//next down
  end;//next across

  if any_sheets=False
     then begin
            if no_alerts=True
               then EXIT
               else begin
                      if print_entire_pad_flag=False     // mods 0.93.a
                         then begin
                                alert(6,'  no  data  for  print  control  template  preview',
                                 'Print control template :  the printing area is empty.'
                                +'||Have you shifted the control template completely below or to the left of the page origin?'
                                +' Or moved it beyond the printing area of '+IntToStr(sheet_down_c+1)+' pages long by '+IntToStr(sheet_across_c+1)+' pages wide?'
                                +' Or switched off the rails and the track centre-lines on the generator?',
                                 '','','','','cancel  printing','',0);


                                EXIT;
                              end
                         else begin
                                alert(6,'  no  data  for  print  preview',
                                           'Print entire trackpad or group :  the printing area is empty.'
                                          +'||Have you cleared or wiped all background templates while the control template is being omitted or is completely below or to the left of the page origin?'
                                          +'||Or moved the entire drawing beyond the printing area of '+IntToStr(sheet_down_c+1)+' pages long by '+IntToStr(sheet_across_c+1)+' pages wide?'
                                          +'||Or switched off the rails and the track centre-lines on the generator for the control template and all of the background templates?',
                                           '','','','','cancel  printing','',0);
                                EXIT;
                              end;
                    end;
          end;

  if (non_print_output=False) and (banner_paper=True) and (print_form.banner_fill_checkbox.Checked=True)
     then begin
            for sheet_across:=0 to sheet_co_wide do begin    // check for any column infills needed...

              column_started:=False;                         // not up to first non-empty page yet.

              for sheet_down:=0 to sheet_co_long do begin

                if sheet[sheet_down,sheet_across].empty=False
                   then column_started:=True
                   else begin
                          if (column_started=True) and (sheet_down<sheet_co_long) // blank page and the column has previously started and there are more pages in the column...
                             then begin
                                    for next_page_down:=(sheet_down+1) to sheet_co_long do begin
                                      if sheet[next_page_down,sheet_across].empty=False then sheet[sheet_down,sheet_across].empty:=False;  // we need to infill this one.
                                    end;
                                  end;
                        end;
              end;//for sheet_down
            end;//for sheet_across
          end;

  RESULT:=True;
end;
//_________________________________________________________________________________________

procedure Tpreview_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key=VK_PAUSE then Application.Minimize;    //  hide TEMPLOT on PAUSE key.

  if Key=VK_F10
     then begin
            Key:=0;                    //  otherwise selects the menus.
          end;

  if Key=VK_F11
     then begin
            Key:=0;
            print_button.Click;
          end;

  if Key=VK_F12
     then begin
            Key:=0;
            pad_form.redraw_menu_entry.Click;     // unlock the redraw if locked.
            Close;
          end;

end;
//_________________________________________________________________________________________

procedure preview_sketchboard_items(on_canvas:TCanvas);    // 0.93.a  add sketchboard items

var
  dtp_rect:TRect;
  dtp_width,dtp_height:extended;

  p1,p2:Tpex;

  x1,y1,x2,y2:extended;

  sb_saved_page_colour:TColor;

begin

  EXIT;   // OT-FIRST

  { OT-FIRST

  if go_sketchboard=False then EXIT;  // 205e   sketchboard not in use

  if print_settings_form.output_sketchboard_items_checkbox.Checked=False then EXIT;  // output not wanted

  if pad_form.preview_sketchboard_items_menu_entry.Checked=False then EXIT;  // preview not wanted

  if dtp_form.Active=True then EXIT;

  sb_saved_page_colour:=dtp_form.dtp_document.CurrentPage.PageColor;  // save for restore

  dtp_form.dtp_document.CurrentPage.PageColor:=paper_colour;  // match background

  dtp_form.dtp_document.ZoomPage;   // needed to print items -- fresh calc from rulers.

  update_model_rulers;

  if trackplan_exists=False then EXIT; // can't scale the items without a trackplan item

  if stretch_factor_wide<minfp then EXIT;    // no division by zero
  if stretch_factor_high<minfp then EXIT;

  if dtp_form.dtp_document.CurrentPage.PageWidth<minfp then EXIT;

  omit_trackplan_from_rendering:= NOT dtp_settings_form.render_trackplan_in_output_checkbox.Checked;   // to omit sketchboard trackplans from output

  copying_sb_to_pad:=True;       //      flag for dtpPage.Print
  copying_sb_to_printer:=False;  // 206e flag for dtpPage.Print
  copying_sb_to_pdf:=False;      // 206e flag for dtpPage.Print

  dtp_width:=dtp_form.dtp_document.CurrentPage.PageWidth/stretch_factor_wide;
  dtp_height:=dtp_form.dtp_document.CurrentPage.PageHeight/stretch_factor_high;

        // use p1,p2 as for background shapes...

  p1.x:=model_ruler_x_offset;
  p2.x:=p1.x+dtp_width;

  p1.y:=0-model_ruler_y_offset-dtp_height;
  p2.y:=p1.y+dtp_height;

  x1:=(p1.x*100+re_org_x)*pvsx;
  y1:=(p1.y*100+re_org_y)*pvsy;
  x2:=(p2.x*100+re_org_x)*pvsx;
  y2:=(p2.y*100+re_org_y)*pvsy;

  dtp_rect.Left:=Round(x1)+left_dots;
  dtp_rect.Bottom:=bottom_dots-Round(y1);

  dtp_rect.Right:=Round(x2)+left_dots;
  dtp_rect.Top:=bottom_dots-Round(y2);

       // draw it..

  dtp_form.dtp_document.CurrentPage.Print(on_canvas,dtp_rect,0,0,False,False);

       // restore...

  omit_trackplan_from_rendering:=False;    // restore for sketchboard

  copying_sb_to_pad:=False;  // 206e flag for dtpPage.Print

  dtp_form.dtp_document.CurrentPage.PageColor:=sb_saved_page_colour;

       // do a hatched rectangle of full sketchboard area (this may be larger than the trackplan item)...
       // must be done after the sketchboard items, otherwise overwritten by the sketchboard page colour...

  on_canvas.Pen.Width:=1;
  on_canvas.Pen.style:=psSolid;
  on_canvas.Pen.Color:=grid_colour;  //$00E0E0E0;

  on_canvas.Brush.Style:=bsBDiagonal;
  on_canvas.Brush.Color:=grid_colour; //$00E0E0E0;

  on_canvas.Rectangle(dtp_rect);

       // restore...

  on_canvas.Pen.Width:=1;
  on_canvas.Pen.style:=psSolid;
  on_canvas.Pen.Color:=clBlack;

  on_canvas.Brush.Style:=bsSolid;
  on_canvas.Brush.Color:=clWhite;
  }

end;
//______________________________________________________________________________

function print_preview(no_alerts,{pdf}non_print_output:boolean; output_code:integer):boolean;  // get current printer info and calc page limits etc.

    // 0.91.d   if non_print_output=True, output_code specifies:
    // 0.91.d   0=PDF, 1=sketchboard, 2=metafile, 3=image file  //pdf:boolean):boolean;    // 0.91.d pdf draw screen preview of printed sheets.

    // 0.93.a   0=PDF, 1=sketchbard bitmap, 2=sketchbook metafile, 3=create image file, 4=create EMF file

var
  psx:integer;

  sheet_down:integer;             // current sheet index.
  sheet_across:integer;           // ditto

  xmax, ymax:integer;

  aq, now:integer;
  w_dots1, l_dots1, w_dots2, l_dots2:integer;

  move_to, line_to :TPoint;
  page_length_pixels:integer;

  text_X:integer;
  page_str,page_prefix_str:string;

  i,n:integer;
  bgs_count:integer;

  bgk,array_max:integer;
  now_keep:Tbgnd_keep;
  xint,yint:integer;

  prev_canvas:TCanvas;

  something_drawn:boolean;

  font_height, font_size:extended;
  now_shape:Tbgnd_shape;
  x1,y1,x2,y2:extended;

  arm,diamond:extended;

  raster_rect:TRect;

  dummy_i:integer;

		  ////////////////////////////////////////////////////////////////////////

                  procedure mark_end(aq1, aq1end, aq2, aq2end:integer; pen_solid:boolean);      // make the mark

                  var
                    p1, p2:TPoint;

                  begin

                    if (endmarks_yn[aq1,aq1end]=True) and (endmarks_yn[aq2,aq2end]=True)
                       then begin
                              p1:=endmarks[aq1,aq1end];
                              p2:=endmarks[aq2,aq2end];

                              with prev_canvas do begin

                                Pen.Width:=1;

                                Brush.Color:=paper_colour;  // gaps in dotted lines.
                                Brush.Style:=bsSolid;

                                TextOut(0,0,'');

                                if pen_solid=True then Pen.Style:=psSolid   // 0.93.a mods for platforms
                                                  else Pen.Style:=psDot;

                                move_to.X:=Round(p1.X*pvsx)+left_dots;  move_to.Y:=bottom_dots-Round((p1.Y+ypd)*pvsy);
                                line_to.X:=Round(p2.X*pvsx)+left_dots;  line_to.Y:=bottom_dots-Round((p2.Y+ypd)*pvsy);
                                if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                              end;//with
                            end;
                  end;
                  /////////////////////////////////////////////////////////////////////////
begin
  RESULT:=False;                   // init error result.
  try
    Screen.Cursor:=crHourglass;

    if calcs_done_and_valid=False then redraw(False);    //  first do a direct redraw if nec to ensure valid calcs.
    if calcs_done_and_valid=False then EXIT;             //  calcs still not valid.

    if page_info(False,no_alerts,non_print_output,output_code)=False     //  read the printer details, get sheet counts and page sizes.
       then EXIT;                                                        //  printer driver problem.

    with pad_form do begin

      xmax:=ClientWidth-10;
      ymax:=ClientHeight-10;

    end;//with

    prev_canvas:=offdraw_bmp.Canvas;     // or the off-screen draw bitmap.

    with prev_canvas do begin

      wipe_draw_bmp(False);  // False = don't add trackpad sketchboard items

               // calc preview scaling to show all the pages...

      pvsx:=(xmax-10)/((sheet_co_long+1)*page_length_out);  // x screen pixels per 1/100th mm (scaled), to fit all pages.
      pvsy:=(ymax-10)/((sheet_co_wide+1)*page_width_out);   // y ditto.

      if pvsx>pvsy then pvsx:=pvsy;            // make sure scales are equal.
      if pvsy>pvsx then pvsy:=pvsx;
                                               // set left origin...

      left_dots:=10;                      // init for no staggered ends needed.

      if staggered_pages=True
         then begin
                for n:=0 to sheet_co_wide do begin  // search for any non-empty staggered column first pages.
                  if (n mod 2)=0 then CONTINUE;     // not even columns.
                  if sheet[0,n].empty=False
                     then begin
                            left_dots:=left_dots+Round((page_length_out*pvsx)/2);   // offset to include all staggered sheets.
                            BREAK;
                          end;
                end;//for
              end;

      left_dots:=left_dots-Round(page_top_offset*pvsx);
      bottom_dots:=ymax+Round(page_left_offset*pvsy);

             // sketchboard items first?...

      if bgnd_form.output_shapes_in_front_of_sb_checkbox.Checked=True
         then preview_sketchboard_items(prev_canvas);  // 206e

            // now draw any background shapes (ignoring page outlines and empty pages)...

      bgs_count:=bgnd_form.bgnd_shapes_listbox.Items.Count;

      if  (pad_form.preview_background_shapes_menu_entry.Checked=True) // 206e
      and (print_settings_form.output_bgnd_shapes_checkbox.Checked=True)
      and (bgs_count>0)
         then begin
                Font.Assign(shapes_label_font);      // for labels.

                font_height:=Font.Size*25.4/72;      // in mm
                font_height:=font_height*pvsx*100;   // in pixels scaled to pad.

                font_size:=font_height/Screen.PixelsPerInch*72;   // point size scaled to pad

                font_size:=limits(1,Font.Size*2,font_size,dummy_i);       // limit font size on screen.

                Font.Size:=Round(font_size);   // font scaled down to the pad (but not up more than double).

                Pen.Width:=1;
                Pen.Mode:=pmCopy;
                Pen.Color:=shapes_colour;

                for i:=0 to (bgs_count-1) do begin

                  try
                    Pen.Style:=psSolid;

                    now_shape:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape;     // next shape.

                    with now_shape do begin

                      if (hide_bits AND $02)<>0 then CONTINUE;   // shape hidden for output

                      if shape_code<>4    // not a target mark
                         then begin
                                case shape_style of
                                          0,1,3: begin
                                                   Brush.Color:=paper_colour;
                                                   Brush.Style:=bsSolid;      // blank or tranaparent.
                                                 end;
                                              2: begin
                                                   Brush.Color:=Pen.Color;
                                                   Brush.Style:=bsDiagCross;  // cross-hatched.

                                                   TextOut(0,0,'');      // !!! Delphi bug? This seems to be necessary before dotted lines will draw properly.
                                                                         // TextOut obviously initialises some background mask property which I have been unable
                                                                         // to find or set any other way.

                                                   if shape_code=0 then Pen.Style:=psDot;  // dashed line.

                                                 end;
                                end;//case

                                x1:=(p1.x*100+re_org_x)*pvsx;
                                y1:=(p1.y*100+re_org_y)*pvsy;
                                x2:=(p2.x*100+re_org_x)*pvsx;
                                y2:=(p2.y*100+re_org_y)*pvsy;

                                move_to.X:=Round(x1)+left_dots;
                                move_to.Y:=bottom_dots-Round(y1);

                                if shape_code=3      // label rectangle..
                                   then begin
                                          line_to.X:=move_to.X+TextWidth(' '+shape_name+' ')+3;
                                          line_to.Y:=move_to.Y+TextHeight(shape_name)+3;
                                        end
                                   else begin
                                          line_to.X:=Round(x2)+left_dots;
                                          line_to.Y:=bottom_dots-Round(y2);
                                        end;

                                if check_limits(move_to, line_to)=True
                                   then begin
                                          case shape_code of

                                                -1: begin     // picture = bitmap image.

                                                      raster_rect.Left:=move_to.X;
                                                      raster_rect.Bottom:=move_to.Y;
                                                      raster_rect.Right:=line_to.X;
                                                      raster_rect.Top:=line_to.Y;

                                                      try
                                                        { OT-FIRST
                                                        if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile=True
                                                           then begin
                                                                  pad_form.bgnd_shape_image.Picture.Graphic:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_metafile;
                                                                  CopyMode:=cmSrcCopy;  // normal
                                                                end
                                                           else begin}
                                                                  pad_form.bgnd_shape_image.Picture.Graphic:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap;
                                                                  if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.show_transparent=True  // 0.93.a moved into file
                                                                     then CopyMode:=cmSrcAnd    // (destination Canvas) transparent if on white background.
                                                                     else CopyMode:=cmSrcCopy;  // reset normal for destination Canvas.
                                                                { OT-FIRST end;}

                                                        StretchDraw(raster_rect,pad_form.bgnd_shape_image.Picture.Graphic);   // needs TGraphic parameter to work reliably.

                                                        CopyMode:=cmSrcCopy;   // reset normal for destination Canvas.

                                                        if print_form.picture_borders_checkbox.Checked=True
                                                           then begin
                                                                  Brush.Color:=paper_colour;
                                                                  Brush.Style:=bsClear;
                                                                  Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                                end;

                                                      except
                                                        CopyMode:=cmSrcCopy;       // reset normal for destination Canvas.

                                                        Brush.Color:=Pen.Color;    // stretch failed - draw hatched outline.
                                                        Brush.Style:=bsBDiagonal;
                                                        Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                      end;//try

                                                    end;
                                                 0: begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                               1,3: Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                 2: Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                          end;//case

                                          if shape_code=3 then TextOut(move_to.X+1,move_to.Y+1,' '+shape_name);   // insert label text in rectangle box.
                                        end;
                              end
                         else begin    // shape_code=4, draw a target mark

                                arm:=p2.x;        // cross arm length.
                                diamond:=arm/2;   // size of centre diamond.

                                x1:=((p1.x-arm)*100+re_org_x)*pvsx;  // p2.x is the arm length each way.
                                y1:=(p1.y*100+re_org_y)*pvsy;         // horizontal line.

                                x2:=((p1.x+arm)*100+re_org_x)*pvsx;
                                y2:=y1;

                                move_to.X:=Round(x1)+left_dots;
                                move_to.Y:=bottom_dots-Round(y1);

                                line_to.X:=Round(x2)+left_dots;
                                line_to.Y:=bottom_dots-Round(y2);


                                if check_limits(move_to, line_to)=True
                                   then begin
                                          MoveTo(move_to.X, move_to.Y);
                                          LineTo(line_to.X, line_to.Y);   // draw horizontal line.
                                        end;

                                x1:=(p1.x*100+re_org_x)*pvsx;
                                y1:=((p1.y-arm)*100+re_org_y)*pvsy;

                                x2:=x1;                                   // vertical line.
                                y2:=((p1.y+arm)*100+re_org_y)*pvsy;

                                move_to.X:=Round(x1)+left_dots;
                                move_to.Y:=bottom_dots-Round(y1);

                                line_to.X:=Round(x2)+left_dots;
                                line_to.Y:=bottom_dots-Round(y2);


                                if check_limits(move_to, line_to)=True
                                   then begin
                                          MoveTo(move_to.X, move_to.Y);
                                          LineTo(line_to.X, line_to.Y);   // draw vertical line.
                                        end;

                                   // now do 4 diamond lines...

                                x1:=((p1.x-diamond)*100+re_org_x)*pvsx;     // NW line.
                                y1:=(p1.y*100+re_org_y)*pvsy;

                                x2:=(p1.x*100+re_org_x)*pvsx;
                                y2:=((p1.y+diamond)*100+re_org_y)*pvsy;

                                move_to.X:=Round(x1)+left_dots;
                                move_to.Y:=bottom_dots-Round(y1);

                                line_to.X:=Round(x2)+left_dots;
                                line_to.Y:=bottom_dots-Round(y2);


                                if check_limits(move_to, line_to)=True
                                   then begin
                                          MoveTo(move_to.X, move_to.Y);
                                          LineTo(line_to.X, line_to.Y);
                                        end;

                                move_to:=line_to;      // NE line.

                                x2:=((p1.x+diamond)*100+re_org_x)*pvsx;
                                y2:=(p1.y*100+re_org_y)*pvsy;

                                line_to.X:=Round(x2)+left_dots;
                                line_to.Y:=bottom_dots-Round(y2);

                                if check_limits(move_to, line_to)=True
                                   then begin
                                          MoveTo(move_to.X, move_to.Y);
                                          LineTo(line_to.X, line_to.Y);
                                        end;

                                move_to:=line_to;      // SE line.

                                x2:=(p1.x*100+re_org_x)*pvsx;
                                y2:=((p1.y-diamond)*100+re_org_y)*pvsy;

                                line_to.X:=Round(x2)+left_dots;
                                line_to.Y:=bottom_dots-Round(y2);

                                if check_limits(move_to, line_to)=True
                                   then begin
                                          MoveTo(move_to.X, move_to.Y);
                                          LineTo(line_to.X, line_to.Y);
                                        end;

                                move_to:=line_to;      // SW line.

                                x2:=((p1.x-diamond)*100+re_org_x)*pvsx;
                                y2:=(p1.y*100+re_org_y)*pvsy;

                                line_to.X:=Round(x2)+left_dots;
                                line_to.Y:=bottom_dots-Round(y2);

                                if check_limits(move_to, line_to)=True
                                   then begin
                                          MoveTo(move_to.X, move_to.Y);
                                          LineTo(line_to.X, line_to.Y);
                                        end;
                              end;

                    end;//with now_shape

                  except
                    CONTINUE;      // ignore this shape if any calc errors.
                  end;//try

                end;//for next i shape

              end;//if draw shapes
            //---------------------------------

            // sketchboard items now?...

      if bgnd_form.output_shapes_in_front_of_sb_checkbox.Checked=False
         then preview_sketchboard_items(prev_canvas);  // 206e

            //  draw background templates first (so current turnout overdraws)...

      if (print_entire_pad_flag=True) and (keeps_list.Count>0)
         then begin

                Pen.Color:=bgkeep_rail_colour;         // use a different colour.
                Pen.Mode:=pmCopy;

                Pen.Width:=1;

                Brush.Color:=paper_colour;  // gaps in dotted lines.
                Brush.Style:=bsSolid;

                TextOut(0,0,'');  // needed for dotted lines - Delphi bug?

                for bgk:=0 to (keeps_list.Count-1) do begin  // next background keep.

                  if Ttemplate(keeps_list.Objects[bgk]).bg_copied=False then CONTINUE;  // no data, not on background.

                  if (Ttemplate(keeps_list.Objects[bgk]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

                  now_keep:=Ttemplate(keeps_list.Objects[bgk]).bgnd_keep;

                  with Ttemplate(keeps_list.Objects[bgk]).template_info.keep_dims.box_dims1 do begin

                    with platform_trackbed_info do begin

                      with now_keep do begin

                        something_drawn:=False;   // nothing drawn yet for this keep.

                        for aq:=0 to aq_max_c do begin

                          if pad_form.both_edges_menu_entry.Checked=True     // if drawing both edges, ignore outer edges..
                             then begin
                                    if (aq>7) and (aq<16) then CONTINUE;                        // gauge faces only.
                                    // 0.93.a now used for platforms and trackbed edges    if (aq=17) or (aq=19) or (aq=21) or (aq=23) then CONTINUE;  // ditto for adjacent tracks.
                                    if (aq=27) or (aq=29) then CONTINUE;                        // ditto for K-crossing check rails.
                                  end;

                          Pen.Style:=psSolid;               // init

                          if adjacent_edges_keep=True  // platforms and trackbed edges
                             then begin
                                    if (aq=18) or (aq=19) or (aq=22) or (aq=23) then Pen.Style:=psDot;  // trackbed edges

                                    if (aq=16) and (draw_ts_platform_rear_edge_keep=False) then Pen.Style:=psDot;  // draw solid or dotted
                                    if (aq=20) and (draw_ms_platform_rear_edge_keep=False) then Pen.Style:=psDot;
                                  end;

                          if (align_info.cl_only_flag=False) and (something_drawn=True) and ( (aq=24) or (aq=25) ) then CONTINUE;  // don't draw centre-lines unless there is nothing else. (cl-only templates).

                          if (aq=24) or (aq=25)   // 212a
                             then begin
                                    if align_info.dummy_template_flag=True then Pen.Color:=shapes_colour  // 212a
                                                                           else Pen.Color:=guide_colour;  // centre-lines
                                  end
                             else Pen.Color:=bgkeep_rail_colour;

                          if (aq=16) or (aq=17) or (aq=20) or (aq=21) then Pen.Color:=bgkeep_platform_colour;  // platforn front and rear edges.

                                    // (platform ends not shown on print preview)

                          array_max:=intarray_max(list_bgnd_rails[aq,0]);
                          if array_max=0 then CONTINUE;                        // empty rail.

                          xint:=intarray_get(list_bgnd_rails[aq,0],0);
                          yint:=intarray_get(list_bgnd_rails[aq,1],0);

                          w_dots1:=Round(yint*pvsy);
                          l_dots1:=Round(xint*pvsx);
                          move_to.X:=l_dots1+left_dots; move_to.Y:=bottom_dots-w_dots1;

                          for now:=1 to array_max do
                            begin
                              xint:=intarray_get(list_bgnd_rails[aq,0],now);
                              yint:=intarray_get(list_bgnd_rails[aq,1],now);

                              w_dots2:=Round(yint*pvsy);
                              l_dots2:=Round(xint*pvsx);
                              line_to.X:=l_dots2+left_dots; line_to.Y:=bottom_dots-w_dots2;
                              //with pad_form.Canvas do begin
                                if check_limits(move_to, line_to)=True
                                   then begin
                                          MoveTo(move_to.X, move_to.Y);
                                          LineTo(line_to.X, line_to.Y);
                                          if aq<>24 then something_drawn:=True;   // (so we can also draw aq=25 if we draw aq=24)
                                        end;
                              //end;//with
                              move_to:=line_to;
                            end;//for next now
                        end;//for next aq

                      end;//with now_keep
                    end;//with trackbed info
                  end;//with template info

                end;//for next bgnd_keep
              end;//if bgnd keeps

          //  draw turnout rails...     (once only).


      if  (print_entire_pad_flag=False) // control template
      and (output_diagram_mode=False)   // 0.93.a  no control template if diagram mode
      and (turnoutx>0)                  // not if invalidated

         then begin

                Pen.Mode:=pmCopy;

                Pen.Width:=1;
                Pen.Style:=psSolid;

                Brush.Color:=paper_colour;  // gaps in dotted lines.
                Brush.Style:=bsSolid;

                TextOut(0,0,'');  // needed for dotted lines - Delphi bug?

                something_drawn:=False;   // nothing drawn yet for control template.

                for aq:=0 to aq_max_c do
                  begin

                    if (cl_only=False) and (something_drawn=True) and ((aq=24) or (aq=25)) then CONTINUE;  // 0.93.a centre-lines not needed on preview, less cluttered

                    if (aq=24) or (aq=25)   // 212a
                       then begin
                              if dummy_template=True then Pen.Color:=shapes_colour  // 212a
                                                     else Pen.Color:=guide_colour;  // centre-lines
                            end
                       else Pen.Color:=rail_colour;

                    if (aq=16) and (adjacent_edges=True) and (draw_ts_platform_rear_edge=False) then Pen.Style:=psDot;    // 0.93.a show dotted on screen if hidden on output.
                    if (aq=20) and (adjacent_edges=True) and (draw_ms_platform_rear_edge=False) then Pen.Style:=psDot;

                    if ( (plain_track=False) or (aq=0) or (aq=8) or (aq=3) or (aq=11) or ((aq>15) and (aq<25)) ) and (aqyn[aq]=True)

                                  // stock rails and adjacent track only, if plain track, and data available ?

                       then begin
                              w_dots1:=Round((outoflist(aq,0,1)+ypd)*pvsy);
                              l_dots1:=Round(outoflist(aq,0,0)*pvsx);
                              move_to.X:=l_dots1+left_dots; move_to.Y:=bottom_dots-w_dots1;

                              for now:=1 to nlmax_array[aq] do
                                begin
                                  w_dots2:=Round((outoflist(aq,now,1)+ypd)*pvsy);
                                  l_dots2:=Round(outoflist(aq,now,0)*pvsx);
                                  line_to.X:=l_dots2+left_dots; line_to.Y:=bottom_dots-w_dots2;
                                  if check_limits(move_to, line_to)=True
                                     then begin
                                            MoveTo(move_to.X, move_to.Y);
                                            LineTo(line_to.X, line_to.Y);

                                            if aq<>24 then something_drawn:=True;   // (so we can also draw aq=25 if we draw aq=24)
                                          end;
                                  move_to:=line_to;
                                end;
                            end;
                  end;//next aq
                                      // finally, draw the rail ends for the control template...

                  if (plain_track=False) and (cl_only=False)
                     then begin                                       // mark rail-ends...

                            mark_end(1,1,9,1,True);    // turnout rail wing rail finish.
                            mark_end(2,1,10,1,True);   // main rail wing rail finish.

                            mark_end(6,0,14,0,True);   // main side check rail start.
                            mark_end(6,1,14,1,True);   // main side check rail finish.

                            mark_end(7,0,15,0,True);   // turnout side check rail start.
                            mark_end(7,1,15,1,True);   // turnout side check rail finish.

                            mark_end(4,0,5,0,True);    // blunt nose.

                            if (half_diamond=True) and (fixed_diamond=True)
                               then begin
                                      mark_end(1,0,9,0,True);       // planed faced of point rails for a fixed-diamond.
                                      mark_end(2,0,10,0,True);

                                      mark_end(26,1,27,1,True);     // MS K-crossing check rails.
                                      mark_end(28,1,29,1,True);     // DS K-crossing check rails.
                                    end;

                          end;

                             // 0.93.a platform ends ...

                        // 0.93.a TS platform start

                  mark_end(16,0,17,0,draw_ts_platform_start_edge);

                        // 0.93.a TS platform end

                  mark_end(16,1,17,1,draw_ts_platform_end_edge);

                        // 0.93.a MS platform start

                  mark_end(20,0,21,0,draw_ms_platform_start_edge);

                        // 0.93.a MS platform end

                  mark_end(20,1,21,1,draw_ms_platform_end_edge);

                end;//control template

                    // finally do the page outlines and page labels...

      Font.Assign(preview_form.Font);    //  use the form font for the page numbers.
      font_size:=Font.Size;

      page_length_pixels:=Round(page_length_out*pvsx);  //  used for page label check.

      While (TextWidth(' page  w/99  ')>page_length_pixels) and (Font.Size>12)   // do we need to change font size (to minimum 12 pt)?
        do Font.Size:=Font.Size-1;

      if TextWidth(' page  w/99  ')<page_length_pixels    // now room between margin lines for full string?
         then page_prefix_str:=' page  '
         else begin                         // shorten label and try full-size font again.
                page_prefix_str:=' ';
                Font.Size:=Round(font_size);
              end;

      While (TextWidth(page_prefix_str+'w/99  ')>page_length_pixels) and (Font.Size>6)   // still need to change font size (minimum 6 pt)?
        do Font.Size:=Font.Size-1;

      for sheet_down:=0 to sheet_down_c do begin              // every sheet (all 600).
        for sheet_across:=0 to sheet_across_c do begin

          with sheet[sheet_down,sheet_across] do begin

            if empty=True    // blank page?
               then begin
                      if (sheet_down=0) and (sheet_across=0)  // page a/1 empty ?
                         then begin                           // yes, mark the corner dotted.
                                Pen.Color:=page_colour;
                                Pen.Mode:=pmCopy;
                                Pen.Style:=psDot;
                                Pen.Width:=1;

                                Brush.Color:=paper_colour;
                                Brush.Style:=bsSolid;
                                TextOut(0,0,'');

                                move_to.X:=Round(grid_top*pvsx)+left_dots; move_to.Y:=bottom_dots-Round(grid_left*pvsy);  // paper top left corner.
                                line_to.X:=move_to.X; line_to.Y:=bottom_dots-Round(grid_right*pvsy);                      // paper top margin.
                                if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                                line_to.X:=Round(grid_bottom*pvsx)+left_dots; line_to.Y:=move_to.Y;                       // paper left margin.
                                if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                                    // put the a/1 page number at the bottom (in brackets)...

                                psx:=Round(grid_top*pvsx);    // page number string X

                                text_X:=psx+left_dots+TextWidth(' ');    // text position.
                                if text_X<0 then text_X:=0;              // (in case of first banner page).

                                page_str:=' ('+page_prefix_str+Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1)+' ) ';

                                TextOut(text_X,bottom_dots-Round(grid_left*pvsy)+(Font.Height*3 div 2),page_str);
                              end;
                       CONTINUE;    // draw pages only if not empty.
                    end;

                    //  draw page outlines...

            Pen.Color:=page_colour;
            Pen.Mode:=pmCopy;
            if banner_paper=True then Pen.Style:=psDot       // for vertical lines on screen
                                 else Pen.Style:=psSolid;
            Pen.Width:=1;

            Brush.Color:=paper_colour;
            Brush.Style:=bsSolid;
            TextOut(0,0,'');

            move_to.X:=Round(grid_top*pvsx)+left_dots; move_to.Y:=bottom_dots-Round(grid_left*pvsy);         // paper top left.

            line_to.X:=Round(grid_top*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_right*pvsy);        // paper top margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            Pen.Style:=psSolid;     // same for banners or cut sheets.
            move_to:=line_to;
            line_to.X:=Round(grid_bottom*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_right*pvsy);        // paper right margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;


            if banner_paper=True then Pen.Style:=psDot;
            move_to:=line_to;
            line_to.X:= Round(grid_bottom*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_left*pvsy);   // paper bottom margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            Pen.Style:=psSolid;     // same for banners or cut sheets.
            move_to:=line_to;
            line_to.X:= Round(grid_top*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_left*pvsy);      // paper left margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                // put the page number at the bottom ...

            psx:=Round(grid_top*pvsx);    // page number string X

            text_X:=psx+left_dots+TextWidth(' ');    // text position.
            if text_X<0 then text_X:=0;              // (in case of first banner page).

            page_str:=page_prefix_str+Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1)+' ';

            TextOut(text_X,bottom_dots-Round(grid_left*pvsy)+(Font.Height*3 div 2),page_str);

          end;//with sheet

        end;//for next sheet across
      end;//for-next sheet down

    end;//with Canvas

    copy_draw_to_pad;   // copy the bitmap to the pad.

    RESULT:=True;
  finally
    Screen.Cursor:=crDefault;
  end;//try
end;
//_____________________________________________________________________________________

function crop_alert(size_str:string):boolean;

const
  multi_print_str:string='     Print / PDF  run  overlap.'
  +'||To change the output size for printing or PDF output, select the OUTPUT > ENLARGE / REDUCE SIZE menu items.'
  +'||To change the printing paper sheet size select the OUTPUT > PRINTER SETUP... menu item.'
  +'||To move the page origin with the mouse select the ACTION > MOUSE ACTIONS:PAD > MOVE PAGE ORIGIN menu item (SHIFT+CTRL-F10), or to enter a new page origin directly'
  +' select the OUTPUT > PAGE ORIENTATION / ORIGIN > SET PAGE ORIGIN... menu item.'
  +'||If the drawing cannot be fitted within the print run by moving the page origin, it will be necessary to print it in multiple runs.'
  +' After printing the first run of pages, select one of the OUTPUT > PAGE ORIENTATION / ORIGIN > MULTIPLE PRINT RUNS menu items to prepare for the next run and to preserve the page joins between runs.'
  +'||N.B. Do not change the print size between multiple print runs.'
  +'||After printing multiple runs the page origin can be restored to the drawing datum by selecting the OUTPUT > PAGE ORIENTATION / ORIGIN > RESET PAGE ORIGIN menu item.';

var
  i:integer;

begin
  RESULT:=False;  // init

  repeat
    i:=alert(2,'    overlap  for  PDF  or  printing  the  '+size_str,
              'This drawing overlaps the maximum print run size of '+IntToStr(sheet_down_c+1)+' pages in length and/or '+IntToStr(sheet_across_c+1)+' pages in width.'
              +'||At your current paper size, this represents a maximum printed area of '+round_str((sheet_down_c+1)*page_length/100,2)+' mm long and '+round_str((sheet_across_c+1)*page_width/100,2)+' mm wide.'
              +'||Unless you move the page origin it will have to be printed in multiple runs, or at a smaller size, or on larger sheets of paper.'
              +'||The current print size is '+round_str(out_factor*100,2)+' %',
               '','','','?  help','cancel  print / PDF    ','O K    ',4);
    case i of
        4: alert_help(0,multi_print_str,'');
        5: RESULT:=True;
    end;//case
  until i<>4;
end;
//_______________________________________________________________________________________

procedure Tpreview_form.print_buttonClick(Sender: TObject);

begin
  if continue_button.Showing=True then continue_button.SetFocus;    // don't want focus on this button next time.

                       // don't hide the form - it causes a normal pad repaint.
                       // ModalResult=mrYes, so form closes itself.
end;
//______________________________________________________________________________________

procedure print_index_sheet(pdf:boolean);    // 0.91.d pdf // print a preview of printed sheets as a page index.

var
  pvsx, pvsy:extended;            // don't use the global
  psx:integer;

  sheet_down:integer;             // current sheet index.
  sheet_across:integer;           // ditto

  left_dots, bottom_dots:integer;

  xmax, ymax:integer;

  aq, now:integer;
  w_dots1, l_dots1, w_dots2, l_dots2:integer;

  page_length_pixels:integer;

  move_to, line_to :TPoint;

  text_X:integer;
  page_str,top_str:string;
  i,n:integer;

  maxbg,bgk,array_max:integer;
  now_keep:Tbgnd_keep;
  xint,yint:integer;

  something_drawn:boolean;

  font_height, font_size:extended;
  now_shape:Tbgnd_shape;
  x1,y1,x2,y2:extended;

  dummy_i:integer;

		  ////////////////////////////////////////////////////////////////////////

                  procedure mark_end(aq1, aq1end, aq2, aq2end:integer; pen_solid:boolean);      // make the mark

                  var
                    p1, p2:TPoint;

                  begin

                    if (endmarks_yn[aq1,aq1end]=True) and (endmarks_yn[aq2,aq2end]=True)
                       then begin
                              p1:=endmarks[aq1,aq1end];
                              p2:=endmarks[aq2,aq2end];

                              with Printer.Canvas do begin

                                Pen.Width:=1;

                                Brush.Color:=clWhite;  // gaps in dotted lines.
                                Brush.Style:=bsSolid;

                                TextOut(0,0,'');

                                if pen_solid=True then Pen.Style:=psSolid   // 0.93.a mods for platforms
                                                  else Pen.Style:=psDot;

                                move_to.X:=Round(p1.X*pvsx)+left_dots;  move_to.Y:=bottom_dots-Round((p1.Y+ypd)*pvsy);
                                line_to.X:=Round(p2.X*pvsx)+left_dots;  line_to.Y:=bottom_dots-Round((p2.Y+ypd)*pvsy);
                                if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                              end;//with
                            end;
                  end;
                  /////////////////////////////////////////////////////////////////////////
begin

  try
    Screen.Cursor:=crHourglass;

    if calcs_done_and_valid=False then redraw(False);    //  first do a direct redraw if nec to ensure valid calcs.
    if calcs_done_and_valid=False then EXIT;             //  calcs still not valid.

    if page_info(False,True,pdf,0)=False   //  read the printer details and page sizes.
       then EXIT;                          //  printer driver problem.

    with Printer.Canvas do begin

      Printer.BeginDoc;

      Font.Assign(set_font('Arial',6,[],clBlack));
      top_str:=' '+DateToStr(Date)+'  '+TimeToStr(Time)+'      TEMPLOT V:'+round_str(program_version/100,2)+'  from  Martin  Wynne   templot.com';
      TextOut(0,0,top_str);

      Font.Assign(set_font('Comic Sans MS',8,[],clBlack));
      top_str:=' page map for :    '+print_form.header_label.Caption+'     '+print_form.origin_label.Caption;
      if box_project_title_str<>'' then top_str:=top_str+'          for :  '+box_project_title_str;
      TextOut(0,TextHeight(' '),top_str);


      xmax:=printer_width_indexmax_dots-20;                     // 20 arbitrary margin   (1/15" at 300dpi)
      ymax:=printer_length_indexmax_dots-20;

                           // calc preview scaling to show all the pages...

      pvsx:=                  (xmax-20)/((sheet_co_long+1)*page_length_out);  // x screen pixels per 1/100th mm (scaled), to fit all pages.
      pvsy:=(ymax-20-TextHeight(' ')*3)/((sheet_co_wide+1)*page_width_out);   // y ditto.

      if pvsx>pvsy then pvsx:=pvsy;            // make sure scales are equal.
      if pvsy>pvsx then pvsy:=pvsx;
                                               // set left origin...

      left_dots:=10;                      // init for no staggered ends needed.

      if staggered_pages=True
         then begin
                for n:=0 to sheet_co_wide do begin  // search for any non-empty staggered column first pages.
                  if (n mod 2)=0 then CONTINUE;     // not even columns.
                  if sheet[0,n].empty=False
                     then begin
                            left_dots:=left_dots+Round((page_length_out*pvsx)/2);   // offset to include all staggered sheets.
                            BREAK;
                          end;
                end;//for
              end;

      left_dots:=left_dots-Round(page_top_offset*pvsx);
      bottom_dots:=ymax+Round(page_left_offset*pvsy);

            // !! N.B. Use all pre-defined colours in case black/white wanted.

            // first draw any background shapes (ignoring page outlines and empty pages)...

      maxbg:=bgnd_form.bgnd_shapes_listbox.Items.Count;
      if (print_settings_form.output_bgnd_shapes_checkbox.Checked=True) and (maxbg>0)
         then begin
                Font.Assign(shapes_label_font);      // for shape labels.

                font_height:=Font.Size*25.4/72;      // in mm
                font_height:=font_height*pvsx*100;   // in pixels scaled to printer.

                font_size:=font_height/nom_width_dpi*72;   // point size scaled to printer

                font_size:=limits(4,Font.Size*2,font_size,dummy_i);    // limit printed font size.

                Font.Size:=Round(font_size);   // font scaled down to the pad contents (but not up more than double).

                Pen.Width:=1;
                Pen.Mode:=pmCopy;

                Pen.Color:=clLime;     // use this colour for shapes (arbitrary).
                Font.Color:=clLime;
                Brush.Color:=clWhite;

                for i:=0 to maxbg-1 do begin

                  try
                    Pen.Style:=psSolid;

                    now_shape:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape;     // next shape.

                    with now_shape do begin

                      if (hide_bits AND $02)<>0 then CONTINUE;   // shape hidden for output

                      case shape_style of
                                0,1,3: begin
                                         Brush.Color:=clWhite;
                                         Brush.Style:=bsSolid;      // blank or transparent.
                                       end;
                                    2: begin
                                         Brush.Color:=Pen.Color;
                                         Brush.Style:=bsDiagCross;  // cross-hatched.

                                         TextOut(0,0,'');      // !!! Delphi bug? This seems to be necessary before dotted lines will draw properly.
                                                               // TextOut obviously initialises some background mask property which I have been unable
                                                               // to find or set any other way.

                                         if shape_code=0 then Pen.Style:=psDot;  // dashed line.

                                       end;
                      end;//case

                      x1:=(p1.x*100+re_org_x)*pvsx;
                      y1:=(p1.y*100+re_org_y)*pvsy;
                      x2:=(p2.x*100+re_org_x)*pvsx;
                      y2:=(p2.y*100+re_org_y)*pvsy;

                      move_to.X:=Round(x1)+left_dots;
                      move_to.Y:=bottom_dots-Round(y1);

                      if shape_code=3      // label rectangle..
                         then begin
                                line_to.X:=move_to.X+TextWidth(' '+shape_name+' ')+3;
                                line_to.Y:=move_to.Y+TextHeight(shape_name)+3;
                              end
                         else begin
                                line_to.X:=Round(x2)+left_dots;
                                line_to.Y:=bottom_dots-Round(y2);
                              end;

                      if check_limits(move_to, line_to)=True
                         then begin
                                case shape_code of
                                      -1: begin                            // picture.
                                            Brush.Color:=paper_colour;
                                            Brush.Style:=bsClear;

                                            TextOut(0,0,'');   // !!! Delphi bug? This seems to be necessary before dotted lines will draw properly.
                                                               // TextOut obviously initialises some background mask property which I have been unable
                                                               // to find or set any other way.

                                            Pen.Style:=psDot;

                                            Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                            MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y);  // picture outlines only - draw diagonal line.
                                            MoveTo(move_to.X, line_to.Y); LineTo(line_to.X, move_to.Y);  // and other diagonal line.
                                          end;

                                       0: begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                     1,3: Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                       2: Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);
                                end;//case

                                if shape_code=3 then TextOut(move_to.X+1,move_to.Y+1,' '+shape_name);   // insert label text in rectangle box.
                              end;
                    end;//with now_shape

                  except
                    CONTINUE;      // ignore this shape if any calc errors.
                  end;//try

                end;//for next i shape
              end;//if draw shapes

            //  draw background keeps first (so current turnout overdraws)...

      if (print_entire_pad_flag=True) and (keeps_list.Count>0)
         then begin

                Pen.Color:=printtimber_colour;   // probably blue for the bg rails.
                Pen.Mode:=pmCopy;
                Pen.Style:=psSolid;
                Pen.Width:=1;

                for bgk:=0 to (keeps_list.Count-1) do begin

                  if Ttemplate(keeps_list.Objects[bgk]).bg_copied=False then CONTINUE;  // no data, not on background.

                  if (Ttemplate(keeps_list.Objects[bgk]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

                  now_keep:=Ttemplate(keeps_list.Objects[bgk]).bgnd_keep;

                  with now_keep do begin

                    something_drawn:=False;   // nothing drawn yet for this keep.

                    for aq:=0 to aq_max_c do begin                                // ignore FB foot lines.
                      if (aq>7) and (aq<16) then CONTINUE;                        // gauge faces only.
                      if (aq=27) or (aq=29) then CONTINUE;                        // ditto for K-crossing check rails.

                      if (something_drawn=True) and ( (aq=24) or (aq=25) ) then CONTINUE;  // don't draw centre-lines unless there is nothing else. (cl-only templates).

                      array_max:=intarray_max(list_bgnd_rails[aq,0]);
                      if array_max=0 then CONTINUE;                        // empty rail.

                      xint:=intarray_get(list_bgnd_rails[aq,0],0);
                      yint:=intarray_get(list_bgnd_rails[aq,1],0);

                      w_dots1:=Round(yint*pvsy);
                      l_dots1:=Round(xint*pvsx);
                      move_to.X:=l_dots1+left_dots; move_to.Y:=bottom_dots-w_dots1;

                      for now:=1 to array_max do
                        begin
                          xint:=intarray_get(list_bgnd_rails[aq,0],now);
                          yint:=intarray_get(list_bgnd_rails[aq,1],now);

                          w_dots2:=Round(yint*pvsy);
                          l_dots2:=Round(xint*pvsx);
                          line_to.X:=l_dots2+left_dots; line_to.Y:=bottom_dots-w_dots2;

                          if check_limits(move_to, line_to)=True
                             then begin
                                    MoveTo(move_to.X, move_to.Y);
                                    LineTo(line_to.X, line_to.Y);
                                    if aq<>24 then something_drawn:=True;   // (so we can also draw aq=25 if we draw aq=24)
                                  end;
                          move_to:=line_to;
                        end;//for next now
                    end;//for next aq
                  end;//with now_keep
                end;//for next bgnd_keep
              end;//if bgnd keeps

          //  draw turnout rails...     (once only).
      
      if  (print_entire_pad_flag=False) // control template
      and (output_diagram_mode=False)   // 0.93.a  no control template if diagram mode
      and (turnoutx>0)                  // not if invalidated

         then begin

                Pen.Color:=printcurail_colour;
                Pen.Mode:=pmCopy;

                Pen.Width:=1;

                Brush.Color:=clWhite;  // gaps in dotted lines.
                Brush.Style:=bsSolid;

                TextOut(0,0,'');  // needed for dotted lines - Delphi bug?

                for aq:=0 to aq_max_c do begin

                    if (cl_only=False) and ((aq=24) or (aq=25)) then CONTINUE;  // 0.93.a centre-lines not needed on preview, less cluttered

                    Pen.Style:=psSolid;  // init

                    if (aq=16) and (adjacent_edges=True) and (draw_ts_platform_rear_edge=False) then Pen.Style:=psDot;    // 0.93.a show dotted on screen if hidden on output.
                    if (aq=20) and (adjacent_edges=True) and (draw_ms_platform_rear_edge=False) then Pen.Style:=psDot;

                    if ( (plain_track=False) or (aq=0) or (aq=8) or (aq=3) or (aq=11) or ((aq>15) and (aq<25)) ) and (aqyn[aq]=True)

                          // stock rails and adjacent track only, if plain track, and data available ?

                       then begin
                              w_dots1:=Round((outoflist(aq,0,1)+ypd)*pvsy);
                              l_dots1:=Round(outoflist(aq,0,0)*pvsx);
                              move_to.X:=l_dots1+left_dots; move_to.Y:=bottom_dots-w_dots1;

                              for now:=1 to nlmax_array[aq] do
                                begin
                                  w_dots2:=Round((outoflist(aq,now,1)+ypd)*pvsy);
                                  l_dots2:=Round(outoflist(aq,now,0)*pvsx);
                                  line_to.X:=l_dots2+left_dots; line_to.Y:=bottom_dots-w_dots2;
                                  if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;
                                  move_to:=line_to;
                                end;
                            end;
                  end;//next aq
                                      // finally, draw the rail ends for the control template...

                  if (plain_track=False) and (cl_only=False)
                     then begin                                       // mark rail-ends...

                            mark_end(1,1,9,1,True);    // turnout rail wing rail finish.
                            mark_end(2,1,10,1,True);   // main rail wing rail finish.

                            mark_end(6,0,14,0,True);   // main side check rail start.
                            mark_end(6,1,14,1,True);   // main side check rail finish.

                            mark_end(7,0,15,0,True);   // turnout side check rail start.
                            mark_end(7,1,15,1,True);   // turnout side check rail finish.

                            mark_end(4,0,5,0,True);    // blunt nose.

                            if (half_diamond=True) and (fixed_diamond=True)
                               then begin
                                      mark_end(1,0,9,0,True);     // planed faced of point rails for a fixed-diamond.
                                      mark_end(2,0,10,0,True);

                                      mark_end(26,1,27,1,True);     // MS K-crossing check rails.
                                      mark_end(28,1,29,1,True);     // DS K-crossing check rails.
                                    end;

                          end;

                           // 0.93.a platform ends ...

                      // 0.93.a TS platform start

                mark_end(16,0,17,0,draw_ts_platform_start_edge);

                      // 0.93.a TS platform end

                mark_end(16,1,17,1,draw_ts_platform_end_edge);

                      // 0.93.a MS platform start

                mark_end(20,0,21,0,draw_ms_platform_start_edge);

                      // 0.93.a MS platform end

                mark_end(20,1,21,1,draw_ms_platform_end_edge);

              end;//control template.

                    // finally do the page outlines and page labels...

      Font.Assign(printer_text_font);                       // use the printer font for the page numbers.
      page_length_pixels:=Round(page_length_out*pvsx);      //  used for page label check.

      While (TextWidth(' w/9  ')>page_length_pixels) and (Font.Size>6)   // need to change font size (minimum 6 pt)?
        do Font.Size:=Font.Size-1;

      for sheet_down:=0 to sheet_down_c do begin              // every sheet (all 600).
        for sheet_across:=0 to sheet_across_c do begin

          with sheet[sheet_down,sheet_across] do begin

            if empty=True    // blank page?
               then begin
                      if (sheet_down=0) and (sheet_across=0)  // page a/1 empty ?
                         then begin                           // yes, mark the corner dotted.
                                Pen.Color:=printmargin_colour;
                                Pen.Mode:=pmCopy;
                                Pen.Style:=psDot;
                                Pen.Width:=1;

                                Brush.Color:=clWhite;
                                Brush.Style:=bsSolid;

                                move_to.X:=Round(grid_top*pvsx)+left_dots; move_to.Y:=bottom_dots-Round(grid_left*pvsy);  // paper top left corner.
                                line_to.X:=move_to.X; line_to.Y:=bottom_dots-Round(grid_right*pvsy);                      // paper top margin.
                                if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                                line_to.X:=Round(grid_bottom*pvsx)+left_dots; line_to.Y:=move_to.Y;                       // paper left margin.
                                if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                                    // put the a/1 page number at the bottom (in brackets)...

                                psx:=Round(grid_top*pvsx);    // page number string X

                                text_X:=psx+left_dots+TextWidth(' ');    // text position.
                                if text_X<0 then text_X:=0;              // (in case of first banner page).

                                page_str:=' ( '+Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1)+' ) ';

                                TextOut(text_X,bottom_dots-Round(grid_left*pvsy)+(Font.Height*2),page_str);
                              end;
                       CONTINUE;    // draw pages only if not empty.
                    end;


            Pen.Color:=printmargin_colour;
            Pen.Mode:=pmCopy;

            if banner_paper=True
               then begin
                      Pen.Width:=1;
                      Pen.Style:=psDot       // for vertical lines
                    end
               else begin
                      Pen.Width:=3;
                      Pen.Style:=psSolid;
                    end;

            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;

            move_to.X:=Round(grid_top*pvsx)+left_dots; move_to.Y:=bottom_dots-Round(grid_left*pvsy);         // paper top left.
            line_to.X:=Round(grid_top*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_right*pvsy);        // paper top margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            Pen.Style:=psSolid;     // same for banners or cut sheets.
            Pen.Width:=3;
            move_to:=line_to;
            line_to.X:=Round(grid_bottom*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_right*pvsy);        // paper right margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            if banner_paper=True
               then begin
                      Pen.Width:=1;
                      Pen.Style:=psDot       // for vertical lines
                    end
               else begin
                      Pen.Width:=3;
                      Pen.Style:=psSolid;
                    end;

            move_to:=line_to;
            line_to.X:= Round(grid_bottom*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_left*pvsy);   // paper bottom margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

            Pen.Style:=psSolid;     // same for banners or cut sheets.
            Pen.Width:=3;
            move_to:=line_to;
            line_to.X:= Round(grid_top*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_left*pvsy);      // paper left margin.
            if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

                // put the page number at the bottom ...

            psx:=Round(grid_top*pvsx);               // page number string X

            text_X:=psx+left_dots+TextWidth(' ');    // text position.
            if text_X<0 then text_X:=0;              // (in case of first banner page).

            page_str:=' '+Chr(sheet_across+97)+'/'+IntToStr(sheet_down+1)+' ';

            TextOut(text_X,bottom_dots-Round(grid_left*pvsy)+(Font.Height*2),page_str);

          end;//with sheet

        end;//for next sheet across
      end;//for-next sheet down

    end;//with Printer.Canvas

    Printer.EndDoc;

  finally
    if Printer.Printing=True then Printer.EndDoc;
    Screen.Cursor:=crDefault;
  end;//try
end;
//_____________________________________________________________________________________

procedure Tpreview_form.continue_buttonClick(Sender: TObject);

begin
  ModalResult:=mrOK;   // for the panel click.
end;
//______________________________________________________________________________________

procedure Tpreview_form.font_buttonClick(Sender: TObject);

begin
  print_form.font_button.Click;
end;
//__________________________________________________________________________________________

procedure outline_in_red(sheet_down,sheet_across:integer);   //  draw next page outline in red.

var
  move_to,line_to:TPoint;

begin
  copy_draw_to_pad;                                 // to erase previous red outlines.
  with pad_form.Canvas do begin                     // directly on the pad, not the offdraw bitmap.
    with sheet[sheet_down,sheet_across] do begin

      Pen.Color:=clRed;
      Pen.Mode:=pmCopy;
      Pen.Style:=psSolid;
      Pen.Width:=2;

      move_to.X:=Round(grid_top*pvsx)+left_dots; move_to.Y:=bottom_dots-Round(grid_left*pvsy);         // paper top left.

      line_to.X:=Round(grid_top*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_right*pvsy);        // paper top margin.
      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

      move_to:=line_to;
      line_to.X:=Round(grid_bottom*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_right*pvsy);        // paper right margin.
      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

      move_to:=line_to;
      line_to.X:= Round(grid_bottom*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_left*pvsy);   // paper bottom margin.
      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

      move_to:=line_to;
      line_to.X:= Round(grid_top*pvsx)+left_dots; line_to.Y:=bottom_dots-Round(grid_left*pvsy);      // paper left margin.
      if check_limits(move_to, line_to)=True then begin MoveTo(move_to.X, move_to.Y); LineTo(line_to.X, line_to.Y); end;

    end;//with sheet
  end;//with canvas
end;
//______________________________________________________________________________________

procedure Tpreview_form.FormCreate(Sender: TObject);

begin
  AutoScroll:=False;

  // OT-FIRST ClientWidth:=504;
  // OT-FIRST ClientHeight:=82;
end;
//______________________________________________________________________________

end.

