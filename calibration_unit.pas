
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

unit calibration_unit;

{$MODE Delphi}

{$ALIGN OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls

  , LCLtype;     // OT-FIRST

type
  Tcalibration_form = class(TForm)
    cal_image: TImage;
    form_label: TLabel;
    open_dialog: TOpenDialog;
    save_dialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  calibration_form: Tcalibration_form;

//---------------------

type

  Tcal_data=record
             printer_calibrated:boolean;

             alignment_byte_1:byte;   // D5 0.91.a

             printer_head_factor:extended;
             printer_roller_factor:extended;

             alignment_byte_2:byte;   // D5 0.91.a
             alignment_byte_3:byte;   // D5 0.91.a

             printer_impact:integer;             // 0=no, 1=yes, impact printer, 2=plotter,  -1= don't yet know.
             printer_name:string[25];

             alignment_byte_4:byte;   // D5 0.91.a
             alignment_byte_5:byte;   // D5 0.91.a

            end;//record

  Tprint_cal=class(TPersistent)             // objects attached to printer_list.

             public                         // 0.85.a

              cal_data:Tcal_data;
             end;//class

const
  cal_prompt_str:string='      Printer  Calibration'

   +'||For the maximum dimensional accuracy of your printed templates it is usually necessary to calibrate the printer.'
   +'||This involves carefully measuring a printed test pattern, and then entering the sizes, so that Templot0 can adjust'
   +' the subsequent printing of your templates accordingly.'
   +'||If this printer has been calibrated previously, the calibration information can be re-entered directly, or reloaded from a file.'
   +'||You can choose to calibrate any or all of your printers, and switch between them without losing the calibration information for each.'
   +' This allows you to to do quick trial templates on one printer and the final construction templates on another, for example.'
   +'||If the printer being used is uncalibrated, Templot0 will mark the printed templates with a warning. They may be acceptably accurate'
   +' for trial planning purposes.'
   +'||For the final construction templates, calibration is strongly recommended, and for maximum accuracy it is important to use the same'
   +' type of paper for both the calibration test print and the finished templates.'
   +'||Take your time to follow through the calibration process carefully and without hurry, as entering incorrect figures will wreck the accuracy'
   +' of your templates.'
   +'||To delete incorrect or unwanted calibrations, select the PRINT > PRINTER CALIBRATION > DELETE menu items.'
   +'||If you are not confident of your ability to measure the test print with sufficient accuracy it would probably be better'
   +' to leave the printer uncalibrated.'
   +'||Click CALIBRATE THIS PRINTER NOW to start the process - there are further detailed help notes as you go along.';

var
  temp:extended;
  procedure calibrate_printer;                    // get calibration data for printer.
  procedure proof_sheet;                          // print proof sheet.
  procedure calibration_load(prindex:integer);    // get calibration from file.
  procedure calibration_save(prindex:integer);    // save calibration to file.

  function get_prindex(var index:integer):boolean;          // return current printer index.

//________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  Printers, pad_unit, preview_unit, help_sheet, alert_unit, entry_sheet, chat_unit,
  control_room, math_unit;

const
  cal_help_str:string='      Printer  Calibration  -  Factors  Known'

   +'||If you have previously performed a calibration for this printer and this type and thickness of paper, Templot0 will'
   +' have printed out the relevant correction factors on the proof sheet. You can therefore enter them again now, thus saving the need to do'
   +' a fresh calibration test print. But take care to enter the figures correctly as invalid correction factors will render your templates'
   +' unusable. In most cases the correction factors will be figures close to 100 %, for example figures such as 102.43 % or 99.72 %.'
   +'||Bear in mind that the existing factors are only valid for this printer and the SAME type and thickness of paper. If you'
   +' are using a new batch of paper, it is recommended that you do a fresh calibration.'
   +'||( Templot0 can detect if you change the printer, but NOT if you change the type of paper. To ensure accurate templates'
   +' make sure that you always use paper that matches the current calibration settings ! )'
   +'||If you have previously saved the calibration factors to a file you can simply reload them.'
   +'||(If you have added, removed or swapped printers on your computer since the file was saved Templot0 may or may not report an error,'
   +' but either way you should print a proof sheet to check, and if necessary re-calibrate and save a fresh file.)';

  save_help_str:string='      Save  Calibration'

  +'||You can save your calibration results to a file, so that they can be quickly reloaded when you next run Templot0.'
  +'||Bear in mind that the settings are only valid for the type of paper that you have used for the calibration, so for each printer you will'
  +' probably want to save different calibration files for different papers.'
  +'||( It is useful to include the paper type in the name of the file. Templot0 will suggest as a file name the name of the printer suffixed "-papertype".'
  +'  Change this to, for example, "-tracing" if this calibration is for tracing paper.)';

var
  head_factor, roller_factor:double;

  function factors_known:boolean;forward;

  procedure do_cal;forward;
  procedure do_proof;forward;                  // print proof sheet.

//_________________________________________________________________________________________

function get_prindex(var index:integer):boolean;           // return current printer index.

var
  prin_device,prin_driver,prin_port:array [0..255] of Char;          // for GetPrinter (NT).
  prin_device_mode:THandle;

  n:integer;

begin
  index:=0;        // init...
  RESULT:=False;
  if printer_list.Count<1 then EXIT;

  try

    { OT-FIRST

    if Win32Platform=VER_PLATFORM_WIN32_NT   // running under Windows NT...
      then begin
             Printer.GetPrinter(prin_device,prin_driver,prin_port,prin_device_mode);    // problem with Windows NT if we use the Printers string list (doesn't return PrinterIndex correctly).
                                                                                        // (ignore driver, port, device mode - parameters needed for call but not used here)
             for n:=0 to printer_list.Count-1 do begin                                  // set to max index if no match.
               if printer_list.Strings[n]=Trim(string(prin_device))
                  then begin
                         index:=n;
                         RESULT:=True;
                         BREAK;         // found match in our list.
                       end;
             end;//for
           end
      else begin   //  Windows 95/98.}

            //  (first make sure PrinterIndex is within our list range) ...

             while printer_list.Count<Printer.Printers.Count do begin              // should never do this...
                 n:=printer_list.AddObject('unknown printer',Tprint_cal.Create);

                 with Tprint_cal(printer_list.Objects[n]).cal_data do begin
                    printer_impact:=-1;                                            // type not yet known.
                    printer_calibrated:=False;
                    printer_head_factor:=1.0;
                    printer_roller_factor:=1.0;
                 end;//with

             end;//while
             n:=Printer.PrinterIndex;
             if n>(printer_list.Count-1) then n:=printer_list.Count-1;

             index:=n;
             RESULT:=True;
           { OT-FIRST end;}
  except
    index:=0;
    RESULT:=False;
  end;//try
end;
//________________________________________________________________________________________

procedure calibrate_printer;          // get calibration data for printer.

var
  i,prindex:integer;
  prstr:string;

begin

  if no_printer_available=True
     then begin
            alert(6,'   no  printers',
                    '||Templot0 is unable to locate a printer on this system.'
                   +'||If you have recently installed a printer, please save your work and restart Templot0.',
                    '','','','','','O K',0);
            EXIT;
          end;

   repeat                                             // in case he wants to change printer.
     if get_prindex(prindex)=False then EXIT;
     prstr:=printer_list.Strings[prindex];            // name of current printer.

     i:=alert(7,'      printer  calibration ...',
                'Calibration for  '+prstr
               +'||Have you previously calibrated this printer for this type of paper?',
                'important  information','change  printer  ( printer  setup ... )','yes  -  load  calibration  from  file','yes  -  calibration  factors  known','cancel  calibration','no  -  calibrate  this  printer  now',1);

     case i of
         1: alert_help(0,cal_prompt_str+'|||'+cal_help_str+'|||'+save_help_str,'');
         2: printer_setup(True,False);
         3: begin
              calibration_load(prindex);
              EXIT;
            end;
         4: begin
              factors_known;       // get data entry and print proof sheet.
              EXIT;
            end;
         5: EXIT;
         6: do_cal;               // print test and proof sheets.
     end;//case
   until i>2;
end;
//__________________________________________________________________________________________

function factors_known:boolean;      // calibration factors already known - let him enter them.

const
  hf_str:string='      Head  Factor'
               +'||This factor corrects movements of the print-head, to ensure accurate dimensions across the width of the sheet.'
               +'||Take care to enter the figure correctly as an invalid correction factor will render your templates'
               +' unusable. In most cases the correction factor will be a figure close to 100 %, for example a figure such as 102.43 % or 99.72 %.'
               +'||If you are not SURE that the your figures are correct for this printer, or you have changed the type of paper, please click'
               +' CANCEL ALL and perform a fresh calibration.';

  rf_str:string='      Roller  Factor'
               +'||This factor corrects movements of the paper through the printer rollers, to ensure accurate dimensions down the length of the sheet.'
               +' The Roller Factor is likely to vary with different paper surfaces and thicknesses.'
               +'||Take care to enter the figure correctly as an invalid correction factor will render your templates'
               +' unusable. In most cases the correction factor will be a figure close to 100 %, for example a figure such as 102.43 % or 99.72 %.'
               +'||If you are not SURE that the your figures are correct for this printer, or you have changed the type of paper, please click'
               +' CANCEL ALL and perform a fresh calibration.';

var
  head_factor,roller_factor:extended;
  printer_str:string;
  prindex:integer;
  n:integer;
  od:Toutdim;    // [0..7] array of extended;

begin
   RESULT:=False;                                   // default init.
   if get_prindex(prindex)=False then EXIT;         // get current printer index or none available.
   printer_str:=printer_list.Strings[prindex];      // name of current printer.

   if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_calibrated=True
      then begin
             head_factor:=Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_head_factor;
             roller_factor:=Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_roller_factor;
           end
      else begin
             head_factor:=1.0;
             roller_factor:=1.0;
           end;

   putdim(hf_str,4,'Head Factor for  '+printer_str,(head_factor*100),True,True,True,False);     // no neg, no preset, no zero, don't terminate on zero.
n:=putdim(rf_str,4,'Roller Factor for  '+printer_str,(roller_factor*100),True,True,True,False);   // ditto.

   if n<>1 then run_error(158);
   if getdims('calibration  factors',cal_help_str,pad_form,n,od)=True
      then begin
             if (od[0]<10) or (od[0]>1000) or (od[1]<10) or (od[1]>1000)
                then begin
                       alert(5,'    calibration  factor  error',
                               '||Your printer calibration factors are invalid.'
                              +'||( Valid range is 10 % to 1000 %.)',
                               '','','','','O K  -  cancel','',0);
                       RESULT:=False;
                     end
                else begin
                       Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_head_factor:=od[0]/100;
                       Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_roller_factor:=od[1]/100;
                       Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_calibrated:=True;         // flag we have the factors

                       page_info(True,True,False,0);  // for page outlines on pad.
                       redraw(True);
                       RESULT:=True;
                       pad_form.proof_sheet_menu_entry.Click;  // do a proof sheet.
                     end;
           end
      else RESULT:=False;
end;
//___________________________________________________________________________________________

procedure do_cal;                    // do printer calibration.

const
                //  WINAPI GetDeviceCaps Constants

   HORZRES         =   8;
   VERTRES         =  10;
   BITSPIXEL       =  12;
   LOGPIXELSX      =  88;
   LOGPIXELSY      =  90;

  cal_prep_str:string='    Printer  Calibration    -    Preparations'
   +'||Before doing a printer calibration, it is worth giving your printer a birthday.'
   +'||Clean out any dust, fluff, paper shreds, etc., and ensure'
   +' that nothing can interfere with the smooth feeding of the paper through the printer. Check that all the leads and plugs are firm and secure, and that the ink'
   +' cartridges / ribbons / consumables are properly seated in their holders.'
   +'||Some modern home printers are quite flimsy in construction, so check that it is'
   +' standing firm on a level surface to avoid any twist or distortion of the mechanism.'
   +'||If your printer setup software includes maintenance tasks such as alignment and cleaning of the cartridges, now is the time to do them.'
   +'||If the printer has not been used for a while, print a few scrap pages to get it warmed up to the normal operating condition.'
   +'||Carefully load the printer with the same type and thickness of paper which you intend to use for your construction templates.'
   +' (It is important for accuracy that the paper has been stored in the same conditions of temperature and humidity in which the finished templates will be used.)'
   +'||90 or 100 gsm paper is suitable, or you might prefer to print on inkjet-coated OHP transparencies, or drafting film, or ordinary tracing paper. Remember that a fresh'
   +' calibration will be needed for each of these to ensure accuracy.'
   +'||Finally, click DO PRINTER SETUP ... and set your printer for :'
   +'||print quality :  Best / Letter-Quality / Final / Super / Slow'
   +'|paper size   :  A4 or US-Letter or Letter'
   +'|orientation  :  Upright / Portrait'
   +'|and for the type of paper which you are using.'
   +'||You may have to click a button or tab marked PROPERTIES or OPTIONS or SETUP or FEATURES to find all of these settings.'
   +'||If your printer offers other setup options (such as setting the dpi resolution, or unidirectional / bidirectional printing), refer to the printer'
   +' documentation and use the recommended settings for CAD / Drawing applications. (Setting the resolution higher than 300 dpi will improve the appearance of angled lines'
   +' but will not advance the practical usefulness of the templates in any significant way; unidirectional printing will probably improve the accuracy, but at the expense of'
   +' longer printing times.)';

  cal_check_str:string='    Printer  Calibration    -    Test  Print  Measurements'
   +'||Please wait for the ink to dry and if the sheet is warm allow it to cool to room temperature before proceeding.'
   +'||While waiting, obtain an accurate measuring instrument such as an engineer''s caliper or a good quality 300-mm (12-inch) steel rule.'
   +' A pocket tape measure could be used if it is known to be accurate. (A school-type wooden or plastic ruler is unlikely to be suitable and may not to be any more accurate'
   +' than the uncalibrated printer.)'
   +'||You should now be seeing on the test sheet a picture enclosed within a rectangular picture-frame outline which fills most of the page.'
   +'||The picture can be ignored - it is there only to show "which way is up".'
   +'||( If you answered "yes" to the "impact printer ?" question during the printer setup, or your printer does not support bitmap transfers (e.g. a pen plotter), the picture will have been omitted.)'
   +'||We are concerned here only with the frame around the picture, which is drawn in thin lines to aid accurate measurement.'
   +'||The OUTER dimensions of the frame should be approximately 180 mm wide (across the width of the page)'
   +' by approximately 240 mm high (down the length of the page).'
   +'||The INNER dimensions of the frame (containing the picture) should be approximately 90 mm wide by approximately 120 mm high.'
   +'||The frame outlines should be rectangular with straight edges and square corners.'
   +'|----------------------------------------------------------------'
   +'|If the size of the frame differs SIGNIFICANTLY from these sizes, Templot0 can make the necessary corrections, but there may be'
   +' something odd about your printer or its setup.'
   +' If other Windows applications have been using the printer without problems, you should refer to your printer documentation, check everything, restart your computer with no'
   +' other applications running, and try a fresh calibration test print. If you get the same result, but the frame outlines are otherwise correctly rectangular and undistorted'
   +' you should proceed with the calibration.'
   +'||If the test print failed completely, or the frame outlines are missing or distorted, Templot0 will not be able to draw your templates until this printer problem has been resolved.'
   +'|----------------------------------------------------------------'
   +'|Lay the test sheet on a flat surface in good light and measure the exact dimensions of the frame as accurately as possible.'
   +'||You can choose to measure either the outer or the inner sizes of the frame, it is not necessary to do both. If you are using an engineer''s caliper it will be more convenient to measure the smaller inner dimensions.'
   +' If you are using a steel rule, you should preferably measure the outer dimensions for greater accuracy. If your printer has drawn lines of significant thickness (width) make your measurements to the centre of them.'

   +'||Templot0 expects the dimensions to be entered in millimetres, so if you are measuring in inches you should either multiply your meaurements by 25.4 , or you can prefix your inches figure with a letter i like this example dimension:  i7.095'
   +'||Alternatively the metric calculator tool can do the conversion for you, and the result in millimetres can be copied and pasted directly into the entry form.'

   +'||Eyesight permitting, try to estimate the dimension to the nearest 1/10th of a millimetre (0.1 mm) or 5 thou (0.005 inches) for good accuracy. If this is not possible, a measurement to the nearest 1/4 of a millimetre (0.25 mm)'
   +' or 10 thou (0.010 inches) will be satisfactory for most purposes. Remember to keep the rule parallel to the edges of the frame.'

   +'||Record your measurements on the test sheet for future reference.'
   +'||Click CONTINUE when you are satisfied that your measurements are as accurate as you can make them, and you are ready to enter them.'
   +'||If you enter incorrect figures, your templates will be dimensionally inaccurate and probably unusable. But you can do a fresh calibration (Shift+F5) at any time to correct things, so a "trial and error" approach is equally valid.';

var
  n:integer;
  od:Toutdim;    // [0..7] array of extended;

  i:integer;
  prstr:string;
  prindex:integer;

  width_dots:integer;    // printer page-width in dots.
  length_dots:integer;   // printer page-length in dots.

  width_dpi:integer;     // dpi across width.
  length_dpi:integer;    // dpi down length.

  width_mm:extended;     // printer page-width in mm.
  length_mm:extended;    // printer page-length in mm.

  l_left:integer;      // large rectangle left X.
  l_right:integer;     // large rectangle right X.
  l_top:integer;       // large rectangle top Y.
  l_bottom:integer;    // large rectangle bottom Y.

  s_left:integer;      // small rectangle left X.
  s_right:integer;     // small rectangle right X.
  s_top:integer;       // small rectangle top Y.
  s_bottom:integer;    // small rectangle bottom Y.

  of_text_left:integer;    // outer frame text from edge.
  if_text_left:integer;    // inner frame text from edge.

  of_text_top:integer;     // outer frame text from top.
  if_text_top:integer;     // inner frame text from top.

  text_top:integer;        // bottom text position.

  res_text_left:integer;   // dpi info text from edge.
  res_text_top:integer;    // dpi info text from top.

  web_note_left, web_note_top, web_url_left, web_url_top:integer;

  hunslet_ratio:extended;  // hunslet picture aspect ratio.

  picture_left:integer;    // hunslet picture position
  picture_right:integer;

  picture_top:integer;
  picture_bottom:integer;

  inner_rect:TRect;

  test_width, test_height:extended;

  which_frame_str,frame_info_str:string;   // 214a
  label 999;                               // 214a

begin
  try
    test_width:=0;
    test_height:=0;
    repeat
      if get_prindex(prindex)=False then EXIT;         // get current printer index or none available.
      prstr:=printer_list.Strings[prindex];

      i:=alert(7,'    printer  calibration  -  test  print',
                 '|Printer calibration for  '+prstr
                +'||Templot0 will now print a test sheet on this printer.'
                +'||Is the printer switched on and set for best quality printing on A4 or US-Letter size paper (or larger)?',
                 '','','please  read  this  first','do  printer  setup ...','cancel  calibration','O K  -  print  test  sheet',3);
      case i of
          3: alert_help(0,cal_prep_str,'');
          4: printer_setup(True,False);
          5: EXIT;
      end;//case

    until i>4;

    Printer.Orientation:=poPortrait;
    if Application.Terminated=False then Application.ProcessMessages;

    { OT-FIRST
    width_dots:=GetDeviceCaps(Printer.Handle, HORZRES);    // printer page-width in dots.
    length_dots:=GetDeviceCaps(Printer.Handle, VERTRES);   // printer page-length in dots.

    width_dpi:=GetDeviceCaps(Printer.Handle, LOGPIXELSX);  // dpi across width.
    length_dpi:=GetDeviceCaps(Printer.Handle, LOGPIXELSY); // dpi down length.
    }

    width_dots:=Printer.PageWidth;    // printer page-width in dots.    // OT-FIRST
    length_dots:=Printer.PageHeight;  // printer page-length in dots.   // OT-FIRST

    width_dpi:=Printer.XDPI;  // dpi across width.  // OT-FIRST
    length_dpi:=Printer.YDPI; // dpi down length.   // OT-FIRST


    if (width_dpi<1) or (length_dpi<1) or (width_dots<1) or (length_dots<1)   // division by zero, or negative.
       then begin
              alert(0,'   printer  software  problem ..',
                      '|||Templot0 is unable to access your printer software.'
                     +'||Please check your printer installation.',
                      '','','','','cancel  calibration','',0);
              EXIT;
            end;

    width_mm:=width_dots/width_dpi*25.4;
    length_mm:=length_dots/length_dpi*25.4;

    if (width_mm<191) or (length_mm<251) or (length_mm<width_mm)
       then begin
              i:=alert(1,'   printer  problem ..',
                         '|Printer calibration halted - there is a problem with the page size.'
                        +'||Please check your printer settings for A4 or US-Letter paper size (or larger) and Upright / Portrait orientation.'
                        +'||Then restart the printer calibration.',
                         '','','','try  printing  now  anyway','cancel','printer  setup ...',0);
              case i of
                  5: EXIT;
                  6: begin
                       printer_setup(True,False);
                       EXIT;
                     end;
              end;//case
            end;

    if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact=-1
       then begin
              if ask_impact_matrix(prindex,prstr)=False then EXIT;
            end;

    calibration_form.form_label.Caption:='printing  the  test  sheet  -  please  wait ...';
    calibration_form.Show;
    calibration_form.BringToFront;
    Application.ProcessMessages;

    l_left:=Round(10/25.4*width_dpi);      // large rectangle left in 10 mm from edge.
    l_right:=Round(190/25.4*width_dpi);    // large rectangle right for 180 mm wide.

    l_top:=Round(10/25.4*length_dpi);       // large rectangle top in 10 mm from top.
    l_bottom:=Round(250/25.4*length_dpi);   // large rectangle bottom for 240 mm long.

    s_left:=Round(55/25.4*width_dpi);      // small rectangle left in 55 mm from edge.
    s_right:=Round(145/25.4*width_dpi);    // small rectangle right for 90 mm wide.

    s_top:=Round(70/25.4*length_dpi);       // small rectangle top in 70 mm from top.
    s_bottom:=Round(190/25.4*length_dpi);   // small rectangle bottom for 120 mm long.


    of_text_left:=Round(15/25.4*width_dpi);    // outer frame text 15 mm from edge.
    if_text_left:=Round(50/25.4*width_dpi);    // inner frame text 50 mm from edge.

    of_text_top:=Round(240/25.4*length_dpi);   // outer frame text 240 mm from top.
    if_text_top:=Round(195/25.4*length_dpi);   // inner frame text 195 mm from top.

    text_top:=Round(254/25.4*length_dpi);      // bottom text position 254 mm from top.

    res_text_left:=Round(30/25.4*width_dpi);   // dpi info text 30 mm from edge.
    res_text_top:=Round(30/25.4*length_dpi);   // dpi info text 30 mm from top.

    web_note_left:=Round(50/25.4*width_dpi);   // web text 50 mm from edge.
    web_note_top:=Round(50/25.4*length_dpi);   // web text 50 mm from top.

    web_url_left:=Round(85/25.4*width_dpi);    // web url 85 mm from edge.
    web_url_top:=Round(60/25.4*length_dpi);    // web url 60 mm from top.

    hunslet_ratio:=calibration_form.cal_image.Height/calibration_form.cal_image.Width;

    picture_left:=Round(70.6/25.4*width_dpi);      // hunslet picture to be 58.8 mm wide for O gauge as scanned.
    picture_right:=Round(129.4/25.4*width_dpi);    // and centred at 100 mm from left edge.

    picture_top:=Round((130-(29.4*hunslet_ratio))/25.4*length_dpi);    // but maintain aspect ratio.
    picture_bottom:=Round((130+(29.4*hunslet_ratio))/25.4*length_dpi); // centred 130 mm from top edge.

    with Printer do begin
      with Canvas do begin

        Font.Name:='Arial';
        Font.Size:=12;
        Font.Color:=clBlack;
        Font.Style:=[];

        Pen.Color:=clBlack;
        Pen.Mode:=pmCopy;
        Pen.Style:=psSolid;
        Pen.Width:=1;

        Brush.Color:=clWhite;   // diagonal-cross filling removed 6-7-99 (paper too wet on David Beale's Epson printer).
        Brush.Style:=bsSolid;

        BeginDoc;

        Rectangle(l_left,l_top,l_right,l_bottom);          // draw outer frame.

        Brush.Color:=clWhite;
        Brush.Style:=bsSolid;
        Rectangle(s_left,s_top,s_right,s_bottom);          // draw inner frame.

        TextOut(of_text_left,of_text_top,'  outer frame :  width =                  height =                  ');
        TextOut(if_text_left,if_text_top,'  inner frame :  width =                  height =                  ');

        Font.Size:=10;
        TextOut(res_text_left,res_text_top,'  ( printer info :  '+IntToStr(width_dpi)+'  x  '+IntToStr(length_dpi)+'  dpi'
               +'     max width =  '+IntToStr(width_dots)+'  dots     max height =  '+IntToStr(length_dots)+'  dots )  ');

        TextOut(web_note_left,web_note_top,' For further information please refer to the Templot web site at');

        Font.Style:=[fsBold];
        TextOut(web_url_left,web_url_top,'templot.com');
        Font.Style:=[];

        MoveTo(l_left,l_top); LineTo(s_left,s_top);        // draw corner mitres (only for decoration!)...
        MoveTo(l_right,l_top); LineTo(s_right,s_top);
        MoveTo(l_left,l_bottom); LineTo(s_left,s_bottom);
        MoveTo(l_right,l_bottom); LineTo(s_right,s_bottom);

        if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact=0     // not impact printer or plotter.
           then begin
                  with inner_rect do begin
                    left:=picture_left;
                    right:=picture_right;
                    top:=picture_top;
                    bottom:=picture_bottom;
                  end;//with
                  StretchDraw(inner_rect,calibration_form.cal_image.Picture.Graphic);   // put hunslet picture in the frame.
                end
           else begin        // no picture for impact printer or plotter.
                  TextOut(picture_left,picture_top,'( impact printer or plotter - picture not shown )');
                end;

        Font.Style:=[fsBold];
        TextOut(l_left,text_top,'TEMPLOT :  printer calibration test sheet for  '+prstr+'  '+DateToStr(Date)+'  '+TimeToStr(Time)+'    ©');

        Font.Style:=[];
        Font.Size:=8;
        TextOut(0,0,' TEMPLOT  v: '+round_str(program_version/100,2));

        EndDoc;

      end;//with Canvas
    end;//with Printer

    calibration_form.Hide;
    Application.ProcessMessages;

    999:

      repeat
        i:=alert(4,'    printer  calibration',
                   '|Calibration test sheet now printed on'
                  +'|'+prstr
                  +'||You may choose to measure either the outer or the inner dimensions of the "picture-frame" outline which has been printed on the test sheet.',
                   '','','please  read  this  first','inner  dimensions','cancel  calibration','outer  dimensions',3);
        case i of
            3: alert_help(0,cal_check_str,'');

            4: begin    // get inner dims.

                 which_frame_str:='inner';
                 frame_info_str:='|||The <i>inner</i> dimensions of the "picture frame" outline on the printed test sheet (containing the picture) should be approximately 90 mm wide (across the width of the page)'
                                +' by approximately 120 mm high (down the length of the page).';

                 putdim('',1,'width of inner frame (across test sheet)',test_width,True,True,True,False);  // mm, no negative, no preset, no zero, don't terminate on zero.
              n:=putdim('',1,'height of inner frame (down test sheet)',test_height,True,True,True,False);  // mm, no negative, no preset, no zero, don't terminate on zero.
                 if n<>1 then run_error(162);

                 if getdims('calibration  test  sheet','',pad_form,n,od)=True
                    then begin
                           test_width:=ABS(od[0]);
                           test_height:=ABS(od[1]);
                           head_factor:=test_width/90;       // factor for width dpi.
                           roller_factor:=test_height/120;   // factor for length dpi.
                         end
                    else EXIT;
               end;

            5: EXIT;

            6: begin    // get outer dims.

                 which_frame_str:='outer';
                 frame_info_str:='|||The <i>outer</i> dimensions of the "picture frame" outline on the printed test sheet should be approximately 180 mm wide (across the width of the page)'
                                 +' by approximately 240 mm high (down the length of the page).';

                 putdim('',1,'width of outer frame (across test sheet)',test_width,True,True,True,False);  // mm, no negative, no preset, no zero, don't terminate on zero.
              n:=putdim('',1,'height of outer frame (down test sheet)',test_height,True,True,True,False);  // mm, no negative, no preset, no zero, don't terminate on zero.
                 if n<>1 then run_error(162);

                 if getdims('calibration  test  sheet','',pad_form,n,od)=True
                    then begin
                           test_width:=ABS(od[0]);
                           test_height:=ABS(od[1]);
                           head_factor:=test_width/180;       // factor for width dpi.
                           roller_factor:=test_height/240;    // factor for length dpi.
                         end
                    else EXIT;
               end;

          else run_error(139);
        end;//case
      until i<>3;

      if (head_factor<0.3) or (head_factor>3) or (roller_factor<0.3) or (roller_factor>3)     // arbitrary 3-fold limit
         then begin
                Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_calibrated:=False;
                alert(0,'    calibration  error',
                        '||Your printer calibration figures are invalid.'
                       +'||Please check your figures and do a fresh calibration (Shift+F5).',
                        '','','','','O K  -  cancel  calibration','',0);
                EXIT;
              end
         else begin

                if (head_factor<0.9) or (head_factor>1.1) or (roller_factor<0.9) or (roller_factor>1.1)     // arbitrary sensible limit
                   then begin
                          Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_calibrated:=False;

                          i:=alert(7,'    confirm  printer  calibration',
                                  '||Your printer calibration figures are unusual and may have been measured or entered incorrectly.'

                                 +frame_info_str

                                 +'||You entered for the <i>'+which_frame_str+'</i> frame on the test sheet:'
                                 +'||width: '+round_str(test_width,2)+' mm'
                                 +'|height: '+round_str(test_height,2)+' mm'

                                 +'|||Please check your figures.',
                                  '','more  information','continue  -  these  figures  are  correct','restart  printer  calibration','cancel  printer  calibration','enter  new  dimensions',2);

                          case i of

                               2: begin
                                    help(0,cal_check_str,'');
                                    goto 999;
                                  end;

                               4: begin
                                    calibration_form.Close;
                                    Application.ProcessMessages;
                                    do_cal;  // recursive
                                    EXIT;
                                  end;

                               5: EXIT;

                               6: goto 999;

                          end;//case

                        end;

                 // save the info...

                Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_head_factor:=head_factor;
                Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_roller_factor:=roller_factor;
                Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_calibrated:=True;

                page_info(True,True,False,0);     // for page outlines on pad.
                redraw(True);
                pad_form.proof_sheet_menu_entry.Click;   // do a proof sheet.
             end;

  finally
    calibration_form.Close;//Hide;
  end;//try
end;
//__________________________________________________________________________________________

procedure proof_sheet;         // print a calibration proof sheet.

const
  proof_str:string='      Calibration  Proof  Sheet'
   +'||Templot0 will print a proof sheet to confirm that the calibration of this printer for this type of paper is correct, and that templates printed'
   +' on this paper using this printer will be dimensionally accurate.'
   +'||Provided the paper is equally smooth on both sides, to save paper the proof sheet can be printed on the back of the test sheet, so that the two are kept together for future reference.'
   +'||The proof pattern is in the shape of a cross comprising :'
   +'||a horizontal bar measuring 150 mm long (across the width of the page)'
   +'||and a vertical bar measuring 250 mm long (down the length of the page).'
   +'||You should carefully check these measurements to confirm that the calibration information is correct.'
   +'||The calibration information will also be printed out, so that it can be entered again at a later date'
   +' (when you next use this type of paper on this printer) without the need to do a fresh calibration.'
   +'||If anything has been printed since the calibration test sheet, check before continuing that your printer is set for A4 or US-Letter page size (or larger),'
   +' Upright/Portrait orientation and the same print quality settings which you used for the test sheet.';

var
  i,j:integer;
  prindex:integer;
  prstr:string;

begin
  repeat
    if get_prindex(prindex)=False then EXIT;         // get current printer index or none available.
    prstr:=printer_list.Strings[prindex];

    if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_calibrated=True
       then begin
              repeat
                i:=alert(7,'    calibration  proof  sheet',
                           '|A proof sheet can be printed on :'
                          +'||'+prstr
                          +'||to confirm the accuracy of the calibration.',
                           '','more  information','save  calibration  file  now','change  printer  ( printer  setup ...)','cancel  proof  sheet','print  proof  sheet',2);
                case i of
                    2: alert_help(0,proof_str+'|||'+save_help_str,'');
                    3: begin
                         calibration_save(prindex);
                         EXIT;
                       end;
                    4: printer_setup(False,False);
                    5: EXIT;
                    6: do_proof;
                  else run_error(190);
                end;//case
              until i<>2;
            end
       else begin
              i:=0;       // repeat proof sheet after printer change
              repeat
                j:=alert(6,'    calibration  proof  sheet',
                           '||There is no calibration information for'
                           +'|'+prstr
                           +'||A proof sheet cannot be printed. If this is your intended printer for Templot0 you should calibrate it now.',
                            '','','more  information','change  printer  ( printer  setup ...)','cancel','calibrate  this  printer  now',3);
                case j of
                    3: alert_help(0,cal_prompt_str,'');
                    4: printer_setup(False,False);
                    5: EXIT;
                    6: begin
                         i:=6;                                         // don't repeat if he's calibrating now.
                         pad_form.calibrate_printer_menu_entry.Click;
                       end;
                  else run_error(191);
                end;//case
              until j<>3;
            end;
  until i>4;
end;
//________________________________________________________________________________________

procedure do_proof;                  // print proof sheet.

const   //  WINAPI GetDeviceCaps Constants

   HORZRES         =   8;
   VERTRES         =  10;
   BITSPIXEL       =  12;
   LOGPIXELSX      =  88;
   LOGPIXELSY      =  90;

var
  cur_cal:Tcal_data;
  n:integer;
  i,j:integer;
  prstr:string;
  prindex:integer;

  width_dots:integer;    // printer page-width in dots.
  length_dots:integer;   // printer page-length in dots.

  width_dpi:integer;     // reported dpi across width.
  length_dpi:integer;    // reported dpi down length.

  cal_width_dpi:extended;   // calibrated dpi...
  cal_length_dpi:extended;

  width_mm:extended;     // printer page-width in mm.
  length_mm:extended;    // printer page-length in mm.

  vertbar_left:integer;      // vertical bar left X.
  vertbar_right:integer;     // vertical bar right X.
  vertbar_top:integer;       // vertical bar top Y.
  vertbar_bottom:integer;    // vertical bar bottom Y.

  horzbar_left:integer;      // horizontal bar left X.
  horzbar_right:integer;     // horizontal bar right X.
  horzbar_top:integer;       // horizontal bar top Y.
  horzbar_bottom:integer;    // horizontal bar bottom Y.

  label_left, label_top, rf_notes_left, hf_notes_left, f_notes_top, d_notes_left, d_notes_top:integer;    // text positions.

  funny_size:boolean;

begin
  try
    if get_prindex(prindex)=False then EXIT;                        // get current printer index or none available.
    cur_cal:=Tprint_cal(printer_list.Objects[prindex]).cal_data;
    prstr:=printer_list.Strings[prindex];

    if (cur_cal.printer_calibrated=False) or (cur_cal.printer_head_factor<minfp) or (cur_cal.printer_roller_factor<minfp)
       then begin
              alert(0,'      proof  sheet  error',
                      '||Sorry, an internal error has occurred in Templot0.'
                     +'||Unable to print proof sheet.'
                     +'||Please quote error code 193',
                     '','','','','cancel  proof  sheet','',0);
              EXIT;
            end;

    Printer.Orientation:=poPortrait;
    if Application.Terminated=False then Application.ProcessMessages;

    { OT-FIRST
    width_dots:=GetDeviceCaps(Printer.Handle, HORZRES);    // printer page-width in dots.
    length_dots:=GetDeviceCaps(Printer.Handle, VERTRES);   // printer page-length in dots.

    width_dpi:=GetDeviceCaps(Printer.Handle, LOGPIXELSX);  // dpi across width.
    length_dpi:=GetDeviceCaps(Printer.Handle, LOGPIXELSY); // dpi down length.
    }

    width_dots:=Printer.PageWidth;    // printer page-width in dots.    // OT-FIRST
    length_dots:=Printer.PageHeight;  // printer page-length in dots.   // OT-FIRST

    width_dpi:=Printer.XDPI;  // dpi across width.  // OT-FIRST
    length_dpi:=Printer.YDPI; // dpi down length.   // OT-FIRST

    if (width_dpi<1) or (length_dpi<1) or (width_dots<1) or (length_dots<1)   // division by zero, or negative.
       then begin
              alert(0,'   printer  software  problem ..',
                      '|||Templot0 is unable to access your printer software.'
                     +'||Please check your printer installation.',
                      '','','','','cancel  proof  sheet','',0);
              EXIT;
            end;

    cal_width_dpi:=width_dpi/cur_cal.printer_head_factor;         // calibrated dpi.
    cal_length_dpi:=length_dpi/cur_cal.printer_roller_factor;

    width_mm:=width_dots/cal_width_dpi*25.4;                      // calibrated page sizes.
    length_mm:=length_dots/cal_length_dpi*25.4;

    if (width_mm<177) or (length_mm<257) or (length_mm<width_mm)
       then begin
              funny_size:=True;
              i:=alert(1,'   printer  problem ..',
                         '|Printer proof sheet halted - there is a problem with the page size.'
                        +'||Please check your printer settings for A4 or US-Letter paper size (or larger) and Upright / Portrait orientation.'
                        +'||Then restart printing the proof sheet.',
                         '','','','try  printing  now  anyway','cancel','printer  setup ...',0);
              case i of
                  5: EXIT;
                  6: begin
                       printer_setup(True,False);
                       EXIT;
                     end;
              end;//case
            end
       else funny_size:=False;

    if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact=-1
       then begin
              if ask_impact_matrix(prindex,prstr)=False then EXIT;
            end;

    calibration_form.form_label.Caption:='printing  the  proof  sheet  -  please  wait ...';
    calibration_form.Show;
    calibration_form.BringToFront;

    vertbar_left:=Round(95/25.4*cal_width_dpi);      // vertical bar left 95 mm from edge.
    vertbar_right:=Round(105/25.4*cal_width_dpi);    // vertical bar right for 10 mm thick bar.

    vertbar_top:=Round(8/25.4*cal_length_dpi);       // vertical bar top 8 mm from top.
    vertbar_bottom:=Round(258/25.4*cal_length_dpi);  // vertical bar bottom for 250 mm long bar.

    horzbar_left:=Round(25/25.4*cal_width_dpi);      // horizontal bar left 25 mm from edge.
    horzbar_right:=Round(175/25.4*cal_width_dpi);    // horizontal bar right for 150 mm long bar.

    horzbar_top:=Round(128/25.4*cal_length_dpi);     // horizontal bar top 128 mm from top.
    horzbar_bottom:=Round(138/25.4*cal_length_dpi);  // horizontal bar bottom for 10 mm thick bar.

    label_left:=Round(5/25.4*cal_width_dpi);         // label left 5 mm from edge.
    label_top:=Round(1/25.4*cal_length_dpi);         // label top 1 mm from top.

    hf_notes_left:=Round(15/25.4*cal_width_dpi);     // head-factor notes left 15 mm from edge.
    rf_notes_left:=Round(115/25.4*cal_width_dpi);    // roller-factor notes left 115 mm from edge.
    f_notes_top:=Round(20/25.4*cal_length_dpi);      // factor notes top 20 mm from top.

    d_notes_left:=Round(115/25.4*cal_width_dpi);     // destroy notes left 115 mm from edge.
    d_notes_top:=Round(205/25.4*cal_length_dpi);     // destroy notes top 205 mm from top.

    with Printer do begin
      with Canvas do begin

        Font.Name:='Arial';
        Font.Color:=clBlack;

        Pen.Color:=clBlack;
        Pen.Mode:=pmCopy;
        Pen.Style:=psSolid;
        Pen.Width:=1;

        BeginDoc;

        if funny_size=True
           then begin

                  Brush.Color:=clWhite;
                  Brush.Style:=bsSolid;

                  n:=0;
                  while n<=250 do begin
                    MoveTo(Round(n/25.4*cal_width_dpi),0);                               // vertical grid bar top.
                    LineTo(Round(n/25.4*cal_width_dpi),Round(300/25.4*cal_length_dpi));  // vertical grid bar bottom.
                    n:=n+25;
                  end;//while

                  n:=0;
                  while n<=300 do begin
                    MoveTo(0,Round(n/25.4*cal_length_dpi));                              // horizontal grid bar left.
                    LineTo(Round(250/25.4*cal_width_dpi),Round(n/25.4*cal_length_dpi));  // horizontal grid bar right.
                    n:=n+25;
                  end;//while

                  Font.Size:=14;
                  Font.Style:=[fsBold];
                  TextOut(TextWidth('W'),TextHeight('|')*3,' ! ! !  PAGE  SIZE  PROBLEM  -  CHECK  25 mm  GRID  ONLY');
                end
           else begin
                  if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact<1      // inkjet/laser printer or not known.
                     then begin
                            Brush.Color:=clBlack;
                            Brush.Style:=bsFDiagonal;
                          end
                     else begin                     // don't cross-hatch if impact printer or plotter - too noisy or slow.
                            Brush.Color:=clWhite;
                            Brush.Style:=bsSolid;
                          end;
                  Rectangle(vertbar_left,vertbar_top,vertbar_right,vertbar_bottom);   // draw vertical bar.

                  if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact<1      // inkjet/laser printer or not known.
                     then Brush.Style:=bsBDiagonal;

                  Rectangle(horzbar_left,horzbar_top,horzbar_right,horzbar_bottom);   // draw horizontal bar.

                  Brush.Color:=clWhite;       // text white background.
                  Brush.Style:=bsSolid;

                  //------------------------

                  Font.Size:=12;
                  n:=Round(TextHeight('ABC')*1.1);        // get line spacing (+10%).
                  i:=f_notes_top;                         // text top.
                  Font.Style:=[];
                          TextOut(rf_notes_left,i,'If the vertical bar is exactly');
                  i:=i+n; TextOut(rf_notes_left,i,'250 mm long');
                  i:=i+n; TextOut(rf_notes_left,i,'then the roller-factor for');
                  i:=i+n; TextOut(rf_notes_left,i,prstr);
                  i:=i+n; TextOut(rf_notes_left,i,'and this paper only is');

                  Font.Size:=14;
                  Font.Style:=[fsBold];
                  i:=i+n;
                  i:=i+n; TextOut(rf_notes_left,i,'roller-factor =  '+FormatFloat('0.00',cur_cal.printer_roller_factor*100)+'  %');

                  Font.Size:=12;
                  Font.Style:=[];
                  i:=i+n;
                  i:=i+n;
                  i:=i+n; TextOut(rf_notes_left,i,'If the vertical bar is not');
                  i:=i+n; TextOut(rf_notes_left,i,'exactly 250 mm long');
                  i:=i+n; TextOut(rf_notes_left,i,'then the printer should be');
                  i:=i+n; TextOut(rf_notes_left,i,'recalibrated (Shift + F5).');

                  //----------------------

                  Font.Size:=12;
                  n:=Round(TextHeight('ABC')*1.1);        // get line spacing (+10%).
                  i:=f_notes_top;                         // text top.
                  Font.Style:=[];
                          TextOut(hf_notes_left,i,'If the horizontal bar is exactly');
                  i:=i+n; TextOut(hf_notes_left,i,'150 mm long');
                  i:=i+n; TextOut(hf_notes_left,i,'then the head-factor for');
                  i:=i+n; TextOut(hf_notes_left,i,prstr);
                  i:=i+n; TextOut(hf_notes_left,i,'and this paper only is');

                  Font.Size:=14;
                  Font.Style:=[fsBold];
                  i:=i+n;
                  i:=i+n; TextOut(hf_notes_left,i,'head-factor =  '+FormatFloat('0.00',cur_cal.printer_head_factor*100)+'  %');

                  Font.Size:=12;
                  Font.Style:=[];
                  i:=i+n;
                  i:=i+n;
                  i:=i+n; TextOut(hf_notes_left,i,'If the horizontal bar is not');
                  i:=i+n; TextOut(hf_notes_left,i,'exactly 150 mm long');
                  i:=i+n; TextOut(hf_notes_left,i,'then the printer should be');
                  i:=i+n; TextOut(hf_notes_left,i,'recalibrated (Shift + F5).');

                  //---------------------

                  Font.Size:=12;
                  n:=Round(TextHeight('ABC')*1.1);        // get line spacing (+10%).
                  i:=d_notes_top;                         // text top.
                  Font.Style:=[];
                          TextOut(d_notes_left,i,'If both dimensions are correct');
                  i:=i+n; TextOut(d_notes_left,i,'this sheet can be kept for reference.');
                  i:=i+n;
                  i:=i+n; TextOut(d_notes_left,i,'If not, it should be destroyed');
                  i:=i+n; TextOut(d_notes_left,i,'to avoid future mistakes.');
                  i:=i+n;
                  i:=i+n; TextOut(d_notes_left,i,'For further information');
                  i:=i+n; TextOut(d_notes_left,i,'refer to the Templot web site at');

                  Font.Style:=[fsBold];
                  i:=i+n+(n div 2); TextOut(d_notes_left,i,'templot.com');
                  Font.Style:=[];
                  Font.Size:=8;
                  TextOut(0,i,' TEMPLOT  v: '+round_str(program_version/100,2));
                end;

        Font.Size:=10;
        Font.Style:=[fsBold];
        TextOut(label_left,label_top,'TEMPLOT :  printer calibration proof sheet for  '+prstr+'  '+DateToStr(Date)+'  '+TimeToStr(Time)+'    ©');

        EndDoc;

      end;//with Canvas
    end;//with Printer

    calibration_form.Hide;

    repeat
      i:=alert(4,'      calibration  proof  sheet',
               '||Carefully measure the two bars forming a cross on the proof sheet.'
              +'||The vertical bar should be exactly 250 mm long.'
              +'||The horizontal bar should be exactly 150 mm long.'
              +'||Are these lengths correct ?',
               '','','','?  help','no  -  re-calibrate  printer','yes  -  printer  calibration  successful',4);
      case i of
          4: alert_help(0,'If the proof sheets shows "PAGE SIZE PROBLEM", Templot0 has been unable to draw the normal proof sheet on this printer'
                         +' because the page size is too small to accommodate it. Was the paper size set to A4 or LETTER (or larger) in your printer setup?'
                         +'||Ignore the instructions about measuring the bars of a cross, but check that the drawn grid lines are exactly 25 mm apart in both directions.'
                         +'||If so, click PRINTER CALIBRATION SUCCESSFUL.'
                         +'||If they are not exactly 25 mm apart, it is likely that the calibration figures were incorrectly entered.'
                         +' Click RE-CALIBRATE PRINTER and start again.','');
          5: begin
              Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_calibrated:=False;
              page_info(True,True,False,0);                         // for page outlines on pad.
              redraw(True);
              pad_form.calibrate_printer_menu_entry.Click;
             end;
          6: repeat
              j:=alert(3,'      calibration  finished',
                         '||'+prstr
                        +'||is now calibrated, ready to print accurate templates.'
                        +'||Do you want to save these settings in a new calibration file?',
                          '','','','?  help','no  -  continue','yes  -  save  calibration  file',4);
              case j of
                   4: alert_help(0,save_help_str,'');
                   6: calibration_save(prindex);
              end;//case
            until j<>4;
      end;//case
    until i<>4;
  finally
    calibration_form.Close;//Hide;
  end;//try
end;
//______________________________________________________________________________________

procedure calibration_load(prindex:integer);   // get calibration from file.

var
  data:Tcal_data;
  cal_file: file of Tcal_data;
  cal_str:string;
  prstr:string;

begin
  prstr:=printer_list.Strings[prindex];

  with calibration_form.open_dialog do begin         // set up the load dialog.
    Title:='    reload  calibration  for  '+prstr;
    InitialDir:=ExtractFilePath(Filename);           // use his previous folder.
    if InitialDir='' then InitialDir:=exe_str;       // or the default one.
    Filter:= ' calibration  file  (*.otcal)|*.otcal';
    Filename:='*.otcal';
  end;//with

  if calibration_form.open_dialog.Execute=True                // get his file name.
     then begin
            cal_str:=calibration_form.open_dialog.FileName;

            AssignFile(cal_file,cal_str);                     // set the file name.
            Reset(cal_file);                                  // open a new file.
            Read(cal_file,data);                              // get the data.
            CloseFile(cal_file);                              // close file.

            if data.printer_name<>Copy(prstr,1,20)            // check printer name.
               then begin
                      alert(5,'   calibration  file  mismatch',
                              '||Sorry, unable to load calibration for :||'
                             +prstr
                             +'||from :||'
                             +cal_str
                             +'||This file does not match this printer.',
                             '','','','','cancel','',0);
                      EXIT;
                    end;

            Tprint_cal(printer_list.Objects[prindex]).cal_data:=data;
          end;
end;
//____________________________________________________________________________________

procedure calibration_save(prindex:integer);   // save calibration to file.

var
  data:Tcal_data;
  cal_file: file of Tcal_data;
  cal_str:string;
  prstr:string;

begin
  if printer_list.Count<1
     then begin
            alert(6,'      no  printers',
                    '||||Templot0 has not found any printers on your system.',
                    '','','','','cancel','',0);
            EXIT;
          end;
  if prindex>=printer_list.Count then run_error(169);

  prstr:=printer_list.Strings[prindex];

  if Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_impact=-1
     then begin
            if ask_impact_matrix(prindex,prstr)=False then EXIT;
          end;

  Tprint_cal(printer_list.Objects[prindex]).cal_data.printer_name:=Copy(prstr,1,20); // save printer name for check.

  with calibration_form.save_dialog do begin         // set up the save dialog.
    Title:='    save  calibration  for  '+prstr;
    InitialDir:=ExtractFilePath(Filename);           // use his previous folder.
    if InitialDir='' then InitialDir:=exe_str;       // or the default one.
    Filter:= ' calibration  file  (*.otcal)|*.otcal';
    Filename:=remove_invalid_str(Copy(Trim(prstr),1,22))+' - papertype.otcal';
  end;//with

  if calibration_form.save_dialog.Execute=True                // get his file name.
     then begin
            cal_str:=calibration_form.save_dialog.FileName;

            if invalid_85a_file_name(cal_str)=True then EXIT;

            AssignFile(cal_file,cal_str);                              // set the file name.
            Rewrite(cal_file);                                         // open a new file.
            data:=Tprint_cal(printer_list.Objects[prindex]).cal_data;  // get the data for this printer.
            Write(cal_file,data);                                      // write data to the file.
            CloseFile(cal_file);                                       // close file.
          end;
end;
//____________________________________________________________________________________

procedure Tcalibration_form.FormCreate(Sender: TObject);

begin

  ClientWidth:=392;
  ClientHeight:=56;
  AutoScroll:=False;
end;
//________________________________________________________________________________________

end.

