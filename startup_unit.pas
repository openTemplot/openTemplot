
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

unit startup_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, ShellAPI, Math,

  Menus, // 0.95.a

  TypInfo,  // 214c   for GetObjectProp

  Printers;  // 0.93.a

type

  { Told_startup_form }

  Told_startup_form = class(TForm)
    starting_text_static: TStaticText;
    version_label: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  old_startup_form: Told_startup_form;

  global_factor:extended=1.0;

  //--------------------------

  { OT-FIRST

  procedure abandon_if_existing_instance;

  procedure force_printer_to_init;    // 0.93.a

  procedure set_menu_style(win7:boolean);    // 0.95.a
  }


  procedure minimize_consolehost_window;      // 291a

  procedure do_dpi_aware_scaling(scaling_size:integer);   // 211b

  procedure detect_wine; // 205a


implementation

uses
  Registry, styleun, control_room, pad_unit, alert_unit, help_sheet, panning_unit, mint_unit, info_unit, { OT-FIRST dtp_unit,} entry_sheet,
  { OT-FIRST file_viewer,} gauge_unit, bgnd_unit, { OT-FIRST sb_rvf_unit, sb_rvf_outer_unit, edit_outline_unit, dtp_settings_unit,} keep_select, { OT-FIRST web_browser_unit,}
  make_slip_unit;


{$BOOLEVAL ON}

{$R *.lfm}

{ OT-FIRST

procedure abandon_if_existing_instance;    // 24-7-01.

var
  window_handle:integer;
  class_name_str:string;
  app_name_str:string;

                          ///////////////////////////////////////////////

                          procedure ask_if_multiple;

                          var
                            msg_str:string;

                          begin
                            if Win32Platform=VER_PLATFORM_WIN32_NT   // running under Windows NT/XP...
                               then begin

                                      msg_str:='A Templot session is already running on this computer.'
                                               +Chr(13)+'______________________________________________'
                                               +Chr(13)+Chr(13)
                                               +'You are using WindowsNT, 2000, XP, Vista, Windows7, Windows8, Windows8.1 or Windows10.'
                                               +Chr(13)+Chr(13)
                                               +'Multiple Templot sessions are permissible, but each should normally be run from separate copies of the templot_2.exe program file in different folders,'
                                               +' otherwise the "restore previous" functions will not work as intended. You will probably want to use different screen colours for each session to avoid confusion.'
                                               +Chr(13)+'______________________________________________'
                                               +Chr(13)+Chr(13)
                                               +'(When using Windows95/98/ME you are strongly advised not to start a second session of Templot.'
                                               +' Templot needs a substantial proportion of the available Windows resources, and running two Templot sessions concurrently on Windows95/98/ME may cause your computer to stop responding.)'
                                               +Chr(13)+'______________________________________________'
                                               +Chr(13)+Chr(13)+'Do you want to start another Templot session?';


                                    end
                               else begin
                                      msg_str:='A Templot session is already running on this computer.'
                                               +Chr(13)+'______________________________________________'
                                               +Chr(13)+Chr(13)
                                               +'You are using Windows 95, 98 or ME and you are strongly advised not to start a second session of Templot - click "No".'
                                               +' Templot needs a substantial proportion of the available Windows resources, and running two Templot sessions concurrently may cause your computer to stop responding.'
                                               +Chr(13)+'______________________________________________'
                                               +Chr(13)+Chr(13)
                                               +'(When using WindowsNT/2000/XP/Vista/Windows7/Windows8/Windows8.1/Windows10 multiple Templot sessions are permissible,'
                                               +' but each should normally be run from separate copies of the templot_2.exe program file in different folders,'
                                               +' otherwise the "restore previous" functions will not work as intended.)'
                                               +Chr(13)+'______________________________________________'
                                               +Chr(13)+Chr(13)+'Do you want to start another Templot session?';
                                    end;


                            if Application.MessageBox(PChar(msg_str), '  start  TEMPLOT  again ?', mb_YesNo + mb_DefButton2)=IDYES
                               then EXIT;

                            Halt(0);
                          end;
                          //////////////////////////////////////////////////////


begin

  app_name_str:='TEMPLOT2';           // must not be the same as executable: templot_2.exe
  SetLength(class_name_str, 255);
  GetClassName(Application.Handle, PChar(class_name_str), 255);
  window_handle:=FindWindow(PChar(class_name_str), PChar(app_name_str));
  if window_handle<>0
     then begin
            ask_if_multiple;
            EXIT;
          end;

  app_name_str:='TEMPLOT';
  SetLength(class_name_str, 255);
  GetClassName(Application.Handle, PChar(class_name_str), 255);
  window_handle:=FindWindow(PChar(class_name_str), PChar(app_name_str));
  if window_handle<>0
     then ask_if_multiple;

end;
}

//______________________________________________________________________________________

{ OT-FIRST

procedure force_printer_to_init;    // 0.93.a

begin

  if Printer=nil then EXIT;  // this forces Printer to be created.

  if Printer.Printers=nil then EXIT;  // this forces list of printers to be created.
end;

}
//______________________________________________________________________________

procedure Told_startup_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=616;
  // OT-FIRST ClientHeight:=150;

  version_label.Caption := GetVersionString(voFull);

  // OT-FIRST if Screen.PixelsPerInch>120 then ScaleBy(4,3);  // 211b

end;
//______________________________________________________________________________

{ OT-FIRST

procedure set_menu_style(win7:boolean);    // 0.95.a

var
  i,j:integer;
  temp_comp_i,temp_comp_j:TComponent;

begin

      // 0.95.a  add a dummy image to any line menu items. This causes the whole menu to display selected items in XP style inverse video (Windows 7 kludge).

  for j:=Application.ComponentCount-1 downto 0 do begin

    temp_comp_j:=Application.Components[j];

    if (temp_comp_j is TForm)

      then begin

             for i:=temp_comp_j.ComponentCount-1 downto 0 do begin

               temp_comp_i:=temp_comp_j.Components[i];

               if (temp_comp_i is TMenuItem)
                  then begin
                         if  (TMenuItem(temp_comp_i).Caption='-')
                         and (TMenuItem(temp_comp_i).Parent<>pad_form.set_peg_position_menu_entry)   // D5 bug fix, apparently not needed if menu contains a break
                            then begin
                                   if win7=True
                                      then TMenuItem(temp_comp_i).Bitmap:=nil
                                      else TMenuItem(temp_comp_i).Bitmap:=control_room_form.menu_kludge_image.Picture.Bitmap;
                                 end;
                       end;


             end;//next i

           end;

  end;//next j
end;

}
//______________________________________________________________________________

procedure do_dpi_aware_scaling(scaling_size:integer);   // 211b

  // if scaling_size 0, get it from file

var
  n,dpi_setting:integer;
  i,j,right_edge,right_max,bottom_edge,bottom_max:integer;
  temp_comp,child_comp:TComponent;
  temp_control:TControl;

  sz_str:string;
  sz_list:TStringList;

  fracmul,fracdiv:integer;
  factor:extended;

  rem_width:integer;

  control_font:TFont;

begin

  dpi_setting:=Screen.PixelsPerInch;   // Windows scaling begins at 144dpi, no effect at 143dpi (Windows 7)
                                       // Windows scaling begins at 120dpi, no effect at 119dpi (Windows 8)


  companion_viewer_str:=ExtractFilePath(Application.ExeName)+'companion_viewer.exe';     // 215a

  if Application.ComponentCount<1 then EXIT;  // ???

     // mods 214c ...

  if scaling_size=0
     then begin       // get from file

            scaling_size:=4;  // init normal medium program size

            sz_str:=ExtractFilePath(Application.ExeName)+'internal\dpi\sz.szx';

            if FileExists(sz_str)
               then begin
                      sz_list:=TStringList.Create;
                      sz_list.LoadFromFile(sz_str);

                      try
                        scaling_size:=StrToInt(sz_list.Strings[0]);
                      except
                        scaling_size:=4;  // normal medium program size
                      end;//try

                      sz_list.Free;
                    end;

            if (scaling_size<1) or (scaling_size>7) then scaling_size:=4;   // corrupted file?

          end;

  current_scaling_position:=scaling_size;      // globals for pad slider
  old_scaling_position:=scaling_size;
  pad_form.scaling_trackbar.Position:=scaling_size;

{
         1: str:='largest';    // top to bottom of trackbar
         2: str:='larger';
         3: str:='large';
         4: str:='medium';
         5: str:='small';
         6: str:='smaller';
         7: str:='smallest';
}

  case scaling_size of       // arbitrary fractions ...

         1: begin fracmul:=17; fracdiv:=10; factor:=1.7; end; // largest
         2: begin fracmul:=14; fracdiv:=10; factor:=1.4; end; // larger
         3: begin fracmul:=7; fracdiv:=6; factor:=7/6;   end; // large     1.167

         4: begin fracmul:=1; fracdiv:=1; factor:=1.0;   end; // medium  do nothing

         5: begin fracmul:=39; fracdiv:=40; factor:=39/40; end; // small      0.975
         6: begin fracmul:=20; fracdiv:=23; factor:=20/23; end; // smaller    0.869
         7: begin fracmul:=10; fracdiv:=13; factor:=10/13; end; // smallest   0.769

       else begin fracmul:=1; fracdiv:=1; factor:=1; end; // keep compiler happy

  end;//case

  global_factor:=factor;

  if scaling_size=4 then EXIT;  // nothing to do

  scaling_done_at_least_once:=True;  // global for slider

     // HTML scaling ...

  help_client_width_as_user:=Round(530*factor);  // defaults unless resized by user.
  help_client_height_as_user:=Round(680*factor);

  case scaling_size of       // after trial and error ...

         1: html_pixels_per_inch:=144; // largest
         2: html_pixels_per_inch:=131; // larger
         3: html_pixels_per_inch:=118; // large

         4: html_pixels_per_inch:=108;  // shouldn't get here, 108 set in help_sheet

         5: html_pixels_per_inch:=105; // small
         6: html_pixels_per_inch:=97;  // smaller
         7: html_pixels_per_inch:=90;  // smallest

  end;//case

  // now scale the forms ...

  with pad_form do begin

    top_toolbar_panel.ScaleBy(fracmul,fracdiv);

    second_toolbar_panel.ScaleBy(fracmul,fracdiv);      // 217a

    with output_mode_panel          do begin ScaleBy(fracmul,fracdiv); Left:=Round(Left*factor); Top:=Round(Top*factor); end;//with
    with group_linked_warning_panel do begin ScaleBy(fracmul,fracdiv); Left:=Round(Left*factor); Top:=Round(Top*factor); end;//with
    with program_warning_panel      do begin ScaleBy(fracmul,fracdiv); Left:=Round(Left*factor); Top:=Round(Top*factor); end;//with
    with slewing_panel              do begin ScaleBy(fracmul,fracdiv); Left:=Round(Left*factor); Top:=Round(Top*factor); end;//with
    with distortion_warning_panel   do begin ScaleBy(fracmul,fracdiv); Left:=Round(Left*factor); Top:=Round(Top*factor); end;//with
    with reorg_warning_panel        do begin ScaleBy(fracmul,fracdiv); Left:=Round(Left*factor); Top:=Round(Top*factor); end;//with
    with dummy_label_panel          do begin ScaleBy(fracmul,fracdiv); Left:=Round(Left*factor); Top:=Round(Top*factor); end;//with

    with reminder_memo1             do begin rem_width:=Width; ScaleBy(fracmul,fracdiv); Left:=Left-Width+rem_width; Top:=Round(Top*factor); end;//with
    with reminder_memo2             do begin rem_width:=Width; ScaleBy(fracmul,fracdiv); Left:=Left-Width+rem_width; Top:=Round(Top*factor); end;//with
    with reminder_memo3             do begin rem_width:=Width; ScaleBy(fracmul,fracdiv); Left:=Left-Width+rem_width; Top:=Round(Top*factor); end;//with
    with reminder_memo4             do begin rem_width:=Width; ScaleBy(fracmul,fracdiv); Left:=Left-Width+rem_width; Top:=Round(Top*factor); end;//with
    with reminder_memo5             do begin rem_width:=Width; ScaleBy(fracmul,fracdiv); Left:=Left-Width+rem_width; Top:=Round(Top*factor); end;//with


    scaling_trackbar.ScaleBy(fracmul,fracdiv);
    scaling_labels_panel.ScaleBy(fracmul,fracdiv);
    scaling_help_static.ScaleBy(fracmul,fracdiv);

    scaling_trackbar.Left:=ClientWidth-scaling_trackbar.Width;
    scaling_trackbar.Top:=ClientHeight-scaling_trackbar.Height;

    scaling_labels_panel.Left:=scaling_trackbar.Left-scaling_labels_panel.Width;
    scaling_labels_panel.Top:=ClientHeight-scaling_labels_panel.Height;

    scaling_help_static.Left:=ClientWidth-scaling_help_static.Width;
    scaling_help_static.Top:=scaling_trackbar.Top-scaling_help_static.Height;

  end;//with

{ OT-FIRST

  dtp_form.top_panel.ScaleBy(fracmul-1,fracdiv);   // -1 a bit smaller   was (4,3); // default is 930 * 27 -- now 1240 * 36

  dtp_form.ClientWidth:=dtp_form.top_panel.Width;               // does a sketchboard resize
  dtp_form.ClientHeight:=Round(dtp_form.ClientWidth*688/930);   // default size 930 x 688

  dtp_form.dtp_document.ZoomWidth;

  update_model_rulers;
}

    // and the rest of them ...

  for n:=Application.ComponentCount-1 downto 0 do begin

    temp_comp:=Application.Components[n];

    if  ((temp_comp is TForm)=True)
    and ((temp_comp is Tpad_form)=False)  // not the pad otherwise child windows get scaled twice. Panels already done above.

    // OT-FIRST    and ((temp_comp is Tdtp_form)=False)  // not the sketchboard already done. otherwise rulers are wrecked

    // OT-FIRST    and ((temp_comp is Tweb_browser_form)=False)  // not the map browser   // 214c

    // OT-FIRST    and ((temp_comp is Tsb_rich_form)=False)  // 212a  RVF inner form is child of outer form.

    // OT-FIRST    and ((temp_comp is Tedit_outline_form)=False)  // 212a  child of dtp_settings form.

      then begin

             with TForm(Application.Components[n]) do begin

               ScaleBy(fracmul,fracdiv);   // scale the form

                  // revise the client size ...

               right_max:=0;   // init
               bottom_max:=0;

               if ControlCount>0
                  then begin

                         for i:=0 to ControlCount-1 do begin

                           if TControl(Controls[i]).Align<>alNone
                              then CONTINUE;

                           right_edge:=TControl(Controls[i]).Left+TControl(Controls[i]).Width;
                           bottom_edge:=TControl(Controls[i]).Top+TControl(Controls[i]).Height;

                           if right_max<right_edge then right_max:=right_edge;
                           if bottom_max<bottom_edge then bottom_max:=bottom_edge;

                         end;//next control

                         TForm(Application.Components[n]).ClientHeight:=bottom_max;
                         TForm(Application.Components[n]).ClientWidth:=right_max;
                         TForm(Application.Components[n]).ClientHeight:=bottom_max;
                         TForm(Application.Components[n]).ClientWidth:=right_max;
                       end;

             end;//with
           end;
  end;//next

    // final tweaks ...

  for n:=Application.ComponentCount-1 downto 0 do begin    // find all the forms

    temp_comp:=Application.Components[n];
    if (temp_comp is TForm)=False then CONTINUE;

    with TForm(temp_comp) do begin

      for i:=ComponentCount-1 downto 0 do begin    // check tickboxes not too small to display properly by Windows

        child_comp:=Components[i];

        if (child_comp is TCheckBox)
           then begin
                  if TCheckBox(child_comp).Height<19 then TCheckBox(child_comp).Height:=19;
                end;

        if (child_comp is TRadioButton)
           then begin
                  if TRadioButton(child_comp).Height<19 then TRadioButton(child_comp).Height:=19;
                end;

      end;//next
    end;//with
  end;//next

     // 214c reduce all font sizes after ScaleBy (which rounds up)...

  for n:=Application.ComponentCount-1 downto 0 do begin    // find all the forms

    temp_comp:=Application.Components[n];
    if (temp_comp is TForm)=False then CONTINUE;

    with TForm(temp_comp) do begin

      for i:=ComponentCount-1 downto 0 do begin

        child_comp:=Components[i];

        if (child_comp is TControl)
           then begin
                  if IsPublishedProp(child_comp,'Font')
                     then begin
                            control_font:=TFont(GetObjectProp(child_comp,'Font',TFont));
                            control_font.Height:=control_font.Height+1;                  // go down in size (Height is negative).   using Height because Size is determined by DPI on compiling system
                          end;
                end;
      end;//next
    end;//with
  end;//next

  with make_slip_form do begin  //217a

    ClientWidth:=Button2.Left+Button2.Width;
    ClientHeight:=Button2.Top+Button2.Height;

  end;//with

  do_toolbars;  // 217a

end;
//______________________________________________________________________________

procedure minimize_consolehost_window;      // 291a

var
  //window_handle:HWND;
  title1_str,title2_str:string;      // Pchar needs a unique local string

  titlet3_str:string;
  titlemec_str:string;

begin

  Application.ProcessMessages;

  titlet3_str:='Templot3';
  titlemec_str:='TemplotMEC';
  title1_str:=Application.Title;
  title2_str:=Application.ExeName;

  ShowWindow(FindWindow(nil,PChar(titlet3_str)),SW_MINIMIZE);
  ShowWindow(FindWindow(nil,PChar(titlemec_str)),SW_MINIMIZE);
  ShowWindow(FindWindow(nil,PChar(title1_str)),SW_MINIMIZE);
  ShowWindow(FindWindow(nil,PChar(title2_str)),SW_MINIMIZE);

end;
//______________________________________________________________________________

procedure detect_wine;    // 205a

var
  reg:TRegistry;

begin

  Application.ProcessMessages;

  control_room_form.WindowState:=wsNormal;

  running_under_wine:=False;   // init

  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_CURRENT_USER;
    running_under_wine:=reg.KeyExists('\Software\Wine');
  finally
    reg.Free;
  end;

end;
//______________________________________________________________________________

end.

