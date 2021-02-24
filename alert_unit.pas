
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

unit alert_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Clipbrd,
  ExtCtrls, StdCtrls, ComCtrls, Spin, Htmlview, Menus, Buttons, HtmlGlobals;

type

  { Talert_box }

  Talert_box = class(TForm)
    alert_header_label: TLabel;
    alert_panel1: TPanel;
    alert_panel2: TPanel;
    alert_panel3: TPanel;
    alert_panel4: TPanel;
    alert_panel5: TPanel;
    alert_panel6: TPanel;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    chat_panel: TPanel;
    colour_panel: TPanel;
    colour_patch: TImage;
    size_updown: TUpDown;
    reply_button_1: TButton;
    reply_button_2: TButton;
    reply_button_3: TButton;
    reply_button_4: TButton;
    reply_button_5: TButton;
    reply_button_6: TButton;
    preferences_checkbox: TCheckBox;
    whats_this_label: TLabel;
    online_help_button: TButton;
    online_help_label: TLabel;
    ref_label: TLabel;
    alert_html_viewer: THTMLViewer;
    dummy_label: TLabel;
    html_popup_menu: TPopupMenu;
    copy_popup_entry: TMenuItem;
    N1: TMenuItem;
    select_all_popup_entry: TMenuItem;
    option1a_checkbox: TCheckBox;
    option1b_checkbox: TCheckBox;
    option2a_button: TButton;
    option2b_button: TButton;
    down_panbutton: TBitBtn;
    left_panbutton: TBitBtn;
    right_panbutton: TBitBtn;
    up_panbutton: TBitBtn;
    drop_panel: TPanel;
    drop_image: TImage;
    drop_label: TLabel;
    open_mystuff_button: TButton;
    drop_top_label: TLabel;
    procedure alert_html_viewerHotSpotClick(Sender: TObject;
      const SRC: ThtString; var Handled: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure alert_panel6Click(Sender: TObject);
    procedure alert_panel1Click(Sender: TObject);
    procedure alert_panel2Click(Sender: TObject);
    procedure alert_panel3Click(Sender: TObject);
    procedure alert_panel4Click(Sender: TObject);
    procedure alert_panel5Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure how_panelClick(Sender: TObject);
    procedure colour_panelClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormActivate(Sender: TObject);
    //procedure font_buttonClick(Sender: TObject);
    procedure chat_panelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure alert_panel6MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure alert_panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure alert_panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure alert_panel3MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure alert_panel4MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure alert_panel5MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure whats_this_labelClick(Sender: TObject);
    procedure online_help_buttonClick(Sender: TObject);
    procedure online_help_labelClick(Sender: TObject);
    procedure ref_labelDblClick(Sender: TObject);
    procedure select_all_popup_entryClick(Sender: TObject);
    procedure copy_popup_entryClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure option2a_buttonClick(Sender: TObject);
    procedure option2b_buttonClick(Sender: TObject);
    procedure left_panbuttonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure left_panbuttonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure right_panbuttonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure down_panbuttonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure up_panbuttonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure open_mystuff_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

{ OT-FIRST
      original_panel_window_proc:TWndMethod;
      procedure panel_window_proc (var msg:TMessage);
      procedure panel_image_drop (var msg:TWMDROPFILES);
}

  public
    { Public declarations }
  end;

var
  alert_box: Talert_box;

  //_________________

  alert_colour:array[0..8] of integer;      // ( 0-7 actually used so far).

  clicked_i:integer=0;

  alert_option2a_click_code:integer=0;    // 205d
  alert_option2b_click_code:integer=0;

  //dropped_is_wmf:boolean=False;
  //dropped_is_emf:boolean=False;

  function alert(head_i:integer; caption_str, message_str, panel1_str, panel2_str, panel3_str, panel4_str, panel5_str, panel6_str:string; help_panel:integer):integer;

//___________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

//  this unit is a replacement for the GFA ALERT box.
//  6 buttons: 6 is always default accept.
//             5 is always cancel (on ESC key or close form).
uses
  ShellAPI, math_unit, control_room, help_sheet, colour_unit, chat_unit,
  pad_unit, panning_unit;

var
  colour_index:integer=0;
  help_i:integer=0;

  php_str:string='';

  online_url_str:string='http://templot.com/companion/info.php?ref=000';
  ref_str:string='000';

  current_load_str:string='';

  options1:boolean=False;  // 205d
  options2:boolean=False;

//________________________________________________________________________________________

function alert(head_i:integer; caption_str, message_str, panel1_str, panel2_str, panel3_str, panel4_str, panel5_str, panel6_str:string; help_panel:integer):integer;

     // !!! mods 21-6-00   now ignores any leading CRs on the message string,
     //                    and trims any leading/trailing spaces from the bar strings.

var
  //msg_str:string;
  line_height:integer;
  bar_spacing,next_top,current_bottom:integer;

  n:integer;

  excess_box_height:integer;
  
  //widened_for_small_screen:integer;

  reduced_height_count:integer;

label
  123;

begin
  //widened_for_small_screen:=0;
  colour_index:=head_i;
  help_i:=help_panel;

  if panel5_str<>'' then clicked_i:=5   //  init return value = escape (in case he closes).
                    else clicked_i:=6;  //  or default.

      // 0.93.a ...

  php_str:=Copy(caption_str,1,7);   // online URL marker   php/nnn


  if Copy(php_str,1,4)='php/'       // marker exists
     then begin
            Delete(caption_str,1,7);                      // remove it
            ref_str:=Copy(php_str,5,3);                   // subject ref

            alert_box.ref_label.Caption:=title_swap(ref_str);
            alert_box.ref_label.Visible:=True;

            alert_box.online_help_button.Visible:=True;   // and enable the button
            alert_box.online_help_label.Visible:=True;

            online_url_str:='http://templot.com/companion/info.php?ref='+ref_str;
          end
     else begin
            ref_str:='';

            alert_box.ref_label.Caption:='';
            alert_box.ref_label.Visible:=False;

            alert_box.online_help_button.Visible:=False;    // no marker supplied
            alert_box.online_help_label.Visible:=False;

            online_url_str:='';
          end;

  while Copy(message_str,1,1)='|' do Delete(message_str,1,1);  // !!! 22-6-00 ignore all leading CR requests.
                                                               // !!! for revised memo form design.

  with alert_box
       do begin
                  // 208d  php ref 950 = file viewer delete confirm, allow width to show screenshot

            if  ref_str<>'950'
                then begin
                       ClientWidth:=alert_panel6.Left*2+alert_panel6.Width;      // width in case he's messed up the size.
                       alert_html_viewer.Width:=ClientWidth;
                     end;

            alert_panel1.Color:=clAqua;   // in case previously white help bar...
            alert_panel2.Color:=clAqua;
            alert_panel3.Color:=clAqua;
            alert_panel4.Color:=clAqua;
            alert_panel5.Color:=clYellow;
            alert_panel6.Color:=clLime;

            alert_panel1.Font.Color:=clBlack;   // in case previously selected...
            alert_panel2.Font.Color:=clBlack;
            alert_panel3.Font.Color:=clBlack;
            alert_panel4.Font.Color:=clBlack;
            alert_panel5.Font.Color:=clBlack;
            alert_panel6.Font.Color:=clBlack;

            Color:=alert_colour[colour_index];
            if caption_str<>'' then Caption:=title_swap(caption_str)  // OT-FIRST
                               else Caption:='   important ...';
            case head_i of
                      0: begin
                           alert_header_label.Caption:='!   PROGRAM  ALERT   !';
                           alert_header_label.Font.Color:=clFuchsia;
                           dummy_label.Font.Color:=clYellow;
                         end;

                      1: begin
                           alert_header_label.Caption:='+   WARNING   +';
                           if hi_color=False then alert_header_label.Font.Color:=clPurple
                                             else alert_header_label.Font.Color:=clRed;
                           dummy_label.Font.Color:=clBlack;
                         end;

                      2: begin
                           alert_header_label.Caption:='i    PROGRAM  INFORMATION    i';
                           alert_header_label.Font.Color:=clBlue;
                           dummy_label.Font.Color:=clBlack;
                         end;

                      3: begin
                           alert_header_label.Caption:='>    HANDY  HINT    >';
                           alert_header_label.Font.Color:=clBlue;
                           dummy_label.Font.Color:=clBlack;
                         end;

                      4: begin
                           alert_header_label.Caption:='?    QUESTION    ?';
                           alert_header_label.Font.Color:=clBlue;
                           dummy_label.Font.Color:=clBlack;
                         end;

                      5: begin
                           alert_header_label.Caption:='***    ERROR    ***';
                           alert_header_label.Font.Color:=clRed;
                           dummy_label.Font.Color:=clBlue;
                         end;

                      6: begin
                           alert_header_label.Caption:='÷     INVALID  REQUEST     ÷';
                           alert_header_label.Font.Color:=clBlue;
                           dummy_label.Font.Color:=clBlack;
                         end;

                      7: begin
                           alert_header_label.Caption:='•    CONFIRM    •';
                           alert_header_label.Font.Color:=clBlue;
                           dummy_label.Font.Color:=clBlack;
                         end;

                    else run_error(35);
            end;//case

                     // mods for HTML alert. 0.97.d ...

            if drop_panel.Visible=True                                       // 214a
               then alert_html_viewer.Top:=drop_panel.Top+drop_panel.Height
               else alert_html_viewer.Top:=alert_header_label.Top+alert_header_label.Height;

            current_load_str:=convert_tagged_string_to_html(0,message_str,True);    // 0.97.d

            alert_html_viewer.LoadFromString(current_load_str);  // 0.97.d

            alert_html_viewer.Height:=alert_html_viewer.FullDisplaySize(alert_html_viewer.Width).cy+16;

            reduced_height_count:=0;

            123:

            bar_spacing:=alert_header_label.Height;                            // in case he has resized it.
            current_bottom:=alert_html_viewer.Top+alert_html_viewer.Height;    // init.
            next_top:=current_bottom+bar_spacing-alert_panel6.Height;          // init top of next bar.
            //bars_started:=False;                                             // not yet started adding bars.

            if panel1_str<>''
               then begin
                      alert_panel1.Top:=next_top;
                      current_bottom:=next_top+alert_panel1.Height;

                      next_top:=next_top+bar_spacing;

                      if Pos('_',panel1_str)<>1   // if begins with underscore, show option boxes instead   205d

                         then begin   // show normal bar

                                options1:=False;  // normal bar

                                option1a_checkbox.Visible:=False;
                                option1b_checkbox.Visible:=False;

                                reply_button_1.Visible:=True;

                                alert_panel1.Caption:=title_swap(Trim(panel1_str));
                                if help_panel=1 then alert_panel1.Caption:=alert_panel1.Caption+'      F1';
                              end
                         else begin     // show option boxes

                                     // n.b. 1b cannot show unless 1a showing.

                                options1:=True;

                                reply_button_1.Visible:=False;

                                alert_panel1.Caption:='';

                                alert_panel1.Color:=alert_box.Color;  // so bar is disguised

                                n:=Pos('_1b',panel1_str);  // 2 strings?

                                if n>0               // 2 checkboxes...
                                   then begin
                                          option1a_checkbox.Caption:=title_swap(Copy(panel1_str,11,(n-11)));
                                          option1b_checkbox.Caption:=title_swap(Copy(panel1_str,n+10,100));

                                          option1a_checkbox.Checked:=(Pos('_1aticked_',panel1_str)>0);
                                          option1b_checkbox.Checked:=(Pos('_1bticked_',panel1_str)>0);

                                          option1a_checkbox.Visible:=True;
                                          option1b_checkbox.Visible:=True;
                                        end
                                   else begin   // 1 checkbox...

                                          option1a_checkbox.Caption:=title_swap(Copy(panel1_str,11,100));

                                          option1a_checkbox.Checked:=(Pos('_1aticked_',panel1_str)>0);

                                          option1a_checkbox.Visible:=True;
                                          option1b_checkbox.Visible:=False;
                                        end;
                              end;

                      alert_panel1.Show;
                    end
               else begin
                      alert_panel1.Hide;
                     end;

            if panel2_str<>''
               then begin
                      alert_panel2.Top:=next_top;
                      current_bottom:=next_top+alert_panel2.Height;

                      next_top:=next_top+bar_spacing;

                      // _2a_     // 205d  option flags     always 4 characters
                      // _2b_

                      if Pos('_',panel2_str)<>1   // if begins with underscore, show option boxes instead   205d

                         then begin   // show normal bar

                                options2:=False;  // normal bar

                                option2a_button.Visible:=False;
                                option2b_button.Visible:=False;

                                reply_button_2.Visible:=True;

                                alert_panel2.Caption:=title_swap(Trim(panel2_str));
                                if help_panel=2 then alert_panel2.Caption:=alert_panel2.Caption+'      F1';
                              end
                         else begin     // show option boxes

                                     // n.b. 2b cannot show unless 2a showing.

                                options2:=True;

                                reply_button_2.Visible:=False;

                                alert_panel2.Caption:='';

                                alert_panel2.Color:=alert_box.Color;  // so bar is disguised

                                n:=Pos('_2b',panel2_str);  // 2 strings?

                                if n>0               // 2 buttons...
                                   then begin
                                          option2a_button.Caption:=title_swap(Copy(panel2_str,5,(n-5)));
                                          option2b_button.Caption:=title_swap(Copy(panel2_str,n+4,100));

                                          option2a_button.Visible:=True;
                                          option2b_button.Visible:=True;
                                        end
                                   else begin   // 1 button...

                                          option2a_button.Caption:=title_swap(Copy(panel2_str,5,100));

                                          option2a_button.Visible:=True;
                                          option2b_button.Visible:=False;
                                        end;
                              end;

                      alert_panel2.Show;
                    end
               else begin
                      alert_panel2.Hide;
                      //{if bars_started=True then }next_top:=next_top+bar_spacing;    // gap for missing bar wanted.
                      //button_2.Hide;
                     end;

            if panel3_str<>''
               then begin
                      alert_panel3.Top:=next_top;
                      current_bottom:=next_top+alert_panel3.Height;

                      next_top:=next_top+bar_spacing;
                      //bars_started:=True;

                      alert_panel3.Caption:=title_swap(Trim(panel3_str));
                      if help_panel=3 then alert_panel3.Caption:=alert_panel3.Caption+'      F1';
                      alert_panel3.Show;
                      //button_3.Show;
                    end
               else begin
                      alert_panel3.Hide;
                      //{if bars_started=True then }next_top:=next_top+bar_spacing;    // gap for missing bar wanted.
                      //button_3.Hide;
                     end;

            if panel4_str<>''
               then begin
                      alert_panel4.Top:=next_top;
                      current_bottom:=next_top+alert_panel4.Height;

                      next_top:=next_top+bar_spacing;
                      //bars_started:=True;

                      alert_panel4.Caption:=title_swap(Trim(panel4_str));
                      if help_panel=4 then alert_panel4.Caption:=alert_panel4.Caption+'      F1';
                      alert_panel4.Show;
                      //button_4.Show;
                    end
               else begin
                      alert_panel4.Hide;
                      //{if bars_started=True then }next_top:=next_top+bar_spacing;    // gap for missing bar wanted.
                      //button_4.Hide;
                     end;

            if panel5_str<>''
               then begin
                      alert_panel5.Top:=next_top;
                      current_bottom:=next_top+alert_panel5.Height;

                      next_top:=next_top+bar_spacing;
                      //bars_started:=True;

                      alert_panel5.Caption:=title_swap(Trim(panel5_str));
                      alert_panel5.Show;
                      //button_5.Show;
                    end
               else begin
                      alert_panel5.Hide;
                      //{if bars_started=True then }next_top:=next_top+bar_spacing;    // gap for missing bar wanted.
                      //button_5.Hide;
                     end;

            if panel6_str<>''
               then begin
                      alert_panel6.Top:=next_top;
                      current_bottom:=next_top+alert_panel6.Height;

                      next_top:=next_top+bar_spacing;
                      //bars_started:=True;

                      alert_panel6.Caption:=title_swap(Trim(panel6_str));
                      alert_panel6.Show;
                      //button_6.Show;
                    end
               else begin
                      alert_panel6.Hide;
                      //{if bars_started=True then }next_top:=next_top+bar_spacing;    // gap for missing bar wanted.
                      //button_6.Hide;
                     end;

            ClientHeight:=current_bottom+10;    // ensure it's all visible.

            // 0.91.d pref_options...


            if preferences_checkbox.Visible=True   // set before showing form.
               then begin
                      preferences_checkbox.Top:=current_bottom+12;
                      ClientHeight:=ClientHeight+preferences_checkbox.Height+6;
                    end;

            whats_this_label.Top:=preferences_checkbox.Top+2;
            whats_this_label.Visible:=preferences_checkbox.Visible;

              // check the form is not too tall to fit screen ...

            Application.ProcessMessages;

            if (reduced_height_count<20)    // prevent infinite loop
                then begin
                       excess_box_height:=alert_box.Height+120-Screen.Height;  // 120 arbitrary

                       if excess_box_height>0   // reduce it so that scrollbars appear
                          then begin
                                 alert_box.Top:=20;   // leaves 100 at bottom for taskbar, etc.

                                 alert_html_viewer.Height:=alert_html_viewer.Height-excess_box_height;

                                 INC(reduced_height_count);     // do it at least once more (adding the scrollbar changes the height)
                                 goto 123;                      // recalculate shorter form
                               end;
                     end;

            case help_panel of                                // help option is on a white bar.
                         1: alert_panel1.Color:=clWhite;
                         2: alert_panel2.Color:=clWhite;
                         3: alert_panel3.Color:=clWhite;
                         4: alert_panel4.Color:=clWhite;
            end;//case

            if alert_box.Top+alert_box.Height>(Screen.Height-100)
               then alert_box.Top:=Screen.Height-alert_box.Height-100;  // move it up if needed;

            if alert_box.Top<0 then alert_box.Top:=0;

            if Showing=False then do_show_modal(alert_box);  // 212a   ShowModal          //  show the form and wait for a click.

          end;//with

  if Application.Terminated=False then Application.ProcessMessages;      // ensure form has closed before returning.
  RESULT:=clicked_i;
end;
//_________________________________________________________________________________________

procedure Talert_box.FormKeyPress(Sender: TObject; var Key: Char);

begin

  if (Key='1') and (alert_panel1.Visible=True)
  and (options1=False) then begin clicked_i:=1; Close; end;  //  accept 1 key instead of clicking panel 1 or ALT-1.

  if (Key='2') and (alert_panel2.Visible=True)
  and (options2=False) then begin clicked_i:=2; Close; end;  //  accept 2 key instead of clicking panel 2 or ALT-2.

  if (Key='3') and (alert_panel3.Visible=True) then begin clicked_i:=3; Close; end;  //  accept 3 key instead of clicking panel 3 or ALT-3.

  if (Key='4') and (alert_panel4.Visible=True) then begin clicked_i:=4; Close; end;  //  accept 4 key instead of clicking panel 4 or ALT-4.

  if (Key='5') and (alert_panel5.Visible=True) then begin clicked_i:=5; Close; end;  //  accept 5 key instead of clicking panel 5 or ESC.

  if (Key='6') and (alert_panel6.Visible=True) then begin clicked_i:=6; Close; end;  //  accept 6 key instead of clicking panel 6 or ALT-6.

end;
//__________________________________________________________________________________________

procedure Talert_box.alert_panel6Click(Sender: TObject);

begin
  clicked_i:=6;
  alert_box.Close;
  end;
//__________________________________________________________________________________________

procedure Talert_box.alert_panel1Click(Sender: TObject);
begin
  if options1=True then EXIT;      // 205d

  clicked_i:=1;
  alert_box.Close;
end;

procedure Talert_box.alert_panel2Click(Sender: TObject);
begin
  if options2=True then EXIT;      // 205d

  clicked_i:=2;
  alert_box.Close;
end;

procedure Talert_box.alert_panel3Click(Sender: TObject);
begin
  clicked_i:=3;
  alert_box.Close;
end;

procedure Talert_box.alert_panel4Click(Sender: TObject);
begin
  clicked_i:=4;
  alert_box.Close;
end;

procedure Talert_box.alert_panel5Click(Sender: TObject);
begin
  clicked_i:=5;
  alert_box.Close;
end;
//________________________________________________________________________________________

procedure Talert_box.FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);

begin
    if Key=VK_PAUSE then Application.Minimize;    //  hide TEMPLOT on PAUSE key.
    if Key=VK_F1
       then begin
              Key:=0;
              case help_i of
                       1: if alert_panel1.Visible=True then begin clicked_i:=1; Close; end;
                       2: if alert_panel2.Visible=True then begin clicked_i:=2; Close; end;
                       3: if alert_panel3.Visible=True then begin clicked_i:=3; Close; end;
                       4: if alert_panel4.Visible=True then begin clicked_i:=4; Close; end;
              end;//case
            end;
end;
//______________________________________________________________________________________

procedure Talert_box.how_panelClick(Sender: TObject);

var
  green_str:string;

begin
  if alert_box.alert_panel6.Showing=True then green_str:='yes  -  green  bar  option'
                                         else green_str:='';

  if help(-1,'Templot needs a response from you.||Click a coloured bar to make your selected response,'
          +' or if you prefer you can tab to the conventional buttons.'
          +'||Keys `01`2 to `06`2 on the keyboard can also be used, as shown on the buttons.'

          //%%%%

          +'||The green bar (if present) can also be selected by pressing the `0ENTER`2 key.'
          +'||The yellow bar (if present) can also be selected by pressing the escape (`0ESC`2) key, or by closing the window.'
          +'||For more information about the available options click the white bar marked `0? help`1 or `0more information`1 (if present),'
          +' or press the `0F1`2 key.'

          //%%%%

          +'||If you tick the bottom box labelled `0don''t show this message again`1 (if present) <U>before</U> clicking a coloured bar,'
          +' your response will be added to your program preferences and repeated automatically next time, instead of showing the message again.'
          +' <SPAN STYLE="color:#800000; font-style:italic;">If you are a new Templot0 user it is recommended that you don''t do this until you are familiar with Templot0.</SPAN>'
          +' If you save your program preferences, the message will also not be shown in subsequent Templot0 sessions.'
          +' To restore seeing messages, re-start Templot0 without using your preferences.'

          +'||If you are unsure about your response, click the green bar at the bottom (if present).'
          +' Do you want to do this now?',green_str)=1 then alert_box.alert_panel6Click(Sender);
end;
//____________________________________________________________________________________

procedure Talert_box.colour_panelClick(Sender: TObject);

begin
  Color:=get_colour('choose  a  new  colour  for  the  panel',alert_colour[colour_index]);  // change current showing.
  alert_colour[colour_index]:=Color;                                                        // and next time.
end;
//___________________________________________________________________________________

procedure Talert_box.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position>size_updown.Tag                          // ! position goes up, size goes down.
     then ScaleBy(9,10);                                           // scale the form contents down.

  if size_updown.Position<size_updown.Tag
     then ScaleBy(10,9);                                           // scale the form contents up.

  size_updown.Tag:=size_updown.Position;                           // and save for the next click.
end;
//_____________________________________________________________________________________

procedure Talert_box.chat_panelClick(Sender: TObject);

const
  chat_str:string='|This multiple choice panel contains lengthier explanations than the usual Windows dialog boxes,'
                 +' and some attractive coloured click bars with detailed captions.'
                 +'||The purpose of the additional buttons is to receive the focus if you prefer to tab to them.'
                 +'||What do you mean, you don''t like it ?';
begin
  chat(chat_str);
end;
//_______________________________________________________________________________________

procedure Talert_box.FormCreate(Sender: TObject);

begin
  if Screen.Height<500
     then Top:=2
     else begin
            Left:=Screen.Width div 2 -Width-50;
            Top:=Screen.Height div 5;
          end;
          
  ClientWidth:=439;
  ClientHeight:=437;
  alert_header_label.Height:=alert_panel6.Top-alert_panel5.Top;  //!!! 22-6-00 this is used as the bar-spacing after any re-sizing.

  AutoScroll:=False;

     // 214a
{ OT-FIRST
  original_panel_window_proc:=drop_panel.WindowProc;     // temp save WindowProc for the drop_panel
  drop_panel.WindowProc:=panel_window_proc;              // and replace it

  DragAcceptFiles(drop_panel.Handle,True);
}
end;
//_______________________________________________________________________________________

procedure invert_panel(panel_tag:integer; the_panel:TPanel);

var
  col:integer;

begin
  if the_panel.Tag=panel_tag then EXIT;

  col:=the_panel.Color;
  the_panel.Color:=the_panel.Font.Color;
  the_panel.Font.Color:=col;
  the_panel.Tag:=panel_tag;
end;

//_____________________________________________________________________________________

procedure reset_panels(not_this:integer);    // reset click-bars to un-selected.

begin
  with alert_box do begin
    if not_this<>1 then invert_panel(0,alert_panel1);
    if not_this<>2 then invert_panel(0,alert_panel2);
    if not_this<>3 then invert_panel(0,alert_panel3);
    if not_this<>4 then invert_panel(0,alert_panel4);
    if not_this<>5 then invert_panel(0,alert_panel5);
    if not_this<>6 then invert_panel(0,alert_panel6);
  end;//with
end;
//_____________________________________________________________________________________

procedure Talert_box.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  reset_panels(0);                                            // reset all click-bars to un-selected.
end;
//____________________________________________________________________________________

procedure Talert_box.alert_panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if options1=True then EXIT;      // 205d

  reset_panels(1);                 // reset other click-bars to un-selected.
  invert_panel(1,alert_panel1);    // invert this one.
end;
//_______________________________________________________________________________________

procedure Talert_box.alert_panel2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if options2=True then EXIT;      // 205d

  reset_panels(2);                 // reset other click-bars to un-selected.
  invert_panel(1,alert_panel2);    // invert this one.
end;
//______________________________

procedure Talert_box.alert_panel3MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  reset_panels(3);                 // reset other click-bars to un-selected.
  invert_panel(1,alert_panel3);    // invert this one.
end;
//______________________________

procedure Talert_box.alert_panel4MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  reset_panels(4);                 // reset other click-bars to un-selected.
  invert_panel(1,alert_panel4);    // invert this one.
end;
//______________________________

procedure Talert_box.alert_panel5MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  reset_panels(5);                 // reset other click-bars to un-selected.
  invert_panel(1,alert_panel5);    // invert this one.
end;
//______________________________

procedure Talert_box.alert_panel6MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  reset_panels(6);                 // reset other click-bars to un-selected.
  invert_panel(1,alert_panel6);    // invert this one.
end;
//_______________________________________________________________________________________

procedure Talert_box.FormDeactivate(Sender: TObject);

            // not on form Activate because the panel colours may differ (help bars = white).
begin
  reset_panels(0);      // reset all click-bars to un-selected.
end;
//_______________________________________________________________________________________

procedure Talert_box.FormActivate(Sender: TObject);

begin
            //%%%%

  if user_prefs_in_use=True
     then preferences_checkbox.Caption:=' don''t  show  this  message  again'
     else preferences_checkbox.Caption:=' don''t  show  this  message  again  in  this  session';


  if alert_panel6.Showing=True then reply_button_6.SetFocus;  //  default button.
end;
//________________________________________________________________________________________

procedure Talert_box.whats_this_labelClick(Sender: TObject);  //%%%%

begin
  help(-6,'      `0Add  to  Preferences`9'
         +'||If you tick this box labelled `0don''t show this message again in this session`1 <U>before</U> clicking a coloured bar,'
         +' your response will be added to your program preferences and repeated automatically next time, instead of showing the message again.'

         +'||<SPAN STYLE="color:#800000; font-style:italic;">If you are a new Templot0 user it is recommended that you don''t do this until you are familiar with Templot0.</SPAN>'

         +'||If you save your program preferences, the message will also not be shown in subsequent Templot0 sessions. To restore seeing messages, re-start Templot0 without using your preferences.'
         +' For information about saving preferences, click <A HREF="show_prefs_dialog.85a">saving preferences</A>.','');
end;
//______________________________________________________________________________

procedure Talert_box.online_help_buttonClick(Sender: TObject);

begin
  if ShellExecute(0,'open',PChar(online_url_str),nil,nil,SW_SHOWNORMAL)<=32
     then show_modal_message('Sorry, unable to open your browser window and connect to the Templot web site.'
                     +#13+#13+'Please check your internet connection.');
end;
//______________________________________________________________________________

procedure Talert_box.online_help_labelClick(Sender: TObject);

var
  more_url_str:string;

begin
  more_url_str:=StringReplace(online_url_str,'http://','',[rfReplaceAll, rfIgnoreCase]);  // ignore protocol

  show_modal_message('If you need more help or information about this subject'
              +#13+'please click the "more information online" button.'
              +#13+#13+'This will connect you to the Templot Companion web site'
              +#13+'where there are detailed user guides -- with explanations,'
              +#13+'diagrams, videos and tutorials.'
              +#13+'          _________________________________________________'
              +#13+#13+'If you don''t have an internet connection on this computer,'
              +#13+'make a note of this address to visit the site at another time:'
              +#13+#13+more_url_str
              +#13+#13+'Or make a note of the reference number for this subject: '+ref_str);
end;
//______________________________________________________________________________

procedure Talert_box.ref_labelDblClick(Sender: TObject);

begin
  Clipboard.AsText:=current_load_str;
  Beep;
end;
//______________________________________________________________________________

procedure Talert_box.select_all_popup_entryClick(Sender: TObject);

begin
  alert_html_viewer.SelectAll;
end;
//______________________________________________________________________________

procedure Talert_box.copy_popup_entryClick(Sender: TObject);

begin
  alert_html_viewer.CopyToClipboard;
end;
//______________________________________________________________________________

procedure Talert_box.FormResize(Sender: TObject);

begin
  alert_html_viewer.Width:=ClientWidth;
end;
//______________________________________________________________________________

procedure options_buttons_click(code:integer);

begin
  case code of

    1:  pad_form.explode_normal_menu_entry.Click;   // zoom in
    2:  pad_form.shrink_normal_menu_entry.Click;    // zoom out

  end;//case
end;
//______________________________________________________________________________

procedure Talert_box.option2a_buttonClick(Sender: TObject);     // 205d

begin
  options_buttons_click(alert_option2a_click_code);
end;
//______________________________________________________________________________

procedure Talert_box.option2b_buttonClick(Sender: TObject);

begin
  options_buttons_click(alert_option2b_click_code);
end;
//______________________________________________________________________________

procedure Talert_box.left_panbuttonMouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);

begin
  panning_form.left_panbuttonMouseDown(Sender,Button,Shift,X,Y);
end;
//______________________________________________________________________________

procedure Talert_box.right_panbuttonMouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);

begin
  panning_form.right_panbuttonMouseDown(Sender,Button,Shift,X,Y);
end;
//______________________________________________________________________________

procedure Talert_box.down_panbuttonMouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);

begin
  panning_form.down_panbuttonMouseDown(Sender,Button,Shift,X,Y);
end;
//______________________________________________________________________________

procedure Talert_box.up_panbuttonMouseDown(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);

begin
  panning_form.up_panbuttonMouseDown(Sender,Button,Shift,X,Y);
end;
//______________________________________________________________________________

procedure Talert_box.left_panbuttonMouseUp(Sender:TObject; Button:TMouseButton; Shift:TShiftState; X,Y:Integer);

begin
  panning_form.up_panbuttonMouseUp(Sender,Button,Shift,X,Y);

  blue_corner_panel.SetFocus;        // don't want it on button.
end;
//______________________________________________________________________________

procedure Talert_box.FormClose(Sender: TObject; var Action: TCloseAction);  // 205d

begin
  left_panbutton.Visible:=False;
  right_panbutton.Visible:=False;
  down_panbutton.Visible:=False;
  up_panbutton.Visible:=False;
end;
//______________________________________________________________________________
{ OT-FIRST
procedure Talert_box.panel_window_proc(var msg:TMessage);       // 214a

begin
  if msg.Msg=WM_DROPFILES
     then panel_image_drop(TWMDROPFILES(msg))
     else original_panel_window_proc(msg);
end;
//______________________________________________________________________________

procedure Talert_box.panel_image_drop(var msg:TWMDROPFILES);    // 214a
}

procedure Talert_box.FormDropFiles(Sender:TObject; const FileNames:array of String);

var
  num_files:integer;

  her_file_name_str,
  dropped_file_name_str,
  dropped_file_ext_str:string;

  dropped_picture:TPicture;

  i:integer;

begin

  her_file_name_str:='';   // keep compiler happy

  num_files:=Length(FileNames);

  if num_files<>1
     then begin
            ShowMessage('error - attempt to drop more than one file');
            EXIT;
          end;

  her_file_name_str:=FileNames[Low(FileNames)];

  dropped_file_name_str:=change_jpeg_filename(her_file_name_str);      // change .jpeg to.jpg

  dropped_file_ext_str:=LowerCase(ExtractFileExt(dropped_file_name_str));

  try
    drop_image.Picture.LoadFromFile(dropped_file_name_str);
  except
    on EInvalidGraphic do begin
      ShowMessage('error - the '+ExtractFileExt(dropped_file_name_str)+' file format is not supported');
      EXIT;
    end;//on
  end;//try
  drop_image.Visible:=True;

  alert_panel6.Caption:='continue  -  use  the  dropped  image';

  alert_panel1.Visible:=False;
  alert_panel2.Visible:=False;
  alert_panel3.Visible:=False;
  alert_panel4.Visible:=False;
end;

procedure Talert_box.alert_html_viewerHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  htmlviewer_hot_spot_clicked(Sender, SRC, Handled);
end;

//______________________________________________________________________________

procedure Talert_box.open_mystuff_buttonClick(Sender: TObject);

begin
  open_MyPictures;    // in control room
end;
//______________________________________________________________________________

procedure Talert_box.FormShow(Sender: TObject);

begin
  alert_box.BringToFront;
end;
//______________________________________________________________________________

initialization


  alert_colour[0]:=clBlack;     //  alert
  alert_colour[1]:=$00D0E8E8;   //  warning      cream
  alert_colour[2]:=$00E8E0D8;   //  information  blue-grey
  alert_colour[3]:=$00E0FFE0;   //  handy hint   lemon green
  alert_colour[4]:=$00E0E8E0;   //  question     sage green
  alert_colour[5]:=$00F0FFD0;   //  error        ice blue
  alert_colour[6]:=$00C8FFE8;   //  invalid      beige
  alert_colour[7]:=$00FFE8C8;   //  confirm      azure blue
  alert_colour[8]:=clWhite;


//______________________________________________________________________________

end.

