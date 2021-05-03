
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

unit file_viewer;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ComCtrls, Grids, Outline, ExtCtrls, ShellCtrls, Htmlview,
  HtmlGlobals, HTMLUn2;

type

  { Tfile_viewer_form }

  Tfile_viewer_form = class(TForm)
    html_preview: THtmlViewer;
    help_shape: TShape;
    help_button: TButton;
    Label4: TLabel;
    blue_corner_panel: TPanel;
    folder_view: TShellTreeView;
    box_file_listview: TShellListView;
    file_panel: TPanel;
    size_updown: TUpDown;
    controls_panel: TPanel;
    found_label: TLabel;
    count_label: TLabel;
    progress_bar: TProgressBar;
    close_panel: TPanel;
    close_button: TButton;
    refresh_button: TButton;
    open_folder_button: TButton;
    images_clickable_checkbox: TCheckBox;
    name_labels_checkbox: TCheckBox;
    images_label: TLabel;
    instant_show_checkbox: TCheckBox;
    escape_label: TLabel;
    Splitter1: TSplitter;
    procedure box_file_listviewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure folder_listboxClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure FormKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure html_file_viewerHotSpotClick(Sender: TObject;
      const SRC: ThtString; var Handled: Boolean);
    procedure html_previewBitmapRequest(Sender: TObject; const SRC: ThtString;
      var Bitmap: TBitmap; var Color: TColor);
    procedure html_previewHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure refresh_buttonClick(Sender: TObject);
    procedure open_folder_buttonClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure html_file_viewerMouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure instant_show_checkboxClick(Sender: TObject);
  private
    { Private declarations }
    procedure show_file_as_bitmap(file_name: string);
    procedure fv_link_clicked(const url: string; var Handled: Boolean);

    procedure fv_reload_file();
    procedure fv_add_file();
    procedure fv_tags();
    procedure fv_read_me();

    procedure no_readme_help(none:boolean);

  public
    { Public declarations }
  end;

var
  file_viewer_form: Tfile_viewer_form;


  box_file_list:TStringList;
  box_size_list:TStringList;
  box_time_list:TStringList;

  num_bgnd_list:TStringList;
  num_unused_list:TStringList;
  num_lib_list:TStringList;

  img_name_list:TStringList;

  next_html_list:TStringList;

  readme_list:TStringList;

  fv_tag_list:TStringList;


  fv_gauge_str:string;

  html_str:string;           // table rows

  keep_form_was_showing:boolean=False;

  fv_has_been_active:boolean=False;

implementation

{$R *.lfm}

uses
  ShellAPI,control_room,pad_unit,grid_unit,math_unit, panning_unit,
  shove_timber,rail_options_unit,platform_unit,check_diffs_unit,
  data_memo_unit,stay_visible_unit,info_unit,keep_select,help_sheet,alert_unit;
  

var

{
  info_was_showing:boolean=False;
  panning_was_showing:boolean=False;
  shove_was_showing:boolean=False;
  spacing_was_showing:boolean=False;
  rail_options_was_showing:boolean=False;
  platform_was_showing:boolean=False;
  check_diffs_was_showing:boolean=False;
  data_child_was_showing:boolean=False;
  stay_visible_was_showing:boolean=False;

  saved_info_left,saved_info_top,saved_info_width,saved_info_height:integer;



  saved_panning_left:integer;
}

  saved_pad_width,saved_pad_height:integer;

  escape_pressed:boolean=False;

  no_onresize:boolean=False;

  use_bmp_image_streams:boolean=False;

  showing_as_png_files:boolean=False;

  bmp_stream:TMemoryStream;

  html_create_str:string;

//______________________________________________________________________________

(*
procedure show_files_as_png;     // 208d

  // deprecated in 208e - use bitmap objects instead of PNG image files - see show_files_as_bitmaps;

  // user can choose option in program menu on control room - this is slower but uses less RAM.


const
  html_header_str:string='<HTML><HEAD><TITLE>Templot file viewer</TITLE></HEAD><BODY><TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="0" CELLPADDING="4" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;">';
  html_footer_str:string='</TABLE></BODY></HTML>';

var
  i,n:integer;

  tag_count:integer;

  screen_rect,print_rect:TRect;

  create_bitmap:TBitmap;

  //create_png:TPNGObject;

  create_png:TPortableNetworkGraphic;

  img_file_name_str:string;  // name part
  img_file_str:string;       // including full path

  html_top_str:string;

  oldbox_str:string;  // save box for restore

  old_save_done:boolean;

  search_record_png,search_record_box:TSearchRec;

{
  TSearchRec = record
		Time: Integer;
		Size: Integer;
		Attr: Integer;
		Name: TFileName;
		ExcludeAttr: Integer;
		FindHandle: THandle;
		FindData: TWin32FindData;
               end;
}


  dir_str,boxfile_str:string;

  next_str:string;

  tag_str,readme_str,fv_tag_str,num_files_str:string;

  hl:integer;
  append:boolean;

  windows_cursor_count:integer;

  saved_hide_name_labels:boolean;

              //////////////////////////////////////////////////////////////////

              procedure add_file_to_box_lists(search_record:TSearchRec);

              begin
                box_file_list.Add(search_record.Name);
                box_size_list.Add(IntToStr(Round(search_record.Size/1024))+' KB');
                box_time_list.Add(DateToStr(FileDateToDateTime(search_record.Time))+' &nbsp; '+TimeToStr(FileDateToDateTime(search_record.Time)));
              end;
              //////////////////////////////////////////////////////////////////

begin
  with file_viewer_form do begin

      // no changes while building list

    //disk_drive_combo.Enabled:=False;
    //folder_listbox.Enabled:=False;
    refresh_button.Enabled:=False;
    images_clickable_checkbox.Enabled:=False;
    name_labels_checkbox.Enabled:=False;
    instant_show_checkbox.Enabled:=False;

    progress_bar.Position:=0;

    escape_pressed:=False;

    img_name_list.Clear;

    box_file_list.Clear;
    box_size_list.Clear;
    box_time_list.Clear;

    num_bgnd_list.Clear;
    num_unused_list.Clear;
    num_lib_list.Clear;

    next_html_list.Clear;

    readme_list.Clear;

    fv_tag_list.Clear;


    //dir_str:=folder_listbox.Directory+'\';   // add trailing slash

       // build the lists ...

    if FindFirst(dir_str+'*.box3',0,search_record_box)=0
       then begin
              add_file_to_box_lists(search_record_box);

              while FindNext(search_record_box)=0 do add_file_to_box_lists(search_record_box);
            end;

    FindClose(search_record_box);

    try

      if (box_file_list.Count>60) and (too_many_files_msg_pref=False)
         then begin

                alert_box.preferences_checkbox.Checked:=False;       //%%%%
                alert_box.preferences_checkbox.Show;

                i:=alert(3,'php/950    large  number  of  files',
                           '||`0'+dir_str+'`f'
                          +'||This folder contains a large number of .box files ('+IntToStr(box_file_list.Count)+').'
                          +'||Creating all the screenshot images may take some time.'
                          +'||You may prefer to cancel and organize your files into|sub-folders.'
                          +'||If you continue you can press the `0ESC`2 key to stop the process.',
                           '','','','cancel  and  open  folder','cancel','continue',0);

                too_many_files_msg_pref:=alert_box.preferences_checkbox.Checked;
                alert_box.preferences_checkbox.Hide;

                case i of

                    4: begin
                         open_folder_button.Click;
                         EXIT;
                       end;

                    5: EXIT;
                end;//case

              end;


         // please wait ...

         // 3 table columns...

      html_top_str:='<TR><TD NOWRAP STYLE="COLOR:#9900FF; FONT-SIZE:18PX; FONT-WEIGHT:BOLD; FONT-STYLE:ITALIC;"> &nbsp; '+IntToStr(box_file_list.Count)+' &nbsp;.box &nbsp;files &nbsp;in &nbsp;this &nbsp;folder</TD><TD>&nbsp;</TD><TD>&nbsp;</TD></TR>'
                   +'<TR><TD COLSPAN="3"> &nbsp; &nbsp; '+dir_str+'</TD></TR>'
                   +'<TR><TD COLSPAN="3" ALIGN="RIGHT"><HR NOSHADE STYLE="COLOR:GRAY; HEIGHT:6PX;"></TD></TR>';

      if box_file_list.Count>0
         then html_str:='<TR><TD COLSPAN="3" ALIGN="CENTER"><IMG SRC="'+exe_str+'internal\hlp\wait_signal_trans.gif"> &nbsp; please wait while the images are generated'
                       +'<BR><BR>&nbsp; &nbsp; you can stop the process by pressing the <SPAN STYLE="COLOR:BLUE; FONT-FAMILY:''COURIER NEW''; FONT-SIZE:17PX;"><B>ESC</B></SPAN> key</TD></TR>'
         else html_str:='';

      html_str:=html_header_str+html_top_str+html_str+html_footer_str;

      use_bmp_image_streams:=False;  // normal load images from file

      html_file_viewer.LoadFromString(html_str);

      count_label.Caption:='0';

         // some .box files to show? ...

      if box_file_list.Count>0
         then begin
                showing_as_png_files:=True;

                images_label.Caption:='png files';

                escape_label.Show;
                count_label.Show;
                found_label.Show;

                ShowCursor(False);   // Windows SDK

                progress_bar.Max:=box_file_list.Count;

                  // first set up the trackpad...

                old_save_done:=save_done;      // save flag in case a confirm is needed on a reload

                oldbox_str:=exe_str+'fv.ebk';  // save existing box
                DeleteFile(oldbox_str);        // delete any previous file.
                save_box(0,0,0,oldbox_str);    // save existing contents for restore later.

                saved_pad_width:=pad_form.ClientWidth;
                saved_pad_height:=pad_form.ClientHeight;

                saved_hide_name_labels:=hide_name_labels;

                hide_name_labels:= NOT name_labels_checkbox.Checked;

                pad_form.ClientWidth:=676;     // screenshot image size
                pad_form.ClientHeight:=320;



                      // delete all previous PNG files in the fview folder...

                while FindFirst(exe_str+'internal\fview\*.png',0,search_record_png)=0 do DeleteFile(exe_str+'internal\fview\'+search_record_png.Name);

                FindClose(search_record_png);


                screen_rect.Top:=0;
                screen_rect.Left:=0;
                screen_rect.Right:=pad_form.ClientWidth; //-1;
                screen_rect.Bottom:=pad_form.ClientHeight; //-1;

                print_rect:=screen_rect;

                for n:=0 to box_file_list.Count-1 do begin

                  clear_keeps(False,False);    // clear all templates
                  append:=False;

                  boxfile_str:=dir_str+box_file_list.Strings[n];

                  if Pos('.box3',LowerCase(boxfile_str))=0 then boxfile_str:=boxfile_str+'.box3';  // in case his system hides extensions.

            //function load_storage_box(normal_load,old_templot_folder:boolean; file_str:string; load_backup,make_lib:boolean; var append:boolean; var last_bgnd_loaded_index:integer):boolean;    // load a file of templates into the keeps box.

                  if load_storage_box(False,False,boxfile_str,False,False,append,hl)=True
                     then begin
                            if keeps_list.Count>0
                               then begin
                                      if (loaded_version<93) and (hl>-1) and (hl<keeps_list.Count) then mint_final_or_copy_control(hl);   // if something loaded mint from highest bgnd if he so wants.
                                      if (loaded_version>92) then mint_final_or_copy_control(hl);                                         // copy the control template if there is one in the file.
                                    end;
                          end;

                  show_and_redraw(False,False);  // update pad

                  num_bgnd_list.Add(IntToStr(any_bgnd));
                  num_unused_list.Add(IntToStr(any_unused));
                  num_lib_list.Add(IntToStr(any_library));

                  readme_list.Add(get_readme_notes);

                  tag_count:=build_tag_list(False);    // tag_count not used
                  tag_str:='';

                  if tag_list.Count>0
                     then begin
                            for i:=0 to tag_list.Count-1 do begin
                              tag_str:=tag_str+' &nbsp; &nbsp; '+tag_list.Strings[i]+'|';   // add indent and separator
                            end;//next
                          end;

                  fv_tag_list.Add(tag_str);

                  img_file_name_str:=StringReplace(box_file_list.Strings[n],'.box3','_box3',[rfReplaceAll, rfIgnoreCase]);
                  img_file_str:=exe_str+'internal\fview\'+img_file_name_str+'.png';

                  img_name_list.Add(img_file_str);

                  create_bitmap:=TBitmap.Create;

                  //create_png:=TPNGObject.Create;

                  create_png:=TPortableNetworkGraphic.Create;



                  try
                    create_bitmap.Width:=pad_form.ClientWidth;
                    create_bitmap.Height:=pad_form.ClientHeight;

                    try
                      create_bitmap.Canvas.CopyMode:=cmSrcCopy;
                      create_bitmap.Canvas.CopyRect(print_rect,offdraw_bmp.Canvas,screen_rect);

                      with create_bitmap.Canvas do begin

                        Brush.Style:=bsSolid;
                        Brush.Color:=clYellow;

                        Font.Color:=clBlack;
                        Font.Height:=0-13;
                        Font.Style:=[fsBold];
                        Font.Name:='Arial';

                        TextOut(0,0,' '+IntToStr(n+1)+': '+fv_gauge_str+' '+box_project_title_str+' ');     // add the short gauge label

                      end;//with

                      create_png.Assign(create_bitmap);
                      create_png.SaveToFile(img_file_str);

                    except
                      show_modal_message('Sorry, an error occurred in creating image:'+#13+#13+IntToStr(n+1)+' : '+img_file_name_str);
                    end;//try

                  finally
                    create_png.Free;
                    create_bitmap.Free;
                  end;//try

                  progress_bar.Position:=n+1;

                  count_label.Caption:=IntToStr(n+1);

                  Application.ProcessMessages;  // for pressing ESC key

                  if escape_pressed=True then BREAK;

                end;//next

                     // restore trackpad...

                clear_keeps(False,False);    // clear all templates
                append:=False;

                if load_storage_box(False,False,oldbox_str,False,False,append,hl)=True
                   then begin
                          if keeps_list.Count>0
                             then begin
                                    if (loaded_version<93) and (hl>-1) and (hl<keeps_list.Count) then mint_final_or_copy_control(hl);   // if something loaded mint from highest bgnd if he so wants.
                                    if (loaded_version>92) then mint_final_or_copy_control(hl);                                         // copy the control template if there is one in the file.
                                  end;
                        end;

                pad_form.ClientWidth:=saved_pad_width;
                pad_form.ClientHeight:=saved_pad_height;

                hide_name_labels:=saved_hide_name_labels;

                show_and_redraw(False,False);  // update pad

                if any_bgnd<1 then pad_form.fit_current_only_menu_entry.Click;

                save_done:=old_save_done;      // restore flag in case a confirm is needed on a reload

                  // now create the HTML view...

                progress_bar.Position:=0;

                html_str:=html_header_str+html_top_str;

                if readme_list.Count=1 then num_files_str:=' file'
                                       else num_files_str:=' files';

                html_str:=html_str+'<TR><TD COLSPAN="3" STYLE="FONT-SIZE:4PX;"><A NAME="#file0">&nbsp;</A></TD></TR>';

                for n:=0 to readme_list.Count-1 do begin // add each image (if cancelled, this list may be shorter than the box list)

                  if readme_list.Strings[n]=''
                     then readme_str:='<SPAN STYLE="COLOR:GRAY;">read me</SPAN><A HREF="fv_no_readme.85a">&nbsp;?&nbsp;</A>'
                     else readme_str:='<A HREF="fv_readme'+IntToStr(n)+'.85a">read me</A>&nbsp; &nbsp;';

                  if fv_tag_list.Strings[n]=''
                     then fv_tag_str:='<SPAN STYLE="COLOR:GRAY;">tags</SPAN>'
                     else fv_tag_str:='<A HREF="fv_tags'+IntToStr(n)+'.85a">tags</A>';

                  next_str:='<TR><TD COLSPAN="3" STYLE="PADDING-TOP:0PX;">';

                  if images_clickable_checkbox.Checked=True
                     then next_str:=next_str+'<A HREF="fv_reload'+IntToStr(n)+'.85a"><IMG SRC="'+img_name_list.Strings[n]+'" BORDER="1"></A>'
                     else next_str:=next_str+'<IMG SRC="'+img_name_list.Strings[n]+'" BORDER="1">';

                  next_str:=next_str+'</TD></TR>'

                           +'<TR><TD COLSPAN="2">'+IntToStr(n+1)+': &nbsp;'+box_file_list.Strings[n]+'</TD>'
                           +'<TD NOWRAP ALIGN="RIGHT">'+box_size_list.Strings[n]+' &nbsp; &nbsp;'+box_time_list.Strings[n]+'</TD></TR>'

                           +'<TR><TD NOWRAP>&nbsp;<A HREF="fv_reload'+IntToStr(n)+'.85a">load this file</A>'
                           +' &nbsp;&nbsp;<SPAN STYLE="FONT-WEIGHT:NORMAL;"><A HREF="fv_add'+IntToStr(n)+'.85a">add from this file</A>'
                           +' &nbsp;&nbsp;'+fv_tag_str
                           +' &nbsp;&nbsp;'+readme_str+'</SPAN></TD>'

                           +'<TD>'
                           +'</TD>'

                           +'<TD NOWRAP ALIGN="RIGHT" STYLE="FONT-WEIGHT:NORMAL;">background templates: '+num_bgnd_list.Strings[n]+'&nbsp;'
                           +'<BR>unused templates: '+num_unused_list.Strings[n]+'&nbsp;'
                           +'<BR>library templates: '+num_lib_list.Strings[n]+'&nbsp;</TD></TR>';

                  html_str:=html_str+next_str
                           +'<TR><TD COLSPAN="3"><HR NOSHADE STYLE="COLOR:GRAY; HEIGHT:6PX;"></TD></TR>'
                           +'<TR><TD COLSPAN="3" STYLE="FONT-SIZE:4PX;"><A NAME="#file'+IntToStr(n+1)+'">&nbsp;</A></TD></TR>';

                  next_html_list.Add(next_str);  // keep this string part for possible delete / replace

                end;//next

                html_str:=html_str+html_footer_str;

                use_bmp_image_streams:=False;  // normal load images from file

                html_file_viewer.LoadFromString(html_str);

                repeat
                  windows_cursor_count:=ShowCursor(True);   // Windows SDK
                until windows_cursor_count>-1;              // ensure visible

                refresh_button.Caption:='refresh  list';
              end;//if count>0

    finally
      //disk_drive_combo.Enabled:=True;
      //folder_listbox.Enabled:=True;
      refresh_button.Enabled:=True;
      images_clickable_checkbox.Enabled:=True;
      name_labels_checkbox.Enabled:=True;
      instant_show_checkbox.Enabled:=True;

      html_file_viewer.SetFocus;  // for mouse wheel and arrow keys after selecting drive or folder.
    end;//try

  end;//with
end;
*)
//______________________________________________________________________________

procedure Tfile_viewer_form.FormCreate(Sender: TObject);

var
  folder_str:string;

begin
  ClientWidth:=1000;         // fit 1024 x 768 screens
  ClientHeight:=740;

  AutoScroll:=False;

  img_name_list:=TStringList.Create;

  box_file_list:=TStringList.Create;
  box_size_list:=TStringList.Create;
  box_time_list:=TStringList.Create;

  num_bgnd_list:=TStringList.Create;
  num_unused_list:=TStringList.Create;
  num_lib_list:=TStringList.Create;

  next_html_list:=TStringList.Create;

  readme_list:=TStringList.Create;

  fv_tag_list:=TStringList.Create;


  //files_listbox.Width:=0;   // fetches the .box file list in Items from the selected folder
                            // !!! Items not available if Visible=False
                            // FileListBox not used directly because not owner-draw


  folder_str:=ExtractFilePath(Application.ExeName)+'BOX-FILES';    // exe_str not yet set

  if DirectoryExists(folder_str)=True then folder_view.Path := folder_str;

  html_create_str:='<P ALIGN="CENTER"><BR><BR><BR><IMG SRC="'+ExtractFilePath(Application.ExeName)+'internal\hlp\wait_signal_trans.gif"></P>'
                  +'<P ALIGN="CENTER" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;"><BR><BR>to see all the .box files in the selected disk drive and folder</P>'
                  +'<P ALIGN="CENTER" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;">click the [show files] button</P>'
                  +'<P ALIGN="CENTER" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;"><BR><BR><BR>to scroll quickly through the list press repeatedly<BR>or hold down the <SPAN STYLE="COLOR:BLUE; FONT-SIZE:17PX; FONT-FAMILY:''Courier New'';">SPACEBAR</SPAN> on the keyboard</P>'
                  +'<P ALIGN="CENTER" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;"><BR><BR>for more information click the [ <A HREF="fv_help.85a">? help</A> ] button</P>'
                  +'<P ALIGN="CENTER" STYLE="FONT-SIZE:12PX;"><BR><BR><BR><BR>to avoid seeing this message every time tick the [ <A HREF="fv_instant.85a">instant show files</A> ] tickbox</P>';

  use_bmp_image_streams:=False;  // normal load images from file

  html_preview.LoadFromString('');

  bmp_stream:=TMemoryStream.Create;

end;

procedure Tfile_viewer_form.box_file_listviewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if (not Assigned(Item)) or (not Selected) then
    begin
    html_preview.LoadFromString('');
    end;

  show_file_as_bitmap(box_file_listview.Root + Item.Caption);
end;

//______________________________________________________________________________

procedure Tfile_viewer_form.FormResize(Sender: TObject);

begin
  if no_onresize=True then EXIT;

  file_panel.Width := ClientWidth - file_panel.Left;
  file_panel.Height := ClientHeight - file_panel.Top;

  controls_panel.Top:=ClientHeight-controls_panel.Height;

end;
//______________________________________________________________________________

procedure Tfile_viewer_form.folder_listboxClick(Sender: TObject);

var
  search_record_box:TSearchRec;

  top_str,dir_str:string;

  file_count:integer;


begin
  (*
  if no_onresize=True then EXIT;   // ??? scaling appears to cause a click

  //folder_listbox.OpenCurrent;   // mimics a double-click to open the folder

  if instant_show_checkbox.Checked=True     // 208g
     then begin
//            if control_room_form.viewer_png_menu_entry.Checked=True
//               then show_files_as_png
//               else show_files_as_bitmaps;
          end
     else begin                                     // 208g
            refresh_button.Caption:='show  files';

            escape_label.Hide;
            count_label.Hide;
            found_label.Hide;

            //dir_str:=folder_listbox.Directory+'\';   // add trailing slash

            file_count:=0; // init

              // count the files ...

            if FindFirst(dir_str+'*.box3',0,search_record_box)=0
               then begin
                      INC(file_count);

                      while FindNext(search_record_box)=0 do INC(file_count);
                    end;

            FindClose(search_record_box);

            top_str:='<TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="0" CELLPADDING="4" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;">'
                    +'<TR><TD NOWRAP STYLE="COLOR:#9900FF; FONT-SIZE:18PX; FONT-WEIGHT:BOLD; FONT-STYLE:ITALIC;"> &nbsp; '+IntToStr(file_count)+' &nbsp;.box &nbsp;files &nbsp;in &nbsp;this &nbsp;folder</TD></TR>'
                    +'<TR><TD> &nbsp; &nbsp; '+dir_str+'</TD></TR>'
                    +'<TR><TD ALIGN="RIGHT"><HR NOSHADE STYLE="COLOR:GRAY; HEIGHT:6PX;"></TD></TR></TABLE>';

            use_bmp_image_streams:=False;  // normal load images from file

//            html_file_viewer.LoadFromString(top_str+html_create_str);
          end;
          *)
end;
//______________________________________________________________________________
{
procedure Tfile_viewer_form.stop_buttonClick(Sender: TObject);

begin
  cancel_clicked:=True;
end;
//______________________________________________________________________________
}

procedure Tfile_viewer_form.fv_reload_file();

var
  file_str:string;

begin
  file_str := file_viewer_form.box_file_listview.Root+box_file_list.Strings[0];
  reload_specified_file(False,False,file_str);

  file_viewer_form.Close;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.fv_add_file();

var
  file_str:string;

begin
  file_str := file_viewer_form.box_file_listview.Root+box_file_list.Strings[0];
  reload_specified_file(False,True,file_str);

  file_viewer_form.Close;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.fv_tags();

var
 tags_str,list_str:string;

begin
  tags_str:=fv_tag_list.Strings[0];

  if tags_str='' then list_str:='No name prefix tags have been used in this file.'
                 else list_str:='The following prefix tags are used in this file:||'+tags_str;

  help(-8,box_file_list.Strings[0]+'||'+list_str,'');    // -8 caption = 'prefix tags'
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.fv_read_me();

begin
  help(-7,box_file_list.Strings[0]+'||'+readme_list.Strings[0],'');    // -7 caption = 'read me'
end;

//______________________________________________________________________________


procedure Tfile_viewer_form.FormDestroy(Sender: TObject);

var
  n:integer;

begin
  img_name_list.Free;

  if box_file_list.Count>0 then
    begin
    for n:=0 to box_file_list.Count-1 do
      begin
      if Assigned(box_file_list.Objects[n]) then
        begin
        TBitmap(box_file_list.Objects[n]).Free;  // free any bitmaps
        end;
      end;
    end;

  box_file_list.Free;

  box_size_list.Free;
  box_time_list.Free;

  num_bgnd_list.Free;
  num_unused_list.Free;
  num_lib_list.Free;

  next_html_list.Free;

  readme_list.Free;

  fv_tag_list.Free;

  bmp_stream.Free;

end;
//______________________________________________________________________________

procedure Tfile_viewer_form.no_readme_help(none:boolean);

const
  readme_help_str:string='green_panel_begin tree.gif To add reference notes to your storage box or data file, store a template with the name `0read me`3.'
         +'||Enter your notes as the memo notes for that template.'
         +'||It can be an existing template, or a dummy template created for the purpose.'
         +'||If it is a background template, you can click on it and then `0edit memo notes...`1 on its menu.'
         +'||Or `0add jotter to memo`1 if you prefer to enter your notes on the jotter as you work.'
         +'||(You can add memo notes to any template, but only those for a template named `0read me`3 are available as read-me notes in the file viewer.)green_panel_end';

var
  none_str:string;

begin
  if none=True then none_str:='  `0<B>&middot;</B> There are no read-me reference notes for this file.`8||'
               else none_str:='';

  if help(-7,'php/950'+none_str+readme_help_str,'more  about  the  file  viewer')=1 then file_viewer_form.help_button.Click;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.help_buttonClick(Sender: TObject);

const
  fv_help_str:string='php/950      `0.box  file  viewer`9'
  +'||Select the disk drive and folder from which you want to browse your .box template files, and then click the|`0show files`1 button.'
  +'||A screenshot image will be created from each file, and displayed in a scrollable list.'
  +'||You can jump quickly down the list one file at a time by pressing or holding down the `0SPACE`2 bar. You can jump quickly back up the list by pressing or holding down `0SHIFT+SPACE`2.'
  +' You can jump to the top or bottom of the list by pressing the `0HOME`2 or `0END`2 keys.'
  +'||green_panel_begintree.gif The screenshots will reflect your current trackpad settings for colours, generator settings etc.'
  +'||They will be created much more quickly if you change to `0skeleton&nbsp;settings`1 in the `0generator`1 menu before using the viewer.green_panel_end'
  +'|If the screenshots are taking too long to create, press the `0ESC`2 key to cancel and then split the files into several smaller folders.'
  +'||You can choose whether to include the template name labels in the screenshots by ticking or unticking the|`0template name labels`1 tickbox.'
  +'||To load a file into Templot click the `0load this file`1 link below the relevant screenshot. Loading a file closes the file viewer.'
  +'||You can choose whether the screenshot images should also be clickable to load a file by ticking or unticking the|`0images clickable`1 tickbox. If an image is clickable it will be outlined in red when the mouse pointer moves over it.'
  +'||If you click the `0add from this file`1 link the templates from the selected file will be added to any existing templates in your storage box and on the trackpad.'
  +'||Click the `0tags`1 link to see a list of the template name prefix tags used in the selected file. If the link is greyed out it means that there are no tags used in the selected file.'
  +'||Click the `0read me`1 link to see the `0read me`3 notes for the selected file. If the link is greyed out it means that there are no notes for the selected file. For more information about `0read me`3 notes, click the bar below.'
  +'||If you would prefer the list to be created instantly without waiting for you to click the `0show files`1 button, tick the `0instant show files`1 tickbox.'
  +'||For more information about using the file viewer please click <A HREF="online_ref950.85a">more information online</A>.';

begin
  if help(0,fv_help_str,'more  about  ''read  me'' notes')=1 then no_readme_help(False);

  box_file_listview.SetFocus;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.close_buttonClick(Sender: TObject);

begin
  Close;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.FormKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);

begin
  if Key=VK_ESCAPE
     then begin
            escape_pressed:=True;
            Key:=0;
          end;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.FormActivate(Sender: TObject);

var
  top_str,dir_str:string;

  file_count:integer;

  search_record_box:TSearchRec;

begin
  pad_form.Hide;  // no trackpad while viewer in use
  keep_form.Hide; // nor storage box

  if fv_has_been_active=False    // first time only
     then begin
            dir_str:=folder_view.Path+'\';   // add trailing slash

            file_count:=0; // init

              // count the files ...

            if FindFirst(dir_str+'*.box3',0,search_record_box)=0
               then begin
                      INC(file_count);

                      while FindNext(search_record_box)=0 do INC(file_count);
                    end;

            FindClose(search_record_box);

            top_str:='<TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="0" CELLPADDING="4" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;">'
                    +'<TR><TD NOWRAP STYLE="COLOR:#9900FF; FONT-SIZE:18PX; FONT-WEIGHT:BOLD; FONT-STYLE:ITALIC;"> &nbsp; '+IntToStr(file_count)+' &nbsp;.box &nbsp;files &nbsp;in &nbsp;this &nbsp;folder</TD></TR>'
                    +'<TR><TD> &nbsp; &nbsp; '+dir_str+'</TD></TR>'
                    +'<TR><TD ALIGN="RIGHT"><HR NOSHADE STYLE="COLOR:GRAY; HEIGHT:6PX;"></TD></TR></TABLE>';

            use_bmp_image_streams:=False;  // normal load images from file

            html_preview.LoadFromString(top_str+html_create_str);

            if instant_show_checkbox.Checked=True   // 208g
               then begin
               //       if control_room_form.viewer_png_menu_entry.Checked=True
               //          then show_files_as_png
               //          else show_files_as_bitmaps;
                    end
               else refresh_button.Caption:='show  files';
          end;

  fv_has_been_active:=True;

    // bug in THTMLViewer -- see Lazarus version -- middle mouse scrolling conflicts with custom cursors.

    // bug fix for THTMLViewer -- prevent middle mouse button scrolling from using our custom cursors...

  Screen.Cursors[adjust_ns_cursor_invert]:=LoadCursor(0,IDC_SIZENS);      // use N-S arrows scrolling instead.
  Screen.Cursors[adjust_we_cursor_invert]:=LoadCursor(0,IDC_SIZENS);
  Screen.Cursors[mouse_action_cursor]:=LoadCursor(0,IDC_SIZENS);

end;

procedure Tfile_viewer_form.html_file_viewerHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  htmlviewer_hot_spot_clicked(Sender, SRC, Handled);
end;

procedure Tfile_viewer_form.html_previewBitmapRequest(Sender: TObject;
  const SRC: ThtString; var Bitmap: TBitmap; var Color: TColor);
var
  n:integer;
  temp : TObject;
begin
  if (control_room_form.viewer_png_menu_entry.Checked=True)
  or (use_bmp_image_streams=False) then
    EXIT;

  try
    n:=StrToInt(SRC);
  except
    n:=0;
  end;//try

  if (n < box_file_list.Count) then
    begin
    temp := box_file_list.Objects[n];
    if Assigned(temp) then
      begin
      if (temp is Tbitmap) then
        begin
        Bitmap := TBitmap(temp);
        end;
      end;
    end;
end;

procedure Tfile_viewer_form.html_previewHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
var
  S, Dest: ThtString;
  OldPos: Integer;
  ft: THtmlFileType;

  url_str:string;     // 85A
  url_ext:string;     // 85A

begin

  url_str:=SRC;
  Handled := False;

  url_ext:=Uppercase(ExtractFileExt(url_str));

  if url_ext='.85A' then
    begin
    fv_link_clicked(url_str, Handled); // 212a  // 85A in help_sheet unit
    end;

  if not Handled then
    begin
    htmlviewer_hot_spot_clicked(Sender, SRC, Handled);
    end;
end;

procedure Tfile_viewer_form.fv_link_clicked(const url: string; var Handled: Boolean);
begin
  if url='fv_reload.85a' then
    begin
    fv_reload_file();
    Handled := true;
    EXIT;
    end;

  if url='fv_add.85a' then
    begin
    fv_add_file();
    Handled := true;
    EXIT;
    end;

  if url='fv_tags.85a' then
    begin
    fv_tags();
    Handled := true;
    EXIT;
    end;

  if url='fv_readme.85a' then
    begin
    fv_read_me();
    Handled := true;
    EXIT;
    end;

  if url='fv_no_readme.85a' then
    begin
    no_readme_help(True);
    Handled := true;
    EXIT;
    end;   // 208d

  if url='fv_help.85a' then
    begin
    help_button.Click;
    Handled := true;
    EXIT;
    end;   // 209a

  if url='fv_instant.85a' then
    begin
    instant_show_checkbox.Checked:=True;
    Handled := true;
    EXIT;
    end;   // 209a
end;



//______________________________________________________________________________

procedure Tfile_viewer_form.FormDeactivate(Sender: TObject);

begin

   // restore custom cursors for trackpad...

  Screen.Cursors[adjust_ns_cursor_invert]:=LoadCursor(HInstance,'CURSOR_ADJUST_NS_INVERT');
  Screen.Cursors[adjust_we_cursor_invert]:=LoadCursor(HInstance,'CURSOR_ADJUST_WE_INVERT');
  Screen.Cursors[mouse_action_cursor]:=LoadCursor(HInstance,'CURSOR_MOUSE_ACTION');

end;
//______________________________________________________________________________

procedure Tfile_viewer_form.refresh_buttonClick(Sender: TObject);

begin
//  if control_room_form.viewer_png_menu_entry.Checked=True
//     then show_files_as_png
//     else show_files_as_bitmaps;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.open_folder_buttonClick(Sender: TObject);

var
  dir_str:string;

begin
  box_file_listview.SetFocus;    // for return

  dir_str:=folder_view.Path;

  if ShellExecute(0,'explore',PChar(dir_str),nil,nil,SW_SHOWNORMAL)<=32
     then show_modal_message('Sorry, unable to open the folder.')
     else external_window_showing:=True;

end;
//______________________________________________________________________________

procedure Tfile_viewer_form.size_updownClick(Sender:TObject; Button:TUDBtnType);

begin
  no_onresize:=True;                            // don't permit on-resize until finished.

  if size_updown.Position>size_updown.Tag       // ! position goes up, size goes down.
     then ScaleBy(9,10);                        // scale the form contents down.

  if size_updown.Position<size_updown.Tag
     then ScaleBy(10,9);                        // scale the form contents up.

  size_updown.Tag:=size_updown.Position;        // and save for the next click.

  no_onresize:=False;                           // can now resize the contents again if he wants to.
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.html_file_viewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
{
  mouse_hover_panel.Visible:=(html_file_viewer.titleattr<>'');   // show panel if element has a title.

  if mouse_hover_panel.Visible=True
     then begin
            mouse_hover_panel.Caption:=html_file_viewer.titleattr;

            mouse_hover_panel.Left:=X+20;
            mouse_hover_panel.Top:=Y+10;
          end;
}
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.show_file_as_bitmap(file_name: string);

const
  html_header_str:string='<HTML><HEAD><TITLE>Templot file viewer</TITLE></HEAD><BODY><TABLE ALIGN="CENTER" WIDTH="100%" BORDER="0" CELLSPACING="0" CELLPADDING="4" STYLE="FONT-SIZE:15PX; FONT-WEIGHT:BOLD;">';
  html_footer_str:string='</TABLE></BODY></HTML>';

  screenshot_width:integer=676;
  screenshot_height:integer=320;

var
  i,n:integer;

  tag_count:integer;

  screen_rect,print_rect:TRect;

  create_bitmap:TBitmap;

  oldbox_str:string;  // save box for restore

  old_save_done:boolean;

  search_record_box:TSearchRec;


  next_str:string;

  tag_str,readme_str,fv_tag_str,num_files_str:string;

  hl:integer;
  append:boolean;

  windows_cursor_count:integer;

  saved_hide_name_labels:boolean;

              //////////////////////////////////////////////////////////////////

              procedure add_file_to_box_lists(search_record:TSearchRec);

              begin
                box_file_list.Add(search_record.Name);
                box_size_list.Add(IntToStr(Round(search_record.Size/1024))+' KB');
                box_time_list.Add(DateToStr(FileDateToDateTime(search_record.Time))+' &nbsp; '+TimeToStr(FileDateToDateTime(search_record.Time)));
              end;
              //////////////////////////////////////////////////////////////////

begin
  img_name_list.Clear;

  box_file_list.Clear;


  box_size_list.Clear;
  box_time_list.Clear;

  num_bgnd_list.Clear;
  num_unused_list.Clear;
  num_lib_list.Clear;

  next_html_list.Clear;

  readme_list.Clear;

  fv_tag_list.Clear;

  // build the lists ...

  if FindFirst(file_name,0,search_record_box)=0 then
    begin
    add_file_to_box_lists(search_record_box);
    end;

  FindClose(search_record_box);

  if box_file_list.Count>0
     then html_str:='<TR><TD COLSPAN="3" ALIGN="CENTER"><IMG SRC="'+exe_str+'internal\hlp\wait_signal_trans.gif"> &nbsp; please wait while the images are generated'
                   +'<BR><BR>&nbsp; &nbsp; you can stop the process by pressing the <SPAN STYLE="COLOR:BLUE; FONT-FAMILY:''COURIER NEW''; FONT-SIZE:17PX;"><B>ESC</B></SPAN> key</TD></TR>'
     else html_str:='';

  html_str:=html_header_str+html_str+html_footer_str;

  use_bmp_image_streams:=False;  // normal load images from file

  html_preview.LoadFromString(html_str);

  count_label.Caption:='0';

     // some .box files to show? ...

  if box_file_list.Count>0 then
    begin
    showing_as_png_files:=False;

    images_label.Caption:='bitmaps';

    escape_label.Show;
    count_label.Show;
    found_label.Show;

    ShowCursor(False);   // Windows SDK

      // first set up the trackpad...

    old_save_done:=save_done;      // save flag in case a confirm is needed on a reload

    oldbox_str:=exe_str+'fv.ebk';  // save existing box
    DeleteFile(oldbox_str);        // delete any previous file.
    save_box(0,0,0,oldbox_str);    // save existing contents for restore later.

    saved_pad_width:=pad_form.ClientWidth;
    saved_pad_height:=pad_form.ClientHeight;

    saved_hide_name_labels:=hide_name_labels;

    hide_name_labels:= NOT name_labels_checkbox.Checked;

    pad_form.ClientWidth:=screenshot_width;     // image size
    pad_form.ClientHeight:=screenshot_height;

    screen_rect:=Rect(0,0,screenshot_width,screenshot_height);

    print_rect:=screen_rect;

    for n:=0 to box_file_list.Count-1 do
      begin

      clear_keeps(False,False);    // clear all templates
      append:=False;


      if Pos('.box3',LowerCase(file_name))=0 then
        begin
        file_name := file_name + '.box3';  // in case his system hides extensions.
        end;

      //function load_storage_box(normal_load,old_templot_folder:boolean; file_str:string; load_backup,make_lib:boolean; var append:boolean; var last_bgnd_loaded_index:integer):boolean;    // load a file of templates into the keeps box.

      if load_storage_box(False,False,file_name,False,False,append,hl)=True then
        begin
        if keeps_list.Count>0 then
          begin
          if (loaded_version<93) and (hl>-1) and (hl<keeps_list.Count) then
            mint_final_or_copy_control(hl);   // if something loaded mint from highest bgnd if he so wants.
          if (loaded_version>92) then
            mint_final_or_copy_control(hl);                                         // copy the control template if there is one in the file.
          end;
        end;

      show_and_redraw(False,False);  // update pad

      num_bgnd_list.Add(IntToStr(any_bgnd));
      num_unused_list.Add(IntToStr(any_unused));
      num_lib_list.Add(IntToStr(any_library));

      readme_list.Add(get_readme_notes);

      tag_count := build_tag_list(False);    // tag_count not used
      tag_str := '';

      if tag_list.Count > 0 then
        begin
        for i := 0 to tag_list.Count-1 do
          begin
          tag_str := tag_str+' &nbsp; &nbsp; '+tag_list.Strings[i]+'|';   // add indent and separator
          end;//next
        end;

      fv_tag_list.Add(tag_str);

      create_bitmap:=TBitmap.Create;

      try
        create_bitmap.Width:=screenshot_width;
        create_bitmap.Height:=screenshot_height;

        create_bitmap.Canvas.CopyMode:=cmSrcCopy;
        create_bitmap.Canvas.CopyRect(print_rect,offdraw_bmp.Canvas,screen_rect);

        with create_bitmap.Canvas do
          begin

          Brush.Style:=bsSolid;
          Brush.Color:=clYellow;

          Font.Color:=clBlack;
          Font.Height:=0-13;
          Font.Style:=[fsBold];
          Font.Name:='Arial';

          TextOut(0,0,' '+IntToStr(n+1)+': '+fv_gauge_str+' '+box_project_title_str+' ');     // add the short gauge label

          end;//with

        box_file_list.Objects[n]:=create_bitmap;

      except
        show_modal_message('Sorry, an error occurred in creating image:'+#13+#13+IntToStr(n+1)+' : '+box_file_list.Strings[n]);
        create_bitmap.Free;
        end;//try

      Application.ProcessMessages;  // for pressing ESC key

      if escape_pressed=True then
        BREAK;

      end;//next

    // restore trackpad...

    clear_keeps(False,False);    // clear all templates
    append:=False;

    if load_storage_box(False,False,oldbox_str,False,False,append,hl)=True then
      begin
      if keeps_list.Count>0 then
        begin
        if (loaded_version<93) and (hl>-1) and (hl<keeps_list.Count) then
          mint_final_or_copy_control(hl);   // if something loaded mint from highest bgnd if he so wants.
        if (loaded_version>92) then
          mint_final_or_copy_control(hl);                                         // copy the control template if there is one in the file.
        end;
      end;

    pad_form.ClientWidth := saved_pad_width;
    pad_form.ClientHeight := saved_pad_height;

    hide_name_labels := saved_hide_name_labels;

    show_and_redraw(False,False);  // update pad

    if any_bgnd<1 then
      pad_form.fit_current_only_menu_entry.Click;

    save_done := old_save_done;      // restore flag in case a confirm is needed on a reload



    // now create the HTML view...

    progress_bar.Position:=0;

    html_str:=html_header_str;

    if readme_list.Count=1 then
      num_files_str:=' file'
    else
      num_files_str:=' files';

    html_str:=html_str+'<TR><TD COLSPAN="3" STYLE="FONT-SIZE:4PX;"><A NAME="#file0">&nbsp;</A></TD></TR>';

    for n:=0 to readme_list.Count-1 do
      begin // add each image (if cancelled, this list may be shorter than the box list)

      if readme_list.Strings[n]='' then
        readme_str:='<SPAN STYLE="COLOR:GRAY;">read me</SPAN><A HREF="fv_no_readme.85a">&nbsp;?&nbsp;</A>'
      else
        readme_str:='<A HREF="fv_readme.85a">read me</A>&nbsp; &nbsp;';

      if fv_tag_list.Strings[n]='' then
        fv_tag_str:='<SPAN STYLE="COLOR:GRAY;">tags</SPAN>'
      else
        fv_tag_str:='<A HREF="fv_tags.85a">tags</A>';

      next_str:='<TR><TD COLSPAN="3" STYLE="PADDING-TOP:0PX;">';

      if images_clickable_checkbox.Checked=True then
        next_str:=next_str+'<A HREF="fv_reload.85a"><IMG SRC="'+IntToStr(n)+'" BORDER="1"></A>'
      else
        next_str:=next_str+'<IMG SRC="'+IntToStr(n)+'" BORDER="1">';

      next_str:=next_str+'</TD></TR>'

                       +'<TR><TD COLSPAN="2">'+box_file_list.Strings[n]+'</TD>'
                       +'<TD NOWRAP ALIGN="RIGHT">'+box_size_list.Strings[n]+' &nbsp; &nbsp;'+box_time_list.Strings[n]+'</TD></TR>'

                       +'<TR><TD NOWRAP>&nbsp;<A HREF="fv_reload.85a">load this file</A>'
                       +' &nbsp;&nbsp;<SPAN STYLE="FONT-WEIGHT:NORMAL;"><A HREF="fv_add.85a">add from this file</A>'
                       +' &nbsp;&nbsp;'+fv_tag_str
                       +' &nbsp;&nbsp;'+readme_str+'</SPAN></TD>'

                       +'<TD></TD>'

                       +'<TD NOWRAP ALIGN="RIGHT" STYLE="FONT-WEIGHT:NORMAL;">background templates: '+num_bgnd_list.Strings[n]+'&nbsp;'
                       +'<BR>unused templates: '+num_unused_list.Strings[n]+'&nbsp;'
                       +'<BR>library templates: '+num_lib_list.Strings[n]+'&nbsp;</TD></TR>';

      html_str:=html_str+next_str
                       +'<TR><TD COLSPAN="3"><HR NOSHADE STYLE="COLOR:GRAY; HEIGHT:6PX;"></TD></TR>'
                       +'<TR><TD COLSPAN="3" STYLE="FONT-SIZE:4PX;"><A NAME="#file'+IntToStr(n+1)+'">&nbsp;</A></TD></TR>';

      next_html_list.Add(next_str);  // keep this string part for possible delete / replace

      end;//next

    html_str:=html_str+html_footer_str;

    use_bmp_image_streams:=True;  // use OnImageRequest to get the images

    html_preview.LoadFromString(html_str);

    use_bmp_image_streams:=False; // restore normal image files

    repeat
      windows_cursor_count:=ShowCursor(True);   // Windows SDK
    until windows_cursor_count>-1;              // ensure visible

    refresh_button.Caption:='refresh  list';
    end;//if count>0

end;

//______________________________________________________________________________

procedure Tfile_viewer_form.FormClose(Sender:TObject; var Action:TCloseAction);

begin
  pad_form.Show; // trackpad visible again

  if keep_form_was_showing=True then pad_form.view_box_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tfile_viewer_form.instant_show_checkboxClick(Sender: TObject);

begin
  if instant_show_checkbox.Checked=True then refresh_button.Click; // 208g
end;
//______________________________________________________________________________

end.

