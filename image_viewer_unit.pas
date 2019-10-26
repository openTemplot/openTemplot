
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

unit image_viewer_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellApi, Clipbrd, ExtCtrls, StdCtrls, Menus;

type

  { Timage_viewer_form }

  Timage_viewer_form = class(TForm)
    nls_copyright_png_image: TImage;
    osm_copyright_png_image: TImage;
    empty_picture_bmp_image: TImage;
    ot_logo_bmp_image: TImage;
    wait_signal_trans_gif_image: TImage;
    image0: TImage;
    link_view_png_image: TImage;
    output_menu_png_image: TImage;
    pad_colours_gif_image: TImage;
    red_pointer_gif_image: TImage;
    redo_changes_png_image: TImage;
    saved_prefs_gif_image: TImage;
    saved_prefs_png_image: TImage;
    tm_logo_bmp_image: TImage;
    sb_show_items_png_image: TImage;
    scangear_dpi_png_image: TImage;
    script_error_png_image: TImage;
    adobe_print_dialog_png_image: TImage;
    full_screen_png_image: TImage;
    companion_taskbar_png_image: TImage;
    b6_startup_gif_image: TImage;
    green_helmet_gif_image: TImage;
    sketchboard_sample_gif_image: TImage;
    smile_gif_image: TImage;
    store_bgnd_options_png_image: TImage;
    tree_symbol_gif_image: TImage;
    undo_changes_png_image: TImage;
    viewer_loading_png_image: TImage;
    wait_signal_png_image: TImage;
    viewer_image: TImage;
    menu_bar: TMainMenu;
    options_menu: TMenuItem;
    open_containing_folder_menu_entry: TMenuItem;
    copy_image_menu_entry: TMenuItem;
    close_menu_entry: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    copy_file_location_menu_entry: TMenuItem;
    N3: TMenuItem;
    copy_file_name_menu_entry: TMenuItem;
    use_your_viewer_menu_entry: TMenuItem;
    delete_image_menu_entry: TMenuItem;
    N4: TMenuItem;
    image_size_menu_entry: TMenuItem;
    N5: TMenuItem;
    datestamp_label: TLabel;
    ok_menu: TMenuItem;
    cancel_menu: TMenuItem;
    procedure open_containing_folder_menu_entryClick(Sender: TObject);
    procedure copy_image_menu_entryClick(Sender: TObject);
    procedure copy_file_location_menu_entryClick(Sender: TObject);
    procedure close_menu_entryClick(Sender: TObject);
    procedure copy_file_name_menu_entryClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure use_your_viewer_menu_entryClick(Sender: TObject);
    procedure delete_image_menu_entryClick(Sender: TObject);
    procedure options_menuClick(Sender: TObject);
    procedure image_size_menu_entryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ok_menuClick(Sender: TObject);
    procedure cancel_menuClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  image_viewer_form: Timage_viewer_form;

  procedure show_an_image_file(file_str:string);

implementation

{$R *.lfm}

uses
  control_room, alert_unit, math_unit;

var
  image_file_str:string='';

//______________________________________________________________________________

procedure show_an_image_file(file_str:string);

begin
  try
    image_viewer_form.viewer_image.Picture.LoadFromFile(file_str);
  except
    show_modal_message('Sorry, unable to display the image.');
    EXIT;
  end;//try

  show_modal_message('The image will be displayed full size. You may need to scroll the viewer to see anything.');

  image_file_str:=file_str;

  image_viewer_form.Caption:='    image:  '+file_str;

  with image_viewer_form do begin

    cancel_menu.Visible:=False;  // used for scanned picture shapes
    options_menu.Visible:=True;  // hidden for scanned picture shapes
  end;//with

  do_show_modal(image_viewer_form);   // 212a ShowModal

end;
//______________________________________________________________________________

procedure Timage_viewer_form.open_containing_folder_menu_entryClick(Sender: TObject);

var
  folder_str:string;

begin
  folder_str:=ExtractFilePath(image_file_str);

  if ShellExecute(0,'explore',PChar(folder_str),nil,nil,SW_SHOWNORMAL)<=32
     then show_modal_message('Sorry, unable to open the folder.')
     else external_window_showing:=True;

end;
//______________________________________________________________________________

procedure Timage_viewer_form.copy_image_menu_entryClick(Sender: TObject);

begin
  try
    Clipboard.Assign(viewer_image.Picture.Graphic);
  except
    show_modal_message('Sorry, unable to copy this image to the Windows clipboard.');
    EXIT;
  end;

  show_modal_message('The image has been copied to the Windows clipboard.'
              +#13+#13+'It can now be pasted into any graphics program.');
end;
//______________________________________________________________________________

procedure Timage_viewer_form.copy_file_location_menu_entryClick(Sender:TObject);

begin
  Clipboard.AsText:=image_file_str;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.close_menu_entryClick(Sender: TObject);

begin
  ModalResult:=mrCancel;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.copy_file_name_menu_entryClick(Sender:TObject);

begin
  Clipboard.AsText:=ExtractFileName(image_file_str);
end;
//______________________________________________________________________________

procedure Timage_viewer_form.FormKeyPress(Sender:TObject; var Key:Char);

begin
  if Key=Chr(27)
     then begin
            Key:=Chr(0);
            ModalResult:=mrCancel;
          end;

end;
//______________________________________________________________________________

procedure Timage_viewer_form.use_your_viewer_menu_entryClick(Sender:TObject);

begin
  if ShellExecute(0,'open',PChar(image_file_str),nil,nil,SW_SHOWNORMAL)<=32
     then show_modal_message('Sorry, unable to display the image in your viewer.')
     else begin
            external_window_showing:=True;
            ModalResult:=mrCancel;
          end;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.delete_image_menu_entryClick(Sender:TObject);

begin
  if alert(7,'      delete  image ?',
            'You are about to delete this image file.'
           +'||This can not be undone.',
             '','','','','cancel','delete  image',0)=5
     then EXIT;

  if DeleteFile(image_file_str)=False
     then show_modal_message('Sorry, unable to delete '+image_file_str)
     else ModalResult:=mrCancel;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.options_menuClick(Sender:TObject);

begin
  image_size_menu_entry.Caption:='image  size :  '+IntToStr(viewer_image.Width)+' x '+IntToStr(viewer_image.Height);
end;
//______________________________________________________________________________

procedure Timage_viewer_form.image_size_menu_entryClick(Sender: TObject);

begin
  show_modal_message('This image is '+IntToStr(viewer_image.Width)+' x '+IntToStr(viewer_image.Height)+' pixels.');
end;
//______________________________________________________________________________

procedure Timage_viewer_form.FormCreate(Sender: TObject);

var
  img_path_str:string;

begin
  // OT-FIRST ClientWidth:=1000;
  // OT-FIRST ClientHeight:=650;

  AutoScroll:=True;

    // these Timage components are not visible on the form. used on first run to create the image files for HtmlViewer and maps    OT-FIRST  ...

  ForceDirectories(ExtractFilePath(Application.ExeName)+'internal\hlp');
  ForceDirectories(ExtractFilePath(Application.ExeName)+'internal\tile');

  img_path_str:=ExtractFilePath(Application.ExeName)+'internal\hlp\';

  if FileExists(img_path_str+'ot_logo.bmp')=False then ot_logo_bmp_image.Picture.SaveToFile(img_path_str+'ot_logo.bmp');

  if FileExists(img_path_str+'tm_logo.bmp')=False then tm_logo_bmp_image.Picture.SaveToFile(img_path_str+'tm_logo.bmp');

  if FileExists(img_path_str+'adobe_print_dialog.png')=False then adobe_print_dialog_png_image.Picture.SaveToFile(img_path_str+'adobe_print_dialog.png');

  if FileExists(img_path_str+'b6_startup.gif')=False then b6_startup_gif_image.Picture.SaveToFile(img_path_str+'b6_startup.gif');

  if FileExists(img_path_str+'companion_taskbar.png')=False then companion_taskbar_png_image.Picture.SaveToFile(img_path_str+'companion_taskbar.png');

  if FileExists(img_path_str+'full_screen.png')=False then full_screen_png_image.Picture.SaveToFile(img_path_str+'full_screen.png');

  if FileExists(img_path_str+'green_helmet.gif')=False then green_helmet_gif_image.Picture.SaveToFile(img_path_str+'green_helmet.gif');

  if FileExists(img_path_str+'link_view.png')=False then link_view_png_image.Picture.SaveToFile(img_path_str+'link_view.png');

  if FileExists(img_path_str+'output_menu.png')=False then output_menu_png_image.Picture.SaveToFile(img_path_str+'output_menu.png');

  if FileExists(img_path_str+'pad_colours.gif')=False then pad_colours_gif_image.Picture.SaveToFile(img_path_str+'pad_colours.gif');

  if FileExists(img_path_str+'red_pointer.gif')=False then red_pointer_gif_image.Picture.SaveToFile(img_path_str+'red_pointer.gif');

  if FileExists(img_path_str+'redo_changes.png')=False then redo_changes_png_image.Picture.SaveToFile(img_path_str+'redo_changes.png');

  if FileExists(img_path_str+'saved_prefs.gif')=False then saved_prefs_gif_image.Picture.SaveToFile(img_path_str+'saved_prefs.gif');

  if FileExists(img_path_str+'saved_prefs.png')=False then saved_prefs_png_image.Picture.SaveToFile(img_path_str+'saved_prefs.png');

  if FileExists(img_path_str+'sb_show_items.png')=False then sb_show_items_png_image.Picture.SaveToFile(img_path_str+'sb_show_items.png');

  if FileExists(img_path_str+'scangear_dpi.png')=False then scangear_dpi_png_image.Picture.SaveToFile(img_path_str+'scangear_dpi.png');

  if FileExists(img_path_str+'script_error.png')=False then script_error_png_image.Picture.SaveToFile(img_path_str+'script_error.png');

  if FileExists(img_path_str+'sketchboard_sample.gif')=False then sketchboard_sample_gif_image.Picture.SaveToFile(img_path_str+'sketchboard_sample.gif');

  if FileExists(img_path_str+'smile.gif')=False then smile_gif_image.Picture.SaveToFile(img_path_str+'smile.gif');

  if FileExists(img_path_str+'store_bgnd_options.png')=False then store_bgnd_options_png_image.Picture.SaveToFile(img_path_str+'store_bgnd_options.png');

  if FileExists(img_path_str+'tree_symbol.gif')=False then tree_symbol_gif_image.Picture.SaveToFile(img_path_str+'tree_symbol.gif');

  if FileExists(img_path_str+'undo_changes.png')=False then undo_changes_png_image.Picture.SaveToFile(img_path_str+'undo_changes.png');

  if FileExists(img_path_str+'viewer_loading.png')=False then viewer_loading_png_image.Picture.SaveToFile(img_path_str+'viewer_loading.png');

  if FileExists(img_path_str+'wait_signal.png')=False then wait_signal_png_image.Picture.SaveToFile(img_path_str+'wait_signal.png');

  if FileExists(img_path_str+'wait_signal_trans.gif')=False then wait_signal_trans_gif_image.Picture.SaveToFile(img_path_str+'wait_signal_trans.gif');

     // for map_loader ...

  img_path_str:=ExtractFilePath(Application.ExeName)+'internal\tile\';

  if FileExists(img_path_str+'osm_copyright.png')=False then osm_copyright_png_image.Picture.SaveToFile(img_path_str+'osm_copyright.png');
  if FileExists(img_path_str+'nls_copyright.png')=False then nls_copyright_png_image.Picture.SaveToFile(img_path_str+'nls_copyright.png');

     // for picture shapes ...

  img_path_str:=ExtractFilePath(Application.ExeName)+'internal\';

  if FileExists(img_path_str+'empty_picture.bmp')=False then empty_picture_bmp_image.Picture.SaveToFile(img_path_str+'empty_picture.bmp');

end;
//______________________________________________________________________________

procedure Timage_viewer_form.ok_menuClick(Sender: TObject);

begin
  ModalResult:=mrOk;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.cancel_menuClick(Sender: TObject);

begin
  ModalResult:=mrCancel;
end;
//______________________________________________________________________________

end.

