
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

unit image_viewer_unit;

{$MODE Delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Clipbrd, ExtCtrls, StdCtrls, Menus;

type

  { Timage_viewer_form }

  Timage_viewer_form = class(TForm)
    t3_logo_bmp_image: TImage;
    top_label: TLabel;
    tickbox_unticked_png_image: TImage;
    tickbox_ticked_png_image: TImage;
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

procedure show_an_image_file(file_str: string; emf_width, emf_height: integer; show_form: boolean);

//______________________________________________________________________________

implementation

{$R *.lfm}

uses
  LCLIntf,
  config_unit, control_room, alert_unit, math_unit;

var
  image_file_str: string = '';

  emf_showing: boolean = False;
  emf_width_showing: integer = 800;     // init
  emf_height_showing: integer = 600;

//______________________________________________________________________________

procedure show_an_image_file(file_str: string; emf_width, emf_height: integer; show_form: boolean);

  // This unit is only called from bgnd_unit, and it is commented out there
  // as part of OT-FIRST.  This function is therefore also commented out to allow
  // compilation on Linux. It will need to be
  // reworked as part of the EMF fixer uppering.  :-)
  //                                                    GDT : 15/9/21


// emf_width, emf_height ignored for bitmaps       291a
//
//var
//  output_rect: Trect;
//  load_DC: HDC;
//
begin
  //if FileExists(file_str) = False         // 291a
  //then begin
  //  show_modal_message('error: unable to find image file');
  //  EXIT;
  //end;
  //
  //emf_showing := False;
  //
  //image_viewer_form.top_label.Caption := '';
  //
  //if (LowerCase(ExtractFileExt(file_str)) = '.emf')       // 291a    show metafile
  //then begin
  //
  //  with image_viewer_form do begin
  //
  //    copy_image_menu_entry.Caption := 'copy  EMF  metafile  as  bitmap  image';
  //
  //    viewer_image.AutoSize := False;
  //
  //    viewer_image.Width := ClientWidth;
  //    viewer_image.Height := Round(ClientWidth * emf_height / emf_width);
  //
  //    viewer_image.Picture.Bitmap.Width := viewer_image.Width;
  //    viewer_image.Picture.Bitmap.Height := viewe r_image.Height;
  //
  //    output_rect := Rect(0, 0, viewer_image.Picture.Bitmap.Width,
  //      viewer_image.Picture.Bitmap.Height);
  //
  //    with viewer_image.Picture.Bitmap.Canvas do begin
  //
  //      Brush.Color := clWhite;     // blank it first
  //      Brush.Style := bsSolid;
  //
  //      FillRect(output_rect);
  //
  //    end;//with
  //
  //    try
  //      load_DC := GetEnhMetaFile(PChar(file_str));  // get metafile handle
  //
  //      if PlayEnhMetaFile(viewer_image.Picture.Bitmap.Canvas.Handle, load_DC, output_rect) =
  //        False    // draw metafile on canvas
  //      then
  //        show_modal_message('error: Sorry, unable to show the EMF metafile image.');
  //
  //      DeleteEnhMetaFile(load_DC);   // release metafile handle
  //    except
  //      show_modal_message('error: Sorry, unable to show EMF image');
  //    end;
  //
  //    top_label.Caption :=
  //      'The EMF metafile is being displayed to fit. To zoom in and examine detail, view the file on the sketchboard.';
  //
  //  end;//with form
  //
  //  // retain globals ...
  //
  //  emf_showing := True;
  //  emf_width_showing := emf_width;
  //  emf_height_showing := emf_height;
  //
  //end
  //else begin
  //
  //  with image_viewer_form do begin
  //
  //    copy_image_menu_entry.Caption := 'copy  image';
  //
  //    viewer_image.AutoSize := True;    // 291a
  //
  //    try
  //      viewer_image.Picture.LoadFromFile(file_str);
  //    except
  //      show_modal_message('Sorry, unable to display the image.');
  //    end;//try
  //
  //    top_label.Caption :=
  //      'The image is being displayed full size. You may need to scroll to see it.';
  //
  //  end;//with
  //
  //end;
  //
  //image_file_str := file_str;
  //
  //image_viewer_form.Caption := '    image:  ' + file_str;
  //
  //with image_viewer_form do begin
  //
  //  cancel_menu.Visible := False;  // used for scanned picture shapes
  //  options_menu.Visible := True;  // hidden for scanned picture shapes
  //end;//with
  //
  //if show_form = True then
  //  do_show_modal(image_viewer_form);   // 212a ShowModal

end;
//______________________________________________________________________________

procedure Timage_viewer_form.open_containing_folder_menu_entryClick(Sender: TObject);

var
  folder_str: string;

begin
  folder_str := ExtractFilePath(image_file_str);

  if not OpenDocument(folder_str) then
    show_modal_message('Sorry, unable to open the folder.')
  else
    external_window_showing := True;

end;
//______________________________________________________________________________
(*
procedure Timage_viewer_form.FormResize(Sender: TObject);      // 291a

begin
  if emf_showing=True
     then show_an_image_file(image_file_str,emf_width_showing,emf_height_showing,False);     // False=don't re-show form (modal)
end;
//______________________________________________________________________________

procedure Timage_viewer_form.FormShow(Sender: TObject);

begin
  emf_showing:=False;  // init
end;
//______________________________________________________________________________
*)

procedure Timage_viewer_form.copy_image_menu_entryClick(Sender: TObject);

begin
  try
    Clipboard.Assign(viewer_image.Picture.Graphic);
  except
    show_modal_message('Sorry, unable to copy this image to the Windows clipboard.');
    EXIT;
  end;

  show_modal_message('The image has been copied to the Windows clipboard.'
    + #13 + #13 + 'It can now be pasted into any graphics program.');
end;
//______________________________________________________________________________

procedure Timage_viewer_form.copy_file_location_menu_entryClick(Sender: TObject);

begin
  Clipboard.AsText := image_file_str;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.close_menu_entryClick(Sender: TObject);

begin
  ModalResult := mrCancel;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.copy_file_name_menu_entryClick(Sender: TObject);

begin
  Clipboard.AsText := ExtractFileName(image_file_str);
end;
//______________________________________________________________________________

procedure Timage_viewer_form.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = Chr(27) then begin
    Key := Chr(0);
    ModalResult := mrCancel;
  end;

end;
//______________________________________________________________________________

procedure Timage_viewer_form.use_your_viewer_menu_entryClick(Sender: TObject);

begin
  if not OpenDocument(image_file_str) then
    show_modal_message('Sorry, unable to display the image in your viewer.')
  else begin
    external_window_showing := True;
    ModalResult := mrCancel;
  end;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.delete_image_menu_entryClick(Sender: TObject);

begin
  if alert(7, '      delete  image ?', 'You are about to delete this image file.'
    + '||This can not be undone.', '', '', '', '', 'cancel',
    'delete  image', 0) = 5 then
    EXIT;

  if DeleteFile(image_file_str) = False then
    show_modal_message('Sorry, unable to delete ' + image_file_str)
  else
    ModalResult := mrCancel;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.options_menuClick(Sender: TObject);

begin
  image_size_menu_entry.Caption := 'image  size :  ' + IntToStr(viewer_image.Width) +
    ' x ' + IntToStr(viewer_image.Height);
end;
//______________________________________________________________________________

procedure Timage_viewer_form.image_size_menu_entryClick(Sender: TObject);

begin
  show_modal_message('This image is ' + IntToStr(viewer_image.Width) + ' x ' +
    IntToStr(viewer_image.Height) + ' pixels.');
end;
//______________________________________________________________________________

procedure Timage_viewer_form.FormCreate(Sender: TObject);

// ---------------------------------------------------------------
  procedure store_image(csfi: TconfigSystemFileID; img: Timage);
  var
    img_path_str: string;
  begin
    img_path_str := Config.GetFilePath(csfi);
    if not FileExists(img_path_str) then
      img.Picture.SaveToFile(img_path_str);
  end;
// ---------------------------------------------------------------


begin
  ClientWidth := 1000;
  ClientHeight := 650;

  AutoScroll := True;

  // these Timage components are not visible on the form. used on first run to create the image files for HtmlViewer and maps

  store_image(csfiOTlogo, ot_logo_bmp_image);
  store_image(csfiAdobePrint, adobe_print_dialog_png_image);
  store_image(csfiB6Startup, b6_startup_gif_image);
  store_image(csfiCompanion, companion_taskbar_png_image);
  store_image(csfiFullScreen, full_screen_png_image);
  store_image(csfiGreenHelmet, green_helmet_gif_image);
  store_image(csfiLinkView, link_view_png_image);
  store_image(csfiOutputMenu, output_menu_png_image);
  store_image(csfiPadColours, pad_colours_gif_image);
  store_image(csfiRedPointer, red_pointer_gif_image);
  store_image(csfiRedoChanges, redo_changes_png_image);
  store_image(csfiSavedPrefs, saved_prefs_png_image);
  store_image(csfiSBshowItems, sb_show_items_png_image);
  store_image(csfiScangearDPI, scangear_dpi_png_image);
  store_image(csfiScriptError, script_error_png_image);
  store_image(csfiSBsample, sketchboard_sample_gif_image);
  store_image(csfiSmile, smile_gif_image);
  store_image(csfiStoreBgnd, store_bgnd_options_png_image);
  store_image(csfiTreeSymbol, tree_symbol_gif_image);
  store_image(csfiUndoChanges, undo_changes_png_image);
  store_image(csfiViewerLoading, viewer_loading_png_image);
  store_image(csfiWaitSignal, wait_signal_png_image);
  store_image(csfiWaitSignalTrans, wait_signal_trans_gif_image);
  store_image(csfiUntickedBox, tickbox_unticked_png_image);
  store_image(csfiTickedBox, tickbox_ticked_png_image);

  // for map_loader ...

  store_image(csfiOSMcopyright, osm_copyright_png_image);
  store_image(csfiNLScopyright, nls_copyright_png_image);

  // for picture shapes ...

  store_image(csfiEmptyPic, empty_picture_bmp_image);

end;
//______________________________________________________________________________

procedure Timage_viewer_form.ok_menuClick(Sender: TObject);

begin
  ModalResult := mrOk;
end;
//______________________________________________________________________________

procedure Timage_viewer_form.cancel_menuClick(Sender: TObject);

begin
  ModalResult := mrCancel;
end;
//______________________________________________________________________________

end.
