
(*

    This file is part of Templot3, a computer program for the design of model railway track.
    Copyright (C) 2019  Martin Wynne.  email: martin@templot.com


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

unit export_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, Utils;

type

  { Texport_form }

  Texport_form = class(TForm)
    bgnd_shape_image: TImage;
    include_groupbox: TGroupBox;
    export_all_radiobutton: TRadioButton;
    export_group_only_radiobutton: TRadioButton;
    export_control_template_radiobutton: TRadioButton;
    pdf_groupbox: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    pdf_long_mm_edit: TEdit;
    pdf_short_mm_edit: TEdit;
    pdf_dpi_edit: TEdit;
    create_pdf_button: TButton;
    pdf_help_button: TButton;
    Label9: TLabel;
    image_groupbox: TGroupBox;
    export_metafile_groupbox: TGroupBox;
    emf_dpi_label: TLabel;
    dpi_label: TLabel;
    metafile_dpi_edit: TEdit;
    create_metafile_button: TButton;
    metafiles_help_button: TButton;
    png_groupbox: TGroupBox;
    image_width_edit: TEdit;
    create_image_file_button: TButton;
    png_help_button: TButton;
    help_button: TButton;
    Label14: TLabel;
    save_static_label: TStaticText;
    datestamp_label: TLabel;
    dxf_groupbox: TGroupBox;
    Label2: TLabel;
    export_dxf_button: TButton;
    Label15: TLabel;
    sketchboard_static_label: TStaticText;
    style_groupbox: TGroupBox;
    Label17: TLabel;
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    hide_panel: TPanel;
    hide_button: TButton;
    Shape1: TShape;
    Label18: TLabel;
    boundary_groupbox: TGroupBox;
    fit_all_radiobutton: TRadioButton;
    use_current_rectangle_radiobutton: TRadioButton;
    save_emf_dialog: TSavePictureDialog;
    image_rectangle_groupbox: TGroupBox;
    draw_export_rectangle_button: TButton;
    rectangle_clear_button: TButton;
    dummy_button: TButton;
    pdf_end_run_radiobutton: TRadioButton;
    pdf_side_run_button: TRadioButton;
    rectangle_set_button: TButton;
    save_imagefile_dialog: TSavePictureDialog;
    export_include_grid_checkbox: TCheckBox;
    export_include_grid_labels_checkbox: TCheckBox;
    export_include_picture_shapes_checkbox: TCheckBox;
    fit_shapes_radiobutton: TRadioButton;
    export_colour_radiobutton: TRadioButton;
    export_grey_radiobutton: TRadioButton;
    export_black_radiobutton: TRadioButton;
    Label10: TLabel;
    Label1: TLabel;
    Label11: TLabel;
    export_include_picture_borders_checkbox: TCheckBox;
    pdf_size_inside_trim_margins_checkbox: TCheckBox;
    export_track_background_checkbox: TCheckBox;
    img_bgnd_colour_panel: TPanel;
    img_bgnd_button: TButton;
    Label12: TLabel;
    Label13: TLabel;
    procedure save_static_labelClick(Sender: TObject);
    procedure sketchboard_static_labelClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure hide_panelClick(Sender: TObject);
    procedure export_dxf_buttonClick(Sender: TObject);
    procedure create_pdf_buttonClick(Sender: TObject);
    procedure create_metafile_buttonClick(Sender: TObject);
    procedure draw_export_rectangle_buttonClick(Sender: TObject);
    procedure rectangle_clear_buttonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure create_image_file_buttonClick(Sender: TObject);
    procedure rectangle_set_buttonClick(Sender: TObject);
    procedure export_include_grid_checkboxClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure png_help_buttonClick(Sender: TObject);
    procedure export_include_picture_shapes_checkboxClick(Sender: TObject);
    procedure img_bgnd_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  export_form: Texport_form;

  export_form_was_showing: boolean = False;

  output_rectangle_x1: double = 0;     // export image rectangle mm...
  output_rectangle_y1: double = 0;

  output_rectangle_x2: double = 1200;  // arbitrary default 1200mm x 600mm (4ft x 2ft)
  output_rectangle_y2: double = 600;

  output_rectangle_width: double = 1200;
  output_rectangle_height: double = 600;

  his_image_file_name: string = '';
  his_emf_file_name: string = '';

  //pdf_head_factor:double=1.0;
  //pdf_roller_factor:double=1.0;

  // OT-FIRST moved from dtp_unit ...

  export_black_white: boolean = False;
  export_grey_shade: boolean = False;

  track_bgnd_width_in: double = 288;  // 24ft default  206a

procedure set_boundary_rectangle_dims(calling_form: TForm);

function do_metafile(file_str: string; met_width_dots, met_height_dots: integer): boolean;

//procedure sb_draw(on_canvas:TCanvas; canvas_width,canvas_height,output_code:integer);    // draw control template or entire pad on a bitmap or metafile. OT-FIRST moved from dtp_unit

//procedure export_bgnd_shapes(on_canvas:TCanvas; canvas_height:integer; grid_left,grid_top:double; output_code:integer);  // print all background shapes.

//procedure export_bgnd(on_canvas:TCanvas; canvas_height:integer; grid_left,grid_top:double; output_code:integer);        // print background templates.


implementation

uses ShellAPI, control_room, point_ex, pad_unit, keep_select, math_unit, preview_unit, entry_sheet, help_sheet,
     { OT-FIRST dtp_unit, dtp_settings_unit,} alert_unit, grid_unit, pdf_unit, print_settings_unit,colour_unit,
  bgnd_unit, dxf_unit, image_viewer_unit,
  export_draw_unit;  // 291a

//print_unit;    // OT-FIRST for calc_intensity

{$R *.lfm}

var
  no_onresize: boolean = False;

//______________________________________________________________________________

procedure Texport_form.save_static_labelClick(Sender: TObject);

begin
  pad_form.pad_save_all_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Texport_form.sketchboard_static_labelClick(Sender: TObject);

begin
  pad_form.sketchboard_button.Click;
end;
//______________________________________________________________________________

procedure Texport_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  no_onresize := True;                            // don't permit on-resize until finished.

  if size_updown.Position > size_updown.Tag       // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                        // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                        // scale the form contents up.

  ClientHeight := VertScrollBar.Range + 4;          // allow 8 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 8;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range + 4;          // do this twice, as each affects the other.


  size_updown.Tag := size_updown.Position;        // and save for the next click.

  no_onresize := False;                           // can now resize the contents again if he wants to.
end;
//______________________________________________________________________________

procedure Texport_form.FormResize(Sender: TObject);

begin
  if no_onresize = True then
    EXIT;
end;
//______________________________________________________________________________

procedure Texport_form.FormCreate(Sender: TObject);

begin
  ClientWidth := 704;
  ClientHeight := 588;

  AutoScroll := True;
end;
//______________________________________________________________________________

procedure Texport_form.hide_panelClick(Sender: TObject);

begin
  Close; //Hide;
  if show_margins = 2 then
    redraw_pad(True, False);  // show PDF outlines if page size changed
end;
//______________________________________________________________________________

procedure Texport_form.export_dxf_buttonClick(Sender: TObject);

begin
  if export_all_radiobutton.Checked = True then
    dxf_form.all_option_button.Checked := True;   // radio item    0.97.c

  if export_group_only_radiobutton.Checked = True then
    dxf_form.group_option_button.Checked := True;   // radio item  0.97.c

  export_templates_dxf;        // go export a DXF file.  //keep_form.export_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Texport_form.create_pdf_buttonClick(Sender: TObject);

var
  temp: double;

  box_value_dpi: integer;      // 205e ...
  box_value_long: double;
  box_value_short: double;

begin
  if check_int(pdf_dpi_edit.text,50,4800,box_value_dpi)=False          // input limits 50dpi to 4800dpi
     then begin
            ShowMessage('Error: The DPI setting must be a valid whole number in the range 50 to 4800. A decimal point is not allowed.');
            EXIT;
          end;

  if check_float(pdf_long_mm_edit.text,50,25000,box_value_long)=False   // input limits 50mm to 25000mm (25m)
     then begin
            ShowMessage('Error: The long-side page dimension must be a valid number in the range 50mm to 25000mm.');
            EXIT;
          end;

  if check_float(pdf_short_mm_edit.text,25,12500,box_value_short)=False  // input limits 25mm to 12500mm (12.5m)
     then begin
            ShowMessage('Error: The short-side page dimension must be a valid number in the range 25mm to 12500mm.');
            EXIT;
          end;

  try
    pdf_width_dpi:=box_value_dpi;
    pdf_height_dpi:=pdf_width_dpi;                      // both the same

    pdf_height_mm:=box_value_long;
    pdf_width_mm:=box_value_short;
  except
    EXIT;    // ??? should have been found in sb_check_valid
  end;//try

  if pdf_height_mm<pdf_width_mm
     then begin
            if alert(7,'    PDF  page  size',
                    'You have set a long side page dimension which is shorter than the short side dimension.'
                   +'||Are you sure this is what you intended? The PDF page outlines may not be what you expect.',
                    '','','','','no  -  cancel','yes  -  continue',0)=5 then EXIT;
          end;

  if export_form.pdf_side_run_button.Checked=True   // swap dimensions for landscape
     then begin
            temp:=pdf_height_mm;
            pdf_height_mm:=pdf_width_mm;
            pdf_width_mm:=temp;
          end;

  if pdf_size_inside_trim_margins_checkbox.Checked=True   // 205e increase document size to allow for trim margin default sizes (fixed for PDF)
     then begin
            pdf_width_mm:=pdf_width_mm+9.0;     // left margin 7mm,  right margin 2mm
            pdf_height_mm:=pdf_height_mm+10.5;  // top margin 6mm, bottom margin 4.5mm
          end;

  pdf_width_dots:=Round(pdf_width_mm*pdf_width_dpi/25.4);
  pdf_height_dots:=Round(pdf_height_mm*pdf_height_dpi/25.4);

  pdf_black_white:=export_black_radiobutton.Checked;
  pdf_grey_shade:=export_grey_radiobutton.Checked;



      // ready to go...

  if export_control_template_radiobutton.Checked=True
     then begin
            show_modal_message('Exporting Control Template');
            print_control_template(True);   // True=PDF
            EXIT;
          end;

  if export_group_only_radiobutton.Checked=True
     then begin
            show_modal_message('Exporting Group Only');
            if any_bgnd=0
               then begin
                      alert_no_bgnd;
                      EXIT;
                    end;

            if any_selected=0
               then begin
                      if alert_no_group=True    // alert him, and does he want all?
                         then EXIT;
                    end;

            print_group_only_flag:=True;
            print_entire_pad(True);        // True=PDF

            EXIT;
          end;

  if export_all_radiobutton.Checked=True
     then begin
            if any_bgnd<1
               then begin
                      alert_no_bgnd;
                      EXIT;            // no background templates
                    end;

            print_group_only_flag:=False;
            print_entire_pad(True);        // True=PDF
          end;

end;
//______________________________________________________________________________

function sb_check_valid_float(edit_box: TEdit; limit_low, limit_high: double;
  var float_val: double): boolean;

  // check valid float dimension entered in sketchboard TEdit boxes...

var
  str: string;

begin
  Result := False;  // init

  if edit_box.Text = ''   // allow empty box
  then begin
    if limit_low < 0 then
      edit_box.Text := '0'
    else
      edit_box.Text := round_str(limit_low, 1);
  end;

  str := edit_box.Text;
  str := StringReplace(str, '[', ' ', [rfReplaceAll, rfIgnoreCase]); // negative brackets to spaces
  str := StringReplace(str, ']', ' ', [rfReplaceAll, rfIgnoreCase]); // negative brackets to spaces
  str := Trim(str);

  try
    float_val := StrToFloat(str);  // return as var
  except
    edit_box.Color := $00CCAAFF;   // pale red
    if edit_box.Visible = True then
      edit_box.SetFocus;
    EXIT;
  end;//try

  if (float_val < limit_low) or (float_val > limit_high) then begin
    edit_box.Color := $0055FFFF;   // pale yellow caution
    if edit_box.Visible = True then
      edit_box.SetFocus;
    EXIT;
  end;

  edit_box.Color := clWhite;
  Result := True;
end;
//______________________________________________________________________________

function do_metafile(file_str: string; met_width_dots, met_height_dots: integer): boolean;
  // 291a

  // if file_str='' create  \internal\temp_emf.emf

var
  met_rect, output_rect: TRect;
  met_DC_handle, load_DC: HDC;
  met_canvas: TCanvas;

  met_file_str: string;

  horz_factor, vert_factor: double;

  ref_DC: HDC;

  met_width_units, met_height_units: integer;

  failed: boolean;

begin
  Result := False;  // init
  failed := False;
  met_file_str := '';

  try
    ref_DC := GetDC(0);  // screen
  except
    failed := True;
  end;

  horz_factor := GetDeviceCaps(ref_DC, HORZSIZE) * 100 / GetDeviceCaps(ref_DC, HORZRES);
  vert_factor := GetDeviceCaps(ref_DC, VERTSIZE) * 100 / GetDeviceCaps(ref_DC, VERTRES);

  met_width_units := Round(met_width_dots * horz_factor);
  met_height_units := Round(met_height_dots * vert_factor);

  met_rect := Rect(0, 0, met_width_units, met_height_units);

  if file_str <> '' then
    met_file_str := file_str
  else
    met_file_str := exe_str + 'internal\temp_emf.emf';

  try
    met_dc_handle := CreateEnhMetaFile(ref_DC, PChar(met_file_str), @met_rect, nil);
  except
    failed := True;
  end;//try

  met_canvas := TCanvas.Create;

  met_canvas.Handle := met_dc_handle;

  export_draw(met_canvas, met_width_dots, met_height_dots, 4);
  // draw control template or entire pad    4= for exported metafile.

  try
    CloseEnhMetaFile(met_dc_handle);
  except
    failed := True;
  end;//try

  met_canvas.Free;

  ReleaseDC(0, ref_DC);

  Result := not failed;
end;
//______________________________________________________________________________

procedure Texport_form.create_metafile_buttonClick(Sender: TObject);       // modified for 291a

var
  emf_extents: Tpex; // mm

  shape_extents: Textents;

  folder_str: string;
  file_name_str: string;  // name part
  file_str: string;       // including full path

  save_print_pages_top_origin: double;
  save_print_pages_left_origin: double;
  save_out_factor: double;

  i: integer;

  img_width_mm: double;
  img_height_mm: double;

  box_value: double;

  emf_warning_str, emf_alert_str: string;

begin

  if fit_shapes_radiobutton.Checked = True      // extent of background shapes
  then begin
    if bgnd_form.bgnd_shapes_listbox.Items.Count < 1 then begin
      alert(6, 'php/215    fit  metafile  boundary  to  background  shapes',
        'Fit metafile boundary to background shapes.'
        +
        '||No background shapes are currently defined.',
        '', '', '', '', 'cancel', '', 0);
      EXIT;
    end;
  end;


  if (use_current_rectangle_radiobutton.Checked = True) and (draw_export_rectangle_flag = False)
  then begin
    alert(6, 'php/210    create  metafile  from  boundary  rectangle',
      'Create  metafile  from  boundary  rectangle.' +
      '||There is not currently an image rectangle on the trackpad to mark the metafile boundary.'
      + '||Click the DRAW NEW RECTANGLE button to create one.'
      + '||Or alternatively select the FIT ALL TEMPLATES option instead.| ',
      '', '', '', '', 'cancel', '', 0);
    EXIT;
  end;

  if (export_control_template_radiobutton.Checked = True) and (output_diagram_mode = True) then
  begin
    repeat
      i := alert(2, 'php/220    export  control  template',
        '||The output is currently set for diagram mode.'
        + '||The control template can not be output in diagram mode.'
        + '||Diagram mode is intended for background templates only, to display a track plan.| ',
        '', '', '', '? output  mode  -  help', 'cancel',
        'change  to  detail  mode  and  continue', 4);

      if i = 4 then
        alert_help(-300, output_mode_help_str, '');

    until i <> 4;

    if i = 5 then
      EXIT;

    pad_form.output_detail_mode_menu_entry.Click;
  end;

  if export_control_template_radiobutton.Checked = False then begin
    if any_bgnd < 1 then begin
      alert_no_bgnd;
      EXIT;            // no background templates
    end;
  end;

  if export_group_only_radiobutton.Checked = True then begin
    if any_selected = 0 then begin
      if alert_no_group = True    // alert him, and does he want all?
      then
        EXIT;
    end;
  end;

  if sb_check_valid_float(metafile_dpi_edit, 50, 1200, box_value) = False
  // input limits 50dpi to 1200dpi
  then begin
    ShowMessage('Error: The DPI setting must be a valid number in the range 50 to 1200.');
    EXIT;
  end;

  try
    metafile_dpi := box_value;
  except
    ShowMessage('Invalid data for metafile DPI');
    // ??? should have been found in sb_check_valid_ earlier
    EXIT;
  end;//try

  // 291a ...

  file_str := '';   // init

  file_name_str := remove_invalid_str(Copy(Trim(box_project_title_str), 1, 20) +
    FormatDateTime('_yyyy_mm_dd_hhmm_ss', Date + Time));

  with save_emf_dialog do begin

    if his_emf_file_name <> '' then
      InitialDir := ExtractFilePath(his_emf_file_name)
    else
      InitialDir := exe_str + 'EMF-FILES\';

    FileName := file_name_str + '.emf';
    DefaultExt := 'emf';
    Filter := 'EMF metafile ( .emf)|*.emf';
    Title := '    create  EMF  metafile ...';

    if Execute = False then
      EXIT;

    FileName := ChangeFileExt(FileName, '.emf');   // force extension

    his_emf_file_name := FileName;        // so we can use the same folder next time.

    // invalid entered chars removed by dialog

    file_str := ExtractFilePath(FileName) + lower_case_filename(ExtractFileName(FileName));
    // to underscores and lower case

  end;//with

  Screen.Cursor := crHourGlass;  // needed for large images or slow systems

  export_black_white := export_black_radiobutton.Checked;
  export_grey_shade := export_grey_radiobutton.Checked;

  print_entire_pad_flag := not export_control_template_radiobutton.Checked;
  print_group_only_flag := export_group_only_radiobutton.Checked;

  save_print_pages_top_origin := print_pages_top_origin;
  save_print_pages_left_origin := print_pages_left_origin;
  save_out_factor := out_factor;

  out_factor := 1.0;   // always 100% for image files

  // get boundary rectangle...

  if fit_all_radiobutton.Checked = True then begin
    emf_extents := get_fit_all_templates_size_mm(
      export_control_template_radiobutton.Checked, print_entire_pad_flag, print_group_only_flag);

    print_pages_top_origin := 0 - 10 * scale;    // mm 10ft scale margin
    print_pages_left_origin := 0 - 10 * scale;

    img_width_mm := emf_extents.x + 20 * scale;     // 2 10ft margins
    img_height_mm := emf_extents.y + 20 * scale;
  end
  else begin
    if fit_shapes_radiobutton.Checked = True      // extent of background shapes
    then begin
      shape_extents := get_fit_all_shapes_size_mm(False, True);
      // True = ignore any shapes hidden on output

      print_pages_top_origin := shape_extents.min.x - 10 * scale;
      // 10ft scale margin
      print_pages_left_origin := shape_extents.min.y - 10 * scale;

      img_width_mm := shape_extents.max.x - shape_extents.min.x + 20 * scale;
      // 2 10ft margins
      img_height_mm := shape_extents.max.y - shape_extents.min.y + 20 * scale;
    end
    else begin     // drawn image boundary rectangle

      if output_rectangle_x1 < output_rectangle_x2 then
        print_pages_top_origin := output_rectangle_x1       // mm
      else
        print_pages_top_origin := output_rectangle_x2;

      if output_rectangle_y1 < output_rectangle_y2 then
        print_pages_left_origin := output_rectangle_y1       // mm
      else
        print_pages_left_origin := output_rectangle_y2;

      img_width_mm := ABS(output_rectangle_x2 - output_rectangle_x1);
      img_height_mm := ABS(output_rectangle_y2 - output_rectangle_y1);
    end;
  end;

  if img_width_mm < 10 then
    img_width_mm := 10;   // 10mm min, no negs and prevent division by zero.
  if img_height_mm < 5 then
    img_height_mm := 5;   // 5mm min

  metafile_width := Round(img_width_mm * metafile_dpi / 25.4);     // dots
  metafile_height := Round(img_height_mm * metafile_dpi / 25.4);

  try
    if do_metafile(file_str, metafile_width, metafile_height) = False then begin
      ShowMessage('Sorry, an error occurred in creating the EMF file.');
      EXIT;
    end;

    Screen.Cursor := crDefault;

    // 291a ...

    emf_warning_str := '';    // init
    emf_alert_str := '';

    if (metafile_width > 16000) or (metafile_height > 9000)     // arbitrary 16x9
    then begin
      emf_warning_str :=
        '||rp.gif <span style="color:red;">WARNING:</span> This EMF metafile covers a very large area.'
        + ' Viewing it in your usual graphics viewer may cause your system to stop responding.'
        + '||It can be safely viewed in Templot, or on the Templot sketchboard.'
        +
        '||If it is necessary to view it in a graphics viewer, try creating it again using a lower DPI setting.';
      emf_alert_str := '  ! ! !  ';
    end;

    if FileExists(file_str) = True then begin
      repeat

        i := alert(2, '   EMF  metafile  created',
          'The EMF metafile was created successfully:||`0' +
          file_str + '`f' +
          '||Click <A HREF="alert_2.85a">view EMF in Templot</A> to see it.'
          + emf_warning_str, 'export  again',
          'view  EMF  in  Templot', emf_alert_str + 'view  EMF  in  your  usual  graphics  viewer' +
          emf_alert_str, 'open  the  containing  folder', '', 'all  done', 0);

        if i = 1 then begin
          if export_form.Showing = False then
            pad_form.export_file_menu_entry.Click;
        end
        else
          hide_button.Click;

        if i = 2 then begin
          show_an_image_file(file_str, metafile_width, metafile_height, True);
        end;

        if i = 3 then begin
          show_modal_message(
            'The EMF image may be slow to load. You may need to scroll the viewer to see anything.'
            + #13 + #13 +
            'If the viewer reports an error, try recreating the EMF using a lower DPI setting.');

          folder_str := file_str;

          if ShellExecute(0, 'open', PChar(folder_str), nil, nil, SW_SHOWNORMAL) <=
            32 then
            ShowMessage('Sorry, unable to display the EMF image.')
          else
            external_window_showing := True;
        end;

        if i = 4 then begin
          folder_str := ExtractFilePath(file_str);

          if ShellExecute(0, 'explore', PChar(folder_str), nil, nil, SW_SHOWNORMAL) <=
            32 then
            ShowMessage('Sorry, unable to open the folder.')
          else
            external_window_showing := True;
        end;

      until (i = 1) or (i = 6) or (FileExists(file_str) = False);
      // may be deleted in image viewer

    end;

  finally                   // restore for user
    print_pages_top_origin := save_print_pages_top_origin;
    print_pages_left_origin := save_print_pages_left_origin;
    out_factor := save_out_factor;

    Screen.Cursor := crDefault;

  end;//try

end;
//______________________________________________________________________________

procedure Texport_form.draw_export_rectangle_buttonClick(Sender: TObject);

begin
  use_current_rectangle_radiobutton.Checked := True;
  Close;      // while drawing rectangle
  export_form_was_showing := True;
  pad_form.draw_boundary_rectangle_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Texport_form.rectangle_clear_buttonClick(Sender: TObject);

begin
  if use_current_rectangle_radiobutton.Checked = True then
    fit_all_radiobutton.Checked := True; // no rectangle to use.
  pad_form.clear_boundary_rectangle_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Texport_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_RETURN    // exit from edit boxes
  then begin
    dummy_button.SetFocus;
    Key := VK_SPACE;
  end;

  if Key = VK_PAUSE then
    Application.Minimize;    //  hide TEMPLOT on PAUSE key.
end;
//______________________________________________________________________________

procedure Texport_form.create_image_file_buttonClick(Sender: TObject);

var
  image_extents: Tpex; // mm

  shape_extents: Textents;  // mm

  create_bitmap: TBitmap;
  create_jpg: TJpegImage;
  create_gif: TGIFImage;

  { OT-FIRST create_png:TPNGObject;}

  create_png: TPortableNetworkGraphic;  //  OT-FIRST

  folder_str: string;
  file_name_str: string;  // name part
  file_str: string;       // including path

  save_print_pages_top_origin: double;
  save_print_pages_left_origin: double;
  save_out_factor: double;

  i: integer;

  img_width_mm: double;
  img_height_mm: double;

  box_value: integer;  // 205e ...

begin

  if fit_shapes_radiobutton.Checked = True      // extent of background shapes
  then begin
    if bgnd_form.bgnd_shapes_listbox.Items.Count < 1 then begin
      alert(6, '    fit  image  boundary  to  background  shapes',
        'Fit image boundary to background shapes.'
        +
        '||No background shapes are currently defined.',
        '', '', '', '', 'cancel', '', 0);
      EXIT;
    end;
  end;

  if (use_current_rectangle_radiobutton.Checked = True) and (draw_export_rectangle_flag = False)
  then begin
    alert(6, 'php/210    create  image  file  from  boundary  rectangle',
      'Create  image file  from  boundary  rectangle.'
      + '||There is not currently an image rectangle on the trackpad to mark the image file boundary.'
      + '||Click the DRAW NEW RECTANGLE button to create one.'
      + '||Or alternatively select the FIT ALL TEMPLATES option instead.',
      '', '', '', '', 'cancel', '', 0);
    EXIT;
  end;

  if (export_control_template_radiobutton.Checked = True) and (output_diagram_mode = True) then
  begin
    repeat
      i := alert(2, 'php/220    export  control  template',
        '||The output is currently set for diagram mode.'
        + '||The control template can not be output in diagram mode.'
        + '||Diagram mode is intended for background templates only, to display a track plan.| ',
        '', '', '', '? output  mode  -  help', 'cancel',
        'change  to  detail  mode  and  continue', 4);

      if i = 4 then
        alert_help(-300, output_mode_help_str, '');

    until i <> 4;

    if i = 5 then
      EXIT;

    pad_form.output_detail_mode_menu_entry.Click;
  end;

  if export_control_template_radiobutton.Checked = False then begin
    if any_bgnd < 1 then begin
      alert_no_bgnd;
      EXIT;            // no background templates
    end;
  end;

  if export_group_only_radiobutton.Checked = True then begin
    if any_selected = 0 then begin
      if alert_no_group = True    // alert him, and does he want all?
      then
        EXIT;
    end;
  end;

  { OT-FIRST

  if sb_check_valid_int(image_width_edit,60,12000,box_value)=False  // input limits 60 dots to 12000 dots (20" @ 600dpi)
     then begin
            ShowMessage('Error: The image width setting must be a valid whole number in the range 60 dots to 12000 dots.');
            EXIT;
          end;
  }
  // OT-FIRST ...

  try
    box_value := StrToInt(image_width_edit.Text);
  except
    ShowMessage('Error: The image width setting must be a valid whole number in the range 60 dots to 12000 dots.');
    EXIT;
  end;

  if (box_value < 60) or (box_value > 12000) then begin
    ShowMessage(
      'Error: The image width setting must be a valid whole number in the range 60 dots to 12000 dots.');
    EXIT;
  end;

  if (output_diagram_mode = False) and (confirm_detail_mode_1_msg_pref = False) then begin

    alert_box.preferences_checkbox.Checked := False;       //%%%%
    alert_box.preferences_checkbox.Show;

    i := alert(3, 'php/230   create  an  image  file ...',
      'Templot is currently set for detail-mode output. This mode is intended for track construction templates.'
      + '||For an image file you may prefer to change to diagram-mode output, which is more suitable for track plan illustrations, signal box diagrams, control panels, and similar purposes.' + '||You can change the setting, with some additional options, by clicking the OUTPUT > OUTPUT MODE OPTIONS > menu items.|| ', '', '', '', 'change  to  diagram  mode', 'cancel', 'continue  -  create  image  file  in  detail  mode ...', 0);

    confirm_detail_mode_1_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
    alert_box.preferences_checkbox.Hide;

    if i = 4 then
      pad_form.output_diagram_mode_menu_entry.Click;
    if i = 5 then
      EXIT;
  end;

  if (output_diagram_mode = False) and (confirm_detail_mode_2_msg_pref = False) then begin

    alert_box.preferences_checkbox.Checked := False;       //%%%%
    alert_box.preferences_checkbox.Show;

    i := alert(7, 'php/230   create  an  image  file  in  detail  mode ...',
      'Image files in detail mode can be used to illustrate your track plan but should not be used as track construction templates.' + ' They are difficult to print to an exact size and unlikely to be sufficiently accurate.' + '||To create track construction templates you can do any of these instead:' + '||1. print them directly from Templot0.' + '||2. create a PDF file, and print it using a PDF Reader program with page scaling set to "none" (important).' + '||3. export a DXF file and print it via a CAD program.| ', '', '', '', '', 'cancel', 'continue  -  create  image  file  in  detail  mode ...', 0);

    confirm_detail_mode_2_msg_pref := alert_box.preferences_checkbox.Checked;    //%%%%
    alert_box.preferences_checkbox.Hide;

    if i = 5 then
      EXIT;
  end;

  try
    create_image_width_dots := box_value;
  except
    ShowMessage('Invalid data for image width.');
    // ??? should have been found in sb_check_valid_ earlier
    EXIT;
  end;//try

  file_name_str := remove_invalid_str(Copy(Trim(box_project_title_str), 1, 20) +
    FormatDateTime('_yyyy_mm_dd_hhmm_ss', Date + Time));

  with save_imagefile_dialog do begin

    Filter := 'PNG image ( .png)|*.png|JPEG image ( .jpg  .jp  .jpeg)|*.jpg;*.jp;*.jpeg|BMP image (  .bmp)|*.bmp';

    if his_image_file_name <> '' then
      InitialDir := ExtractFilePath(his_image_file_name)
    else
      InitialDir := exe_str + 'IMAGE-FILES\';

    { T3-FIRST
    if transparent_gif_checkbox.Checked=True
       then begin
              DefaultExt:='gif';
              FileName:=file_name_str+'.gif';
              FilterIndex:=2;
            end
       else begin}

    DefaultExt := 'png';
    FileName := file_name_str + '.png';
    FilterIndex := 1;

    //end;

    if Execute = False then
      EXIT;

    his_image_file_name := FileName;        // so we can use the same folder next time.

    // invalid entered chars removed by dialog

    file_str := ExtractFilePath(FileName) + lower_case_filename(ExtractFileName(FileName));
    // to underscores and lower case

  end;//with

  Screen.Cursor := crHourGlass;  // needed for large images or slow systems

  export_black_white := export_black_radiobutton.Checked;
  export_grey_shade := export_grey_radiobutton.Checked;

  print_entire_pad_flag := not export_control_template_radiobutton.Checked;
  print_group_only_flag := export_group_only_radiobutton.Checked;

  if (classic_templot = False) and (print_entire_pad_flag = True) then
    store_and_background(False, True);  // 0.93.a Quick mode - first store existing control template

  save_print_pages_top_origin := print_pages_top_origin;
  save_print_pages_left_origin := print_pages_left_origin;
  save_out_factor := out_factor;

  out_factor := 1.0;   // always 100% for image files

  // output rectangle ...

  if fit_all_radiobutton.Checked = True then begin
    image_extents := get_fit_all_templates_size_mm(
      export_control_template_radiobutton.Checked, print_entire_pad_flag, print_group_only_flag);

    print_pages_top_origin := 0 - 10 * scale;    // mm 10ft scale margin
    print_pages_left_origin := 0 - 10 * scale;

    img_width_mm := image_extents.x + 20 * scale;     // 2 10ft margins
    img_height_mm := image_extents.y + 20 * scale;
  end
  else begin
    if fit_shapes_radiobutton.Checked = True      // extent of background shapes
    then begin
      shape_extents := get_fit_all_shapes_size_mm(False, True);
      // True = ignore any shapes hidden on output

      print_pages_top_origin := shape_extents.min.x - 10 * scale;
      // 10ft scale margin
      print_pages_left_origin := shape_extents.min.y - 10 * scale;

      img_width_mm := shape_extents.max.x - shape_extents.min.x + 20 * scale;
      // 2 10ft margins
      img_height_mm := shape_extents.max.y - shape_extents.min.y + 20 * scale;
    end
    else begin     // drawn image boundary rectangle

      if output_rectangle_x1 < output_rectangle_x2 then
        print_pages_top_origin := output_rectangle_x1       // mm
      else
        print_pages_top_origin := output_rectangle_x2;

      if output_rectangle_y1 < output_rectangle_y2 then
        print_pages_left_origin := output_rectangle_y1       // mm
      else
        print_pages_left_origin := output_rectangle_y2;

      img_width_mm := ABS(output_rectangle_x2 - output_rectangle_x1);
      img_height_mm := ABS(output_rectangle_y2 - output_rectangle_y1);
    end;
  end;

  if img_width_mm < 10 then
    img_width_mm := 10;   // 10mm min, no negs and prevent division by zero.
  if img_height_mm < 5 then
    img_height_mm := 5;   // 5mm min

  create_image_dpi := create_image_width_dots * 25.4 / img_width_mm;

  create_image_height_dots := Round(create_image_width_dots * img_height_mm / img_width_mm);

  create_bitmap := TBitmap.Create;
  create_jpg := TJpegImage.Create;
  create_gif := TGIFImage.Create;

  { OT-FIRST create_png:=TPNGObject.Create;}
  create_png := TPortableNetworkGraphic.Create;   // OT-FIRST

  try

    create_bitmap.Width := create_image_width_dots;
    create_bitmap.Height := create_image_height_dots;

    try
      // draw templates on a bitmap, output_code 1=sketchbook bitmap, 2=sketchbook metafile, 3=create image file bitmap, 4=create EMF metafile

      export_draw(create_bitmap.Canvas, create_image_width_dots, create_image_height_dots, 3);

      if (LowerCase(ExtractFileExt(file_str)) = '.jpg') or
        (LowerCase(ExtractFileExt(file_str)) = '.jpeg') then begin
        create_jpg.Assign(create_bitmap);

        create_jpg.CompressionQuality := jpg_quality;   // global on control_room

        create_jpg.SaveToFile(file_str);
      end;

      { T3-FIRST
      if LowerCase(ExtractFileExt(file_str))='.gif'
         then begin
                create_bitmap.TransparentColor:=clWhite;
                create_bitmap.Transparent:=transparent_gif_checkbox.Checked;

                create_gif.Assign(create_bitmap);
                create_gif.SaveToFile(file_str);
              end;
      }

      if LowerCase(ExtractFileExt(file_str)) = '.png' then begin
        create_png.Assign(create_bitmap);
        create_png.SaveToFile(file_str);
      end;

      if LowerCase(ExtractFileExt(file_str)) = '.bmp' then
        create_bitmap.SaveToFile(file_str);


      repeat

        i := alert(2, '   image  file  created',
          'The image file was created successfully:||`0' + file_str +
          '`f' + '||Click <A HREF="alert_2.85a">view image in Templot</A> to see it.',
          'export  again', 'view  image  in  Templot',
          'view  image  in  your  usual  image  viewer', 'open  the  containing  folder', '', 'all  done', 0);

        if i = 1 then begin
          if export_form.Showing = False then
            pad_form.export_file_menu_entry.Click;
        end
        else
          hide_button.Click;

        if i = 2 then begin
          show_an_image_file(file_str, 0, 0, True);
        end;

        if i = 3 then begin
          folder_str := file_str;

          if ShellExecute(0, 'open', PChar(folder_str), nil, nil, SW_SHOWNORMAL) <=
            32 then
            ShowMessage('Sorry, unable to display the image.')
          else
            external_window_showing := True;
        end;

        if i = 4 then begin
          folder_str := ExtractFilePath(file_str);

          if ShellExecute(0, 'explore', PChar(folder_str), nil, nil, SW_SHOWNORMAL) <= 32
          then
            ShowMessage('Sorry, unable to open the folder.')
          else
            external_window_showing := True;
        end;

      until (i = 1) or (i = 6) or (FileExists(file_str) = False);    // may be deleted in image viewer

    except
      ShowMessage('Sorry, an error occurred in creating the image file.');
    end;//try

  finally
    create_gif.Free;
    create_png.Free;
    create_jpg.Free;
    create_bitmap.Free;
  end;//try

  // restore for user

  print_pages_top_origin := save_print_pages_top_origin;
  print_pages_left_origin := save_print_pages_left_origin;
  out_factor := save_out_factor;

  Screen.Cursor := crDefault;
end;
//______________________________________________________________________________

procedure set_boundary_rectangle_dims(calling_form: TForm);

const
  x_str: string = '    `0X  dimensions`9' +
    '||Enter the X dimension to a corner of the desired image boundary rectangle.'
    + '||X dimensions are measured across the screen from the left, and read on the bottom grid scale.'
    + '||Dimensions measured forwards to the right from the origin (0 mark) are positive; dimensions measured backwards to the left from the origin are negative.' + '||Note that unless you use conversion factors the dimension should be entered in `0mm`3. To avoid confusion, set the grid spacing in mm (click the `0trackpad > trackpad grid options >`1 menu items).' + '||For more information about using conversion factors click the|`0? HELP`1 button.';

  y_str: string = '    `0Y dimensions`9' +
    '||Enter the Y dimension to a corner of the desired image boundary rectangle.'
    + '||Y dimensions are measured up the screen from the bottom, and read on the left grid scale.'
    + '||Dimensions measured upwards from the origin (0 mark) are positive; dimensions measured downwards from the origin are negative.' + '||Note that unless you use conversion factors the dimension should be entered in `0mm`3. To avoid confusion, set the grid spacing in mm (click the `0trackpad > trackpad grid options >`1 menu items).' + '||For more information about using conversion factors click the|`0? HELP`1 button.';

var
  i: integer;
  od: Toutdim;

begin
  putdim(x_str, 1, 'image  rectangle  first  corner  dimension  X1',
    output_rectangle_x1, False, True, False, False);
  // neg ok, no preset, allow zero, don't terminate on zero.
  putdim(y_str, 1, 'image  rectangle  first  corner  dimension  Y1',
    output_rectangle_y1, False, True, False, False);   // ditto.
  putdim(x_str, 1, 'image  rectangle  opposite  corner  dimension  X2',
    output_rectangle_x2, False, True, False, False);   // ditto.
  i := putdim(y_str, 1, 'image  rectangle  opposite  corner  dimension  Y2',
    output_rectangle_y2, False, True, False, False);   // ditto.
  if i <> 3 then
    EXIT;
  if getdims('image  boundary  rectangle', '', calling_form, i, od) = True then begin
    output_rectangle_x1 := od[0];
    output_rectangle_y1 := od[1];
    output_rectangle_x2 := od[2];
    output_rectangle_y2 := od[3];

    draw_export_rectangle_flag := True;  // available for calcs
    //non_print_output_rectangle:=True;  // for drawing function

    redraw(True);
  end;
end;
//______________________________________________________________________________

procedure Texport_form.rectangle_set_buttonClick(Sender: TObject);

begin
  set_boundary_rectangle_dims(export_form);
end;
//______________________________________________________________________________

procedure Texport_form.export_include_grid_checkboxClick(Sender: TObject);

begin
  export_include_grid_labels_checkbox.Enabled := export_include_grid_checkbox.Checked;
end;
//______________________________________________________________________________

procedure Texport_form.FormActivate(Sender: TObject);

begin
  if draw_export_rectangle_flag = True then
    use_current_rectangle_radiobutton.Checked := True;

  print_now_bang := False;
end;
//______________________________________________________________________________

procedure Texport_form.png_help_buttonClick(Sender: TObject);

const
  img_help_str: string = '    `0Exporting  Image  Files`9' +
    '||Please export image files in PNG format rather than JPG format.' +
    '||green_panel_begin tree.gif PNG format will create a smaller file size with 100% image quality.'
    +
    '||JPG format is unsuitable for computer-generated graphics such as Templot0 templates. The JPG format is intended only for photographic images from a camera or a scanner.' + '||If you prefer to use JPG format, you can change the setting for file size and image quality by clicking the `0set JPG image quality`z button below.green_panel_end';

begin
  if help(0, img_help_str, 'set  JPG  image  quality') = 1 then
    control_room_form.jpg_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Texport_form.export_include_picture_shapes_checkboxClick(Sender: TObject);

begin
  export_include_picture_borders_checkbox.Enabled := export_include_picture_shapes_checkbox.Checked;
end;
//______________________________________________________________________________

procedure Texport_form.img_bgnd_buttonClick(Sender: TObject);

begin
  img_bgnd_colour_panel.Color := get_colour('choose  a  background  colour  for  the  image',
    img_bgnd_colour_panel.Color);
end;
//______________________________________________________________________________

// T3-FIRST   draw functions are now in export_draw_unit  11/11/2019

end.
