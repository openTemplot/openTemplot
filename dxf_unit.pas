
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

unit dxf_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Tdxf_form = class(TForm)
    rails_combo: TComboBox;
    Label1: TLabel;
    adjacent_lines_label: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    adjrails_combo: TComboBox;
    tkclines_combo: TComboBox;
    timbout_combo: TComboBox;
    sleeperend_combo: TComboBox;
    timbcent_combo: TComboBox;
    gmarks_combo: TComboBox;
    radmarks_combo: TComboBox;
    joints_combo: TComboBox;
    solid_bgnd_combo: TComboBox;
    radcentres_combo: TComboBox;
    text_combo: TComboBox;
    rails_style_combo: TComboBox;
    adjrails_style_combo: TComboBox;
    tkclines_style_combo: TComboBox;
    timbout_style_combo: TComboBox;
    timbcent_style_combo: TComboBox;
    gmarks_style_combo: TComboBox;
    radmarks_style_combo: TComboBox;
    joints_style_combo: TComboBox;
    solid_bgnd_style_combo: TComboBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    ok_panel: TPanel;
    ok_button: TButton;
    datestamp_label: TLabel;
    blue_corner_panel: TPanel;
    chat_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    save_dialog: TSaveDialog;
    dxf_memo: TMemo;
    options_panel: TPanel;
    all_option_button: TRadioButton;
    group_option_button: TRadioButton;
    cancel_panel: TPanel;
    cancel_button: TButton;
    sleeperend_style_combo: TComboBox;
    radcentres_style_combo: TComboBox;
    top_panel: TPanel;
    bottom_panel: TPanel;
    units_panel: TPanel;
    mm_radio_button: TRadioButton;
    inches_radio_button: TRadioButton;
    limits_checkbox: TCheckBox;
    Label16: TLabel;
    dot_bgnd_combo: TComboBox;
    dot_bgnd_style_combo: TComboBox;
    scaled_checkbox: TCheckBox;
    Label17: TLabel;
    tbnum_combo: TComboBox;
    _3d_panel: TPanel;
    _2d_radiobutton: TRadioButton;
    _3d_wireframe_radiobutton: TRadioButton;
    _3d_solid_radiobutton: TRadioButton;
    _3d_colours_button: TButton;
    _3d_timbers_groupbox: TGroupBox;
    timb_full_radiobutton: TRadioButton;
    timb_flat_radiobutton: TRadioButton;
    help_button: TButton;
    Shape1: TShape;
    Label2: TLabel;
    procedure ok_panelClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure chat_panelClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure cancel_panelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rails_comboChange(Sender: TObject);
    procedure rails_comboDropDown(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure scaled_checkboxClick(Sender: TObject);
    procedure _2d_radiobuttonClick(Sender: TObject);
    procedure _3d_wireframe_radiobuttonClick(Sender: TObject);
    procedure _3d_solid_radiobuttonClick(Sender: TObject);
    procedure _3d_colours_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dxf_form: Tdxf_form;

//--------------------------

procedure export_templates_dxf;

//__________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses
  control_room, pad_unit, help_sheet, chat_unit, colour_unit, math_unit, alert_unit,
  bgkeeps_unit, keep_select, bgnd_unit, preview_unit, print_unit, print_settings_unit;

var
  layer_str: array[0..17] of string;
  line_type_str: array[0..5] of string;

  user_save_file_name: string = '';

  xmax: extended = 1000;     // default drawing limits.
  ymax: extended = 1000;
  xmin: extended = -25;
  ymin: extended = -25;

  _3d: boolean = False;         // 2-D init.
  wire_frame: boolean = False;
  // DXF colour codes ...

  railsides_3d_colour: integer = 33;   // dark rust  (166,122,82)
  railtop_3d_colour: integer = 252;    // steel grey (150,150,150)
  chair_3d_colour: integer = 54;       // olive      (128,128,0)
  timber_3d_colour: integer = 8;       // grey       (128,128,128)

  rail_foot_z: extended = 0 - 2.75;
  // mm default rail base 6" at 5.5 mm scale bleow zero datum (rail top).
  timb_top_z: extended = 0 - 3.5;     // mm default timbertop 7.6" at 5.5 mm scale below rail top.
  timb_bot_z: extended = 0 - 6.0;     // mm default timber bottom 13.1" at 5.5 mm scale below rail top.

procedure dxf_background_keeps(var dxf_file: TextFile); forward;  // do the background keeps.
procedure dxf_shapes(var dxf_file: TextFile); forward;            // do any background shapes.

//______________________________________________________________________________________

function make_code(n: integer): string;    // generate 3-character group code with leading spaces.

begin
  Result := space_lead(FormatFloat('000', n)) + '|';
end;
//________________________________________________________________________________________

function make_dim(d: extended): string;    // generate floating point string.
  // input d in mm.
var
  pz: extended;
  out_str: string; // 0.94.a

begin
  if dxf_form.scaled_checkbox.Checked = True then
    pz := out_factor                        // scale in accordance with current print size.
  else
    pz := 1.0;

  if dxf_form.inches_radio_button.Checked = True then
    out_str := FormatFloat('0.0000', d * pz / 25.4) + '|'
  else
    out_str := FormatFloat('0.00', d * pz) + '|';

  // 0.94.a bug fix for non English speaking contries. DXF format uses dot for decimal point regardless of regional settings.

  Result := StringReplace(out_str, ',', '.', []);
  // 0.94.a  StringReplace(str,'old','new',[rfReplaceAll, rfIgnoreCase]);
end;
//___________________________________________________________________________________________

function dxf_3dface(x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4: extended; layer: integer): string;
  // make up a four-cornered 3-D face.

var
  s: string;

begin
  Result := '';      // default init.

  if dxf_form.limits_checkbox.Checked = True     // only within x,y limits?
  then begin
    if (x1 > xmax) or (x2 > xmax) or (x3 > xmax) or (x4 > xmax) then
      EXIT;
    if (y1 > ymax) or (y2 > ymax) or (y3 > ymax) or (y4 > ymax) then
      EXIT;

    if (x1 < xmin) or (x2 < xmin) or (x3 < xmin) or (x4 < xmin) then
      EXIT;
    if (y1 < ymin) or (y2 < ymin) or (y3 < ymin) or (y4 < ymin) then
      EXIT;
  end;

  {
100
 Subclass marker (AcDbFace)

10
 First corner (in WCS)
DXF: X value; APP: 3D point

20, 30
 DXF: Y and Z values of first corner (in WCS)

11
 Second corner (in WCS)
DXF: X value; APP: 3D point

21, 31
 DXF: Y and Z values of second corner (in WCS)

12
 Third corner (in WCS)
DXF: X value; APP: 3D point

22, 32
 DXF: Y and Z values of third corner (in WCS)

13
 Fourth corner (in WCS). If only three corners are entered, this is the same as the third corner
DXF: X value; APP: 3D point

23, 33
 DXF: Y and Z values of fourth corner (in WCS)

70
 Invisible edge flags (optional; default = 0):
1 = First edge is invisible
2 = Second edge is invisible
4 = Third edge is invisible
8 = Fourth edge is invisible

}

  s := '  0|3DFACE|  8|' + layer_str[layer] + ' 10|' + make_dim(x1) + ' 20|' +
    make_dim(y1) + ' 30|' + make_dim(z1) + ' 11|' + make_dim(x2) + ' 21|' + make_dim(
    y2) + ' 31|' + make_dim(z2) + ' 12|' + make_dim(x3) + ' 22|' + make_dim(y3) +
    ' 32|' + make_dim(z3) + ' 13|' + make_dim(x4) + ' 23|' + make_dim(y4) + ' 33|' + make_dim(z4);

  //+' 70| 15|';           // edges invisible (lines drawn separately)

  Result := insert_crlf_str(s);
end;
//________________________________________________________________________________________

function dxf_line(x1, y1, z1, x2, y2, z2: extended; layer: integer): string;    // make up a line group

var
  s: string;

begin
  Result := '';      // default init.

  if dxf_form.limits_checkbox.Checked = True     // only within limits?
  then begin
    if (x1 > xmax) or (x2 > xmax) or (y1 > ymax) or (y2 > ymax) then
      EXIT;
    if (x1 < xmin) or (x2 < xmin) or (y1 < ymin) or (y2 < ymin) then
      EXIT;
  end;

  s := '  0|LINE|  8|' + layer_str[layer] + ' 10|' + make_dim(x1) + ' 20|' + make_dim(y1);

  if _3d = True then
    s := s + ' 30|' + make_dim(z1);

  s := s + ' 11|' + make_dim(x2) + ' 21|' + make_dim(y2);

  if _3d = True then
    s := s + ' 31|' + make_dim(z2);

  Result := insert_crlf_str(s);
end;
//________________________________________________________________________________________

function line_type(layer: integer): string;     // get his requested line type.

begin
  with dxf_form do begin
    case layer of
      0:
        Result := line_type_str[rails_style_combo.ItemIndex];
      1:
        Result := line_type_str[adjrails_style_combo.ItemIndex];
      2:
        Result := line_type_str[tkclines_style_combo.ItemIndex];
      3:
        Result := line_type_str[timbout_style_combo.ItemIndex];
      4:
        Result := line_type_str[sleeperend_style_combo.ItemIndex];
      5:
        Result := line_type_str[timbcent_style_combo.ItemIndex];
      6:
        Result := line_type_str[gmarks_style_combo.ItemIndex];
      7:
        Result := line_type_str[radmarks_style_combo.ItemIndex];
      8:
        Result := line_type_str[radcentres_style_combo.ItemIndex];
      9:
        Result := line_type_str[joints_style_combo.ItemIndex];
      10:
        Result := line_type_str[solid_bgnd_style_combo.ItemIndex];
      11:
        Result := line_type_str[dot_bgnd_style_combo.ItemIndex];
      12:
        Result := 'CONTINUOUS|';                                     // text layer.
      13:
        Result := 'CONTINUOUS|';                                     // timmber numbering text.
      else
        Result := 'CONTINUOUS|';      // 3-D layers.
    end;//case
  end;//with
end;
//_________________________________________________________________________________

function colour(layer: integer): string;    // get his requested colour.

begin
  with dxf_form do begin
    case layer of
      0:
        Result := IntToStr(rails_combo.ItemIndex) + '|';
      1:
        Result := IntToStr(adjrails_combo.ItemIndex) + '|';
      2:
        Result := IntToStr(tkclines_combo.ItemIndex) + '|';
      3:
        Result := IntToStr(timbout_combo.ItemIndex) + '|';
      4:
        Result := IntToStr(sleeperend_combo.ItemIndex) + '|';
      5:
        Result := IntToStr(timbcent_combo.ItemIndex) + '|';
      6:
        Result := IntToStr(gmarks_combo.ItemIndex) + '|';
      7:
        Result := IntToStr(radmarks_combo.ItemIndex) + '|';
      8:
        Result := IntToStr(radcentres_combo.ItemIndex) + '|';
      9:
        Result := IntToStr(joints_combo.ItemIndex) + '|';
      10:
        Result := IntToStr(solid_bgnd_combo.ItemIndex) + '|';
      11:
        Result := IntToStr(dot_bgnd_combo.ItemIndex) + '|';
      12:
        Result := IntToStr(text_combo.ItemIndex) + '|';
      13:
        Result := IntToStr(tbnum_combo.ItemIndex) + '|';
      14:
        Result := IntToStr(railsides_3d_colour) + '|';
      15:
        Result := IntToStr(railtop_3d_colour) + '|';
      16:
        Result := IntToStr(chair_3d_colour) + '|';
      17:
        Result := IntToStr(timber_3d_colour) + '|';
      else
        Result := '7|';        // black.
    end;//case
  end;//with
end;
//__________________________________________________________________________________

procedure export_templates_dxf;

var
  dxf_file: TextFile;
  s: string;
  colour_str: string;
  f_str: string;
  layer: integer;

begin
  if any_bgnd < 1 then begin
    alert_no_bgnd;
    EXIT;            // no background templates
  end;

  with dxf_form do begin

    do_show_modal(dxf_form);     // 212a  ShowModal

    if ModalResult <> mrOk then
      EXIT;

    if (group_option_button.Checked = True) and (any_selected = 0) then begin
      if alert(4, '    no  group  templates',
        '|||You have clicked the GROUP TEMPLATES ONLY option button,'
        + ' but there are no group templates currently selected on the trackpad.'
        + '||Do you want to include all background templates in the DXF file?',
        '', '', '', '', 'no  -  cancel  DXF', 'yes  -  all  templates  in  file', 0) = 5
      then
        EXIT;
      all_option_button.Checked := True;           // radio item.
    end;

    with save_dialog do begin                          // set up the save dialog.
      if inches_radio_button.Checked = True then begin
        Title := '    export  DXF  file  ( in  inches )  as ...';
        f_str := ' in';
      end
      else begin
        Title := '    export  DXF  file  ( in  mm )  as ...';
        f_str := ' mm';
      end;

      if user_save_file_name <> '' then
        InitialDir := ExtractFilePath(user_save_file_name)   // use his previous folder.
      else
        InitialDir := exe_str + 'DXF-FILES\';                  // or the default one.

      Filter := 'DXF files (*.dxf)|*.dxf';
      Filename := remove_invalid_str(Copy(Trim(box_project_title_str), 1, 20) +
        FormatDateTime(' yy_mm_dd hhmm ss', Date + Time)) + f_str + '.dxf';

      Filename := lower_case_filename(Filename);   // 0.79.a   to underscores and lower case.

    end;//with

    showing_dialog := True;   // 212a Wine bug

    if save_dialog.Execute = False then begin
      showing_dialog := False;   // 212a Wine bug
      EXIT;
    end;

    showing_dialog := False;   // 212a Wine bug

    if invalid_85a_file_name(save_dialog.FileName) = True then
      EXIT;

    save_dialog.FileName := ChangeFileExt(save_dialog.FileName, '.dxf');   // force extension

    user_save_file_name := save_dialog.FileName;

    if classic_templot = False then
      store_and_background(False, True);
    // 0.93.a Quick mode - first store and zero existing control template

    try
      Screen.Cursor := crHourGlass;
      // could take a while if big file.
      if Application.Terminated = False then
        Application.ProcessMessages;  // so let the form repaint.

      AssignFile(dxf_file, save_dialog.FileName);      // set the file name.
      Rewrite(dxf_file);                              // open a new file.
      // make up the header...

      s := '  0|SECTION|  2|HEADER|'; //  9|$ACADVER|  1|+'Templot V.0|';

      xmin := zoom_offsetx;
      ymin := zoom_offsety;

      xmax := xmin + screenx;
      ymax := ymin + screeny;

      if limits_checkbox.Checked = True          // add the current pad limits in mm.
      then
        s := s + '  9|$LIMMIN|' + ' 10|' + make_dim(xmin) +
          ' 20|' + make_dim(ymin) + '  9|$LIMMAX|' + ' 10|' +
          make_dim(xmax) + ' 20|' + make_dim(ymax);

      s := s + '  0|ENDSEC|';

      Write(dxf_file, insert_crlf_str(s));  // write header to file.
      Write(dxf_file, dxf_memo.Text);       // write the line type tables.

      s := '  0|TABLE|  2|LAYER| 70|18|';    // 18 layers.
      Write(dxf_file, insert_crlf_str(s));  // write layer table header.

      for layer := 0 to 17 do begin          // 18 layers.
        // write layer entries from his current settings.

        colour_str := colour(layer);
        if colour_str = '0|' then
          CONTINUE;  // he wants this layer omitted.

        s := '  0|LAYER|  2|' + layer_str[layer] + ' 70|0|  6|' + line_type(layer) + ' 62|' + colour_str;
        Write(dxf_file, insert_crlf_str(s));
      end;//for

      s := '  0|ENDTAB|  0|ENDSEC|  0|SECTION|  2|ENTITIES|';
      Write(dxf_file, insert_crlf_str(s));    // end tables and do entities header.

      dxf_shapes(dxf_file);              // go do all the shapes.

      dxf_background_keeps(dxf_file);    // then go do all the background keeps.

      Write(dxf_file, insert_crlf_str('  0|ENDSEC|  0|EOF|'));

      CloseFile(dxf_file);         // and close the file.
    except
      Screen.Cursor := crDefault;
      alert(5, '      file  error',
        '||Unable to create DXF file.' +
        '||Please check the file and folder names and that this file is not in use by another application.'
        + '||If saving to a floppy disk, please check that it is not write-protected.'
        + ' DXF files are much larger than Templot0 box data files. A large drawing may have exceeded the capacity of the floppy disk.',
        '', '', '', '', 'cancel  DXF', '', 0);
    end;//try
  end;//with
  Screen.Cursor := crDefault;
end;
//_________________________________________________________________________________________

procedure Tdxf_form.ok_panelClick(Sender: TObject);

begin
  ModalResult := mrOk;
end;
//_______________________________________________________________________________________

procedure chat_click;

const
  chat_str: string = 'This DXF generator is fairly minimal - the object is simply to get the drawing into your CAD software.' + ' What you do with it after that is up to you. The file contains no header information to change your CAD setup.' + '||( Handy hint - make a note of your DXF file name and date in the memo text for the first template in the box. You will then have a reference' + ' to it when you reload this box file at some future date.)' + '||Templot0''s drawing space is  +/- 1.0E7 mm ( that''s 20 kilometres or 12 miles square ! ) and drawing is done' + ' to the nearest 0.01 mm within it. Your CAD program may need this information to load the file correctly.' + '||Although it is possible to import a DXF file as BACKGROUND SHAPES, there is currently no means of importing individual templates from a DXF file' + ' and never will be as far as the normal template generating functions are concerned - Templot0 cannot work backwards from a collection of drawn lines to recreate' + ' the original template specification which produced them.' + '||p.s. Never say "never" !';

begin
  chat(chat_str);
end;
//_____________________________________________________________________________________________

procedure Tdxf_form.help_buttonClick(Sender: TObject);

const
  help_str: string = '   Export  in  DXF  file  format.' +
    '||N.B. This export function is not Templot0''s normal data file saving function. To save your work in the normal way for later reloading, click the SAVE ALL... or SAVE GROUP... buttons on the STORAGE BOX,' + ' or click the FILES > SAVE ALL TEMPLATES... menu item on the trackpad window.' + '||This DXF export function is primarily intended as a means of transferring Templot0''s drawings to other software, or to specialist copying services for printing in large format.' + '||Exported DXF files can only be re-imported into Templot0 as part of the BACKGROUND SHAPES functions, which do not permit the templates to be further copied, adjusted or aligned.' + ' This is occasionally useful as a means of comparing two track plans, or when you want to use an existing track plan as a background design guide.' + '||Templot0 will generate a DXF file from the current background drawing using these colours and styles for each layer.' + ' Select your required settings from the drop-down lists.' + '||To omit a layer from the file, select NONE as the colour for that layer. You will rarely want to do this as' + ' most CAD software using this file format permits you to omit unwanted layers. The exception to this is the RADIAL CENTRES layer which should normally be omitted - see Handy Hints below.' + ' Omitting layers here does reduce the file size, which might be useful if you are saving to a floppy disk.' + '||Don''t get confused - these "layers" are only meaningful within your CAD program. To toggle elements of the drawing on or off within Templot,' + ' use the GENERATOR > GENERATOR SETTINGS > menu items. Each background template is exported in its present state. If timber outlines, say, were switched off in the GENERATOR SETTINGS > menu when the background template' + ' was copied or rebuilt, they won''t appear in the DXF file, regardless of which colour you choose for them.' + '||The DXF file can be exported in millimetres (to 2 decimal places) or inches (to 4 decimal places), click the MM or INCHES option buttons accordingly.' + ' You will probably need to specify which when importing the file into your CAD program. (If you have difficulty getting the CAD drawing to import at the correct size, try using the other option.)' + '||If the SCALED box is ticked, the DXF data will be scaled in accordance with the current PRINT > ENLARGE / REDUCE SIZE menu setting, as shown.' + ' If this box is blank, the DXF data will be exported at full-size (100%) regardless of the print size setting.' + '||To omit some of your background templates from the file, click the GROUP TEMPLATES ONLY option button, having first selected the ones you do require as members of a group.' + ' (By clicking them on the pad, and then clicking GROUP SELECT (TOGGLE) on the pop-up menu, or by clicking the GROUP SELECT button in the storage box.)' + '||The control template is not included in DXF files. To export the control template only, you should store a copy of the control template as a background template, and then select it as the only member of a group.' + '||If the DRAWING LIMITS box is ticked, only the currently visible part of the trackpad drawing will be included in the file, and the drawing limit dimensions will be included in the file header.' + '||By re-sizing the trackpad window on the screen, and zooming in or out, you can in this way export any selected area of the drawing.' + ' Compare this option with GROUP TEMPLATES ONLY (see above), in which only the selected group templates are exported, but without regard to whether they are currently visible.' + '||If the DRAWING LIMITS box is left blank, the entire background area will be exported, regardless of the current trackpad view and zoom settings. (If your CAD software reports "Bad Header" when loading files from Templot,' + ' this box should be ticked, and you should then zoom out and re-position the trackpad view sufficiently to see all of the required part of your drawing.)' + '||If the 3-D boxes are ticked, some 3-dimensional data will be included in the DXF file for the rail height and timber thickness (wire-frame or solid-rendered format).' + ' Bear in mind that this will very significantly increase the DXF file size and the time taken for your CAD software to load the data. There is nothing to be gained by this if your CAD software does not have 3-D capability.' + ' Do not use the 3-D options if this exported DXF file is intended to be re-imported into Templot0''s Background Shapes.' + '||The rail and timber 3-D dimensions are set via the REAL > RAILS > RAIL SECTION DATA..., REAL > CHAIRS/BASEPLATES > CHAIR/BASEPLATE DATA... and REAL > TIMBERING > TIMBERING DATA... menu items,' + ' and can differ for each template. The rail top is set as the zero datum Z-dimension, so that templates with differing rail section heights will be correctly aligned.' + '||When the 3-D options are selected, timber centre-lines, 8ft6in timber end marks and timber numbering are omitted from the file automatically. You may wish to omit other features by setting their colours to NONE.' + '||N.B. For the timbers to be correctly drawn in 3-D, it is necessary to have de-selected OUTLINE EXTENSION MARKS in the GENERATOR > GENERATOR SETTNGS > menu, and then clicked REBUILD ALL BACKGROUND.' + ' ( And TIMBER INFILL should be selected, otherwise the timbers will be drawn hollow.)' + '||Your chosen colours may or may not appear correctly - some CAD software maps different colours onto the DXF pen codes' + ' or uses the current pen colour settings instead. And "black" means "white" if your CAD program uses a dark screen.' + ' Experiment with dotted and dashed lines to get the required result in the CAD program you are using.' + '||Unlike Templot0''s own box file format, the DXF file contains only the template drawings. The information texts' + ' and your memo texts are not included. If these details will be needed externally to Templot, click the WRITE button in the information panel' + ' and save them as a separate text file.' + '||In generating the DXF file, any printer calibration is ignored.' + ' The grid and page alignment marks are also omitted. If necessary for the destination software or printer, the aspect ratio and scaling can' + ' be adjusted using data distortions - see the PROGRAM > EXPERT menu items on the PROGRAM PANEL menus.' + '||For background PICTURE SHAPES the DXF file contains only a rectangular outline, not the bitmap image contents. Consult the documentaion for your CAD or drawing software about importing bitmap images.' + '||Handy Hints :' + '||If the DRAWING LIMITS box is left blank (see above), it is better to omit the radial centre marks. Your CAD program may otherwise scale down the drawing in order to accommodate them. If you are using very large radii your templates' + ' could then appear as little more than a smudge in the corner of the paper.' + '||If the DRAWING LIMITS box is ticked, the radial centre marks can be included, since only those visible on the trackpad will be included in the file.' + '||When using the DRAWING LIMITS option to export a selected area, set a fraction larger area to allow for the ragged edges. The margins can be cropped cleanly to your exact requirements in your CAD software.' + '||Remember that DXF files are much larger than Templot0''s own box data files. A complete track layout which requires say 150KB in Templot0 format can be 3MB or more as a DXF file. Don''t accumulate more of them than you need,' + ' and remember to omit unwanted features to save file space. It is better to save your work as Templot0 files and generate DXFs only as needed.' + '||N.B. If the SCALED box is ticked, and the print size is currently set to PRINT SINGLE PAGE, it will be necessary to have actually printed such a page before this size option will take effect for the DXF file.';

begin
  if help(0, help_str, 'DXF  chat') = 1 then
    chat_click;
end;
//________________________________________________________________________________________

procedure Tdxf_form.chat_panelClick(Sender: TObject);

begin
  chat_click;
end;
//____________________________________________________________________

procedure formsize_updown_click;

begin
  with dxf_form do begin

    if size_updown.Position > size_updown.Tag  // ! position goes up, size goes down.
    then
      ScaleBy(9, 10);                   // scale the form contents down.

    if size_updown.Position < size_updown.Tag then
      ScaleBy(10, 9);                   // scale the form contents up.

    ClientHeight := VertScrollBar.Range;
    // don't need bottom margin - datestamp label provides this (aligned alBottom).
    ClientWidth := HorzScrollBar.Range + 4;      // allow 4 pixel right margin.
    ClientHeight := VertScrollBar.Range;
    // do this twice, as each affects the other (autoscroll).

    size_updown.Tag := size_updown.Position;   // and save for the next click.
  end;//with
end;//proc
//____________________________________________________________________

procedure Tdxf_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  formsize_updown_click;
end;//proc
//____________________________________________________________________

procedure Tdxf_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  DXF  dialog', Color);
end;
//__________________________________________________________________________________________

procedure Tdxf_form.cancel_panelClick(Sender: TObject);

begin
  ModalResult := mrCancel;
end;
//_________________________________________________________________________________________

procedure dxf_background_keeps(var dxf_file: TextFile);  //  all the background keeps.

var
  i, n: integer;
  code: integer;

  move_to, line_to: TPoint;
  x1, y1, x2, y2, x3, y3, x4, y4: extended;

  layer: integer;

  aq: integer;
  array_max: integer;

  radcen_dim: integer;
  cen_point: Tpoint;

  now_keep: Tbgnd_keep;

  mod_name_x, mod_name_y: extended;
  text_height: extended;
  tbnum_str, text_str: string;

  fixed_diamond_ends: boolean;


  ////////////////////////////////////////////////////////////

  procedure dxf_text;      // put a text item in the file.

  var
    s: string;

  begin
    if colour(layer) <> '0|'       // does he want this in the file ?
    then begin
      x1 := move_to.x / 100;
      y1 := move_to.y / 100 - text_height;   // (convert Templot0 top-left to DXF bottom-left.)

      if dxf_form.limits_checkbox.Checked =
        True     // only within limits?
      then begin
        if (x1 > xmax) or (y1 > ymax) then
          EXIT;
        if (x1 < xmin) or (y1 < ymin) then
          EXIT;
      end;

      s :=
        '  0|TEXT|  8|' + layer_str[layer] +
        ' 10|' + make_dim(x1) + ' 20|' + make_dim(
        y1) + ' 40|' + make_dim(
        text_height) + '  1|' + text_str + '|';

      Write(dxf_file, insert_crlf_str(s));
    end;
  end;
  ////////////////////////////////////////////////////////////

  procedure dxf_mark;      // put a mark in the file.

  begin
    if colour(layer) <> '0|'       // does he want this in the file ?
    then begin

      if (_3d = False) or ((code <> 3) and
        (code <> 33) and (code <> 93))  // not 3-D timber edges.
      then begin
        x1 := move_to.x / 100;
        y1 := move_to.y / 100;
        x2 := line_to.x / 100;
        y2 := line_to.y / 100;

        Write(dxf_file, dxf_line(x1,
          y1, 0, x2, y2, 0, layer));
      end
      else begin    // 3-D timber edges...

        if dxf_form.timb_full_radiobutton.Checked =
          True     // (flat tops done elswhere.)
        then begin

          x1 := move_to.x / 100;
          y1 := move_to.y / 100;
          x2 := line_to.x / 100;
          y2 := line_to.y / 100;

          if wire_frame =
            True then begin
            Write(
              dxf_file, dxf_line(x1, y1, timb_top_z, x2, y2, timb_top_z, layer));  // top edge.
            Write(
              dxf_file, dxf_line(x2, y2, timb_top_z, x2, y2, timb_bot_z, layer));  // then down corner
            Write(
              dxf_file, dxf_line(x2, y2, timb_bot_z, x1, y1, timb_bot_z, layer));  // bottom edge.
            Write(
              dxf_file, dxf_line(x1, y1, timb_bot_z, x1, y1, timb_top_z, layer));  // then back up to top.
          end
          else
            Write(dxf_file, dxf_3dface(x1, y1, timb_top_z, x2, y2, timb_top_z, x2, y2, timb_bot_z,
              x1, y1, timb_bot_z, 17));
        end;
      end;
    end;
  end;
  ///////////////////////////////////////////////////////////

  procedure dxf_mark_end(aq1, aq1end, aq2, aq2end: integer);
  // make the background rail end mark.

  begin
    with now_keep do begin
      if (bgnd_endmarks_yn[aq1, aq1end] = True) and
        (bgnd_endmarks_yn[aq2, aq2end] = True) then begin
        move_to := bgnd_endmarks[aq1, aq1end];
        line_to := bgnd_endmarks[aq2, aq2end];

        x1 := move_to.x / 100;
        y1 := move_to.y / 100;
        x2 := line_to.x / 100;
        y2 := line_to.y / 100;

        Write(dxf_file, dxf_line(x1, y1, 0, x2, y2, 0, layer));
        // at rail top.

        if _3d = True   // add 3-D effect...
        then begin
          Write(
            dxf_file, dxf_line(x2, y2, 0, x2, y2, rail_foot_z, layer));  // then down corner to foot
          Write(
            dxf_file, dxf_line(x2, y2, rail_foot_z, x1, y1, rail_foot_z, layer));  // end of foot.
          Write(
            dxf_file, dxf_line(x1, y1, rail_foot_z, x1, y1, 0, layer));
          // then back up corner to top.

          if wire_frame = False
          // add solid infill for rail-end...
          then begin
            Write(
              dxf_file, dxf_3dface(x1, y1, 0, x2, y2, 0, x2, y2, rail_foot_z, x1, y1, rail_foot_z, 14));
          end;

        end;

      end;
    end;//with
  end;
  //////////////////////////////////////////////////////////

  procedure dxf_bgnd_rail(do_3d: boolean);

  var
    nk: integer;

    nko, array_max_outer, nk_3d_start: integer;
    move_to_outer, line_to_outer: TPoint;
    x_outer1, y_outer1, x_outer2, y_outer2: extended;

  begin
    with now_keep do begin

      if Length(list_bgnd_rails[aq]) = 0 then
        EXIT;                       // empty rail.

      array_max := High(list_bgnd_rails[aq]);
      if (do_3d = True) and (aq < 9) then
        array_max_outer := High(list_bgnd_rails[aq + 8])
      else
        array_max_outer := 0;

      // run along rail head...

      move_to.x := list_bgnd_rails[aq][0].X;
      move_to.y := list_bgnd_rails[aq][0].Y;

      nk_3d_start := 0;     // keep compiler happy.

      if array_max_outer > 0     // for 3-D.
      then begin
        move_to_outer.x :=
          list_bgnd_rails[aq + 8][0].X;
        move_to_outer.y :=
          list_bgnd_rails[aq + 8][0].Y;

        case aq of
          1:
            nk_3d_start := planing_end_aq1; // start top infill from end of planing
          2:
            nk_3d_start := planing_end_aq2;
          else
            nk_3d_start := 0;
        end;//case
      end;

      for nk := 1 to array_max do begin

        line_to.x := list_bgnd_rails[aq][nk].X;
        line_to.y := list_bgnd_rails[aq][nk].Y;

        x1 := move_to.x / 100;
        y1 := move_to.y / 100;
        x2 := line_to.x / 100;
        y2 := line_to.y / 100;

        Write(dxf_file, dxf_line(x1, y1, 0, x2, y2, 0, layer));
        // along rail top

        if do_3d = True               // add 3-D effect...
        then begin
          // rail sides...

          if wire_frame = True
          then
            Write(dxf_file, dxf_line(x2, y2, 0, x2, y2, rail_foot_z, layer));  // then down to foot

          Write(
            dxf_file, dxf_line(x2, y2, rail_foot_z, x1, y1, rail_foot_z, layer));
          // back along foot.

          if wire_frame = True
          then
            Write(dxf_file, dxf_line(x1, y1, rail_foot_z, x1, y1, 0, layer));
          // then back up to top.

          if wire_frame = False  // add solid rail side infill
          then begin
            Write(
              dxf_file, dxf_3dface(x1, y1, 0, x2, y2, 0, x2, y2, rail_foot_z, x1, y1, rail_foot_z, 14));
          end;

          // rail top surface, use same index into outer rail edge (aproximation) (except blades)...

          if array_max_outer > 0
          // valid outer rail edge.
          then begin
            nko := nk - nk_3d_start;
            if nko > array_max_outer then
              nko := array_max_outer;
            if nko < 0 then
              nko := 0;

            line_to_outer.x :=
              list_bgnd_rails[aq + 8][nko].X;
            line_to_outer.y :=
              list_bgnd_rails[aq + 8][nko].Y;

            x_outer1 := move_to_outer.x / 100;
            y_outer1 := move_to_outer.y / 100;
            x_outer2 := line_to_outer.x / 100;
            y_outer2 := line_to_outer.y / 100;


            if wire_frame = True
            then
              Write(dxf_file, dxf_line(x1, y1, 0, x_outer1, y_outer1, 0, layer))  // wire across the top.
            else
              Write(dxf_file, dxf_3dface(x1, y1, 0, x_outer1, y_outer1, 0, x_outer2, y_outer2, 0, x2, y2, 0, 15));
            // solid top face of rail (4 corners of segment).

            move_to_outer := line_to_outer;
            // for next
          end;
        end;

        move_to := line_to;

      end;//next nk
    end;//with
  end;
  ////////////////////////////////////////////////////////////

begin

  if keeps_list.Count < 1 then
    EXIT;  // how did this happen ?

  fixed_diamond_ends := False;  // keep compiler happy.

  for n := 0 to (keeps_list.Count - 1) do begin

    with Ttemplate(keeps_list.Objects[n]) do begin   // to next template.

      if bg_copied = False then
        CONTINUE;              // no data, unused template.

      if (dxf_form.group_option_button.Checked = True) and (group_selected = False)
      // ignore this one.
      then
        CONTINUE;

      with template_info.keep_dims.box_dims1.proto_info do begin
        // 3-D data for this template...
        rail_foot_z := 0 - rail_height_pi * scale_pi / 12;          // rail top is z datum.
        timb_top_z := rail_foot_z - seat_thick_pi * scale_pi / 12;
        timb_bot_z := timb_top_z - timber_thick_pi * scale_pi / 12;
      end;//with proto_info

      with template_info.keep_dims.turnout_info2 do
        fixed_diamond_ends := (semi_diamond_flag = True) and (diamond_fixed_flag = True);
      // need end marks on fixed diamond point rails.

      now_keep := bgnd_keep;   // get the drawing data.

    end;//with Ttemplate

    with now_keep do begin

      tbnum_str := timber_numbers_string;      // the full string of timber numbering.

      // first do the bgnd marks and timbers ...

      array_max := High(list_bgnd_marks);

      for i := 0 to array_max do begin

        code := list_bgnd_marks[i].code;   // check this mark wanted.

        case code of
          -5:
            layer := 12;   // keep labels

          -3, -2:
            layer := 8;    // curving rad centres.

          -4, -1, 0, 8, 9, 10:
            CONTINUE;    // timber selector, fixing peg, blank entries, peg arms, plain track start marks.  not in DXF.

          1:
            layer := 6;    // guide marks.

          2, 7:
            layer := 7;    // radial end marks.  transition marks.

          3, 33, 93:
            layer := 3;    // timber outlines.

          4, 14, 44, 54: begin
            if _3d = True then
              CONTINUE
            else
              layer := 5;    // timber centre-lines.
          end;

          5, 55, 95: begin
            if _3d = True then
              CONTINUE
            else
              layer := 4;    // timber reduced ends.
          end;

          6:
            layer := 9;    // rail joints.

          99: begin
            if _3d = True then
              CONTINUE
            else
              layer := 13;   // timber numbering.
          end;

          203, 233, 293: begin
            if _3d = False then
              CONTINUE
            else
              layer := 17;   // timber infill for 3-D.
          end;

          else
            CONTINUE;
        end;//case

        move_to.x := list_bgnd_marks[i].p1.X;    // x1,y1 in  1/100ths mm
        move_to.y := list_bgnd_marks[i].p1.Y;

        if (code = 99) or (code = -5)  // text
        then begin
          line_to.x := 0;    // x2,y2  not used.
          line_to.y := 0;
        end
        else begin
          line_to.x := list_bgnd_marks[i].p2.X;    // x2,y2 in  1/100ths mm
          line_to.y := list_bgnd_marks[i].p2.Y;
        end;


        if ((code = 203) or (code = 233) or (code = 293)) and (i < array_max)      // timber infill
          and (_3d = True) and (wire_frame = False) and (colour(layer) <> '0|') then begin
          x1 := move_to.x / 100;
          y1 := move_to.y / 100;  // to mm.
          x2 := line_to.x / 100;
          y2 := line_to.y / 100;


          x3 := (list_bgnd_marks[i + 1].p1.X) / 100;
          y3 := (list_bgnd_marks[i + 1].p1.Y) / 100;

          x4 := (list_bgnd_marks[i + 1].p2.X) / 100;
          y4 := (list_bgnd_marks[i + 1].p2.Y) / 100;

          Write(dxf_file, dxf_3dface(x1, y1, timb_top_z, x2, y2, timb_top_z,
            x3, y3, timb_top_z, x4, y4, timb_top_z, 17));

          CONTINUE;
        end;

        if (code > 0) and (code <> 99) then
          dxf_mark                // put it in the file
        else begin
          if (code = -2) or (code = -3)    // curving rad centres...
          then begin
            cen_point := move_to;            // temp save rad centre point.
            radcen_dim := Round(screenx / 2);
            // i.e. *100/200  to 1/100th mm  (200 arbitrary).

            move_to.x := cen_point.x - radcen_dim;
            move_to.y := cen_point.y;
            line_to.x := cen_point.x + radcen_dim;
            line_to.y := cen_point.y;

            dxf_mark;  // horizontal part.

            move_to.x := cen_point.x;
            move_to.y := cen_point.y - radcen_dim;
            line_to.x := cen_point.x;
            line_to.y := cen_point.y + radcen_dim;

            dxf_mark;  // vertical part.
          end;

          if code = -5       // keep name labels
          then begin
            text_str :=
              Trim(Copy(Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.reference_string, 1, 99));
            // not the keep number.

            text_height := pad_form.bgnd_keeps_font_label.Font.Size * 25.4 / 72;
            // font height in mm.
            text_height := text_height * Screen.PixelsPerInch / 96;
            // to match screen appearance.

            mod_name_x :=
              Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.mod_text_x;
            mod_name_y :=
              Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.mod_text_y;

            move_to.x := move_to.x + Round(mod_name_x * 100);
            // (Templot top-left of text.)
            move_to.y := move_to.y + Round(mod_name_y * 100);

            dxf_text;
          end;

          if code = 99     // timber numbering
          then begin
            move_to.x := move_to.x - 200;
            // 2mm arbitrary, because width of CAD font not known.

            text_height := print_timber_numbers_font.Size * 25.4 / 72;
            // font height in mm.

            text_str := extract_tbnumber_str(tbnum_str);
            // get next timber numbering string from the acummulated string.
            if text_str = '' then
              CONTINUE;              // no string available??

            text_str := IntToStr(n + 1) + '.' + text_str;
            // add template number prefix.

            dxf_text;
          end;
        end;

      end;//next i background mark


      // and then rails (do turnout rails last so they overwrite) ...

      layer := 2;           // layer CENTLINE    track centre-lines.

      if colour(layer) <> '0|'                              // does he want them in the file ?
      then
        for aq := 24 to 25 do
          dxf_bgnd_rail(False);   // track centre-lines (no 3-D option).

      layer := 1;           // layer ADJTRACK    adjacent track rails.

      if colour(layer) <> '0|'                            // does he want them in the file ?
      then
        for aq := 16 to 23 do
          dxf_bgnd_rail(_3d);   // all the adjacent rails.

      layer := 0;           // layer RAILS       turnout rails.

      if colour(layer) <> '0|'                      // does he want them in the file ?
      then begin

        for aq := 0 to 15 do
          dxf_bgnd_rail(_3d);   // all the turnout rails.
        for aq := 26 to 29 do
          dxf_bgnd_rail(_3d);   // K-crossing check rails.

        // next, draw in the rail ends...

        dxf_mark_end(1, 1, 9, 1);    // main rail wing rail finish.
        dxf_mark_end(2, 1, 10, 1);   // turnout rail wing rail finish.

        dxf_mark_end(6, 0, 14, 0);   // main side check rail start.
        dxf_mark_end(6, 1, 14, 1);   // main side check rail finish.

        dxf_mark_end(7, 0, 15, 0);   // turnout side check rail start.
        dxf_mark_end(7, 1, 15, 1);   // turnout side check rail finish.

        dxf_mark_end(4, 0, 5, 0);    // blunt nose.

        if fixed_diamond_ends = True then begin
          dxf_mark_end(1, 0, 9, 0);   // planed faced of point rails for a fixed-diamond.
          dxf_mark_end(2, 0, 10, 0);

          dxf_mark_end(26, 1, 27, 1);     // MS K-crossing check rails.
          dxf_mark_end(28, 1, 29, 1);     // DS K-crossing check rails.
        end;
      end;//if RAILS

    end;//with now_keep

  end;//next n template
end;
//___________________________________________________________________________________________

procedure dxf_shapes(var dxf_file: TextFile);     // do any background shapes.

var
  i, maxbg: integer;
  now_shape: Tbgnd_shape;
  x1, y1, x2, y2: extended;
  p, q, rad: extended;
  layer: integer;
  font_height: extended;
  s: string;
  arm, diamond: extended;

begin
  maxbg := bgnd_form.bgnd_shapes_listbox.Items.Count;
  if maxbg > 0                                             // any there ?
  then begin
    font_height := shapes_label_font.Size * 25.4 / 72;      // in mm for labels

    for i := 0 to maxbg - 1 do begin
      now_shape := Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape;
      // next shape.

      with now_shape do begin

        if ((shape_code = 0) and (shape_style = 2)) or (shape_code = -1) then
          layer := 11    // for dotted lines or pictures.
        else
          layer := 10;   // for the rest.

        if colour(layer) = '0|' then
          CONTINUE;        // he doesn't want it.

        if shape_code <> 4    // not a target mark
        then begin

          x1 := p1.x + re_org_x / 100;
          y1 := p1.y + re_org_y / 100;
          x2 := p2.x + re_org_x / 100;
          y2 := p2.y + re_org_y / 100;

          if dxf_form.limits_checkbox.Checked = True     // only within limits?
          // (!!! this check is duplicated in dxf_line.)
          then begin
            if (x1 > xmax) or (y1 > ymax) then
              CONTINUE;
            if (x1 < xmin) or (y1 < ymin) then
              CONTINUE;

            if shape_code <> 3         // x2,y2 not valid for labels...
            then begin
              if (x2 > xmax) or (y2 > ymax) then
                CONTINUE;
              if (x2 < xmin) or (y2 < ymin) then
                CONTINUE;
            end;
          end;

          case shape_code of
            0:
              Write(dxf_file, dxf_line(x1, y1, 0, x2, y2, 0, layer));  // line.

            -1, 1: begin  // make rectangle or picture.

              Write(dxf_file, dxf_line(x1, y1, 0, x1, y2, 0, layer));
              Write(dxf_file, dxf_line(x1, y2, 0, x2, y2, 0, layer));
              Write(dxf_file, dxf_line(x2, y2, 0, x2, y1, 0, layer));
              Write(dxf_file, dxf_line(x2, y1, 0, x1, y1, 0, layer));

              if shape_code = -1     // add diagonals for picture...
              then begin
                Write(
                  dxf_file, dxf_line(x1, y1, 0, x2, y2, 0, layer));  // diagonal
                Write(
                  dxf_file, dxf_line(x1, y2, 0, x2, y1, 0, layer));  // other diagonal
              end;
            end;

            2: begin  // make circle.

              p := (x1 + x2) / 2;    // average to get circle centres..
              q := (y1 + y2) / 2;
              rad := (ABS((x1 - x2) / 2) + ABS((y1 - y2) / 2)) / 2;
              // and radius.

              s :=
                '  0|CIRCLE|  8|' + layer_str[layer] +
                ' 10|' + make_dim(p) + ' 20|' + make_dim(
                q) + ' 40|' + make_dim(rad);

              Write(dxf_file, insert_crlf_str(s));
            end;

            3: begin    // label shape
              s :=
                '  0|TEXT|  8|' + layer_str[layer] +
                ' 10|' + make_dim(x1) + ' 20|' +
                make_dim(y1) + ' 40|' + make_dim(
                font_height)  // text height for labels
                + '  1|' + shape_name + '|';        // label string.

              Write(dxf_file, insert_crlf_str(s));

            end;
          end;//case
        end
        else begin    // shape_code=4, draw a target mark

          x1 := p1.x + re_org_x / 100;
          y1 := p1.y + re_org_y / 100;

          //( !!! check limits is in dxf_line.)

          arm := p2.x;            // cross arm length.
          diamond := arm / 2;       // size of centre diamond.

          Write(dxf_file, dxf_line(x1 - arm, y1, 0, x1 + arm, y1, 0, layer));
          // horizontal arms.
          Write(dxf_file, dxf_line(x1, y1 - arm, 0, x1, y1 + arm, 0, layer));
          // vertical arms.

          // now do 4 diamond lines...

          Write(dxf_file, dxf_line(x1 - diamond, y1, 0,
            x1, y1 + diamond, 0, layer));     // NW line.
          Write(dxf_file, dxf_line(x1, y1 + diamond, 0,
            x1 + diamond, y1, 0, layer));     // NE line.
          Write(dxf_file, dxf_line(x1 + diamond, y1, 0,
            x1, y1 - diamond, 0, layer));     // SE line.
          Write(dxf_file, dxf_line(x1, y1 - diamond, 0,
            x1 - diamond, y1, 0, layer));     // SW line.
        end;

      end;//with now_shape
    end;//for next i shape
  end;//if shapes
end;
//__________________________________________________________________________________________

procedure Tdxf_form.FormShow(Sender: TObject);

begin
  ok_button.SetFocus;
end;
//__________________________________________________________________________________

procedure Tdxf_form.rails_comboChange(Sender: TObject);

begin
  if (Sender is TComboBox) then
    TComboBox(Sender).Color := clWindow;
  top_panel.Hide;
  bottom_panel.Hide;
end;
//_________________________________________________________________________________________

procedure Tdxf_form.rails_comboDropDown(Sender: TObject);

var
  top_panel_bottom: integer;

begin
  with TComboBox(Sender) do begin
    Color := $00FFFFAA;      // clAqua
    top_panel_bottom := Top;
    top_panel.Height := top_panel_bottom - top_panel.Top;

    bottom_panel.Top := Top + Height;
    bottom_panel.Height := text_combo.Top + text_combo.Height - bottom_panel.Top;

    top_panel.Show;
    bottom_panel.Show;
  end;//with

end;
//_______________________________________________________________________________________

procedure Tdxf_form.FormCreate(Sender: TObject);

begin
  if Screen.Height < 500 then
    Top := 2;    // move form up the screen for lo-res.

  // OT-FIRST ClientWidth:=642;
  // OT-FIRST ClientHeight:=444;
  AutoScroll := True;
end;
//_______________________________________________________________________________________

procedure Tdxf_form.FormActivate(Sender: TObject);

begin
  with scaled_checkbox do begin
    if Checked = True then
      Caption := 's&caled  ' + round_str(out_factor * 100, 2) + ' %'
    else
      Caption := 's&caled';
  end;//with

end;
//________________________________________________________________________________________

procedure Tdxf_form.scaled_checkboxClick(Sender: TObject);

var
  dxf_scaling_help_str: string;

begin
  dxf_scaling_help_str := '      `0DXF output scaling`9' +
    '||The DXF output will now be scaled at ' + round_str(out_factor * 100, 2) + ' %'
    + '||This setting can be changed on the `0output`1 menu at `0enlarge/reduce size`1 menu items.'
    +
    '||For export to a CAD program you should normally leave this on 100% and do any required scaling in the CAD program.'
    +
    '||For output to graphics editing and drawing programs you may get better results by exporting a metafile image instead (EMF file).';

  with scaled_checkbox do begin
    if Checked = True then begin
      Caption := 's&caled  ' + round_str(out_factor * 100, 2) + ' %';
      help(0, dxf_scaling_help_str, '');
    end
    else
      Caption := 's&caled';
  end;//with

end;
//_______________________________________________________________________________________

procedure Tdxf_form._2d_radiobuttonClick(Sender: TObject);

begin
  _3d := False;

  timb_full_radiobutton.Enabled := False;
  timb_flat_radiobutton.Enabled := False;
  _3d_timbers_groupbox.Enabled := False;
  _3d_colours_button.Enabled := False;

end;
//_________________________________________________________________________

procedure enable_warn_timber_extensions;

begin

  dxf_form.timb_full_radiobutton.Enabled := True;
  dxf_form.timb_flat_radiobutton.Enabled := True;
  dxf_form._3d_timbers_groupbox.Enabled := True;
  dxf_form._3d_colours_button.Enabled := True;

  if outline_extensions = True then begin
    alert(3, ' 3-D  -  timber  outline  extension  marks',
      'The timber OUTLINE EXTENSION MARKS option is currently selected in the GENERATOR > GENERATOR SETTINGS > menu.' + '||This will prevent the timbers from being correctly drawn in 3-D.' + '||De-select this option and then click GENERATOR > REBUILD ALL BACKGROUND.' + '||Ensure that GENERATOR > GENERATOR SETTINGS > TIMBER INFILL is selected, otherwise the timbers will be drawn hollow.',
      '', '', '', '', '', 'continue', 0);
  end;
end;
//_______________________________________________________________________________

procedure Tdxf_form._3d_wireframe_radiobuttonClick(Sender: TObject);

begin
  _3d := True;
  wire_frame := True;
  enable_warn_timber_extensions;
end;
//________________________________________________________________________

procedure Tdxf_form._3d_solid_radiobuttonClick(Sender: TObject);

begin
  _3d := True;
  wire_frame := False;
  enable_warn_timber_extensions;
end;
//___________________________________________________________________

procedure Tdxf_form._3d_colours_buttonClick(Sender: TObject);

var
  i: integer;

begin
  i := alert(4, '   3-D  rail-side  colour ...',
    'Choose a colour for the rail-sides in 3-D solid format.',
    'red', 'blue', 'steel  grey', 'light  rust', 'cancel', 'dark  rust', 0);

  case i of
    1:
      railsides_3d_colour := 1;  // red
    2:
      railsides_3d_colour := 5;
    3:
      railsides_3d_colour := 252;
    4:
      railsides_3d_colour := 31;
    5:
      EXIT;
    6:
      railsides_3d_colour := 33;
  end;//case


  i := alert(4, '   3-D  timber  colour ...',
    'Choose a colour for the timbers in 3-D solid format.',
    'black', 'stone', 'dark  brown', 'light  brown', 'cancel', 'grey', 0);

  case i of
    1:
      timber_3d_colour := 7;    // black.
    2:
      timber_3d_colour := 54;   // stone = olive.
    3:
      timber_3d_colour := 33;   // dark brown.
    4:
      timber_3d_colour := 31;   // light brown.
    5:
      EXIT;
    6:
      timber_3d_colour := 8;    // grey.
  end;//case

end;
//_________________________________________________________________________


initialization

  line_type_str[0] := 'CONTINUOUS|';
  line_type_str[1] := 'DASHED|';
  line_type_str[2] := 'HIDDEN|';
  line_type_str[3] := 'CENTER|';
  line_type_str[4] := 'DOT|';
  line_type_str[5] := 'DASHDOT|';

  layer_str[0] := 'RAILS|';
  layer_str[1] := 'ADJTRACK|';
  layer_str[2] := 'CENTLINE|';
  layer_str[3] := 'TIMBOUTL|';
  layer_str[4] := 'SLEEPEND|';
  layer_str[5] := 'TIMBCENT|';
  layer_str[6] := 'GDMARKS|';
  layer_str[7] := 'RADMARKS|';
  layer_str[8] := 'RADCENTS|';
  layer_str[9] := 'JOINTS|';
  layer_str[10] := 'SBGSHAPE|';
  layer_str[11] := 'DBGSHAPE|';
  layer_str[12] := 'INFOTEXT|';
  layer_str[13] := 'TIMBNUMB|';
  layer_str[14] := 'RLSIDE3D|';
  layer_str[15] := 'RLTOP3D|';
  layer_str[16] := 'CHAIR3D|';
  layer_str[17] := 'TIMBER3D|';
  //_______________________________________________________________________________________

end.
