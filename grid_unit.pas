
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

unit grid_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  Tgrid_form = class(TForm)
    blue_corner_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    close_panel: TPanel;
    close_button: TButton;
    make_shapes_button: TButton;
    make_target_button: TButton;
    ring_copies_groupbox: TGroupBox;
    make_copy_button: TButton;
    delete_copy_button: TButton;
    clear_button: TButton;
    pen_groupbox: TGroupBox;
    move_pen_button: TButton;
    draw_line_button: TButton;
    set_groupbox: TGroupBox;
    ring_size_button: TButton;
    dia_label: TLabel;
    measuring_panel: TPanel;
    x_label: TLabel;
    y_label: TLabel;
    diag_label: TLabel;
    cross_hairs_button: TButton;
    ring_location_button: TButton;
    colour_button: TButton;
    six_foot_button: TButton;
    jump_groupbox: TGroupBox;
    jump_to_notch_button: TButton;
    jump_to_centre_button: TButton;
    jump_to_mouse_button: TButton;
    mouse_actions_groupbox: TGroupBox;
    mouse_button: TButton;
    adjust_ring_dia_button: TButton;
    help_button: TButton;
    dummy_vehicle_copies_groupbox: TGroupBox;
    dummy_vehicle_make_copy_button: TButton;
    dummy_vehicle_delete_copy_button: TButton;
    dummy_vehicle_clear_copies_button: TButton;
    roll_dummy_vehicle_button: TButton;
    show_ring_groupbox: TGroupBox;
    show_rings_radio_button: TRadioButton;
    hide_rings_radio_button: TRadioButton;
    dummy_vehicle_groupbox: TGroupBox;
    show_dummy_vehicles_radio_button: TRadioButton;
    hide_dummy_vehicles_radio_button: TRadioButton;
    road_groupbox: TGroupBox;
    main_road_dummy_vehicle_radio_button: TRadioButton;
    turnout_road_dummy_vehicle_radio_button: TRadioButton;
    dummy_vehicle_dimensions_button: TButton;
    dummy_vehicle_clearance_button: TButton;
    attach_ring_checkbox: TCheckBox;
    adjust_adjacent_centres_ms_button: TButton;
    adjust_adjacent_centres_ts_button: TButton;
    help_shape: TShape;
    Shape1: TShape;
    reset_centre_line_button: TButton;
    vehicle_envelope_button: TButton;
    clear_first_envelope_button: TButton;
    ring_copy_colour_button: TButton;
    clear_last_envelope_button: TButton;
    clear_all_envelopes_button: TButton;
    Label1: TLabel;
    blank_ends_checkbox: TCheckBox;
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure colour_panelClick(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ring_size_buttonClick(Sender: TObject);
    procedure colour_buttonClick(Sender: TObject);
    procedure make_target_buttonClick(Sender: TObject);
    procedure make_shapes_buttonClick(Sender: TObject);
    procedure clear_buttonClick(Sender: TObject);
    procedure delete_copy_buttonClick(Sender: TObject);
    procedure make_copy_buttonClick(Sender: TObject);
    procedure move_pen_buttonClick(Sender: TObject);
    procedure draw_line_buttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure show_rings_radio_buttonClick(Sender: TObject);
    procedure hide_rings_radio_buttonClick(Sender: TObject);
    procedure mouse_buttonClick(Sender: TObject);
    procedure jump_to_centre_buttonClick(Sender: TObject);
    procedure ring_copy_colour_buttonClick(Sender: TObject);
    procedure cross_hairs_buttonClick(Sender: TObject);
    procedure ring_location_buttonClick(Sender: TObject);
    procedure jump_to_notch_buttonClick(Sender: TObject);
    procedure six_foot_buttonClick(Sender: TObject);
    procedure jump_to_mouse_buttonClick(Sender: TObject);
    procedure adjust_ring_dia_buttonClick(Sender: TObject);
    procedure dummy_vehicle_dimensions_buttonClick(Sender: TObject);
    procedure turnout_road_dummy_vehicle_radio_buttonClick(Sender: TObject);
    procedure main_road_dummy_vehicle_radio_buttonClick(Sender: TObject);
    procedure hide_dummy_vehicles_radio_buttonClick(Sender: TObject);
    procedure show_dummy_vehicles_radio_buttonClick(Sender: TObject);
    procedure roll_dummy_vehicle_buttonClick(Sender: TObject);
    procedure dummy_vehicle_clear_copies_buttonClick(Sender: TObject);
    procedure dummy_vehicle_delete_copy_buttonClick(Sender: TObject);
    procedure dummy_vehicle_make_copy_buttonClick(Sender: TObject);
    procedure dummy_vehicle_clearance_buttonClick(Sender: TObject);
    procedure attach_ring_checkboxClick(Sender: TObject);
    procedure adjust_adjacent_centres_ts_buttonClick(Sender: TObject);
    procedure adjust_adjacent_centres_ms_buttonClick(Sender: TObject);
    procedure reset_centre_line_buttonClick(Sender: TObject);
    procedure vehicle_envelope_buttonClick(Sender: TObject);
    procedure clear_first_envelope_buttonClick(Sender: TObject);
    procedure clear_last_envelope_buttonClick(Sender: TObject);
    procedure clear_all_envelopes_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  grid_form: Tgrid_form;

procedure arc_ellipse(canv: TCanvas; X1, Y1, X2, Y2: integer);

//---------------------

const
  ring_help_str: string = '     Spacing-Ring  Tool' +
    '||The spacing-ring is used as a design aid to check the spacing and clearances between adjacent tracks, model structures and baseboard constraints.' + '||It can also be used to make reference marks on the trackpad, as a general measuring tool, and as a drawing pen.' + '||It can be positioned anywhere on the trackpad by mouse action (click the MOUSE ACTIONS > MOVE RING button) or by entering X,Y co-ordinates directly.' + ' Its diameter can be adjusted by mouse action (click the MOUSE ACTIONS > RING SIZE button) or by entering the diameter directly.' + '||Multiple copies of the ring can be created and left in position on the trackpad as fixed markers.' + '||Any infringement of the ring (and optionally any copies) by the control template causes a warning lamp to flash on the trackpad INFORMATION panel.' + ' This is useful when adjusting the template to remain clear of obstructions, and when using the ring to measure clearances.' + '||    1.  Checking  Track  Spacings  and  Clearances' + '||To set the size of the ring, click the SET RING > SIZE... button and enter the required inner diameter of the spacing-ring in mm, or click the MOUSE ACTIONS > RING SIZE button to set the diameter by mouse action.' + ' The outer diameter of the ring is always automatically set to correspond to a ring-width equal to the current rail-width.' + '||This then means that you normally use the INNER diameter of the ring to check against the OUTER-EDGE of the rail, and the OUTER diameter of the ring' + ' to check against the INNER or GAUGE-FACE edge of the rail.' + '||While using the mouse actions to adjust the template (with SKELETON DRAW - the normal setting), only the gauge-face rail edges are shown,' + ' which means that only the outer ring diameter is then relevant.' + '||If you select the pre-set dimension by clicking the SET RING > P-S button (or enter a slash "/") the ring size will be set to give a track centre-to-centre dimension of 11ft 2in scale for your current gauge and scale.' + ' This dimension corresponds to the standard minimum 6ft way between running lines for standard-gauge track.' + '||You should use the spacing-ring to check that no two tracks come closer than this pre-set dimension to maintain the proper clearance for passing trains, and if there is a curving radius' + '  of less than about 750ft scale (3000 mm or 10ft radius in 4mm scale), the clearance should be increased.' + '||The pre-set dimension represents the MINIMUM spacing for running lines - there is no harm in using wider spacings if conditions permit. Also, railway regulations require increased spacing (9ft or 10ft way)' + ' for loops and sidings alongside running lines for the safety of staff on the ground, and to give room for signal posts, etc.' + '||( If you are designing for a track gauge other than standard-gauge the pre-set should not be used - enter the ring dimension as required. For Irish 5ft 3in gauge, for example, enter the 6ft way' + ' dimension directly, e.g. enter 24mm for 6ft way with exact scale Irish 21mm gauge in 4mm/ft scale.)' + '||    2.  Marking  and  Measuring' + '||You can leave a copy of the ring in position as a reference point. Click the RING COPIES > MAKE button. This is useful when you need to mark several clearance points while making adjustments.' + ' You can have up to 32 such copies if you wish, and each one can be a different size. While you are moving the ring its current position on the trackpad is shown in title bar at the top of the screen.' + '||If the inner ring size is set to a small dimension or zero (click the MAKE TARGET button to make it 6" scale diameter), the spacing-ring and any copies of it become small target marks which are useful as general markers.' + ' The cross-hair lines are each 18" scale long.' + '||The spacing-ring can also be used as a gauge to check any other dimensions on the drawing as required - just remember that the size you set is the inner diameter of the ring.' + ' For example, if you need to fit a locomotive which is 200mm long into an engine spur, set the ring to 200mm and check the clear length of the spur.' + ' This method is sometimes easier than using the ruler tool (UTILS > RULER menu item) to check dimensions. A fixed diameter can be set and the ring can then be positioned with a single mouse action,' + ' whereas the ruler needs to have each end positioned independently.' + '||Or you can measure dimensions directly with the ring or the mouse. The readout panel displays the dimensions from the most recent copy of the ring to the current position.' + ' When the ring is being moved the dimensions are to the centre of the ring and the readout panel is yellow; at other times the readout panel is white and dimensions are to the mouse pointer,' + ' which can be changed to the cross-hairs symbol for accuracy by clicking the MOUSE CROSS-HAIRS button.' + '||To measure the distance between two positions on the drawing, simply move the centre of the ring to the first position and click the RING COPIES / MAKE button.' + ' Then move the ring or the mouse to the second position and read off the dimensions from the first one. The DIAGONAL dimension is the straight-line diagonal dimension between the two positions.' + '||    3.  Handy  Hints :' + '||For simple point-to-point measurements it is often easier to use the mouse readout functions on the Jotter. (UTILS > JOTTER ~ X-Y READOUT menu item. Right-click on the jotter for the help notes.)' + '||Moving the ring with the mouse action generates continuous trackpad redraws. When using the ring for marking and measuring you can get a faster response by using the mouse pointer directly. Try this:' + '|Press the F12 key to cancel the mouse action.' + '|Click the MOUSE CROSS-HAIRS button (or press CTRL-full stop key).' + '|Move the cross-hairs mouse pointer to the required ring position. But don''t click the mouse on it otherwise you will cancel the cross-hairs symbol.' + '|Select the JUMP RING TO > MOUSE button (not with the mouse of course, press the U key on the keyboard, shown underlined on the button).' + '|You can now click the RING COPIES > MAKE button (or press the K key) to start measuring from the ring to the mouse pointer.' + '||Make a note of the underlined accelerator keys on the other buttons. Then you can resize the form much smaller to avoid obstructing the drawing and still be able to use the buttons.' + '||The JUMP RING TO > NOTCH button jumps the ring to the current position of the pegging notch. By first using the NOTCH UNDER PEG functions, you can position the ring on the fixing peg of the control template, or any background template.' + '||Likewise the GEOMETRY > NOTCH OPTIONS > NOTCH ON SPACING RING menu item moves the pegging notch to the ring position, and you can then use the SHIFT ONTO NOTCH functions to position the control template at the ring position.' + '||To temporarily hide the ring and any copies, click the RINGS > HIDE option button. They will re-appear when you click the RINGS > SHOW option button, or select the UTILS > DUMMY VEHICLE • SPACING-RING menu item.' + '||You can choose different colours for the ring and any copies by clicking the COLOUR... buttons. But avoid choosing red if possible, otherwise at some zoom settings the ring might be confused with the fixing peg.' + '||When accuracy is needed, always zoom in so that the ring fills a good proportion of the visible pad. At lower zoom settings, rounding effects for the screen may appear to make the two ring diameters non-concentric.' + '||Like the pegging notch, the spacing-ring and its copies "belong" to the trackpad, not the control template, so you can''t save these items in the data files as they stand.' + ' To save a ring for future use, it can be converted to a collection of 4 background shapes (2 rings and 2 cross-hair lines) and included in your background shapes list along with any other shapes which you have defined. Click the' + ' MAKE SHAPES button to do this.' + '||Once in the background shapes list, the shapes forming the ring image can be modified as required in the same way as any other shape. You could delete the outer ring, for example, or change it to a square without changing any dimensions.' + ' (Note that you won''t actually see the underlying shapes until you move the ring, and then only if background shapes have been switched on in the TRACKPAD menu.)' + '||The spacing ring can also be used as a means of drawing other background features. If you click the DRAWING PEN > MOVE TO RING button, an imaginary "drawing pen" is moved to the current position of the spacing ring. After moving' + ' the ring, if you then click the DRAW TO RING button, the "pen" will draw a line to the new position of the ring. By repeatedly moving the ring and clicking DRAW TO RING, an irregular outline can be produced.' + ' Each drawn line becomes a separate shape in the background shapes list, and can be deleted or modified as required.' + '||( An easier way to draw simple free-hand shapes is to use the normal mouse drawing function - select the BACKGROUND > DRAW WITH MOUSE menu item.)' + '||Ring copies can also be used to define the dimensions of other background shapes as pre-sets. This is useful if you need a shape which is a specified distance from the rails, or from another shape.' + ' Click the help buttons in the background shapes list for more information (BACKGROUND > SHAPES menu item).' + '||Remember to save the shapes before quitting Templot0 (they are not included in the storage box data files).';

var
  ex, by, sx, sy, fx, fy, ffx, ffy, gx, gy: extended;
  xmax, ymax: integer;

  rc_ok: boolean = False;          // global release code. Here to fox hackers!

  show_spacing_rings: boolean = False;
  show_ruler_tool: boolean = False;

  draw_export_rectangle_flag: boolean = False;  // 0.93.a

procedure draw_background_templates(canv: TCanvas; group_code: integer;
  highlight_index: integer; highlight_on: boolean; highlight_colour: TColor);
//  all the background templates.

procedure do_background(mode: integer);                  //  draw the complete background.
procedure copy_draw_to_pad;                             //  copy the draw-bitmap to the pad.
procedure wipe_draw_bmp(add_sketchboard_items: boolean); //  wipe the draw-bitmap.
procedure wipe_pad;                                     //  wipe the actual pad form.

procedure draw_bg_shapes(canv: TCanvas; index, colour: integer);     // draw any background shapes.
procedure draw_notch(canv: TCanvas);
// draw the pegging notch. (might get redrawn by the control template).
procedure draw_rings(canv: TCanvas; copies, dummy_vehicle: boolean);
//206b  draw the spacing-ring or copies.

procedure draw_dummy_vehicle_copies(on_canvas: TCanvas);    // 0.98.a

function hover_mousedown(click_x, click_y, limit: extended): integer;
// highlight a bgnd keep if any at this clicked location.

function get_show_margins_info(calc_for_none: boolean): boolean;  // 0.93.a  

//_________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


uses
  point_ex, control_room, pad_unit,
  background_shapes,
  dummy_vehicle,
  bgnd_unit, bgkeeps_unit, math_unit, Math, preview_unit,
  colour_unit, help_sheet, shove_timber, keep_select, print_settings_unit,
  { OT-FIRST dtp_unit, dtp_settings_unit,} export_unit, { OT-FIRST pdf_unit,} entry_sheet,
  alert_unit,
  action_unit { OT-FIRST , file_viewer};

{$R *.lfm}
//_________________________________________________________________________________________

{     calc scaling, etc. ..

           y scaling factor for screen = same as x:

           640 * 480 dots = 1.33333 : 1  for which the
           screen area is 187 mm * 141 mm = 1.326 : 1 (Iiyama 17" monitor at 1024 x 768 resolution).

           y factor is negative - (calcs origin at bottom left).

           ex = end x dim (pixels) to left grid margin (printed page top)
           by = bottom y dim (pixels) to grid bottom margin (printed page left).

           gx = end zoom offset x (pixels)
           gy = bottom zoom offset y (pixels)

           draw_mode = 0 means initial draw - calc scaling.
                       1 means zoom free, so adjust the scaling to show whole turnout (per turnoutx).
                       2 means zoom locked, so we don't even change the existing scale. (but adjust zoom might have changed screenx).
}
//__________________________________________________________________________________

var
  now_keep: Tbgnd_keep;
  move_to, line_to, infill3, infill4: TPoint;
  p1, p2, p3, p4: TPoint;

  pen_now_at: Tpex;

procedure show_scalebar(canv: TCanvas); forward;
procedure draw_ruler(canv: TCanvas); forward;

procedure copy_drop_to_canvas(canv: TCanvas); forward;
// copy the drop-bitmap to the specified canvas.
procedure wipe_fill_drop; forward;
// draw as much as will remain fixed on the backdrop.
procedure draw_all_on_canvas(canv: TCanvas); forward;
// normal draw, use the specified canvas for everything.
procedure finish_on_canvas(canv: TCanvas); forward;
// draw any items not on the backdrop on the specified canvas.

procedure draw_export_rectangle(canv: TCanvas); forward;   // 0.93.a

procedure draw_dummy_vehicle_outline_envelope_as_polygon(on_canvas: TCanvas); forward;  // 215c

//___________________________________________________________________________________

function get_show_margins_info(calc_for_none: boolean): boolean;  // 0.93.a

var
  temp: extended;

begin

  Result := False;  // init

  case show_margins of     // 0.93.a

    0: begin
      // if no margins, do calcs as for printer if wanted but return False
      if calc_for_none = True then
        page_info(True, True, False, 0);
      EXIT;
    end;

    1:
      if page_info(True, True, False, 0) = False then
        EXIT;  // get current printer info and calc page limits etc.

      { OT-FIRST
      2: begin
           try
             pdf_width_dpi:=StrToInt(export_form.pdf_dpi_edit.Text);
             pdf_height_dpi:=pdf_width_dpi;                      // both the same

             pdf_height_mm:=StrToFloat(export_form.pdf_long_mm_edit.Text);
             pdf_width_mm:=StrToFloat(export_form.pdf_short_mm_edit.Text);
           except
             EXIT;
           end;//try

           if export_form.pdf_side_run_button.Checked=True   // swap dimensions for landscape
              then begin
                     temp:=pdf_height_mm;
                     pdf_height_mm:=pdf_width_mm;
                     pdf_width_mm:=temp;
                   end;

           if export_form.pdf_size_inside_trim_margins_checkbox.Checked=True   // 205e increase document size to allow for trim margin default sizes (fixed for PDF)
              then begin
                     pdf_width_mm:=pdf_width_mm+9.0;     // left margin 7mm,  right margin 2mm
                     pdf_height_mm:=pdf_height_mm+10.5;  // top margin 6mm, bottom margin 4.5mm
                   end;

           pdf_width_dots:=Round(pdf_width_mm*pdf_width_dpi/25.4);
           pdf_height_dots:=Round(pdf_height_mm*pdf_height_dpi/25.4);

           if page_info(True,True,True,0)=False then EXIT;   // get current PDF info and calc page limits etc.
         end;
}

    else
      EXIT;

  end;//case

  Result := True;
end;
//______________________________________________________________________________

procedure draw_page_outlines(canv: TCanvas);

var
  n, m, pl, pw, ml: extended;
  staggered: boolean;
  top_offset, left_offset: extended;
  page_count_long: integer;
  page_count_wide: integer;

  //margin_end_x:integer;
  porg_rec: integer;

  /////////////////////////////////////////////////////////////////

  procedure draw_margin_line;

  begin
    move_to.X := limits_i(-1, xmax + 2, move_to.X);
    // force within pad limits...
    move_to.Y := limits_i(-1, ymax + 2, move_to.Y);

    line_to.X := limits_i(-1, xmax + 2, line_to.X);
    line_to.Y := limits_i(-1, ymax + 2, line_to.Y);

    with canv do begin
      MoveTo(move_to.X, move_to.Y);
      LineTo(line_to.X, line_to.Y);
    end;//with
  end;
  //////////////////////////////////////////////////////////////////
begin

  if get_show_margins_info(False) = False then
    EXIT;   // no outlines wanted

  with canv do begin

    Brush.Color := paper_colour;
    Brush.Style := bsSolid;
    TextOut(0, 0, '');
    // !!! Delphi bug? This seems to be necessary before dotted lines and hatched masks will draw properly.
    // TextOut obviously initialises some background mask property which I have been unable
    // to find or set any other way.

    // show the printed page margins (if not more than outlines_limit pages long on screen, or adjust page origin is in progress) ...

    if (page_length > minfp) and (out_factor > minfp)          // div 0 checks.
    then begin
      if ((show_margins > 0) and
        (paper_bunching = False) { out 0.93.a and ((screenx*100*out_factor/page_length)<outlines_limit)}
        ) or (porg_mod = 1) or (out_factor_mod = 1) then begin

        Pen.Width := 1;
        Pen.Mode := pmCopy;           // use Pen.Color.
        Pen.Style := psDot;           // dotted trim margin lines.
        Pen.Color := page_colour;

        pl := page_length * sx / out_factor;
        // effective printer page length between trim margins.
        pw := page_width * sy / out_factor;
        // (sy negative) printer page width between trim margins (in 1/100 mm actual unscaled).

        if staggered_pages = True then
          ml := (sheet_down_c + 1) * pl + pl / 2    // total margin length.
        else
          ml := (sheet_down_c + 1) * pl;

        top_offset := print_pages_top_origin * fx;
        left_offset := print_pages_left_origin * fy;      //  (fy negative).

        if banner_paper = True then
          top_offset := top_offset - 25 * fx;    // 25 mm extra top space for banner paper.

        page_count_wide := 0;      // init.

        m := by - gy + left_offset;    //  draw first horz. line.
        staggered := False;

        repeat
          if staggered_pages = True then begin
            if page_count_wide = 0 then
            begin
              move_to.X := Round(ex - gx + top_offset);
              // first line.
              line_to.X := move_to.X + Round(ml - pl / 2);
              // shorter margin length.
            end
            else begin
              move_to.X := Round(ex - gx + top_offset - pl / 2);
              // subsequent lines.
              line_to.X := move_to.X + Round(ml);
              // full margin length.
            end;
          end
          else begin
            move_to.X := Round(ex - gx + top_offset);   // no stagger.
            line_to.X := move_to.X + Round(ml);       // full margin length.
          end;

          move_to.Y := Round(m);
          line_to.Y := move_to.Y;                     // horizontal line.

          if move_to.Y <> Round(by - gy) then
            draw_margin_line;  // don't overwrite the grid bottom datum line.

          // now add vertical lines...

          if staggered = True then
            n := top_offset - pl / 2
          else
            n := top_offset;       // alternate columns staggered.

          page_count_long := 0;      // init.

          while (n < (xmax + gx)) and (pl > 0) do begin
            // draw staggered vertical page lines up to next horizontal.

            if (banner_paper = False) or (page_count_long = 0) or
              (page_count_long = (sheet_down_c + 1))
            // no intermediate vertical page margins for banner printing.
            then begin

              move_to.X := Round(ex - gx + n);
              line_to.X := move_to.X;

              move_to.Y := Round(m);
              line_to.Y := Round(m + pw + 1);

              if move_to.X <> Round(ex - gx) then
                draw_margin_line;   // don't overwrite the grid left datum line.
            end;

            n := n + pl;
            Inc(page_count_long);
            if page_count_long > (sheet_down_c + 1) then
              BREAK;  // index+2 = count+1 to add right margin line (33 sheets = 34 margin lines)
          end;//while

          m := m + pw;
          Inc(page_count_wide);

          if staggered_pages = True then
            staggered := not staggered      // alternate columns staggered.
          else
            staggered := False;

        until (m < pw) or (page_count_wide > (sheet_across_c));
        // index+2 = count+1 to add top margin line (26 sheets = 27 margin lines)

        if staggered_pages = True then begin
          move_to.X := Round(ex - gx + top_offset - pl / 2);
          // add top margin line (26 sheets = 27 margin lines).
          line_to.X := move_to.X + Round(ml - pl / 2);
          // final margin is shorter by half a page.
        end
        else begin
          move_to.X := Round(ex - gx + top_offset);   // no stagger.
          line_to.X := move_to.X + Round(ml);       // full margin length.
        end;

        move_to.Y := Round(m);
        line_to.Y := move_to.Y;     // horizontal line.

        draw_margin_line;

        if (pad_form.show_printable_area_menu_entry.Checked = True) and
          (out_factor = 1.0) then begin
          move_to.X :=
            Round(ex - gx + (print_pages_top_origin - page_margin_top_mm) * fx);
          move_to.Y :=
            Round(by - gy + (print_pages_left_origin - page_margin_left_mm) * fy);

          line_to.X := move_to.X + Round(print_length * sx);
          line_to.Y := move_to.Y + Round(print_width * sy);

          Pen.Width := 1;
          Pen.Mode := pmCopy;
          Pen.Style := psSolid;
          Pen.Color := page_colour;

          Brush.Color := page_colour;
          Brush.Style := bsClear;

          if check_limits(move_to, line_to) = True then
            Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
        end;

      end;//if show page outlines

      // draw rectangle mark at the page origin...

      if (show_margins > 0) or (porg_mod = 1) or (out_factor_mod = 1) then begin
        porg_rec := Round(page_length * sx / 4);
        // 1/2 page length overall size.

        if porg_rec > 4 then
          porg_rec := 4;  // max size of mark (arbitrary).

        // 0.91.b was if porg_rec>Round(xmax/300) then porg_rec:=Round(xmax/300);  // max size of mark (arbitrary).

        move_to.X := Round(ex - gx + print_pages_top_origin * fx) - porg_rec;
        line_to.X := move_to.X + porg_rec * 2;

        move_to.Y := Round(by - gy + print_pages_left_origin * fy) - porg_rec;
        line_to.Y := move_to.Y + porg_rec * 2;

        Pen.Width := 1;
        Pen.Mode := pmCopy;
        Pen.Style := psSolid;
        Pen.Color := page_colour;

        Brush.Style := bsSolid;
        Brush.Color := page_colour;

        if check_limits(move_to, line_to) = True then
          Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
      end;
    end;
  end;//with
end;
//_________________________________________________________________________________________

procedure shift_keep_moveto(n: integer; canv: TCanvas);

begin
  if (n < 0) or (n > (keeps_list.Count - 1)) then
    EXIT;     // ???

  if Ttemplate(keeps_list.Objects[n]).group_selected = False  // not selected for mouse shift/rotate.
  then
    EXIT;

  move_to.X := move_to.X + Round(xshift_keeps * fx);   // for mouse action shift all keeps...
  move_to.Y := move_to.Y + Round(yshift_keeps * fy);

  infill3.X := infill3.X + Round(xshift_keeps * fx);   // timber infill...
  infill3.Y := infill3.Y + Round(yshift_keeps * fy);

  infill4.X := infill4.X + Round(xshift_keeps * fx);
  infill4.Y := infill4.Y + Round(yshift_keeps * fy);

  canv.Pen.Color := selection_colour;              // draw selected keeps in this colour.
end;
//_________________________________________________________________________________

procedure shift_keep_lineto(n: integer);

begin
  if (n < 0) or (n > (keeps_list.Count - 1)) then
    EXIT;   // ???

  if Ttemplate(keeps_list.Objects[n]).group_selected = False  // not selected for mouse shift/rotate.
  then
    EXIT;

  line_to.X := line_to.X + Round(xshift_keeps * fx);   // these increments are zero at other times.
  line_to.Y := line_to.Y + Round(yshift_keeps * fy);
  // n.b. approximation using screen co-ordinates during mouse action.
end;
//___________________________________________________________________________________

function keep_transform(krot: extended; pin: TPoint): TPoint;

  //  perform rotations/transformations about notch using on screen co-ords for twist keeps.
  //  enter with rotation angle krot.
  //  and input point pin.
  //  result point returned.

var
  x, y: extended;
  notch_centre_x, notch_centre_y: extended;

begin
  notch_centre_x := notchx * fx + ex - gx;      // notch centre in screen co-ords.
  notch_centre_y := notchy * fy + by - gy;

  x := pin.x - notch_centre_x;              // shift to origin
  y := pin.y - notch_centre_y;

  Result.x := Round(x * COS(krot) - y * SIN(krot) + notch_centre_x);   // rotate and shift back onto notch.
  Result.y := Round(x * SIN(krot) + y * COS(krot) + notch_centre_y);
end;
//______________________________________________________________________________________

procedure twist_keep_moveto(n: integer; canv: TCanvas);

begin
  if (n < 0) or (n > (keeps_list.Count - 1)) then
    EXIT;  // ???

  if Ttemplate(keeps_list.Objects[n]).group_selected = False  // not selected for mouse shift/rotate.
  then
    EXIT;

  move_to := keep_transform(kform_keeps, move_to);  // for mouse action twist all keeps...

  infill3 := keep_transform(kform_keeps, infill3);  // timber infill.
  infill4 := keep_transform(kform_keeps, infill4);

  canv.Pen.Color := selection_colour;              // draw selected keeps in this colour.
end;
//_________________________________________________________________________________

procedure twist_keep_lineto(n: integer);

begin
  if (n < 0) or (n > (keeps_list.Count - 1)) then
    EXIT;  // ???

  if Ttemplate(keeps_list.Objects[n]).group_selected = False  // not selected for mouse shift/rotate.
  then
    EXIT;

  line_to := keep_transform(kform_keeps, line_to);  // for mouse action twist all keeps.
end;
//___________________________________________________________________________________

procedure mark_end(n: integer; canv: TCanvas; aq1, aq1end, aq2, aq2end: integer; pen_solid: boolean);
// make the background rail end mark.

begin
  try

    with now_keep do begin
      if (bgnd_endmarks_yn[aq1, aq1end] = True) and (bgnd_endmarks_yn[aq2, aq2end] = True) then begin
        p1 := bgnd_endmarks[aq1, aq1end];
        p2 := bgnd_endmarks[aq2, aq2end];

        with canv do begin

          Pen.Width := 1;

          Brush.Color := paper_colour;  // gaps in dotted lines.
          Brush.Style := bsSolid;

          if pen_solid = True then
            Pen.Style := psSolid   // 0.93.a mods for platforms
          else
            Pen.Style := psDot;


          move_to.X := Round(p1.X * sx + ex - gx);
          move_to.Y := Round(P1.Y * sy + by - gy);
          line_to.X := Round(p2.X * sx + ex - gx);
          line_to.Y := Round(p2.Y * sy + by - gy);

          if shift_keeps_mod = 1 then begin
            shift_keep_moveto(n, canv);
            // mouse action shift keeps, sets pen colour if selected.
            shift_keep_lineto(n);
          end;

          if twist_keeps_mod = 1 then begin
            twist_keep_moveto(n, canv);  // mouse action twist keeps.
            twist_keep_lineto(n);
          end;

          if check_limits(move_to, line_to) = True then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;
        end;//with
      end;
    end;//with
  except
    EXIT;   // abandon this mark if calcs fail.
  end;//try
end;
//_______________________________________________________________________________________

procedure arc_ellipse(canv: TCanvas; X1, Y1, X2, Y2: integer);

// draw ellipse as 2 arcs. This is a bug fix.
// allows ellipse with centre clear.
// Delphi bug - Brush.Style bsClear doesn't work properly when drawing directly on the form canvas.

// 0.95.a now also used for control template peg

begin
  with canv do begin
    Arc(X1, Y1, X2, Y2, X1, Y1, X2, Y2);     // draw 2 180 deg arcs.
    Arc(X1, Y1, X2, Y2, X2, Y2, X1, Y1);
  end;//with
end;
//____________________________________________________________________________________

procedure draw_zooming_ring(canv: TCanvas);

// 205a changed to octogon to avoid being confused with spacing ring

var
  target_x, target_y, target_rad: integer;

  oct_w, oct_l: extended;           // 205a
  target_w, target_l: integer;      // 205a

begin
  if pad_form.show_zoom_target_menu_entry.Checked = True then begin
    with canv do begin
      Pen.Width := 2;         // 0.93.a , was 1
      Pen.Mode := pmCopy;
      Pen.Style := psSolid;

      Pen.Color := clYellow;   // changed 0.93.a

      oct_w := (xmax - ex) / 40;     // 205a half octagon width (octogon 1/20th of pad width)
      oct_l := oct_w * TAN(Pi / 8);  // 205a half of octogon side length

      target_w := Round(oct_w);
      target_l := Round(oct_l);

      target_x := Round((ex + xmax) / 2);
      target_y := Round(by / 2);

      // draw octogon...  205a

      MoveTo(target_x - target_w, target_y + target_l);
      LineTo(target_x - target_l, target_y + target_w);
      LineTo(target_x + target_l, target_y + target_w);
      LineTo(target_x + target_w, target_y + target_l);
      LineTo(target_x + target_w, target_y - target_l);

      LineTo(target_x + target_l, target_y - target_w);
      LineTo(target_x - target_l, target_y - target_w);
      LineTo(target_x - target_w, target_y - target_l);
      LineTo(target_x - target_w, target_y + target_l);

      Pen.Width := 1;  // restored for next

      // add cross lines...  205a

      MoveTo(target_x, target_y + target_w);
      LineTo(target_x, target_y - target_w);

      MoveTo(target_x - target_w, target_y);
      LineTo(target_x + target_w, target_y);

    end;//with
  end;
end;
//________________________________________________________________________________________

procedure draw_dotted_line(canv: TCanvas; move_to, line_to: TPoint);

begin
  with canv do begin
    Brush.Color := paper_colour;      // gaps in dotted lines
    Brush.Style := bsSolid;
    TextOut(0, 0, '');
    // !!! Delphi bug? This seems to be necessary before dotted lines will draw properly.
    // TextOut obviously initialises some background mask property which I have been unable
    // to find or set any other way.

    Pen.Style := psDot;
    Pen.Width := 1;         // dots work only for single-width lines.

    if check_limits(move_to, line_to) = True then begin
      MoveTo(move_to.X, move_to.Y);
      LineTo(line_to.X, line_to.Y);
    end;
  end;//with
end;
//_________________________________________________________________________________________

procedure draw_export_rectangle(canv: TCanvas);   // 0.93.a

var
  x1, y1, x2, y2: extended;

begin
  if draw_export_rectangle_flag = False then
    EXIT;

  with canv do begin
    Pen.Width := 2;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Color := pad_form.Font.Color;    // arbitrary

    Brush.Color := paper_colour;
    Brush.Style := bsClear;

    x1 := (output_rectangle_x1 * 100 + re_org_x) * sx;
    y1 := (output_rectangle_y1 * 100 + re_org_y) * sy;

    x2 := (output_rectangle_x2 * 100 + re_org_x) * sx;
    y2 := (output_rectangle_y2 * 100 + re_org_y) * sy;

    move_to.X := Round(ex - gx + x1);
    move_to.Y := Round(by - gy + y1);

    line_to.X := Round(ex - gx + x2);
    line_to.Y := Round(by - gy + y2);

    if check_limits(move_to, line_to) = True then
      Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);

    Pen.Width := 1;        // for all other lines on pad.
  end;//with canvas
end;
//______________________________________________________________________________

procedure draw_bg_shapes(canv: TCanvas; index, colour: integer);     // draw any background shapes.
// if index=-1 then draw all.
var
  font_height, font_size: extended;
  i, first_index, max_index: integer;
  x1, y1, x2, y2: extended;
  arm, diamond: extended;
  dummy_i: integer;
  raster_rect: TRect;

begin
  if (index < 0) or (index > (bgnd_form.bgnd_shapes_listbox.Items.Count - 1))
  // index out of range, draw all.
  then begin
    first_index := 0;
    max_index := bgnd_form.bgnd_shapes_listbox.Items.Count - 1;
  end
  else begin
    first_index := index;
    max_index := index;
  end;

  if (pad_form.show_shapes_menu_entry.Checked = True) and
    (bgnd_form.bgnd_shapes_listbox.Items.Count > 0) then begin
    with canv do begin
      Font.Assign(shapes_label_font);      // for labels.

      font_height := Font.Size * 25.4 / 72;      // in mm (on printer when full size).
      font_height := font_height * fx;         // in pixels scaled to pad.

      font_size := font_height / Screen.PixelsPerInch * 72;      // point size scaled to pad

      font_size := limits(1, Font.Size * 2, font_size, dummy_i);  // limit font size on screen.

      Font.Size := Round(font_size);
      // font scaled down to the pad (but not up more than double).

      Pen.Mode := pmCopy;

      for i := first_index to max_index do begin

        if ((corner1_mod = 1) or (corner2_mod = 1) or (oneshape_shift_mod = 1) or
          (oneshape_scale_mod = 1)) and (i = bgnd_form.bgnd_shapes_listbox.ItemIndex)
        then
          Pen.Color := clRed                  // he's adjusting a single shape (but not all of them).
        else
          Pen.Color := colour;
        try
          Pen.Style := psSolid;              // changes later for dotted lines.

          // mods 0.73.a 23-9-01...
          Pen.Width := 1;
          if bgnd_form.pad_shapes_linewidth_2_radiobutton.Checked = True then
            Pen.Width := 2;
          if bgnd_form.pad_shapes_linewidth_3_radiobutton.Checked = True then
            Pen.Width := 3;

          with Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape do
          begin     // next shape.

            if (hide_bits and $01) <> 0 then
              CONTINUE;   // 214a  byte,  0=normal,  1=hide on trackpad,  2=hide on output,  3=hide both

            if shape_code <> 4    // not a target mark
            then begin

              x1 := (p1.x * 100 + re_org_x) * sx;
              y1 := (p1.y * 100 + re_org_y) * sy;

              x2 := (p2.x * 100 + re_org_x) * sx;
              y2 := (p2.y * 100 + re_org_y) * sy;

              move_to.X := Round(ex - gx + x1);
              move_to.Y := Round(by - gy + y1);

              line_to.X := Round(ex - gx + x2);   // gets changed later for a label..
              line_to.Y := Round(by - gy + y2);


              case shape_style of
                0: begin
                  Brush.Color := paper_colour;
                  Brush.Style := bsClear;
                  // transparent (not used for elipse). (also line shapes).
                end;
                1: begin
                  Brush.Color := timber_infill_colour;
                  // 0.93.a was paper_colour
                  Brush.Style := bsSolid;      // blank.
                end;
                2: begin
                  if shape_code = 0
                  then begin
                    draw_dotted_line(canv, move_to, line_to);
                    // bug workaround!!!
                    CONTINUE;
                  end
                  else begin
                    Brush.Color := Pen.Color;
                    Brush.Style := bsDiagCross;
                    // cross-hatched.
                  end;
                end;
              end;//case

              if shape_code = 3      // label rectangle..
              then begin
                line_to.X := move_to.X + TextWidth(' ' + shape_name + ' ') + 3;
                line_to.Y := move_to.Y + TextHeight(shape_name) + 6;
                // 0.93.a was +3
                Brush.Color := paper_colour;
                Brush.Style := bsSolid;
                // blank rectangle box for label.

                if (not
                  (((corner1_mod = 1) or (corner2_mod = 1) or (oneshape_shift_mod = 1) or
                  (oneshape_scale_mod = 1)) and
                  (i = bgnd_form.bgnd_shapes_listbox.ItemIndex))) and
                  (index < 0) then
                  Pen.Color := Font.Color;   // if only one, use highlight colour instead.
              end;

              if check_limits(move_to, line_to) = True
              then begin
                case shape_code of
                  -1: begin     // picture
                    try
                      raster_rect.Left := move_to.X;
                      raster_rect.Bottom := move_to.Y;
                      raster_rect.Right := line_to.X;
                      raster_rect.Top := line_to.Y;

                      if Tbgshape(
                        bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile =
                        True then begin

                        if Tbgshape(
                          bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.show_transparent =
                          True then
                          CopyMode := cmSrcAnd          // for transparent bitmaps in the metafile
                        else begin
                          Brush.Color := clWhite;
                          // blank it first for metafile overdraw
                          Brush.Style := bsSolid;
                          FillRect(raster_rect);
                          CopyMode := cmSrcCopy;
                          // normal for any bitmaps in the metafile
                        end;

                        if PlayEnhMetaFile(Handle,
                          Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_metafile.emf_HDC,
                          raster_rect) =
                          False // draw metafile on canvas
                        then begin
                          CopyMode := cmSrcCopy;
                          // normal for Canvas
                          Brush.Color := Pen.Color;
                          // metafile failed - draw hatched rectangle instead
                          Brush.Style := bsBDiagonal;
                          Rectangle(
                            move_to.X, move_to.Y, line_to.X, line_to.Y);
                        end;
                      end
                      else begin
                        pad_form.
                          bgnd_shape_image.Picture.Graphic :=
                          Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap;
                        if Tbgshape(
                          bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.show_transparent =
                          True  // 0.93.a moved into file
                        then
                          CopyMode := cmSrcAnd    // (destination Canvas) transparent if on white background.
                        else
                          CopyMode := cmSrcCopy;  // normal

                        StretchDraw(
                          raster_rect, pad_form.bgnd_shape_image.Picture.Graphic);
                        // needs TGraphic parameter to work reliably.

                        CopyMode := cmSrcCopy;
                        // reset normal for destination Canvas
                      end;

                      if
                      bgnd_form.picture_borders_checkbox.Checked = True
                      then begin
                        Brush.Color := paper_colour;
                        Brush.Style := bsClear;
                        Rectangle(
                          move_to.X, move_to.Y, line_to.X, line_to.Y);
                      end;

                    except
                      CopyMode := cmSrcCopy;
                      // reset normal for destination Canvas
                      Brush.Color := Pen.Color;
                      // stretch failed - draw hatched rectangle instead
                      Brush.Style := bsBDiagonal;
                      Rectangle(move_to.X,
                        move_to.Y, line_to.X, line_to.Y);
                    end;//try

                  end;//-1

                  0: begin
                    MoveTo(move_to.X, move_to.Y);
                    LineTo(line_to.X, line_to.Y);
                  end;
                  1, 3:
                    Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);
                  2: begin
                    if shape_style = 0 then
                      arc_ellipse(canv, move_to.X, move_to.Y, line_to.X, line_to.Y)   // transparent. Bug-fix.
                    else
                      Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);
                  end;
                end;//case

                if shape_code = 3 then
                  TextOut(move_to.X + 2, move_to.Y + 1, ' ' + shape_name);
                // 0.93.a was X+1  // insert label text in rectangle box.
              end;
            end
            else begin    // shape_code=4, draw a target mark

              arm := p2.x;        // cross arm length.
              diamond := arm / 2;   // size of centre diamond.

              x1 := ((p1.x - arm) * 100 + re_org_x) * sx;
              // p2.x is the arm length each way.
              y1 := (p1.y * 100 + re_org_y) * sy;         // horizontal line.

              x2 := ((p1.x + arm) * 100 + re_org_x) * sx;
              y2 := y1;

              move_to.X := Round(ex - gx + x1);
              move_to.Y := Round(by - gy + y1);

              line_to.X := Round(ex - gx + x2);
              line_to.Y := Round(by - gy + y2);


              if check_limits(move_to, line_to) = True
              then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);   // draw horizontal line.
              end;

              x1 := (p1.x * 100 + re_org_x) * sx;
              y1 := ((p1.y - arm) * 100 + re_org_y) * sy;

              x2 := x1;                                   // vertical line.
              y2 := ((p1.y + arm) * 100 + re_org_y) * sy;

              move_to.X := Round(ex - gx + x1);
              move_to.Y := Round(by - gy + y1);

              line_to.X := Round(ex - gx + x2);
              line_to.Y := Round(by - gy + y2);


              if check_limits(move_to, line_to) = True
              then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);   // draw vertical line.
              end;

              // now do 4 diamond lines...

              x1 := ((p1.x - diamond) * 100 + re_org_x) * sx;     // NW line.
              y1 := (p1.y * 100 + re_org_y) * sy;

              x2 := (p1.x * 100 + re_org_x) * sx;
              y2 := ((p1.y + diamond) * 100 + re_org_y) * sy;

              move_to.X := Round(ex - gx + x1);
              move_to.Y := Round(by - gy + y1);

              line_to.X := Round(ex - gx + x2);
              line_to.Y := Round(by - gy + y2);


              if check_limits(move_to, line_to) = True
              then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

              move_to := line_to;      // NE line.

              x2 := ((p1.x + diamond) * 100 + re_org_x) * sx;
              y2 := (p1.y * 100 + re_org_y) * sy;

              line_to.X := Round(ex - gx + x2);
              line_to.Y := Round(by - gy + y2);

              if check_limits(move_to, line_to) = True
              then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

              move_to := line_to;      // SE line.

              x2 := (p1.x * 100 + re_org_x) * sx;
              y2 := ((p1.y - diamond) * 100 + re_org_y) * sy;

              line_to.X := Round(ex - gx + x2);
              line_to.Y := Round(by - gy + y2);

              if check_limits(move_to, line_to) = True
              then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;

              move_to := line_to;      // SW line.

              x2 := ((p1.x - diamond) * 100 + re_org_x) * sx;
              y2 := (p1.y * 100 + re_org_y) * sy;

              line_to.X := Round(ex - gx + x2);
              line_to.Y := Round(by - gy + y2);

              if check_limits(move_to, line_to) = True
              then begin
                MoveTo(move_to.X, move_to.Y);
                LineTo(line_to.X, line_to.Y);
              end;
            end;

          end;//with shape

        except
          CONTINUE;      // ignore this shape if any calc errors.
        end;

      end;//for next i shape

      Pen.Width := 1;             // all other lines on pad.

    end;//with canvas
  end;//if draw shapes
end;
//__________________________________________________________________________________________

procedure draw_screengrid(canv: TCanvas);      // draw a screen grid if it's wanted.

var
  gridx, gridy: extended;
  m, grid_now: extended;
  gridco: integer;
  green_dot_x, green_dot_y, green_dot_r: extended;
  grid_str: string;
  scrub: integer;
  max_x: extended;
  line_endx: extended;

begin
  if screengrid_flag = True then begin

    case grid_labels_code_i of
      1:
        grid_str := ' feet ';     //  labels in feet.
      2:
        grid_str := ' inches ';   //  labels in inches.
      3:
        grid_str := ' proto-feet '; //  labels in prototype feet.
      4:
        grid_str := ' cm ';       //  labels in cm.
      6:
        grid_str := ' mm ';       //  labels in mm.
      else
        run_error(223);
    end;//case

    gridx := grid_spacex * fx;        // grid line spacings in pixels.
    gridy := grid_spacey * ABS(fy);

    with canv do begin

      Font.Assign(pad_form.Font);    // use the form font for the grid labels.

      Brush.Color := paper_colour;     // for grid labels;
      Brush.Style := bsSolid;

      Pen.Mode := pmCopy;           // use Pen.Color.
      Pen.Style := psSolid;         // solid grid margin lines.

      if paper_bunching = False then begin
        max_x := xmax;
        line_endx := xmax;
      end
      else begin
        max_x := xmax + bunch_jump_i;
        // need extra lines to fill the bunch.
        if bunch_shear_i = 0 then
          line_endx := max_x
        else
          line_endx := bunch_start;  // but no horizontal lines if there is a shear.

        Pen.Color := grid_colour;       // draw the bunching zone...
        Pen.Width := bunch_gap;
        MoveTo(Round((ex + xmax) / 2), 0);
        LineTo(Round((ex + xmax) / 2), pad_form.ClientHeight);
      end;

      Pen.Width := 1;
      scrub := 1;                  // factor for omitting vertical lines if too close

      while Round(screenx / (grid_spacex * scrub)) > (xmax / 25) do
        Inc(scrub);    // number of grid lines more than 1/25th of screen pixels ?.

      Pen.Color := Font.Color;   // zero line in font colour.

      move_to.X := Round(ex);
      move_to.Y := Round(by - gy);     // do bottom margin line before any others,
      line_to.X := Round(line_endx);
      line_to.Y := Round(by - gy);     // so that we don't rule through the labels.

      if (check_limit(False, False, move_to) = True) and
        (check_limit(False, False, line_to) = True) then begin
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);
      end;

      //  draw vertical grid lines...

      gridco := 0;       //  grid line count.
      m := ex;           //  start with zero line.
      repeat
        if gridco = 0 then
          Pen.Color := Font.Color   // zero line in margin colour.
        else
          Pen.Color := grid_colour;

        move_to.X := Round(m - gx);
        move_to.Y := 0;
        line_to.X := Round(m - gx);
        line_to.Y := Round(by);

        grid_now := 0;                  // keep compiler happy.

        if (paper_bunching = False) or (move_to.X < (bunch_start - bunch_gap)) or
          (move_to.X > (bunch_start + bunch_gap * 2 + bunch_jump_i))
        // no vertical lines anywhere near the bunch gap.
        then begin

          if (check_limit(True, False, move_to) = True) and
            (check_limit(True, False, line_to) = True) then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          case grid_labels_code_i of
            1:
              grid_now := grid_spacex * gridco / 304.8; //  labels in feet.
            2:
              grid_now := grid_spacex * gridco / 25.4;  //  labels in inches.
            3:
              grid_now := grid_spacex * gridco / scale; //  labels in prototype feet.
            4:
              grid_now := grid_spacex * gridco / 10;    //  labels in cm.
            6:
              grid_now := grid_spacex * gridco;       //  labels in mm.
            else
              run_error(222);
          end;//case

          move_to.X := Round(m - 6 - gx);
          move_to.Y := Round(by + 1);

          if check_limit(True, False, move_to) = True then
            TextOut(move_to.X, move_to.Y, FormatFloat('0.###', grid_now)); //  add bottom margin labels.
        end;

        Inc(gridco, scrub);
        m := m + gridx * scrub;

      until m > (max_x + gx);


      scrub := 1;      // factor for omitting horizontal lines if too close.

      while Round(screeny / (grid_spacey * scrub)) > (ymax / 25) do
        Inc(scrub);    // number of grid lines more than 1/25th of screen pixels ?.

      // and draw horizontal grid lines...

      Pen.Color := grid_colour;

      gridco := scrub;       // grid line count (bottom margin line already done).
      m := by - gridy * scrub;   // bottom margin line done,

      move_to.X := Round(ex / 8);
      move_to.Y := Round(by - gy) - 2 + Font.Height div 2;
      if check_limit(True, False, move_to) = True then
        TextOut(move_to.X, move_to.Y, '0');  //  but not the label for it.

      repeat
        move_to.X := Round(ex);
        move_to.Y := Round(m - gy);
        line_to.X := Round(line_endx);
        line_to.Y := Round(m - gy);

        if move_to.Y > Round(by) then
          line_to.X := Round(0 - gx);  // don't overwrite bottom labels.

        if (check_limit(False, False, move_to) = True) and
          (check_limit(False, False, line_to) = True) then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;

        case grid_labels_code_i of
          1:
            grid_now := grid_spacey * gridco / 304.8; //  labels in feet.
          2:
            grid_now := grid_spacey * gridco / 25.4;  //  labels in inches.
          3:
            grid_now := grid_spacey * gridco / scale; //  labels in prototype feet.
          4:
            grid_now := grid_spacey * gridco / 10;    //  labels in cm.
          6:
            grid_now := grid_spacey * gridco;       //  labels in mm.
          else
            run_error(222);
        end;//case

        move_to.X := Round(ex / 8);
        move_to.Y := Round(m - gy) - 2 + Font.Height div 2;
        if check_limit(False, False, move_to) = True then
          TextOut(move_to.X, move_to.Y, FormatFloat('0.###', grid_now));  //  add left margin labels.

        Inc(gridco, scrub);
        m := m - gridy * scrub;
      until m < gy;

      if zoom_offsety = 0 then
        TextOut(Round(ex / 8), Round(by) - 2 + Font.Height div 2, '0');   // add origin label.

      green_dot_r := 5; // 0.91.b was xmax/200;  // radius of green datum dot (pixels).
      if green_dot_r > Round(scale * fx) then
        green_dot_r := Round(scale * fx); // but not more than 12" scale,
      if green_dot_r < 3 then
        green_dot_r := 3;                             // or less than 3 pixels radius.

      green_dot_x := ex + (re_org_x * sx) - gx;    // re-org is in 1/100th mm.
      if green_dot_x < (ex - minfp) then begin
        green_dot_x := ex;
        // don't let datum dot disappear off-stage left.
        if check_dark_paper = False then
          Brush.Color := clBlack  // but make it black.
        else
          Brush.Color := clGray;
      end
      else
        Brush.Color := $0000C000;    // dot normally green (was red!).

      Brush.Style := bsSolid;              // solid dot.

      green_dot_y := by - gy + (y_datum * 100 + re_org_y) * sy;

      Pen.Color := paper_colour;              // draw green dot.

      move_to.X := Round(green_dot_x - green_dot_r);
      move_to.Y := Round(green_dot_y - green_dot_r);
      line_to.X := Round(green_dot_x + green_dot_r);
      line_to.Y := Round(green_dot_y + green_dot_r);

      if check_limits(move_to, line_to) = True then
        Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);


      Brush.Color := paper_colour;

      m := ex + ex / 4 - gx;    // units label X position.

      if m > (pad_form.ClientWidth - TextWidth(grid_str)) then
        m := pad_form.ClientWidth - TextWidth(grid_str);
      if m < 0 then
        m := 0;

      TextOut(Round(m), Round(by + 1), grid_str);     // grid dimension units label

    end;//with canvas

  end;//if screengrid

end;
//_______________________________________________________________________________________

procedure draw_reminders(canv: TCanvas);      // 216a

var
  memo: TMemo;
  n: integer;
  memo_count: integer;

  midx, midy: extended;

begin
  memo_count := 0;                      // init

  with pad_form do begin

    if (show_reminders_menu_entry.Checked = False) or (keeps_list.Count < 1) or
      (show_bgnd_keeps_menu_entry.Checked = False) then begin

      reminder_memo1.Visible := False;
      reminder_memo2.Visible := False;
      reminder_memo3.Visible := False;
      reminder_memo4.Visible := False;
      reminder_memo5.Visible := False;

      EXIT;
    end;

    for n := 0 to (keeps_list.Count - 1) do begin

      with Ttemplate(keeps_list.Objects[n]) do begin

        if bg_copied = False then
          CONTINUE;  // not a background template.

        if (group_selected = True) and (hide_group_templates_menu_entry.Checked = True) then
          CONTINUE;

        with template_info.keep_dims.box_dims1.align_info do begin

          if (reminder_str = '') or (reminder_flag = False) then
            CONTINUE;

          //  first get position of template mid-point on pad...

          midx := snap_peg_positions.ctrl_mid_pos.notch_x;
          midy := snap_peg_positions.ctrl_mid_pos.notch_y;

          move_to.X := Round(Round(midx * 100) * sx + ex - gx);
          move_to.Y := Round(Round(midy * 100) * sy + by - gy);

          //  draw coloured blob on template...

          if check_limit(True, True, move_to) = True then begin
            with canv do begin
              Pen.Mode := pmCopy;
              Pen.Style := psSolid;
              Pen.Color := reminder_colour;
              Pen.Width := 1;
              Brush.Style := bsSolid;
              Brush.Color := reminder_colour;

              Ellipse(move_to.X - 8, move_to.Y - 8, move_to.X + 8, move_to.Y + 8);
              // 8 dots radius arbitrary

              Pen.Color := clBlack;           // restore..
              Brush.Color := paper_colour;
            end;//with
          end;

          Inc(memo_count);

          if memo_count > 5 then
            CONTINUE;

          //  if one of first 5 reminders, also add callout ...

          case memo_count of

            1:
              memo := reminder_memo1;
            2:
              memo := reminder_memo2;
            3:
              memo := reminder_memo3;
            4:
              memo := reminder_memo4;
            else
              memo := reminder_memo5;

          end;//case

          memo.Tag := n;
          memo.Color := reminder_colour;
          memo.Lines.Text := reminder_str;
          memo.Hint := reminder_str + '  -  click to zoom template  -  right-click for options ';

          memo.Visible := True;

          // draw call-out line ...

          line_to.X := memo.Left + 2; //+memo.Width DIV 2;
          line_to.Y := memo.Top + memo.Height div 2;

          if check_limits(move_to, line_to) = True then begin
            with canv do begin
              Pen.Mode := pmCopy;
              Pen.Style := psSolid;
              Pen.Width := 3;
              Pen.Color := reminder_colour;

              MoveTo(move_to.X, move_to.Y);
              LineTo(line_to.X, line_to.Y);

              Pen.Width := 1;         // restore..
              Pen.Color := clBlack;
            end;//with
          end;

        end;//with align_info
      end;//with template

    end;//next template

    // turn off any not used...

    if memo_count < 5 then
      reminder_memo5.Visible := False;
    if memo_count < 4 then
      reminder_memo4.Visible := False;
    if memo_count < 3 then
      reminder_memo3.Visible := False;
    if memo_count < 2 then
      reminder_memo2.Visible := False;
    if memo_count < 1 then
      reminder_memo1.Visible := False;

  end;//with pad_form
end;
//______________________________________________________________________________

procedure draw_rings(canv: TCanvas; copies, dummy_vehicle: boolean);
//  draw the spacing-ring or copies.
//  might be on different canvases.
var
  i: integer;
  imin, imax: integer;

  pad_ringX1, pad_ringY1, pad_ringX2, pad_ringY2: integer;
  pad_ring_outerX1, pad_ring_outerY1, pad_ring_outerX2, pad_ring_outerY2: integer;

  sp_ring_x, sp_ring_y, sp_ring_dia, sp_ring_outer: extended;

  ring_dim, ringx, ringy: integer;

begin
  if show_spacing_rings = True then begin
    if copies = True then begin
      if ring_index < 1 then
        EXIT;   // no copies to draw
      imin := 1;
      imax := ring_index;            // current max number of copies.
    end
    else begin

      if (dummy_vehicle = False)                              // 206b...
        and (grid_form.attach_ring_checkbox.Checked = True) and
        (grid_form.show_dummy_vehicles_radio_button.Checked = True) then
        EXIT;   // will be drawn by the control template (with dummy_vehicle=True)

      imin := 0;        // the ring itself is the first entry in the array.
      imax := 0;
    end;

    for i := imin to imax do begin       // ring or copies

      sp_ring_x := rings[i, 0];
      sp_ring_y := rings[i, 1];

      sp_ring_dia := rings[i, 2];
      sp_ring_outer := rings[i, 3];

      // first 2 rings...

      pad_ringX1 := Round((sp_ring_x - sp_ring_dia / 2) * fx + ex - gx);
      pad_ringY1 := Round((sp_ring_y - sp_ring_dia / 2) * fy + by - gy);

      pad_ringX2 := Round((sp_ring_x + sp_ring_dia / 2) * fx + ex - gx);
      pad_ringY2 := Round((sp_ring_y + sp_ring_dia / 2) * fy + by - gy);

      pad_ring_outerX1 := Round((sp_ring_x - sp_ring_outer / 2) * fx + ex - gx);
      pad_ring_outerY1 := Round((sp_ring_y - sp_ring_outer / 2) * fy + by - gy);

      pad_ring_outerX2 := Round((sp_ring_x + sp_ring_outer / 2) * fx + ex - gx);
      pad_ring_outerY2 := Round((sp_ring_y + sp_ring_outer / 2) * fy + by - gy);

      with canv do begin
        Pen.Width := 1;
        Pen.Mode := pmCopy;
        Pen.Style := psSolid;

        if i = 0 then
          Pen.Color := ring_colour
        else
          Pen.Color := ring_copy_colour;

        move_to.X := pad_ring_outerX1;
        move_to.Y := pad_ring_outerY1;
        line_to.X := pad_ring_outerX2;
        line_to.Y := pad_ring_outerY2;

        if check_limits(move_to, line_to) = True then
          arc_ellipse(canv, move_to.X, move_to.Y, line_to.X, line_to.Y);

        move_to.X := pad_ringX1;
        move_to.Y := pad_ringY1;
        line_to.X := pad_ringX2;
        line_to.Y := pad_ringY2;

        if check_limits(move_to, line_to) = True then
          arc_ellipse(canv, move_to.X, move_to.Y, line_to.X, line_to.Y);

        // add cross-hairs...

        ring_dim := Round(inscale * 9 * fx);      // 9" each way.

        ringx := Round(sp_ring_x * fx + ex - gx);
        ringy := Round(sp_ring_y * fy + by - gy);

        move_to.X := ringx - ring_dim;
        move_to.Y := ringy;
        line_to.X := ringx + ring_dim;
        line_to.Y := ringy;

        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;

        move_to.X := ringx;
        move_to.Y := ringy - ring_dim;
        line_to.X := ringx;
        line_to.Y := ringy + ring_dim;

        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;
      end;//with canvas
    end;//for next copy ring i
  end;//if spacing ring
end;
//____________________________________________________________________________________________


function is_bgnd_in_rect(bgk, X_left, X_right, Y_top, Y_bottom: integer): boolean;   // 218d

  // return True if any part of a background template rails or lines is inside rectangle area
  // rectangle is in mm*100

  // used for testing if on screen, in export rectangle, etc.

var
  now_keep: Tbgnd_keep;

  aq: integer;
  nk, array_max: integer;
  xint, yint: integer;

begin
  Result := False;  // default init.

  with Ttemplate(keeps_list.Objects[bgk]) do begin

    if bg_copied = False then
      EXIT;  // not a background template.

    if (group_selected = True) and (pad_form.hide_group_templates_menu_entry.Checked = True) then
      EXIT;

    try
      now_keep := bgnd_keep;   // next background keep.

      with now_keep do begin

        for aq := 25 downto 0 do begin
          // save time by searching centre-lines first, ignore FB foot lines.

          if Length(list_bgnd_rails[aq]) = 0 then
            CONTINUE;                       // empty rail.

          array_max := High(list_bgnd_rails[aq]);
          for nk := 0 to array_max do begin

            xint := list_bgnd_rails[aq][nk].X;
            yint := list_bgnd_rails[aq][nk].Y;

            if (xint > X_left) and (xint < X_right) and (yint > Y_bottom) and
              (yint < Y_top) then begin
              Result := True;     // found in rect
              EXIT;
            end;

          end;//next nk
        end;//next aq

      end;//with now_keep
    except
      Result := False;
    end;//try
  end;//with template.
end;
//______________________________________________________________________________


procedure draw_background_templates(canv: TCanvas; group_code: integer;
  highlight_index: integer; highlight_on: boolean; highlight_colour: TColor);
//  all the background keeps.

// if highlight_index <> -1 draw this one only (highlighted in highlight_colour if highlight_on=True).

// group_code =0 draw all templates
//            =1 draw only selected group
//            =-1 draw all except selected group
var
  keeps_index: integer;  // 218d

  i, n: integer;
  bgk: integer;
  code: integer;

  numb_str, tbnum_str: string;

  xint, yint: integer;

  aq: integer;
  array_max: integer;

  peg_dim, bg_pegx, bg_pegy: integer;
  radcen_dim, radcenx, radceny: integer;

  no_name, number_yes, name_yes: boolean;
  id_yes: boolean; // 208a

  mod_name_x, mod_name_y: extended;
  max_label_height: integer;

  space_pos: integer;

  check_int1x, check_int1y, check_int2x, check_int2y: extended;
  check_int3x, check_int3y, check_int4x, check_int4y: extended;

  infill_points: array [0..3] of TPoint;

  this_ti: Ttemplate_info;

  keep_selected: boolean;
  using_marker_colour: boolean;
  marker_colour: integer;

  fixed_diamond_ends: boolean;

  dummy_i: integer;

  the_name_str: string;  // 208a

  X_left, X_right, Y_top, Y_bottom: integer;  // 218d

  last_bgnd_index: integer;  // 219a


  //////////////////////////////////////////////////////////

  procedure draw_bgnd_rail(pen_solid: boolean);
  // 0.93.a pen_solid added

  var
    nk: integer;
    save_line_to: TPoint;

  begin
    with now_keep do begin

      if Length(list_bgnd_rails[aq]) = 0 then
        EXIT;                       // empty rail.

      array_max := High(list_bgnd_rails[aq]);
      xint := list_bgnd_rails[aq][0].X;
      yint := list_bgnd_rails[aq][0].Y;

      move_to.X := Round(ex - gx + xint * sx);
      move_to.Y := Round(by - gy + yint * sy);

      if shift_keeps_mod = 1 then
        shift_keep_moveto(bgk, canv);  // mouse action shift keeps, sets pen colour if selected.

      if twist_keeps_mod = 1 then
        twist_keep_moveto(bgk, canv);  // mouse action twist keeps.

      with canv do begin

        if pen_solid = True then
          Pen.Style := psSolid
        else
          Pen.Style := psDot; // 0.93.a show dotted on screen if platform edges hidden on output.

        for nk := 1 to array_max do begin

          xint := list_bgnd_rails[aq][nk].X;
          yint := list_bgnd_rails[aq][nk].Y;

          line_to.X := Round(ex - gx + xint * sx);
          line_to.Y := Round(by - gy + yint * sy);
          if shift_keeps_mod = 1 then
            shift_keep_lineto(bgk);  // mouse action shift keeps.
          if twist_keeps_mod = 1 then
            twist_keep_lineto(bgk);  // mouse action twist keeps.

          save_line_to := line_to;
          // in case check_limits modifies it (e.g paper bunching)


          if check_limits(move_to, line_to) = True then begin
            MoveTo(move_to.X, move_to.Y);
            LineTo(line_to.X, line_to.Y);
          end;

          move_to := save_line_to;

        end;//next nk
      end;//with
    end;//with
  end;
  ////////////////////////////////////////////////////////////

begin
  if (keeps_list.Count < 1) or (pad_form.show_bgnd_keeps_menu_entry.Checked = False) then
    EXIT;

  last_bgnd_index := highest_bgnd_template;  // get last bgnd template in list  219a

  // 218d set limit rectangle ...

  if (pad_form.show_bgnd_keeps_in_rect_menu_entry.Checked = True)
    { OT-FIRST and (file_viewer_form.Showing=False)} and (output_rectangle_width > 0) and
    (output_rectangle_height > 0) then begin
    draw_export_rectangle_flag := True;

    X_left := Round(MIN(output_rectangle_x1, output_rectangle_x2) * 100);
    // export rectangle  ...
    X_right := Round(MAX(output_rectangle_x1, output_rectangle_x2) * 100);

    Y_bottom := Round(MIN(output_rectangle_y1, output_rectangle_y2) * 100);
    Y_top := Round(MAX(output_rectangle_y1, output_rectangle_y2) * 100);
  end
  else begin
    X_left := Round((zoom_offsetx - g * 2) * 100);
    // screen area instead ...       g over-limits arbitrary
    X_right := Round((zoom_offsetx + screenx + g) * 100);

    Y_bottom := Round((zoom_offsety - g * 2) * 100);
    Y_top := Round((zoom_offsety + screeny + g) * 100);
  end;

  for keeps_index := 0 to (keeps_list.Count - 1) do begin
    Ttemplate(keeps_list.Objects[keeps_index]).bgnd_is_in_rect :=
      is_bgnd_in_rect(keeps_index, X_left, X_right, Y_top, Y_bottom);   // temp flag
  end;//next


  with canv do begin
    Pen.Width := 1;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;

    Brush.Color := paper_colour;  // gaps in dotted lines.
    Brush.Style := bsSolid;

    TextOut(0, 0, '');  // needed for dotted lines - Delphi bug?

    // first do timbering and all marks ...

    for bgk := 0 to (keeps_list.Count - 1) do begin

      if (highlight_index <> -1) and (bgk <> highlight_index) then
        CONTINUE;  // don't need to draw any others if one is for highlighting.

      with Ttemplate(keeps_list.Objects[bgk]) do begin

        if bgnd_is_in_rect = False then
          CONTINUE;     // 218d no part of template in screen area

        if bg_copied = False then
          CONTINUE;  // not a background template.

        if (group_selected = True) and (pad_form.hide_group_templates_menu_entry.Checked = True) then
          CONTINUE;  // 209c

        keep_selected := group_selected;

        // first run through all the marks (mod 0.76.a 24-10-01)...

        try
          now_keep := bgnd_keep;   // next background keep.

          with now_keep do begin

            tbnum_str := timber_numbers_string;      // the full string of timber numbering.


            if (keep_selected = True) and (group_code = -1) then
              CONTINUE;    // group templates not wanted.
            if (keep_selected = False) and (group_code = 1) then
              CONTINUE;    // only group templates wanted.

            // marker_colours_pad: 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the print mapping colour instead.

            using_marker_colour := False;  // default init.

            with template_info.keep_dims.box_dims1 do begin

              case marker_colours_pad of

                2, 3:
                  if use_pad_marker_colour = True then begin
                    marker_colour := pad_marker_colour;
                    using_marker_colour := True;
                  end;

                4:
                  if use_print_mapping_colour = True then begin
                    marker_colour := print_mapping_colour;
                    using_marker_colour := True;
                  end;
              end;//case

            end;//with

            // first draw bgnd marks and timbers ...

            array_max := High(list_bgnd_marks);

            for i := 0 to array_max do begin

              try

                code := list_bgnd_marks[i].code;   // check this mark wanted.

                with bgkeeps_form do begin

                  case code of
                    -5:
                      Pen.Color := clBlack;    // label position - colour ignored.

                    -4, 0, 501..508, 600..605, 700..703:
                      CONTINUE;    // ignore timber selector mark, blank lines. // 0.94.a ignore check-rail labels
                    // 206b  600..605, 700..703 ignore long marks and switch/xing labels on trackpad

                    -3, -2:
                      if marks_checkbox.Checked = True then begin
                        if using_marker_colour = False then
                          Pen.Color := bgkeep_mark_colour  // curving rad centres.
                        else
                          Pen.Color := marker_colour;
                      end
                      else
                        CONTINUE;
                    -1, 8, 9:
                      if peg_checkbox.Checked = True then
                        Pen.Color := bgkeep_peg_colour     // fixing peg, peg arms.
                      else
                        CONTINUE;

                    1, 2, 7, 10:
                      if marks_checkbox.Checked = True then begin
                        if using_marker_colour = False then
                          Pen.Color := bgkeep_mark_colour     // guide marks, radial ends, transition ends.
                        else
                          Pen.Color := marker_colour;
                      end
                      else
                        CONTINUE;

                    3, 33, 93:
                      if timber_outlines_checkbox.Checked = True then begin
                        if using_marker_colour = False then
                          Pen.Color := bgkeep_timber_colour  // timber outlines.
                        else
                          Pen.Color := marker_colour;
                      end
                      else
                        CONTINUE;

                    4, 14, 44, 54:
                      if timber_centres_checkbox.Checked = True then begin
                        if using_marker_colour = False then
                          Pen.Color := bgkeep_timber_colour  // timber centre-lines only on pad.
                        else
                          Pen.Color := marker_colour;
                      end
                      else
                        CONTINUE;

                    5, 55, 95:
                      if reduced_ends_checkbox.Checked = True  // timber reduced ends.
                      then begin
                        if using_marker_colour = False then
                          Pen.Color := bgkeep_timber_colour
                        else
                          Pen.Color := marker_colour;
                      end
                      else
                        CONTINUE;

                    6:
                      if joints_checkbox.Checked = True then begin
                        if using_marker_colour = False then
                          Pen.Color := bgkeep_mark_colour    // rail joints.
                        else
                          Pen.Color := marker_colour;
                      end
                      else
                        CONTINUE;

                    99:
                      if timber_numbering_checkbox.Checked = False then
                        CONTINUE;  // text.

                    203, 233, 293:
                      if (timber_infill_checkbox.Checked = True) and ((screenx < 200 * scale) or (bgpad_timb_infill_style > 2))
                      // infill on pad if solid/blank fill or large enough to see hatching.
                      then
                        Pen.Color := paper_colour //use paper colour to avoid line thickening caused by rounding, when the outline overwrites.
                      //was bgkeep_timber_colour  // timber infill.
                      else
                        CONTINUE;

                    else
                      Pen.Color := clBlack;
                  end;//case
                end;//with

                p1 := list_bgnd_marks[i].p1;    // x1,y1 in  1/100ths mm

                check_int1x := limits(h_minint, h_maxint, p1.X * sx + ex - gx, dummy_i);
                // h_min, h_max 31 bit to give room for some arithmetic on the data (shift keeps, etc.)
                check_int1y := limits(h_minint, h_maxint, p1.Y * sy + by - gy, dummy_i);

                if code = 99    // code=99, timber numbering.
                then begin
                  move_to.X := Round(check_int1x);
                  move_to.Y := Round(check_int1y);

                  numb_str := extract_tbnumber_str(tbnum_str);
                  // get next timber numbering string from the acummulated string.
                  if numb_str = '' then
                    CONTINUE;                // no string available??

                  if check_limit(True, True, move_to) = True then begin
                    Font.Assign(pad_form.pad_timber_font_label.Font);
                    Brush.Color := paper_colour;
                    Brush.Style := bsSolid;

                    TextOut(move_to.X - (TextWidth(numb_str) div 2), move_to.Y -
                      (TextHeight(numb_str) div 2), numb_str);
                    Font.Assign(pad_form.Font);
                  end;
                  CONTINUE;
                end;

                p2 := list_bgnd_marks[i].p2;    // x2,y2 in  1/100ths mm

                check_int2x := limits(h_minint, h_maxint, p2.X * sx + ex - gx, dummy_i);
                check_int2y := limits(h_minint, h_maxint, p2.Y * sy + by - gy, dummy_i);

                if ((code = 203) or (code = 233) or (code = 293)) and (i < array_max)    // timber infill
                then begin
                  p3 := list_bgnd_marks[i + 1].p1;  // x3,y3 in  1/100ths mm
                  p4 := list_bgnd_marks[i + 1].p2;  // x4,y4 in  1/100ths mm

                  check_int3x := limits(h_minint, h_maxint, p3.X * sx + ex - gx, dummy_i);
                  check_int3y := limits(h_minint, h_maxint, p3.Y * sy + by - gy, dummy_i);

                  check_int4x := limits(h_minint, h_maxint, p4.X * sx + ex - gx, dummy_i);
                  check_int4y := limits(h_minint, h_maxint, p4.Y * sy + by - gy, dummy_i);
                end
                else begin       // keep compiler happy...
                  p3.X := 0;
                  p3.Y := 0;

                  p4.X := 0;
                  p4.Y := 0;

                  check_int3x := 0;
                  check_int3y := 0;

                  check_int4x := 0;
                  check_int4y := 0;
                end;

                if (keep_selected = True) and (pad_form.show_group_menu_entry.Checked = True) then
                  Pen.Color := selection_colour; // draw selected group in this colour.

                if (bgk = highlight_index) and (highlight_on = True) then
                  Pen.Color := highlight_colour;    // this one to be highlighted.

                if code > 0 then begin
                  move_to.X := Round(check_int1x);
                  move_to.Y := Round(check_int1y);
                  line_to.X := Round(check_int2x);
                  line_to.Y := Round(check_int2y);

                  infill3.X := Round(check_int3x);
                  infill3.Y := Round(check_int3y);
                  infill4.X := Round(check_int4x);
                  infill4.Y := Round(check_int4y);

                  if shift_keeps_mod = 1 then begin
                    shift_keep_moveto(bgk, canv);
                    // mouse action shift keeps, sets pen colour if selected.
                    shift_keep_lineto(bgk);
                  end;

                  if twist_keeps_mod = 1 then begin
                    twist_keep_moveto(bgk, canv);  // mouse action twist keeps.
                    twist_keep_lineto(bgk);
                  end;

                  if (code = 203) or (code = 233) or (code = 293)   // timber infill...
                  then begin
                    infill_points[0] := move_to;
                    infill_points[1] := line_to;
                    infill_points[2] := infill3;
                    infill_points[3] := infill4;

                    if (check_limits(infill_points[0], infill_points[1]) = True) and
                      (check_limits(infill_points[2], infill_points[3]) = True) then
                    begin
                      Pen.Color := paper_colour;
                      Brush.Color := bgkeep_timberfill_colour;

                      case bgpad_timb_infill_style of
                        0:
                          CONTINUE;
                        1:
                          Brush.Style := bsBDiagonal;     // hatched. Backward diagonal for background templates.
                        2:
                          Brush.Style := bsDiagCross;
                        3:
                          Brush.Style := bsSolid;
                        4: begin
                          // blank.
                          Brush.Style := bsSolid;
                          Brush.Color := paper_colour;
                          // overide.
                        end;
                        else
                          CONTINUE;
                      end;//case

                      Polygon(infill_points);
                    end;
                  end
                  else begin
                    if (bgk = last_bgnd_index) and
                      (Pen.Color = bgkeep_timber_colour) and (bgkeeps_form.bold_timber_outlines_checkbox.Checked = True)
                    // 219a
                    then
                      Pen.Width := 3
                    else
                      Pen.Width := 1;

                    if check_limits(move_to, line_to) = True then begin
                      MoveTo(move_to.X, move_to.Y);
                      LineTo(line_to.X, line_to.Y);
                    end;
                  end;
                end
                else begin
                  Pen.Width := 1;  // 219a

                  if code = -1              // code -1, draw bgnd fixing peg...
                  then begin
                    peg_dim := 10;
                    // 0.91.b was Screen.Width div 100; // 100 arbitrary.
                    if peg_dim > Round(scale * fx) then
                      peg_dim := Round(scale * fx); // but not more than 2ft scale.

                    bg_pegx := Round(check_int1x);
                    bg_pegy := Round(check_int1y);

                    move_to.X := bg_pegx - peg_dim;
                    move_to.Y := bg_pegy - peg_dim;
                    line_to.X := bg_pegx + peg_dim;
                    line_to.Y := bg_pegy + peg_dim;

                    if shift_keeps_mod = 1 then
                    begin
                      shift_keep_moveto(bgk, canv);
                      // mouse action shift keeps, sets pen colour if selected.
                      shift_keep_lineto(bgk);
                    end;

                    if twist_keeps_mod = 1 then
                    begin
                      twist_keep_moveto(bgk, canv);
                      // mouse action twist keeps.
                      twist_keep_lineto(bgk);
                    end;

                    if check_limits(move_to, line_to) = True then
                      arc_ellipse(canv, move_to.X, move_to.Y, line_to.X, line_to.Y);

                    move_to.X := bg_pegx - peg_dim * 2;
                    move_to.Y := bg_pegy;
                    line_to.X := bg_pegx + peg_dim * 2;
                    line_to.Y := bg_pegy;

                    if shift_keeps_mod = 1 then
                    begin
                      shift_keep_moveto(bgk, canv);
                      // mouse action shift keeps, sets pen colour if selected.
                      shift_keep_lineto(bgk);
                    end;

                    if twist_keeps_mod = 1 then
                    begin
                      twist_keep_moveto(bgk, canv);
                      // mouse action twist keeps.
                      twist_keep_lineto(bgk);
                    end;

                    if check_limits(move_to, line_to) = True then begin
                      MoveTo(move_to.X, move_to.Y);
                      LineTo(line_to.X, line_to.Y);
                    end;

                    move_to.X := bg_pegx;
                    move_to.Y := bg_pegy - peg_dim * 2;
                    line_to.X := bg_pegx;
                    line_to.Y := bg_pegy + peg_dim * 2;

                    if shift_keeps_mod = 1 then
                    begin
                      shift_keep_moveto(bgk, canv);
                      // mouse action shift keeps, sets pen colour if selected.
                      shift_keep_lineto(bgk);
                    end;

                    if twist_keeps_mod = 1 then
                    begin
                      twist_keep_moveto(bgk, canv);
                      // mouse action twist keeps.
                      twist_keep_lineto(bgk);
                    end;

                    if check_limits(move_to, line_to) = True then begin
                      MoveTo(move_to.X, move_to.Y);
                      LineTo(line_to.X, line_to.Y);
                    end;
                  end;

                  if (code = -2) or (code = -3)              // draw bgnd curving rad centres...
                  then begin
                    radcen_dim := 4;
                    // 0.91.b was Screen.Width div 250; // 250 arbitrary.  (smaller than for the current).
                    if radcen_dim > Round(scale * 3 * fx) then
                      radcen_dim := Round(scale * 3 * fx); // but not more than 3ft scale.

                    radcenx := Round(check_int1x);
                    radceny := Round(check_int1y);

                    move_to.X := radcenx - radcen_dim * 2;
                    move_to.Y := radceny;
                    line_to.X := radcenx + radcen_dim * 2;
                    line_to.Y := radceny;

                    if shift_keeps_mod = 1 then
                    begin
                      shift_keep_moveto(bgk, canv);
                      // mouse action shift keeps, sets pen colour if selected.
                      shift_keep_lineto(bgk);
                    end;

                    if twist_keeps_mod = 1 then
                    begin
                      twist_keep_moveto(bgk, canv);
                      // mouse action twist keeps.
                      twist_keep_lineto(bgk);
                    end;

                    if check_limits(move_to, line_to) = True then begin
                      MoveTo(move_to.X, move_to.Y);
                      LineTo(line_to.X, line_to.Y);
                    end;

                    move_to.X := radcenx;
                    move_to.Y := radceny - radcen_dim * 2;
                    line_to.X := radcenx;
                    line_to.Y := radceny + radcen_dim * 2;

                    if shift_keeps_mod = 1 then
                    begin
                      shift_keep_moveto(bgk, canv);
                      // mouse action shift keeps, sets pen colour if selected.
                      shift_keep_lineto(bgk);
                    end;

                    if twist_keeps_mod = 1 then
                    begin
                      twist_keep_moveto(bgk, canv);
                      // mouse action twist keeps.
                      twist_keep_lineto(bgk);
                    end;

                    if check_limits(move_to, line_to) = True then begin
                      MoveTo(move_to.X, move_to.Y);
                      LineTo(line_to.X, line_to.Y);
                    end;
                  end;

                  if code = -5        // save the label position - actual draw later.
                  then begin
                    move_to.X := Round(check_int1x);
                    move_to.Y := Round(check_int1y);

                    if shift_keeps_mod = 1 then
                      shift_keep_moveto(bgk, canv);  // mouse action shift keeps, sets pen colour if selected.

                    if twist_keeps_mod = 1 then
                      twist_keep_moveto(bgk, canv);  // mouse action twist keeps.

                    with template_info.keep_dims.box_dims1 do begin
                      mod_name_x := mod_text_x;
                      mod_name_y := mod_text_y;
                      //name_string:=Trim(reference_string);       // name part of label into now_keep.
                    end;//with

                    move_to.X := move_to.X + Round(mod_name_x * fx);
                    move_to.Y := move_to.Y + Round(mod_name_y * fy);

                    if check_limit(True, True, move_to) = True
                    then begin
                      text_begin_X := move_to.X;
                      // save position for the name label.
                      text_begin_Y := move_to.Y;
                    end
                    else begin
                      text_begin_X := min_draw_int;
                      text_begin_Y := min_draw_int;
                    end;
                  end;//code -5
                end;//code <0

              except
                CONTINUE;      // ignore this mark if calc exception.
              end;// try

            end;//next i background mark

          end;//with now_keep data.

          bgnd_keep := now_keep;   // save the label location.

        except
          CONTINUE;      // ignore this keep if calc exception.
        end;// try
      end;//with template.
    end;//next bgk template.

    // now draw all the rails... (mod 0.76.a 24-10-01).

    for bgk := 0 to (keeps_list.Count - 1) do begin

      if (highlight_index <> -1) and (bgk <> highlight_index) then
        CONTINUE;  // don't need to draw any others if one is for highlighting.

      with Ttemplate(keeps_list.Objects[bgk]) do begin

        if bgnd_is_in_rect = False then
          CONTINUE;     // 218d no part of template in screen area

        if bg_copied = False then
          CONTINUE;  // not a background template.

        if (group_selected = True) and (pad_form.hide_group_templates_menu_entry.Checked = True) then
          CONTINUE;  // 209c

        keep_selected := group_selected;

        try

          now_keep := bgnd_keep;

          with now_keep do begin

            if (keep_selected = True) and (group_code = -1) then
              CONTINUE;    // group templates not wanted.
            if (keep_selected = False) and (group_code = 1) then
              CONTINUE;    // only group templates wanted.

            // marker_colours_pad: 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the print mapping colour instead.

            using_marker_colour := False;  // default init.

            with template_info.keep_dims.box_dims1 do begin

              case marker_colours_pad of

                1, 3:
                  if use_pad_marker_colour = True then begin
                    marker_colour := pad_marker_colour;
                    using_marker_colour := True;
                  end;

                4:
                  if use_print_mapping_colour = True then begin
                    marker_colour := print_mapping_colour;
                    using_marker_colour := True;
                  end;
              end;//case

            end;//with

            with template_info.keep_dims.turnout_info2 do
              fixed_diamond_ends := (semi_diamond_flag = True) and (diamond_fixed_flag = True);
            // need end marks on fixed diamond point rails.

            //  first draw bgnd centre-lines...

            if bgkeeps_form.centres_checkbox.Checked = True then begin
              if (using_marker_colour = False) or (marker_colours_pad < 3)
              then begin
                // 212a  dummy template = centre-lines as background shapes...

                if
                template_info.keep_dims.box_dims1.align_info.dummy_template_flag =
                  True then
                  Pen.Color := shapes_colour
                else
                  Pen.Color := bgkeep_mark_colour;
              end
              else
                Pen.Color := marker_colour;

              if (keep_selected = True) and (pad_form.show_group_menu_entry.Checked = True)
              // he's clicking selections.
              then
                Pen.Color := selection_colour;
              // draw selected keeps in this colour.

              if (bgk = highlight_index) and (highlight_on = True) then
                Pen.Color := highlight_colour;               // this one to be highlighted.

              // 212a ...

              if template_info.keep_dims.box_dims1.align_info.dummy_template_flag =
                True then begin
                Pen.Width := 1;
                if bgnd_form.pad_shapes_linewidth_2_radiobutton.Checked = True then
                  Pen.Width := 2;
                if bgnd_form.pad_shapes_linewidth_3_radiobutton.Checked = True then
                  Pen.Width := 3;
              end;

              for aq := 24 to 25 do
                draw_bgnd_rail(True);

              Pen.Width := 1;  // reset if necessary  212a

            end;

            // and rails...

            if using_marker_colour = False then
              Pen.Color := bgkeep_rail_colour
            else
              Pen.Color := marker_colour;

            if (keep_selected = True) and (pad_form.show_group_menu_entry.Checked = True)
            //and  (click_bgnd_select=True)  // he's clicking selections.
            then
              Pen.Color := selection_colour;
            // draw selected keeps in this colour.

            if (bgk = highlight_index) and (highlight_on = True) then
              Pen.Color := highlight_colour;  // this one to be highlighted.

            // new code 0.93.a ...

            with template_info.keep_dims.box_dims1 do begin

              if bgkeeps_form.gauge_faces_checkbox.Checked = True then begin
                for aq := 0 to 7 do begin
                  // main rails gauge faces
                  if (turnout_info1.plain_track_flag = False) or (aq = 0) or
                    (aq = 3) // stock rails only, if plain track
                  then
                    draw_bgnd_rail(True);
                end;//next aq

                if fixed_diamond_ends = True then begin
                  aq := 26;     // K-crossing check rails, gauge-faces.
                  repeat
                    draw_bgnd_rail(True);
                    Inc(aq, 2);
                  until aq > 28;
                end;
              end;//gauge faces

              if bgkeeps_form.outer_edges_checkbox.Checked = True then begin
                for aq := 8 to 15 do begin                     // main rails outer edges
                  if (turnout_info1.plain_track_flag = False) or (aq = 8) or
                    (aq = 11) then
                    draw_bgnd_rail(True);
                end;//next aq

                if fixed_diamond_ends = True then begin
                  aq := 27;     // K-crossing check rails, outer-edges.
                  repeat
                    draw_bgnd_rail(True);
                    Inc(aq, 2);
                  until aq > 29;
                end;
              end;//outer edges

              // next, draw in the rail ends...

              if (bgkeeps_form.gauge_faces_checkbox.Checked = True) and
                (bgkeeps_form.outer_edges_checkbox.Checked = True) then begin
                mark_end(bgk, canv, 1, 1, 9, 1, True);    // main rail wing rail finish.
                mark_end(bgk, canv, 2, 1, 10, 1, True);   // turnout rail wing rail finish.

                mark_end(bgk, canv, 6, 0, 14, 0, True);   // main side check rail start.
                mark_end(bgk, canv, 6, 1, 14, 1, True);   // main side check rail finish.

                mark_end(bgk, canv, 7, 0, 15, 0, True);   // turnout side check rail start.
                mark_end(bgk, canv, 7, 1, 15, 1, True);   // turnout side check rail finish.

                mark_end(bgk, canv, 4, 0, 5, 0, True);    // blunt nose.

                if fixed_diamond_ends = True then begin
                  mark_end(bgk, canv, 1, 0, 9, 0, True);
                  // planed faced of point rails for a fixed-diamond.
                  mark_end(bgk, canv, 2, 0, 10, 0, True);

                  mark_end(bgk, canv, 26, 1, 27, 1, True);
                  // MS K-crossing check rails.
                  mark_end(bgk, canv, 28, 1, 29, 1, True);
                  // DS K-crossing check rails.
                end;

              end;//if rail ends


              with platform_trackbed_info do begin

                if (adjacent_edges_keep = False) and (bgkeeps_form.gauge_faces_checkbox.Checked = True)
                then begin
                  aq := 16;
                  repeat             // adjacent tracks gauge faces.
                    draw_bgnd_rail(True);
                    Inc(aq, 2);
                  until aq > 22;
                end;

                if (adjacent_edges_keep = False) and (bgkeeps_form.outer_edges_checkbox.Checked = True)
                then begin
                  aq := 17;
                  repeat             // adjacent tracks outer edges.
                    draw_bgnd_rail(True);
                    Inc(aq, 2);
                  until aq > 23;
                end;

                if (adjacent_edges_keep = True) and (bgkeeps_form.platforms_checkbox.Checked = True)
                // 0.93.a platform edges
                then begin

                  Brush.Color := paper_colour;  // gaps in dotted lines.
                  Brush.Style := bsSolid;
                  TextOut(0, 0, '');            // needed for dotted lines - Delphi bug?

                  if (using_marker_colour = False) or (marker_colours_pad < 3)
                  then
                    Pen.Color := bgkeep_platform_colour                // 0.93.a
                  else
                    Pen.Color := marker_colour;

                  if (keep_selected = True) and (pad_form.show_group_menu_entry.Checked = True)
                  //and  (click_bgnd_select=True)  // he's clicking selections.
                  then
                    Pen.Color := selection_colour;
                  // draw selected keeps in this colour.

                  if (bgk = highlight_index) and (highlight_on = True) then
                    Pen.Color := highlight_colour;               // this one to be highlighted.

                  aq := 16;             // TS platform rear edge
                  draw_bgnd_rail(draw_ts_platform_rear_edge_keep);
                  // draw solid or dotted

                  aq := 17;             // TS platform front edge
                  draw_bgnd_rail(True);

                  aq := 20;             // MS platform rear edge
                  draw_bgnd_rail(draw_ms_platform_rear_edge_keep);

                  aq := 21;             // MS platform front edge
                  draw_bgnd_rail(True);
                end;

                if (adjacent_edges_keep = True) and
                  (bgkeeps_form.trackbed_edges_checkbox.Checked = True)   // trackbed edges
                then begin
                  aq := 18;
                  draw_bgnd_rail(False);  // dotted lines on screen
                  aq := 19;
                  draw_bgnd_rail(False);
                  aq := 22;
                  draw_bgnd_rail(False);
                  aq := 23;
                  draw_bgnd_rail(False);
                end;

              end;//with platform_trackbed_info
            end;//with box_dims1

            with template_info.keep_dims.box_dims1.platform_trackbed_info do begin    // 0.93.a ...

              if (bgkeeps_form.platforms_checkbox.Checked = True) and
                (adjacent_edges_keep = True)  // platforms wanted?
              then begin
                // 0.93.a draw platform ends ...

                if draw_ts_platform_keep = True then begin
                  mark_end(bgk, canv, 16, 0, 17, 0, draw_ts_platform_start_edge_keep);
                  mark_end(bgk, canv, 16, 1, 17, 1, draw_ts_platform_end_edge_keep);
                end;

                if draw_ms_platform_keep = True then begin
                  mark_end(bgk, canv, 20, 0, 21, 0, draw_ms_platform_start_edge_keep);
                  mark_end(bgk, canv, 20, 1, 21, 1, draw_ms_platform_end_edge_keep);
                end;
              end;
            end;//with

          end;//with  now_keep

        except
          CONTINUE;      // ignore this keep if calc exception (zoom in on v. large radius?).
        end;// try
      end;//with template.
    end;//next bgk template.

    // finally, add the number and name labels for all the bgnd templates...
    // (doing it this way ensures that no labels are covered by subsequent templates.)

    number_yes := (bgkeeps_form.template_number_checkbox.Checked) and (hide_name_labels = False);
    // 0.82.b
    name_yes := (bgkeeps_form.template_name_checkbox.Checked) and (hide_name_labels = False);
    // 0.82.b
    id_yes := (bgkeeps_form.template_id_checkbox.Checked) and (hide_name_labels = False);
    // 208a

    for bgk := 0 to (keeps_list.Count - 1) do begin

      if (highlight_index <> -1) and (bgk <> highlight_index) then
        CONTINUE;  // don't need to draw any others if one is for highlighting.

      with Ttemplate(keeps_list.Objects[bgk]) do begin

        if bgnd_is_in_rect = False then
          CONTINUE;     // 218d no part of template in screen area

        if bg_copied = False then
          CONTINUE;  // not a background template.

        if (group_selected = True) and (pad_form.hide_group_templates_menu_entry.Checked = True) then
          CONTINUE;  // 209c

        keep_selected := group_selected;

        try

          now_keep := bgnd_keep;

          with now_keep do begin

            if (keep_selected = True) and (group_code = -1) then
              CONTINUE;    // group templates not wanted.
            if (keep_selected = False) and (group_code = 1) then
              CONTINUE;    // only group templates wanted.

            Font.Assign(pad_form.bgnd_keeps_font_label.Font);   // defaults.
            Brush.Color := paper_colour;

            if keep_selected = True        // but this one is selected.
            then begin
              Brush.Style := bsSolid;
              if paper_colour = clBlack then begin
                Brush.Color := clWhite;
                Font.Color := clBlack;
              end
              else begin
                Brush.Color := clBlack;
                Font.Color := clWhite;
              end;
            end
            else begin
              if pad_form.transparent_names_menu_entry.Checked = True then
                Brush.Style := bsClear   // this one not selected - normal label.
              else
                Brush.Style := bsSolid;
            end;


            the_name_str := Trim(template_info.keep_dims.box_dims1.reference_string);
            // 208a

            if (the_name_str <> '') and (id_yes = True) then
              the_name_str := the_name_str + '-';      // 208a

            full_label_string := ' ' + IntToStr(bgk + 1) + ': ' + the_name_str +
              template_info.keep_dims.box_dims1.id_number_str + ' ';    // 208a ID added

            requested_label_string := '';

            if (number_yes = True) and (name_yes = True) and (id_yes = True) then
              requested_label_string := full_label_string
            else begin
              if id_yes = True then begin
                if (number_yes = True) and (name_yes = False) then
                  requested_label_string := ' ' + IntToStr(bgk + 1) + ': ' +
                    template_info.keep_dims.box_dims1.id_number_str + ' ';
                // 208a ID added
                if (number_yes = False) and (name_yes = True) then
                  requested_label_string := ' ' + the_name_str + template_info.keep_dims.box_dims1.id_number_str + ' ';
                // 208a ID added
                if (number_yes = False) and (name_yes = False) then
                  requested_label_string := ' ' + template_info.keep_dims.box_dims1.id_number_str + ' ';
                // 208a ID added
              end
              else begin
                if (number_yes = True) and (name_yes = False) then
                  requested_label_string := ' ' + IntToStr(bgk + 1) + ' ';
                // 208a ID added
                if (number_yes = False) and (name_yes = True) then
                  requested_label_string := ' ' + the_name_str + ' ';
                // 208a ID added
                if (number_yes = False) and (name_yes = False) then
                  requested_label_string := '';
                // 208a ID added
                if (number_yes = True) and (name_yes = True) then
                  requested_label_string := ' ' + IntToStr(bgk + 1) + ': ' + the_name_str + ' ';
                // 208a ID added
              end;
            end;


            max_label_height := Round(ABS(20 * scale * fx));
            // arbitrary - scale labels to 20ft high max.

            if pad_form.names_scaled_menu_entry.Checked = True then
              repeat
                if ABS(TextHeight('A')) < max_label_height then
                  BREAK
                // n.b. scale is per current pad, not necessarily the same as the bgnd template.
                else
                  Font.Size := Font.Size - 1;
              until Font.Size <= 1;                                // 1pt minimum font size.

            if (check_limit(True, True, move_to) = True) and
              ((Font.Size > 2) or (click_bgnd_select = True) or (shift_click = True) or
              (keep_selected = True) or ((GetKeyState(VK_CAPITAL) and 1) <> 0)) then begin
              if requested_label_string = '' then
                showing_label_string := full_label_string
              else
                showing_label_string := requested_label_string;

              text_end_X := text_begin_X + TextWidth(showing_label_string);
              // for boxed over and mouse hover...
              text_end_Y := text_begin_Y + TextHeight(showing_label_string);
              text_font_height := Font.Height;

              if (paper_bunching = False) or (text_begin_X < (bunch_start - bunch_gap)) or
                (text_begin_X > (bunch_start + bunch_gap * 2))    // no name labels anywhere near the bunch gap.
              then begin
                if (requested_label_string <> '') and
                  (text_begin_X <> min_draw_int) and (text_begin_Y <> min_draw_int)
                then begin
                  if pad_form.boxed_over_names_menu_entry.Checked =
                    True then begin
                    Pen.Color := paper_colour;
                    Pen.Width := 1;
                    Pen.Style := psSolid;     // 215b
                    Rectangle(text_begin_X - 4,
                      text_begin_Y - 4, text_end_X + 4, text_end_Y + 4);
                    Rectangle(text_begin_X - 3,
                      text_begin_Y - 3, text_end_X + 3, text_end_Y + 3);

                    Pen.Color := Font.Color;
                    Rectangle(text_begin_X - 2,
                      text_begin_Y - 2, text_end_X + 2, text_end_Y + 2);
                  end;

                  TextOut(text_begin_X, text_begin_Y,
                    requested_label_string);
                end;//if requested.
              end;//if not bunching.
            end;//if big enough to see.
          end;//with now_keep

          bgnd_keep := now_keep;   // save the label extent.

        except
          CONTINUE;      // ignore this name if calc exception.
        end;//try
      end;//with template.
    end;//for next bgk template

  end;//with canvas
end;
//___________________________________________________________________________________________

procedure draw_notch(canv: TCanvas);
// draw the pegging notch. (might get redrawn by the control template).

var
  notch_dim, pad_notchx, pad_notchy: integer;

  pad_notch_arm_rightx, pad_notch_arm_leftx: integer;
  pad_notch_arm_righty, pad_notch_arm_lefty: integer;

begin

  if pad_form.show_notch_menu_entry.Checked = False then
    EXIT;   // 0.93.a

  with canv do begin
    notch_dim := 10; // 0.91.b was (pad_form.ClientWidth div 100);  // 100 arbitrary - matches size of peg.
    if notch_dim > Round(scale * fx) then
      notch_dim := Round(scale * fx); // but not more than 2ft scale.

    notch_dim := notch_dim + 2;  //  + 2 pixels all round.

    // this way of rounding matches that of the peg (1/100ths mm from the marks list),
    // to ensure exact-looking alignment on the screen. 0.76.a
    // (no effect on actual geometry - this is simply the screen drawing)...

    pad_notchx := Round(Round(notchx * 100) * sx + ex - gx);  // notch centre..
    pad_notchy := Round(Round(notchy * 100) * sy + by - gy);

    pad_notch_arm_rightx := Round(Round((notchx + peg_arm_length * COS(notch_angle)) * 100) * sx + ex - gx);
    // notch arm length is same as peg arm length.
    pad_notch_arm_leftx := Round(Round((notchx - peg_arm_length * COS(notch_angle)) * 100) * sx + ex - gx);

    pad_notch_arm_righty := Round(Round((notchy + peg_arm_length * SIN(notch_angle)) * 100) * sy + by - gy);
    pad_notch_arm_lefty := Round(Round((notchy - peg_arm_length * SIN(notch_angle)) * 100) * sy + by - gy);

    move_to.X := pad_notch_arm_leftx;         // arm dims.
    move_to.Y := pad_notch_arm_lefty;

    line_to.X := pad_notch_arm_rightx;
    line_to.Y := pad_notch_arm_righty;

    if check_limits(move_to, line_to) = True       // arms ok, check the square..
    then begin
      move_to.X := pad_notchx - notch_dim;
      move_to.Y := pad_notchy - notch_dim;
      line_to.X := pad_notchx + notch_dim;
      line_to.Y := pad_notchy + notch_dim;

      if check_limits(move_to, line_to) = True   // square ok, draw the notch...
      then begin
        // first the square ...

        if paper_colour <> clWhite then
          Brush.Color := clWhite   // square with white centre.
        else
          Brush.Color := clLime;   // square with lime centre.
        Brush.Style := bsSolid;

        if paper_colour <> clBlack then
          Pen.Color := clBlack     // black border.
        else
          Pen.Color := clGray;     // grey border.

        Pen.Mode := pmCopy;
        Pen.Style := psSolid;

        Rectangle(move_to.X, move_to.Y, line_to.X, line_to.Y);

        // then the arms...

        case paper_colour of

          clWhite: begin
            Brush.Color := clSilver;
            Pen.Color := clBlack;   // black/silver dots.
          end;

          clBlack: begin
            Brush.Color := clGray;
            Pen.Color := clWhite;   // white/grey dots.
          end;

          else begin
            Brush.Color := clWhite;
            Pen.Color := clBlack;   // black/white dots.
          end;
        end;//case

        Pen.Style := psDot;
        TextOut(0, 0, '');  // needed for dotted lines - Delphi bug?

        // two arms are done separately to match rounding alignment with peg arms (marks codes 8 and 9). 0.76.a  ...

        move_to.X := pad_notchx;
        move_to.Y := pad_notchy;
        line_to.X := pad_notch_arm_rightx;
        line_to.Y := pad_notch_arm_righty;
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);

        line_to.X := pad_notch_arm_leftx;
        line_to.Y := pad_notch_arm_lefty;
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);

        // finally the hair-lines..

        Pen.Color := clBlack;
        Pen.Style := psSolid;

        move_to.X := pad_notchx;
        move_to.Y := pad_notchy - notch_dim;
        line_to.X := pad_notchx;
        line_to.Y := pad_notchy + notch_dim;
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);       // add vertical hair-line.

        move_to.X := pad_notchx - notch_dim;
        move_to.Y := pad_notchy;
        line_to.X := pad_notchx + notch_dim;
        line_to.Y := pad_notchy;
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);       // add horizontal hair-line.
      end;
    end;
  end;//with canvas
end;
//______________________________________________________________________________________

procedure do_background(mode: integer);   //  draw the complete background.

// mode  0 = normal fixed off-screen redraw.

//  modes for mouse actions...

// on-screen modes..

//       1 = zoom fixed. background keeps count less than mode_swap (default 3)
//       2 = ditto, first click.

//       3 = zoom fixed. mode_swap or more background keeps.
//       4 = ditto, first click.

//       5 = zoom or position changing.
//       6 = ditto, first click.

// off-screen modes..

//       7 = zoom fixed.
//       8 = ditto, first click.

//       9 = zoom or position changing.
//      10 = ditto, first click.

begin
  case mode of

    0, 9, 10: begin                   // normal redraw on draw-bitmap, no mouse action.
      wipe_draw_bmp(True);                      // True = add sketchboad items
      draw_all_on_canvas(offdraw_bmp.Canvas);
      // (gocalc does the copy to the pad after adding the mouse labels).
    end;

    1, 2:
      draw_all_on_canvas(pad_form.Canvas);     // no wiping, current is erased instead.

    3, 4: begin
      if mode = 4 then
        wipe_fill_drop;         // draw as much as will remain fixed on the backdrop.
      copy_drop_to_canvas(pad_form.Canvas);  // copy the drop-bitmap to the pad.
      finish_on_canvas(pad_form.Canvas);
      // draw any items not on the backdrop directly on the pad.
    end;

    5, 6: begin                                    // zoom or position changing.
      wipe_pad;
      draw_all_on_canvas(pad_form.Canvas);
    end;

    7, 8: begin
      if mode = 8 then
        wipe_fill_drop;            // draw as much as will remain fixed on the backdrop.
      copy_drop_to_canvas(offdraw_bmp.Canvas);  // copy the drop-bitmap to the draw-bitmap.
      finish_on_canvas(offdraw_bmp.Canvas);
      // draw any items not on the backdrop on the draw bitmap.
    end;
      // (gocalc does the copy to the pad after adding the mouse labels).

    else
      run_error(50);
  end;//case
end;
//________________________________________________________________________________________

procedure draw_all_on_canvas(canv: TCanvas);
// normal draw, use the specified canvas for everything.

begin

  if bgnd_form.trackpad_grid_in_front_checkbox.Checked = True then
    draw_bg_shapes(canv, -1, shapes_colour);   // mod 3-2-01 draw all background shapes before the grid.

  draw_screengrid(canv);        // first draw the grid.
  draw_page_outlines(canv);     // next draw the page outlines.
  show_scalebar(canv);          // add the scalebar.

  if bgnd_form.trackpad_grid_in_front_checkbox.Checked = False then
    draw_bg_shapes(canv, -1, shapes_colour);  // draw all background shapes over grid.

  draw_dummy_vehicle_outline_envelope_as_polygon(canv);  // 215c

  draw_background_templates(canv, 0, -1, False, clBlack);
  //  all the background keeps (clBlack is dummy, not used).

  draw_reminders(canv);

  draw_rings(canv, True, False);                           //  draw the spacing-ring copies.
  draw_rings(canv, False, False);                          //  draw the ring itself.
  draw_notch(canv);                                      // draw the pegging notch.
  draw_ruler(canv);                                      // and the ruler.

  draw_export_rectangle(canv);  // 0.93.a  add the export rectangle.

  draw_dummy_vehicle_copies(canv);    // 0.98.a

  draw_zooming_ring(canv);      // then the zooming ring.    205a moved to top

end;
//________________________________________________________________________________________

procedure draw_dtp_items_on_pad(on_canvas: TCanvas);    // 0.93.a  add sketchboard items

// called only while sketchboard not active  // 205a  was  not showing.

var
  dtp_rect: TRect;
  dtp_left, dtp_top, dtp_width, dtp_height: extended;

  low_res_bitmap: TBitmap;
  low_res_bitmap_width, low_res_bitmap_height: integer;

  low_resolution: extended;

  this_graphic: TGraphic;

begin
  EXIT;  // OT-FIRST
  { OT-FIRST
  if go_sketchboard=False then EXIT;  // 205e   sketchboard not in use

  if trackplan_exists=False then EXIT; // can't scale the items without a trackplan item

  if dtp_form.show_dtp_on_pad_checkbox.Checked=False then EXIT;

  //if dtp_form.Showing=True then EXIT;

  if dtp_form.Active=True then EXIT;


  if  (dtp_settings_form.sb_zoom_limit_checkbox.Checked=True)
  and (dtp_settings_form.display_quality_radiobutton.Checked=True)  // added 205a
     then begin
            if screenx<(g*8) then EXIT;   // in display quality limit zoom-in to screen width = 8 times the track gauge.  // 205a was 5*
          end;

  if stretch_factor_wide<minfp then EXIT;    // no division by zero
  if stretch_factor_high<minfp then EXIT;

  if dtp_form.dtp_document.CurrentPage.PageWidth<minfp then EXIT;

  omit_trackplan_from_rendering:= NOT dtp_settings_form.render_trackplan_on_pad_checkbox.Checked;   // to omit sketchboard trackplans from pad

  dtp_top:=0-onpad_y_offset*fy;     // fy is negative
  dtp_left:=onpad_x_offset*fx;      // fx screen pixels per mm

  dtp_width:=dtp_form.dtp_document.CurrentPage.PageWidth*fx/stretch_factor_wide;        // fx screen pixels per mm
  dtp_height:=0-dtp_form.dtp_document.CurrentPage.PageHeight*fy/stretch_factor_high;    // fy is negative

  dtp_rect.Left:=Round(ex-gx+dtp_left);
  dtp_rect.Top:=Round(by-gy+dtp_top);
  dtp_rect.Right:=Round(ex-gx+dtp_left+dtp_width);
  dtp_rect.Bottom:=Round(by-gy+dtp_top+dtp_height);

  if dtp_settings_form.transfer_mode_low_res_radiobutton.Checked=True    // stretchdraw a bitmap onto pad

     then begin
               // lo-res mode (stretch small bitmap)...

            low_res_bitmap_width:=1300;   // default 1300 x 900

            low_res_bitmap_height:=Round(low_res_bitmap_width*dtp_form.dtp_document.CurrentPage.PageHeight/dtp_form.dtp_document.CurrentPage.PageWidth);

            low_resolution:=low_res_bitmap_width/dtp_form.dtp_document.CurrentPage.PageWidth;  // dots per mm.

            low_res_bitmap:=TBitmap.Create;

            low_res_bitmap.Width:=low_res_bitmap_width;
            low_res_bitmap.Height:=low_res_bitmap_height;

            // OT-FIRST low_res_bitmap.PixelFormat:=pf24bit;    // this seems to allow maximum zoomimg in stretchdraw

            low_res_bitmap.Canvas.Brush.Color:=dtp_form.dtp_document.CurrentPage.PageColor;
            low_res_bitmap.Canvas.Brush.Style:=bsSolid;
            low_res_bitmap.Canvas.FillRect(Rect(0, 0, low_res_bitmap_width,low_res_bitmap_height));

            dtp_form.dtp_document.CurrentPage.Print(low_res_bitmap.Canvas,Rect(0,0,low_res_bitmap_width,low_res_bitmap_height),low_resolution,0,False,False);

            this_graphic:=low_res_bitmap;

            try
              with on_canvas do begin

                //CopyMode:=cmSrcAnd;    // (destination Canvas) transparent if on white background.

                CopyMode:=cmSrcCopy;  // normal for destination Canvas.

                StretchDraw(dtp_rect,this_graphic);   // needs TGraphic parameter to work reliably.
              end;//with

            except
              ShowMessage('Low-res stretch-draw failed.');
            end;//try

            low_res_bitmap.Free;
          end
     else begin
              // he-res, print directly to pad

            copying_sb_to_pad:=True; // flag for DtpPage.Print

            try
              dtp_form.dtp_document.CurrentPage.Print(on_canvas,dtp_rect,fx/stretch_factor_wide,0,False,False);
            except
              ShowMessage('SB hi-res draw to trackpad failed');  //do_nothing;    // integer overflow?
            end;             //try

            copying_sb_to_pad:=False;
          end;

  omit_trackplan_from_rendering:=False; // restore for sketchboard
}

end;
//______________________________________________________________________________

procedure wipe_fill_drop;  // draw as much as will remain fixed on the backdrop.

var
  group_code: integer;

begin
  with backdrop_bmp.Canvas do begin        // clear the backdrop for starters.
    Brush.Color := paper_colour;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, pad_form.ClientWidth, pad_form.ClientHeight));

    draw_dtp_items_on_pad(backdrop_bmp.Canvas);    // 0.93.a  add sketchbook items

    Brush.Color := guide_colour;
    //  top-right bitmap indicator.
    FillRect(Rect(pad_form.ClientWidth - 3, 0, pad_form.ClientWidth, 3));
    //  (visible only when maximized).
    Brush.Color := paper_colour;
    //  restore for next use.

  end;//with

  if bgnd_form.trackpad_grid_in_front_checkbox.Checked = True then
    draw_bg_shapes(backdrop_bmp.Canvas, -1, shapes_colour);
  // mod 3-2-01 draw any background shapes before grid.

  draw_screengrid(backdrop_bmp.Canvas);        //  first draw the grid.
  draw_page_outlines(backdrop_bmp.Canvas);     //  next draw the page outlines.
  show_scalebar(backdrop_bmp.Canvas);          //  add the scalebar.

  if bgnd_form.trackpad_grid_in_front_checkbox.Checked = False then
    draw_bg_shapes(backdrop_bmp.Canvas, -1, shapes_colour);  // draw any background shapes over grid.

  draw_dummy_vehicle_outline_envelope_as_polygon(backdrop_bmp.Canvas);  // 215c

  draw_export_rectangle(backdrop_bmp.Canvas);  // 0.93.a  add the export rectangle.

  group_code := 0;    // default init - all templates.

  if (shift_keeps_mod = 1) or (twist_keeps_mod = 1)  // moving group.
  then
    group_code := -1;                        // so non-group templates only.

  if (mouse_modify > -1) and (notch_linked_to_current = True) and (group_notch_linked = True)
  // mouse action moving linked group?
  then
    group_code := -1;
  // so non-group templates only.

  draw_background_templates(backdrop_bmp.Canvas, group_code, -1, False, clBlack);
  //  clBlack is dummy, not used.

  draw_reminders(backdrop_bmp.Canvas);

  draw_rings(backdrop_bmp.Canvas, True, False);
  //  draw the spacing-ring copies.

  if (ring_mod <> 1) and (ringdia_mod <> 1) then
    draw_rings(backdrop_bmp.Canvas, False, False);   //  draw the ring itself.
  if (ruler1_mod <> 1) and (ruler2_mod <> 1) then
    draw_ruler(backdrop_bmp.Canvas);              //  and the ruler.

  if (notch_mod = 1) or ((mouse_modify > -1) and (notch_linked_to_current = True)) then
    EXIT; //  continue on the other canvas.
  draw_notch(backdrop_bmp.Canvas);
  //  draw the pegging notch.

  draw_dummy_vehicle_copies(backdrop_bmp.Canvas);    // 0.98.a

  draw_zooming_ring(backdrop_bmp.Canvas);      //  then the zooming ring.   205a moved to top
end;
//___________________________________________________________________________________________

procedure finish_on_canvas(canv: TCanvas);
// draw any items not on the backdrop on the specified canvas.

begin
  if (shift_keeps_mod = 1) or (twist_keeps_mod = 1) or
    ((mouse_modify > -1) and (notch_linked_to_current = True) and (group_notch_linked = True))
  // then these are not on backdrop...
  then
    draw_background_templates(canv, 1, -1, False, clBlack);
  //  draw the group templates (clBlack is dummy, not used).

  if (ring_mod = 1) or (ringdia_mod = 1) then
    draw_rings(canv, False, False);      //  draw the ring itself.

  if (notch_mod = 1) or ((mouse_modify > -1) and (notch_linked_to_current = True)) then
    draw_notch(canv);                       //  draw the pegging notch.

  if (ruler1_mod = 1) or (ruler2_mod = 1) then
    draw_ruler(canv);  //  and the ruler.
end;
//___________________________________________________________________________________________

procedure show_scalebar(canv: TCanvas);

var
  sb_colour1, sb_colour2: integer;

  y, scony, incb: integer;
  scx, sbex, black, Inc: extended;
  sb_str: string;

  move_to, line_to: TPoint;

begin
  if paper_bunching = True then
    EXIT;     // scalebar would cross the bunch gap.

  scx := 1;                    // keep compiler happy.
  Inc := 1;                    // ditto.
  case scale_bar_i of
    0:
      EXIT;                 //  scalebar not wanted.

    1: begin
      scx := sx * 100;
      //  adjust scaling to mm. (bar is 20 * 10 mm = 200 mm long).
      sb_str := '200 mm';
      Inc := 5;             //  long marks at 5 * 10 = 50 mm intervals.
    end;

    2: begin
      scx := sx * 63.5;        //  or inches (bar is 20 * 0.25 = 5 inches long).
      sb_str := '5"';
      Inc := 4;              //  long marks at 4 * 0.25 = 1 inch intervals.
    end;

    else
      run_error(72);
  end;//case

  sbex := (xmax / 2) - 100 * scx;             //  start of bar to put it in middle of screen width.
  if sbex < ex then
    sbex := ex;

  case sb_colour_use of
    0: begin                             //  use current grid colours for scalebar.
      sb_colour1 := grid_colour;
      sb_colour2 := paper_colour;
    end;
    1: begin
      sb_colour1 := clBlack;
      sb_colour2 := clWhite;
    end;
    2: begin
      sb_colour1 := clRed;
      sb_colour2 := clYellow;
    end;

    3: begin
      sb_colour1 := clSilver;
      sb_colour2 := clGray;
    end;

    4: begin
      sb_colour1 := clYellow;
      sb_colour2 := clBlue;
    end;

    5: begin
      sb_colour1 := clWhite;
      sb_colour2 := clBlack;
    end;

    6: begin
      sb_colour1 := clTeal;
      sb_colour2 := clAqua;
    end;

    else begin
      sb_colour1 := grid_colour;
      sb_colour2 := paper_colour;
    end;
  end;//case

  scony := ymax - 10;
  if scony > Round(by - 18) then
    scony := Round(by - 18);   // y for top of scale bar, max 18 above sheet edge.

  with canv do begin  // canvas

    Pen.Width := 1;                 // thin black solid lines
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    Pen.Color := sb_colour1;

    Brush.Color := paper_colour;
    Brush.Style := bsSolid;

    move_to.X := Round(sbex);
    move_to.Y := scony;
    line_to.X := Round(sbex + 200 * scx);
    line_to.Y := scony;  // upper rule scale 200 mm or 10" long.

    if check_limits(move_to, line_to) = True then begin
      MoveTo(move_to.X, move_to.Y);
      LineTo(line_to.X, line_to.Y);
    end;

    for y := (scony + 1) to (scony + 3) do begin
      black := 0;
      //  10 pairs of short intervals
      for incb := 1 to 10 do begin
        move_to.X := Round(sbex + black * scx);
        move_to.Y := y;                                // black/white scale 10 mm or 0.5" intervals.

        Pen.Color := sb_colour1;
        line_to.X := Round(sbex + (black + 10) * scx + 1);
        line_to.Y := y;   // +1 is a bodge to make black/white look equal intervals.

        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;

        move_to := line_to;

        Pen.Color := sb_colour2;
        line_to.X := Round(sbex + (black + 20) * scx);
        line_to.Y := y;
        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;

        black := black + 20;
      end;
    end;                           //  3 lines deep.

    for y := (scony + 5) to (scony + 7) do begin
      black := 0;
      //  2 pairs of long intervals
      for incb := 1 to 2 do begin
        move_to.X := Round(sbex + black * scx);
        move_to.Y := y;                                    // black/white scale 10 mm or 0.5" intervals.

        Pen.Color := sb_colour1;
        line_to.X := Round(sbex + (black + 10 * Inc) * scx + 1);
        line_to.Y := y;   // +1 is a bodge to make black/white look equal intervals.
        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;

        move_to := line_to;

        Pen.Color := sb_colour2;
        line_to.X := Round(sbex + (black + 20 * Inc) * scx);
        line_to.Y := y;                     // only do half of last pair (for inches).
        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;

        black := black + 20 * Inc;
      end;
      if Inc = 4                   //   need another half pair if inches.
      then begin
        move_to.X := Round(sbex + black * scx);
        move_to.Y := y;
        Pen.Color := sb_colour1;
        line_to.X := Round(sbex + (black + 10 * Inc) * scx);
        line_to.Y := y;
        if check_limits(move_to, line_to) = True then begin
          MoveTo(move_to.X, move_to.Y);
          LineTo(line_to.X, line_to.Y);
        end;

      end;

    end;                          // 3 lines deep.

    Pen.Color := sb_colour1;

    move_to.X := Round(sbex);
    move_to.Y := scony + 4;
    line_to.X := Round(sbex + 200 * scx);
    line_to.Y := scony + 4;    // middle rule
    if check_limits(move_to, line_to) = True then begin
      MoveTo(move_to.X, move_to.Y);
      LineTo(line_to.X, line_to.Y);
    end;

    move_to.X := Round(sbex);
    move_to.Y := scony + 8;
    line_to.X := Round(sbex + 200 * scx);
    line_to.Y := scony + 8;    // lower rule
    if check_limits(move_to, line_to) = True then begin
      MoveTo(move_to.X, move_to.Y);
      LineTo(line_to.X, line_to.Y);
    end;

    move_to := line_to;
    line_to.X := Round(sbex + 200 * scx);
    line_to.Y := scony - 1;    // close white end. (-1 finishes the corner).
    if check_limits(move_to, line_to) = True then begin
      MoveTo(move_to.X, move_to.Y);
      LineTo(line_to.X, line_to.Y);
    end;

    Font.Assign(pad_form.Font);
    Font.Color := sb_colour1;

    move_to.X := Round(sbex + 200 * scx + 4);
    move_to.Y := scony + 2 + (Font.Height div 2);
    if check_limit(False, False, move_to) = True then
      TextOut(move_to.X, move_to.Y, sb_str);  // label end of bar. (font height is negative).
    Font.Color := pad_form.Font.Color;              // reset the font colour.
  end;//with
end;
//_______________________________________________________________________________

procedure draw_ruler(canv: TCanvas);

var
  sb_colour1, sb_colour2: integer;
  move_to, line_to: TPoint;
  scx, scy: extended;
  ruler_xlength, ruler_ylength, ruler_len, ruler_k: extended;
  end_start_len, first_len: extended;
  xbar, ybar, ruler_halfbar_width: extended;
  xx1, xx2, yy1, yy2, x1, y1, x2, y2, x3, y3, x4, y4, len_done: extended;
  rule_factor: extended;
  rule_str: string;

  ////////////////////////////////////

  function screen_x(x: extended): integer;

  begin
    Result := Round(x * scx + ex - gx);
  end;
  /////////////////////////////////////

  function screen_y(y: extended): integer;

  begin
    Result := Round(y * scy + by - gy);
  end;
  /////////////////////////////////////

  procedure do_line;

  begin
    with canv do begin
      move_to.X := screen_x(x1);
      move_to.Y := screen_y(y1);

      line_to.X := screen_x(x2);
      line_to.Y := screen_y(y2);     // ruler mid-line.

      if check_limits(move_to, line_to) = True
      then begin
        MoveTo(move_to.X, move_to.Y);
        LineTo(line_to.X, line_to.Y);
      end;
    end;//with
  end;
  //////////////////////////////////////

  procedure do_polygon;

  var
    infill_points: array [0..3] of TPoint;

  begin
    infill_points[0].X := screen_x(x1);
    infill_points[1].X := screen_x(x2);
    infill_points[2].X := screen_x(x3);
    infill_points[3].X := screen_x(x4);

    infill_points[0].Y := screen_y(y1);
    infill_points[1].Y := screen_y(y2);
    infill_points[2].Y := screen_y(y3);
    infill_points[3].Y := screen_y(y4);

    with canv do begin
      if (check_limits(infill_points[0], infill_points[1]) = True) and
        (check_limits(infill_points[2], infill_points[3]) = True)
      then
      begin
        Polygon(infill_points);
      end;
    end;//with
  end;
  ///////////////////////////////////////

begin
  if show_ruler_tool = False then
    EXIT;   // not wanted.
  if paper_bunching = True then
    EXIT;     // ruler might cross the bunch gap.

  if pad_form.ClientHeight < 1 then
    EXIT;

  scx := sx * 100;        //  screen scaling.
  scy := sy * 100;

  ruler_halfbar_width := 4 * screeny / pad_form.ClientHeight;   // bar width mm = 8 screen lines.

  ruler_xlength := ruler_endx - ruler_startx;             // mm
  ruler_ylength := ruler_endy - ruler_starty;

  if ABS(ruler_xlength) < minfp then begin
    ruler_k := Pi / 2 * SGZ(ruler_ylength);
    ruler_len := ABS(ruler_ylength);       // ABS is bug fix, 0.79.a
  end
  else begin
    ruler_len := SQRT(SQR(ruler_xlength) + SQR(ruler_ylength));

    if ruler_len < minfp then
      EXIT;   // ???

    try
      ruler_k := ARCSIN(ruler_ylength / ruler_len);
    except
      ruler_k := Pi / 2 * SGZ(ruler_ylength);
    end;//try

    if ruler_xlength < 0 then
      ruler_k := Pi - ruler_k;
  end;


  case ruler_units of
    0: begin      // as grid
      case grid_labels_code_i of
        1:
          rule_str := 'feet';     //  labels in feet.
        2:
          rule_str := 'inches';   //  labels in inches.
        3:
          rule_str := 'proto-feet'; //  labels in prototype feet.
        4:
          rule_str := 'cm';       //  labels in cm.
        6:
          rule_str := 'mm';       //  labels in mm.
        else
          rule_str := 'mm';       //  was run_error(223);
      end;//case
    end;
    1:
      rule_str := 'mm';
    2:
      rule_str := 'inches';
    else
      rule_str := 'mm';
  end;//case

  case ruler_units of
    0: begin      // as grid
      case grid_labels_code_i of
        1:
          rule_factor := 1 / 304.8; //  labels in feet.
        2:
          rule_factor := 1 / 25.4;  //  labels in inches.
        3:
          rule_factor := 1 / scale; //  labels in prototype feet.
        4:
          rule_factor := 1 / 10;    //  labels in cm.
        6:
          rule_factor := 1.0;     //  labels in mm.
        else
          rule_factor := 1.0;     //  mm, was run_error(222);
      end;//case
    end;
    1:
      rule_factor := 1.0;       // mm
    2:
      rule_factor := 1 / 25.4;    // inches
    else
      rule_factor := 1.0;       // mm
  end;//case

  case sb_colour_use of
    0: begin                             //  use current grid colours for scalebar.
      sb_colour1 := grid_colour;
      sb_colour2 := paper_colour;
    end;
    1: begin
      sb_colour1 := clBlack;
      sb_colour2 := clWhite;
    end;
    2: begin
      sb_colour1 := clRed;
      sb_colour2 := clYellow;
    end;

    3: begin
      sb_colour1 := clSilver;
      sb_colour2 := clGray;
    end;

    4: begin
      sb_colour1 := clYellow;
      sb_colour2 := clBlue;
    end;

    5: begin
      sb_colour1 := clWhite;
      sb_colour2 := clBlack;
    end;

    6: begin
      sb_colour1 := clTeal;
      sb_colour2 := clAqua;
    end;

    else begin
      sb_colour1 := grid_colour;
      sb_colour2 := paper_colour;
    end;
  end;//case

  with canv do begin  // canvas

    Pen.Width := 1;                 // thin solid lines
    Pen.Style := psSolid;
    Pen.Mode := pmCopy;
    Pen.Color := sb_colour1;
    Brush.Style := bsSolid;

    // do first colour bars...

    Brush.Color := sb_colour1;
    xbar := ruler_startx;
    ybar := ruler_starty;
    len_done := 0;           // no offset for first colour.

    while len_done <= (ruler_len - ruler_div) do begin
      x1 := xbar - ruler_halfbar_width * SIN(ruler_k);
      y1 := ybar + ruler_halfbar_width * COS(ruler_k);

      x2 := x1 + ruler_div * COS(ruler_k);
      y2 := y1 + ruler_div * SIN(ruler_k);

      x4 := xbar + ruler_halfbar_width * SIN(ruler_k);
      y4 := ybar - ruler_halfbar_width * COS(ruler_k);

      x3 := x4 + ruler_div * COS(ruler_k);
      y3 := y4 + ruler_div * SIN(ruler_k);

      do_polygon;

      xbar := xbar + ruler_div * 2 * COS(ruler_k);
      ybar := ybar + ruler_div * 2 * SIN(ruler_k);
      len_done := len_done + ruler_div * 2;
    end;//while

    first_len := len_done;

    // do second colour bars...

    Brush.Color := sb_colour2;
    xbar := ruler_startx + ruler_div * COS(ruler_k);
    ybar := ruler_starty + ruler_div * SIN(ruler_k);

    len_done := ruler_div;              // start offset for 2nd colour.

    while len_done <= (ruler_len - ruler_div) do begin
      x1 := xbar - ruler_halfbar_width * SIN(ruler_k);
      y1 := ybar + ruler_halfbar_width * COS(ruler_k);

      x2 := x1 + ruler_div * COS(ruler_k);
      y2 := y1 + ruler_div * SIN(ruler_k);

      x4 := xbar + ruler_halfbar_width * SIN(ruler_k);
      y4 := ybar - ruler_halfbar_width * COS(ruler_k);

      x3 := x4 + ruler_div * COS(ruler_k);
      y3 := y4 + ruler_div * SIN(ruler_k);

      do_polygon;

      xbar := xbar + ruler_div * 2 * COS(ruler_k);
      ybar := ybar + ruler_div * 2 * SIN(ruler_k);
      len_done := len_done + ruler_div * 2;
    end;//while

    // do final bar in red..

    if len_done > first_len then
      end_start_len := len_done - ruler_div
    else
      end_start_len := first_len - ruler_div;

    if (sb_colour1 = clRed) or (sb_colour2 = clRed) then
      Brush.Color := grid_colour
    else
      Brush.Color := clRed;

    xbar := ruler_startx + end_start_len * COS(ruler_k);
    ybar := ruler_starty + end_start_len * SIN(ruler_k);

    x1 := xbar - ruler_halfbar_width * SIN(ruler_k);
    y1 := ybar + ruler_halfbar_width * COS(ruler_k);

    x2 := ruler_endx - ruler_halfbar_width * SIN(ruler_k);
    y2 := ruler_endy + ruler_halfbar_width * COS(ruler_k);

    x3 := ruler_endx + ruler_halfbar_width * SIN(ruler_k);
    y3 := ruler_endy - ruler_halfbar_width * COS(ruler_k);

    x4 := xbar + ruler_halfbar_width * SIN(ruler_k);
    y4 := ybar - ruler_halfbar_width * COS(ruler_k);

    do_polygon;      // fill final bar.

    x1 := ruler_startx;         // start extension mark..
    y1 := ruler_starty;
    x2 := x1 - ruler_halfbar_width * 2 * COS(ruler_k);
    y2 := y1 - ruler_halfbar_width * 2 * SIN(ruler_k);
    do_line;

    x1 := ruler_endx;           // end extension mark..
    y1 := ruler_endy;
    x2 := x1 + ruler_halfbar_width * 2 * COS(ruler_k);
    y2 := y1 + ruler_halfbar_width * 2 * SIN(ruler_k);
    do_line;

    // mark divisions...

    xx1 := ruler_startx - ruler_halfbar_width * 2 * SIN(ruler_k);    // co-ords for start division mark..
    yy1 := ruler_starty + ruler_halfbar_width * 2 * COS(ruler_k);
    // 2 arbitrary (mark length each side of centre).

    xx2 := ruler_startx + ruler_halfbar_width * 2.5 * SIN(ruler_k);  // 2.5 looks better
    yy2 := ruler_starty - ruler_halfbar_width * 2.5 * COS(ruler_k);

    len_done := 0;  // first division at start.

    Font.Assign(pad_form.Font);
    //Font.Color:=sb_colour1;
    Brush.Color := paper_colour;
    Brush.Style := bsClear;

    repeat        // mark divisions..

      x1 := xx1 + len_done * COS(ruler_k);
      y1 := yy1 + len_done * SIN(ruler_k);
      x2 := xx2 + len_done * COS(ruler_k);
      y2 := yy2 + len_done * SIN(ruler_k);
      do_line;

      if len_done < (ruler_len - ruler_div)  // not the last one, so not under the full length text.
      then begin
        if ruler_endx > ruler_startx then begin
          move_to.X := screen_x(x2);
          move_to.Y := screen_y(y2);
        end
        else begin
          move_to.X := screen_x(x1);      // swap label side if ruler reversed.
          move_to.Y := screen_y(y1);
        end;

        if check_limit(False, False, move_to) = True then
          TextOut(move_to.X, move_to.Y, FormatFloat('0.###', len_done * rule_factor)); // label divisions.
      end;

      len_done := len_done + ruler_div;

    until len_done > ruler_len;

    x1 := xx1 + ruler_len * COS(ruler_k);    // end division mark..
    y1 := yy1 + ruler_len * SIN(ruler_k);
    x2 := xx2 + ruler_len * COS(ruler_k);
    y2 := yy2 + ruler_len * SIN(ruler_k);
    do_line;

    if ruler_endx > ruler_startx then begin
      move_to.X := screen_x(x2);
      move_to.Y := screen_y(y2);
    end
    else begin
      move_to.X := screen_x(x1);      // swap label side if ruler reversed.
      move_to.Y := screen_y(y1);
    end;

    if check_limit(False, False, move_to) = True then
      TextOut(move_to.X, move_to.Y, FormatFloat('0.###', ruler_len * rule_factor) + ' ' + rule_str);
    // label end mark.

    Font.Color := pad_form.Font.Color;              // reset the font colour.

  end;//with
end;
//____________________________________________________________________________________________

procedure copy_draw_to_pad;     // copy the draw-bitmap to the pad.

var
  copy_rect: TRect;

begin
  copy_rect := Rect(0, 0, pad_form.ClientWidth, pad_form.ClientHeight);
  pad_form.Canvas.CopyMode := cmSrcCopy;
  pad_form.Canvas.CopyRect(copy_rect, offdraw_bmp.Canvas, copy_rect);
end;
//_______________________________________________________________________________________

procedure copy_drop_to_canvas(canv: TCanvas);
// copy the backdrop-bitmap to the specified canvas.

var
  copy_rect: TRect;

begin
  copy_rect := Rect(0, 0, pad_form.ClientWidth, pad_form.ClientHeight);
  canv.CopyMode := cmSrcCopy;
  canv.CopyRect(copy_rect, backdrop_bmp.Canvas, copy_rect);
end;
//_______________________________________________________________________________________

procedure wipe_pad;

begin
  pad_form.Color := paper_colour;         //  reset if necessary the underlying form colour.

  with pad_form do begin
    with Canvas do begin
      Brush.Color := paper_colour;          //  this clears the trackpad.
      Brush.Style := bsSolid;
      FillRect(Rect(0, 0, ClientWidth, ClientHeight));

      Brush.Color := rail_colour;                         //  top-right bitmap indicator.
      FillRect(Rect(ClientWidth - 3, 0, ClientWidth, 3)); //  (visible only when maximized).
      Brush.Color := paper_colour;                        //  restore for next use.
      //   end;
    end;//with
  end;//with
end;
//______________________________________________________________________________

procedure wipe_draw_bmp(add_sketchboard_items: boolean);  // wipe the draw-bitmap.

begin

  with offdraw_bmp.Canvas do begin
    Brush.Color := paper_colour;     //  this clears the draw-bitmap.
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, pad_form.ClientWidth, pad_form.ClientHeight));

    if add_sketchboard_items = True then
      draw_dtp_items_on_pad(offdraw_bmp.Canvas);    // 0.93.a  add sketchbook items

    Brush.Color := timber_colour;
    //  top-right bitmap indicator.
    FillRect(Rect(pad_form.ClientWidth - 3, 0, pad_form.ClientWidth, 3));
    //  (visible only when maximized).
    Brush.Color := paper_colour;                                          //  restore for next use.
  end;//with
end;
//______________________________________________________________________________

function hover_mousedown(click_x, click_y, limit: extended): integer;
  // to highlight a bgnd keep if any at this clicked location.
  // return the index to it, or -1 if none.
var
  bgk: integer;
  now_keep: Tbgnd_keep;

  aq: integer;
  nk, array_max: integer;
  xint, yint: integer;

  X_left, X_right, Y_top, Y_bottom: integer;

begin
  Result := -1;  // default init.

  if keeps_list.Count < 1 then
    EXIT;

  if pad_form.hide_bgnd_keeps_menu_entry.Checked = True then
    EXIT;  // 209c bug-fix  can't click on invisible template

  X_left := Round((click_x - limit) * 100);     // hot zone limit*2 mm square. 1/100ths mm.
  X_right := Round((click_x + limit) * 100);
  Y_bottom := Round((click_y - limit) * 100);
  Y_top := Round((click_y + limit) * 100);

  for bgk := 0 to (keeps_list.Count - 1) do begin

    with Ttemplate(keeps_list.Objects[bgk]) do begin

      if bg_copied = False then
        CONTINUE;  // not a background template.

      if (group_selected = True) and (pad_form.hide_group_templates_menu_entry.Checked = True) then
        CONTINUE;  // 209c  can't click on invisible template

      try
        now_keep := bgnd_keep;   // next background keep.

        with now_keep do begin

          for aq := 25 downto 0 do begin
            // save time by searching centre-lines first, ignore FB foot lines.

            if Length(list_bgnd_rails[aq]) = 0 then
              CONTINUE;                       // empty rail.

            array_max := High(list_bgnd_rails[aq]);
            for nk := 0 to array_max do begin

              xint := list_bgnd_rails[aq][nk].X;
              yint := list_bgnd_rails[aq][nk].Y;

              if (xint > X_left) and (xint < X_right) and (yint > Y_bottom) and
                (yint < Y_top) then begin
                Result := bgk;     // found clicked bgnd keep.
                EXIT;
              end;

            end;//next nk
          end;//next aq

        end;//with now_keep
      except
        Result := -1;
        EXIT;
      end;//try
    end;//with template.
  end;//next bgk template.
end;
//______________________________________________________________________________


//  SPACING RING FUNCTIONS :-


procedure Tgrid_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position > size_updown.Tag       // ! position goes up, size goes down.
  then
    ScaleBy(9, 10);                        // scale the form contents down.

  if size_updown.Position < size_updown.Tag then
    ScaleBy(10, 9);                        // scale the form contents up.

  ClientHeight := VertScrollBar.Range;            // allow 4 pixel right margin.
  ClientWidth := HorzScrollBar.Range + 4;
  // don't need bottom margin - datestamp label provides this.
  ClientHeight := VertScrollBar.Range;            // do this twice, as each affects the other.

  size_updown.Tag := size_updown.Position;        // and save for the next click.

  x_label.Top := measuring_panel.Height div 10;
  // attempt to keep these labels central in the panel.
  y_label.Top := measuring_panel.Height div 10;
  diag_label.Top := measuring_panel.Height div 10;
end;
//__________________________________________________________________________________________

procedure Tgrid_form.colour_panelClick(Sender: TObject);

begin
  Color := get_colour('choose  a  new  colour  for  the  spacing-ring  dialog', Color);
end;
//________________________________________________________________________________________

procedure Tgrid_form.close_buttonClick(Sender: TObject);

begin
  Close;
end;
//________________________________________________________________________________________

procedure Tgrid_form.FormCreate(Sender: TObject);

begin
  pad_form.InsertControl(grid_form);

  AutoScroll := True;
end;
//________________________________________________________________________________________

procedure Tgrid_form.help_buttonClick(Sender: TObject);

begin
  help(0, ring_help_str, '');
end;
//________________________________________________________________________________________

procedure Tgrid_form.six_foot_buttonClick(Sender: TObject);

begin
  set_six_foot_ring;
end;
//_______________________________________________________________________________________

procedure Tgrid_form.ring_size_buttonClick(Sender: TObject);

begin
  get_ring_size;
end;
//_________________________________________________________________________________________

procedure Tgrid_form.ring_location_buttonClick(Sender: TObject);

begin
  attach_ring_checkbox.Checked := False;  // 206b
  get_ring_location;
end;
//_________________________________________________________________________________________

procedure Tgrid_form.colour_buttonClick(Sender: TObject);

begin
  ring_colour := get_colour('choose  a  new  colour  for  the  spacing - ring  tool', ring_colour);
end;
//__________________________________________________________________________________________

procedure Tgrid_form.ring_copy_colour_buttonClick(Sender: TObject);

begin
  ring_copy_colour := get_colour('choose  a  new  colour  for  the  spacing - ring  copies',
    ring_copy_colour);
end;
//___________________________________________________________________________________________

procedure Tgrid_form.make_target_buttonClick(Sender: TObject);

begin
  ring_dia := scale / 2;
  rings[0, 2] := ring_dia;
  grid_form.dia_label.Caption := 'dia: ' + round_str(ring_dia, 2) + ' mm';
  do_rollback := False;
  redraw(True);
end;
//___________________________________________________________________________________________

procedure Tgrid_form.make_shapes_buttonClick(Sender: TObject);

var                               // convert the spacing ring to a background shape.
  new_shape: Tbgnd_shape;
  n: integer;
  time_str: string;

begin
  n := 0;  // default.

  time_str := DateTimeToStr(Date + Time); // identify this shape with the time of creation.
  with new_shape do begin

    shape_name := 'ring target vert ' + time_str;

    hide_bits := 0;  // 214a  normal visibility
    option_bits := 0;     // byte;

    shape_code := 0;      // 0=line, 1=rectangle, 2=circle.
    shape_style := 0;     // 0=transparent, 1=blank, 2=cross-hatched;

    p1.x := rings[0, 0];
    p1.y := rings[0, 1] - inscale * 9;     // 9" scale.

    p2.x := p1.x;
    p2.y := rings[0, 1] + inscale * 9;

    with bgnd_form.bgnd_shapes_listbox.Items do begin
      n := AddObject(new_shape.shape_name, Tbgshape.Create);
      // create and add a new line in the shapes list.
      Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.
      Tbgshape(Objects[n]).bgimage := nil;                   // not bitmap image 3-2-01.
    end;//with

    shape_name := 'ring target horz ' + time_str;

    hide_bits := 0;  // 214a  normal visibility
    option_bits := 0;     // byte;

    shape_code := 0;      // 0=line, 1=rectangle, 2=circle.
    shape_style := 0;     // 0=transparent, 1=blank, 2=cross-hatched;

    p1.x := rings[0, 0] - inscale * 9;    // 9" scale.
    p1.y := rings[0, 1];

    p2.x := rings[0, 0] + inscale * 9;
    p2.y := p1.y;

    with bgnd_form.bgnd_shapes_listbox.Items do begin
      n := AddObject(new_shape.shape_name, Tbgshape.Create);
      // create and add a new line in the shapes list.
      Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.
      Tbgshape(Objects[n]).bgimage := nil;                   // not bitmap image 3-2-01.
    end;//with

    if rings[0, 2] > 0              // inner diameter.
    then begin
      shape_name := 'ring inner ' + time_str;

      hide_bits := 0;  // 214a  normal visibility
      option_bits := 0;     // byte;

      shape_code := 2;      // 0=line, 1=rectangle, 2=circle.
      shape_style := 0;     // 0=transparent, 1=blank, 2=cross-hatched;

      p1.x := rings[0, 0] - rings[0, 2] / 2;     // centre-inner radius.
      p1.y := rings[0, 1] - rings[0, 2] / 2;

      p2.x := rings[0, 0] + rings[0, 2] / 2;     // centre+inner radius.
      p2.y := rings[0, 1] + rings[0, 2] / 2;

      with bgnd_form.bgnd_shapes_listbox.Items do begin
        n := AddObject(new_shape.shape_name, Tbgshape.Create);
        // create and add a new line in the shapes list.
        Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.
        Tbgshape(Objects[n]).bgimage := nil;                   // not bitmap image 3-2-01.
      end;//with

    end;

    if rings[0, 3] > 0              // outer diameter.
    then begin
      shape_name := 'ring outer ' + time_str;

      hide_bits := 0;  // 214a  normal visibility
      option_bits := 0;     // byte;

      shape_code := 2;      // 0=line, 1=rectangle, 2=circle.
      shape_style := 0;     // 0=transparent, 1=blank, 2=cross-hatched;

      p1.x := rings[0, 0] - rings[0, 3] / 2;     // centre-inner radius.
      p1.y := rings[0, 1] - rings[0, 3] / 2;

      p2.x := rings[0, 0] + rings[0, 3] / 2;     // centre+inner radius.
      p2.y := rings[0, 1] + rings[0, 3] / 2;

      with bgnd_form.bgnd_shapes_listbox.Items do begin
        n := AddObject(new_shape.shape_name, Tbgshape.Create);
        // create and add a new line in the shapes list.
        Tbgshape(Objects[n]).bgnd_shape := new_shape;          // put data in list.
        Tbgshape(Objects[n]).bgimage := nil;                   // not bitmap image 3-2-01.
      end;//with
    end;
  end;//with

  bgnd_form.bgnd_shapes_listbox.ItemIndex := n;         // make last shape entered current.

  shapes_saved := False;            // need a fresh save.
  shapes_current_state;           // update the form.

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tgrid_form.clear_buttonClick(Sender: TObject);

begin
  ring_index := 0;             // only the current ring.
  do_rollback := False;        // no need to put this change in rollback register on redraw.
  delete_copy_button.Enabled := False;
  clear_button.Enabled := False;

  do_rollback := False;       // no need to put this change in rollback register on redraw.  
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tgrid_form.delete_copy_buttonClick(Sender: TObject);

begin
  if ring_index < 1 then
    EXIT;
  Dec(ring_index);
  if ring_index = 0 then begin
    delete_copy_button.Enabled := False;
    clear_button.Enabled := False;
  end;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tgrid_form.make_copy_buttonClick(Sender: TObject);

begin
  if ring_index > (ring_count_c - 1) then begin
    ShowMessage('Ring copies - limit reached.' + #13 + #13 +
      'To make another copy you must first delete one or more existing copies.');
    EXIT;
  end;

  Inc(ring_index);                // update count of ring copies.

  rings[ring_index, 0] := rings[0, 0];     // and copy current ring.
  rings[ring_index, 1] := rings[0, 1];
  rings[ring_index, 2] := rings[0, 2];
  rings[ring_index, 3] := rings[0, 3];

  delete_copy_button.Enabled := True;
  clear_button.Enabled := True;

  measure_org_x := rings[0, 0];          // new origin for measurements in measuring panel.
  measure_org_y := rings[0, 1];

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_______________________________________________________________________________________

procedure Tgrid_form.move_pen_buttonClick(Sender: TObject);

begin
  pen_now_at.x := rings[0, 0];    // current ring.
  pen_now_at.y := rings[0, 1];
end;
//________________________________________________________________________________________

procedure Tgrid_form.draw_line_buttonClick(Sender: TObject);

var                               // draw line with "pen".
  new_shape: Tbgnd_shape;
  n: integer;
  time_str: string;

begin
  time_str := DateTimeToStr(Date + Time); // identify this shape with the time of creation.
  with new_shape do begin

    shape_name := 'pen-line ' + time_str;

    hide_bits := 0;  // 214a  normal visibility
    option_bits := 0;     // byte;

    shape_code := 0;      // 0=line, 1=rectangle, 2=circle.

    if bgnd_form.dotted_radio_button.Checked = True then
      shape_style := 2
    else
      shape_style := 0;     // 0=transparent, 1=blank, 2=cross-hatched or dotted.

    p1 := pen_now_at;     // starting point (Tpex).

    p2.x := rings[0, 0];   // end at current ring.
    p2.y := rings[0, 1];

    pen_now_at := p2;     // for next time.

  end;//with

  with bgnd_form.bgnd_shapes_listbox do begin
    n := Items.AddObject(new_shape.shape_name, Tbgshape.Create);
    // create and add a new line in the shapes list.
    Tbgshape(Items.Objects[n]).bgnd_shape := new_shape;          // put data in list.
    Tbgshape(Items.Objects[n]).bgimage := nil;                   // not bitmap image 3-2-01.
    ItemIndex := n;                                              // make it current.
  end;//with

  shapes_saved := False;            // need a fresh save.
  shapes_current_state;           // update the form.

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tgrid_form.FormShow(Sender: TObject);

begin
  dia_label.Caption := 'dia: ' + round_str(ring_dia, 2) + ' mm';
  if show_spacing_rings = True then
    show_rings_radio_button.Checked := True
  else
    hide_rings_radio_button.Checked := True;

  measuring_panel.Color := clWhite;
end;
//_________________________________________________________________________________________

procedure Tgrid_form.show_rings_radio_buttonClick(Sender: TObject);

begin
  if ring_has_been_shown = False then
    grid_form.jump_to_centre_button.Click;    // 0.98.a move to centre on first showing

  show_spacing_rings := True;
  ring_has_been_shown := True;    // 0.98.a
  do_rollback := False;           // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//_________________________________________________________________________________________

procedure Tgrid_form.hide_rings_radio_buttonClick(Sender: TObject);

begin
  show_spacing_rings := False;
  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//________________________________________________________________________________________

procedure Tgrid_form.mouse_buttonClick(Sender: TObject);

begin
  attach_ring_checkbox.Checked := False;  // 206b
  pad_form.move_ring_menu_entry.Click;
end;
//_______________________________________________________________________________________

procedure Tgrid_form.adjust_ring_dia_buttonClick(Sender: TObject);

begin
  pad_form.adjust_spacing_ring_size_menu_entry.Click;
end;
//______________________________________________________________________________________

procedure Tgrid_form.jump_to_mouse_buttonClick(Sender: TObject);

begin
  attach_ring_checkbox.Checked := False;  // 206b

  show_spacing_rings := True;      // otherwise won't see it (no cursor).
  ring_has_been_shown := True;     // 0.98.a

  rings[0, 0] := mouse_now_x;       //  jump ring to mouse.
  rings[0, 1] := mouse_now_y;

  do_rollback := False;
  redraw(True);
end;
//________________________________________________________________________________________

procedure Tgrid_form.jump_to_centre_buttonClick(Sender: TObject);

begin
  attach_ring_checkbox.Checked := False;  // 206b

  show_spacing_rings := True;      // otherwise won't see it (no cursor).
  ring_has_been_shown := True;     // 0.98.a

  rings[0, 0] := zoom_offsetx + screenx / 2;         //  jump ring to screen centre.
  rings[0, 1] := zoom_offsety + screeny / 2;

  if (Top < pad_form.ClientHeight * 8 div 15) and ((Top + Height) > pad_form.ClientHeight * 7 div 15) and
    (Left < pad_form.ClientWidth * 8 div 15) and ((Left + Width) > pad_form.ClientWidth * 7 div 15) then
    Top := 10;
  // make sure form isn't covering the centre.
  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________________

procedure Tgrid_form.jump_to_notch_buttonClick(Sender: TObject);

begin
  attach_ring_checkbox.Checked := False;  // 206b

  show_spacing_rings := True;      // otherwise won't see it (no cursor).
  ring_has_been_shown := True;     // 0.98.a

  rings[0, 0] := notchx;       //  jump ring to pegging notch.
  rings[0, 1] := notchy;

  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________________

procedure Tgrid_form.cross_hairs_buttonClick(Sender: TObject);

begin
  pad_form.cross_hairs_pointer_menu_entry.Click;
end;
//_______________________________________________________________________________________

procedure Tgrid_form.dummy_vehicle_dimensions_buttonClick(Sender: TObject);

const
  helpdv_str: string = 'php/920    `0Dummy  Vehicle  Tool`9' +
    '||For information about using the dummy vehicle tool click <A HREF="online_ref920.85a">more information online</A>.';

  helpdv_len_str: string = 'php/920    `0Dummy  Vehicle  Length`9' +
    '||Enter a body length for the dummy vehicle, in full size prototype inches. The pre-set length is 780 inches (65ft).'
    +
    '||For more information about using the dummy vehicle click <A HREF="online_ref920.85a">more information online</A>.';

  helpdv_wide_str: string = 'php/920    `0Dummy  Vehicle  Width`9' +
    '||Enter a body width for the dummy vehicle, in full size prototype inches. The pre-set width is 111 inches (9ft-3in).'
    +
    '||For more information about using the dummy vehicle click <A HREF="online_ref920.85a">more information online</A>.';

  helpdv_wb_str: string = 'php/920    `0Dummy  Vehicle  Wheelbase`9'
    + '||Enter a dimension for the wheelbase of the dummy vehicle, in full size prototype inches.'
    + '||For bogie vehicles enter the distance between the bogie centres.'
    + '||For fixed-axle vehicles enter the distance between the outer axles.'
    + '||The pre-set wheelbase is 558 inches (46ft-6in).'
    + '||rp.gif  The scaled wheelbase dimension must not exceed the track radius.'
    +
    '||For more information about using the dummy vehicle click <A HREF="online_ref920.85a">more information online</A>.';

  helpdv_clr_str: string = 'php/920    `0Dummy  Vehicle  Clearance`9' +
    '||Enter a dimension for the clearance marker lines on the dummy vehicle, in full size prototype inches.'
    +
    '||These are the dashed lines which show along each side of the dummy vehicle, as a guide to checking clearance between vehicles.'
    +
    '||The entered dimension is the distance of each marker line from the body side. The pre-set clearance is 6 inches.'
    +
    '||These marker lines can be adjusted using the `0dummy vehicle clearance`1 mouse action to measure the actual clearance available from other objects.' + '||For more information about using the dummy vehicle click <A HREF="online_ref920.85a">more information online</A>.';

  helpdv_pos_str: string = 'php/920    `0Dummy  Vehicle  Position`9'
    + '||Enter a dimension for the position of the dummy vehicle on the control template, in model mm.'
    +
    '||This dimension is the distance from the `0CTRL-0`2 datum end of the template to the first bogie centre, or first fixed axle.'
    + '||The pre-set position is the scale equivalent of 9ft-3in.'
    +
    '||You will normally want to adjust this dimension visually, using the `0roll dummy vehicle`1 mouse action.'
    +
    '||For more information about using the dummy vehicle click <A HREF="online_ref920.85a">more information online</A>.';

var
  n: integer;
  od: Toutdim;

begin
  with cdvi do begin
    // these are all stored in FULL-SIZE INCHES ...

    putdim(helpdv_len_str, 2, 'vehicle  body  length  ( full-size  inches )',
      dv_length, True, False, False, False);
    // no neg, preset ok, zero ok, don't terminate on zero.
    putdim(helpdv_wide_str, 2, 'vehicle  body  width  ( full-size  inches )',
      dv_width, True, False, False, False);
    // no neg, preset ok, zero ok, don't terminate on zero.
    putdim(helpdv_wb_str, 2, 'wheelbase  or  bogie  centres  ( full-size  inches )',
      dv_wheelbase, True, False, False, False); // no neg, preset ok, zero ok, don't terminate on zero.
    putdim(helpdv_clr_str, 2, 'body  side  clearance  allowance  ( full-size  inches )',
      dv_clearance, True, False, False, False); // no neg, preset ok, zero ok, don't terminate on zero.
    n := putdim(helpdv_pos_str, 1, 'vehicle  position  on  template',
      dv_start * inscale, False, False, False, False);
    // neg ok, preset ok, zero ok, don't terminate on zero.

    if n <> 4 then
      EXIT;

    if getdims('dummy  vehicle  dimensions', helpdv_str, grid_form, n, od) = True then begin
      dv_length := od[0];
      if dv_length = def_req then
        dv_length := 780;        // body length 780" = 65ft

      dv_width := od[1];
      if dv_width = def_req then
        dv_width := 111;          // body width 111" = 9ft-3in

      dv_wheelbase := od[2];
      if dv_wheelbase = def_req then
        dv_wheelbase := 558;  // wheelbase / bogie centres 558" = 46ft-6in

      dv_clearance := od[3];
      if dv_clearance = def_req then
        dv_clearance := 6;    // 6" clearance each side

      if od[4] = def_req then
        dv_start := 111              // to first axle/bogie-pin from CTRL-0  111" = 9ft-3in
      else
        dv_start := od[4] / inscale;

      redraw(False);   // see results

      if dv_copies_index > -1    // any copies?
      then begin
        if alert(4, 'php/920    delete  all  existing  copies ?',
          'Do you want to delete all existing copies of the dummy vehicle?'
          +
          '||rp.gif  If you answer no, any dummy vehicle copies will retain their previous dimensions.'
          +
          ' This may produce confusing results when testing clearances, if you have now changed the current dummy vehicle dimensions.',
          '', '', '', '', 'no  -  leave  existing  dummy  vehicle  copies',
          'yes  -  delete  all  existing  dummy  vehicle  copies', 0) = 6 then
          dummy_vehicle_clear_copies_button.Click;
      end;

    end;
  end;//with

  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.turnout_road_dummy_vehicle_radio_buttonClick(Sender: TObject);

begin
  show_dummy_vehicles_radio_button.Checked := True;  // make sure showing
  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.main_road_dummy_vehicle_radio_buttonClick(Sender: TObject);

begin
  show_dummy_vehicles_radio_button.Checked := True;  // make sure showing
  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.hide_dummy_vehicles_radio_buttonClick(Sender: TObject);

begin
  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.show_dummy_vehicles_radio_buttonClick(Sender: TObject);

begin
  do_rollback := False;
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.roll_dummy_vehicle_buttonClick(Sender: TObject);   // 0.98.a

begin
  show_dummy_vehicles_radio_button.Checked := True;  // make sure showing
  pad_form.roll_dummy_vehicle_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tgrid_form.dummy_vehicle_clearance_buttonClick(Sender: TObject);

begin
  show_dummy_vehicles_radio_button.Checked := True;            // make sure showing
  pad_form.adjust_dummy_vehicle_clearance_menu_entry.Click;  // 0.98.a  dummy vehicle
end;
//______________________________________________________________________________

procedure Tgrid_form.dummy_vehicle_clear_copies_buttonClick(Sender: TObject);

begin
  dv_copies_index := 0 - 1;      // -1 = no copies

  dummy_vehicle_delete_copy_button.Enabled := False;
  dummy_vehicle_clear_copies_button.Enabled := False;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.dummy_vehicle_delete_copy_buttonClick(Sender: TObject);

begin
  if dv_copies_index < 0 then
    EXIT;    // -1 = no copies
  Dec(dv_copies_index);
  if dv_copies_index < 0 then begin
    dummy_vehicle_delete_copy_button.Enabled := False;
    dummy_vehicle_clear_copies_button.Enabled := False;
  end;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.dummy_vehicle_make_copy_buttonClick(Sender: TObject);

begin

  if dv_copies_index > (dv_copies_c - 1) then begin
    ShowMessage('Dummy vehicle copies - limit reached.' + #13 + #13 +
      'To make another copy you must first delete one or more existing copies,'
      + #13 + 'or click the "clear all" button to delete all of them.');
    EXIT;
  end;

  Inc(dv_copies_index);                // update count of copies.

  //  corners for copy ...

  dv_copies[dv_copies_index] := dv_corners_calc;   // x dims

  with dv_copies[dv_copies_index] do begin       // modify y dims for bgnd...
    pt1.y := pt1.y * hand_i + y_datum;
    pt2.y := pt2.y * hand_i + y_datum;
    b1.y := b1.y * hand_i + y_datum;
    b2.y := b2.y * hand_i + y_datum;
    b3.y := b3.y * hand_i + y_datum;
    b4.y := b4.y * hand_i + y_datum;
    c1.y := c1.y * hand_i + y_datum;
    c2.y := c2.y * hand_i + y_datum;
    c3.y := c3.y * hand_i + y_datum;
    c4.y := c4.y * hand_i + y_datum;
    m1.y := m1.y * hand_i + y_datum;
    m2.y := m2.y * hand_i + y_datum;
    o1.y := o1.y * hand_i + y_datum;
    o2.y := o2.y * hand_i + y_datum;
    o3.y := o3.y * hand_i + y_datum;
    o4.y := o4.y * hand_i + y_datum;
  end;//with

  dummy_vehicle_delete_copy_button.Enabled := True;
  dummy_vehicle_clear_copies_button.Enabled := True;

  do_rollback := False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

procedure draw_dummy_vehicle_copies(on_canvas: TCanvas);    // 0.98.a

var
  n, pin_dim: integer;

  move_to, line_to, pin1, pin2: TPoint;


  /////////////////////////////////////////////////////////

  function dvc_mm_to_pixels(p: Tpex): TPoint;

  begin
    Result.X := Round(p.x * fx + ex - gx);
    Result.Y := Round(p.y * fy + by - gy);
  end;
  /////////////////////////////////////////////////////////

  procedure draw_dvc_line(p1, p2: Tpex);

  begin
    move_to := dvc_mm_to_pixels(p1);
    line_to := dvc_mm_to_pixels(p2);

    // draw line between them ...

    if check_limits(move_to, line_to) = True then
    begin
      on_canvas.MoveTo(move_to.X, move_to.Y);
      on_canvas.LineTo(line_to.X, line_to.Y);
    end;

  end;
  /////////////////////////////////////////////////////////

begin

  if grid_form.show_dummy_vehicles_radio_button.Checked = False then
    EXIT;   // turned off

  if dv_copies_index < 0 then
    EXIT;  // no copies exist

  with on_canvas do begin

    for n := 0 to dv_copies_index do begin

      Pen.Width := 1;
      Pen.Color := ring_copy_colour;

      with dv_copies[n] do begin

        // b1 - b2 - b3 - b4  clockwise corners from bottom left ...

        Pen.Style := psSolid;
        Brush.Color := bgkeep_timber_colour;
        Brush.Style := bsDiagCross;

        // (check_limits omitted ...)

        Polygon([dvc_mm_to_pixels(b1), dvc_mm_to_pixels(b2), dvc_mm_to_pixels(
          b3), dvc_mm_to_pixels(b4)]);


        Brush.Color := paper_colour;  // restore Brush for gaps in dotted lines.
        Brush.Style := bsSolid;

        // clearance extensions c1 - c2 - c3 - c4  clockwise corners from bottom left ...

        Pen.Style := psDash;        // clearance sides ...

        draw_dvc_line(c2, c3);
        draw_dvc_line(c4, c1);

        Pen.Style := psSolid;       // clearance corners ...

        draw_dvc_line(b2, c2);
        draw_dvc_line(c3, b3);
        draw_dvc_line(b4, c4);
        draw_dvc_line(c1, b1);

        draw_dvc_line(m1, m2);      // mid-line


        // draw line between bogie pins ...

        Pen.Style := psSolid;

        draw_dvc_line(pt1, pt2);

        // draw bogie pins ...

        pin1 := dvc_mm_to_pixels(pt1);
        pin2 := dvc_mm_to_pixels(pt2);

      end;//with dv_copies

      pin_dim := 7;                                                   // pin blob radius arbitrary.
      if pin_dim > Round(scale * fx / 2) then
        pin_dim := Round(scale * fx / 2); // but not more than 6" scale radius.
      if pin_dim < 1 then
        pin_dim := 1;                                 // and not less than this.

      // 1st pin ...

      Brush.Color := Pen.Color;
      Brush.Style := bsSolid;   // for pins

      move_to.X := pin1.X - pin_dim;
      move_to.Y := pin1.Y - pin_dim;
      line_to.X := pin1.X + pin_dim;
      line_to.Y := pin1.Y + pin_dim;

      if check_limits(move_to, line_to) = True then
        Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);

      // 2nd pin ...

      move_to.X := pin2.X - pin_dim;
      move_to.Y := pin2.Y - pin_dim;
      line_to.X := pin2.X + pin_dim;
      line_to.Y := pin2.Y + pin_dim;

      if check_limits(move_to, line_to) = True then
        Ellipse(move_to.X, move_to.Y, line_to.X, line_to.Y);

      Brush.Color := paper_colour;  // restore Brush for gaps in dotted lines.

    end;//next n
  end;//with on_canvas
end;
//______________________________________________________________________________

procedure Tgrid_form.attach_ring_checkboxClick(Sender: TObject);    // 206b

begin
  if attach_ring_checkbox.Checked = True then begin
    cancel_adjusts(False);

    show_spacing_rings := True;      // otherwise won't see it (no cursor).
    ring_has_been_shown := True;

    show_dummy_vehicles_radio_button.Checked := True;  // make sure vehicle showing

    gocalc(2, 0);
    // force re-draw even though control template hasn't changed (to show attached ring).

    do_rollback := False;
    redraw(True);
  end;
end;
//______________________________________________________________________________

procedure Tgrid_form.adjust_adjacent_centres_ts_buttonClick(Sender: TObject);

begin
  pad_form.adjust_adjacent_centres_ts_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tgrid_form.adjust_adjacent_centres_ms_buttonClick(Sender: TObject);

begin
  pad_form.adjust_adjacent_centres_ms_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tgrid_form.reset_centre_line_buttonClick(Sender: TObject);

begin
  cancel_adjusts(False);
  pad_form.centre_line_option_normal_menu_entry.Click;
end;
//______________________________________________________________________________

procedure Tgrid_form.vehicle_envelope_buttonClick(Sender: TObject);    // 215c

var
  end_pos: extended;       // full-size inches
  save_start: extended;

  n: integer;

begin

  if dv_envelopes.Count >= dv_envelopes_c then begin
    ShowMessage('Unable to create more than ' + IntToStr(dv_envelopes_c + 1) +
      ' outline envelopes.' + #13 + #13 + 'Delete one or more existing enelopes first.');
    EXIT;
  end;

  show_dummy_vehicles_radio_button.Checked := True;  // make sure showing

  dv_envelopes.Add(cdvi.generate_envelope(inscale, centre_line_path));

  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.clear_first_envelope_buttonClick(Sender: TObject);      // 215c

var
  n: integer;

begin
  if dv_envelopes.Count = 0 then begin
    ShowMessage('There are no envelopes to delete.');
    EXIT;
  end;

  dv_envelopes.Delete(0);
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.clear_last_envelope_buttonClick(Sender: TObject);

begin
  if dv_envelopes.Count = 0 then begin
    ShowMessage('There are no envelopes to delete.');
    EXIT;
  end;

  dv_envelopes.Delete(dv_envelopes.Count - 1);
  redraw(True);
end;
//______________________________________________________________________________

procedure Tgrid_form.clear_all_envelopes_buttonClick(Sender: TObject);

begin
  if dv_envelopes.Count = 0 then begin
    ShowMessage('There are no envelopes to delete.');
    EXIT;
  end;

  dv_envelopes.Clear;
  redraw(True);
end;
//______________________________________________________________________________

procedure draw_dummy_vehicle_outline_envelope_as_polygon(on_canvas: TCanvas);   // 215c

var
  i, n: integer;

  ////////////////////////////////////////////////////////

  function dvo_mm_to_pixels(p: Tpex): TPoint;

  begin
    Result.X := Round(p.x * fx + ex - gx);
    Result.Y := Round(p.y * fy + by - gy);
  end;
  ////////////////////////////////////////////////////////

begin
  if grid_form.show_dummy_vehicles_radio_button.Checked = False then
    EXIT;   // turned off

  if dv_envelopes.Count = 0 then
    EXIT;  // no envelopes exist

  with on_canvas do begin

    for i := 0 to dv_envelopes.Count - 1 do begin

      with dv_envelopes[i] do begin

        if Length(dv_outlines) = 0 then
          CONTINUE;  // ??? no outlines exist at this index

        Pen.Width := 1;
        Pen.Style := psSolid;
        Brush.Style := bsSolid;

        Pen.Color := ring_copy_colour;
        Brush.Color := ring_copy_colour;

        for n := 0 to High(dv_outlines) do begin

          with dv_outlines[n] do begin

            // c1 - c2 - c3 - c4  clearance, clockwise corners from bottom left ...

            Polygon([dvo_mm_to_pixels(c1), dvo_mm_to_pixels(c2), dvo_mm_to_pixels(
              c3), dvo_mm_to_pixels(c4)]);

          end;//with
        end;//next


        Pen.Color := clYellow;
        Brush.Color := clYellow;

        for n := 0 to High(dv_outlines) do begin

          with dv_outlines[n] do begin

            // b1 - b2 - b3 - b4  body, clockwise corners from bottom left ...

            Polygon([dvo_mm_to_pixels(b1), dvo_mm_to_pixels(b2), dvo_mm_to_pixels(
              b3), dvo_mm_to_pixels(b4)]);

          end;//with
        end;//next

        // blank overdraw at start and end...

        if grid_form.blank_ends_checkbox.Checked = True then begin
          Pen.Width := 3;                // ensure overdraw ends
          Pen.Color := paper_colour;
          Brush.Color := paper_colour;

          Polygon([dvo_mm_to_pixels(dv_outlines[0].o1), dvo_mm_to_pixels(
            dv_outlines[0].o2), dvo_mm_to_pixels(dv_outlines[0].o3), dvo_mm_to_pixels(dv_outlines[0].o4)]);

          Polygon([dvo_mm_to_pixels(dv_outlines[High(dv_outlines)].o1),
            dvo_mm_to_pixels(dv_outlines[High(dv_outlines)].o2), dvo_mm_to_pixels(
            dv_outlines[High(dv_outlines)].o3), dvo_mm_to_pixels(dv_outlines[High(dv_outlines)].o4)]);
        end;
      end;//with
    end;//next i
  end;//with canvas

  draw_screengrid(on_canvas);   // reinstate the grid over envelopes and blanked ends

end;
//______________________________________________________________________________

initialization

  pen_now_at.x := 0;
  pen_now_at.y := 0;
  //________________________________________________________________________________________

end.
