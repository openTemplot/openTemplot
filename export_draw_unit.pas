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

unit export_draw_unit;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;

//______________________________________________________________________________

var

  unit_dummy:integer;    // keep compiler happy

  procedure export_draw(on_canvas:TCanvas; canvas_width,canvas_height,output_code:integer);    // draw control template or entire pad on on_canvas

  procedure export_bgnd_shapes(on_canvas:TCanvas; canvas_height:integer; grid_left,grid_top:extended; output_code:integer);  // export all background shapes.

  procedure export_bgnd(on_canvas:TCanvas; canvas_height:integer; grid_left,grid_top:extended; output_code:integer);         // export background templates.


//______________________________________________________________________________

implementation


uses
  control_room, pad_unit, math_unit, export_unit, print_settings_unit, preview_unit, print_unit, bgnd_unit;


//______________________________________________________________________________

function calc_intensity(colour:integer):integer;

var
  red,green,blue,av:integer;    // colour components (0-255).

begin
  RESULT:=colour;  // default init

  if colour=clWhite then EXIT;

  if (export_black_white=True) or (colour=virtual_black_colour)
     then begin
            RESULT:=clBlack;
            EXIT;
          end;

  if export_grey_shade=True     // average to a grey..
     then begin
            try
              red:=(colour AND $000000FF);
              green:=(colour AND $0000FF00) div $100;
              blue:=(colour AND $00FF0000) div $10000;

              av:=(red+green+blue) div 3;

              red:=av;
              green:=av;
              blue:=av;

              RESULT:=(blue*$10000)+(green*$100)+red;

            except
              RESULT:=clGray;
            end;//try
          end;
end;
//____________________________________________________________________________________

procedure print_colours_setup;  // set grey shades and/or print intensity.

begin
  if export_black_white=True
     then begin
            print_railedge_colour:=clBlack;

            printcurail_colour:=clBlack;
            printbgrail_colour:=clBlack;

            printtimber_colour:=clBlack;

            printrail_infill_colour_cu:=clWhite;      // !!! used for solid infill.
            printrail_infill_colour_bg:=clWhite;      // !!! used for solid infill.

            printtimber_infill_colour:=clBlack;       // for hatched infill
            printgrid_colour:=clBlack;
            printmargin_colour:=clBlack;
            printguide_colour:=clBlack;
            printjoint_colour:=clBlack;
            printalign_colour:=clBlack;

            print_labels_font.Color:=clBlack;
            print_timber_numbers_font.Color:=clBlack;

            printshape_colour:=clBlack;
            printbg_single_colour:=clBlack;

            printplat_edge_colour:=clBlack;    // platform edges
            printplat_infill_colour:=clBlack;  // platform infill - hatched?

            sb_track_bgnd_colour:=clWhite;     // 206a
            sb_diagram_colour:=clWhite;        // 209c

          end
     else begin
                     //  calc grey shades if wanted...

            print_railedge_colour:=calc_intensity(save_prc);

            printcurail_colour:=print_railedge_colour;
            printbgrail_colour:=print_railedge_colour;

            printtimber_colour:=calc_intensity(save_ptc);

            printrail_infill_colour_cu:=calc_intensity(save_priccu);
            printrail_infill_colour_bg:=calc_intensity(save_pricbg);

            printtimber_infill_colour:=calc_intensity(save_ptic);
            printgrid_colour:=calc_intensity(save_grc);
            printmargin_colour:=calc_intensity(save_pmc);
            printguide_colour:=calc_intensity(save_pgc);
            printjoint_colour:=calc_intensity(save_pjc);
            printalign_colour:=calc_intensity(save_pac);

            print_labels_font.Color:=calc_intensity(save_fc);
            print_timber_numbers_font.Color:=calc_intensity(save_tnfc);

            print_corner_page_numbers_font.Color:=calc_intensity(save_cpnfc);     // 0.93.a added

            printshape_colour:=calc_intensity(save_psc);
            printbg_single_colour:=calc_intensity(save_pbg);

            printplat_edge_colour:=calc_intensity(save_priplatedge);   // platform edges
            printplat_infill_colour:=calc_intensity(save_priplatfill); // platform infill

            sb_track_bgnd_colour:=calc_intensity(save_sb_track_bgnd);  // 206a
            sb_diagram_colour:=calc_intensity(save_sb_diagram_col);    // 209c

          end;
end;
//__________________________________________________________________________________________

procedure print_line_thickness_setup;

var
  av_dpi:extended;

                 ///////////////////////////////////////////////

                 function calc_thick(mm_thick:extended; adjust:boolean):integer;  // calc line thickness in dots.

                 var
                   line_dots:extended;

                 begin
                   line_dots:=mm_thick*av_dpi/25.4;
                   if (adjust=True) and (pad_form.adjust_line_thickness_menu_entry.Checked=True) then line_dots:=line_dots*out_factor;

                   RESULT:=Round(line_dots);
                   if RESULT<1 then RESULT:=1;
                 end;
                 ///////////////////////////////////////////////

begin
  av_dpi:=(nom_width_dpi+nom_length_dpi)/2;

     printgrid_wide:=calc_thick(  printgrid_thick,False);  // don't reduce line thickness for scaled output - the paper size is still the same.
   printmargin_wide:=calc_thick(printmargin_thick,False);  // ditto.

        printshape_wide:=calc_thick(    printshape_thick,True);
    printpicborder_wide:=calc_thick(printpicborder_thick,True);
         printrail_wide:=calc_thick(     printrail_thick,True);
       printtimber_wide:=calc_thick(   printtimber_thick,True);
           printcl_wide:=calc_thick(       printcl_thick,True);    // 0.79.a
         printmark_wide:=calc_thick(     printmark_thick,True);

end;
//______________________________________________________________________________

procedure swap_line_to(on_canvas:TCanvas; canvas_height,Xin,Yin:integer);

    // swap X and Y for drawing on sketchboard instead of printer...

//var
//  bitmap_height:integer;

begin
  //bitmap_height:=dtp_track_bitmap.Height;
  //dtp_track_bitmap.Canvas.LineTo(Yin,bitmap_height-Xin);
  on_canvas.LineTo(Yin,canvas_height-Xin);
end;
//____________________________

procedure swap_move_to(on_canvas:TCanvas; canvas_height,Xin,Yin:integer);

//var
//  bitmap_height:integer;

begin
  //bitmap_height:=dtp_track_bitmap.Height;
  //dtp_track_bitmap.Canvas.MoveTo(Yin,bitmap_height-Xin);
  on_canvas.MoveTo(Yin,canvas_height-Xin);
end;
//____________________________

procedure swap_text_out(on_canvas:TCanvas; canvas_height,Xin,Yin:integer; str:string);

//var
//  bitmap_height:integer;

begin
  //bitmap_height:=dtp_track_bitmap.Height;
  //dtp_track_bitmap.Canvas.TextOut(Yin,bitmap_height-Xin,str);
  on_canvas.TextOut(Yin,canvas_height-Xin,str);
end;
//____________________________

procedure swap_rectangle(on_canvas:TCanvas; canvas_height,X1in,Y1in,X2in,Y2in:integer);

//var
//  bitmap_height:integer;

begin
  //bitmap_height:=dtp_track_bitmap.Height;
  //dtp_track_bitmap.Canvas.Rectangle(Y1in, bitmap_height-X2in, Y2in, bitmap_height-X1in);
  on_canvas.Rectangle(Y1in, canvas_height-X2in, Y2in, canvas_height-X1in);
end;
//____________________________

procedure swap_polygon(on_canvas:TCanvas; canvas_height:integer; corners: array of TPoint);

var
  //bitmap_height:integer;
  n:integer;
  Xin,Yin:integer;

begin
  //bitmap_height:=dtp_track_bitmap.Height;

  for n:=Low(corners) to High(corners) do begin
    Xin:=corners[n].X; Yin:=corners[n].Y;
    corners[n].X:=Yin; corners[n].Y:=canvas_height-Xin;
  end;//next corner

  on_canvas.Polygon(corners);
end;
//______________________________

procedure swap_ellipse(on_canvas:TCanvas; canvas_height,X1in,Y1in,X2in,Y2in:integer);

//var
//  bitmap_height:integer;

begin
  //bitmap_height:=dtp_track_bitmap.Height;
  //dtp_track_bitmap.Canvas.Ellipse(Y1in, bitmap_height-X2in, Y2in, bitmap_height-X1in);
  on_canvas.Ellipse(Y1in, canvas_height-X2in, Y2in, canvas_height-X1in);
end;
//____________________________


//__________________________________________________________________________________________

procedure do_text_out(on_canvas:TCanvas; canvas_height,textoutX,textoutY:integer; str:string);

           // blank text backgrounds, swapped X,Y for sketchboard

var
  text_rect:TRect;
  //bitmap_height:integer;

begin
  //bitmap_height:=dtp_track_bitmap.Height;

  with on_canvas do begin

    text_rect.Left:=textoutY;
    text_rect.Top:=canvas_height-textoutX;
    text_rect.Right:=text_rect.Left+TextWidth(str);
    text_rect.Bottom:=text_rect.Top+TextHeight(str);

    Brush.Color:=clWhite;
    Brush.Style:=bsSolid;

    FillRect(text_rect);
  end;//with

  swap_text_out(on_canvas,canvas_height,textoutX,textoutY,str);
end;
//______________________________________________________________________________

procedure do_export_draw(on_canvas:TCanvas; canvas_width,canvas_height,output_code:integer);    // draw control template or entire pad on a bitmap or metafile.

   // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

var
  infill_points:array[0..3] of TPoint;   // array of corners for infilled timbers.

  gridx, gridy, now_gridx, now_gridy:extended;
  grid_label:extended;

  grid_now_dots:integer;

  aq, rail, now, dots_index, mark_code:integer;

  p1,p2,p3,p4:Tpoint;
  radcen_arm:extended;

  move_to, line_to: TPoint;

  l_dims_valid:boolean;
  w_dims_valid:boolean;

  x_dots, y_dots:integer;
  bangrid_dots:integer;

  pen_width:integer;

  ptr_1st,ptr_2nd:^Tmark;     // pointers to a Tmark record
  markmax:integer;

  fontsize:extended;
  num_str, tbnum_str:string;

  grid_str:string;        // grid units
  grid_label_str:string;

  img_bgnd_colour:TColor;  // 206a

              //////////////////////////////////////////////////////////////////

              procedure draw_marks(grid_left,grid_top:extended; rail_joints:boolean);

                // if rail_joints=True draw only the rail joints, otherwise omit them.

              var
                i:integer;

              begin
                markmax:=High(marks_list_ptr);  // max index for the present list.

                if mark_index>markmax then mark_index:=markmax;  // ??? shouldn't be.

                tbnum_str:=timb_numbers_str;   // the full string of timber numbering for the control template.

                with on_canvas do begin

                  for i:=0 to (mark_index-1) do begin   // (mark_index is always the next free slot)

                    ptr_1st:=@marks_list_ptr[i];  // pointer to the next Tmark record.
                    if ptr_1st=nil then BREAK;

                    mark_code:=ptr_1st^.code;              // check this mark wanted.

                    if mark_code=0 then CONTINUE;     // ignore mark entries with code zero (might be the second or third of a multi-mark entry, e.g. for timber infill).

                    if print_settings_form.output_rail_joints_checkbox.Checked=False    // 223d
                       then begin
                              case mark_code of
                                 6: CONTINUE;     // rail joints not wanted.
                              end;//case
                            end;

                            // overwrite rail joints on rails..

                    if rail_joints=(mark_code<>6) then CONTINUE;  // do only the rail joints if rail_joints=True and ignore them otherwise.

                    if print_settings_form.output_timbering_checkbox.Checked=False
                       then begin
                              case mark_code of
                                 3,4,5,14,33,44,54,55,93,95,99,203,233,293: CONTINUE;     // no timbering wanted.
                              end;//case
                            end;

                    if print_settings_form.output_timber_centres_checkbox.Checked=False    // 223d
                       then begin
                              case mark_code of
                                 4,14,44,54: CONTINUE;     // timber centre-lines not wanted.
                              end;//case
                            end;

                    if print_settings_form.output_guide_marks_checkbox.Checked=False    // 223d
                       then begin
                              case mark_code of
                                 1: CONTINUE;     // guide marks not wanted.
                              end;//case
                            end;

                    if print_settings_form.output_switch_drive_checkbox.Checked=False    // 223d
                       then begin
                              case mark_code of
                                 101: CONTINUE;     // switch drive not wanted.
                              end;//case
                            end;

                   if print_settings_form.output_chairs_checkbox.Checked=False
                       then begin
                              case mark_code of
                                 480..499: CONTINUE;     // no chair outlines wanted  221a
                              end;//case
                            end;

                    if print_settings_form.output_radial_ends_checkbox.Checked=False
                       then begin
                              case mark_code of
                                 2,7: CONTINUE;     // no radial ends wanted  206a
                              end;//case
                            end;

                    if ((mark_code=203) or (mark_code=233) or (mark_code=293)) and (i<(mark_index-1))      // timber infill
                       then begin
                              ptr_2nd:=@marks_list_ptr[i+1];        // pointer to the second infill Tmark record.
                              if ptr_2nd=nil then BREAK;

                              p1:=ptr_1st^.p1;              // x1,y1 in  1/100ths mm
                              p2:=ptr_1st^.p2;              // x2,y2 in  1/100ths mm
                              p3:=ptr_2nd^.p1;              // x3,y3 in  1/100ths mm
                              p4:=ptr_2nd^.p2;              // x4,y4 in  1/100ths mm
                            end
                       else ptr_2nd:=nil;    // keep compiler happy.

                    if (mark_code>0) and (mark_code<200) and (mark_code<>8) and (mark_code<>9) and (mark_code<>10) // ignore peg, rad centres, timber selector and peg arms, plain track start, label.
                       then begin

                              if ((mark_code=5) or (mark_code=55) or (mark_code=95)) and (out_factor<>1.0) then CONTINUE;   // reduced ends are meaningless if not full-size.

                              p1:=ptr_1st^.p1;              // x1,y1 in  1/100ths mm

                              if mark_code<>99
                                 then begin
                                        p2:=ptr_1st^.p2;    // x2,y2 in  1/100ths mm

                                        Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                                        Brush.Style:=bsClear;
                                        TextOut(0,0,'');

                                        if export_black_white=True
                                           then Pen.Color:=clBlack
                                           else case mark_code of
                                                       1,101: Pen.Color:=printguide_colour;  // guide marks. switch drive
                                                           2: Pen.Color:=printalign_colour;  // rad end marks.
                                                     3,33,93: Pen.Color:=printtimber_colour; // timber outlines.
                                                           6: Pen.Color:=printjoint_colour;  // rail joint marks.
                                                           7: Pen.Color:=printalign_colour;         // transition/slewing ends.
                                                         else Pen.Color:=calc_intensity(clBlack);   // thin dotted lines in black only.
                                                end;//case

                                        Pen.Mode:=pmCopy;
                                        Pen.Width:=1;
                                        Pen.Style:=psSolid; // default init.

                                        case mark_code of
                                           1,101: Pen.Width:=printmark_wide;    // guide marks.   switch drive
                                               2: Pen.Width:=printmark_wide;    // rad end marks.
                                         3,33,93: Pen.Width:=printtimber_wide;  // timber outlines.

                                            4,44: Pen.Style:=psDash;    // timber centre-lines.

                                         5,55,95: Pen.Style:=psDot;                   // timber reduced ends.

                                               6: Pen.Width:=printmark_wide;    // rail joint marks.
                                               7: Pen.Width:=printmark_wide;    // transition ends.

                                           14,54: Pen.Width:=printrail_wide;  // timber centre-lines with rail centre-lines (for rivet locations?).

                                             else begin
                                                    Pen.Width:=1;
                                                    Pen.Style:=psSolid;
                                                  end;

                                        end;//case

                                           // overides...

                                        // out 0.73.a 12-8-01 (now done in thickness setup) if out_factor<1 then Pen.Width:=Round(Pen.Width*out_factor); // scale down the line width.

                                        if Pen.Width<1 then Pen.Width:=1;
                                        if Pen.Style<>psSolid then Pen.Width:=1;   // delphi bug? (patterns only work for lines 1 dot wide.)
                                        // pdf if impact>0 then Pen.Width:=1;      // overide for impact printer or plotter.

                                        move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                        line_to.X:=Round((p2.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;
                                        if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;

                                      end;
                                               // (no timber numbering here for exports)

                            end
                       else begin   // other codes...

                              if  ( (mark_code=-2) or (mark_code=-3) )
                              and (print_settings_form.output_radial_centres_checkbox.Checked=True)

                                         // draw curving rad centres...

                                 then begin
                                        Pen.Width:=printmark_wide;  // guide marks.

                                        if Pen.Width<1 then Pen.Width:=1;

                                        Pen.Style:=psSolid;
                                        Pen.Mode:=pmCopy;
                                        Pen.Color:=calc_intensity(clBlack);

                                        p1:=ptr_1st^.p1;        // x1,y1 in  1/100ths mm
                                        radcen_arm:=400*scale;  // 4ft scale arbitrary (scale is for control template).

                                        move_to.X:=Round((p1.Y+radcen_arm+ypd-grid_left)*scaw_out)+page_left_dots;      // mark centre widthwise.
                                        move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                        line_to.X:=Round((p1.Y-radcen_arm+ypd-grid_left)*scaw_out)+page_left_dots;
                                        line_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;
                                        if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;

                                        move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;                 // mark centre lengthwise
                                        move_to.Y:=Round((p1.X+radcen_arm-grid_top)*scal_out)+page_top_dots;

                                        line_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        line_to.Y:=Round((p1.X-radcen_arm-grid_top)*scal_out)+page_top_dots;
                                        if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                      end;

                              if ((mark_code=203) or (mark_code=233) or (mark_code=293))  and (ptr_2nd<>nil)        // timber infill...
                                 then begin
                                        infill_points[0].X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[0].Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                        infill_points[1].X:=Round((p2.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[1].Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;

                                        infill_points[2].X:=Round((p3.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[2].Y:=Round((p3.X-grid_top)*scal_out)+page_top_dots;

                                        infill_points[3].X:=Round((p4.Y+ypd-grid_left)*scaw_out)+page_left_dots;
                                        infill_points[3].Y:=Round((p4.X-grid_top)*scal_out)+page_top_dots;

                                        if (check_limits(infill_points[0],infill_points[1])=True) and (check_limits(infill_points[2],infill_points[3])=True)
                                           then begin
                                                  Pen.Width:=1;
                                                  Pen.Style:=psSolid;
                                                  Pen.Mode:=pmCopy;

                                                  Pen.Color:=clWhite;  // so no overdrawing of timber outlines.

                                                  if export_black_white=True
                                                     then Brush.Color:=clBlack
                                                     else Brush.Color:=printtimber_infill_colour;

                                                  case print_timb_infill_style of
                                                                  0: CONTINUE;
                                                                  1: Brush.Style:=bsFDiagonal;           // hatched. Forward diagonal for the foreground (control template).
                                                                  2: Brush.Style:=bsDiagCross;

                                                                  3: if export_black_white=True
                                                                        then CONTINUE  // 209c now no fill   was Brush.Style:=bsFDiagonal
                                                                        else Brush.Style:=bsSolid;

                                                                  4: begin                         // blank.
                                                                       Brush.Style:=bsSolid;
                                                                       Brush.Color:=clWhite;       // overide.
                                                                     end;
                                                                else CONTINUE;
                                                  end;//case

                                                  swap_polygon(on_canvas,canvas_height,infill_points);
                                                end;
                                      end;

                            end;
                  end;//next mark i
                end;//with on_canvas
              end;
              //////////////////////////////////////////////////////////////////

              function get_w_dots(q,n:integer):integer;

              begin
                with sheet[0,0] do begin // (grid_left)
                  RESULT:=Round((outoflist(q,n,1)+ypd-grid_left)*scaw_out)+page_left_dots;
                end;//with
                w_dims_valid:=check_draw_dim_w(RESULT);
              end;
              //////////////////////////////////////////////////////////////////

              function get_l_dots(q,n:integer):integer;

              begin
                with sheet[0,0] do begin // (grid_top)
                  RESULT:=Round((outoflist(q,n,0)-grid_top)*scal_out)+page_top_dots;
                end;//with
                l_dims_valid:=check_draw_dim_l(RESULT);
              end;
              //////////////////////////////////////////////////////////////////

              procedure draw_outline_railedge(aq,pencol:integer);

              var
                now:integer;

              begin
                if ( (plain_track=False) or (aq=0) or (aq=8) or (aq=3) or (aq=11) or ((aq>15) and (aq<24)) ) and (aqyn[aq]=True)

                   then begin
                          with on_canvas do begin
                            Pen.Color:=pencol;
                            Pen.Mode:=pmCopy;
                            Pen.Style:=psSolid;

                            move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                            for now:=1 to nlmax_array[aq] do begin
                              line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                              if check_limits(move_to, line_to)=True
                                 then begin
                                        swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                        swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                      end;
                              move_to:=line_to;
                            end;//for
                          end;//with on_canvas
                        end;
              end;
              ////////////////////////////

              procedure draw_fill_rail(outer_add:integer);    // draw a complete filled rail.

              const
                dots_max_c=xy_pts_c*2;     // 6000 max      xy_pts_c=3000

              var
                dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                  //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                  //   template max is 4500' scale length.
                  //   ( = 18000 mm or 59ft approx in 4 mm scale).
                  //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                  // N.B. huge standard Pascal array is used instead of our integer arrays,
                  // because needed for the Polygon function.

                  // total memory = 6000*8 bytes = 48KB

                now, start, now_max:integer;
                edge_started:boolean;
                mid_dots_index:integer;
                edge_colour, blanking_colour:integer;


                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure modify_rail_end(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with on_canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,move_to.X, move_to.Y);

                                                          swap_move_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


              begin
                with on_canvas do begin

                  if (rail=16) or (rail=20)                    // 0.93.a platforms
                     then Pen.Color:=printplat_edge_colour
                     else Pen.Color:=printcurail_colour;

                  Pen.Mode:=pmCopy;
                  Pen.Style:=psSolid;

                  aq:=rail;  // gauge-faces.

                  if ( (plain_track=False) or (aq=0) or (aq=3) or ((aq>15) and (aq<24)) )

                         // if plain track, stock rails and adjacent tracks only.

                     then begin
                            if (aqyn[aq]=False) or (aqyn[aq+outer_add]=False)  // data not for both edges?
                               then begin
                                      if aqyn[aq]=True then draw_outline_railedge(aq,Pen.Color);
                                      if aqyn[aq+outer_add]=True then draw_outline_railedge(aq+outer_add,Pen.Color);
                                      EXIT;
                                    end;

                            now_max:=nlmax_array[aq];

                            if gaunt=False    // 0.93.a normal turnout.
                               then begin

                                      case aq of
                                           1: begin
                                                start:=list_planing_mark_aq1;  // start from end of planing - no infill in planing.

                                                if (start<0) or (start>now_max) then EXIT;  // ???

                                              end;

                                           2: begin                         // ditto
                                                start:=list_planing_mark_aq2;

                                                if (start<0) or (start>now_max) then EXIT;  // ???

                                              end;

                                         else start:=0;                   // whole list.
                                      end;//case

                                    end
                               else start:=0;  // 093a gaunt template, no planing

                            dots_index:=0-1;   // first increment is to zero.

                            edge_started:=False;
                            for now:=start to now_max do
                              begin
                                x_dots:=get_w_dots(aq,now);
                                y_dots:=get_l_dots(aq,now);
                                if (w_dims_valid=True) and (l_dims_valid=True)
                                   then begin
                                          edge_started:=True;

                                          Inc(dots_index);
                                          if dots_index>dots_max_c then dots_index:=dots_max_c;

                                          dots[dots_index].X:=x_dots;
                                          dots[dots_index].Y:=y_dots;
                                        end
                                   else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                              end;//next now

                            mid_dots_index:=dots_index;

                            aq:=rail+outer_add;             // outer-edges.

                            now_max:=nlmax_array[aq];

                            edge_started:=False;
                            for now:=now_max downto 0 do
                              begin
                                x_dots:=get_w_dots(aq,now);
                                y_dots:=get_l_dots(aq,now);
                                if (w_dims_valid=True) and (l_dims_valid=True)
                                   then begin
                                          edge_started:=True;

                                          Inc(dots_index);
                                          if dots_index>dots_max_c then dots_index:=dots_max_c;

                                          dots[dots_index].X:=x_dots;
                                          dots[dots_index].Y:=y_dots;
                                        end
                                   else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                              end;//next now

                              if (rail=16) or (rail=20)   // 093a platforms
                                 then begin
                                        Brush.Color:=printplat_infill_colour;

                                        case print_platform_infill_style of
                                                0: Brush.Style:=bsClear;
                                                1: Brush.Style:=bsBDiagonal;    // hatched. backward diagonal (forward diagonal on control template timbers).
                                                2: Brush.Style:=bsDiagCross;

                                                3: if (export_black_white=True) or (mapping_colours_print<0)          // solid.
                                                      then Brush.Style:=bsBDiagonal    // impact printer or plotter, or printing black and white or in a single colour.
                                                      else Brush.Style:=bsSolid;

                                              else begin                         // 4 = blank.
                                                     Brush.Style:=bsSolid;
                                                     Brush.Color:=clWhite;       // overide.
                                                   end;

                                        end;//case
                                      end
                                 else begin
                                        if ((draw_ts_trackbed_cess_edge=True) and (rail=18))
                                        or ((draw_ms_trackbed_cess_edge=True) and (rail=22))   // 215a
                                           then begin
                                                  Brush.Color:=sb_track_bgnd_colour; // cess use same colour as track background
                                                  Brush.Style:=bsFDiagonal;
                                                end
                                           else begin   // normal rails...

                                                  Brush.Color:=printrail_infill_colour_cu;

                                                  case rail_infill_i of
                                                        1: Brush.Style:=bsBDiagonal;   // hatched
                                                        2: Brush.Style:=bsSolid;       // solid
                                                        3: Brush.Style:=bsDiagCross;   // cross_hatched
                                                        4: begin                       // blank
                                                             Brush.Style:=bsSolid;
                                                             Brush.Color:=clWhite;
                                                           end;
                                                      else Brush.Style:=bsSolid;       // ??? solid
                                                  end;//case
                                                end;
                                      end;

                            if dots_index>2
                               then begin
                                      swap_polygon(on_canvas,canvas_height,Slice(dots,dots_index+1));   // +1, number of points, not index.  must have 4 points.

                                      edge_colour:=Pen.Color;  // existing rail edges.

                                      if Brush.Style=bsSolid
                                         then blanking_colour:=Brush.Color       // infill colour.
                                         else begin   // 206b hatched fill...
                                                      // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

                                                case output_code of
                                                  1,2: blanking_colour:=Brush.Color;  // T3-FIRST dtp_settings_form.sb_page_colour_panel.Color;
                                                    3: blanking_colour:=export_form.img_bgnd_colour_panel.Color;
                                                    4: blanking_colour:=Brush.Color; // for metafile export
                                                 else  blanking_colour:=clWhite;  // assume white background (print, PDF)
                                                end;//case
                                              end;

                                            // remove polygon line across end of planing (not for fixed-diamond)..
                                            // (for gaunt template this removes the polygon line across the rail end)

                                      if ((half_diamond=False) or (fixed_diamond=False)) and ((rail=1) or (rail=2)) then modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                            // remove polygon lines across stock rail ends...
                                            // and trackbed ends  206b

                                      if (rail=0) or (rail=3) or (rail=18) or (rail=22)   // 18,22 added 206b
                                         then begin
                                                modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // toe or approach end.

                                                modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);  // exit end.
                                              end;

                                      if adjacent_edges=True  // platforms
                                       then begin

                                                   // 093a blank platform rear edges ...

                                              if (rail=16) and (draw_ts_platform=True) and (draw_ts_platform_rear_edge=False)   // 0.93.a TS platform start
                                                 then draw_outline_railedge(16,blanking_colour);    // blank rear edge

                                              if (rail=20) and (draw_ms_platform=True) and (draw_ms_platform_rear_edge=False)   // 0.93.a TS platform start
                                                 then draw_outline_railedge(20,blanking_colour);    // blank rear edge

                                                   // 093a blank platform ends ...

                                              if (rail=16) and (draw_ts_platform=True) and (draw_ts_platform_start_edge=False)   // 0.93.a TS platform start
                                                 then modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=16) and (draw_ts_platform=True) and (draw_ts_platform_end_edge=False)     // 0.93.a TS platform end
                                                 then modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform=True) and (draw_ms_platform_start_edge=False)   // 0.93.a MS platform start
                                                 then modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform=True) and (draw_ms_platform_end_edge=False)     // 0.93.a MS platform end
                                                 then modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                            end;

                                      if (rail=26) or (rail=28)
                                         then begin
                                                modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // centre of K-crossing check rails.
                                              end;

                                    end;
                          end;
                end;//with
              end;
              //////////////////////////////////////////////////////////////////

              procedure draw_fill_vee;    // do complete vee in one go ...

              const
                dots_max_c=xy_pts_c*2;    // 6000 max     xy_pts_c=3000

              var
                dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                  //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                  //   template max is 4500' scale length.
                  //   ( = 18000 mm or 59ft approx in 4 mm scale).
                  //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                  // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                  // because needed for the Polygon function.

                  // total memory = 6000*8 bytes = 48KB

                now:integer;
                edge_started:boolean;
                dots_index:integer;
                x_dots,y_dots:integer;
                aq:integer;
                point_mid_dots_index, splice_mid_dots_index:integer;
                edge_colour, blanking_colour:integer;

                              //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                              procedure modify_vee_end(start_index,stop_index,edge,blank:integer);

                              begin

                                if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                   then begin
                                          move_to:=dots[start_index];
                                          line_to:=dots[stop_index];

                                          if check_limits(move_to, line_to)=True
                                             then begin
                                                    with on_canvas do begin
                                                      Pen.Color:=blank;                // first blank across..
                                                      swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                      swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);

                                                      Pen.Color:=edge;                 // then restore the corner points..
                                                      swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                      swap_line_to(on_canvas,canvas_height,move_to.X, move_to.Y);

                                                      swap_move_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                      swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                    end;//with
                                                  end;
                                        end;
                              end;
                              //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


              begin
                if plain_track=True then EXIT;   // not if plain track.

                if (aqyn[4]=False) or (aqyn[5]=False) or (aqyn[12]=False) or (aqyn[13]=False)            // not enough data for filled vee.
                or (nlmax_array[4]=0) or (nlmax_array[5]=0) or (nlmax_array[12]=0) or (nlmax_array[13]=0)
                   then begin
                          if aqyn[4]=True then draw_outline_railedge(4,printcurail_colour);       // draw outline vee...
                          if aqyn[5]=True then draw_outline_railedge(5,printcurail_colour);
                          if aqyn[12]=True then draw_outline_railedge(12,printcurail_colour);
                          if aqyn[13]=True then draw_outline_railedge(13,printcurail_colour);
                        end
                   else begin
                          dots_index:=0-1;   // first increment is to zero.

                          aq:=4;
                          edge_started:=False;
                          for now:=0 to nlmax_array[aq] do    // vee main-side, gauge_face, start from the tip.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          point_mid_dots_index:=dots_index;

                          aq:=12;
                          edge_started:=False;
                          for now:=nlmax_array[aq] downto 0 do  // back along outer-edge.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          aq:=13;
                          edge_started:=False;
                          for now:=0 to nlmax_array[aq] do    // and then turnout side outer edge.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          splice_mid_dots_index:=dots_index;

                          aq:=5;
                          edge_started:=False;
                          for now:=nlmax_array[aq] downto 0 do  // and back along the gauge face to the tip.
                            begin
                              x_dots:=get_w_dots(aq,now);
                              y_dots:=get_l_dots(aq,now);
                              if (w_dims_valid=True) and (l_dims_valid=True)
                                 then begin
                                        edge_started:=True;

                                        Inc(dots_index);
                                        if dots_index>dots_max_c then dots_index:=dots_max_c;

                                        dots[dots_index].X:=x_dots;
                                        dots[dots_index].Y:=y_dots;
                                      end
                                 else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                            end;//next now

                          with on_canvas do begin

                            Pen.Color:=printcurail_colour;         //  1 = virtual black. Bug in HP driver if black (0) specified.
                                                                   //  (but not on "econofast" print !).
                            Pen.Mode:=pmCopy;
                            Pen.Style:=psSolid;

                            Brush.Color:=printrail_infill_colour_cu;

                            case rail_infill_i of
                                          1: Brush.Style:=bsBDiagonal;   // hatched
                                          2: Brush.Style:=bsSolid;       // solid
                                          3: Brush.Style:=bsDiagCross;   // cross_hatched
                                          4: begin                       // blank
                                               Brush.Style:=bsSolid;
                                               Brush.Color:=clWhite;
                                             end;
                                        else Brush.Style:=bsSolid;       // solid
                            end;//case

                            if dots_index>4
                               then begin
                                      swap_polygon(on_canvas,canvas_height,Slice(dots,dots_index+1));   // +1, number of points, not index.  must have at least 5 points.

                                      edge_colour:=Pen.Color;  // existing rail edges.

                                      if Brush.Style=bsSolid
                                         then blanking_colour:=Brush.Color   // infill colour.
                                         else begin   // 206b hatched fill...
                                                      // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

                                                case output_code of
                                                  1,2: blanking_colour:=Brush.Color;  // T3-FIRST  dtp_settings_form.sb_page_colour_panel.Color;
                                                    3: blanking_colour:=export_form.img_bgnd_colour_panel.Color;
                                                    4: blanking_colour:=Brush.Color; // for metafile export
                                                 else  blanking_colour:=clWhite;     // assume white background on paper (print, PDF)
                                                end;//case
                                              end;

                                            // remove polygon lines across vee rail ends...

                                      modify_vee_end(point_mid_dots_index,point_mid_dots_index+1,edge_colour,blanking_colour); // point rail end.

                                      modify_vee_end(splice_mid_dots_index,splice_mid_dots_index+1,edge_colour,blanking_colour); // splice rail end.
                                    end;

                          end;//with on_canvas
                        end;
              end;
              //////////////////////////////////////////////////////////////////

              procedure mark_end(aq1, aq1end, aq2, aq2end:integer);      // make a rail end mark

              begin
                if (endmarks_yn[aq1,aq1end]=True) and (endmarks_yn[aq2,aq2end]=True)
                   then begin
                          p1:=endmarks[aq1,aq1end];
                          p2:=endmarks[aq2,aq2end];


                          with sheet[0,0] do begin
                            move_to.X:=Round((p1.Y+ypd-grid_left)*scaw_out)+page_left_dots;  move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;
                            line_to.X:=Round((p2.Y+ypd-grid_left)*scaw_out)+page_left_dots;  line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;
                          end;//with

                          with on_canvas do begin
                            Pen.Color:=printcurail_colour;         //  1 = virtual black. Bug in HP driver if black (0) specified.
                                                                   //  (but not on "econofast" print !).
                            Pen.Mode:=pmCopy;
                            Pen.Style:=psSolid;

                            if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                          end;//with
                        end;
              end;
              //////////////////////////////////////////////////////////////////

              procedure outline_railends;       // draw the rail ends in outline mode.

              begin
                if {(railend_marks=True) and} (plain_track=False)
                   then begin                                       // mark rail-ends...

                          mark_end(1,1,9,1);    // turnout rail wing rail finish.
                          mark_end(2,1,10,1);   // main rail wing rail finish.

                          mark_end(6,0,14,0);   // main side check rail start.
                          mark_end(6,1,14,1);   // main side check rail finish.

                          mark_end(7,0,15,0);   // turnout side check rail start.
                          mark_end(7,1,15,1);   // turnout side check rail finish.

                          mark_end(4,0,5,0);    // blunt nose.

                          if (half_diamond=True) and (fixed_diamond=True)     // planed faced of point rails for a fixed-diamond.
                             then begin
                                    mark_end(1,0,9,0);
                                    mark_end(2,0,10,0);

                                    mark_end(26,1,27,1);     // MS K-crossing check rails.
                                    mark_end(28,1,29,1);     // DS K-crossing check rails.
                                  end;
                        end;
              end;
              //////////////////////////////////////////////////////////////////

begin

  grid_label:=0;   // keep the compiler happy.

  print_colours_setup;    // first set up the colours.

  if calcs_done_and_valid=False then redraw(False);    //  first do a direct redraw if nec to ensure valid calcs.
  if calcs_done_and_valid=False then EXIT;             //  calcs still not valid.

     // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

  case output_code of
      1: { T3-FIRST if dtp_settings_form.raster_trackplan_bgnd_checkbox.Checked=True
            then img_bgnd_colour:=dtp_settings_form.sb_page_colour_panel.Color        // use the page colour
            else} img_bgnd_colour:=export_form.img_bgnd_colour_panel.Color;           // use same as image export

      3: img_bgnd_colour:=export_form.img_bgnd_colour_panel.Color;  // export bitmap image file

    else img_bgnd_colour:=clWhite;
  end;//case

  if page_info(False,True,True,output_code)=False then EXIT;     // sets sheet[0,0] (single sheet)

  print_line_thickness_setup;  // needs the dpi via print_preview.  mod 0.73.a 12-9-01.

  gridx:=grid_spacex*100;  //gridsizex*2540;   // grid line spacings. in 1/100th mm.  (any output scaling is done later).
  gridy:=grid_spacey*100;  //gridsizey*2540;

  while gridx*out_factor<1000{500} do gridx:=gridx*2;      // 10mm (was 5 mm) closest grid spacing permitted down the page.
  while gridy*out_factor<1000 do gridy:=gridy*2;     // 10 mm ditto across page to allow for labels.

  slow_run:=0;                                            // cancel any slow-running.
  control_room_form.run_slow_menu_entry.Checked:=False;

  with sheet[0,0] do begin    // single sheet for exports

    if empty=True then EXIT;

    with on_canvas do begin

      Font.Assign(print_labels_font);         // for labels

      Brush.Style:=bsSolid;

      case output_code of
        1,3: begin                           // bitmaps
               Brush.Color:=img_bgnd_colour;
               FillRect(Rect(0,0,printer_length_indexmax_dots,printer_width_indexmax_dots));   //  swap X,Y for sketchboard
             end;

        else Brush.Color:=clWhite;  // metafiles
      end;//case

      if pad_form.grid_in_front_of_shapes_menu_entry.Checked=True then export_bgnd_shapes(on_canvas,canvas_height,grid_left,grid_top,output_code);   // first print all background shapes if wanted behind the grid.

           // draw grid...

      Font.Assign(print_labels_font);

      if output_code=1            // sketchboard track plan as bitmap
         then Font.Height:=0-Round(7*Font.Size*track_bmp_dpi/72);     // 7* arbitrary trial and error

      if output_code=2            // sketchboard track plan as metafile
         then Font.Height:=0-Round(7*Font.Size*metafile_dpi/72);

      if output_code=3            // track plan as an image file
         then Font.Height:=0-Round(7*Font.Size*create_image_dpi/72);

      if output_code=4            // track plan as EMF file
         then Font.Height:=0-Round(7*Font.Size*metafile_dpi/72);

      if ABS(Font.Height)<2 then Font.Height:=0-2; // dots

      if (((output_code=1) or (output_code=2)) { T3-FIRST and (dtp_settings_form.include_grid_checkbox.Checked=True)} )
      or (((output_code=3) or (output_code=4)) and (export_form.export_include_grid_checkbox.Checked=True))
             then begin
                    case grid_labels_code_i of
                                      1: grid_str:=' feet ';     //  labels in feet.
                                      2: grid_str:=' inches ';   //  labels in inches.
                                      3: grid_str:=' proto-feet '; //  labels in prototype feet.
                                      4: grid_str:=' cm ';       //  labels in cm.
                                      6: grid_str:=' mm ';       //  labels in mm.
                                    else run_error(213);
                    end;//case

                    Pen.Color:=printgrid_colour;           // for grid lines.
                    Pen.Mode:=pmCopy;

                    if pad_form.printed_grid_dotted_menu_entry.Checked=True
                       then begin
                              Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                              Brush.Style:=bsSolid;

                              Pen.Style:=psDot;
                              pen_width:=1;         // must be 1 for dots.
                            end
                       else begin
                              Pen.Style:=psSolid;
                              {if impact>0 then pen_width:=1                   // impact printer or plotter.
                                          else}
                              pen_width:=printgrid_wide;
                              if pen_width<1 then pen_width:=1;
                            end;

                       //  draw horizontal grid lines (across width)...

                    if print_pages_top_origin<>0
                       then now_gridx:=0-gridx
                       else now_gridx:=0;        //  init grid lines. no need for first line (gets overwritten by trim margins).

                    repeat
                      now_gridx:=now_gridx+gridx;

                      grid_now_dots:=Round((now_gridx-grid_top)*scal_out)+page_top_dots;

                      if grid_now_dots<0 then CONTINUE;

                      if grid_now_dots>page_bottom_dots then BREAK;   // 0.93.a  remove unwanted extra line

                      if (now_gridx=0) and (Pen.Style=psSolid)
                         then Pen.Width:=pen_width+2    // thicker datum line (only appears if page origin is negative).
                         else Pen.Width:=pen_width;

                      move_to.X:=left_blanking_dots;          move_to.Y:=grid_now_dots;
                      line_to.X:=printer_width_indexmax_dots; line_to.Y:=grid_now_dots;

                      if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;

                                 // 093a option to omit grid labels on sketchboard ...

                      if (((output_code=1) or (output_code=2)) { T3-FIRST and (dtp_settings_form.include_grid_labels_checkbox.Checked=True)} )    // for sketchboard
                      or (((output_code=3) or (output_code=4)) and (export_form.export_include_grid_labels_checkbox.Checked=True))
                         then begin

                                case grid_labels_code_i of
                                                  1: grid_label:=now_gridx/30480;       //  labels in feet.
                                                  2: grid_label:=now_gridx/2540;        //  labels in inches.
                                                  3: grid_label:=now_gridx/(100*scale); //  labels in prototype feet.
                                                  4: grid_label:=now_gridx/1000;        //  labels in cm.
                                                  6: grid_label:=now_gridx/100;         //  labels in mm.
                                                else begin
                                                       grid_label:=0;   // keep the compiler happy.
                                                       run_error(223);
                                                     end;
                                end;//case

                                grid_label_str:={' '+}FormatFloat('0.###',grid_label){+' '};

                                if (output_code=2) or (output_code=4)  //metafile
                                   then do_text_out(on_canvas,canvas_height,TextHeight('A')+60{left_blanking_dots},grid_now_dots-(TextWidth(grid_label_str) div 2),grid_label_str) //  add labels.
                                   else do_text_out(on_canvas,canvas_height,TextHeight('A')+6{left_blanking_dots},grid_now_dots-(TextWidth(grid_label_str) div 2),grid_label_str); //  add labels.
                              end;

                    until 0<>0; // 093a   //grid_now_dots>page_bottom_dots;

                           //  draw vertical grid lines (down length)...

                    if print_pages_left_origin<>0
                       then now_gridy:=0-gridy
                       else now_gridy:=0;        //  init grid lines. no need for first line (gets overwritten by trim margin).

                    repeat
                      now_gridy:=now_gridy+gridy;
                      grid_now_dots:=Round((now_gridy-grid_left)*scaw_out)+page_left_dots;

                      if grid_now_dots<0 then CONTINUE;

                      if grid_now_dots>page_right_dots then BREAK;   // 0.93.a  remove unwanted extra line

                      if (now_gridy=0) and (Pen.Style=psSolid)
                         then Pen.Width:=pen_width+2    // thicker datum line (only appears if page origin is negative).
                         else Pen.Width:=pen_width;

                      move_to.X:=grid_now_dots; move_to.Y:=top_blanking_dots;
                      line_to.X:=grid_now_dots; line_to.Y:=printer_length_indexmax_dots;
                      if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;

                                 // 093a option to omit grid labels on sketchboard ...

                      if (((output_code=1) or (output_code=2)) { T3-FIRST and (dtp_settings_form.include_grid_labels_checkbox.Checked=True)} )
                      or (((output_code=3) or (output_code=4)) and (export_form.export_include_grid_labels_checkbox.Checked=True))
                         then begin

                                case grid_labels_code_i of
                                                  1: grid_label:=now_gridy/30480;       //  labels in feet.
                                                  2: grid_label:=now_gridy/2540;        //  labels in inches.
                                                  3: grid_label:=now_gridy/(100*scale); //  labels in prototype feet.
                                                  4: grid_label:=now_gridy/1000;        //  labels in cm.
                                                  6: grid_label:=now_gridy/100;         //  labels in mm.
                                                else begin
                                                       grid_label:=0;   // keep the compiler happy.
                                                       run_error(224);
                                                     end;
                                end;//case

                                grid_label_str:={' '+}FormatFloat('0.###',grid_label){+' '};

                                {if banner_paper=False
                                   then}

                                if (output_code=2) or (output_code=4)  // metafile
                                   then do_text_out(on_canvas,canvas_height,grid_now_dots+(TextHeight('A') div 2),page_top_dots{-(printmargin_wide div 2)-halfmm_dots-TextHeight('A')}+40,grid_label_str) //  add labels.
                                   else do_text_out(on_canvas,canvas_height,grid_now_dots+(TextHeight('A') div 2),page_top_dots{-(printmargin_wide div 2)-halfmm_dots-TextHeight('A')}+4,grid_label_str); //  add labels.

                              end;

                    until 0<>0;  // 093a  was grid_now_dots>page_right_dots;

                              // finally add the units string...

                              // 093a option to omit grid labels on sketchboard ...

                    if (((output_code=1) or (output_code=2)) { T3-FIRST and (dtp_settings_form.include_grid_labels_checkbox.Checked=True)} )
                    or (((output_code=3) or (output_code=4)) and (export_form.export_include_grid_labels_checkbox.Checked=True))
                       then begin

                              if (output_code=2) or (output_code=4)   // metafile
                                 then do_text_out(on_canvas,canvas_height,TextHeight('A')+60{left_blanking_dots},page_top_dots{-(printmargin_wide div 2)-halfmm_dots-TextHeight('A')}+40,grid_str)  // add the units string.
                                 else do_text_out(on_canvas,canvas_height,TextHeight('A')+6{left_blanking_dots},page_top_dots{-(printmargin_wide div 2)-halfmm_dots-TextHeight('A')}+4,grid_str);   // add the units string.
                            end;

                    Pen.Style:=psSolid;  // reset in case of dotted.

                  end;//grid

                  // grid finished.

              //----------------------------------------

          if pad_form.grid_in_front_of_shapes_menu_entry.Checked=False then export_bgnd_shapes(on_canvas,canvas_height,grid_left,grid_top,output_code);   // first print all background shapes if not already done.

          export_bgnd(on_canvas,canvas_height,grid_left,grid_top,output_code);       // now print any background templates.


                   //  control template - draw timbers and all marks except rail joints...

          if  (print_entire_pad_flag=False)
          and (output_diagram_mode=False)  // 093a  no control template if diagram mode
          and (turnoutx>0)                 // not if invalidated
             then begin

                    if marks_list_ptr=nil then EXIT; //BREAK;       // pointer to marks list not valid, exit all sheets.

                    draw_marks(grid_left,grid_top,False);   // print all the background timbering and marks except rail joints.

                    if ( (print_settings_form.output_centrelines_checkbox.Checked=True) and (dummy_template=False) )       // 212a
                    or ( (print_settings_form.output_bgnd_shapes_checkbox.Checked=True) and (dummy_template=True) )

                       then begin

                              Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                              Brush.Style:=bsClear;
                              TextOut(0,0,'');

                              Pen.Mode:=pmCopy;

                              if dummy_template=True   // 212a
                                 then begin
                                        Pen.Style:=psSolid;
                                        Pen.Color:=printshape_colour;
                                        Pen.Width:=printshape_wide;
                                      end
                                 else begin
                                         Pen.Color:=printcurail_colour;

                                             // mods for track centre-lines  0.79.a  ...

                                         Pen.Width:=printcl_wide;
                                         if Pen.Width<1 then Pen.Width:=1;

                                         if Pen.Width=1 then Pen.Style:=psDash
                                                        else Pen.Style:=psSolid;

                                      end;

                              for aq:=24 to 25 do begin
                                if ( (plain_track=False) or (aq=24) ) and (aqyn[aq]=True)

                                        // main side only only if plain track, and data available ?

                                   then begin
                                          move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                                          for now:=1 to nlmax_array[aq] do begin
                                            line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                                            if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                            move_to:=line_to;
                                          end;//for
                                        end;
                              end;//for-next aq
                            end;//if track centre-lines.

                    if {pad_form.print_rails_menu_entry.Checked=True}  // 0.82.b
                       print_settings_form.output_rails_checkbox.Checked=True

                       then begin
                                          //  draw turnout rails...

                              Pen.Width:=printrail_wide;
                              if Pen.Width<1 then Pen.Width:=1;
                                               {end;}

                              if (rail_infill_i=0) // out for sketchboard,   was or ((scale*out_factor)<0.75)   // less than 18.75% for 4mm scale (control template) (10.71% for 7mm).
                                 then begin        //  outline (pen) mode ...
                                                   //  n.b. this mode does not automatically close the rail-ends.

                                        for aq:=0 to 23 do begin                                // 24, 25 centre-lines already done.
                                          if (adjacent_edges=False) and (aq>15) then CONTINUE;  // no adjacent tracks in output  // 206b
                                          draw_outline_railedge(aq,printcurail_colour);
                                        end;//next aq

                                        for aq:=26 to aq_max_c do draw_outline_railedge(aq,printcurail_colour);  // K-crossing check rails.

                                        outline_railends;     // finally do the rail ends for outline mode
                                      end
                                 else begin      // infill (polygon) mode ...

                                                 // do blades first - neater result.

                                        for rail:=1 to 3 do draw_fill_rail(8);  // closure rails and curved stock rail.

                                        rail:=0;                                // straight stock rail.
                                        draw_fill_rail(8);

                                        for rail:=6 to 7 do draw_fill_rail(8);  // check rails

                                        if adjacent_edges=True    // 206b
                                           then begin
                                                  rail:=16;
                                                  repeat
                                                    draw_fill_rail(1);   // platforms and trackbed edges
                                                    rail:=rail+2;
                                                  until rail>22;
                                                end;

                                        rail:=26;
                                        repeat
                                          draw_fill_rail(1);      // K-crossing MS check rails.
                                          rail:=rail+2;
                                        until rail>28;

                                        draw_fill_vee;   // now do the vee.

                                                  // finally draw in or overdraw the planing gauge-faces - (no infill) ...
                                        aq:=1;
                                        if (plain_track=False) and (gaunt=False) and (aqyn[1]=True) and (list_planing_mark_aq1>0) {and (drawn_full_aq1=False)}    // not if already drawn.
                                           then begin
                                                  move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                                                  for now:=1 to list_planing_mark_aq1{+1} do begin                    // +1 to overdraw
                                                    line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                                                    if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                                    move_to:=line_to;
                                                  end;//for
                                                end;

                                        aq:=2;
                                        if (plain_track=False) and (gaunt=False) and (aqyn[2]=True)  and (list_planing_mark_aq2>0) {and (drawn_full_aq2=False)}    // not if already drawn.
                                           then begin
                                                  move_to.X:=get_w_dots(aq,0); move_to.Y:=get_l_dots(aq,0);
                                                  for now:=1 to list_planing_mark_aq2{+1} do begin                      // +1 to overdraw
                                                    line_to.X:=get_w_dots(aq,now); line_to.Y:=get_l_dots(aq,now);
                                                    if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                                    move_to:=line_to;
                                                  end;//for
                                                end;

                                              //  CAN'T GET FLOODFILL TO WORK ON THE PRINTER 26-8-98
                                              //    and flood fill the planing with the margin colour ...

                                              //Brush.Bitmap:=nil;     // so can use style again if it was dots.

                                      end;//polygon mode

                                  // finally add rail joint marks across rails (will now mark over rail infill)...

                              draw_marks(grid_left,grid_top,True);

                            end;//if rails

                  end;// if control template

    end;//with on_canvas
  end;//with sheet

end;
//______________________________________________________________________________

procedure export_draw(on_canvas:TCanvas; canvas_width,canvas_height,output_code:integer);    // draw control template or entire pad on a bitmap or metafile.

   // output_code 1=sketchbook bitmap, 2=sketchbook metafile, 3=create image file, 4=create EMF file

var
  kludge_count:integer;
  saved_extensions:boolean;
  margin_dots:integer;

begin
  saved_extensions:=False;  // init

  if (output_code=2) or (output_code=4)  // metafiles (exact rectangle, no margin)...
     then begin
            export_limits:=True;   // limit output to defined rectangle ...

            min_export_x:=0;                              // dots
            max_export_x:=canvas_width;

            min_export_y:=0;                              // dots
            max_export_y:=canvas_height;
          end
     else begin  // bitmaps, allow overlaps, GDI trims to canvas rectangle ...

            margin_dots:=Round(50*scale*create_image_dpi/25.4);     // 50ft scale overlap all round

            min_export_x:=0-margin_dots;
            max_export_x:=canvas_width+margin_dots;

            min_export_y:=0-margin_dots;
            max_export_y:=canvas_height+margin_dots;
          end;

  if classic_templot=False then store_and_background(False,True);  // 093a make-on-click mode - first store existing control template

{ T3-ZERO  223d rebuild generator functions nyi

  if (print_settings_form.output_timber_extensions_checkbox.Checked=False) and (print_settings_form.output_timbering_checkbox.Checked=True)   // 223d   rebuild templates if necessary
     then begin
            saved_extensions:=outline_extensions;

            if print_group_only_flag=True
               then pad_form.remove_extensions_from_group_menu_entry.Click
               else begin
                      pad_form.remove_extensions_from_all_menu_entry.Tag:=1;       // flag no messages
                      pad_form.remove_extensions_from_all_menu_entry.Click;
                    end;
          end;
}

  if paper_bunching=True then cancel_paper_bunching;  // don't want bunching in the export.

  if output_diagram_mode=False
     then kludge_count:=create_fb_kludge_templates  // 0.94.a  if any
     else kludge_count:=0;

  do_export_draw(on_canvas,canvas_width,canvas_height,output_code);  // draw control template or entire pad on a bitmap or metafile.

  if kludge_count>0 then delete_fb_kludge_templates;  // 0.94.a  if any

  export_limits:=False;  // reset if necessary afterwards for global limits checking only

// T3-ZERO  if saved_extensions=True then pad_form.restore_timber_extension_marks_menu_entry.Click;  // 223d

end;
//______________________________________________________________________________

procedure export_bgnd_shapes(on_canvas:TCanvas; canvas_height:integer; grid_left,grid_top:extended; output_code:integer);  // print all background shapes.

var
  i,maxbg_index:integer;
  font_size:integer;

  arm,diamond:extended;

  now_shape:Tbgnd_shape;
  move_to,line_to:TPoint;
  raster_rect:TRect;

begin
  if print_settings_form.output_bgnd_shapes_checkbox.Checked=False then EXIT;

  maxbg_index:=bgnd_form.bgnd_shapes_listbox.Items.Count-1;

  if maxbg_index<0 then EXIT;

  with on_canvas do begin

      // label shapes..

    Font.Assign(shapes_label_font);
    Font.Color:=printshape_colour;

     // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

     // *3 arbitrary for typical non-100% use ...

    case output_code of

        1: Font.Height:=0-Round(3*Font.Size*track_bmp_dpi/72);     // sketchboard bitmap

      2,4: Font.Height:=0-Round(3*Font.Size*metafile_dpi/72);      // metafiles

        3: Font.Height:=0-Round(3*Font.Size*create_image_dpi/72);  // export bitmap

    end;//case

    Pen.Mode:=pmCopy;

    for i:=0 to maxbg_index do begin
      Pen.Style:=psSolid;
      Pen.Color:=printshape_colour;   // it changes for a label or monochrome picture.
      Pen.Width:=printshape_wide;     // it changes for a picture border and label border.

      now_shape:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape;     // next shape.

      with now_shape do begin

        if (hide_bits AND $02)<>0 then CONTINUE;   // shape hidden for output

        if shape_code<>4    // not a target mark
           then begin

                  case shape_style of
                                0: begin
                                     Brush.Color:=clWhite;
                                     Brush.Style:=bsClear;      // transparent. (also lines).
                                   end;
                                1: begin
                                     Brush.Color:=printtimber_infill_colour;
                                     Brush.Style:=bsSolid;      // blank out.
                                   end;

                                2: begin
                                     Brush.Color:=Pen.Color;
                                     Brush.Style:=bsDiagCross;  // cross-hatched.

                                     swap_text_out(on_canvas,canvas_height,0,0,'');    // !!! Delphi bug? This seems to be necessary before dotted lines will draw properly.

                                     if shape_code=0 then Pen.Style:=psDot;  // dashed line.
                                   end;

                              else begin
                                     Brush.Color:=clWhite;
                                     Brush.Style:=bsClear;      // transparent.
                                   end;
                  end;//case

                  move_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  move_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if shape_code=3     // label rectangle..
                     then begin
                                 // need to swap corners for swapped label rectangle...

                            line_to.Y:=move_to.Y+TextWidth(shape_name+'   '); // add 3 spaces
                            line_to.X:=move_to.X-ABS(Font.Height*4 div 3);    // arbitrary
                          end
                     else begin
                            line_to.X:=Round((p2.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                            line_to.Y:=Round((p2.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;
                          end;

                  if (move_to.X<0) and (line_to.X<0) then CONTINUE;                                                        // not on this page.
                  if (move_to.X>printer_width_indexmax_dots) and (line_to.X>printer_width_indexmax_dots) then CONTINUE;    // not on this page.

                  if (move_to.Y<0) and (line_to.Y<0) then CONTINUE;                                                        // not on this page.
                  if (move_to.Y>printer_length_indexmax_dots) and (line_to.Y>printer_length_indexmax_dots) then CONTINUE;  // not on this page.

                  if check_limits(move_to, line_to)=True
                      then begin
                             case shape_code of

                                    -1: begin     // picture = bitmap image...

                                          if (((output_code=1) or (output_code=2)) { T3-FIRST and (dtp_settings_form.include_picture_shapes_checkbox.Checked=True)} )
                                          or (((output_code=3) or (output_code=4)) and (export_form.export_include_picture_shapes_checkbox.Checked=True))
                                             then begin
                                                    try
                                                               // swap raster rectangle ...

                                                      raster_rect.Left:=move_to.Y;
                                                      raster_rect.Top:=canvas_height-line_to.X;

                                                      raster_rect.Right:=line_to.Y;
                                                      raster_rect.Bottom:=canvas_height-move_to.X;

                                                      if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.picture_is_metafile=True
                                                         then begin
                                                                { T3-ZERO     nested metafile nyi
                                                                      // metafile...

                                                                bgnd_form.bgnd_shape_image.Picture.Graphic:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_metafile;

                                                                StretchDraw(raster_rect,bgnd_form.bgnd_shape_image.Picture.Graphic);   // needs TGraphic to work reliably.
                                                                }
                                                              end
                                                         else begin
                                                                         // bitmap...

                                                                if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgnd_shape.show_transparent=True  // 0.93.a moved into file
                                                                   then CopyMode:=cmSrcAnd    // (destination Canvas) transparent if on white background.
                                                                   else CopyMode:=cmSrcCopy;  // reset normal for destination Canvas.

                                                                if Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap.Monochrome=True
                                                                   then begin
                                                                          Brush.Style:=bsSolid;    //!!! these are all needed to get StretchDraw to work with monochrome bitmaps
                                                                          Brush.Color:=clWhite;
                                                                          Pen.Color:=clBlack;
                                                                          Font.Color:=clBlack;  // !!!! including this.
                                                                          swap_text_out(on_canvas,canvas_height,0,0,'');      // !!! Delphi bug?
                                                                                                                              // TextOut obviously initialises some background mask property which I have been unable
                                                                                                                              // to find or set any other way.
                                                                        end;

                                                                pad_form.bgnd_shape_image.Picture.Graphic:=Tbgshape(bgnd_form.bgnd_shapes_listbox.Items.Objects[i]).bgimage.image_shape.image_bitmap;

                                                                StretchDraw(raster_rect,pad_form.bgnd_shape_image.Picture.Graphic);   // needs TGraphic parameter to work reliably.

                                                                CopyMode:=cmSrcCopy;   // reset normal for destination Canvas.

                                                              end;//metafile/bitmap

                                                              // add a picture border if wanted...  205a

                                                      if (((output_code=1) or (output_code=2)) { T3-FIRST and (dtp_settings_form.include_picture_borders_checkbox.Checked=True)} )
                                                      or (((output_code=3) or (output_code=4)) and (export_form.export_include_picture_borders_checkbox.Checked=True))
                                                         then begin
                                                                Pen.Width:=printpicborder_wide;
                                                                Pen.Color:=printshape_colour;
                                                                Brush.Color:=clWhite;
                                                                Brush.Style:=bsClear;
                                                                swap_rectangle(on_canvas,canvas_height,move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                              end;

                                                    except
                                                      CopyMode:=cmSrcCopy;          // reset normal for destination Canvas.
                                                      Pen.Width:=1;
                                                      Pen.Color:=printshape_colour;
                                                      Brush.Color:=Pen.Color;       // stretch failed - draw hatched outline.
                                                      Brush.Style:=bsBDiagonal;
                                                      swap_rectangle(on_canvas,canvas_height,move_to.X, move_to.Y, line_to.X, line_to.Y);
                                                    end;//try

                                                  end;   //include pictures

                                        end;//-1

                                     0: begin
                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                        end;

                                     1: swap_rectangle(on_canvas,canvas_height,move_to.X, move_to.Y, line_to.X, line_to.Y);

                                     2: swap_ellipse(on_canvas,canvas_height,move_to.X, move_to.Y, line_to.X, line_to.Y);

                                     3: begin  // label -- text first to avoid metafiles blanking over the box

                                          swap_text_out(on_canvas,canvas_height,move_to.X,move_to.Y,'  '+shape_name);   // add 2 spaces

                                          Brush.Color:=clWhite;
                                          Brush.Style:=bsClear;                 // empty rectangle box over label text.
                                          Pen.Color:=Font.Color;
                                          Pen.Width:=ABS(Font.Height div 24);   // arbitrary
                                          if Pen.Width<1 then Pen.Width:=1;

                                          swap_rectangle(on_canvas,canvas_height,move_to.X, move_to.Y, line_to.X, line_to.Y);
                                        end;
                             end;//case
                           end;

                end
           else begin    // shape_code=4, draw a target mark

                  arm:=p2.x;        // cross arm length.
                  diamond:=arm/2;   // size of centre diamond.

                  move_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;          // lengthwise arms...
                  move_to.Y:=Round(((p1.x-arm)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  line_to.X:=move_to.X;
                  line_to.Y:=Round(((p1.x+arm)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                            swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);   // draw lengthwise arms.
                          end;

                  move_to.X:=Round(((p1.y-arm)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;    // widthwise arms...
                  move_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  line_to.X:=Round(((p1.y+arm)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=move_to.Y;

                  if check_limits(move_to, line_to)=True
                     then begin
                            swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                            swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);   // draw widthwise arms.
                          end;

                     // now do 4 diamond lines...

                                // NW line...

                  move_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  move_to.Y:=Round(((p1.x-diamond)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  line_to.X:=Round(((p1.y+diamond)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                            swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                          end;

                  move_to:=line_to;      // NE line...
                  line_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round(((p1.x+diamond)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                            swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                          end;

                  move_to:=line_to;      // SE line...
                  line_to.X:=Round(((p1.y-diamond)*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round((p1.x*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                            swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                          end;

                  move_to:=line_to;      // SW line...
                  line_to.X:=Round((p1.y*100+re_org_y-grid_left)*scaw_out)+page_left_dots;
                  line_to.Y:=Round(((p1.x-diamond)*100+re_org_x-grid_top)*scal_out)+page_top_dots;

                  if check_limits(move_to, line_to)=True
                     then begin
                            swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                            swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                          end;
                end;

      end;//with now_shape
    end;//for next i
  end;//with on_canvas
end;
//_______________________________________________________________________________________

procedure export_bgnd_marks(on_canvas:TCanvas; canvas_height:integer; grid_left,grid_top:extended; maxbg_index:integer; rail_joints:boolean);  // export all the background timbering and marks

                // if rail_joints=True print only the rail joints, otherwise omit them.
var
  //single_colour_flag:boolean;
  i,n:integer;

  move_to,line_to:TPoint;
  p1,p2,p3,p4: TPoint;

  now_keep:Tbgnd_keep;

  array_max:integer;
  code:integer;

  radcen_arm:extended;

  infill_points:array [0..3] of TPoint;

  fontsize:extended;
  num_str:string;
  tbnum_str:string;

  mapping_colour:integer;
  using_mapping_colour:boolean;

begin
  with on_canvas do begin

    Pen.Mode:=pmCopy;   // defaults.
    Pen.Style:=psSolid;

    for n:=0 to maxbg_index do begin

      if Ttemplate(keeps_list.Objects[n]).bg_copied=False then CONTINUE;  // no data, not on background.

      if (Ttemplate(keeps_list.Objects[n]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

      if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.fb_kludge_template_code>0 then CONTINUE;  // 209c no marks for fb_kludge templates

      now_keep:=Ttemplate(keeps_list.Objects[n]).bgnd_keep;    // next background keep.

      with now_keep do begin

              // mapping_colours_print: 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.

        using_mapping_colour:=False;  // default init.
        mapping_colour:=clBlack;      // init - keep compiler happy.

        with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1 do begin

          if (use_print_mapping_colour=True)
             and ( (mapping_colours_print=2) or (mapping_colours_print=3) )
             and (export_black_white=False)
             and (export_grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(print_mapping_colour);
                        using_mapping_colour:=True;
                      end;

          if (use_pad_marker_colour=True)
             and (mapping_colours_print=4)   // use pad settings instead
             and (export_black_white=False)
             and (export_grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(pad_marker_colour);
                        using_mapping_colour:=True;
                      end;
        end;//with

        tbnum_str:=timber_numbers_string;      // the full string of timber numbering.

             // first draw bgnd marks and timbers ...

        array_max:=intarray_max(list_bgnd_marks[0]);

        for i:=0 to array_max do begin

          code:=intarray_get(list_bgnd_marks[4],i);

          case code of
            -5,-4,-1,0,8,9,10,501..508,600..607: CONTINUE;     // no name label, timber selector, peg centre, blank, peg arms, plain-track end marks.    // 0.94.a no check-rail labels
          end;//case

          if print_settings_form.output_rail_joints_checkbox.Checked=False    // 223d
             then begin
                    case code of
                       6: CONTINUE;     // rail joints not wanted.
                    end;//case
                  end;

              // overwrite rail joints on rails..

          if rail_joints=(code<>6) then CONTINUE;  // do only the rail joints if rail_joints=True and ignore them otherwise.

          if print_settings_form.output_timbering_checkbox.Checked=False
             then begin
                    case code of
                      3,4,5,14,33,44,54,55,93,95,99,203,233,293: CONTINUE;     // no timbering wanted.
                    end;//case
                  end;

          if print_settings_form.output_timber_centres_checkbox.Checked=False    // 223d
             then begin
                    case code of
                       4,14,44,54: CONTINUE;     // timber centre-lines not wanted.
                    end;//case
                  end;

          if print_settings_form.output_guide_marks_checkbox.Checked=False    // 223d
             then begin
                    case code of
                       1: CONTINUE;     // guide marks not wanted.
                    end;//case
                  end;

          if print_settings_form.output_switch_drive_checkbox.Checked=False    // 223d
             then begin
                    case code of
                       101: CONTINUE;     // switch drive not wanted.
                    end;//case
                  end;

         if print_settings_form.output_chairs_checkbox.Checked=False
             then begin
                    case code of
                       480..499: CONTINUE;     // no chair outlines wanted  221a
                    end;//case
                  end;

          if print_settings_form.output_radial_ends_checkbox.Checked=False
             then begin
                    case code of
                       2,7: CONTINUE;     // no radial ends wanted  206a
                    end;//case
                  end;

          if ((code=5) or (code=55) or (code=95)) and (out_factor<>1.0) then CONTINUE;   // reduced ends are meaningless if not full-size.

          if ((code=203) or (code=233) or (code=293)) and (i<array_max)         // timber infill
             then begin
                     p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm
                     p1.Y:=intarray_get(list_bgnd_marks[1],i);

                     p2.X:=intarray_get(list_bgnd_marks[2],i);    // x2,y2 in  1/100ths mm
                     p2.Y:=intarray_get(list_bgnd_marks[3],i);

                     p3.X:=intarray_get(list_bgnd_marks[0],i+1);    // x3,y3 in  1/100ths mm
                     p3.Y:=intarray_get(list_bgnd_marks[1],i+1);

                     p4.X:=intarray_get(list_bgnd_marks[2],i+1);    // x4,y4 in  1/100ths mm
                     p4.Y:=intarray_get(list_bgnd_marks[3],i+1);
                  end
             else begin         // keep compiler happy...
                     p1.X:=0;
                     p1.Y:=0;

                     p2.X:=0;
                     p2.Y:=0;

                     p3.X:=0;
                     p3.Y:=0;

                     p4.X:=0;
                     p4.Y:=0;
                  end;

          if ( (code>0) and (code<200) and (code<>99) )  // 223d
             then begin
                    Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                    Brush.Style:=bsClear;
                    TextOut(0,0,'');


                     p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm
                     p1.Y:=intarray_get(list_bgnd_marks[1],i);

                     p2.X:=intarray_get(list_bgnd_marks[2],i);    // x2,y2 in  1/100ths mm
                     p2.Y:=intarray_get(list_bgnd_marks[3],i);

                     {if impact>0 then Pen.Width:=1        // impact printer or plotter.
                                 else begin}
                                       case code of
                                         1,101: Pen.Width:=printmark_wide;    // guide marks.  switch drive
                                             2: Pen.Width:=printmark_wide;    // rad end marks.
                                       3,33,93: Pen.Width:=printtimber_wide;  // timber outlines.
                                          4,44: Pen.Width:=1;                  // timber centre-lines.
                                       5,55,95: Pen.Width:=1;                  // timber reduced ends.
                                             6: Pen.Width:=printmark_wide;    // rail joint marks.
                                             7: Pen.Width:=printmark_wide;    // transition ends.
                                         14,54: Pen.Width:=printrail_wide;    // timber centre-lines with rail centre-lines (for rivet locations?).

                                           else Pen.Width:=1;                  // others not drawn.
                                       end;//case

                                       // out 0.73.a 12-8-01 (now done in thickness setup) if out_factor<1 then Pen.Width:=Round(Pen.Width*out_factor); // scale down the line width.
                                       if Pen.Width<1 then Pen.Width:=1;
                                     {end;}

                    case code of
                        4,44: Pen.Style:=psDash;    // timber centre-lines (not for rivets).
                     5,55,95: Pen.Style:=psDot;     // timber reduced ends.
                         else Pen.Style:=psSolid;   // all the rest.
                    end;//case

                    if Pen.Style<>psSolid then Pen.Width:=1;   // delphi bug? (patterns only work for lines 1 dot wide.)

                    if export_black_white=True
                       then Pen.Color:=clBlack
                       else begin

                              if using_mapping_colour=True
                                 then Pen.Color:=mapping_colour
                                 else begin
                                        if mapping_colours_print<0    // 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.
                                           then Pen.Color:=printbg_single_colour     // single colour for all of background templates.
                                           else begin
                                                  case code of
                                                   1,101: Pen.Color:=printguide_colour;  // guide marks.  switch drive
                                                       2: Pen.Color:=printalign_colour;  // rad end marks.
                                                 3,33,93: Pen.Color:=printtimber_colour; // timber outlines.
                                                       6: Pen.Color:=printjoint_colour;  // rail joints.
                                                       7: Pen.Color:=printalign_colour;        // transition ends.
                                                     else Pen.Color:=calc_intensity(clBlack);  // thin dotted lines in black only for timber centres and reduced ends.
                                                  end;//case
                                                end;
                                      end;
                            end;

                    Pen.Mode:=pmCopy;

                    move_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                    move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                    line_to.X:=Round((p2.Y-grid_left)*scaw_out)+page_left_dots;
                    line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;
                    if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                  end
             else begin
                    if ((code=-2) or (code=-3)) and {(pad_form.print_radial_centres_menu_entry.Checked=True)}  // 0.82.b
                       (print_settings_form.output_radial_centres_checkbox.Checked=True)

                           // draw curving rad centres...

                       then begin
                              {if impact>0 then Pen.Width:=1                // impact printer or plotter.
                                          else}
                              Pen.Width:=printmark_wide;  // guide marks.

                              // out 0.73.a 12-8-01 (now done in thickness setup) if out_factor<1 then Pen.Width:=Round(Pen.Width*out_factor); // scale down the line width.
                              if Pen.Width<1 then Pen.Width:=1;

                              Pen.Style:=psSolid;
                              Pen.Mode:=pmCopy;

                              if export_black_white=True
                                 then Pen.Color:=clBlack  // overide.
                                 else begin
                                        if mapping_colours_print<>-1
                                           then Pen.Color:=calc_intensity(clBlack)
                                           else Pen.Color:=printbg_single_colour;
                                      end;

                              p1.X:=intarray_get(list_bgnd_marks[0],i);    // x1,y1 in  1/100ths mm
                              p1.Y:=intarray_get(list_bgnd_marks[1],i);

                              radcen_arm:=400*scale;  // 4ft scale arbitrary (scale is for control template).

                              move_to.X:=Round((p1.Y+radcen_arm-grid_left)*scaw_out)+page_left_dots;      // mark centre widthwise.
                              move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                              line_to.X:=Round((p1.Y-radcen_arm-grid_left)*scaw_out)+page_left_dots;
                              line_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;
                              if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;

                              move_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;                 // mark centre lengthwise
                              move_to.Y:=Round((p1.X+radcen_arm-grid_top)*scal_out)+page_top_dots;

                              line_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                              line_to.Y:=Round((p1.X-radcen_arm-grid_top)*scal_out)+page_top_dots;
                              if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                            end;

                    if (code=203) or (code=233) or (code=293)       // timber infill...
                       then begin
                              infill_points[0].X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[0].Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                              infill_points[1].X:=Round((p2.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[1].Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;

                              infill_points[2].X:=Round((p3.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[2].Y:=Round((p3.X-grid_top)*scal_out)+page_top_dots;

                              infill_points[3].X:=Round((p4.Y-grid_left)*scaw_out)+page_left_dots;
                              infill_points[3].Y:=Round((p4.X-grid_top)*scal_out)+page_top_dots;

                              if (check_limits(infill_points[0],infill_points[1])=True) and (check_limits(infill_points[2],infill_points[3])=True)
                                 then begin
                                        Pen.Width:=1;
                                        Pen.Style:=psSolid;
                                        Pen.Mode:=pmCopy;

                                        Pen.Color:=clWhite;  // so no overdrawing of timber outlines.

                                        if export_black_white=True
                                           then Brush.Color:=clBlack
                                           else begin
                                                  if mapping_colours_print<>-1
                                                     then Brush.Color:=printtimber_infill_colour
                                                     else Brush.Color:=printbg_single_colour;
                                                end;

                                        case print_timb_infill_style of
                                                        0: CONTINUE;
                                                        1: Brush.Style:=bsBDiagonal;    // hatched. Backward diagonal for the background templates.
                                                        2: Brush.Style:=bsDiagCross;

                                                        3: if (export_black_white=True) or (mapping_colours_print<0)
                                                              then CONTINUE  // 209c now no fill   was  Brush.Style:=bsBDiagonal
                                                              else Brush.Style:=bsSolid;

                                                        4: begin                         // blank.
                                                             Brush.Style:=bsSolid;
                                                             Brush.Color:=clWhite;       // overide.
                                                           end;

                                                      else CONTINUE;
                                        end;//case

                                        swap_polygon(on_canvas,canvas_height,infill_points);
                                      end;
                            end;

                  end;//other codes

        end;//next i background mark

      end;//with now_keep
    end;//next n template
  end;//with on_canvas
end;
//__________________________________________________________________________________________

procedure export_bgnd(on_canvas:TCanvas; canvas_height:integer; grid_left,grid_top:extended; output_code:integer);    // export background templates

var
  max_list_index:integer;
  move_to,line_to:TPoint;
  p1,p2: TPoint;
  now_keep:Tbgnd_keep;

  n,aq,nk:integer;
  array_max:integer;

  xint,yint:integer;

  l_dims_valid:boolean;
  w_dims_valid:boolean;

  now,rail:integer;

  mapping_colour:integer;
  using_mapping_colour:boolean;

  fixed_diamond_ends:boolean;

  gaunt_template:boolean;  // 0.93.a  ex 0.81

  fb_kludge_this:integer;  // 0.94.a

  cl_warning_shown:boolean;  // 206a

  this_one_platforms_trackbed:boolean;  // 206b

  this_one_trackbed_cess_ms:boolean;       // 215a
  this_one_trackbed_cess_ts:boolean;       // 215a

                  //////////////////////////////////////////////////////////////

                  procedure set_pen_railcolour; // 094a        (rail_edges:boolean);  // 0.76.a 3-11-01.

                  begin
                    with on_canvas do begin
                      if export_black_white=True
                         then begin
                                Pen.Color:=clBlack;
                                EXIT;
                              end;

                      if output_diagram_mode=True    // 094a  don't use mapping colour for rail edges (used for infill instead).
                         then begin
                                if (rail=16) or (rail=20)                  // 0.93.a platforms
                                   then Pen.Color:=printplat_edge_colour
                                   else Pen.Color:=printbgrail_colour;
                                EXIT;
                              end;

                      if using_mapping_colour=True       // detail mode
                         then begin
                                Pen.Color:=mapping_colour;
                                EXIT;
                              end;

                      if mapping_colours_print<0    // detail mode     // 0=normal, 1=rails only, 2=timbers only, 3=rails and timber outlines, 4:=use the PAD colour instead, -1=single colour.
                         then begin
                                Pen.Color:=printbg_single_colour;     // single colour for all of background templates.
                                EXIT;
                              end;

                                // normal output, detail mode ...

                      if (rail=16) or (rail=20)   // 0.93.a platforms
                         then Pen.Color:=printplat_edge_colour
                         else Pen.Color:=printbgrail_colour;

                    end;//with
                  end;
                  //////////////////////////////////////////////////////////////

                  function pbg_get_w_dots(q,n:integer):integer;

                  var
                    yint:integer;

                  begin
                    yint:=intarray_get(now_keep.list_bgnd_rails[q,1],n);

                    RESULT:=Round((yint-grid_left)*scaw_out)+page_left_dots;

                    w_dims_valid:=check_draw_dim_w(RESULT);
                  end;
                  //////////////////////////////////////////////////////////////

                  function pbg_get_l_dots(q,n:integer):integer;

                  var
                    xint:integer;

                  begin
                    xint:=intarray_get(now_keep.list_bgnd_rails[q,0],n);

                    RESULT:=Round((xint-grid_top)*scal_out)+page_top_dots;

                    l_dims_valid:=check_draw_dim_l(RESULT);
                  end;
                  //////////////////////////////////////////////////////////////

                  procedure pbg_outline_railedge(aq,blanking_colour:integer; blank_it:boolean);

                  var
                    nk:integer;

                  begin
                    with now_keep do begin
                      array_max:=intarray_max(list_bgnd_rails[aq,0]);
                      if array_max=0 then EXIT;                         // empty rail.

                      xint:=intarray_get(list_bgnd_rails[aq,0],0);
                      yint:=intarray_get(list_bgnd_rails[aq,1],0);

                      move_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                      move_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                      with on_canvas do begin

                        if blank_it=True
                           then Pen.Color:=blanking_colour
                           else set_pen_railcolour;

                        for nk:=1 to array_max do begin

                          xint:=intarray_get(list_bgnd_rails[aq,0],nk);
                          yint:=intarray_get(list_bgnd_rails[aq,1],nk);

                          line_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                          line_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                          if check_limits(move_to, line_to)=True
                             then begin
                                    swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                    swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                  end;
                          move_to:=line_to;
                        end;//next nk

                      end;//with on_canvas
                    end;//with bgnd template
                  end;
                  //////////////////////////////////////////////////////////////

                  procedure pbg_draw_fill_rail(outer_add:integer);    // draw a complete filled rail.

                  const
                    dots_max_c=xy_pts_c*2;   // max 6000     xy_pts_c=3000

                  var
                    dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                      //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                      //   template max is 4500' scale length.
                      //   ( = 18000 mm or 59ft approx in 4 mm scale).
                      //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                      // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                      // because needed for the Polygon function.

                      // total memory = 6000*8 bytes = 48kb.

                    now, start, now_max:integer;
                    edge_started:boolean;
                    dots_index:integer;
                    x_dots,y_dots:integer;
                    aq:integer;
                    mid_dots_index:integer;
                    edge_colour, blanking_colour:integer;

                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure pbg_modify_rail_end(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with on_canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,move_to.X, move_to.Y);

                                                          swap_move_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                  begin
                    aq:=rail;  // gauge-faces.

                    with now_keep do begin

                      if (intarray_max(list_bgnd_rails[aq,0])=0) or (intarray_max(list_bgnd_rails[aq+outer_add,0])=0)  // data not for both edges?
                         then begin
                                if intarray_max(list_bgnd_rails[aq,0])<>0 then pbg_outline_railedge(aq,0,False);
                                if intarray_max(list_bgnd_rails[aq+outer_add,0])<>0 then pbg_outline_railedge(aq+outer_add,0,False);
                                EXIT;
                              end;

                      now_max:=intarray_max(list_bgnd_rails[aq,0]);

                      if gaunt_template=False
                         then begin

                                case aq of
                                     1: begin
                                          start:=planing_end_aq1; // start from end of planing - no infill in planing.

                                          if (start<0) or (start>now_max) then EXIT;  // ???
                                        end;

                                     2: begin                     // ditto
                                          start:=planing_end_aq2; // start from end of planing - no infill in planing.

                                          if (start<0) or (start>now_max) then EXIT;  // ???
                                        end;

                                   else start:=0;                   // whole list.
                                end;//case

                              end
                         else start:=0;  // gaunt template, no planing. 0.81 09-Jul-2005

                      dots_index:=0-1;          // first increment is to zero.

                      edge_started:=False;

                      for now:=start to now_max do begin
                        x_dots:=pbg_get_w_dots(aq,now);
                        y_dots:=pbg_get_l_dots(aq,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      mid_dots_index:=dots_index;

                      aq:=rail+outer_add;             // outer-edges.

                      now_max:=intarray_max(list_bgnd_rails[aq,0]);

                      edge_started:=False;

                      for now:=now_max downto 0 do begin
                        x_dots:=pbg_get_w_dots(aq,now);
                        y_dots:=pbg_get_l_dots(aq,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      with on_canvas do begin

                        set_pen_railcolour;

                        Pen.Width:=printrail_wide;
                        if Pen.Width<1 then Pen.Width:=1;

                        if (rail=16) or (rail=20)   // 093a platforms
                           then begin
                                  if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )   // 206b
                                     then Brush.Color:=Pen.Color
                                     else Brush.Color:=printplat_infill_colour;

                                  case print_platform_infill_style of
                                          0: Brush.Style:=bsClear;
                                          1: Brush.Style:=bsFDiagonal;    // hatched. forward diagonal (backward diagonal on bgnd template timbers).
                                          2: Brush.Style:=bsDiagCross;

                                          3: if (export_black_white=True) or (mapping_colours_print<0)    // single colour
                                                then Brush.Style:=bsFDiagonal
                                                else Brush.Style:=bsSolid;

                                        else begin                         // 4 = blank.
                                               Brush.Style:=bsSolid;
                                               Brush.Color:=clWhite;       // overide.
                                             end;

                                  end;//case
                                end
                           else begin
                                  if ((this_one_trackbed_cess_ts=True) and (rail=18))  // 215a
                                  or ((this_one_trackbed_cess_ms=True) and (rail=22))  // 215a
                                     then begin
                                            if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
                                               then Brush.Color:=Pen.Color
                                               else Brush.Color:=sb_track_bgnd_colour;      // cess use same colour as track background
                                            Brush.Style:=bsBDiagonal;
                                          end
                                     else begin   // normal rails...

                                            if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
                                               then Brush.Color:=calc_intensity(clGray)
                                               else begin
                                                      if fb_kludge_this>0 then Brush.Color:=printrail_infill_colour_cu    // 0.94.a
                                                                          else Brush.Color:=printrail_infill_colour_bg;
                                                    end;

                                            case rail_infill_i of
                                                            1: Brush.Style:=bsBDiagonal;   // hatched
                                                            2: Brush.Style:=bsSolid;       // solid
                                                            3: Brush.Style:=bsDiagCross;   // cross_hatched
                                                            4: begin                       // blank
                                                                 Brush.Style:=bsSolid;
                                                                 Brush.Color:=clWhite;     // overide
                                                               end;
                                                          else Brush.Style:=bsSolid;       // solid
                                            end;//case

                                          end;
                                end;

                        if dots_index>2
                           then begin
                                  swap_polygon(on_canvas,canvas_height,Slice(dots,dots_index+1));   // +1, number of points, not index.  must have 4 points.

                                  edge_colour:=Pen.Color;  // existing rail edges.

                                  if Brush.Style=bsSolid
                                     then blanking_colour:=Brush.Color   // infill colour.
                                     else begin   // 206b hatched fill...
                                                  // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

                                            case output_code of
                                              1,2: blanking_colour:=Brush.Color; { T3-FIRST dtp_settings_form.sb_page_colour_panel.Color; }
                                                3: blanking_colour:=export_form.img_bgnd_colour_panel.Color;
                                                4: blanking_colour:=Brush.Color; // for metafile export
                                             else  blanking_colour:=clWhite;  // assume white background (print, PDF)
                                            end;//case
                                          end;

                                        // remove polygon line across end of planing (not for fixed-diamond)..
                                        // (for gaunt template this removes the polygon line across the rail end)

                                  if (fixed_diamond_ends=False) and ((rail=1) or (rail=2)) then pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                        // remove polygon lines across stock rail ends...
                                        // and trackbed ends  206b

                                  if (rail=0) or (rail=3) or (rail=18) or (rail=22)   // 18,22 added 206b
                                     then begin
                                            pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // toe or approach end.

                                            pbg_modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);  // exit end.
                                          end;

                                           // 093a blank platform edges ...

                                  with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info do begin

                                    if adjacent_edges_keep=True  // platforms
                                       then begin
                                                    // 0.93.a blank platform rear edges ...

                                              if (rail=16) and (draw_ts_platform_keep=True) and (draw_ts_platform_rear_edge_keep=False)   // 0.93.a TS platform start
                                                 then pbg_outline_railedge(16,blanking_colour,True);     // blank rear edge

                                              if (rail=20) and (draw_ms_platform_keep=True) and (draw_ms_platform_rear_edge_keep=False)   // 0.93.a TS platform start
                                                 then pbg_outline_railedge(20,blanking_colour,True);     // blank rear edge

                                                    // 0.93.a blank platform ends ...

                                              if (rail=16) and (draw_ts_platform_keep=True) and (draw_ts_platform_start_edge_keep=False)   // 0.93.a TS platform start
                                                 then pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=16) and (draw_ts_platform_keep=True) and (draw_ts_platform_end_edge_keep=False)     // 0.93.a TS platform end
                                                 then pbg_modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform_keep=True) and (draw_ms_platform_start_edge_keep=False)   // 0.93.a MS platform start
                                                 then pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);

                                              if (rail=20) and (draw_ms_platform_keep=True) and (draw_ms_platform_end_edge_keep=False)     // 0.93.a MS platform end
                                                 then pbg_modify_rail_end(mid_dots_index,mid_dots_index+1,edge_colour,blanking_colour);

                                            end;
                                  end;//with

                                  if (rail=26) or (rail=28)
                                     then begin
                                            pbg_modify_rail_end(0,dots_index,edge_colour,blanking_colour);  // centre of K-crossing check rails.
                                          end;

                                end;
                      end;//with on_canvas
                    end;//with background template
                  end;
                  //////////////////////////////////////////////////////////////

                  procedure pbg_draw_fill_vee;    // do complete vee in one go ...

                  const
                    dots_max_c=xy_pts_c*2;     // max 6000      xy_pts_c=3000

                  var
                    dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                      //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                      //   template max is 4500' scale length.
                      //   ( = 18000 mm or 59ft approx in 4 mm scale).
                      //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                      // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                      // because needed for the Polygon function.

                      // total memory = 6000*8 bytes = 48KB

                    now:integer;
                    edge_started:boolean;
                    dots_index:integer;
                    x_dots,y_dots:integer;
                    aq:integer;
                    point_mid_dots_index, splice_mid_dots_index:integer;
                    edge_colour, blanking_colour:integer;

                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure pbg_modify_vee_end(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with on_canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,move_to.X, move_to.Y);

                                                          swap_move_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                  begin
                    with now_keep do begin

                      if (intarray_max(list_bgnd_rails[4,0])=0)
                      or (intarray_max(list_bgnd_rails[5,0])=0)
                      or (intarray_max(list_bgnd_rails[12,0])=0)
                      or (intarray_max(list_bgnd_rails[13,0])=0)  // not enough data for filled vee.
                         then begin
                                if intarray_max(list_bgnd_rails[4,0])<>0  then pbg_outline_railedge(4,0,False);       // draw outline vee...
                                if intarray_max(list_bgnd_rails[5,0])<>0  then pbg_outline_railedge(5,0,False);
                                if intarray_max(list_bgnd_rails[12,0])<>0 then pbg_outline_railedge(12,0,False);
                                if intarray_max(list_bgnd_rails[13,0])<>0 then pbg_outline_railedge(13,0,False);
                              end
                         else begin                // polygon mode...

                                dots_index:=0-1;   // first increment is to zero.

                                aq:=4;
                                edge_started:=False;
                                for now:=0 to intarray_max(list_bgnd_rails[aq,0]) do begin    // vee main-side, gauge_face, start from the tip.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                point_mid_dots_index:=dots_index;

                                aq:=12;
                                edge_started:=False;
                                for now:=intarray_max(list_bgnd_rails[aq,0]) downto 0 do begin // back along outer-edge.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                aq:=13;
                                edge_started:=False;
                                for now:=0 to intarray_max(list_bgnd_rails[aq,0]) do begin    // and then turnout side outer edge.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                splice_mid_dots_index:=dots_index;

                                aq:=5;
                                edge_started:=False;
                                for now:=intarray_max(list_bgnd_rails[aq,0]) downto 0 do begin // and back along the gauge face to the tip.
                                  x_dots:=pbg_get_w_dots(aq,now);
                                  y_dots:=pbg_get_l_dots(aq,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now

                                with on_canvas do begin

                                  set_pen_railcolour;

                                  Pen.Width:=printrail_wide;
                                  if Pen.Width<1 then Pen.Width:=1;

                                  if ( (using_mapping_colour=True) and (Pen.Color=mapping_colour) ) or ( (mapping_colours_print<0) and (Pen.Color=printbg_single_colour) )
                                     then Brush.Color:=calc_intensity(clGray)
                                     else begin
                                            if fb_kludge_this>0 then Brush.Color:=printrail_infill_colour_cu    // 0.94.a
                                                                else Brush.Color:=printrail_infill_colour_bg;
                                          end;

                                  case rail_infill_i of
                                                  1: Brush.Style:=bsBDiagonal;   // hatched
                                                  2: Brush.Style:=bsSolid;       // solid
                                                  3: Brush.Style:=bsDiagCross;   // cross_hatched
                                                  4: begin                       // blank
                                                       Brush.Style:=bsSolid;
                                                       Brush.Color:=clWhite;     // overide
                                                     end;
                                                else Brush.Style:=bsSolid;       // solid
                                  end;//case

                                  if dots_index>4
                                     then begin
                                            swap_polygon(on_canvas,canvas_height,Slice(dots,dots_index+1));   // +1, number of points, not index.  must have at least 5 points.

                                            edge_colour:=Pen.Color;  // existing rail edges.

                                            if Brush.Style=bsSolid
                                               then blanking_colour:=Brush.Color   // infill colour.
                                               else begin   // 206b hatched fill...
                                                            // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

                                                      case output_code of
                                                        1,2: blanking_colour:=Brush.Color; { T3-FIRST dtp_settings_form.sb_page_colour_panel.Color;}
                                                          3: blanking_colour:=export_form.img_bgnd_colour_panel.Color;
                                                          4: blanking_colour:=Brush.Color; // for metafile export
                                                       else  blanking_colour:=clWhite;  // assume white background (print, PDF)
                                                      end;//case
                                                    end;

                                                  // remove polygon lines across vee rail ends...

                                            pbg_modify_vee_end(point_mid_dots_index,point_mid_dots_index+1,edge_colour,blanking_colour); // point rail end.

                                            pbg_modify_vee_end(splice_mid_dots_index,splice_mid_dots_index+1,edge_colour,blanking_colour); // splice rail end.

                                          end;

                                end;//with on_canvas

                            end;//polygon mode
                    end;//with background template
                  end;
                  //////////////////////////////////////////////////////////////

                  procedure pbg_mark_end(aq1, aq1end, aq2, aq2end:integer);    // print the background rail end mark.

                  begin
                    with now_keep do begin
                      if (bgnd_endmarks_yn[aq1,aq1end]=True) and (bgnd_endmarks_yn[aq2,aq2end]=True)
                         then begin
                                p1:=bgnd_endmarks[aq1,aq1end];
                                p2:=bgnd_endmarks[aq2,aq2end];

                                with on_canvas do begin

                                  set_pen_railcolour;

                                  Pen.Width:=printrail_wide;
                                  if Pen.Width<1 then Pen.Width:=1;

                                  move_to.X:=Round((p1.Y-grid_left)*scaw_out)+page_left_dots;
                                  move_to.Y:=Round((p1.X-grid_top)*scal_out)+page_top_dots;

                                  line_to.X:=Round((p2.Y-grid_left)*scaw_out)+page_left_dots;
                                  line_to.Y:=Round((p2.X-grid_top)*scal_out)+page_top_dots;

                                  if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                end;//with
                              end;
                    end;//with
                  end;
                  ////////////////////////////////////////////////////////////

                  procedure  pbg_outline_railends;  // draw in the rail ends using existing pen settings...

                  begin
                    pbg_mark_end(1,1,9,1);    // main rail wing rail finish.
                    pbg_mark_end(2,1,10,1);   // turnout rail wing rail finish.

                    pbg_mark_end(6,0,14,0);   // main side check rail start.
                    pbg_mark_end(6,1,14,1);   // main side check rail finish.

                    pbg_mark_end(7,0,15,0);   // turnout side check rail start.
                    pbg_mark_end(7,1,15,1);   // turnout side check rail finish.

                    pbg_mark_end(4,0,5,0);    // blunt nose.

                    if fixed_diamond_ends=True
                             then begin
                                    pbg_mark_end(1,0,9,0);   // planed faced of point rails for a fixed-diamond.
                                    pbg_mark_end(2,0,10,0);

                                    pbg_mark_end(26,1,27,1);     // MS K-crossing check rails.
                                    pbg_mark_end(28,1,29,1);     // DS K-crossing check rails.
                                  end;

                  end;
                  //////////////////////////////////////////////////////////////

                  procedure pbg_draw_diagram_mode;    // 0.91.d draw a complete template in diagrammatic mode (main rails)

                  const
                    dots_max_c=xy_pts_c*2;    // max 6000        xy_pts_c=3000

                  var
                    dots:array[0..dots_max_c] of TPoint;     // array of points for filled polygon mode.

                      //   3000 points for each side of rail. if incx is 18" scale (SQRT 9ft scale in 4mm = SQRT(36) =6 mm),
                      //   template max is 4500' scale length.
                      //   ( = 18000 mm or 59ft approx in 4 mm scale).
                      //   ( = 66 A4 sheets long if straight turnout - but normally less for curved turnout).

                      // N.B. huge standard Pascal array is used instead of our own dynamic integer arrays,
                      // because needed for the Polygon function.

                      // total memory = 6000*8 bytes = 48KB

                    now,now_max:integer;
                    edge_started:boolean;
                    dots_index:integer;
                    x_dots,y_dots:integer;
                    ms_mid_dots_index,ts_mid_dots_index:integer;
                    edge_colour, blanking_colour:integer;

                    no_vee:boolean;

                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                  procedure pbg_modify_boundary(start_index,stop_index,edge,blank:integer);

                                  begin

                                    if (start_index>=0) and (start_index<=dots_index) and (stop_index>=0) and (stop_index<=dots_index)
                                       then begin
                                              move_to:=dots[start_index];
                                              line_to:=dots[stop_index];

                                              if check_limits(move_to, line_to)=True
                                                 then begin
                                                        with on_canvas do begin
                                                          Pen.Color:=blank;                // first blank across..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);

                                                          Pen.Color:=edge;                 // then restore the corner points..
                                                          swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,move_to.X, move_to.Y);

                                                          swap_move_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                          swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y);
                                                        end;//with
                                                      end;
                                            end;
                                  end;
                                  //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                  begin
                    with now_keep do begin

                      if intarray_max(list_bgnd_rails[0,0])=0 then EXIT;    // no data for straight stock rail
                      if intarray_max(list_bgnd_rails[3,0])=0 then EXIT;    // no data for curved stock rail

                      if (intarray_max(list_bgnd_rails[4,0])=0) or (intarray_max(list_bgnd_rails[5,0])=0)   // no data for vee rails
                         then no_vee:=True
                         else no_vee:=False;

                      dots_index:=0-1;                    // first increment is to zero.


                      now_max:=intarray_max(list_bgnd_rails[0,0]);    // straight stock rail
                      edge_started:=False;

                      for now:=0 to now_max do begin
                        x_dots:=pbg_get_w_dots(0,now);
                        y_dots:=pbg_get_l_dots(0,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      ms_mid_dots_index:=dots_index;

                      if no_vee=False
                         then begin
                                now_max:=intarray_max(list_bgnd_rails[4,0]);    // point rail
                                edge_started:=False;

                                for now:=now_max downto 0 do begin
                                  x_dots:=pbg_get_w_dots(4,now);
                                  y_dots:=pbg_get_l_dots(4,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now


                                now_max:=intarray_max(list_bgnd_rails[5,0]);    // splice rail
                                edge_started:=False;

                                for now:=0 to now_max do begin
                                  x_dots:=pbg_get_w_dots(5,now);
                                  y_dots:=pbg_get_l_dots(5,now);
                                  if (w_dims_valid=True) and (l_dims_valid=True)
                                     then begin
                                            edge_started:=True;

                                            Inc(dots_index);
                                            if dots_index>dots_max_c then dots_index:=dots_max_c;

                                            dots[dots_index].X:=x_dots;
                                            dots[dots_index].Y:=y_dots;
                                          end
                                     else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                                end;//next now
                              end;//if vee

                      ts_mid_dots_index:=dots_index;

                      now_max:=intarray_max(list_bgnd_rails[3,0]);    // curved stock rail
                      edge_started:=False;

                      for now:=now_max downto 0 do begin
                        x_dots:=pbg_get_w_dots(3,now);
                        y_dots:=pbg_get_l_dots(3,now);
                        if (w_dims_valid=True) and (l_dims_valid=True)
                           then begin
                                  edge_started:=True;

                                  Inc(dots_index);
                                  if dots_index>dots_max_c then dots_index:=dots_max_c;

                                  dots[dots_index].X:=x_dots;
                                  dots[dots_index].Y:=y_dots;
                                end
                           else if edge_started=True then BREAK;   // don't resume adding dots to this edge once started and then gone out of limits.
                      end;//next now

                      with on_canvas do begin

                        set_pen_railcolour;

                        Pen.Width:=printrail_wide;
                        if Pen.Width<1 then Pen.Width:=1;

                        if using_mapping_colour=True
                           then Brush.Color:=mapping_colour
                           else Brush.Color:=sb_diagram_colour; // 209c  was printrail_infill_colour_bg;

                        case rail_infill_i of
                                        1: Brush.Style:=bsBDiagonal;   // hatched
                                        2: Brush.Style:=bsSolid;       // solid
                                        3: Brush.Style:=bsDiagCross;   // cross_hatched
                                        4: begin                       // blank
                                             Brush.Style:=bsSolid;
                                             Brush.Color:=clWhite;     // overide
                                           end;
                                      else Brush.Style:=bsSolid;       // solid
                        end;//case

                        if dots_index>2
                           then begin
                                  swap_polygon(on_canvas,canvas_height,Slice(dots,dots_index+1));   // +1, number of points, not index.  must have 4 points.

                                               // blank out template boundaries...

                                  if output_include_boundaries=False
                                     then begin
                                            edge_colour:=Pen.Color;  // existing rail edges.

                                            if Brush.Style=bsSolid
                                               then blanking_colour:=Brush.Color   // infill colour.
                                               else begin   // 206b hatched fill...
                                                            // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

                                                      case output_code of
                                                        1,2: blanking_colour:=Brush.Color; { T3-FIRST dtp_settings_form.sb_page_colour_panel.Color;}
                                                          3: blanking_colour:=export_form.img_bgnd_colour_panel.Color;
                                                          4: blanking_colour:=Brush.Color; // for metafile export
                                                       else  blanking_colour:=clWhite;  // assume white background (print, PDF)
                                                      end;//case
                                                    end;

                                            pbg_modify_boundary(0,dots_index,edge_colour,blanking_colour);  // toe or approach end.

                                            pbg_modify_boundary(ms_mid_dots_index,ms_mid_dots_index+1,edge_colour,blanking_colour);  // exit end (turnout) or Ctrl-1 end (plain track).

                                            if no_vee=False then pbg_modify_boundary(ts_mid_dots_index,ts_mid_dots_index+1,edge_colour,blanking_colour);  // turnout road end.
                                          end;
                                end;

                        if output_show_points_mark=True  // mark position of points
                           then begin

                                   if  (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.turnout_info1.plain_track_flag=False)   // not for plain track
                                   and (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.turnout_info2.semi_diamond_flag=False)            // not for half-diamond
                                   and (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.turnout_info2.gaunt_flag=False)                   // not for gaunt turnout

                                   and (intarray_max(list_bgnd_rails[1,0])<>0)    // data for straight switch rail
                                   and (intarray_max(list_bgnd_rails[2,0])<>0)    // data for curved stock rail
                                       then begin
                                              if Brush.Color=clWhite
                                                 then Pen.Color:=clBlack
                                                 else Pen.Color:=clWhite;     // white points mark

                                              x_dots:=pbg_get_w_dots(1,0);  // ms toe.
                                              y_dots:=pbg_get_l_dots(1,0);

                                              if (w_dims_valid=True) and (l_dims_valid=True)
                                              then begin
                                                     swap_move_to(on_canvas,canvas_height,x_dots,y_dots);

                                                     x_dots:=pbg_get_w_dots(2,0);  // ts toe.
                                                     y_dots:=pbg_get_l_dots(2,0);

                                                     if (w_dims_valid=True) and (l_dims_valid=True)
                                                        then swap_line_to(on_canvas,canvas_height,x_dots,y_dots);
                                                   end;
                                            end;

                                end;//mark points
                      end;//with on_canvas
                    end;//with background template
                  end;
                  //////////////////////////////////////////////////////////////


begin          // export background templates...

  max_list_index:=keeps_list.Count-1;

  if max_list_index<0 then EXIT;  // no templates in box.

  cl_warning_shown:=False;  // init 206a

  if output_diagram_mode=False    // first the timbering...
     then begin

            if (output_code>2)       // not sketchboard

            { T3-FIRST or (dtp_settings_form.include_track_checkbox.Checked=True) }

               then export_bgnd_marks(on_canvas,canvas_height,grid_left,grid_top,max_list_index,False); // 091d if // first print all the background timbering and marks except rail joints.

          end;

  with on_canvas do begin

    Pen.Mode:=pmCopy;     // default


                  // or do all track backgrounds, if any ...  206a

    if output_diagram_mode=True    // mods 206a for track background
       then begin
                // first do track background as wide centre-lines  // 206a
                // output_code 1=sketchboard bitmap, 2=sketchboard metafile, 3=create image file, 4=create EMF file

              if ( (output_code<3) { T3-FIRST and (dtp_settings_form.track_background_checkbox.Checked=True)} )   // sketchboard
              or ( (output_code>2) and (export_form.export_track_background_checkbox.Checked=True) )              // export image file
                 then begin

                        Pen.Mode:=pmCopy;
                        Pen.Style:=psSolid;
                        Pen.Color:=sb_track_bgnd_colour;

                        for n:=0 to max_list_index do begin

                          if Ttemplate(keeps_list.Objects[n]).bg_copied=False then CONTINUE;  // no data, not on background.

                          if (Ttemplate(keeps_list.Objects[n]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

                          if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.fb_kludge_template_code>0 then CONTINUE;  // 209c no track background for fb_kludge templates

                          if Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.align_info.dummy_template_flag=True then CONTINUE;  // 212a dummy templates not part of track plan

                          with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims do begin

                            if box_dims1.bgnd_code_077<>1 then CONTINUE;    // 0.77.b BUG???

                            Pen.Width:=Round(scaw_out*track_bgnd_width_in*box_dims1.proto_info.scale_pi*100/12);    // scaw_out = dots per 1/100th mm. (at required output scaling).
                            if Pen.Width<1 then Pen.Width:=1;

                          end;//with

                          now_keep:=Ttemplate(keeps_list.Objects[n]).bgnd_keep;    // next background keep.

                          with now_keep do begin

                            for aq:=24 to 25 do begin         // use wide track centre-lines as track background

                              array_max:=intarray_max(list_bgnd_rails[aq,0]);
                              if array_max=0
                                 then begin
                                        if (aq=24) and (cl_warning_shown=False)   // only checks main road in case plain track
                                           then begin
                                                  show_modal_message('N.B. This trackplan contains one or more templates which have been created without track centre-lines.'
                                                     +#13+#13+'It is not possible to display a track background for these templates.');
                                                  cl_warning_shown:=True;
                                                end;
                                        CONTINUE;                      // empty rail.
                                      end;

                              xint:=intarray_get(list_bgnd_rails[aq,0],0);
                              yint:=intarray_get(list_bgnd_rails[aq,1],0);

                              move_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                              move_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                              for nk:=1 to array_max do begin

                                xint:=intarray_get(list_bgnd_rails[aq,0],nk);
                                yint:=intarray_get(list_bgnd_rails[aq,1],nk);

                                line_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                                line_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                                if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                move_to:=line_to;
                              end;//next nk

                            end;//next aq
                          end;//with now_keep
                        end;//next template
                      end;//backgrounds wanted
            end;//diagram mode

            // all done if he doesn't want the actual track ...

    if  (output_code<3)              // sketchboard
    { T3-FIRST and (dtp_settings_form.include_track_checkbox.Checked=False)}
        then EXIT;

                  //  now export bgnd track centre-lines and turnout rails...

    for n:=0 to max_list_index do begin

      if Ttemplate(keeps_list.Objects[n]).bg_copied=False then CONTINUE;  // no data, not on background.

      if (Ttemplate(keeps_list.Objects[n]).group_selected=False) and (print_group_only_flag=True) then CONTINUE;  // not in group. 0.78.b 10-12-02.

      if  (Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.fb_kludge_template_code>0)  // 209c
      and (print_settings_form.output_fb_foot_lines_checkbox.Checked=False)
          then CONTINUE;                                                      // foot lines not wanted.

      this_one_platforms_trackbed:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.adjacent_edges_keep;           // True = platforms and trackbed edges   206b

      this_one_trackbed_cess_ms:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ms_trackbed_cess_edge_keep;  // True = cess width instead of trackbed cutting line 215a
      this_one_trackbed_cess_ts:=Ttemplate(keeps_list.Objects[n]).template_info.keep_dims.box_dims1.platform_trackbed_info.draw_ts_trackbed_cess_edge_keep;  // True = cess width instead of trackbed cutting line 215a


      now_keep:=Ttemplate(keeps_list.Objects[n]).bgnd_keep;    // next background keep.

      with now_keep do begin

        using_mapping_colour:=False;  // default init.

        with Ttemplate(keeps_list.Objects[n]).template_info.keep_dims do begin

          if box_dims1.bgnd_code_077<>1 then CONTINUE;    // 0.77.b BUG???

          with turnout_info2 do begin
            fixed_diamond_ends:=(semi_diamond_flag=True) and (diamond_fixed_flag=True);    // need end marks on fixed diamond point rails.
            gaunt_template:=gaunt_flag;                                                    // 093a ex 081
          end;//with

          if (box_dims1.use_print_mapping_colour=True)
             and ( (mapping_colours_print=1) or (mapping_colours_print=3) )
             and (export_black_white=False)
             and (export_grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(box_dims1.print_mapping_colour);
                        using_mapping_colour:=True;
                      end;

          if (box_dims1.use_pad_marker_colour=True)
             and (mapping_colours_print=4)   // use pad settings instead
             and (export_black_white=False)
             and (export_grey_shade=False)
                 then begin
                        mapping_colour:=calc_intensity(box_dims1.pad_marker_colour);
                        using_mapping_colour:=True;
                      end;

          fb_kludge_this:=box_dims1.fb_kludge_template_code;  // 094a

          if fb_kludge_this=0   // no track centre-lines or diagram mode for kludge templates  212a
             then begin

                    if output_diagram_mode=True
                       then pbg_draw_diagram_mode;  // now draw template in diagrammatic mode (main rails).

                    if ((print_settings_form.output_centrelines_checkbox.Checked=True) and (output_diagram_mode=False) and (box_dims1.align_info.dummy_template_flag=False))
                    or ((print_settings_form.output_bgnd_shapes_checkbox.Checked=True) and (box_dims1.align_info.dummy_template_flag=True))  // 212a dummy templates not part of track plan

                       then begin

                              Brush.Color:=clWhite;  // 0.93.a gaps in dotted lines.
                              Brush.Style:=bsClear;
                              TextOut(0,0,'');

                              Pen.Mode:=pmCopy;

                              if box_dims1.align_info.dummy_template_flag=True   // 212a   dummy template as bgnd shapes
                                 then begin
                                        Pen.Style:=psSolid;
                                        Pen.Color:=printshape_colour;
                                        Pen.Width:=printshape_wide;
                                      end
                                 else begin

                                        set_pen_railcolour;

                                        Pen.Width:=printcl_wide;
                                        if Pen.Width<1 then Pen.Width:=1;

                                        if Pen.Width=1 then Pen.Style:=psDash
                                                       else Pen.Style:=psSolid;

                                      end;

                              for aq:=24 to 25 do begin         // track centre-lines.

                                array_max:=intarray_max(list_bgnd_rails[aq,0]);
                                if array_max=0 then CONTINUE;                      // empty rail.

                                xint:=intarray_get(list_bgnd_rails[aq,0],0);
                                yint:=intarray_get(list_bgnd_rails[aq,1],0);

                                move_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                                move_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                                for nk:=1 to array_max do begin

                                  xint:=intarray_get(list_bgnd_rails[aq,0],nk);
                                  yint:=intarray_get(list_bgnd_rails[aq,1],nk);

                                  line_to.X:=Round((yint-grid_left)*scaw_out)+page_left_dots;
                                  line_to.Y:=Round((xint-grid_top)*scal_out)+page_top_dots;

                                  if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                  move_to:=line_to;
                                end;//next nk

                              end;//next aq
                            end;//if track-centres

                  end;//if not kludge

        end;//with template

        if print_settings_form.output_rails_checkbox.Checked=True
           then begin
                  Pen.Mode:=pmCopy;
                  Pen.Style:=psSolid;

                  Pen.Width:=printrail_wide;
                  if Pen.Width<1 then Pen.Width:=1;

                  if (rail_infill_i=0)  // out for pdf, was  or ((scale*out_factor)<0.75)   // less than 18.75% for 4mm scale (control template) (10.71% for 7mm).
                  and (output_diagram_mode=False)
                     then begin           //  outline (pen) mode ...
                                          //  n.b. this mode does not automatically close the rail-ends.
                                          //  no infill for platforms

                            set_pen_railcolour;

                            for aq:=0 to 23 do begin                                             // 24, 25 centre-lines already done.
                              if (this_one_platforms_trackbed=False) and (aq>15) then CONTINUE;  // no adjacent tracks in output  // 206b

                              case aq of     // 223d
                                16,17,20,21: if print_settings_form.output_platforms_checkbox.Checked=False then CONTINUE;         // platforms not wanted
                                18,19,22,23: if print_settings_form.output_trackbed_edges_checkbox.Checked=False then CONTINUE;    // trackbed edges not wanted
                              end;//case

                              pbg_outline_railedge(aq,0,False);
                            end;

                            for aq:=26 to aq_max_c do pbg_outline_railedge(aq,0,False);   // K-crossing check rails.

                            pbg_outline_railends;  // next, draw in the rail ends using same pen settings...
                          end
                     else begin      // infill (polygon) mode ...

                                     // do blades first - neater result.

                            if output_diagram_mode=False // detail mode only
                               then begin

                                      for rail:=1 to 3 do pbg_draw_fill_rail(8);  // closure rails and curved stock rail.

                                      rail:=0;                                    // straight stock rail.
                                      pbg_draw_fill_rail(8);

                                      for rail:=6 to 7 do pbg_draw_fill_rail(8);  // check rails
                                    end;

                            if this_one_platforms_trackbed=True   // no adjacent tracks in output 206b
                               then begin
                                      rail:=16;     // detail mode or diagram mode (for platforms/trackbed)
                                      repeat
                                        case rail of     // 223d
                                          16,20: if print_settings_form.output_platforms_checkbox.Checked=True then pbg_draw_fill_rail(1);        // platforms
                                          18,22: if print_settings_form.output_trackbed_edges_checkbox.Checked=True then pbg_draw_fill_rail(1);   // trackbed edges
                                        end;//case
                                        rail:=rail+2;
                                        if (output_diagram_mode=True) and (output_include_trackbed_edges=False) and ((rail=18) or (rail=22)) then rail:=rail+2;  // 206b no trackbed/cess in diagram mode
                                      until rail>22;
                                    end;

                            if output_diagram_mode=False // detail mode only
                               then begin
                                      rail:=26;
                                      repeat
                                        pbg_draw_fill_rail(1);                    // K-crossing check rails.
                                        rail:=rail+2;
                                      until rail>28;

                                      pbg_draw_fill_vee;    // do complete vee in one go ...

                                                            // finally draw in the planing gauge-faces - no infill...

                                      set_pen_railcolour;

                                      aq:=1;

                                      if (intarray_max(list_bgnd_rails[aq,0])<>0) and (planing_end_aq1>0) { and (drawn_full_aq1=False)}
                                         then begin
                                                move_to.X:=pbg_get_w_dots(aq,0); move_to.Y:=pbg_get_l_dots(aq,0);
                                                for now:=1 to planing_end_aq1{+1} do begin
                                                  line_to.X:=pbg_get_w_dots(aq,now); line_to.Y:=pbg_get_l_dots(aq,now);
                                                  if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                                  move_to:=line_to;
                                                end;//for
                                              end;

                                      aq:=2;
                                      if (intarray_max(list_bgnd_rails[aq,0])<>0) and (planing_end_aq2>0) { and (drawn_full_aq2=False)}
                                         then begin
                                                move_to.X:=pbg_get_w_dots(aq,0); move_to.Y:=pbg_get_l_dots(aq,0);
                                                for now:=1 to planing_end_aq2{+1} do begin
                                                  line_to.X:=pbg_get_w_dots(aq,now); line_to.Y:=pbg_get_l_dots(aq,now);
                                                  if check_limits(move_to, line_to)=True then begin swap_move_to(on_canvas,canvas_height,move_to.X, move_to.Y); swap_line_to(on_canvas,canvas_height,line_to.X, line_to.Y); end;
                                                  move_to:=line_to;
                                                end;//for
                                              end;

                                    end;//detail mode

                          end;//polygon mode

                end;//if rails

      end;//with bgnd_keep
    end;//for next n template
  end;//with on_canvas

           // finally add the rail-joint marks over the rail infill...   // 209c moved outside loop

  if (print_settings_form.output_rails_checkbox.Checked=True) and (output_diagram_mode=False)
     then export_bgnd_marks(on_canvas,canvas_height,grid_left,grid_top,max_list_index,True);

end;
//______________________________________________________________________________


end.

