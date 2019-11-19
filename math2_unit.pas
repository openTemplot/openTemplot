
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
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

====================================================================================
*)

unit math2_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, MaskEdit, FileCtrl, Math,

  pad_unit;      //  need Tpex declaration in this part for parameters to routines.

function do_notch_on_intersection(making_diamond,move_notch:boolean; rail_offset_control,rail_offset_bgnd:integer; top_str,next_str:string):boolean;

procedure do_make_diamond_crossing_at_intersection; // pad menu link

function make_diamond_crossing_at_intersection:boolean;

function notch_on_intersection(move_notch:boolean; control_rail_offset,bgnd_rail_offset:integer):integer;
function get_circle_intersections(x1,y1,r1, x2,y2,r2:extended; var qx1,qy1,k1_r1,k1_r2, qx2,qy2,k2_r1,k2_r2:extended):integer;

function check_if_control_template_on_screen:boolean;

procedure extend_to_boundary(index,boundary:integer; grow:boolean);  // 213b

function grow_prune_to_meet(boundary1,boundary2:Tnotch; index1,index2:integer; grow,from_zero:boolean):boolean;   // 213b  extend/shorten control template to meet a matching bgnd boundary


implementation

uses
  control_room, math_unit, info_unit, keep_select, shove_timber, alert_unit, help_sheet, grid_unit, wait_message;

var
  dummy:extended=0;

  which_one:integer=1;       // which_one = +1 or -1 to select which intersection
  xing_angle:extended=0;     // radians

  x_for_notch:extended=0;
  y_for_notch:extended=0;

  fail_code:integer=0;

//______________________________________________________________________________

function check_if_control_template_on_screen:boolean;

  // return True if any part of the control template enclosing rectangle will be on the screen (central 80%).
  // part of intersection selector for the user.

var
  ctx_max:extended;
  ctx_min:extended;

  cty_max:extended;
  cty_min:extended;

  scx_max:extended;
  scx_min:extended;

  scy_max:extended;
  scy_min:extended;

begin
  RESULT:=False;  // init

    // 80% screen corners around intersection...

  scx_max:=x_for_notch+screenx/2.5;
  scx_min:=x_for_notch-screenx/2.5;

  scy_max:=y_for_notch+screeny/2.5;
  scy_min:=y_for_notch-screeny/2.5;

    // control template extents in 1/100th mm, convert to mm ...

  ctx_max:=xy_max[0]/100;              // top right corner
  cty_max:=y_datum+(xy_max[1]/100);

  ctx_min:=xy_min[0]/100;              // bottom left corner
  cty_min:=y_datum+xy_min[1]/100;

  if ctx_max<scx_min then EXIT;    // control template entirely to left of screen

  if ctx_min>scx_max then EXIT;    // control template entirely to right of screen

  if cty_max<scy_min then EXIT;    // control template entirely below screen

  if cty_min>scy_max then EXIT;    // control template entirely above screen

  RESULT:=True;  // must be some overlap of rectangles
end;
//______________________________________________________________________________

function calc_radial_angle(xc,yc,x,y:extended):extended;

  // return all positive angles (acw), so we can do arithmetic on them easily

var
  k:extended;

begin
  RESULT:=0;          // init
  k:=0;

  if ABS(x-xc)<minfp
     then begin
            if y>=yc                   // north from centre
               then RESULT:=Pi/2
               else RESULT:=Pi+Pi/2;   // south from centre
            EXIT;
          end;

  if ABS(y-yc)<minfp
     then begin
            if x>=xc              // east from centre
               then RESULT:=0
               else RESULT:=Pi;   // west from centre
            EXIT;
          end;

  k:=ARCTAN(ABS(y-yc)/ABS(x-xc));

  RESULT:=k; // north-east

  if (x>xc) and (y<yc) then RESULT:=Pi*2-k;  // south-east
  if (x<xc) and (y>yc) then RESULT:=Pi-k;    // north-west
  if (x<xc) and (y<yc) then RESULT:=Pi+k;    // south-west
end;
//______________________________________________________________________________

// This is a public domain work. 26/3/2005 Tim Voght

function get_circle_intersections(x1,y1,r1, x2,y2,r2:extended; var qx1,qy1,k1_r1,k1_r2, qx2,qy2,k2_r1,k2_r2:extended):integer;

   // return code:
   // 2 = OK, two usable intersections
   // 1 = OK, but only one intersection is usable
   // 0 = OK, but neither intersection is usable
   // -1 = FAIL, one circle is completely outside the other
   // -2 = FAIL, one circle is completely inside the other
   // -3 = FAIL, circles are identical copies
   // -4 = FAIL, calculation exception

   // return intersections at q1 and q2   // k is radial angle to intersection, not rail angle.

var
  a,dx,dy,d,h,rx,ry,xp,yp:extended;

  limit_dim:extended;

begin
  RESULT:=2;  // init good result

  // fail inits for returned vars ...

  qx1:=0;
  qy1:=0;

  qy2:=0;
  qx2:=0;

  k1_r1:=0;
  k1_r2:=0;

  k2_r1:=0;
  k2_r2:=0;

  r1:=ABS(r1);  // prevent exceptions
  r2:=ABS(r2);

  if (r1<scale) or (r2<scale)  // sensible limits
     then begin
            RESULT:=0-4;
            EXIT;
          end;

  try

    if (x1=x2) and (y1=y2) and (r1=r2)
       then begin
              RESULT:=0-3;   // circles identical
              EXIT;
            end;

         // X and Y distances between the circle centres..

    dx:=x2-x1;
    dy:=y2-y1;

    d:=SQRT(SQR(dy)+SQR(dx));    // diagonal distance between the centres

    if d>(r1+r2)       // no intersection, gap between circles
       then begin
              if (d-(r1+r2))<1E-9   // if by tiny amount, adjust r2 in the calcs. 1E-9 arbitrary. templates not changed.
                 then begin
                        r2:=r2+1E-9;
                        RESULT:=1;    // only one intersect
                      end
                 else begin
                        RESULT:=0-1;
                        EXIT;        // circles do not intersect
                      end;
            end
       else begin
              if d<ABS(r1-r2)   // one within other
                 then begin
                        if (ABS(r1-r2)-d)<1E-9   // if by tiny amount, adjust r2 in the calcs. 1E-9 arbitrary. templates not changed.
                           then begin
                                  if r2>r1 then r2:=r2-1E-9
                                           else r2:=r2+1E-9;
                                  RESULT:=1;
                                end
                           else begin
                                  RESULT:=0-2;
                                  EXIT;        // one circle is contained in the other
                                end;
                      end;
            end;

          // xp,yp is the point where the line through the circle intersection points
          // crosses the line between the circle centres ...

    a:=(SQR(r1)-SQR(r2)+SQR(d))/2/d;    // distance point 0 to point 2


    xp:=x1+(dx*a/d);  // coordinates of point p
    yp:=y1+(dy*a/d);

         // get the distance from point p to either of the intersection points ...


    if (SQR(r1)-SQR(a))<=0    // no neg
       then h:=0
       else h:=SQRT(SQR(r1)-SQR(a));

         // get the offsets of the intersection points from point 2 ...

    rx:=0-dy*h/d;
    ry:=dx*h/d;

        // return the intersection points q ...

    qx1:=xp+rx;    // 1st intersect..
    qy1:=yp+ry;

    qy2:=yp-ry;    // 2nd intersect..
    qx2:=xp-rx;

    if (ABS(qx1-qx2)<1E-4) and (ABS(qy1-qy2)<1E-4) then RESULT:=1;  // coincident, effectively only one intersection. 1E-4 arbitrary.

    limit_dim:=max_rad_test/10;  // arbitrary screen limit for sensible display

    if  ( (ABS(qx1)>limit_dim) or (ABS(qy1)>limit_dim) )
    and ( (ABS(qx2)>limit_dim) or (ABS(qy2)>limit_dim) )   // neither can sensibly be displayed
        then begin
               RESULT:=0;
               EXIT;
             end;

    if (ABS(qx1)>limit_dim) or (ABS(qy1)>limit_dim)   // q1 can't sensibly be displayed
       then begin
              RESULT:=1;
              qx1:=qx2;    // so duplicate q2
              qy1:=qy2;
            end;

    if (ABS(qx2)>limit_dim) or (ABS(qy2)>limit_dim)   // q2 can't sensibly be displayed
       then begin
              RESULT:=1;
              qx2:=qx1;    // so duplicate q1
              qy2:=qy1;
            end;

           // radial angles from centres ...

    k1_r1:=calc_radial_angle(x1,y1,qx1,qy1);     // to 1st intersect..
    k1_r2:=calc_radial_angle(x2,y2,qx1,qy1);

    k2_r1:=calc_radial_angle(x1,y1,qx2,qy2);     // to 2nd intersect..
    k2_r2:=calc_radial_angle(x2,y2,qx2,qy2);

  except
    RESULT:=0-4;
    EXIT;
  end;//try
end;
//______________________________________________________________________________

function notch_on_intersection(move_notch:boolean; control_rail_offset,bgnd_rail_offset:integer):integer;

   // return code:
   // 2 = OK, two usable intersections
   // 1 = OK, but only one intersection is usable, which_one ignored
   // 0 = OK, but neither intersection is usable
   // -1 = FAIL, one circle is completely outside the other
   // -2 = FAIL, one circle is completely inside the other
   // -3 = FAIL, circles are identical copies
   // -4 = FAIL, calculation exception
   // -5 = FAIL, transition template
   // -6 = FAIL, slew template
   // -7 = FAIL, invalid control template

   // returns also the angle difference at the notch

   // move_notch=False means here for calcs only, no visual change

var
  saved_current:Ttemplate_info;

  new_notch_data:Tnotch;

  saved_name_str:string;
  saved_memo_str:string;

  x1,y1,r1:extended;
  x2,y2,r2:extended;
  xi,yi,xj,yj:extended;

  k1_i,k1_j,k2_i,k2_j:extended;

  k:extended;

                          ///////////////////////////////////////////////////////////////////

                          procedure restore_current;

                          begin
                            copy_keep(saved_current);                    // retrieve saved current.
                            current_name_str:=saved_name_str;
                            current_memo_str:=saved_memo_str;

                            info_form.ref_name_label.Caption:=current_name_str;
                          end;
                          ////////////////////////////////////////////////////////////////////


begin
  RESULT:=0-4;               // default fail init.

  xing_angle:=0;
  x_for_notch:=0;
  y_for_notch:=0;

  with new_notch_data do begin
    notch_x:=0;
    notch_y:=0;
    notch_k:=0;
  end;//with

  if (clicked_keep_index<0) or (clicked_keep_index>(keeps_list.Count-1)) or (keeps_list.Count<1) then EXIT;

  if check_control_template_is_valid('intersection')=False
     then begin
            RESULT:=0-7;
            EXIT;  // zero length
          end;

  if spiral=True
     then begin
            RESULT:=0-5;
            EXIT;
          end;

   if slewing=True
       then begin
              RESULT:=0-6;
              EXIT;
            end;

        // save the control ...

  saved_current.keep_shove_list:=TStringList.Create;   // local stringlist not initialised.
  fill_kd(saved_current);                              // save control template
  saved_name_str:=current_name_str;
  saved_memo_str:=current_memo_str;

  try   // finally

    try  // except

              // get data for current control template ...

      gocalc(0,0);

      x1:=rad1_orgx;
      y1:=rad1_orgy;
      r1:=ABS(nomrad)+control_rail_offset*g/2;

          // now get the background template, and repeat...

      list_position:=clicked_keep_index;              // make it current in the keeps box.
      copy_keep_to_current(False,False,True,False);   // copy to pad.

          // get data for the bgnd template ...

      gocalc(0,0);

      if check_control_template_is_valid('intersection')=False
         then begin
                RESULT:=0-7;
                EXIT;  // zero length
              end;

      if spiral=True
         then begin
                RESULT:=0-5;
                EXIT;
              end;

       if slewing=True
           then begin
                  RESULT:=0-6;
                  EXIT;
                end;

      x2:=rad1_orgx;
      y2:=rad1_orgy;
      r2:=ABS(nomrad)+bgnd_rail_offset*g/2;   // (bgnd template may be n.g. but is now the control)

      RESULT:=get_circle_intersections(x1,y1,r1, x2,y2,r2, xi,yi,k1_i,k2_i, xj,yj,k1_j,k2_j);

      if RESULT<1 then EXIT;  // no usable intersections

      if which_one=1         // if RESULT=1 both of these are the same...
         then begin
                with new_notch_data do begin
                  notch_x:=xi;
                  notch_y:=yi;
                  notch_k:=k1_i-Pi/2;  // align with control template
                end;//with

                k:=ABS(k1_i-k2_i);        // angle difference
              end
         else begin
                with new_notch_data do begin
                  notch_x:=xj;
                  notch_y:=yj;
                  notch_k:=k1_j-Pi/2;  // align with control template
                end;//with

                k:=ABS(k1_j-k2_j);        // angle difference
              end;

            // return data (globals) ...

      if k>Pi then k:=Pi*2-k;        // adjust angle ..
      if k>(Pi/2) then k:=Pi-k;

      xing_angle:=ABS(k);

      x_for_notch:=new_notch_data.notch_x;
      y_for_notch:=new_notch_data.notch_y;

      if move_notch=True
         then begin
                pad_form.notch_unlinked_from_current_menu_entry.Click;    // cancel any moving the notch in mouse actions.

                new_notch(new_notch_data,True);      // new data, and link group if wanted.

                redraw_pad(True,False);       // no need to put this change in rollback register on redraw.
              end;

    except
      RESULT:=0-4;
      EXIT;
    end;//try

  finally
    restore_current;                                  // restore the original control
    free_shove_list(saved_current.keep_shove_list);   // free the local stringlist.
  end;//try
end;
//______________________________________________________________________________

function do_notch_on_intersection(making_diamond,move_notch:boolean; rail_offset_control,rail_offset_bgnd:integer; top_str,next_str:string):boolean;

  // rail offsets from centre-line: 0=none, 1=outer, -1=inner

  // return True if the notch is now on an intersection

             // 0.93.a
const
  notch_intersection_help_str:string='php/109    `0Notch on Intersection`9';

var
  i,code:integer;

  angle_str,hd_str:string;

  target_showing:boolean;

  temp:extended;

begin
  RESULT:=False;  // init
  which_one:=1;   // init

  i:=0;           // keep compiler happy
  target_showing:=False;

  code:=notch_on_intersection(move_notch,rail_offset_control,rail_offset_bgnd);

  if code=2        // 2 intersections found. Show firstly the one with the control template on screen.
     then begin
            if check_if_control_template_on_screen=False
               then begin
                      which_one:=0-1;
                      code:=notch_on_intersection(move_notch,rail_offset_control,rail_offset_bgnd);  // code shouldn't change.
                    end;
          end;

  if ABS(xing_angle-Pi/2)<1E-2          // 2 decimal places shown.
     then angle_str:='90 degrees'
     else if ABS(xing_angle)<1E-2
             then angle_str:='0'
             else begin
                    temp:=TAN(xing_angle);
                    if ABS(temp)>minfp then angle_str:=round_str(xing_angle*180/Pi,2)+' degrees ( 1: '+round_str(1/temp,2)+'  RAM )'
                                       else angle_str:='0';
                  end;

  case code of

     -7: EXIT; // invalid zero-length template -- already alerted in notch_on_intersection()

     -6: i:=alert(6,'php/109    notch  on  intersection',
                   top_str+'Sorry, this function is not available because one or both templates contains a slew.'
                  +'||If the intersection is not within the slewing zone, try again after using the `0TOOLS > MAKE SPLIT >`1 menu options accordingly.'
                  +'||If the intersection is within the slewing zone, you may be able to perform this operation manually by moving the fixing peg along the rails (`0CTRL+F8`2 mouse action).',
                   '','','','more  information','cancel','',4);

     -5: i:=alert(6,'php/109    notch  on  intersection',
                   top_str+'Sorry, this function is not available because one or both templates contains a transition curve.'
                  +'||If the intersection is not within the transition zone, try again after using the TOOLS > MAKE SPLIT > menu options accordingly.'
                  +'||If the intersection is within the transition zone, you may be able to perform this operation manually by moving the fixing peg along the rails (CTRL+F8 mouse action).',
                   '','','','more  information','cancel','',4);

     -4: alert(2,'php/109    notch  on  intersection',
               top_str+'Sorry, the intersect calculations have failed to produce a result',
               '','','','','cancel','',0);

     -3: i:=alert(6,'php/109    notch  on  intersection',
               top_str+'There is no intersection because the control template and background template are both on the same alignment.',
               '','','','more  information','cancel','',4);

  -2,-1: i:=alert(6,'php/109    notch  on  intersection',
               top_str+'The control template does not intersect the background template.',
               '','','','more  information','cancel','',4);

      0: i:=alert(3,'php/109    notch  on  intersection',
               top_str+'The template intersections are too far off-screen to be usable.',
               '','','','more  information','cancel','',4);

      1: begin                              // only one usable intersect. one or both straight templates, or very large radius

           target_showing:=pad_form.show_zoom_target_menu_entry.Checked;
           pad_form.show_zoom_target_menu_entry.Checked:=True;             // show the zooming ring at centre

           pad_form.pad_on_notch_menu_entry.Click;   // centre on it
           redraw_pad(False,False);                  // and show it

           if making_diamond=True   // called from make_diamond_crossing
              then begin
                     pad_form.show_zoom_target_menu_entry.Checked:=target_showing;  // reset
                     RESULT:=True;
                   end
              else begin
                     repeat

                       with alert_box do begin          // 205d
                         left_panbutton.Visible:=True;
                         right_panbutton.Visible:=True;
                         down_panbutton.Visible:=True;
                         up_panbutton.Visible:=True;
                       end;//with

                       alert_option2a_click_code:=1;    // zoom in   205d
                       alert_option2b_click_code:=2;    // zoom out

                       i:=alert(3,'php/109    notch  on  intersection',
                               top_str+next_str+'There is only one usable intersection, now shown by the position of the notch at the centre of the trackpad.'
                              +'||The intersection angle is  '+angle_str+'.| ',
                               '','_2a_+  zoom in_2b_-  zoom out','','more  information','cancel  -  reset  notch','continue',4);

                       if i=4 then alert_help(0,notch_intersection_help_str,'');

                     until i>4;

                     pad_form.show_zoom_target_menu_entry.Checked:=target_showing;  // reset

                     if i=5 then begin
                                   pad_form.reset_notch_menu_entry.Click;
                                   EXIT;
                                 end;

                     if i=6 then RESULT:=True;
                   end;
         end;

      2: begin     // normal result, 2 curved templates ...

           target_showing:=pad_form.show_zoom_target_menu_entry.Checked;
           pad_form.show_zoom_target_menu_entry.Checked:=True;             // show the zooming ring at centre

           pad_form.pad_on_notch_menu_entry.Click;   // centre on it
           redraw_pad(False,False);                  // and show it

           repeat

              with alert_box do begin          // 205d
                left_panbutton.Visible:=True;
                right_panbutton.Visible:=True;
                down_panbutton.Visible:=True;
                up_panbutton.Visible:=True;
              end;//with

              alert_option2a_click_code:=1;    // zoom in   205d
              alert_option2b_click_code:=2;    // zoom out

              if making_diamond=True
                 then begin
                        i:=alert(7,'php/109    make  diamond - crossing  at  intersection',
                                   'This function uses the pegging notch.'
                                  +'||There are two places where the templates intersect, or would intersect if sufficiently extended in length. Templot0 needs to know which intersection is the one you want.'
                                  +'||If the place now shown by the position of the notch (ringed in yellow) is not the place where you want to make a diamond-crossing, please click|TRY OTHER INTERSECTION.'
                                  +'||If the trackpad appears to be blank or you do not understand what you are seeing, please click|TRY OTHER INTERSECTION.'
                                  +'||For some explanations and diagrams, please click|MORE INFORMATION.'
                                  +'||The intersection angle is  '+angle_str+'.',
                                   '','_2a_+  zoom in_2b_-  zoom out','more  information','try  other  intersection','cancel  diamond - crossing   -   reset  notch      ','continue   -   make  diamond - crossing  at  notch    ',3);
                      end
                 else begin
                        i:=alert(4,'php/109    notch  on  intersection',
                                   'There are two places where the templates intersect, or would intersect if sufficiently extended in length.'
                                  +'||Is the notch on the required intersection, now showing at the centre of the trackpad?'
                                  +'||The intersection angle is  '+angle_str+'.| ',
                                   '','_2a_+  zoom in_2b_-  zoom out','more  information','no  -  try  other  intersection  instead','cancel  -  reset  notch','yes  -  continue',3);
                      end;

              case i of
                     3: alert_help(0,notch_intersection_help_str,'');

                     4: begin
                          which_one:=0-which_one;
                          notch_on_intersection(move_notch,rail_offset_control,rail_offset_bgnd);  // don't need result, same dims as before
                          pad_form.pad_on_notch_menu_entry.Click;   // centre on it
                          redraw_pad(False,False);                  // and show it
                        end;

                     5: begin
                          pad_form.reset_notch_menu_entry.Click;
                          pad_form.show_zoom_target_menu_entry.Checked:=target_showing;   // reset
                          EXIT;
                        end;

                     6: RESULT:=True;

              end;//case
           until i=6;

           pad_form.show_zoom_target_menu_entry.Checked:=target_showing;   // reset

         end;

    else run_error(40);
  end;//case
  if i=4 then help(0,notch_intersection_help_str,'');

     // extra ...

  if (RESULT=True) and (plain_track=False) and (move_notch=True)
  and (rail_offset_control<>0) and (rail_offset_bgnd<>0) and (ABS(TAN(xing_angle))>minfp)
     then begin
            if 1/ABS(TAN(xing_angle))<1.5 then EXIT;

            if half_diamond=True
               then hd_str:='half-diamond'
               else hd_str:='turnout';

            repeat

              with alert_box do begin          // 205d
                left_panbutton.Visible:=True;
                right_panbutton.Visible:=True;
                down_panbutton.Visible:=True;
                up_panbutton.Visible:=True;
              end;//with

              alert_option2a_click_code:=1;    // zoom in   205d
              alert_option2b_click_code:=2;    // zoom out

              i:=alert(3,'php/111    notch  now  on  rail  intersection',
                         'The pegging notch is now on the requested rail intersection.'
                        +'||Do you want to set the '+hd_str+' in the control template to a V-crossing angle of '+angle_str
                        +' and peg the V-crossing onto the notch?'
                        +'||The fixing peg will be moved to the FP position (`0CTRL-4`2).'
                        +'||The turnout-road exit will be set to a minimum (`0GEOMETRY > TURNOUT-ROAD EXIT LENGTH >`1 menu options).'
                        +'||Select the `0CURVIFORM`z option below unless both tracks are straight or curved to the same radius in the same direction.'
                        +'||rp.gif This function will work correctly only if the '+hd_str+' in the control template has been previously set to the required hand and is facing in the required direction.'
                        +'  It may be necessary to rotate the '+hd_str+' by 180 degrees after pegging. Click the `0GEOMETRY > SHIFT/ROTATE > ROTATE 180 DEGREES`1 menu item.',
                         '','_2a_+  zoom in_2b_-  zoom out','more  information','yes  please  -  with  REGULAR  V - crossing','no  thanks','yes  please  -  with  CURVIFORM  V - crossing',3);

              if i=3 then alert_help(0,notch_intersection_help_str,'');

              if i=5 then EXIT;

            until i>3;

            k3n:=1/ABS(TAN(xing_angle));

            if i=4 then xing_type_i:=0
                   else xing_type_i:=1;

            turnout_road_i:=3;        // set minimum length to minimize mis-match

            gocalc(0,0);

            if peg_code<>4  // not already on main side peg angle.
               then pad_form.peg_on_fp_menu_entry.Click;

            gocalc(0,0);

            shift_onto_notch(False,False);

            redraw_pad(True,True);
          end;
end;
//______________________________________________________________________________

function make_diamond_crossing_at_intersection:boolean;

var
  way:integer;  // 1 = similar curvature, -1 = opposed curvature

  saved_current:Ttemplate_info;
  saved_name_str:string;
  saved_memo_str:string;


      //////////////////////////////////////////////////////////////////////////

      function diamond_calcs:boolean;  // enter with way +/- 1.

      type
        Tcrossing=record
          x:extended;
          y:extended;
          kk:extended;    // radians
	  kn:extended;    // unit angle
        end;//record

      var
        k1,k2,v1,v2:Tcrossing;    // rails
        proximity_limit:extended;
        found_it:boolean;

                            //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                            function convert_to_unit_angle(k:extended):extended;

                            begin
                              try
                                RESULT:=ABS(1/TAN(k));
                              except
                                RESULT:=0;
                              end;//try
                            end;
                            //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                            function test_proximity(xing:Tcrossing):boolean;

                                    // test proximity, generated FP to true xing ...
                                    // return True if within limit.

                            var
                              fp_loc_x,fp_loc_y:extended;
                              fp_on_pad_x,fp_on_pad_y:extended;
                              dummy1,dummy2:extended;
                            begin
                              gocalc(0,0);

                              try
                                docurving(True,True,fpx,g,fp_loc_x,fp_loc_y,dummy1,dummy2);  // curve and transform FP to get position on pad.

                                fp_on_pad_x:=fp_loc_x;
                                fp_on_pad_y:=fp_loc_y*hand_i+y_datum;

                                RESULT:=(SQRT(SQR(fp_on_pad_x-xing.x)+SQR(fp_on_pad_y-xing.y))<proximity_limit);
                              except
                                RESULT:=False;
                              end;//try

                              found_it:=RESULT;
                            end;



      begin
        RESULT:=False;  //init

          // assume which_one will be the same as already found for the centre-lines (notch now on intersection).
          // notch_on_intersection(False = notch not moved.

          // V crossing is always in same background template rail as K crossing, and in opposite control template rail.

        case way of

           -1: begin   // opposed curvature --  -- Vs are both inner or both outer.

                       // V-crossings ...

                 if notch_on_intersection(False,0-1,0-1)<1 then EXIT; // both inner rails

                 v1.x:=x_for_notch;
                 v1.y:=y_for_notch;
                 v1.kk:=xing_angle;     // radians
                 v1.kn:=convert_to_unit_angle(xing_angle);
                 if v1.kn=0 then EXIT;

                 if notch_on_intersection(False,1,1)<1 then EXIT;     // both outer rails

                 v2.x:=x_for_notch;
                 v2.y:=y_for_notch;
                 v2.kk:=xing_angle;     // radians
                 v2.kn:=convert_to_unit_angle(xing_angle);
                 if v2.kn=0 then EXIT;

                       // K-crossings ...

                 if notch_on_intersection(False,1,0-1)<1 then EXIT;   // control outer, bgnd inner

                 k1.x:=x_for_notch;
                 k1.y:=y_for_notch;
                 k1.kk:=xing_angle;     // radians
                 k1.kn:=convert_to_unit_angle(xing_angle);
                 if k1.kn=0 then EXIT;

                 if notch_on_intersection(False,0-1,1)<1 then EXIT;   // control inner, bgnd outer

                 k2.x:=x_for_notch;
                 k2.y:=y_for_notch;
                 k2.kk:=xing_angle;     // radians
                 k2.kn:=convert_to_unit_angle(xing_angle);
                 if k2.kn=0 then EXIT;

               end;

            1: begin   // similar curvature -- Ks are both inner or both outer.

                       // K-crossings ...

                 if notch_on_intersection(False,0-1,0-1)<1 then EXIT; // both inner rails

                 k1.x:=x_for_notch;
                 k1.y:=y_for_notch;
                 k1.kk:=xing_angle;     // radians
                 k1.kn:=convert_to_unit_angle(xing_angle);
                 if k1.kn=0 then EXIT;

                 if notch_on_intersection(False,1,1)<1 then EXIT;     // both outer rails

                 k2.x:=x_for_notch;
                 k2.y:=y_for_notch;
                 k2.kk:=xing_angle;     // radians
                 k2.kn:=convert_to_unit_angle(xing_angle);
                 if k2.kn=0 then EXIT;

                       // V-crossings ...

                 if notch_on_intersection(False,1,0-1)<1 then EXIT;   // control outer, bgnd inner

                 v1.x:=x_for_notch;
                 v1.y:=y_for_notch;
                 v1.kk:=xing_angle;     // radians
                 v1.kn:=convert_to_unit_angle(xing_angle);
                 if v1.kn=0 then EXIT;

                 if notch_on_intersection(False,0-1,1)<1 then EXIT;   // control inner, bgnd outer

                 v2.x:=x_for_notch;
                 v2.y:=y_for_notch;
                 v2.kk:=xing_angle;     // radians
                 v2.kn:=convert_to_unit_angle(xing_angle);
                 if v2.kn=0 then EXIT;

               end;

        end;//case

       fail_code:=0;

       if (k1.kn<1.5) or (v1.kn<1.5) or (k2.kn<1.5) or (v2.kn<1.5)
          then begin
                 fail_code:=1;
                 EXIT;
               end;

       insert_half_diamond;

       xing_type_i:=1;          // default to curviform crossings
       turnout_road_i:=3;       // set minimum exit length to minimize mis-match

       hd_vcheck_rails:=0;     // normal check rails
       hd_timbers:=0;          // normal timber lengths

       if retain_shoves_on_make=False then clear_shovedata;  // clear any shoved timbers.
       if retain_diffs_on_make=False then clear_check_diffs; // 0.94.a clear any check rail diffs

       proximity_limit:=g/500;  // init
       found_it:=False;         // init

                        // find first half-diamond ...

       hdkn:=k1.kn;
       k3n:=v1.kn;

       gocalc(0,0);

       turnoutx:=tvjpx+5*scale;   // arbitrary template length  -- 5ft beyond vee joint
       turnout_i:=1;              // length locked at turnoutx.

       gocalc(0,0);

       pad_form.reset_peg_menu_entry.Click;   // to CTRL-0
       shift_onto_notch(False,False);         // notch is already on centre

       repeat
         if test_proximity(v1)=True then BREAK;

         invert_handing;                         // try opposite hand
         if test_proximity(v1)=True then BREAK;

         invert_curving;                         // try opposite curving
         if test_proximity(v1)=True then BREAK;

         invert_handing;                         // try opposite hand again
         if test_proximity(v1)=True then BREAK;

         rotate_turnout(Pi,True);                // rotate 180 degrees and try all again...
         if test_proximity(v1)=True then BREAK;

         invert_handing;                         // try opposite hand
         if test_proximity(v1)=True then BREAK;

         invert_curving;                         // try opposite curving
         if test_proximity(v1)=True then BREAK;

         invert_handing;                         // try opposite hand again
         if test_proximity(v1)=True then BREAK;

         proximity_limit:=proximity_limit+g/500;  // not found, increase limit and repeat all

       until proximity_limit>g/25;  // up to 20 repeats

       if found_it=True
          then begin
                          // 212a convert to sensible regular diamond...

                 if ABS(k1.kk-v1.kk)<0.0002  // difference less than 1/5000th radian arbitrary
                    then begin
                           hdkn:=k3n;
                           xing_type_i:=0;     // change to regular crossing
                           turnout_road_i:=0;  // normal
                           gocalc(0,0);
                         end;

                 store_and_background(False,False)       // keep it and copy to background.
               end
          else EXIT;                                     // failed to find 1st half-diamond

               // and now the 2nd half-diamond ...

       hdkn:=k2.kn;
       k3n:=v2.kn;

          // 212a convert to sensible regular diamond...

       if ABS(k2.kk-v2.kk)<0.0002  // difference less than 1/5000th radian arbitrary
          then begin
                 hdkn:=k3n;
                 xing_type_i:=0;     // change to regular crossing
                 turnout_road_i:=0;  // normal
               end;

       gocalc(0,0);

       turnoutx:=tvjpx+5*scale;   // arbitrary template length  -- 5ft beyond vee joint
       turnout_i:=1;              // length locked at turnoutx.

       gocalc(0,0);

       rotate_turnout(Pi,True);                // first rotate 180 degrees
       invert_curving;                         // and invert the curving

          // leave the control template showing.

        redraw(False);

        RESULT:=True;
      end;
      //////////////////////////////////////////////////////////////////////////

      procedure restore_current;

      begin
        copy_keep(saved_current);                    // retrieve saved current.
        current_name_str:=saved_name_str;
        current_memo_str:=saved_memo_str;

        info_form.ref_name_label.Caption:=current_name_str;
      end;
      //////////////////////////////////////////////////////////////////////////


begin
  RESULT:=False; // init fail result

  saved_current.keep_shove_list:=TStringList.Create;   // local stringlist not initialised.
  fill_kd(saved_current);                              // save control template
  saved_name_str:=current_name_str;
  saved_memo_str:=current_memo_str;

  try
    if do_notch_on_intersection(True,True,0,0,'Make a diamond-crossing at the intersection of the control template with the selected background template.||','This function uses the pegging notch.||')=False  // get notch on required intersection
       then EXIT;

    way:=1;                        // first try similar curvature
    if diamond_calcs=True
       then begin
              RESULT:=True;
              EXIT;                // good return
            end;

    restore_current;    // diamond_calcs has modified it.

    way:=0-1;                      // try opposing curvature instead
    if diamond_calcs=True
       then begin
              RESULT:=True;
              EXIT;                // good return
            end;

  restore_current;   // failed to find diamond-crossing, so restore the original control template
  redraw(True);

  finally
    free_shove_list(saved_current.keep_shove_list);   // free the local stringlist.
  end;//try
end;
//______________________________________________________________________________

procedure do_make_diamond_crossing_at_intersection;

             // 0.93.a
const
  diamond_intersection_help_str:string='php/110    `0Diamond-Crossing at Intersection`9';

begin
  if plain_track=False
     then begin
            if alert(6,'php/110    make  diamond - crossing  at  intersection',
                      'Sorry, this function is not available because the control template is not plain track.'
                     +'||Try again after using the `0TEMPLATE > CONVERT TO PLAIN TRACK`1 menu function, or split off the approach or exit track using the `0TOOLS > MAKE SPLIT >`1 menu options accordingly.',
                      '','','','more  information','cancel','',4)=4
               then help(0,diamond_intersection_help_str,'');
            EXIT;
          end;

  if slewing=True
     then begin
            if alert(6,'php/110    make  diamond - crossing  at  intersection',
                      'Sorry, this function is not available because the control template contains a slew.'
                     +'||If the intersection is not within the slewing zone, try again after using the `0TOOLS > MAKE SPLIT >`1 menu options accordingly.'
                     +'||If the intersection is within the slewing zone, you may be able to perform this operation manually by moving the fixing peg along the track (`0CTRL+F8`2 mouse action).',
                      '','','','more  information','cancel','',4)=4
               then help(0,diamond_intersection_help_str,'');
            EXIT;
          end;

  if spiral=True
     then begin
            if alert(6,'php/110    make  diamond - crossing  at  intersection',
                      'Sorry, this function is not available because the control template contains a transition curve.'
                     +'||If the intersection is not within the transition zone, try again after using the `0TOOLS > MAKE SPLIT >`1 menu options accordingly.'
                     +'||If the intersection is within the transition zone, you may be able to perform this operation manually by moving the fixing peg along the track (`0CTRL+F8`2 mouse action).',
                      '','','','more  information','cancel','',4)=4
               then help(0,diamond_intersection_help_str,'');
            EXIT;
          end;


  if make_diamond_crossing_at_intersection=True then EXIT;


  case fail_code of

         1: begin
              if alert(6,'php/110    make  diamond - crossing  at  intersection',
                       'One or more of the crossing angles would be shorter than 1:1.5'
                      +'||Sorry, crossings shorter than 1:1.5 are not supported in Templot0.'
                      +'||On the prototype such short crossings are not be made from standard track components. They are specially designed and constructed, and designs vary.'
                      +'||For a model, you can create the basis of a template by overlaying plain tracks. Construction details from your chosen prototype can then be marked on the printed template.| ',
                       '','','','more  information','cancel','',4)=4
               then help(0,diamond_intersection_help_str,'');
            end;
  end;//case

end;
//______________________________________________________________________________

procedure extend_to_boundary(index,boundary:integer; grow:boolean);  // 213b

  // this function requires an index (bgnd template).

  // boundary = 0, 6, 9, for CTRL-0, 6, 9, on bgnd template
  //          = 240 for TMINP
  //          = 241 for TEXITP

  //          = 260 for MMINP    217a
  //          = 261 for MEXITP   217a

  //          = 600 for TOLP

  //          =-1 for nearest

  // control template CTRL-0 , CTRL-9  only, to extend it

  // grow ignored if boundary not -1.  True=extend template  False=shorten template

const
  try_manual_str:string=#13+#13+'Try adjusting the control template manually.';

var
  end0,end9,new_end:Tnotch;

  bgnd_x,bgnd_y:extended;

  nearest_diag:extended;
  //long_diag:extended;

  i,which_end,which_boundary:integer;

  mod_dir:integer;      // 1=extend, -1=shorten
  arc_length:extended;  // change in template length needed
  arc_rad:extended;

  x_diff,y_diff,k_diff:extended;

  was_end_swapped:boolean;

  old_xorg,old_turnoutx:extended;

  bg_pt,bg_hd,bg_rp:boolean;

  diag0,diag9:extended;

  temp:extended;

  been_here_before:integer;

  grow_str:string;


label
  999;

                            ////////////////////////////////////////////////////

                            procedure show_not_needed;

                            begin
                              ShowMessage(grow_str+'No adjustment to the control template appears to be needed to meet the background template.'
                                          +#13+#13+'If this is not the case, try adjusting the control template manually.');

                            end;
                            ////////////////////////////////////////////////////

                            function get_nearest_diag:integer;   // update nearest and return which end of control

                            var
                              diag:extended;

                            begin
                              RESULT:=0-1; // init no change to nearest

                              temp:=SQR(bgnd_x-end0.notch_x)+SQR(bgnd_y-end0.notch_y);  // SQRT protection
                              if temp>minfp then diag:=SQRT(temp)
                                            else EXIT;                                  // ???

                              if nearest_diag>diag
                                 then begin
                                        nearest_diag:=diag;
                                        RESULT:=0;            // at CTRL-0
                                      end;

                              temp:=SQR(bgnd_x-end9.notch_x)+SQR(bgnd_y-end9.notch_y);  // SQRT protection
                              if temp>minfp then diag:=SQRT(temp)
                                            else EXIT;                                  // ???

                              if nearest_diag>diag
                                 then begin
                                        nearest_diag:=diag;
                                        RESULT:=9;            // at CTRL-9
                                      end;
                            end;
                            ////////////////////////////////////////////////////

begin

  grow_str:='';  // init

  if boundary=-1
     then begin
            if grow=True
               then grow_str:='Extending the control template to meet --'+#13+#13
               else grow_str:='Shortening the control template to meet --'+#13+#13;
          end;

  bg_pt:=Ttemplate(keeps_list.Objects[index]).bgnd_plain_track;
  bg_hd:=Ttemplate(keeps_list.Objects[index]).bgnd_half_diamond;
  bg_rp:=Ttemplate(keeps_list.Objects[index]).bgnd_retpar;        // parallel crossing

  was_end_swapped:=False;  // init
  been_here_before:=0;     // init

  arc_length:=0;           // keep compiler happy...
  //long_diag:=0;

  end0.notch_x:=0;
  end0.notch_y:=0;
  end0.notch_k:=0;

  diag0:=0;
  diag9:=0;

  old_xorg:=xorg;          // init for error exit ..
  old_turnoutx:=turnoutx;  // init

  mod_dir:=1;              // init   1=extend  -1=shorten


  with Ttemplate(keeps_list.Objects[index]).boundary_info do begin

    try

      999:

      repeat                    // come back and try again after changing the control template

        if been_here_before>20    // protect against endless loop   20 arbitrary
           then begin             // undo and exit
                  arc_length:=0;  // for finally

                  turnoutx:=old_turnoutx;
                  xorg:=old_xorg;

                  if boundary=-1
                     then ShowMessage(grow_str+'Sorry, unable to find a matching MS (main-side) boundary on the selected background template.'+try_manual_str)
                     else ShowMessage('Sorry, unable to find the required boundary on the selected background template.'+try_manual_str);
                  EXIT;
                end;
        INC(been_here_before);

        nearest_diag:=max_rad;  // init   abs between control template and bgnd template
        which_end:=-1;          // init   control template  0 or 9
        which_boundary:=-1;     // init   bgnd template 0 or 9 or 241 or 261 or 600
        arc_rad:=0;             // init   radius at boundary
        arc_length:=0;          // init   abs value of change needed

          // control template end positions...

        if half_diamond=False then end0:=calc_snap_peg_data(0);   // ctrl-0

        end9:=calc_snap_peg_data(11);  // ctrl-9

          // boundary positions, angles, and diagonal across boundaries
          // recorded at copy_keep_to_background() in keep_select unit

        case boundary of

            -1: begin   // nearest MS

                  if grow=True             // grow ignored if boundary not -1 (nearest)
                     then mod_dir:=1       // extend
                     else mod_dir:=-1;     // shorten

                       // find which MS boundary loc is nearest on bgnd template ...

                       // CTRL-0 ...

                  if bg_hd=False    // not CTRL-0 for half-diamond
                     then begin

                            bgnd_x:=loc_0.notch_x;   // using a Tnotch type, no relevance to actual notch
                            bgnd_y:=loc_0.notch_y;

                            i:=get_nearest_diag;
                            if i<>-1
                               then begin
                                      which_end:=i;          // which end of the control template is closest to a bgnd boundary.  =0 or =9
                                      which_boundary:=0;     // which boundary on bgnd template. =0 or =6 or =9
                                    end;
                          end;


                       // CTRL-9 ...

                  bgnd_x:=loc_9.notch_x;
                  bgnd_y:=loc_9.notch_y;

                  i:=get_nearest_diag;
                  if i<>-1
                     then begin
                            which_end:=i;        // which end of the control template is closest to a bgnd boundary.   =0 or =9
                            which_boundary:=9;   // which boundary on bgnd template. =0 or =9
                          end;

                  if (which_end=-1) or (which_boundary=-1) or (nearest_diag>(g*250))  // 250 arbitrary limit
                     then begin
                            ShowMessage(grow_str+'Sorry, unable to find a near boundary on the selected background template.'+try_manual_str);
                            EXIT;
                          end;

                  if which_end=0
                     then begin
                            swap_end_for_end;   // we can modify at nearer CTRL-9 end only. far CTRL-0 datum end stays fixed.
                            gocalc(0,0);

                            was_end_swapped:= NOT was_end_swapped;  // might be coming back here

                            CONTINUE;           // start again
                          end;

                  diag9:=nearest_diag;   // which_end=9

                  if which_boundary=0
                     then begin
                            temp:=SQR(loc_0.notch_x-end0.notch_x)+SQR(loc_0.notch_x-end0.notch_y);  // SQRT protection
                            if temp>minfp then diag0:=SQRT(temp)
                                          else begin
                                                 show_not_needed;
                                                 EXIT;
                                               end;
                          end
                     else begin   // which_boundary=9
                            temp:=SQR(loc_9.notch_x-end0.notch_x)+SQR(loc_9.notch_x-end0.notch_y);  // SQRT protection
                            if temp>minfp then diag0:=SQRT(temp)
                                          else begin
                                                 show_not_needed;
                                                 EXIT;
                                               end;
                          end;

                end;// nearest MS

             0: if bg_hd=False    // not CTRL-0 for half-diamond    menu should be disabled!
                   then begin
                          temp:=SQR(loc_0.notch_x-end0.notch_x)+SQR(loc_0.notch_y-end0.notch_y);  // SQRT protection
                          if temp>minfp then diag0:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          temp:=SQR(loc_0.notch_x-end9.notch_x)+SQR(loc_0.notch_y-end9.notch_y);  // SQRT protection
                          if temp>minfp then diag9:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          which_boundary:=0;
                        end
                   else EXIT;

             6: if bg_pt=False    // not CTRL-6 for plain track    menu should be disabled!
                   then begin
                          temp:=SQR(loc_6.notch_x-end0.notch_x)+SQR(loc_6.notch_y-end0.notch_y);  // SQRT protection
                          if temp>minfp then diag0:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          temp:=SQR(loc_6.notch_x-end9.notch_x)+SQR(loc_6.notch_y-end9.notch_y);  // SQRT protection
                          if temp>minfp then diag9:=SQRT(temp)
                                        else  begin
                                                show_not_needed;
                                                EXIT;
                                              end;

                          which_boundary:=6;

                        end
                   else EXIT;

             9: begin
                  temp:=SQR(loc_9.notch_x-end0.notch_x)+SQR(loc_9.notch_y-end0.notch_y);  // SQRT protection
                  if temp>minfp then diag0:=SQRT(temp)
                                else begin
                                       show_not_needed;
                                       EXIT;
                                     end;

                  temp:=SQR(loc_9.notch_x-end9.notch_x)+SQR(loc_9.notch_y-end9.notch_y);  // SQRT protection
                  if temp>minfp then diag9:=SQRT(temp)
                                else begin
                                       show_not_needed;
                                       EXIT;
                                     end;

                  which_boundary:=9;
                end;

           240: if bg_pt=False    // not TMINP for plain track    menu should be disabled!
                   then begin
                          temp:=SQR(loc_240.notch_x-end0.notch_x)+SQR(loc_240.notch_y-end0.notch_y);  // SQRT protection
                          if temp>minfp then diag0:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          temp:=SQR(loc_240.notch_x-end9.notch_x)+SQR(loc_240.notch_y-end9.notch_y);  // SQRT protection
                          if temp>minfp then diag9:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          which_boundary:=240;

                        end
                   else EXIT;

           241: if bg_pt=False    // not TEXITP for plain track    menu should be disabled!
                   then begin
                          temp:=SQR(loc_241.notch_x-end0.notch_x)+SQR(loc_241.notch_y-end0.notch_y);  // SQRT protection
                          if temp>minfp then diag0:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          temp:=SQR(loc_241.notch_x-end9.notch_x)+SQR(loc_241.notch_y-end9.notch_y);  // SQRT protection
                          if temp>minfp then diag9:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          which_boundary:=241;

                        end
                   else EXIT;

           260: if bg_pt=False    // not MMINP for plain track    menu should be disabled!       217a
                   then begin
                          temp:=SQR(loc_260.notch_x-end0.notch_x)+SQR(loc_260.notch_y-end0.notch_y);  // SQRT protection
                          if temp>minfp then diag0:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          temp:=SQR(loc_260.notch_x-end9.notch_x)+SQR(loc_260.notch_y-end9.notch_y);  // SQRT protection
                          if temp>minfp then diag9:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          which_boundary:=260;

                        end
                   else EXIT;

           261: if bg_pt=False    // not MEXITP for plain track    menu should be disabled!         217a
                   then begin
                          temp:=SQR(loc_261.notch_x-end0.notch_x)+SQR(loc_261.notch_y-end0.notch_y);  // SQRT protection
                          if temp>minfp then diag0:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          temp:=SQR(loc_261.notch_x-end9.notch_x)+SQR(loc_261.notch_y-end9.notch_y);  // SQRT protection
                          if temp>minfp then diag9:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          which_boundary:=261;

                        end
                   else EXIT;



           600: if (bg_pt=False) and (bg_rp=True)    // TOLP on parallel crossing
                   then begin
                          temp:=SQR(loc_600.notch_x-end0.notch_x)+SQR(loc_600.notch_y-end0.notch_y);  // SQRT protection
                          if temp>minfp then diag0:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          temp:=SQR(loc_600.notch_x-end9.notch_x)+SQR(loc_600.notch_y-end9.notch_y);  // SQRT protection
                          if temp>minfp then diag9:=SQRT(temp)
                                        else begin
                                               show_not_needed;
                                               EXIT;
                                             end;

                          which_boundary:=600;

                        end
                   else EXIT;

        end;//case

        if diag9>diag0
           then begin
                  swap_end_for_end;   // we can modify at nearer CTRL-9 end only. far CTRL-0 datum end stays fixed.
                  gocalc(0,0);

                  was_end_swapped:= NOT was_end_swapped;  // might be coming back here

                  CONTINUE;           // start again
                end;


        startx:=0;     // cancel any blanking

        if spiral=True
           then begin
                  if turnoutx<os
                     then begin
                            arc_rad:=ABS(nomrad1);
                          end
                     else if turnoutx>(os+tst)
                             then begin
                                    arc_rad:=ABS(nomrad2);
                                  end
                             else begin   // move boundary out of transition zone

                                    turnoutx:=os+tst+g/10;    // g/10 arbitrary

                                    if plain_track=True then xorg:=turnoutx;

                                    gocalc(0,0);

                                         // now repeat from the top...

                                    CONTINUE;
                                  end;
                end
           else begin
                  arc_rad:=ABS(nomrad);
                end;

      until arc_rad<>0;


      if ABS((diag9/2)/arc_rad)>0.98    // more than 90 degs?  ARCSIN protection
         then begin
                ShowMessage(grow_str+'Sorry, the control template is too far from the background template.'
                   +#13+#13+'Please extend the control template and then try again.');
                EXIT;
              end;

      arc_length:=mod_dir*2*ARCSIN((diag9/2)/arc_rad)*arc_rad;

      if ABS(arc_length)<0.001    // 0.001mm arbitrary
         then begin
                show_not_needed;
                EXIT;
              end;

         // now ready to modify the control template - extend or shorten the template to fit...

      turnoutx:=turnoutx+arc_length;

      if turnoutx>turnoutx_max
         then begin

                if (boundary<>-1) and (mod_dir=1)    // try shortening instead
                   then begin
                          turnoutx:=turnoutx-arc_length;  // undo change
                          mod_dir:=-1;
                          goto 999;
                        end;

                                 // undo and exit
                arc_length:=0;   // for finally

                turnoutx:=old_turnoutx;
                xorg:=old_xorg;

                ShowMessage(grow_str+'Sorry, the control template would exceed the maximum template length.'+try_manual_str);
                EXIT;
              end;


      if plain_track=True
         then begin
                if turnoutx<0
                   then begin
                                                    // undo and exit
                          arc_length:=0;            // for finally

                          turnoutx:=old_turnoutx;
                          xorg:=old_xorg;

                          ShowMessage(grow_str+'Sorry, the length of the control template would be too short.'+try_manual_str);
                          EXIT;
                        end;
                xorg:=turnoutx;     // plain track
              end
         else begin                 // turnout
                if turnoutx<xorg
                   then begin
                                              // undo and exit
                          arc_length:=0;      // for finally

                          turnoutx:=old_turnoutx;
                          xorg:=old_xorg;

                          ShowMessage(grow_str+'Sorry, the length of the control template would be too short.'
                             +#13+#13+'Try adjusting the control template manually or converting to plain track.');
                          EXIT;
                        end;
              end;

  gocalc(0,0);    // calc new template

  new_end:=calc_snap_peg_data(11);  // 11 = CTRL-9 on control template

  case which_boundary of

       0: begin
            x_diff:=ABS(loc_0.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_0.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_0.notch_k-new_end.notch_k);
          end;

       6: begin
            x_diff:=ABS(loc_6.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_6.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_6.notch_k-new_end.notch_k);
          end;

       9: begin
            x_diff:=ABS(loc_9.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_9.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_9.notch_k-new_end.notch_k);
          end;

     240: begin
            x_diff:=ABS(loc_240.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_240.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_240.notch_k-new_end.notch_k);
          end;

     241: begin
            x_diff:=ABS(loc_241.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_241.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_241.notch_k-new_end.notch_k);
          end;

     260: begin                                             // 217a
            x_diff:=ABS(loc_260.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_260.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_260.notch_k-new_end.notch_k);
          end;

     261: begin                                             // 217a
            x_diff:=ABS(loc_261.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_261.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_261.notch_k-new_end.notch_k);
          end;

     600: begin
            x_diff:=ABS(loc_600.notch_x-new_end.notch_x);
            y_diff:=ABS(loc_600.notch_y-new_end.notch_y);
            k_diff:=ABS(loc_600.notch_k-new_end.notch_k);
          end;

     else begin                     // ???
            arc_length:=0;          // for finally
            turnoutx:=old_turnoutx;
            xorg:=old_xorg;

            ShowMessage(grow_str+'Sorry, an error occurred.'
               +#13+#13+'Please try adjusting the control template manually instead.');
            EXIT;
          end;

  end;//case

  while k_diff>(Pi/2) do k_diff:=k_diff-Pi;    // more than 90, subtract 180 degrees.

  if (ABS(x_diff)>(scale/20))    // arbitrary from experiments 1/20ft = 0.6in  = 0.2mm at 4mm/ft.
  or (ABS(y_diff)>(scale/20))
  or (ABS(k_diff)>0.01)          // radians  arbitrary from experiments = 0.57 degrees
     then begin

             if (boundary<>-1) and (mod_dir=1)    // try shortening instead
                then begin
                       turnoutx:=turnoutx-arc_length;  // undo change
                       mod_dir:=-1;
                       goto 999;
                     end;

            arc_length:=0;          // for finally
            turnoutx:=old_turnoutx;
            xorg:=old_xorg;

            if boundary=-1
               then ShowMessage(grow_str+'Sorry, unable to find a matching MS (main-side) boundary on the selected background template.'+try_manual_str)
               else ShowMessage('The control template does not align with the background template at that boundary.'+try_manual_str);
            EXIT;
          end;

  if (ABS(x_diff)>0.01)    // mm  arbitrary from experiments
  or (ABS(y_diff)>0.01)
  or (ABS(k_diff)>0.001)   // radians  arbitrary from experiments
     then begin

     if alert(4,'php/203    extend / shorten  the  control  template  to  meet',
               'Small error found:'
               +'||If extended the control template will not align perfectly with the background template, but may be acceptable for use.'
               +'||The discrepancies will be:|'
               +'|on X: '+round_str(ABS(x_diff),3)+' mm'
               +'|on Y: '+round_str(ABS(y_diff),3)+' mm'
               +'|on angle: '+round_str(ABS(k_diff*180/Pi),3)+' degrees'
               +'||It is likely that there is a small error in the alignments which could be corrected.'
               +'||Do you want to continue uncorrected?',
                '','','','continue  anyway','cancel','',0)=5
        then begin
               arc_length:=0;          // for finally
               turnoutx:=old_turnoutx;
               xorg:=old_xorg;
               EXIT;
             end;
          end;

    finally
      if was_end_swapped=True
         then begin
                swap_end_for_end;            // swap it back for him

                if (plain_track=False) and (half_diamond=False) and (arc_length<>0)
                   then begin

                          xorg:=xorg+arc_length;    // extension was on turnout approach track
                          if xorg<0 then xorg:=0;
                          if xorg>turnoutx then xorg:=turnoutx;

                        end;

                gocalc(0,0);
              end;

    end;//finally

  end;//with Ttemplate
end;
//______________________________________________________________________________

function grow_prune_to_meet(boundary1,boundary2:Tnotch; index1,index2:integer; grow,from_zero:boolean):boolean;   // 213b  extend/shorten control template to meet a matching bgnd boundary

  // boundary1, boundary2 = targets (try one or both if first not met), if invalid .notch_x=def_req  (0-1.0E300).

  // if no boundary, index1, index2 = bgnd templates (try one or both if first not met), if invalid = -1 (search all bgnd templates).

  // grow  True = extend template to meet boundary, False = shorten template to meet boundary.

  // from_zero  True = grow from xorg=0 (CTRL-0 for plain track, CTRL-1 for turnouts), False = grow from existing templaste length.  ignored if pruning.

  // grow_prune_to_meet(boundary1,boundary2,-1,-1,True,True);

var
  n,step_count,calc_count:integer;
  step:extended;
  approach_length:extended;

  nb:integer;
  start_peg,end_peg:Tnotch;
  bgnd_pegs:array[0..4] of Tnotch;
  x1,y1,x2,y2,k1,k2,proximity:extended;
  readout_str:string;

  k_diff,nearest_bgnd:extended;

  temp1,temp2:extended;

label
  100;



                               /////////////////////////////////////////////////

                               procedure draw_it;

                               begin
                                 do_rollback:=False;
                                 gocalc(2,0);
                               end;
                               /////////////////////////////////////////////////

begin
  RESULT:=False;  // init

  if any_bgnd<1
     then begin
            alert_no_bgnd;
            EXIT;            // no background templates
          end;

  RESULT:=True;   // temp

  wait_cancel_clicked:=False;
  wait_form.cancel_button.Visible:=True;

  wait_form.waiting_label.Caption:='growing  template ...';

  wait_form.waiting_label.Width:=wait_form.Canvas.TextWidth(wait_form.waiting_label.Caption);  // 205b bug fix for Wine

  wait_form.wait_progressbar.Max:=100;
  wait_form.wait_progressbar.Min:=0;
  wait_form.wait_progressbar.Position:=0;
  wait_form.wait_progressbar.Step:=1;
  wait_form.wait_progressbar.Visible:=True;

  wait_form.Show;


  approach_length:=xorg;

    // first find distance from CTRL-0 to nearest bgnd boundary (which might be the target)..

  nearest_bgnd:=max_rad;             // init

  k1:=0;   // keep compiler happy
  k2:=0;

  start_peg:=calc_snap_peg_data(0);  // CTRL-0

  x1:=start_peg.notch_x;
  y1:=start_peg.notch_y;

  for n:=0 to (keeps_list.Count-1) do begin

    with Ttemplate(keeps_list.Objects[n]) do begin

      if bg_copied=False then CONTINUE;  // not on background.

      bgnd_pegs[0]:=snap_peg_positions.ctrl_0_pos;
      bgnd_pegs[1]:=snap_peg_positions.ctrl_1_pos;
      bgnd_pegs[2]:=snap_peg_positions.ctrl_6_pos;
      bgnd_pegs[3]:=snap_peg_positions.ctrl_9_pos;
      bgnd_pegs[4]:=snap_peg_positions.ctrl_tolp_pos;

      for nb:=0 to 4 do begin

          // check valid position on background template...

        if (bgnd_half_diamond=True) and (nb<2) then CONTINUE;    // only Ctrl-6, Ctrl-9, TOLP valid for a half-diamond.
        if (bgnd_plain_track=True) and (nb>1) then CONTINUE;     // only Ctrl-0 and Ctrl-1 valid for plain track.

        if (bgnd_half_diamond=False) and (bgnd_plain_track=False) and (nb=1) then CONTINUE;  // Ctrl-1 not valid for a turnout.

        if (bgnd_retpar=False) and (nb=4) then CONTINUE;  // TOLP valid only for parallel crossing.

        if (bgnd_blanked=True) and (nb<2) then CONTINUE;  // 215a   Ctrl-0 and Ctrl-1 not valid if any blanking (startx>0)

        if (bgnd_no_xing=True) and (nb>1) then CONTINUE;  // 215a   Ctrl-6, Ctrl-9, TOLP not valid if template shortened to exclude the crossing (turnoutx<fpx)

          // calc distance...

        x2:=bgnd_pegs[nb].notch_x;
        y2:=bgnd_pegs[nb].notch_y;

        temp1:=SQR(x1-x2)+SQR(y1-y2);

        if temp1>0.0001    // SQRT protection     .01mm^2 arbitrary
           then begin
                  temp2:=SQRT(temp1);
                  if nearest_bgnd>temp2 then nearest_bgnd:=temp2;
                end
           else CONTINUE; // ignore this boundary, it is probably the adjoining template


      end;//next nb
    end;//with Ttemplate
  end;//next n

  proximity:=nearest_bgnd*4/5;    // 1st rough
  step:=proximity/3;
  step_count:=0;
  calc_count:=0;

  try

      // start close to zero (plain track), or at CTRL-1 (turnout)

    if plain_track=True
       then begin
              turnoutx:=g/20;
              xorg:=g/20;
            end
       else begin
              if xorg=0
                 then turnoutx:=g/20
                 else turnoutx:=xorg;
            end;

    draw_it;

    100:

    repeat

      if wait_cancel_clicked=True then EXIT;

      wait_form.wait_progressbar.StepIt;
      if wait_form.wait_progressbar.Position=wait_form.wait_progressbar.Max
         then wait_form.wait_progressbar.Position:=wait_form.wait_progressbar.Min;    // wrap if necessary

      Application.ProcessMessages;  // to show the wait form updating

      INC(calc_count);

      turnoutx:=turnoutx+step;

      if plain_track=True
         then xorg:=turnoutx
         else xorg:=approach_length;

      draw_it;

          // control template end peg position...

      if plain_track=True
         then end_peg:=calc_snap_peg_data(1)    // CTRL-1
         else end_peg:=calc_snap_peg_data(11);  // CTRL-9

          // background templates peg positions...

      for n:=0 to (keeps_list.Count-1) do begin

        with Ttemplate(keeps_list.Objects[n]) do begin

          if bg_copied=False then CONTINUE;  // not on background.

          bgnd_pegs[0]:=snap_peg_positions.ctrl_0_pos;
          bgnd_pegs[1]:=snap_peg_positions.ctrl_1_pos;
          bgnd_pegs[2]:=snap_peg_positions.ctrl_6_pos;
          bgnd_pegs[3]:=snap_peg_positions.ctrl_9_pos;
          bgnd_pegs[4]:=snap_peg_positions.ctrl_tolp_pos;

          for nb:=0 to 4 do begin

              // check valid position on background template...

            if (bgnd_half_diamond=True) and (nb<2) then CONTINUE;    // only Ctrl-6, Ctrl-9, TOLP valid for a half-diamond.
            if (bgnd_plain_track=True) and (nb>1) then CONTINUE;     // only Ctrl-0 and Ctrl-1 valid for plain track.

            if (bgnd_half_diamond=False) and (bgnd_plain_track=False) and (nb=1) then CONTINUE;  // Ctrl-1 not valid for a turnout.

            if (bgnd_retpar=False) and (nb=4) then CONTINUE;  // TOLP valid only for parallel crossing.

              // calc proximity...

            x1:=end_peg.notch_x;
            y1:=end_peg.notch_y;
            k1:=end_peg.notch_k;   // for final angle check

            x2:=bgnd_pegs[nb].notch_x;
            y2:=bgnd_pegs[nb].notch_y;
            k2:=bgnd_pegs[nb].notch_k;  // for final angle check

            temp1:=SQR(x1-x2)+SQR(y1-y2);

            if temp1>minfp
               then begin
                      if SQRT(temp1)<proximity
                         then begin
                                step_count:=5000;
                                BREAK;
                              end;
                    end
               else begin                    // hit it exactly by magic
                      step_count:=5000;
                      BREAK;
                    end;


          end;//nb next boundary on bgnd template

        end;//with Ttemplate

        if step_count>500 then BREAK;

      end;//n next bgnd template

      INC(step_count);

    until step_count>500;

    if proximity<0.01
       then begin
              k_diff:=ABS(k1-k2);

              while k_diff>(Pi/2) do k_diff:=k_diff-Pi;    // more than 90, subtract 180 degrees.

              if ABS(k_diff)>0.01          // radians  arbitrary from experiments = 0.57 degrees
                 then begin
                        ShowMessage('Sorry, the extended the control template does not align with the background template.'
                           +#13+#13+'Try adjusting the control template manually instead.');
                        EXIT;
                      end;

              if ABS(k_diff)>0.001   // radians  arbitrary from experiments
                 then begin

                        if alert(4,'php/203    grow  the  control  template  to  meet',
               'Small error found:'
               +'||The extended the control template does not align perfectly with the background template, but may be acceptable for use.'
               +'||The discrepancy is '+round_str(ABS(k_diff*180/Pi),3)+' degrees away from tangency.'
               +'||It is likely that there is a small error in the alignments which could be corrected.'
               +'||Do you want to continue uncorrected?',
                '','','','continue  anyway','cancel','',0)=5
                          then begin
                                 EXIT;
                               end;
                      end;

              EXIT;      // 0.01mm arbitrary
            end;

    if calc_count>500000 then EXIT;

    proximity:=proximity/3;    // finer steps
    step:=proximity/3;
    step_count:=0;

    if proximity<1  //  1mm arbitrary
       then begin
              wait_form.waiting_label.Caption:='precision  fitting,  please  wait ...';
              wait_form_onshow;
            end;

    goto 100;

  finally

    wait_form.Hide;
    wait_cancel_clicked:=False;
    wait_form.wait_progressbar.Visible:=False;
    wait_form.cancel_button.Visible:=False;

  end;//try
end;
//______________________________________________________________________________

end.
