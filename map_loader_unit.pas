
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

unit map_loader_unit;

{$MODE Delphi}

{$ALIGN OFF}

     // 215a  load map as tiles or screenshot directly into picture shapes

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, HTTPSend;

type

  { Tmap_loader_form }

  Tmap_loader_form = class(TForm)
    cancel_button: TButton;
    highlight_label: TLabel;
    map_load_groupbox: TGroupBox;
    manifest_memo: TMemo;
    osm_radio_button: TRadioButton;
    nls_london_radio_button: TRadioButton;
    datestamp_label: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    location_groupbox: TGroupBox;
    os_grid_radio_button: TRadioButton;
    lat_lon_radio_button: TRadioButton;
    os_easting_edit: TEdit;
    os_northing_edit: TEdit;
    os_letters_edit: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lon_edit: TEdit;
    lat_edit: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label16: TLabel;
    Label14: TLabel;
    load_button: TButton;
    info_button: TButton;
    Label19: TLabel;
    extend_area_groupbox: TGroupBox;
    add_top_button: TButton;
    add_left_button: TButton;
    add_right_button: TButton;
    add_bottom_button: TButton;
    Shape2: TShape;
    crop_area_groupbox: TGroupBox;
    Shape3: TShape;
    crop_top_button: TButton;
    crop_left_button: TButton;
    crop_right_button: TButton;
    crop_bottom_button: TButton;
    background_shapes_button: TButton;
    delete_button: TButton;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    nls_6inch_radio_button: TRadioButton;
    highlight_shape: TShape;
    show_loading_checkbox: TCheckBox;
    test_zoom_png_image: TImage;
    transparent_checkbox: TCheckBox;
    load_new_label: TLabel;
    name_edit: TEdit;
    name_listbox: TListBox;
    list_label: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    zoom_edit: TEdit;
    zoom_label: TLabel;
    Label32: TLabel;
    screenshot_25inch_radio_button: TRadioButton;
    info_label: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    url_radio_button: TRadioButton;
    url_edit: TEdit;
    Label10: TLabel;
    Label15: TLabel;
    screenshot_50inch_radio_button: TRadioButton;
    Label18: TLabel;
    Label31: TLabel;
    screenshot_25k_radio_button: TRadioButton;
    Label33: TLabel;
    Shape1: TShape;
    Label25: TLabel;
    stop_button: TButton;
    current_map_label: TLabel;
    close_panel: TPanel;
    close_button: TButton;
    pause_button: TButton;
    screenshot_url_edit: TEdit;
    other_screenshot_url_radio_button: TRadioButton;
    previous_loc_radio_button: TRadioButton;
    use_location_checkbox: TCheckBox;
    Label34: TLabel;
    t_55_label: TLabel;
    at_zero_checkbox: TCheckBox;
    procedure cancel_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label3MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure map_load_groupboxMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Label4MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Label6MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Label5MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Label6Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label14Click(Sender: TObject);
    procedure Label14MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure location_groupboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure lon_editEnter(Sender: TObject);
    procedure os_letters_editEnter(Sender: TObject);
    procedure load_buttonClick(Sender: TObject);
    procedure background_shapes_buttonClick(Sender: TObject);
    procedure Label24Click(Sender: TObject);
    procedure Label25Click(Sender: TObject);
    procedure Label24MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Label23MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure delete_buttonClick(Sender: TObject);
    procedure add_top_buttonClick(Sender: TObject);
    procedure name_editChange(Sender: TObject);
    procedure name_listboxClick(Sender: TObject);
    procedure add_bottom_buttonClick(Sender: TObject);
    procedure add_left_buttonClick(Sender: TObject);
    procedure add_right_buttonClick(Sender: TObject);
    procedure osm_radio_buttonClick(Sender: TObject);
    procedure nls_london_radio_buttonClick(Sender: TObject);
    procedure nls_6inch_radio_buttonClick(Sender: TObject);
    procedure screenshot_25inch_radio_buttonClick(Sender: TObject);
    procedure screenshot_50inch_radio_buttonClick(Sender: TObject);
    procedure screenshot_25k_radio_buttonClick(Sender: TObject);
    procedure url_editEnter(Sender: TObject);
    procedure stop_buttonClick(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure pause_buttonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure info_buttonClick(Sender: TObject);
    procedure screenshot_url_editEnter(Sender: TObject);
    procedure other_screenshot_url_radio_buttonClick(Sender: TObject);
    procedure crop_top_buttonClick(Sender: TObject);
    procedure crop_right_buttonClick(Sender: TObject);
    procedure crop_bottom_buttonClick(Sender: TObject);
    procedure crop_left_buttonClick(Sender: TObject);
    procedure use_location_checkboxClick(Sender: TObject);
    procedure previous_loc_radio_buttonClick(Sender: TObject);
    procedure screenshot_url_editClick(Sender: TObject);
    procedure url_editClick(Sender: TObject);
    procedure name_editClick(Sender: TObject);
    procedure lon_editClick(Sender: TObject);
    procedure lat_editClick(Sender: TObject);
    procedure os_letters_editClick(Sender: TObject);
    procedure os_easting_editClick(Sender: TObject);
    procedure os_northing_editClick(Sender: TObject);
    procedure zoom_editClick(Sender: TObject);
    procedure info_labelMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  map_loader_form: Tmap_loader_form;

implementation

{$R *.lfm}

uses
  Math,control_room, bgnd_unit, pad_unit, grid_unit, math_unit, entry_sheet, alert_unit, help_sheet, gauge_unit,
  web_map_help_unit;

type

  Tlat_lon=record
             lat:extended;  // latitude degrees
             lon:extended;  // longitude degrees
           end;

  Tos_grid=record
             easting:extended;
             northing:extended;
           end;

var
  xtile_min,ytile_min,xtile_max,ytile_max:integer;

  xtile_min_p1,ytile_min_p1,xtile_max_p1,ytile_max_p1:Tpex;
  xtile_min_p2,ytile_min_p2,xtile_max_p2,ytile_max_p2:Tpex;

  location_code:integer=0;     // 0=long/lat 1=OS Grid  2=from URL  3=previous screenshot

  map_tile_count_x:integer=0;
  map_tile_count_y:integer=0;

  map_tile_size_mm:extended=0;

  map_num_tiles_x:integer=0;
  map_num_tiles_y:integer=0;

  map_org_x:extended=0;
  map_org_y:extended=0;

  map_name_str:string='';

  tile_name_str:string='';    // 'map::'(1-5) + user name (padded with trailing spaces to 15 chars, 6-20) + map_code(21) + 'T'or'S'(tiled/screenshot)(22) + zoom_level(padded with trailing spaces to 2, 23-24) + separator'#'(25) + xtile_str(padded with trailing spaces to 10, 26-35) + separator'#'(36) + ytile_str(padded to 10, 37-46).
                              // total 46 characters. string index as above.

  getting_tile:boolean=False;  // global lock

  abort_map:boolean=False;

  loading_in_progress:boolean=False;
  stop_loading:boolean=False;
  pause_loading:boolean=False;

  base_width:extended=2000;   // arbitrary init ...
  base_height:extended=1000;

  cancel_clicked:boolean=False;

  got_osgrid:boolean=False;

  previous_lat_lon:Tlat_lon;

  map_code:integer=0;          // 0=OpenStreetMap(tiled)    1=NLS London 1890s(tiled)     2=NLS 6-inch(tiled)   3=NLS 25-inch(screenshot)     4=NLS 50-inch(screenshot)     5=NLS 25K(screenshot)     6=URL(screenshot)

  function get_tile_extents(map_str:string):boolean;forward;

  procedure map_loader_form_update;forward;

  function parse_url(str:string; var zoom_level:integer; var lat_lon:Tlat_lon):boolean;forward;

  function format_his_name:string;forward;

//______________________________________________________________________________

function parse_os_grid(modx,mody:extended; letter_str,easting_str,northing_str:string):Tos_grid;

  // e.g. TG 51409 13177  = easting:651409  northing:313177
  // e.g. ND 51 13        = easting:351000  northing:913000

  // modify result by modx,mody metres

var
  east,north:integer;
  east_chars,north_chars:integer;

                ////////////////////////////////////////////////////////////////

                procedure get_letters;

                begin
                  if letter_str='HP' then begin east:=4; north:=12; EXIT; end;
                  if letter_str='HT' then begin east:=3; north:=11; EXIT; end;
                  if letter_str='HU' then begin east:=4; north:=11; EXIT; end;
                  if letter_str='HW' then begin east:=1; north:=10; EXIT; end;
                  if letter_str='HX' then begin east:=2; north:=10; EXIT; end;
                  if letter_str='HY' then begin east:=3; north:=10; EXIT; end;
                  if letter_str='HZ' then begin east:=4; north:=10; EXIT; end;
                  if letter_str='NA' then begin east:=0; north:=9; EXIT; end;
                  if letter_str='NB' then begin east:=1; north:=9; EXIT; end;
                  if letter_str='NC' then begin east:=2; north:=9; EXIT; end;
                  if letter_str='ND' then begin east:=3; north:=9; EXIT; end;
                  if letter_str='NF' then begin east:=0; north:=8; EXIT; end;
                  if letter_str='NG' then begin east:=1; north:=8; EXIT; end;
                  if letter_str='NH' then begin east:=2; north:=8; EXIT; end;
                  if letter_str='NJ' then begin east:=3; north:=8; EXIT; end;
                  if letter_str='NK' then begin east:=4; north:=8; EXIT; end;
                  if letter_str='NL' then begin east:=0; north:=7; EXIT; end;
                  if letter_str='NM' then begin east:=1; north:=7; EXIT; end;
                  if letter_str='NN' then begin east:=2; north:=7; EXIT; end;
                  if letter_str='NO' then begin east:=3; north:=7; EXIT; end;
                  if letter_str='NQ' then begin east:=0; north:=6; EXIT; end;
                  if letter_str='NR' then begin east:=1; north:=6; EXIT; end;
                  if letter_str='NS' then begin east:=2; north:=6; EXIT; end;
                  if letter_str='NT' then begin east:=3; north:=6; EXIT; end;
                  if letter_str='NU' then begin east:=4; north:=6; EXIT; end;
                  if letter_str='NV' then begin east:=0; north:=5; EXIT; end;
                  if letter_str='NW' then begin east:=1; north:=5; EXIT; end;
                  if letter_str='NX' then begin east:=2; north:=5; EXIT; end;
                  if letter_str='NY' then begin east:=3; north:=5; EXIT; end;
                  if letter_str='NZ' then begin east:=4; north:=5; EXIT; end;
                  if letter_str='OV' then begin east:=5; north:=5; EXIT; end;
                  if letter_str='SA' then begin east:=0; north:=4; EXIT; end;
                  if letter_str='SB' then begin east:=1; north:=4; EXIT; end;
                  if letter_str='SC' then begin east:=2; north:=4; EXIT; end;
                  if letter_str='SD' then begin east:=3; north:=4; EXIT; end;
                  if letter_str='SE' then begin east:=4; north:=4; EXIT; end;
                  if letter_str='SH' then begin east:=2; north:=3; EXIT; end;
                  if letter_str='SJ' then begin east:=3; north:=3; EXIT; end;
                  if letter_str='SK' then begin east:=4; north:=3; EXIT; end;
                  if letter_str='SM' then begin east:=1; north:=2; EXIT; end;
                  if letter_str='SN' then begin east:=2; north:=2; EXIT; end;
                  if letter_str='SO' then begin east:=3; north:=2; EXIT; end;
                  if letter_str='SP' then begin east:=4; north:=2; EXIT; end;
                  if letter_str='SR' then begin east:=1; north:=1; EXIT; end;
                  if letter_str='SS' then begin east:=2; north:=1; EXIT; end;
                  if letter_str='ST' then begin east:=3; north:=1; EXIT; end;
                  if letter_str='SU' then begin east:=4; north:=1; EXIT; end;
                  if letter_str='SV' then begin east:=0; north:=0; EXIT; end;
                  if letter_str='SW' then begin east:=1; north:=0; EXIT; end;
                  if letter_str='SX' then begin east:=2; north:=0; EXIT; end;
                  if letter_str='SY' then begin east:=3; north:=0; EXIT; end;
                  if letter_str='SZ' then begin east:=4; north:=0; EXIT; end;
                  if letter_str='TA' then begin east:=5; north:=4; EXIT; end;
                  if letter_str='TF' then begin east:=5; north:=3; EXIT; end;
                  if letter_str='TG' then begin east:=6; north:=3; EXIT; end;
                  if letter_str='TL' then begin east:=5; north:=2; EXIT; end;
                  if letter_str='TM' then begin east:=6; north:=2; EXIT; end;
                  if letter_str='TQ' then begin east:=5; north:=1; EXIT; end;
                  if letter_str='TR' then begin east:=6; north:=1; EXIT; end;
                  if letter_str='TV' then begin east:=5; north:=0; EXIT; end;

                end;
                ////////////////////////////////////////////////////////////////

begin
  east:=0-1;  // init invalid ...

  RESULT.easting:=0-1;
  RESULT.northing:=0-1;

  letter_str:=Trim(letter_str);
  easting_str:=Trim(easting_str);
  northing_str:=Trim(northing_str);

  letter_str:=UpperCase(letter_str);

  get_letters;

  if east=-1
     then begin
            ShowMessage('Invalid OS Grid letters.'+#13+#13+'The letters must be 2 valid grid letters marking an area 100km square in the UK.'
                         +#13+#13+'Refer to your OS map for the correct letters for your area.');
            got_osgrid:=False;
            EXIT;
          end;

  east:=east*100000;    // metres to SW corner of letter squares
  north:=north*100000;

  east_chars:=Length(easting_str);
  north_chars:=Length(northing_str);

  if (east_chars<1) or (east_chars>5) or (north_chars<1) or (north_chars>5)
     then begin
            ShowMessage('Invalid OS Grid reference.'+#13+#13+'Grid numbers with letters must contain at least 1 and not more than 5 digits.'
            +#13+#13+'Alternatively you can enter 6 digits and leave the letters blank');
            got_osgrid:=False;
            EXIT;
          end;

  case east_chars of

    1: easting_str:=easting_str+'0000';
    2: easting_str:=easting_str+'000';
    3: easting_str:=easting_str+'00';
    4: easting_str:=easting_str+'0';

  end;//case

  case north_chars of

    1: northing_str:=northing_str+'0000';
    2: northing_str:=northing_str+'000';
    3: northing_str:=northing_str+'00';
    4: northing_str:=northing_str+'0';

  end;//case

  try
    east:=east+StrToInt(easting_str);
    north:=north+StrToInt(northing_str);
  except
    ShowMessage('Invalid OS Grid reference.'+#13+#13+'Grid numbers must contain valid digits 0-9 only.');
    got_osgrid:=False;
    EXIT;
  end;//try

  got_osgrid:=True;

    // to floats ...

  RESULT.easting:=east+modx;
  RESULT.northing:=north+mody;

end;
//______________________________________________________________________________

// Converts Ordnance Survey grid reference easting/northing coordinate to latitude/longitude
// (SW corner of grid square).

// (c) Chris Veness 2005-2016   MIT Licence

function os_grid_to_lat_lon(easting,northing:extended):Tlat_lon;

var
  E,NN,
  a,b,
  F0,
  k10,k20,
  N0,E0,
  e2,
  n,n2,n3,
  k1,M,
  Ma,
  Mb,
  Mc,
  Md,
  cosk1,sink1,
  v,
  k3,
  k42,

  tank1,
  tan2k1,tan4k1,tan6k1,
  seck1,
  v3,v5,v7,

  VII,VIII,IX,X,XI,XII,XIIA,

  dE,dE2,dE3,dE4,dE5,dE6,dE7,k2:extended;

begin
  E:=easting;
  NN:=northing;

  a:=6377563.396;        // Airy 1830 major & minor semi-axes
  b:=6356256.909;

  F0:=0.9996012717;      // NatGrid scale factor on central meridian

  k10:=49*Pi/180;        // radians // NatGrid true origin is 49°N,2°W
  k20:=0-2*Pi/180;

  N0:=-100000;           // northing & easting of true origin, metres
  E0:=400000;

  e2:=1-(b*b)/(a*a);     // eccentricity squared

  n:=(a-b)/(a+b);
  n2:=n*n;
  n3:=n*n*n;             // n, n squared, n cubed

  k1:=k10;
  M:=0;

  repeat
    k1:=(NN-N0-M)/(a*F0)+k1;

    Ma:=(1+n+(5/4)*n2+(5/4)*n3)*(k1-k10);
    Mb:=(3*n+3*n*n+(21/8)*n3)*SIN(k1-k10)*COS(k1+k10);
    Mc:=((15/8)*n2+(15/8)*n3)*SIN(2*(k1-k10))*COS(2*(k1+k10));
    Md:=(35/24)*n3*SIN(3*(k1-k10))*COS(3*(k1+k10));

    M:=b*F0*(Ma-Mb+Mc-Md);       // meridional arc

  until (NN-N0-M)<0.00001;       // i.e. until < 0.01mm

  cosk1:=COS(k1);
  sink1:=SIN(k1);

  v:=a*F0/SQRT(1-e2*sink1*sink1);                // transverse radius of curvature

  k3:=a*F0*(1-e2)/POWER(1-e2*sink1*sink1,1.5);   // meridional radius of curvature

  k42:=v/k3-1;

  tank1:=TAN(k1);
  tan2k1:=tank1*tank1;
  tan4k1:=tan2k1*tan2k1;
  tan6k1:=tan4k1*tan2k1;

  seck1:=1/cosk1;

  v3:=v*v*v;
  v5:=v3*v*v;
  v7:=v5*v*v;

  VII:=tank1/(2*k3*v);
  VIII:=tank1/(24*k3*v3)*(5+3*tan2k1+k42-9*tan2k1*k42);
  IX:=tank1/(720*k3*v5)*(61+90*tan2k1+45*tan4k1);
  X:=seck1/v;
  XI:=seck1/(6*v3)*(v/k3+2*tan2k1);
  XII:=seck1/(120*v5)*(5+28*tan2k1+24*tan4k1);
  XIIA:=seck1/(5040*v7)*(61+662*tan2k1+1320*tan4k1+720*tan6k1);

  dE:=(E-E0);
  dE2:=dE*dE;
  dE3:=dE2*dE;
  dE4:=dE2*dE2;
  dE5:=dE3*dE2;
  dE6:=dE4*dE2;
  dE7:=dE5*dE2;

  k1:=k1- VII*dE2+VIII*dE4-IX*dE6;

  k2:=k20+X*dE-XI*dE3+XII*dE5-XIIA*dE7;

  RESULT.lat:=k1*180/Pi;       // back to degrees
  RESULT.lon:=k2*180/Pi;
end;
//______________________________________________________________________________

function do_screenshot:boolean;

var
  grab_bmp:TBitmap;

  grab_width,grab_height:integer;

  url_str:string;

  latitude:extended;
  zoom_level:integer;

  i,n:integer;
  zoom_str:string;

  od:Toutdim;
  map_width_mm,map_height_mm:extended;

  shotx,shoty:extended;

  new_shape:Tbgnd_shape;

  lat_lon:Tlat_lon;

  sheet_code:integer;

  scan_factor:extended;

  georef:boolean;
  zoom_ok:boolean;

  shot_list:TStringlist;

  shot_code:integer;

begin
  RESULT:=False;  // default init

     // http://maps.nls.uk/view/128378924#zoom=6&lat=7479&lon=6650&layers=BT       example find by place individual sheets

  sheet_code:=1;  // keep compiler happy

  shot_list:=TStringlist.Create;

  if FileExists(exe_str+'internal\map\mapshot.txt')
     then shot_list.LoadFromFile(exe_str+'internal\map\mapshot.txt')
     else begin
            ShowMessage('error: no screenshot found');
            shot_list.Free;
            EXIT;
          end;

  shot_code:=StrToInt(shot_list.Strings[0]);

  if shot_code<>1     // no valid screenshot
     then begin
            map_loader_form_update;

            map_loader_form.Show;
            map_loader_form.BringToFront;

            shot_list.Free;

            RESULT:=(shot_code=2);   // code 2 she cancelled, valid result
            EXIT;
          end;

  url_str:=shot_list.Strings[1];
  grab_width:=StrToInt(shot_list.Strings[2]);
  grab_height:=StrToInt(shot_list.Strings[3]);

  shot_list.Free;

  if Pos('maps.nls.uk/view/',url_str)>0
     then begin
            i:=alert(4,'php/408    NLS  individual  map  sheet ?',

                          '    `0NLS individual "Find by place" map ?`9'
                         +'||This appears to be an individual scanned sheet from NLS, i.e. not georeferenced to modern maps:'
                         +'||<SPAN NOWRAP>`0'+url_str+'`f</SPAN>'

                         +'||The NLS refer to these maps as "Find by place" maps.'

                         +'||Templot cannot by itself determine which type of map you are displaying, so you must indicate accordingly below. Please check that the screenshot has been sized correctly before using it for track design.'

                         +'||The scale is usually shown in the heading bar above the map, and also on the map itself in the bottom-right corner of the sheet.'

                         +'||• The 1:1250 500m square maps are indicated on the NLS web site as "1940s-1960s". These maps have OS Grid lines marked on them, 5 squares each way.'

                         +'||• The Metric 1:2500 maps are indicated on the NLS web site as "1940s-1960s". Zoom out to the full sheet to see whether it is a 1km wide square sheet, or a 2km wide oblong sheet. These maps have OS Grid lines marked on them.'

                         +'||• The historic County Series 1:2500 maps are indicated on the NLS web site as "1841-1952" or "1890-1960" or older dates. They pre-date the introduction of the OS Grid and do not have grid lines.',

                          'NOT  any  below  ( picture  shape  will  not  be  sized ! )    ','50 inch/mile  1:1250  500 m x 500 m  square     ','25 inch/mile  1:2500  Metric  Series  1 km x 1 km  square    ','25 inch/mile  1:2500  Metric  Series  2 km x 1 km  oblong    ','cancel  screenshot','25 inch/mile  1:2500  County  Series  1.5 mile x 1 mile   ',0);

                        case i of

                            1: sheet_code:=5;

                            2: sheet_code:=4;

                            3: sheet_code:=3;

                            4: sheet_code:=2;

                            5: begin
                                 map_loader_form_update;

                                 map_loader_form.Show;
                                 map_loader_form.BringToFront;

                                 shot_list.Free;

                                 EXIT;
                               end;

                          else sheet_code:=1;

                        end;//case

            try
              n:=Pos('#zoom=',url_str);
              Delete(url_str,1,n+5);

              zoom_str:=Copy(url_str,1,2);    // might be 2 digits   ignore "lat" and "lon" pixels (if present)

              if Pos('&',zoom_str)>0 then zoom_str:=Copy(url_str,1,1);   // was only 1.

              zoom_level:=StrToInt(zoom_str);
            except
              ShowMessage('error - Unable to read zoom level from URL.'
                          +#13+#13+'Please be sure to click "Link to this view" at the bottom of the map,'
                          +' and check that the zoom level is showing in the browser address at the top of the map before making the screenshot.'
                          +#13+#13+'For more information click the "? info >>>" button.');

              map_loader_form_update;

              map_loader_form.Show;
              map_loader_form.BringToFront;

              EXIT;
            end;//try

            georef:=False;  // not slippy map
          end
     else begin   // georeferenced slippy map

               // get zoom and latitude from URL ...

            if parse_url(url_str,zoom_level,lat_lon)=False
               then begin
                      ShowMessage('error - Unable to read URL from map window.');

                      map_loader_form_update;

                      map_loader_form.Show;
                      map_loader_form.BringToFront;

                      EXIT;
                    end;

            zoom_str:=IntToStr(zoom_level);

            georef:=True;  // slippy map
          end;

       // get screenshot bitmap ...

  web_map_help_form.Hide;
  map_loader_form.Hide;

  grab_bmp:=TBitmap.Create;

  if FileExists(exe_str+'internal\map\mapshot.bmp')
     then grab_bmp.LoadFromFile(exe_str+'internal\map\mapshot.bmp')
     else begin
            grab_bmp.Free;

            ShowMessage('error: Unable to make screenshot from browser window.');

            map_loader_form_update;

            map_loader_form.Show;
            map_loader_form.BringToFront;

            EXIT;
          end;

  shotx:=0;      // init
  shoty:=0;

  if map_loader_form.at_zero_checkbox.Checked=False      // he wants to set position...
     then begin
               putdim('Enter the required position for the bottom-left corner of the map on the trackpad.',1,'map  corner  X  position  ( from  left )',shotx,False,True,False,False);     // negative ok, no preset, zero ok, don't terminate on zero.
            n:=putdim('Enter the required position for the bottom-left corner of the map on the trackpad.',1,'map  corner  Y  position  ( from  bottom )',shoty,False,True,False,False);   // negative ok, no preset, zero ok, don't terminate on zero.

            if n<>1           // ???
               then begin
                      grab_bmp.Free;
                      EXIT;
                    end;

            if getdims('map  position  on  trackpad','',map_loader_form,n,od)=True
               then begin
                      shotx:=od[0];
                      shoty:=od[1];
                    end
               else begin               // she cancelled
                      grab_bmp.Free;
                      map_loader_form_update;

                      map_loader_form.Show;
                      map_loader_form.BringToFront;

                      EXIT;
                    end;

          end;

  if georef=False      // individual scanned sheet map...
     then begin

            case sheet_code of

                // County Series: 1860 average dots inside margin (measured at zoom 3)
                // 1.5 miles = 7920ft on old projection.
                // outlines measured on georeferenced index = 7905ft.
                // per dot at zoom 0 = 8*7905/1860 = 34ft   8=2^zoom3

               1: scan_factor:=8*7905/1860;   // = 34 proto-feet per screen dot (at zoom 0)



                // Metric Series oblong: 1832 average dots measured at zoom 3
                // 2km = 6561.68ft
                // outlines measured on georeferenced index = 6555ft.
                // per dot at zoom 0 = 8*6555/1832 = 28.624 proto-ft   8=2^zoom3

               2: scan_factor:=8*6555/1832;



                // Metric Series square: 1422 average dots measured at zoom 3
                // 1km = 3280.84ft
                // outlines measured on georeferenced index = 3279ft.
                // per dot at zoom 0 = 8*3279/1422 = 18.447 proto-ft   8=2^zoom3

               3: scan_factor:=8*3279/1422;



                // 1:1250 500m square: 1420 average dots measured at zoom 3
                // 500m = 1640.42 ft
                // outlines measured on georeferenced index = 1640ft.
                // per dot at zoom 0 = 8*1640/1420 = 9.24 proto-ft   8=2^zoom3

               4: scan_factor:=8*1640/1420;  // 1:1250 500m square = 9.24 proto-ft  per dot at zoom 0

             else scan_factor:=24;  // sheet_code 5   arbitrary  proto-feet per screen dot at zoom 0.

            end;//case

            map_width_mm:=scan_factor*grab_width*scale/Power(2,zoom_level);

          end
     else map_width_mm:=ABS(156543033.928*COS(lat_lon.lat*Pi/180)*grab_width*scale/304.8/Power(2,zoom_level));  // resolution = 156543.033928 metres/pixel * cos(latitude) / (2 ^ zoomlevel)

  map_height_mm:=map_width_mm*grab_height/grab_width;

  with new_shape do begin

    shape_name:='map::'+format_his_name+Copy(IntToStr(map_code),1,1)+'S'+Copy(zoom_str+'  ',1,2)+' screenshot';

    shape_code:=-1;   // -1=picture
    shape_style:=0;   // not used

    wrap_offset:=0;
    show_transparent:=map_loader_form.transparent_checkbox.Checked;  // 215b  was  show_transparent:=False;
    picture_is_metafile:=False;

    hide_bits:=0;     // normal visibility
    option_bits:=0;   // byte;
  end;//with

  with bgnd_form.bgnd_shapes_listbox do begin

    n:=Items.AddObject(new_shape.shape_name,Tbgshape.Create);  // create and insert a new entry in the shapes list.

    Tbgshape(Items.Objects[n]).bgnd_shape:=new_shape;          // put data in list.

    ItemIndex:=n;                                              // make it current.

    with Tbgshape(Items.Objects[n]) do begin

      bgimage:=Tbgimage.Create;

      with bgimage.image_shape do begin

        image_bitmap:=TBitmap.Create;
        rotated_bitmap:=TBitmap.Create;

        // OT-FIRST image_metafile:=TMetafile.Create;    // 213b
        // OT-FIRST rotated_metafile:=TMetafile.Create;  // 213b

        rotated_picture:=TPicture.Create;

        image_bitmap.Assign(grab_bmp);

        // OT-FIRST image_bitmap.PixelFormat:=pf24bit;

        image_width:=image_bitmap.Width;
        image_height:=image_bitmap.Height;

        bgnd_shape.p1.x:=shotx;
        bgnd_shape.p1.y:=shoty;

        bgnd_shape.p2.x:=shotx+map_width_mm;
        bgnd_shape.p2.y:=shoty+map_height_mm;

      end;//with image shape
    end;//with object
  end;//with listbox

  shapes_saved:=False;   // need a fresh save.

  if georef=True
     then begin
            previous_lat_lon:=lat_lon;
            map_loader_form.previous_loc_radio_button.Enabled:=True;
          end;

  shapes_current_state;

  redraw(False);

  if bgnd_form.bgnd_shapes_listbox.Items.Count>0
     then pad_form.fit_shapes_menu_entry.Click;

  grab_bmp.Free;

  ShowMessage('The screenshot has been made and should now be visible as a picture shape on the trackpad behind this message.'
     +#13+#13+'If not, click on the trackpad and then press the SHIFT+HOME keys to toggle background shapes on and off.'
     +#13+#13+'To remove any headings and other extraneous content, use the crop/combine function on the background shapes dialog.');

  map_loader_form_update;

  map_loader_form.Show;
  map_loader_form.BringToFront;

  RESULT:=True;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.FormCreate(Sender: TObject);

var
  test_zoom_html_list:TStringList;

begin
  // OT-FIRST ClientWidth:=900;
  // OT-FIRST ClientHeight:=740;

  previous_lat_lon.lat:=52;      // arbitrary init
  previous_lat_lon.lon:=0-2;

  AutoScroll:=True;

    // for screenshot capture ...

  if FileExists(ExtractFilePath(Application.ExeName)+'test_zoom_page.html')=False
     then begin
            test_zoom_html_list:=TStringList.Create;
            test_zoom_html_list.Clear;
            test_zoom_html_list.Add('<html><head></head><body style="margin:0px;"><img src="test_zoom.png"></body></html>');
            test_zoom_html_list.SaveToFile(ExtractFilePath(Application.ExeName)+'test_zoom_page.html');
            test_zoom_html_list.Free;
          end;

  if FileExists(ExtractFilePath(Application.ExeName)+'test_zoom.png')=False
     then test_zoom_png_image.Picture.SaveToFile(ExtractFilePath(Application.ExeName)+'test_zoom.png');

  if FileExists(ExtractFilePath(Application.ExeName)+'ot_screenshot_capture.exe.manifest')=False
     then manifest_memo.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'ot_screenshot_capture.exe.manifest');

end;
//______________________________________________________________________________

procedure Tmap_loader_form.cancel_buttonClick(Sender: TObject);

begin
  alert_box.Left:=4;

  if alert(7,'    cancel  capture ?',
    'Use this `0cancel`1 button only if the screenshot capture window has stopped responding.'
   +'||Otherwise close the screenshot capture window in the usual way by clicking the top corner X.'
   +'||You will then be able to cancel the screenshot if not wanted.'
   +'||Has the screenshot capture window stopped responding?',
     '','','','','yes','no',0)=5
     then begin
            load_button.Caption:='show  map';
            cancel_clicked:=True;
     end;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label3Click(Sender: TObject);

begin
  Label3.Font.color:=clBlue;
  go_to_url('http://maps.nls.uk');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label4Click(Sender: TObject);

begin
  Label4.Font.color:=clBlue;
  go_to_url('http://maps.nls.uk/os/london-1890s/info.html');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label5Click(Sender: TObject);

begin
  Label5.Font.color:=clBlue;
  go_to_url('http://openstreetmap.org');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label6Click(Sender: TObject);

begin
  Label6.Font.color:=clBlue;
  go_to_url('http://openstreetmap.org/about');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label14Click(Sender: TObject);

begin
  Label14.Font.color:=clBlue;
  go_to_url('http://maps.nls.uk/geo/explore/#zoom=15&lat=51.9996&lon=-0.9989&layers=10&b=4');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label24Click(Sender: TObject);

begin
  Label24.Font.color:=clBlue;
  go_to_url('http://maps.nls.uk/os/6inch-england-and-wales/index.html');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label25Click(Sender: TObject);

begin
  go_to_url('http://maps.nls.uk/os/6inch-2nd-and-later/index.html');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.map_load_groupboxMouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label3.Font.color:=clBlue;
  Label4.Font.color:=clBlue;
  Label5.Font.color:=clBlue;
  Label6.Font.color:=clBlue;

  Label23.Font.color:=clBlue;
  Label24.Font.color:=clBlue;

  info_label.Font.color:=clBlue;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.location_groupboxMouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label14.Font.color:=clBlue;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label3MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label3.Font.color:=clRed;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label4MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label4.Font.color:=clRed;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label5MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label5.Font.color:=clRed;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label6MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label6.Font.color:=clRed;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label14MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label14.Font.color:=clRed;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label23MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label23.Font.color:=clRed;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.Label24MouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label24.Font.color:=clRed;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.FormMouseMove(Sender:TObject; Shift:TShiftState; X,Y:Integer);

begin
  Label3.Font.color:=clBlue;
  Label4.Font.color:=clBlue;
  Label5.Font.color:=clBlue;
  Label6.Font.color:=clBlue;
  Label14.Font.color:=clBlue;

  Label23.Font.color:=clBlue;
  Label24.Font.color:=clBlue;

  info_label.Font.color:=clBlue;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.lon_editEnter(Sender: TObject);     // also lat edit Enter , lat_lon radio click

begin
  location_code:=0;  // 0=long/lat,  1=OS Grid,  2=URL, 3=previous

  lat_lon_radio_button.Checked:=True;

  lon_edit.Font.Color:=clBlue;
  lat_edit.Font.Color:=clBlue;

  url_edit.Font.Color:=clGray;

  os_letters_edit.Font.Color:=clGray;
  os_easting_edit.Font.Color:=clGray;
  os_northing_edit.Font.Color:=clGray;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.os_letters_editEnter(Sender: TObject);

begin
  location_code:=1;  // 0=long/lat,  1=OS Grid,  2=URL, 3=previous

  os_grid_radio_button.Checked:=True;

  lon_edit.Font.Color:=clGray;
  lat_edit.Font.Color:=clGray;

  url_edit.Font.Color:=clGray;

  os_letters_edit.Font.Color:=clBlue;
  os_easting_edit.Font.Color:=clBlue;
  os_northing_edit.Font.Color:=clBlue;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.url_editEnter(Sender: TObject);

begin
  location_code:=2;  // 0=long/lat,  1=OS Grid,  2=URL, 3=previous

  url_radio_button.Checked:=True;

  lon_edit.Font.Color:=clGray;
  lat_edit.Font.Color:=clGray;

  os_letters_edit.Font.Color:=clGray;
  os_easting_edit.Font.Color:=clGray;
  os_northing_edit.Font.Color:=clGray;

  url_edit.Font.Color:=clBlue;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.previous_loc_radio_buttonClick(Sender: TObject);

begin
  location_code:=3;  // 0=long/lat,  1=OS Grid,  2=URL, 3=previous

  lon_edit.Font.Color:=clGray;
  lat_edit.Font.Color:=clGray;

  os_letters_edit.Font.Color:=clGray;
  os_easting_edit.Font.Color:=clGray;
  os_northing_edit.Font.Color:=clGray;

  url_edit.Font.Color:=clGray;
end;
//______________________________________________________________________________

function format_his_name:string;

var
  n:integer;
  name_str:string;

begin
  RESULT:='';  // init

  name_str:=Copy(Trim(map_loader_form.name_edit.Text)+'                    ',1,15);  // pad with spaces to 15 characters

     // check and remove any UTF or control chars in his name ..

  for n:=1 to Length(name_str) do begin
    if (Ord(name_str[n])<32) or (Ord(name_str[n])>127) or (name_str[n]='#') then name_str[n]:='_';    // # is our separator
  end;//next

  RESULT:=name_str;
end;
//______________________________________________________________________________

procedure got_tile;

var
  new_shape:Tbgnd_shape;
  n:integer;

  load_picture:TPicture;

begin
  with new_shape do begin

       // defaults ...

    shape_name:=tile_name_str;    // global

    shape_code:=-1;   // -1=picture
    shape_style:=0;   // not used

    wrap_offset:=0;
    show_transparent:=map_loader_form.transparent_checkbox.Checked;  // map_transparent;
    picture_is_metafile:=False;

    hide_bits:=0;  // normal visibility
    option_bits:=0;     // byte;

    p1.x:=map_org_x+map_tile_count_x*map_tile_size_mm;
    p1.y:=map_org_y+(map_num_tiles_y-map_tile_count_y-1)*map_tile_size_mm;

    p2.x:=p1.x+map_tile_size_mm;    // square ..
    p2.y:=p1.y+map_tile_size_mm;

  end;//with

  with bgnd_form.bgnd_shapes_listbox do begin

    n:=Items.AddObject(new_shape.shape_name,Tbgshape.Create);  // create and insert a new entry in the shapes list.

    Tbgshape(Items.Objects[n]).bgnd_shape:=new_shape;          // put data in list.

    ItemIndex:=n;                                              // make it current.

    with Tbgshape(Items.Objects[n]) do begin

      bgimage:=Tbgimage.Create;     // create new image  3-2-01.

      with bgimage.image_shape do begin

        image_bitmap:=TBitmap.Create;
        rotated_bitmap:=TBitmap.Create;

        // OT-FIRST image_metafile:=TMetafile.Create;    // 213b
        // OT-FIRST rotated_metafile:=TMetafile.Create;  // 213b

        rotated_picture:=TPicture.Create;

        load_picture:=TPicture.Create; //0.93.a

        try
          case map_code of
              0: load_picture.LoadFromFile(exe_str+'internal\tile\osm_tile.png');        // OpenStreetMap PNG
              1: load_picture.LoadFromFile(exe_str+'internal\tile\nls_tile.jpg');        // NLS London  JPG
              2: load_picture.LoadFromFile(exe_str+'internal\tile\nls_tile.jpg');        // NLS 6-inch  JPG
            else load_picture.LoadFromFile(exe_str+'internal\empty_picture.bmp');        // ??? invalid map_code for tiled maps
          end;//case

          image_bitmap.Assign(load_picture.Graphic);

          // OT-FIRST image_bitmap.PixelFormat:=pf24bit;  // for deeper zooming  (was probably 32 bit)

          image_width:=image_bitmap.Width;
          image_height:=image_bitmap.Height;

        finally
          load_picture.Free;
        end;//try
      end;//with
    end;//with

    copy_draw_to_pad; // remove any previous highlighting

    ItemIndex:=n;
    draw_bg_shapes(pad_form.Canvas,ItemIndex,clRed);   // show new bitmap and highlight in red, directly on the pad.
  end;//with

  shapes_saved:=False;      // need a resave.
  shapes_current_state;

  do_rollback:=False;       // no need to put this change in rollback register on redraw.
  redraw(True);
end;
//______________________________________________________________________________

function obtain_tile_from_server(zoom_str,xtile_str,ytile_str:string):boolean;

var
  map_str:string;
  http_count:integer;

  http_result:Boolean;     // OT-FIRST ...
  http_sender:THTTPSend;
  file_str:string;

begin
  RESULT:=False;  // init

  if (map_code<0) or (map_code>2) then EXIT;   // ??? not a tiled map

  Sleep(250);  // don't call server more than once every 250ms.    4 tiles per second.

  http_count:=0;  // init reset

  repeat
    if getting_tile=False
      then BREAK
      else Sleep(200);  // wait for previous tile to finish

    INC(http_count);
    if http_count>50    // 10 seconds
       then begin
              ShowMessage('Sorry, unable to obtain the map tile. The server is taking too long to respond.'+#13+#13+'Please check your internet connection.');
              EXIT;
            end;

  until 0<>0;

  getting_tile:=True;        // global lock

     // create shape name = map name (padded with spaces to 20 chars) + map_code(1) + spare(1) + zoom_level(padded to 2) + separator(#) + xtile_str(padded to 10) + separator(#) + ytile_str(padded to 10).

  tile_name_str:=map_name_str+Copy(IntToStr(map_code),1,1)+'T'+Copy(zoom_str+'  ',1,2)+'#'+Copy(xtile_str+'          ',1,10)+'#'+Copy(ytile_str+'          ',1,10);

  map_str:='';   // init

  if map_code=0
     then begin
            DeleteFile(exe_str+'internal\tile\osm_tile.png');                                             // if it already exists

            file_str:=exe_str+'internal\tile\osm_tile.png';  // OT-FIRST

            map_str:='http://a.tile.openstreetmap.org/'+zoom_str+'/'+xtile_str+'/'+ytile_str+'.png';      // where from
          end;

  if map_code=1
     then begin
            DeleteFile(exe_str+'internal\tile\nls_tile.jpg');                                                   // if it already exists

            file_str:=exe_str+'internal\tile\nls_tile.jpg';  // OT-FIRST

            map_str:='http://nls-0.tileserver.com/U1k2BDmGHaow/'+zoom_str+'/'+xtile_str+'/'+ytile_str+'.jpg';   // where from
          end;

  if map_code=2
     then begin
            DeleteFile(exe_str+'internal\tile\nls_tile.jpg');                                                   // if it already exists

            file_str:=exe_str+'internal\tile\nls_tile.jpg';  // OT-FIRST

            map_str:='http://nls-0.tileserver.com/U1k2BDr0GorO/'+zoom_str+'/'+xtile_str+'/'+ytile_str+'.jpg';   // where from
          end;

  http_sender:=THTTPSend.Create;

  try
    http_result:=http_sender.HTTPMethod('GET',map_str);

    if (http_sender.ResultCode>=100) and (http_sender.ResultCode<=299)
       then begin
              http_sender.Document.SaveToFile(file_str);

              got_tile;              // put in background shapes

              getting_tile:=False;   // ready for another one    global

              RESULT:=True;
            end
       else begin

              ShowMessage('Sorry, unable to obtain the map tiles. Web site code: '+IntToStr(http_sender.ResultCode)
              +#13+#13+'You may have set a location outside the area covered by the selected map series.'
              +#13+#13+'You may have set a zoom level outside the valid range for this map series.'
              +#13+#13+'For NLS maps, the API subscription may have expired. Please ask on the Templot Club user forum.'
              +#13+#13+'Otherwise please check your internet connection, or try again later.');

              getting_tile:=False;  // reset global lock
              abort_map:=True;      // no more tiles
            end
  finally
    http_sender.Free;
  end;
end;
//______________________________________________________________________________

procedure zoom_to_map;

var
  n:integer;

  shape_str:string;

  wl_factor:extended;
  margin_factor:extended;

begin
  map_name_str:='map::'+format_his_name;    // global

  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if Count<1 then EXIT;   // no shapes ???

    for n:=0 to Count-1 do begin

      if Pos(map_name_str,Strings[n])=1
         then begin
                shape_str:=Strings[n];

                if shape_str[22]='S'    // screenshot
                   then begin
                          bgnd_form.bgnd_shapes_listbox.ItemIndex:=n;
                          bgnd_form.zoom_fit_shape_menu_entry.Click;
                          BREAK;
                        end;


                if shape_str[22]='T'    // tiles
                   then begin
                           if get_tile_extents(map_name_str)=False
                              then begin
                                     ShowMessage('error - Sorry, unable to identify all map tiles.');
                                     EXIT;
                                   end;

                           if screeny<minfp then EXIT;         // ??
                           wl_factor:=screenx/screeny;

                           margin_factor:=1.10;         // arbitrary 10% extra for margins.

                           screenx:=(xtile_max_p2.x-xtile_min_p1.x)*margin_factor;

                               // ytile_min at top!!!

                           if screenx<((ytile_min_p2.y-ytile_max_p1.y)*margin_factor*wl_factor) then screenx:=(ytile_min_p2.y-ytile_max_p1.y)*margin_factor*wl_factor;

                           if screenx<screenx_min then screenx:=screenx_min; // minimum for screen width (max zoom in).
                           if screenx>screenx_max then screenx:=screenx_max; // maximum zoom out.

                             // centralize on pad..

                          zoom_offsetx:=xtile_min_p1.x-(screenx-(xtile_max_p2.x-xtile_min_p1.x))/2;
                          if wl_factor>minfp then zoom_offsety:=ytile_max_p1.y-(screenx/wl_factor-(ytile_min_p2.y-ytile_max_p1.y))/2;

                          pad_form.lock_scaling_menu_entry.Click;       // lock pad zoom.

                          BREAK;
                        end;
              end;
    end;//next
  end;//with

  redraw(True);
end;
//______________________________________________________________________________

function parse_url(str:string; var zoom_level:integer; var lat_lon:Tlat_lon):boolean;

      // get zoom and latitude from URL ...

var
  n:integer;
  zoom_str,lat_str,lon_str:string;

begin
  RESULT:=False;  // init

  if (Pos('maps.nls.uk',str)=0)
  and (Pos('openstreetmap.org',str)=0)
  and (Pos('google.co.uk/maps',str)=0)
  and (Pos('google.com/maps',str)=0)
  and (Pos('ordnancesurvey.co.uk',str)=0)

     then begin
            ShowMessage('error - Invalid URL.'+#13+#13+'Only NLS, OpenStreetMap, Google, OrdnanceSurvey map pages are supported.');
            EXIT;
          end;

      //http://maps.nls.uk/geo/explore/#zoom=15&lat=52.3754&lon=-2.3161&layers=171&b=6   example

  if Pos('maps.nls.uk',str)<>0
     then begin

            n:=Pos('#zoom=',str);
            Delete(str,1,n+5);

            n:=Pos('&lat=',str);
            zoom_str:=Copy(str,1,n-1);

            Delete(str,1,n+4);

            n:=Pos('&lon=',str);
            lat_str:=Copy(str,1,n-1);

            Delete(str,1,n+4);

            n:=Pos('&',str);
            lon_str:=Copy(str,1,n-1);

            try
              zoom_level:=Round(StrToFloat(Trim(zoom_str)));
              lat_lon.lat:=StrToFloat(Trim(lat_str));
              lat_lon.lon:=StrToFloat(Trim(lon_str));
            except
              ShowMessage('error - Unable to read NLS URL.'+#13+#13+'Copy it from your browser address bar when displaying a map at the required location.');
              EXIT;
            end;//try

          end;

      //http://www.openstreetmap.org/#map=19/51.41804/-0.07285

  if Pos('openstreetmap.org',str)<>0
     then begin
            n:=Pos('#map=',str);
            Delete(str,1,n+4);

            n:=Pos('/',str);
            zoom_str:=Copy(str,1,n-1);

            Delete(str,1,n);

            n:=Pos('/',str);
            lat_str:=Copy(str,1,n-1);

            Delete(str,1,n);

            lon_str:=str;

            try
              zoom_level:=Round(StrToFloat(Trim(zoom_str)));
              lat_lon.lat:=StrToFloat(Trim(lat_str));
              lat_lon.lon:=StrToFloat(Trim(lon_str));
            except
              ShowMessage('error - Unable to read OpenStreetMap URL.'+#13+#13+'Copy it from your browser address bar when displaying a map at the required location.');
              EXIT;
            end;//try

          end;

      //https://www.google.co.uk/maps/@52.3258601,-2.274551,16z

      //https://www.google.co.uk/maps/@52.2683549,-2.2637109,16.5z

  if Pos('google.co',str)<>0
     then begin
            n:=Pos('/maps/@',str);
            Delete(str,1,n+6);

            n:=Pos(',',str);
            lat_str:=Copy(str,1,n-1);

            Delete(str,1,n);

            n:=Pos(',',str);
            lon_str:=Copy(str,1,n-1);

            Delete(str,1,n);

            n:=Pos('z',str);
            zoom_str:=Copy(str,1,n-1);

            try
              zoom_level:=Round(StrToFloat(Trim(zoom_str)));
              lat_lon.lat:=StrToFloat(Trim(lat_str));
              lat_lon.lon:=StrToFloat(Trim(lon_str));
            except
              ShowMessage('error - Unable to read Google URL.'+#13+#13+'Copy it from your browser address bar when displaying a map at the required location.');
              EXIT;
            end;//try

          end;

          //http://osmaps.ordnancesurvey.co.uk/52.37134,-2.71626,18

  if Pos('osmaps.ordnancesurvey.co.uk',str)<>0
     then begin
            n:=Pos('.co.uk/',str);
            Delete(str,1,n+6);

            n:=Pos(',',str);
            lat_str:=Copy(str,1,n-1);

            Delete(str,1,n);

            n:=Pos(',',str);
            lon_str:=Copy(str,1,n-1);

            Delete(str,1,n);

            zoom_str:=Copy(str,1,2);

            try
              zoom_level:=Round(StrToFloat(Trim(zoom_str)));
              lat_lon.lat:=StrToFloat(Trim(lat_str));
              lat_lon.lon:=StrToFloat(Trim(lon_str));
            except
              ShowMessage('error - Unable to read Ordnance Survey URL.'+#13+#13+'Copy it from your browser address bar when displaying a map at the required location.');
              EXIT;
            end;//try

          end;

  RESULT:=True;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.load_buttonClick(Sender: TObject);

const
  size_help_str:string='Enter the required size of the map on the trackpad.'
                    +'||Do not start with too large a size, otherwise you may end up waiting for hundreds of tiles to load.'
                    +'||The map area can be easily increased later by adding additional rows and columns of tiles.|| ';

label
  123;

var
  os_grid_org:Tos_grid;

  lat_lon_org:Tlat_lon;

  grid_str:string;

  xtile_str,ytile_str,zoom_str:string;

  xtile_org,ytile_org:extended;

  numtilesx,numtilesy:extended;

  numx,numy:integer;

  zoom_level:integer;

  dummy_zoom:integer;

  lat_radians:extended;

  xscaling,yscaling:extended;

  mapx,mapy:integer;

  n:integer;
  od:Toutdim;

  new_shape:Tbgnd_shape;

  lat_long_entry:boolean;

  bgnd_shapes_text_str,name_str:string;

  easting_str,northing_str:string;

  east,north:integer;

  load_picture:TPicture;

  wl_factor,max_long,min_long,max_wide,min_wide,margin_factor:extended;

  web_str:string;

  url_str:string;

  seconds:integer;

  pad_width:integer;

  capture_exe_str,capture_url_str:string;

begin
  if (gauge_i=t_T55_i) and (scale=5.5) and (g=25.4)
     then begin
            if alert(7,'    using  T-55  gauge',
               'You are currently using the fictional T-55 start-up gauge and scale.'
              +'||Have you forgotten to set your model scale ?'
              +'||Your intended model scale must first be set in order for maps to be scaled (sized) correctly.'
              +'||<SPAN STYLE="COLOR:RED;">• DO NOT</SPAN> change the model scale after adding maps - they will <SPAN STYLE="COLOR:RED;">NOT</SPAN> be re-sized to match.',
               '','','','continue  using  T-55','cancel','',0)=5
               then EXIT;
          end
     else t_55_label.Hide;

  pause_button.Caption:='pause';  // reset if stopped after pause.

  if load_button.Tag=1
     then begin
            zoom_to_map;
            EXIT;
          end;

  if loading_in_progress=True then EXIT;

  if osm_radio_button.Checked=True
     then map_code:=0
     else if nls_london_radio_button.Checked=True
             then map_code:=1
             else if nls_6inch_radio_button.Checked=True
                     then map_code:=2
                     else if screenshot_25inch_radio_button.Checked=True
                             then map_code:=3
                             else if screenshot_50inch_radio_button.Checked=True
                                     then map_code:=4
                                     else if screenshot_25k_radio_button.Checked=True
                                             then map_code:=5
                                             else if other_screenshot_url_radio_button.Checked=True
                                                     then map_code:=6

                                                     else begin
                                                            ShowMessage('Please select a map to load.');
                                                            EXIT;
                                                          end;  


  bgnd_shapes_text_str:=bgnd_form.bgnd_shapes_listbox.Items.Text;

  name_str:=format_his_name;

  if Trim(name_str)=''
     then begin
            ShowMessage('Please enter a name for the map, of up to 15 characters.');
            EXIT;
          end;

  map_name_str:='map::'+name_str;    // global

  if Pos(map_name_str,bgnd_shapes_text_str)<>0     // ??? should not get here
     then begin
            ShowMessage('Error - there is already one or more background shapes with the name: "'+map_name_str+'"'
               +#13+#13+'Please enter a different name. Simply adding a suffix should be sufficient, for example: '+name_str+'-A'
               +#13+#13+'(Providing the new name does not exceed 15 characters.)');
            EXIT;
          end;

  grid_str:='';   //init

  case location_code of

       0: begin      // long/lat
            try
              lat_lon_org.lon:=StrToFloat(lon_edit.Text);
              lat_lon_org.lat:=StrToFloat(lat_edit.Text);
            except
              ShowMessage('Error - longitude or latitude is not valid.');
              EXIT;
            end;//try

            if (ABS(lat_lon_org.lon)>180) or (ABS(lat_lon_org.lat)>85)
               then begin
                      ShowMessage('Error - longitude or latitude is outside the valid range.');
                      EXIT;
                    end;
          end;

       1: begin      // entered OS Grid ...

            easting_str:=Trim(os_easting_edit.Text);
            northing_str:=Trim(os_northing_edit.Text);

            if (Length(easting_str)=6) and (Length(northing_str)=6)      // 6-digits, no letters needed
                then begin
                       try
                         east:=StrToInt(easting_str);
                         north:=StrToInt(northing_str);
                       except
                         ShowMessage('Invalid OS Grid reference.'+#13+#13+'Grid numbers must contain valid digits 0-9 only.');
                         got_osgrid:=False;
                         EXIT;
                       end;//try

                          // modify by -100, 25 metres, arbitrary for better result...

                       os_grid_org.easting:=east-100;
                       os_grid_org.northing:=north+25;

                       got_osgrid:=True;
                     end
                else begin      // letters and up to 5 digits ..

                       os_grid_org:=parse_os_grid(0-100,25,Trim(os_letters_edit.Text),easting_str,northing_str);    // in web_browser_unit

                       if got_osgrid=False then EXIT;  // invalid grid letters
                     end;

            lat_lon_org:=os_grid_to_lat_lon(os_grid_org.easting,os_grid_org.northing);
          end;


       2: begin                                           //  get from URL
            url_str:=LowerCase(Trim(url_edit.Text));

            if parse_url(url_str,dummy_zoom,lat_lon_org)=False then EXIT;           // zoom ignored in URL, get location only
          end;

       3: lat_lon_org:=previous_lat_lon;

     else begin                                       // ???
            ShowMessage('error finding location');
            EXIT;
          end;

  end;//case

       // get model space...

  map_org_x:=0;   // init  ...
  map_org_y:=0;

  base_width:=500*scale;   // init ...
  base_height:=250*scale;

  123:

  if map_code<3     // tiled maps only ...
     then begin
               putdim('Enter the required position for the bottom-left corner of the map on the trackpad.',1,'map  corner  X  position  ( from  left )',map_org_x,False,True,False,False);     // negative ok, no preset, zero ok, don't terminate on zero.
               putdim('Enter the required position for the bottom-left corner of the map on the trackpad.',1,'map  corner  Y  position  ( from  bottom )',map_org_y,False,True,False,False);   // negative ok, no preset, zero ok, don't terminate on zero.

               putdim(size_help_str,1,'width  of  mapped  area',base_width,True,True,True,False);     // no negative, no preset, no zero, don't terminate on zero.
            n:=putdim(size_help_str,1,'height  of  mapped  area',base_height,True,True,True,False);   // no negative, no preset, no zero, don't terminate on zero.

            if n<>3 then EXIT;

            if getdims('map  size  and  position','',pad_form,n,od)=True
               then begin
                      map_org_x:=od[0];   // global
                      map_org_y:=od[1];   // global

                      base_width:=ABS(od[2]);
                      base_height:=ABS(od[3]);
                    end
               else EXIT;  // cancelled
          end;

  try
    zoom_level:=StrToInt(Trim(zoom_edit.Text));
  except
    case map_code of                     // trial and error ...

              // tiled ...

        0: zoom_level:=19;   // OSM
        1: zoom_level:=21;   // NLS 60-inch London
        2: zoom_level:=19;   // NLS 6-inch GB

              // screenshots

        3: zoom_level:=19;   // NLS 25-inch GB
        4: zoom_level:=21;   // NLS 50-inch Scotland
        5: zoom_level:=16;   // NLS 25K GB
        6: zoom_level:=19;   // URL
      else zoom_level:=17;   // ???
    end;//case

    zoom_edit.Text:=IntToStr(zoom_level);
  end;//try

  if (zoom_level>22) or (zoom_level<15)
     then begin
            ShowMessage('error - Zoom level outside useful range (15-22).');

            case map_code of                     // trial and error ...

                      // tiled ...

                0: zoom_level:=19;   // OSM
                1: zoom_level:=21;   // NLS 60-inch London
                2: zoom_level:=19;   // NLS 6-inch GB

                      // screenshots

                3: zoom_level:=19;   // NLS 25-inch GB
                4: zoom_level:=21;   // NLS 50-inch Scotland
                5: zoom_level:=16;   // NLS 25K GB
                6: zoom_level:=19;   // URL
              else zoom_level:=17;   // ???
            end;//case

            zoom_edit.Text:=IntToStr(zoom_level);
          end;

  zoom_str:=IntToStr(zoom_level);

  if map_code>2  // screenshots
     then begin

            case map_code of

                3: web_str:='http://maps.nls.uk/geo/explore/#zoom='+zoom_str+'&lat='+FloatToStr(lat_lon_org.lat)+'&lon='+FloatToStr(lat_lon_org.lon)+'&layers=168&b=4';    // 25-inch GB + aerial
                4: web_str:='http://maps.nls.uk/geo/explore/#zoom='+zoom_str+'&lat='+FloatToStr(lat_lon_org.lat)+'&lon='+FloatToStr(lat_lon_org.lon)+'&layers=170&b=7';    // 50-inch Scotland + OSM
                5: web_str:='http://maps.nls.uk/geo/explore/#zoom='+zoom_str+'&lat='+FloatToStr(lat_lon_org.lat)+'&lon='+FloatToStr(lat_lon_org.lon)+'&layers=10&b=9a';    // 25K GB + OS 6-inch

                6: begin
                     web_str:=Trim(screenshot_url_edit.Text);   // init   // from URL

                     web_str:=StringReplace(web_str,'https','http',[]);

                     if use_location_checkbox.Checked=True     // use domain, but add location and zoom from user settings
                        then begin

                               url_str:=LowerCase(web_str);

                               //http://maps.nls.uk/geo/explore/#zoom=15&lat=52.3754&lon=-2.3161&layers=171&b=6

                               if Pos('nls',url_str)<>0
                                  then begin
                                         web_str:='http://maps.nls.uk/geo/explore/#zoom='+zoom_str+'&lat='+FloatToStr(lat_lon_org.lat)+'&lon='+FloatToStr(lat_lon_org.lon);
                                         n:=Pos('&layers=',url_str);
                                         if n>0 then web_str:=web_str+Copy(url_str,n,50);
                                       end;

                               //http://www.openstreetmap.org/#map=19/51.41804/-0.07285

                               if Pos('openstreetmap',url_str)<>0
                                  then web_str:='http://www.openstreetmap.org/#map='+zoom_str+'/'+FloatToStr(lat_lon_org.lat)+'/'+FloatToStr(lat_lon_org.lon);


                               //http://www.google.co.uk/maps/@52.3258601,-2.274551,16z

                               if Pos('google',url_str)<>0
                                  then  web_str:='http://www.google.co.uk/maps/@'+FloatToStr(lat_lon_org.lat)+','+FloatToStr(lat_lon_org.lon)+','+zoom_str+'z';


                               //http://osmaps.ordnancesurvey.co.uk/52.37134,-2.71626,18

                               if Pos('ordnancesurvey',url_str)<>0
                                  then  web_str:='http://osmaps.ordnancesurvey.co.uk/'+FloatToStr(lat_lon_org.lat)+','+FloatToStr(lat_lon_org.lon)+','+zoom_str;

                             end;
                   end;

              else web_str:='http://maps.nls.uk/geo/explore/#zoom='+zoom_str+'&lat='+FloatToStr(lat_lon_org.lat)+'&lon='+FloatToStr(lat_lon_org.lon)+'&layers=10&b=4';     // 25K + OSM

            end;//case

            screenshot_url_edit.Text:=web_str;      // he might want to copy it

            pad_width:=pad_form.Width;

            pad_form.Width:=Screen.Width div 4;    // ensure screen capture visible

            DeleteFile(exe_str+'internal\map\mapshot.txt');      // init

            capture_exe_str:='"'+exe_str+'ot_screenshot_capture.exe"';
            capture_url_str:='"'+web_str+'"';

            if ShellExecute(0,'open',PChar(capture_exe_str),PChar(capture_url_str),nil,SW_SHOWNORMAL)<=32
               then begin
                      ShowMessage('error: Unable to show map.');
                      EXIT;
                    end;

            Application.ProcessMessages;

            if screenshot_msg_pref=False
               then begin
                      web_map_help_form.Show;
                      web_map_help_form.BringToFront;
                    end;

            cancel_clicked:=False;    // init
            cancel_button.Show;

            load_button.Enabled:=False;   // no clicks until ready
            load_button.Caption:='map  showing';

            repeat                                        // wait for capture
              Application.ProcessMessages;
              Sleep(300);
            until (FileExists(exe_str+'internal\map\mapshot.txt')) or (cancel_clicked=True);

            cancel_button.Hide;
            web_map_help_form.Hide;

            load_button.Enabled:=True;

            pad_form.Width:=pad_width;  // restore

            if cancel_clicked=True then EXIT;

            if do_screenshot=False
               then ShowMessage('Sorry, it has not been possible to make a screenshot background map.');

            EXIT;     // no return here, see do_screenshot in web_browser_unit

          end;  // end screenshots

       //------------------

    // tiled maps ...

  xscaling:=Power(2,zoom_level);
  yscaling:=Power(2,zoom_level-1);

      // org tile ...

  lat_radians:=lat_lon_org.lat*Pi/180;
  xtile_org:=xscaling*((lat_lon_org.lon+180)/360);                             // in tile units (floats) ..
  ytile_org:=yscaling*(1-(LN(TAN(lat_radians)+(1/COS(lat_radians)) )/Pi));

  map_tile_size_mm:=ABS(156543033.928*COS(lat_lon_org.lat*Pi/180)*256*scale/304.8/Power(2,zoom_level));  //resolution = 156543.033928 metres/pixel * cos(latitude) / (2 ^ zoomlevel)
                                                                                                         // 156543.03392804096153584694438047
  numtilesx:=base_width/map_tile_size_mm;  // floats
  numtilesy:=base_height/map_tile_size_mm;

  map_num_tiles_x:=TRUNC(numtilesx)+1;     // globals  +1 to ensure coverage
  map_num_tiles_y:=TRUNC(numtilesy)+1;

  numx:=map_num_tiles_x;  // locals for loop
  numy:=map_num_tiles_y;

  if (numx*numy)>1500
     then begin
            Screen.Cursor:=crDefault;
            ShowMessage('Map too large. Your map area would require '+IntToStr(numx*numy)+' image tiles.'
                        +#13+#13+'Having more than 1500 tiles could prevent rapid panning and zooming, and may overload the tile server if loaded in one go.'
                        +#13+#13+'Please try again with a smaller map area. If necessary it can be extended later by adding extra rows and columns.');
            goto 123;
          end;

  seconds:=Ceil(numx*numy/4);

  if alert(7,'    ready  to  load  map',

             'You are about to load '+IntToStr(numx*numy)+' map image tiles.'
            +'||To avoid impacting other users of the tile server, Templot loads tiles at a maximum rate of 4 tiles per second.'
            +'||This means your map will take at least '+IntToStr(seconds)+' seconds to load.'
            +'||On slow systems or connections it may take a lot longer.'
            +'|||If the map is satisfactory you can save your background shapes so that you don''t need to load the map from the server again in future, or remember the settings.',
             '','','','','cancel','continue  -  load  map',0)=5

     then EXIT;

  abort_map:=False;    // init;

     // first zoom pad to map area ...

  if screeny<minfp then EXIT;         // ??
  wl_factor:=screenx/screeny;

  max_long:=map_org_x+base_width;
  min_long:=map_org_x;

  max_wide:=map_org_y+base_height;
  min_wide:=map_org_y;

  margin_factor:=1.10;               // arbitrary 10% extra for margins

  screenx:=(max_long-min_long)*margin_factor;

  if screenx<((max_wide-min_wide)*margin_factor*wl_factor) then screenx:=(max_wide-min_wide)*margin_factor*wl_factor;

  if screenx<screenx_min then screenx:=screenx_min; // minimum for screen width (max zoom in).
  if screenx>screenx_max then screenx:=screenx_max; // maximum zoom out.

             // centralize on pad..

  zoom_offsetx:=min_long-(screenx-(max_long-min_long))/2;
  if wl_factor>minfp then zoom_offsety:=min_wide-(screenx/wl_factor-(max_wide-min_wide))/2;

  pad_form.lock_scaling_menu_entry.Click;  // lock pad zoom.
  redraw(False);


     // adjust from centre to NW corner origin for tiles ..

  xtile_org:=xtile_org-numtilesx/2;
  ytile_org:=ytile_org-numtilesy/2;

  //do_bgnd(False);  // show the bgnd shapes dialog as they load.

  loading_in_progress:=True;
  stop_loading:=False;
  pause_loading:=False;

  load_button.Hide;

  pause_button.Show;
  stop_button.Show;

  //if stop_button.Showing then stop_button.SetFocus;    // ??? prevents it showing

  Screen.Cursor:=crAppStart;

  try

    for mapy:=0 to numy-1 do begin     // tile row

      map_tile_count_y:=mapy;   // global

      for mapx:=0 to numx-1 do begin   // tile column


        map_tile_count_x:=mapx;   // global

        xtile_str:=IntToStr(TRUNC(xtile_org+mapx));
        ytile_str:=IntToStr(TRUNC(ytile_org+mapy));

        if obtain_tile_from_server(zoom_str,xtile_str,ytile_str)=False
           then EXIT;

        if abort_map=True
           then EXIT;

        if show_loading_checkbox.Checked=True then redraw(False);   // show map loading

        if stop_loading=True then EXIT;

        repeat
          Application.ProcessMessages;
        until (pause_loading=False) or (stop_loading=True);

      end;//next column
    end;// next row

       // add the copyright label ...

    with new_shape do begin

         // defaults ...

      shape_name:=map_name_str+' copyright notice';

      shape_code:=-1;   // -1=picture
      shape_style:=0;   // not used

      wrap_offset:=0;
      show_transparent:=False;
      picture_is_metafile:=False;

      hide_bits:=0;       // normal visibility
      option_bits:=0;     // byte;
    end;//with

    with bgnd_form.bgnd_shapes_listbox do begin

      n:=Items.AddObject(new_shape.shape_name,Tbgshape.Create);  // create and insert a new entry in the shapes list.

      Tbgshape(Items.Objects[n]).bgnd_shape:=new_shape;          // put data in list.

      ItemIndex:=n;                                              // make it current.

      with Tbgshape(Items.Objects[n]) do begin

        bgimage:=Tbgimage.Create;

        with bgimage.image_shape do begin

          image_bitmap:=TBitmap.Create;
          rotated_bitmap:=TBitmap.Create;

          // OT-FIRST image_metafile:=TMetafile.Create;    // 213b
          // OT-FIRST rotated_metafile:=TMetafile.Create;  // 213b

          rotated_picture:=TPicture.Create;

          load_picture:=TPicture.Create; //0.93.a

          try
            case map_code of
                0: load_picture.LoadFromFile(exe_str+'internal\tile\osm_copyright.png');    // OpenStreetMap
                1: load_picture.LoadFromFile(exe_str+'internal\tile\nls_copyright.png');    // NLS London
                2: load_picture.LoadFromFile(exe_str+'internal\tile\nls_copyright.png');    // NLS 6-inch
              else load_picture.LoadFromFile(exe_str+'internal\empty_picture.bmp');         // ??? invalid map_code  not a tiled map
            end;//case

            image_bitmap.Assign(load_picture.Graphic);

            // OT-FIRST image_bitmap.PixelFormat:=pf24bit;

            image_width:=image_bitmap.Width;
            image_height:=image_bitmap.Height;

            bgnd_shape.p1.x:=map_org_x+base_width-map_tile_size_mm;
            bgnd_shape.p1.y:=map_org_y;

            bgnd_shape.p2.x:=map_org_x+base_width;
            bgnd_shape.p2.y:=bgnd_shape.p1.y+map_tile_size_mm*image_height/image_width;


          finally
            load_picture.Free;
          end;//try

        end;//with image shape
      end;//with object
    end;//with listbox

       // add the map label ...

    with new_shape do begin
      shape_name:=map_name_str;

      hide_bits:=0;     // 214a  normal visibility
      option_bits:=0;   // byte;
      shape_code:=3;    // 3=label
      shape_style:=0;   // 0=transparent/empty

      wrap_offset:=0;                   // default (used only for image wrapping)
      show_transparent:=False;          // default (used only for images)
      picture_is_metafile:=False;       // default (used only for images) //213b

      p1.x:=map_org_x;                  // top left
      p1.y:=map_org_y+base_height;

      p2.x:=0;
      p2.y:=0;
    end;//with

    with bgnd_form.bgnd_shapes_listbox do begin
      with Items do begin
        n:=AddObject(new_shape.shape_name,Tbgshape.Create);  // create and insert a new line in the shapes list.
        Tbgshape(Objects[n]).bgimage:=nil;
        Tbgshape(Objects[n]).bgnd_shape:=new_shape;          // put data in list.
      end;//with
    end;//with

       // add the map rectangle ...

    with new_shape do begin
      shape_name:=map_name_str+' rectangle';

      hide_bits:=0;     // 214a  normal visibility
      option_bits:=0;   // byte;
      shape_code:=1;    // 1=rectangle
      shape_style:=0;   // 0=transparent/empty

      wrap_offset:=0;                   // default (used only for image wrapping)
      show_transparent:=False;          // default (used only for images)
      picture_is_metafile:=False;       // default (used only for images) //213b

      p1.x:=map_org_x;
      p1.y:=map_org_y;

      p2.x:=p1.x+base_width;
      p2.y:=p1.y+base_height;
    end;//with

    with bgnd_form.bgnd_shapes_listbox do begin
      with Items do begin
        n:=AddObject(new_shape.shape_name,Tbgshape.Create);  // create and insert a new line in the shapes list.
        Tbgshape(Objects[n]).bgimage:=nil;
        Tbgshape(Objects[n]).bgnd_shape:=new_shape;          // put data in list.

        ItemIndex:=n;                                        // make it current.
      end;//with
    end;//with

    shapes_saved:=False;   // need a fresh save.

  finally
    shapes_current_state;  // update the form.

    redraw(False);

    if bgnd_form.bgnd_shapes_listbox.Items.Count>0
       then pad_form.fit_shapes_menu_entry.Click;

    Screen.Cursor:=crDefault;

    loading_in_progress:=False;

    stop_button.Hide;
    pause_button.Hide;

    load_button.Show;

    map_loader_form_update;

    map_loader_form.Show;
    map_loader_form.BringToFront;

  end;//try
end;
//______________________________________________________________________________

procedure Tmap_loader_form.background_shapes_buttonClick(Sender: TObject);

begin
  do_bgnd(False);  // False = show new shape tab
end;
//______________________________________________________________________________

procedure map_loader_form_update;

var
  bgnd_shapes_text_str,shape_name_str,shape_str:string;

  found_str,list_str:string;
  n:integer;

  server:integer;

begin

  if NOT ( (gauge_i=t_T55_i) and (scale=5.5) and (g=25.4) )
     then map_loader_form.t_55_label.Hide;

  map_loader_form.stop_button.Hide;
  map_loader_form.pause_button.Hide;

  map_loader_form.load_button.Show;

    // see if map already exists ...

  bgnd_shapes_text_str:=bgnd_form.bgnd_shapes_listbox.Items.Text;

  shape_name_str:='map::'+format_his_name;

  if Pos(shape_name_str,bgnd_shapes_text_str)<>0      // map exists
     then begin
            map_loader_form.load_button.Tag:=1;

            map_loader_form.load_button.Caption:='zoom  to  map';

            map_loader_form.load_new_label.Visible:=True;

            map_loader_form.delete_button.Enabled:=True;

            map_loader_form.zoom_edit.Visible:=False;
            map_loader_form.zoom_label.Visible:=False;

                // update current map ...

            with bgnd_form.bgnd_shapes_listbox.Items do begin
              for n:=0 to Count-1 do begin

              if Pos(shape_name_str,Strings[n])=1
                 then begin

                        shape_str:=Strings[n];

                        with map_loader_form do begin

                          if Pos('screenshot',shape_str)>0           // not a tiled map
                             then begin

                                    bgnd_form.bgnd_shapes_listbox.ItemIndex:=n;   // make it current in list  215b

                                    extend_area_groupbox.Visible:=False;
                                    crop_area_groupbox.Visible:=False;

                                    server:=StrToInt(shape_str[21]);

                                    case server of
                                       3: current_map_label.Caption:=screenshot_25inch_radio_button.Caption;
                                       4: current_map_label.Caption:=screenshot_50inch_radio_button.Caption;
                                       5: current_map_label.Caption:=screenshot_25k_radio_button.Caption;
                                       6: current_map_label.Caption:='screenshot  map  from  URL';
                                    end;//case

                                  end;

                          if Length(shape_str)<>46 then CONTINUE;      // not a map tile  (label, copyright, rectangle, screenshot)

                          server:=StrToInt(shape_str[21]);

                          case server of
                             0: current_map_label.Caption:=osm_radio_button.Caption;
                             1: current_map_label.Caption:=nls_london_radio_button.Caption;
                             2: current_map_label.Caption:=nls_6inch_radio_button.Caption;
                          end;//case

                          case server of
                             0,1,2: begin                                                   // tiled maps ...
                                      extend_area_groupbox.Visible:=True;
                                      crop_area_groupbox.Visible:=True;
                                    end;

                               else begin                                                   // screenshot maps  (should not get here)
                                      extend_area_groupbox.Visible:=False;
                                      crop_area_groupbox.Visible:=False;
                                    end;
                          end;//case

                        end;//with

                        BREAK;
                      end;
              end;//next
            end;//with

          end
     else begin

            map_loader_form.current_map_label.Caption:='';

            map_loader_form.load_button.Tag:=0;

            if (map_loader_form.osm_radio_button.Checked=True)
            or (map_loader_form.nls_london_radio_button.Checked=True)
            or (map_loader_form.nls_6inch_radio_button.Checked=True)    // tiled maps
               then begin
                      map_loader_form.load_button.Caption:='load  tiled  map';

                      map_loader_form.at_zero_checkbox.Visible:=False;
                      map_loader_form.show_loading_checkbox.Visible:=True;
                    end
               else begin
                      if map_loader_form.load_button.Enabled=True
                         then map_loader_form.load_button.Caption:='show  map';    // screenshot maps

                      map_loader_form.show_loading_checkbox.Visible:=False;
                      map_loader_form.at_zero_checkbox.Visible:=True;
                    end;

            map_loader_form.load_new_label.Visible:=False;

            map_loader_form.delete_button.Enabled:=False;

            map_loader_form.extend_area_groupbox.Visible:=False;
            map_loader_form.crop_area_groupbox.Visible:=False;

            map_loader_form.zoom_edit.Visible:=True;
            map_loader_form.zoom_label.Visible:=True;

          end;

  with map_loader_form do begin
    Label3.Font.color:=clBlue;
    Label4.Font.color:=clBlue;
    Label5.Font.color:=clBlue;
    Label6.Font.color:=clBlue;
    Label14.Font.color:=clBlue;

    Label23.Font.color:=clBlue;
    Label24.Font.color:=clBlue;

    info_label.Font.color:=clBlue;
  end;//with

       // fill list with existing maps ...

  map_loader_form.name_listbox.Items.Clear;

  try
    with bgnd_form.bgnd_shapes_listbox.Items do begin
      if Count<1 then EXIT;   // no shapes ???

      for n:=0 to Count-1 do begin

        if Pos('map::',Strings[n])=1
           then begin
                  found_str:=Strings[n];
                  list_str:=Trim(Copy(found_str,6,15));

                  if map_loader_form.name_listbox.Items.IndexOf(list_str)=-1
                     then map_loader_form.name_listbox.Items.Add(list_str);
                end;
      end;//next
    end;//with

  finally
    map_loader_form.name_listbox.Visible:=(map_loader_form.name_listbox.Items.Count>0);
    map_loader_form.list_label.Visible:=map_loader_form.name_listbox.Visible;

    map_loader_form.location_groupbox.Visible:=(map_loader_form.other_screenshot_url_radio_button.Checked=False) or (map_loader_form.use_location_checkbox.Checked=True);

    map_loader_form.zoom_edit.Enabled:=map_loader_form.location_groupbox.Visible;

  end;//try
end;
//______________________________________________________________________________

procedure Tmap_loader_form.name_editChange(Sender: TObject);

begin
  highlight_label.Visible:=False;
  highlight_shape.Visible:=False;

  map_loader_form_update;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.delete_buttonClick(Sender: TObject);

var
  n:integer;
  bgnd_shapes_text_str,name_str,shape_name_str:string;

begin
  bgnd_shapes_text_str:=bgnd_form.bgnd_shapes_listbox.Items.Text;

  name_str:=format_his_name;

  shape_name_str:='map::'+name_str;

  if Pos(shape_name_str,bgnd_shapes_text_str)<>0
     then begin
            if alert(7,'    delete  map  '+name_str,
                       '      '+name_str
                      +'||You are about to delete this map from the background shapes.'
                      +'||It is not possible to restore a map which has been deleted.'
                      +'||Are you sure you want to delete this map ?',
                       '','','','yes  -  delete  map','no  -  cancel','',0)=5 then EXIT;
          end
     else begin
            ShowMessage('Error: map '+name_str+' not found.');
            EXIT;
          end;

  try
    with bgnd_form.bgnd_shapes_listbox do begin
      with Items do begin
        if Count<1 then EXIT;

        n:=0;   // init

        repeat

          if Pos(shape_name_str,Strings[n])<>0
             then begin
                    free_shape_object(n);  // free any picture bitmaps and the shape object.
                    Delete(n);             // delete the entry.
                  end
             else INC(n);

        until n>(Count-1);

        shapes_saved:=(Count<1);   // need a fresh save unless all gone.

      end;//with

      ItemIndex:=0;
    end;//with

  finally
    map_loader_form_update;

    shapes_current_state;
    do_rollback:=False;       // no need to put this change in rollback register on redraw.
    redraw(True);
  end;//try
end;
//______________________________________________________________________________

procedure Tmap_loader_form.name_listboxClick(Sender: TObject);

begin
  if name_listbox.ItemIndex>-1 then name_edit.Text:=name_listbox.Items.Strings[name_listbox.ItemIndex];
end;
//______________________________________________________________________________

function get_tile_extents(map_str:string):boolean;

var
  n:integer;
  shape_str:string;

  xtile,ytile:integer;

  xmin,xmax,ymin,ymax:integer;

begin
  RESULT:=False;  // init

  xmin:=1000000000;
  ymin:=1000000000;

  xmax:=0-1000000000;
  ymax:=0-1000000000;

    // init globals ...

  xtile_min_p1.x:=minfp;
  ytile_min_p1.x:=minfp;
  xtile_max_p1.x:=minfp;
  ytile_max_p1.x:=minfp;
  xtile_min_p2.x:=minfp;
  ytile_min_p2.x:=minfp;
  xtile_max_p2.x:=minfp;
  ytile_max_p2.x:=minfp;

  xtile_min_p1.y:=minfp;
  ytile_min_p1.y:=minfp;
  xtile_max_p1.y:=minfp;
  ytile_max_p1.y:=minfp;
  xtile_min_p2.y:=minfp;
  ytile_min_p2.y:=minfp;
  xtile_max_p2.y:=minfp;
  ytile_max_p2.y:=minfp;

  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if Count<1 then EXIT;   // no shapes ???

    for n:=0 to Count-1 do begin

      if Pos(map_str,Strings[n])=1
         then begin
                shape_str:=Strings[n];
                if Length(shape_str)<>46 then CONTINUE;      // not a map tile  (label, copyright, rectangle)

                xtile:=StrToInt(Trim(Copy(shape_str,26,10)));
                ytile:=StrToInt(Trim(Copy(shape_str,37,10)));

                if xmax<xtile
                   then begin
                          xmax:=xtile;                                        // update tile limits
                          xtile_max_p1:=Tbgshape(Objects[n]).bgnd_shape.p1;   // and update globals
                          xtile_max_p2:=Tbgshape(Objects[n]).bgnd_shape.p2;
                        end;

                if xmin>xtile
                   then begin
                          xmin:=xtile;
                          xtile_min_p1:=Tbgshape(Objects[n]).bgnd_shape.p1;
                          xtile_min_p2:=Tbgshape(Objects[n]).bgnd_shape.p2;
                        end;

                if ymax<ytile
                   then begin
                          ymax:=ytile;
                          ytile_max_p1:=Tbgshape(Objects[n]).bgnd_shape.p1;
                          ytile_max_p2:=Tbgshape(Objects[n]).bgnd_shape.p2;
                        end;

                if ymin>ytile
                   then begin
                          ymin:=ytile;
                          ytile_min_p1:=Tbgshape(Objects[n]).bgnd_shape.p1;
                          ytile_min_p2:=Tbgshape(Objects[n]).bgnd_shape.p2;
                        end;
              end;

    end;//next

  end;//with

  if (xmax>=xmin) and (ymax>=ymin)    // set global tile limits ...

     and (xtile_min_p1.x<>minfp)
     and (ytile_min_p1.x<>minfp)
     and (xtile_max_p1.x<>minfp)
     and (ytile_max_p1.x<>minfp)
     and (xtile_min_p2.x<>minfp)
     and (ytile_min_p2.x<>minfp)
     and (xtile_max_p2.x<>minfp)
     and (ytile_max_p2.x<>minfp)

     and (xtile_min_p1.y<>minfp)
     and (ytile_min_p1.y<>minfp)
     and (xtile_max_p1.y<>minfp)
     and (ytile_max_p1.y<>minfp)
     and (xtile_min_p2.y<>minfp)
     and (ytile_min_p2.y<>minfp)
     and (xtile_max_p2.y<>minfp)
     and (ytile_max_p2.y<>minfp)

     then begin
            xtile_min:=xmin;
            ytile_min:=ymin;

            xtile_max:=xmax;
            ytile_max:=ymax;

            RESULT:=True;
          end;
end;
//______________________________________________________________________________

procedure add_row(top_row:boolean);

var
  n:integer;
  server_code,zoom_level:integer;
  numx,numy,mapx:integer;
  xtile_org:integer;

  shape_str:string;
  xtile_str,ytile_str,zoom_str:string;

begin
  map_name_str:='map::'+format_his_name;    // global

  server_code:=-1; // init
  zoom_level:=-1;  // init;

  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if Count<1 then EXIT;   // no shapes ???

    for n:=0 to Count-1 do begin

      if Pos(map_name_str,Strings[n])=1
         then begin
                shape_str:=Strings[n];
                if Length(shape_str)<>46 then CONTINUE;      // not a map tile  (label, copyright, rectangle, screenshot)

                server_code:=StrToInt(shape_str[21]);
                zoom_level:=StrToInt(Trim(Copy(shape_str,23,2)));

                BREAK;
              end;
    end;//next
  end;//with

  if (get_tile_extents(map_name_str)=False) or (server_code=-1) or (zoom_level=-1)
     then begin
            ShowMessage('error - Sorry, unable to identify all map tiles.');
            EXIT;
          end;

  Screen.Cursor:=crHourGlass;

  map_tile_size_mm:=ABS(ytile_min_p2.x-ytile_min_p1.x);

  map_org_x:=xtile_min_p1.x;         // align left on pad

  if top_row=True
     then begin
            ytile_str:=IntToStr(ytile_min-1);             // y tiles count from top
            map_org_y:=ytile_min_p1.y+map_tile_size_mm;   // pad grid from bottom
          end
     else begin
            ytile_str:=IntToStr(ytile_max+1);             // bottom row
            map_org_y:=ytile_max_p1.y-map_tile_size_mm;
          end;

  map_num_tiles_y:=1;                // 1 row to add
  map_tile_count_y:=0;               // first row only

  xtile_org:=xtile_min;

  numx:=ABS(xtile_max-xtile_min)+1;

  numy:=1;                           // 1 row

  map_code:=server_code;             // global

  zoom_str:=IntToStr(zoom_level);

  try
    for mapx:=0 to numx-1 do begin   // tile column

      map_tile_count_x:=mapx;        // global

      xtile_str:=IntToStr(xtile_org+mapx);

      if obtain_tile_from_server(zoom_str,xtile_str,ytile_str)=False
         then EXIT;

      if map_loader_form.show_loading_checkbox.Checked=True then redraw(False);   // show map loading

    end;//next tile

  finally
    Screen.Cursor:=crDefault;
    redraw(True);
  end;//try

end;
//______________________________________________________________________________

procedure Tmap_loader_form.add_top_buttonClick(Sender: TObject);

begin
  add_row(True);     // True = at top
end;
//______________________________________________________________________________

procedure Tmap_loader_form.add_bottom_buttonClick(Sender: TObject);

begin
  add_row(False);    // False = at bottom
end;
//______________________________________________________________________________

procedure add_column(left_column:boolean);

var
  n:integer;
  server_code,zoom_level:integer;
  numx,numy,mapy:integer;
  ytile_org:integer;

  shape_str:string;
  xtile_str,ytile_str,zoom_str:string;

begin
  map_name_str:='map::'+format_his_name;    // global

  server_code:=-1; // init
  zoom_level:=-1;  // init;

  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if Count<1 then EXIT;   // no shapes ???

    for n:=0 to Count-1 do begin

      if Pos(map_name_str,Strings[n])=1
         then begin
                shape_str:=Strings[n];
                if Length(shape_str)<>46 then CONTINUE;      // not a map tile  (label, copyright, rectangle, screenshot)

                server_code:=StrToInt(shape_str[21]);
                zoom_level:=StrToInt(Trim(Copy(shape_str,23,2)));

                BREAK;
              end;
    end;//next
  end;//with

  if (get_tile_extents(map_name_str)=False) or (server_code=-1) or (zoom_level=-1)
     then begin
            ShowMessage('error - Sorry, unable to identify all map tiles.');
            EXIT;
          end;

  Screen.Cursor:=crHourGlass;

  map_tile_size_mm:=ABS(ytile_min_p2.x-ytile_min_p1.x);

  map_org_y:=ytile_max_p1.y;

  if left_column=True
     then begin
            xtile_str:=IntToStr(xtile_min-1);
            map_org_x:=xtile_min_p1.x-map_tile_size_mm;
          end
     else begin
            xtile_str:=IntToStr(xtile_max+1);
            map_org_x:=xtile_max_p1.x+map_tile_size_mm;
          end;

  map_num_tiles_x:=1;                // 1 column to add
  map_tile_count_x:=0;               // first column only

  numy:=ABS(ytile_max-ytile_min)+1;   // 1 column
  map_num_tiles_y:=numy;              // global

  ytile_org:=ytile_min;

  numx:=1;

  map_code:=server_code;             // global

  zoom_str:=IntToStr(zoom_level);

  try
    for mapy:=0 to numy-1 do begin   // tile column

      map_tile_count_y:=mapy;        // global

      ytile_str:=IntToStr(ytile_org+mapy);

      if obtain_tile_from_server(zoom_str,xtile_str,ytile_str)=False
         then EXIT;

      if map_loader_form.show_loading_checkbox.Checked=True then redraw(False);   // show map loading

    end;//next tile

  finally
    Screen.Cursor:=crDefault;
    redraw(True);
  end;//try

end;
//______________________________________________________________________________

procedure Tmap_loader_form.add_left_buttonClick(Sender: TObject);

begin
  add_column(True);
end;
//______________________________________________________________________________

procedure Tmap_loader_form.add_right_buttonClick(Sender: TObject);

begin
  add_column(False);
end;
//______________________________________________________________________________

procedure Tmap_loader_form.osm_radio_buttonClick(Sender: TObject);

begin
  zoom_edit.Text:='19';
  map_loader_form_update;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.nls_london_radio_buttonClick(Sender: TObject);

begin
  zoom_edit.Text:='21';
  map_loader_form_update;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.nls_6inch_radio_buttonClick(Sender: TObject);

begin
  zoom_edit.Text:='19';
  map_loader_form_update;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.screenshot_25inch_radio_buttonClick(Sender: TObject);

begin
  zoom_edit.Text:='19';
  map_loader_form_update;

  screenshot_url_edit.Font.Color:=clGray;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.screenshot_50inch_radio_buttonClick(Sender: TObject);

begin
  zoom_edit.Text:='20';
  map_loader_form_update;

  screenshot_url_edit.Font.Color:=clGray;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.screenshot_25k_radio_buttonClick(Sender: TObject);

begin
  zoom_edit.Text:='16';
  map_loader_form_update;

  screenshot_url_edit.Font.Color:=clGray;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.other_screenshot_url_radio_buttonClick(Sender: TObject);

begin
  zoom_edit.Text:='18';           // not used unless other location checkbox
  map_loader_form_update;

  screenshot_url_edit.Font.Color:=clBlue;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.stop_buttonClick(Sender: TObject);

begin
  stop_loading:=True;

  pause_button.Caption:='pause';  // reset if stopped after pause.
end;
//______________________________________________________________________________

procedure Tmap_loader_form.pause_buttonClick(Sender: TObject);

begin
  pause_loading:= NOT pause_loading;

  if pause_loading=True then pause_button.Caption:='continue'
                        else pause_button.Caption:='pause';
end;
//______________________________________________________________________________

procedure Tmap_loader_form.close_buttonClick(Sender: TObject);

begin
  Hide;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.FormActivate(Sender: TObject);

begin
  if loading_in_progress=False then map_loader_form_update;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.info_buttonClick(Sender: TObject);

begin
  info_label.Font.color:=clBlue;
  go_to_url('http://templot.com/companion/get_map_from_the_web.php');
end;
//______________________________________________________________________________

procedure Tmap_loader_form.screenshot_url_editEnter(Sender: TObject);

begin
  other_screenshot_url_radio_button.Checked:=True;
  screenshot_url_edit.Font.Color:=clBlue;
end;
//______________________________________________________________________________

procedure crop_row_or_column(which_tiles:integer);

  // which tiles  0=top row  1=right column  2=bottom row  3=left column  (clockwise)

var
  n:integer;
  server_code,zoom_level:integer;

  shape_str:string;
  xtile_str,ytile_str,crop_tile_str:string;

  pos_loc:integer;

begin
  map_name_str:='map::'+format_his_name;    // global

  server_code:=-1; // init
  zoom_level:=-1;  // init;

  with bgnd_form.bgnd_shapes_listbox.Items do begin
    if Count<1 then EXIT;   // no shapes ???

    for n:=0 to Count-1 do begin

      if Pos(map_name_str,Strings[n])=1
         then begin
                shape_str:=Strings[n];
                if Length(shape_str)<>46 then CONTINUE;      // not a map tile  (label, copyright, rectangle, screenshot)

                server_code:=StrToInt(shape_str[21]);
                zoom_level:=StrToInt(Trim(Copy(shape_str,23,2)));

                BREAK;
              end;
    end;//next
  end;//with

  if (get_tile_extents(map_name_str)=False) or (server_code=-1) or (zoom_level=-1)
     then begin
            ShowMessage('error - Sorry, unable to identify all map tiles.');
            EXIT;
          end;

  case which_tiles of

      0: begin crop_tile_str:=IntToStr(ytile_min); pos_loc:=36; end;   // top row   y tiles count from top

      1: begin crop_tile_str:=IntToStr(xtile_max); pos_loc:=25; end;   // right column

      2: begin crop_tile_str:=IntToStr(ytile_max); pos_loc:=36; end;   // bottom row

      3: begin crop_tile_str:=IntToStr(xtile_min); pos_loc:=25; end;   // left column

    else EXIT;  // ???
  end;//case

   // delete all matching tiles ...

  try

    with bgnd_form.bgnd_shapes_listbox do begin
      with Items do begin
        if Count<1 then EXIT;

        n:=0;   // init

        repeat

          if Pos('#'+crop_tile_str,Strings[n])=pos_loc  // position in shape name string
             then begin
                    free_shape_object(n);  // free the picture bitmaps and the shape object.
                    Delete(n);             // delete the entry.
                  end
             else INC(n);

        until n>(Count-1);

        shapes_saved:=(Count<1);   // need a fresh save unless all gone.

      end;//with

      ItemIndex:=0;
    end;//with

    if get_tile_extents(map_name_str)=False                // no tiles remaining?
       then begin                                          // delete the map entirely, rectangle, label
              with bgnd_form.bgnd_shapes_listbox do begin
                with Items do begin
                  if Count<1 then EXIT;

                  n:=0;   // init

                  repeat

                    if Pos(map_name_str,Strings[n])<>0
                       then begin
                              free_shape_object(n);  // free any picture bitmaps and the shape object.
                              Delete(n);             // delete the entry.
                            end
                       else INC(n);

                  until n>(Count-1);

                  shapes_saved:=(Count<1);   // need a fresh save unless all gone.

                end;//with

                ItemIndex:=0;
              end;//with
            end;

  finally
    map_loader_form_update;

    shapes_current_state;
    do_rollback:=False;       // no need to put this change in rollback register on redraw.
    redraw(True);
  end;//try
end;
//______________________________________________________________________________

procedure Tmap_loader_form.crop_top_buttonClick(Sender: TObject);

begin
  crop_row_or_column(0);
end;
//_____________________________

procedure Tmap_loader_form.crop_right_buttonClick(Sender: TObject);

begin
  crop_row_or_column(1);
end;
//_____________________________

procedure Tmap_loader_form.crop_bottom_buttonClick(Sender: TObject);

begin
  crop_row_or_column(2);
end;
//_____________________________

procedure Tmap_loader_form.crop_left_buttonClick(Sender: TObject);

begin
  crop_row_or_column(3);
end;
//______________________________________________________________________________

procedure Tmap_loader_form.use_location_checkboxClick(Sender: TObject);

begin
  map_loader_form_update;
end;
//______________________________________________________________________________
(*
function check_browser_zoom:boolean;

var
  window_handle:HWND;
  device_context:HDC;

  grab_bmp:TBitmap;

  window_rect:TRect;

  grab_width,grab_height:integer;

  i,n:integer;

  row,col:integer;

  wait_count:integer;

begin

     // test zoom setting   -   display test image, then grab it as a screenshot ...

  RESULT:=False;  // init

  web_map_help_form.Hide;
  map_loader_form.Hide;

  Screen.Cursor:=crHourGlass;

  if web_browser_form.Showing=False then web_browser_form.Show;  // calling Show resets the page zoom.

  if web_browser_form.ClientWidth<480   // allow for scrollbars
     then begin
            web_browser_form.ClientWidth:=480;
            Application.ProcessMessages;
          end;

  Screen.Cursor:=crHourGlass;

  Application.ProcessMessages;

  wait_count:=running_counter;

  Screen.Cursor:=crDefault;

  Application.ProcessMessages;   // once more

  window_handle:=GetForegroundWindow;

  Windows.GetClientRect(window_handle,window_rect);
  device_context:=GetDC(window_handle);

  grab_width:=window_rect.Right-window_rect.Left;
  grab_height:=window_rect.Bottom-window_rect.Top;

  if (grab_width<1) or (grab_height<1) then EXIT;  // ???

  grab_bmp:=TBitmap.Create;

  grab_bmp.Width:=grab_width;
  grab_bmp.Height:=grab_height;

  row:=0;  // keep compiler happy
  col:=0;

  try

    BitBlt(grab_bmp.Canvas.Handle,0,0,grab_width,grab_height,device_context,0,0,SRCCOPY);

    //ShowMessage(IntToStr(grab_width)+'  '+IntToStr(grab_height));     // debug

    //grab_bmp.SaveToFile('C:\85A\85A_TEMP\grabbed_zoom.bmp'); // debug

    for i:=0 to 16 do begin

      for n:=0 to 16 do begin

        if grab_bmp.Canvas.Pixels[n,i]=clRed      // find top row of actual image in screenshot
           then begin
                  row:=i;
                  col:=n;
                  EXIT;
                end;

      end;//next n
    end;//next i


  finally
    //ShowMessage(IntToStr(col)+'  '+IntToStr(row));   // debug

    with grab_bmp.Canvas do begin

       if (Pixels[col+396,row]<>clRed) or (Pixels[col+397,row]<>clGreen) or (Pixels[col+398,row]<>clBlue)

          then RESULT:=False
          else RESULT:=True;

    end;//with

    grab_bmp.Free;

    ReleaseDC(window_handle,device_context);

    web_browser_form.Hide;

    //map_loader_form.Show;

    Screen.Cursor:=crDefault;
  end;//try
end;
//______________________________________________________________________________
*)

procedure Tmap_loader_form.screenshot_url_editClick(Sender: TObject);

begin
  screenshot_url_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.url_editClick(Sender: TObject);

begin
  url_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.name_editClick(Sender: TObject);

begin
  highlight_label.Visible:=False;
  highlight_shape.Visible:=False;

  name_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.lon_editClick(Sender: TObject);

begin
  lon_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.lat_editClick(Sender: TObject);

begin
  lat_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.os_letters_editClick(Sender: TObject);

begin
  os_letters_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.os_easting_editClick(Sender: TObject);

begin
  os_easting_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.os_northing_editClick(Sender: TObject);

begin
  os_northing_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.zoom_editClick(Sender: TObject);

begin
  zoom_edit.SelectAll;
end;
//______________________________________________________________________________

procedure Tmap_loader_form.info_labelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  info_label.Font.color:=clRed;
end;
//______________________________________________________________________________

end.

