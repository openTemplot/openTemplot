
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

unit plain_track_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

type
  Tplain_track_form = class(TForm)
    ok_panel: TPanel;
    ok_button: TButton;
    plain_track_spacings_listbox: TListBox;
    cancel_panel: TPanel;
    cancel_button: TButton;
    blue_corner_panel: TPanel;
    how_panel: TPanel;
    chat_panel: TPanel;
    size_updown: TUpDown;
    colour_panel: TPanel;
    colour_patch: TImage;
    help_button: TButton;
    custom_spacings_button: TButton;
    datestamp_label: TLabel;
    Shape1: TShape;
    procedure ok_panelClick(Sender: TObject);
    procedure FormKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure colour_panelClick(Sender: TObject);
    procedure size_updownClick(Sender: TObject; Button: TUDBtnType);
    procedure custom_spacings_buttonClick(Sender: TObject);
    procedure help_buttonClick(Sender: TObject);
    procedure chat_panelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cancel_buttonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  plain_track_form: Tplain_track_form;


  procedure init_plain_track;             // fill plain-track timbering.
  procedure plain_track_spacings_click;   // show form and get data.

//__________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses pad_unit, alert_unit, control_room, entry_sheet, colour_unit, help_sheet, chat_unit,
  math_unit;

var
  plain_track_spacing_size:integer;

//______________________________________________________________________________________

procedure adopt_current(cpt:integer);

var
  n:integer;

begin
  railen[cpt]:=railen[pt_i];                               // rail length in inches.
  sleeper_count[cpt]:=sleeper_count[pt_i];                 // number of sleepers per length.
  for n:=0 to psleep_c do psleep[cpt,n]:=psleep[pt_i,n];   // spacings.

  plain_track_form.plain_track_spacings_listbox.Items.Strings[cpt]:=plain_track_form.plain_track_spacings_listbox.Items.Strings[pt_i];   // name.
end;
//______________________________________________________________________________________

function custom_plain_track:boolean;     // get custom plain track settings.

const
  spacing_help_str:string='      Custom  Rail  Lengths  and  Sleeper  Spacings'
  +'||All dimensions (including rail lengths) are entered in full-size INCHES.'
  +'||Terminate the list of sleeper (tie) spacings by entering a zero.'
  +'||Make sure that all your spacings add up to the full rail length, otherwise there will be a'
  +' discontinuity in your sleepering.'
  +'||But DO NOT enter the spacing from the final sleeper in the length to the end rail joint, this space'
  +' will appear automatically if your calculations are correct.'
  +'||It is helpful to number the sleepers and work out all the spacings on paper before entering the figures.'
  +'||The sleeper spacings are normally symmetrical about the centre of each rail length'
  +' but this does not happen automatically, ALL the spacings have to be entered and they could be non-symmetrical if desired.'
  +'||If staggered rail joints are wanted (e.g. for US and some Irish practice), click the REAL > PLAIN TRACK OPTIONS > RAIL JOINT MARKS > STAGGERED menu item. This omits alternate joints on each rail.'
  +' Then set here a "rail length" which is half of the real rail length, and enter the sleeper (tie) spacings for each half-length.'
  +'||If CWR (long-welded rail) is wanted, click the REAL > PLAIN TRACK OPTIONS > RAIL JOINT MARKS > NONE menu item.'
  +' Then set here a dummy "rail length" which is equal to the sleeper spacing, and have a single sleeper spaced at half that distance from the dummy "joint".';

  adopt_help_str:string='      Adopt  Control  Template  Settings  for  Rail  Lengths  and  Sleeper  Spacings'
  +'||This option is useful when a template containing custom rail lengths and sleeper spacings has been reloaded from a data file.'
  +'||Copy it to the control template, and then use this option to adopt the settings into one of the custom slots.'
  +'||The data will also be automatically entered into the lowermost slot in the list, but will not be preserved there'
  +' if a subsequent control template has different custom plain track data.'
  +'||N.B. Although adopting the control template settings serves no apparent purpose if the control template is already set to one of the existing settings in the list,'
  +' this can be a useful way of pre-setting the custom data before modifying it.';

  name_prompt_str:string=
      '||||            Custom  Plain  Track  Name'
     +'|||Enter below a reference name for this custom setting for the plain track rail lengths and sleeper spacings.';

  railen_help_str:string='      Custom  Rail  Length'
   +'||Enter your custom rail length in full-size INCHES, i.e. for 60ft rails enter 720 inches.'
   +'||If staggered rail joints are wanted (e.g. for US and some Irish practice), click the REAL > PLAIN TRACK OPTIONS > RAIL JOINT MARKS > STAGGERED menu item. This omits alternate joints on each rail.'
   +' Then set here a "rail length" which is half of the real rail length, and enter the sleeper (tie) spacings for each half-length.'
   +'||If CWR (long-welded rail) is wanted, click the REAL > PLAIN TRACK OPTIONS > RAIL JOINT MARKS > NONE menu item.'
   +' Then set here a dummy "rail length" which is equal to the sleeper spacing, and have a single sleeper spaced at half that distance from the dummy "joint".';

var
  cpt:integer;
  i,m,n:integer;
  od:Toutdim;
  custom_count:integer;
  str:string;

begin
  RESULT:=False;         // init default result.

  with plain_track_form do begin

    cpt:=plain_track_spacings_listbox.ItemIndex;   // list index for custom plain track slot.

    try
      if cpt>10
         then begin
                alert(6,'    control  template  custom  slot  selected',
                        'The lowermost slot in the list is reserved for the plain track settings from the most recent control template which contained custom plain track data.'
                       +'||The settings in this slot are maintained automatically by Templot, it is not possible to change them manually.'
                       +'||You can select these custom settings for the control template if it is currently set to one of the standard plain track settings.',
                        '','','','','','O K',0);
                EXIT;
              end;

      if cpt<5
         then begin
                alert(6,'    no  custom  slot  selected',
                        'Please click one of the custom slots in the list to select the one (1-6) for which you wish to enter custom settings.'
                       +'||It is not possible to change the settings for the standard slots.',
                        '','','','','','O K',0);
                EXIT;
              end;

      str:=Chr(cpt+44);   // 5 = '1', etc.

      if railen[cpt]<>0
         then begin
                i:=alert(4,'    existing  custom  setting  ( slot  '+str+' )',
                           '||You have an existing custom plain track setting in this slot.'
                          +'||You can clear the existing data before entering new, or modify it as required.',
                           '','','','clear  existing  custom  data  in  slot  '+str,'cancel  -  no  change','modify  existing  custom  data  in  slot  '+str,0);
                if i=4 then railen[cpt]:=0;
                if i=5 then EXIT;
              end;

      repeat
        i:=alert(4,'    adopt  control  template  settings ?',
                   'Do you want to adopt the plain track rail length and sleeper spacings from the control template as a custom setting?'
                  +'||Or enter new data?',
                   '','','?  help','adopt  control  template  data  for  slot  '+str,'cancel  -  no  change','enter  new  data  for  slot  '+str,3);

        if i=3 then alert_help(0,adopt_help_str,'');
      until i<>3;

      if i=4
         then begin
                adopt_current(cpt);
                RESULT:=True;
                EXIT;
              end;

      if i=5 then EXIT;

      with math_form do begin
        Caption:='    custom  plain  track  name ...';
        big_label.Caption:=insert_crlf_str(name_prompt_str);
        math_editbox.Text:=plain_track_spacings_listbox.Items.Strings[cpt];

        do_show_modal(math_form);     // 212a  ShowModal

        if (ModalResult=mrOK) and (Trim(math_editbox.Text)<>'') then plain_track_spacings_listbox.Items.Strings[cpt]:='  '+Trim(math_editbox.Text);

        Caption:='    '+Application.Title;   // reset form caption.
      end;//with

         putdim(railen_help_str,2,'custom rail length in full-size INCHES',railen[cpt],True,False,True,False);      // no neg, preset ok, no zero, don't terminate on zero.
      n:=putdim('The first spacing dimension required is from the rail joint to the centre of the first sleeper. For bullhead standard gauge this dimension is normally 12 inches.',
                     2,'spacing from rail joint to first sleeper',psleep[cpt,0],True,False,False,False);  // no neg, preset ok, zero ok, but don't terminate on zero.

      if n<>1 then run_error(210);
      if getdims('custom  plain  track  ( slot  '+str+' )',spacing_help_str,plain_track_form,n,od)=True
         then begin
                if od[0]=def_req then railen[cpt]:=720            // 60ft rails default.
                                 else railen[cpt]:=ABS(od[0]);

                if od[1]=def_req then psleep[cpt,0]:=12           // 12" default.
                                 else psleep[cpt,0]:=ABS(od[1]);

                i:=1;        // starting point for next 8 spacings.
                repeat

                    for m:=0 to 7 do begin

                      n:=putdim('Terminate this list of spacings by entering a zero.',
                                2,'spacing to next sleeper : number '+IntToStr(i+m+1),psleep[cpt,(i+m)],True,True,False,True);  // zero ok - terminates the list.
                    end;//for

                    if n<>7 then run_error(212);

                    custom_count:=i;

                    if getdims('custom  sleeper  spacings',spacing_help_str,plain_track_form,n,od)=True
                       then begin
                              if psleep[cpt,(i-1)]<>0 then begin psleep[cpt,i  ]:=ABS(od[0]);INC(custom_count); end else BREAK;
                              if psleep[cpt, i   ]<>0 then begin psleep[cpt,i+1]:=ABS(od[1]);INC(custom_count); end else BREAK;
                              if psleep[cpt,(i+1)]<>0 then begin psleep[cpt,i+2]:=ABS(od[2]);INC(custom_count); end else BREAK;
                              if psleep[cpt,(i+2)]<>0 then begin psleep[cpt,i+3]:=ABS(od[3]);INC(custom_count); end else BREAK;
                              if psleep[cpt,(i+3)]<>0 then begin psleep[cpt,i+4]:=ABS(od[4]);INC(custom_count); end else BREAK;
                              if psleep[cpt,(i+4)]<>0 then begin psleep[cpt,i+5]:=ABS(od[5]);INC(custom_count); end else BREAK;
                              if psleep[cpt,(i+5)]<>0 then begin psleep[cpt,i+6]:=ABS(od[6]);INC(custom_count); end else BREAK;
                              if psleep[cpt,(i+6)]<>0 then begin psleep[cpt,i+7]:=ABS(od[7]);INC(custom_count); end else BREAK;
                            end
                       else begin
                              psleep[cpt,i]:=0;    // cancelled form, so terminate list right here.
                              BREAK;
                            end;
                  INC(i,8);
                until i>(psleep_c-8);     // list full (no space for another 8 plus end marker).

                psleep[cpt,psleep_c]:=0;               // ensure there is always an end marker.
                sleeper_count[cpt]:=custom_count-1;    // number of sleepers per length (was incremented on final zero entry).
              end
         else EXIT;
      RESULT:=True;
    finally
      plain_track_spacings_listbox.ItemIndex:=cpt;  // ?? Delphi bug - gets changed when strings modified?
      plain_track_spacing_size:=cpt;
      if Showing=True then ok_button.SetFocus;
    end;//try

  end;//with form
end;
//________________________________________________________________________________________

procedure plain_track_spacings_click;   // clicked on the pad menu.

var
  pt_ok:boolean;
  str:string;

begin
  pt_ok:=False;                             //  get new PLAIN TRACK

  repeat
    plain_track_spacing_size:=pt_i;

    do_show_modal(plain_track_form);     // 212a  ShowModal         // get new plain track size.

    if railen[plain_track_spacing_size]<>0  // valid data?
                                                                                     // not custom plain track or custom data available.
       then pt_ok:=True
       else begin
              str:=Chr(plain_track_spacing_size+44);              // 5 ='1', 6 ='2', etc.
              if alert(4,'    new  custom  plain  track ?  ( slot  '+str+' )',
                         'You have selected one of the user-defined custom plain track settings,'
                        +' but you have not yet entered any custom rail length or sleeper spacing data for this slot.'
                        +'||Do you want to do this now?',
                         '','','','','cancel','O K  -  enter  custom  plain  track  data  for  slot  '+str,0)=5 then EXIT;

              if custom_plain_track=True        // no custom data, so get some.
                 then begin
                        if railen[plain_track_spacing_size]<>0   // produced valid data?
                           then pt_i:=plain_track_spacing_size;  // change plain track setting, and show form again.
                      end;
            end;
  until pt_ok=True;

  pt_i:=plain_track_spacing_size;   // change plain track setting.
end;
//__________________________________________________________________________________________

procedure Tplain_track_form.ok_panelClick(Sender: TObject);

begin
  plain_track_spacing_size:=plain_track_spacings_listbox.ItemIndex;
  ModalResult:=mrOk;                  // not set at design time, as panel click comes here.
end;
//_________________________________________________________________________________________

procedure Tplain_track_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key=VK_F10
     then begin
            Key:=0;      //  otherwise selects the menus.
          end;

  if Key=VK_PAUSE then Application.Minimize;    //  hide TEMPLOT on PAUSE key.
end;
//__________________________________________________________________________________________

procedure Tplain_track_form.FormShow(Sender: TObject);

begin
  plain_track_spacings_listbox.ItemIndex:=plain_track_spacing_size;
  plain_track_spacings_listbox.SetFocus;
end;
//________________________________________________________________________________________

procedure Tplain_track_form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin
  plain_track_spacings_listbox.ItemIndex:=plain_track_spacing_size;
end;
//________________________________________________________________________________________

procedure Tplain_track_form.colour_panelClick(Sender: TObject);

begin
  Color:=get_colour('choose  a  new  colour  for  the  plain  track  dialog',Color);
end;
//_________________________________________________________________________________________

procedure Tplain_track_form.size_updownClick(Sender: TObject; Button: TUDBtnType);

begin
  if size_updown.Position>size_updown.Tag      // ! position goes up, size goes down.
     then ScaleBy(9,10);                       // scale the form contents down.

  if size_updown.Position<size_updown.Tag
     then ScaleBy(10,9);                       // scale the form contents up.

  ClientHeight:=VertScrollBar.Range;           // allow 4 pixel right margin.
  ClientWidth:=HorzScrollBar.Range+4;          // don't need bottom margin - datestamp label provides this.
  ClientHeight:=VertScrollBar.Range;           // do this twice, as each affects the other.

  size_updown.Tag:=size_updown.Position;       // and save for the next click.

end;
//___________________________________________________________________________________________

procedure Tplain_track_form.custom_spacings_buttonClick(Sender: TObject);

begin
  custom_plain_track;   // don't need the return value
end;
//___________________________________________________________________________________________

procedure Tplain_track_form.help_buttonClick(Sender: TObject);

const
  plain_track_help_str:string='        `0Plain  Track  Rail  Lengths`9|        `0and  Sleeper  Spacings`9'
  +'||(For information about using plain track templates in track planning, click the `0about using plain track templates`1 button below.)'

  +'||Use this dialog to select the rail lengths and sleeper spacings to be used'
  +' when generating plain track templates, and also for the approach and exit track on turnout templates.'

  +'||Select the rail length and sleeper spacings you require by clicking the list, and then click the `0OK`z button.'
  +'||Clicking the `0RESTORE`1 button will restore the previous setting.'

  +'||The following notes refer primarily to UK trackwork practice:'

  +'||The 30ft and 45ft lengths are generally more suitable for the pre-grouping era.'
  +' Longer 60ft rails became the norm for new bullhead track after the grouping of railway companies in 1923, although of'
  +' course shorter rails remained in use for many years and could be found on secondary routes well into the BR period.'

  +'||The number of sleepers per rail length was increased on sharp curves.'
  +'| For 60ft rails the rules are these:'
  +'|Over 40 chains radius and straight track : 24 sleepers per length.'
  +'|40-20 chains radius : 25 sleepers per length.'
  +'|under 20 chains radius : 26 sleepers per length.'

  +'||green_panel_begintree.gif 20 chains radius is over 5000 mm (17ft) in 4mm scale and over 9000 mm (30ft) in 7mm scale,'
  +' so clearly most model curves should use the 26 sleeper spacing to be correct.'
  +'||In most cases, however, we are trying to disguise the sharpness of our curves, so a wider spacing will look more in keeping.'
  +' 25 sleepers per length is a good compromise for most model track.green_panel_end'

  +'|The sleepers are not equally spaced along the rail length. Those nearer the rail joints are progressively closed up to strengthen the joints.'

  +'||If preferred you can enter custom rail lengths and sleeper spacings. Select one of the 6 custom slots in the list, then click the `0CUSTOM SETTINGS...`1 button and read the help notes for more information.'

  +'||You can swap between your custom settings and the standard settings without losing the custom dimensions.'
  +' To save these custom dimensions in a template data file, ensure that there is at least one template in the storage box which uses them.'

  +'||For staggered rail joints (US practice) and CWR (long-welded rail), use a custom setting and read the help notes.'

  +'||The bottom slot in the list is reserved for the plain track settings from the most recent control template which contained custom plain track data.'
  +' The settings in this slot are maintained automatically by Templot, it is not possible to change them manually.'
  +' You can select these custom settings for the control template if it is currently set to one of the standard plain track settings.'

  +'||This dialog is concerned only with the rail lengths and sleeper spacings. To change the sleeper size, click the `0REAL > TIMBERING > TIMBERING DATA...`1 menu item.'
  +'||To change the position of the rail joints (plain track only) click the `0REAL > PLAIN TRACK OPTIONS > ROLL RAILS AND SLEEPERS BY...`1 menu item.';

begin
  if help(0,plain_track_help_str,'about  using  plain  track  templates')=1
     then if help(0,pt_templates_help_str,'about  plain  track  settings')=1 then help_button.Click;
end;
//____________________________________________________________________________________________

procedure Tplain_track_form.chat_panelClick(Sender: TObject);

const
  chat_str:string='    Plain Track Chat'
  +'||The pre-grouping railways used a wide variety of rail lengths and sleeper spacings, and sometimes wider sleepers next to the joint,'
  +' or a single joint sleeper with a combined chair/fishplate. I would like to include some of these in the list, but accurate spacing info is'
  +' hard to come by.'
  +'||If you are using custom spacings based on prototype information, I would be very pleased to learn the details,'
  +' so that I can offer a wider choice of pre-set options on this form.';

begin
  chat(chat_str);
end;
//_______________________________________________________________________________________

procedure init_plain_track;     // fill plain-track timbering.

var
  i,n:integer;

begin

  for n:=0 to railen_c do begin    // init clear all data...

    railen[n]:=0;           // flag no data here.
    sleeper_count[n]:=0;    // number of sleepers per length.

    for i:=0 to psleep_c do psleep[n,i]:=0;   // sleeper spacings.

  end;//next n

     // centre-centre dims in full size inches starting from rail-joint...

  n:=0;      // 30 ft rail length / 13 sleepers...

  railen[n]:=360;          // 30 ft rails
  sleeper_count[n]:=13;    // number of sleepers per length.

  psleep[n,0]:=12;          // 1
  psleep[n,1]:=26.5;        // 2
  psleep[n,2]:=27.5;        // 3
  psleep[n,3]:=28.5;        // 4
  psleep[n,4]:=28.5;        // 5
  psleep[n,5]:=28.5;        // 6
  psleep[n,6]:=28.5;        // 7
  psleep[n,7]:=28.5;        // 8
  psleep[n,8]:=28.5;        // 9
  psleep[n,9]:=28.5;        // 10
  psleep[n,10]:=28.5;       // 11
  psleep[n,11]:=27.5;       // 12
  psleep[n,12]:=26.5;       // 13
  psleep[n,13]:=0;

  //-----------------------------

  n:=1;      // 45 ft rail length / 19 sleepers...

  railen[n]:=540;          // 45 ft rails
  sleeper_count[n]:=19;    // number of sleepers per length.

  psleep[n,0]:=12;          // 1
  psleep[n,1]:=26;          // 2
  psleep[n,2]:=27;          // 3
  psleep[n,3]:=28;          // 4
  psleep[n,4]:=29.5;        // 5
  psleep[n,5]:=29.5;        // 6
  psleep[n,6]:=29.5;        // 7
  psleep[n,7]:=29.5;        // 8
  psleep[n,8]:=29.5;        // 9
  psleep[n,9]:=29.5;        // 10
  psleep[n,10]:=29.5;       // 11
  psleep[n,11]:=29.5;       // 12
  psleep[n,12]:=29.5;       // 13
  psleep[n,13]:=29.5;       // 14
  psleep[n,14]:=29.5;       // 15
  psleep[n,15]:=29.5;       // 16
  psleep[n,16]:=28;         // 17
  psleep[n,17]:=27;         // 18
  psleep[n,18]:=26;         // 19
  psleep[n,19]:=0;

  //------------------------------

  n:=2;      // 60 ft rail length / 24 sleepers...

  railen[n]:=720;          // 60 ft rails
  sleeper_count[n]:=24;    // number of sleepers per length.

  psleep[n,0]:=12;        // 1
  psleep[n,1]:=27.5;      // 2
  psleep[n,2]:=28;        // 3
  psleep[n,3]:=29;        // 4
  psleep[n,4]:=31;        // 5
  psleep[n,5]:=31;        // 6
  psleep[n,6]:=31;        // 7
  psleep[n,7]:=31;        // 8
  psleep[n,8]:=31;        // 9
  psleep[n,9]:=31;        // 10
  psleep[n,10]:=31;       // 11
  psleep[n,11]:=31;       // 12
  psleep[n,12]:=31;       // 13
  psleep[n,13]:=31;       // 14
  psleep[n,14]:=31;       // 15
  psleep[n,15]:=31;       // 16
  psleep[n,16]:=31;       // 17
  psleep[n,17]:=31;       // 18
  psleep[n,18]:=31;       // 19
  psleep[n,19]:=31;       // 20
  psleep[n,20]:=31;       // 21
  psleep[n,21]:=29;       // 22
  psleep[n,22]:=28;       // 23
  psleep[n,23]:=27.5;     // 24

  psleep[n,24]:=0;

  //-----------------------------

  n:=3;      // 60 ft rail length / 25 sleepers...

  railen[n]:=720;          // 60 ft rails
  sleeper_count[n]:=25;    // number of sleepers per length.

  psleep[n,0]:=12;          // 1
  psleep[n,1]:=26.5;        // 2
  psleep[n,2]:=27.5;        // 3
  psleep[n,3]:=28.5;        // 4
  psleep[n,4]:=29.5;        // 5
  psleep[n,5]:=29.5;        // 6
  psleep[n,6]:=29.5;        // 7
  psleep[n,7]:=29.5;        // 8
  psleep[n,8]:=29.5;        // 9
  psleep[n,9]:=29.5;        // 10
  psleep[n,10]:=29.5;       // 11
  psleep[n,11]:=29.5;       // 12
  psleep[n,12]:=29.5;       // 13
  psleep[n,13]:=29.5;       // 14
  psleep[n,14]:=29.5;       // 15
  psleep[n,15]:=29.5;       // 16
  psleep[n,16]:=29.5;       // 17
  psleep[n,17]:=29.5;       // 18
  psleep[n,18]:=29.5;       // 19
  psleep[n,19]:=29.5;       // 20
  psleep[n,20]:=29.5;       // 21
  psleep[n,21]:=29.5;       // 22
  psleep[n,22]:=28.5;       // 23
  psleep[n,23]:=27.5;       // 24
  psleep[n,24]:=26.5;       // 25

  psleep[n,25]:=0;

  //---------------------------

  n:=4;      // 60 ft rail length / 26 sleepers...

  railen[n]:=720;          // 60 ft rails
  sleeper_count[n]:=26;    // number of sleepers per length.

  psleep[n,0]:=12;        // 1
  psleep[n,1]:=26.5;      // 2
  psleep[n,2]:=27.5;      // 3
  psleep[n,3]:=28;        // 4
  psleep[n,4]:=28;        // 5
  psleep[n,5]:=28;        // 6
  psleep[n,6]:=28;        // 7
  psleep[n,7]:=28;        // 8
  psleep[n,8]:=28;        // 9
  psleep[n,9]:=28;        // 10
  psleep[n,10]:=28;       // 11
  psleep[n,11]:=28;       // 12
  psleep[n,12]:=28;       // 13
  psleep[n,13]:=28;       // 14
  psleep[n,14]:=28;       // 15
  psleep[n,15]:=28;       // 16
  psleep[n,16]:=28;       // 17
  psleep[n,17]:=28;       // 18
  psleep[n,18]:=28;       // 19
  psleep[n,19]:=28;       // 20
  psleep[n,20]:=28;       // 21
  psleep[n,21]:=28;       // 22
  psleep[n,22]:=28;       // 23
  psleep[n,23]:=28;       // 24
  psleep[n,24]:=27.5;     // 25
  psleep[n,25]:=26.5;     // 26

  psleep[n,26]:=0;

  //---------------------------

  n:=plain_track_form.plain_track_spacings_listbox.Items.Count-1;

        // use 45 ft rail length / 19 sleepers as initial default for the current custom slot...

  railen[n]:=540;          // 45 ft rails
  sleeper_count[n]:=19;    // number of sleepers per length.

  psleep[n,0]:=12;          // 1
  psleep[n,1]:=26;          // 2
  psleep[n,2]:=27;          // 3
  psleep[n,3]:=28;          // 4
  psleep[n,4]:=29.5;        // 5
  psleep[n,5]:=29.5;        // 6
  psleep[n,6]:=29.5;        // 7
  psleep[n,7]:=29.5;        // 8
  psleep[n,8]:=29.5;        // 9
  psleep[n,9]:=29.5;        // 10
  psleep[n,10]:=29.5;       // 11
  psleep[n,11]:=29.5;       // 12
  psleep[n,12]:=29.5;       // 13
  psleep[n,13]:=29.5;       // 14
  psleep[n,14]:=29.5;       // 15
  psleep[n,15]:=29.5;       // 16
  psleep[n,16]:=28;         // 17
  psleep[n,17]:=27;         // 18
  psleep[n,18]:=26;         // 19
  psleep[n,19]:=0;

  //---------------------------

end;
//______________________________________________________________________________________

procedure Tplain_track_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=680;
  // OT-FIRST ClientHeight:=260;
  AutoScroll:=True;
end;
//______________________________________________________________________________

procedure Tplain_track_form.cancel_buttonClick(Sender: TObject);

begin
  plain_track_spacings_listbox.ItemIndex:=plain_track_spacing_size;
  Modalresult:=mrCancel;
end;
//______________________________________________________________________________

end.

