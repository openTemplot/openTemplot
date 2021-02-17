
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

unit enter_timber;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  Tenter_timber_form = class(TForm)
    Label1: TLabel;
    ok_button: TButton;
    shove_combo: TComboBox;
    Label2: TLabel;
    datestamp_label: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ok_buttonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  enter_timber_form: Tenter_timber_form;

//---------------

  function timb_num_strip(in_str:string):string;   // remove extraneous characters from a timber number string.

//__________________________________________________________________________________________

implementation

{$BOOLEVAL ON}


{$R *.lfm}

uses control_room, pad_unit, math_unit, shove_timber, alert_unit;

//___________________________________________________________________________________________

procedure Tenter_timber_form.FormActivate(Sender: TObject);

var
  n:integer;

begin                          // fill dropdown with shoved timbers.
  shove_combo.Items.Clear;

  if current_shove_list.Count>0
     then begin
            for n:=0 to current_shove_list.Count-1 do shove_combo.Items.Add(current_shove_list.Strings[n]);
          end;

  if current_shove_str=''
     then begin
            if plain_track=False then shove_combo.Text:='J2'
                                 else shove_combo.Text:='A1';
          end
     else shove_combo.Text:=current_shove_str;

  shove_combo.SetFocus;
end;
//_________________________________________________________________________________________

procedure Tenter_timber_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=420;
  // OT-FIRST ClientHeight:=144;
  AutoScroll:=False;
end;
//________________________________________________________________________________________

function select_entered:boolean;   // returns true if entered timber exists in template.

var
  i,n:integer;
  entered_str:string;

  code:integer;
  ptr_1st:^Tmark;         // pointer to a Tmark record..
  markmax:integer;
  num_str,tbnum_str:string;

begin
  RESULT:=False;      // init.

  entered_str:=timb_num_strip(UpperCase(enter_timber_form.shove_combo.Text));

  if Length(entered_str)<2
     then begin
            alert(6,'    invalid  timber  number',
                    enter_timber_form.shove_combo.Text+'  is not a valid timber number.',
                    '','','','','cancel','',0);

            if plain_track=False then enter_timber_form.shove_combo.Text:='J2'
                                 else enter_timber_form.shove_combo.Text:='A1';
            EXIT;
          end;

  enter_timber_form.shove_combo.Text:=entered_str;

      // check entered number string is in control template marks list...

  if marks_list_ptr=nil then EXIT;        // pointer to marks list not valid.

  markmax:=intarray_max(marks_list_ptr);  // max index for the present list.

  if mark_index>markmax then mark_index:=markmax;  // ??? shouldn't be.

  tbnum_str:=timb_numbers_str;      // the full string of timber numbering for the control template.

  for i:=0 to (mark_index-1) do begin     // (mark_index is always the next free slot)
    try
      ptr_1st:=Pointer(intarray_get(marks_list_ptr,i));  // pointer to the next Tmark record.
      if ptr_1st=nil then EXIT;

      code:=ptr_1st^.code;

      if code<>99 then CONTINUE;   // we are only looking for timber number entries.

      num_str:=timb_num_strip(extract_tbnumber_str(tbnum_str));   // get next timber numbering string from the acummulated string.

      if num_str=entered_str   // timber exists...
         then begin
                current_shove_str:=entered_str;

                n:=find_shove(current_shove_str,True);     // find it or create an empty slot.
                if n>=0                                    // valid slot.
                   then begin
                          with Tshoved_timber(current_shove_list.Objects[n]).shove_data do begin
                            if sv_code=0              // new slot.
                               then begin
                                      sv_code:=1;                                        // flag to shove this timber.
                                      current_shove_list.Strings[n]:=current_shove_str;  // and add it to list.
                                    end;
                            shove_buttons(True,sv_code,n);
                          end;//with

                          RESULT:=True;
                        end
                   else begin
                          current_shove_str:='';         // !!! error?
                          shove_buttons(False,0,-1);
                        end;
                EXIT;         // found this timber number.
              end;
    except
      CONTINUE;
    end;//try
  end;//next i

  alert(6,'    unknown  timber',
          'Timber  '+entered_str+'  is not present in this template.',
          '','','','','','O K    ',0);

  if plain_track=False then enter_timber_form.shove_combo.Text:='J2'
                       else enter_timber_form.shove_combo.Text:='A1';

end;
//_________________________________________________________________________________________

procedure Tenter_timber_form.ok_buttonClick(Sender: TObject);

begin
  if shove_combo.Text=''
     then begin
            shove_combo.SetFocus;   // for re-entry.
            EXIT;
          end;

  if select_entered=True         // warn him if invalid.
    then begin
           Close;
           show_and_redraw(True,False);    // show it selected, no rollback to this.
         end
    else shove_combo.SetFocus;   // for re-entry.
end;
//________________________________________________________________________________________

function timb_num_strip(in_str:string):string;   // remove extraneous characters from a timber number string.

var
  out_str:string;
  c:char;

begin
  out_str:='';
  in_str:=remove_space_str(in_str);

  while Length(in_str)>0 do begin
    c:=in_str[1];
    Delete(in_str,1,1);         // extract first char.
    if (c>='A') and (c<='Z')    // find prefix letter
       then begin
              out_str:=c;   // prefix should be upper case already.
              BREAK;
            end;
  end;//while

  while Length(in_str)>0 do begin
    c:=in_str[1];
    Delete(in_str,1,1);
    if (c>='0') and (c<='9') then  out_str:=out_str+c;   // add all the numbers.
  end;//while

  RESULT:=out_str;
end;
//__________________________________________________________________________________________

procedure Tenter_timber_form.FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);

begin
  if Key=VK_F10
     then begin
            Key:=0;      //  otherwise selects the menus.
          end;

  if Key=VK_PAUSE then Application.Minimize;   //  hide TEMPLOT on PAUSE key.
end;
//_________________________________________________________________________________________

end.

