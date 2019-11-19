
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

unit edit_memo_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  Tedit_memo_form = class(TForm)
    font_button: TButton;
    ok_panel: TPanel;
    ok_button: TButton;
    Label1: TLabel;
    vis_edit_memo: TMemo;
    datestamp_label: TLabel;
    cancel_button: TButton;
    overwrite_label: TLabel;
    procedure font_buttonClick(Sender: TObject);
    procedure vis_edit_memoKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ok_panelClick(Sender: TObject);
    procedure overwrite_labelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  edit_memo_form: Tedit_memo_form;

implementation

{$BOOLEVAL ON}


uses
  control_room, keep_select, colour_unit, math_unit, info_unit;

{$R *.lfm}

//______________________________________________________________________________

var

  form_overwrite_mode:boolean=False;  // 214a

procedure Tedit_memo_form.font_buttonClick(Sender: TObject);

begin
  vis_edit_memo.Font.Assign(get_font('choose  a  new  text  font  for  this  panel',vis_edit_memo.Font,True));
  if vis_edit_memo.Showing=True then vis_edit_memo.SetFocus;
end;
//_________________________________________________________________________________________

procedure Tedit_memo_form.vis_edit_memoKeyPress(Sender:TObject; var Key:Char);

begin
  if (Key=Chr(27)) and (Tag=0) then Close;

   // 214a  overwrite mode ...

  if (Sender is TCustomEdit) and (form_overwrite_mode=True)
     then begin
            with TCustomEdit(Sender) do begin
              if (SelLength=0) and (SelStart<Length(Text))
                 then begin
                        case Key of
                          ' '..#126, #128..#255: SelLength:=1;    // select next character
                        end;//case
                      end;
            end;//with
          end;

end;
//_____________________________________________________________________________________

procedure Tedit_memo_form.FormCreate(Sender: TObject);

begin
  // OT-FIRST ClientWidth:=740;
  // OT-FIRST ClientHeight:=380;
  AutoScroll:=False;
end;
//____________________________________________________________________________________

procedure Tedit_memo_form.FormShow(Sender: TObject);

begin
  if vis_edit_memo.Visible=True then vis_edit_memo.SetFocus;
end;
//______________________________________________________________________________________

procedure Tedit_memo_form.FormResize(Sender: TObject);

begin
  ok_panel.Left:=ClientWidth-ok_panel.Width-8;
  cancel_button.Left:=ok_panel.Left-cancel_button.Width-10;

  vis_edit_memo.Height:=ClientHeight-vis_edit_memo.Top-datestamp_label.Height;
  vis_edit_memo.Width:=ClientWidth-vis_edit_memo.Left*2;
end;
//_________________________________________________________________________

procedure Tedit_memo_form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key=VK_F10
     then begin
            Key:=0;      //  otherwise selects the menus.
          end;

  if Key=VK_PAUSE then Application.Minimize;         //  hide TEMPLOT on PAUSE key.

  if Key=VK_INSERT      // 214a
     then begin
            form_overwrite_mode:= NOT form_overwrite_mode;
            if form_overwrite_mode=True
               then overwrite_label.Caption:='OVR'
               else overwrite_label.Caption:='INS';

            Key:=0;
          end;

end;
//__________________________________________________________________________________________

procedure Tedit_memo_form.ok_panelClick(Sender: TObject);

begin
  edit_memo_form.ModalResult:=mrOk;
end;
//______________________________________________________________________________

procedure Tedit_memo_form.overwrite_labelClick(Sender:TObject);  // 214a

begin
  form_overwrite_mode:= NOT form_overwrite_mode;
  if form_overwrite_mode=True
     then overwrite_label.Caption:='OVR'
     else overwrite_label.Caption:='INS';
end;
//______________________________________________________________________________

end.

