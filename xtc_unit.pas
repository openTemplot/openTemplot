
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

unit xtc_unit;

{$MODE Delphi}

// 218e

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  Txtc_form = class(TForm)
    connectors_groupbox: TGroupBox;
    zero_checkbox: TCheckBox;
    texit_checkbox: TCheckBox;
    length_checkbox: TCheckBox;
    ok_button: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  xtc_form: Txtc_form;

implementation

{$R *.lfm}

procedure Txtc_form.FormCreate(Sender: TObject);

begin
  AutoScroll := False;

  // OT-FIRST ClientWidth:=370;
  // OT-FIRST ClientHeight:=150;
end;
//______________________________________________________________________________

end.
