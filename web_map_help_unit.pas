
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

unit web_map_help_unit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Htmlview, StdCtrls, ExtCtrls, HtmlGlobals;

type

  { Tweb_map_help_form }

  Tweb_map_help_form = class(TForm)
    map_help_htmlview: THTMLViewer;
    close_panel: TPanel;
    close_button: TButton;
    show_checkbox: TCheckBox;
    datestamp_label: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure close_buttonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure map_help_htmlviewHotSpotClick(Sender: TObject;
      const SRC: ThtString; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  web_map_help_form: Tweb_map_help_form;

implementation

{$R *.lfm}

uses
  control_room,help_sheet;

procedure Tweb_map_help_form.FormCreate(Sender: TObject);

begin
  ClientWidth:=450;
  ClientHeight:=650;

  AutoScroll:=False;
end;
//______________________________________________________________________________

procedure Tweb_map_help_form.close_buttonClick(Sender: TObject);

begin
  Close;
end;
//______________________________________________________________________________

procedure Tweb_map_help_form.FormResize(Sender: TObject);

begin
  map_help_htmlview.Width:=Clientwidth;
  map_help_htmlview.Height:=ClientHeight-close_panel.Height*2;

  close_panel.left:=ClientWidth-close_panel.Width*5 div 4;
  close_panel.Top:=ClientHeight-close_panel.Height*3 div 2;
  show_checkbox.Top:=close_panel.top;
end;
//______________________________________________________________________________

procedure Tweb_map_help_form.FormClose(Sender: TObject;  var Action: TCloseAction);

begin
  screenshot_msg_pref:=show_checkbox.Checked;
end;
//______________________________________________________________________________

procedure Tweb_map_help_form.FormShow(Sender: TObject);

var
  browser_help_str:string;

begin
  browser_help_str:='      `0Screenshot  Maps  -  Please  Read`9'

  +'||rp.gif  Script Error showing? - click `0Yes`z, and read on ...'

  +'||<i>Please read all of this to the end. Scroll if necessary.|Resize this window if it is obscuring the map.</i>'

  +'||Templot can capture a screenshot from this map for you and insert it on the trackpad as a background picture shape at the correct size for your model scale.'

  +'||You should be seeing the map web site displayed in an embedded version of Internet Explorer (Script Error showing? - please read all of this).'

  +' How well this works may depend on which version of Windows and/or Internet Explorer is installed on your system.'
  +'||Any errors or failings are a matter between the web site and Internet Explorer and are beyond OpenTemplot''s control. You may be able to resolve them by changing the settings in Internet Explorer on your system.'

  +'||rp.gif In this version of OpenTemplot the map window is a separate application, with its own tab on the Windows bottom task bar. The trackpad will be temporarily shrunk to the left of the screen to ensure that it does not obscure the map window.'

  +'||If dragging of the map does not work on your system, click on it and then use the arrow keys on the keyboard to move the map. If the +/- zoom buttons do not work, click on the map and then roll the mouse wheel to zoom the map.'

  +'||<U>Do not use the usual browser zoom settings to zoom the map page</U>. Always click on the map and use the controls provided on the map itself. This is important to allow OpenTemplot to scale (re-size) the map correctly from Internet Explorer.'

  +'||You can control the size of the screenshot by resizing or maximizing the map window in the usual way. If you zoom a long way out you should make the window much smaller,'
  +' otherwise when resized to your model scale the screenshot may be too large to zoom and pan properly on the trackpad.'

  +'||When you are ready to capture a screenshot, close the map by clicking the <SPAN STYLE="COLOR:WHITE; BACKGROUND-COLOR:RED;">&nbsp;<B>X</B>&nbsp;</SPAN> at the top right of the map window.'
  +' You will then have options to make the screenshot, or cancel, or return to the map.'

  +'||1. Notes for <B>Google maps</B>: Ignore any warnings about Internet Explorer compatibility. Drag or zoom the map to restore the image if it is missing.'
  +'|_________________________'

  +'||2. Notes for <B>NLS Georeferenced</B> maps:'

  +'||(These maps can be identified by the presence of a slider for changing the transparency over a different base map or aerial image.)'

  +'||When the map window first appears, you may see a few messages like this:'
  +'||<img src="'+exe_str+'internal\hlp\script_error.png">'
  +'|Click the `0Yes`z button each time.'

  +'||When the map appears, click the `0Full-screen`z button at the bottom left of the map:'
  +'||<img src="'+exe_str+'internal\hlp\full_screen.png">'
  +'|This will remove the details column on the left, and allow access to the transparency slider at the top of the map to see the underlying base map or aerial imagery.'

  +'||If you can see only the background base map, this means that you have set a UK location outside the available areas covered by the foreground map. In this case, <U>do not make a screenshot</U> because the zoom setting will be wrong.'
  +' If you zoom out far enough, you should be able to see the UK areas available for the foreground map.'
  +'|_________________________'

  +'||3. Notes for <B>NLS "Find by place"</B> maps:'

  +'||(These maps can be identified by zooming fully out. You will see a single map sheet.)'

  +'||When the map appears, you <U>must</U> click the `0Link to this view`z button at the bottom left of the map:'
  +'||<img src="'+exe_str+'internal\hlp\link_view.png">'
  +'||Then zoom out and back <U>on the +/- buttons</U>, or move the map (arrow keys), to ensure that the full URL is shown at the top of the browser, and is changing as you adjust the map.'
  +'||This ensures that OpenTemplot can read the required information from the NLS URL.'
  +'|_________________________'

  +'||4. Notes for <B>OS Maps</B>:'

  +'||The "Standard" and "Aerial" map layers can be used for screenshots. Zoom out and back on the buttons, or drag the map, to ensure that the full URL is shown at the top of the browser, and is changing as you adjust the map.'
  +'||This ensures that OpenTemplot can read the required information from the OS URL.'
  +'||rp.gif The "OS Leisure Maps" layers (Landranger 50K and Explorer 25K maps) at the time of writing use different zoom settings and will not be sized correctly for screenshots.'
  +'|_________________________'

  +'||5. Notes for <B>OpenStreetMap</B>:'

  +'||OpenStreetMap "just works". Why can''t the others?'
  +'||If the map doesn''t contain the information you need, you can edit it yourself by tracing over aerial imagery or from other information. Visit openstreetmap.org/about and Sign Up.'
  +'|_________________________'

  +'|||6. If you want to explore the full range of the NLS maps it is probably better to use your regular browser -- this embedded browser is limited in its capabilities.||&nbsp;';

  map_help_htmlview.LoadFromString(convert_tagged_string_to_html(0,browser_help_str,False));

end;

procedure Tweb_map_help_form.map_help_htmlviewHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  htmlviewer_hot_spot_clicked(Sender, SRC, Handled);
end;

//______________________________________________________________________________

end.
