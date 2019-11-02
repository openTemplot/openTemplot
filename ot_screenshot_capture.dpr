
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

This file is intended to be compiled in Delphi5 or other early versions of Delphi.

*)

program ot_screenshot_capture;

uses
  Forms,
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Dialogs,
  ShellAPI,
  web_capture_unit in 'web_capture_unit.pas' {web_capture_form};

{$R *.RES}

var
  param_str:string;

begin
  Application.Initialize;

  Application.Title:='OT Screenshot Capture';

  Application.CreateForm(Tweb_capture_form, web_capture_form);

  if (ParamCount>0)    // command line parameters
     then begin
            param_str:=ParamStr(ParamCount);   // last or only parameter   ignore any others

            web_capture_form.Show;

            Application.ProcessMessages;

            web_capture_form.web_browser.Navigate('about:blank');    // force it to update

            Application.ProcessMessages;

            web_capture_form.web_browser.Navigate(param_str);        // to the URL

            Application.ProcessMessages;

            if (Pos('maps.nls.uk/view/',param_str)>0) and (Pos('zoom',param_str)=0)
               then ShowMessage('Please click the "Link to this view" button at the bottom left of this map.'
                       +#13+#13+'Then zoom out and back in, and make sure that the full URL is shown at the top of the browser, including the word "zoom".'
                       +#13+#13+'This ensures that OpenTemplot can read the required information from the browser.');

            web_capture_form.caption_timer.Enabled:=True;

            Application.Run;

          end
     else begin
            ShowMessage('error - no capture URL supplied        ');
            Application.Terminate;
          end;
//______________________________________________________________________________          

end.

