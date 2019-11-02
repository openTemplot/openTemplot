
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

unit web_capture_unit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  OleCtrls, SHDocVw, StdCtrls, ExtCtrls;

type
  Tweb_capture_form = class(TForm)
    web_browser: TWebBrowser;
    web_timer: TTimer;
    caption_timer: TTimer;
    screenshot_panel: TPanel;
    legal_memo: TMemo;
    cancel_return_button: TButton;
    cancel_close_button: TButton;
    continue_button: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure web_timerTimer(Sender: TObject);
    procedure caption_timerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  web_capture_form: Tweb_capture_form;

implementation

{$R *.DFM}

var

  web_timeout:boolean=False;

//______________________________________________________________________________

procedure Tweb_capture_form.FormCreate(Sender: TObject);

begin

  AutoScroll:=False;

  Left:=Screen.Width div 4;              // 25%    allow for alerts on left
  Top:=Screen.Height div 20;             // 5%

  Width:=Screen.Width*74 div 100;        // 74%
  Height:=Screen.Height*4 div 5;         // 80%

  web_browser.Navigate('about:blank');     // force browser to init

end;
//______________________________________________________________________________

function check_browser_zoom:boolean;

     // test zoom setting   -   display test image, then grab it as a screenshot ...

var
  window_handle:HWND;
  device_context:HDC;

  grab_bmp:TBitmap;

  window_rect:TRect;

  grab_width,grab_height:integer;

  i,n,row,col:integer;

begin
  RESULT:=False;  // init

  Screen.Cursor:=crHourGlass;

  if web_capture_form.Showing=False then web_capture_form.Show;     // calling Show resets the page zoom.

  if web_capture_form.ClientWidth<480   // allow for scrollbars
     then begin
            web_capture_form.ClientWidth:=480;
            Application.ProcessMessages;
          end;

  Application.ProcessMessages;

  web_capture_form.web_timer.Enabled:=True;

  web_capture_form.web_browser.Navigate('file:///'+ExtractFilePath(Application.ExeName)+'test_zoom_page.html');

  repeat
    Application.ProcessMessages;

  until (web_capture_form.web_browser.Busy=False) or (web_timeout=True);     // wait 8 seconds max to load file

  Screen.Cursor:=crDefault;

  Application.ProcessMessages;   // once more

  try
    window_handle:=GetForegroundWindow;

    Windows.GetClientRect(window_handle,window_rect);
    device_context:=GetDC(window_handle);
  except
    ShowMessage('error: Unable to perform zoom check.');
    EXIT;
  end;//try

  grab_width:=window_rect.Right-window_rect.Left;
  grab_height:=window_rect.Bottom-window_rect.Top;

  if (grab_width<1) or (grab_height<1)  // ???
   then begin
          ShowMessage('error: Unable to perform zoom check.');
          EXIT;
        end;

  grab_bmp:=TBitmap.Create;

  grab_bmp.Width:=grab_width;
  grab_bmp.Height:=grab_height;

  row:=0;  // keep compiler happy
  col:=0;

  try
    BitBlt(grab_bmp.Canvas.Handle,0,0,grab_width,grab_height,device_context,0,0,SRCCOPY);

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
    with grab_bmp.Canvas do begin

       if (Pixels[col+396,row]<>clRed) or (Pixels[col+397,row]<>clGreen) or (Pixels[col+398,row]<>clBlue)

          then RESULT:=False
          else RESULT:=True;

    end;//with

    grab_bmp.Free;
    ReleaseDC(window_handle,device_context);
  end;//try
end;
//______________________________________________________________________________

procedure Tweb_capture_form.FormCloseQuery(Sender:TObject;  var CanClose:Boolean);

var
  window_handle:HWND;
  device_context:HDC;
  grab_bmp:TBitmap;

  window_rect:TRect;

  grab_width,grab_height:integer;

  url_str:string;

  return_list:TStringList;

  zoom_ok:boolean;

begin
  CanClose:=False;     // not yet

  return_list:=TStringList.Create;
  return_list.Clear;
  return_list.Add('-1');  // init default result - fail

  Application.ProcessMessages;

  window_handle:=GetForegroundWindow;

  try
    Windows.GetClientRect(window_handle,window_rect);
  except
    ShowMessage('error: Unable to get the dimensions of the browser window.');

    return_list.SaveToFile(ExtractFilePath(Application.ExeName)+'internal\map\mapshot.txt');
    return_list.Free;

    Application.Terminate;

    EXIT;
  end;//try

  grab_width:=window_rect.Right-window_rect.Left;
  grab_height:=window_rect.Bottom-window_rect.Top;

  if (grab_width<1) or (grab_height<1)   // ???
     then begin
            ShowMessage('error: Unable to get the dimensions of the browser window.');

            return_list.SaveToFile(ExtractFilePath(Application.ExeName)+'internal\map\mapshot.txt');
            return_list.Free;

            Application.Terminate;

            EXIT;
          end;

  try
    device_context:=GetDC(window_handle);
  except
    ShowMessage('error: Unable to get the device context for the browser window.');

    return_list.SaveToFile(ExtractFilePath(Application.ExeName)+'internal\map\mapshot.txt');
    return_list.Free;

    Application.Terminate;

    EXIT;
  end;//try

  grab_bmp:=TBitmap.Create;

  grab_bmp.Width:=grab_width;
  grab_bmp.Height:=grab_height;

  try
    BitBlt(grab_bmp.Canvas.Handle,0,0,grab_width,grab_height,device_context,0,0,SRCCOPY);
  except
    ShowMessage('error: Unable to make screenshot from browser window.');

    grab_bmp.Free;
    ReleaseDC(window_handle,device_context);
    return_list.SaveToFile(ExtractFilePath(Application.ExeName)+'internal\map\mapshot.txt');
    return_list.Free;

    Application.Terminate;

    EXIT;
  end;//try

  url_str:=LowerCase(web_browser.LocationURL);

  legal_memo.lines.Strings[2]:=url_str;

  screenshot_panel.Visible:=True;

  repeat
    Application.Processmessages;
    Sleep(250);
  until ModalResult<>mrNone;

  if ModalResult=mrRetry
     then begin
            screenshot_panel.Visible:=False;
            grab_bmp.Free;
            ReleaseDC(window_handle,device_context);
            return_list.Free;
            ModalResult:=mrNone;
            EXIT;                // ready for another try
          end;

  if ModalResult=mrCancel
     then begin
            grab_bmp.Free;
            ReleaseDC(window_handle,device_context);

            return_list.Clear;
            return_list.Add('2');  // result - cancelled
            return_list.SaveToFile(ExtractFilePath(Application.ExeName)+'internal\map\mapshot.txt');
            return_list.Free;

            Application.Terminate;

            EXIT;
          end;

  zoom_ok:=check_browser_zoom;    // valid screenshot?

  if zoom_ok=True
     then begin
            return_list.Clear;
            return_list.Add('1');        // result - ok
            return_list.Add(url_str);
            return_list.Add(IntToStr(grab_width));
            return_list.Add(IntToStr(grab_height));

            grab_bmp.SaveToFile(ExtractFilePath(Application.ExeName)+'internal\map\mapshot.bmp');
          end
     else begin
            ShowMessage('error: The screenshot failed because the map browser page zoom is not set for dot-for-dot display of maps.'
                  +#13#13+'The current page zoom setting is not suitable for making screenshots.'
                  +#13#13+'Click on a blank part of the header space just above the map, and then press the CTRL+0 (zero) keys to reset the page zoom. Then try again.'
                  +#13#13+'Always use instead the +/- buttons on the map itself to zoom the map for screenshots.'
                  +#13#13+'If you are still unable to make screenshots, check your zoom settings in Internet Explorer.');

            screenshot_panel.Visible:=False;
            grab_bmp.Free;
            ReleaseDC(window_handle,device_context);
            return_list.Free;
            ModalResult:=mrNone;

            Application.ProcessMessages;

            web_browser.Navigate(url_str);    // back to the map

            EXIT;                // ready for another try
          end;

  grab_bmp.Free;
  ReleaseDC(window_handle,device_context);
  return_list.SaveToFile(ExtractFilePath(Application.ExeName)+'internal\map\mapshot.txt');
  return_list.Free;

  Application.Terminate;
end;
//______________________________________________________________________________

procedure Tweb_capture_form.web_timerTimer(Sender: TObject);

begin
  web_timeout:=True;
  web_timer.Enabled:=False;
end;
//______________________________________________________________________________

procedure Tweb_capture_form.caption_timerTimer(Sender: TObject);

begin
  if web_capture_form.Showing=True
     then web_capture_form.Caption:='    OpenTemplot  map  viewer :  '+web_capture_form.web_browser.LocationURL;
end;
//______________________________________________________________________________

end.

