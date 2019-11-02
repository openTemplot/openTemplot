object web_capture_form: Tweb_capture_form
  Left = 253
  Top = 162
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = '    OpenTemplot  map  viewer :  '
  ClientHeight = 683
  ClientWidth = 802
  Color = clWindow
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 18
  object web_browser: TWebBrowser
    Left = 0
    Top = 0
    Width = 802
    Height = 683
    Align = alClient
    TabOrder = 0
    ControlData = {
      4C00000050420000793800000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object screenshot_panel: TPanel
    Left = 18
    Top = 18
    Width = 770
    Height = 644
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = 12648447
    TabOrder = 1
    Visible = False
    object legal_memo: TMemo
      Left = 12
      Top = 16
      Width = 745
      Height = 593
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      Lines.Strings = (
        'You are about to capture a screenshot from this web site:'
        ' '
        'url'
        ' '
        
          'The National Library of Scotland make their map content availabl' +
          'e under a Creative Commons '
        
          'licence for personal non-commercial use only, and provided alway' +
          's that it is clearly attributed '
        
          'to "© National Library of Scotland" in any public display. Any u' +
          'se for commercial purposes or '
        
          'financial gain requires you to obtain an appropriate agreement w' +
          'ith them.'
        ' '
        
          'Content from OpenStreetMap may be freely used for any purpose, p' +
          'rovided always that it is '
        
          'clearly attributed to "© OpenStreetMap contributors" in any publ' +
          'ic display.'
        ' '
        
          'Google Maps allow non-commercial use of screenshots provided you' +
          ' comply with their '
        
          'guidelines at: http://google.co.uk/permissions/geoguidelines.htm' +
          'l'
        ' '
        
          'Any other content appearing on the above web site may not be cov' +
          'ered by these provisions.'
        
          'It is your responsibility to ensure that you are complying with ' +
          'any copyright restrictions before '
        'making screenshots.'
        ' '
        
          'If you click [ continue ] below, a screenshot will be captured a' +
          'nd inserted as a background '
        'picture shape on the trackpad at your current model scale.'
        ' '
        
          'Any extraneous content such as borders and headings can be remov' +
          'ed using the crop/combine '
        'function in the background shapes.')
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object cancel_return_button: TButton
      Left = 12
      Top = 592
      Width = 229
      Height = 33
      Caption = 'cancel  and  return  to  map'
      ModalResult = 4
      TabOrder = 1
    end
    object cancel_close_button: TButton
      Left = 256
      Top = 592
      Width = 201
      Height = 33
      Caption = 'cancel  and  close  map'
      ModalResult = 2
      TabOrder = 2
    end
    object continue_button: TButton
      Left = 476
      Top = 588
      Width = 269
      Height = 41
      Caption = 'continue  -  make  screenshot'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ModalResult = 1
      ParentFont = False
      TabOrder = 3
    end
  end
  object web_timer: TTimer
    Enabled = False
    Interval = 8000
    OnTimer = web_timerTimer
    Left = 544
    Top = 8
  end
  object caption_timer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = caption_timerTimer
    Left = 656
    Top = 12
  end
end
