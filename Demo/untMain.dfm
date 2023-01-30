object Form22: TForm22
  Left = 0
  Top = 0
  Caption = 'HTML Email Builder'
  ClientHeight = 595
  ClientWidth = 1081
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 185
    Top = 0
    Width = 896
    Height = 595
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 432
      Height = 595
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Memo1: TMemo
        Left = 0
        Top = 35
        Width = 432
        Height = 560
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WordWrap = False
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 432
        Height = 35
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Html'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold, fsUnderline]
        ParentFont = False
        TabOrder = 1
      end
    end
    object Panel8: TPanel
      Left = 432
      Top = 0
      Width = 464
      Height = 595
      Align = alRight
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 1
      object Panel9: TPanel
        Left = 0
        Top = 0
        Width = 464
        Height = 35
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Output'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold, fsUnderline]
        ParentFont = False
        TabOrder = 0
      end
      object WebBrowser1: TWebBrowser
        Left = 0
        Top = 35
        Width = 464
        Height = 560
        Align = alClient
        TabOrder = 1
        ExplicitLeft = -40
        ExplicitTop = 19
        ControlData = {
          4C000000F52F0000E13900000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 595
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object ListBox1: TListBox
      Left = 0
      Top = 150
      Width = 185
      Height = 445
      Style = lbOwnerDrawFixed
      Align = alLeft
      BevelInner = bvNone
      BevelOuter = bvNone
      ItemHeight = 30
      Items.Strings = (
        'Bootstrap Style Alerts'
        'Bootstrap Style Buttons'
        'Cancel Booking'
        'Payment Failed'
        'Voucher')
      Sorted = True
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object Panel3: TPanel
      Left = 0
      Top = 35
      Width = 185
      Height = 80
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object rbHtmlEmail: TRadioButton
        Left = 16
        Top = 43
        Width = 129
        Height = 17
        Caption = 'E-mail Compatible'
        TabOrder = 0
        OnClick = rbHtmlEmailClick
      end
      object rbHtmlCss: TRadioButton
        Left = 16
        Top = 17
        Width = 129
        Height = 17
        Caption = 'Standard Css/Html'
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = rbHtmlCssClick
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 115
      Width = 185
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Examples'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      TabOrder = 2
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Output Format'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      TabOrder = 3
    end
  end
end
