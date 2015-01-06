object FormActionTest: TFormActionTest
  Left = 300
  Top = 113
  Caption = 'Server - Einstellungen'
  ClientHeight = 302
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 344
    Height = 248
    Align = alClient
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 6
      Top = 6
      Width = 332
      Height = 236
      Align = alClient
      Caption = 'Smtp - Server'
      TabOrder = 0
      object Button3: TButton
        Left = 48
        Top = 112
        Width = 121
        Height = 65
        Caption = 'Button3'
        TabOrder = 0
        OnClick = Button3Click
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 248
    Width = 344
    Height = 54
    Align = alBottom
    TabOrder = 1
    object Button1: TButton
      Left = 144
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Speichern'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 248
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Abbrechen'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object IdSMTP1: TIdSMTP
    IOHandler = IdSSLIOHandlerSocketOpenSSL1
    HeloName = 'SteffenHornig'
    Host = 'mail.gmx.net'
    Password = 'omTestPass'
    Port = 587
    SASLMechanisms = <>
    UseTLS = utUseExplicitTLS
    Username = 'optimeas@gmx.de'
    Left = 262
    Top = 62
  end
  object IdMessage1: TIdMessage
    AttachmentEncoding = 'UUE'
    Body.Strings = (
      'TestTest')
    BccList = <>
    CCList = <>
    Encoding = meDefault
    FromList = <
      item
      end>
    Recipients = <
      item
        Address = 'steffen.hornig@optimeas.de'
        Text = 'steffen.hornig@optimeas.de'
        Domain = 'optimeas.de'
        User = 'steffen.hornig'
      end>
    ReplyTo = <>
    Subject = 'Hollaaaaaaaaaaaaaaaaaaa...'
    Sender.Address = 'optimeas@gmx.de'
    Sender.Text = 'optimeas@gmx.de'
    Sender.Domain = 'gmx.de'
    Sender.User = 'optimeas'
    ConvertPreamble = True
    Left = 262
    Top = 118
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    Destination = 'mail.gmx.net:587'
    Host = 'mail.gmx.net'
    MaxLineAction = maException
    Port = 587
    DefaultPort = 0
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 254
    Top = 174
  end
end
