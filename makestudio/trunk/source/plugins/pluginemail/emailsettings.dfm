object FormEmailSettings: TFormEmailSettings
  Left = 300
  Top = 113
  BorderStyle = bsDialog
  Caption = 'Email Settings'
  ClientHeight = 297
  ClientWidth = 563
  Color = clBtnFace
  Constraints.MaxHeight = 325
  Constraints.MinHeight = 325
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    563
    297)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 254
    Height = 13
    Caption = 'These settings will be used by the mail plugin'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cxLabel2: TLabel
    Left = 12
    Top = 49
    Width = 53
    Height = 13
    Caption = 'Mail Server'
    Transparent = True
  end
  object cxLabel4: TLabel
    Left = 12
    Top = 76
    Width = 48
    Height = 13
    Caption = 'Username'
    Transparent = True
  end
  object cxLabel3: TLabel
    Left = 12
    Top = 103
    Width = 46
    Height = 13
    Caption = 'Password'
    Transparent = True
  end
  object cxLabel5: TLabel
    Left = 12
    Top = 130
    Width = 17
    Height = 13
    Caption = 'SSL'
    Transparent = True
  end
  object cxLabel6: TLabel
    Left = 12
    Top = 157
    Width = 40
    Height = 13
    Caption = 'SSL Port'
    Transparent = True
  end
  object cxLabel7: TLabel
    Left = 12
    Top = 184
    Width = 61
    Height = 13
    Caption = 'Email Sender'
    Transparent = True
  end
  object cxLabel8: TLabel
    Left = 12
    Top = 211
    Width = 62
    Height = 13
    Caption = 'Emailaddress'
    Transparent = True
  end
  object edMailserver: TEdit
    Left = 120
    Top = 48
    Width = 430
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object edMailuser: TEdit
    Left = 120
    Top = 75
    Width = 430
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
  end
  object edMailpassword: TEdit
    Left = 120
    Top = 102
    Width = 430
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    PasswordChar = '*'
    TabOrder = 0
  end
  object edMailSSLPort: TEdit
    Left = 120
    Top = 156
    Width = 430
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = '0'
  end
  object edMailSenderName: TEdit
    Left = 120
    Top = 183
    Width = 430
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edMailsenderaddress: TEdit
    Left = 120
    Top = 210
    Width = 430
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object btOk: TButton
    Left = 12
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btCancel: TButton
    Left = 104
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object cbMailSSL: TComboBox
    Left = 120
    Top = 129
    Width = 420
    Height = 21
    Style = csDropDownList
    TabOrder = 7
    Items.Strings = (
      'keine Verschl'#252'sselung'
      'SSL/TLS')
  end
end
