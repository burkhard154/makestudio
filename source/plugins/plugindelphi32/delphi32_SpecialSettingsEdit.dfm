object FormEditSpecialSettingsParams: TFormEditSpecialSettingsParams
  Left = 441
  Top = 306
  Width = 344
  Height = 223
  Caption = 'Get Delphi32 Info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 72
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object Label2: TLabel
    Left = 16
    Top = 24
    Width = 57
    Height = 13
    Caption = 'Set Variable'
  end
  object Button1: TButton
    Left = 16
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 96
    Top = 136
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbOperation: TComboBox
    Left = 16
    Top = 88
    Width = 305
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'cbOperation'
    Items.Strings = (
      'BPL Directory'
      'DCP Directory'
      'Root Directory'
      'Searchpath'
      'BDS Project Directory')
  end
  object cbVars: TComboBox
    Left = 16
    Top = 40
    Width = 305
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Text = 'ComboBox1'
  end
end
