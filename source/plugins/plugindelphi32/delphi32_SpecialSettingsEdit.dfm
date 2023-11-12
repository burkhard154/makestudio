object FormEditSpecialSettingsParams: TFormEditSpecialSettingsParams
  Left = 441
  Top = 306
  Caption = 'Get Delphi32 Info'
  ClientHeight = 335
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 26
  object Label1: TLabel
    Left = 32
    Top = 144
    Width = 26
    Height = 26
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'To'
  end
  object Label2: TLabel
    Left = 32
    Top = 48
    Width = 114
    Height = 26
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Set Variable'
  end
  object Button1: TButton
    Left = 32
    Top = 272
    Width = 151
    Height = 51
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 192
    Top = 272
    Width = 151
    Height = 51
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbOperation: TComboBox
    Left = 32
    Top = 176
    Width = 611
    Height = 34
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
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
    Left = 32
    Top = 80
    Width = 611
    Height = 34
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    TabOrder = 3
    Text = 'ComboBox1'
  end
end
