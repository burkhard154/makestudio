object FormEditSpecialSettingsParams: TFormEditSpecialSettingsParams
  Left = 441
  Top = 306
  Margins.Left = 2
  Margins.Top = 2
  Margins.Right = 2
  Margins.Bottom = 2
  Caption = 'Get Delphi32 Info'
  ClientHeight = 251
  ClientWidth = 494
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 144
  TextHeight = 20
  object Label1: TLabel
    Left = 24
    Top = 108
    Width = 18
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'To'
  end
  object Label2: TLabel
    Left = 24
    Top = 36
    Width = 87
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Set Variable'
  end
  object Button1: TButton
    Left = 24
    Top = 204
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 144
    Top = 204
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbOperation: TComboBox
    Left = 24
    Top = 132
    Width = 458
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
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
    Left = 24
    Top = 60
    Width = 458
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 3
    Text = 'ComboBox1'
  end
end
