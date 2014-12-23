object FormEditWhile: TFormEditWhile
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'While'
  ClientHeight = 260
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 60
    Height = 13
    Caption = 'Current Wert'
  end
  object lbContent: TLabel
    Left = 16
    Top = 72
    Width = 289
    Height = 49
    AutoSize = False
    Caption = '???'
    Color = clBtnFace
    ParentColor = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 16
    Top = 168
    Width = 27
    Height = 13
    Caption = 'Value'
  end
  object Label4: TLabel
    Left = 16
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Variable'
  end
  object Label5: TLabel
    Left = 16
    Top = 128
    Width = 44
    Height = 13
    Caption = 'Condition'
  end
  object cbVars: TComboBox
    Left = 16
    Top = 24
    Width = 289
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'cbVars'
    OnChange = cbVarsChange
  end
  object edValue: TEdit
    Left = 16
    Top = 184
    Width = 289
    Height = 21
    TabOrder = 1
  end
  object cbCondition: TComboBox
    Left = 16
    Top = 144
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
    Text = '='
    Items.Strings = (
      '='
      '>'
      '<'
      '>='
      '<='
      '<>'
      'Enth'#228'lt'
      'Enth'#228'lt nicht'
      'Datei existiert'
      'Verzeichnis existiert')
  end
  object btOk: TButton
    Left = 16
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btCancel: TButton
    Left = 96
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
