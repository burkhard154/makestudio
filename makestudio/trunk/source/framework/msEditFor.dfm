object FormEditFor: TFormEditFor
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'For'
  ClientHeight = 219
  ClientWidth = 320
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
  object Label4: TLabel
    Left = 16
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Variable'
  end
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 64
    Height = 13
    Caption = 'Current Value'
  end
  object Label2: TLabel
    Left = 16
    Top = 128
    Width = 51
    Height = 13
    Caption = 'Start value'
  end
  object Label3: TLabel
    Left = 120
    Top = 128
    Width = 48
    Height = 13
    Caption = 'End value'
  end
  object btOk: TButton
    Left = 16
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 96
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbVars: TComboBox
    Left = 16
    Top = 24
    Width = 289
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = 'cbVars'
    OnChange = cbVarsChange
  end
  object edStart: TJvSpinEdit
    Left = 16
    Top = 144
    Width = 97
    Height = 21
    TabOrder = 3
  end
  object edEnd: TJvSpinEdit
    Left = 120
    Top = 144
    Width = 97
    Height = 21
    Value = 1.000000000000000000
    TabOrder = 4
  end
end
