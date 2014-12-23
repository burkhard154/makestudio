object FormRunCmd: TFormRunCmd
  Left = 0
  Top = 0
  Width = 546
  Height = 482
  Caption = 'Execute Command'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 136
    Width = 52
    Height = 13
    Caption = 'Arguments'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 82
    Height = 13
    Caption = 'Default Directory'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 52
    Height = 13
    Caption = 'Application'
  end
  object MemoCmd: TMemo
    Left = 8
    Top = 152
    Width = 521
    Height = 57
    TabOrder = 0
  end
  object btRun: TButton
    Left = 8
    Top = 216
    Width = 521
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = btRunClick
  end
  object MemoAnswer: TMemo
    Left = 8
    Top = 248
    Width = 521
    Height = 193
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 8
    Top = 24
    Width = 521
    Height = 21
    TabOrder = 3
  end
  object MemoApp: TMemo
    Left = 8
    Top = 72
    Width = 521
    Height = 57
    TabOrder = 4
  end
  object JvFormAnimation1: TJvFormAnimation
    Left = 224
    Top = 136
  end
end
