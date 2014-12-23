object FormEditExample_commandParams: TFormEditExample_commandParams
  Left = 440
  Top = 305
  Width = 346
  Height = 136
  Caption = 'FormEditParams'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 16
    Top = 24
    Width = 305
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 64
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 144
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 2
  end
end
