object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 185
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Assembley'
  end
  object Label2: TLabel
    Left = 24
    Top = 56
    Width = 70
    Height = 13
    Caption = 'Full Classname'
  end
  object Edit1: TEdit
    Left = 24
    Top = 24
    Width = 297
    Height = 21
    TabOrder = 0
    Text = 'dotnetclient.dll'
  end
  object Edit2: TEdit
    Left = 24
    Top = 72
    Width = 297
    Height = 21
    TabOrder = 1
    Text = 'dotnetclient.TClientClass'
  end
  object Button1: TButton
    Left = 24
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 2
    OnClick = Button1Click
  end
end
