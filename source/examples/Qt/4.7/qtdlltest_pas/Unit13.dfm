object Form13: TForm13
  Left = 0
  Top = 0
  Caption = 'Form13'
  ClientHeight = 100
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 59
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 0
    OnClick = Button1Click
  end
  object JvFilenameEdit1: TJvFilenameEdit
    Left = 24
    Top = 24
    Width = 361
    Height = 21
    TabOrder = 1
    Text = 'C:\Qt\2010.05\qt\bin\plugin_qt_test.dll'
  end
end
