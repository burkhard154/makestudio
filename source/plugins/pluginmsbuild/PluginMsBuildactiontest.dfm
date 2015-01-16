object FormActionTest: TFormActionTest
  Left = 300
  Top = 113
  BorderStyle = bsDialog
  Caption = 'MSBuild settings'
  ClientHeight = 120
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 24
    Width = 154
    Height = 13
    Caption = 'Path of MSBuild programm:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object tbMSBuildExe: TJvFilenameEdit
    Left = 16
    Top = 43
    Width = 433
    Height = 21
    Filter = 'All files (*.exe)|*.exe'
    TabOrder = 0
    Text = 'MSBuild.exe'
  end
  object Button1: TButton
    Left = 270
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 374
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
