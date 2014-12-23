object FormEditNSISProjectParams: TFormEditNSISProjectParams
  Left = 481
  Top = 346
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'NSIS Project'
  ClientHeight = 133
  ClientWidth = 562
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
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 74
    Height = 13
    Caption = 'Output filename'
  end
  object Button1: TButton
    Left = 16
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 96
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilename: TJvFilenameEdit
    Left = 16
    Top = 24
    Width = 529
    Height = 21
    AddQuotes = False
    TabOrder = 2
  end
  object edOutputfilename: TJvFilenameEdit
    Left = 16
    Top = 64
    Width = 529
    Height = 21
    AddQuotes = False
    TabOrder = 3
  end
  object btCompiler: TButton
    Left = 176
    Top = 96
    Width = 137
    Height = 25
    Caption = 'NSIS Compiler...'
    TabOrder = 4
    OnClick = btCompilerClick
  end
  object btEdit: TButton
    Left = 320
    Top = 96
    Width = 89
    Height = 25
    Caption = 'Edit Project...'
    TabOrder = 5
    OnClick = btEditClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'NSIS Compiler (makensis.exe)|makensis.exe'
    Title = 'Open NSIS Compiler'
    Left = 312
    Top = 8
  end
end
