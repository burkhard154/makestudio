object FormEditInclude: TFormEditInclude
  Left = 561
  Top = 236
  Width = 455
  Height = 138
  Caption = 'Include File...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object btOk: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 96
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilename: TJvFilenameEdit
    Left = 16
    Top = 32
    Width = 417
    Height = 21
    DialogKind = dkSave
    DefaultExt = 'txt'
    TabOrder = 2
  end
  object btEdit: TButton
    Left = 360
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 3
    OnClick = btEditClick
  end
end
