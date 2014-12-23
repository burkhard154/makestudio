object FormPassoloModuleEdit: TFormPassoloModuleEdit
  Left = 484
  Top = 180
  BorderStyle = bsDialog
  Caption = 'Passolo Project'
  ClientHeight = 110
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    426
    110)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 425
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      425
      65)
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 42
      Height = 13
      Caption = 'Filename'
    end
    object Panel3: TPanel
      Left = 4
      Top = 4
      Width = 417
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Caption = 'Passolo Project'
      Color = clGray
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object edtProjectPath: TJvFilenameEdit
      Left = 8
      Top = 40
      Width = 409
      Height = 21
      Filter = 'Passolo Project Files (*.lpu)|*.lpu'
      DialogOptions = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 66
    Width = 425
    Height = 43
    Anchors = []
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
