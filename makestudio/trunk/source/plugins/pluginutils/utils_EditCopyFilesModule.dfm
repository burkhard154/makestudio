object FormCopyFilesEdit: TFormCopyFilesEdit
  Left = 384
  Top = 179
  BorderStyle = bsDialog
  Caption = 'Copy Files'
  ClientHeight = 340
  ClientWidth = 516
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 216
    Width = 50
    Height = 13
    Caption = 'Source file'
  end
  object Label2: TLabel
    Left = 8
    Top = 256
    Width = 47
    Height = 13
    Caption = 'Target file'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 516
    Height = 209
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object lvFiles: TListView
      Left = 4
      Top = 25
      Width = 508
      Height = 180
      Align = alClient
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Source'
          Width = 250
        end
        item
          Caption = 'Target'
          Width = 250
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
    object Panel2: TPanel
      Left = 4
      Top = 4
      Width = 508
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Files'
      Color = clAppWorkSpace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object edSource: TJvFilenameEdit
    Left = 8
    Top = 232
    Width = 497
    Height = 21
    Filter = 'All Files (*.*)|*.*'
    DialogOptions = [ofPathMustExist, ofFileMustExist]
    ButtonFlat = True
    TabOrder = 1
  end
  object edTarget: TJvFilenameEdit
    Left = 8
    Top = 272
    Width = 497
    Height = 21
    DialogKind = dkSave
    Filter = 'All Files (*.*)|*.*'
    DialogOptions = []
    ButtonFlat = True
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 88
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btAdd: TButton
    Left = 168
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 5
    OnClick = btAddClick
  end
  object btDelete: TButton
    Left = 248
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Delete'
    TabOrder = 6
    OnClick = btDeleteClick
  end
  object btReplace: TButton
    Left = 328
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Replace'
    TabOrder = 7
    OnClick = btReplaceClick
  end
end
