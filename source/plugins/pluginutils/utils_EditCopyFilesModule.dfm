object FormCopyFilesEdit: TFormCopyFilesEdit
  Left = 384
  Top = 179
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  BorderStyle = bsDialog
  Caption = 'Copy Files'
  ClientHeight = 510
  ClientWidth = 783
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 144
  TextHeight = 20
  object Label1: TLabel
    Left = 12
    Top = 324
    Width = 75
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Source file'
  end
  object Label2: TLabel
    Left = 12
    Top = 384
    Width = 70
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Target file'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 783
    Height = 314
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object lvFiles: TListView
      Left = 4
      Top = 36
      Width = 775
      Height = 274
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      BorderStyle = bsNone
      Columns = <
        item
          Caption = 'Source'
          Width = 375
        end
        item
          Caption = 'Target'
          Width = 375
        end>
      TabOrder = 1
      ViewStyle = vsReport
      OnClick = lvFilesClick
    end
    object Panel2: TPanel
      Left = 4
      Top = 4
      Width = 775
      Height = 32
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Files'
      Color = clAppWorkSpace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object edSource: TJvFilenameEdit
    Left = 12
    Top = 348
    Width = 746
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Filter = 'All Files (*.*)|*.*'
    DialogOptions = [ofPathMustExist, ofFileMustExist]
    ButtonFlat = True
    ButtonWidth = 32
    TabOrder = 1
    Text = ''
  end
  object edTarget: TJvFilenameEdit
    Left = 12
    Top = 408
    Width = 746
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    DialogKind = dkSave
    Filter = 'All Files (*.*)|*.*'
    DialogOptions = []
    ButtonFlat = True
    ButtonWidth = 32
    TabOrder = 2
    Text = ''
  end
  object Button1: TButton
    Left = 12
    Top = 456
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 132
    Top = 456
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object btAdd: TButton
    Left = 252
    Top = 456
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Add'
    TabOrder = 5
    OnClick = btAddClick
  end
  object btDelete: TButton
    Left = 372
    Top = 456
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Delete'
    TabOrder = 6
    OnClick = btDeleteClick
  end
  object btReplace: TButton
    Left = 492
    Top = 456
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Replace'
    TabOrder = 7
    OnClick = btReplaceClick
  end
end
