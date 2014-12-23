object FormEditEditMultipleVersionResourceParams: TFormEditEditMultipleVersionResourceParams
  Left = 340
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Multiple Version'
  ClientHeight = 532
  ClientWidth = 347
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
    Top = 289
    Width = 51
    Height = 13
    Caption = 'Version file'
  end
  object Label2: TLabel
    Left = 16
    Top = 4
    Width = 56
    Height = 13
    Caption = 'Version files'
  end
  object Button1: TButton
    Left = 16
    Top = 492
    Width = 75
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 96
    Top = 492
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilename: TJvFilenameEdit
    Left = 16
    Top = 145
    Width = 169
    Height = 21
    AddQuotes = False
    DefaultExt = 'res'
    Filter = 'Resources (*.res)|*.res'
    TabOrder = 2
  end
  object rgType: TRadioGroup
    Left = 16
    Top = 316
    Width = 313
    Height = 105
    Caption = 'Increase'
    Enabled = False
    ItemIndex = 3
    Items.Strings = (
      'Major version'
      'Minor version'
      'Release'
      'Build')
    TabOrder = 3
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 428
    Width = 313
    Height = 57
    Caption = 'Version Key Name'
    TabOrder = 4
    object edVersionKey: TEdit
      Left = 8
      Top = 24
      Width = 297
      Height = 21
      TabOrder = 0
      Text = 'edVersionKey'
    end
  end
  object rgIncSet: TRadioGroup
    Left = 16
    Top = 198
    Width = 313
    Height = 41
    Caption = 'Set/Increase'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Set'
      'Increase')
    TabOrder = 5
    OnClick = rgIncSetClick
  end
  object grpSet: TGroupBox
    Left = 16
    Top = 246
    Width = 313
    Height = 65
    Caption = 'Set'
    TabOrder = 6
    object Label3: TLabel
      Left = 8
      Top = 17
      Width = 61
      Height = 13
      Caption = 'Main Version'
    end
    object Label4: TLabel
      Left = 160
      Top = 17
      Width = 39
      Height = 13
      Caption = 'Release'
    end
    object Label5: TLabel
      Left = 238
      Top = 17
      Width = 23
      Height = 13
      Caption = 'Build'
    end
    object Label6: TLabel
      Left = 84
      Top = 17
      Width = 64
      Height = 13
      Caption = 'Minor Version'
    end
    object edtMainVer: TEdit
      Left = 8
      Top = 33
      Width = 55
      Height = 21
      TabOrder = 0
      Text = '0'
    end
    object edtMinorVer: TEdit
      Left = 84
      Top = 33
      Width = 55
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object edtRel: TEdit
      Left = 160
      Top = 33
      Width = 55
      Height = 21
      TabOrder = 2
      Text = '0'
    end
    object edtBuild: TEdit
      Left = 237
      Top = 33
      Width = 55
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object updoMain: TUpDown
      Left = 63
      Top = 33
      Width = 15
      Height = 21
      Associate = edtMainVer
      TabOrder = 4
    end
    object updoMinor: TUpDown
      Left = 139
      Top = 33
      Width = 15
      Height = 21
      Associate = edtMinorVer
      TabOrder = 5
    end
    object updoRel: TUpDown
      Left = 215
      Top = 33
      Width = 15
      Height = 21
      Associate = edtRel
      TabOrder = 6
    end
    object updoBuild: TUpDown
      Left = 292
      Top = 33
      Width = 15
      Height = 21
      Associate = edtBuild
      TabOrder = 7
    end
  end
  object btAdd: TButton
    Left = 192
    Top = 144
    Width = 65
    Height = 25
    Caption = 'Add'
    TabOrder = 7
    OnClick = btAddClick
  end
  object btRemove: TButton
    Left = 264
    Top = 144
    Width = 67
    Height = 25
    Caption = 'Remove'
    TabOrder = 8
    OnClick = btRemoveClick
  end
  object cbRecursive: TCheckBox
    Left = 16
    Top = 176
    Width = 313
    Height = 17
    Caption = 
      'Include sub-directories (else: all files in dir, if path is a di' +
      'r)'
    TabOrder = 9
  end
  object mmFiles: TMemo
    Left = 16
    Top = 24
    Width = 313
    Height = 113
    Lines.Strings = (
      'mmFiles')
    ScrollBars = ssBoth
    TabOrder = 10
  end
end
