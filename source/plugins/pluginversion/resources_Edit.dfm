object FormEditParams: TFormEditParams
  Left = 467
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Version'
  ClientHeight = 473
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 20
    Top = 5
    Width = 66
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Version file'
  end
  object Button1: TButton
    Left = 20
    Top = 428
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 118
    Top = 428
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilename: TJvFilenameEdit
    Left = 20
    Top = 27
    Width = 385
    Height = 24
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AddQuotes = False
    DefaultExt = 'res'
    Filter = 
      'Resources (*.res;*.dcr;*.rc)|*.res;*.dcr;*.rc|All files (*.*)|*.' +
      '*'
    ButtonWidth = 26
    TabOrder = 2
    Text = ''
  end
  object rgType: TRadioGroup
    Left = 20
    Top = 213
    Width = 385
    Height = 129
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
    Left = 20
    Top = 350
    Width = 385
    Height = 70
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Version Key Name'
    TabOrder = 4
    object edVersionKey: TEdit
      Left = 10
      Top = 30
      Width = 365
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
      Text = 'edVersionKey'
    end
  end
  object rgIncSet: TRadioGroup
    Left = 20
    Top = 66
    Width = 385
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Type'
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      'Set'
      'Increase')
    TabOrder = 5
    OnClick = rgIncSetClick
  end
  object grpSet: TGroupBox
    Left = 20
    Top = 125
    Width = 385
    Height = 80
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Set'
    TabOrder = 6
    object Label3: TLabel
      Left = 10
      Top = 21
      Width = 78
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Main Version'
    end
    object Label4: TLabel
      Left = 197
      Top = 21
      Width = 52
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Release'
    end
    object Label5: TLabel
      Left = 293
      Top = 21
      Width = 30
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Build'
    end
    object Label6: TLabel
      Left = 103
      Top = 21
      Width = 82
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Minor Version'
    end
    object edtMainVer: TEdit
      Left = 10
      Top = 41
      Width = 68
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
      Text = '0'
    end
    object edtMinorVer: TEdit
      Left = 103
      Top = 41
      Width = 68
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 1
      Text = '0'
    end
    object edtRel: TEdit
      Left = 197
      Top = 41
      Width = 68
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
      Text = '0'
    end
    object edtBuild: TEdit
      Left = 292
      Top = 41
      Width = 67
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 3
      Text = '0'
    end
    object updoMain: TUpDown
      Left = 78
      Top = 41
      Width = 18
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = edtMainVer
      TabOrder = 4
    end
    object updoMinor: TUpDown
      Left = 171
      Top = 41
      Width = 19
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = edtMinorVer
      TabOrder = 5
    end
    object updoRel: TUpDown
      Left = 265
      Top = 41
      Width = 18
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = edtRel
      TabOrder = 6
    end
    object updoBuild: TUpDown
      Left = 359
      Top = 41
      Width = 19
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Associate = edtBuild
      TabOrder = 7
    end
  end
end
