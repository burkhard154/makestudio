object FormEditBatchModule: TFormEditBatchModule
  Left = 385
  Top = 227
  Width = 511
  Height = 193
  Caption = 'Select Batch File'
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
  object Panel1: TPanel
    Left = 0
    Top = 118
    Width = 503
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 503
    Height = 118
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 6
      Top = 6
      Width = 491
      Height = 106
      Align = alClient
      Caption = 'Batch File'
      TabOrder = 0
      DesignSize = (
        491
        106)
      object Label1: TLabel
        Left = 16
        Top = 24
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object edFilename: TJvFilenameEdit
        Left = 16
        Top = 40
        Width = 453
        Height = 21
        DefaultExt = 'bat'
        Filter = 'Batch Files (*.bat)|*.bat|All Files (*.*)|*.*'
        DialogOptions = [ofPathMustExist, ofFileMustExist]
        ButtonFlat = True
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
  end
end
