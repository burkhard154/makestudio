object FormEditBatchModule: TFormEditBatchModule
  Left = 385
  Top = 227
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Select Batch File'
  ClientHeight = 206
  ClientWidth = 734
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 144
  TextHeight = 20
  object Panel1: TPanel
    Left = 0
    Top = 144
    Width = 734
    Height = 62
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 12
      Top = 12
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 132
      Top = 12
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 734
    Height = 177
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 6
      Top = 6
      Width = 722
      Height = 165
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Caption = 'Batch File'
      TabOrder = 0
      DesignSize = (
        722
        165)
      object Label1: TLabel
        Left = 24
        Top = 36
        Width = 65
        Height = 20
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Filename'
      end
      object edFilename: TJvFilenameEdit
        Left = 24
        Top = 60
        Width = 665
        Height = 28
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        DefaultExt = 'bat'
        Filter = 'Batch Files (*.bat)|*.bat|All Files (*.*)|*.*'
        DialogOptions = [ofPathMustExist, ofFileMustExist]
        ButtonFlat = True
        ButtonWidth = 32
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
      end
    end
  end
end
