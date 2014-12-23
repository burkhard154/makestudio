object FormEditBatchInternal: TFormEditBatchInternal
  Left = 385
  Top = 227
  Width = 598
  Height = 555
  Caption = 'Edit Batch command list'
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
    Top = 456
    Width = 590
    Height = 65
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 32
      Width = 75
      Height = 25
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 88
      Top = 32
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btImport: TButton
      Left = 168
      Top = 32
      Width = 97
      Height = 25
      Caption = 'Load from...'
      TabOrder = 2
      OnClick = btImportClick
    end
    object btExport: TButton
      Left = 272
      Top = 32
      Width = 97
      Height = 25
      Caption = 'Save to...'
      TabOrder = 3
      OnClick = btExportClick
    end
    object cbReplaceVars: TCheckBox
      Left = 16
      Top = 8
      Width = 297
      Height = 17
      Caption = 'Replace Jedi Make variables (%Varname%) in text'
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 590
    Height = 456
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object Memo: TSynEdit
      Left = 6
      Top = 6
      Width = 578
      Height = 444
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 0
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Highlighter = SynBatSyn1
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'bat'
    Filter = 'Batch Files (*.bat)|*.bat'
    Title = 'Open Batch File'
    Left = 384
    Top = 432
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'bat'
    Filter = 'Batch Files (*.bat)|*.bat'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Save Batch File'
    Left = 416
    Top = 432
  end
  object SynBatSyn1: TSynBatSyn
    Left = 448
    Top = 432
  end
end
