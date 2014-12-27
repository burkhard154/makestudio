object FormEditBatchInternal: TFormEditBatchInternal
  Left = 385
  Top = 227
  Caption = 'Edit Batch command list'
  ClientHeight = 517
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 437
    Width = 582
    Height = 80
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 10
      Top = 39
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
      Left = 108
      Top = 39
      Width = 93
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
    object btImport: TButton
      Left = 207
      Top = 39
      Width = 119
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Load from...'
      TabOrder = 2
      OnClick = btImportClick
    end
    object btExport: TButton
      Left = 335
      Top = 39
      Width = 119
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Save to...'
      TabOrder = 3
      OnClick = btExportClick
    end
    object cbReplaceVars: TCheckBox
      Left = 20
      Top = 10
      Width = 365
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Replace Jedi Make variables (%Varname%) in text'
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 437
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object Memo: TSynEdit
      Left = 6
      Top = 6
      Width = 570
      Height = 425
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 0
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Highlighter = SynBatSyn1
      FontSmoothing = fsmNone
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
    Left = 424
    Top = 432
  end
  object SynBatSyn1: TSynBatSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 472
    Top = 432
  end
  object SynBatSyn2: TSynBatSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 296
    Top = 368
  end
end
