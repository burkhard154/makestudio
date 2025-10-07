object FormEditBatchInternal: TFormEditBatchInternal
  Left = 385
  Top = 227
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'Edit Batch command list'
  ClientHeight = 776
  ClientWidth = 882
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
    Top = 656
    Width = 882
    Height = 120
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 15
      Top = 59
      Width = 138
      Height = 46
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 162
      Top = 59
      Width = 140
      Height = 46
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btImport: TButton
      Left = 311
      Top = 59
      Width = 178
      Height = 46
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Load from...'
      TabOrder = 2
      OnClick = btImportClick
    end
    object btExport: TButton
      Left = 503
      Top = 59
      Width = 178
      Height = 46
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Save to...'
      TabOrder = 3
      OnClick = btExportClick
    end
    object cbReplaceVars: TCheckBox
      Left = 30
      Top = 15
      Width = 548
      Height = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Replace Jedi Make variables (%Varname%) in text'
      TabOrder = 4
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 882
    Height = 656
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object Memo: TSynEdit
      Left = 6
      Top = 6
      Width = 870
      Height = 644
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -24
      Font.Name = 'Courier New'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 0
      UseCodeFolding = False
      BookMarkOptions.LeftMargin = 3
      BookMarkOptions.Xoffset = 18
      ExtraLineSpacing = 3
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Font.Quality = fqClearTypeNatural
      Gutter.Bands = <>
      Highlighter = SynBatSyn1
      ScrollbarAnnotations = <>
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
    Left = 472
    Top = 432
  end
  object SynBatSyn2: TSynBatSyn
    Left = 296
    Top = 368
  end
end
