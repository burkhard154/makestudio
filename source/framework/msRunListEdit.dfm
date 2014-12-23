object FormMemo: TFormMemo
  Left = 645
  Top = 465
  Width = 556
  Height = 441
  Caption = 'Sequence (as Text)'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel8: TPanel
    Left = 0
    Top = 0
    Width = 540
    Height = 364
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object Memo: TMemo
      Left = 4
      Top = 4
      Width = 532
      Height = 356
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Memo')
      ParentFont = False
      PopupMenu = PopupMenu1
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 364
    Width = 540
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 80
    Top = 64
    object mnFind: TMenuItem
      Caption = 'Find'
      ShortCut = 16454
      OnClick = mnFindClick
    end
    object mnFindReplace: TMenuItem
      Caption = 'Find and Replace'
      ShortCut = 16456
      OnClick = mnFindReplaceClick
    end
  end
  object JvFindReplace: TJvFindReplace
    EditControl = Memo
    Options = []
    Left = 80
    Top = 104
  end
end
