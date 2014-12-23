object FormKeywords: TFormKeywords
  Left = 372
  Top = 219
  BorderStyle = bsDialog
  Caption = 'Keywords'
  ClientHeight = 325
  ClientWidth = 377
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
  object Panel1: TPanel
    Left = 281
    Top = 0
    Width = 96
    Height = 325
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Button3: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object Button4: TButton
      Left = 8
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 325
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel3: TPanel
      Left = 0
      Top = 284
      Width = 281
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 88
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button5: TButton
        Left = 168
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Change'
        TabOrder = 2
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 243
      Width = 281
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        281
        41)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 42
        Height = 13
        Caption = 'Keyword'
      end
      object edKeyword: TEdit
        Left = 80
        Top = 8
        Width = 195
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 281
      Height = 243
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 2
      object lbKeywords: TListBox
        Left = 6
        Top = 6
        Width = 269
        Height = 231
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 16
        ParentFont = False
        TabOrder = 0
        OnClick = lbKeywordsClick
      end
    end
  end
end
