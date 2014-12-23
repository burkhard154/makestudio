object FormSelectCommandtypeByExt: TFormSelectCommandtypeByExt
  Left = 0
  Top = 0
  ActiveControl = lvCommands
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Select Command Type'
  ClientHeight = 379
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 376
    Height = 338
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 113
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 0
      object MemoComment: TMemo
        Left = 4
        Top = 4
        Width = 368
        Height = 105
        Align = alClient
        BorderStyle = bsNone
        Lines.Strings = (
          'You have dropped the file '
          '"%s"'
          ''
          
            'The file extension is associated with more than one possible com' +
            'mands. '
          'Please select the command you want to assign it to.')
        TabOrder = 0
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 113
      Width = 376
      Height = 225
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 1
      object lvCommands: TListView
        Left = 4
        Top = 4
        Width = 368
        Height = 217
        Align = alClient
        Columns = <
          item
            Caption = 'Command'
            Width = 150
          end
          item
            Caption = 'Comment'
            Width = 250
          end>
        HideSelection = False
        SmallImages = ImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvCommandsDblClick
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 338
    Width = 376
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btOk: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btCancel: TButton
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
  object ImageList: TImageList
    Left = 40
    Top = 24
  end
end
