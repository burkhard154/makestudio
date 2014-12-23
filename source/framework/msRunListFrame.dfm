object FrameModuleList: TFrameModuleList
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  TabStop = True
  object Panel8: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ModuleListBox: TListBox
      Left = 0
      Top = 17
      Width = 320
      Height = 223
      Style = lbOwnerDrawVariable
      Align = alClient
      ItemHeight = 16
      TabOrder = 0
      OnDblClick = ModuleListBoxDblClick
      OnDrawItem = ModuleListBoxDrawItem
      OnMeasureItem = ModuleListBoxMeasureItem
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 320
      Height = 17
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Auftragsliste'
      Color = clAppWorkSpace
      Font.Charset = ANSI_CHARSET
      Font.Color = clHighlightText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object JvDragDrop1: TJvDragDrop
    DropTarget = Owner
    OnDrop = JvDragDrop1Drop
    Left = 40
    Top = 40
  end
end
