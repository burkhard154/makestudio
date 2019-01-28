object FormEditDelphi32CheckVersionParams: TFormEditDelphi32CheckVersionParams
  Left = 654
  Top = 292
  Caption = 'Check Delphi Version'
  ClientHeight = 145
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 56
    Width = 127
    Height = 13
    Caption = 'Return Variable (true/false)'
  end
  object Label2: TLabel
    Left = 16
    Top = 8
    Width = 80
    Height = 13
    Caption = 'Delphi32 Version'
  end
  object Button1: TButton
    Left = 16
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 96
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbVersion: TJvImageComboBox
    Left = 16
    Top = 24
    Width = 305
    Height = 23
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    DroppedWidth = 375
    DropDownCount = 20
    ImageHeight = 0
    ImageWidth = 0
    Images = ImageList1
    ItemHeight = 17
    ItemIndex = 0
    TabOrder = 2
    Items = <
      item
        Brush.Style = bsClear
        ImageIndex = 1
        Indent = 0
        Text = 'Delphi 5'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 1
        Indent = 0
        Text = 'Delphi 6'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 2
        Indent = 0
        Text = 'Delphi 7'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 3
        Indent = 0
        Text = 'Delphi 2005'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 3
        Indent = 0
        Text = 'Delphi 2006'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 3
        Indent = 0
        Text = 'Delphi 2007'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 3
        Indent = 0
        Text = 'Delphi 2009'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 3
        Indent = 0
        Text = 'Delphi 2010'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE2'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE3'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE4'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE5'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE6'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE7'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi XE8'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi 10 Seattle'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi 10.1 Berlin'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi 10.2 Tokyo'
      end
      item
        Brush.Style = bsClear
        ImageIndex = 5
        Indent = 0
        Text = 'Delphi 10.3 Rio'
      end>
  end
  object cbVar: TComboBox
    Left = 16
    Top = 72
    Width = 305
    Height = 21
    TabOrder = 3
    Text = 'cbVar'
  end
  object ImageList1: TImageList
    Left = 288
    Top = 112
    Bitmap = {
      494C010106000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000404020000000
      0000000000000000000000000000A4A0A000800000008000000080000000C040
      4000004060000040600000406000004060000000000000000000000000000000
      00005049E727342DE375241CE1BF231BE1BF231BE1BF251DE19F3730E4660000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080004040
      2000404020004040200040402000C0C0C000C0404000C060600080000000F0FB
      FF00F0FBFF000040600040C0E000004060000000000000000000000000002D26
      E2922B23E2FF3032E7FF3B48EFFF4356F4FF4152F2FF3944EEFF2D2BE5FF2C24
      E2E0352EE3550000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      6000A4A0A000C0A0A000C0A0A000C0C0C000C0404000C0606000C04040008000
      00000040600040C0E000004060000040600000000000231BE2102B23E3D02C29
      E5FF4153F3FF4E6AFAFF4E6AFAFF4E6AFAFF4E6AFAFF4E6AFAFF4D69FAFF3B47
      EFFF2923E3FF332BE36400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008060
      6000806060008080600080806000C0C0C000F0CAA600C0C0C000C0606000C040
      400040A0C00000406000F0FBFF0000406000000000002C24E3922A24E5FFE2E6
      FEFFE6EAFEFFE5E9FEFFE5E9FEFFE5E9FEFFE5E9FEFFE5E9FEFFE5E9FEFFE8EB
      FEFF4051F2FF2820E2FF3730E444000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0A0A000A4A0
      A000806060008080600080808000C0C0C00000406000C0C0C00000406000C060
      60008000000080000000F0FBFF00F0CAA600231BE2302A22E3FF3B47EFFF5C76
      FAFF96A7FCFFA2B1FCFFA3B2FCFF9CACFCFFA5B3FCFFA1B0FCFF9EADFCFF778C
      FBFF4E6AFAFF3134E9FF2B23E2E0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0A0
      A000F0FBFF0080606000A4A0A000C0C0C000004060000040600040C0E0000040
      6000C0606000C04040008000000080000000231BE29F271FE3FF4961F7FFCFD7
      FEFFFBFBFFFFE4E8FEFFDCE2FEFFFBFBFFFFD6DDFEFFECF0FEFFFAFAFFFFCDD5
      FEFF4E6AFAFF3E4DF1FF261FE1FF231BE1300000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C0A0
      A00040402000C0A0A000C0A0A000C0DCC0000040600040C0E00000406000C0DC
      C000C0C0C000C0606000C040400080000000231BE2DF261EE2FF4D68F9FF6981
      FBFFF3F4FFFF8598FCFF667EFBFFF3F4FFFF4E6AFAFFA8B5FDFFDDE2FEFF4E6A
      FAFF4E6AFAFF4255F3FF231BE1FF231BE1800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0A0A000C0C0C000C0C0C000F0FBFF0040A0C00040608000406080000040
      6000C0606000C0404000C040400080000000241CE2FF261FE2FF231BE2FF453E
      E6FFEFEEFDFF6D68ECFF4841E7FFEFEFFDFF2B23E3FF9793F1FFD6D4FAFF231B
      E2FF231BE1FF231BE1FF231BE1FF231BE1800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C000C0C0A000C0C0C000C0C0C000F0FBFF00C0DCC000C0DCC000C0C0
      C000C0DCC000C0C0C0008080C0008080A000241CE2FF271FE2FF241CE2FF453F
      E6FFEFEEFDFF6D68ECFF4841E7FFEFEFFDFF2B23E3FF9793F1FFD6D4FAFF231B
      E2FF231BE2FF231BE2FF231BE2FF231BE2800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A4A0A000C0A0A000C0A0A000808080008080800080808000C0C0
      C000C0A08000402060000000800000406000241CE2CF2720E2FF241CE2FF3C35
      E5FFEFEEFDFF645FEBFF3D36E5FFEFEFFDFF231BE2FF908CF0FFD4D2F9FF231B
      E2FF231BE2FF231BE2FF241CE2FF231BE2800000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004020
      600040406000808080008080800080808000C0A0A000C0A0A000C0C0C000A4A0
      A00040208000000080000000800000406000241CE29F2921E3FF241CE2FFEBEA
      FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF261EE2FF231BE2FF271FE3FF231BE2300000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000080004020
      C0000020A0008060800080808000C0A0A000C0C0C000C0A0A000808080000020
      800000008000000080000040600000008000241CE2302C24E3FF261EE2FF241C
      E2FF241CE2FF241CE2FF241CE2FF241CE2FF241CE2FF241CE2FF241CE2FF241C
      E2FF261EE2FF251DE2FF2B23E3E0000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000080000020C0000020
      E000402080000020C000402080004020A0004020A000402080000020A0000020
      A0000020A0004040A0000000800000000000000000002C24E3912A22E3FF332B
      E4FF9995F1FFF5F4FEFFFBFBFEFFF6F6FEFFF8F8FEFFFCFCFFFFB6B3F5FF433C
      E6FF241CE2FF2A22E3FF3028E443000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000080000020E0000020E0008060
      E0000020C0000020C000402080000020C0004040C000402080000020C0004040
      A0000020A00000008000000000000000000000000000000000002A22E3C02A22
      E3FF2820E2FF635DEAFFCECCF9FFFFFFFFFFE7E6FCFF7C77EEFF2D26E3FF271F
      E2FF2A22E3FF2E27E36300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000080008060E0000020
      E0000020E000402080000020C0008060E0000020C0000020C0000020C0000040
      6000000080000000000000000000000000000000000000000000000000002B23
      E3822C24E3FF2B23E3FF4C46E8FF8D89F0FF615BEAFF2820E3FF2B23E3FF2D25
      E3E03129E4430000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80000020C0000020C0000020C0004040C0000000800000406000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000241CE220241CE270241CE2AF241CE2BF241CE2BF241CE29F241CE2600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000002040000000
      0000000000000000000000000000A4A0A0004040400040404000404040004040
      4000404040004040400040404000404040000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000040C0E000006080000060
      8000006080000060800000608000006080000060800000000000000000000000
      0000000000000000000000000000000000000000000000000000002060000080
      8000406080000000000000000000406060000020600000204000002060004020
      6000000000000000000000000000000000000000000000000000406080004060
      800000204000002040008080A00080606000F0CAA600F0CAA600C08060004040
      400040A0C00040C0E00040C0E000404040000000000000000000806060008060
      6000806060008060600080606000806060008060600080606000806060008060
      6000806060000000000000000000000000000000000040C0E00080E0E00080C0
      E0004080C0004060A0004080C0000060A0000060800000000000000000000000
      00000000000000000000000000000000000000000000000000000080A0004060
      A0000020600000206000002040004080E00040C0E00040C0E0000060A0000080
      80004080A0000000000000000000000000000000000000000000000000000040
      A0004060A0004080C00080A0C00080606000F0FBFF00F0CAA600C08060004040
      400040A0C00040C0E00080E0E00040404000000000000000000080606000C080
      6000C08060004040400000000000002060000080C0000060A00040606000C080
      400040404000000000000000000000000000000000000000000040C0E00080C0
      E00040A0E0004080C0004080C000006080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004080
      A0000080A00040C0E00040A0E0000080A0004060A0000060A0000080A0004080
      E000004080000020400000000000000000000000000000000000000000000040
      800000408000004080008080C000806060004040400040404000404040004040
      40004040400040404000404040004040400000000000A4A0A000406060004040
      40004000800080808000806060008060600080C0E00080808000C0806000C080
      600040606000000000000000000000000000000000000000000040C0E00080E0
      E00040C0E00000A0E00000608000006080000000000080202000802020008020
      2000000000000000000000000000000000000000000000000000000000000060
      A0004060A0000080A0000060A0004060A0000080A0004080A0000080E0004080
      A0004080A00000206000002040000020600000000000000000004060C0008080
      A00000206000004080008080C000806060004000E0004000E0004040C0004040
      40008060400040C0800040C08000404040004000A0004000C0004000A0004000
      80004000A000400080004000A0004040400080C0E000A4A0A000C0806000C080
      60004060600000000000000000000000000000000000000000000000000040C0
      E00040C0E000006080004060A00080808000802020008080000080800000C0A0
      600080202000808080000000000000000000000000000000000040A0E000A4A0
      A000004080000060A0004080A0000080E0000080A00000A0E0004080E00040A0
      E0000080A0004040800000204000000000000000000000000000000000004080
      C000F0FBFF000020600080A0C0008060600040A0E0004000E0004040C0004040
      40008060400040C0800040E0A000404040004000C00000000000A4A0A000C080
      6000C0806000C080600040008000400080000040800080606000C0806000C080
      60004060600000000000000000000000000000000000000000000000000040C0
      E00040C0E00000A0E0004060A0008020200080800000C0A06000408000008080
      0000C0A0600080202000000000000000000000000000000000000000000040C0
      E00000000000004080004080E00040A0E0004080E00040A0E00000A0E0004080
      E00040A0E0000060A00000206000000000000000000000000000000000004060
      C000002060004080C00080C0E000A4A0A0008060600080606000806060008060
      6000806060008060600080606000A4A0A0008060E0008060E00080808000C080
      6000C0806000C080600080606000404040008060E00000208000406060008080
      8000406060000000000000000000000000000000000000000000000000000000
      000080E0E00040C0E0000060800040800000C0A060008080000040802000C0A0
      600080800000C0A0600080202000000000000000000000000000000000004080
      E0000060800040C0E00040C0E00040A0E00040C0E00040A0E00040A0E00040A0
      E0004060A0004080A00000206000002040000000000000000000000000000000
      00004060C00080C0E00080C0E000C0DCC00080E0E00080C0E00080A0E00080A0
      E00080A0C00080A0C0008060A0008060A000000000008060E00040404000C080
      6000C0806000C0806000806060004040400080E0E000A4A0A000806060004040
      4000404040000000000000000000000000000000000000000000000000000000
      000040C0E00080E0E00000A0E00000608000C0C06000C0C06000408000004080
      0000C0A060008080000080202000000000000000000000000000000000000000
      00004080E00040E0E00080E0E00080E0E00040E0E000C0C0C00040C0E0000060
      A00040A0E0000080A00000206000002060000000000000000000000000000000
      000080A0C0004080C00040A0E00080C0E00080C0E0004080E0004060C0004060
      A0004060A000402080000000800000206000000000000000000080808000C080
      6000C0806000C0806000806060004040400080A0E00080808000C08060008080
      8000400080008080800000000000000000000000000000000000000000000000
      000040C0E00080E0E00040C0E000406080004080200040800000408000004080
      0000408000008080000080202000000000000000000000000000000000000000
      00004080A00040C0E00040E0E00080E0E00040E0E00040C0E0004060A00040A0
      E00040A0E0004060A00000206000002060000000000000000000000000000000
      0000000000004060A0004060C0004060C0004060C0004060A0004060A00080C0
      E0004060A000000080000000800000406000000000000000000080606000C080
      6000C0806000C0806000806060004040400080A0E00080808000C0806000C080
      6000806060004000A00080808000000000000000000000000000000000000000
      00000000000080E0E00040C0E0000060800080A0C00040800000408000004080
      00004080000080202000C0C0C000000000000000000000000000000000000000
      0000000000004080A0000060A0004080A0004060E0000060A00040A0E00040E0
      E0004080A0000000800000008000004060000000000000000000000000000000
      8000000080004060A0004080A0004080A0004080C00040A0C00080C0E0004060
      C00040208000000080000000800000406000000000000000000080606000C080
      6000C0806000C0806000806060004040400080A0E00080808000C0806000C080
      6000406060008080E0004000C000A4A0A0000000000000000000000000000000
      00000000000040C0E00040C0E00000A0E00000608000C0C06000C0C06000C0C0
      6000802020008080800000000000000000000000000000000000000000000000
      80000000800000A0A00040A0E00040A0E00040C0E00040C0E000C0DCC0004080
      E0000020A0000000800000008000004060000000000000000000000080004020
      C0000020C000402080004060A00040A0C00080C0E0004080C0004040A0004020
      800000008000000080000040600000008000000000000000000080606000C080
      6000C080600040404000404040008060600080C0E0008060600080606000C080
      600040606000000000008080E0004000A0000000000000000000000000000000
      0000000000000000000080E0E00040C0E0004060A00040800000408000004080
      2000C0C0C0000000000000000000000000000000000000000000000080000020
      A0000020A000404080000080A00040C0E00080E0E00040A0E0000040A0000020
      A000000080000020A000004060000000800000000000000080000020C0000020
      E000402080000020C000402080000020A0000020A000402080000020A0000020
      A0000020A000000080000000800000000000000000000000000080606000C080
      6000C08060008060600040404000404040000080C0008060600080606000C080
      6000406060004000C0004000C0004000C0000000000000000000000000000000
      0000000000000000000080E0E00040C0E0004080C00000608000000000000000
      00000000000000000000000000000000000000000000000080000020E0000020
      E000404080000020A000402080000020A0000020A000004080000020A0000020
      A0000020A000000080000000800000000000000080000020E0000020E0008060
      E0000020C0000020C000402080000020C0000020C000402080000020C0000020
      C0000020A000000080000000000000000000000000000000000080606000C080
      6000C0806000C0806000C0806000806000008060600080808000C0806000C080
      600040404000000000008080E000000000000000000040A0E0004080C0004060
      A0004060A0004060A0004080C00000A0E0004080C0004060A000000000000000
      000000000000000000000000000000000000000080000020E0000020E0004060
      E0000020E0000020E000404080000020A0000020E000402060000020E0000020
      A0000020A00000008000000000000000000000000000000080008060E0000020
      E0000020E000402080000020C0008060E0000020C0000020C0000020C0000040
      6000000080000000000000000000000000000000000000000000806060008060
      6000806060008060600080606000806060008060600080606000806060008060
      6000806060000000000000000000000000000000000080E0E00040A0E00040A0
      E00040A0E00040A0E00040C0E00040C0E00040C0E0004080C000000000000000
      00000000000000000000000000000000000000000000000080004060E0000020
      E0000020E000402080000020E0000020E0004060E0000020E0000020A0000040
      6000000080000000000000000000000000000000000000000000000000000000
      80000020C0000020C0000020C0004020C0000000800000406000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080E0E00080E0E00080E0
      E00080E0E00080E0E00080E0E00080E0E00080E0E00040C0E000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80000020A0000020A0000020A0000020A0000000800000406000000080000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00DE00F01F00000000C000E00700000000
      E000800300000000E000800100000000C000000100000000E000000000000000
      E000000000000000F000000000000000F000000000000000F800000000000000
      E000000000000000C00000010000000080018001000000000003C00300000000
      8007E00700000000E01FF01F00000000DE00FFFF807FC60BC000C007807FC003
      E000C007C0FFE001E0008007C08FE000C0000007E003C000E0004007E003E800
      E0000007F001E000F0008007F001F000F000C003F001F000F800C001F801F800
      E000C000F803E000C000C004FC07C0008001C000FC3F80010003C005803F0001
      8007C007803F8007E01FFFFF803FE00F00000000000000000000000000000000
      000000000000}
  end
end
