object Form3: TForm3
  Left = 700
  Top = 181
  Caption = 'Form3'
  ClientHeight = 245
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 20
  object ActionList1: TActionList
    Images = ImageList1
    Left = 48
    Top = 24
    object acDelphiSearchPath: TAction
      Caption = 'Edit Delphi Search Path'
      OnExecute = acDelphiSearchPathExecute
    end
    object acSelectDelphiVersion: TAction
      Caption = 'Select default Delphi32 Version...'
      ImageIndex = 4
      OnExecute = acSelectDelphiVersionExecute
    end
    object acBDSProjectDir: TAction
      Caption = 'Edit Borland BDS3 environment settings...'
      ImageIndex = 2
      OnExecute = acBDSProjectDirExecute
    end
    object acAddComponentFolder: TAction
      Caption = 'Add Component Folder'
      OnExecute = acAddComponentFolderExecute
    end
  end
  object ImageList1: TImageList
    Left = 112
    Top = 24
    Bitmap = {
      494C01010B000D00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      00000000000000000000000000000000000000000000000000000C0B0A23322E
      2B91393531A6393431A7393431A7393431A7393431A7363230A512100F6B0000
      00200000000100000000000000000000000000000000000000000A0A0A144F4F
      4F96797979DE808080F8757575DA4A4A4A8F0808080F00000000000000000000
      00000000000000000000000000000000000000000000F0F0F000000000000000
      0000F1F1F100000000000000000000000000000000000000000000000000F5F5
      F500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000040303089A9490F8CBCB
      CBFFCDCDCDFFD3D3D3FFDADADAFFE1E1E1FFE8E8E8FFEAEAEAFF98938FFD9491
      8FE90404034C000000040000000000000000000000002020203F818181F3BDBD
      BDFED9D9D9FFDBDBDBFFC5C5C5FFA1A1A1FE7C7C7CEF1B1B1B35000000000000
      000000000000000000000000000000000000F5F5F50000000000F7F7F7000000
      000000000000FBFBFB00D3D3D300FAFAFA0000000000F4F4F400E8E8E8000000
      0000EEEEEE0000000000FBFBFB00F5F5F5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001513122FB2B2B1FF8181
      81FFB6B6B6FF868686FFC6C6C6FF929292FFDADADAFF878787FF858382FFF7F7
      F7FFB8B6B4F90D0C0B6400000004000000000A0A0A14838383F3E6E6E6FFF0F0
      F0FFE1E1E1FFD3D3D3FFC8C8C8FFC1C1C1FFB3B3B3FF7C7C7CED0707070D0000
      0000000000000000000000000000000000000000000000000000FCFCFC00F6F6
      F600EEEEEE00888888004343430000000000EEEEEE00000000003F3F3F009090
      900000000000F8F8F80000000000FCFCFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230838382FFA8A8
      A8FF646464FFB2B2B2FF6D6D6DFFBFBFBFFFAAAAAAFF979797FFA19F9DFFFEFE
      FEFFF7F7F7FFB4B2B0F90403034B000000014F4F4F96D3D3D3FEFBFBFBFFE7E7
      E7FFDBDBDBFFDADADAFFC8C8C8FFBCBCBCFFCBCBCBFFA3A3A3FE45555EAE0422
      34530106090E000000000000000000000000FEFEFE00FEFEFE0000000000F1F1
      F100999999000E0E0E00D1D1D100EFEFEF000000000000000000BABABA001515
      15008B8B8B00F2F2F20000000000F8F8F8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230AFAFAFFF7C7C
      7CFFABABABFF7E7E7EFFB7B7B7FF888888FFCFCFCFFF8B8B8BFFEBEBEBFFE1DE
      DDFFFEFEFEFFF3F3F3FF928E8BEB00000022797979DEEDEDEDFFF6F6F6FFF2F2
      F2FFD5D5D5FFB6B7B8FFDDDEDEFFCBCCCCFFCDCDCDFFC4C4C4FF758590FD1D70
      A5F00E659BF504243757000000000000000000000000F3F3F30000000000F4F4
      F4005050500010101000F2F2F2000000000000000000FAFAFA00000000000707
      070044444400FDFDFD00EAEAEA00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230C1C1C0FF7E7E
      7EFFBCBCBCFF929292FFCCCCCCFF8D8D8DFFD5D5D5FFA8A8A8FFE9E9E9FFA8A8
      A8FFC9C7C5FF959392FF9F9995FD15131275808080F8EFEFEFFFEAEAEAFFE9E9
      E9FFBABCBCFF4FA0D4FFBABABAFFD8D9D9FFD3D3D3FFD6D6D6FF83898CFF50A2
      D7FF3C91C7FF0E5C8EDD00000000000000000000000000000000F2F2F2000000
      00005555550000000000F9F9F900FCFCFC0000000000F5F5F500E1E1E1001111
      110067676700FEFEFE00F7F7F700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230D2D2D1FF7E7E
      7EFF8F8F8FFFABABABFFA6A6A6FF8A8A8AFFB3B3B3FFB8B8B8FF7F7F7FFFBABA
      BAFF858585FFB8B8B8FF9B9B9BFF46413EB5767676DADBDBDBFFDCDCDCFFDCDC
      DCFFD4D5D5FFBDBEBEFFDFE0E0FFD5D5D5FFDBDBDBFFCFCFCFFF7D8E9AFF4A9E
      D4FF4A9DD4FF0C67A0FF0000000000000000F9F9F90000000000FAFAFA00F8F8
      F80000000000D0D0D0000000000000000000000000000000000000000000C5C5
      C50000000000F6F6F600F3F3F300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230D3D3D2FF8282
      82FFC8C8C8FF8C8C8CFFC1C1C1FF797979FFCECECEFF858585FFAAAAAAFF8888
      88FFAFAFAFFF8C8C8CFFB0B0B0FF494440B84B4B4B8FB6B6B6FED9D9D9FFC3C3
      C3FFD4D5D5FFE3E4E4FFECEDEDFFECECECFFE8E8E8FFADADADFF628EABFF4499
      D1FF4499D1FF0C67A0FF000000000000000000000000FBFBFB00F9F9F9000A0A
      0A009999990000000000F3F3F30000000000FCFCFC00FCFCFC0000000000FAFA
      FA009898980012121200EEEEEE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230E0E0DFFFBDBD
      BDFFE4E4E4FFBABABAFFE2E2E2FFC9C9C9FFE7E7E7FFC7C7C7FFE3E3E3FFB1B1
      B1FFD2D2D2FFB1B1B1FFCACACAFF494440B80808080F7E7E7EEFC8C8C8FFD2D2
      D2FFCECECEFFD9D9D9FFE8E8E8FFF4F4F4FFE0E0E0FF80878CFF3A85B5FF3785
      B7FF3885B8FF0C67A0FF0000000000000000FAFAFA0000000000FDFDFD00F2F2
      F20037373700C9C9C900F4F4F4000000000000000000F7F7F700FBFBFB00DADA
      DA002C2C2C00EFEFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230E5E5E5FF8C8C
      8CFFF1F1F1FF8B8B8BFFB7B7B7FFA0A0A0FF8D8D8DFF959595FFA6A6A6FF7F7F
      7FFFDADADAFF7C7C7CFFD1D1D1FF494440B8000000001F3E508B7D8286FFAEAF
      AFFFD0D0D0FFDDDDDDFFD9D9D9FFBBBBBBFF7C8489FF206B9AFF0C67A0FF0C67
      A0FF0C67A0FF0C67A0FF094972B40000000000000000FCFCFC00F0F0F000FAFA
      FA005E5E5E0037373700FEFEFE00F7F7F70000000000FEFEFE00F9F9F9003838
      38005F5F5F00FCFCFC00F9F9F900F4F4F4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230DFDEDEFF8A8A
      8AFFEEEEEEFF868686FFC0C0C0FF9B9B9BFF949494FF919191FFA5A5A5FF7777
      77FFD8D8D8FF717171FFD0D0D0FF494440B800000000021521341C70A4FD749A
      B3FF81909AFF84898CFF7E8F9BFF6792AEFF499ACFFF4196CFFF3A92CBFF348D
      C7FF2E88C4FF1773AEFF06304A7700000000F1F1F1000000000000000000DEDE
      DE003B3B3B003F3F3F00EAEAEA00FEFEFE000000000000000000ECECEC002121
      210058585800EAEAEA00F9F9F900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230E7E7E6FFE2E2
      E2FFF7F7F7FFDADADAFFF5F5F5FFDCDCDCFFF5F5F5FFD2D2D2FFE5E5E5FFC6C6
      C6FFDBDBDBFFBDBDBDFFD1D1D1FF494440B80C5786D2112F425F0E68A0FD6AB5
      E6FF6FBAEAFF6FBAEAFF6FBAEAFF6FBAEAFF6FBAEAFF6FBAEAFF6CB8E8FF66B3
      E4FF60AEE1FF3886B8FE193647600738568600000000ECECEC00000000000000
      0000949494003E3E3E00D2D2D20000000000F7F7F700F6F6F600F5F5F5002B2B
      2B007E7E7E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016141230B9B9B9FF9797
      97FFD2D2D2FF6E6E6EFFC4C4C4FF8D8D8DFFC2C2C2FF6C6C6CFFD7D7D7FF6868
      68FFBFBFBFFF7E7E7EFF9F9F9FFF494440B80D649BF412699FF86DA4C9F55296
      C2FF5C9DC6FF66A4CBFF70AAD0FF7AB1D5FF83B8D9FF8DBEDEFF97C5E3FF9CC8
      E5FF9CC8E5FF9AC7E4FF3983B1F70D679FFCFEFEFE0000000000F3F3F300FBFB
      FB00E1E1E1009F9F9F0018181800FBFBFB0000000000FBFBFB0017171700A1A1
      A100FDFDFD00F5F5F500FAFAFA00E7E7E7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001513112E9D9D9DFFC4C4
      C4FFC9C9C9FF999999FFBCBCBCFFC0C0C0FFB3B3B3FF949494FFF6F6F6FF9090
      90FFC8C8C8FFA9A9A9FF8D8D8DFF423E3AA7105F91E064AAD7FF71AACFFE1958
      81FF0F466AFF0F4569FF0E4568FF0E4468FF0E4467FF0E4467FF0E4366FF0E43
      65FF0E4265FF458FBDFF6EB0DBFF0F6093E5EDEDED0000000000000000000000
      000000000000E9E9E90000000000FCFCFC00FBFBFB00F7F7F70000000000E5E5
      E500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000020202049B9791EDB0AF
      AFFFE1E1E1FFB5B5B5FFEBEBEBFFAFAFAFFFE0DFDFFFACACABFFE1E1E0FFA8A8
      A8FFE3E3E2FFA2A2A2FFA7A3A0FD14121136093856851A6DA1F42477ABFF347A
      A8FF2C719DFF2E729DFF2F719DFF30729EFF31739EFF32739EFF33749FFF3474
      9EFF34759FFF4790BDFF156A9FF508324C7600000000F6F6F600FBFBFB000000
      0000E9E9E900FDFDFD0000000000F6F6F60000000000E7E7E700000000000000
      0000EEEEEE0000000000F7F7F700F2F2F2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000040403091F1C
      1A44211E1B48211E1B48211E1B48211E1B48211E1B48211E1B48211E1B48211E
      1B48211E1B48211E1B480A090917000000000000000005263B5E135D8CD91A6D
      A1F61B6FA3F81C6FA3F81C70A3F81D70A4F81D71A4F81E71A4F81F72A5F81F72
      A5F82072A5F81B6595E1031B2A4300000000FBFBFB0000000000FBFBFB000000
      00000000000000000000F7F7F70000000000F6F6F60000000000FDFDFD00E7E7
      E70000000000F8F8F800FCFCFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0000000000000000000060A0000080C00080606000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00002060000080
      800040608000FF00FF00FF00FF00406060000020600000204000002060004020
      6000FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00002060000080
      800040608000FF00FF00FF00FF00406060000020600000204000002060004020
      6000FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00002060000080
      8000F0CAA600FF00FF00FF00FF00406060000020600000204000002060004020
      6000FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00A4A0A000404040008080E000A4A0A00080606000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000080A0004060
      A0000020600000206000002040004080E00040C0E00040C0E0000060A0000080
      80004080A00000000000FF00FF00FF00FF00FF00FF00FF00FF000080A0004060
      A0000020600000206000002040004080E00040C0E00040C0E0000060A0000080
      80004080A00000000000FF00FF00FF00FF00FF00FF00FF00FF000080A000C080
      600080402000C0806000002040004080E00040C0E00040C0E0000060A0000080
      80004080A00000000000FF00FF00FF00FF00FF00FF00FF00FF0080808000FF00
      FF00FF00FF00FF00FF00FF00FF004040400000FFFF00A4A0A000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004080
      A0000080A00040C0E00040A0E0000080A0004060A0000060A0000080A0004080
      E000004080000020400000000000FF00FF00FF00FF00FF00FF00FF00FF004080
      A0000080A00040C0E00040A0E0000080A0004060A0000060A0000080A0004080
      E000004080000020400000000000FF00FF00FF00FF00FF00FF00C0A080008040
      2000C0604000C0602000C0A080000080A0004060A0000060A0000080A0004080
      E000004080000020400000000000FF00FF00FF00FF0080808000404040008060
      600080606000FF00FF00FF00FF004040400000FFFF00A4A0A000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000060
      A0004060A0000080A0000060A0004060A0000080A0004080A0000080E0004080
      A0004080A000002060000020400000206000FF00FF00FF00FF00FF00FF000060
      A0004060A0000080A0000060A0004060A0000080A0004080A0000080E0004080
      A0004080A000002060000020400000206000FF00FF00C080600080402000C060
      4000C0804000C0604000C0602000F0CAA6000080A0004080A0000080E0004080
      A0004080A000002060000020400000206000FF00FF00FF00FF00FF00FF008080
      80004040400080606000FF00FF004040400000FFFF00A4A0A000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0040A0E000A4A0
      A000004080000060A0004080A0000080E0000080A00000A0E000406040004020
      20000080A000404080000020400000000000FF00FF00FF00FF0040A0E000A4A0
      A0000040800000600000006000000080E0000080A00000A0E0004080E00040A0
      E0000080A000404080000020400000000000C0604000C0604000C0806000C0A0
      8000C0806000C0A06000C0604000C08060000080A00000A0E0004080E00040A0
      E0000080A000404080000020400000000000FF00FF008060600080606000FF00
      FF00FF00FF00FF00FF00FF00FF004040400000FFFF00A4A0A00080808000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0040C0
      E000FF00FF00004080004080E00040A0E0004080E00040606000404040004060
      400040A0E0000060A000002060000000000040C0A00040A0400040A0400040A0
      4000408040000080000000800000008020004080E00040A0E00000A0E0004080
      E00040A0E0000060A0000020600000000000F0CAA600C0A08000C0A08000C0A0
      8000FF00FF00C0A08000C0A08000C0604000C0A0800040A0E00000A0E0004080
      E00040A0E0000060A0000020600000000000FF00FF00FF00FF00A4A0A0008060
      60000000000080606000FF00FF004040400000FFFF00C0C0C000808080004040
      40008080800080808000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004080
      E0000060800040C0E00040C0E00080606000006060004080A0004080800040A0
      E0004060A0004080A000002060000020400040A0600040C0400040C0400040C0
      400000A0400000A0400000A0200000A000000060000040A0E00040A0E00040A0
      E0004060A0004080A0000020600000204000FF00FF00F0CAA600F0CAA6004080
      E0000060800040C0E000C0A08000C0A06000C0604000F0CAA60040A0E00040A0
      E0004060A0004080A0000020600000204000FF00FF00FF00FF00FF00FF00FF00
      FF0080606000FF00FF00FF00FF004040400000FFFF00A4A0A000FF00FF00FF00
      FF00FF00FF004040400080808000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF004080E00040E0E00080606000C0804000808080004080A000408080000060
      A00040A0E0000080A000002060000020600040A0600040C0600040E0600040C0
      400040C0400040C0400000A0400000A02000008000000060000040C0E0000060
      A00040A0E0000080A0000020600000206000FF00FF00FF00FF00FF00FF00FF00
      FF004080E00040E0E00080E0E000C0A08000C0806000C0804000F0CAA6000060
      A00040A0E0000080A0000020600000206000FF00FF0080606000406060008060
      6000FF00FF00FF00FF00FF00FF004040400000FFFF00A4A0A000A4A0A000A4A0
      A000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF004080A00080606000C0804000C0A06000F0CAA600806080004060A00040A0
      E00040A0E0004060A000002060000020600040C0A00040A0600040A0600040A0
      60000080C00000A0400000C0600000A020000080400040C0E0004060A00040A0
      E00040A0E0004060A0000020600000206000FF00FF00FF00FF00FF00FF00FF00
      FF004080A00040C0E00040E0E00080E0E000C0A08000C0804000C0806000F0CA
      A60040A0E0004060A0000020600000206000FF00FF00FF00FF00FF00FF008060
      600080606000FF00FF00FF00FF004040400000FFFF00A4A0A000A4A0A0008060
      6000A4A0A00080606000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0080606000C0804000C0A06000F0CAA600F0CAA600C080800040A0E00040E0
      E0004080A000000080000000800000406000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0000A0400000A02000008040004060E0000060A00040A0E00040E0
      E0004080A000000080000000800000406000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF004080A0000060A0004080A0004060E000F0CAA600C0804000C080
      6000F0CAA600000080000000800000406000FF00FF0080808000808080008080
      8000FF00FF004020200040006000004080004080E0000080C00080606000FF00
      FF00FF00FF00A4A0A00080808000FF00FF00FF00FF00FF00FF00FF00FF008060
      6000C0804000C0A06000F0CAA600F0CAA600C080800040C0E000C0DCC0004080
      E0000020A000000080000000800000406000FF00FF00FF00FF00FF00FF000000
      8000000080000080200040A0600040A0E00040C0E00040C0E000C0DCC0004080
      E0000020A000000080000000800000406000FF00FF00FF00FF00FF00FF000000
      80000000800000A0A00040A0E00040A0E00040C0E00040C0E00000000000C060
      4000C0804000F0CAA6000000800000406000FF00FF0080808000806060008060
      600040404000FF00FF0040202000A4A0A000A4A0A000A4A0A000402020008080
      8000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080606000C060
      4000C0A06000F0CAA600F0CAA600C080800080E0E00040A0E0000040A0000020
      A000000080000020A0000040600000008000FF00FF00FF00FF00000080000020
      A0000020A000404080000080A00040C0E00080E0E00040A0E0000040A0000020
      A000000080000020A0000040600000008000FF00FF00FF00FF00000080000020
      A0000020A000404080000080A00040C0E00080E0E00040A0E000808060008060
      6000C0806000C08080000040600000008000FF00FF00FF00FF00FF00FF00FF00
      FF008060600040606000402020008060600040202000FF00FF00FF00FF00FF00
      FF008080800040404000FF00FF00FF00FF00FF00FF00000080000020A00040A0
      E000A4A0A000F0CAA600C08080000020A0000020A000004080000020A0000020
      A0000020A0000000800000008000FF00FF00FF00FF00000080000020E0000020
      E000404080000020A000402080000020A0000020A000004080000020A0000020
      A0000020A0000000800000008000FF00FF00FF00FF00000080000020E0000020
      E000404080000020A000402080000020A0000020A000004080000020A0000020
      A000F0CAA600C0A0800000008000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00806060000000000000000000A4A0A0008080
      8000FF00FF00FF00FF00FF00FF00FF00FF00000080000020A0000020E0000020
      E00040A0E000C0808000404080000020A0000020E000402060000020E0000020
      A0000020A0000000800000000000FF00FF00000080000020E0000020E0004060
      E0000020E0000020E000404080000020A0000020E000402060000020E0000020
      A0000020A0000000800000000000FF00FF00000080000020E0000020E0004060
      E0000020E0000020E000404080000020A0000020E000402060000020E0000020
      A0000020A0000000800000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00A4A0A000806060004040
      4000A4A0A000A4A0A000FF00FF00FF00FF00FF00FF000020E0000060E0008080
      E0000020C000402080000020E0000020E0004060E0000020E0000020A0000040
      600000008000FF00FF00FF00FF00FF00FF00FF00FF00000080004060E0000020
      E0000020E000402080000020E0000020E0004060E0000020E0000020A0000040
      600000008000FF00FF00FF00FF00FF00FF00FF00FF00000080004060E0000020
      E0000020E000402080000020E0000020E0004060E0000020E0000020A0000040
      600000008000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00808080008060600080808000FF00FF00FF00FF00FF00FF000020C0000020
      C0000020A0000020A0000020A0000020A0000000800000406000000080000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      80000020A0000020A0000020A0000020A0000000800000406000000080000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      80000020A0000020A0000020A0000020A0000000800000406000000080000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000204000FF00
      FF00FF00FF00FF00FF00FF00FF00A4A0A0004040400040404000404040004040
      400040404000404040004040400040404000FF00FF00FF00FF00002060000080
      800040608000FF00FF00FF00FF00406060000020600000204000002060004020
      6000FF00FF0000000000FF00FF00FF00FF00FF00FF00FF00FF0040402000FF00
      FF00FF00FF00FF00FF00FF00FF00A4A0A000800000008000000080000000C040
      400000406000004060000040600000406000FF00FF00FF00FF0080606000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00C0C0C00040606000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00406080004060
      800000204000002040008080A00080606000F0CAA600F0CAA600C08060004040
      400040A0C00040C0E00040C0E00040404000FF00FF00FF00FF000080A0004060
      A0000020600000206000002040004080E00040C0E00040C0E0000060A0000080
      80004080A00000000000FF00FF00FF00FF00FF00FF00FF00FF00808080004040
      2000404020004040200040402000C0C0C000C0404000C060600080000000F0FB
      FF00F0FBFF000040600040C0E00000406000FF00FF00FF00FF00FF00FF004060
      6000FFFFFF00F0FBFF00F0FBFF00F0FBFF00C0C0C00040606000406060004060
      600040606000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000040
      A0004060A0004080C00080A0C00080606000F0FBFF00F0CAA600C08060004040
      400040A0C00040C0E00080E0E00040404000FF00FF00FF00FF00FF00FF004080
      A0000080A00040C0E00040A0E0000080A0004060A0000060A0000080A0004080
      E000004080000020400000000000FF00FF00FF00FF00FF00FF00FF00FF008080
      6000A4A0A000C0A0A000C0A0A000C0C0C000C0404000C0606000C04040008000
      00000040600040C0E0000040600000406000FF00FF00FF00FF00FF00FF00FF00
      FF0040606000FFFFFF00F0FBFF0040606000F0FBFF00C0C0C000C0C0C000C0C0
      C000C0C0C00040606000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000040
      800000408000004080008080C000806060004040400040404000404040004040
      400040404000404040004040400040404000FF00FF00FF00FF00FF00FF000060
      A0004060A0000080A0000060A0004060A0000080A0004080A0000080E0004080
      A0004080A000002060000020400000206000FF00FF00FF00FF00FF00FF008060
      6000806060008080600080806000C0C0C000F0CAA600C0C0C000C0606000C040
      400040A0C00000406000F0FBFF0000406000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF0040606000FFFFFF00C0DCC000C0DCC000C0DCC000C0DCC000FFFF
      FF00FFFFFF00C0DCC00040606000FF00FF00FF00FF00FF00FF004060C0008080
      A00000206000004080008080C000806060004000E0004000E0004040C0004040
      40008060400040C0800040C0800040404000FF00FF00FF00FF0040A0E000A4A0
      A000004080000060A0004080A0000080E0000080A00000A0E0004080E00040A0
      E0000080A000404080000020400000000000FF00FF00FF00FF00C0A0A000A4A0
      A000806060008080600080808000C0C0C00000406000C0C0C00000406000C060
      60008000000080000000F0FBFF00F0CAA600FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0080606000FFFFFF00C0DCC000C0DCC000C0C0C0004060
      600040606000FFFFFF0040606000FF00FF00FF00FF00FF00FF00FF00FF004080
      C000F0FBFF000020600080A0C0008060600040A0E0004000E0004040C0004040
      40008060400040C0800040E0A00040404000FF00FF00FF00FF00FF00FF0040C0
      E000FF00FF00004080004080E00040A0E0004080E00040A0E00000A0E0004080
      E00040A0E0000060A0000020600000000000FF00FF00FF00FF00FF00FF00C0A0
      A000F0FBFF0080606000A4A0A000C0C0C000004060000040600040C0E0000040
      6000C0606000C04040008000000080000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0080606000FFFFFF00C0DCC000C0C0C00040606000FF00
      FF00C0C0C0004060600040606000FF00FF00FF00FF00FF00FF00FF00FF004060
      C000002060004080C00080C0E000A4A0A0008060600080606000806060008060
      6000806060008060600080606000A4A0A000FF00FF00FF00FF00FF00FF004080
      E0000060800040C0E00040C0E00040A0E00040C0E00040A0E00040A0E00040A0
      E0004060A0004080A0000020600000204000FF00FF00FF00FF00FF00FF00C0A0
      A00040402000C0A0A000C0A0A000C0DCC0000040600040C0E00000406000C0DC
      C000C0C0C000C0606000C040400080000000FF00FF00F0FBFF0040606000F0FB
      FF00FF00FF00FF00FF0080606000FFFFFF00C0DCC00040606000FF00FF00FF00
      FF00FF00FF00FF00FF0040606000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF004060C00080C0E00080C0E000C0DCC00080E0E00080C0E00080A0E00080A0
      E00080A0C00080A0C0008060A0008060A000FF00FF00FF00FF00FF00FF00FF00
      FF004080E00040E0E00080E0E00080E0E00040E0E000C0C0C00040C0E0000060
      A00040A0E0000080A0000020600000206000FF00FF00FF00FF00FF00FF00FF00
      FF00C0A0A000C0C0C000C0C0C000F0FBFF0040A0C00040608000406080000040
      6000C0606000C0404000C040400080000000F0FBFF0040606000406060004060
      6000F0FBFF00FF00FF0080606000FFFFFF00C0DCC00040606000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF0080A0C0004080C00040A0E00080C0E00080C0E0004080E0004060C0004060
      A0004060A000402080000000800000206000FF00FF00FF00FF00FF00FF00FF00
      FF004080A00040C0E00040E0E00080E0E00040E0E00040C0E0004060A00040A0
      E00040A0E0004060A0000020600000206000FF00FF00FF00FF00FF00FF00FF00
      FF00C0C0C000C0C0A000C0C0C000C0C0C000F0FBFF00C0DCC000C0DCC000C0C0
      C000C0DCC000C0C0C0008080C0008080A0004060600040606000FF00FF004060
      600040606000F0FBFF00FF00FF0080606000FFFFFF00C0DCC00040606000C0C0
      C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF004060A0004060C0004060C0004060C0004060A0004060A00080C0
      E0004060A000000080000000800000406000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF004080A0000060A0004080A0004060E0000060A00040A0E00040E0
      E0004080A000000080000000800000406000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00A4A0A000C0A0A000C0A0A000808080008080800080808000C0C0
      C000C0A08000402060000000800000406000FF00FF00FF00FF00FF00FF00FF00
      FF004060600040606000F0FBFF00FF00FF008060600080606000806060004060
      6000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      8000000080004060A0004080A0004080A0004080C00040A0C00080C0E0004060
      C00040208000000080000000800000406000FF00FF00FF00FF00FF00FF000000
      80000000800000A0A00040A0E00040A0E00040C0E00040C0E000C0DCC0004080
      E0000020A000000080000000800000406000FF00FF00FF00FF00FF00FF004020
      600040406000808080008080800080808000C0A0A000C0A0A000C0C0C000A4A0
      A00040208000000080000000800000406000FF00FF00FF00FF0040606000F0FB
      FF00FFFFFF004060600040606000F0FBFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000080004020
      C0000020C000402080004060A00040A0C00080C0E0004080C0004040A0004020
      800000008000000080000040600000008000FF00FF00FF00FF00000080000020
      A0000020A000404080000080A00040C0E00080E0E00040A0E0000040A0000020
      A000000080000020A0000040600000008000FF00FF00FF00FF00000080004020
      C0000020A0008060800080808000C0A0A000C0C0C000C0A0A000808080000020
      800000008000000080000040600000008000F0FBFF0040606000406060004060
      6000F0FBFF00FFFFFF0040606000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000080000020C0000020
      E000402080000020C000402080000020A0000020A000402080000020A0000020
      A0000020A0000000800000008000FF00FF00FF00FF00000080000020E0000020
      E000404080000020A000402080000020A0000020A000004080000020A0000020
      A0000020A0000000800000008000FF00FF00FF00FF00000080000020C0000020
      E000402080000020C000402080004020A0004020A000402080000020A0000020
      A0000020A0004040A00000008000FF00FF004060600040606000FF00FF004060
      600040606000F0FBFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00000080000020E0000020E0008060
      E0000020C0000020C000402080000020C0000020C000402080000020C0000020
      C0000020A00000008000FF00FF00FF00FF00000080000020E0000020E0004060
      E0000020E0000020E000404080000020A0000020E000402060000020E0000020
      A0000020A0000000800000000000FF00FF00000080000020E0000020E0008060
      E0000020C0000020C000402080000020C0004040C000402080000020C0004040
      A0000020A00000008000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF004060600040606000F0FBFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000080008060E0000020
      E0000020E000402080000020C0008060E0000020C0000020C0000020C0000040
      600000008000FF00FF00FF00FF00FF00FF00FF00FF00000080004060E0000020
      E0000020E000402080000020E0000020E0004060E0000020E0000020A0000040
      600000008000FF00FF00FF00FF00FF00FF00FF00FF00000080008060E0000020
      E0000020E000402080000020C0008060E0000020C0000020C0000020C0000040
      600000008000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF004060600040606000F0FBFF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      80000020C0000020C0000020C0004020C000000080000040600000008000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      80000020A0000020A0000020A0000020A0000000800000406000000080000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      80000020C0000020C0000020C0004040C000000080000040600000008000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0040606000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000B7EF00000000000058940000
      00000000C14A00000000000020C2000000000000A1A1000000000000D0810000
      0000000043E10000000000008521000000000000418300000000000080800000
      0000000060C1000000000000B10700000000000040800000000000007A2F0000
      0000000092B40000000000005D49000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
end
