object FormEditDelphi32Globals: TFormEditDelphi32Globals
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Delphi 32 Settings'
  ClientHeight = 448
  ClientWidth = 525
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
    Width = 525
    Height = 448
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object JvPageControl1: TJvPageControl
      Left = 6
      Top = 6
      Width = 513
      Height = 395
      ActivePage = tabCommon
      Align = alClient
      Images = ImageList1
      TabOrder = 0
      TabPainter = JvTabDefaultPainter1
      object tabCommon: TTabSheet
        Caption = 'Commom'
        ImageIndex = -1
        object GroupBox1: TGroupBox
          Left = 0
          Top = 0
          Width = 505
          Height = 161
          Align = alTop
          Caption = 'Actual Delphi Version'
          TabOrder = 0
          object cbVer: TJvImageComboBox
            Left = 16
            Top = 24
            Width = 473
            Height = 23
            Style = csOwnerDrawVariable
            ButtonStyle = fsLighter
            DroppedWidth = 697
            ImageHeight = 0
            ImageWidth = 0
            Images = ImageList1
            ItemHeight = 17
            ItemIndex = 3
            TabOrder = 0
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
                ImageIndex = 0
                Indent = 0
                Text = 'Delphi 2005'
              end
              item
                Brush.Style = bsClear
                ImageIndex = 0
                Indent = 0
                Text = 'Delphi 2006'
              end>
          end
          object d5: TCheckBox
            Left = 16
            Top = 64
            Width = 97
            Height = 17
            Caption = 'Delphi 5'
            Enabled = False
            TabOrder = 1
          end
          object d6: TCheckBox
            Left = 16
            Top = 80
            Width = 97
            Height = 17
            Caption = 'Delphi 6'
            Enabled = False
            TabOrder = 2
          end
          object d7: TCheckBox
            Left = 16
            Top = 96
            Width = 97
            Height = 17
            Caption = 'Delphi 7'
            Enabled = False
            TabOrder = 3
          end
          object d2005: TCheckBox
            Left = 16
            Top = 112
            Width = 97
            Height = 17
            Caption = 'Delphi 2005'
            Enabled = False
            TabOrder = 4
          end
          object d2006: TCheckBox
            Left = 16
            Top = 128
            Width = 97
            Height = 17
            Caption = 'Delphi 2006'
            Enabled = False
            TabOrder = 5
          end
        end
      end
      object tabD5: TTabSheet
        Caption = 'Delphi 5'
        ImageIndex = 1
        object Label1: TLabel
          Left = 16
          Top = 16
          Width = 58
          Height = 13
          Caption = 'Search path'
        end
        object Edit2: TEdit
          Left = 16
          Top = 32
          Width = 441
          Height = 21
          TabOrder = 0
          Text = 'Edit1'
        end
        object Button2: TButton
          Left = 464
          Top = 32
          Width = 25
          Height = 21
          Caption = '...'
          TabOrder = 1
        end
        object Panel5: TPanel
          Left = 0
          Top = 194
          Width = 505
          Height = 172
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 2
          object GroupBox3: TGroupBox
            Left = 4
            Top = 4
            Width = 497
            Height = 164
            Align = alClient
            Caption = 'Info'
            TabOrder = 0
            object Panel6: TPanel
              Left = 2
              Top = 15
              Width = 493
              Height = 147
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 4
              TabOrder = 0
              object Memo1: TMemo
                Left = 4
                Top = 4
                Width = 485
                Height = 139
                Align = alClient
                BevelEdges = []
                BevelInner = bvNone
                BevelOuter = bvNone
                BorderStyle = bsNone
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Lines.Strings = (
                  'Root directory: %s'
                  'BPL directory: %s'
                  'DCP directory: %s'
                  'Registry Key: %s'
                  ''
                  'Search path:')
                ParentFont = False
                ReadOnly = True
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
          end
        end
      end
      object tabD7: TTabSheet
        Caption = 'Delphi 7'
        ImageIndex = 2
        object Label2: TLabel
          Left = 16
          Top = 16
          Width = 58
          Height = 13
          Caption = 'Search path'
        end
        object Edit1: TEdit
          Left = 16
          Top = 32
          Width = 441
          Height = 21
          TabOrder = 0
          Text = 'Edit1'
        end
        object Button1: TButton
          Left = 464
          Top = 32
          Width = 25
          Height = 21
          Caption = '...'
          TabOrder = 1
        end
        object Panel2: TPanel
          Left = 0
          Top = 194
          Width = 505
          Height = 172
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 2
          object GroupBox2: TGroupBox
            Left = 4
            Top = 4
            Width = 497
            Height = 164
            Align = alClient
            Caption = 'Info'
            TabOrder = 0
            object Panel4: TPanel
              Left = 2
              Top = 15
              Width = 493
              Height = 147
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 4
              TabOrder = 0
              object Memo: TMemo
                Left = 4
                Top = 4
                Width = 485
                Height = 139
                Align = alClient
                BevelEdges = []
                BevelInner = bvNone
                BevelOuter = bvNone
                BorderStyle = bsNone
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Lines.Strings = (
                  'Root directory: %s'
                  'BPL directory: %s'
                  'DCP directory: %s'
                  'Registry Key: %s'
                  ''
                  'Search path:')
                ParentFont = False
                ReadOnly = True
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
          end
        end
      end
      object tabD2005: TTabSheet
        Caption = 'Delphi 2005'
        object Label3: TLabel
          Left = 16
          Top = 16
          Width = 58
          Height = 13
          Caption = 'Search path'
        end
        object Label5: TLabel
          Left = 16
          Top = 56
          Width = 64
          Height = 13
          Caption = 'BPL Directory'
        end
        object Label6: TLabel
          Left = 16
          Top = 96
          Width = 67
          Height = 13
          Caption = 'DCP Directory'
        end
        object Label7: TLabel
          Left = 16
          Top = 136
          Width = 72
          Height = 13
          Caption = 'BDS Project Dir'
        end
        object Edit3: TEdit
          Left = 16
          Top = 32
          Width = 441
          Height = 21
          TabOrder = 0
          Text = 'Edit1'
        end
        object Button3: TButton
          Left = 464
          Top = 32
          Width = 25
          Height = 21
          Caption = '...'
          TabOrder = 1
        end
        object Panel7: TPanel
          Left = 0
          Top = 194
          Width = 505
          Height = 172
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 2
          object GroupBox4: TGroupBox
            Left = 4
            Top = 4
            Width = 497
            Height = 164
            Align = alClient
            Caption = 'Info'
            TabOrder = 0
            object Panel8: TPanel
              Left = 2
              Top = 15
              Width = 493
              Height = 147
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 4
              TabOrder = 0
              object Memo2: TMemo
                Left = 4
                Top = 4
                Width = 485
                Height = 139
                Align = alClient
                BevelEdges = []
                BevelInner = bvNone
                BevelOuter = bvNone
                BorderStyle = bsNone
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Lines.Strings = (
                  'Root directory: %s'
                  'BPL directory: %s'
                  'DCP directory: %s'
                  'Registry Key: %s'
                  ''
                  'Search path:')
                ParentFont = False
                ReadOnly = True
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
          end
        end
        object JvDirectoryEdit1: TJvDirectoryEdit
          Left = 16
          Top = 72
          Width = 473
          Height = 21
          DialogKind = dkWin32
          TabOrder = 3
          Text = 'JvDirectoryEdit1'
        end
        object JvDirectoryEdit2: TJvDirectoryEdit
          Left = 16
          Top = 112
          Width = 473
          Height = 21
          DialogKind = dkWin32
          TabOrder = 4
          Text = 'JvDirectoryEdit1'
        end
        object JvDirectoryEdit3: TJvDirectoryEdit
          Left = 16
          Top = 152
          Width = 473
          Height = 21
          DialogKind = dkWin32
          TabOrder = 5
          Text = 'JvDirectoryEdit1'
        end
      end
      object tabD2006: TTabSheet
        Caption = 'Delphi 2006'
        object Label4: TLabel
          Left = 16
          Top = 16
          Width = 58
          Height = 13
          Caption = 'Search path'
        end
        object Label8: TLabel
          Left = 16
          Top = 56
          Width = 64
          Height = 13
          Caption = 'BPL Directory'
        end
        object Label9: TLabel
          Left = 16
          Top = 96
          Width = 67
          Height = 13
          Caption = 'DCP Directory'
        end
        object Label10: TLabel
          Left = 16
          Top = 136
          Width = 72
          Height = 13
          Caption = 'BDS Project Dir'
        end
        object Edit4: TEdit
          Left = 16
          Top = 32
          Width = 441
          Height = 21
          TabOrder = 0
          Text = 'Edit1'
        end
        object Button4: TButton
          Left = 464
          Top = 32
          Width = 25
          Height = 21
          Caption = '...'
          TabOrder = 1
        end
        object Panel9: TPanel
          Left = 0
          Top = 194
          Width = 505
          Height = 172
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 2
          object GroupBox5: TGroupBox
            Left = 4
            Top = 4
            Width = 497
            Height = 164
            Align = alClient
            Caption = 'Info'
            TabOrder = 0
            object Panel10: TPanel
              Left = 2
              Top = 15
              Width = 493
              Height = 147
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 4
              TabOrder = 0
              object Memo3: TMemo
                Left = 4
                Top = 4
                Width = 485
                Height = 139
                Align = alClient
                BevelEdges = []
                BevelInner = bvNone
                BevelOuter = bvNone
                BorderStyle = bsNone
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                Lines.Strings = (
                  'Root directory: %s'
                  'BPL directory: %s'
                  'DCP directory: %s'
                  'Registry Key: %s'
                  ''
                  'Search path:')
                ParentFont = False
                ReadOnly = True
                ScrollBars = ssBoth
                TabOrder = 0
              end
            end
          end
        end
        object JvDirectoryEdit4: TJvDirectoryEdit
          Left = 16
          Top = 72
          Width = 473
          Height = 21
          DialogKind = dkWin32
          TabOrder = 3
          Text = 'JvDirectoryEdit1'
        end
        object JvDirectoryEdit5: TJvDirectoryEdit
          Left = 16
          Top = 112
          Width = 473
          Height = 21
          DialogKind = dkWin32
          TabOrder = 4
          Text = 'JvDirectoryEdit1'
        end
        object JvDirectoryEdit6: TJvDirectoryEdit
          Left = 16
          Top = 152
          Width = 473
          Height = 21
          DialogKind = dkWin32
          TabOrder = 5
          Text = 'JvDirectoryEdit1'
        end
      end
      object tabD2007: TTabSheet
        Caption = 'Delphi 2007'
      end
    end
    object Panel3: TPanel
      Left = 6
      Top = 401
      Width = 513
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
        TabOrder = 0
      end
      object btCancel: TButton
        Left = 88
        Top = 8
        Width = 75
        Height = 25
        Caption = '&Cancel'
        TabOrder = 1
      end
    end
  end
  object JvTabDefaultPainter1: TJvTabDefaultPainter
    ActiveFont.Charset = DEFAULT_CHARSET
    ActiveFont.Color = clHighlight
    ActiveFont.Height = -11
    ActiveFont.Name = 'Tahoma'
    ActiveFont.Style = [fsBold]
    InactiveFont.Charset = DEFAULT_CHARSET
    InactiveFont.Color = clWindowText
    InactiveFont.Height = -11
    InactiveFont.Name = 'Tahoma'
    InactiveFont.Style = []
    DisabledFont.Charset = DEFAULT_CHARSET
    DisabledFont.Color = clGrayText
    DisabledFont.Height = -11
    DisabledFont.Name = 'Tahoma'
    DisabledFont.Style = []
    Left = 168
    Top = 96
  end
  object ImageList1: TImageList
    Left = 136
    Top = 96
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000002060000080
      8000406080000000000000000000406060000020600000204000002060004020
      6000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000040C0E000006080000060
      8000006080000060800000608000006080000060800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000080A0004060
      A0000020600000206000002040004080E00040C0E00040C0E0000060A0000080
      80004080A0000000000000000000000000000000000000000000806060008060
      6000806060008060600080606000806060008060600080606000806060008060
      6000806060000000000000000000000000000000000040C0E00080E0E00080C0
      E0004080C0004060A0004080C0000060A0000060800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004080
      A0000080A00040C0E00040A0E0000080A0004060A0000060A0000080A0004080
      E00000408000002040000000000000000000000000000000000080606000C080
      6000C08060004040400000000000002060000080C0000060A00040606000C080
      400040404000000000000000000000000000000000000000000040C0E00080C0
      E00040A0E0004080C0004080C000006080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000060
      A0004060A0000080A0000060A0004060A0000080A0004080A0000080E0004080
      A0004080A00000206000002040000020600000000000A4A0A000406060004040
      40004000800080808000806060008060600080C0E00080808000C0806000C080
      600040606000000000000000000000000000000000000000000040C0E00080E0
      E00040C0E00000A0E00000608000006080000000000080202000802020008020
      2000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000040A0E000A4A0
      A000004080000060A0004080A0000080E0000080A00000A0E0004080E00040A0
      E0000080A0004040800000204000000000004000A0004000C0004000A0004000
      80004000A000400080004000A0004040400080C0E000A4A0A000C0806000C080
      60004060600000000000000000000000000000000000000000000000000040C0
      E00040C0E000006080004060A00080808000802020008080000080800000C0A0
      6000802020008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000040C0
      E00000000000004080004080E00040A0E0004080E00040A0E00000A0E0004080
      E00040A0E0000060A00000206000000000004000C00000000000A4A0A000C080
      6000C0806000C080600040008000400080000040800080606000C0806000C080
      60004060600000000000000000000000000000000000000000000000000040C0
      E00040C0E00000A0E0004060A0008020200080800000C0A06000408000008080
      0000C0A060008020200000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004080
      E0000060800040C0E00040C0E00040A0E00040C0E00040A0E00040A0E00040A0
      E0004060A0004080A00000206000002040008060E0008060E00080808000C080
      6000C0806000C080600080606000404040008060E00000208000406060008080
      8000406060000000000000000000000000000000000000000000000000000000
      000080E0E00040C0E0000060800040800000C0A060008080000040802000C0A0
      600080800000C0A0600080202000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004080E00040E0E00080E0E00080E0E00040E0E000C0C0C00040C0E0000060
      A00040A0E0000080A0000020600000206000000000008060E00040404000C080
      6000C0806000C0806000806060004040400080E0E000A4A0A000806060004040
      4000404040000000000000000000000000000000000000000000000000000000
      000040C0E00080E0E00000A0E00000608000C0C06000C0C06000408000004080
      0000C0A060008080000080202000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004080A00040C0E00040E0E00080E0E00040E0E00040C0E0004060A00040A0
      E00040A0E0004060A0000020600000206000000000000000000080808000C080
      6000C0806000C0806000806060004040400080A0E00080808000C08060008080
      8000400080008080800000000000000000000000000000000000000000000000
      000040C0E00080E0E00040C0E000406080004080200040800000408000004080
      0000408000008080000080202000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004080A0000060A0004080A0004060E0000060A00040A0E00040E0
      E0004080A000000080000000800000406000000000000000000080606000C080
      6000C0806000C0806000806060004040400080A0E00080808000C0806000C080
      6000806060004000A00080808000000000000000000000000000000000000000
      00000000000080E0E00040C0E0000060800080A0C00040800000408000004080
      00004080000080202000C0C0C000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80000000800000A0A00040A0E00040A0E00040C0E00040C0E000C0DCC0004080
      E0000020A000000080000000800000406000000000000000000080606000C080
      6000C0806000C0806000806060004040400080A0E00080808000C0806000C080
      6000406060008080E0004000C000A4A0A0000000000000000000000000000000
      00000000000040C0E00040C0E00000A0E00000608000C0C06000C0C06000C0C0
      6000802020008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000080000020
      A0000020A000404080000080A00040C0E00080E0E00040A0E0000040A0000020
      A000000080000020A0000040600000008000000000000000000080606000C080
      6000C080600040404000404040008060600080C0E0008060600080606000C080
      600040606000000000008080E0004000A0000000000000000000000000000000
      0000000000000000000080E0E00040C0E0004060A00040800000408000004080
      2000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000080000020E0000020
      E000404080000020A000402080000020A0000020A000004080000020A0000020
      A0000020A000000080000000800000000000000000000000000080606000C080
      6000C08060008060600040404000404040000080C0008060600080606000C080
      6000406060004000C0004000C0004000C0000000000000000000000000000000
      0000000000000000000080E0E00040C0E0004080C00000608000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000080000020E0000020E0004060
      E0000020E0000020E000404080000020A0000020E000402060000020E0000020
      A0000020A000000080000000000000000000000000000000000080606000C080
      6000C0806000C0806000C0806000806000008060600080808000C0806000C080
      600040404000000000008080E000000000000000000040A0E0004080C0004060
      A0004060A0004060A0004080C00000A0E0004080C0004060A000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000080004060E0000020
      E0000020E000402080000020E0000020E0004060E0000020E0000020A0000040
      6000000080000000000000000000000000000000000000000000806060008060
      6000806060008060600080606000806060008060600080606000806060008060
      6000806060000000000000000000000000000000000080E0E00040A0E00040A0
      E00040A0E00040A0E00040C0E00040C0E00040C0E0004080C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      80000020A0000020A0000020A0000020A0000000800000406000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080E0E00080E0E00080E0
      E00080E0E00080E0E00080E0E00080E0E00080E0E00040C0E000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00C60BFFFF807F0000C003C007807F0000
      E001C007C0FF0000E0008007C08F0000C0000007E0030000E8004007E0030000
      E0000007F0010000F0008007F0010000F000C003F0010000F800C001F8010000
      E000C000F8030000C000C004FC0700008001C000FC3F00000001C005803F0000
      8007C007803F0000E00FFFFF803F000000000000000000000000000000000000
      000000000000}
  end
end
