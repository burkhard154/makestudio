object msPluginWizardNewCommandForm: TmsPluginWizardNewCommandForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'MakeStudio New Command Wizard'
  ClientHeight = 398
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 127
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 4
      Top = 4
      Width = 489
      Height = 119
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Caption = 'Common'
      TabOrder = 0
      object Label1: TLabel
        Left = 20
        Top = 73
        Width = 53
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Plugin ID'
      end
      object cbAddMPLHeader: TCheckBox
        Left = 20
        Top = 30
        Width = 257
        Height = 20
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Add MPL Header to source'
        TabOrder = 0
      end
      object edFilesPrefix: TEdit
        Left = 138
        Top = 69
        Width = 306
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 1
        Text = 'myPlugin'
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 127
    Width = 497
    Height = 218
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object GroupBox3: TGroupBox
      Left = 4
      Top = 4
      Width = 489
      Height = 210
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Caption = 'Command'
      TabOrder = 0
      object Label6: TLabel
        Left = 10
        Top = 25
        Width = 99
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Command name'
      end
      object Label7: TLabel
        Left = 138
        Top = 49
        Width = 89
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Klassenpostfix:'
      end
      object lbCommandTypeName: TLabel
        Left = 226
        Top = 49
        Width = 142
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'lbCommandTypeName'
      end
      object Label9: TLabel
        Left = 158
        Top = 69
        Width = 109
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Kommandoklasse'
      end
      object Label8: TLabel
        Left = 138
        Top = 69
        Width = 11
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '->'
      end
      object Label11: TLabel
        Left = 286
        Top = 69
        Width = 48
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Label11'
      end
      object Label12: TLabel
        Left = 286
        Top = 89
        Width = 48
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Label12'
      end
      object Label10: TLabel
        Left = 158
        Top = 89
        Width = 122
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'KommandoCallback'
      end
      object lbCSharpNY2: TLabel
        Left = 266
        Top = 113
        Width = 77
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Sorry not yet!'
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clInfoText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Visible = False
      end
      object imgCommando: TImage
        Left = 138
        Top = 113
        Width = 16
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        AutoSize = True
        Picture.Data = {
          07544269746D617036040000424D360400000000000036000000280000001000
          0000100000000100200000000000000400000000000000000000000000000000
          0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000268D0000268D000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF000267CF00056BD2000369D100076DD4002A8FF1003A9FF8000268
          D000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF000B71D7000E74D9002A8FF1003A9FF8003A9FF800258BED000268D000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF000268D0003A9FF8002287E9003A9FF8003A9FF800369BF7000268D000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF000268D0003A9FF800197EE2002086E8003398F6000268D000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000268D000369BF7001177DC001C81E5000268D000FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00898D8C005555
          5600FF00FF00FF00FF00FF00FF000268D0003398F6000E74D9000268D000FF00
          FF00FF00FF00FF00FF00FF00FF000C851300016D0100898D8C0055555600FF00
          FF00898D8C00898D8C00FF00FF00FF00FF000268D0000268D000FF00FF00FF00
          FF00FF00FF00FF00FF0006730900178A2300629F6F00016D0100FF00FF00898D
          8C0055555600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000B740F003E9E4C002C9239004E9E5C00016D01005555
          5600FF00FF00898D8C0055555600FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000B740F003E9E4C001E8E2B000C85130005820600016D
          0100898D8C0055555600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF000B740F003E9E4C001E8E2B0007800900027B0300037F
          0400016D0100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00178A23000D8616000A830F00037F0400027B0300016F
          0200016D0100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF001E8E2B003E9E4C0005710700016D0100016D0100016D0100016D
          0100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0012871B003E9E4C0009820D00016D0100FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF000470060002770300016D0100FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00}
        Transparent = True
      end
      object Label13: TLabel
        Left = 10
        Top = 113
        Width = 106
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Command bitmap'
      end
      object edCommandName: TEdit
        Left = 138
        Top = 20
        Width = 306
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
        Text = 'Testmodule'
        OnChange = edCommandNameChange
      end
      object btnLoadCommandoImage: TButton
        Left = 167
        Top = 108
        Width = 93
        Height = 31
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Load'
        TabOrder = 1
        OnClick = btnLoadActionImageClick
      end
      object cbSampleVar: TCheckBox
        Left = 10
        Top = 148
        Width = 188
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Add sample property'
        TabOrder = 2
      end
      object cbSamplePaintCode: TCheckBox
        Left = 10
        Top = 177
        Width = 375
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Add sample source code for owner draw behavior'
        TabOrder = 3
      end
    end
  end
  object Cancel: TButton
    Left = 113
    Top = 357
    Width = 90
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OK: TButton
    Left = 15
    Top = 357
    Width = 90
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 304
    Top = 56
  end
end
