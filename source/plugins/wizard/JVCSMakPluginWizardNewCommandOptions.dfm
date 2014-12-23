object JVCSMakePluginWizardNewCommandForm: TJVCSMakePluginWizardNewCommandForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'JEDI Make New Command Wizard'
  ClientHeight = 323
  ClientWidth = 404
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
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 404
    Height = 103
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 4
      Top = 4
      Width = 396
      Height = 95
      Align = alClient
      Caption = 'Common'
      TabOrder = 0
      ExplicitHeight = 59
      object Label1: TLabel
        Left = 16
        Top = 59
        Width = 43
        Height = 13
        Caption = 'Plugin ID'
      end
      object cbAddMPLHeader: TCheckBox
        Left = 16
        Top = 24
        Width = 209
        Height = 17
        Caption = 'Add MPL Header to source'
        TabOrder = 0
      end
      object edFilesPrefix: TEdit
        Left = 112
        Top = 56
        Width = 249
        Height = 21
        TabOrder = 1
        Text = 'myPlugin'
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 103
    Width = 404
    Height = 177
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 67
    object GroupBox3: TGroupBox
      Left = 4
      Top = 4
      Width = 396
      Height = 169
      Align = alClient
      Caption = 'Command'
      TabOrder = 0
      ExplicitTop = 36
      object Label6: TLabel
        Left = 8
        Top = 20
        Width = 76
        Height = 13
        Caption = 'Command name'
      end
      object Label7: TLabel
        Left = 112
        Top = 40
        Width = 70
        Height = 13
        Caption = 'Klassenpostfix:'
      end
      object lbCommandTypeName: TLabel
        Left = 184
        Top = 40
        Width = 107
        Height = 13
        Caption = 'lbCommandTypeName'
      end
      object Label9: TLabel
        Left = 128
        Top = 56
        Width = 83
        Height = 13
        Caption = 'Kommandoklasse'
      end
      object Label8: TLabel
        Left = 112
        Top = 56
        Width = 9
        Height = 13
        Caption = '->'
      end
      object Label11: TLabel
        Left = 232
        Top = 56
        Width = 38
        Height = 13
        Caption = 'Label11'
      end
      object Label12: TLabel
        Left = 232
        Top = 72
        Width = 38
        Height = 13
        Caption = 'Label12'
      end
      object Label10: TLabel
        Left = 128
        Top = 72
        Width = 94
        Height = 13
        Caption = 'KommandoCallback'
      end
      object lbCSharpNY2: TLabel
        Left = 216
        Top = 92
        Width = 62
        Height = 13
        Caption = 'Sorry not yet!'
        Color = clInfoBk
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clInfoText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Visible = False
      end
      object imgCommando: TImage
        Left = 112
        Top = 92
        Width = 16
        Height = 16
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
        Left = 8
        Top = 92
        Width = 81
        Height = 13
        Caption = 'Command bitmap'
      end
      object edCommandName: TEdit
        Left = 112
        Top = 16
        Width = 249
        Height = 21
        TabOrder = 0
        Text = 'Testmodule'
        OnChange = edCommandNameChange
      end
      object btnLoadCommandoImage: TButton
        Left = 136
        Top = 88
        Width = 75
        Height = 25
        Caption = 'Load'
        TabOrder = 1
        OnClick = btnLoadActionImageClick
      end
      object cbSampleVar: TCheckBox
        Left = 8
        Top = 120
        Width = 153
        Height = 17
        Caption = 'Add sample property'
        TabOrder = 2
      end
      object cbSamplePaintCode: TCheckBox
        Left = 8
        Top = 144
        Width = 305
        Height = 17
        Caption = 'Add sample source code for owner draw behavior'
        TabOrder = 3
      end
    end
  end
  object Cancel: TButton
    Left = 92
    Top = 290
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OK: TButton
    Left = 12
    Top = 290
    Width = 73
    Height = 25
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
