object msPluginWizardOptionsForm: TmsPluginWizardOptionsForm
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'JEDI VCS Make Plugin Wizard'
  ClientHeight = 543
  ClientWidth = 473
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 543
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      473
      543)
    object Label1: TLabel
      Left = 20
      Top = 25
      Width = 53
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Plugin ID'
    end
    object Label2: TLabel
      Left = 20
      Top = 54
      Width = 37
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Name'
    end
    object Label3: TLabel
      Left = 20
      Top = 84
      Width = 38
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Author'
    end
    object Label4: TLabel
      Left = 20
      Top = 113
      Width = 23
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Hint'
    end
    object Label5: TLabel
      Left = 20
      Top = 143
      Width = 55
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Category'
    end
    object lbMenuActionPath: TLabel
      Left = 20
      Top = 212
      Width = 62
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Menu path'
    end
    object Label6: TLabel
      Left = 20
      Top = 310
      Width = 62
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Command'
    end
    object Label7: TLabel
      Left = 148
      Top = 335
      Width = 78
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Class postfix:'
    end
    object lbCommandTypeName: TLabel
      Left = 236
      Top = 335
      Width = 142
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'lbCommandTypeName'
    end
    object Label8: TLabel
      Left = 148
      Top = 354
      Width = 11
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '->'
    end
    object Label9: TLabel
      Left = 167
      Top = 354
      Width = 97
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Command class'
    end
    object Label10: TLabel
      Left = 167
      Top = 374
      Width = 116
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Command callback'
    end
    object Label11: TLabel
      Left = 295
      Top = 354
      Width = 48
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Label11'
    end
    object Label12: TLabel
      Left = 295
      Top = 374
      Width = 48
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Label12'
    end
    object lbTestActionCaption: TLabel
      Left = 20
      Top = 241
      Width = 46
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Caption'
    end
    object imgCommando: TImage
      Left = 148
      Top = 399
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
    object imgAction: TImage
      Left = 148
      Top = 271
      Width = 16
      Height = 16
      Hint = 'This image will be copied into the target directory'
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = True
      Picture.Data = {
        07544269746D617036040000424D360400000000000036000000280000001000
        0000100000000100200000000000000400000000000000000000000000000000
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000A70D800FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00076DD400197EE400076DD400076D
        D400FF00FF00FF00FF0018CEF60019CBF700FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00076DD4002A94F3002BACF900076DD400FF00
        FF00FF00FF00FF00FF00FF00FF0018CEF60019CBF700FF00FF00FF00FF00076D
        D400076DD400076DD400076DD4002790F0002CA4F900076DD400FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000A70D8000E73
        DB002790F0004FCBF3004FCBF300238BEE00076DD400FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0010635D001176DE002CA4
        F9001F86E9002E9EF8002BACF9002E9EF800076DD400FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF000B8813001A9A2A0010635D002790
        F0004FCBF300197EE4001F86E9002D98F600076DD400FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF000674080012911E004EB47A002EBB52001063
        5D001F86E9002E9EF8001176DE001C82E700076DD400FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF000A790D0033C25D001FA3340037C266001291
        1E0011635C00197EE4002D98F6000E73DB00076DD400FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF000A790D0033C25D001C9D2E000B881300057F
        08000279030011635C001176DE00076DD400FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF000A790D002AB749001C9D2E0006810B00037B
        0500027903000279030011635C00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0012911E000E8B170009851000037B0500037B
        0500016E0100016C0100FF00FF0018CEF60019CBF700FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF001C9D2E0033C25D000674080001690200016902000169
        020001690200FF00FF00FF00FF00FF00FF0018CEF60019CBF700FF00FF00FF00
        FF00FF00FF00137C160033C25D000985100001690200FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00026B0300036F05000277020001690200FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF0015952200026B0300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00}
      Transparent = True
    end
    object Label13: TLabel
      Left = 20
      Top = 399
      Width = 83
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Image (16x16)'
    end
    object lbimgAction: TLabel
      Left = 20
      Top = 271
      Width = 83
      Height = 16
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Image (16x16)'
    end
    object edPluginName: TEdit
      Left = 148
      Top = 49
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 1
      Text = 'Plugin Template'
    end
    object edPluginAuthor: TEdit
      Left = 148
      Top = 79
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
      Text = 'Burkhard Schranz (burkhard.schranz@optimeas.de)'
    end
    object edPluginHint: TEdit
      Left = 148
      Top = 108
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 3
      Text = 'Plugin for <>'
    end
    object edPluginCategory: TEdit
      Left = 148
      Top = 138
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 4
      Text = 'None'
    end
    object edFilesPrefix: TEdit
      Left = 148
      Top = 20
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
      Text = 'myPlugin'
    end
    object edMenuActionPath: TEdit
      Left = 148
      Top = 207
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 6
      Text = 'Testplugin\TestItem\'
    end
    object edCommandName: TEdit
      Left = 148
      Top = 305
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 9
      Text = 'Testcommand'
      OnChange = edCommandNameChange
    end
    object cbMenuAction: TCheckBox
      Left = 20
      Top = 177
      Width = 168
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add a menu action'
      TabOrder = 5
      OnClick = cbMenuActionClick
    end
    object cbSampleVar: TCheckBox
      Left = 20
      Top = 433
      Width = 434
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add Sample Variables'
      TabOrder = 11
    end
    object cbSamplePaintCode: TCheckBox
      Left = 20
      Top = 463
      Width = 414
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add Sample code for owner draw commands'
      TabOrder = 12
    end
    object OK: TButton
      Left = 143
      Top = 496
      Width = 90
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 13
    end
    object Cancel: TButton
      Left = 241
      Top = 496
      Width = 90
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 14
    end
    object edTestActionCaption: TEdit
      Left = 148
      Top = 236
      Width = 306
      Height = 24
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 7
      Text = 'Menu caption'
    end
    object btnLoadCommandoImage: TButton
      Left = 177
      Top = 394
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Load'
      TabOrder = 10
      OnClick = btnLoadActionImageClick
    end
    object btnLoadActionImage: TButton
      Left = 177
      Top = 266
      Width = 93
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Load'
      TabOrder = 8
      OnClick = btnLoadActionImageClick
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 248
    Top = 136
  end
end
