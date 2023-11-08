object FormAddComponentFolder: TFormAddComponentFolder
  Left = 0
  Top = 0
  Anchors = [akLeft, akBottom]
  Caption = 'Add Component Folder'
  ClientHeight = 687
  ClientWidth = 830
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 144
  DesignSize = (
    830
    687)
  TextHeight = 21
  object Label1: TLabel
    Left = 12
    Top = 143
    Width = 135
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Component Folder'
  end
  object Label2: TLabel
    Left = 12
    Top = 212
    Width = 111
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Directory Mask'
  end
  object Label3: TLabel
    Left = 348
    Top = 212
    Width = 131
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'File Mask (+.dpk)'
  end
  object Label4: TLabel
    Left = 12
    Top = 284
    Width = 203
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Found and sorted Packages'
  end
  object Label5: TLabel
    Left = 12
    Top = 464
    Width = 129
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akBottom]
    Caption = 'Missing Packages'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 830
    Height = 134
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 820
    object JvHTLabel1: TJvHTLabel
      AlignWithMargins = True
      Left = 12
      Top = 12
      Width = 806
      Height = 110
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Align = alClient
      AutoSize = False
      Caption = 
        'In cases that you want to <b>add a whole directory with a compon' +
        'ent collection </b><br>'#13#10'and multiple packages with a lot of dep' +
        'endencies, you can do that here. <br>'#13#10'Just select the folder, s' +
        'pecify the wildcard for searching the package <br>'#13#10'directories ' +
        '(e.g. *D15.* or just *.*) and the file mask (*D7, *D16, *). <br>' +
        #13#10'The file mask always adds ".dpk" automatically. '
      SuperSubScriptRatio = 0.666666666666666600
      ExplicitWidth = 791
    end
  end
  object edCompFolder: TJvDirectoryEdit
    Left = 12
    Top = 168
    Width = 783
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ButtonWidth = 32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = ''
    ExplicitWidth = 773
  end
  object edDirectoryMask: TEdit
    Left = 12
    Top = 240
    Width = 314
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 2
    Text = '*.*'
  end
  object edFileMask: TEdit
    Left = 348
    Top = 240
    Width = 447
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '*'
    ExplicitWidth = 437
  end
  object lbFound: TListBox
    Left = 12
    Top = 312
    Width = 783
    Height = 141
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 21
    TabOrder = 4
    ExplicitWidth = 773
    ExplicitHeight = 139
  end
  object lbMissing: TListBox
    Left = 12
    Top = 492
    Width = 783
    Height = 122
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 21
    TabOrder = 5
    ExplicitTop = 490
    ExplicitWidth = 773
  end
  object btOk: TButton
    Left = 12
    Top = 642
    Width = 132
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft]
    Caption = 'Ok'
    TabOrder = 6
    OnClick = btOkClick
    ExplicitTop = 640
  end
  object btCancel: TButton
    Left = 153
    Top = 642
    Width = 137
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
    ExplicitTop = 640
  end
  object btFind: TButton
    Left = 648
    Top = 638
    Width = 147
    Height = 37
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Search'
    TabOrder = 8
    OnClick = btFindClick
    ExplicitTop = 636
    ExplicitWidth = 137
  end
  object FileSearcher: TJvSearchFiles
    DirParams.MinSize = 0
    DirParams.MaxSize = 0
    FileParams.SearchTypes = [stFileMask]
    FileParams.MinSize = 0
    FileParams.MaxSize = 0
    OnFindFile = FileSearcherFindFile
    Left = 384
    Top = 40
  end
end
