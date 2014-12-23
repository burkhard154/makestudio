object FormAddComponentFolder: TFormAddComponentFolder
  Left = 0
  Top = 0
  Anchors = [akLeft, akBottom]
  Caption = 'Add Component Folder'
  ClientHeight = 458
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    543
    458)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 95
    Width = 88
    Height = 13
    Caption = 'Component Folder'
  end
  object Label2: TLabel
    Left = 8
    Top = 141
    Width = 71
    Height = 13
    Caption = 'Directory Mask'
  end
  object Label3: TLabel
    Left = 232
    Top = 141
    Width = 83
    Height = 13
    Caption = 'File Mask (+.dpk)'
  end
  object Label4: TLabel
    Left = 8
    Top = 189
    Width = 133
    Height = 13
    Caption = 'Found and sorted Packages'
  end
  object Label5: TLabel
    Left = 8
    Top = 309
    Width = 82
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Missing Packages'
    ExplicitTop = 365
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 543
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 579
    object JvHTLabel1: TJvHTLabel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 527
      Height = 73
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      AutoSize = False
      Caption = 
        'In cases that you want to <b>add a whole directory with a compon' +
        'ent collection </b><br>'#13#10'and multiple packages with a lot of dep' +
        'endencies, you can do that here. <br>'#13#10'Just select the folder, s' +
        'pecify the wildcard for searching the package <br>'#13#10'directories ' +
        '(e.g. *D15.* or just *.*) and the file mask (*D7, *D16, *). <br>' +
        #13#10'The file mask always adds ".dpk" automatically. '
      ExplicitTop = 11
      ExplicitWidth = 563
    end
  end
  object edCompFolder: TJvDirectoryEdit
    Left = 8
    Top = 112
    Width = 522
    Height = 21
    DialogKind = dkWin32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 558
  end
  object edDirectoryMask: TEdit
    Left = 8
    Top = 160
    Width = 209
    Height = 21
    TabOrder = 2
    Text = '*.*'
  end
  object edFileMask: TEdit
    Left = 232
    Top = 160
    Width = 298
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = '*'
    ExplicitWidth = 334
  end
  object lbFound: TListBox
    Left = 8
    Top = 208
    Width = 522
    Height = 94
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
    ExplicitWidth = 558
    ExplicitHeight = 88
  end
  object lbMissing: TListBox
    Left = 8
    Top = 328
    Width = 522
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
    ExplicitTop = 322
    ExplicitWidth = 558
  end
  object btOk: TButton
    Left = 8
    Top = 428
    Width = 88
    Height = 25
    Anchors = [akLeft]
    Caption = 'Ok'
    TabOrder = 6
    OnClick = btOkClick
    ExplicitTop = 422
  end
  object btCancel: TButton
    Left = 102
    Top = 428
    Width = 91
    Height = 25
    Anchors = [akLeft]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
    ExplicitTop = 422
  end
  object btFind: TButton
    Left = 432
    Top = 425
    Width = 98
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Search'
    TabOrder = 8
    OnClick = btFindClick
    ExplicitTop = 419
    ExplicitWidth = 134
  end
  object FileSearcher: TJvSearchFiles
    FileParams.SearchTypes = [stFileMask]
    OnFindFile = FileSearcherFindFile
    Left = 384
    Top = 40
  end
end
