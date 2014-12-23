object FormEditZipCommand: TFormEditZipCommand
  Left = 440
  Top = 305
  Caption = 'Zip Command'
  ClientHeight = 503
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    386
    503)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 30
    Height = 13
    Caption = 'Action'
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 36
    Height = 13
    Caption = 'ZIP File'
  end
  object lbParams: TLabel
    Left = 16
    Top = 168
    Width = 43
    Height = 13
    Caption = 'lbParams'
  end
  object Button1: TButton
    Left = 16
    Top = 472
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 96
    Top = 472
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cbAction: TComboBox
    Left = 16
    Top = 24
    Width = 361
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = cbActionChange
  end
  object edZIPFilename: TJvFilenameEdit
    Left = 16
    Top = 72
    Width = 361
    Height = 21
    AddQuotes = False
    DefaultExt = 'zip'
    Filter = 'ZIPl files (*.zip)|*.zip|All files (*.*)|*.*'
    DialogOptions = []
    TabOrder = 3
    Text = ''
  end
  object edParams: TMemo
    Left = 16
    Top = 184
    Width = 360
    Height = 233
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
    ExplicitWidth = 361
  end
  object cbVerbose: TCheckBox
    Left = 16
    Top = 424
    Width = 201
    Height = 17
    Caption = 'Verbose Zip Messages'
    TabOrder = 5
  end
  object cbTrace: TCheckBox
    Left = 16
    Top = 440
    Width = 201
    Height = 17
    Caption = 'Trace ZIP Messages'
    TabOrder = 6
  end
  object rgZipUpdate: TRadioGroup
    Left = 16
    Top = 104
    Width = 361
    Height = 57
    Caption = 'Zip file handling'
    ItemIndex = 0
    Items.Strings = (
      'Always create a new Zip file'
      'If Zip file exists - just update new or modified files')
    TabOrder = 7
  end
end
