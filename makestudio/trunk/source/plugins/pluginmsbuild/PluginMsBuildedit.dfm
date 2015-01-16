object FormEditTestcommand: TFormEditTestcommand
  Left = 440
  Top = 305
  BorderStyle = bsDialog
  ClientHeight = 202
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 80
    Height = 13
    Caption = 'Solution path:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 18
    Top = 54
    Width = 72
    Height = 13
    Caption = 'Output path:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 18
    Top = 100
    Width = 72
    Height = 13
    Caption = 'Build config:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 320
    Top = 155
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 430
    Top = 155
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object tbSolutionPath: TJvFilenameEdit
    Left = 18
    Top = 27
    Width = 487
    Height = 21
    Filter = 'Solution (*.sln)|*.sln|All files (*.*)|*.*'
    TabOrder = 2
    Text = ''
  end
  object tbOutputPath: TJvDirectoryEdit
    Left = 18
    Top = 73
    Width = 487
    Height = 21
    DialogKind = dkWin32
    TabOrder = 3
    Text = ''
  end
  object cbCleanup: TCheckBox
    Left = 18
    Top = 159
    Width = 191
    Height = 17
    Caption = 'Clean up before build'
    TabOrder = 4
    Visible = False
  end
  object tbBuildConfig: TEdit
    Left = 18
    Top = 119
    Width = 487
    Height = 21
    TabOrder = 5
  end
end
