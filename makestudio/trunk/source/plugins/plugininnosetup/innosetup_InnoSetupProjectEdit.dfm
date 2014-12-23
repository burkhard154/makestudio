object FormEditInnoSetupProjectParams: TFormEditInnoSetupProjectParams
  Left = 481
  Top = 346
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Inno Setup Project'
  ClientHeight = 356
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 74
    Height = 13
    Caption = 'Output filename'
  end
  object Label3: TLabel
    Left = 16
    Top = 88
    Width = 83
    Height = 13
    Caption = 'Additional options'
  end
  object Button1: TButton
    Left = 16
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 96
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilename: TJvFilenameEdit
    Left = 16
    Top = 24
    Width = 529
    Height = 21
    TabOrder = 2
  end
  object edOutputfilename: TJvFilenameEdit
    Left = 16
    Top = 64
    Width = 529
    Height = 21
    TabOrder = 3
  end
  object edOptions: TJvEdit
    Left = 16
    Top = 104
    Width = 529
    Height = 21
    Modified = False
    TabOrder = 4
  end
  object JvEditor1: TJvEditor
    Left = 16
    Top = 136
    Width = 529
    Height = 169
    Cursor = crIBeam
    BorderStyle = bsNone
    Lines.Strings = (
      ' definition    emulates #define directive:'
      
        '                 /d<name>[=<value>]   #define public <name> <val' +
        'ue>'
      ''
      ' option        emulates #pragma directive:'
      
        '                 /$<letter>(+|-)      #pragma option -<letter>(+' +
        '|-)'
      
        '                 /p<letter>(+|-)      #pragma parseroption -<let' +
        'ter>(+|-)'
      '                 /i<paths>            #pragma include <paths>'
      
        '                 /s<string>           #pragma inlinestart <strin' +
        'g>'
      '                 /e<string>           #pragma inlineend <string>'
      
        '                 /v<number>           #pragma verboselevel <numb' +
        'er>')
    ScrollBars = ssNone
    GutterWidth = 0
    RightMarginColor = clSilver
    ReadOnly = True
    Completion.ItemHeight = 13
    Completion.Interval = 800
    Completion.ListBoxStyle = lbStandard
    Completion.CaretChar = '|'
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    SelForeColor = clHighlightText
    SelBackColor = clHighlight
    Color = 15066597
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    TabStop = True
    UseDockManager = False
  end
  object btCompiler: TButton
    Left = 176
    Top = 320
    Width = 137
    Height = 25
    Caption = 'Inno Setup Compiler...'
    TabOrder = 6
    OnClick = btCompilerClick
  end
  object btEdit: TButton
    Left = 320
    Top = 320
    Width = 89
    Height = 25
    Caption = 'Edit Project...'
    TabOrder = 7
    OnClick = btEditClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Inno Setup (iscc.exe)|iscc.exe'
    Left = 72
    Top = 248
  end
end
