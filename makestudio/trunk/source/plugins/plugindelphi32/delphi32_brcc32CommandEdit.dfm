object FormEditbrcc32Command: TFormEditbrcc32Command
  Left = 481
  Top = 346
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Borland Resource Compiler 32 Bit'
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
  object Label3: TLabel
    Left = 16
    Top = 51
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
  object edOptions: TJvEdit
    Left = 16
    Top = 70
    Width = 529
    Height = 21
    TabOrder = 3
  end
  object JvEditor1: TJvEditor
    Left = 16
    Top = 112
    Width = 529
    Height = 193
    Cursor = crIBeam
    BorderStyle = bsNone
    Lines.Strings = (
      '  -r                    (ignored for compatibility)'
      
        '  -16                   Build 16-bit Windows compatible .res fil' +
        'e'
      
        '  -32                 * Build 32-bit Windows compatible .res fil' +
        'e'
      '  -fofilename           Set output filename'
      '  -v                    Verbose'
      '  -ipath                Set include path'
      '  -dname[=string]       Define #define'
      '  -x                    Ignore INCLUDE environment variable'
      '  -m                    Enable multi-byte character support'
      '  -cdddd                set default code page to nnnn'
      '  -lxxxx                set default language to xxxx'
      
        '  -31   Provided for downward compatibility (build 16-bit .res f' +
        'ile)'
      
        '  -w32  Provided for downward compatibility (build 16-bit .res f' +
        'ile)')
    ScrollBars = ssNone
    ReadOnly = True
    Completion.ItemHeight = 13
    Completion.CRLF = '/n'
    Completion.Separator = '='
    TabStops = '3 5'
    BracketHighlighting.StringEscape = #39#39
    Color = 15066597
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Inno Setup (iscc.exe)|iscc.exe'
    Left = 112
    Top = 224
  end
end
