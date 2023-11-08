object FormEditbrcc32Command: TFormEditbrcc32Command
  Left = 481
  Top = 346
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Borland Resource Compiler 32 Bit'
  ClientHeight = 534
  ClientWidth = 858
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 144
  TextHeight = 20
  object Label1: TLabel
    Left = 24
    Top = 12
    Width = 65
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Filename'
  end
  object Label3: TLabel
    Left = 24
    Top = 77
    Width = 126
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Additional options'
  end
  object Button1: TButton
    Left = 24
    Top = 480
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 144
    Top = 480
    Width = 113
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilename: TJvFilenameEdit
    Left = 24
    Top = 36
    Width = 794
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ButtonWidth = 32
    TabOrder = 2
    Text = ''
  end
  object edOptions: TJvEdit
    Left = 24
    Top = 105
    Width = 794
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 3
    Text = ''
  end
  object JvEditor1: TJvEditor
    Left = 24
    Top = 168
    Width = 794
    Height = 290
    Cursor = crIBeam
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
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
    Font.Height = -17
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
