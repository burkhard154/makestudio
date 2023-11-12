object FormEditbrcc32Command: TFormEditbrcc32Command
  Left = 481
  Top = 346
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Borland Resource Compiler 32 Bit'
  ClientHeight = 712
  ClientWidth = 1163
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 192
  TextHeight = 26
  object Label1: TLabel
    Left = 32
    Top = 16
    Width = 84
    Height = 26
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Filename'
  end
  object Label3: TLabel
    Left = 32
    Top = 103
    Width = 166
    Height = 26
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Additional options'
  end
  object Button1: TButton
    Left = 32
    Top = 640
    Width = 151
    Height = 51
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 192
    Top = 640
    Width = 151
    Height = 51
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object edFilename: TJvFilenameEdit
    Left = 32
    Top = 48
    Width = 1059
    Height = 34
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    ButtonWidth = 64
    TabOrder = 2
    Text = ''
  end
  object edOptions: TJvEdit
    Left = 32
    Top = 140
    Width = 1059
    Height = 34
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    TabOrder = 3
    Text = ''
  end
  object JvEditor1: TJvEditor
    Left = 32
    Top = 224
    Width = 1059
    Height = 387
    Cursor = crIBeam
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
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
    Font.Height = -23
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
