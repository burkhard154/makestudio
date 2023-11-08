object FormCompilerSwitches: TFormCompilerSwitches
  Left = 419
  Top = 277
  BorderStyle = bsSizeToolWin
  Caption = 'Compiler switches'
  ClientHeight = 293
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 144
  TextHeight = 21
  object lb: TListBox
    Left = 0
    Top = 0
    Width = 536
    Height = 231
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    ItemHeight = 21
    Items.Strings = (
      'Allgemeine Optionen'
      '--------------------------------------------'
      '  -A<unit>=<alias> = set Unit-Alias '
      '  -B = Build all Units '
      '  -CC = Target: Console'
      '  -CG = Target: GUI'
      '  -D<syms> = define conditions'
      '  -E<path> = output directory'
      '  -F<offset> = debugging'
      '  -GD = Detailled Map-File'
      '  -GP = Map-File with publics'
      '  -GS = Map-File with Segments'
      '  -H = output hints'
      '  -I<paths> = Include-directory'
      '  -J = generate .obj-files'
      '  -JP = generate C++-.obj-files'
      '  -K<addr> = Image-Basise adress'
      '  -LU<package> = use Package'
      '  -M = create modified Units'
      '  -N<path> = DCU-output directory'
      '  -NS<namespaces> = Namespace-search path'
      '  -O<paths> = object directories'
      '  -P = search for 8.3-file names'
      '  -Q = (Quiet)'
      '  -R<paths> = Resource directories'
      '  -U<paths> = Unit-directories'
      '  -V = Debug-Information in EXE'
      '  -VR = External Debugging'
      '  -W = output warnings'
      '  --depends = output Unit-dependency information'
      '')
    TabOrder = 0
    OnClick = lbClick
    ExplicitWidth = 522
    ExplicitHeight = 214
  end
  object Panel1: TPanel
    Left = 0
    Top = 231
    Width = 536
    Height = 62
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 214
    ExplicitWidth = 522
    object Button1: TButton
      Left = 12
      Top = 12
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
      Left = 132
      Top = 12
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
