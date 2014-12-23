object FormCompilerSwitches: TFormCompilerSwitches
  Left = 419
  Top = 277
  Width = 379
  Height = 251
  BorderStyle = bsSizeToolWin
  Caption = 'Compiler switches'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lb: TListBox
    Left = 0
    Top = 0
    Width = 363
    Height = 174
    Align = alClient
    ItemHeight = 13
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
  end
  object Panel1: TPanel
    Left = 0
    Top = 174
    Width = 363
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
