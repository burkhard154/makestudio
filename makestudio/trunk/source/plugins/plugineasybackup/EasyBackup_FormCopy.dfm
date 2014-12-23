object FormDoCopy: TFormDoCopy
  Left = 234
  Top = 122
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Kopieren...'
  ClientHeight = 250
  ClientWidth = 575
  Color = clWindow
  Constraints.MinHeight = 236
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 112
    Width = 45
    Height = 13
    Caption = 'Source:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 192
    Width = 84
    Height = 13
    Caption = 'Number found:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 184
    Top = 192
    Width = 90
    Height = 13
    Caption = 'Number copied:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbFound: TLabel
    Left = 120
    Top = 192
    Width = 3
    Height = 13
  end
  object lbCopied: TLabel
    Left = 272
    Top = 192
    Width = 3
    Height = 13
  end
  object lbFile: TLabel
    Left = 88
    Top = 112
    Width = 473
    Height = 33
    AutoSize = False
    Caption = 'lbFile'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 16
    Top = 176
    Width = 56
    Height = 13
    Caption = 'Directory:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbDir: TLabel
    Left = 88
    Top = 176
    Width = 32
    Height = 13
    Caption = 'Label6'
  end
  object Label2: TLabel
    Left = 16
    Top = 144
    Width = 42
    Height = 13
    Caption = 'Target:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbTarget: TLabel
    Left = 88
    Top = 144
    Width = 473
    Height = 33
    AutoSize = False
    Caption = 'lbFile'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 328
    Top = 192
    Width = 38
    Height = 13
    Caption = 'Errors:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbErrors: TLabel
    Left = 376
    Top = 192
    Width = 3
    Height = 13
  end
  object Label8: TLabel
    Left = 16
    Top = 80
    Width = 41
    Height = 13
    Caption = 'Action:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbAction: TLabel
    Left = 88
    Top = 80
    Width = 473
    Height = 25
    AutoSize = False
    Caption = 'lbAction'
    WordWrap = True
  end
  object Animate1: TAnimate
    Left = 151
    Top = 8
    Width = 272
    Height = 60
    Active = True
    CommonAVI = aviCopyFiles
    StopFrame = 31
  end
  object Button1: TButton
    Left = 257
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 392
    Top = 160
  end
end
