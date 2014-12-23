object FormHelpmergerRun: TFormHelpmergerRun
  Left = 0
  Top = 0
  Width = 489
  Height = 290
  Caption = 'Build Help System'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 254
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 8
    TabOrder = 0
    DesignSize = (
      473
      254)
    object edlog: TMemo
      Left = 8
      Top = 8
      Width = 457
      Height = 200
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object btCancel: TButton
      Left = 8
      Top = 219
      Width = 113
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Cancel'
      TabOrder = 1
      OnClick = btCancelClick
    end
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 24
    Top = 88
  end
end
