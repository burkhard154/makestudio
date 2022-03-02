object FormHelpWorkshopError: TFormHelpWorkshopError
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Help Error'
  ClientHeight = 243
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 196
    Caption = 'Html Help Workshop'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 164
      Width = 151
      Height = 13
      Cursor = crHandPoint
      Caption = 'HTML Help Workshop Download'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label1Click
    end
    object StaticText1: TStaticText
      Left = 16
      Top = 24
      Width = 297
      Height = 118
      AutoSize = False
      Caption = 
        'The help system cannot be compiled because the '#13#10'Microsoft HTML ' +
        'Help Workshop could not be found.'#13#10#13#10'You first have to install t' +
        'he HTML Help Workshop and'#13#10'then restart the program.'#13#10#13#10'To downl' +
        'oad the HTML Help Workshop follow the link below. If you get a m' +
        'essage saying you already a more recent version on your computer' +
        ', ignore it and continue.'
      TabOrder = 0
    end
  end
  object btOk: TButton
    Left = 144
    Top = 210
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
