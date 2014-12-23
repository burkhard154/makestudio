object FormHelpWorkshopError: TFormHelpWorkshopError
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Help Error'
  ClientHeight = 209
  ClientWidth = 360
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
    Height = 161
    Caption = 'Html Help Workshop'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 128
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
      Height = 105
      AutoSize = False
      Caption = 
        'The help system cannot be compiled because the '#13#10'Microsoft HTML ' +
        'Help Workshop could not be found.'#13#10#13#10'You first have to install t' +
        'he HTML Help Workshop and'#13#10'then restart the program.'#13#10#13#10'You can ' +
        'download the HTML Help Workshop here:'
      TabOrder = 0
    end
  end
  object btOk: TButton
    Left = 128
    Top = 176
    Width = 75
    Height = 25
    Caption = '&Ok'
    TabOrder = 1
  end
end
