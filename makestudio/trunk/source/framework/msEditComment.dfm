object FormEditComment: TFormEditComment
  Left = 561
  Top = 236
  Caption = 'Comment'
  ClientHeight = 100
  ClientWidth = 439
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
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 45
    Height = 13
    Caption = 'Comment'
  end
  object btOk: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 96
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edComment: TEdit
    Left = 16
    Top = 32
    Width = 409
    Height = 21
    TabOrder = 2
  end
end
