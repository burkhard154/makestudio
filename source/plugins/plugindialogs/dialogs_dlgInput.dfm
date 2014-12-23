object FormDlgInput: TFormDlgInput
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FormDlgInput'
  ClientHeight = 211
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbInputText: TLabel
    Left = 16
    Top = 8
    Width = 48
    Height = 13
    Caption = 'InputText'
  end
  object Memo1: TMemo
    Left = 16
    Top = 27
    Width = 481
    Height = 126
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btOk: TButton
    Left = 16
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object btCancel: TButton
    Left = 97
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
