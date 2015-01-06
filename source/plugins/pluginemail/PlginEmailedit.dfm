object FormEditSendemail: TFormEditSendemail
  Left = 440
  Top = 305
  BorderStyle = bsDialog
  Caption = 'Send email'
  ClientHeight = 293
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 44
    Width = 48
    Height = 13
    Caption = 'Subject:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 70
    Width = 72
    Height = 13
    Caption = 'Email - Text:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 12
    Width = 49
    Height = 13
    Caption = 'Send to:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 275
    Top = 251
    Width = 75
    Height = 27
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 364
    Top = 251
    Width = 75
    Height = 27
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object tbSubject: TEdit
    Left = 81
    Top = 41
    Width = 358
    Height = 21
    TabOrder = 2
    Text = 'Subject...'
  end
  object memoText: TMemo
    Left = 81
    Top = 68
    Width = 358
    Height = 168
    Lines.Strings = (
      '...')
    TabOrder = 3
  end
  object tbSendTo: TEdit
    Left = 81
    Top = 9
    Width = 358
    Height = 21
    TabOrder = 4
  end
  object Button3: TButton
    Left = 81
    Top = 253
    Width = 120
    Height = 25
    Caption = 'Send test email...'
    TabOrder = 5
    OnClick = Button3Click
  end
end
