object FormEditEditNamespaces: TFormEditEditNamespaces
  Left = 440
  Top = 305
  BorderStyle = bsDialog
  Caption = 'Edit Namespaces'
  ClientHeight = 178
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 85
    Width = 62
    Height = 13
    Caption = 'Namespaces'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 297
    Height = 57
    AutoSize = False
    Caption = 
      'For compatibility reasons starting with Delphi XE2 - you can spe' +
      'cify predefined namespaces here. E.g. "VCL" to compile the units' +
      ' Forms, Graphics etc. if they are not fully referenced in the so' +
      'urce code (like Vcl.Forms or Vcl.Imagin.jepeg.'
    WordWrap = True
  end
  object edNamespaces: TEdit
    Left = 9
    Top = 104
    Width = 305
    Height = 21
    TabOrder = 0
    Text = 'edNamespaces'
  end
  object Button1: TButton
    Left = 8
    Top = 137
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 89
    Top = 137
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
