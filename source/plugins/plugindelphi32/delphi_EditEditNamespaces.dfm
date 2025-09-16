object FormEditEditNamespaces: TFormEditEditNamespaces
  Left = 440
  Top = 305
  Margins.Left = 2
  Margins.Top = 2
  Margins.Right = 2
  Margins.Bottom = 2
  BorderStyle = bsDialog
  Caption = 'Edit Namespaces'
  ClientHeight = 267
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  PixelsPerInch = 144
  TextHeight = 20
  object Label1: TLabel
    Left = 14
    Top = 128
    Width = 93
    Height = 20
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Namespaces'
  end
  object Label2: TLabel
    Left = 12
    Top = 12
    Width = 446
    Height = 86
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    AutoSize = False
    Caption = 
      'For compatibility reasons starting with Delphi XE2 - you can spe' +
      'cify predefined namespaces here. E.g. "VCL" to compile the units' +
      ' Forms, Graphics etc. if they are not fully referenced in the so' +
      'urce code (like Vcl.Forms or Vcl.Imagin.jepeg.'
    WordWrap = True
  end
  object edNamespaces: TEdit
    Left = 14
    Top = 156
    Width = 457
    Height = 28
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 0
    Text = 'edNamespaces'
  end
  object Button1: TButton
    Left = 12
    Top = 206
    Width = 113
    Height = 37
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 134
    Top = 206
    Width = 112
    Height = 37
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
