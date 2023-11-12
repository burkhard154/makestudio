object FormEditEditNamespaces: TFormEditEditNamespaces
  Left = 440
  Top = 305
  BorderStyle = bsDialog
  Caption = 'Edit Namespaces'
  ClientHeight = 356
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  PixelsPerInch = 192
  TextHeight = 26
  object Label1: TLabel
    Left = 19
    Top = 171
    Width = 124
    Height = 26
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Namespaces'
  end
  object Label2: TLabel
    Left = 16
    Top = 16
    Width = 595
    Height = 115
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    AutoSize = False
    Caption = 
      'For compatibility reasons starting with Delphi XE2 - you can spe' +
      'cify predefined namespaces here. E.g. "VCL" to compile the units' +
      ' Forms, Graphics etc. if they are not fully referenced in the so' +
      'urce code (like Vcl.Forms or Vcl.Imagin.jepeg.'
    WordWrap = True
  end
  object edNamespaces: TEdit
    Left = 19
    Top = 208
    Width = 609
    Height = 34
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    TabOrder = 0
    Text = 'edNamespaces'
  end
  object Button1: TButton
    Left = 16
    Top = 275
    Width = 151
    Height = 49
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 179
    Top = 275
    Width = 149
    Height = 49
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
