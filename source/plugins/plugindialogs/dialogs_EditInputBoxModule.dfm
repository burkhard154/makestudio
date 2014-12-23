object FormInputBoxModuleEdit: TFormInputBoxModuleEdit
  Left = 534
  Top = 201
  BorderStyle = bsDialog
  Caption = 'Input box - Properties'
  ClientHeight = 503
  ClientWidth = 551
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object Label2: TLabel
    Left = 10
    Top = 79
    Width = 26
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Text'
  end
  object Label8: TLabel
    Left = 10
    Top = 332
    Width = 51
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Variable'
  end
  object btnInsertTextVar: TJvSpeedButton
    Left = 423
    Top = 327
    Width = 57
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Text'
    Flat = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    Glyph.Data = {
      B6000000424DB600000000000000360000002800000005000000080000000100
      1800000000008000000000000000000000000000000000000000FF00FF000000
      FF00FFFF00FFFF00FF00000000FF00FFFF00FFFF00FFFF00FF00000000000000
      FF00FFFF00FFFF00FF00000000000000000000FF00FF00000000FF00FF000000
      00000000000000000000FF00FFFF00FF00000000000000000000FF00FF000000
      00000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FF00}
    ParentFont = False
    OnClick = btnInsertTextVarClick
  end
  object Label5: TLabel
    Left = 12
    Top = 394
    Width = 130
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Save Input in Variable'
  end
  object Label1: TLabel
    Left = 10
    Top = 30
    Width = 46
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Caption'
  end
  object Label3: TLabel
    Left = 10
    Top = 276
    Width = 78
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Default value'
  end
  object Label4: TLabel
    Left = 352
    Top = 332
    Width = 56
    Height = 16
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'insert into'
  end
  object btnInsertDefaultVar: TJvSpeedButton
    Left = 482
    Top = 327
    Width = 57
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Default'
    Flat = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = []
    Glyph.Data = {
      B6000000424DB600000000000000360000002800000005000000080000000100
      1800000000008000000000000000000000000000000000000000FF00FF000000
      FF00FFFF00FFFF00FF00000000FF00FFFF00FFFF00FFFF00FF00000000000000
      FF00FFFF00FFFF00FF00000000000000000000FF00FF00000000FF00FF000000
      00000000000000000000FF00FFFF00FF00000000000000000000FF00FF000000
      00000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FF00}
    ParentFont = False
    OnClick = btnInsertDefaultVarClick
  end
  object Panel3: TPanel
    Left = 5
    Top = 5
    Width = 538
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Content'
    Color = clGray
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object edtText: TMemo
    Left = 10
    Top = 98
    Width = 533
    Height = 175
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    MaxLength = 1024
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object cmbVarList: TComboBox
    Left = 68
    Top = 330
    Width = 278
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 4
    Top = 369
    Width = 538
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Options'
    Color = clGray
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
  end
  object cmbReturnInput: TComboBox
    Left = 10
    Top = 414
    Width = 257
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    MaxLength = 256
    TabOrder = 4
  end
  object btnBoxTest: TButton
    Left = 286
    Top = 409
    Width = 257
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Test &Box'
    TabOrder = 7
    OnClick = btnBoxTestClick
  end
  object btnOk: TButton
    Left = 10
    Top = 463
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 108
    Top = 463
    Width = 93
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object edtCaption: TEdit
    Left = 10
    Top = 49
    Width = 533
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    MaxLength = 256
    TabOrder = 0
  end
  object edtDefault: TEdit
    Left = 10
    Top = 295
    Width = 533
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    MaxLength = 256
    TabOrder = 2
  end
end
