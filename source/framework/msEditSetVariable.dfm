object FormEditSetVariable: TFormEditSetVariable
  Left = 192
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Set Variable'
  ClientHeight = 360
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btOk: TButton
    Left = 8
    Top = 328
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btCancel: TButton
    Left = 88
    Top = 328
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 329
    Height = 313
    ActivePage = tbCommon
    TabOrder = 2
    object tbCommon: TTabSheet
      Caption = 'Common'
      object Label4: TLabel
        Left = 16
        Top = 8
        Width = 38
        Height = 13
        Caption = 'Variable'
      end
      object Label1: TLabel
        Left = 16
        Top = 56
        Width = 64
        Height = 13
        Caption = 'Current Value'
      end
      object lbContent: TLabel
        Left = 16
        Top = 72
        Width = 289
        Height = 49
        AutoSize = False
        Caption = '???'
        Color = clWindow
        ParentColor = False
        WordWrap = True
      end
      object Label3: TLabel
        Left = 16
        Top = 128
        Width = 52
        Height = 13
        Caption = 'New Value'
      end
      object Label2: TLabel
        Left = 16
        Top = 216
        Width = 46
        Height = 13
        Caption = 'Operation'
      end
      object cbVars: TComboBox
        Left = 16
        Top = 24
        Width = 289
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'cbVars'
        OnChange = cbVarsChange
      end
      object edValue: TEdit
        Left = 16
        Top = 144
        Width = 289
        Height = 21
        TabOrder = 1
      end
      object cbAppend: TCheckBox
        Left = 16
        Top = 176
        Width = 289
        Height = 17
        Caption = 'Append on current value'
        TabOrder = 2
      end
      object cbOperation: TComboBox
        Left = 16
        Top = 232
        Width = 289
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 3
        Text = 'None'
        OnChange = cbOperationChange
        Items.Strings = (
          'None'
          'Increment'
          'Decrement'
          'Add trailing backslash'
          'Remove trailing Backslash'
          'Date, Time'
          'Get file version')
      end
      object cbReplaceVars: TCheckBox
        Left = 16
        Top = 192
        Width = 289
        Height = 17
        Caption = 'Replace variables (%VAR%) on execute'
        TabOrder = 4
      end
    end
    object tbDataType: TTabSheet
      Caption = 'Data type'
      ImageIndex = 3
      object rgDataType: TRadioGroup
        Left = 16
        Top = 16
        Width = 289
        Height = 105
        Caption = 'Set variable as'
        ItemIndex = 0
        Items.Strings = (
          'Text'
          'Yes/No'
          'Integer'
          'Floating Point')
        TabOrder = 0
      end
    end
    object tbDateTime: TTabSheet
      Caption = 'Date-Time format'
      ImageIndex = 1
      object Label6: TLabel
        Left = 8
        Top = 8
        Width = 32
        Height = 13
        Caption = 'Format'
      end
      object Label7: TLabel
        Left = 8
        Top = 56
        Width = 265
        Height = 65
        AutoSize = False
        Caption = 
          'Examples'#13#10#13#10'YY = 05...; YYYY = 2005...'#13#10'MM = 01; MMMM = January'#13 +
          #10'HH_MM_SS = 12_30_59...; '
        WordWrap = True
      end
      object edDateTimeFormat: TEdit
        Left = 8
        Top = 24
        Width = 265
        Height = 21
        TabOrder = 0
        Text = 'YYYY-MM-DD HH:MM:SS'
      end
    end
    object tbVersionInfo: TTabSheet
      Caption = 'File Version'
      ImageIndex = 2
      object Label5: TLabel
        Left = 8
        Top = 8
        Width = 32
        Height = 13
        Caption = 'Format'
      end
      object Label8: TLabel
        Left = 8
        Top = 72
        Width = 265
        Height = 65
        AutoSize = False
        Caption = 
          'Place holders'#13#10#13#10'%V1 = Main version'#13#10'%V2 = Second version'#13#10'%V3 =' +
          ' Release'#13#10'%V4 = Build'
        WordWrap = True
      end
      object Label9: TLabel
        Left = 8
        Top = 200
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object cbVersionFormat: TComboBox
        Left = 8
        Top = 24
        Width = 289
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = '%V1.%V2.%V3.%V4'
        Items.Strings = (
          '%V1.%V2.%V3.%V4'
          '%V1_%V2_%V3_%V4'
          '%V1.%V2R%V3 Build %V4')
      end
      object edVersionFilename: TJvFilenameEdit
        Left = 8
        Top = 216
        Width = 297
        Height = 21
        DefaultExt = 'exe'
        Filter = 'Executables (*.exe;*.dll)|*.exe;*.dll'
        TabOrder = 1
      end
    end
  end
end
