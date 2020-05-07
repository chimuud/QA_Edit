object FrmTPG: TFrmTPG
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Give me TPG Jira number and try'
  ClientHeight = 634
  ClientWidth = 1266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1266
    Height = 153
    Align = alTop
    Color = clInfoBk
    ParentBackground = False
    TabOrder = 0
    object Label1: TLabel
      Left = 25
      Top = 58
      Width = 60
      Height = 13
      Caption = 'System Year'
    end
    object Label6: TLabel
      Left = 24
      Top = 85
      Width = 61
      Height = 13
      Caption = 'Record Type'
    end
    object Label7: TLabel
      Left = 38
      Top = 112
      Width = 45
      Height = 13
      Caption = 'Sub Type'
    end
    object Label8: TLabel
      Left = 508
      Top = 31
      Width = 58
      Height = 13
      Caption = 'Primary SSN'
    end
    object Label9: TLabel
      Left = 493
      Top = 58
      Width = 73
      Height = 13
      Caption = 'Secondary SSN'
    end
    object sbtnTogglePSSN: TSpeedButton
      Left = 683
      Top = 27
      Width = 23
      Height = 23
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C34CB1224CB122C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C34CB1224CB122C7C3C8C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C4C3C44CB1224CB122C3C3C3C3C3C3C7
        C3C8C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C4C3C5
        4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
        224CB122C3C3C3C3C3C3C3C3C34CB1224CB1224CB1224CB1224CB1224CB1224C
        B1224CB1224CB1224CB1224CB1224CB1224CB122CCC4D0C3C3C3C5C3C5B8C1B5
        4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
        224CB122D0C5D4C3C3C3C3C3C3C6C3C8C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C2C4C4C3C3C3C3C3C3C3C3C3CC483FCC483FCC483FCC483FCC483FCC
        483FCC483FCC483FCC483FCC483FCC483FCC483FC2C2C2C2C4C4C3C3C3C3C3C3
        CC483FCD342ACD382ECD382ECC483FCC483FCC483FCD382ECD382ECC483FCC43
        3ACD3025CC483FC2C6C6C3C3C3C3C3C3CC483FCC483FCC483FCC483FCC483FCC
        483FCC483FCC483FCC483FCC483FCC483FCC483FC2CFD0C3C3C3C3C3C3C3C3C3
        C2C5C6C2C6C7C2C6C7C2C6C7C2C6C7C2C6C7C2C8C8C3C0C0C3C3C3CB524ACC48
        3FC2CECFC3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C2C4C4C3BABACC483FCC483FC2C5C6C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C2C4C4CC483FC3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3}
    end
    object sbtnRandPSSN: TSpeedButton
      Left = 712
      Top = 27
      Width = 61
      Height = 23
      Caption = 'Random'
    end
    object sbtnToggleSSSN: TSpeedButton
      Left = 683
      Top = 54
      Width = 23
      Height = 23
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C34CB1224CB122C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C34CB1224CB122C7C3C8C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C4C3C44CB1224CB122C3C3C3C3C3C3C7
        C3C8C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C4C3C5
        4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
        224CB122C3C3C3C3C3C3C3C3C34CB1224CB1224CB1224CB1224CB1224CB1224C
        B1224CB1224CB1224CB1224CB1224CB1224CB122CCC4D0C3C3C3C5C3C5B8C1B5
        4CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1224CB1
        224CB122D0C5D4C3C3C3C3C3C3C6C3C8C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C2C4C4C3C3C3C3C3C3C3C3C3CC483FCC483FCC483FCC483FCC483FCC
        483FCC483FCC483FCC483FCC483FCC483FCC483FC2C2C2C2C4C4C3C3C3C3C3C3
        CC483FCD342ACD382ECD382ECC483FCC483FCC483FCD382ECD382ECC483FCC43
        3ACD3025CC483FC2C6C6C3C3C3C3C3C3CC483FCC483FCC483FCC483FCC483FCC
        483FCC483FCC483FCC483FCC483FCC483FCC483FC2CFD0C3C3C3C3C3C3C3C3C3
        C2C5C6C2C6C7C2C6C7C2C6C7C2C6C7C2C6C7C2C8C8C3C0C0C3C3C3CB524ACC48
        3FC2CECFC3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C2C4C4C3BABACC483FCC483FC2C5C6C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C2C4C4CC483FC3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
        C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3}
    end
    object sbtnRandSSSN: TSpeedButton
      Left = 712
      Top = 54
      Width = 61
      Height = 23
      Caption = 'Random'
    end
    object SpeedButton2: TSpeedButton
      Left = 493
      Top = 109
      Width = 91
      Height = 30
      Caption = 'Add EditBox'
    end
    object SpeedButton3: TSpeedButton
      Left = 590
      Top = 109
      Width = 91
      Height = 30
      Caption = 'Add CheckBox'
    end
    object SpeedButton4: TSpeedButton
      Left = 687
      Top = 109
      Width = 86
      Height = 30
      Caption = 'Add ComboBox'
    end
    object sbtnCreate: TSpeedButton
      Left = 792
      Top = 109
      Width = 73
      Height = 30
      Caption = 'Create'
      OnClick = sbtnCreateClick
    end
    object cmbSystemYear: TComboBox
      Left = 91
      Top = 55
      Width = 63
      Height = 21
      TabOrder = 0
    end
    object GroupBox1: TGroupBox
      Left = 176
      Top = 10
      Width = 297
      Height = 129
      Caption = ' Database Data '
      TabOrder = 1
      object Label3: TLabel
        Left = 13
        Top = 21
        Width = 32
        Height = 13
        Caption = 'Server'
      end
      object Label2: TLabel
        Left = 13
        Top = 48
        Width = 76
        Height = 13
        Caption = 'Transmitter Efin'
      end
      object Label4: TLabel
        Left = 24
        Top = 79
        Width = 54
        Height = 13
        Caption = 'Master Efin'
      end
      object Label5: TLabel
        Left = 40
        Top = 104
        Width = 42
        Height = 13
        Caption = 'Office Id'
      end
      object cmbServer: TComboBox
        Left = 115
        Top = 18
        Width = 167
        Height = 21
        TabOrder = 0
        OnChange = cmbServerChange
        Items.Strings = (
          'TPG-SQL-TESTENV\QA,2867'
          'TPG-SQL-TESTENV\DEV,2866')
      end
      object cmbTransEfin: TComboBox
        Left = 115
        Top = 45
        Width = 166
        Height = 21
        TabOrder = 1
        OnChange = cmbTransEfinChange
        OnKeyPress = cmbTransEfinKeyPress
      end
      object cmbMasterEfin: TComboBox
        Left = 115
        Top = 72
        Width = 166
        Height = 21
        TabOrder = 2
        OnChange = cmbMasterEfinChange
        OnKeyPress = cmbMasterEfinKeyPress
      end
      object cmbOfficeId: TComboBox
        Left = 115
        Top = 99
        Width = 166
        Height = 21
        TabOrder = 3
      end
    end
    object cmbRecordType: TComboBox
      Left = 91
      Top = 82
      Width = 64
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnClick = cmbRecordTypeClick
      Items.Strings = (
        'R'
        'Y'
        '6')
    end
    object cmbSubType: TComboBox
      Left = 89
      Top = 109
      Width = 66
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'T'
        'P'
        'I'
        'D'
        'L')
    end
    object edPrimarySSN: TEdit
      Left = 572
      Top = 28
      Width = 105
      Height = 21
      MaxLength = 9
      TabOrder = 4
    end
    object edSecondarySSN: TEdit
      Left = 572
      Top = 55
      Width = 105
      Height = 21
      MaxLength = 9
      TabOrder = 5
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 30
      Width = 89
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Use Templates'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 153
    Width = 1266
    Height = 481
    Align = alClient
    TabOrder = 1
  end
end
