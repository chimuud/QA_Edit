object FrmDemo: TFrmDemo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FrmDemo'
  ClientHeight = 469
  ClientWidth = 789
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelDemo: TPanel
    Left = 0
    Top = 0
    Width = 789
    Height = 469
    Align = alClient
    TabOrder = 0
  end
  object GridDemo: TStringGrid
    Left = 24
    Top = 24
    Width = 320
    Height = 345
    TabOrder = 1
  end
  object XML: TXMLDocument
    Left = 712
    Top = 400
  end
end
