object FormUnit: TFormUnit
  Left = 0
  Top = 0
  Caption = 'FormUnit'
  ClientHeight = 116
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object ValueB: TLabel
    Left = 96
    Top = 16
    Width = 105
    Height = 15
    Caption = 'Enter a value below.'
  end
  object Edit1: TEdit
    Left = 8
    Top = 48
    Width = 282
    Height = 23
    TabOrder = 0
  end
  object Button1: TButton
    Left = 64
    Top = 83
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 145
    Top = 83
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
end
