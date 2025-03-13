object FormMain: TFormMain
  Left = 297
  Top = 289
  Width = 423
  Height = 396
  Caption = 'SaveTxt demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    415
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonEditEffect: TButton
    Left = 16
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Edit effect'
    TabOrder = 0
    OnClick = ButtonEditEffectClick
  end
  object MemoTxt: TMemo
    Left = 16
    Top = 48
    Width = 387
    Height = 300
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object ButtonSave: TButton
    Left = 112
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Save to ini'
    TabOrder = 2
    OnClick = ButtonSaveClick
  end
  object ButtonLoad: TButton
    Left = 208
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Load from ini'
    TabOrder = 3
    OnClick = ButtonLoadClick
  end
end
