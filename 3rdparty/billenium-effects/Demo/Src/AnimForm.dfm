object AnimatedForm: TAnimatedForm
  Left = 408
  Top = 246
  Width = 467
  Height = 340
  Caption = 'Animated form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 376
    Top = 278
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 0
    Kind = bkClose
  end
  object EffectsPanel1: TEffectsPanel
    Left = 0
    Top = 0
    Width = 459
    Height = 306
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 1
    BackgroundOptions.PictureMode = fcpmCenter
  end
end
