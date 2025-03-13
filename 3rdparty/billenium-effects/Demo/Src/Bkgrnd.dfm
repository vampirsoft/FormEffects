object FormBkgrnd: TFormBkgrnd
  Left = 231
  Top = 133
  Width = 708
  Height = 527
  Caption = 'FormBkgrnd'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object FormShadowV: TEffectsPanel
    Left = 592
    Top = 128
    Width = 10
    Height = 249
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.GlassTranslucency = 128
  end
  object FormShadowH: TEffectsPanel
    Left = 271
    Top = 367
    Width = 321
    Height = 10
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.GlassTranslucency = 128
  end
  object FormTransitions: TFormTransitions
    BackgroundOptions.PictureMode = fcpmCenter
    Left = 8
    Top = 8
  end
end
