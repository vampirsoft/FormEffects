object Form1: TForm1
  Left = 340
  Top = 310
  Width = 452
  Height = 330
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object FormTransitions1: TFormTransitions
    DestroyTransitions = False
    HideAnimation = Animation1
    ShowAnimation = Animation1
    Left = 16
    Top = 16
  end
  object TEAnimationList1: TTEAnimationList
    Left = 56
    Top = 16
    object Animation1: TTEZoomFrameAnimation
      MinStepIncrement = 30
      MinStepMilliseconds = 50
    end
  end
end
