object FormAbout: TFormAbout
  Left = 467
  Top = 393
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'Product information'
  ClientHeight = 300
  ClientWidth = 300
  Color = clWhite
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
  object Image: TImage
    Left = -84
    Top = 0
    Width = 400
    Height = 300
    Cursor = crHandPoint
    OnClick = TimerTimer
  end
  object LabelVersion: TLabel
    Left = 157
    Top = 85
    Width = 101
    Height = 16
    Alignment = taRightJustify
    Caption = 'Version number'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Visible = False
    OnClick = TimerTimer
  end
  object TransitionList1: TTransitionList
    Left = 16
    Top = 8
    object Transition: TBmpMaskTransition
      AbortOnClick = True
      AbortOnEscape = True
      OnAfterTransition = TransitionAfterTransition
      Milliseconds = 2000
      MaskMode = fcpmZoom
      SmoothingLevel = 2
    end
    object TransitionVersion: TPixelateTransition
      AbortOnClick = True
      AbortOnEscape = True
      OnAfterTransition = TransitionVersionAfterTransition
      Milliseconds = 1500
      BoxSize = 15
    end
    object Transition1: TWipeTransition
      AbortOnClick = True
      AbortOnEscape = True
      Milliseconds = 1000
      BandWidth = 100
      Direction = tedUp
      SmoothBand = True
    end
    object Transition2: TBlendTransition
      Milliseconds = 700
    end
  end
  object FormTransitions: TFormTransitions
    DestroyTransitions = False
    HideTransition = Transition2
    ShowAnimation = Animation1
    ShowTransition = Transition
    Left = 56
    Top = 8
  end
  object Timer: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = TimerTimer
    Left = 96
    Top = 8
  end
  object TEAnimationList1: TTEAnimationList
    Left = 136
    Top = 8
    object Animation1: TTEZoomFrameAnimation
      HidingEnabled = False
      DefaultOrigin = tezoCursor
      GlassTranslucency = 200
      ShowFirstStep = False
      ShowLastStep = False
      MinStepIncrement = 10
      MinStepMilliseconds = 10
    end
  end
end
