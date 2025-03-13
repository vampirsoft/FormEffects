object FormTransitionSamples: TFormTransitionSamples
  Left = 261
  Top = 210
  Caption = 'Transition samples'
  ClientHeight = 493
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Tag = -17
    Left = 0
    Top = 0
    Width = 501
    Height = 493
    HelpContext = -17
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 5
    TabOrder = 0
    object FormContainer: TFormContainer
      Tag = -17
      Left = 7
      Top = 7
      Width = 487
      Height = 479
      HelpContext = -17
      Align = alClient
      TabOrder = 0
    end
    object EffectsPanel1: TEffectsPanel
      Left = 7
      Top = 452
      Width = 486
      Height = 34
      TabOrder = 1
      BackgroundOptions.Opaque = False
      BackgroundOptions.GlassColor = clSilver
      BackgroundOptions.GlassTranslucency = 64
      object SpeedButtonIndex: TSpeedButton
        Left = 366
        Top = 5
        Width = 25
        Height = 25
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333000000000
          333333777777777F33333330B00000003333337F7777777F3333333000000000
          333333777777777F333333330EEEEEE033333337FFFFFF7F3333333300000000
          333333377777777F3333333330BFBFB03333333373333373F33333330BFBFBFB
          03333337F33333F7F33333330FBFBF0F03333337F33337F7F33333330BFBFB0B
          03333337F3F3F7F7333333330F0F0F0033333337F7F7F773333333330B0B0B03
          3333333737F7F7F333333333300F0F03333333337737F7F33333333333300B03
          333333333377F7F33333333333330F03333333333337F7F33333333333330B03
          3333333333373733333333333333303333333333333373333333}
        NumGlyphs = 2
        OnClick = SpeedButtonIndexClick
      end
      object BitBtnBack: TBitBtn
        Left = 283
        Top = 5
        Width = 75
        Height = 25
        Caption = '&Back'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = BitBtnBackClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333FF3333333333333003333333333333F77F33333333333009033
          333333333F7737F333333333009990333333333F773337FFFFFF330099999000
          00003F773333377777770099999999999990773FF33333FFFFF7330099999000
          000033773FF33777777733330099903333333333773FF7F33333333333009033
          33333333337737F3333333333333003333333333333377333333333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
      end
      object BitBtnNext: TBitBtn
        Left = 400
        Top = 5
        Width = 75
        Height = 25
        Caption = '&Next'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = BitBtnNextClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333FF3333333333333003333
          3333333333773FF3333333333309003333333333337F773FF333333333099900
          33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
          99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
          33333333337F3F77333333333309003333333333337F77333333333333003333
          3333333333773333333333333333333333333333333333333333333333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
      end
    end
  end
  object TransitionList: TTransitionList
    Left = 16
    Top = 16
    object Transition1: TBlendTransition
      Milliseconds = 1000
    end
    object TransitionBmpTwirl1: TBmpMaskTransition
      Milliseconds = 1000
      MaskMode = fcpmZoom
    end
    object TransitionBmpTwirl2: TBmpMaskTransition
      Milliseconds = 2000
      MaskMode = fcpmZoom
      SmoothingLevel = 5
    end
    object TransitionBmpWiper1: TBmpMaskTransition
      Milliseconds = 1000
      MaskMode = fcpmZoom
    end
    object TransitionBmpWiper2: TBmpMaskTransition
      Milliseconds = 2000
      MaskMode = fcpmZoom
      SmoothingLevel = 3
    end
    object TransitionBmpSand: TBmpMaskTransition
      Milliseconds = 2000
      MaskMode = fcpmZoom
    end
    object TransitionBmpCrowd: TBmpMaskTransition
      Milliseconds = 2000
      MaskMode = fcpmZoom
      SmoothingLevel = 3
    end
    object Transition2: TBlockTransition
      Milliseconds = 1500
      Style = 4
    end
    object Transition3: TBlockTransition
      Milliseconds = 1500
      BlockHeight = 35
      BlockWidth = 35
      Puzzle = True
    end
    object Transition4: TBlockTransition
      Milliseconds = 1000
      BlockHeight = 15
      BlockWidth = 15
    end
    object Transition5: TBlockTransition
      Milliseconds = 1500
      BlockHeight = 1
      BlockWidth = 1
    end
    object Transition43: TBlurTransition
      Milliseconds = 1500
      Radius = 50
    end
    object Transition44: TBlurTransition
      Milliseconds = 500
      Radius = 15
    end
    object Transition6: TCircleTransition
      Milliseconds = 1500
    end
    object Transition7: TCircleTransition
      Milliseconds = 1000
      SmoothingLevel = 3
      SubStyle = 2
    end
    object Transition8: TCircleTransition
      Milliseconds = 2000
      SmoothingLevel = 5
      Style = 3
    end
    object Transition9: TDiagonalTransition
      Milliseconds = 1000
    end
    object Transition10: TDiagonalTransition
      Milliseconds = 1500
      SmoothingLevel = 6
      SubStyle = 2
    end
    object Transition11: TDiagonalTransition
      Milliseconds = 1000
      Style = 2
    end
    object Transition12: TDripTransition
      Milliseconds = 1500
      Direction = tedRandom
    end
    object Transition14: TFuseTransition
      Milliseconds = 1000
      Style = 4
    end
    object Transition15: TFuseTransition
      Milliseconds = 1000
      Style = 13
    end
    object Transition16: TInterlacedTransition
      Milliseconds = 1000
      Style = 2
    end
    object Transition17: TInterlacedTransition
      Milliseconds = 1000
      SmoothingLevel = 1
    end
    object Transition18: TInterlacedTransition
      Milliseconds = 1000
      SubStyle = 5
    end
    object Transition41: TPageTransition
      Milliseconds = 1500
      Direction = tedRandom
    end
    object Transition42: TPageTransition
      Milliseconds = 2500
      Direction = tedRandom
      Size = 255
      Uncover = False
    end
    object Transition45: TPixelateTransition
      Milliseconds = 2000
      BoxSize = 50
    end
    object Transition46: TPixelateTransition
      Milliseconds = 1000
      BoxSize = 15
    end
    object Transition19: TPushTransition
      Milliseconds = 1500
      Direction = tedRandom
    end
    object Transition20: TRadialTransition
      Milliseconds = 2000
      SmoothingLevel = 4
    end
    object Transition21: TRadialTransition
      Milliseconds = 2000
      Style = 2
    end
    object Transition22: TRadialTransition
      Milliseconds = 2000
      Style = 2
      SubStyle = 8
    end
    object Transition23: TRadialTransition
      Milliseconds = 2000
      Style = 3
    end
    object Transition24: TRadialTransition
      Milliseconds = 1000
      Style = 3
      SubStyle = 5
    end
    object Transition25: TRadialTransition
      Milliseconds = 2000
      SmoothingLevel = 3
      Style = 4
    end
    object Transition26: TRadialTransition
      Milliseconds = 2000
      SmoothingLevel = 4
      Style = 6
      SubStyle = 8
    end
    object Transition28: TRollTransition
      Milliseconds = 1000
      Direction = tedRandom
      Size = 30
      Unroll = False
    end
    object Transition27: TRollTransition
      Milliseconds = 2000
      Direction = tedRandom
      Size = 100
    end
    object Transition29: TSlideTransition
      Milliseconds = 1000
      Direction = tedUp
    end
    object Transition30: TSlideTransition
      Milliseconds = 1000
      Direction = tedLeft
      SlideOut = True
    end
    object Transition31: TSlideTransition
      Milliseconds = 1000
      Direction = tedDownRight
      ElasticSrc = True
    end
    object Transition32: TSlideTransition
      Milliseconds = 1000
      Direction = tedDownLeft
      ElasticDst = True
      SlideOut = True
    end
    object Transition33: TSlideTransition
      Milliseconds = 1000
      Direction = tedUp
      ElasticSrc = True
    end
    object Transition34: TSlideTransition
      Milliseconds = 2000
      Direction = tedOut
      Pass2Options.DistributedTime = True
      Pass2Options.Reversed = True
      Pass2Options.SolidColor = clBtnFace
      PassSetting = teTwoPasses
    end
    object Transition35: TWaterfallTransition
      Milliseconds = 1000
      Direction = tedRandom
    end
    object Transition36: TWipeTransition
      Milliseconds = 1000
      BandWidth = 60
    end
    object Transition37: TWipeTransition
      Milliseconds = 1000
      BandWidth = 100
      Direction = tedDown
      SmoothBand = True
    end
    object Transition38: TWipeTransition
      Milliseconds = 1000
      BandWidth = 0
      Direction = tedDownLeft
    end
    object Transition39: TWipeTransition
      Milliseconds = 1000
      BandWidth = 0
      Direction = tedIn
    end
    object Transition40: TWipeTransition
      Milliseconds = 1000
      BandWidth = 0
      Direction = tedOut
    end
  end
  object PopupMenu: TPopupMenu
    Left = 47
    Top = 16
  end
end
