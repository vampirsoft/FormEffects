object FormBkgrndEditor: TFormBkgrndEditor
  Left = 303
  Top = 267
  Caption = 'Background samples'
  ClientHeight = 412
  ClientWidth = 448
  Color = clWhite
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  BackgroundOptions.ParentBkgrndForm = True
  BackgroundOptions.ParentPicture = True
  PixelsPerInch = 96
  TextHeight = 16
  object EffectsGroupBoxBkrnd: TEffectsGroupBox
    Left = 12
    Top = 8
    Width = 424
    Height = 393
    Caption = 'Demo'#39's background'
    TabOrder = 0
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object LabelGlassColor: TLabel
      Left = 10
      Top = 337
      Width = 66
      Height = 16
      Caption = 'Glass color'
      Transparent = True
    end
    object LabelFormTranslucency: TLabel
      Left = 229
      Top = 337
      Width = 117
      Height = 16
      Caption = 'Form'#39's translucency'
      FocusControl = EditFormTranslucency
      Transparent = True
    end
    object EditFormTranslucency: TEdit
      Left = 229
      Top = 356
      Width = 41
      Height = 24
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 2
      Text = '0'
      OnChange = EditFormTranslucencyChange
    end
    object UpDownFormTranslucency: TUpDown
      Left = 270
      Top = 356
      Width = 16
      Height = 24
      Associate = EditFormTranslucency
      Min = 0
      Max = 255
      Increment = 10
      Position = 0
      TabOrder = 3
      Thousands = False
      Wrap = False
    end
    object ButtonGlassColor: TEffectsPanel
      Left = 10
      Top = 356
      Width = 25
      Height = 25
      BorderStyle = bsSingle
      Caption = '...'
      ParentColor = True
      TabOrder = 1
      OnClick = ButtonGlassColorClick
    end
    object EffectsGroupBoxPicture: TEffectsGroupBox
      Left = 10
      Top = 254
      Width = 404
      Height = 75
      Caption = 'Picture'
      TabOrder = 0
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      BackgroundOptions.ParentGlass = True
      object LabelPicFile: TLabel
        Left = 10
        Top = 21
        Width = 21
        Height = 16
        Caption = 'File'
        Transparent = True
      end
      object LabelPicMode: TLabel
        Left = 219
        Top = 21
        Width = 32
        Height = 16
        Caption = 'Mode'
        Transparent = True
      end
      object SpeedButtonPicFile: TSpeedButton
        Left = 186
        Top = 41
        Width = 22
        Height = 21
        Hint = 'Open file'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          5555555555555555555555555555555555555555555555555555555555555555
          555555555555555555555555555555555555555FFFFFFFFFF555550000000000
          55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
          B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
          000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
          555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
          55555575FFF75555555555700007555555555557777555555555555555555555
          5555555555555555555555555555555555555555555555555555}
        NumGlyphs = 2
        OnClick = SpeedButtonPicFileClick
      end
      object EditPicFile: TEdit
        Left = 10
        Top = 41
        Width = 175
        Height = 21
        TabStop = False
        CharCase = ecLowerCase
        Ctl3D = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
        OnChange = EditPicFileChange
      end
      object ComboBoxPicMode: TComboBox
        Left = 219
        Top = 41
        Width = 177
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        TabOrder = 1
        OnChange = ComboBoxPicModeChange
        Items.Strings = (
          'fcpmCenter'
          'fcpmCenterStretch'
          'fcpmStretch'
          'fcpmTile'
          'fcpmZoom')
      end
    end
    object EffectsGroupBox1: TEffectsGroupBox
      Left = 10
      Top = 24
      Width = 404
      Height = 217
      Caption = 'Preset samples'
      TabOrder = 4
      BackgroundOptions.ParentOpaque = True
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      BackgroundOptions.ParentGlass = True
      object ButtonGradient: TSpeedButton
        Left = 15
        Top = 20
        Width = 374
        Height = 25
        Caption = 'Gradient'
        Flat = True
        OnClick = ButtonGradientClick
      end
      object ButtonStone: TSpeedButton
        Left = 15
        Top = 52
        Width = 374
        Height = 25
        Caption = 'Stone texture'
        Flat = True
        OnClick = ButtonStoneClick
      end
      object ButtonStrokes: TSpeedButton
        Left = 15
        Top = 84
        Width = 374
        Height = 25
        Caption = 'Strokes texture'
        Flat = True
        OnClick = ButtonStrokesClick
      end
      object ButtonWall: TSpeedButton
        Left = 15
        Top = 116
        Width = 374
        Height = 25
        Caption = 'Wall texture'
        Flat = True
        OnClick = ButtonWallClick
      end
      object ButtonWood: TSpeedButton
        Left = 15
        Top = 148
        Width = 374
        Height = 25
        Caption = 'Wood texture'
        Flat = True
        OnClick = ButtonWoodClick
      end
      object ButtonForm: TSpeedButton
        Left = 15
        Top = 180
        Width = 374
        Height = 25
        Caption = 'Complex background with form shading'
        Flat = True
        OnClick = ButtonFormClick
      end
    end
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Options = [cdFullOpen]
    Left = 50
    Top = 366
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.gif;*.bmp;*.ico;*.emf;*.wmf)|*.gif;*.bmp;*.ico;*.emf;*.wm' +
      'f|CompuServe GIF Image (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|Icons' +
      ' (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf' +
      ')|*.wmf'
    Left = 206
    Top = 273
  end
  object TransitionList: TTransitionList
    Left = 384
    Top = 32
    object Transition: TRadialTransition
      Milliseconds = 1500
      SmoothingLevel = 4
    end
    object Transition2: TFlickerFreeTransition
    end
  end
end
