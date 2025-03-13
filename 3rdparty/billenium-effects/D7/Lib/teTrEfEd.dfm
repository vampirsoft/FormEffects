object TransitionEffectEditor: TTransitionEffectEditor
  Tag = -1
  Left = 487
  Top = 401
  HelpContext = -1
  BorderStyle = bsNone
  Caption = 'TransitionEffectEditor'
  ClientHeight = 292
  ClientWidth = 224
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object EffectsGroupBoxPass2: TEffectsGroupBox
    Left = 266
    Top = 64
    Width = 155
    Height = 113
    Caption = '&2nd pass options'
    TabOrder = 0
    Visible = False
    BackgroundOptions.Opaque = False
    BackgroundOptions.GlassColor = clBtnFace
    BackgroundOptions.GlassTranslucency = 38
    object LabelDistributedTime: TLabel
      Left = 26
      Top = 19
      Width = 73
      Height = 14
      Caption = 'Distributed time'
      FocusControl = CheckBoxDistributedTime
      Transparent = True
      OnClick = LabelDistributedTimeClick
    end
    object Label2PReversed: TLabel
      Left = 26
      Top = 43
      Width = 47
      Height = 14
      Caption = 'Reversed'
      FocusControl = CheckBox2PReversed
      Transparent = True
      OnClick = Label2PReversedClick
    end
    object LabelUseSolidColor: TLabel
      Left = 26
      Top = 67
      Width = 71
      Height = 14
      Caption = 'Use solid color'
      FocusControl = CheckBoxUseSolidColor
      Transparent = True
      OnClick = LabelUseSolidColorClick
    end
    object LabelSolidColor: TLabel
      Left = 26
      Top = 91
      Width = 50
      Height = 14
      Caption = 'Solid color'
      FocusControl = CheckBoxUseSolidColor
      Transparent = True
      OnClick = SpeedButtonSolidColorClick
    end
    object SpeedButtonSolidColor: TSpeedButton
      Left = 9
      Top = 92
      Width = 13
      Height = 13
      OnClick = SpeedButtonSolidColorClick
    end
    object SpeedButtonClosePanel: TSpeedButton
      Left = 128
      Top = 86
      Width = 21
      Height = 21
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        0400000000000001000000000000000000001000000010000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        555555555555555555555555555555555555555555FF55555555555559055555
        55555555577FF5555555555599905555555555557777F5555555555599905555
        555555557777FF5555555559999905555555555777777F555555559999990555
        5555557777777FF5555557990599905555555777757777F55555790555599055
        55557775555777FF5555555555599905555555555557777F5555555555559905
        555555555555777FF5555555555559905555555555555777FF55555555555579
        05555555555555777FF5555555555557905555555555555777FF555555555555
        5990555555555555577755555555555555555555555555555555}
      NumGlyphs = 2
      OnClick = SpeedButtonClosePanelClick
    end
    object CheckBoxDistributedTime: TCheckBox
      Left = 9
      Top = 19
      Width = 13
      Height = 13
      Caption = 'CheckBoxDistributedTime'
      TabOrder = 0
      OnClick = EditMillisecondsChange
    end
    object CheckBox2PReversed: TCheckBox
      Left = 9
      Top = 43
      Width = 13
      Height = 13
      Caption = 'CheckBoxDistributedTime'
      TabOrder = 1
      OnClick = EditMillisecondsChange
    end
    object CheckBoxUseSolidColor: TCheckBox
      Left = 9
      Top = 67
      Width = 13
      Height = 13
      Caption = 'CheckBoxDistributedTime'
      TabOrder = 2
      OnClick = EditMillisecondsChange
    end
  end
  object PanelPasses: TEffectsPanel
    Left = 0
    Top = 0
    Width = 224
    Height = 64
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object LabelPassSetting: TLabel
      Left = 47
      Top = 8
      Width = 36
      Height = 14
      Alignment = taRightJustify
      Caption = 'Passes'
      FocusControl = ComboBoxPasses
      Transparent = True
    end
    object LabelPass2Options: TLabel
      Left = 38
      Top = 40
      Width = 45
      Height = 14
      Alignment = taRightJustify
      Caption = '2nd pass'
      Transparent = True
    end
    object SpeedButtonPass2: TSpeedButton
      Left = 90
      Top = 37
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButtonPass2Click
    end
    object ComboBoxPasses: TComboBox
      Tag = -1
      Left = 90
      Top = 5
      Width = 129
      Height = 22
      HelpContext = -1
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 0
      OnChange = EditMillisecondsChange
      Items.Strings = (
        '1 pass'
        '2 passes'
        'Palette dependent')
    end
  end
  object PanelMilliseconds: TEffectsPanel
    Left = 0
    Top = 64
    Width = 224
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object LabelMilliseconds: TLabel
      Left = 25
      Top = 8
      Width = 58
      Height = 14
      Alignment = taRightJustify
      Caption = '&Milliseconds'
      FocusControl = EditMilliseconds
      Transparent = True
    end
    object EditMilliseconds: TEdit
      Tag = -1
      Left = 90
      Top = 5
      Width = 57
      Height = 22
      HelpContext = -1
      TabOrder = 0
      Text = '0'
      OnChange = EditMillisecondsChange
      OnExit = EditMillisecondsExit
    end
    object UpDownMilliseconds: TUpDown
      Tag = -1
      Left = 147
      Top = 5
      Width = 16
      Height = 22
      HelpContext = -1
      Associate = EditMilliseconds
      Max = 30000
      Increment = 100
      TabOrder = 1
      Thousands = False
    end
  end
  object PanelDirection: TEffectsPanel
    Left = 0
    Top = 96
    Width = 224
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object LabelDirection: TLabel
      Left = 41
      Top = 8
      Width = 42
      Height = 14
      Alignment = taRightJustify
      Caption = '&Direction'
      FocusControl = ComboBoxDirection
      Transparent = True
    end
    object ComboBoxDirection: TComboBox
      Tag = -1
      Left = 90
      Top = 5
      Width = 129
      Height = 22
      HelpContext = -1
      Style = csDropDownList
      DropDownCount = 16
      ItemHeight = 14
      Sorted = True
      TabOrder = 0
      OnChange = EditMillisecondsChange
    end
  end
  object PanelOther: TEffectsPanel
    Left = 0
    Top = 128
    Width = 224
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 130
    Top = 32
  end
  object FormTransitions: TFormTransitions
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    Left = 162
    Top = 32
  end
end
