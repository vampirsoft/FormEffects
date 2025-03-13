object FormAnimations: TFormAnimations
  Tag = -17
  Left = 340
  Top = 189
  HelpContext = -17
  Caption = 'FormAnimations'
  ClientHeight = 319
  ClientWidth = 501
  Color = 46765739
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  BackgroundOptions.Opaque = False
  BackgroundOptions.ParentPicture = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label13: TLabel
    Left = 351
    Top = 194
    Width = 27
    Height = 13
    Alignment = taRightJustify
    Caption = 'Origin'
    Transparent = True
  end
  object BitBtnAnimate: TBitBtn
    Left = 419
    Top = 288
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Animate'
    Default = True
    TabOrder = 0
    OnClick = BitBtnAnimateClick
  end
  object GroupBoxSteps: TEffectsGroupBox
    Tag = -17
    Left = 4
    Top = 184
    Width = 274
    Height = 97
    HelpContext = -17
    Caption = 'Steps'
    TabOrder = 2
    BackgroundOptions.Opaque = False
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    object Label1: TLabel
      Left = 61
      Top = 45
      Width = 89
      Height = 13
      Alignment = taRightJustify
      Caption = 'Min step increment'
      Transparent = True
    end
    object Label2: TLabel
      Left = 51
      Top = 72
      Width = 99
      Height = 13
      Alignment = taRightJustify
      Caption = 'Min step milliseconds'
      Transparent = True
    end
    object Label17: TLabel
      Left = 34
      Top = 19
      Width = 69
      Height = 13
      Caption = 'Show first step'
      Transparent = True
    end
    object Label18: TLabel
      Left = 175
      Top = 19
      Width = 69
      Height = 13
      Caption = 'Show last step'
      Transparent = True
    end
    object CheckBoxFirstStep: TCheckBox
      Left = 16
      Top = 19
      Width = 13
      Height = 13
      Caption = 'Show first step'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxLastStep: TCheckBox
      Left = 157
      Top = 19
      Width = 13
      Height = 13
      Caption = 'Show last step'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object EditMinStepIncrement: TEdit
      Left = 157
      Top = 41
      Width = 61
      Height = 21
      TabOrder = 2
      Text = '30'
    end
    object EditStepMilliseconds: TEdit
      Left = 157
      Top = 68
      Width = 61
      Height = 21
      TabOrder = 3
      Text = '100'
    end
  end
  object GroupBoxBorderPen: TEffectsGroupBox
    Left = 4
    Top = 2
    Width = 274
    Height = 89
    Caption = 'Border pen'
    TabOrder = 1
    BackgroundOptions.Opaque = False
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    object Label3: TLabel
      Left = 12
      Top = 24
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Color'
      Transparent = True
    end
    object PenColorButton: TSpeedButton
      Left = 43
      Top = 20
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = PenColorButtonClick
    end
    object Label4: TLabel
      Left = 125
      Top = 24
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = 'Mode'
      Transparent = True
    end
    object Label5: TLabel
      Left = 8
      Top = 56
      Width = 28
      Height = 13
      Alignment = taRightJustify
      Caption = 'Width'
      Transparent = True
    end
    object Label6: TLabel
      Left = 129
      Top = 56
      Width = 23
      Height = 13
      Alignment = taRightJustify
      Caption = 'Style'
      Transparent = True
    end
    object ComboBoxPenMode: TComboBox
      Left = 157
      Top = 20
      Width = 110
      Height = 21
      Style = csDropDownList
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBoxPenModeChange
      Items.Strings = (
        'pmBlack'
        'pmWhite'
        'pmNop'
        'pmNot'
        'pmCopy'
        'pmNotCopy'
        'pmMergePenNot'
        'pmMaskPenNot'
        'pmMergeNotPen'
        'pmMaskNotPen'
        'pmMerge'
        'pmNotMerge'
        'pmMask'
        'pmNotMask'
        'pmXor'
        'pmNotXor')
    end
    object EditPenWidth: TEdit
      Left = 43
      Top = 52
      Width = 22
      Height = 21
      ReadOnly = True
      TabOrder = 1
      Text = '1'
      OnChange = EditPenWidthChange
    end
    object UpDownPenWidth: TUpDown
      Left = 65
      Top = 52
      Width = 15
      Height = 21
      Associate = EditPenWidth
      Min = 1
      Max = 9
      Position = 1
      TabOrder = 2
      Wrap = False
    end
    object ComboBoxPenStyle: TComboBox
      Left = 157
      Top = 52
      Width = 110
      Height = 21
      Style = csDropDownList
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 3
      OnChange = ComboBoxPenStyleChange
      Items.Strings = (
        'psSolid'
        'psDash'
        'psDot'
        'psDashDot'
        'psDashDotDot'
        'psClear'
        'psInsideFrame')
    end
  end
  object GroupBoxFillBrush: TEffectsGroupBox
    Left = 283
    Top = 2
    Width = 210
    Height = 89
    Caption = 'Fill brush'
    TabOrder = 3
    BackgroundOptions.Opaque = False
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    object Label7: TLabel
      Left = 8
      Top = 24
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Color'
      Transparent = True
    end
    object BrushColorButton: TSpeedButton
      Left = 38
      Top = 20
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = BrushColorButtonClick
    end
    object Label8: TLabel
      Left = 68
      Top = 24
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = 'Mode'
      Transparent = True
    end
    object ComboBoxBrushStyle: TComboBox
      Left = 100
      Top = 20
      Width = 104
      Height = 21
      Style = csDropDownList
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBoxBrushStyleChange
      Items.Strings = (
        'bsSolid'
        'bsClear'
        'bsHorizontal'
        'bsVertical'
        'bsFDiagonal'
        'bsBDiagonal'
        'bsCross'
        'bsDiagCross')
    end
  end
  object GroupBoxGlass: TEffectsGroupBox
    Tag = -17
    Left = 283
    Top = 93
    Width = 210
    Height = 89
    HelpContext = -17
    Caption = 'Glass'
    TabOrder = 4
    BackgroundOptions.Opaque = False
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    object Label9: TLabel
      Left = 7
      Top = 24
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Color'
      Transparent = True
    end
    object GlassColorButton: TSpeedButton
      Left = 38
      Top = 20
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = GlassColorButtonClick
    end
    object Label10: TLabel
      Left = 71
      Top = 24
      Width = 64
      Height = 13
      Alignment = taRightJustify
      Caption = 'Translucency'
      Transparent = True
    end
    object Label16: TLabel
      Left = 25
      Top = 57
      Width = 30
      Height = 13
      Caption = 'Visible'
      Transparent = True
    end
    object EditTranslucency: TEdit
      Left = 141
      Top = 20
      Width = 39
      Height = 21
      TabOrder = 0
      Text = '1'
      OnChange = EditTranslucencyChange
    end
    object UpDownTranslucency: TUpDown
      Left = 180
      Top = 20
      Width = 15
      Height = 21
      Associate = EditTranslucency
      Min = 0
      Max = 255
      Position = 1
      TabOrder = 1
      Wrap = False
    end
    object CheckBoxGlassVisible: TCheckBox
      Tag = -17
      Left = 7
      Top = 57
      Width = 13
      Height = 13
      HelpContext = -17
      Caption = 'Visible'
      TabOrder = 2
      OnClick = CheckBoxGlassVisibleClick
    end
  end
  object GroupBoxPicture: TEffectsGroupBox
    Tag = -17
    Left = 4
    Top = 93
    Width = 274
    Height = 89
    HelpContext = -17
    Caption = 'Picture'
    TabOrder = 5
    BackgroundOptions.Opaque = False
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    object Label11: TLabel
      Left = 52
      Top = 24
      Width = 37
      Height = 13
      Alignment = taRightJustify
      Caption = 'Graphic'
      Transparent = True
    end
    object PictureButton: TSpeedButton
      Left = 94
      Top = 20
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = PictureButtonClick
    end
    object Label12: TLabel
      Left = 124
      Top = 24
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = 'Mode'
      Transparent = True
    end
    object Label14: TLabel
      Left = 6
      Top = 56
      Width = 83
      Height = 13
      Alignment = taRightJustify
      Caption = 'Transparent color'
      Transparent = True
    end
    object PicColorButton: TSpeedButton
      Left = 94
      Top = 52
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = PicColorButtonClick
    end
    object Label15: TLabel
      Left = 175
      Top = 57
      Width = 30
      Height = 13
      Caption = 'Visible'
      Transparent = True
    end
    object ComboBoxPicMode: TComboBox
      Tag = -17
      Left = 157
      Top = 20
      Width = 110
      Height = 21
      HelpContext = -17
      Style = csDropDownList
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBoxPicModeChange
      Items.Strings = (
        'fcpmCenter'
        'fcpmCenterStretch'
        'fcpmStretch'
        'fcpmTile')
    end
    object CheckBoxPicVisible: TCheckBox
      Tag = -17
      Left = 158
      Top = 57
      Width = 13
      Height = 13
      HelpContext = -17
      Caption = 'Visible'
      TabOrder = 1
      OnClick = CheckBoxPicVisibleClick
    end
  end
  object PreviewPanel: TEffectsPanel
    Left = 288
    Top = 220
    Width = 205
    Height = 61
    BevelOuter = bvNone
    TabOrder = 6
    BackgroundOptions.Opaque = False
    BackgroundOptions.PictureTranspColor = clWhite
    BackgroundOptions.PictureVisible = False
    BackgroundOptions.GlassTranslucency = 128
    object PreviewShape: TShape
      Left = 0
      Top = 0
      Width = 205
      Height = 61
      Align = alClient
      Brush.Style = bsClear
      Pen.Color = clRed
      Pen.Width = 3
    end
  end
  object ComboBoxOrg: TComboBox
    Tag = -17
    Left = 383
    Top = 190
    Width = 110
    Height = 21
    HelpContext = -17
    Style = csDropDownList
    DropDownCount = 20
    ItemHeight = 13
    TabOrder = 7
    Items.Strings = (
      #39'Animate'#39' button'
      'Preview panel'
      'Cursor position'
      'Form'#39's center')
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Options = [cdFullOpen, cdAnyColor]
    Left = 298
    Top = 288
  end
  object PictureDialog: TOpenPictureDialog
    Left = 328
    Top = 288
  end
  object TEAnimationList1: TTEAnimationList
    Left = 264
    Top = 288
    object Animation: TTEZoomFrameAnimation
    end
  end
end
