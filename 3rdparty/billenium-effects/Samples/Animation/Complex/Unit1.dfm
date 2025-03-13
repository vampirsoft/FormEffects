object Form1: TForm1
  Tag = -17
  Left = 340
  Top = 189
  Width = 541
  Height = 369
  HelpContext = -17
  Caption = 'Form1'
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
  object Label13: TLabel
    Left = 374
    Top = 211
    Width = 27
    Height = 13
    Alignment = taRightJustify
    Caption = 'Origin'
  end
  object BitBtnAnimate: TBitBtn
    Left = 451
    Top = 309
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Animate'
    Default = True
    TabOrder = 0
    OnClick = BitBtnAnimateClick
  end
  object GroupBoxSteps: TGroupBox
    Tag = -17
    Left = 8
    Top = 200
    Width = 281
    Height = 97
    HelpContext = -17
    Caption = 'Steps'
    TabOrder = 2
    object Label1: TLabel
      Left = 66
      Top = 45
      Width = 89
      Height = 13
      Alignment = taRightJustify
      Caption = 'Min step increment'
      Transparent = True
    end
    object Label2: TLabel
      Left = 56
      Top = 72
      Width = 99
      Height = 13
      Alignment = taRightJustify
      Caption = 'Min step milliseconds'
      Transparent = True
    end
    object CheckBoxFirstStep: TCheckBox
      Left = 16
      Top = 17
      Width = 97
      Height = 17
      Caption = 'Show first step'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBoxLastStep: TCheckBox
      Left = 162
      Top = 17
      Width = 97
      Height = 17
      Caption = 'Show last step'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object EditMinStepIncrement: TEdit
      Left = 162
      Top = 41
      Width = 61
      Height = 21
      TabOrder = 2
      Text = '30'
    end
    object EditStepMilliseconds: TEdit
      Left = 162
      Top = 68
      Width = 61
      Height = 21
      TabOrder = 3
      Text = '100'
    end
  end
  object GroupBoxBorderPen: TGroupBox
    Left = 8
    Top = 8
    Width = 281
    Height = 89
    Caption = 'Border pen'
    TabOrder = 1
    object Label3: TLabel
      Left = 12
      Top = 24
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Color'
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
      Left = 130
      Top = 24
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = 'Mode'
    end
    object Label5: TLabel
      Left = 8
      Top = 56
      Width = 28
      Height = 13
      Alignment = taRightJustify
      Caption = 'Width'
    end
    object Label6: TLabel
      Left = 134
      Top = 56
      Width = 23
      Height = 13
      Alignment = taRightJustify
      Caption = 'Style'
    end
    object ComboBoxPenMode: TComboBox
      Left = 162
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
      Left = 162
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
  object GroupBoxFillBrush: TGroupBox
    Left = 298
    Top = 8
    Width = 227
    Height = 89
    Caption = 'Fill brush'
    TabOrder = 3
    object Label7: TLabel
      Left = 13
      Top = 24
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Color'
    end
    object BrushColorButton: TSpeedButton
      Left = 43
      Top = 20
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = BrushColorButtonClick
    end
    object Label8: TLabel
      Left = 76
      Top = 24
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = 'Mode'
    end
    object ComboBoxBrushStyle: TComboBox
      Left = 108
      Top = 20
      Width = 110
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
  object GroupBoxGlass: TGroupBox
    Tag = -17
    Left = 298
    Top = 104
    Width = 227
    Height = 89
    HelpContext = -17
    Caption = 'Glass'
    TabOrder = 4
    object Label9: TLabel
      Left = 12
      Top = 24
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = 'Color'
    end
    object GlassColorButton: TSpeedButton
      Left = 43
      Top = 20
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = GlassColorButtonClick
    end
    object Label10: TLabel
      Left = 85
      Top = 24
      Width = 64
      Height = 13
      Alignment = taRightJustify
      Caption = 'Translucency'
    end
    object EditTranslucency: TEdit
      Left = 155
      Top = 20
      Width = 39
      Height = 21
      TabOrder = 0
      Text = '1'
      OnChange = EditTranslucencyChange
    end
    object UpDownTranslucency: TUpDown
      Left = 194
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
      Left = 12
      Top = 56
      Width = 97
      Height = 17
      HelpContext = -17
      Caption = 'Visible'
      TabOrder = 2
      OnClick = CheckBoxGlassVisibleClick
    end
  end
  object GroupBoxPicture: TGroupBox
    Tag = -17
    Left = 8
    Top = 104
    Width = 281
    Height = 89
    HelpContext = -17
    Caption = 'Picture'
    TabOrder = 5
    object Label11: TLabel
      Left = 56
      Top = 24
      Width = 37
      Height = 13
      Alignment = taRightJustify
      Caption = 'Graphic'
    end
    object PictureButton: TSpeedButton
      Left = 99
      Top = 20
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = PictureButtonClick
    end
    object Label12: TLabel
      Left = 129
      Top = 24
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = 'Mode'
    end
    object Label14: TLabel
      Left = 10
      Top = 56
      Width = 83
      Height = 13
      Alignment = taRightJustify
      Caption = 'Transparent color'
    end
    object PicColorButton: TSpeedButton
      Left = 99
      Top = 52
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = PicColorButtonClick
    end
    object ComboBoxPicMode: TComboBox
      Tag = -17
      Left = 162
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
      Left = 162
      Top = 56
      Width = 97
      Height = 17
      HelpContext = -17
      Caption = 'Visible'
      TabOrder = 1
      OnClick = CheckBoxPicVisibleClick
    end
  end
  object PreviewPanel: TEffectsPanel
    Left = 298
    Top = 236
    Width = 227
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
      Width = 227
      Height = 61
      Align = alClient
      Brush.Style = bsClear
      Pen.Color = clRed
      Pen.Width = 3
    end
  end
  object ComboBoxOrg: TComboBox
    Tag = -17
    Left = 406
    Top = 207
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
    Top = 304
  end
  object PictureDialog: TOpenPictureDialog
    Left = 328
    Top = 304
  end
  object TEAnimationList1: TTEAnimationList
    Left = 264
    Top = 304
    object Animation: TTEZoomFrameAnimation
    end
  end
end
