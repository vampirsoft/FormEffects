inherited BmpMaskTransitionEditor: TBmpMaskTransitionEditor
  Caption = 'BmpMaskTransitionEditor'
  PixelsPerInch = 96
  TextHeight = 14
  inherited PanelOther: TEffectsPanel
    Height = 164
    Align = alClient
    Visible = True
    object LabelSmoothingLevel: TLabel
      Left = 8
      Top = 8
      Width = 75
      Height = 14
      Alignment = taRightJustify
      Caption = '&Smoothing level'
      FocusControl = EditSmoothingLevel
      Transparent = True
    end
    object LabelMask: TLabel
      Left = 58
      Top = 40
      Width = 25
      Height = 14
      Alignment = taRightJustify
      Caption = 'Mask'
      FocusControl = EditMilliseconds
      Transparent = True
    end
    object SpeedButtonMask: TSpeedButton
      Left = 90
      Top = 37
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = SpeedButtonMaskClick
    end
    object LabelMode: TLabel
      Left = 123
      Top = 40
      Width = 26
      Height = 14
      Alignment = taRightJustify
      Caption = 'M&ode'
      FocusControl = ComboBoxMode
      Transparent = True
    end
    object EditSmoothingLevel: TEdit
      Left = 90
      Top = 5
      Width = 34
      Height = 22
      ReadOnly = True
      TabOrder = 0
      Text = '0'
      OnChange = EditMillisecondsChange
    end
    object UpDownSmoothingLevel: TUpDown
      Left = 124
      Top = 5
      Width = 16
      Height = 22
      Associate = EditSmoothingLevel
      Min = 0
      Max = 0
      Position = 0
      TabOrder = 1
      Wrap = False
    end
    object EffectsPanelMask: TEffectsPanel
      Left = 8
      Top = 67
      Width = 209
      Height = 90
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 2
      BackgroundOptions.Opaque = False
      BackgroundOptions.PictureMode = fcpmStretch
    end
    object ComboBoxMode: TComboBox
      Tag = -1
      Left = 152
      Top = 37
      Width = 67
      Height = 22
      HelpContext = -1
      Style = csDropDownList
      DropDownCount = 16
      ItemHeight = 14
      Sorted = True
      TabOrder = 3
      OnChange = ComboBoxModeChange
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofExtensionDifferent, ofPathMustExist, ofFileMustExist]
    Left = 136
    Top = 96
  end
end
