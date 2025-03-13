inherited MaskedTransitionEditor: TMaskedTransitionEditor
  Caption = 'MaskedTransitionEditor'
  PixelsPerInch = 96
  TextHeight = 14
  inherited PanelOther: TEffectsPanel
    Height = 128
    Visible = True
    object PanelStyle: TEffectsPanel
      Left = 0
      Top = 0
      Width = 224
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      BackgroundOptions.ParentOpaque = True
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      BackgroundOptions.ParentGlass = True
      object LabelStyle: TLabel
        Left = 59
        Top = 8
        Width = 24
        Height = 14
        Alignment = taRightJustify
        Caption = 'St&yle'
        FocusControl = EditStyle
        Transparent = True
      end
      object LabelRndStyle: TLabel
        Left = 162
        Top = 8
        Width = 39
        Height = 14
        Caption = 'Random'
        FocusControl = EditStyle
        Transparent = True
      end
      object EditStyle: TEdit
        Left = 90
        Top = 5
        Width = 34
        Height = 22
        ReadOnly = True
        TabOrder = 0
        Text = '1'
        OnChange = EditStyleChange
      end
      object UpDownStyle: TUpDown
        Left = 124
        Top = 5
        Width = 16
        Height = 22
        Associate = EditStyle
        Min = 1
        Max = 30000
        Position = 1
        TabOrder = 1
      end
      object CheckBoxRndStyle: TCheckBox
        Left = 146
        Top = 8
        Width = 13
        Height = 13
        TabOrder = 2
        OnClick = CheckBoxRndStyleClick
      end
    end
    object PanelSubstyle: TEffectsPanel
      Left = 0
      Top = 32
      Width = 224
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      BackgroundOptions.ParentOpaque = True
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      BackgroundOptions.ParentGlass = True
      object LabelSubstyle: TLabel
        Left = 41
        Top = 8
        Width = 42
        Height = 14
        Alignment = taRightJustify
        Caption = 'Su&bstyle'
        FocusControl = EditSubstyle
        Transparent = True
      end
      object LabelRndSubstyle: TLabel
        Left = 161
        Top = 8
        Width = 39
        Height = 14
        Caption = 'Random'
        Transparent = True
      end
      object EditSubstyle: TEdit
        Left = 90
        Top = 5
        Width = 34
        Height = 22
        ReadOnly = True
        TabOrder = 0
        Text = '1'
        OnChange = EditMillisecondsChange
      end
      object UpDownSubstyle: TUpDown
        Left = 124
        Top = 5
        Width = 16
        Height = 22
        Associate = EditSubstyle
        Min = 1
        Max = 30000
        Position = 1
        TabOrder = 1
      end
      object CheckBoxRndSubstyle: TCheckBox
        Left = 146
        Top = 8
        Width = 13
        Height = 13
        TabOrder = 2
        OnClick = CheckBoxRndSubstyleClick
      end
    end
    object PanelSmoothingLevel: TEffectsPanel
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
        Max = 0
        TabOrder = 1
      end
    end
    object PanelMsk: TEffectsPanel
      Left = 0
      Top = 96
      Width = 224
      Height = 32
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      Visible = False
      BackgroundOptions.ParentOpaque = True
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      BackgroundOptions.ParentGlass = True
    end
  end
end
