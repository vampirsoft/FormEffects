inherited BlurTransitionEditor: TBlurTransitionEditor
  Caption = 'BlurTransitionEditor'
  PixelsPerInch = 96
  TextHeight = 14
  inherited PanelOther: TEffectsPanel
    Visible = True
    object LabelRadius: TLabel
      Left = 50
      Top = 8
      Width = 33
      Height = 14
      Alignment = taRightJustify
      Caption = '&Radius'
      FocusControl = EditRadius
      Transparent = True
    end
    object EditRadius: TEdit
      Left = 90
      Top = 5
      Width = 57
      Height = 22
      TabOrder = 0
      Text = '0'
      OnChange = EditMillisecondsChange
    end
    object UpDownRadius: TUpDown
      Left = 147
      Top = 5
      Width = 16
      Height = 22
      Associate = EditRadius
      Min = 0
      Max = 50
      Increment = 5
      Position = 0
      TabOrder = 1
      Wrap = False
    end
  end
end
