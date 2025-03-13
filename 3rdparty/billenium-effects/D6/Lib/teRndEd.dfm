inherited RandomTransitionEditor: TRandomTransitionEditor
  Caption = 'RandomTransitionEditor'
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 14
  inherited PanelOther: TEffectsPanel
    Height = 164
    Align = alClient
    Visible = True
    object PanelRandom: TEffectsPanel
      Left = 0
      Top = 0
      Width = 224
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      BackgroundOptions.ParentOpaque = True
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      BackgroundOptions.ParentGlass = True
      object RadioGroupMode: TRadioGroup
        Left = 6
        Top = -4
        Width = 213
        Height = 37
        Columns = 2
        Items.Strings = (
          'Random'
          'Cyclical')
        TabOrder = 0
      end
    end
    object FormContainerList: TFormContainer
      Left = 0
      Top = 38
      Width = 224
      Height = 126
      Align = alClient
      TabOrder = 1
    end
  end
end
