object FormPower: TFormPower
  Left = 338
  Top = 258
  Caption = 'Power'
  ClientHeight = 442
  ClientWidth = 558
  Color = 48416467
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  Alignment = fcfaClient
  BackgroundOptions.ParentBkgrndForm = True
  BackgroundOptions.ParentPicture = True
  BackgroundOptions.GlassColor = 48416467
  BackgroundOptions.GlassTranslucency = 64
  PixelsPerInch = 96
  TextHeight = 13
  object PanelButton: TEffectsPanel
    Left = 0
    Top = 0
    Width = 558
    Height = 35
    Align = alTop
    ParentColor = True
    TabOrder = 0
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object LabelMagic: TLabel
      Left = 38
      Top = 10
      Width = 357
      Height = 16
      Caption = 'Press this button to see FormContainer'#39's power in action'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object BitBtnMagic: TBitBtn
      Left = 7
      Top = 5
      Width = 25
      Height = 25
      TabOrder = 0
      TabStop = False
      OnClick = BitBtnMagicClick
      Glyph.Data = {
        66010000424D6601000000000000760000002800000014000000140000000100
        040000000000F000000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777700007777700000007777777700007777700F00007777777700007777
        700F00007777777700007777700F000077777777000077700000000000777777
        00007788888888888887777700007777770F0FF07777777700007777770F0000
        077BB77700007777000FFFFFF077B77700007770FFFFFF000077779700007770
        F0F0F070FF07799700007770FFFFF0770FF0777700007770FFF0F07770FF07E7
        0000770FF0FFF077770F0077000070FF00F00777777000070000700070F07777
        E77700770000777770F07797EE7BB77700007777770077997777B77700007777
        77777777777777770000}
    end
  end
  object PanelForm: TEffectsPanel
    Left = 0
    Top = 35
    Width = 558
    Height = 407
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 5
    ParentColor = True
    TabOrder = 1
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object FormContainer: TFormContainer
      Left = 7
      Top = 7
      Width = 544
      Height = 393
      Align = alClient
      AutoScroll = True
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      TabOrder = 0
    end
  end
  object TransitionList: TTransitionList
    Left = 24
    Top = 56
    object Transition: TFuseTransition
      Milliseconds = 1000
      Style = 0
    end
  end
end
