object FormTransitionEditor: TFormTransitionEditor
  Left = 375
  Top = 463
  Caption = 'Transitions editor'
  ClientHeight = 223
  ClientWidth = 317
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  BackgroundOptions.ParentBkgrndForm = True
  BackgroundOptions.ParentPicture = True
  PixelsPerInch = 96
  TextHeight = 16
  object LabelText: TLabel
    Left = 11
    Top = 12
    Width = 294
    Height = 165
    AutoSize = False
    Caption = 
      'Press the button below to launch the runtime transition editor. ' +
      'This editor is also available at design time to help you to visu' +
      'ally configure your transitions.'#13#10#13#10'In this case, the transition' +
      ' effect you are configuring is the one that applies whenever you' +
      ' select an option in the menu.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object BitBtnEditor: TBitBtn
    Left = 121
    Top = 184
    Width = 75
    Height = 25
    Caption = '&Editor...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = BitBtnEditorClick
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      0400000000000001000000000000000000001000000010000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00370777033333
      3330337F3F7F33333F3787070003333707303F737773333373F7007703333330
      700077337F3333373777887007333337007733F773F333337733700070333333
      077037773733333F7F37703707333300080737F373333377737F003333333307
      78087733FFF3337FFF7F33300033330008073F3777F33F777F73073070370733
      078073F7F7FF73F37FF7700070007037007837773777F73377FF007777700730
      70007733FFF77F37377707700077033707307F37773F7FFF7337080777070003
      3330737F3F7F777F333778080707770333333F7F737F3F7F3333080787070003
      33337F73FF737773333307800077033333337337773373333333}
    NumGlyphs = 2
  end
end
