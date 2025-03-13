object TransitionListEditorForm: TTransitionListEditorForm
  Left = 369
  Top = 365
  Width = 390
  Height = 329
  Caption = 'TransitionList Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object PanelButtons: TPanel
    Left = 0
    Top = 0
    Width = 382
    Height = 24
    Align = alTop
    TabOrder = 0
    object SpeedButtonAdd: TSpeedButton
      Left = 0
      Top = 1
      Width = 23
      Height = 22
      Hint = 'Add new transition'
      Glyph.Data = {
        4E010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D8000000C30E0000C30E00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333330000003333333333333333330000003338888888888888830000003300
        00000000000083000000330FBFBFBFB00FB083000000330BFBFBFBF070F08300
        0000330FBFBFBFB0B70083000000330BFBFBFBF00000830000003F0F8BFBFBBF
        BFB08300000038BB8FB8BFFBFBF083000000338F8B8BFBBFBFB0830000003888
        F8FBFBFBFBF0830000003FB8BF888800000033000000338B8B8B333333333300
        000038B38F38B3333333330000003B338B33833333333300000033338F333333
        333333000000333333333333333333000000}
      OnClick = SpeedButtonAddClick
    end
    object SpeedButtonDelete: TSpeedButton
      Left = 23
      Top = 1
      Width = 23
      Height = 22
      Hint = 'Delete selected transition'
      Glyph.Data = {
        B6010000424DB601000000000000760000002800000024000000100000000100
        04000000000040010000C30E0000C30E00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFFFFFFFFFFF000033388888888888888333388888888888888F
        000033000000000000008333888888888888888F0000330FBFBFBFB00FB08333
        8F878787888F888F0000380BFBFBFBF070F08338887878787888F88F0000310F
        BFBFBFB0B70083388F87878F8888888F0000311BFBF81BF00000833888F87888
        7888888F00003818BF818FBFBFB08338888F88878787888F000033118B11FBFB
        FBF08333888888787878788F00003381111FBFBFBFB08333888887878787888F
        0000338111FBFBFBFBF083338888F8F8F8F8F883000038111180000000003338
        8888888888888833000031183118333333333338883888FF3333333300003333
        33118333333333333333888FF333333300003333333118333333333333333888
        3333333300003333333333333333333333333333333333330000}
      NumGlyphs = 2
      OnClick = SpeedButtonDeleteClick
    end
    object SpeedButtonEdit: TSpeedButton
      Left = 46
      Top = 1
      Width = 23
      Height = 22
      Hint = 'Edit selected transition'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000000
        000033333377777777773333330FFFFFFFF03FF3FF7FF33F3FF700300000FF0F
        00F077F777773F737737E00BFBFB0FFFFFF07773333F7F3333F7E0BFBF000FFF
        F0F077F3337773F3F737E0FBFBFBF0F00FF077F3333FF7F77F37E0BFBF00000B
        0FF077F3337777737337E0FBFBFBFBF0FFF077F33FFFFFF73337E0BF0000000F
        FFF077FF777777733FF7000BFB00B0FF00F07773FF77373377373330000B0FFF
        FFF03337777373333FF7333330B0FFFF00003333373733FF777733330B0FF00F
        0FF03333737F37737F373330B00FFFFF0F033337F77F33337F733309030FFFFF
        00333377737FFFFF773333303300000003333337337777777333}
      NumGlyphs = 2
      OnClick = SpeedButtonEditClick
    end
    object SpeedButtonUp: TSpeedButton
      Left = 69
      Top = 1
      Width = 23
      Height = 22
      Hint = 'Move selected up'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
        3333333333777F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
        3333333777737777F333333099999990333333373F3333373333333309999903
        333333337F33337F33333333099999033333333373F333733333333330999033
        3333333337F337F3333333333099903333333333373F37333333333333090333
        33333333337F7F33333333333309033333333333337373333333333333303333
        333333333337F333333333333330333333333333333733333333}
      NumGlyphs = 2
      OnClick = SpeedButtonUpClick
    end
    object SpeedButtonDown: TSpeedButton
      Left = 92
      Top = 1
      Width = 23
      Height = 22
      Hint = 'Move selected down'
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
        333333333337F33333333333333033333333333333373F333333333333090333
        33333333337F7F33333333333309033333333333337373F33333333330999033
        3333333337F337F33333333330999033333333333733373F3333333309999903
        333333337F33337F33333333099999033333333373333373F333333099999990
        33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
        33333333337F7F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333300033333333333337773333333}
      NumGlyphs = 2
      OnClick = SpeedButtonDownClick
    end
  end
  object ListViewTransitions: TListView
    Left = 0
    Top = 24
    Width = 382
    Height = 271
    Align = alClient
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Caption = 'Description'
        Width = 160
      end
      item
        Caption = 'Name'
        Width = 161
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListViewTransitionsDblClick
    OnKeyDown = ListViewTransitionsKeyDown
    OnSelectItem = ListViewTransitionsSelectItem
  end
end
