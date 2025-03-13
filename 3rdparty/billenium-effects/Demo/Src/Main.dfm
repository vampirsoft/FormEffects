object MainForm: TMainForm
  Left = 192
  Top = 118
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Billenium effects demo (%s)'
  ClientHeight = 493
  ClientWidth = 700
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainFormContainer: TFormContainer
    Left = 0
    Top = 0
    Width = 700
    Height = 493
    Align = alClient
    TabOrder = 0
  end
  object FormTransitions: TFormTransitions
    BackgroundOptions.PictureMode = fcpmCenter
    DestroyTransitions = False
    HideTransition = Transition
    ShowTransition = Transition
    Left = 8
    Top = 8
  end
  object TransitionList: TTransitionList
    Left = 40
    Top = 8
    object Transition: TRollTransition
      AbortOnClick = True
      AbortOnEscape = True
      OnAfterTransition = TransitionAfterTransition
      OnStartTransition = TransitionStartTransition
      Milliseconds = 2000
      Size = 100
    end
  end
  object TFlickerFreeTransition
  end
end
