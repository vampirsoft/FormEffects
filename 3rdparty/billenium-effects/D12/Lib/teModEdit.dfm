object TransitionModalEditor: TTransitionModalEditor
  Left = 347
  Top = 350
  BorderStyle = bsDialog
  Caption = 'Transition editor'
  ClientHeight = 328
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 14
  object EditorPanel: TEffectsPanel
    Left = 0
    Top = 0
    Width = 412
    Height = 294
    Align = alClient
    ParentColor = True
    TabOrder = 0
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object FormContainer: TFormContainer
      Left = 1
      Top = 1
      Width = 410
      Height = 292
      Align = alClient
      BackgroundOptions.ParentOpaque = True
      BackgroundOptions.ParentBkgrndForm = True
      BackgroundOptions.ParentPicture = True
      BackgroundOptions.ParentGlass = True
      TabOrder = 0
    end
  end
  object ButtonsPanel: TEffectsPanel
    Left = 0
    Top = 294
    Width = 412
    Height = 34
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    object BitBtnOk: TButton
      Left = 252
      Top = 5
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object BitBtnCancel: TButton
      Left = 332
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object BitBtnAbout: TButton
      Left = 172
      Top = 5
      Width = 75
      Height = 25
      Caption = 'About'
      TabOrder = 0
      Visible = False
      OnClick = BitBtnAboutClick
    end
  end
  object FormTransitions: TFormTransitions
    OnAfterShow = FormTransitionsAfterShow
    Left = 17
    Top = 9
  end
end
