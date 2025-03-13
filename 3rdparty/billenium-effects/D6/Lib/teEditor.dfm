object TransitionEditor: TTransitionEditor
  Left = 262
  Top = 394
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsNone
  Caption = 'TransitionEditor'
  ClientHeight = 292
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Bevel: TBevel
    Left = 0
    Top = 0
    Width = 186
    Height = 292
    Shape = bsRightLine
    Style = bsRaised
  end
  object LabelTransitions: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 14
    Caption = '&Transition'
    FocusControl = ComboBoxTransitions
    Transparent = True
  end
  object ImageB: TImage
    Left = 150
    Top = 2
    Width = 33
    Height = 32
    Visible = False
  end
  object ImageA: TImage
    Left = 113
    Top = 2
    Width = 33
    Height = 32
    Stretch = True
    Visible = False
  end
  object LabelAutoPreview: TLabel
    Left = 25
    Top = 264
    Width = 66
    Height = 14
    Caption = '&Auto preview'
    FocusControl = CheckBoxAutoPreview
    Transparent = True
  end
  object FormContainer: TFormContainer
    Left = 186
    Top = 0
    Width = 224
    Height = 292
    Align = alRight
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    TabOrder = 2
  end
  object ComboBoxTransitions: TComboBox
    Left = 8
    Top = 23
    Width = 169
    Height = 22
    Style = csDropDownList
    DropDownCount = 20
    ItemHeight = 14
    TabOrder = 0
    OnChange = ComboBoxTransitionsChange
  end
  object BitBtnPreview: TButton
    Left = 104
    Top = 259
    Width = 75
    Height = 25
    Caption = '&Preview'
    TabOrder = 1
    OnClick = BitBtnPreviewClick
  end
  object TEImagePreview: TTEImage
    Left = 8
    Top = 56
    Width = 169
    Height = 193
    BackgroundOptions.ParentPicture = True
    PictureMode = fcpmZoom
    TabOrder = 3
  end
  object CheckBoxAutoPreview: TCheckBox
    Left = 7
    Top = 265
    Width = 13
    Height = 13
    Caption = '&Auto preview'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CheckBoxAutoPreviewClick
  end
  object FormTransitions: TFormTransitions
    BackgroundOptions.ParentOpaque = True
    BackgroundOptions.ParentBkgrndForm = True
    BackgroundOptions.ParentPicture = True
    BackgroundOptions.ParentGlass = True
    OnAfterShow = FormTransitionsAfterShow
    Left = 194
    Top = 8
  end
  object TimerPreview: TTimer
    Enabled = False
    OnTimer = TimerPreviewTimer
    Left = 226
    Top = 8
  end
end
