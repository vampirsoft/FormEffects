object FormLRU: TFormLRU
  Left = 288
  Top = 388
  Caption = 'LRU forms'
  ClientHeight = 319
  ClientWidth = 444
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnShow = FormShow
  BackgroundOptions.ParentBkgrndForm = True
  BackgroundOptions.ParentPicture = True
  PixelsPerInch = 96
  TextHeight = 14
  object LabelView: TLabel
    Left = 29
    Top = 287
    Width = 93
    Height = 16
    Cursor = crHandPoint
    Caption = 'View LRU forms'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    OnClick = LabelViewClick
  end
  object LabelText: TLabel
    Left = 12
    Top = 12
    Width = 420
    Height = 269
    AutoSize = False
    Caption = 
      'FormContainer can maintain a LRU (Last Recently Used) list of fo' +
      'rms showed into it.'#13#10#13#10'You can navigate forward and backward thr' +
      'ough this list.'#13#10#13#10'By default, if a form is destroyed it won'#39't b' +
      'e included in the list, but you can include it if you set '#39'SaveL' +
      'RUDestroyedForms'#39' to '#39'True'#39'. FormContainer even includes a mecha' +
      'nism to re-create previously destroyed forms.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object CheckBoxViewLRU: TCheckBox
    Left = 12
    Top = 290
    Width = 12
    Height = 11
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = CheckBoxViewLRUClick
  end
  object ListBoxLRU: TListBox
    Left = 182
    Top = 15
    Width = 249
    Height = 284
    IntegralHeight = True
    ItemHeight = 14
    TabOrder = 1
    Visible = False
  end
  object TransitionList: TTransitionList
    Left = 24
    Top = 192
    object Transition: TSlideTransition
      Milliseconds = 500
      Direction = tedDownRight
      ElasticSrc = True
    end
  end
end
