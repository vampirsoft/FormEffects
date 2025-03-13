object Form1: TForm1
  Left = 328
  Top = 333
  Width = 465
  Height = 379
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 21
    Top = 24
    Width = 414
    Height = 267
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'This project shows how easy is to show/hide a form with a transi' +
      'tion effect.'#13#10#13#10'In this case we use the same transition but the ' +
      'closing effect is reversed.'#13#10#13#10'Please note that this closing eff' +
      'ect won'#39't be executed under Windows 95/98/NT/ME'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object BitBtn1: TBitBtn
    Left = 373
    Top = 319
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 0
    Kind = bkClose
  end
  object FormTransitions1: TFormTransitions
    DestroyTransitions = False
    HideTransition = Transition1
    ShowTransition = Transition1
    Left = 8
    Top = 8
  end
  object TransitionList1: TTransitionList
    Left = 40
    Top = 8
    object Transition1: TRollTransition
      Milliseconds = 2000
    end
  end
end
