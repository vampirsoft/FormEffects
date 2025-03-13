object FormImage: TFormImage
  Left = 277
  Top = 233
  Caption = 'Image transitions'
  ClientHeight = 242
  ClientWidth = 351
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  BackgroundOptions.ParentBkgrndForm = True
  BackgroundOptions.ParentPicture = True
  PixelsPerInch = 96
  TextHeight = 13
  object LabelText: TLabel
    Left = 10
    Top = 10
    Width = 329
    Height = 221
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 
      'The TTEImage component displays pictures with several positionin' +
      'g options and natively supports transitions.'#13#10#13#10'These transition' +
      's can be executed both synchronous or asynchronously. Asynchrono' +
      'us transitions use a separate thead per transition, allowing sim' +
      'ultaneous execution of several transitions.'#13#10#13#10'Of course, the co' +
      'ntrol'#39's background can be translucent, use textures and so on, a' +
      's all our controls.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
end
