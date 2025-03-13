object FormBkOptions: TFormBkOptions
  Left = 246
  Top = 145
  Caption = 'Background options'
  ClientHeight = 446
  ClientWidth = 475
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
    Left = 11
    Top = 8
    Width = 453
    Height = 431
    AutoSize = False
    Caption = 
      'All our visual controls have a BackgroundOptions property that a' +
      'llows them to have incredible backgrounds.'#13#10'The background of a ' +
      'control is organized in layers. All active layers are rendered a' +
      'nd the final result is their combination. The layers are (from d' +
      'own to top):'#13#10#13#10'· Opaque solid color layer: Set it to True to ma' +
      'ke sure the controls behave like a standard windowed control. Th' +
      'at is, its background is filled with the control'#39's Color. If you' +
      ' set it to False this layer is not painted, so the controls belo' +
      'w it are not obscured and show through (like a fully transparent' +
      ' glass).'#13#10'· Background form layer: Uses a form'#39's image as backgr' +
      'ound. This is useful if you need the greatest flexibility.'#13#10'· Pi' +
      'cture layer: Renders a picture in several modes. If this layer i' +
      's active then the opacity layer will automatically be enabled.'#13#10 +
      '· Glass layer: Renders a colored translucent glass. This effect ' +
      'works under every Windows versions, not only Windows 2000 or XP.' +
      #13#10#13#10'A control can inherit some or all of its background settings' +
      ' from their parents (see ParentOpaque, ParentBkgrndForm, ParentP' +
      'icture and ParentGlass). This goes further than just grabbing pa' +
      'rent'#39's resources, as it even adjust the rendering coordinates to' +
      ' avoid discontinued painting, so it is really easy to have a uni' +
      'form look and save resources, create application-wide watermarks' +
      ', textures, etc.'
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
