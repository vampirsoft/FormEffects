object Form1: TForm1
  Left = 49
  Top = 121
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 500
  ClientWidth = 700
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TEImage1: TTEImage
    Left = 8
    Top = 8
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 0
  end
  object TEImage2: TTEImage
    Left = 242
    Top = 8
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 1
  end
  object TEImage3: TTEImage
    Left = 475
    Top = 8
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 2
  end
  object TEImage4: TTEImage
    Left = 8
    Top = 162
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 3
  end
  object TEImage5: TTEImage
    Left = 242
    Top = 162
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 4
  end
  object TEImage6: TTEImage
    Left = 475
    Top = 162
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 5
  end
  object TEImage7: TTEImage
    Left = 8
    Top = 316
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 6
  end
  object TEImage8: TTEImage
    Left = 242
    Top = 316
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 7
  end
  object TEImage9: TTEImage
    Left = 475
    Top = 316
    Width = 220
    Height = 140
    BackgroundOptions.PictureMode = fcpmZoom
    PictureMode = fcpmZoom
    TabOrder = 8
  end
  object ButtonExe: TButton
    Left = 604
    Top = 466
    Width = 91
    Height = 25
    Caption = '&Execute'
    TabOrder = 9
    OnClick = ButtonExeClick
  end
  object TransitionList1: TTransitionList
    Left = 560
    Top = 464
    object Transition1: TRandomTransition
      Transitions = {
        5450463011545445526E645472616E736974696F6E7300001054426C656E6454
        72616E736974696F6E000C4D696C6C697365636F6E647303E80300001054426C
        6F636B5472616E736974696F6E000C4D696C6C697365636F6E647303E8030650
        757A7A6C650900001054426C6F636B5472616E736974696F6E000C4D696C6C69
        7365636F6E647303E8030B426C6F636B486569676874021E0A426C6F636B5769
        647468021E055374796C650200085375625374796C65020000001054426C6F63
        6B5472616E736974696F6E000C4D696C6C697365636F6E647303E8030B426C6F
        636B48656967687402010A426C6F636B5769647468020100000F54426C757254
        72616E736974696F6E000C4D696C6C697365636F6E647303E803065261646975
        73023200001154436972636C655472616E736974696F6E000C4D696C6C697365
        636F6E647303E8030E536D6F6F7468696E674C6576656C0203055374796C6502
        00085375625374796C65020000001354446961676F6E616C5472616E73697469
        6F6E000C4D696C6C697365636F6E647303E8030E536D6F6F7468696E674C6576
        656C0202055374796C650200085375625374796C65020000000F544472697054
        72616E736974696F6E000C4D696C6C697365636F6E647303E803094469726563
        74696F6E070974656452616E646F6D00000F54467573655472616E736974696F
        6E000C4D696C6C697365636F6E647303E803055374796C65020000001554496E
        7465726C616365645472616E736974696F6E000C4D696C6C697365636F6E6473
        03E803055374796C650200085375625374796C65020000000F54506167655472
        616E736974696F6E000C4D696C6C697365636F6E647303E80309446972656374
        696F6E070974656452616E646F6D00000F54507573685472616E736974696F6E
        000C4D696C6C697365636F6E647303E80309446972656374696F6E0709746564
        52616E646F6D0000115452616469616C5472616E736974696F6E000C4D696C6C
        697365636F6E647303E8030E536D6F6F7468696E674C6576656C020205537479
        6C650200085375625374796C65020000000F54526F6C6C5472616E736974696F
        6E000C4D696C6C697365636F6E647303E80309446972656374696F6E07097465
        6452616E646F6D00001054536C6964655472616E736974696F6E000C4D696C6C
        697365636F6E647303E80309446972656374696F6E070974656452616E646F6D
        00001454576174657266616C6C5472616E736974696F6E000C4D696C6C697365
        636F6E647303E80309446972656374696F6E070974656452616E646F6D00000F
        54576970655472616E736974696F6E000C4D696C6C697365636F6E647303E803
        00000F54576970655472616E736974696F6E000C4D696C6C697365636F6E6473
        03E8030942616E645769647468026409446972656374696F6E07077465644C65
        66740A536D6F6F746842616E640900000F54576970655472616E736974696F6E
        000C4D696C6C697365636F6E647303E8030942616E6457696474680200094469
        72656374696F6E070974656452616E646F6D000000}
    end
  end
  object FormTransitions1: TFormTransitions
    Left = 512
    Top = 464
  end
end
