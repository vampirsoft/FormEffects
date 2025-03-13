unit TransitionSamples;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, FormCont,
  TransEff, StdCtrls, Buttons, ExtCtrls, teBmpMsk, teTimed, teBlend,
  teCtrls, teMasked, teBlock, teCircle, teFuse, teDrip, teDiagon, teIntrlc,
  teRadial, tePush, teSlide, teRoll, teWFall, teWipe, Menus, Dialogs,
  tePixelt, teBlur, tePage;

type
  TFormTransitionSamples = class(TFCEmbeddedForm)
    TransitionList: TTransitionList;
    Transition1: TBlendTransition;
    TransitionBmpTwirl1: TBmpMaskTransition;
    TransitionBmpTwirl2: TBmpMaskTransition;
    TransitionBmpWiper1: TBmpMaskTransition;
    TransitionBmpWiper2: TBmpMaskTransition;
    TransitionBmpSand: TBmpMaskTransition;
    Panel1: TPanel;
    FormContainer: TFormContainer;
    EffectsPanel1: TEffectsPanel;
    BitBtnBack: TBitBtn;
    BitBtnNext: TBitBtn;
    Transition2: TBlockTransition;
    Transition3: TBlockTransition;
    Transition4: TBlockTransition;
    Transition5: TBlockTransition;
    Transition6: TCircleTransition;
    Transition7: TCircleTransition;
    Transition8: TCircleTransition;
    Transition9: TDiagonalTransition;
    Transition10: TDiagonalTransition;
    Transition11: TDiagonalTransition;
    Transition12: TDripTransition;
    Transition14: TFuseTransition;
    Transition15: TFuseTransition;
    Transition16: TInterlacedTransition;
    Transition17: TInterlacedTransition;
    Transition18: TInterlacedTransition;
    Transition19: TPushTransition;
    Transition20: TRadialTransition;
    Transition21: TRadialTransition;
    Transition22: TRadialTransition;
    Transition23: TRadialTransition;
    Transition24: TRadialTransition;
    Transition25: TRadialTransition;
    Transition26: TRadialTransition;
    Transition27: TRollTransition;
    Transition28: TRollTransition;
    Transition29: TSlideTransition;
    Transition30: TSlideTransition;
    Transition31: TSlideTransition;
    Transition32: TSlideTransition;
    Transition33: TSlideTransition;
    Transition34: TSlideTransition;
    Transition35: TWaterfallTransition;
    Transition36: TWipeTransition;
    Transition37: TWipeTransition;
    Transition38: TWipeTransition;
    Transition39: TWipeTransition;
    Transition40: TWipeTransition;
    SpeedButtonIndex: TSpeedButton;
    PopupMenu: TPopupMenu;
    TransitionBmpCrowd: TBmpMaskTransition;
    Transition41: TPageTransition;
    Transition42: TPageTransition;
    Transition43: TBlurTransition;
    Transition44: TBlurTransition;
    Transition45: TPixelateTransition;
    Transition46: TPixelateTransition;
    procedure FormCreate(Sender: TObject);
    procedure BitBtnBackClick(Sender: TObject);
    procedure BitBtnNextClick(Sender: TObject);
    procedure SpeedButtonIndexClick(Sender: TObject);
  private
    Index: Integer;

    procedure MenuClick(Sender: TObject);
    procedure Next;
    procedure Back;
  public
  end;

var
  FormTransitionSamples: TFormTransitionSamples;

implementation

uses SampTr1;

{$R *.DFM}

procedure TFormTransitionSamples.FormCreate(Sender: TObject);
var
  i,
  Count: Integer;
  CurTransition: TClass;
  MenuItem: TMenuItem;
begin
  Index := -1;

  SampTrForm1 := TSampTrForm1(FormContainer.CreateForm(TSampTrForm1));
  SampTrForm1.Initialize('launch.jpg');
  SampTrForm1 := TSampTrForm1(FormContainer.CreateForm(TSampTrForm1));
  SampTrForm1.Initialize('earth.jpg');
  SampTrForm1 := TSampTrForm1(FormContainer.CreateForm(TSampTrForm1));
  SampTrForm1.Initialize('galaxy.jpg');

  FormContainer.ShowForm(FormContainer.Forms[1], False);
  (FormContainer.Form as TSampTrForm1).ShowText('Press ''Next'' button');

  TransitionBmpTwirl1.Mask.LoadFromFile(ExtractFilePath(Application.ExeName) +
    'Twirl.bmp');
  TransitionBmpTwirl2.Mask.Assign(TransitionBmpTwirl1.Mask);
  TransitionBmpWiper1.Mask.LoadFromFile(ExtractFilePath(Application.ExeName) +
    'Wiper.bmp');
  TransitionBmpWiper2.Mask.Assign(TransitionBmpWiper1.Mask);
  TransitionBmpSand  .Mask.LoadFromFile(ExtractFilePath(Application.ExeName) +
    'Sand.bmp');
  TransitionBmpCrowd .Mask.LoadFromFile(ExtractFilePath(Application.ExeName) +
    'Crowd.bmp');

  BitBtnBack.Enabled := False;

  i        := 0;
  Count    := 1;
  MenuItem := nil;
  CurTransition := nil;
  repeat
    if CurTransition <> TransitionList[i].ClassType
    then
    begin
      Count := 1;
      CurTransition := TransitionList[i].ClassType;
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := TransitionList[i].Description;
      MenuItem.Tag     := i;
      MenuItem.OnClick := MenuClick;
      PopupMenu.Items.Add(MenuItem);
    end
    else
    begin
      Inc(Count);
      MenuItem.Caption := Format('%s (x %d)', [TransitionList[i].Description, Count]);
    end;
    Inc(i);
  until i = TransitionList.TransitionCount;
end;

procedure TFormTransitionSamples.Next;
var
  FormIndex: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    Inc(Index);
    FormIndex := FormContainer.IndexOf(FormContainer.Form);
    Inc(FormIndex);
    if FormIndex > FormContainer.FormCount then
      FormIndex := 1;
    TransitionList[Index].Prepare(Self, ClientRect);
    try
      BitBtnNext.Enabled := Index < TransitionList.TransitionCount-1;
      BitBtnBack.Enabled := Index >= 0;
      FormContainer.ShowForm(FormContainer.Forms[FormIndex], False);
      (FormContainer.Form as TSampTrForm1).ShowText(
        TransitionList.Transitions[Index].Description);
      TransitionList[Index].Execute;
    finally
      TransitionList[Index].UnPrepare;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormTransitionSamples.Back;
var
  FormIndex: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    FormIndex := FormContainer.IndexOf(FormContainer.Form);
    Inc(FormIndex);
    if FormIndex > FormContainer.FormCount then
      FormIndex := 1;
    TransitionList[Index].Reversed := True;
    TransitionList[Index].Prepare(Self, ClientRect);
    try
      BitBtnBack.Enabled := Index > 0;
      BitBtnNext.Enabled := True;
      FormContainer.ShowForm(FormContainer.Forms[FormIndex], False);
      (FormContainer.Form as TSampTrForm1).ShowText(
        TransitionList.Transitions[Index].Description);
      TransitionList[Index].Execute;
    finally
      TransitionList[Index].UnPrepare;
    end;
    TransitionList[Index].Reversed := False;
    Dec(Index);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormTransitionSamples.BitBtnBackClick(Sender: TObject);
begin
  Back;
end;

procedure TFormTransitionSamples.BitBtnNextClick(Sender: TObject);
begin
  Next;
end;

procedure TFormTransitionSamples.SpeedButtonIndexClick(Sender: TObject);
var
  P: TPoint;
begin
  P := SpeedButtonIndex.ClientToScreen(Point(0, -2));
  PopupMenu.Popup(P.x, P.y);
end;

procedure TFormTransitionSamples.MenuClick(Sender: TObject);
begin
  Index := (Sender as TMenuItem).Tag - 1;
  Next;
end;

end.
