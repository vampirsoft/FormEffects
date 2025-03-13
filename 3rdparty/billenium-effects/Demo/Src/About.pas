unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, FormCont, teForm, TransEff, teTimed, teMasked,
  teCircle, jpeg, teBlock, teSlide, teBmpMsk, teAnim, teFormAn, teZFrAn,
  teWipe, tePixelt, teBlend;

type
  TFormAbout = class(TForm)
    Image: TImage;
    LabelVersion: TLabel;
    TransitionList1: TTransitionList;
    FormTransitions: TFormTransitions;
    Timer: TTimer;
    Transition: TBmpMaskTransition;
    TransitionVersion: TPixelateTransition;
    Transition1: TWipeTransition;
    TEAnimationList1: TTEAnimationList;
    Animation1: TTEZoomFrameAnimation;
    Transition2: TBlendTransition;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TransitionAfterTransition(Sender: TObject);
    procedure TransitionVersionAfterTransition(Sender: TObject);
  private
  public
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.DFM}

uses teRender, mmSystem;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  LabelVersion.Caption := BilleniumEffectsVersion;

  SetWindowRgn(Handle, CreateEllipticRgn(0, 0, Width, Height), False);
  Image.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Box.jpg');

  Transition.Mask.LoadFromFile(ExtractFilePath(Application.ExeName) + 'sand.bmp');

  if not RGBDevice(False) then
    ShowMessage('This demo has to be viewed under true' + #13#10 +
                'color or high color video mode');
end;

procedure TFormAbout.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  Close;
end;

procedure TFormAbout.TransitionAfterTransition(Sender: TObject);
begin
  TransitionVersion.Prepare(LabelVersion.Parent, LabelVersion.BoundsRect);
  try
    LabelVersion.Visible := True;
    TransitionVersion.Execute;
  finally
    TransitionVersion.UnPrepare;
  end;
end;

procedure TFormAbout.TransitionVersionAfterTransition(Sender: TObject);
begin
  Timer.Enabled := True;
end;

end.
