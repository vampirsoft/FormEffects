unit ImgSample;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, teImage, TransEff, teRandom, teWipe, teWFall, teSlide, teRoll,
  teRadial, tePush, tePage, teIntrlc, teFuse, teDrip, teDiagon, teCircle,
  teBlur, teMasked, teBlock, teTimed, teBlend, jpeg, teForm, ExtCtrls, FormCont;

type
  TFormImageSample = class(TFCEmbeddedForm)
    TEImage1: TTEImage;
    TEImage2: TTEImage;
    TEImage3: TTEImage;
    TEImage4: TTEImage;
    TEImage5: TTEImage;
    TEImage6: TTEImage;
    TEImage7: TTEImage;
    TEImage8: TTEImage;
    TEImage9: TTEImage;
    TransitionList1: TTransitionList;
    Transition1: TRandomTransition;
    TimerExe: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerExeTimer(Sender: TObject);
  private
  public
  end;

var
  FormImageSample: TFormImageSample;

implementation

{$R *.DFM}

procedure TFormImageSample.FormCreate(Sender: TObject);

  procedure LoadPics(Image: TTEImage; PicVisible: Boolean);
  begin
    Image.PictureVisible := PicVisible;
    Image.BackgroundOptions.Picture.LoadFromFile('earth.jpg');
    Image.Picture.LoadFromFile('galaxy.jpg');
  end;

begin
  LoadPics(TEImage1, True);
  LoadPics(TEImage2, False);
  LoadPics(TEImage3, True);
  LoadPics(TEImage4, False);
  LoadPics(TEImage5, True);
  LoadPics(TEImage6, False);
  LoadPics(TEImage7, True);
  LoadPics(TEImage8, False);
  LoadPics(TEImage9, True);
end;

procedure TFormImageSample.FormShow(Sender: TObject);
begin
  TimerExe.Enabled := True;
end;

procedure TFormImageSample.TimerExeTimer(Sender: TObject);

  procedure PrepareTransition(Transition: TTransitionEffect;
    Image: TTEImage);
  begin
    Image.PrepareTransition(Transition, True, False);
    Image.PictureVisible := not Image.PictureVisible;
  end;

  procedure ExeTransition(Transition: TTransitionEffect; Image: TTEImage);
  begin
    Image.ExecuteTransition(False);
  end;

begin
  TimerExe.Enabled  := False;
  TimerExe.Interval := 4000;
  PrepareTransition(Transition1, TEImage1);
  PrepareTransition(Transition1, TEImage2);
  PrepareTransition(Transition1, TEImage3);
  PrepareTransition(Transition1, TEImage4);
  PrepareTransition(Transition1, TEImage5);
  PrepareTransition(Transition1, TEImage6);
  PrepareTransition(Transition1, TEImage7);
  PrepareTransition(Transition1, TEImage8);
  PrepareTransition(Transition1, TEImage9);

  ExeTransition(Transition1, TEImage1);
  ExeTransition(Transition1, TEImage2);
  ExeTransition(Transition1, TEImage3);
  ExeTransition(Transition1, TEImage4);
  ExeTransition(Transition1, TEImage5);
  ExeTransition(Transition1, TEImage6);
  ExeTransition(Transition1, TEImage7);
  ExeTransition(Transition1, TEImage8);
  ExeTransition(Transition1, TEImage9);
  TimerExe.Enabled := True;
end;

end.
