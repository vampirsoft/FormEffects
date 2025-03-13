unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TransEff, teRandom, teImage, teWipe, teWFall, teSlide, teRoll,
  teRadial, tePush, tePage, teIntrlc, teFuse, teDrip, teDiagon, teCircle,
  teBlur, teMasked, teBlock, teTimed, teBlend, jpeg, teForm;

const
  CM_ANIMLOGO = CM_BASE + 234;

type
  TForm1 = class(TForm)
    TEImage1: TTEImage;
    TransitionList1: TTransitionList;
    RandomTransition: TRandomTransition;
    ButtonNext: TButton;
    TEImage2: TTEImage;
    CheckBoxFullArea: TCheckBox;
    CheckBoxWait: TCheckBox;
    LogoTransition: TSlideTransition;
    FormTransitions1: TFormTransitions;
    procedure ButtonNextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LogoTransitionAfterTransition(Sender: TObject);
    procedure FormTransitions1AfterShow(Sender: TObject;
      const FirstTime: Boolean);
  private
    CurPic: Integer;

    procedure CMAnimLogo(var Msg: TMessage); message CM_ANIMLOGO;
  public
    procedure AnimateLogo;
    function  NextPic: String;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CurPic := -1;
  TEImage1.Picture.LoadFromFile(NextPic);
end;

function TForm1.NextPic: String;
var
  FileName: String;
begin
  Inc(CurPic);
  case CurPic of
    0: FileName := 'earth.jpg';
    1: FileName := 'galaxy.jpg';
    else
    begin
      FileName := 'earth.jpg';
      CurPic   := 0;
    end;
  end;
  Result := ExtractFilePath(Application.ExeName) + '..\..\Resources\' + FileName;
end;

procedure TForm1.ButtonNextClick(Sender: TObject);
begin
  TEImage1.PrepareTransition(RandomTransition, CheckBoxFullArea.Checked, False);
  TEImage1.Picture.LoadFromFile(NextPic);
  TEImage1.ExecuteTransition(CheckBoxWait.Checked);
end;

procedure TForm1.LogoTransitionAfterTransition(Sender: TObject);
begin
  PostMessage(Handle, CM_ANIMLOGO, 0, 0);
end;

procedure TForm1.AnimateLogo;
begin
  if not(TEImage2.TransitionPrepared or TEImage2.TransitionExecuting) then
  begin
    LogoTransition.Reversed := TEImage2.PictureVisible;
    TEImage2.PrepareTransition(LogoTransition, False, False);
    TEImage2.PictureVisible := not TEImage2.PictureVisible;
    TEImage2.ExecuteTransition(False);
  end;
end;

procedure TForm1.CMAnimLogo(var Msg: TMessage);
begin
  AnimateLogo;
end;

procedure TForm1.FormTransitions1AfterShow(Sender: TObject;
  const FirstTime: Boolean);
begin
  AnimateLogo;
end;

end.
