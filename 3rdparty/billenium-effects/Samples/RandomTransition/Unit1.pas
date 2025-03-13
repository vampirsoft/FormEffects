unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teWipe, teWFall, teSlide, teRoll, teRadial, tePush, teIntrlc, teFuse,
  teDrip, teDiagon, teCircle, teMasked, teBlock, teBmpMsk, TransEff,
  teTimed, teBlend, StdCtrls, teImage, teBlur, tePage, teRandom, jpeg;

type
  TForm1 = class(TForm)
    TransitionList1: TTransitionList;
    TEImage: TTEImage;
    ButtonNext: TButton;
    RandomTransition: TRandomTransition;
    procedure FormCreate(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
  private
    CurPic: Integer;
  public
    function NextPic: String;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CurPic := -1;
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
  Result := ExtractFilePath(Application.ExeName) + '..\Resources\' + FileName;
end;

procedure TForm1.ButtonNextClick(Sender: TObject);
begin
  TEImage.PrepareTransition(RandomTransition, False, False);
  TEImage.Picture.LoadFromFile(NextPic);
  TEImage.ExecuteTransition(False);
end;

end.
