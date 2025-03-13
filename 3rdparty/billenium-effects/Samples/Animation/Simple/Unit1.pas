unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teAnim, teFormAn, teZFrAn, teForm;

type
  TForm1 = class(TForm)
    FormTransitions1: TFormTransitions;
    TEAnimationList1: TTEAnimationList;
    Animation1: TTEZoomFrameAnimation;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
