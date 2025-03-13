unit FormContIntro;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormCont, StdCtrls, ExtCtrls, teBlend, teMasked, teDiagon, teWipe,
  teDrip, TransEff, teTimed, teSlide, teCtrls, teForm;

type
  TFormContainerIntro = class(TFCEmbeddedForm)
    LabelText: TLabel;
  private
  public
  end;

var
  FormContainerIntro: TFormContainerIntro;

implementation

{$R *.DFM}

end.
