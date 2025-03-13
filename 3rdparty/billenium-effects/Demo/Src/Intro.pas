unit Intro;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, teCtrls, teForm, FormCont;

type
  TFormIntro = class(TFCEmbeddedForm)
    LabelText: TLabel;
  private
  public
  end;

var
  FormIntro: TFormIntro;

implementation

{$R *.DFM}

uses Transeff;

end.
