unit Transitions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, teCtrls, teForm, FormCont;

type
  TFormTransitionEffects = class(TFCEmbeddedForm)
    LabelText: TLabel;
  private
  public
  end;

var
  FormTransitionEffects: TFormTransitionEffects;

implementation

{$R *.DFM}

uses Transeff;

end.
