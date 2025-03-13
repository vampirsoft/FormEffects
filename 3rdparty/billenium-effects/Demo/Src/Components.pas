unit Components;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, teCtrls, teForm, FormCont;

type
  TFormComponents = class(TFCEmbeddedForm)
    LabelText: TLabel;
  private
  public
  end;

var
  FormComponents: TFormComponents;

implementation

{$R *.DFM}

uses Transeff;

end.
