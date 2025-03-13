unit BkOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, teCtrls, teForm, FormCont;

type
  TFormBkOptions = class(TFCEmbeddedForm)
    LabelText: TLabel;
  private
  public
  end;

var
  FormBkOptions: TFormBkOptions;

implementation

{$R *.DFM}

uses Transeff;

end.
