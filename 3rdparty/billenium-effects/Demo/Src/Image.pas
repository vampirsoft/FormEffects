unit Image;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, teCtrls, teForm, FormCont;

type
  TFormImage = class(TFCEmbeddedForm)
    LabelText: TLabel;
  private
  public
  end;

var
  FormImage: TFormImage;

implementation

{$R *.DFM}

uses Transeff;

end.
