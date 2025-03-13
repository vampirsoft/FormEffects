unit Contact;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, teCtrls, teForm, FormCont;

type
  TFormContact = class(TFCEmbeddedForm)
    LabelText: TLabel;
  private
  public
  end;

var
  FormContact: TFormContact;

implementation

{$R *.DFM}

end.
