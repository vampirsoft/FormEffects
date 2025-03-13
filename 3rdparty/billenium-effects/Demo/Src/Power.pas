unit Power;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormCont, StdCtrls, Buttons, ExtCtrls, TransEff, teTimed, teCtrls, teForm,
  teFuse;

type
  TFormPower = class(TFCEmbeddedForm)
    PanelButton: TEffectsPanel;
    BitBtnMagic: TBitBtn;
    LabelMagic: TLabel;
    PanelForm: TEffectsPanel;
    FormContainer: TFormContainer;
    TransitionList: TTransitionList;
    Transition: TFuseTransition;
    procedure BitBtnMagicClick(Sender: TObject);
  private
  public
  end;

var
  FormPower: TFormPower;

implementation

uses Navigator, Bkgrnd;

{$R *.DFM}

procedure TFormPower.BitBtnMagicClick(Sender: TObject);
var
  SaveCursor: TCursor;
  Navigator: TFormNavigator;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    if FormContainer.Form = nil
    then
    begin
      Navigator := TFormNavigator(FormContainer.CreateForm(TFormNavigator));
      FormContainer.ShowFormEx(Navigator, True, Transition, nil, fcfaDefault);
    end
    else FormContainer.ShowFormEx(nil, True, Transition, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

end.
