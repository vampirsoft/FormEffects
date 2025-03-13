unit TransitionEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, teForm, ExtCtrls, teCtrls, TransEff, FormCont;

type
  TFormTransitionEditor = class(TFCEmbeddedForm)
    LabelText: TLabel;
    BitBtnEditor: TBitBtn;
    procedure BitBtnEditorClick(Sender: TObject);
  private
  public
  end;

var
  FormTransitionEditor: TFormTransitionEditor;

implementation

uses Navigator, teEditor, teAllTr;

{$R *.DFM}

procedure TFormTransitionEditor.BitBtnEditorClick(Sender: TObject);
begin
  ChangeTransition((Parent.Parent as TFormNavigator).TransEffct);
end;

end.
