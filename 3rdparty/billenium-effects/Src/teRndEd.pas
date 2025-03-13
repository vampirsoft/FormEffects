unit teRndEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, ComCtrls, StdCtrls, ExtCtrls, teCtrls, Buttons, TransEff,
  teEditor, teModEdit, teRandom, FormCont, teRndLis;

type
  TRandomTransitionEditor = class(TTransitionEffectEditor)
    PanelRandom: TEffectsPanel;
    RadioGroupMode: TRadioGroup;
    FormContainerList: TFormContainer;
  private
    RndTrListEditor: TRndTrListEditor;

    procedure BeforeTransition(Sender: TObject);
  protected
  public
    procedure Initialize(TransitionValue: TTransitionEffect); override;
    procedure ReadValues; override;
    procedure WriteValues; override;
    function  RandomTransition: TRandomTransition;
  end;

var
  RandomTransitionEditor: TRandomTransitionEditor;

implementation

{$R *.DFM}

uses teTrLEdi;

type
  TRandomTransitionHack = class(TRandomTransition);

procedure TRandomTransitionEditor.Initialize(TransitionValue: TTransitionEffect);
begin
  inherited;

  RandomTransition.OnBeforeTransition := BeforeTransition;
  RndTrListEditor :=
    TRndTrListEditor(FormContainerList.CreateShowForm(TRndTrListEditor, False));
  RndTrListEditor.RandomTransition := RandomTransition;

  RndTrListEditor.Initialize(
    TRandomTransitionHack(RandomTransition).FTransitions, nil);
end;

procedure TRandomTransitionEditor.BeforeTransition(Sender: TObject);
begin
  RndTrListEditor.SelectTransition(RandomTransition.SelectedTransition);
end;

procedure TRandomTransitionEditor.ReadValues;
begin
  inherited;

  RadioGroupMode.ItemIndex := Ord(RandomTransition.Mode);
end;

procedure TRandomTransitionEditor.WriteValues;
begin
  inherited;

  RandomTransition.Mode := TTERndMode(RadioGroupMode.ItemIndex);
end;

function TRandomTransitionEditor.RandomTransition: TRandomTransition;
begin
  Result := Transition as TRandomTransition;
end;

initialization

  RegisterClasses([TRandomTransitionEditor]);

end.
