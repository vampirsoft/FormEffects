unit teModEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Buttons, ExtCtrls, TransEff, teCtrls, FormCont, teEditor, teForm;

{$INCLUDE teDefs.inc}

type
  TTransitionModalEditor = class(TForm)
    EditorPanel: TEffectsPanel;
    ButtonsPanel: TEffectsPanel;
    BitBtnOk: TButton;
    BitBtnCancel: TButton;
    FormContainer: TFormContainer;
    FormTransitions: TFormTransitions;
    BitBtnAbout: TButton;
    procedure BitBtnAboutClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormTransitionsAfterShow(Sender: TObject;
      const FirstTime: Boolean);
  public
    TransitionEditor: TTransitionEditor;

    procedure InitializeForChange(var Transition: TTransitionEffect);
    procedure InitializeForEdition(Transition: TTransitionEffect);
    procedure Apply;                  
  end;

  {$ifndef TE_NOHLP}
  TTEAboutClick = procedure;
  {$endif TE_NOHLP}

var
  TransitionModalEditor: TTransitionModalEditor;
  {$ifndef TE_NOHLP}
  TEAboutClick: TTEAboutClick = nil;
  {$endif TE_NOHLP}

implementation

{$R *.DFM}

procedure TTransitionModalEditor.InitializeForChange(
  var Transition: TTransitionEffect);
begin
  TransitionEditor :=
    TTransitionEditor(FormContainer.CreateForm(TTransitionEditor));
  teEditor.TransitionEditor := TransitionEditor;
  TransitionEditor.InitializeForChange(Transition);
  FormContainer.ShowForm(TransitionEditor, True);
end;

procedure TTransitionModalEditor.InitializeForEdition(
  Transition: TTransitionEffect);
begin
  TransitionEditor :=
    TTransitionEditor(FormContainer.CreateForm(TTransitionEditor));
  teEditor.TransitionEditor := TransitionEditor;
  TransitionEditor.InitializeForEdition(Transition);
  TransitionEditor.Color := Color;
  FormContainer.ShowForm(TransitionEditor, True);
end;

procedure TTransitionModalEditor.Apply;
begin
  TransitionEditor.Apply;
end;

procedure TTransitionModalEditor.BitBtnAboutClick(Sender: TObject);
begin
  if Assigned(TEAboutClick) then
    TEAboutClick;
end;

procedure TTransitionModalEditor.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ModalResult := mrCancel;
end;

procedure TTransitionModalEditor.FormTransitionsAfterShow(Sender: TObject;
  const FirstTime: Boolean);
begin
  TransitionEditor.AutoPreview;
end;

end.
