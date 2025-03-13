unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormCont, ExtCtrls, ComCtrls, teForm, Menus, TransEff, teTimed, teMasked,
  teBlock, teBlend, Jpeg, teRoll, tePage;

type
  TMainForm = class(TForm)
    MainFormContainer: TFormContainer;
    FormTransitions: TFormTransitions;
    TransitionList: TTransitionList;
    Transition: TRollTransition;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TransitionStartTransition(Sender: TObject);
    procedure TransitionAfterTransition(Sender: TObject);
  private
  protected
  end;

var
  MainForm: TMainForm;

implementation

uses Navigator, About, teBkgrnd, mmSystem, teRender;

{$R *.DFM}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Format(Caption, [BilleniumEffectsVersion]);
  FormAbout := TFormAbout.Create(Self);
  FormAbout.Show;
  repeat
    Application.ProcessMessages;
  until not FormAbout.Visible;
  FormAbout.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FormNavigator := TFormNavigator(MainFormContainer.CreateForm(TFormNavigator));
  MainFormContainer.ShowForm(FormNavigator, True);
end;

procedure TMainForm.TransitionStartTransition(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
end;

procedure TMainForm.TransitionAfterTransition(Sender: TObject);
begin
  FormNavigator.Initialize;
  Screen.Cursor := crDefault;
end;

end.
