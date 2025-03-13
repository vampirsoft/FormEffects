unit LRU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FormCont, TransEff, teTimed, teSlide, teCtrls, teForm;

type
  TFormLRU = class(TFCEmbeddedForm)
    TransitionList: TTransitionList;
    Transition: TSlideTransition;
    CheckBoxViewLRU: TCheckBox;
    LabelView: TLabel;
    LabelText: TLabel;
    ListBoxLRU: TListBox;
    procedure FormShow(Sender: TObject);
    procedure CheckBoxViewLRUClick(Sender: TObject);
    procedure LabelViewClick(Sender: TObject);
  private
  public
  end;
var
  FormLRU: TFormLRU;

implementation

uses Navigator;

{$R *.DFM}

var
  TheNavigator: TFormNavigator;

procedure TFormLRU.FormShow(Sender: TObject);
var
  i: Integer;
begin
  TheNavigator := Parent.Parent as TFormNavigator;

  for i:= TheNavigator.FormContainerNavigator.LRUFormCount downto 1 do
    ListBoxLRU.Items.Add(
      TheNavigator.FormContainerNavigator.LRUFormsData[i].Description);

  if TheNavigator.FormContainerNavigator.LRUFormIndex <> 0 then
    ListBoxLRU.ItemIndex := TheNavigator.FormContainerNavigator.LRUFormIndex-1;
end;

procedure TFormLRU.CheckBoxViewLRUClick(Sender: TObject);
begin
  Transition.Reversed := not ListBoxLRU.Visible;
  Transition.Prepare(ListBoxLRU.Parent, ListBoxLRU.BoundsRect);
  try
    ListBoxLRU.Visible := CheckBoxViewLRU.Checked;
    if Transition.Prepared then
      Transition.Execute;
  finally
    Transition.UnPrepare;
  end;
end;

procedure TFormLRU.LabelViewClick(Sender: TObject);
begin
  CheckBoxViewLRU.Checked := not CheckBoxViewLRU.Checked;
end;

end.
