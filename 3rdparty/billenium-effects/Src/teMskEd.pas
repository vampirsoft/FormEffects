unit teMskEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, ExtCtrls, teCtrls, StdCtrls, ComCtrls, Buttons,
  TransEff;

{$INCLUDE teDefs.inc}

type
  TMaskedTransitionEditor = class(TTransitionEffectEditor)
    PanelStyle: TEffectsPanel;
    LabelStyle: TLabel;
    EditStyle: TEdit;
    UpDownStyle: TUpDown;
    CheckBoxRndStyle: TCheckBox;
    LabelRndStyle: TLabel;
    PanelSubstyle: TEffectsPanel;
    LabelSubstyle: TLabel;
    EditSubstyle: TEdit;
    UpDownSubstyle: TUpDown;
    CheckBoxRndSubstyle: TCheckBox;
    LabelRndSubstyle: TLabel;
    PanelSmoothingLevel: TEffectsPanel;
    LabelSmoothingLevel: TLabel;
    EditSmoothingLevel: TEdit;
    UpDownSmoothingLevel: TUpDown;
    PanelMsk: TEffectsPanel;
    procedure EditStyleChange(Sender: TObject);
    procedure CheckBoxRndStyleClick(Sender: TObject);
    procedure CheckBoxRndSubstyleClick(Sender: TObject);
  protected
    FirstChange: Boolean;

    procedure StyleActivation(StyleChanged: Boolean);
  public
    procedure CheckAssignment(Transition: TTransitionEffect); override;
    procedure Initialize(TransitionValue: TTransitionEffect); override;
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  MaskedTransitionEditor: TMaskedTransitionEditor;

implementation

{$R *.DFM}

uses teMasked, TypInfo, teEditor;

procedure TMaskedTransitionEditor.CheckAssignment(Transition: TTransitionEffect);
begin
  with Transition as TMaskedTransition do
  begin
    if SubStyle > 1 then
      SubStyle := 1;
    if Style > 1 then
      Style := 1;
  end;
end;

{ TMaskedTransitionEditor }

procedure TMaskedTransitionEditor.Initialize(
  TransitionValue: TTransitionEffect);
begin
  inherited;

  FirstChange := True;

  PanelStyle.Visible :=
    GetPropInfo(Transition.ClassType, 'Style', [tkInteger]) <> nil;

  PanelSubstyle.Visible :=
    GetPropInfo(Transition.ClassType, 'Substyle', [tkInteger]) <> nil;

  PanelSmoothingLevel.Visible :=
    GetPropInfo(Transition.ClassType, 'SmoothingLevel', [tkInteger]) <> nil;

  UpDownSmoothingLevel.Max := (Transition as TMaskedTransition).MaxSmoothingLevel;
end;

procedure TMaskedTransitionEditor.ReadValues;
var
  MaskedTransition: TMaskedTransition;
  Msg: TMsg;
begin
  inherited;

  MaskedTransition := Transition as TMaskedtransition;

  if MaskedTransition.Style = 0
  then CheckBoxRndStyle.Checked := True
  else
  begin
    UpDownStyle.Max      := MaskedTransition.CountOfStyles;
    UpDownStyle.Position := MaskedTransition.Style;
    // Process just the needed events
    while PeekMessage(Msg, EditStyle.Handle, CM_TEXTCHANGED, CM_TEXTCHANGED, PM_REMOVE) do
      DispatchMessage(Msg);
  end;

  if MaskedTransition.Substyle = 0
  then CheckBoxRndSubstyle.Checked := True
  else
  begin
    UpDownSubstyle.Max      :=
      MaskedTransition.CountOfSubstyles(MaskedTransition.Style);
    UpDownSubstyle.Position := MaskedTransition.Substyle;
  end;

  UpDownSmoothingLevel.Position := MaskedTransition.SmoothingLevel;
end;

procedure TMaskedTransitionEditor.WriteValues;
var
  MaskedTransition: TMaskedTransition;
begin
  inherited;

  MaskedTransition := Transition as TMaskedtransition;

  MaskedTransition.Style          := UpDownStyle         .Position;
  MaskedTransition.Substyle       := UpDownSubstyle      .Position;
  MaskedTransition.SmoothingLevel := UpDownSmoothingLevel.Position;
end;

procedure TMaskedTransitionEditor.EditStyleChange(Sender: TObject);
var
  MaskedTransition: TMaskedTransition;
begin
  if Transition <> nil then
  begin
    MaskedTransition := Transition as TMaskedTransition;

    if(UpDownSubstyle.Position > 1) and (not FirstChange) then
      UpDownSubstyle.Position := 1;
      UpDownSubstyle.Max :=
        MaskedTransition.CountOfSubstyles(UpDownStyle.Position);
      UpDownSubstyle.Refresh;
  end;
  FirstChange := False;

  TransitionEditor.AutoPreview;
end;

procedure TMaskedTransitionEditor.StyleActivation(StyleChanged: Boolean);
begin
  UpDownStyle        .Enabled := not CheckBoxRndStyle.Checked;
  CheckBoxRndSubstyle.Enabled := not CheckBoxRndStyle.Checked;
  LabelRndSubstyle   .Enabled := not CheckBoxRndStyle.Checked;
  UpDownSubstyle     .Enabled :=
    (not CheckBoxRndStyle.Checked) and (not CheckBoxRndSubstyle.Checked);

  if StyleChanged then
  begin
    if CheckBoxRndStyle.Checked
    then
    begin
      UpDownStyle.Min      := 0;
      UpDownStyle.Position := 0;
    end
    else
    begin
      UpDownStyle.Position := 1;
      UpDownStyle.Min      := 1;
      UpDownSubStyle.Min   := 1;
    end;
  end;

  if CheckBoxRndStyle.Checked or CheckBoxRndSubstyle.Checked
  then
  begin
    UpDownSubstyle.Min      := 0;
    UpDownSubstyle.Position := 0;
  end
  else
  begin
    UpDownSubstyle.Position := 1;
    UpDownSubstyle.Min      := 1;
  end;
end;

procedure TMaskedTransitionEditor.CheckBoxRndStyleClick(Sender: TObject);
begin
  StyleActivation(True);
  TransitionEditor.AutoPreview;
end;

procedure TMaskedTransitionEditor.CheckBoxRndSubstyleClick(
  Sender: TObject);
begin
  StyleActivation(False);
  TransitionEditor.AutoPreview;
end;

initialization

  RegisterClasses([TMaskedTransitionEditor]);

end.
