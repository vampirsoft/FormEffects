unit teFuseEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, ExtCtrls, teCtrls, StdCtrls, ComCtrls, Buttons;

{$INCLUDE teDefs.inc}

type
  TFuseTransitionEditor = class(TTransitionEffectEditor)
    LabelStyle: TLabel;
    LabelRndStyle: TLabel;
    EditStyle: TEdit;
    UpDownStyle: TUpDown;
    CheckBoxRndStyle: TCheckBox;
    procedure CheckBoxRndStyleClick(Sender: TObject);
  protected
    procedure StyleActivation;
  public
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  FuseTransitionEditor: TFuseTransitionEditor;

implementation

uses teFuse, teEditor;

{$R *.DFM}

{ TFuseTransitionEditor }

procedure TFuseTransitionEditor.ReadValues;
var
  FuseTransition: TFuseTransition;
begin
  inherited;

  FuseTransition := Transition as TFuseTransition;

  if FuseTransition.Style = 0
  then CheckBoxRndStyle.Checked := True
  else
  begin
    UpDownStyle.Max      := FuseTransition.CountOfStyles;
    UpDownStyle.Position := FuseTransition.Style;
  end;
end;

procedure TFuseTransitionEditor.WriteValues;
var
  FuseTransition: TFuseTransition;
begin
  inherited;

  FuseTransition := Transition as TFuseTransition;

  FuseTransition.Style := UpDownStyle.Position;
end;

procedure TFuseTransitionEditor.StyleActivation;
begin
  EditStyle  .Enabled := not CheckBoxRndStyle.Checked;
  UpDownStyle.Enabled := not CheckBoxRndStyle.Checked;

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
  end;
end;

procedure TFuseTransitionEditor.CheckBoxRndStyleClick(Sender: TObject);
begin
  StyleActivation;
  TransitionEditor.AutoPreview;
end;

initialization

  RegisterClasses([TFuseTransitionEditor]);

end.
 
