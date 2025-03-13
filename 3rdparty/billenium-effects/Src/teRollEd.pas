unit teRollEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, StdCtrls, teCtrls, ComCtrls, Buttons, ExtCtrls;

{$INCLUDE teDefs.inc}

type
  TRollTransitionEditor = class(TTransitionEffectEditor)
    LabelRollSize: TLabel;
    Label3D: TLabel;
    LabelUnroll: TLabel;
    EditRollSize: TEdit;
    UpDownRollSize: TUpDown;
    CheckBox3D: TCheckBox;
    CheckBoxUnroll: TCheckBox;
  private
  public
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  RollTransitionEditor: TRollTransitionEditor;

implementation

uses teRoll;

{$R *.DFM}

procedure TRollTransitionEditor.ReadValues;
var
  RollTransition: TRollTransition;
begin
  inherited;

  RollTransition := Transition as TRollTransition;

  UpDownRollSize.Position := RollTransition.Size;
  CheckBox3D    .Checked  := RollTransition.Use3D;
  CheckBoxUnroll.Checked  := RollTransition.Unroll;
end;

procedure TRollTransitionEditor.WriteValues;
var
  RollTransition: TRollTransition;
begin
  inherited;

  RollTransition := Transition as TRollTransition;

  RollTransition.Size   := UpDownRollSize.Position;
  RollTransition.Use3D  := CheckBox3D    .Checked;
  RollTransition.Unroll := CheckBoxUnroll.Checked;
end;

initialization

  RegisterClasses([TRollTransitionEditor]);

end.
