unit teBlurEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, StdCtrls, teCtrls, ComCtrls, Buttons, ExtCtrls;

type
  TBlurTransitionEditor = class(TTransitionEffectEditor)
    LabelRadius: TLabel;
    EditRadius: TEdit;
    UpDownRadius: TUpDown;
  private
  public
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  BlurTransitionEditor: TBlurTransitionEditor;

implementation

uses teBlur;

{$R *.DFM}

{ TTransitionEffectEditor1 }

procedure TBlurTransitionEditor.ReadValues;
var
  BlurTransition: TBlurTransition;
begin
  inherited;

  BlurTransition := Transition as TBlurTransition;

  UpDownRadius.Position := BlurTransition.Radius;
end;

procedure TBlurTransitionEditor.WriteValues;
var
  BlurTransition: TBlurTransition;
begin
  inherited;

  BlurTransition := Transition as TBlurTransition;

  BlurTransition.Radius := UpDownRadius.Position;
end;

initialization

  RegisterClasses([TBlurTransitionEditor]);

end.
