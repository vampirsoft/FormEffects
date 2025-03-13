unit tePixlEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, StdCtrls, teCtrls, ComCtrls, Buttons, ExtCtrls;

type
  TPixelateTransitionEditor = class(TTransitionEffectEditor)
    LabelBoxSize: TLabel;
    EditBoxSize: TEdit;
    UpDownBoxSize: TUpDown;
  private
  public
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  PixelateTransitionEditor: TPixelateTransitionEditor;

implementation

uses tePixelt;

{$R *.DFM}

procedure TPixelateTransitionEditor.ReadValues;
var
  PixelateTransition: TPixelateTransition;
begin
  inherited;

  PixelateTransition := Transition as TPixelateTransition;

  UpDownBoxSize.Position := PixelateTransition.BoxSize;
end;

procedure TPixelateTransitionEditor.WriteValues;
var
  PixelateTransition: TPixelateTransition;
begin
  inherited;

  PixelateTransition := Transition as TPixelateTransition;

  PixelateTransition.BoxSize := UpDownBoxSize.Position;
end;

initialization

  RegisterClasses([TPixelateTransitionEditor]);

end.
