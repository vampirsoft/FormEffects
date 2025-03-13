unit teSldEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, StdCtrls, ExtCtrls, teCtrls, ComCtrls, Buttons;

{$INCLUDE teDefs.inc}

type
  TSlideTransitionEditor = class(TTransitionEffectEditor)
    LabelElasticSrc: TLabel;
    LabelElasticDst: TLabel;
    LabelSlideOut: TLabel;
    CheckBoxElasticSrc: TCheckBox;
    CheckBoxElasticDst: TCheckBox;
    CheckBoxSlideOut: TCheckBox;
  private
  public
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  SlideTransitionEditor: TSlideTransitionEditor;

implementation

uses teSlide;

{$R *.DFM}

{ TSlideTransitionEditor }

procedure TSlideTransitionEditor.ReadValues;
var
  SlideTransition: TSlideTransition;
begin
  inherited;

  SlideTransition := Transition as TSlideTransition;

  CheckBoxSlideOut  .Checked := SlideTransition.SlideOut;
  CheckBoxElasticSrc.Checked := SlideTransition.ElasticSrc;
  CheckBoxElasticDst.Checked := SlideTransition.ElasticDst;
end;

procedure TSlideTransitionEditor.WriteValues;
var
  SlideTransition: TSlideTransition;
begin
  inherited;

  SlideTransition := Transition as TSlideTransition;

  SlideTransition.SlideOut   := CheckBoxSlideOut  .Checked;
  SlideTransition.ElasticSrc := CheckBoxElasticSrc.Checked;
  SlideTransition.ElasticDst := CheckBoxElasticDst.Checked;
end;


initialization

  RegisterClasses([TSlideTransitionEditor]);

end.
