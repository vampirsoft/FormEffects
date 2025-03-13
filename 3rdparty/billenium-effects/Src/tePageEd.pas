unit tePageEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, StdCtrls, teCtrls, ComCtrls, Buttons, ExtCtrls;

type
  TPageTransitionEditor = class(TTransitionEffectEditor)
    LabelSize: TLabel;
    LabelUncover: TLabel;
    EditSize: TEdit;
    UpDownSize: TUpDown;
    CheckBoxUncover: TCheckBox;
    Label3D: TLabel;
    CheckBox3D: TCheckBox;
  private
  public
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  PageTransitionEditor: TPageTransitionEditor;

implementation

uses tePage;

{$R *.DFM}

{ TPageTransitionEditor }

procedure TPageTransitionEditor.ReadValues;
var
  PageTransition: TPageTransition;
begin
  inherited;

  PageTransition := Transition as TPageTransition;

  UpDownSize     .Position := PageTransition.Size;
  CheckBox3D     .Checked  := PageTransition.Use3D;
  CheckBoxUncover.Checked  := PageTransition.Uncover;
end;

procedure TPageTransitionEditor.WriteValues;
var
  PageTransition: TPageTransition;
begin
  inherited;

  PageTransition := Transition as TPageTransition;

  PageTransition.Size    := UpDownSize     .Position;
  PageTransition.Use3D   := CheckBox3D     .Checked;
  PageTransition.Uncover := CheckBoxUncover.Checked;
end;

initialization

  RegisterClasses([TPageTransitionEditor]);

end.
