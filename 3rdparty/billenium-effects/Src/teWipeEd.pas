unit teWipeEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, teForm, StdCtrls, ExtCtrls, teCtrls, ComCtrls, Buttons;

{$INCLUDE teDefs.inc}

type
  TWipeTransitionEditor = class(TTransitionEffectEditor)
    LabelBandWidth: TLabel;
    EditBandWidth: TEdit;
    UpDownBandWidth: TUpDown;
    CheckBoxSmoothBand: TCheckBox;
    LabelSmoothBand: TLabel;
    procedure EditBandWidthExit(Sender: TObject);
  public
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  WipeTransitionEditor: TWipeTransitionEditor;

implementation

uses teWipe;

{$R *.DFM}

{ TWipeTransitionEditor }

procedure TWipeTransitionEditor.ReadValues;
var
  WipeTransition: TWipeTransition;
begin
  inherited;

  WipeTransition := Transition as TWipeTransition;

  UpDownBandWidth.Position   := WipeTransition.BandWidth;
  CheckBoxSmoothBand.Checked := WipeTransition.SmoothBand;
end;

procedure TWipeTransitionEditor.WriteValues;
var
  WipeTransition: TWipeTransition;
begin
  inherited;

  WipeTransition := Transition as TWipeTransition;

  WipeTransition.BandWidth  := UpDownBandWidth.Position;
  WipeTransition.SmoothBand := CheckBoxSmoothBand.Checked;
end;

procedure TWipeTransitionEditor.EditBandWidthExit(Sender: TObject);
begin
  try
    UpDownBandWidth.Position := StrToInt(EditBandWidth.Text);
  except
    on E: Exception do
    begin
      EditBandWidth.SetFocus;
      raise;
    end;
  end;
end;

initialization

  RegisterClasses([TWipeTransitionEditor]);

end.
