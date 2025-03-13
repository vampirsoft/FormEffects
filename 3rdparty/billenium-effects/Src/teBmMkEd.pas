unit teBmMkEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  teTrEfEd, ExtDlgs, teForm, StdCtrls, teCtrls, ComCtrls, Buttons, teBmpMsk,
  ExtCtrls, TransEff;

{$INCLUDE teDefs.inc}

type
  TBmpMaskTransitionEditor = class(TTransitionEffectEditor)
    OpenPictureDialog: TOpenPictureDialog;
    LabelSmoothingLevel: TLabel;
    EditSmoothingLevel: TEdit;
    UpDownSmoothingLevel: TUpDown;
    LabelMask: TLabel;
    SpeedButtonMask: TSpeedButton;
    EffectsPanelMask: TEffectsPanel;
    LabelMode: TLabel;
    ComboBoxMode: TComboBox;
    procedure SpeedButtonMaskClick(Sender: TObject);
    procedure ComboBoxModeChange(Sender: TObject);
  private
  public
    procedure Initialize(TransitionValue: TTransitionEffect); override;
    procedure ReadValues; override;
    procedure WriteValues; override;
  end;

var
  BmpMaskTransitionEditor: TBmpMaskTransitionEditor;

implementation

{$R *.DFM}

uses teBkgrnd, teEditor;

procedure TBmpMaskTransitionEditor.ReadValues;
var
  i: Integer;
begin
  inherited;

  UpDownSmoothingLevel.Position := TBmpMaskTransition(Transition).SmoothingLevel;

  if not TBmpMaskTransition(Transition).Mask.Empty then
    EffectsPanelMask.BackgroundOptions.Picture.Assign(
      TBmpMaskTransition(Transition).Mask);

  EffectsPanelMask.BackgroundOptions.PictureMode :=
    TBmpMaskTransition(Transition).MaskMode;

  for i:=0 to ComboBoxMode.Items.Count-1 do
    if TFCPictureMode(ComboBoxMode.Items.Objects[i]) =
       TBmpMaskTransition(Transition).MaskMode then
      ComboBoxMode.ItemIndex := i;
end;

procedure TBmpMaskTransitionEditor.WriteValues;
begin
  inherited;

  TBmpMaskTransition(Transition).SmoothingLevel := UpDownSmoothingLevel.Position;
  TBmpMaskTransition(Transition).Mask :=
    EffectsPanelMask.BackgroundOptions.Picture.Bitmap;
  TBmpMaskTransition(Transition).MaskMode :=
    TFCPictureMode(ComboBoxMode.Items.Objects[ComboBoxMode.ItemIndex]);
end;

procedure TBmpMaskTransitionEditor.SpeedButtonMaskClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    EffectsPanelMask.BackgroundOptions.Picture.LoadFromFile(
      OpenPictureDialog.Filename);
    TransitionEditor.AutoPreview;
  end;
end;

procedure TBmpMaskTransitionEditor.Initialize(
  TransitionValue: TTransitionEffect);
var
  i: Integer;
begin
  inherited;

  UpDownSmoothingLevel.Max := (Transition as TBmpMaskTransition).MaxSmoothingLevel;

  for i:=0 to Integer(High(TFCPictureMode)) do
  begin
    if TBmpMaskTransition(Transition).MaskModeAllowed(TFCPictureMode(i)) then
      ComboBoxMode.Items.AddObject(
        TEGetPictureModeDesc(TFCPictureMode(i)), TObject(i));
  end;
end;

procedure TBmpMaskTransitionEditor.ComboBoxModeChange(Sender: TObject);
begin
  EffectsPanelMask.BackgroundOptions.PictureMode :=
    TFCPictureMode(ComboBoxMode.Items.Objects[ComboBoxMode.ItemIndex]);
  TransitionEditor.AutoPreview;
end;

initialization

  RegisterClasses([TBmpMaskTransitionEditor]);

end.
