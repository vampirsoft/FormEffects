unit teTrEfEd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TransEff, StdCtrls, Buttons, ExtCtrls, teCtrls, teForm, ComCtrls;

{$INCLUDE teDefs.inc}

type
  {$ifndef TE_NOHLP}
  TTEFormDesignerBase = class
  public
    procedure MakeSubComponentsLinkable(Transition: TTransitionEffect); virtual;
    procedure Modified; virtual;
    procedure SelectComponent(Instance: TPersistent); virtual;
    function  UniqueName(const BaseName: string): string; virtual;
  end;
  {$endif TE_NOHLP}

  TTransitionEffectEditor = class(TForm)
    ColorDialog: TColorDialog;
    EffectsGroupBoxPass2: TEffectsGroupBox;
    LabelDistributedTime: TLabel;
    CheckBoxDistributedTime: TCheckBox;
    Label2PReversed: TLabel;
    CheckBox2PReversed: TCheckBox;
    CheckBoxUseSolidColor: TCheckBox;
    LabelUseSolidColor: TLabel;
    LabelSolidColor: TLabel;
    SpeedButtonSolidColor: TSpeedButton;
    SpeedButtonClosePanel: TSpeedButton;
    FormTransitions: TFormTransitions;
    PanelPasses: TEffectsPanel;
    PanelMilliseconds: TEffectsPanel;
    PanelDirection: TEffectsPanel;
    LabelPassSetting: TLabel;
    ComboBoxPasses: TComboBox;
    LabelPass2Options: TLabel;
    SpeedButtonPass2: TSpeedButton;
    LabelMilliseconds: TLabel;
    EditMilliseconds: TEdit;
    UpDownMilliseconds: TUpDown;
    LabelDirection: TLabel;
    ComboBoxDirection: TComboBox;
    PanelOther: TEffectsPanel;
    procedure SpeedButtonPass2Click(Sender: TObject);
    procedure SpeedButtonClosePanelClick(Sender: TObject);
    procedure SpeedButtonSolidColorClick(Sender: TObject);
    procedure EditMillisecondsExit(Sender: TObject);
    procedure LabelDistributedTimeClick(Sender: TObject);
    procedure Label2PReversedClick(Sender: TObject);
    procedure LabelUseSolidColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditMillisecondsChange(Sender: TObject);
  protected
    Transition: TTransitionEffect;
    GlassPanel: TEffectsPanel;
    FocusedControl: TWincontrol;

  public
    ShowMilliseconds,
    ShowPasses: Boolean;

    procedure Initialize(TransitionValue: TTransitionEffect); virtual;
    procedure ReadValues; virtual;
    procedure WriteValues; virtual;
    procedure CheckAssignment(Transition: TTransitionEffect); virtual;
  end;

var
  TransitionEffectEditor: TTransitionEffectEditor;

implementation

uses teSlide, teBlend, TypInfo, teRender, teEditor;

{$R *.DFM}

type
  TTransitionEffectHack = class(TTransitionEffect);

{ TTransitionEffectEditor }

procedure TTransitionEffectEditor.Initialize(TransitionValue: TTransitionEffect);
var
  i: Integer;
  ShowDirection: Boolean;
begin
  Transition  := TransitionValue;

  PanelMilliseconds.Visible :=
    ShowMilliseconds and
    (tetiMillisecondsCapable in TTransitionEffectHack(Transition).GetInfo(nil)) and
    (GetPropInfo(Transition.ClassType, 'Milliseconds', [tkInteger]) <> nil);

  PanelPasses.Visible :=
    ShowPasses and
    (tetiTwoPassesCapable in TTransitionEffectHack(Transition).GetInfo(nil));

  ShowDirection :=
    GetPropInfo(Transition.ClassType, 'Direction', [tkEnumeration]) <> nil;

  PanelDirection.Visible := ShowDirection;

  if ShowDirection then
  begin
    for i:=1 to Integer(High(TTEEffectDirection)) do
      if TTEEffectDirection(i) in Transition.AllowedDirections then
        ComboBoxDirection.Items.
          AddObject(TEGetDirectionDesc(TTEEffectDirection(i)), TObject(i));
  end;
end;

procedure TTransitionEffectEditor.ReadValues;
var
  i: Integer;
begin
  ComboBoxPasses.ItemIndex         := Integer(Transition.PassSetting);
  CheckBoxDistributedTime.Checked  := Transition.Pass2Options.DistributedTime;
  CheckBox2PReversed     .Checked  := Transition.Pass2Options.Reversed;
  CheckBoxUseSolidColor  .Checked  := Transition.Pass2Options.UseSolidColor;
  UpDownMilliseconds.Position      := Transition.Milliseconds;
  if PanelDirection.Visible then
  begin
    for i:=0 to ComboBoxDirection.Items.Count-1 do
      if TTEEffectDirection(ComboBoxDirection.Items.Objects[i]) =
         Transition.Direction then
        ComboBoxDirection.ItemIndex := i;
  end;
end;

procedure TTransitionEffectEditor.WriteValues;
begin
  Transition.PassSetting                  :=
    TTEPassSettingType(ComboBoxPasses.ItemIndex);
  Transition.Pass2Options.DistributedTime := CheckBoxDistributedTime.Checked;
  Transition.Pass2Options.Reversed        := CheckBox2PReversed.Checked;
  Transition.Pass2Options.UseSolidColor   := CheckBoxUseSolidColor.Checked;
  Transition.Milliseconds                 := UpDownMilliseconds.Position;
  if PanelDirection.Visible then
    Transition.Direction                  :=
      TTEEffectDirection(ComboBoxDirection.Items.Objects[
       ComboBoxDirection.ItemIndex]);
end;

procedure TTransitionEffectEditor.SpeedButtonPass2Click(Sender: TObject);
var
  PanelTransition: TSlideTransition;
  GlassTransition: TBlendTransition;
  CursorBak: TCursor;
  R: TRect;
begin
  FocusedControl := Screen.ActiveControl;
  CursorBak := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    GlassTransition := TBlendTransition.Create(nil);
    try
      GlassTransition.Milliseconds := 500;
      GlassTransition.Prepare(Parent, BoundsRect);
      GlassPanel        := TEffectsPanel.Create(Self);
      GlassPanel.Parent := Self;
      GlassPanel.SetBounds(0, 0, ClientWidth, ClientHeight);
      GlassPanel.BevelInner  := bvNone;
      GlassPanel.BevelOuter  := bvNone;
      GlassPanel.ParentColor := True;
      GlassPanel.BackgroundOptions.Assign(EffectsGroupBoxPass2.BackgroundOptions);
      if RGBDevice(False)
      then GlassPanel.BackgroundOptions.GlassTranslucency := 38
      else
      begin
        GlassPanel          .BackgroundOptions.GlassTranslucency := 0;
        EffectsGroupBoxPass2.BackgroundOptions.GlassTranslucency := 0;
      end;
      if GlassTransition.Prepared then
        GlassTransition.Execute;
    finally
      GlassTransition.Free;
    end;

    PanelTransition := TSlideTransition.Create(nil);
    try
      EffectsGroupBoxPass2.Visible := True;
      EffectsGroupBoxPass2.BringToFront;
      PanelTransition.Milliseconds := 500;
      PanelTransition.Direction    := tedOut;
      R := EffectsGroupBoxPass2.BoundsRect;
      OffsetRect(R, -200, 0);
      PanelTransition.Prepare(Self, R);
      EffectsGroupBoxPass2.Left := R.Left;
      CheckBoxDistributedTime.SetFocus;
      if PanelTransition.Prepared then
        PanelTransition.Execute;
    finally
      PanelTransition.Free;
    end;
  finally
    Screen.Cursor := CursorBak;
  end;
end;

procedure TTransitionEffectEditor.SpeedButtonClosePanelClick(
  Sender: TObject);
var
  PanelTransition: TSlideTransition;
  GlassTransition: TBlendTransition;
  CursorBak: TCursor;
begin
  CursorBak := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    PanelTransition := TSlideTransition.Create(nil);
    try
      PanelTransition.Milliseconds := 500;
      PanelTransition.Direction    := tedIn;
      PanelTransition.Prepare(Self, EffectsGroupBoxPass2.BoundsRect);
      EffectsGroupBoxPass2.Visible := False;
      EffectsGroupBoxPass2.Left    := 266;

      if PanelTransition.Prepared then
        PanelTransition.Execute;
    finally
      PanelTransition.Free;
    end;

    GlassTransition := TBlendTransition.Create(nil);
    try
      GlassTransition.Milliseconds := 500;
      GlassTransition.Prepare(Self, ClientRect);
      GlassPanel.Free;
      if FocusedControl <> nil then
        FocusedControl.SetFocus;
      if GlassTransition.Prepared then
        GlassTransition.Execute;
    finally
      GlassTransition.Free;
    end;
  finally
    Screen.Cursor := CursorBak;
  end;
end;

procedure TTransitionEffectEditor.SpeedButtonSolidColorClick(
  Sender: TObject);
begin
  ColorDialog.Color := Transition.Pass2Options.SolidColor;
  if ColorDialog.Execute then
  begin
    Transition.Pass2Options.SolidColor := ColorDialog.Color;
    TransitionEditor.AutoPreview;
  end;
end;

procedure TTransitionEffectEditor.EditMillisecondsExit(Sender: TObject);
begin
  try
    UpDownMilliseconds.Position := StrToInt(EditMilliseconds.Text);
  except
    on E: Exception do
    begin
      EditMilliseconds.SetFocus;
      raise;
    end;
  end;
end;

procedure TTransitionEffectEditor.LabelDistributedTimeClick(
  Sender: TObject);
begin
  CheckBoxDistributedTime.Checked := not CheckBoxDistributedTime.Checked;
end;

procedure TTransitionEffectEditor.Label2PReversedClick(Sender: TObject);
begin
  CheckBox2PReversed.Checked := not CheckBox2PReversed.Checked;
end;

procedure TTransitionEffectEditor.LabelUseSolidColorClick(Sender: TObject);
begin
  CheckBoxUseSolidColor.Checked := not CheckBoxUseSolidColor.Checked;
end;

procedure TTransitionEffectEditor.FormCreate(Sender: TObject);
begin
  ShowMilliseconds := True;
  ShowPasses       := True;
end;

procedure TTEFormDesignerBase.MakeSubComponentsLinkable(
  Transition: TTransitionEffect);
begin
end;

procedure TTEFormDesignerBase.Modified;
begin
end;

procedure TTEFormDesignerBase.SelectComponent(Instance: TPersistent);
begin
end;

function TTEFormDesignerBase.UniqueName(const BaseName: string): string;
begin
  Result := BaseName;
end;

procedure TTransitionEffectEditor.EditMillisecondsChange(Sender: TObject);
begin
  TransitionEditor.AutoPreview;
end;

procedure TTransitionEffectEditor.CheckAssignment(
  Transition: TTransitionEffect);
begin

end;

initialization

  {$ifdef D6UP}
  StartClassGroup(TControl);
  GroupDescendentsWith(TTransitionEffectEditor, Controls.TControl);
  {$endif D6UP}
  RegisterClass(TTransitionEffectEditor);

end.
