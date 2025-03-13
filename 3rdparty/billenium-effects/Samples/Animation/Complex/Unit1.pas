unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, teFormAn, teForm, jpeg, ExtCtrls, TransEff, teTimed,
  teBlend, teCtrls, teZFrAn, teAnim, teSlide, ComCtrls, ExtDlgs;

type
  TForm1 = class(TForm)
    BitBtnAnimate: TBitBtn;
    GroupBoxSteps: TGroupBox;
    CheckBoxFirstStep: TCheckBox;
    CheckBoxLastStep: TCheckBox;
    Label1: TLabel;
    EditMinStepIncrement: TEdit;
    Label2: TLabel;
    EditStepMilliseconds: TEdit;
    GroupBoxBorderPen: TGroupBox;
    Label3: TLabel;
    PenColorButton: TSpeedButton;
    ColorDialog: TColorDialog;
    Label4: TLabel;
    ComboBoxPenMode: TComboBox;
    Label5: TLabel;
    EditPenWidth: TEdit;
    UpDownPenWidth: TUpDown;
    Label6: TLabel;
    ComboBoxPenStyle: TComboBox;
    GroupBoxFillBrush: TGroupBox;
    Label7: TLabel;
    BrushColorButton: TSpeedButton;
    Label8: TLabel;
    ComboBoxBrushStyle: TComboBox;
    GroupBoxGlass: TGroupBox;
    Label9: TLabel;
    GlassColorButton: TSpeedButton;
    Label10: TLabel;
    EditTranslucency: TEdit;
    UpDownTranslucency: TUpDown;
    CheckBoxGlassVisible: TCheckBox;
    GroupBoxPicture: TGroupBox;
    Label11: TLabel;
    PictureButton: TSpeedButton;
    Label12: TLabel;
    Label14: TLabel;
    ComboBoxPicMode: TComboBox;
    PicColorButton: TSpeedButton;
    CheckBoxPicVisible: TCheckBox;
    PictureDialog: TOpenPictureDialog;
    PreviewPanel: TEffectsPanel;
    PreviewShape: TShape;
    Label13: TLabel;
    ComboBoxOrg: TComboBox;
    TEAnimationList1: TTEAnimationList;
    Animation: TTEZoomFrameAnimation;
    procedure BitBtnAnimateClick(Sender: TObject);
    procedure PenColorButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxPenModeChange(Sender: TObject);
    procedure EditPenWidthChange(Sender: TObject);
    procedure ComboBoxPenStyleChange(Sender: TObject);
    procedure BrushColorButtonClick(Sender: TObject);
    procedure ComboBoxBrushStyleChange(Sender: TObject);
    procedure GlassColorButtonClick(Sender: TObject);
    procedure EditTranslucencyChange(Sender: TObject);
    procedure CheckBoxGlassVisibleClick(Sender: TObject);
    procedure CheckBoxPicVisibleClick(Sender: TObject);
    procedure PicColorButtonClick(Sender: TObject);
    procedure PictureButtonClick(Sender: TObject);
    procedure ComboBoxPicModeChange(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses Unit2, teBkgrnd;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  PreviewPanel.BackgroundOptions.Picture.LoadFromFile(
    ExtractFilePath(Application.ExeName) + '\no.bmp');
  CheckBoxPicVisible  .Checked := PreviewPanel.BackgroundOptions.PictureVisible;
  CheckBoxGlassVisible.Checked := PreviewPanel.BackgroundOptions.GlassVisible;
  UpDownTranslucency.Position  := PreviewPanel.BackgroundOptions.GlassTranslucency;
  UpDownPenWidth    .Position  := PreviewShape.Pen.Width;
  ComboBoxPenMode   .ItemIndex := Ord(PreviewShape.Pen.Mode);
  ComboBoxPenStyle  .ItemIndex := Ord(PreviewShape.Pen.Style);
  ComboBoxBrushStyle.ItemIndex := Ord(PreviewShape.Brush.Style);
  ComboBoxPicMode   .ItemIndex := Ord(PreviewPanel.BackgroundOptions.PictureMode);
  ComboBoxOrg       .ItemIndex := 0;
end;

procedure TForm1.PenColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := PreviewShape.Pen.Color;
  if ColorDialog.Execute then
  begin
    PreviewShape.Pen.Color := ColorDialog.Color;
    PreviewShape.Invalidate;
  end;
end;

procedure TForm1.ComboBoxPenModeChange(Sender: TObject);
begin
  PreviewShape.Pen.Mode := TPenMode(ComboBoxPenMode.ItemIndex);
  PreviewShape.Invalidate;
end;

procedure TForm1.ComboBoxPenStyleChange(Sender: TObject);
begin
  PreviewShape.Pen.Style := TPenStyle(ComboBoxPenStyle.ItemIndex);
  PreviewShape.Invalidate;
end;

procedure TForm1.EditPenWidthChange(Sender: TObject);
begin
  PreviewShape.Pen.Width := UpDownPenWidth.Position;
  PreviewShape.Invalidate;
end;

procedure TForm1.BrushColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := PreviewShape.Brush.Color;
  if ColorDialog.Execute then
  begin
    PreviewShape.Brush.Color := ColorDialog.Color;
    PreviewShape.Invalidate;
  end;
end;

procedure TForm1.ComboBoxBrushStyleChange(Sender: TObject);
begin
  PreviewShape.Brush.Style := TBrushStyle(ComboBoxBrushStyle.ItemIndex);
  PreviewShape.Invalidate;
end;

procedure TForm1.GlassColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := PreviewPanel.BackgroundOptions.GlassColor;
  if ColorDialog.Execute then
  begin
    PreviewPanel.BackgroundOptions.GlassColor := ColorDialog.Color;
    PreviewPanel.Invalidate;
  end;
end;

procedure TForm1.EditTranslucencyChange(Sender: TObject);
begin
  PreviewPanel.BackgroundOptions.GlassTranslucency := UpDownTranslucency.Position;
  PreviewPanel.Invalidate;
end;

procedure TForm1.CheckBoxGlassVisibleClick(Sender: TObject);
begin
  PreviewPanel.BackgroundOptions.GlassVisible := CheckBoxGlassVisible.Checked;
  PreviewPanel.Invalidate;
end;

procedure TForm1.CheckBoxPicVisibleClick(Sender: TObject);
begin
  PreviewPanel.BackgroundOptions.PictureVisible := CheckBoxPicVisible.Checked;
  PreviewPanel.Invalidate;
end;

procedure TForm1.PicColorButtonClick(Sender: TObject);
begin
  ColorDialog.Color := PreviewPanel.BackgroundOptions.PictureTranspColor;
  if ColorDialog.Execute then
  begin
    PreviewPanel.BackgroundOptions.PictureTranspColor := ColorDialog.Color;
    PreviewPanel.Invalidate;
  end;
end;

procedure TForm1.PictureButtonClick(Sender: TObject);
begin
  if PictureDialog.Execute then
  begin
    PreviewPanel.BackgroundOptions.Picture.LoadFromFile(PictureDialog.FileName);
    PreviewPanel.Invalidate;
  end;
end;

procedure TForm1.ComboBoxPicModeChange(Sender: TObject);
begin
  PreviewPanel.BackgroundOptions.PictureMode := TFCPictureMode(ComboBoxPicMode.ItemIndex);
  PreviewPanel.Invalidate;
end;

procedure TForm1.BitBtnAnimateClick(Sender: TObject);
var
  Control: TControl;
begin
  Control := nil;
  Form2 := TForm2.Create(Self);

  // Border settings
  Animation.BorderPen.Assign(PreviewShape.Pen);

  // Brush settings
  Animation.FillBrush.Assign(PreviewShape.Brush);

  // Picture settings
  Animation.Picture.Assign(PreviewPanel.BackgroundOptions.Picture);
  Animation.PictureMode         := PreviewPanel.BackgroundOptions.PictureMode;
  Animation.PictureTranspColor  := PreviewPanel.BackgroundOptions.PictureTranspColor;
  Animation.PictureVisible      := PreviewPanel.BackgroundOptions.PictureVisible;

  // Glass settings
  Animation.GlassColor          := PreviewPanel.BackgroundOptions.GlassColor;
  Animation.GlassTranslucency   := PreviewPanel.BackgroundOptions.GlassTranslucency;
  Animation.GlassVisible        := PreviewPanel.BackgroundOptions.GlassVisible;

  // Steps
  Animation.ShowFirstStep       := CheckBoxFirstStep.Checked;
  Animation.ShowLastStep        := CheckBoxLastStep.Checked;
  Animation.MinStepIncrement    := StrToInt(EditMinStepIncrement.Text);
  Animation.MinStepMilliseconds := StrToInt(EditStepMilliseconds.Text);

  case ComboBoxOrg.ItemIndex of
    0: Control := BitBtnAnimate;
    1: Control := PreviewPanel;
    2:
    begin
      Control                 := nil;
      Animation.DefaultOrigin := tezoCursor;
    end;
    3:
    begin
      Control                 := nil;
      Animation.DefaultOrigin := tezoFormCenter;
    end;
  end;

  if Control = nil
  then Animation.ShowModalForm(Form2)
  else Animation.ShowModalFormEx(Form2,
         Rect(0, 0, Control.Width, Control.Height), Control, False);
end;

end.
