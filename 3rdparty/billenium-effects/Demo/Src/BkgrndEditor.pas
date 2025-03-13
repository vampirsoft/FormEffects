unit BkgrndEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, teCtrls, teForm, ComCtrls, TransEff, teTimed,
  teMasked, teRadial, Buttons, Navigator, ExtDlgs, FormCont;

type
  TFormBkgrndEditor = class(TFCEmbeddedForm)
    ColorDialog: TColorDialog;
    EffectsGroupBoxBkrnd: TEffectsGroupBox;
    LabelGlassColor: TLabel;
    LabelFormTranslucency: TLabel;
    EditFormTranslucency: TEdit;
    UpDownFormTranslucency: TUpDown;
    ButtonGlassColor: TEffectsPanel;
    EffectsGroupBoxPicture: TEffectsGroupBox;
    LabelPicFile: TLabel;
    EditPicFile: TEdit;
    LabelPicMode: TLabel;
    SpeedButtonPicFile: TSpeedButton;
    ComboBoxPicMode: TComboBox;
    OpenPictureDialog: TOpenPictureDialog;
    TransitionList: TTransitionList;
    Transition: TRadialTransition;
    Transition2: TFlickerFreeTransition;
    EffectsGroupBox1: TEffectsGroupBox;
    ButtonGradient: TSpeedButton;
    ButtonStone: TSpeedButton;
    ButtonStrokes: TSpeedButton;
    ButtonWall: TSpeedButton;
    ButtonWood: TSpeedButton;
    ButtonForm: TSpeedButton;
    procedure EditFormTranslucencyChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonGlassColorClick(Sender: TObject);
    procedure ComboBoxPicModeChange(Sender: TObject);
    procedure EditPicFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonPicFileClick(Sender: TObject);
    procedure ButtonGradientClick(Sender: TObject);
    procedure ButtonStoneClick(Sender: TObject);
    procedure ButtonStrokesClick(Sender: TObject);
    procedure ButtonWallClick(Sender: TObject);
    procedure ButtonWoodClick(Sender: TObject);
    procedure ButtonFormClick(Sender: TObject);
  private
    Inicializado: Boolean;
  public
  end;

  procedure GradientBk  (Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
  procedure StoneBk     (Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
  procedure StrokesBk   (Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
  procedure WallBk      (Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
  procedure WoodBk      (Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
  procedure FormBk      (Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);

var
  FormBkgrndEditor: TFormBkgrndEditor;

implementation

uses teBkgrnd, Bkgrnd;

{$R *.DFM}

var
  TheNavigator: TFormNavigator;

type
  TWinControlHack = class(TWinControl);

procedure GradientBk(Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
var
  Filename: String;
begin
  if Navigator = nil then
    Navigator := TheNavigator;
  with Navigator.BackgroundOptions do
  begin
    Navigator.FormBkgrndInstance := nil;
    SetBkgrndForm(nil);
    Navigator.ChildFormTranslucency := 255;
    PictureMode := fcpmTile;
    Filename    := ExtractFilePath(Application.ExeName) + 'gradient.jpg';
    if FormBkgrndEditor <> nil
    then
    begin
      FormBkgrndEditor.EditPicFile.Text := Filename;
      FormBkgrndEditor.ComboBoxPicMode.ItemIndex := Integer(PictureMode);
      FormBkgrndEditor.BackgroundOptions.GlassTranslucency :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.UpDownFormTranslucency.Position :=
        Navigator.ChildFormTranslucency;
    end
    else Navigator.BackgroundOptions.Picture.LoadFromFile(Filename);
  end;
end;

procedure StoneBk(Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
var
  Filename: String;
begin
  if Navigator = nil then
    Navigator := TheNavigator;
  with Navigator.BackgroundOptions do
  begin
    Navigator.FormBkgrndInstance := nil;
    SetBkgrndForm(nil);
    Navigator.ChildFormTranslucency := 191;
    Navigator.ChildGlassColor := $0280FFFF;
    PictureMode := fcpmTile;
    Filename    := ExtractFilePath(Application.ExeName) + 'stone.jpg';
    if FormBkgrndEditor <> nil
    then
    begin
      FormBkgrndEditor.EditPicFile.Text := Filename;
      FormBkgrndEditor.ComboBoxPicMode.ItemIndex := Integer(PictureMode);
      FormBkgrndEditor.BackgroundOptions.GlassTranslucency :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.UpDownFormTranslucency.Position :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.BackgroundOptions.GlassColor := Navigator.ChildGlassColor;
      FormBkgrndEditor.ButtonGlassColor.Color := Navigator.ChildGlassColor;
    end
    else Navigator.BackgroundOptions.Picture.LoadFromFile(Filename);
  end;
end;

procedure StrokesBk(Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
var
  Filename: String;
begin
  if Navigator = nil then
    Navigator := TheNavigator;
  with Navigator.BackgroundOptions do
  begin
    Navigator.FormBkgrndInstance := nil;
    SetBkgrndForm(nil);
    Navigator.ChildFormTranslucency := 191;
    Navigator.ChildGlassColor := $02FFFF80;
    PictureMode := fcpmTile;
    Filename    := ExtractFilePath(Application.ExeName) + 'Strokes.jpg';
    if FormBkgrndEditor <> nil
    then
    begin
      FormBkgrndEditor.EditPicFile.Text := Filename;
      FormBkgrndEditor.ComboBoxPicMode.ItemIndex := Integer(PictureMode);
      FormBkgrndEditor.BackgroundOptions.GlassTranslucency :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.UpDownFormTranslucency.Position :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.BackgroundOptions.GlassColor := Navigator.ChildGlassColor;
      FormBkgrndEditor.ButtonGlassColor.Color := Navigator.ChildGlassColor;
    end
    else Navigator.BackgroundOptions.Picture.LoadFromFile(Filename);
  end;
end;

procedure WallBk(Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
var
  Filename: String;
begin
  if Navigator = nil then
    Navigator := TheNavigator;
  with Navigator.BackgroundOptions do
  begin
    Navigator.FormBkgrndInstance := nil;
    SetBkgrndForm(nil);
    Navigator.ChildFormTranslucency := 128;
    Navigator.ChildGlassColor := clWhite;
    PictureMode := fcpmTile;
    Filename    := ExtractFilePath(Application.ExeName) + 'wall.jpg';
    if FormBkgrndEditor <> nil
    then
    begin
      FormBkgrndEditor.EditPicFile.Text := Filename;
      FormBkgrndEditor.ComboBoxPicMode.ItemIndex := Integer(PictureMode);
      FormBkgrndEditor.BackgroundOptions.GlassTranslucency :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.UpDownFormTranslucency.Position :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.BackgroundOptions.GlassColor := Navigator.ChildGlassColor;
      FormBkgrndEditor.ButtonGlassColor.Color := Navigator.ChildGlassColor;
    end
    else Navigator.BackgroundOptions.Picture.LoadFromFile(Filename);
  end;
end;

procedure WoodBk(Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
var
  Filename: String;
begin
  if Navigator = nil then
    Navigator := TheNavigator;
  with Navigator.BackgroundOptions do
  begin
    Navigator.FormBkgrndInstance := nil;
    SetBkgrndForm(nil);
    Navigator.ChildFormTranslucency := 204;
    Navigator.ChildGlassColor := $020080FF;
    PictureMode := fcpmTile;
    Filename    := ExtractFilePath(Application.ExeName) + 'wood.jpg';
    if FormBkgrndEditor <> nil
    then
    begin
      FormBkgrndEditor.EditPicFile.Text := Filename;
      FormBkgrndEditor.ComboBoxPicMode.ItemIndex := Integer(PictureMode);
      FormBkgrndEditor.BackgroundOptions.GlassTranslucency :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.UpDownFormTranslucency.Position :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.BackgroundOptions.GlassColor := Navigator.ChildGlassColor;
      FormBkgrndEditor.ButtonGlassColor.Color := Navigator.ChildGlassColor;
    end
    else Navigator.BackgroundOptions.Picture.LoadFromFile(Filename);
  end;
end;

procedure FormBk(Navigator: TFormNavigator; FormBkgrndEditor: TFormBkgrndEditor);
begin
  if Navigator = nil then
    Navigator := TheNavigator;
  with Navigator.BackgroundOptions do
  begin
    SetBkgrndForm(TFormBkgrnd);
    Navigator.FormBkgrndInstance :=
      TFormBkgrnd(Navigator.FormContainerNavigator.BackgroundOptions.BkgrndForm);
    Navigator.FormBkgrndInstance.Navigator := Navigator;
    Navigator.ChildFormTranslucency := 25;
    Navigator.ChildGlassColor := $02E2C6D3;

    Picture.Graphic := nil;
    if FormBkgrndEditor <> nil then
    begin
      FormBkgrndEditor.EditPicFile.Text := '';
      FormBkgrndEditor.BackgroundOptions.GlassTranslucency :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.UpDownFormTranslucency.Position :=
        Navigator.ChildFormTranslucency;
      FormBkgrndEditor.BackgroundOptions.GlassColor := Navigator.ChildGlassColor;
      FormBkgrndEditor.ButtonGlassColor.Color := Navigator.ChildGlassColor;
    end
  end;
end;

procedure TFormBkgrndEditor.EditFormTranslucencyChange(Sender: TObject);
begin
  if TheNavigator.ChildFormTranslucency <> UpDownFormTranslucency.Position then
  begin
    Screen.Cursor := crHourglass;
    try
      Transition2.Prepare(Self, ClientRect);
      try
        TheNavigator.ChildFormTranslucency := UpDownFormTranslucency.Position;
        BackgroundOptions.GlassTranslucency := TheNavigator.ChildFormTranslucency;
        if Transition2.Prepared then
          Transition2.Execute;
      finally
        Transition2.UnPrepare;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TFormBkgrndEditor.FormShow(Sender: TObject);
begin
  TheNavigator := Parent.Parent as TFormNavigator;

  UpDownFormTranslucency.Position :=
    (Parent.Parent as TFormNavigator).FormContainerNavigator.BackgroundOptions.
    GlassTranslucency;
  UpDownFormTranslucency.Position := BackgroundOptions.GlassTranslucency;
  ComboBoxPicMode.ItemIndex := Integer(BackgroundOptions.PictureMode);
  FormBkgrndEditor.ButtonGlassColor.Color := TheNavigator.ChildGlassColor;

  Inicializado := True;
end;

procedure TFormBkgrndEditor.ButtonGlassColorClick(Sender: TObject);
begin
  ColorDialog.Color := ButtonGlassColor.Color;
  if ColorDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      Transition2.Prepare(Self, ClientRect);
      try
        TheNavigator.ChildGlassColor := ColorDialog.Color;
        BackgroundOptions.GlassColor := TheNavigator.ChildGlassColor;
        ButtonGlassColor.Color := TheNavigator.ChildGlassColor;
        if Transition2.Prepared then
          Transition2.Execute;
      finally
        Transition2.UnPrepare;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TFormBkgrndEditor.ComboBoxPicModeChange(Sender: TObject);
begin
  if ComboBoxPicMode.ItemIndex <>
     Integer(BackgroundOptions.PictureMode) then
  begin
    Screen.Cursor := crHourglass;
    try
      Transition2.Prepare(TheNavigator, TheNavigator.ClientRect);
      try
        TheNavigator.BackgroundOptions.PictureMode :=
          TFCPictureMode(ComboBoxPicMode.ItemIndex);
        if Transition2.Prepared then
          Transition2.Execute;
      finally
        Transition2.UnPrepare;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TFormBkgrndEditor.EditPicFileChange(Sender: TObject);
begin
  if Inicializado and (EditPicFile.Text <> '') then
    TheNavigator.BackgroundOptions.Picture.
      LoadFromFile(EditPicFile.Text);
end;

procedure TFormBkgrndEditor.FormCreate(Sender: TObject);
begin
  Inicializado := False;
end;

procedure TFormBkgrndEditor.SpeedButtonPicFileClick(Sender: TObject);
begin
  OpenPictureDialog.FileName := EditPicFile.Text;
  if OpenPictureDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      Transition2.Prepare(TheNavigator, TheNavigator.ClientRect);
      try
        TheNavigator.BackgroundOptions.SetBkgrndForm(nil);
        EditPicFile.Text := OpenPictureDialog.FileName;
        if Transition2.Prepared then
          Transition2.Execute;
      finally
        Transition2.UnPrepare;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TFormBkgrndEditor.ButtonGradientClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    Transition.Prepare(TheNavigator, TheNavigator.ClientRect);
    try
      GradientBk(nil, Self);
      if Transition.Prepared then
        Transition.Execute;
    finally
      Transition.UnPrepare;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormBkgrndEditor.ButtonStoneClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    Transition.Prepare(TheNavigator, TheNavigator.ClientRect);
    try
      StoneBk(nil, Self);
      if Transition.Prepared then
        Transition.Execute;
    finally
      Transition.UnPrepare;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormBkgrndEditor.ButtonStrokesClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    Transition.Prepare(TheNavigator, TheNavigator.ClientRect);
    try
      StrokesBk(nil, Self);
      if Transition.Prepared then
        Transition.Execute;
    finally
      Transition.UnPrepare;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormBkgrndEditor.ButtonWallClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    Transition.Prepare(TheNavigator, TheNavigator.ClientRect);
    try
      WallBk(nil, Self);
      if Transition.Prepared then
        Transition.Execute;
    finally
      Transition.UnPrepare;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormBkgrndEditor.ButtonWoodClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    Transition.Prepare(TheNavigator, TheNavigator.ClientRect);
    try
      WoodBk(nil, Self);
      if Transition.Prepared then
        Transition.Execute;
    finally
      Transition.UnPrepare;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormBkgrndEditor.ButtonFormClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    Transition.Prepare(TheNavigator, TheNavigator.ClientRect);
    try
      FormBk(nil, Self);
      if Transition.Prepared then
        Transition.Execute;
    finally
      Transition.UnPrepare;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
