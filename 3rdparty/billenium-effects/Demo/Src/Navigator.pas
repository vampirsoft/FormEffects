unit Navigator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormCont, ExtCtrls, ComCtrls, StdCtrls, TransEff, teWipe, teTimed, Bkgrnd,
  teForm, teCtrls, teMasked, teRadial, jpeg, teBkgrnd, teBlend, teBlur,
  tePixelt, teIntrlc, tePage;

type
  TFormNavigator = class(TFCEmbeddedForm)
    FormContainerNavigator: TFormContainer;
    TransitionList: TTransitionList;
    AuxTransition: TWipeTransition;
    EffectsPanelNavigator: TEffectsPanel;
    LabelFormContainer: TEffectsPanel;
    LabelLRU: TEffectsPanel;
    LabelTransitionEffects: TEffectsPanel;
    LabelImage: TEffectsPanel;
    LabelImgSample: TEffectsPanel;
    LabelAnimations: TEffectsPanel;
    LabelTransitionEditor: TEffectsPanel;
    LabelBkgrndSamples: TEffectsPanel;
    ImageContact: TImage;
    LabelContact: TEffectsPanel;
    LabelTransitionSamples: TEffectsPanel;
    LabelBkOptions: TEffectsPanel;
    LabelBuyNow: TEffectsPanel;
    LabelWeb: TEffectsPanel;
    LabelIntro: TEffectsPanel;
    LabelNew: TEffectsPanel;
    TimerNew: TTimer;
    MenuTransition: TBlendTransition;
    LabelPower: TEffectsPanel;
    LabelPrices: TEffectsPanel;
    LabelComponents: TEffectsPanel;
    NavigatorTransition: TPageTransition;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelFormContainerClick(Sender: TObject);
    procedure LabelLRUClick(Sender: TObject);
    procedure LabelPowerClick(Sender: TObject);
    procedure LabelTransitionEditorClick(Sender: TObject);
    procedure LabelBkgrndSamplesClick(Sender: TObject);
    procedure LabelContactClick(Sender: TObject);
    procedure LabelTransitionSamplesClick(Sender: TObject);
    procedure LabelTransitionEffectsClick(Sender: TObject);
    procedure LabelBkOptionsClick(Sender: TObject);
    procedure LabelBuyNowClick(Sender: TObject);
    procedure LabelWebClick(Sender: TObject);
    procedure LabelIntroClick(Sender: TObject);
    procedure LabelNewClick(Sender: TObject);
    procedure TimerNewTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LabelIntroMouseEnter(Sender: TObject);
    procedure LabelIntroMouseLeave(Sender: TObject);
    procedure LabelPricesClick(Sender: TObject);
    procedure LabelComponentsClick(Sender: TObject);
    procedure LabelImgSampleClick(Sender: TObject);
    procedure LabelImageClick(Sender: TObject);
    procedure LabelAnimationsClick(Sender: TObject);
  private
    function  SelectNode(Node: TEffectsPanel): Boolean;
  public
    TransEffct: TTransitionEffect;
    Selected: TEffectsPanel;
    ChildGlassColor: TColor;
    ChildFormTranslucency: Integer;
    DefBkOptions,
    SelBkOptions,
    OverBkOptions,
    SaveBkOptions: TFCBackgroundOptions;
    DefFont,
    SelFont,
    OverFont,
    SaveFont: TFont;

    FormBkgrndInstance: TFormBkgrnd;

    procedure Intro;
    procedure Components;
    procedure FormContIntro;
    procedure LRU;
    procedure Power;
    procedure Transitions;
    procedure TransitionSamples;
    procedure TransitionEditor;
    procedure Image;
    procedure ImgSample;
    procedure Animations;
    procedure BkgrndOptions;
    procedure BkgrndEditor;
    procedure Contact;
    procedure Prices;
    procedure New;

    procedure Initialize;
  end;

var
  FormNavigator: TFormNavigator;
  MajorVersion,
  MinorVersion: Char;

implementation

uses FormContIntro, Power, TransitionSamples, Contact, teChrono, mmSystem, LRU,
  Main, TransitionEditor, BkgrndEditor, Transitions, BkOptions, ShellApi, Intro,
  Prices, teRender, Components, ImgSample, Image, New, Animations;

{$R *.DFM}

procedure TFormNavigator.FormCreate(Sender: TObject);
begin
  MajorVersion := BilleniumEffectsVersion[ 9];
  MinorVersion := BilleniumEffectsVersion[11];
  Assert(MajorVersion in ['0'..'9']);
  Assert(MinorVersion in ['0'..'9']);

  TransEffct    := AuxTransition;

  ImageContact.Picture.LoadFromFile(
    ExtractFilePath(Application.ExeName) + 'Contact.jpg');

  DefBkOptions  := TFCBackgroundOptions.Create;
  SelBkOptions  := TFCBackgroundOptions.Create;
  OverBkOptions := TFCBackgroundOptions.Create;
  SaveBkOptions := TFCBackgroundOptions.Create;
  DefBkOptions .Assign(LabelIntro.BackgroundOptions);
  SelBkOptions .Assign(DefBkOptions);
  OverBkOptions.Assign(DefBkOptions);
  SelBkOptions .GlassColor        := clWhite;
  SelBkOptions .GlassTranslucency := 45;
  OverBkOptions.GlassColor        := clYellow;
  OverBkOptions.GlassTranslucency := 45;
  DefFont  := TFont.Create;
  SelFont  := TFont.Create;
  OverFont := TFont.Create;
  SaveFont := TFont.Create;
  DefFont .Assign(LabelIntro.Font);
  SelFont .Assign(DefFont);
  OverFont.Assign(DefFont);
  SelFont .Color := clBlack;
  SelFont .Style := [fsBold, fsItalic];
  OverFont.Color := clNavy;
end;

procedure TFormNavigator.FormDestroy(Sender: TObject);
begin
  DefBkOptions .Free;
  SelBkOptions .Free;
  OverBkOptions.Free;
  SaveBkOptions.Free;
  DefFont      .Free;
  SelFont      .Free;
  OverFont     .Free;
  SaveFont     .Free;
end;

procedure TFormNavigator.FormContIntro;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormContainerIntro :=
      TFormContainerIntro(FormContainerNavigator.CreateForm(TFormContainerIntro));
    FormContainerIntro.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormContainerIntro.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormContainerIntro, True, TransEffct, nil,
      fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.Power;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormPower := TFormPower(FormContainerNavigator.CreateForm(TFormPower));
    FormContainerNavigator.ShowFormEx(FormPower, True, TransEffct, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.LRU;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormLRU := TFormLRU(FormContainerNavigator.CreateForm(TFormLRU));
    FormLRU.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormLRU.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormLRU, True, TransEffct, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.Transitions;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormTransitionEffects := TFormTransitionEffects(FormContainerNavigator.CreateForm(TFormTransitionEffects));
    FormTransitionEffects.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormTransitionEffects.BackgroundOptions.GlassTranslucency :=
      ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormTransitionEffects, True, TransEffct, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.TransitionSamples;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormTransitionSamples := TFormTransitionSamples(FormContainerNavigator.CreateForm(TFormTransitionSamples));
    FormContainerNavigator.ShowFormEx(FormTransitionSamples, True, TransEffct, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.Intro;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormIntro := TFormIntro(FormContainerNavigator.CreateForm(TFormIntro));
    FormIntro.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormIntro.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormIntro, True, TransEffct, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.Contact;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormContact := TFormContact(FormContainerNavigator.CreateForm(TFormContact));
    FormContact.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormContact.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormContact, True, TransEffct, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.Prices;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormPrices := TFormPrices(FormContainerNavigator.CreateForm(TFormPrices));
    FormPrices.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormPrices.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormPrices, True, TransEffct, nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.Components;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormComponents :=
      TFormComponents(FormContainerNavigator.CreateForm(TFormComponents));
    FormComponents.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormComponents.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormComponents, True, TransEffct, nil,
      fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.New;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormNew :=
      TFormNew(FormContainerNavigator.CreateForm(TFormNew));
    FormNew.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormNew.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormNew, True, TransEffct,
      nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.TransitionEditor;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormTransitionEditor :=
      TFormTransitionEditor(FormContainerNavigator.CreateForm(
        TFormTransitionEditor));
    FormTransitionEditor.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormTransitionEditor.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormTransitionEditor, True, TransEffct,
      nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.BkgrndOptions;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormBkOptions :=
      TFormBkOptions(FormContainerNavigator.CreateForm(TFormBkOptions));
    FormBkOptions.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormBkOptions.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormBkOptions, True, TransEffct,
      nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.BkgrndEditor;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormBkgrndEditor :=
      TFormBkgrndEditor(FormContainerNavigator.CreateForm(TFormBkgrndEditor));
    FormBkgrndEditor.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormBkgrndEditor.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormBkgrndEditor, True, TransEffct,
      nil, fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.FormShow(Sender: TObject);
begin
  FormBk(Self, nil);
  if Parent <> MainForm.MainFormContainer then
    EffectsPanelNavigator.Visible := True;

  TimerNew.Enabled := True;
end;

procedure TFormNavigator.LabelFormContainerClick(Sender: TObject);
begin
  if SelectNode(LabelFormContainer) then
    FormContIntro;
end;

procedure TFormNavigator.LabelPowerClick(Sender: TObject);
begin
  if SelectNode(LabelPower) then
    Power;
end;

procedure TFormNavigator.LabelLRUClick(Sender: TObject);
begin
  if SelectNode(LabelLRU) then
    LRU;
end;

procedure TFormNavigator.LabelIntroClick(Sender: TObject);
begin
  if SelectNode(LabelIntro) then
    Intro;
end;

procedure TFormNavigator.LabelTransitionEditorClick(Sender: TObject);
begin
  if SelectNode(LabelTransitionEditor) then
    TransitionEditor;
end;

procedure TFormNavigator.LabelBkgrndSamplesClick(Sender: TObject);
begin
  if SelectNode(LabelBkgrndSamples) then
    BkgrndEditor;
end;

procedure TFormNavigator.LabelContactClick(Sender: TObject);
begin
  if SelectNode(LabelContact) then
    Contact;
end;

procedure TFormNavigator.Initialize;
begin
  NavigatorTransition.Prepare(EffectsPanelNavigator.Parent,
    EffectsPanelNavigator.BoundsRect);
  try
    EffectsPanelNavigator.Visible := True;
    if NavigatorTransition.Prepared then
      NavigatorTransition.Execute;
  finally
    NavigatorTransition.UnPrepare;
  end;
end;

function TFormNavigator.SelectNode(Node: TEffectsPanel): Boolean;
begin
  Result := FormContainerNavigator.CloseQuery;
  if Result then
  begin
    MenuTransition.Prepare(Node, Node.ClientRect);
    try
      if Selected <> nil then
      begin
        Selected.BackgroundOptions.Assign(DefBkOptions);
        Selected.Font             .Assign(DefFont);
        Selected.Update;
      end;
      if Selected = Node
      then
      begin
        SaveBkOptions.Assign(DefBkOptions);
        SaveFont     .Assign(DefFont);
        Selected.BackgroundOptions.Assign(OverBkOptions);
        Selected.Font             .Assign(OverFont);
        Selected.Update;
        Selected := nil;
        Result   := False;
      end
      else
      begin
        Selected := Node;
        MenuTransition.Prepare(Selected, Selected.ClientRect);
        try
          Selected.BackgroundOptions.Assign(SelBkOptions);
          Selected.Font             .Assign(SelFont);
          MenuTransition.Execute;
        finally
          MenuTransition.UnPrepare;
        end;
      end;
      MenuTransition.Execute;
    finally
      MenuTransition.UnPrepare;
    end;
    if not Result then
    begin
      TransEffct.Reversed := True;
      FormContainerNavigator.ShowFormEx(nil, True, TransEffct, nil, fcfaDefault);
      TransEffct.Reversed := False;
    end;
  end;
end;

procedure TFormNavigator.LabelIntroMouseEnter(Sender: TObject);
begin
  if Sender = LabelNew then
    TimerNew.Enabled := False;

  if Sender <> Selected then
  begin
    SaveBkOptions.Assign((Sender as TEffectsPanel).BackgroundOptions);
    SaveFont     .Assign((Sender as TEffectsPanel).Font);

    (Sender as TEffectsPanel).BackgroundOptions.Assign(OverBkOptions);
    (Sender as TEffectsPanel).Font             .Assign(OverFont);
    (Sender as TEffectsPanel).Update;
  end;
end;

procedure TFormNavigator.LabelIntroMouseLeave(Sender: TObject);
begin
  if Sender = LabelNew then
    TimerNew.Enabled := True;

  if Sender <> Selected then
  begin
    (Sender as TEffectsPanel).BackgroundOptions.Assign(SaveBkOptions);
    (Sender as TEffectsPanel).Font             .Assign(SaveFont);
    (Sender as TEffectsPanel).Update;
  end;
end;

procedure TFormNavigator.LabelTransitionSamplesClick(Sender: TObject);
begin
  if SelectNode(LabelTransitionSamples) then
    TransitionSamples;
end;

procedure TFormNavigator.LabelTransitionEffectsClick(Sender: TObject);
begin
  if SelectNode(LabelTransitionEffects) then
    Transitions;
end;

procedure TFormNavigator.LabelBkOptionsClick(Sender: TObject);
begin
  if SelectNode(LabelBkOptions) then
    BkgrndOptions;
end;

procedure TFormNavigator.LabelBuyNowClick(Sender: TObject);
begin
  ShellExecute(Application.MainForm.Handle, nil,
    PChar('http://www.billeniumsoft.com/bef/order.htm?' +
      MajorVersion + MinorVersion + 'd1'), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormNavigator.LabelWebClick(Sender: TObject);
begin
  ShellExecute(Application.MainForm.Handle, nil,
    PChar('http://www.billeniumsoft.com?be' +
      MajorVersion + MinorVersion + 'd'), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormNavigator.LabelNewClick(Sender: TObject);
begin
  if SelectNode(LabelNew) then
    New;
end;

procedure TFormNavigator.TimerNewTimer(Sender: TObject);
begin
  if LabelNew.Font.Color = clWhite
  then LabelNew.Font.Color := clLime
  else LabelNew.Font.Color := clWhite;
end;

procedure TFormNavigator.FormHide(Sender: TObject);
begin
  TimerNew.Enabled := False;
end;

procedure TFormNavigator.LabelPricesClick(Sender: TObject);
begin
  if SelectNode(LabelPrices) then
    Prices;
end;

procedure TFormNavigator.LabelComponentsClick(Sender: TObject);
begin
  if SelectNode(LabelComponents) then
    Components;
end;

procedure TFormNavigator.LabelImgSampleClick(Sender: TObject);
begin
  if SelectNode(LabelImgSample) then
    ImgSample;
end;

procedure TFormNavigator.ImgSample;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormImageSample :=
      TFormImageSample(FormContainerNavigator.CreateForm(TFormImageSample));
    FormImageSample.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormImageSample.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormImageSample, True, TransEffct, nil,
      fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.Image;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormImage :=
      TFormImage(FormContainerNavigator.CreateForm(TFormImage));
    FormImage.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormImage.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormImage, True, TransEffct, nil,
    fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TFormNavigator.LabelImageClick(Sender: TObject);
begin
  if SelectNode(LabelImage) then
    Image;
end;

procedure TFormNavigator.LabelAnimationsClick(Sender: TObject);
begin
  if SelectNode(LabelAnimations) then
    Animations;
end;

procedure TFormNavigator.Animations;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    FormAnimations :=
      TFormAnimations(FormContainerNavigator.CreateForm(TFormAnimations));
    FormAnimations.BackgroundOptions.GlassColor        := ChildGlassColor;
    FormAnimations.BackgroundOptions.GlassTranslucency := ChildFormTranslucency;
    FormContainerNavigator.ShowFormEx(FormAnimations, True, TransEffct, nil,
    fcfaDefault);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

end.
