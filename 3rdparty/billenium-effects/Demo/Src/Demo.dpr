program Demo;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  FormContIntro in 'FormContIntro.pas' {FormContainerIntro},
  Navigator in 'Navigator.pas' {FormNavigator},
  Power in 'Power.pas' {FormPower},
  TransitionSamples in 'TransitionSamples.pas' {FormTransitionSamples},
  About in 'About.pas' {FormAbout},
  Contact in 'Contact.pas' {FormContact},
  Transitions in 'Transitions.pas' {FormTransitionEffects},
  LRU in 'LRU.pas' {FormLRU},
  Bkgrnd in 'Bkgrnd.pas' {FormBkgrnd},
  TransitionEditor in 'TransitionEditor.pas' {FormTransitionEditor},
  BkgrndEditor in 'BkgrndEditor.pas' {FormBkgrndEditor},
  BkOptions in 'BkOptions.pas' {FormBkOptions},
  Intro in 'Intro.pas' {FormIntro},
  Prices in 'Prices.pas' {FormPrices: TFCEmbeddedForm},
  Components in 'Components.pas' {FormComponents: TFCEmbeddedForm},
  SampTr1 in 'SampTr1.pas' {SampTrForm1: TFCEmbeddedForm},
  ImgSample in 'ImgSample.pas' {FormImageSample},
  Image in 'Image.pas' {FormImage: TFCEmbeddedForm},
  New in 'New.pas' {FormNew: TFCEmbeddedForm},
  Animations in 'Animations.pas' {FormAnimations: TFCEmbeddedForm},
  AnimForm in 'AnimForm.pas' {AnimatedForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
