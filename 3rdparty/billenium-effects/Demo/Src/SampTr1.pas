unit SampTr1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FormCont, ExtCtrls, teCtrls;

type
  TSampTrForm1 = class(TFCEmbeddedForm)
    EffectsPanelText: TEffectsPanel;
  private
  public
    procedure Initialize(Picture: String);
    procedure ShowText(Txt: String);
  end;

var
  SampTrForm1: TSampTrForm1;

implementation

{$R *.DFM}

procedure TSampTrForm1.Initialize(Picture: String);
begin
  BackgroundOptions.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) +
    Picture);
end;

procedure TSampTrForm1.ShowText(Txt: String);
begin
  EffectsPanelText.Caption := Txt;
end;

end.
