unit Bkgrnd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, teCtrls, teForm, StdCtrls, FormCont;

type
  TFormBkgrnd = class(TForm)
    FormTransitions: TFormTransitions;
    FormShadowV: TEffectsPanel;
    FormShadowH: TEffectsPanel;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure CheckPainting;
  protected
    function GetPalette: HPALETTE; override;
  public
    Navigator: TFCEmbeddedForm;
  end;

var
  FormBkgrnd: TFormBkgrnd;

implementation

uses Navigator;

{$R *.DFM}

procedure TFormBkgrnd.FormPaint(Sender: TObject);
begin
  CheckPainting;
end;

procedure TFormBkgrnd.CheckPainting;
var
  TheNavigator: TFormNavigator;
  R: TRect;
begin
  if Navigator = nil then exit;
  
  TheNavigator := (Navigator as TFormNavigator);

  FormShadowV.Visible := TheNavigator.FormContainerNavigator.Form <> nil;
  FormShadowH.Visible := FormShadowV.Visible;
  if FormShadowV.Visible then
  begin
    R := TheNavigator.FormContainerNavigator.Form.BoundsRect;
    OffsetRect(R, TheNavigator.FormContainerNavigator.Left + 10,
      TheNavigator.FormContainerNavigator.Top + 10);
    FormShadowV.Left   := R.Right  - FormShadowV.Width;
    FormShadowV.Top    := R.Top;
    FormShadowV.Height := R.Bottom - R.Top;
    FormShadowH.Left   := R.Left;
    FormShadowH.Top    := R.Bottom - FormShadowH.Height;
    FormShadowH.Width  := R.Right  - R.Left;
  end;
end;

function TFormBkgrnd.GetPalette: HPALETTE;
begin
  Result := 0;
  if FormTransitions.BackgroundOptions.Picture.Graphic <> nil then
    Result := FormTransitions.BackgroundOptions.Picture.Graphic.Palette;
end;

procedure TFormBkgrnd.FormCreate(Sender: TObject);
begin
  FormTransitions.BackgroundOptions.Picture.LoadFromFile(
    ExtractFilePath(Application.ExeName) + 'BEff.jpg');
end;

end.
