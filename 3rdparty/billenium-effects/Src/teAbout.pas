unit teAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TransEff, teTimed, teRoll, teForm, ExtCtrls, teCtrls, teSlide,
  teDrip;

type
  TTEAboutForm = class(TForm)
    FormTransitions: TFormTransitions;
    TransitionList1: TTransitionList;
    TransitionPic: TRollTransition;
    LabelVersion: TLabel;
    TransitionButtons: TSlideTransition;
    BkStd: TEffectsPanel;
    BkOver: TEffectsPanel;
    BkClick: TEffectsPanel;
    PanelButtons: TEffectsPanel;
    BuyButton: TEffectsPanel;
    BuyShape: TShape;
    CloseButton: TEffectsPanel;
    CloseShape: TShape;
    WebButton: TEffectsPanel;
    WebShape: TShape;
    TransitionVersion: TDripTransition;
    procedure FormCreate(Sender: TObject);
    procedure TransitionPicAfterTransition(Sender: TObject);
    procedure TransitionVersionAfterTransition(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BuyButtonMouseEnter(Sender: TObject);
    procedure BuyButtonMouseLeave(Sender: TObject);
    procedure WebButtonMouseEnter(Sender: TObject);
    procedure WebButtonMouseLeave(Sender: TObject);
    procedure CloseButtonMouseEnter(Sender: TObject);
    procedure CloseButtonMouseLeave(Sender: TObject);
    procedure BuyShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WebShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BuyShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WebShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
  public
    procedure DisableTransitions;
  end;

var
  TEAboutForm: TTEAboutForm;

implementation

{$R *.DFM}

uses teRender, teReg, jpeg, ShellApi;

procedure FixAntialias(Font: TFont);
var
  SaveFont: HGDIOBJ;
  DC: HDC;
begin
  DC := GetDC(0);
  try
    SaveFont := SelectObject(DC, Font.Handle);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TTEAboutForm.FormCreate(Sender: TObject);
var
  Rgn: HRGN;
begin
  Rgn := CreateRoundRectRgn(0, 0, BuyButton.Width+1, BuyButton.Height+1,
    7, 8);
  SetWindowRgn(BuyButton.Handle, Rgn, True);

  Rgn := CreateRoundRectRgn(0, 0, WebButton.Width+1, WebButton.Height+1,
    7, 8);
  SetWindowRgn(WebButton.Handle, Rgn, True);

  Rgn := CreateRoundRectRgn(0, 0, CloseButton.Width+1, CloseButton.Height+1,
    7, 8);
  SetWindowRgn(CloseButton.Handle, Rgn, True);

  FixAntialias(LabelVersion.Font);
  FixAntialias(CloseButton.Font);

  LabelVersion.Caption := BilleniumEffectsVersion;
end;

procedure TTEAboutForm.DisableTransitions;
begin
  TransitionPic    .Enabled := False;
  TransitionVersion.Enabled := False;
  TransitionButtons.Enabled := False;
end;

procedure TTEAboutForm.TransitionPicAfterTransition(Sender: TObject);
begin
  TransitionVersion.Prepare(LabelVersion.Parent, LabelVersion.BoundsRect);
  try
    LabelVersion.Visible := True;
    TransitionVersion.Execute;
  finally
    TransitionVersion.UnPrepare;
  end;
end;

procedure TTEAboutForm.TransitionVersionAfterTransition(Sender: TObject);
begin
  TransitionButtons.Prepare(PanelButtons.Parent, PanelButtons.BoundsRect);
  try
    PanelButtons.Visible := True;
    {$ifdef Trial}
    BuyButton  .Visible := True;
    {$endif Trial}
    TransitionButtons.Execute;
  finally
    TransitionButtons.UnPrepare;
  end;
end;

procedure TTEAboutForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TTEAboutForm.BuyButtonMouseEnter(Sender: TObject);
begin
  BuyButton.Font.Assign(BkOver.Font);
  BuyButton.BackgroundOptions.Assign(BkOver.BackgroundOptions);
  BuyButton.Update;
end;

procedure TTEAboutForm.BuyButtonMouseLeave(Sender: TObject);
begin
  BuyButton.Font.Assign(BkStd.Font);
  BuyButton.BackgroundOptions.Assign(BkStd.BackgroundOptions);
  BuyButton.Update;
end;

procedure TTEAboutForm.BuyShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    BuyButton.Font.Assign(BkClick.Font);
    BuyButton.BackgroundOptions.Assign(BkClick.BackgroundOptions);
    BuyButton.Update;
  end;
end;

procedure TTEAboutForm.BuyShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    GotoWeb('AB', True);
    BuyButton.Font.Assign(BkOver.Font);
    BuyButton.BackgroundOptions.Assign(BkOver.BackgroundOptions);
    BuyButton.Update;
  end;
end;

procedure TTEAboutForm.WebButtonMouseEnter(Sender: TObject);
begin
  WebButton.Font.Assign(BkOver.Font);
  WebButton.BackgroundOptions.Assign(BkOver.BackgroundOptions);
  WebButton.Update;
end;

procedure TTEAboutForm.WebButtonMouseLeave(Sender: TObject);
begin
  WebButton.Font.Assign(BkStd.Font);
  WebButton.BackgroundOptions.Assign(BkStd.BackgroundOptions);
  WebButton.Update;
end;

procedure TTEAboutForm.WebShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    WebButton.Font.Assign(BkClick.Font);
    WebButton.BackgroundOptions.Assign(BkClick.BackgroundOptions);
    WebButton.Update;
  end;
end;

procedure TTEAboutForm.WebShapeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    GotoWeb('AB', False);
    WebButton.Font.Assign(BkOver.Font);
    WebButton.BackgroundOptions.Assign(BkOver.BackgroundOptions);
    WebButton.Update;
  end;
end;

procedure TTEAboutForm.CloseButtonMouseEnter(Sender: TObject);
begin
  CloseButton.Font.Assign(BkOver.Font);
  CloseButton.BackgroundOptions.Assign(BkOver.BackgroundOptions);
  CloseButton.Update;
end;

procedure TTEAboutForm.CloseButtonMouseLeave(Sender: TObject);
begin
  CloseButton.Font.Assign(BkStd.Font);
  CloseButton.BackgroundOptions.Assign(BkStd.BackgroundOptions);
  CloseButton.Update;
end;

procedure TTEAboutForm.CloseShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    CloseButton.Font.Assign(BkClick.Font);
    CloseButton.BackgroundOptions.Assign(BkClick.BackgroundOptions);
    CloseButton.Update;
  end;
end;

procedure TTEAboutForm.CloseShapeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    Close;
end;

end.
