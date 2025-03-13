unit Prices;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, FormCont, StdCtrls, ExtCtrls, teCtrls, TransEff, teTimed,
  teSlide, teMasked, teBlock, teBlur;

type
  TFormPrices = class(TFCEmbeddedForm)
    LabelText: TLabel;
    EffectsPanel1: TEffectsPanel;
    EffectsPanel2: TEffectsPanel;
    EffectsPanel3: TEffectsPanel;
    EffectsPanel4: TEffectsPanel;
    EffectsPanel5: TEffectsPanel;
    EffectsPanel6: TEffectsPanel;
    EffectsPanel7: TEffectsPanel;
    EffectsPanel8: TEffectsPanel;
    EffectsPanel9: TEffectsPanel;
    EffectsPanel10: TEffectsPanel;
    EffectsPanel11: TEffectsPanel;
    EffectsPanel12: TEffectsPanel;
    EffectsPanel13: TEffectsPanel;
    EffectsPanel14: TEffectsPanel;
    EffectsPanel15: TEffectsPanel;
    EffectsPanel16: TEffectsPanel;
    EffectsPanel17: TEffectsPanel;
    EffectsPanel18: TEffectsPanel;
    EffectsPanel19: TEffectsPanel;
    EffectsPanel20: TEffectsPanel;
    EffectsPanel21: TEffectsPanel;
    EffectsPanel22: TEffectsPanel;
    EffectsPanel23: TEffectsPanel;
    EffectsPanel24: TEffectsPanel;
    EffectsPanel25: TEffectsPanel;
    EffectsPanel26: TEffectsPanel;
    EffectsPanel27: TEffectsPanel;
    EffectsPanel28: TEffectsPanel;
    EffectsPanel29: TEffectsPanel;
    EffectsPanel30: TEffectsPanel;
    EffectsPanel31: TEffectsPanel;
    EffectsPanel32: TEffectsPanel;
    EffectsPanel33: TEffectsPanel;
    EffectsPanel34: TEffectsPanel;
    EffectsPanel35: TEffectsPanel;
    EffectsPanel36: TEffectsPanel;
    LabelBuyNow: TEffectsPanel;
    EffectsPanel37: TEffectsPanel;
    EffectsPanel38: TEffectsPanel;
    EffectsPanel39: TEffectsPanel;
    EffectsPanel40: TEffectsPanel;
    Timer1: TTimer;
    TransitionList1: TTransitionList;
    Transition1: TSlideTransition;
    LabelWindows: TLabel;
    EffectsPanel41: TEffectsPanel;
    EffectsPanel42: TEffectsPanel;
    EffectsPanel43: TEffectsPanel;
    EffectsPanel44: TEffectsPanel;
    procedure FormCreate(Sender: TObject);
    procedure LabelBuyNowMouseEnter(Sender: TObject);
    procedure LabelBuyNowMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelBuyNowClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
  end;

var
  FormPrices: TFormPrices;

implementation

uses ShellApi, Navigator;

{$R *.DFM}

procedure TFormPrices.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to ControlCount-1 do
  begin
    if Controls[i].Tag <> 0 then
    begin
      if Controls[i] is TEffectsPanel then
      begin
        with TEffectsPanel(Controls[i]) do
        begin
          case Tag of
            1: BackgroundOptions.Picture.LoadFromFile(
                 ExtractFilePath(Application.ExeName) + 'no.bmp');
            2: BackgroundOptions.Picture.LoadFromFile(
                 ExtractFilePath(Application.ExeName) + 'yes.bmp');
          end;
        end;
      end;
    end;
  end;
end;

procedure TFormPrices.LabelBuyNowMouseEnter(Sender: TObject);
begin
  LabelBuyNow.BackgroundOptions.GlassColor := clYellow;
end;

procedure TFormPrices.LabelBuyNowMouseLeave(Sender: TObject);
begin
  LabelBuyNow.BackgroundOptions.GlassColor := clWhite;
end;

procedure TFormPrices.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TFormPrices.LabelBuyNowClick(Sender: TObject);
begin
  ShellExecute(Application.MainForm.Handle, nil,
    PChar('http://www.billeniumsoft.com/bef/order.htm?' +
      MajorVersion + MinorVersion + 'd2'),
    nil, nil, SW_SHOWNORMAL);
end;

procedure TFormPrices.Timer1Timer(Sender: TObject);

  procedure PanelTransition(Panel: TEffectsPanel);
  begin
    Transition1.Prepare(Panel, Panel.ClientRect);
    try
      Panel.BackgroundOptions.PictureVisible := True;
      Transition1.Execute;
    finally
      Transition1.UnPrepare;
    end;
  end;

var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Timer1.Enabled := False;
    PanelTransition(EffectsPanel11);
    PanelTransition(EffectsPanel27);
    PanelTransition(EffectsPanel12);
    PanelTransition(EffectsPanel36);
    PanelTransition(EffectsPanel10);
    PanelTransition(EffectsPanel18);
    PanelTransition(EffectsPanel30);
    PanelTransition(EffectsPanel43);
    PanelTransition(EffectsPanel15);
    PanelTransition(EffectsPanel22);
    PanelTransition(EffectsPanel40);
    PanelTransition(EffectsPanel38);
    PanelTransition(EffectsPanel19);
    PanelTransition(EffectsPanel26);
    PanelTransition(EffectsPanel23);
    PanelTransition(EffectsPanel28);
    PanelTransition(EffectsPanel20);
    PanelTransition(EffectsPanel24);
    PanelTransition(EffectsPanel42);
    PanelTransition(EffectsPanel39);
    PanelTransition(EffectsPanel16);
    PanelTransition(EffectsPanel34);
    PanelTransition(EffectsPanel44);
    PanelTransition(EffectsPanel32);
    PanelTransition(EffectsPanel35);
    PanelTransition(EffectsPanel14);
    PanelTransition(EffectsPanel31);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

end.
