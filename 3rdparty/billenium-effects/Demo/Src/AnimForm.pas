unit AnimForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Buttons, ExtCtrls, teCtrls, StdCtrls;

type
  TAnimatedForm = class(TForm)
    BitBtn1: TBitBtn;
    EffectsPanel1: TEffectsPanel;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  AnimatedForm: TAnimatedForm;

implementation

{$R *.DFM}

procedure TAnimatedForm.FormCreate(Sender: TObject);
begin
  EffectsPanel1.BackgroundOptions.Picture.LoadFromFile(
    ExtractFilePath(Application.ExeName) + 'Box.jpg');
end;

end.
