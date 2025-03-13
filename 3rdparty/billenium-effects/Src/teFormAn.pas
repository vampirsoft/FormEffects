unit teFormAn;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, Windows, Messages, Controls, Forms, teAnim;

type
  TTEFormAnimation = class;

  {$ifndef TE_NOHLP}
  TTEFormAnimationData = class
  public
    Animation: TTEFormAnimation;
    Form: TCustomForm;
    Control: TControl;
    Origin: TRect;

    constructor Create(AnAnimation: TTEFormAnimation; AForm: TCustomForm;
      AControl: TControl; AnOrigin: TRect); virtual;
  end;
  {$endif TE_NOHLP}

  TTEFormAnimation = class(TTEAnimationEffect)
  private
    FHidingEnabled: Boolean;
  protected
    function  DoShowForm(Form: TCustomForm; Modal: Boolean;
      AnimationData: TTEFormAnimationData): Integer;
    procedure ExecuteHiding(AnimationData: TTEFormAnimationData); virtual; abstract;
    procedure ExecuteShowing(HasTransition: Boolean;
      AnimationData: TTEFormAnimationData;
      var CanDestroyAnimationData: Boolean); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    function  CreateAnimationData(Form: TCustomForm): TTEFormAnimationData; virtual;
    procedure ShowForm(Form: TCustomForm); virtual;
    function  ShowModalForm(Form: TCustomForm): Integer; virtual;
  published
    property HidingEnabled: Boolean read FHidingEnabled write FHidingEnabled default True;
  end;

implementation

uses teForm;

resourcestring
  rsFormIsNil = 'Form is nil';

{ TTEFormAnimation }

constructor TTEFormAnimation.Create(AOwner: TComponent);
begin
  inherited;

  FHidingEnabled := True;
end;

function TTEFormAnimation.CreateAnimationData(
  Form: TCustomForm): TTEFormAnimationData;
begin
  Result := TTEFormAnimationData.Create(Self, Form, nil, Rect(-1, -1, -1, -1));
end;

function TTEFormAnimation.DoShowForm(Form: TCustomForm; Modal: Boolean;
  AnimationData: TTEFormAnimationData): Integer;
var
  i: Integer;
  FormTransitions: TFormTransitions;
begin
  Result := 0;

  if Enabled then
  begin
    if Form = nil then
      raise Exception.Create(rsFormIsNil);

    FormTransitions := nil;
    for i := 0 to Form.ComponentCount-1 do
    begin
      if Form.Components[i] is TFormTransitions then
      begin
        FormTransitions := TFormTransitions(Form.Components[i]);
        FreeNotification(FormTransitions);
        if AnimationData.Control <> nil then
          AnimationData.Control.FreeNotification(FormTransitions);
        break;
      end;
    end;

    if FormTransitions = nil then
      FormTransitions := TFormTransitions.Create(Form);

    FormTransitions.PrepareAnimation(AnimationData);
  end;

  if Modal
  then Result := Form.ShowModal
  else Form.Show;
end;

procedure TTEFormAnimation.ShowForm(Form: TCustomForm);
begin
  DoShowForm(
    Form,
    False,
    CreateAnimationData(Form));
end;

function TTEFormAnimation.ShowModalForm(Form: TCustomForm): Integer;
begin
  Result :=
    DoShowForm(
      Form,
      True,
      CreateAnimationData(Form));
end;

{ TTEFormAnimationData }

constructor TTEFormAnimationData.Create(AnAnimation: TTEFormAnimation;
  AForm: TCustomForm; AControl: TControl; AnOrigin: TRect);
begin
  Animation := AnAnimation;
  Form      := AForm;
  Control   := AControl;
  Origin    := AnOrigin;
end;

end.
