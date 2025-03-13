unit teZFrAn;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, Windows, Messages, Controls, Forms, Graphics,
  teChrono, teBkgrnd, teFormAn;

type
  TTEZoomFrameAnimOrg = (tezoCursor, tezoFormCenter);

  TTEZoomFrameAnimation = class;

  {$ifndef TE_NOHLP}
  TTEZoomFrameAnimationData = class(TTEFormAnimationData)
  public
    ClientCoordinates: Boolean;

    constructor CreateEx(AnAnimation: TTEZoomFrameAnimation; AForm: TCustomForm;
      AControl: TControl; AnOrigin: TRect; AClientCoordinates: Boolean); virtual;
  end;
  {$endif TE_NOHLP}

  TTEZoomFrameAnimation = class(TTEFormAnimation)
  private
    BkgrndOptions: TFCBackgroundOptions;
    FShowFirstStep: Boolean;
    FShowLastStep: Boolean;
    FMinStepIncrement: Integer;
    FMinStepMilliseconds: Integer;
    FFillBrush: TBrush;
    FBorderPen: TPen;
    FDefaultOrigin: TTEZoomFrameAnimOrg;

    procedure SetMinStepIncrement(const Value: Integer);
    procedure SetMinStepMilliseconds(const Value: Integer);
    function GetGlassColor: TColor;
    function GetGlassTranslucency: TFCTranslucency;
    function GetGlassVisible: Boolean;
    procedure SetGlassColor(const Value: TColor);
    procedure SetGlassTranslucency(const Value: TFCTranslucency);
    procedure SetGlassVisible(const Value: Boolean);
    function GetPicture: TPicture;
    function GetPictureMode: TFCPictureMode;
    function GetPictureTranspColor: TColor;
    function GetPictureVisible: Boolean;
    procedure SetPicture(const Value: TPicture);
    procedure SetPictureMode(const Value: TFCPictureMode);
    procedure SetPictureTranspColor(const Value: TColor);
    procedure SetPictureVisible(const Value: Boolean);
    procedure SetBorderPen(const Value: TPen);
    procedure SetFillBrush(const Value: TBrush);
  protected
    procedure Execute(Hiding, HasTransition: Boolean;
      AnimationData: TTEZoomFrameAnimationData);
    procedure ExecuteStep(Canvas: TCanvas; DesktopBmp, WorkBmp: TBitmap;
      Panel: TWinControl; var LastPaintedRect: TRect; AnimationRect: TRect;
      R: TRect; Chrono: TTEChrono);
  public
    constructor Create(AOwner: TComponent = nil); override;
    destructor  Destroy; override;
    {$ifndef TE_NOHLP}
    function  CreateAnimationData(Form: TCustomForm): TTEFormAnimationData; override;
    class function Description: String; override;
    procedure ExecuteHiding(AnimationData: TTEFormAnimationData); override;
    procedure ExecuteShowing(HasTransition: Boolean;
      AnimationData: TTEFormAnimationData;
      var CanDestroyAnimationData: Boolean); override;
    {$endif TE_NOHLP}
    procedure ShowForm(Form: TCustomForm); override;
    function  ShowModalForm(Form: TCustomForm): Integer; override;
    procedure ShowFormEx(Form: TCustomForm; Origin: TRect;
      Control: TControl = nil; ClientCoordinates: Boolean = False);
    function  ShowModalFormEx(Form: TCustomForm; Origin: TRect;
      Control: TControl = nil; ClientCoordinates: Boolean = False): Integer;
  published
    property BorderPen: TPen read FBorderPen write SetBorderPen;
    property DefaultOrigin: TTEZoomFrameAnimOrg read FDefaultOrigin write FDefaultOrigin default tezoFormCenter;
    property FillBrush: TBrush read FFillBrush write SetFillBrush;
    property GlassColor: TColor read GetGlassColor write SetGlassColor default clBlack;
    property GlassTranslucency: TFCTranslucency read GetGlassTranslucency write SetGlassTranslucency default 128;
    property GlassVisible: Boolean read GetGlassVisible write SetGlassVisible default True;
    property ShowFirstStep: Boolean read FShowFirstStep write FShowFirstStep default True;
    property ShowLastStep: Boolean read FShowLastStep write FShowLastStep default True;
    property MinStepIncrement: Integer read FMinStepIncrement write SetMinStepIncrement default 60;
    property MinStepMilliseconds: Integer read FMinStepMilliseconds write SetMinStepMilliseconds default 80;
    property Picture: TPicture read GetPicture write SetPicture;
    property PictureMode: TFCPictureMode read GetPictureMode write SetPictureMode default fcpmTile;
    property PictureTranspColor: TColor read GetPictureTranspColor write SetPictureTranspColor default clNone;
    property PictureVisible: Boolean read GetPictureVisible write SetPictureVisible default True;
  end;

implementation

uses ExtCtrls, teForm, teRender, teCtrls, teBlndWk;

type
  TTEBrush = class(TBrush)
  public
    constructor Create;
  published
    property Style default bsClear;
  end;

  TTEPen = class(TPen)
  public
    constructor Create;
  published
    property Color default clNavy;
    property Width default 2;
  end;

  TTEDesktopPaintPanel = class(TEffectsPanel)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

const
  Translucency = 0;

{ TTEZoomFrameAnimation }

constructor TTEZoomFrameAnimation.Create(AOwner: TComponent);
begin
  inherited;

  FShowFirstStep       := True;
  FShowLastStep        := True;
  FMinStepIncrement    := 60;
  FMinStepMilliseconds := 80;
  FDefaultOrigin       := tezoFormCenter;

  BkgrndOptions := TFCBackgroundOptions.Create;
  BkgrndOptions.Opaque             := False;
  BkgrndOptions.PictureMode        := fcpmTile;
  BkgrndOptions.GlassColor         := clBlack;
  BkgrndOptions.GlassTranslucency  := 128;
  BkgrndOptions.GlassVisible       := True;

  FBorderPen := TTEPen  .Create;
  FFillBrush := TTEBrush.Create;
end;

destructor TTEZoomFrameAnimation.Destroy;
begin
  BkgrndOptions.Free;
  FBorderPen   .Free;
  FFillBrush   .Free;

  inherited;
end;

procedure TTEZoomFrameAnimation.Execute(Hiding, HasTransition: Boolean;
  AnimationData: TTEZoomFrameAnimationData);

  function CalcNewCoordinate(
    const Origin, Destination, InBetweenSteps, InBetweenStep: Integer): Integer;
  begin
    Result :=
      Origin + Round(((Destination - Origin) / (InBetweenSteps + 1)) * InBetweenStep);
  end;

  function CalcRect(Origin, Destination: TRect;
    InBetweenSteps, InBetweenStep: Integer): TRect;
  begin
    Result.Left   := CalcNewCoordinate(Origin.Left  , Destination.Left  , InBetweenSteps, InBetweenStep);
    Result.Top    := CalcNewCoordinate(Origin.Top   , Destination.Top   , InBetweenSteps, InBetweenStep);
    Result.Right  := CalcNewCoordinate(Origin.Right , Destination.Right , InBetweenSteps, InBetweenStep);
    Result.Bottom := CalcNewCoordinate(Origin.Bottom, Destination.Bottom, InBetweenSteps, InBetweenStep);
  end;

var
  i,
  SaveIndex,
  Steps,
  InBetweenSteps: Integer;
  aux,
  FirstRect,
  LastRect,
  Origin,
  Target,
  LastPaintedRect,
  AnimationRect,
  DirtyR,
  FormR: TRect;
  Canvas: TCanvas;
  DesktopBmp,
  WorkBmp: TBitmap;
  PanelDesktop,
  PanelFrame: TTEDesktopPaintPanel;
  ShowFirstStepToUse,
  ShowLastStepToUse: Boolean;
  Chrono: TTEChrono;
begin
  if not Enabled then
    exit;

  LastPaintedRect := Rect(0, 0, 0, 0);
  Origin          := AnimationData.Origin;
  Target          := AnimationData.Form.BoundsRect;

  if AnimationData.Control <> nil
  then
  begin
    if EqualRect(Origin, Rect(-1, -1, -1, -1)) then
    begin // Origin has not been set, so calculate it
      if AnimationData.ClientCoordinates
      then Origin := AnimationData.Control.ClientRect                                       // Origin is client rect of the control
      else Origin := Rect(0, 0, AnimationData.Control.Width, AnimationData.Control.Height); // Origin is bounds rect of the control
    end;
    Origin.TopLeft     :=
      ControlClientToScreen(AnimationData.Control, Origin.TopLeft);
    Origin.BottomRight :=
      ControlClientToScreen(AnimationData.Control, Origin.BottomRight);
    if not AnimationData.ClientCoordinates then
    begin
      with ControlClientOffset(AnimationData.Control) do
        OffsetRect(Origin, -x, -y);
    end;
  end
  else
  begin
    if EqualRect(Origin, Rect(-1, -1, -1, -1)) then
    begin
      // Origin has not been set, so we use the default
      case TTEZoomFrameAnimation(AnimationData.Animation).DefaultOrigin of
        tezoCursor    : GetCursorPos(Origin.TopLeft);
        tezoFormCenter: Origin.TopLeft :=
          Point(
            Target.Left + ((Target.Right  - Target.Left) div 2),
            Target.Top  + ((Target.Bottom - Target.Top ) div 2));
      end;
      Origin.Right  := Origin.Left + 1;
      Origin.Bottom := Origin.Top  + 1;
      if TTEZoomFrameAnimation(AnimationData.Animation).DefaultOrigin = tezoCursor then
        AnimationData.Origin := Origin;
    end;
  end;

  // Calculate number of steps
  aux :=
    Rect(
      Abs(Origin.Left   - Target.Left  ),
      Abs(Origin.Top    - Target.Top   ),
      Abs(Origin.Right  - Target.Right ),
      Abs(Origin.Bottom - Target.Bottom));
  i := aux.Left;
  if aux.Top > i then
    i := aux.Top;
  if aux.Right > i then
    i := aux.Right;
  if aux.Bottom > i then
    i := aux.Bottom;
  Steps := i div FMinStepIncrement;
  if(Steps < 2) and (FShowFirstStep and FShowLastStep)
  then Steps := 2
  else if Steps < 1 then
    Steps := 1;

  if Hiding
  then
  begin
    aux    := Origin;
    Origin := Target;
    Target := aux;
    ShowFirstStepToUse := FShowLastStep;
    ShowLastStepToUse  := FShowFirstStep;
  end
  else
  begin
    ShowFirstStepToUse := FShowFirstStep;
    ShowLastStepToUse  := FShowLastStep;
  end;

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(0);
    try
      InBetweenSteps := Steps;
      if ShowFirstStepToUse then
        Dec(InBetweenSteps);
      if ShowLastStepToUse then
        Dec(InBetweenSteps);

      if ShowFirstStepToUse
      then
      begin
        if ShowLastStepToUse
        then
        begin
          FirstRect := Origin;
          LastRect  := Target;
        end
        else
        begin
          FirstRect := Origin;
          LastRect  := Target;
        end
      end
      else
      begin
        if ShowLastStepToUse
        then
        begin
          FirstRect := CalcRect(Origin, Target, InBetweenSteps, 1);
          LastRect  := Target;
        end
        else
        begin
          FirstRect := CalcRect(Origin, Target, InBetweenSteps, 1);
          LastRect  := CalcRect(Origin, Target, InBetweenSteps, InBetweenSteps);
        end;
      end;
      UnionRect(AnimationRect, FirstRect, LastRect);
      if not Hiding then
        UnionRect(AnimationRect, AnimationRect, Target);

      DesktopBmp := GetSnapShotImage(AnimationRect, DevicePixelFormat(False), True);
      try
        WorkBmp := TBitmap.Create;
        try
          WorkBmp.Canvas.Lock;
          try
            AdjustBmpForTransition(WorkBmp, 0,
              AnimationRect.Right - AnimationRect.Left,
              AnimationRect.Bottom - AnimationRect.Top, DevicePixelFormat(False));

            WorkBmp.Canvas.Brush.Style := bsClear;

            WorkBmp.Canvas.Pen.Assign(FBorderPen);
            if FBorderPen.Width > 1 then
              WorkBmp.Canvas.Pen.Style := psInsideFrame;
            WorkBmp.Canvas.Brush.Assign(FFillBrush);
            BitBlt(WorkBmp.Canvas.Handle, 0, 0,
              AnimationRect.Right - AnimationRect.Left,
              AnimationRect.Bottom - AnimationRect.Top, DesktopBmp.Canvas.Handle,
              0, 0, cmSrcCopy);

            PanelDesktop := nil;
            PanelFrame   := nil;
            try
              if WorkBmp.Canvas.Brush.Style <> bsSolid then
              begin
                PanelDesktop := TTEDesktopPaintPanel.Create(nil);
                PanelDesktop.Parent     := nil;
                PanelDesktop.BoundsRect := AnimationRect;
                PanelDesktop.BevelOuter := bvNone;
                PanelDesktop.BackgroundOptions.Picture.Assign(DesktopBmp);

                PanelFrame := TTEDesktopPaintPanel.Create(nil);
                PanelFrame.BevelOuter := bvNone;
                PanelFrame.BackgroundOptions.Assign(BkgrndOptions);
                if(not BkgrndOptions.PictureVisible) or (BkgrndOptions.Picture = nil) then
                  PanelFrame.BackgroundOptions.ParentPicture := True;
                PanelFrame.Parent  := PanelDesktop;
                PanelFrame.Visible := True;
              end;

              Chrono := TTEChrono.Create;
              try
                if ShowFirstStepToUse then
                  ExecuteStep(Canvas, DesktopBmp, WorkBmp, PanelFrame, LastPaintedRect,
                    AnimationRect, Origin, Chrono);
                for i := 1 to InBetweenSteps do
                  ExecuteStep(Canvas, DesktopBmp, WorkBmp, PanelFrame, LastPaintedRect,
                    AnimationRect,
                    CalcRect(Origin, Target, InBetweenSteps, i), Chrono);
                if ShowLastStepToUse and (InBetweenSteps >= 0) then
                  ExecuteStep(Canvas, DesktopBmp, WorkBmp, PanelFrame, LastPaintedRect,
                    AnimationRect, Target, Chrono);

                if not Chrono.IsReset then
                  while Chrono.Milliseconds < FMinStepMilliseconds do;

                // Restore screen
                if Hiding or HasTransition
                then
                begin
                  BitBlt(
                    Canvas.Handle,
                    LastPaintedRect.Left,
                    LastPaintedRect.Top,
                    LastPaintedRect.Right  - LastPaintedRect.Left,
                    LastPaintedRect.Bottom - LastPaintedRect.Top,
                    DesktopBmp.Canvas.Handle,
                    LastPaintedRect.Left   - AnimationRect.Left,
                    LastPaintedRect.Top    - AnimationRect.Top,
                    cmSrcCopy);
                end
                else
                begin
                  OffsetRect(LastPaintedRect, -AnimationRect.Left, -AnimationRect.Top);
                  BitBlt(
                    WorkBmp.Canvas.Handle,
                    LastPaintedRect.Left,
                    LastPaintedRect.Top,
                    LastPaintedRect.Right  - LastPaintedRect.Left,
                    LastPaintedRect.Bottom - LastPaintedRect.Top,
                    DesktopBmp.Canvas.Handle,
                    LastPaintedRect.Left,
                    LastPaintedRect.Top,
                    cmSrcCopy);

                  if not IsCompositionEnabled then
                  begin
                    // Draw form contents
                    FormR := AnimationData.Form.BoundsRect;
                    OffsetRect(FormR, -AnimationRect.Left, -AnimationRect.Top);
                    SaveIndex := SaveDC(WorkBmp.Canvas.Handle);
                    try
                      RenderWindowToBmp(WorkBmp, AnimationData.Form.Handle, 0,
                        AnimationData.Form, FormR, False, False, True, False,
                        WorkBmp.PixelFormat);
                    finally
                      RestoreDC(WorkBmp.Canvas.Handle, SaveIndex);
                    end;

                    UnionRect(DirtyR, FormR, LastPaintedRect);
                    OffsetRect(DirtyR, AnimationRect.Left, AnimationRect.Top);

                    // Draw to screen
                    BitBlt(
                      Canvas.Handle,
                      DirtyR.Left,
                      DirtyR.Top ,
                      DirtyR.Right  - DirtyR.Left,
                      DirtyR.Bottom - DirtyR.Top,
                      WorkBmp.Canvas.Handle,
                      DirtyR.Left - AnimationRect.Left,
                      DirtyR.Top  - AnimationRect.Top,
                      cmSrcCopy);
                  end;
                end;
              finally
                Chrono.Free;
              end;
            finally
              PanelFrame       .Free;
              PanelDesktop     .Free;
            end;
          finally
            WorkBmp.Canvas.Unlock;
          end;
        finally
          WorkBmp.Free;
        end;
      finally
        DesktopBmp.Free;
      end;
    finally
      ReleaseDC(0, Canvas.Handle);
    end;
  finally
    Canvas.Free;
  end;
end;

procedure TTEZoomFrameAnimation.ExecuteStep(Canvas: TCanvas;
  DesktopBmp, WorkBmp: TBitmap; Panel: TWinControl;
  var LastPaintedRect: TRect; AnimationRect: TRect; R: TRect;
  Chrono: TTEChrono);
var
  PanelR,
  BmpLastR,
  BmpR,
  DirtyR: TRect;
  SaveIndex: Integer;
  SaveTEAPIHooksDisabled: Boolean;
begin
  // Restore screen
  BmpLastR := LastPaintedRect;
  if IsRectEmpty(LastPaintedRect)
  then LastPaintedRect := R
  else
  begin
    OffsetRect(BmpLastR, -AnimationRect.Left, -AnimationRect.Top);
    BitBlt(
      WorkBmp.Canvas.Handle,
      BmpLastR.Left,
      BmpLastR.Top,
      BmpLastR.Right  - BmpLastR.Left,
      BmpLastR.Bottom - BmpLastR.Top,
      DesktopBmp.Canvas.Handle,
      BmpLastR.Left,
      BmpLastR.Top,
      cmSrcCopy);
  end;

  // Draw step
  BmpR := R;
  OffsetRect(BmpR, -AnimationRect.Left, -AnimationRect.Top);
  if WorkBmp.Canvas.Brush.Style <> bsSolid then
  begin
    // Repos panel
    PanelR := BmpR;
    if FBorderPen.Style <> psClear
    then
    begin
      InflateRect(PanelR, -FBorderPen.Width, -FBorderPen.Width);
      Panel.BoundsRect := PanelR;
    end
    else Panel.BoundsRect := PanelR;

    // Draw contents
    SaveTEAPIHooksDisabled := TEAPIHooksDisabled;
    TEAPIHooksDisabled     := True;
    SaveIndex := SaveDC(WorkBmp.Canvas.Handle);
    try
      RenderWindowToBmp(WorkBmp, Panel.Handle, 0, Panel, PanelR, False, False,
        False, False, WorkBmp.PixelFormat);
    finally
      TEAPIHooksDisabled := SaveTEAPIHooksDisabled;
      RestoreDC(WorkBmp.Canvas.Handle, SaveIndex);
    end;
  end;

  // Draw rect
  if(FBorderPen.Style <> psClear) or (FFillBrush.Style <> bsClear) then
    WorkBmp.Canvas.Rectangle(BmpR.Left, BmpR.Top, BmpR.Right, BmpR.Bottom);

  UnionRect(DirtyR, R, LastPaintedRect);

  if not Chrono.IsReset then
    while Chrono.Milliseconds < FMinStepMilliseconds do;
  Chrono.Reset;
  Chrono.Start;

  // Draw to screen
  BitBlt(
    Canvas.Handle,
    DirtyR.Left,
    DirtyR.Top ,
    DirtyR.Right  - DirtyR.Left,
    DirtyR.Bottom - DirtyR.Top,
    WorkBmp.Canvas.Handle,
    DirtyR.Left - AnimationRect.Left,
    DirtyR.Top  - AnimationRect.Top,
    cmSrcCopy);

  LastPaintedRect := R;
end;

function TTEZoomFrameAnimation.GetGlassColor: TColor;
begin
  Result := BkgrndOptions.GlassColor;
end;

function TTEZoomFrameAnimation.GetGlassTranslucency: TFCTranslucency;
begin
  Result := BkgrndOptions.GlassTranslucency;
end;

function TTEZoomFrameAnimation.GetGlassVisible: Boolean;
begin
  Result := BkgrndOptions.GlassVisible;
end;

function TTEZoomFrameAnimation.GetPicture: TPicture;
begin
  Result := BkgrndOptions.Picture;
end;

function TTEZoomFrameAnimation.GetPictureMode: TFCPictureMode;
begin
  Result := BkgrndOptions.PictureMode;
end;

function TTEZoomFrameAnimation.GetPictureTranspColor: TColor;
begin
  Result := BkgrndOptions.PictureTranspColor;
end;

function TTEZoomFrameAnimation.GetPictureVisible: Boolean;
begin
  Result := BkgrndOptions.PictureVisible;
end;

procedure TTEZoomFrameAnimation.SetGlassColor(const Value: TColor);
begin
  BkgrndOptions.GlassColor := Value;
end;

procedure TTEZoomFrameAnimation.SetGlassTranslucency(
  const Value: TFCTranslucency);
begin
  BkgrndOptions.GlassTranslucency := Value;
end;

procedure TTEZoomFrameAnimation.SetGlassVisible(const Value: Boolean);
begin
  BkgrndOptions.GlassVisible := Value;
end;

procedure TTEZoomFrameAnimation.SetMinStepIncrement(const Value: Integer);
begin
  if Value > 0
  then FMinStepIncrement := Value
  else FMinStepIncrement := 1;
end;

procedure TTEZoomFrameAnimation.SetPicture(const Value: TPicture);
begin
  BkgrndOptions.Picture := Value;
end;

procedure TTEZoomFrameAnimation.SetPictureMode(const Value: TFCPictureMode);
begin
  BkgrndOptions.PictureMode := Value;
end;

procedure TTEZoomFrameAnimation.SetPictureTranspColor(const Value: TColor);
begin
  BkgrndOptions.PictureTranspColor := Value;
end;

procedure TTEZoomFrameAnimation.SetPictureVisible(const Value: Boolean);
begin
  BkgrndOptions.PictureVisible := Value;
end;

procedure TTEZoomFrameAnimation.SetMinStepMilliseconds(const Value: Integer);
begin
  if Value > 0
  then FMinStepMilliseconds := Value
  else FMinStepMilliseconds := 1;
end;

procedure TTEZoomFrameAnimation.ShowForm(Form: TCustomForm);
begin
  DoShowForm(
    Form,
    False,
    CreateAnimationData(Form));
end;

procedure TTEZoomFrameAnimation.ShowFormEx(Form: TCustomForm; Origin: TRect;
  Control: TControl; ClientCoordinates: Boolean);
begin
  DoShowForm(
    Form,
    False,
    TTEZoomFrameAnimationData.CreateEx(Self, Form, Control, Origin, False));
end;

function TTEZoomFrameAnimation.ShowModalForm(Form: TCustomForm): Integer;
begin
  Result :=
    DoShowForm(
      Form,
      True,
      CreateAnimationData(Form));
end;

function TTEZoomFrameAnimation.ShowModalFormEx(Form: TCustomForm;
  Origin: TRect; Control: TControl; ClientCoordinates: Boolean): Integer;
begin
  Result :=
    DoShowForm(
      Form,
      True,
      TTEZoomFrameAnimationData.CreateEx(Self, Form, Control, Origin, False));
end;

procedure TTEZoomFrameAnimation.ExecuteHiding(
  AnimationData: TTEFormAnimationData);
begin
  Execute(True, False, AnimationData as TTEZoomFrameAnimationData);
end;

procedure TTEZoomFrameAnimation.ExecuteShowing(HasTransition: Boolean;
  AnimationData: TTEFormAnimationData; var CanDestroyAnimationData: Boolean);
var
  SaveOrigin: TRect;
begin
  SaveOrigin := AnimationData.Origin;
  Execute(False, HasTransition, AnimationData as TTEZoomFrameAnimationData);
  CanDestroyAnimationData :=
    not
    (
      EqualRect(SaveOrigin, Rect(-1, -1, -1, -1)) and
      (DefaultOrigin = tezoCursor) 
    );
end;

function TTEZoomFrameAnimation.CreateAnimationData(
  Form: TCustomForm): TTEFormAnimationData;
begin
  Result := TTEZoomFrameAnimationData.CreateEx(
    Self, Form, nil, Rect(-1, -1, -1, -1), False);
end;

class function TTEZoomFrameAnimation.Description: String;
begin
  Result := 'Zoom frames';
end;

procedure TTEZoomFrameAnimation.SetBorderPen(const Value: TPen);
begin
  FBorderPen.Assign(Value);
end;

procedure TTEZoomFrameAnimation.SetFillBrush(const Value: TBrush);
begin
  FFillBrush.Assign(Value);
end;

{ TTEZoomFrameAnimationData }

constructor TTEZoomFrameAnimationData.CreateEx(AnAnimation: TTEZoomFrameAnimation;
  AForm: TCustomForm; AControl: TControl; AnOrigin: TRect;
  AClientCoordinates: Boolean);
begin
  Create(AnAnimation, AForm, AControl, AnOrigin);

  ClientCoordinates := AClientCoordinates;
end;

{ TTEDesktopPaintPanel }

procedure TTEDesktopPaintPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    if (Parent = nil) and (ParentWindow = 0) then
    begin
      Style := WS_POPUP;
      WndParent := Application.Handle;
    end;
  end;
end;

constructor TTEPen.Create;
begin
  inherited Create;

  Color := clNavy;
  Width := 2;
end;

{ TTEBrush }

constructor TTEBrush.Create;
begin
  inherited Create;

  Style := bsClear;
end;

initialization
  {$ifdef D6UP}
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TTEZoomFrameAnimation, Controls.TControl);
  {$endif D6UP}

  Classes.RegisterClass(TTEZoomFrameAnimation);
end.
