unit teVclScr;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, TransEff;
{$INCLUDE teDefs.inc}

type
  TTERenderWindow = class(TCustomControl)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd);
    message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    Palette: HPalette;
    BkPicture: TBitmap;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Canvas;
  end;

  TTEVCLScreenTrDevice = class(TTETransitionDevice)
  private
    FRenderWindow: TTERenderWindow;
    FPrepared: Boolean;
    FFrozen: Boolean;
    FClientCoordinates: Boolean;
  protected
    SaveCtrl: TControl;
    SaveR, ScreenR: TRect;
    SaveStyle: Longint;
    OpeningForm, ClosingForm, UseClientCoordinates: Boolean;
{$IFNDEF TE_NOHLP}
    ClipRgn: HRGN;
    LayerAlpha: Byte;
    LayerKey: COLORREF;
    LayerFlags: DWord;
{$ENDIF TE_NOHLP}
    procedure CustomExecute; override;
    function GetDelegateTransition(Original: TTransitionEffect;
      const ReturnCopy: Boolean): TTransitionEffect; override;
    function GetRenderWndHandle: HWnd; override;
    class function TransitionIsDisabled(Transition: TTransitionEffect;
      NoFlickerFreeWhenDisabled: Boolean): Boolean; override;
  public
    UseBkPicture: Boolean;

    constructor Create; override;
    destructor Destroy; override;
    function AvoidScrolling: Boolean; override;

    function Clipped: Boolean; override;
    procedure Defrost;
    procedure Execute(WaitForCompletion: Boolean = True); override;
    function Freeze(Ctrl: TControl; R: TRect): Boolean;
    procedure GetOffScreenBmp(var OldPalette: HPalette); override;
    function HasPalette: Boolean; override;
    function PixelFormat: TPixelFormat; override;
    function Prepare(Ctrl: TControl; R: TRect): Boolean;
    procedure Prepare2ndPass;
    function TwoPassesCapable: Boolean; override;
    procedure UnPrepare;

    property ClientCoordinates
      : Boolean read FClientCoordinates write FClientCoordinates;
    property Frozen: Boolean read FFrozen;
    property Prepared: Boolean read FPrepared;
    property RenderWindow: TTERenderWindow read FRenderWindow;
  end;

var
{$IFNDEF TE_NOHLP}
  TEVclScrPrepared: Boolean; // Avoids nested transitions
{$ENDIF TE_NOHLP}

implementation

uses teRender, ComCtrls, teChrono, teTimed;

const
  WS_EX_LAYERED = $00080000;

type
  TTEWinControl = class(TWinControl);
    TTECustomForm = class(TCustomForm);
    TTEScrollingWinControl = class(TScrollingWinControl);
    TTransitionEffectHack = class(TTransitionEffect);

    TLayeredBlendTransition = class(TTimedTransitionEffect)
      public Opening: Boolean;
    MaxAlpha: Byte;
    Key: COLORREF;
    Flags: DWord;
  protected
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    procedure Finalize(Data: TTETransitionData); override;
  end;

constructor TTEVCLScreenTrDevice.Create;
begin
  inherited;

  ClipRgn := 0;
  FClientCoordinates := True;
  FFrozen := False;
  FPrepared := False;
  OpeningForm := False;
  ClosingForm := False;
  UseBkPicture := False;
  FRenderWindow := nil;
end;

destructor TTEVCLScreenTrDevice.Destroy;
begin
  UnPrepare;

  inherited;
end;

function TTEVCLScreenTrDevice.Clipped: Boolean;
var
  RenderHandle: HWnd;
begin
  if SaveCtrl <> nil then
  begin
    if RenderWindow <> nil then
      RenderHandle := RenderWindow.Handle
    else
      RenderHandle := 0;
    Result := IsWindowClipped(TWinControl(SaveCtrl).Handle, RenderHandle,
      ScreenR);
  end
  else
    Result := False;
end;

procedure TTEVCLScreenTrDevice.Defrost;
var
  ParentWindow: TWinControl;
begin
  if RenderWindow <> nil then
  begin
    if ClipRgn <> 0 then
    begin
      DeleteObject(ClipRgn);
      ClipRgn := 0;
    end;

    ParentWindow := RenderWindow.Parent;
    // This avoids flickering under some circumstances
    if ParentWindow <> nil then
      ParentWindow.DisableAlign;

    if SaveStyle <> 0 then
    begin
      SetWindowLong(ParentWindow.Handle, GWL_STYLE, SaveStyle);
      SaveStyle := 0;
    end;

    RenderWindow.Free;
    if ParentWindow <> nil then
    begin
      ParentWindow.ControlState := ParentWindow.ControlState -
        [csAlignmentNeeded];
      ParentWindow.EnableAlign;
    end;
    FRenderWindow := nil;
  end;
  SaveCtrl := nil;

  if Data <> nil then
    Finalize;

  FFrozen := False;
end;

procedure TTEVCLScreenTrDevice.Execute(WaitForCompletion: Boolean = True);
begin
  if (ClipRgn <> 0) and Assigned(RenderWindow) then // this has to be done before UseOffScreenBmp is called
  begin
    SetWindowRgn(RenderWindow.Handle, ClipRgn, False);
    ClipRgn := 0;
  end;

  inherited;
end;

function TTEVCLScreenTrDevice.Freeze(Ctrl: TControl; R: TRect): Boolean;
var
  Bounds: TRect;
  ParentCtrl: TWinControl;
  Order, Ok: Boolean;
  VHandle: HWnd;
  Cursor: TCursor;
  Flags: DWord;

  procedure SetCtrlToParent;
  begin
    if Ctrl.Parent = nil then
      Exit;

    if not UseClientCoordinates then
    begin
      with ControlClientOffset(Ctrl) do
        OffsetRect(R, Ctrl.Left + X, Ctrl.Top + Y);
    end
    else
      OffsetRect(R, Ctrl.Left, Ctrl.Top);
    Ctrl := Ctrl.Parent;
    UseClientCoordinates := True;
  end;

  procedure SetChildOrderAfter(Child: TWinControl; Control: TControl);
  var
    i: Integer;
  begin
    for i := 0 to Child.Parent.ControlCount do
    begin
      if Child.Parent.Controls[i] = Control then
      begin
        TTEWinControl(Child.Parent).SetChildOrder(Child, i + 1);
        break;
      end;
    end;
  end;

  function GetMDIClientClippingRgn: HRGN;
  var
    MDIClientWindowClientRect, MDIChildRect, VisibleRect: TRect;
  begin
    GetClientRect(Application.MainForm.ClientHandle, MDIClientWindowClientRect);
    MDIChildRect := SaveCtrl.BoundsRect;
    OffsetRect(MDIClientWindowClientRect, -MDIChildRect.Left,
      -MDIChildRect.Top);
    OffsetRect(MDIChildRect, -MDIChildRect.Left, -MDIChildRect.Top);
    IntersectRect(VisibleRect, MDIClientWindowClientRect, MDIChildRect);
    Result := CreateRectRgn(VisibleRect.Left, VisibleRect.Top,
      VisibleRect.Right, VisibleRect.Bottom);
  end;

var
  MDIFullForm: Boolean;
begin
  if TransitionToUse = nil then
    raise ETransitionEffectError.Create(rsTEDevTrIsNil);

  Result := False;

  if Frozen then
  begin
    if (Ctrl = SaveCtrl)
      then
    begin
      Result := True;
      Exit;
    end
    else
      Defrost;
  end;

  if not AllowTransition then
    Exit;

  if TEVclScrPrepared then
    Exit;

  Cursor := Ctrl.Cursor;

  UseClientCoordinates := ClientCoordinates;
  if not(Ctrl is TWinControl) then
    SetCtrlToParent;

  MDIFullForm := (OpeningForm or ClosingForm) and (Ctrl is TCustomForm) and
    (TTECustomForm(Ctrl).FormStyle = fsMDIChild);

  Ok := True;
  repeat
    if not Ok then
      SetCtrlToParent;

    if Ctrl.Parent is TPageControl then
      SetCtrlToParent;

    if MDIFullForm then
      VHandle := Application.MainForm.Handle
    else
      VHandle := TWinControl(Ctrl).Handle;

    Ok := IsWindowVisible(VHandle);
  until Ok or (Ctrl.Parent = nil);

  if not Ok then
    Exit;

  if (not UseClientCoordinates) and (not MDIFullForm) then
  begin
    with ControlClientOffset(Ctrl) do
    begin
      if (X <= R.Left) and (Y <= R.Top) and
        ((R.Right - X) <= ControlClientWidth(Ctrl)) and
        ((R.Bottom - Y) <= ControlClientHeight(Ctrl)) then
      begin
        UseClientCoordinates := True;
        OffsetRect(R, -X, -Y);
      end;
    end;
  end;

  if (not UseClientCoordinates) and (Ctrl is TCustomForm) and
    (TTECustomForm(Ctrl).FormStyle <> fsMDIChild) and (Ctrl.Parent = nil) then
    ScreenR := R
  else
  begin
    ScreenR.TopLeft := ControlClientToScreen(Ctrl, R.TopLeft);
    ScreenR.BottomRight := ControlClientToScreen(Ctrl, R.BottomRight);

    if not UseClientCoordinates then
      with ControlClientOffset(Ctrl) do
        OffsetRect(ScreenR, -X, -Y);
  end;

  if not OpeningForm then
    Ctrl.Update;
  // Application.ProcessMessages; // This messes up events

  SaveCtrl := Ctrl;
  Order := False;
  if MDIFullForm then
  begin
    ParentCtrl := Application.MainForm;
    Bounds.TopLeft := ControlScreenToClient(ParentCtrl, ScreenR.TopLeft);
    Bounds.BottomRight := ControlScreenToClient
      (ParentCtrl, ScreenR.BottomRight);
    SaveR := R;
  end
  else
  begin
    if UseClientCoordinates and (not ClosingForm) then
      ParentCtrl := TWinControl(SaveCtrl)
    else
    begin
      if SaveCtrl.Parent <> nil then
      begin
        ParentCtrl := SaveCtrl.Parent;
        Order := True;
      end
      else
        ParentCtrl := nil;
    end;

    if ParentCtrl = nil then
      Bounds := ScreenR
    else
    begin
      Bounds.TopLeft := ControlScreenToClient(ParentCtrl, ScreenR.TopLeft);
      Bounds.BottomRight := ControlScreenToClient
        (ParentCtrl, ScreenR.BottomRight);
    end;

    SaveR.TopLeft := ControlScreenToClient(SaveCtrl, ScreenR.TopLeft);
    SaveR.BottomRight := ControlScreenToClient(SaveCtrl, ScreenR.BottomRight);
    if not UseClientCoordinates then
    begin
      with ControlClientOffset(SaveCtrl) do
        OffsetRect(SaveR, X, Y);
    end;
  end;

  try
    if Data = nil then
      Initialize;
    if not(TransitionToUse is TLayeredBlendTransition) then
    begin
      FRenderWindow := TTERenderWindow.Create(Ctrl);
      RenderWindow.Cursor := Cursor;
    end;

    if Assigned(RenderWindow) then
    begin
      if UseBkPicture and Assigned(SrcImage) then
      begin
        RenderWindow.BkPicture := TBitmap.Create;
        RenderWindow.BkPicture.Assign(SrcImage);
      end;

      if Assigned(ParentCtrl) then
      begin
        RenderWindow.Parent := ParentCtrl;
        SaveStyle := GetWindowLong(ParentCtrl.Handle, GWL_STYLE);
        if (SaveStyle and WS_CLIPCHILDREN) = 0 then
          SetWindowLong(ParentCtrl.Handle, GWL_STYLE,
            SaveStyle or WS_CLIPCHILDREN)
        else
          SaveStyle := 0;
      end;

      if Order then
        SetChildOrderAfter(RenderWindow, Ctrl);
      RenderWindow.BoundsRect := Bounds;

      Data.Width := RenderWindow.Width;
      Data.Height := RenderWindow.Height;
      Data.DeviceCanvas := RenderWindow.Canvas;

      if (ParentCtrl = nil) or
        (MDIFullForm and (TTECustomForm(SaveCtrl).WindowState <> wsMaximized)
          and ClosingForm) then
      begin
        if (not OpeningForm) and (ClipRgn = 0) then
        begin
          ClipRgn := CreateRectRgn(0, 0, 0, 0);
          if GetWindowRgn(TWinControl(Ctrl).Handle, ClipRgn) = ERROR then
          begin
            DeleteObject(ClipRgn);
            ClipRgn := 0;
          end
          else
          begin
            if MDIFullForm then
              CombineRgn(ClipRgn, ClipRgn, GetMDIClientClippingRgn, RGN_AND);
          end;
        end;

        Flags := SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE;
        SetWindowPos(RenderWindow.Handle, GetWindow(TWinControl(Ctrl).Handle,
            GW_HWNDPREV), 0, 0, 0, 0, Flags);
        if OpeningForm and (SrcImage <> nil) then // To reduce flickering when layered windows underneath
          BitBlt(Data.DeviceCanvas.Handle, 0, 0, RenderWindow.Width,
            RenderWindow.Height, SrcImage.Canvas.Handle, 0, 0, cmSrcCopy);
      end
      else
      begin
        if MDIFullForm and (TTECustomForm(SaveCtrl).WindowState <> wsMaximized)
          then
        begin
          if ClipRgn = 0 then
            ClipRgn := GetMDIClientClippingRgn
          else
            CombineRgn(ClipRgn, ClipRgn, GetMDIClientClippingRgn, RGN_AND);
        end;

        ShowWindow(RenderWindow.Handle, SW_SHOWNA);
      end;

      Data.DeviceWnd := RenderWindow.Handle;

      ValidateRect(RenderWindow.Handle, nil);
      // We don't want to receive WM_PAINT
    end
    else
      Data.DeviceWnd := TTECustomForm(Ctrl).Handle;

    FFrozen := True;
  except
    on Exception do
    begin
      Defrost;
      raise ;
    end;
  end;
  Result := FFrozen;
end;

function TTEVCLScreenTrDevice.PixelFormat: TPixelFormat;
begin
  Result := DevicePixelFormat(False);
end;

function TTEVCLScreenTrDevice.Prepare(Ctrl: TControl; R: TRect): Boolean;
var
  auxR: TRect;
  NeedSrcBmp: Boolean;
begin
  if TransitionToUse = nil then
    raise ETransitionEffectError.Create(rsTEDevTrIsNil);

  Result := False;

  if Prepared then
    UnPrepare;

  if not AllowTransition then
    Exit;

  try
    if not Frozen then
      SaveCtrl := Ctrl; // Needed for GetDelegateTransition

    if Data = nil then
      Initialize;

    Data.Width := R.Right - R.Left;
    Data.Height := R.Bottom - R.Top;

    NeedSrcBmp := tetiNeedSrcBmp in TTransitionEffectHack(DelegateTransition)
      .GetInfo(Self);

    if (not TEVclScrPrepared) and OpeningForm and NeedSrcBmp and
      (TEWinVersion >= teWin2000) and (TForm(SaveCtrl).FormStyle <> fsMDIChild)
      then
    begin // To avoid problems with layered windows underneath
      auxR := Ctrl.BoundsRect;
      auxR.Right := auxR.Left + TTransitionEffectHack(DelegateTransition)
        .GetBitmapsWidth(Data);
      SrcImage := GetSnapShotImage(auxR, TTransitionEffectHack
          (DelegateTransition).GetPixelFormat(Self), True);
    end;

    if not Freeze(Ctrl, R) then
      Exit;

    FPrepared := True;
    TEVclScrPrepared := True;

    if NeedSrcBmp and (SrcImage = nil) then
    begin
      if (not DelegateTransition.NeverRendering) and
        (DelegateTransition.ForceRendering or
          ((not(tetiStaticSrcPixels in TTransitionEffectHack(DelegateTransition)
                .GetInfo(Self))) and Clipped)) then
      begin
        SrcImage := RenderControl(SaveCtrl, Data.DeviceWnd, Rect
            (SaveR.Left, SaveR.Top, SaveR.Left + TTransitionEffectHack
              (DelegateTransition).GetBitmapsWidth(Data), SaveR.Bottom),
          UseClientCoordinates, False, False, Data.PixelFormat);
      end;
    end;
  except
    on Exception do
    begin
      UnPrepare;
      raise ;
    end;
  end;
  Result := Prepared;
end;

procedure TTEVCLScreenTrDevice.Prepare2ndPass;
begin
  if TransitionToUse = nil then
    raise ETransitionEffectError.Create(rsTEDevTrIsNil);

  if not Prepared then
    Exit;

  if (SaveCtrl <> nil) and (TransitionToUse.Passes(Self) = 2) and
    (not TransitionToUse.Pass2Options.UseSolidColor) then
    Pass2Image := RenderControl
      (SaveCtrl, 0, Rect(SaveR.Left, SaveR.Top,
        SaveR.Left + TTransitionEffectHack(TransitionToUse).GetBitmapsWidth
          (Data), SaveR.Bottom), UseClientCoordinates, False, False,
      Data.PixelFormat);
end;

class function TTEVCLScreenTrDevice.TransitionIsDisabled
  (Transition: TTransitionEffect; NoFlickerFreeWhenDisabled: Boolean): Boolean;
begin
  Result := TEGlobalDisabledStrict or
    ((NoFlickerFreeWhenDisabled or (not Transition.FlickerFreeWhenDisabled))
      and (( inherited TransitionIsDisabled(Transition,
          NoFlickerFreeWhenDisabled)) or TEGlobalDisabled));
end;

procedure TTEVCLScreenTrDevice.UnPrepare;
begin
  if Prepared then
  begin
    FreeAndNil(SrcImage);
    FreeAndNil(Pass2Image);
    FreeAndNil(DstImage);
    FPrepared := False;
    TEVclScrPrepared := False;
  end;

  Defrost;
end;

{ TTERenderWindow }
constructor TTERenderWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := 'teRenderWindow';
  Visible := False;
  Palette := 0;
  Color := clPurple;
  BkPicture := nil;
end;

procedure TTERenderWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    if (Parent = nil) and (ParentWindow = 0) then
    begin
      Style := WS_POPUP;

      if (Owner is TWinControl) and
        ((GetWindowLong(TWinControl(Owner).Handle, GWL_EXSTYLE)
            and WS_EX_TOPMOST) <> 0) then
        ExStyle := ExStyle or WS_EX_TOPMOST;

      WndParent := Application.Handle;
    end;
  end;
end;

destructor TTERenderWindow.Destroy;
begin
  BkPicture.Free;

  inherited;
end;

procedure TTERenderWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if Assigned(BkPicture) then
    BitBlt(Canvas.Handle, 0, 0, Width, Height, BkPicture.Canvas.Handle, 0, 0,
      cmSrcCopy);

  Message.Result := 1;
end;

function TTEVCLScreenTrDevice.AvoidScrolling: Boolean;
begin
  Result := WindowHasRegion(RenderWindow.Handle);
end;

procedure TTEVCLScreenTrDevice.GetOffScreenBmp(var OldPalette: HPalette);
begin
  OldPalette := 0;
  if (Data.SrcBmp = nil) and (tetiNeedSrcBmp in TTransitionEffectHack
      (DelegateTransition).GetInfo(Self)) then
  begin
    SrcImage := GetSnapShotImage(Rect(ScreenR.Left, ScreenR.Top,
        ScreenR.Left + TTransitionEffectHack(DelegateTransition).GetBitmapsWidth
          (Data), ScreenR.Bottom), TTransitionEffectHack(DelegateTransition)
        .GetPixelFormat(Self), True);
    Data.SrcBmp := SrcImage;
  end;

  inherited;
end;

function TTEVCLScreenTrDevice.HasPalette: Boolean;
begin
  Result := PalettedDevice(False);
end;

procedure TTEVCLScreenTrDevice.CustomExecute;
var
  CaretWnd: HWnd;
  R, R2: TRect;
  SaveExStyle: Longint;
  OldPalette: HPalette;
  DirtyRender, UsingLayerTransition: Boolean;
  Msg: TMsg;
  Pass2Chrono: TTEChrono;
  TotalMilliseconds: Integer;
  Flags: DWord;
begin
  if not Prepared then
  begin
    Defrost;
    Exit;
  end;
  try
    if Assigned(Screen.ActiveControl) then
    begin
      CaretWnd := Screen.ActiveControl.Handle;
      if CaretWnd <> 0 then
        HideCaret(CaretWnd);
    end
    else
      CaretWnd := 0;
    try
      if ClosingForm and (tetiNeedDstBmp in TTransitionEffectHack
          (DelegateTransition).GetInfo(Self)) then
      begin
        if TTECustomForm(SaveCtrl).FormStyle = fsMDIChild then
        begin
          if DstImage = nil then
          begin
            if GetMaximizedMDIChild(SaveCtrl as TWinControl) then
            begin
              DstImage := RenderWindowToBmp
                (nil, Application.MainForm.ClientHandle, TCustomForm(SaveCtrl)
                  .Handle, nil, Rect(0, 0, TTransitionEffectHack
                    (DelegateTransition).GetBitmapsWidth(Data), Data.Height),
                True, True, False, False, Data.PixelFormat);
            end
            else
            begin
              DstImage := RenderWindowToBmp
                (nil, Application.MainForm.ClientHandle, TCustomForm(SaveCtrl)
                  .Handle, nil, Rect(SaveCtrl.BoundsRect.Left,
                  SaveCtrl.BoundsRect.Top, SaveCtrl.BoundsRect.Left +
                    TTransitionEffectHack(DelegateTransition).GetBitmapsWidth
                    (Data), SaveCtrl.BoundsRect.Bottom), True, True, False,
                False, Data.PixelFormat);
            end;
          end;
        end
        else
        begin
          // Check if clipped by the screen (that would cause flickering)
          GetWindowRect(RenderWindow.Handle, R);
{$IFDEF D6UP}
          R2 := Screen.DesktopRect;
{$ELSE}
          R2 := Bounds(Screen.DesktopLeft, Screen.DesktopTop,
            Screen.DesktopWidth, Screen.DesktopHeight);
{$ENDIF D6UP}
          IntersectRect(R2, R, R2);
          if not EqualRect(R, R2) then
          begin
            SetWindowPos(RenderWindow.Handle, 0, R2.Left, R2.Top,
              R2.Right - R2.Left - 1, R2.Bottom - R2.Top - 1,
              SWP_NOACTIVATE or SWP_NOZORDER);
          end;
          try
            if DstImage = nil then
            begin
              SaveExStyle := GetWindowLong(RenderWindow.Handle, GWL_EXSTYLE);
              SetWindowLong(RenderWindow.Handle, GWL_EXSTYLE,
                SaveExStyle or WS_EX_LAYERED);
              try
                Sleep(50); // The system needs some time to process previous sentence
                DstImage := GetSnapShotImage
                  (Rect(ScreenR.Left, ScreenR.Top,
                    ScreenR.Left + TTransitionEffectHack(DelegateTransition)
                      .GetBitmapsWidth(Data), ScreenR.Bottom),
                  TTransitionEffectHack(DelegateTransition).GetPixelFormat(Self)
                    , False);
              finally
                SetWindowLong(RenderWindow.Handle, GWL_EXSTYLE, SaveExStyle);
              end;
            end;
          finally
            if not EqualRect(R, R2) then
            begin
              // Recover previous bounds rect
              SetWindowPos(RenderWindow.Handle, 0, R.Left, R.Top,
                R.Right - R.Left - 1, R.Bottom - R.Top - 1,
                SWP_NOACTIVATE or SWP_NOZORDER);
            end;
          end;
        end;
      end;

      if (Pass2Image = nil) and (DelegateTransition.Passes(Self) = 2) then
      begin
        if DelegateTransition.Pass2Options.SolidColor = clNone then
          DelegateTransition.Pass2Options.SolidColor := TTEWinControl(SaveCtrl)
            .Color;
        Get2ndPassBmp;
      end;

      if Pass2Image = nil then
      begin
        RealizeControlPalette(SaveCtrl, False);
        if (DstImage = nil) and (tetiNeedDstBmp in TTransitionEffectHack
            (DelegateTransition).GetInfo(Self)) then
        begin
          DstImage := RenderControl(SaveCtrl, FRenderWindow.Handle, Rect
              (SaveR.Left, SaveR.Top, SaveR.Left + TTransitionEffectHack
                (DelegateTransition).GetBitmapsWidth(Data), SaveR.Bottom),
            UseClientCoordinates, False, False, TTransitionEffectHack
              (DelegateTransition).GetPixelFormat(Self));
        end;

        Data.SrcBmp := SrcImage;
        Data.DstBmp := DstImage;
        GetOffScreenBmp(OldPalette);
        try
          ExePass(1, nil, DelegateTransition.Milliseconds);
        finally
          if OldPalette <> 0 then
            SelectPalette(RenderWindow.Canvas.Handle, OldPalette, True);
        end;
      end
      else
      begin
        TotalMilliseconds := DelegateTransition.Milliseconds;
        if DelegateTransition.Pass2Options.DistributedTime and
          (DelegateTransition.Milliseconds <> 0) then
        begin
          DelegateTransition.Milliseconds := TotalMilliseconds DIV 2;
          Pass2Chrono := TTEChrono.Create;
        end
        else
          Pass2Chrono := nil;

        try
          Data.SrcBmp := SrcImage;
          Data.DstBmp := Pass2Image;
          GetOffScreenBmp(OldPalette);
          try
            ExePass(1, Pass2Chrono, TotalMilliseconds);
          finally
            if OldPalette <> 0 then
              SelectPalette(RenderWindow.Canvas.Handle, OldPalette, True);
          end;

          FreeAndNil(SrcImage);

          RealizeControlPalette(SaveCtrl, False);
          if (DstImage = nil) and (tetiNeedDstBmp in TTransitionEffectHack
              (DelegateTransition).GetInfo(Self)) then
          begin
            DstImage := RenderControl(SaveCtrl, FRenderWindow.Handle, Rect
                (SaveR.Left, SaveR.Top, SaveR.Left + TTransitionEffectHack
                  (DelegateTransition).GetBitmapsWidth(Data), SaveR.Bottom),
              UseClientCoordinates, False, False, TTransitionEffectHack
                (DelegateTransition).GetPixelFormat(Self));
          end;

          Data.SrcBmp := Pass2Image;
          Data.DstBmp := DstImage;
          GetOffScreenBmp(OldPalette);
          try
            ExePass(2, Pass2Chrono, TotalMilliseconds);
          finally
            if OldPalette <> 0 then
              SelectPalette(RenderWindow.Canvas.Handle, OldPalette, True);
          end;
        finally
          Pass2Chrono.Free;
        end;
      end;
      if Assigned(RenderWindow) then
      begin
        DirtyRender := False;
        while PeekMessage(Msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) do
        begin
          if (not DirtyRender) and (Msg.HWnd = RenderWindow.Handle) then
            DirtyRender := True;
          DispatchMessage(Msg);
        end;
        if DirtyRender and Assigned(DstImage) then
          BitBlt(Data.DeviceCanvas.Handle, 0, 0, RenderWindow.Width,
            RenderWindow.Height, DstImage.Canvas.Handle, 0, 0, cmSrcCopy);

        Flags := SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or
          SWP_HIDEWINDOW;
        UsingLayerTransition := TTransitionEffectHack(DelegateTransition)
          .ClassNameIs('TLayeredBlendTransition');
        if (not ClosingForm) or UsingLayerTransition then
          Flags := Flags or SWP_NOREDRAW;
        SetWindowPos(RenderWindow.Handle, 0, 0, 0, 0, 0, Flags);
        if (SaveCtrl is TWinControl) and (not ClosingForm) and
          (not UsingLayerTransition) then
          RefreshWindows(TWinControl(SaveCtrl).Handle);
      end;
    finally
      if CaretWnd <> 0 then
        ShowCaret(CaretWnd);
    end;
  finally
    UnPrepare;
  end;
end;

function TTEVCLScreenTrDevice.GetRenderWndHandle: HWnd;
begin
  Result := RenderWindow.Handle;
end;

function TTEVCLScreenTrDevice.TwoPassesCapable: Boolean;
begin
  Result := True;
end;

function TTEVCLScreenTrDevice.GetDelegateTransition
  (Original: TTransitionEffect; const ReturnCopy: Boolean): TTransitionEffect;
begin
  if(
      {$ifndef AERO_RENDER}OpeningForm or {$endif AERO_RENDER}
      ClosingForm
    )                                                 and
    (TTECustomForm(SaveCtrl).FormStyle <> fsMDIChild) and
    IsCompositionEnabled                              and
    (not(Original is TFlickerFreeTransition))         and
    (
      ClosingForm                                   or
      (TCustomForm(SaveCtrl).BorderStyle <> bsNone) or
      (Original.ClassName = 'TBlendTransition')
    ) then
  begin
    Result := TLayeredBlendTransition.Create(nil);
    with TLayeredBlendTransition(Result) do
    begin
      Opening  := OpeningForm;
      MaxAlpha := LayerAlpha;
      Key      := LayerKey;
      Flags    := LayerFlags;
    end;
    Result.Assign(Original);
  end
  else
    Result := inherited GetDelegateTransition(Original, ReturnCopy);
end;

{ TLayeredBlendTransition }

procedure TLayeredBlendTransition.Initialize(Data: TTETransitionData;
  var TotalFrames: Integer);
begin
  inherited;

  TotalFrames := MaxAlpha - 1;
end;

procedure TLayeredBlendTransition.Finalize(Data: TTETransitionData);
var
  Alpha: Byte;
begin
  if Opening then
    Alpha := MaxAlpha
  else
    Alpha := 0;
  teRender.SetLayeredWindowAttributes(Data.DeviceWnd, Key, Alpha,
    Flags or LWA_ALPHA);
  Sleep(1);

  inherited;
end;

procedure TLayeredBlendTransition.ExecuteFrame(Data: TTETransitionData;
  CurrentFrame, Step, LastExecutedFrame: Integer);
var
  Alpha: Byte;
begin
{$IFDEF LogTiming}
  if Assigned(Log) then
    Log.ChronoExtra.Start;
{$ENDIF LogTiming}
  if Opening then
    Alpha := CurrentFrame
  else
    Alpha := MaxAlpha - CurrentFrame;
  teRender.SetLayeredWindowAttributes(Data.DeviceWnd, Key, Alpha,
    Flags or LWA_ALPHA);
  Sleep(1);
{$IFDEF LogTiming}
  if Assigned(Log) then
  begin
    Log.ChronoExtra.Pause;
    Log.CurrentItem^.LogExTime := Log.ChronoExtra.Milliseconds;
    Log.ChronoExtra.Reset;
  end;
{$ENDIF LogTiming}
end;

function TLayeredBlendTransition.GetInfo(Device: TTETransitionDevice)
  : TTETransitionInfo;
begin
  Result := [tetiMillisecondsCapable];
end;

initialization

TEVclScrPrepared := False;

end.
