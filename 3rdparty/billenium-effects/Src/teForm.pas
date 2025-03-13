unit teForm;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, TransEff, teBkgrnd, FormCont, Windows, Messages, Forms,
  Graphics, Controls, teFormAn, teVclScr;

type
  TTEFormTransitionsOnAfterShowEvent = procedure(Sender: TObject;
    const FirstTime: Boolean) of object;

  {$ifndef TE_NOHLP}
  TTECustomForm = class(TCustomForm);
  TTELockData = record
    Locked: Boolean;
    UseRegion: Boolean;
    Region: HRGN;
    Key: COLORREF;
    Alpha: Byte;
    Flags: DWord;
  end;
  {$endif TE_NOHLP}

  TFormTransitions = class(TComponent)
  private
    FBackgroundOptions: TFCBackgroundOptions;
    FDestroyTransitions: Boolean;
    FEnabled: Boolean;
    FHideTransReversed: Boolean;
    FirstTimeShowed: Boolean;
    FOwnerForm: TTECustomForm;
    FShowTransition: TTransitionEffect;
    FAnimationData: TTEFormAnimationData;
    WasVisible,
    IsAppMinimizing: Boolean; // Make sure hiding effects do not execute when application is minimizing
    WindowProcBak: TWndMethod;
    FShowAnimation: TTEFormAnimation;
    FHideAnimation: TTEFormAnimation;
    FHideTransition: TTransitionEffect;
    FOnAfterShow: TTEFormTransitionsOnAfterShowEvent;

    ShowEffectWaiting: Boolean;

    procedure SetBackgroundOptions(Value: TFCBackgroundOptions);
    procedure SetShowTransition(const Value: TTransitionEffect);
    function  GetVersion: String;
    procedure SetVersion(const Value: String);

    procedure ActivateHookForm(const Activate: Boolean);
    procedure ActivateHookMDIClient(const Activate: Boolean);
    procedure ActivateHookMDIClientTrans(const Activate: Boolean);
    procedure ActivateHookMDIClientBkgrnd(const Activate: Boolean);
    function  MainWndHook(var Message: TMessage): Boolean;
    procedure NewWindowProc(var Message: TMessage);
    procedure SetHideAnimation(const Value: TTEFormAnimation);
    procedure SetShowAnimation(const Value: TTEFormAnimation);
    procedure SetHideTransition(const Value: TTransitionEffect);
  protected
    Device: TTEVCLScreenTrDevice;
    LockData: TTELockData;

    function  CanEnable: Boolean;
    function  GetPalette: HPalette;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property OwnerForm: TTECustomForm read FOwnerForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PrepareAnimation(AnAnimationData: TTEFormAnimationData);
  published
    property BackgroundOptions: TFCBackgroundOptions read FBackgroundOptions write SetBackgroundOptions;
    property DestroyTransitions: Boolean read FDestroyTransitions write FDestroyTransitions default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property HideAnimation: TTEFormAnimation read FHideAnimation write SetHideAnimation default nil;
    property HideTransition: TTransitionEffect read FHideTransition write SetHideTransition default nil;
    property HideTransReversed: Boolean read FHideTransReversed write FHideTransReversed default True;
    property ShowAnimation: TTEFormAnimation read FShowAnimation write SetShowAnimation default nil;
    property ShowTransition: TTransitionEffect read FShowTransition write SetShowTransition default nil;
    property Version: String read GetVersion write SetVersion stored False;
    property OnAfterShow: TTEFormTransitionsOnAfterShowEvent read FOnAfterShow write FOnAfterShow;
  end;

var
  TENoFormTransitionsInAero: Boolean;

implementation

uses FlatSB, teRender{$ifdef D6UP}, Types{$endif D6UP};

resourcestring
  rsOnlyOne      = 'Only one FormTransitions component is allowed per form';
  rsEmbeddedForm = 'A FormTransitions component is not necessary in a TFCEmbeddedForm';

const
  WS_EX_LAYERED = $00080000;
type
  TTEWinControl          = class(TWinControl);
  TTEFormAnimationHack   = class(TTEFormAnimation);
  TTransitionEffectHack  = class(TTransitionEffect);
  TTEVCLScreenTrDeviceHack = class(TTEVCLScreenTrDevice);
var
  SaveMDIClientWndProc: Pointer = nil;
  MDIClientLocked     : Boolean = False;
  NestedFormTransition: Boolean = False;
  LockMDIClient       : Boolean = False;
  MDIClientBkOptions  : TFCBackgroundOptions = nil;
  MDIClientHookCount  : Integer = 0;

procedure LockWindow(Form: TCustomForm; Window: HWnd;
  CheckRegion, MDIForm: Boolean; var Data: TTELockData);
begin
  Data.UseRegion :=
    MDIForm                    or
    (TEWinVersion < teWin2000) or
    (
      Assigned(Form)              and
      (Form.BorderStyle = bsNone) and
      (
        (
          (Form.Left = 0) and
          (Form.Top  = 0)
         ) or
        (Form.WindowState = wsMaximized)
      )
    );

  CheckRegion := CheckRegion and (not MDIForm);

  if CheckRegion then
  begin
    Data.Region := CreateRectRgn(0, 0, 0, 0);
    if GetWindowRgn(Window, Data.Region) = ERROR then
    begin
      DeleteObject(Data.Region);
      Data.Region := 0;
    end;
  end;

  if Data.UseRegion
  then SetWindowRgn(Window, CreateRectRgn(0, 0, 0, 0), False)
  else
  begin
    if IsWindowLayered(Window)
    then
    begin
      GetLayeredWindowAttributes(Window, Data.Key, Data.Alpha, Data.Flags);
      if(Data.Flags and LWA_ALPHA) = 0 then
        Data.Alpha := 255;
    end
    else
    begin
      Data.Key   := 0;
      Data.Alpha := 255;
      Data.Flags := 0;
      SetWindowLong(Window, GWL_EXSTYLE, GetWindowLong(Window, GWL_EXSTYLE) or WS_EX_LAYERED);
    end;
    teRender.SetLayeredWindowAttributes(Window, Data.Key, 0, Data.Flags or LWA_ALPHA);
  end;
  Data.Locked := True;
end;

function UnlockWindow(Window: HWnd; Rgn: HRGN; CheckRegion: Boolean;
  var Data: TTELockData): HRGN;
var
  OSRgn,
  RgnCopy: HRGN;
begin
  Result := 0;
  if Data.Locked then
  begin
    if Data.UseRegion
    then
    begin
      SetWindowRgn(Window, 0, IsCompositionEnabled);
      if Rgn <> 0 then
      begin
        Result := Rgn;
        OSRgn := CreateRectRgn(0, 0, 0, 0);
        try
          GetWindowRgn(Window, OSRgn);
          if not EqualRgn(OSRgn, Rgn) then // check if it is a XP theme region
          begin
            RgnCopy := CreateRectRgn(0, 0, 0, 0);
            CombineRgn(RgnCopy, Rgn, 0, RGN_COPY);
            SetWindowRgn(Window, RgnCopy, False);
          end;
        finally
          DeleteObject(OSRgn);
        end;
      end;

      if CheckRegion and (Result = 0) then
      begin
        SendMessage(Window, WM_NCPAINT, 0, 0); // Fix a problem with XP themes

        Result := CreateRectRgn(0, 0, 0, 0);
        if GetWindowRgn(Window, Result) = ERROR then
        begin
          DeleteObject(Result);
          Result := 0;
        end;
      end;
    end
    else
    begin
      if Data.Flags = 0
      then SetWindowLong(Window, GWL_EXSTYLE,
             GetWindowLong(Window, GWL_EXSTYLE) and not WS_EX_LAYERED)
      else teRender.SetLayeredWindowAttributes(Window, Data.Key, Data.Alpha, Data.Flags);

      Result := CreateRectRgn(0, 0, 0, 0);
      if GetWindowRgn(Window, Result) = ERROR then
      begin
        DeleteObject(Result);
        Result := 0;
      end;
    end;
    Data.Locked := False;
  end;
end;

function MaximizedChildren: Boolean;
var
  I: Integer;
begin
  Result := False;

  if Application.MainForm = nil then
    Exit;

  for I := 0 to Application.MainForm.MDIChildCount - 1 do
    if Application.MainForm.MDIChildren[I].WindowState = wsMaximized then
    begin
      Result := True;
      Exit;
    end;
end;

function MDIClientWndProc(Wnd: HWND;
  Msg, WParam, LParam: Longint): Longint; stdcall;

  function CallDefWndProc: Longint;
  begin
    Result := CallWindowProc(SaveMDIClientWndProc, Wnd, Msg, WParam, LParam);
  end;

var
  DC: HDC;
  PS: TPaintStruct;
  R: TRect;
  LockData: TTELockData;
begin
  Result := 0;
  case Msg of
    WM_MDICREATE:
    begin
      try
        if(Application.MainForm <> nil)                and
          IsWindowVisible(Application.MainForm.Handle) and
          (not NestedFormTransition)                   and
          (not TEVclScrPrepared)                       and
          LockMDIClient
        then
        begin
          LockWindow(nil, Application.MainForm.ClientHandle, False, True, LockData);
          MDIClientLocked := True;
        end
        else MDIClientLocked := False;
      finally
        LockMDIClient := False;
      end;
      Result := CallDefWndProc;
    end;
    WM_MDIDESTROY:
    begin
      if not(csDestroying in Application.ComponentState) then
        SendMessage(WParam, CM_MDIDESTROY, WParam, LParam);
      Result := CallDefWndProc;
    end;
    WM_ERASEBKGND:
    begin
      if Assigned(MDIClientBkOptions) and MDIClientBkOptions.IsActive
      then Result := 1
      else Result := CallDefWndProc;
    end;
    WM_PAINT:
    begin
      if Assigned(MDIClientBkOptions) and MDIClientBkOptions.IsActive
      then
      begin
        DC := WParam;
        if DC = 0
        then
        begin
          DC := BeginPaint(Application.MainForm.ClientHandle, PS);
          try
            if IsRectEmpty(PS.rcPaint) then
              GetClientRect(Application.MainForm.ClientHandle, PS.rcPaint);
            MDIClientBkOptions.DrawBackGround(DC, nil, PS.rcPaint);
          finally
            if WParam = 0 then
              EndPaint(Application.MainForm.ClientHandle, PS);
          end;
        end
        else
        begin
          GetClientRect(Application.MainForm.ClientHandle, R);
          MDIClientBkOptions.DrawBackGround(DC, nil, R);
        end;
      end
      else Result := CallDefWndProc;
    end;
    WM_SIZE,
    WM_VSCROLL,
    WM_HSCROLL:
    begin
      Result := CallDefWndProc;
      if Assigned(MDIClientBkOptions)                                    and
        (not(csDestroying in MDIClientBkOptions.Control.ComponentState)) and
         MDIClientBkOptions.IsActive                                     and
        (MDIClientBkOptions.PictureMode <> fcpmTile)                     then
        InvalidateRect(Application.MainForm.ClientHandle, nil, True);
    end

    else Result := CallDefWndProc;
  end;
end;

procedure MDIClientWndProcSubclass(ClientHandle: HWND);
begin
  Assert(SaveMDIClientWndProc = nil);

  SaveMDIClientWndProc :=
    Pointer(GetWindowLong(ClientHandle, GWL_WNDPROC));
  SetWindowLong(ClientHandle, GWL_WNDPROC,
    Longint(@MDIClientWndProc));
end;

procedure RestoreMDIClientWndProc(ClientHandle: HWND);
begin
  SetWindowLong(ClientHandle, GWL_WNDPROC,
    Longint(@SaveMDIClientWndProc));
  SaveMDIClientWndProc := nil;
end;

{ TFormTransitions }

constructor TFormTransitions.Create(AOwner: TComponent);
var
  i: Integer;
begin
  if Assigned(AOwner) then
  begin
    if AOwner is TFCEmbeddedForm then
      raise Exception.Create(rsEmbeddedForm);

    for i := 0 to AOwner.ComponentCount - 1 do
      if AOwner.Components[i] is TFormTransitions then
        raise Exception.Create(rsOnlyOne);
  end;

  inherited Create(AOwner);

  FBackgroundOptions  := TFCBackgroundOptions.Create;
  FDestroyTransitions := True;
  FEnabled            := True;
  FHideTransReversed  := True;
  FirstTimeShowed     := True;
  FHideAnimation      := nil;
  FHideTransition     := nil;
  FShowAnimation      := nil;
  FShowTransition     := nil;
  FAnimationData      := nil;
  Device              := nil;

  if Assigned(AOwner) and (AOwner is TCustomForm)
  then
  begin
    FOwnerForm := TTECustomForm(AOwner);
    ActivateHookForm(True);
    if OwnerForm.FormStyle = fsMDIChild then
      ActivateHookMDIClientTrans(True);

    if csDesigning in Componentstate then
      BackgroundOptions.Control := OwnerForm;
  end
  else FOwnerForm := nil;
end;

destructor TFormTransitions.Destroy;
begin
  ActivateHookForm(False);

  if DestroyTransitions then
  begin
    if Assigned(FHideAnimation) then
      FHideAnimation.Free;
    if Assigned(FHideTransition) then
      FHideTransition.Free;
    if Assigned(FShowAnimation) then
      FShowAnimation.Free;
    if Assigned(FShowTransition) then
      FShowTransition.Free;
  end;

  FBackgroundOptions.Free;
  FAnimationData    .Free;
  Device            .Free;

  inherited;
end;

procedure TFormTransitions.ActivateHookForm(const Activate: Boolean);
begin
  if Activate
  then
  begin
    if(not Assigned(WindowProcBak)) then
    begin
      WasVisible      := OwnerForm.Visible;
      IsAppMinimizing := False;
      WindowProcBak   := OwnerForm.WindowProc;
      OwnerForm.WindowProc := NewWindowProc;
      Application.HookMainWindow(MainWndHook);
    end;
  end
  else
  begin
    if Assigned(Owner) and Assigned(WindowProcBak) then
      OwnerForm.WindowProc := WindowProcBak;
    WindowProcBak := nil;
    Application.UnhookMainWindow(MainWndHook);
  end;
end;

procedure TFormTransitions.ActivateHookMDIClient(const Activate: Boolean);
var
  MainForm: TTECustomForm;
begin
  if csDesigning in OwnerForm.ComponentState then
    exit;

  if OwnerForm.FormStyle = fsMDIForm
  then MainForm := OwnerForm
  else MainForm := TTECustomForm(Application.MainForm);

  if Activate
  then
  begin
    if MDIClientHookCount = 0 then
      MDIClientWndProcSubclass(MainForm.ClientHandle);
    Inc(MDIClientHookCount);
  end
  else
  begin
    Dec(MDIClientHookCount);
    if MDIClientHookCount = 0 then
    begin
      if not(csDestroying in MainForm.ComponentState) then
        RestoreMDIClientWndProc(MainForm.ClientHandle);
    end;
  end;
end;

procedure TFormTransitions.ActivateHookMDIClientTrans(const Activate: Boolean);
begin
  if Activate then
  begin
    if OwnerForm.FormStyle = fsMdiChild then
      LockMDIClient :=
        Enabled and
        ((OwnerForm.WindowState = wsMaximized) or MaximizedChildren);
  end;
  ActivateHookMDIClient(Activate);
end;

procedure TFormTransitions.ActivateHookMDIClientBkgrnd(const Activate: Boolean);
begin
  if Activate
  then MDIClientBkOptions := BackgroundOptions
  else MDIClientBkOptions := nil;
  ActivateHookMDIClient(Activate);
end;

function TFormTransitions.CanEnable: Boolean;
begin
  Result := (not(csDesigning in ComponentState)) and (OwnerForm.Parent = nil);
end;

procedure TFormTransitions.Loaded;
begin
  inherited Loaded;

  if(OwnerForm.FormStyle = fsMDIChild) and MDIClientLocked then
  begin
    LockData.Locked    := True;
    LockData.UseRegion := True;
    LockData.Region    := 0;
  end;

  if OwnerForm.FormStyle = fsMDIForm then
    ActivateHookMDIClientBkgrnd(True);

  if Enabled and CanEnable
  then
  begin
    if OwnerForm.FormStyle = fsStayOnTop then
    begin
      OwnerForm.FormStyle := fsNormal;
      OwnerForm.FormStyle := fsStayOnTop;
    end;
  end
  else
  begin
    if(OwnerForm.FormStyle = fsMDIChild) and MDIClientLocked then
    begin
      UnlockWindow(Application.MainForm.ClientHandle, 0, True, LockData);
      MDIClientLocked := False;
    end;
  end;
end;

procedure TFormTransitions.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if(AComponent = FHideTransition) and
      Assigned(FHideTransition)      then
    begin
      if(FHideTransition = OldTransition) and Assigned(NewTransition)
      then FHideTransition := NewTransition
      else FHideTransition := nil;
    end;
    if(AComponent = FShowTransition) and
      Assigned(FShowTransition)      then
    begin
      if(FShowTransition = OldTransition) and Assigned(NewTransition)
      then FShowTransition := NewTransition
      else FShowTransition := nil;
    end;
    if(FAnimationData <> nil) and
      (
        (AComponent = FAnimationData.Control  ) or
        (AComponent = FAnimationData.Animation)
      ) then
      FreeAndNil(FAnimationData);
    if AComponent = FHideAnimation then
      FHideAnimation := nil;
    if AComponent = FShowAnimation then
      FShowAnimation := nil;
  end;
end;

function TFormTransitions.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

procedure TFormTransitions.SetVersion(const Value: String);
begin
end;

var
  SaveSetWindowRgn: function(hWnd: HWND; hRgn: HRGN; bRedraw: BOOL): Integer; stdcall;
  HookedWnd: HWnd;
  LockedRgn: HRGN;

function HookedSetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: BOOL): Integer; stdcall;
begin
  if hWnd = HookedWnd
  then
  begin
    if LockedRgn <> 0 then
      DeleteObject(LockedRgn);
    LockedRgn := hRgn;
    Result    := 1;
  end
  else Result := SaveSetWindowRgn(hWnd, hRgn, bRedraw);
end;

function TFormTransitions.MainWndHook(var Message: TMessage): Boolean;
begin
  if(Message.Msg = WM_SYSCOMMAND) and (Message.WParam = SC_MINIMIZE) then
    IsAppMinimizing := True;
  Result := False;
end;

procedure TFormTransitions.NewWindowProc(var Message: TMessage);

  procedure PrepareShowingEffects;
  begin
    if(OwnerForm.FormStyle <> fsMDIChild) and IsCompositionEnabled then
    begin
      if(not TENoFormTransitionsInAero) and
        (
           Assigned(FShowTransition) or
           Assigned(FShowAnimation ) or
           Assigned(FHideTransition) or
           Assigned(FHideAnimation ) or
           Assigned(FAnimationData )
        )
      then DisableDwmTransitions(OwnerForm.Handle)
      else
      begin
        WindowProcBak(Message);
        exit;
      end;
    end;

    ShowEffectWaiting  := False;
    LockedRgn          := 0;
    HookedWnd          := 0;
    LockData.UseRegion := True;

    if(OwnerForm.FormStyle = fsMDIChild) and MDIClientLocked
    then
    begin
      if MaximizedChildren then
      begin
        SetWindowLong(Application.MainForm.ClientHandle, GWL_EXSTYLE,
          GetWindowLong(Application.MainForm.ClientHandle,
            GWL_EXSTYLE) and not WS_EX_CLIENTEDGE);
        SetWindowPos(Application.MainForm.ClientHandle, 0, 0, 0, 0, 0,
          SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
      end;
    end
    else
    begin
      LockWindow(OwnerForm, OwnerForm.Handle, OwnerForm.WindowState <> wsMaximized,
        TTECustomForm(OwnerForm).FormStyle = fsMDIChild, LockData);
      if LockData.UseRegion then
        LockedRgn := LockData.Region;
    end;
    try
      with OwnerForm do
      begin
        if(FormStyle = fsStayOnTop) and
          (
            (
              (
                HorzScrollBar.Visible and
                (HorzScrollBar.Style = ssRegular)
              ) and
              not
                (
                  VertScrollBar.Visible and
                  (VertScrollBar.Style <> ssRegular)
                )
            ) or
            (
              (
                VertScrollBar.Visible and
                (VertScrollBar.Style = ssRegular)
              ) and
              not
                (
                  HorzScrollBar.Visible and
                  (HorzScrollBar.Style <> ssRegular)
                )
            )
          ) then
          UninitializeFlatSB(Handle);
      end;

      if OwnerForm.FormStyle = fsMDIForm then
        NestedFormTransition := True;
      try
        TEVclScrPrepared := True;
        try
          SaveSetWindowRgn := nil;
          if LockData.UseRegion then
          begin
            HookedWnd := OwnerForm.Handle;
            HookAPICall('user32', 'SetWindowRgn', @Windows.SetWindowRgn,
              @HookedSetWindowRgn, @SaveSetWindowRgn, True);
          end;
          try
            WindowProcBak(Message);
            ShowEffectWaiting :=
              SendMessage(OwnerForm.Handle, CM_BEWAIT, 0, BE_ID) = BE_ID;
          finally
            if(not ShowEffectWaiting) and LockData.UseRegion then
              UnhookAPICall(@Windows.SetWindowRgn, @HookedSetWindowRgn,
                @SaveSetWindowRgn, True);
          end;
          if not(ShowEffectWaiting or IsWindowVisible(OwnerForm.Handle)) then
            Abort;
        finally
          TEVclScrPrepared := False;
        end;
      finally
        if OwnerForm.FormStyle = fsMDIForm then
          NestedFormTransition := False;
      end;
    except
      on E: Exception do
      begin
        if LockData.Locked then
        begin
          if(OwnerForm.FormStyle = fsMDIChild) and MDIClientLocked
          then
          begin
            UnlockWindow(Application.MainForm.ClientHandle, 0, True, LockData);
            MDIClientLocked := False;
          end
          else UnlockWindow(OwnerForm.Handle, LockedRgn, False, LockData);
        end;
        if not(E is EAbort) then
          raise;
      end;
    end;
  end;

  procedure ShowEffects;
  var
    SaveNeverRendering,
    CanDestroyAnimationData: Boolean;
    R: TRect;
    TransitionToUse: TTransitionEffect;
    AnimationDataToUse: TTEFormAnimationData;
    Device: TTEVCLScreenTrDeviceHack;

    procedure DoUnlock;
    begin
      if LockData.Locked then
      begin
        if(OwnerForm.FormStyle = fsMDIChild) and MDIClientLocked
        then
        begin
          UnlockWindow(Application.MainForm.ClientHandle, 0, True, LockData);
          MDIClientLocked := False;
        end
        else LockedRgn := UnlockWindow(OwnerForm.Handle, LockedRgn,
                            OwnerForm.FormStyle = fsMDIChild, LockData);
        if(LockedRgn <> 0) and (not MDIClientLocked) then
        begin
          if(OwnerForm.FormStyle = fsMDIChild) and
            (TTEVCLScreenTrDeviceHack(Device).ClipRgn <> 0)
          then CombineRgn(
                 TTEVCLScreenTrDeviceHack(Device).ClipRgn,
                 TTEVCLScreenTrDeviceHack(Device).ClipRgn,
                 LockedRgn,
                 RGN_AND)
          else TTEVCLScreenTrDeviceHack(Device).ClipRgn := LockedRgn;
        end;
      end;
    end;

  var
    UsingLayerTransition,
    CompositedForm,
    FlickerFreeAero: Boolean;
  begin
    Application.ProcessMessages; // Allow control to repaint
    UnhookAPICall(@Windows.SetWindowRgn, @HookedSetWindowRgn, @SaveSetWindowRgn,
      True);
    AnimationDataToUse := nil;
    SaveNeverRendering := False;
    if FAnimationData <> nil
    then AnimationDataToUse := FAnimationData
    else if(FShowAnimation <> nil) and FShowAnimation.Enabled then
      AnimationDataToUse := FShowAnimation.CreateAnimationData(OwnerForm);
    try
      TransitionToUse := nil;
      CompositedForm  :=
        (OwnerForm.FormStyle <> fsMDIChild) and IsCompositionEnabled;
      FlickerFreeAero :=
        CompositedForm and (FShowTransition is TFlickerFreeTransition);
      if(FShowTransition <> nil) and
        (not(CompositedForm and TENoFormTransitionsInAero))                         and
        (not TTEVCLScreenTrDeviceHack.TransitionIsDisabled(FShowTransition, False)) and
        (not FlickerFreeAero)                                                       and
        (
          (not IsWindowLayered(OwnerForm.Handle))      or
          (LockData.UseRegion or (LockData.Flags = 0)) or
          CompositedForm
        )
      then TransitionToUse := FShowTransition
      else
      begin
        if FlickerFreeAero or
          (
            LockData.UseRegion or
            (
              Assigned(AnimationDataToUse) or
              (
                FlickerFreeTransition.FlickerFreeWhenDisabled and
                ( not
                  (
                    CompositedForm and
                    (FShowTransition = nil)
                  )
                )
              )
            )
          ) then
        begin
          TransitionToUse := TFlickerFreeTransition.Create(nil);
          TransitionToUse.FlickerFreeWhenDisabled := True;
          TFlickerFreeTransition(TransitionToUse).Fake :=
            FlickerFreeAero or
            Assigned(AnimationDataToUse);
          if FShowTransition <> nil then
          begin
            // OnStart, OnEnd events are not fired because they are only related to the original transition
            TransitionToUse.OnAfterTransition  := FShowTransition.OnAfterTransition;
            TransitionToUse.OnBeforeTransition := FShowTransition.OnBeforeTransition;
          end;
        end;
      end;
      try
        if OwnerForm.Visible then
        begin
          Device := TTEVCLScreenTrDeviceHack(TTEVCLScreenTrDevice.Create);
          try
            Device.LayerAlpha := LockData.Alpha;
            Device.LayerKey   := LockData.Key;
            Device.LayerFlags := LockData.Flags;
            try
              Device.Transition := TransitionToUse;
            except
              FreeAndNil(Device);
              raise;
            end;

            try
              if Assigned(TransitionToUse) then
                SaveNeverRendering := TransitionToUse.NeverRendering;

              if OwnerForm.FormStyle = fsMDIChild
              then
              begin
                if MaximizedChildren
                then
                begin
                  GetClientRect(Application.MainForm.ClientHandle, R);
                  Device.ClientCoordinates := True;
                end
                else
                begin
                  R := Rect(0, 0, OwnerForm.Width, OwnerForm.Height);
                  Device.ClientCoordinates := False;
                end;
              end
              else
              begin
                Device.ClientCoordinates := False;
                R := OwnerForm.BoundsRect;
              end;
{
              if(OwnerForm.FormStyle = fsMDIChild) and MaximizedChildren
              then
              begin
                GetClientRect(Application.MainForm.ClientHandle, R);
                Device.ClientCoordinates := True;
              end
              else
              begin
                Device.ClientCoordinates := False;
                R := OwnerForm.BoundsRect;
              end;
}
              UsingLayerTransition := False;
              try
                if Assigned(TransitionToUse) then
                begin
                  TransitionToUse.NeverRendering := OwnerForm.FormStyle <> fsMDIChild;

                  TTEVCLScreenTrDeviceHack(Device).OpeningForm := True;
                  try
                    Device.Prepare(OwnerForm, R);
                  finally
                    TTEVCLScreenTrDeviceHack(Device).OpeningForm := False;
                  end;
                end;

                if LockData.UseRegion
                then DoUnlock
                else
                begin
                  TTEVCLScreenTrDeviceHack(Device).ClipRgn := LockData.Region;
                  UsingLayerTransition :=
                    Assigned(Device.DelegateTransition) and
                    Device.DelegateTransition.ClassNameIs('TLayeredBlendTransition');
                end;

                if Device.Prepared or (not LockData.UseRegion) then
                begin
                  if(AnimationDataToUse <> nil) and AnimationDataToUse.Animation.Enabled then
                  begin
                    TTEFormAnimationHack(AnimationDataToUse.Animation).ExecuteShowing(
                      not(TransitionToUse is TFlickerFreeTransition),
                      AnimationDataToUse, CanDestroyAnimationData);
                    if(not CanDestroyAnimationData) and (FAnimationData = nil) then
                      FAnimationData := AnimationDataToUse;
                  end;
                end;

                if not UsingLayerTransition then
                  DoUnlock;

                // Although the transition could be disabled, we want to fire
                // its OnBeforeTransition and OnAfterTransition events
                if Assigned(TransitionToUse) then
                begin
                  Device.Execute;

                  // Fixes a problem with VCLSkin
                  SendMessage(OwnerForm.Handle, WM_NCPAINT, 0, 0);
                end;
              finally
                if UsingLayerTransition then
                  DoUnlock;
                if Assigned(TransitionToUse) then
                  TransitionToUse.NeverRendering := SaveNeverRendering;
              end;
            finally
              Device.UnPrepare;
            end;
          finally
            FreeAndNil(Device);
          end;
        end;
      finally
        if FShowTransition = nil then
          TransitionToUse.Free;
      end;
    finally
      if(AnimationDataToUse <> nil)            and
        (AnimationDataToUse <> FAnimationData) then
        AnimationDataToUse.Free;
      ShowEffectWaiting := False;
    end;
  end;

  procedure HideEffects(ExecuteTransition, ExecuteAnimation: Boolean);
  var
    AnimationDataToUse: TTEFormAnimationData;
  begin
    if ExecuteTransition       and
      (FHideTransition <> nil) and
      Assigned(Device)         and
      Device.Prepared
    then
    begin
      try
        WindowProcBak(Message);
        try
          Device.Execute;
        finally
          if FHideTransReversed then
            FHideTransition.Reversed := not FHideTransition.Reversed;
        end;
      finally
        Device.UnPrepare;
      end;
    end
    else WindowProcBak(Message);

    if ExecuteAnimation then
    begin
      AnimationDataToUse := nil;
      if FAnimationData <> nil
      then AnimationDataToUse := FAnimationData
      else if(FHideAnimation <> nil) and FHideAnimation.Enabled and FHideAnimation.HidingEnabled then
        AnimationDataToUse := FHideAnimation.CreateAnimationData(OwnerForm);
      try
        if(AnimationDataToUse <> nil)                 and
           AnimationDataToUse.Animation.Enabled       and
           AnimationDataToUse.Animation.HidingEnabled then
        begin
          // Allow destop to repaint
          Application.ProcessMessages;
          Sleep(100);

          TTEFormAnimationHack(AnimationDataToUse.Animation).ExecuteHiding(
            AnimationDataToUse);
        end;
      finally
        if AnimationDataToUse <> nil then
          AnimationDataToUse.Free;
        FAnimationData := nil; // No more needed in any case
      end;
    end;
  end;

  function LayeredWindowsUnder(Window: HWND): Boolean;
  var
    R,
    R2,
    R3: TRect;
  begin
    Result := False;

    if Window = 0 then Exit;

    GetWindowRect(Window, R);

    // Check if covered by top level windows 'over' in the z-order
    while(Window <> 0) and not Result do
    begin
      Window := GetWindow(Window, GW_HWNDNEXT);
      if(Window <> 0)           and
        IsWindowVisible(Window) and
        (not IsIconic(Window))  and
        IsWindowLayered(Window) then
      begin
        GetWindowRect(Window, R2);
        Result := IntersectRect(R3, R, R2);
      end;
    end;
  end;

  procedure PrepareHidingTransition;
  var
    R: TRect;
    CompositedForm: Boolean;
  begin
    CompositedForm :=
      (OwnerForm.FormStyle <> fsMDIChild) and IsCompositionEnabled;
    if(not(csDesigning in Componentstate))                                        and
      (FHideTransition <> nil)                                                    and
      (not(CompositedForm and TENoFormTransitionsInAero))                         and
      (not TTEVCLScreenTrDeviceHack.TransitionIsDisabled(FHideTransition, False)) and
      (
        (OwnerForm.FormStyle = fsMDIChild) or
        (
          (TEWinVersion >= teWin2000) and
          (
            (not IsWindowLayered(OwnerForm.Handle)) or
            IsCompositionEnabled
          )
        )
      ) then
    begin
      Device := TTEVCLScreenTrDeviceHack(TTEVCLScreenTrDevice.Create);
      try
        with TTEVCLScreenTrDeviceHack(Device) do
          Transition := FHideTransition;
        if FHideTransReversed then
          FHideTransition.Reversed := not FHideTransition.Reversed;
      except
        FreeAndNil(Device);
        raise;
      end;
      TTEVCLScreenTrDeviceHack(Device).ClosingForm := True;

      if(OwnerForm.FormStyle = fsMDIChild) //and (not MaximizedChildren)
      then
      begin
        if MaximizedChildren
        then
        begin
          Device.ClientCoordinates := True;
          GetClientRect(Application.MainForm.ClientHandle, R);
          Device.Prepare(OwnerForm, R);
        end
        else
        begin
          Device.ClientCoordinates := False;
          Device.Prepare(OwnerForm, Rect(0, 0, OwnerForm.Width, OwnerForm.Height));
        end;
      end
      else
      begin
        Device.ClientCoordinates := False;
        if IsCompositionEnabled
        then
        begin
          try
            if IsWindowLayered(OwnerForm.Handle)
            then
            begin
              GetLayeredWindowAttributes(OwnerForm.Handle, LockData.Key,
                LockData.Alpha, LockData.Flags);
              if(LockData.Flags and LWA_ALPHA) = 0 then
                LockData.Alpha := 255;
            end
            else
            begin
              LockData.Key   := 0;
              LockData.Alpha := 255;
              LockData.Flags := 0;
              SetWindowLong(OwnerForm.Handle, GWL_EXSTYLE,
                GetWindowLong(OwnerForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
              OwnerForm.Update;
            end;
            with TTEVCLScreenTrDeviceHack(Device) do
            begin
              LayerAlpha := LockData.Alpha;
              LayerKey   := LockData.Key;
              LayerFlags := LockData.Flags;
            end;
            Device.Prepare(OwnerForm, OwnerForm.BoundsRect);
            try
              HideEffects(True, False);
            finally
              teRender.SetLayeredWindowAttributes(OwnerForm.Handle, LockData.Key,
                LockData.Alpha, LockData.Flags or LWA_ALPHA);
            end;
          finally
            FreeAndNil(Device);
          end;
        end
        else Device.Prepare(OwnerForm, OwnerForm.BoundsRect);
      end;
    end;
  end;

var
  R: TRect;
  Rgn: HRGN;
  DC: HDC;
  PS: TPaintStruct;
  WasVisibleBak: Boolean;
begin
  case Message.Msg of
    CM_MDIDESTROY:
    begin
      if Enabled and (Device = nil) then
      begin
        try
          PrepareHidingTransition;
          if Assigned(Device) then
          begin
            SetWindowRgn(OwnerForm.Handle, CreateRectRgn(0, 0, 0, 0), False);
            SendMessage(Application.MainForm.ClientHandle, WM_MDINEXT, 0, 0);
            HideEffects(Assigned(Device), True);
          end;
        finally
          FreeAndNil(Device);
        end;
      end;
    end;
    WM_WINDOWPOSCHANGING:
    begin
      if((TWMWindowPosMsg(Message).WindowPos.Flags and SWP_HIDEWINDOW) <> 0) and
        Enabled                                                              and
        (Device = nil)                                                       and
        (not IsAppMinimizing)                                                and
        (not(csDesigning in ComponentState))                                 and
        (not(OwnerForm.FormStyle = fsMDIChild))                              then
        PrepareHidingTransition;
      WindowProcBak(Message);
    end;
    WM_WINDOWPOSCHANGED:
    begin
      if((TWMWindowPosMsg(Message).WindowPos.Flags and SWP_HIDEWINDOW) <> 0) and
        Enabled                                                              and
        Assigned(Device)                                                     and
        (not(csDesigning in ComponentState))                                 and
        (not((OwnerForm.FormStyle = fsMDIChild) and (OwnerForm.WindowState = wsMaximized)))
      then
      begin
        try
          HideEffects(Assigned(Device), True);
        finally
          FreeAndNil(Device);
        end;
      end
      else WindowProcBak(Message);
      WasVisible := OwnerForm.Visible;
      IsAppMinimizing := False;
    end;
    CM_SHOWINGCHANGED:
    begin
      WasVisibleBak := WasVisible;
      if not WasVisible then
        BackgroundOptions.Control := OwnerForm;

      if(not(csDesigning in Componentstate)) and
         Enabled                             and
         CanEnable                           and
        (not NestedFormTransition)           and
        (not WasVisible)                     and
        (not TEVclScrPrepared)
      then
      begin
        PrepareShowingEffects;
        if not ShowEffectWaiting then
        begin
          ShowEffects;
          PostMessage(OwnerForm.Handle, CM_BEFORMSHOWN, 0, BE_ID);
        end;
      end
      else
      begin
        WindowProcBak(Message);
        if(not(csDesigning in Componentstate)) and
          (not WasVisibleBak)                  then
        begin
          if Assigned(ShowTransition) then
          begin
            if Assigned(ShowTransition.OnBeforeTransition) then
              ShowTransition.OnBeforeTransition(Self);
            if Assigned(ShowTransition.OnAfterTransition) then
              ShowTransition.OnAfterTransition(Self);
          end;

          PostMessage(OwnerForm.Handle, CM_BEFORMSHOWN, 0, BE_ID);
        end;
      end;
    end;

    CM_BEFORMSHOWN:
    begin
      if Message.LParam = BE_ID
      then
      begin
        if Assigned(FOnAfterShow) then
          FOnAfterShow(Self, FirstTimeShowed);
        FirstTimeShowed := False;
      end;
    end;

    CM_BERUN:
    begin
      // If message comes from Billenium Effects
      if Message.LParam = BE_ID
      then
      begin
        if ShowEffectWaiting then
        begin
          ShowEffects;
          PostMessage(OwnerForm.Handle, CM_BEFORMSHOWN, 0, BE_ID);
        end;
      end
      else WindowProcBak(Message);
    end;

    CM_BEWAIT:
    begin
      // If message goes to Billenium Effects
      if Message.WParam = BE_ID
      then
      begin
        if(not(csDesigning in Componentstate)) and
           Enabled                             and
           CanEnable
        then Message.Result := BE_ID // Hi, Billenium Effects here
        else WindowProcBak(Message);
      end
      else WindowProcBak(Message);
    end;

    CM_PARENTFONTCHANGED:
    begin
      WindowProcBak(Message);
      BackgroundOptions.Control := OwnerForm;
    end;

    CM_TEGETBKGNDOPTIONS:
      Message.Result := Longint(BackgroundOptions);

    WM_ERASEBKGND:
    begin
      if MDIClientLocked then
      begin
        Message.Result := 1;
        exit;
      end;
      if(OwnerForm.FormStyle <> fsMDIForm) and BackgroundOptions.IsActive
      then
      begin
        {$ifdef D7UP}
        if BEParentBackgroundPainted(OwnerForm.Handle) then
          BackgroundOptions.DrawBackGround(TWmEraseBkgnd(Message).DC, nil,
            Rect(0, 0, 0, 0));
        {$endif D7UP}
        Message.Result := 1;
      end
      else WindowProcBak(Message);
    end;

    WM_PAINT:
    begin
      if(OwnerForm.FormStyle <> fsMDIForm) and BackgroundOptions.IsActive
      then
      begin
        DC := TWMPaint(Message).DC;
        if DC = 0
        then
        begin
          DC := BeginPaint((Owner as TWinControl).Handle, PS);
          try
            if IsRectEmpty(PS.rcPaint) then
              PS.rcPaint := (Owner as TWinControl).ClientRect;

            BackgroundOptions.DrawBackGround(DC, nil, PS.rcPaint);
            if not(csDesigning in OwnerForm.ComponentState) then
              OwnerForm.Paint;
            TTEWinControl(Owner).PaintControls(DC, nil);
          finally
            if TWMPaint(Message).DC = 0 then
              EndPaint((Owner as TWinControl).Handle, PS);
          end;
        end
        else
        begin
          Rgn := CreateRectRgn(0, 0, 0, 0);
          GetClipRgn(DC, Rgn);
          GetRgnBox(Rgn, R);
          DeleteObject(Rgn);
          DPToLP(DC, R, 2);

          BackgroundOptions.DrawBackGround(DC, nil, R);
          if not(csDesigning in OwnerForm.ComponentState) then
            OwnerForm.Paint;
          TTEWinControl(Owner).PaintControls(DC, nil);
        end;
      end
      else WindowProcBak(Message);
    end;

    WM_MOVE:
    begin
      WindowProcBak(Message);
      if(not(csDestroying in ComponentState)) and
        BackgroundOptions.IsActive            and
        (BackgroundOptions.ParentPicture      or
         BackgroundOptions.ParentBkgrndForm   or
         not BackgroundOptions.Opaque) then
        BackgroundOptions.ControlChanged(Self);
    end;

    WM_SIZE,
    WM_VSCROLL,
    WM_HSCROLL:
    begin
      WindowProcBak(Message);
      if(not(csDestroying in ComponentState)) and
        BackgroundOptions.IsActive then
        BackgroundOptions.ControlChanged(Self);
    end;

    else WindowProcBak(Message);
  end;
{if ShowEffectWaiting and IsWindowVisible(OwnerForm.Handle) then
begin
  ShowEffectWaiting := False;
  ShowEffects;
end;}
end;

function TFormTransitions.GetPalette: HPalette;
begin
  Result := BackgroundOptions.GetPalette;
end;

procedure TFormTransitions.SetBackgroundOptions(
  Value: TFCBackgroundOptions);
begin
  BackgroundOptions.Assign(Value);
end;

procedure TFormTransitions.PrepareAnimation(
  AnAnimationData: TTEFormAnimationData);
begin
  FAnimationData.Free;
  FAnimationData := AnAnimationData;
end;

procedure TFormTransitions.SetHideAnimation(
  const Value: TTEFormAnimation);
begin
  if Value <> FHideAnimation then
  begin
    if DestroyTransitions                   and
      (not (csDesigning in ComponentState)) and
       Assigned(FHideAnimation)             then
      FHideAnimation.Free;

    FHideAnimation := Value;

    if Assigned(FHideAnimation) and
       Assigned(FHideAnimation.AnimationList) then
      DestroyTransitions := False;
  end;
end;

procedure TFormTransitions.SetHideTransition(
  const Value: TTransitionEffect);
begin
  if Value <> FHideTransition then
  begin
    if DestroyTransitions                   and
      (not (csDesigning in ComponentState)) and
       Assigned(FHideTransition)            then
      FHideTransition.Free;

    FHideTransition := Value;

    if Assigned(FHideTransition) and
       Assigned(FHideTransition.TransitionList) then
      DestroyTransitions := False;
  end;
end;

procedure TFormTransitions.SetShowAnimation(
  const Value: TTEFormAnimation);
begin
  if Value <> FShowAnimation then
  begin
    if DestroyTransitions                   and
      (not (csDesigning in ComponentState)) and
       Assigned(FShowAnimation)             then
      FShowAnimation.Free;

    FShowAnimation := Value;

    if Assigned(FShowAnimation) and
       Assigned(FShowAnimation.AnimationList) then
      DestroyTransitions := False;
  end;
end;

procedure TFormTransitions.SetShowTransition(const Value: TTransitionEffect);
begin
  if Value <> FShowTransition then
  begin
    if DestroyTransitions                   and
      (not (csDesigning in ComponentState)) and
       Assigned(FShowTransition)            then
      FShowTransition.Free;

    FShowTransition := Value;

    if Assigned(FShowTransition) and
       Assigned(FShowTransition.TransitionList) then
      DestroyTransitions := False;
  end;
end;

initialization
  TENoFormTransitionsInAero := False;
end.
