unit teImage;

interface

{$INCLUDE teDefs.inc}

uses
  SysUtils, Classes, teBkgrnd, Windows, Messages, Graphics, Controls, TransEff,
  teVclCtl;

type
  TTEImage = class(TCustomControl)
  private
    FBackgroundOptions: TFCBackgroundOptions;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FPicture: TPicture;
    FPictureMode: TFCPictureMode;
    FPictureTranspColor: TColor;
    FDrawing: Boolean;
    FPictureVisible: Boolean;
    FTransitionDevice: TTEVCLControlTrDevice;
    FPictureMargin: Word;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTEThreadTerminated(var Message: TWMNoParams); message CM_TETHREADTERMINATED;
    procedure WMDestroy(var Msg: TMessage); message WM_DESTROY;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure SetBackgroundOptions(Value: TFCBackgroundOptions);
    function  GetVersion: String;
    procedure SetPicture(const Value: TPicture);
    procedure SetPictureMargin(const Value: Word);
    procedure SetPictureMode(Value: TFCPictureMode);
    procedure SetPictureTranspColor(Value: TColor);
    procedure SetVersion(const Value: String);
    procedure PictureChanged(Sender: TObject);
    procedure BkgrndChanged(Sender: TObject);
    procedure DoPaint(Bmp: TBitmap; R: TRect; DrawPic: Boolean);
    procedure InternalPrepareTransition(Transition: TTransitionEffect;
      R: TRect; SrcBmp: TBitmap);
    procedure InternalUnPrepareTransition(FullUnprepare: Boolean);
    procedure SetPictureVisible(const Value: Boolean);
  protected
    FullAreaTransition,
    FDestroyTransition: Boolean;
    PreparedPicRect: TRect;
    BkgrndHasChanged: Boolean;

    function  DoPaletteChange: Boolean;
    function  GetPalette: HPalette; override;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  PrepareTransition(Transition: TTransitionEffect;
      const FullArea: Boolean = True;
      const DestroyTransition: Boolean = False): Boolean;
    procedure ExecuteTransition(WaitForCompletion: Boolean = True);
    procedure AbortTransition;
    {$ifndef TE_NOHLP}
    procedure UnPrepareTransition;
    {$endif TE_NOHLP}
    function  Transition: TTransitionEffect;
    function  TransitionExecuting: Boolean;
    function  TransitionPrepared: Boolean;

    property TransitionDevice: TTEVCLControlTrDevice read FTransitionDevice;
  published
    property  Anchors;
    property  Align;
    property  BackgroundOptions: TFCBackgroundOptions read FBackgroundOptions write SetBackgroundOptions;
    property  BevelEdges;
    property  BevelInner;
    property  BevelOuter;
    property  BevelKind;
    property  BevelWidth;
    property  BiDiMode;
    property  BorderWidth;
    property  Color nodefault;
    property  Constraints;
    property  Ctl3D;
    property  DragCursor;
    property  DragKind;
    property  DragMode;
    property  Enabled;
    property  Font;
    property  OnClick;
    property  OnDblClick;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEndDock;
    property  OnEnter;
    property  OnExit;
    property  OnMouseDown;
    property  OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property  OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property  OnMouseMove;
    property  OnMouseUp;
    property  OnStartDock;
    property  OnStartDrag;
    property  ParentColor;
    property  ParentCtl3D;
    property  ParentFont;
    property  ParentShowHint;
    property  ParentBiDiMode;
    property  Picture: TPicture read FPicture write SetPicture;
    property  PictureMargin: Word read FPictureMargin write SetPictureMargin default 0;
    property  PictureMode: TFCPictureMode read FPictureMode write SetPictureMode default fcpmTile;
    property  PictureTranspColor: TColor read FPictureTranspColor write SetPictureTranspColor default clNone;
    property  PictureVisible: Boolean read FPictureVisible write SetPictureVisible default True;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  TabStop;
    property  Version: String read GetVersion write SetVersion stored False;
    property  Visible;
    {$ifdef D7UP}
    property  ParentBackground;
    {$endif D7UP}
    {$ifdef D10UP}
    property  Padding;
    {$endif D10UP}
    {$ifdef D14UP}
    property Touch;
    property OnGesture;
    {$endif D14UP}
  end;

implementation

uses
  {$ifdef D7UP}Themes, {$endif D7UP}
  Forms,
  teRender;

type
  TTransitionDeviceHack = class(TTEVCLControlTrDevice);
  TTransitionEffectHack = class(TTransitionEffect);

{ TTEImage }

constructor TTEImage.Create(AOwner: TComponent);
begin
  inherited;

  Width               := 185;
  Height              :=  41;
  FBackgroundOptions  := TFCBackgroundOptions.Create;
  FBackgroundOptions.OnChange := BkgrndChanged;
  FPicture            := TPicture.Create;
  FPicture.OnChange   := PictureChanged;
  FPictureMargin      := 0;
  FPictureMode        := fcpmCenter;
  FPictureTranspColor := clNone;
  FPictureVisible     := True;
  FDrawing            := False;
  FullAreaTransition  := False;
  FDestroyTransition  := False;
  PreparedPicRect     := Rect(0, 0, 0, 0);
  BkgrndHasChanged    := False;
  FTransitionDevice    := nil;
end;

destructor TTEImage.Destroy;
begin
  UnPrepareTransition;

  FPicture          .Free;
  FBackgroundOptions.Free;

  inherited;
end;

procedure TTEImage.WMDestroy(var Msg: TMessage);
begin
  UnPrepareTransition;

  inherited;
end;

procedure TTEImage.AbortTransition;
begin
  if TransitionPrepared then
  begin
    if TransitionExecuting then
      // Avoid showing the destination bitmap
      TTransitionDeviceHack(TransitionDevice).Data.AlwaysShowLastFrame := False;
    FTransitionDevice.Abort;
    UnPrepareTransition;
  end;
end;

function TTEImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := FPicture.Graphic;
  if Visible and FPictureVisible and (not (csLoading in ComponentState)) and
    (Tmp <> nil) and (Tmp.PaletteModified) then
  begin
    if (Tmp.Palette = 0) then
      Tmp.PaletteModified := False
    else
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
      begin
        if FDrawing then
          ParentForm.Perform(wm_QueryNewPalette, 0, 0)
        else
          PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
        Result := True;
        Tmp.PaletteModified := False;
      end;
    end;
  end;
end;

function TTEImage.GetPalette: HPalette;
begin
  if(FPicture.Graphic <> nil)      and
     FPictureVisible               and  
    (FPicture.Graphic.Palette <> 0)
  then Result := FPicture.Graphic.Palette
  else Result := BackgroundOptions.GetPalette;
end;

procedure TTEImage.SetParent(AParent: TWinControl);
begin
  inherited;

  if Assigned(BackgroundOptions) and (not(csDestroying in ComponentState)) then
    BackgroundOptions.Control := Self;
end;

function TTEImage.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

procedure TTEImage.SetVersion(const Value: String);
begin
end;

procedure TTEImage.SetBackgroundOptions(Value: TFCBackgroundOptions);
begin
  BackgroundOptions.Assign(Value);
end;

procedure TTEImage.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TTEImage.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TTEImage.SetPicture(const Value: TPicture);
begin
  if Assigned(Value)
  then FPicture.Assign(Value)
  else FPicture.Graphic := nil;
end;

procedure TTEImage.SetPictureMargin(const Value: Word);
begin
  if FPictureMargin <> Value then
  begin
    FPictureMargin := Value;
    PictureChanged(Self);
  end;
end;

procedure TTEImage.SetPictureMode(Value: TFCPictureMode);
begin
  if FPictureMode <> Value then
  begin
    FPictureMode := Value;
    PictureChanged(Self);
  end;
end;

procedure TTEImage.SetPictureTranspColor(Value: TColor);
begin
  if FPictureTranspColor <> Value then
  begin
    FPictureTranspColor := Value;
    PictureChanged(Self);
  end;
end;

procedure TTEImage.SetPictureVisible(const Value: Boolean);
begin
  if FPictureVisible <> Value then
  begin
    FPictureVisible := Value;
    PictureChanged(Self);
    Invalidate;
  end;
end;

procedure TTEImage.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;

  if(not(csDestroying in ComponentState)) and
    BackgroundOptions.IsActive then
    BackgroundOptions.ControlChanged(Self);
end;

procedure TTEImage.WMWindowPosChanging(var Message: TWMWindowPosChanging);
var
  Sizing,
  Hiding: Boolean;
begin
  inherited;

  Sizing :=
    ((Message.WindowPos^.flags and SWP_NOSIZE    ) =  0) and
    IsWindowVisible(Handle);
  Hiding :=
    ((Message.WindowPos^.flags and SWP_HIDEWINDOW) <> 0) and
    IsWindowVisible(Handle);

  if(Sizing or Hiding) then
    UnPrepareTransition;
end;

procedure TTEImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
begin
  G := FPicture.Graphic;
  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := FPictureTranspColor <> clNone;

    if DoPaletteChange and FDrawing and FPictureVisible then
      Update;
  end;
  if(not FDrawing) and FPictureVisible and (not TransitionPrepared) then
    Invalidate;
end;

procedure TTEImage.BkgrndChanged(Sender: TObject);
begin
  if TransitionPrepared then
    BkgrndHasChanged := True;
end;

procedure TTEImage.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  {$ifdef D7UP}
  if BEParentBackgroundPainted(Handle) then
    BackgroundOptions.DrawBackGround(Message.DC, nil, Rect(0, 0, 0, 0));
  {$endif D7UP}
  Message.Result := 1;
end;

procedure TTEImage.DoPaint(Bmp: TBitmap; R: TRect; DrawPic: Boolean);
var
  P: TPoint;
begin
  OffsetWindowOrgEx(Bmp.Canvas.Handle, R.Left, R.Top, P);
  try
    BackgroundOptions.DrawBackGround(Bmp.Canvas.Handle, Bmp, R);
    if DrawPic                      and
       FPictureVisible              and
      (FPicture.Graphic <> nil)     and
      (not FPicture.Graphic.Empty)  and
      (FPicture.Graphic.Width  > 0) and
      (FPicture.Graphic.Height > 0) then
    begin
      teBkgrnd.DrawPicture(FPicture.Graphic, FPictureMode, FPictureTranspColor,
        Self, Bmp, R, FPictureMargin, Self);
    end;
  finally
    SetWindowOrgEx(Bmp.Canvas.Handle, P.X, P.Y, nil);
  end;
end;

procedure TTEImage.Paint;
var
  R,
  RAux,
  RFrame: TRect;
  Save,
  Painted,
  LeaveCSBitmap,
  LeaveCSThread: Boolean;
  LocalBmp,
  FrameBmp: TBitmap;
  DeviceHack: TTransitionDeviceHack;
begin
  Save     := FDrawing;
  FDrawing := True;
  try
    R := Canvas.ClipRect;
    if IsRectEmpty(R) then
      R := Rect(0, 0, ClientWidth, ClientHeight);

    DeviceHack    := TTransitionDeviceHack(FTransitionDevice);
    LeaveCSThread := False;
    if Assigned(DeviceHack) then
    begin
      EnterCriticalSection(DeviceHack.csThread);
      LeaveCSThread := True;
    end;
    try
      Painted := False;
      if TransitionPrepared then
      begin // Painting while transition is prepared
        LeaveCSBitmap := False;
        try
          FrameBmp := FTransitionDevice.GetCurrentFrameBmp(LeaveCSBitmap);
          if Assigned(FrameBmp) then
          begin
            Painted := True;
            IntersectRect(RAux, R, DeviceHack.CtrlRect);
            if not IsRectEmpty(RAux)
            then // Current frame must be repainted
            begin
              if not EqualRect(RAux, R)
              then // The prepared area does not cover all the dirty pixels
              begin
                LocalBmp := TBitmap.Create;
                try
                  AdjustBmpForTransition(LocalBmp, 0, R.Right - R.Left,
                    R.Bottom - R.Top, DevicePixelFormat(False));
                  DoPaint(LocalBmp, R, False); // Paint the background
                  if not IsRectEmpty(RAux) then
                  begin
                    RFrame := RAux;
                    OffsetRect(RFrame, -R.Left, -R.Top);
                    // Paint the current frame
                    BitBlt(
                      LocalBmp.Canvas.Handle,
                      RFrame.Left,
                      RFrame.Top,
                      RFrame.Right  - RFrame.Left,
                      RFrame.Bottom - RFrame.Top,
                      FrameBmp.Canvas.Handle,
                      RAux.Left - DeviceHack.CtrlRect.Left,
                      RAux.Top  - DeviceHack.CtrlRect.Top,
                      cmSrcCopy);
                  end;
                  BitBlt(Canvas.Handle, R.Left, R.Top, R.Right - R.Left,
                    R.Bottom - R.Top, LocalBmp.Canvas.Handle, 0, 0, cmSrcCopy);
                finally
                  LocalBmp.Free;
                end;
              end
              else // The prepared area covers all the dirty pixels
              begin
                // Paint the current frame
                BitBlt(
                  Canvas.Handle,
                  R.Left,
                  R.Top,
                  R.Right - R.Left,
                  R.Bottom - R.Top,
                  FrameBmp.Canvas.Handle,
                  R.Left - DeviceHack.CtrlRect.Left,
                  R.Top  - DeviceHack.CtrlRect.Top,
                  cmSrcCopy);
              end
            end
            else // Only background needs to be painted
            begin
              LocalBmp := TBitmap.Create;
              try
                AdjustBmpForTransition(LocalBmp, 0, R.Right - R.Left, R.Bottom - R.Top,
                  DevicePixelFormat(False));
                DoPaint(LocalBmp, R, False);
                BitBlt(Canvas.Handle, R.Left, R.Top, R.Right - R.Left,
                  R.Bottom - R.Top, LocalBmp.Canvas.Handle, 0, 0, cmSrcCopy);
              finally
                LocalBmp.Free;
              end;
            end;
          end;
        finally
          if LeaveCSBitmap then
            LeaveCriticalSection(TTransitionDeviceHack(FTransitionDevice).CSBitmap);
        end;
      end;

      if not Painted then
      begin  // Standard painting
        LocalBmp := TBitmap.Create;
        try
          AdjustBmpForTransition(LocalBmp, 0, R.Right - R.Left, R.Bottom - R.Top,
            DevicePixelFormat(False));
          DoPaint(LocalBmp, R, True);
          BitBlt(Canvas.Handle, R.Left, R.Top, R.Right - R.Left,
            R.Bottom - R.Top, LocalBmp.Canvas.Handle, 0, 0, cmSrcCopy);
        finally
          LocalBmp.Free;
        end;
      end;
    finally
      if LeaveCSThread then
        LeaveCriticalSection(DeviceHack.csThread);
    end;
  finally
    FDrawing := Save;
  end;
end;

function TTEImage.Transition: TTransitionEffect;
begin
  if TransitionPrepared
  then Result := FTransitionDevice.Transition
  else Result := nil;
end;

function TTEImage.TransitionExecuting: Boolean;
begin
  Result := Assigned(FTransitionDevice) and FTransitionDevice.Executing;
end;

function TTEImage.TransitionPrepared: Boolean;
begin
  Result := Assigned(FTransitionDevice);
end;

procedure TTEImage.InternalPrepareTransition(Transition: TTransitionEffect;
  R: TRect; SrcBmp: TBitmap);
var
  DeviceHack: TTransitionDeviceHack;
begin
  if FTransitionDevice = nil then
    FTransitionDevice := TTEVCLControlTrDevice.Create;
  try
    if FTransitionDevice.Transition = nil then
      FTransitionDevice.Transition := Transition;
    if not TTransitionDeviceHack(FTransitionDevice).AllowTransition then
      exit;
    DeviceHack := TTransitionDeviceHack(FTransitionDevice);
    if FTransitionDevice.Prepare(Self, R, Canvas)
    then
    begin
      DeviceHack.SrcImage := TBitmap.Create;
      AdjustBmpForTransition(DeviceHack.SrcImage, 0,
        TTransitionEffectHack(FTransitionDevice.DelegateTransition).GetBitmapsWidth(DeviceHack.Data),
        DeviceHack.Data.Height,
        TTransitionEffectHack(FTransitionDevice.DelegateTransition).GetPixelFormat(FTransitionDevice));
      if Assigned(SrcBmp)
      then BitBlt(DeviceHack.SrcImage.Canvas.Handle, 0, 0,
             DeviceHack.Data.Width, DeviceHack.Data.Height,
             SrcBmp.Canvas.Handle, R.Left, R.Top, cmSrcCopy)
      else DoPaint(DeviceHack.SrcImage, R, True);
    end
    else FreeAndNil(FTransitionDevice);
  except
    on Exception do
    begin
      FreeAndNil(FTransitionDevice);
      raise;
    end;
  end;
end;

function TTEImage.PrepareTransition(Transition: TTransitionEffect; const
  FullArea: Boolean = True; const DestroyTransition: Boolean = False): Boolean;
var
  aux: TRect;
begin
  AbortTransition;

  Result             := False;
  FullAreaTransition := FullArea;
  FDestroyTransition := DestroyTransition;
  BkgrndHasChanged   := False;

  if FullAreaTransition or (not FPictureVisible) or (FPicture.Graphic = nil)
  then PreparedPicRect := Rect(0, 0, 0, 0)
  else
  begin
    PreparedPicRect := teBkgrnd.PictureRect(FPicture.Graphic, FPictureMode,
      FPictureMargin, Self, Self, aux);
    IntersectRect(PreparedPicRect, PreparedPicRect, ClientRect);
  end;

  try
    InternalPrepareTransition(Transition, Rect(0, 0, ClientWidth, ClientHeight),
      nil);
    Result := Assigned(FTransitionDevice);
  finally
    if not Result then
      UnPrepareTransition;
  end;
end;

procedure TTEImage.InternalUnPrepareTransition(FullUnprepare: Boolean);
var
  SaveTransition: TTransitionEffect;
  SaveFreeDelegateTransition: Boolean;
begin
  if TransitionPrepared and FTransitionDevice.AllowAbort then
  begin
    if FTransitionDevice.Executing or FTransitionDevice.UsingThread
    then AbortTransition
    else
    begin
      if FullUnprepare
      then
      begin
        if FDestroyTransition
        then SaveTransition := FTransitionDevice.Transition
        else SaveTransition := nil;
        FreeAndNil(FTransitionDevice);
        SaveTransition.Free;
        FullAreaTransition := False;
        PreparedPicRect    := Rect(0, 0, 0, 0);
        Invalidate;
      end
      else
      begin
        SaveFreeDelegateTransition :=
          TTransitionDeviceHack(FTransitionDevice).FreeDelegateTransition;
        TTransitionDeviceHack(FTransitionDevice).FreeDelegateTransition := False;
        try
          FTransitionDevice.UnPrepare;
        finally
          TTransitionDeviceHack(FTransitionDevice).FreeDelegateTransition :=
            SaveFreeDelegateTransition;
        end;
      end;
    end;
  end;
end;

procedure TTEImage.UnPrepareTransition;
begin
  InternalUnPrepareTransition(True);
end;

procedure TTEImage.ExecuteTransition(WaitForCompletion: Boolean = True);
var
  DeviceHack: TTransitionDeviceHack;
  R,
  NewPicRect,
  aux: TRect;
  DoUnprepare: Boolean;
  Bmp: TBitmap;
  SaveTransition: TTransitionEffect;
begin
  Update;
  if TransitionPrepared then
  begin
    DoUnprepare := True;
    try
      if FullAreaTransition or BkgrndHasChanged
      then R := ClientRect
      else
      begin
        if FPictureVisible
        then
        begin
          NewPicRect := teBkgrnd.PictureRect(FPicture.Graphic, FPictureMode,
            FPictureMargin, Self, Self, aux);
          IntersectRect(NewPicRect, NewPicRect, ClientRect);
        end
        else NewPicRect := Rect(0, 0, 0, 0);
        UnionRect(R, PreparedPicRect, NewPicRect);
      end;

      if not IsRectEmpty(R) then
      begin
        DeviceHack := TTransitionDeviceHack(FTransitionDevice);

        if not EqualRect(R, DeviceHack.CtrlRect) then
        begin
          Bmp := DeviceHack.SrcImage;
          try
            DeviceHack.SrcImage := nil;
            SaveTransition      := DeviceHack.Transition;
            InternalUnPrepareTransition(False);
            InternalPrepareTransition(SaveTransition, R, Bmp);
          finally
            FreeAndNil(Bmp);
          end;
          if Assigned(FTransitionDevice) then
            DeviceHack := TTransitionDeviceHack(FTransitionDevice);
        end;

        DeviceHack.DstImage := TBitmap.Create;
        AdjustBmpForTransition(DeviceHack.DstImage, 0,
          TTransitionEffectHack(DeviceHack.DelegateTransition).GetBitmapsWidth(DeviceHack.Data),
          DeviceHack.Data.Height,
          TTransitionEffectHack(DeviceHack.DelegateTransition).GetPixelFormat(FTransitionDevice));
        DoPaint(DeviceHack.DstImage, R, True);

        if FPictureVisible                                            and
          (DeviceHack.TransitionToUse.Passes(DeviceHack) = 2)         and
          (not DeviceHack.TransitionToUse.Pass2Options.UseSolidColor) then
        begin
          DeviceHack.Pass2Image := TBitmap.Create;
          AdjustBmpForTransition(DeviceHack.Pass2Image, 0,
            TTransitionEffectHack(DeviceHack.TransitionToUse).GetBitmapsWidth(DeviceHack.Data),
            DeviceHack.Data.Height,
            TTransitionEffectHack(DeviceHack.DelegateTransition).GetPixelFormat(FTransitionDevice));
          DoPaint(DeviceHack.Pass2Image, R, False);
        end;

        FTransitionDevice.Execute(WaitForCompletion);
        DoUnprepare := WaitForCompletion;
      end;
    finally
      if DoUnprepare then
        UnPrepareTransition;
    end;
  end;
end;

procedure TTEImage.CMTEThreadTerminated(var Message: TWMNoParams);
begin
  if TransitionPrepared then
  begin
    try
      TTransitionDeviceHack(FTransitionDevice).OnTransitionThreadTerminated;
    finally
      UnPrepareTransition;
    end;
  end;
end;

end.
