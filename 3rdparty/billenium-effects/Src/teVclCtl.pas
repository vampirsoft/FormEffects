unit teVclCtl;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Graphics, TransEff;

{$INCLUDE teDefs.inc}

type
  TTEVCLControlTrDevice = class(TTETransitionDevice)
  private
  protected
    Control: TWinControl;
    CtrlRect: TRect;

    procedure CustomExecute; override;
    function  GetRenderWndHandle: HWnd; override;
    class function TransitionIsDisabled(Transition: TTransitionEffect;
      NoFlickerFreeWhenDisabled: Boolean): Boolean; override;
    procedure OnTransitionThreadTerminated; override;
    class function IsThreadSafe: Boolean; override;
    function NeedOffScreenBmp: Boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure Abort; override;
    function  Clipped: Boolean; override;
    function  DynamicClipping: Boolean; override;
    function  HasPalette: Boolean; override;
    function  PixelFormat: TPixelFormat; override;
    function  Prepare(Ctrl: TWinControl; R: TRect; CtrlCanvas: TCanvas): Boolean;
    function  Prepared: Boolean;
    function  TwoPassesCapable: Boolean; override;
    procedure UnPrepare;
  end;

implementation

uses teRender, ComCtrls, teChrono;

const
  WS_EX_LAYERED = $00080000;
  MinStep       = 1;

type
  TTEWinControl         = class(TWinControl);
  TTransitionEffectHack = class(TTransitionEffect);

constructor TTEVCLControlTrDevice.Create;
begin
  inherited;

  Control := nil;
end;

destructor TTEVCLControlTrDevice.Destroy;
begin
  UnPrepare;

  inherited;
end;

function TTEVCLControlTrDevice.Clipped: Boolean;
var
  ScreenR: TRect;
begin
  ScreenR := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
  ScreenR.TopLeft     := ControlClientToScreen(Control, ScreenR.TopLeft);
  ScreenR.BottomRight := ControlClientToScreen(Control, ScreenR.BottomRight);
  Result              := IsWindowClipped(Control.Handle, 0, ScreenR);
end;

function TTEVCLControlTrDevice.PixelFormat: TPixelFormat;
begin
  Result := DevicePixelFormat(False);
end;

function TTEVCLControlTrDevice.Prepare(Ctrl: TWinControl; R: TRect;
  CtrlCanvas: TCanvas): Boolean;
begin
  if Transition = nil then
    raise ETransitionEffectError.Create(rsTEDevTrIsNil);

  if Prepared then
    UnPrepare;

  if IsWindowVisible(Ctrl.Handle) then
  begin
    try
      Control  := Ctrl;
      CtrlRect := R;

      Initialize;

      Data.Width              := CtrlRect.Right  - CtrlRect.Left;
      Data.Height             := CtrlRect.Bottom - CtrlRect.Top;
      Data.DeviceCanvasOrgOff := CtrlRect.TopLeft;
      Data.DeviceCanvas       := CtrlCanvas;
      Data.DeviceWnd          := Control.Handle;

      if TEWinVersion >= teWinNT then
        Data.DeviceCanvas.Handle; // Validate handle
      Data.DeviceCanvas.Lock;   // Avoids memory leaking
    except
      on Exception do
      begin
        UnPrepare;
        raise;
      end;
    end;
  end;
  Result := Prepared;
end;

function TTEVCLControlTrDevice.Prepared: Boolean;
begin
  Result := Assigned(Control);
end;

procedure TTEVCLControlTrDevice.UnPrepare;
begin
  if Assigned(Data) then
  begin
    Data.DeviceCanvas.Unlock;
    Finalize;
  end;

  if Prepared then
  begin
    FreeAndNil(SrcImage);
    FreeAndNil(Pass2Image);
    FreeAndNil(DstImage);
    Control   := nil;
  end;
end;

class function TTEVCLControlTrDevice.TransitionIsDisabled(
  Transition: TTransitionEffect; NoFlickerFreeWhenDisabled: Boolean): Boolean;
begin
  Result :=
    TEGlobalDisabledStrict or
    TEGlobalDisabled       or
    inherited TransitionIsDisabled(Transition, NoFlickerFreeWhenDisabled);
end;

function TTEVCLControlTrDevice.HasPalette: Boolean;
begin
  Result := PalettedDevice(False);
end;

procedure TTEVCLControlTrDevice.CustomExecute;
var
  OldPalette: hPalette;
  Pass2Chrono: TTEChrono;
  TotalMilliseconds: Integer;
begin
  if not Prepared then
    Exit;
  try
    if(Pass2Image = nil) and (DelegateTransition.Passes(Self) = 2) then
    begin
      if DelegateTransition.Pass2Options.SolidColor = clNone then
        DelegateTransition.Pass2Options.SolidColor := TTEWinControl(Control).Color;
      Get2ndPassBmp;
    end;

    if Pass2Image = nil
    then
    begin
      Data.SrcBmp := SrcImage;
      Data.DstBmp := DstImage;
      GetOffScreenBmp(OldPalette);
      ExePass(1, nil, DelegateTransition.Milliseconds);
    end
    else
    begin
      TotalMilliseconds := DelegateTransition.Milliseconds;
      if DelegateTransition.Pass2Options.DistributedTime and
        (DelegateTransition.Milliseconds <> 0)
      then
      begin
        DelegateTransition.Milliseconds := TotalMilliseconds DIV 2;
        Pass2Chrono := TTEChrono.Create;
      end
      else Pass2Chrono := nil;

      try
        Data.SrcBmp := SrcImage;
        Data.DstBmp := Pass2Image;
        GetOffScreenBmp(OldPalette);
        ExePass(1, Pass2Chrono, TotalMilliseconds);
        Data.SrcBmp := Pass2Image;
        Data.DstBmp := DstImage;
        GetOffScreenBmp(OldPalette);
        ExePass(2, Pass2Chrono, TotalMilliseconds);
      finally
        Pass2Chrono.Free;
      end;
    end;
  finally
    if not UsingThread then
      UnPrepare;
  end;
end;

function TTEVCLControlTrDevice.TwoPassesCapable: Boolean;
begin
  Result := True;
end;

class function TTEVCLControlTrDevice.IsThreadSafe: Boolean;
begin
  Result := True;
end;

function TTEVCLControlTrDevice.GetRenderWndHandle: HWnd;
begin
  Result := Control.Handle;
end;

procedure TTEVCLControlTrDevice.OnTransitionThreadTerminated;
begin
  UnPrepare;

  inherited;
end;

procedure TTEVCLControlTrDevice.Abort;
var
  Msg: TMsg;
begin
  EnterCriticalSection(CSThread);
  try
    if Assigned(FTransitionThread)
    then FTransitionThread.NotifyTermination := False
    else
    begin
      inherited;
      exit;
    end;
  finally
    LeaveCriticalSection(CSThread);
  end;

  inherited;

  if Assigned(FTransitionThread) then
  begin
    TransitionThread.WaitFor; // Beware deadlock WaitFor <-> Synchronize
    while Assigned(Control) and
          PeekMessage(Msg, Control.Handle, CM_TETHREADTERMINATED, CM_TETHREADTERMINATED, PM_REMOVE) do;
    if Assigned(Control) then
      Control.Perform(CM_TETHREADTERMINATED, 0, 0);
  end;
end;

// Returns if the transition output clipping state may change during execution
function TTEVCLControlTrDevice.DynamicClipping: Boolean;
begin
  Result := UsingThread;
end;

function TTEVCLControlTrDevice.NeedOffScreenBmp: Boolean;
begin
  Result := UsingThread;
end;

end.
