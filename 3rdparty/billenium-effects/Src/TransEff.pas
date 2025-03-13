unit TransEff;

interface

{$INCLUDE teDefs.inc}
{$ifdef NOVCL}
  {$define NoDefTrDev}
{$endif NOVCL}

uses
  Windows, Messages, SysUtils, Classes, Consts, Graphics, Controls, 
  {$ifndef NOVCL}
  Forms,
  {$endif NOVCL}
  teChrono, teRender, SyncObjs;

{$ifndef TE_NOHLP}
const
  {$ifndef NOVCL}
  CM_TENAMECHANGED      = CM_BASE + 533;
  {$endif NOVCL}
  CM_TETHREADTERMINATED = $B000   + 534;
{$endif TE_NOHLP}

resourcestring
  rsTEDevNotThreadSafe   = 'Device is not thread safe';
  rsTEDevTrIsNil         = 'Device''s transition is not assigned';
  rsTETransitionBusy     = 'Transition is busy';
  rsTETransNotThreadSafe = 'Transition is not thread safe';
  rsTEUnknownTransition  = 'Unknow transition "%s"';

type
  TTEPassSettingType  = (teOnePass, teTwoPasses, tePaletteDependent);
  TTEEffectDirection  = (tedNone, tedRight, tedLeft, tedDown, tedUp,
    tedDownRight, tedDownLeft, tedUpRight, tedUpLeft, tedIn, tedOut, tedRandom);
  TTEEffectDirections = set of TTEEffectDirection;
  {$ifndef TE_NOHLP}
  TTETransitionInfo = set of (
    tetiMillisecondsCapable,   // Supports timing
    tetiNeedDstBmp,            // Needs the destination bitmap
    tetiNeedOffScreenBmp,      // Needs the offscreen bitmap
    tetiNeedSrcBmp,            // Needs the source bitmap
    tetiOffScreenBmpCapable,   // Supports using an offscreen bitmap
    tetiStaticSrcPixels,       // Pixels in the source bitmap never move while displayed
    tetiUseDirtyRects,         // Uses the list of dirty rects
    tetiUseSrcAsOffScreenBmp,  // Uses SrcBmp as offscreen bitmap
    tetiThreadSafe,            // Transition is thread-safe
    tetiTwoPassesCapable,      // Supports two passes
    tetiLayeredCapable);       // Supports layered form transitions (Vista aero interface)
  {$endif TE_NOHLP}

  ETransitionEffectError = class(Exception);

  TTransitionEffect   = class;
  {$ifndef TE_NOHLP}
  TTETransitionDevice = class;
  {$endif TE_NOHLP}
  TTEAbortQueryEvent  = procedure(Sender: TObject; Transition: TTransitionEffect;
    var Abort: Boolean) of object;

  TTEPass2OptionsType = class(TPersistent)
  private
    FDistributedTime,
    FReversed,
    FUseSolidColor: Boolean;
    FSolidColor: TColor;
  public
    constructor Create; virtual;

    procedure Assign(Source: TPersistent); override;
  published
    property DistributedTime: Boolean read FDistributedTime write FDistributedTime default False;
    property Reversed: Boolean read FReversed write FReversed default False;
    property UseSolidColor: Boolean read FUseSolidColor write FUseSolidColor default True;
    property SolidColor: TColor read FSolidColor write FSolidColor default clNone;
  end;

  {$ifndef TE_NOHLP}
  TTETransitionData = class;
  TTEDirtyRects     = class;
  TTEMakeSubComponentLinkable = procedure(ComponentClass: TComponentClass) of object;
  {$endif TE_NOHLP}

  {$ifndef NOVCL}
  TTransitionList   = class;
  {$endif NOVCL}

  {$ifdef LogTiming}
  TTELogItem = record
    LogExTime: Single;
    LogFrame: Integer;
    LogInterval: Single;
    LogUpdateTime: Single;
    LogSleepTime: Single;
    LogSleepPrecision: Single;
    LogStep: Single;
    LogStepTime: Single;
    LogTransitionTime: Single;
    LogWorkTime: Single;
    LogText: string;
  end;
  PTELogItem = ^TTELogItem;

  TTELogBase = class(TComponent)
  public
    ItemCount: Integer;
    ChronoExtra: TTEChrono;
    Detailed: Boolean;
    function CurrentItem: PTELogItem; virtual; abstract;
    function LastTransitionTime: Single; virtual; abstract;
    procedure NewItem; virtual; abstract;
    procedure SaveLog(Transition: TTransitionEffect; Data: TTETransitionData;
      ElapsedTime: Double); virtual; abstract;
    procedure SetSize(Size: Integer); virtual; abstract;
  end;
  {$endif LogTiming}

  TTransitionEffect = class(TComponent)
  private
    FDirection: TTEEffectDirection;
    FReversed: Boolean;
    {$ifndef NOVCL}
    FTransitionList: TTransitionList;
    {$endif NOVCL}
    FAbortOnClick,
    FAbortOnEscape,
    FEnabled,
    FFlickerFreeWhenDisabled: Boolean;
    FMinAbortInterval,
    FMilliseconds: Longint;
    FPass2Options: TTEPass2OptionsType;
    FPassSetting: TTEPassSettingType;
    FDelegatedFrom: TTransitionEffect;

    FOnAbortQuery: TTEAbortQueryEvent;
    FOnAfterTransition,
    FOnBeforeTransition,
    FOnStartTransition,
    FOnEndTransition: TNotifyEvent;
    {$ifndef NoDefTrDev}
    FClientCoordinates: Boolean;
    {$endif NoDefTrDev}

    procedure SetDirection(Value: TTEEffectDirection);
    procedure SetEnabled(const Value: Boolean);
    {$ifndef NOVCL}
    function  GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    procedure SetTransitionList(const Value: TTransitionList);
    {$endif NOVCL}
    function  GetVersion: String;
    procedure SetVersion(const Value: String);

    {$ifndef NoDefTrDev}
    procedure CheckDefaultDevice;
    procedure ReleaseDefaultDevice;
    function  GetAborted: Boolean;
    function  GetExecuting: Boolean;
    function  GetPrepared: Boolean;
    {$endif NoDefTrDev}
  protected
    {$ifndef NoDefTrDev}
    DefaultDevice: TTETransitionDevice;
    {$endif NoDefTrDev}

    function  DirectionToUse: TTEEffectDirection;
    function  EditorQuestion: string; virtual;
    function  ReversedDirection: TTEEffectDirection; virtual;
    function  GetBitmapsWidth(Data: TTETransitionData): Integer; virtual;
    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; virtual;
    procedure Initialize(Data: TTETransitionData; var Frames: Longint); virtual;
    {$ifndef NOVCL}
    procedure ReadState(Reader: TReader); override;
    procedure SetName(const Value: TComponentName); override;
    {$endif NOVCL}
    function  MakeSubComponentsLinkable(Proc: TTEMakeSubComponentLinkable): Boolean; virtual;
    procedure Finalize(Data: TTETransitionData); virtual;
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame,
      Step, LastExecutedFrame: Longint); virtual; abstract;
    function  GetInfo(Device: TTETransitionDevice): TTETransitionInfo; virtual;
    function  BoolToStr(Value: Boolean): String;
    procedure LoadFromStrings(List: TStrings; Prefix: String); virtual;
    procedure SaveToStrings(List: TStrings; OmitDefaultValues: Boolean;
      Prefix: String); virtual;
  public
    AllowedDirections: TTEEffectDirections;
    ForceRendering,
    NeverRendering: Boolean;
    {$ifdef LogTiming}
    Log: TTELogBase;
    {$endif LogTiming}

    constructor Create(AOwner: TComponent = nil); override;
    destructor  Destroy; override;

    class function Description: String; virtual;

    procedure Assign(Source: TPersistent); override;
    {$ifndef NOVCL}
    function  HasParent: Boolean; override;
    function  GetParentComponent: TComponent; override;
    procedure SetParentComponent(AParent: TComponent); override;
    {$endif NOVCL}
    class function GetEditor: String; virtual;
    function  Passes(Device: TTETransitionDevice): Integer;

    {$ifndef TE_NOHLP}
    function  GetDelegate(Device: TTETransitionDevice;
      const ReturnCopy: Boolean): TTransitionEffect; virtual;
    {$endif TE_NOHLP}
    {$ifndef NoDefTrDev}
    {$ifndef TE_NOHLP}
    procedure Abort;
    {$endif TE_NOHLP}
    procedure Defrost;
    procedure Execute;
    function  Freeze(Ctrl: TControl; R: TRect): Boolean;
    function  Frozen: Boolean;
    function  Prepare(Ctrl: TControl; R: TRect): Boolean;
    procedure Prepare2ndPass;
    procedure UnPrepare;

    property ClientCoordinates: Boolean read FClientCoordinates write FClientCoordinates;
    {$endif NoDefTrDev}

    property Direction: TTEEffectDirection read FDirection write SetDirection;
    property Milliseconds: Longint read FMilliseconds write FMilliseconds default 0;
    property MinAbortInterval: Longint read FMinAbortInterval write FMinAbortInterval default 300;
    property Reversed: Boolean read FReversed write FReversed default False;
    {$ifndef TE_NOHLP}
    {$ifndef NOVCL}
    property Index: Integer read GetIndex write SetIndex stored False;
    property TransitionList: TTransitionList read FTransitionlist write SetTransitionList;
    {$endif NOVCL}
    {$endif TE_NOHLP}
    {$ifndef NoDefTrDev}
    property Aborted: Boolean read GetAborted;
    property DelegatedFrom: TTransitionEffect read FDelegatedFrom;
    property Executing: Boolean read GetExecuting;
    property Prepared: Boolean read GetPrepared;
    {$endif NoDefTrDev}
    property Pass2Options: TTEPass2OptionsType read FPass2Options write FPass2Options;
    property PassSetting: TTEPassSettingType read FPassSetting write FPassSetting default teOnePass;
  published
    property AbortOnClick: Boolean read FAbortOnClick write FAbortOnClick default False;
    property AbortOnEscape: Boolean read FAbortOnEscape write FAbortOnEscape default False;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FlickerFreeWhenDisabled: Boolean read FFlickerFreeWhenDisabled write FFlickerFreeWhenDisabled default False;
    property Version: String read GetVersion write SetVersion stored False;

    property OnAbortQuery: TTEAbortQueryEvent read FOnAbortQuery write FOnAbortQuery;
    property OnAfterTransition: TNotifyEvent read FOnAfterTransition write FOnAfterTransition;
    property OnBeforeTransition: TNotifyEvent read FOnBeforeTransition write FOnBeforeTransition;
    property OnEndTransition  : TNotifyEvent read FOnEndTransition   write FOnEndTransition;
    property OnStartTransition: TNotifyEvent read FOnStartTransition write FOnStartTransition;
  end;

  {$ifndef TE_NOHLP}
  TTETransitionThread = class(TThread)
  private
    Device: TTETransitionDevice;

    procedure OnStart;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    CSSync: TRTLCriticalSection;
    WaitEvent: TSimpleEvent;
    NotifyTermination: Boolean;
    Executing,
    Executed: Boolean;

    constructor Create(ADevice: TTETransitionDevice);
    destructor  Destroy; override;
    procedure   DebugString(const Text: String);
  end;

  TTETransitionDevice = class(TObject)
  private
    FUsingThread: Boolean;
    FTransition: TTransitionEffect;

    procedure SetTransition(const Value: TTransitionEffect);
    procedure DoTimedExecution(TransitionChrono: TTEChrono);
    procedure DoCompleteExecution(TransitionChrono: TTEChrono);
  protected
    FAborted,
    FExecuting,
    FreeDelegateTransition,
    RenderSrcFrame,
    RenderDstFrame: Boolean;
    FTransitionThread: TTETransitionThread;
    SrcImage,
    Pass2Image,
    DstImage: TBitmap;
    Data: TTETransitionData;
    CSThread,
    CSBitmap: TRTLCriticalSection;
    PostponedOnEnd: Boolean;

    function  AllowTransition: Boolean; virtual;
    function  CheckAbort(CheckTimer: Boolean): Boolean; virtual;
    function  ExePassBegin(Pass: Integer; Pass2Chrono: TTEChrono): Boolean;
    procedure ExePassEnd(Pass: Integer);
    procedure ExePass(Pass: Integer; Pass2Chrono: TTEChrono; TotalMilliseconds:
      Integer);
    procedure Finalize; virtual;
    procedure Initialize; virtual;
    procedure TransitionInitialized; virtual;
    procedure CustomExecute; virtual; abstract;
    procedure GetOffScreenBmp(var OldPalette: hPalette); virtual;
    procedure Get2ndPassBmp;
    function  GetDelegateTransition(Original: TTransitionEffect;
      const ReturnCopy: Boolean): TTransitionEffect; virtual;
    function  GetExtTimingData(FrameRendered: Boolean): Integer; virtual;
    function  GetRenderWndHandle: HWnd; virtual;
    function  NeedOffScreenBmp: Boolean; virtual;
    procedure NextFrame(InternalLoop: Boolean;
      CurrentFrame, Milliseconds, ElapsedTime: Longint; Chrono:
      TTEChrono; var Interval, LastWorkTime, StepStartTime, Step, SleepPrec1,
      SleepPrec2: Single);
    procedure OnTransitionThreadTerminated; virtual;
    function  TransitionToUse: TTransitionEffect;
    function  TransitionThread: TTETransitionThread;
    procedure UpdateDevice(TransitionChrono: TTEChrono);
    class function IsThreadSafe: Boolean; virtual;
    class function TransitionIsDisabled(Transition: TTransitionEffect;
      NoFlickerFreeWhenDisabled: Boolean): Boolean; virtual;
  public
    AllowAbort: Boolean;
    DelegateTransition: TTransitionEffect;

    constructor Create; virtual;
    destructor  Destroy; override;

    procedure Abort; virtual;
    function  AvoidScrolling: Boolean; virtual;
    function  Clipped: Boolean; virtual;
    function  DynamicClipping: Boolean; virtual;
    procedure Execute(WaitForCompletion: Boolean = True); virtual;
    function  GetCurrentFrameBmp(var CriticalSectionEntered: Boolean): TBitmap;
    function  HasPalette: Boolean; virtual; abstract;
    function  IsRGB: Boolean;
    function  PixelFormat: TPixelFormat; virtual; abstract;
    function  TwoPassesCapable: Boolean; virtual; abstract;
    property  Aborted: Boolean read FAborted;
    property  Executing: Boolean read FExecuting;
    property  Transition: TTransitionEffect read FTransition write SetTransition;
    property  UsingThread: Boolean read FUsingThread;
  end;

  TTransitionEffectClass = class of TTransitionEffect;
  PTransitionEffect = ^TTransitionEffect;

  TCMTENameChanged = packed record
    Msg: Cardinal;
    Transition: TTransitionEffect;
    Unused,
    Result: Longint;
  end;
  {$endif TE_NOHLP}

  {$ifndef NOVCL}
  TTransitionList = class(TComponent)
  private
    function  GetTransition(Index: Integer): TTransitionEffect;
    procedure SetTransition(Index: Integer; const Value: TTransitionEffect);
    function  GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    FTransitions: TList;

    function  GetTransitionCount: Integer;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    {$ifndef TE_NOHLP}
    Editor: TWinControl;
    {$endif TE_NOHLP}

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure AddTransition(Transition: TTransitionEffect);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure RemoveTransition(Transition: TTransitionEffect);
    function  GetTransitionIndex(Transition: TTransitionEffect): Integer;

    property TransitionCount: Integer read GetTransitionCount;
    property Transitions[Index: Integer]: TTransitionEffect read GetTransition write SetTransition; default;
  published
    property Version: String read GetVersion write SetVersion stored False;
  end;
  {$endif NOVCL}

  {$ifndef TE_NOHLP}
  TTECustomData = class(TObject)
  public
    Data: TTETransitionData;

    constructor Create(AData: TTETransitionData); virtual;
  end;

  TTETransitionData = class(TObject)
  private
    FExternalTiming: Boolean;
    FBitmap: TBitmap;
    FSrcBmp: TBitmap;
    function GetCanvas: TCanvas;
    procedure SetExternalTiming(const Value: Boolean);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetSrcBmp(const Value: TBitmap);
  protected
    AbortChrono: TTEChrono;
    CurFrameBmp: TBitmap;         // Current frame bitmap
    LastUpdateTime: Double;
    SleepChrono: TTEChrono;
  public
    AllowDeviceUpdate: Boolean;   // Updating the device's canvas is allowed or not
    AlwaysShowLastFrame: Boolean; // Show last transition's frame in any case
    Custom: TTECustomData;        // Custom data placeholder for transitions
    Device: TTETransitionDevice;  // Device where the transition is rendered
    DeviceCanvas: TCanvas;        // Canvas of the device
    DeviceWnd: HWnd;              // Window handle of the device (in case it's a window)
    DirtyRects: TTEDirtyRects;    // List of dirty rects (rects which need updating)
    DstBmp: TBitmap;              // Bitmap containing the final frame
    FirstFrame: Integer;          // First frame to render (if -1 then calculate it)
    Frames: Integer;              // Count of transition frames for the pass
    PassFrames: Integer;          // Frames in pass (Frames + Src or Dst frames)
    TotalFrames: Integer;         // Total frames in transition (Frames + Src or Dst frames in all passes)
    TotalFrameIndex: Integer;     // Absolute frame index
    Height: Integer;              // Height in pixels of the frames
    DeviceCanvasOrgOff: TPoint;   // Origin offset to apply to DeviceCanvas
    Palette: HPalette;            // Palette to use
    Pass: Integer;                // The current pass number
    PassCount: Integer;           // The count of passes to execute
    PixelFormat: TPixelFormat;    // Exact pixel format of the frames
    PassRenderSrcFrame: Boolean;  // Render source frame in current pass
    PassRenderDstFrame: Boolean;  // Render destination frame in current pass
    UnUpdateRect: TRect;          // Rectangle which doesn't need to be updated
    UnUpdateRectBak: TRect;       // Value of UnUpdateRect in the previous step
    UpdateRect: TRect;            // Rectangle which needs to be updated
    UpdateRectBak: TRect;         // Value of UpdateRect in the previous step
    Width: Integer;               // Width in pixels of the frames

    constructor Create; virtual;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;       // Offscreen bitmap which holds the working frame
    property Canvas: TCanvas read GetCanvas;                     // Canvas of the current frame
    property ExternalTiming: Boolean read FExternalTiming write SetExternalTiming; // The timing is provided by the device
    property SrcBmp: TBitmap read FSrcBmp write SetSrcBmp;       // Bitmap containing the initial frame
  end;

  TTEDirtyRects = class(TObject)
  private
    FRects: TList;

    function  GetRect(Index: Integer): TRect;
    procedure SetRect(Index: Integer; const Value: TRect);
  protected
    procedure CheckOverlap(R: TRect);
    function  GetRectCount: Integer;
  public
    CheckBounds,
    AutoClear: Boolean;
    Bounds: TRect;

    constructor Create;
    destructor  Destroy; override;

    procedure AddRect(R: TRect);
    procedure RemoveRect(Index: Integer);
    procedure Clear;

    property Count: Integer read GetRectCount;
    property Rects[Index: Integer]: TRect read GetRect write SetRect; default;
  end;
  {$endif TE_NOHLP}

  TFlickerFreeTransition = class(TTransitionEffect)
  public
    {$ifndef TE_NOHLP}
    Fake: Boolean;
    {$endif TE_NOHLP}

    constructor Create(AOwner: TComponent = nil); override;
    procedure Assign(Source: TPersistent); override;

    class function Description: String; override;
  protected
    procedure ExecuteFrame(Data: TTETransitionData; CurrentFrame, Step,
      LastExecutedFrame: Longint); override;
    function GetInfo(Device: TTETransitionDevice): TTETransitionInfo; override;
    function  GetPixelFormat(Device: TTETransitionDevice): TPixelFormat; override;
    procedure Initialize(Data: TTETransitionData; var TotalFrames: Longint);
      override;
  end;

  function  TEGetDirectionDesc(Direction: TTEEffectDirection): String;
  function  TEGetDirectionFromDesc(Description: String): TTEEffectDirection;
  procedure TERegisterTransition(TransitionEffectClass: TTransitionEffectClass);
  function  TELoadTransitionFromStrings(List: TStrings): TTransitionEffect;
  procedure TESaveTransitionToStrings(Transition: TTransitionEffect;
    List: TStrings; OmitDefaultValues: Boolean=True);
  {$ifndef TE_NOHLP}
  function InternalLoadTransitionFromStrings(List: TStrings;
    Prefix: String): TTransitionEffect;
  {$endif TE_NOHLP}

var
  TEGlobalDisabled,
  TEGlobalDisabledStrict: Boolean;
  TERegisteredTransitions: TList;
  FlickerFreeTransition: TFlickerFreeTransition;
  {$ifndef TE_NOHLP}
  OldTransition,
  NewTransition: TTransitionEffect;
  {$endif TE_NOHLP}

implementation

uses {$ifndef NoVCL}teVclScr, {$endif NoVCL} TypInfo;

{ Common procedures and functions }

function InternalLoadTransitionFromStrings(List: TStrings;
  Prefix: String): TTransitionEffect;
var
  TransitionName: String;
  TransitionClass: TTransitionEffectClass;
  {$ifdef D6UP}
  OldGroup: TPersistentClass;
  {$endif D6UP}
begin
  TransitionName := List.Values[Prefix + 'Transition'];
  {$ifdef D6UP}
  OldGroup := ActivateClassGroup(TControl);
  try
  {$endif D6UP}
    TransitionClass := TTransitionEffectClass(GetClass(TransitionName));
    if TransitionClass = nil then
      raise ETransitionEffectError.CreateFmt(rsTEUnknownTransition, [TransitionName]);
  {$ifdef D6UP}
  finally
    ActivateClassGroup(OldGroup);
  end;
  {$endif D6UP}
  Result := TransitionClass.Create(nil);
  Result.LoadFromStrings(List, Prefix);
end;

function TELoadTransitionFromStrings(List: TStrings): TTransitionEffect;
begin
  Result := InternalLoadTransitionFromStrings(List, '');
end;

procedure TESaveTransitionToStrings(Transition: TTransitionEffect;
  List: TStrings; OmitDefaultValues: Boolean=True);
begin
  if Assigned(Transition) then
    Transition.SaveToStrings(List, OmitDefaultValues, '');
end;

function TEGetDirectionDesc(Direction: TTEEffectDirection): String;
begin
  case Direction of
    tedNone     : Result := '';
    tedRight    : Result := 'Right';
    tedLeft     : Result := 'Left';
    tedDown     : Result := 'Down';
    tedUp       : Result := 'Up';
    tedDownRight: Result := 'Down and right';
    tedDownLeft : Result := 'Down and left';
    tedUpRight  : Result := 'Up and right';
    tedUpLeft   : Result := 'Up and left';
    tedIn       : Result := 'In';
    tedOut      : Result := 'Out';
    tedRandom   : Result := 'Random';
    else          Result := '';
  end;
end;

function TEGetDirectionFromDesc(Description: String): TTEEffectDirection;
var
  i: Integer;
begin
  Result := tedNone;
  for i := 0 to Integer(High(TTEEffectDirection)) do
  begin
    if TEGetDirectionDesc(TTEEffectDirection(i)) = Description then
    begin
      Result := TTEEffectDirection(i);
      break;
    end;
  end;
end;

procedure TERegisterTransition(TransitionEffectClass: TTransitionEffectClass);
begin
  if TERegisteredTransitions = nil then
    TERegisteredTransitions := TList.Create;

  if TERegisteredTransitions.IndexOf(TransitionEffectClass) = -1 then
    TERegisteredTransitions.Add(TransitionEffectClass);

  {$ifdef D6UP}
  StartClassGroup(TControl);
  ActivateClassGroup(TControl);
  GroupDescendentsWith(TransitionEffectClass, Controls.TControl);
  {$endif D6UP}

  Classes.RegisterClass(TransitionEffectClass);
end;

{ TTEPass2OptionsType }

constructor TTEPass2OptionsType.Create;
begin
  FDistributedTime := False;
  FReversed        := False;
  FUseSolidColor   := True;
  FSolidColor      := clNone;
end;

procedure TTEPass2OptionsType.Assign(Source: TPersistent);
var
  aux: TTEPass2OptionsType;
begin
  if Source is TTEPass2OptionsType
  then
  begin
    aux := (Source as TTEPass2OptionsType);

    FDistributedTime := aux.DistributedTime;
    FReversed        := aux.Reversed;
    FUseSolidColor   := aux.UseSolidColor;
    FSolidColor      := aux.SolidColor;
  end
  else inherited;
end;

{ TTransitionEffect }
constructor TTransitionEffect.Create(AOwner: TComponent);
begin
  inherited;

  AllowedDirections        := [tedNone];
  FDirection               := tedNone;
  FAbortOnClick            := False;
  FAbortOnEscape           := False;
  FEnabled                 := True;
  FFlickerFreeWhenDisabled := False;
  ForceRendering           := False;
  NeverRendering           := False;
  {$ifndef NOVCL}
  FTransitionList          := nil;
  {$endif NOVCL}
  FPassSetting             := teOnePass;
  FReversed                := False;
  FPass2Options            := TTEPass2OptionsType.Create;
  FMinAbortInterval        := 300;
  FDelegatedFrom           := nil;

  {$ifndef NoDefTrDev}
  DefaultDevice      := nil;
  FClientCoordinates := True;
  {$endif NoDefTrDev}
end;

destructor TTransitionEffect.Destroy;
begin
  {$ifndef NOVCL}
  if Assigned(FTransitionList) then
    FTransitionList.RemoveTransition(Self);
  {$endif NOVCL}

  {$ifndef NoDefTrDev}
  UnPrepare;
  ReleaseDefaultDevice;
  {$endif NoDefTrDev}

  FPass2Options.Free;

  inherited;
end;

class function TTransitionEffect.Description: String;
begin
  Result := ClassName;
end;

procedure TTransitionEffect.Assign(Source: TPersistent);
var
  Transition: TTransitionEffect;
begin
  if Source is TTransitionEffect
  then
  begin
    Transition              := TTransitionEffect(Source);
    Milliseconds            := Transition.Milliseconds;
    MinAbortInterval        := Transition.MinAbortInterval;
    FPass2Options.Assign(Transition.Pass2Options);
    PassSetting             := Transition.PassSetting;
    ForceRendering          := Transition.ForceRendering;
    NeverRendering          := Transition.NeverRendering;
    Enabled                 := Transition.Enabled;
    FlickerFreeWhenDisabled := Transition.FlickerFreeWhenDisabled;
    if Transition.Direction in AllowedDirections then
      Direction             := Transition.Direction;
    Reversed                := Transition.Reversed;
    AbortOnClick            := Transition.AbortOnClick;
    AbortOnEscape           := Transition.AbortOnEscape;
    {$ifndef NoDefTrDev}
    ClientCoordinates       := Transition.ClientCoordinates;
    {$endif NoDefTrDev}
    {$ifdef LogTiming}
    Log                     := Transition.Log;
    {$endif LogTiming}
  end
  else inherited;
end;

{$ifndef NOVCL}
procedure TTransitionEffect.SetTransitionList(const Value: TTransitionList);
begin
  if Value <> FTransitionList then
  begin
    if FTransitionList <> nil then
      FTransitionList.RemoveTransition(Self);
    if Value <> nil then
      Value.AddTransition(Self);
  end;
end;

function TTransitionEffect.HasParent: Boolean;
begin
  if FTransitionList <> nil
  then Result := True
  else Result := inherited HasParent;
end;

function TTransitionEffect.GetParentComponent: TComponent;
begin
  if FTransitionList <> nil
  then Result := FTransitionList
  else Result := inherited GetParentComponent;
end;

procedure TTransitionEffect.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TTransitionList then
    TransitionList := TTransitionList(Reader.Parent);
end;

procedure TTransitionEffect.SetParentComponent(AParent: TComponent);
begin
  if(not(csLoading in ComponentState)) and (AParent is TTransitionList) then
    FTransitionList := TTransitionList(AParent);
end;

procedure TTransitionEffect.SetName(const Value: TComponentName);
begin
  inherited;

  if Assigned(FTransitionList) and Assigned(FTransitionList.Editor) then
    FTransitionList.Editor.Perform(CM_TENAMECHANGED, Longint(Self), 0);
end;

function TTransitionEffect.GetIndex: Integer;
begin
  if FTransitionList <> nil
  then Result := FTransitionList.FTransitions.IndexOf(Self)
  else Result := -1;
end;

procedure TTransitionEffect.SetIndex(Value: Integer);
var
  CurIndex,
  Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := FTransitionList.FTransitions.Count;
    if Value < 0 then
      Value := 0;
    if Value >= Count then
      Value := Count - 1;
    if Value <> CurIndex then
    begin
      FTransitionList.FTransitions.Delete(CurIndex);
      FTransitionList.FTransitions.Insert(Value, Self);
    end;
  end;
end;
{$endif NOVCL}

class function TTransitionEffect.GetEditor: String;
begin
  Result := 'TTransitionEffectEditor';
end;

function TTransitionEffect.DirectionToUse: TTEEffectDirection;
begin
  if Reversed
  then Result := ReversedDirection
  else Result := Direction;
end;

function TTransitionEffect.ReversedDirection: TTEEffectDirection;
begin
  case Direction of
    tedRight    : Result := tedLeft;
    tedLeft     : Result := tedRight;
    tedDown     : Result := tedUp;
    tedUp       : Result := tedDown;
    tedDownRight: Result := tedUpLeft;
    tedDownLeft : Result := tedUpRight;
    tedUpRight  : Result := tedDownLeft;
    tedUpLeft   : Result := tedDownRight;
    tedIn       : Result := tedOut;
    tedOut      : Result := tedIn;
    else          Result := Direction;
  end;
end;

function TTransitionEffect.GetPixelFormat(
  Device: TTETransitionDevice): TPixelFormat;
begin
  Result := Device.PixelFormat;
end;

function TTransitionEffect.GetBitmapsWidth(Data: TTETransitionData): Integer;
begin
  Result := Data.Width;
end;

procedure TTransitionEffect.SetDirection(Value: TTEEffectDirection);
begin
  if Value in AllowedDirections then
    FDirection := Value;
end;

procedure TTransitionEffect.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    {$ifndef NoDefTrDev}
    if not FEnabled then
      UnPrepare;
    {$endif NoDefTrDev}
  end;
end;

function TTransitionEffect.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

procedure TTransitionEffect.SetVersion(const Value: String);
begin
end;

function TTransitionEffect.Passes(Device: TTETransitionDevice): Integer;
{$ifndef NoDefTrDev}
var
  SaveDev: TTETransitionDevice;
{$endif NoDefTrDev}
begin
  {$ifndef NoDefTrDev}
  SaveDev := DefaultDevice;
  try
    if Device = nil then
    begin
      CheckDefaultDevice;
      Device := DefaultDevice;
    end;
  {$endif NoDefTrDev}

    Result := 0;
    if Device.TwoPassesCapable and (tetiTwoPassesCapable in GetInfo(Device))
    then
    begin
      case PassSetting of
        teOnePass:   Result := 1;
        teTwoPasses: Result := 2;
        tePaletteDependent:
          if Device.HasPalette
          then Result := 2
          else Result := 1;
      end;
    end
    else Result := 1;

  {$ifndef NoDefTrDev}
  finally
    if SaveDev = nil then
      ReleaseDefaultDevice;
  end;
  {$endif NoDefTrDev}
end;

{$ifndef NoDefTrDev}
procedure TTransitionEffect.CheckDefaultDevice;
begin
  if DefaultDevice = nil then
  begin
    DefaultDevice := TTEVCLScreenTrDevice.Create;
    try
      DefaultDevice.Transition := Self;
      TTEVCLScreenTrDevice(DefaultDevice).ClientCoordinates := ClientCoordinates;
    except
      on Exception do
      begin
        ReleaseDefaultDevice;
        raise;
      end;
    end;
  end;
end;

procedure TTransitionEffect.ReleaseDefaultDevice;
begin
  if DefaultDevice <> nil then
  begin
    DefaultDevice.Free;
    DefaultDevice := nil;
  end;
end;

function TTransitionEffect.GetAborted: Boolean;
begin
  if DefaultDevice <> nil
  then Result := DefaultDevice.Aborted
  else Result := False;
end;

function TTransitionEffect.GetExecuting: Boolean;
begin
  if DefaultDevice <> nil
  then Result := DefaultDevice.Executing
  else Result := False;
end;

function TTransitionEffect.GetPrepared: Boolean;
begin
  if DefaultDevice <> nil
  then Result := TTEVCLScreenTrDevice(DefaultDevice).Prepared
  else Result := False;
end;

procedure TTransitionEffect.Defrost;
begin
  if Assigned(DefaultDevice) then
    TTEVCLScreenTrDevice(DefaultDevice).Defrost;
end;

function TTransitionEffect.Freeze(Ctrl: TControl; R: TRect): Boolean;
begin
  CheckDefaultDevice;
  Result := TTEVCLScreenTrDevice(DefaultDevice).Freeze(Ctrl, R);
end;

function TTransitionEffect.Prepare(Ctrl: TControl; R: TRect): Boolean;
begin
  CheckDefaultDevice;
  Result := TTEVCLScreenTrDevice(DefaultDevice).Prepare(Ctrl, R);
end;

procedure TTransitionEffect.Prepare2ndPass;
begin
  if Assigned(DefaultDevice) then
    TTEVCLScreenTrDevice(DefaultDevice).Prepare2ndPass;
end;

procedure TTransitionEffect.UnPrepare;
begin
  if Assigned(DefaultDevice) then
    TTEVCLScreenTrDevice(DefaultDevice).UnPrepare;
end;

procedure TTransitionEffect.Execute;
var
  SaveDev: TTETransitionDevice;
begin
  SaveDev := DefaultDevice;
  try
    CheckDefaultDevice;
    DefaultDevice.Execute;
  finally
    if SaveDev = nil then
      ReleaseDefaultDevice;
  end;
end;

function TTransitionEffect.Frozen: Boolean;
begin
  if Assigned(DefaultDevice)
  then Result := TTEVCLScreenTrDevice(DefaultDevice).Frozen
  else Result := False;
end;

procedure TTransitionEffect.Abort;
begin
  if DefaultDevice <> nil then
    DefaultDevice.Abort;
end;

{$endif NoDefTrDev}

function TTransitionEffect.EditorQuestion: string;
begin
  Result := '';
end;

procedure TTransitionEffect.Finalize(Data: TTETransitionData);
begin
  FreeAndNil(Data.Custom);
end;

function TTransitionEffect.GetDelegate(
  Device: TTETransitionDevice; const ReturnCopy: Boolean): TTransitionEffect;
begin
  Result := Device.GetDelegateTransition(Self, ReturnCopy);
end;

function TTransitionEffect.MakeSubComponentsLinkable(Proc:
    TTEMakeSubComponentLinkable): Boolean;
begin
  Result := False;
end;

procedure TTransitionEffect.Initialize(Data: TTETransitionData;
  var Frames: Longint);
begin
end;

function TTransitionEffect.GetInfo(
  Device: TTETransitionDevice): TTETransitionInfo;
begin
  Result :=
    [
      tetiNeedDstBmp,
      tetiNeedSrcBmp,
      tetiTwoPassesCapable
    ];
end;

procedure TTransitionEffect.LoadFromStrings(List: TStrings; Prefix: String);
var
  Value: String;
begin
  inherited;

  Value := List.Values[Prefix + 'Direction'];
  if Value <> '' then
    Direction := TEGetDirectionFromDesc(Value);
end;

procedure TTransitionEffect.SaveToStrings(List: TStrings;
  OmitDefaultValues: Boolean; Prefix: String);
var
  Prop: String;
  PropInfo: PPropInfo;
begin
  List.Values[Prefix + 'Transition'] := ClassName;

  Prop := 'Direction';
  if IsPublishedProp(Self, Prop) then
  begin
    if OmitDefaultValues
    then PropInfo := GetPropInfo(Self, Prop, [tkEnumeration])
    else PropInfo := nil;
    if(not OmitDefaultValues) or
      (PropInfo.Default <> Ord(Direction)) then
      List.Values[Prefix + Prop] := TEGetDirectionDesc(Direction);
  end;
end;

function TTransitionEffect.BoolToStr(Value: Boolean): String;
const
  ft: array[0..1] of String = ('False', 'True');
begin
  Result := ft[Integer(Value)];
end;

{$ifndef NOVCL}

{ TTransitionList }

constructor TTransitionList.Create(AOwner: TComponent);
begin
  inherited;

  Editor       := nil;
  FTransitions := TList.Create;
end;

destructor TTransitionList.Destroy;
begin
  Clear;
  FTransitions.Free;

  inherited;
end;

procedure TTransitionList.AddTransition(Transition: TTransitionEffect);
begin
  FTransitions.Add(Transition);
  Transition.FTransitionList := Self;
end;

procedure TTransitionList.Assign(Source: TPersistent);
var
  i: Integer;
  Src: TTransitionList;
  TransitionClass: TTransitionEffectClass;
  NewTransition: TTransitionEffect;
begin
  if Source is TTransitionList
  then
  begin
    Src := TTransitionList(Source);
    Clear;
    for i := 0 to Src.TransitionCount - 1 do
    begin
      TransitionClass := TTransitionEffectClass(Src.Transitions[i].ClassType);
      NewTransition   := TransitionClass.Create(Self);
      NewTransition.Assign(Src.Transitions[i]);
      AddTransition(NewTransition);
    end;
  end
  else inherited;
end;

procedure TTransitionList.Clear;
begin
  if Assigned(FTransitions) then
    while FTransitions.Count > 0  do
      TTransitionEffect(FTransitions[0]).Free;
end;

procedure TTransitionList.RemoveTransition(Transition: TTransitionEffect);
begin
  if FTransitions.Remove(Transition) >= 0 then
    Transition.FTransitionList := nil;
end;

function TTransitionList.GetTransitionCount: Integer;
begin
  if FTransitions = nil
  then Result := 0
  else Result := FTransitions.Count;
end;

function TTransitionList.GetTransition(Index: Integer): TTransitionEffect;
begin
  Result := FTransitions[Index];
end;

procedure TTransitionList.SetTransition(Index: Integer;
  const Value: TTransitionEffect);
begin
  Transitions[Index].Free;
  AddTransition(Value);
  Value.Index := Index;
end;

procedure TTransitionList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  
  if(Operation = opRemove)            and
    (AComponent is TTransitionEffect) then
    RemoveTransition(TTransitionEffect(AComponent));
end;

function TTransitionList.GetVersion: String;
begin
  Result := BilleniumEffectsVersion;
end;

procedure TTransitionList.SetVersion(const Value: String);
begin
end;

procedure TTransitionList.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  i: Integer;
begin
  for i := 0 to FTransitions.Count - 1 do
    Proc(Transitions[i]);
end;

function TTransitionList.GetTransitionIndex(Transition: TTransitionEffect): Integer;
begin
  Result := FTransitions.IndexOf(Transition);
end;

{$endif NOVCL}

{ TTEDirtyRects }

procedure TTEDirtyRects.AddRect(R: TRect);
var
  P: PRect;
begin
  if CheckBounds then
  begin
    IntersectRect(R, R, Bounds);
    if IsRectEmpty(R) then
      exit;
  end;


  New(P);
  P^ := R;
  FRects.Add(P);
end;

procedure TTEDirtyRects.RemoveRect(Index: Integer);
begin
  Dispose(FRects[Index]);
  FRects.Delete(Index);
end;

constructor TTEDirtyRects.Create;
begin
  FRects := TList.Create;

  CheckBounds := False;
  AutoClear   := True;
end;

destructor TTEDirtyRects.Destroy;
begin
  Clear;
  FRects.Free;

  inherited;
end;

procedure TTEDirtyRects.Clear;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Dispose(FRects[i]);

  FRects.Clear;
end;

function TTEDirtyRects.GetRect(Index: Integer): TRect;
begin
  Result := TRect(FRects[Index]^);
end;

function TTEDirtyRects.GetRectCount: Integer;
begin
  Result := FRects.Count;
end;

procedure TTEDirtyRects.SetRect(Index: Integer; const Value: TRect);
begin
  TRect(FRects[Index]^) := Value;
end;

constructor TTECustomData.Create(AData: TTETransitionData);
begin
  inherited Create;

  Data := AData;
end;

constructor TTETransitionData.Create;
begin
  AllowDeviceUpdate   := True;
  AlwaysShowLastFrame := True;
  FBitmap             := nil;
  Custom              := nil;
  CurFrameBmp         := nil;
  Device              := nil;
  DeviceCanvas        := nil;
  DeviceWnd           := 0;
  DirtyRects          := nil;
  DstBmp              := nil;
  Height              := -1;
  LastUpdateTime      := 0;
  DeviceCanvasOrgOff  := Point(0, 0);
  Palette             := 0;
  Pass                := 0;
  PassCount           := 0;
  FirstFrame          := -1;
  Frames              := 0;
  PassFrames          := 0;
  PassRenderSrcFrame  := False;
  PassRenderDstFrame  := False;
  TotalFrames         := 0;
  PixelFormat         := pfDevice;
  FExternalTiming     := True;
  SleepChrono         := nil;
  FSrcBmp             := nil;
  UnUpdateRect        := Rect(0, 0, 0, 0);
  UpdateRect          := Rect(0, 0, 0, 0);
  UnUpdateRectBak     := Rect(0, 0, 0, 0);
  UpdateRectBak       := Rect(0, 0, 0, 0);
  Width               := -1;
end;

destructor TTETransitionData.Destroy;
begin
  if FBitmap <> FSrcBmp then
  begin
    if Assigned(FBitmap) then
    begin
      FBitmap.Canvas.Unlock;
      FreeAndNil(FBitmap);
    end;
  end;
  DirtyRects .Free;
  SleepChrono.Free;
  Custom     .Free;
end;

procedure TTEDirtyRects.CheckOverlap(R: TRect);
var
  i: Integer;
  RAux: TRect;
begin
  for i := 0 to FRects.Count-1 do
  begin
    if IntersectRect(RAux, R, GetRect(i)) then
      raise Exception.Create(
        Format(
          'DirtyRect overlapping: (%d, %d, %d, %d) -> (%d, %d, %d, %d)', [
          R.Left,
          R.Top,
          R.Right,
          R.Bottom,
          GetRect(i).Left,
          GetRect(i).Top,
          GetRect(i).Right,
          GetRect(i).Bottom
          ]));
  end;
end;
{ TTETransitionData }

function TTETransitionData.GetCanvas: TCanvas;
begin
  if Bitmap <> nil
  then Result := Bitmap.Canvas
  else Result := DeviceCanvas;
end;

procedure TTETransitionData.SetBitmap(const Value: TBitmap);
begin
  FBitmap     := Value;
  CurFrameBmp := Value;
end;

procedure TTETransitionData.SetExternalTiming(const Value: Boolean);
begin
  if FExternalTiming <> Value then
  begin
    FExternalTiming := Value;
    if FExternalTiming
    then FreeAndNil(SleepChrono)
    else SleepChrono := TTEChrono.Create;
  end;
end;

procedure TTETransitionData.SetSrcBmp(const Value: TBitmap);
begin
  if CurFrameBmp = FSrcBmp then
    CurFrameBmp := Value;
  FSrcBmp := Value;
end;

{ TFlickerFreeTransition }
constructor TFlickerFreeTransition.Create(AOwner: TComponent);
begin
  inherited;

  Fake := False;
end;

procedure TFlickerFreeTransition.Assign(Source: TPersistent);
var
  Transition: TFlickerFreeTransition;
begin
  if Source is TFlickerFreeTransition
  then
  begin
    inherited;

    Transition := TFlickerFreeTransition(Source);
    Fake       := Transition.Fake;
  end
  else inherited;
end;

class function TFlickerFreeTransition.Description: String;
begin
  Result := 'Flicker free cut';
end;

procedure TFlickerFreeTransition.ExecuteFrame(Data: TTETransitionData;
    CurrentFrame, Step, LastExecutedFrame: Longint);
begin
  // Nothing
end;

function TFlickerFreeTransition.GetInfo(Device: TTETransitionDevice):
  TTETransitionInfo;
begin
  Result := inherited GetInfo(Device) +
    [
      tetiThreadSafe
    ] -
    [
      tetiNeedSrcBmp,
      tetiStaticSrcPixels,
      tetiTwoPassesCapable
    ];
  if Fake then
    Exclude(Result, tetiNeedDstBmp);
end;

function TFlickerFreeTransition.GetPixelFormat(
  Device: TTETransitionDevice): TPixelFormat;
begin
  Result := Device.PixelFormat;
end;

procedure TFlickerFreeTransition.Initialize(Data: TTETransitionData; var
  TotalFrames: Longint);
begin
  inherited;
  
  TotalFrames := 1;
end;

{ TTETransitionDevice }

procedure TTETransitionDevice.Abort;
begin
  FAborted := True;
  if TransitionThread <> nil then
    TransitionThread.WaitEvent.SetEvent;
end;

function TTETransitionDevice.AllowTransition: Boolean;
begin
  Result :=
    (not TransitionIsDisabled(TransitionToUse, False)) and
    ((Data = nil) or ((Data.Width > 0) and (Data.Height > 0)));
end;

function TTETransitionDevice.CheckAbort(CheckTimer: Boolean): Boolean;
var
  Msg: TMsg;
begin
  Result :=
    Aborted or
    ((TransitionThread <> nil) and TransitionThread.Terminated);
  if not Result then
  begin
    if(TransitionThread = nil)                    and
      (
        DelegateTransition.AbortOnClick  or
        DelegateTransition.AbortOnEscape or
        Assigned(DelegateTransition.OnAbortQuery)
      )                                           then
    begin
      if(not CheckTimer) or (Assigned(Data.AbortChrono) and
        (Data.AbortChrono.Milliseconds > DelegateTransition.MinAbortInterval)) then
      begin
        if DelegateTransition.AbortOnClick and (Data.DeviceWnd <> 0) then
          Result :=
            PeekMessage(Msg, Data.DeviceWnd, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE);

        if(not Result) and DelegateTransition.AbortOnEscape then
          while(not Result) and
                PeekMessage(Msg, 0, WM_KEYDOWN, WM_KEYDOWN, PM_REMOVE) do
            Result := Msg.wParam = VK_ESCAPE;

        if(not Result) and Assigned(DelegateTransition.OnAbortQuery) then
          DelegateTransition.OnAbortQuery(Self, DelegateTransition, Result);

        if Assigned(Data.AbortChrono) then
        begin
          Data.AbortChrono.Reset;
          Data.AbortChrono.Start;
        end;
      end;
    end;
  end;
  if Result then
    FAborted := True;
end;

constructor TTETransitionDevice.Create;
begin
  FTransition            := nil;
  FTransitionThread      := nil;
  DelegateTransition     := nil;
  Data                   := nil;
  FreeDelegateTransition := True;
  FAborted               := False;
  FExecuting             := False;
  FUsingThread           := False;
  RenderSrcFrame         := False;
  RenderDstFrame         := False;
  AllowAbort             := True;
  InitializeCriticalSection(CSThread);
  InitializeCriticalSection(CSBitmap);
end;

destructor TTETransitionDevice.Destroy;
begin
  if Assigned(Data) then
    Finalize;

  DeleteCriticalSection(CSThread);
  DeleteCriticalSection(CSBitmap);
  
  if Assigned(DelegateTransition) then
    FreeAndNil(DelegateTransition);

  inherited;
end;

function TTETransitionDevice.AvoidScrolling: Boolean;
begin
  Result := False;
end;

procedure TTETransitionDevice.DoCompleteExecution(TransitionChrono: TTEChrono);
var
  CurrentFrame: Longint;
  P: TPoint;
  LastFrame: Integer;
begin
  TransitionChrono.Start;
  LastFrame := Data.PassFrames;
  if Data.FirstFrame <> -1
  then CurrentFrame := Data.FirstFrame
  else
  begin
    if Data.PassRenderSrcFrame
    then
    begin
      Dec(LastFrame);
      CurrentFrame := 0;
    end
    else CurrentFrame := 1;
  end;

  while CurrentFrame <= LastFrame do
  begin
    if CheckAbort(True) then
      break;
    Inc(Data.TotalFrameIndex);
    {$ifdef LogTiming}
    if Assigned(DelegateTransition.Log) then
      DelegateTransition.Log.NewItem;
    {$endif LogTiming}
    if Assigned(Data.DeviceCanvas)
    then
    begin
      OffsetWindowOrgEx(Data.DeviceCanvas.Handle,
        -Data.DeviceCanvasOrgOff.X, -Data.DeviceCanvasOrgOff.Y, P);
      try
        if Data.PassRenderSrcFrame and (CurrentFrame = 0)
        then BitBlt(Data.DeviceCanvas.Handle, 0, 0, Data.Width, Data.Height,
               Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy)
        else if Data.PassRenderDstFrame and (CurrentFrame > Data.Frames)
        then BitBlt(Data.DeviceCanvas.Handle, 0, 0, Data.Width, Data.Height,
               Data.DstBmp.Canvas.Handle, 0, 0, cmSrcCopy)
        else
        begin
          DelegateTransition.ExecuteFrame(Data, CurrentFrame, 1, CurrentFrame-1);
          UpdateDevice(TransitionChrono);
        end;
        if Data.ExternalTiming then
          GetExtTimingData(True);
      finally
        SetWindowOrgEx(Data.DeviceCanvas.Handle, P.X, P.Y, nil);
      end;
    end
    else DelegateTransition.ExecuteFrame(Data, CurrentFrame, 1, CurrentFrame-1);
    {$ifdef LogTiming}
    if Assigned(DelegateTransition.Log) then
    begin
      with DelegateTransition.Log.CurrentItem^ do
      begin
        LogFrame          := CurrentFrame;
        LogStep           := 1;
        LogTransitionTime := TransitionChrono.Milliseconds;
        LogStepTime       := LogTransitionTime - DelegateTransition.Log.LastTransitionTime;
        LogWorkTime       := LogStepTime;
        LogSleepTime      := 0;
        LogSleepPrecision := 0;
        LogInterval       := 0;
        if DelegateTransition.Log.ChronoExtra.Milliseconds > 0 then
        begin
          LogExTime       := DelegateTransition.Log.ChronoExtra.Milliseconds;
          DelegateTransition.Log.ChronoExtra.Reset;
        end;
      end;
    end;
    {$endif LogTiming}
    Inc(CurrentFrame);
  end;
  TransitionChrono.Pause;
end;

procedure TTETransitionDevice.DoTimedExecution(TransitionChrono: TTEChrono);
var
  LastExecutedFrame,
  CurrentFrame,
  LastFrame,
  Milliseconds: Longint;
  Step,
  LastWorkTime,
  LastWorkTimeBak,
  StepStartTime,
  StepStartTimeBak,
  Interval,
  IntervalBak,
  SleepPrec1,
  SleepPrec2: Single;
  P: TPoint;
begin
  LastFrame         := Data.PassFrames;
  LastExecutedFrame := 0;
  SleepPrec1        := 0;
  SleepPrec2        := 0;

  if Data.ExternalTiming
  then
  begin
    Milliseconds  := GetExtTimingData(False);
    Interval      := Milliseconds;
    LastWorkTime  := Interval;
    StepStartTime := -Interval;
  end
  else
  begin
    Interval      := DelegateTransition.Milliseconds / (Data.PassFrames + 1);
    LastWorkTime  := Interval;
    StepStartTime := 0;
  end;

  if Data.FirstFrame <> -1
  then Step := Data.FirstFrame
  else
  begin
    if Data.PassRenderSrcFrame
    then
    begin
      Step := 0;
      Dec(LastFrame);
    end
    else
    begin
      if Data.ExternalTiming
      then
      begin
        NextFrame(True, 0, DelegateTransition.Milliseconds,
          0, TransitionChrono, Interval, LastWorkTime, StepStartTime,
          Step, SleepPrec1, SleepPrec2);
      end
      else Step := 1;
    end;
  end;

  CurrentFrame := Round(Step);
  if Step = 0 then
    Step := 1;

  TransitionChrono.Start;
  while CurrentFrame <= LastFrame do
  begin
    if CheckAbort(True) then
      break;
    Inc(Data.TotalFrameIndex, Round(Step));
    {$ifdef LogTiming}
    if Assigned(DelegateTransition.Log) then
      DelegateTransition.Log.NewItem;
    {$endif LogTiming}
    if Assigned(Data.DeviceCanvas) then
    begin
      OffsetWindowOrgEx(Data.DeviceCanvas.Handle,
        -Data.DeviceCanvasOrgOff.X, -Data.DeviceCanvasOrgOff.Y, P);
      try
        if CurrentFrame = 0
        then BitBlt(Data.DeviceCanvas.Handle, 0, 0, Data.Width, Data.Height,
               Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy)
        else if Data.PassRenderDstFrame and (CurrentFrame > Data.Frames)
        then BitBlt(Data.DeviceCanvas.Handle, 0, 0, Data.Width, Data.Height,
               Data.DstBmp.Canvas.Handle, 0, 0, cmSrcCopy)
        else
        begin
          DelegateTransition.ExecuteFrame(
            Data,
            CurrentFrame,
            CurrentFrame - LastExecutedFrame,
            LastExecutedFrame);
          if Assigned(Data.DeviceCanvas) then
            UpdateDevice(TransitionChrono);
        end;
      finally
        SetWindowOrgEx(Data.DeviceCanvas.Handle, P.X, P.Y, nil);
      end;
    end
    else DelegateTransition.ExecuteFrame(
           Data,
           CurrentFrame,
           CurrentFrame - LastExecutedFrame,
           LastExecutedFrame);
    if Aborted then
      break;
    LastExecutedFrame := CurrentFrame;

    if Data.ExternalTiming
    then
    begin
      IntervalBak      := Interval;
      LastWorkTimeBak  := LastWorkTime;
      StepStartTimeBak := StepStartTime;
      repeat
        Milliseconds := GetExtTimingData(True);
        NextFrame(True, CurrentFrame, DelegateTransition.Milliseconds,
          Milliseconds, TransitionChrono, Interval, LastWorkTime, StepStartTime,
          Step, SleepPrec1, SleepPrec2);
        if(Round(Step) = 0) and (Milliseconds < DelegateTransition.Milliseconds) then
        begin
          Interval      := IntervalBak;
          LastWorkTime  := LastWorkTimeBak;
          StepStartTime := StepStartTimeBak;
        end;
      until(Round(Step) > 0) or (Milliseconds > DelegateTransition.Milliseconds);
    end
    else
    begin
      Milliseconds := Round(TransitionChrono.Milliseconds);
      NextFrame(True, CurrentFrame, DelegateTransition.Milliseconds,
        Milliseconds, TransitionChrono, Interval, LastWorkTime, StepStartTime,
        Step, SleepPrec1, SleepPrec2);
    end;

    CurrentFrame := CurrentFrame + Round(Step);
  end;
  TransitionChrono.Pause;
end;

procedure TTETransitionDevice.Execute(WaitForCompletion: Boolean = True);
begin
  if TransitionToUse = nil then
    raise ETransitionEffectError.Create(rsTEDevTrIsNil);

  if(not WaitForCompletion) and (not IsThreadSafe) then
    raise ETransitionEffectError.Create(rsTEDevNotThreadSafe);

  if(not WaitForCompletion) and
    (not(tetiThreadSafe in DelegateTransition.GetInfo(Self))) then
    raise ETransitionEffectError.Create(rsTETransNotThreadSafe);

  if Assigned(Transition.FOnBeforeTransition) then
    Transition.FOnBeforeTransition(Self);

  if WaitForCompletion
  then
  begin
    try
      if AllowTransition then
      begin
        FAborted   := False;
        FExecuting := True;
        try
          CustomExecute;
        finally
          FExecuting := False;
        end;
      end;
    finally
      if Assigned(Transition.FOnAfterTransition) then
      begin
        AllowAbort := False;
        try
          Transition.FOnAfterTransition(Self);
        finally
          AllowAbort := True;
        end;
      end;
    end;
  end
  else
  begin
    if(TransitionThread = nil) and AllowTransition
    then
    begin
      EnterCriticalSection(CSThread);
      try
        FUsingThread      := True;
        FAborted          := False;
        PostponedOnEnd    := False;
        FTransitionThread := TTETransitionThread.Create(Self);
      finally
        LeaveCriticalSection(CSThread);
      end;
    end
    else
    begin
      if Assigned(Transition.FOnAfterTransition) then
      begin
        AllowAbort := False;
        try
          Transition.FOnAfterTransition(Self);
        finally
          AllowAbort := True;
        end;
      end;
    end;
  end;
end;

procedure TTETransitionDevice.OnTransitionThreadTerminated;
begin
  try
    EnterCriticalSection(CSThread);
    try
        FreeAndNil(FTransitionThread);
        FUsingThread := False;
    finally
      LeaveCriticalSection(CSThread);
    end;
  finally
    AllowAbort := False;
    try
      if Assigned(Transition.FOnEndTransition) and PostponedOnEnd then
        Transition.FOnEndTransition(Self);
      if Assigned(Transition.FOnAfterTransition) then
        Transition.FOnAfterTransition(Self);
    finally
      AllowAbort := True;
    end;
  end;
end;

function TTETransitionDevice.ExePassBegin(Pass: Integer;
  Pass2Chrono: TTEChrono): Boolean;
begin
  Result := False;
  Assert((Data.Bitmap = nil) or (Data.Bitmap.Width = DelegateTransition.GetBitmapsWidth(Data)));
  Assert((Data.SrcBmp = nil) or (Data.SrcBmp.Width = DelegateTransition.GetBitmapsWidth(Data)));
  Assert((Data.DstBmp = nil) or (Data.DstBmp.Width = DelegateTransition.GetBitmapsWidth(Data)));

  Data.Pass := Pass;
  if DelegateTransition.Pass2Options.Reversed and (Data.PassCount = 2) then
    DelegateTransition.Reversed := not DelegateTransition.Reversed;

  if Assigned(DelegateTransition.FOnStartTransition) and (Pass=1) then
  begin
    if (TransitionThread <> nil) then
      EnterCriticalSection(TransitionThread.CSSync);
    try
      if not Aborted then
      begin
        if (TransitionThread <> nil)
        then TransitionThread.Synchronize(TransitionThread.OnStart)
        else DelegateTransition.FOnStartTransition(Self);
      end;
    finally
      if (TransitionThread <> nil) then
        LeaveCriticalSection(TransitionThread.CSSync);
    end;
  end;

  if(TransitionThread = nil)                   and
    (
      DelegateTransition.AbortOnClick  or
      DelegateTransition.AbortOnEscape or
      Assigned(DelegateTransition.OnAbortQuery)
    )                                          then
    Data.AbortChrono := TTEChrono.Create;
  try
    if Assigned(Data.AbortChrono) then
      Data.AbortChrono.Start;
    if not CheckAbort(False) then
    begin
      DelegateTransition.Initialize(Data, Data.Frames);
      TransitionInitialized;
      if(Pass2Chrono <> nil) and (Pass = 1) then
        Pass2Chrono.Start;
      if Data.Frames > 0 then
      begin
        Result := True;
        {$ifdef LogTiming}
        if Assigned(DelegateTransition.Log) then
          DelegateTransition.Log.SetSize(Data.TotalFrames);
        {$endif LogTiming}
      end;
    end;
  except
    ExePassEnd(Pass);
  end;
end;

procedure TTETransitionDevice.ExePassEnd(Pass: Integer);
var
  P: TPoint;
begin
  try
    DelegateTransition.Finalize(Data);
  finally
    FreeAndNil(Data.AbortChrono);
    if(Data.DeviceWnd <> 0) and (Data.Palette <> 0) then
      SelectPalette(Data.DeviceCanvas.Handle, Data.Palette, True);
    if Data.DstBmp <> nil then
    begin
      if(Data.AlwaysShowLastFrame)                 and
        ((not Aborted) or (Pass = Data.PassCount)) then
      begin
        OffsetWindowOrgEx(Data.DeviceCanvas.Handle,
          -Data.DeviceCanvasOrgOff.X, -Data.DeviceCanvasOrgOff.Y, P);
        try
          BitBlt(Data.DeviceCanvas.Handle, 0, 0, Data.Width, Data.Height,
            Data.DstBmp.Canvas.Handle, 0, 0, cmSrcCopy);
        finally
          SetWindowOrgEx(Data.DeviceCanvas.Handle, P.X, P.Y, nil);
        end;
      end;
    end;

    if Assigned(DelegateTransition.FOnEndTransition) and
      ((Pass=2) or (Data.PassCount = 1))            then
    begin
      if not Aborted then
      begin
        if TransitionThread <> nil
        then PostponedOnEnd := True
        else
        begin
          AllowAbort := False;
          try
            DelegateTransition.FOnEndTransition(Self);
          finally
            AllowAbort := True;
          end;
        end;
      end;
    end;
  end;
end;

procedure TTETransitionDevice.ExePass(Pass: Integer; Pass2Chrono: TTEChrono;
  TotalMilliseconds: Integer);
var
  TransitionChrono: TTEChrono;
  OutOfTime: Boolean;
begin
  if ExePassBegin(Pass, Pass2Chrono) then
  begin
    try
      TransitionChrono := TTEChrono.Create;
      try
        OutOfTime := False;
        if(Pass2Chrono <> nil) and (Pass = 2) then
        begin
          Pass2Chrono.Pause;
          if Round(Pass2Chrono.Milliseconds) < TotalMilliseconds
          then DelegateTransition.Milliseconds :=
                 TotalMilliseconds - Round(Pass2Chrono.Milliseconds)
          else OutOfTime := True;
        end;

        if not OutOfTime then
        begin
          if(DelegateTransition.Milliseconds > 0) and
            (tetiMillisecondsCapable in DelegateTransition.GetInfo(Self))
          then DoTimedExecution   (TransitionChrono)
          else DoCompleteExecution(TransitionChrono);
        end;
        {$ifdef LogTiming}
        if Assigned(DelegateTransition.Log) then
          DelegateTransition.Log.SaveLog(DelegateTransition, Data,
            TransitionChrono.Milliseconds);
        {$endif LogTiming}
      finally
        TransitionChrono.Free;
      end;
    finally
      ExePassEnd(Pass);
    end;
  end;
end;

procedure TTETransitionDevice.Finalize;
begin
  Assert(Data <> nil);
  EnterCriticalSection(CSBitmap);
  try
    FreeAndNil(Data);
  finally
    LeaveCriticalSection(CSBitmap);
  end;
  if Assigned(DelegateTransition) and FreeDelegateTransition then
    FreeAndNil(DelegateTransition);
end;

procedure TTETransitionDevice.GetOffScreenBmp(var OldPalette: hPalette);
var
  aux: TBitmap;
  TransitionInfo: TTETransitionInfo;
begin
  TransitionInfo := DelegateTransition.GetInfo(Self);
  Assert(
    not(
         (tetiNeedOffScreenBmp in TransitionInfo) and
         (not(tetiOffScreenBmpCapable in TransitionInfo))
       )
    );
  aux := nil;
  if(tetiOffScreenBmpCapable in TransitionInfo) and
    ((tetiNeedOffScreenBmp in TransitionInfo) or NeedOffScreenBmp) then
  begin
    if(Data.SrcBmp <> nil) and
      (tetiUseSrcAsOffScreenBmp in TransitionInfo)
    then aux := Data.SrcBmp
    else
    begin
      if Data.Pass = 1 then
      begin
        aux := TBitmap.Create;
        aux.Canvas.Lock;
        AdjustBmpForTransition(aux, 0,
          DelegateTransition.GetBitmapsWidth(Data), Data.Height,
          DelegateTransition.GetPixelFormat(Self));
      end;

      if Data.DstBmp <> nil
      then Data.Palette := Data.DstBmp.Palette
      else if Data.SrcBmp <> nil
      then Data.Palette := Data.SrcBmp.Palette
      else Data.Palette := 0;
      if Data.Palette <> 0
      then
      begin
        OldPalette := SelectPalette(Data.DeviceCanvas.Handle,
          Data.Palette, True);
        RealizePalette(Data.DeviceCanvas.Handle);
      end
      else OldPalette := 0;
      if HasPalette and (Data.DstBmp <> nil) then
        aux.Palette := CopyPalette(Data.DstBmp.Palette);
      if Assigned(Data.SrcBmp) and
        (
          (tetiNeedSrcBmp in TransitionInfo) or
          (not(tetiNeedOffScreenBmp in TransitionInfo))
        ) then
        BitBlt(aux.Canvas.Handle, 0, 0, Data.Width, Data.Height,
          Data.SrcBmp.Canvas.Handle, 0, 0, cmSrcCopy);
    end;
    EnterCriticalSection(CSBitmap);
    try
      if not(tetiUseSrcAsOffScreenBmp in TransitionInfo) then
      begin
        if Assigned(Data.Bitmap) and (Data.Bitmap <> Data.SrcBmp) then
        begin
          Data.Bitmap.Canvas.Unlock;
          Data.Bitmap.Free;
        end;
      end;
      Data.Bitmap := aux;
    finally
      LeaveCriticalSection(CSBitmap);
    end;
  end;
end;

procedure TTETransitionDevice.Get2ndPassBmp;
begin
  Assert(Data <> nil);
  Pass2Image := TBitmap.Create;
  Pass2Image.Canvas.Lock;
  GetSolidColorBmp(Pass2Image, TransitionToUse.GetBitmapsWidth(Data),
    Data.Height, TransitionToUse.Pass2Options.SolidColor, Data.Palette,
    Data.PixelFormat);
end;

function TTETransitionDevice.GetCurrentFrameBmp(
  var CriticalSectionEntered: Boolean): TBitmap;
begin
  Result                 := nil;
  CriticalSectionEntered := False;
  if Assigned(FTransitionThread)
  then
  begin
    CriticalSectionEntered := True;
    EnterCriticalSection(CSBitmap);
    try
      if FTransitionThread.Executing or (not FTransitionThread.Executed) then
      begin
        if Assigned(Data) and Assigned(Data.CurFrameBmp)
        then Result := Data.CurFrameBmp
        else Result := SrcImage;
      end;
    finally
      if Result = nil then
      begin
        LeaveCriticalSection(CSBitmap);
        CriticalSectionEntered := False;
      end;
    end;
  end
  else Result := SrcImage;
end;

procedure TTETransitionDevice.Initialize;
var
  RandomDirection: TTEEffectDirection;
begin
  Assert(Data = nil);

  if not Assigned(DelegateTransition) then
  begin
    // Create transition's copy
    if not TransitionIsDisabled(Transition, True)
    then
    begin
      DelegateTransition := Transition.GetDelegate(Self, True);
      DelegateTransition.FDelegatedFrom := Transition;
      if DelegateTransition.Direction = tedRandom then
      begin
        repeat
          RandomDirection :=
            TTEEffectDirection(Random(Integer(High(TTEEffectDirection))+1));
        until
          (RandomDirection in DelegateTransition.AllowedDirections) and
          (RandomDirection <> tedRandom);
        DelegateTransition.Direction := RandomDirection;
      end;
    end
    else
    begin
      DelegateTransition := TFlickerFreeTransition.Create(nil);
      DelegateTransition.Assign(Transition);
    end;
    DelegateTransition.FOnStartTransition := Transition.FOnStartTransition;
    DelegateTransition.FOnEndTransition   := Transition.FOnEndTransition;
    DelegateTransition.FOnAbortQuery      := Transition.FOnAbortQuery;
  end;

  Data := TTETransitionData.Create;
  Data.AllowDeviceUpdate := True;
  Data.Bitmap            := nil;
  Data.Device            := Self;
  Data.DeviceCanvas      := nil;
  Data.DeviceWnd         := 0;
  Data.DstBmp            := nil;
  Data.Height            := 0;
  Data.Palette           := 0;
  Data.Pass              := 1;
  Data.PassCount         := DelegateTransition.Passes(Self);
  Data.PixelFormat       := DelegateTransition.GetPixelFormat(Self);
  Data.ExternalTiming    := False;
  Data.SrcBmp            := nil;
  Data.Width             := 0;
  Data.TotalFrameIndex   := 0;
  if tetiUseDirtyRects in DelegateTransition.GetInfo(Self) then
    Data.DirtyRects := TTEDirtyRects.Create;
end;

class function TTETransitionDevice.IsThreadSafe: Boolean;
begin
  Result := False;
end;

procedure TTETransitionDevice.NextFrame(InternalLoop: Boolean;
  CurrentFrame, Milliseconds, ElapsedTime: Longint; Chrono: TTEChrono;
  var Interval, LastWorkTime, StepStartTime, Step, SleepPrec1, SleepPrec2: Single);

  procedure GoToBed(ms, SleepPrecision: Single);
  var
    aux: Integer;
  begin
    if Assigned(FTransitionThread)
    then
    begin
      while(Data.SleepChrono.Milliseconds < ms) and (not CheckAbort(False)) do
      begin
        aux := Trunc(ms - Data.SleepChrono.Milliseconds - SleepPrecision);
        if aux > 1
        then TransitionThread.WaitEvent.WaitFor(aux)
        else TransitionThread.WaitEvent.WaitFor(1);
      end;
    end
    else
    begin
      while(Data.SleepChrono.Milliseconds < ms) and (not CheckAbort(False))
        do; {Nothing}
    end;
  end;

  procedure CalculateParameters(CurrentFrame: Longint;
    TotalMilliseconds, TransitionTime, LastStepStartTime, SleepTime: Double;
    var Interval, WorkTime, Step: Single);
  var
    FramesToEnd: Longint;
    TimeToGo: Single;
  begin
    WorkTime    := (TransitionTime - LastStepStartTime) - SleepTime;
    FramesToEnd := (Data.Frames + 1) - CurrentFrame;
    TimeToGo    := TotalMilliseconds - TransitionTime;
    if TimeToGo <= 0
    then Step := FramesToEnd
    else
    begin
      if WorkTime <> 0
      then Step := FramesToEnd / ((TotalMilliseconds - TransitionTime) / WorkTime)
      else Step := FramesToEnd /  (TotalMilliseconds - TransitionTime);
      if Step < 1 then
        Step := 1;
    end;
    Interval := (TotalMilliseconds - TransitionTime) / (FramesToEnd / Step);
  end;

var
  LastStepStartTime,
  ms,
  SleepTime,
  ChronoMilliseconds: Single;
begin
  {$ifdef LogTiming}
  ms        := 0;
  {$endif LogTiming}
  SleepTime := 0;
  if InternalLoop
  then
  begin
    if Data.ExternalTiming
    then
    begin
      LastStepStartTime := StepStartTime;
      LastWorkTime      := ElapsedTime - LastStepStartTime;
      Interval          := LastWorkTime;
      if Milliseconds > ElapsedTime
      then Step         :=
             (Data.TotalFrames - Data.TotalFrameIndex) /
             ((Milliseconds - ElapsedTime) / Interval)
      else Step := Data.TotalFrames - Data.TotalFrameIndex + 1;
      StepStartTime     := ElapsedTime;
    end
    else
    begin
      if Assigned(Chrono)
      then ChronoMilliseconds := Chrono.Milliseconds
      else ChronoMilliseconds := ElapsedTime;
      ms := Interval - Data.LastUpdateTime - (ChronoMilliseconds - StepStartTime);
      if ms > 0 then
      begin
        Data.SleepChrono.Start;
        GoToBed(ms, (SleepPrec1 + SleepPrec2) / 2);
        Data.SleepChrono.Pause;
        SleepPrec2 := SleepPrec1;
        SleepPrec1 := Data.SleepChrono.Milliseconds - ms;
        if SleepPrec1 < 0 then
          SleepPrec1 := SleepPrec2;
      end;
      SleepTime := Data.SleepChrono.Milliseconds;
      Data.SleepChrono.Reset;

      LastStepStartTime := StepStartTime;
      if Assigned(Chrono)
      then ChronoMilliseconds := Chrono.Milliseconds
      else ChronoMilliseconds := ElapsedTime;
      CalculateParameters(CurrentFrame, Milliseconds,
        ChronoMilliseconds + ((SleepPrec1 + SleepPrec2) / 2),
        LastStepStartTime, SleepTime - ((SleepPrec1 + SleepPrec2) / 2),
        Interval, LastWorkTime, Step);
      if Assigned(Chrono)
      then ChronoMilliseconds := Chrono.Milliseconds
      else ChronoMilliseconds := ElapsedTime;
      StepStartTime := ChronoMilliseconds;
    end;
  end
  else
  begin
    LastStepStartTime := StepStartTime;
    CalculateParameters(CurrentFrame, Milliseconds,
      ElapsedTime{ + Chrono.Milliseconds}, LastStepStartTime, SleepTime, Interval,
      LastWorkTime, Step);
    StepStartTime := ElapsedTime{ + Chrono.Milliseconds};
  end;

  {$ifdef LogTiming}
  if Assigned(DelegateTransition.Log) then
  begin
    with DelegateTransition.Log.CurrentItem^ do
    begin
      LogFrame          := Round(CurrentFrame);
      LogStep           := Step;
      LogTransitionTime := ElapsedTime;
      LogStepTime       := StepStartTime - LastStepStartTime;
      LogWorkTime       := (StepStartTime - LastStepStartTime) - SleepTime;
      LogSleepTime      := SleepTime;
      LogSleepPrecision := SleepTime - ms;
      LogInterval       := Interval;
      if DelegateTransition.Log.ChronoExtra.Milliseconds > 0 then
      begin
        LogExTime       := DelegateTransition.Log.ChronoExtra.Milliseconds;
        DelegateTransition.Log.ChronoExtra.Reset;
      end;
    end;
  end;
  {$endif LogTiming}
end;

procedure TTETransitionDevice.SetTransition(const Value: TTransitionEffect);
begin
  if Data <> nil then
    raise ETransitionEffectError.Create(rsTETransitionBusy);

  FTransition := Value;
end;

class function TTETransitionDevice.TransitionIsDisabled(
  Transition: TTransitionEffect; NoFlickerFreeWhenDisabled: Boolean): Boolean;
begin
  Result := not Transition.Enabled;
end;

function TTETransitionDevice.TransitionToUse: TTransitionEffect;
begin
  if DelegateTransition <> nil
  then Result := DelegateTransition
  else Result := FTransition;
end;

procedure TTETransitionDevice.UpdateDevice(TransitionChrono: TTEChrono);
var
  Left,
  Top,
  Width,
  Height,
  i: Integer;
  StartTime: Double;
begin
  if UsingThread                          and
    (Data.DeviceWnd <> 0)                 and
    (not IsWindowVisible(Data.DeviceWnd)) then
    FAborted := True; 

  if(not Aborted) and Data.AllowDeviceUpdate and Assigned(Data.Bitmap) then
  begin
    if Assigned(TransitionChrono)
    then StartTime := TransitionChrono.Milliseconds
    else StartTime := 0;

    Left   := Data.UpdateRect.Left;
    Top    := Data.UpdateRect.Top;
    Width  := Data.UpdateRect.Right  - Data.UpdateRect.Left;
    Height := Data.UpdateRect.Bottom - Data.UpdateRect.Top;

    if{(not UsingThread) and }(not IsRectEmpty(Data.UnUpdateRect)) then
      ExcludeClipRect(
        Data.DeviceCanvas.Handle,
        Data.UnUpdateRect.Left,
        Data.UnUpdateRect.Top,
        Data.UnUpdateRect.Right,
        Data.UnUpdateRect.Bottom);

    if Assigned(Data.DirtyRects) and
      (Data.DirtyRects.Count > 1)
    then
    begin
      for i := 0 to Data.DirtyRects.Count-1 do
        with Data.DirtyRects[i] do
        begin
          BitBlt(Data.DeviceCanvas.Handle, Left, Top, Right - Left,
            Bottom - Top, Data.Bitmap.Canvas.Handle, Left, Top,
            cmSrcCopy);
        end;
    end
    else
    begin
      BitBlt(Data.DeviceCanvas.Handle, Left, Top, Width, Height,
        Data.Bitmap.Canvas.Handle, Left, Top, cmSrcCopy);
    end;

    if{(not UsingThread) and} (not IsRectEmpty(Data.UnUpdateRect)) then
      SelectClipRgn(Data.DeviceCanvas.Handle, 0);

    if Assigned(TransitionChrono)
    then Data.LastUpdateTime := TransitionChrono.Milliseconds - StartTime
    else Data.LastUpdateTime := 0;
    {$ifdef LogTiming}
    if Assigned(DelegateTransition.Log) then
      DelegateTransition.Log.CurrentItem^.LogUpdateTime := Data.LastUpdateTime;
    {$endif LogTiming}
  end;

  Data.UnUpdateRectBak := Data.UnUpdateRect;
  Data.UpdateRectBak   := Data.UpdateRect;
  Data.UnUpdateRect    := Rect(0, 0, 0, 0);
  Data.UpdateRect      := Rect(0, 0, 0, 0);
  if(Data.DirtyRects <> nil) and Data.DirtyRects.AutoClear then
    Data.DirtyRects.Clear;

  if(not Aborted)       and
    (Data.Bitmap = nil) and
    (Data.CurFrameBmp = Data.SrcBmp) then
    Data.CurFrameBmp := nil;
end;

function TTETransitionDevice.GetRenderWndHandle: HWnd;
begin
  raise ETransitionEffectError.Create(rsTEDevNotThreadSafe);
end;

function TTETransitionDevice.TransitionThread: TTETransitionThread;
begin
  Result := FTransitionThread;
end;

function TTETransitionDevice.NeedOffScreenBmp: Boolean;
begin
  Result := False;
end;

// Returns whether the transition output is clipped at startup
function TTETransitionDevice.Clipped: Boolean;
begin
  Result := False;
end;

// Returns whether the transition output clipping state may change during execution
function TTETransitionDevice.DynamicClipping: Boolean;
begin
  Result := False;
end;

function TTETransitionDevice.GetExtTimingData(FrameRendered: Boolean): Integer;
begin
  Result := 0;
end;

function TTETransitionDevice.IsRGB: Boolean;
begin
  Result := PixelFormat in [pf15bit, pf16bit, pf24bit, pf32bit];
end;

procedure TTETransitionDevice.TransitionInitialized;
begin
  if Data.Pass = 1 then
  begin
    Data.TotalFrames := Data.Frames * Data.PassCount;
    if RenderSrcFrame then
      Inc(Data.TotalFrames);
    if RenderDstFrame then
      Inc(Data.TotalFrames);
  end;
  Data.PassRenderSrcFrame := RenderSrcFrame and (Data.Pass = 1);
  if Data.PassRenderSrcFrame
  then Data.PassFrames := Data.Frames + 1
  else Data.PassFrames := Data.Frames;
  Data.PassRenderDstFrame :=
    RenderDstFrame and ((Data.Pass = 2) or (Data.PassCount = 1));
  if Data.PassRenderDstFrame then
    Inc(Data.PassFrames);
end;

function TTETransitionDevice.GetDelegateTransition(
  Original: TTransitionEffect;
  const ReturnCopy: Boolean): TTransitionEffect;
begin
  if ReturnCopy
  then
  begin
    Result := TTransitionEffectClass(Original.ClassType).Create(nil);
    Result.Assign(Original);
  end
  else Result := Original;
end;

{ TTETransitionThread }

constructor TTETransitionThread.Create(ADevice: TTETransitionDevice);
begin
  inherited Create(True);

  NotifyTermination := True;
  FreeOnTerminate   := False;
  Device            := ADevice;
  InitializeCriticalSection(CSSync);
  WaitEvent := TSimpleEvent.Create;
  Executing := False;
  Executed  := False;

  if Assigned(Device.SrcImage  ) then
  begin
    Device.SrcImage  .Canvas.Handle; // Validates handle before locking
    Device.SrcImage  .Canvas.Lock;
  end;
  if Assigned(Device.Pass2Image) then
  begin
    Device.Pass2Image.Canvas.Handle; // Validates handle before locking
    Device.Pass2Image.Canvas.Lock;
  end;
  if Assigned(Device.DstImage  ) then
  begin
    Device.DstImage  .Canvas.Handle; // Validates handle before locking
    Device.DstImage  .Canvas.Lock;
  end;

  Resume;
end;

destructor TTETransitionThread.Destroy;
begin
  if Assigned(Device.SrcImage  ) then
    Device.SrcImage  .Canvas.Unlock;
  if Assigned(Device.Pass2Image) then
    Device.Pass2Image.Canvas.Unlock;
  if Assigned(Device.DstImage  ) then
    Device.DstImage  .Canvas.Unlock;

  DeleteCriticalSection(CSSync);
  WaitEvent.Free;

  inherited;
end;

procedure TTETransitionThread.OnStart;
begin
  Device.DelegateTransition.FOnStartTransition(Device);
end;

procedure TTETransitionThread.Execute;
begin
  EnterCriticalSection(Device.csThread);
  LeaveCriticalSection(Device.csThread);
  Executing         := True;
  Device.FExecuting := True;
  try
    try
      try
        Device.CustomExecute;
      finally
        if Assigned(Device.SrcImage  ) then
          Device.SrcImage  .Canvas.Unlock;
        if Assigned(Device.Pass2Image) then
          Device.Pass2Image.Canvas.Unlock;
        if Assigned(Device.DstImage  ) then
          Device.DstImage  .Canvas.Unlock;
      end;
    except
      on Exception do;    
    end;
  finally
    Device.FExecuting := False;
    Executing         := False;
    Executed          := True;
    EnterCriticalSection(Device.csThread);
    LeaveCriticalSection(Device.csThread);
  end;
end;

procedure TTETransitionThread.DoTerminate;
begin
  if NotifyTermination then
    PostMessage(Device.GetRenderWndHandle, CM_TETHREADTERMINATED, 0, 0);

  try
    inherited;
  except
  end;
end;

procedure TTETransitionThread.DebugString(const Text: String);
begin
  OutputDebugString(PChar(Format('(%x) %s', [ThreadID, Text])));
end;

initialization

  Randomize;
  TERegisterTransition(TFlickerFreeTransition);

  FlickerFreeTransition := TFlickerFreeTransition.Create(nil);
  FlickerFreeTransition.FlickerFreeWhenDisabled := True;

  TEGlobalDisabled       := False;
  TEGlobalDisabledStrict := False;
  OldTransition          := nil;
  NewTransition          := nil;

finalization

  TERegisteredTransitions.Free;
  FlickerFreeTransition.Free;

end.
