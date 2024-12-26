/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Rendering.pas                                  *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Rendering;

{$INCLUDE FormEffects.inc}

interface

uses
  System.Types,
  Winapi.Windows,
  System.SysUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.ComCtrls, Vcl.OleCtrls, Vcl.StdCtrls, Vcl.Forms,
  FormEffects.TypeHelpers, FormEffects.FormContainer, FormEffects.Utils.OS, FormEffects.Utils.ScrollBars,
  FormEffects.Utils.Rects, FormEffects.Utils.Forms
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Winapi.Windows.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.ComCtrls.Mocks
  , FormEffects.Vcl.StdCtrls.Mocks
  , FormEffects.Vcl.OleCtrls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

const // Form Effects messages
  // Thir party components interface
  FE_ID           = $41A2;
  FE_BASE         = CM_BASE + $0C4A;
  CM_FEPAINT      = FE_BASE + 0; // Paint client area to Form Effects' DC
  CM_FENCPAINT    = FE_BASE + 1; // Paint non client area to Form Effects' DC
  CM_FEFULLRENDER = FE_BASE + 2; // Paint whole control to Form Effects' DC

type
  TFEPaintCallback = reference to procedure(const WinControl: TWinControl; const DC: HDC);

const
  feAuto           = $00000000;
  fePaint          = $00000001;
  fePrint          = $00000002;
  feEmulate        = $00000003;
  feCallback       = $00000004;
  fePaintCopy      = $00000005;

  feThemed         = $10000000;
  feRefreshFocused = $20000000;
  feOwnCanvas      = $40000000;
  feNoRender       = $80000000;

{$IFDEF FORM_EFFECTS_TESTS}
function DoesAncestorHandle(const WinControl: TWinControl; const Selector: Integer): TClass;
function CompleteFlags(const WinControl: TWinControl; const Flags: DWORD): DWORD;
procedure GetRegControl(const Wnd: HWND; const WinControl: TWinControl;
  out Flags: DWORD; out NonClientCallback, ClientCallback: TFEPaintCallback); overload;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure RegisterControl(const ControlClass: TControlClass; const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil); overload; inline;
procedure RegisterControl(const ControlClassName: string; const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil); overload;

{$MESSAGE 'Need to test NCPrintControl'}
procedure NCPrintControl(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);

{$MESSAGE 'Need to test RenderWindowToDC'}
procedure RenderWindowToDC(const Wnd, StopWnd: HWND; const WinControl: TWinControl; const DC: HDC; const Rect: TRect;
  const ClientCoordinates, CheckVisibility, CheckRegion: Boolean);

implementation

uses
  Winapi.Messages, Winapi.ActiveX, Winapi.FlatSB,
  System.Classes, System.Math, System.Generics.Collections,
  Vcl.ToolWin, Vcl.Themes,
  FormEffects.Utils
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.ToolWin.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

{$REGION 'Internal definitions'}

const
  RCF_RENDERNC         = $00000001; // Do render the non-client area
  RCF_REFRESHNC        = $00000002; // Always refresh the non-client area
  RCF_REFRESHFOCUSEDNC = $00000004; // Refresh the non-client area only if the control is focused
  RCF_PRINTNC          = $00000008; // Render non-client area using WM_PRINT message
  RCF_PAINTNC          = $00000010; // Render non-client area using WM_PAINT message
  RCF_EMULNC           = $00000020; // Render non-client with custom code
  RCF_CALLBACKNC       = $00000040; // Render non-client area using a callback method
  RCF_THEMEDNC         = $00000080; // Render non-client area XP themes
  RCF_PAINTCOPYNC      = $00000100; // Render non-client area setting the csPaintCopy control state
  RCF_BENCPREPAINT     = $00000200; // Use BE_NCPAINT, then render non-client area
  RCF_BENCPAINT        = $00000400; // Render non-client area only using BE_NCPAINT
  RCF_BENCPOSTPAINT    = $00000800; // Render non-client area, then use BE_NCPAINT
  RCF_OWNCANVASNC      = $00001000; // Render non-client area in a separate bitmap

  RCF_BEFULLRENDER     = $00080000; // Renders whole window at once using CM_BEFULLRENDER

  RCF_RENDER           = $00100000; // Do render the client area
  RCF_REFRESH          = $00200000; // Always refresh the client area
  RCF_REFRESHFOCUSED   = $00400000; // Refresh the client area only if the control is focused
  RCF_PRINT            = $00800000; // Render client area using WM_PRINT message
  RCF_PAINT            = $01000000; // Render client area using WM_PAINT message
  RCF_EMUL             = $02000000; // Render client with custom code
  RCF_CALLBACK         = $04000000; // Render client area using a callback method
  RCF_PAINTCOPY        = $08000000; // Render client area setting the csPaintCopy control state
  RCF_BEPREPAINT       = $10000000; // Use BE_PAINT, then render client area
  RCF_BEPAINT          = $20000000; // Render client area only using BE_PAINT
  RCF_BEPOSTPAINT      = $40000000; // Render client area, then use BE_PAINT
  RCF_OWNCANVAS        = $80000000; // Render client area in a separate bitmap

  RCF_RENDERMASK       = $FFF00000;
  RCF_RENDERNCMASK     = $00001FFF;

type

{ TRegControl }

  TRegControl = record
  public
    Flags: DWORD;
    NonClientCallback: TFEPaintCallback;
    ClientCallback: TFEPaintCallback;

  public
    class function Create: TRegControl; overload; static; inline;
    class function Create(const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback): TRegControl; overload; static; inline;

  public
    procedure Assign(Source: TRegControl);
    procedure Clear; inline;
  end;

{ TRegControlsManager }

  TRegControlsManager = class
  strict private
    FRegControls: TDictionary<string, TRegControl>;

  strict private
    function NormalizeKey(const Name: string): string; inline;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;

  public
    procedure FindRegControl(const ControlClass: TClass; var RegControl: TRegControl); inline;
    procedure SaveRegControl(const ControlClassName: string; const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback); inline;
  end;

var
  RegControlsManager: TRegControlsManager = nil;

function GetRegControlsManager: TRegControlsManager; inline;
begin
  if RegControlsManager = nil then RegControlsManager := TRegControlsManager.Create;
  Result := RegControlsManager;
end;

function GetFlagsFromWindow(const Wnd: HWND): DWORD; inline;
begin
  if SendMessage(Wnd, CM_FEFULLRENDER, 0, FE_ID) = FE_ID then Exit(RCF_RENDER or RCF_RENDERNC or RCF_BEFULLRENDER);

  Result := 0;
  case SendMessage(Wnd, CM_FENCPAINT, 0, FE_ID) of
    FE_ID - 1: Result := RCF_RENDERNC or RCF_BENCPREPAINT;
    FE_ID    : Result := RCF_RENDERNC or RCF_BENCPAINT;
    FE_ID + 1: Result := RCF_RENDERNC or RCF_BENCPOSTPAINT;
  end;
  case SendMessage(Wnd, CM_FEPAINT, 0, FE_ID) of
    FE_ID - 1: Result := Result or RCF_RENDER or RCF_BEPREPAINT;
    FE_ID    : Result := Result or RCF_RENDER or RCF_BEPAINT;
    FE_ID + 1: Result := Result or RCF_RENDER or RCF_BEPOSTPAINT;
  end;
end;

function DoesAncestorHandle(const WinControl: TWinControl; const Selector: Integer): TClass;
asm
{$IF DEFINED(WIN64)}
  // Start
  // --> RCX    addres WinControl
  //     EDX    dynamic method Selector
  // <-- RAX    vmt of Class or null
  MOV       EAX,        EDX
  MOV       RDX,        [RCX]         // Get vmt of class of WinControl
  JMP       @@HaveVMT

@@OuterLoop:
  MOV       RDX,        [RDX]

@@HaveVMT:
  MOV       RDI,        [RDX].vmtDynamicTable
  TEST      RDI,        RDI
  JE        @@Parent
  MOVZX     RCX,        WORD PTR [RDI]
  PUSH      RCX
  ADD       RDI,        2
  REPNE     SCASW
  JE        @@Found
  POP       RCX

@@Parent:
  MOV       RDX,        [RDX].vmtParent
  TEST      RDX,        RDX
  JNE       @@OuterLoop
  MOV       RAX,        0
  JMP       @@Exit

@@Found:
  POP       RAX
  MOV       RAX,        RDX

@@Exit:
{$ENDIF ~ WIN64}
{$IF DEFINED(WIN32)}
  // Start
  // --> EAX    addres WinControl
  //     EDX    dynamic method Selector
  // <-- EAX    vmt of Class or null
  MOV       EAX,        [EAX]         // Get vmt of class of WinControl

  PUSH      EDI
  XCHG      EAX,        EDX
  JMP       @@HaveVMT

@@OuterLoop:
  MOV       EDX,        [EDX]

@@HaveVMT:
  MOV       EDI,        [EDX].vmtDynamicTable
  TEST      EDI,        EDI
  JE        @@Parent
  MOVZX     ECX,        WORD PTR [EDI]
  PUSH      ECX
  ADD       EDI,        2
  REPNE     SCASW
  JE        @@Found
  POP       ECX

@@Parent:
  MOV       EDX,        [EDX].vmtParent
  TEST      EDX,        EDX
  JNE       @@OuterLoop
  MOV       EAX,        0
  JMP       @@Exit

@@Found:
  POP       EAX
  MOV       EAX,        EDX

@@Exit:
  POP       EDI
{$ENDIF ~ WIN32}
end;

{$IFDEF FORM_EFFECTS_TESTS}
function CompleteFlags(const WinControl: TWinControl; const Flags: DWORD): DWORD;
{$ELSE ~ FORM_EFFECTS_TESTS}
function CompleteFlags(const WinControl: TWinControl; const Flags: DWORD): DWORD; inline;
{$ENDIF ~ FORM_EFFECTS_TESTS}
begin
  Result := Flags;

  if ((Result and RCF_PAINTCOPYNC) <> 0) or ((Result and RCF_PAINTCOPY) <> 0) then
  begin
    if WinControl.Focused then
    begin
      if (Result and RCF_PAINTCOPYNC) <> 0 then Result := (Result and not RCF_PAINTCOPYNC) or RCF_PRINT;
      if (Result and RCF_PAINTCOPY  ) <> 0 then Result := (Result and not RCF_PAINTCOPY  ) or RCF_PAINT;
    end;
  end;

  if
    (((Result and RCF_RENDERNC) <> 0) and ((Result and RCF_RENDERNCMASK) = RCF_RENDERNC)) or
    (((Result and RCF_RENDER  ) <> 0) and ((Result and RCF_RENDERMASK  ) = RCF_RENDER  ))
  then
  begin
    const ClassPrint = DoesAncestorHandle(WinControl, WM_PRINT);

    if ((Result and RCF_RENDER) <> 0) and ((Result and RCF_RENDERMASK) = RCF_RENDER) then
    begin
      if ClassPrint = nil then Result := Result or RCF_PAINT else Result := Result or RCF_PRINT;
    end;

    if ((Result and RCF_RENDERNC) <> 0) and ((Result and RCF_RENDERNCMASK) = RCF_RENDERNC) then
    begin
      const ClassNCPaint = DoesAncestorHandle(WinControl, WM_NCPAINT);

      if ClassNCPaint = nil then Result := Result or RCF_PRINTNC
      else
      begin
      {$IFDEF FORM_EFFECTS_TESTS}
        if ClassNCPaint.ClassNameIs(TWinControl.ClassName) then
      {$ELSE ~ FORM_EFFECTS_TESTS}
        if ClassNCPaint = TWinControl then
      {$ENDIF ~ FORM_EFFECTS_TESTS}
        begin
          Result := Result or RCF_EMULNC
        end
        else
        begin
          if (ClassPrint = nil) or not ClassPrint.InheritsFrom(ClassNCPaint)
          then Result := Result or RCF_PRINTNC or RCF_REFRESHNC
          else Result := Result or RCF_PRINTNC;
        end;
      end;
    end;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure GetRegControl(const Wnd: HWND; const WinControl: TWinControl; var RegControl: TRegControl); overload;
{$ELSE ~ FORM_EFFECTS_TESTS}
procedure GetRegControl(const Wnd: HWND; const WinControl: TWinControl; var RegControl: TRegControl);
{$ENDIF ~ FORM_EFFECTS_TESTS}
begin
  RegControl.Clear;

  var Flags := GetFlagsFromWindow(Wnd);

  if WinControl = nil then
  begin
    if Flags and RCF_RENDERNCMASK = 0 then Flags := RCF_RENDERNC or RCF_PRINTNC;
    if Flags and RCF_RENDERMASK   = 0 then Flags := Flags or RCF_RENDER or RCF_PAINT;
    RegControl.Flags := Flags;

    Exit;
  end;

  GetRegControlsManager.FindRegControl(WinControl.ClassType, RegControl);
  if Flags and RCF_BEFULLRENDER <> 0 then RegControl.Flags := Flags
  else
  begin
    if Flags and RCF_RENDERNCMASK <> 0 then
    begin
      if Flags and (RCF_BENCPREPAINT or RCF_BENCPOSTPAINT) <> 0 then
      begin
        RegControl.Flags := RegControl.Flags or (Flags and RCF_RENDERNCMASK);
      end
      else
      begin
        RegControl.Flags := (RegControl.Flags and not RCF_RENDERNCMASK) or (Flags and RCF_RENDERNCMASK);
      end;
    end;

    if Flags and RCF_RENDERMASK <> 0 then
    begin
      if Flags and (RCF_BEPREPAINT or RCF_BEPOSTPAINT) <> 0 then
      begin
        RegControl.Flags := RegControl.Flags or (Flags and RCF_RENDERMASK)
      end
      else
      begin
        RegControl.Flags := (RegControl.Flags and not RCF_RENDERMASK) or (Flags and RCF_RENDERMASK);
      end;
    end;

    RegControl.Flags := CompleteFlags(WinControl, RegControl.Flags);
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure GetRegControl(const Wnd: HWND; const WinControl: TWinControl;
  out Flags: DWORD; out NonClientCallback, ClientCallback: TFEPaintCallback); overload;
var
  Result: TRegControl;

begin
  GetRegControl(Wnd, WinControl, Result);

  Flags             := Result.Flags;
  NonClientCallback := Result.NonClientCallback;
  ClientCallback    := Result.ClientCallback;
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

{ TRegControl }

procedure TRegControl.Assign(Source: TRegControl);
begin
  Flags             := Source.Flags;
  NonClientCallback := Source.NonClientCallback;
  ClientCallback    := Source.ClientCallback;
end;

procedure TRegControl.Clear;
begin
  Flags             := 0;
  NonClientCallback := nil;
  ClientCallback    := nil;
end;

class function TRegControl.Create: TRegControl;
begin
  Result := TRegControl.Create(0, nil, nil);
end;

class function TRegControl.Create(const Flags: DWORD; const NonClientCallback,
  ClientCallback: TFEPaintCallback): TRegControl;
begin
  Result.Flags             := Flags;
  Result.NonClientCallback := NonClientCallback;
  Result.ClientCallback    := ClientCallback;
end;

{ TRegControlsManager }

constructor TRegControlsManager.Create;
begin
  FRegControls := TDictionary<string, TRegControl>.Create;
end;

destructor TRegControlsManager.Destroy;
begin
  FreeAndNil(FRegControls);
end;

procedure TRegControlsManager.FindRegControl(const ControlClass: TClass; var RegControl: TRegControl);
begin
  if ControlClass = nil then Exit;

  const NormalizedKey = NormalizeKey(ControlClass.ClassName);
  if FRegControls.ContainsKey(NormalizedKey) then
  begin
    RegControl.Assign(FRegControls[NormalizedKey]);
    Exit;
  end;

  for var RegControlPair in FRegControls do
  begin
    if IsInheritsClass(ControlClass, RegControlPair.Key) then
    begin
      RegControl.Assign(RegControlPair.Value);
      Exit;
    end;
  end;
end;

function TRegControlsManager.NormalizeKey(const Name: string): string;
begin
  Result := Name.ToLower;
end;

procedure TRegControlsManager.SaveRegControl(const ControlClassName: string; const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback);
begin
  const NormalizedKey = NormalizeKey(ControlClassName);
  const RegControl    = TRegControl.Create(Flags, NonClientCallback, ClientCallback);

  if (FRegControls.ContainsKey(NormalizedKey)) then FRegControls[NormalizedKey].Assign(RegControl)
  else FRegControls.Add(NormalizedKey, RegControl);
end;

{$ENDREGION 'Internal definitions'}

procedure RegisterControl(const ControlClass: TControlClass; const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil);
begin
  RegisterControl(
    ControlClass.ClassName,
    NonClientRenderMode,
    ClientRenderMode,
    RefreshNonClient,
    RefreshClient,
    NonClientCallback,
    ClientCallback
  );
end;

procedure RegisterControl(const ControlClassName: string; const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil; const ClientCallback: TFEPaintCallback = nil);
begin
  var Flags: DWORD := $00000000;

  var ResolvedNonClientCallback: TFEPaintCallback := nil;
  var ResolvedClientCallback   : TFEPaintCallback := nil;

  var ResolvedNonClientRenderMode := NonClientRenderMode;
  var ResolvedClientRenderMode    := ClientRenderMode;

  if not StyleServices.Enabled then ResolvedNonClientRenderMode := ResolvedNonClientRenderMode and not feThemed;

  if ResolvedNonClientRenderMode and feThemed <> 0 then
  begin
    Flags                       := Flags or RCF_THEMEDNC;
    ResolvedNonClientRenderMode := ResolvedNonClientRenderMode and not feThemed;
  end;
  if ResolvedNonClientRenderMode and feOwnCanvas <> 0 then
  begin
    Flags                       := Flags or RCF_OWNCANVASNC;
    ResolvedNonClientRenderMode := ResolvedNonClientRenderMode and not feOwnCanvas;
  end;
  if ResolvedClientRenderMode and feOwnCanvas <> 0 then
  begin
    Flags                    := Flags or RCF_OWNCANVAS;
    ResolvedClientRenderMode := ResolvedClientRenderMode and not feOwnCanvas;
  end;
  if ResolvedNonClientRenderMode and feRefreshFocused <> 0 then
  begin
    Flags                       := Flags or RCF_REFRESHFOCUSEDNC;
    ResolvedNonClientRenderMode := ResolvedNonClientRenderMode and not feRefreshFocused;
  end;
  if ResolvedClientRenderMode and feRefreshFocused <> 0 then
  begin
    Flags                    := Flags or RCF_REFRESHFOCUSED;
    ResolvedClientRenderMode := ResolvedClientRenderMode and not feRefreshFocused;
  end;

  if ResolvedNonClientRenderMode <> feNoRender then
  begin
    Flags := Flags or RCF_RENDERNC;
    case ResolvedNonClientRenderMode of
      fePaint    : Flags := Flags or RCF_PAINTNC;
      fePrint    : Flags := Flags or RCF_PRINTNC;
      feEmulate  : Flags := Flags or RCF_EMULNC;
      fePaintCopy: Flags := Flags or RCF_PAINTCOPYNC or RCF_REFRESHFOCUSEDNC;
      feCallback :
      begin
        ResolvedNonClientCallback := NonClientCallback;
        Flags                     := Flags or RCF_CALLBACKNC;
      end;
    end;
  end;
  if RefreshNonClient then Flags := Flags or RCF_REFRESHNC;

  if ResolvedClientRenderMode <> feNoRender then
  begin
    Flags := Flags or RCF_RENDER;
    case ResolvedClientRenderMode of
      fePaint    : Flags := Flags or RCF_PAINT;
      fePrint    : Flags := Flags or RCF_PRINT;
      feEmulate  : Flags := Flags or RCF_EMUL;
      fePaintCopy: Flags := Flags or RCF_PAINTCOPY or RCF_REFRESHFOCUSED;
      feCallback :
      begin
        ResolvedClientCallback := ClientCallback;
        Flags                  := Flags or RCF_CALLBACK;
      end;
    end;
  end;
  if RefreshClient then Flags := Flags or RCF_REFRESH;

  GetRegControlsManager.SaveRegControl(ControlClassName, Flags, ResolvedNonClientCallback, ResolvedClientCallback);
end;

{$REGION 'Internal definitions'}

type

{ TRenderWindowToDCHelper }

  TRenderWindowToDCHelper = record
  strict private type
    TPaintClientCallback = procedure(const DC: HDC) of object;
    TPaintClientReference = reference to procedure(const DC: HDC);

  strict private
    FWnd: HWND;
    FStopWnd: HWND;
//    FParentWnd: HWND;
    FIsMaximizedMDIClient: Boolean;
    FIsMaximizedMDIChild: Boolean;
    FIsMDIClient: Boolean;
  {$IFDEF USE_TRANSITION_EFFECTS}
    FIsRenderWindow: Boolean;
  {$ENDIF ~  USE_TRANSITION_EFFECTS}
    FRegControl: TRegControl;
    FWinControl: TWinControl;
//    FWinControlType: TClass;

  private
    class procedure AdjustBitmap(const DC: HDC; const ClientSize: TSize;
      const PaintClientCallback: TPaintClientReference); overload; static;

  strict private
    function HasFlags(const Flags: DWORD): Boolean; inline;
    procedure LoadWinControlData; inline;
    procedure CheckClipRegion(const DC: HDC; const CheckRegion: Boolean; const Size: TSize; const Rect: TRect); inline;
    procedure DoRender(const DC: HDC; const Size: TSize; const Rect: TRect); inline;
    procedure AdjustBrushOrigin(const DC: HDC; const PaintClientCallback: TPaintClientCallback);
    procedure AdjustBitmap(const DC: HDC; const ClientSize: TSize;
      const PaintClientCallback: TPaintClientCallback); overload; inline;
    procedure RenderChildWindows(const DC: HDC; const ClientOrg: TPoint; const Rect: TRect); inline;
    procedure PaintNonClient(const DC: HDC);
    procedure PaintClient(const DC: HDC);
    procedure PaintCopy(const DC: HDC); inline;
    procedure EraseAndPaintMessage(const DC: HDC); inline;
    procedure EmulatePaint(const DC: HDC); inline;
    procedure EmulateNCPaint(const DC: HDC);
    procedure ToolWindowNCPaint(const ToolWindow: TToolWindow; const DC: HDC); inline;
    procedure WinControlNCPaint(const DC: HDC); inline;
    procedure PaintThemeBorder(const DC: HDC); inline;

  public
    class function Create(const Window, StopWnd: HWND;
      const WinControl: TWinControl): TRenderWindowToDCHelper; overload; static; inline;
    class function Create(const Window, StopWnd{, ParentWnd}: HWND;
      const WinControl: TWinControl; const RegControl: TRegControl): TRenderWindowToDCHelper; overload; static; inline;

  public
    procedure RenderWindowToDC(const DC: HDC; const Rect: TRect; const CheckVisibility, CheckRegion: Boolean);
  end;

function ResolveStopWnd(const Window, StopWnd: HWND): HWND; inline;
begin
  if IsChild(Window, StopWnd) then Exit(StopWnd);
  Result := 0;
end;

{ TRenderWindowToDCHelper }

procedure TRenderWindowToDCHelper.AdjustBitmap(const DC: HDC; const ClientSize: TSize;
  const PaintClientCallback: TPaintClientCallback);
begin
  TRenderWindowToDCHelper.AdjustBitmap(
    DC,
    ClientSize,
    procedure(const DC: HDC)
    begin
      PaintClientCallback(DC);
    end
  );
end;

class procedure TRenderWindowToDCHelper.AdjustBitmap(const DC: HDC; const ClientSize: TSize;
  const PaintClientCallback: TPaintClientReference);
begin
  const Bitmap = TBitmap.Create;
  try
    const Canvas = Bitmap.Canvas;
    Canvas.Lock;
    try
      const Handle = Canvas.Handle;
      Bitmap.AdjustForTransition(0, ClientSize, TPixelFormat.pfDevice);
      PaintClientCallback(Handle);
      BitBlt(DC, 0, 0, ClientSize.Width, ClientSize.Height, Handle, 0, 0, SRCCOPY);
    finally
      Canvas.Unlock;
    end;
  finally
    FreeAndNil(Bitmap);
  end;
end;

procedure TRenderWindowToDCHelper.AdjustBrushOrigin(const DC: HDC; const PaintClientCallback: TPaintClientCallback);
var
  BrushOrgPos: TPoint;

begin
  const WindowDC = GetDC(FWnd);
  try
    GetBrushOrgEx(WindowDC, BrushOrgPos);
    try
      var AbsPos := TPoint.Zero;
      LPtoDP(DC, AbsPos, 1);
      SetBrushOrgEx(DC, BrushOrgPos.X + AbsPos.X, BrushOrgPos.Y + AbsPos.Y, nil);
      PaintClientCallback(DC);
    finally
      SetBrushOrgEx(DC, BrushOrgPos.X, BrushOrgPos.Y, nil);
    end;
  finally
    ReleaseDC(FWnd, WindowDC);
  end;
end;

procedure TRenderWindowToDCHelper.CheckClipRegion(const DC: HDC; const CheckRegion: Boolean; const Size: TSize;
  const Rect: TRect);
begin
  const WindowRect = TRect.InlineCreate(0, 0, Size.Width, Size.Height);
  const WindowRgn  = WindowRect.CreateRgn;

  if CheckRegion and (not FIsMaximizedMDIChild) then GetWindowRgn(FWnd, WindowRgn);

  var Point := TPoint.Zero;
  LPToDP(DC, Point, 1);
  OffsetRgn(WindowRgn, Point.X, Point.Y);

  const ClipRgn = WindowRect.CreateRgn;
  GetClipRgn(DC, ClipRgn);
  try
    CombineRgn(ClipRgn, WindowRgn, ClipRgn, RGN_AND);
    DeleteObject(WindowRgn);
    SelectClipRgn(DC, ClipRgn);
    var TempRect := Rect;
    GetRgnBox(ClipRgn, TempRect);
    DPToLP(DC, TempRect, 2);
  finally
    DeleteObject(ClipRgn);
  end;
end;

class function TRenderWindowToDCHelper.Create(const Window, StopWnd{, ParentWnd}: HWND;
  const WinControl: TWinControl; const RegControl: TRegControl): TRenderWindowToDCHelper;
begin
  with Result do
  begin
    FWnd        := Window;
    FStopWnd    := StopWnd;
//    FParentWnd  := ParentWnd;
    FRegControl := RegControl;

    if Assigned(WinControl) then FWinControl := WinControl else FWinControl := FindControl(Window);
    LoadWinControlData;
  end;
end;

procedure TRenderWindowToDCHelper.DoRender(const DC: HDC; const Size: TSize; const Rect: TRect);
var
  Point: TPoint;

begin
  const ClientCallback    = FRegControl.ClientCallback;
  const NonClientCallback = FRegControl.NonClientCallback;

  const RenderClient    = HasFlags(RCF_RENDER  );
  const RenderNonClient = HasFlags(RCF_RENDERNC);
  const CommonPainting  =
    RenderClient    and
    RenderNonClient and
    (
      (Assigned(ClientCallback) and Assigned(NonClientCallback) and (@ClientCallback = @NonClientCallback)) or
      (HasFlags(RCF_PAINTCOPY) or HasFlags(RCF_PAINTCOPYNC)) or HasFlags(RCF_BEFULLRENDER)
    );

  if CommonPainting then
  begin
    if FIsMaximizedMDIChild then OffsetWindowOrgEx(DC, -FWinControl.Left, -FWinControl.Top, Point);
    try
      if HasFlags(RCF_OWNCANVASNC or RCF_OWNCANVAS) then AdjustBitmap(DC, Size, PaintNonClient)
      else AdjustBrushOrigin(DC, PaintNonClient);
    finally
      if FIsMaximizedMDIChild then SetWindowOrgEx(DC, Point.X, Point.Y, nil);
    end;
  end
  else
  begin
    var ClizetSize: TSize;
    var ClientOrgPoint: TPoint;
    GetClientSize(FWnd, FWinControl, FIsMaximizedMDIChild, ClizetSize, ClientOrgPoint);

    if RenderClient then
    begin
      // Remember current clipping region
      const SaveRgn = TRect.CreateRgn(0, 0, 0, 0);
      GetClipRgn(DC, SaveRgn);
      try
        OffsetWindowOrgEx(DC, -ClientOrgPoint.X, -ClientOrgPoint.Y, Point);
        try
          DC.IntersectClip(ClizetSize);

          if HasFlags(RCF_OWNCANVAS) then AdjustBitmap(DC, ClizetSize, PaintClient)
          else AdjustBrushOrigin(DC, PaintClient);
        finally
          SetWindowOrgEx(DC, Point.X, Point.Y, nil);
        end;
      finally
        SelectClipRgn(DC, SaveRgn);
        DeleteObject(SaveRgn);
      end;
    end;

    const HasNonClientArea = (ClizetSize.Width <> Size.Width) or (ClizetSize.Height <> Size.Height);

    if HasNonClientArea and RenderNonClient then
    begin
      // Remember current clipping region
      const SaveRgn = TRect.CreateRgn(0, 0, 0, 0);
      GetClipRgn(DC, SaveRgn);
      try
        DC.ExcludeClip(ClientOrgPoint, ClizetSize);

        if FIsMaximizedMDIChild then OffsetWindowOrgEx(DC, -FWinControl.Left, -FWinControl.Top, Point);
        try
          if HasFlags(RCF_OWNCANVASNC) then AdjustBitmap(DC, Size, PaintNonClient)
          else AdjustBrushOrigin(Dc, PaintNonClient);
        finally
          if FIsMaximizedMDIChild then SetWindowOrgEx(DC, Point.X, Point.Y, nil);
        end;
      finally
        SelectClipRgn(DC, SaveRgn);
        DeleteObject(SaveRgn);
      end;
    end;
  end;

  if (FWinControl = nil) or not (FWinControl is TOleControl) then
  begin
    var ClizetSize: TSize;
    var ClientOrgPoint: TPoint;
    GetClientSize(FWnd, FWinControl, FIsMaximizedMDIChild, ClizetSize, ClientOrgPoint);

    const SaveRgn = TRect.CreateRgn(0, 0, 0, 0);
    GetClipRgn(DC, SaveRgn);
    try
      DC.IntersectClip(ClientOrgPoint, ClizetSize);
      RenderChildWindows(DC, ClientOrgPoint, Rect);
    finally
      SelectClipRgn(DC, SaveRgn);
      DeleteObject(SaveRgn);
    end;
  end;
end;

procedure TRenderWindowToDCHelper.EmulateNCPaint(const DC: HDC);
begin
  if FWinControl = nil then Exit;

  {$MESSAGE WARN 'Need to check this condition `FWinControl is TToolWindow`'}
  if IsInheritsClass(FWinControl.ClassType, TToolWindow.ClassName) then ToolWindowNCPaint(FWinControl as TToolWindow, DC)
  else WinControlNCPaint(DC);
end;

procedure TRenderWindowToDCHelper.EmulatePaint(const DC: HDC);
begin
  if FWinControl = nil then Exit;

  if FWinControl is TOleControl then
  begin
    FWinControl.HandleNeeded;
    OleDraw(IUnknown(TOleControl(FWinControl).OleObject), DVASPECT_CONTENT, DC, FWinControl.ClientRect);
  end;
end;

procedure TRenderWindowToDCHelper.EraseAndPaintMessage(const DC: HDC);
begin
  const DoubleBuffered = Assigned(FWinControl) and FWinControl.DoubleBuffered;
  if DoubleBuffered then FWinControl.DoubleBuffered := False;

  const SavedDCIndex = SaveDC(DC);
  try
    SendMessage(FWnd, WM_ERASEBKGND, DC, 0);
  finally
    RestoreDC(DC, SavedDCIndex);
  end;
  SendMessage(FWnd, WM_PAINT, DC, FE_ID);

  if DoubleBuffered then FWinControl.DoubleBuffered := True;
end;

function TRenderWindowToDCHelper.HasFlags(const Flags: DWORD): Boolean;
begin
  Result := FRegControl.Flags and Flags <> 0;
end;

procedure TRenderWindowToDCHelper.LoadWinControlData;
var
  ClassName: string;

begin
  if Assigned(FWinControl) then
  begin
    ClassName       := FWinControl.ClassName;
//    FWinControlType := FWinControl.ClassType;

    if IsMDIFormWithMaximizedMDIChild(FWinControl) then
    begin // Edge changing
      Application.MainForm.ClientHandle.SetWindowData(
        GWL_EXSTYLE,
        Application.MainForm.ClientHandle.GetWindowData(GWL_EXSTYLE) and not WS_EX_CLIENTEDGE
      );
      SetWindowPos(
        Application.MainForm.ClientHandle,
        0,
        0,
        0,
        0,
        0,
        SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER
      );
    end;

    FIsMaximizedMDIClient := False;
    FIsMaximizedMDIChild  := IsMaximizedMDIChild(FWinControl);
  end
  else
  begin
    ClassName       := FWnd.GetWindowClassName;
//    FWinControlType := GetClass(ClassName);

    FIsMaximizedMDIClient := IsMaximizedMDIClient(ClassName);
    FIsMaximizedMDIChild  := False;
  end;

  FIsMDIClient    := SameText(ClassName, MDI_CLIENT_CLASS_NAME);
{$IFDEF USE_TRANSITION_EFFECTS}
  FIsRenderWindow := SameText(ClassName, TFERenderWindow.ClassName);
  {$MESSAGE 'Need to implement `TFERenderWindow` class'}
{$ENDIF ~ USE_TRANSITION_EFFECTS}
end;

procedure TRenderWindowToDCHelper.PaintClient(const DC: HDC);
var
  SavedDCIndex: Integer;

begin
  if HasFlags(RCF_BEPREPAINT) then
  begin
    SavedDCIndex := SaveDC(DC);
    try
      SendMessage(FWnd, CM_FEPAINT, DC, FE_ID)
    finally
      RestoreDC(DC, SavedDCIndex);
    end;
  end;

  SavedDCIndex := SaveDC(DC);
  try
    if HasFlags(RCF_PRINT) then SendMessage(FWnd, WM_PRINT, DC, PRF_ERASEBKGND or PRF_CLIENT)
    else if HasFlags(RCF_PAINT) then EraseAndPaintMessage(DC)
    else if HasFlags(RCF_CALLBACK) then
    begin
      if Assigned(FRegControl.ClientCallback) then FRegControl.ClientCallback(FWinControl, DC);
    end
    else if HasFlags(RCF_BEPAINT) then SendMessage(FWnd, CM_FEPAINT, DC, FE_ID)
    else EmulatePaint(DC);
  finally
    RestoreDC(DC, SavedDCIndex);
  end;

  if HasFlags(RCF_BEPOSTPAINT) then
  begin
    SavedDCIndex := SaveDC(DC);
    try
      SendMessage(FWnd, CM_FEPAINT, DC, FE_ID)
    finally
      RestoreDC(DC, SavedDCIndex);
    end;
  end;
end;

procedure TRenderWindowToDCHelper.PaintCopy(const DC: HDC);
begin
  if FWinControl = nil then Exit;

  FWinControl.ControlState := FWinControl.ControlState + [TControlStateItem.csPaintCopy];
  try
    SendMessage(FWinControl.Handle, WM_PAINT, DC, FE_ID);
  finally
    FWinControl.ControlState := FWinControl.ControlState - [TControlStateItem.csPaintCopy];
  end;
end;

procedure TRenderWindowToDCHelper.PaintNonClient(const DC: HDC);
var
  SavedDCIndex: Integer;

begin
  if FWinControl is TScrollingWinControl then
  begin
    with TScrollingWinControl(FWinControl) do
    begin
    {$MESSAGE WARN 'Need to check this condition'}
      if ((HorzScrollBar.Visible and (HorzScrollBar.Style = ssRegular)) and not (VertScrollBar.Visible and (VertScrollBar.Style <> ssRegular))) or
         ((VertScrollBar.Visible and (VertScrollBar.Style = ssRegular)) and not (HorzScrollBar.Visible and (HorzScrollBar.Style <> ssRegular))) then
        UninitializeFlatSB(FWnd);
    end;
  end;

  if HasFlags(RCF_BENCPREPAINT) then
  begin
    SavedDCIndex := SaveDC(DC);
    try
      SendMessage(FWnd, CM_FENCPAINT, DC, FE_ID)
    finally
      RestoreDC(DC, SavedDCIndex);
    end;
  end;

  SavedDCIndex := SaveDC(DC);
  try
    if
      Assigned(FWinControl) and
      ((FWinControl.GetInternalBorderWidth > 0) or (FWinControl.GetInternalBevelKind <> TBevelKind.bkNone))
    then
    begin
      const SavedDCIndex2 = SaveDC(DC);
      try
        EmulateNCPaint(DC);
      finally
        RestoreDC(DC, SavedDCIndex2);
      end;
    end;

    if HasFlags(RCF_PRINTNC) then NCPrintControl(FWnd, FWinControl, DC)
    else if HasFlags(RCF_PAINTNC) then
    begin
      NCPrintControl(FWnd, FWinControl, DC);
      SendMessage(FWnd, WM_NCPAINT, 0, DC);
    end
    else if HasFlags(RCF_CALLBACKNC) then
    begin
      const NonClientCallback = FRegControl.NonClientCallback;
      if Assigned(FWinControl) and Assigned(NonClientCallback) then NonClientCallback(FWinControl, DC);
    end
    else if HasFlags(RCF_PAINTCOPY or RCF_PAINTCOPYNC) then PaintCopy(DC)
    else if HasFlags(RCF_BENCPAINT) then SendMessage(FWnd, CM_FENCPAINT, DC, FE_ID)
    else if HasFlags(RCF_BEFULLRENDER) then SendMessage(FWnd, CM_FEFULLRENDER, DC, FE_ID)
    else EmulateNCPaint(DC);
  finally
    RestoreDC(DC, SavedDCIndex);
  end;

  if HasFlags(RCF_BENCPOSTPAINT) then
  begin
    SavedDCIndex := SaveDC(DC);
    try
      SendMessage(FWnd, CM_FENCPAINT, DC, FE_ID)
    finally
      RestoreDC(DC, SavedDCIndex);
    end;
  end;
end;

procedure TRenderWindowToDCHelper.PaintThemeBorder(const DC: HDC);
begin
  const WinControlWnd = FWinControl.Handle;

  const ExStyle = WinControlWnd.GetWindowData(GWL_EXSTYLE);
  if (ExStyle and WS_EX_CLIENTEDGE) = 0 then Exit;

  var DrawRect := TRect.CreateWindowRect(WinControlWnd);

  OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
  DC.ExcludeClip(DrawRect, 2);
  const Details = StyleServices.GetElementDetails(teEditTextNormal);
  StyleServices.DrawElement(DC, Details, DrawRect);
end;

procedure TRenderWindowToDCHelper.RenderChildWindows(const DC: HDC; const ClientOrg: TPoint; const Rect: TRect);
begin
  var ChildWindow := GetWindow(FWnd, GW_CHILD);
  if ChildWindow <> 0 then
  begin
    if FIsMaximizedMDIClient then ChildWindow := GetWindow(ChildWindow, GW_HWNDFIRST)
    else ChildWindow := GetWindow(ChildWindow, GW_HWNDLAST);
  end;

  while (ChildWindow <> 0) and ((ChildWindow <> FStopWnd) or (FIsMDIClient and not FIsMaximizedMDIClient)) do
  begin
    if IsWindowVisible(ChildWindow) then
    begin
      var ChildRect: TRect;
      var ChildOrg : TPoint;

      if FIsMaximizedMDIClient then
      begin
        ChildRect := Rect;
        ChildOrg  := TPoint.Zero;
      end
      else
      begin
        ChildRect := TRect.CreateWindowRect(ChildWindow);
//        ScreenToClient(FWindow, ChildRect.TopLeft);
//        ScreenToClient(FWindow, ChildRect.BottomRight);
        FWnd.MapWindowFromScreen(ChildRect);
        OffsetRect(ChildRect, ClientOrg.X, ClientOrg.Y);
        ChildOrg := ChildRect.TopLeft;
        IntersectRect(ChildRect, ChildRect, Rect);
        OffsetRect(ChildRect, -ChildOrg.X, -ChildOrg.Y);
      end;

      if not IsRectEmpty(ChildRect) then
      begin
        var Point: TPoint;
        OffsetWindowOrgEx(DC, -ChildOrg.X, -ChildOrg.Y, Point);
        with TRenderWindowToDCHelper.Create(ChildWindow, FStopWnd{, FWindow}, nil, FRegControl) do
        try
          RenderWindowToDC(DC, ChildRect, True, True);
        finally
          SetWindowOrgEx(DC, Point.X, Point.Y, nil);
        end;
      end;
    end;

    if FIsMaximizedMDIClient then ChildWindow := 0 else ChildWindow := GetWindow(ChildWindow, GW_HWNDPREV);
  end;
end;

procedure TRenderWindowToDCHelper.RenderWindowToDC(const DC: HDC; const Rect: TRect;
  const CheckVisibility, CheckRegion: Boolean);
begin
{$IFDEF USE_TRANSITION_EFFECTS}
  if FIsRenderWindow then Exit;
{$ENDIF ~ USE_TRANSITION_EFFECTS}
  if not CheckVisibility or IsWindowVisible(FWnd) then
  begin
    // Remember current clipping region
    const SaveRgn = TRect.CreateRgn(0, 0, 0, 0);
    GetClipRgn(DC, SaveRgn);
    try
      const Size = FWnd.GetWindowSize(FIsMaximizedMDIChild);
      CheckClipRegion(DC, CheckRegion, Size, Rect);
      if not IsRectEmpty(Rect) then
      begin
        GetRegControl(FWnd, FWinControl, FRegControl);
        DoRender(DC, Size, Rect);
      end;
    finally
      SelectClipRgn(DC, SaveRgn);
      DeleteObject(SaveRgn);
    end;
  end;
end;

procedure TRenderWindowToDCHelper.ToolWindowNCPaint(const ToolWindow: TToolWindow; const DC: HDC);
const
  InnerStyles: array[TEdgeStyle] of Cardinal = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TEdgeStyle] of Cardinal = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
  Ctl3DStyles: array[Boolean]    of Cardinal = (BF_MONO, 0);

begin
  const ToolWindowHandle = ToolWindow.Handle;

  var WindowRect := TRect.CreateWindowRect(ToolWindowHandle);
  var ClientRect := TRect.CreateClientRect(ToolWindowHandle);

  ToolWindowHandle.MapWindowFromScreen(WindowRect);
  OffsetRect(ClientRect, -WindowRect.Left, -WindowRect.Top);
  ClientRect.ExcludeClip(DC);
  // Draw borders in non-client area
  OffsetRect(WindowRect, -WindowRect.Left, -WindowRect.Top);

  DrawEdge(
    DC,
    WindowRect,
    InnerStyles[ToolWindow.EdgeInner] or OuterStyles[ToolWindow.EdgeOuter],
    Byte(ToolWindow.EdgeBorders) or Ctl3DStyles[ToolWindow.GetInternalCtl3D] or BF_ADJUST
  );
  // Erase parts not drawn
  WindowRect.IntersectClip(DC);
  FillRect(DC, WindowRect, ToolWindow.Brush.Handle);
end;

procedure TRenderWindowToDCHelper.WinControlNCPaint(const DC: HDC);
const
  InnerStyles: array[TBevelCut]  of Cardinal = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut]  of Cardinal = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles:  array[TBevelKind] of Cardinal = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean]    of Cardinal = (BF_MONO, 0);

begin
  const SavedDCIndex = SaveDC(DC);
  try
    const WinControlWnd = FWinControl.Handle;

    if (FWinControl.GetInternalBevelKind <> TBevelKind.bkNone) or (FWinControl.GetInternalBorderWidth > 0) then
    begin
      var ClientRect := TRect.CreateClientRect(WinControlWnd);
      var WindowRect := TRect.CreateWindowRect(WinControlWnd);

      WinControlWnd.MapWindowFromScreen(WindowRect);
      OffsetRect(ClientRect, -WindowRect.Left, -WindowRect.Top);
      ClientRect.ExcludeClip(DC);

      { Draw borders in non-client area }
      const SavedWindowRect = WindowRect;
      ClientRect.Inflate(FWinControl.GetInternalBorderWidth);
      WindowRect := ClientRect;

      const WinStyle = WinControlWnd.GetWindowData(GWL_STYLE);
      if (WinStyle and WS_VSCROLL) <> 0 then Inc(WindowRect.Right, GetSystemMetrics(SM_CYVSCROLL));
      if (WinStyle and WS_HSCROLL) <> 0 then Inc(WindowRect.Bottom, GetSystemMetrics(SM_CXHSCROLL));

      const BevelKind = FWinControl.GetInternalBevelKind;
      if BevelKind <> TBevelKind.bkNone then
      begin
        var EdgeSize := 0;
        const BevelEdges = FWinControl.GetInternalBevelEdges;

        if FWinControl.GetInternalBevelInner <> TBevelCut.bvNone then Inc(EdgeSize, FWinControl.GetInternalBevelWidth);
        if FWinControl.GetInternalBevelOuter <> TBevelCut.bvNone then Inc(EdgeSize, FWinControl.GetInternalBevelWidth);

        if TBevelEdge.beLeft   in BevelEdges then Dec(WindowRect.Left,   EdgeSize);
        if TBevelEdge.beTop    in BevelEdges then Dec(WindowRect.Top,    EdgeSize);
        if TBevelEdge.beRight  in BevelEdges then Inc(WindowRect.Right,  EdgeSize);
        if TBevelEdge.beBottom in BevelEdges then Inc(WindowRect.Bottom, EdgeSize);

        DrawEdge(
          DC,
          WindowRect,
          InnerStyles[FWinControl.GetInternalBevelInner] or OuterStyles[FWinControl.GetInternalBevelOuter],
          Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[FWinControl.GetInternalCtl3D] or BF_ADJUST
        );
      end;

      WindowRect.IntersectClip(DC);
      WindowRect := SavedWindowRect;
      { Erase parts not drawn }
      OffsetRect(WindowRect, -WindowRect.Left, -WindowRect.Top);
      FillRect(DC, WindowRect, FWinControl.Brush.Handle);
    end;

    const SavedDCIndex2 = SaveDC(DC);
    try
      NCPrintControl(WinControlWnd, FWinControl, DC);
    finally
      RestoreDC(DC, SavedDCIndex2);
    end;

    const Themed = HasFlags(RCF_THEMEDNC) and TWinVersion.IsWinXP;
    if Themed or (csNeedsBorderPaint in FWinControl.ControlStyle) then PaintThemeBorder(DC);
  finally
    RestoreDC(DC, SavedDCIndex);
  end;
end;

class function TRenderWindowToDCHelper.Create(const Window, StopWnd: HWND;
  const WinControl: TWinControl): TRenderWindowToDCHelper;
begin
//  var Parent := GetParent(Window);
//  if Parent <> 0 then
//  begin
//    if SameText(Parent.GetClassName, TApplication.ClassName) then Parent := 0;
//  end;
//
  Result := TRenderWindowToDCHelper.Create(Window, ResolveStopWnd(Window, StopWnd){, Parent}, WinControl, TRegControl.Create);
end;

{$ENDREGION 'Internal definitions'}

procedure NCPrintControl(const Wnd: HWND; const WinControl: TWinControl; const DC: HDC);
begin
  if
    Assigned(WinControl)                                                       and
    (WinControl is TCustomForm)                                                and
    ((WinControl as TCustomForm).GetInternalFormStyle = TFormStyle.fsMDIChild) and
    TWinVersion.IsWinXP                                                        and
    Wnd.HasRegion
  then
  begin // XP does something weird with the clipping region
    TRenderWindowToDCHelper.AdjustBitmap(
      DC,
      TSize.InlineCreate(WinControl.Width, WinControl.Height),
      procedure(const DC: HDC)
      begin
        SendMessage(Wnd, WM_PRINT, DC, PRF_NONCLIENT);
      end
    );
  end
  else SendMessage(Wnd, WM_PRINT, DC, PRF_NONCLIENT);
end;

procedure RenderWindowToDC(const Wnd, StopWnd: HWND; const WinControl: TWinControl; const DC: HDC; const Rect: TRect;
  const ClientCoordinates, CheckVisibility, CheckRegion: Boolean);
begin
  if (Wnd = StopWnd) or (CheckVisibility and (not IsWindowVisible(Wnd))) then Exit;

  var TempRect := Rect;
  var Point    := TPoint.Zero;

  if ClientCoordinates and not IsMaximizedMDIChild(WinControl) then
  begin
    const ClientOffset = Wnd.GetWindowOffset;
    OffsetRect(TempRect, ClientOffset.X, ClientOffset.Y);
    OffsetWindowOrgEx(DC, ClientOffset.X, ClientOffset.Y, Point);
  end;

  with TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl) do
  try
    RenderWindowToDC(DC, TempRect, False, CheckRegion);
  finally
    if not EqualRect(Rect, TempRect) then SetWindowOrgEx(DC, Point.X, Point.Y, nil);
  end;
end;

procedure RegisterRichEdit; inline;
begin
{$MESSAGE WARN 'Not Implemented RegisterRichEdit into another unit'}
end;

initialization
// Vcl.Controls
  RegisterControl(TControl,           fePrint,               fePaint,                  False, False);
  RegisterControl(TCustomListControl, feEmulate or feThemed, fePrint,                  True,  False);
  RegisterControl(TCustomListBox,     feEmulate or feThemed, fePrint,                  False, False);
// Vcl.ComCtrls
  RegisterControl(TAnimate,           fePrint,               fePrint,                  False, True );
  RegisterControl(TToolBar,           fePrint,               fePaint,                  True,  False);
  RegisterControl(TCommonCalendar,    feEmulate or feThemed, fePrint,                  False, False);
// Vcl.StdCtrls
  RegisterControl(TCustomEdit,        feEmulate or feThemed, fePrint,                  False, False);
// Vcl.OleCtrls
  RegisterControl(TOleControl,        feNoRender,            feEmulate or feOwnCanvas, True,  True );

// Not in general VCL
  RegisterRichEdit;
// Vcl.Grids
//  if SysLocale.MiddleEast then
//    RegisterTEControl('TCustomGrid', tePrint, tePaint or teOwnCanvas, False, False);
// Vcl.CheckLst
//  RegisterTEControl('TCheckListBox'     , teEmulate or teThemed, tePrint or teOwnCanvas, False, False);

finalization
  if Assigned(RegControlsManager) then FreeAndNil(RegControlsManager);

end.
