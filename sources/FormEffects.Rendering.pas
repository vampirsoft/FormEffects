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
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.OleCtrls,
  Vcl.StdCtrls,
{$IFDEF FORM_EFFECTS_TESTS}
  Vcl.ToolWin,
{$ENDIF ~ FORM_EFFECTS_TESTS}
  Vcl.Forms,
  FormEffects.TypeHelpers,
  FormEffects.FormContainer,
  FormEffects.Utils.OS,
  FormEffects.Utils.Windows,
  FormEffects.Utils.ScrollBars,
  FormEffects.Utils.Rects,
  FormEffects.Utils.Forms,
  FormEffects.Rendering.Ext
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.ComCtrls.Mocks
  , FormEffects.Vcl.StdCtrls.Mocks
  , FormEffects.Vcl.OleCtrls.Mocks
  , FormEffects.Vcl.ToolWin.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

const

  // Form Effects messages

  // Thir party components interface
  FE_ID           = $41A2;
  FE_BASE         = CM_BASE + $0C4A;
  CM_FEPAINT      = FE_BASE + 0; // Paint client area to Form Effects' DC
  CM_FENCPAINT    = FE_BASE + 1; // Paint non client area to Form Effects' DC
  CM_FEFULLRENDER = FE_BASE + 2; // Paint whole control to Form Effects' DC

type
  TFEPaintCallback = procedure(const WinControl: TWinControl; const DC: HDC);

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

type
  TPaintClientCallback = procedure(
    const Wnd, StopWnd: HWND;
    const WinControl: TWinControl;
    const Flags: DWORD;
    const NonClientCallback, ClientCallback: TFEPaintCallback;
    const DC: HDC
  );

function DoesAncestorHandle(const WinControl: TWinControl; const Selector: Integer): TClass;
function CompleteFlags(const WinControl: TWinControl; const Flags: DWORD): DWORD;
procedure GetRegControl(
  const Wnd: HWND;
  const WinControl: TWinControl;
  var Flags: DWORD;
  var NonClientCallback, ClientCallback: TFEPaintCallback
); overload;

procedure ToolWindowNCPaint(const ToolWindow: TToolWindow; const DC: HDC);
procedure WinControlNCPaint(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
); overload;
procedure EmulateNCPaint(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
); overload;
{$MESSAGE 'Need to test PaintNonClient'}
procedure PaintNonClient(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
); overload;
{$MESSAGE 'Need to test PaintClient'}
procedure PaintClient(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
); overload;
{$MESSAGE 'Need to test RenderChildWindows'}
procedure RenderChildWindows(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
  const DC: HDC;
  const ClientOrgPoint: TPoint;
  const Rect: TRect
); overload;
{$MESSAGE 'Need to test AdjustBrushOrigin'}
procedure AdjustBrushOrigin(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC;
  const PaintClientCallback: TPaintClientCallback
); overload;
{$MESSAGE 'Need to test AdjustBitmap'}
procedure AdjustBitmap(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC;
  const ClientSize: TSize;
  const PaintClientCallback: TPaintClientCallback
); overload;
{$MESSAGE 'Need to test CheckClipRegion'}
procedure CheckClipRegion(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIChild: Boolean;
  const DC: HDC;
  const CheckRegion: Boolean;
  const Size: TSize
); overload;
{$MESSAGE 'Need to test DoRender'}
procedure DoRender(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
  const DC: HDC;
  const Size: TSize;
  const Rect: TRect
); overload;
procedure RenderWindowToDCAux(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC;
  const Rect: TRect;
  const CheckVisibility, CheckRegion: Boolean
); overload;

{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure RegisterControl(
  const ControlClass: TControlClass;
  const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil;
  const ClientCallback: TFEPaintCallback = nil
); overload; inline;
procedure RegisterControl(
  const ControlClassName: string;
  const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback: TFEPaintCallback = nil;
  const ClientCallback: TFEPaintCallback = nil
); overload;

procedure RenderWindowToDC(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const DC: HDC;
  const Rect: TRect;
  const ClientCoordinates, CheckVisibility, CheckRegion: Boolean
);

implementation

uses
  Winapi.Messages,
  Winapi.ActiveX,
  Winapi.FlatSB,
  System.Classes,
  System.Math,
  System.Generics.Collections,
{$IFNDEF FORM_EFFECTS_TESTS}
  Vcl.ToolWin,
{$ENDIF ~ FORM_EFFECTS_TESTS}
  Vcl.Themes,
  FormEffects.Utils,
  FormEffects.Utils.Pictures,
  FormEffects.Rendering.Pictures
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Themes.Mocks
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
    class function Create(
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback
    ): TRegControl; overload; static; inline;

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
    procedure SaveRegControl(
      const ControlClassName: string;
      const Flags: DWORD;
      const NonClientCallback, ClientCallback: TFEPaintCallback
    ); inline;
  end;

var
  RegControlsManager: TRegControlsManager = nil;

function GetRegControlsManager: TRegControlsManager; inline;
begin
  if RegControlsManager = nil then
    RegControlsManager := TRegControlsManager.Create;
  Result := RegControlsManager;
end;

function GetFlagsFromWindow(const Wnd: HWND): DWORD; inline;
begin
  if Wnd.SendMessage(CM_FEFULLRENDER, 0, FE_ID) = FE_ID then
    Exit(RCF_RENDER or RCF_RENDERNC or RCF_BEFULLRENDER);

  Result := 0;
  case Wnd.SendMessage(CM_FENCPAINT, 0, FE_ID) of
    FE_ID - 1: Result := RCF_RENDERNC or RCF_BENCPREPAINT;
    FE_ID    : Result := RCF_RENDERNC or RCF_BENCPAINT;
    FE_ID + 1: Result := RCF_RENDERNC or RCF_BENCPOSTPAINT;
  end;
  case Wnd.SendMessage(CM_FEPAINT, 0, FE_ID) of
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
      if (Result and RCF_PAINTCOPYNC) <> 0 then
        Result := (Result and not RCF_PAINTCOPYNC) or RCF_PRINT;
      if (Result and RCF_PAINTCOPY  ) <> 0 then
        Result := (Result and not RCF_PAINTCOPY) or RCF_PAINT;
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
      if ClassPrint = nil then
        Result := Result or RCF_PAINT
      else
        Result := Result or RCF_PRINT;
    end;

    if ((Result and RCF_RENDERNC) <> 0) and ((Result and RCF_RENDERNCMASK) = RCF_RENDERNC) then
    begin
      const ClassNCPaint = DoesAncestorHandle(WinControl, WM_NCPAINT);

      if ClassNCPaint = nil then
        Result := Result or RCF_PRINTNC
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
          if (ClassPrint = nil) or not ClassPrint.InheritsFrom(ClassNCPaint) then
            Result := Result or RCF_PRINTNC or RCF_REFRESHNC
          else
            Result := Result or RCF_PRINTNC;
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
    if Flags and RCF_RENDERNCMASK = 0 then
      Flags := RCF_RENDERNC or RCF_PRINTNC;
    if Flags and RCF_RENDERMASK   = 0 then
      Flags := Flags or RCF_RENDER or RCF_PAINT;
    RegControl.Flags := Flags;

    Exit;
  end;

  GetRegControlsManager.FindRegControl(WinControl.ClassType, RegControl);
  if Flags and RCF_BEFULLRENDER <> 0 then
    RegControl.Flags := Flags
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
procedure GetRegControl(
  const Wnd: HWND;
  const WinControl: TWinControl;
  var Flags: DWORD;
  var NonClientCallback, ClientCallback: TFEPaintCallback
); overload;
var
  Result: TRegControl;

begin
  Result := TRegControl.Create(Flags, NonClientCallback, ClientCallback);

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

class function TRegControl.Create(
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback
): TRegControl;
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
  if ControlClass = nil then
    Exit;

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

procedure TRegControlsManager.SaveRegControl(
  const ControlClassName: string;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback
);
begin
  const NormalizedKey = NormalizeKey(ControlClassName);
  const RegControl    = TRegControl.Create(Flags, NonClientCallback, ClientCallback);

  if (FRegControls.ContainsKey(NormalizedKey)) then
    FRegControls[NormalizedKey].Assign(RegControl)
  else
    FRegControls.Add(NormalizedKey, RegControl);
end;

{$ENDREGION 'Internal definitions'}

procedure RegisterControl(
  const ControlClass: TControlClass;
  const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback, ClientCallback: TFEPaintCallback
);
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

procedure RegisterControl(
  const ControlClassName: string;
  const NonClientRenderMode, ClientRenderMode: DWORD;
  const RefreshNonClient, RefreshClient: Boolean;
  const NonClientCallback, ClientCallback: TFEPaintCallback
);
begin
  var Flags: DWORD := $00000000;

  var ResolvedNonClientCallback: TFEPaintCallback := nil;
  var ResolvedClientCallback   : TFEPaintCallback := nil;

  var ResolvedNonClientRenderMode := NonClientRenderMode;
  var ResolvedClientRenderMode    := ClientRenderMode;

  if not StyleServices.Enabled then
    ResolvedNonClientRenderMode := ResolvedNonClientRenderMode and not feThemed;

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
  if RefreshNonClient then
    Flags := Flags or RCF_REFRESHNC;

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
  if RefreshClient then
    Flags := Flags or RCF_REFRESH;

  GetRegControlsManager.SaveRegControl(ControlClassName, Flags, ResolvedNonClientCallback, ResolvedClientCallback);
end;

{$REGION 'Internal definitions'}

type

{ TRenderWindowToDCHelper }

  TRenderWindowToDCHelper = record
  private
    FWnd: HWND;
    FStopWnd: HWND;
    FIsMaximizedMDIClient: Boolean;
    FIsMaximizedMDIChild: Boolean;
    FIsMDIClient: Boolean;
  {$IFDEF USE_TRANSITION_EFFECTS}
    FIsRenderWindow: Boolean;
  {$ENDIF ~  USE_TRANSITION_EFFECTS}
    FRegControl: TRegControl;
    FWinControl: TWinControl;

  private
    function HasFlags(const Flags: DWORD): Boolean; inline;

  public
    class function Create(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl
    ): TRenderWindowToDCHelper; overload; static; inline;
    class function Create(
      const Wnd, StopWnd: HWND;
      const WinControl: TWinControl;
      const RegControl: TRegControl
    ): TRenderWindowToDCHelper; overload; static; inline;

  public
    procedure RenderWindowToDC(const DC: HDC; const Rect: TRect; const CheckVisibility, CheckRegion: Boolean);
    procedure LoadWinControlData; inline;
  end;

  TPaintClientReference = reference to procedure(const DC: HDC);
{$IFNDEF FORM_EFFECTS_TESTS}
  TPaintClientCallback = procedure(
    const Helper: TRenderWindowToDCHelper;
    const DC: HDC
  );
{$ENDIF ~ FORM_EFFECTS_TESTS}

function ResolveStopWnd(const Wnd, StopWnd: HWND): HWND; inline;
begin
  if StopWnd.IsChild(Wnd) then
    Exit(StopWnd);
  Result := 0;
end;

procedure ToolWindowNCPaint(const ToolWindow: TToolWindow; const DC: HDC);{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
const
  InnerStyles: array[TEdgeStyle] of Cardinal = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TEdgeStyle] of Cardinal = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
  Ctl3DStyles: array[Boolean]    of Cardinal = (BF_MONO, 0);

begin
  const ToolWindowWnd = ToolWindow.Handle;

  var WindowRect := ToolWindowWnd.GetWindowRect;
  var ClientRect := ToolWindowWnd.GetClientRect;

  ToolWindowWnd.MapWindowRectFromScreen(WindowRect);
  ClientRect.OffsetRect(-WindowRect.TopLeft);
  DC.ExcludeClip(ClientRect);
  // Draw borders in non-client area
  WindowRect.OffsetRect(-WindowRect.TopLeft);

  DC.DrawEdge(
    WindowRect,
    InnerStyles[ToolWindow.EdgeInner] or OuterStyles[ToolWindow.EdgeOuter],
    Byte(ToolWindow.EdgeBorders) or Ctl3DStyles[ToolWindow.GetProtectedCtl3D] or BF_ADJUST
  );
  // Erase parts not drawn
  DC.IntersectClip(WindowRect);
  DC.Fill(WindowRect, ToolWindow.Brush.Handle);
end;

procedure WinControlNCPaint(const Helper: TRenderWindowToDCHelper; const DC: HDC); overload; inline;
const
  InnerStyles: array[TBevelCut]  of Cardinal = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut]  of Cardinal = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles:  array[TBevelKind] of Cardinal = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean]    of Cardinal = (BF_MONO, 0);

begin
  with Helper, DC.Save do
  try
    const WinControlWnd = FWinControl.Handle;
    const BevelKind     = FWinControl.GetProtectedBevelKind;

    if (BevelKind <> bkNone) or (FWinControl.GetProtectedBorderWidth > 0) then
    begin
      var ClientRect := WinControlWnd.GetClientRect;
      var WindowRect := WinControlWnd.GetWindowRect;

      WinControlWnd.MapWindowRectFromScreen(WindowRect);
      ClientRect.OffsetRect(-WindowRect.TopLeft);
      DC.ExcludeClip(ClientRect);

      { Draw borders in non-client area }
      const SavedWindowRect = WindowRect;
      ClientRect.InflateRect(FWinControl.GetProtectedBorderWidth);
      WindowRect := ClientRect;

      const WinStyle = WinControlWnd.GetWindowData(GWL_STYLE);
      if (WinStyle and WS_VSCROLL) <> 0 then
        Inc(WindowRect.Right, GetSystemMetrics(SM_CYVSCROLL));
      if (WinStyle and WS_HSCROLL) <> 0 then
        Inc(WindowRect.Bottom, GetSystemMetrics(SM_CXHSCROLL));

      if BevelKind <> bkNone then
      begin
        const BevelEdges = FWinControl.GetProtectedBevelEdges;
        const BevelInner = FWinControl.GetProtectedBevelInner;
        const BevelOuter = FWinControl.GetProtectedBevelOuter;

        var EdgeSize := 0;

        if BevelInner <> bvNone then
          Inc(EdgeSize, FWinControl.GetProtectedBevelWidth);
        if BevelOuter <> bvNone then
          Inc(EdgeSize, FWinControl.GetProtectedBevelWidth);

        if TBevelEdge.beLeft in BevelEdges then
          Dec(WindowRect.Left, EdgeSize);
        if TBevelEdge.beTop in BevelEdges then
          Dec(WindowRect.Top, EdgeSize);
        if TBevelEdge.beRight in BevelEdges then
          Inc(WindowRect.Right, EdgeSize);
        if TBevelEdge.beBottom in BevelEdges then
          Inc(WindowRect.Bottom, EdgeSize);

        DC.DrawEdge(
          WindowRect,
          InnerStyles[BevelInner] or OuterStyles[BevelOuter],
          Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[FWinControl.GetProtectedCtl3D] or BF_ADJUST
        );
      end;

      DC.IntersectClip(WindowRect);
      WindowRect := SavedWindowRect;
      { Erase parts not drawn }
      WindowRect.OffsetRect(-WindowRect.TopLeft);
      DC.Fill(WindowRect, FWinControl.Brush.Handle);
    end;

    const SavedDCIndex = DC.Save;
    try
      NCPrintControl(WinControlWnd, FWinControl, DC);
    finally
      SavedDCIndex.RestoreDC;
    end;

    const Themed = HasFlags(RCF_THEMEDNC) and IsWinXPUp;
    if Themed or (csNeedsBorderPaint in FWinControl.ControlStyle) then
      PaintThemeBorder(FWinControl, DC);
  finally
    RestoreDC;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure WinControlNCPaint(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  const Helper     = TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);
  WinControlNCPaint(Helper, DC);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure EmulateNCPaint(const Helper: TRenderWindowToDCHelper; const DC: HDC); overload;
begin
  with Helper do
  begin
    if FWinControl = nil then
      Exit;

    if FWinControl is TToolWindow then
      ToolWindowNCPaint(FWinControl as TToolWindow, DC)
    else
    begin
      WinControlNCPaint(
      {$IFDEF FORM_EFFECTS_TESTS}
        FWnd,
        FStopWnd,
        FWinControl,
        FRegControl.Flags,
        FRegControl.NonClientCallback,
        FRegControl.ClientCallback,
      {$ELSE ~ FORM_EFFECTS_TESTS}
        Helper,
      {$ENDIF ~ FORM_EFFECTS_TESTS}
        DC
      );
    end;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure EmulateNCPaint(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  const Helper     = TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);
  EmulateNCPaint(Helper, DC);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure PaintNonClient(const Helper: TRenderWindowToDCHelper; const DC: HDC); overload;
begin
  with Helper do
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
      with DC.Save do
      try
        FWnd.SendMessage(CM_FENCPAINT, DC, FE_ID)
      finally
        RestoreDC;
      end;
    end;

    with DC.Save do
    try
      if
        Assigned(FWinControl) and
        ((FWinControl.GetProtectedBorderWidth > 0) or (FWinControl.GetProtectedBevelKind <> bkNone))
      then
      begin
        const SavedDCIndex = DC.Save;
        try
          EmulateNCPaint(
          {$IFDEF FORM_EFFECTS_TESTS}
            FWnd,
            FStopWnd,
            FWinControl,
            FRegControl.Flags,
            FRegControl.NonClientCallback,
            FRegControl.ClientCallback,
          {$ELSE ~ FORM_EFFECTS_TESTS}
            Helper,
          {$ENDIF ~ FORM_EFFECTS_TESTS}
            DC
          );
        finally
          SavedDCIndex.RestoreDC;
        end;
      end;

      if HasFlags(RCF_PRINTNC) then
        NCPrintControl(FWnd, FWinControl, DC)
      else if HasFlags(RCF_PAINTNC) then
      begin
        NCPrintControl(FWnd, FWinControl, DC);
        FWnd.SendMessage(WM_NCPAINT, 0, DC);
      end
      else if HasFlags(RCF_CALLBACKNC) then
      begin
        if Assigned(FWinControl) and Assigned(FRegControl.NonClientCallback) then
          FRegControl.NonClientCallback(FWinControl, DC);
      end
      else if HasFlags(RCF_PAINTCOPY or RCF_PAINTCOPYNC) then
        PaintCopy(FWinControl, DC)
      else if HasFlags(RCF_BENCPAINT) then
        FWnd.SendMessage(CM_FENCPAINT, DC, FE_ID)
      else if HasFlags(RCF_BEFULLRENDER) then
        FWnd.SendMessage(CM_FEFULLRENDER, DC, FE_ID)
      else
      begin
        EmulateNCPaint(
        {$IFDEF FORM_EFFECTS_TESTS}
          FWnd,
          FStopWnd,
          FWinControl,
          FRegControl.Flags,
          FRegControl.NonClientCallback,
          FRegControl.ClientCallback,
        {$ELSE ~ FORM_EFFECTS_TESTS}
          Helper,
        {$ENDIF ~ FORM_EFFECTS_TESTS}
          DC
        );
      end;
    finally
      RestoreDC;
    end;

    if HasFlags(RCF_BENCPOSTPAINT) then
    begin
      with DC.Save do
      try
        FWnd.SendMessage(CM_FENCPAINT, DC, FE_ID)
      finally
        RestoreDC;
      end;
    end;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure PaintNonClient(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  const Helper     = TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);
  PaintNonClient(Helper, DC);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure PaintClient(const Helper: TRenderWindowToDCHelper; const DC: HDC); overload;
begin
  with Helper do
  begin
    if HasFlags(RCF_BEPREPAINT) then
    begin
      with DC.Save do
      try
        FWnd.SendMessage(CM_FEPAINT, DC, FE_ID)
      finally
        RestoreDC;
      end;
    end;

    with DC.Save do
    try
      if HasFlags(RCF_PRINT) then
        FWnd.SendMessage(WM_PRINT, DC, PRF_ERASEBKGND or PRF_CLIENT)
      else if HasFlags(RCF_PAINT) then
        EraseAndPaintMessage(FWnd, FWinControl, DC)
      else if HasFlags(RCF_CALLBACK) then
      begin
        if Assigned(FRegControl.ClientCallback) then
          FRegControl.ClientCallback(FWinControl, DC);
      end
      else if HasFlags(RCF_BEPAINT) then
        FWnd.SendMessage(CM_FEPAINT, DC, FE_ID)
      else
        EmulatePaint(FWinControl, DC);
    finally
      RestoreDC;
    end;

    if HasFlags(RCF_BEPOSTPAINT) then
    begin
      with DC.Save do
      try
        FWnd.SendMessage(CM_FEPAINT, DC, FE_ID)
      finally
        RestoreDC;
      end;
    end;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure PaintClient(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  const Helper     = TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);
  PaintClient(Helper, DC);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure RenderChildWindows(
  const Helper: TRenderWindowToDCHelper;
  const DC: HDC;
  const ClientOrgPoint: TPoint;
  const Rect: TRect
); overload; inline;
begin
  with Helper do
  begin
    var ChildWnd := FWnd.GetWindow(GW_CHILD);
    if ChildWnd <> 0 then
    begin
      if FIsMaximizedMDIClient then
        ChildWnd := ChildWnd.GetWindow(GW_HWNDFIRST)
      else
        ChildWnd := ChildWnd.GetWindow(GW_HWNDLAST);
    end;

    while (ChildWnd <> 0) and ((ChildWnd <> FStopWnd) or (FIsMDIClient and not FIsMaximizedMDIClient)) do
    begin
      if ChildWnd.IsVisible then
      begin
        var ChildRect    : TRect;
        var ChildOrgPoint: TPoint;

        if FIsMaximizedMDIClient then
        begin
          ChildRect     := Rect;
          ChildOrgPoint := TPoint.Zero;
        end
        else
        begin
          ChildRect := ChildWnd.GetWindowRect;
  //        ScreenToClient(FWindow, ChildRect.TopLeft);
  //        ScreenToClient(FWindow, ChildRect.BottomRight);
          FWnd.MapWindowRectFromScreen(ChildRect);
          ChildRect.OffsetRect(ClientOrgPoint);
          ChildOrgPoint := ChildRect.TopLeft;
          ChildRect := TRect.IntersectRects(ChildRect, Rect);
          ChildRect.OffsetRect(-ChildOrgPoint);
        end;

        if not ChildRect.IsEmptyRect then
        begin
          const Point = DC.OffsetWindowOrg(-ChildOrgPoint);
          with TRenderWindowToDCHelper.Create(ChildWnd, FStopWnd, nil, FRegControl) do
          try
            RenderWindowToDC(DC, ChildRect, True, True);
          finally
            DC.SetWindowOrg(Point);
          end;
        end;
      end;

      if FIsMaximizedMDIClient then
        ChildWnd := 0
      else
        ChildWnd := ChildWnd.GetWindow(GW_HWNDPREV);
    end;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure RenderChildWindows(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
  const DC: HDC;
  const ClientOrgPoint: TPoint;
  const Rect: TRect
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  var Helper      := TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);

  Helper.FIsMaximizedMDIClient := IsMaximizedMDIClient;
  Helper.FIsMaximizedMDIChild  := IsMaximizedMDIChild;
  Helper.FIsMDIClient          := IsMDIClient;

  RenderChildWindows(Helper, DC, ClientOrgPoint, Rect);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure AdjustBrushOrigin(
  const Helper: TRenderWindowToDCHelper;
  const DC: HDC;
  const PaintClientCallback: TPaintClientCallback
); overload;
begin
  const AbsPos = TPoint.Zero;
  AbsPos.ToDevicePoint(DC);

  with Helper, FWnd.CreateDC do
  try
    const BrushOrgPos = GetBrushOrg;
    try
      DC.SetBrushOrg(BrushOrgPos + AbsPos);
      PaintClientCallback(
      {$IFDEF FORM_EFFECTS_TESTS}
        FWnd,
        FStopWnd,
        FWinControl,
        FRegControl.Flags,
        FRegControl.NonClientCallback,
        FRegControl.ClientCallback,
      {$ELSE ~ FORM_EFFECTS_TESTS}
        Helper,
      {$ENDIF ~ FORM_EFFECTS_TESTS}
        DC
      );
    finally
      DC.SetBrushOrg(BrushOrgPos);
    end;
  finally
    ReleaseDC;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure AdjustBrushOrigin(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC;
  const PaintClientCallback: TPaintClientCallback
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  const Helper     = TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);
  AdjustBrushOrigin(Helper, DC, PaintClientCallback);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure AdjustBitmap(
  const Helper: TRenderWindowToDCHelper;
  const DC: HDC;
  const ClientSize: TSize;
  const PaintClientCallback: TPaintClientCallback
); overload; inline;
begin
{$IFDEF FORM_EFFECTS_TESTS}
  with Helper do
  begin
{$ENDIF ~ FORM_EFFECTS_TESTS}
    AdjustBitmap(
      DC,
      ClientSize,
      procedure(const CanvasDC: HDC)
      begin
        PaintClientCallback(
        {$IFDEF FORM_EFFECTS_TESTS}
          FWnd,
          FStopWnd,
          FWinControl,
          FRegControl.Flags,
          FRegControl.NonClientCallback,
          FRegControl.ClientCallback,
        {$ELSE ~ FORM_EFFECTS_TESTS}
          Helper,
        {$ENDIF ~ FORM_EFFECTS_TESTS}
          CanvasDC
        );
      end
    );
{$IFDEF FORM_EFFECTS_TESTS}
  end;
{$ENDIF ~ FORM_EFFECTS_TESTS}
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure AdjustBitmap(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC;
  const ClientSize: TSize;
  const PaintClientCallback: TPaintClientCallback
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  const Helper     = TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);
  AdjustBitmap(Helper, DC, ClientSize, PaintClientCallback);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure CheckClipRegion(
  const Helper: TRenderWindowToDCHelper;
  const DC: HDC;
  const CheckRegion: Boolean;
  const Size: TSize
); overload; inline;
begin
  const WindowRect = TRect.InlineCreate(Size);

  const WindowRgn  = HRGN.Create(WindowRect);
  try
    with Helper do
    begin
      if CheckRegion and (not FIsMaximizedMDIChild) then
        FWnd.GetWindowRgn(WindowRgn);
    end;

    const Point = TPoint.Zero;
    Point.ToDevicePoint(DC);
    WindowRgn.Offset(Point);

//    const Rgn = HRGN.Create(WindowRect);
//    DC.GetClipRgn(Rgn);
    with DC.CreateClipRgn(WindowRect, False) do
    try
      Rgn.Combine(WindowRgn, Rgn);

      const Rect = Rgn.GetRgnBox;
      Rect.FromDeviceRect(DC);
    finally
      DeleteRegion;
//      DC.SelectClipRgn(Rgn);
//      Rgn.Delete;
    end;
  finally
    WindowRgn.Delete;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure CheckClipRegion(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIChild: Boolean;
  const DC: HDC;
  const CheckRegion: Boolean;
  const Size: TSize
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  var Helper      := TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);

  Helper.FIsMaximizedMDIChild := IsMaximizedMDIChild;

  CheckClipRegion(Helper, DC, CheckRegion, Size);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure DoRender(
  const Helper: TRenderWindowToDCHelper;
  const DC: HDC;
  const Size: TSize;
  const Rect: TRect
); overload; inline;
begin
  with Helper do
  begin
    const ClientCallback    = FRegControl.ClientCallback;
    const NonClientCallback = FRegControl.NonClientCallback;

    const RenderClient    = HasFlags(RCF_RENDER  );
    const RenderNonClient = HasFlags(RCF_RENDERNC);
    const CommonPainting  =
      RenderClient    and
      RenderNonClient and
      (
        (
          Assigned(ClientCallback)    and
          Assigned(NonClientCallback) and
          (@ClientCallback = @NonClientCallback)
        )                         or
        HasFlags(RCF_PAINTCOPY)   or
        HasFlags(RCF_PAINTCOPYNC) or
        HasFlags(RCF_BEFULLRENDER)
      );

    if CommonPainting then
    begin
      var Point: TPoint;
      if FIsMaximizedMDIChild then
        Point := DC.OffsetWindowOrg(-FWinControl.Left, -FWinControl.Top);
      try
        if HasFlags(RCF_OWNCANVASNC or RCF_OWNCANVAS) then
        begin
          AdjustBitmap(
          {$IFDEF FORM_EFFECTS_TESTS}
            FWnd,
            FStopWnd,
            FWinControl,
            FRegControl.Flags,
            FRegControl.NonClientCallback,
            FRegControl.ClientCallback,
          {$ELSE ~ FORM_EFFECTS_TESTS}
            Helper,
          {$ENDIF ~ FORM_EFFECTS_TESTS}
            DC,
            Size,
            PaintNonClient
          );
        end
        else
        begin
          AdjustBrushOrigin(
          {$IFDEF FORM_EFFECTS_TESTS}
            FWnd,
            FStopWnd,
            FWinControl,
            FRegControl.Flags,
            FRegControl.NonClientCallback,
            FRegControl.ClientCallback,
          {$ELSE ~ FORM_EFFECTS_TESTS}
            Helper,
          {$ENDIF ~ FORM_EFFECTS_TESTS}
            DC,
            PaintNonClient
          );
        end;
      finally
        if FIsMaximizedMDIChild then
          DC.SetWindowOrg(Point);
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
//        const SaveRgn = HRGN.Zero;
//        DC.GetClipRgn(SaveRgn);
        with DC.CreateClipRgn(False) do
        try
          const Point = DC.OffsetWindowOrg(-ClientOrgPoint);
          try
            DC.IntersectClip(ClizetSize);

            if HasFlags(RCF_OWNCANVAS) then
            begin
              AdjustBitmap(
              {$IFDEF FORM_EFFECTS_TESTS}
                FWnd,
                FStopWnd,
                FWinControl,
                FRegControl.Flags,
                FRegControl.NonClientCallback,
                FRegControl.ClientCallback,
              {$ELSE ~ FORM_EFFECTS_TESTS}
                Helper,
              {$ENDIF ~ FORM_EFFECTS_TESTS}
                DC,
                ClizetSize,
                PaintClient
              );
            end
            else
            begin
              AdjustBrushOrigin(
              {$IFDEF FORM_EFFECTS_TESTS}
                FWnd,
                FStopWnd,
                FWinControl,
                FRegControl.Flags,
                FRegControl.NonClientCallback,
                FRegControl.ClientCallback,
              {$ELSE ~ FORM_EFFECTS_TESTS}
                Helper,
              {$ENDIF ~ FORM_EFFECTS_TESTS}
                DC,
                PaintClient
              );
            end;
          finally
            DC.SetWindowOrg(Point);
          end;
        finally
          DeleteRegion;
//          DC.SelectClipRgn(SaveRgn);
//          SaveRgn.Delete;
        end;
      end;

//      const HasNonClientArea = (ClizetSize.Width <> Size.Width) or (ClizetSize.Height <> Size.Height);
//
//      if HasNonClientArea and RenderNonClient then

      if (ClizetSize <> Size) and RenderNonClient then
      begin
        // Remember current clipping region
//        const SaveRgn = HRGN.Zero;
//        DC.GetClipRgn(SaveRgn);
        with DC.CreateClipRgn(False) do
        try
          DC.ExcludeClip(ClientOrgPoint, ClizetSize);

          var Point: TPoint;
          if FIsMaximizedMDIChild then
            Point := DC.OffsetWindowOrg(-FWinControl.Left, -FWinControl.Top);
          try
            if HasFlags(RCF_OWNCANVASNC) then
            begin
              AdjustBitmap(
              {$IFDEF FORM_EFFECTS_TESTS}
                FWnd,
                FStopWnd,
                FWinControl,
                FRegControl.Flags,
                FRegControl.NonClientCallback,
                FRegControl.ClientCallback,
              {$ELSE ~ FORM_EFFECTS_TESTS}
                Helper,
              {$ENDIF ~ FORM_EFFECTS_TESTS}
                DC,
                Size,
                PaintNonClient
              );
            end
            else
            begin
              AdjustBrushOrigin(
              {$IFDEF FORM_EFFECTS_TESTS}
                FWnd,
                FStopWnd,
                FWinControl,
                FRegControl.Flags,
                FRegControl.NonClientCallback,
                FRegControl.ClientCallback,
              {$ELSE ~ FORM_EFFECTS_TESTS}
                Helper,
              {$ENDIF ~ FORM_EFFECTS_TESTS}
                Dc,
                PaintNonClient
              );
            end;
          finally
            if FIsMaximizedMDIChild then
              DC.SetWindowOrg(Point);
          end;
        finally
          DeleteRegion;
//          DC.SelectClipRgn(SaveRgn);
//          SaveRgn.Delete;
        end;
      end;
    end;

    if (FWinControl = nil) or not (FWinControl is TOleControl) then
    begin
      var ClizetSize: TSize;
      var ClientOrgPoint: TPoint;
      GetClientSize(FWnd, FWinControl, FIsMaximizedMDIChild, ClizetSize, ClientOrgPoint);

//      const SaveRgn = HRGN.Zero;
//      DC.GetClipRgn(SaveRgn);
      with DC.CreateClipRgn(False) do
      try
        DC.IntersectClip(ClientOrgPoint, ClizetSize);
        RenderChildWindows(
        {$IFDEF FORM_EFFECTS_TESTS}
          FWnd,
          FStopWnd,
          FWinControl,
          FRegControl.Flags,
          FRegControl.NonClientCallback,
          FRegControl.ClientCallback,
          FIsMaximizedMDIClient,
          FIsMaximizedMDIChild,
          FIsMDIClient,
        {$ELSE ~ FORM_EFFECTS_TESTS}
          Helper,
        {$ENDIF ~ FORM_EFFECTS_TESTS}
          DC,
          ClientOrgPoint,
          Rect
        );
      finally
        DeleteRegion;
//        DC.SelectClipRgn(SaveRgn);
//        SaveRgn.Delete;
      end;
    end;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure DoRender(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const IsMaximizedMDIClient, IsMaximizedMDIChild, IsMDIClient: Boolean;
  const DC: HDC;
  const Size: TSize;
  const Rect: TRect
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  var Helper      := TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);

  Helper.FIsMaximizedMDIClient := IsMaximizedMDIClient;
  Helper.FIsMaximizedMDIChild  := IsMaximizedMDIChild;
  Helper.FIsMDIClient          := IsMDIClient;

  DoRender(Helper, DC, Size, Rect);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

procedure RenderWindowToDCAux(
  var Helper: TRenderWindowToDCHelper;
  const DC: HDC;
  const Rect: TRect;
  const CheckVisibility, CheckRegion: Boolean
); overload; inline;
begin
  with Helper do
  begin
    if not CheckVisibility or FWnd.IsVisible then
    begin
      LoadWinControlData;
    {$IFDEF USE_TRANSITION_EFFECTS}
      if FIsRenderWindow then
        Exit;
    {$ENDIF ~ USE_TRANSITION_EFFECTS}
      // Remember current clipping region
      with DC.CreateClipRgn(False) do
      try
        const Size = GetWindowSize(FWnd, FIsMaximizedMDIChild);
        CheckClipRegion(
        {$IFDEF FORM_EFFECTS_TESTS}
          FWnd,
          FStopWnd,
          FWinControl,
          FRegControl.Flags,
          FRegControl.NonClientCallback,
          FRegControl.ClientCallback,
          FIsMaximizedMDIChild,
        {$ELSE ~ FORM_EFFECTS_TESTS}
          Helper,
        {$ENDIF ~ FORM_EFFECTS_TESTS}
          DC,
          CheckRegion,
          Size
        );
        if not Rect.IsEmptyRect then
        begin
          GetRegControl(
            FWnd,
            FWinControl,
          {$IFDEF FORM_EFFECTS_TESTS}
            FRegControl.Flags,
            FRegControl.NonClientCallback,
            FRegControl.ClientCallback
          {$ELSE ~ FORM_EFFECTS_TESTS}
            FRegControl
          {$ENDIF ~ FORM_EFFECTS_TESTS}
          );
          DoRender(
          {$IFDEF FORM_EFFECTS_TESTS}
            FWnd,
            FStopWnd,
            FWinControl,
            FRegControl.Flags,
            FRegControl.NonClientCallback,
            FRegControl.ClientCallback,
            FIsMaximizedMDIClient,
            FIsMaximizedMDIChild,
            FIsMDIClient,
          {$ELSE ~ FORM_EFFECTS_TESTS}
            Helper,
          {$ENDIF ~ FORM_EFFECTS_TESTS}
            DC,
            Size,
            Rect
          );
        end;
      finally
        DeleteRegion;
      end;
    end;
  end;
end;

{$IFDEF FORM_EFFECTS_TESTS}
procedure RenderWindowToDCAux(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const Flags: DWORD;
  const NonClientCallback, ClientCallback: TFEPaintCallback;
  const DC: HDC;
  const Rect: TRect;
  const CheckVisibility, CheckRegion: Boolean
);
begin
  const RegControl = TRegControl.Create(Flags, NonClientCallback, ClientCallback);
  var Helper      := TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl, RegControl);
  RenderWindowToDCAux(Helper, DC, Rect, CheckVisibility, CheckRegion);
end;
{$ENDIF ~ FORM_EFFECTS_TESTS}

{ TRenderWindowToDCHelper }

class function TRenderWindowToDCHelper.Create(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const RegControl: TRegControl
): TRenderWindowToDCHelper;
begin
  with Result do
  begin
    FWnd        := Wnd;
    FStopWnd    := StopWnd;
    FRegControl := RegControl;

    if Assigned(WinControl) then
      FWinControl := WinControl
    else
      FWinControl := FindControl(Wnd);
  end;
end;

class function TRenderWindowToDCHelper.Create(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl
): TRenderWindowToDCHelper;
begin
  Result := TRenderWindowToDCHelper.Create(Wnd, ResolveStopWnd(Wnd, StopWnd), WinControl, TRegControl.Create);
end;

procedure TRenderWindowToDCHelper.LoadWinControlData;
var
  ClassName: string;
//  WinControlType: TClass;

begin
  if Assigned(FWinControl) then
  begin
    ClassName       := FWinControl.ClassName;
//    WinControlType := FWinControl.ClassType;

    if IsMDIFormWithMaximizedMDIChild(FWinControl) then
    begin // Edge changing
      const MainFormWnd = Application.MainForm.ClientHandle;
      MainFormWnd.SetWindowData(GWL_EXSTYLE, MainFormWnd.GetWindowData(GWL_EXSTYLE) and not WS_EX_CLIENTEDGE);
      MainFormWnd.SetWindowPos(0, SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    end;

    FIsMaximizedMDIClient := False;
    FIsMaximizedMDIChild  := IsMaximizedMDIChild(FWinControl);
  end
  else
  begin
    ClassName       := FWnd.GetWindowClassName;
//    WinControlType := GetClass(ClassName);

    FIsMaximizedMDIClient := IsMaximizedMDIClient(ClassName);
    FIsMaximizedMDIChild  := False;
  end;

  FIsMDIClient    := SameText(ClassName, MDI_CLIENT_CLASS_NAME);
{$IFDEF USE_TRANSITION_EFFECTS}
  FIsRenderWindow := SameText(ClassName, TFERenderWindow.ClassName);
  {$MESSAGE 'Need to implement `TFERenderWindow` class'}
{$ENDIF ~ USE_TRANSITION_EFFECTS}
end;

function TRenderWindowToDCHelper.HasFlags(const Flags: DWORD): Boolean;
begin
  Result := FRegControl.Flags and Flags <> 0;
end;

procedure TRenderWindowToDCHelper.RenderWindowToDC(
  const DC: HDC;
  const Rect: TRect;
  const CheckVisibility, CheckRegion: Boolean
);
begin
  RenderWindowToDCAux(
  {$IFDEF FORM_EFFECTS_TESTS}
    FWnd,
    FStopWnd,
    FWinControl,
    FRegControl.Flags,
    FRegControl.NonClientCallback,
    FRegControl.ClientCallback,
  {$ELSE ~ FORM_EFFECTS_TESTS}
    Self,
  {$ENDIF ~ FORM_EFFECTS_TESTS}
    DC,
    Rect,
    CheckVisibility,
    CheckRegion
  );
end;

{$ENDREGION 'Internal definitions'}

procedure RenderWindowToDC(
  const Wnd, StopWnd: HWND;
  const WinControl: TWinControl;
  const DC: HDC;
  const Rect: TRect;
  const ClientCoordinates, CheckVisibility, CheckRegion: Boolean
);
begin
  if (Wnd = StopWnd) or (CheckVisibility and not Wnd.IsVisible) then
    Exit;

  var TempRect := Rect;
  var Point    := TPoint.Zero;

  if ClientCoordinates and not IsMaximizedMDIChild(WinControl) then
  begin
    const ClientOffset = GetWindowOffset(Wnd);
    TempRect.OffsetRect(ClientOffset);
    Point := DC.OffsetWindowOrg(ClientOffset);
  end;

  with TRenderWindowToDCHelper.Create(Wnd, StopWnd, WinControl) do
  try
    RenderWindowToDC(DC, TempRect, False, CheckRegion);
  finally
    if not Rect.IsEqual(TempRect) then
      DC.SetWindowOrg(Point);
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
  if Assigned(RegControlsManager) then
    FreeAndNil(RegControlsManager);

end.
