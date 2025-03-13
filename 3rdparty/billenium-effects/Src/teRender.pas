unit teRender;

interface

{$INCLUDE teDefs.inc}
{$RANGECHECKS OFF}

uses
  Windows, Messages, SysUtils, Classes, Consts, Graphics,
  {$ifndef NoVCL}
  Forms, Controls,
  {$endif NoVCL}
  MultiMon;

{$ifndef NoVCL}
{$ifndef TE_NOHLP}
const // Billenium Effects messages
  // Thir party components interface
  BE_ID           = $41A2;
  BE_BASE         = CM_BASE + $0C4A;
  CM_BEPAINT      = BE_BASE + 0; // Paint client area to Billenium Effects' DC
  CM_BENCPAINT    = BE_BASE + 1; // Paint non client area to Billenium Effects' DC
  CM_BEFULLRENDER = BE_BASE + 2; // Paint whole control to Billenium Effects' DC
  CM_BEWAIT       = BE_BASE + 3; // Don't execute effect yet
  CM_BERUN        = BE_BASE + 4; // Execute effect now!
  // Internal messages
  CM_BEFORMSHOWN  = BE_BASE + 5; // The form is now visible for the user
  CM_MDIDESTROY   = BE_BASE + 6; // An MDI form is being destroyed

  {$EXTERNALSYM LWA_ALPHA}
  LWA_ALPHA = $00000002;

{$endif TE_NOHLP}

const
  teAuto           = $00000000;
  tePaint          = $00000001;
  tePrint          = $00000002;
  teEmulate        = $00000003;
  teCallback       = $00000004;
  tePaintCopy      = $00000005;

  teThemed         = $10000000;
  {$ifndef TE_NOHLP}
  teRefreshFocused = $20000000;
  {$endif TE_NOHLP}
  teOwnCanvas      = $40000000;
  teNoRender       = $80000000;
{$endif NoVCL}

type
  TTEWinVersion = (teWinUnknown, teWin32s, teWin95, teWin98, teWin98SE, teWinME,
    teWinNT, teWin2000, teWinXP, teWin2003, teWinVista, teWin7, teWinFuture);

  {$ifndef TE_NOHLP}
  TDWordArray = array[0..65535] of DWord;
  PDWordArray = ^TDWordArray;
  TTEProcessorInfo = record
    MMX,
    SSE: Boolean;
  end;
  {$endif TE_NOHLP}

  {$ifndef NoVCL}
  TTEPaintCallback = procedure(Control: TWinControl; DC: HDC);
  {$endif NoVCL}

  procedure AdjustBmpForTransition(Bmp: TBitmap; Palette: HPalette;
    Width, Height: Integer; PixelFormat: TPixelFormat);
  function  DeviceBitsPerPixel(
    Recalculate: Boolean = False): Integer;
  function  DevicePixelFormat(
    Recalculate: Boolean = False): TPixelFormat;
  function  GetBytesPerScanline(Bitmap: TBitmap; PixelFormat: TPixelFormat;
    Alignment: Longint = 32): Longint;
  function  GetSnapShotImage(R: TRect; PixelFormat: TPixelFormat;
    GrabLayeredWindows: Boolean): TBitmap;
  function  PalettedDevice(
    Recalculate: Boolean = False): Boolean;
  function  RGBDevice(Recalculate: Boolean = False): Boolean;
  function  BilleniumEffectsVersion: String;
  {$ifndef TE_NOHLP}
  function  GetPixelFormatBPP(PixelFormat: TPixelFormat): Integer;
  function  GetBitmapGap(Bitmap: TBitmap; PixelFormat: TPixelFormat): Integer;
  procedure GetSolidColorBmp(Bmp: TBitmap; Width, Height: Integer;
    Color: TColor; Palette: HPalette; PixelFormat: TPixelFormat);
  function  IsWindowClipped(Window, AvoidWnd: HWND; R: TRect): Boolean;
  {$endif TE_NOHLP}

  {$ifndef NoVCL}
  function  IsWindowLayered(Window: HWND): Boolean;
  procedure RegisterTEControl(const ControlClassName: String;
    NonClientRenderMode, ClientRenderMode: DWord;
    RefreshNonClient, RefreshClient: Boolean);
  procedure RegisterTEControlCallback(const ControlClassName: String;
    NonClientRenderMode, ClientRenderMode: DWord;
    RefreshNonClient, RefreshClient: Boolean;
    NonClientCallback, ClientCallback: TTEPaintCallback);
  function  RenderControl(Control: TControl; StopWnd: Hwnd; R: TRect;
    ClientCoordinates, CheckRegion, Fast: Boolean;
    PixelFormat: TPixelFormat): TBitmap;
  {$ifndef TE_NOHLP}
  function  ControlClientAreaHasRegion(Control: TWinControl): Boolean;
  function  ControlClientOffset(Control: TControl): TPoint;
  function  WindowClientOffset(Window: HWND): TPoint;
  function  ControlClientOrigin(Control: TControl): TPoint;
  function  ControlScreenToClient(Control: TControl; Point: TPoint): TPoint;
  function  ControlClientToScreen(Control: TControl; Point: TPoint): TPoint;
  function  ControlClientRect(Control: TControl): TRect;
  function  ControlClientHeight(Control: TControl): Integer;
  function  ControlClientWidth(Control: TControl): Integer;
  function  GetMaximizedMDIChild(WinControl: TWinControl): Boolean;
  function  IsScrollBarVisible(Control: TControl; Window: HWND;
    Kind: TScrollBarKind): Boolean;
  // Dwm API
  function  IsCompositionEnabled: Boolean;
  procedure DisableDwmTransitions(Window: HWND);
  function  DwmSetWindowAttribute(Hwnd: HWND; dwAttribute: DWORD;
    pvAttribute: Pointer; cbAttribute: DWORD): HResult;
  function  RealizeControlPalette(Control: TControl;
    ForceBackground: Boolean): Boolean;
  procedure RefreshWindows(Window: HWND);
  function RenderWindowToBmp(Bmp: TBitmap;
    Window, StopWnd: HWND;
    WinControl: TWinControl;
    R: TRect; ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean;
    PixelFormat: TPixelFormat): TBitmap;
  procedure RenderWindowToDC(Window, StopWnd: HWND; WinControl: TWinControl;
    DC: HDC; R: TRect;
    ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean);
  function ClassInheritsFrom(const ClassType: TClass;
    const ClassName: String): Boolean;
  function WindowHasRegion(Window: HWnd): Boolean;
  procedure HookAPICall(Dll, Name: string; OrgAPICall, NewAPICall: Pointer;
    var SaveAPICall: Pointer; IATPatching: Boolean);
  procedure UnhookAPICall(OrgAPICall, NewAPICall: Pointer;
    var SaveAPICall: Pointer; IATPatching: Boolean);
  {$endif TE_NOHLP}
  {$endif NoVCL}

{$ifndef TE_NOHLP}
type
  TUpdateLayeredWindow        = function (Hwnd: THandle; hdcDst: HDC;
    pptDst: PPoint; psize: PSize; hdcSrc: HDC; pptSrc: PPoint; crKey: COLORREF;
    pblend: PBlendFunction; dwFlags: DWord): Boolean; stdcall;
  TGetLayeredWindowAttributes = function (Hwnd: THandle; out pcrKey: COLORREF;
    out pbAlpha: Byte; out pdwFlags: DWORD): Boolean; stdcall;
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF;
    bAlpha: Byte; dwFlags: DWORD): Boolean; stdcall;

var
  TEProcessorInfo: TTEProcessorInfo;
  TEWinVersion: TTEWinVersion;
  {$ifndef NOVCL}
  UpdateLayeredWindow: TUpdateLayeredWindow = nil;
  GetLayeredWindowAttributes: TGetLayeredWindowAttributes = nil;
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;
//  TECurBmp: TBitmap = nil;
  {$endif TE_NOHLP}
  TEXPRenderDisabled,
  TEAPIHooksDisabled,
  TEIsRunTimePackage: Boolean;
  {$endif NOVCL}

implementation

{$ifndef NOVCL}
uses
  FlatSB,
  {$ifdef D7UP} Themes, {$endif D7UP}
  {$ifdef D11UP} UxTheme, {$endif D11UP}
  TypInfo, OleCtrls, ActiveX, RichEdit;
{$endif NOVCL}

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

  // Dwm API
  DWMWA_TRANSITIONS_FORCEDISABLED = 3;

{$ifndef NoVCL}
type
  TTEControl    = class(TControl);
  TTEWinControl = class(TWinControl);
  TTECustomForm = class(TCustomForm);

  TTERegControl = class(TObject)
  public
    Flags: DWord;
    NonClientCallback,
    ClientCallback: TTEPaintCallback;

    constructor Create(FlagsValue: DWord;
      NonClientCallbackValue, ClientCallbackValue: TTEPaintCallback);
    procedure Assign(Source: TTERegControl);
    procedure Clear;
  end;

  TTERegControls = class(TStringList)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure FindRegControl(Control: TWinControl; ControlClass: TControlClass;
      var Data: TTERegControl);
    procedure SaveRegControl(ControlClassName: String; Flags: DWord;
      NonClientCallback, ClientCallback: TTEPaintCallback);
  end;
{$endif NoVCL}

var
  {$ifndef NoVCL}
  TERegControls: TTERegControls;
  // Dwm API
  hDWMAPI: THandle = 0;
  _DwmIsCompositionEnabled: function(out pfEnabled: BOOL): HResult; stdcall = nil;
  _DwmSetWindowAttribute: function (Hwnd: HWND; dwAttribute: DWORD;
    pvAttribute: Pointer; cbAttribute: DWORD): HResult; stdcall = nil;
  // Hooks
  HookDCCount: Integer = 0;
  HookDC: HDC   = 0;
  HookWnd: HWND = 0;
  SaveGetDC      : function(hWnd: HWND): HDC; stdcall = nil;
  SaveGetDCEx    : function(hWnd: HWND; hrgnClip: HRGN; flags: DWORD): HDC; stdcall = nil;
  SaveGetWindowDC: function(hWnd: HWND): HDC; stdcall = nil;
  SaveReleaseDC  : function(hWnd: HWND; hDC: HDC): Integer; stdcall = nil;
  SaveBeginPaint : function(hWnd: HWND; var lpPaint: TPaintStruct): HDC; stdcall = nil;
  SaveEndPaint   : function(hWnd: HWND; const lpPaint: TPaintStruct): BOOL; stdcall = nil;
  {$endif NoVCL}

  FDevicePixelFormat: TPixelFormat;
  FDeviceBitsPerPixel: Integer;

{ Global routines }

function CreateIdentityPalette: HPalette;
var
  DC: HDC;
  SysPalSize: Integer;
  Pal: TMaxLogPalette;
begin
  DC := GetDC(0);
  try
    SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
    Pal.palVersion := $300;
    Pal.palNumEntries := SysPalSize;
    GetSystemPaletteEntries(DC, 0, SysPalSize, Pal.palPalEntry);
    Result := CreatePalette(PLogPalette(@Pal)^);
  finally
    ReleaseDC(0, DC);
  end;
end;

{$ifdef Trial}
{$include trial\taux5.inc}
{$endif Trial}

procedure AdjustBmpForTransition(Bmp: TBitmap; Palette: HPalette;
  Width, Height: Integer; PixelFormat: TPixelFormat);
begin
  Bmp.PixelFormat := PixelFormat;
  case PixelFormat of
    pf1bit : Bmp.Monochrome := True;
    pf8bit : begin
               if Palette = 0
               then Bmp.Palette := CreateIdentityPalette
               else Bmp.Palette := Palette;
             end;
  end;
  Bmp.Width  := Width;
  Bmp.Height := Height;
end;

function DeviceBitsPerPixel(Recalculate: Boolean): Integer;
var
  DC: HDC;
begin
  if Recalculate then
  begin
    DC := GetDC(0);
    try
      FDeviceBitsPerPixel := 
        GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
    finally
      ReleaseDC(0, DC);
    end;
  end;
  Result := FDeviceBitsPerPixel;
end;

function Check16bpp: TPixelFormat;
var
  hdcDisplay,
  hdcBitmap: HDC;
  Bitmap,
  OldBitmap: HBitmap;
  Green: Byte;
  Count: Integer;
  PrevGPixel: DWord;
  PrevPixel,
  NewPixel: TColorRef;
begin
  hdcDisplay := GetDC(0);
  try
    hdcBitmap  := CreateCompatibleDC(hdcDisplay);
    try
      Bitmap := CreateCompatibleBitmap(hdcDisplay, 10, 10);
      try
        OldBitmap := SelectObject(hdcBitmap, Bitmap);
        try
          PrevGPixel := 255;
          Count := 0;
          for Green := 0 to 255 do
          begin
            NewPixel := RGB(0, Green, 0);
            SetPixel(hdcBitmap, 1, 1, NewPixel);
            PrevPixel  := GetPixel(hdcBitmap, 1, 1);
            if GetGValue(PrevPixel) <> PrevGPixel then
              Inc(Count);
            PrevGPixel := GetGValue(PrevPixel);
          end;
          if Count > 32
          then Result := pf16bit
          else Result := pf15bit;
        finally
          SelectObject(hdcBitmap, OldBitmap);
        end;
      finally
        DeleteObject(Bitmap);
      end;
    finally
      DeleteDC(hdcBitmap);
    end;
  finally
    ReleaseDC(0, hdcDisplay);
  end;
end;

function DevicePixelFormat(Recalculate: Boolean): TPixelFormat;
begin
  if Recalculate then
  begin
    case DeviceBitsPerPixel(True) of
      1  : FDevicePixelFormat := pf1bit;
      4  : FDevicePixelFormat := pf4bit;
      8  : FDevicePixelFormat := pf8bit;
      15 : FDevicePixelFormat := pf15bit;
      16 : FDevicePixelFormat := Check16bpp;
      24 : FDevicePixelFormat := pf24bit;
      32 : FDevicePixelFormat := pf32bit;
      else FDevicePixelFormat := pf24bit;
    end;
  end;
  Result := FDevicePixelFormat;
end;

function GetBytesPerScanline(Bitmap: TBitmap;
  PixelFormat: TPixelFormat;
  Alignment: Longint): Longint;
var
  PixelsPerScanline: Longint;
begin
  if PixelFormat in [pfDevice, pfCustom] then
  begin
    Result := 0;
    exit;
  end;
  PixelsPerScanline := Bitmap.Width;
  Dec(Alignment);
  Result := ((PixelsPerScanline * GetPixelFormatBPP(PixelFormat) + Alignment)
    and not Alignment) div 8;
end;

function GetSnapShotImage(R: TRect; PixelFormat: TPixelFormat;
  GrabLayeredWindows: Boolean): TBitmap;
const
  CAPTUREBLT = $40000000;
var
  ScreenDC: HDC;
  RopCode: Cardinal;
begin
  Result := TBitmap.Create;
  try
    Result.Canvas.Lock;
    try
      AdjustBmpForTransition(Result, 0, R.Right - R.Left,
        R.Bottom - R.Top, PixelFormat);
      ScreenDC := GetDC(0);
      try
        if GrabLayeredWindows and (TEWinVersion >= teWin2000)
        then RopCode := cmSrcCopy or CAPTUREBLT
        else RopCode := cmSrcCopy;
        BitBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height,
          ScreenDC, R.Left, R.Top, RopCode);
      finally
        ReleaseDC(0, ScreenDC);
      end;
    finally
      Result.Canvas.Unlock;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function PalettedDevice(Recalculate: Boolean): Boolean;
begin
  Result := DeviceBitsPerPixel(Recalculate) = 8;
end;

function RGBDevice(Recalculate: Boolean): Boolean;
begin
  Result := DeviceBitsPerPixel(Recalculate) > 8;
end;

function GetPixelFormatBPP(PixelFormat: TPixelFormat): Integer;
const
  BitCounts: array [pf1Bit..pf32Bit] of Byte = (1,4,8,16,16,24,32);
begin
  if PixelFormat <> pfCustom
  then Result := BitCounts[PixelFormat]
  else Result := BitCounts[DevicePixelFormat(False)];
end;

function GetBitmapGap(Bitmap: TBitmap; PixelFormat: TPixelFormat): Integer;
begin
  Result :=
    GetBytesPerScanline(Bitmap, PixelFormat, 32) -
    GetBytesPerScanline(Bitmap, PixelFormat, 8);
end;

procedure GetSolidColorBmp(Bmp: TBitmap; Width, Height: Integer; Color: TColor;
  Palette: HPalette; PixelFormat: TPixelFormat);
begin
  Bmp.Canvas.Lock;
  try
    AdjustBmpForTransition(Bmp, Palette, Width, Height, PixelFormat);
    Bmp.Canvas.Brush.Color := Color;
    Bmp.Canvas.FillRect(Rect(0, 0, Width+1, Height+1));
  finally
    Bmp.Canvas.Unlock;
  end;
end;

function IsWindowClipped(Window, AvoidWnd: HWND; R: TRect): Boolean;
var
  WndBak,
  Sibling: hWnd;
  R2,
  R3: TRect;
begin
  // Check if the rect is contained in the window (self-clipping)
  GetWindowRect(Window, R2);
  IntersectRect(R3, R, R2);
  Result := not EqualRect(R, R3);

  if(not Result) and (Window <> 0) then
  begin
    // Check if clipped by the screen
    R2 := Bounds(
            GetSystemMetrics(SM_XVIRTUALSCREEN ),
            GetSystemMetrics(SM_YVIRTUALSCREEN ),
            GetSystemMetrics(SM_CXVIRTUALSCREEN),
            GetSystemMetrics(SM_CYVIRTUALSCREEN));
    IntersectRect(R3, R, R2);
    Result := not EqualRect(R, R3);

    WndBak := Window;
    while(not Result) and (Window <> 0) do
    begin
      // Check if covered by siblings 'over' in the z-order
      Sibling := GetWindow(Window, GW_HWNDPREV);
      while(not Result) and (Sibling <> 0) do
      begin
        if IsWindowVisible(Sibling) and (Sibling <> AvoidWnd) then
        begin
          GetWindowRect(Sibling, R2);
          Result := IntersectRect(R3, R, R2);
        end;
        Sibling := GetWindow(Sibling, GW_HWNDPREV);
      end;

      if not Result then
      begin
        Window := GetParent(Window);
        if Window <> 0 then
        begin
          if(GetWindowLong(Window, GWL_STYLE) and WS_CHILD) <> 0
          then
          begin
            if Window <> AvoidWnd then
            begin
              // Check if clipped by this parent window
              GetWindowRect(Window, R2);
              IntersectRect(R3, R, R2);
              Result := not EqualRect(R, R3);
            end;
          end
          else
          begin
            // Check if covered by top level windows 'over' in the z-order
            Window := WndBak;
            while(Window <> 0) and not Result do
            begin
              Window := GetWindow(Window, GW_HWNDPREV);
              if(Window <> 0)           and
                IsWindowVisible(Window) and
                (Window <> AvoidWnd)    and
                (not IsIconic(Window))  then
              begin
                GetWindowRect(Window, R2);
                Result := IntersectRect(R3, R, R2);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure GetProcessorInfo;
const
  // Symbolic constants for feature flags in CPUID standard feature flags
  CPUID_STD_FPU         = $00000001;
  CPUID_STD_VME         = $00000002;
  CPUID_STD_DEBUGEXT    = $00000004;
  CPUID_STD_4MPAGE      = $00000008;
  CPUID_STD_TSC         = $00000010;
  CPUID_STD_MSR         = $00000020;
  CPUID_STD_PAE         = $00000040;
  CPUID_STD_MCHKXCP     = $00000080;
  CPUID_STD_CMPXCHG8B   = $00000100;
  CPUID_STD_APIC        = $00000200;
  CPUID_STD_SYSENTER    = $00000800;
  CPUID_STD_MTRR        = $00001000;
  CPUID_STD_GPE         = $00002000;
  CPUID_STD_MCHKARCH    = $00004000;
  CPUID_STD_CMOV        = $00008000;
  CPUID_STD_PAT         = $00010000;
  CPUID_STD_PSE36       = $00020000;
  CPUID_STD_MMX         = $00800000;
  CPUID_STD_FXSAVE      = $01000000;
  CPUID_STD_SSE         = $02000000;

  // Symbolic constants for feature flags in CPUID extended feature flags
  CPUID_EXT_3DNOW       = $80000000;
  CPUID_EXT_AMD_3DNOWEXT= $40000000;
  CPUID_EXT_AMD_MMXEXT  = $00400000;

  // Symbolic constants for application specific feature flags
  FEATURE_CPUID         = $00000001;
  FEATURE_STD_FEATURES  = $00000002;
  FEATURE_EXT_FEATURES  = $00000004;
  FEATURE_TSC           = $00000010;
  FEATURE_MMX           = $00000020;
  FEATURE_CMOV          = $00000040;
  FEATURE_3DNOW         = $00000080;
  FEATURE_3DNOWEXT      = $00000100;
  FEATURE_MMXEXT        = $00000200;
  FEATURE_SSEFP         = $00000400;
  FEATURE_K6_MTRR       = $00000800;
  FEATURE_P6_MTRR       = $00001000;

  function GetFeatureFlags: DWord;
  (* get_feature_flags extracts all features the application wants to know
     about from CPUID information and returns a bit string of application
     specific feature bits. The following design criteria apply:

     1. Processor capabilities should be directly derived from CPUID feature bits
        wherever possible, instead of being derived from vendor strings and
        processor signatures. However, some features are not indicated by CPUID
        feature flags (whether basic or extended) and do require looking at
        vendor strings and processor signatures. Applications may also choose to
        implement pseudo capabilities, for example indicating performance
        levels.
     2. The basic feature flags returned by CPUID function #1 are compatible
        across all x86 processor vendors with very few exceptions and therefore
        common feature checks for things like MMX or TSC support do not require
        a vendor check before evaluating the basic feature flag information.
        If unsure about a particular feature, review the processor vendor’s
        literature.
     3. 3DNow! is an open standard. Therefore 3DNow! capabilities are indicated
        by bit 31 in the extended feature flags regardless of processor vendor.
     4. Applications should always treat the floating-point part of SSE and
        the MMX part of SSE as separate capabilities because SSE FP requires
        OS support that might not be available, while SSE MMX works with all
        operating systems.
  *)
  var
    Signature: DWord;
    Vendor: array[0..12] of char;
    PVendor: PChar;

    // Define known vendor strings here
    VendorAMD: array[0..12] of char;
    PVendorAMD: PChar;
  begin
    Result := 0;
    Signature := 0;
    PVendor := Vendor;
    StrCopy(PVendor, 'UnknownVendr');
    PVendorAMD := VendorAMD;
    StrCopy(PVendorAMD, 'AuthenticAMD');

    // Step 1: Check if processor has CPUID support. Processor faults
    // with an illegal instruction exception if the instruction is not
    // supported. This step catches the exception and immediately returns
    // with feature string bits with all 0s, if the exception occurs.
    try
      asm
        xor eax, eax
        xor ebx, ebx
        xor ecx, ecx
        xor edx, edx
        db $0F,$A2               
      end;
    except
      exit;
    end;

    Result := Result or FEATURE_CPUID;

    asm
      // Step 2: Check if CPUID supports function 1 (signature/std features)

      xor     eax, eax                      // CPUID function #0
      db $0F,$A2              
      mov     dword ptr [vendor], ebx       // save
      mov     dword ptr [vendor+4], edx     //  vendor
      mov     dword ptr [vendor+8], ecx     //   string
      test    eax, eax                      // largest standard function==0?
      jz      @no_standard_features         // yes, no standard features func
      or      [result], FEATURE_STD_FEATURES// does have standard features

      // Step 3: Get standard feature flags and signature

      mov     eax, 1                        // CPUID function #1
      db $0F,$A2               
      mov     [signature], eax              // save processor signature

      // Step 4: Extract desired features from standard feature flags

      // Check for time stamp counter support

      mov     ecx, CPUID_STD_TSC            // bit 4 indicates TSC support
      and     ecx, edx                      // supports TSC ? CPUID_STD_TSC:0
      neg     ecx                           // supports TSC ? CY : NC
      sbb     ecx, ecx                      // supports TSC ? 0xffffffff:0
      and     ecx, FEATURE_TSC              // supports TSC ? FEATURE_TSC:0
      or      [result], ecx                 // merge into feature flags

      // Check for MMX support

      mov     ecx, CPUID_STD_MMX            // bit 23 indicates MMX support
      and     ecx, edx                      // supports MMX ? CPUID_STD_MMX:0
      neg     ecx                           // supports MMX ? CY : NC
      sbb     ecx, ecx                      // supports MMX ? 0xffffffff:0
      and     ecx, FEATURE_MMX              // supports MMX ? FEATURE_MMX:0
      or      [result], ecx                 // merge into feature flags

      // Check for CMOV support

      mov     ecx, CPUID_STD_CMOV           // bit 15 indicates CMOV support
      and     ecx, edx                      // supports CMOV?CPUID_STD_CMOV:0
      neg     ecx                           // supports CMOV ? CY : NC
      sbb     ecx, ecx                      // supports CMOV ? 0xffffffff:0
      and     ecx, FEATURE_CMOV             // supports CMOV ? FEATURE_CMOV:0
      or      [result], ecx                 // merge into feature flags

      // Check support for P6-style MTRRs

      mov     ecx, CPUID_STD_MTRR           // bit 12 indicates MTRR support
      and     ecx, edx                      // supports MTRR?CPUID_STD_MTRR:0
      neg     ecx                           // supports MTRR ? CY : NC
      sbb     ecx, ecx                      // supports MTRR ? 0xffffffff:0
      and     ecx, FEATURE_P6_MTRR          // supports MTRR ? FEATURE_MTRR:0
      or      [result], ecx                 // merge into feature flags

      // Check for initial SSE support. There can still be partial SSE
      // support. Step 9 will check for partial support.

      mov     ecx, CPUID_STD_SSE            // bit 25 indicates SSE support
      and     ecx, edx                      // supports SSE ? CPUID_STD_SSE:0
      neg     ecx                           // supports SSE ? CY : NC
      sbb     ecx, ecx                      // supports SSE ? 0xffffffff:0
      and     ecx, (FEATURE_MMXEXT+FEATURE_SSEFP) // supports SSE ?
                                            // FEATURE_MMXEXT+FEATURE_SSEFP:0
      or      [result], ecx                 // merge into feature flags

      // Step 5: Check for CPUID extended functions

      mov     eax, $80000000               // extended function 0x80000000
      db $0F,$A2               
      cmp     eax, $80000000               // no function > 0x80000000 ?
      jbe     @no_extended_features         // yes, no extended feature flags
      or      [result], FEATURE_EXT_FEATURES// does have ext. feature flags

      // Step 6: Get extended feature flags

      mov     eax, $80000001               // CPUID ext. function 0x80000001
      db $0F,$A2               

      // Step 7: Extract vendor independent features from extended flags

      // Check for 3DNow! support (vendor independent)

      mov     ecx, CPUID_EXT_3DNOW          // bit 31 indicates 3DNow! support
      and     ecx, edx                      // supp 3DNow! ?CPUID_EXT_3DNOW:0
      neg     ecx                           // supports 3DNow! ? CY : NC
      sbb     ecx, ecx                      // supports 3DNow! ? 0xffffffff:0
      and     ecx, FEATURE_3DNOW            // support 3DNow!?FEATURE_3DNOW:0
      or      [result], ecx                 // merge into feature flags

      // Step 8: Determine CPU vendor

      lea     esi, vendorAMD                // AMD's vendor string
      lea     edi, vendor                   // this CPU's vendor string
      mov     ecx, 12                       // strings are 12 characters
      cld                                   // compare lowest to highest
      repe    cmpsb                         // current vendor str == AMD's ?
      jnz     @not_AMD                      // no, CPU vendor is not AMD

      // Step 9: Check AMD specific extended features

      mov     ecx, CPUID_EXT_AMD_3DNOWEXT   // bit 30 indicates 3DNow! ext.
      and     ecx, edx                      // 3DNow! ext?
      neg     ecx                           // 3DNow! ext ? CY : NC
      sbb     ecx, ecx                      // 3DNow! ext ? 0xffffffff : 0
      and     ecx, FEATURE_3DNOWEXT         // 3DNow! ext?FEATURE_3DNOWEXT:0
      or      [result], ecx                 // merge into feature flags

      test    [result], FEATURE_MMXEXT      // determined SSE MMX support?
      jnz     @has_mmxext                   // yes, don't need to check again

      // Check support for AMD's multimedia instruction set additions

      mov     ecx, CPUID_EXT_AMD_MMXEXT     // bit 22 indicates MMX extension
      and     ecx, edx                      // MMX ext?CPUID_EXT_AMD_MMXEXT:0
      neg     ecx                           // MMX ext? CY : NC
      sbb     ecx, ecx                      // MMX ext? 0xffffffff : 0
      and     ecx, FEATURE_MMXEXT           // MMX ext ? FEATURE_MMXEXT:0
      or      [result], ecx                 // merge into feature flags

      @has_mmxext:

      // Step 10: Check AMD-specific features not reported by CPUID

      // Check support for AMD-K6 processor-style MTRRs

      mov     eax, [signature] 	   // get processor signature
      and     eax, $FFF 		       // extract family/model/stepping
      cmp     eax, $588 		       // CPU < AMD-K6-2/CXT ? CY : NC
      sbb     edx, edx 		         // CPU < AMD-K6-2/CXT ? 0xffffffff:0
      not     edx 			           // CPU < AMD-K6-2/CXT ? 0:0xffffffff
      cmp     eax, $600      		 // CPU < AMD Athlon ? CY : NC
      sbb     ecx, ecx 		         // CPU < AMD-K6 ? 0xffffffff:0
      and     ecx, edx 		         // (CPU>=AMD-K6-2/CXT)&&
                                   // (CPU<AMD Athlon) ? 0xffffffff:0
      and     ecx, FEATURE_K6_MTRR // (CPU>=AMD-K6-2/CXT)&&
                                   // (CPU<AMD Athlon) ? FEATURE_K6_MTRR:0
      or      [result], ecx 		   // merge into feature flags

      jmp     @all_done 		       // desired features determined

      @not_AMD:

      // Extract features specific to non AMD CPUs

      @no_extended_features:
      @no_standard_features:
      @all_done:
    end;
  end;

{$ifndef TrialLimited}
var
  Value: DWord;
{$endif TrialLimited}
begin
{$ifdef TrialLimited}
  TEProcessorInfo.MMX := False;
  TEProcessorInfo.SSE := False;
{$else}
  Value := GetFeatureFlags;
  TEProcessorInfo.MMX := (Value and FEATURE_MMX   ) <> 0;
  TEProcessorInfo.SSE := (Value and FEATURE_MMXEXT) <> 0;
{$endif TrialLimited}
end;

// Returns Windows version
function GetWinVersion: TTEWinVersion;
begin
  Result := teWinUnknown;
  case Win32Platform of
    VER_PLATFORM_WIN32s: Result := teWin32s;
    VER_PLATFORM_WIN32_WINDOWS: // Windows 9x/ME
    begin
      if(Win32MajorVersion = 4) and (Win32MinorVersion = 0)
      then Result := teWin95
      else
      begin
        if(Win32MajorVersion = 4) and (Win32MinorVersion = 10)
        then
        begin
          if Win32CSDVersion[1] = 'A'
          then Result := teWin98SE
          else Result := teWin98;
        end
        else
        begin
          if(Win32MajorVersion = 4) and (Win32MinorVersion = 90)
          then Result := teWinME
          else Result := teWinUnknown;
        end;
      end;
    end;
    VER_PLATFORM_WIN32_NT: // Windows NT/2000/XP/2003/Vista/7
    begin
      case Win32MajorVersion of
        4: Result := teWinNT;
        5:
        begin
          case Win32MinorVersion of
            0: Result := teWin2000;
            1: Result := teWinXP;
            2: Result := teWin2003;
          end;
        end;
        6:
        begin
          case Win32MinorVersion of
            0: Result := teWinVista;
            1: Result := teWin7;
          end;
        end;
        else Result := teWinFuture;
      end;
    end;
  end;
end;

{$ifndef NoVCL}

function IsWindowLayered(Window: HWND): Boolean;
const
  WS_EX_LAYERED = $00080000;
begin
  Result := (GetWindowLong(Window, GWL_EXSTYLE) and WS_EX_LAYERED) <> 0;
end;

procedure RegisterTEControl(const ControlClassName: String;
  NonClientRenderMode, ClientRenderMode: DWord;
  RefreshNonClient, RefreshClient: Boolean);
begin
  RegisterTEControlCallback(ControlClassName, NonClientRenderMode, ClientRenderMode,
    RefreshNonClient, RefreshClient, nil, nil);
end;

procedure RegisterTEControlCallback(const ControlClassName: String;
  NonClientRenderMode, ClientRenderMode: DWord;
  RefreshNonClient, RefreshClient: Boolean;
  NonClientCallback, ClientCallback: TTEPaintCallback);
var
  Flags: DWord;
  NonClientCallback2,
  ClientCallback2: TTEPaintCallback;
begin
  NonClientCallback2 := nil;
  ClientCallback2    := nil;

  Flags := $00000000;

  {$ifdef D7UP}
  if not ThemeServices.ThemesEnabled then
    NonClientRenderMode := NonClientRenderMode and not teThemed;
  {$endif D7UP}

  if NonClientRenderMode and teThemed <> 0 then
  begin
    Flags               := Flags or RCF_THEMEDNC;
    NonClientRenderMode := NonClientRenderMode and not teThemed;
  end;
  if NonClientRenderMode and teOwnCanvas <> 0 then
  begin
    Flags               := Flags or RCF_OWNCANVASNC;
    NonClientRenderMode := NonClientRenderMode and not teOwnCanvas;
  end;
  if ClientRenderMode and teOwnCanvas <> 0 then
  begin
    Flags            := Flags or RCF_OWNCANVAS;
    ClientRenderMode := ClientRenderMode and not teOwnCanvas;
  end;
  if NonClientRenderMode and teRefreshFocused <> 0 then
  begin
    Flags               := Flags or RCF_REFRESHFOCUSEDNC;
    NonClientRenderMode := NonClientRenderMode and not teRefreshFocused;
  end;
  if ClientRenderMode and teRefreshFocused <> 0 then
  begin
    Flags            := Flags or RCF_REFRESHFOCUSED;
    ClientRenderMode := ClientRenderMode and not teRefreshFocused;
  end;

  if NonClientRenderMode <> teNoRender then
  begin
    Flags := Flags or RCF_RENDERNC;
    case NonClientRenderMode of
      tePaint    : Flags := Flags or RCF_PAINTNC;
      tePrint    : Flags := Flags or RCF_PRINTNC;
      teEmulate  : Flags := Flags or RCF_EMULNC;
      tePaintCopy: Flags := Flags or RCF_PAINTCOPYNC or RCF_REFRESHFOCUSEDNC;
      teCallback :
                 begin
                   NonClientCallback2 := NonClientCallback;
                   Flags := Flags or RCF_CALLBACKNC;
                 end;
    end;
  end;
  if RefreshNonClient then
    Flags := Flags or RCF_REFRESHNC;

  if ClientRenderMode <> teNoRender then
  begin
    Flags := Flags or RCF_RENDER;
    case ClientRenderMode of
      tePaint    : Flags := Flags or RCF_PAINT;
      tePrint    : Flags := Flags or RCF_PRINT;
      teEmulate  : Flags := Flags or RCF_EMUL;
      tePaintCopy: Flags := Flags or RCF_PAINTCOPY or RCF_REFRESHFOCUSED;
      teCallback :
                 begin
                   ClientCallback2 := ClientCallback;
                   Flags := Flags or RCF_CALLBACK;
                 end;
    end;
  end;

  if RefreshClient then
    Flags := Flags or RCF_REFRESH;
  TERegControls.SaveRegControl(ControlClassName, Flags, NonClientCallback2,
    ClientCallback2);
end;

// Returns the offset of the client area within the control
function ControlClientOffset(Control: TControl): TPoint;
var
  R: TRect;
begin
  if Control is TWinControl
  then
  begin
    Result := ControlClientOrigin(Control);
    GetWindowRect(TWinControl(Control).Handle, R);
    Dec(Result.X, R.Left);
    Dec(Result.Y, R.Top);
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

// Returns the offset of the client area within the window
function WindowClientOffset(Window: HWND): TPoint;
var
  ScreenR: TRect;
begin
  GetWindowRect(Window, ScreenR);
  Result.X := 0;
  Result.Y := 0;
  ClientToScreen(Window, Result);
  Result.X := Result.X - ScreenR.Left;
  Result.Y := Result.Y - ScreenR.Top;
end;

// Returns the client area origin in screen coordinates
function ControlClientOrigin(Control: TControl): TPoint;
begin
  Result := Control.ClientOrigin;
end;

// Maps the given point in screen coordinates to Control coordinates
function ControlScreenToClient(Control: TControl; Point: TPoint): TPoint;
begin
  Result := Control.ScreenToClient(Point);
end;

// Maps the given point in Control coordinates to screen coordinates
function ControlClientToScreen(Control: TControl; Point: TPoint): TPoint;
begin
  Result := Control.ClientToScreen(Point);
end;

// Returns the control client area
function ControlClientRect(Control: TControl): TRect;
begin
  Result := Control.ClientRect;
end;

function ControlClientHeight(Control: TControl): Integer;
begin
  with ControlClientRect(Control) do
    Result := Bottom - Top;
end;

function ControlClientWidth(Control: TControl): Integer;
begin
  with ControlClientRect(Control) do
    Result := Right - Left;
end;

function GetMaximizedMDIChild(WinControl: TWinControl): Boolean;
var
  i: Integer;
begin
  Result := False;

  if(WinControl is TCustomForm) and
    (TTECustomForm(WinControl).FormStyle = fsMDIChild) and
    (Application.MainForm <> nil) and
    (TTECustomForm(Application.MainForm).FormStyle = fsMDIForm) then
  begin
    if TTECustomForm(WinControl).WindowState = wsMaximized
    then Result := True
    else
    begin
      for i := 0 to TTECustomForm(Application.MainForm).MDIChildCount - 1 do
        if TTECustomForm(Application.MainForm).MDIChildren[I].WindowState = wsMaximized then
        begin
          Result := True;
          Exit;
        end;
    end;
  end;
end;

function GetMaximizedMDIClient(ClassName: PChar): Boolean;
var
  i: Integer;
begin
  Result := False;
  if StrIComp(ClassName, 'MDICLIENT') = 0 then
  begin
    for i := 0 to TTECustomForm(Application.MainForm).MDIChildCount - 1 do
      if TTECustomForm(Application.MainForm).MDIChildren[I].WindowState = wsMaximized then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function GetMDIFormWithMaximizedMDIChild(WinControl: TWinControl): Boolean;
begin
  Result :=
    (WinControl is TCustomForm)                       and
    (TTECustomForm(WinControl).FormStyle = fsMDIForm) and
    GetMaximizedMDIClient('MDICLIENT');
end;

// Dwm API
function InitDwmApi: Boolean;
begin
  if(hDWMAPI = 0) and (TEWinVersion >= teWinVista) then
    hDWMAPI := LoadLibrary('DWMAPI.DLL');
  Result := hDWMAPI > 0;
end;

function IsCompositionEnabled: Boolean;
var
  pfEnabled: BOOL;
begin
  pfEnabled := False;
  if not Assigned(_DwmIsCompositionEnabled) then
  begin
    if InitDwmApi then
      _DwmIsCompositionEnabled := GetProcAddress(hDWMAPI, 'DwmIsCompositionEnabled');
  end;
  if Assigned(_DwmIsCompositionEnabled) then
    _DwmIsCompositionEnabled(pfEnabled);
  Result := pfEnabled;
end;

procedure DisableDwmTransitions(Window: HWND);
var
  TransitionsForceDisabledValue: BOOL;
begin
  if IsCompositionEnabled then
  begin
    // Disable Windows own transitions
    TransitionsForceDisabledValue := True;
    DwmSetWindowAttribute(Window, DWMWA_TRANSITIONS_FORCEDISABLED,
      @TransitionsForceDisabledValue, SizeOf(BOOL));
  end;
end;

function DwmSetWindowAttribute(Hwnd: HWND; dwAttribute: DWORD;
  pvAttribute: Pointer; cbAttribute: DWORD): HResult;
begin
  Result := E_NOTIMPL;
  if not Assigned(_DwmSetWindowAttribute) then
  begin
    if InitDwmApi then
      _DwmSetWindowAttribute := GetProcAddress(hDWMAPI, 'DwmSetWindowAttribute');
  end;
  if Assigned(_DwmSetWindowAttribute) then
    Result := _DwmSetWindowAttribute(Hwnd, dwAttribute, pvAttribute, cbAttribute);
end;

function IsScrollBarVisible(Control: TControl; Window: HWND;
  Kind: TScrollBarKind): Boolean;
var
  Style,
  MinPos,
  MaxPos,
  nBar: Longint;
  ControlScrollBar: TControlScrollBar;
begin
  ControlScrollBar := nil;
  if Kind = sbVertical
  then
  begin
    if(Control <> nil) and (Control is TScrollingWinControl) then
      ControlScrollBar := TScrollingWinControl(Control).VertScrollBar;
    Style := WS_VSCROLL;
    nBar  := SB_VERT;
  end
  else
  begin
    if(Control <> nil) and (Control is TScrollingWinControl) then
      ControlScrollBar := TScrollingWinControl(Control).HorzScrollBar;
    Style := WS_HSCROLL;
    nBar  := SB_HORZ;
  end;
  Result := ((Control = nil) or (ControlScrollBar = nil) or ControlScrollBar.Visible) and
            (GetWindowLong(Window, GWL_STYLE) and Style <> 0);
  if Result then
  begin
    GetScrollRange(Window, nBar, MinPos, MaxPos);
    Result := (MinPos <> 0) or (MaxPos <> 0);
  end;
end;

function RealizeControlPalette(Control: TControl;
  ForceBackground: Boolean): Boolean;
var
  i: integer;
  Palette,
  OldPalette: HPalette;
  WindowHandle: HWnd;
  DC: HDC;
begin
  Result := False;

  if(Control = nil) or (not PalettedDevice(False)) then Exit;

  Palette := TTEControl(Control).GetPalette;
  if Palette <> 0 then
  begin
    Result := True;
    if Control is TWinControl
    then WindowHandle := TWinControl(Control).Handle
    else WindowHandle := Control.Parent.Handle;
    DC := GetDC(WindowHandle);
    try
      OldPalette := SelectPalette(DC, Palette, ForceBackground);
      RealizePalette(DC);
      SelectPalette(DC, OldPalette, True);
      ForceBackground := True;
    finally
      ReleaseDC(WindowHandle, DC);
    end;
  end;

  if Control is TWinControl then
  begin
    with TWinControl(Control) do
    begin
      for i:=ControlCount-1 downto 0 do
        if Controls[i].Visible and RealizeControlPalette(Controls[i],
          ForceBackground) then
        begin
          ForceBackground := True;
          Result := True;
        end;
    end;
  end;
end;

// Finds the parent of input vmt instance that handles the message in BX
procedure GetDynaMethodX;
asm
//     -> EAX vmt of class
//     BX dynamic method index
//     <- EBX pointer to vmt of parent or self
//     ZF = 0 if found
//     trashes: EAX, ECX
        PUSH    EDI
        XCHG    EAX,EBX
        JMP     @@haveVMT
@@outerLoop:
        MOV     EBX,[EBX]
@@haveVMT:
        MOV     EDI,[EBX].vmtDynamicTable
        TEST    EDI,EDI
        JE      @@parent
        MOVZX   ECX,word ptr [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@found
        POP     ECX
@@parent:
        MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JNE     @@outerLoop
        JMP     @@exit
@@found:
        POP     EAX
        ADD     EAX,EAX
        SUB     EAX,ECX // this will always clear the Z-flag !
        //    ...return EBX as reference to class
@@exit:
        POP     EDI
end;

// returns the class pointer of self or ancestors that handles the Message
function DoesAncestorHandle(Instance : Pointer; var Message): TClass;
asm
        PUSH    EBX
        MOV     BX,[EDX] //Check if message valid
        OR      BX,BX
        JE      @@bypass
        CMP     BX,0C000H
        JAE     @@bypass
        PUSH    EAX //Prepare stack
        MOV     EAX,[EAX]
        CALL    GetDynaMethodX //try to obtain parents method
        POP     EAX
        JE      @@bypass //not found so return false
        MOV     EAX, EBX //found so return class
        JMP     @@exit
      @@bypass:
        POP    EBX
        MOV    EAX,0
        RET
      @@exit:
        POP  EBX
end;

function CompleteFlags(WinControl: TControl; Flags: DWord): DWord;
var
  Ms: TMessage;
  ClassNCPaint,
  ClassPrint: TClass;
begin
  if((Flags and RCF_PAINTCOPYNC) <> 0) or
    ((Flags and RCF_PAINTCOPY  ) <> 0) then
  begin
    if TWinControl(WinControl).Focused then
    begin
      if(Flags and RCF_PAINTCOPYNC) <> 0 then
        Flags := (Flags and not(RCF_PAINTCOPYNC)) or RCF_PRINT;
      if(Flags and RCF_PAINTCOPY  ) <> 0 then
        Flags := (Flags and not(RCF_PAINTCOPY  )) or RCF_PAINT;
    end;
  end;

  if(((Flags and RCF_RENDERNC) <> 0) and ((Flags and RCF_RENDERNCMASK) = RCF_RENDERNC)) or
    (((Flags and RCF_RENDER  ) <> 0) and ((Flags and RCF_RENDERMASK  ) = RCF_RENDER  )) then
  begin
    Ms.Msg := WM_PRINT;
    ClassPrint := DoesAncestorHandle(WinControl, Ms);

    if((Flags and RCF_RENDER) <> 0) and ((Flags and RCF_RENDERMASK) = RCF_RENDER) then
    begin
      if ClassPrint <> nil
      then Flags := Flags or RCF_PRINT
      else Flags := Flags or RCF_PAINT;
    end;

    if((Flags and RCF_RENDERNC) <> 0) and ((Flags and RCF_RENDERNCMASK) = RCF_RENDERNC) then
    begin
      Ms.Msg       := WM_NCPAINT;
      ClassNCPaint := DoesAncestorHandle(WinControl, Ms);

      if ClassNCPaint = nil
      then Flags := Flags or RCF_PRINTNC
      else
      begin
        if ClassNCPaint.ClassNameIs('TWinControl')
        then Flags := Flags or RCF_EMULNC
        else
        begin
          if(ClassPrint = nil) or not ClassPrint.InheritsFrom(ClassNCPaint)
          then Flags := Flags or RCF_PRINTNC or RCF_REFRESHNC
          else Flags := Flags or RCF_PRINTNC;
        end;
      end;
    end;
  end;
  Result := Flags;
end;

procedure GetTERegControl(const Window: HWND;
  const WinControl: TWinControl; var TERegControl: TTERegControl);

  function GetFlagsFromWindow(const Window: HWND): DWord;
  begin
    Result := 0;
    if SendMessage(Window, CM_BEFULLRENDER, 0, BE_ID) = BE_ID
    then Result := RCF_RENDER or RCF_RENDERNC or RCF_BEFULLRENDER
    else
    begin
      case SendMessage(Window, CM_BENCPAINT, 0, BE_ID) of
        BE_ID-1: Result := RCF_RENDERNC or RCF_BENCPREPAINT;
        BE_ID  : Result := RCF_RENDERNC or RCF_BENCPAINT;
        BE_ID+1: Result := RCF_RENDERNC or RCF_BENCPOSTPAINT;
      end;
      case SendMessage(Window, CM_BEPAINT, 0, BE_ID) of
        BE_ID-1: Result := Result or RCF_RENDER or RCF_BEPREPAINT;
        BE_ID  : Result := Result or RCF_RENDER or RCF_BEPAINT;
        BE_ID+1: Result := Result or RCF_RENDER or RCF_BEPOSTPAINT;
      end;
    end;
  end;

var
  Flags: DWord;
begin
  TERegControl.Clear;
  if WinControl = nil
  then
  begin
    TERegControl.Flags := GetFlagsFromWindow(Window);
    if(TERegControl.Flags and RCF_RENDERNCMASK) = 0 then
      TERegControl.Flags := RCF_RENDERNC or RCF_PRINTNC;
    if(TERegControl.Flags and RCF_RENDERMASK) = 0 then
      TERegControl.Flags := TERegControl.Flags or (RCF_RENDER or RCF_PAINT);
  end
  else
  begin
    {$ifndef NoVCL}
    TERegControls.FindRegControl(WinControl,
      TControlClass(WinControl.ClassType), TERegControl);
    {$endif NoVCL}
    Flags := GetFlagsFromWindow(Window);
    if(Flags and RCF_BEFULLRENDER) <> 0
    then TERegControl.Flags := Flags
    else
    begin
      if(Flags and RCF_RENDERNCMASK) <> 0 then
      begin
        if(Flags and (RCF_BENCPREPAINT or RCF_BENCPOSTPAINT)) <> 0
        then TERegControl.Flags := TERegControl.Flags or (Flags and RCF_RENDERNCMASK)
        else TERegControl.Flags :=
               (TERegControl.Flags and (not RCF_RENDERNCMASK)) or
               (Flags and RCF_RENDERNCMASK);
      end;
      if(Flags and RCF_RENDERMASK) <> 0 then
      begin
        if(Flags and (RCF_BEPREPAINT or RCF_BEPOSTPAINT)) <> 0
        then TERegControl.Flags := TERegControl.Flags or (Flags and RCF_RENDERMASK)
        else TERegControl.Flags :=
               (TERegControl.Flags and (not RCF_RENDERMASK)) or
               (Flags and RCF_RENDERMASK);
      end;
      TERegControl.Flags := CompleteFlags(WinControl, TERegControl.Flags);
    end;
  end;
end;

procedure InternalRefreshWindows(Window: HWND; TERegControl: TTERegControl);
var
  ChildWnd: HWND;
  Control: TWinControl;
  RefreshNonClient,
  RefreshClient: Boolean;
begin
  if not IsWindowVisible(Window)
    then Exit;

  Control := FindControl(Window);

  if Control <> nil then
  begin
    GetTERegControl(0, Control, TERegControl);

    RefreshNonClient := (TERegControl.Flags and RCF_REFRESHNC) <> 0;
    RefreshClient    := (TERegControl.Flags and RCF_REFRESH  ) <> 0;

    if Control.Focused then
    begin
      RefreshNonClient :=
        RefreshNonClient or
        ((TERegControl.Flags and RCF_REFRESHFOCUSEDNC) <> 0);
      RefreshClient    :=
        RefreshClient    or
        ((TERegControl.Flags and RCF_REFRESHFOCUSED  ) <> 0);
    end;

    if RefreshNonClient then
      SendMessage(Window, WM_NCPAINT, 0, 0);

    if RefreshClient then
      if(Control <> nil) and (Control.ControlCount > 0)
      then RedrawWindow(Window, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN)
      else RedrawWindow(Window, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;

  ChildWnd := GetWindow(Window, GW_CHILD);
  while ChildWnd <> 0 do
  begin
    InternalRefreshWindows(ChildWnd, TERegControl);
    ChildWnd := GetWindow(ChildWnd, GW_HWNDNEXT);
  end;
end;

procedure RefreshWindows(Window: HWND);
var
  TERegControl: TTERegControl;
begin
  TERegControl := TTERegControl.Create(0, nil, nil);
  try
    InternalRefreshWindows(Window, TERegControl);
  finally
    TERegControl.Free;
  end;
end;

procedure GetData(WinControl: TWinControl;
  Window: HWnd; var ClassType: TClass;
  var IsMDIClient, IsMaximizedMDIClient, IsMaximizedMDIChild, IsRenderWindow: Boolean);
var
  ClassName: array[0..63] of Char;
begin
  if WinControl <> nil
  then
  begin
    ClassType := WinControl.ClassType;
    StrPCopy(ClassName, WinControl.ClassName);

    if GetMDIFormWithMaximizedMDIChild(WinControl) then
    begin // Edge changing
      SetWindowLong(Application.MainForm.ClientHandle, GWL_EXSTYLE,
        GetWindowLong(Application.MainForm.ClientHandle,
          GWL_EXSTYLE) and not WS_EX_CLIENTEDGE);
      SetWindowPos(Application.MainForm.ClientHandle, 0, 0, 0, 0, 0,
        SWP_FRAMECHANGED or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE or
        SWP_NOZORDER);
    end;

    IsMaximizedMDIClient := False;
    IsMaximizedMDIChild  := GetMaximizedMDIChild(WinControl);
  end
  else
  begin
    GetClassName(Window, ClassName, Sizeof(ClassName));
    ClassType := GetClass(ClassName);

    IsMaximizedMDIClient := GetMaximizedMDIClient(ClassName);
    IsMaximizedMDIChild  := False;
  end;
  IsMDIClient    := StrIComp(ClassName, 'MDICLIENT') = 0;
  IsRenderWindow := StrIComp(ClassName, 'TTERenderWindow') = 0;
end;

procedure GetSize(Window: HWnd; IsMaximizedMDIChild: Boolean;
  var Width, Height: Integer);
var
  WndRect: TRect;
begin
  if IsMaximizedMDIChild
  then GetClientRect(GetParent(Window), WndRect)
  else GetWindowRect(Window, WndRect);
  Width  := WndRect.Right  - WndRect.Left;
  Height := WndRect.Bottom - WndRect.Top;
end;

procedure CheckClipRegion(Window: HWnd; DC: HDC;
  CheckRegion, IsMaximizedMDIChild: Boolean; Width, Height: Integer; R: TRect);
var
  WndRect: TRect;
  WndRgn,
  ClipRgn: HRGN;
  P: TPoint;
begin
  WndRect := Rect(0, 0, Width, Height);
  WndRgn  := CreateRectRgn(WndRect.Left, WndRect.Top, WndRect.Right,
    WndRect.Bottom);

  if CheckRegion and (not IsMaximizedMDIChild) then
    GetWindowRgn(Window, WndRgn);

  P := Point(0, 0);
  LPToDP(DC, P, 1);
  OffsetRgn(WndRgn, P.x, P.y);

  ClipRgn := CreateRectRgn(WndRect.Left, WndRect.Top, WndRect.Right,
    WndRect.Bottom);
  GetClipRgn(DC, ClipRgn);
  CombineRgn(ClipRgn, WndRgn, ClipRgn, RGN_AND);
  DeleteObject(WndRgn);
  SelectClipRgn(DC, ClipRgn);
  GetRgnBox(ClipRgn, R);
  DPToLP(DC, R, 2);
  DeleteObject(ClipRgn);
end;

procedure GetClientSize(WinControl: TWinControl; Window: HWnd;
  IsMaximizedMDIClient, IsMaximizedMDIChild: Boolean;
  var ClientWidth, ClientHeight: Integer; var ClientOrg: TPoint);
var
  WndRect,
  ClientRect: TRect;
  aux: TPoint;
begin
  if IsMaximizedMDIChild
  then
  begin
    GetClientRect(Window, ClientRect);
    ClientOrg :=
      Point(TTECustomForm(WinControl).BorderWidth,
            TTECustomForm(WinControl).BorderWidth);
  end
  else
  begin
    GetWindowRect(Window, WndRect);
    aux := Point(0, 0);
    ClientToScreen(Window, aux);
    ClientOrg.x := aux.x - WndRect.Left;
    ClientOrg.y := aux.y - WndRect.Top;
    ScreenToClient(Window, WndRect.TopLeft);
    ScreenToClient(Window, WndRect.BottomRight);
    GetClientRect(Window, ClientRect);
  end;
  ClientWidth  := ClientRect.Right  - ClientRect.Left;
  ClientHeight := ClientRect.Bottom - ClientRect.Top;
end;

function ClassInheritsFrom(const ClassType: TClass;
  const ClassName: String): Boolean;
var
  ParentClass: TClass;
begin
  Result := False;

  ParentClass := ClassType;
  while ParentClass <> TObject do
  begin
    if ParentClass.ClassNameIs(ClassName) then
    begin
      Result := True;
      break;
    end;
    ParentClass := ParentClass.ClassParent;
  end;
end;

procedure ToolWindowNCPaint(WinControl: TWinControl; DC: HDC);
type
  TEdgeStyle   = (esNone, esRaised, esLowered);
  TEdgeBorder  = (ebLeft, ebTop, ebRight, ebBottom);
  TEdgeBorders = set of TEdgeBorder;
const
  InnerStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  RC, RW: TRect;
  EdgeInner,
  EdgeOuter: TEdgeStyle;
  EdgeBorders: TEdgeBorders;
  PropInfo: PPropInfo;
  aux: Longint;
begin
  GetClientRect(WinControl.Handle, RC);
  GetWindowRect(WinControl.Handle, RW);
  MapWindowPoints(0, WinControl.Handle, RW, 2);
  OffsetRect(RC, -RW.Left, -RW.Top);
  ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
  // Draw borders in non-client area
  OffsetRect(RW, -RW.Left, -RW.Top);

  PropInfo    := GetPropInfo(WinControl.ClassInfo, 'EdgeInner');
  EdgeInner   := TEdgeStyle(GetOrdProp(WinControl, PropInfo));
  PropInfo    := GetPropInfo(WinControl.ClassInfo, 'EdgeOuter');
  EdgeOuter   := TEdgeStyle(GetOrdProp(WinControl, PropInfo));
  PropInfo    := GetPropInfo(WinControl.ClassInfo, 'EdgeBorders');
  aux         := GetOrdProp(WinControl, PropInfo);
  EdgeBorders := [];
  if(aux and $00000001) <> 0 then
    EdgeBorders := EdgeBorders + [ebLeft];
  if(aux and $00000002) <> 0 then
    EdgeBorders := EdgeBorders + [ebTop];
  if(aux and $00000004) <> 0 then
    EdgeBorders := EdgeBorders + [ebRight];
  if(aux and $00000008) <> 0 then
    EdgeBorders := EdgeBorders + [ebBottom];

  DrawEdge(DC, RW,
    InnerStyles[EdgeInner] or OuterStyles[EdgeOuter],
    Byte(EdgeBorders) or Ctl3DStyles[TTEWinControl(WinControl).Ctl3D] or BF_ADJUST);
  // Erase parts not drawn
  IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
  Windows.FillRect(DC, RW, WinControl.Brush.Handle);
end;

function ControlClientAreaHasRegion(Control: TWinControl): Boolean;
var
  ControlRgn,
  ClientRectRgn: HRgn;
  ControlR,
  R: TRect;
begin
  ControlRgn := CreateRectRgn(0, 0, 0, 0);
  try
    Result := GetWindowRgn(Control.Handle, ControlRgn) <> ERROR;
    if Result then
    begin
      GetRgnBox(ControlRgn, R);
      ControlR := Control.ClientRect;
      with ControlClientOffset(Control) do
        OffsetRect(ControlR, x, y);
      ClientRectRgn :=
        CreateRectRgn(ControlR.Left, ControlR.Top, ControlR.Right, ControlR.Bottom);
      try
        CombineRgn(ControlRgn, ControlRgn, ClientRectRgn, RGN_AND);
        Result := not EqualRgn(ControlRgn, ClientRectRgn);
      finally
        DeleteObject(ClientRectRgn);
      end;
    end;
  finally
    DeleteObject(ControlRgn);
  end;
end;

function WindowHasRegion(Window: HWnd): Boolean;
var
  Rgn: HRgn;
begin
  Rgn := CreateRectRgn(0, 0, 0, 0);
  try
    Result := GetWindowRgn(Window, Rgn) <> ERROR;
  finally
    DeleteObject(Rgn);
  end;
end;

procedure NCPrintControl(DC: HDC; WinControl: TWinControl; Window: HWnd);
var
  Bmp: TBitmap;
begin
  if(WinControl <> nil)                                and
    (WinControl is TCustomForm)                        and
    (TTECustomForm(WinControl).FormStyle = fsMDIChild) and
    (TEWinVersion >= teWinXP)                          and
    WindowHasRegion(Window)                            then
  begin // XP does something weird with the clipping region
    Bmp := TBitmap.Create;
    try
      Bmp.Canvas.Lock;
      try
        AdjustBmpForTransition(Bmp, 0, WinControl.Width, WinControl.Height,
          pfDevice);
        SendMessage(Window, WM_PRINT, Bmp.Canvas.Handle, PRF_NONCLIENT);
        BitBlt(DC, 0, 0, WinControl.Width, WinControl.Height,
          Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        Bmp.Canvas.Unlock;
      end;
    finally
      Bmp.Free;
    end;
  end
  else SendMessage(Window, WM_PRINT, DC, PRF_NONCLIENT);
end;

procedure WinControlNCPaint(WinControl: TWinControl; DC: HDC; Themed: Boolean);
  {$ifdef D7UP}
  procedure PaintThemeBorder(Control: TWinControl; DC: HDC; EraseLRCorner: Boolean);
  var
    EmptyRect,
    DrawRect: TRect;
    H, W: Integer;
    AStyle,
    ExStyle: Integer;
    Details: TThemedElementDetails;
  begin
    with Control do
    begin
      ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
      if (ExStyle and WS_EX_CLIENTEDGE) <> 0 then
      begin
        GetWindowRect(Handle, DrawRect);
        OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
          EmptyRect := DrawRect;
          if EraseLRCorner then
          begin
            AStyle := GetWindowLong(Handle, GWL_STYLE);
            if ((AStyle and WS_HSCROLL) <> 0) and ((AStyle and WS_VSCROLL) <> 0) then
            begin
              W := GetSystemMetrics(SM_CXVSCROLL);
              H := GetSystemMetrics(SM_CYHSCROLL);
              InflateRect(EmptyRect, -2, -2);
              with EmptyRect do
                EmptyRect := Rect(Right - W, Bottom - H, Right, Bottom);
              FillRect(DC, EmptyRect, GetSysColorBrush(COLOR_BTNFACE));
            end;
          end;
          with DrawRect do
            ExcludeClipRect(DC, Left + 2, Top + 2, Right - 2, Bottom - 2);
          Details := ThemeServices.GetElementDetails(teEditTextNormal);
          ThemeServices.DrawElement(DC, Details, DrawRect);
      end;
    end;
  end;
  {$endif D7UP}
const
  InnerStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENINNER, BDR_RAISEDINNER, 0);
  OuterStyles: array[TBevelCut] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDOUTER, 0);
  EdgeStyles: array[TBevelKind] of Integer = (0, 0, BF_SOFT, BF_FLAT);
  Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  RC, RW, SaveRW: TRect;
  EdgeSize: Integer;
  WinStyle: Longint;
  SaveIndex,
  SaveIndex2: Integer;
begin
  SaveIndex := SaveDC(DC);
  try
    with TTEWinControl(WinControl) do
    begin
      if (BevelKind <> bkNone) or (BorderWidth > 0) then
      begin
        Windows.GetClientRect(Handle, RC);
        GetWindowRect(Handle, RW);
        MapWindowPoints(0, Handle, RW, 2);
        OffsetRect(RC, -RW.Left, -RW.Top);
        ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
        { Draw borders in non-client area }
        SaveRW := RW;
        InflateRect(RC, BorderWidth, BorderWidth);
        RW := RC;
        {$ifdef D9UP}
        with RW do
        begin
          WinStyle := GetWindowLong(Handle, GWL_STYLE);
          if (WinStyle and WS_VSCROLL) <> 0 then Inc(Right, GetSystemMetrics(SM_CYVSCROLL));
          if (WinStyle and WS_HSCROLL) <> 0 then Inc(Bottom, GetSystemMetrics(SM_CXHSCROLL));
        end;
        if BevelKind <> bkNone then
        begin
          EdgeSize := 0;
          if BevelInner <> bvNone then Inc(EdgeSize, BevelWidth);
          if BevelOuter <> bvNone then Inc(EdgeSize, BevelWidth);
          with RW do
          begin
            if beLeft in BevelEdges then Dec(Left, EdgeSize);
            if beTop in BevelEdges then Dec(Top, EdgeSize);
            if beRight in BevelEdges then Inc(Right, EdgeSize);
            if beBottom in BevelEdges then Inc(Bottom, EdgeSize);
          end;
          DrawEdge(DC, RW, InnerStyles[BevelInner] or OuterStyles[BevelOuter],
            Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST);
        end;
        {$else}
        if BevelKind <> bkNone then
        begin
          EdgeSize := 0;
          if BevelInner <> bvNone then Inc(EdgeSize, BevelWidth);
          if BevelOuter <> bvNone then Inc(EdgeSize, BevelWidth);
          with RW do
          begin
            WinStyle := GetWindowLong(Handle, GWL_STYLE);
            if beLeft in BevelEdges then Dec(Left, EdgeSize);
            if beTop in BevelEdges then Dec(Top, EdgeSize);
            if beRight in BevelEdges then Inc(Right, EdgeSize);
            if (WinStyle and WS_VSCROLL) <> 0 then Inc(Right, GetSystemMetrics(SM_CYVSCROLL));
            if beBottom in BevelEdges then Inc(Bottom, EdgeSize);
            if (WinStyle and WS_HSCROLL) <> 0 then Inc(Bottom, GetSystemMetrics(SM_CXHSCROLL));
          end;
          DrawEdge(DC, RW, InnerStyles[BevelInner] or OuterStyles[BevelOuter],
            Byte(BevelEdges) or EdgeStyles[BevelKind] or Ctl3DStyles[Ctl3D] or BF_ADJUST);
        end;
        {$endif D9UP}
        IntersectClipRect(DC, RW.Left, RW.Top, RW.Right, RW.Bottom);
        RW := SaveRW;
        { Erase parts not drawn }
        OffsetRect(RW, -RW.Left, -RW.Top);
        Windows.FillRect(DC, RW, Brush.Handle);
      end;
    end;

    SaveIndex2 := SaveDC(DC);
    try
      NCPrintControl(DC, WinControl, WinControl.Handle);
    finally
      RestoreDC(DC, SaveIndex2);
    end;

    {$ifdef D7UP}
    if Themed or (csNeedsBorderPaint in WinControl.ControlStyle) then
      PaintThemeBorder(WinControl, DC, False);
    {$endif D7UP}
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure EraseAndPaintMessage(DC: HDC; WinControl: TWinControl; Window: HWND);
var
  SaveIndex: Integer;
  DoubleBuffered: Boolean;
begin
  DoubleBuffered := Assigned(WinControl) and (WinControl.DoubleBuffered);
  if DoubleBuffered then
    WinControl.DoubleBuffered := False;

  SaveIndex := SaveDC(DC);
  try
    SendMessage(Window, WM_ERASEBKGND, DC, 0);
  finally
    RestoreDC(DC, SaveIndex);
  end;
  SendMessage(Window, WM_PAINT, DC, BE_ID);

  if DoubleBuffered then
    WinControl.DoubleBuffered := True;
end;

// ****************************************************************
// Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// This code has been donated by Mathias Rauen and can only be used
// here. You are not allowed to modify or use it in your own code.

type
  TModule = record
    handle   : dword;
    fileName : string;
  end;
  TDAModule = array of TModule;

  // directory structure for imported APIs
  TImageImportDirectory = packed record
    HintNameArray         : dword;
    TimeDateStamp         : dword;
    ForwarderChain        : dword;
    Name_                 : dword;
    ThunkArray            : dword;
  end;

  TPPointer = ^pointer;
  TPWord    = ^word; TAWord = array [0..maxInt shr 1-1] of word;

function GetImageNtHeaders(module: dword) : PImageNtHeaders;
const
  // PE header constants
  CENEWHDR = $003C;  // offset of new EXE header
  CEMAGIC  = $5A4D;  // old EXE magic id:  'MZ'
  CPEMAGIC = $4550;  // NT portable executable
begin
  result := nil;
  try
    if (not IsBadReadPtr(pointer(module), 2)) and (word(pointer(module)^) = CEMAGIC) then begin
      result := pointer(module + dword(pointer(module + CENEWHDR)^));
      if result^.signature <> CPEMAGIC then
        result := nil;
    end;
  except result := nil end;
end;

// returns all modules of the current process
function GetModuleList : TDAModule;
var p1, p2 : pointer;
    mbi    : TMemoryBasicInformation;
    arrCh  : array [0..MAX_PATH] of char;
    i1     : integer;
begin
  SetLength(result, 10);
  i1 := 0;
  p1 := nil;
  p2 := nil;
  while VirtualQuery(p1, mbi, sizeOf(mbi)) = sizeOf(mbi) do begin
    if (mbi.State = MEM_COMMIT) and
       (mbi.AllocationBase <> p2) and (mbi.AllocationBase = mbi.BaseAddress) and
       (GetModuleFileName(dword(mbi.AllocationBase), arrCh, MAX_PATH) > 0)
    then begin
      if i1 = Length(result) then
        SetLength(result, i1 * 2);
      with result[i1] do begin
        handle   := dword(mbi.AllocationBase);
        fileName := ExtractFileName(arrCh);
      end;
      inc(i1);
    end;
    p2 := mbi.AllocationBase;
    dword(p1) := dword(p1) + mbi.RegionSize;
  end;
  SetLength(result, i1);
end;

procedure PatchImportTable(module: dword; old, new: pointer);

  procedure InvalidImportTable;
  begin
//    if (module = HInstance) and (DebugHook <> 0) then
//      MessageBox(0, 'The import table is invalid.' + #$D#$A + #$D#$A +
//                    'This way madExcept can''t install the thread hooks.',
//                 pchar(ExtractFileName(ModuleName(HInstance))), 0);
  end;

var pinh : PImageNtHeaders;
    pid  : ^TImageImportDirectory;
    p1   : TPPointer;
    c1   : dword;
    eis  : dword;  // end of import section
begin
  pinh := GetImageNtHeaders(module);
  if pinh <> nil then begin
    with pinh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT] do begin
      pid := pointer(module + VirtualAddress);
      eis := VirtualAddress + Size;
    end;
    if pid <> nil then
      while pid^.Name_ <> 0 do begin
        if pid^.ThunkArray > eis then begin
          InvalidImportTable;
          break;
        end;
        if pid^.ThunkArray <> 0 then begin
          p1 := pointer(module + pid^.ThunkArray);
          while p1^ <> nil do begin
            if (p1^ = old) and VirtualProtect(p1, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
              p1^ := new;
              VirtualProtect(p1, 4, c1, @c1);
            end;
            inc(p1);
          end;
        end;
        inc(pid);
      end;
  end;
end;

procedure PatchImportTables(const modules: TDAModule; old, new: pointer);
var i1 : integer;
begin
  for i1 := 0 to high(modules) do
    if(TEWinVersion >= teWinNT) or (modules[i1].handle < $80000000) then
      PatchImportTable(modules[i1].handle, old, new);
end;

// ****************************************************************
// general API hooking stuff

function PatchAPI(dll, api: string; org: pointer; var next: pointer;
callback: pointer) : boolean;
var p1 : pointer;
    c1 : dword;
begin
  if next = nil then begin
//    next := GetImageProcAddress(GetModuleHandle(PAnsiChar(dll)), api);
    next := GetProcAddress(GetModuleHandle(PChar(dll)), PChar(api));
    if (org <> nil) and (TPWord(org)^ = $25ff) then
         p1 := pointer(pointer(pointer(dword(org) + 2)^)^)
    else p1 := nil;
    // only the application module may hook all modules
    // dlls/packages may not, because they might come and go
    // however, the madExcept bpl may hook all modules, too
//    if (HInstance = GetModuleHandle(nil)) or AmMeBpl then
//         PatchImportTables(GetModuleList, next, callback)
//    else PatchImportTable (HInstance,     next, callback);
    PatchImportTables(GetModuleList, next, callback);

    result := (p1 <> nil) and (p1 = pointer(pointer(pointer(dword(org) + 2)^)^));
    if result then begin
      // the import table patching didn't change our own API import
      // this can happen with exe compressors/protectors
      // so we patch our own API import, additionally
      p1 := pointer(pointer(dword(org) + 2)^);
      if VirtualProtect(p1, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
        pointer(p1^) := callback;
        VirtualProtect(p1, 4, c1, @c1);
      end;
    end;
  end else
    result := false;
end;

procedure UnpatchAPI(org, next, callback: pointer);
var p1 : pointer;
    c1 : dword;
begin
  if next <> nil then begin
    if (org <> nil) and (TPWord(org)^ = $25ff) then
         p1 := pointer(pointer(pointer(dword(org) + 2)^)^)
    else p1 := nil;
    PatchImportTables(GetModuleList, callback, next);

    if (p1 <> nil) and (p1 = pointer(pointer(pointer(dword(org) + 2)^)^)) then begin
      p1 := pointer(pointer(dword(org) + 2)^);
      if VirtualProtect(p1, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
        pointer(p1^) := next;
        VirtualProtect(p1, 4, c1, @c1);
      end;
    end;
  end;
end;
// ***************************************************************
type
  TImportJumP  = packed record
    JMP: Word;
    Proc: ^Pointer;
  end;
  PImportJump = ^TImportJump;

procedure HookModuleAPICall(const DllName, Name: String;
  OrgAPICall, NewAPICall: Pointer; var SaveAPICall: Pointer);
var
  ImportJump: PImportJump;
  Oldprot: DWord;
begin
  if SaveAPICall = nil then
  begin
    ImportJump := OrgAPICall;
    SaveAPICall := GetProcAddress(GetModuleHandle(PChar(DllName)), PChar(Name));
    if VirtualProtect(ImportJump^.Proc, 4, PAGE_EXECUTE_READWRITE, Oldprot) then
    begin
      ImportJump.Proc^ := NewAPICall;
      VirtualProtect(ImportJump^.Proc, 4, Oldprot, Oldprot);
    end;
  end;
end;

procedure UnhookModuleAPICall(OrgAPICall: Pointer; var SaveAPICall: Pointer);
var
  ImportJump: PImportJump;
  Oldprot: DWord;
begin
  if Assigned(SaveAPICall) then
  begin
    ImportJump := OrgAPICall;
    VirtualProtect(ImportJump.Proc, 4, PAGE_EXECUTE_READWRITE, Oldprot);
    ImportJump.Proc^ := SaveAPICall;
    SaveAPICall := nil;
    VirtualProtect(ImportJump.Proc, 4, Oldprot, Oldprot);
  end;
end;

function IsRunTimePackage: Boolean;
var
  Buffer: array[0..261] of Char;
begin
  Windows.GetModuleFileName(HInstance, Buffer, SizeOf(Buffer));
  Result :=  ExtractFileExt(Buffer) = '.bpl';
end;

procedure HookAPICall(Dll, Name: string; OrgAPICall, NewAPICall: Pointer;
  var SaveAPICall: Pointer; IATPatching: Boolean);
begin
  if not TEAPIHooksDisabled then
  begin
    if IATPatching and TEIsRunTimePackage
    then PatchAPI(Dll, Name, OrgAPICall, SaveAPICall, NewAPICall)
    else HookModuleAPICall(Dll, Name, OrgAPICall, NewAPICall, SaveAPICall);
  end;
end;

procedure UnhookAPICall(OrgAPICall, NewAPICall: Pointer;
  var SaveAPICall: Pointer; IATPatching: Boolean);
begin
  if Assigned(SaveAPICall) then
  begin
    if IATPatching and TEIsRunTimePackage
    then UnpatchAPI(OrgAPICall, SaveAPICall, NewAPICall)
    else UnhookModuleAPICall(OrgAPICall, SaveAPICall);
    SaveAPICall := nil;
  end;
end;

function HookedGetDC(hWnd: HWND): hDC; stdcall;
begin
  if(hWnd <> HookWnd) or (HookDC = 0)
  then Result := SaveGetDC(hWnd)
  else Result := HookDC;
end;

function HookedGetDCEx(hWnd: HWND; hrgnClip: HRGN; flags: DWORD): HDC; stdcall;
begin
  if(hWnd <> HookWnd) or (HookDC = 0)
  then Result := SaveGetDCEx(hWnd, hrgnClip, flags)
  else Result := HookDC;
end;

function HookedGetWindowDC(hWnd: HWND): hDC; stdcall;
begin
  if(hWnd <> HookWnd) or (HookDC = 0)
  then Result := SaveGetWindowDC(hWnd)
  else Result := HookDC;
end;

function HookedReleaseDC(hWnd: HWND; DC: hDC): Integer; stdcall;
begin
  if(hWnd <> HookWnd) or (HookDC = 0)
  then Result := SaveReleaseDC(hWnd, DC)
  else Result := 1;
end;

function HookedBeginPaint(hWnd: HWND; var lpPaint: TPaintStruct): HDC; stdcall;
begin
  if(hWnd <> HookWnd) or (HookDC = 0)
  then Result := SaveBeginPaint(hWnd, lpPaint)
  else
  begin
    lpPaint.hdc := HookDC;
    Result      := HookDC;
  end;
end;

function HookedEndPaint(hWnd: HWND; const lpPaint: TPaintStruct): BOOL; stdcall;
begin
  if(hWnd <> HookWnd) or (HookDC = 0)
  then Result := SaveEndPaint(hWnd, lpPaint)
  else Result := True;
end;

procedure HookDCAPI(DC: HDC; Window: HWnd; IATPatching: Boolean);
begin
  if not TEAPIHooksDisabled then
  begin
    Inc(HookDCCount);
    HookDC  := DC;
    HookWnd := Window;

    if HookDCCount = 1 then
    begin
      HookAPICall('user32', 'GetDC'      , @Windows.GetDC      ,
        @HookedGetDC      , @SaveGetDC      , IATPatching);
      HookAPICall('user32', 'GetDCEx'    , @Windows.GetDCEx    ,
        @HookedGetDCEx    , @SaveGetDCEx    , IATPatching);
      HookAPICall('user32', 'GetWindowDC', @Windows.GetWindowDC,
        @HookedGetWindowDC, @SaveGetWindowDC, IATPatching);
      HookAPICall('user32', 'ReleaseDC'  , @Windows.ReleaseDC  ,
        @HookedReleaseDC  , @SaveReleaseDC  , IATPatching);
      HookAPICall('user32', 'BeginPaint' , @Windows.BeginPaint ,
        @HookedBeginPaint , @SaveBeginPaint , IATPatching);
      HookAPICall('user32', 'EndPaint'   , @Windows.EndPaint   ,
        @HookedEndPaint   , @SaveEndPaint   , IATPatching);
    end;
  end;
end;

procedure UnhookDCAPI(IATPatching: Boolean);
begin
  Dec(HookDCCount);
  if HookDCCount = 0 then
  begin
    HookDC  := 0;
    HookWnd := 0;

    UnhookAPICall(@Windows.GetDC      , @HookedGetDC      , @SaveGetDC      , IATPatching);
    UnhookAPICall(@Windows.GetDCEx    , @HookedGetDCEx    , @SaveGetDCEx    , IATPatching);
    UnhookAPICall(@Windows.GetWindowDC, @HookedGetWindowDC, @SaveGetWindowDC, IATPatching);
    UnhookAPICall(@Windows.ReleaseDC  , @HookedReleaseDC  , @SaveReleaseDC  , IATPatching);
    UnhookAPICall(@Windows.BeginPaint , @HookedBeginPaint , @SaveBeginPaint , IATPatching);
    UnhookAPICall(@Windows.EndPaint   , @HookedEndPaint   , @SaveEndPaint   , IATPatching);
  end;
end;

procedure PaintCopy(DC: HDC; WinControl: TWinControl);
begin
  if WinControl = nil then
    Exit;

  WinControl.ControlState := WinControl.ControlState + [csPaintCopy];
  try
    SendMessage(WinControl.Handle, WM_PAINT, DC, BE_ID);
  finally
    WinControl.ControlState := WinControl.ControlState - [csPaintCopy];
  end;
end;

procedure EmulatePaint(DC: HDC; WinControl: TWinControl);
begin
  if WinControl = nil then
    Exit;

  if WinControl is TOleControl
  then
  begin
    WinControl.HandleNeeded;
    OleDraw(IUnknown(TOleControl(WinControl).OleObject), DVASPECT_CONTENT,
      DC, ControlClientRect(WinControl));
  end;
end;

procedure EmulateNCPaint(DC: HDC; WinControl: TWinControl; Window: HWnd;
  Themed: Boolean);
begin
  if WinControl = nil then
    exit;

  {$ifdef D7UP}
  Themed := Themed and (TEWinVersion >= teWinXP);
  {$endif D7UP}

  if ClassInheritsFrom(WinControl.ClassType, 'TToolWindow')
  then ToolWindowNCPaint(WinControl, DC)
  else
  begin
    WinControlNCPaint(TWinControl(WinControl), DC, Themed);
  end;
end;

procedure PaintNonClient(DC: HDC; WinControl: TWinControl; Window: HWnd;
  TERegControl: TTERegControl);
var
  SaveIndex,
  SaveIndex2: Integer;
begin
  if Assigned(WinControl) and (WinControl is TScrollingWinControl) then
    with TScrollingWinControl(WinControl) do
    begin
      if((HorzScrollBar.Visible and (HorzScrollBar.Style = ssRegular)) and
         not(VertScrollBar.Visible and (VertScrollBar.Style <> ssRegular))) or
        ((VertScrollBar.Visible and (VertScrollBar.Style = ssRegular)) and
         not(HorzScrollBar.Visible and (HorzScrollBar.Style <> ssRegular))) then
        UninitializeFlatSB(Window);
    end;

  if(TERegControl.Flags and RCF_BENCPREPAINT) <> 0 then
  begin
    SaveIndex := SaveDC(DC);
    try
      SendMessage(Window, CM_BENCPAINT, DC, BE_ID)
    finally
      RestoreDC(DC, SaveIndex);
    end;
  end;

  SaveIndex := SaveDC(DC);
  try
    if Assigned(WinControl) and
      (
        (TTEWinControl(WinControl).BorderWidth > 0)
        {$ifdef D9UP}
        or
        (TTEWinControl(WinControl).BevelKind <> bkNone)
        {$endif D9UP}
      ) then
    begin
      SaveIndex2 := SaveDC(DC);
      try
        EmulateNCPaint(DC, WinControl, Window, (TERegControl.Flags and (RCF_THEMEDNC)) <> 0);
      finally
        RestoreDC(DC, SaveIndex2);
      end;
    end;

    if(TERegControl.Flags and RCF_PRINTNC) <> 0
    then NCPrintControl(DC, WinControl, Window)
    else if(TERegControl.Flags and RCF_PAINTNC) <> 0
    then
    begin
      NCPrintControl(DC, WinControl, Window);
      SendMessage(Window, WM_NCPAINT, 0, DC);
    end
    else if(TERegControl.Flags and RCF_CALLBACKNC) <> 0
    then
    begin
      if Assigned(WinControl) then
        if Assigned(TERegControl.NonClientCallback) then
          TERegControl.NonClientCallback(WinControl, DC);
    end
    else if(TERegControl.Flags and (RCF_PAINTCOPY or RCF_PAINTCOPYNC)) <> 0
    then PaintCopy(DC, WinControl)
    else if(TERegControl.Flags and RCF_BENCPAINT) <> 0
    then SendMessage(Window, CM_BENCPAINT, DC, BE_ID)
    else if(TERegControl.Flags and RCF_BEFULLRENDER) <> 0
    then SendMessage(Window, CM_BEFULLRENDER, DC, BE_ID)
    else EmulateNCPaint(DC, WinControl, Window,
          (TERegControl.Flags and (RCF_THEMEDNC)) <> 0);
  finally
    RestoreDC(DC, SaveIndex);
  end;

  if(TERegControl.Flags and RCF_BENCPOSTPAINT) <> 0 then
  begin
    SaveIndex := SaveDC(DC);
    try
      SendMessage(Window, CM_BENCPAINT, DC, BE_ID)
    finally
      RestoreDC(DC, SaveIndex);
    end;
  end;
end;

procedure PaintClient(DC: HDC; WinControl: TWinControl; Window: HWnd;
  TERegControl: TTERegControl);
var
  SaveIndex: Integer;
begin
  if(TERegControl.Flags and RCF_BEPREPAINT) <> 0 then
  begin
    SaveIndex := SaveDC(DC);
    try
      SendMessage(Window, CM_BEPAINT, DC, BE_ID)
    finally
      RestoreDC(DC, SaveIndex);
    end;
  end;

  SaveIndex := SaveDC(DC);
  try
    if(TERegControl.Flags and RCF_PRINT) <> 0
    then SendMessage(Window, WM_PRINT, DC, PRF_ERASEBKGND or PRF_CLIENT)
    else if(TERegControl.Flags and RCF_PAINT) <> 0
    then EraseAndPaintMessage(DC, WinControl, Window)
    else if(TERegControl.Flags and RCF_CALLBACK) <> 0
    then
    begin
      if Assigned(TERegControl.ClientCallback) then
        TERegControl.ClientCallback(WinControl, DC);
    end
    else if(TERegControl.Flags and RCF_BEPAINT) <> 0
    then SendMessage(Window, CM_BEPAINT, DC, BE_ID)
    else EmulatePaint(DC, WinControl);
  finally
    RestoreDC(DC, SaveIndex);
  end;

  if(TERegControl.Flags and RCF_BEPOSTPAINT) <> 0 then
  begin
    SaveIndex := SaveDC(DC);
    try
      SendMessage(Window, CM_BEPAINT, DC, BE_ID)
    finally
      RestoreDC(DC, SaveIndex);
    end;
  end;
end;

procedure RenderWindowToDCAux(Window, StopWnd, Parent: HWND;
  WinControl: TWinControl; DC: HDC; R: TRect;
  CheckVisibility, CheckRegion, Fast: Boolean;
  TERegControl: TTERegControl); forward;

procedure RenderChildWindows(DC: HDC; Window, StopWnd: HWND;
  IsMDIClient, IsMaximizedMDIClient, Fast: Boolean; ClientOrg: TPoint; R: TRect;
  TERegControl: TTERegControl);
var
  ChildWnd: HWND;
  ChildRect: TRect;
  P,
  ChildOrg: TPoint;
begin
  ChildWnd := GetWindow(Window, GW_CHILD);
  if ChildWnd <> 0 then
  begin
    if not IsMaximizedMDIClient
    then ChildWnd := GetWindow(ChildWnd, GW_HWNDLAST)
    else ChildWnd := GetWindow(ChildWnd, GW_HWNDFIRST);
  end;

  while(ChildWnd <> 0) and
       (
        (ChildWnd <> StopWnd) or
        (IsMDIClient and (not IsMaximizedMDIClient))
       ) do
  begin
    if IsWindowVisible(ChildWnd) then
    begin
      if not IsMaximizedMDIClient
      then
      begin
        GetWindowRect(ChildWnd, ChildRect);
        ScreenToClient(Window, ChildRect.TopLeft);
        ScreenToClient(Window, ChildRect.BottomRight);
        OffsetRect(ChildRect, ClientOrg.x, ClientOrg.y);
        ChildOrg := ChildRect.TopLeft;
        IntersectRect(ChildRect, ChildRect, R);
        OffsetRect(ChildRect, -ChildOrg.x, -ChildOrg.y);
      end
      else
      begin
        ChildRect := R;
        ChildOrg  := Point(0, 0);
      end;

      if not IsRectEmpty(ChildRect) then
      begin
        OffsetWindowOrgEx(DC, -ChildOrg.x, -ChildOrg.y, P);
        try
          RenderWindowToDCAux(ChildWnd, StopWnd, Window, nil, DC, ChildRect,
            True, True, Fast, TERegControl);
        finally
          SetWindowOrgEx(DC, P.x, P.y, nil);
        end;
      end;
    end;

    if IsMaximizedMDIClient
    then ChildWnd := 0
    else ChildWnd := GetWindow(ChildWnd, GW_HWNDPREV);
  end;
end;

{$ifdef XP_RENDER}
procedure DoRenderXP(DC: HDC; Window: HWnd);
type
  TPrintWindow = function(hwnd: HWND; hdcBlt: HDC; nFlags: DWORD): BOOL; stdcall;
var
  PrintWindow: TPrintWindow;
  hUser32: THandle;
begin
  PrintWindow := nil;
  hUser32     := LoadLibrary('user32.dll');
  try
    if hUser32 <> 0 then
      @PrintWindow := GetProcAddress(hUser32, 'PrintWindow');
    if @PrintWindow = nil then
      exit;

    PrintWindow(Window, DC, 0);
  finally
    if hUser32 <> 0 then
      FreeLibrary(hUser32);
  end;
end;
{$endif XP_RENDER}

{$ifdef AERO_RENDER}
procedure DoRenderAero(DC: HDC; R: TRect; Window: HWnd);
var
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  PaintBuffer := BeginBufferedPaint(DC, R, BPBF_COMPOSITED, nil, MemDC);
  try
    SendMessage(Window, WM_PRINT, MemDC, PRF_NONCLIENT or PRF_ERASEBKGND or PRF_CLIENT);
  finally
    EndBufferedPaint(PaintBuffer, True);
  end;
end;
{$endif AERO_RENDER}

procedure DoRender(DC: HDC; WinControl: TWinControl; Window, StopWnd: HWnd;
  IsMDIClient, IsMaximizedMDIClient, IsMaximizedMDIChild, Fast: Boolean;
  Width, Height: Integer; R: TRect; TERegControl: TTERegControl;
  ClassType: TClass);
var
  AbsPos,
  BOrg,
  P: TPoint;
  WndDC,
  SaveHookDC: HDC;
  HasNonClientArea,
  RenderClient,
  RenderNonClient,
  CommonPainting: Boolean;
  ClientOrg: TPoint;
  ClientWidth,
  ClientHeight: Integer;
  SaveRgn: HRgn;
  Bmp: TBitmap;
begin
  HookDCAPI(DC, Window, not Fast);
  try
    RenderClient    := (TERegControl.Flags and RCF_RENDER  ) <> 0;
    RenderNonClient := (TERegControl.Flags and RCF_RENDERNC) <> 0;
    CommonPainting  :=
      RenderClient    and
      RenderNonClient and
      (
        (
          Assigned(TERegControl.ClientCallback   ) and
          Assigned(TERegControl.NonClientCallback) and
          (@TERegControl.ClientCallback = @TERegControl.NonClientCallback)
        ) or
        (
          ((TERegControl.Flags and RCF_PAINTCOPY  ) <> 0) or
          ((TERegControl.Flags and RCF_PAINTCOPYNC) <> 0)
        ) or
        ((TERegControl.Flags and RCF_BEFULLRENDER) <> 0)
      );

    if CommonPainting
    then
    begin
      if IsMaximizedMDIChild then
        OffsetWindowOrgEx(DC, -WinControl.Left, -WinControl.Top, P);
      try
        if(TERegControl.Flags and (RCF_OWNCANVASNC or RCF_OWNCANVAS)) = 0
        then
        begin
          // Adjust the brush origin
          AbsPos.x := 0;
          AbsPos.y := 0;
          LPtoDP(DC, AbsPos, 1);
          SaveHookDC := HookDC;
          HookDC := 0;
          WndDC  := GetDC(Window);
          try
            GetBrushOrgEx(WndDC, BOrg);
            SetBrushOrgEx(DC, BOrg.X + AbsPos.x, BOrg.Y + AbsPos.y, nil);
          finally
            ReleaseDC(Window, WndDC);
          end;
          HookDC := SaveHookDC;
          try
            PaintNonClient(DC, WinControl, Window, TERegControl);
          finally
            SetBrushOrgEx(DC, BOrg.X, BOrg.Y, nil);
          end;
        end
        else
        begin
          Bmp := TBitmap.Create;
          try
            Bmp.Canvas.Lock;
            try
              AdjustBmpForTransition(Bmp, 0, Width, Height, pfDevice);
              PaintNonClient(Bmp.Canvas.Handle, WinControl, Window, TERegControl);
              BitBlt(DC, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
            finally
              Bmp.Canvas.Unlock;
            end;
          finally
            Bmp.Free;
          end;
        end;
      finally
        if IsMaximizedMDIChild then
          SetWindowOrgEx(DC, P.x, P.y, nil);
      end;
    end
    else
    begin
      GetClientSize(WinControl, Window, IsMaximizedMDIClient,
        IsMaximizedMDIChild, ClientWidth, ClientHeight, ClientOrg);

      if RenderClient then 
      begin
        // Remember current clipping region
        SaveRgn := CreateRectRgn(0,0,0,0);
        GetClipRgn(DC, SaveRgn);
        try
          OffsetWindowOrgEx(DC, -ClientOrg.x, -ClientOrg.y, P);
          try
            IntersectClipRect(DC, 0, 0, ClientWidth, ClientHeight);

            if(TERegControl.Flags and RCF_OWNCANVAS) = 0
            then
            begin
              // Adjust the brush origin
              AbsPos.x := 0;
              AbsPos.y := 0;
              LPtoDP(DC, AbsPos, 1);
              SaveHookDC := HookDC;
              HookDC := 0;
              WndDC  := GetDC(Window);
              try
                GetBrushOrgEx(WndDC, BOrg);
                SetBrushOrgEx(DC, BOrg.X + AbsPos.x, BOrg.Y + AbsPos.y, nil);
              finally
                ReleaseDC(Window, WndDC);
              end;
              HookDC := SaveHookDC;

              try
                PaintClient(DC, WinControl, Window, TERegControl);
              finally
                SetBrushOrgEx(DC, BOrg.X, BOrg.Y, nil);
              end;
            end
            else
            begin
              Bmp := TBitmap.Create;
              try
                Bmp.Canvas.Lock;
                try
                  AdjustBmpForTransition(Bmp, 0, ClientWidth, ClientHeight, pfDevice);
                  PaintClient(Bmp.Canvas.Handle, WinControl, Window, TERegControl);
                  BitBlt(DC, 0, 0, ClientWidth, ClientHeight, Bmp.Canvas.Handle, 0, 0,
                    SRCCOPY);
                finally
                  Bmp.Canvas.Unlock;
                end;
              finally
                Bmp.Free;
              end;
            end;
          finally
            SetWindowOrgEx(DC, P.x, P.y, nil);
          end;
        finally
          SelectClipRgn(DC, SaveRgn);
          DeleteObject(SaveRgn);
        end;
      end;

      HasNonClientArea := ((ClientWidth <> Width) or (ClientHeight <> Height));

      if HasNonClientArea and RenderNonClient then
      begin
        // Remember current clipping region
        SaveRgn := CreateRectRgn(0,0,0,0);
        GetClipRgn(DC, SaveRgn);
        try
          ExcludeClipRect(DC, ClientOrg.x, ClientOrg.y,
            ClientOrg.x + ClientWidth, ClientOrg.y + ClientHeight);

          if IsMaximizedMDIChild then
            OffsetWindowOrgEx(DC, -WinControl.Left, -WinControl.Top, P);
          try
            if(TERegControl.Flags and RCF_OWNCANVASNC) = 0
            then
            begin
              // Adjust the brush origin
              AbsPos.x := 0;
              AbsPos.y := 0;
              LPtoDP(DC, AbsPos, 1);
              SaveHookDC := HookDC;
              HookDC := 0;
              WndDC  := GetDC(Window);
              try
                GetBrushOrgEx(WndDC, BOrg);
                SetBrushOrgEx(DC, BOrg.X + AbsPos.x, BOrg.Y + AbsPos.y, nil);
              finally
                ReleaseDC(Window, WndDC);
              end;
              HookDC := SaveHookDC;
              try
                PaintNonClient(DC, WinControl, Window, TERegControl);
              finally
                SetBrushOrgEx(DC, BOrg.X, BOrg.Y, nil);
              end;
            end
            else
            begin
              Bmp := TBitmap.Create;
              try
                Bmp.Canvas.Lock;
                try
                  AdjustBmpForTransition(Bmp, 0, Width, Height, pfDevice);
                  PaintNonClient(Bmp.Canvas.Handle, WinControl, Window, TERegControl);
                  BitBlt(DC, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
                finally
                  Bmp.Canvas.Unlock;
                end;
              finally
                Bmp.Free;
              end;
            end;
          finally
            if IsMaximizedMDIChild then
              SetWindowOrgEx(DC, P.x, P.y, nil);
          end;
        finally
          SelectClipRgn(DC, SaveRgn);
          DeleteObject(SaveRgn);
        end;
      end;
    end;

    if(WinControl = nil) or (not(WinControl is TOleControl)) then
    begin
      GetClientSize(WinControl, Window, IsMaximizedMDIClient,
        IsMaximizedMDIChild, ClientWidth, ClientHeight, ClientOrg);
      SaveRgn := CreateRectRgn(0,0,0,0);
      GetClipRgn(DC, SaveRgn);
      try
        IntersectClipRect(DC, ClientOrg.x, ClientOrg.y,
          ClientOrg.x + ClientWidth, ClientOrg.y + ClientHeight);
        RenderChildWindows(DC, Window, StopWnd, IsMDIClient,
          IsMaximizedMDIClient, Fast, ClientOrg, R, TERegControl);
      finally
        SelectClipRgn(DC, SaveRgn);
        DeleteObject(SaveRgn);
      end;
    end;
  finally
    UnhookDCAPI(not Fast);
  end;
end;

procedure RenderWindowToDCAux(Window, StopWnd, Parent: HWND;
  WinControl: TWinControl; DC: HDC; R: TRect;
  CheckVisibility, CheckRegion, Fast: Boolean; TERegControl: TTERegControl);

var
  SaveRgn: HRgn;
  ClassType: TClass;
  IsMDIClient,
  IsMaximizedMDIClient,
  IsMaximizedMDIChild,
  IsRenderWindow,
  AlreadyRendered: Boolean;
  Width,
  Height: Integer;
begin
  if WinControl = nil then
    WinControl := FindControl(Window);

  GetData(WinControl, Window, ClassType, IsMDIClient, IsMaximizedMDIClient,
    IsMaximizedMDIChild, IsRenderWindow);

  if IsRenderWindow then
    Exit;

  if((not CheckVisibility) or
    IsWindowVisible(Window)) then
  begin
    // Remember current clipping region
    SaveRgn := CreateRectRgn(0,0,0,0);
    GetClipRgn(DC, SaveRgn);
    try
      GetSize(Window, IsMaximizedMDIChild, Width, Height);
      CheckClipRegion(Window, DC, CheckRegion, IsMaximizedMDIChild, Width,
        Height, R);
      if not IsRectEmpty(R) then
      begin
        AlreadyRendered := False;

        {$ifdef XP_RENDER}
        if(not AlreadyRendered)     and
          (not TEXPRenderDisabled)  and
          (TEWinVersion >= teWinXP) and
          (StopWnd = 0)             and
          IsWindowVisible(Window)   then
        begin
          DoRenderXP(DC, Window);
          AlreadyRendered := True;
        end;
        {$endif XP_RENDER}
        {$ifdef AERO_RENDER}
        if(not AlreadyRendered)   and
          (StopWnd = 0)           and
          (GetParent(Window) = 0) and
          IsCompositionEnabled    and
          IsWindowVisible(Window) then
        begin
          DoRenderAero(DC, R, Window);
          AlreadyRendered := True;
        end;
        {$endif AERO_RENDER}
        if not AlreadyRendered then
        begin
          GetTERegControl(Window, WinControl, TERegControl);
          DoRender(DC, WinControl, Window, StopWnd, IsMDIClient,
            IsMaximizedMDIClient, IsMaximizedMDIChild, Fast, Width, Height, R,
            TERegControl, ClassType);
        end;
      end;
    finally
      SelectClipRgn(DC, SaveRgn);
      DeleteObject(SaveRgn);
    end;
  end;
end;

procedure RenderWindowToDC(Window, StopWnd: HWND; WinControl: TWinControl;
  DC: HDC; R: TRect;
  ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean);
var
  ClientOffset: TPoint;
  Parent: HWND;
  ParentClassName: array[0..63] of Char;
  SaveR: TRect;
  P: TPoint;
  TERegControl: TTERegControl;
begin
  if(Window = StopWnd) or
    (CheckVisibility and (not IsWindowVisible(Window))) then
    Exit;

  if not IsChild(Window, StopWnd) then
    StopWnd := 0;

  SaveR := R;
  if ClientCoordinates and (not GetMaximizedMDIChild(WinControl)) then
  begin
    ClientOffset := WindowClientOffset(Window);
    OffsetRect(R, ClientOffset.X, ClientOffset.Y);
    OffsetWindowOrgEx(DC, ClientOffset.x, ClientOffset.y, P);
  end;
  try
    Parent := GetParent(Window);
    if Parent <> 0 then
    begin
      GetClassName(Parent, ParentClassName, Sizeof(ParentClassName));
      if StrIComp(ParentClassName, 'TApplication') = 0 then
        Parent := 0;
    end;

    TERegControl := TTERegControl.Create(0, nil, nil);
    try
      RenderWindowToDCAux(Window, StopWnd, Parent, WinControl, DC, R, False,
        CheckRegion, Fast, TERegControl);
    finally
      TERegControl.Free;
    end;
  finally
    if not EqualRect(R, SaveR) then
    begin
      R := SaveR;
      SetWindowOrgEx(DC, P.x, P.y, nil);
    end;
  end;
end;

{$ifdef Trial}
{$include trial\taux3.inc}
{$endif Trial}

function RenderWindowToBmp(Bmp: TBitmap; Window, StopWnd: HWND;
  WinControl: TWinControl;
  R: TRect; ClientCoordinates, CheckVisibility, CheckRegion, Fast: Boolean;
  PixelFormat: TPixelFormat): TBitmap;
var
//  CurBmpBak: TBitmap;
  DC: HDC;
  ClipRgn: HRGN;
  SaveWndOrg: TPoint;
  {$ifdef D11UP}
  PaintBuffer: HPAINTBUFFER;
  RAux: TRect;
  {$endif D11UP}
begin
  if Bmp = nil
  then
  begin
    Result := TBitmap.Create;
    Result.Canvas.Lock;
    AdjustBmpForTransition(Result, 0, R.Right - R.Left, R.Bottom - R.Top,
      PixelFormat);
  end
  else
  begin
    Result := Bmp;
    Result.Canvas.Lock;
  end;

//  try
//    CurBmpBak := TECurBmp;
//    TECurBmp  := Result;
    try
      try
        {$ifdef D11UP}
        if TEWinVersion >= teWinVista
        then
        begin
          RAux := Rect(0, 0, Result.Width, Result.Height);
          PaintBuffer := BeginBufferedPaint(Result.Canvas.Handle,
            RAux, BPBF_COMPOSITED, nil, DC);
        end
        else PaintBuffer := 0;
        if PaintBuffer = 0 then
          DC := Result.Canvas.Handle;
        try
        {$else}
        DC := Result.Canvas.Handle;
        {$endif D11UP}
          if Bmp = nil
          then
          begin
            SetWindowOrgEx(DC, R.Left, R.Top, @SaveWndOrg);
            ClipRgn := CreateRectRgn(0, 0, R.Right-R.Left, R.Bottom-R.Top);
          end
          else
          begin
            OffsetWindowOrgEx(DC, -R.Left, -R.Top, SaveWndOrg);
            ClipRgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
          end;
          SelectClipRgn(DC, ClipRgn);
          DeleteObject(ClipRgn);
          try
            RenderWindowToDC(Window, StopWnd, WinControl, DC, R,
              ClientCoordinates, CheckVisibility, CheckRegion, Fast);
          finally
            SelectClipRgn(DC, 0);
            SetWindowOrgEx(DC, SaveWndOrg.x, SaveWndOrg.y, nil);
          end;
        {$ifdef D11UP}
        finally
          if PaintBuffer <> 0 then
            EndBufferedPaint(PaintBuffer, True);
        end;
        {$endif D11UP}
      finally
        Result.Canvas.Unlock;
      end;
//    finally
//      TECurBmp := CurBmpBak;
//    end;
  except
    if Bmp = nil then
      Result.Free;
    raise;
  end;
end;

function RenderControl(Control: TControl; StopWnd: HWnd; R: TRect;
  ClientCoordinates, CheckRegion, Fast: Boolean;
  PixelFormat: TPixelFormat): TBitmap;
begin
  if Control is TWinControl
  then
  begin
    Result := RenderWindowToBmp(nil,
      TWinControl(Control).Handle, StopWnd, TWinControl(Control), R,
      ClientCoordinates, True, CheckRegion, Fast, PixelFormat);
  end
  else Result := nil;
end;

{$ifdef Trial}
{$include trial\taux4.inc}
{$endif Trial}

// Old RichEdit
procedure PaintRichEdit1(Control: TWinControl; DC: HDC);
begin
  SendMessage(Control.Handle, WM_PAINT, 0, 0);
  SendMessage(Control.Handle, WM_PRINT, DC, PRF_ERASEBKGND or PRF_CLIENT);
end;

// RichEdit Version 5
function Twips(Pixels, PixelsPerInch: Integer): Integer;
begin
  Result := (Pixels * 1440) div PixelsPerInch;
end;

procedure PaintRichEditV5(Control: TWinControl; DC: HDC; EraseBkgrnd: Boolean);
var
  PixelsPerInchX: Integer;
  PixelsPerInchY: Integer;
  Range: TFormatRange;
  P,
  aux: TPoint;
//  ClipBox,
  EditRect: TRect;
  Params: TCreateParams;
  SaveIndex: Integer;
begin
  if EraseBkgrnd then
  begin
    SaveIndex := SaveDC(DC);
    try
      SendMessage(Control.Handle, WM_ERASEBKGND, DC, 0);
    finally
      RestoreDC(DC, SaveIndex);
    end;
  end;

  PixelsPerInchX := GetDeviceCaps(DC, LOGPIXELSX);
  PixelsPerInchY := GetDeviceCaps(DC, LOGPIXELSY);

  FillChar(Range, SizeOf(TFormatRange), 0);
  with Range do
  begin
    hdc        := DC;
    hdcTarget  := DC;
    chrg.cpMax := -1;
    P          := Point(1, 1);

    chrg.cpMin := SendMessage(Control.Handle, EM_CHARFROMPOS, 0, Longint(@P));
    SendMessage(Control.Handle, EM_POSFROMCHAR, Longint(@P), chrg.cpMin);

    SendMessage(Control.Handle, EM_GETRECT, 0, Integer(@EditRect));

    TTEWinControl(Control).CreateParams(Params);
    rcPage := Rect(Twips(EditRect.Left   , PixelsPerInchX), Twips(EditRect.Top   , PixelsPerInchY),
                   Twips(EditRect.Right-1, PixelsPerInchX), Twips(EditRect.Bottom, PixelsPerInchY));
    ExcludeClipRect(DC, Control.ClientWidth-1, 0, Control.ClientWidth,
      Control.ClientHeight);
    if(Params.Style and (WS_HSCROLL or ES_AUTOHSCROLL) <> 0) then
    begin
//      GetClipBox(DC, ClipBox);
//      ExcludeClipRect(DC, ClipBox.Right-1, 0, ClipBox.Right, ClipBox.Bottom);
      EditRect.Right := 9999999;
    end;

    rc := Rect(Twips(EditRect.Left , PixelsPerInchX), Twips(EditRect.Top         , PixelsPerInchY),
               Twips(EditRect.Right, PixelsPerInchX), Twips(EditRect.Bottom + 100, PixelsPerInchY));
  end;

  SendMessage(Control.Handle, EM_FORMATRANGE, 0, 0);
  OffsetViewportOrgEx(DC, 0{P.X-1}, P.Y-1, aux);
  try
    SendMessage(Control.Handle, EM_FORMATRANGE, 1, Longint(@Range));
  finally
    OffsetViewportOrgEx(DC, 0{-(P.X-1)}, -(P.Y-1), aux);
  end;
  SendMessage(Control.Handle, EM_FORMATRANGE, 0, 0);
end;

procedure PaintRichEdit2(Control: TWinControl; DC: HDC);
begin
  PaintRichEditV5(Control, DC, True);
end;

// Woll2Woll RichEdit
procedure PaintRichEdit3(Control: TWinControl; DC: HDC);
var
  DoPaintCopy: Boolean;
  SaveIndex,
  ClientWidth,
  ClientHeight: Integer;
  P,
  ClientOrg: TPoint;
  Info: PPropInfo;
  Prop: Longint;
  FrameEnabled,
  FrameTransparent: Boolean;
begin
  DoPaintCopy :=
     Assigned(Control)                                                  and
    (ClassInheritsFrom(Control.Parent.ClassType, 'TCustomGrid'       ) or
     ClassInheritsFrom(Control.Parent.ClassType, 'TwwRecordViewPanel')) and
    not Control.Focused;

  FrameEnabled     := False;
  FrameTransparent := False;
  Info := GetPropInfo(Control.ClassInfo, 'Frame');
  if Assigned(Info) and (Info^.PropType^^.Name = 'TwwEditFrame') then
  begin
    Prop := GetOrdProp(Control, Info);
    Info := GetPropInfo(TObject(Prop).ClassInfo, 'Enabled');
    if Info <> nil then
      FrameEnabled := GetOrdProp(TObject(Prop), Info) = 1;
    Info := GetPropInfo(TObject(Prop).ClassInfo, 'Transparent');
    if Info <> nil then
      FrameTransparent := GetOrdProp(TObject(Prop), Info) = 1;
    DoPaintCopy := FrameEnabled and FrameTransparent;
  end;

  SaveIndex := SaveDC(DC);
  try
    if DoPaintCopy
    then PaintCopy(DC, Control) // To support Infopower's transparency
    else NCPrintControl(DC, Control, Control.Handle);
  finally
    RestoreDC(DC, SaveIndex);
  end;

  GetClientSize(Control, Control.Handle, False, False, ClientWidth,
    ClientHeight, ClientOrg);
  OffsetWindowOrgEx(DC, -ClientOrg.x, -ClientOrg.y, P);
  SaveIndex := SaveDC(DC);
  try
    IntersectClipRect(DC, 0, 0, ClientWidth, ClientHeight);
    PaintRichEditV5(Control, DC, not (FrameEnabled and FrameTransparent));
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

{$ifndef NoVCL}
procedure RegisterRichEdit;

  function GetFileVersion(const Filename: String): Integer;
  var
    aux: DWord;
    aux2: Cardinal;
    Size: Integer;
    Data: Pointer;
    Info: PVSFixedFileInfo;
  begin
    Result := 0;
    Size := GetFileVersionInfoSize(PChar(Filename), aux);
    if Size > 0 then
    begin
      GetMem(Data, Size);
      try
        aux2 := 0;
        GetFileVersionInfo(PChar(Filename), aux2, Size, Data);
        VerQueryValue(Data, '\', Pointer(Info), aux);
        Result := HiWord(Info.dwFileVersionMS);
      finally
        FreeMem(Data, Size);
      end;
    end;
  end;

var
 NonClientRenderMode: DWord;
begin
  {$ifdef D7UP}
  if ThemeServices.ThemesEnabled
  then NonClientRenderMode := teEmulate or teThemed
  else NonClientRenderMode := tePrint;
  {$else}
  NonClientRenderMode := tePrint;
  {$endif D7UP}
  if GetFileVersion('RICHED32.DLL') < 5
  then
  begin
    RegisterTEControlCallback('TCustomRichEdit'  , NonClientRenderMode,
      teCallback, False, False, nil, PaintRichEdit1);
  end
  else
  begin
    RegisterTEControlCallback('TCustomRichEdit'  , NonClientRenderMode,
      teCallback, False, False, nil, PaintRichEdit2);
  end;
end;
{$endif NoVCL}

{ TTERegControl }

constructor TTERegControl.Create(FlagsValue: DWord;
      NonClientCallbackValue, ClientCallbackValue: TTEPaintCallback);
begin
  Flags             := FlagsValue;
  NonClientCallback := NonClientCallbackValue;
  ClientCallback    := ClientCallbackValue;
end;

procedure TTERegControl.Assign(Source: TTERegControl);
begin
  if Source is TTeRegControl
  then
  begin
    Flags             := Source.Flags;
    NonClientCallback := Source.NonClientCallback;
    ClientCallback    := Source.ClientCallback;
  end
  else inherited;
end;

procedure TTERegControl.Clear;
begin
  Flags             := 0;
  NonClientCallback := nil;
  ClientCallback    := nil;
end;

{ TTERegControls }
{$ifndef NoVCL}
constructor TTERegControls.Create;
begin
  Duplicates    := dupError;
  Sorted        := False;
  {$ifdef D6UP}
  CaseSensitive := False;
  {$endif D6UP}
end;

destructor TTERegControls.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Objects[i].Free;

  inherited;
end;

procedure TTERegControls.FindRegControl(Control: TWinControl;
  ControlClass: TControlClass; var Data: TTERegControl);
var
  i,
  h: Integer;
  ControlClassName: String;
begin
  if ControlClass = nil then
    exit;

  ControlClassName := ControlClass.ClassName;
  i := 0;
  h := Count - 1;
  while i <= h do
  begin
    if AnsiCompareText(Strings[i], ControlClassName) = 0
    then
    begin
      Data.Assign(TTERegControl(Objects[i]));
      h := i-1;
    end
    else
    begin
      if ClassInheritsFrom(ControlClass, Strings[i])
      then
      begin
        Data.Assign(TTERegControl(Objects[i]));
        h := i-1;
      end
      else Inc(i);
    end;
  end;
end;

procedure TTERegControls.SaveRegControl(ControlClassName: String;
  Flags: DWord; NonClientCallback, ClientCallback: TTEPaintCallback);
var
  i: Integer;
  RegControl: TTERegControl;
begin
  i := IndexOf(ControlClassName);
  if i <> -1
  then RegControl := TTERegControl(Objects[i])
  else
  begin
    RegControl := TTERegControl.Create(Flags, NonClientCallback, ClientCallback);
    InsertObject(0, ControlClassName, RegControl);
  end;

  RegControl.Flags             := Flags;
  RegControl.NonClientCallback := NonClientCallback;
  RegControl.ClientCallback    := ClientCallback;
end;
{$endif NoVCL}

procedure InitProcs;
const
  sUser32 = 'User32.dll';
var
  ModH: HMODULE;
begin
  ModH := GetModuleHandle(sUser32);
  if ModH <> 0 then
  begin
     @UpdateLayeredWindow        := GetProcAddress(ModH, 'UpdateLayeredWindow');
     @GetLayeredWindowAttributes := GetProcAddress(ModH, 'GetLayeredWindowAttributes');
     @SetLayeredWindowAttributes := GetProcAddress(ModH, 'SetLayeredWindowAttributes');
  end;
end;

{$endif NOVCL}

function BilleniumEffectsVersion: String;
begin
  Result := 'Version 4.3';
  {$ifdef Trial}
  Result := Result + ' trial';
  {$endif Trial}
end;

initialization
  DevicePixelFormat(True);
  GetProcessorInfo;
  TEWinVersion := GetWinVersion;

  {$ifndef NOVCL}

  {$ifdef Trial}
  {$include trial\taux.inc}
  {$endif Trial}

  InitProcs;
  TEXPRenderDisabled := False;
  TEAPIHooksDisabled := False;
  TEIsRunTimePackage := IsRunTimePackage;

  {$ifdef Trial}
  {$include trial\taux2.inc}
  {$endif Trial}

  TERegControls := TTERegControls.Create;

  RegisterTEControl('TControl'          , tePrint   , tePaint  , False, False);
  RegisterTEControl('TAnimate'          , tePrint   , tePrint  , False, True );
  RegisterTEControl('TToolBar'          , tePrint   , tePaint  , True , False);
  RegisterTEControl('TOleControl'       , teNoRender, teEmulate or teOwnCanvas, True, True);
  if SysLocale.MiddleEast then
    RegisterTEControl('TCustomGrid', tePrint, tePaint or teOwnCanvas, False, False); 
  {$ifdef D7UP}

  RegisterTEControl('TCustomEdit'       , teEmulate or teThemed, tePrint, False, False);
  RegisterTEControl('TCustomListControl', teEmulate or teThemed, tePrint, True , False);
  RegisterTEControl('TCommonCalendar'   , teEmulate or teThemed, tePrint, False, False);
  {$else}
  RegisterTEControl('TCustomListView'   , tePrint              , tePaint, True , False);
  {$endif D7UP}
  RegisterRichEdit;
  RegisterTEControl('TCustomListBox'    , teEmulate or teThemed, tePrint  , False, False);
  RegisterTEControl('TCheckListBox'     , teEmulate or teThemed, tePrint or teOwnCanvas, False, False);

  {$ifdef Trial}
  {$include trial\taux2.inc}
  {$endif Trial}

  // Third party components
  RegisterTEControl('TdxInplaceEdit'      , tePaintCopy           ,
    tePaintCopy           , False, False); // Developer Express v<= 3 inplace edit controls
  RegisterTEControl('TCustomdxBarControl' , tePaint               ,
    tePaint or teOwnCanvas, True , False); // Developer Express Bars
  RegisterTEControl('TCustomdxTreeList'   , tePaint               ,
    tePaint or teOwnCanvas, True , False); // Developer Express QuantumGrid v<=3
  RegisterTEControl('TcxGridSite'         , tePrint,
    tePaint or teOwnCanvas, False, False); // Developer Express QuantumGrid v4
  RegisterTEControl('TwwDBGrid'           , tePrint               ,
    tePaint               , False, False); // Woll2Woll InfoPower 3000 grid
  RegisterTEControl('TwwCustomMaskEdit'   , tePrint               ,
    tePaint or teRefreshFocused           , False , False ); // Woll2Woll InfoPower 3000 edit control
  RegisterTEControlCallback('TwwCustomRichEdit', teCallback, teCallback, False,
    False, PaintRichEdit3, PaintRichEdit3); // Woll2Woll InfoPower 3000 richedit
  RegisterTEControl('TWPCustomRTFControl' , tePrint, tePrint, False, False); // WPTools
  RegisterTEControl('TVirtualStringTree'  , tePaint, tePrint, False, False); // VirtualTreeView

  {$ifdef Trial}
  {$include trial\taux7.inc}
  {$endif Trial}

  {$endif NOVCL}

finalization

  {$ifndef NoVCL}
  TERegControls.Free;
  if hDWMAPI > 0 then
    FreeLibrary(hDWMAPI);
  {$endif NoVCL}

end.
