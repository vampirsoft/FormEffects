/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.TypeHelpers.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.TypeHelpers;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.OleCtrls,
  Vcl.Forms
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Graphics.Mocks
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.OleCtrls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

// For test uses Winapi.Windows and System.Types
const
  SIF_ALL   = Winapi.Windows.SIF_ALL;
  SIF_POS   = Winapi.Windows.SIF_POS;
  SIF_RANGE = Winapi.Windows.SIF_RANGE;

  WS_VSCROLL       = Winapi.Windows.WS_VSCROLL;
  WS_HSCROLL       = Winapi.Windows.WS_HSCROLL;
  WS_EX_CLIENTEDGE = Winapi.Windows.WS_EX_CLIENTEDGE;

  SWP_FRAMECHANGED = Winapi.Windows.SWP_FRAMECHANGED;
  SWP_NOACTIVATE   = Winapi.Windows.SWP_NOACTIVATE;
  SWP_NOMOVE       = Winapi.Windows.SWP_NOMOVE;
  SWP_NOSIZE       = Winapi.Windows.SWP_NOSIZE;
  SWP_NOZORDER     = Winapi.Windows.SWP_NOZORDER;

  PRF_ERASEBKGND = Winapi.Windows.PRF_ERASEBKGND;
  PRF_CLIENT     = Winapi.Windows.PRF_CLIENT;

  SB_VERT = Winapi.Windows.SB_VERT;
  SB_HORZ = Winapi.Windows.SB_HORZ;

  GWL_STYLE   = Winapi.Windows.GWL_STYLE;
  GWL_EXSTYLE = Winapi.Windows.GWL_EXSTYLE;

  GW_CHILD     = Winapi.Windows.GW_CHILD;
  GW_HWNDFIRST = Winapi.Windows.GW_HWNDFIRST;
  GW_HWNDLAST  = Winapi.Windows.GW_HWNDLAST;
  GW_HWNDPREV  = Winapi.Windows.GW_HWNDPREV;

  BDR_RAISEDINNER = Winapi.Windows.BDR_RAISEDINNER;
  BDR_SUNKENINNER = Winapi.Windows.BDR_SUNKENINNER;
  BDR_RAISEDOUTER = Winapi.Windows.BDR_RAISEDOUTER;
  BDR_SUNKENOUTER = Winapi.Windows.BDR_SUNKENOUTER;

  BF_MONO   = Winapi.Windows.BF_MONO;
  BF_ADJUST = Winapi.Windows.BF_ADJUST;
  BF_SOFT   = Winapi.Windows.BF_SOFT;
  BF_FLAT   = Winapi.Windows.BF_FLAT;

  SM_CYVSCROLL = Winapi.Windows.SM_CYVSCROLL;
  SM_CXHSCROLL = Winapi.Windows.SM_CXHSCROLL;

  PRF_NONCLIENT = Winapi.Windows.PRF_NONCLIENT;

  SIZEPALETTE = Winapi.Windows.SIZEPALETTE;

type
  HDC         = Winapi.Windows.HDC;
  HWND        = Winapi.Windows.HWND;
  HRGN        = Winapi.Windows.HRGN;
  DWORD       = Winapi.Windows.DWORD;
  HPALETTE    = Winapi.Windows.HPALETTE;
  TScrollInfo = Winapi.Windows.TScrollInfo;
  TMaxLogPalette = Winapi.Windows.TMaxLogPalette;
  TPoint      = Winapi.Windows.TPoint;
  TRect       = Winapi.Windows.TRect;
  TSize       = Winapi.Windows.TSize;
// For test uses Winapi.Windows and System.Types

type

{ TPixelFormatHelper }

  TPixelFormatHelper = record helper for TPixelFormat
  public
    class function DevicePixelFormat: TPixelFormat; static;
  end;

{ TScrollInfoHelper }

  TScrollInfoHelper = record helper for TScrollInfo
  public
    class function Create(const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo
    /// </summary>
    class function GetInfo(const Wnd: HWND; const BarFlag: Integer; const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo for SB_HORZ bar flag
    /// </summary>
    class function GetHorzScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo for SB_VERT bar flag
    /// </summary>
    class function GetVertScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo; inline; static;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo
    /// </summary>
    function LoadInfo(const Wnd: HWND; const BarFlag: Integer): BOOL; inline;
  end;

{ TSizeHelper }

  TSizeHelper = record helper for TSize
  public
    class function Zero: TSize; inline; static;
    class function InlineCreate(const Width, Height: Integer): TSize; overload; inline; static;
    class function InlineCreate(const Graphic: TGraphic): TSize; overload; inline; static;
    class function InlineCreate(const Control: TControl): TSize; overload; inline; static;
    class function InlineCreate(const ClientSize, GraphicSize: TSize): TSize; overload; inline; static;
    class function InlineCreate(const Rect: TRect): TSize; overload; inline; static;
  end;

{ TPointHelper }

  TPointHelper = record helper for TPoint
  private
    /// <summary>
    ///   Invoke Winapi.Windows.MapWindowPoints
    /// </summary>
    function MapWindowPoint(const FromWnd, ToWnd: HWND): Integer; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.MapWindowPoints
    /// </summary>
    function MapWindowPointToScreen(const FromWnd: HWND): Integer; inline;
  {$IFDEF USE_TRANSITION_EFFECTS}
    /// <summary>
    ///   Invoke Winapi.Windows.MapWindowPoints
    /// </summary>
    function MapWindowPointFromScreen(const ToWnd: HWND): Integer; inline;
  {$ENDIF ~ USE_TRANSITION_EFFECTS}

  public
    /// <summary>
    ///   Coordinate = X = Y
    /// </summary>
    class function InlineCreate(const Coordinate: Integer): TPoint; overload; inline; static;
    class function InlineCreate(const X, Y: Integer): TPoint; overload; inline; static;

  public
    class operator Negative(const Point: TPoint): TPoint; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.LPtoDP
    /// </summary>
    function ToDevicePoint(const DC: HDC): BOOL; inline;

  public
    procedure InlineSetLocation(const X, Y : Integer); overload; inline;
  {$IFDEF USE_TRANSITION_EFFECTS}
    procedure InlineSetLocation(const Point : TPoint); overload; inline;
  {$ENDIF ~ USE_TRANSITION_EFFECTS}
    function InlineSubtract(const Point: TPoint): TPoint; inline;
  end;

{ TRectHelper }

  TRectHelper = record helper for TRect
  private
    /// <summary>
    ///   Invoke Winapi.Windows.MapWindowPoints
    /// </summary>
    function MapWindowRect(const FromWnd, ToWnd: HWND): Integer; inline;
  {$IFDEF USE_TRANSITION_EFFECTS}
    /// <summary>
    ///   Invoke Winapi.Windows.MapWindowPoints
    /// </summary>
    function MapWindowRectToScreen(const FromWnd: HWND): Integer; inline;
  {$ENDIF ~ USE_TRANSITION_EFFECTS}
    /// <summary>
    ///   Invoke Winapi.Windows.MapWindowPoints
    /// </summary>
    function MapWindowRectFromScreen(const ToWnd: HWND): Integer; inline;

  public
    class function Zero: TRect; static; inline;
    class function InlineCreate(const Left, Top, Right, Bottom: Integer): TRect; overload; inline; static;
    class function InlineCreate(const Size: TSize): TRect; overload; inline; static;
    class function InlineCreate(const Size: TSize; const Margin: Word): TRect; overload; inline; static;
  {$IFDEF USE_TRANSITION_EFFECTS}
  private
    class function InlineCreate(const Point: TPoint; const Width, Height: Integer): TRect; overload; inline; static;

  public
  {$ENDIF ~ USE_TRANSITION_EFFECTS}
    class function InlineCreate(const Left, Top: Integer; const Size: TSize): TRect; overload; inline; static;
    class function InlineCreate(const Point: TPoint; const Size: TSize): TRect; overload; inline; static;
    class function InlineCreate(
      const Left, Top: Integer;
      const Size: TSize;
      const Margin: Word
    ): TRect; overload; inline; static;
// Specific constructors;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectRect
    /// </summary>
    class function IntersectRects(const Rect1, Rect2: TRect): TRect; inline; static;

  public
    /// <summary>
    ///   Return Rect as Left, Top, Right, Bottom of Control if self is empty, otherwise - self
    /// </summary>
    function ResolveRect(const Control: TControl): TRect; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.IsRectEmpty
    /// </summary>
    function IsEmptyRect: Boolean; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.EqualRect
    /// </summary>
    function IsEqual(const Rect: TRect): BOOL; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.InflateRect
    /// </summary>
    function InflateRect(const Delta: Integer): BOOL; overload; inline;
  {$IFDEF USE_TRANSITION_EFFECTS}
  private
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetRect
    /// </summary>
    procedure OffsetRect(const Delta: Integer); overload; inline;

  public
  {$ENDIF ~ USE_TRANSITION_EFFECTS}
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetRect
    /// </summary>
    procedure OffsetRect(const X, Y: Integer); overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetRect
    /// </summary>
    procedure OffsetRect(const Point: TPoint); overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.DPToLP
    /// </summary>
    function FromDeviceRect(const DC: HDC): BOOL; inline;
  end;

{ TBitmapHelper }

  TBitmapHelper = class helper for TBitmap
  strict private
    procedure SetSize(const Value: TSize); inline;

  public
    /// <summary>
    ///   Return new bitmap if need to destroy bitmap, otherwise - passed bitmap
    /// </summary>
    class function ResolveBitmap(const IsDestroyBitmap: Boolean; const Bitmap: TBitmap): TBitmap; inline; static;

  public
    /// <summary>
    ///   Set width and height into bitmap
    /// </summary>
    property Size: TSize write SetSize;
  end;

{ TControlHelper }

  TControlHelper = class helper for TControl
  public
//    /// <summary>
//    ///   Resturn control handle if control is WinControl, otherwise - 0
//    /// </summary>
//    function GetWinControlHandle: HWND; inline;
  end;

{ TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
  public
    function GetProtectedBorderWidth: TBorderWidth; inline;
    function GetProtectedBevelKind: TBevelKind; inline;
    function GetProtectedBevelInner: TBevelCut; inline;
    function GetProtectedBevelOuter: TBevelCut; inline;
    function GetProtectedBevelWidth: TBevelWidth; inline;
    function GetProtectedBevelEdges: TBevelEdges; inline;
    function GetProtectedCtl3D: Boolean; inline;
  end;

{ TCustomFormHelper }

  TCustomFormHelper = class helper for TCustomForm
  public
    function GetProtectedFormStyle: TFormStyle; inline;
    function GetProtectedClientHandle: HWND; inline;
  end;

  THRGNHelper = record helper for HRGN
  public type

  { TClipRegin }

    TClipRegin = record
    strict private
      FDC: HDC;
      FRgn: HRGN;
      FRgnForSelect: HRGN;

    private
      /// <summary>
      ///   Invoke Winapi.Windows.CreateRectRgn and Winapi.Windows.GetClipRgn
      /// </summary>
      class function Create(
        const DC: HDC;
        const Left, Top, Right, Bottom: Integer;
        const CheckRgn: Boolean
      ): TClipRegin; inline; static;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.SelectClipRgn and Winapi.Windows.DeleteObject
      /// </summary>
      function DeleteRegion: BOOL; inline;

    public
      property Rgn: HRGN read FRgn;
    end;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn with Left = 0, Top = 0, Right = 0, Bottom = 0
    /// </summary>
    class function Zero: HRGN; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn
    /// </summary>
    class function Create(const Left, Top, Right, Bottom: Integer): HRGN; overload; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn
    /// </summary>
    class function Create(const Rect: TRect): HRGN; overload; inline; static;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetRgn
    /// </summary>
    function Offset(const XOffset, YOffset: Integer): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetRgn
    /// </summary>
    function Offset(const Point: TPoint): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.CombineRgn
    /// </summary>
    function Combine(const SrcRgn1, SrcRgn2: HRGN; const Mode: Integer = RGN_AND): Integer; overload; inline;
  {$IFDEF USE_TRANSITION_EFFECTS}
  private
    /// <summary>
    ///   Invoke Winapi.Windows.CombineRgn
    /// </summary>
    function Combine(const SrcRgn: HRGN): Integer; overload; inline;

  public
  {$ENDIF ~ USE_TRANSITION_EFFECTS}
    /// <summary>
    ///   Invoke Winapi.Windows.GetRgnBox
    /// </summary>
    function GetRgnBox: TRect; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.DeleteObject
    /// </summary>
    function Delete: BOOL; inline;
  end;

{ THDCHelper }

  THDCHelper = record helper for HDC
  private type

  { THDC }

    THDC = record
    strict private
      FDC: HDC;
      FWnd: HWND;

    private
      /// <summary>
      ///   Invoke Winapi.Windows.GetDC
      /// </summary>
      class function Create(const Wnd: HWND): THDC; inline; static;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.ReleaseDC
      /// </summary>
      function ReleaseDC: Integer; inline;
//
//    public
//      class operator Implicit(const DC: THDC): HDC; inline;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.GetDeviceCaps
      /// </summary>
      function GetDeviceCaps(const Index: Integer): Integer; inline;
      /// <summary>
      ///   Invoke Winapi.Windows.GetSystemPaletteEntries
      /// </summary>
      function GetSystemPaletteEntries(const StartIndex, NumEntries: UINT; out PaletteEntries): UINT; inline;
      /// <summary>
      ///   Invoke Winapi.Windows.GetBrushOrgEx
      /// </summary>
      function GetBrushOrg: TPoint; inline;
    end;

  { THDCIndex }

    THDCIndex = record
    strict private
      FDC: HDC;
      FIndex: Integer;

    private
      /// <summary>
      ///   Invoke Winapi.Windows.SaveDC
      /// </summary>
      class function Create(const DC: HDC): THDCIndex; inline; static;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.RestoreDC
      /// </summary>
      function RestoreDC: BOOL; inline;
    end;

  public
    /// <summary>
    ///   Create THDC with Wnd = 0
    /// </summary>
    class function Create: THDC; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.SaveDC
    /// </summary>
    function Save: THDCIndex; inline;
  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetDeviceCaps
    /// </summary>
    function GetDeviceCaps(const Index: Integer): Integer; inline;
  {$IFDEF USE_TRANSITION_EFFECTS}
  private
    /// <summary>
    ///   Invoke Winapi.Windows.DeleteDC
    /// </summary>
    function Delete: BOOL; inline;
  {$ENDIF ~ USE_TRANSITION_EFFECTS}

  public
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const X, Y, Width, Height, XSrc, YSrc: Integer;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt
    /// </summary>
    function BitBlt(const DestDC: HDC; const Size: TSize; const Rop: DWORD = SRCCOPY): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const Point: TPoint;
      const Size: TSize;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const Point: TPoint;
      const Size: TSize;
      const SrcPoint: TPoint;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt
    /// </summary>
    function BitBlt(const DestDC: HDC; const Rect: TRect; const Rop: DWORD = SRCCOPY): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const Rect: TRect;
      const SrcPoint: TPoint;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect
    /// </summary>
    function ExcludeClip(const Rect: TRect): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect
    /// </summary>
    function ExcludeClip(const Left, Top, Right, Bottom: Integer): Integer; overload; inline;
    /// <summfary>
    ///   Invoke Winapi.Windows.ExcludeClipRect
    /// </summary>
    function ExcludeClip(const X, Y: Integer; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect
    /// </summary>
    function ExcludeClip(const Point: TPoint; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect
    /// </summary>
    function ExcludeClip(const Rect: TRect; const Padding: Integer): Integer; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect
    /// </summary>
    function IntersectClip(const Rect: TRect): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect
    /// </summary>
    function IntersectClip(const Left, Top, Right, Bottom: Integer): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect
    /// </summary>
    function IntersectClip(const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect
    /// </summary>
    function IntersectClip(const X, Y: Integer; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect
    /// </summary>
    function IntersectClip(const Point: TPoint; const Size: TSize): Integer; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.SetBrushOrgEx
    /// </summary>
    function SetBrushOrg(const X, Y: Integer): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetBrushOrgEx
    /// </summary>
    function SetBrushOrg(const Point: TPoint): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetClipRgn
    /// </summary>
    function GetClipRgn(const Rgn: HRGN): Integer; overload; inline; deprecated 'Use CreateClipRgn';
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn and Winapi.Windows.GetClipRgn
    /// </summary>
    function CreateClipRgn(
      const Left, Top, Right, Bottom: Integer;
      const CheckRgn: Boolean
    ): THRGNHelper.TClipRegin; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn and Winapi.Windows.GetClipRgn
    /// </summary>
    function CreateClipRgn(const Rect: TRect; const CheckRgn: Boolean): THRGNHelper.TClipRegin; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn and Winapi.Windows.GetClipRgn
    /// </summary>
    function CreateClipRgn(const CheckRgn: Boolean): THRGNHelper.TClipRegin; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SelectClipRgn
    /// </summary>
    function SelectClipRgn(const Rgn: HRGN): Integer; inline; deprecated 'Use TClipRgn.Delete';
    /// <summary>
    ///   Invoke Winapi.Windows.FillRect
    /// </summary>
    function Fill(const Rect: TRect; const Brush: HBRUSH): Integer; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetWindowOrgEx
    /// </summary>
    function OffsetWindowOrg(const X, Y: Integer): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetWindowOrgEx
    /// </summary>
    function OffsetWindowOrg(const Point: TPoint): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowOrgEx
    /// </summary>
    function SetWindowOrg(const X, Y: Integer): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowOrgEx
    /// </summary>
    function SetWindowOrg(const Point: TPoint): TPoint; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.DrawEdge
    /// </summary>
    function DrawEdge(var Rect: TRect; const Edge, Flags: UINT): BOOL; inline;
  end;

{ THWNDHelper }

  THWNDHelper = record helper for HWND
  public
    /// <summary>
    ///   Create THDC with Wnd = Self
    /// </summary>
    function CreateDC: THDCHelper.THDC; inline;

  public
    function GetWindowClassName: string; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IsChild
    /// </summary>
    function IsChild(const ParentWnd: HWND): Boolean; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetParent
    /// </summary>
    function GetParent: HWND; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindow
    /// </summary>
    function GetWindow(const Cmd: UINT): HWND; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IsWindowVisible
    /// </summary>
    function IsVisible: BOOL; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindowLongPtr
    /// </summary>
    function GetWindowData(const Index: Integer): LONG_PTR; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowLongPtr
    /// </summary>
    function SetWindowData(const Index: Integer; const Value: LONG_PTR): LONG_PTR; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowPos
    /// </summary>
    function SetWindowPos(
      const InsertAfterWnd: HWND;
      const Left, Top, Width, Height: Integer;
      const Flags: UINT
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowPos
    /// </summary>
    function SetWindowPos(const InsertAfterWnd: HWND; const Flags: UINT): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindowRgn
    /// </summary>
    function GetWindowRgn(const Rgn: HRGN): Integer; inline;

  public
    /// <summary>
    ///   Invoke Point.MapWindowPointToScreen
    /// </summary>
    function MapWindowPointToScreen(var Point: TPoint): Integer; inline;
    /// <summary>
    ///   Invoke Rect.MapWindowRect
    /// </summary>
    function MapWindowRect(const ToWnd: HWND; var Rect: TRect): Integer; inline;
    /// <summary>
    ///   Invoke Rect.MapWindowRectFromScreen
    /// </summary>
    function MapWindowRectFromScreen(var Rect: TRect): Integer; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetClientRect
    /// </summary>
    function GetClientRect: TRect; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindowRect
    /// </summary>
    function GetWindowRect: TRect; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.SendMessage
    /// </summary>
    function SendMessage(const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT; inline;
  end;

  THPALETTEHelper = record helper for HPALETTE
  public
    /// <summary>
    ///   Invoke Winapi.Windows.CreatePalette
    /// </summary>
    class function Create(const LogPalette: TMaxLogPalette): HPALETTE; inline; static;
  end;

  TTOleControlHelper = class helper for TOleControl
  public
    /// <summary>
    ///   Invoke Winapi.ActiveX.OleDraw
    /// </summary>
    function OleDraw(const DC: HDC; const Aspect: LongInt = DVASPECT_CONTENT): HRESULT; inline;
  end;

implementation

uses
  System.Math;

{$REGION 'Internal definitions'}

var
  FDeviceBitsPerPixel: Integer = -1;
  FDevicePixelFormat: TPixelFormat;

function CheckPixelFormatFor16Bit: TPixelFormat; inline;
begin
  const DisplayDC = GetDC(0);
  try
    const BitmapDC = CreateCompatibleDC(DisplayDC);
    try
      const Bitmap = CreateCompatibleBitmap(DisplayDC, 10, 10);
      try
        const OldBitmap = SelectObject(BitmapDC, Bitmap);
        try
          var PrevGPixel := 255;
          var Count      := 0;
          for var Green := 0 to 255 do
          begin
            const Pixel = RGB(0, Green, 0);
            SetPixel(BitmapDC, 1, 1, Pixel);
            var PrevPixel := GetPixel(BitmapDC, 1, 1);
            if GetGValue(PrevPixel) <> PrevGPixel then
              Inc(Count);
            PrevGPixel := GetGValue(PrevPixel);
          end;
          if Count > 32 then
            Result := pf16bit
          else
            Result := pf15bit;
        finally
          SelectObject(BitmapDC, OldBitmap);
        end;
      finally
        DeleteObject(Bitmap);
      end;
    finally
      DeleteDC(BitmapDC);
    end;
  finally
    ReleaseDC(0, DisplayDC);
  end;
end;

procedure CalculateDeviceBitsPerPixel; inline;
begin
  const DC = GetDC(0);
  try
    FDeviceBitsPerPixel := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
  finally
    ReleaseDC(0, DC);
  end;

  case FDeviceBitsPerPixel of
    1 :  FDevicePixelFormat := pf1bit;
    4 :  FDevicePixelFormat := pf4bit;
    8 :  FDevicePixelFormat := pf8bit;
    15:  FDevicePixelFormat := pf15bit;
    16:  FDevicePixelFormat := CheckPixelFormatFor16Bit;
    24:  FDevicePixelFormat := pf24bit;
    32:  FDevicePixelFormat := pf32bit;
    else FDevicePixelFormat := pf24bit;
  end;
end;

{$ENDREGION 'Internal definitions'}

{ TPixelFormatHelper }

class function TPixelFormatHelper.DevicePixelFormat: TPixelFormat;
begin
  Result := FDevicePixelFormat;
end;

{ TScrollInfoHelper }

class function TScrollInfoHelper.Create(const Mask: UINT): TScrollInfo;
begin
  Result.cbSize := SizeOf(Result);
  Result.fMask  := Mask;
end;

class function TScrollInfoHelper.GetHorzScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo;
begin
  Result := TScrollInfo.GetInfo(Wnd, SB_HORZ, Mask);
end;

class function TScrollInfoHelper.GetInfo(const Wnd: HWND; const BarFlag: Integer; const Mask: UINT): TScrollInfo;
begin
  Result := TScrollInfo.Create(Mask);
  Result.LoadInfo(Wnd, BarFlag);
end;

class function TScrollInfoHelper.GetVertScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo;
begin
  Result := TScrollInfo.GetInfo(Wnd, SB_VERT, Mask);
end;

function TScrollInfoHelper.LoadInfo(const Wnd: HWND; const BarFlag: Integer): BOOL;
begin
  Result := Winapi.Windows.GetScrollInfo(Wnd, BarFlag, Self);
end;

{ TSizeHelper }

class function TSizeHelper.InlineCreate(const Width, Height: Integer): TSize;
begin
  Result.Width  := Width;
  Result.Height := Height;
end;

class function TSizeHelper.InlineCreate(const Graphic: TGraphic): TSize;
begin
  if Assigned(Graphic) then
    Exit(TSize.InlineCreate(Graphic.Width, Graphic.Height));
  Result := TSize.Zero;
end;

class function TSizeHelper.InlineCreate(const ClientSize, GraphicSize: TSize): TSize;
begin
  if (ClientSize.Width / ClientSize.Height) > (GraphicSize.Width / GraphicSize.Height) then
  begin
    Exit(TSize.InlineCreate((GraphicSize.Width * ClientSize.Height) div GraphicSize.Height, ClientSize.Height));
  end;
  Result := TSize.InlineCreate(ClientSize.Width, (GraphicSize.Height * ClientSize.Width) div GraphicSize.Width);
end;

class function TSizeHelper.InlineCreate(const Rect: TRect): TSize;
begin
  Result := TSize.InlineCreate(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

class function TSizeHelper.InlineCreate(const Control: TControl): TSize;
begin
  Result := TSize.InlineCreate(Control.Width, Control.Height);
end;

class function TSizeHelper.Zero: TSize;
begin
  Result.Width  := 0;
  Result.Height := 0;
end;

{ TPointHelper }

class function TPointHelper.InlineCreate(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointHelper.MapWindowPoint(const FromWnd, ToWnd: HWND): Integer;
begin
  Result := Winapi.Windows.MapWindowPoints(FromWnd, ToWnd, Self, 1);
end;

{$IFDEF USE_TRANSITION_EFFECTS}

function TPointHelper.MapWindowPointFromScreen(const ToWnd: HWND): Integer;
begin
  Result := MapWindowPoint(HWND_DESKTOP, ToWnd);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

function TPointHelper.MapWindowPointToScreen(const FromWnd: HWND): Integer;
begin
  Result := MapWindowPoint(FromWnd, HWND_DESKTOP);
end;

class operator TPointHelper.Negative(const Point: TPoint): TPoint;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;

function TPointHelper.ToDevicePoint(const DC: HDC): BOOL;
begin
  Result := Winapi.Windows.LPtoDP(DC, Self, 1);
end;

{$IFDEF USE_TRANSITION_EFFECTS}

procedure TPointHelper.InlineSetLocation(const Point: TPoint);
begin
  Self := Point;
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

function TPointHelper.InlineSubtract(const Point: TPoint): TPoint;
begin
  Result.InlineSetLocation(Self.X - Point.X, Self.Y - Point.Y);
end;

class function TPointHelper.InlineCreate(const Coordinate: Integer): TPoint;
begin
  Result := TPoint.Create(Coordinate, Coordinate);
end;

procedure TPointHelper.InlineSetLocation(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

{ TRectHelper }

class function TRectHelper.InlineCreate(const Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left   := Left;
  Result.Top    := Top;
  Result.Right  := Right;
  Result.Bottom := Bottom;
end;

{$IFDEF USE_TRANSITION_EFFECTS}

class function TRectHelper.InlineCreate(const Point: TPoint; const Width, Height: Integer): TRect;
begin
  Result := TRect.InlineCreate(Point.X, Point.Y, Point.X + Width, Point.Y + Height);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

class function TRectHelper.InlineCreate(const Left, Top: Integer; const Size: TSize): TRect;
begin
  Result := TRect.InlineCreate(Left, Top, Left + Size.Width, Top + Size.Height);
end;

function TRectHelper.FromDeviceRect(const DC: HDC): BOOL;
begin
  Result := Winapi.Windows.DPtoLP(DC, Self, 2);
end;

{$IFDEF USE_TRANSITION_EFFECTS}

function TRectHelper.MapWindowRectToScreen(const FromWnd: HWND): Integer;
begin
  Result := MapWindowRect(FromWnd, HWND_DESKTOP);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

function TRectHelper.InflateRect(const Delta: Integer): BOOL;
begin
  Result := Winapi.Windows.InflateRect(Self, Delta, Delta);
end;

class function TRectHelper.InlineCreate(const Left, Top: Integer; const Size: TSize; const Margin: Word): TRect;
begin
  Result := TRect.InlineCreate(Left + Margin, Top + Margin, Size);
end;

class function TRectHelper.InlineCreate(const Point: TPoint; const Size: TSize): TRect;
begin
  Result := TRect.InlineCreate(Point.X, Point.Y, Size);
end;

class function TRectHelper.InlineCreate(const Size: TSize; const Margin: Word): TRect;
begin
  Result := TRect.InlineCreate(0, 0, Size, Margin);
end;

class function TRectHelper.IntersectRects(const Rect1, Rect2: TRect): TRect;
begin
  Winapi.Windows.IntersectRect(Result, Rect1, Rect2);
end;

function TRectHelper.IsEmptyRect: Boolean;
begin
  Result := Winapi.Windows.IsRectEmpty(Self);
end;

function TRectHelper.IsEqual(const Rect: TRect): BOOL;
begin
  Result := Winapi.Windows.EqualRect(Self, Rect);
end;

class function TRectHelper.InlineCreate(const Size: TSize): TRect;
begin
  Result := TRect.InlineCreate(0, 0, Size.Width, Size.Height);
end;

function TRectHelper.MapWindowRect(const FromWnd, ToWnd: HWND): Integer;
begin
  Result := Winapi.Windows.MapWindowPoints(FromWnd, ToWnd, Self, 2);
end;

function TRectHelper.ResolveRect(const Control: TControl): TRect;
begin
  if IsEmptyRect then
    Exit(TRect.InlineCreate(0, 0, Control.ClientWidth, Control.ClientHeight));
  Result := Self;
end;

function TRectHelper.MapWindowRectFromScreen(const ToWnd: HWND): Integer;
begin
  Result := MapWindowRect(HWND_DESKTOP, ToWnd);
end;

procedure TRectHelper.OffsetRect(const Point: TPoint);
begin
  OffsetRect(Point.X, Point.Y);
end;

procedure TRectHelper.OffsetRect(const X, Y: Integer);
begin
  Winapi.Windows.OffsetRect(Self, X, Y);
end;

{$IFDEF USE_TRANSITION_EFFECTS}

procedure TRectHelper.OffsetRect(const Delta: Integer);
begin
  OffsetRect(Delta, Delta);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

class function TRectHelper.Zero: TRect;
begin
  Result := TRect.InlineCreate(0, 0, 0, 0);
end;

{ TBitmapHelper }

class function TBitmapHelper.ResolveBitmap(const IsDestroyBitmap: Boolean; const Bitmap: TBitmap): TBitmap;
begin
  if IsDestroyBitmap then
    Exit(TBitmap.Create);
  Result := Bitmap;
end;

procedure TBitmapHelper.SetSize(const Value: TSize);
begin
  Self.Width  := Value.Width;
  Self.Height := Value.Height;
end;

{ TControlHelper }

//function TControlHelper.GetWinControlHandle: HWND;
//begin
//  if Self is TWinControl then Exit((Self as TWinControl).Handle);
//  Result := 0;
//end;

{ TWinControlHelper }

function TWinControlHelper.GetProtectedBevelEdges: TBevelEdges;
begin
  Result := Self.BevelEdges;
end;

function TWinControlHelper.GetProtectedBevelInner: TBevelCut;
begin
  Result := Self.BevelInner;
end;

function TWinControlHelper.GetProtectedBevelKind: TBevelKind;
begin
  Result := Self.BevelKind;
end;

function TWinControlHelper.GetProtectedBevelOuter: TBevelCut;
begin
  Result := Self.BevelOuter;
end;

function TWinControlHelper.GetProtectedBevelWidth: TBevelWidth;
begin
  Result := Self.BevelWidth;
end;

function TWinControlHelper.GetProtectedBorderWidth: TBorderWidth;
begin
  Result := Self.BorderWidth;
end;

function TWinControlHelper.GetProtectedCtl3D: Boolean;
begin
  Result := Self.Ctl3D;
end;

{ TCustomFormHelper }

function TCustomFormHelper.GetProtectedClientHandle: HWND;
begin
  Result := Self.ClientHandle;
end;

function TCustomFormHelper.GetProtectedFormStyle: TFormStyle;
begin
  Result := Self.FormStyle;
end;

{ THRGNHelper }

{$IFDEF USE_TRANSITION_EFFECTS}

function THRGNHelper.Combine(const SrcRgn: HRGN): Integer;
begin
  Result := Combine(SrcRgn, 0, RGN_COPY);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

function THRGNHelper.Combine(const SrcRgn1, SrcRgn2: HRGN; const Mode: Integer): Integer;
begin
  Result := Winapi.Windows.CombineRgn(Self, SrcRgn1, SrcRgn2, Mode);
end;

class function THRGNHelper.Create(const Rect: TRect): HRGN;
begin
  Result := HRGN.Create(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

function THRGNHelper.Delete: BOOL;
begin
  Result := Winapi.Windows.DeleteObject(Self);
end;

function THRGNHelper.GetRgnBox: TRect;
begin
  Winapi.Windows.GetRgnBox(Self, Result);
end;

function THRGNHelper.Offset(const Point: TPoint): Integer;
begin
  Result := Offset(Point.X, Point.Y);
end;

function THRGNHelper.Offset(const XOffset, YOffset: Integer): Integer;
begin
  Result := Winapi.Windows.OffsetRgn(Self, XOffset, YOffset);
end;

class function THRGNHelper.Zero: HRGN;
begin
  Result := HRGN.Create(0, 0, 0, 0);
end;

class function THRGNHelper.Create(const Left, Top, Right, Bottom: Integer): HRGN;
begin
  Result := Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom);
end;

{ THRGNHelper.TClipRegin }

class function THRGNHelper.TClipRegin.Create(
  const DC: HDC;
  const Left, Top, Right, Bottom: Integer;
  const CheckRgn: Boolean
): TClipRegin;
begin
  with Result do
  begin
    const CreatedRgn = HRGN.Create(Left, Top, Right, Bottom);

    FDC  := DC;
    FRgn := CreatedRgn;

    const GetClipRgnResult = Winapi.Windows.GetClipRgn(DC, CreatedRgn);
    if (GetClipRgnResult <> 1) and CheckRgn then
      FRgnForSelect := 0
    else
      FRgnForSelect := CreatedRgn;
  end;
end;

function THRGNHelper.TClipRegin.DeleteRegion: BOOL;
begin
  Winapi.Windows.SelectClipRgn(FDC, FRgnForSelect);
  Result := Winapi.Windows.DeleteObject(FRgn);
end;

{ THDCHelper }

function THDCHelper.ExcludeClip(const Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := Winapi.Windows.ExcludeClipRect(Self, Left, Top, Right, Bottom);
end;

function THDCHelper.ExcludeClip(const X, Y: Integer; const Size: TSize): Integer;
begin
  Result := ExcludeClip(X, Y, X + Size.Width, Y + Size.Height);
end;

function THDCHelper.ExcludeClip(const Point: TPoint; const Size: TSize): Integer;
begin
  Result := ExcludeClip(Point.X, Point.Y, Size);
end;

function THDCHelper.BitBlt(const DestDC: HDC; const X, Y, Width, Height, XSrc, YSrc: Integer; const Rop: DWORD): BOOL;
begin
  Result := Winapi.Windows.BitBlt(DestDC, X, Y, Width, Height, Self, XSrc, YSrc, Rop);
end;

function THDCHelper.BitBlt(const DestDC: HDC; const Size: TSize; const Rop: DWORD): BOOL;
begin
  Result := BitBlt(DestDC, 0, 0, Size.Width, Size.Height, 0, 0, Rop);
end;

function THDCHelper.BitBlt(const DestDC: HDC; const Point: TPoint; const Size: TSize; const Rop: DWORD): BOOL;
begin
  Result := BitBlt(DestDC, Point, Size, Point, Rop);
end;

function THDCHelper.BitBlt(const DestDC: HDC; const Rect: TRect; const Rop: DWORD): BOOL;
begin
  Result := BitBlt(DestDC, Rect, Rect.TopLeft, Rop);
end;

function THDCHelper.BitBlt(const DestDC: HDC; const Rect: TRect; const SrcPoint: TPoint; const Rop: DWORD): BOOL;
begin
  Result := BitBlt(DestDC, Rect.TopLeft, TSize.InlineCreate(Rect), SrcPoint, Rop);
end;

function THDCHelper.BitBlt(
  const DestDC: HDC;
  const Point: TPoint;
  const Size: TSize;
  const SrcPoint: TPoint;
  const Rop: DWORD
): BOOL;
begin
  Result := BitBlt(DestDC, Point.X, Point.Y, Size.Width, Size.Height, SrcPoint.X, SrcPoint.Y, Rop);
end;

class function THDCHelper.Create: THDC;
begin
  Result := THDC.Create(0);
end;

function THDCHelper.CreateClipRgn(const Rect: TRect; const CheckRgn: Boolean): THRGNHelper.TClipRegin;
begin
  Result := THRGNHelper.TClipRegin.Create(Self, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, CheckRgn);
end;

function THDCHelper.CreateClipRgn(const CheckRgn: Boolean): THRGNHelper.TClipRegin;
begin
  Result := THRGNHelper.TClipRegin.Create(Self, 0, 0, 0, 0, CheckRgn);
end;

function THDCHelper.DrawEdge(var Rect: TRect; const Edge, Flags: UINT): BOOL;
begin
  Result := Winapi.Windows.DrawEdge(Self, Rect, Edge, Flags);
end;

{$IFDEF USE_TRANSITION_EFFECTS}

function THDCHelper.Delete: BOOL;
begin
  Result := Winapi.Windows.DeleteDC(Self);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

function THDCHelper.ExcludeClip(const Rect: TRect; const Padding: Integer): Integer;
begin
  Result := ExcludeClip(Rect.Left + Padding, Rect.Top + Padding, Rect.Right - Padding, Rect.Bottom - Padding);
end;

function THDCHelper.Fill(const Rect: TRect; const Brush: HBRUSH): Integer;
begin
  Result := Winapi.Windows.FillRect(Self, Rect, Brush);
end;

function THDCHelper.GetClipRgn(const Rgn: HRGN): Integer;
begin
  Result := Winapi.Windows.GetClipRgn(Self, Rgn);
end;

function THDCHelper.CreateClipRgn(
  const Left, Top, Right, Bottom: Integer;
  const CheckRgn: Boolean
): THRGNHelper.TClipRegin;
begin
  Result := THRGNHelper.TClipRegin.Create(Self, Left, Top, Right, Bottom, CheckRgn)
end;

function THDCHelper.GetDeviceCaps(const Index: Integer): Integer;
begin
  Result := Winapi.Windows.GetDeviceCaps(Self, Index);
end;

function THDCHelper.ExcludeClip(const Rect: TRect): Integer;
begin
  Result := ExcludeClip(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

function THDCHelper.IntersectClip(const Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := Winapi.Windows.IntersectClipRect(Self, Left, Top, Right, Bottom);
end;

function THDCHelper.IntersectClip(const Size: TSize): Integer;
begin
  Result := IntersectClip(0, 0, Size.Width, Size.Height);
end;

function THDCHelper.IntersectClip(const X, Y: Integer; const Size: TSize): Integer;
begin
  Result := IntersectClip(TRect.InlineCreate(X, Y, Size));
end;

function THDCHelper.IntersectClip(const Point: TPoint; const Size: TSize): Integer;
begin
  Result := IntersectClip(Point.X, Point.Y, Size);
end;

function THDCHelper.OffsetWindowOrg(const Point: TPoint): TPoint;
begin
  Result := OffsetWindowOrg(Point.X, Point.Y);
end;

function THDCHelper.OffsetWindowOrg(const X, Y: Integer): TPoint;
begin
  Winapi.Windows.OffsetWindowOrgEx(Self, X, Y, Result);
end;

function THDCHelper.Save: THDCIndex;
begin
  Result := THDCIndex.Create(Self);
end;

function THDCHelper.SelectClipRgn(const Rgn: HRGN): Integer;
begin
  Result := Winapi.Windows.SelectClipRgn(Self, Rgn);
end;

function THDCHelper.SetBrushOrg(const Point: TPoint): TPoint;
begin
  Result := SetBrushOrg(Point.X, Point.Y);
end;

function THDCHelper.SetWindowOrg(const Point: TPoint): TPoint;
begin
  Result := SetWindowOrg(Point.X, Point.Y);
end;

function THDCHelper.SetWindowOrg(const X, Y: Integer): TPoint;
begin
  Winapi.Windows.SetWindowOrgEx(Self, X, Y, @Result);
end;

function THDCHelper.SetBrushOrg(const X, Y: Integer): TPoint;
begin
  Winapi.Windows.SetBrushOrgEx(Self, X, Y, @Result);
end;

function THDCHelper.IntersectClip(const Rect: TRect): Integer;
begin
  Result := IntersectClip(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

{ THDCHelper.THDC }

class function THDCHelper.THDC.Create(const Wnd: HWND): THDC;
begin
  Result.FWnd := Wnd;
  Result.FDC  := Winapi.Windows.GetDC(Wnd);
end;

function THDCHelper.THDC.GetBrushOrg: TPoint;
begin
  Winapi.Windows.GetBrushOrgEx(FDC, Result);
end;

function THDCHelper.THDC.GetDeviceCaps(const Index: Integer): Integer;
begin
  Result := FDC.GetDeviceCaps(Index);
end;

function THDCHelper.THDC.GetSystemPaletteEntries(const StartIndex, NumEntries: UINT; out PaletteEntries): UINT;
begin
  Result := Winapi.Windows.GetSystemPaletteEntries(FDC, StartIndex, NumEntries, PaletteEntries);
end;

//class operator THDCHelper.THDC.Implicit(const DC: THDC): HDC;
//begin
//  Result := DC.FDC;
//end;

function THDCHelper.THDC.ReleaseDC: Integer;
begin
  Result := Winapi.Windows.ReleaseDC(FWnd, FDC);
end;

{ THDCHelper.THDCIndex }

class function THDCHelper.THDCIndex.Create(const DC: HDC): THDCIndex;
begin
  with Result do
  begin
    FDC    := DC;
    FIndex := Winapi.Windows.SaveDC(DC);
  end;
end;

function THDCHelper.THDCIndex.RestoreDC: BOOL;
begin
  Result := Winapi.Windows.RestoreDC(FDC, FIndex);
end;

{ THWNDHelper }

function THWNDHelper.CreateDC: THDCHelper.THDC;
begin
  Result := THDCHelper.THDC.Create(Self);
end;

function THWNDHelper.GetClientRect: TRect;
begin
  Winapi.Windows.GetClientRect(Self, Result);
end;

function THWNDHelper.GetParent: HWND;
begin
  Result := Winapi.Windows.GetParent(Self);
end;

function THWNDHelper.GetWindow(const Cmd: UINT): HWND;
begin
  Result := Winapi.Windows.GetWindow(Self, Cmd);
end;

function THWNDHelper.GetWindowClassName: string;
const
  MaxClassNameLength = 257; //256 plus null terminator

var
  ClassName: array[0..MaxClassNameLength - 1] of Char;

begin
  const ClassNameLength = Winapi.Windows.GetClassName(Self, ClassName, Length(ClassName));
  SetString(Result, ClassName, ClassNameLength);
end;

function THWNDHelper.GetWindowData(const Index: Integer): LONG_PTR;
begin
  Result := Winapi.Windows.GetWindowLongPtr(Self, Index);
end;

function THWNDHelper.GetWindowRect: TRect;
begin
  Winapi.Windows.GetWindowRect(Self, Result);
end;

function THWNDHelper.GetWindowRgn(const Rgn: HRGN): Integer;
begin
  Result := Winapi.Windows.GetWindowRgn(Self, Rgn);
end;

function THWNDHelper.IsChild(const ParentWnd: HWND): Boolean;
begin
  Result := Winapi.Windows.IsChild(ParentWnd, Self);
end;

function THWNDHelper.IsVisible: BOOL;
begin
  Result := Winapi.Windows.IsWindowVisible(Self);
end;

function THWNDHelper.MapWindowRect(const ToWnd: HWND; var Rect: TRect): Integer;
begin
  Result := Rect.MapWindowRect(Self, ToWnd);
end;

function THWNDHelper.MapWindowRectFromScreen(var Rect: TRect): Integer;
begin
  Result := Rect.MapWindowRectFromScreen(Self);
end;

function THWNDHelper.MapWindowPointToScreen(var Point: TPoint): Integer;
begin
  Result := Point.MapWindowPointToScreen(Self);
end;

function THWNDHelper.SendMessage(const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT;
begin
  Result := Winapi.Windows.SendMessage(Self, Msg, wParam, lParam);
end;

function THWNDHelper.SetWindowData(const Index: Integer; const Value: LONG_PTR): LONG_PTR;
begin
  Result := Winapi.Windows.SetWindowLongPtr(Self, Index, Value);
end;

function THWNDHelper.SetWindowPos(const InsertAfterWnd: HWND; const Flags: UINT): BOOL;
begin
  Result := SetWindowPos(InsertAfterWnd, 0, 0, 0, 0, Flags);
end;

function THWNDHelper.SetWindowPos(
  const InsertAfterWnd: HWND;
  const Left, Top, Width, Height: Integer;
  const Flags: UINT
): BOOL;
begin
  Result := Winapi.Windows.SetWindowPos(Self, InsertAfterWnd, Left, Top, Width, Height, Flags);
end;

{ THPALETTEHelper }

class function THPALETTEHelper.Create(const LogPalette: TMaxLogPalette): HPALETTE;
begin
  Result := Winapi.Windows.CreatePalette(PLogPalette(@LogPalette)^);
end;

{ TTOleControlHelper }

function TTOleControlHelper.OleDraw(const DC: HDC; const Aspect: LongInt): HRESULT;
begin
  Result := Winapi.ActiveX.OleDraw(OleObject, Aspect, DC, ClientRect);
end;

initialization
  CalculateDeviceBitsPerPixel;

end.
