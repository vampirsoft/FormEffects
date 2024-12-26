/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.TypeHelpers.pas                                *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
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
  System.SysUtils,
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

{$IFDEF FORM_EFFECTS_TESTS}
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
{$ENDIF ~ FORM_EFFECTS_TESTS}

type

{ TScrollInfoHelper }

  TScrollInfoHelper = record helper for TScrollInfo
  public
    class function Create(const Mask: UINT): TScrollInfo; overload; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo(Wnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo)
    /// </summary>
    class function GetInfo(const Wnd: HWND; const BarFlag: Integer; const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo(Wnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo) for SB_HORZ bar flag
    /// </summary>
    class function GetHorzScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo(Wnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo) for SB_VERT bar flag
    /// </summary>
    class function GetVertScrollInfo(const Wnd: HWND; const Mask: UINT): TScrollInfo; inline; static;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetScrollInfo(Wnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo)
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

  public
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
  end;

{ TRectHelper }

  TRectHelper = record helper for TRect
  {$IFDEF USE_TRANSITION_EFFECTS}
  private
    class function InlineCreate(const Point: TPoint; const Width, Height: Integer): TRect; overload; inline; static;
  {$ENDIF ~ USE_TRANSITION_EFFECTS}
  public
    class function Zero: TRect; static; inline;
    class function InlineCreate(const Left, Top, Right, Bottom: Integer): TRect; overload; inline; static;
    class function InlineCreate(const Size: TSize): TRect; overload; inline; static;
    class function InlineCreate(const Left, Top: Integer; const Size: TSize): TRect; overload; inline; static;
    class function InlineCreate(const Point: TPoint; const Size: TSize): TRect; overload; inline; static;
    class function InlineCreate(const Size: TSize; const Margin: Word): TRect; overload; inline; static;
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
    /// <summary>
    ///   Invoke Winapi.Windows.UnionRect
    /// </summary>
    class function UnionRects(const Rect1, Rect2: TRect): TRect; inline; static;

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
    ///   Invoke Winapi.Windows.LPtoDP
    /// </summary>
    function ToDeviceRect(const DC: HDC): BOOL; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.DPToLP
    /// </summary>
    function FromDeviceRect(const DC: HDC): BOOL; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom: Integer) with Rect properties
    /// </summary>
    function CreateRectRgn: HRGN; inline;

  public
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
  end;

{ TGraphicHelper }

  TGraphicHelper = class helper for TGraphic
  strict private
    function GetSize: TSize; inline;
    procedure SetSize(const Value: TSize); inline;

  public
    /// <summary>
    ///   Get and Set width and height
    /// </summary>
    property Size: TSize read GetSize write SetSize;
  end;

{ TControlHelper }

  TControlHelper = class helper for TControl
  public
    function GetProtectedColor: TColor; inline;
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
    procedure SetProtectedBorderIcons(const Icons: TBorderIcons); inline;
  end;

  THRGNHelper = record helper for HRGN
  public type

  { TClipRegin }

    TClipRegin = record
    strict private
      FDC: HDC;
      FRegion: HRGN;
      FRgnForSelect: HRGN;

    private
      /// <summary>
      ///   Invoke Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom: Integer) and Winapi.Windows.GetClipRgn(DC: HDC; Rgn: HRGN)
      /// </summary>
      class function Create(
        const DC: HDC;
        const Left, Top, Right, Bottom: Integer;
        const CheckRgn: Boolean
      ): TClipRegin; inline; static;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.SelectClipRgn(DC: HDC; Rgn: HRGN) and Winapi.Windows.DeleteObject(Obj: HGDIOBJ)
      /// </summary>
      function DeleteRegion: BOOL; inline;

    public
      property Region: HRGN read FRegion;
    end;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom: Integer) with Left = 0, Top = 0, Right = 0, Bottom = 0
    /// </summary>
    class function CreateRectRgn: HRGN; overload; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom: Integer)
    /// </summary>
    class function CreateRectRgn(const Left, Top, Right, Bottom: Integer): HRGN; overload; inline; static;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetRgn(Rgn: HRGN; XOffset, YOffset: Integer)
    /// </summary>
    function OffsetRgn(const XOffset, YOffset: Integer): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetRgn(Rgn: HRGN; XOffset, YOffset: Integer) with Point properties
    /// </summary>
    function OffsetRgn(const Point: TPoint): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.CombineRgn(Result, SrcRgn1, SrcRgn2: HRGN; Mode: Integer)
    /// </summary>
    function CombineRgn(const SrcRgn1, SrcRgn2: HRGN; const Mode: Integer = RGN_AND): Integer; overload; inline;
  {$IFDEF USE_TRANSITION_EFFECTS}
  private
    /// <summary>
    ///   Invoke Winapi.Windows.CombineRgn
    /// </summary>
    function CombineRgn(const SrcRgn: HRGN): Integer; overload; inline;

  public
  {$ENDIF ~ USE_TRANSITION_EFFECTS}
    /// <summary>
    ///   Invoke Winapi.Windows.GetRgnBox(Rgn: HRGN; var Rect: TRect)
    /// </summary>
    function GetRgnBox: TRect; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.DeleteObject(Obj: HGDIOBJ)
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
      ///   Invoke Winapi.Windows.GetDC(Wnd: HWND)
      /// </summary>
      class function Create(const Wnd: HWND): THDC; inline; static;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.ReleaseDC(Wnd: HWND; DC: HDC)
      /// </summary>
      function ReleaseDC: Integer; inline;
//
//    public
//      class operator Implicit(const DC: THDC): HDC; inline;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.GetDeviceCaps(DC: HDC; Index: Integer)
      /// </summary>
      function GetDeviceCaps(const Index: Integer): Integer; inline;
      /// <summary>
      ///   Invoke Winapi.Windows.GetSystemPaletteEntries(DC: HDC; StartIndex, NumEntries: UINT; var PaletteEntries)
      /// </summary>
      function GetSystemPaletteEntries(const StartIndex, NumEntries: UINT; out PaletteEntries): UINT; inline;
      /// <summary>
      ///   Invoke Winapi.Windows.GetBrushOrgEx(DC: HDC; var Point: TPoint)
      /// </summary>
      function GetBrushOrgEx: TPoint; inline;
      /// <summary>
      ///   Invoke Winapi.Windows.CreateCompatibleDC(DC: HDC)
      /// </summary>
      function CreateCompatibleDC: HDC; inline;
      /// <summary>
      ///   Invoke Winapi.Windows.CreateCompatibleBitmap(DC: HDC; Width, Height: Integer)
      /// </summary>
      function CreateCompatibleBitmap(const Width, Height: Integer): HBITMAP; inline;
    end;

  { THDCIndex }

    THDCIndex = record
    strict private
      FDC: HDC;
      FIndex: Integer;

    private
      /// <summary>
      ///   Invoke Winapi.Windows.SaveDC(DC: HDC)
      /// </summary>
      class function Create(const DC: HDC): THDCIndex; inline; static;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.RestoreDC(DC: HDC; Index: Integer)
      /// </summary>
      function RestoreDC: BOOL; inline;
    end;

    THGDIOBJ = record
    strict private
      FDC: HDC;
      FObj: HGDIOBJ;

    private
      /// <summary>
      ///   Invoke Winapi.Windows.SelectObject(DC: HDC; Obj: HGDIOBJ)
      /// </summary>
      class function Create(const DC: HDC; const Obj: HGDIOBJ): THGDIOBJ; inline; static;

    public
      /// <summary>
      ///   Invoke Winapi.Windows.SelectObject(DC: HDC; Obj: HGDIOBJ)
      /// </summary>
      function Delete: HGDIOBJ; inline;
    end;

  public
    /// <summary>
    ///   Create THDC with Wnd = 0 (Invoke Winapi.Windows.GetDC(Wnd: HWND))
    /// </summary>
    class function Create: THDC; inline; static;
    /// <summary>
    ///   Invoke Winapi.Windows.SaveDC(DC: HDC)
    /// </summary>
    function Save: THDCIndex; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.DeleteDC(DC: HDC)
    /// </summary>
    function Delete: BOOL; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetDeviceCaps(DC: HDC; Index: Integer)
    /// </summary>
    function GetDeviceCaps(const Index: Integer): Integer; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SelectObject(DC: HDC; Obj: HGDIOBJ)
    /// </summary>
    function Select(const Obj: HGDIOBJ): THGDIOBJ; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetPixel(DC: HDC; X, Y: Integer)
    /// </summary>
    function GetPixel(const X, Y: Integer): COLORREF; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetPixel(DC: HDC; X, Y: Integer; Color: COLORREF)
    /// </summary>
    function SetPixel(const X, Y: Integer; const Color: COLORREF): COLORREF; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD)
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const X, Y, Width, Height, XSrc, YSrc: Integer;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD)
    /// </summary>
    function BitBlt(const DestDC: HDC; const Size: TSize; const Rop: DWORD = SRCCOPY): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD)
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const Point: TPoint;
      const Size: TSize;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD)
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const Point: TPoint;
      const Size: TSize;
      const SrcPoint: TPoint;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD)
    /// </summary>
    function BitBlt(const DestDC: HDC; const Rect: TRect; const Rop: DWORD = SRCCOPY): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Rop: DWORD)
    /// </summary>
    function BitBlt(
      const DestDC: HDC;
      const Rect: TRect;
      const SrcPoint: TPoint;
      const Rop: DWORD = SRCCOPY
    ): BOOL; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function ExcludeClipRect(const Rect: TRect): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function ExcludeClipRect(const Left, Top, Right, Bottom: Integer): Integer; overload; inline;
    /// <summfary>
    ///   Invoke Winapi.Windows.ExcludeClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function ExcludeClipRect(const X, Y: Integer; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function ExcludeClipRect(const Point: TPoint; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ExcludeClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function ExcludeClipRect(const Rect: TRect; const Padding: Integer): Integer; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function IntersectClipRect(const Rect: TRect): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function IntersectClipRect(const Left, Top, Right, Bottom: Integer): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function IntersectClipRect(const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function IntersectClipRect(const X, Y: Integer; const Size: TSize): Integer; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IntersectClipRect(DC: HDC; Left, Top, Right, Bottom: Integer)
    /// </summary>
    function IntersectClipRect(const Point: TPoint; const Size: TSize): Integer; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.SetBrushOrgEx(DC: HDC; X, Y: Integer; Point: PPoint)
    /// </summary>
    function SetBrushOrgEx(const X, Y: Integer): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetBrushOrgEx(DC: HDC; X, Y: Integer; Point: PPoint)
    /// </summary>
    function SetBrushOrgEx(const Point: TPoint): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetClipRgn(DC: HDC; Rgn: HRGN)
    /// </summary>
    function GetClipRgn(const Rgn: HRGN): Integer; overload; inline; deprecated 'Use CreateClipRgn';
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom: Integer) and Winapi.Windows.GetClipRgn(DC: HDC; Rgn: HRGN)
    /// </summary>
    function CreateClipRgn(
      const Left, Top, Right, Bottom: Integer;
      const CheckRgn: Boolean
    ): THRGNHelper.TClipRegin; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom: Integer) and Winapi.Windows.GetClipRgn(DC: HDC; Rgn: HRGN)
    /// </summary>
    function CreateClipRgn(const Rect: TRect; const CheckRgn: Boolean): THRGNHelper.TClipRegin; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.CreateRectRgn(Left, Top, Right, Bottom: Integer) and Winapi.Windows.GetClipRgn(DC: HDC; Rgn: HRGN)
    /// </summary>
    function CreateClipRgn(const CheckRgn: Boolean): THRGNHelper.TClipRegin; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SelectClipRgn(DC: HDC; Rgn: HRGN)
    /// </summary>
    function SelectClipRgn(const Rgn: HRGN): Integer; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.FillRect(DC: HDC; const Rect: TRect; Brush: HBRUSH)
    /// </summary>
    function FillRect(const Rect: TRect; const Brush: HBRUSH): Integer; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetWindowOrgEx(DC: HDC; X, Y: Integer; var Points)
    /// </summary>
    function OffsetWindowOrgEx(const X, Y: Integer): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.OffsetWindowOrgEx(DC: HDC; X, Y: Integer; var Points)
    /// </summary>
    function OffsetWindowOrgEx(const Point: TPoint): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowOrgEx(DC: HDC; X, Y: Integer; Point: PPoint)
    /// </summary>
    function SetWindowOrgEx(const X, Y: Integer): TPoint; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowOrgEx(DC: HDC; X, Y: Integer; Point: PPoint)
    /// </summary>
    function SetWindowOrgEx(const Point: TPoint): TPoint; overload; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.DrawEdge(DC: HDC; var Rect: TRect; Edge: UINT; Flags: UINT)
    /// </summary>
    function DrawEdge(var Rect: TRect; const Edge, Flags: UINT): BOOL; inline;
  end;

{ THWNDHelper }

  THWNDHelper = record helper for HWND
  public
    /// <summary>
    ///   Create THDC with Wnd = Self (Invoke Winapi.Windows.GetDC(Wnd: HWND))
    /// </summary>
    function CreateDC: THDCHelper.THDC; inline;

  public
    function GetWindowClassName: string; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IsChild(ParentWnd, Wnd: HWND)
    /// </summary>
    function IsChild(const ParentWnd: HWND): Boolean; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetParent(Wnd: HWND)
    /// </summary>
    function GetParent: HWND; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindow(Wnd: HWND; Cmd: UINT)
    /// </summary>
    function GetWindow(const Cmd: UINT): HWND; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.IsWindowVisible(Wnd: HWND)
    /// </summary>
    function IsWindowVisible: BOOL; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindowLongPtr(Wnd: HWND; Index: Integer)
    /// </summary>
    function GetWindowLongPtr(const Index: Integer): LONG_PTR; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowLongPtr(Wnd: HWND; Index: Integer; Value: LONG_PTR)
    /// </summary>
    function SetWindowLongPtr(const Index: Integer; const Value: LONG_PTR): LONG_PTR; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowPos(Wnd: HWND; InsertAfterWnd: HWND; Left, Top, Width, Height: Integer; Flags: UINT)
    /// </summary>
    function SetWindowPos(
      const InsertAfterWnd: HWND;
      const Left, Top, Width, Height: Integer;
      const Flags: UINT
    ): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowPos(Wnd: HWND; InsertAfterWnd: HWND; Left, Top, Width, Height: Integer; Flags: UINT)
    /// </summary>
    function SetWindowPos(const InsertAfterWnd: HWND; const Flags: UINT): BOOL; overload; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.SetWindowRgn(Wnd: HWND; Rgn: HRGN; Redraw: BOOL)
    /// </summary>
    function SetWindowRgn(const Rgn: HRGN; const Redraw: BOOL): Integer; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ShowWindow(Wnd: HWND; CmdShow: Integer)
    /// </summary>
    function ShowWindow(const CmdShow: Integer): BOOL; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindowRgn(Wnd: HWND; Rgn: HRGN)
    /// </summary>
    function GetWindowRgn(const Rgn: HRGN): Integer; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetClientRect(Wnd: HWND; var Rect: TRect)
    /// </summary>
    function GetClientRect: TRect; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.GetWindowRect(Wnd: HWND; var Rect: TRect)
    /// </summary>
    function GetWindowRect: TRect; inline;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.GetUpdateRect(Wnd: HWND; Rect: PRect; Erase: BOOL) with nil as Rect
    /// </summary>
    function HasUpdateRect(const Erase: Boolean): BOOL; inline;
    /// <summary>
    ///   Invoke Winapi.Windows.ValidateRect(Wnd: HWND; Rect: PRect) with nil as Rect
    /// </summary>
    function ValidateRect: BOOL;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.SendMessage(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM)
    /// </summary>
    function SendMessage(const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT; inline;
  end;

{ THPALETTEHelper }

  THPALETTEHelper = record helper for HPALETTE
  public
    /// <summary>
    ///   Invoke Winapi.Windows.CreatePalette(LogPalette: PLogPalette)
    /// </summary>
    class function Create(const LogPalette: TMaxLogPalette): HPALETTE; inline; static;
  end;

{ THBITMAPHelper }

  THBITMAPHelper = record helper for HBITMAP
  public
    /// <summary>
    ///   Invoke Winapi.Windows.DeleteObject(Obj: HGDIOBJ)
    /// </summary>
    function Delete: BOOL; inline;
  end;

{ THBRUSHHelper }

  THBRUSHHelper = record helper for HBRUSH
  public
    /// <summary>
    ///   Invoke Winapi.Windows.CreateSolidBrush(Color: COLORREF)
    /// </summary>
    class function Create(const Color: COLORREF): HBRUSH; inline; static;

  public
    /// <summary>
    ///   Invoke Winapi.Windows.DeleteObject(Obj: HGDIOBJ)
    /// </summary>
    function Delete: BOOL; inline;
  end;

{ TTOleControlHelper }

  TTOleControlHelper = class helper for TOleControl
  public
    /// <summary>
    ///   Invoke Winapi.ActiveX.OleDraw(Unknown: IUnknown; Aspect: Longint; DC: HDC; const Bounds: TRect)
    /// </summary>
    function OleDraw(const DC: HDC; const Aspect: LongInt = DVASPECT_CONTENT): HRESULT; inline;
  end;

{ TExceptionHelper }

  TExceptionHelper = class helper for Exception
  public
    constructor CreateResFmt(const ResStringRec: PString; const Args: array of const); overload;
  end;

implementation

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
    Result := TSize.InlineCreate(Graphic.Width, Graphic.Height)
  else
    Result := TSize.Zero;
end;

class function TSizeHelper.InlineCreate(const ClientSize, GraphicSize: TSize): TSize;
begin
  if (ClientSize.Width / ClientSize.Height) > (GraphicSize.Width / GraphicSize.Height) then
    Result := TSize.InlineCreate((GraphicSize.Width * ClientSize.Height) div GraphicSize.Height, ClientSize.Height)
  else
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

function TRectHelper.CreateRectRgn: HRGN;
begin
  Result := HRGN.CreateRectRgn(Self.Left, Self.Top, Self.Right, Self.Bottom);
end;

function TRectHelper.FromDeviceRect(const DC: HDC): BOOL;
begin
  Result := Winapi.Windows.DPtoLP(DC, Self, 2);
end;

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

function TRectHelper.MapWindowRectFromScreen(const ToWnd: HWND): Integer;
begin
  Result := MapWindowRect(HWND_DESKTOP, ToWnd);
end;

function TRectHelper.ToDeviceRect(const DC: HDC): BOOL;
begin
  Result := Winapi.Windows.LPToDP(DC, Self, 2);
end;

class function TRectHelper.UnionRects(const Rect1, Rect2: TRect): TRect;
begin
  Winapi.Windows.UnionRect(Result, Rect1, Rect2);
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

function TRectHelper.MapWindowRectToScreen(const FromWnd: HWND): Integer;
begin
  Result := MapWindowRect(FromWnd, HWND_DESKTOP);
end;

procedure TRectHelper.OffsetRect(const Delta: Integer);
begin
  OffsetRect(Delta, Delta);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

class function TRectHelper.Zero: TRect;
begin
  Result := TRect.InlineCreate(0, 0, 0, 0);
end;

{ TGraphicHelper }

function TGraphicHelper.GetSize: TSize;
begin
  Result.Width  := Width;
  Result.Height := Height;
end;

procedure TGraphicHelper.SetSize(const Value: TSize);
begin
  Self.Width  := Value.Width;
  Self.Height := Value.Height;
end;

{ TControlHelper }

function TControlHelper.GetProtectedColor: TColor;
begin
  Result := Self.Color;
end;

//function TControlHelper.GetWinControlHandle: HWND;
//begin
//  if Self is TWinControl then
//    Result := TWinControl(Self).Handle
//  else
//    Result := 0;
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

procedure TCustomFormHelper.SetProtectedBorderIcons(const Icons: TBorderIcons);
begin
  Self.BorderIcons := Icons;
end;

{ THRGNHelper }

{$IFDEF USE_TRANSITION_EFFECTS}

function THRGNHelper.CombineRgn(const SrcRgn: HRGN): Integer;
begin
  Result := CombineRgn(SrcRgn, 0, RGN_COPY);
end;

{$ENDIF ~ USE_TRANSITION_EFFECTS}

function THRGNHelper.CombineRgn(const SrcRgn1, SrcRgn2: HRGN; const Mode: Integer): Integer;
begin
  Result := Winapi.Windows.CombineRgn(Self, SrcRgn1, SrcRgn2, Mode);
end;

function THRGNHelper.Delete: BOOL;
begin
  Result := Winapi.Windows.DeleteObject(Self);
end;

function THRGNHelper.GetRgnBox: TRect;
begin
  Winapi.Windows.GetRgnBox(Self, Result);
end;

function THRGNHelper.OffsetRgn(const Point: TPoint): Integer;
begin
  Result := OffsetRgn(Point.X, Point.Y);
end;

function THRGNHelper.OffsetRgn(const XOffset, YOffset: Integer): Integer;
begin
  Result := Winapi.Windows.OffsetRgn(Self, XOffset, YOffset);
end;

class function THRGNHelper.CreateRectRgn: HRGN;
begin
  Result := HRGN.CreateRectRgn(0, 0, 0, 0);
end;

class function THRGNHelper.CreateRectRgn(const Left, Top, Right, Bottom: Integer): HRGN;
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
    const CreatedRegion = HRGN.CreateRectRgn(Left, Top, Right, Bottom);

    FDC  := DC;
    FRegion := CreatedRegion;

    const GetClipRgnResult = Winapi.Windows.GetClipRgn(DC, CreatedRegion);
    if (GetClipRgnResult <> 1) and CheckRgn then
      FRgnForSelect := 0
    else
      FRgnForSelect := CreatedRegion;
  end;
end;

function THRGNHelper.TClipRegin.DeleteRegion: BOOL;
begin
  FDC.SelectClipRgn(FRgnForSelect);
  Result := FRegion.Delete;
end;

{ THDCHelper }

function THDCHelper.ExcludeClipRect(const Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := Winapi.Windows.ExcludeClipRect(Self, Left, Top, Right, Bottom);
end;

function THDCHelper.ExcludeClipRect(const X, Y: Integer; const Size: TSize): Integer;
begin
  Result := ExcludeClipRect(X, Y, X + Size.Width, Y + Size.Height);
end;

function THDCHelper.ExcludeClipRect(const Point: TPoint; const Size: TSize): Integer;
begin
  Result := ExcludeClipRect(Point.X, Point.Y, Size);
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

function THDCHelper.Delete: BOOL;
begin
  Result := Winapi.Windows.DeleteDC(Self);
end;

function THDCHelper.ExcludeClipRect(const Rect: TRect; const Padding: Integer): Integer;
begin
  Result := ExcludeClipRect(Rect.Left + Padding, Rect.Top + Padding, Rect.Right - Padding, Rect.Bottom - Padding);
end;

function THDCHelper.FillRect(const Rect: TRect; const Brush: HBRUSH): Integer;
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

function THDCHelper.GetPixel(const X, Y: Integer): COLORREF;
begin
  Result := Winapi.Windows.GetPixel(Self, X, Y);
end;

function THDCHelper.ExcludeClipRect(const Rect: TRect): Integer;
begin
  Result := ExcludeClipRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

function THDCHelper.IntersectClipRect(const Left, Top, Right, Bottom: Integer): Integer;
begin
  Result := Winapi.Windows.IntersectClipRect(Self, Left, Top, Right, Bottom);
end;

function THDCHelper.IntersectClipRect(const Size: TSize): Integer;
begin
  Result := IntersectClipRect(0, 0, Size.Width, Size.Height);
end;

function THDCHelper.IntersectClipRect(const X, Y: Integer; const Size: TSize): Integer;
begin
  Result := IntersectClipRect(TRect.InlineCreate(X, Y, Size));
end;

function THDCHelper.IntersectClipRect(const Point: TPoint; const Size: TSize): Integer;
begin
  Result := IntersectClipRect(Point.X, Point.Y, Size);
end;

function THDCHelper.OffsetWindowOrgEx(const Point: TPoint): TPoint;
begin
  Result := OffsetWindowOrgEx(Point.X, Point.Y);
end;

function THDCHelper.OffsetWindowOrgEx(const X, Y: Integer): TPoint;
begin
  Winapi.Windows.OffsetWindowOrgEx(Self, X, Y, Result);
end;

function THDCHelper.Save: THDCIndex;
begin
  Result := THDCIndex.Create(Self);
end;

function THDCHelper.Select(const Obj: HGDIOBJ): THGDIOBJ;
begin
  Result := THGDIOBJ.Create(Self, Obj);
end;

function THDCHelper.SelectClipRgn(const Rgn: HRGN): Integer;
begin
  Result := Winapi.Windows.SelectClipRgn(Self, Rgn);
end;

function THDCHelper.SetBrushOrgEx(const Point: TPoint): TPoint;
begin
  Result := SetBrushOrgEx(Point.X, Point.Y);
end;

function THDCHelper.SetPixel(const X, Y: Integer; const Color: COLORREF): COLORREF;
begin
  Result := Winapi.Windows.SetPixel(Self, X, Y, Color);
end;

function THDCHelper.SetWindowOrgEx(const Point: TPoint): TPoint;
begin
  Result := SetWindowOrgEx(Point.X, Point.Y);
end;

function THDCHelper.SetWindowOrgEx(const X, Y: Integer): TPoint;
begin
  Winapi.Windows.SetWindowOrgEx(Self, X, Y, @Result);
end;

function THDCHelper.SetBrushOrgEx(const X, Y: Integer): TPoint;
begin
  Winapi.Windows.SetBrushOrgEx(Self, X, Y, @Result);
end;

function THDCHelper.IntersectClipRect(const Rect: TRect): Integer;
begin
  Result := IntersectClipRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

{ THDCHelper.THDC }

class function THDCHelper.THDC.Create(const Wnd: HWND): THDC;
begin
  Result.FWnd := Wnd;
  Result.FDC  := Winapi.Windows.GetDC(Wnd);
end;

function THDCHelper.THDC.CreateCompatibleBitmap(const Width, Height: Integer): HBITMAP;
begin
  Result := Winapi.Windows.CreateCompatibleBitmap(FDC, Width, Height);
end;

function THDCHelper.THDC.CreateCompatibleDC: HDC;
begin
  Result := Winapi.Windows.CreateCompatibleDC(FDC);
end;

function THDCHelper.THDC.GetBrushOrgEx: TPoint;
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

{ THDCHelper.THGDIOBJ }

class function THDCHelper.THGDIOBJ.Create(const DC: HDC; const Obj: HGDIOBJ): THGDIOBJ;
begin
  with Result do
  begin
    FDC  := DC;
    FObj := Winapi.Windows.SelectObject(DC, Obj);
  end;
end;

function THDCHelper.THGDIOBJ.Delete: HGDIOBJ;
begin
  Result := Winapi.Windows.SelectObject(FDC, FObj);
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

function THWNDHelper.GetWindowLongPtr(const Index: Integer): LONG_PTR;
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

function THWNDHelper.HasUpdateRect(const Erase: Boolean): BOOL;
begin
  Result := Winapi.Windows.GetUpdateRect(Self, nil, Erase);
end;

function THWNDHelper.IsChild(const ParentWnd: HWND): Boolean;
begin
  Result := Winapi.Windows.IsChild(ParentWnd, Self);
end;

function THWNDHelper.IsWindowVisible: BOOL;
begin
  Result := Winapi.Windows.IsWindowVisible(Self);
end;

function THWNDHelper.SendMessage(const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT;
begin
  Result := Winapi.Windows.SendMessage(Self, Msg, wParam, lParam);
end;

function THWNDHelper.SetWindowLongPtr(const Index: Integer; const Value: LONG_PTR): LONG_PTR;
begin
  Result := Winapi.Windows.SetWindowLongPtr(Self, Index, Value);
end;

function THWNDHelper.SetWindowPos(const InsertAfterWnd: HWND; const Flags: UINT): BOOL;
begin
  Result := SetWindowPos(InsertAfterWnd, 0, 0, 0, 0, Flags);
end;

function THWNDHelper.SetWindowRgn(const Rgn: HRGN; const Redraw: BOOL): Integer;
begin
  Result := Winapi.Windows.SetWindowRgn(Self, Rgn, Redraw);
end;

function THWNDHelper.ShowWindow(const CmdShow: Integer): BOOL;
begin
  Result := Winapi.Windows.ShowWindow(Self, CmdShow);
end;

function THWNDHelper.ValidateRect: BOOL;
begin
  Result := Winapi.Windows.ValidateRect(Self, nil);
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

{ THBITMAPHelper }

function THBITMAPHelper.Delete: BOOL;
begin
  Result := Winapi.Windows.DeleteObject(Self);
end;

{ THBRUSHHelper }

class function THBRUSHHelper.Create(const Color: COLORREF): HBRUSH;
begin
  Result := Winapi.Windows.CreateSolidBrush(Color);
end;

function THBRUSHHelper.Delete: BOOL;
begin
  Result := Winapi.Windows.DeleteObject(Self);
end;

{ TTOleControlHelper }

function TTOleControlHelper.OleDraw(const DC: HDC; const Aspect: LongInt): HRESULT;
begin
  Result := Winapi.ActiveX.OleDraw(OleObject, Aspect, DC, ClientRect);
end;

{ TExceptionHelper }

constructor TExceptionHelper.CreateResFmt(const ResStringRec: PString; const Args: array of const);
begin
  CreateResFmt(PResStringRec(ResStringRec), Args);
end;

{ TControlHelper }

end.
