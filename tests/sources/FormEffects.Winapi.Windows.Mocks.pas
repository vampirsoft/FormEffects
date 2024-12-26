/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Winapi.Windows.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Winapi.Windows.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Types,
  FormEffects.Utils.Mocks;

type

{ TWinapiWindowsMocks }

  TWinapiWindowsMocks = class abstract(TMocksManager)
  public
    function GetWindowLongPtr(const Wnd: HWND; const Index: Integer): LONG_PTR; virtual;
    procedure SetWindowPos(
      const Wnd, InsertAfterWnd: HWND;
      const Left, Top, Width, Height: Integer;
      const Flags: UINT
    ); virtual; abstract;
    function GetScrollInfo(const Wnd: HWND; const BarFlag: Integer): TScrollInfo; virtual; abstract;
    function GetClientRect(const Wnd: HWND): TRect; virtual;
    function GetWindowRect(const Wnd: HWND): TRect; virtual;
    function SendMessage(
      const Wnd: HWND;
      const Msg: UINT;
      const wParam: WPARAM;
      const lParam: LPARAM
    ): LRESULT; virtual;
    function GetDC(const Wnd: HWND): HDC; virtual; abstract;
    function GetDeviceCaps(const DC: HDC; const Index: Integer): Integer; virtual;
    function CreatePalette: HPALETTE; virtual;
    function IsChild(const ParentWnd, Wnd: HWND): Boolean; virtual;
    function GetParent(const Wnd: HWND): HWND; virtual;
    function GetWindow(const Wnd: HWND; const Cmd: UINT): HWND; virtual; abstract;
    function IsWindowVisible(const Wnd: HWND): BOOL; virtual; abstract;
    function GetBrushOrgEx(const DC: HDC): TPoint; virtual; abstract;
    function SetBrushOrgEx(const DC: HDC; const X, Y: Integer): TPoint; virtual; abstract;
    function CreateRectRgn(const Left, Top, Right, Bottom: Integer): HRGN; virtual;
    function GetWindowRgn(const Wnd: HWND; const Rgn: HRGN): Integer; virtual; abstract;
    function GetClipRgn(const DC: HDC; const Rgn: HRGN): Integer; virtual;
    function GetRgnBox(const Rgn: HRGN): TRect; virtual; abstract;
    function OffsetWindowOrgEx(const DC: HDC; const X, Y: Integer): TPoint; virtual;
    function SetWindowOrgEx(const DC: HDC; const X, Y: Integer): TPoint; virtual;
    function SaveDC(const DC: HDC): Integer; virtual;
    function CreateCompatibleDC(const DC: HDC): HDC; virtual; abstract;
    function CreateCompatibleBitmap(const DC: HDC; const Width, Height: Integer): HBITMAP; virtual; abstract;
    function SelectObject(const DC: HDC; const Obj: HGDIOBJ): HGDIOBJ; virtual;
    function GetPixel(const DC: HDC; const X, Y: Integer): COLORREF; virtual; abstract;
    function SetPixel(const DC: HDC; const X, Y: Integer; const Color: COLORREF): COLORREF; virtual;
    function GetUpdateRect(const Wnd: HWND; Rect: PRect; const Erase: Boolean): Boolean; virtual; abstract;

  public
    procedure SetWindowLongPtr(const Wnd: HWND; const Index: Integer; const Value: LONG_PTR); virtual; abstract;
    procedure ReleaseDC(const Wnd: HWND; const DC: HDC); virtual; abstract;
    procedure DeleteDC(const DC: HDC); virtual; abstract;
    procedure GetSystemPaletteEntries(const DC: HDC; const StartIndex, NumEntries: UINT); virtual; abstract;
    procedure BitBlt(
      const DestDC: HDC;
      const X, Y, Width, Height: Integer;
      const SrcDC: HDC;
      const XSrc, YSrc: Integer;
      const Rop: DWORD
    ); virtual; abstract;
    procedure DeleteObject(const Obj: HGDIOBJ); virtual; abstract;
    procedure OffsetRgn(const Rgn: HRGN; const XOffset, YOffset: Integer); virtual; abstract;
    procedure CombineRgn(const DestRgn, SrcRgn1, SrcRgn2: HRGN; const Mode: Integer); virtual; abstract;
    procedure SelectClipRgn(const DC: HDC; const Rgn: HRGN); virtual; abstract;
    procedure IntersectClipRect(const DC: HDC; const Left, Top, Right, Bottom: Integer); virtual; abstract;
    procedure OleDraw(
      const Unknown: IUnknown;
      const Aspect: LongInt;
      const DC: HDC;
      const Bounds: TRect
    ); virtual; abstract;
    procedure DrawEdge(const DC: HDC; const Rect: TRect; const Edge, Flags: UINT); virtual; abstract;
    procedure RestoreDC(const DC: HDC; const Index: Integer); virtual; abstract;
    procedure FillRect(const DC: HDC; const Rect: TRect; const Brush: HBRUSH); virtual; abstract;
    procedure ExcludeClipRect(const DC: HDC; const Left, Top, Right, Bottom: Integer); virtual; abstract;
    procedure GetSystemMetrics(const Index: Integer); virtual; abstract;
    procedure SetWindowRgn(const Wnd: HWND; const Rgn: HRGN; const Redraw: BOOL); virtual; abstract;
    procedure ShowWindow(const Wnd: HWND; const CmdShow: Integer); virtual; abstract;
    procedure ValidateRect(const Wnd: HWND; const Rect: PRect); virtual; abstract;

  public
    constructor Create; override;
  end;

implementation

uses
  FormEffects.TypeHelpers;

var
  WinapiWindowsMocks: TWinapiWindowsMocks;

type
  TGetWindowLongPtr = function(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall;

function GetWindowLongPtrMock(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall;
begin
  Result := WinapiWindowsMocks.GetWindowLongPtr(hWnd, nIndex);
end;

function TWinapiWindowsMocks.GetWindowLongPtr(const Wnd: HWND; const Index: Integer): LONG_PTR;
begin
  Result := 0;
end;

type
  TSetWindowLongPtr = function(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall;

function SetWindowLongPtrMock(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall;
begin
  WinapiWindowsMocks.SetWindowLongPtr(hWnd, nIndex, dwNewLong);
  Result := 0;
end;

type
  TSetWindowPos = function(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx, cy: Integer; uFlags: UINT): BOOL; stdcall;

function SetWindowPosMock(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx, cy: Integer; uFlags: UINT): BOOL; stdcall;
begin
  WinapiWindowsMocks.SetWindowPos(hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
  Result := True;
end;

{$IFDEF USE_BILLENIUM_EFFECTS}

type
  TGetScrollRange = function(hWnd: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): BOOL; stdcall;

function GetScrollRangeMock(hWnd: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): BOOL; stdcall;
begin
  const ScrollInfo = WinapiWindowsMocks.GetScrollInfo(hWnd, nBar);
  lpMinPos        := ScrollInfo.nMin;
  lpMaxPos        := ScrollInfo.nMax;
  Result := True;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

type
  TGetScrollInfo = function(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL; stdcall;

function GetScrollInfoMock(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL; stdcall;
begin
  ScrollInfo := WinapiWindowsMocks.GetScrollInfo(hWnd, BarFlag);
  Result     := True;
end;

type
  TGetClientRect = function(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;

function GetClientRectMock(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;
begin
  lpRect := WinapiWindowsMocks.GetClientRect(hWnd);
  Result := True;
end;

function TWinapiWindowsMocks.GetClientRect(const Wnd: HWND): TRect;
begin
  Result := TRect.Zero;
end;

type
  TGetWindowRect = function(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;

function GetWindowRectMock(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;
begin
  lpRect := WinapiWindowsMocks.GetWindowRect(hWnd);
  Result := true;
end;

function TWinapiWindowsMocks.GetWindowRect(const Wnd: HWND): TRect;
begin
  Result := TRect.Zero;
end;

type
  TMapWindowPoints = function(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer; stdcall;

function MapWindowPoint(hWndFrom, hWndTo: HWND; var lpPoints: TPoint; cPoints: UINT): Integer;
begin
  lpPoints.Offset(hWndTo - hWndFrom, hWndTo - hWndFrom);
  Result := cPoints;
end;

function MapWindowRect(hWndFrom, hWndTo: HWND; var lpPoints: TRect; cPoints: UINT): Integer;
begin
  MapWindowPoint(hWndFrom, hWndTo, lpPoints.TopLeft, 1);
  MapWindowPoint(hWndFrom, hWndTo, lpPoints.BottomRight, 1);
  Result := cPoints;
end;

function MapWindowPointsMock(hWndFrom, hWndTo: HWND; var lpPoints; cPoints: UINT): Integer; stdcall;
begin
  if cPoints = 2 then
    Exit(MapWindowRect(hWndFrom, hWndTo, PRect(@lpPoints)^, cPoints));
  Result := MapWindowPoint(hWndFrom, hWndTo, PPoint(@lpPoints)^, cPoints);
end;

{$IFDEF USE_BILLENIUM_EFFECTS}

type
  TClientToScreen = function(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;

function ClientToScreenMock(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;
begin
  MapWindowPoint(hWnd, HWND_DESKTOP, lpPoint, 1);
  Result := True;
end;

type
  TScreenToClient = function(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;

function ScreenToClientMock(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;
begin
  MapWindowPoint(HWND_DESKTOP, hWnd, lpPoint, 1);
  Result := True;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

type
  TSendMessageUnsigned = function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

function SendMessageWideMock(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := WinapiWindowsMocks.SendMessage(hWnd, Msg, wParam, lParam);
end;

function TWinapiWindowsMocks.SendMessage(
  const Wnd: HWND;
  const Msg: UINT;
  const wParam: WPARAM;
  const lParam: LPARAM
): LRESULT;
begin
  Result := -1;
end;

type
  TGetDC = function(hWnd: HWND): HDC; stdcall;

function GetDCMock(hWnd: HWND): HDC; stdcall;
begin
  Result := WinapiWindowsMocks.GetDC(hWnd);
end;

type
  TReleaseDC = function(hWnd: HWND; hDC: HDC): Integer; stdcall;

function ReleaseDCMock(hWnd: HWND; hDC: HDC): Integer; stdcall;
begin
  WinapiWindowsMocks.ReleaseDC(hWnd, hDC);
  Result := -1;
end;

type
  TDeleteDC = function(DC: HDC): BOOL; stdcall;

function DeleteDCMock(DC: HDC): BOOL; stdcall;
begin
  WinapiWindowsMocks.DeleteDC(DC);
  Result := False;
end;

type
  TGetDeviceCaps = function(DC: HDC; Index: Integer): Integer; stdcall;

function GetDeviceCapsMock(DC: HDC; Index: Integer): Integer; stdcall;
begin
  Result := WinapiWindowsMocks.GetDeviceCaps(DC, Index);
end;

function TWinapiWindowsMocks.GetDeviceCaps(const DC: HDC; const Index: Integer): Integer;
begin
  Result := DC * Index;
end;

type
  TGetSystemPaletteEntries =
    function(
      DC: HDC;
      StartIndex, NumEntries: UINT;
      var PaletteEntries
    ): UINT; stdcall;

function GetSystemPaletteEntriesMock(
  DC: HDC;
  StartIndex, NumEntries: UINT;
  var PaletteEntries
): UINT; stdcall;
begin
  WinapiWindowsMocks.GetSystemPaletteEntries(DC, StartIndex, NumEntries);
  Result := 0;
end;

type
  TCreatePalette = function(LogPalette: PLogPalette): HPALETTE; stdcall;

function CreatePaletteMock(LogPalette: PLogPalette): HPALETTE; stdcall;
begin
  Result := WinapiWindowsMocks.CreatePalette;
end;

function TWinapiWindowsMocks.CreatePalette: HPALETTE;
begin
  Result := 0;
end;

type
  TIsChild = function(hWndParent, hWnd: HWND): BOOL; stdcall;

function IsChildMock(hWndParent, hWnd: HWND): BOOL; stdcall;
begin
  Result := WinapiWindowsMocks.IsChild(hWndParent, hWnd);
end;

function TWinapiWindowsMocks.IsChild(const ParentWnd, Wnd: HWND): Boolean;
begin
  Result := False;
end;

type
  TGetParent = function(hWnd: HWND): HWND; stdcall;

function GetParentMock(hWnd: HWND): HWND; stdcall;
begin
  Result := WinapiWindowsMocks.GetParent(hWnd);
end;

function TWinapiWindowsMocks.GetParent(const Wnd: HWND): HWND;
begin
  Result := 0;
end;

type
  TGetWindow = function(hWnd: HWND; uCmd: UINT): HWND; stdcall;

function GetWindowMock(hWnd: HWND; uCmd: UINT): HWND; stdcall;
begin
  Result := WinapiWindowsMocks.GetWindow(hWnd, uCmd);
end;

type
  TIsWindowVisible = function(hWnd: HWND): BOOL; stdcall;

function IsWindowVisibleMock(hWnd: HWND): BOOL; stdcall;
begin
  Result := WinapiWindowsMocks.IsWindowVisible(hWnd);
end;

type
  TBitBlt =
    function(
      DestDC: HDC;
      X, Y, Width, Height: Integer;
      SrcDC: HDC;
      XSrc, YSrc: Integer;
      Rop: DWORD
    ): BOOL; stdcall;

function BitBltMock(
  DestDC: HDC;
  X, Y, Width, Height: Integer;
  SrcDC: HDC;
  XSrc, YSrc: Integer;
  Rop: DWORD
): BOOL; stdcall;
begin
  WinapiWindowsMocks.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
  Result := True;
end;

type
  TGetBrushOrgEx = function(DC: HDC; var lppt: TPoint): BOOL; stdcall;

function GetBrushOrgExMock(DC: HDC; var lppt: TPoint): BOOL; stdcall;
begin
  lppt := WinapiWindowsMocks.GetBrushOrgEx(DC);
  Result := True;
end;

type
  TSetBrushOrgEx = function(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL; stdcall;

function SetBrushOrgExMock(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL; stdcall;
begin
  var Prev := WinapiWindowsMocks.SetBrushOrgEx(DC, X, Y);
  if Assigned(PrevPt) then
    PrevPt^ := Prev;
  Result := True;
end;

procedure LPtoDP(const DC: HDC; var Point: TPoint); overload;
begin
  Point.Offset(-DC, -DC);
end;

procedure LPtoDP(const DC: HDC; var Rect: TRect); overload;
begin
  LPtoDP(DC, Rect.TopLeft);
  LPtoDP(DC, Rect.BottomRight);
end;

type
  TLPtoDP = function(DC: HDC; var Points; Count: Integer): BOOL; stdcall;

function LPtoDPMock(DC: HDC; var Points; Count: Integer): BOOL; stdcall;
begin
  if Count = 2 then
    LPtoDP(DC, PRect(@Points)^)
  else
    LPtoDP(DC, PPoint(@Points)^);

  Result := True;
end;

procedure DPtoLP(const DC: HDC; var Point: TPoint); overload;
begin
  Point.Offset(DC, DC);
end;

procedure DPtoLP(const DC: HDC; var Rect: TRect); overload;
begin
  DPtoLP(DC, Rect.TopLeft);
  DPtoLP(DC, Rect.BottomRight);
end;

type
  TDPtoLP = function(DC: HDC; var Points; Count: Integer): BOOL; stdcall;

function DPtoLPMock(DC: HDC; var Points; Count: Integer): BOOL; stdcall;
begin
  if Count = 2 then
    DPtoLP(DC, PRect(@Points)^)
  else
    DPtoLP(DC, PPoint(@Points)^);

  Result := True;
end;

type
  TDeleteObject = function(p1: HGDIOBJ): BOOL; stdcall;

function DeleteObjectMock(p1: HGDIOBJ): BOOL; stdcall;
begin
  WinapiWindowsMocks.DeleteObject(p1);
  Result := True;
end;

type
  TCreateRectRgn = function(p1, p2, p3, p4: Integer): HRGN; stdcall;

function CreateRectRgnMock(p1, p2, p3, p4: Integer): HRGN; stdcall;
begin
  Result := WinapiWindowsMocks.CreateRectRgn(p1, p2, p3, p4);
end;

function TWinapiWindowsMocks.CreateRectRgn(const Left, Top, Right, Bottom: Integer): HRGN;
begin
  Result := 0;
end;

type
  TGetWindowRgn = function(hWnd: HWND; hRgn: HRGN): Integer; stdcall;

function GetWindowRgnMock(hWnd: HWND; hRgn: HRGN): Integer; stdcall;
begin
  Result := WinapiWindowsMocks.GetWindowRgn(hWnd, hRgn);
end;

type
  TOffsetRgn = function(RGN: HRGN; XOffset, YOffset: Integer): Integer; stdcall;

function OffsetRgnMock(RGN: HRGN; XOffset, YOffset: Integer): Integer; stdcall;
begin
  WinapiWindowsMocks.OffsetRgn(RGN, XOffset, YOffset);
  Result := 0;
end;

type
  TGetClipRgn = function(DC: HDC; rgn: HRGN): Integer; stdcall;

function GetClipRgnMock(DC: HDC; rgn: HRGN): Integer; stdcall;
begin
  Result := WinapiWindowsMocks.GetClipRgn(DC, rgn);
end;

function TWinapiWindowsMocks.GetClipRgn(const DC: HDC; const Rgn: HRGN): Integer;
begin
  Result := -1;
end;

type
  TCombineRgn = function(p1, p2, p3: HRGN; p4: Integer): Integer; stdcall;

function CombineRgnMock(p1, p2, p3: HRGN; p4: Integer): Integer; stdcall;
begin
  WinapiWindowsMocks.CombineRgn(p1, p2, p3, p4);
  Result := -1;
end;

type
  TSelectClipRgn = function(DC: HDC; Region: HRGN): Integer; stdcall;

function SelectClipRgnMock(DC: HDC; Region: HRGN): Integer; stdcall;
begin
  WinapiWindowsMocks.SelectClipRgn(DC, Region);
  Result := -1;
end;

type
  TGetRgnBox = function(RGN: HRGN; var p2: TRect): Integer; stdcall;

function GetRgnBoxMock(RGN: HRGN; var p2: TRect): Integer; stdcall;
begin
  p2 := WinapiWindowsMocks.GetRgnBox(RGN);
  Result := 0;
end;

type
  TIntersectClipRect = function(DC: HDC; X1, Y1, X2, Y2: Integer): Integer; stdcall;

function IntersectClipRectMock(DC: HDC; X1, Y1, X2, Y2: Integer): Integer; stdcall;
begin
  WinapiWindowsMocks.IntersectClipRect(DC, X1, Y1, X2, Y2);
  Result := -1;
end;

type
  TOffsetWindowOrgEx = function(DC: HDC; X, Y: Integer; var Points): BOOL; stdcall;

function OffsetWindowOrgExMock(DC: HDC; X, Y: Integer; var Points): BOOL; stdcall;
begin
  PPoint(@Points)^ := WinapiWindowsMocks.OffsetWindowOrgEx(DC, X, Y);
  Result := True;
end;

function TWinapiWindowsMocks.OffsetWindowOrgEx(const DC: HDC; const X, Y: Integer): TPoint;
begin
  Result := TPoint.InlineCreate(X, Y);
end;

type
  TSetWindowOrgEx = function(DC: HDC; X, Y: Integer; Point: PPoint): BOOL; stdcall;

function SetWindowOrgExMock(DC: HDC; X, Y: Integer; Point: PPoint): BOOL; stdcall;
begin
  const Prev = WinapiWindowsMocks.SetWindowOrgEx(DC, X, Y);
  if Assigned(Point) then
    Point^ := Prev;
  Result := True;
end;

function TWinapiWindowsMocks.SetWindowOrgEx(const DC: HDC; const X, Y: Integer): TPoint;
begin
  Result := TPoint.InlineCreate(X, Y);
end;

type
  TOleDraw = function(unknown: IUnknown; dwAspect: Longint; hdcDraw: HDC; const rcBounds: TRect): HRESULT; stdcall;

function OleDrawMock(unknown: IUnknown; dwAspect: Longint; hdcDraw: HDC; const rcBounds: TRect): HRESULT; stdcall;
begin
  WinapiWindowsMocks.OleDraw(unknown, dwAspect, hdcDraw, rcBounds);
  Result := S_OK;
end;

type
  TDrawEdge = function(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;

function DrawEdgeMock(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
begin
  WinapiWindowsMocks.DrawEdge(hdc, qrc, edge, grfFlags);
  Result := True;
end;

type
  TSaveDC = function(DC: HDC): Integer; stdcall;

function SaveDCMock(DC: HDC): Integer; stdcall;
begin
  Result := WinapiWindowsMocks.SaveDC(DC);
end;

function TWinapiWindowsMocks.SaveDC(const DC: HDC): Integer;
begin
  Result := -1;
end;

type
  TRestoreDC = function(DC: HDC; Index: Integer): BOOL; stdcall;

function RestoreDCMock(DC: HDC; Index: Integer): BOOL; stdcall;
begin
  WinapiWindowsMocks.RestoreDC(DC, Index);
  Result := True;
end;

type
  TFillRect = function(hDC: HDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;

function FillRectMock(hDC: HDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
begin
  WinapiWindowsMocks.FillRect(hDC, lprc, hbr);
  Result := -1;
end;

type
  TExcludeClipRect = function(DC: HDC; LeftRect, TopRect, RightRect, BottomRect: Integer): Integer; stdcall;

function ExcludeClipRectMock(DC: HDC; LeftRect, TopRect, RightRect, BottomRect: Integer): Integer; stdcall;
begin
  WinapiWindowsMocks.ExcludeClipRect(DC, LeftRect, TopRect, RightRect, BottomRect);
  Result := -1;
end;

type
  TGetSystemMetrics = function(Index: Integer): Integer; stdcall;

function GetSystemMetricsMock(Index: Integer): Integer; stdcall;
begin
  WinapiWindowsMocks.GetSystemMetrics(Index);
  Result := Index;
end;

type
  TSetWindowRgn = function(hWnd: HWND; hRgn: HRGN; bRedraw: BOOL): Integer; stdcall;

function SetWindowRgnMock(hWnd: HWND; hRgn: HRGN; bRedraw: BOOL): Integer; stdcall;
begin
  WinapiWindowsMocks.SetWindowRgn(hWnd, hRgn, bRedraw);
  Result := -1;
end;

type
  TShowWindow = function(hWnd: HWND; nCmdShow: Integer): BOOL; stdcall;

function ShowWindowMock(hWnd: HWND; nCmdShow: Integer): BOOL; stdcall;
begin
  WinapiWindowsMocks.ShowWindow(hWnd, nCmdShow);
  Result := True;
end;

type
  TCreateCompatibleDC = function(DC: HDC): HDC; stdcall;

function CreateCompatibleDCMock(DC: HDC): HDC; stdcall;
begin
  Result := WinapiWindowsMocks.CreateCompatibleDC(DC);
end;

type
  TCreateCompatibleBitmap = function(DC: HDC; Width, Height: Integer): HBITMAP; stdcall;

function CreateCompatibleBitmapMock(DC: HDC; Width, Height: Integer): HBITMAP; stdcall;
begin
  Result := WinapiWindowsMocks.CreateCompatibleBitmap(DC, Width, Height);
end;

type
  TSelectObject = function(DC: HDC; p2: HGDIOBJ): HGDIOBJ; stdcall;

function SelectObjectMock(DC: HDC; p2: HGDIOBJ): HGDIOBJ; stdcall;
begin
  Result := WinapiWindowsMocks.SelectObject(DC, p2);
end;

function TWinapiWindowsMocks.SelectObject(const DC: HDC; const Obj: HGDIOBJ): HGDIOBJ;
begin
  Result := Obj;
end;

type
  TGetPixel = function(DC: HDC; X, Y: Integer): COLORREF; stdcall;

function GetPixelMock(DC: HDC; X, Y: Integer): COLORREF; stdcall;
begin
  Result := WinapiWindowsMocks.GetPixel(DC, X, Y);
end;

type
  TSetPixel = function(DC: HDC; X, Y: Integer; Color: COLORREF): COLORREF; stdcall;

function SetPixelMock(DC: HDC; X, Y: Integer; Color: COLORREF): COLORREF; stdcall;
begin
  Result := WinapiWindowsMocks.SetPixel(DC, X, Y, Color);
end;

function TWinapiWindowsMocks.SetPixel(const DC: HDC; const X, Y: Integer; const Color: COLORREF): COLORREF;
begin
  Result := Color;
end;

{$IFDEF USE_BILLENIUM_EFFECTS}
type
  TGetUpdateRect = function(Wnd: HWND; var Rect: TRect; Erase: BOOL): BOOL; stdcall;

function GetUpdateRectMock(Wnd: HWND; var Rect: TRect; Erase: BOOL): BOOL; stdcall;
begin
  Result := WinapiWindowsMocks.GetUpdateRect(Wnd, @Rect, Erase);
end;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
type
  TGetUpdateRect = function(Wnd: HWND; Rect: PRect; Erase: BOOL): BOOL; stdcall;

function GetUpdateRectMock(Wnd: HWND; Rect: PRect; Erase: BOOL): BOOL; stdcall;
begin
  Result := WinapiWindowsMocks.GetUpdateRect(Wnd, Rect, Erase);
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

type
  TValidateRect = function(Wnd: HWND; Rect: PRect): BOOL; stdcall;

function ValidateRectMock(Wnd: HWND; Rect: PRect): BOOL; stdcall;
begin
  WinapiWindowsMocks.ValidateRect(Wnd, Rect);
  Result := True;
end;

{ TWinapiWindowsMocks }

constructor TWinapiWindowsMocks.Create;
begin
  inherited Create;

  AddIntercept<TGetWindowLongPtr>(Winapi.Windows.GetWindowLongPtr, GetWindowLongPtrMock);

  AddIntercept<TSetWindowLongPtr>(Winapi.Windows.SetWindowLongPtr, SetWindowLongPtrMock);

  AddIntercept<TSetWindowPos>(Winapi.Windows.SetWindowPos, SetWindowPosMock);

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetScrollRange>(GetScrollRange, GetScrollRangeMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetScrollInfo>(Winapi.Windows.GetScrollInfo, GetScrollInfoMock);

  AddIntercept<TGetClientRect>(Winapi.Windows.GetClientRect, GetClientRectMock);

  AddIntercept<TGetWindowRect>(Winapi.Windows.GetWindowRect, GetWindowRectMock);

  AddIntercept<TMapWindowPoints>(MapWindowPoints, MapWindowPointsMock);
{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TClientToScreen>(ClientToScreen, ClientToScreenMock);
  AddIntercept<TScreenToClient>(ScreenToClient, ScreenToClientMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  AddIntercept<TSendMessageUnsigned>(Winapi.Windows.SendMessage, SendMessageWideMock);

  AddIntercept<TGetDC>(Winapi.Windows.GetDC, GetDCMock);

  AddIntercept<TReleaseDC>(Winapi.Windows.ReleaseDC, ReleaseDCMock);

  AddIntercept<TDeleteDC>(Winapi.Windows.DeleteDC, DeleteDCMock);

  AddIntercept<TGetDeviceCaps>(Winapi.Windows.GetDeviceCaps, GetDeviceCapsMock);

  AddIntercept<TGetSystemPaletteEntries>(Winapi.Windows.GetSystemPaletteEntries, GetSystemPaletteEntriesMock);

  AddIntercept<TCreatePalette>(Winapi.Windows.CreatePalette, CreatePaletteMock);

  AddIntercept<TIsChild>(Winapi.Windows.IsChild, IsChildMock);

  AddIntercept<TGetParent>(Winapi.Windows.GetParent, GetParentMock);

  AddIntercept<TGetWindow>(Winapi.Windows.GetWindow, GetWindowMock);

  AddIntercept<TIsWindowVisible>(Winapi.Windows.IsWindowVisible, IsWindowVisibleMock);

  AddIntercept<TBitBlt>(Winapi.Windows.BitBlt, BitBltMock);

  AddIntercept<TGetBrushOrgEx>(Winapi.Windows.GetBrushOrgEx, GetBrushOrgExMock);

  AddIntercept<TSetBrushOrgEx>(Winapi.Windows.SetBrushOrgEx, SetBrushOrgExMock);

  AddIntercept<TLPtoDP>(Winapi.Windows.LPtoDP, LPtoDPMock);

  AddIntercept<TDPtoLP>(Winapi.Windows.DPtoLP, DPtoLPMock);

  AddIntercept<TDeleteObject>(Winapi.Windows.DeleteObject, DeleteObjectMock);

  AddIntercept<TCreateRectRgn>(Winapi.Windows.CreateRectRgn, CreateRectRgnMock);

  AddIntercept<TGetWindowRgn>(Winapi.Windows.GetWindowRgn, GetWindowRgnMock);

  AddIntercept<TOffsetRgn>(Winapi.Windows.OffsetRgn, OffsetRgnMock);

  AddIntercept<TGetClipRgn>(Winapi.Windows.GetClipRgn, GetClipRgnMock);

  AddIntercept<TCombineRgn>(Winapi.Windows.CombineRgn, CombineRgnMock);

  AddIntercept<TSelectClipRgn>(Winapi.Windows.SelectClipRgn, SelectClipRgnMock);

  AddIntercept<TGetRgnBox>(Winapi.Windows.GetRgnBox, GetRgnBoxMock);

  AddIntercept<TIntersectClipRect>(Winapi.Windows.IntersectClipRect, IntersectClipRectMock);

  AddIntercept<TOffsetWindowOrgEx>(Winapi.Windows.OffsetWindowOrgEx, OffsetWindowOrgExMock);

  AddIntercept<TSetWindowOrgEx>(Winapi.Windows.SetWindowOrgEx, SetWindowOrgExMock);

  AddIntercept<TOleDraw>(Winapi.ActiveX.OleDraw, OleDrawMock);

  AddIntercept<TDrawEdge>(Winapi.Windows.DrawEdge, DrawEdgeMock);

  AddIntercept<TSaveDC>(Winapi.Windows.SaveDC, SaveDCMock);

  AddIntercept<TRestoreDC>(Winapi.Windows.RestoreDC, RestoreDCMock);

  AddIntercept<TFillRect>(Winapi.Windows.FillRect, FillRectMock);

  AddIntercept<TExcludeClipRect>(Winapi.Windows.ExcludeClipRect, ExcludeClipRectMock);

  AddIntercept<TGetSystemMetrics>(Winapi.Windows.GetSystemMetrics, GetSystemMetricsMock);

  AddIntercept<TSetWindowRgn>(Winapi.Windows.SetWindowRgn, SetWindowRgnMock);

  AddIntercept<TShowWindow>(Winapi.Windows.ShowWindow, ShowWindowMock);

  AddIntercept<TCreateCompatibleDC>(Winapi.Windows.CreateCompatibleDC, CreateCompatibleDCMock);

  AddIntercept<TCreateCompatibleBitmap>(Winapi.Windows.CreateCompatibleBitmap, CreateCompatibleBitmapMock);

  AddIntercept<TSelectObject>(Winapi.Windows.SelectObject, SelectObjectMock);

  AddIntercept<TGetPixel>(Winapi.Windows.GetPixel, GetPixelMock);

  AddIntercept<TSetPixel>(Winapi.Windows.SetPixel, SetPixelMock);

  AddIntercept<TGetUpdateRect>(Winapi.Windows.GetUpdateRect, GetUpdateRectMock);

  AddIntercept<TValidateRect>(Winapi.Windows.ValidateRect, ValidateRectMock);

  WinapiWindowsMocks := Self;
end;

end.
