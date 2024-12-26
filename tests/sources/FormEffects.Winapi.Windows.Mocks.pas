/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Winapi.Windows.Mocks.pas                       *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Winapi.Windows.Mocks;

{$INCLUDE FormEffects.Tests.inc}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  FormEffects.Utils.Mocks;

type

{ TWinapiWindowsMocks }

  TWinapiWindowsMocks = class(TMocksManager)
  private type
    TGetWindowLongPtrResultReference = reference to function(const Wnd: HWND; const Index: Integer): LONG_PTR;
  public
    procedure GetWindowLongPtr_Result(const GetWindowLongPtrResult: LONG_PTR); overload;
    procedure GetWindowLongPtr_Result(const Callback: TGetWindowLongPtrResultReference); overload;

  private type
    TSetWindowLongPtrResultReference =
      reference to function(const Wnd: HWND; const Index: Integer; const NewLong: LONG_PTR): LONG_PTR;
  public
    procedure SetWindowLongPtr_Result(const SetWindowLongPtrResult: LONG_PTR); overload;
    procedure SetWindowLongPtr_Result(const Callback: TSetWindowLongPtrResultReference); overload;

  private type
    TSetWindowPosResultReference =
      reference to procedure(
        const Wnd, InsertAfterWnd: HWND;
        const Left, Top, Width, Height: Integer;
        const Flags: UINT
      );
  public
    procedure SetWindowPos_Result(const Callback: TSetWindowPosResultReference);

  private type
    TGetScrollInfoResultReference = reference to function(const Wnd: HWND; const BarFlag: Integer): TScrollInfo;
  public
    procedure GetScrollInfo_Result(const ScrollInfoResult: TScrollInfo); overload;
    procedure GetScrollInfo_Result(const Callback: TGetScrollInfoResultReference); overload;

  private type
    TGetClientRectResultReference = reference to function(const Wnd: HWND): TRect;
  public
    procedure GetClientRect_Result(const GetClientRectResult: TRect); overload;
    procedure GetClientRect_Result(const Callback: TGetClientRectResultReference); overload;

  private type
    TGetWindowRectResultReference = reference to function(const Wnd: HWND): TRect;
  public
    procedure GetWindowRect_Result(const GetWindowRectResult: TRect); overload;
    procedure GetWindowRect_Result(const Callback: TGetWindowRectResultReference); overload;

  private type
    TSendMessageResultReference =
      reference to function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT;
  public
    procedure SendMessage_Result(const SendMessageResult: LRESULT); overload;
    procedure SendMessage_Result(const Callback: TSendMessageResultReference); overload;

  private type
    TGetDCResultReference = reference to function(const Wnd: HWND): HDC;
  public
    procedure GetDC_Result(const GetDCResult: HDC); overload;
    procedure GetDC_Result(const Callback: TGetDCResultReference); overload;

  private type
    TReleaseDCResultReference = reference to procedure(const Wnd: HWND; const DC: HDC);
  public
    procedure ReleaseDC_Result(const Callback: TReleaseDCResultReference);

  private type
    TDeleteDCResultReference = reference to procedure(const DC: HDC);
  public
    procedure DeleteDC_Result(const Callback: TDeleteDCResultReference);

  private type
    TGetDeviceCapsResultReference = reference to function(const DC: HDC; const Index: Integer): Integer;
  public
    procedure GetDeviceCaps_Result(const GetDeviceCapsResult: Integer); overload;
    procedure GetDeviceCaps_Result(const Callback: TGetDeviceCapsResultReference); overload;

  private type
    TGetSystemPaletteEntriesResultReference =
      reference to procedure(const DC: HDC; const StartIndex, NumEntries: UINT; out PaletteEntries);
  public
    procedure GetSystemPaletteEntries_Result(const Callback: TGetSystemPaletteEntriesResultReference);

  private type
    TCreatePaletteResultReference = reference to function(const LogPalette: PLogPalette): HPALETTE;
  public
    procedure CreatePalette_Result(const CreatePaletteResult: HPALETTE); overload;
    procedure CreatePalette_Result(const Callback: TCreatePaletteResultReference); overload;

  private type
    TIsChildResultReference = reference to function(const ParentWnd, Wnd: HWND): Boolean;
  public
    procedure IsChild_Result(const IsChildResult: Boolean); overload;
    procedure IsChild_Result(const Callback: TIsChildResultReference); overload;

  private type
    TGetParentResultReference = reference to function(const Wnd: HWND): HWND;
  public
    procedure GetParent_Result(const GetParentResult: HWND); overload;
    procedure GetParent_Result(const Callback: TGetParentResultReference); overload;

  private type
    TGetWindowResultReference = reference to function(const Wnd: HWND; const Cmd: UINT): HWND;
  public
    procedure GetWindow_Result(const GetWindowResult: HWND); overload;
    procedure GetWindow_Result(const Callback: TGetWindowResultReference); overload;

  private type
    TIsWindowVisibleResultReference = reference to function(const Wnd: HWND): BOOL;
  public
    procedure IsWindowVisible_Result(const IsWindowVisibleResult: BOOL); overload;
    procedure IsWindowVisible_Result(const Callback: TIsWindowVisibleResultReference); overload;

  private type
    TBitBltResultReference =
      reference to procedure(
        const DestDC: HDC;
        const X, Y, Width, Height: Integer;
        const SrcDC: HDC;
        const XSrc, YSrc: Integer;
        const Rop: DWORD
      );
  public
    procedure BitBlt_Result(const Callback: TBitBltResultReference);

  private type
    TGetBrushOrgExResultReference = reference to function(const DC: HDC): TPoint;
  public
    procedure GetBrushOrgEx_Result(const GetBrushOrgExResult: TPoint); overload;
    procedure GetBrushOrgEx_Result(const Callback: TGetBrushOrgExResultReference); overload;

  private type
    TSetBrushOrgExResultReference = reference to function(const DC: HDC; const X, Y: Integer): TPoint;
  public
    procedure SetBrushOrgEx_Result(const SetBrushOrgExResult: TPoint); overload;
    procedure SetBrushOrgEx_Result(const Callback: TSetBrushOrgExResultReference); overload;

  private type
    TDeleteObjectResultReference = reference to procedure(const Obj: HGDIOBJ);
  public
    procedure DeleteObject_Result(const Callback: TDeleteObjectResultReference);

  private type
    TCreateRectRgnResultReference = reference to function(const Left, Top, Right, Bottom: Integer): HRGN;
  public
    procedure CreateRectRgn_Result(const CreateRectRgnResult: HRGN); overload;
    procedure CreateRectRgn_Result(const Callback: TCreateRectRgnResultReference); overload;

  private type
    TGetWindowRgnResultReference = reference to function(const Wnd: HWND; const Rgn: HRGN): Integer;
  public
    procedure GetWindowRgn_Result(const GetWindowRgnResult: Integer); overload;
    procedure GetWindowRgn_Result(const Callback: TGetWindowRgnResultReference); overload;

  private type
    TOffsetRgnResultReference = reference to procedure(const Rgn: HRGN; const XOffset, YOffset: Integer);
  public
    procedure OffsetRgn_Result(const Callback: TOffsetRgnResultReference);

  private type
    TGetClipRgnResultReference   = reference to procedure(const DC: HDC; const Rgn: HRGN);
  public
    procedure GetClipRgn_Result(
      const Callback: TGetClipRgnResultReference;
      const GetClipRgnResult: Integer = -1
    ); overload;

  private type
    TCombineRgnResultReference = reference to procedure(const DestRgn, SrcRgn1, SrcRgn2: HRGN; const Mode: Integer);
  public
    procedure CombineRgn_Result(const Callback: TCombineRgnResultReference);

  private type
    TSelectClipRgnResultReference = reference to procedure(const DC: HDC; const Rgn: HRGN);
  public
    procedure SelectClipRgn_Result(const Callback: TSelectClipRgnResultReference);

  private type
    TGetRgnBoxResultReference = reference to function(const Rgn: HRGN): TRect;
  public
    procedure GetRgnBox_Result(const GetRgnBoxResult: TRect); overload;
    procedure GetRgnBox_Result(const Callback: TGetRgnBoxResultReference); overload;

  private type
    TIntersectClipRectResultReference = reference to procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer);
  public
    procedure IntersectClipRect_Result(Callback: TIntersectClipRectResultReference);

  private type
    TOffsetWindowOrgExResultReference = reference to function(const DC: HDC; const X, Y: Integer): TPoint;
  public
    procedure OffsetWindowOrgEx_Result(const OffsetWindowOrgExResult: TPoint); overload;
    procedure OffsetWindowOrgEx_Result(const Callback: TOffsetWindowOrgExResultReference); overload;

  private type
    TSetWindowOrgExResultReference = reference to procedure(const DC: HDC; const X, Y: Integer);
  public
    procedure SetWindowOrgEx_Result(const Callback: TSetWindowOrgExResultReference); overload;
    procedure SetWindowOrgEx_Result(
      const Callback: TSetWindowOrgExResultReference;
      const SetWindowOrgExResult: TPoint
    ); overload;

  private type
    TOleDrawResultReference =
      reference to procedure(const Unknown: IUnknown; const Aspect: LongInt; const DC: HDC; const Bounds: TRect);
  public
    procedure OleDraw_Result(const Callback: TOleDrawResultReference);

  private type
    TDrawEdgeResultReference = reference to procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT);
  public
    procedure DrawEdge_Result(const Callback: TDrawEdgeResultReference); overload;
    procedure DrawEdge_Result(const Callback: TDrawEdgeResultReference; const DrawEdgeRect: TRect); overload;

  private type
    TSaveDCResultReference = reference to function(const DC: HDC): Integer;
  public
    procedure SaveDC_Result(const SaveDCResult: Integer); overload;
    procedure SaveDC_Result(const Callback: TSaveDCResultReference); overload;

  private type
    TRestoreDCResultReference = reference to procedure(const DC: HDC; const Index: Integer);
  public
    procedure RestoreDC_Result(const Callback: TRestoreDCResultReference);

  private type
    TFillRectResultReference = reference to procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH);
  public
    procedure FillRect_Result(const Callback: TFillRectResultReference);

  private type
    TExcludeClipRectResultReference = reference to procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer);
  public
    procedure ExcludeClipRect_Result(const Callback: TExcludeClipRectResultReference);

  private type
    TGetSystemMetricsResultReference = reference to function(const Index: Integer): Integer;
  public
    procedure GetSystemMetrics_Result(const Callback: TGetSystemMetricsResultReference = nil);

  public
    constructor Create; override;
  end;

implementation

uses
  System.Types,
  FormEffects.TypeHelpers;

var
  GetWindowLongPtrResultReference: TWinapiWindowsMocks.TGetWindowLongPtrResultReference = nil;

type
  TGetWindowLongPtr = function(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall;

function GetWindowLongPtrMock(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall;
begin
  Result := GetWindowLongPtrResultReference(hWnd, nIndex);
end;

procedure TWinapiWindowsMocks.GetWindowLongPtr_Result(const GetWindowLongPtrResult: LONG_PTR);
begin
  GetWindowLongPtr_Result(
    function(const Wnd: HWND; const Index: Integer): LONG_PTR
    begin
      Result := GetWindowLongPtrResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetWindowLongPtr_Result(const Callback: TGetWindowLongPtrResultReference);
begin
  if Assigned(Callback) then
    GetWindowLongPtrResultReference := Callback
  else
  begin
    GetWindowLongPtrResultReference :=
      function(const Wnd: HWND; const Index: Integer): LONG_PTR
      begin
        Result := 0;
      end;
  end;
end;

var
  SetWindowLongPtrResultReference: TWinapiWindowsMocks.TSetWindowLongPtrResultReference = nil;

type
  TSetWindowLongPtr = function(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall;

function SetWindowLongPtrMock(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall;
begin
  Result := SetWindowLongPtrResultReference(hWnd, nIndex, dwNewLong);
end;

procedure TWinapiWindowsMocks.SetWindowLongPtr_Result(const SetWindowLongPtrResult: LONG_PTR);
begin
  SetWindowLongPtr_Result(
    function(const Wnd: HWND; const Index: Integer; const NewLong: LONG_PTR): LONG_PTR
    begin
      Result := SetWindowLongPtrResult;
    end
  );
end;

procedure TWinapiWindowsMocks.SetWindowLongPtr_Result(const Callback: TSetWindowLongPtrResultReference);
begin
  if Assigned(Callback) then
    SetWindowLongPtrResultReference := Callback
  else
  begin
    SetWindowLongPtrResultReference :=
      function(const Wnd: HWND; const Index: Integer; const NewLong: LONG_PTR): LONG_PTR
      begin
        Result := 0;
      end;
  end;
end;

var
  SetWindowPosResultReference: TWinapiWindowsMocks.TSetWindowPosResultReference = nil;

type
  TSetWindowPos = function(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx, cy: Integer; uFlags: UINT): BOOL; stdcall;

function SetWindowPosMock(hWnd: HWND; hWndInsertAfter: HWND; X, Y, cx, cy: Integer; uFlags: UINT): BOOL; stdcall;
begin
  SetWindowPosResultReference(hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
  Result := True;
end;

procedure TWinapiWindowsMocks.SetWindowPos_Result(const Callback: TSetWindowPosResultReference);
begin
  if Assigned(Callback) then
    SetWindowPosResultReference := Callback
  else
  begin
    SetWindowPosResultReference :=
      procedure(const Wnd, InsertAfterWnd: HWND; const Left, Top, Width, Height: Integer; const Flags: UINT)
      begin
      end;
  end;
end;

var
  GetScrollInfoResultReference: TWinapiWindowsMocks.TGetScrollInfoResultReference = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}

type
  TGetScrollRange = function(hWnd: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): BOOL; stdcall;

function GetScrollRangeMock(hWnd: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): BOOL; stdcall;
begin
  const ScrollInfo = GetScrollInfoResultReference(hWnd, nBar);
  lpMinPos        := ScrollInfo.nMin;
  lpMaxPos        := ScrollInfo.nMax;
  Result := True;
end;

{$ENDIF ~ USE_BILLENIUM_EFFECTS}

type
  TGetScrollInfo = function(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL; stdcall;

function GetScrollInfoMock(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL; stdcall;
begin
  ScrollInfo := GetScrollInfoResultReference(hWnd, BarFlag);
  Result     := True;
end;

procedure TWinapiWindowsMocks.GetScrollInfo_Result(const ScrollInfoResult: TScrollInfo);
begin
  GetScrollInfo_Result(
    function(const Wnd: HWND; const BarFlag: Integer): TScrollInfo
    begin
      Result := ScrollInfoResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetScrollInfo_Result(const Callback: TGetScrollInfoResultReference);
begin
  if Assigned(Callback) then
    GetScrollInfoResultReference := Callback
  else
  begin
    GetScrollInfoResultReference :=
      function(const Wnd: HWND; const BarFlag: Integer): TScrollInfo
      begin
      end;
  end;
end;

var
  GetClientRectResultReference: TWinapiWindowsMocks.TGetClientRectResultReference = nil;

type
  TGetClientRect = function(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;

function GetClientRectMock(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;
begin
  lpRect := GetClientRectResultReference(hWnd);
  Result := True;
end;

procedure TWinapiWindowsMocks.GetClientRect_Result(const GetClientRectResult: TRect);
begin
  GetClientRect_Result(
    function(const Wnd: HWND): TRect
    begin
      Result := GetClientRectResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetClientRect_Result(const Callback: TGetClientRectResultReference);
begin
  if Assigned(Callback) then
    GetClientRectResultReference := Callback
  else
  begin
    GetClientRectResultReference :=
      function(const Wnd: HWND): TRect
      begin
        Result := TRect.Zero;
      end;
  end;
end;

var
  GetWindowRectResultReference: TWinapiWindowsMocks.TGetWindowRectResultReference = nil;

type
  TGetWindowRect = function(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;

function GetWindowRectMock(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;
begin
  lpRect := GetWindowRectResultReference(hWnd);
  Result := true;
end;

procedure TWinapiWindowsMocks.GetWindowRect_Result(const GetWindowRectResult: TRect);
begin
  GetWindowRect_Result(
    function(const Wnd: HWND): TRect
    begin
      Result := GetWindowRectResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetWindowRect_Result(const Callback: TGetWindowRectResultReference);
begin
  if Assigned(Callback) then
    GetWindowRectResultReference := Callback
  else
  begin
    GetWindowRectResultReference :=
      function(const Wnd: HWND): TRect
      begin
        Result := TRect.Zero;
      end;
  end;
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

var
  SendMessageResultReference: TWinapiWindowsMocks.TSendMessageResultReference = nil;

type
  TSendMessageUnsigned = function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

function SendMessageWideMock(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := SendMessageResultReference(hWnd, Msg, wParam, lParam);
end;

procedure TWinapiWindowsMocks.SendMessage_Result(const SendMessageResult: LRESULT);
begin
  SendMessage_Result(
    function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
    begin
      Result := SendMessageResult;
    end
  );
end;

procedure TWinapiWindowsMocks.SendMessage_Result(const Callback: TSendMessageResultReference);
begin
  if Assigned(Callback) then
    SendMessageResultReference := Callback
  else
  begin
    SendMessageResultReference :=
      function(const Wnd: HWND; const Msg: UINT; const wParam: WPARAM; const lParam: LPARAM): LRESULT
      begin
        Result := -1;
      end;
  end;
end;

var
  GetDCResultReference: TWinapiWindowsMocks.TGetDCResultReference = nil;

type
  TGetDC = function(hWnd: HWND): HDC; stdcall;

function GetDCMock(hWnd: HWND): HDC; stdcall;
begin
  Result := GetDCResultReference(hWnd);
end;

procedure TWinapiWindowsMocks.GetDC_Result(const GetDCResult: HDC);
begin
  GetDC_Result(
    function(const Wnd: HWND): HDC
    begin
      Result := GetDCResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetDC_Result(const Callback: TGetDCResultReference);
begin
  if Assigned(Callback) then
    GetDCResultReference := Callback
  else
  begin
    GetDCResultReference :=
      function(const Wnd: HWND): HDC
      begin
        Result := 0;
      end;
  end;
end;

var
  ReleaseDCResultReference: TWinapiWindowsMocks.TReleaseDCResultReference = nil;

type
  TReleaseDC = function(hWnd: HWND; hDC: HDC): Integer; stdcall;

function ReleaseDCMock(hWnd: HWND; hDC: HDC): Integer; stdcall;
begin
  ReleaseDCResultReference(hWnd, hDC);
  Result := -1;
end;

procedure TWinapiWindowsMocks.ReleaseDC_Result(const Callback: TReleaseDCResultReference);
begin
  if Assigned(Callback) then
    ReleaseDCResultReference := Callback
  else
  begin
    ReleaseDCResultReference :=
      procedure(const hWnd: HWND; const hDC: HDC)
      begin
      end;
  end;
end;

var
  DeleteDCResultReference: TWinapiWindowsMocks.TDeleteDCResultReference = nil;

type
  TDeleteDC = function(DC: HDC): BOOL; stdcall;

function DeleteDCMock(DC: HDC): BOOL; stdcall;
begin
  DeleteDCResultReference(DC);
  Result := False;
end;

procedure TWinapiWindowsMocks.DeleteDC_Result(const Callback: TDeleteDCResultReference);
begin
  if Assigned(Callback) then
    DeleteDCResultReference := Callback
  else
  begin
    DeleteDCResultReference :=
      procedure(const DC: HDC)
      begin
      end;
  end;
end;

var
  GetDeviceCapsResultReference: TWinapiWindowsMocks.TGetDeviceCapsResultReference = nil;

type
  TGetDeviceCaps = function(DC: HDC; Index: Integer): Integer; stdcall;

function GetDeviceCapsMock(DC: HDC; Index: Integer): Integer; stdcall;
begin
  Result := GetDeviceCapsResultReference(DC, Index);
end;

procedure TWinapiWindowsMocks.GetDeviceCaps_Result(const GetDeviceCapsResult: Integer);
begin
  GetDeviceCaps_Result(
    function(const DC: HDC; const Index: Integer): Integer
    begin
      Result := GetDeviceCapsResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetDeviceCaps_Result(const Callback: TGetDeviceCapsResultReference);
begin
  if Assigned(Callback) then
    GetDeviceCapsResultReference := Callback
  else
  begin
    GetDeviceCapsResultReference :=
      function(const DC: HDC; const Index: Integer): Integer
      begin
        Result := -1;
      end;
  end;
end;

var
  GetSystemPaletteEntriesResultReference: TWinapiWindowsMocks.TGetSystemPaletteEntriesResultReference = nil;

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
  GetSystemPaletteEntriesResultReference(DC, StartIndex, NumEntries, PaletteEntries);
  Result := 0;
end;

procedure TWinapiWindowsMocks.GetSystemPaletteEntries_Result(
  const Callback: TGetSystemPaletteEntriesResultReference
);
begin
  if Assigned(Callback) then
    GetSystemPaletteEntriesResultReference := Callback
  else
  begin
    GetSystemPaletteEntriesResultReference :=
      procedure(const DC: HDC; const StartIndex, NumEntries: UINT; out PaletteEntries)
      begin
      end;
  end;
end;

var
  CreatePaletteResultReference: TWinapiWindowsMocks.TCreatePaletteResultReference = nil;

type
  TCreatePalette = function(LogPalette: PLogPalette): HPALETTE; stdcall;

function CreatePaletteMock(LogPalette: PLogPalette): HPALETTE; stdcall;
begin
  Result := CreatePaletteResultReference(LogPalette);
end;

procedure TWinapiWindowsMocks.CreatePalette_Result(const CreatePaletteResult: HPALETTE);
begin
  CreatePalette_Result(
    function(const LogPalette: PLogPalette): HPALETTE
    begin
      Result := CreatePaletteResult;
    end
  );
end;

procedure TWinapiWindowsMocks.CreatePalette_Result(const Callback: TCreatePaletteResultReference);
begin
  if Assigned(Callback) then
    CreatePaletteResultReference := Callback
  else
  begin
    CreatePaletteResultReference :=
      function(const LogPalette: PLogPalette): HPALETTE
      begin
        Result := 0;
      end;
  end;
end;

var
  IsChildResultReference: TWinapiWindowsMocks.TIsChildResultReference = nil;

type
  TIsChild = function(hWndParent, hWnd: HWND): BOOL; stdcall;

function IsChildMock(hWndParent, hWnd: HWND): BOOL; stdcall;
begin
  Result := IsChildResultReference(hWndParent, hWnd);
end;

procedure TWinapiWindowsMocks.IsChild_Result(const IsChildResult: Boolean);
begin
  IsChild_Result(
    function(const ParentWnd, Wnd: HWND): Boolean
    begin
      Result := IsChildResult;
    end
  );
end;

procedure TWinapiWindowsMocks.IsChild_Result(const Callback: TIsChildResultReference);
begin
  if Assigned(Callback) then
    IsChildResultReference := Callback
  else
  begin
    IsChildResultReference :=
      function(const ParentWnd, Wnd: HWND): Boolean
      begin
        Result := False;
      end;
  end;
end;

var
  GetParentResultReference: TWinapiWindowsMocks.TGetParentResultReference = nil;

type
  TGetParent = function(hWnd: HWND): HWND; stdcall;

function GetParentMock(hWnd: HWND): HWND; stdcall;
begin
  Result := GetParentResultReference(hWnd);
end;

procedure TWinapiWindowsMocks.GetParent_Result(const GetParentResult: HWND);
begin
  GetParent_Result(
    function(const hWnd: HWND): HWND
    begin
      Result := GetParentResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetParent_Result(const Callback: TGetParentResultReference);
begin
  if Assigned(Callback) then
    GetParentResultReference := Callback
  else
  begin
    GetParentResultReference :=
      function(const hWnd: HWND): HWND
      begin
        Result := 0;
      end;
  end;
end;

var
  GetWindowResultReference: TWinapiWindowsMocks.TGetWindowResultReference = nil;

type
  TGetWindow = function(hWnd: HWND; uCmd: UINT): HWND; stdcall;

function GetWindowMock(hWnd: HWND; uCmd: UINT): HWND; stdcall;
begin
  Result := GetWindowResultReference(hWnd, uCmd);
end;

procedure TWinapiWindowsMocks.GetWindow_Result(const GetWindowResult: HWND);
begin
  GetWindow_Result(
    function(const Wnd: HWND; const Cmd: UINT): HWND
    begin
      Result := GetWindowResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetWindow_Result(const Callback: TGetWindowResultReference);
begin
  if Assigned(Callback) then
    GetWindowResultReference := Callback
  else
  begin
    GetWindowResultReference :=
      function(const Wnd: HWND; const Cmd: UINT): HWND
      begin
        Result := 0;
      end;
  end;
end;

var
  IsWindowVisibleResultReference: TWinapiWindowsMocks.TIsWindowVisibleResultReference = nil;

type
  TIsWindowVisible = function(hWnd: HWND): BOOL; stdcall;

function IsWindowVisibleMock(hWnd: HWND): BOOL; stdcall;
begin
  Result := IsWindowVisibleResultReference(hWnd);
end;

procedure TWinapiWindowsMocks.IsWindowVisible_Result(const IsWindowVisibleResult: BOOL);
begin
  IsWindowVisible_Result(
    function(const Wnd: HWND): BOOL
    begin
      Result := IsWindowVisibleResult;
    end
  );
end;

procedure TWinapiWindowsMocks.IsWindowVisible_Result(const Callback: TIsWindowVisibleResultReference);
begin
  if Assigned(Callback) then
    IsWindowVisibleResultReference := Callback
  else
  begin
    IsWindowVisibleResultReference :=
      function(const Wnd: HWND): BOOL
      begin
        Result := False;
      end;
  end;
end;

var
  BitBltResultReference: TWinapiWindowsMocks.TBitBltResultReference = nil;

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
  BitBltResultReference(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop);
  Result := True;
end;

procedure TWinapiWindowsMocks.BitBlt_Result(const Callback: TBitBltResultReference);
begin
  if Assigned(Callback) then
    BitBltResultReference := Callback
  else
  begin
    BitBltResultReference :=
      procedure(
        const DestDC: HDC;
        const X, Y, Width, Height: Integer;
        const SrcDC: HDC;
        const XSrc, YSrc: Integer;
        const Rop: DWORD
      )
      begin
      end;
  end;
end;

var
  GetBrushOrgExResultReference: TWinapiWindowsMocks.TGetBrushOrgExResultReference = nil;

type
  TGetBrushOrgEx = function(DC: HDC; var lppt: TPoint): BOOL; stdcall;

function GetBrushOrgExMock(DC: HDC; var lppt: TPoint): BOOL; stdcall;
begin
  lppt := GetBrushOrgExResultReference(DC);
  Result := True;
end;

procedure TWinapiWindowsMocks.GetBrushOrgEx_Result(const GetBrushOrgExResult: TPoint);
begin
  GetBrushOrgEx_Result(
    function(const DC: HDC): TPoint
    begin
      Result := GetBrushOrgExResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetBrushOrgEx_Result(const Callback: TGetBrushOrgExResultReference);
begin
  if Assigned(Callback) then
    GetBrushOrgExResultReference := Callback
  else
  begin
    GetBrushOrgExResultReference :=
      function(const DC: HDC): TPoint
      begin
        Result := TPoint.Zero;
      end;
  end;
end;

var
  SetBrushOrgExResultReference: TWinapiWindowsMocks.TSetBrushOrgExResultReference = nil;

type
  TSetBrushOrgEx = function(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL; stdcall;

function SetBrushOrgExMock(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL; stdcall;
begin
  var Prev := SetBrushOrgExResultReference(DC, X, Y);
  if Assigned(PrevPt) then
    PrevPt^ := Prev;
  Result := True;
end;

procedure TWinapiWindowsMocks.SetBrushOrgEx_Result(const SetBrushOrgExResult: TPoint);
begin
  SetBrushOrgEx_Result(
    function(const DC: HDC; const X, Y: Integer): TPoint
    begin
      Result := SetBrushOrgExResult;
    end
  );
end;

procedure TWinapiWindowsMocks.SetBrushOrgEx_Result(const Callback: TSetBrushOrgExResultReference);
begin
  if Assigned(Callback) then
    SetBrushOrgExResultReference := Callback
  else
  begin
    SetBrushOrgExResultReference :=
      function(const DC: HDC; const X, Y: Integer): TPoint
      begin
        Result := TPoint.Zero;
      end;
  end;
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

var
  DeleteObjectResultReference: TWinapiWindowsMocks.TDeleteObjectResultReference = nil;

type
  TDeleteObject = function(p1: HGDIOBJ): BOOL; stdcall;

function DeleteObjectMock(p1: HGDIOBJ): BOOL; stdcall;
begin
  DeleteObjectResultReference(p1);
  Result := True;
end;

procedure TWinapiWindowsMocks.DeleteObject_Result(const Callback: TDeleteObjectResultReference);
begin
  if Assigned(Callback) then
    DeleteObjectResultReference := Callback
  else
  begin
    DeleteObjectResultReference :=
      procedure(const Obj: HGDIOBJ)
      begin
      end;
  end;
end;

var
  CreateRectRgnResultReference: TWinapiWindowsMocks.TCreateRectRgnResultReference = nil;

type
  TCreateRectRgn = function(p1, p2, p3, p4: Integer): HRGN; stdcall;

function CreateRectRgnMock(p1, p2, p3, p4: Integer): HRGN; stdcall;
begin
  Result := CreateRectRgnResultReference(p1, p2, p3, p4);
end;

procedure TWinapiWindowsMocks.CreateRectRgn_Result(const CreateRectRgnResult: HRGN);
begin
  CreateRectRgn_Result(
    function(const Left, Top, Right, Bottom: Integer): HRGN
    begin
      Result := CreateRectRgnResult;
    end
  );
end;

procedure TWinapiWindowsMocks.CreateRectRgn_Result(const Callback: TCreateRectRgnResultReference);
begin
  if Assigned(Callback) then
    CreateRectRgnResultReference := Callback
  else
  begin
    CreateRectRgnResultReference :=
      function(const Left, Top, Right, Bottom: Integer): HRGN
      begin
        Result := 0;
      end;
  end;
end;

var
  GetWindowRgnResultReference: TWinapiWindowsMocks.TGetWindowRgnResultReference = nil;

type
  TGetWindowRgn = function(hWnd: HWND; hRgn: HRGN): Integer; stdcall;

function GetWindowRgnMock(hWnd: HWND; hRgn: HRGN): Integer; stdcall;
begin
  Result := GetWindowRgnResultReference(hWnd, hRgn);
end;

procedure TWinapiWindowsMocks.GetWindowRgn_Result(const GetWindowRgnResult: Integer);
begin
  GetWindowRgn_Result(
    function(const Wnd: HWND; const Rgn: HRGN): Integer
    begin
      Result := GetWindowRgnResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetWindowRgn_Result(const Callback: TGetWindowRgnResultReference);
begin
  if Assigned(Callback) then
    GetWindowRgnResultReference := Callback
  else
  begin
    GetWindowRgnResultReference :=
      function(const Wnd: HWND; const Rgn: HRGN): Integer
      begin
        Result := ERROR;
      end;
  end;
end;

var
  OffsetRgnResultReference: TWinapiWindowsMocks.TOffsetRgnResultReference = nil;

type
  TOffsetRgn = function(RGN: HRGN; XOffset, YOffset: Integer): Integer; stdcall;

function OffsetRgnMock(RGN: HRGN; XOffset, YOffset: Integer): Integer; stdcall;
begin
  OffsetRgnResultReference(RGN, XOffset, YOffset);
  Result := 0;
end;

procedure TWinapiWindowsMocks.OffsetRgn_Result(const Callback: TOffsetRgnResultReference);
begin
  if Assigned(Callback) then
    OffsetRgnResultReference := Callback
  else
  begin
    OffsetRgnResultReference :=
      procedure(const RGN: HRGN; const XOffset, YOffset: Integer)
      begin
      end;
  end;
end;

type
 TGetClipRgnResultReference = reference to function(const DC: HDC; const Rgn: HRGN): Integer;

var
  GetClipRgnResultReference: TGetClipRgnResultReference = nil;

type
  TGetClipRgn = function(DC: HDC; rgn: HRGN): Integer; stdcall;

function GetClipRgnMock(DC: HDC; rgn: HRGN): Integer; stdcall;
begin
  Result := GetClipRgnResultReference(DC, rgn);
end;

procedure TWinapiWindowsMocks.GetClipRgn_Result(
  const Callback: TWinapiWindowsMocks.TGetClipRgnResultReference;
  const GetClipRgnResult: Integer
);
begin
  GetClipRgnResultReference :=
    function(const DC: HDC; const Rgn: HRGN): Integer
    begin
      if Assigned(Callback) then
        Callback(DC, Rgn);

      Result := GetClipRgnResult;
    end;
end;

var
  CombineRgnResultReference: TWinapiWindowsMocks.TCombineRgnResultReference = nil;

type
  TCombineRgn = function(p1, p2, p3: HRGN; p4: Integer): Integer; stdcall;

function CombineRgnMock(p1, p2, p3: HRGN; p4: Integer): Integer; stdcall;
begin
  CombineRgnResultReference(p1, p2, p3, p4);
  Result := -1;
end;

procedure TWinapiWindowsMocks.CombineRgn_Result(const Callback: TCombineRgnResultReference);
begin
  if Assigned(Callback) then
    CombineRgnResultReference := Callback
  else
  begin
    CombineRgnResultReference :=
      procedure(const DestRgn, SrcRgn1, SrcRgn2: HRGN; const Mode: Integer)
      begin
      end;
  end;
end;

var
  SelectClipRgnResultReference: TWinapiWindowsMocks.TSelectClipRgnResultReference = nil;

type
  TSelectClipRgn = function(DC: HDC; Region: HRGN): Integer; stdcall;

function SelectClipRgnMock(DC: HDC; Region: HRGN): Integer; stdcall;
begin
  SelectClipRgnResultReference(DC, Region);
  Result := -1;
end;

procedure TWinapiWindowsMocks.SelectClipRgn_Result(const Callback: TSelectClipRgnResultReference);
begin
  if Assigned(Callback) then
    SelectClipRgnResultReference := Callback
  else
  begin
    SelectClipRgnResultReference :=
      procedure(const DC: HDC; const Rgn: HRGN)
      begin
      end;
  end;
end;

var
  GetRgnBoxResultReference: TWinapiWindowsMocks.TGetRgnBoxResultReference = nil;

type
  TGetRgnBox = function(RGN: HRGN; var p2: TRect): Integer; stdcall;

function GetRgnBoxMock(RGN: HRGN; var p2: TRect): Integer; stdcall;
begin
  p2 := GetRgnBoxResultReference(RGN);
  Result := 0;
end;

procedure TWinapiWindowsMocks.GetRgnBox_Result(const GetRgnBoxResult: TRect);
begin
  GetRgnBox_Result(
    function(const Rgn: HRGN): TRect
    begin
      Result := GetRgnBoxResult;
    end
  );
end;

procedure TWinapiWindowsMocks.GetRgnBox_Result(const Callback: TGetRgnBoxResultReference);
begin
  if Assigned(Callback) then
    GetRgnBoxResultReference := Callback
  else
  begin
    GetRgnBoxResultReference :=
      function(const Rgn: HRGN): TRect
      begin
        Result := TRect.Zero;
      end;
  end;
end;

var
  IntersectClipRectResultReference: TWinapiWindowsMocks.TIntersectClipRectResultReference = nil;

type
  TIntersectClipRect = function(DC: HDC; X1, Y1, X2, Y2: Integer): Integer; stdcall;

function IntersectClipRectMock(DC: HDC; X1, Y1, X2, Y2: Integer): Integer; stdcall;
begin
  IntersectClipRectResultReference(DC, X1, Y1, X2, Y2);
  Result := -1;
end;

procedure TWinapiWindowsMocks.IntersectClipRect_Result(Callback: TIntersectClipRectResultReference);
begin
  if Assigned(Callback) then
    IntersectClipRectResultReference := Callback
  else
  begin
    IntersectClipRectResultReference :=
      procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
      begin
      end;
  end;
end;

var
  OffsetWindowOrgExResultReference: TWinapiWindowsMocks.TOffsetWindowOrgExResultReference = nil;

type
  TOffsetWindowOrgEx = function(DC: HDC; X, Y: Integer; var Points): BOOL; stdcall;

function OffsetWindowOrgExMock(DC: HDC; X, Y: Integer; var Points): BOOL; stdcall;
begin
  PPoint(@Points)^ := OffsetWindowOrgExResultReference(DC, X, Y);
  Result := True;
end;

procedure TWinapiWindowsMocks.OffsetWindowOrgEx_Result(const OffsetWindowOrgExResult: TPoint);
begin
  OffsetWindowOrgEx_Result(
    function(const DC: HDC; const X, Y: Integer): TPoint
    begin
      Result := OffsetWindowOrgExResult;
    end
  );
end;

procedure TWinapiWindowsMocks.OffsetWindowOrgEx_Result(const Callback: TOffsetWindowOrgExResultReference);
begin
  if Assigned(Callback) then
    OffsetWindowOrgExResultReference := Callback
  else
  begin
    OffsetWindowOrgExResultReference :=
      function(const DC: HDC; const X, Y: Integer): TPoint
      begin
        Result := TPoint.Zero;
      end;
  end;
end;

type
  TSetWindowOrgExResultReference = reference to function(const DC: HDC; const X, Y: Integer): TPoint;

var
  SetWindowOrgExResultReference: TSetWindowOrgExResultReference = nil;

type
  TSetWindowOrgEx = function(DC: HDC; X, Y: Integer; Point: PPoint): BOOL; stdcall;

function SetWindowOrgExMock(DC: HDC; X, Y: Integer; Point: PPoint): BOOL; stdcall;
begin
  const Prev = SetWindowOrgExResultReference(DC, X, Y);
  if Assigned(Point) then
    Point^ := Prev;
  Result := True;
end;

procedure TWinapiWindowsMocks.SetWindowOrgEx_Result(const Callback: TWinapiWindowsMocks.TSetWindowOrgExResultReference);
begin
  SetWindowOrgEx_Result(Callback, TPoint.Zero);
end;

procedure TWinapiWindowsMocks.SetWindowOrgEx_Result(
  const Callback: TWinapiWindowsMocks.TSetWindowOrgExResultReference;
  const SetWindowOrgExResult: TPoint
);
begin
  SetWindowOrgExResultReference :=
    function(const DC: HDC; const X, Y: Integer): TPoint
    begin
      if Assigned(Callback) then
        Callback(DC, X, Y);

      Result := SetWindowOrgExResult;
    end;
end;

var
  OleDrawResultReference: TWinapiWindowsMocks.TOleDrawResultReference = nil;

type
  TOleDraw = function(unknown: IUnknown; dwAspect: Longint; hdcDraw: HDC; const rcBounds: TRect): HRESULT; stdcall;

function OleDrawMock(unknown: IUnknown; dwAspect: Longint; hdcDraw: HDC; const rcBounds: TRect): HRESULT; stdcall;
begin
  OleDrawResultReference(unknown, dwAspect, hdcDraw, rcBounds);
  Result := S_OK;
end;

procedure TWinapiWindowsMocks.OleDraw_Result(const Callback: TOleDrawResultReference);
begin
  if Assigned(Callback) then
    OleDrawResultReference := Callback
  else
  begin
    OleDrawResultReference :=
      procedure(const Unknown: IUnknown; const Aspect: LongInt; const DC: HDC; const Bounds: TRect)
      begin
      end;
  end;
end;

var
  DrawEdgeResultReference: TWinapiWindowsMocks.TDrawEdgeResultReference = nil;

type
  TDrawEdge = function(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;

function DrawEdgeMock(hdc: HDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
begin
  DrawEdgeResultReference(hdc, qrc, edge, grfFlags);
  Result := True;
end;

procedure TWinapiWindowsMocks.DrawEdge_Result(const Callback: TDrawEdgeResultReference);
begin
  DrawEdge_Result(Callback, TRect.Zero);
end;

procedure TWinapiWindowsMocks.DrawEdge_Result(const Callback: TDrawEdgeResultReference; const DrawEdgeRect: TRect);
begin
  if Assigned(Callback) then
    DrawEdgeResultReference := Callback
  else
  begin
    DrawEdgeResultReference :=
      procedure(const DC: HDC; var Rect: TRect; const Edge, Flags: UINT)
      begin
        Rect := DrawEdgeRect;
      end;
  end;
end;

var
  SaveDCResultReference: TWinapiWindowsMocks.TSaveDCResultReference = nil;

type
  TSaveDC = function(DC: HDC): Integer; stdcall;

function SaveDCMock(DC: HDC): Integer; stdcall;
begin
  Result := SaveDCResultReference(DC);
end;

procedure TWinapiWindowsMocks.SaveDC_Result(const SaveDCResult: Integer);
begin
  SaveDC_Result(
    function(const DC: HDC): Integer
    begin
      Result := SaveDCResult;
    end
  );
end;

procedure TWinapiWindowsMocks.SaveDC_Result(const Callback: TSaveDCResultReference);
begin
  if Assigned(Callback) then
    SaveDCResultReference := Callback
  else
  begin
    SaveDCResultReference :=
      function(const DC: HDC): Integer
      begin
        Result := -1;
      end;
  end;
end;

var
  RestoreDCResultReference: TWinapiWindowsMocks.TRestoreDCResultReference = nil;

type
  TRestoreDC = function(DC: HDC; Index: Integer): BOOL; stdcall;

function RestoreDCMock(DC: HDC; Index: Integer): BOOL; stdcall;
begin
  RestoreDCResultReference(DC, Index);
  Result := True;
end;

procedure TWinapiWindowsMocks.RestoreDC_Result(const Callback: TRestoreDCResultReference);
begin
  if Assigned(Callback) then
    RestoreDCResultReference := Callback
  else
  begin
    RestoreDCResultReference :=
      procedure(const DC: HDC; const Index: Integer)
      begin
      end;
  end;
end;

var
  FillRectResultReference: TWinapiWindowsMocks.TFillRectResultReference = nil;

type
  TFillRect = function(hDC: HDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;

function FillRectMock(hDC: HDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
begin
  FillRectResultReference(hDC, lprc, hbr);
  Result := -1;
end;

procedure TWinapiWindowsMocks.FillRect_Result(const Callback: TFillRectResultReference);
begin
  if Assigned(Callback) then
    FillRectResultReference := Callback
  else
  begin
    FillRectResultReference :=
      procedure(const DC: HDC; const Rect: TRect; Brush: HBRUSH)
      begin
      end;
  end;
end;

var
  ExcludeClipRectResultReference: TWinapiWindowsMocks.TExcludeClipRectResultReference = nil;

type
  TExcludeClipRect = function(DC: HDC; LeftRect, TopRect, RightRect, BottomRect: Integer): Integer; stdcall;

function ExcludeClipRectMock(DC: HDC; LeftRect, TopRect, RightRect, BottomRect: Integer): Integer; stdcall;
begin
  ExcludeClipRectResultReference(DC, LeftRect, TopRect, RightRect, BottomRect);
  Result := -1;
end;

procedure TWinapiWindowsMocks.ExcludeClipRect_Result(const Callback: TExcludeClipRectResultReference);
begin
  if Assigned(Callback) then ExcludeClipRectResultReference := Callback
  else
  begin
    ExcludeClipRectResultReference :=
      procedure(const DC: HDC; const Left, Top, Right, Bottom: Integer)
      begin
      end;
  end;
end;

var
  GetSystemMetricsResultReference: TWinapiWindowsMocks.TGetSystemMetricsResultReference = nil;

type
  TGetSystemMetrics = function(Index: Integer): Integer; stdcall;

function GetSystemMetricsMock(Index: Integer): Integer; stdcall;
begin
  Result := GetSystemMetricsResultReference(Index);
end;

procedure TWinapiWindowsMocks.GetSystemMetrics_Result(const Callback: TGetSystemMetricsResultReference);
begin
  if Assigned(Callback) then GetSystemMetricsResultReference := Callback
  else
  begin
    GetSystemMetricsResultReference :=
      function(const Index: Integer): Integer
      begin
        Result := Index;
      end
  end;
end;

{ TWinapiWindowsMocks }

constructor TWinapiWindowsMocks.Create;
begin
  inherited Create;

  AddIntercept<TGetWindowLongPtr>(GetWindowLongPtr, GetWindowLongPtrMock);
  GetWindowLongPtr_Result(nil);

  AddIntercept<TSetWindowLongPtr>(SetWindowLongPtr, SetWindowLongPtrMock);
  SetWindowLongPtr_Result(nil);

  AddIntercept<TSetWindowPos>(SetWindowPos, SetWindowPosMock);
  SetWindowPos_Result(nil);

{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetScrollRange>(GetScrollRange, GetScrollRangeMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
  AddIntercept<TGetScrollInfo>(GetScrollInfo, GetScrollInfoMock);
  GetScrollInfo_Result(nil);

  AddIntercept<TGetClientRect>(GetClientRect, GetClientRectMock);
  GetClientRect_Result(nil);

  AddIntercept<TGetWindowRect>(GetWindowRect, GetWindowRectMock);
  GetWindowRect_Result(nil);

  AddIntercept<TMapWindowPoints>(MapWindowPoints, MapWindowPointsMock);
{$IFDEF USE_BILLENIUM_EFFECTS}
  AddIntercept<TClientToScreen>(ClientToScreen, ClientToScreenMock);
  AddIntercept<TScreenToClient>(ScreenToClient, ScreenToClientMock);
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

  AddIntercept<TSendMessageUnsigned>(SendMessage, SendMessageWideMock);
  SendMessage_Result(nil);

  AddIntercept<TGetDC>(GetDC, GetDCMock);
  GetDC_Result(nil);

  AddIntercept<TReleaseDC>(ReleaseDC, ReleaseDCMock);
  ReleaseDC_Result(nil);

  AddIntercept<TDeleteDC>(DeleteDC, DeleteDCMock);
  DeleteDC_Result(nil);

  AddIntercept<TGetDeviceCaps>(GetDeviceCaps, GetDeviceCapsMock);
  GetDeviceCaps_Result(nil);

  AddIntercept<TGetSystemPaletteEntries>(GetSystemPaletteEntries, GetSystemPaletteEntriesMock);
  GetSystemPaletteEntries_Result(nil);

  AddIntercept<TCreatePalette>(CreatePalette, CreatePaletteMock);
  CreatePalette_Result(nil);

  AddIntercept<TIsChild>(IsChild, IsChildMock);
  IsChild_Result(nil);

  AddIntercept<TGetParent>(GetParent, GetParentMock);
  GetParent_Result(nil);

  AddIntercept<TGetWindow>(GetWindow, GetWindowMock);
  GetWindow_Result(nil);

  AddIntercept<TIsWindowVisible>(IsWindowVisible, IsWindowVisibleMock);
  IsWindowVisible_Result(nil);

  AddIntercept<TBitBlt>(BitBlt, BitBltMock);
  BitBlt_Result(nil);

  AddIntercept<TGetBrushOrgEx>(GetBrushOrgEx, GetBrushOrgExMock);
  GetBrushOrgEx_Result(nil);

  AddIntercept<TSetBrushOrgEx>(SetBrushOrgEx, SetBrushOrgExMock);
  SetBrushOrgEx_Result(nil);

  AddIntercept<TLPtoDP>(Winapi.Windows.LPtoDP, LPtoDPMock);

  AddIntercept<TDPtoLP>(Winapi.Windows.DPtoLP, DPtoLPMock);

  AddIntercept<TDeleteObject>(DeleteObject, DeleteObjectMock);
  DeleteObject_Result(nil);

  AddIntercept<TCreateRectRgn>(CreateRectRgn, CreateRectRgnMock);
  CreateRectRgn_Result(nil);

  AddIntercept<TGetWindowRgn>(GetWindowRgn, GetWindowRgnMock);
  GetWindowRgn_Result(nil);

  AddIntercept<TOffsetRgn>(OffsetRgn, OffsetRgnMock);
  OffsetRgn_Result(nil);

  AddIntercept<TGetClipRgn>(GetClipRgn, GetClipRgnMock);
  GetClipRgn_Result(nil);

  AddIntercept<TCombineRgn>(CombineRgn, CombineRgnMock);
  CombineRgn_Result(nil);

  AddIntercept<TSelectClipRgn>(SelectClipRgn, SelectClipRgnMock);
  SelectClipRgn_Result(nil);

  AddIntercept<TGetRgnBox>(GetRgnBox, GetRgnBoxMock);
  GetRgnBox_Result(nil);

  AddIntercept<TIntersectClipRect>(IntersectClipRect, IntersectClipRectMock);
  IntersectClipRect_Result(nil);

  AddIntercept<TOffsetWindowOrgEx>(OffsetWindowOrgEx, OffsetWindowOrgExMock);
  OffsetWindowOrgEx_Result(nil);

  AddIntercept<TSetWindowOrgEx>(SetWindowOrgEx, SetWindowOrgExMock);
  SetWindowOrgEx_Result(nil);

  AddIntercept<TOleDraw>(OleDraw, OleDrawMock);
  OleDraw_Result(nil);

  AddIntercept<TDrawEdge>(DrawEdge, DrawEdgeMock);
  DrawEdge_Result(nil);

  AddIntercept<TSaveDC>(SaveDC, SaveDCMock);
  SaveDC_Result(nil);

  AddIntercept<TRestoreDC>(RestoreDC, RestoreDCMock);
  RestoreDC_Result(nil);

  AddIntercept<TFillRect>(FillRect, FillRectMock);
  FillRect_Result(nil);

  AddIntercept<TExcludeClipRect>(ExcludeClipRect, ExcludeClipRectMock);
  ExcludeClipRect_Result(nil);

  AddIntercept<TGetSystemMetrics>(GetSystemMetrics, GetSystemMetricsMock);
  GetSystemMetrics_Result(nil);
end;

end.
