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
  Winapi.Windows;

{$IFDEF USE_BILLENIUM_EFFECTS}
function GetWindowLong(hWnd: HWND; nIndex: Integer): NativeInt;
function GetScrollRange(hWnd: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): BOOL; stdcall;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
procedure SetResult_GetWindowLongPtr(const Result: NativeInt);

type
  TGetScrollInfoResult = reference to function(const hWnd: HWND; const BarFlag: Integer): TScrollInfo;

function GetScrollInfo(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL; stdcall;
procedure SetResult_GetScrollInfo(const GetScrollInfoResult: TGetScrollInfoResult);

var
  GetScrollInfo_Wnd: HWND;
  GetScrollInfo_BarFlag: Integer;
  GetScrollInfo_ScrollInfo: TScrollInfo;

function GetClientRect(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;
procedure SetResult_GetClientRect(const Result: TRect);

var
  GetClientRect_Wnd: HWND;
  GetClientRect_Rect: TRect;

function ClientToScreen(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;
function ScreenToClient(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;

/// <summary>
///   cPoints should be 1
/// </summary>
function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints: TPoint; cPoints: UINT): Integer; stdcall; overload;
/// <summary>
///   cPoints should be 2
/// </summary>
function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints: TRect; cPoints: UINT): Integer; stdcall; overload;

type
  TSendMessageResult = reference to function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;

function SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
procedure SetResult_SendMessage(const SendMessageResult: LRESULT); overload;
procedure SetResult_SendMessage(const SendMessageResult: TSendMessageResult); overload;

implementation

var
  WindowLongResult: NativeInt;

{$IFDEF USE_BILLENIUM_EFFECTS}
function GetWindowLong(hWnd: HWND; nIndex: Integer): NativeInt;
{$ELSE ~ NOT USE_BILLENIUM_EFFECTS}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}
begin
  Result := WindowLongResult;
end;

procedure SetResult_GetWindowLongPtr(const Result: NativeInt);
begin
  WindowLongResult := Result;
end;

var
  GetScrollInfoResult: TGetScrollInfoResult = nil;

{$IFDEF USE_BILLENIUM_EFFECTS}
function GetScrollRange(hWnd: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): BOOL;
begin
  GetScrollInfo_Wnd     := hWnd;
  GetScrollInfo_BarFlag := nBar;

  const ScrollInfoResult = GetScrollInfoResult(hWnd, nBar);
  lpMinPos := ScrollInfoResult.nMin;
  lpMaxPos := ScrollInfoResult.nMax;
  Result := True;
end;
{$ENDIF ~ USE_BILLENIUM_EFFECTS}

function GetScrollInfo(hWnd: HWND; BarFlag: Integer; var ScrollInfo: TScrollInfo): BOOL;
begin
  GetScrollInfo_Wnd        := hWnd;
  GetScrollInfo_BarFlag    := BarFlag;
  GetScrollInfo_ScrollInfo := ScrollInfo;

  ScrollInfo := GetScrollInfoResult(hWnd, BarFlag);
  Result := True;
end;

procedure SetResult_GetScrollInfo(const GetScrollInfoResult: TGetScrollInfoResult);
begin
  FormEffects.Winapi.Windows.Mocks.GetScrollInfoResult := GetScrollInfoResult;
end;

var
  GetClientRectResult: TRect;

function GetClientRect(hWnd: HWND; var lpRect: TRect): BOOL; stdcall;
begin
  GetClientRect_Wnd  := hWnd;
  GetClientRect_Rect := lpRect;

  lpRect := GetClientRectResult;
  Result := True;
end;

procedure SetResult_GetClientRect(const Result: TRect);
begin
  GetClientRectResult := Result;
end;

function ClientToScreen(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;
begin
  lpPoint.Offset(-hWnd, -hWnd);
  Result := True;
end;

function ScreenToClient(hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;
begin
  lpPoint.Offset(hWnd, hWnd);
  Result := True;
end;

function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints: TPoint; cPoints: UINT): Integer; stdcall; overload;
begin
  lpPoints.Offset(hWndTo - hWndFrom, hWndTo - hWndFrom);
  Result := cPoints;
end;

function MapWindowPoints(hWndFrom, hWndTo: HWND; var lpPoints: TRect; cPoints: UINT): Integer; stdcall; overload;
begin
  MapWindowPoints(hWndFrom, hWndTo, lpPoints.TopLeft, 1);
  MapWindowPoints(hWndFrom, hWndTo, lpPoints.BottomRight, 1);
  Result := cPoints;
end;

var
  SendMessageResult: TSendMessageResult = nil;

function SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
begin
  Result := SendMessageResult(hWnd, Msg, wParam, lParam);
end;

procedure SetResult_SendMessage(const SendMessageResult: LRESULT);
begin
  SetResult_SendMessage(
    function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT
    begin
      Result := SendMessageResult;
    end
  );
end;

procedure SetResult_SendMessage(const SendMessageResult: TSendMessageResult); overload;
begin
  if Assigned(SendMessageResult) then FormEffects.Winapi.Windows.Mocks.SendMessageResult := SendMessageResult
  else
  begin
    FormEffects.Winapi.Windows.Mocks.SendMessageResult :=
      function(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT
      begin
        Result := -1;
      end;
  end;
end;

initialization
  SetResult_SendMessage(nil);

end.
