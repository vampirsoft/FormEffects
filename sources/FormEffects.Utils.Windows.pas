/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.Windows.pas                              *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2025 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.Windows;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  FormEffects.TypeHelpers;

/// <summary>
///   Returns the offset of the client area within the window
/// </summary>
function GetWindowOffset(const Wnd: HWND): TPoint;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}
function HasWindowRegion(const Wnd: HWND): Boolean;
/// <summary>
///   Invoke Winapi.Windows.GetClientRect for True, otherwise - Winapi.Windows.GetWindowRect
/// </summary>
function GetWindowSize(
  const Wnd: HWND;
  const IsMaximizedMDIChild: Boolean
): TSize;{$IFNDEF FORM_EFFECTS_TESTS}inline;{$ENDIF}

implementation

function GetWindowOffset(const Wnd: HWND): TPoint;
begin
  const ScreenRect = Wnd.GetWindowRect;

  Result := TPoint.Zero;
  Wnd.MapWindowPointToScreen(Result);

  Result := Result.InlineSubtract(ScreenRect.TopLeft);
end;

function HasWindowRegion(const Wnd: HWND): Boolean;
begin
  const Rgn = HRGN.Zero;
  try
    Result := Wnd.GetWindowRgn(Rgn) <> ERROR;
  finally
    Rgn.Delete;
  end;
end;

function GetWindowSize(const Wnd: HWND; const IsMaximizedMDIChild: Boolean): TSize;
var
  Rect: TRect;

begin
  if IsMaximizedMDIChild then
    Rect := Wnd.GetParent.GetClientRect
  else
    Rect := Wnd.GetWindowRect;
  Result := TSize.InlineCreate(Rect);
end;

end.
