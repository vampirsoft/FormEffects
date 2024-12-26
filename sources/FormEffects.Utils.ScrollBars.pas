/////////////////////////////////////////////////////////////////////////////////
//*****************************************************************************//
//* Project      : FormEffects                                                *//
//* Latest Source: https://github.com/vampirsoft/FormEffects                  *//
//* Unit Name    : FormEffects.Utils.ScrollBars.pas                           *//
//* Author       : Сергей (LordVampir) Дворников                              *//
//* Copyright 2026 LordVampir (https://github.com/vampirsoft)                 *//
//* Licensed under MIT                                                        *//
//*****************************************************************************//
/////////////////////////////////////////////////////////////////////////////////

unit FormEffects.Utils.ScrollBars;

{$INCLUDE FormEffects.inc}

interface

uses
  Winapi.Windows,
  System.Types,
  Vcl.Controls,
  Vcl.Forms
{$IFDEF FORM_EFFECTS_TESTS}
  , FormEffects.Vcl.Controls.Mocks
  , FormEffects.Vcl.Forms.Mocks
{$ENDIF ~ FORM_EFFECTS_TESTS}
  ;

{$MESSAGE 'Refactoring IsScrollBarVisible for optimize'}
function IsScrollBarVisible(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean;
function GetScrollbarsOffset(const Wnd: HWND; const Control: TControl; const Mask: UINT): TPoint;

implementation

uses
  FormEffects.TypeHelpers;

{$REGION 'Internal definitions'}

type

{ TScrollBarKindHelper }

  TScrollBarKindHelper = record helper for TScrollBarKind
  public
    function GetBarStyle: NativeInt; inline;
    function GetBarType: Integer; inline;
  end;

{ TScrollingWinControlHelper }

  TScrollingWinControlHelper = class helper for TScrollingWinControl
  public
    function GetControlScrollBar(const Kind: TScrollBarKind): TControlScrollBar; inline;
  end;

function GetControlScrollBar(const Control: TControl; const Kind: TScrollBarKind): TControlScrollBar; inline;
begin
  if Control is TScrollingWinControl then
    Result := TScrollingWinControl(Control).GetControlScrollBar(Kind)
  else
    Result := nil;
end;

{ TScrollBarKindHelper }

function TScrollBarKindHelper.GetBarStyle: NativeInt;
const
  BarStyles: array[TScrollBarKind] of NativeInt = (WS_HSCROLL, WS_VSCROLL);

begin
  Result := BarStyles[Self];
//  if Self = sbVertical then
//    Result := WS_VSCROLL
//  else
//    Result := WS_HSCROLL;
end;

function TScrollBarKindHelper.GetBarType: Integer;
const
  BarTypes: array[TScrollBarKind] of Integer = (SB_HORZ, SB_VERT);

begin
  Result := BarTypes[Self];
//  if Self = sbVertical then
//    Result := SB_VERT
//  else
//    Result := SB_HORZ;
end;

{ TScrollingWinControlHelper }

function TScrollingWinControlHelper.GetControlScrollBar(const Kind: TScrollBarKind): TControlScrollBar;
begin
  if Kind = sbVertical then
    Result := Self.VertScrollBar
  else
    Result := Self.HorzScrollBar;
end;

{$ENDREGION 'Internal definitions'}

function IsScrollBarVisible(const Wnd: HWND; const Control: TControl; const Kind: TScrollBarKind): Boolean;
begin
  const ScrollBar = GetControlScrollBar(Control, Kind);
  const BarStyle  = Kind.GetBarStyle;

  Result := ((ScrollBar = nil) or ScrollBar.Visible) and (Wnd.GetWindowLongPtr(GWL_STYLE) and BarStyle <> 0);

  if Result then
  begin
    const ScrollInfo = TScrollInfo.GetInfo(Wnd, Kind.GetBarType, SIF_RANGE);
    Result := (ScrollInfo.nMin <> 0) or (ScrollInfo.nMax <> 0);
  end;
end;

{$REGION 'Internal definitions'}

function GetScrollbarOffset(
  const Wnd: HWND;
  const Control: TControl;
  const Mask: UINT;
  const Kind: TScrollBarKind
): Integer; inline;
begin
  if IsScrollBarVisible(Wnd, Control, Kind) then
    Result := TScrollInfo.GetInfo(Wnd, Kind.GetBarType, Mask).nPos
  else
    Result := 0;
end;

{$ENDREGION 'Internal definitions'}

function GetScrollbarsOffset(const Wnd: HWND; const Control: TControl; const Mask: UINT): TPoint;
begin
  Result.X := GetScrollbarOffset(Wnd, Control, Mask, sbHorizontal);
  Result.Y := GetScrollbarOffset(Wnd, Control, Mask, sbVertical);
end;

end.
